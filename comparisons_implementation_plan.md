# Plan: the comparison functions (`pairwise_comparisons()`, `reference_comparisons()`, general contrasts)

**Status:**

- **Phase 1 — `pairwise_comparisons()`: IMPLEMENTED** on the `feature/pairwise-comparisons`
  branch (off `dev`). Difference-centric tidy table + forest-plot `autoplot`. Ships in the next
  release (currently the `1.5.0` section of `NEWS.md`).
- **Phase 2a — `reference_comparisons()` (exact Dunnett) + `include_means`: IMPLEMENTED** on
  `feature/pairwise-comparisons`. The Dunnett correlation is reconstructed from the SED matrix and
  per-mean SEs (`V_ij = (V_ii + V_jj - SED_ij^2)/2`) inside a shared block engine, validated to
  machine precision against `emmeans` `trt.vs.ctrl` (`adjust = "mvt"`). The planned `get_predictions()`
  vcov plumbing (§5.1) was **not needed** — reconstruction is exact and engine-agnostic, and was the
  only path testable without a licensed asreml. Direct `V` (asreml `vcov = TRUE`, emmeans) remains a
  possible later numerical-robustness optimisation.
- **Phase 2b — general `contrasts =` argument: IMPLEMENTED** on `feature/pairwise-comparisons`,
  validated to machine precision against `emmeans` contrasts (scalar-df models). Implementation
  deviations from §5: the `get_predictions()` vcov plumbing (§5.1) was **not** done (reconstruction is
  exact); the shared engine is `build_pairwise_block()` + `build_contrast_block()` +
  `reconstruct_vcov()` + `dunnett_adjust()` rather than the single `compute_comparison_block()` of §5.7.
- **Open — post-implementation review actions: see §7** (adversarial sanity check against `mct.R`,
  awaiting triage).

This document captures (1) the conceptual discussion and decisions behind the comparison functions,
and (2) the implementation plans for both phases. It is a planning artefact, not package code, and is
listed in `.Rbuildignore` (and should stay excluded from the build). If you rename it, update the
`.Rbuildignore` entry.

> **Phasing change (this revision).** `reference_comparisons()` was originally going to be a separate,
> later effort. It is now folded into **Phase 2 alongside general contrasts**, because (a) both are
> just linear-contrast sugar over one engine, (b) both need the full prediction vcov `V`, so plumbing
> `V` through `get_predictions()` serves both at once, and (c) these are brand-new functions with no
> backwards-compatibility to protect — the `get_predictions()` extension is internal and
> non-disruptive.

---

## 1. Background & motivation

### 1.1 Means-centric vs difference-centric

`multiple_comparisons()` is **means-centric**: one row per treatment level (mean, SE, CI, letter
group), where the compact letter display (CLD) encodes the *complete* matrix of all pairwise
comparisons. Its implicit contract is that every pair has been compared, so "share a letter" ⟺
"no evidence of a difference".

A `pairs` argument for selective comparisons exposed a conceptual mismatch:

- **Letter groupings require a complete comparison set.** With an incomplete/irregular graph
  (e.g. compare A–B, A–C, B–D but *not* A–D, C–D, B–C), CLD has no valid input.
- A "untested → `FALSE`" convention makes letters *run*, but silently asserts *not different* for
  pairs that were never tested — conflating "we didn't test" with "no evidence of difference".

Selective comparisons are inherently **difference-centric**: the unit of interest is the contrast
A − B, not the individual means. The honest representation is a long table, one row per comparison
(estimate, SE, statistic, df, p-value), which represents the complete, selective, and irregular
cases uniformly.

### 1.2 The reference / control case → a third, means-centric view

There is a third, common question that neither of the above answers cleanly: **"how does each
treatment compare to a single control?"** (new varieties vs a standard check; treated vs untreated).
The user genuinely wants a **means-centric** output here — the *means* of the control and each
treatment, each flagged for whether it differs from the control — but only a *subset* of comparisons
(everyone vs the control), for which letters are invalid.

The clean mapping of significance onto a mean works **only when each treatment of interest appears in
exactly one comparison against a common reference** (a star pattern). That is precisely the
control-vs-rest / **Dunnett** structure. For arbitrary non-reference subsets a p-value belongs to a
*pair*, not a mean, so "means output" is not well-defined — which is why this is a dedicated
reference mode, not a general "means instead of differences" toggle.

### 1.3 The trichotomy: three questions → three functions

| Function | Which comparisons | Output emphasis | Default `adjust` |
|---|---|---|---|
| `multiple_comparisons()` | all pairs | **means + letters** | `"tukey"` |
| `pairwise_comparisons()` | selected differences / (Phase 2) general contrasts | **differences** | `"holm"` |
| `reference_comparisons()` *(new, Phase 2)* | every level vs one control | **means** vs reference | `"dunnett"` |

Each function has **one identity and one output shape**, so each `print`/`autoplot` method handles a
single shape and never branches on a mode flag. Each function's default adjustment is the *exact*
method for its comparison structure: Tukey is exact for the complete all-pairs set, Dunnett is exact
for all-vs-one-control, and Holm is the always-valid general FWER method for an arbitrary chosen set.

### 1.4 The key reframing: pairwise, reference, and general are all linear contrasts

Pairwise-vs-general is **not** a paradigm fork (unlike means-vs-differences). A pairwise difference
A − B is the contrast `c(A = 1, B = -1)`; a reference comparison T vs control C is
`c(T = 1, C = -1)`; a general contrast A vs mean(B,C) is `c(A = 1, B = -0.5, C = -0.5)`. **All produce
the same output row** (estimate, SE, statistic, df, p). So there is **one engine** underneath, and
`pairwise_comparisons()` and `reference_comparisons()` are both sugar over it. (They differ only in
how comparisons are *specified* and how the result is *presented*.)

### 1.5 The real constraint: which variance you need (this drives phasing)

- **Pairwise differences need only the SED matrix.** `Var(τ_i − τ_j) = SED[i,j]²`.
  `get_predictions()` already returns SED for **every** supported engine (asreml, lm, aovlist, lmer,
  nlme). → Pairwise works everywhere, today, with no new plumbing. **(Phase 1.)**
- **General contrasts need the full prediction vcov `V`**, because `Var(c′τ) = c′ V c`.
- **Exact Dunnett also needs `V`**, because the simultaneous correction uses the *correlation matrix*
  among the all-vs-control contrasts, which is derived from `V`.

Because general contrasts **and** exact Dunnett both need `V`, plumbing `V` through
`get_predictions()` once serves both → they belong in the **same phase (Phase 2)**.

---

## 2. Architecture: three functions, one engine

- **`multiple_comparisons()` / `mct.R`** — stays means-centric. Keeps `adjust` and `by` (both
  means-paradigm-compatible). Selective comparisons are **not** its job (no `pairs` argument; the
  "untested → FALSE" convention is not used). Letters remain its exclusive output. Default
  `adjust = "tukey"`.
- **`pairwise_comparisons()` / `pairwise_comparisons.R`** — difference-centric tidy table.
  **Phase 1 implemented.** Phase 2 adds a general `contrasts =` argument (same output shape) and the
  `include_means` columns. Default `adjust = "holm"`; rejects `"tukey"` and `"dunnett"` (the latter
  needs a reference).
- **`reference_comparisons()` *(new, Phase 2)*** — means-vs-control (Dunnett). Distinct class,
  distinct means-centric `autoplot`. Default `adjust = "dunnett"`; rejects `"tukey"`.
- **Shared internal engine** — a single internal worker computes a block of comparisons from a set of
  contrast/pair specifications (estimate, SE, df, statistic, raw p, multiplicity-adjusted p, CI),
  with the by-group loop. `pairwise_comparisons()` and `reference_comparisons()` both call it; only
  the *specification of which comparisons* and the *presentation* differ. **No duplicated
  orchestration.**
- **`get_predictions()` / `prediction_methods.R`** — Phase 1: used as-is (`predictions`, `sed`, `df`,
  `ylab`, `aliased_names`). **Phase 2: extended to also return the full prediction vcov `V`**
  (optional element, `NULL` when unavailable) — asreml via `predict(..., vcov = TRUE)`, emmeans via
  its vcov; reconstruction fallback (§5.4) for engines that don't expose it.
- **`waldTest.R`** (research/scratch) — *the* general contrast engine reference. **Reuse the kernel,
  not the public API.** Kernel = build contrast matrix → estimate `c′τ`, var `c′Vc`, statistic,
  `p.adjust` over the family, by-group loop. Its positional, asreml-bound `cc = list(...)` interface
  must **not** be exposed on a CRAN function; Phase 2 ports the kernel to be engine-agnostic and
  label-driven. Its `zero` joint tests are a Phase 2 bonus.
- **`compare.R`** (research/scratch) — different paradigm (average-SED HSD/LSD/Bonferroni
  *thresholds*). **Set aside.** Its `avsed` idea could inform a plot annotation someday.

---

## 3. Decisions log

### 3.1 Phase 1 decisions (settled, implemented)

| # | Question | Decision |
|---|----------|----------|
| A | Default `adjust` (pairwise) | **`"holm"`** (FWER, conventional for a small planned set; uniformly more powerful than Bonferroni). **`"tukey"` rejected** (exact only for the complete set — `multiple_comparisons()`'s job). Offer BH/BY/bonferroni/none. |
| — | FDR vs Holm rationale | Holm controls family-wise error; BH controls expected false-discovery proportion among rejections. For a small set of *specifically chosen planned* comparisons → FWER/Holm. BH earns its keep when comparisons are many/exploratory. |
| B | Transformations | **Model scale only + warn** if response looks transformed. A difference of transformed means doesn't back-transform to an original-scale difference; for log/logit it's a ratio. (Ratios → Phase 2.) |
| — | Interaction label syntax | **`:`** (standard R interaction syntax, e.g. `"A:X"`). Pair separator `-`. Hyphen-in-level → list form required (string form errors helpfully). |
| — | Row ordering / `descending` | Input order of `pairs` by default; tri-state `descending` (`NULL` input order, `FALSE` asc, `TRUE` desc), within by-group. |
| — | Output object | **Data frame + attributes** (not a list) — the result *is* one tidy table; metadata in attributes. |
| — | `decimals` | **Not** an argument of `pairwise_comparisons()` (full precision stored); rounding is `print.pairwise_comparisons(x, decimals = 2)` only (consistent with the #175 store-full-precision/round-at-print convention). |

### 3.2 Phase 2 decisions (this discussion)

| # | Question | Decision |
|---|----------|----------|
| R1 | Reference: argument on `pairwise_comparisons()` or its own function? | **Its own function, `reference_comparisons()`.** It wants a means-centric table emphasis and a means-centric plot, which would force `pairwise_comparisons`' `print`/`autoplot` to branch and muddy its difference-centric identity (right as Phase 2 adds `contrasts =`). The trichotomy keeps each function single-shape. Shared internal engine ⇒ no duplication. |
| R2 | A `reference()` helper too? | **No.** With a dedicated `reference` argument the helper adds no capability, and an exported `reference()` would clash in name with the argument and pollute the user namespace (generic name). |
| R3 | Reference sign convention | Reference on the **RHS** of every contrast → `estimate = mean(level) − mean(reference)` (positive = above the control). |
| R4 | Default `adjust` for `reference_comparisons()` | **`"dunnett"`** (exact for all-vs-one-control). Because the function always has a reference, this is a plain default — no `missing()` auto-switch needed. Honour an explicit valid alternative (e.g. `"holm"`); emit a one-line *message* that Dunnett is the exact option. `"tukey"` rejected. |
| R5 | Dunnett df edge case | Exact Dunnett (`mvtnorm`) needs a single (effectively integer) df. When `get_predictions()` returns a non-scalar/fractional df (some KR-style asreml/lme), **fall back to Holm with a warning** (a genuine "couldn't deliver the requested method" case). |
| R6 | `include_means` columns | **On by default**, with a flag, on `pairwise_comparisons()`. Columns `level1.mean`, `level2.mean` inserted **after `estimate`**. In `reference_comparisons()` they are **forced on**; if the user explicitly sets `include_means = FALSE` there, override to `TRUE` and **warn**. |
| R7 | Self-reference row | **Not** in the table (the reference mean is already on every row as `level2.mean`, and in the `reference` attribute). The reference **is** shown in the plot as a distinct point + a reference line. |
| R8 | vcov source | **Use the engine's `V` directly where cheaply available** (asreml `vcov = TRUE`, emmeans); **reconstruct from SED + per-mean SE otherwise** (§5.4). Reconstruction is algebraically exact; direct `V` is preferred mainly to avoid catastrophic cancellation near zero off-diagonals. |
| R9 | `by` vs reference/contrasts | **Orthogonal.** `by` = split into independent families (adjust within each). The function choice = *which* comparisons within each family. They compose (e.g. each Trt vs Control within each Year). |
| C | General contrasts: new function or arg? | **An argument** (`contrasts =`) on `pairwise_comparisons()` — same engine, same output shape. (`reference_comparisons()` is the only spin-out, because of its means-centric presentation.) |

---

## 4. Phase 1 specification — `pairwise_comparisons()` (IMPLEMENTED)

> This section documents what shipped. Kept for reference; deviations from the original draft are
> noted inline.

### 4.1 Purpose

Test a chosen set of pairwise differences between predicted means from a fitted model, with
multiplicity adjustment over the chosen set, optionally split into independent by-groups. Output is a
tidy contrast table (forest-plottable), **not** means-and-letters. Universal across engines because
it needs only the SED matrix that `get_predictions()` already returns.

### 4.2 Signature (as implemented)

```r
pairwise_comparisons(
  model.obj,
  classify,
  pairs      = NULL,      # NULL = all pairs; else selective (see 4.3)
  adjust     = "holm",    # any stats::p.adjust() method; "tukey" rejected
  by         = NULL,      # column(s) of classify to split within
  sig        = 0.05,
  descending = NULL,      # NULL = input order; FALSE = asc by est; TRUE = desc by est
  ...                     # forwarded to get_predictions() (e.g. asreml predict args)
)
# NOTE: no `decimals` argument (deviation from the original draft) — see decision 3.1.
```

### 4.3 `pairs` syntax (two forms, normalised by `parse_pairs()`)

```r
pairs = c("A:X-B:Y", "A:X-C:Z")               # string form (':' joins a cell, '-' separates a pair)
pairs = list(c("A:X","B:Y"), c("A:X","C:Z"))  # list form (robust to awkward level names)
```

- Sign: `estimate = mean(level1) − mean(level2)`, surfaced via `level1`/`level2` columns.
- Hyphen-in-level guard: string form errors and points to the list form.
- Unknown / identical-level pairs → error; reversed/duplicate pairs → warn + de-duplicate
  (order-insensitive).

### 4.4 `by` semantics

`by ⊆ classify`; rows split by `interaction(by, drop = TRUE)`; same `pairs` tested and adjusted
**within each group**; within-group labels reference the **non-`by`** factor levels; all factors
consumed → error; group with < 2 levels → warn + skip.

### 4.5 Computation (per pair, within group)

```
est   = pv[i] - pv[j]
se    = sed[i, j]                         # = sqrt(Var(τi − τj))
df_ij = ndf[i, j] (matrix) else ndf
t     = est / se
p_raw = 2 * stats::pt(-abs(t), df_ij)
```
Then `stats::p.adjust(p_raw, method = adjust)` over the group's family. Per-comparison CI
(*not* simultaneity-adjusted): `est ± qt(1 - sig/2, df_ij) * se`.

### 4.6 Output

`data.frame`, class `c("pairwise_comparisons", "data.frame")`, one row per (group × pair):

`[by col(s)] | level1 | level2 | comparison | estimate | std.error | statistic | df | p.value | conf.low | conf.high`

Full precision stored; attributes `sig_level`, `comparison_method` (= `adjust`), `classify`, `by`,
`ylab`. *(Phase 2 inserts `level1.mean`/`level2.mean` after `estimate` — see §5.5.)*

### 4.7 Methods (as implemented)

- `print.pairwise_comparisons(x, decimals = 2, ...)` — header (classify / adjustment / sig) then the
  table; numeric cols rounded to `decimals`, `p.value` to `signif(., 3)`.
- `autoplot.pairwise_comparisons(object, ..., axis_rotation = 0, label_rotation = 0)` — horizontal
  **forest plot**: `geom_vline(0, dashed)`, `geom_linerange(conf.low..conf.high)`, `geom_point`,
  `theme_bw`, `x = "Estimated difference (<ylab>)"`. Significance shown by an **asterisk prefix** on
  the y-axis label (unfaceted via `scale_y_discrete(labels=)`; faceted via `geom_text` + `facet_wrap`
  on `by`).

### 4.8 Shared helpers reused

`get_predictions()` (predictions/SED/df), `validate_inputs()` (sig/classify), `make_treatment_labels()`
(`:`-joined labels; shared with `multiple_comparisons()` via `process_treatment_names(..., sep)`),
`parse_pairs()` (normalise + de-dup), `stats::p.adjust()`.

---

## 5. Phase 2 specification — general contrasts, `reference_comparisons()`, vcov plumbing

### 5.1 Plumb the full prediction vcov `V` through `get_predictions()`

Add an optional `vcov` element to the `get_predictions()` return list (`NULL` when unavailable):

- **asreml** — request `predict(..., vcov = TRUE)` on the Phase 2 path (only when `V` is actually
  needed — it's an n×n matrix), return the vcov of the predicted means.
- **emmeans-backed engines** (lm, aov, aovlist, lmer, nlme via the emmeans route) — pull `V` from the
  emmeans object.
- **Fallback (any engine with SED + per-mean SE)** — reconstruct `V` (§5.4); exact, engine-agnostic.

Pairwise (Phase 1) continues to need only SED; general contrasts and Dunnett consume `V`.

### 5.2 General `contrasts =` argument on `pairwise_comparisons()`

Label-driven named numeric vectors keyed by level:

```r
pairwise_comparisons(
  model, classify = "Trt",
  contrasts = list("A vs B&C" = c(A = 1, B = -0.5, C = -0.5))
)
```

- List names become row labels (the `comparison` column).
- `pairs = "A-B"` is sugar that expands to `c(A = 1, B = -1)` → **same engine, same output shape** as
  `pairs`. `print`/`autoplot` do not branch.
- Estimate `c′τ`, variance `c′ V c`, statistic, df, raw p, `p.adjust` over the family, by-group loop —
  the ported `waldTest` kernel (engine-agnostic, label-driven).
- `pairs` and `contrasts` are mutually compatible only as far as both reduce to the same engine; the
  exact precedence/coexistence rule is an implementation sub-decision (recommend: allow at most one of
  `pairs` / `contrasts`; error if both supplied).

### 5.3 `reference_comparisons()` — signature & semantics

```r
reference_comparisons(
  model.obj,
  classify,
  reference,                # REQUIRED: a single existing (within-group) level label
  adjust        = "dunnett",# default exact; any p.adjust method also accepted; "tukey" rejected
  by            = NULL,
  sig           = 0.05,
  include_means = TRUE,     # forced TRUE; explicit FALSE → override + warn
  descending    = NULL,
  ...                       # forwarded to get_predictions() (e.g. asreml predict args)
)
```

- Generates the contrasts "every other level − reference" (reference on the RHS).
- `reference` validated as a single existing within-group level; with `by`, it must exist in each
  group (missing in a group → warn + skip, consistent with the < 2-levels rule). `by` must not be the
  factor that contains the reference.
- Output: a `data.frame`, class `c("reference_comparisons", "data.frame")`, **same rich schema** as a
  pairwise table with `include_means` on (rows are `level − reference`, `level2`/`level2.mean` are the
  constant reference). **No self-reference row.** Attributes: `sig_level`, `comparison_method`,
  `classify`, `by`, `ylab`, plus `reference` (label) and `reference_mean` (per by-group) for the plot.
- Transformations: model scale only + warn (inherits Phase-1 behaviour; ratios are a later add).

### 5.4 Dunnett computation

Let `V` be the vcov of the predicted means (from §5.1, or reconstructed). For engines that don't
expose `V`, reconstruct it exactly from the per-mean variances (`V_ii = std.error_i²`) and the SED
matrix:

```
V_ij = (V_ii + V_jj − SED_ij²) / 2
```

For the all-vs-control contrasts `c_i = mean_i − mean_ref`:

```
Var(c_i)    = SED(i, ref)²
Cov(c_i,c_j)= V_ij − V_i,ref − V_j,ref + V_ref,ref
R_ij        = Cov(c_i, c_j) / ( SED(i,ref) · SED(j,ref) )      # correlation matrix
```

Exact **two-sided** Dunnett adjusted p-values and **simultaneous** critical values come from the
multivariate-t with correlation `R` and the (single) residual df:

- adjusted p for comparison `i`: `P(max_k |T_k| ≥ |t_i|)` via `mvtnorm::pmvt`;
- simultaneous critical value for CIs: `mvtnorm::qmvt`.

**Dependency:** add `mvtnorm` to `Suggests` (there is no base-R Dunnett, unlike `ptukey`/`qtukey` for
Tukey; `mvtnorm` is light and already a transitive dep of `multcomp`/`emmeans`).

**df fallback (R5):** `mvtnorm::pmvt` wants a single, effectively integer df. If `df` is a
matrix/fractional, **fall back to `adjust = "holm"` with a warning**.

**`adjust` policy (R4):** default `"dunnett"`; an explicit valid `p.adjust` method is honoured (with a
one-line message noting Dunnett is the exact option); `"dunnett"` is only defined here (error if it
ever reaches `pairwise_comparisons()`); `"tukey"` rejected.

Validate against `multcomp::glht(..., mcp(... = "Dunnett"))` / `emmeans` `trt.vs.ctrl` on the engines
that support them.

### 5.5 `include_means` columns (R6)

- On `pairwise_comparisons()`: default `TRUE`, flag to disable. Insert `level1.mean`, `level2.mean`
  **after `estimate`**, so the row reads as a decomposition (`estimate == level1.mean − level2.mean`).
  `std.error` stays the SE of the **difference**.
- On `reference_comparisons()`: forced `TRUE`; explicit `FALSE` → override + warn.
- Means come straight from `pp$predicted.value` (already available for every engine); no extra
  computation.

### 5.6 `reference_comparisons()` `autoplot` (means-centric)

Visually consistent with the pairwise forest, but means-centric:

- Horizontal layout, one row per non-reference level; **x = the level's mean** (not the difference).
- `geom_point` at each mean; per-mean CI `mean ± qt(1 - sig/2, df) * SE_mean` as `geom_linerange`
  (per-mean SE = `pp$std.error`). *Not* simultaneity-adjusted — the **asterisk tracks the adjusted
  test**, documented as in the pairwise CI caveat.
- A **reference line at the reference mean** (the analogue of the dashed line at 0), plus a
  distinctly-styled **point + label for the reference itself** (R7).
- Asterisk **prefix** on labels of levels significantly different from the reference (reuse the
  Phase-1 prefix machinery).
- `facet_wrap(~ by)` when `by` is present (reference line per facet).
- `x = "Estimated mean (<ylab>)"`.

### 5.7 Shared internal engine (factoring)

Extract the per-block computation into one internal worker, e.g.
`compute_comparison_block(pp, sed, df, V = NULL, specs, adjust, sig, ...)` where `specs` is a list of
contrast vectors (or `(i, j)` index pairs for the pairwise special case). It returns the block
data frame (estimate, SE, statistic, df, raw + adjusted p, CI). Responsibilities:

- p.adjust methods → per-spec t/SE from SED or `c′Vc`, then `stats::p.adjust`;
- `"dunnett"` → build `R` from `V`, `mvtnorm::pmvt`/`qmvt`, with the §5.4 df fallback.

`pairwise_comparisons()` builds `(i,j)`/contrast specs from `pairs`/`contrasts`;
`reference_comparisons()` builds all-vs-reference specs. Both wrap the same by-group loop. Presentation
(class, `print`, `autoplot`, `include_means` defaulting) stays in each public function.

### 5.8 `by` orthogonality (R9)

Two independent axes: **which comparisons** (function choice) × **grouping** (`by`). `by` is the same
"split into independent families, adjust within each, no pooling" mechanism in all three functions.
Canonical reference example:

```r
reference_comparisons(model, classify = "Trt:Year", reference = "Control", by = "Year")
# within each Year: every Trt vs Control, Dunnett-adjusted within that Year.
```

### 5.9 Files (Phase 2)

| File | Action |
|------|--------|
| `R/prediction_methods.R` | **Edit.** Add optional `vcov` (`V`) to each `get_predictions()` method (`NULL` when unavailable); asreml `vcov = TRUE` on demand; reconstruction fallback. |
| `R/pairwise_comparisons.R` | **Edit.** Add `contrasts =` and `include_means`; factor out the shared engine. |
| `R/reference_comparisons.R` | **New.** `reference_comparisons()`, `print.reference_comparisons`. |
| `R/autoplot.R` | **Edit.** Add `autoplot.reference_comparisons` (means-centric). |
| `R/utils.R` (or a new `R/comparisons_engine.R`) | **New/Edit.** Shared `compute_comparison_block()` + Dunnett kernel. |
| `tests/testthat/test-reference_comparisons.R` | **New.** Unit + vdiffr. Validate Dunnett vs `multcomp`/`emmeans`. |
| `tests/testthat/test-pairwise_comparisons.R` | **Edit.** `contrasts`, `include_means` cases. |
| `DESCRIPTION` | **Edit.** Add `mvtnorm` to `Suggests`. |
| `NAMESPACE`, `man/*.Rd` | Regenerated via `devtools::document()`. |
| `NEWS.md` | Phase 2 entries (new `reference_comparisons()`, `contrasts =`, `include_means`). |
| `vignettes/choosing-multiple-comparisons.qmd` | **Edit.** Add a `reference_comparisons()` / Dunnett section and a contrasts note. |
| `_pkgdown.yml` | **Edit.** Add `reference_comparisons` to the Analysis reference. |

### 5.10 Tests to add (Phase 2)

- `reference_comparisons()`: class/shape; means columns present and `estimate == level1.mean −
  level2.mean`; reference on RHS (sign); no self-row; `reference`/`reference_mean` attributes.
- Dunnett numbers vs `multcomp::glht`/`emmeans` `trt.vs.ctrl` (skip-if-not-installed) on `aov`/`lmer`;
  exact-vs-Holm fallback path when df is non-scalar (warning emitted).
- `V` reconstruction equals direct `V` to tolerance on an engine that exposes both.
- `adjust` policy: default dunnett; explicit holm honoured + message; `"tukey"` rejected;
  `"dunnett"` rejected in `pairwise_comparisons()`.
- `include_means`: pairwise default on, flag off; reference forces on + warns on explicit FALSE;
  column placement after `estimate`.
- `by` composition: reference within each group; reference missing in a group → warn + skip.
- `contrasts =`: a general contrast (A vs mean(B,C)) matches a hand-computed `c′τ` / `c′Vc`; `pairs`
  sugar equals the equivalent `contrasts`.
- `print` snapshot; `autoplot.reference_comparisons` vdiffr doppelganger (respect `ggplot2_variant()`).

---

## 6. Future work & out of scope

Two kinds of item live here: **planned** work that needs doing but is deferred to its own effort
(too large to fold into the post-implementation review), and things that are **out of scope** (not
planned, or explicitly never).

### Planned (needed, deferred to their own work)

- **Back-transformation / ratios for transformed responses.** *(Substantial — needs its own design
  pass and dedicated implementation. Raised in the review as REV-10.)*
  Comparisons are currently reported on the **model scale** only (documented, with a warning when the
  response looks transformed). This is a real limitation, not a nicety, and it bites
  `reference_comparisons()` hardest: its `level1.mean`/`level2.mean` stay on the transformed scale, and
  the natural "treatment vs control" quantity on the original scale — a **ratio** for log/logit links —
  is unavailable. The work spans all three functions (pairwise differences, reference comparisons,
  general contrasts) and must decide: ratios + their (delta-method or profile) intervals for
  log/logit; a clearly-documented "model-scale only" fallback for sqrt/power/inverse/arcsin where a
  difference has no clean original-scale meaning; how back-transformed means interact with the
  difference/contrast columns; and how `mct`'s existing `trans`/`offset`/`power` machinery should (or
  should not) be reused. Tracked as its own task — **do not** attempt as a review fix.
- **Joint multi-df "zero" tests** from the `waldTest` kernel. **Reference implementation available:**
  the scratch file `waldTest.R` (asreml, works directly from `predict(model, classify, vcov =
  TRUE)$vcov`) implements exactly this — the `type = "zero"` branch computes
  `W = τ′Z′(Z V Z′)⁻¹ Z τ ~ χ²_q` (or `F_{q,ν}/q`). It needs the full `V`, so it pairs with the
  direct-vcov path (REV-14). Use it as the blueprint if/when this is built.

### Out of scope (not planned)

- **Simultaneity-adjusted CIs** for the non-Dunnett methods (Bonferroni-consistent intervals).
- **Letter groupings** — never; that remains `multiple_comparisons()`'s exclusive job.
- One-sided Dunnett (`trt > ctrl` / `trt < ctrl`) variants, if requested.

---

## 7. Post-implementation review (adversarial sanity check vs `mct.R`)

Findings from reviewing `pairwise_comparisons.R` / `reference_comparisons.R` and their `autoplot`
methods against `multiple_comparisons()`. **Status: proposed — awaiting triage.** Tick the box and
record a decision per item.

**Overall verdict:** the trichotomy is well-motivated; the code largely follows `mct`'s conventions
(`validate_inputs()`, `get_predictions()`, `make_treatment_labels()`, the by-loop, store-full /
round-at-print). Not over-engineered for what was asked. Two real bugs and a few consistency/coverage
gaps below.

Severity: **bug** > **robustness** > **accuracy** > **consistency** > **incompleteness** > **nit**.

### High priority

- [x] **REV-1 — `print.pairwise_comparisons()` mislabels the `contrasts` form.** ✅ *Done.*
  *Severity: bug · Priority: High* — [`pairwise_comparisons.R:753`]
  Header is always `"Pairwise comparisons of means"`, but `contrasts =` rows are general contrasts,
  not pairwise differences. Branch the header on whether `level1`/`level2` exist (or store a mode
  flag/attribute).
  **Fix:** added a `comparison_type` attribute (`"pairs"` / `"contrasts"`) set when the object is
  built; `print()` now prints `"Contrasts of means"` for the contrasts form, `"Pairwise comparisons
  of means"` otherwise. Test: *"print header reflects the comparison type"* in
  `test-pairwise_comparisons.R` (asserts the attribute, the right header, and the absence of the
  pairwise header on the contrasts form). *(Note: `autoplot`'s x-axis label "Estimated difference" is
  also arguably loose for a general contrast — left as-is, out of REV-1 scope.)*
- [x] **REV-2 — `c'Vc` can be negative → `NaN` standard error.** ✅ *Done.*
  *Severity: robustness · Priority: High* — [`pairwise_comparisons.R:690`]
  `se <- sqrt(t(wal) %*% V %*% wal)`. Reconstructed `V` isn't guaranteed PSD under floating-point
  cancellation, so a near-degenerate contrast could give a tiny negative quadratic form and a silent
  `NaN`. Guard with `sqrt(max(0, ...))` (and optionally warn if materially negative). `mct` is immune
  (uses SEDs directly).
  **Fix:** `build_contrast_block()` now floors the contrast variance —
  `se <- sqrt(max(0, c'Vc))` — so a contrast whose true variance is ~0 (aliased / null-space) that
  rounds just below zero yields `se = 0` rather than a `NaN`.
  **Decision (scope-down):** the originally-proposed "warn if materially negative" guard was
  **dropped** as unreachable, not just deferred. `reconstruct_vcov()` recovers the model's *exact*
  prediction covariance, which is PSD, so `c'Vc` can only ever go *trivially* negative from
  floating-point rounding — never materially. A materially-negative form would require SEDs and SEs
  that didn't come from a common covariance (which no supported engine produces), so a warning for it
  is speculative defensive code. The floor is the whole fix; no helper, flag, or `tol`/`scale`
  machinery. (The existing emmeans-equivalence test already pins the normal path; flooring `max(0, x)`
  is too trivial to warrant its own test.)
  *(Note: a 2-level contrast's variance is exactly `SED²` ≥ 0, so the floor only ever matters for
  ≥3-level contrasts under rounding cancellation — which is non-deterministic and so not asserted
  end-to-end.)*
- [x] **REV-3 — General-contrast df on matrix-df models is an unvalidated, silent heuristic.** ✅ *Done.*
  *Severity: accuracy · Priority: High* — [`pairwise_comparisons.R:692-700`]
  For `aovlist`/`lmer`, `build_contrast_block()` uses `min` of the involved pairwise dfs. Exact for a
  2-level contrast; for ≥3-level contrasts it's ad-hoc and **won't match** emmeans' Satterthwaite df.
  `contrasts` was validated only on `aov` (scalar df). At minimum **warn** when contrasts meet
  matrix-df; better, document as approximate (or restrict).
  **Fix (chosen: solve properly, not warn/restrict — user decision "all emmeans-backed engines"):**
  the `min`-of-pairwise-df heuristic is **deleted**. For every emmeans-backed engine (`aov`, `lm`,
  `lme`, `lmer`, `aov+Error`), contrasts now delegate to `emmeans::contrast()` on the model's
  **reference grid**, giving the exact estimate, SE *and* df (Satterthwaite / Kenward-Roger /
  containment) for any linear contrast. Plumbing: `get_predictions.lm` and `get_predictions.aovlist`
  now return the kept `emmGrid` as `emmeans_grid`; `pairwise_comparisons()` builds `emm_info` (grid +
  `pp_to_grid` label-matched mapping + grid size) and threads it into `build_contrast_block()`, which
  branches: emmeans-delegate when a grid is present, SED-reconstruction (scalar df) for `asreml`. No
  new dependency — `emmeans` is already in **Imports** and already produces these predictions. The
  previously-untested matrix-df contrast path now has a test (*">2-level contrast on a matrix-df model
  uses the exact emmeans df"*, validating estimate/SE/df against `emmeans::contrast()` on
  `aov(weight ~ Diet + Error(Chick))`). aov/lm/lme/asreml contrast behaviour is numerically unchanged
  (now exercised through the grid path, still matches the existing emmeans-equivalence tests).
  *(asreml df: see note under REV-3a below.)*

- [x] **REV-3a — asreml contrast df is the term-level `denDF`, not per-contrast. (decision/doc)** ✅ *Resolved: option (a).*
  *Severity: accuracy / doc · Priority: Low* — `get_predictions.asreml` ([prediction_methods.R:134-164])
  For `asreml`, `get_predictions()` already pulls the term's Kenward-Roger-style **denominator df**
  from `asreml::wald(..., denDF = "default")` (falling back to residual df with a warning if the term
  isn't fixed). `build_contrast_block()` uses that single `denDF` for every contrast within the term —
  the conventional, defensible choice (all linear contrasts within one fixed term share the term
  denominator df). asreml does **not** expose a per-contrast approximate df the way emmeans does, so a
  contrast-specific denDF would need bespoke KR computation and a **licensed asreml** to author/verify
  (parallel to REV-14).
  **Decision (option a — accept + document):** the user-supplied scratch function `waldTest.R`
  (asreml, working from `predict(..., vcov = TRUE)`) was reviewed and **confirms there is nothing
  better to lean on** — by default it does an asymptotic Wald χ² test (no denominator df at all), and
  its F-test path uses a single `df_error = model$nedf` (raw residual df) for every contrast. Our term
  `denDF` is *finer* than that `nedf`. So asreml contrasts keep the term `denDF`; this is now
  documented in a new "Standard error and degrees of freedom for `contrasts`" section in the
  `pairwise_comparisons()` roxygen (emmeans engines → exact emmeans df; asreml → term `denDF`). No
  code change. A per-contrast denDF remains a possible licensed-only future item but is not planned.
- [x] **REV-5 — No `rlang::check_dots_used()`.** ✅ *Done.*
  *Severity: consistency / footgun · Priority: High* — both functions
  `mct` guards (`mct.R:286`); the new functions don't, so a misspelled argument
  (`adjut = "none"`) is silently swallowed into `...` and ignored. Add the same guard (note the
  trade-off: legitimate-but-unconsumed `...` args would then error too, as in `mct`).
  **Fix:** added `rlang::check_dots_used()` as the first statement of both `pairwise_comparisons()`
  and `reference_comparisons()` (mirrors `mct.R:286`). A misspelled argument now errors with rlang's
  "Arguments in `...` must be used / Did you misspell an argument name?" rather than being ignored.
  Tests added to both suites (*"a misspelled argument is caught (check_dots_used)…"*). Behaviour for
  legitimate ASReml-R `predict()` args is preserved — `get_predictions.asreml()` consumes the dots, so
  they count as used (same mechanism, and same minor limitation, as `mct`).

### Medium priority

- [x] **REV-6 — Aliased levels not surfaced in the output object.** ✅ *Done (attribute + better error + consistent print note).*
  *Severity: consistency / incompleteness · Priority: Medium* — [`pairwise_comparisons.R:178-183`]
  `mct` captures `result$aliased_names` and reports them (object + print, `mct.R:326`). The new
  functions discard them — the `get_predictions()` warning still fires, but the object has no record.
  Consider an `aliased` attribute + a print note.
  **Implemented:**
  * **Attribute:** `pairwise_comparisons()` and `reference_comparisons()` now store
    `attr(out, "aliased")` (only when aliasing occurred) — parity with `mct`, and makes the existing
    `process_aliased()` warning's "saved in the output object" promise true.
  * **Clear error on a named aliased level:** new internal `stop_unknown_levels()` splits requested-
    but-missing levels into *aliased* (dropped, not estimable) vs *genuinely unknown* and reports each
    appropriately, used by `parse_pairs()` and `parse_contrasts()`; `reference_comparisons()` gives the
    same clear message when the `reference` itself is aliased. Previously these all said the misleading
    "Unknown level: X". (Unique to the new functions — `mct` can't name levels.)
  **Reporting-design decision (revisited after a dedicated discussion — *reverses* the initial "no
  print note"):** the runtime warning and the print note do *different* jobs — the warning flags a
  one-time computation *event*, the print note flags that the displayed table is *incomplete*
  (persists when the object is reloaded and reprinted). Silent incomplete tables are the worse failure
  mode for a teaching-oriented package, so we keep **both**, made consistent and terse:
  * New shared helper `aliased_note()` (in `utils.R`) used by **all three** print methods
    (`print.mct`, `print.pairwise_comparisons`, `print.reference_comparisons`) so wording is identical.
    Lists the levels when few; **collapses to a count** (pointing to the `aliased` attribute) once
    there are more than `max_show` (= 6). `print.mct` was refactored onto this helper with its
    small-`n` wording preserved exactly (existing mct `expect_output` tests still pass).
  * `process_aliased()` warning likewise made terse: identical wording for `n == 1` and `2 ≤ n ≤ 6`
    (existing mct warning tests preserved), collapsed to a count for `n > 6` (the "very large warning
    block" the user flagged).
  Tests: both suites (empty-cell factorial → `A:Y` aliased) assert the attribute, exclusion from the
  comparison set, the clear aliased error for a named level / reference, the print note, and that a
  genuinely-unknown level still says "Unknown level"; `aliased_note()` has a unit test in
  `test-utility_functions.R` covering n = 0/1/2/3, the 6-vs-7 threshold and the collapse. `@returns`
  roxygen updated for both functions. `mct` behaviour unchanged for realistic counts.
- **REV-10 — Back-transformation / ratios for transformed responses.** ➜ *Moved out of the review:
  too large for a review fix. Now tracked as planned future work in §6 ("Planned"). Not a defect to
  patch here — needs its own design + implementation effort.*
- [x] **REV-11 — `by` + missing level: error vs warn-and-skip asymmetry.** ✅ *Done (unified on lenient).*
  *Severity: consistency · Priority: Medium* — `parse_pairs`/`parse_contrasts` vs `reference_comparisons`
  A `pairs`/`contrasts` entry naming a level absent from one `by` group **errors** the whole call,
  whereas `reference_comparisons` **warns and skips** that group. Pick one behaviour across the three.
  **Decision (user chose lenient warn-and-skip):** all three now warn-and-skip a comparison that a
  particular by-group cannot compute (an unbalanced design where a level is absent from some group),
  while still computing the rest — instead of aborting the whole call. **Implementation:**
  `pairwise_comparisons()` now validates the user-named levels **once, globally** against the full
  estimable-level set (`parse_pairs`/`parse_contrasts` called on `unique(within_label)`), so a typo or
  an aliased level still hard-errors up front (REV-6 messages preserved). The per-group loop then
  *filters* the canonical pairs/contrasts to those whose levels are all present in the group, emitting
  a per-group warning naming the skipped comparison(s); a group left with nothing to compute is
  skipped (and if *no* group can compute anything, the existing "No comparisons could be computed."
  error fires). `reference_comparisons()` already behaved this way (global error if the reference is
  absent everywhere; warn-and-skip a group missing it) and is unchanged. Test added
  (*"by + a level missing from one group: warn and skip, compute the rest"*): unbalanced `Trt:Site`
  design, asserts the skip warning, that only the computable group is returned, and that a level absent
  everywhere still errors "Unknown level". `by`-semantics roxygen updated.

### Low priority

- [x] **REV-4 — Dunnett df rounded to integer.** ✅ *Done.*
  *Severity: nit / doc · Priority: Low* — [`pairwise_comparisons.R:510`]
  `as.integer(round(df_ij[1]))` for `mvtnorm::pmvt`. A fractional scalar denDF (asreml/KR) is silently
  rounded, so Dunnett vs Holm on the same model use slightly different df. Document the one-line caveat.
  **Finding:** the rounding is *forced* — confirmed `mvtnorm::pmvt`/`qmvt` reject a fractional `df`
  (`'df' is not an integer`), so the multivariate-t path must use an integer. It only ever bites an
  ASReml-R fractional Kenward-Roger `denDF` (`aov`/`lm`/`lme` are integer; `lmer`/`aov+Error` fall back
  to Holm before Dunnett runs). **Fix:** (1) the single-comparison branch (`m == 1`) now uses the
  **exact** df via `stats::pt`/`qt` (which accept fractional df and don't call `mvtnorm`), so a single
  reference comparison under `adjust = "dunnett"` matches the ordinary t-test/Holm exactly; only the
  `m > 1` multivariate-t path keeps the forced integer rounding, with a code comment explaining why.
  (2) Documented the caveat in `reference_comparisons()`'s Dunnett details (rounding applies only for
  ≥2 comparisons on a fractional denDF; the reported `df` column stays the exact value). No behaviour
  change for the common integer-df engines.
- [x] **REV-7 — No CI/significance consistency check.** ✅ *Done.*
  *Severity: consistency · Priority: Low* — `mct` warns (`check_ci_consistency`, `mct.R:846`)
  The new functions have the same per-comparison-CI-vs-adjusted-p tension but only document it. A
  parallel nudge would match `mct`'s helpfulness (judgment call, not a defect).
  **Fix:** new shared helper `note_ci_padjust_mismatch()` (in `utils.R`), called at the end of
  `pairwise_comparisons()` and `reference_comparisons()`. It emits a one-time `message()` when any
  comparison's per-comparison CI disagrees with its adjusted p-value (CI excludes zero but adjusted
  p ≥ `sig`, or the rare reverse). Distinct from `mct`'s `check_ci_consistency()` — that flags the
  letter-vs-CI overlap case; this is the difference-centric CI-vs-adjusted-p case. **Dunnett is never
  flagged** (its intervals are the simultaneous intervals and agree with the test by construction);
  `"none"` never disagrees either, so the note only fires for genuine multiplicity adjustments
  (holm/bonferroni/BH/…) when a borderline comparison actually flips. Uses `message()` (informational,
  suppressible) to match `mct`'s style. Unit test in `test-utility_functions.R` covers the mismatch,
  both agreement directions, and the Dunnett skip; documented in the pairwise "Confidence intervals"
  roxygen.
- [x] **REV-8 — `descending` orders by *estimate*, not by *mean*.** ✅ *Done (conscious, now explicit).*
  *Severity: consistency (documented) · Priority: Low* — [`pairwise_comparisons.R:52-54`]
  Same argument name as `mct` but a different axis (difference vs mean). Documented; flagging only so
  the divergence is a conscious choice.
  **Decision:** keep ordering by the **estimate** (the difference) — appropriate for the
  difference-centric output, and consistent between `pairwise_comparisons()` and
  `reference_comparisons()`. The `descending` roxygen in both now states explicitly that it sorts by
  the comparison estimate, *unlike* `multiple_comparisons()` which sorts by the predicted mean, so the
  divergence is documented rather than surprising. No behaviour change.
- [x] **REV-9 — Style drift vs `mct`.** ✅ *Done (aligned the one that mattered; rest consciously kept).*
  *Severity: nit · Priority: Low*
  `print` doc via `@rdname` vs `mct`'s `@method print mct`; `%notin%` vs `!(x %in% y)`; `@param
  model.obj` says `asreml` vs `ASReml-R`; output label `"A - B"` (spaces) vs `mct`'s internal `"A-B"`.
  **Resolution:**
  * **Naming convention (user-specified):** the model **object type** is written `asreml` (lowercase,
    code font) for consistency with the other types in the lists (`aov`, `lm`, `lme`, `lmerMod`);
    **ASReml-R** (proper name) is used only for the software as a noun (e.g. "ASReml-R `predict()`
    arguments", "ASReml-R does not provide a per-contrast df"). Applied throughout both functions:
    `@param model.obj` reads "An `asreml`, `aov`, ..." in both, and "ASReml-R models" was corrected to
    "`asreml` models" where it denotes the object type, while software-noun usages keep "ASReml-R".
  * **Consciously kept:** `@rdname` for the `print` methods (modern roxygen, fine); `%notin%` (the new
    code is internally consistent, and the user is standardising `mct` onto `%notin%` separately, so
    the new functions are already on the target convention); the `"A - B"` spaced display label is a
    deliberate readability choice for the user-facing `comparison` column (distinct from `mct`'s
    internal `"A-B"`/`"A_B"` keys, which are not displayed). No behaviour change.
- [x] **REV-12 — Dunnett branch lives in the pairwise file but is only reached via `reference_comparisons`.** ✅ *Done (clarifying comment).*
  *Severity: nit / clarity · Priority: Low* — [`pairwise_comparisons.R:430`]
  `build_pairwise_block()` handles `adjust == "dunnett"`, yet `pairwise_comparisons()` rejects that
  value — so the branch is "dead" from the pairwise entry point. Add a clarifying comment, or move the
  shared engine to its own file (`R/comparisons_engine.R`, per §5.7).
  **Fix:** added a comment at the `adjust == "dunnett"` branch in `build_pairwise_block()` explaining
  it is only reached via `reference_comparisons()` (pairwise rejects `"dunnett"` because it is not a
  `stats::p.adjust.methods` value), and that the engine lives here because `reference_comparisons()`
  reuses `build_pairwise_block()`. Chose the comment over a file move — splitting out
  `R/comparisons_engine.R` is churn for no functional gain at this size.

### Test coverage gaps (vs §5.10)

Audit of the §5.10 checklist against the implemented suite (`test-pairwise_comparisons.R`,
`test-reference_comparisons.R`). All other §5.10 bullets are covered; these two are not.

- [x] **REV-13 — `adjust = "dunnett"` is never tested as rejected in `pairwise_comparisons()`.** ✅ *Done.*
  *Severity: incompleteness (coverage) · Priority: Medium* — §5.10 "adjust policy" sub-item
  The policy bullet asks for `"dunnett"` to be rejected by `pairwise_comparisons()` (it is its own
  function via `reference_comparisons()`), but only `"tukey"` and `"nonsense"` rejection are asserted
  ([`test-pairwise_comparisons.R:109-116`]). First **confirm the code actually rejects `"dunnett"`** —
  the shared `build_pairwise_block()` has a live Dunnett branch (REV-12), so verify the
  `pairwise_comparisons()` entry guard refuses it before relying on the test. Then add an
  `expect_error(pairwise_comparisons(..., adjust = "dunnett"), "reference_comparisons")` (or whatever
  the guard message points to). Quick to add.
  **Fix:** previously `"dunnett"` was rejected only by the generic "Invalid `adjust` method" check (it
  isn't a `stats::p.adjust.methods` value). Added a **dedicated** rejection branch mirroring `"tukey"`,
  pointing the user to `reference_comparisons()`: *"`adjust = "dunnett"` is not valid for
  pairwise_comparisons(): the Dunnett test compares every level against a single control. Use
  reference_comparisons() for that."* Test added asserting the error matches `"reference_comparisons"`.
  The REV-12 comment in `build_pairwise_block()` was updated to say pairwise *explicitly* rejects
  `"dunnett"` (no longer "because it is not a p.adjust.methods value").
- [x] **REV-14 — `V`-reconstruction-equals-direct-`V` test is unimplemented (blocked on asreml). 🔶 NEEDS SAM**
  *Severity: accuracy (validation) · Priority: Medium — requires your manual input* — §5.10 "`V`
  reconstruction equals direct `V`" bullet
  The reconstruction (`reconstruct_vcov()`, `V_ij = (V_ii + V_jj − SED_ij²)/2`) is algebraically exact
  and validated indirectly (Dunnett vs emmeans), but the §5.10 bullet wants a direct check against a
  `V` an engine exposes natively. The only engine that does so here is **ASReml-R** (`predict(...,
  vcov = TRUE)`), which is **not licensed in this environment** — so this test can only be authored and
  run by Sam on a licensed machine. **Action for Sam:** fit a small asreml model, pull the native
  `vcov`, and assert `reconstruct_vcov(sed, se)` matches it to tolerance. A scaffold has been added
  (`test-reference_comparisons.R`, skipped on CI/CRAN and when asreml is absent) for you to fill in the
  fixture and assertion.
  **Reference implementation:** the scratch file `waldTest.R` already pulls and uses the native
  `predict(model, classify, vcov = TRUE)$vcov` directly (no reconstruction). It is the blueprint for
  both this direct-`V` validation and the §6 joint multi-df "zero" tests — if the direct-`V` path is
  ever wired into `get_predictions.asreml`, `waldTest.R` shows the exact `predict()` call and vcov
  alignment (drop-NA rows + matching `Vcov` subset) to reuse.
