# Plan: the comparison functions (`pairwise_comparisons()`, `reference_comparisons()`, general contrasts)

**Status:**
- **Phase 1 — `pairwise_comparisons()`: IMPLEMENTED** on the `feature/pairwise-comparisons`
  branch (off `dev`). Difference-centric tidy table + forest-plot `autoplot`. Ships in the next
  release (currently the `1.5.0` section of `NEWS.md`).
- **Phase 2 — general `contrasts =` + new `reference_comparisons()` + full vcov plumbing +
  `include_means`: DESIGNED (this document), not yet implemented.** Target: a later release (likely
  `v2.0.0`, alongside the `design()` backend swap).

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

## 6. Out of scope / future

- **Ratios for log/logit** back-transformation of contrasts (and a documented "model-scale only"
  fallback for sqrt/power/inverse/arcsin). Applies to pairwise, reference, and general contrasts.
- **Joint multi-df "zero" tests** as an optional bonus from the `waldTest` kernel.
- **Simultaneity-adjusted CIs** for the non-Dunnett methods (Bonferroni-consistent intervals).
- **Letter groupings** — never; that remains `multiple_comparisons()`'s exclusive job.
- One-sided Dunnett (`trt > ctrl` / `trt < ctrl`) variants, if requested.
