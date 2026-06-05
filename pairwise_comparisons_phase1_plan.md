# Plan: `pairwise_comparisons()` — Phase 1

**Status:** Design finalised, not yet implemented.
**Target release:** v2.0.0 (later this year, alongside the `design()` engine swap).
**Branch:** to be implemented on a dedicated feature branch off `dev` (do **not** build on the
current `dev` state in a way that risks the in-progress `multiple_comparisons()` work).

This document captures (1) the conceptual discussion and decisions that led here, and (2) a
detailed Phase 1 implementation plan. It is a planning artefact, not package code — add it to
`.Rbuildignore` (and optionally `air.toml`'s exclude list) if it stays in the repo root.

---

## 1. Background & motivation

`multiple_comparisons()` is **means-centric**: one row per treatment level (mean, SE, CI, letter
group), where the compact letter display (CLD) encodes the *complete* matrix of all pairwise
comparisons. Its implicit contract is that every pair has been compared, so "share a letter" ⟺
"no evidence of a difference".

We added a `pairs` argument to `multiple_comparisons()` to allow selective comparisons, but this
exposed a conceptual mismatch:

- **Letter groupings require a complete comparison set.** With an incomplete/irregular graph
  (e.g. compare A–B, A–C, B–D but *not* A–D, C–D, B–C), CLD has no valid input.
- The "untested → `FALSE`" convention we used makes letters *run*, but it silently asserts
  *not different* for pairs that were never tested — conflating "we didn't test" with "no evidence
  of difference". For an incomplete graph the resulting letters are a misleading artefact.

Selective comparisons are inherently **difference-centric**: the unit of interest is the contrast
A − B, not the individual means. The honest representation is a long table, one row per requested
comparison (estimate, SE, statistic, df, p-value), which represents the complete, selective, and
irregular cases uniformly.

**Decision:** selective pairwise comparisons become a **new function**, not an output mode of
`multiple_comparisons()`. The `pairs` argument and the dubious "untested → FALSE" convention are
removed from `multiple_comparisons()` once `pairwise_comparisons()` lands.

### The key reframing: pairwise *is* a linear contrast

Pairwise-vs-general is **not** a paradigm fork (unlike means-vs-differences). A pairwise
difference A − B is just the contrast `c(A = 1, B = -1)`; a general contrast A vs mean(B,C) is
`c(A = 1, B = -0.5, C = -0.5)`. **Both produce the same output row** (estimate, SE, statistic, df,
p). So there is one engine underneath, and pairwise is sugar over it. Shipping pairwise first does
not paint us into a corner.

### The real constraint: which variance you need

This is the architectural fork that decides phasing:

- **Pairwise differences need only the SED matrix.** `Var(τ_i − τ_j) = SED[i,j]²`.
  `get_predictions()` already returns SED for **every** supported engine (asreml, lm, aovlist,
  lmer, nlme). → Pairwise works everywhere, today, with no new plumbing.
- **General contrasts need the full prediction vcov `V`**, because `Var(c′τ) = c′ V c`.
  `get_predictions()` currently surfaces only SED, not `V`. asreml gives `V` via
  `predict(..., vcov = TRUE)`; emmeans exposes it too; but it must be plumbed through
  `get_predictions()` and may not be clean for all engines.

**Phasing falls out of this:**

- **Phase 1 (this plan):** `pairwise_comparisons()` on the SED matrix — universal, cheap.
- **Phase 2 (later):** general `contrasts =` argument — extend `get_predictions()` to also return
  `V` where available, reuse the `waldTest` kernel, add ratios for log/logit, possibly joint
  multi-df "zero" tests. Same output shape, so `print`/`autoplot` don't branch.

---

## 2. Relationship to existing code

- **`multiple_comparisons()` / `mct.R`** — stays means-centric. Keeps `adjust` and `by`
  (both means-paradigm-compatible). `pairs` is **removed** when `pairwise_comparisons()` ships,
  along with the "untested → FALSE" convention. Letters remain its exclusive job.
- **`get_predictions()` / `prediction_methods.R`** — reused as-is for Phase 1 (returns
  `predictions`, `sed`, `df`, `ylab`, `aliased_names`). Phase 2 will extend it to optionally
  return the full vcov `V`.
- **`waldTest.R`** (research/scratch file) — *the* general contrast engine. **Reuse its kernel,
  not its public API.** Kernel = build contrast matrix → estimate `c′τ`, var `c′Vc`, Wald/F
  statistic, `p.adjust` over the family, by-group loop. Its public `cc = list(list(coef=...,
  comp=...))` interface uses positional indices and is asreml-bound — **do not** expose that on a
  CRAN function. Phase 2 will port the kernel to be engine-agnostic and label-driven. Its `zero`
  joint tests are a Phase 2 bonus.
- **`compare.R`** (research/scratch file) — different paradigm (average-SED HSD/LSD/Bonferroni
  *thresholds*, means-centric). **Set aside** for this function; not the basis for
  `pairwise_comparisons()`. Its `avsed` idea could inform a plot annotation someday.

---

## 3. Decisions log

| # | Question | Decision |
|---|----------|----------|
| A | Default `adjust`? | **`"holm"`** (FWER, conventional for a small set of planned comparisons; uniformly more powerful than Bonferroni). Not `"none"`. Offer BH/BY/bonferroni/none as alternatives. **`"tukey"` is rejected** (it's exact only for the complete set — that's `multiple_comparisons()`' job). |
| — | FDR vs Holm rationale | Holm controls family-wise error (any false rejection); BH controls expected false-discovery proportion among rejections. For a small set of *specifically chosen planned* comparisons you want each claim trustworthy → FWER/Holm. BH earns its keep when comparisons are many/exploratory. Pairwise stats are positively dependent, so BH is approximately valid; BY is dependence-proof but conservative. |
| B | Transformations? | **Defer to Phase 2.** A difference of transformed means doesn't back-transform to an original-scale difference; for log/logit it's a ratio. Phase 1 reports contrasts **on the model scale** and **warns** if the response looks transformed. |
| C | General contrasts: new function or arg? | **An argument** (`contrasts =`) on the same function — same engine, same output shape. Phase 1 implements `pairs`; `contrasts` is reserved for Phase 2. |
| — | Interaction label syntax | **`:`** (standard R interaction syntax, e.g. `"A:X"`), not `_`. Pair separator stays `-`. |
| — | Row ordering | **Input order of `pairs`** by default (within by-group). |
| — | `descending` | **Tri-state, default `NULL`:** `NULL` → input order; `FALSE` → ascending by estimate; `TRUE` → descending by estimate. Always within by-group blocks. |
| — | Output object | **Data frame + attributes** (not a list). Unlike `mct` (which became a list because it accumulated auxiliary structures — a p-value matrix, HSD value), this function has **no auxiliary structure**: the result *is* one tidy table; p-values/estimates/SE/df are columns. Only metadata goes in attributes. Tidyverse-friendly for free. Revisit only if Phase 2 adds a second deliverable (e.g. a joint-test object). |

---

## 4. Phase 1 specification

### 4.1 Purpose

Test a chosen set of pairwise differences between predicted means from a fitted model, with
multiplicity adjustment over the chosen set, optionally split into independent by-groups. Output is
a tidy contrast table (forest-plottable), **not** means-and-letters. Universal across engines
because it needs only the SED matrix that `get_predictions()` already returns.

### 4.2 Signature

```r
pairwise_comparisons(
  model.obj,
  classify,
  pairs      = NULL,      # NULL = all pairs; else selective (see 4.3)
  adjust     = "holm",    # any stats::p.adjust() method; "tukey" rejected
  by         = NULL,      # column(s) of classify to split within
  sig        = 0.05,
  decimals   = 2,
  descending = NULL,      # NULL = input order; FALSE = asc by est; TRUE = desc by est
  ...                     # forwarded to get_predictions() (e.g. asreml predict args)
  # contrasts = NULL      # RESERVED for Phase 2 — same output shape
)
```

### 4.3 `pairs` syntax (two accepted forms, normalised internally)

```r
pairs = c("A:X-B:Y", "A:X-C:Z")                  # string form (':' joins a cell, '-' separates pair)
pairs = list(c("A:X","B:Y"), c("A:X","C:Z"))     # list form (robust to awkward level names)
```

- Labels are the classify factors joined by `:` (e.g. `"A:X"` for a `Trt:Site` interaction).
- **Sign convention:** `estimate = mean(level1) − mean(level2)` — order as written (`"A-B"` → A − B).
  Surfaced via separate `level1`/`level2` columns so the sign is unambiguous.
- **Parse-ambiguity rule:** with `:` as the cell join and `-` as the pair separator, the string form
  is unambiguous **only if no factor level contains `-`**. If a hyphen-containing level is referenced
  via the string form → **error**, directing the user to the list form. The list form is the
  unambiguous escape hatch. (Do **not** carry over the old `"-"→"_"` silent rewrite.)
- Each label must exist in the predictions (within the by-group context, see 4.4); unknown labels →
  error listing the offenders and the available labels.
- A pair with two identical levels → error.
- Reversed/duplicate pairs (`"A-B"` and `"B-A"`) → **warn and de-duplicate** (order-insensitive via
  the existing `normalize_pair()` helper), since duplicates would inflate the adjustment family.

### 4.4 `by` semantics

- `by` must be a subset of the `classify` factors (its columns exist in the predictions).
- Rows are split by `interaction(by, drop = TRUE)`; the **same** `pairs` set is tested **within each
  group**, adjusted **within each group** (no cross-group pooling).
- Inside a group, pair labels reference the **remaining (non-`by`) factor levels** — e.g.
  `classify = "Trt:Site"`, `by = "Site"`, `pairs = c("A-B")` compares Trt levels A vs B within each
  Site. (Mirrors `waldTest`/`compare` within-group labelling.)
- If `by` consumes all `classify` factors (no levels remain to compare) → error.
- A group with fewer than 2 levels → warn and skip that group.

### 4.5 Computation (per pair, within group)

From `get_predictions()` output (`predictions` `pv`, `sed`, `df` `ndf`):

```
est    = pv[i] - pv[j]
se     = sed[i, j]                  # SED is exactly sqrt(Var(τi − τj))
df_ij  = ndf[i, j]  if ndf is a matrix (aovlist/lmer) else ndf
t      = est / se
p_raw  = 2 * stats::pt(-abs(t), df_ij)
```

Then `stats::p.adjust(p_raw_vector, method = adjust)` over the **group's** family.

Per-comparison CI (**not** simultaneity-adjusted in Phase 1):

```
conf.low / conf.high = est ± stats::qt(1 - sig/2, df_ij) * se
```

**Documented caveat:** as with the CI-vs-letters note in `multiple_comparisons()`, a per-comparison
CI may exclude 0 while the adjusted p ≥ sig. (Bonferroni-consistent simultaneous CIs are a possible
later refinement.)

Compute **per requested pair directly** (cheaper than building the full matrix for selective sets);
for `pairs = NULL` iterate all `C(n,2)` pairs within the group.

### 4.6 Output

A `data.frame` with class `c("pairwise_comparisons", "data.frame")`, one row per (group × pair):

| `by` col(s) | `level1` | `level2` | `comparison` | `estimate` | `std.error` | `statistic` | `df` | `p.value` | `conf.low` | `conf.high` |

- `by` columns present only when `by` is used (one column per `by` variable, matching the predictions
  style).
- `comparison` = the `"A - B"` label for printing/plotting.
- `p.value` is the **adjusted** value.
- Row order: by-group blocks first (group order = factor/appearance order), then within group per
  `descending` (NULL = input order of `pairs`).
- **Rounding:** round `estimate`, `std.error`, `statistic`, `conf.low`, `conf.high` to `decimals`.
  **Open sub-decision:** `p.value` rounding — recommend *not* rounding to `decimals` (too lossy);
  use `signif(p, 3)` or round to `max(decimals, 4)`. Pick one at implementation.
- **Attributes:** `sig_level`, `comparison_method` (= `adjust`), `classify`, `by`, `ylab`.

### 4.7 Methods

- **`print.pairwise_comparisons(x, ...)`** — header (`Pairwise comparisons of means`, classify,
  `Adjustment method: holm`, `Significance level: 0.05`) then `print.data.frame` of the table.
- **`autoplot.pairwise_comparisons(object, ...)`** — **forest plot**:
  - `comparison` labels on the y-axis (preserve row order via factor with `levels = unique(...)`);
    `estimate` points; `conf.low`..`conf.high` as horizontal error bars.
  - `geom_vline(xintercept = 0, linetype = "dashed")` reference line.
  - `facet_wrap(~ <by>)` when `by` is present (free scales optional).
  - Optional colour by `p.value < sig` (significant vs not).
  - x-axis label = `Estimated difference (<ylab>)`.
  - Axis/label rotation args mirroring `autoplot.mct()` (`axis_rotation`, `label_rotation`).

### 4.8 Validation / errors

- Model type via `get_predictions()` dispatch (same error as `multiple_comparisons()` for
  unsupported models).
- `classify` checked via the existing `check_classify_in_terms()` path.
- `adjust` validated against `stats::p.adjust.methods`; **`"tukey"` rejected** with a message
  pointing to `multiple_comparisons()`.
- `sig` range check (reuse `validate_inputs()` logic).
- `pairs`: label existence (within group), 2 distinct levels per pair, hyphen-in-level guard,
  duplicate handling per 4.3.
- `by`: columns exist; not all classify factors consumed.
- Aliased levels are dropped upstream by `get_predictions()` (with its warning); a `pairs` entry
  naming a dropped level then fails the "label not found" check — acceptable and informative.

### 4.9 Out of scope for Phase 1 (→ Phase 2)

- General `contrasts =` argument (same output shape; needs full prediction vcov `V` plumbed through
  `get_predictions()`, and the engine-agnostic port of the `waldTest` kernel).
- Transformation back-transformation / ratios for log/logit.
- Joint/multi-df "zero" tests (possible bonus from the `waldTest` kernel).
- Letter groupings (never — that's `multiple_comparisons()`).
- Removing `pairs` from `multiple_comparisons()` — do this **with** the Phase 1 merge so users have
  the replacement, but track it as a separate, deliberate step (it's a behaviour change / possible
  deprecation cycle).

---

## 5. Detailed implementation plan

### 5.1 Files

| File | Action |
|------|--------|
| `R/pairwise_comparisons.R` | **New.** Main function, internal helpers, `print.pairwise_comparisons`. |
| `R/autoplot.R` | **Edit.** Add `autoplot.pairwise_comparisons` (S3 method registered for the existing `ggplot2::autoplot` generic). |
| `tests/testthat/test-pairwise_comparisons.R` | **New.** Unit + snapshot/vdiffr tests. |
| `NAMESPACE`, `man/*.Rd` | Regenerated via `devtools::document()` — do not hand-edit. |
| `NEWS.md` | Add an entry (under the v2.0.0 section when that exists). |
| `DESCRIPTION` | Likely no new deps (`stats`, `ggplot2`, `rlang`, `emmeans` already imported). Confirm. |
| `.Rbuildignore` / `air.toml` | Add this plan file if it stays in the repo. |

### 5.2 Functions to write (in `R/pairwise_comparisons.R`)

1. **`pairwise_comparisons()`** — orchestrator. Flow:
   1. `vars <- validate_inputs(sig, classify, model.obj, trans = NULL)` (reuse; gives sig/classify
      checks and the split `vars`).
   2. Validate `adjust`: `adjust %in% stats::p.adjust.methods` else error; special-case `"tukey"`
      → error pointing to `multiple_comparisons()`.
   3. `result <- get_predictions(model.obj, classify, ...)`; pull `pp`, `sed`, `ndf`, `ylab`,
      `aliased`.
   4. Build canonical `:`-joined labels (see helper `build_labels()` — or parameterise
      `process_treatment_names()` with a `sep` argument; prefer the latter to keep one code path,
      but guard the existing `_` behaviour for `multiple_comparisons()`).
   5. Validate `by ⊆ vars`; compute non-`by` "within-group" factor set; error if empty.
   6. Split row indices into groups (`interaction(pp[by], drop = TRUE)`, or one `"All"` group).
   7. `pairs_norm <- parse_pairs(pairs, within_group_labels)` — see helper. For `NULL`, defer to
      per-group all-pairs expansion (labels differ per group only if levels differ; usually same).
   8. For each group: map each pair to row indices `(i, j)` by within-group label; compute
      `est/se/df_ij/t/p_raw` (per 4.5); `p.adjust` within group; CIs; assemble a per-group block
      data frame (with the `by` columns filled in).
   9. `rbind` blocks; apply ordering (`descending` tri-state, within-group); round; set row names
      `NULL`.
   10. Attach class + attributes; return.

2. **`parse_pairs(pairs, labels)`** *(internal)* — normalise both input forms to a list of
   `c(level1, level2)` character pairs:
   - `NULL` → all `combn(labels, 2)` pairs (as `c(a, b)` with a before b in `labels` order).
   - character vector → split each on `-`; **hyphen-in-level guard**: if any element doesn't split
     into exactly two labels that both exist in `labels`, error → suggest list form.
   - list → validate each element is length-2 character.
   - validate existence, distinctness; de-duplicate via `normalize_pair()` with a warning.
   - return list of ordered `c(level1, level2)` preserving input order.

3. **`build_labels(pp, vars, sep = ":")`** *(internal, optional)* — the `:`-joined label builder, or
   fold into a `process_treatment_names(..., sep = ":")` parameter.

4. **`print.pairwise_comparisons(x, ...)`** — per 4.7.

### 5.3 `autoplot.pairwise_comparisons` (in `R/autoplot.R`)

- Signature mirroring `autoplot.mct` where sensible: `(object, ..., axis_rotation = 0,
  label_rotation = 0)`.
- Build a plotting frame; set `comparison` as an ordered factor to preserve row order.
- Layers: `geom_vline(0, dashed)`, `geom_point`, `geom_errorbarh` (or `geom_linerange` +
  `coord_flip`). Facet by `by` if present. Optional `colour = p.value < sig`.
- Reuse rotation/theming helpers used by `autoplot.mct` for consistency.
- Register: `#' @exportS3Method ggplot2::autoplot pairwise_comparisons` (+ `@importFrom ggplot2 autoplot`
  already present).

### 5.4 Reuse map

| Need | Reuse |
|------|-------|
| Predictions + SED + df per engine | `get_predictions()` (S3) |
| sig / classify validation | `validate_inputs()` |
| Treatment-name labels | `process_treatment_names()` (parameterise `sep`) |
| Order-insensitive pair matching / de-dup | `normalize_pair()` |
| Raw two-sided t p-values | same formula as `calculate_raw_pvalue_matrix()` (per-pair) |
| Multiplicity adjustment | `stats::p.adjust()` |
| Plot theming / rotation | helpers behind `autoplot.mct()` |

### 5.5 Edge cases to cover

- `df` is a matrix (aovlist/lmer/some asreml) → index `ndf[i, j]`; subset `ndf[idx, idx]` per group.
- `sed` may be a `Matrix::*` object (asreml) → indexing `sed[i, j]` still works; coerce to numeric.
- Single-level group → warn + skip.
- Interaction classify with `by` a sub-factor → within-group labels exclude the `by` factor(s).
- Levels containing `-` → list-form required; string form errors helpfully.
- Aliased levels dropped upstream → referenced-but-missing label errors clearly.
- `pairs = NULL` with a large factor → many rows; fine, but the adjustment family is all pairs
  within each group (document).

### 5.6 Tests (`test-pairwise_comparisons.R`)

- Returns object of class `pairwise_comparisons`, is a data frame, expected columns present.
- Single-factor `aov` (iris Species): selective `pairs` produce correct `estimate`/`std.error`;
  spot-check a `p.value` against a hand-computed two-sided t-test using SED and df.
- Sign convention: `"A-B"` gives `mean(A) − mean(B)` (check sign flips when reversed).
- `adjust`: default is `"holm"`; `"none"` equals raw; Bonferroni = `pmin(raw * m, 1)` over the
  family; `"tukey"` errors.
- `pairs = NULL` → all `C(n,2)` rows.
- `by`: per-group families, adjustment within group, within-group label resolution, correct row
  blocks; group with <2 levels warns/skips.
- Interaction classify with `:` labels (string and list forms equivalent).
- Hyphen-in-level → string form errors, list form works.
- Duplicate/reversed pair → warning + de-dup.
- Unknown label / identical-level pair → errors.
- `descending` tri-state: NULL = input order; TRUE/FALSE sort by estimate within group.
- Engine coverage: at least `aov`, `aovlist`, and (skip-if-not-installed) `lmer`/`asreml`/`lme`,
  reusing the pre-fitted `tests/testthat/data/*.Rdata` objects.
- `print` snapshot; `autoplot` vdiffr doppelganger (respect `ggplot2_variant()` split).

### 5.7 Suggested commit/PR sequence (feature branch)

1. `pairwise_comparisons()` core + `parse_pairs()` + `print` method (no plot) + unit tests.
2. `autoplot.pairwise_comparisons` forest plot + vdiffr snapshots.
3. `devtools::document()`, `air format .`, `devtools::check()` clean.
4. NEWS entry.
5. *(Separate, deliberate)* remove `pairs` + "untested → FALSE" from `multiple_comparisons()`;
   update its tests/docs. Consider a deprecation message if any release shipped `pairs`.

---

## 6. Phase 2 forward notes (not for Phase 1)

- **`contrasts =` argument**, label-driven (named numeric vectors keyed by level), e.g.
  `contrasts = list("A vs B&C" = c(A = 1, B = -0.5, C = -0.5))`. List names become row labels.
  `pairs = "A-B"` becomes sugar expanding to `c(A = 1, B = -1)` → same engine, same output shape.
- **Plumb full vcov `V`** through `get_predictions()` (asreml `vcov = TRUE`; emmeans `@V`/`vcov`).
  Gate general contrasts on availability of `V`; pairwise continues to need only SED.
- **Port the `waldTest` kernel** engine-agnostic: estimate `c′τ`, var `c′Vc`, Wald/F, `p.adjust`
  per family, by-group loop. Drop positional `coef` indices in favour of level labels.
- **Ratios for log/logit** back-transformation (and a documented "model-scale only" fallback for
  sqrt/power/inverse/arcsin).
- **Joint multi-df "zero" tests** as an optional bonus from the same kernel.
- Decide whether the general door stays an argument on `pairwise_comparisons()` (current decision C)
  or graduates to a sibling once the name no longer fits.
