# Design decisions

Questions that have been considered and settled without a code change,
recorded because the code may still look wrong to a fresh reader. Check
here before re-raising one.

## Keep supporting sommer `mmer` models

**Proposed:** Drop `mmer` support (or refit its test fixture), since
sommer has deprecated the legacy `mmer()` interface in favour of
`mmes()`.

**Decided:** Keep supporting `mmer` in
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md),
and keep the informative error in the comparison functions, for as long
as sommer still ships `mmer()`.

**Why:** It is deprecated, not removed — sommer still exports it and
only warns. Users with existing `mmer` fits should keep working. The
saved `model_mmer` in `tests/testthat/data/sommer_models.Rdata` stays as
the test fixture, so those tests don’t depend on a deprecated fitting
function.
