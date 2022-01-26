# ompr.roi 1.0.0

* `ompr.roi` is now licensed MIT.
* `LazyData` was removed from the `DESCRIPTION` file as is produced a
  CRAN warning.
* `ROI`'s `status` and `message` information are now part of the returned
  solution and can be accessed calling `additional_solver_output()` on the
  returned value.

## Breaking changes

* In line with `ROI`'s way of encoding a general termination status, the
  `solver_status(result)` is now `"success"` if `ROI` returned `code = 0`,
  otherwise it is `"error"`.

# ompr.roi 0.8.0

## Breaking changes

* The package exports column and row duals of LPs. Extracting duals might be solver dependent. Only tested with GLPK. This requires version `0.8` of ompr.
* Minimum supported R version is `3.4.0` since `slam` also requires `>= 3.4`

## New features

* `ompr.roi` works with other `ompr` backends as well.
* `ompr.roi` always returns a solution, even if the solution status is not optimal. It extracts the `ROI` solution with `force=TRUE`.

# ompr.roi 0.7.0

## Major breaking changes

* The package uses the new sparse matrix interface of `ompr` and `ROI`.
* Minimum supported R version is now `3.3.0`

## Minor changes

* `as_ROI_model` exports an 'ompr' model to `ROI::OP` [Github Issue #9](https://github.com/dirkschumacher/ompr.roi/issues/9)

# ompr.roi 0.6.1

## Bugfix

* Fixed a bug where the `ROI::V_bound` object was incorrectly initialised. This causes an error with `ROI` >= 0.3.0.

# ompr.roi 0.6.0

* First release on CRAN


