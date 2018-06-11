# ompr.roi 0.8.0

## Breaking changes

* The package exports column and row duals of LPs. Extracting duals might be solver dependent. Only tested with GLPK. This requires version `0.8` of ompr.
* Minimum supported version is `3.4.0` since `slam` also requires `>= 3.4`

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


