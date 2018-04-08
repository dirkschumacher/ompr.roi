# ompr.roi 0.7.0.9001

* `ompr.roi` now works with other `ompr` backends as well.
* Minimum supported version is now `3.4.0` since `slam` also now requires `>= 3.4`
* It now exports column duals of LPs
* `ompr.roi` now always returns a solution, even if the solution status is not optimal. It extracts the `ROI` solution with `force=TRUE`.

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


