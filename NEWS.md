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


