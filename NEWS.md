# WB_HartJohnstoneForestClassesAI_Sol6_1

## Development version

### 0.1.3

* Defer initial classification to a same-time event at priority 8.5, after core
  growth and summary; remove the ineffective `.initEventPriority` parameter.
* Add a time-zero, zero-biomass regression and a classifier-only `exportDirectory`
  option so reruns can preserve previous exports without changing upstream paths.

### 0.1.2

* Save the initial GeoTIFF and matching QGIS style at simulation start, including
  time 0, in addition to every succession boundary. Keep overwrite protection.
* Verify the saved baseline, nonzero starts, and time-zero-only runs with all six
  classes and two NoData pixels.

### 0.1.1

* Added opt-in `demoMode`, guarded synthetic inputs through `.inputObjects`, and
  `demoSuccessionTimestep` fallback only when the core timestep is absent.
* Preserve supplied and upstream-promised inputs, including partial-input runs;
  inconsistent inputs still fail validation. Production defaults are unchanged.
* Added standalone SpaDES runs, six-class fixtures, partial-input preservation,
  fallback precedence and failure-path tests: 80 assertions pass in total.

* Added documentation for the biomass-based adaptation of Hart and Johnstone
  forest classes, including the six-class legend, inclusive 66% threshold,
  pooled-pine extension, and residual-conifer behavior.
* Documented standalone `classifyHartJohnstoneForest(cohortData, pixelGroupMap)`
  use, validation behavior, and NoData encoding for map IDs `0`/missing.
* Documented synchronization with positive `Biomass_core` succession boundaries,
  priority 8.5, per-boundary TIFF/QML exports, collision protection, and manual
  QGIS style loading. Actual QGIS GUI rendering remains unverified.
* Implemented standalone classification, SpaDES succession events, TIFF/QML export
  and `tests/run-tests.R`; verified 56 focused assertions without running the real
  Biomass_core module.

Developed with AI assistance (Posit Assistant).
