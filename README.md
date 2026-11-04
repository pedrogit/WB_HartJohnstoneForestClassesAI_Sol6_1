# WB_HartJohnstoneForestClassesAI_Sol6_1

Classifies LandR biomass cohorts into six Hart–Johnstone-style forest classes.
It is an **adaptation** of Hart et al. (2019), Table 1 and section 2.4: biomass
replaces basal area and stem density. It does not apply age, drainage, minimum
biomass, basal-area, or stem-density thresholds.

## Interface

The reusable helper is `R/classifyHartJohnstoneForest.R`:

```r
source("R/classifyHartJohnstoneForest.R")
classes <- classifyHartJohnstoneForest(cohortData, pixelGroupMap)
```

`cohortData` is a `data.frame` or `data.table` with `pixelGroup`, `speciesCode`,
and `B`. Biomass is summed over all cohorts and ages within each `pixelGroup`.
`pixelGroupMap` is a single-layer `terra::SpatRaster` of pixel-group IDs. The
helper obtains the LandR taxonomy with `utils::data()` without attaching LandR,
does not require a simulation, and performs no file input/output other than
loading that package data. It returns a labeled integer `SpatRaster` named
`WB_HartJohnstoneForestClassesAI_Sol6_1_Map` with the original geometry.

## Classification

The denominator is all recognized tree biomass in a pixel group. The dominance
threshold is inclusive: $\geq 0.66$.

| Code | Label | Rule |
|---:|---|---|
| 1 | Deciduous | Broadleaf-deciduous biomass $\geq 66\%$ |
| 2 | Conifer–deciduous mix | Neither broadleaf deciduous nor all conifers reaches $66\%$ |
| 3 | Conifer mix | All conifers $\geq 66\%$ and pine, larch, and spruce each remain below $66\%$ |
| 4 | Pine (jack pine + other pines) | Pooled pine biomass $\geq 66\%$ |
| 5 | Larch | Pooled larch biomass $\geq 66\%$ |
| 6 | Spruce | Pooled spruce biomass $\geq 66\%$ |

Pine includes jack pine, lodgepole pine, and other pines; this broadens the
requested jack-pine class. Larch is a conifer. Single-group conifer classes
take precedence over class 3. Class 3 is a residual conifer class: fir and
other conifers contribute to conifer biomass, but even a fir-dominated stand is
class 3 rather than spruce.

Map cells with ID `0`, `NA`, or `NaN` return `NA` (NoData); there is no class
0. A `NULL` map, malformed IDs, positive map groups without cohorts, zero group
biomass, missing/non-finite/negative `B`, or an unknown positive-biomass species
is an error. Empty cohorts are valid only when the map has no positive IDs.
Extra cohort groups not represented in the map are allowed.

## SpaDES use and exports

Load this module after `Biomass_core`. It consumes `cohortData` and
`pixelGroupMap`, creates `WB_HartJohnstoneForestClassesAI_Sol6_1_Map`, and
initializes that in-memory map in a same-time event at priority 8.5, after core
growth and summary (not during the framework's priority-1 init). It then updates after core
growth, mortality, cohort reclassification, summaries, and dispersal at every
positive `Biomass_core` `successionTimestep` boundary (event priority 8.5).
The initial map is also saved at simulation start (time 0 in the main project),
followed by every positive succession-boundary map.

### Standalone demonstration

Set this module's `demoMode = TRUE` in `simInit()` parameters and load only
`WB_HartJohnstoneForestClassesAI_Sol6_1`. No Biomass_core module is needed.
The default is `FALSE`, so production runs never silently fabricate inputs.
The demonstration is deterministic and static: it illustrates classification
and exports, not forest growth or succession dynamics.

With neither input supplied, `.inputObjects` creates a six-cell EPSG:3005 map
and synthetic cohorts representing all six classes. With cohorts alone, it
creates a map of their positive group IDs. With a map alone, it creates cohorts
for its positive IDs, cycling the six example compositions. With both supplied,
it creates neither. `suppliedElsewhere()` guards each input, including inputs
promised by another module; existing objects are never overwritten. Inconsistent
supplied inputs still fail validation rather than being repaired or replaced.

If `Biomass_core$successionTimestep` is absent, demo mode uses the module's
`demoSuccessionTimestep` (default 1 year). A supplied core timestep takes
precedence, and an invalid supplied core timestep is an error, not a reason to
fall back. The fallback is unavailable when demo mode is off. Synthetic-data
creation and fallback use produce informational messages. Exports follow the
same TIFF/QML naming and collision safeguards; use a separate output directory
for demonstrations so they cannot be confused with real simulation results.

Each update writes to `outputPath(sim)` by default. Set the module's
`exportDirectory` parameter to a nonempty directory to redirect only classifier
exports while retaining upstream cache/output paths. Existing-file protection
also applies to that directory. Filenames are:

```text
WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_<simulation-time>.tif
WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_<simulation-time>.qml
```

The TIFF and same-basename QML sidecar preserve the six labels, stable palette,
integer values, geometry, and transparent NoData. Existing output-name
collisions stop the run rather than overwrite a result. In QGIS, load the TIFF,
then use **Layer Properties → Symbology → Style → Load Style** to select its
matching `.qml` if the style is not applied automatically. Actual QGIS GUI
rendering has not been verified.

## Tests

Set `HART_TEST_OUTPUT_DIR` to a new disposable output directory, then run the
focused tests from this module directory:

```r
source("tests/run-tests.R")
```

The runner covers helper classification and module/export behavior. It does not
run a full Biomass simulation.

## Reference

Hart, S. J., Henkelman, J., McLoughlin, P. D., Nielsen, S. E., Truchon-Savard, A.,
and Johnstone, J. F. (2019). Examining forest resilience to changing fire frequency
in a fire-prone region of boreal forest. Global Change Biology, 25, 869–884.
doi:10.1111/gcb.14550. Classification: Table 1 and section 2.4, p. 873.

Developed with AI assistance (Posit Assistant).
