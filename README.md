# WB_HartJohnstoneForestClassesAI_Sol6_1

Version **0.2.0** matches the original module's implemented non-drainage
six-class rules. This adapts the stand-type definitions of Appendix S3 of
Greuel et al. (2021, doi:10.1002/ecs2.3481), which follow Hart et al. (2019),
Table 1 and section 2.4: biomass replaces basal area and stem density.
Equivalence is not ecological validation. No age, drainage or minimum-biomass
threshold is applied.

## Interface and rules

```r
source("R/classifyHartJohnstoneForest.R")
classes <- classifyHartJohnstoneForest(cohortData, pixelGroupMap)
```

Supply a data.frame/data.table containing numeric `pixelGroup`, factor
`speciesCode`, and finite nonnegative numeric `B`, plus a valued single-layer
terra SpatRaster of nonnegative integer IDs (or missing cells). Inputs are not
modified. Output geometry is unchanged; its layer/variable name is `standtype`.
The helper has no simulation or taxonomy-package dependency. Optional
`classificationTablePath` writes CSV; NULL disables that output.

Species codes use **case-insensitive literal prefix matching**, assigned only
to the first matching group: jack pine, larch, spruce, additional conifer,
then residual nonconifer. Missing and unmatched codes enter the residual,
which need not mean true deciduous trees. Sum biomass across all ages/cohorts;
the denominator includes all biomass. Apply these rules in priority order. Every
threshold is inclusive ($\geq$), without floating-point tolerance adjustments:

| Priority | Code / label | Rule | Appendix S3 rule |
|---:|---|---|---|
| 1 | 4 / jackpine | Jack pine / total biomass $\geq 2/3$ | $\geq$ 66% jack pine |
| 2 | 5 / larch | Larch / total biomass $\geq 0.75$ | $>$ 75% larch |
| 3 | 6 / spruce | Spruce / total biomass $\geq 2/3$ | $\geq$ 66% spruce |
| 4 | 3 / conimix | All four conifer groups / total biomass $\geq 2/3$ | $<$ 66% spruce or jack pine but $\geq$ 2/3 conifer |
| 5 | 1 / deci | $1 - \mathrm{coniferTotB}/\mathrm{totB} \geq 2/3$ (the non-conifer share) | $\geq$ 66% broadleaf deciduous species |
| 6 | 2 / mixed | Otherwise (default class), including zero-total groups | $<$ 66% broadleaf and $<$ 66% conifer |

Differences from Appendix S3 that remain: thresholds use 2/3 where the appendix
writes 66%; larch is inclusive at 0.75 where the appendix says "> 75%"; the
deciduous class is the share of all biomass outside the four conifer groups
(including unrecognised or missing species codes), not the share of broadleaf
species; mixed is the default for everything else, so it also receives stands
that the appendix leaves undefined; and biomass replaces basal area and stem
density.

Default jack pine is `Pinu_ban`, not all pines. Other pines and fir contribute
to conifer mix unless a higher-priority rule applies. A 70% larch stand is
conifer mix. Each species parameter accepts an empty character vector to match
nothing; entries cannot be missing, blank or whitespace-padded.

Zero map IDs with cohorts may receive a class. IDs without cohorts become
NoData, as do missing raster cells; extra unmapped cohort groups are allowed.
Sol6 retains stricter structural, ID and finite/nonnegative biomass checks
than the original, rather than promising malformed-input error parity.

## SpaDES parameters and timing

Load after `Biomass_core` when coupled. Inputs must be supplied explicitly or
by another module; production execution never silently constructs synthetic
inputs. The simulation object remains
`WB_HartJohnstoneForestClassesAI_Sol6_1_Map`, independently of its `standtype`
layer name. No drainage parameter or input is declared or consumed.

| Parameter | Default |
|---|---|
| `.plots` | `"screen"` |
| `.plotInitialTime` | `NA` |
| `.plotInterval` | `NA` |
| `WB_HJForestClassesTimeStep` | `NA_real_` |
| `.saveInitialTime` | `NA` |
| `.saveInterval` | `NA` |
| `.useCache` | `FALSE` |
| `jackPineSp` | `c("Pinu_ban")` |
| `larchSp` | `c("Lari")` |
| `spruceSp` | `c("Pice")` |
| `otherConiferSp` | `c("Abie", "Calc", "Cham", "Juni", "Pinu", "Pseu", "Sequ", "Taxu", "Thuj", "Tsug")` |
| `writeClassificationCsv` | `FALSE` |

NA classification interval follows core succession, or uses 10 years
standalone. An explicit interval must be finite, positive and match core when
coupled. Classification runs at simulation start and `start + n * interval`,
through simulation end, at priority 8.5, after growth 6 and summary 8.25.
Plot/save events start only when their initial-time parameters are set, with
priorities 8.75/9. Their NA intervals follow classification; explicit intervals
must be finite and positive. As in the original, plot/save reschedule relative
to their event time, independently of classification.

When `writeClassificationCsv = TRUE`, every classification writes
`WB_HartJohnstoneForestClassesAI_Sol6_1_<time padded to three digits>.csv`
to `outputPath(sim)`, using original column/group order, row-name column,
integer-truncated percentages and six-class values. CSV writing is disabled
by default and does not affect the classified raster. **TIFF/QML saving is off
by default**, occurring only on requested save events. A save exports the
latest map rather than performing an extra classification.

## Explicit exports and migration

```r
source("R/writeHartJohnstoneForestMap.R")
writeHartJohnstoneForestMap(classes, outputDirectory, simulationTime = 0)
```

The explicit writer and save events retain the six-class TIFF/QML palette.
Default filenames are
`WB_HartJohnstoneForestClassesAI_Sol6_1_Map_time_<simulation-time>.tif`
and the matching `.qml`; optional writer `filenamePrefix` is independent of
the raster name. Time formatting avoids rounding collisions for these exports.
Existing CSV, TIFF or QML outputs are never overwritten: collisions stop the
run. CSV names follow the original three-digit convention and can therefore
collide at fractional times. Load the matching QML in QGIS via **Layer
Properties → Symbology → Style → Load Style** if needed; GUI rendering remains
unverified.

Removed public parameters: `demoMode`, `demoSuccessionTimestep`, and
`exportDirectory`. Replace these with explicitly constructed inputs,
`WB_HJForestClassesTimeStep`, and simulation `outputPath`, respectively.
Set `.saveInitialTime` (and optionally `.saveInterval`) for requested raster
exports; they no longer happen at every update.

```r
source("R/hartJohnstoneDemoInputs.R")
exampleInputs <- hartJohnstoneDemoInputs()
# Pass exampleInputs as objects to simInit(), or call the helper directly:
exampleMap <- classifyHartJohnstoneForest(exampleInputs$cohortData,
                                        exampleInputs$pixelGroupMap)
```

This explicit deterministic helper preserves supplied inputs, constructs only
missing demonstration inputs, and covers all six classes when neither is
supplied. It illustrates classification, not forest growth.

## Tests and reference

Set `HART_TEST_OUTPUT_DIR` to a new disposable assistant-scratch directory,
then run `source("tests/run-tests.R")` from this module. Tests do not run a full
Biomass simulation. The parity harness accepts an explicit original-module path;
standard tests do not require that module.

Hart, S. J. et al. (2019). Examining forest resilience to changing fire frequency
in a fire-prone region of boreal forest. *Global Change Biology*, 25, 869–884.
doi:10.1111/gcb.14550. Table 1 and section 2.4, p. 873.

Developed with AI assistance (Posit Assistant).
