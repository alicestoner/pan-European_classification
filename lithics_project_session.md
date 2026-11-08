# Lithic Technology Project — Session Export

**Date:** November 8, 2026
**User:** Alice E Stoner (astone8@uw.edu)
**Dataset:** Hussain, Shumon T., et al. (2023) *A Pan-European Dataset Revealing Variability in Lithic Technology, Toolkits, and Artefact Shapes ~15-11 Kya* (version 0.1.1) [data set]. Sci Data 10. https://doi.org/10.5281/zenodo.7940337

---

## Research Question

Did specific toolkit traits (e.g., tanged points, Zinken, geometric microliths) spread along identifiable spatial-temporal corridors across Late Glacial Europe, consistent with directional movement of people or transmitted ideas rather than independent local invention?

---

## Hypotheses

### H1 (Research Hypothesis)
The spatial and temporal distribution of [tanged points / Zinken / geometric microliths] across Late Glacial Europe (~15–11 kya) shows a non-random, directional gradient — i.e., trait occurrences are spatially clustered, temporally ordered (older dates concentrated in one region, progressively younger dates in adjacent regions along a consistent axis), and this pattern holds after controlling for site density/sampling effort — consistent with transmission (via migration and/or cultural diffusion) rather than independent local invention.

### H0 (Null Hypothesis)
The spatial-temporal distribution of [trait] is statistically indistinguishable from random with respect to geography and chronology (no significant spatial autocorrelation, no directional chronological gradient), consistent with independent local invention, chance co-occurrence, or patterns driven entirely by sampling/preservation bias rather than transmission.

### Key guidance
- **Do** frame the hypothesis around the observable signature (clustering + directional chronological trend), not a causal claim like "people moved."
- **Do** build sampling/preservation bias explicitly into the null so it isn't trivially rejected for the wrong reasons.
- **Do** keep the hypothesis falsifiable and measurable (clinal/directional gradient in space-time), so it maps onto an actual statistical test (e.g., Mantel test, trend surface analysis, spatiotemporal permutation test).
- **Don't** bundle all three artifact types into a single combined hypothesis unless using a composite index — they may have independently originated and spread via different routes/timeframes.
- **Don't** treat spatial clustering alone as sufficient evidence for transmission — convergent local invention under shared environmental pressures can also cluster without directionality; the directional/chronological ordering is the key discriminating element.

---

## Dataset Structure (from `glimpse()`)

Key columns identified:
- `Timeslice` (chr) — time period code (e.g., "TS1")
- `TaxUnit` (chr) — techno-complex / archaeological culture label (e.g., "LEpigrav", "Feder", "Ahr")
- `Region`, `Site` (chr) — spatial/site identifiers
- `ToolClass` (chr) — only two values found: **"AR"** (Armature) and **"D"** (Debitage) — too broad to isolate specific trait types directly
- `Long`, `Lat` (dbl) — site coordinates
- `Timeslice_range_from`, `Timeslice_range_to` (dbl) — numeric chronological range, useful for resolving ambiguous/compound timeslice codes
- Shape metrics (`length_cm`, `width_cm`, `area_cm2`, `circularity`, `eccentricityeigen`, `AD`, `BC`, `amp`, `sym`, `SCALE`) — not needed for the spatial-temporal hypothesis unless also testing shape change along a corridor

### `unique(data$ToolClass)`
```
[1] "AR" "D"
```

### `unique(data$TaxUnit)`
```
[1] "LEpigrav" "AzilTECE" "LPal"     "AzilTSG"  "REpigrav" "Mag"      "Feder"
[8] "EpiAhr"   "ABPFeder" "EpiMag"   "ABPTishn" "Meso"     "FMag"     "UMag"
[15] "EMag"     "PAzil"    "EAzil"    "Sauv"     "LAzil"    "Ham"      "Ahr"
[22] "Brom"     "Swid"     "Bell"     "Hav"      "Magle"    "ESauv"    "Kam"
[29] "Wito"     "Perst"    "Wolk"     "Plu"      "SwidAhr"  "LMagTKE"  "TransMag"
[36] "Fuerst"   "Atzen"    "BeurA"    "EMeso"    "ELab"     "LLab"     "Cres"
[43] "HFeder"   "AFeder"   "FBT"
```

### Inferred trait → TaxUnit mapping (needs verification against Zenodo codebook)
| Trait | Likely TaxUnit codes |
|---|---|
| Tanged points | `Ahr` (Ahrensburgian), `Swid` (Swiderian), `Brom` (Bromme/Lyngby) |
| Zinken | `Feder`, `ABPFeder`, `HFeder`, `AFeder` (Federmesser-groups) |
| Geometric microliths | `Sauv`, `ESauv` (Sauveterrian), `Meso`, `EMeso` (Mesolithic), `Magle` (Maglemosian) |

**Caveat:** this mapping is based on general archaeological convention, not the dataset's own documentation. Must verify against the Zenodo deposit's metadata/codebook before finalizing analysis.

### Timeslice oddity explained
`Timeslice` contains compound codes like `"TS12"`, `"TS13"` in addition to `"TS1"`–`"TS4"`. These are not a plotting artifact — they are genuine values in the raw data, likely representing artifacts whose radiocarbon dates span two adjacent timeslices (can't be resolved to a single bin). This should be confirmed against the dataset documentation. Options for handling:
- **Option A:** Filter to only `TS1`–`TS4` (cleanest, loses some data) — recommended for main figure, with limitation noted in write-up.
- **Option B:** Keep as their own "uncertain" category with relabeled factor levels.
- **Option C:** Reassign using `Timeslice_range_from`/`Timeslice_range_to` numeric columns.

---

## README.qmd — Variables Section

````markdown
## Variables

My hypothesis tests whether specific toolkit traits (tanged points, Zinken,
geometric microliths) show a directional spatial-temporal pattern across
Late Glacial Europe. The variables I need are: ToolClass (trait type),
Region/Site/Long/Lat (spatial location), and Timeslice/Timeslice_range_from/to
(chronological placement).

```{r}
library(dplyr)
library(readr)

data <- read_csv("data/lithic_dataset.csv")  # update path/filename as needed

data %>%
  select(Timeslice, Timeslice_range_from, Timeslice_range_to,
         Region, Site, Long, Lat, ToolClass) %>%
  slice(1:5)
```
````

**Why `select()` + `slice()`:**
- `select()` narrows columns to only what the hypothesis needs (trait, location, chronology) rather than printing everything.
- `slice(1:5)` grabs the first 5 rows by position — the direct equivalent of `head()`, as required by the assignment, but more flexible for future use (e.g., slicing by group or condition).

---

## Figure: Spatial-Temporal Distribution Map

Final working code after debugging two `annotate()`/aesthetic errors caused by `borders()` conflicting with faceted/global aesthetics:

```r
library(tidyverse)

data <- read_csv("data/lithic_dataset.csv")

lithics_sub <- data %>%
  mutate(Trait = case_when(
    TaxUnit %in% c("Ahr", "Swid", "Brom") ~ "Tanged points",
    TaxUnit %in% c("Feder", "ABPFeder", "HFeder", "AFeder") ~ "Zinken",
    TaxUnit %in% c("Sauv", "ESauv", "Meso", "EMeso", "Magle") ~ "Geometric microliths",
    TRUE ~ NA_character_
  )) %>%
  drop_na(Trait, Long, Lat, Timeslice) %>%
  filter(Timeslice %in% c("TS1", "TS2", "TS3", "TS4"))  # drop ambiguous compound codes

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", colour = "grey70") +
  geom_point(data = lithics_sub, aes(x = Long, y = Lat, color = Timeslice),
             size = 2, alpha = 0.8) +
  coord_fixed(xlim = c(-10, 40), ylim = c(35, 60)) +
  facet_wrap(~ Trait) +
  scale_color_viridis_d(name = "Time Period") +
  labs(
    title = "Spatial-Temporal Distribution of Toolkit Traits Across Late Glacial Europe",
    subtitle = "Faceted by trait type, coloured by time period",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()
```

**Debugging notes:**
- Original error: aesthetics mismatch between `borders()` layer and lithics data when `aes()` was set globally in `ggplot()`.
- Fix attempt 1: moved `aes()` into `geom_point()` only — still failed because `borders()` itself calls `annotate()` internally, which can break under faceting/certain ggplot2 versions.
- Fix attempt 2 (working): replaced `borders()` with an explicit `geom_polygon()` layer using `map_data("world")`, fully decoupling the basemap data from the lithics data.

**Why this figure fits the hypothesis:** faceting by `Trait` and coloring by `Timeslice` allows a direct visual test of whether each trait independently shows spatial clustering that shifts systematically with time (supporting H1) versus random scatter across all time periods (supporting H0).

---

## Benchmark (Null Model via Permutation)

Assignment prompt: describe a benchmark for the "trends over time or across space" option — compare the real result to a null model built by shuffling dates/labels many times; a real pattern should be stronger than almost all shuffled versions.

### Brainstormed approaches
1. **Shuffle Timeslice labels across fixed site locations** — keep `Long`/`Lat` fixed, randomly reassign `Timeslice` among trait-matching rows, recompute the spatial-temporal statistic (e.g., correlation between longitude and time-order) across 1,000+ shuffles to build a null distribution.
2. **Shuffle site locations across fixed timeslices** — reverse of above; useful if the statistic is framed as "do locations cluster by time" rather than "does time trend with location."
3. **Permutation test on a directionality statistic** — e.g., Mantel correlation (spatial distance matrix vs. temporal distance matrix) or regression slope (longitude ~ time-order); shuffle one side independently many times and compute the fraction of shuffled values as extreme as the real one (≈ p-value).

### Candidate benchmark sentences
> "My benchmark will be a null model built by randomly shuffling the Timeslice labels across my filtered site locations 1,000 times, recalculating the correlation between longitude and chronological order each time, and comparing my dataset's real correlation to this shuffled distribution — a real directional pattern should produce a correlation stronger than at least 95% of the shuffled versions."

> "For each trait (tanged points, Zinken, geometric microliths), I will randomly permute the Timeslice assignments among that trait's site locations many times (e.g., 1,000 iterations) to generate a null distribution of spatial-temporal correlation values, and I will consider my observed pattern meaningful only if it falls in the extreme tail (e.g., top 5%) of that null distribution, indicating the real spatial-temporal gradient is unlikely to have arisen by chance alone."

---

## Outstanding Action Items
- [ ] Verify `TaxUnit` → trait (tanged points / Zinken / geometric microliths) mapping against Zenodo dataset codebook/metadata.
- [ ] Confirm interpretation of compound `Timeslice` codes (e.g., "TS12", "TS13") against dataset documentation.
- [ ] Decide on final handling of ambiguous timeslice records (Option A/B/C above).
- [ ] Select and implement final benchmark statistic (correlation, regression slope, or Mantel test) and write permutation test code in R.
- [ ] Run `lithics_sub %>% count(Trait)` to confirm adequate sample sizes per trait group.
