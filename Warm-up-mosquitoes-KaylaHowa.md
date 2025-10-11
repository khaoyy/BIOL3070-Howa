Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Kayla Howa
2025-10-10

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION & HYPOTHESIS](#study-question--hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
  - [](#section)
- [METHODS](#methods)
  - [Blood Meal DNA Extraction](#blood-meal-dna-extraction)
  - [PCR](#pcr)
  - [Sequencing](#sequencing)
  - [Analysis](#analysis)
  - [First Analysis: Horizontal Host
    Graph](#first-analysis-horizontal-host-graph)
  - [Second Analysis: Viremia Duration
    Plot](#second-analysis-viremia-duration-plot)
  - [Third Analysis: Location Heat
    Maps](#third-analysis-location-heat-maps)
- [DISCUSSION](#discussion)
  - [Interpretation - Second Analysis: Viremia Duration
    Plot](#interpretation---second-analysis-viremia-duration-plot)
  - [Interpretation - Third Analysis: Location Heat
    Maps](#interpretation---third-analysis-location-heat-maps)
  - [Limitations](#limitations)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

This report investigates the transmission of West Nile Virus (WNV) in
Salt Lake City, UT. *Haemorhous mexicanus* are a large host body for the
transmission of WNV in Salt Lake City. We hypothesize that *H.
mexicanus* serve as the primary amplifying hosts for WNV due to their
high viremia duration and large presence in the Salt Lake City area. We
predict that locations with a higher ratio of *H. mexicanus* in our
blood host analysis are also the same locations with elevated positive
tests for WNV.

Utilizing DNA extraction, PCR, and sequencing of mosquitoes, we can
BLAST the sequence to determine what species the mosquito last fed on.
There is a significatnly higher porportion of *H. mexicanus* bloodmeals
compared to all other collected species. Additionally, areas with
increased *H. mexicanus* visitation also see an increase in WNV positive
pools.

Using this information we can determine that there is evidence to
suggest our prediction and hypothesis are correct; the large abundance
of the *Haemorhous mexicanus* and long duration of detectible viremea in
said *H. mexicanus* aid mosquitoes in the positive transmission of the
disease.

# BACKGROUND

West Nile Virus (WNV) touched down in the U.S. in New York in 1999
(Wikimedia Foundation, 2025). WNV is primarily transmitted to people
through the bite of an infected mosquito. The virus naturally cycles
between mosquitoes and birds; mosquitoes become infected after feeding
on infected birds and then transmit the virus to other birds when they
bite. Mosquitoes also spread WNV to humans, horses, and other mammals.
However, these hosts are considered ‘dead-end’ hosts because they do not
develop sufficient levels of the virus in their bloodstream to pass it
on to other mosquitoes, preventing further transmission from them
(Centers for Disease Control and Prevention, 2024; Gemini, 2025).

We capture mosquitoes in either gravid traps or CO2 traps. Then, we
extract the blood meal DNA from them and analyze/sequence the DNA. By
monitoring the animals that host WNV and applying it to viremia duration
data (Kumar et al., 2003), we can predict transmission risks for various
areas and work to reduce/prevent the spread of the virus throughout the
population.

# STUDY QUESTION & HYPOTHESIS

## Questions

What bird species is acting as the primary WNV amplifying host in the
Salt Lake City, UT area?

## Hypothesis

We hypothesize that house finches serve as the primary amplifying hosts
for WNV due to their high viremia duration and large presence in the
Salt Lake City area.

## Prediction

We predict that locations with a higher ratio of house finches in our
blood host analysis are also the same locations with elevated positive
tests for WNV.

## 

# METHODS

### Blood Meal DNA Extraction

By taking mosquitoes that were collected from the two traps types, we
can then extract the DNA by placing a single mosquito in a
microcentrifuge tube along with 100 micro liters of DNA extraction/lysis
buffer. Then, we can mash the mosquito with a pestle.

### PCR

Once the DNA has been extracted from the mosquito, we will run a PCR in
order to amplify the extracted DNA. In each well of a 96 well plate, we
pipette the extracted DNA and a reagent. We also pipette a negative and
positive control into two other empty wells then set up the
thermocycler.

The thermocycler is the machine that runs the PCR by increasing and
decreasing the temperature of our samples for a certain amount of cycles
in order to amplify the DNA. Once the desired run is set, we start it
and wait for it to complete.

### Sequencing

We are able to determine if our samples have any DNA successfully
amplified after the PCR is complete by running a gel electrophoresis. We
place our samples into the gel and the electricity current pulls the DNA
fragments up the gel. This separates our DNA fragments that we amplified
by size with smaller fragments moving further up the gel than larger
fragments. Bands appear on the gel and we can compare where the sample
bands are to our DNA ladder to determine the size of our DNA fragments.

Once we know that we do have DNA, we can sequence it using MinION. By
using electric currents, similar to the gel electrophoresis, we can
determine the sequence of nucleotide sequences based on how the nucleic
acids change the current of the electricity.

### Analysis

Last, with our newly acquired nucleotide sequences, we can paste our
collected sequence into BLASTn to identify what species the DNA belongs
to!

## First Analysis: Horizontal Host Graph

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("/Users/kaylapedersen/Desktop/USU/2025/Fall 2025/CompBio/R/Warm-up-mosquitoes-TEMPLATE_files/bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warm-up-mosquitoes-KaylaHowa_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## Second Analysis: Viremia Duration Plot

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-KaylaHowa_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

## Third Analysis: Location Heat Maps

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)

hm <- counts_matrix %>%
  filter(!is.na(long), !is.na(lat)) %>%
  transmute(
    long, lat,
    `WNV-positive pools (rate)` = loc_rate,
    `House finch blood meals`   = host_House_finch
  ) %>%
  pivot_longer(-c(long, lat), names_to = "metric", values_to = "value") %>%
  mutate(value = ifelse(is.na(value), 0, value))

# plot extent (a small pad around data)
xpad <- diff(range(hm$long))*0.05
ypad <- diff(range(hm$lat))*0.05
xlim <- range(hm$long) + c(-xpad, xpad)
ylim <- range(hm$lat) + c(-ypad, ypad)

ggplot(hm, aes(long, lat)) +
  borders("state", regions = "utah", colour = "grey75", fill = NA) +
  # KDE hotspot, weighted by value
  stat_density_2d_filled(
    aes(weight = value, fill = after_stat(level)),
    contour_var = "ndensity", geom = "polygon",
    bins = 12, alpha = 0.9, adjust = 0.8
  ) +
  # optional faint contours
  stat_density_2d(
    aes(weight = value, color = after_stat(level)),
    contour_var = "ndensity", bins = 12, linewidth = 0.15, show.legend = FALSE
  ) +
  # show sites lightly on top
  geom_point(aes(size = pmax(value, 0.0001)), alpha = 0.5, shape = 21, stroke = 0.2,
             fill = NA, color = "grey30", show.legend = FALSE) +
  coord_quickmap(xlim = xlim, ylim = ylim) +
  facet_wrap(~ metric, nrow = 1) +
  scale_fill_viridis_d(option = "C", direction = -1, name = "Hotspot") +
  guides(color = "none") +
  theme_minimal() +
  labs(x = NULL, y = NULL)
```

![](Warm-up-mosquitoes-KaylaHowa_files/figure-gfm/second-analysis-1.png)<!-- -->

# DISCUSSION

### Interpretation - Second Analysis: Viremia Duration Plot

The second plot shows a horizontal bar graph featuring many different
avian species on the y-axis and a scale of days of detectable viremia
ranging from 0-7 on the x-axis. This graph appears to contain two or
three larger peaks in days of detectable viremia. Specifically, these
peaks come from the *H. mexicanus*, *Bubo virginianus* (Great Horned
Owl), and *Chroicocephalus novaehollandiae scopulinus* (Red-Billed
Gull). Larger peaks suggest a longer amount of time that there is
detectable viremia in these species blood which allows for longer
transmission capabilites from mosquitoes. Longer transmission
capabilities allows for more mosquito feeding and infection
opportunities, which finally would increase the amount of transmited WNV
to other species.

### Interpretation - Third Analysis: Location Heat Maps

This third plot shows two heat maps. The heat map on the left shows
House finch blood meals while the heat map on the right shows
WNV-positive pools (rate). The fact that these two maps can almost
perfectly fit on top of each other suggests that the *H. mexicanus*
could be directly correlated to increases in WNV-positive pools, as the
areas that contain more *H. mexicanus* also contain more WNV-positive
pools.

### Limitations

One limitation could be the locations of the mosquito traps. If most of
the traps are in areas where *H. mexicanus* thrive, then we would have a
bias in our collected data, skewed towards *H. mexicanus*. We would need
to ensure that an equal amount of samples are collected from traps
across SLC area to account for locations where *H. mexicanus* may not be
as abundant.

Second, our data have a small sample size. When you consider how many
mosquitoes are feeding and transmitting WNV, a sample size as small as
ours may not tell the full story of what is occuring. In the future, it
would be ideal to collect many more samples to analyze.

# CONCLUSION

As a reminder, we hypothesized that *Haemorhous mexicanus* serve as the
primary amplifying hosts for WNV due to their high viremia duration and
large presence in the Salt Lake City area. Additionally, we predicted
that locations with a higher ratio of *H. mexicanus* in our blood host
analysis are also the same locations with elevated positive tests for
WNV. Based on evidence of a long detectible viremea in *H. mexicanus*,
and the correlation between House finch blood meals and WNV-positive
rates, there is evidence to suggest our prediction and hypothesis are
correct.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  Wikimedia Foundation. (2025, September 3). West Nile virus.
    Wikipedia. <https://en.wikipedia.org/wiki/West_Nile_virus>

3.  Centers for Disease Control and Prevention. (2024b, May 15).
    Transmission of west nile virus. Centers for Disease Control and
    Prevention.
    <https://www.cdc.gov/west-nile-virus/php/transmission/index.html>

4.  Gemini. Google, version 2.5 Pro. Summarization of parts of the CDC
    West Nile Virus transmission page. Accessed 2025, October 10.
    <https://gemini.google.com/>
