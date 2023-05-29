Lisa_Wu_HW_4
================

## Setting up the data

    ## # A tibble: 2,018,477 × 21
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 17 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>

## create workgroup, clear the NAs in examiner_id

``` r
applications <- applications %>%
  mutate(wg = (floor(examiner_art_unit / 10) * 10)) %>%
  mutate(appl_status_date = dmy_hms(appl_status_date)) %>%
  mutate(year = year(appl_status_date)) %>%
  filter(year <= 2017) %>%
  drop_na(examiner_id) 
```

## create a new dataframe shows examiner_id, examiner_art_unit, race, year,tc,and wg. Also, drop all NAs in gender and renamed examiner_id as id, and examiner_art_unit as art unit

``` r
examiners <- applications %>%
  group_by(examiner_id, examiner_art_unit, year) %>%
  drop_na(gender) %>%
  rename(id = examiner_id, art_unit = examiner_art_unit) %>%
  summarise( 
    gender = first(gender),
    race = first(race),
    year = first(year),
    tc = first(tc),
    wg = first(wg)
    )
```

    ## `summarise()` has grouped output by 'id', 'art_unit'. You can override using
    ## the `.groups` argument.

# `{r print} # write_csv(examiners, "applications_cleaned.csv") #`

## show relationship between tc vs gender

``` r
library(ggplot2)
plots <- lapply(unique(examiners$tc), function(tc_value) {
  subset_df <- subset(examiners, tc == tc_value)
  filtered_df <- subset_df[!duplicated(subset_df$id), ]
  ggplot(subset_df, aes(x = factor(year), fill = gender)) +
    geom_bar(position="fill") +
    labs(x = "Year", y = "Frequency", fill = "Gender") +
    scale_fill_manual(values = c("blue", "pink")) +
    ggtitle(paste("TC:", tc_value))
})

# Set the layout to a 2x2 grid
layout(matrix(1:4, nrow = 2, ncol = 2))

# Plot each histogram in a separate panel
for (i in 1:4) {
  plot(plots[[i]])
}
```

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-tcs-by-gender-1.png)<!-- -->![](Lisa_Wu_Hw_4_files/figure-gfm/compare-tcs-by-gender-2.png)<!-- -->![](Lisa_Wu_Hw_4_files/figure-gfm/compare-tcs-by-gender-3.png)<!-- -->![](Lisa_Wu_Hw_4_files/figure-gfm/compare-tcs-by-gender-4.png)<!-- -->

## show relationship between workgroup vs gender

``` r
library(dplyr)
library(tidyr)
# Subset the dataframe for tc = 1600
subset_data <- subset(examiners, tc == 1600)
# Filter out unique wg values
wg_values <- sort(unique(subset_data$wg))
# Generate individual plots for each wg value
for (wg_value in wg_values) {
  # Subset the data for the current wg value
  subset_data_wg <- subset(subset_data, wg == wg_value)
  # Create a new dataframe with counts for each combination of year and gender
  counts <- with(subset_data_wg, table(year, gender))
  # Convert the counts dataframe to long format
  counts_long <- as.data.frame.table(counts)
  # Convert "year" to numeric
  counts_long$year <- as.numeric(as.character(counts_long$year))
  # Expand the data to include all years from 2000 to 2017
  counts_long <- counts_long %>%
    complete(year = min(year):max(year), nesting(gender), fill = list(Freq = 0))
  # Generate the bar plot
  plot <- ggplot(counts_long, aes(x = factor(year), y = Freq, fill = gender)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = c("female" = "steelblue", "male" = "orange")) +
    labs(x = "Year", y = "Gender Count", fill = "Gender", title = paste("Workgroup", wg_value)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # Save the plot as an individual file (optional)
  ggsave(filename = paste("workgroup", wg_value, ".png", sep = ""), plot = plot)
  # Display the plot
  print(plot)
}
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 2 rows containing missing values (`geom_bar()`).
    ## Removed 2 rows containing missing values (`geom_bar()`).

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-1.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-2.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-3.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-4.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-5.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-6.png)<!-- -->

    ## Saving 7 x 5 in image

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-7.png)<!-- -->![](Lisa_Wu_Hw_4_files/figure-gfm/compare-workgroups-by-gender-8.png)<!-- -->

## show relationship between art_units vs gender

``` r
# Select the necessary variables from the examiners dataframe
subset_data <- select(examiners, art_unit, gender, year)
```

    ## Adding missing grouping variables: `id`

``` r
# ratio of gender for each art unit in each year
au_year_ratio <- subset_data %>% 
  group_by(year, art_unit) %>% 
  summarise(percent_female = sum(gender=="female",na.rm = TRUE)/n())
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
# average ratio of gender across 17 years for each art unit
au_ratio <- au_year_ratio %>% 
  group_by(art_unit) %>% 
  summarise(average_percent_female= mean(percent_female))

# create a new graph
graph <- ggplot(data=au_ratio) +
  geom_col(aes(as.factor(x=art_unit), y = average_percent_female)) 

graph
```

![](Lisa_Wu_Hw_4_files/figure-gfm/compare-art_units-and-gender-1.png)<!-- -->
