R Notebook
================

## About

FastconcR is a program to get an fast and easy discriminatory
concentrations for fungi based on serial dilution.

The FastconcR project was developed by Edgar Nieto and Sydney Everhart.
A paper with more details about the program is available from

-   \[XX\] -XXX

##License GPL-3 # Loading packages

``` r
using("tidyverse", "ezec", "broom")
```

    ## Loading required package: tidyverse

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Loading required package: ezec

    ## Loading required package: broom

#Reading serial dilution data which has 4 repeatss

``` r
data.serial.dilution <-
  reading_data_serial_dilution("example_serial_dilution_entry.csv",length_repeats = 4,plug = 0.6) 
data.serial.dilution 
```

# getting EC50 of serial dilution

``` r
hola <- getting_EC50(data.serial.dilution)
```

``` r
hola
```

# Getting RG of serial dilution

``` r
data.RG <- getting_RG(data.serial.dilution)
```

    ## Joining, by = "ID"

``` r
data.RG
```

#Getting the potential Discriminatory concentration

``` r
DC_2(data.RG)
```

#Testing the potential gotten Discriminatory concentration

``` r
test.DC <- lineal_model_1(data.RG, "logEC50","RG",desiredR2 = 0.75)
#if it release results it means it has good R2 the model
test.DC

### Now testing with default r2

test.DC.2 <- lineal_model_1(data.RG, "logEC50","RG")
test.DC.2
```

# Graph of the linear model between LogEC50 and Relative growth

``` r
myplot_model_1(data.RG)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Graph%20of%20the%20linear%20model%201-1.png)<!-- -->

``` r
# Graph of the linear model between LogEC50 and Relative growth included the model and R2 and p value: it depends of the values it might look weird, so it must be modified the function if so

myplot_model_1_label(data.RG)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Graph%20of%20the%20linear%20model%201-2.png)<!-- -->

# It tells you if the Estimate.50DC is good or not based on the R2 of the linear regression between Estimate.50DC and the EC50

``` r
test.2.DC <- lineal_model_2( test.DC, "Estimate.50DC","EC50", desiredR2 = 0.70)
test.2.DC
### Now testing with default r2

test.2.DC.2 <- lineal_model_2( test.DC, "Estimate.50DC","EC50")
test.2.DC.2
```

# Plotting model 2

``` r
myplot_model_2(test.DC)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Graph%20of%20the%20linear%20model%202-1.png)<!-- -->

``` r
# plotting model 2 included the model and R2 and p value: it depends of the values it might look weird, so it must be modified the function if so
myplot_model_2_label (test.DC)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](README_files/figure-gfm/Graph%20of%20the%20linear%20model%202-2.png)<!-- -->
#n the following example the data has 3 repeats per ID #Reading the data
using the discriminatory concentrations of the survey

``` r
data.dis.concentration <- reading_data_discriminatory_concentration("example_discriminatory_concentration_entry.csv", length_repeats = 3, plug = 0.6)
data.dis.concentration

#Getting the Estimate EC50DC  of the survey 

dis.concentration<- getting_EC50D_survey(data.RG, data.dis.concentration, "logEC50","RG")
dis.concentration

#If you have some defined  by groups if you may have

Farmer.fields.isolates <- c(136:442, 455, 456, 466, 468, 470, 471, 478:554, 602:610, 612:635, 671, 672, 682:690, 695:797, 810:823, 834, 835, 848:1024, 1255: 1326, 1491: 1500, 1661:1670, 1831: 2246, 2303: 2342,2362: 2381, 2393: 2573)

baseline.isolates <- c(1: 136, 444: 454, 457: 465, 467, 469, 472:477, 555:601, 611, 636:670, 673: 680, 691:694 , 798: 809,  824: 833, 836: 847)


#Overwriting and adding the groups
dis.concentration.by.groups <- add_groups(filename = dis.concentration, group1 =  baseline.isolates, group2 =  Farmer.fields.isolates)
```

``` r
myplot_comparison_s(dis.concentration.by.groups)
```

![](README_files/figure-gfm/myplot%20comparison-1.png)<!-- -->
