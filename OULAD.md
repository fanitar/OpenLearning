Introduction
------------

Open University Learning Analytics Dataset (OULAD) contains data about courses, students and their interactions with Virtual Learning Environment (VLE) for seven selected courses (called modules). Presentations of courses start in February and October - they are marked by "B" and "J" respectively. The dataset consists of tables connected using unique identifiers. Dataset is stored in several csv files. We start off the analysis by declaring various packages that we will use.

``` r
library(tidyverse)
library(data.table)
library(lubridate)
```

Exploratory Data Analysis
-------------------------

We can now start reading in the csv files and explore the data within. The `tidyverse` package is ideally suited for all data wrangling operations. Since one of the csv files (`studentVle.csv`) is 432 MB in size, we will utilize the `fread()` function from the `data.table` package to speed up data ingestion.

``` r
assess <- read_csv("assessments.csv")
courses <- read_csv("courses.csv")
stuAssess <- read_csv("studentAssessment.csv")
stuInfo <- read_csv("studentInfo.csv")
stuReg <- read_csv("studentRegistration.csv")
stuVle <- fread("studentVle.csv")
```

    ## 
    Read 22.9% of 10655280 rows
    Read 72.2% of 10655280 rows
    Read 10655280 rows and 6 (of 6) columns from 0.423 GB file in 00:00:04

``` r
vle <- read_csv("vle.csv")
```

Biggest Problem Area
--------------------

Let's start off by exploring our students.

``` r
glimpse(stuInfo)
```

    ## Observations: 32,593
    ## Variables: 12
    ## $ code_module          <chr> "AAA", "AAA", "AAA", "AAA", "AAA", "AAA",...
    ## $ code_presentation    <chr> "2013J", "2013J", "2013J", "2013J", "2013...
    ## $ id_student           <int> 11391, 28400, 30268, 31604, 32885, 38053,...
    ## $ gender               <chr> "M", "F", "F", "F", "F", "M", "M", "F", "...
    ## $ region               <chr> "East Anglian Region", "Scotland", "North...
    ## $ highest_education    <chr> "HE Qualification", "HE Qualification", "...
    ## $ imd_band             <chr> "90-100%", "20-30%", "30-40%", "50-60%", ...
    ## $ age_band             <chr> "55<=", "35-55", "35-55", "35-55", "0-35"...
    ## $ num_of_prev_attempts <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ studied_credits      <int> 240, 60, 60, 60, 60, 60, 60, 120, 90, 60,...
    ## $ disability           <chr> "N", "N", "Y", "N", "N", "N", "N", "N", "...
    ## $ final_result         <chr> "Pass", "Pass", "Withdrawn", "Pass", "Pas...

``` r
colSums(is.na(stuInfo))
```

    ##          code_module    code_presentation           id_student 
    ##                    0                    0                    0 
    ##               gender               region    highest_education 
    ##                    0                    0                    0 
    ##             imd_band             age_band num_of_prev_attempts 
    ##                    0                    0                    0 
    ##      studied_credits           disability         final_result 
    ##                    0                    0                    0

As an educator, I would be most concerned about students who fail in the courses or those who withdraw.

``` r
table(stuInfo$final_result)
```

    ## 
    ## Distinction        Fail        Pass   Withdrawn 
    ##        3024        7052       12361       10156

As we can see 31% of the students withdraw from the courses. Further, 22% of the students fail in courses. We can now dive a bit deeper into these students who fail or withdraw from courses.

``` r
stuInfo$gender <- as.factor(stuInfo$gender)
stuInfo$final_result <- as.factor(stuInfo$final_result)
stuWF <- stuInfo %>% filter(final_result=='Withdrawn'|final_result=='Fail')

ggplot(stuWF, aes(x=final_result, fill=gender))+geom_bar()
```

![](OULAD_files/figure-markdown_github/unnamed-chunk-5-1.png)

Are there any particular modules that these students are facing problems in?

``` r
stuInfo$code_module <- as.factor(stuInfo$code_module)
ggplot(stuWF, aes(x=code_module, fill=final_result))+geom_bar()
```

![](OULAD_files/figure-markdown_github/unnamed-chunk-6-1.png)

What about the level of education?

``` r
stuInfo$highest_education <- as.factor(stuInfo$highest_education)
ggplot(stuWF, aes(x=highest_education, fill=final_result))+geom_bar()
```

![](OULAD_files/figure-markdown_github/unnamed-chunk-7-1.png)
