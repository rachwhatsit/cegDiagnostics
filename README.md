``` r
chds.df <-read.csv(file = "CHDS.latentexample1.csv")#,stringsAsFactors = F) #note: sst requires factors
head(chds.df)
```

    ##   Social Economic  Events Admission
    ## 1   High     High Average        No
    ## 2    Low      Low    High        No
    ## 3   High     High     Low        No
    ## 4    Low      Low Average        No
    ## 5   High      Low Average        No
    ## 6    Low      Low Average        No

Result is shown with a list corresponding to each stage, beginning indexing at 1. Labels record the edges coming into each stage.

    ## [[1]]
    ## label label label 
    ##   "1"   "1"   "1" 
    ## 
    ## [[2]]
    ##  label  label  label 
    ## "High"   "NA"   "NA" 
    ## 
    ## [[3]]
    ## label label label 
    ## "Low"  "NA"  "NA" 
    ## 
    ## [[4]]
    ##       [,1]   [,2]   [,3]  
    ## label "High" "High" "Low" 
    ## label "High" "Low"  "High"
    ## label "NA"   "NA"   "NA"  
    ## 
    ## [[5]]
    ## label label label 
    ## "Low" "Low"  "NA" 
    ## 
    ## [[6]]
    ##       [,1]      [,2]  [,3]      [,4]      [,5]  
    ## label "High"    "Low" "High"    "Low"     "Low" 
    ## label "High"    "Low" "Low"     "High"    "High"
    ## label "Average" "Low" "Average" "Average" "Low" 
    ## 
    ## [[7]]
    ##       [,1]   [,2]   [,3]      [,4]   [,5]  
    ## label "High" "High" "Low"     "Low"  "Low" 
    ## label "High" "Low"  "Low"     "Low"  "High"
    ## label "High" "High" "Average" "High" "High"
    ## 
    ## [[8]]
    ##       [,1]   [,2]  
    ## label "High" "High"
    ## label "High" "Low" 
    ## label "Low"  "Low"

Prior is shown for each of the situations, indexing from 1. NAs correspond to merged stages

``` r
chds.sst$prior #indexed for
```

    ## [[1]]
    ##      [,1] [,2]
    ## [1,]  1.5  1.5
    ## 
    ## [[2]]
    ##      [,1] [,2]
    ## [1,] 0.75 0.75
    ## 
    ## [[3]]
    ##      [,1] [,2]
    ## [1,] 0.75 0.75
    ## 
    ## [[4]]
    ##      [,1] [,2] [,3]
    ## [1,] 0.75 0.75 0.75
    ## 
    ## [[5]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[6]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[7]]
    ##      [,1] [,2] [,3]
    ## [1,] 0.25 0.25 0.25
    ## 
    ## [[8]]
    ##       [,1]  [,2]
    ## [1,] 0.625 0.625
    ## 
    ## [[9]]
    ##       [,1]  [,2]
    ## [1,] 0.625 0.625
    ## 
    ## [[10]]
    ##      [,1] [,2]
    ## [1,] 0.25 0.25
    ## 
    ## [[11]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[12]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[13]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[14]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[15]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[16]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[17]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[18]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[19]]
    ##      [,1] [,2]
    ## [1,]   NA   NA

Stages is the list of situations that the others have been merged too. (Remember to subtract 1 when looking for corresponding results from Collazzo 2017).

``` r
chds.sst$stages
```

    ## [1]  1  2  3  4  7  8  9 10

#### Global monitor

Score records the staging after the merging ofe ach fo the stages. The final entry in score corresponds to the total Bayes Factor score, also recorded by lik.

``` r
chds.sst$score
```

    ##  [1] -2512.707 -2507.179 -2503.448 -2499.995 -2496.619 -2493.313 -2490.253
    ##  [8] -2487.212 -2484.646 -2482.133 -2479.791 -2478.490

``` r
chds.sst$lik
```

    ## [1] -2478.49

Data record the final counts in each of the situations.

``` r
chds.sst$data
```

    ## [[1]]
    ##      High Low
    ## [1,]  507 383
    ## 
    ## [[2]]
    ##      [,1] [,2]
    ## [1,]  237  270
    ## 
    ## [[3]]
    ##      [,1] [,2]
    ## [1,]   46  337
    ## 
    ## [[4]]
    ##      [,1] [,2] [,3]
    ## [1,]  190  108  255
    ## 
    ## [[5]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[6]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[7]]
    ##      [,1] [,2] [,3]
    ## [1,]  105  158   74
    ## 
    ## [[8]]
    ##      [,1] [,2]
    ## [1,]  235   50
    ## 
    ## [[9]]
    ##      [,1] [,2]
    ## [1,]  273   98
    ## 
    ## [[10]]
    ##      [,1] [,2]
    ## [1,]  213   21
    ## 
    ## [[11]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[12]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[13]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[14]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[15]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[16]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[17]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[18]]
    ##      [,1] [,2]
    ## [1,]   NA   NA
    ## 
    ## [[19]]
    ##      [,1] [,2]
    ## [1,]   NA   NA

The stage key construct is how I coded the trees when I was interested in inputting an elicited tree. tostagekey takes the results from sst and puts them into tbl\_dfs that provides the input for the diagnostic functions. This is almost definitely not the most efficient way to do this. Positions are denoted by w and stages are denotoed by u.

``` r
tostagekey(chds.df, chds.sst)-> chds.stage.key
chds.stage.key
```

    ## [[1]]
    ## # A tibble: 1 x 3
    ##       n pos   stage
    ##   <int> <chr> <chr>
    ## 1   890 w0    u0   
    ## 
    ## [[2]]
    ## # A tibble: 2 x 5
    ##   key   Social     n pos   stage
    ##   <chr> <fct>  <int> <chr> <chr>
    ## 1 High  High     507 w1    u1   
    ## 2 Low   Low      383 w2    u2   
    ## 
    ## [[3]]
    ## # A tibble: 4 x 6
    ##   key      Social Economic     n pos   stage
    ##   <chr>    <fct>  <fct>    <int> <chr> <chr>
    ## 1 HighHigh High   High       237 w3    u3   
    ## 2 HighLow  High   Low        270 w3    u3   
    ## 3 LowHigh  Low    High        46 w4    u3   
    ## 4 LowLow   Low    Low        337 w5    u4   
    ## 
    ## [[4]]
    ## # A tibble: 12 x 7
    ##    key             Social Economic Events      n pos   stage
    ##    <chr>           <fct>  <fct>    <fct>   <int> <chr> <chr>
    ##  1 HighHighAverage High   High     Average    86 w6    u5   
    ##  2 HighHighHigh    High   High     High       32 w7    u6   
    ##  3 HighHighLow     High   High     Low       119 w8    u7   
    ##  4 HighLowAverage  High   Low      Average    90 w6    u5   
    ##  5 HighLowHigh     High   Low      High       65 w7    u6   
    ##  6 HighLowLow      High   Low      Low       115 w8    u7   
    ##  7 LowHighAverage  Low    High     Average    14 w6    u5   
    ##  8 LowHighHigh     Low    High     High       11 w7    u6   
    ##  9 LowHighLow      Low    High     Low        21 w6    u5   
    ## 10 LowLowAverage   Low    Low      Average   105 w7    u6   
    ## 11 LowLowHigh      Low    Low      High      158 w7    u6   
    ## 12 LowLowLow       Low    Low      Low        74 w6    u5

This provides the name of the stages as text strings, beginning indexing at 0, to correspond with Collazzo 2017.

``` r
pull(map_df(chds.stage.key, ~distinct(.x,stage)),stage )-> chds.stages#the stages
```

If we elicited a CEG structure, we can manualy input the information from sst object stored in data. To put them in a different format to work with the diagnostic function, I put this in an object poorly named 'struct.'

``` r
chds.struct <- to.struct(chds.df,chds.stage.key,chds.sst) 
chds.struct
```

    ## [[1]]
    ## # A tibble: 2 x 2
    ##   Social n    
    ##   <chr>  <chr>
    ## 1 High   507  
    ## 2 Low    383  
    ## 
    ## [[2]]
    ## # A tibble: 2 x 2
    ##   Economic n    
    ##   <chr>    <chr>
    ## 1 High     237  
    ## 2 Low      270  
    ## 
    ## [[3]]
    ## # A tibble: 2 x 2
    ##   Economic n    
    ##   <chr>    <chr>
    ## 1 High     46   
    ## 2 Low      337  
    ## 
    ## [[4]]
    ## # A tibble: 3 x 2
    ##   Events  n    
    ##   <chr>   <chr>
    ## 1 Average 190  
    ## 2 High    108  
    ## 3 Low     255  
    ## 
    ## [[5]]
    ## # A tibble: 3 x 2
    ##   Events  n    
    ##   <chr>   <chr>
    ## 1 Average 105  
    ## 2 High    158  
    ## 3 Low     74   
    ## 
    ## [[6]]
    ## # A tibble: 2 x 2
    ##   Admission n    
    ##   <chr>     <chr>
    ## 1 No        235  
    ## 2 Yes       50   
    ## 
    ## [[7]]
    ## # A tibble: 2 x 2
    ##   Admission n    
    ##   <chr>     <chr>
    ## 1 No        273  
    ## 2 Yes       98   
    ## 
    ## [[8]]
    ## # A tibble: 2 x 2
    ##   Admission n    
    ##   <chr>     <chr>
    ## 1 No        213  
    ## 2 Yes       21

``` r
chds.cuts <- colnames(chds.df)
chds.cuts
```

    ## [1] "Social"    "Economic"  "Events"    "Admission"

``` r
renderCEG(chds.stage.key,chds.df)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-b37b4d2f9abc26a2c502">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       rankdir = \"LR\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = \"w0\", fillcolor = \"#DCCA6C\", fontcolor = \"#FFFFFF\"] \n  \"2\" [label = \"w1\", fillcolor = \"#9E50E1\", fontcolor = \"#FFFFFF\"] \n  \"3\" [label = \"w2\", fillcolor = \"#DB6FC3\", fontcolor = \"#FFFFFF\"] \n  \"4\" [label = \"w3\", fillcolor = \"#84E1A5\", fontcolor = \"#FFFFFF\"] \n  \"5\" [label = \"w4\", fillcolor = \"#84E1A5\", fontcolor = \"#FFFFFF\"] \n  \"6\" [label = \"w5\", fillcolor = \"#DE7875\", fontcolor = \"#FFFFFF\"] \n  \"7\" [label = \"w6\", fillcolor = \"#84D2D4\", fontcolor = \"#FFFFFF\"] \n  \"8\" [label = \"w7\", fillcolor = \"#D6CDC0\", fontcolor = \"#FFFFFF\"] \n  \"9\" [label = \"w8\", fillcolor = \"#9CE752\", fontcolor = \"#FFFFFF\"] \n  \"10\" [label = \"winf\", fillcolor = \"#AEA4D6\", fontcolor = \"#FFFFFF\"] \n\"1\"->\"2\" [label = \"High\"] \n\"1\"->\"3\" [label = \"Low\"] \n\"2\"->\"4\" [label = \"High\"] \n\"2\"->\"4\" [label = \"Low\"] \n\"3\"->\"5\" [label = \"High\"] \n\"3\"->\"6\" [label = \"Low\"] \n\"4\"->\"7\" [label = \"Average\"] \n\"4\"->\"8\" [label = \"High\"] \n\"4\"->\"9\" [label = \"Low\"] \n\"4\"->\"7\" [label = \"Average\"] \n\"4\"->\"8\" [label = \"High\"] \n\"4\"->\"9\" [label = \"Low\"] \n\"5\"->\"7\" [label = \"Average\"] \n\"5\"->\"8\" [label = \"High\"] \n\"5\"->\"7\" [label = \"Low\"] \n\"6\"->\"8\" [label = \"Average\"] \n\"6\"->\"8\" [label = \"High\"] \n\"6\"->\"7\" [label = \"Low\"] \n\"7\"->\"10\" [label = \"No\"] \n\"8\"->\"10\" [label = \"Yes\"] \n\"9\"->\"10\" [label = \"No\"] \n\"7\"->\"10\" [label = \"Yes\"] \n\"8\"->\"10\" [label = \"No\"] \n\"9\"->\"10\" [label = \"Yes\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
map(chds.stage.key[-1], ~select(.x,-key))->chds.sk#a little bit of finagling 
chds.sk <- c(chds.stage.key[1],chds.sk)
map(chds.sk, ~select(.x,-pos))->chds.sk.nocol
```

#### Partition monitor
