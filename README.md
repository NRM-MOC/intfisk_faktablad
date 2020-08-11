Integrerad kustfiskeövervakning
================

Detta bibliotek innehåller kod som använts för att generera figurer och
tabeller i [*Faktablad från integrerad kustfiskövervakning*
2020:1](http://www.slu.se/faktablad-kustfisk). Alla beräkningar är
gjorda i
[R](https://www.r-project.org/)/[RStudio](https://rstudio.com/).

Datafiler från de olika övervakningsprogrammen finns i
[data/](https://github.com/NRM-MOC/intfisk/tree/master/data), dessa har
sammanslagits med skriptet
[prepare\_data.R](https://github.com/NRM-MOC/intfisk/blob/master/prepare_data.R)
till filen
[fig\_data.csv](https://github.com/NRM-MOC/intfisk/blob/master/fig_data.csv).

Figurer och tabeller genereras sedan av
[figs\_and\_tables.R](https://github.com/NRM-MOC/intfisk/blob/master/figs_and_tables.R).
Dessa kan sedan placeras i ett MS word-dokument med hjälp av R-markdown
filerna
[figs\_in\_word\_document.Rmd](https://github.com/NRM-MOC/intfisk/blob/master/figs_in_word_document.Rmd)
respektive
[tables\_in\_word\_document.Rmd](https://github.com/NRM-MOC/intfisk/blob/master/tables_in_word_document.Rmd).

``` r
sessionInfo()
```

    ## R version 4.0.0 (2020-04-24)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 18363)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252   
    ## [3] LC_MONETARY=Swedish_Sweden.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=Swedish_Sweden.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.0.0  magrittr_1.5    tools_4.0.0     htmltools_0.4.0
    ##  [5] yaml_2.2.1      Rcpp_1.0.4.6    stringi_1.4.6   rmarkdown_2.2  
    ##  [9] knitr_1.28      stringr_1.4.0   xfun_0.14       digest_0.6.25  
    ## [13] rlang_0.4.6     evaluate_0.14
