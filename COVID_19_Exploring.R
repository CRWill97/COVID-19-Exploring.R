COVID-19 Exploring & Predictive.R
Cameron Willliams
2020-04-26
library(rvest)
## Loading required package: xml2
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
library(maps)
library(viridis)
## Loading required package: viridisLite
library(devtools)
## Loading required package: usethis
library(gdata)
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## The following object is masked from 'package:stats':
## 
##     nobs
## The following object is masked from 'package:utils':
## 
##     object.size
## The following object is masked from 'package:base':
## 
##     startsWith
##data was taken 4.10.20
url <-"https://www.worldometers.info/coronavirus/country/us"  

corona_table <- url %>% 
  html() %>%
  html_nodes(xpath = '//*[@id="usa_table_countries_today"]') %>%
  html_table()
## Warning: 'html' is deprecated.
## Use 'xml2::read_html' instead.
## See help("Deprecated")
corona_table_df <- data.frame(corona_table)

corona_table_df
##                        USAState TotalCases NewCases TotalDeaths NewDeaths
## 1                     USA Total    987,160       NA      55,413        NA
## 2                      New York    293,991       NA      22,275        NA
## 3                    New Jersey    109,038       NA       5,938        NA
## 4                 Massachusetts     54,938       NA       2,899        NA
## 5                      Illinois     43,903       NA       1,933        NA
## 6                    California     43,541       NA       1,718        NA
## 7                  Pennsylvania     42,708       NA       1,823        NA
## 8                      Michigan     37,778       NA       3,315        NA
## 9                       Florida     31,528       NA       1,074        NA
## 10                    Louisiana     26,773       NA       1,729        NA
## 11                  Connecticut     25,269       NA       1,924        NA
## 12                        Texas     24,981       NA         654        NA
## 13                      Georgia     23,481       NA         916        NA
## 14                     Maryland     18,581       NA         910        NA
## 15                         Ohio     15,963       NA         728        NA
## 16                      Indiana     15,012       NA         813        NA
## 17                   Washington     13,521       NA         749        NA
## 18                     Colorado     13,441       NA         680        NA
## 19                     Virginia     12,970       NA         448        NA
## 20                    Tennessee      9,667       NA         181        NA
## 21               North Carolina      8,994       NA         325        NA
## 22                 Rhode Island      7,439       NA         226        NA
## 23                     Missouri      7,029       NA         283        NA
## 24                      Arizona      6,526       NA         275        NA
## 25                      Alabama      6,418       NA         219        NA
## 26                    Wisconsin      5,911       NA         272        NA
## 27                  Mississippi      5,911       NA         227        NA
## 28               South Carolina      5,490       NA         174        NA
## 29                         Iowa      5,476       NA         118        NA
## 30                       Nevada      4,602       NA         206        NA
## 31                         Utah      4,123       NA          41        NA
## 32                     Kentucky      4,074       NA         208        NA
## 33                     Delaware      4,034       NA         120        NA
## 34         District Of Columbia      3,841       NA         178        NA
## 35                    Minnesota      3,602       NA         272        NA
## 36                     Oklahoma      3,253       NA         195        NA
## 37                       Kansas      3,174       NA         120        NA
## 38                     Nebraska      3,028       NA          56        NA
## 39                     Arkansas      3,001       NA          50        NA
## 40                   New Mexico      2,726       NA          99        NA
## 41                       Oregon      2,311       NA          91        NA
## 42                 South Dakota      2,212       NA          11        NA
## 43                        Idaho      1,897       NA          56        NA
## 44                New Hampshire      1,864       NA          60        NA
## 45                West Virginia      1,044       NA          34        NA
## 46                        Maine      1,015       NA          50        NA
## 47                 North Dakota        867       NA          17        NA
## 48                      Vermont        851       NA          46        NA
## 49                       Hawaii        606       NA          14        NA
## 50                      Wyoming        502       NA           7        NA
## 51                      Montana        448       NA          14        NA
## 52                       Alaska        341       NA           9        NA
## 53                         Guam        141       NA           5        NA
## 54     Northern Mariana Islands         14       NA           2        NA
## 55                  Puerto Rico      1,371       NA          84        NA
## 56 United States Virgin Islands         57       NA           4        NA
## 57              Veteran Affairs      6,860       NA         424        NA
## 58                  US Military      6,213       NA          26        NA
## 59                Navajo Nation      1,540       NA          58        NA
## 60              Federal Prisons      1,118       NA          27        NA
## 61          Grand Princess Ship        103       NA           3        NA
## 62            Wuhan Repatriated          3       NA                    NA
## 63        Diamond Princess Ship         46       NA                    NA
## 64                       Total:    987,160       NA      55,413        NA
##    ActiveCases Tot.Cases.1M.pop Deaths.1M.pop TotalTests Tests.1M.pop
## 1      812,966            2,982           167  5,470,464       16,527
## 2      240,498           14,985         1,135    805,350       41,051
## 3      101,829           12,277           669    223,062       25,114
## 4       43,921            8,043           424    236,100       34,567
## 5       41,364            3,424           151    214,952       16,765
## 6       38,486            1,112            44    494,173       12,623
## 7       40,193            3,339           143    198,593       15,526
## 8       26,121            3,794           333    156,010       15,668
## 9       29,768            1,531            52    346,365       16,815
## 10      10,117            5,741           371    142,056       30,460
## 11      23,280            7,055           537     79,811       22,284
## 12      14,341              896            23    276,021        9,898
## 13      22,534            2,280            89    122,604       11,906
## 14      16,494            3,095           152     96,665       16,102
## 15      15,115            1,371            63    115,783        9,945
## 16      14,185            2,262           122     81,708       12,310
## 17      10,965            1,854           103    175,477       24,057
## 18      12,202            2,430           123     63,274       11,440
## 19      10,707            1,542            53     76,118        9,047
## 20       4,959            1,453            27    147,474       22,173
## 21       7,367              886            32    107,894       10,624
## 22       6,871            7,040           214     53,403       50,542
## 23       6,199            1,154            46     67,017       11,004
## 24       6,181              939            40     64,811        9,330
## 25       6,179            1,319            45     73,626       15,135
## 26       3,326            1,023            47     65,146       11,274
## 27       5,684            1,978            76     60,788       20,339
## 28       1,615            1,108            35     50,761       10,242
## 29       3,754            1,748            38     36,090       11,521
## 30       2,555            1,574            70     45,885       15,699
## 31       3,194            1,354            13     95,702       31,426
## 32       2,744              918            47     48,474       10,917
## 33       3,003            4,249           126     19,249       20,273
## 34       3,006            5,611           260     18,068       26,396
## 35       1,556              652            49     58,987       10,672
## 36         919              830            50     53,012       13,530
## 37       2,551            1,091            41     25,199        8,663
## 38       2,950            1,590            29     22,525       11,826
## 39       1,964            1,003            17     39,551       13,225
## 40       1,977            1,303            47     56,615       27,057
## 41       2,220              566            22     48,964       11,995
## 42       1,011            2,559            13     15,596       18,045
## 43         974            1,124            33     19,361       11,471
## 44       1,254            1,387            45     17,727       13,193
## 45         785              571            19     39,063       21,357
## 46         433              762            38     17,721       13,296
## 47         524            1,153            23     20,447       27,183
## 48         805            1,362            74     14,797       23,676
## 49         104              426            10     29,247       20,567
## 50         174              863            12      7,623       13,102
## 51          95              430            13     12,862       12,347
## 52         115              462            12     16,177       21,905
## 53          10                                       605             
## 54           1                                        45             
## 55         896              405            25     11,633        3,435
## 56           2                                       776             
## 57       6,436                                    83,372             
## 58       4,240                                                       
## 59       1,482                                                       
## 60         582                                                       
## 61         100                                                       
## 62           3                                         3             
## 63          46                                        46             
## 64     812,966            2,982           167  5,470,464       16,527
##                                                          Source
## 1                                                              
## 2                      [1] [2] [3] [4] [5] [6] [7] [8] [9] [10]
## 3                                                   [1] [2] [3]
## 4                                                       [1] [2]
## 5                                               [1] [2] [3] [4]
## 6                                               [1] [2] [3] [4]
## 7                                       [1] [2] [3] [4] [5] [6]
## 8                                                   [1] [2] [3]
## 9                                                       [1] [2]
## 10                                                      [1] [2]
## 11                                                      [1] [2]
## 12                [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [11]
## 13                                                  [1] [2] [3]
## 14                                                          [1]
## 15                                                  [1] [2] [3]
## 16                                                      [1] [2]
## 17 [1] [2] [3] [4] [5] [6] [7] [8] [9] [10] [11] [12] [13] [14]
## 18                                                          [1]
## 19                                                      [1] [2]
## 20                                              [1] [2] [3] [4]
## 21                                              [1] [2] [3] [4]
## 22                                                  [1] [2] [3]
## 23                                  [1] [2] [3] [4] [5] [6] [7]
## 24                                                          [1]
## 25                                                          [1]
## 26                                      [1] [2] [3] [4] [5] [6]
## 27                                                      [1] [2]
## 28                                                      [1] [2]
## 29                                                      [1] [2]
## 30                                                      [1] [2]
## 31                                                          [1]
## 32                                                          [1]
## 33                                                          [1]
## 34                                                          [1]
## 35                                                      [1] [2]
## 36                                                      [1] [2]
## 37                                  [1] [2] [3] [4] [5] [6] [7]
## 38                                                      [1] [2]
## 39                                                          [1]
## 40                                                      [1] [2]
## 41                                                      [1] [2]
## 42                                                          [1]
## 43                                                      [1] [2]
## 44                                                          [1]
## 45                                                          [1]
## 46                                                      [1] [2]
## 47                                                          [1]
## 48                                                          [1]
## 49                                                      [1] [2]
## 50                                                      [1] [2]
## 51                                                  [1] [2] [3]
## 52                                                          [1]
## 53                                                          [1]
## 54                                                      [1] [2]
## 55                                                  [1] [2] [3]
## 56                                                      [1] [2]
## 57                                                      [1] [2]
## 58                                                          [1]
## 59                                                          [1]
## 60                                                          [1]
## 61                                                          [1]
## 62                                                             
## 63                                                             
## 64
dim(corona_table_df)
## [1] 64 11
corona_table_df <- corona_table_df[,-11]
corona_table_df <- corona_table_df[-1,]
corona_table_df <- corona_table_df[-52:-62,]
corona_table_df <- corona_table_df[-32,]
corona_table_df
##                USAState TotalCases NewCases TotalDeaths NewDeaths ActiveCases
## 2              New York    293,991       NA      22,275        NA     240,498
## 3            New Jersey    109,038       NA       5,938        NA     101,829
## 4         Massachusetts     54,938       NA       2,899        NA      43,921
## 5              Illinois     43,903       NA       1,933        NA      41,364
## 6            California     43,541       NA       1,718        NA      38,486
## 7          Pennsylvania     42,708       NA       1,823        NA      40,193
## 8              Michigan     37,778       NA       3,315        NA      26,121
## 9               Florida     31,528       NA       1,074        NA      29,768
## 10            Louisiana     26,773       NA       1,729        NA      10,117
## 11          Connecticut     25,269       NA       1,924        NA      23,280
## 12                Texas     24,981       NA         654        NA      14,341
## 13              Georgia     23,481       NA         916        NA      22,534
## 14             Maryland     18,581       NA         910        NA      16,494
## 15                 Ohio     15,963       NA         728        NA      15,115
## 16              Indiana     15,012       NA         813        NA      14,185
## 17           Washington     13,521       NA         749        NA      10,965
## 18             Colorado     13,441       NA         680        NA      12,202
## 19             Virginia     12,970       NA         448        NA      10,707
## 20            Tennessee      9,667       NA         181        NA       4,959
## 21       North Carolina      8,994       NA         325        NA       7,367
## 22         Rhode Island      7,439       NA         226        NA       6,871
## 23             Missouri      7,029       NA         283        NA       6,199
## 24              Arizona      6,526       NA         275        NA       6,181
## 25              Alabama      6,418       NA         219        NA       6,179
## 26            Wisconsin      5,911       NA         272        NA       3,326
## 27          Mississippi      5,911       NA         227        NA       5,684
## 28       South Carolina      5,490       NA         174        NA       1,615
## 29                 Iowa      5,476       NA         118        NA       3,754
## 30               Nevada      4,602       NA         206        NA       2,555
## 31                 Utah      4,123       NA          41        NA       3,194
## 32             Kentucky      4,074       NA         208        NA       2,744
## 34 District Of Columbia      3,841       NA         178        NA       3,006
## 35            Minnesota      3,602       NA         272        NA       1,556
## 36             Oklahoma      3,253       NA         195        NA         919
## 37               Kansas      3,174       NA         120        NA       2,551
## 38             Nebraska      3,028       NA          56        NA       2,950
## 39             Arkansas      3,001       NA          50        NA       1,964
## 40           New Mexico      2,726       NA          99        NA       1,977
## 41               Oregon      2,311       NA          91        NA       2,220
## 42         South Dakota      2,212       NA          11        NA       1,011
## 43                Idaho      1,897       NA          56        NA         974
## 44        New Hampshire      1,864       NA          60        NA       1,254
## 45        West Virginia      1,044       NA          34        NA         785
## 46                Maine      1,015       NA          50        NA         433
## 47         North Dakota        867       NA          17        NA         524
## 48              Vermont        851       NA          46        NA         805
## 49               Hawaii        606       NA          14        NA         104
## 50              Wyoming        502       NA           7        NA         174
## 51              Montana        448       NA          14        NA          95
## 52               Alaska        341       NA           9        NA         115
## 64               Total:    987,160       NA      55,413        NA     812,966
##    Tot.Cases.1M.pop Deaths.1M.pop TotalTests Tests.1M.pop
## 2            14,985         1,135    805,350       41,051
## 3            12,277           669    223,062       25,114
## 4             8,043           424    236,100       34,567
## 5             3,424           151    214,952       16,765
## 6             1,112            44    494,173       12,623
## 7             3,339           143    198,593       15,526
## 8             3,794           333    156,010       15,668
## 9             1,531            52    346,365       16,815
## 10            5,741           371    142,056       30,460
## 11            7,055           537     79,811       22,284
## 12              896            23    276,021        9,898
## 13            2,280            89    122,604       11,906
## 14            3,095           152     96,665       16,102
## 15            1,371            63    115,783        9,945
## 16            2,262           122     81,708       12,310
## 17            1,854           103    175,477       24,057
## 18            2,430           123     63,274       11,440
## 19            1,542            53     76,118        9,047
## 20            1,453            27    147,474       22,173
## 21              886            32    107,894       10,624
## 22            7,040           214     53,403       50,542
## 23            1,154            46     67,017       11,004
## 24              939            40     64,811        9,330
## 25            1,319            45     73,626       15,135
## 26            1,023            47     65,146       11,274
## 27            1,978            76     60,788       20,339
## 28            1,108            35     50,761       10,242
## 29            1,748            38     36,090       11,521
## 30            1,574            70     45,885       15,699
## 31            1,354            13     95,702       31,426
## 32              918            47     48,474       10,917
## 34            5,611           260     18,068       26,396
## 35              652            49     58,987       10,672
## 36              830            50     53,012       13,530
## 37            1,091            41     25,199        8,663
## 38            1,590            29     22,525       11,826
## 39            1,003            17     39,551       13,225
## 40            1,303            47     56,615       27,057
## 41              566            22     48,964       11,995
## 42            2,559            13     15,596       18,045
## 43            1,124            33     19,361       11,471
## 44            1,387            45     17,727       13,193
## 45              571            19     39,063       21,357
## 46              762            38     17,721       13,296
## 47            1,153            23     20,447       27,183
## 48            1,362            74     14,797       23,676
## 49              426            10     29,247       20,567
## 50              863            12      7,623       13,102
## 51              430            13     12,862       12,347
## 52              462            12     16,177       21,905
## 64            2,982           167  5,470,464       16,527
dim(corona_table_df)
## [1] 51 10
str(corona_table_df)
## 'data.frame':    51 obs. of  10 variables:
##  $ USAState        : chr  "New York" "New Jersey" "Massachusetts" "Illinois" ...
##  $ TotalCases      : chr  "293,991" "109,038" "54,938" "43,903" ...
##  $ NewCases        : logi  NA NA NA NA NA NA ...
##  $ TotalDeaths     : chr  "22,275" "5,938" "2,899" "1,933" ...
##  $ NewDeaths       : logi  NA NA NA NA NA NA ...
##  $ ActiveCases     : chr  "240,498" "101,829" "43,921" "41,364" ...
##  $ Tot.Cases.1M.pop: chr  "14,985" "12,277" "8,043" "3,424" ...
##  $ Deaths.1M.pop   : chr  "1,135" "669" "424" "151" ...
##  $ TotalTests      : chr  "805,350" "223,062" "236,100" "214,952" ...
##  $ Tests.1M.pop    : chr  "41,051" "25,114" "34,567" "16,765" ...
## removing NA's and converting variables to numeric
corona_table_df$TotalCases <- gsub(",", "", corona_table_df$TotalCases)
corona_table_df$TotalDeaths <- gsub(",", "", corona_table_df$TotalDeaths)
corona_table_df$ActiveCases <- gsub(",", "", corona_table_df$ActiveCases)
corona_table_df$TotalTests <- gsub(",", "", corona_table_df$TotalTests)
corona_table_df$Tot.Cases.1M.pop <- gsub(",", "", corona_table_df$Tot.Cases.1M.pop)
corona_table_df$Deaths.1M.pop <- gsub(",", "", corona_table_df$Deaths.1M.pop)
corona_table_df$Tests.1M.pop <- gsub(",", "", corona_table_df$Tests.1M.pop)

corona_table_df$TotalCases <- as.numeric(gsub(" ", "", corona_table_df$TotalCases))
corona_table_df$TotalDeaths <- as.numeric(gsub(" ", "", corona_table_df$TotalDeaths))
corona_table_df$ActiveCases <- as.numeric(gsub(" ", "", corona_table_df$ActiveCases))
corona_table_df$TotalTests <- as.numeric(gsub(" ", "", corona_table_df$TotalTests))
corona_table_df$Tot.Cases.1M.pop <- as.numeric(gsub(" ", "", corona_table_df$Tot.Cases.1M.pop))
corona_table_df$Deaths.1M.pop <- as.numeric(gsub(" ", "", corona_table_df$Deaths.1M.pop))
corona_table_df$Tests.1M.pop <- as.numeric(gsub(" ", "", corona_table_df$Tests.1M.pop))

str(corona_table_df)
## 'data.frame':    51 obs. of  10 variables:
##  $ USAState        : chr  "New York" "New Jersey" "Massachusetts" "Illinois" ...
##  $ TotalCases      : num  293991 109038 54938 43903 43541 ...
##  $ NewCases        : logi  NA NA NA NA NA NA ...
##  $ TotalDeaths     : num  22275 5938 2899 1933 1718 ...
##  $ NewDeaths       : logi  NA NA NA NA NA NA ...
##  $ ActiveCases     : num  240498 101829 43921 41364 38486 ...
##  $ Tot.Cases.1M.pop: num  14985 12277 8043 3424 1112 ...
##  $ Deaths.1M.pop   : num  1135 669 424 151 44 ...
##  $ TotalTests      : num  805350 223062 236100 214952 494173 ...
##  $ Tests.1M.pop    : num  41051 25114 34567 16765 12623 ...
## Attempting to create map
corona_table_df$USAState <- tolower(corona_table_df$USAState)

corona_table_df
##                USAState TotalCases NewCases TotalDeaths NewDeaths ActiveCases
## 2              new york     293991       NA       22275        NA      240498
## 3            new jersey     109038       NA        5938        NA      101829
## 4         massachusetts      54938       NA        2899        NA       43921
## 5              illinois      43903       NA        1933        NA       41364
## 6            california      43541       NA        1718        NA       38486
## 7          pennsylvania      42708       NA        1823        NA       40193
## 8              michigan      37778       NA        3315        NA       26121
## 9               florida      31528       NA        1074        NA       29768
## 10            louisiana      26773       NA        1729        NA       10117
## 11          connecticut      25269       NA        1924        NA       23280
## 12                texas      24981       NA         654        NA       14341
## 13              georgia      23481       NA         916        NA       22534
## 14             maryland      18581       NA         910        NA       16494
## 15                 ohio      15963       NA         728        NA       15115
## 16              indiana      15012       NA         813        NA       14185
## 17           washington      13521       NA         749        NA       10965
## 18             colorado      13441       NA         680        NA       12202
## 19             virginia      12970       NA         448        NA       10707
## 20            tennessee       9667       NA         181        NA        4959
## 21       north carolina       8994       NA         325        NA        7367
## 22         rhode island       7439       NA         226        NA        6871
## 23             missouri       7029       NA         283        NA        6199
## 24              arizona       6526       NA         275        NA        6181
## 25              alabama       6418       NA         219        NA        6179
## 26            wisconsin       5911       NA         272        NA        3326
## 27          mississippi       5911       NA         227        NA        5684
## 28       south carolina       5490       NA         174        NA        1615
## 29                 iowa       5476       NA         118        NA        3754
## 30               nevada       4602       NA         206        NA        2555
## 31                 utah       4123       NA          41        NA        3194
## 32             kentucky       4074       NA         208        NA        2744
## 34 district of columbia       3841       NA         178        NA        3006
## 35            minnesota       3602       NA         272        NA        1556
## 36             oklahoma       3253       NA         195        NA         919
## 37               kansas       3174       NA         120        NA        2551
## 38             nebraska       3028       NA          56        NA        2950
## 39             arkansas       3001       NA          50        NA        1964
## 40           new mexico       2726       NA          99        NA        1977
## 41               oregon       2311       NA          91        NA        2220
## 42         south dakota       2212       NA          11        NA        1011
## 43                idaho       1897       NA          56        NA         974
## 44        new hampshire       1864       NA          60        NA        1254
## 45        west virginia       1044       NA          34        NA         785
## 46                maine       1015       NA          50        NA         433
## 47         north dakota        867       NA          17        NA         524
## 48              vermont        851       NA          46        NA         805
## 49               hawaii        606       NA          14        NA         104
## 50              wyoming        502       NA           7        NA         174
## 51              montana        448       NA          14        NA          95
## 52               alaska        341       NA           9        NA         115
## 64               total:     987160       NA       55413        NA      812966
##    Tot.Cases.1M.pop Deaths.1M.pop TotalTests Tests.1M.pop
## 2             14985          1135     805350        41051
## 3             12277           669     223062        25114
## 4              8043           424     236100        34567
## 5              3424           151     214952        16765
## 6              1112            44     494173        12623
## 7              3339           143     198593        15526
## 8              3794           333     156010        15668
## 9              1531            52     346365        16815
## 10             5741           371     142056        30460
## 11             7055           537      79811        22284
## 12              896            23     276021         9898
## 13             2280            89     122604        11906
## 14             3095           152      96665        16102
## 15             1371            63     115783         9945
## 16             2262           122      81708        12310
## 17             1854           103     175477        24057
## 18             2430           123      63274        11440
## 19             1542            53      76118         9047
## 20             1453            27     147474        22173
## 21              886            32     107894        10624
## 22             7040           214      53403        50542
## 23             1154            46      67017        11004
## 24              939            40      64811         9330
## 25             1319            45      73626        15135
## 26             1023            47      65146        11274
## 27             1978            76      60788        20339
## 28             1108            35      50761        10242
## 29             1748            38      36090        11521
## 30             1574            70      45885        15699
## 31             1354            13      95702        31426
## 32              918            47      48474        10917
## 34             5611           260      18068        26396
## 35              652            49      58987        10672
## 36              830            50      53012        13530
## 37             1091            41      25199         8663
## 38             1590            29      22525        11826
## 39             1003            17      39551        13225
## 40             1303            47      56615        27057
## 41              566            22      48964        11995
## 42             2559            13      15596        18045
## 43             1124            33      19361        11471
## 44             1387            45      17727        13193
## 45              571            19      39063        21357
## 46              762            38      17721        13296
## 47             1153            23      20447        27183
## 48             1362            74      14797        23676
## 49              426            10      29247        20567
## 50              863            12       7623        13102
## 51              430            13      12862        12347
## 52              462            12      16177        21905
## 64             2982           167    5470464        16527
us<- map_data("state")

## putting Total Cases cases to map
maptotalCases <- ggplot(corona_table_df, aes(map_id = USAState))
maptotalCases <- maptotalCases + geom_map(map = us, aes(fill = TotalCases))
maptotalCases <- maptotalCases + expand_limits(x = us$long, y = us$lat)
maptotalCases <- maptotalCases + coord_map() + ggtitle("Total Coronvirus Cases by State")
maptotalCases
 
tC <- ggplot(corona_table_df, aes(TotalCases, TotalDeaths))
tC <- tC + geom_point(aes(size = ActiveCases, color = ActiveCases))
tC
 
## putting total deaths to map
maptotaldeaths <- ggplot(corona_table_df, aes(map_id = USAState))
maptotaldeaths <- maptotaldeaths + geom_map(map = us,aes(fill = TotalDeaths))
maptotaldeaths <- maptotaldeaths + expand_limits(x = us$long, y = us$lat)
maptotaldeaths <- maptotaldeaths + coord_map() + ggtitle("Total Death Cases by State")
maptotaldeaths
 
tD <- ggplot(corona_table_df, aes(ActiveCases, TotalDeaths))
tD <- tD + geom_point(aes(size = ActiveCases, color = ActiveCases))
tD <- tD + geom_smooth()
tD
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
 
## putting total test per state to map 
maptotaltest <- ggplot(corona_table_df, aes(map_id = USAState))
maptotaltest <- maptotaltest + geom_map(map = us, aes(fill = TotalTests))
maptotaltest <- maptotaltest + expand_limits(x = us$long, y = us$lat)
maptotaltest <- maptotaltest + coord_map() + ggtitle("Total Tests per State")
maptotaltest
 
tt <- ggplot(corona_table_df, aes(ActiveCases, TotalTests, label = rownames(USAState)))
tt <- tt + geom_point(aes(size = TotalDeaths, color = ActiveCases))
tt <- tt + geom_smooth()
tt
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
 
## putting total cases per million pop
maptotalmillion <- ggplot(corona_table_df, aes(map_id = USAState))
maptotalmillion <- maptotalmillion + geom_map(map = us, aes(fill = Tot.Cases.1M.pop))
maptotalmillion <- maptotalmillion + expand_limits(x = us$long, y = us$lat)
maptotalmillion <- maptotalmillion + coord_map() + ggtitle("Total Cases Per Million")
maptotalmillion
 
tM <- ggplot(corona_table_df, aes(Tot.Cases.1M.pop, ActiveCases))
tM <- tM + geom_point()
tM
 
##mapping total deaths per million
maptotalDeathmillion <- ggplot(corona_table_df, aes(map_id = USAState))
maptotalDeathmillion <- maptotalDeathmillion + geom_map(map = us, aes(fill = Deaths.1M.pop))
maptotalDeathmillion <- maptotalDeathmillion + expand_limits(x = us$long, y = us$lat)
maptotalDeathmillion <- maptotalDeathmillion + coord_map() + ggtitle("Total Death Cases Per Million")
maptotalDeathmillion
 
##Predictions: linear modeling
model1 <- lm(formula = TotalDeaths ~ TotalCases, data = corona_table_df)
summary(model1)
## 
## Call:
## lm(formula = TotalDeaths ~ TotalCases, data = corona_table_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1468.6  -147.3   -45.2    23.4  5369.9 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.001e+01  1.237e+02  -0.404    0.688    
## TotalCases   5.767e-02  8.466e-04  68.119   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 852.4 on 49 degrees of freedom
## Multiple R-squared:  0.9896, Adjusted R-squared:  0.9893 
## F-statistic:  4640 on 1 and 49 DF,  p-value: < 2.2e-16
plot(corona_table_df$TotalCases,corona_table_df$TotalDeaths)
abline(model1)
 
## in the model we can see that the r sqaured values predicted the data perfectly with a 98% rate
test = data.frame(TotalCases = 10000)
predict(model1, test)
##        1 
## 526.7158
test = data.frame(TotalCases = 30000)
predict(model1, test)
##        1 
## 1680.158
test = data.frame(TotalCases = 80000)
predict(model1, test)
##        1 
## 4563.765
test = data.frame(TotalCases = 150000)
predict(model1, test)
##        1 
## 8600.813
##Things i found out during this project:
## New york is the epicenter of the virus in the United States, with it having the most total cases,
## the most total death Cases, as well as the most most test being administered. In the first plot 
## that i calculated you can defiently see the that with more cases that we are being 
## accounted for there is a rising death rate which i think creates a very scare scenario.
## In another plot we can see that even with the active cases the death rate continues to climb 
## with a positive correlation