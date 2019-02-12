
tbgeneratr
==========

[![Travis build status](https://travis-ci.org/JayAchar/tbgeneratr.svg?branch=master)](https://travis-ci.org/JayAchar/tbgeneratr) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/JayAchar/tbgeneratr?branch=master&svg=true)](https://ci.appveyor.com/project/JayAchar/tbgeneratr) [![Coverage status](https://codecov.io/gh/JayAchar/tbgeneratr/branch/master/graph/badge.svg)](https://codecov.io/github/JayAchar/tbgeneratr?branch=master) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Generate variables from cleaned MSF TB programme data

Introduction
------------

The R package `tbgeneratr` was developed to be used in synergy with `tbcleanr` to simplify cleaning and processing for routinely collected TB programme data. `tbgeneratr` compliments `tbcleanr` output data frames by processing and generating more complex data points, such as individual patient culture conversion dates. Following development of `tbreportr`, a simplified workflow from raw data to automated reporting will be realised through `tbcleanr`, `tbgeneratr` and `tbreportr`.

Installation
------------

You can install the released version of `tbgeneratr` from [Github](http://www.github.com/JayAchar) with:

``` r
require(devtools)
install_github("JayAchar/tbgeneratr")
library(tbgeneratr)
```

Important functions
-------------------

<table style="width:81%;">
<colgroup>
<col width="20%" />
<col width="59%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Use it for</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>adm_generator()</code></td>
<td>Calculate age and BMI variables, and split start treatment date to allow easier analysis</td>
</tr>
<tr class="even">
<td><code>adm_lab_generator()</code></td>
<td>Calculate baseline smear and culture status, conversion dates, and baseline drug susceptbility results</td>
</tr>
</tbody>
</table>
