# db.alistfirm

Functions handling original datasets from [CSMAR](https://data.csmar.com/) database.

### Installation

```r
install.packages("devtools")
devtools::install_github("yang-yuchuan/db.alistfirm")
```

### Usage

```r
# # look, import and use the database
# db.alistfirm::db_alistfirm

# # look up for the variables information
# ?db.alistfirm::db_alistfirm
# ## or directly watch the detailed Chinese version in 
# ## https://github.com/yang-yuchuan/db.alistfirm/blob/master/R/data.R

# suppose your CSMAR datasets are stored in "work/raw"
# this will generate a large list contain all datasets
dt = db.alistfirm::dpp_csmar("work/raw")
```

### Updates

- 2023-05-30: Refined to quarterly data, remove completion when merging (now merge only if balance sheet information is valid).
- 2023-06-10: Do seasonal difference for flow variables, e.g., cash, invest payment, etc. Now that these variables reflect the flow in the corresponding quarter, but not the sum of the previous quarters in that year.
- 2023-12-13: 
  - Offer a function `dpp_csmar` that can automatically rename CSMAR datasets to reasonable Chinese variable names.
  - Remove data files for copyright issues.
