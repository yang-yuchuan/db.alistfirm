# db.alistfirm

Database of A-share listed firms

### Installation

```r
install.packages("devtools")
devtools::install_github("yang-yuchuan/db.alistfirm")
```

### Usage

```r
# look, import and use the database
db.alistfirm::db_alistfirm

# look up for the variables information
?db.alistfirm::db_alistfirm
## or directly watch the detailed Chinese version in 
## https://github.com/yang-yuchuan/db.alistfirm/blob/master/R/data.R
```

### Updates

- 2023-05-30: Refined to quarterly data, remove completion when merging (now merge only if balance sheet information is valid).
- 2023-06-10: Do seasonal difference for flow variables, e.g., cash, invest payment, etc. Now that these variables reflect the flow in the corresponding quarter, but not the sum of the previous quarters in that year.
