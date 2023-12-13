# 后续改进：根据csv还是excel来选择data.table或tidy的处理方式
dpp_csmar = function(path) {
  library(stringr)
  library(dplyr)
  # library(magrittr)
  # library(data.table)

  result <- list()
  datasets <- list.dirs(path)[-1]
  if (length(datasets) > 10) {
    warning("The number of files is large and the result may be even larger.
Consider separating them if your memory is not large enough.")
  }

  for (dataset in datasets) {
    # 提取数据文件和变量名文件
    files <- dir(dataset, full.names = T)[dir(dataset) != "版权声明.pdf"]
    varname_file <- files[str_detect(files, "\\].txt$")]
    files <- setdiff(files, varname_file)

    # 提取变量名，得到中文名和英文变量名一一对应的命名向量
    varnames <- rio::import(varname_file, header = F, fill = T) |>
      reframe(
        name = str_match(V2, "\\[(.*)\\]")[, 2],
        origin = str_to_lower(V1)
      ) |>
      filter(!is.na(name)) |>
      pull(origin, name)

    # 读取数据，更改变量名
    for (file in files) {
      ## 如果多个数据表，第一个单独创建，后续合并
      if (file == files[1]) {
        ### 如果不是xlsx或csv格式数据，报错
        if (!str_detect(file, "xlsx$|csv$")) stop("Please download xlsx or csv!")

        data <- rio::import(file, setclass = "tbl") |>
          #### tolower这个在csv文件中可以省略
          rename_with(tolower) |>
          rename(any_of(varnames))

        ### 如果第一列列名等于第一个值（前两列不是数据），则删除前两列
        if (colnames(data)[1] == data %>% slice(1) %>% pull(1)) data <- slice(data, -(1:2))
      } else {
        if (!str_detect(file, "xlsx$|csv$")) stop("Please download xlsx or csv!")

        data <- rio::import(file, setclass = "tbl") |>
          rename_with(tolower) |>
          rename(any_of(varnames)) |>
          bind_rows(data)

        if (colnames(data)[1] == data %>% slice(1) %>% pull(1)) data <- slice(data, -(1:2))
      }
    }

    message(str_c("Successfully processed: ", dataset))

    dataset <- dataset %>%
      str_extract("(?<=/)[^/]+$") %>%
      str_remove_all("\\(|\\)|（|）|\\-")
    result[[dataset]] <- data
  }

  return(result)
}
