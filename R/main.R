alistdata = function(change = F) {

stop("This function need not be invoked.")

# setup ----
pacman::p_load(tidyverse, rio, lubridate, stringr, jsonlite)

# functions ----
csmar_clean = function(filepath,
                       type = TRUE,
                       date = accper,
                       id = stkcd) {
  # 初步清理CSMAR数据：小写化，保留母公司年报

  date <- enquo(date)
  id <- enquo(id)

  data <- import(filepath, setclass = "tbl") %>%
    rename_with(tolower) %>%
    rename(accper = !!date, stkcd = !!id)

  if (class(data$stkcd) != "integer") {
    data <- data[-(1:2),]
  }

  print(data %>% count(accper %>% ymd() %>% month()))

  if (type) {
    print(data %>% count(typrep))
    data <- data %>%
      filter(typrep == "A") %>%
      select(-typrep)
  }
  data <- data %>%
    filter(
      month(ymd(accper)) == 12
    ) %>%
    mutate(
      year = year(ymd(accper)),
      stkcd = as.integer(stkcd)
    )

  return(data)
}

csmar_select = function(data, ...) {
  # 删掉不要的变量

  data <- data %>%
    select(stkcd, year, ...)
  data %>%
    summarise(across(.fn = ~ sum(is.na(.x)))) %>%
    glimpse()
  return(data)
}

get_add = function(lonlat) {
  # 若办公地址中包含注册城市，则采用注册城市；
  # 否则，利用办公地址经纬度和高德地图进行地理逆编码

  key_gaode <- c(
    "Your Gaode Map Key"
  )
  url <- "https://restapi.amap.com/v3/geocode/regeo?"

  for (ii in 1:3) {
    msg <- fromJSON(
      str_c(
        url, "key=", key_gaode[ii],
        "&location=", lonlat
      )
    )
    if (msg$status == "0") ii <- ii + 1 else break
  }
  add <- msg$regeocode$addressComponent
  province <- add$province
  if (str_detect(province, "市")) {
    city <- province
  } else if (toString(add$city) == "" &
             toString(add$district) != "") {
    city <- add$district
  } else if (toString(add$city) == "") {
    city <- add$province
  } else {
    city <- add$city
  }
  return(list(
    city = city,
    province = province
  ))
}

gen_spill = function(x, data) {
  # 构造知识溢出变量

  if (is.na(x)) {
    x
  } else {
    data %>%
      as_tibble() %>%
      mutate(excess = data - x) %>%
      filter(excess > 0) %>%
      summarise(sum(excess)) %>%
      as.numeric()
  }
}

# import: Financial Statements ----
fs_balance <- csmar_clean(
    "inst/extdata/资产负债表-2022/FS_Combas.csv"
  ) %>%
  csmar_select(
    total_asset = a001000000,
    current_asset = a001100000,
    total_debt = a002000000,
    short_borrow = a002101000,
    fix_asset = a001212000,
    retain_earn = a003105000,
    book_equity = a003000000,
    long_borrow = a002201000,
    current_debt = a002100000,
    construction = a001213000,
    long_pay = a002204000,
    special_pay = a002205000,
    invisible_asset = a001218000
  )

fs_income <- csmar_clean(
    "inst/extdata/利润表-2022/FS_Comins.csv"
  ) %>%
  csmar_select(
    revenue = b001101000,
    net_profit = b002000000,
    cost = b001201000,
    sale_expense = b001209000,
    admin_expense = b001210000
  )

fs_cashflow <- csmar_clean(
    "inst/extdata/现金流量表(直接法)-2022/FS_Comscfd.csv"
  ) %>%
  csmar_select(
    cash = c006000000,
    operate_cash = c001000000,
    dividend_pay = c003006000,
    invest_pay = c002006000,
    invest_receive = c002003000
  )

# import: Financial Index ----
fi_turnover <- csmar_clean(
    "inst/extdata/经营能力-2022/FI_T4.csv"
  ) %>%
  csmar_select(
    current_asset_to = f041202b
  )

fi_ratio <- csmar_clean(
    "inst/extdata/比率结构-2022/FI_T3.csv"
  ) %>%
  csmar_select(
    cash_ratio = f030201a,
    receive_ratio = f030301a,
    intan_ratio = f030901a
  )

fi_profit <- csmar_clean(
    "inst/extdata/盈利能力-2022/FI_T5.csv"
  ) %>%
  csmar_select(
    roa = f050202b,
    roe = f050502b,
    ebitda = f050801b
  )

fi_rvi <- csmar_clean(
    "inst/extdata/相对价值指标-2022/FI_T10.csv",
    type = F
  ) %>%
  csmar_select(
    tobinq = f100901a,
    btm = f101001a,
    market_equity = f100801a
  )

# import: Corporate Governance ----
## 股权性质
cg_ownership <- csmar_clean(
    "inst/extdata/中国上市公司股权性质文件-2022/EN_EquityNatureAll.xlsx",
    type = F, date = enddate, id = symbol
  ) %>%
  mutate(
    year = year, stkcd = stkcd,
    top1_share = largestholderrate,
    top10_share = toptenholdersrate,
    seperation = seperation,
    state = if_else(
      equitynature == "其他", NA, equitynature
    ),
    across(
      c(top1_share, top10_share, seperation),
      as.numeric
    ),
    checkandbalance_equity =
      (top10_share - top1_share) / top1_share,
    .keep = "none"
  )

## 治理结构
cg_gov <- csmar_clean(
  "inst/extdata/治理综合信息文件/CG_Ybasic.xlsx",
  type = F, date = reptdt
) %>%
  mutate(
    ceo_duality = if_else(
      y1001b == "1.0", "合一", "不合一"
    ),
    ceo_duality_num = if_else(y1001b == "1.0", 1, 0),
    inddir_workplace = case_when(
      y1801b == "1.0" ~ "相同",
      y1801b == "2.0" ~ "不相同"
    ),
    labor = as.numeric(y0601b)
  ) %>%
  csmar_select(
    # 两职合一、独立董事工作地点是否相同、员工人数
    ceo_duality, inddir_workplace, ceo_duality_num,
    labor
  )

## 独立董事缺席会议情况
cg_indabs <- csmar_clean(
  "inst/extdata/独立董事出席会议情况文件/CG_Cxhy.xlsx",
  type = F, date = reptdt
) %>%
  mutate(
    absence = as.numeric(attennum3) %>%
      replace_na(0),
  ) %>%
  group_by(stkcd, year) %>%
  summarise(
    inddir_absence = as.numeric(sum(absence) > 0)
  ) %>%
  ungroup()

## 独立董事比例
cg_indrat <- csmar_clean(
  "inst/extdata/高管人数、持股及薪酬情况表/CG_ManagerShareSalary.xlsx",
  type = F, date = enddate, id = symbol
) %>%
  filter(statisticalcaliber == "1") %>%
  mutate(
    year = year, stkcd = stkcd,
    inddir_rate = as.numeric(independentdirectornumber) /
      as.numeric(directornumber),
    director_wage = as.numeric(directorsumsalary),
    director_size = as.numeric(directornumber),
    supervisor_size = as.numeric(supervisornumber),
    .keep = "none"
  )

# import: Basic Info. ----
bi_base <- import(
    "inst/extdata/公司文件/TRD_Co.csv",
    setclass = "tbl"
  ) %>%
  rename_with(tolower) %>%
  # 保留A股公司
  filter(
    str_detect(stkcd, "\\d"),
    markettype %in% c("1", "4", "16", "32"),
    province != "开曼群岛"
  ) %>%
  # 成立/上市年份不会有变更吧？
  mutate(
    year_estb = estbdt %>% ymd() %>% year(),
    year_list = listdt %>% ymd() %>% year(),
    stkcd = as.integer(stkcd),
    prov_cs = str_remove(
        province,
        "省|市|自治区|维吾尔自治区|壮族自治区|回族自治区"
      ) %>%
      str_trim(),
    city_cs = str_remove(city, "市")
  ) %>%
  select(
    stkcd, year_estb, year_list,
    # 横截面信息
    ind_cs = nnindcd, ind_class = indcd,
    prov_cs, city_cs, state_cs = ownershiptype
  )

bi_st <- import(
  "inst/extdata/上市公司基本信息库2022.xlsx",
  setclass = "tbl"
) %>%
  mutate(
    stkcd = str_extract(证券代码, "\\d*") %>%
      as.numeric(),
    st = str_detect(证券名称, "ST|PT"),
    .keep = "none"
  ) %>%
  filter(!is.na(stkcd))

bi_ccer <- import(
  "inst/extdata/中国上市公司股权性质文件-2022/股权性质.xlsx",
  setclass = "tbl"
) %>%
  mutate(across(everything(), as.numeric)) %>%
  distinct() %>%
  # 手动筛选异常的值：
  #   保留-删除为相同的股票-年样本，删除对应的是NA
  #   警告为相同股票-年但一个为国有一个为非国有
  # 目前处理是将警告的样本也删除；还可以都为国有
  left_join(
    import(
      "inst/extdata/中国上市公司股权性质文件-2022/异常.xlsx",
      setclass = "tbl"
    ),
    unmatched = "error"
  ) %>%
  filter(!(abnormal %in% c("删除", "警告"))) %>%
  # 生成SOE
  mutate(
    soe_ccer = case_when(
      上市公司实际控制人类别 == 0 ~ "国有",
      上市公司实际控制人类别 %in% c(-95, 6, NA) ~ NA,
      T ~ "非国有"
    )
  ) %>%
  select(stkcd = 股票代码, year = 年度,
         starts_with("soe"))
## 看看还有没有重复值
bi_ccer %>% count(stkcd, year) %>% filter(n > 1)

# import: Basic Info. - address & industry ----
if (!file.exists("inst/extdata/bi_add.rds")) {
  bi_add <- import(
    "inst/extdata/上市公司基本信息年度表/STK_LISTEDCOINFOANL.xlsx",
    setclass = "tbl"
  ) %>%
    rename_with(tolower) %>%
    filter(str_detect(symbol, "\\d")) %>%
    mutate(
      year = year(ymd(enddate)),
      stkcd = as.integer(symbol),
      lon = as.numeric(lng),
      lat = as.numeric(lat)
    ) %>%
    select(
      stkcd, lon, lat, province, city,
      officeaddress, year, ind_code = industrycode
    )

  # 办公注册一致者，直接使用注册地址
  temp_add_reg <- bi_add %>%
    filter(str_detect(officeaddress, city)) %>%
    select(stkcd, year, province, city, ind_code)
  # 经纬度缺失且办公注册不一致者，手动修改
  temp_add_na <- bi_add %>%
    filter(
      is.na(lon) | is.na(lat),
      !str_detect(officeaddress, city)
    ) %>%
    mutate(
      province = str_match(
        .$officeaddress,
        "^(.{2,3})(省|(维吾尔|壮族|回族|)自治区|市)"
      ) %>% .[,2],
      city = str_match(
        .$officeaddress, "(省|自治区)(.{2,4})市"
      ) %>% .[,3],
      city = case_when(
        province %in% c("上海", "北京") ~ province,
        city == "即墨" ~ "青岛",
        city == "东阳" ~ "金华",
        city == "桐乡" ~ "嘉兴",
        city == "凌源" ~ "朝阳",
        city == "泰兴" ~ "泰州",
        city == "江阴" ~ "无锡",
        province == "江苏" & is.na(city) ~ "苏州",
        city == "宁乡" ~ "长沙",
        city == "应城" ~ "孝感",
        city == "天长" ~ "滁州",
        province == "山东" ~ "威海",
        T ~ city
      )
    ) %>%
    select(stkcd, year, province, city, ind_code)

  # 剩余的，地理逆编码爬取
  bi_add <- bi_add %>%
    filter(
      !str_detect(officeaddress, city),
      !is.na(lon), !is.na(lat)
    ) %>%
    mutate(
      lonlat = str_c(lon, ",", lat),
      add = map(lonlat, get_add),
      city = map_chr(add, ~ .$city),
      province = map_chr(add, ~ .$province)
    ) %>%
    select(
      stkcd, year, province, city, ind_code
    ) %>%
    mutate(
      province = if_else(
        province == "中华人民共和国",
        true = "福建省", false = province
      ),
      city = if_else(
        city == "中华人民共和国",
        true = "福州市", false = city
      )
    ) %>%
    # 三者合并
    bind_rows(temp_add_na, temp_add_reg) %>%
    # 补全缺失行业代码：正在上市的831689
    replace_na(list(ind_code = "C34")) %>%
    mutate(
      across(
        c(province, city),
        ~ str_remove(.x, "市|省|自治区|维吾尔自治区|壮族自治区|回族自治区")
      ),
      city = case_when(
        city == "巢湖" ~ "合肥",
        city == "明光" ~ "滁州",
        city == "宁国" ~ "宣城",
        city == "潜山" ~ "安庆",
        city == "福安" ~ "宁德",
        str_detect(city, "福清|长乐") ~ "福州",
        str_detect(city, "晋江|南安") ~ "泉州",
        str_detect(city, "恩平|鹤山|开平|台山") ~ "江门",
        str_detect(city, "普宁") ~ "揭阳",
        str_detect(city, "四会") ~ "肇庆",
        str_detect(city, "兴宁") ~ "梅州",
        str_detect(city, "阳春") ~ "阳江",
        str_detect(city, "林州") ~ "安阳",
        str_detect(city, "项城") ~ "周口",
        str_detect(city, "偃师") ~ "洛阳",
        str_detect(city, "义马") ~ "三门峡",
        str_detect(city, "永城") ~ "商丘",
        str_detect(city, "长葛") ~ "许昌",
        str_detect(city, "当阳|枝江") ~ "宜昌",
        str_detect(city, "汉川|应城") ~ "孝感",
        str_detect(city, "武穴") ~ "黄冈",
        str_detect(city, "浏阳|宁乡") ~ "长沙",
        str_detect(city, "沅江") ~ "益阳",
        str_detect(city, "梅河口") ~ "通化",
        str_detect(city, "磐石") ~ "吉林",
        str_detect(city, "常熟|昆山|太仓|张家港") ~ "苏州",
        str_detect(city, "丹阳") ~ "镇江",
        str_detect(city, "高邮|仪征") ~ "扬州",
        str_detect(city, "海门|启东|如皋") ~ "南通",
        str_detect(city, "江阴|宜兴") ~ "无锡",
        str_detect(city, "靖江|泰兴") ~ "泰州",
        str_detect(city, "项城") ~ "苏州",
        str_detect(city, "溧阳") ~ "常州",
        str_detect(city, "新沂") ~ "徐州",
        str_detect(city, "贵溪") ~ "鹰潭",
        str_detect(city, "乐平") ~ "景德镇",
        str_detect(city, "凌海") ~ "锦州",
        str_detect(city, "灵武") ~ "银川",
        str_detect(city, "昌邑|寿光|诸城") ~ "潍坊",
        str_detect(city, "即墨|胶州") ~ "青岛",
        str_detect(city, "莱阳|莱州|龙口|招远") ~ "烟台",
        str_detect(city, "乐陵|禹城") ~ "德州",
        str_detect(city, "曲阜") ~ "济宁",
        str_detect(city, "荣成") ~ "威海",
        str_detect(city, "滕州") ~ "枣庄",
        str_detect(city, "韩城") ~ "渭南",
        str_detect(city, "神木") ~ "榆林",
        str_detect(city, "简阳|崇州") ~ "成都",
        str_detect(city, "峨眉山") ~ "乐山",
        str_detect(city, "广汉|什邡") ~ "德阳",
        str_detect(city, "江油") ~ "绵阳",
        str_detect(city, "奉化|慈溪|余姚") ~ "宁波",
        str_detect(city, "义乌|东阳|永康") ~ "金华",
        str_detect(city, "海宁|平湖|桐乡") ~ "嘉兴",
        str_detect(city, "建德|临安") ~ "杭州",
        str_detect(city, "江山") ~ "衢州",
        str_detect(city, "乐清|瑞安") ~ "温州",
        str_detect(city, "临海|温岭|玉环") ~ "台州",
        str_detect(city, "嵊州|诸暨") ~ "绍兴",
        T ~ city
      )
    )
  export(bi_add, "inst/extdata/bi_add.rds")
} else {
  bi_add <- import(
    "inst/extdata/bi_add.rds",
    setclass = "tbl"
  )
}

# import: Corporate Innovation ----
## 是合并报表
ci_rd <- import(
  "inst/extdata/川川研发.xlsx", setclass = "tbl"
) %>%
  pivot_longer(
    cols = 3:19,
    names_to = "year",
    names_pattern = "(\\d{4})",
    values_to = "rd"
  ) %>%
  rename(stkcd = `证券代码`) %>%
  mutate(
    stkcd = stkcd %>%
      str_remove("\\.[A-Z]*$") %>%
      as.integer(),
    year = as.integer(year)
  ) %>%
  select(-`证券简称`) %>%
  drop_na(stkcd)

ci_rd_other <- import(
  "inst/extdata/研发投入情况表/PT_LCRDSPENDING.xlsx",
  setclass = "tbl"
) %>%
  slice(-(1:2)) %>%
  rename_with(tolower) %>%
  filter(
    source == "0.0",
    statetypecode == "1.0",
    month(ymd(enddate)) == 12
  ) %>%
  mutate(
    year = year(ymd(enddate)),
    stkcd = as.numeric(symbol),
    rd_person = as.numeric(rdperson),
    rd_person_ratio = as.numeric(rdpersonratio),
    rd_expense = as.numeric(rdexpenses),
    rd_invest = as.numeric(rdinvest),
    .keep = "none"
  )

ci_citation <- import(
    "inst/extdata/分年度专利被引用次数统计表/PCT_CitedNumPub.xlsx",
    setclass = "tbl"
  ) %>%
  rename_with(tolower) %>%
  filter(
    str_detect(stkcd, "\\d"),
    rel %in% c("4", "5")
  ) %>%
  mutate(
    stkcd = as.integer(stkcd),
    citation = as.integer(num),
    year = as.integer(cgpubyear)
  ) %>%
  group_by(stkcd, year) %>%
  summarise(citation = sum(citation, na.rm = T)) %>%
  ungroup()

ci_patent <- import(
    "inst/extdata/上市公司专利数据mark.dta",
    setclass = "tbl"
  ) %>%
  rename(
    stkcd = `股票代码`,
    type = `专利类型`,
    year_app = `专利申请年份`,
    year_pub = `公开公告年份`,
    year_grt = `授权公告年份`
  )
ci_pat_app <- ci_patent %>%
  group_by(stkcd, year_app) %>%
  summarise(patent = n()) %>%
  ungroup()
ci_pat_appgrt <- ci_patent %>%
  filter(year_app == year_grt) %>%
  group_by(stkcd, year_app) %>%
  summarise(patent_grt = n()) %>%
  ungroup()
ci_pat_inv <- ci_patent %>%
  filter(str_detect(type, "发明")) %>%
  group_by(stkcd, year_app) %>%
  summarise(patent_inv = n()) %>%
  ungroup()
ci_patent <- ci_pat_app %>%
  full_join(ci_pat_appgrt) %>%
  full_join(ci_pat_inv) %>%
  rename(year = year_app)

# import: Digitalization ----
digit_text <- import(
    "inst/extdata/上市公司年报-数字化转型.xlsx",
    setclass = "tbl"
  ) %>%
  select(
    stkcd = `股票代码`,
    year = `年份`,
    length_all = `全文-文本总长度`,
    length_txt = `仅中英文-文本总长度`,
    digit_a = `数字化转型程度-A`,
    digit_b = `数字化转型程度-B`
  ) %>%
  mutate(
    across(c(stkcd, year), .fns = as.integer)
  ) %>%
  drop_na(stkcd, year) %>%
  ## 重复的企业-年删掉
  filter(
    !(stkcd == 638 & year == 2003 & length_all == 0)
  )

# import: Stock market performance ----
stock_return <- import(
  "inst/extdata/年个股回报率文件/TRD_Year.xlsx",
  setclass = "tbl"
) %>%
  slice(-c(1:2)) %>%
  rename_with(tolower) %>%
  select(stkcd, year = trdynt, return = yretwd) %>%
  mutate(across(everything(), as.numeric))

# merge ----
data_gross <- bi_base %>%
  left_join(bi_st) %>%
  left_join(fs_balance, by = "stkcd",
            multiple = "all") %>%
  filter(year >= 2001, prov_cs != "") %>%
  complete(
    nesting(stkcd, year_estb, year_list, prov_cs,
            city_cs, ind_cs, ind_class, state_cs),
    year = 2001:2021
  ) %>%
  left_join(stock_return, by = c("stkcd", "year")) %>%
  left_join(bi_add,       by = c("stkcd", "year")) %>%
  left_join(bi_ccer,      by = c("stkcd", "year")) %>%
  left_join(fs_income,    by = c("stkcd", "year")) %>%
  left_join(fs_cashflow,  by = c("stkcd", "year")) %>%
  left_join(fi_profit,    by = c("stkcd", "year")) %>%
  left_join(fi_ratio,     by = c("stkcd", "year")) %>%
  left_join(fi_rvi,       by = c("stkcd", "year")) %>%
  left_join(fi_turnover,  by = c("stkcd", "year")) %>%
  left_join(ci_citation,  by = c("stkcd", "year")) %>%
  left_join(ci_patent,    by = c("stkcd", "year")) %>%
  left_join(ci_rd,        by = c("stkcd", "year")) %>%
  left_join(ci_rd_other,  by = c("stkcd", "year")) %>%
  left_join(cg_ownership, by = c("stkcd", "year")) %>%
  left_join(cg_gov,       by = c("stkcd", "year")) %>%
  left_join(cg_indabs,    by = c("stkcd", "year")) %>%
  left_join(cg_indrat,    by = c("stkcd", "year")) %>%
  left_join(digit_text,   by = c("stkcd", "year"))
rm(list = setdiff(ls(), "data_gross"))

# process ----
db_alistfirm <- data_gross %>%
  mutate(
    # 根据横截面补全行业、地址、SOE
    across(c(prov_cs, city_cs, state_cs),
           ~ ifelse(.x == "", NA, .x)),
    ind_code = ifelse(is.na(ind_code),
                       ind_cs, ind_code),
    province = ifelse(is.na(province),
                       prov_cs, province),
    city = ifelse(is.na(city), city_cs, city),
    state = ifelse(is.na(state), state_cs, state),
    # 所有权性质二元变量；“其他”设为NA
    soe_csmar = case_when(
      str_detect(state, "国企|国有|政府") ~ "国有",
      str_detect(state, "其他") ~ NA_character_,
      T ~ "非国有"
    ),
    soe_csmar_num = if_else(soe_csmar == "国有", 1, 0),
    # CCER应该用截面补还是用时变补？
    soe_ccer = case_when(
      !is.na(soe_ccer) ~ soe_ccer,
      str_detect(state_cs, "国有|政府") ~ "国有",
      state_cs == "其他" ~ NA_character_,
      T ~ "非国有"
    ),
    soe_ccer_num = case_when(
      soe_ccer == "国有" ~ 1,
      soe_ccer == "非国有" ~ 0,
    )
  ) %>%
  select(
    stkcd, year, province, city, ind_code,
    sort(names(.))
  )
save(
  db_alistfirm,
  file = "data/db_alistfirm.RData"
)

}
