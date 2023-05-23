#' @title A-share listed firm
#'
#' @docType data
#' @name db_alistfirm
#'
#' @description A dataset containing listed firm data
#'     during 2001 ~ 2021.
#'
#' @format It has 49 variables:
#' \describe{
#'   \item{stkcd}{stock code}
#'   \item{year}{year}
#'   \item{province}{office address: province}
#'   \item{prov_cs}{cross-sectional province, downloaded from CSMAR}
#'   \item{city}{office address: city}
#'   \item{city_cs}{cross-sectional city, downloaded from CSMAR}
#'   \item{ind_code}{industry code CSRC-2012}
#'   \item{ind_cs}{cross-sectional industry, downloaded from CSMAR}  #
#'   \item{ind_class}{industry class, only six categories}
#'   \item{admin_expense}{administration (handling) expense}
#'   \item{book_equity}{book value of equity}
#'   \item{btm}{book-to-market ratio}
#'   \item{cash}{cash holdings}
#'   \item{cash_ratio}{cash holdings over total assets}
#'   \item{ceo_duality}{}
#'   \item{ceo_duality_num}{}
#'   \item{checkandbalance_equity}{}
#'   \item{citation}{}
#'   \item{construction}{}
#'   \item{current_asset}{}
#'   \item{current_asset_to}{}
#'   \item{current_debt}{}
#'   \item{digit_a}{}
#'   \item{digit_b}{}
#'   \item{director_size}{}
#'   \item{director_wage}{}
#'   \item{dividend_pay}{}
#'   \item{ebitda}{}
#'   \item{fix_asset}{}
#'   \item{inddir_absence}{}
#'   \item{inddir_rate}{}
#'   \item{inddir_workplace}{}
#'   \item{intan_ratio}{}
#'   \item{invest_pay}{}
#'   \item{invest_receive}{}
#'   \item{invisible_asset}{}
#'   \item{labor}{}
#'   \item{length_all}{}
#'   \item{length_txt}{}
#'   \item{long_borrow}{}
#'   \item{long_pay}{}
#'   \item{market_equity}{}
#'   \item{net_profit}{}
#'   \item{operate_cash}{}
#'   \item{patent}{}
#'   \item{patent_grt}{}
#'   \item{patent_inv}{}
#'   \item{rd}{}
#'   \item{rd_expense}{}
#'   \item{rd_invest}{}
#'   \item{rd_person}{}
#'   \item{rd_person_ratio}{}
#'   \item{receive_ratio}{}
#'   \item{retain_earn}{}
#'   \item{return}{}
#'   \item{revenue}{}
#'   \item{roa}{}
#'   \item{roe}{}
#'   \item{sale_expense}{}
#'   \item{seperation}{}
#'   \item{short_borrow}{}
#'   \item{soe_ccer}{}
#'   \item{soe_ccer_num}{}
#'   \item{soe_csmar}{}
#'   \item{soe_csmar_num}{}
#'   \item{special_pay}{}
#'   \item{st}{}
#'   \item{state}{ownership type, from CSMAR}                         # 时变所有权性质
#'   \item{state_cs}{cross-sectional ownershiptype, from CSMAR}       # 截面所有权性质
#'   \item{supervisor_size}{number of supervisors}                    # 监事会人数，包括主席
#'   \item{tobinq}{Tobin's Q ratio}                                   # 托宾Q
#'   \item{top10_share}{share prop. of the largest ten shareholder}   # 前十大股东占比
#'   \item{top1_share}{share proportion of the largest shareholder}   # 第一大股东占比
#'   \item{total_asset}{total assets}                                 # 总资产
#'   \item{total_debt}{total liabilities}                             # 总负债
#'   \item{year_estb}{the year of establishment}                      # 成立年份
#'   \item{year_list}{the year of IPO}                                # 上市年份
#' }
#'
#' @source CSMAR, Wind, CCER
#'
NULL
