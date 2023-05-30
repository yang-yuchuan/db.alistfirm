#' @title A-share listed firm
#'
#' @docType data
#' @name db_alistfirm_quarterly
#'
#' @description A dataset containing listed firm data during 2001 ~ 2021.
#'     For detailed Chinese information, see
#'     https://github.com/yang-yuchuan/db.alistfirm/blob/master/R/data.R
#'
#' @format It has 80 variables:
#' \describe{
#'   \item{stkcd}{stock code}
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{quarter}{quarter = 1, 2, 3, 4}
#'   \item{province}{office address: province}
#'   \item{prov_cs}{cross-sectional province, from CSMAR}
#'   \item{city}{office address: city}
#'   \item{city_cs}{cross-sectional city, from CSMAR}
#'   \item{ind_code}{industry code CSRC-2012}
#'   \item{ind_cs}{cross-sectional industry, from CSMAR}
#'   \item{ind_class}{industry class, only six categories}
#'   \item{admin_expense}{administration (handling) expense}
#'   \item{book_equity}{book value of equity}
#'   \item{btm}{book-to-market ratio}
#'   \item{cash}{cash holdings}
#'   \item{cash_ratio}{cash holdings over total assets}
#'   \item{ceo_duality}{whether CEO and general manager is the same}
#'   \item{ceo_duality_num}{numerical version of `ceo_duality`}
#'   \item{checkandbalance_equity}{(`top10_share` - `top1_share`) / `top1_share`}
#'   \item{citation}{total number of patent citations}
#'   \item{construction}{net amount of construction in process}
#'   \item{current_asset}{current assets}
#'   \item{current_asset_to}{current assets turnover}
#'   \item{current_debt}{current liabilities}
#'   \item{digit_a}{digitalization based on text analysis, see 10.19744/j.cnki.11-1235/f.2021.0097}
#'   \item{digit_b}{digitalization based on text analysis, see 10.19795/j.cnki.cn11-1166/f.20210705.001}
#'   \item{director_size}{number of directors}
#'   \item{director_wage}{total salary of directors}
#'   \item{dividend_pay}{dividend payment}
#'   \item{ebitda}{EBITDA}
#'   \item{fix_asset}{fixed assets}
#'   \item{inddir_absence}{1 if any independent director has attended board meeting, 0 otherwise}
#'   \item{inddir_rate}{number of independent directors over `director_size`}
#'   \item{inddir_workplace}{whether the office of independent director located in the same city as the firms}
#'   \item{intan_ratio}{intangible assets over total assets}
#'   \item{invest_pay}{investment payment}
#'   \item{invest_receive}{investment receivings}
#'   \item{invisible_asset}{invisible assets}
#'   \item{labor}{number of staff}
#'   \item{length_all}{total word count of annual report}
#'   \item{length_txt}{word count of Chinese/English in annual report}
#'   \item{long_borrow}{long-term loan}
#'   \item{long_pay}{long-term payable}
#'   \item{market_equity}{market value of equity}
#'   \item{net_profit}{net profit}
#'   \item{operate_cash}{operating cash flow}
#'   \item{patent}{patent application}
#'   \item{patent_grt}{patent application that granted}
#'   \item{patent_inv}{patent application for invention that granted}
#'   \item{rd}{R&D}
#'   \item{rd_expense}{expensed R&D}
#'   \item{rd_invest}{capitalized R&D}
#'   \item{rd_person}{research staff}
#'   \item{rd_person_ratio}{`rd_person` / `labor`}
#'   \item{receive_ratio}{receivable assets over total assets}
#'   \item{retain_earn}{retained earnings}
#'   \item{return}{yearly averaged stock return}
#'   \item{revenue}{operating revenue}
#'   \item{roa}{ROA}
#'   \item{roe}{ROE}
#'   \item{sale_expense}{selling expense}
#'   \item{seperation}{seperation of control right and ownership right}
#'   \item{short_borrow}{short-term loan}
#'   \item{soe_ccer}{whether state-owned-enterprise (SOE), from CCER}
#'   \item{soe_ccer_num}{numerical version of `soe_ccer`}
#'   \item{soe_csmar}{whether SOE, from CSMAR}
#'   \item{soe_csmar_num}{numerical version of `soe_csmar`}
#'   \item{special_pay}{special payment}
#'   \item{st}{is this firm coded "ST"?}
#'   \item{state}{ownership type, from CSMAR}
#'   \item{state_cs}{cross-sectional ownershiptype, from CSMAR}
#'   \item{supervisor_size}{number of supervisors}
#'   \item{tobinq}{Tobin's Q ratio}
#'   \item{top10_share}{share prop. of the largest ten shareholder}
#'   \item{top1_share}{share proportion of the largest shareholder}
#'   \item{total_asset}{total assets}
#'   \item{total_debt}{total liabilities}
#'   \item{year_estb}{the year of establishment}
#'   \item{year_list}{the year of IPO}
#' }
#'
#' @source CSMAR, Wind, CCER
#'
NULL


# stkcd                  - 股票代码
# year                   - 年份
# province               - 省份信息，根据CSMAR办公地址经纬度用高德地图API爬取
# prov_cs                - 截面省份，来自CSMAR
# city                   - 城市信息，根据CSMAR办公地址经纬度用高德地图API爬取
# city_cs                - 截面城市，来自CSMAR
# ind_code               - 行业代码，根据证监会2012分类
# ind_cs                 - 截面行业代码，来自CSMAR
# ind_class              - 行业大类：金融、公用、房地产、综合、工业、商业
# admin_expense          - 管理费用
# book_equity            - 账面价值
# btm                    - 账面市值比
# cash                   - 期末现金余额
# cash_ratio             - 现金比率
# ceo_duality            - CEO和总经理是否为同一人
# ceo_duality_num        - 0/1变量版本
# checkandbalance_equity - 股权制衡：第二到九大股东持股比例 / 第一大股东持股比例
# citation               - 总专利被引用次数
# construction           - 在建工程净额
# current_asset          - 流动资产
# current_asset_to       - 流动资产周转率
# current_debt           - 流动负债
# digit_a                - 企业数字化文本指标，基于吴非等（2021）
# digit_b                - 企业数字化文本指标，基于赵宸宇（2021）
# director_size          - 董事会人数
# director_wage          - 董事会工资总额
# dividend_pay           - 股利分配
# ebitda                 - 息税折旧摊销前利润
# fix_asset              - 固定资产
# inddir_absence         - 独立董事是否出席过董事会会议
# inddir_rate            - 董事会中独立董事占比
# inddir_workplace       - 独立董事工作地址是否与公司办公地址在同一个城市
# intan_ratio            - 无形资产比率
# invest_pay             - 购建固定资产、无形资产和其他长期资产支付的现金
# invest_receive         - 处置固定资产、无形资产和其他长期资产收回的现金
# invisible_asset        - 无形资产
# labor                  - 员工人数
# length_all             - 年报全文总文本长度
# length_txt             - 年报中英文文本长度
# long_borrow            - 长期借款
# long_pay               - 长期应付款
# market_equity          - 市值
# net_profit             - 净利润
# operate_cash           - 经营活动现金流
# patent                 - 专利申请
# patent_grt             - 专利申请：其中已授权
# patent_inv             - 专利申请：发明专利
# rd                     - R&D投入
# rd_expense             - 费用化的R&D投入
# rd_invest              - 资本化的R&D投入
# rd_person              - 研发人员数量
# rd_person_ratio        - 研发人员占比
# receive_ratio          - 应收资产比率
# retain_earn            - 未分配利润
# return                 - 股票收益
# revenue                - 营业收入
# roa                    - ROA
# roe                    - ROE
# sale_expense           - 销售费用
# seperation             - 两权分离率
# short_borrow           - 短期借款
# soe_ccer               - CCER数据库的国企指标
# soe_ccer_num           - 0/1变量版本
# soe_csmar              - CSMAR数据库的国企指标
# soe_csmar_num          - 0/1变量版本
# special_pay            - 专项应付款
# st                     - 是否为ST/PT股
# state                  - 时变所有权性质
# state_cs               - 截面所有权性质
# supervisor_size        - 监事会人数，包括主席
# tobinq                 - 托宾Q
# top10_share            - 前十大股东占比
# top1_share             - 第一大股东占比
# total_asset            - 总资产
# total_debt             - 总负债
# year_estb              - 成立年份
# year_list              - 上市年份


