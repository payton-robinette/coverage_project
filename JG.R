library(tidyverse)
library(hrbrthemes)
library(rms)
library(janitor)
library(ggsci)

enrollment_ <- #https://www2.census.gov/programs-surveys/demo/tables/health-insurance/time-series/acs/hic04_acs.xlsx
  tibble::tribble(
    ~year,  ~Total, ~Any.coverage, ~Uninsured, ~Private, ~`..Employer-based`, ~`..Direct-purchase`, ~..TRICARE, ~Public, ~..Medicaid, ~..Medicare, ~..VA.Care,
    2023L, 330000L,       303800L,     26170L,  221100L,             180400L,               45770L,      9088L, 123500L,      70330L,      61900L,      7396L,
    2022L, 328300L,       301900L,     26370L,  220700L,             179900L,               45720L,      8925L, 122000L,      69710L,      60860L,      7358L,
    2021L, 326900L,       298700L,     28230L,  219100L,             178700L,               44860L,      8954L, 120200L,      68980L,      59560L,      7140L,
    2019L, 323100L,       293500L,     29640L,  217800L,             178900L,               42300L,      8782L, 114300L,      64080L,      58330L,      7247L,
    2018L, 322200L,       293700L,     28570L,  217600L,             177700L,               43190L,      8767L, 114800L,      65970L,      56870L,      7477L,
    2017L, 320800L,       292800L,     28020L,  217000L,             176300L,               43410L,      8640L, 113700L,      66130L,      55470L,      7290L,
    2016L, 318200L,       290900L,     27300L,  215900L,             173900L,               44820L,      8566L, 112700L,      66360L,      54130L,      7291L,
    2015L, 316500L,       286700L,     29760L,  213500L,             172300L,               43630L,      8415L, 109900L,      64740L,      52690L,      7044L,
    2014L, 313900L,       277200L,     36670L,  208300L,             170300L,               40220L,      8311L, 104200L,      60350L,      51110L,      6911L,
    2013L, 311200L,       266000L,     45180L,  202400L,             168100L,               37020L,      8228L,  98170L,      55670L,      49420L,      6735L,
    2012L, 308900L,       263300L,     45620L,  201200L,             168700L,               37680L,      8291L,  96140L,      55180L,      47910L,      6700L,
    2011L, 306600L,       260200L,     46380L,  199900L,             167500L,               37450L,      8118L,  93420L,      54010L,      46100L,      6471L,
    2010L, 304300L,       257100L,     47210L,  200300L,             167100L,               38720L,      7905L,  90380L,      51860L,      44820L,      6266L,
    2009L, 301500L,       255800L,     45670L,  203300L,             170600L,               39630L,      7639L,  85960L,      48770L,      43490L,      6201L,
    2008L, 298600L,       255100L,     43500L,  207800L,             175300L,               42350L,      7615L,  81430L,      45230L,      42650L,      6261L
  ) %>%
  clean_names()


# New Source: (use )
# https://data.cms.gov/summary-statistics-on-beneficiary-enrollment/medicare-and-medicaid-reports/medicare-monthly-enrollment

medicare_enrollment_ <- #Source: https://data.cms.gov/summary-statistics-on-beneficiary-enrollment/medicare-and-medicaid-reports/medicare-total-enrollment
  tibble::tribble(
    ~year, ~original,        ~ma,
    2008L, 35496280L,    9985121,
    2009L, 35482719L,   11077880,
    2010L, 35996264L,   11667132,
    2011L, 36539482L,   12353276,
    2012L, 37213622L,   13555092,
    2013L, 37613096L,   14812563,
    2014L, 37790373L,   16222665,
    2015L, 38025274L,   17470948,
    2016L, 38610384L,   18370800,
    2017L, 38667830L,   19789414,
    2018L, 38665082L,   21324800,
    2019L, 38577012L,   22937498,
    2020L, 37776345L,   25063922
  )

mmc_enrollment_ <-
  tibble::tribble(
    ~year, ~total_medicaid_enrollees, ~comprehensive_managed_care,                                                                                                    ~source,
    2019L,                  82955000,                       0.696,                        https://www.macpac.gov/subtopic/enrollment-and-spending-on-medicaid-managed-care/,
    2018L,                  78393091,                        0.69,                                                                                                         NA,
    2017L,                  78840664,                       0.687, https://www.macpac.gov/wp-content/uploads/2015/12/MACStats-Medicaid-and-CHIP-Data-Book-December-2019.pdf,
    2016L,                  78567000,                       0.675,                   https://www.macpac.gov/wp-content/uploads/2018/12/December-2018-MACStats-Data-Book.pdf,
    2015L,                  76388150,                       0.648,     https://www.macpac.gov/wp-content/uploads/2015/12/MACStats-Medicaid-CHIP-Data-Book-December-2017.pdf,
    2014L,                  70246197,                       0.597, https://www.macpac.gov/wp-content/uploads/2020/07/MACStats-Medicaid-and-CHIP-Data-Book-December-2016.pdf,
    2013L,                  60512000,                       0.553, https://www.macpac.gov/wp-content/uploads/2020/07/MACStats-Medicaid-and-CHIP-Data-Book-December-2015.pdf,
    2012L,                58259479.5,                       0.498,                                 https://www.macpac.gov/wp-content/uploads/2015/03/June-2014-MACStats.pdf,
    2011L,                  56006959,                       0.502,                                 https://www.macpac.gov/wp-content/uploads/2015/03/June-2013-MACStats.pdf,
    2010L,                  53565848,                        0.48,                                 https://www.macpac.gov/wp-content/uploads/2015/03/June-2012-MACStats.pdf,
    2009L,                  49450645,                       0.468,                                 https://www.macpac.gov/wp-content/uploads/2015/03/June-2011-MACStats.pdf
  )

df <-
  enrollment_ %>%
  select(year,uninsured,private,medicaid,medicare,va_care)  %>%
  inner_join(
    medicare_enrollment_ %>%
      mutate(medicare_mco_share = ma/(original+ma)) %>%
      select(year,medicare_mco_share) ,"year"
  ) %>%
  inner_join(
    mmc_enrollment_ %>%
      select(year,medicaid_mco_share = comprehensive_managed_care)
    ,"year"
  ) %>%
  mutate(total = private + medicaid + medicare + va_care) %>%
  mutate(total_managed = private + medicaid_mco_share * medicaid + medicare_mco_share * medicare) %>%
  mutate(pct_managed = total_managed / total)

text_size = 5

df %>%
  mutate(share_private = private / total,
         share_medicaid = (medicaid_mco_share * medicaid) / total,
         share_medicare = (medicare_mco_share * medicare) / total) %>%
  select(year, starts_with("share")) %>%
  gather(measure,value,-year)  %>%
  ggplot(aes(x = year, y = value, fill=measure)) + geom_area() +
  theme_ipsum() +
  ylim(c(0,1)) +
  scale_x_continuous(breaks = seq(2009,2019,2)) +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = paste0(100*seq(0,1,0.25),"%"), limits = c(0,1)) +
  ggsci::scale_fill_d3() +
  labs(x = "", y = "" ) +
  theme(legend.position = "none") +
  annotate("text",x = 2011, y = 0.5,
           label = "Employer-Sponsored, TRICARE,\nand Individually-Purchased", col = "white", size = text_size,hjust=0,fontface=2) +
  annotate("text",x = 2011, y = 0.67,
           label = "Medicare Advantage", col = "white", size = text_size,hjust=0,fontface=2) +
  annotate("text",x = 2011, y = 0.74,
           label = "Medicaid Managed Care", col = "white", size = text_size,hjust=0,fontface=2) +
  annotate("text", x = 2009, y = .81, col = "black", size = text_size, hjust=0, fontface =2, label = "78%") +
  annotate("text", x = 2019, y = .85, col = "black", size = text_size, hjust=1, fontface =2, label = "82%") +
  annotate("text", x = 2009.5, y = .15, col = "white", size = text_size, hjust=0, fontface =2,
           label = "Source: Authors' analysis of\nAmerican Community Survey, Medicare\nand Medicaid enrollment data")

ggsave(here("figures/percent-with-private.png"),width = 6, height = 8)



kmedicare_enrollment <-
  medicare_enrollment_ %>%
  mutate(total = original + ma) %>%
  gather(group,value,-total,-year) %>%
  mutate(percent = 100 * value / total) %>%
  mutate(type = "observed")

knots <- rcspline.eval(medicare_enrollment_$year)
fit_ma <- with(medicare_enrollment_, glm(ma ~ rcspline.eval(year,knots=knots)))
fit_orig <- with(medicare_enrollment_, glm(original ~ rcspline.eval(year,knots=knots)))
predicted <-
  data.frame(year = 2021:2030,
             ma = predict(fit_ma, newdata = data.frame(year = 2021:2030)),
             original = predict(fit_orig, newdata = data.frame(year = 2021:2030))) %>%
  mutate(total = original + ma) %>%
  gather(group,value,-total,-year) %>%
  mutate(percent = 100 * value / total) %>%
  mutate(type = "predicted")

df_p <- medicare_enrollment %>%
  bind_rows(predicted)

df_p %>%
  ggplot() +
  geom_bar(aes(fill = group, y = value, x = year), position = "fill", stat="identity",alpha = 0.5) +
  geom_bar(data = df_p %>% filter(type=="observed"), aes(fill = group, y = value, x = year), position = "fill", stat="identity",alpha = 1) +
  theme_ipsum() +
  ggsci::scale_fill_jama() +
  theme(legend.position = "none") +
  labs(x = "year", y = "Share of Enrollment")
