
# Load necessary libraries
library(readxl)
library(lubridate)
library(stringr)
library(janitor)
library(skimr)
library(here)
library(gt)
library(knitr)
library(writexl)
library(tidyverse)
library(rlang)
library(openxlsx)

# Set working directory using the here package
here::here()

# Load custom functions from separate R scripts 
source("scripts/read_clean_survey_data.R")
source("scripts/balance_score.R")
source("scripts/freq_gt_tables.R")


# Set output paths for this specific region 
region <- "karakalpak"
output_path <- paste0("output/", region, "/analysis_output.xlsx")
output_folder <- paste0("output/", region)

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Read in data and clean it using the custom function read_clean_data()
survey_data <- read_clean_data(region) %>% 
  clean_names() %>%
  filter(district != "Давлатобод") 

survey_data %>% filter(is_working != "Пенсиядаман") %>% tabyl (gender, is_working) %>% adorn_percentages()

# Apply the function to each question column by district with custom labels
district_bs_score <- survey_data %>% 
  group_by(district) %>% 
  summarise(q_1 = balance_score(table(q_1), pos_labels_q1),
            q_2 = balance_score(table(q_2), pos_labels_q2),
            q_3 = balance_score(table(q_3), pos_labels_q3),
            q_4 = balance_score(table(q_4), pos_labels_q4),
            q_5 = balance_score(table(q_5), pos_labels_q5),
            q_6 = balance_score(table(q_6), pos_labels_q6)) 

# Create a new data frame with values for whole region
whole_region <- data.frame(district = "Whole Region",
                           q_1 = balance_score(table(survey_data$q_1), pos_labels_q1),
                           q_2 = balance_score(table(survey_data$q_2), pos_labels_q2),
                           q_3 = balance_score(table(survey_data$q_3), pos_labels_q3),
                           q_4 = balance_score(table(survey_data$q_4), pos_labels_q4),
                           q_5 = balance_score(table(survey_data$q_5), pos_labels_q5),
                           q_6 = balance_score(table(survey_data$q_6), pos_labels_q6))

# Append the whole region data to the district data using rbind(), then create new columns using mutate()
gen_bs_score <- rbind(district_bs_score, whole_region) %>%
  mutate(bs_score_cur = (q_2 + q_4 + q_6)/3,
         bs_score_fut = (q_1 + q_3 + q_5)/3,
         bs_gen = (bs_score_cur + bs_score_fut)/2) %>% 
  mutate_if(is.numeric, round, digits=0) %>% 
  select(district, starts_with("bs"))

# Frequency table for question q_7
q7 <- survey_data %>%
  tabyl(district, q_7) %>% 
  adorn_totals() %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>% 
  mutate_at(vars(-district), as.double) %>% 
  mutate(across(where(is.numeric), round, digits=0))

# Frequency table for question q_8
q8 <- survey_data %>%
  mutate(q_8 = str_replace_all(q_8, "\\(.*\\)", "")) %>% 
  separate_rows(q_8, sep = ",") %>%
  mutate(q_8 = str_trim(q_8)) %>% 
  mutate(q_8 = recode(q_8, 
                      "Соғлиқни сақлаш хизматлари сифатини ошириш  борасидаги ишлар" = "Соғлиқни сақлашда",
                      "Мактабгача таълим тизими билан қамров даражасини ошириш борасидаги ишлар" = "Мактабгача таълим тизими қамровида",
                      "Мактабда таълим сифатини ошириш борасидаги ишлар" = "Мактаб таълим сифатида",
                      "Аҳолини уй-жой билан таъминлаш ишларини" = "Уй-жой билан таъминлаш",
                      "Маҳаллабай ишлаш ва ҳоким ёрдамчилари тизимидаги ишларни" = "Маҳаллабай ишлаш ва ҳоким ёрдамчилари",
                      "Ўқитувчиларни қўллаб-қувватлаш ишларини" = "Ўқитувчиларни қўллаб-қувватлаш",
                      "Шифокорларни қўллаб-қувватлаш ишларини" = "Шифокорларни қўллаб-қувватлаш",
                      "Коррупцияга қарши курашишлар" = "Коррупцияга қарши курашиш",
                      "Камбағаликка қарши кураш ишларини" = "Камбағаликка қарши кураш",
                      "Олий таълим тизимидаги ислоҳотларни" = "Олий таълимдаги ислоҳотлар",
                      "Тадбиркорларни қўллаб-қувватлаш ишларини" = "Тадбиркорларни қўллаб-қувватлаш",
                      "Автомобил сотиб олиш жараёнларини" = "Автомобил сотиб олиш жараёнларида",
                      "Газ таъминоти соҳаси" = "Газ таъминотида",
                      "Иш билан таъминлаш соҳаси" = "Бандликда",
                      "Электр таъминоти соҳасида" = "Электр таъминотида",
                      "Ахолини ижтимоий куллаб кувватлаш" = "Ижтимоий қўллаб-қувватлаш",
                      "Йул курилиши тизимини ривожлантириш" = "Йўл қуриш ишларида",
                      .default = "Бошқа")) %>% 
  count(district, q_8) %>%
  group_by(district) %>%
  mutate(freq = n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>% 
  pivot_wider(names_from = q_8, values_from = freq, values_fill = 0) %>%
  mutate(across(-district, as.double)) %>%
  adorn_totals() %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE)


# Create a list of data frames to write to the Excel file
list_of_dataframes <- list("index"= gen_bs_score, "muammolar"=q7, "islohotlar_tezlatish"=q8)  

# Create a new workbook and add worksheets for each data frame in the list
wb <- createWorkbook()
lapply(seq_along(list_of_dataframes), function(i){
  addWorksheet(wb=wb, sheetName = names(list_of_dataframes[i]))
  writeData(wb, sheet = i, list_of_dataframes[[i]])
})
#Save Workbook
saveWorkbook(wb, output_path, overwrite = TRUE) 

#Create a list of column names
cols <- c("q_1", "q_3", "q_5", "q_2", "q_4", "q_6", "q_9", "is_working", "is_official", "income")

#Create a list of labels for each column
labels <- list(c("Ёмонлашади", "Ўзгармайди", "Яхшиланади"), 
               c("Қисқаради", "Ўзгармайди", "Кўпаяди"), 
               c("Камаяди", "Ўзгармайди", "Кўпаяди"), 
               c("Пасайди", "Ўзгармади", "Ошди"), 
               c("Камайди", "Ўзгармади", "Кўпайди"), 
               c("Йўқ", "Билмайман", "Ҳа"), 
               c("Жуда ёмон", "Ёмон", "Ўртача", "Яхши", "Жуда яхши"),
               c("Йўқ", "Ҳа"),
               c("Йуқ", "Ҳа"),
               c("Даромади мавжуд эмас", "1 млн сўмгача", "1-3 млн", "3 млн сўмдан баланд")
               )

#Create a list of titles for each column
titles <- list("*1-савол.* **3 ойдан сўнг Ўзбекистонда <span style='color:red'>иқтисодий ҳолат</span> қандай ўзгаради?**", 
               "*3-савол*. **Кейинги 3 ой давомида <span style='color:red'>оилавий даромадингиз</span> қандай кутиляпти?**",
               "*5-савол*. **3 ойдан сўнг  <span style='color:red'>янги иш ўринлари сони</span> қандай ўзгаради?**",
               "*2-савол*. **Ўтган 3 ойга нисбатан <span style='color:red'>оилавий даромадингиз </span> қандай ўзгарди?**",
               "*4-савол*. **Ўтган 3 ойга нисбатан <span style='color:red'>янги иш ўринлари  </span> қандай ўзгарди?**",
               "*6-савол*. **Ҳозир узоқ муддатли <span style='color:red'> истеъмол товарларини харид қилиш  </span> учун қулай фурсатми?**",
               "**Маҳаллий ҳокимият органи фаолиятини қандай баҳолайсиз?**", 
               "**Туманлар (шаҳарлар) кесимида меҳнат ресурсларнинг <span style='color:red'>иш билан бандлик</span>  ҳолати**",
               "**Туманлар (шаҳарлар) кесимида расмий меҳнат фаолияти билан банд бўлганлар ҳолати**",
               "**Аҳоли ойлик даромадининг тақсимланиши**")

#Create a list of subtitles for each column
subtitles <- rep("(*Респондентларнинг жавоблари*)", length(cols))

#Use map to loop over the columns and apply the function
tables <- map(seq_along(cols), function(i) { 
  create_frequency_table (survey_data, cols[i], labels[[i]], titles[[i]], subtitles[[i]])
    }) 


#Save each table as an image file using gtsave and paste0 
map(seq_along(tables), function(k) {
  gtsave(tables[[k]], file = paste0(output_folder, "/table_", cols[k], ".png"))
})