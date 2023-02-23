
#gt_tables
my_theme_gt <- function(data, ...) {
  data %>% 
    tab_stubhead(label = md("**Туманлар**")) %>% 
    #mutate_at(vars(everything()), as.numeric) %>% 
    fmt_percent(columns = everything()[-1], decimals = 0) %>% 
    grand_summary_rows(columns = everything(),
                       fns = list("Вилоят бўйича ўртача" = ~ mean(., na.rm = TRUE)), 
                       formatter = fmt_percent,  decimals = 0) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>% 
    cols_align(align = "center", columns = everything()) %>% 
    tab_options(
      heading.subtitle.font.size = 13,
      heading.align = "center",
      table.border.top.color = "white",
      column_labels.border.bottom.width= px(3),
    ) %>% 
    tab_style(
      style = cell_borders(sides = "top", 
                           color = "black", weight = px(3)),
      locations = cells_column_labels(everything())
    ) %>% 
    tab_style(
      style = cell_borders(sides = "top", 
                           color = "black", weight = px(3)),
      locations = cells_stubhead()) %>% 
    tab_style(
      style = cell_borders(sides = "bottom", 
                           color = "black", weight = px(3)),
      locations = cells_grand_summary()) %>% 
    tab_style(
      style = cell_borders(sides = "bottom", 
                           color = "black", weight = px(3)),
      locations = cells_stub_grand_summary()) %>% 
    tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = cells_stub_grand_summary()
    ) %>% 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_grand_summary()
    ) 
}


# Define a function that takes the data frame, the variable name, the labels, the title and the subtitle as arguments
create_frequency_table <- function(data, var, labels, title, subtitle) {

  # Filter the data by the desired values
  if(var == "is_working") {
    data <- data %>%
      filter(is_working != "Пенсиядаман")
  } else if(var == "is_official") {
    data <- data %>%
      filter(is_working == "Ҳа")
  } else if(var == "income") {
    data <- data %>%
      filter(income != "Жавоб беришдан бош тортди") %>% 
      mutate(income = str_replace_all(income, " ", "")) %>%  
      mutate(income = as.double(income)) %>% 
      mutate(income_group = case_when(income == 0 ~ "Даромади мавжуд эмас",
                                      income >= 0 & income <= 1000000 ~ "1 млн сўмгача",
                                      income >= 1000001 & income <= 3000000 ~ "1-3 млн",
                                      income >= 3000001 ~ "3 млн сўмдан баланд"))
  }
   
  # Create a frequency table using tabyl and adorn_percentages
  table <- data %>%
    tabyl(district, !!sym(var)) %>%
    adorn_percentages() %>% 
    select(district, all_of(labels)) %>% 
    mutate_at(vars(-district), as.double) %>% 
    arrange(desc(across(starts_with(labels[1])))) %>% 
    as_tibble() %>% # Convert to tibble
    gt(rowname_col = "district") %>% # Create gt table
    tab_header(title = md(title),
               subtitle = md(subtitle)) %>% 
    my_theme_gt() %>% 
    cols_width(everything() ~ px(180))
  
  # Return the table as a gt object
  return(table)
}