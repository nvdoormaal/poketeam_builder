## Function for obtaining dual-type data
get_dual_type <- function(x, keep_fairy=FALSE){
  url <- "https://pokemondb.net/type/dual"
  
  data <- url %>% 
    read_html() %>% 
    html_element(css = "#dualtypechart") %>% 
    html_table() %>% 
    clean_names() %>% 
    rename(
      "dual_type" = attack_defense_u_21b4
    ) %>% 
    filter(
      pkmn != "PKMN",
    ) %>% 
    select(-pkmn)

  if (keep_fairy == FALSE) {
    data <- data %>% 
      filter(
        str_detect(dual_type, pattern = "Fairy", negate = TRUE)
      ) %>% 
      select(-fai)
  }
  base_score <- if_else(
    keep_fairy == FALSE, 17, 18
  )
  final_data <- data %>% 
    separate(col = dual_type, 
             into = c("type_one", "type_two"), sep = "\\s", extra = "merge") %>% 
    mutate(
      across(.cols = type_one:type_two, ~
               trimws(tolower(.))
      ),
      across(
        .cols = nor:last_col(), ~ 
          recode(., "¼" = 0.25,  "½" = 0.5, "0" = 0, "2" = 2, "4" = 4, .default = 1)
      ),
      type_two = na_if(type_two, "—"),
      type_score = rowSums(across(where(is.numeric)), na.rm = TRUE),
      score_multiplier = 1 + (type_score / base_score) - 1
      )
      
  return(final_data)
}
