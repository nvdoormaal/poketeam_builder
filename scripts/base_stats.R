get_base_stats <- function(){
  
bulbapedia <- 
  read_html(
    "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_base_stats_(Generation_II-V)"
  ) %>% 
  html_element(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
  html_table() %>% 
  remove_constant() %>% 
  clean_names()
}
