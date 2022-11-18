get_poke_data <- function(url, time = 5){
  
  page <- read_html(url)
  
  number <- page %>% 
    html_element(css = ".fooinfo:nth-child(3) tr:nth-child(1) td+ td") %>% 
    html_text() %>% 
    parse_number()
  name <- page %>% 
    html_element(css = "a+ .dextable tr:nth-child(2) .fooinfo:nth-child(1)") %>%
    html_text()
  type_one <- page %>% 
    html_element(css = ".fooinfo+ .fooinfo a:nth-child(1) img") %>% 
    html_attr("src") %>% 
    str_remove_all("/pokedex-dp/type/|.gif")
  type_two <- page %>% 
    html_element(css = "a+ a img") %>% 
    html_attr("src") %>% 
    str_remove_all("/pokedex-dp/type/|.gif")
  location_one <- page %>% 
    html_element("p+ .dextable .heartgold+ .fooinfo") %>% 
    html_text() %>% 
    str_remove_all("\n|\t")
  location_two <- page %>% 
    html_element("p+ .dextable .soulsilver+ .fooinfo") %>% 
    html_text() %>% 
    str_remove_all("\n|\t")
  first_form_number <- page %>% 
    html_element(".evochain .pkmn:nth-child(1) img") %>% 
    html_attr("src") %>% 
    str_remove_all("\\D") %>% 
    parse_number()
  
  poke_tibble <- tibble(
    number = number,
    name = name,
    type_one = type_one,
    type_two = type_two,
    location_one = location_one,
    location_two = location_two,
    first_form_number = first_form_number
  )
  return(poke_tibble)
  Sys.sleep(time = time)
}