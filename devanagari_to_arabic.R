devanagari_to_arabic <- function(inputdf) {
  inputdf %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '०', '0')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '१', '1')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '२', '2')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '३', '3')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '४', '4')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '५', '5')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '६', '6')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '७', '7')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '८', '8')) %>% 
    dplyr::mutate_all(~stringr::str_replace_all(., '९', '9')) 
}
