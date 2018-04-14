# ----- Preample ----------------------------------------------------------


# ----- Data Dictionary ---------------------------------------------------

td <- '~/Desktop/opensecrets/'

pag <- 'https://www.opensecrets.org/resources/datadictionary/'
dic_links <- list(
  pacs = 'Data%20Dictionary%20for%20PAC%20to%20Cands%20Data.htm',
  cands = 'Data%20Dictionary%20Candidates%20Data.htm',
  cmtes = 'Data%20Dictionary%20for%20Cmtes.htm',
  indivs = 'Data%20Dictionary%20for%20Individual%20Contribution%20Data.htm',
  pacs_other = 'Data%20Dictionary%20PAC%20to%20PAC%20Data.htm'
)
dic_fun <- function(pag) { html_table(read_html(pag), header = T)[[1]] }
dic <- map(dic_links, ~str_c(pag, .x))
dic %<>% map(dic_fun)

dic$cands %<>%
  mutate(
    Type = str_extract(`Type (Length)`, '[^()]') %>% str_trim(),
    Length = str_extract(`Type (Length)`, '\\d+')
  ) %>% 
  select(-`Type (Length)`)

dic %<>% do.call(rbind, .)
dic$Table <- str_split(row.names(dic), '\\.', simplify = T)[, 1]
