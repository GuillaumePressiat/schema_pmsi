
library(pmeasyr)

# cidd2 <- DBI::dbConnect(odbc::odbc(), 
#                time_out = 10)

cidd2 <- DBI::dbConnect(odbc::odbc(), 
                        time_out = 10)

library(pmeasyr)
library(dplyr)
library(dbplyr)


p <- noyau_pmeasyr(
  finess = '290000017',
  annee  = 2019L,
  mois   = 12L,
  path = '~/Documents/data/ssr',
  progress = FALSE,
  n_max = Inf
)



params <- list(annee = 2012:2021, mois = c(rep(12, 9), 3))

rha_compile <- purrr::map2(params$annee, params$mois,
                           function(i, j){
                             # adezip(p, type = "out", annee = i, mois = j)
                             temp <- irha(p, annee = i, mois = j) %>% tdiag()
                             temp <- temp %>% purrr::map(function(x){x %>% mutate(ANSOR = as.character(i))})
                             temp <- temp %>% purrr::map(function(x){inner_tra(x, itra(p, annee = i, mois = j, champ = "ssr"), champ = "ssr")})
                             
                             if (i > 2016){
                             temp$rha <- temp$rha %>%
                               rename(JOURNEES_HWE = JOURNESS_HWE)
                               }
                             
                             temp$rha <- temp$rha %>% mutate(ANSMOISRHS = paste0(substr(MOISANSRHS,3,6), substr(MOISANSRHS,1,2))) %>% 
                               mutate(nbjpres = stringr::str_count(paste0(JOURNEES_HWE, JOURNEES_WE), '1'))
                             
                             temp$acdi <- temp$acdi %>% mutate(ANSMOISRHS = paste0(substr(NOSEMAINE,3,6), substr(NOSEMAINE,1,2)))
                             
                             temp$diags <- temp$diags %>% mutate(ANSMOISRHS = paste0(substr(NOSEMAINE,3,6), substr(NOSEMAINE,1,2)))
                             
                             if ('NBPATREEL' %in% names(temp$acdi)){
                             if (is.character(temp$acdi$NBPATREEL)){
                               temp$acdi$NBPATREEL <- as.integer(temp$acdi$NBPATREEL)
                             }
                             }
                             temp
                           }
)

rha_rha <-   rha_compile %>% purrr::map_df('rha')     %>% stringfix::toupper_names()
rha_diags <- rha_compile %>% purrr::map_df('diags') %>% stringfix::toupper_names()
rha_actes <-  rha_compile %>% purrr::map_df('acdi') %>% stringfix::toupper_names() %>%
  filter(CODE != 'DA') %>%
  select(-DA)


library(DBI)
library(dplyr)
# DBI::dbWriteTable(cidd2,  "iris", iris)
tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHA_RHA", rha_rha, append = FALSE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHA_ACTES", rha_actes, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHA_DIAGS", rha_diags, append = TRUE)
tictoc::toc()


rhs_compile <- purrr::map2(params$annee, params$mois,
                           function(i, j){
                             # adezip(p, type = "in", annee = i, mois = j)
                             temp <- irhs(p, annee = i, mois = j) 
                             temp <- temp %>% purrr::map(function(x){x %>% 
                                 mutate(ANSOR = as.character(i))  %>%
                                 mutate_at(vars(starts_with('DT')), as.character) %>% 
                                 mutate_at(vars(starts_with('D8')), as.character)})


                             temp$rhs <- temp$rhs %>% mutate(ANSMOISRHS = paste0(substr(NOSEM,3,6), substr(NOSEM,1,2))) %>% 
                               mutate(nbjpres = stringr::str_count(paste0(JOURNEES_HWE, JOURNEES_WE), '1'))
                             
                             temp$acdi <- temp$acdi %>% mutate(ANSMOISRHS = paste0(substr(NOSEM,3,6), substr(NOSEM,1,2)))
                             
                             # if ('NBPATREEL' %in% names(temp$acdi)){
                             #   if (is.character(temp$acdi$NBPATREEL)){
                             #     temp$acdi$NBPATREEL <- as.integer(temp$acdi$NBPATREEL)
                             #   }
                             # }

                             # temp$rhs$DTSORT <- format(temp$rhs$DTSORT, '%d/%m/%Y')
                             # temp$rhs$DTENT <- format(temp$rhs$DTENT,   '%d/%m/%Y')
                             # temp$rhs$D8EEUE <- format(temp$rhs$D8EEUE, '%d/%m/%Y')
                             # temp$rhs$D8SOUE <- format(temp$rhs$D8SOUE, '%d/%m/%Y')
                             # temp$rhs$DTNAIS <- format(temp$rhs$DTNAIS, '%d/%m/%Y')
                             # 
                             # temp$rhs$D8SOUE <- ifelse(is.na(temp$rhs$D8SOUE), '', temp$rhs$D8SOUE)
                             temp$acdi$DATE_ACTE <- as.character(temp$acdi$DATE_ACTE)
                             # 
                             
                             temp
                           }
)

rhs_rhs <-   rhs_compile %>% purrr::map_df('rhs')     %>% stringfix::toupper_names()
# rha_diags <- rha_compile %>% purrr::map_df('diags') %>% stringfix::toupper_names()
rhs_actes <-  rhs_compile %>% purrr::map_df('acdi') %>% stringfix::toupper_names() %>%
  filter(CODE != 'DA') %>%
  select(-DA)

rhs_da <-  rhs_compile %>% purrr::map_df('acdi') %>% stringfix::toupper_names() %>%
  filter(CODE == 'DA') %>%
  select(NAS, NOSEJ, NOSEM, DA, ANSOR, ANSMOISRHS)

tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHS_RHS", rhs_rhs, overwrite = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHS_ACTES", rhs_actes, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "SSR_RHS_DA", rhs_da, append = TRUE)
tictoc::toc()


# DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CIM_HIERARCHIE_CODE")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHS_RHS   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHA_RHA   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHS_DA   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHS_ACTES   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHA_ACTES   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.SSR_RHA_DIAGS   COMPUTE STATISTICS")


DBI::dbDisconnect(cidd2)

