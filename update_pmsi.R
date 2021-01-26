
library(pmeasyr)
library(dplyr)
library(dbplyr)

cidd2 <- DBI::dbConnect(odbc::odbc(), 
                        # {...},
                        time_out = 10)


# drop des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_drops.sql'))

# delete des tables sur la période appropriée (2020)
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_RSA   where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_ACTES where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_DIAGS where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_UM    where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_ANO   where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RSA_VALO  where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RUM_RUM   where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RUM_ACTES where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RUM_DIAGS where ANSOR = '2020'")
DBI::dbGetQuery(cidd2, "DELETE FROM PMSI.MCO_RUM_VALO  where ANSOR = '2020'")


# création des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_pmsi_refs_v2.sql'))



p <- noyau_pmeasyr(
  finess = '290000017',
  annee  = 2020L,
  mois   = 5L,
  path = '~/Documents/data/mco',
  progress = FALSE,
  n_max = Inf
)

#params <- list(annee = 2016:2020, mois = c(rep(12, 4), 5))

params <- list(annee = 2020, mois = 5)

rsa_compile <- purrr::map2(params$annee, params$mois,
  function(i, j){
    temp <- irsa(p, typi = 4, annee = i, mois = j) %>% tdiag()
    temp <- temp %>% purrr::map(function(x){x %>% mutate(ANSOR = as.character(i))})
    temp <- temp %>% purrr::map(function(x){inner_tra(x, itra(p, annee = i, mois = j)) %>% select(-NOHOP)})
    
    temp
    }
)

rsa_rsa <- rsa_compile %>% purrr::map_df('rsa')     %>% stringfix::toupper_names()
rsa_diags <- rsa_compile %>% purrr::map_df('diags') %>% stringfix::toupper_names()
rsa_actes <- rsa_compile %>% purrr::map_df('actes') %>% stringfix::toupper_names()
rsa_um <- rsa_compile %>% purrr::map_df('rsa_um') %>% stringfix::toupper_names()

rm(rsa_compile)
gc()

library(DBI)
library(dplyr)
# DBI::dbWriteTable(cidd2,  "iris", iris)
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_RSA", rsa_rsa, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_ACTES", rsa_actes, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_DIAGS", rsa_diags, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_UM", rsa_um, append = TRUE)
tictoc::toc()

ano_compile <- purrr::map2(params$annee, params$mois,
                           function(i, j){
                             temp <- iano_mco(p, annee = i, mois = j)
                             temp <- temp %>% mutate(ANSOR = as.character(i))
                             temp <- temp %>% inner_tra(itra(p, annee = i, mois = j)) %>% select(-NOHOP) %>% 
                               mutate_at(vars(starts_with('DT')), as.character) %>% 
                               select(-starts_with('ZONECHIFF'))
                             
                             temp
                           }
)

# rm(ano)
gc()
ano <- ano_compile %>% bind_rows() %>% stringfix::toupper_names()
rm(ano_compile)


# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_ANO")
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_ANO", ano, append = TRUE)
tictoc::toc()

# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_RSA WHERE ANSEQTA = '2018'")

# DBI::dbWriteTable(cidd2,  "MCO_RSA_RSA", rsa %>% head(10), append = TRUE)

# adezip(p, annee = 2020, mois = 5, type = "in")


# p$n_max <- Inf
rum_compile <- purrr::map2(rev(params$annee), rev(params$mois),
                           function(i, j){
                             temp <- irum(p, typi = 3, annee = i, mois = j) %>% tdiag()
                             temp <- temp %>% purrr::map(function(x){x %>% mutate(ANSOR = as.character(i))})
                             temp <- temp %>% purrr::map(function(x){x %>% mutate(NORSS = substr(NORUM, 1, 7))})
                             
                             temp %>% purrr::map(function(x)mutate_at(x, vars(starts_with('DT'), starts_with('D8')), as.character))
                           }
)

rum_rum <- rum_compile %>% purrr::map_df('rum')     %>% stringfix::toupper_names()
rum_diags <- rum_compile %>% purrr::map_df('diags') %>% stringfix::toupper_names()
rum_actes <- rum_compile %>% purrr::map_df('actes') %>% stringfix::toupper_names()
# rsa_um <- rsa_compile %>% purrr::map_df('rsa_um') %>% stringfix::toupper_names()

library(DBI)
library(dplyr)
# DBI::dbWriteTable(cidd2,  "iris", iris)
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RUM_RUM")
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RUM_ACTES")
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RUM_DIAGS")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_RUM", rum_rum, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_ACTES", rum_actes, append = TRUE)
tictoc::toc()
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_DIAGS", rum_diags, append = TRUE)
tictoc::toc()
DBI::dbCommit(cidd2)

#params <- list(annee = 2015:2020, mois = c(rep(12, 5), 4))
p$tolower_names <- TRUE
rsa_valo_compile <- purrr::map2(rev(params$annee), rev(params$mois),
                                function(i, j){
                                  p$annee <- i
                                  p$mois <- j
                                  vrsa <- vvr_rsa(p)
                                  vano <- vvr_ano_mco(p)
                                  library(nomensland)
                                  tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)
                                  
                                  tarifs_supp <- get_table('tarifs_mco_supplements') %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% 
                                    select(-cgeo)
                                  
                                  resu <- vvr_mco(
                                    vvr_ghs_supp(rsa = vrsa, 
                                                 tarifs = tarifs_ghs, 
                                                 supplements =  tarifs_supp, 
                                                 ano = vano, 
                                                 porg = ipo(p, annee = i, mois = j), 
                                                 diap = idiap(p, annee = i, mois = j), 
                                                 pie = ipie(p, annee = i, mois = j), 
                                                 mo = imed_mco(p, annee = i, mois = j), 
                                                 full = FALSE,
                                                 cgeo = 1L, 
                                                 prudent = NULL,
                                                 bee = FALSE),
                                    vvr_mco_sv(vrsa, vano, ipo(p, annee = i, mois = j))
                                  ) %>% inner_tra(itra(p, annee = i, mois = j)) %>% 
                                    mutate(ansor = as.character(i))
                                  resu %>% select(-nohop)
                                }
)


rsa_valo <- rsa_valo_compile %>% bind_rows %>% stringfix::toupper_names()

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_VALO", rsa_valo, append = TRUE)
tictoc::toc()

# params <- list(annee = 2016:2020, mois = c(rep(12, 4), 4))
# p$tolower_names <- TRUE

rum_valo_compile <- purrr::map2(rev(params$annee), rev(params$mois),
                                function(i, j){
                                  source('Z:/3_Outils/cellule_statistique/demandes/cardio_src/vvr_rum.R')
                                  p$annee <- i
                                  p$mois <- j
                                  vrsa <- vvr_rsa(p) 
                                  vano <- vvr_ano_mco(p)
                                  library(nomensland)
                                  tarifs_ghs <- dplyr::distinct(get_table('tarifs_mco_ghs'), ghs, anseqta, .keep_all = TRUE)
                                  
                                  tarifs_supp <- get_table('tarifs_mco_supplements') %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% 
                                    select(-cgeo)
                                  
                                  valo <- vvr_mco(
                                    vvr_ghs_supp(rsa = vrsa, 
                                                 tarifs = tarifs_ghs, 
                                                 supplements =  tarifs_supp, 
                                                 ano = vano, 
                                                 porg = ipo(p, annee = i, mois = j), 
                                                 diap = idiap(p, annee = i, mois = j), 
                                                 pie = ipie(p, annee = i, mois = j), 
                                                 mo = imed_mco(p, annee = i, mois = j), 
                                                 full = FALSE,
                                                 cgeo = 1L, 
                                                 prudent = NULL,
                                                 bee = FALSE),
                                    vvr_mco_sv(vrsa, vano, ipo(p, annee = i, mois = j))
                                  ) %>% 
                                    left_join(irsa(p)$rsa %>% select(cle_rsa, agean, duree, ghm, nbrum))
                                  
                                  valo_rum <- vvr_rum(p, 
                                                      valo, 
                                                      repartition_multi = '{prop_pmct_um}*0+{prop_pass}*1',
                                                      seuil_pmct = 20,
                                                      type_passage = "RUM", 
                                                      pmct_mono = FALSE)%>% inner_tra(itra(p, annee = i, mois = j)) %>% 
                                    mutate(ansor = as.character(i)) %>% 
                                    select(-nohop)
                                }
)

rum_valo <- rum_valo_compile %>% bind_rows %>% stringfix::toupper_names()

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_VALO", rum_valo, append = TRUE)
tictoc::toc()

# https://oracle-base.com/dba/script?category=miscellaneous&file=analyze_all.sql
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_RSA   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_ACTES COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_DIAGS COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_UM    COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_ANO   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_VALO  COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_RUM   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_ACTES COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_DIAGS COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_VALO  COMPUTE STATISTICS")

l_tables <- DBI::dbListTables(cidd2, schema = "PMSI") %>% .[grepl('^MCO_', .)]
i <- l_tables[1]

l <- l_tables %>% 
  purrr::map(function(i){
    cat('## ', i, '\n')
    r <- collect(arrange(count(tbl(cidd2, in_schema('PMSI', i)), ANSOR), desc(ANSOR))) %>% 
      mutate(m = max(n),
             p = round(n / m,2))
    print(r)
    cat('\n')
    
    return(setNames(list(r), i))
  })

