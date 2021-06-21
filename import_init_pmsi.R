







library(pmeasyr)

# cidd2 <- DBI::dbConnect(odbc::odbc(), 
#                time_out = 10)

cidd2 <- DBI::dbConnect(odbc::odbc(), 
                        time_out = 10)

library(dplyr)
a <- tibble(A = seq(Sys.Date()+1, Sys.Date() + 1e6, by = 1))
DBI::dbWriteTable(cidd2, 'TEST_DATES', a, overwrite = TRUE)
DBI::dbWriteTable(cidd2, 'TEST_DATES', a %>% mutate(A = as.character(A)), append = TRUE)

library(dbplyr)
tbl(cidd2, in_schema('PMSI', 'TEST_DATES')) %>% 
  count(y = year(A), m = month(A)) %>% 
  arrange(y, m)

# drop des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_drops.sql'))

# delete des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_delete.sql'))

# création des tables
DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_pmsi_refs_v2.sql'))

library(dplyr)
library(dbplyr)


p <- noyau_pmeasyr(
  finess = '290000017',
  annee  = 2019L,
  mois   = 12L,
  path = '~/Documents/data/mco',
  progress = FALSE,
  n_max = Inf
)


formats_pmsi <- pmeasyr::formats %>% 
  select(champ, table, nom, libelle, everything())

DBI::dbWriteTable(cidd2, 'DOC_DICO_PMSI', stringfix::toupper_names(formats_pmsi) %>% 
                    filter(CHAMP == 'mco', AN > '11') %>% select(-CLA, -TYPER, -Z, - RG, -CURSEUR, -FIN) %>% mutate_all(iconv, from = "utf-8", to = "iso-8859-1"), overwrite = TRUE)

DBI::dbWriteTable(cidd2, 'DOC_VALO_LIB_TYPE_FIN', stringfix::toupper_names(pmeasyr::vvr_libelles_valo('lib_type_sej') %>% 
                                                                             mutate(type_fin = as.character(type_fin))), overwrite = TRUE)
DBI::dbWriteTable(cidd2, 'DOC_VALO_LIB_VALO', stringfix::toupper_names(pmeasyr::vvr_libelles_valo('lib_valo')), overwrite = TRUE)
DBI::dbWriteTable(cidd2, 'DOC_VALO_LIB_TYPE_VIDHOSP', stringfix::toupper_names(pmeasyr::vvr_libelles_valo('lib_vidhosp')), overwrite = TRUE)


params <- list(annee = 2012:2019, mois = c(rep(12, 8)))

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

# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_RSA")
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_ACTES")
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_UM")
# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_DIAGS")

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

adezip(p, annee = 2019, mois = 12, type = "in")


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


i <- 2020
j <- 10
params <- list(annee = 2020, mois = 11) 
med_compile <- purrr::map2(params$annee, params$mois,
                           function(i, j){
                             temp <- imed_mco(p, annee = i, mois = j)
                             temp <- temp %>% mutate(ANSOR = as.character(i))
                             temp <- temp %>% inner_tra(itra(p, annee = i, mois = j)) %>% select(-NOHOP)
                             
                             temp
                           }
)

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_MED", bind_rows(med_compile), append = TRUE)
tictoc::toc()

i <- 2020
j <- 11
med_in_compile <- purrr::map2(params$annee, params$mois,
                           function(i, j){
                             temp <- imed_mco(p, annee = i, mois = j, typmed = "in") %>% mutate(ANSOR = as.character(i)) %>%
                             mutate(DTDISP = as.character(DTDISP))
                             
                             temp
                           }
)

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_MED", bind_rows(med_in_compile), append = TRUE)
tictoc::toc()

i <- 2019
j <- 12
dmi_in_compile <- purrr::map2(params$annee, params$mois,
                              function(i, j){
                                temp <- idmi_mco(p, annee = i, mois = j, typdmi = "in") %>% mutate(ANSOR = as.character(i)) %>%
                                  mutate(DTPOSE = as.character(DTPOSE))
                                
                                temp
                              }
)

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_DMI", bind_rows(dmi_in_compile), append = TRUE)
tictoc::toc()

dmi_out_compile <- purrr::map2(params$annee, params$mois,
                              function(i, j){
                                temp <- idmi_mco(p, annee = i, mois = j)
                                
                                temp <- temp %>% mutate(ANSOR = as.character(i))
                                temp <- temp %>% inner_tra(itra(p, annee = i, mois = j)) %>% select(-NOHOP)
                                
                                temp
                              }
)
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_DMI", bind_rows(dmi_in_compile), append = TRUE)
tictoc::toc()


library(nomensland)
ucd_indications <- get_table('mco_medref_atih_indications') %>% 
  mutate(ucd13 = stringr::str_remove_all(ucd13, "\\'| ")) %>% 
  mutate(datedebut = ifelse(grepl('\\/', datedebut), datedebut, '')) %>% 
    bind_rows(ucd_indications %>% filter(periode == "202003") %>% mutate(periode = "202004"),
              ucd_indications %>% filter(periode == "202003") %>% mutate(periode = "202005"),
              ucd_indications %>% filter(periode == "202003") %>% mutate(periode = "202006"))


# %>%
#   filter(periode == max(periode))

library(lubridate)
library(nomensland)

atu_indications <- get_table('mco_aturef_atih_indications') %>%
  #filter(periode == max(periode)) %>%
  tidyr::replace_na(list(datedefin = '01/01/3000', datededebut = '01/01/2000')) %>%
  group_by(ucd7, ucd13, codeindication, periode) %>%
  mutate(DTDEB_INDIC = as.character(min(dmy(datededebut))),
         DTFIN_INDIC = as.character(max(dmy(datedefin)))) %>%
  ungroup() %>% 
  mutate(codeindication = substr(codeindication,1,7))

# atu_indications <- atu_indications %>%
#   bind_rows(atu_indications %>% filter(periode == "202002") %>% mutate(periode = "202001"),
#             atu_indications %>% filter(periode == "202003") %>% mutate(periode = "202004"),
#             atu_indications %>% filter(periode == "202003") %>% mutate(periode = "202005"),
#             atu_indications %>% filter(periode == "202003") %>% mutate(periode = "202006"))

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_MED_ATIH_INDICATIONS")
tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_MED_ATIH_INDICATIONS", stringfix::toupper_names(ucd_indications) %>% mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_ATU_ATIH_INDICATIONS")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_ATU_ATIH_INDICATIONS", stringfix::toupper_names(atu_indications) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_ATU_ATIH_INDICATIONS   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_MED_ATIH_INDICATIONS   COMPUTE STATISTICS")


ucd_historique <- get_table('mco_medref_atih_historique_liste_ucd')


liste_table <- c('ccam_actes', 
                 'ccam_actes_avec_descri', 
                 'ccam_icr',
                 'ccam_hierarchie_actes',
                 'cim_hierarchie_code', 
                 'tarifs_mco_ghs_9999', 
                 'tarifs_mco_ghs')



DBI::dbWriteTable(cidd2,  "REF_CCAM_ICR", stringfix::toupper_names(get_table('ccam_icr')), append = TRUE)



get_table('ghm_ghm_regroupement')

# tab <- 'ccam_actes'

# tab <- 'tarifs_mco_ghs'
# tab <- 'ghm_ghm_regroupement'
# tab <- 'ghm_rghm_regroupement'
# tab <- 'ccam_actes_avec_descri'

refs <- function(tab){ 

  temp <- get_table(tab) %>% stringfix::toupper_names() %>% 
    mutate_at(vars(starts_with('DATE')), as.character) %>% 
    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1")
  name <- paste0('REF_', toupper(tab))
  
  DBI::dbWriteTable(cidd2, name, temp, append = TRUE)
  # DBI::dbWriteTable(cidd2, name, temp, overwrite = TRUE)
}


ghm_ghm_regroupement <- get_table('ghm_ghm_regroupement')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_GHM_GHM_REGROUPEMENT")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_GHM_GHM_REGROUPEMENT", stringfix::toupper_names(ghm_ghm_regroupement) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

ghm_rghm_regroupement <- get_table('ghm_rghm_regroupement')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_GHM_RGHM_REGROUPEMENT")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_GHM_RGHM_REGROUPEMENT", stringfix::toupper_names(ghm_rghm_regroupement) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

ccam_actes <- get_table('ccam_actes')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CCAM_ACTES")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_CCAM_ACTES", stringfix::toupper_names(ccam_actes) %>% 
                    # mutate_at(vars(starts_with('DATE')), as.character) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

ccam_actes_avec_descri <- get_table('ccam_actes_avec_descri')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CCAM_ACTES_AVEC_DESCRI")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_CCAM_ACTES_AVEC_DESCRI", stringfix::toupper_names(ccam_actes_avec_descri) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

ccam_hierarchie_actes <- get_table('ccam_hierarchie_actes')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CCAM_HIERARCHIE_ACTES")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_CCAM_HIERARCHIE_ACTES", stringfix::toupper_names(ccam_hierarchie_actes) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

ccam_icr <- get_table('ccam_icr')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CCAM_ICR")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_CCAM_ICR", stringfix::toupper_names(ccam_icr) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

cim_hierarchie_code <- get_table('cim_hierarchie_code')

DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CIM_HIERARCHIE_CODE")

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_CIM_HIERARCHIE_CODE", stringfix::toupper_names(cim_hierarchie_code) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()


DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CIM_HIERARCHIE_CODE")
library(nomensland)
ghs_tarifs_mco <- get_table('tarifs_mco_ghs')


DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_TARIFS_MCO_GHS")
library(dplyr)
tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_TARIFS_MCO_GHS", stringfix::toupper_names(ghs_tarifs_mco) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()

# DBI::dbSendQuery(cidd2, "TRUNCATE TABLE REF_CIM_HIERARCHIE_CODE")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_CIM_HIERARCHIE_CODE   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_CCAM_ICR   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_CCAM_HIERARCHIE_ACTES   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_CCAM_ACTES_AVEC_DESCRI   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_CCAM_ACTES   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_GHM_RGHM_REGROUPEMENT   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.REF_GHM_GHM_REGROUPEMENT   COMPUTE STATISTICS")


library(readr)
u <- readr::read_csv2('Z:\\3_Outils\\CIDD2\\csv\\GHS_2021_monorum_uhcd.csv', locale = readr::locale(encoding = 'latin1'),
                      col_types =  cols(
                        GHS = col_character(),
                        GHM = col_character(),
                        LIBELLE = col_character()
                      )) %>% 
  mutate(ANSEQTA = '2021')

tictoc::tic()
DBI::dbWriteTable(cidd2,  "REF_GHS_MONORUM_UHCD", stringfix::toupper_names(u) %>% 
                    mutate_if(is.character, iconv, from = "utf-8", to = "iso-8859-1"), append = TRUE)
tictoc::toc()


DBI::dbDisconnect(cidd2)

