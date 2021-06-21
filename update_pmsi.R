







library(pmeasyr)
library(dplyr)
library(dbplyr)

# cidd2 <- DBI::dbConnect(odbc::odbc(),
#                time_out = 10)

cidd2 <- DBI::dbConnect(odbc::odbc(), 
                        time_out = 10)

# DBI::dbDisconnect(cidd2)


# from_pmsi <- tbl(cidd2, in_schema('PMSI', 'NM_CCAM')) %>% 
#   collect() %>% 
#   mutate_if(is.character, iconv) %>% 
#   select(CODE, ACTIV, PHASE, DATE_DEBUT, DATE_FIN, PU_BASE)
# 
# library(nomensland)
# get_table('ccam_tarifs')
# drop des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_drops.sql'))

# delete des tables sur la période appropriée (2021)
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_RSA        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_ACTES      where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_DIAGS      where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_UM         where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_ANO        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_VALO       where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_RUM        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_ACTES      where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_DIAGS      where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_VALO       where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_MED        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RUM_DMI        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_MED        where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_RSA_DMI        where ANSOR = '2021'")

DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_CORA_IPP       where ANSOR = '2021'")
DBI::dbExecute(cidd2, "DELETE FROM PMSI.MCO_CORA_UF_PMSI   where ANSOR = '2021'")




# Cora FISH 2021

update_ipp_year <- "
select to_char(b.ID_RSS) as NORSS, to_char(b.DATE_FIN, 'yyyy') as ANSOR, 
b.ID_SEJOUR, b.ID_PATIENT, to_char(b.DATE_DEBUT, 'dd/mm/yyyy') as DATE_DEBUT, to_char(b.DATE_FIN, 'dd/mm/yyyy') as DATE_FIN, c.IPP_PATIENT, 
c.NOM_USUEL_PATIENT, c.PRENOM_PATIENT, to_char(c.DATE_NAIS, 'dd/mm/yyyy') as DATE_NAIS, to_char(c.DATE_DECES, 'dd/mm/yyyy') as DATE_DECES from
CORA_REC.TB_SYNTH_RSS b  
INNER JOIN 
CORA_REC.TB_PATIENT c on b.ID_PATIENT = c.ID_PATIENT and extract(year from b.DATE_FIN) = 2021
"

source('Z:/3_Outils/scripts_statisticiens/tirage_rss_a_coder_2020/tools/R_for_cora_cid/R/function_connect_oracle.R')
cora <- oracle_brest(which = "CORA", mdp = "atih")
df_ipp_year <- DBI::dbGetQuery(cora, update_ipp_year) %>% as_tibble()

DBI::dbWriteTable(cidd2,  "MCO_CORA_IPP", df_ipp_year, append = TRUE)

DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_CORA_IPP   COMPUTE STATISTICS")


update_uf_year <- "
select to_char(b.ID_RSS) as NORSS, to_char(b.DATE_FIN, 'yyyy') as ANSOR,  b.ID_RSS, b.ID_RSS||LPAD(c.CHRONO,2, '0') as NORUM, 
       c.DUREE as DUREE_RUM, to_char(c.DATE_DEBUT_RUM, 'dd/mm/yyyy') as DATE_DEBUT_RUM, to_char(c.DATE_FIN_RUM, 'dd/mm/yyyy') as DATE_FIN_RUM, 
       round(e.DATE_FIN_MVT - e.DATE_DEBUT_MVT)  as DUREE_MVT, to_char(e.DATE_DEBUT_MVT, 'dd/mm/yyyy') as DATE_DEBUT_MVT,to_char( e.DATE_FIN_MVT, 'dd/mm/yyyy') as DATE_FIN_MVT, 
       c.ID_RUM, c.CHRONO, 
       d.ID_MVT, e.ID_UNITE_M as UF_PMSI,
       c.ID_UM_HEBERG, c.ID_UNITE_HEBERG, c.ID_UM_RESP_MED, c.ID_UNITE_RESP_MED, f.CODE_UNITE_MED as CDURM from
CORA_REC.TB_SYNTH_RSS b INNER JOIN
CORA_REC.TB_SYNTH_RUM c on b.ID_RSS = c.ID_RSS  and extract(year from b.DATE_FIN) = 2021 inner join
CORA_REC.TB_MVT_RUM d on c.ID_RUM = d.ID_RUM inner join
CORA_REC.TB_MVT e on d.ID_MVT = e.ID_MVT left join
CORA_REC.TB_UNITE_MEDICALE f on c.ID_UM_RESP_MED = f.ID_UNITE_MED
"

df_uf_year <- DBI::dbGetQuery(cora, update_uf_year) %>% as_tibble()

DBI::dbWriteTable(cidd2,  "MCO_CORA_UF_PMSI", df_uf_year, append = TRUE)
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_CORA_UF_PMSI   COMPUTE STATISTICS")


# création des tables
# DBI::dbSendStatement(cidd2, readr::read_file('Z:/3_Outils/CIDD2/save_schema/schema_cidd2_pmsi_refs_v2.sql'))




p <- noyau_pmeasyr(
  finess = '290000017',
  annee  = 2021L,
  mois   = 4L,
  path = '~/Documents/data/mco',
  progress = FALSE,
  n_max = Inf
)

#params <- list(annee = 2016:2020, mois = c(rep(12, 4), 5))

# params <- list(annee = 2012:2013, mois = c(rep(12, 2)))

# params <- list(annee = 2015, mois = 12)

params <- list(annee = p$annee, mois = p$mois)
adezip(p, type = "out")
adezip(p, type = "in")

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


# DBI::dbExecute(cidd2, "DELETE FROM MCO_RSA_ANO WHERE ANSOR = '2020'")
tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_ANO", ano, append = TRUE)
tictoc::toc()

# DBI::dbSendStatement(cidd2, "DELETE FROM MCO_RSA_RSA WHERE ANSEQTA = '2018'")

# DBI::dbWriteTable(cidd2,  "MCO_RSA_RSA", rsa %>% head(10), append = TRUE)

# adezip(p, annee = 2020, mois = 11, type = "in")

# rum <- irum(p, typi = 3)
# View(rum$rum)

# p$n_max <- Inf

rum_compile <- purrr::map2(rev(params$annee), rev(params$mois),
                           function(i, j){
                             if (i > 2015){
                               
                               temp <- irum(p, typi = 3, annee = i, mois = j) %>% tdiag()
                               temp <- temp %>% purrr::map(function(x){x %>% mutate(ANSOR = as.character(i))})
                               temp <- temp %>% purrr::map(function(x){x %>% mutate(NORSS = substr(NORUM, 1, 7))})
                               
                               temp %>% purrr::map(function(x)mutate_at(x, vars(starts_with('DT'), starts_with('D8')), as.character))
                             } else {
                               
                               temp <- irum(p, typi = 3, annee = i, mois = j) %>% tdiag()
                               temp_num <- temp$rum %>% 
                                 rename(NORUM_cora = NORUM) %>% 
                                 group_by(NORSS) %>% 
                                 mutate(NORUM= row_number(NORUM_cora)) %>% 
                                 ungroup() %>% 
                                 distinct(NORSS, NORUM, NORUM_cora) %>% 
                                 mutate(NORUM = paste0(NORSS, stringr::str_pad(NORUM, 2, "left", "0"))) %>% 
                                 select(NORUM, NORUM_cora)
                               
                               temp <- temp %>% purrr::map(function(x)rename(x, NORUM_cora = NORUM))
                               temp <- temp %>% purrr::map(function(x){x %>% mutate(ANSOR = as.character(i))})
                               temp <- temp %>% purrr::map(function(x){x %>% inner_join(temp_num, by = c('NORUM_cora' = 'NORUM_cora')) %>% select(-NORUM_cora)})
                               
                               temp$actes <- temp$actes %>% mutate(DESCRI = substr(CDCCAM,9, 10), CDCCAM = substr(CDCCAM,1,7))
                               temp$actes <- temp$actes %>% mutate(NORSS  = substr(NORUM, 1, 7))
                               temp$diags <- temp$diags %>% mutate(NORSS  = substr(NORUM, 1, 7))
                               
                               temp %>% purrr::map(function(x)mutate_at(x, vars(starts_with('DT'), starts_with('D8')), as.character))
                               
                             }
                           }
)


rum_rum   <- rum_compile %>% purrr::map_df('rum')   %>% stringfix::toupper_names()
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
DBI::dbWriteTable(cidd2,  "MCO_RUM_DIAGS", rum_diags, append = TRUE) # attention en 2012 (RUM RDTH), il faudrait mettre 20 comme longueur de NAS dans Oracle, pas 10...
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

epmsi_mco_rav(rsa_valo_compile %>% bind_rows())
# epmsi_mco_rav(rsa_valo %>% filter(moissor < '03'))
# epmsi_mco_rav(rsa_valo %>% filter(moissor >= '03') %>% mutate_if(is.numeric, function(x)x*1.0019))

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RSA_VALO", rsa_valo, append = TRUE)
tictoc::toc()

# rsa_valo <- tolower_names(rsa_valo)
# epmsi_mco_rav(rsa_valo)

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

# rum_valo <- tolower_names(rum_valo)
# epmsi_mco_rav_rum(rum_valo)
# epmsi_mco_rav2(rsa_valo)

vvr_rum_check_rubriques_rav <- function (valo, valo_rum, ...) 
{
  epmsi_mco_rav2(valo, ...) %>% left_join(epmsi_mco_rav_rum(valo_rum, ...), 
                                          by = c("ordre_epmsi", "lib_valo", "var"), suffix = c("_rsa", 
                                                                                               "_rum")) %>% mutate_if(is.numeric, tidyr::replace_na, 
                                                                                                                      0) %>% mutate(delta = val_rum - val_rsa)
}

# rum_valo <- rum_valo %>% inner_join(rsa_valo %>% select(norss, type_fin))

# vvr_rum_check_rubriques_rav(rsa_valo, rum_valo, theorique = TRUE)

tictoc::tic()
DBI::dbWriteTable(cidd2,  "MCO_RUM_VALO", rum_valo, append = TRUE)
tictoc::toc()


p$tolower_names <- FALSE
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
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_DMI   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_MED   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_DMI   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_MED   COMPUTE STATISTICS")
 

# générer les tables avec les dates aux formats dates (ou lancer ce script dans toad / l'ouvrir dans le schéma puis ctrl+A puis cliquer sur l'éclair)

DBI::dbSendStatement(cidd2, "TRUNCATE TABLE PMSI.MCO_RSA_DATES") 

DBI::dbSendStatement(cidd2, "insert into PMSI.MCO_RSA_DATES
select a.ANSOR, a.NORSS, a.DTENT, a.DTSORT, to_date(a.DTENT, 'yyyy-mm-dd' ), to_date( a.DTSORT, 'yyyy-mm-dd' )
from PMSI.MCO_RSA_ANO a")

DBI::dbSendStatement(cidd2, "truncate TABLE PMSI.MCO_RUM_DATES")

DBI::dbSendStatement(cidd2, "INSERT INTO PMSI.MCO_RUM_DATES 
select a.ANSOR, a.NORUM, a.D8EEUE, a.D8SOUE, to_date(a.D8EEUE, 'yyyy-mm-dd' ), to_date(a.D8SOUE, 'yyyy-mm-dd' )
from PMSI.MCO_RUM_RUM a")

DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RSA_DATES   COMPUTE STATISTICS")
DBI::dbSendStatement(cidd2, "ANALYZE TABLE PMSI.MCO_RUM_DATES   COMPUTE STATISTICS")


DBI::dbDisconnect(cidd2)










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







library(nomensland)


liste_table <- c('ccam_actes', 
                 'ccam_actes_avec_descri', 
                 'ccam_hierarchie_actes',
                 'cim_hierarchie_code', 
                 'tarifs_mco_ghs_9999', 
                 'tarifs_mco_ghs')

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







