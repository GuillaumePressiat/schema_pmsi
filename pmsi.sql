--
-- Create Schema Script 
--   Database Version   : 11.2.0.2.0 
--   TOAD Version       : 9.6.0.27 
--   DB Connect String  : vs-oracle-cid:1521/CIDD2 
--   Schema             : PMSI 
--   Script Created by  : SYSTEM 
--   Script Created at  : 16/07/2020 15:10:44 
--   Physical Location  :  
--   Notes              :  
--

-- Object Counts: 
--   Tables: 21         Columns: 544        


CREATE TABLE MCO_RSA_ACTES
(
  CLE_RSA    VARCHAR2(10 BYTE),
  NSEQRUM    VARCHAR2(10 BYTE),
  DELAI      INTEGER,
  CDCCAM     VARCHAR2(7 BYTE),
  PHASE      VARCHAR2(1 BYTE),
  ACT        VARCHAR2(1 BYTE),
  EXTDOC     VARCHAR2(20 BYTE),
  MODIF      VARCHAR2(20 BYTE),
  RMBTEXCEP  VARCHAR2(20 BYTE),
  ASSONP     VARCHAR2(20 BYTE),
  NBEXEC     INTEGER,
  INDVAL     VARCHAR2(20 BYTE),
  ANSOR      VARCHAR2(4 BYTE),
  NORSS      VARCHAR2(20 BYTE),
  NAS        VARCHAR2(20 BYTE),
  DESCRI     VARCHAR2(2 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RSA_RSA
(
  NOFINESS                    VARCHAR2(9 BYTE),
  NOVRSA                      VARCHAR2(3 BYTE),
  CLE_RSA                     VARCHAR2(10 BYTE),
  NOVRSS                      VARCHAR2(3 BYTE),
  NOSEQTA                     VARCHAR2(3 BYTE),
  GPVCLASS                    VARCHAR2(2 BYTE),
  GPCMD                       VARCHAR2(2 BYTE),
  GPTYPE                      VARCHAR2(1 BYTE),
  GPNUM                       VARCHAR2(2 BYTE),
  GPCOMPX                     VARCHAR2(1 BYTE),
  GPCDRETR                    VARCHAR2(3 BYTE),
  RSAVCLASS                   VARCHAR2(2 BYTE),
  RSACMD                      VARCHAR2(2 BYTE),
  RSATYPE                     VARCHAR2(1 BYTE),
  RSANUM                      VARCHAR2(2 BYTE),
  RSACOMPX                    VARCHAR2(1 BYTE),
  RSACDRETR                   VARCHAR2(3 BYTE),
  NBRUM                       INTEGER,
  AGEAN                       INTEGER,
  AGEJR                       INTEGER,
  SEXE                        VARCHAR2(1 BYTE),
  ECHPMSI                     VARCHAR2(1 BYTE),
  PROV                        VARCHAR2(1 BYTE),
  MOISSOR                     VARCHAR2(2 BYTE),
  ANSOR                       VARCHAR2(4 BYTE),
  SCHPMSI                     VARCHAR2(1 BYTE),
  DEST                        VARCHAR2(1 BYTE),
  TYPESEJ                     VARCHAR2(1 BYTE),
  DUREE                       INTEGER,
  CDGEO                       VARCHAR2(5 BYTE),
  POIDS                       INTEGER,
  AGEGEST                     INTEGER,
  DELAIREG                    INTEGER,
  NBSEANCE                    INTEGER,
  NOGHS                       VARCHAR2(4 BYTE),
  NBJRBS                      INTEGER,
  SEJINFBI                    VARCHAR2(1 BYTE),
  NBJREXB                     INTEGER,
  UHCD                        VARCHAR2(1 BYTE),
  CONFCDSEJ                   VARCHAR2(1 BYTE),
  NBAUTPGV                    INTEGER,
  NBSUPHS                     INTEGER,
  NBSUPAHS                    INTEGER,
  NBSUPCHS                    INTEGER,
  NBSUPEHS                    INTEGER,
  NBACTE9615                  INTEGER,
  NBSUPREAPED                 INTEGER,
  NBSUPATPART                 INTEGER,
  NB_RDTH                     INTEGER,
  VALVAORT                    VARCHAR2(1 BYTE),
  NBSUPCAISSON                INTEGER,
  TYPRESTPO                   VARCHAR2(1 BYTE),
  NBSUPREA                    INTEGER,
  NBSUPSI                     INTEGER,
  NBSUPSTF                    INTEGER,
  NBSUPSRC                    INTEGER,
  NBSUPNN1                    INTEGER,
  NBSUPNN2                    INTEGER,
  NBSUPNN3                    INTEGER,
  NBSUPREP                    INTEGER,
  PASLITSP                    VARCHAR2(1 BYTE),
  TYPMACHRADIO                VARCHAR2(1 BYTE),
  TYPEDOSIM                   VARCHAR2(1 BYTE),
  NUMINNO                     VARCHAR2(20 BYTE),
  NBFAISC                     INTEGER,
  NOSEQRUM                    VARCHAR2(10 BYTE),
  DP                          VARCHAR2(6 BYTE),
  DR                          VARCHAR2(6 BYTE),
  NDAS                        INTEGER,
  NA                          INTEGER,
  GHM                         VARCHAR2(255 BYTE),
  ANSEQTA                     VARCHAR2(255 BYTE),
  TYPGLOB                     VARCHAR2(255 BYTE),
  RDTH                        VARCHAR2(255 BYTE),
  NORSS                       VARCHAR2(255 BYTE),
  NAS                         VARCHAR2(255 BYTE),
  GHSMINORE                   VARCHAR2(255 BYTE),
  GENAUTORSA                  VARCHAR2(255 BYTE),
  NBIVG                       VARCHAR2(255 BYTE),
  ANIVGPREC                   VARCHAR2(255 BYTE),
  NBNAISSANCE                 VARCHAR2(255 BYTE),
  TOPADMNAIS                  VARCHAR2(255 BYTE),
  TOPRADAVASTIN               VARCHAR2(255 BYTE),
  TOPRADALIMTA                VARCHAR2(255 BYTE),
  GHSHORSINNO                 VARCHAR2(255 BYTE),
  SUPPDEFCARD                 VARCHAR2(255 BYTE),
  CONVERSION_HC               VARCHAR2(255 BYTE),
  PC_RAAC                     VARCHAR2(255 BYTE),
  ADMISSION_MAISON_NAISSANCE  VARCHAR2(255 BYTE),
  ELL_FORF_DIABETE            VARCHAR2(255 BYTE),
  CDPOSTAL                    VARCHAR2(255 BYTE),
  ELL_GRADATION               VARCHAR2(255 BYTE),
  SURVEILLANCE_PARTICULIERE   VARCHAR2(255 BYTE),
  RESERERVE_HOSP              VARCHAR2(255 BYTE),
  RESCRIT_TARIFAIRE           VARCHAR2(255 BYTE),
  CAT_NB_INTERVENANTS         VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RSA_UM
(
  CLE_RSA       VARCHAR2(10 BYTE),
  NSEQRUM       VARCHAR2(10 BYTE),
  DPUM          VARCHAR2(6 BYTE),
  DRUM          VARCHAR2(6 BYTE),
  IGS2          VARCHAR2(3 BYTE),
  NBDIAGAS      INTEGER,
  NBACTE        INTEGER,
  DUREESEJPART  INTEGER,
  NOHOP1        VARCHAR2(9 BYTE),
  TYPAUT1       VARCHAR2(4 BYTE),
  NATSUPP1      VARCHAR2(4 BYTE),
  NBSUPP1       INTEGER,
  NOHOP2        VARCHAR2(9 BYTE),
  TYPAUT2       VARCHAR2(4 BYTE),
  NATSUPP2      VARCHAR2(4 BYTE),
  NBSUPP2       VARCHAR2(4 BYTE),
  ANSOR         VARCHAR2(4 BYTE),
  NORSS         VARCHAR2(10 BYTE),
  NAS           VARCHAR2(20 BYTE),
  AGEGESTRUM    VARCHAR2(2 BYTE),
  NSEQUM        VARCHAR2(4 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RUM_RUM
(
  NOCLAS             VARCHAR2(2 BYTE),
  CDGHM              VARCHAR2(6 BYTE),
  NOVERG             VARCHAR2(3 BYTE),
  CDERG              VARCHAR2(3 BYTE),
  NOFINESS           VARCHAR2(9 BYTE),
  NOVERR             VARCHAR2(3 BYTE),
  NORSS              VARCHAR2(10 BYTE),
  NAS                VARCHAR2(20 BYTE),
  NORUM              VARCHAR2(20 BYTE),
  DTNAIS             VARCHAR2(10 BYTE),
  SXPMSI             VARCHAR2(1 BYTE),
  CDURM              VARCHAR2(4 BYTE),
  KYTYPAUTLIT        VARCHAR2(20 BYTE),
  D8EEUE             VARCHAR2(10 BYTE),
  MDEEUE             VARCHAR2(1 BYTE),
  TYTRPR             VARCHAR2(1 BYTE),
  D8SOUE             VARCHAR2(10 BYTE),
  MDSOUE             VARCHAR2(1 BYTE),
  TYTRDS             VARCHAR2(1 BYTE),
  CDRESI             VARCHAR2(5 BYTE),
  PDNAIS             INTEGER,
  AGEGEST            INTEGER,
  DDR2               VARCHAR2(10 BYTE),
  NBSEAN             INTEGER,
  NBDAS              INTEGER,
  NBDAD              INTEGER,
  NBACTE             INTEGER,
  DP                 VARCHAR2(8 BYTE),
  DR                 VARCHAR2(8 BYTE),
  IGS                VARCHAR2(3 BYTE),
  CONFCDRSS          VARCHAR2(1 BYTE),
  RDT_TYPMACH        VARCHAR2(1 BYTE),
  RDT_TYPDOSIM       VARCHAR2(1 BYTE),
  NUMINNO            VARCHAR2(20 BYTE),
  CONVERSION_HC      VARCHAR2(1 BYTE),
  PC_RAAC            VARCHAR2(1 BYTE),
  SURVEILLANCE_PART  VARCHAR2(1 BYTE),
  ADMIN_RESERV_HOSP  VARCHAR2(1 BYTE),
  RESCRIT_TARIFAIRE  VARCHAR2(1 BYTE),
  CATEG_NB_INTERV    VARCHAR2(1 BYTE),
  ZONRES             VARCHAR2(20 BYTE),
  DUREESEJPART       INTEGER,
  ANSOR              VARCHAR2(4 BYTE),
  NBIVGANT           VARCHAR2(20 BYTE),
  ANIVGPREC          VARCHAR2(20 BYTE),
  NBNAIVIVANT        VARCHAR2(20 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RSA_DIAGS
(
  CLE_RSA   VARCHAR2(10 BYTE),
  NSEQRUM   VARCHAR2(10 BYTE),
  POSITION  BINARY_DOUBLE,
  DIAG      VARCHAR2(6 BYTE),
  ANSOR     VARCHAR2(4 BYTE),
  NORSS     VARCHAR2(20 BYTE),
  NAS       VARCHAR2(20 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RUM_DIAGS
(
  NAS       VARCHAR2(10 BYTE),
  NORUM     VARCHAR2(20 BYTE),
  POSITION  BINARY_DOUBLE,
  DIAG      VARCHAR2(8 BYTE),
  ANSOR     VARCHAR2(4 BYTE),
  NORSS     VARCHAR2(10 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RSA_ANO
(
  NOFINESS          VARCHAR2(9 BYTE),
  NOFORMAT          VARCHAR2(3 BYTE),
  NOFORMHOSP        VARCHAR2(3 BYTE),
  MOISSORT          VARCHAR2(2 BYTE),
  ANNEESORT         VARCHAR2(4 BYTE),
  CRSECU            VARCHAR2(1 BYTE),
  CRDNAI            VARCHAR2(1 BYTE),
  CRSEXE            VARCHAR2(1 BYTE),
  CRNODA            VARCHAR2(1 BYTE),
  CRFUSHOSP         VARCHAR2(1 BYTE),
  CRFUSPMSI         VARCHAR2(1 BYTE),
  CRDTENT           VARCHAR2(1 BYTE),
  NOANON            VARCHAR2(17 BYTE),
  NOSEJ             INTEGER,
  CLE_RSA           VARCHAR2(10 BYTE),
  DTENT             VARCHAR2(10 BYTE),
  DTSORT            VARCHAR2(10 BYTE),
  CRCODEGR          VARCHAR2(1 BYTE),
  CREXTICM          VARCHAR2(1 BYTE),
  CRPRFOJO          VARCHAR2(1 BYTE),
  CRNATASS          VARCHAR2(1 BYTE),
  CRTYPECO          VARCHAR2(1 BYTE),
  CRFACTAM          VARCHAR2(1 BYTE),
  CRMOTNOFACT       VARCHAR2(1 BYTE),
  CRTOP18           VARCHAR2(1 BYTE),
  CRNBEVEN          VARCHAR2(1 BYTE),
  CRTICMOD          VARCHAR2(1 BYTE),
  CRFORJOU          VARCHAR2(1 BYTE),
  CRFACTOT          VARCHAR2(1 BYTE),
  CRMAJPAR          VARCHAR2(1 BYTE),
  CRMTBASE          VARCHAR2(1 BYTE),
  CRTXRM            VARCHAR2(1 BYTE),
  CRPBCMU           VARCHAR2(1 BYTE),
  CRNOADM           VARCHAR2(1 BYTE),
  CRHOSPNE          VARCHAR2(1 BYTE),
  CRHOSPPO          VARCHAR2(1 BYTE),
  CRDTHOSP          VARCHAR2(1 BYTE),
  CRRMBAMC          VARCHAR2(1 BYTE),
  CODEGR            VARCHAR2(2 BYTE),
  CDEXTICM          VARCHAR2(2 BYTE),
  CDPRFOJO          VARCHAR2(2 BYTE),
  NATASS            VARCHAR2(2 BYTE),
  TYPECONT          VARCHAR2(2 BYTE),
  FACTAM            VARCHAR2(1 BYTE),
  MOTNOFACT         VARCHAR2(1 BYTE),
  FAC18             VARCHAR2(1 BYTE),
  NBEVEN            INTEGER,
  MTFACTMO          BINARY_DOUBLE,
  MTFORJOU          BINARY_DOUBLE,
  MTFACTOT          BINARY_DOUBLE,
  MTMALPAR          BINARY_DOUBLE,
  MTBASERM          BINARY_DOUBLE,
  TAUXRM            BINARY_DOUBLE,
  PBCMU             VARCHAR2(1 BYTE),
  NOANOMER          VARCHAR2(32 BYTE),
  HOSPNE            VARCHAR2(1 BYTE),
  HOSPPO            VARCHAR2(1 BYTE),
  DTHOSP            VARCHAR2(10 BYTE),
  MTRMBAMC          BINARY_DOUBLE,
  COK               INTEGER,
  ANSOR             VARCHAR2(4 BYTE),
  NORSS             VARCHAR2(10 BYTE),
  NAS               VARCHAR2(20 BYTE),
  CRCDNAI           VARCHAR2(1 BYTE),
  CRCSEXE           VARCHAR2(1 BYTE),
  CRCDGESTION       VARCHAR2(1 BYTE),
  CRPARTASS         VARCHAR2(1 BYTE),
  CRNUMENT          VARCHAR2(1 BYTE),
  CRRGNAISS         VARCHAR2(1 BYTE),
  CRRGBEN           VARCHAR2(1 BYTE),
  CRCAISSGEST       VARCHAR2(1 BYTE),
  CRCENTRGEST       VARCHAR2(1 BYTE),
  CDGESTION         VARCHAR2(2 BYTE),
  PARTASS           VARCHAR2(20 BYTE),
  NUMENT            VARCHAR2(20 BYTE),
  RGNAISS           VARCHAR2(1 BYTE),
  RGBEN             VARCHAR2(3 BYTE),
  CAISSGEST         VARCHAR2(3 BYTE),
  CENTRGEST         VARCHAR2(4 BYTE),
  CRPECSU           VARCHAR2(1 BYTE),
  CRAT              VARCHAR2(1 BYTE),
  CRCOMP            VARCHAR2(1 BYTE),
  IDHOSPI1          VARCHAR2(20 BYTE),
  PECSU             VARCHAR2(20 BYTE),
  NUMAT             VARCHAR2(20 BYTE),
  NUMCOMP           VARCHAR2(20 BYTE),
  NOANONIND         VARCHAR2(17 BYTE),
  CRCFINESSPMSI     VARCHAR2(1 BYTE),
  CRCIMMATIND       VARCHAR2(1 BYTE),
  CRCNATJUSTDROITS  VARCHAR2(1 BYTE),
  CRCDATEPEC        VARCHAR2(1 BYTE),
  CRCDATEATTEST     VARCHAR2(1 BYTE),
  CRCDELIVR         VARCHAR2(1 BYTE),
  CRCREGPRESTASS    VARCHAR2(1 BYTE),
  CRCECLATFLUX      VARCHAR2(1 BYTE),
  CRCDTENT          VARCHAR2(1 BYTE),
  CRCDTSORT         VARCHAR2(1 BYTE),
  NOCHAINMER        VARCHAR2(32 BYTE),
  NATJUSTDROITS     VARCHAR2(1 BYTE),
  DATETABLPEC       VARCHAR2(8 BYTE),
  DATCARTVITALE     VARCHAR2(8 BYTE),
  DELIVR            VARCHAR2(3 BYTE),
  REGPRESTASS       VARCHAR2(3 BYTE),
  ECLATFLUX         VARCHAR2(1 BYTE),
  DTENT2            VARCHAR2(8 BYTE),
  DTSORT2           VARCHAR2(8 BYTE),
  EMPNUM            VARCHAR2(100 BYTE),
  CRMTTFACPAT       VARCHAR2(1 BYTE),
  CRREJETAMO        VARCHAR2(1 BYTE),
  CRDATFACTAMO      VARCHAR2(1 BYTE),
  CRDATFACTAMC      VARCHAR2(1 BYTE),
  CRDATFACTPAT      VARCHAR2(1 BYTE),
  CRDATPAIAMO       VARCHAR2(1 BYTE),
  CRDATPAIAMC       VARCHAR2(1 BYTE),
  CRDATPAIPAT       VARCHAR2(1 BYTE),
  CRSTATFTAMO       VARCHAR2(1 BYTE),
  CRSTATFTAMC       VARCHAR2(1 BYTE),
  CRSTATFTPAT       VARCHAR2(1 BYTE),
  CRPAYSPAT         VARCHAR2(1 BYTE),
  CRIDHOSPI1        VARCHAR2(1 BYTE),
  MTTFACPAT         BINARY_DOUBLE,
  REJETAMO          VARCHAR2(1 BYTE),
  DATFACTAMO        VARCHAR2(8 BYTE),
  DATFACTAMC        VARCHAR2(8 BYTE),
  DATFACTPAT        VARCHAR2(8 BYTE),
  DATPAIAMO         VARCHAR2(8 BYTE),
  DATPAIAMC         VARCHAR2(8 BYTE),
  DATPAIPAT         VARCHAR2(8 BYTE),
  STATFTAMO         VARCHAR2(1 BYTE),
  STATFTAMC         VARCHAR2(1 BYTE),
  STATFTPAT         VARCHAR2(1 BYTE),
  PAYSPAT           VARCHAR2(3 BYTE),
  CRIPP             VARCHAR2(1 BYTE),
  IPPA              VARCHAR2(100 BYTE),
  ILIASON           VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_CIM_HIERARCHIE_CODE
(
  CODE           VARCHAR2(6 BYTE),
  TR             VARCHAR2(1 BYTE),
  LIB_COURT      VARCHAR2(300 BYTE),
  LIB_LONG       VARCHAR2(1000 BYTE),
  CATEGORIE      VARCHAR2(3 BYTE),
  LIB_CATEGORIE  VARCHAR2(1000 BYTE),
  CHAPITRE       VARCHAR2(15 BYTE),
  BLOC           VARCHAR2(15 BYTE),
  LIB_CHAPITRE   VARCHAR2(1000 BYTE),
  LIB_BLOC       VARCHAR2(1000 BYTE),
  ANSEQTA        VARCHAR2(4 BYTE),
  TSSR           VARCHAR2(4 BYTE),
  TPSY           VARCHAR2(1 BYTE),
  TIME_I         INTEGER
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_TARIFS_MCO_GHS
(
  GHS          VARCHAR2(255 BYTE),
  GHM          VARCHAR2(255 BYTE),
  LIBELLE_GHM  VARCHAR2(255 BYTE),
  BORNE_BASSE  INTEGER,
  BORNE_HAUTE  INTEGER,
  TARIF_BASE   BINARY_DOUBLE,
  FORFAIT_EXB  BINARY_DOUBLE,
  TARIF_EXB    BINARY_DOUBLE,
  TARIF_EXH    BINARY_DOUBLE,
  DATE_EFFET   VARCHAR2(255 BYTE),
  ANSEQTA      VARCHAR2(255 BYTE),
  TIME_I       VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RUM_ACTES
(
  NAS         VARCHAR2(20 BYTE),
  NORUM       VARCHAR2(20 BYTE),
  DTACTE      VARCHAR2(10 BYTE),
  CDCCAM      VARCHAR2(10 BYTE),
  DESCRI      VARCHAR2(3 BYTE),
  PHASE       VARCHAR2(1 BYTE),
  ACT         VARCHAR2(1 BYTE),
  EXTDOC      VARCHAR2(20 BYTE),
  MODIF       VARCHAR2(20 BYTE),
  REMBEXCEPT  VARCHAR2(20 BYTE),
  ASSONP      VARCHAR2(20 BYTE),
  NBEXEC      INTEGER,
  ANSOR       VARCHAR2(4 BYTE),
  NORSS       VARCHAR2(10 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RSA_VALO
(
  CLE_RSA         VARCHAR2(255 BYTE),
  NBSEANCE        INTEGER,
  MOISSOR         VARCHAR2(255 BYTE),
  ANSEQTA         VARCHAR2(255 BYTE),
  REC_TOTALE      BINARY_DOUBLE,
  REC_BEE         BINARY_DOUBLE,
  REC_BASE        BINARY_DOUBLE,
  REC_EXB         BINARY_DOUBLE,
  REC_EXH         BINARY_DOUBLE,
  REC_REP         BINARY_DOUBLE,
  REC_REA         BINARY_DOUBLE,
  REC_STF         BINARY_DOUBLE,
  REC_SRC         BINARY_DOUBLE,
  REC_NN1         BINARY_DOUBLE,
  REC_NN2         BINARY_DOUBLE,
  REC_NN3         BINARY_DOUBLE,
  REC_DIALHOSP    BINARY_DOUBLE,
  REC_CAISHYP     BINARY_DOUBLE,
  REC_APH         BINARY_DOUBLE,
  REC_ANT         BINARY_DOUBLE,
  REC_RAP         BINARY_DOUBLE,
  REC_REHOSP_GHM  BINARY_DOUBLE,
  REC_RDT_TOT     BINARY_DOUBLE,
  REC_SDC         BINARY_DOUBLE,
  REC_PO_TOT      BINARY_DOUBLE,
  TYPE_FIN        BINARY_DOUBLE,
  TYPVIDHOSP      BINARY_DOUBLE,
  NORSS           VARCHAR2(255 BYTE),
  NAS             VARCHAR2(255 BYTE),
  ANSOR           VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_RUM_VALO
(
  CLE_RSA             VARCHAR2(255 BYTE),
  NSEQRUM             VARCHAR2(255 BYTE),
  NBRUM               INTEGER,
  P1                  INTEGER,
  P2                  INTEGER,
  N                   INTEGER,
  PMCT_UM_DP          BINARY_DOUBLE,
  NB_UMA              INTEGER,
  NORSS               VARCHAR2(255 BYTE),
  CDURM               VARCHAR2(255 BYTE),
  TYPAUT1             VARCHAR2(255 BYTE),
  REC_REA_RUM         BINARY_DOUBLE,
  REC_STF_RUM         BINARY_DOUBLE,
  REC_SRC_RUM         BINARY_DOUBLE,
  REC_REP_RUM         BINARY_DOUBLE,
  REC_NN1_RUM         BINARY_DOUBLE,
  REC_NN2_RUM         BINARY_DOUBLE,
  REC_NN3_RUM         BINARY_DOUBLE,
  REC_BEE_RUM         BINARY_DOUBLE,
  REC_PO_TOT_RUM      BINARY_DOUBLE,
  REC_BASE_RUM        BINARY_DOUBLE,
  REC_EXB_RUM         BINARY_DOUBLE,
  REC_EXH_RUM         BINARY_DOUBLE,
  REC_ANT_RUM         BINARY_DOUBLE,
  REC_SDC_RUM         BINARY_DOUBLE,
  REC_CAISHYP_RUM     BINARY_DOUBLE,
  REC_REHOSP_GHM_RUM  BINARY_DOUBLE,
  REC_APH_RUM         BINARY_DOUBLE,
  REC_TOTALE_RUM      BINARY_DOUBLE,
  REC_DIALHOSP_RUM    BINARY_DOUBLE,
  REC_RDT_TOT_RUM     BINARY_DOUBLE,
  REC_RAP_RUM         BINARY_DOUBLE,
  REC_ANNEXES_RUM     BINARY_DOUBLE,
  SUPREA              BINARY_DOUBLE,
  SUPSTF              BINARY_DOUBLE,
  SUPSRC              BINARY_DOUBLE,
  SUPNN1              BINARY_DOUBLE,
  SUPNN2              BINARY_DOUBLE,
  SUPNN3              BINARY_DOUBLE,
  SUPREP              BINARY_DOUBLE,
  SUPREA_PROP         BINARY_DOUBLE,
  SUPSTF_PROP         BINARY_DOUBLE,
  SUPSRC_PROP         BINARY_DOUBLE,
  SUPREP_PROP         BINARY_DOUBLE,
  SUPNN1_PROP         BINARY_DOUBLE,
  SUPNN2_PROP         BINARY_DOUBLE,
  SUPNN3_PROP         BINARY_DOUBLE,
  FLAG_REA            BINARY_DOUBLE,
  FLAG_STF            BINARY_DOUBLE,
  FLAG_SRC            BINARY_DOUBLE,
  FLAG_NN1            BINARY_DOUBLE,
  FLAG_NN2            BINARY_DOUBLE,
  FLAG_NN3            BINARY_DOUBLE,
  FLAG_REP            BINARY_DOUBLE,
  NAS                 VARCHAR2(255 BYTE),
  ANSOR               VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_TARIFS_MCO_GHS_9999
(
  GHS          VARCHAR2(4 BYTE),
  GHM          VARCHAR2(6 BYTE),
  LIBELLE_GHM  VARCHAR2(200 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_CCAM_ACTES
(
  CODE                VARCHAR2(7 BYTE),
  LIBELLE_COURT       VARCHAR2(500 BYTE),
  TYPE_ACTE           VARCHAR2(1 BYTE),
  COMPATIBILITE_SEXE  VARCHAR2(1 BYTE),
  DATE_DEBUT          VARCHAR2(10 BYTE),
  LIBELLE_LONG        VARCHAR2(1000 BYTE),
  DATE_FIN            VARCHAR2(10 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_GHM_GHM_REGROUPEMENT
(
  GHM             VARCHAR2(255 BYTE),
  LIBELLE_GHM     VARCHAR2(255 BYTE),
  DA              VARCHAR2(255 BYTE),
  LIBELLE_DA      VARCHAR2(255 BYTE),
  GP_CAS          VARCHAR2(255 BYTE),
  LIBELLE_GP_CAS  VARCHAR2(255 BYTE),
  GA              VARCHAR2(255 BYTE),
  LIBELLE_GA      VARCHAR2(255 BYTE),
  DA_GP           VARCHAR2(255 BYTE),
  DA_GP_GA        VARCHAR2(255 BYTE),
  ANSEQTA         VARCHAR2(255 BYTE),
  TIME_I          VARCHAR2(255 BYTE),
  ASO             VARCHAR2(255 BYTE),
  RACINE          VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_CCAM_HIERARCHIE_ACTES
(
  STRATE            VARCHAR2(11 BYTE),
  CODE              VARCHAR2(7 BYTE),
  PARENT            VARCHAR2(7 BYTE),
  LIBELLE           VARCHAR2(500 BYTE),
  NIVEAU_1          VARCHAR2(2 BYTE),
  NIVEAU_2          VARCHAR2(2 BYTE),
  NIVEAU_3          VARCHAR2(2 BYTE),
  NIVEAU_4          VARCHAR2(2 BYTE),
  TYPE              VARCHAR2(1 BYTE),
  LIB_NIVEAU        VARCHAR2(40 BYTE),
  NIVEAU            INTEGER,
  LIBELLE_NIVEAU_1  VARCHAR2(500 BYTE),
  LIBELLE_NIVEAU_2  VARCHAR2(500 BYTE),
  LIBELLE_NIVEAU_3  VARCHAR2(500 BYTE),
  LIBELLE_NIVEAU_4  VARCHAR2(500 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_GHM_RGHM_REGROUPEMENT
(
  RACINE          VARCHAR2(255 BYTE),
  LIBELLE_RACINE  VARCHAR2(255 BYTE),
  DA              VARCHAR2(255 BYTE),
  LIBELLE_DA      VARCHAR2(255 BYTE),
  GP_CAS          VARCHAR2(255 BYTE),
  LIBELLE_GP_CAS  VARCHAR2(255 BYTE),
  GA              VARCHAR2(255 BYTE),
  LIBELLE_GA      VARCHAR2(255 BYTE),
  DA_GP           VARCHAR2(255 BYTE),
  DA_GP_GA        VARCHAR2(255 BYTE),
  ANSEQTA         VARCHAR2(255 BYTE),
  TIME_I          VARCHAR2(255 BYTE),
  ASO             VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE REF_CCAM_ACTES_AVEC_DESCRI
(
  CODE                   VARCHAR2(255 BYTE),
  TYPE_ACTE              VARCHAR2(255 BYTE),
  COMPATIBILITE_SEXE     VARCHAR2(255 BYTE),
  LIBELLE_COURT          VARCHAR2(300 BYTE),
  DATE_DEBUT             VARCHAR2(255 BYTE),
  LIBELLE_LONG           VARCHAR2(500 BYTE),
  DATE_FIN               VARCHAR2(255 BYTE),
  FLAG_DESCRI            INTEGER,
  EXTENSION_DESCRIPTIVE  VARCHAR2(255 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE STAT_ASSOC_UM_UF
(
  CDURM         VARCHAR2(10 BYTE),
  UF_PMSI_ALT   VARCHAR2(10 BYTE),
  NOMBRE_RUM    NUMBER,
  NOMBRE_RSS    NUMBER,
  DATE_STAT     DATE,
  MAX_DATE_RUM  DATE,
  RANG_UF       NUMBER
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_CORA_IPP
(
  NORSS        VARCHAR2(255 BYTE),
  ANSOR        VARCHAR2(4 BYTE),
  ID_SEJOUR    NUMBER(8)                        NOT NULL,
  ID_PATIENT   NUMBER(8)                        NOT NULL,
  DATE_DEBUT   DATE,
  DATE_FIN     DATE,
  IPP_PATIENT  VARCHAR2(20 BYTE)                NOT NULL
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;


CREATE TABLE MCO_CORA_UF_PMSI
(
  NORSS              VARCHAR2(255 BYTE),
  ANSOR              VARCHAR2(4 BYTE),
  ID_RSS             NUMBER(8)                  NOT NULL,
  NORUM              VARCHAR2(42 BYTE),
  DUREE_RUM          NUMBER(5),
  DATE_DEBUT_RUM     DATE,
  DATE_FIN_RUM       DATE,
  ID_RUM             NUMBER(10)                 NOT NULL,
  CHRONO             NUMBER(3),
  ID_MVT             NUMBER(8)                  NOT NULL,
  UF_PMSI            VARCHAR2(10 BYTE),
  ID_UM_HEBERG       NUMBER(5),
  ID_UNITE_HEBERG    NUMBER(5),
  ID_UM_RESP_MED     NUMBER(5),
  ID_UNITE_RESP_MED  NUMBER(5),
  CDURM              VARCHAR2(10 BYTE)
)
LOGGING 
NOCOMPRESS 
NOCACHE
NOPARALLEL
MONITORING;

