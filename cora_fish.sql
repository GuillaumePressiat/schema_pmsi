

/*drop table PMSI.MCO_CORA_IPP */

create table PMSI.MCO_CORA_IPP as
select a.NORSS, a.ANSOR, b.ID_SEJOUR, b.ID_PATIENT, b.DATE_DEBUT, b.DATE_FIN, c.IPP_PATIENT from
PMSI.MCO_RSA_RSA a inner join 
CORA_REC.TB_SYNTH_RSS@CORAD b on a.NORSS = b.NO_RSS INNER JOIN 
CORA_REC.TB_PATIENT@CORAD c on b.ID_PATIENT = c.ID_PATIENT

drop table PMSI.MCO_CORA_UF_PMSI 


create table PMSI.MCO_CORA_UF_PMSI as
select a.NORSS, a.ANSOR, b.ID_RSS, b.ID_RSS||LPAD(c.CHRONO,2, '0') as NORUM, 
       c.DUREE as DUREE_RUM, c.DATE_DEBUT_RUM, c.DATE_FIN_RUM, c.ID_RUM, c.CHRONO, 
       d.ID_MVT, e.UF_PMSI,
       c.ID_UM_HEBERG, c.ID_UNITE_HEBERG, c.ID_UM_RESP_MED, c.ID_UNITE_RESP_MED, f.CODE_UNITE_MED as CDURM from
PMSI.MCO_RSA_RSA a inner join 
CORA_REC.TB_SYNTH_RSS@CORAD b on a.NORSS = b.NO_RSS INNER JOIN
CORA_REC.TB_SYNTH_RUM@CORAD c on b.ID_RSS = c.ID_RSS inner join
CORA_REC.TB_MVT_RUM@CORAD d on c.ID_RUM = d.ID_RUM inner join
CORA_REC.TB_MVT@CORAD e on d.ID_MVT = e.ID_MVT left join
CORA_REC.TB_UNITE_MEDICALE@CORAD f on c.ID_UM_RESP_MED = f.ID_UNITE_MED


select * from CORA_REC.TB_UNITE_MEDICALE@CORAD

select ANSOR, count(distinct NORSS) from PMSI.MCO_CORA_IPP
group by ANSOR

select ansor, count(distinct NORSS) from (
select distinct a.NORSS, a.NAS, a.ANSOR, b.ID_SEJOUR from
PMSI.MCO_RSA_RSA a left join PMSI.MCO_CORA_IPP b
on a.NORSS = b.NORSS
where ID_SEJOUR is NULL and a.ANSOR < '2020'
)
group by ANSOR


