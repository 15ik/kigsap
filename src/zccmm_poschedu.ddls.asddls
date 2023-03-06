@AbapCatalog.sqlViewName: 'ZSVCMM_POSCHEDU'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매납품일정 계획'
define view ZCCMM_POSCHEDU as select from eket as a
                           left outer join ekpo as b on a.ebeln = b.ebeln and a.ebelp = b.ebelp
                           left outer join zsvbmmekbesum as c on a.ebeln = c.ebeln and a.ebelp = c.ebelp
                           left outer join ekko as d on  a.ebeln = d.ebeln
                           association [1] to t001w as t001w  on $projection.werks = t001w.werks
                           association [1]   to t001l as T001L  on $projection.werks = T001L.werks
                                                                and $projection.lgort = T001L.lgort

 {
   key a.ebeln,
   key a.ebelp,
   key a.etenr,
       d.lifnr,
       a.eindt,
       b.meins,
       a.menge,
       a.dabmg,
       a.menge - a.dabmg as rem_sl_idqty,
       a.wemng,
       a.dabmg - a.wemng as rem_sl_grqty,
       b.matnr,
       b.txz01,
       b.bukrs,
       b.werks,
       t001w.name1,
       b.loekz,
       b.lgort,
       t001l.lgobe,
       b.elikz,
       b.bstae,
       b.menge as po_qty,
       b.pstyp,

       c.menge as sum_grqty,
       b.menge - c.menge as rema_grqty,
       b.netpr,
       b.peinh,
       d.waers,
       d.bsart,
       d.bstyp,
       d.frgke,
       case when d.frgke ='X' then '결재대기'
                when d.frgke ='D' then '상신중'
                when d.frgke ='P' then '승인완료'
                when d.frgke ='J' then '반려'
                when d.frgke ='' then '승인완료'
                end as wf_status,
       a.banfn,
       a.bnfpo
}
