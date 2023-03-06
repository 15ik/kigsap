@AbapCatalog.sqlViewName: 'ZSVBMMMATDOCB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]자재문서 리스트 조회-BATCH BASE'
define view ZCBMMMATDOCB as select from matdoc  as a left outer join ZCCMM_BATCHFIND as b on a.matnr = b.matnr
                                                                                    and  a.charg = b.BATCH
                                       association [1] to t001 as t001 on $projection.bukrs = t001.bukrs
                                       association [1] to t001w as t001w on $projection.werks = t001w.werks
                                       association [1] to t001l as t001l on $projection.werks = t001l.werks
                                                         and $projection.lgort = t001l.lgort
                                       association [1] to t001w as t001w2 on $projection.umwrk = t001w2.werks
                                       association [1] to t001l as t001l2 on $projection.umwrk = t001l2.werks
                                                         and $projection.umlgo = t001l2.lgort
                                       association [1]   to makt as makt  on $projection.matnr = makt.matnr
                                                           and makt.spras =  $session.system_language
                                       association [1] to lfa1 as lfa1 on $projection.lifnr = lfa1.lifnr
                                       association [1] to kna1 as kna1 on $projection.kunnr = kna1.kunnr
                                       association [1] to t156t as t156t on $projection.bwart = t156t.bwart
                                                           and  $projection.sobkz = t156t.sobkz
                                                           and  $projection.kzbew = t156t.kzbew
                                                           and  $projection.kzzug = t156t.kzzug
                                                           and  $projection.kzvbr = t156t.kzvbr
                                                           and t156t.spras =  $session.system_language

{
 key a.budat,
 key a.charg,
 key a.cpudt,
 key a.cputm,
 a.mjahr,
 a.mblnr,
 a.zeile,
 a.bukrs,
 t001.butxt,
 a.matnr,
 makt.maktx,
 a.bwart,
t156t.btext,
 case when a.bwart = '101' and a.kzbew = 'B' and a.lbbsa_sid <> '6' then '구매입고'
      when a.bwart = '102' and a.kzbew = 'B' and a.lbbsa_sid <> '6' then '구매입고-취소'
      when a.bwart = '101' and a.kzbew = 'F' and a.lbbsa_sid <> '6' then '생산입고'
      when a.bwart = '102' and a.kzbew = 'F' and a.lbbsa_sid <> '6' then '생산입고-취소'
      when a.bwart = '101' and a.kzbew = 'B' and a.lbbsa_sid = '6' then 'STO입고'
      when a.bwart = '102' and a.kzbew = 'B' and a.lbbsa_sid = '6' then 'STO입고-취소'
      when a.bwart = '601' then '판매출고'
      when a.bwart = '641' then 'STO출고'
      when a.bwart = '261' then '생산출고'
      when a.bwart = '541' then '사급자재반출'
      when a.bwart = '543' then '사급자재투입'
      when a.bwart = '511' then '무상입고'
      when a.bwart = '561' then '기초재고'
      else case when a.shkzg = 'H' then '기타이동(+)' else '기타이동(-)' end
 end as mvttype,
 a.sobkz,
 a.werks,
 t001w.name1 as werksname,
 a.lgort,
 t001l.lgobe as lgortname,
 a.umwrk,
 t001w2.name1 as umwrkname,
 a.umlgo,
 t001l2.lgobe as umlgoname,
 a.bwtar,
 a.menge,
 a.erfme,
 a.erfmg,
 a.meins,
 a.lifnr,
 lfa1.name1 as lifnrname,
 a.kunnr,
 kna1.name1 as kunnrname,
 b.hsdat,
 b.VFdat,
 b.LICHN,
 b.Zmaker,
 b.waterratiO,
 b.potency,
 b.net,
 b.custlot ,
 a.lbbsa_sid,
 a.kzbew,
 a.bstaus_sg,
 a.bsttyp_sg,
 a.berid,
 a.waers,
 a.dmbtr,
 a.ebeln,
 a.ebelp,
 a.aufnr,
 a.aufps,
 a.rsnum,
 a.rspos,
 a.vbeln_im,
 a.vbelp_im,
 a.cancelled,
 a.reversal_movement,
 a.shkzg,
 a.elikz,
 a.kokrs,
 a.kzvbr,
 a.kzzug,
 a.prctr,
 a.aufpl,
 a.aplzl,
 a.sakto,
 a.tcode2,
 a.usnam
}
where  a.xauto = '' 
//and a.charg <> ''
//where a.cancelled = '' 
//and a.cancellation_type = '' and a.xauto = '' 
// and a.charg <> ''
