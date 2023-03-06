@AbapCatalog.sqlViewName: 'ZSVCMM_EKET1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '납품일 대비 입고'
define view ZCCMM_EKET1 as select from ekpo
                         inner join eket
                         on ekpo.ebeln = eket.ebeln and ekpo.ebelp = eket.ebelp
                         left outer join ekko
                         on ekpo.ebeln = ekko.ebeln
           association [1] to lfa1 as lfa1 on $projection.lifnr = lfa1.lifnr
           association [1] to lfm1 as lfm1 on ekko.lifnr = lfm1.lifnr
                                             and ekko.ekorg = lfm1.ekorg
           association [1] to t161t as t161t on $projection.bsart = t161t.bsart
                                             and t161t.spras = $session.system_language

 {   key eket.ebeln,
     key eket.ebelp,
     key eket.etenr,
     ekko.lifnr,
     lfa1.name1,
     ekpo.matnr,
     ekpo.txz01,
     ekpo.bwtar,
     ekpo.bstae,
     ekpo.meins,
     ekpo.menge,
     eket.eindt,
     eket.menge as eket_qty,
     eket.dabmg as id_qty,
     eket.wemng as gr_qty,

     (case when bstae = '0004'
     then (eket.dabmg-eket.wemng)
     else eket.dabmg end) as remain_id,  /// 납품 미입고

     (eket.menge-eket.wemng) as remain_gr,  /// 미입고잔량
     ekpo.werks,
     ekpo.lgort,
     ekpo.retpo,
     ekko.bsart,
     t161t.batxt,
     lfm1.kalsk,
     ekko.ekgrp,
     ekko.zorder_person,
     ekko.zorder_department,
     ekko.zexpen_person,
     ekko.zexpen_department,
     ekko.aedat,
     ekpo.matkl,
     ekpo.pstyp,
     ekpo.knttp,
     ekko.bukrs,
     ekko.bedat, ///증빙일
     ekpo.elikz,   /// 납품완료
     ekpo.aedat as aedat_po

}
    where ekko.lifnr is not initial
     and ekpo.loekz is initial
     and eket.menge is not initial
