@AbapCatalog.sqlViewName: 'ZSVCMM_REALSTOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '실시간 재고 현황'

//배치 재고(창고)
define view ZCCMM_STOCK_REALTIME as select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mchb_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                       inner join ZCCMM_BATCHFIND as C on B.matnr = C.matnr and B.charg = C.BATCH
                                       inner join t001k as k on B.werks = k.bwkey
                                       inner join mcha as J on B.matnr = J.matnr and B.werks = J.werks and B.charg = J.charg
                                       association [1] to t001w as D on $projection.PLANT = D.werks
                                       association [1] to t001l as E on $projection.PLANT = E.werks
                                                         and $projection.SLOCATION = E.lgort
                                       association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
//                                       association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr

{
   key k.bukrs as company,
   key B.werks as PLANT,
   key D.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key E.lgobe as SLNAME,
   key cast('' as abap.char(10)) as BP,
   key cast('' as abap.char(35)) as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SL_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key J.charg as BATCHSOL,
   key J.bwtar as BWTAR,
   A.meins,
   case when B.clabs is null then 0 else B.clabs end as AVAILABLESTOCK,
   case when B.cinsm is null then 0 else B.cinsm end as QISTOCK,
   case when B.cspem is null then 0 else B.cspem end as BLOCKSTOCK,
   C.lifnr as CHAR_BP,
   C.hsdat as PRDUCTIONDATE,
   C.VFdat as EXPIREDDATE,
   C.LICHN as PRDDUCTLOT,
   C.Zmaker as MAKER,
   C.lwedt,
   C.waterratiO,
   C.potency        ,
   C.net,
   C.custlot,
   case when ( C.VFdat <> '00000000' ) then
   dats_days_between( tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                      $session.client,
                      'NULL' ) ,cast(C.VFdat as abap.dats))
   end  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl

}
where ( B.clabs > 0 or B.cinsm > 0 or B.cspem > 0 ) and z.xchpf = 'X'

//일반 재고(창고-일반)
union all

select from mara as A
inner join marc as z on A.matnr = z.matnr
inner join nsdm_e_mard_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                       inner join t001k as k on B.werks = k.bwkey
                                       inner join marc as C on B.matnr = C.matnr and B.werks = C.werks
                                       association [1] to t001w as D on $projection.PLANT = D.werks
                                       association [1] to t001l as E on $projection.PLANT = E.werks
                                                         and $projection.SLOCATION = E.lgort
                                       association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
  //                                     association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr

{
   key k.bukrs as company,
   key B.werks as PLANT,
   key D.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key E.lgobe as SLNAME,
   key cast('' as abap.char(10)) as BP,
   key cast('' as abap.char(35)) as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SL_STOCK' as abap.char(8)) as TYPE,
   key cast('' as abap.char(10)) as BATCH,
   key cast('' as abap.char(10)) as BATCHSOL,
   key cast('' as abap.char(10)) as BWTAR,
   A.meins,
   case when B.labst is null then 0 else B.labst end as AVAILABLESTOCK,
   case when B.insme is null then 0 else B.insme end as QISTOCK,
   case when B.speme is null then 0 else B.speme end as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   C.ekgrp,
   t024.eknam,
   C.dispo,
   t024d.dsnam   ,
   A.matkl
}
where ( B.labst > 0 or B.insme > 0 or B.speme > 0 ) and z.xchpf = '' and C.bwtty = ''

//일반 재고(창고-분할 평가)
union all

select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mchb_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                       inner join marc as C on B.matnr = C.matnr and B.werks = C.werks
                                       inner join t001k as k on B.werks = k.bwkey
                                       association [1] to t001w as D on $projection.PLANT = D.werks
                                       association [1] to t001l as E on $projection.PLANT = E.werks
                                                         and $projection.SLOCATION = E.lgort
                                       association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
//                                       association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr

{
   key k.bukrs as company,
   key B.werks as PLANT,
   key D.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key E.lgobe as SLNAME,
   key cast('' as abap.char(10)) as BP,
   key cast('' as abap.char(35)) as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SL_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key cast('' as abap.char(10)) as BATCHSOL,
   key B.charg as BWTAR,
   A.meins,
   case when B.clabs is null then 0 else B.clabs end as AVAILABLESTOCK,
   case when B.cinsm is null then 0 else B.cinsm end as QISTOCK,
   case when B.cspem is null then 0 else B.cspem end as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl
}
where ( B.clabs > 0 or B.cinsm > 0 or B.cspem > 0 ) and z.xchpf = '' and C.bwtty <> ''


// 업체 배치재고
union all
select from mara as A

                                        inner join nsdm_e_mslb_diff   as B on A.matnr = B.matnr
                                        inner join ZCCMM_BATCHFIND as C on  B.matnr = C.matnr and B.charg = C.BATCH
                                        inner join marc as z on B.matnr = z.matnr and B.werks = z.werks
                                        inner join t001k as k on B.werks = k.bwkey
                                        inner join mcha as J on B.matnr = J.matnr and B.werks = J.werks and B.charg = J.charg
                                        association [1]   to lfa1 as D on  $projection.BP = D.lifnr
                                        association [1]   to t001w as E on $projection.PLANT = E.werks
                                        association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
    //                                   association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr
{
   key k.bukrs as company,
   key B.werks as PLANT,
   key E.name1 as PLANTNAME,
   key cast('' as abap.char(4)) as SLOCATION,
   key cast('' as abap.char(16)) as SLNAME,
   key B.lifnr as BP,
   key D.name1 as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SC_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key J.charg as BATCHSOL,
   key J.bwtar as BWTAR,
   A.meins,
   case when B.lblab is null then 0 else B.lblab end as AVAILABLESTOCK,
   case when B.lbins is null then 0 else B.lbins end as QISTOCK,
   0 as BLOCKSTOCK,
   C.lifnr as CHAR_BP,
   C.hsdat as PRDUCTIONDATE,
   C.VFdat as EXPIREDDATE,
   C.LICHN as PRDDUCTLOT,
   C.Zmaker as MAKER,
   C.lwedt,
   C.waterratiO,
   C.potency        ,
   C.net,
   C.custlot,
   case when ( C.VFdat <> '00000000' ) then
   dats_days_between( tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                      $session.client,
                      'NULL' ) ,cast(C.VFdat as abap.dats))
   end  as Remaindate ,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl

}
where ( B.lblab > 0 or B.lbins > 0 ) and z.xchpf = 'X'


// 업체 일반재고
union all                        select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mslb_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                        inner join t001k as k on B.werks = k.bwkey
                                        association [1]   to lfa1 as D on  $projection.BP = D.lifnr
                                        association [1]   to t001w as E on $projection.PLANT = E.werks
                                        association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
                //                       association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr
{
   key k.bukrs as company,
   key B.werks as PLANT,
   key E.name1 as PLANTNAME,
   key cast('' as abap.char(4)) as SLOCATION,
   key cast('' as abap.char(16)) as SLNAME,
   key B.lifnr as BP,
   key D.name1 as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SC_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key B.charg as BATCHSOL,
   key cast('' as abap.char(10)) as BWTAR,
   A.meins,
   case when B.lblab is null then 0 else B.lblab end as AVAILABLESTOCK,
   case when B.lbins is null then 0 else B.lbins end as QISTOCK,
   0 as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl

}
where ( B.lblab > 0 or B.lbins > 0 ) and z.xchpf = '' and z.bwtty = ''


// 업체 평가유형재고
union all                        select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mslb_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                        inner join t001k as k on B.werks = k.bwkey
                                        association [1]   to lfa1 as D on  $projection.BP = D.lifnr
                                        association [1]   to t001w as E on $projection.PLANT = E.werks
                                        association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
                //                       association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr
{
   key k.bukrs as company,
   key B.werks as PLANT,
   key E.name1 as PLANTNAME,
   key cast('' as abap.char(4)) as SLOCATION,
   key cast('' as abap.char(16)) as SLNAME,
   key B.lifnr as BP,
   key D.name1 as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SC_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key cast('' as abap.char(10)) as BATCHSOL,
   B.charg as BWTAR,
   A.meins,
   case when B.lblab is null then 0 else B.lblab end as AVAILABLESTOCK,
   case when B.lbins is null then 0 else B.lbins end as QISTOCK,
   0 as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl

}
where ( B.lblab > 0 or B.lbins > 0 ) and z.xchpf = '' and z.bwtty <> ''
// 업체 위탁 재고(일반)
union all                        select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mkol_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                        inner join t001k as k on B.werks = k.bwkey
                                        association [1] to t001l as C on $projection.PLANT = C.werks
                                                         and $projection.SLOCATION = C.lgort
                                        association [1]   to lfa1 as D on  $projection.BP = D.lifnr
                                        association [1]   to t001w as E on $projection.PLANT = E.werks
                                        association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
               //                        association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr
{
   key k.bukrs as company,
   key B.werks as PLANT,
   key E.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key C.lgobe as SLNAME,
   key B.lifnr as BP,
   key D.name1 as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('CS_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key cast('' as abap.char(10)) as BATCHSOL,
   key cast('' as abap.char(10)) as BWTAR,
   A.meins,
   case when B.slabs is null then 0 else B.slabs end as AVAILABLESTOCK,
   case when B.sinsm is null then 0 else B.sinsm end as QISTOCK,
   case when B.sspem is null then 0 else B.sspem end as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl

}
where ( B.slabs > 0 or B.sinsm > 0 or B.sspem > 0 ) and z.xchpf = ''


// 업체 위탁 재고(배치)
union all                        select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mkol_diff   as B on z.matnr = B.matnr and z.werks = B.werks
                                        inner join ZCCMM_BATCHFIND as C on  B.matnr = B.matnr and B.charg = C.BATCH
                                        inner join t001k as k on B.werks = k.bwkey
                                        inner join mcha as J on B.matnr = J.matnr and B.werks = J.werks and B.charg = J.charg
                                        association [1] to t001l as G on $projection.PLANT = G.werks
                                                         and $projection.SLOCATION = G.lgort
                                        association [1]   to lfa1 as D on  $projection.BP = D.lifnr
                                        association [1]   to t001w as E on $projection.PLANT = E.werks
                                        association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
        //                              association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr
{
   key k.bukrs as company,
   key B.werks as PLANT,
   key E.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key G.lgobe as SLNAME,
   key B.lifnr as BP,
   key D.name1 as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('CS_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key J.charg as BATCHSOL,
   key J.bwtar as BWTAR,
   A.meins,
   case when B.slabs is null then 0 else B.slabs end as AVAILABLESTOCK,
   case when B.sinsm is null then 0 else B.sinsm end as QISTOCK,
   case when B.sspem is null then 0 else B.sspem end as BLOCKSTOCK,
   C.lifnr as CHAR_BP,
   C.hsdat as PRDUCTIONDATE,
   C.VFdat as EXPIREDDATE,
   C.LICHN as PRDDUCTLOT,
   C.Zmaker as MAKER,
   C.lwedt,
   C.waterratiO,
   C.potency        ,
   C.net,
   C.custlot,
   case when ( C.VFdat <> '00000000' ) then
   dats_days_between( tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                      $session.client,
                      'NULL' ) ,cast(C.VFdat as abap.dats))
   end  as Remaindate ,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl
}
where ( B.slabs > 0 or B.sinsm > 0 or B.sspem > 0 ) and z.xchpf = 'X'

//판매오더 재고(창고-일반-배치 미사용)
union all

select from mara as A
inner join marc as z on A.matnr = z.matnr
inner join nsdm_e_mska   as B on z.matnr = B.matnr and z.werks = B.werks
                                       inner join t001k as k on B.werks = k.bwkey
                                       inner join marc as C on B.matnr = C.matnr and B.werks = C.werks
                                       association [1] to t001w as D on $projection.PLANT = D.werks
                                       association [1] to t001l as E on $projection.PLANT = E.werks
                                                         and $projection.SLOCATION = E.lgort
                                       association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
  //                                     association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr

{
   key k.bukrs as company,
   key B.werks as PLANT,
   key D.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key E.lgobe as SLNAME,
   key cast('' as abap.char(10)) as BP,
   key cast('' as abap.char(35)) as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SO_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key B.charg as BATCHSOL,
   key cast('' as abap.char(10)) as BWTAR,
   A.meins,
   case when B.kalab is null then 0 else B.kalab end as AVAILABLESTOCK,
   case when B.kains is null then 0 else B.kains end as QISTOCK,
   case when B.kaspe is null then 0 else B.kaspe end as BLOCKSTOCK,
   cast('' as abap.char(10)) as CHAR_BP,
   cast('' as abap.dats) as PRDUCTIONDATE,
   cast('' as abap.dats) as EXPIREDDATE,
   cast('' as abap.char(16)) as PRDDUCTLOT,
   cast('' as abap.char(30)) as MAKER,
   cast('' as abap.dats) as LWEDT,
   cast(0 as abap.dec(8,3)) as waterratiO,
   cast(0 as abap.dec(8,3)) as potency,
   cast(0 as abap.dec(8,3)) as NET,
   cast(0 as abap.char(30)) as CUSTLOT,
   0  as Remaindate,
   C.ekgrp,
   t024.eknam,
   C.dispo,
   t024d.dsnam,
   A.matkl
}
where ( B.kalab > 0 or B.kains > 0 or B.kaspe > 0 ) and z.xchpf = '' and C.bwtty = ''

union all

select from mara as A
                                       inner join marc as z on A.matnr = z.matnr
                                       inner join nsdm_e_mska   as B on z.matnr = B.matnr and z.werks = B.werks
                                       inner join ZCCMM_BATCHFIND as C on B.matnr = C.matnr and B.charg = C.BATCH
                                       inner join t001k as k on B.werks = k.bwkey
                                       inner join mcha as J on B.matnr = J.matnr and B.werks = J.werks and B.charg = J.charg
                                       association [1] to t001w as D on $projection.PLANT = D.werks
                                       association [1] to t001l as E on $projection.PLANT = E.werks
                                                         and $projection.SLOCATION = E.lgort
                                       association [1]   to makt as F  on $projection.MATERIAL = F.matnr
                                                           and F.spras =  $session.system_language
                                       association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
                                       association [1]   to t024d as t024d  on $projection.PLANT = t024d.werks
                                                            and $projection.dispo = t024d.dispo
//                                       association [0..1] to I_ProductUnitsOfMeasure as z on z.Product = A.matnr

{
   key k.bukrs as company,
   key B.werks as PLANT,
   key D.name1 as PLANTNAME,
   key B.lgort as SLOCATION,
   key E.lgobe as SLNAME,
   key cast('' as abap.char(10)) as BP,
   key cast('' as abap.char(35)) as BPNAME,
   key A.matnr as MATERIAL,
   key F.maktx as MATNAME,
   key cast('SO_STOCK' as abap.char(8)) as TYPE,
   key B.charg as BATCH,
   key J.charg as BATCHSOL,
   key J.bwtar as BWTAR,
   A.meins,
   case when B.kalab is null then 0 else B.kalab end as AVAILABLESTOCK,
   case when B.kains is null then 0 else B.kains end as QISTOCK,
   case when B.kaspe is null then 0 else B.kaspe end as BLOCKSTOCK,
   C.lifnr as CHAR_BP,
   C.hsdat as PRDUCTIONDATE,
   C.VFdat as EXPIREDDATE,
   C.LICHN as PRDDUCTLOT,
   C.Zmaker as MAKER,
   C.lwedt,
   C.waterratiO,
   C.potency        ,
   C.net,
   C.custlot,
   case when ( C.VFdat <> '00000000' ) then
   dats_days_between( tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                      $session.client,
                      'NULL' ) ,cast(C.VFdat as abap.dats))
   end  as Remaindate,
   z.ekgrp,
   t024.eknam,
   z.dispo,
   t024d.dsnam,
   A.matkl
}
where ( B.kalab > 0 or B.kains > 0 or B.kaspe > 0 ) and z.xchpf = 'X'
