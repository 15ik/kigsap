@AbapCatalog.sqlViewName: 'ZSVCMMPR_RELEASE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매요청 릴리즈 상태'
define view ZCCMMPR_RELEASE as select from eban
            association [1]   to t161t as T161T  on $projection.bsart = T161T.bsart
            and t161t.spras = $session.system_language
            and t161t.bstyp ='B'
            association [1]   to iplant as iplant  on $projection.werks = iplant.plant
            association [1]   to imatgrouptext as IMATGROUPTEXT  on $projection.matkl = imatgrouptext.materialgroup
            and imatgrouptext.language = $session.system_language
            association [1]   to t024 as t024  on $projection.ekgrp = t024.ekgrp
            association [1]   to lfa1 as lfa1  on $projection.lifnr = lfa1.lifnr
            association [1]   to t001l as t001l  on $projection.werks = t001l.werks
            and $projection.lgort = t001l.lgort
            association [1]   to t001k as t001k  on $projection.werks = t001k.bwkey
            association [1]   to ZCCMM_EKET_PR as ZCCMM_EKET_PR  on $projection.banfn = ZCCMM_EKET_PR.banfn
            and $projection.bnfpo = ZCCMM_EKET_PR.bnfpo
            association [0..1] to I_Currency  as _Currency on  $projection.PurReqnItemCurrency = _Currency.Currency
 {

    key  eban.banfn,
         bnfpo,
         bsart,
         t161t.batxt,
         loekz,
         statu,
         estkz,
         frgkz,
         frgzu,
         frgst,

         ( case when frgst='01' and frgzu='X X'     then '접수대기'
                when frgst='01' and frgzu='X X X'     then '접수완료'
                when frgst='01' and frgzu='X XX'     then '접수반려'
                when frgst='01' and frgzu=''     then '결재대기'
                when frgst='01' and frgzu='X'     then '상신중'
                when frgst='01' and frgzu='XX'     then '결재반려'
                when frgst='02' and frgzu=''     then '생성중'
                when frgst='02' and frgzu='X'     then '접수대기'
                when frgst='02' and frgzu='X X'     then '접수완료'
                when frgst='02' and frgzu='XX'     then '접수반려'
                when frgst='03' and frgzu=''     then '접수대기'
                when frgst='03' and frgzu='X'     then '접수완료'
                when frgst='03' and frgzu=' X'     then '접수반려'
                when frgst='04' and frgzu=''     then '결재대기'
                when frgst='04' and frgzu='X'     then '상신중'
                when frgst='04' and frgzu='XX'     then '승인완료'
                when frgst='04' and frgzu='X X'     then '반려'
                when frgst='05' and frgzu=''     then '생성중'
                when frgst='05' and frgzu='X'     then '접수대기'
                when frgst='05' and frgzu='XX'     then '접수반려'
                when frgst='05' and frgzu='X X'     then '접수완료'

                when frgst=''  then '접수완료'

            else '결재중' end ) as wf_staus,
                ///01  수작업 생성 PR 결재 : 전자결재
                ///02  MRP 생성 PR 접수 : 전자결재없음
                ///03  PM/PS 생성 PR 접수 : 전자결재없음
                ///04  MRO 릴리즈 절차 : 전자결재
                ///05  영진 월발주 구매요청 : 전자결재 없음

         ekgrp,
         t024.eknam,
         afnam,
         matnr,
         txz01,
         bwtar,
         pstyp,
         werks,
         iplant.plantname,
         lgort,
         t001l.lgobe,
         bednr,
         matkl,
         imatgrouptext.materialgroupname,
         menge,
         meins,
         bumng,
         badat,
         lpein,
         lfdat,
         frgdt,
         webaz,
         preis,
         waers as PurReqnItemCurrency,
         peinh,
         knttp,
         lifnr,
         lfa1.name1,
         flief,
         infnr,
         zugba,
         dispo,
         ebeln,
         ebelp,
         bedat,
         bsmng,
         @Semantics.quantity.unitOfMeasure: 'meins'
         menge-bsmng as open_pr_qty,
         @Semantics.quantity.unitOfMeasure: 'meins'
         zccmm_eket_pr.GR_QTY,
         ebakz,
       ( case when estkz='U' or estkz='B' then 'MRP'
              when estkz='R' then '수작업'
              when estkz= 'F'or estkz='V' then 'PM/PS/SD'
               else '기타' end ) as CREAT_SOURCE,
                        fixkz,
         t001k.bukrs
}
