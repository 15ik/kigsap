@AbapCatalog.sqlViewName: 'ZSVCMM_PO_TOTAL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매오더 가격 총합'
define view ZCCMM_PO_TOTAL as select from ZCCMM_PRICE
            inner join ekko on ZCCMM_PRICE.ebeln = ekko.ebeln
            association [1] to lfa1 as lfa1 on $projection.lifnr = lfa1.lifnr

 { key ZCCMM_PRICE.ebeln,
    sum (ZCCMM_PRICE.brtwr) as totval,
     ekko.lifnr,
     lfa1.name1,
     ekko.kdatb,
     ekko.kdate,
     ekko.bsart,
     ekko.zterm

 } 
group by ZCCMM_PRICE.ebeln,ekko.lifnr,ekko.kdatb,ekko.kdate,ekko.bsart,ekko.zterm,lfa1.name1
