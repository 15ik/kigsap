@AbapCatalog.sqlViewName: 'ZSVCMM_EKET_PR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매요청기준 입고 집계'
define view ZCCMM_EKET_PR as select from eket {

    key banfn,
    key bnfpo,
    sum(wemng) as GR_QTY

}
    where banfn <> ''
    group by banfn,bnfpo
