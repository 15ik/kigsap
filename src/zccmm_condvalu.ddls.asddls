@AbapCatalog.sqlViewName: 'ZSVCMM_CONDVALU'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'MM PO Condition 집계_C'
define view ZCCMM_CONDVALU as select from ZCBMM_CONDVALU

{

    key PricingDocument,
    key PricingDocumentItem,

    max(currency) as currency,
    sum(ZCPR) as ZCPR,
    sum(ZPRI) as ZPRI,
    sum(ZCU1) as ZCU1,
    sum(ZCU1_RATE) as ZCU1_RATE,
    sum(ZFR1) as ZFR1,
    sum(ZFR1_RATE) as ZFR1_RATE,
    sum(ZIN1) as ZIN1,
    sum(ZIN1_RATE) as ZIN1_RATE,
    sum(ZOT1) as ZOT1,
    sum(ZOT1_RATE) as ZOT1_RATE,
    sum(ZTAX) as ZTAX,
    sum(ZTAX_RATE) as ZTAX_RATE,
    sum(NAVS) as NAVS,
    sum(NAVS_RATE) as NAVS_RATE,
    sum(ZSUP) as ZSUP,
    sum(ZSUP_UNITPRICE) as ZSUP_UNITPRICE,
    sum(ZSUM) as ZSUM,
    sum(ZSUM_UNITPRICE) as ZSUM_UNITPRICE,
    sum(ZSUR) as ZSUR,
    sum(ZSUR_UNITPRICE) as ZSUR_UNITPRICE,
    max(PB00) as PB00,
    max(PB00_UNITPRICE) as PB00_UNITPRICE,
    max(PBXX) as PBXX,
    max(PBXX_UNITPRICE) as PBXX_UNITPRICE

}

   group by PricingDocument,PricingDocumentItem
