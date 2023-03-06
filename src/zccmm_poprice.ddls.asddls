@AbapCatalog.sqlViewName: 'ZSVCMM_POPRICE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO PRICE_basic'
define view ZCCMM_POPRICE as select from ekko
      left outer join ZCCMM_CONDVALU
      on ekko.knumv = ZCCMM_CONDVALU.PricingDocument
      association [0..1] to I_Currency as _Currency on $projection.currency = _Currency.Currency



   {
      key ekko.ebeln,
      key right(PricingDocumentItem,5) as ebelp,
       bstyp,

       currency,
        ZCPR,
        ZPRI,
        ZCU1,
        ZCU1_RATE,
        ZFR1,
        ZFR1_RATE,
        ZIN1,
        ZIN1_RATE,
        ZOT1,
        ZOT1_RATE,
        ZTAX,
        ZTAX_RATE,
        NAVS,
        NAVS_RATE,
        @Semantics.amount.currencyCode: 'Currency'
        ZSUP, //금액
        @Semantics.amount.currencyCode: 'Currency'
        ZSUP_UNITPRICE, // 단가
        @Semantics.amount.currencyCode: 'Currency'
        ZSUM, //금액
        @Semantics.amount.currencyCode: 'Currency'
        ZSUM_UNITPRICE, // 단가
        @Semantics.amount.currencyCode: 'Currency'
        ZSUR, //금액
        @Semantics.amount.currencyCode: 'Currency'
        ZSUR_UNITPRICE, // 단가
        @Semantics.amount.currencyCode: 'Currency'
        PB00, //금액
        @Semantics.amount.currencyCode: 'Currency'
        PB00_UNITPRICE, //단가
        @Semantics.amount.currencyCode: 'Currency'
        PBXX, //금액
        @Semantics.amount.currencyCode: 'Currency'
        PBXX_UNITPRICE // 단가
    }
