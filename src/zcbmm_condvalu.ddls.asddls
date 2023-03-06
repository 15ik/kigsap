@AbapCatalog.sqlViewName: 'ZSVBMM_CONDVALU'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MM PO Condition 집계'
define view ZCBMM_CONDVALU as select from I_PricingElement
association [0..1] to I_Currency as _Currency on $projection.currency = _Currency.Currency


 {
    key PricingDocument,
    key PricingDocumentItem,
        PricingProcedureStep, ///
        PricingProcedureCounter,
        ConditionType,
        ConditionAmount,
        ConditionRateValue,
        I_PricingElement.ConditionInactiveReason,


        TransactionCurrency as currency,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='PB00' then ConditionAmount end as PB00,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='PB00' then ConditionRateValue end as PB00_UNITPRICE,

        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='PBXX' then ConditionAmount end as PBXX,
       case  when I_PricingElement.ConditionType='PBXX' then ConditionRateValue end as PBXX_UNITPRICE,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZPRI' then ConditionAmount end as ZPRI,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZCPR' then ConditionAmount end as ZCPR,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZCU1' then ConditionAmount end as ZCU1,
       case  when I_PricingElement.ConditionType='ZCU1' then ConditionRateValue end as ZCU1_RATE,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZFR1' then ConditionAmount end as ZFR1,
       case  when I_PricingElement.ConditionType='ZFR1' then ConditionRateValue end as ZFR1_RATE,

        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZIN1' then ConditionAmount end as ZIN1,
       case  when I_PricingElement.ConditionType='ZIN1' then ConditionRateValue end as ZIN1_RATE,

        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZOT1' then ConditionAmount end as ZOT1,
       case  when I_PricingElement.ConditionType='ZOT1' then ConditionRateValue end as ZOT1_RATE,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZTAX' then ConditionAmount end as ZTAX,
       case  when I_PricingElement.ConditionType='ZTAX' then ConditionRateValue end as ZTAX_RATE,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='NAVS' then ConditionAmount end as NAVS,
       case  when I_PricingElement.ConditionType='NAVS' then ConditionRateValue end as NAVS_RATE,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUP' then ConditionAmount end as ZSUP,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUP' then ConditionRateValue end as ZSUP_UNITPRICE,

       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUM' then ConditionAmount end as ZSUM,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUM' then ConditionRateValue end as ZSUM_UNITPRICE,
       @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUR' then ConditionAmount end as ZSUR,
        @Semantics.amount.currencyCode: 'Currency'
       case  when I_PricingElement.ConditionType='ZSUR' then ConditionRateValue end as ZSUR_UNITPRICE

}
   where (ConditionType = 'ZPRI' or ConditionType ='ZCPR'or ConditionType ='ZCU1'
          or ConditionType ='ZFR1'or ConditionType ='ZIN1'or ConditionType ='ZOT1'
          or ConditionType ='ZTAX'or ConditionType ='NAVS'
           or ConditionType ='PB00'or ConditionType ='PBXX'
           or ConditionType ='ZSUP'or ConditionType ='ZSUM'
           or ConditionType ='ZSUR' or ConditionType ='ZTR1' )
     and  I_PricingElement.ConditionInactiveReason = ''
