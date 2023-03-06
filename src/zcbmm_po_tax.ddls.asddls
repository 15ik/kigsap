@AbapCatalog.sqlViewName: 'ZSVBMM_PO_TAX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO 세금계산_Basic'
define view ZCBMM_PO_TAX as select from I_PricingElement 
{
    key PricingDocument,
        ConditionAmount,
        TransactionCurrency,
        ConditionInactiveReason
}
    where
      ( ConditionType = 'ZTAX' or  ConditionType = 'NAVS' )
    and ConditionAmount <> 0
    and ConditionInactiveReason is initial
