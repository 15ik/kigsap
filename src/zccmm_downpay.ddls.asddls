@AbapCatalog.sqlViewName: 'ZSVCMM_DOWNPAY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '선급금 현황'
define view zccmm_downpay as select from zcbmm_downpay as a
        association [0..1] to I_Currency as _Currency on $projection.Currency = _Currency.Currency


{ key a.PurchaseOrder,
      a.Currency,

     @Semantics.amount.currencyCode: 'Currency'
     sum(a.downpay_req) as downpay_req,
     @Semantics.amount.currencyCode: 'Currency'
     sum(a.downpay_amount) as downpay_amount,

     @Semantics.amount.currencyCode: 'Currency'
     sum(a.downpay_tax) as downpay_tax,

     @Semantics.amount.currencyCode: 'Currency'
     sum(a.downpay_clear) as downpay_clear,

     @Semantics.amount.currencyCode: 'Currency'
     sum(a.clear_tax) as clear_tax


}
   group by a.PurchaseOrder,a.Currency
