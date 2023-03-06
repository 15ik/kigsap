@AbapCatalog.sqlViewName: 'ZSVBMM_DOWNPAY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '선급금 현황_basic'
define view zcbmm_downpay as select from C_APProcFlowPurOrdHist as a

        left outer join zcbmm_downpayt as B
            on a.ProcessFlowNodeDocument = B.belnr
            and a.PurchaseOrder = B.PurchaseOrder

      association [0..1] to I_Currency as _Currency on $projection.Currency = _Currency.Currency


  { key a.PurchaseOrder,
        a.ProcessFlowNodeDocument,
        a.Currency,

     @Semantics.amount.currencyCode: 'Currency'
     case  when a.ProcessFlowNodeDocCategory='A' then PurchaseOrderAmount end as downpay_req,
     @Semantics.amount.currencyCode: 'Currency'
     case  when a.ProcessFlowNodeDocCategory='4' then PurchaseOrderAmount end as downpay_amount,
     @Semantics.amount.currencyCode: 'Currency'
     case  when B.ProcessFlowNodeDocCategory='4' then B.TAX_AMOUNT end as downpay_tax,  //선급급 부가세
     @Semantics.amount.currencyCode: 'Currency'
     case  when a.ProcessFlowNodeDocCategory='C' then PurchaseOrderAmount end as downpay_clear,
      @Semantics.amount.currencyCode: 'Currency'
     case  when B.ProcessFlowNodeDocCategory='C' then B.TAX_AMOUNT end as clear_tax  // 선급반제 부가세


}
    where (a.ProcessFlowNodeDocCategory = '4' or a.ProcessFlowNodeDocCategory = 'C' or a.ProcessFlowNodeDocCategory = 'A'   )
