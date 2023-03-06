@AbapCatalog.sqlViewName: 'ZSVBMM_DOWNPAYT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '선급금관련 부가세 정보'
define view zcbmm_downpayt as select from C_APProcFlowPurOrdHist as A
    left outer join bseg as B
    on A.CompanyCode = B.bukrs
    and A.FiscalYear = B.gjahr and A.ProcessFlowNodeDocument = B.belnr
    association [0..1] to I_Currency as _Currency on $projection.pswsl = _Currency.Currency


    {
    key B.belnr,
    key B.gjahr,
    key B.bukrs,


       A.PurchaseOrder,
       A.ProcessFlowNodeDocCategory,
       B.pswsl as CURRENCY,
       B.mwskz,
       B.bupla,

      case B.shkzg
        when 'S' then B.wrbtr
        else (B.wrbtr * (-1) )
        end as TAX_AMOUNT,

        case B.shkzg
        when 'S' then B.fwbas
        else (B.fwbas * (-1) )
        end as BASE_AMOUNT


}
   where (A.ProcessFlowNodeDocCategory = '4' or A.ProcessFlowNodeDocCategory = 'C')
   // and B.hkont = '0011110201'
