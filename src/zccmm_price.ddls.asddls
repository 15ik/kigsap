@AbapCatalog.sqlViewName: 'ZSVCMM_PRICE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO 가격 구성'
define view ZCCMM_PRICE as select from ZCCMM_POPRICE
   left outer join ekpo
    on ZCCMM_POPRICE.ebeln = ekpo.ebeln
    and ZCCMM_POPRICE.ebelp = ekpo.ebelp
    association [0..1] to I_Currency as _Currency on $projection.currency = _Currency.Currency
{
    key ekpo.ebeln,
    key ekpo.ebelp,
    ZCCMM_POPRICE.bstyp,
    matnr,
    txz01,
    bwtar,
    menge,
    meins,
    currency,
    peinh,
    @Semantics.amount.currencyCode: 'Currency'
    case ekpo.retpo
        when 'X' then (ekpo.netpr * (-1) )
        else ekpo.netpr
        end as netpr,


    @Semantics.amount.currencyCode: 'Currency'
     case ekpo.retpo
        when 'X' then (ekpo.brtwr * (-1) )
        else ekpo.brtwr
        end as brtwr,




    retpo,
    mwskz,
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
