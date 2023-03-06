@AbapCatalog.sqlViewName: 'ZSVPMMMATDOCAG1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-기간 수불 Aggregation1'
define view ZCPMMMATDOCAG1
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate
as select from ZCPMMMATDOCMVT (P_StartDate: $parameters.P_StartDate,
                                             P_EndDate : $parameters.P_EndDate)
{
    key bukrs,
    key werks,
    key lgort,
    key matnr,
    key charg,
    key budat,
    key LIFNR,
    key bwtar,
    lbbsa_sid,
    bstaus_sg,
    bstaus_cg,
    bwart,

    kzbew,
    movegroup,
    case movegroup when 'GR_PUR' then stock_qty end as GR_PUR_QTY,
    case movegroup when 'GR_PRD' then stock_qty end as GR_PRD_QTY,
    case movegroup when 'GR_DSC' then stock_qty end as GR_DSC_QTY,
    case movegroup when 'GR_STO' then stock_qty end as GR_STO_QTY,
    case movegroup when 'GR_SL' then stock_qty end as GR_SL_QTY,
    case when movegroup = 'GR_ETC' or movegroup = 'GR_TRS' or movegroup = 'GR_CU' then stock_qty end  as GR_ETC_QTY,
    case when movegroup = 'GR_TR' or movegroup = 'GR_321' or movegroup = 'GR_322' or movegroup = 'GR_343' or movegroup = 'GR_344'
                   then stock_qty end as GR_TR_QTY,
    case movegroup when 'GR_SC' then stock_qty end as GR_SC_QTY,
    case movegroup when 'GR_INV' then stock_qty end as GR_INV_QTY,
    case movegroup when 'SIT' then stock_qty end as SIT_QTY,

    case movegroup when 'STO_ETC' then stock_qty end as STO_ETC_QTY,

    case movegroup when 'GI_PRD' then stock_qty end as GI_PRD_QTY,
    case movegroup when 'GI_SO' then stock_qty end as GI_SO_QTY,
    case movegroup when 'GI_STO' then stock_qty end as GI_STO_QTY,
    case movegroup when 'GI_SL' then stock_qty end as GI_SL_QTY,
    case when  movegroup = 'GI_ETC' or movegroup = 'GI_TRS' or movegroup = 'GI_CU' then stock_qty end as GI_ETC_QTY,
    case when movegroup = 'GI_TR' or movegroup = 'GI_321' or movegroup = 'GI_322' or movegroup = 'GI_343' or movegroup = 'GI_344'
                   then stock_qty end as GI_TR_QTY,
    case movegroup when 'GI_SC' then stock_qty end as GI_SC_QTY,
    case movegroup when 'GI_RETS' then stock_qty end as GI_RETS_QTY,
    case movegroup when 'GI_RETM' then stock_qty end as GI_RETM_QTY,
    case movegroup when 'GI_INV' then stock_qty end as GI_INV_QTY,
    meins,
    case movegroup when 'GR_PUR' then dmbtr end as GR_PUR_VAL,
    case movegroup when 'GR_PRD' then dmbtr end as GR_PRD_VAL,
    case movegroup when 'GR_DSC' then dmbtr end as GR_DSC_VAL,
    case movegroup when 'GR_STO' then dmbtr end as GR_STO_VAL,
    case movegroup when 'GR_SL' then dmbtr end as GR_SL_VAL,
    case when movegroup = 'GR_ETC' or movegroup = 'GR_TRS' or movegroup = 'GR_CU'  then dmbtr end as GR_ETC_VAL,
    case when movegroup = 'GR_TR' or movegroup = 'GR_321' or movegroup = 'GR_322' or movegroup = 'GR_343' or movegroup = 'GR_344'
                   then dmbtr end as GR_TR_VAL,
    case movegroup when 'GR_SC' then dmbtr end as GR_SC_VAL,
    case movegroup when 'GR_INV' then dmbtr end as GR_INV_VAL,
    case movegroup when 'SIT' then dmbtr end as SIT_VAL,
    case movegroup when 'STO_ETC' then dmbtr end as STO_ETC_VAL,
    case movegroup when 'GI_PRD' then dmbtr end as GI_PRD_VAL,
    case movegroup when 'GI_SO' then dmbtr end as GI_SO_VAL,
    case movegroup when 'GI_STO' then dmbtr end as GI_STO_VAL,
    case movegroup when 'GI_SL' then dmbtr end as GI_SL_VAL,
    case when  movegroup = 'GI_ETC' or movegroup = 'GI_TRS' or movegroup = 'GI_CU' then dmbtr end as GI_ETC_VAL,
    case when movegroup = 'GI_TR' or movegroup = 'GI_321' or movegroup = 'GI_322' or movegroup = 'GI_343' or movegroup = 'GI_344'
                  then dmbtr end as GI_TR_VAL,
    case movegroup when 'GI_SC' then dmbtr end as GI_SC_VAL,
    case movegroup when 'GI_RETS' then dmbtr end as GI_RETS_VAL,
    case movegroup when 'GI_RETM' then dmbtr end as GI_RETM_VAL,
    case movegroup when 'GI_INV' then dmbtr end as GI_INV_VAL,
    waers
}
