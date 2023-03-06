@AbapCatalog.sqlViewName: 'ZSVPMMMATDOCAG2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-기간 수불 Aggregation2'
define view ZCPMMMATDOCAG2
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate
as select from ZCPMMMATDOCAG1 (P_StartDate: $parameters.P_StartDate,
                                 P_EndDate : $parameters.P_EndDate)

{
    key bukrs,
    key werks,
    key lgort,
    key matnr,
    key charg,
    key bwtar,
    key  LIFNR ,
    lbbsa_sid,
    bstaus_sg,
    bstaus_cg,

    sum(GR_PUR_QTY) as GR_PUR_QTY,
    sum(GR_PRD_QTY) as GR_PRD_QTY,
    sum(GR_DSC_QTY) as GR_DSC_QTY,
    sum(GR_STO_QTY) as GR_STO_QTY,
    sum(GR_SL_QTY) as GR_SL_QTY,
    sum(GR_ETC_QTY) as GR_ETC_QTY,
    sum(GR_TR_QTY) as GR_TR_QTY,
    sum(GR_SC_QTY) as GR_SC_QTY,
    sum(GR_INV_QTY) as GR_INV_QTY,
    sum(SIT_QTY) as SIT_QTY,
    sum(STO_ETC_QTY) as STO_ETC_QTY,

    sum(GI_PRD_QTY) as GI_PRD_QTY,
    sum(GI_SO_QTY) as GI_SO_QTY,
    sum(GI_STO_QTY) as GI_STO_QTY,
    sum(GI_SL_QTY) as GI_SL_QTY,
    sum(GI_ETC_QTY) as GI_ETC_QTY,
    sum(GI_TR_QTY) as GI_TR_QTY,
    sum(GI_SC_QTY) as GI_SC_QTY,
    sum(GI_RETS_QTY) as GI_RETS_QTY,
    sum(GI_RETM_QTY) as GI_RETM_QTY,
    sum(GI_INV_QTY) as GI_INV_QTY,
    meins,

    sum(GR_PUR_VAL) as GR_PUR_VAL,
    sum(GR_PRD_VAL) as GR_PRD_VAL,
    sum(GR_DSC_VAL) as GR_DSC_VAL,
    sum(GR_STO_VAL) as GR_STO_VAL,
    sum(GR_SL_VAL) as GR_SL_VAL,
    sum(GR_ETC_VAL) as GR_ETC_VAL,
    sum(GR_TR_VAL) as GR_TR_VAL,
    sum(GR_SC_VAL) as GR_SC_VAL,
    sum(GR_INV_VAL) as GR_INV_VAL,
    sum(SIT_VAL) as SIT_VAL,
    sum(STO_ETC_VAL) as STO_ETC_VAL,
    sum(GI_PRD_VAL) as GI_PRD_VAL,
    sum(GI_SO_VAL) as GI_SO_VAL,
    sum(GI_STO_VAL) as GI_STO_VAL,
    sum(GI_SL_VAL) as GI_SL_VAL,
    sum(GI_ETC_VAL) as GI_ETC_VAL,
    sum(GI_TR_VAL) as GI_TR_VAL,
    sum(GI_SC_VAL) as GI_SC_VAL,
    sum(GI_RETS_VAL) as GI_RETS_VAL,
    sum(GI_RETM_VAL) as GI_RETM_VAL,
    sum(GI_INV_VAL) as GI_INV_VAL,
    waers
}

group by bukrs,werks,lgort,matnr,charg,lbbsa_sid,bstaus_sg,meins,waers,LIFNR,bwtar,bstaus_cg
