@AbapCatalog.sqlViewName: 'ZSVPMMSTOCKREP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-일별 수불 DATA Aggregation'
define view ZCPMMESTOCKREP
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
p_EndDate : sydate,
//BUKRS ADD
P_Bukrs: bukrs
as select from ZCCMMEDAYSTOCK(P_Language: $session.system_language,
                              P_EndDate : $parameters.P_StartDate,
                              P_Bukrs: $parameters.P_Bukrs) as A
                            left outer join   ZCPMMMATDOCAG3 (P_StartDate: $parameters.P_StartDate,
                                                            p_EndDate : $parameters.p_EndDate)  as B
                                            on A.bukrs = B.bukrs
                                           and A.werks = B.werks
                                           and A.lgort = B.lgort
                                           and A.matnr = B.matnr
                                           and A.charg = B.charg
                                           and A.LIFNR = B.LIFNR
                                           and A.bwtar = B.bwtar

 {

 key A.bukrs,
 key A.werks,
 key A.lgort,
 key A.matnr,
 key A.charg,
 key A.LIFNR,
 key A.matkl,
 B.LIFNR as lifnr2,
 A.bwtar,

 //INITIAL STOCK
 case when A.CurrentStock is null then 0 else A.CurrentStock end as initstock,
 case when A.CurrentStockCurVal is null then 0 else A.CurrentStockCurVal end as initstock_VAL,
 case when A.QIStockQty is null then 0 else A.QIStockQty end as initQIstock,
 case when A.QIStockCurVal is null then 0 else A.QIStockCurVal end as initQIstock_VAL,
 case when A.BLStockQTY is null then 0 else A.BLStockQTY end as initBLstock,
 case when A.BLStockValue is null then 0 else A.BLStockValue end as initBLstock_VAL,
 case when A.TRStockSLQty is null then 0 else A.TRStockSLQty end as TRStockSLQty,
 case when A.TRStockSLValue is null then 0 else A.TRStockSLValue end as TRStockSLValue,
 case when A.TRStockPLQty is null then 0 else A.TRStockPLQty end as TRStockPLQty,
 case when A.TRStockPLValue is null then 0 else A.TRStockPLValue end as TRStockPLValue,
 case when A.SITQty is null then 0 else A.SITQty end as SITQty,
 case when A.SITValue is null then 0 else A.SITValue end as SITValue,


case when B.GR_PUR_QTY_UN is null then 0 else B.GR_PUR_QTY_UN end as GR_PUR_QTY_UN,
case when B.GR_PUR_QTY_QI is null then 0 else B.GR_PUR_QTY_QI end as GR_PUR_QTY_QI,
case when B.GR_PUR_QTY_BL is null then 0 else B.GR_PUR_QTY_BL end as GR_PUR_QTY_BL,
case when B.GR_PRD_QTY_UN is null then 0 else B.GR_PRD_QTY_UN end as GR_PRD_QTY_UN,
case when B.GR_PRD_QTY_QI is null then 0 else B.GR_PRD_QTY_QI end as GR_PRD_QTY_QI,
case when B.GR_PRD_QTY_BL is null then 0 else B.GR_PRD_QTY_BL end as GR_PRD_QTY_BL,

case when B.GR_DSC_QTY_UN is null then 0 else B.GR_DSC_QTY_UN end as GR_DSC_QTY_UN,
case when B.GR_DSC_QTY_QI is null then 0 else B.GR_DSC_QTY_QI end as GR_DSC_QTY_QI,
case when B.GR_DSC_QTY_BL is null then 0 else B.GR_DSC_QTY_BL end as GR_DSC_QTY_BL,

case when B.GR_STO_QTY_UN is null then 0 else B.GR_STO_QTY_UN end as GR_STO_QTY_UN,
case when B.GR_STO_QTY_QI is null then 0 else B.GR_STO_QTY_QI end as GR_STO_QTY_QI,
case when B.GR_STO_QTY_BL is null then 0 else B.GR_STO_QTY_BL end as GR_STO_QTY_BL,
case when B.GR_STO_QTY_ST is null then 0 else B.GR_STO_QTY_ST end as GR_STO_QTY_ST,

case when B.GR_SL_QTY_UN is null then 0 else B.GR_SL_QTY_UN end as GR_SL_QTY_UN,
case when B.GR_SL_QTY_QI is null then 0 else B.GR_SL_QTY_QI end as GR_SL_QTY_QI,
case when B.GR_SL_QTY_BL is null then 0 else B.GR_SL_QTY_BL end as GR_SL_QTY_BL,


case when B.GR_ETC_QTY_UN is null then 0 else B.GR_ETC_QTY_UN end as GR_ETC_QTY_UN,
case when B.GR_ETC_QTY_QI is null then 0 else B.GR_ETC_QTY_QI end as GR_ETC_QTY_QI,
case when B.GR_ETC_QTY_BL is null then 0 else B.GR_ETC_QTY_BL end as GR_ETC_QTY_BL,

case when B.GR_TR_QTY_UN is null then 0 else B.GR_TR_QTY_UN end as GR_TR_QTY_UN,
case when B.GR_TR_QTY_QI is null then 0 else B.GR_TR_QTY_QI end as GR_TR_QTY_QI,
case when B.GR_TR_QTY_BL is null then 0 else B.GR_TR_QTY_BL end as GR_TR_QTY_BL,

case when B.GR_SC_QTY_UN is null then 0 else B.GR_SC_QTY_UN end as GR_SC_QTY_UN,
case when B.GR_SC_QTY_UN_I is null then 0 else B.GR_SC_QTY_UN_I end as GR_SC_QTY_UN_I,
case when B.GR_SC_QTY_QI is null then 0 else B.GR_SC_QTY_QI end as GR_SC_QTY_QI,
case when B.GR_SC_QTY_BL is null then 0 else B.GR_SC_QTY_BL end as GR_SC_QTY_BL,


case when B.GR_INV_QTY_UN is null then 0 else B.GR_INV_QTY_UN end as GR_INV_QTY_UN,
case when B.GR_INV_QTY_QI is null then 0 else B.GR_INV_QTY_QI end as GR_INV_QTY_QI,
case when B.GR_INV_QTY_BL is null then 0 else B.GR_INV_QTY_BL end as GR_INV_QTY_BL,

case when B.SIT_QTY_UN is null then 0 else B.SIT_QTY_UN end as SIT_QTY_UN,
case when B.SIT_QTY_QI is null then 0 else B.SIT_QTY_QI end as SIT_QTY_QI,
case when B.SIT_QTY_BL is null then 0 else B.SIT_QTY_BL end as SIT_QTY_BL,


case when B.GI_PRD_QTY_UN is null then 0 else B.GI_PRD_QTY_UN end as GI_PRD_QTY_UN,
case when B.GI_PRD_QTY_QI is null then 0 else B.GI_PRD_QTY_QI end as GI_PRD_QTY_QI,
case when B.GI_PRD_QTY_BL is null then 0 else B.GI_PRD_QTY_BL end as GI_PRD_QTY_BL,

case when B.GI_SO_QTY_UN is null then 0 else B.GI_SO_QTY_UN end as GI_SO_QTY_UN,
case when B.GI_SO_QTY_QI is null then 0 else B.GI_SO_QTY_QI end as GI_SO_QTY_QI,
case when B.GI_SO_QTY_BL is null then 0 else B.GI_SO_QTY_BL end as GI_SO_QTY_BL,

case when B.GI_STO_QTY_UN is null then 0 else B.GI_STO_QTY_UN end as GI_STO_QTY_UN,
case when B.GI_STO_QTY_QI is null then 0 else B.GI_STO_QTY_QI end as GI_STO_QTY_QI,
case when B.GI_STO_QTY_BL is null then 0 else B.GI_STO_QTY_BL end as GI_STO_QTY_BL,
case when B.GI_STO_QTY_ST is null then 0 else B.GI_STO_QTY_ST end as GI_STO_QTY_ST,

case when B.GI_SL_QTY_UN is null then 0 else B.GI_SL_QTY_UN end as GI_SL_QTY_UN,
case when B.GI_SL_QTY_QI is null then 0 else B.GI_SL_QTY_QI end as GI_SL_QTY_QI,
case when B.GI_SL_QTY_BL is null then 0 else B.GI_SL_QTY_BL end as GI_SL_QTY_BL,


case when B.GI_ETC_QTY_UN is null then 0 else B.GI_ETC_QTY_UN end as GI_ETC_QTY_UN,
case when B.GI_ETC_QTY_QI is null then 0 else B.GI_ETC_QTY_QI end as GI_ETC_QTY_QI,
case when B.GI_ETC_QTY_BL is null then 0 else B.GI_ETC_QTY_BL end as GI_ETC_QTY_BL,

case when B.GI_TR_QTY_UN is null then 0 else B.GI_TR_QTY_UN end as GI_TR_QTY_UN,
case when B.GI_TR_QTY_QI is null then 0 else B.GI_TR_QTY_QI end as GI_TR_QTY_QI,
case when B.GI_TR_QTY_BL is null then 0 else B.GI_TR_QTY_BL end as GI_TR_QTY_BL,

case when B.GI_SC_QTY_UN is null then 0 else B.GI_SC_QTY_UN end as GI_SC_QTY_UN,
case when B.GI_SC_QTY_QI is null then 0 else B.GI_SC_QTY_QI end as GI_SC_QTY_QI,
case when B.GI_SC_QTY_BL is null then 0 else B.GI_SC_QTY_BL end as GI_SC_QTY_BL,

case when B.GI_RETS_QTY_UN is null then 0 else B.GI_RETS_QTY_UN end as GI_RETS_QTY_UN,
case when B.GI_RETS_QTY_QI is null then 0 else B.GI_RETS_QTY_QI end as GI_RETS_QTY_QI,
case when B.GI_RETS_QTY_BL is null then 0 else B.GI_RETS_QTY_BL end as GI_RETS_QTY_BL,

case when B.GI_RETM_QTY_UN is null then 0 else B.GI_RETM_QTY_UN end as GI_RETM_QTY_UN,
case when B.GI_RETM_QTY_QI is null then 0 else B.GI_RETM_QTY_QI end as GI_RETM_QTY_QI,
case when B.GI_RETM_QTY_BL is null then 0 else B.GI_RETM_QTY_BL end as GI_RETM_QTY_BL,

case when B.GI_INV_QTY_UN is null then 0 else B.GI_INV_QTY_UN end as GI_INV_QTY_UN,
case when B.GI_INV_QTY_QI is null then 0 else B.GI_INV_QTY_QI end as GI_INV_QTY_QI,
case when B.GI_INV_QTY_BL is null then 0 else B.GI_INV_QTY_BL end as GI_INV_QTY_BL,


A.MaterialBaseUnit as meins,


case when B.GR_PUR_VAL_UN is null then 0 else B.GR_PUR_VAL_UN end as GR_PUR_VAL_UN,
case when B.GR_PUR_VAL_QI is null then 0 else B.GR_PUR_VAL_QI end as GR_PUR_VAL_QI,
case when B.GR_PUR_VAL_BL is null then 0 else B.GR_PUR_VAL_BL end as GR_PUR_VAL_BL,

case when B.GR_PRD_VAL_UN is null then 0 else B.GR_PRD_VAL_UN end as GR_PRD_VAL_UN,
case when B.GR_PRD_VAL_QI is null then 0 else B.GR_PRD_VAL_QI end as GR_PRD_VAL_QI,
case when B.GR_PRD_VAL_BL is null then 0 else B.GR_PRD_VAL_BL end as GR_PRD_VAL_BL,

case when B.GR_DSC_VAL_UN is null then 0 else B.GR_DSC_VAL_UN end as GR_DSC_VAL_UN,
case when B.GR_DSC_VAL_QI is null then 0 else B.GR_DSC_VAL_QI end as GR_DSC_VAL_QI,
case when B.GR_DSC_VAL_BL is null then 0 else B.GR_DSC_VAL_BL end as GR_DSC_VAL_BL,

case when B.GR_STO_VAL_UN is null then 0 else B.GR_STO_VAL_UN end as GR_STO_VAL_UN,
case when B.GR_STO_VAL_QI is null then 0 else B.GR_STO_VAL_QI end as GR_STO_VAL_QI,
case when B.GR_STO_VAL_BL is null then 0 else B.GR_STO_VAL_BL end as GR_STO_VAL_BL,
case when B.GR_STO_VAL_ST is null then 0 else B.GR_STO_VAL_ST end as GR_STO_VAL_ST,


case when B.GR_SL_VAL_UN is null then 0 else B.GR_SL_VAL_UN end as GR_SL_VAL_UN,
case when B.GR_SL_VAL_QI is null then 0 else B.GR_SL_VAL_QI end as GR_SL_VAL_QI,
case when B.GR_SL_VAL_BL is null then 0 else B.GR_SL_VAL_BL end as GR_SL_VAL_BL,

case when B.GR_ETC_VAL_UN is null then 0 else B.GR_ETC_VAL_UN end as GR_ETC_VAL_UN,
case when B.GR_ETC_VAL_QI is null then 0 else B.GR_ETC_VAL_QI end as GR_ETC_VAL_QI,
case when B.GR_ETC_VAL_BL is null then 0 else B.GR_ETC_VAL_BL end as GR_ETC_VAL_BL,

case when B.GR_TR_VAL_UN is null then 0 else B.GR_TR_VAL_UN end as GR_TR_VAL_UN,
case when B.GR_TR_VAL_QI is null then 0 else B.GR_TR_VAL_QI end as GR_TR_VAL_QI,
case when B.GR_TR_VAL_BL is null then 0 else B.GR_TR_VAL_BL end as GR_TR_VAL_BL,

case when B.GR_SC_VAL_UN is null then 0 else B.GR_SC_VAL_UN end as GR_SC_VAL_UN,
case when B.GR_SC_VAL_QI is null then 0 else B.GR_SC_VAL_QI end as GR_SC_VAL_QI,
case when B.GR_SC_VAL_BL is null then 0 else B.GR_SC_VAL_BL end as GR_SC_VAL_BL,


case when B.GR_INV_VAL_UN is null then 0 else B.GR_INV_VAL_UN end as GR_INV_VAL_UN,
case when B.GR_INV_VAL_QI is null then 0 else B.GR_INV_VAL_QI end as GR_INV_VAL_QI,
case when B.GR_INV_VAL_BL is null then 0 else B.GR_INV_VAL_BL end as GR_INV_VAL_BL,

case when B.SIT_VAL_UN is null then 0 else B.SIT_VAL_UN end as SIT_VAL_UN,
case when B.SIT_VAL_QI is null then 0 else B.SIT_VAL_QI end as SIT_VAL_QI,
case when B.SIT_VAL_BL is null then 0 else B.SIT_VAL_BL end as SIT_VAL_BL,


case when B.GI_PRD_VAL_UN is null then 0 else B.GI_PRD_VAL_UN end as GI_PRD_VAL_UN,
case when B.GI_PRD_VAL_QI is null then 0 else B.GI_PRD_VAL_QI end as GI_PRD_VAL_QI,
case when B.GI_PRD_VAL_BL is null then 0 else B.GI_PRD_VAL_BL end as GI_PRD_VAL_BL,

case when B.GI_SO_VAL_UN is null then 0 else B.GI_SO_VAL_UN end as GI_SO_VAL_UN,
case when B.GI_SO_VAL_QI is null then 0 else B.GI_SO_VAL_QI end as GI_SO_VAL_QI,
case when B.GI_SO_VAL_BL is null then 0 else B.GI_SO_VAL_BL end as GI_SO_VAL_BL,

case when B.GI_STO_VAL_UN is null then 0 else B.GI_STO_VAL_UN end as GI_STO_VAL_UN,
case when B.GI_STO_VAL_QI is null then 0 else B.GI_STO_VAL_QI end as GI_STO_VAL_QI,
case when B.GI_STO_VAL_BL is null then 0 else B.GI_STO_VAL_BL end as GI_STO_VAL_BL,
case when B.GI_STO_VAL_ST is null then 0 else B.GI_STO_VAL_ST end as GI_STO_VAL_ST,


case when B.GI_SL_VAL_UN is null then 0 else B.GI_SL_VAL_UN end as GI_SL_VAL_UN,
case when B.GI_SL_VAL_QI is null then 0 else B.GI_SL_VAL_QI end as GI_SL_VAL_QI,
case when B.GI_SL_VAL_BL is null then 0 else B.GI_SL_VAL_BL end as GI_SL_VAL_BL,

case when B.GI_ETC_VAL_UN is null then 0 else B.GI_ETC_VAL_UN end as GI_ETC_VAL_UN,
case when B.GI_ETC_VAL_QI is null then 0 else B.GI_ETC_VAL_QI end as GI_ETC_VAL_QI,
case when B.GI_ETC_VAL_BL is null then 0 else B.GI_ETC_VAL_BL end as GI_ETC_VAL_BL,

case when B.GI_TR_VAL_UN is null then 0 else B.GI_TR_VAL_UN end as GI_TR_VAL_UN,
case when B.GI_TR_VAL_QI is null then 0 else B.GI_TR_VAL_QI end as GI_TR_VAL_QI,
case when B.GI_TR_VAL_BL is null then 0 else B.GI_TR_VAL_BL end as GI_TR_VAL_BL,

case when B.GI_SC_VAL_UN is null then 0 else B.GI_SC_VAL_UN end as GI_SC_VAL_UN,
case when B.GI_SC_VAL_QI is null then 0 else B.GI_SC_VAL_QI end as GI_SC_VAL_QI,
case when B.GI_SC_VAL_BL is null then 0 else B.GI_SC_VAL_BL end as GI_SC_VAL_BL,

case when B.GI_RETS_VAL_UN is null then 0 else B.GI_RETS_VAL_UN end as GI_RETS_VAL_UN,
case when B.GI_RETS_VAL_QI is null then 0 else B.GI_RETS_VAL_QI end as GI_RETS_VAL_QI,
case when B.GI_RETS_VAL_BL is null then 0 else B.GI_RETS_VAL_BL end as GI_RETS_VAL_BL,

case when B.GI_RETM_VAL_UN is null then 0 else B.GI_RETM_VAL_UN end as GI_RETM_VAL_UN,
case when B.GI_RETM_VAL_QI is null then 0 else B.GI_RETM_VAL_QI end as GI_RETM_VAL_QI,
case when B.GI_RETM_VAL_BL is null then 0 else B.GI_RETM_VAL_BL end as GI_RETM_VAL_BL,

case when B.GI_INV_VAL_UN is null then 0 else B.GI_INV_VAL_UN end as GI_INV_VAL_UN,
case when B.GI_INV_VAL_QI is null then 0 else B.GI_INV_VAL_QI end as GI_INV_VAL_QI,
case when B.GI_INV_VAL_BL is null then 0 else B.GI_INV_VAL_BL end as GI_INV_VAL_BL,

 A.CURRENCY as waers
}
