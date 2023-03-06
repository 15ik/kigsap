@AbapCatalog.sqlViewName: 'ZSVCMMSTOCKREP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-일자 별 수불부'
define view ZCCMMESTOCKREP
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
p_EndDate : sydate,
//BUKRS ADD
P_Bukrs: bukrs
as select from ZCPMMESTOCKREP ( P_StartDate: $parameters.P_StartDate,
                                p_EndDate : $parameters.p_EndDate,
                                P_Bukrs: $parameters.P_Bukrs) as a
                                       association [1] to t001w as B on $projection.werks = B.werks
                                       association [1] to t001l as C on $projection.werks = C.werks
                                                         and $projection.lgort = C.lgort
                                       association [1]   to makt as D  on $projection.matnr = D.matnr
                                                           and D.spras =  $session.system_language
                                       association [1] to lfa1 as E on $projection.LIFNR = E.lifnr
                                       association [1] to makt as makt on makt.matnr = $projection.matnr
                                                          and makt.spras =  $session.system_language

{
 key a.bukrs,
 key a.werks,
 key B.name1,
 key a.lgort,
 key C.lgobe,
 key a.LIFNR,
 key E.name1 as NAME2,
 key a.matnr,
 key makt.maktx,
 key a.matkl,
 key a.charg,
 a.bwtar,
 //a.lbbsa_sid,
 //a.bstaus_sg,
a.initstock,
a.initstock_VAL,
a.initQIstock,
a.initQIstock_VAL,
a.initBLstock,
a.initBLstock_VAL,
a.TRStockSLQty,
a.TRStockSLValue,
a.TRStockPLQty,
a.TRStockPLValue,
a.SITQty,
a.SITValue,


a.GR_PUR_QTY_UN,
a.GR_PUR_QTY_QI,
a.GR_PUR_QTY_BL,
a.GR_PRD_QTY_UN,
a.GR_PRD_QTY_QI,
a.GR_PRD_QTY_BL,
a.GR_DSC_QTY_UN,
a.GR_DSC_QTY_QI,
a.GR_DSC_QTY_BL,
a.GR_STO_QTY_UN,
a.GR_STO_QTY_QI,
a.GR_STO_QTY_BL,
a.GR_STO_QTY_ST,
a.GR_SL_QTY_UN,
a.GR_SL_QTY_QI,
a.GR_SL_QTY_BL,
a.GR_ETC_QTY_UN,
a.GR_ETC_QTY_QI,
a.GR_ETC_QTY_BL,
a.GR_TR_QTY_UN,
a.GR_TR_QTY_QI,
a.GR_TR_QTY_BL,
a.GR_SC_QTY_UN + a.GR_SC_QTY_UN_I as GR_SC_QTY_UN,
a.GR_SC_QTY_QI,
a.GR_SC_QTY_BL,
a.GR_INV_QTY_UN,
a.GR_INV_QTY_QI,
a.GR_INV_QTY_BL,
a.SIT_QTY_UN,
a.SIT_QTY_QI,
a.SIT_QTY_BL,

a.GI_PRD_QTY_UN,
a.GI_PRD_QTY_QI,
a.GI_PRD_QTY_BL,
a.GI_SO_QTY_UN,
a.GI_SO_QTY_QI,
a.GI_SO_QTY_BL,
a.GI_STO_QTY_UN,
a.GI_STO_QTY_QI,
a.GI_STO_QTY_BL,
a.GI_STO_QTY_ST,
a.GI_SL_QTY_UN,
a.GI_SL_QTY_QI,
a.GI_SL_QTY_BL,
a.GI_ETC_QTY_UN,
a.GI_ETC_QTY_QI,
a.GI_ETC_QTY_BL,
a.GI_TR_QTY_UN,
a.GI_TR_QTY_QI,
a.GI_TR_QTY_BL,
a.GI_SC_QTY_UN,
a.GI_SC_QTY_QI,
a.GI_SC_QTY_BL,
a.GI_RETS_QTY_UN,
a.GI_RETS_QTY_QI,
a.GI_RETS_QTY_BL,
a.GI_RETM_QTY_UN,
a.GI_RETM_QTY_QI,
a.GI_RETM_QTY_BL,
a.GI_INV_QTY_UN,
a.GI_INV_QTY_QI,
a.GI_INV_QTY_BL,


meins,


a.GR_PUR_VAL_UN,
a.GR_PUR_VAL_QI,
a.GR_PUR_VAL_BL,
a.GR_PRD_VAL_UN,
a.GR_PRD_VAL_QI,
a.GR_PRD_VAL_BL,
a.GR_DSC_VAL_UN,
a.GR_DSC_VAL_QI,
a.GR_DSC_VAL_BL,
a.GR_STO_VAL_UN,
a.GR_STO_VAL_QI,
a.GR_STO_VAL_BL,
a.GR_STO_VAL_ST,
a.GR_SL_VAL_UN,
a.GR_SL_VAL_QI,
a.GR_SL_VAL_BL,
a.GR_ETC_VAL_UN,
a.GR_ETC_VAL_QI,
a.GR_ETC_VAL_BL,
a.GR_TR_VAL_UN,
a.GR_TR_VAL_QI,
a.GR_TR_VAL_BL,
a.GR_SC_VAL_UN,
a.GR_SC_VAL_QI,
a.GR_SC_VAL_BL,
a.GR_INV_VAL_UN,
a.GR_INV_VAL_QI,
a.GR_INV_VAL_BL,
a.SIT_VAL_UN,
a.SIT_VAL_QI,
a.SIT_VAL_BL,

a.GI_PRD_VAL_UN,
a.GI_PRD_VAL_QI,
a.GI_PRD_VAL_BL,
a.GI_SO_VAL_UN,
a.GI_SO_VAL_QI,
a.GI_SO_VAL_BL,
a.GI_STO_VAL_UN,
a.GI_STO_VAL_QI,
a.GI_STO_VAL_BL,
a.GI_STO_VAL_ST,
a.GI_SL_VAL_UN,
a.GI_SL_VAL_QI,
a.GI_SL_VAL_BL,
a.GI_ETC_VAL_UN,
a.GI_ETC_VAL_QI,
a.GI_ETC_VAL_BL,
a.GI_TR_VAL_UN,
a.GI_TR_VAL_QI,
a.GI_TR_VAL_BL,
a.GI_SC_VAL_UN,
a.GI_SC_VAL_QI,
a.GI_SC_VAL_BL,
a.GI_RETS_VAL_UN,
a.GI_RETS_VAL_QI,
a.GI_RETS_VAL_BL,
a.GI_RETM_VAL_UN,
a.GI_RETM_VAL_QI,
a.GI_RETM_VAL_BL,
a.GI_INV_VAL_UN,
a.GI_INV_VAL_QI,
a.GI_INV_VAL_BL,
waers,

a.SITQty + a.SIT_QTY_UN + a.SIT_QTY_QI + a.SIT_QTY_BL as lastqty_sit,

a.initstock +
a.GR_PUR_QTY_UN +
a.GR_PRD_QTY_UN + a.GR_DSC_QTY_UN + a.GR_SL_QTY_UN + a.GR_TR_QTY_UN + a.GR_SC_QTY_UN + a.GR_SC_QTY_UN_I + a.GR_INV_QTY_UN  + a.GI_RETS_QTY_UN +
a.GR_STO_QTY_UN +
a.GR_ETC_QTY_UN +
//a.STO_ETC_QTY_UN +
a.GI_PRD_QTY_UN + a.GI_SO_QTY_UN +  a.GI_SL_QTY_UN + a.GI_TR_QTY_UN + a.GI_SC_QTY_UN + a.GI_RETM_QTY_UN + a.GI_INV_QTY_UN  +
a.GI_STO_QTY_UN +
a.GI_ETC_QTY_UN  as LASTQTY_UN,

a.initQIstock +
a.GR_PUR_QTY_QI +
a.GR_PRD_QTY_QI + a.GR_DSC_QTY_QI + a.GR_SL_QTY_QI + a.GR_TR_QTY_QI + a.GR_SC_QTY_QI + a.GR_INV_QTY_QI +  a.GI_RETS_QTY_QI +
a.GR_STO_QTY_QI +
a.GR_ETC_QTY_QI +
//a.STO_ETC_QTY_QI +
a.GI_PRD_QTY_QI +
a.GI_SO_QTY_QI +
a.GI_STO_QTY_QI + a.GI_SO_QTY_QI + a.GI_SL_QTY_QI + a.GI_TR_QTY_QI + a.GI_SC_QTY_QI + a.GI_RETM_QTY_QI + a.GI_INV_QTY_QI +
a.GI_ETC_QTY_QI  as LASTQTY_QI,

a.initBLstock +
a.GR_PUR_QTY_BL +
a.GR_PRD_QTY_BL + + a.GR_DSC_QTY_BL + a.GR_SL_QTY_BL + a.GR_TR_QTY_BL + a.GR_SC_QTY_BL + a.GR_INV_QTY_BL  + a.GI_RETS_QTY_BL +
a.GR_STO_QTY_BL +
a.GR_ETC_QTY_BL +
//a.STO_ETC_QTY_BL +
a.GI_PRD_QTY_BL +
a.GI_SO_QTY_BL +
a.GI_STO_QTY_BL + a.GI_SO_QTY_BL + a.GI_SL_QTY_BL + a.GI_TR_QTY_BL + a.GI_SC_QTY_BL + a.GI_RETM_QTY_BL + a.GI_INV_QTY_BL +
a.GI_ETC_QTY_BL  as LASTQTY_BL,

a.GR_PUR_QTY_UN +
a.GR_PRD_QTY_UN + a.GR_DSC_QTY_UN + a.GR_SL_QTY_UN + a.GR_TR_QTY_UN + a.GR_SC_QTY_UN + a.GR_SC_QTY_UN_I + a.GR_INV_QTY_UN  + a.GI_RETS_QTY_UN +
a.GR_STO_QTY_UN +
a.GR_ETC_QTY_UN as LASTQTY_GRUN,

a.GI_PRD_QTY_UN + a.GI_SO_QTY_UN +  a.GI_SL_QTY_UN + a.GI_TR_QTY_UN + a.GI_SC_QTY_UN + a.GI_RETM_QTY_UN + a.GI_INV_QTY_UN  +
a.GI_STO_QTY_UN +
a.GI_ETC_QTY_UN  as LASTQTY_GIUN,

a.GR_PUR_QTY_QI +
a.GR_PRD_QTY_QI + a.GR_DSC_QTY_QI + a.GR_SL_QTY_QI + a.GR_TR_QTY_QI + a.GR_SC_QTY_QI + a.GR_INV_QTY_QI +  a.GI_RETS_QTY_QI +
a.GR_STO_QTY_QI +
a.GR_ETC_QTY_QI as LASTQTY_GRQI,

a.GI_PRD_QTY_QI +
a.GI_SO_QTY_QI +
a.GI_STO_QTY_QI + a.GI_SO_QTY_QI + a.GI_SL_QTY_QI + a.GI_TR_QTY_QI + a.GI_SC_QTY_QI + a.GI_RETM_QTY_QI + a.GI_INV_QTY_QI +
a.GI_ETC_QTY_QI  as LASTQTY_GIQI,

a.GR_PUR_QTY_BL +
a.GR_PRD_QTY_BL + + a.GR_DSC_QTY_BL + a.GR_SL_QTY_BL + a.GR_TR_QTY_BL + a.GR_SC_QTY_BL + a.GR_INV_QTY_BL  + a.GI_RETS_QTY_BL +
a.GR_STO_QTY_BL +
a.GR_ETC_QTY_BL as LASTQTY_GRBL,

a.GI_PRD_QTY_BL +
a.GI_SO_QTY_BL +
a.GI_STO_QTY_BL + a.GI_SO_QTY_BL + a.GI_SL_QTY_BL + a.GI_TR_QTY_BL + a.GI_SC_QTY_BL + a.GI_RETM_QTY_BL + a.GI_INV_QTY_BL +
a.GI_ETC_QTY_BL  as LASTQTY_GIBL

} where
a.initstock > 0 or a.initQIstock > 0 or a.initBLstock > 0 or a.TRStockSLQty > 0 or  a.TRStockPLQty > 0 or a.SITQty > 0 or
a.GR_PUR_QTY_UN > 0 or a.GR_PUR_QTY_QI > 0 or a.GR_PUR_QTY_BL > 0 or a.GR_PRD_QTY_UN >0 or a.GR_PRD_QTY_QI > 0 or a.GR_PRD_QTY_BL > 0 or
a.GR_DSC_QTY_UN > 0 or a.GR_DSC_QTY_QI > 0 or a.GR_DSC_QTY_BL > 0 or a.GR_STO_QTY_UN > 0 or a.GR_STO_QTY_QI > 0 or a.GR_STO_QTY_BL > 0 or
a.GR_STO_QTY_ST > 0 or a.GR_SL_QTY_UN > 0 or a.GR_SL_QTY_QI > 0 or a.GR_SL_QTY_BL > 0 or a.GR_ETC_QTY_UN > 0 or a.GR_ETC_QTY_QI > 0 or
a.GR_ETC_QTY_BL > 0 or a.GR_TR_QTY_UN > 0 or a.GR_TR_QTY_QI > 0 or a.GR_TR_QTY_BL > 0 or a.GR_SC_QTY_UN > 0 or a.GR_SC_QTY_QI > 0 or
a.GR_SC_QTY_BL > 0 or a.GR_INV_QTY_UN > 0 or a.GR_INV_QTY_QI > 0 or a.GR_INV_QTY_BL > 0 or a.SIT_QTY_UN > 0 or a.SIT_QTY_QI > 0 or
a.SIT_QTY_BL > 0 or
a.GI_PRD_QTY_UN > 0 or a.GI_PRD_QTY_QI > 0 or a.GI_PRD_QTY_BL > 0 or a.GI_SO_QTY_UN > 0 or a.GI_SO_QTY_QI > 0 or a.GI_SO_QTY_BL > 0 or
a.GI_STO_QTY_UN > 0 or a.GI_STO_QTY_QI > 0 or a.GI_STO_QTY_BL > 0 or a.GI_STO_QTY_ST > 0 or a.GI_SL_QTY_UN > 0 or a.GI_SL_QTY_QI > 0 or
a.GI_SL_QTY_BL > 0 or  a.GI_ETC_QTY_UN > 0 or a.GI_ETC_QTY_QI > 0 or a.GI_ETC_QTY_BL > 0 or a.GI_TR_QTY_UN > 0 or  a.GI_TR_QTY_QI > 0 or
a.GI_TR_QTY_BL > 0 or a.GI_SC_QTY_UN > 0 or a.GI_SC_QTY_QI > 0 or a.GI_SC_QTY_BL > 0  or a.GI_RETS_QTY_UN > 0 or a.GI_RETS_QTY_QI > 0 or
a.GI_RETS_QTY_BL > 0 or a.GI_RETM_QTY_UN > 0 or a.GI_RETM_QTY_QI > 0 or a.GI_RETM_QTY_BL > 0 or a.GI_INV_QTY_UN > 0 or a.GI_INV_QTY_QI > 0 or
a.GI_INV_QTY_BL > 0
