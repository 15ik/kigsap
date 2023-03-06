@AbapCatalog.sqlViewName: 'ZSVPMMMATDOCAG3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-기말재고 Aggregation3'
define view ZCPMMMATDOCAG3

with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
p_EndDate : sydate
as select from ZCPMMMATDOCAG2  (P_StartDate: $parameters.P_StartDate,
                               P_EndDate : $parameters.p_EndDate) as b
{
 key b.bukrs,
 key b.werks,
 key b.lgort,
 key b.matnr,
 key b.charg,
 key b.bwtar,

 key case when b.LIFNR is null then '' else b.LIFNR end as LIFNR,

 sum(case b.bstaus_sg when 'A' then
    case when b.GR_PUR_QTY is null then 0 else b.GR_PUR_QTY end
 end) as GR_PUR_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
    case when b.GR_PUR_QTY is null then 0 else b.GR_PUR_QTY end
 end) as GR_PUR_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
    case when b.GR_PUR_QTY is null then 0 else b.GR_PUR_QTY end
 end) as GR_PUR_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_PRD_QTY  is null then 0 else b.GR_PRD_QTY end
 end) as GR_PRD_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_PRD_QTY  is null then 0 else b.GR_PRD_QTY end
 end) as GR_PRD_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_PRD_QTY  is null then 0 else b.GR_PRD_QTY end
 end) as GR_PRD_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_DSC_QTY  is null then 0 else b.GR_DSC_QTY end
 end) as GR_DSC_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_DSC_QTY  is null then 0 else b.GR_DSC_QTY end
 end) as GR_DSC_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_DSC_QTY  is null then 0 else b.GR_DSC_QTY end
 end) as GR_DSC_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_STO_QTY  is null then 0 else b.GR_STO_QTY end
 end) as GR_STO_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_STO_QTY  is null then 0 else b.GR_STO_QTY end
 end) as GR_STO_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_STO_QTY  is null then 0 else b.GR_STO_QTY end
 end) as GR_STO_QTY_BL,
 sum(case b.bstaus_sg when 'H' then
      case when b.GR_STO_QTY  is null then 0 else b.GR_STO_QTY end
 end) as GR_STO_QTY_ST,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_SL_QTY  is null then 0 else b.GR_SL_QTY end
 end) as GR_SL_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_SL_QTY  is null then 0 else b.GR_SL_QTY end
 end) as GR_SL_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_SL_QTY  is null then 0 else b.GR_SL_QTY end
 end) as GR_SL_QTY_BL,


 sum(case b.bstaus_sg when 'A' then
      case when b.GR_ETC_QTY  is null then 0 else b.GR_ETC_QTY end
 end) as GR_ETC_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_ETC_QTY  is null then 0 else b.GR_ETC_QTY end
 end) as GR_ETC_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_ETC_QTY  is null then 0 else b.GR_ETC_QTY end
 end) as GR_ETC_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_TR_QTY  is null then 0 else b.GR_TR_QTY end
 end) as GR_TR_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_TR_QTY  is null then 0 else b.GR_TR_QTY end
 end) as GR_TR_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_TR_QTY  is null then 0 else b.GR_TR_QTY end
 end) as GR_TR_QTY_BL,
//[KTGP-79423변경시작 2023.02.22]
//sum(case when b.bstaus_sg = 'Q' and ( b.bstaus_cg = 'A' or b.bstaus_cg = '' ) then
//    case when b.GR_SC_QTY  is null then 0 else b.GR_SC_QTY end
 sum(case when b.bstaus_sg = 'Q' and b.bstaus_cg = 'A' then
      case when b.GR_SC_QTY  is null then 0 else b.GR_SC_QTY end
 end) as GR_SC_QTY_UN,

 sum(case when b.bstaus_sg = 'Q' and b.bstaus_cg = '' then
      case when b.GI_SC_QTY  is null then 0 else b.GI_SC_QTY end
 end) as GR_SC_QTY_UN_I,
 //[KTGP-79423변경종료 2023.02.22]
 sum(case when b.bstaus_sg = 'Q' and b.bstaus_cg = 'B' then
      case when b.GR_SC_QTY  is null then 0 else b.GR_SC_QTY end
 end) as GR_SC_QTY_QI,
 sum(case when b.bstaus_sg = 'Q' and b.bstaus_cg = 'D' then
      case when b.GR_SC_QTY  is null then 0 else b.GR_SC_QTY end
 end) as GR_SC_QTY_BL,


 sum(case when b.bstaus_sg = 'A' and b.bstaus_cg = 'Q' then
      case when b.GR_INV_QTY  is null then 0 else b.GR_INV_QTY end
 end) as GR_INV_QTY_UN,
 sum(case when b.bstaus_sg = 'B' and b.bstaus_cg = 'Q' then
      case when b.GR_INV_QTY  is null then 0 else b.GR_INV_QTY end
 end) as GR_INV_QTY_QI,
 sum(case when b.bstaus_sg = 'D' and b.bstaus_cg = 'Q' then
      case when b.GR_INV_QTY  is null then 0 else b.GR_INV_QTY end
 end) as GR_INV_QTY_BL,

 sum(case b.bstaus_cg when 'A' then
      case when b.SIT_QTY  is null then 0 else b.SIT_QTY end
 end) as SIT_QTY_UN,
 sum(case b.bstaus_cg when 'B' then
      case when b.SIT_QTY  is null then 0 else b.SIT_QTY end
 end) as SIT_QTY_QI,
 sum(case b.bstaus_cg when 'D' then
      case when b.SIT_QTY  is null then 0 else b.SIT_QTY end
 end) as SIT_QTY_BL,


/*
 case B.bstaus_sg when 'A' then
      case when B.STO_ETC_QTY  is null then 0 else B.STO_ETC_QTY end
 end as STO_ETC_QTY_UN,
 case B.bstaus_sg when 'B' then
      case when B.STO_ETC_QTY  is null then 0 else B.STO_ETC_QTY end
 end as STO_ETC_QTY_QI,
 case B.bstaus_sg when 'D' then
      case when B.STO_ETC_QTY  is null then 0 else B.STO_ETC_QTY end
 end as STO_ETC_QTY_BL,
 */

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_PRD_QTY  is null then 0 else b.GI_PRD_QTY end
 end) as GI_PRD_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_PRD_QTY  is null then 0 else b.GI_PRD_QTY end
 end) as GI_PRD_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_PRD_QTY  is null then 0 else b.GI_PRD_QTY end
 end) as GI_PRD_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_SO_QTY  is null then 0 else b.GI_SO_QTY end
 end) as GI_SO_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_SO_QTY  is null then 0 else b.GI_SO_QTY end
 end) as GI_SO_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_SO_QTY  is null then 0 else b.GI_SO_QTY end
 end) as GI_SO_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_STO_QTY  is null then 0 else b.GI_STO_QTY end
 end) as GI_STO_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_STO_QTY  is null then 0 else b.GI_STO_QTY end
 end) as GI_STO_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_STO_QTY  is null then 0 else b.GI_STO_QTY end
 end) as GI_STO_QTY_BL,
 sum(case b.bstaus_sg when 'H' then
      case when b.GI_STO_QTY  is null then 0 else b.GI_STO_QTY end
 end) as GI_STO_QTY_ST,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_SL_QTY  is null then 0 else b.GI_SL_QTY end
 end) as GI_SL_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_SL_QTY  is null then 0 else b.GI_SL_QTY end
 end) as GI_SL_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_SL_QTY  is null then 0 else b.GI_SL_QTY end
 end) as GI_SL_QTY_BL,


 sum(case b.bstaus_sg when 'A' then
      case when b.GI_ETC_QTY  is null then 0 else b.GI_ETC_QTY end
 end) as GI_ETC_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_ETC_QTY  is null then 0 else b.GI_ETC_QTY end
 end) as GI_ETC_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_ETC_QTY  is null then 0 else b.GI_ETC_QTY end
 end) as GI_ETC_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_TR_QTY  is null then 0 else b.GI_TR_QTY end
 end) as GI_TR_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_TR_QTY  is null then 0 else b.GI_TR_QTY end
 end) as GI_TR_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_TR_QTY  is null then 0 else b.GI_TR_QTY end
 end) as GI_TR_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_SC_QTY  is null then 0 else b.GI_SC_QTY end
 end) as GI_SC_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_SC_QTY  is null then 0 else b.GI_SC_QTY end
 end) as GI_SC_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_SC_QTY  is null then 0 else b.GI_SC_QTY end
 end) as GI_SC_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_RETS_QTY  is null then 0 else b.GI_RETS_QTY end
 end) as GI_RETS_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_RETS_QTY  is null then 0 else b.GI_RETS_QTY end
 end) as GI_RETS_QTY_QI,
 sum(case when b.bstaus_sg = 'D' or b.bstaus_sg = 'C'  then
      case when b.GI_RETS_QTY  is null then 0 else b.GI_RETS_QTY end

 end) as GI_RETS_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_RETM_QTY  is null then 0 else b.GI_RETM_QTY end
 end) as GI_RETM_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_RETM_QTY  is null then 0 else b.GI_RETM_QTY end
 end) as GI_RETM_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_RETM_QTY  is null then 0 else b.GI_RETM_QTY end
 end) as GI_RETM_QTY_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_INV_QTY  is null then 0 else b.GI_INV_QTY end
 end) as GI_INV_QTY_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_INV_QTY  is null then 0 else b.GI_INV_QTY end
 end) as GI_INV_QTY_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_INV_QTY  is null then 0 else b.GI_INV_QTY end
 end) as GI_INV_QTY_BL,
 b.meins,


 sum(case b.bstaus_sg when 'A' then
      case when b.GR_PUR_VAL  is null then 0 else b.GR_PUR_VAL end
 end) as GR_PUR_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_PUR_VAL  is null then 0 else b.GR_PUR_VAL end
 end) as GR_PUR_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_PUR_VAL  is null then 0 else b.GR_PUR_VAL end
 end) as GR_PUR_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_PRD_VAL  is null then 0 else b.GR_PRD_VAL end
 end) as GR_PRD_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_PRD_VAL  is null then 0 else b.GR_PRD_VAL end
 end) as GR_PRD_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_PRD_VAL  is null then 0 else b.GR_PRD_VAL end
 end) as GR_PRD_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_DSC_VAL  is null then 0 else b.GR_DSC_VAL end
 end) as GR_DSC_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_DSC_VAL  is null then 0 else b.GR_DSC_VAL end
 end) as GR_DSC_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_DSC_VAL  is null then 0 else b.GR_DSC_VAL end
 end) as GR_DSC_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_STO_VAL  is null then 0 else b.GR_STO_VAL end
 end) as GR_STO_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_STO_VAL  is null then 0 else b.GR_STO_VAL end
 end) as GR_STO_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_STO_VAL  is null then 0 else b.GR_STO_VAL end
 end) as GR_STO_VAL_BL,
 sum(case b.bstaus_sg when 'H' then
      case when b.GR_STO_VAL  is null then 0 else b.GR_STO_VAL end
 end) as GR_STO_VAL_ST,


 sum(case b.bstaus_sg when 'A' then
      case when b.GR_SL_VAL  is null then 0 else b.GR_SL_VAL end
 end) as GR_SL_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_SL_VAL  is null then 0 else b.GR_SL_VAL end
 end) as GR_SL_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_SL_VAL  is null then 0 else b.GR_SL_VAL end
 end) as GR_SL_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_ETC_VAL  is null then 0 else b.GR_ETC_VAL end
 end) as GR_ETC_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_ETC_VAL  is null then 0 else b.GR_ETC_VAL end
 end) as GR_ETC_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_ETC_VAL  is null then 0 else b.GR_ETC_VAL end
 end) as GR_ETC_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_TR_VAL  is null then 0 else b.GR_TR_VAL end
 end) as GR_TR_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_TR_VAL  is null then 0 else b.GR_TR_VAL end
 end) as GR_TR_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_TR_VAL  is null then 0 else b.GR_TR_VAL end
 end) as GR_TR_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GR_SC_VAL  is null then 0 else b.GR_SC_VAL end
 end) as GR_SC_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_SC_VAL  is null then 0 else b.GR_SC_VAL end
 end) as GR_SC_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_SC_VAL  is null then 0 else b.GR_SC_VAL end
 end) as GR_SC_VAL_BL,


 sum(case b.bstaus_sg when 'A' then
      case when b.GR_INV_VAL  is null then 0 else b.GR_INV_VAL end
 end) as GR_INV_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GR_INV_VAL  is null then 0 else b.GR_INV_VAL end
 end) as GR_INV_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GR_INV_VAL  is null then 0 else b.GR_INV_VAL end
 end) as GR_INV_VAL_BL,

 sum(case b.bstaus_cg when 'A' then
      case when b.SIT_VAL  is null then 0 else b.SIT_VAL end
 end) as SIT_VAL_UN,
 sum(case b.bstaus_cg when 'B' then
      case when b.SIT_VAL  is null then 0 else b.SIT_VAL end
 end) as SIT_VAL_QI,
 sum(case b.bstaus_cg when 'D' then
      case when b.SIT_VAL  is null then 0 else b.SIT_VAL end
 end) as SIT_VAL_BL,

 /*
 case B.bstaus_sg when 'A' then
      case when B.STO_ETC_VAL  is null then 0 else B.STO_ETC_VAL end
 end as STO_ETC_VAL_UN,
 case B.bstaus_sg when 'B' then
      case when B.STO_ETC_VAL  is null then 0 else B.STO_ETC_VAL end
 end as STO_ETC_VAL_QI,
 case B.bstaus_sg when 'D' then
      case when B.STO_ETC_VAL  is null then 0 else B.STO_ETC_VAL end
 end as STO_ETC_VAL_BL,
*/
 sum(case b.bstaus_sg when 'A' then
      case when b.GI_PRD_VAL  is null then 0 else b.GI_PRD_VAL end
 end) as GI_PRD_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_PRD_VAL  is null then 0 else b.GI_PRD_VAL end
 end) as GI_PRD_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_PRD_VAL  is null then 0 else b.GI_PRD_VAL end
 end) as GI_PRD_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
      case when b.GI_SO_VAL  is null then 0 else b.GI_SO_VAL end
 end) as GI_SO_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
      case when b.GI_SO_VAL  is null then 0 else b.GI_SO_VAL end
 end) as GI_SO_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
      case when b.GI_SO_VAL  is null then 0 else b.GI_SO_VAL end
 end) as GI_SO_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_STO_VAL  is null then 0 else b.GI_STO_VAL end
 end) as GI_STO_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_STO_VAL  is null then 0 else b.GI_STO_VAL end
 end) as GI_STO_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
         case when b.GI_STO_VAL  is null then 0 else b.GI_STO_VAL end
 end) as GI_STO_VAL_BL,
 sum(case b.bstaus_sg when 'H' then
 case when b.GI_STO_VAL  is null then 0 else b.GI_STO_VAL end
 end) as GI_STO_VAL_ST,


 sum(case b.bstaus_sg when 'A' then
     case when b.GI_SL_VAL  is null then 0 else b.GI_SL_VAL end
 end) as GI_SL_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_SL_VAL  is null then 0 else b.GI_SL_VAL end
 end) as GI_SL_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_SL_VAL  is null then 0 else b.GI_SL_VAL end
 end) as GI_SL_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_ETC_VAL  is null then 0 else b.GI_ETC_VAL end
 end) as GI_ETC_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_ETC_VAL  is null then 0 else b.GI_ETC_VAL end
 end) as GI_ETC_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_ETC_VAL  is null then 0 else b.GI_ETC_VAL end
 end) as GI_ETC_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_TR_VAL  is null then 0 else b.GI_TR_VAL end
 end) as GI_TR_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_TR_VAL  is null then 0 else b.GI_TR_VAL end
 end) as GI_TR_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_TR_VAL  is null then 0 else b.GI_TR_VAL end
 end) as GI_TR_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_SC_VAL  is null then 0 else b.GI_SC_VAL end
 end) as GI_SC_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_SC_VAL  is null then 0 else b.GI_SC_VAL end
 end) as GI_SC_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_SC_VAL  is null then 0 else b.GI_SC_VAL end
 end) as GI_SC_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_RETS_VAL  is null then 0 else b.GI_RETS_VAL end
 end) as GI_RETS_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_RETS_VAL  is null then 0 else b.GI_RETS_VAL end
 end) as GI_RETS_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_RETS_VAL  is null then 0 else b.GI_RETS_VAL end
 end) as GI_RETS_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_RETM_VAL  is null then 0 else b.GI_RETM_VAL end
 end) as GI_RETM_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_RETM_VAL  is null then 0 else b.GI_RETM_VAL end
 end) as GI_RETM_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_RETM_VAL  is null then 0 else b.GI_RETM_VAL end
 end) as GI_RETM_VAL_BL,

 sum(case b.bstaus_sg when 'A' then
     case when b.GI_INV_VAL  is null then 0 else b.GI_INV_VAL end
 end) as GI_INV_VAL_UN,
 sum(case b.bstaus_sg when 'B' then
     case when b.GI_INV_VAL  is null then 0 else b.GI_INV_VAL end
 end) as GI_INV_VAL_QI,
 sum(case b.bstaus_sg when 'D' then
     case when b.GI_INV_VAL  is null then 0 else b.GI_INV_VAL end
 end) as GI_INV_VAL_BL,

 b.waers
}

where GR_PUR_QTY <> 0 or GR_PRD_QTY <> 0 or GR_DSC_QTY <> 0 or GR_STO_QTY <> 0 or
   GR_SL_QTY <> 0 or GR_TR_QTY <> 0 or GR_ETC_QTY <> 0 or GR_SC_QTY <> 0 or GR_INV_QTY <> 0 or SIT_QTY <> 0
   or GI_PRD_QTY <> 0 or GI_SO_QTY <> 0 or GI_STO_QTY <> 0 or GI_SL_QTY <> 0 or  GI_ETC_QTY <> 0 or GI_TR_QTY <> 0
   or GI_SC_QTY <> 0 or GI_RETS_QTY <> 0 or GI_RETM_QTY <> 0 or GI_INV_QTY <> 0

group by  b.bukrs, b.werks, b.lgort, b.matnr, b.charg, b.bwtar,  b.LIFNR, b.meins, b.waers
