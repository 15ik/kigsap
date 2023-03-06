@AbapCatalog.sqlViewName: 'ZSVBMMDOCDETAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]자재문서리스트조회(수불부 상세 용)'
define view ZCBMMDOCDETAIL
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate,
P_matnr : abap.char(40),
p_werks : abap.char(4)
as select from matdoc as a inner join ztmm00002 as b
 on a.bwart = b.field1 and a.kzbew = b.field2 and a.bstaus_sg = b.field4 and a.bstaus_cg = b.field5 and a.shkzg = b.field6
 inner join marc as X on a.matnr = X.matnr and a.werks = X.werks
 left outer join mcha as K on a.matnr = K.matnr and a.werks = K.werks and a.charg = K.charg
                                       association [1] to makt as makt on makt.matnr = $projection.matnr
                                                          and makt.spras =  $session.system_language
                                       association [1] to t001w as T001W on $projection.werks = T001W.werks
                                       association [1] to t001l as T001L on $projection.werks = T001L.werks
                                                                           and $projection.lgort = T001L.lgort
                                       association [1] to t001w as T001W2 on $projection.umwrk = T001W2.werks
                                       association [1] to t001l as T001L2 on $projection.umwrk = T001L2.werks
                                                                           and $projection.umlgo = T001L2.lgort
                                       association [1] to t156t as T156T on $projection.bwart = T156T.bwart
                                                                           and t156t.spras =  $session.system_language
                                                                           and t156t.sobkz = a.sobkz
                                                                           and t156t.kzbew = a.kzbew
                                                                           and t156t.kzzug = a.kzzug
                                                                           and t156t.kzvbr = a.kzvbr
                                       association [1] to lfa1 as lfa1 on a.lifnr = lfa1.lifnr


 {
    key a.mjahr,               //연도
    key a.mblnr,               //자재문서
    key a.zeile,               //항번
    key a.bukrs,
    key a.werks,               //플랜트
    key t001w.name1,           //플랜트명
    key a.lgort,               //저장위치
    key t001l.lgobe,           //저장위치명
    key a.matnr,               //자재코드
    key makt.maktx,            //자재명
    key case when X.xchpf is null then ''
             when X.xchpf = '' then ''
             else a.charg end as CHARG,               //Batch
    key a.budat,               //전기일
    a.umwrk,                   //이동플랜트
    T001W2.name1 as toplant,   //이동플랜트명
    a.umlgo,                   //이동저장위치
    T001L2.lgobe as tostorage, //이동저장위치명
    a.sobkz,
    case a.sobkz when 'O' then a.lifnr else cast('' as abap.char(10))  end as LIFNR, //공급업체
    lfa1.name1 as vendorname,  //공급업체명
    a.lbbsa_sid,
    a.bstaus_sg,
    a.bstaus_cg,
    a.bwart,                   //이동유형
    t156t.btext,               //이동유형명
    case when K.bwtar is null then ''
         else K.bwtar end as BWTAR,                   //연산
    a.kzbew,
    b.field3 as movegroup,
    a.stock_qty,              //수량
    a.meins,                  //단위
    a.shkzg,
    a.bktxt,                  //텍스트
//    a.kzzug,
//    a.kzvbr,
    case b.field3
       when 'GR_PUR' then
          case a.bstaus_sg when 'A' then cast('GR_PUR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_PUR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_PUR_QTY_BL' as abap.char(14))
                           end
       when 'GR_PRD' then
          case a.bstaus_sg when 'A' then cast('GR_PRD_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_PRD_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_PRD_QTY_BL' as abap.char(14))
                           end
       when 'GR_DSC' then
          case a.bstaus_sg when 'A' then cast('GR_DSC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_DSC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_DSC_QTY_BL' as abap.char(14))
                           end
       when 'GR_STO' then
          case a.bstaus_sg when 'A' then cast('GR_STO_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_STO_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_STO_QTY_BL' as abap.char(14))
                           when 'H' then cast('GR_STO_QTY_ST' as abap.char(14))
                           end
       when 'GR_SL' then
          case a.bstaus_sg when 'A' then cast('GR_SL_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_SL_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_SL_QTY_BL' as abap.char(14))
                           end
       when 'GR_ETC' then
          case a.bstaus_sg when 'A' then cast('GR_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GR_TRS' then
          case a.bstaus_sg when 'A' then cast('GR_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GR_CU' then
          case a.bstaus_sg when 'A' then cast('GR_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GR_TR' then
          case a.bstaus_sg when 'A' then cast('GR_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_TR_QTY_BL' as abap.char(14))
                           end
       when 'GR_321' then
          case a.bstaus_sg when 'A' then cast('GR_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_TR_QTY_BL' as abap.char(14))
                           end
       when 'GR_322' then
          case a.bstaus_sg when 'A' then cast('GR_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_TR_QTY_BL' as abap.char(14))
                           end
       when 'GR_343' then
          case a.bstaus_sg when 'A' then cast('GR_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_TR_QTY_BL' as abap.char(14))
                           end
       when 'GR_344' then
          case a.bstaus_sg when 'A' then cast('GR_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GR_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GR_TR_QTY_BL' as abap.char(14))
                           end
       when 'GR_SC' then
          case a.bstaus_sg when 'Q' then
             case a.bstaus_cg when 'A' then cast('GR_SC_QTY_UN' as abap.char(14))
                              when 'B' then cast('GR_SC_QTY_QI' as abap.char(14))
                              when 'D' then cast('GR_SC_QTY_BL' as abap.char(14))
                              else   cast('GR_SC_QTY_UN' as abap.char(14))
                              end
          end
       when 'GR_INV' then
          case a.bstaus_cg when 'Q' then
             case a.bstaus_sg when 'A' then cast('GR_INV_QTY_UN' as abap.char(14))
                              when 'B' then cast('GR_INV_QTY_QI' as abap.char(14))
                              when 'D' then cast('GR_INV_QTY_BL' as abap.char(14))
                              end
          end
       when 'SIT' then
          case a.bstaus_sg when 'A' then cast('SIT_QTY_UN' as abap.char(14))
                           when 'B' then cast('SIT_QTY_QI' as abap.char(14))
                           when 'D' then cast('SIT_QTY_BL' as abap.char(14))
                           end
       when 'STO_ETC' then
          case a.bstaus_sg when 'A' then cast('STO_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('STO_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('STO_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GI_PRD' then
          case a.bstaus_sg when 'A' then cast('GI_PRD_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_PRD_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_PRD_QTY_BL' as abap.char(14))
                           end
       when 'GI_SO' then
          case a.bstaus_sg when 'A' then cast('GI_SO_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_SO_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_SO_QTY_BL' as abap.char(14))
                           end
       when 'GI_STO' then
          case a.bstaus_sg when 'A' then cast('GI_STO_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_STO_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_STO_QTY_BL' as abap.char(14))
                           when 'H' then cast('GI_STO_QTY_ST' as abap.char(14))
                           end
       when 'GI_SL' then
          case a.bstaus_sg when 'A' then cast('GI_SL_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_SL_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_SL_QTY_BL' as abap.char(14))
                           end
       when 'GI_ETC' then
          case a.bstaus_sg when 'A' then cast('GI_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GI_TRS' then
          case a.bstaus_sg when 'A' then cast('GI_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GI_CU' then
          case a.bstaus_sg when 'A' then cast('GI_ETC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_ETC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_ETC_QTY_BL' as abap.char(14))
                           end
       when 'GI_TR' then
          case a.bstaus_sg when 'A' then cast('GI_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_TR_QTY_BL' as abap.char(14))
                           end
       when 'GI_321' then
          case a.bstaus_sg when 'A' then cast('GI_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_TR_QTY_BL' as abap.char(14))
                           end
       when 'GI_322' then
          case a.bstaus_sg when 'A' then cast('GI_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_TR_QTY_BL' as abap.char(14))
                           end
       when 'GI_343' then
          case a.bstaus_sg when 'A' then cast('GI_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_TR_QTY_BL' as abap.char(14))
                           end
       when 'GI_344' then
          case a.bstaus_sg when 'A' then cast('GI_TR_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_TR_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_TR_QTY_BL' as abap.char(14))
                           end
       when 'GI_SC' then
          case a.bstaus_sg when 'A' then cast('GI_SC_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_SC_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_SC_QTY_BL' as abap.char(14))
                           end
       when 'GI_RETS' then
          case a.bstaus_sg when 'A' then cast('GI_RETS_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_RETS_QTY_QI' as abap.char(14))
                           when 'C' then cast('GI_RETS_QTY_BL' as abap.char(14))
                           when 'D' then cast('GI_RETS_QTY_BL' as abap.char(14))
                           end
       when 'GI_RETM' then
          case a.bstaus_sg when 'A' then cast('GI_RETM_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_RETM_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_RETM_QTY_BL' as abap.char(14))
                           end
       when 'GI_INV' then
          case a.bstaus_sg when 'A' then cast('GI_RETM_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_RETM_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_RETM_QTY_BL' as abap.char(14))
                           end
       when 'GI_INV' then
          case a.bstaus_sg when 'A' then cast('GI_RETM_QTY_UN' as abap.char(14))
                           when 'B' then cast('GI_RETM_QTY_QI' as abap.char(14))
                           when 'D' then cast('GI_RETM_QTY_BL' as abap.char(14))
                           end
       end  as ZTYPE
}

where a.budat between $parameters.P_StartDate and $parameters.P_EndDate
 and b.zmain_cat = 'D1' and b.zmidd_cat = 'D1300' and b.zsmal_cat = 'D1302'
 and a.matnr = $parameters.P_matnr
 and a.werks = $parameters.p_werks
