@AbapCatalog.sqlViewName: 'ZSVCMMDOCDETAIL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]자재문서리스트조회(수불부 상세 용)'
define view ZCCMMDOCDETAIL
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate,
P_matnr : abap.char(40),
P_werks : abap.char(4),
P_TYPE : abap.char(14)
as select from ZCBMMDOCDETAIL(P_StartDate: $parameters.P_StartDate, P_EndDate: $parameters.P_EndDate,
                              P_matnr: $parameters.P_matnr, p_werks: $parameters.P_werks)

{
    key mjahr,               //연도
    key mblnr,               //자재문서
    key zeile,               //항번
    key bukrs,
    key werks,               //플랜트
    key name1,           //플랜트명
    key lgort,               //저장위치
    key lgobe,           //저장위치명
    key matnr,               //자재코드
    key maktx,            //자재명
    key CHARG,               //Batch
    key budat,               //전기일
    umwrk,                   //이동플랜트
    toplant,   //이동플랜트명
    umlgo,                   //이동저장위치
    tostorage, //이동저장위치명
    sobkz,
    LIFNR, //공급업체
    vendorname,  //공급업체명
    lbbsa_sid,
    bstaus_sg,
    bstaus_cg,
    bwart,                   //이동유형
    btext,               //이동유형명
    BWTAR,                   //연산
    kzbew,
    movegroup,
    stock_qty,              //수량
    meins,                  //단위
    shkzg,
    bktxt,                  //텍스트
    ZTYPE
} where ZTYPE = $parameters.P_TYPE
