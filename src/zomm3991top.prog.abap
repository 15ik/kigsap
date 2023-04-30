*&---------------------------------------------------------------------*
*& Include          ZOMM3991TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: SSCRFIELDS, EKKO.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000'.

CONSTANTS: GC_OBJID_DO TYPE WWWDATA-OBJID VALUE 'ZOMM3991_01',
           GC_OBJID_IM TYPE WWWDATA-OBJID VALUE 'ZOMM3991_02'.

CONSTANTS: GC_BSART_PSM1 TYPE EKKO-BSART VALUE 'PSM1',
           GC_BSART_PSM2 TYPE EKKO-BSART VALUE 'PSM2',
           GC_BSART_PSIC TYPE EKKO-BSART VALUE 'PSIC'.

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300'. "노란색

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
**---------------------
*- Upload 관련
**---------------------
*> 국내 발주 EXCEL 포맷
TYPES: BEGIN OF TY_EXCEL_DO,
*> HEADER
         CNTR_NO       TYPE ZE_CNTR_NO,                 "AS-IS 계약번호
         CNTR_REV      TYPE ZE_CNTR_REV,                "AS-IS 차수
         CNTR_ITEM     TYPE ZE_CNTR_ITEM_LNO,           "AS-IS 품목
         BSART         TYPE EKKO-BSART,                 "문서유형
         EKGRP         TYPE EKKO-EKGRP,                 "구매그룹
         TITLE         TYPE EKKO-/BOFU/BCSD_SUBJ,       "발주명
         ORDER         TYPE EKKO-ZORDER_PERSON,         "발주담당
         EXPER         TYPE EKKO-ZEXPEN_PERSON,         "지출발의담당
         QMPER         TYPE EKKO-ZQM_PERSON,            "검수담당자
         ABSGR         TYPE EKKO-ABSGR,                 "세금계산서발행방식
         MTYPE         TYPE C,                          "임가공발주
         LIFNR         TYPE EKKO-LIFNR,                 "공급업체
         VERKF         TYPE EKKO-VERKF,                 "담당자
         BEDAT         TYPE EKKO-BEDAT,                 "계약체결일
         ZTERM         TYPE EKKO-ZTERM,                 "지급조건
         INCO1         TYPE EKKO-INCO1,                 "인도조건
         WAERS         TYPE EKKO-WAERS,                 "통화
         DPPCT         TYPE EKKO-DPPCT,                 "선급비율
         ZREAL_COST    TYPE EKKO-ZREAL_COST,            "실비정산여부
*> ITEM
         MATNR         TYPE EKPO-MATNR,               "자재
         BWTAR         TYPE EKPO-BWTAR,               "평가유형
         MEINS         TYPE EKPO-MEINS,               "단위
         MENGE         TYPE EKPO-MENGE,               "수량
         NETPR         TYPE PRCT_NET_PRICE , "EKPO-NETPR,  "단가
         PEINH         TYPE EKPO-PEINH,               "가격단위
         MWSKZ         TYPE EKPO-MWSKZ,               "세금코드
         MIUWR         TYPE PRCT_NET_PRICE, "EKPO-NETWR,  "품목가감액
         ZPRICE_CHANGE TYPE EKPO-ZPRICE_CHANGE,       "조정사유코드
         ZPRICE_REASON TYPE EKPO-ZPRICE_REASON,       "조정사유상세
         WERKS         TYPE EKPO-WERKS,               "플랜트
         LGORT         TYPE EKPO-LGORT,               "창고
         EINDT         TYPE EKET-EINDT,               "납품일
         UMWRK         TYPE EKPO-WERKS,               "입고플랜트
         SC_VENDER     TYPE LFA1-LIFNR,               "SC업체
         SAKTO         TYPE EKKN-SAKTO,               "G/L계정
         KOSTL         TYPE EKKN-KOSTL,               "코스트센터
         WBSNO         TYPE PS_POSID, "EKKN-PS_PSP_PNR,          "WBS
         VBELN         TYPE EKKN-VBELN,               "영업오더
         VBELP         TYPE EKKN-VBELP,               "영업품목
         IDNLF         TYPE EKPO-IDNLF,               "공급업체 자재코드
         CHARG         TYPE CHARG_D,                  "배치

*> 결과 (UPLOAD 시 오류 방지용)
         MSGTX       TYPE BAPI_MSG,
       END OF TY_EXCEL_DO.


TYPES: BEGIN OF TY_EXCEL_IM,
*> HEADER
         CNTR_NO       TYPE ZE_CNTR_NO,                 "AS-IS 계약번호
         CNTR_REV      TYPE ZE_CNTR_REV,                "AS-IS 차수
         CNTR_ITEM     TYPE ZE_CNTR_ITEM_LNO,           "AS-IS 품목
         BSART         TYPE EKKO-BSART,                 "문서유형
         EKGRP         TYPE EKKO-EKGRP,                 "구매그룹
         TITLE         TYPE EKKO-/BOFU/BCSD_SUBJ,       "발주명
         ORDER         TYPE EKKO-ZORDER_PERSON,         "발주담당자
         EXPER         TYPE EKKO-ZEXPEN_PERSON,         "지출발의자
         LIFNR         TYPE EKKO-LIFNR,                 "공급업체
         VERKF         TYPE EKKO-VERKF,                 "담당자
         BEDAT         TYPE EKKO-VERKF,                 "계약체결일
         ZTERM         TYPE EKKO-ZTERM,                 "지급조건
         INCO1         TYPE EKKO-INCO1,                 "인도조건
         WAERS         TYPE EKKO-WAERS,                 "통화
         DPPCT         TYPE EKKO-DPPCT,                 "선급비율
         ZEINSPECT     TYPE EKKO-ZEINSPECT,             "수입검사여부
         ZEMANAGE2     TYPE EKKO-ZEMANAGE2,             "관리번호
         HERKL         TYPE EKKO-HERKL,                 "원산지
         ZESHIPTYPE    TYPE EKKO-ZESHIPTYPE,            "선적구분
         ZEDEDLINE     TYPE EKKO-ZEDEDLINE,             "선적기한
         INCO2_L       TYPE EKKO-INCO2_L,                "선적항
         INCO3_L       TYPE EKKO-INCO3_L,                "도착항

*> ITEM
         MATNR         TYPE EKPO-MATNR,               "자재
         BWTAR         TYPE EKPO-BWTAR,               "평가유형
         MEINS         TYPE EKPO-MEINS,               "단위
         MENGE         TYPE EKPO-MENGE,               "수량
         NETPR         TYPE PRCT_NET_PRICE,               "단가
         PEINH         TYPE EKPO-PEINH,               "가격단위
         MIUWR         TYPE PRCT_NET_PRICE,               "품목가감액
         ZPRICE_CHANGE TYPE EKPO-ZPRICE_CHANGE,       "조정사유코드
         ZPRICE_REASON TYPE EKPO-ZPRICE_REASON,       "조정사유상세
         WERKS         TYPE EKPO-WERKS,               "플랜트
         LGORT         TYPE EKPO-LGORT,               "창고
         EINDT         TYPE EKET-EINDT,               "납품일
         SAKTO         TYPE EKKN-SAKTO,               "G/L계정
         KOSTL         TYPE EKKN-KOSTL,               "코스트센터
         WBSNO         TYPE PS_POSID, "EKKN-PS_PSP_PNR,          "WBS

*> 결과 (UPLOAD 시 오류 방지용)
         MSGTX       TYPE BAPI_MSG,
       END OF TY_EXCEL_IM.


**---------------------
"*- Alv 관련
**---------------------
TYPES: BEGIN OF TY_DATA,
*---------------------
*> HEADER 구매 공통...
*---------------------
         CNTR_NO       TYPE ZE_CNTR_NO,                 "AS-IS 계약번호
         CNTR_REV      TYPE ZE_CNTR_REV,                "AS-IS 차수
         CNTR_ITEM     TYPE ZE_CNTR_ITEM_LNO,           "AS-IS 품목
         EBELN         TYPE EKKO-EBELN,                 "PO 번호
         EBELP         TYPE EKPO-EBELP,                 "PO 품목
         BSART         TYPE EKKO-BSART,                 "문서유형
         EKGRP         TYPE EKKO-EKGRP,                 "구매그룹
         TITLE         TYPE EKKO-/BOFU/BCSD_SUBJ,       "발주명
         ORDER         TYPE EKKO-ZORDER_PERSON,         "발주담당자
         EXPER         TYPE EKKO-ZEXPEN_PERSON,         "지출발의자
         LIFNR         TYPE EKKO-LIFNR,                 "공급업체
         VERKF         TYPE EKKO-VERKF,                 "담당자
         BEDAT         TYPE EKKO-BEDAT,                 "계약체결일
         ZTERM         TYPE EKKO-ZTERM,                 "지급조건
         INCO1         TYPE EKKO-INCO1,                 "인도조건
         WAERS         TYPE EKKO-WAERS,                 "통화
         DPPCT         TYPE EKKO-DPPCT,                 "선급비율
         ZREAL_COST    TYPE EKKO-ZREAL_COST,            "실비정산여부

*---------------------
*> HEADER 국내 전용...
*---------------------
         QMPER         TYPE EKKO-ZQM_PERSON,            "검수담당자
         ABSGR         TYPE EKKO-ABSGR,                 "세금계산서발행방식
         MTYPE         TYPE C,                          "임가공발주

*---------------------
*> HEADER 수입 전용...
*---------------------
         ZEINSPECT     TYPE EKKO-ZEINSPECT,             "수입검사여부
         ZEMANAGE2     TYPE EKKO-ZEMANAGE2,             "관리번호
         HERKL         TYPE EKKO-HERKL,                 "원산지
         ZESHIPTYPE    TYPE EKKO-ZESHIPTYPE,            "선적구분
         ZEDEDLINE     TYPE EKKO-ZEDEDLINE,             "선적기한
         INCO2_L       TYPE EKKO-INCO2_L,                "선적항
         INCO3_L       TYPE EKKO-INCO3_L,                "도착항

*---------------------
*> ITEM 구매 공통...
*---------------------
         MATNR         TYPE EKPO-MATNR,               "자재
         MAKTX        TYPE MAKT-MAKTX,
         BWTAR         TYPE EKPO-BWTAR,               "평가유형
         MEINS         TYPE EKPO-MEINS,               "단위
         MENGE         TYPE EKPO-MENGE,               "수량
         NETPR         TYPE PRCT_NET_PRICE,               "단가
         PEINH         TYPE EKPO-PEINH,               "가격단위

         MIUWR         TYPE BSEG-HWBAS,               "품목가감액
         ZPRICE_CHANGE TYPE EKPO-ZPRICE_CHANGE,       "조정사유코드
         ZPRICE_REASON TYPE EKPO-ZPRICE_REASON,       "조정사유상세
         WERKS         TYPE EKPO-WERKS,               "플랜트
         LGORT         TYPE EKPO-LGORT,               "창고
         EINDT         TYPE EKET-EINDT,               "납품일

         SAKTO         TYPE EKKN-SAKTO,               "G/L계정
         KOSTL         TYPE EKKN-KOSTL,               "코스트센터
         WBSNO         TYPE PS_POSID , "EKKN-PS_PSP_PNR,          "WBS

*---------------------
*> ITEM 국내 전용...
*---------------------
         MWSKZ         TYPE EKPO-MWSKZ,               "세금코드
         UMWRK         TYPE EKPO-WERKS,               "입고플랜트
         SC_VENDER     TYPE LFA1-LIFNR,               "SC업체
         VBELN         TYPE EKKN-VBELN,               "영업오더
         VBELP         TYPE EKKN-VBELP,               "영업품목
         IDNLF         TYPE EKPO-IDNLF,               "공급업체 자재코드
         CHARG         TYPE CHARG_D,                  "배치

*> 결과
         MESSAGE       TYPE BAPI_MSG,


*---------------------
*> NO-DISPLAY
*---------------------
         ZORDER_DEPARTMENT TYPE EKKO-ZORDER_DEPARTMENT,   "발주부서
         ZEXPEN_DEPARTMENT TYPE EKKO-ZEXPEN_DEPARTMENT,   "지출발의부서
         ZQM_DEPARTMENT TYPE EKKO-ZQM_DEPARTMENT,      "검수부서
         ROW_KEY(10)   TYPE C,  "라인 KEY
       END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP.


**---------------------
"*- BAPI 관련
**---------------------
* AS-IS 계약번호, 차수, 품목별 계약 생성
TYPES: BEGIN OF TY_COLL,
         CNTR_NO  TYPE ZE_CNTR_NO,
         CNTR_REV TYPE ZE_CNTR_REV,
*         CNTR_ITEM TYPE ZE_CNTR_ITEM_LNO,
         COUNT    TYPE I,
       END OF TY_COLL.

**---------------------
**-- Error 관련
**---------------------
TYPES: BEGIN OF TY_ERROR_DO,
       CNTR_NO(100),    "AS-IS 계약번호
       CNTR_REV(100),    "AS-IS 차수
       CNTR_ITEM(100),    "AS-IS 품목
       BSART(100),    "문서유형
       EKGRP(100),    "구매그룹
       TITLE(100),    "발주명
       ORDER(100),    "발주담당
       EXPER(100),    "지출발의담당
       QMPER(100),    "검수담당자
       ABSGR(100),             "세금계산서발행방식
       MTYPE(100),    "임가공발주
       LIFNR(100),    "공급업체
       VERKF(100),    "담당자
       BEDAT(100),    "계약체결일
       ZTERM(100),    "지급조건
       INCO1(100),    "인도조건
       WAERS(100),    "통화
       DPPCT(100),    "선급비율
       ZREAL_COST(100),    "실비정산여부

       MATNR(100),  "자재
       BWTAR(100),  "평가유형
       MEINS(100),  "단위
       MENGE(100),  "수량
       NETPR(100),  "단가
       PEINH(100),  "가격단위
       MWSKZ(100),  "세금코드
       MIUWR(100),  "품목가감액
       ZPRICE_CHANGE(100),  "조정사유코드
       ZPRICE_REASON(100),  "조정사유상세
       WERKS(100),  "플랜트
       LGORT(100),  "창고
       EINDT(100),  "납품일
       UMWRK(100),  "입고플랜트
       SC_VENDER(100),  "SC업체
       SAKTO(100),  "G/L계정
       KOSTL(100),  "코스트센터
       WBSNO(100),  "WBS
       VBELN(100),  "영업오더
       VBELP(100),  "영업품목
       IDNLF(100),  "공급업체 자재코드
       CHARG(100),  "배치

       MESSAGE(220),
       END OF TY_ERROR_DO.

TYPES: BEGIN OF TY_ERROR_IM,
        CNTR_NO(100),   "AS-IS 계약번호
        CNTR_REV(100),   "AS-IS 차수
        CNTR_ITEM(100),   "AS-IS 품목
        BSART(100),   "문서유형
        EKGRP(100),   "구매그룹
        TITLE(100),   "발주명
        ORDER(100),   "발주담당자
        EXPER(100),   "지출발의자
        LIFNR(100),   "공급업체
        VERKF(100),   "담당자
        BEDAT(100),   "계약체결일
        ZTERM(100),   "지급조건
        INCO1(100),   "인도조건
        WAERS(100),   "통화
        DPPCT(100),    "선급비율
        ZEINSPECT(100),   "수입검사여부
        ZEMANAGE2(100),   "관리번호
        HERKL(100),   "원산지
        ZESHIPTYPE(100),   "선적구분
        ZEDEDLINE(100),   "선적기한
        INCO2_L(100),    "선적항
        INCO3_L(100),    "도착항

        MATNR(100), "자재
        BWTAR(100), "평가유형
        MEINS(100), "단위
        MENGE(100), "수량
        NETPR(100), "단가
        PEINH(100), "가격단위
        MIUWR(100), "품목가감액
        ZPRICE_CHANGE(100), "조정사유코드
        ZPRICE_REASON(100), "조정사유상세
        WERKS(100), "플랜트
        LGORT(100), "창고
        EINDT(100), "납품일
        SAKTO(100), "G/L계정
        KOSTL(100), "코스트센터
        WBSNO(100), "WBS
       MESSAGE(220),
       END OF TY_ERROR_IM.

  TYPES:
    BEGIN OF  ts_ztmm00002,
        field1  TYPE  field_name,
        field2  TYPE  field_name,
        field3  TYPE  field_name,
        field4  TYPE  field_name,
        field5  TYPE  field_name,
        field6  TYPE  field_name,
        field7  TYPE  field_name,
        field8  TYPE  field_name,
        field9  TYPE  field_name,
        field10 TYPE  field_name,
        field11 TYPE  field_name,
        field12 TYPE  field_name,
        field13 TYPE  field_name,
      END OF ts_ztmm00002 .

**---------------------
**-- Redefine
**---------------------
TYPES: TY_LINE(3000).

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID.   " ALV Grid

*---------------------
*-- Excel Object
*---------------------
DATA: GO_EXCEL     TYPE OLE2_OBJECT, " EXCEL OBJECT
      GO_WORKBOOK  TYPE OLE2_OBJECT, " WORKBOOK
      GO_WORKSHEET TYPE OLE2_OBJECT,
      GO_RANGE     TYPE OLE2_OBJECT,
      GO_CELL1     TYPE OLE2_OBJECT,
      GO_CELL2     TYPE OLE2_OBJECT,
      GO_BUFFER    TYPE OLE2_OBJECT,
      GO_ROW       TYPE OLE2_OBJECT.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_EXCEL_DO TYPE TABLE OF TY_EXCEL_DO,
      GT_EXCEL_IM TYPE TABLE OF TY_EXCEL_IM,
      GT_DISP     TYPE TABLE OF TY_DISP.

*DATA: GT_INCO TYPE TABLE OF TY_INCO.

DATA: "GT_ERROR_DO TYPE TABLE OF TY_ERROR_DO,
      "GT_ERROR_IM TYPE TABLE OF TY_ERROR_IM,
      GT_ERROR TYPE TABLE OF TY_LINE.

* 검증 시 사용
DATA: GT_T161  TYPE TABLE OF T161,
      GT_ORG   TYPE TABLE OF ZSVCMM_ORG,
      GT_USER  TYPE TABLE OF ZSVMM_USER_INFO,
      GT_T001W TYPE TABLE OF T001W,
      GT_CONFIG TYPE TABLE OF ts_ztmm00002.

*> BAPI 수행 시 추가 필요 데이터
DATA: BEGIN OF GS_ZSVBMMINFOPRICE,
        INFNR TYPE ZSVBMMINFOPRICE-INFNR,
        LIFNR TYPE ZSVBMMINFOPRICE-LIFNR,
        MATNR TYPE ZSVBMMINFOPRICE-MATNR,
      END OF GS_ZSVBMMINFOPRICE,
      GT_ZSVBMMINFOPRICE LIKE TABLE OF GS_ZSVBMMINFOPRICE.

*DATA: GV_CHECK TYPE C VALUE ''.   "검증 여부 체크

* AS-IS 계약번호, 차수, 품목별 계약 생성
DATA: GT_COLL TYPE TABLE OF TY_COLL.

DATA: OK_CODE    TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM.

*DATA: GV_FIELD TYPE FIELDNAME.

DATA: GS_MSGTB TYPE ZSCN00001.

* Selected Rows
*DATA: GT_ROWS TYPE LVC_T_ROW.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <GV_FS>.
FIELD-SYMBOLS: <GT_EXCEL> TYPE TABLE.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION.
**> 값 단위 별 변환 (EXT TO SAP)
DEFINE _G_CONV_DATA_EXT_TO_SAP.

  ZCL_MM_COMMON=>CONV_DATA_EAI_TO_SAP( EXPORTING IV_VALUE  = &1
                                          IV_UNIT = &2
                                IMPORTING EV_VALUE   = &3 ).

END-OF-DEFINITION.
DEFINE _G_SET_MSGTB.

  CLEAR:gs_msgtb.

    gs_msgtb = VALUE #( fieldname = &3   msgty = 'E'  arbgb = &4   txtnr = &5
                        msgv1     = &6   msgv2 = &7   msgv3 = &8  ).

  grf_grid->set_msgtb( EXPORTING iv_delete = &1
                                 is_msgtb  = gs_msgtb
                       CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _G_WAIT_1_SECOND.

  WAIT UP TO '0.5' SECONDS.

END-OF-DEFINITION .
DEFINE _G_ADD_1.

  &1 = &1 + 1.

END-OF-DEFINITION .
