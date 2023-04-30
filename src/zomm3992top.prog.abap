*&---------------------------------------------------------------------*
*& Include          ZOMM3992TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: SSCRFIELDS.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000'.

CONSTANTS: GC_OBJID TYPE WWWDATA-OBJID VALUE 'ZOMM3992',
           GC_OBJID_EN TYPE WWWDATA-OBJID VALUE 'ZOMM3992_01',  "U1: 영문화
           GC_LANGU_EN TYPE SY-LANGU VALUE 'EN'.

CONSTANTS: GC_BSART_C10 TYPE EBAN-BSART VALUE 'C10',  "공사/용역 구매요청
           GC_BSART_M10 TYPE EBAN-BSART VALUE 'M10',  "비재고 물품 구매요청
           GC_BSART_M20 TYPE EBAN-BSART VALUE 'M20'.  "재고 물품 구매요청

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300'. "노란색

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_EXCEL,
         CNTR_NO   TYPE ZE_CNTR_NO,                 "AS-IS 계약요청
         CNTR_ITEM TYPE ZE_CNTR_ITEM_LNO,           "AS-IS 품목
         BSART     TYPE EBAN-BSART,                 "구매요청유형
         TITLE     TYPE ZTMM30010-ZPRTITLE,         "구매요청명
         REQER     TYPE ZSVMM_USER_INFO-EMPLOY_NO,  "요청자
*                        ZTMM30010-ZREQUESTER,
         PSTYP     TYPE EBAN-PSTYP,                 "품목범주
         MATNR     TYPE EBAN-MATNR,                 "자재
         TXZ01     TYPE EBAN-TXZ01,                 "자재내역(공사,대표자재)
         BWTAR     TYPE EBAN-BWTAR,                 "평가유형
         MENGE     TYPE EBAN-MENGE,                 "요청수량
         LFDAT     TYPE EBAN-LFDAT,                 "납품일
         WERKS     TYPE EBAN-WERKS,                 "플랜트
         LGORT     TYPE EBAN-LGORT,                 "저장위치
         ORDER     TYPE ZSVMM_USER_INFO-EMPLOY_NO,  "계약담당자
*                        EBAN-ZORDER_PERSON,
         LIFNR     TYPE EBAN-LIFNR,                 "공급업체
         EKGRP     TYPE EBAN-EKGRP,                 "구매그룹
         PREIS     TYPE EBAN-PREIS,                 "추정가격
         WAERS     TYPE EBAN-WAERS,                 "통화
         SAKTO     TYPE EBKN-SAKTO,                 "G/L계정
         WBSNO     TYPE EBKN-PS_PSP_PNR,            "WBS
         KOSTL     TYPE EBKN-KOSTL,                 "코스트센터
         MESSAGE   TYPE BAPIRET2-MESSAGE,           "처리메시지
       END OF TY_EXCEL.

TYPES: BEGIN OF TY_DATA,
         ROW_NO TYPE SY-TABIX,
         STATUS TYPE ICON-ID,                     "처리결과
         BANFN  TYPE EBAN-BANFN,
         BNFPO  TYPE EBAN-BNFPO,
         KNTTP  TYPE EBAN-KNTTP,
         MEINS  TYPE EBAN-MEINS,
         DEPTQ  TYPE ZTMM30010-ZPEQ_DEPARTMENT,   "요청부서
         DEPTO  TYPE EBAN-ZORDER_DEPARTMENT,      "계약부서
       END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_EXCEL.
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP.

* AS-IS 계약요청, 품목별 구매요청 생성
TYPES: BEGIN OF TY_COLL,
         CNTR_NO   TYPE ZE_CNTR_NO,
         CNTR_ITEM TYPE ZE_CNTR_ITEM_LNO,
         COUNT     TYPE I,
       END OF TY_COLL.

TYPES: BEGIN OF TY_ERROR,
         CNTR_NO(30),
         CNTR_ITEM(5),
         BSART(4),
         TITLE(225),
         REQER(12),
         PSTYP(1),
         MATNR(40),
         TXZ01(40),
         BWTAR(10),
         MENGE(13),
         LFDAT(8),
         WERKS(4),
         LGORT(4),
         ORDER(12),
         LIFNR(10),
         EKGRP(3),
         PREIS(13),
         WAERS(5),
         SAKTO(10),
         WBSNO(8),
         KOSTL(10),

         MESSAGE(220),
       END OF TY_ERROR.

*---------------------
*-- Redefine
*---------------------
TYPES: BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MEINS TYPE MARA-MEINS,
       END OF TY_MARA.

TYPES: BEGIN OF TY_MBEW,
         MATNR TYPE MBEW-MATNR,
         BWKEY TYPE MBEW-BWKEY,
         BWTAR TYPE MBEW-BWTAR,
       END OF TY_MBEW.

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
DATA: GT_EXCEL TYPE TABLE OF TY_EXCEL,
      GT_DISP  TYPE TABLE OF TY_DISP.

DATA: GT_ERROR TYPE TABLE OF TY_LINE.

* 검증 시 사용
DATA: GT_USER_REQ TYPE TABLE OF ZSVMM_USER_INFO,
      GT_USER_ORD TYPE TABLE OF ZSVMM_USER_INFO,
      GT_T001W    TYPE TABLE OF T001W,
      GT_T001L    TYPE TABLE OF T001L,
      GT_ORG      TYPE TABLE OF ZSVCMM_ORG,
      GT_MBEW     TYPE TABLE OF TY_MBEW,
      GT_MARA     TYPE TABLE OF TY_MARA,
      GT_MAKT     TYPE TABLE OF MAKT,
      GT_SKA1     TYPE TABLE OF SKA1,
      GT_PRPS     TYPE TABLE OF PRPS,
      GT_CSKS     TYPE TABLE OF CSKS.

* AS-IS 계약요청, 품목별 구매요청 생성
DATA: GT_COLL TYPE TABLE OF TY_COLL.

DATA: OK_CODE    TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM.

* Selected Rows
*DATA: GT_ROWS TYPE LVC_T_ROW.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <GV_FS>.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION.

DEFINE _G_SET_VALUES.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = &1
      VALUES = &2.

END-OF-DEFINITION.
