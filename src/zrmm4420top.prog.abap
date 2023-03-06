*&---------------------------------------------------------------------*
*& Include          ZRMM4420TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : T001, MKPF, T001W, T001L, LFA1, MARA, MARC, MCH1, MBEW.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
*>TYPE

CONSTANTS: GC_RA2(4) TYPE C VALUE 'RA2',

           GC_C110(10) VALUE 'C110',
           GC_C200(10) VALUE 'C200',
           GC_C201(10) VALUE 'C201',
           GC_C210(10) VALUE 'C210',
           GC_C300(10) VALUE 'C300',
           GC_C310(10) VALUE 'C310',
           GC_C400(10) VALUE 'C400',
           GC_C500(10) VALUE 'C500',
           GC_C510(10) VALUE 'C510',
           GC_C600(10) VALUE 'C600',
           GC_C610(10) VALUE 'C610',
           GC_C700(10) VALUE 'C700',
           GC_C710(10) VALUE 'C710',

           GC_1000(4)  VALUE '1000',
           GC_BUKRS(7) VALUE 'P_BUKRS',
           GC_WERKS(7) VALUE 'S_WERKS',
           GC_LGORT(7) VALUE 'S_LGORT',
           GC_DISPO(7) VALUE 'S_DISPO',

*----------------------------------------------------------------------*
* U4 - CDS VIEW 에 있는 한글을 그대로 읽기 위한 CONSTANT
*---------------------------START--------------------------------------*
           GC_ASTOCK(10) VALUE '가용재고',
           GC_QSTOCK(10) VALUE '품질검사중재고',
           GC_BSTOCK(10) VALUE '보류재고'.
*-----------------------------END--------------------------------------*




*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATE.
         include structure ZSMM_DATE_VALUE_2.
TYPES: END OF TY_DATE.
TYPES: BEGIN OF TY_DATA,
         BUKRS                     TYPE ZSVPMMSTOCKTIMES-BUKRS,                     "회사코드
         COMPANYCODENAME           TYPE ZSVPMMSTOCKTIMES-COMPANYCODENAME,           "회사명
         MATNR                     TYPE ZSVPMMSTOCKTIMES-MATNR,                     "자재
         MATERIALNAME              TYPE ZSVPMMSTOCKTIMES-MATERIALNAME,              "자재명
         MATKL                     TYPE ZSVPMMSTOCKTIMES-MATKL,                     "자재그룹
         WGBEZ                     TYPE T023T-WGBEZ,                                "자재그룹명
         WERKS                     TYPE ZSVPMMSTOCKTIMES-WERKS,                     "플랜트
         PLANTNAME                 TYPE ZSVPMMSTOCKTIMES-PLANTNAME,                 "플랜트명
         LGORT                     TYPE ZSVPMMSTOCKTIMES-LGORT,                     "저장위치
         STORAGELOCATIONNAME       TYPE ZSVPMMSTOCKTIMES-STORAGELOCATIONNAME,       "저장위치명
         LIFNR                     TYPE ZSVPMMSTOCKTIMES-LIFNR,                     "공급업체
         NAME1                     TYPE ZSVPMMSTOCKTIMES-NAME1,                     "공급업체명
         TYPECODE                  TYPE ZSVPMMSTOCKTIMES-TYPECODE,                  "재고유형코드
         TYPENAME                  TYPE CHAR20,                  "재고유형
         INVENTORYSPECIALSTOCKTYPE TYPE ZSVPMMSTOCKTIMES-INVENTORYSPECIALSTOCKTYPE, "특별재고유형
         PERIODTYPE                TYPE ZSVPMMSTOCKTIMES-PERIODTYPE,                "기간구분
         BATCH                     TYPE ZSVPMMSTOCKTIMES-BATCH,                     "BATCH
         MEINS                     TYPE ZSVPMMSTOCKTIMES-MEINS,                     "단위
         ZDATE                     TYPE ZSVPMMSTOCKTIMES-ZDATE,                     "기간
         STOCKQTY                  TYPE ZSVPMMSTOCKTIMES-STOCKQTY,                  "수량
         LICHN                     TYPE ZSVPMMSTOCKTIMES-LICHN,                     "제조LOT
         HSDAT                     TYPE ZSVPMMSTOCKTIMES-HSDAT,                     "제조일
         VFDAT                     TYPE ZSVPMMSTOCKTIMES-VFDAT,                     "사용기한
         EKGRP                     TYPE ZSVPMMSTOCKTIMES-EKGRP,                     "구매그룹
         EKNAM                     TYPE ZSVPMMSTOCKTIMES-EKNAM,                     "구매그룹내역
         DISPO                     TYPE ZSVPMMSTOCKTIMES-DISPO,                     "MRP 관리자
         DSNAM                     TYPE ZSVPMMSTOCKTIMES-DSNAM,                     "MRP 관리자내역
*         BKLAS                     TYPE ZSVPMMSTOCKTIMES-BKLAS,                     "평가클래스
       END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
         INCLUDE TYPE TY_DATE.
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
         NEW,                    "신규데이타
       END OF  TS_DISP.

TYPES: BEGIN OF TY_DAY,
         ZDATE TYPE ZSVPMMSTOCKTIMES-ZDATE,
         DAY   TYPE CHAR6,                     "기간
       END OF TY_DAY.

TYPES: BEGIN OF TY_BASE,
         MATNR      TYPE ZSVPMMSTOCKTIMES-MATNR,                     "자재
         WERKS      TYPE ZSVPMMSTOCKTIMES-WERKS,                     "플랜트
         LGORT      TYPE ZSVPMMSTOCKTIMES-LGORT,                     "저장위치
         LIFNR      TYPE ZSVPMMSTOCKTIMES-LIFNR,                     "공급업체
         TYPECODE   TYPE ZSVPMMSTOCKTIMES-TYPECODE,                  "재고유형코드
         PERIODTYPE TYPE ZSVPMMSTOCKTIMES-PERIODTYPE,                "기간구분
         BATCH      TYPE ZSVPMMSTOCKTIMES-BATCH,                     "BATCH
       END OF TY_BASE.

TYPES: BEGIN OF TY_BASE_A,
         MATNR      TYPE ZSVPMMSTOCKTIMES-MATNR,                     "자재
         WERKS      TYPE ZSVPMMSTOCKTIMES-WERKS,                     "플랜트
         LGORT      TYPE ZSVPMMSTOCKTIMES-LGORT,                     "저장위치
         LIFNR      TYPE ZSVPMMSTOCKTIMES-LIFNR,                     "공급업체
         TYPECODE   TYPE ZSVPMMSTOCKTIMES-TYPECODE,                  "재고유형코드
         PERIODTYPE TYPE ZSVPMMSTOCKTIMES-PERIODTYPE,                "기간구분
       END OF TY_BASE_A.
*----------------------------------------------------------------------*
* ALV Object
*----------------------------------------------------------------------*
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " ALV Gridv

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA : GT_DATA TYPE TABLE OF TY_DATA,
       GT_DISP   TYPE TABLE OF TS_DISP,
       GT_BASE   TYPE SORTED TABLE OF TY_BASE
               WITH UNIQUE KEY MATNR WERKS LGORT LIFNR TYPECODE PERIODTYPE BATCH,
       GT_BASE_A TYPE SORTED TABLE OF TY_BASE_A
               WITH UNIQUE KEY MATNR WERKS LGORT LIFNR TYPECODE PERIODTYPE.



DATA : OK_CODE TYPE SY-UCOMM,
       GV_MODE    TYPE C VALUE '',                          "Display :D  Edit:E (화면편집)
       GV_OK_CODE TYPE SY-UCOMM.

DATA : GV_TCNT TYPE SY-TABIX.

DATA : GV_PERIODTYPE TYPE ZSVPMMSTOCKTIMES-PERIODTYPE, "기간 유형
       GR_TYPECODE   TYPE RANGE OF ZSVPMMSTOCKTIMES-TYPECODE. "재고 유형

DATA : GT_DAY TYPE TABLE OF TY_DAY.

DATA : GS_VARIANT TYPE DISVARIANT. " Variant
DATA : GV_NAME1 TYPE T001W-NAME1,
       GV_LGOBE TYPE T001L-LGOBE,
       GV_DISPO TYPE T024D-DISPO.

DATA : GV_ERROR TYPE C.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_INIT.

  CLEAR:&1, &1[].

END-OF-DEFINITION .

DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .
