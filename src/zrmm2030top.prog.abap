*&---------------------------------------------------------------------*
*& Include          ZRMM2020TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : SSCRFIELDS, MARA, V_MARC_MD, PPH_DBVM, T001W,
         ZTMM20020, ZTMM00002, PLSC, S194.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_FC01(4) TYPE C VALUE 'FC01',
           GC_BERTY_02 TYPE BERTY VALUE '02'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATE.
         include structure ZSMM_DATE_VALUE.
TYPES: STOCK TYPE ZTMM20020-MNG01,
         MGSUM TYPE ZTMM20020-MNG01. "소요량 Sum
TYPES: END OF TY_DATE.
TYPES: BEGIN OF TY_DATA,
         WERKS TYPE T001W-WERKS,
         NAME1 TYPE T001W-NAME1,
         PLSCN TYPE PLSC-PLSCN,
         PLSCT TYPE PLSC-PLSCT,
         BERID TYPE MDLV-BERID,
         BERTX TYPE MDLV-BERTX,
         MATNR TYPE MARA-MATNR,
         MAKTX TYPE MAKT-MAKTX,
         MEINS TYPE MARA-MEINS,
         MMSTA TYPE V_MARC_MD-MMSTA,
         MTSTB TYPE T141T-MTSTB.
TYPES: END OF TY_DATA.

"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
         INCLUDE TYPE TY_DATE.
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_DISP.

TYPES: BEGIN OF TY_DAY,
         SEQ     TYPE SY-TABIX,
         DATE    TYPE SY-DATLO,
         DAY     TYPE CHAR6,
         DAY_TXT TYPE CHAR10,
       END OF TY_DAY.

*-
TYPES: BEGIN OF TY_BASE_DAT,
         MATNR TYPE V_MARC_MD-MATNR,
         WERKS TYPE V_MARC_MD-WERKS,
       END OF TY_BASE_DAT.
*-
TYPES: BEGIN OF TY_LTP_DAT,
         OPT   TYPE CHAR2,
         MATNR TYPE ZTMM20020-MATNR,
         WERKS TYPE ZTMM20020-WERKS,
         MNG01 TYPE ZTMM20020-MNG01,
         DAT00 TYPE ZTMM20020-DAT00,
       END OF TY_LTP_DAT.

*-
TYPES: BEGIN OF TY_REQ_LTP_DAT,
         MATNR TYPE ZTMM20020-MATNR,
         WERKS TYPE ZTMM20020-WERKS,
         DELVR TYPE ZTMM20020-DELVR,
         MNG01 TYPE ZTMM20020-MNG01,
         DAT00 TYPE ZTMM20020-DAT00,
         BAUGR TYPE ZTMM20020-BAUGR,
         AUFVR TYPE ZTMM20020-AUFVR,
       END OF TY_REQ_LTP_DAT.

*-
TYPES: BEGIN OF TY_REQ_DT,
         MATNR      TYPE MATNR_D,
         MAKTX      TYPE MAKTX,
         GSMNG      TYPE PLAF-GSMNG,
         MARA_MEINS TYPE MARA-MEINS,
         REQ_QTY    TYPE ZTMM20010-MNG01, "소요량
         MEINS      TYPE MEINS,
       END OF TY_REQ_DT.
"*- Alv Dispaly
TYPES: BEGIN OF TS_REQ_DT.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_REQ_DT.  "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_REQ_DT.

*-
TYPES: BEGIN OF TY_PLAF,
         PLNUM TYPE PLAF-PLNUM,
         MATNR TYPE PLAF-MATNR,
         GSMNG TYPE PLAF-GSMNG,
       END OF TY_PLAF.

*-
TYPES: BEGIN OF TY_MAIN_KEY,
         MATNR TYPE V_MARD_MD-MATNR,
         WERKS TYPE V_MARD_MD-WERKS.
TYPES: END OF TY_MAIN_KEY.

*-
TYPES: BEGIN OF TY_STB,
         MATNR TYPE MARC-MATNR,
         WERKS TYPE MARC-WERKS,
       END OF TY_STB.

*----------------------------------------------------------------------*
* Class `
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_HEAD_0200 TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY      TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID, " ALV Gridv
       GRF_GRID_0200 TYPE REF TO LCL_CUST_ALV_GRID.

DATA :
  GV_CONTAINER_0200         TYPE SCRFNAME VALUE 'CC0200',
  GRF_CUSTOM_CONTAINER_0200 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_MAIN TYPE TABLE OF TY_DATA,
      GT_DISP          TYPE TABLE OF TS_DISP,
      GT_DAY           TYPE TABLE OF TY_DAY
                       WITH NON-UNIQUE SORTED KEY IDX_DAY COMPONENTS DAY,
      GT_BASE_DAT      TYPE SORTED TABLE OF TY_BASE_DAT
                       WITH UNIQUE KEY MATNR WERKS,
      GT_SEL_LIST      TYPE TABLE OF EDPLINE,
      GT_LTP_ORG       TYPE STANDARD TABLE OF ZTMM20020,
      GT_LTP_ORG_20010 TYPE STANDARD TABLE OF ZTMM20010,
      GT_LTP_DAT       TYPE TABLE OF TY_LTP_DAT
                       WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS OPT MATNR WERKS,
      GT_DATE          TYPE TABLE OF TY_DATE
                       WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS OPT,
      GT_STB_MATNR  TYPE STANDARD TABLE OF TY_STB,
      GT_STB           TYPE STANDARD TABLE OF TY_MAIN_KEY,
      GT_BERID         TYPE STANDARD TABLE OF MDLV,
      GT_COMM_CFG_A    TYPE TABLE OF ZTMM00002,
      GT_COMM_CFG_B    TYPE TABLE OF ZTMM00002,
      GT_COMM_CFG_C    TYPE TABLE OF ZTMM00002
                       WITH NON-UNIQUE SORTED KEY IDX_KEY  COMPONENTS FIELD1
                       WITH NON-UNIQUE SORTED KEY IDX_TKEY COMPONENTS FIELD11,
      GT_COMM_CFG_D    TYPE TABLE OF ZTMM00002,
      GT_REQ_LTP_DAT   TYPE STANDARD TABLE OF TY_REQ_LTP_DAT,
      GT_REQ_DT        TYPE STANDARD TABLE OF TS_REQ_DT,
      GT_PLAF          TYPE STANDARD TABLE OF TY_PLAF,
      GT_COMM_CFG_B312 TYPE TABLE OF ZTMM00002.

DATA: GT_BERID_R TYPE RANGE OF BERID.

DATA: GV_FIRST_DAY TYPE DAT01,
      GV_FLAST_DAY  TYPE DAT01,
      GV_DAY_DO     TYPE SY-TABIX,
      GV_BASE_STK   TYPE ZTMM2001H-MNG01,
      GV_MATNR_0200 TYPE MATNR.

DATA: OK_CODE TYPE SY-UCOMM,
      OK_CODE_0200    TYPE SY-UCOMM,
      GV_OK_CODE      TYPE SY-UCOMM,
      GV_OK_CODE_0200 TYPE SY-UCOMM.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
************************************************************************
DEFINE _G_APPEND_VALUE.
  &1-OPTION = &2.
  &1-SIGN   = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1.
END-OF-DEFINITION.

************************************************************************
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .

DEFINE _G_INIT.
  CLEAR: &1, &1[].

END-OF-DEFINITION .
DEFINE _G_SET_MSGTB.

  CLEAR:gs_msgtb.

    gs_msgtb = VALUE #( fieldname = &3   msgty = 'E'  arbgb = &4   txtnr = &5
                        msgv1     = &6   msgv2 = &7   msgv3 = &8  ).

  grf_grid->set_msgtb( EXPORTING iv_delete = &1
                                 is_msgtb  = gs_msgtb
                       CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _G_SET_TIMESTMAP.


  zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = &1
                                  CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _G_ENTER_FUNCTIONCODE.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE           = 'ENTER'
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED = 1
      OTHERS                 = 2.

END-OF-DEFINITION .
DEFINE _G_WAIT_1_SECOND.

  WAIT UP TO '0.5' SECONDS.

END-OF-DEFINITION .
