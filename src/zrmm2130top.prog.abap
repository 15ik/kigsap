*&---------------------------------------------------------------------*
*& Include          ZRMM2130TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : ZTMM00002, MARA, V_MARC_MD, MBEW, EORD, EKKO, EKPO,
         LFA1, T001W, ZSVBMMINFOPRICE, MDLV.
*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.
*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_FC01(4) TYPE C VALUE 'FC01'.
*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATE.
         include structure ZSMM_R2130_LIST.
TYPES: MGSUM TYPE FGD_QNT, "기말(예상)재고수량 Sum
         QTY_EXIST TYPE CHAR1.  "수량존재여부
TYPES: END OF TY_DATE.

*-
TYPES: BEGIN OF TY_MAIN,
         KEY        TYPE CHAR60,
         WERKS      TYPE V_MARC_MD-WERKS, "플랜트
         NAME1      TYPE T001W-NAME1,     "플랜트 명
         BERID      TYPE PPH_DBVM-BERID,  "MRP영역
         MATKL      TYPE MARA-MATKL,      "자재그룹
         WGBEZ      TYPE T023T-WGBEZ,     "자재그룹명
         WGBEZ60    TYPE T023T-WGBEZ60,   "자재그룹명2
         MATNR      TYPE V_MARC_MD-MATNR, "자재
         MAKTX      TYPE MAKT-MAKTX,      "자재내역
         LIFNR      TYPE LFA1-LIFNR,      "공급업체
         LFA1_NAME1 TYPE LFA1-NAME1,      "공급업체명
         MAT_TEXT   TYPE TDLINE,  "기본텍스트
         MEINS      TYPE MEINS.
TYPES: END OF TY_MAIN.

"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_MAIN.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
         INCLUDE TYPE TY_DATE.
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_DISP.

*-
TYPES: BEGIN OF TY_MRP_DAT,
         OPT   TYPE CHAR2,
         MATNR TYPE ZTMM20010-MATNR,
         WERKS TYPE ZTMM20010-WERKS,
         MNG01 TYPE ZTMM20010-MNG01,
         DAT01 TYPE ZTMM20010-DAT01,
         DELET TYPE ZTMM20010-DELET,
         ETMEN TYPE ZTMM20010-ETMEN,
         VRFKZ TYPE ZTMM20010-VRFKZ,
       END OF TY_MRP_DAT.

*-
TYPES: BEGIN OF TY_BASE_DAT,
         MATNR TYPE MARA-MATNR,
         WERKS TYPE T001W-WERKS,
       END OF TY_BASE_DAT.
*-
TYPES: BEGIN OF TY_BASE_BERID_DAT,
         MATNR TYPE MARA-MATNR,
         WERKS TYPE T001W-WERKS,
         BERID TYPE BERID,
       END OF TY_BASE_BERID_DAT.
*-
TYPES: BEGIN OF TY_DAY,
         SEQ     TYPE SY-TABIX,
         DATE    TYPE SY-DATLO,
         DAY     TYPE CHAR6,
         DAY_TXT TYPE CHAR10,
       END OF TY_DAY.
*-
TYPES: BEGIN OF TY_HOLIDAY,
         WERKS TYPE T001W-WERKS,
         DAY   TYPE CHAR6,
       END OF TY_HOLIDAY.
*-
TYPES: BEGIN OF TY_CONF_DAY,
         WERKS TYPE T001W-WERKS,
         DATE  TYPE SY-DATLO,
         DAY   TYPE CHAR6,
       END OF TY_CONF_DAY.

*-
TYPES: BEGIN OF TY_EORD,
         MATNR TYPE EORD-MATNR,
         WERKS TYPE EORD-WERKS,
         EBELN TYPE EORD-EBELN,
         EBELP TYPE EORD-EBELP,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
         VDATU TYPE EORD-VDATU,
         BDATU TYPE EORD-BDATU,
         NOTKZ TYPE EORD-NOTKZ,
       END OF TY_EORD.

*-
TYPES: BEGIN OF TY_PRD,
         PRD    TYPE CHAR1,
         PRD_NM TYPE CHAR20,
         BPCD   TYPE CHAR10,
         PRDFN  TYPE CHAR5,
       END OF TY_PRD.
*-
TYPES: BEGIN OF TY_PLANWH,
         MATNR TYPE EORD-MATNR,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TY_PLANWH.

*-
TYPES: BEGIN OF TY_STB,
         MATNR TYPE MARC-MATNR,
         WERKS TYPE MARC-WERKS,
       END OF TY_STB.

"*- Alv Dispaly
TYPES: BEGIN OF TS_BOM.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         include TYPE ZSMAT.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_BOM.

*-
TYPES: BEGIN OF TY_BDC_WRK,
         WERKS TYPE T001W-WERKS,
       END OF TY_BDC_WRK.
*-
TYPES: BEGIN OF TY_BDC_MAT,
         MATNR TYPE MATNR_D,
       END OF TY_BDC_MAT.

*----------------------------------------------------------------------*
* Class `
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID, " ALV Gridv
       GRF_GRID_0200 TYPE REF TO LCL_CUST_ALV_GRID.

DATA: GV_CONTAINER TYPE SCRFNAME VALUE 'CC0200',
      GRF_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_MAIN TYPE STANDARD TABLE OF TY_MAIN,
      GT_DISP     TYPE STANDARD TABLE OF TS_DISP
                  WITH NON-UNIQUE SORTED KEY IDX_STATU COMPONENTS STATU,
      GT_DISP_BK  TYPE STANDARD TABLE OF TS_DISP
                  WITH NON-UNIQUE SORTED KEY IDX_STATU COMPONENTS STATU,
      GT_STB      TYPE STANDARD TABLE OF TY_STB,
      GT_SEL_LIST TYPE TABLE OF EDPLINE,
      GT_BASE_DAT TYPE SORTED TABLE OF TY_BASE_DAT
                    WITH UNIQUE KEY MATNR WERKS,
      GT_BASE_BERID_DAT       TYPE SORTED TABLE OF TY_BASE_BERID_DAT
                    WITH UNIQUE KEY MATNR WERKS BERID,
      GT_DATE     TYPE TABLE OF TY_DATE
                    WITH NON-UNIQUE SORTED KEY IDX_KEY       COMPONENTS OPT
                    WITH NON-UNIQUE SORTED KEY IDX_KEY_EXIST COMPONENTS QTY_EXIST,
      GT_DAY      TYPE TABLE OF TY_DAY
                    WITH NON-UNIQUE SORTED KEY IDX_DAY COMPONENTS DAY,
      GT_HOLIDAY  TYPE TABLE OF TY_HOLIDAY
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS WERKS DAY,
      GT_CONF_DAY TYPE TABLE OF TY_CONF_DAY
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS WERKS,
      GT_EORD     TYPE STANDARD TABLE OF TY_EORD
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS MATNR WERKS,
      GT_EKPO     TYPE STANDARD TABLE OF TY_EORD
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS MATNR WERKS,
      GT_PRD      TYPE STANDARD TABLE OF TY_PRD,
      GT_PLANWH   TYPE STANDARD TABLE OF TY_PLANWH
                  WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS MATNR,
      GT_INFOP    TYPE STANDARD TABLE OF TY_PLANWH
                  WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS MATNR,
      GT_BOM      TYPE TABLE OF TS_BOM,
      GT_BDC_WRK  TYPE STANDARD TABLE OF TY_BDC_WRK,
      GT_BDC_MAT  TYPE STANDARD TABLE OF TY_BDC_MAT.

DATA: GT_COMM_CFG_A TYPE TABLE OF ZTMM00002,
      GT_COMM_CFG_B    TYPE TABLE OF ZTMM00002
                    WITH NON-UNIQUE SORTED KEY IDX_FIELD2 COMPONENTS FIELD2,
      GT_COMM_CFG_C    TYPE TABLE OF ZTMM00002,
      GT_COMM_CFG_D    TYPE TABLE OF ZTMM00002
                    WITH NON-UNIQUE SORTED KEY IDX_FIELD1 COMPONENTS FIELD1,
      GT_COMM_CFG_B312 TYPE TABLE OF ZTMM00002,
      GT_MRP_ORG       TYPE STANDARD TABLE OF ZTMM20010,
      GT_MRP_DAT       TYPE TABLE OF TY_MRP_DAT
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS OPT MATNR WERKS.

DATA: GR_PWERKS LIKE RANGE OF T001W-WERKS WITH HEADER LINE,
      GT_STB_MATNR TYPE STANDARD TABLE OF TY_STB.

DATA: GV_BASE_STK TYPE ZTMM2001H-MNG01,
      GV_MATNR_0200  TYPE MATNR,
      GV_FIRST_DAY   TYPE DAT01,
      GV_DAY_DO      TYPE SY-TABIX,
      GV_CONFIRM_DAY TYPE DAT01,
      GV_CONFIRM_DD  TYPE NUMC2.

DATA: GV_TCNT TYPE SY-TABIX.

DATA: OK_CODE TYPE SY-UCOMM,
      OK_CODE_0200    TYPE SY-UCOMM,
      GV_MODE         TYPE C VALUE '',   "Display :D  Edit:E (화면편집)
      GV_OK_CODE      TYPE SY-UCOMM,
      GV_OK_CODE_0200 TYPE SY-UCOMM.

DATA: GS_MSGTB TYPE ZSCN00001.

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
  CLEAR:&1, &1[].

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
