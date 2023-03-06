*&---------------------------------------------------------------------*
*& Include          ZRMM1020TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : SSCRFIELDS, MARA, V_MARC_MD, MAKT, T023T, T141T, T024D,
         T024, MAST, STKO, MKAL, ZSVBMMINFOPRICE.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.


*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_SOBSL_30 TYPE SOBSL VALUE '30',
           GC_PB00     TYPE KSCHA VALUE 'PB00'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_MAIN,
         MATKL   TYPE MARA-MATKL,
         WGBEZ   TYPE T023T-WGBEZ,
         MATNR   TYPE MARA-MATNR,
         MAKTX   TYPE MAKT-MAKTX,
         MMSTA   TYPE V_MARC_MD-MMSTA,
         MTSTB   TYPE T141T-MTSTB,
         LAEDA   TYPE MARA-LAEDA,
         WERKS   TYPE V_MARC_MD-WERKS,
         DISPO   TYPE V_MARC_MD-DISPO,
         DSNAM   TYPE T024D-DSNAM,
         EKGRP   TYPE V_MARC_MD-EKGRP,
         EKNAM   TYPE T024-EKNAM,
         BESKZ   TYPE V_MARC_MD-BESKZ,
         SOBSL   TYPE V_MARC_MD-SOBSL,
         BOM_ID  TYPE ICON-ID,
         VER_ID  TYPE ICON-ID,
         INFO_ID TYPE ICON-ID,
         VERID   TYPE MKAL-VERID,
         APLYN   TYPE CHAR1,
         INFNR   TYPE ZSVBMMINFOPRICE-INFNR,
         LIFNR   TYPE ZSVBMMINFOPRICE-LIFNR,
         NAME1   TYPE ZSVBMMINFOPRICE-NAME1,
         EKORG   TYPE ZSVBMMINFOPRICE-PURCHASINGORGANIZATION,
         ESOKZ   TYPE ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY,
         MSG     TYPE BAPI_MSG.
TYPES:
  MSG_MSGTB TYPE ZYCN00001,
  MESSAGE   TYPE BAPI_MSG.
TYPES: END OF TY_MAIN.

"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_MAIN.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
         NEW,                    "신규데이타
       END OF  TS_DISP.

TYPES: BEGIN OF TY_DET,
         EKORG  TYPE ZSVBMMINFOPRICE-PURCHASINGORGANIZATION,
         INFNR  TYPE EINA-INFNR,
         LIFNR  TYPE ZSVBMMINFOPRICE-LIFNR,
         NAME1  TYPE ZSVBMMINFOPRICE-NAME1,
         MATNR  TYPE MARD-MATNR,
         MATKX  TYPE ZSVBMMINFOPRICE-MATERIALDESCRIPTION,
         VERID  TYPE ZSVBMMINFOPRICE-VERID,
         WERKS  TYPE ZSVBMMINFOPRICE-PLANT,
         APLYN  TYPE CHAR1, "적용여부
         ESOKZ  TYPE ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY,
         SWERKS TYPE V_MARC_MD-WERKS, "
       END OF TY_DET.

"*- Alv Dispaly
TYPES: BEGIN OF TS_DET.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DET.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_DET.

TYPES: BEGIN OF TY_BOM_VER_KEY,
         MATNR TYPE MARD-MATNR, "자재
         WERKS TYPE MARC-WERKS. "플랜트
TYPES: END OF TY_BOM_VER_KEY.

TYPES: BEGIN OF TY_INFO_KEY,
         MATNR TYPE MARD-MATNR, "자재
         WERKS TYPE MARC-WERKS. "플랜트
TYPES: END OF TY_INFO_KEY.

TYPES: BEGIN OF TY_BOM,
         MATNR TYPE MARD-MATNR, "자재
         WERKS TYPE MARC-WERKS, "플랜트
         STLNR TYPE MAST-STLNR, "BOM
         DATUV TYPE STKO-DATUV,
       END OF TY_BOM,
       TT_BOM TYPE SORTED TABLE
                    OF TY_BOM
                    WITH NON-UNIQUE KEY MATNR WERKS.

TYPES: BEGIN OF TY_VER,
         MATNR TYPE MARD-MATNR, "자재
         WERKS TYPE MARC-WERKS, "플랜트
         VERID TYPE MKAL-VERID, "생산버젼
         TEXT1 TYPE MKAL-TEXT1,
       END OF TY_VER,
       TT_VER TYPE SORTED TABLE
                    OF TY_VER
                    WITH NON-UNIQUE KEY MATNR WERKS.

TYPES: BEGIN OF TY_INFO,
         MATNR TYPE MARD-MATNR, "자재
         INFNR TYPE EINA-INFNR,
         LIFNR TYPE ZSVBMMINFOPRICE-LIFNR,
         NAME1 TYPE ZSVBMMINFOPRICE-NAME1,
         EKORG TYPE ZSVBMMINFOPRICE-PURCHASINGORGANIZATION,
         ESOKZ TYPE ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY,
         MATKX TYPE ZSVBMMINFOPRICE-MATERIALDESCRIPTION,
         WERKS TYPE ZSVBMMINFOPRICE-PLANT,
         VERID TYPE ZSVBMMINFOPRICE-VERID,
         APLYN TYPE CHAR1,
       END OF TY_INFO,
       TT_INFO TYPE SORTED TABLE
                    OF TY_INFO
                    WITH NON-UNIQUE KEY MATNR WERKS VERID APLYN.

TYPES: BEGIN OF TS_OTHERS,
         BOM  TYPE TT_BOM,
         VER  TYPE TT_VER,
         INFO TYPE TT_INFO,
       END OF TS_OTHERS.

*-
TYPES: BEGIN OF TY_ALV_EMSG,
         ROW_ID    TYPE INT4,
         FIELDNAME TYPE LVC_FNAME,
         MSGV1     TYPE SYMSGV,
         MSGV2     TYPE SYMSGV,
         MSGV3     TYPE SYMSGV,
         MSGV4     TYPE SYMSGV,
       END OF TY_ALV_EMSG.
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

DATA : GV_CONTAINER_0200 TYPE SCRFNAME VALUE 'CC0200',
       GRF_CUSTOM_CONTAINER_0200 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_MAIN TYPE STANDARD TABLE OF TY_MAIN,
      GT_DISP        TYPE STANDARD TABLE OF TS_DISP
               WITH NON-UNIQUE SORTED KEY IDX_STATU COMPONENTS STATU
               WITH NON-UNIQUE SORTED KEY IDX_KEY   COMPONENTS MATNR WERKS,
      GT_BOM_VER_KEY TYPE SORTED TABLE OF TY_BOM_VER_KEY
                       WITH UNIQUE KEY MATNR WERKS,
      GT_INFO_KEY    TYPE SORTED TABLE OF TY_INFO_KEY
                     WITH UNIQUE KEY MATNR WERKS,
      GT_OTHERS      TYPE TABLE OF TS_OTHERS WITH HEADER LINE,
      GT_DET         TYPE TABLE OF TS_DET
                     WITH NON-UNIQUE SORTED KEY IDX_VERID COMPONENTS VERID,
      GT_VERID       TYPE STANDARD TABLE OF TY_VER
                     WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS VERID.

DATA: GV_MULTI_VER.

DATA: GR_VALIDPRICE TYPE RANGE OF ZSVBMMINFOPRICE-VALIDPRICE.

DATA: GV_TCNT TYPE SY-TABIX, GV_SCNT TYPE SY-TABIX, GV_ECNT TYPE SY-TABIX.

DATA:GS_VARIANT TYPE DISVARIANT. " Variant

DATA: OK_CODE TYPE SY-UCOMM,
      OK_CODE_0200    TYPE SY-UCOMM,
      GV_MODE         TYPE C VALUE '',   "Display :D  Edit:E (화면편집)
      GV_OK_CODE      TYPE SY-UCOMM,
      GV_OK_CODE_0200 TYPE SY-UCOMM.

DATA:GS_MSGTB TYPE ZSCN00001,
     GV_MSGTB TYPE ZYCN00001.

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
  CLEAR:&1[].

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
DEFINE _G_CONV_USING_TIMEZONE.
  zcl_cng_abap_util=>conv_using_timezone( EXPORTING iv_conv_type = &1   " 'S': Local -> System, 'L' System -> Local, 'Z' timezone
                                           CHANGING cv_date      = &2        " Date
                                                    cv_time      = &3 ).     " Time
END-OF-DEFINITION.
