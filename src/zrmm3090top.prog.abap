*&---------------------------------------------------------------------*
*& Include          ZRMM3090TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES: T001, ZSVCMM_EKET1.


*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.


*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_KALSK_DO TYPE LFM1-KALSK VALUE 'DO',
           GC_VEBLN_MULTI(5) TYPE C VALUE '***'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
*>Main
TYPES: BEGIN OF TY_DATA,
         LIFNR                TYPE ZSVCMM_EKET1-LIFNR,   "공급업체
         NAME1                TYPE ZSVCMM_EKET1-NAME1,   "업체명
         EBELN                TYPE ZSVCMM_EKET1-EBELN,   "구매오더
         EBELP                TYPE ZSVCMM_EKET1-EBELP,   "품목
         MATNR                TYPE ZSVCMM_EKET1-MATNR,   "자재
         TXZ01                TYPE ZSVCMM_EKET1-TXZ01,   "내역
         BWTAR                TYPE ZSVCMM_EKET1-BWTAR,   "평가유형
         MEINS                TYPE ZSVCMM_EKET1-MEINS,   "단위
         MENGE                TYPE ZSVCMM_EKET1-MENGE,   "PO수량
         ETENR                TYPE ZSVCMM_EKET1-ETENR,   "납품일정
         BSTAE                TYPE ZSVCMM_EKET1-BSTAE,   "확인관리키
         EINDT                TYPE ZSVCMM_EKET1-EINDT,   "납품일
         ELIKZ                TYPE EKPO-ELIKZ,           "납품완료
         EKET_QTY             TYPE ZSVCMM_EKET1-EKET_QTY,   "납품예정수량
         ID_QTY               TYPE ZSVCMM_EKET1-ID_QTY,   "납품서수량
         GR_QTY               TYPE ZSVCMM_EKET1-GR_QTY,   "입고수량
         REMAIN_ID            TYPE ZSVCMM_EKET1-REMAIN_ID,   "납품미입고
         REMAIN_GR            TYPE ZSVCMM_EKET1-REMAIN_GR,   "미입고잔량
         WERKS                TYPE ZSVCMM_EKET1-WERKS,   "플랜트
         LGORT                TYPE ZSVCMM_EKET1-LGORT,   "창고
         VBELN                TYPE EKES-VBELN,           "납품번호 (대표)
         BSART                TYPE ZSVCMM_EKET1-BSART,   "구매문서유형
         BATXT                TYPE ZSVCMM_EKET1-BATXT,   "문서유형 내역
         RETPO                TYPE ZSVCMM_EKET1-RETPO,   "반품품목
         ZORDER_PERSON        TYPE ZSVCMM_EKET1-ZORDER_PERSON,   "계약담당자
*         ZORDER_PERSON_NM     TYPE ZTCN00002-USER_NM,   "계약담당자명
         ZORDER_DEPARTMENT    TYPE ZSVCMM_EKET1-ZORDER_DEPARTMENT,   "계약부서
*         ZORDER_DEPARTMENT_NM TYPE ZTCN00001-ORGN_NM,   "계약부서명
         ZEXPEN_PERSON        TYPE ZSVCMM_EKET1-ZEXPEN_PERSON,   "지출발의담당자
*         ZEXPEN_PERSON_NM     TYPE ZTCN00002-USER_NM,   "자출발의담당자명
         ZEXPEN_DEPARTMENT    TYPE ZSVCMM_EKET1-ZEXPEN_DEPARTMENT,   "지출발의부서
*         ZEXPEN_DEPARTMENT_NM TYPE ZTCN00001-ORGN_NM,   "지출발의부서명
         AEDAT_PO             TYPE ZSVCMM_EKET1-AEDAT_PO,  "(+)U2
       END OF TY_DATA.

*> Inbound D/O Popup
TYPES: BEGIN OF TY_DATA_0200,
         VBELN TYPE EKES-VBELN,   "납품
         VBELP TYPE EKES-VBELP,   "납품품목
         EINDT TYPE EKES-EINDT,  "납품일
         MENGE TYPE EKES-MENGE,  "수량

*> NO-DISPLAY
         EBELN TYPE EKES-EBELN,   "구매오더
         EBELP TYPE EKES-EBELP,   "구매오더품목
         ETENS TYPE EKES-ETENS,   "순번
         MEINS TYPE ZSVCMM_EKET1-MEINS,  "단위
       END OF TY_DATA_0200.
*--------------------------------
* F4 Type 선언
*--------------------------------
"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.  "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: END OF TS_DISP.

TYPES: BEGIN OF TS_DISP_0200.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA_0200.  "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: END OF TS_DISP_0200.
*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
*DATA:grf_co TYPE REF TO ycl_co_common.


*---------------------
*-- ALV Object
*---------------------
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " 상(Header) ALV Grid

DATA : GRF_DOCKING_CON_0200 TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA : GRF_GRID_0200 TYPE REF TO LCL_CUST_ALV_GRID. " ALV Grid 0200

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP TYPE TABLE OF TS_DISP.
DATA: GT_ALL_0200 TYPE TABLE OF TS_DISP_0200,
      GT_DISP_0200 TYPE TABLE OF TS_DISP_0200.

DATA: GS_VARIANT TYPE DISVARIANT. " Variant

DATA: OK_CODE TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
      GV_MODE,
      GV_MROW    TYPE I           VALUE 10.                   "Multi Row

DATA: GS_MSGTB TYPE ZSCN00001.

DATA GV_EXC_USER.

DATA: OK_CODE_0200 TYPE SY-UCOMM,
      GV_OK_CODE_0200 TYPE SY-UCOMM,
      GV_MODE_0200,
*      GV_MROW_0200    TYPE I           VALUE 10,                   "Multi Row
      GV_0200_TITLE TYPE CHAR200.
*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .

DEFINE _G_INIT.
  REFRESH:&1.

END-OF-DEFINITION .
DEFINE _G_GET_CELL_VALUE.
  CALL METHOD irf_data_changed->get_cell_value
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
    IMPORTING
      e_value     = &3.

END-OF-DEFINITION .
DEFINE _G_MODIFY_CELL.
  CALL METHOD irf_data_changed->modify_cell
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
      i_value     = &3.

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
DEFINE _G_SET_VALUE.
  &1 = &2.
END-OF-DEFINITION .
DEFINE _G_ADD_1.

  &1 = &1 + 1.

END-OF-DEFINITION .
DEFINE _G_WAIT_1_SECOND.

  WAIT UP TO '0.5' SECONDS.

END-OF-DEFINITION .
