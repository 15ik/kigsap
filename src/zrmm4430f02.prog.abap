*&---------------------------------------------------------------------*
*& Include          ZRMM4430F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID .

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docing container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID.

*--------------------------------
* Create Item Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_ITEM.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_DISP ).

*--------------------------------
* Dislay Item Grid..
*--------------------------------
    GRF_ITEM->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_DISP_ITEM ).

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER .

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON
    EXPORTING
      REPID     = SY-REPID " 프로그램명 id
      DYNNR     = SY-DYNNR " 화면번호 (Screen) "
      SIDE      = GRF_DOCKING_CON->DOCK_AT_TOP "
      EXTENSION = 10000.


*----------------------------------------------------
* Split Container (1 Row:header 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(LRF_SPLITTER) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = GRF_DOCKING_CON
                                                      NO_AUTODEF_PROGID_DYNNR = 'X'
                                                      ROWS    = 2
                                                      COLUMNS = 1 ).
  LRF_SPLITTER->SET_ROW_MODE( MODE = CL_GUI_SPLITTER_CONTAINER=>TYPE_MOVABLE ).
  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 110 ).
  LRF_SPLITTER->SET_BORDER( BORDER = SPACE ).

*--------------------------------
* Set Header Container
*--------------------------------
  DATA(LRF_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

  DATA(LRF_SPLITTER_HTML) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_CONT
                                                           NO_AUTODEF_PROGID_DYNNR = 'X'
                                                           ROWS    = 1
                                                           COLUMNS = 1 ).
  GRF_HEAD = LRF_SPLITTER_HTML->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

  "--------------------------------
  "- Set Body Container
  "--------------------------------
  DATA(LRF_GRID_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

  DATA(LRF_SPLIT_BDY) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_GRID_CONT
                                                       NO_AUTODEF_PROGID_DYNNR = 'X'
                                                       ROWS    = 2
                                                       COLUMNS = 1 ).

*-- Field 비율
  LRF_SPLIT_BDY->SET_ROW_HEIGHT( EXPORTING ID = 1 HEIGHT = 70 ).
  LRF_SPLIT_BDY->SET_ROW_HEIGHT( EXPORTING ID = 2 HEIGHT = 30 ).

*-- Set Alv Container..
  GRF_BODY      = LRF_SPLIT_BDY->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
  GRF_BODY_ITEM = LRF_SPLIT_BDY->GET_CONTAINER( ROW = 2 COLUMN = 1 ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID .
  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD,
       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*  LS_TOOLBTN-BTN_INFO  = 'X'.       "Batch Search
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM10010'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
**  SELECT SINGLE BUTXT,WAERS
**    INTO @DATA(LS_DATA)
**    FROM T001
**   WHERE BUKRS = @P_BUKRS.
**
**  _L_ADD_FIELD: 'KOKRS' P_KOKRS,
**                'BUKRS' P_BUKRS,
**                'BUTXT' LS_DATA-BUTXT,
**                'WAERS' LS_DATA-WAERS,
**                'VERSN' P_VERSN.

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

*--------------------------------------------------
* Set Lock Name..
*--------------------------------------------------
  DATA: LV_TIMESTAMP_OUT TYPE TIMESTAMP.
  GET TIME STAMP FIELD LV_TIMESTAMP_OUT.
*  DATA(LV_LOCK_NM) = S_LAEDA-LOW. "LV_TIMESTAMP_OUT.

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD
*     iv_scr_wr  = '20:10:70'
      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.
*  DATA : LV_TEXT(255) TYPE C.

  DATA : LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
    IF &2 IS NOT INITIAL.
    CLEAR ls_header.
    ls_header-key  = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
    ENDIF.
  END-OF-DEFINITION.

**  CONSTANTS : lc_if(20) TYPE c VALUE 'Controlling Area '.


*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외
*  CLEAR: LV_TEXT.
*
*  SELECT SINGLE bezei INTO lv_text
*    FROM tka01
*   WHERE kokrs = p_kokrs.
*
*  IF s_spmon[] IS NOT INITIAL.
*    READ TABLE s_spmon INDEX 1.
*    IF s_spmon-high IS NOT INITIAL.
*      CONCATENATE s_spmon-low '~' s_spmon-high INTO DATA(lv_spmon).
*    ELSE.
*      lv_spmon = s_spmon-low.
*    ENDIF.
*  ENDIF.

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER : TEXT-H01  P_BUKRS  '' .
*  _L_SET_HEADER : TEXT-H02  S_WERKS-LOW  '' .

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_ITEM .

  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.


  CREATE OBJECT GRF_ITEM
    EXPORTING
      IV_NAME    = 'ALV_ITEM'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY_ITEM
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD.
*     iv_scr_wr  = '20:10:70'
*      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_exit .

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_110
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_data_110 .

  PERFORM get_data.
*  PERFORM PROCESSING_DATA.

  _g_init : gt_disp_item.

  grf_grid->refresh_grid_display( ).
  grf_item->refresh_grid_display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_OTHERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> LS_LVC_MODI
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_OTHERS USING IRF_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                   IS_CELL TYPE LVC_S_MODI
                           CHANGING CS_DISP TYPE ANY.


*-------------------
* Delete Message
*-------------------
  CHECK SY-SUBRC = 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_on_top_down_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_TOP_DOWN_INIT .

  DATA : LT_SELIDX    TYPE LVC_T_ROW.

  DATA : LT_SUB     TYPE TABLE OF TS_DISP,    "중간 집계용
         LT_SUB_PRO TYPE TABLE OF TS_DISP,    "생산오더 집계용
         LT_SUB_PUR TYPE TABLE OF TS_DISP,    "구매오더 집계용
         LT_SUB_STO TYPE TABLE OF TS_DISP.    "재고 집계용

  _G_INIT: GT_DISP_ITEM.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S006(ZMM01) DISPLAY LIKE 'I'. EXIT. "선택된 데이타가 없습니다.
  ELSEIF LV_TSELLINES > 1.
    MESSAGE S007(ZMM01) DISPLAY LIKE 'E'. EXIT. "1 건의 데이타만 선택 가능 합니다.
  ENDIF.

  READ TABLE LT_SELIDX INTO DATA(LV_SELIDX) INDEX 1.
*-

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LV_SELIDX-INDEX.

  IF LS_DISP IS NOT INITIAL.

    APPEND LS_DISP TO LT_SUB.               "중간 집계용
    "    APPEND LS_DISP TO GT_DISP_ITEM.        "최종 ITEM ALV Display 용

  ENDIF.
*-

  WHILE LT_SUB[] IS NOT INITIAL.

    LOOP AT LT_SUB ASSIGNING FIELD-SYMBOL(<LS_SUB>) WHERE CHARG NE ''.
*      IF <LS_SUB>-AUFNR IS NOT INITIAL AND ( <LS_SUB>-BWART ne GC_261 and <LS_SUB>-BWART ne GC_262 ).
      IF <LS_SUB>-AUFNR IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PRO.              " 생산오더 집계용
*      ELSEIF <LS_SUB>-EBELN IS NOT INITIAL AND ( <LS_SUB>-BWART ne GC_543 and <LS_SUB>-BWART = GC_544 ).
      ELSEIF <LS_SUB>-EBELN IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PUR.              " 구매오더 집계용
*      ELSEIF <LS_SUB>-AUFNR IS INITIAL AND LS_DISP-EBELN IS INITIAL.
      ELSE.
        APPEND <LS_SUB> TO LT_SUB_STO.              " 재고 집계용
      ENDIF.
    ENDLOOP.

    CLEAR : LT_SUB, LT_SUB[].

    IF LT_SUB_PRO[] IS NOT INITIAL.
      PERFORM TOP_DOWN_PRO TABLES LT_SUB_PRO LT_SUB.
      _G_INIT LT_SUB_PRO.
    ENDIF.

    IF LT_SUB_PUR[] IS NOT INITIAL.
      PERFORM TOP_DOWN_PUR TABLES LT_SUB_PUR LT_SUB.
      _G_INIT LT_SUB_PUR.
    ENDIF.

    IF LT_SUB_STO[] IS NOT INITIAL.
      PERFORM TOP_DOWN_STO TABLES LT_SUB_STO LT_SUB.
      _G_INIT LT_SUB_STO.
    ENDIF.

  ENDWHILE.

  IF GT_DISP_ITEM[] IS NOT INITIAL.

*- U2 START > T2 일 경우에 영문화 적용.
    IF GV_LANGU NE '3'.
      LOOP AT GT_DISP_ITEM INTO DATA(LS_DISP_ITEM).

        READ TABLE GT_MVTTYPE INTO DATA(LS_MVT) WITH KEY MVT_KO = LS_DISP_ITEM-MVTTYPE.
        IF SY-SUBRC = 0.
          LS_DISP_ITEM-MVTTYPE = LS_MVT-MVT_EN.
        ENDIF.

        MODIFY GT_DISP_ITEM FROM LS_DISP_ITEM.

      ENDLOOP.
    ENDIF.
*- U2 END > T2 일 경우에 영문화 적용.

*    SORT GT_DISP_ITEM BY MATNR ASCENDING  CPUDT DESCENDING CPUTM DESCENDING.
    SORT GT_DISP_ITEM BY  CPUDT DESCENDING CPUTM DESCENDING MATNR CHARG.

    DELETE ADJACENT DUPLICATES FROM GT_DISP_ITEM COMPARING ALL FIELDS.

  ENDIF.

  GRF_ITEM->REFRESH_GRID_DISPLAY( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_on_all_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_ALL_INIT .

  DATA : LT_SELIDX    TYPE LVC_T_ROW.

  DATA : LT_SUB     TYPE TABLE OF TS_DISP,    "중간 집계용
         LT_SUB_PRO TYPE TABLE OF TS_DISP,    "생산오더 집계용
         LT_SUB_PUR TYPE TABLE OF TS_DISP,    "구매오더 집계용
         LT_SUB_STO TYPE TABLE OF TS_DISP.    "재고 집계용

  _G_INIT: GT_DISP_ITEM.

  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S006(ZMM01) DISPLAY LIKE 'I'. EXIT. "선택된 데이타가 없습니다.
  ELSEIF LV_TSELLINES > 1.
    MESSAGE S007(ZMM01) DISPLAY LIKE 'E'. EXIT. "1 건의 데이타만 선택 가능 합니다.
  ENDIF.

  READ TABLE LT_SELIDX INTO DATA(LV_SELIDX) INDEX 1.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LV_SELIDX-INDEX.

  IF LS_DISP IS NOT INITIAL.

    APPEND LS_DISP TO LT_SUB.              "중간 집계용
    APPEND LS_DISP TO GT_DISP_ITEM.        "최종 ITEM ALV Display 용 최초 적재

  ENDIF.

  WHILE LT_SUB[] IS NOT INITIAL.

    LOOP AT LT_SUB ASSIGNING FIELD-SYMBOL(<LS_SUB>).    "분류작업

      IF <LS_SUB>-AUFNR IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PRO.              " 생산오더 집계용
      ELSEIF <LS_SUB>-EBELN IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PUR.            " 구매오더 집계용
      ELSEIF <LS_SUB>-AUFNR IS INITIAL AND LS_DISP-EBELN IS INITIAL.
        APPEND <LS_SUB> TO LT_SUB_STO.            " 재고 집계용
      ENDIF.

    ENDLOOP.

    CLEAR : LT_SUB, LT_SUB[].

    IF LT_SUB_PRO[] IS NOT INITIAL.

      PERFORM ALL_PRO TABLES LT_SUB_PRO LT_SUB.
      _G_INIT LT_SUB_PRO.

    ELSEIF LT_SUB_PUR[] IS NOT INITIAL.

      PERFORM ALL_PUR TABLES LT_SUB_PUR LT_SUB.
      _G_INIT LT_SUB_PUR.

    ELSEIF LT_SUB_STO[] IS NOT INITIAL.

      PERFORM ALL_STO TABLES LT_SUB_STO LT_SUB.
      _G_INIT LT_SUB_STO.
    ENDIF.

  ENDWHILE.

  IF GT_DISP_ITEM[] IS NOT INITIAL.

    SORT GT_DISP_ITEM BY BUDAT DESCENDING CPUDT DESCENDING CPUTM DESCENDING AUFNR DESCENDING AUFPS DESCENDING EBELN DESCENDING EBELP DESCENDING
                          MATNR ASCENDING  CHARG ASCENDING.

    DELETE ADJACENT DUPLICATES FROM GT_DISP_ITEM COMPARING ALL FIELDS.


  ENDIF.

  GRF_ITEM->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_on_std_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_STD_DISPLAY .

  DATA : LT_SELIDX    TYPE LVC_T_ROW.

*  _G_INIT: GT_DISP_ITEM.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M02.  "조회 대상 배치를 선택하세요.
  ELSEIF LV_TSELLINES > 1.
    MESSAGE S007(ZMM01) DISPLAY LIKE 'E'. EXIT. "1 건의 데이타만 선택 가능 합니다.
  ENDIF.

  READ TABLE LT_SELIDX INTO DATA(LV_SELIDX) INDEX 1.
*-

  READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
  IF SY-SUBRC = 0.
*---------------------------
* 배치 사용처 리스트 조회(MB56 호출)
*---------------------------
    SET PARAMETER ID 'MAT' FIELD <LS_DISP>-MATNR.
    SET PARAMETER ID 'WRK' FIELD <LS_DISP>-WERKS.
    SET PARAMETER ID 'CHA' FIELD <LS_DISP>-CHARG.
    CALL TRANSACTION 'MB56' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_on_bottom_up_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_BOTTOM_UP_INIT .

  DATA : LT_SELIDX    TYPE LVC_T_ROW.

  DATA : LT_SUB     TYPE TABLE OF TS_DISP,    "중간 집계용
         LT_SUB_PRO TYPE TABLE OF TS_DISP,    "생산오더 집계용
         LT_SUB_PUR TYPE TABLE OF TS_DISP,    "구매오더 집계용
         LT_SUB_STO TYPE TABLE OF TS_DISP.    "재고 집계용

  _G_INIT: GT_DISP_ITEM.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S006(ZMM01) DISPLAY LIKE 'I'. EXIT. "선택된 데이타가 없습니다.
  ELSEIF LV_TSELLINES > 1.
    MESSAGE S007(ZMM01) DISPLAY LIKE 'E'. EXIT. "1 건의 데이타만 선택 가능 합니다.
  ENDIF.

  READ TABLE LT_SELIDX INTO DATA(LV_SELIDX) INDEX 1.
*-

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LV_SELIDX-INDEX.

  IF LS_DISP IS NOT INITIAL.

    APPEND LS_DISP TO LT_SUB.              "중간 집계용
*    APPEND LS_DISP TO GT_DISP_ITEM.        "최종 ITEM ALV Display 용 최초 적재

  ENDIF.

*-
  WHILE LT_SUB[] IS NOT INITIAL.

    LOOP AT LT_SUB ASSIGNING FIELD-SYMBOL(<LS_SUB>) WHERE CHARG NE ''..    "분류작업

      IF <LS_SUB>-AUFNR IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PRO.              " 생산오더 집계용
      ELSEIF <LS_SUB>-EBELN IS NOT INITIAL.
        APPEND <LS_SUB> TO LT_SUB_PUR.             " 구매오더 집계용
*      ELSEIF <LS_SUB>-AUFNR IS INITIAL AND LS_DISP-EBELN IS INITIAL.
      ELSE.
        APPEND <LS_SUB> TO LT_SUB_STO.            " 재고 집계용
      ENDIF.

    ENDLOOP.

    CLEAR : LT_SUB, LT_SUB[].

    IF LT_SUB_PRO[] IS NOT INITIAL.

      PERFORM BOTTOM_UP_PRO TABLES LT_SUB_PRO LT_SUB.
      _G_INIT LT_SUB_PRO.

    ENDIF.

    IF LT_SUB_PUR[] IS NOT INITIAL.

      PERFORM BOTTOM_UP_PUR TABLES LT_SUB_PUR LT_SUB.
      _G_INIT LT_SUB_PUR.
    ENDIF.

    IF LT_SUB_STO[] IS NOT INITIAL.

      PERFORM BOTTOM_UP_STO TABLES LT_SUB_STO LT_SUB.
      _G_INIT LT_SUB_STO.
    ENDIF.

  ENDWHILE.

  IF GT_DISP_ITEM[] IS NOT INITIAL.

*- U2 START > T2 일 경우에 영문화 적용.
    IF GV_LANGU NE '3'.
      LOOP AT GT_DISP_ITEM INTO DATA(LS_DISP_ITEM).

        READ TABLE GT_MVTTYPE INTO DATA(LS_MVT) WITH KEY MVT_KO = LS_DISP_ITEM-MVTTYPE.
        IF SY-SUBRC = 0.
          LS_DISP_ITEM-MVTTYPE = LS_MVT-MVT_EN.
        ENDIF.

        MODIFY GT_DISP_ITEM FROM LS_DISP_ITEM.

      ENDLOOP.
    ENDIF.
*- U2 END > T2 일 경우에 영문화 적용.

*    SORT GT_DISP_ITEM BY MATNR ASCENDING  CPUDT CPUTM ASCENDING .
    SORT GT_DISP_ITEM BY  CPUDT CPUTM MATNR CHARG .
    DELETE ADJACENT DUPLICATES FROM GT_DISP_ITEM COMPARING ALL FIELDS.

  ENDIF.

  GRF_ITEM->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ME_>M_NAME
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR USING IV_NAME
                       CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

  DEFINE _L_ADD_TOOLBAR.
    ls_add_toolbar-function    = &1.
    ls_add_toolbar-icon        = &2.
    ls_add_toolbar-quickinfo   = &3.
    ls_add_toolbar-butn_type   = &4.
    ls_add_toolbar-disabled    = &5.
    ls_add_toolbar-text        = &6.

    APPEND ls_add_toolbar TO Ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
*  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
*    RETURN.
*  ENDIF.

  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
  _L_ADD_TOOLBAR : 'BTN_ON_TOP_DOWN'  ICON_PAGE_DOWN   TEXT-U01 '' '' TEXT-U01.
  _L_ADD_TOOLBAR : 'BTN_ON_BOTTOM_UP' ICON_PAGE_UP   TEXT-U02 '' '' TEXT-U02.
  _L_ADD_TOOLBAR : 'BTN_ON_ALL'       ICON_SELECT_ALL   TEXT-U03 '' '' TEXT-U03.
  _L_ADD_TOOLBAR : 'BTN_ON_STD'       ICON_DISPLAY_NOTE   TEXT-U04 '' '' TEXT-U04.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FIELDNAME
*&      <-- <LT_F4_LIST>
*&      <-- CV_TITLE
*&      <-- EV_MONTH_DISPLAY
*&---------------------------------------------------------------------*
FORM EVT_GRID_F4 USING IV_FIELDNAME
                  CHANGING CT_F4_LIST  TYPE TABLE
                           CV_TITLE CV_MONTH_DISPLAY .

  CASE IV_FIELDNAME.
    WHEN 'CDVAL'.
      _G_SET_VALUE : CV_TITLE          'F4 Code Value'.

*      PERFORM F4_CDVAL_GRID USING    IV_FIELDNAME
*                            CHANGING CT_F4_LIST.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_GRID_DOUBLE_CLICK USING IV_ROW
                                  IV_COLUMN.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_hotspot_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&      --> E_COLUMN_ID
*&---------------------------------------------------------------------*
FORM EVT_GRID_HOTSPOT_CLICK USING IV_ROW_ID TYPE INT4
                                  IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW_ID.

  CASE IV_COLUMN.
    WHEN 'MATNR'.
*---------------------------
* 자재 조회(MM03 호출)
*---------------------------
      CHECK LS_DISP-MATNR IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'CHARG'.
*---------------------------
* 배치 조회(MSC3N 호출)
*---------------------------
      CHECK LS_DISP-CHARG IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'CHA' FIELD LS_DISP-CHARG.
      CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.

    WHEN 'MBLNR'.
*- U1 START : 자재문서 조회 시 ‘'MIGO_DIALOG’ 펑션 호출 하도록 변경
      PERFORM CALL_MIGO_DIALOG     USING LS_DISP-MBLNR    "자재전표번호
                                         LS_DISP-MJAHR.   "전표년도
**---------------------------
** 자재문서 조회(MIGO_DIALOG 호출) - 주석처리
**---------------------------
*      CHECK ls_disp-mblnr IS NOT INITIAL.
*      SET PARAMETER ID 'MBN' FIELD ls_disp-mblnr.
*      SET PARAMETER ID 'MJA' FIELD ls_disp-mjahr.
*      CALL TRANSACTION 'MIGO' AND SKIP FIRST SCREEN.
*-U1 END

    WHEN 'EBELN'.
*---------------------------
* 구매오더조회(VL33N 호출)
*---------------------------
      CHECK LS_DISP-EBELN IS NOT INITIAL.
      SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'AUFNR'.
*---------------------------
* 생산오더조회(CO03 호출)
*---------------------------
      CHECK LS_DISP-AUFNR IS NOT INITIAL.
      SET PARAMETER ID 'ANR' FIELD LS_DISP-AUFNR.
      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_changed_finished
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM EVT_GRID_CHANGED_FINISHED USING IT_GOOD_CELLS TYPE LVC_T_MODI.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_grid_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-no_out     = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'MVTTYPE'. "구분그룹
        _L_SET_FCAT: 1   ''  ''  TEXT-X01        'C' ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C400.
      WHEN 'BUKRS'. "회사코드
        _L_SET_FCAT: 2   ''  ''  TEXT-X02      'C'  '' '' '' 'X'.
      WHEN 'BUTXT'. "회사명
        _L_SET_FCAT: 3   ''  ''  TEXT-X03      '' '' '' '' 'X'.
      WHEN 'BUDAT'. "전기일
        _L_SET_FCAT: 4   ''  ''  TEXT-X04    'C' '' '' '' ''.
      WHEN 'CPUDT'. "입력일
        _L_SET_FCAT: 5   ''  ''  TEXT-X05      'C'  ''  '' '' 'X'.
      WHEN 'CPUTM'. "입력시간
        _L_SET_FCAT: 6   ''  ''  TEXT-X06        'C' ''  '' '' 'X'.
      WHEN 'ZNUM'.  "Seq
        _L_SET_FCAT: 7   ''  ''  'Seq'      'C' '' '' '' ''.
      WHEN 'MATNR'. "자재코드
        _L_SET_FCAT: 8   ''  ''  TEXT-X07      'C' '' '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C400.
      WHEN 'MAKTX'. "자재명
        _L_SET_FCAT: 9   ''  ''  TEXT-X08      '' '' '' '' ''.
      WHEN 'CHARG'. "배치
        _L_SET_FCAT: 10  ''  ''  TEXT-X09        'C'  ''  '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C700.
      WHEN 'BWART'. "이동유형
        _L_SET_FCAT: 11  ''  ''  TEXT-X10          'C' '' '' '' ''.
      WHEN 'BTEXT'. "이동유형명
        _L_SET_FCAT: 12  ''  ''   TEXT-X11         ''  '' '' '' ''.
      WHEN 'SOBKZ'. "특별재고지시자
        _L_SET_FCAT: 13  ''   ''   TEXT-X12     'C' '' '' '' ''.
      WHEN 'MENGE'. "수량
        _L_SET_FCAT: 14  ''   ''   TEXT-X13 ''  ''  '' '' 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'ERFMG'. "이동수량
        _L_SET_FCAT: 15  ''   ''   TEXT-X14     '' ''  '' 'ERFME' ''.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'ERFME'. "입력단위
        _L_SET_FCAT: 16  ''   ''   TEXT-X15     'C' ''  '' '' ''.
      WHEN 'MEINS'. "기본단위
        _L_SET_FCAT: 17  ''   ''   TEXT-X16       'C' ''  '' '' 'X'.
      WHEN 'WERKS'. "플랜트
        _L_SET_FCAT: 18  ''   ''   TEXT-X17       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'WERKSNAME'. "플랜트명
        _L_SET_FCAT: 19  ''   ''   TEXT-X18       ''  ''  '' '' ''.
      WHEN 'LGORT'. "저장위치
        _L_SET_FCAT: 20  ''   ''   TEXT-X19       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C510.
      WHEN 'LGORTNAME'. "저장위치명
        _L_SET_FCAT: 21  ''   ''   TEXT-X20       ''  ''  '' '' ''.
      WHEN 'UMWRK'. "관전플랜트
        _L_SET_FCAT: 22  ''   ''   TEXT-X21       'C'  ''  '' '' ''.
      WHEN 'UMWRKNAME'. "관전플랜트명
        _L_SET_FCAT: 23  ''   ''   TEXT-X22       ''  ''  '' '' ''.
      WHEN 'UMLGO'. "관전저장위치
        _L_SET_FCAT: 24  ''   ''   TEXT-X23       'C'  ''  '' '' ''.
      WHEN 'UMLGONAME'. "관전저장위치명
        _L_SET_FCAT: 25  ''   ''   TEXT-X24       ''  ''  '' '' ''.
      WHEN 'MJAHR'. "자재연도
        _L_SET_FCAT: 26  ''   ''   TEXT-X25       'C'  ''  '' '' ''.
      WHEN 'MBLNR'. "자재문서
        _L_SET_FCAT: 27  ''   ''   TEXT-X26       'C'  ''  '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C400.
      WHEN 'ZEILE'. "자재문서항번
        _L_SET_FCAT: 28  ''   ''   TEXT-X27       'C'  ''  '' '' 'X'.
      WHEN 'BWTAR'. "평가유형
        _L_SET_FCAT: 29  ''   ''   TEXT-X28       'C'  ''  '' '' ''.
      WHEN 'LIFNR'. "공급업체
        _L_SET_FCAT: 30  ''   ''   TEXT-X29       'C'  ''  '' '' ''.
      WHEN 'LIFNRNAME'. "업체명
        _L_SET_FCAT: 31  ''   ''   TEXT-X30       ''  ''  '' '' ''.
      WHEN 'KUNNR'. "고객
        _L_SET_FCAT: 32  ''   ''   TEXT-X31       'C'  ''  '' '' ''.
      WHEN 'KUNNRNAME'. "고객명
        _L_SET_FCAT: 33  ''   ''   TEXT-X32       ''  ''  '' '' ''.
      WHEN 'HSDAT'. "제조일
        _L_SET_FCAT: 34  ''   ''   TEXT-X33       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'VFDAT'. "사용기한
        _L_SET_FCAT: 35  ''   ''   TEXT-X34       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C310.
      WHEN 'LICHN'. "제조처LOT
        _L_SET_FCAT: 36  ''   ''   TEXT-X35       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'ZMAKER'. "메이커
        _L_SET_FCAT: 37  ''   ''   TEXT-X36       ''  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C310.
      WHEN 'FOODTRACKING'. "식품이력번호
        _L_SET_FCAT: 38  ''   ''   TEXT-X37       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'UNIQUENO'. "고유번호
        _L_SET_FCAT: 39  ''   ''   TEXT-X38       'C'  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C310.
      WHEN 'WATERRATIO'. "수분함량
        _L_SET_FCAT: 40  ''   ''   TEXT-X39       ''  ''  '' '' ''.
        <LS_FCAT>-EMPHASIZE = GC_C300.
      WHEN 'EBELN'. "구매오더
        _L_SET_FCAT: 41  ''   ''   TEXT-X40       'C'  ''  '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'EBELP'. "항번
        _L_SET_FCAT: 42  ''   ''   TEXT-X41       'C'  ''  '' '' ''.
      WHEN 'AUFNR'. "생산오더
        _L_SET_FCAT: 43  ''   ''   TEXT-X42       'C'  ''  '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'AUFPS'. "오더 품목 번호
        _L_SET_FCAT: 44  ''   ''   TEXT-X43       'C'  ''  '' '' ''.
      WHEN 'VBELN_IM'. "납품서
        _L_SET_FCAT: 45  ''   ''   TEXT-X44       'C'  ''  '' '' ''.
      WHEN 'VBELP_IM'. "항번
        _L_SET_FCAT: 46  ''   ''   TEXT-X41       'C'  ''  '' '' ''.
      WHEN 'USNAM'. "사용자
        _L_SET_FCAT: 47  ''   ''   TEXT-X45       'C'  ''  '' '' ''.
      WHEN 'LBBSA_SID'. "자재 이동 재고 유형(재고 ID)
        _L_SET_FCAT: 48  ''   ''   TEXT-X46       'C'  ''  '' '' 'X'.
      WHEN 'KZBEW'. "이동지시자
        _L_SET_FCAT: 49  ''   ''   TEXT-X47       'C'  ''  '' '' 'X'.
      WHEN 'BSTAUS_SG'. "재고 특징
        _L_SET_FCAT: 50  ''   ''   TEXT-X48       'C'  ''  '' '' 'X'.
      WHEN 'BSTTYP_SG'. "재고 범주
        _L_SET_FCAT: 51  ''   ''   TEXT-X49       'C'  ''  '' '' 'X'.
      WHEN 'BERID'. "MRP 영역
        _L_SET_FCAT: 52  ''   ''   TEXT-X50       'C'  ''  '' '' 'X'.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 53  ''   ''   TEXT-X51       'C'  ''  '' '' 'X'.
      WHEN 'DMBTR'. "금액(현지 통화)
        _L_SET_FCAT: 54  ''   ''   TEXT-X52       'C'  ''  'WAERS' '' 'X'.
      WHEN 'RSNUM'. "예약/종속 소요량의 번호
        _L_SET_FCAT: 55  ''   ''   TEXT-X53       'C'  ''  '' '' 'X'.
      WHEN 'RSPOS'. "예약/종속 소요량에 대한 품목 번호
        _L_SET_FCAT: 56  ''   ''   TEXT-X54       'C'  ''  '' '' 'X'.
      WHEN 'SHKZG'. "차대구분
        _L_SET_FCAT: 57  ''   ''   TEXT-X55       'C'  ''  '' '' 'X'.
      WHEN 'ELIKZ'. "납품 완료 지시자
        _L_SET_FCAT: 58  ''   ''   TEXT-X56       'C'  ''  '' '' 'X'.
      WHEN 'KOKRS'. "관리회계 영역
        _L_SET_FCAT: 59  ''   ''   TEXT-X57       'C'  ''  '' '' 'X'.
      WHEN 'KZVBR'. "소비 전기
        _L_SET_FCAT: 60  ''   ''   TEXT-X58       'C'  ''  '' '' 'X'.
      WHEN 'PRCTR'. "손익센터
        _L_SET_FCAT: 61  ''   ''   TEXT-X59       'C'  ''  '' '' 'X'.
      WHEN 'AUFPL'. "오더에 있는 작업 라우팅 번호
        _L_SET_FCAT: 62  ''   ''   TEXT-X60       'C'  ''  '' '' 'X'.
      WHEN 'APLZL'. "내부카운터
        _L_SET_FCAT: 63  ''   ''   TEXT-X61       'C'  ''  '' '' 'X'.
      WHEN 'SAKTO'. "G/L 계정 번호
        _L_SET_FCAT: 64  ''   ''   TEXT-X62       'C'  ''  '' '' 'X'.
      WHEN 'TCODE2'.
        _L_SET_FCAT: 65  ''   ''   'TCODE'       'C'  ''  '' '' 'X'.
      WHEN OTHERS.
*        <LS_FCAT>-NO_OUT = 'X'.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_grid_set_line_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT TYPE LVC_T_FCAT.
  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LV_INDEX    TYPE I.



  LOOP AT GT_DISP INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.


    CLEAR  : LT_LVC_STYL[].
*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_FIELD_CELLTAB USING    IT_FCAT
                              CHANGING LT_LVC_STYL LS_DISP.

*-- Insert Style Talble
    CLEAR LS_DISP-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DISP-CELLS.

*-- Modify Line..
    MODIFY GT_DISP FROM LS_DISP INDEX LV_INDEX.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_STYL
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CHECK LS_FIELDCAT-EDIT = 'X'.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_FIELDCAT-KEY.
      WHEN 'X'.
        IF CS_DISP-DCFLG EQ 'C'. "'N'. "신규필드인 경우
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
      WHEN OTHERS.
        LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    ENDCASE.

    INSERT LS_LVC_STYL INTO TABLE CT_STYL.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FIELD
*&---------------------------------------------------------------------*
FORM ALV_SET_F4 CHANGING CT_FIELD TYPE ZCL_CN_ALV_GRID=>TT_FIELD.

  CHECK 1 <> 1.
*----------------------------
* Set F4 Field..
*----------------------------
**  CT_FIELD = VALUE #( ( FIELDNAME = 'SPMON' )
**                      ( FIELDNAME = 'CDVAL' ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_sort
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_SORT
*&---------------------------------------------------------------------*
FORM ALV_SET_SORT CHANGING CT_SORT TYPE LVC_T_SORT.

  DEFINE _L_APPEND_SORT.
    ls_sort-spos      = &1.
    ls_sort-fieldname = &2.
    ls_sort-up        = &3.
    ls_sort-down      = &4.
    ls_sort-group     = &5.
    ls_sort-subtot    = &6.
    ls_sort-comp      = &7.
    ls_sort-level     = &8.

  APPEND ls_sort TO ct_sort.
  END-OF-DEFINITION.

  DATA:LS_SORT TYPE LVC_S_SORT.

*  _l_append_sort: 1 'SPMON' 'X' '' '' '' '' ''.
ENDFORM.
