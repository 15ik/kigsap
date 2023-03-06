*&---------------------------------------------------------------------*
*& Include          ZRMM2020F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
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
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( "EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_DISP ).

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
  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 100 ).
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

*--------------------------------
* Set Body Container
*--------------------------------
  GRF_BODY = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

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
*  LS_TOOLBTN-BTN_INFO   = 'X'.       "Information
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM20030'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
**  SELECT SINGLE NAME1
**    INTO @DATA(LV_NAME1)
**    FROM T001W
**   WHERE WERKS = @P_WERKS.
**
**  _L_ADD_FIELD: 'ZMONTH' P_SPMON,
**                'WERKS'  P_WERKS,
**                'NAME1'  LV_NAME1.

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

*--------------------------------------------------
* Set Infomationv 버튼
*--------------------------------------------------
*  LT_INFO = VALUE #( ( FIELDNAME = 'STLAL' ) ( FIELDNAME = 'INFNR' ) ( FIELDNAME = 'MSG' ) ).

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함.
      IRF_PARENT = GRF_BODY
*     IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IT_INFO    = LT_INFO
      IRF_HEAD   = GRF_HEAD
      IV_SCR_WR  = '20:20:60'
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

**  DATA : LV_TEXT(255) TYPE C.

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

  CONSTANTS : LC_LINE1(20) TYPE C VALUE '조회 구간 : ',
              LC_LINE2(20) TYPE C VALUE '현재고/외주업체 포함 기초재고 계산 : ',
              LC_LINE3(20) TYPE C VALUE '확정 입고 예정 포함 : ',
              LC_LINE4(20) TYPE C VALUE '공급가능일수(재고) : ',
              LC_LINE5(20) TYPE C VALUE '공급가능일수(재고+확정입고예정)) : '.

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외
  CASE P_FORMAT.
    WHEN 'D'. DATA(LV_LINE1_TXT) = |D | && |{ S_DATE-LOW } ~ | && |{ S_DATE-HIGH }|.
    WHEN 'W'. LV_LINE1_TXT       = |W | && |{ S_SWEEK-LOW } ~ | && |{ S_SWEEK-HIGH }|.
    WHEN 'M'. LV_LINE1_TXT       = |M | && |{ S_SPMON-LOW } ~ | && |{ S_SPMON-HIGH }|.
  ENDCASE.

  DATA(LV_LINE2_TXT)  = COND CHAR8( WHEN P_CHK1 IS INITIAL THEN TEXT-004
                                    ELSE                   TEXT-O05 ).

  DATA(LV_LINE21_TXT) = COND CHAR4( WHEN P_CHK5 IS INITIAL THEN TEXT-004  "U2
                                    ELSE                   TEXT-O05 ).
  LV_LINE2_TXT = LV_LINE2_TXT && '/' && LV_LINE21_TXT. "U2

  DATA(LV_LINE3_TXT) = COND CHAR30( WHEN P_CHK2 IS INITIAL     AND P_CHK3 IS INITIAL     THEN TEXT-006
                                    WHEN P_CHK2 IS INITIAL     AND P_CHK3 IS NOT INITIAL THEN TEXT-007
                                    WHEN P_CHK2 IS NOT INITIAL AND P_CHK3 IS INITIAL     THEN TEXT-008
                                    WHEN P_CHK2 IS NOT INITIAL AND P_CHK3 IS NOT INITIAL THEN TEXT-009 ).

  IF P_FORMAT = 'D' AND S_DAY1-LOW IS NOT INITIAL AND S_DAY1-HIGH IS NOT INITIAL.
    DATA(LV_LINE4_TXT) = |{ S_DAY1-LOW } ~ | && |{ S_DAY1-HIGH }|.
  ENDIF.

  IF P_FORMAT = 'D' AND S_DAY2-LOW IS NOT INITIAL AND S_DAY2-HIGH IS NOT INITIAL.
    DATA(LV_LINE5_TXT) = |{ S_DAY2-LOW } ~ | && |{ S_DAY2-HIGH }|.
  ENDIF.

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER : LC_LINE1   LV_LINE1_TXT '',
                  LC_LINE2   LV_LINE2_TXT '',
                  LC_LINE3   LV_LINE3_TXT '',
                  LC_LINE4   LV_LINE4_TXT '',
                  LC_LINE5   LV_LINE5_TXT ''.

*  _G_SET_VALUE:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

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
**  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
**    RETURN.
**  ENDIF.

*  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
*  _L_ADD_TOOLBAR : 'BTN_0100_SAVE'  ICON_SYSTEM_SAVE            TEXT-T11 '' '' TEXT-T11,
*                   'BTN_0100_INPUT' ICON_WD_TOOLBAR_INPUT_FIELD TEXT-T12 '' '' TEXT-T12.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
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
    WHEN 'MATNR'.
      READ TABLE GT_DISP INTO LS_DISP INDEX IV_ROW.
      IF LS_DISP-MATNR IS NOT INITIAL.
        SET PARAMETER ID  'MAT'    FIELD  LS_DISP-MATNR.
        SET PARAMETER ID  'WRK'    FIELD  LS_DISP-WERKS.
        SET PARAMETER ID  'BERID'  FIELD  LS_DISP-BERID.
        CALL TRANSACTION  'MD04' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&      --> E_COLUMN_ID
*&---------------------------------------------------------------------*
FORM EVT_GRID_HOTSPOT_CLICK USING IV_ROW_ID TYPE INT4
                                  IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW_ID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DATA: LV_CNT TYPE SY-TABIX.

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
  DATA(LV_LIST_CNT) = LINES( GT_DAY ).
  CLEAR LV_CNT.
  SORT GT_DAY BY DAY.
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'WERKS'.
        _L_SET_FCAT: 1   'X' ''  '플랜트'        'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'NAME1'.
        _L_SET_FCAT: 2   'X' ''  '플랜트명'        ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'PLSCN'.
        _L_SET_FCAT: 3   '' ''  'LTP 시나리오'   'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'PLSCT'.
        _L_SET_FCAT: 12   '' ''  'LTP 시나리오 명' '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'BERID'.
        _L_SET_FCAT: 5   '' ''  'MRP 영역'   'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'BERTX'.
        _L_SET_FCAT: 12   '' ''  'MRP 영역 텍스트' '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'MATNR'.
        _L_SET_FCAT: 7   'X' 'X'  '자재'    ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 8   'X' ''  '자재내역'   ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 25.
      WHEN 'MMSTA'.
        _L_SET_FCAT: 9   '' ''  '자재상태'       'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'MTSTB'.
        _L_SET_FCAT: 10   '' ''  '자재상태명'         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'MEINS'.
        _L_SET_FCAT: 11   'X' ''  '단위'     'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'STOCK'.
        _L_SET_FCAT: 12  '' ''  '현재고'     ''  ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
        <LS_FCAT>-NO_OUT = 'X'. <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'SUB_STOCK'.  "U2
        _L_SET_FCAT: 12  '' ''  '외주업체재고'     ''  ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
        <LS_FCAT>-NO_OUT = 'X'. <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'DAY1'.
        ADD 1 TO LV_LIST_CNT.
        _L_SET_FCAT: LV_LIST_CNT  '' ''  '공급가능일수(재고)'     ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
      WHEN 'DAY2'.
        ADD 1 TO LV_LIST_CNT.
        _L_SET_FCAT: LV_LIST_CNT '' ''  '공급가능일수(재고+확정입고예정)'     ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'MGSUM'.
        _L_SET_FCAT: 13  '' ''  '소요량합'     ''  ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_OUT = 'X'. <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'OPT_TXT'.
        _L_SET_FCAT: 14   'X' ''  '구분'        '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
        <LS_FCAT>-FIX_COLUMN = 'X'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.

      WHEN 'CDLST' OR 'DCFLG' OR 'IEDIT' OR 'ZDELE' OR 'OPT' OR 'DEL_TEMP' OR 'OPT_TXT'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = 'T'.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN OTHERS.
        CASE <LS_FCAT>-FIELDNAME(03).

          WHEN 'DAT'.
            LV_CNT = 13.

            READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY DAY = <LS_FCAT>-FIELDNAME BINARY SEARCH.
            IF SY-SUBRC = 0.
              _L_SET_FCAT: LV_CNT   '' 'X'  LS_DAY-DAY_TXT ''  '' '' 'MEINS' ''.

              <LS_FCAT>-OUTPUTLEN = 9.
              <LS_FCAT>-NO_ZERO   = 'X'.

            ELSE.
              _L_SET_FCAT: LV_CNT   '' ''  '' ''  '' '' 'MEINS' ''.
              <LS_FCAT>-NO_OUT = 'X'.
            ENDIF.

            ADD 1 TO LV_CNT.

          WHEN OTHERS.
            <LS_FCAT>-NO_OUT = 'X'.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

ENDFORM. " FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT.

  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LT_LVC_SCOL TYPE LVC_T_SCOL,
         LV_INDEX    TYPE I.

  LOOP AT GT_DISP INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.

    CLEAR: LT_LVC_STYL[], LT_LVC_SCOL.
*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_FIELD_CELLTAB USING    IT_FCAT
                              CHANGING LT_LVC_STYL LS_DISP.

*-- Insert Style Talble
    CLEAR LS_DISP-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DISP-CELLS.

*---------------------------
* Set Field Color..
*---------------------------
    PERFORM SET_FIELD_CELLTAB_COLOR USING IT_FCAT
                                 CHANGING LT_LVC_SCOL LS_DISP.

    INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_DISP-CELLC.

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
FORM SET_FIELD_CELLTAB_COLOR USING IT_FCAT TYPE LVC_T_FCAT
                            CHANGING CT_SCOL TYPE LVC_T_SCOL
                                     CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).

    CLEAR LS_LVC_SCOL.

    LS_LVC_SCOL-FNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_LVC_SCOL-FNAME.
      WHEN 'STATU' OR 'MAKTX' OR 'NAME1' OR 'MATNR' OR 'WERKS' OR 'MEINS' OR 'MMSTA' OR 'MTSTB'.
        LS_LVC_SCOL-COLOR-COL = 5.
        LS_LVC_SCOL-COLOR-INT = 0.
        LS_LVC_SCOL-COLOR-INV = 0.
      WHEN 'OPT_TXT'.
        CASE CS_DISP-OPT.
          WHEN 'A' OR 'B' OR 'C'. "기초재고 / "소요량 / "확정 입고 예정
            LS_LVC_SCOL-COLOR-COL = 3.
            LS_LVC_SCOL-COLOR-INT = 1.
            LS_LVC_SCOL-COLOR-INV = 0.
          WHEN 'E'. "예상 기말 재고
            LS_LVC_SCOL-COLOR-COL = 3.
            LS_LVC_SCOL-COLOR-INT = 0.
            LS_LVC_SCOL-COLOR-INV = 0.
        ENDCASE.

      WHEN 'DAY1' OR 'DAY2'.
        LS_LVC_SCOL-COLOR-COL = 1.
        LS_LVC_SCOL-COLOR-INT = 1.
        LS_LVC_SCOL-COLOR-INV = 1.

      WHEN OTHERS.
        CASE LS_LVC_SCOL-FNAME(03).
          WHEN 'DAT'.
            CASE CS_DISP-OPT.
              WHEN 'A' OR 'B' OR 'C'. "기초재고 / "소요량 / "확정 입고 예정
                LS_LVC_SCOL-COLOR-COL = 2.
                LS_LVC_SCOL-COLOR-INT = 0.
                LS_LVC_SCOL-COLOR-INV = 0.
              WHEN 'E'. "예상 기말 재고
                LS_LVC_SCOL-COLOR-COL = 3.
                LS_LVC_SCOL-COLOR-INT = 0.
                LS_LVC_SCOL-COLOR-INV = 0.
            ENDCASE.
        ENDCASE.
    ENDCASE.

    INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.
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
*& Form ALV_SET_SORT
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

  _L_APPEND_SORT: 1 'WERKS' 'X' '' '' '' '' '',
                  1 'NAME1' 'X' '' '' '' '' '',
                  2 'MATNR' 'X' '' '' '' '' '',
                  2 'MAKTX' 'X' '' '' '' '' '',
                  3 'MEINS' 'X' '' '' '' '' ''.

ENDFORM.
