*&---------------------------------------------------------------------*
*& Include          ZRMM2130F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form BTN_0100_REQ_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_0100_REQ_INIT .

  DATA : LT_SELIDX TYPE LVC_T_ROW,
         LV_PROC   TYPE CHAR1 VALUE 'X'.

  DATA : LT_BDC     TYPE STANDARD TABLE OF BDCDATA,
         LS_BDC     LIKE LINE OF LT_BDC,
         LS_BDC_WRK LIKE LINE OF GT_BDC_WRK,
         LS_BDC_MAT LIKE LINE OF GT_BDC_MAT.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
  IF LT_SELIDX[] IS INITIAL.
    CLEAR LV_PROC. MESSAGE I006(ZMM01). EXIT.
  ENDIF.

*-
  CHECK LV_PROC IS NOT INITIAL.

*-
  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).

    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      LS_BDC_WRK-WERKS = LS_DISP-WERKS.
      LS_BDC_MAT-MATNR = LS_DISP-MATNR.
      COLLECT : LS_BDC_WRK INTO GT_BDC_WRK, LS_BDC_MAT INTO GT_BDC_MAT.
    ENDIF.

  ENDLOOP.

  PERFORM SET_BDC_USING_TAB TABLES LT_BDC.


  DATA: LV_TASK(20) VALUE 'TEST'.
  CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
    STARTING NEW TASK LV_TASK
    EXPORTING
      TCODE                   = 'ZRMM2160'
      MODE_VAL                = 'E'
    TABLES
      USING_TAB               = LT_BDC
    EXCEPTIONS
      CALL_TRANSACTION_DENIED = 1
      TCODE_INVALID           = 2
      OTHERS                  = 3.

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

*-
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
  _L_ADD_TOOLBAR : 'BTN_0100_REQ'  ICON_TREE   TEXT-T25 '' '' TEXT-T25.

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
    WHEN 'STATU' OR 'MSG'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'MATNR' OR 'MAKTX'.
      PERFORM CALL_POPUP_BOM USING IV_ROW.

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

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW_ID.
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'MATNR'.
      READ TABLE GT_DISP INTO LS_DISP INDEX IV_ROW_ID.

  ENDCASE.

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
  CLEAR LV_CNT.
  SORT GT_DAY BY DAY.
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).
    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'WERKS'.
        _L_SET_FCAT: 2   '' ''  '플랜트'      'C'  'X' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'NAME1'.
        _L_SET_FCAT: 3   '' ''  '플랜트 명'    ''  '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MATKL'.
        _L_SET_FCAT: 4   '' ''  '자재그룹'     'C' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'WGBEZ'.
        _L_SET_FCAT: 5   '' ''  '자재그룹명'     '' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MATNR'.
        _L_SET_FCAT: 6   '' ''  '자재'        ''  'X' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 7   '' ''  '자재내역'     '' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 8   '' ''  '공급업체'     'C' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'LFA1_NAME1'.
        _L_SET_FCAT: 9   '' ''  '공급업체명'     '' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MAT_TEXT'.
        _L_SET_FCAT: 10  '' ''   '기본텍스트'   ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'ZFDLV'.
        _L_SET_FCAT: 11  '' ''   '자율납품'   ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4. <LS_FCAT>-CHECKBOX = 'X'.
      WHEN 'MEINS'.
        _L_SET_FCAT: 12  '' ''  '단위'        'C' '' '' '' ''.
        <LS_FCAT>-COL_OPT = 'X'.
        <LS_FCAT>-FIX_COLUMN = 'X'.

      WHEN 'OPT_TXT'.
        _L_SET_FCAT: 13   '' ''  '구분'        '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-FIX_COLUMN = 'X'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.

      WHEN 'CDLST' OR 'DCFLG' OR 'IEDIT' OR 'ZDELE' OR 'OPT'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = 'T'.
        <LS_FCAT>-NO_OUT = 'X'.

      WHEN OTHERS.
        CASE <LS_FCAT>-FIELDNAME(03).

          WHEN 'DAT'.
            LV_CNT = 17.

            READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY DAY = <LS_FCAT>-FIELDNAME BINARY SEARCH.
            IF SY-SUBRC = 0.
              _L_SET_FCAT: LV_CNT   '' ''  LS_DAY-DAY_TXT ''  '' '' 'MEINS' ''.

              <LS_FCAT>-OUTPUTLEN = 7.
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_0200_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_0200_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

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
      WHEN 'MATNR'.
        _L_SET_FCAT: 1   'X'  ''  '상위제품'        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 18.
        <LS_FCAT>-CONVEXIT = TEXT-O06. "CONV
      WHEN 'MAKTX'.
        _L_SET_FCAT: 2   ''  ''   '내역'    '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 30.
      WHEN 'CDLST' OR 'DCFLG' OR 'ZDELE'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = 'T'.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN OTHERS.
        <LS_FCAT>-NO_OUT = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
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

    CLEAR: LT_LVC_STYL[], LT_LVC_SCOL[].
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
    CHECK P_RD1C = 'X'.

    PERFORM SET_FIELD_CELLTAB_COLOR USING   IT_FCAT
                                 CHANGING LT_LVC_SCOL LS_DISP.

    CLEAR LS_DISP-CELLC.
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
        LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDCASE.

    INSERT LS_LVC_STYL INTO TABLE CT_STYL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_COLOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_SCOL
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB_COLOR   USING IT_FCAT TYPE LVC_T_FCAT
                            CHANGING CT_SCOL TYPE LVC_T_SCOL
                                     CS_DISP TYPE TS_DISP.

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

  CHECK P_RD1C = 'X'.

  DATA:LS_SORT TYPE LVC_S_SORT.

  _L_APPEND_SORT: 1 'WERKS' 'X' '' '' '' '' '',
                  2 'NAME1' 'X' '' '' '' '' '',
                  3 'MATKL' 'X' '' '' '' '' '',
                  4 'WGBEZ' 'X' '' '' '' '' '',
                  5 'MATNR' 'X' '' '' '' '' '',
                  6 'MAKTX' 'X' '' '' '' '' '',
                  7 'MEINS' 'X' '' '' '' '' ''.

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
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
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
                                                      ROWS    = 1
                                                      COLUMNS = 1 ).

*--------------------------------
* Set Body Container
*--------------------------------
  GRF_BODY = LRF_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

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
  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
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
**  _L_ADD_FIELD: 'ZPMON' P_ZPMON.


*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
**  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

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
      IV_SCR_WR  = '15:25:60'
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_0200 .

  IF GRF_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT GRF_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = GV_CONTAINER.

*-
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_0200.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID_0200->SET_GRID( EXPORTING IV_VARI = 'VERI_0200' CHANGING  CT_DATA = GT_BOM ).

  ELSE.
    GRF_GRID_0200->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_0200.

  DEFINE _L_ADD_FIELD.

    LT_DFTVL = VALUE #( BASE LT_DFTVL ( FIELDNAME = &1 VALUE = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.
*       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
*  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
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

  CREATE OBJECT GRF_GRID_0200
    EXPORTING
      IV_NAME    = 'ALV_GRID_0200'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CUSTOM_CONTAINER
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
