*&---------------------------------------------------------------------*
*& Include          ZRMM4400F02
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
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( EXPORTING IV_VARI = P_VAR
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

*--------------------------------
* Set Body Container
*--------------------------------
  GRF_BODY = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.
  DATA : LV_TEXT(255) TYPE C.

  DATA : LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
    IF &2 IS NOT INITIAL.
    CLEAR LS_HEADER.
    LS_HEADER-KEY  = &1.
    LS_HEADER-INFO  = &2.
    LS_HEADER-TEXT  = &3.
    APPEND LS_HEADER TO CT_HEADER.
    ENDIF.
  END-OF-DEFINITION.

**  CONSTANTS : lc_if(20) TYPE c VALUE 'Controlling Area '.


*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외
  CLEAR: LV_TEXT.

  SELECT SINGLE BUTXT
  INTO @LV_TEXT
  FROM T001
 WHERE BUKRS = @P_BUKRS.

  READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX 1.

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
  _L_SET_HEADER : TEXT-H01  P_BUKRS  LV_TEXT .

  IF S_WERKS IS NOT INITIAL.
    _L_SET_HEADER : TEXT-H02  S_WERKS-LOW  <LS_DISP>-PLANTNAME.
  ENDIF.

*&    U2     T0210054      2022.05.24
  IF P_BUKRS = GC_3101.
    _L_SET_HEADER : TEXT-M11 TEXT-M07   TEXT-M08.
    _L_SET_HEADER : ' ' TEXT-M09   TEXT-M10.
  ENDIF.
*&    U2

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-EDIT       = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
    <LS_FCAT>-NO_OUT     = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'MATERIAL'.
        _L_SET_FCAT: 1   'X'  ''  TEXT-C01      'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'MATNAME'.
        _L_SET_FCAT: 2  'X'  ''  TEXT-C02        ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
        <LS_FCAT>-EMPHASIZE = GC_C400.
        "[U4 변경시작 2022.07.04].
      WHEN 'ZZNAME'.
        _L_SET_FCAT: 2  'X'  ''  TEXT-C03        ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
        "[U4 변경시작 2022.07.04].
      WHEN 'MATKL'.
        _L_SET_FCAT: 3  'X'  ''  TEXT-C04          'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'WGBEZ'.
        _L_SET_FCAT: 4  'X'  ''  TEXT-C05          '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
      WHEN 'TYPE_KOR'.
        _L_SET_FCAT: 5  'X'  ''  TEXT-C06          'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'PLANT'.
        _L_SET_FCAT: 6   ''  ''  TEXT-C07        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'PLANTNAME'.
        _L_SET_FCAT: 7   ''  ''  TEXT-C08      ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'EKGRP'.
        _L_SET_FCAT: 8   ''  ''  TEXT-C09      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'EKNAM'.
        _L_SET_FCAT: 9   ''  ''  TEXT-C10      ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'DISPO'.
        _L_SET_FCAT: 10   ''  ''  TEXT-C11      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'DSNAM'.
        _L_SET_FCAT: 11   ''  ''  TEXT-C12      ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'SLOCATION'.
        IF P_RD1C = 'X'.
          _L_SET_FCAT: 12   ''  ''  TEXT-C13      'C' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 12   ''  ''  TEXT-C13      'C' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 6.
        ENDIF.
      WHEN 'SLNAME'.
        IF P_RD1C = 'X'.
          _L_SET_FCAT: 13   ''  ''  TEXT-C14    '' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 13   ''  ''  TEXT-C14    '' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 12.
        ENDIF.
        "[U4 변경시작 2022.07.04].
      WHEN 'LGPBE'.
        IF P_RD1C = 'X'.
          _L_SET_FCAT: 14   ''  ''  TEXT-C15    'C' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 14   ''  ''  TEXT-C15    'C' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
        "[U4 변경종료 2022.07.04].
      WHEN 'BP'.
        IF P_RD1B = 'X'.
          _L_SET_FCAT: 15   ''  ''  TEXT-C16      'C'  ''  '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 15   ''  ''  TEXT-C16      'C'  ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
      WHEN 'BPNAME'.
        IF P_RD1B = 'X'.
          _L_SET_FCAT: 16   ''  ''  TEXT-C17        '' ''  '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 16   ''  ''  TEXT-C17        '' ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
      WHEN 'BATCH'.
        _L_SET_FCAT: 17  ''  ''   TEXT-C18         'C'  '' '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'BWTAR'.
        _L_SET_FCAT: 18  ''  ''   TEXT-C19         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'AVAILABLESTOCK'.
        _L_SET_FCAT: 19  ''   ''   TEXT-C20     '' '' '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
        <LS_FCAT>-EMPHASIZE = GC_C300.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'QISTOCK'.
        _L_SET_FCAT: 20  ''   ''   TEXT-C21 ''  ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-EMPHASIZE = GC_C700.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'BLOCKSTOCK'.
        _L_SET_FCAT: 21  ''   ''   TEXT-C22     '' ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-EMPHASIZE = GC_C600.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN 'ZSTO'.
        _L_SET_FCAT: 22  ''   ''   TEXT-C23     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-EMPHASIZE = GC_C600.

*&  [U10 시작 T0220139   2022.12.08   TIER2 수정 - 운송중 재고 관련 로직 보완]
*&    U2     T0210054      2022.05.24
      WHEN 'BDMNG'.
        _L_SET_FCAT: 23  ''   ''   TEXT-C24     '' ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_ZERO = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C200.
        IF GV_APPLIED_BUKRS IS INITIAL.
          <LS_FCAT>-TECH = 'X'.
        ENDIF.

*&    U8     T0220139      2022.11.03  RESERVATION STOCK ALV 필드추가
      WHEN 'GLMNG'.
        _L_SET_FCAT: 24  ''   ''   TEXT-C41     '' ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_ZERO = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C200.
        IF GV_APPLIED_BUKRS IS INITIAL.
          <LS_FCAT>-TECH = 'X'.
        ENDIF.

      WHEN 'WEMNG'.
        _L_SET_FCAT: 25  ''   ''   TEXT-C42     '' ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_ZERO = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C200.
        IF GV_APPLIED_BUKRS IS INITIAL.
          <LS_FCAT>-TECH = 'X'.
        ENDIF.
*&    U8     T0220139      2022.11.03  END
*&  [U10 종료 T0220139   2022.12.08   TIER2 수정 - 운송중 재고 관련 로직 보완]

      WHEN 'AVE_STOCK_C'.
        _L_SET_FCAT: 26  ''   ''   TEXT-C25     '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_ZERO = 'X'.
        <LS_FCAT>-JUST = 'R'.
        <LS_FCAT>-EMPHASIZE = GC_C500.
        IF GV_APPLIED_BUKRS IS INITIAL.
          <LS_FCAT>-TECH = 'X'.
        ENDIF.
*&    U2

      WHEN 'MEINS'.
        _L_SET_FCAT: 27  ''   ''   TEXT-C26     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'CHAR_BP'.
        _L_SET_FCAT: 28  ''   ''   TEXT-C27       'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'NAME1'.
        _L_SET_FCAT: 29  ''   ''   TEXT-C28       '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MAKER'.
        _L_SET_FCAT: 30  ''   ''   TEXT-C29       'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'PRDDUCTLOT'.
        _L_SET_FCAT: 31  ''   ''   TEXT-C30     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'PRDUCTIONDATE'.
        _L_SET_FCAT: 32  ''   ''   TEXT-C31       'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'EXPIREDDATE'.
        _L_SET_FCAT: 33  ''   ''   TEXT-C32   'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'REMAINDATE_2'.
        _L_SET_FCAT: 34  ''   ''   TEXT-C33     'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
        <LS_FCAT>-EMPHASIZE = GC_C510.
*      WHEN 'BKLAS'.
*        _L_SET_FCAT: 35  'X'  ''  TEXT-C43          'C' '' '' '' ''. "U11
*        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN OTHERS.
        <LS_FCAT>-NO_OUT = 'X'.
        <LS_FCAT>-TECH = 'X'.
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
FORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT TYPE LVC_T_FCAT.

  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LT_LVC_SCOL TYPE LVC_T_SCOL,
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

*---------------------------
* Set Field Color..
*---------------------------
    PERFORM SET_FIELD_CELLTAB_COLOR USING   IT_FCAT
                                 CHANGING LT_LVC_SCOL LS_DISP.


    INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_DISP-CELLC.
    IF SY-SUBRC = 0.
      CLEAR : LT_LVC_SCOL[].
    ENDIF.

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
*& Form ALV_SET_F4
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
*& Form ALV_SET_SORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_SORT
*&---------------------------------------------------------------------*
FORM ALV_SET_SORT CHANGING CT_SORT TYPE LVC_T_SORT.

  DEFINE _L_APPEND_SORT.
    LS_SORT-SPOS      = &1.
    LS_SORT-FIELDNAME = &2.
    LS_SORT-UP        = &3.
    LS_SORT-DOWN      = &4.
    LS_SORT-GROUP     = &5.
    LS_SORT-SUBTOT    = &6.
    LS_SORT-COMP      = &7.
    LS_SORT-LEVEL     = &8.

  APPEND LS_SORT TO CT_SORT.
  END-OF-DEFINITION.

  DATA:LS_SORT TYPE LVC_S_SORT.

*  _l_append_sort: 1 'SPMON' 'X' '' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form AVL_GRID_SET_DROP_DOWN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_DROP
*&---------------------------------------------------------------------*
FORM AVL_GRID_SET_DROP_DOWN CHANGING CT_DROP TYPE LVC_T_DRAL.
  CT_DROP = VALUE #( BASE CT_DROP
                           ( HANDLE = '1' INT_VALUE = 'A' VALUE = 'A Text' )
                           ( HANDLE = '1' INT_VALUE = 'B' VALUE = 'A Text' ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR USING IV_NAME
                       CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

  DEFINE _L_ADD_TOOLBAR.
    LS_ADD_TOOLBAR-FUNCTION    = &1.
    LS_ADD_TOOLBAR-ICON        = &2.
    LS_ADD_TOOLBAR-QUICKINFO   = &3.
    LS_ADD_TOOLBAR-BUTN_TYPE   = &4.
    LS_ADD_TOOLBAR-DISABLED    = &5.
    LS_ADD_TOOLBAR-TEXT        = &6.

    APPEND LS_ADD_TOOLBAR TO Ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
*  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
*    RETURN.
*  ENDIF.

  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.


  IF SY-TCODE = GC_ZRMM4400_01.
    _L_ADD_TOOLBAR : 'BTN_ON_ZFDLV'  ICON_BATCH   TEXT-T07 '' '' TEXT-T07.    "배치변경
    IF P_RD1C = 'X'.
      _L_ADD_TOOLBAR : 'BTN_ON_QI_AV'  ICON_CHANGE   TEXT-T08 '' '' TEXT-T08.    "품질-->가용
      _L_ADD_TOOLBAR : 'BTN_ON_AV_QI'  ICON_CHANGE   TEXT-T10 '' '' TEXT-T10.    "가용-->품질
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> IV_ONF4
*&      --> IV_ONF4_BEFORE
*&      --> IV_ONF4_AFTER
*&      --> IV_UCOMM
*&---------------------------------------------------------------------*
FORM EVT_GRID_DATA_CHANGED USING IRF_DATA_CHANGED
                                  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                  IV_ONF4
                                  IV_ONF4_BEFORE
                                  IV_ONF4_AFTER
                                  IV_UCOMM TYPE  SY-UCOMM.


  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).

    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LS_LVC_MODI-ROW_ID.

*-----------------------------------------------------------------
* 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
*-----------------------------------------------------------------
    CASE LS_LVC_MODI-FIELDNAME.
      WHEN 'BUKRS'.
*        PERFORM check_changed_bukrs  USING   irf_data_changed
*                                             ls_lvc_modi
*                                     CHANGING <ls_disp>.

      WHEN OTHERS.
        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
                                              LS_LVC_MODI
                                     CHANGING <LS_DISP>.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_changed_others
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
*& Form EVT_GRID_F4
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

      PERFORM F4_CDVAL_GRID USING    IV_FIELDNAME
                            CHANGING CT_F4_LIST.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f4_cdval_grid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FIELDNAME
*&      <-- CT_F4_LIST
*&---------------------------------------------------------------------*
FORM F4_CDVAL_GRID USING IV_FIELDNAME TYPE LVC_FNAME
                    CHANGING CT_F4_LIST      TYPE TABLE.

  TYPES:BEGIN OF LTY_F4,
          CDVAL TYPE CHAR2,
          TEXT  TYPE LTEXT,
        END OF LTY_F4.

  DATA:LT_F4_LIST TYPE TABLE OF LTY_F4.

*-------------------------------------------------------------
* Set F4 List
* Select 에서 데이타를 가져올 경우는 Type 선언 불필요)
*-------------------------------------------------------------
  LT_F4_LIST = VALUE #( ( CDVAL = 'A1' TEXT = '1st Longt Text' )
                        ( CDVAL = 'A2' TEXT = '2nd Longt Text' ) ).

  MOVE-CORRESPONDING LT_F4_LIST TO CT_F4_LIST.

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
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM EVT_GRID_CHANGED_FINISHED USING IT_GOOD_CELLS TYPE LVC_T_MODI.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).

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

  CASE IV_COLUMN.
    WHEN 'MATERIAL'.
*---------------------------
* 자재 조회(MM03 호출)
*---------------------------
      CHECK LS_DISP-MATERIAL IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATERIAL.
      SET PARAMETER ID 'MXX' FIELD 'K'.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN 'BATCH'.
*---------------------------
* 배치 조회(MSC3N 호출)
*---------------------------
      CHECK LS_DISP-BATCH IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATERIAL.
      SET PARAMETER ID 'CHA' FIELD LS_DISP-BATCH.
      SET PARAMETER ID 'WRK' FIELD ''.
      CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
  ENDCASE.
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
FORM SET_FIELD_CELLTAB_COLOR USING IT_FCAT TYPE LVC_T_FCAT
                            CHANGING CT_SCOL TYPE LVC_T_SCOL
                                     CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  CLEAR : LS_LVC_SCOL.

  READ TABLE IT_FCAT INTO DATA(LS_FIELDCAT) WITH KEY FIELDNAME = GC_MATNAME.

  LS_LVC_SCOL-FNAME = GC_MATNAME.
  LS_LVC_SCOL-COLOR-COL = 4.
  LS_LVC_SCOL-COLOR-INT = 0.
  LS_LVC_SCOL-COLOR-INV = 0.

  INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_QI_AV_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_QI_AV_INIT .

  DATA : LT_SELIDX TYPE LVC_T_ROW.

  CLEAR : GS_CHANGE, GS_DISP.

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

  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.

      GS_CHANGE-QISTOCK = <LS_DISP>-QISTOCK.
*      GS_CHANGE-AVAILABLESTOCK = <LS_DISP>-AVAILABLESTOCK.
      GS_CHANGE-MEINS = <LS_DISP>-MEINS.
      GS_CHANGE-ZMEINS = <LS_DISP>-MEINS.
      GS_CHANGE-ZDATE = SY-DATLO.

      GS_DISP = <LS_DISP>.

      CALL SCREEN 0200 STARTING AT 20 10.
    ENDIF.
  ENDLOOP.

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

  IF GRF_DE_CON IS NOT INITIAL.
    CALL METHOD GRF_DE_CON->FREE.
    CLEAR GRF_DE_CON.
    CLEAR GRF_DE_GRID.
    CLEAR : GT_DE_FIELD[].
  ENDIF.

  " Create a custom container control for our ALV Control
  CREATE OBJECT GRF_DE_CON
    EXPORTING
      CONTAINER_NAME = 'CC0200'
    EXCEPTIONS
      OTHERS         = 1.

  " Create ALV
  CREATE OBJECT GRF_DE_GRID
    EXPORTING
      I_PARENT = GRF_DE_CON.

  " Layout
  GS_DE_LAYOUT-SEL_MODE = 'A'.
  GS_DE_LAYOUT-CWIDTH_OPT = ' '.
  GS_DE_LAYOUT-NO_TOOLBAR = 'X'.
  GS_DE_LAYOUT-NO_ROWMARK = 'X'.

  PERFORM FCAT_DEFINE TABLES GT_DE_FIELD.

  " display.
  CALL METHOD GRF_DE_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = GS_DE_LAYOUT
    CHANGING
      IT_OUTTAB       = GT_DISP[]
      IT_FIELDCATALOG = GT_DE_FIELD.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FCAT_DEFINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DE_FIELD
*&---------------------------------------------------------------------*
FORM FCAT_DEFINE TABLES CT_DE_FIELD STRUCTURE LVC_S_FCAT.

  DATA : LS_FIELD TYPE LVC_S_FCAT.

  DEFINE _L_SET_FCAT.
    CLEAR : LS_FIELD.
    LS_FIELD-FIELDNAME    = &1.
    LS_FIELD-COL_POS      = &2.
    LS_FIELD-KEY             = &3.
    LS_FIELD-COLTEXT        = &4.
    LS_FIELD-JUST             = &5.
    LS_FIELD-OUTPUTLEN      = &6.
    LS_FIELD-CFIELDNAME      = &7.
    APPEND LS_FIELD TO CT_DE_FIELD.
  END-OF-DEFINITION.

  _L_SET_FCAT: 'QISTOCK' 1 '' TEXT-C34 '' '3' ''.

  _L_SET_FCAT: 'MEINS' 2 '' TEXT-C26 '' '3' ''.

  _L_SET_FCAT: 'AVAILABLESTOCK' 3 '' TEXT-C35 '' '3' ''.

  _L_SET_FCAT: 'ZMEINS' 4 '' TEXT-C26 '' '3' ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_AV_QI_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_AV_QI_INIT .

  DATA : LT_SELIDX TYPE LVC_T_ROW.

  CLEAR : GS_CHANGE, GS_DISP.

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

  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.

*      GS_CHANGE-QISTOCK = <LS_DISP>-QISTOCK.
      GS_CHANGE-AVAILABLESTOCK = <LS_DISP>-AVAILABLESTOCK.
      GS_CHANGE-MEINS = <LS_DISP>-MEINS.
      GS_CHANGE-ZMEINS = <LS_DISP>-MEINS.
      GS_CHANGE-ZDATE = SY-DATLO.

      GS_DISP = <LS_DISP>.

      CALL SCREEN 0300 STARTING AT 20 10.
    ENDIF.
  ENDLOOP.

ENDFORM.
