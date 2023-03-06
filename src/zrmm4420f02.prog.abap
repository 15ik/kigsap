*&---------------------------------------------------------------------*
*& Include          ZRMM4420F02
*&---------------------------------------------------------------------*
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
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ME_>M_NAME
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
*& Form F4_CDVAL_GRID
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
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DATA: LV_CNT TYPE SY-TABIX,
        LV_MOD TYPE I.

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
      WHEN 'BUKRS'.
        _L_SET_FCAT: 1   ''  ''  TEXT-C01        'C' ''  '' '' 'X'.
      WHEN 'COMPANYCODENAME'.
        _L_SET_FCAT: 2   ''  ''  TEXT-C02      'C'  '' '' '' 'X'.
      WHEN 'MATNR'.
        _L_SET_FCAT: 3   ''  ''  TEXT-C03      'C' '' '' '' ''.
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_C300.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MATERIALNAME'.
        _L_SET_FCAT: 4   ''  ''  TEXT-C04    '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
      WHEN 'MATKL'.
        _L_SET_FCAT: 5  ''  ''   TEXT-C05          'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'WGBEZ'.
        _L_SET_FCAT: 6  ''  ''   TEXT-C06          '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
      WHEN 'WERKS'.
        _L_SET_FCAT: 7   ''  ''  TEXT-C07      'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'PLANTNAME'.
        _L_SET_FCAT: 8   ''  ''  TEXT-C08        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 16.
      WHEN 'EKGRP'.
        _L_SET_FCAT: 9   ''  ''  TEXT-C09      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'EKNAM'.
        _L_SET_FCAT: 10   ''  '' TEXT-C10      ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'DISPO'.
        _L_SET_FCAT: 11   ''  ''  TEXT-C11      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'DSNAM'.
        _L_SET_FCAT: 12   ''  ''  TEXT-C12      ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'LGORT'.
        IF P_RD2C = 'X'.
          _L_SET_FCAT: 13   ''  ''  TEXT-C13    'C' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 13   ''  ''  TEXT-C13    'C' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 6.
        ENDIF.
      WHEN 'STORAGELOCATIONNAME'.
        IF P_RD2C = 'X'.
          _L_SET_FCAT: 14  ''  ''  TEXT-C14        'C'  ''  '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 14  ''  ''  TEXT-C14        ''  ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 12.
        ENDIF.
      WHEN 'LIFNR'.
        IF P_RD2B = 'X'.
          _L_SET_FCAT: 15  ''  ''  TEXT-C15          'C' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 15  ''  ''  TEXT-C15          'C' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 8.
        ENDIF.
      WHEN 'NAME1'.
        IF P_RD2B = 'X'.
          _L_SET_FCAT: 16  ''  ''   TEXT-C16         'C'  '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 16  ''  ''   TEXT-C16         ''  '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
      WHEN 'TYPECODE'.
        _L_SET_FCAT: 17  ''   ''   TEXT-C17     'C' '' '' '' 'X'.
      WHEN 'TYPENAME'.
        _L_SET_FCAT: 18  ''   ''   TEXT-C18 'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'INVENTORYSPECIALSTOCKTYPE'.
        _L_SET_FCAT: 19  ''   ''   TEXT-C19     '' ''  '' '' 'X'.
      WHEN 'PERIODTYPE'.
        _L_SET_FCAT: 20  ''   ''   TEXT-C20     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'BATCH'.
        IF P_CHK1 = 'X'.
          _L_SET_FCAT: 21  ''   ''   TEXT-C21       'C' ''  '' '' ''.
          <LS_FCAT>-HOTSPOT = 'X'.
          <LS_FCAT>-EMPHASIZE = GC_C700.
          <LS_FCAT>-OUTPUTLEN = 10.
        ELSE.
          _L_SET_FCAT: 21  ''   ''   TEXT-C21       'C' ''  '' '' 'X'.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.

      WHEN 'LICHN'.
        IF P_CHK1 = 'X'.
          _L_SET_FCAT: 22  ''   ''   TEXT-C22       'C' ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ELSE.
          _L_SET_FCAT: 22  ''   ''   TEXT-C22       'C' ''  '' '' 'X'.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
      WHEN 'HSDAT'.
        IF P_CHK1 = 'X'.
          _L_SET_FCAT: 23  ''   ''   TEXT-C23       'C' ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ELSE.
          _L_SET_FCAT: 23  ''   ''   TEXT-C23       'C' ''  '' '' 'X'.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.
      WHEN 'VFDAT'.
        IF P_CHK1 = 'X'.
          _L_SET_FCAT: 24  ''   ''   TEXT-C24       'C' ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 10.
        ELSE.
          _L_SET_FCAT: 24  ''   ''   TEXT-C24       'C' ''  '' '' 'X'.
          <LS_FCAT>-OUTPUTLEN = 10.
        ENDIF.

      WHEN 'MEINS'.
        _L_SET_FCAT: 25  ''   ''     TEXT-C25       'C'  ''  '' '' ''.

*      WHEN 'BKLAS'.
*        _L_SET_FCAT: 26   ''  ''     TEXT-C26       'C' '' '' '' ''. "U9 KTGA-3924: 평가클래스 삭제 BKLAS 20230109
*        <LS_FCAT>-OUTPUTLEN = 4.

      WHEN OTHERS.
        CASE <LS_FCAT>-FIELDNAME(03).

          WHEN 'DAT'.
            LV_CNT = 27.

            READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY DAY = <LS_FCAT>-FIELDNAME BINARY SEARCH.
            IF SY-SUBRC = 0.
              _L_SET_FCAT: LV_CNT   '' 'X'  LS_DAY-ZDATE ''  '' '' 'MEINS' ''.

              LV_CNT = LS_DAY-DAY+3(3).
              LV_MOD = LV_CNT MOD 2.

              IF LV_MOD = 0.
                <LS_FCAT>-EMPHASIZE = GC_C210.
              ELSE.
                <LS_FCAT>-EMPHASIZE = GC_C400.
              ENDIF.
            ELSE.
              <LS_FCAT>-TECH = 'X'.
            ENDIF.

*            ADD 1 TO LV_CNT.
            <LS_FCAT>-NO_ZERO = 'X'.
            <LS_FCAT>-OUTPUTLEN = 10.

          WHEN OTHERS.
            <LS_FCAT>-TECH = 'X'.
        ENDCASE.
*        <LS_FCAT>-NO_OUT = 'X'.
    ENDCASE.
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
    WHEN 'MATNR'.
*---------------------------
* 자재 조회(MM03 호출)
*---------------------------
      CHECK LS_DISP-MATNR IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'MXX' FIELD 'K'.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'BATCH'.
*---------------------------
* 배치 조회(MSC3N 호출)
*---------------------------
      CHECK LS_DISP-BATCH IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'CHA' FIELD LS_DISP-BATCH.
      SET PARAMETER ID 'WRK' FIELD ''.
      CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.


    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_100 .
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
  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 140 ).
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

    LT_DFTVL = VALUE #( BASE LT_DFTVL ( FIELDNAME = &1 VALUE = &2 ) ).

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
      IV_SCR_WR  = '20:10:70'
      IT_HEADER  = LT_HEADER
      IV_CELLC   = ''
      IV_CELLS   = ''.
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
    CLEAR LS_HEADER.
    LS_HEADER-KEY  = &1.
    LS_HEADER-INFO  = &2.
    LS_HEADER-TEXT  = &3.
    APPEND LS_HEADER TO CT_HEADER.
    ENDIF.
  END-OF-DEFINITION.

**  CONSTANTS : lc_if(20) TYPE c VALUE 'Controlling Area '.
  READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX 1.

*---------------------------------------
* Header Text 지정
*---------------------------------------
  DATA(LV_TEXT1_TXT) = S_BUDAT-LOW && '~' && S_BUDAT-HIGH.


*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER : TEXT-H01  P_BUKRS <LS_DISP>-COMPANYCODENAME  .     "회사코드
  _L_SET_HEADER : TEXT-H02  LV_TEXT1_TXT  '' .                       "조회기간

  CASE 'X'. "기간유형
    WHEN P_RD1A.
      _L_SET_HEADER : TEXT-H03  TEXT-T01  '' .
    WHEN P_RD1B.
      _L_SET_HEADER : TEXT-H03  TEXT-T02  '' .
    WHEN P_RD1C.
      _L_SET_HEADER : TEXT-H03  TEXT-T03  '' .
    WHEN P_RD1D.
      _L_SET_HEADER : TEXT-H03  TEXT-T04  '' .
  ENDCASE.

  CASE 'X'. "작업구분
    WHEN P_RD2A.
      _L_SET_HEADER : TEXT-H04  TEXT-T05  '' .
    WHEN P_RD2B.
      _L_SET_HEADER : TEXT-H04  TEXT-T06  '' .
    WHEN P_RD2C.
      _L_SET_HEADER : TEXT-H04  TEXT-T07  '' .
    WHEN P_RD2D.
      _L_SET_HEADER : TEXT-H04  TEXT-T08  '' .
  ENDCASE.

  CASE 'X'. "재고유형
    WHEN P_RD3A.
      _L_SET_HEADER : TEXT-H05  TEXT-T09  '' .
    WHEN P_RD3B.
      _L_SET_HEADER : TEXT-H05  TEXT-T10  '' .
    WHEN P_RD3C.
      _L_SET_HEADER : TEXT-H05  TEXT-T11  '' .
    WHEN P_RD3D.
      _L_SET_HEADER : TEXT-H05  TEXT-T12  '' .
  ENDCASE.

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)
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

  CASE GV_OK_CODE.
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
FORM REFRESH_DATA_110 .
  PERFORM GET_DATA.
  PERFORM PROCESSING_DATA.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).
ENDFORM.
