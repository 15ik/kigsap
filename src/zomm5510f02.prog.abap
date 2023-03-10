*&---------------------------------------------------------------------*
*& Include          ZOMM5510F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form EVT_GRID_01_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_GRID_01_DOUBLE_CLICK USING IV_ROW
                                    IV_COLUMN.

  READ TABLE GT_DISP_01 INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID_01->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'BELNR'.
      CHECK LS_DISP-BELNR IS NOT INITIAL AND LS_DISP-GJAHR IS NOT INITIAL.

      SET PARAMETER ID 'RBN' FIELD LS_DISP-BELNR.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR.

      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN 'BELNR_FI'.
      CHECK LS_DISP-BELNR_FI IS NOT INITIAL AND LS_DISP-GJAHR_FI IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD LS_DISP-BELNR_FI.
      SET PARAMETER ID 'BUK' FIELD P_BUKRS.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR_FI.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_02_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_GRID_02_DOUBLE_CLICK USING IV_ROW
                                    IV_COLUMN.

  READ TABLE GT_DISP_02 INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID_01->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'BELNR'.
      CHECK LS_DISP-BELNR IS NOT INITIAL AND LS_DISP-GJAHR IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD LS_DISP-BELNR.
      SET PARAMETER ID 'BUK' FIELD P_BUKRS.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR.

      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_01_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_01_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-NO_OUT     = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
    <LS_FCAT>-OUTPUTLEN  = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'BUKRS'. "????????????
        _L_SET_FCAT: 01  ''  ''  TEXT-C01  ''  ''  ''  ''  '06'.
      WHEN 'BELNR'. "??????????????????
        _L_SET_FCAT: 02  'X'  ''  TEXT-C02  ''  ''  ''  ''  '12'.
      WHEN 'GJAHR'. "????????????
        _L_SET_FCAT: 03  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
      WHEN 'BUDAT'. "?????????
        _L_SET_FCAT: 04  ''  ''  TEXT-C22  ''  ''  ''  ''  '10'.
      WHEN 'BLDAT'. "?????????
        _L_SET_FCAT: 05  ''  ''  TEXT-C23  ''  ''  ''  ''  '10'.
      WHEN 'BELNR_FI'.  "FI ????????????
        _L_SET_FCAT: 06  ''  ''  TEXT-C04  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'GJAHR_FI'.  "????????????
        _L_SET_FCAT: 07  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'BUZEI_FI'.  "??????
        _L_SET_FCAT: 08  ''  ''  TEXT-C05  ''  ''  ''  ''  '05'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'SRMWWR'.  "????????????
        _L_SET_FCAT: 09  ''  ''  TEXT-C06  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'WAERS'. "??????
        _L_SET_FCAT: 10  ''  ''  TEXT-C07  ''  ''  ''  ''  '05'.
      WHEN 'ZTERM'. "????????????
        _L_SET_FCAT: 11  ''  ''  TEXT-C08  ''  ''  ''  ''  '08'.
      WHEN 'ZTERM_TEXT'.  "??????????????????
        _L_SET_FCAT: 12  ''  ''  TEXT-C09  ''  ''  ''  ''  '20'.
      WHEN 'MWSKZ'. "????????????
        _L_SET_FCAT: 13  ''  ''  TEXT-C10  ''  ''  ''  ''  '08'.
      WHEN 'ZLSCH'. "????????????
        _L_SET_FCAT: 14  ''  ''  TEXT-C11  ''  ''  ''  ''  '08'.
      WHEN 'ZLSCH_TEXT'.  "??????????????????
        _L_SET_FCAT: 15  ''  ''  TEXT-C12  ''  ''  ''  ''  '25'.
      WHEN 'EBELN'. "????????????
        _L_SET_FCAT: 16  ''  ''  TEXT-C13  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'BUPLA'. "?????????
        _L_SET_FCAT: 17  ''  ''  TEXT-C24  ''  ''  ''  ''  '06'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_02_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM ALV_GRID_02_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-EDIT       = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
    <LS_FCAT>-OUTPUTLEN  = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'BUKRS'. "????????????
        _L_SET_FCAT: 01  ''  ''  TEXT-C01  ''  ''  ''  ''  '06'.
      WHEN 'BELNR'. "??????????????????
        _L_SET_FCAT: 02  ''  ''  TEXT-C14  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'GJAHR'. "????????????
        _L_SET_FCAT: 03  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'BUZEI'. "??????
        _L_SET_FCAT: 04  ''  ''  TEXT-C05  ''  ''  ''  ''  '05'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'WRBTR'. "??????????????????
        _L_SET_FCAT: 05  ''  ''  TEXT-C15  ''  ''  'WAERS'  ''  '15'.
      WHEN 'TAX_AMT'. "???????????????
        _L_SET_FCAT: 06  ''  ''  TEXT-C20  ''  ''  'WAERS'  ''  '15'.
      WHEN 'TOT_AMT'. "???????????????
        _L_SET_FCAT: 07  ''  ''  TEXT-C21  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'WAERS'. "??????
        _L_SET_FCAT: 08  ''  ''  TEXT-C07  ''  ''  ''  ''  '05'.
      WHEN 'MWSKZ'. "????????????
        _L_SET_FCAT: 09  ''  ''  TEXT-C10  ''  ''  ''  ''  '08'.
      WHEN 'CLE_AMT'. "??????????????????
        _L_SET_FCAT: 10  ''  ''  TEXT-C16  ''  ''  'WAERS'  ''  '15'.
      WHEN 'REM_AMT'. "????????????
        _L_SET_FCAT: 11  ''  ''  TEXT-C17  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'EBELN'. "????????????
        _L_SET_FCAT: 12  ''  ''  TEXT-C13  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'LIFNR'. "????????????
        _L_SET_FCAT: 13  ''  ''  TEXT-C18  ''  ''  ''  ''  '10'.
      WHEN 'NAME1'. "?????????
        _L_SET_FCAT: 14  ''  ''  TEXT-C19  ''  ''  ''  ''  '30'.
      WHEN 'BUPLA'. "?????????
        _L_SET_FCAT: 15  ''  ''  TEXT-C24  ''  ''  ''  ''  '06'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
FORM SET_GRID.

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docking container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_01.

    PERFORM CREATE_ALV_GRID_02.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID_01->SET_GRID( CHANGING  CT_DATA = GT_DISP_01 ).

    GRF_GRID_02->SET_GRID( CHANGING  CT_DATA = GT_DISP_02 ).

  ELSE.
    GRF_GRID_01->REFRESH_GRID_DISPLAY( ).

    GRF_GRID_02->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER.

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON
    EXPORTING
      REPID     = SY-REPID    "??????????????? id
      DYNNR     = SY-DYNNR    "????????????(Screen)
      SIDE      = GRF_DOCKING_CON->DOCK_AT_TOP
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
  DATA(LRF_GRID_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

  DATA(LRF_SPLIT_BODY) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_GRID_CONT
                                                        NO_AUTODEF_PROGID_DYNNR = 'X'
                                                        ROWS    = 2
                                                        COLUMNS = 1 ).

*-- Column ??????
  LRF_SPLIT_BODY->SET_COLUMN_WIDTH( EXPORTING ID = 1 WIDTH = 50 ).
  LRF_SPLIT_BODY->SET_COLUMN_WIDTH( EXPORTING ID = 2 WIDTH = 50 ).

*-- Set Alv Container..
  GRF_BODY_01 = LRF_SPLIT_BODY->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
  GRF_BODY_02 = LRF_SPLIT_BODY->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_01
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_01.

  DATA: LS_TOOLBTN TYPE ZSCN00004,
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row ??? ???????????? ????????? ?????? ??????
        LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.


  CREATE OBJECT GRF_GRID_01
    EXPORTING
      IV_NAME    = 'ALV_GRID_01'   "????????? ???????????? ?????? ???????????? ??????..
      IRF_PARENT = GRF_BODY_01
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD
*     IV_SCR_WR  = '20:10:70'
      IV_CELLC   = ''           "?????? ???????????? CELLC?????? ???????????? ?????? ??????
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_02
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_02.

  DATA: LS_TOOLBTN TYPE ZSCN00004.
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row ??? ???????????? ????????? ?????? ??????

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row


  CREATE OBJECT GRF_GRID_02
    EXPORTING
      IV_NAME    = 'ALV_GRID_02'   "????????? ???????????? ?????? ???????????? ??????..
      IRF_PARENT = GRF_BODY_02
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IV_SCR_WR  = '20:10:70'
      IV_CELLC   = ''.             "?????? ???????????? CELLC?????? ???????????? ?????? ??????.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID_01->TT_HEADER.

  DATA: LS_HEADER TYPE GRF_GRID_01->TS_HEADER.

  DEFINE _L_SET_HEADER.
    CLEAR LS_HEADER.
    LS_HEADER-KEY   = &1.
    LS_HEADER-INFO  = &2.
    LS_HEADER-TEXT  = &3.
    APPEND LS_HEADER TO CT_HEADER.
  END-OF-DEFINITION.

*---------------------------------------
* Header Text ??????
*---------------------------------------
*-??????????????? ???????????? ??????

  "???????????? TEXT
  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

  "???????????? TEXT
  SELECT SINGLE EKOTX
    FROM T024E
   WHERE EKORG = @P_EKORG
    INTO @DATA(LV_EKOTX).

*-----------------------------------
* Header Column ??????
*-----------------------------------
  _L_SET_HEADER: TEXT-F01   P_BUKRS   LV_BUTXT,
                 TEXT-F04   P_EKORG   LV_EKOTX,
                 TEXT-F03   P_PERSN   GV_PERNM.

*-----------------------------------
* Header ??????
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default ?????? (?????? ????????? ??????)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
