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
      WHEN 'BUKRS'. "회사코드
        _L_SET_FCAT: 01  ''  ''  TEXT-C01  ''  ''  ''  ''  '06'.
      WHEN 'BELNR'. "송장문서번호
        _L_SET_FCAT: 02  'X'  ''  TEXT-C02  ''  ''  ''  ''  '12'.
      WHEN 'GJAHR'. "회계연도
        _L_SET_FCAT: 03  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
      WHEN 'BUDAT'. "전기일
        _L_SET_FCAT: 04  ''  ''  TEXT-C22  ''  ''  ''  ''  '10'.
      WHEN 'BLDAT'. "증빙일
        _L_SET_FCAT: 05  ''  ''  TEXT-C23  ''  ''  ''  ''  '10'.
      WHEN 'BELNR_FI'.  "FI 전표번호
        _L_SET_FCAT: 06  ''  ''  TEXT-C04  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'GJAHR_FI'.  "회계연도
        _L_SET_FCAT: 07  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'BUZEI_FI'.  "항목
        _L_SET_FCAT: 08  ''  ''  TEXT-C05  ''  ''  ''  ''  '05'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'SRMWWR'.  "반제금액
        _L_SET_FCAT: 09  ''  ''  TEXT-C06  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 10  ''  ''  TEXT-C07  ''  ''  ''  ''  '05'.
      WHEN 'ZTERM'. "지급조건
        _L_SET_FCAT: 11  ''  ''  TEXT-C08  ''  ''  ''  ''  '08'.
      WHEN 'ZTERM_TEXT'.  "지급조건내역
        _L_SET_FCAT: 12  ''  ''  TEXT-C09  ''  ''  ''  ''  '20'.
      WHEN 'MWSKZ'. "세금코드
        _L_SET_FCAT: 13  ''  ''  TEXT-C10  ''  ''  ''  ''  '08'.
      WHEN 'ZLSCH'. "지급방법
        _L_SET_FCAT: 14  ''  ''  TEXT-C11  ''  ''  ''  ''  '08'.
      WHEN 'ZLSCH_TEXT'.  "지급방법내역
        _L_SET_FCAT: 15  ''  ''  TEXT-C12  ''  ''  ''  ''  '25'.
      WHEN 'EBELN'. "구매오더
        _L_SET_FCAT: 16  ''  ''  TEXT-C13  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'BUPLA'. "사업장
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
      WHEN 'BUKRS'. "회사코드
        _L_SET_FCAT: 01  ''  ''  TEXT-C01  ''  ''  ''  ''  '06'.
      WHEN 'BELNR'. "선급전표번호
        _L_SET_FCAT: 02  ''  ''  TEXT-C14  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'GJAHR'. "회계연도
        _L_SET_FCAT: 03  ''  ''  TEXT-C03  ''  ''  ''  ''  '06'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'BUZEI'. "항목
        _L_SET_FCAT: 04  ''  ''  TEXT-C05  ''  ''  ''  ''  '05'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN 'WRBTR'. "선급공급가액
        _L_SET_FCAT: 05  ''  ''  TEXT-C15  ''  ''  'WAERS'  ''  '15'.
      WHEN 'TAX_AMT'. "선급부가세
        _L_SET_FCAT: 06  ''  ''  TEXT-C20  ''  ''  'WAERS'  ''  '15'.
      WHEN 'TOT_AMT'. "선급금총액
        _L_SET_FCAT: 07  ''  ''  TEXT-C21  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 08  ''  ''  TEXT-C07  ''  ''  ''  ''  '05'.
      WHEN 'MWSKZ'. "세금코드
        _L_SET_FCAT: 09  ''  ''  TEXT-C10  ''  ''  ''  ''  '08'.
      WHEN 'CLE_AMT'. "선급반제금액
        _L_SET_FCAT: 10  ''  ''  TEXT-C16  ''  ''  'WAERS'  ''  '15'.
      WHEN 'REM_AMT'. "선급잔액
        _L_SET_FCAT: 11  ''  ''  TEXT-C17  ''  ''  'WAERS'  ''  '15'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'EBELN'. "구매오더
        _L_SET_FCAT: 12  ''  ''  TEXT-C13  ''  ''  ''  ''  '12'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'LIFNR'. "공급업체
        _L_SET_FCAT: 13  ''  ''  TEXT-C18  ''  ''  ''  ''  '10'.
      WHEN 'NAME1'. "업체명
        _L_SET_FCAT: 14  ''  ''  TEXT-C19  ''  ''  ''  ''  '30'.
      WHEN 'BUPLA'. "사업장
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
      REPID     = SY-REPID    "프로그램명 id
      DYNNR     = SY-DYNNR    "화면번호(Screen)
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

*-- Column 비율
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
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row 시 자동으로 입력될 필드 관리
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
      IV_NAME    = 'ALV_GRID_01'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY_01
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD
*     IV_SCR_WR  = '20:10:70'
      IV_CELLC   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_02
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_02.

  DATA: LS_TOOLBTN TYPE ZSCN00004.
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row 시 자동으로 입력될 필드 관리

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row


  CREATE OBJECT GRF_GRID_02
    EXPORTING
      IV_NAME    = 'ALV_GRID_02'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY_02
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IV_SCR_WR  = '20:10:70'
      IV_CELLC   = ''.             "공통 스트럭쳐 CELLC필드 사용하지 않을 경우.

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
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

  "회사코드 TEXT
  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

  "구매조직 TEXT
  SELECT SINGLE EKOTX
    FROM T024E
   WHERE EKORG = @P_EKORG
    INTO @DATA(LV_EKOTX).

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER: TEXT-F01   P_BUKRS   LV_BUTXT,
                 TEXT-F04   P_EKORG   LV_EKOTX,
                 TEXT-F03   P_PERSN   GV_PERNM.

*-----------------------------------
* Header 주석
*-----------------------------------

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
FORM CHECK_EXIT.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
