*&---------------------------------------------------------------------*
*& Include          ZRMM4410F02
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

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.

*-U3 START : PDF 버튼추가 : 조회조건의 플랜트 = ‘3002’ 인 경우에만
*                 ‘위험물 관리대장 출력’ 버튼을 추가 한다.
  CHECK P_BUKRS = GC_3101.  "영진약품

  IF S_WERKS-OPTION = GC_BT.
    IF  S_WERKS-LOW <= GC_3002 AND S_WERKS-HIGH >= GC_3002.
      _L_ADD_TOOLBAR : 'BTN_ON_PRINT'  ICON_PRINT   TEXT-T01 '' '' TEXT-T01.
      EXIT.
    ENDIF.

  ELSEIF S_WERKS-OPTION = GC_EQ.

    LOOP AT S_WERKS INTO DATA(LS_WERKS).
      IF LS_WERKS-LOW = GC_3002.
        _L_ADD_TOOLBAR : 'BTN_ON_PRINT'  ICON_PRINT   TEXT-T01 '' '' TEXT-T01.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.
*-U3 END

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

  DATA : LV_COLUMN(20) TYPE C.

  CLEAR : LV_COLUMN.
  LV_COLUMN = IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN OTHERS.

      PERFORM GET_DETAIL_DATA USING LS_DISP
                                    LV_COLUMN.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DETAIL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      --> IV_COLUMN
*&---------------------------------------------------------------------*
FORM GET_DETAIL_DATA USING IS_DISP TYPE TS_DISP
                            IV_COLUMN TYPE C.

  DATA : LT_ZCCMMDOCDETAIL TYPE TABLE OF ZCCMMDOCDETAIL,
         LS_DISP_0200      TYPE TS_DISP_0200.

  _G_INIT : LT_ZCCMMDOCDETAIL, GT_DISP_0200.

  IF IS_DISP IS NOT INITIAL.
    IF IS_DISP-LGORT IS NOT INITIAL.
      SELECT FROM ZCCMMDOCDETAIL( P_STARTDATE = @S_DATE-LOW, P_ENDDATE = @S_DATE-HIGH, P_MATNR = @IS_DISP-MATNR, P_WERKS = @IS_DISP-WERKS, P_TYPE = @IV_COLUMN )
        FIELDS MJAHR, MBLNR, ZEILE, BUKRS, WERKS, NAME1, LGORT, LGOBE, MATNR, MAKTX,
              CHARG, BUDAT, UMWRK, TOPLANT, UMLGO, TOSTORAGE, SOBKZ, LIFNR, VENDORNAME, LBBSA_SID,
              BSTAUS_SG, BSTAUS_CG, BWART, BTEXT, BWTAR, KZBEW, MOVEGROUP, STOCK_QTY, MEINS,
              SHKZG, BKTXT, ZTYPE
        WHERE LGORT = @IS_DISP-LGORT
          AND    CHARG = @IS_DISP-CHARG
          AND    BWTAR = @IS_DISP-BWTAR
        INTO CORRESPONDING FIELDS OF TABLE @LT_ZCCMMDOCDETAIL.

    ELSE.
      SELECT FROM ZCCMMDOCDETAIL( P_STARTDATE = @S_DATE-LOW, P_ENDDATE = @S_DATE-HIGH, P_MATNR = @IS_DISP-MATNR, P_WERKS = @IS_DISP-WERKS, P_TYPE = @IV_COLUMN )
        FIELDS MJAHR, MBLNR, ZEILE, BUKRS, WERKS, NAME1, LGORT, LGOBE, MATNR, MAKTX,
              CHARG, BUDAT, UMWRK, TOPLANT, UMLGO, TOSTORAGE, SOBKZ, LIFNR, VENDORNAME, LBBSA_SID,
              BSTAUS_SG, BSTAUS_CG, BWART, BTEXT, BWTAR, KZBEW, MOVEGROUP, STOCK_QTY, MEINS,
              SHKZG, BKTXT, ZTYPE
        WHERE LIFNR = @IS_DISP-LIFNR
          AND    CHARG = @IS_DISP-CHARG
          AND    BWTAR = @IS_DISP-BWTAR
        INTO CORRESPONDING FIELDS OF TABLE @LT_ZCCMMDOCDETAIL.
    ENDIF.
  ENDIF.

  IF LT_ZCCMMDOCDETAIL[] IS NOT INITIAL.
    LOOP AT LT_ZCCMMDOCDETAIL INTO DATA(LS_ZCCMMDOCDETAIL).

      CLEAR : LS_DISP_0200.

      MOVE-CORRESPONDING LS_ZCCMMDOCDETAIL TO LS_DISP_0200.
      APPEND LS_DISP_0200 TO GT_DISP_0200.
    ENDLOOP.

    IF GT_DISP_0200[] IS NOT INITIAL.
      SORT GT_DISP_0200 BY BUDAT MBLNR ZEILE.

      CALL SCREEN 0200 STARTING AT 05 05 ENDING AT 190 30.
    ENDIF.
  ELSE.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M03.  "상세 정보가 없습니다.

  ENDIF.


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
      WHEN 'BUKRS'.             _L_SET_FCAT: 1   'X'  ''  TEXT-F02                'C' ''  '' '' ''.         <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'WERKS'.             _L_SET_FCAT: 2   'X'  ''  TEXT-F03                'C' ''  '' '' ''.         <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'NAME1'.             _L_SET_FCAT: 3   'X'  ''  TEXT-C01                '' ''  '' '' ''.          <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'LGORT'.             _L_SET_FCAT: 4   'X'  ''  TEXT-F04                'C' ''  '' '' ''.
      WHEN 'LGOBE'.             _L_SET_FCAT: 5   'X'  ''  TEXT-C02                '' ''  '' '' ''.
      WHEN 'LIFNR'.             _L_SET_FCAT: 6   'X'  ''  TEXT-C03                'C' ''  '' '' ''.
      WHEN 'NAME2'.             _L_SET_FCAT: 7   'X'  ''  TEXT-C04                '' ''  '' '' ''.
      WHEN 'MATNR'.             _L_SET_FCAT: 8   'X'  ''  TEXT-C05                'C' ''  '' '' ''.          <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'MAKTX'.             _L_SET_FCAT: 9   'X'  ''  TEXT-C06                '' ''  '' '' ''.
      WHEN 'MATKL'.             _L_SET_FCAT: 10  'X'  ''  TEXT-C07                'C' ''  '' '' ''.
      WHEN 'WGBEZ'.             _L_SET_FCAT: 11  'X'  ''  TEXT-C08                '' ''  '' '' ''.
      WHEN 'CHARG'.             _L_SET_FCAT: 12  'X'  ''  TEXT-C09                  'C' ''  '' '' ''.
      WHEN 'BWTAR'.             _L_SET_FCAT: 13  'X'  ''  TEXT-C10                  'C' ''  '' '' ''.
      WHEN 'INITSTOCK'.         _L_SET_FCAT: 14   ''  ''  TEXT-C11        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C500.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'INITSTOCK_VAL'.     _L_SET_FCAT: 15   ''  ''  TEXT-C12        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.
      WHEN 'INITQISTOCK'.       _L_SET_FCAT: 16   ''  ''  TEXT-C13        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'INITQISTOCK_VAL'.   _L_SET_FCAT: 17   ''  ''  TEXT-C14        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.
      WHEN 'INITBLSTOCK'.       _L_SET_FCAT: 18   ''  ''  TEXT-C15        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'INITBLSTOCK_VAL'.   _L_SET_FCAT: 19   ''  ''  TEXT-C16        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.
      WHEN 'TRSTOCKSLQTY'.      _L_SET_FCAT: 20   ''  ''  TEXT-C17        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C500.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'TRSTOCKSLVALUE'.    _L_SET_FCAT: 21   ''  ''  TEXT-C18        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.
      WHEN 'TRSTOCKPLQTY'.      _L_SET_FCAT: 22   ''  ''  TEXT-C19        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C510.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'TRSTOCKPLVALUE'.    _L_SET_FCAT: 23   ''  ''  TEXT-C20        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.
      WHEN 'SITQTY'.            _L_SET_FCAT: 24   ''  ''  TEXT-C21        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C510.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'SITVALUE'.          _L_SET_FCAT: 25   ''  ''  TEXT-C22        '' ''  'WAERS' '' 'X'.    <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_PUR_QTY_UN'.     _L_SET_FCAT: 26   ''  ''  TEXT-C30        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PUR_QTY_QI'.     _L_SET_FCAT: 27   ''  ''  TEXT-C31        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PUR_QTY_BL'.     _L_SET_FCAT: 28   ''  ''  TEXT-C32        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_PRD_QTY_UN'.     _L_SET_FCAT: 29   ''  ''  TEXT-C33        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PRD_QTY_QI'.     _L_SET_FCAT: 30   ''  ''  TEXT-C34        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PRD_QTY_BL'.     _L_SET_FCAT: 31   ''  ''  TEXT-C35        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_DSC_QTY_UN'.     _L_SET_FCAT: 32   ''  ''  TEXT-C36        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_DSC_QTY_QI'.     _L_SET_FCAT: 33   ''  ''  TEXT-C37        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_DSC_QTY_BL'.     _L_SET_FCAT: 34   ''  ''  TEXT-C38        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_STO_QTY_UN'.     _L_SET_FCAT: 35   ''  ''  TEXT-C40        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_QTY_QI'.     _L_SET_FCAT: 36   ''  ''  TEXT-C41        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_QTY_BL'.     _L_SET_FCAT: 37   ''  ''  TEXT-C42        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_QTY_ST'.     _L_SET_FCAT: 38   ''  ''  TEXT-C43         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_SL_QTY_UN'.     _L_SET_FCAT: 39   ''  ''   TEXT-C44     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SL_QTY_QI'.     _L_SET_FCAT: 40   ''  ''   TEXT-C45     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SL_QTY_BL'.     _L_SET_FCAT: 41   ''  ''   TEXT-C46     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_ETC_QTY_UN'.     _L_SET_FCAT: 42   ''  ''  TEXT-C47        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_ETC_QTY_QI'.     _L_SET_FCAT: 43   ''  ''  TEXT-C48        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_ETC_QTY_BL'.     _L_SET_FCAT: 44   ''  ''  TEXT-C49        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_TR_QTY_UN'.     _L_SET_FCAT: 45   ''  ''  TEXT-C50         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_TR_QTY_QI'.     _L_SET_FCAT: 46   ''  ''  TEXT-C51         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_TR_QTY_BL'.     _L_SET_FCAT: 47   ''  ''  TEXT-C52         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_SC_QTY_UN'.     _L_SET_FCAT: 48   ''  ''  TEXT-C53     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SC_QTY_QI'.     _L_SET_FCAT: 49   ''  ''  TEXT-C54     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SC_QTY_BL'.     _L_SET_FCAT: 50   ''  ''  TEXT-C55     '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_INV_QTY_UN'.     _L_SET_FCAT: 51   ''  ''  TEXT-C56       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_INV_QTY_QI'.     _L_SET_FCAT: 52   ''  ''  TEXT-C57       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_INV_QTY_BL'.     _L_SET_FCAT: 53   ''  ''  TEXT-C58       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.<LS_FCAT>-NO_SIGN = ''.

      WHEN 'LASTQTY_GRUN'.        _L_SET_FCAT: 54   ''  ''  TEXT-C59      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C400.
      WHEN 'LASTQTY_GRQI'.        _L_SET_FCAT: 55   ''  ''  TEXT-C60      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'LASTQTY_GRBL'.        _L_SET_FCAT: 56   ''  ''  TEXT-C61      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C510.


      WHEN 'GI_RETS_QTY_UN'.     _L_SET_FCAT: 57   ''  ''  TEXT-C62   '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.
      WHEN 'GI_RETS_QTY_QI'.     _L_SET_FCAT: 58   ''  ''  TEXT-C63   '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.
      WHEN 'GI_RETS_QTY_BL'.     _L_SET_FCAT: 59   ''  ''  TEXT-C64   '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.

      WHEN 'SIT_QTY_UN'.     _L_SET_FCAT: 60   ''  ''  TEXT-C65                '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C200.
      WHEN 'SIT_QTY_QI'.     _L_SET_FCAT: 61   ''  ''  TEXT-C66                '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C100.
      WHEN 'SIT_QTY_BL'.     _L_SET_FCAT: 62   ''  ''  TEXT-C67                '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C210.

      WHEN 'GI_PRD_QTY_UN'.     _L_SET_FCAT: 63   ''  ''  TEXT-C68        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_PRD_QTY_QI'.     _L_SET_FCAT: 64   ''  ''  TEXT-C69        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_PRD_QTY_BL'.     _L_SET_FCAT: 65   ''  ''  TEXT-C70        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_SO_QTY_UN'.     _L_SET_FCAT: 66   ''  ''  TEXT-C71         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_SO_QTY_QI'.     _L_SET_FCAT: 67   ''  ''  TEXT-C72         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_SO_QTY_BL'.     _L_SET_FCAT: 68   ''  ''  TEXT-C73         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_STO_QTY_UN'.     _L_SET_FCAT: 69   ''  ''  TEXT-C74        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_STO_QTY_QI'.     _L_SET_FCAT: 70   ''  ''  TEXT-C75        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_STO_QTY_BL'.     _L_SET_FCAT: 71   ''  ''  TEXT-C76        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C601.
      WHEN 'GI_STO_QTY_ST'.     _L_SET_FCAT: 72   ''  ''  TEXT-C77         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_SL_QTY_UN'.     _L_SET_FCAT: 73   ''  ''  TEXT-C78        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_SL_QTY_QI'.     _L_SET_FCAT: 74   ''  ''  TEXT-C79        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_SL_QTY_BL'.     _L_SET_FCAT: 75   ''  ''  TEXT-C80        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_ETC_QTY_UN'.     _L_SET_FCAT: 76   ''  ''  TEXT-C81        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_ETC_QTY_QI'.     _L_SET_FCAT: 77   ''  ''  TEXT-C82        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_ETC_QTY_BL'.     _L_SET_FCAT: 78   ''  ''  TEXT-C83        '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_TR_QTY_UN'.     _L_SET_FCAT: 79   ''  ''  TEXT-C84         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_TR_QTY_QI'.     _L_SET_FCAT: 80   ''  ''  TEXT-C85         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_TR_QTY_BL'.     _L_SET_FCAT: 81   ''  ''  TEXT-C86         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_SC_QTY_UN'.     _L_SET_FCAT: 82   ''  ''  TEXT-C87         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_SC_QTY_QI'.     _L_SET_FCAT: 83   ''  ''  TEXT-C88         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_SC_QTY_BL'.     _L_SET_FCAT: 84   ''  ''  TEXT-C89         '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_RETM_QTY_UN'.     _L_SET_FCAT: 85   ''  ''  TEXT-C90       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_RETM_QTY_QI'.     _L_SET_FCAT: 86   ''  ''  TEXT-C91   '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_RETM_QTY_BL'.     _L_SET_FCAT: 87   ''  ''  TEXT-C92   '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.

      WHEN 'GI_INV_QTY_UN'.     _L_SET_FCAT: 88   ''  ''  TEXT-C93       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C600.
      WHEN 'GI_INV_QTY_QI'.     _L_SET_FCAT: 89   ''  ''  TEXT-C94       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C710.
      WHEN 'GI_INV_QTY_BL'.     _L_SET_FCAT: 90   ''  ''  TEXT-C95       '' ''  '' 'MEINS' ''.     <LS_FCAT>-EMPHASIZE = GC_C700.


      WHEN 'LASTQTY_GIUN'.        _L_SET_FCAT: 91   ''  ''  TEXT-C96      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C400.
      WHEN 'LASTQTY_GIQI'.        _L_SET_FCAT: 92   ''  ''  TEXT-C97      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C500.
      WHEN 'LASTQTY_GIBL'.        _L_SET_FCAT: 93   ''  ''  TEXT-C98      '' ''  '' 'MEINS' ''.         <LS_FCAT>-EMPHASIZE = GC_C510.

      WHEN 'MEINS'.     _L_SET_FCAT: 94   ''  ''  TEXT-C99        'C' ''  '' '' ''.

      WHEN 'GR_PUR_VAL_UN'.     _L_SET_FCAT: 95   ''  ''  TEXT-D01       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PUR_VAL_QI'.     _L_SET_FCAT: 96   ''  ''  TEXT-D02       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PUR_VAL_BL'.     _L_SET_FCAT: 97   ''  ''  TEXT-D03       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_PRD_VA_UN'.     _L_SET_FCAT: 98   ''  ''  TEXT-D04        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PRD_VA_QI'.     _L_SET_FCAT: 99   ''  ''  TEXT-D05        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_PRD_VA_BL'.     _L_SET_FCAT: 100  ''  ''  TEXT-D06        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_DSC_VAL_UN'.     _L_SET_FCAT: 101   ''  ''  TEXT-D07       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_DSC_VAL_QI'.     _L_SET_FCAT: 102   ''  ''  TEXT-D08       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_DSC_VAL_BL'.     _L_SET_FCAT: 103   ''  ''  TEXT-D09       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_STO_VAL_UN'.     _L_SET_FCAT: 104   ''  ''  TEXT-D10       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_VAL_QI'.     _L_SET_FCAT: 105   ''  ''  TEXT-D11       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_VAL_BL'.     _L_SET_FCAT: 106   ''  ''  TEXT-D12       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_STO_VAL_ST'.     _L_SET_FCAT: 107   ''  ''  TEXT-D13        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_SL_VAL_UN'.     _L_SET_FCAT: 108   ''  ''  TEXT-D14   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SL_VAL_QI'.     _L_SET_FCAT: 109   ''  ''  TEXT-D15   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SL_VAL_BL'.     _L_SET_FCAT: 110   ''  ''  TEXT-D16   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_ETC_VAL_UN'.     _L_SET_FCAT: 111   ''  ''  TEXT-D17      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_ETC_VAL_QI'.     _L_SET_FCAT: 112   ''  ''  TEXT-D18      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_ETC_VAL_BL'.     _L_SET_FCAT: 113   ''  ''  TEXT-D19      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_TR_VAL_UN'.     _L_SET_FCAT: 114   ''  ''  TEXT-D20      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_TR_VAL_QI'.     _L_SET_FCAT: 115   ''  ''  TEXT-D21      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_TR_VAL_BL'.     _L_SET_FCAT: 116   ''  ''  TEXT-D22      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_SC_VAL_UN'.     _L_SET_FCAT: 117   ''  ''  TEXT-D23   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SC_VAL_QI'.     _L_SET_FCAT: 118   ''  ''  TEXT-D24   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_SC_VAL_BL'.     _L_SET_FCAT: 119   ''  ''  TEXT-D25   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GR_INV_VAL_UN'.     _L_SET_FCAT: 120   ''  ''  TEXT-D26     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_INV_VAL_QI'.     _L_SET_FCAT: 121   ''  ''  TEXT-D27     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GR_INV_VAL_BL'.     _L_SET_FCAT: 122   ''  ''  TEXT-D28     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_RETS_VAL_UN'.     _L_SET_FCAT: 123   ''  ''  TEXT-D29 '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_RETS_VAL_QI'.     _L_SET_FCAT: 124   ''  ''  TEXT-D30 '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_RETS_VAL_BL'.     _L_SET_FCAT: 125   ''  ''  TEXT-D31 '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'SIT_VAL_UN'.     _L_SET_FCAT: 126   ''  ''  TEXT-D32        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'SIT_VAL_QI'.     _L_SET_FCAT: 127   ''  ''  TEXT-D33        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'SIT_VAL_BL'.     _L_SET_FCAT: 128   ''  ''  TEXT-D34        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_PRD_VAL_UN'.     _L_SET_FCAT: 129   ''  ''  TEXT-D35        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_PRD_VAL_QI'.     _L_SET_FCAT: 130   ''  ''  TEXT-D36        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_PRD_VAL_BL'.     _L_SET_FCAT: 131   ''  ''  TEXT-D37        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_SO_VAL_UN'.     _L_SET_FCAT: 132   ''  ''  TEXT-D38         '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SO_VAL_QI'.     _L_SET_FCAT: 133   ''  ''  TEXT-D39         '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SO_VAL_BL'.     _L_SET_FCAT: 134   ''  ''  TEXT-D40         '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_STO_VAL_UN'.     _L_SET_FCAT: 135   ''  ''  TEXT-D41        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_STO_VAL_QI'.     _L_SET_FCAT: 136   ''  ''  TEXT-D42        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_STO_VAL_BL'.     _L_SET_FCAT: 137   ''  ''  TEXT-D43        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_STO_VAL_ST'.     _L_SET_FCAT: 138   ''  ''  TEXT-D44         '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_SL_VAL_UN'.     _L_SET_FCAT: 139   ''  ''  TEXT-D45     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SL_VAL_QI'.     _L_SET_FCAT: 140   ''  ''  TEXT-D46     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SL_VAL_BL'.     _L_SET_FCAT: 141   ''  ''  TEXT-D47     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_ETC_VAL_UN'.     _L_SET_FCAT: 142   ''  ''  TEXT-D48        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_ETC_VAL_QI'.     _L_SET_FCAT: 143   ''  ''  TEXT-D49        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_ETC_VAL_BL'.     _L_SET_FCAT: 144   ''  ''  TEXT-D50        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_TR_VAL_UN'.     _L_SET_FCAT: 145   ''  ''  TEXT-D51        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_TR_VAL_QI'.     _L_SET_FCAT: 146   ''  ''  TEXT-D52        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_TR_VAL_BL'.     _L_SET_FCAT: 147   ''  ''  TEXT-D53        '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_SC_VAL_UN'.     _L_SET_FCAT: 148   ''  ''  TEXT-D54     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SC_VAL_QI'.     _L_SET_FCAT: 149   ''  ''  TEXT-D55     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_SC_VAL_BL'.     _L_SET_FCAT: 150   ''  ''  TEXT-D56     '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_RETM_VAL_UN'.     _L_SET_FCAT: 151   ''  ''  TEXT-D57   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_RETM_VAL_QI'.     _L_SET_FCAT: 152   ''  ''  TEXT-D58   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_RETM_VAL_BL'.     _L_SET_FCAT: 153   ''  ''  TEXT-D59   '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'GI_INV_VAL_UN'.     _L_SET_FCAT: 154   ''  ''  TEXT-D60       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_INV_VAL_QI'.     _L_SET_FCAT: 155   ''  ''  TEXT-D61      '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.
      WHEN 'GI_INV_VAL_BL'.     _L_SET_FCAT: 156   ''  ''  TEXT-D62       '' ''  'WAERS' '' 'X'. <LS_FCAT>-NO_SIGN = ''.

      WHEN 'WAERS'.             _L_SET_FCAT: 157   ''  ''  TEXT-D63                      'C' ''  '' '' 'X'.
      WHEN 'LASTQTY_SIT'.        _L_SET_FCAT: 158   ''  '' TEXT-D64            '' ''  '' 'MEINS' ''.    <LS_FCAT>-EMPHASIZE = GC_C300.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'LASTQTY_UN'.        _L_SET_FCAT: 159   ''  ''  TEXT-D65            '' ''  '' 'MEINS' ''. <LS_FCAT>-EMPHASIZE = GC_C500.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'LASTQTY_QI'.        _L_SET_FCAT: 160   ''  ''  TEXT-D66            '' ''  '' 'MEINS' ''. <LS_FCAT>-EMPHASIZE = GC_C700.<LS_FCAT>-NO_SIGN = ''.
      WHEN 'LASTQTY_BL'.        _L_SET_FCAT: 161   ''  ''  TEXT-D67            '' ''  '' 'MEINS' ''. <LS_FCAT>-EMPHASIZE = GC_C600.<LS_FCAT>-NO_SIGN = ''.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_0200 CHANGING CT_FCAT TYPE LVC_T_FCAT.
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


  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'MJAHR'.
        _L_SET_FCAT: 1   ''  ''  TEXT-E01        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'MBLNR'.
        _L_SET_FCAT: 2   ''  ''  TEXT-E02        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'ZEILE'.
        _L_SET_FCAT: 3   ''  ''  TEXT-E03        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 3.
      WHEN 'WERKS'.
        _L_SET_FCAT: 4   ''  ''  TEXT-F03        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'NAME1'.
        _L_SET_FCAT: 5   ''  ''  TEXT-C01        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'LGORT'.
        _L_SET_FCAT: 6   ''  ''  TEXT-F04        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'LGOBE'.
        _L_SET_FCAT: 7   ''  ''  TEXT-C02       '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MATNR'.
        _L_SET_FCAT: 8   ''  ''  TEXT-C05        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 9   ''  ''  TEXT-C06        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'CHARG'.
        _L_SET_FCAT: 10   ''  ''  TEXT-C09        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'BUDAT'.
        _L_SET_FCAT: 11   ''  ''  TEXT-E04        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'UMWRK'.
        _L_SET_FCAT: 12   ''  ''  TEXT-E05        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'TOPLANT'.
        _L_SET_FCAT: 13   ''  ''  TEXT-E06        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'UMLGO'.
        _L_SET_FCAT: 14   ''  ''  TEXT-E07        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'TOSTORAGE'.
        _L_SET_FCAT: 15   ''  ''  TEXT-E08        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 16   ''  ''  TEXT-E09        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'VENDORNAME'.
        _L_SET_FCAT: 17   ''  ''  TEXT-E10        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'BWART'.
        _L_SET_FCAT: 18   ''  ''  TEXT-E11        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'BTEXT'.
        _L_SET_FCAT: 19   ''  ''  TEXT-E12        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'BWTAR'.
        _L_SET_FCAT: 20   ''  ''  TEXT-E13        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 3.
      WHEN 'STOCK_QTY'.
        _L_SET_FCAT: 21   ''  ''  TEXT-E14        'C' ''  '' 'MEINS' ''.
        <LS_FCAT>-DO_SUM = 'X'.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'MEINS'.
        _L_SET_FCAT: 22   ''  ''  TEXT-C99        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'BKTXT'.
        _L_SET_FCAT: 23   ''  ''  TEXT-E15        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
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
*     iv_scr_wr  = '20:10:70'
      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).


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

*---------------------------------------------------
* 그리드가 여러개일 경우 PARMA으로 구분하여 사용
*---------------------------------------------------

  IF GRF_DOCKING_CON_0200 IS INITIAL.

* Creating Docing container instance
    PERFORM CREATE_CONTAINER_0200.   "각 구간별 Container Split

*--------------------------------
* Create Alv Grid
*--------------------------------
*-- Header Grid..
    PERFORM CREATE_ALV_GRID_0200.

*--------------------------------
* Dislay Grid..
*--------------------------------
*-- Header Grid
    GRF_GRID_0200->SET_GRID( "EXPORTING IV_VARI = P_VAR
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
                        CHANGING  CT_DATA = GT_DISP_0200 ).

  ELSE.
    GRF_GRID_0200->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER_0200 .

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON_0200
    EXPORTING
      REPID     = SY-REPID " 프로그램명 id
      DYNNR     = SY-DYNNR " 화면번호 (Screen) "
      SIDE      = GRF_DOCKING_CON_0200->DOCK_AT_TOP "
      EXTENSION = 10000.


*----------------------------------------------------
* Split Container (1 Row: ALV Grid 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(LRF_SPLITTER_0200) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = GRF_DOCKING_CON_0200
                                                      NO_AUTODEF_PROGID_DYNNR = 'X'
                                                      ROWS    = 1
                                                      COLUMNS = 1 ).
  LRF_SPLITTER_0200->SET_ROW_MODE( MODE = CL_GUI_SPLITTER_CONTAINER=>TYPE_MOVABLE ).
  LRF_SPLITTER_0200->SET_ROW_HEIGHT( ID = 1 HEIGHT = 200 ).
  LRF_SPLITTER_0200->SET_BORDER( BORDER = SPACE ).

*--------------------------------
* Set Header Cocntainer
*--------------------------------
  GRF_HEAD_0200     = LRF_SPLITTER_0200->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_0200 .
  DEFINE _L_ADD_FIELD.

    LT_DFTVL = VALUE #( BASE LT_DFTVL ( FIELDNAME = &1 VALUE = &2 ) ).

  END-OF-DEFINITION.

  DATA: LS_TOOLBTN TYPE ZSCN00004,
        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.
*       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  PERFORM SET_ALV_DEFAULT_TOOLBAR CHANGING LS_TOOLBTN.

*--------------------------------------------------
* Set Lock Name..
*--------------------------------------------------

*--------------------------------------------------
* Create Object Grid
*--------------------------------------------------
  CREATE OBJECT GRF_GRID_0200
    EXPORTING
      IV_NAME    = 'ALV_GRID_0200'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_HEAD_0200
*     IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IRF_HEAD   = GRF_HEAD           "Header Information
*     iv_scr_wr  =                    "Default 20:10:70
*     IT_HEADER  = LT_HEADER
      IV_CELLC   = ''
      IV_CELLS   = ''.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ALV_DEFAULT_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_TOOLBTN
*&---------------------------------------------------------------------*
FORM SET_ALV_DEFAULT_TOOLBAR CHANGING CS_TOOLBAR TYPE ZSCN00004.

*  CS_TOOLBAR-btn_add    = 'X'.       "Add Row
*  CS_TOOLBAR-btn_madd   = 'X'.       "Multi Add Row
*  CS_TOOLBAR-btn_del    = 'X'.       "Delete Row
*  CS_TOOLBAR-BTN_REC    = 'X'.       "Recovery Row
  CS_TOOLBAR-BTN_EXLD   = 'X'.       "Excel Download
*  CS_TOOLBAR-BTN_EXLU   = 'X'.       "Excel Uploa
*  CS_TOOLBAR-MLTI_LINES = GV_MROW.   "Multi Row

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

  IF S_DATE[] IS NOT INITIAL.
    READ TABLE S_DATE INDEX 1.
    IF S_DATE-HIGH IS NOT INITIAL.
      CONCATENATE S_DATE-LOW '~' S_DATE-HIGH INTO DATA(LV_DATE).
    ELSE.
      LV_DATE = S_DATE-LOW.
    ENDIF.
  ENDIF.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX 1.

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
    _L_SET_HEADER : TEXT-H02  S_WERKS-LOW  LS_DISP-NAME1 .
  ENDIF.

  _L_SET_HEADER : TEXT-H03  LV_DATE ''.

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)
ENDFORM.
