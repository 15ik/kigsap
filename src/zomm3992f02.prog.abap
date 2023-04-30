*&---------------------------------------------------------------------*
*& Include          ZOMM3992F02
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
    PERFORM CREATE_ALV_GRID.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( CHANGING  CT_DATA = GT_DISP ).

  ELSE.
    GRF_GRID->REFRESH_GRID_DISPLAY( ).
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID.

  DATA: LS_TOOLBTN TYPE ZSCN00004.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_DOCKING_CON
      IS_TOOLBTN = LS_TOOLBTN
      IV_CELLC   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      IV_CELLS   = ''.          "공통 스트럭쳐 CELLS필드 사용하지 않을 경우

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
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
    <LS_FCAT>-OUTPUTLEN  = &9.
*    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATUS'.  "처리결과
        _L_SET_FCAT: 01  'X'  ''  TEXT-C01  'C'  ''  ''  ''  '06'.
      WHEN 'CNTR_NO'.  "AS-IS 계약요청
        _L_SET_FCAT: 02  'X'  ''  TEXT-C02  ''  ''  ''  ''  '12'.
      WHEN 'CNTR_ITEM'.  "AS-IS 품목
        _L_SET_FCAT: 03  'X'  ''  TEXT-C03  ''  ''  ''  ''  '05'.
      WHEN 'BANFN'. "구매요청번호
        _L_SET_FCAT: 04  ''  ''  TEXT-C04  ''  ''  ''  ''  '10'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'BNFPO'. "품목
        _L_SET_FCAT: 05  ''  ''  TEXT-C03  ''  ''  ''  ''  '05'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-FIX_COLUMN = 'X'.

      WHEN 'BSART'. "구매요청유형
        _L_SET_FCAT: 11  ''  ''  TEXT-C05  ''  ''  ''  ''  '06'.
      WHEN 'TITLE'. "구매요청명
        _L_SET_FCAT: 12  ''  ''  TEXT-C06  ''  ''  ''  ''  '30'.
      WHEN 'REQER'. "요청자
        _L_SET_FCAT: 13  ''  ''  TEXT-C07  ''  ''  ''  ''  '12'.
      WHEN 'PSTYP'. "품목범주
        _L_SET_FCAT: 14  ''  ''  TEXT-C08  ''  ''  ''  ''  '05'.
      WHEN 'MATNR'. "자재
        _L_SET_FCAT: 15  ''  ''  TEXT-C09  ''  ''  ''  ''  '15'.
      WHEN 'TXZ01'. "자재내역(공사,대표자재)
        _L_SET_FCAT: 16  ''  ''  TEXT-C10  ''  ''  ''  ''  '30'.
      WHEN 'BWTAR'. "평가유형
        _L_SET_FCAT: 17  ''  ''  TEXT-C11  ''  ''  ''  ''  '06'.
      WHEN 'MENGE'. "요청수량
        _L_SET_FCAT: 18  ''  ''  TEXT-C12  ''  ''  ''  'MEINS'  '12'.
      WHEN 'MEINS'. "단위
        _L_SET_FCAT: 19  ''  ''  TEXT-C13  ''  ''  ''  ''  '05'.
      WHEN 'LFDAT'. "납품일
        _L_SET_FCAT: 20  ''  ''  TEXT-C14  ''  ''  ''  ''  '10'.
      WHEN 'WERKS'. "플랜트
        _L_SET_FCAT: 21  ''  ''  TEXT-C15  ''  ''  ''  ''  '06'.
      WHEN 'LGORT'. "저장위치
        _L_SET_FCAT: 22  ''  ''  TEXT-C16  ''  ''  ''  ''  '06'.
      WHEN 'ORDER'. "계약담당자
        _L_SET_FCAT: 23  ''  ''  TEXT-C17  ''  ''  ''  ''  '12'.
      WHEN 'LIFNR'. "공급업체
        _L_SET_FCAT: 24  ''  ''  TEXT-C18  ''  ''  ''  ''  '10'.
      WHEN 'EKGRP'. "구매그룹
        _L_SET_FCAT: 25  ''  ''  TEXT-C19  ''  ''  ''  ''  '06'.
      WHEN 'PREIS'. "추정가격
        _L_SET_FCAT: 26  ''  ''  TEXT-C20  ''  ''  'WAERS'  ''  '12'.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 27  ''  ''  TEXT-C21  ''  ''  ''  ''  '05'.
      WHEN 'SAKTO'. "G/L계정
        _L_SET_FCAT: 28  ''  ''  TEXT-C22  ''  ''  ''  ''  '10'.
      WHEN 'WBSNO'. "WBS
        _L_SET_FCAT: 29  ''  ''  TEXT-C23  ''  ''  ''  ''  '10'.
      WHEN 'KOSTL'. "코스트센터
        _L_SET_FCAT: 30  ''  ''  TEXT-C24  ''  ''  ''  ''  '10'.

      WHEN 'MESSAGE'. "처리메시지
        _L_SET_FCAT: 99  ''  ''  TEXT-C25  ''  ''  ''  ''  '50'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

  DEFINE _L_ADD_TOOLBAR.
    LS_ADD_TOOLBAR-FUNCTION    = &1.
    LS_ADD_TOOLBAR-ICON        = &2.
    LS_ADD_TOOLBAR-QUICKINFO   = &3.
    LS_ADD_TOOLBAR-BUTN_TYPE   = &4.
    LS_ADD_TOOLBAR-DISABLED    = &5.
    LS_ADD_TOOLBAR-TEXT        = &6.

    APPEND LS_ADD_TOOLBAR TO CT_TOOLBAR.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.

  _L_ADD_TOOLBAR: "'BTN_CONVERS'   ICON_TRANSLATION  TEXT-U03 '' '' TEXT-U03,
                  'BTN_ERR_DOWN'  ICON_EXPORT       TEXT-U04 '' '' TEXT-U04.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVT_GRID_DOUBLE_CLICK USING IV_ROW
                                 IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'BANFN'.
      CHECK LS_DISP-BANFN IS NOT INITIAL.

      SET PARAMETER ID 'BAN' FIELD LS_DISP-BANFN.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
