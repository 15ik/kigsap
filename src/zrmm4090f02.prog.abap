*&---------------------------------------------------------------------*
*& Include          ZRMM4090F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID_110
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_110 .
  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docing container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_PO.

*--------------------------------
* Create Item Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_POITEM.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_POHEADER ).

*--------------------------------
* Dislay Item Grid..
*--------------------------------
    GRF_ITEM->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_DISP_PO ).


    DATA(LV_MODE) = SPACE.

    CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = LV_MODE ) .
    CALL METHOD GRF_ITEM->SET_CHANGE_MODE( CHANGING CV_MODE = LV_MODE ) .

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
*& Form CREATE_ALV_GRID_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_PO .
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
  PERFORM SET_HEADER_INFO_200 CHANGING LT_HEADER.

*--------------------------------------------------
* Set Lock Name..
*--------------------------------------------------
  DATA: LV_TIMESTAMP_OUT TYPE TIMESTAMP.
  GET TIME STAMP FIELD LV_TIMESTAMP_OUT.
*  DATA(LV_LOCK_NM) = S_LAEDA-LOW. "LV_TIMESTAMP_OUT.

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID_PO'   "다수의 그리드일 경우 식별하기 위함..
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
*& Form SET_HEADER_INFO_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO_200 CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.
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

  READ TABLE GT_POHEADER ASSIGNING FIELD-SYMBOL(<LS_POHEADER>) INDEX 1.

*---------------------------------------
* Header Text 지정
*---------------------------------------
  DATA(LV_TEXT1_TXT) = S_UDATE-LOW && '~' && S_UDATE-HIGH.

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER : TEXT-H01  <LS_POHEADER>-BUKRS  <LS_POHEADER>-BUTXT .  "회사코드
  _L_SET_HEADER : TEXT-H02  LV_TEXT1_TXT         ''.         "생성일

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_POITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_POITEM .

  DEFINE _L_ADD_FIELD.

    LT_DFTVL = VALUE #( BASE LT_DFTVL ( FIELDNAME = &1 VALUE = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.

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

  CREATE OBJECT GRF_ITEM
    EXPORTING
      IV_NAME    = 'ALV_ITEM_PO'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY_ITEM
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN.
*      IRF_HEAD   = GRF_HEAD
*     iv_scr_wr  = '20:10:70'
*      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).
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
*FORM EVT_GRID_DATA_CHANGED  USING IRF_DATA_CHANGED
*                                  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
*                                  IV_ONF4
*                                  IV_ONF4_BEFORE
*                                  IV_ONF4_AFTER
*                                  IV_UCOMM TYPE  SY-UCOMM.
*
*
*  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).
*
*    READ TABLE GT_POITEM ASSIGNING FIELD-SYMBOL(<LS_POITEM>) INDEX LS_LVC_MODI-ROW_ID.
*
**-----------------------------------------------------------------
** 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
**-----------------------------------------------------------------
*    CASE LS_LVC_MODI-FIELDNAME.
*
*      WHEN OTHERS.
*        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
*                                              LS_LVC_MODI
*                                     CHANGING <LS_POITEM>.
*    ENDCASE.
*
*  ENDLOOP.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_IN_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ME_>M_NAME
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR  USING    IV_NAME
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
  _L_ADD_TOOLBAR : 'BTN_ON_TRANS'  ICON_TRANSPORT   TEXT-U01 '' '' TEXT-U01.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_ITEM_PO_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ME_>M_NAME
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_ITEM_TOOLBAR  USING    IV_NAME
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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM EVT_GRID_CHANGED_FINISHED  USING IT_GOOD_CELLS TYPE LVC_T_MODI.
  GRF_GRID->REFRESH_GRID_DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_PO_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_PO_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

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
      WHEN 'EBELN'.
        _L_SET_FCAT: 1   ''  ''  '구매오더번호'        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'BUKRS'.
        _L_SET_FCAT: 2   ''  ''  '회사코드'      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'BUTXT'.
        _L_SET_FCAT: 3   ''  ''  '회사명'      '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'BSART'.
        _L_SET_FCAT: 4   ''  ''  '구매오더유형'    'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'ZAEDAT'.
        _L_SET_FCAT: 5   ''  ''  '생성일'      'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 9.
      WHEN 'UDATE'.
        _L_SET_FCAT: 6   ''  ''  '변경일'        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 9.
      WHEN 'UTIME'.
        _L_SET_FCAT: 7   ''  ''  '변경시간'      'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 9.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 8   ''  ''  '업체코드'      'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'NAME1'.
        _L_SET_FCAT: 9  ''  ''   '업체명'         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'ZEBELN'.
        _L_SET_FCAT: 10  ''   ''   '대체PO번호'     'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'LOEKZ'.
        _L_SET_FCAT: 11  ''   ''   '삭제지시자' 'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'ZFLAG'.
        _L_SET_FCAT: 12  ''   ''   'IUD'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 3.
      WHEN 'ZMSG'.
        _L_SET_FCAT: 13  ''   ''   '메시지'     '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.

  GT_FCAT[] = CT_FCAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_ITEM_PO_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_ITEM_PO_FCAT_MODIFY  CHANGING CT_FCAT TYPE LVC_T_FCAT.

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
      WHEN 'EBELN'.
        _L_SET_FCAT: 1   ''  ''  '구매오더번호'        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'EBELP'.
        _L_SET_FCAT: 2   ''  ''  '항번'      'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'MATNR'.
        _L_SET_FCAT: 3   ''  ''  '자재코드'      'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'TXZ01'.
        _L_SET_FCAT: 4   ''  ''  '자재명'    '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 24.
      WHEN 'WERKS'.
        _L_SET_FCAT: 5   ''  ''  '플랜트'      'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'NAME1'.
        _L_SET_FCAT: 6   ''  ''  '플랜트명'        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 16.
      WHEN 'LGORT'.
        _L_SET_FCAT: 7   ''  ''  '저장위치'      'C' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'LGOBE'.
        _L_SET_FCAT: 8   ''  ''  '저장위치명'      '' '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'LOEKZ'.
        _L_SET_FCAT: 9  ''  ''   '삭제지시자'         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'CHARG'.
        IF P_BUKRS = GC_1101.
          _L_SET_FCAT: 10  ''   ''   '배치'     'C' '' '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 10  ''   ''   '배치'     'C' '' '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 8.
        ENDIF.
      WHEN 'BWTAR'.
        IF P_BUKRS <> GC_1101.
          _L_SET_FCAT: 11  ''   ''   '평가유형' 'C'  ''  '' '' 'X'.
        ELSE.
          _L_SET_FCAT: 11  ''   ''   '평가유형' 'C'  ''  '' '' ''.
          <LS_FCAT>-OUTPUTLEN = 8.
        ENDIF.
      WHEN 'RETPO'.
        _L_SET_FCAT: 12  ''   ''   '반품'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'EINDT'.
        _L_SET_FCAT: 13  ''   ''   '납품예정일'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MENGE'.
        _L_SET_FCAT: 14  ''   ''   '수량'     'C' ''  '' 'MEINS' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'MEINS'.
        _L_SET_FCAT: 15  ''   ''   '단위'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'ELIKZ'.
        _L_SET_FCAT: 16  ''   ''   '납품완료'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'ZEBELN'.
        _L_SET_FCAT: 17  ''   ''   '대체PO번호'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'ZEBELP'.
        _L_SET_FCAT: 18  ''   ''   '대체PO번호항번'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
      WHEN 'INSMK'.
        _L_SET_FCAT: 19  ''   ''   '재고유형'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZFLAG'.
        _L_SET_FCAT: 20  ''   ''   'IUD'     'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 3.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_PO_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN_FIELDNAME
*&---------------------------------------------------------------------*
FORM EVT_GRID_PO_DOUBLE_CLICK   USING IV_ROW    TYPE LVC_INDEX
                                  IV_COLUMN TYPE LVC_FNAME.

  DATA : LT_LVC_SCOL TYPE LVC_T_SCOL.

  READ TABLE GT_POHEADER INTO DATA(LS_POHEADER) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_POHEADER-MSGTB ).

    WHEN OTHERS.

      PERFORM GET_ITEM_IN USING LS_POHEADER.
  ENDCASE.

  LOOP AT GT_POHEADER ASSIGNING FIELD-SYMBOL(<LS_POHEADER>) WHERE CELLC IS NOT INITIAL.

    CLEAR : <LS_POHEADER>-CELLC.

  ENDLOOP.

  PERFORM SET_FIELD_CELLTAB_COLOR_PO CHANGING LT_LVC_SCOL LS_POHEADER.
  INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_POHEADER-CELLC.

  MODIFY GT_POHEADER FROM LS_POHEADER INDEX IV_ROW.

  "Refresh
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ITEM_IN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_POHEADER
*&---------------------------------------------------------------------*
FORM GET_ITEM_IN  USING IS_POHEADER TYPE TS_POHEADER.

  DATA : LS_DISP_PO TYPE TS_POITEM.

*-
  _G_INIT: GT_DISP_PO.
*-

  IF IS_POHEADER IS INITIAL.
    RETURN.
  ENDIF.

  DATA(LT_POITEM) = GT_POITEM[].

  LOOP AT LT_POITEM INTO DATA(LS_POITEM) WHERE EBELN = IS_POHEADER-EBELN.

    MOVE-CORRESPONDING LS_POITEM TO LS_DISP_PO.

    APPEND LS_DISP_PO TO GT_DISP_PO. CLEAR LS_DISP_PO.

  ENDLOOP.

  FREE LT_POITEM.

  SORT GT_DISP_PO BY EBELN EBELP.

  "Refresh
  GRF_ITEM->REFRESH_GRID_DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_COLOR_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_LVC_SCOL
*&      <-- LS_POHEADER
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB_COLOR_PO  CHANGING CT_SCOL TYPE LVC_T_SCOL
                                          CS_DISP TYPE TS_POHEADER.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  _G_INIT CT_SCOL.

  LOOP AT GT_FCAT INTO DATA(LS_FCAT).

    CLEAR LS_LVC_SCOL.

    LS_LVC_SCOL-FNAME = LS_FCAT-FIELDNAME.


    LS_LVC_SCOL-COLOR-COL = 5.
    LS_LVC_SCOL-COLOR-INT = 0.
    LS_LVC_SCOL-COLOR-INV = 0.

    INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.

  ENDLOOP.
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
* Create Item Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_ITEM.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_IDHEADER ).

*--------------------------------
* Dislay Item Grid..
*--------------------------------
    GRF_ITEM->SET_GRID( EXPORTING IV_VARI = P_VAR
                        CHANGING  CT_DATA = GT_DISP_ID ).


    DATA(LV_MODE) = SPACE.

    CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = LV_MODE ) .
    CALL METHOD GRF_ITEM->SET_CHANGE_MODE( CHANGING CV_MODE = LV_MODE ) .

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_IN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID.
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
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO  CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.
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

  READ TABLE GT_IDHEADER ASSIGNING FIELD-SYMBOL(<LS_IDHEADER>) INDEX 1.

*---------------------------------------
* Header Text 지정
*---------------------------------------
  DATA(LV_TEXT1_TXT) = S_UDATE-LOW && '~' && S_UDATE-HIGH.

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER : TEXT-H01  <LS_IDHEADER>-BUKRS  <LS_IDHEADER>-BUTXT.  "회사코드
  _L_SET_HEADER : TEXT-H02  LV_TEXT1_TXT         ''.         "생성일

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

    LT_DFTVL = VALUE #( BASE LT_DFTVL ( FIELDNAME = &1 VALUE = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.

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

  CREATE OBJECT GRF_ITEM
    EXPORTING
      IV_NAME    = 'ALV_ITEM'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY_ITEM
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN.
*      IRF_HEAD   = GRF_HEAD
*     iv_scr_wr  = '20:10:70'
*      IT_HEADER  = LT_HEADER.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN_FIELDNAME
*&---------------------------------------------------------------------*
FORM EVT_GRID_DOUBLE_CLICK  USING IV_ROW    TYPE LVC_INDEX
                                  IV_COLUMN TYPE LVC_FNAME.

  DATA : LT_LVC_SCOL TYPE LVC_T_SCOL.

  READ TABLE GT_IDHEADER INTO DATA(LS_IDHEADER) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_IDHEADER-MSGTB ).

    WHEN OTHERS.

      PERFORM GET_ITEM USING LS_IDHEADER.
  ENDCASE.

  LOOP AT GT_IDHEADER ASSIGNING FIELD-SYMBOL(<LS_IDHEADER>) WHERE CELLC IS NOT INITIAL.

    CLEAR : <LS_IDHEADER>-CELLC.

  ENDLOOP.

  PERFORM SET_FIELD_CELLTAB_COLOR_ID CHANGING LT_LVC_SCOL LS_IDHEADER.
  INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_IDHEADER-CELLC.

  MODIFY GT_IDHEADER FROM LS_IDHEADER INDEX IV_ROW.

  "Refresh
  GRF_GRID->REFRESH_GRID_DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_IDHEADER
*&---------------------------------------------------------------------*
FORM GET_ITEM  USING IS_IDHEADER TYPE TS_IDHEADER.

  DATA : LS_DISP_ID TYPE TS_IDITEM.

*-
  _G_INIT: GT_DISP_ID.
*-

  IF IS_IDHEADER IS INITIAL.
    RETURN.
  ENDIF.

  DATA(LT_IDITEM) = GT_IDITEM[].

  LOOP AT LT_IDITEM INTO DATA(LS_IDITEM) WHERE VBELN = IS_IDHEADER-VBELN.

    MOVE-CORRESPONDING LS_IDITEM TO LS_DISP_ID.

    APPEND LS_DISP_ID TO GT_DISP_ID. CLEAR LS_DISP_ID.

  ENDLOOP.

  FREE LT_IDITEM.

  SORT GT_DISP_ID BY VBELN POSNR.

  "Refresh
  GRF_ITEM->REFRESH_GRID_DISPLAY( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY  CHANGING CT_FCAT TYPE LVC_T_FCAT.

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
      WHEN 'VBELN'.
        _L_SET_FCAT: 1   ''  ''  '납품문서'        'C' ''  '' '' ''.
      WHEN 'BUKRS'.
        _L_SET_FCAT: 2   ''  ''  '회사코드'      'C'  '' '' '' ''.
      WHEN 'BUTXT'.
        _L_SET_FCAT: 3   ''  ''  '회사명'      '' '' '' '' ''.
      WHEN 'LFDAT'.
        _L_SET_FCAT: 4   ''  ''  '납품요청일'    'C' '' '' '' ''.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 5   ''  ''  '업체코드'      'C'  ''  '' '' ''.
      WHEN 'NAME1'.
        _L_SET_FCAT: 6   ''  ''  '업체명'        '' ''  '' '' ''.
      WHEN 'SUBLIFNR'.
        _L_SET_FCAT: 7   ''  ''  '거점'      '' '' '' '' ''.
      WHEN 'SUBNAME'.
        _L_SET_FCAT: 8   ''  ''  '거점명'      '' '' '' '' ''.
      WHEN 'ZTRANS'.
        _L_SET_FCAT: 9  ''  ''   '배차대상여부'         'C'  '' '' '' ''.
      WHEN 'ZTRFEE'.
        _L_SET_FCAT: 10  ''   ''   '운송비정산대상'     'C' '' '' '' ''.
      WHEN 'WERKS'.
        _L_SET_FCAT: 11  ''   ''   '플랜트'     'C' ''  '' '' ''.
      WHEN 'NAME2'.
        _L_SET_FCAT: 12  ''   ''   '플랜트명'     '' ''  '' '' ''.
      WHEN 'LGORT'.
        _L_SET_FCAT: 13  ''   ''   '저장위치'     'C' ''  '' '' ''.
      WHEN 'LGOBE1'.
        _L_SET_FCAT: 14  ''   ''   '저장위치명'     'C' ''  '' '' ''.
      WHEN 'ZWERKS'.
        _L_SET_FCAT: 15  ''   ''   '최종납품플랜트'     'C' ''  '' '' ''.
      WHEN 'NAME3'.
        _L_SET_FCAT: 16  ''   ''   '최종납품플랜트명'     '' ''  '' '' ''.
      WHEN 'ZLGORT'.
        _L_SET_FCAT: 17  ''   ''   '최종납품저장위치'     'C' ''  '' '' ''.
      WHEN 'LGOBE2'.
        _L_SET_FCAT: 18  ''   ''   '최종납품저장위치명'     '' ''  '' '' ''.
      WHEN 'ZSTATUS'.
        _L_SET_FCAT: 19  ''   ''   '상태'     'C' ''  '' '' ''.
      WHEN 'ZMESSAGE'.
        _L_SET_FCAT: 20  ''   ''   '메시지'     '' ''  '' '' ''.
      WHEN 'ZCONFIRM'.
        _L_SET_FCAT: 21  ''   ''   '확정여부'     'C' ''  '' '' 'X'.
      WHEN 'VSBED'.
        _L_SET_FCAT: 22  ''   ''   '출하조건'     'C' ''  '' '' ''.
      WHEN 'ZTRMEMO'.
        _L_SET_FCAT: 23  ''   ''   '운송비고'     'C' ''  '' '' ''.
      WHEN 'ZGRMEMO'.
        _L_SET_FCAT: 24  ''   ''   '입고메모'     '' ''  '' '' ''.
      WHEN 'LOEKZ'.
        _L_SET_FCAT: 25  ''   ''   '삭제'     'C' ''  '' '' ''.
      WHEN 'ZWMSIFID'.
        _L_SET_FCAT: 26  ''   ''   '전송 I/F'     'C' ''  '' '' ''.
      WHEN 'ZWMSTRCID'.
        _L_SET_FCAT: 27  ''   ''   '전송 TRC ID'     'C' ''  '' '' ''.
      WHEN 'ZFLAG'.
        _L_SET_FCAT: 28  ''   ''   'IUD'     'C' ''  '' '' ''.
      WHEN 'ERDAT'.
        _L_SET_FCAT: 29  ''   ''   '생성일'     'C' ''  '' '' 'X'.
      WHEN 'ERZET'.
        _L_SET_FCAT: 30  ''   ''   '생성시간'     'C' ''  '' '' 'X'.
      WHEN 'ERNAM'.
        _L_SET_FCAT: 31  ''   ''   '생성자'     'C' ''  '' '' 'X'.
      WHEN 'AEDAT'.
        _L_SET_FCAT: 32  ''   ''   '변경일'     'C' ''  '' '' 'X'.
      WHEN 'AEZET'.
        _L_SET_FCAT: 33  ''   ''   '변경시간'     'C' ''  '' '' 'X'.
      WHEN 'AENAM'.
        _L_SET_FCAT: 34  ''   ''   '변경자'     'C' ''  '' '' 'X'.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.

  GT_FCAT[] = CT_FCAT[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_ITEM_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_ITEM_FCAT_MODIFY   CHANGING CT_FCAT TYPE LVC_T_FCAT.

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
      WHEN 'VBELN'.
        _L_SET_FCAT: 1   ''  ''  '납품문서'        'C' ''  '' '' ''.
      WHEN 'POSNR'.
        _L_SET_FCAT: 2   ''  ''  '항번'      'C'  '' '' '' ''.
      WHEN 'MATNR'.
        _L_SET_FCAT: 3   ''  ''  '자재코드'      'C' '' '' '' ''.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 4   ''  ''  '자재명'    '' '' '' '' ''.
      WHEN 'WERKS'.
        _L_SET_FCAT: 5   ''  ''  '플랜트'      'C'  ''  '' '' ''.
      WHEN 'NAME1'.
        _L_SET_FCAT: 6   ''  ''  '플랜트명'        '' ''  '' '' ''.
      WHEN 'LGORT'.
        _L_SET_FCAT: 7   ''  ''  '저장위치'      'C' '' '' '' ''.
      WHEN 'LGOBE'.
        _L_SET_FCAT: 8   ''  ''  '저장위치명'      '' '' '' '' ''.
      WHEN 'LFIMG'.
        _L_SET_FCAT: 9  ''  ''   '수량'         'C'  '' '' 'VRKME' ''.
      WHEN 'VRKME'.
        _L_SET_FCAT: 10  ''   ''   '단위'     'C' '' '' '' ''.
      WHEN 'VGBEL'.
        _L_SET_FCAT: 11  ''   ''   'PO번호' 'C'  ''  '' '' ''.
      WHEN 'VGPOS'.
        _L_SET_FCAT: 12  ''   ''   'PO항번'     'C' ''  '' '' ''.
      WHEN 'PSTYP'.
        _L_SET_FCAT: 13  ''   ''   'ItemCategory'     'C' ''  '' '' ''.
      WHEN 'CHARG'.
        _L_SET_FCAT: 14  ''   ''   '배치'     'C' ''  '' '' ''.
      WHEN 'BWTAR'.
        _L_SET_FCAT: 15  ''   ''   '평가유형'     'C' ''  '' '' ''.
      WHEN 'HSDAT'.
        _L_SET_FCAT: 16  ''   ''   '제조일'     'C' ''  '' '' ''.
      WHEN 'LICHN'.
        _L_SET_FCAT: 17  ''   ''   '제조처 LOT'     'C' ''  '' '' ''.
      WHEN 'VFDAT'.
        _L_SET_FCAT: 18  ''   ''   '유효기간'     'C' ''  '' '' ''.
      WHEN 'ZMAKER'.
        _L_SET_FCAT: 19  ''   ''   'MAKER'     'C' ''  '' '' ''.
      WHEN 'ZBRESV1'.
        _L_SET_FCAT: 20  ''   ''   '예비 1'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV2'.
        _L_SET_FCAT: 21  ''   ''   '예비 2'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV3'.
        _L_SET_FCAT: 22  ''   ''   '예비 3'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV4'.
        _L_SET_FCAT: 23  ''   ''   '예비 4'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV5'.
        _L_SET_FCAT: 24  ''   ''   '예비 5'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV6'.
        _L_SET_FCAT: 25  ''   ''   '예비 6'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV7'.
        _L_SET_FCAT: 26  ''   ''   '예비 7'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV8'.
        _L_SET_FCAT: 27  ''   ''   '예비 8'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV9'.
        _L_SET_FCAT: 28  ''   ''   '예비 9'     'C' ''  '' '' 'X'.
      WHEN 'ZBRESV10'.
        _L_SET_FCAT: 29  ''   ''   '예비 10'     'C' ''  '' '' 'X'.
      WHEN 'ZPALQTY'.
        _L_SET_FCAT: 30  ''   ''   '파렛트 수량'     'C' ''  '' '' ''.
      WHEN 'ZINSPECTNO'.
        _L_SET_FCAT: 31  ''   ''   '검사의뢰번호'     'C' ''  '' '' ''.
      WHEN 'ZDOCPATH1'.
        _L_SET_FCAT: 32  ''   ''   '문서 1'     'C' ''  '' '' 'X'.
      WHEN 'ZDOCPATH2'.
        _L_SET_FCAT: 33  ''   ''   '문서 2'     'C' ''  '' '' 'X'.
      WHEN 'ZDOCPATH3'.
        _L_SET_FCAT: 34  ''   ''   '문서 3'     'C' ''  '' '' 'X'.
      WHEN 'ZTFLAG'.
        _L_SET_FCAT: 35  ''   ''   '전송상태'     'C' ''  '' '' ''.
      WHEN 'ZFLAG'.
        _L_SET_FCAT: 36  ''   ''   'IUD'     'C' ''  '' '' ''.
      WHEN 'ZINSPECTION'.
        IF P_BUKRS = GC_1101.
        _L_SET_FCAT: 37  ''   ''   '사전검사대상'     'C' ''  '' '' ''.
        ELSE.
        _L_SET_FCAT: 37  ''   ''   '사전검사대상'     'C' ''  '' '' 'X'.
        ENDIF.
      WHEN 'ERDAT'.
        _L_SET_FCAT: 38  ''   ''   '생성일'     'C' ''  '' '' ''.
      WHEN 'ERZET'.
        _L_SET_FCAT: 39  ''   ''   '생성시간'     'C' ''  '' '' ''.
      WHEN 'ERNAM'.
        _L_SET_FCAT: 40  ''   ''   '생성자'     'C' ''  '' '' ''.
      WHEN 'AEDAT'.
        _L_SET_FCAT: 41  ''   ''   '변경일'     'C' ''  '' '' ''.
      WHEN 'AEZET'.
        _L_SET_FCAT: 42  ''   ''   '변경시간'     'C' ''  '' '' ''.
      WHEN 'AENAM'.
        _L_SET_FCAT: 43  ''   ''   '변경자'     'C' ''  '' '' ''.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_COLOR_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_LVC_SCOL
*&      <-- LS_IDHEADER
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB_COLOR_ID  CHANGING CT_SCOL TYPE LVC_T_SCOL
                                          CS_DISP TYPE TS_IDHEADER.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  _G_INIT CT_SCOL.

  LOOP AT GT_FCAT INTO DATA(LS_FCAT).

    CLEAR LS_LVC_SCOL.

    LS_LVC_SCOL-FNAME = LS_FCAT-FIELDNAME.


    LS_LVC_SCOL-COLOR-COL = 5.
    LS_LVC_SCOL-COLOR-INT = 0.
    LS_LVC_SCOL-COLOR-INV = 0.

    INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.

  ENDLOOP.
ENDFORM.
