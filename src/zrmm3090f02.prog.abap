*&---------------------------------------------------------------------*
*& Include          ZRMM3090F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
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
*      WHEN 'BUKRS'.
*        PERFORM CHECK_CHANGED_BUKRS  USING   IRF_DATA_CHANGED
*                                             LS_LVC_MODI
*                                     CHANGING <LS_DISP>.
      WHEN OTHERS.
        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
                                              LS_LVC_MODI
                                     CHANGING <LS_DISP>.
    ENDCASE.

  ENDLOOP.

*  CALL METHOD cl_gui_cfw=>set_new_ok_code( 'OK' ).

ENDFORM. " DATA_CHANGED
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_OTHER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> LS_LVC_MODI
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_OTHERS USING IRF_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                   IS_CELL TYPE LVC_S_MODI
                           CHANGING CS_DISP TYPE ANY.


*-------------------------------
* Delete Message
*-------------------------
  _G_SET_MSGTB:'D' CS_DISP IS_CELL-FIELDNAME 'ZCN00' '000' '' '' '' .  "Msg 삭제


ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_OBJECT
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

    APPEND ls_add_toolbar TO CT_TOOLBAR.
  END-OF-DEFINITION.

*-------------------------------------------------
*-- 추가 User Toolbar (그리드명으로 구분하여 사용)
*-------------------------------------------------
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.

  CASE IV_NAME.
    WHEN 'ALV_GRID'.
*      _L_ADD_TOOLBAR : 'MAPPING_CANCEL'  ICON_PREVIOUS_OBJECT  '매핑취소'(U02) '' '' '매핑취소'(U02).

    WHEN 'GRID_DTL'.
*      _L_ADD_TOOLBAR : 'BTN_0100_DTL_CLOSE'  ICON_CLOSE  TEXT-U09 '' '' TEXT-U09.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FIELDNAME
*&      <-- EP_F4_LIST
*&      <-- CV_TITLE
*&---------------------------------------------------------------------*
FORM EVT_GRID_F4 USING IV_FIELDNAME
                  CHANGING CT_F4
                           CV_TITLE .

  CASE IV_FIELDNAME.
    WHEN 'CDVAL'.
      _G_SET_VALUE : CV_TITLE          'F4 Code Value'.

*      PERFORM F4_CDVAL_GRID USING    IV_FIELDNAME
*                            CHANGING CT_F4.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOUBLE CLICK
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


    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      IF LS_DISP-BSART(2) = 'SA'.
        SET PARAMETER ID: 'SAG' FIELD LS_DISP-EBELN.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
      ELSE.
        SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.


    WHEN 'MATNR'.
      PERFORM MOVE_TO_MM03 USING LS_DISP-MATNR
                                 LS_DISP-WERKS.

    WHEN 'LIFNR'.
      CALL METHOD ZCL_MM_COMMON=>CALL_TRANSACTION_BP( IV_LIFNR = LS_DISP-LIFNR ).

    WHEN 'VBELN'.
      CASE LS_DISP-VBELN.
        WHEN SPACE.
          EXIT.
        WHEN GC_VEBLN_MULTI.
          PERFORM POPUP_TO_0200 USING LS_DISP.
        WHEN OTHERS.
          SET PARAMETER ID 'VLM' FIELD LS_DISP-VBELN.
          CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
      ENDCASE.

    WHEN 'ID_QTY'.
      PERFORM POPUP_TO_0200 USING LS_DISP.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOUBLE CLICK_0200
*&---------------------------------------------------------------------*
FORM EVT_GRID_DOUBLE_CLICK_0200 USING IV_ROW
                                  IV_COLUMN.

  READ TABLE GT_DISP_0200 INTO DATA(LS_DISP_0200) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP_0200-MSGTB ).

    WHEN 'VBELN'.
      SET PARAMETER ID 'VLM' FIELD LS_DISP_0200-VBELN.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM POPUP_TO_0200 USING IS_DISP TYPE TS_DISP.

  DATA: LT_200_HIGH TYPE TABLE OF TS_DISP_0200,
        LT_200_LOW  TYPE TABLE OF TS_DISP_0200.

  CLEAR GT_DISP_0200.

  SORT GT_ALL_0200 BY EBELN EBELP.

  READ TABLE GT_ALL_0200 WITH KEY EBELN = IS_DISP-EBELN
                                  EBELP = IS_DISP-EBELP
                         BINARY SEARCH
                         TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    MESSAGE S005 WITH TEXT-M01.
    EXIT.
  ENDIF.

  LOOP AT GT_ALL_0200 INTO DATA(LS_ALL_0200) FROM SY-TABIX.
    IF LS_ALL_0200-EBELN NE IS_DISP-EBELN OR
       LS_ALL_0200-EBELP NE IS_DISP-EBELP.
      EXIT.
    ENDIF.

    LS_ALL_0200-MEINS = IS_DISP-MEINS.

    "납품일 이후 최대 5개 까지만 보여줌.
    IF LS_ALL_0200-EINDT > IS_DISP-EINDT AND
       LINES( LT_200_HIGH ) < 5.
      APPEND LS_ALL_0200 TO LT_200_HIGH.
    ENDIF.

    "납품일 이전 최대 5개 까지만 보여줌.
    IF LS_ALL_0200-EINDT <= IS_DISP-EINDT AND
       LINES( LT_200_LOW ) < 5.
      APPEND LS_ALL_0200 TO LT_200_LOW.
    ENDIF.

  ENDLOOP.

  IF NOT LT_200_HIGH[] IS INITIAL.
    APPEND LINES OF LT_200_HIGH TO GT_DISP_0200.
  ENDIF.

  IF NOT LT_200_LOW[] IS INITIAL.
    APPEND LINES OF LT_200_LOW TO GT_DISP_0200.
  ENDIF.

  SORT GT_DISP_0200 BY EINDT DESCENDING.

  CONCATENATE IS_DISP-EBELN '/'
              IS_DISP-EBELP
              INTO GV_0200_TITLE.

  CALL SCREEN 0200 STARTING AT 05 05 ENDING AT 60 20.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM EVT_GRID_CHANGED_FINISHED USING IT_GOOD_CELLS TYPE LVC_T_MODI.


  GRF_GRID->REFRESH_GRID_DISPLAY( ).


ENDFORM. " DATA_CHANGED
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
                         ( HANDLE = '1' INT_VALUE = 'B' VALUE = 'B Text' ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC_FCAT  text
*----------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  CONSTANTS: LC_COLOR_ZSVAT(5) TYPE C VALUE 'C300',
             LC_COLOR_IM(5)    TYPE C VALUE 'C500'.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-icon       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-EMPHASIZE  = &9.
    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 11   'X' '' TEXT-C01            ''  ''  ''  '' ''.
      WHEN 'NAME1'.
        _L_SET_FCAT: 12   'X' '' TEXT-C02       ''  ''  ''  '' ''.
      WHEN 'EBELN'.
        _L_SET_FCAT: 13   'X' '' TEXT-C03          ''  ''  ''  '' ''.
      WHEN 'EBELP'.
        _L_SET_FCAT: 14   'X' '' TEXT-C04          ''  ''  ''  '' ''.
      WHEN 'MATNR'.
        _L_SET_FCAT: 15   '' ''  TEXT-C05          ''  ''  ''  '' ''.
      WHEN 'TXZ01'.
        _L_SET_FCAT: 16   '' ''  TEXT-C06          ''  ''  ''  '' ''.

      WHEN 'BWTAR'.
        _L_SET_FCAT: 21   '' ''  TEXT-C07       ''   ''  ''  '' ''.
      WHEN 'MEINS'.
        _L_SET_FCAT: 22   '' ''  TEXT-C08           ''  '' ''  '' ''.
      WHEN 'MENGE'.
        _L_SET_FCAT: 23   '' ''  TEXT-C09           ''  '' ''  'MEINS' ''.
      WHEN 'ETENR'.
        _L_SET_FCAT: 24   '' ''  TEXT-C10         ''   ''  ''  '' ''.
      WHEN 'BSTAE'.
        _L_SET_FCAT: 25   '' ''  TEXT-C11         ''   ''  ''  '' ''.
      WHEN 'EINDT'.
        _L_SET_FCAT: 26   '' ''  TEXT-C12         ''   ''  ''  '' ''.
      WHEN 'ELIKZ'.
        _L_SET_FCAT: 27   '' ''  TEXT-C13         ''   ''  ''  '' ''.

      WHEN 'EKET_QTY'.
        _L_SET_FCAT: 31   '' ''  TEXT-C14         ''   ''  ''  'MEINS' ''.
      WHEN 'ID_QTY'.
        _L_SET_FCAT: 32   '' ''  TEXT-C15         ''   ''  ''  'MEINS' ''.
      WHEN 'GR_QTY'.
        _L_SET_FCAT: 33   '' ''  TEXT-C16         ''   ''  ''  'MEINS' ''.
      WHEN 'REMAIN_ID'.
        _L_SET_FCAT: 34   '' ''  TEXT-C17         ''   ''  ''  'MEINS' ''.
      WHEN 'REMAIN_GR'.
        _L_SET_FCAT: 35   '' ''  TEXT-C18         ''   ''  ''  'MEINS' ''.

      WHEN 'WERKS'.
        _L_SET_FCAT: 41   '' ''  TEXT-C19         ''   ''  ''  '' ''.
      WHEN 'LGORT'.
        _L_SET_FCAT: 42   '' ''  TEXT-C20         ''   ''  ''  '' ''.
      WHEN 'VBELN'.
        _L_SET_FCAT: 43   '' ''  TEXT-C21         ''   ''  ''  '' ''.
      WHEN 'BSART'.
        _L_SET_FCAT: 44   '' ''  TEXT-C22         ''   ''  ''  '' ''.
      WHEN 'BATXT'.
        _L_SET_FCAT: 45   '' ''  TEXT-C23         ''   ''  ''  '' ''.
      WHEN 'RETPO'.
        _L_SET_FCAT: 46   '' ''  TEXT-C24         ''   ''  ''  '' ''.

      WHEN 'ZORDER_PERSON'.
        _L_SET_FCAT: 51   '' ''  TEXT-C25         ''   ''  ''  '' ''.
      WHEN 'ZORDER_PERSON_NM'.
        _L_SET_FCAT: 52   '' ''  TEXT-C26         ''   ''  ''  '' ''.
      WHEN 'ZORDER_DEPARTMENT'.
        _L_SET_FCAT: 53   '' ''  TEXT-C27         ''   ''  ''  '' ''.
      WHEN 'ZORDER_DEPARTMENT_NM'.
        _L_SET_FCAT: 54   '' ''  TEXT-C28         ''   ''  ''  '' ''.
      WHEN 'ZEXPEN_PERSON'.
        _L_SET_FCAT: 55   '' ''  TEXT-C29         ''   ''  ''  '' ''.
      WHEN 'ZEXPEN_PERSON_NM'.
        _L_SET_FCAT: 56   '' ''  TEXT-C30         ''   ''  ''  '' ''.
      WHEN 'ZEXPEN_DEPARTMENT'.
        _L_SET_FCAT: 57   '' ''  TEXT-C31         ''   ''  ''  '' ''.
      WHEN 'ZEXPEN_DEPARTMENT_NM'.
        _L_SET_FCAT: 58   '' ''  TEXT-C32         ''   ''  ''  '' ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM. " FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG_MODIFY_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC_FCAT  text
*----------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY_0200 CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-icon       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-outputlen  = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-EMPHASIZE  = &9.
*    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'VBELN'.
        _L_SET_FCAT: 11   'X' ''  TEXT-C21          ''  '10'  ''  '' ''.
      WHEN 'VBELP'.
        _L_SET_FCAT: 12   'X' ''  TEXT-C04          ''  '6'  ''  '' ''.
      WHEN 'EINDT'.
        _L_SET_FCAT: 13   'X' ''  TEXT-C12          ''  '10'  ''  '' ''.
      WHEN 'MENGE'.
        _L_SET_FCAT: 14   '' ''   TEXT-C09          ''  '15'  ''  'MEINS' ''.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM. " FIELDCATALOG_MODIFY_0200
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
    "납품미입고, 미입고 잔량이 0 아니면 COLOR 표시
    IF LS_DISP-REMAIN_ID NE 0 OR LS_DISP-REMAIN_GR NE 0.
      PERFORM SET_FIELD_CELLTAB_COLOR USING   IT_FCAT
                                   CHANGING LT_LVC_SCOL LS_DISP.

      CLEAR LS_DISP-CELLC.
      INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_DISP-CELLC.
    ENDIF.


*-- Modify Line..
    MODIFY GT_DISP FROM LS_DISP INDEX LV_INDEX.

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
FORM SET_FIELD_CELLTAB_COLOR USING IT_FCAT TYPE LVC_T_FCAT
                            CHANGING CT_SCOL TYPE LVC_T_SCOL
                                     CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  IF CS_DISP-EINDT < SY-DATLO.  "현재일보다 과거면 빨강
    LS_LVC_SCOL-COLOR-COL = 6.
    LS_LVC_SCOL-COLOR-INT = 1.
    LS_LVC_SCOL-COLOR-INV = 0.
  ELSEIF CS_DISP-EINDT = SY-DATLO.  "현재일과 같으면 노랑
    LS_LVC_SCOL-COLOR-COL = 3.
    LS_LVC_SCOL-COLOR-INT = 1.
    LS_LVC_SCOL-COLOR-INV = 0.
  ELSE.
    LS_LVC_SCOL-COLOR-COL = 5.
    LS_LVC_SCOL-COLOR-INT = 1.
    LS_LVC_SCOL-COLOR-INV = 0.
  ENDIF.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).

*    CLEAR LS_LVC_SCOL.

    LS_LVC_SCOL-FNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_LVC_SCOL-FNAME.
      WHEN 'EINDT' OR 'REMAIN_ID' OR 'REMAIN_GR'.
        INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_LVC_STYL
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CLEAR LS_LVC_STYL.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

    IF LS_FIELDCAT-EDIT = 'X'.
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
    ENDIF.

**>PUSH BUTTON STYLE 구성
*    CASE LS_LVC_STYL-FIELDNAME.
*      WHEN 'TOLL_MANUF'.
*        IF NOT CS_DISP-TOLL_MANUF IS INITIAL.
*          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*        ENDIF.
*      WHEN 'ATTCH_FILE'.
*        IF NOT CS_DISP-ATTCH_FILE IS INITIAL.
*          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*        ENDIF.
*      WHEN OTHERS.
*    ENDCASE.

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

  _L_APPEND_SORT: 1 'LIFNR' 'X' '' '' '' '' '',
                  2 'NAME1' 'X' '' '' '' '' '',
                  3 'EBELN' '' 'X' '' '' '' '',
                  4 'EBELP' 'X' '' '' '' '' ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_grid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID .

*---------------------------------------------------
* 그리드가 여러개일 경우 PARMA으로 구분하여 사용
*---------------------------------------------------

  CHECK GRF_DOCKING_CON IS INITIAL.

* Creating Docing container instance
  PERFORM CREATE_CONTAINER.   "각 구간별 Container Split

*--------------------------------
* Create Alv Grid
*--------------------------------
*-- Header Grid..
  PERFORM CREATE_ALV_GRID.

*--------------------------------
* Dislay Grid..
*--------------------------------
*-- Header Grid
  GRF_GRID->SET_GRID( EXPORTING IV_VARI = P_VAR
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
                      CHANGING  CT_DATA = GT_DISP ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
* Set Header Cocntainer
*--------------------------------
  DATA(LRF_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

  DATA(LRF_SPLITTER_HTML) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_CONT
                                                           NO_AUTODEF_PROGID_DYNNR = 'X'
                                                           ROWS    = 1
                                                           COLUMNS = 1 ).
  GRF_HEAD = LRF_SPLITTER_HTML->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

*--------------------------------
* Set Body Cocntainer
*--------------------------------
*-- Set Alv Container..
  GRF_BODY     = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

ENDFORM. " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*& Form create_alv_grid
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
*  ls_toolbtn-btn_add    = 'X'.       "Add Row
*  ls_toolbtn-btn_madd   = 'X'.       "Multi Add Row
*  ls_toolbtn-btn_del    = 'X'.       "Delete Row
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Uploa
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER CHANGING LT_HEADER.

*--------------------------------------------------
* Set Lock Name..
*--------------------------------------------------

*--------------------------------------------------
* Create Object Grid
*--------------------------------------------------
  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY
      IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD           "Header Information
*     iv_scr_wr  =                    "Default 20:10:70
      IT_HEADER  = LT_HEADER
      IV_CELLC   = 'X'
      IV_CELLS   = 'X'.
*      IV_LOCK_NM = CONV #( LV_LOCK_NM ).


ENDFORM.
*---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM SET_HEADER CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

  DATA : LS_HEADER TYPE GRF_GRID->TS_HEADER,
         LC_BUKRS(20) TYPE C,
         LC_KALSK(20) TYPE C.

  DEFINE _L_SET_HEADER.
*    IF &2 IS NOT INITIAL.
    CLEAR ls_header.
    ls_header-key  = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
*    ENDIF.
  END-OF-DEFINITION.

  LC_BUKRS = TEXT-F01.
  LC_KALSK = TEXT-F02.

  DATA: LV_KALSK_TX(6) TYPE C.

  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

  IF P_KALSK = GC_KALSK_DO.
    LV_KALSK_TX = TEXT-T13.
  ELSE.
    LV_KALSK_TX = TEXT-T14.
  ENDIF.

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER: LC_BUKRS      P_BUKRS LV_BUTXT,
                 LC_KALSK      LV_KALSK_TX ''.

ENDFORM. " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Form create_alv_grid_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_0200 .
  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA: LS_TOOLBTN TYPE ZSCN00004,
        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.
*       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  ls_toolbtn-btn_add    = 'X'.       "Add Row
*  ls_toolbtn-btn_madd   = 'X'.       "Multi Add Row
*  ls_toolbtn-btn_del    = 'X'.       "Delete Row
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Uploa
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row

*--------------------------------------------------
* Create Object Grid
*--------------------------------------------------
  CREATE OBJECT GRF_GRID_0200
    EXPORTING
      IV_NAME    = 'ALV_GRID_0200'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_DOCKING_CON_0200
*     IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IRF_HEAD   = GRF_HEAD           "Header Information
*     iv_scr_wr  =                    "Default 20:10:70
*     IT_HEADER  = LT_HEADER
      IV_CELLC   = ''
      IV_CELLS   = 'X'.
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
*&      Form  CREATE_CONTAINER_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

ENDFORM. " CREATE_CONTAINER
