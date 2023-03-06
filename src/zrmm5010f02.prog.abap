*&---------------------------------------------------------------------*
*& Include          ZRMM5010F02
*&---------------------------------------------------------------------*
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
      IV_CELLC   = ''
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

  DATA : LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
*    IF &2 IS NOT INITIAL.
    CLEAR ls_header.
    ls_header-key  = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
*    ENDIF.
  END-OF-DEFINITION.

  CONSTANTS : LC_BUKRS(20) TYPE C VALUE '회사코드 ',
              LC_BUDAT(20) TYPE C VALUE '전기일'.

  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

  READ TABLE S_BUDAT INDEX 1.

  CONCATENATE S_BUDAT-LOW '~' S_BUDAT-HIGH INTO DATA(LV_BUDAT).

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _L_SET_HEADER: LC_BUKRS       P_BUKRS LV_BUTXT,
                 LC_BUDAT      LV_BUDAT ''.

ENDFORM. " TOP_OF_PAGE*&---------------------------------------------------------------------*
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
*      WHEN 'ARTNR'.
*        PERFORM CHECK_CHANGED_ARTNR  USING    IRF_DATA_CHANGED
*                                              LS_LVC_MODI
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
*& Form evt_dtl_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM EVT_DTL_DATA_CHANGED USING IRF_DATA_CHANGED
                                  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).

    READ TABLE GT_DISP_DTL ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LS_LVC_MODI-ROW_ID.

*-----------------------------------------------------------------
* 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
*-----------------------------------------------------------------
    CASE LS_LVC_MODI-FIELDNAME.
*      WHEN 'ARTNR'.
*        PERFORM CHECK_CHANGED_ARTNR  USING    IRF_DATA_CHANGED
*                                              LS_LVC_MODI
*                                     CHANGING <LS_DISP>.
      WHEN OTHERS.
        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
                                              LS_LVC_MODI
                                     CHANGING <LS_DISP>.
    ENDCASE.

  ENDLOOP.

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
      _L_ADD_TOOLBAR : 'MAPPING_CANCEL'  ICON_PREVIOUS_OBJECT  '매핑취소'(U02) '' '' '매핑취소'(U02),
                       'MAPPING_EXCUTE'  ICON_NEXT_OBJECT  '전표맵핑'(U03) '' '' '전표맵핑'(U03),
                       'POSTING_CANCEL'  ICON_SYSTEM_UNDO  '송장취소'(U04) '' '' '송장취소'(U04),
                       'DETAIL_POPUP'  ICON_DOC_HEADER_DETAIL  '거래명세서'(U01) '' '' '거래명세서'(U01),
                       'REISSUE_SEND'  ICON_TRANSPORT  '역발행 전송'(U05) '' '' '역발행 전송'(U05),
                       'REISSUE_CANCEL'  ICON_DELIVERY_NO_CONFIRMATION  '역발행 회수'(U06) '' '' '역발행 회수'(U06),
                       'APPROVAL_LINK'  ''  '결재상신'(U07) '' '' '결재상신'(U07).

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

    WHEN 'MSGYN'.
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

*    WHEN 'ISSUE_ID'.
*      PERFORM DISPLAY_ELECT_INFO USING LS_DISP-INV_SEQ.
    WHEN 'BELNR'.
      CHECK NOT LS_DISP-BELNR IS INITIAL.
      SET PARAMETER ID 'RBN' FIELD LS_DISP-BELNR.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    WHEN 'BELNR_FI'.
      CHECK NOT LS_DISP-BELNR_FI IS INITIAL.
      SET PARAMETER ID 'BLN' FIELD LS_DISP-BELNR_FI.
      SET PARAMETER ID 'BUK' FIELD P_BUKRS.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*    WHEN 'APVIFKEY'.
*      IF LS_DISP-APPSTATUS IS INITIAL.
*        MESSAGE S033 WITH '결재정보'(M07) DISPLAY LIKE 'E'.
*      ELSE.
*        PERFORM POPUP_TO_APVIF USING LS_DISP-APVIFKEY.
*      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_dtl_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_DTL_DOUBLE_CLICK USING IV_ROW
                                 IV_COLUMN.

  READ TABLE GT_DISP_DTL INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID_DTL->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).
    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.
      IF LS_DISP-BSTYP EQ 'L'.
        SET PARAMETER ID 'SAG' FIELD LS_DISP-EBELN.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
      ELSE.
        SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
        CALL TRANSACTION 'ME23N'.
      ENDIF.
    WHEN 'LFBNR'.
      CHECK LS_DISP-LFBNR IS NOT INITIAL.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          I_MBLNR             = LS_DISP-LFBNR
          I_MJAHR             = LS_DISP-LFGJA
        EXCEPTIONS
          ILLEGAL_COMBINATION = 1
          OTHERS              = 2.
    WHEN 'BELNR'.
      SET PARAMETER ID 'RBN' FIELD LS_DISP-BELNR.
      SET PARAMETER ID 'GJR' FIELD LS_DISP-GJAHR.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ENDCASE.


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
*& Form evt_DTL_changed_finished
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM EVT_DTL_CHANGED_FINISHED USING IT_GOOD_CELLS TYPE LVC_T_MODI.

  GRF_GRID_DTL->REFRESH_GRID_DISPLAY( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLIK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM EVT_GRID_BUTTON_CLIK USING IS_COL_ID TYPE LVC_S_COL
                                    IS_ROW_NO TYPE LVC_S_ROID
                                    IV_SENDER.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IS_ROW_NO-ROW_ID.

  CHECK SY-SUBRC EQ 0.

*  CASE IS_COL_ID-FIELDNAME.
*    WHEN OTHERS.
*  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form avl_grid_set_drop_down
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
*&      Form  FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC_FCAT  text
*----------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.
  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-icon       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-no_out     = &9.
    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'APPSTATUS_TX'.
        _L_SET_FCAT: 11   '' ''  '결재상태'            ''  ''  ''  '' ''.
        CLEAR <LS_FCAT>-COL_OPT.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'APVIFKEY'.
        _L_SET_FCAT: 12   '' ''  '전자결재 연동KEY'       ''  ''  ''  '' ''.
        CLEAR <LS_FCAT>-COL_OPT.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'INV_SIGN_TX'.
        _L_SET_FCAT: 13   '' ''  '발행구분'          ''  ''  ''  '' ''.
      WHEN 'TYPE_CODE_TX'.
        _L_SET_FCAT: 14   '' ''  '세금계산서종류'             ''  '' ''  '' ''.
      WHEN 'BELNR'.
        _L_SET_FCAT: 21   '' ''  '송장번호'       ''   ''  ''  '' ''.
      WHEN 'BELNR_FI'.
        _L_SET_FCAT: 22   '' ''  '회계전표번호'           ''  '' ''  '' ''.
      WHEN 'BUDAT'.
        _L_SET_FCAT: 23   '' ''  '전기일'           ''  '' ''  '' ''.
      WHEN 'BLDAT'.
        _L_SET_FCAT: 24   '' ''  '증빙일'           ''  '' ''  '' ''.
      WHEN 'BUPLA'.
        _L_SET_FCAT: 25   '' ''  '사업장'           ''  '' ''  '' ''.
      WHEN 'BUPLA_TX'.
        _L_SET_FCAT: 26   '' ''  '사업장명'           ''  '' ''  '' ''.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 27   '' ''  '공급업체'           ''  '' ''  '' ''.
      WHEN 'LIFNR_TX'.
        _L_SET_FCAT: 28   '' ''  '공급업체명'           ''  '' ''  '' ''.
      WHEN 'STCD2'.
        _L_SET_FCAT: 29   '' ''  '사업자 등록번호'         ''   ''  ''  '' ''.
      WHEN 'DMBTR'.
        _L_SET_FCAT: 31   '' ''  '공급가액'         ''   ''  'WAERS'  '' ''.
      WHEN 'WMWST1'.
        _L_SET_FCAT: 32   '' ''  '부가세'         ''   ''  'WAERS'  '' ''.
      WHEN 'MWSKZ1'.
        _L_SET_FCAT: 33   '' ''  '세금코드'         ''   ''  ''  '' ''.
      WHEN 'RMWWR'.
        _L_SET_FCAT: 34   '' ''  '계약금액'         ''   ''  'WAERS'  '' ''.
      WHEN 'ISSUE_ID'.
        _L_SET_FCAT: 35   '' ''  '승인번호'         ''   ''  ''  '' ''.
      WHEN 'DWPAYC'.
        _L_SET_FCAT: 41   '' ''  '선급금 반제금액'         ''   ''  'WAERS'  '' ''.
      WHEN 'DELAYF'.
        _L_SET_FCAT: 42   '' ''  '지체상금공제금액'         ''   ''  'WAERS'  '' ''.
      WHEN 'ESTPAY'.
        _L_SET_FCAT: 43   '' ''  '실지급예정금액'         ''   ''  'WAERS'  '' ''.
      WHEN 'WAERS'.
        _L_SET_FCAT: 44   '' ''  '통화'         ''   ''  ''  '' ''.
      WHEN 'PAYSTS'.
        _L_SET_FCAT: 51   '' 'X'  '대금지급여부'         ''   ''  ''  '' ''.
      WHEN 'AUGDT'.
        _L_SET_FCAT: 52   '' ''  '반제일'         ''   ''  ''  '' ''.
      WHEN 'PRTCHG'.
        _L_SET_FCAT: 53   '' ''  '인쇄교체비'         ''   ''  'WAERS'  '' ''.
      WHEN 'ACTPAY'.
        _L_SET_FCAT: 54   '' ''  '실비지급 '         ''   ''  'WAERS'  '' ''.
      WHEN 'ZTERM'.
        _L_SET_FCAT: 55   '' ''  '지급조건'         ''   ''  ''  '' ''.
      WHEN 'ZTERM_TX'.
        _L_SET_FCAT: 56   '' ''  '지급조건명'         ''   ''  ''  '' ''.
      WHEN 'FIDOC_TYPE'.
        _L_SET_FCAT: 61   '' ''  '전표구분'         ''   ''  ''  '' ''.
      WHEN 'ERFNAM'.
        _L_SET_FCAT: 62   '' ''  '송장처리자'         ''   ''  ''  '' ''.
      WHEN 'ERFNAM_TX'.
        _L_SET_FCAT: 63   '' ''  '송장처리자명'         ''   ''  ''  '' ''.
      WHEN 'AMEND_CODE'.
        _L_SET_FCAT: 64   '' ''  '수정사유코드'         ''   ''  ''  '' ''.
      WHEN 'ED_STATUS'.
        _L_SET_FCAT: 65   '' ''  '세금계산서상태'         ''   ''  ''  '' ''.
      WHEN 'ED_STATUS_TX'.
        _L_SET_FCAT: 66   '' ''  '세금계산서상태내역'         ''   ''  ''  '' ''.
      WHEN 'NTS_SEND_FLAG'.
        _L_SET_FCAT: 67   '' ''  '국세청전송유무'         ''   ''  ''  '' ''.
      WHEN 'BSART'.
        _L_SET_FCAT: 71   '' ''  '구매오더유형'         ''   ''  ''  '' 'X'.
      WHEN 'BKTXT_FI'.
        _L_SET_FCAT: 72   '' ''  '전표헤더텍스트'         ''   ''  ''  '' ''.
      WHEN 'MSGYN'.
        _L_SET_FCAT: 91   '' 'X'  '메시지'         ''   ''  ''  '' ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM. " FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*& Form alv_DTL_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_DTL_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.
  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-cfieldname = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'BUDAT'.
        _L_SET_FCAT: 1   ''  '' '입고일'  ''  ''  ''  '' ''.
      WHEN 'EBELN'.
        _L_SET_FCAT: 2   '' ''  'PO번호'  ''  ''  ''  '' '' .
      WHEN 'EBELP'.
        _L_SET_FCAT: 3   '' ''  'PO품목'  ''  ''  ''  '' '' .
      WHEN 'LFBNR'.
        _L_SET_FCAT: 4   '' ''  '자재문서'  ''  ''  ''  '' '' .
      WHEN 'LFPOS'.
        _L_SET_FCAT: 5   '' ''  '항목'  ''  ''  ''  '' '' .
      WHEN 'MATNR'.
        _L_SET_FCAT: 11   '' ''  '자재'  ''  ''  ''  '' '' .
      WHEN 'MAKTX'.
        _L_SET_FCAT: 12   '' ''  '자재내역'  ''  ''  ''  '' '' .
      WHEN 'MENGE'.
        _L_SET_FCAT: 13   '' ''  '입고수량'  'R'  '' ''  'MEINS' ''.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN 'MEINS'.
        _L_SET_FCAT: 14   '' ''  '단위'  ''  '' ''  '' ''.
      WHEN 'WRBTR'.
        _L_SET_FCAT: 21   '' ''  '입고금액'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN 'WAERS'.
        _L_SET_FCAT: 22   '' ''  '통화'  ''  '' ''  '' ''.
      WHEN 'NETPR'.
        _L_SET_FCAT: 23   '' ''  '품목단가'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
      WHEN 'MWSKZ'.
        _L_SET_FCAT: 24   '' ''  '세금코드'  ''  '' ''  '' ''.
      WHEN 'WMWST'.
        _L_SET_FCAT: 25   '' ''  '세액'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN 'NETPR_PO'.
        _L_SET_FCAT: 31   '' ''  '구매단가'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
      WHEN 'BELNR'.
        _L_SET_FCAT: 32   '' ''  '송장번호'  ''  '' ''  '' ''.
      WHEN 'MIDAM'.
        _L_SET_FCAT: 33   '' ''  '검수금액'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN 'ACTEX'.
        _L_SET_FCAT: 34   '' ''  '실비금액'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN 'PRTAMT'.
        _L_SET_FCAT: 35   '' ''  '인쇄교체비'  ''  '' ''  '' 'WAERS'.
        CLEAR <LS_FCAT>-NO_SIGN.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT.

  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LV_INDEX    TYPE I.


  LOOP AT GT_DISP INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.

    CLEAR LT_LVC_STYL[].
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
*& Form alv_dtl_set_line_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ALV_DTL_SET_LINE_STYLE USING IT_FCAT.


  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LV_INDEX    TYPE I.

  LOOP AT GT_DISP_DTL INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.

    CLEAR LT_LVC_STYL[].
*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_DTL_FIELD_CELLTAB USING    IT_FCAT
                                  CHANGING LT_LVC_STYL LS_DISP.

*-- Insert Style Talble
    CLEAR LS_DISP-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DISP-CELLS.

*-- Modify Line..
    MODIFY GT_DISP_DTL FROM LS_DISP INDEX LV_INDEX.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_sort
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

*  _l_append_sort: 1 'ERDAT' 'X' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_dtl_field_celltab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FCAT
*&      <-- LT_LVC_STYL
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_DTL_FIELD_CELLTAB USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TS_DISP_DTL.

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
*&      Form  CREATE_CONTAINER_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_200 .

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON_200
    EXPORTING
      REPID     = SY-REPID " 프로그램명 id
      DYNNR     = SY-DYNNR " 화면번호 (Screen) "
      SIDE      = GRF_DOCKING_CON_200->DOCK_AT_TOP "
      EXTENSION = 10000.

ENDFORM. " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*& Form create_alv_grid_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_200 .
  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  ls_toolbtn-btn_exlu   = 'X'.       "Excel Upload
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'YKHTEST2'.  " 그리드별 마스터 Table Name..

*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드(고정Value)
*--------------------------------------------------

  CREATE OBJECT GRF_GRID_DTL
    EXPORTING
      IV_NAME    = 'GRID_DTL'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_DOCKING_CON_200
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN.    "User 기본 Toolbar Button

ENDFORM.
