*&---------------------------------------------------------------------*
*& Include          ZOMM4990F02
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
    GRF_GRID->SET_GRID( "EXPORTING IV_VARI = P_VAR
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
                        CHANGING  CT_DATA = GT_BATCH ).

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
  LRF_SPLITTER->SET_ROW_MODE( MODE = CL_GUI_SPLITTER_CONTAINER=>TYPE_MOVABLE ).
*  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 110 ).
  LRF_SPLITTER->SET_BORDER( BORDER = SPACE ).

*--------------------------------
* Set Header Container
*--------------------------------
*  DATA(LRF_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
*
*  DATA(LRF_SPLITTER_HTML) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_CONT
*                                                           NO_AUTODEF_PROGID_DYNNR = 'X'
*                                                           ROWS    = 1
*                                                           COLUMNS = 1 ).
*  GRF_HEAD = LRF_SPLITTER_HTML->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

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
*  IF P_RD2B = 'X'.
    LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  ENDIF.
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*  LS_TOOLBTN-BTN_INFO   = 'X'.       "Information
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM20030'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

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
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO  CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

**  DATA : LV_TEXT(255) TYPE C.

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

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

*-----------------------------------
* Header Column 지정
*-----------------------------------
**  _L_SET_HEADER : LC_TEXT1  LV_TEXT1_TXT '',
**                  LC_TEXT3  LV_TEXT3_TXT '',
**                  LC_TEXT2  LV_TEXT2_TXT ''.

*_g_set_value:'20:10:70'.  "D
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

*  DATA: LS_ALV_EMSG TYPE TY_ALV_EMSG.

  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).

    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LS_LVC_MODI-ROW_ID.

*-----------------------------------------------------------------
* 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
*-----------------------------------------------------------------
    CASE LS_LVC_MODI-FIELDNAME.
      WHEN OTHERS.
*        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
*                                              LS_LVC_MODI
*                                     CHANGING <LS_DISP>.
    ENDCASE.

  ENDLOOP.

*-
*  IF LS_ALV_EMSG IS NOT INITIAL.
*    PERFORM ADD_ALV_ERROR_MSG USING IRF_DATA_CHANGED LS_ALV_EMSG.
*  ENDIF.

*  CALL METHOD cl_gui_cfw=>set_new_ok_code( 'OK' ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY  CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DATA: LV_CNT TYPE SY-TABIX.

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
  CLEAR LV_CNT.
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATUS'.
        _L_SET_FCAT: 0   ''  '' TEXT-F01   'C' ''  '' '' ''.  "상태
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'BUKRS'.
        _L_SET_FCAT: 1   '' ''  TEXT-F02      'C'  '' '' '' ''.  "회사코드
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ERNAM'.
        _L_SET_FCAT: 2   '' ''  TEXT-F03     'C'  '' '' '' ''.  "작업자
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'ZSEQ'.
        _L_SET_FCAT: 3   '' ''  TEXT-F04     'C'  '' '' '' ''.  "순번
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'MATNR'.
        _L_SET_FCAT: 4   '' ''  TEXT-F05         'C'  '' '' '' ''.  "자재코드
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 5   '' ''  TEXT-F06     ''  '' '' '' ''.  "자재명
        <LS_FCAT>-OUTPUTLEN = 16.
      WHEN 'XCHPF'.
        _L_SET_FCAT: 6   '' ''  TEXT-F07   'C'  '' '' '' ''.  "배치대상
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'CLASS'.
        _L_SET_FCAT: 7   '' ''  TEXT-F08         'C'  '' '' '' ''.  "배치클래스
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'ZCHECK'.
        _L_SET_FCAT: 8   '' ''  TEXT-F09    'C'  ''  '' '' ''.  "구매관리
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'CHARG'.
        _L_SET_FCAT: 9   '' ''  TEXT-F10       'C'  '' '' '' ''.  "배치번호
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'EX_CHARG'.
        _L_SET_FCAT: 10   '' ''  TEXT-F11    'C'  '' '' '' ''.  "외부배치번호
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 11   '' ''  TEXT-F12       'C'  ''  '' '' ''.  "공급업체
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'HSDAT'.
        _L_SET_FCAT: 12   '' ''  TEXT-F13       'C'  '' '' '' ''.  "제조일자
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'VFDAT'.
        _L_SET_FCAT: 13   '' ''  TEXT-F14       'C'  '' '' '' ''.  "사용기한
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'LICHA'.
        _L_SET_FCAT: 14   '' ''  TEXT-F15       'C'  '' '' '' ''.  "제조 LOT
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'ZMM_MAKER'.
        _L_SET_FCAT: 15   '' ''  TEXT-F16       'C'  '' '' '' ''.  "제조처
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZCOMP_RATIO'.
        _L_SET_FCAT: 16   '' ''  TEXT-F17       'C'  '' '' '' ''.  "역가계산비율
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZCOMP_MOIST'.
        _L_SET_FCAT: 17   '' ''  TEXT-F18       'C'  '' '' '' ''.  "수분비율
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZCOMP_NET'.
        _L_SET_FCAT: 18   '' ''  TEXT-F19       'C'  '' '' '' ''.  "함량
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZCOMP_DRY'.
        _L_SET_FCAT: 19   '' ''  TEXT-F20       'C'  '' '' '' ''.  "건조중량
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZLOTNO2'.
        _L_SET_FCAT: 20   '' ''  TEXT-F21       'C'  '' '' '' ''.  "수탁 LOTNO
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'ZMESSAGE'.
        _L_SET_FCAT: 21   '' ''  TEXT-F22       ''  '' '' '' ''.  "메세지
        <LS_FCAT>-OUTPUTLEN = 100.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR  CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

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
**  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
**    RETURN.
**  ENDIF.

  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
  IF P_RD2A = 'X'.
*    _L_ADD_TOOLBAR : 'BTN_ON_EXCEL_DOWNLOAD'   ICON_XLS  TEXT-T14 '' '' TEXT-T14.  "다운로드
    _L_ADD_TOOLBAR : 'BTN_ON_MIGRATION'   ICON_CREATE  TEXT-T11 '' '' TEXT-T11.  "Migration
  ELSE.
    _L_ADD_TOOLBAR : 'BTN_ON_MIGRATION'   ICON_CREATE  TEXT-T11 '' 'X' TEXT-T11.  "Migration
  ENDIF.

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

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docing container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_0200.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( "EXPORTING IV_VARI = P_VAR
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
                        CHANGING  CT_DATA = GT_DISP ).

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
FORM CREATE_ALV_GRID_0200 .
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
*  IF P_RD2B = 'X'.
    LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  ENDIF.
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*  LS_TOOLBTN-BTN_INFO   = 'X'.       "Information
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM20030'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

*--------------------------------------------------
* Set Infomationv 버튼
*--------------------------------------------------
*  LT_INFO = VALUE #( ( FIELDNAME = 'STLAL' ) ( FIELDNAME = 'INFNR' ) ( FIELDNAME = 'MSG' ) ).

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID_0200'   "다수의 그리드일 경우 식별하기 위함.
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
*& Form ALV_GRID_0200_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_0200_FCAT_MODIFY  CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DATA: LV_CNT TYPE SY-TABIX.

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
  CLEAR LV_CNT.
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATUS'.
        _L_SET_FCAT: 0   ''  ''  TEXT-F01   'C' ''  '' '' ''.  "상태
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'BUKRS'.
        _L_SET_FCAT: 1   '' ''  TEXT-F02      'C'  '' '' '' ''.  "회사코드
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'WERKS'.
        _L_SET_FCAT: 2   '' ''  TEXT-F23     'C'  '' '' '' ''.  "플랜트
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'NAME1_W'.
        _L_SET_FCAT: 3   '' ''  TEXT-F24     ''  '' '' '' ''.  "플랜트명
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'LGORT'.
        IF P_RD1C = 'X'.  "사급 재고 HIDDEN
          _L_SET_FCAT: 4   '' ''  TEXT-F25         'C'  '' '' '' 'X'.  "저장위치
        ELSE.
          _L_SET_FCAT: 4   '' ''  TEXT-F25         'C'  '' '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'LGOBE'.
        IF P_RD1C = 'X'.  "사급 재고 HIDDEN
          _L_SET_FCAT: 5   '' ''  TEXT-F26     ''  '' '' '' 'X'.  "저장위치명
        ELSE.
          _L_SET_FCAT: 5   '' ''  TEXT-F26     ''  '' '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 16.
      WHEN 'LIFNR'.
        IF P_RD1B = 'X' OR P_RD1E = 'X'.    " 창고 / 고객 HIDDEN
          _L_SET_FCAT: 6   '' ''  TEXT-F12   'C'  '' '' '' 'X'.  "공급업체
        ELSE.
          _L_SET_FCAT: 6   '' ''  TEXT-F12   'C'  '' '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'NAME1_L'.
        IF P_RD1B = 'X' OR P_RD1E = 'X'.    " 창고 / 고객 HIDDEN
          _L_SET_FCAT: 7   '' ''  TEXT-F27         ''  '' '' '' 'X'.  "공급업체명
        ELSE.
          _L_SET_FCAT: 7   '' ''  TEXT-F27         ''  '' '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'KUNNR'.
        IF P_RD1B = 'X' OR P_RD1C = 'X' OR P_RD1D = 'X'.    " 창고 / 사급 / 위탁 HIDDEN
          _L_SET_FCAT: 8   '' ''  TEXT-F28    'C'  ''  '' '' 'X'.  "고객
        ELSE.
          _L_SET_FCAT: 8   '' ''  TEXT-F28    'C'  ''  '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'NAME1_K'.
        IF P_RD1B = 'X' OR P_RD1C = 'X' OR P_RD1D = 'X'.    " 창고 / 사급 / 위탁 HIDDEN
          _L_SET_FCAT: 9   '' ''  TEXT-F29       ''  '' '' '' 'X'.  "고객명
        ELSE.
          _L_SET_FCAT: 9   '' ''  TEXT-F29       ''  '' '' '' ''.
        ENDIF.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'ZSEQ'.
        _L_SET_FCAT: 10   '' ''  TEXT-F04    'C'  '' '' '' ''.  "순번
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'ERNAM'.
        _L_SET_FCAT: 11   '' ''  TEXT-F30       'C'  ''  '' '' ''.  "작성자
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MATNR'.
        _L_SET_FCAT: 12   '' ''  TEXT-F05       'C'  '' '' '' ''.  "자재코드
        <LS_FCAT>-HOTSPOT = 'X'.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 13   '' ''  TEXT-F06       ''  '' '' '' ''.  "자재명
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'BESKZ'.
        _L_SET_FCAT: 14   '' ''  TEXT-F31       'C'  '' '' '' ''.  "조달유형
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'SOBSL'.
        _L_SET_FCAT: 15   '' ''  TEXT-F32       'C'  '' '' '' ''.  "특별조달
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'MMSTA'.
        _L_SET_FCAT: 16   '' ''  TEXT-F33       'C'  '' '' '' ''.  "자재상태
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'BKLAS'.
        _L_SET_FCAT: 17   '' ''  TEXT-F34       'C'  '' '' '' ''.  "평가클래스
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'XCHPF'.
        _L_SET_FCAT: 18   '' ''  TEXT-F35       'C'  '' '' '' ''.  "배치여부
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'CHARG'.
        _L_SET_FCAT: 19   '' ''  TEXT-F36       'C'  '' '' '' ''.  "배치
      WHEN 'BWTTY'.
        _L_SET_FCAT: 20   '' ''  TEXT-F37       'C'  '' '' '' ''.  "평가유형여부
      WHEN 'BWTAR'.
        _L_SET_FCAT: 21   '' ''  TEXT-F38       'C'  '' '' '' ''.  "평가유형
      WHEN 'VPRSV'.
        _L_SET_FCAT: 22   '' ''  TEXT-F39       'C'  '' '' '' ''.  "가격지시자
      WHEN 'BWART'.
        _L_SET_FCAT: 23   '' ''  TEXT-F40       'C'  '' '' '' ''.  "이동유형
      WHEN 'ERFMG'.
        _L_SET_FCAT: 24   '' ''  TEXT-F41       ''  '' '' 'ZMEINS' ''.  "입력수량
      WHEN 'ZMEINS'.
        _L_SET_FCAT: 25   '' ''  TEXT-F42       'C'  '' '' '' ''.  "입력단위
      WHEN 'MEINS'.
        _L_SET_FCAT: 26   '' ''  TEXT-F43       'C'  '' '' '' ''.  "기본단위
      WHEN 'INSMK'.
        _L_SET_FCAT: 27   '' ''  TEXT-F44       'C'  '' '' '' ''.  "재고유형
      WHEN 'VBELN'.
        IF P_RD1B = 'X' OR P_RD1C = 'X' OR P_RD1D = 'X'.    " 창고 / 사급 / 위탁 HIDDEN
          _L_SET_FCAT: 28   '' ''  TEXT-F45       'C'  '' '' '' 'X'.  "판매오더
        ELSE.
          _L_SET_FCAT: 28   '' ''  TEXT-F45       'C'  '' '' '' ''.
        ENDIF.
      WHEN 'POSNR'.
        IF P_RD1B = 'X' OR P_RD1C = 'X' OR P_RD1D = 'X'.    " 창고 / 사급 / 위탁 HIDDEN
          _L_SET_FCAT: 29   '' ''  TEXT-F46       'C'  '' '' '' 'X'.  "항번
        ELSE.
          _L_SET_FCAT: 29   '' ''  TEXT-F46       'C'  '' '' '' ''.
        ENDIF.
      WHEN 'EXBWR'.
        _L_SET_FCAT: 30   '' ''  TEXT-F47       ''  '' '' '' ''.  "평가금액
      WHEN 'WAERS'.
        _L_SET_FCAT: 31   '' ''  TEXT-F48       'C'  '' '' '' ''.  "통화
      WHEN 'MJAHR'.
        _L_SET_FCAT: 32   '' ''  TEXT-F49       'C'  '' '' '' ''.  "연도
      WHEN 'MBLNR'.
        _L_SET_FCAT: 33   '' ''  TEXT-M62       'C'  '' '' '' ''.  "자재문서
      WHEN 'ZEILE'.
        _L_SET_FCAT: 34   '' ''  TEXT-F46       'C'  '' '' '' ''.
      WHEN 'MJAHR_R'.
        _L_SET_FCAT: 35   '' ''  TEXT-F49       'C'  '' '' '' ''.
      WHEN 'MBLNR_R'.
        _L_SET_FCAT: 36   '' ''  TEXT-F50       'C'  '' '' '' ''.  "취소자재문서
      WHEN 'ZEILE_R'.
        _L_SET_FCAT: 37   '' ''  TEXT-F46       'C'  '' '' '' ''.
      WHEN 'ZSTATUS'.
        _L_SET_FCAT: 38   '' ''  TEXT-F01       'C'  '' '' '' ''.  "상태
      WHEN 'ZMESSAGE'.
        _L_SET_FCAT: 39   '' ''  TEXT-F22       ''  '' '' '' ''.  "메세지
        <LS_FCAT>-OUTPUTLEN = 100.
      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR_200  CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

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
**  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
**    RETURN.
**  ENDIF.

  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
  IF P_RD2A = 'X'.
*    _L_ADD_TOOLBAR : 'BTN_ON_EXCEL_DOWN_STO'   ICON_XLS  TEXT-T14 '' '' TEXT-T14.  "다운로드
    _L_ADD_TOOLBAR : 'BTN_ON_MIGRATION'   ICON_CREATE  TEXT-T11 '' '' TEXT-T11.  "Migration
  ELSE.
    _L_ADD_TOOLBAR : 'BTN_ON_MIGRATION'   ICON_CREATE  TEXT-T11 '' 'X' TEXT-T11.  "Migration
  ENDIF.

  IF P_RD2B = 'X'.
    _L_ADD_TOOLBAR : 'BTN_ON_CANCEL'   ICON_CANCEL  TEXT-T12 '' '' TEXT-T12.  "취소
  ELSE.
    _L_ADD_TOOLBAR : 'BTN_ON_CANCEL'   ICON_CANCEL  TEXT-T12 '' 'X' TEXT-T12.  "취소
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&      --> E_COLUMN_ID
*&---------------------------------------------------------------------*
FORM EVT_GRID_HOTSPOT_CLICK  USING IV_ROW_ID TYPE INT4
                                         IV_COLUMN.

*  DATA: LV_MBLNR TYPE  MBLNR,
*        LV_GJAHR TYPE  GJAHR.

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

    WHEN OTHERS.
  ENDCASE.
ENDFORM.
