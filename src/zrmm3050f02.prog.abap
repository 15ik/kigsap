*&---------------------------------------------------------------------*
*& Include          ZRMM3050F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
FORM set_grid.

  IF grf_docking_con IS INITIAL.

* Creating Docking container instance
    PERFORM create_container.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM create_alv_grid.

*--------------------------------
* Dislay Grid..
*--------------------------------
    CASE 'X'.
      WHEN p_ra.  "개요
        grf_grid->set_grid( CHANGING ct_data = gt_disp_ra ).
      WHEN p_rb.  "상세
        grf_grid->set_grid( CHANGING ct_data = gt_disp_rb ).
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    grf_grid->refresh_grid_display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
*&---------------------------------------------------------------------*
FORM create_container.

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT grf_docking_con
    EXPORTING
      repid     = sy-repid    "프로그램명 id
      dynnr     = sy-dynnr    "화면번호(Screen)
      side      = grf_docking_con->dock_at_top
      extension = 10000.

*----------------------------------------------------
* Split Container (1 Row:header 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(lrf_splitter) = NEW cl_gui_splitter_container(
    parent                  = grf_docking_con
    no_autodef_progid_dynnr = 'X'
    rows                    = 2
    columns                 = 1 ).
  lrf_splitter->set_row_mode( mode = cl_gui_splitter_container=>type_movable ).
  lrf_splitter->set_row_height( id = 1 height = 100 ).
  lrf_splitter->set_border( border = space ).

*--------------------------------
* Set Header Container
*--------------------------------
  DATA(lrf_cont) = lrf_splitter->get_container( row = 1 column = 1 ).

  DATA(lrf_splitter_html) = NEW cl_gui_splitter_container(
    parent                  = lrf_cont
    no_autodef_progid_dynnr = 'X'
    rows                    = 1
    columns                 = 1 ).
  grf_head = lrf_splitter_html->get_container( row = 1 column = 1 ).

*--------------------------------
* Set Body Container
*--------------------------------
  grf_body = lrf_splitter->get_container( row = 2 column = 1 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
FORM create_alv_grid.

  DATA: ls_toolbtn TYPE zscn00004,
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row 시 자동으로 입력될 필드 관리
        lt_header  TYPE zcl_cn_alv_grid=>tt_header.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  ls_toolbtn-btn_rec    = 'X'.       "Recovery Row
  ls_toolbtn-btn_exld   = 'X'.       "Excel Download
  ls_toolbtn-btn_exlu   = 'X'.       "Excel Upload
  ls_toolbtn-mlti_lines = gv_mrow.   "Multi Row

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM set_header_info CHANGING lt_header.


  CREATE OBJECT grf_grid
    EXPORTING
      iv_name    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      irf_parent = grf_body
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      is_toolbtn = ls_toolbtn
      irf_head   = grf_head
*     IV_SCR_WR  = '20:10:70'
      iv_cellc   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      it_header  = lt_header.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
FORM set_header_info CHANGING ct_header TYPE grf_grid->tt_header.

  DATA: ls_header TYPE grf_grid->ts_header.

  DEFINE _l_set_header.
    CLEAR ls_header.
    ls_header-key   = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
  END-OF-DEFINITION.

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

  "회사코드 TEXT
  SELECT SINGLE butxt
    FROM t001
   WHERE bukrs = @p_bukrs
    INTO @DATA(lv_butxt).

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _l_set_header: TEXT-f01   p_bukrs   lv_butxt.

* 개요/상세
  CASE 'X'.
    WHEN p_ra.
      _l_set_header: TEXT-f02   TEXT-f03  ''.
    WHEN p_rb.
      _l_set_header: TEXT-f02   TEXT-f04  ''.
  ENDCASE.

* 국내/해외
  CASE p_kalsk.
    WHEN gc_kalsk_do.
      _l_set_header: TEXT-f05   TEXT-f06  ''.
    WHEN gc_kalsk_im.
      _l_set_header: TEXT-f05   TEXT-f07  ''.
  ENDCASE.

*-----------------------------------
* Header 주석
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM evt_grid_toolbar CHANGING ct_toolbar TYPE ttb_button.

  DEFINE _l_add_toolbar.
    ls_add_toolbar-function    = &1.
    ls_add_toolbar-icon        = &2.
    ls_add_toolbar-quickinfo   = &3.
    ls_add_toolbar-butn_type   = &4.
    ls_add_toolbar-disabled    = &5.
    ls_add_toolbar-text        = &6.

    APPEND ls_add_toolbar TO ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:ls_add_toolbar TYPE stb_button.

  _l_add_toolbar: 'BTN_DETAIL'  icon_doc_item_detail  TEXT-u06 '' '' TEXT-u06.  "구매실적 상세조회

  IF p_ra = 'X'.
    _l_add_toolbar: 'BTN_TRANS'   icon_rename        TEXT-u01 '' '' TEXT-u01. "담당자이관
    _l_add_toolbar: 'BTN_CHANGE'  icon_change_text   TEXT-u04 '' '' TEXT-u04, "발주변경
                    'BTN_CHANGE_MASS'   icon_mass_change  TEXT-t05 '' '' TEXT-t05,  "헤더변경
                    'BTN_HISTORY'  icon_history   TEXT-u07 '' '' TEXT-u07. "변경이력
  ENDIF.

  IF p_rb = 'X'.
    _l_add_toolbar: 'BTN_CLOSE_MULTI'   icon_close        TEXT-u02 '' '' TEXT-u02,  "발주종료(Multi)
                    'BTN_CLOSE_SINGLE'  icon_close        TEXT-u03 '' '' TEXT-u03,  "발주종료(Single)
                    'BTN_CHANGE_MASS'   icon_mass_change  TEXT-u05 '' '' TEXT-u05,  "일괄변경
                    'BTN_COMP_DIS'   icon_display_note  TEXT-u08 '' '' TEXT-u08.  "구성품 조회
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_subc_grid_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM evt_subc_grid_toolbar CHANGING ct_toolbar TYPE ttb_button.

  DEFINE _l_add_toolbar.
    ls_add_toolbar-function    = &1.
    ls_add_toolbar-icon        = &2.
    ls_add_toolbar-quickinfo   = &3.
    ls_add_toolbar-butn_type   = &4.
    ls_add_toolbar-disabled    = &5.
    ls_add_toolbar-text        = &6.

    APPEND ls_add_toolbar TO ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:ls_add_toolbar TYPE stb_button.

  IF gv_expand IS INITIAL.
    _l_add_toolbar: 'BTN_EXPAND'   icon_expand        TEXT-u09 '' '' TEXT-u09.  "BOM 펼치기
  ELSE.
    _l_add_toolbar:   'BTN_COLLAPSE'  icon_collapse        TEXT-u10 '' '' TEXT-u10.  "BOM 접기
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_button_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_COL_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM evt_grid_button_click USING is_col_id TYPE lvc_s_col
                                 is_row_no TYPE lvc_s_roid.

  CASE 'X'.
    WHEN p_ra.  "개요
      PERFORM evt_grid_ra_button_click USING is_col_id
                                             is_row_no.

    WHEN p_rb.  "상세
      PERFORM evt_grid_rb_button_click USING is_col_id
                                             is_row_no.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_RA_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM evt_grid_ra_button_click USING is_col_id TYPE lvc_s_col
                                    is_row_no TYPE lvc_s_roid.

*  DATA: lt_apvkey  TYPE TABLE OF zscn00219.
  DATA: lv_result  TYPE bapi_mtype,
        lv_message TYPE bapi_msg.

  READ TABLE gt_disp_ra INTO DATA(ls_disp) INDEX is_row_no-row_id.

  CHECK sy-subrc EQ 0.

  CASE is_col_id-fieldname.
    WHEN 'APV_STAT'.
*      READ TABLE gt_apv WITH KEY wfobject = ls_disp-ebeln
*                                 BINARY SEARCH
*                                 TRANSPORTING NO FIELDS.
*
*      IF sy-subrc = 0.
*        LOOP AT gt_apv INTO DATA(ls_apv) FROM sy-tabix.
*          IF ls_apv-wfobject NE ls_disp-ebeln.
*            EXIT.
*          ENDIF.
*
*          lt_apvkey = VALUE #( BASE lt_apvkey ( apvifkey = ls_apv-apvifkey ) ).
*        ENDLOOP.
*
*        call function 'ZFCN_APV_STATUS_MONI'
*          EXPORTING
*            iv_bukrs   = ls_disp-bukrs
*          IMPORTING
*            ev_result  = lv_result
*            ev_message = lv_message
*          TABLES
*            it_apvkey  = lt_apvkey.
*
*        IF lv_result EQ 'E'.
*          MESSAGE i000 WITH lv_message DISPLAY LIKE 'E'.
*        ENDIF.
*      ENDIF.

    WHEN 'AFILE'.
      DATA(ls_sibflporb) = VALUE sibflporb( ).

      ls_sibflporb = VALUE #( instid = ls_disp-ebeln
                              typeid = gc_gos_typeid
                              catid  = gc_gos_catid ).

      CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
        EXPORTING
          is_object = ls_sibflporb
          ip_mode   = 'D'.         "Create/Display/Edit

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_RB_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM evt_grid_rb_button_click USING is_col_id TYPE lvc_s_col
                                    is_row_no TYPE lvc_s_roid.

  READ TABLE gt_disp_rb INTO DATA(ls_disp) INDEX is_row_no-row_id.

  CHECK sy-subrc EQ 0.

  CASE is_col_id-fieldname.
    WHEN 'TEXT_F04'.
      gv_text_mode = gc_mode_display.

      PERFORM popup_item_text_f04 USING ls_disp-ebeln
                                        ls_disp-ebelp.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_subc_button_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_COL_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM evt_subc_button_click USING is_col_id TYPE lvc_s_col
                                    is_row_no TYPE lvc_s_roid.

  DATA : lv_index TYPE sy-index.

  CLEAR : lv_index.
  READ TABLE gt_subcomp INTO DATA(ls_comp) INDEX is_row_no-row_id.

  CHECK sy-subrc EQ 0.

  CASE is_col_id-fieldname.
    WHEN 'ICON'.


      CASE ls_comp-icon.
        WHEN gc_icon_expa.
          LOOP AT gt_subcomp_p INTO DATA(ls_comp_p) WHERE bauwg = ls_comp-aufwg.
            ADD 1 TO lv_index.
            DATA(lv_row) = lv_index + is_row_no-row_id.

            INSERT ls_comp_p  INTO  gt_subcomp  INDEX lv_row.
          ENDLOOP.

          ls_comp-icon = gc_icon_coll.
          MODIFY gt_subcomp FROM ls_comp INDEX is_row_no-row_id
                                         TRANSPORTING icon .

        WHEN gc_icon_coll.


          DELETE gt_subcomp WHERE bauwg = ls_comp-aufwg.

          ls_comp-icon = gc_icon_expa.
          MODIFY gt_subcomp FROM ls_comp INDEX is_row_no-row_id
                                         TRANSPORTING icon .

      ENDCASE.



    WHEN OTHERS.
  ENDCASE.
  grf_subc_grid->refresh_table_display( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_grid_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_grid_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  CASE 'X'.
    WHEN p_ra.  "개요
      PERFORM alv_grid_ra_fcat_modify CHANGING ct_fcat.

    WHEN p_rb.  "상세
      PERFORM alv_grid_rb_fcat_modify CHANGING ct_fcat.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_RA_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_grid_ra_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-tech       = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'APV_STAT'.  "결재진행
        _l_set_fcat: 01  'X'  ''  TEXT-c01  'C'  ''  ''  ''  '06'.
      WHEN 'WF_STATUS'.  "결재상태
        _l_set_fcat: 02  'X'  ''  TEXT-c02  ''  ''  ''  ''  '15'.

      WHEN 'LIFNR'.  "공급업체
        _l_set_fcat: 03  'X'  ''  TEXT-c03  ''  ''  ''  ''  '10'.
      WHEN 'NAME1'.  "업체명
        _l_set_fcat: 04  'X'  ''  TEXT-c04  ''  ''  ''  ''  '20'.
      WHEN 'EBELN'.  "구매오더
        _l_set_fcat: 05  'X'  ''  TEXT-c05  ''  ''  ''  ''  '12'.
      WHEN 'BSART'.  "문서유형
        _l_set_fcat: 06  'X'  ''  TEXT-c06  ''  ''  ''  ''  '08'.
      WHEN 'BATXT'.  "문서유형내역
        _l_set_fcat: 07  'X'  ''  TEXT-c07  ''  ''  ''  ''  '20'.
      WHEN 'TITLE'.  "발주명
        _l_set_fcat: 08  ''  ''  TEXT-c08  ''  ''  ''  ''  '30'.

      WHEN 'TAG_AMT'.  "공급가액
        _l_set_fcat: 11  ''  ''  TEXT-c09  ''  ''  'WAERS'  ''  '12'.
      WHEN 'TAX_AMT'.  "부가세
        _l_set_fcat: 12  ''  ''  TEXT-c10  ''  ''  'WAERS'  ''  '10'.
      WHEN 'TOT_AMT'.  "계약금액
        _l_set_fcat: 13  ''  ''  TEXT-c11  ''  ''  'WAERS'  ''  '12'.
      WHEN 'WAERS'.  "통화
        _l_set_fcat: 14  ''  ''  TEXT-c12  ''  ''  ''  ''  '05'.
      WHEN 'ZTERM'.  "지급조건
        _l_set_fcat: 15  ''  ''  TEXT-c13  ''  ''  ''  ''  '08'.
      WHEN 'VTEXT'.  "지급조건내역
        _l_set_fcat: 16  ''  ''  TEXT-c14  ''  ''  ''  ''  '15'.
      WHEN 'INCO1'.  "인도조건
        _l_set_fcat: 17  ''  ''  TEXT-c15  ''  ''  ''  ''  '08'.
      WHEN 'BEZEI'.  "인도조건내역
        _l_set_fcat: 18  ''  ''  TEXT-c16  ''  ''  ''  ''  '15'.
      WHEN 'AFILE'.  "첨부파일
        _l_set_fcat: 19  ''  ''  TEXT-c17  'C'  ''  ''  ''  '06'.
      WHEN 'BEDAT'.  "발주일자
        _l_set_fcat: 20  ''  ''  TEXT-c18  ''  ''  ''  ''  '10'.
      WHEN 'TEXT_K00'.  "특약사항
        _l_set_fcat: 21  ''  ''  TEXT-c19  ''  ''  ''  ''  '30'.
      WHEN 'TEXT_K01'.  "지출발의조건
        _l_set_fcat: 22  ''  ''  TEXT-c20  ''  ''  ''  ''  '30'.
      WHEN 'VERKF'.  "파트너담당
        _l_set_fcat: 23  ''  ''  TEXT-c21  ''  ''  ''  ''  '10'.
      WHEN 'IHREZ'.  "접수/거부
        _l_set_fcat: 24  ''  ''  TEXT-c22  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'UNSEZ'.  "참조
        _l_set_fcat: 25  ''  ''  TEXT-c23  ''  ''  ''  ''  '10'.
      WHEN 'KDATB'.  "계약시작일
        _l_set_fcat: 26  ''  ''  TEXT-c24  ''  ''  ''  ''  '10'.
      WHEN 'KDATE'.  "계약종료일
        _l_set_fcat: 27  ''  ''  TEXT-c25  ''  ''  ''  ''  '10'.
      WHEN 'GWLDT'.  "대금지급예정일
        _l_set_fcat: 28  ''  ''  TEXT-c26  ''  ''  ''  ''  '10'.
      WHEN 'SUBMI'.  "계약서명담당
        _l_set_fcat: 29  ''  ''  TEXT-c27  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ABSGR_TEXT'.  "세금계산서발행방식
        _l_set_fcat: 30  ''  ''  TEXT-c28  ''  gv_tech+1(1)  ''  ''  '06'.
      WHEN 'EKGRP'.  "구매그룹
        _l_set_fcat: 31  ''  ''  TEXT-c29  ''  ''  ''  ''  '08'.
      WHEN 'EKNAM'.  "구매그룹명
        _l_set_fcat: 32  ''  ''  TEXT-c68  ''  ''  ''  ''  '20'.
      WHEN 'DPPCT'.  "선급비율
        _l_set_fcat: 33  ''  ''  TEXT-c30  ''  ''  ''  ''  '10'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'INCO_SITE'.  "인코텀스장소
        _l_set_fcat: 34  ''  ''  TEXT-c31  ''  gv_tech+1(1)  ''  ''  '20'.

      WHEN 'INCO2_L'.  "선적항
        _l_set_fcat: 35  ''  ''  TEXT-c61  ''  gv_tech+0(1)  ''  ''  '20'.
      WHEN 'INCO3_L'.  "도착항
        _l_set_fcat: 36  ''  ''  TEXT-c62  ''  gv_tech+0(1)  ''  ''  '20'.

      WHEN 'ZORDER_PERSON'.  "발주담당자
        _l_set_fcat: 41  ''  ''  TEXT-c32  ''  ''  ''  ''  '10'.
      WHEN 'ZORDER_PERSON_NAME'.  "담당자명
        _l_set_fcat: 42  ''  ''  TEXT-c69  ''  ''  ''  ''  '10'.
      WHEN 'ZORDER_DEPARTMENT'.  "발주부서
        _l_set_fcat: 43  ''  ''  TEXT-c33  ''  ''  ''  ''  '08'.
      WHEN 'ZORDER_DEPARTMENT_NAME'.  "부서명
        _l_set_fcat: 44  ''  ''  TEXT-c70  ''  ''  ''  ''  '15'.
      WHEN 'ZEXPEN_PERSON'.  "지출발의담당자
        _l_set_fcat: 45  ''  ''  TEXT-c34  ''  ''  ''  ''  '10'.
      WHEN 'ZEXPEN_PERSON_NAME'.  "담당자명
        _l_set_fcat: 46  ''  ''  TEXT-c69  ''  ''  ''  ''  '10'.
      WHEN 'ZEXPEN_DEPARTMENT'.  "지출발의부서
        _l_set_fcat: 47  ''  ''  TEXT-c35  ''  ''  ''  ''  '08'.
      WHEN 'ZEXPEN_DEPARTMENT_NAME'.  "부서명
        _l_set_fcat: 48  ''  ''  TEXT-c70  ''  ''  ''  ''  '15'.

      WHEN 'ZCOOP_QM'.  "공동검수여부
        _l_set_fcat: 51  ''  ''  TEXT-c36  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZQM_PERSON'.  "검수담당
        _l_set_fcat: 52  ''  ''  TEXT-c37  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZQM_PERSON_NAME'.  "담당자명
        _l_set_fcat: 53  ''  ''  TEXT-c69  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZQM_DEPARTMENT'.  "검수부서
        _l_set_fcat: 54  ''  ''  TEXT-c38  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZQM_DEPARTMENT_NAME'.  "부서명
        _l_set_fcat: 55  ''  ''  TEXT-c70  ''  gv_tech+1(1)  ''  ''  '15'.
      WHEN 'ZREAL_COST'.  "실비정산
        _l_set_fcat: 56  ''  ''  TEXT-c39  'C'  gv_tech+1(1)  ''  ''  '06'.
      WHEN 'ZCONTRACT_DEPOSIT'.  "계약이행보증율
        _l_set_fcat: 57  ''  ''  TEXT-c40  'C'  gv_tech+1(1)  ''  ''  '08'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'ZCONTRACT_GUARN'.  "계약이행보증금
        _l_set_fcat: 58  ''  ''  TEXT-c41  ''  gv_tech+1(1)  ''  ''  '10'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'ZCONT_GUA_TYPE'.  "계약이행 보증유형
        _l_set_fcat: 59  ''  ''  TEXT-c42  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZCON_KDATB'.  "계약이행 보증시작
        _l_set_fcat: 60  ''  ''  TEXT-c43  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZCON_KDATE'.  "계약이행보증 종료
        _l_set_fcat: 61  ''  ''  TEXT-c44  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZPREPAY_DEPOSIT'.  "선급이행보증율
        _l_set_fcat: 62  ''  ''  TEXT-c45  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZPREPAY_GRARN'.  "선급이행보증금
        _l_set_fcat: 63  ''  ''  TEXT-c46  ''  gv_tech+1(1)  ''  ''  '08'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'ZPREP_GUA_TYPE'.  "선급이행 보증유형
        _l_set_fcat: 64  ''  ''  TEXT-c47  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZPAY_KDATB'.  "선급이행 보증시작
        _l_set_fcat: 65  ''  ''  TEXT-c48  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZPAY_KDATE'.  "선급이행보증 종료
        _l_set_fcat: 66  ''  ''  TEXT-c49  ''  gv_tech+1(1)  ''  ''  '10'.
      WHEN 'ZDEFECT_DEPOSIT'.  "하자이행보증율
        _l_set_fcat: 67  ''  ''  TEXT-c50  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZDEFECT_GUARN'.  "하자이행보증금
        _l_set_fcat: 68  ''  ''  TEXT-c51  ''  gv_tech+1(1)  ''  ''  '08'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'ZDEFEC_GUA_TYPE'.  "하자이행 보증유형
        _l_set_fcat: 69  ''  ''  TEXT-c52  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZDEF_BASE_DATE'.  "하자이행 보증기준
        _l_set_fcat: 70  ''  ''  TEXT-c53  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZDEF_KDATB'.  "하자보증기간
        _l_set_fcat: 71  ''  ''  TEXT-c54  ''  gv_tech+1(1)  ''  ''  '08'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'LATE_RATE'.  "지체상금율(1000분의)
        _l_set_fcat: 72  ''  ''  TEXT-c55  ''  gv_tech+1(1)  ''  ''  '15'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'LIFN2_C1'.  "공동수급업체1
        _l_set_fcat: 73  ''  ''  TEXT-c56  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'NAME1_C1'.  "업체명
        _l_set_fcat: 73  ''  ''  TEXT-c04  ''  gv_tech+1(1)  ''  ''  '20'.
      WHEN 'LIFN2_C2'.  "공동수급업체2
        _l_set_fcat: 74  ''  ''  TEXT-c57  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'NAME1_C2'.  "업체명
        _l_set_fcat: 74  ''  ''  TEXT-c04  ''  gv_tech+1(1)  ''  ''  '20'.
      WHEN 'LIFN2_C3'.  "공동수급업체3
        _l_set_fcat: 75  ''  ''  TEXT-c58  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'NAME1_C3'.  "업체명
        _l_set_fcat: 75  ''  ''  TEXT-c04  ''  gv_tech+1(1)  ''  ''  '20'.
      WHEN 'LIFN2_C4'.  "공동수급업체4
        _l_set_fcat: 76  ''  ''  TEXT-c59  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'NAME1_C4'.  "업체명
        _l_set_fcat: 76  ''  ''  TEXT-c04  ''  gv_tech+1(1)  ''  ''  '20'.
      WHEN 'LIFN2_C5'.  "공동수급업체5
        _l_set_fcat: 77  ''  ''  TEXT-c60  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'NAME1_C5'.  "업체명
        _l_set_fcat: 77  ''  ''  TEXT-c04  ''  gv_tech+1(1)  ''  ''  '20'.

      WHEN 'ZEMANAGE2'.  "관리번호
        _l_set_fcat: 81  ''  ''  TEXT-c63  ''  gv_tech+0(1)  ''  ''  '10'.
      WHEN 'ZESHIPTYPE'.  "선적구분
        _l_set_fcat: 82  ''  ''  TEXT-c64  'C'  gv_tech+0(1)  ''  ''  '06'.
      WHEN 'ZEDEDLINE'.  "선적기한
        _l_set_fcat: 83  ''  ''  TEXT-c65  ''  gv_tech+0(1)  ''  ''  '10'.
      WHEN 'HERKL'.  "원산지
        _l_set_fcat: 84  ''  ''  TEXT-c66  'C'  gv_tech+0(1)  ''  ''  '10'.
      WHEN 'ZEINSPECT'.  "수입검사여부
        _l_set_fcat: 85  ''  ''  TEXT-c67  'C'  gv_tech+0(1)  ''  ''  '08'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_RB_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_grid_rb_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-tech       = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'WF_STATUS'.   _l_set_fcat: 01  'X'  ''  TEXT-c02  ''  ''  ''  ''  '15'. "결재상태

      WHEN 'LIFNR'.   _l_set_fcat: 02  'X'  ''  TEXT-d01  ''  ''  ''  ''  '10'."공급업체

      WHEN 'NAME1'. _l_set_fcat: 03  'X'  ''  TEXT-d02  ''  ''  ''  ''  '20'."업체명

      WHEN 'BSART'.  _l_set_fcat: 04  'X'  ''  TEXT-d03  ''  ''  ''  ''  '08'."문서유형

      WHEN 'BATXT'.  _l_set_fcat: 05  'X'  ''  TEXT-d04  ''  ''  ''  ''  '20'."문서유형내역

      WHEN 'EBELN'.  _l_set_fcat: 06  'X'  ''  TEXT-d05  ''  ''  ''  ''  '12'."구매오더

      WHEN 'EBELP'.  _l_set_fcat: 07  'X'  ''  TEXT-d06  ''  ''  ''  ''  '06'."품목

      WHEN 'LOEKZ'.  "삭제
        _l_set_fcat: 08  ''  ''  TEXT-d76  'C'  ''  ''  ''  '05'.         <ls_fcat>-checkbox = 'X'.
      WHEN 'TITLE'.  _l_set_fcat: 09  ''  ''  TEXT-d07  ''  ''  ''  ''  '30'."발주명

      WHEN 'MATNR'.  _l_set_fcat: 10  ''  ''  TEXT-d08  ''  ''  ''  ''  '15'."자재

      WHEN 'TXZ01'.  _l_set_fcat: 11  ''  ''  TEXT-d09  ''  ''  ''  ''  '20'."내역

      WHEN 'BWTAR'.  _l_set_fcat: 12  ''  ''  TEXT-d10  ''  ''  ''  ''  '06'."평가유형

      WHEN 'CHARG'.  _l_set_fcat: 13  ''  ''  TEXT-d75  ''  ''  ''  ''  '10'."배치

      WHEN 'MENGE'.  _l_set_fcat: 14  ''  ''  TEXT-d11  ''  ''  ''  'MEINS'  '12'."수량

      WHEN 'MEINS'.  _l_set_fcat: 15  ''  ''  TEXT-d12  ''  ''  ''  ''  '05'."단위

      WHEN 'NETPR'.  _l_set_fcat: 16  ''  ''  TEXT-d13  ''  ''  'WAERS'  ''  '12'."단가

      WHEN 'WAERS'.  _l_set_fcat: 17  ''  ''  TEXT-d14  ''  ''  ''  ''  '05'."통화

      WHEN 'PEINH'.  _l_set_fcat: 18  ''  ''  TEXT-d15  ''  ''  ''  ''  '06'."가격단위

      WHEN 'BPRME'.  _l_set_fcat: 18  ''  ''  TEXT-f16  ''  ''  ''  ''  '06'."OPU

      WHEN 'BRTWR'.  _l_set_fcat: 19  ''  ''  TEXT-d16  ''  ''  'WAERS'  ''  '12'."품목금액

      WHEN 'ZCPR'.  _l_set_fcat: 20  ''  ''  TEXT-d17  ''  ''  'WAERS'  ''  '10'."품목가감액

      WHEN 'ADJ_NET'.  _l_set_fcat: 21  ''  ''  TEXT-d18  ''  ''  'WAERS'  ''  '10'."조정단가

      WHEN 'ADJ_AMT'.  _l_set_fcat: 22  ''  ''  TEXT-d19  ''  ''  'WAERS'  ''  '12'."조정금액

      WHEN 'CHANGE'.  _l_set_fcat: 23  ''  ''  TEXT-d20  ''  ''  ''  ''  '08'."조정사유코드

      WHEN 'CHANGE_TEXT'. _l_set_fcat: 24  ''  ''  TEXT-d62  ''  ''  ''  ''  '15'."조정사유내역

      WHEN 'REASON'.  _l_set_fcat: 25  ''  ''  TEXT-d21  ''  ''  ''  ''  '20'."조정사유상세

      WHEN 'MWSKZ'.   _l_set_fcat: 26  ''  ''  TEXT-d22  ''  ''  ''  ''  '06'."세금코드

      WHEN 'MWSKZ_TEXT'.  _l_set_fcat: 27  ''  ''  TEXT-d63  ''  ''  ''  ''  '20'."세금코드내역

      WHEN 'TOTAL_AMT'.  _l_set_fcat: 28  ''  ''  TEXT-c76  ''  ''  'WAERS'  ''  '10'."공급가액

      WHEN 'ZTAX'.  _l_set_fcat: 29  ''  ''  TEXT-d23  ''  ''  'WAERS'  ''  '10'."VAT

      WHEN 'CONT_AMT'.  _l_set_fcat: 30  ''  ''  TEXT-d24  ''  ''  'WAERS'  ''  '12'."계약금액

      WHEN 'EINDT'.  _l_set_fcat: 31  ''  ''  TEXT-d25  ''  ''  ''  ''  '10'."납품일

      WHEN 'WERKS'.  _l_set_fcat: 32  ''  ''  TEXT-d26  ''  ''  ''  ''  '06'."플랜트

      WHEN 'WERKS_TEXT'.  _l_set_fcat: 33  ''  ''  TEXT-d64  ''  ''  ''  ''  '20'."플랜트명

      WHEN 'LGORT'.   _l_set_fcat: 34  ''  ''  TEXT-d27  ''  ''  ''  ''  '06'."저장위치

      WHEN 'LGOBE'.  _l_set_fcat: 35  ''  ''  TEXT-d65  ''  ''  ''  ''  '20'."저장위치명

      WHEN 'MATKL'.  _l_set_fcat: 36  ''  ''  TEXT-d28  ''  ''  ''  ''  '08'."자재그룹

      WHEN 'WGBEZ'.  _l_set_fcat: 37  ''  ''  TEXT-d66  ''  ''  ''  ''  '20'. "자재그룹내역

      WHEN 'ZINSPECTION'.  "검사여부
        _l_set_fcat: 38  ''  ''  TEXT-d29  'C'  gv_tech+1(1)  ''  ''  '08'.         <ls_fcat>-checkbox = 'X'.
      WHEN 'ZQM_DEPARTMENT'.  "검사부서
        _l_set_fcat: 39  ''  ''  TEXT-d30  ''  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZQM_DEPARTMENT_NAME'.  "부서명
        _l_set_fcat: 40  ''  ''  TEXT-d67  ''  gv_tech+1(1)  ''  ''  '15'.
      WHEN 'BANFN'.  "구매요청
        _l_set_fcat: 41  ''  ''  TEXT-d31  ''  ''  ''  ''  '12'.
      WHEN 'BNFPO'.  "요청품목
        _l_set_fcat: 42  ''  ''  TEXT-d32  ''  ''  ''  ''  '06'.
      WHEN 'KONNR'.  "계약번호
        _l_set_fcat: 43  ''  ''  TEXT-d33  ''  ''  ''  ''  '12'.
      WHEN 'KTPNR'.  "계약품목
        _l_set_fcat: 44  ''  ''  TEXT-d34  ''  ''  ''  ''  '06'.

      WHEN 'EMLIF'.  "SC벤더
        _l_set_fcat: 45  ''  ''  TEXT-f17  ''  ''  ''  ''  '07'.

      WHEN 'GLACCOUNT'.  "G/L계정
        _l_set_fcat: 46  ''  ''  TEXT-d35  ''  ''  ''  ''  '10'.
      WHEN 'GLACCOUNT_TEXT'.  "G/L계정명
        _l_set_fcat: 47  ''  ''  TEXT-d68  ''  ''  ''  ''  '20'.
      WHEN 'WBSELEMENT'.  "WBS
        _l_set_fcat: 48  ''  ''  TEXT-d36  ''  ''  ''  ''  '10'.
      WHEN 'WBSELEMENT_TEXT'.  "WBS내역
        _l_set_fcat: 49  ''  ''  TEXT-d69  ''  ''  ''  ''  '20'.
      WHEN 'COSTCENTER'.  "코스트센터
        _l_set_fcat: 50  ''  ''  TEXT-d37  ''  ''  ''  ''  '10'.
      WHEN 'COSTCENTER_TEXT'.  "코스트센터명
        _l_set_fcat: 51  ''  ''  TEXT-d70  ''  ''  ''  ''  '20'.
      WHEN 'ORDERID'.  "오더
        _l_set_fcat: 52  ''  ''  TEXT-d38  ''  ''  ''  ''  '10'.
      WHEN 'ORDERID_TEXT'.  "오더내역
        _l_set_fcat: 53  ''  ''  TEXT-d71  ''  ''  ''  ''  '20'.
      WHEN 'SALESORDER'.  "영업오더
        _l_set_fcat: 54  ''  ''  TEXT-d39  ''  ''  ''  ''  '10'.
      WHEN 'SALESORDERITEM'.  "영업오더품목
        _l_set_fcat: 55  ''  ''  TEXT-d40  ''  ''  ''  ''  '10'.
      WHEN 'FIXEDASSET'.  "자산번호
        _l_set_fcat: 56  ''  ''  TEXT-d41  ''  ''  ''  ''  '10'.
      WHEN 'FIXEDASSET_TEXT'.  "자산명
        _l_set_fcat: 57  ''  ''  TEXT-d72  ''  ''  ''  ''  '20'.

      WHEN 'FUNDSCENTER'.  "자금관리센터
        _l_set_fcat: 52  ''  ''  TEXT-f14  ''  ''  ''  ''  '10'.
      WHEN 'COMMITMENTITEM'.  "약정항목
        _l_set_fcat: 53  ''  ''  TEXT-f15  ''  ''  ''  ''  '8'.

      WHEN 'ZPRI'.  "인쇄교체비
        _l_set_fcat: 61  ''  ''  TEXT-d42  ''  gv_tech+1(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUP_UNITPRICE'.  "임가공 가공단가
        _l_set_fcat: 62  ''  ''  TEXT-d43  ''  gv_tech+1(1)  'WAERS'  ''  '10'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUP'.  "임가공 가공비
        _l_set_fcat: 63  ''  ''  TEXT-d44  ''  gv_tech+1(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUM_UNITPRICE'.  "임가공 원료단가
        _l_set_fcat: 64  ''  ''  TEXT-d77  ''  gv_tech+1(1)  'WAERS'  ''  '10'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUM'.  "임가공 원료비
        _l_set_fcat: 65  ''  ''  TEXT-d78  ''  gv_tech+1(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUR_UNITPRICE'.  "임가공 재료품단가
        _l_set_fcat: 66  ''  ''  TEXT-d79  ''  gv_tech+1(1)  'WAERS'  ''  '10'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZSUR'.  "임가공 재료품비
        _l_set_fcat: 67  ''  ''  TEXT-d80  ''  gv_tech+1(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
*      WHEN 'NAVS'.  "불공제여부
*        _L_SET_FCAT: 64  ''  'X'  TEXT-D45  ''  GV_TECH+1(1)  ''  ''  '08'.
      WHEN 'NAVS_TEXT'.  "불공제여부
        _l_set_fcat: 68  ''  ''  TEXT-d45  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'BSTAE'.  "납품서생성
        _l_set_fcat: 69  ''  ''  TEXT-d46  'C'  gv_tech+1(1)  ''  ''  '08'.
      WHEN 'ZCU1_RATE'.  "관세율_ST
        _l_set_fcat: 71  ''  ''  TEXT-d54  ''  gv_tech+0(1)  ''  ''  '08'.              <ls_fcat>-no_zero = 'X'.
        <ls_fcat>-decimals_o = 2.
      WHEN 'ZCU1'.  "관세금액_ST
        _l_set_fcat: 72  ''  ''  TEXT-d55  ''  gv_tech+0(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZFR1_RATE'.  "운임율_ST
        _l_set_fcat: 73  ''  ''  TEXT-d56  ''  gv_tech+0(1)  ''  ''  '08'.              <ls_fcat>-no_zero = 'X'.
        <ls_fcat>-decimals_o = 2.
      WHEN 'ZFR1'.  "운임금액_ST
        _l_set_fcat: 74  ''  ''  TEXT-d57  ''  gv_tech+0(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZIN1_RATE'.  "보험율_ST
        _l_set_fcat: 75  ''  ''  TEXT-d58  ''  gv_tech+0(1)  ''  ''  '08'.              <ls_fcat>-no_zero = 'X'.
        <ls_fcat>-decimals_o = 2.
      WHEN 'ZIN1'.  "보험금액_ST
        _l_set_fcat: 76  ''  ''  TEXT-d59  ''  gv_tech+0(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'ZOT1_RATE'.  "기타부대비율_ST
        _l_set_fcat: 77  ''  ''  TEXT-d60  ''  gv_tech+0(1)  ''  ''  '08'.              <ls_fcat>-no_zero = 'X'.
        <ls_fcat>-decimals_o = 2.
      WHEN 'ZOT1'.  "기타부대비금액_ST
        _l_set_fcat: 78  ''  ''  TEXT-d61  ''  gv_tech+0(1)  'WAERS'  ''  '12'.         <ls_fcat>-no_zero = 'X'.
      WHEN 'RETPO'.  "반품품목
        _l_set_fcat: 81  ''  ''  TEXT-d47  'C'  ''  ''  ''  '08'.         <ls_fcat>-checkbox = 'X'.
      WHEN 'REPOS'.  "무상품목
        _l_set_fcat: 82  ''  ''  TEXT-d48  'C'  ''  ''  ''  '08'.         <ls_fcat>-checkbox = 'X'.
*      WHEN 'PSTYP'.  "품목범주(Internal)
*        _L_SET_FCAT: 83  ''  'X'  TEXT-D49  'C'  ''  ''  ''  '08'.
      WHEN 'EPSTP'.  "품목범주(External)
        _l_set_fcat: 83  ''  ''  TEXT-d49  'C'  ''  ''  ''  '08'.
      WHEN 'PTEXT'.  "품목범주내역
        _l_set_fcat: 83  ''  ''  TEXT-d73  ''  ''  ''  ''  '08'.
      WHEN 'KNTTP'.  "계정지정범주
        _l_set_fcat: 84  ''  ''  TEXT-d50  'C'  ''  ''  ''  '08'.
      WHEN 'ELIKZ'.  "납품완료
        _l_set_fcat: 85  ''  ''  TEXT-d51  'C'  ''  ''  ''  '08'.
        <ls_fcat>-checkbox = 'X'.
      WHEN 'UEBTO'.  "초과납품
        _l_set_fcat: 86  ''  ''  TEXT-d52  ''  ''  ''  'MEINS'  '08'.
        <ls_fcat>-no_zero = 'X'.
      WHEN 'WEBRE'.  "GR기준IV
        _l_set_fcat: 87  ''  ''  TEXT-d53  'C'  ''  ''  ''  '08'.
        <ls_fcat>-checkbox = 'X'.
      WHEN 'TEXT_F04'.  "납품 텍스트
        _l_set_fcat: 91  ''  ''  TEXT-d74  'C'  ''  ''  ''  '10'.

      WHEN 'PRICE_STAMP'.  "가격날인유무
        _l_set_fcat: 92  ''  ''  TEXT-d81  'C'  ''  ''  ''  '09'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'SALES_PRICE'.  "기준 소비자가
        _l_set_fcat: 93  ''  ''  TEXT-d82  'R'  ''  'KONWA'  ''  '09'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'KONWA'.  "통화
        _l_set_fcat: 94  ''  ''  TEXT-d83  'C'  ''  ''  ''  '03'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'CONS_FO'.  "위탁제형
        _l_set_fcat: 95  ''  ''  TEXT-d84  'C'  ''  ''  ''  '08'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'UNIT_SZ'.  "단위용량(숫자)
        _l_set_fcat: 96  ''  ''  TEXT-d85  'C'  ''  ''  ''  '10'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'UNIT_UN'.  "단위용량(단위)
        _l_set_fcat: 97  ''  ''  TEXT-d86  'C'  ''  ''  ''  '10'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'BOX_QTY'.  "입수량(숫자)
        _l_set_fcat: 98  ''  ''  TEXT-d87  'C'  ''  ''  ''  '09'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'BOX_UNIT'.  "입수량(단위)
        _l_set_fcat: 99  ''  ''  TEXT-d88  'C'  ''  ''  ''  '09'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'SET_QTY'.  "세트수량(숫자)
        _l_set_fcat: 100  ''  ''  TEXT-d89  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'SET_UNIT'.  "세트수량(단위)
        _l_set_fcat: 101  ''  ''  TEXT-d90  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'ADD_QTY'.  "추가수량(숫자)
        _l_set_fcat: 102  ''  ''  TEXT-d91  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'ADD_UNIT'.  "추가수량(단위)
        _l_set_fcat: 103  ''  ''  TEXT-d92  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'PRODUCE_NO1'.  "제조번호(부여)
        _l_set_fcat: 104  ''  ''  TEXT-d93  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'PRODUCE_NO2'.  "제조번호(원주)
        _l_set_fcat: 105  ''  ''  TEXT-d94  'C'  ''  ''  ''  '12'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'HISTORY_TR1'.  "식품이력추적번호(부여)
        _l_set_fcat: 106  ''  ''  TEXT-d95  'C'  ''  ''  ''  '16'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'HISTORY_TR2'.  "식품이력추적번호(원주)
        _l_set_fcat: 107  ''  ''  TEXT-d96  'C'  ''  ''  ''  '16'.        <ls_fcat>-parameter1 =  'C'.

      WHEN 'COM_ITEM'.  "구성항목
        _l_set_fcat: 108  ''  ''  TEXT-d97  'C'  ''  ''  ''  '08'.        <ls_fcat>-parameter1 =  'C'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

*    PERFORM adjust_field_by_bukrs CHANGING <ls_fcat>.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_grid_set_line_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM alv_grid_set_line_style USING it_fcat.

  CASE 'X'.
    WHEN p_ra.  "개요
      PERFORM alv_grid_ra_set_line_style USING it_fcat.

    WHEN p_rb.  "상세
      PERFORM alv_grid_rb_set_line_style USING it_fcat.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_RA_SET_LINE_STYLE
*&---------------------------------------------------------------------*
FORM alv_grid_ra_set_line_style USING it_fcat.

  DATA : lt_lvc_styl TYPE lvc_t_styl,
         lv_index    TYPE i.

  LOOP AT gt_disp_ra INTO DATA(ls_disp).
    lv_index = sy-tabix.

    CLEAR lt_lvc_styl[].

*---------------------------
* Set Field Style..
*---------------------------
    PERFORM set_field_celltab_ra USING    it_fcat
                                 CHANGING lt_lvc_styl ls_disp.

*-- Insert Style Talble
    CLEAR ls_disp-cells.
    INSERT LINES OF lt_lvc_styl INTO TABLE ls_disp-cells.

*-- Modify Line..
    MODIFY gt_disp_ra FROM ls_disp INDEX lv_index.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_RA
*&---------------------------------------------------------------------*
FORM set_field_celltab_ra USING it_fcat TYPE lvc_t_fcat
                          CHANGING ct_styl TYPE lvc_t_styl
                                   cs_disp TYPE ty_disp_ra.

  DATA : ls_lvc_styl TYPE lvc_s_styl.

  LOOP AT it_fcat INTO DATA(ls_fieldcat).
    CLEAR ls_lvc_styl.

    ls_lvc_styl-fieldname = ls_fieldcat-fieldname.

* PUSH BUTTON STYLE 구성
    CASE ls_lvc_styl-fieldname.
      WHEN 'APV_STAT'.
        IF cs_disp-apv_stat IS NOT INITIAL.
          ls_lvc_styl-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.

      WHEN 'AFILE'.
        IF cs_disp-afile IS NOT INITIAL.
          ls_lvc_styl-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    INSERT ls_lvc_styl INTO TABLE ct_styl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_RB_SET_LINE_STYLE
*&---------------------------------------------------------------------*
FORM alv_grid_rb_set_line_style USING it_fcat.

  DATA : lt_lvc_styl TYPE lvc_t_styl,
         lv_index    TYPE i.

  LOOP AT gt_disp_rb INTO DATA(ls_disp).
    lv_index = sy-tabix.

    CLEAR lt_lvc_styl[].

*---------------------------
* Set Field Style..
*---------------------------
    PERFORM set_field_celltab_rb USING    it_fcat
                                 CHANGING lt_lvc_styl ls_disp.

*-- Insert Style Talble
    CLEAR ls_disp-cells.
    INSERT LINES OF lt_lvc_styl INTO TABLE ls_disp-cells.

*-- Modify Line..
    MODIFY gt_disp_rb FROM ls_disp INDEX lv_index.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_RB
*&---------------------------------------------------------------------*
FORM set_field_celltab_rb USING it_fcat TYPE lvc_t_fcat
                          CHANGING ct_styl TYPE lvc_t_styl
                                   cs_disp TYPE ty_disp_rb.

  DATA : ls_lvc_styl TYPE lvc_s_styl.

  LOOP AT it_fcat INTO DATA(ls_fieldcat).
    CLEAR ls_lvc_styl.

    ls_lvc_styl-fieldname = ls_fieldcat-fieldname.

* PUSH BUTTON STYLE 구성
    CASE ls_lvc_styl-fieldname.
      WHEN 'TEXT_F04'.
        IF cs_disp-text_f04 IS NOT INITIAL.
          ls_lvc_styl-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    INSERT ls_lvc_styl INTO TABLE ct_styl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_subc_set_line_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM alv_subc_set_line_style USING it_fcat.

  DATA : lt_lvc_styl TYPE lvc_t_styl,
         lt_lvc_scol TYPE lvc_t_scol,
         lv_index    TYPE i.

  LOOP AT gt_subcomp INTO DATA(ls_comp).
    lv_index = sy-tabix.

    CLEAR : lt_lvc_styl[], lt_lvc_scol[].

*---------------------------
* Set Field Style..
*---------------------------
    PERFORM set_field_celltab_subc USING    it_fcat
                                 CHANGING lt_lvc_styl lt_lvc_scol ls_comp.

*-- Insert Style Talble
    CLEAR : ls_comp-cells, ls_comp-cellc.
    INSERT LINES OF lt_lvc_scol INTO TABLE ls_comp-cellc.
    INSERT LINES OF lt_lvc_styl INTO TABLE ls_comp-cells.

*-- Modify Line.
    MODIFY gt_subcomp FROM ls_comp INDEX lv_index.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_field_celltab_SUBC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_STYL
*&      <-- LS_COMP
*&---------------------------------------------------------------------*
FORM set_field_celltab_subc USING it_fcat TYPE lvc_t_fcat
                          CHANGING ct_styl TYPE lvc_t_styl ct_scol TYPE lvc_t_scol
                                   cs_comp TYPE ty_sub_comp.

  DATA : ls_lvc_styl TYPE lvc_s_styl.

  LOOP AT it_fcat INTO DATA(ls_fieldcat).
    CLEAR ls_lvc_styl.

    ls_lvc_styl-fieldname = ls_fieldcat-fieldname.

    IF cs_comp-bauwg IS INITIAL.
      APPEND VALUE #( fname = ls_fieldcat-fieldname
                                   color-col = 3  ) TO ct_scol.
    ENDIF.

* PUSH BUTTON STYLE 구성
    CASE ls_lvc_styl-fieldname.
      WHEN 'ICON'.
        IF cs_comp-icon IS NOT INITIAL.
          ls_lvc_styl-style = cl_gui_alv_grid=>mc_style_button.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    INSERT ls_lvc_styl INTO TABLE ct_styl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_sort
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_SORT
*&---------------------------------------------------------------------*
FORM alv_set_sort CHANGING ct_sort TYPE lvc_t_sort.

  DEFINE _l_append_sort.
    ls_sort-spos      = &1.
    ls_sort-fieldname = &2.
    ls_sort-up        = &3.
    ls_sort-down      = &4.
    ls_sort-group     = &5.
    ls_sort-subtot    = &6.
    ls_sort-comp      = &7.
    ls_sort-expa     = &8.

  APPEND ls_sort TO ct_sort.
  end-OF-DEFINITION.

  DATA:ls_sort TYPE lvc_s_sort.

*  _l_append_sort: 1 'MATNR' '' '' '' 'X' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT_EDITOR
*&---------------------------------------------------------------------*
FORM set_text_editor.

  IF grf_cont_text IS INITIAL.
    CREATE OBJECT grf_cont_text
      EXPORTING
        container_name              = 'CONT_TEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT grf_text_edit
      EXPORTING
        max_number_chars           = '1000'
        style                      = 0
        wordwrap_mode              =
                                     cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = '80' "GC_LINE_LENGTH
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
        parent                     = grf_cont_text
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.

    CALL METHOD grf_text_edit->protect_lines
      EXPORTING
        from_line              = 3
        protect_mode           = 1
        to_line                = 100
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2.

    CALL METHOD grf_text_edit->set_toolbar_mode
      EXPORTING
        toolbar_mode = grf_text_edit->false.

    CALL METHOD grf_text_edit->set_statusbar_mode
      EXPORTING
        statusbar_mode = grf_text_edit->false.
  ENDIF.

* Read Only Mode
  IF gv_text_mode = gc_mode_change.
    CALL METHOD grf_text_edit->set_readonly_mode
      EXPORTING
        readonly_mode = grf_text_edit->false.

  ELSE.
    CALL METHOD grf_text_edit->set_readonly_mode
      EXPORTING
        readonly_mode = grf_text_edit->true.
  ENDIF.

* Display
  CALL METHOD grf_text_edit->set_text_as_r3table
    EXPORTING
      table = gt_text_f04.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_grid_history
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form set_grid_history
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_grid_history .

  DATA: ls_toolbtn TYPE zscn00004.

  ls_toolbtn-btn_exld   = 'X'.       "Excel Download

**  헤더.
*  IF grf_his_h_con IS INITIAL.
*
**--------------------------------
** Create Custom Container..
**--------------------------------
*    CREATE OBJECT grf_his_h_con
*      EXPORTING
*        container_name = 'HIS_HEAD'
*      EXCEPTIONS
*        OTHERS         = 1.
*
**--------------------------------
** Create Alv Grid
**--------------------------------
*    CREATE OBJECT grf_his_h_grid
*      EXPORTING
*        iv_name    = 'HIS_HEAD'   "다수의 그리드일 경우 식별하기 위함..
*        irf_parent = grf_his_h_con
*        is_toolbtn = ls_toolbtn.
*
**--------------------------------
** Dislay Grid..
**--------------------------------
*    grf_his_h_grid->set_grid( CHANGING ct_data = gt_his_head ).
*
*  ELSE.
*    grf_his_h_grid->refresh_grid_display( ).
*  ENDIF.
*
*
** 품목.
*  IF grf_his_i_con IS INITIAL.
*
**--------------------------------
** Create Custom Container..
**--------------------------------
*    CREATE OBJECT grf_his_i_con
*      EXPORTING
*        container_name = 'HIS_ITEM'
*      EXCEPTIONS
*        OTHERS         = 1.
*
**--------------------------------
** Create Alv Grid
**--------------------------------
*    CREATE OBJECT grf_his_i_grid
*      EXPORTING
*        iv_name    = 'HIS_ITEM'   "다수의 그리드일 경우 식별하기 위함..
*        irf_parent = grf_his_i_con
*        is_toolbtn = ls_toolbtn.
*
**--------------------------------
** Dislay Grid..
**--------------------------------
*    grf_his_i_grid->set_grid( CHANGING ct_data = gt_his_item ).
*
*  ELSE.
*    grf_his_i_grid->refresh_grid_display( ).
*  ENDIF.

ENDFORM.
