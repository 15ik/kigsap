*&---------------------------------------------------------------------*
*& Include          ZOMM3030F02
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
    grf_grid->set_grid( CHANGING  ct_data = gt_disp ).

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
  DATA(lrf_splitter) = NEW cl_gui_splitter_container( parent  = grf_docking_con
                                                      no_autodef_progid_dynnr = 'X'
                                                      rows    = 2
                                                      columns = 1 ).
  lrf_splitter->set_row_mode( mode = cl_gui_splitter_container=>type_movable ).
  lrf_splitter->set_row_height( id = 1 height = 100 ).
  lrf_splitter->set_border( border = space ).

*--------------------------------
* Set Header Container
*--------------------------------
  DATA(lrf_cont) = lrf_splitter->get_container( row = 1 column = 1 ).

  DATA(lrf_splitter_html) = NEW cl_gui_splitter_container( parent  = lrf_cont
                                                           no_autodef_progid_dynnr = 'X'
                                                           rows    = 1
                                                           columns = 1 ).
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

  "구매조직 TEXT
  SELECT SINGLE ekotx
    FROM t024e
   WHERE ekorg = @p_ekorg
    INTO @DATA(lv_ekotx).


*-----------------------------------
* Header Column 지정
*-----------------------------------
  _l_set_header: TEXT-f01   p_bukrs   lv_butxt,
                 TEXT-f02   p_ekorg   lv_ekotx,
                 TEXT-f03   p_persn   gv_pernm,
                 TEXT-f04   p_depat   gv_depnm.

*-----------------------------------
* Header 주석
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_grid_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DATA: lv_tech.

  "품목 상세보기
  IF p_item IS INITIAL.
    lv_tech = 'X'.
  ENDIF.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-tech       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'APV_STAT'.  "결재진행
        _l_set_fcat: 01  'X'  ''  TEXT-c01  'C'  ''  ''  ''  '06'.
      WHEN 'WF_STATUS'. "결재상태
        _l_set_fcat: 10  'X'  ''  TEXT-c02  ''  ''  ''  ''  '08'.
      WHEN 'BANFN'. "구매요청번호
        _l_set_fcat: 11  'X'  ''  TEXT-c03  ''  ''  ''  ''  '12'.
      WHEN 'PR_LOCK'. "PR잠김
        _l_set_fcat: 12  ''   ''  TEXT-c04  'C'  ''  ''  ''  '06'.
      WHEN 'BSART'. "문서유형
        _l_set_fcat: 13  ''   ''  TEXT-c05  ''  ''  ''  ''  '06'.
      WHEN 'BATXT'. "문서유형내역
        _l_set_fcat: 14  ''   ''  TEXT-c06  ''  ''  ''  ''  '20'.
      WHEN 'TITLE'. "구매요청명
        _l_set_fcat: 15  ''   ''  TEXT-c07  ''  ''  ''  ''  '30'.
      WHEN 'BACNT'. "품목수
        _l_set_fcat: 16  ''   ''  TEXT-c08  ''  ''  ''  ''  '05'.
        <ls_fcat>-emphasize = gc_emphsz_c300.

      WHEN 'BNFPO'. "품목
        _l_set_fcat: 21  ''   lv_tech  TEXT-c09  ''  ''  ''  ''  '06'.
      WHEN 'PSTYP_IND'. "임가공
        _l_set_fcat: 22  ''   lv_tech  TEXT-c10  ''  ''  ''  ''  '05'.
        <ls_fcat>-checkbox = 'X'.
      WHEN 'MATNR'. "자재
        _l_set_fcat: 23  ''   lv_tech  TEXT-c11  ''  ''  ''  ''  '15'.
      WHEN 'TXZ01'. "자재내역
        _l_set_fcat: 24  ''   lv_tech  TEXT-c12  ''  ''  ''  ''  '40'.
      WHEN 'MENGE'. "요청수량
        _l_set_fcat: 25  ''   lv_tech  TEXT-c13  ''  ''  ''  'MEINS'  '15'.
      WHEN 'MEINS'. "단위
        _l_set_fcat: 26  ''   lv_tech  TEXT-c14  ''  ''  ''  ''  '05'.
      WHEN 'BADAT'. "요청일
        _l_set_fcat: 27  ''   lv_tech  TEXT-c15  ''  ''  ''  ''  '10'.
      WHEN 'LFDAT'. "납품일
        _l_set_fcat: 28  ''   lv_tech  TEXT-c16  ''  ''  ''  ''  '10'.
      WHEN 'WERKS'. "플랜트
        _l_set_fcat: 29  ''   lv_tech  TEXT-c17  ''  ''  ''  ''  '08'.
      WHEN 'PLANTNAME'. "플랜트명
        _l_set_fcat: 30  ''   lv_tech  TEXT-c18  ''  ''  ''  ''  '20'.

      WHEN 'BAPRE'. "정가
        _l_set_fcat: 51  ''   ''  TEXT-c19  ''  ''  'WAERS'  ''  '15'.
      WHEN 'WAERS'. "통화
        _l_set_fcat: 52  ''   ''  TEXT-c20  ''  ''  ''  ''  '05'.
      WHEN 'AFILE'. "첨부파일
        _l_set_fcat: 53  ''   ''  TEXT-c21   'C'  ''  ''  ''  '06'.
        <ls_fcat>-emphasize = gc_emphsz_c300.
      WHEN 'ZURGENT_REASON'.  "긴급사유
        _l_set_fcat: 54  ''   ''  TEXT-c22  ''  ''  ''  ''  '30'.
      WHEN 'ZPRE_INPUT_REASON'. "선투입사유
        _l_set_fcat: 55  ''   ''  TEXT-c23  ''  ''  ''  ''  '30'.

      WHEN 'APVIFKEY'.  "결재번호
        _l_set_fcat: 90  ''   ''  TEXT-c24   ''  ''  ''  ''  '20'.
        <ls_fcat>-no_out = 'X'.
      WHEN 'APVSTATUS'. "결재상태
        _l_set_fcat: 91  ''   ''  TEXT-c25   ''  ''  ''  ''  '10'.
        <ls_fcat>-no_out = 'X'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
*&---------------------------------------------------------------------*
FORM alv_grid_set_line_style USING it_fcat.

  DATA : lt_lvc_styl TYPE lvc_t_styl,
         lv_index    TYPE i.

  LOOP AT gt_disp INTO DATA(ls_disp).
    lv_index = sy-tabix.

    CLEAR lt_lvc_styl[].

*---------------------------
* Set Field Style..
*---------------------------
    PERFORM set_field_celltab USING    it_fcat
                              CHANGING lt_lvc_styl ls_disp.

*-- Insert Style Talble
    CLEAR ls_disp-cells.
    INSERT LINES OF lt_lvc_styl INTO TABLE ls_disp-cells.

*-- Modify Line..
    MODIFY gt_disp FROM ls_disp INDEX lv_index.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB
*&---------------------------------------------------------------------*
FORM set_field_celltab USING    it_fcat TYPE lvc_t_fcat
                       CHANGING ct_styl TYPE lvc_t_styl
                                cs_disp TYPE ty_disp.

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
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM evt_grid_double_click USING iv_row
                                 iv_column.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX iv_row.

  CASE iv_column.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      grf_grid->show_msgtb( it_msgtb = ls_disp-msgtb ).

    WHEN 'BANFN'.
      CHECK ls_disp-banfn IS NOT INITIAL.

      SET PARAMETER ID 'BAN' FIELD ls_disp-banfn.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM evt_grid_button_click USING is_col_id TYPE lvc_s_col
                                 is_row_no TYPE lvc_s_roid.

  DATA: lt_apvkey  TYPE TABLE OF zscn00219,
        lv_result  TYPE bapi_mtype,
        lv_message TYPE bapi_msg.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX is_row_no-row_id.

  CHECK sy-subrc EQ 0.

  CASE is_col_id-fieldname.
    WHEN 'APV_STAT'.
      READ TABLE gt_apv WITH KEY wfobject = ls_disp-banfn
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        LOOP AT gt_apv INTO DATA(ls_apv) FROM sy-tabix.
          IF ls_apv-wfobject NE ls_disp-banfn.
            EXIT.
          ENDIF.

          lt_apvkey = VALUE #( BASE lt_apvkey ( apvifkey = ls_apv-apvifkey ) ).
        ENDLOOP.

        CALL FUNCTION 'ZFCN_APV_STATUS_MONI'
          EXPORTING
            iv_bukrs   = ls_disp-bukrs
          IMPORTING
            ev_result  = lv_result
            ev_message = lv_message
          TABLES
            it_apvkey  = lt_apvkey.

        IF lv_result EQ 'E'.
          MESSAGE i000 WITH lv_message DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

    WHEN 'AFILE'.
      DATA(ls_sibflporb) = VALUE sibflporb( ).

      ls_sibflporb = VALUE #( instid = ls_disp-banfn
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
*& Form EVT_GRID_TOOLBAR
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

  _l_add_toolbar: 'BTN_DRAFT'     icon_workflow_inbox TEXT-u01 '' '' TEXT-u01,
                  'BTN_WITHDRAW'  icon_system_undo    TEXT-u02 '' '' TEXT-u02,
                  'BTN_CANCEL'    icon_cancel         TEXT-u03 '' '' TEXT-u03.

*  테스트를 위한 버튼 확장 셋팅.
  zcl_mm_common=>common_config(
  EXPORTING  is_common =  VALUE #( m = 'A1' d = 'A1010' s = 'AB200' )
                      it_where = VALUE #( ( field = 1 value = sy-cprog )
                                                     ( field = 2 value = 'Y' ) )
   IMPORTING et_outtab = DATA(lt_config) ).
  IF lt_config[] IS NOT INITIAL.
    _l_add_toolbar:  'BTN_SUBMIT'  '' '상신TEST' '' '' '상신TEST',
                    'BTN_ALLOW'   '' '승인TEST' '' '' '승인TEST',
                    'BTN_REJECT'  '' '반려TEST' '' '' '반려TEST'.
  ENDIF.

  FREE lt_config.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_APPR_DRAFT
*&---------------------------------------------------------------------*
FORM btn_appr_draft.

  CLEAR: gt_rows, gt_disp_apv.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  LOOP AT gt_rows INTO DATA(ls_rows).
    READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

    IF sy-subrc = 0.
      APPEND ls_disp TO gt_disp_apv.
    ENDIF.
  ENDLOOP.

  CHECK gt_disp_apv IS NOT INITIAL.

  PERFORM submit_draft.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_APPR_WITHDRAW
*&---------------------------------------------------------------------*
FORM btn_appr_withdraw.

  DATA: lv_apvifkey   TYPE zapvifkey,
        lv_fiid       TYPE zapvfiid,
        lv_approverid TYPE zapproverid.

  DATA: lv_result  TYPE bapi_mtype,
        lv_message TYPE bapi_msg.

  CLEAR: gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF lines( gt_rows ) > 1.
    MESSAGE i007 DISPLAY LIKE 'E'.  "1 개의 라인을 선택하십시오.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

  IF sy-subrc EQ 0.
    "결재 상태 체크
    IF ls_disp-frgkz NE 'D'.
      MESSAGE i000 WITH TEXT-m05 DISPLAY LIKE 'E'.  "결재중 상태만 회수 가능합니다.
      EXIT.
    ENDIF.

    READ TABLE gt_apv WITH KEY wfobject = ls_disp-banfn
                               BINARY SEARCH
                               INTO DATA(ls_apv).

    lv_apvifkey   = ls_apv-apvifkey.
    lv_fiid       = ls_apv-fiid.
    lv_approverid = sy-uname.

  ENDIF.

*-----------------------------
* 전자결재 회수
* 회수 후 전자결재 후속처리 Func.에서 결과 저장 및 구매오더 릴리즈 자동 수행
*-----------------------------
  CALL FUNCTION 'ZFCN_APV_WITHDRAW'
    EXPORTING
      iv_bukrs      = ls_disp-bukrs
      iv_apvifkey   = lv_apvifkey
      iv_fiid       = lv_fiid
      iv_approverid = lv_approverid
    IMPORTING
      ev_result     = lv_result
      ev_message    = lv_message.

  IF lv_result EQ 'E'.
    MESSAGE i000 WITH lv_message DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM refresh_data.

  grf_grid->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_CANCEL
*&---------------------------------------------------------------------*
FORM btn_cancel.

  DATA: ls_header   LIKE ZSCN_IF_HEADER,
        lt_return   TYPE TABLE OF ZSMM_PR_CANCEL_RESULT,
        ls_message  LIKE ZSMM_PR_CANCEL_RESULT.

  CLEAR: gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF lines( gt_rows ) > 1.
    MESSAGE i007 DISPLAY LIKE 'E'.  "1 개의 라인을 선택하십시오.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

  IF sy-subrc EQ 0.
    "결재 상태 체크
    CASE ls_disp-frgkz.
      WHEN 'X' OR 'W' OR 'J'.
        SKIP.
      WHEN OTHERS.
        MESSAGE i000 WITH TEXT-m04 DISPLAY LIKE 'E'.
        EXIT.
    ENDCASE.
  ENDIF.

*-----------------------------
* 구매요청 취소 처리
*-----------------------------
  CALL FUNCTION 'ZFIMM_PR_CANCEL'
    EXPORTING
      iv_banfn        = ls_disp-banfn
      iv_delete       = 'X'
    IMPORTING
      es_header       = ls_header
    TABLES
      et_return       = lt_return.

  CHECK lt_return[] IS NOT INITIAL.

  LOOP AT lt_return INTO DATA(ls_return) WHERE type IS NOT INITIAL.
    ls_message-type    = ls_return-type.
    ls_message-message = ls_return-message.
  ENDLOOP.

  IF ls_message-type EQ 'S'.
    MESSAGE i000 WITH ls_message-message DISPLAY LIKE 'S'.
  ELSE.
    MESSAGE i000 WITH ls_message-message DISPLAY LIKE 'E'.
  ENDIF.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM refresh_data.

  grf_grid->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_TEST
*&---------------------------------------------------------------------*
FORM btn_test USING iv_ucomm.

  DATA: lv_run_type TYPE ze_run_type,
        lv_rel_code TYPE frgco.

  CLEAR: gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF lines( gt_rows ) > 1.
    MESSAGE i007 DISPLAY LIKE 'E'.  "1 개의 라인을 선택하십시오.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
  CLEAR: gt_apv_pr.

  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

  IF sy-subrc = 0.
    "결재 대상 PR 번호
    gt_apv_pr = VALUE #( BASE gt_apv_pr ( banfn  = ls_disp-banfn ) ).
  ENDIF.

*-----------------------------
* 구매오더 릴리즈
*-----------------------------
*FRGKE 'X': '결재대기'
*      'D': '상신중'
*      'P': '승인완료'
*      'J': '반려'

  CASE iv_ucomm.
    WHEN 'BTN_SUBMIT'.
      IF ls_disp-frgkz NE 'X' AND ls_disp-frgkz NE 'J'.
        MESSAGE i000 WITH TEXT-m90 DISPLAY LIKE 'E'.  "결재대기, 반려 상태만 가능합니다.
        EXIT.
      ENDIF.

      lv_run_type = 'REL'.
      lv_rel_code = 'DR'.

    WHEN 'BTN_ALLOW'.
      IF ls_disp-frgkz NE 'D'.
        MESSAGE i000 WITH TEXT-m91 DISPLAY LIKE 'E'.  "상신중 상태만 가능합니다.
        EXIT.
      ENDIF.

      lv_run_type = 'REL'.
      lv_rel_code = 'AP'.

    WHEN 'BTN_REJECT'.
      IF ls_disp-frgkz NE 'D'.
        MESSAGE i000 WITH TEXT-m91 DISPLAY LIKE 'E'.  "상신중 상태만 가능합니다.
        EXIT.
      ENDIF.

      lv_run_type = 'REL'.
      lv_rel_code = 'RE'.

  ENDCASE.

  CALL FUNCTION 'ZFIMM_PR_RELEASE'
    EXPORTING
      iv_run_type = lv_run_type
      iv_rel_code = lv_rel_code
    TABLES
      it_banfn    = gt_apv_pr.

  WAIT UP TO '0.1' SECONDS.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM refresh_data.

  grf_grid->refresh_grid_display( ).

ENDFORM.
