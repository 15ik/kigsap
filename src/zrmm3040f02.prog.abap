*&---------------------------------------------------------------------*
*& Include          ZRMM3040F02
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
    grf_grid->set_grid( CHANGING ct_data = gt_disp ).

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

  "구매조직 TEXT
  SELECT SINGLE ekotx
    FROM t024e
   WHERE ekorg = @p_ekorg
    INTO @DATA(lv_ekotx).

*-----------------------------------
* Header Column 지정
*-----------------------------------
  _l_set_header: TEXT-f01   p_bukrs   lv_butxt,
                 TEXT-f02   p_ekorg   lv_ekotx.

* 국내/해외
  CASE p_kalsk.
    WHEN gc_kalsk_do.
      _l_set_header: TEXT-f03   TEXT-f03  ''.
    WHEN gc_kalsk_im.
      _l_set_header: TEXT-f04   TEXT-f04  ''.
  ENDCASE.

*-----------------------------------
* Header 주석
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_grid_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
*    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'INFNR'. "구매정보레코드
        _l_set_fcat: 01 'X'  ''  TEXT-c01  ''  ''  ''  ''  '10'.
      WHEN 'ESOKZ'. "범주
        _l_set_fcat: 02 ''  ''  TEXT-c02  'C'  ''  ''  ''  '04'.
      WHEN 'ESOKZ_TEXT'.  "범주내역
        _l_set_fcat: 03 ''  ''  TEXT-c03  ''  ''  ''  ''  '08'.
      WHEN 'LIFNR'. "공급업체
        _l_set_fcat: 04 ''  ''  TEXT-c04  ''  ''  ''  ''  '10'.
      WHEN 'NAME1'. "업체명
        _l_set_fcat: 05 ''  ''  TEXT-c05  ''  ''  ''  ''  '20'.
      WHEN 'MATNR'. "자재
        _l_set_fcat: 06 ''  ''  TEXT-c06  ''  ''  ''  ''  '10'.
      WHEN 'MAKTX'. "자재내역
        _l_set_fcat: 07 ''  ''  TEXT-c07  ''  ''  ''  ''  '20'.
      WHEN 'EKORG'. "구매조직
        _l_set_fcat: 08 ''  ''  TEXT-c08  'C'  ''  ''  ''  '08'.
      WHEN 'EKOTX'. "구매조직명
        _l_set_fcat: 09 ''  ''  TEXT-c09  ''  ''  ''  ''  '15'.
      WHEN 'WERKS'. "플랜트
        _l_set_fcat: 10 ''  ''  TEXT-c10  'C'  ''  ''  ''  '06'.
      WHEN 'WERKS_TEXT'.  "플랜트명
        _l_set_fcat: 11 ''  ''  TEXT-c11  ''  ''  ''  ''  '10'.
      WHEN 'EKGRP'. "구매그룹
        _l_set_fcat: 12 ''  ''  TEXT-c12  'C'  ''  ''  ''  '08'.
      WHEN 'EKNAM'. "구매그룹명
        _l_set_fcat: 13 ''  ''  TEXT-c13  ''  ''  ''  ''  '10'.
      WHEN 'BSTAE'. "확인관리
        _l_set_fcat: 14 ''  ''  TEXT-c14  'C'  ''  ''  ''  '08'.
      WHEN 'BSBEZ'. "확인관리내역
        _l_set_fcat: 15 ''  ''  TEXT-c15  ''  ''  ''  ''  '15'.
      WHEN 'TAXIM'. "자재세금지시자
        _l_set_fcat: 16 ''  ''  TEXT-c16  'C'  ''  ''  ''  '10'.
      WHEN 'TAXIB'. "자재세금지시자 내역
        _l_set_fcat: 17 ''  ''  TEXT-c17  ''  ''  ''  ''  '15'.
      WHEN 'MWSKZ'. "세금코드
        _l_set_fcat: 18 ''  ''  TEXT-c18  'C'  ''  ''  ''  '08'.
      WHEN 'TEXT1'. "세금코드내역
        _l_set_fcat: 19 ''  ''  TEXT-c19  ''  ''  ''  ''  '15'.
      WHEN 'WEBRE'. "GR기준I/V
        _l_set_fcat: 20 ''  ''  TEXT-c20  'C'  ''  ''  ''  '08'.
        <ls_fcat>-checkbox = 'X'.
      WHEN 'INCO1'. "인도조건
        _l_set_fcat: 21 ''  ''  TEXT-c21  'C'  ''  ''  ''  '06'.
      WHEN 'BEZEI'. "인도조건내역
        _l_set_fcat: 22 ''  ''  TEXT-c22  ''  ''  ''  ''  '15'.
      WHEN 'EXPRF'. "단가유형
        _l_set_fcat: 23 ''  ''  TEXT-c23  'C'  ''  ''  ''  '06'.
      WHEN 'EXPRF_TEXT'.  "단가유형내역
        _l_set_fcat: 24 ''  ''  TEXT-c24  ''  ''  ''  ''  '10'.
      WHEN 'URZLA'. "원산지
        _l_set_fcat: 25 ''  ''  TEXT-c25  'C'  ''  ''  ''  '06'.
      WHEN 'UEBTO'. "초과납품허용
        _l_set_fcat: 26 ''  ''  TEXT-c26  ''  ''  ''  ''  '08'.
      WHEN 'VERID'. "생산버전
        _l_set_fcat: 27 ''  ''  TEXT-c27  ''  ''  ''  ''  '08'.
      WHEN 'KBETR'. "인쇄교체비
        _l_set_fcat: 28 ''  ''  TEXT-c28  ''  ''  'WAERS'  ''  '10'.
        <ls_fcat>-parameter0 = 'I'.
        IF p_bukrs <> gc_bukrs_1101.
          <ls_fcat>-tech = 'X'.
        ENDIF.
      WHEN 'NETPR'. "단가
        _l_set_fcat: 29 ''  ''  TEXT-c29  ''  ''  'WAERS'  ''  '15'.
      WHEN 'WAERS'. "통화
        _l_set_fcat: 30 ''  ''  TEXT-c30  ''  ''  ''  ''  '05'.
      WHEN 'PEINH'. "가격단위
        _l_set_fcat: 31 ''  ''  TEXT-c31  ''  ''  ''  ''  '08'.
      WHEN 'KMEIN'. "발주단위
        _l_set_fcat: 32 ''  ''  TEXT-c43  'C'  ''  ''  ''  '05'.
      WHEN 'DATAB'. "시작일
        _l_set_fcat: 33 ''  ''  TEXT-c32  ''  ''  ''  ''  '8'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'DATBI'. "종료일
        _l_set_fcat: 34 ''  ''  TEXT-c33  ''  ''  ''  ''  '8'.
        <ls_fcat>-parameter0 = 'I'.

      WHEN 'CNTR_NO'. "계약번호
        _l_set_fcat: 41 ''  ''  TEXT-c40  ''  ''  ''  ''  '12'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'CNTR_REV'. "계약차수
        _l_set_fcat: 42 ''  ''  TEXT-c41  ''  ''  ''  ''  '06'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'ZORDER_PERSON'. "계약담당자
        _l_set_fcat: 43 ''  ''  TEXT-c34  ''  ''  ''  ''  '12'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'ZORDER_PERSON_NAME'.  "담당자명
        _l_set_fcat: 44 ''  ''  TEXT-c35  ''  ''  ''  ''  '10'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'ZORDER_DEPARTMENT'. "계약부서
        _l_set_fcat: 45 ''  ''  TEXT-c36  ''  ''  ''  ''  '10'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'ZORDER_DEPARTMENT_NAME'.  "부서명
        _l_set_fcat: 46 ''  ''  TEXT-c37  ''  ''  ''  ''  '25'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'TITLE'.  "계약제목
        _l_set_fcat: 47 ''  ''  TEXT-c42  ''  ''  ''  ''  '30'.
        <ls_fcat>-parameter0 = 'I'.
      WHEN 'AFILE'. "첨부파일
        _l_set_fcat: 48 ''  ''  TEXT-c38  'C'  ''  ''  ''  '06'.
        <ls_fcat>-parameter0 = 'I'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

* 외자 선택 시 필드 숨김
    IF p_kalsk = gc_kalsk_im.
      IF <ls_fcat>-parameter0 = 'I'.
        <ls_fcat>-tech = 'X'.
      ENDIF.
    ENDIF.

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

    WHEN 'LIFNR'.
      CHECK ls_disp-lifnr IS NOT INITIAL.

      PERFORM call_transaction_bp USING ls_disp-lifnr.

    WHEN 'MATNR'.
      CHECK ls_disp-matnr IS NOT INITIAL.

      SET PARAMETER ID 'MAT' FIELD ls_disp-matnr.
*      SET PARAMETER ID 'WRK' FIELD LS_DISP-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'K'.               "Purchasing View (Basic View: "K" without WERKS)
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'INFNR'.
      CHECK ls_disp-infnr IS NOT INITIAL.

      SET PARAMETER ID 'INF' FIELD ls_disp-infnr.
      SET PARAMETER ID 'LIF' FIELD ls_disp-lifnr.
      SET PARAMETER ID 'MAT' FIELD ls_disp-matnr.
      SET PARAMETER ID 'EKO' FIELD ls_disp-ekorg.
      SET PARAMETER ID 'WRK' FIELD ls_disp-werks.
      SET PARAMETER ID 'ESO' FIELD ls_disp-esokz.
      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION_BP
*&---------------------------------------------------------------------*
FORM call_transaction_bp USING iv_lifnr.

  DATA: lv_request TYPE REF TO cl_bupa_navigation_request,
        lv_options TYPE REF TO cl_bupa_dialog_joel_options,
        lv_bp_guid TYPE bu_partner_guid,
        ls_bp_role TYPE bus_roles.

  CONSTANTS: lc_fole TYPE bus_roles-role VALUE 'FLVN01'.  "공급업체

  ls_bp_role-role = lc_fole.

* Get BP number linked to the selected vendor
  DATA(lo_bp_vendor) = cvi_ka_bp_vendor=>get_instance( ).
  lv_bp_guid = lo_bp_vendor->get_assigned_bp_for_vendor( iv_lifnr ).

* Create a request/option.
  CREATE OBJECT lv_request.
  CREATE OBJECT lv_options.

* Fill the request fields.
  CALL METHOD lv_request->set_maintenance_id( lv_request->gc_maintenance_id_partner ).
  CALL METHOD lv_request->set_partner_guid( lv_bp_guid ).
  CALL METHOD lv_request->set_bupa_partner_role( ls_bp_role ).
  CALL METHOD lv_request->set_bupa_activity( lv_request->gc_activity_display ).

* Fill option - Navigation disable..
  CALL METHOD lv_options->set_navigation_disabled( 'X' ).
  CALL METHOD lv_options->set_activity_switching_off( 'X' ).
  CALL METHOD lv_options->set_navigate_on_first_tab( 'X' ).

* Start the maintenance.
  CALL METHOD cl_bupa_dialog_joel=>start_with_navigation
    EXPORTING
      iv_request = lv_request
      iv_options = lv_options
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM evt_grid_button_click USING is_col_id TYPE lvc_s_col
                                 is_row_no TYPE lvc_s_roid.

  READ TABLE gt_disp INTO DATA(ls_disp) INDEX is_row_no-row_id.

  CHECK sy-subrc EQ 0.

  CASE is_col_id-fieldname.
    WHEN 'AFILE'.
      DATA(ls_sibflporb) = VALUE sibflporb( ).

      ls_sibflporb = VALUE #( instid = ls_disp-infnr
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

  _l_add_toolbar: 'BTN_HIST'  icon_history  TEXT-u01  ''  ''  TEXT-u01.

  IF p_bukrs = gc_bukrs_1101.
    _l_add_toolbar: 'BTN_ZPRI'  icon_history  TEXT-u02  ''  ''  TEXT-u02.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_COND_HISTORY
*&---------------------------------------------------------------------*
FORM popup_cond_history.

  DATA: lt_mat TYPE TABLE OF ty_cond_hist.

*  DATA: LV_SDATE TYPE SY-DATUM.


  CLEAR: gt_rows, gt_cond_hist.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

*-----------------------------
* 조회 기간 설정
*-----------------------------
* 회사코드별 구매단가 이력조회 기간
*  ZCL_MM_COMMON=>COMMON_CONFIG(
*     EXPORTING IS_COMMON = VALUE #( M = 'C1' D = 'PMAST' S = 'PM002' )
*                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = P_BUKRS ) )
*     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
*
*  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.
*
*  GV_YEAR = LS_CONFIG-FIELD2.
*
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      DATE      = SY-DATUM
*      SIGNUM    = '-'
*      DAYS      = 0
*      MONTHS    = 0
*      YEARS     = GV_YEAR
*    IMPORTING
*      CALC_DATE = LV_SDATE.

*-----------------------------
* 검색대상 자재 추출
*-----------------------------
  LOOP AT gt_rows INTO DATA(ls_rows).

    READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

    IF sy-subrc EQ 0 AND ls_disp-matnr IS NOT INITIAL.
      lt_mat = VALUE #( BASE lt_mat ( matnr = ls_disp-matnr ) ).
    ENDIF.

  ENDLOOP.

  SORT lt_mat BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING matnr.

*-----------------------------
* 단가 이력 검색
*-----------------------------
  IF lt_mat IS NOT INITIAL.

    SELECT infnr,
           lifnr,
           name1,
           matnr,
           materialdescription AS maktx,
           purchasingorganization AS ekorg,
           purchasinginforecordcategory AS esokz,
           plant AS werks,
           conditionratevalue AS netpr,
           conditionquantity AS peinh,
           conditionratevalueunit AS waers,
           conditionvaliditystartdate AS datab,
           conditionvalidityenddate AS datbi
      FROM zsvbmminfoprice
       FOR ALL ENTRIES IN @lt_mat
     WHERE matnr = @lt_mat-matnr
       AND purchasingorganization = @p_ekorg
*       AND CONDITIONVALIDITYSTARTDATE BETWEEN @LV_SDATE AND @SY-DATUM
       AND conditiontype = @gc_cond_type
       AND confirm_price = @gc_confirm
      INTO CORRESPONDING FIELDS OF TABLE @gt_cond_hist.

    SORT gt_cond_hist BY matnr datab DESCENDING.

    FREE lt_mat.

  ENDIF.

  IF gt_cond_hist IS INITIAL.
    MESSAGE s033 WITH TEXT-m01 DISPLAY LIKE 'E'.  "단가 이력이 존재하지 않습니다.
    EXIT.
  ENDIF.

  LOOP AT gt_cond_hist ASSIGNING FIELD-SYMBOL(<ls_cond_hist>).

    "구매정보레코드 범주
    PERFORM domain_value_get USING    'ESOKZ'
                                      <ls_cond_hist>-esokz
                             CHANGING <ls_cond_hist>-esokz_text.

  ENDLOOP.

  CALL SCREEN '0200' STARTING AT 01 01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_HIST
*&---------------------------------------------------------------------*
FORM set_grid_hist.

  DATA : ls_toolbtn TYPE zscn00004.

  IF grf_cont_hist IS NOT INITIAL.
    CALL METHOD grf_cont_hist->free.
    CLEAR: grf_cont_hist, grf_grid_hist.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT grf_cont_hist
    EXPORTING
      container_name = 'CONT_HIST'
    EXCEPTIONS
      OTHERS         = 1.

  ls_toolbtn-btn_exld   = 'X'.       "Excel Download

*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT grf_grid_hist
    EXPORTING
      iv_name    = 'ALV_HIST'   "다수의 그리드일 경우 식별하기 위함..
      irf_parent = grf_cont_hist
      is_toolbtn = ls_toolbtn.

*--------------------------------
* Dislay Grid..
*--------------------------------
  grf_grid_hist->set_grid( CHANGING ct_data = gt_cond_hist ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HIST_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_hist_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  CONSTANTS: lc_ref_table TYPE tabname VALUE 'ZSVBMMPRICE'.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
    IF <ls_fcat>-outputlen IS INITIAL.
    <ls_fcat>-col_opt    = 'X'.
    ENDIF.
    <ls_fcat>-ref_table  = lc_ref_table.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'INFNR'. "구매정보레코드
        _l_set_fcat: 01  'X'  ''  TEXT-c01  'C'  ''  ''  ''  '8'.
      WHEN 'ESOKZ'. "범주
        _l_set_fcat: 02  ''  'X'  TEXT-c02  'C'  ''  ''  ''  '4'.
      WHEN 'ESOKZ_TEXT'. "범주
        _l_set_fcat: 02  ''  ''  TEXT-c02  'C'  ''  ''  ''  '4'.
      WHEN 'LIFNR'. "공급업체
        _l_set_fcat: 03  ''  ''  TEXT-c04  'C'  ''  ''  ''  '7'.
      WHEN 'NAME1'. "업체명
        _l_set_fcat: 04  ''  ''  TEXT-c05  ''  ''  ''  ''  ''.
      WHEN 'MATNR'. "자재
        _l_set_fcat: 05  ''  ''  TEXT-c06  'C'  ''  ''  ''  '8'.
      WHEN 'MAKTX'. "자재내역
        _l_set_fcat: 06  ''  ''  TEXT-c07  ''  ''  ''  ''  ''.
      WHEN 'EKORG'. "구매조직
        _l_set_fcat: 07  ''  ''  TEXT-c08  'C'  ''  ''  ''  '5'.
      WHEN 'WERKS'. "플랜트
        _l_set_fcat: 08  ''  ''  TEXT-c10  'C'  ''  ''  ''  '5'.
      WHEN 'NETPR'. "단가
        _l_set_fcat: 09  ''  ''  TEXT-c29  ''  ''  'WAERS'  ''  '9'.
      WHEN 'PEINH'. "가격단위
        _l_set_fcat: 10  ''  ''  TEXT-c31  ''  ''  ''  ''  '5'.
      WHEN 'WAERS'. "통화
        _l_set_fcat: 11  ''  ''  TEXT-c30  'C'  ''  ''  ''  '4'.
      WHEN 'DATAB'. "시작일
        _l_set_fcat: 12  ''  ''  TEXT-c32  'C'  ''  ''  ''  '6'.
      WHEN 'DATBI'. "종료일
        _l_set_fcat: 13  ''  ''  TEXT-c33  'C'  ''  ''  ''  '6'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_HIST_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM evt_hist_double_click USING iv_row
                                 iv_column.

  READ TABLE gt_cond_hist INTO DATA(ls_cond_hist) INDEX iv_row.

  CASE iv_column.
    WHEN 'INFNR'.
      CHECK ls_cond_hist-infnr IS NOT INITIAL.

      SET PARAMETER ID 'LIF' FIELD ls_cond_hist-lifnr.
      SET PARAMETER ID 'MAT' FIELD ls_cond_hist-matnr.
      SET PARAMETER ID 'INF' FIELD ls_cond_hist-infnr.
      SET PARAMETER ID 'EKO' FIELD ls_cond_hist-ekorg.
      SET PARAMETER ID 'WRK' FIELD ls_cond_hist-werks.
      SET PARAMETER ID 'ESO' FIELD ls_cond_hist-esokz.
      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_ZPRI_HISTORY
*&---------------------------------------------------------------------*
FORM popup_zpri_history.

  DATA: lt_mat TYPE TABLE OF ty_zpri_hist.

  CLEAR: gt_rows, gt_zpri_hist.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

*-----------------------------
* 검색대상 자재 추출
*-----------------------------
  LOOP AT gt_rows INTO DATA(ls_rows).

    READ TABLE gt_disp INTO DATA(ls_disp) INDEX ls_rows-index.

    IF sy-subrc EQ 0 AND ls_disp-matnr IS NOT INITIAL.
      lt_mat = VALUE #( BASE lt_mat ( matnr = ls_disp-matnr ) ).
    ENDIF.

  ENDLOOP.

  SORT lt_mat BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING matnr.

*-----------------------------
* 인쇄교체비 이력 검색
*-----------------------------
  IF lt_mat IS NOT INITIAL.

    SELECT matnr,                 "자재
           datab,                 "유효 시작일
           datbi,                 "유효 종료일
           kbetr,                 "금액
           kpein,                 "가격단위
           currency AS waers      "통화
      FROM zsvcmm_a445_c
       FOR ALL ENTRIES IN @lt_mat
     WHERE matnr = @lt_mat-matnr
      INTO CORRESPONDING FIELDS OF TABLE @gt_zpri_hist.

    SORT gt_zpri_hist BY matnr datab.

    FREE lt_mat.

  ENDIF.

  IF gt_zpri_hist IS INITIAL.
    MESSAGE s033 WITH TEXT-m02 DISPLAY LIKE 'E'.  "인쇄교체비 이력이 존재하지 않습니다.
    EXIT.
  ENDIF.

  CALL SCREEN '0300' STARTING AT 01 01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_ZPRI
*&---------------------------------------------------------------------*
FORM set_grid_zpri.

 data : ls_toolbtn type zscn00004.

  IF grf_cont_zpri IS NOT INITIAL.
    CALL METHOD grf_cont_zpri->free.
    CLEAR: grf_cont_zpri, grf_grid_zpri.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT grf_cont_zpri
    EXPORTING
      container_name = 'CONT_ZPRI'
    EXCEPTIONS
      OTHERS         = 1.

  ls_toolbtn-btn_exld   = 'X'.       "Excel Download
*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT grf_grid_zpri
    EXPORTING
      iv_name    = 'ALV_ZPRI'   "다수의 그리드일 경우 식별하기 위함..
      irf_parent = grf_cont_zpri
      is_toolbtn = ls_toolbtn.

*--------------------------------
* Dislay Grid..
*--------------------------------
  grf_grid_zpri->set_grid( CHANGING ct_data = gt_zpri_hist ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_ZPRI_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM alv_zpri_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
*    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'MATNR'. "자재
        _l_set_fcat: 01  ''  ''  TEXT-c06  ''  ''  ''  ''  '15'.
      WHEN 'DATAB'. "시작일
        _l_set_fcat: 02  ''  ''  TEXT-c32  ''  ''  ''  ''  '10'.
      WHEN 'DATBI'. "종료일
        _l_set_fcat: 03  ''  ''  TEXT-c33  ''  ''  ''  ''  '10'.
      WHEN 'KBETR'. "금액
        _l_set_fcat: 04  ''  ''  TEXT-c39  ''  ''  'WAERS'  ''  '10'.
      WHEN 'KPEIN'. "가격단위
        _l_set_fcat: 05  ''  ''  TEXT-c31  ''  ''  ''  ''  '06'.
      WHEN 'WAERS'. "통화
        _l_set_fcat: 06  ''  ''  TEXT-c30  ''  ''  ''  ''  '05'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
