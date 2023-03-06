*&---------------------------------------------------------------------*
*& Include          ZOMM5010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      FORM  SET_SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM set_screen_modify .
  CONSTANTS: lc_p_bukrs TYPE screen-name VALUE 'P_BUKRS'.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'MST'.   "회사코드
        CASE gv_exc_user.
          WHEN 'EX01'.
            screen-input = 1.
          WHEN 'EX02'.
            IF screen-name EQ lc_p_bukrs.
              screen-input = 0.
            ENDIF.
          WHEN OTHERS.
            screen-input = 0.
        ENDCASE.

      WHEN 'PYT'. "정발행/역발행 구분
        "물품선급/공사용역은 무조건 정발행 , 자율납품&위탁정산은 무조건 역발행
        IF p_rp2 EQ 'X' OR p_rp4 EQ 'X'OR p_rp3 EQ 'X'.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN 'CUR'. "외화/원화 구분
        "물품선급/자율납품&위탁정산 무조건 원화.
        IF p_rp2 EQ 'X' OR p_rp3 EQ 'X'
          OR ( p_rp1 EQ 'X' AND p_r2 EQ 'X' ).
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN 'RP3'.
        IF p_bukrs NE gc_bukrs_1101.
          screen-input = 0.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM. " SET_SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      FORM  INITIALIZE_SET
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM initialize_set .

  SET TITLEBAR  'T0001' WITH '대급지급 요청'(t17).

*  DATA LV_DAY(2) TYPE N.

*-회사코드
*  SELECT SINGLE company, employ_no, department
*    FROM zsvmm_user_info
*   WHERE user_id = @sy-uname
*    INTO @DATA(ls_user_info).
*
*  IF sy-subrc EQ 0.
*    p_bukrs = ls_user_info-company.
*    s_orddp[] = VALUE #( ( sign = 'I' option = 'EQ' low = ls_user_info-department ) ).
*    s_expdp[] = VALUE #( ( sign = 'I' option = 'EQ' low = ls_user_info-department ) ).
*  ENDIF.

  "실제 값이 있어야 OUTPUT SCREEN 제어가 가능하기 때문..
  IF p_bukrs IS INITIAL.
    GET PARAMETER ID 'BUK' FIELD p_bukrs.
  ENDIF.

*> HEADER/ITEM 기본값 설정.
  gs_expand-h = abap_true.
  gs_expand-i = abap_true.
  gs_expand-d = abap_false.


*-입고일: 당월1일 ~ 말일
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = s_budat-high.

  s_budat-sign   = 'I'.
  s_budat-option = gc_bt.
  s_budat-low    = sy-datum(6) && gc_01.
  APPEND s_budat.

*>예외처리 유저 점검.
  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'A1' d = 'A1010' s = 'AB100' )
                                     it_where = VALUE #(
                                                         ( field = 1 value = sy-repid )
                                                         ( field = 3 value = sy-uname )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).

  READ TABLE lt_config INTO DATA(ls_config) INDEX 1.
  IF sy-subrc EQ 0.
    gv_exc_user = ls_config-field2.
  ENDIF.

* 기본 Variant
  PERFORM set_default_variant.

  PERFORM build_dynpro_flow.

ENDFORM. " INITIALIZE_SET
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM handle_double_click USING iv_row TYPE lvc_s_row
                               iv_column  TYPE lvc_s_col
                               is_row_no TYPE lvc_s_roid
                               irf_sender    TYPE REF TO cl_gui_alv_grid.


  CASE irf_sender.
    WHEN gv_grid_left.
      PERFORM select_data_head USING is_row_no-row_id
                                     iv_column-fieldname.

    WHEN gv_grid_head.
      READ TABLE gt_head INTO gs_head INDEX is_row_no-row_id.
      CHECK sy-subrc EQ 0.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      FORM  CREATE_LEFT_ALV
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM create_left_alv.

  CONSTANTS: lc_0001 TYPE sy-dynnr VALUE '0001'.

  CHECK gv_docking_container IS NOT BOUND.

  CREATE OBJECT gv_docking_container
    EXPORTING
      repid = sy-repid
      dynnr = lc_0001 "SY-DYNNR
      ratio = 27
      side  = cl_gui_docking_container=>dock_at_left.

  PERFORM switch_nav_cont.
  PERFORM set_left_area_alv.

ENDFORM. " CREATE_LEFT_ALV
*&---------------------------------------------------------------------*
*&      FORM  SWITCH_NAV_CONT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM switch_nav_cont.

  CASE gv_visible_n.
    WHEN cl_gui_docking_container=>visible_false OR abap_false OR space.
      gv_docking_container->set_visible( cl_gui_docking_container=>visible_true ).
      gv_visible_n   = cl_gui_docking_container=>visible_true.
      gv_dynamic_txt = TEXT-022.  "개요 숨기기

    WHEN cl_gui_docking_container=>visible_true.
      gv_docking_container->set_visible( cl_gui_docking_container=>visible_false ).
      gv_visible_n   = cl_gui_docking_container=>visible_false.
      gv_dynamic_txt = TEXT-023.  "개요 표시

  ENDCASE.
ENDFORM. " SWITCH_NAV_CONT
*&---------------------------------------------------------------------*
*&      FORM  SET_LEFT_AREA_ALV
*&---------------------------------------------------------------------*
*       화면 왼쪽영역: 생성한 송장번호를 보여준다.
*----------------------------------------------------------------------*
FORM set_left_area_alv .

  DATA: lt_fcat TYPE lvc_t_fcat,
        lt_excl TYPE ui_functions,
        ls_layo TYPE lvc_s_layo,
        lt_sort TYPE lvc_t_sort.

  CHECK gv_grid_left IS NOT BOUND.

  CREATE OBJECT gv_grid_left
    EXPORTING
      i_parent = gv_docking_container.

  PERFORM build_layout_left   CHANGING ls_layo.
  PERFORM build_fieldcat_left CHANGING lt_fcat.
  PERFORM build_toolbar_left  TABLES   lt_excl.
  PERFORM build_sort_left     TABLES   lt_sort.
  PERFORM event_handler_register_left.

  CALL METHOD gv_grid_left->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layo
      it_toolbar_excluding = lt_excl
    CHANGING
      it_outtab            = gt_left[]
      it_fieldcatalog      = lt_fcat
      it_sort              = lt_sort.


ENDFORM. " SET_LEFT_AREA_ALV
*&---------------------------------------------------------------------*
*&      FORM  BUILD_LAYOUT_LEFT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_layout_left CHANGING cs_layo TYPE lvc_s_layo.

  cs_layo-no_rowmark = 'X'.
  cs_layo-no_hgridln = 'X'.
  cs_layo-no_vgridln = 'X'.
  cs_layo-zebra      = 'X'.
  cs_layo-info_fname = gc_linecolor.

ENDFORM. " BUILD_LAYOUT_LEFT
*&---------------------------------------------------------------------*
*&      FORM  BUILD_FIELDCAT_LEFT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_fieldcat_left CHANGING ct_fcat TYPE lvc_t_fcat.

  PERFORM build_fieldcat_merge TABLES ct_fcat
                               USING  gc_struc_left.

  LOOP AT ct_fcat INTO gs_lvc_fcat.

    CLEAR: gs_lvc_fcat-no_out, gs_lvc_fcat-key.

    CASE gs_lvc_fcat-fieldname.
      WHEN 'LIFNR'.
        gs_lvc_fcat-col_pos    = 0.
        gs_lvc_fcat-coltext    = TEXT-f01. "업체
        gs_lvc_fcat-key        = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'LIFNR_TX'.
        gs_lvc_fcat-col_pos    = 1.
        gs_lvc_fcat-coltext    = TEXT-f02. "업체명
        gs_lvc_fcat-outputlen  = 12.

      WHEN 'MENGE'.
        gs_lvc_fcat-col_pos    = 2.
        gs_lvc_fcat-coltext    = TEXT-f12.  "총수량
        gs_lvc_fcat-no_out     = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'DMBTR'.
        gs_lvc_fcat-col_pos    = 3.
        gs_lvc_fcat-coltext    = TEXT-f13.  "총금액
        gs_lvc_fcat-outputlen  = 12.

      WHEN 'WAERS'.
        gs_lvc_fcat-col_pos    = 5.
        gs_lvc_fcat-coltext    = TEXT-f09.  "통화
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'MWSKZ'.
        gs_lvc_fcat-col_pos    = 6.
        gs_lvc_fcat-coltext    = TEXT-f15.  "세금
        gs_lvc_fcat-no_out     = abap_true.
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'LIGHT'.
        gs_lvc_fcat-col_pos    = 7.
        gs_lvc_fcat-coltext    = TEXT-f29.  "전자세금계산서 수령 여부
        gs_lvc_fcat-tech       = p_r2.
        gs_lvc_fcat-outputlen  = 10.
        gs_lvc_fcat-icon  = 'X'.

      WHEN OTHERS.
        gs_lvc_fcat-no_out     = abap_true.

    ENDCASE.

    gs_lvc_fcat-scrtext_s =
    gs_lvc_fcat-scrtext_m =
    gs_lvc_fcat-scrtext_l = gs_lvc_fcat-reptext = gs_lvc_fcat-coltext.
    MODIFY ct_fcat FROM gs_lvc_fcat.
    CLEAR gs_lvc_fcat.

  ENDLOOP.

ENDFORM. " BUILD_FIELDCAT_LEFT
*&---------------------------------------------------------------------*
*&      FORM  EVENT_HANDLER_REGISTER_LEFT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM event_handler_register_left.

  CREATE OBJECT gv_grid_event_receiver.
  SET HANDLER gv_grid_event_receiver->handle_double_click
                                                     FOR gv_grid_left.

ENDFORM. " EVENT_HANDLER_REGISTER_LEFT
*&---------------------------------------------------------------------*
*&      FORM  BUILD_TOOLBAR_LEFT
*&---------------------------------------------------------------------*
*       툴바의 버튼중 나타내지 않을 버튼을 지정한다
*----------------------------------------------------------------------*
FORM build_toolbar_left TABLES et_uifunc TYPE ui_functions.

  PERFORM append_exclude_functions
        TABLES  et_uifunc
         USING : cl_gui_alv_grid=>mc_fc_loc_undo,
                 cl_gui_alv_grid=>mc_fc_back_classic,
                 cl_gui_alv_grid=>mc_fc_call_chain,
                 cl_gui_alv_grid=>mc_fc_call_crbatch,
                 cl_gui_alv_grid=>mc_fc_call_crweb,
                 cl_gui_alv_grid=>mc_fc_call_lineitems,
                 cl_gui_alv_grid=>mc_fc_call_master_data,
                 cl_gui_alv_grid=>mc_fc_call_more,
                 cl_gui_alv_grid=>mc_fc_call_report,
                 cl_gui_alv_grid=>mc_fc_call_xint,
                 cl_gui_alv_grid=>mc_fc_col_invisible,
                 cl_gui_alv_grid=>mc_fc_data_save,
                 cl_gui_alv_grid=>mc_fc_delete_filter,
                 cl_gui_alv_grid=>mc_fc_expcrdata,
                 cl_gui_alv_grid=>mc_fc_expcrdesig,
                 cl_gui_alv_grid=>mc_fc_expcrtempl,
                 cl_gui_alv_grid=>mc_fc_expmdb,
                 cl_gui_alv_grid=>mc_fc_extend,
                 cl_gui_alv_grid=>mc_fc_f4,
                 cl_gui_alv_grid=>mc_fc_find,
                 cl_gui_alv_grid=>mc_fc_help,
                 cl_gui_alv_grid=>mc_fc_info,
                 cl_gui_alv_grid=>mc_fc_loc_copy,          " 행 카피
                cl_gui_alv_grid=>mc_fc_loc_copy_row,      " 행 카피
                cl_gui_alv_grid=>mc_fc_loc_cut,           " 가위
                cl_gui_alv_grid=>mc_fc_loc_delete_row,    " 행삭제
                cl_gui_alv_grid=>mc_fc_loc_insert_row,    " 행삽입
                cl_gui_alv_grid=>mc_fc_loc_move_row,
                 cl_gui_alv_grid=>mc_fc_loc_append_row,    " 라인생성
                cl_gui_alv_grid=>mc_fc_loc_paste,         " 겹쳐쓰기
                cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " 겹쳐쓰기
                cl_gui_alv_grid=>mc_fc_maximum,
                 cl_gui_alv_grid=>mc_fc_minimum,
                 cl_gui_alv_grid=>mc_fc_reprep,
                 cl_gui_alv_grid=>mc_fc_send,
                 cl_gui_alv_grid=>mc_fc_separator,
                 cl_gui_alv_grid=>mc_fc_to_rep_tree,
                 cl_gui_alv_grid=>mc_fc_unfix_columns,
                 cl_gui_alv_grid=>mc_fc_refresh,
                 cl_gui_alv_grid=>mc_fc_check,
                 cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
                 cl_gui_alv_grid=>mc_fc_graph,
                 cl_gui_alv_grid=>mc_fc_to_office,
                 cl_gui_alv_grid=>mc_fc_view_grid,
                 cl_gui_alv_grid=>mc_fc_word_processor.


ENDFORM. " BUILD_TOOLBAR_LEFT
*&---------------------------------------------------------------------*
*&      FORM  BUILD_SORT_LEFT
*&---------------------------------------------------------------------*
*       소팅
*----------------------------------------------------------------------*
FORM build_sort_left TABLES et_sort TYPE lvc_t_sort.

  et_sort-fieldname = gc_lifnr_tx.
  et_sort-up        = abap_true.
  APPEND et_sort.

ENDFORM. " BUILD_SORT_LEFT
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_toolbar USING irf_object TYPE REF TO cl_alv_event_toolbar_set
                           iv_interactive
                           irf_sender   TYPE REF TO cl_gui_alv_grid..


  CASE irf_sender.
    WHEN gv_grid_head.
      PERFORM handle_toobar2 USING
           irf_object
           3     space  space
           space space  space.               "분리선

      PERFORM handle_toobar2 USING
            irf_object
            space TEXT-t22  icon_deselect_all
            TEXT-t23  TEXT-t23  space.        "전체해제

      PERFORM handle_toobar2 USING
            irf_object
            space TEXT-t24  icon_select_all
            TEXT-t25  TEXT-t25  space.        "전체선택

      PERFORM handle_toobar2 USING
            irf_object
            3     space  space
            space space  space.               "분리선

      PERFORM handle_toobar2 USING
            irf_object
            space TEXT-t26  icon_history
            TEXT-t27  TEXT-t27  space.        "구매문서이력

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_TOOBAR2
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_toobar2
     USING irf_object TYPE REF TO cl_alv_event_toolbar_set
           VALUE(iv_btype)
           VALUE(iv_func)
           VALUE(iv_ficon)
           VALUE(iv_fqinfo)
           VALUE(iv_ftext)
           VALUE(iv_fdisa).

  DATA ls_toolbar  TYPE stb_button.

  ls_toolbar-butn_type = iv_btype.
  ls_toolbar-function = iv_func.
  ls_toolbar-icon = iv_ficon.
  ls_toolbar-quickinfo = iv_fqinfo.
  ls_toolbar-text = iv_ftext.
  ls_toolbar-disabled = iv_fdisa.
  APPEND ls_toolbar TO irf_object->mt_toolbar.

ENDFORM. " handle_toobar2
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_user_command USING iv_ucomm
                               irf_sender TYPE REF TO cl_gui_alv_grid.

  CASE irf_sender.
    WHEN gv_grid_head.
      PERFORM link_to_button_action_head USING iv_ucomm.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      FORM  ICON_CREATE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM icon_create_toggle .

  FIELD-SYMBOLS: <lv_field>  TYPE any.
  FIELD-SYMBOLS: <lv_expand> TYPE any.

  DATA: lv_new_icon(30).

  CASE sy-dynnr.
    WHEN 20.
      ASSIGN gs_dyn_0020-button TO <lv_field>.
      ASSIGN COMPONENT 1 OF STRUCTURE gs_expand TO <lv_expand>.
    WHEN 21.
      ASSIGN gs_dyn_0021-button TO <lv_field>.
      ASSIGN COMPONENT 2 OF STRUCTURE gs_expand TO <lv_expand>.
    WHEN 22.
      ASSIGN gs_dyn_0022-button TO <lv_field>.
      ASSIGN COMPONENT 3 OF STRUCTURE gs_expand TO <lv_expand>.
  ENDCASE.

  CASE <lv_expand>.
    WHEN abap_true.
      lv_new_icon = icon_data_area_expand.
    WHEN abap_false.
      lv_new_icon = icon_data_area_collapse.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = lv_new_icon
    IMPORTING
      result = <lv_field>
    EXCEPTIONS
      OTHERS = 0.                                       "511109

ENDFORM. " ICON_CREATE
*&---------------------------------------------------------------------*
*&      FORM  BUILD_SCREEN_FLOW
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_dynpro_flow .

  CONSTANTS: lc_0003 TYPE sy-dynnr VALUE '0003',
             lc_0040 TYPE sy-dynnr VALUE '0040'.

  PERFORM make_dynpro_flow USING : lc_0003 '0020' 1,
                                   lc_0003 '0021' 2,
*                                   '0003' '0022' 3,
                                   lc_0040 '0050' 1,
                                   lc_0040 '0051' 2.
*                                   '0040' '0052' 3.

ENDFORM. " BUILD_SCREEN_FLOW
*&---------------------------------------------------------------------*
*&      FORM  MAKE_SCREEN_FLOW
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM make_dynpro_flow USING VALUE(iv_dynpro_calling)
                                VALUE(iv_dynpro_called)
                                VALUE(iv_dynpro_flow).

  CLEAR gs_dynpro_flow.

  gs_dynpro_flow-dynpro_calling = iv_dynpro_calling.
  gs_dynpro_flow-dynpro_called  = iv_dynpro_called.
  gs_dynpro_flow-dynpro_flow    = iv_dynpro_flow.

  APPEND gs_dynpro_flow TO gt_dynpro_flow.

ENDFORM. " MAKE_SCREEN_FLOW
*&---------------------------------------------------------------------*
*&      MODULE  DETERMINE_SUBSCREEN_FLOW  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE determine_togg_subscreen_flow OUTPUT.

  PERFORM determine_togg_subscreen_flow.

ENDMODULE. " DETERMINE_SUBSCREEN_FLOW OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  DETERMINE_EXPA_DYNPRO  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE determine_expa_dynpro OUTPUT.

  PERFORM determine_expa_dynpro.

ENDMODULE. " DETERMINE_EXPA_DYNPRO OUTPUT
*&---------------------------------------------------------------------*
*&      FORM  DETERMINE_TOGG_SUBSCREEN_FLOW
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM determine_togg_subscreen_flow .

  STATICS lv_next_flow_id TYPE i.

  IF lv_next_flow_id IS INITIAL.
    READ TABLE gt_dynpro_flow INTO gs_dynpro_flow
                              WITH KEY dynpro_calling = sy-dynnr.
  ELSE.
    ADD 1 TO lv_next_flow_id.
    READ TABLE gt_dynpro_flow INTO gs_dynpro_flow
                              WITH KEY dynpro_calling = sy-dynnr
                                       dynpro_flow    = lv_next_flow_id.
  ENDIF.

  gv_call_subscreen = gs_dynpro_flow-dynpro_called.
  lv_next_flow_id = gs_dynpro_flow-dynpro_flow.

  IF lv_next_flow_id = 2.
    CLEAR lv_next_flow_id.
  ENDIF.

ENDFORM. " DETERMINE_TOGG_SUBSCREEN_FLOW
*&---------------------------------------------------------------------*
*&      FORM  DETERMINE_EXPA_DYNPRO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM determine_expa_dynpro .

  STATICS lv_next_flow_id TYPE i.

  IF lv_next_flow_id IS INITIAL.
    READ TABLE gt_dynpro_flow INTO gs_dynpro_flow
                              WITH KEY dynpro_calling = sy-dynnr.
  ELSE.
    ADD 1 TO lv_next_flow_id.
    READ TABLE gt_dynpro_flow INTO gs_dynpro_flow
                              WITH KEY dynpro_calling = sy-dynnr
                                       dynpro_flow    = lv_next_flow_id.
  ENDIF.

  gv_call_subscreen = gs_dynpro_flow-dynpro_called.

  CASE gs_dynpro_flow-dynpro_flow.
    WHEN 1.
      IF gs_expand-h = abap_false.
        gv_call_subscreen = gc_0030.
        gs_dyn_0030-label = TEXT-011.
      ENDIF.
    WHEN 2.
      IF gs_expand-i = abap_false.
        gv_call_subscreen = gc_0030.
        gs_dyn_0030-label = TEXT-012.
      ENDIF.
  ENDCASE.

  lv_next_flow_id = gs_dynpro_flow-dynpro_flow.

  IF lv_next_flow_id = 2.
    CLEAR lv_next_flow_id.
  ENDIF.

ENDFORM. " DETERMINE_EXPA_DYNPRO
*&---------------------------------------------------------------------*
*&      MODULE  DETERMINE_SIZE_DYNPRO  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE determine_size_dynpro OUTPUT.

  IF     gs_expand-h = abap_true AND
         gs_expand-i = abap_true AND
         gs_expand-d = abap_true.

    gv_call_subscreen = 10.

  ELSEIF gs_expand-h = abap_true  AND
         gs_expand-i = abap_false AND
         gs_expand-d = abap_true.

    gv_call_subscreen = 10.

  ELSEIF gs_expand-h = abap_false AND
         gs_expand-i = abap_false AND
         gs_expand-d = abap_false.

    gv_call_subscreen = 16.

  ELSEIF gs_expand-h = abap_false AND
         gs_expand-i = abap_true  AND
         gs_expand-d = abap_false.

    gv_call_subscreen = 16.

  ELSEIF gs_expand-h = abap_false AND
         gs_expand-i = abap_true AND
         gs_expand-d = abap_true.

    gv_call_subscreen = 15.

  ELSEIF gs_expand-h = abap_true AND
         gs_expand-i = abap_true AND
         gs_expand-d = abap_false.

    gv_call_subscreen = 13.

  ELSEIF gs_expand-h = abap_true AND
         gs_expand-i = abap_false AND
         gs_expand-d = abap_false.

    gv_call_subscreen = 13.

  ELSEIF gs_expand-h = abap_false AND
         gs_expand-i = abap_false AND
         gs_expand-d = abap_true.

    gv_call_subscreen = 16.

  ENDIF.

ENDMODULE. " DETERMINE_SIZE_DYNPRO OUTPUT
*&---------------------------------------------------------------------*
*&      FORM  BUILD_LAYOUT_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_layout_head CHANGING cs_layo TYPE lvc_s_layo.

  cs_layo-sel_mode   = 'A'.
  cs_layo-zebra      = abap_true.
  cs_layo-col_opt    = abap_true.
  cs_layo-no_rowmark = abap_true.

ENDFORM. " BUILD_LAYOUT_HEAD
*&---------------------------------------------------------------------*
*&      FORM  BUILD_FIELDCAT_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_fieldcat_head CHANGING ct_fcat TYPE lvc_t_fcat.

  DATA lv_flag(1)  TYPE c.
  DATA lv_flag2(1) TYPE c.
  DATA lv_flag4(1) TYPE c.
  DATA lv_text     TYPE string.


  PERFORM build_fieldcat_merge TABLES ct_fcat
                               USING  gc_struc_head.

  IF p_rp4 EQ abap_true.     "공사용역
    lv_flag = abap_true.
  ENDIF.

  LOOP AT ct_fcat INTO gs_lvc_fcat.

    CLEAR: gs_lvc_fcat-no_out, gs_lvc_fcat-key.

    CASE gs_lvc_fcat-fieldname.
      WHEN 'CBOX'.
        gs_lvc_fcat-col_pos    = 0.
        gs_lvc_fcat-coltext    = TEXT-f00. "선택
        gs_lvc_fcat-checkbox   = abap_true.
        gs_lvc_fcat-edit       = abap_true.
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'LIFNR'.
        gs_lvc_fcat-col_pos    = 1.
        gs_lvc_fcat-coltext    = TEXT-f01. "업체
        gs_lvc_fcat-key        = abap_true.
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'LIFNR_TX'.
        gs_lvc_fcat-col_pos    = 2.
        gs_lvc_fcat-coltext    = TEXT-f02. "업체명
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 25.

      WHEN 'BELNR'.
        gs_lvc_fcat-col_pos    = 3.
        gs_lvc_fcat-coltext    = TEXT-f03.  "입고번호
        gs_lvc_fcat-emphasize  = gc_c111.
        gs_lvc_fcat-hotspot    = abap_true.
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'BEZEI'.
        gs_lvc_fcat-col_pos    = 4.
        gs_lvc_fcat-coltext    = TEXT-f04.  "입고품번
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'BUDAT'.
        gs_lvc_fcat-col_pos    = 5.
        gs_lvc_fcat-coltext    = TEXT-f05.  "입고일자
        gs_lvc_fcat-fix_column = abap_true.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'MWSKZ'.
        gs_lvc_fcat-col_pos    = 6.
        gs_lvc_fcat-coltext    = TEXT-f37.  "구매오더세금코드
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'CHARG'.
        gs_lvc_fcat-col_pos    = 11.
        gs_lvc_fcat-coltext    = TEXT-f36.  "배치
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'MENGE'.
        gs_lvc_fcat-col_pos    = 12.
        gs_lvc_fcat-coltext    = TEXT-f06.  "수량
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'MEINS'.
        gs_lvc_fcat-col_pos    = 13.
        gs_lvc_fcat-coltext    = TEXT-f07.  "단위
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'TAXM1_T'.
        gs_lvc_fcat-col_pos    = 21.
        gs_lvc_fcat-coltext    = TEXT-f14.  "세금지시자
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'DMBTR_KRW'.
        gs_lvc_fcat-col_pos    = 22.
        gs_lvc_fcat-coltext    = TEXT-f31.  "입고금액(KRW)
        gs_lvc_fcat-currency   = gc_krw.
        gs_lvc_fcat-cfieldname = ''.
        gs_lvc_fcat-tech       = lv_flag.
        gs_lvc_fcat-outputlen  = 15.

      WHEN 'DMBTR_KRW_NEW'.
        gs_lvc_fcat-col_pos    = 23.
        gs_lvc_fcat-coltext    = TEXT-f31.  "입고금액(KRW)
        gs_lvc_fcat-currency   = gc_krw.
        gs_lvc_fcat-cfieldname = ''.

        IF lv_flag4 EQ space.
          gs_lvc_fcat-tech     = abap_true.
        ELSE.
          gs_lvc_fcat-edit     = abap_true.
        ENDIF.
        gs_lvc_fcat-outputlen  = 15.

      WHEN 'WRBTR'.
        gs_lvc_fcat-col_pos    = 24.
        gs_lvc_fcat-coltext    = TEXT-f32.  "입고금액(전표)
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 15.

      WHEN 'DMBTR'.
        gs_lvc_fcat-col_pos    = 25.
        gs_lvc_fcat-coltext    = lv_text.   "입고금액/월말단가확정금액
        gs_lvc_fcat-outputlen  = 16.

      WHEN 'WAERS'.
        gs_lvc_fcat-col_pos    = 26.
        gs_lvc_fcat-coltext    = TEXT-f09.  "통화
        gs_lvc_fcat-outputlen  = 5.

      WHEN 'EBELN'.
        gs_lvc_fcat-col_pos    = 31.
        gs_lvc_fcat-coltext    = TEXT-f10.  "구매오더번호
        gs_lvc_fcat-emphasize  = gc_c111.
        gs_lvc_fcat-hotspot    = abap_true.
        gs_lvc_fcat-outputlen  = 12.

      WHEN 'EBELP'.
        gs_lvc_fcat-col_pos    = 32.
        gs_lvc_fcat-coltext    = TEXT-f11.  "구매오더품번
        gs_lvc_fcat-outputlen  = 12.

      WHEN 'MATNR'.
        gs_lvc_fcat-col_pos    = 33.
        gs_lvc_fcat-coltext    = TEXT-f16.  "자재번호
        gs_lvc_fcat-outputlen  = 18.

      WHEN 'MATNR_TX'.
        gs_lvc_fcat-col_pos    = 34.
        gs_lvc_fcat-coltext    = TEXT-f55.  "자재내역
        gs_lvc_fcat-outputlen  = 20.

      WHEN 'WERKS'.
        gs_lvc_fcat-col_pos    = 35.
        gs_lvc_fcat-coltext    = TEXT-f17.  "플랜트
        gs_lvc_fcat-outputlen  = 6.

      WHEN 'WERKS_TX'.
        gs_lvc_fcat-col_pos    = 36.
        gs_lvc_fcat-coltext    = TEXT-f56.  "플랜트내역
        gs_lvc_fcat-outputlen  = 20.

      WHEN 'XBLNR'.
        gs_lvc_fcat-col_pos    = 37.
        gs_lvc_fcat-coltext    = TEXT-f81.  "납품서 번호
        gs_lvc_fcat-outputlen  = 10.
        gs_lvc_fcat-hotspot    = abap_true.

      WHEN 'EKORG'.
        gs_lvc_fcat-col_pos    = 41.
        gs_lvc_fcat-coltext    = TEXT-f18.  "구매조직
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 4.

      WHEN 'PSTYP'.
        gs_lvc_fcat-col_pos    = 42.
        gs_lvc_fcat-coltext    = TEXT-f19.  "정보범주
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 4.

      WHEN 'INFNR'.
        gs_lvc_fcat-col_pos    = 43.
        gs_lvc_fcat-coltext    = TEXT-f20.  "정보레코드
        gs_lvc_fcat-hotspot    = abap_true.
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 10.

      WHEN 'KBETR'.
        gs_lvc_fcat-col_pos    = 44.
        gs_lvc_fcat-coltext    = TEXT-f33.  "단가(정보레코드)
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 16.

      WHEN 'KONWA'.
        gs_lvc_fcat-col_pos    = 45.
        gs_lvc_fcat-coltext    = TEXT-f34.  "통화(정보레코드)
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 16.

      WHEN 'ACTEX'.
        gs_lvc_fcat-col_pos    = 46.
        gs_lvc_fcat-coltext    = TEXT-f78.  "실비
        IF p_bukrs NE gc_bukrs_1101.
          gs_lvc_fcat-tech = 'X'.
        ENDIF.
        gs_lvc_fcat-outputlen  = 16.

      WHEN 'KPEIN'.
        gs_lvc_fcat-col_pos    = 47.
        gs_lvc_fcat-coltext    = TEXT-f35.  "가격결정단위(정보레코드)
        gs_lvc_fcat-tech       = lv_flag2.
        gs_lvc_fcat-outputlen  = 25.

      WHEN 'MATKL'.
        gs_lvc_fcat-col_pos    = 48.
        gs_lvc_fcat-coltext    = TEXT-f76.  "자재그룹
        gs_lvc_fcat-outputlen  = 8.
        gs_lvc_fcat-no_out     = abap_true.

      WHEN 'KOKRS'.
        gs_lvc_fcat-col_pos    = 49.
        gs_lvc_fcat-coltext    = TEXT-f38.  "관리회계영역
        gs_lvc_fcat-no_out     = abap_true.
        gs_lvc_fcat-outputlen  = 12.

*> CODE INSPECTON 때문에 분리..
      WHEN OTHERS.
        PERFORM build_fieldcat_head_etc USING lv_flag
                                               lv_flag2
                                         CHANGING gs_lvc_fcat.
    ENDCASE.

    gs_lvc_fcat-scrtext_s =
    gs_lvc_fcat-scrtext_m =
    gs_lvc_fcat-scrtext_l = gs_lvc_fcat-reptext = gs_lvc_fcat-coltext.
    MODIFY ct_fcat FROM gs_lvc_fcat.
    CLEAR gs_lvc_fcat.

  ENDLOOP.

ENDFORM. " BUILD_FIELDCAT_HEAD
*&---------------------------------------------------------------------*
*&      FORM  BUILD_TOOLBAR_HEAD
*&---------------------------------------------------------------------*
*       툴바의 버튼중 나타내지 않을 버튼을 지정한다
*----------------------------------------------------------------------*
FORM build_toolbar_head TABLES et_uifunc TYPE ui_functions.

  PERFORM append_exclude_functions
        TABLES  et_uifunc
         USING : cl_gui_alv_grid=>mc_fc_loc_undo,
                 cl_gui_alv_grid=>mc_fc_back_classic,
                 cl_gui_alv_grid=>mc_fc_call_chain,
                 cl_gui_alv_grid=>mc_fc_call_crbatch,
                 cl_gui_alv_grid=>mc_fc_call_crweb,
                 cl_gui_alv_grid=>mc_fc_call_lineitems,
                 cl_gui_alv_grid=>mc_fc_call_master_data,
                 cl_gui_alv_grid=>mc_fc_call_more,
                 cl_gui_alv_grid=>mc_fc_call_report,
                 cl_gui_alv_grid=>mc_fc_call_xint,
                 cl_gui_alv_grid=>mc_fc_data_save,
                 cl_gui_alv_grid=>mc_fc_expcrdata,
                 cl_gui_alv_grid=>mc_fc_expcrdesig,
                 cl_gui_alv_grid=>mc_fc_expcrtempl,
                 cl_gui_alv_grid=>mc_fc_expmdb,
                 cl_gui_alv_grid=>mc_fc_extend,
                 cl_gui_alv_grid=>mc_fc_f4,
                 cl_gui_alv_grid=>mc_fc_find,
                 cl_gui_alv_grid=>mc_fc_help,
                 cl_gui_alv_grid=>mc_fc_info,
                 cl_gui_alv_grid=>mc_fc_loc_copy,          " 행 카피
                cl_gui_alv_grid=>mc_fc_loc_copy_row,      " 행 카피
                cl_gui_alv_grid=>mc_fc_loc_cut,           " 가위
                cl_gui_alv_grid=>mc_fc_loc_delete_row,    " 행삭제
                cl_gui_alv_grid=>mc_fc_loc_insert_row,    " 행삽입
                cl_gui_alv_grid=>mc_fc_loc_move_row,
                 cl_gui_alv_grid=>mc_fc_loc_append_row,    " 라인생성
                cl_gui_alv_grid=>mc_fc_loc_paste,         " 겹쳐쓰기
                cl_gui_alv_grid=>mc_fc_loc_paste_new_row, " 겹쳐쓰기
                cl_gui_alv_grid=>mc_fc_maximum,
                 cl_gui_alv_grid=>mc_fc_minimum,
                 cl_gui_alv_grid=>mc_fc_reprep,
                 cl_gui_alv_grid=>mc_fc_send,
                 cl_gui_alv_grid=>mc_fc_separator,
                 cl_gui_alv_grid=>mc_fc_to_rep_tree,
                 cl_gui_alv_grid=>mc_fc_delete_filter,
                 cl_gui_alv_grid=>mc_fc_refresh,
                 cl_gui_alv_grid=>mc_fc_check,
                 cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
                 cl_gui_alv_grid=>mc_fc_graph,
                 cl_gui_alv_grid=>mc_fc_to_office,
                 cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM. " BUILD_TOOLBAR_HEAD
*&---------------------------------------------------------------------*
*&      FORM  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM append_exclude_functions TABLES et_table
                              USING  iv_value.

  DATA lv_ui_func TYPE ui_func.

  lv_ui_func = iv_value.
  APPEND lv_ui_func TO et_table.

ENDFORM. " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      FORM  REFRESH_ALV
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM refresh_alv USING irf_grid TYPE REF TO cl_gui_alv_grid .

  TRY.
      irf_grid->refresh_table_display( is_stable = gc_stable ).
    CATCH cx_root.
  ENDTRY.

ENDFORM. " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_hotspot_click USING iv_e_row TYPE lvc_s_row
                                iv_e_column  TYPE lvc_s_col
                                is_row_no TYPE lvc_s_roid
                                irf_sender    TYPE REF TO cl_gui_alv_grid.

  CASE irf_sender.
    WHEN gv_grid_left.
      READ TABLE gt_left INTO gs_left INDEX iv_e_row.
      CHECK sy-subrc EQ 0.

    WHEN gv_grid_head.
      READ TABLE gt_head INTO gs_head INDEX iv_e_row.
      CHECK sy-subrc EQ 0.
      PERFORM link_to_transaction_head USING iv_e_column-fieldname.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_data_changed
     USING irf_data_changed TYPE REF TO cl_alv_changed_data_protocol
           irf_sender        TYPE REF TO cl_gui_alv_grid..

  DATA ls_mod_cells  TYPE lvc_s_modi.
  DATA lv_belnr2     LIKE rbkp-belnr.
  DATA lv_cbox(1)    TYPE c.

  LOOP AT irf_data_changed->mt_good_cells INTO ls_mod_cells.

    CASE ls_mod_cells-fieldname.
      WHEN 'CBOX'.

        CALL METHOD irf_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = lv_cbox.

        IF lv_cbox EQ abap_true.
          CALL METHOD irf_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_mod_cells-row_id
              i_fieldname = 'BELNR2'
            IMPORTING
              e_value     = lv_belnr2.

*         전표생성된 경우/작성일자 없는 경우 체크박스 선택을 해제한다.
          IF lv_belnr2       IS NOT INITIAL OR
              gs_scr100-bldat IS INITIAL.
            CALL METHOD irf_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_mod_cells-row_id
                i_fieldname = ls_mod_cells-fieldname
                i_value     = abap_false.
            EXIT.
          ENDIF.
        ENDIF.

      WHEN 'DMBTR_KRW'.  "입고금액(KRW)
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM. " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      FORM  EVNET_PBO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM evnet_pbo_0061 .

  LOOP AT SCREEN.

    CASE screen-group4.
      WHEN 'G4'.
*        IF GS_SCR100-WAERS2 EQ GV_LOCAL_CUR. "KRW 인 경우 환율적용 숨김
        IF p_r11 EQ abap_true. "원화인 경우 환율적용 숨김
          screen-active = 0.
        ENDIF.

      WHEN 'S4'.
        IF gs_scr100-zlsch NE 'K'.  "국책과제 아닌경우 거래은행 숨김
          screen-active = 0.
          CLEAR: gs_scr100-hbkid, gs_scr100-hktid.
        ENDIF.

      WHEN 'R12'.
        IF p_r11            EQ abap_true OR
           gs_scr100-waers2 EQ gv_local_cur.
          screen-active = 0.  "원화인 경우 총액 KRW환산값 숨김
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    CASE screen-name.
      WHEN 'GS_SCR100-DWPAYC' OR 'GS_SCR100-DWTAX' OR 'GS_SCR100-LABEL5'. "선급관련
        IF NOT ( p_rp2 = 'X' OR p_rp4 = 'X' ).   "물품선급과 공사용역일때만 출력
          screen-active = 0.
        ELSE.
          IF p_rp4 = 'X' AND gt_po_etc_amt[] IS INITIAL.  "공사용역 시 선급비율이 존재하는 것만 출력.
            screen-active = 0.
          ENDIF.
        ENDIF.
      WHEN 'GS_SCR100-DELAYF' OR 'GS_SCR100-LABEL6'.  "지체상금 관련
        IF NOT p_rp4 = 'X'.
          screen-active = 0.  "공사용역일때만 출력
        ENDIF.
      WHEN 'GS_SCR100-PRTCH'. "인쇄교체비 관련
        IF NOT ( ( p_rp1 = 'X' OR p_rp3 = 'X' ) AND p_bukrs EQ gc_bukrs_1101 ).
          screen-active = 0.  "물품일반과 자율납품와 KT&G 만 출력
        ENDIF.

*U1> 2022.04.14 : 대체수취인 기능 추가
      WHEN 'GS_SCR100-EMPFK' OR 'GS_SCR100-EMPFK_TX'.
        IF gv_empfk_used IS INITIAL.
          screen-active = 0.  "대체수취인 적용가능할떄만 활성화
        ENDIF.

*U1> 2022.04.14 : 송장발행처 기능 추가
      WHEN 'GS_SCR100-DIFF_LIFNR' OR 'GS_SCR100-DIFF_LIFNR_TX'.
        IF gv_diff_lifnr_used IS INITIAL.
          screen-active = 0.  "송장발행처 적용가능할떄만 활성화
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM. " EVNET_PBO
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_ON_F4
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_on_f4 USING irf_sender TYPE REF TO cl_gui_alv_grid
                        iv_fieldname   TYPE lvc_fname
                        iv_fieldvalue  TYPE lvc_value
                        is_row_no     TYPE lvc_s_roid
                        irf_event_data TYPE REF TO cl_alv_event_data
                        it_bad_cells  TYPE lvc_t_modi
                        iv_display.

  CONSTANTS: lc_pstyp TYPE dynpread-fieldname VALUE 'PSTYP'.

  irf_event_data->m_event_handled = abap_true.

  CASE iv_fieldname.
    WHEN 'EPSTP'.
      CALL FUNCTION 'HELP_VALUES_EPSTP'
        EXPORTING
          program   = sy-repid
          dynnr     = sy-dynnr
          fieldname = lc_pstyp
        EXCEPTIONS
          OTHERS    = 2.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " HANDLE_ON_F4
*&---------------------------------------------------------------------*
*&      FORM  HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM handle_data_changed_finished
     USING iv_modified
           it_good_cells TYPE lvc_t_modi
           irf_sender      TYPE REF TO cl_gui_alv_grid.

  DATA ls_cells TYPE lvc_s_modi.

  CHECK it_good_cells IS NOT INITIAL.

  LOOP AT it_good_cells INTO ls_cells.
    CASE ls_cells-fieldname.
      WHEN 'CBOX'.

        gs_expand-h = abap_true.
        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = gc_calcamt.

        PERFORM refresh_alv USING irf_sender.

      WHEN 'DMBTR_KRW_NEW'.

        "공급가액
        CLEAR gs_scr100-dmbtr2.
        LOOP AT gt_head INTO gs_head WHERE cbox = abap_true.
          gs_scr100-dmbtr2 = gs_scr100-dmbtr2 + gs_head-dmbtr_krw_new.
        ENDLOOP.
        PERFORM set_dmbtr2 USING abap_true space.
        gs_scr100_back = gs_scr100.

        CALL METHOD cl_gui_cfw=>set_new_ok_code
          EXPORTING
            new_code = gc_change.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM. " HANDLE_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      FORM  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM start_of_selection .

  CLEAR: gt_head, gs_head,  gt_head_all, gt_left,
           gr_bsart,  gr_absgr,
           gr_vgabe,  gt_ekbe,
           gr_bupla,    gr_werks.

  CLEAR: gt_t001w, gt_prepaid.

  CLEAR  : gs_scr100, gs_scr100_back.

  PERFORM check_code_validation.

  PERFORM select_data USING space.

ENDFORM. " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      FORM  SELECT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM select_data USING iv_flag.

  DATA lt_head   LIKE TABLE OF gs_head.

  CASE iv_flag.
    WHEN space.

      PERFORM get_bsart_list.   "문서유형
      PERFORM get_bupla_list2.  "사업장
      PERFORM get_taxcode_list. "세금코드

*> 자율납품&위탁정산은 기본 역발행이므로 작성구분이 무의미하여 제외.
*     세금계산서 작성구분
      IF p_rp3 NE 'X'.
        IF p_r1 EQ abap_true.  "정발행
          _g_append3 gr_absgr 'I' gc_eq gc_absgr_01.
          _g_append3 gr_absgr 'I' gc_eq space.
        ENDIF.
        IF p_r2 EQ abap_true.  "역발행
          _g_append3 gr_absgr 'I' gc_eq gc_absgr_02.
        ENDIF.
      ENDIF.

*     트랜잭션유형
      _g_append3 gr_vgabe 'I' gc_eq '1'.
      _g_append3 gr_vgabe 'I' gc_eq '2'.

      gs_scr100-label1 = TEXT-t41. "공급가액(KRW)
      gs_scr100-label2 = TEXT-t42. "세액(KRW)
      gs_scr100-label3 = TEXT-t43. "총액(KRW)
      gs_scr100-label4 = TEXT-t44. "차이(KRW)
      gs_scr100-label5 = TEXT-t54. "선급반제금액(KRW)
      gs_scr100-label6 = TEXT-t55. "지체상금액액(KRW)

    WHEN OTHERS.
  ENDCASE.

*-구매오더 이력 조회
  TRY.
      PERFORM select_ekbe   TABLES lt_head.  "입고
      PERFORM get_ekkn_list TABLES lt_head.  "계정지정
      PERFORM merge_data_01 TABLES lt_head.
      PERFORM get_data_for_post.
    CATCH cx_root.
      "송장 처리 중 오류가 발생하였습니다.
      MESSAGE s053 WITH '송장 처리 중'(m00).
      LEAVE LIST-PROCESSING.
  ENDTRY.

  gt_head_all[] = gt_head[].
  CLEAR: gs_head, gt_head.

*  PERFORM check_receive_esero. "전자세금계산서 수령여부

ENDFORM. " SELECT_DATA
*&---------------------------------------------------------------------*
*&      FORM  SELECT_EKBE
*&---------------------------------------------------------------------*
*       구매오더 이력 조회: 입고
*----------------------------------------------------------------------*
FORM select_ekbe TABLES et_data STRUCTURE gs_head.

  DATA: lr_waers TYPE RANGE OF t001-waers,
        lr_land1 TYPE RANGE OF lfa1-land1.

  CONSTANTS: lc_kr(2)      TYPE c VALUE 'KR',
             lc_bsart_pkm2 TYPE ekko-bsart VALUE 'PKM2'.

  CLEAR gv_local_cur.

  SELECT SINGLE land1, waers
    FROM t001
   WHERE bukrs = @p_bukrs
    INTO (@gv_land1, @gv_local_cur).

  IF p_r11 = 'X'. "원화는 외화 구분없이 출력(PO 가 외화지만 원화로 처리하는 경우 존재)
*    LR_WAERS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = GV_LOCAL_CUR ) ).
    CLEAR lr_waers.
  ELSE.
    lr_waers = VALUE #( ( sign = 'I' option = 'NE' low = gv_local_cur ) ).
  ENDIF.

  "공사용역이 아닐때만 국내업체로 필터링.
  IF p_rp4 NE 'X'.
    lr_land1 = VALUE #( ( sign = 'I' option = 'EQ' low = gv_land1 ) ).
  ENDIF.

  SELECT e~lifnr,
         e~name1    AS lifnr_tx,
         e~land1 AS lifnr_land,
         a~belnr,
         a~buzei,
         a~gjahr,
         a~budat,
         a~menge,
         b~meins,
         a~wrbtr AS dmbtr,      "입고금액
        a~dmbtr AS dmbtr_krw,  "현지금액
        a~wrbtr,               "전표금액
        a~waers,
         a~ebeln,
         a~ebelp,
         a~vgabe,
         a~bwart,
         a~shkzg,
         a~xblnr,
         a~lfgja,
         a~lfbnr,
         a~lfpos,
         a~werks,
         a~matnr,
         c~ekorg,
         c~bsart,
         c~bstyp,
         c~dppct,
         c~late_rate,
         b~knttp,
         b~pstyp,
         b~mwskz,
         c~zterm,
         a~cpudt,
         a~cputm,
         e~stcd2,
         b~brtwr,
         b~lebre,
         b~bprme,
         b~netpr,
         b~peinh,
         a~charg,
         f~name1 AS werks_tx,
         b~txz01 AS matnr_tx,
         b~txz01,
         h~bismt,
         c~zorder_person,
         c~zorder_department,
         c~zexpen_person,
         c~zexpen_department,
         c~ekgrp,
         j~taxim,
         a~bwtar,
         c~dpamt,
         c~absgr,
         b~matkl
         FROM ekbe AS a INNER JOIN ekpo AS b
           ON a~ebeln = b~ebeln AND
              a~ebelp = b~ebelp
                        INNER JOIN ekko AS c
           ON b~ebeln = c~ebeln
                        INNER JOIN mseg AS k
           ON k~mblnr = a~belnr AND
              k~mjahr = a~gjahr AND
              k~zeile = a~buzei
                        INNER JOIN lfm1 AS d
           ON c~lifnr = d~lifnr AND
              c~ekorg = d~ekorg
                        INNER JOIN lfa1 AS e
           ON c~lifnr = e~lifnr
                        LEFT OUTER JOIN t001w AS f
           ON a~werks = f~werks
                        LEFT OUTER JOIN mara  AS h
           ON a~matnr = h~matnr
                        LEFT OUTER JOIN marc  AS i
           ON a~matnr = i~matnr AND
              a~werks = i~werks
                        LEFT OUTER JOIN mlan AS j
           ON j~matnr = b~matnr AND
              j~aland = @lc_kr
        WHERE c~bukrs   EQ @p_bukrs     "회사코드
         AND a~vgabe   IN @gr_vgabe    "트랜잭션유형
         AND a~ebeln   IN @s_ebeln     "구매오더 번호
         AND a~werks   IN @gr_werks    "플랜트
         AND c~lifnr   IN @s_lifnr     "업체
         AND c~ekorg   IN @s_ekorg     "구매조직
         AND c~ekgrp   IN @s_ekgrp     "구매그룹
         AND c~bsart   IN @gr_bsart    "구매문서 유형
         AND c~bstyp   IN @gr_bstyp    "문서범주
         AND c~dppct   IN @gr_dppct
         AND c~absgr   IN @gr_absgr    "정발행/역발행업체 구분
         AND e~stcd2   IN @s_stcd2     "사업자번호
         AND a~matnr   IN @s_matnr
         AND a~xblnr   IN @s_xblnr    "납품서 번호
         AND c~waers   IN @lr_waers
         AND b~matkl   IN @s_matkl
         AND j~taxim   IN @s_taxim
         AND c~bsart   IN @s_bsart
         AND k~smbln   = @space "취소 입고문서 제외
         AND ( ( a~belnr NOT IN @s_belnr  AND a~vgabe NE '1' )
                OR ( a~belnr IN @s_belnr ) )
         AND ( ( a~budat NOT IN @s_budat  AND a~vgabe NE '1' )
                OR ( a~budat IN @s_budat ) )
          AND b~loekz   NE 'L'         "삭제 아닌건
         AND b~pstyp   NE '2'        "위탁 아닌건
         AND ( c~zorder_person IN @s_ordps     "발주자
          OR c~zexpen_person IN @s_expps )     "지출발의자
         AND ( c~zorder_department IN @s_orddp     "발주부서
          OR c~zexpen_department IN @s_expdp )     "지출발의 부서
         AND NOT EXISTS ( SELECT mblnr
                           FROM mseg
                          WHERE sjahr = a~gjahr
                            AND smbln = a~belnr
                            AND smblp = a~buzei ) "취소가 된 입고문서 제외
    INTO CORRESPONDING FIELDS OF TABLE @et_data.

*> 발주자/ 지출발의자 둘중 하나라도 값이 있을 때 조건 만족시 출력
  DATA lv_exist TYPE c.

  LOOP AT et_data INTO DATA(ls_data).
    CLEAR lv_exist.

    "오더유형이 PMK2 이면 국가 관계없이 모두 출력.
    IF ls_data-bsart NE lc_bsart_pkm2 AND
       NOT ls_data-lifnr_land IN lr_land1.
      DELETE et_data.
      CONTINUE.
    ENDIF.

    IF s_ordps[] IS NOT INITIAL OR s_expps[] IS NOT INITIAL.
      IF ls_data-zorder_person IN s_ordps AND NOT s_ordps[] IS INITIAL.
        lv_exist = 'X'.
      ENDIF.

      IF ls_data-zexpen_person IN s_expps AND NOT s_expps[] IS INITIAL.
        lv_exist = 'X'.
      ENDIF.
    ENDIF.

    IF s_orddp[] IS NOT INITIAL OR s_expdp[] IS NOT INITIAL.
      IF ls_data-zorder_department IN s_orddp AND NOT s_orddp[] IS INITIAL.
        lv_exist = 'X'.
      ENDIF.

      IF ls_data-zexpen_department IN s_expdp AND NOT s_expdp[] IS INITIAL.
        lv_exist = 'X'.
      ENDIF.
    ENDIF.

    IF s_ordps[] IS INITIAL AND s_expps[] IS INITIAL AND
       s_orddp[] IS INITIAL AND s_expdp[] IS INITIAL.
      lv_exist = 'X'.
    ENDIF.

    IF lv_exist IS INITIAL.
      DELETE et_data.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM. " SELECT_EKBE
*&---------------------------------------------------------------------*
*&      FORM  MERGE_DATA_01
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM merge_data_01 TABLES it_data STRUCTURE gs_head.

  DATA: lt_tmp  LIKE TABLE OF gs_head,
        ls_data LIKE gs_head.
  DATA lv_rf_cxroot TYPE REF TO cx_root.
  DATA lv_tline     TYPE sytabix.
  DATA lv_tabix     TYPE sytabix.
  DATA lv_mesg      TYPE string.
  DATA lv_chk(2)    TYPE c.

  DATA: lt_del_data LIKE TABLE OF gs_head.

  DELETE it_data WHERE vgabe = '2'     "송장처리
                   OR bwart = gc_122   "반품
                   OR bwart = gc_123   "반품취소
                   OR bwart = gc_162.   "GR 반품취소


  SORT it_data BY ebeln ebelp cpudt cputm.
  PERFORM select_ekbe_with_ebeln TABLES it_data.

  lt_tmp[] = gt_ekbe[].

  SORT   lt_tmp    BY ebeln ebelp lfgja lfbnr lfpos.

  PERFORM get_user_name_list TABLES it_data.   "사용자명

*-구매문서 이력데이터 GR/IV 데이터를  + - 해서
* 입고대상 데이터를 발췌한다.
  LOOP AT it_data INTO ls_data.
    lv_tabix = sy-tabix.

*-  GR/IV 데이터 체크한다.
    PERFORM check_gr_iv TABLES lt_tmp USING ls_data.

    IF ls_data-menge = 0.
      APPEND ls_data TO lt_del_data.
    ELSE.
      MODIFY it_data FROM ls_data TRANSPORTING dmbtr_krw dmbtr wrbtr menge.
    ENDIF.
  ENDLOOP.

  SORT lt_del_data BY ebeln ebelp gjahr belnr buzei.
  LOOP AT it_data INTO ls_data.
    lv_tabix = sy-tabix.

    READ TABLE lt_del_data WITH KEY ebeln = ls_data-ebeln
                                    ebelp = ls_data-ebelp
                                    gjahr = ls_data-gjahr
                                    belnr = ls_data-belnr
                                    buzei = ls_data-buzei
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      DELETE it_data INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

  lv_tline = lines( it_data ).
  SORT it_data BY lifnr waers mwskz gjahr belnr.

*> 자재별 세금내역
*  SELECT taxim, taxib
*    FROM tmkm1t
*   WHERE spras = @sy-langu
*     AND land1 = @gv_land1
*    INTO TABLE @DATA(lt_tmkm1t).
*  SORT lt_tmkm1t BY taxim.

*> 실비 계산
*  DATA(lt_tmp2) = it_data[].
*  SORT lt_tmp2 BY gjahr belnr buzei.
*  DELETE ADJACENT DUPLICATES FROM lt_tmp2 COMPARING gjahr belnr buzei.
*  IF NOT lt_tmp2 IS INITIAL.
*    SELECT mjahr, mblnr, zeile, actex
*      FROM zsvcmm_realcost
*       FOR ALL ENTRIES IN @lt_tmp2
*     WHERE mjahr = @lt_tmp2-gjahr
*       AND mblnr = @lt_tmp2-belnr
*       AND zeile = @lt_tmp2-buzei
*      INTO TABLE @DATA(lt_zsvcmm_realcost).
*    FREE lt_tmp2.
*    SORT lt_zsvcmm_realcost BY mjahr mblnr zeile.
*  ENDIF.

*  SELECT SINGLE eml
*    FROM zsvmm_user_info
*   WHERE company = @p_bukrs
*     AND user_id = @sy-uname
*    INTO @DATA(lv_uname_email).

  CLEAR lv_tabix.
  LOOP AT it_data INTO ls_data.

    CLEAR lv_chk.

    ADD 1 TO lv_tabix.

    IF ls_data-knttp EQ 'A'.  "자산
      ls_data-dmbtr_krw = ls_data-dmbtr
                         = ls_data-wrbtr = ls_data-brtwr.
    ENDIF.

    IF ls_data-dmbtr_krw IS INITIAL AND
       ls_data-knttp NE 'A'."자산
      CONTINUE.
    ENDIF.

    gs_head = ls_data.

    IF gs_head-matnr_tx IS INITIAL.
      gs_head-matnr_tx = gs_head-txz01.  "내역
    ENDIF.

*-  계정지정 데이터
    CLEAR gs_ekkn.
    READ TABLE gt_ekkn INTO gs_ekkn WITH KEY ebeln = gs_head-ebeln
                                ebelp = gs_head-ebelp
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING gs_ekkn TO gs_head.
    ENDIF.

*-  발주자/부서
*    READ TABLE gt_usernm INTO DATA(ls_usernm)
*                         WITH KEY employ_no = gs_head-zorder_person
*                          BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      gs_head-zorder_person_name = ls_usernm-employ_name.
*      gs_head-zorder_department = ls_usernm-department.
*      gs_head-zorder_depart_name = ls_usernm-depart_name.
*    ENDIF.

*- 지출결의자/부서/메일주소
*    READ TABLE gt_usernm INTO ls_usernm
*                         WITH KEY employ_no = gs_head-zexpen_person
*                          BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      gs_head-zexpen_person_name = ls_usernm-employ_name.
*      gs_head-zexpen_department = ls_usernm-department.
*      gs_head-zexpen_depart_name = ls_usernm-depart_name.
**      GS_HEAD-ZEXPEN_EML = LS_USERNM-EML.
*    ENDIF.

*> EMAIL 주소는 LOGIN 유저의 MAIL 주소로 설정
*    gs_head-zexpen_eml = lv_uname_email.

*- 의제매입세 계산
*    IF gs_head-taxim EQ 'Y'.
*      gs_head-svat_amt = ( gs_head-wrbtr * 2 ) / 102.
*    ENDIF.
*
*    READ TABLE lt_tmkm1t INTO DATA(ls_tmkm1t)
*                         WITH KEY taxim = gs_head-taxim
*                         BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      gs_head-taxm1_t = ls_tmkm1t-taxib.
*    ENDIF.

*- 실비 계산
*    IF p_bukrs = gc_bukrs_1101.
*      READ TABLE lt_zsvcmm_realcost INTO DATA(ls_zsvcmm_realcost)
*                                    WITH KEY mjahr = gs_head-gjahr
*                                             mblnr = gs_head-belnr
*                                             zeile = gs_head-buzei
*                                    BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        gs_head-actex = ls_zsvcmm_realcost-actex.
*        gs_head-dmbtr = gs_head-wrbtr + gs_head-actex.
*
*        ls_data-dmbtr = gs_head-dmbtr.
*      ENDIF.
*    ENDIF.

*    PERFORM p5_calculation.

    APPEND gs_head TO gt_head.

*-  SUM(업체+통화)
    MOVE-CORRESPONDING ls_data TO gs_left.

    CLEAR gs_left-mwskz.
    TRY.
        COLLECT gs_left INTO gt_left.
      CATCH cx_root INTO lv_rf_cxroot.
        lv_mesg = lv_rf_cxroot->get_text( ).
        MESSAGE s054 DISPLAY LIKE 'E' WITH lv_mesg.
        LEAVE LIST-PROCESSING.
    ENDTRY.
  ENDLOOP.


ENDFORM. " MERGE_DATA_01
*&---------------------------------------------------------------------*
*&      FORM  END_OF_SELECTION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM end_of_selection .

  IF gt_head_all[] IS INITIAL.
    MESSAGE s005.  "데이터가 존재하지 않습니다.
    EXIT.
  ENDIF.


* 탭제목 설정
  CASE abap_true.
    WHEN p_rp1.
      gv_tab1_title = TEXT-t12. "물품일반
    WHEN p_rp2.
      gv_tab1_title = TEXT-t14. "물품선급
    WHEN p_rp3.
      gv_tab1_title = TEXT-t15. "자율납품&위탁정산
    WHEN p_rp4.
      gv_tab1_title = TEXT-t13. "공사용역
    WHEN OTHERS.
  ENDCASE.

  APPEND gc_issue_r TO gt_ui_func.
  APPEND gc_issue   TO gt_ui_func.

  IF NOT ( p_rp2 = 'X' OR p_rp4 = 'X' ).
    APPEND gc_dwpay_log TO gt_ui_func.
  ENDIF.

  IF p_rp1 NE 'X'.
    APPEND gc_chg_kalsk TO gt_ui_func.
  ENDIF.

* 전자세금계산서 임시생성 숨김.
  zcl_mm_common=>common_config(
   EXPORTING  is_common =  VALUE #( m = 'A1' d = 'A1010' s = 'AB200' )
                       it_where = VALUE #( ( field = 1 value = sy-cprog )
                                           ( field = 2 value = 'Y' ) )
    IMPORTING et_outtab = DATA(lt_config) ).
  IF lt_config[] IS INITIAL. "CRT_EACC"
    APPEND gc_crt_eacc TO gt_ui_func.
  ENDIF.

  gs_scr100-bukrs = p_bukrs.
  gs_scr100-budat = sy-datum.
  gs_scr100-wwert  = sy-datum.

  PERFORM set_signal_status USING    space
                            CHANGING gs_scr100-status.
  PERFORM create_left_alv.
  CALL SCREEN 0001.

ENDFORM. " END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      FORM  EVENT_HANDLER_REGISTER_HEAD
*&---------------------------------------------------------------------*
*       ALV 이벤트
*----------------------------------------------------------------------*
FORM event_handler_register_head.

  CREATE OBJECT gv_grid_event_receiver.

  SET HANDLER :
      gv_grid_event_receiver->handle_toolbar          FOR gv_grid_head,
      gv_grid_event_receiver->handle_user_command     FOR gv_grid_head,
      gv_grid_event_receiver->handle_hotspot_click    FOR gv_grid_head,
      gv_grid_event_receiver->handle_double_click     FOR gv_grid_head,
      gv_grid_event_receiver->handle_data_changed     FOR gv_grid_head,
      gv_grid_event_receiver->handle_data_changed_finished
                                                      FOR gv_grid_head.

  CALL METHOD gv_grid_head->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gv_grid_head->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.


ENDFORM. " EVENT_HANDLER_REGISTER_HEAD
*&---------------------------------------------------------------------*
*&      FORM  BUILD_FIELDCAT_MERGE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_fieldcat_merge TABLES et_fieldcat TYPE lvc_t_fcat
                           USING  iv_struc     TYPE c.


  DATA lt_fieldcat TYPE kkblo_t_fieldcat.
  DATA lv_date     LIKE sy-datum.

  lv_date = sy-datum + 1.
  SET PARAMETER ID 'ALVBUFFER' FIELD lv_date.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      i_tabname              = iv_struc
      i_callback_program     = sy-repid
      i_inclname             = sy-repid
      i_bypassing_buffer     = abap_true
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      OTHERS                 = 2.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lt_fieldcat
    IMPORTING
      et_fieldcat_lvc   = et_fieldcat[]
    EXCEPTIONS
      it_data_missing   = 1.

ENDFORM. " BUILD_FIELDCAT_MERGE
*&---------------------------------------------------------------------*
*&      FORM  CHECK_CODE_VALIDATION
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM check_code_validation.


  PERFORM get_bupla_list.   "사업장
  PERFORM get_plant_list.   "플랜트

*> 회사코드 별 설정.
  CLEAR: gv_bukrs_code, gr_bankn.
  CASE p_bukrs.
    WHEN gc_bukrs_1101.
      gv_bukrs_code = gc_code_ktg.
    WHEN gc_bukrs_2101.
      gv_bukrs_code = gc_code_kgc.
    WHEN gc_bukrs_3101.
      gv_bukrs_code = gc_code_yjp.
    WHEN OTHERS.
  ENDCASE.

  IF NOT gv_bukrs_code IS INITIAL.
    APPEND VALUE #( sign = 'I' option = 'CP' low = gv_bukrs_code && '*' ) TO gr_bankn.
  ENDIF.

*> 공통 IMG 에 의한 점검 (금액 상하선, 은행계좌 필수, 입고송장월 차이 점검...)
  CLEAR gs_config_e1013.
  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'E1' d = 'E1010' s = 'E1013' )
                                     it_where = VALUE #(
                                                         ( field = 1 value = p_bukrs )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).

  READ TABLE lt_config INTO DATA(ls_config_e1013) INDEX 1.
  MOVE-CORRESPONDING ls_config_e1013 TO gs_config_e1013.

ENDFORM. " CHECK_CODE_VALIDATION
*&---------------------------------------------------------------------*
*&      FORM  LINK_TO_TRANSACTION_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM link_to_transaction_head USING iv_fieldname.

*  DATA LT_ERROR TYPE TABLE OF BAPIRET2.

  CASE iv_fieldname.
    WHEN 'BELNR'.
      CHECK gs_head-belnr IS NOT INITIAL.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_mblnr             = gs_head-belnr
          i_mjahr             = gs_head-gjahr
        EXCEPTIONS
          illegal_combination = 1
          OTHERS              = 2.

    WHEN 'EBELN'.
      CHECK gs_head-ebeln IS NOT INITIAL.
      IF gs_head-bstyp EQ 'L'.
        SET PARAMETER ID 'SAG' FIELD gs_head-ebeln.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
      ELSE.
        SET PARAMETER ID 'BES' FIELD gs_head-ebeln.
        CALL TRANSACTION 'ME23N'.
      ENDIF.

    WHEN 'MATNR'.
      CHECK gs_head-matnr IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD gs_head-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'LIFNR'.
      CHECK gs_head-lifnr IS NOT INITIAL.

      SET PARAMETER ID 'LIF' FIELD gs_head-lifnr.
      SET PARAMETER ID 'BUK' FIELD p_bukrs.
      CALL TRANSACTION 'BP' AND SKIP FIRST SCREEN.


    WHEN 'INFNR'.
      CHECK gs_head-infnr IS NOT INITIAL.
      SET PARAMETER ID 'LIF' FIELD gs_head-lifnr.
      SET PARAMETER ID 'MAT' FIELD gs_head-matnr.
      SET PARAMETER ID 'EKO' FIELD gs_head-ekorg.
      SET PARAMETER ID 'WRK' FIELD gs_head-werks.
      SET PARAMETER ID 'INF' FIELD gs_head-infnr.
      SET PARAMETER ID 'ESO' FIELD gs_head-pstyp.

      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

    WHEN 'BELNR2'.
      CHECK gs_head-belnr2 IS NOT INITIAL.
      SET PARAMETER ID 'RBN' FIELD gs_head-belnr2.
      SET PARAMETER ID 'GJR' FIELD gs_head-gjahr2.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN 'XBLNR'.
      CHECK gs_head-xblnr IS NOT INITIAL.
      SET PARAMETER ID 'VLM' FIELD gs_head-xblnr.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM. " LINK_TO_TRANSACTION_HEAD
*&---------------------------------------------------------------------*
*&      FORM  SET_SIGNAL_STATUS
*&---------------------------------------------------------------------*
*       상태에 따른 신호등 설정
*----------------------------------------------------------------------*
FORM set_signal_status USING iv_flag
                        CHANGING cv_status.

  CASE iv_flag.
    WHEN 'E'.     "에러
      cv_status = icon_red_light.

    WHEN 'S'.     "성공
      cv_status = icon_green_light.

    WHEN OTHERS.  "대기
      cv_status = icon_light_out.
  ENDCASE.

ENDFORM. " SET_SIGNAL_STATUS
*&---------------------------------------------------------------------*
*&      FORM  LINK_TO_BUTTON_ACTION_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM link_to_button_action_head USING iv_ucomm.

  DATA lv_cnt   TYPE sytabix.
  DATA lv_ebeln TYPE ebeln.
  DATA lv_ebelp TYPE ebelp.

  CASE iv_ucomm.
    WHEN 'SALL'.     "동일업체선택

      READ TABLE gt_head INTO gs_head WITH KEY cbox = abap_true.
      IF sy-subrc NE 0.
        MESSAGE s006.  "6 선택된 데이타가 없습니다.
        EXIT.
      ENDIF.
      IF gs_head-belnr2 IS NOT INITIAL.
        "송장생성 된 품목이 존재합니다.
        MESSAGE s043 WITH '송장생성 된 품목'(m18).
        EXIT.
      ENDIF.

      gs_head-cbox = abap_true.
      MODIFY gt_head FROM gs_head TRANSPORTING cbox WHERE cbox   EQ space
                                         AND lifnr  EQ gs_head-lifnr
                                         AND waers  EQ gs_head-waers
                                         AND belnr2 EQ space.

      PERFORM calculate_amount.

    WHEN 'DALL'.     "전체해제
      gs_head-cbox = space.
      MODIFY gt_head FROM gs_head TRANSPORTING cbox WHERE cbox EQ abap_true.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = gc_clear.

    WHEN 'DALL_S'.     "전체선택
      gs_head-cbox = 'X'.
      MODIFY gt_head FROM gs_head TRANSPORTING cbox WHERE cbox EQ space.

      PERFORM calculate_amount.

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = gc_enter.

    WHEN 'ALLC'.     "업체전체조회
      gt_head[] = gt_head_all[].

      CALL METHOD cl_gui_cfw=>set_new_ok_code
        EXPORTING
          new_code = gc_clear.

    WHEN 'HIST'.     "구매문서이력

      LOOP AT gt_head INTO gs_head WHERE cbox = abap_true.
        ADD 1 TO lv_cnt.
        IF lv_cnt EQ 2.
          EXIT.
        ENDIF.

        lv_ebeln = gs_head-ebeln.
        lv_ebelp = gs_head-ebelp.
      ENDLOOP.
      IF lv_cnt LT 1.
        MESSAGE s006 DISPLAY LIKE 'E'.   "선택된 데이타가 없습니다.
        EXIT.
      ELSEIF lv_cnt GT 1.
        MESSAGE s007 DISPLAY LIKE 'E'.   "1 건의 데이타만 선택 가능 합니다.
        EXIT.
      ENDIF.

      PERFORM show_ekbe_list USING lv_ebeln lv_ebelp.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

  PERFORM refresh_alv USING gv_grid_head.


ENDFORM. " LINK_TO_BUTTON_ACTION_HEAD
*&---------------------------------------------------------------------*
*&      FORM  CHECK_CONDITION
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM check_condition USING iv_chk.

  CONSTANTS: lc_bsart_pkm2 TYPE ekko-bsart VALUE 'PKM2'.

  DATA: BEGIN OF ls_ebeln,
          ebeln LIKE ekbe-ebeln,
        END OF ls_ebeln,
        lt_ebeln LIKE TABLE OF ls_ebeln.

  DATA: BEGIN OF ls_mck_taxim,
          taxim TYPE mlan-taxim,
        END OF ls_mck_taxim,
        lt_mck_taxim LIKE TABLE OF ls_mck_taxim.

  DATA: BEGIN OF ls_bsart,
          bsart TYPE ekko-bsart,
        END OF ls_bsart,
        lt_bsart LIKE TABLE OF ls_bsart.

  DATA: lv_retpo       TYPE c,
        lv_sel_line    TYPE i,
        lv_belnr_exist TYPE c.

  CLEAR: gs_post_split, gv_diff_gr_month.

  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'E1' d = 'E1010' s = 'E1015' )
                                     it_where = VALUE #(
                                                         ( field = 1 value = p_bukrs )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).
  SORT lt_config BY field2.

  CLEAR: lv_belnr_exist.

  LOOP AT gt_head INTO gs_head WHERE cbox EQ abap_true.

    ADD 1 TO lv_sel_line.

    ls_ebeln-ebeln = gs_head-ebeln.
    COLLECT ls_ebeln INTO lt_ebeln.

    ls_mck_taxim-taxim = gs_head-taxim.
    COLLECT ls_mck_taxim INTO lt_mck_taxim.

    IF gs_head-bsart = lc_bsart_pkm2.
      lv_retpo = 'X'.
    ENDIF.

    ls_bsart-bsart = gs_head-bsart.
    COLLECT ls_bsart INTO lt_bsart.

    READ TABLE lt_config INTO DATA(ls_config)
                         WITH KEY field2 = gs_head-bsart
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_post_split-use = 'X'.
      gs_post_split-base_line = ls_config-field3.
    ENDIF.

    IF NOT gs_head-belnr2 IS INITIAL.
      lv_belnr_exist = 'X'.
    ENDIF.

*U4> 입고전기월과 송장전기월이 다를때 메시지 처리하기 위함.
    IF gs_head-budat(6) NE gs_scr100-budat(6).
      gv_diff_gr_month = 'X'.
    ENDIF.
  ENDLOOP.

  IF lv_sel_line EQ 0.
    MESSAGE s006 DISPLAY LIKE 'E'.     "선택된 데이타가 없습니다.
    iv_chk = abap_true.
    EXIT.
  ENDIF.

  IF NOT lv_belnr_exist IS INITIAL.
    MESSAGE s043 WITH '이미 생성된 송장문서'(m54) DISPLAY LIKE 'E'.
    iv_chk = abap_true.
    EXIT.
  ENDIF.

  "선택된 라인이 SPLIT 기준 초과 시 전표 분할
  IF gs_post_split-use = 'X' AND lv_sel_line > gs_post_split-base_line.
    gs_post_split-sel_line = lv_sel_line.
  ELSE.
    CLEAR gs_post_split.
  ENDIF.

  "선급금 또는 공사용역이면 PO 별로 진행해야 함.
  IF ( p_rp2 = 'X' OR p_rp4 = 'X' ) AND lines( lt_ebeln ) > 1.
    MESSAGE s049 WITH '구매오더번호'(f10) DISPLAY LIKE 'E'.
    iv_chk = abap_true.
    EXIT.
  ENDIF.

  "반품은 다른 오더 유형과 같은 전표 처리 불가.
  IF lv_retpo = 'X' AND lines( lt_bsart ) > 1.
    MESSAGE s131 DISPLAY LIKE 'E'.
    iv_chk = abap_true.
    EXIT.
  ENDIF.

  PERFORM check_condition_header CHANGING iv_chk.

  IF lines( lt_mck_taxim ) > 1.
    MESSAGE w049 WITH '세금지시자'(m39) DISPLAY LIKE 'E'.
  ENDIF.

  PERFORM check_condition_by_img CHANGING iv_chk.

  CHECK iv_chk IS INITIAL.

*> CODE INS 떄문에 분리..
  PERFORM check_condition_by_etc CHANGING iv_chk.

ENDFORM. " CHECK_CONDITION
*&---------------------------------------------------------------------*
*&      FORM  BUILD_ALV_HEAD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM build_alv_head .

  DATA: lt_fcat TYPE lvc_t_fcat,
        lt_excl TYPE ui_functions,
        ls_layo TYPE lvc_s_layo.

  CHECK gv_grid_head IS NOT BOUND.

  CREATE OBJECT gv_grid_head
    EXPORTING
      i_parent = gv_custom_container_head.

  PERFORM build_layout_head   CHANGING ls_layo.
  PERFORM build_fieldcat_head CHANGING lt_fcat.
  PERFORM build_toolbar_head  TABLES   lt_excl.
  PERFORM event_handler_register_head.


  CALL METHOD gv_grid_head->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layo
      is_variant           = gs_variant
      i_save               = 'U'
      it_toolbar_excluding = lt_excl
    CHANGING
      it_outtab            = gt_head[]
      it_fieldcatalog      = lt_fcat.


ENDFORM. " BUILD_ALV_HEAD
*&---------------------------------------------------------------------*
*&      FORM  CALCULATE_AMOUNT
*&---------------------------------------------------------------------*
*       금액을 SUM 하여 화면 상단에 보여준다.
*
*       화면상단 입고기준 공급가액/공급가는 원화외화 상관없이
*       현지통화 입고금액(GS_HEAD-DMBTR_KRW)을 보여준다.
*
*       단, 외화인 경우 환율을 적용하면
*       입고기준 공급가액(GS_HEAD-DMBTR_KRW)은 그대로 두고,
*       공급가필드는 GS_HEAD-DMBTR(입고금액)에 환율을 적용해서 보여준다.
*
*       월말단가확정 또는 PLANT BATCH 체크로직을 타는 경우에는
*       외화인 경우에는 위와 동일하고,
*       원화인 경우에는 GS_HEAD-DMBTR(입고금액) 을 적용한다.
*
*       GS_SCR100-WAERS 통화는 화면 상단 금액관련 필드에 적용한다.
*----------------------------------------------------------------------*
FORM calculate_amount.

  DATA lv_rf_cxroot  TYPE REF TO cx_root.
  DATA lv_mesg       TYPE string.
  DATA lv_error(1)   TYPE c.
  DATA lv_dmbtr      LIKE ekbe-wrbtr.
  DATA lv_chk(1)     TYPE c.
  DATA lv_text       TYPE string.
  DATA lv_tabix      TYPE sytabix.

  DATA: BEGIN OF ls_ebeln,
          ebeln LIKE ekko-ebeln,
        END OF ls_ebeln,
        lt_ebeln      LIKE TABLE OF ls_ebeln,
        lt_ebeln_loop LIKE TABLE OF ls_ebeln.

  PERFORM clear_screen_amount.

  PERFORM check_item_data_sum USING lv_error.

  CHECK lv_error IS INITIAL.

  LOOP AT gt_head INTO gs_head WHERE cbox = abap_true.

    ADD 1 TO lv_tabix.

    IF p_r12    EQ abap_true AND   "외화
      lv_tabix EQ 1.
      PERFORM set_label USING gs_head-waers.
    ENDIF.


    "선금체크
    IF lv_chk IS INITIAL.
      PERFORM check_prepaid_amount USING gs_head-ebeln gs_head-ebelp
                                   CHANGING lv_chk lv_text.
    ENDIF.

    "업체
    IF gs_scr100-lifnr NE gs_head-lifnr.
      gs_scr100-lifnr = gs_head-lifnr.
    ENDIF.

    "사업자번호
    IF gs_scr100-stceg NE gs_head-stcd2.
      gs_scr100-stceg = gs_head-stcd2.
    ENDIF.

    IF gs_scr100-mwskz NE gs_head-mwskz.
      gs_scr100-mwskz = gs_head-mwskz.   "세금코드
    ENDIF.

    "지출결의 담당자 EMAIL
*    IF gs_scr100-zexpen_eml NE gs_head-zexpen_eml.
*      gs_scr100-zexpen_eml = gs_head-zexpen_eml.
*    ENDIF.

    IF p_r11 EQ abap_true.  "원화
      gs_scr100-waers = gc_krw.
    ELSE.
      gs_scr100-waers = gs_head-waers.
    ENDIF.
    gs_scr100-waers2  = gs_head-waers.

    "지급조건
    IF gs_scr100-zterm NE gs_head-zterm.
      gs_scr100-zterm = gs_head-zterm.
    ENDIF.

    PERFORM adjust_amount USING    gs_head
                          CHANGING lv_dmbtr.

    ls_ebeln-ebeln = gs_head-ebeln.
    APPEND ls_ebeln TO lt_ebeln.

    TRY.
        gs_scr100-dmbtr2 = gs_scr100-dmbtr
                         = gs_scr100-dmbtr + lv_dmbtr.
      CATCH cx_root INTO lv_rf_cxroot.
        lv_mesg = lv_rf_cxroot->get_text( ).
        MESSAGE s054 DISPLAY LIKE 'E' WITH lv_mesg.
        lv_error = abap_true.
        EXIT.
    ENDTRY.

  ENDLOOP.

  IF lv_error EQ abap_true.
    PERFORM clear_screen_amount.
    EXIT.
  ENDIF.

*> 선급금 비율/금액 기본 값 설정.
  SORT lt_ebeln BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_ebeln COMPARING ebeln.
  lt_ebeln_loop[] = lt_ebeln[].
  IF NOT lt_ebeln[] IS INITIAL.

*> PO 의 선급금 및 선급비율
    SELECT a~ebeln, a~dppct, a~late_rate,
           b~downpay_req,
           b~downpay_amount, b~downpay_tax,
           b~downpay_clear, b~clear_tax
      FROM ekko AS a LEFT OUTER JOIN zsvcmm_downpay AS b
        ON a~ebeln = b~purchaseorder
       FOR ALL ENTRIES IN @lt_ebeln
     WHERE ebeln EQ @lt_ebeln-ebeln
       AND dppct NE @space
      INTO CORRESPONDING FIELDS OF TABLE @gt_po_etc_amt.
    FREE lt_ebeln.
    SORT gt_po_etc_amt BY ebeln.

  ENDIF.


  DATA lv_delayf TYPE ekbe-wrbtr.

  LOOP AT lt_ebeln_loop INTO ls_ebeln.
    READ TABLE gt_po_etc_amt INTO gs_po_etc_amt
                                WITH KEY ebeln = ls_ebeln-ebeln
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      "선급금 취합 계산
      gs_scr100-dwpayc = gs_scr100-dwpayc +
                         ( gs_po_etc_amt-downpay_amount + gs_po_etc_amt-downpay_tax
                         + gs_po_etc_amt-downpay_clear + gs_po_etc_amt-clear_tax ).

      "지체상금 취합 계산
      IF NOT gs_po_etc_amt-late_rate IS INITIAL.
        CLEAR lv_delayf.
        lv_delayf = gs_scr100-dmbtr2 * gs_po_etc_amt-late_rate / 1000.

        gs_scr100-delayf_m = gs_scr100-delayf_m + lv_delayf.
        gs_scr100-delayf_x = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "최대 처리 선급액
  IF gs_scr100-dwpayc_m IS INITIAL.
    gs_scr100-dwpayc_m = gs_scr100-dwpayc.
  ENDIF.

  PERFORM calculate_tax USING space.               "세금계산
  PERFORM get_exchange_rate.                       "환율구하기
  PERFORM convert_total_amount_to_krw USING space. "KRW 환산금액

  "인쇄교체비 SUM
  PERFORM get_print_changing_fee.

*--------------------------------------------------------------------*
*U1> 2022.04.14 : 대체수취인 & 송장발행처 기능 추가.
*  SELECT DISTINCT BPBANKACCOUNTINTERNALID AS BVTYP,
*         BANK, BANKA, BANKACCOUNT,
*         BANKACCOUNTHOLDERNAME
*    FROM ZCCMM_BANKACCNT
*   WHERE SUPPLIER = @GS_SCR100-LIFNR
*     AND BANKACCOUNT IN @GR_BANKN
*    INTO TABLE @DATA(LT_BANK_ACC).
*  READ TABLE LT_BANK_ACC INTO DATA(LS_BANK_ACC) INDEX 1.
*  IF SY-SUBRC EQ 0.
*    GS_SCR100-BVTYP = LS_BANK_ACC-BVTYP.
*  ENDIF.

*> 송장발행처 기능 추가
  PERFORM set_init_diff_lifnr.

*> 대체수취인 기능 추가
  PERFORM set_init_empfk.

*> 송장발행처 및 대체수취인별 계좌 설정
  PERFORM set_init_bank_acc.
*--------------------------------------------------------------------*

  "만기일 재계산
  PERFORM set_adjust_netdt.

  IF lv_chk EQ abap_true.
    "해당 PO에 미결선급이 존재합니다.
    lv_text = '미결선급이 존재합니다.'(m44).
    MESSAGE s000 DISPLAY LIKE 'W' WITH lv_text.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  CALCULATE_TAX
*&---------------------------------------------------------------------*
*       세금 계산
*        - P_FLAG 가 SPACE 이면  상단 금액부분 입고공급가, 공급가
*          둘다 동일하게 적용한다.
*        - P_FLAG 가 TRUE  이면  상단 공급가 세액 총액 부분만 적용한다.
*----------------------------------------------------------------------*
FORM calculate_tax USING iv_flag.

  DATA lt_tax   LIKE TABLE OF rtax1u15.
  DATA lv_fwste LIKE bset-fwste.
  DATA lv_dmbtr LIKE bseg-dmbtr.

  CASE iv_flag.
    WHEN 'X'.  "세금계산 체크박스
      CLEAR gs_scr100-wmwst2.
      IF gs_scr100-mwskz IS INITIAL.
        gs_scr100-total2    = gs_scr100-dmbtr2.
        gs_scr100-differenz = gs_scr100-dmbtr2 - gs_scr100-dmbtr.
        EXIT.
      ENDIF.
      lv_dmbtr = gs_scr100-dmbtr2.

    WHEN OTHERS.
      CLEAR: gs_scr100-wmwst, gs_scr100-wmwst2.
      IF gs_scr100-mwskz IS INITIAL.
        gs_scr100-total2 = gs_scr100-total
                         = gs_scr100-dmbtr2 = gs_scr100-dmbtr.
        CLEAR gs_scr100-differenz.
        EXIT.
      ENDIF.
      lv_dmbtr = gs_scr100-dmbtr2 = gs_scr100-dmbtr.
  ENDCASE.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = gs_scr100-bukrs
      i_mwskz = gs_scr100-mwskz
      i_waers = gs_scr100-waers
      i_wrbtr = lv_dmbtr
    IMPORTING
      e_fwste = lv_fwste
    TABLES
      t_mwdat = lt_tax
    EXCEPTIONS
      OTHERS  = 13.

  IF sy-subrc EQ 0.
    IF iv_flag IS INITIAL.
      gs_scr100-wmwst2  = gs_scr100-wmwst = lv_fwste.
      gs_scr100-total2  = gs_scr100-total
                        = gs_scr100-dmbtr + gs_scr100-wmwst.
    ELSE.
      gs_scr100-wmwst2  = lv_fwste.
      gs_scr100-total2  = gs_scr100-dmbtr2 + gs_scr100-wmwst2.
    ENDIF.

    gs_scr100-differenz = gs_scr100-dmbtr2 - gs_scr100-dmbtr.

  ELSE.
    MESSAGE ID     sy-msgid
            TYPE   'S'
            NUMBER sy-msgno
            DISPLAY LIKE 'E'
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    PERFORM clear_screen_amount.
  ENDIF.


ENDFORM. " CALCULATE_TAX
*&---------------------------------------------------------------------*
*&      FORM  CREATE_INCOMINGINVOICE
*&---------------------------------------------------------------------*
*       송장처리
*----------------------------------------------------------------------*
FORM create_incominginvoice .

  DATA lv_chk(1)    TYPE c.
  DATA lv_chk_c(1)  TYPE c.
  DATA lv_text      TYPE string.
*[KTGP-78437 변경시작 2023.02.08]
  PERFORM check_compliance USING lv_chk_c.
  CHECK lv_chk_c IS INITIAL.
*[KTGP-78437 변경종료 2023.02.08]

  PERFORM check_condition USING lv_chk.
  CHECK lv_chk IS INITIAL.


  lv_text = TEXT-m06.  "송장처리 하시겠습니까?

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '확인'
      text_question         = lv_text
      text_button_1         = 'Yes'
      icon_button_1         = 'ICON_ALLOW'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_REJECT'
      default_button        = '2'
      display_cancel_button = space
      popup_type            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      answer                = lv_chk
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_chk EQ '1'.

  IF gs_post_split-sel_line IS INITIAL.
    PERFORM create_iv_no_split.
  ELSE.
    PERFORM create_iv_split_by_line.
  ENDIF.

ENDFORM. " CREATE_INCOMINGINVOICE
*&---------------------------------------------------------------------*
*&      FORM  BUSINESS_PLACE
*&---------------------------------------------------------------------*
*       사업장
*----------------------------------------------------------------------*
FORM business_place USING iv_werks
                     CHANGING cv_business_place.

  READ TABLE gt_t001w INTO gs_t001w
                      WITH KEY werks = iv_werks
                      BINARY SEARCH.
  IF sy-subrc EQ 0.
    cv_business_place = gs_t001w-j_1bbranch.
  ENDIF.

ENDFORM. " BUSINESS_PLACE
*&---------------------------------------------------------------------*
*&      FORM  F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM f4if_int_table_value_request TABLES et_value
                                   USING iv_field   TYPE dfies-fieldname.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = iv_field
      value_org       = 'S'
    TABLES
      value_tab       = et_value
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.


ENDFORM. " F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      FORM  SET_ADJUST_NETDT
*&---------------------------------------------------------------------*
*       기산일 계산, 지급방법 체크
*----------------------------------------------------------------------*
FORM set_adjust_netdt.

  CASE ok_code.
    WHEN 'FTAX2' OR 'FRATE'.
      EXIT.
    WHEN OTHERS.
  ENDCASE.

  PERFORM set_netdt.

  READ TABLE gt_head INTO gs_head WITH KEY cbox = abap_true.
  CHECK sy-subrc EQ 0.

ENDFORM. " SET_ADJUST_NETDT
*&---------------------------------------------------------------------*
*&      FORM  SET_NETDT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM set_netdt.

  DATA ls_faede   LIKE faede.
  DATA lv_zfbdt   TYPE sydatum.

  CHECK gs_scr100-bldat IS NOT INITIAL.

  lv_zfbdt = gs_scr100-zfbdt.

  CLEAR gs_scr100-zfbdt.
  PERFORM fi_terms_of_payment_propose CHANGING ls_faede.

  IF gs_scr100-total2 < 0.
    ls_faede-shkzg = 'S'.
  ELSE.
    ls_faede-shkzg = 'H'.
  ENDIF.

  ls_faede-koart = 'K'.             "구매송장

  CALL FUNCTION 'DETERMINE_DUE_DATE'
    EXPORTING
      i_faede                    = ls_faede
    IMPORTING
      e_faede                    = ls_faede
    EXCEPTIONS
      account_type_not_supported = 1
      OTHERS                     = 2.

  IF ls_faede-zfbdt IS NOT INITIAL.
    gs_scr100-zfbdt = ls_faede-zfbdt.  "기산일
    gs_scr100-netdt = ls_faede-netdt.  "만기일
  ENDIF.

  gs_scr100-zlsch = gs_t052-zlsch.    "지급방법
  gs_scr100-zbd1t = ls_faede-zbd1t.   "현금할인기간

  PERFORM set_netdt_with_zlsch USING lv_zfbdt.


ENDFORM. " SET_NETDT
*&---------------------------------------------------------------------*
*&      FORM  SET_NETDT2
*&---------------------------------------------------------------------*
*       기산일 변경시 만기일 계산
*----------------------------------------------------------------------*
FORM set_netdt2.

  DATA ls_faede   LIKE faede.

  CHECK gs_scr100-bldat IS NOT INITIAL.

  PERFORM fi_terms_of_payment_propose CHANGING ls_faede.

  IF gs_scr100-total2 < 0.
    ls_faede-shkzg = 'S'.
  ELSE.
    ls_faede-shkzg = 'H'.
  ENDIF.

  ls_faede-koart = 'K'.             "구매송장
  ls_faede-zfbdt = gs_scr100-zfbdt. "기산일

  CALL FUNCTION 'DETERMINE_DUE_DATE'
    EXPORTING
      i_faede                    = ls_faede
    IMPORTING
      e_faede                    = ls_faede
    EXCEPTIONS
      account_type_not_supported = 1
      OTHERS                     = 2.

  IF ls_faede-zfbdt IS NOT INITIAL.
    gs_scr100-netdt = ls_faede-netdt.  "만기일
  ENDIF.

  gs_scr100-zbd1t = ls_faede-zbd1t.    "현금할인기간
  PERFORM set_netdt_with_zlsch USING gs_scr100-zfbdt.

ENDFORM. " SET_NETDT2
*&---------------------------------------------------------------------*
*&      FORM  FI_TERMS_OF_PAYMENT_PROPOSE
*&---------------------------------------------------------------------*
*       기산일 계산
*----------------------------------------------------------------------*
FORM fi_terms_of_payment_propose CHANGING cs_faede LIKE faede.

  CLEAR: cs_faede, gs_t052.

  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
    EXPORTING
      i_bldat         = gs_scr100-bldat
      i_budat         = gs_scr100-budat
      i_zfbdt         = gs_scr100-zfbdt
      i_zterm         = gs_scr100-zterm
      i_bukrs         = gs_scr100-bukrs
    IMPORTING
      e_zbd1t         = cs_faede-zbd1t
      e_zbd2t         = cs_faede-zbd2t
      e_zbd3t         = cs_faede-zbd3t
      e_zfbdt         = cs_faede-zfbdt
      e_t052          = gs_t052
    EXCEPTIONS
      terms_not_found = 1
      OTHERS          = 2.

ENDFORM. " FI_TERMS_OF_PAYMENT_PROPOSE
*&---------------------------------------------------------------------*
*&      FORM  SELECT_DATA_HEAD
*&---------------------------------------------------------------------*
*       왼쪽 ALV 에서 선택한 업체 상세 데이터를 우측 ALV 에 보여준다.
*       - 조회된 데이터 모두 선택한 상태로 보여준다.
*       - 금액 및 세금계산을 해서 스크린에 반영한다.
*----------------------------------------------------------------------*
FORM select_data_head USING iv_row_id iv_fieldname.

*  DATA lr_lifnr TYPE RANGE OF lifnr.
*  DATA lr_stcd2 TYPE RANGE OF zdtv3e_su_id.
*  DATA lr_email TYPE RANGE OF zdtv3e_ip_email1.


  READ TABLE gt_left INTO gs_left INDEX iv_row_id.
  CASE iv_fieldname.
    WHEN 'LIGHT'.  "전자세금계산서 수령 여부
*      IF sy-subrc EQ 0.
*        CHECK gs_left-light(3) NE gc_icon_red.
*        _g_append3 lr_lifnr 'I' gc_eq gs_left-lifnr.
*        _g_append3 lr_stcd2 'I' gc_eq gs_left-stcd2.
*        "매입 전자세금계산서 관리프로그램
*        SUBMIT zdtv3_ap WITH &0000001 EQ p_bukrs
*                         WITH &0000002 IN s_budat
*                         WITH &0000004 IN lr_stcd2
*                         WITH &0000004 IN lr_email
*                         WITH &0000008 IN lr_lifnr
*                         "VIA SELECTION-SCREEN
*                         AND RETURN.
*      ENDIF.
*      EXIT.

    WHEN OTHERS.
  ENDCASE.

  IF sy-subrc EQ 0.

    PERFORM bldat_dynp_values_update.

    "작성일자를 입력하세요.
    IF gs_scr100-bldat IS INITIAL OR
       gs_scr100-bldat EQ space.
      MESSAGE s017 DISPLAY LIKE 'E' WITH TEXT-m13.
      EXIT.
    ENDIF.

    gs_expand-i = abap_true.
    gt_head[]   = gt_head_all[].
    DELETE gt_head WHERE lifnr NE gs_left-lifnr.


    gs_head-cbox = abap_true.
    MODIFY gt_head FROM gs_head TRANSPORTING cbox WHERE lifnr = gs_left-lifnr
                                       AND waers = gs_left-waers.

    gs_expand-h = abap_true.

    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gc_calcamt.

  ELSE.
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = gc_refresh_head.
  ENDIF.

  PERFORM refresh_alv USING gv_grid_head.

ENDFORM. " SELECT_DATA_HEAD
*&---------------------------------------------------------------------*
*&      FORM  CLEAR_SCREEN_AMOUNT
*&---------------------------------------------------------------------*
*       헤더 데이터 금액관련 필드 초기화
*----------------------------------------------------------------------*
FORM clear_screen_amount .

  CLEAR: gs_scr100-dmbtr,     gs_scr100-wmwst,  gs_scr100-total,
         gs_scr100-dmbtr2,    gs_scr100-wmwst2, gs_scr100-total2,
         gs_scr100-differenz, gs_scr100-belnr,  gs_scr100-total2_krw,
         gs_scr100-dwpayc,
*U1> 2022.04.14 : 대체수취인/송장발행처 입력기능 추가
* (계좌도 대체수취 -> 송장발행처 -> 공급업체 우선순위로 검색 및 체크
         gv_empfk_used, gs_scr100-empfk, gs_scr100-empfk_tx,
         gv_diff_lifnr_used, gs_scr100-diff_lifnr, gs_scr100-diff_lifnr_tx,
         gt_bank_acc, gs_bank_acc.

  PERFORM set_signal_status USING    space
                            CHANGING gs_scr100-status.

ENDFORM. " CLEAR_SCREEN_AMOUNT
*&---------------------------------------------------------------------*
*&      MODULE  SET_DMBTR2  INPUT
*&---------------------------------------------------------------------*
*       세금계산이 체크인 경우 입력한 금액에 대한 세금계산을 한다.
*----------------------------------------------------------------------*
MODULE set_dmbtr2 INPUT.

  CASE ok_code.
    WHEN 'CHANGE'.
    WHEN 'DYN' OR 'CLEAR'.
    WHEN space.
      PERFORM set_dmbtr2 USING space abap_true.  "공급가액변경
    WHEN OTHERS.
      PERFORM set_dmbtr2 USING space space.
  ENDCASE.

ENDMODULE. " SET_DMBTR2 INPUT
*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       송장처리결과 DISPLAY
*----------------------------------------------------------------------*
FORM display_log .

  DATA ls_layout TYPE slis_layout_alv.

  IF gt_log[] IS INITIAL.
    MESSAGE s031 WITH TEXT-m15. "메시지 내역이 없습니다.
    EXIT.
  ENDIF.

  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      i_structure_name   = gc_struc_log
      is_layout          = ls_layout
    TABLES
      t_outtab           = gt_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM. " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      FORM  BAPI_CURRENCY_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
*       금액변환 BAPI(23,4)
*----------------------------------------------------------------------*
FORM bapi_currency_conv_to_external USING iv_amount
                                             iv_currency
                                    CHANGING cv_amount_doc.

  DATA lv_amount LIKE bapicurr-bapicurr.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = iv_currency
      amount_internal = iv_amount
    IMPORTING
      amount_external = lv_amount.

  cv_amount_doc = lv_amount.

ENDFORM. " BAPI_CURRENCY_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
*&      FORM  CURRENCY_AMOUNT_IDOC_TO_SAP
*&---------------------------------------------------------------------*
*       금액변환 SAP 내부
*----------------------------------------------------------------------*
FORM currency_amount_idoc_to_sap USING iv_amount_in
                                           iv_waers
                                  CHANGING cv_amount_out.

  CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
    EXPORTING
      currency    = iv_waers
      idoc_amount = iv_amount_in
    IMPORTING
      sap_amount  = cv_amount_out.


ENDFORM. " CURRENCY_AMOUNT_IDOC_TO_SAP
*&---------------------------------------------------------------------*
*&      FORM  CURRENCY_AMOUNT_SAP_TO_IDOC
*&---------------------------------------------------------------------*
*       금액변환 SAP_TO_IDOC
*----------------------------------------------------------------------*
FORM currency_amount_sap_to_idoc USING iv_amount_sap
                                           iv_currency
                                  CHANGING cv_amount_doc.

  DATA lv_amount TYPE string.

  CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
    EXPORTING
      currency    = iv_currency
      sap_amount  = iv_amount_sap
    IMPORTING
      idoc_amount = lv_amount.

  cv_amount_doc = lv_amount.

ENDFORM. " CURRENCY_AMOUNT_SAP_TO_IDOC
*&---------------------------------------------------------------------*
*&      FORM  MAPPING_INVOICE
*&---------------------------------------------------------------------*
*       생성된 전표를 MAPPING 하기 위한 프로그램을 호출한다.
*----------------------------------------------------------------------*
FORM mapping_invoice.

  DATA lr_lifnr  TYPE RANGE OF lifnr.
  DATA lr_gjahr  TYPE RANGE OF gjahr   .
  DATA lr_bldat  TYPE RANGE OF bldat   .
  DATA lr_blart  TYPE RANGE OF blart   .
  DATA lr_budat  TYPE RANGE OF budat   .
  DATA lr_belnr  TYPE RANGE OF belnr_d .
  DATA lr_usnam  TYPE RANGE OF usnam   .
*  DATA lr_stcd2  TYPE RANGE OF zdtv3e_su_id.
*  DATA lr_email  TYPE RANGE OF zdtv3e_ip_email1.


  DATA lv_belnr  TYPE bkpf-belnr.
  DATA lv_gjahr  TYPE bkpf-gjahr.
  DATA lv_awkey  TYPE bkpf-awkey.
  DATA lv_text   TYPE string.
  DATA lv_chk(1) TYPE c.
  DATA lv_date_f TYPE p0001-begda.
  DATA lv_date_t TYPE p0001-begda.
  DATA lv_bldat  TYPE p0001-begda.
  DATA lv_lifnr  TYPE lfa1-lifnr.
  DATA lv_stcd2  TYPE lfa1-stcd2.


  IF p_r1 EQ abap_true.  "정발행인 경우
    lv_text = TEXT-m19.  "전표 Mapping 화면으로 이동 하시겠습니까?
  ELSE.
    lv_text = TEXT-m28.  "역발행 요청 화면으로 이동 하시겠습니까?
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '확인'
      text_question         = lv_text
      text_button_1         = 'Yes'
      icon_button_1         = 'ICON_ALLOW'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_REJECT'
      default_button        = '2'
      display_cancel_button = space
      popup_type            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      answer                = lv_chk
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_chk EQ '1'.

  lv_awkey = gs_scr100-belnr.
  CONDENSE lv_awkey NO-GAPS.


*-회계전표번호
  SELECT SINGLE belnr gjahr
          INTO (lv_belnr,
                lv_gjahr)
          FROM bkpf
         WHERE bukrs = p_bukrs
           AND blart = gc_re
           AND bldat = gs_scr100-bldat
           AND budat = gs_scr100-budat
           AND awkey = lv_awkey
           AND cpudt = sy-datum
           AND usnam = sy-uname.


  _g_append3 lr_gjahr 'I' gc_eq lv_gjahr.
  _g_append3 lr_blart 'I' gc_eq gc_re.
  _g_append3 lr_budat 'I' gc_eq gs_scr100-budat.
  _g_append3 lr_belnr 'I' gc_eq lv_belnr.
  _g_append3 lr_usnam 'I' gc_eq sy-uname.

  "송장발행처
  SELECT SINGLE b~lifnr b~stcd2
            INTO (lv_lifnr,
                  lv_stcd2)
            FROM rbkp AS a INNER JOIN lfa1 AS b
              ON a~lifnr = b~lifnr
           WHERE a~belnr EQ gs_scr100-belnr(10)
             AND a~gjahr EQ gs_scr100-belnr+11(*).

  _g_append3 lr_lifnr 'I' gc_eq lv_lifnr.
*  _g_append3 lr_stcd2 'I' gc_eq lv_stcd2.


  lv_bldat = gs_scr100-bldat.
  IF lv_bldat IS INITIAL.
    lv_bldat = sy-datum.
  ENDIF.
  "작성일 기준 이전 15일
  PERFORM get_calculate_date USING    lv_bldat '-'
                              CHANGING lv_date_f.

  "작성일 기준 이후 15일
  PERFORM get_calculate_date USING    lv_bldat '+'
                              CHANGING lv_date_t.
  _g_append4 lr_bldat 'I' gc_bt lv_date_f lv_date_t.

  CASE p_r1.
    WHEN abap_true.  "정발행
      SUBMIT zdtv3_ap_p01 WITH p_bukrs EQ p_bukrs    "회사코드
                          WITH s_datum IN lr_bldat       "기준일자
*                          WITH s_stcd2 IN lr_stcd2       "사업자번호
                          WITH s_blart IN lr_blart       "전표유형
                          WITH s_budat IN lr_budat       "전표전기일
                          WITH s_bln   IN lr_belnr       "전표번호
*                          WITH s_email IN lr_email       "이메일
                          "VIA SELECTION-SCREEN
                           AND RETURN.

    WHEN OTHERS.     "역발행
      "별도 화면 이동 존재
  ENDCASE.


ENDFORM. " MAPPING_INVOICE
*&---------------------------------------------------------------------*
*&      FORM  GET_BSART_LIST
*&---------------------------------------------------------------------*
*       송장처리 문서유형 조회
*----------------------------------------------------------------------*
FORM get_bsart_list .

  CONSTANTS: lc_bsart_pkc1 TYPE ekko-bsart VALUE 'PKC1',
             lc_bsart_psic TYPE ekko-bsart VALUE 'PSIC'.

  CASE abap_true.
    WHEN p_rp2. "물품(선급)
      gr_bstyp[] = VALUE #( ( sign = 'I' option = gc_eq low = 'F' ) ).
      gr_bsart[] = VALUE #( ( sign = 'E' option = gc_eq low = lc_bsart_pkc1 )
                            ( sign = 'E' option = gc_eq low = lc_bsart_psic ) ).
      gr_dppct[] = VALUE #( ( sign = 'I' option = gc_ne low = 0 ) ).
    WHEN p_rp3. "자율납품&위탁정산
      gr_bstyp[] = VALUE #( ( sign = 'I' option = gc_eq low = 'L' ) ).
    WHEN p_rp4. "공사/용역
      gr_bstyp[] = VALUE #( ( sign = 'I' option = gc_eq low = 'F' ) ).
      gr_bsart[] = VALUE #( ( sign = 'I' option = gc_eq low = lc_bsart_pkc1 )
                            ( sign = 'I' option = gc_eq low = lc_bsart_psic ) ).
    WHEN OTHERS. "물품일반
      gr_bstyp[] = VALUE #( ( sign = 'I' option = gc_eq low = 'F' ) ).
      gr_bsart[] = VALUE #( ( sign = 'E' option = gc_eq low = lc_bsart_pkc1 )
                            ( sign = 'E' option = gc_eq low = lc_bsart_psic ) ).
      gr_dppct[] = VALUE #( ( sign = 'I' option = gc_eq low = 0 ) ).
  ENDCASE.

ENDFORM. " GET_BSART_LIST
*&---------------------------------------------------------------------*
*&      FORM  GET_TAXCODE_LIST
*&---------------------------------------------------------------------*
*       송장처리 TAX 코드 조회
*----------------------------------------------------------------------*
FORM get_taxcode_list .

  CLEAR: gt_mwskz, gr_mwskz_p.

  DATA: lv_kalsm        LIKE t007s-kalsm,
        lr_mwskz_no_use TYPE RANGE OF t007s-mwskz.

  CONSTANTS lc_tax(3) TYPE c VALUE 'TAX'.

  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'E1' d = 'E1010' s = 'E1014' )
     IMPORTING et_outtab = DATA(lt_tax_no_use) ).

  LOOP AT lt_tax_no_use INTO DATA(ls_tax_no_use).
    APPEND VALUE #( sign = 'E' option = 'EQ' low = ls_tax_no_use-field1 ) TO lr_mwskz_no_use.
  ENDLOOP.

  SELECT SINGLE land1
    FROM t001
   WHERE bukrs EQ @p_bukrs
    INTO @DATA(lv_land1).

  lv_kalsm = lc_tax && lv_land1.

  SELECT a~mwskz, a~text1
    FROM t007s AS a JOIN t007a AS b
      ON a~mwskz = b~mwskz
     AND a~kalsm = b~kalsm
   WHERE a~kalsm = @lv_kalsm
     AND a~spras = @sy-langu
     AND b~mwart = 'V'
     AND a~mwskz IN @lr_mwskz_no_use
    INTO TABLE @gt_mwskz.

  IF gt_mwskz[] IS INITIAL.
    "세금코드가 없습니다.
    MESSAGE s031 WITH TEXT-m26.
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT gt_mwskz BY mwskz.
  ENDIF.

*> 종이세금계산서 코드 조회
  CLEAR gt_paper_tax.
  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'E1' d = 'E1010' s = 'E1011' )
                                     it_where = VALUE #(
                                                ( field = 1 value = lv_kalsm )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).
  LOOP AT lt_config INTO DATA(ls_config).
    APPEND VALUE #( mwskz = ls_config-field2 text = ls_config-field3 ) TO gt_paper_tax.
  ENDLOOP.

  SORT gt_paper_tax BY mwskz.

ENDFORM. " GET_TAXCODE_LIST
*&---------------------------------------------------------------------*
*&      FORM  GET_BUPLA_LIST
*&---------------------------------------------------------------------*
*       회사 사업장 조회
*----------------------------------------------------------------------*
FORM get_bupla_list .

  SELECT branch AS low,
         'I' AS sign,
         @gc_eq AS option
           FROM j_1bbranch
          WHERE bukrs  EQ @p_bukrs
            AND branch IN @s_bupla
    INTO CORRESPONDING FIELDS OF TABLE @gr_bupla.

  IF gr_bupla[] IS INITIAL.
    MESSAGE s005 WITH TEXT-m30. "회사에 없는 사업장입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM. " GET_BUPLA_LIST
*&---------------------------------------------------------------------*
*&      FORM  GET_BUPLA_LIST2
*&---------------------------------------------------------------------*
*       회사 사업장 조회2
*----------------------------------------------------------------------*
FORM get_bupla_list2.

  SELECT branch AS bupla
         name
         INTO TABLE gt_bupla
         FROM j_1bbranch
        WHERE bukrs EQ p_bukrs.
  SORT gt_bupla BY bupla.

ENDFORM. " GET_BUPLA_LIST2
*&---------------------------------------------------------------------*
*&      FORM  GET_PLANT_LIST
*&---------------------------------------------------------------------*
*       사업장 플랜트 조회
*----------------------------------------------------------------------*
FORM get_plant_list .

  CONSTANTS: lc_i TYPE c VALUE 'I'.

  DATA: lr_bwkey TYPE RANGE OF t001k-bwkey.

  SELECT bwkey AS low,
         @lc_i AS sign,
         @gc_eq AS option
    FROM t001k
   WHERE bukrs = @p_bukrs
    INTO CORRESPONDING FIELDS OF TABLE @lr_bwkey.

  SELECT werks AS low,
         'I' AS sign,
         @gc_eq AS option
          FROM t001w
         WHERE werks IN @s_werks
           AND bwkey IN @lr_bwkey
    INTO CORRESPONDING FIELDS OF TABLE @gr_werks.

  IF gr_werks[] IS INITIAL.
    " 회사코드,사업장에 해당하는 플랜트가 존재하지 않습니다.
    MESSAGE s005 WITH TEXT-m31.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM. " GET_PLANT_LIST
*&---------------------------------------------------------------------*
*&      FORM  CHECK_RECEIVE_ESERO
*&---------------------------------------------------------------------*
*       전자세금계산서 수령여부
*----------------------------------------------------------------------*
FORM check_receive_esero .

*  DATA: BEGIN OF ls_eacc_data,
*          stcd2       LIKE zdtv3t_ap_head-su_id,
*          chargetotal LIKE zdtv3t_ap_head-chargetotal,
*        END OF ls_eacc_data,
*        lt_eacc_data LIKE TABLE OF ls_eacc_data,
*        lt_tmp       LIKE TABLE OF ls_eacc_data.
*
*  CHECK p_r1 EQ abap_true.   "정발행인 경우
*
*
*  MOVE-CORRESPONDING gt_left[] TO lt_tmp[].
*
*  SORT lt_tmp BY stcd2.
*  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING stcd2.
*  IF NOT lt_tmp[] IS INITIAL.
*    SELECT a~su_id AS stcd2, a~chargetotal
*      FROM zdtv3t_ap_head AS a INNER JOIN zdtv3t_ap_ext AS b
*        ON a~bukrs        = b~bukrs          AND
*           a~issue_date   = b~issue_date     AND
*           a~bupla        = b~bupla          AND
*           a~inv_seq      = b~inv_seq
*       FOR ALL ENTRIES IN @lt_tmp
*      WHERE a~su_id        EQ @lt_tmp-stcd2
*        AND a~bukrs        EQ @p_bukrs
*        AND a~issue_date   IN @s_budat
*        AND b~use_doc      EQ @space
*      INTO TABLE @lt_eacc_data.
*
*    FREE lt_tmp.
*    SORT lt_eacc_data BY stcd2.
*  ENDIF.
*
*  LOOP AT gt_left INTO gs_left.
*
*    CLEAR ls_eacc_data.
*
*    READ TABLE lt_eacc_data INTO ls_eacc_data
*                            WITH KEY stcd2 = gs_left-stcd2
*                            BINARY SEARCH.
*
*    IF sy-subrc EQ 0.
*      IF ls_eacc_data-chargetotal EQ gs_left-dmbtr. "사업자번호,총금액이 같은경우
*        gs_left-light = icon_led_green.
*      ELSE.
*        gs_left-light = icon_led_yellow. "사업자번호 같은경우
*      ENDIF.
*    ELSE.
*      gs_left-light   = icon_led_red.    "공급업체 없는경우
*    ENDIF.
*
*    MODIFY gt_left FROM gs_left.
*  ENDLOOP.

ENDFORM. " CHECK_RECEIVE_ESERO
*&---------------------------------------------------------------------*
*&      FORM  CHECK_GR_IV
*&---------------------------------------------------------------------*
*       동일한 입고전표로 발생한 입고, 반품, 송장수령 데이터를
*       + - 해서 입고데이터를 체크한다.
*       입고금액,수량 - 반품금액,수량
*----------------------------------------------------------------------*
FORM check_gr_iv TABLES it_tmp STRUCTURE gs_head
                 USING    is_data LIKE gs_head.

  DATA lv_dmbtr     TYPE ekbe-dmbtr.
  DATA lv_dmbtr_krw TYPE ekbe-dmbtr.
  DATA lv_menge     TYPE ekbe-menge.
  DATA ls_tmp       LIKE gs_head.

*> 공사용역은 1:1
  READ TABLE it_tmp WITH KEY ebeln = is_data-ebeln
                             ebelp = is_data-ebelp
                             lfgja = is_data-gjahr
                             lfbnr = is_data-belnr
                             lfpos = is_data-buzei
                    BINARY SEARCH
                    TRANSPORTING NO FIELDS.

  LOOP AT it_tmp INTO ls_tmp FROM sy-tabix.
    IF ls_tmp-ebeln NE is_data-ebeln OR
       ls_tmp-ebelp NE is_data-ebelp OR
       ls_tmp-lfgja NE is_data-gjahr OR
       ls_tmp-lfbnr NE is_data-belnr OR
       ls_tmp-lfpos NE is_data-buzei.
      EXIT.
    ENDIF.

    IF ls_tmp-vgabe EQ '1'.    "입고
      IF ls_tmp-shkzg EQ 'H'.  "이동유형:102,122,161
        ls_tmp-dmbtr_krw = ls_tmp-dmbtr_krw * -1.
        ls_tmp-dmbtr     = ls_tmp-dmbtr * -1.
        ls_tmp-menge     = ls_tmp-menge * -1.
      ENDIF.
    ELSE.   "송장수령
      IF ls_tmp-shkzg EQ 'S'.
        ls_tmp-dmbtr_krw = ls_tmp-dmbtr_krw * -1.
        ls_tmp-dmbtr     = ls_tmp-dmbtr * -1.
        ls_tmp-menge     = ls_tmp-menge * -1.
      ENDIF.
    ENDIF.

    lv_dmbtr_krw = lv_dmbtr_krw + ls_tmp-dmbtr_krw.
    lv_dmbtr     = lv_dmbtr     + ls_tmp-dmbtr.
    lv_menge     = lv_menge     + ls_tmp-menge.

    is_data-dmbtr_krw = lv_dmbtr_krw.
    is_data-dmbtr     = is_data-wrbtr  = lv_dmbtr.
    is_data-menge     = lv_menge.

  ENDLOOP.

ENDFORM. " CHECK_GR_IV
*&---------------------------------------------------------------------*
*&      FORM  SHOW_EKBE_LIST
*&---------------------------------------------------------------------*
*       구매문서이력을 보여준다.
*----------------------------------------------------------------------*
FORM show_ekbe_list USING iv_ebeln iv_ebelp.

  DATA: lt_selfields LIKE TABLE OF se16n_seltab,
        ls_selfields LIKE se16n_seltab.
  DATA lv_tab       TYPE se16n_tab.

  lv_tab = gc_ekbe.

  CLEAR ls_selfields.
  ls_selfields-field  = gc_ebeln.
  ls_selfields-sign   = 'I'.
  ls_selfields-option = gc_eq.
  ls_selfields-low    = iv_ebeln.
  APPEND ls_selfields TO lt_selfields.

  CLEAR ls_selfields.
  ls_selfields-field  = gc_ebelp.
  ls_selfields-sign   = 'I'.
  ls_selfields-option = gc_eq.
  ls_selfields-low    = iv_ebelp.
  APPEND ls_selfields TO lt_selfields.

  CLEAR ls_selfields.
  ls_selfields-field  = gc_vgabe.
  ls_selfields-sign   = 'I'.
  ls_selfields-option = gc_bt.
  ls_selfields-low    = '1'.
  ls_selfields-high   = '2'.
  APPEND ls_selfields TO lt_selfields.

  CALL FUNCTION 'SE16N_INTERFACE'
    EXPORTING
      i_tab        = lv_tab
    TABLES
      it_selfields = lt_selfields
    EXCEPTIONS
      no_values    = 1
      OTHERS       = 2.


ENDFORM. " SHOW_EKBE_LIST
**&---------------------------------------------------------------------*
**&      FORM  CONVERSION_EXIT_ALPHA_INPUT
**&---------------------------------------------------------------------*
**       TEXT
**----------------------------------------------------------------------*
*FORM CONVERSION_EXIT_ALPHA_INPUT USING IV_INPUT
*                                  CHANGING CV_OUTPUT.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT  = IV_INPUT
*    IMPORTING
*      OUTPUT = CV_OUTPUT.
*
*
*ENDFORM. " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      FORM  CALCULATE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       외화인 경우
*       체크박스 선택시 화면 상단에 표시된 금액은 KRW 금액(현지금액)
*       을 기본값으로 보여준다.
*       화면상단에 있는 환율 적용 버튼을 클릭하면
*       현재 선택한 데이터들의 입고금액(전표금액)을 SUM 해서 입력한
*       환율을 적용해서 화면상단에 보여준다.
*       화면상단 입고기준 금액 부분은 변경하지 않는다.
*----------------------------------------------------------------------*
FORM calculate_exchange_rate.

  DATA lv_rf_cxroot TYPE REF TO cx_root.
  DATA lv_rate      TYPE invfo-kursf.
  DATA lv_mesg      TYPE string.
  DATA lv_error(1)  TYPE c.

  CHECK gs_scr100-waers2 NE gc_krw.

  PERFORM check_item_data_sum USING lv_error.
  CHECK lv_error IS INITIAL.

  CLEAR: gs_scr100-dmbtr2,
         gs_scr100-wmwst2,
         gs_scr100-total2,
         gs_scr100-total2_krw.

  LOOP AT gt_head INTO gs_head WHERE cbox = abap_true.

    TRY.
        gs_scr100-dmbtr2 = gs_scr100-dmbtr2 + gs_head-dmbtr.
      CATCH cx_root INTO lv_rf_cxroot.
        lv_mesg = lv_rf_cxroot->get_text( ).
        MESSAGE s000 DISPLAY LIKE 'E' WITH lv_mesg.
        RETURN.
    ENDTRY.
  ENDLOOP.

  IF gs_scr100-kursf IS INITIAL.
    PERFORM get_exchange_rate.  "환율구하기
  ENDIF.

  PERFORM currency_amount_idoc_to_sap  USING gs_scr100-kursf
                                             gc_krw
                                    CHANGING lv_rate.


  IF p_r11 EQ abap_true. "원화
    TRY.
        gs_scr100-dmbtr2 = gs_scr100-dmbtr2 * lv_rate.         "공급가
        PERFORM calculate_tax USING abap_true.                 "세액계산
        gs_scr100-total2 = gs_scr100-dmbtr2 + gs_scr100-wmwst2."총액
      CATCH cx_root INTO lv_rf_cxroot.
        lv_mesg = lv_rf_cxroot->get_text( ).
        MESSAGE s000 DISPLAY LIKE 'E' WITH lv_mesg.
    ENDTRY.
  ELSE.
    TRY.
        PERFORM calculate_tax USING abap_true.                 "세액계산
        gs_scr100-total2 = gs_scr100-dmbtr2 + gs_scr100-wmwst2."총액
        PERFORM convert_total_amount_to_krw USING lv_rate.     "KRW 환산금액
      CATCH cx_root INTO lv_rf_cxroot.
        lv_mesg = lv_rf_cxroot->get_text( ).
        MESSAGE s000 DISPLAY LIKE 'E' WITH lv_mesg.
    ENDTRY.
  ENDIF.

ENDFORM. " CALCULATE_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      FORM  GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       환율 구하기
*----------------------------------------------------------------------*
FORM get_exchange_rate .

  DATA ls_exch_rate  LIKE bapi1093_0.
  DATA ls_return     LIKE bapiret1.

  IF gs_scr100-wwert IS INITIAL.
    gs_scr100-wwert = sy-datum.
  ENDIF.

  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      rate_type  = 'M'
      from_curr  = gs_scr100-waers2
      to_currncy = 'KRW'
      date       = gs_scr100-wwert
    IMPORTING
      exch_rate  = ls_exch_rate
      return     = ls_return.

  IF ls_return IS INITIAL.
    gs_scr100-kursf = ls_exch_rate-exch_rate.
  ELSE.
    CLEAR gs_scr100-kursf.
  ENDIF.


ENDFORM. " GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      FORM  CHECK_ITEM_DATA_SUM
*&---------------------------------------------------------------------*
*       동일한 조건의 데이터를 선택했는지 체크한다.
*----------------------------------------------------------------------*
FORM check_item_data_sum USING iv_error.

  DATA lt_lifnr      LIKE TABLE OF gs_head.
  DATA lt_waers      LIKE TABLE OF gs_head.
  DATA lt_mwskz      LIKE TABLE OF gs_head.

  lt_lifnr[] = gt_head[].
  DELETE lt_lifnr WHERE cbox IS INITIAL.

  lt_mwskz[] = lt_waers[] = lt_lifnr[].
  SORT lt_lifnr BY lifnr.
  SORT lt_waers BY waers.
  SORT lt_mwskz BY mwskz.


* 동일한 업체인지 체크
  SORT lt_lifnr BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_lifnr COMPARING lifnr.
  IF lines( lt_lifnr ) GT 1.
    "동일한 업체를 선택하세요.
    MESSAGE s049 DISPLAY LIKE 'E' WITH TEXT-m01.
    iv_error = abap_true.
    EXIT.
  ENDIF.


* 동일한 통화인지 체크
  SORT lt_waers BY waers.
  DELETE ADJACENT DUPLICATES FROM lt_waers COMPARING waers.
  IF lines( lt_waers ) GT 1.
    "동일한 통화를 선택하세요.
    MESSAGE s049 DISPLAY LIKE 'E' WITH TEXT-m02.
    iv_error = abap_true.
    EXIT.
  ENDIF.


  "작성일자를 입력하세요.
  IF gs_scr100-bldat IS INITIAL.
    MESSAGE s017 DISPLAY LIKE 'E' WITH TEXT-m13.
    iv_error = abap_true.
    EXIT.
  ENDIF.


ENDFORM. " CHECK_ITEM_DATA_SUM
*&---------------------------------------------------------------------*
*&      FORM  ADJUST_AMOUNT
*&---------------------------------------------------------------------*
*/- 화면상단 입고기준 공급가액은 DMBTR_KRW 현지통화금액 기준으로
*   적용한다.
*   외화인 경우 환율 적용시 DMBTR(입고금액,월말단가)기준으로
*   공급가를 적용한다.
*   월말 단가 조정 또는 CHK_PLANT = 'X'인 경우
*   원화이면 DMBTR(입고금액,월말단가)을 적용하고 외화인 경우 DMBTR_KRW
*   를 적용해서 화면 상단 입고기준 공급가액에 적용한다.
*----------------------------------------------------------------------*
FORM adjust_amount USING is_head LIKE gs_head
                   CHANGING cv_dmbtr.

  CASE p_r11. "원화
    WHEN 'X'.

*> 실비 정산을 입고금액에 추가함.
      cv_dmbtr = is_head-dmbtr_krw.

    WHEN OTHERS.  "외화
      cv_dmbtr = is_head-dmbtr.

  ENDCASE.
ENDFORM. " ADJUST_AMOUNT
*&---------------------------------------------------------------------*
*&      FORM  CHECK_PREPAID_AMOUNT
*&---------------------------------------------------------------------*
*       PO 에 미결선급이 존재하는지 체크
*----------------------------------------------------------------------*
FORM check_prepaid_amount USING iv_ebeln iv_ebelp
                          CHANGING cv_chk   cv_text.

  DATA lv_dmbtr(16) TYPE p DECIMALS 2.
  DATA: lv_text1 TYPE string,
        lv_text2 TYPE string.

  READ TABLE gt_prepaid WITH KEY ebeln = iv_ebeln
                                 ebelp = iv_ebelp
                        BINARY SEARCH
                        TRANSPORTING NO FIELDS.

  CHECK sy-subrc EQ 0.

  LOOP AT gt_prepaid INTO gs_prepaid FROM sy-tabix.
    IF gs_prepaid-ebeln NE iv_ebeln OR
       gs_prepaid-ebelp NE iv_ebelp.
      EXIT.
    ENDIF.

    CASE gs_prepaid-vgabe.
      WHEN '4'.      "선금
      WHEN OTHERS.   "선금반제
        gs_prepaid-dmbtr = gs_prepaid-dmbtr * -1.
    ENDCASE.
    lv_dmbtr = lv_dmbtr + gs_prepaid-dmbtr.
  ENDLOOP.

  IF lv_dmbtr > 0.
    cv_chk  = abap_true.

    "오더번호: &1 에 미결선급( &2 원) 존재합니다.
    cv_text  = TEXT-m27.
    CONCATENATE iv_ebeln iv_ebelp INTO lv_text1 SEPARATED BY space.

    PERFORM currency_amount_sap_to_idoc USING    lv_dmbtr gc_krw
                                        CHANGING lv_text2.
    MESSAGE s089 WITH lv_text1 lv_text2 INTO cv_text.
  ENDIF.

ENDFORM. " CHECK_PREPAID_AMOUNT
*&---------------------------------------------------------------------*
*&      FORM  HELP_F4_HBKID
*&---------------------------------------------------------------------*
*       거래은행명
*----------------------------------------------------------------------*
FORM help_f4_hbkid .

  DATA lt_dynpfields LIKE TABLE OF dynpread.

  DATA: lr_hbkid LIKE RANGE OF t012t-hbkid.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
      request    = 'A'
    TABLES
      dynpfields = lt_dynpfields
    EXCEPTIONS
      OTHERS     = 11.

  READ TABLE lt_dynpfields INTO DATA(ls_dynpfields) WITH KEY fieldname = gc_hbkid2.
  IF sy-subrc EQ 0 AND
     ls_dynpfields-fieldvalue IS NOT INITIAL.
    lr_hbkid = VALUE #( ( sign = 'I' option = 'EQ' low = ls_dynpfields-fieldvalue ) ).
  ENDIF.

  IF gt_hbkid[] IS INITIAL.
    SELECT hbkid hktid text1
           INTO TABLE gt_hbkid
           FROM t012t
          WHERE bukrs = gs_scr100-bukrs
            AND spras = sy-langu
            AND hbkid IN lr_hbkid.
  ENDIF.

  CLEAR gt_mapping.

  "거래은행명
  CLEAR gs_mapping.
  gs_mapping-fldname   = gc_f0001.
  gs_mapping-dyfldname = gc_hbkid2.
  APPEND gs_mapping TO gt_mapping.

  "거래계정
  CLEAR gs_mapping.
  gs_mapping-fldname   = gc_f0002.
  gs_mapping-dyfldname = gc_hktid2.
  APPEND gs_mapping TO gt_mapping.


  PERFORM f4if_int_table_value_request2 TABLES  gt_hbkid
                                                gt_mapping
                                                gt_return_tab
                                        USING   gc_hbkid
                                                TEXT-t09. "거래은행/계정



ENDFORM. " HELP_F4_HBKID
*&---------------------------------------------------------------------*
*&      FORM  F4IF_INT_TABLE_VALUE_REQUEST2
*&---------------------------------------------------------------------*
*       F4
*----------------------------------------------------------------------*
FORM f4if_int_table_value_request2 TABLES it_value
                                          it_mapping
                                          et_return_tab
                                   USING  iv_retfield
                                          iv_title.

  DATA lv_retfield      TYPE dfies-fieldname.
  DATA lv_dynprofield   TYPE help_info-dynprofld.


  lv_retfield    = iv_retfield.
  lv_dynprofield = iv_retfield.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_retfield
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = lv_dynprofield
      value_org       = 'S'
      window_title    = iv_title
    TABLES
      value_tab       = it_value
      dynpfld_mapping = it_mapping
      return_tab      = et_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2.


ENDFORM. " F4IF_INT_TABLE_VALUE_REQUEST2
*&---------------------------------------------------------------------*
*&      FORM  GET_EKKN_LIST
*&---------------------------------------------------------------------*
*       계정지정 데이터
*----------------------------------------------------------------------*
FORM get_ekkn_list TABLES it_head STRUCTURE gs_head.


  DATA lt_head LIKE TABLE OF gs_head.

  lt_head[] = it_head[].

  SORT lt_head BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_head COMPARING ebeln ebelp.

  CLEAR: gt_ekkn, gs_ekkn.

  CHECK lt_head[] IS NOT INITIAL.

  SELECT  a~ebeln
          a~ebelp
          a~kokrs
          a~sakto
          a~prctr
          a~kostl
          a~fistl
          a~vbeln
          a~vbelp
          a~anln1
          a~ps_psp_pnr AS pspnr
          a~aufnr
          b~user0
          INTO CORRESPONDING FIELDS OF TABLE gt_ekkn
          FROM ekkn AS a LEFT OUTER JOIN aufk AS b
            ON a~aufnr = b~aufnr
           FOR ALL ENTRIES IN lt_head
         WHERE ebeln EQ lt_head-ebeln
           AND ebelp EQ lt_head-ebelp.
  FREE lt_head.

  PERFORM get_ekkn_list_etc.

ENDFORM. " GET_EKKN_LIST
*&---------------------------------------------------------------------*
*&      FORM  REFRESH_ALL
*&---------------------------------------------------------------------*
*       전체 데이터를 다시 조회한다.
*----------------------------------------------------------------------*
FORM refresh_all .

  DATA ls_scr100 LIKE gs_scr100.

  ls_scr100 = gs_scr100.

  CLEAR : gs_scr100, gt_head, gt_head_all, gs_left, gt_left. " GS_TAB1.

  "데이터 Refresh 중입니다.
  PERFORM select_data USING abap_true.

  PERFORM refresh_alv  USING gv_grid_left.
  PERFORM refresh_alv  USING gv_grid_head.

  gs_scr100-bukrs = p_bukrs.
  gs_scr100-bldat = ls_scr100-bldat.
  gs_scr100-budat = ls_scr100-budat.
  gs_scr100-wwert  = ls_scr100-wwert.

  "화면상단 전표번호 신호등 초기화
  PERFORM set_signal_status USING    space
                             CHANGING gs_scr100-status.

  gs_scr100-label1 = ls_scr100-label1. "공급가액
  gs_scr100-label2 = ls_scr100-label2. "세액
  gs_scr100-label3 = ls_scr100-label3. "총액
  gs_scr100-label4 = ls_scr100-label4. "차이
  gs_scr100-label5 = ls_scr100-label5. "선급금
  gs_scr100-label6 = ls_scr100-label6. "지체상금액

  gs_expand-h      = abap_true.
  gs_expand-i      = abap_true.
  gs_expand-d      = abap_false.

  "왼쪽 ALV 표시
  CLEAR gv_visible_n.
  PERFORM switch_nav_cont.


ENDFORM. " REFRESH_ALL
*&---------------------------------------------------------------------*
*&      FORM  GET_USER_NAME_LIST
*&---------------------------------------------------------------------*
*       사용자명
*----------------------------------------------------------------------*
FORM get_user_name_list TABLES it_data STRUCTURE gs_head.

*  CLEAR gt_usernm.
*
*  DATA: BEGIN OF ls_employ_no,
*          employ_no LIKE zsvmm_user_info-employ_no,
*        END OF ls_employ_no,
*        lt_employ_no LIKE TABLE OF ls_employ_no.
*
*  LOOP AT it_data INTO DATA(ls_data).
*    IF NOT ls_data-zorder_person IS INITIAL.
*      ls_employ_no-employ_no = ls_data-zorder_person.
*      APPEND ls_employ_no TO lt_employ_no.
*    ENDIF.
*
*    IF NOT ls_data-zexpen_person IS INITIAL.
*      ls_employ_no-employ_no = ls_data-zexpen_person.
*      APPEND ls_employ_no TO lt_employ_no.
*    ENDIF.
*  ENDLOOP.
*
*  SORT lt_employ_no BY employ_no.
*  DELETE ADJACENT DUPLICATES FROM lt_employ_no COMPARING employ_no.
*  IF NOT lt_employ_no[] IS INITIAL.
*    SELECT employ_no, employ_name, department, depart_name, eml
*      FROM zsvmm_user_info
*       FOR ALL ENTRIES IN @lt_employ_no
*     WHERE employ_no = @lt_employ_no-employ_no
*      INTO CORRESPONDING FIELDS OF TABLE @gt_usernm.
*    SORT gt_usernm BY employ_no.
*    FREE lt_employ_no.
*  ENDIF.

ENDFORM. " GET_USER_NAME_LIST
*&---------------------------------------------------------------------*
*&      FORM  SET_DMBTR2
*&---------------------------------------------------------------------*
*       상단 헤더 공급가액 변경시 헤더 금액 및 세금 재계산
*----------------------------------------------------------------------*
FORM set_dmbtr2 USING iv_flag iv_change.

  IF gs_scr100-xmwst EQ abap_true OR
     iv_flag          EQ abap_true.
    PERFORM calculate_tax USING abap_true.
  ELSE.
    gs_scr100-total2    = gs_scr100-dmbtr2 + gs_scr100-wmwst2.
    gs_scr100-differenz = gs_scr100-dmbtr2 - gs_scr100-dmbtr.
  ENDIF.

  IF iv_change EQ abap_true.  "공급가액 변경된 경우
    PERFORM convert_total_amount_to_krw2.
  ELSE.
    PERFORM convert_total_amount_to_krw USING space. "KRW 환산금액
  ENDIF.

ENDFORM. " SET_DMBTR2
*&---------------------------------------------------------------------*
*&      FORM  SET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*       문서유형에 따른 ALV 레이아웃 기본값
*----------------------------------------------------------------------*
FORM set_default_variant .

  gs_variant-report    = sy-repid.
  gs_variant-username  = sy-uname.
  gs_variant-log_group = gc_struc_head.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'U'
    CHANGING
      cs_variant    = gs_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.


ENDFORM. " SET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      FORM  SET_LABEL
*&---------------------------------------------------------------------*
*       통화에 따른 라벨 적용
*----------------------------------------------------------------------*
FORM set_label USING iv_waers.

  gs_scr100-label1 = TEXT-t45 && '(' && iv_waers && ')'. "공급가
  gs_scr100-label2 = TEXT-t46 && '(' && iv_waers && ')'. "세액
  gs_scr100-label3 = TEXT-t47 && '(' && iv_waers && ')'. "총액
  gs_scr100-label4 = TEXT-t48 && '(' && iv_waers && ')'. "차이

  gs_scr100-label5 = TEXT-t49 && '(' && iv_waers && ')'. "선급반제금액
  gs_scr100-label6 = TEXT-t50 && '(' && iv_waers && ')'. "지체상금금액

ENDFORM. " SET_LABEL
*&---------------------------------------------------------------------*
*&      FORM  CONVERT_TOTAL_AMOUNT_TO_KRW
*&---------------------------------------------------------------------*
*       외화인 경우 KRW 환산 금액 계산
*       1원 차이 문제로 인해
*       라인별 세금 + 입고금액 을 합해서 KRW 로 환산한다.
*       환산금액을 더해서 총금액을 구한다.
*----------------------------------------------------------------------*
FORM convert_total_amount_to_krw USING iv_rate.

  DATA lv_local_currency     LIKE gs_scr100-total2.
  DATA lv_local_currency_sum LIKE gs_scr100-total2.
  DATA lt_tax                LIKE TABLE OF rtax1u15.
  DATA lv_fwste              LIKE bset-fwste.
  DATA lv_wrbtr              LIKE bseg-wrbtr.
  DATA lv_err(1)             TYPE c.

  CHECK p_r12            EQ abap_true.  "외화
  CHECK gs_scr100-waers2 NE gc_krw.

  IF gs_scr100-kursf IS INITIAL OR
     gs_scr100-wwert  IS INITIAL.
    "환율 또는 환율 적용일자를 입력하세요.
    MESSAGE s017 WITH TEXT-m37.
    EXIT.
  ENDIF.

  TRY.

      LOOP AT gt_head INTO gs_head WHERE cbox = abap_true.
        CLEAR: lv_local_currency, lv_wrbtr.

*       세금
        lv_wrbtr = gs_head-dmbtr.
        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs = gs_scr100-bukrs
            i_mwskz = gs_scr100-mwskz
            i_waers = gs_scr100-waers
            i_wrbtr = lv_wrbtr
          IMPORTING
            e_fwste = lv_fwste
          TABLES
            t_mwdat = lt_tax
          EXCEPTIONS
            OTHERS  = 13.

        IF sy-subrc NE 0.
          lv_err = abap_true.
          EXIT.
        ENDIF.

        gs_head-dmbtr = gs_head-dmbtr + lv_fwste.


*       KRW 환산금액
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            date             = gs_scr100-wwert
            foreign_amount   = gs_head-dmbtr
            foreign_currency = gs_scr100-waers2
            local_currency   = 'KRW'
            rate             = gs_scr100-kursf
          IMPORTING
            local_amount     = lv_local_currency
          EXCEPTIONS
            OTHERS           = 6.

        IF sy-subrc NE 0.
          lv_err = abap_true.
          EXIT.
        ENDIF.

        lv_local_currency_sum = lv_local_currency_sum
                              + lv_local_currency.
      ENDLOOP.

    CATCH cx_root.
      lv_err = abap_true.
  ENDTRY.

  IF lv_err EQ abap_true.
    "원화 환산 금액 계산중 오류가 발생하였습니다.
    MESSAGE s053 DISPLAY LIKE 'E' WITH TEXT-m38.
    CLEAR gs_scr100-total2_krw.
  ENDIF.

  WRITE lv_local_currency_sum TO gs_scr100-total2_krw CURRENCY gc_krw.


ENDFORM. " CONVERT_TOTAL_AMOUNT_TO_KRW
*&---------------------------------------------------------------------*
*&      FORM  CONVERT_TOTAL_AMOUNT_TO_KRW2
*&---------------------------------------------------------------------*
*      외화인 경우
*      공급가액 변경인 경우
*      변경된 공급가액에 세금을 더해 환율을 적용해서
*      KRW 환산 금액에 보여준다.
*----------------------------------------------------------------------*
FORM convert_total_amount_to_krw2.

  DATA lv_rate      TYPE invfo-kursf.
  DATA lv_total(16) TYPE p DECIMALS 2.
  DATA lv_err(1)    TYPE c.

  CHECK p_r12 EQ abap_true.  "외화

  IF gs_scr100-kursf IS INITIAL OR
      gs_scr100-wwert  IS INITIAL.
    "환율 또는 환율 적용일자를 입력하세요.
    MESSAGE s017 WITH TEXT-m37.
    EXIT.
  ENDIF.

  TRY.
      PERFORM currency_amount_idoc_to_sap  USING gs_scr100-kursf
                                                 gc_krw
                                        CHANGING lv_rate.

      lv_total = gs_scr100-total2 * lv_rate.
      WRITE lv_total TO gs_scr100-total2_krw CURRENCY gc_krw.

    CATCH cx_root.
      lv_err = abap_true.
  ENDTRY.

  IF lv_err EQ abap_true.
    "원화 환산 금액 계산중 오류가 발생하였습니다.
    MESSAGE s053 DISPLAY LIKE 'E' WITH TEXT-m38.
    CLEAR gs_scr100-total2_krw.
  ENDIF.

ENDFORM. " CONVERT_TOTAL_AMOUNT_TO_KRW2
*&---------------------------------------------------------------------*
*&      FORM  SET_NETDT_WITH_ZLSCH
*&---------------------------------------------------------------------*
*       지급방법 변경시 만기일 계산
*       표준을 그대로 사용하면 생성된 회계문서 만기일이
*       송장 처리시 보여지는 만기일과 다르기 때문에
*       아래 조건에 해당하는 경우 만기일을 다시 계산한다.
*----------------------------------------------------------------------*
FORM set_netdt_with_zlsch USING iv_date.

  CHECK gs_scr100-bldat IS NOT INITIAL.

  IF gs_scr100-zfbdt IS INITIAL.
    gs_scr100-zfbdt = iv_date.
    IF gs_scr100-zfbdt IS INITIAL.
      gs_scr100-zfbdt = gs_scr100-bldat.
    ENDIF.
  ENDIF.

ENDFORM. " SET_NETDT_WITH_ZLSCH
*&---------------------------------------------------------------------*
*&      FORM  CHANGE_LIFNR_LIPRE
*&---------------------------------------------------------------------*
*       공급업체 정발행/역발행 변경
*----------------------------------------------------------------------*
FORM change_lifnr_lipre.

  DATA: lv_absgr TYPE ekko-absgr,
        lv_text  TYPE char100,
        lv_chk   TYPE c.

  DATA: lv_total TYPE i,
        lv_succe TYPE i,
        lv_error TYPE i.

  DATA: BEGIN OF ls_ebeln,
          ebeln  LIKE ekko-ebeln,
          bsart  LIKE ekko-bsart,
          ekorg  LIKE ekko-ekorg,
          ekgrp  LIKE ekko-ekgrp,
          result TYPE c.
  DATA: END OF ls_ebeln,
  lt_ebeln LIKE TABLE OF ls_ebeln.

  DATA: ls_bapi_header  LIKE zsmm_poheader,
        ls_bapi_headerx LIKE zsmm_poheaderx,
        ls_bapi_result  LIKE bapimepoheader,
        lt_bapi_return  TYPE TABLE OF bapiret2,
        lt_show_msg     TYPE TABLE OF bapiret2.

  LOOP AT gt_head INTO gs_head WHERE cbox EQ abap_true.
    MOVE-CORRESPONDING gs_head TO ls_ebeln.
    APPEND ls_ebeln TO lt_ebeln.
  ENDLOOP.

  IF sy-subrc NE 0.
    MESSAGE s006.     "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  "역발행이면 정발행으로 그 외에는 역발행으로 변환
  IF p_r1 = 'X'.  "정발행이면 역발행
    lv_absgr = gc_absgr_02.
    lv_text = '역발행으로 변경하시겠습니까?'(m08).
  ELSE. "역발행이면 정발행
    lv_absgr = gc_absgr_01.
    lv_text = '정발행으로 변경하시겠습니까?'(m09).
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '확인'
      text_question         = lv_text
      text_button_1         = 'Yes'
      icon_button_1         = 'ICON_ALLOW'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_REJECT'
      default_button        = '2'
      display_cancel_button = space
      popup_type            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      answer                = lv_chk
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_chk EQ '1'.

*  DATA(LT_TMP) = L
  SORT lt_ebeln BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_ebeln COMPARING ebeln.

  LOOP AT lt_ebeln INTO ls_ebeln.

    DATA(lv_tabix) = sy-tabix.

    ADD 1 TO lv_total.

    CLEAR: ls_bapi_header, ls_bapi_headerx, ls_bapi_result, lt_bapi_return.

    ls_bapi_header-ebeln = ls_ebeln-ebeln.
    ls_bapi_header-bukrs = gs_scr100-bukrs.
    ls_bapi_header-esart = ls_ebeln-bsart.
    ls_bapi_header-ekorg = ls_ebeln-ekorg.
    ls_bapi_header-bkgrp = ls_ebeln-ekgrp.
    ls_bapi_header-absgr = lv_absgr.

    ls_bapi_headerx-ebeln = ls_ebeln-ebeln.
    ls_bapi_headerx-absgr = 'X'.

    call function 'ZFMM_PO_CHANGE'
      EXPORTING
        iv_ebeln     = ls_ebeln-ebeln
        is_poheader  = ls_bapi_header
        is_poheaderx = ls_bapi_headerx
      IMPORTING
        ev_header    = ls_bapi_result
      TABLES
        et_return    = lt_bapi_return.

    IF NOT ls_bapi_result-po_number IS INITIAL.
      ADD 1 TO lv_succe.
    ELSE.
      ADD 1 TO lv_error.
      APPEND LINES OF lt_bapi_return TO lt_show_msg.
    ENDIF.

    MODIFY lt_ebeln FROM ls_ebeln INDEX lv_tabix.
  ENDLOOP.

  IF NOT lt_show_msg[] IS INITIAL.
    CALL METHOD zcl_mm_common=>show_bapi_message( it_return = lt_show_msg[] ).
  ENDIF.


* 세금계산서 작성방법이 변경된 경우 해당업체 화면에서 삭제 및 왼쪽 금액등 재계산
  IF lv_succe > 0.
    PERFORM refresh_all.
  ENDIF.

*--> 결과 메시지 출력.
  IF lv_total = lv_succe.
    MESSAGE s009 WITH lv_total.
  ELSE.
    MESSAGE i014 WITH lv_total lv_succe lv_error.
  ENDIF.


ENDFORM. " CHANGE_LIFNR_LIPRE
*&---------------------------------------------------------------------*
*&      FORM  BLDAT_DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*       작성일자를 체크해서 화면에 업데이트한다.
*----------------------------------------------------------------------*
FORM bldat_dynp_values_update .

  CONSTANTS: lc_scr_bldat TYPE screen-name VALUE 'GS_SCR100-BLDAT'.

  DATA lt_dynp      TYPE TABLE OF dynpread.
  DATA lv_value(10) TYPE c.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = '0061'
      request    = 'A'
    TABLES
      dynpfields = lt_dynp
    EXCEPTIONS
      OTHERS     = 11.

  LOOP AT lt_dynp INTO DATA(ls_dynp).
    IF ls_dynp-fieldname NE lc_scr_bldat.
      DELETE lt_dynp.
      CONTINUE.
    ENDIF.
    IF ls_dynp-fieldvalue IS NOT INITIAL.
      lv_value = ls_dynp-fieldvalue.
      TRANSLATE lv_value USING gc_mask.
      CONDENSE lv_value NO-GAPS.
      gs_scr100-bldat = lv_value.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = gs_scr100-bldat
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.

  IF sy-subrc NE 0.
    CLEAR gs_scr100-bldat.
  ENDIF.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = '0061'
    TABLES
      dynpfields = lt_dynp
    EXCEPTIONS
      OTHERS     = 8.


ENDFORM. " BLDAT_DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      FORM  GET_CALCULATE_DATE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM get_calculate_date USING iv_date iv_sign
                         CHANGING cv_ndate.

  DATA lv_sign TYPE t5a4a-split.

  lv_sign = iv_sign.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = iv_date
      days      = '15'
      months    = '00'
      signum    = lv_sign
      years     = '00'
    IMPORTING
      calc_date = cv_ndate.


ENDFORM. " GET_CALCULATE_DATE
*&---------------------------------------------------------------------*
*&      FORM  SELECT_EKBE_WITH_EBELN
*&---------------------------------------------------------------------*
*       EKBE 테이블에서
*       조회된 구매오더에 대한 전체 이력을 가져온다.
*----------------------------------------------------------------------*
FORM select_ekbe_with_ebeln TABLES it_data STRUCTURE gs_head.

  DATA: BEGIN OF ls_data,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
        END OF ls_data,
        lt_data LIKE TABLE OF ls_data.

  CHECK it_data[] IS NOT INITIAL.

  MOVE-CORRESPONDING it_data[] TO lt_data[].
  SORT lt_data BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING ebeln ebelp.

  IF NOT lt_data[] IS INITIAL.
    SELECT ebeln ebelp vgabe
           gjahr belnr buzei
           shkzg bwart
           menge
           wrbtr AS dmbtr      "입고금액
          dmbtr AS dmbtr_krw  "현지금액
          wrbtr               "전표금액
          lfgja lfbnr lfpos
           cpudt cputm
           INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
           FROM ekbe
            FOR ALL ENTRIES IN lt_data
          WHERE ebeln EQ lt_data-ebeln
            AND ebelp EQ lt_data-ebelp
            AND vgabe IN gr_vgabe.
    FREE lt_data.
  ENDIF.


ENDFORM. " SELECT_EKBE_WITH_EBELN
*&---------------------------------------------------------------------*
*& Form SET_F4_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_listbox .

*> 회사코드 List box
  PERFORM set_f4_bukrs_listbox.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BUKRS_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_f4_bukrs_listbox .

  DATA: lv_name TYPE  vrm_id,
        lt_list TYPE  vrm_values.

  CONSTANTS: lc_scr_name TYPE screen-name VALUE 'P_BUKRS'.

  lv_name =  lc_scr_name.

  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
                                     it_where = VALUE #(
                                                         ( field = 1 value = 'BUKRS' )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).

  SELECT a~field2 AS key, a~field3 AS text
    FROM @lt_config AS a
   WHERE a~field2 NE @space
    INTO CORRESPONDING FIELDS OF TABLE @lt_list.

  SORT  lt_list  BY  key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_CREATE_OPTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_sel_create_option.

  CASE sy-ucomm.
    WHEN 'R2' OR 'R3' OR 'ONLI' OR 'R4'.

      CASE abap_true.
        WHEN p_rp1. "물품일반
           IF p_r2 EQ 'X'.  "역발행은 무조건 원화
             p_r11 = 'X'.
             CLEAR: p_r12.
           ENDIF.
        WHEN p_rp2. "물품선급
          "무조건 정발행
          p_r1 = abap_true.
          CLEAR p_r2.

          "무조건 원화
          p_r11 = abap_true.
          CLEAR p_r12.
        WHEN p_rp3. "자율납품&위탁정산
*
*          "무조건 역발행
          p_r2 = abap_true.
          CLEAR p_r1.

          "무조건 원화
          p_r11 = abap_true.
          CLEAR p_r12.


        WHEN p_rp4. "공사용역
          "무조건 정발행
          p_r1 = abap_true.
          CLEAR p_r2.

        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

  IF p_bukrs NE gc_bukrs_1101 AND p_rp3 = 'X'.
    CLEAR p_rp3.
    p_rp1 = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EKKN_LIST_ETC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_ekkn_list_etc .

  "GL 계정
  DATA(lt_tmp) = gt_ekkn[].
  SORT lt_tmp BY sakto.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING sakto.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT saknr, txt20
      FROM skat
       FOR ALL ENTRIES IN @lt_tmp
     WHERE saknr = @lt_tmp-sakto
       AND spras = @sy-langu
       AND ktopl = @gc_1000
      INTO TABLE @DATA(lt_skat).
    SORT lt_skat BY saknr.
    FREE lt_tmp.
  ENDIF.

  "손익센터
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY prctr kokrs.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING prctr kokrs.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT prctr, kokrs, ktext
      FROM cepct
      FOR ALL ENTRIES IN @lt_tmp
     WHERE prctr = @lt_tmp-prctr
       AND kokrs = @lt_tmp-kokrs
       AND spras = @sy-langu
      INTO TABLE @DATA(lt_cepct).
    FREE lt_tmp.
    SORT lt_cepct BY prctr kokrs.

  ENDIF.

  "코스트센터
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY kostl kokrs.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING kostl kokrs.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT kokrs, kostl, ktext
      FROM cskt
       FOR ALL ENTRIES IN @lt_tmp
     WHERE kostl = @lt_tmp-kostl
       AND kokrs = @lt_tmp-kokrs
       AND spras = @sy-langu
      INTO TABLE @DATA(lt_cskt).
    FREE lt_tmp.
    SORT lt_cskt BY kostl kokrs.
  ENDIF.

  "Fund Center
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY fistl kokrs.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING fistl kokrs.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT fikrs, fictr, bezeich
      FROM fmfctrt
       FOR ALL ENTRIES IN @lt_tmp
     WHERE fikrs = @lt_tmp-kokrs
       AND fictr = @lt_tmp-fistl
       AND spras = @sy-langu
       AND datbis >= @sy-datum
       AND datab  <= @sy-datum
      INTO TABLE @DATA(lt_fmfctrt).
    FREE lt_tmp.
    SORT lt_fmfctrt BY fictr fikrs.
  ENDIF.

  "자산번호
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY anln1 kokrs.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING anln1 kokrs.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT anln1, bukrs, txt50
      FROM anla
       FOR ALL ENTRIES IN @lt_tmp
     WHERE anln1 = @lt_tmp-anln1
       AND bukrs = @lt_tmp-kokrs
      INTO TABLE @DATA(lt_anla).
    FREE lt_tmp.
    SORT lt_anla BY anln1 bukrs.
  ENDIF.

  "WBS 요소
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY pspnr.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING pspnr.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT pspnr, post1
      FROM prps
       FOR ALL ENTRIES IN @lt_tmp
     WHERE pspnr = @lt_tmp-pspnr
      INTO TABLE @DATA(lt_prps).
    FREE lt_tmp.
    SORT lt_prps BY pspnr.
  ENDIF.

  "Internal Order
  lt_tmp[] = gt_ekkn[].
  SORT lt_tmp BY aufnr.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING aufnr.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT aufnr, ktext
      FROM aufk
       FOR ALL ENTRIES IN @lt_tmp
     WHERE aufnr = @lt_tmp-aufnr
      INTO TABLE @DATA(lt_aufk).
    FREE lt_tmp.
    SORT lt_aufk BY aufnr.
  ENDIF.

  LOOP AT gt_ekkn INTO gs_ekkn.

    "GL 계정
    READ TABLE lt_skat INTO DATA(ls_skat)
                       WITH KEY saknr = gs_ekkn-sakto
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-sakto_tx = ls_skat-txt20.
    ENDIF.

    "손익센터
    READ TABLE lt_cepct INTO DATA(ls_cepct)
                       WITH KEY prctr = gs_ekkn-prctr
                                kokrs = gs_ekkn-kokrs
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-prctr_tx = ls_cepct-ktext.
    ENDIF.

    "코스트센터
    READ TABLE lt_cskt INTO DATA(ls_cskt)
                       WITH KEY kostl = gs_ekkn-kostl
                                kokrs = gs_ekkn-kokrs
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-kostl_tx = ls_cskt-ktext.
    ENDIF.

    "Fund Center
    READ TABLE lt_fmfctrt INTO DATA(ls_fmfctrt)
                       WITH KEY fictr = gs_ekkn-fistl
                                fikrs = gs_ekkn-kokrs
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-fistl_tx = ls_fmfctrt-bezeich.
    ENDIF.

    "자산번호
    READ TABLE lt_anla INTO DATA(ls_anla)
                       WITH KEY anln1 = gs_ekkn-anln1
                                bukrs = gs_ekkn-kokrs
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-anln1_tx = ls_anla-txt50.
    ENDIF.

    "WBS 요소
    READ TABLE lt_prps INTO DATA(ls_prps)
                       WITH KEY pspnr = gs_ekkn-pspnr
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-pspnr_tx = ls_prps-post1.
    ENDIF.

    "Internal Order
    READ TABLE lt_aufk INTO DATA(ls_aufk)
                       WITH KEY aufnr = gs_ekkn-aufnr
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_ekkn-aufnr_tx = ls_aufk-ktext.
    ENDIF.

    MODIFY gt_ekkn FROM gs_ekkn.
  ENDLOOP.

  SORT gt_ekkn BY ebeln ebelp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_FOR_POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_for_post .

  CLEAR: gt_t001w, gt_prepaid, gt_po_etc_amt.

*> 플랜트 별 사업장 정보
  DATA(lt_tmp) = gt_head[].
  SORT lt_tmp BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING werks.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT werks, j_1bbranch
      FROM t001w
       FOR ALL ENTRIES IN @lt_tmp
     WHERE werks = @lt_tmp-werks
      INTO CORRESPONDING FIELDS OF TABLE @gt_t001w.
    SORT gt_t001w BY werks.
    FREE lt_tmp.
  ENDIF.

*> GET 선급미결 AP
  lt_tmp = gt_head[].
  SORT lt_tmp BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING ebeln ebelp.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT ebeln, ebelp, vgabe, shkzg, dmbtr
      FROM ekbe
       FOR ALL ENTRIES IN @lt_tmp
     WHERE ebeln EQ @lt_tmp-ebeln
       AND ebelp EQ @lt_tmp-ebelp
       AND vgabe IN ('4','C')  "선금/선금반제
      INTO CORRESPONDING FIELDS OF TABLE @gt_prepaid.

    SORT gt_prepaid BY ebeln ebelp.
    FREE lt_tmp.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_EMPLOY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_sel_scr_employ USING iv_scr_name.


  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '유저정보',
             lc_retfield  TYPE fieldname VALUE 'EMPLOY_NO'.

  PERFORM get_dynp_value USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT employ_no, employ_name, department, depart_name
*    FROM zsvmm_user_info
*   WHERE company EQ @p_bukrs
*    INTO TABLE @DATA(lt_user_info).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = iv_scr_name        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
*      value_tab         = lt_user_info
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

*  SORT lt_user_info BY employ_no.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
*    READ TABLE lt_user_info INTO DATA(ls_user_info)
*                        WITH KEY employ_no = ls_return-fieldval
*                        BINARY SEARCH.

    "사번
    CLEAR ls_update.
    ls_update-fieldname = iv_scr_name.
*    ls_update-fieldvalue  = ls_user_info-employ_no.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_DEPART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_sel_scr_depart USING iv_scr_name
                                  iv_scr_desc.


  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '부서정보',
             lc_retfield  TYPE fieldname VALUE 'DEPARTMENT'.

  PERFORM get_dynp_value USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT DISTINCT
*         department, depart_name
*    FROM zsvmm_user_info
*   WHERE company EQ @p_bukrs
*    INTO TABLE @DATA(lt_depart_info).
*
*  SORT lt_depart_info BY department.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = iv_scr_name        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
*      value_tab         = lt_depart_info
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
*    READ TABLE lt_depart_info INTO DATA(ls_depart_info)
*                        WITH KEY department = ls_return-fieldval
*                        BINARY SEARCH.
*
*    "부서
*    CLEAR ls_update.
*    ls_update-fieldname = iv_scr_name.
*    ls_update-fieldvalue  = ls_depart_info-department.
*    APPEND ls_update TO lt_update.
*
*    "부서명
*    IF NOT iv_scr_desc IS INITIAL.
*      CLEAR ls_update.
*      ls_update-fieldname = iv_scr_desc.
*      ls_update-fieldvalue  = ls_depart_info-depart_name.
*      APPEND ls_update TO lt_update.
*    ENDIF.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DYNP_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_dynp_value USING iv_scr_name
                     CHANGING ev_value.

  DATA: lt_dynpfields TYPE TABLE OF dynpread,
        ls_dynpfields TYPE dynpread.

  ls_dynpfields-fieldname = iv_scr_name.
  APPEND ls_dynpfields TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc = 0.
    SORT lt_dynpfields BY fieldname.
    READ TABLE lt_dynpfields INTO ls_dynpfields
                             WITH KEY fieldname = iv_scr_name
                             BINARY SEARCH.
    IF sy-subrc = 0.
      ev_value = ls_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_BUPLA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_sel_scr_bupla USING iv_scr_name..


  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '사업장',
             lc_retfield  TYPE fieldname VALUE 'BUPLA'.

  PERFORM get_dynp_value USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT branch AS bupla,
         name
    FROM j_1bbranch
   WHERE bukrs EQ @p_bukrs
    INTO TABLE @DATA(lt_bupla).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = iv_scr_name        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = lt_bupla
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  SORT lt_bupla BY bupla.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    READ TABLE lt_bupla INTO DATA(ls_bupla)
                        WITH KEY bupla = ls_return-fieldval
                        BINARY SEARCH.

    CLEAR ls_update.
    ls_update-fieldname = iv_scr_name.
    ls_update-fieldvalue  = ls_bupla-bupla.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_TAXIM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_sel_scr_taxim USING iv_scr_name..


  DATA: lt_return TYPE TABLE OF ddshretval.

  CONSTANTS: lc_title(15) TYPE c VALUE '세금지시자',
             lc_retfield  TYPE fieldname VALUE 'TAXIM',
             lc_kr        TYPE tmkm1t-land1 VALUE 'KR'.

  SELECT taxim,
         taxib
    FROM tmkm1t
   WHERE spras EQ @sy-langu
     AND land1 EQ @lc_kr
    INTO TABLE @DATA(lt_tmkm1t).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = iv_scr_name        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = lt_tmkm1t
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EACC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_eacc_data .

***********************************************************************
**> 확인 필 : 임시로직 - 전자세금계산서 강제 생성.(open 전 로직 삭제 예정)
***********************************************************************
*  IF gs_scr100-lifnr IS INITIAL.
*    MESSAGE s000 WITH '공급업체를 선택하세요!'(z01) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gs_scr100-dmbtr2 IS INITIAL.
*    MESSAGE s000 WITH '공급가액이 없습니다!'(z02) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gs_scr100-bupla IS INITIAL.
*    MESSAGE s000 WITH '사업장을 입력하세요!'(z03) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gs_scr100-bldat IS INITIAL.
*    MESSAGE s000 WITH '작성일자를 입력하세요!'(z04) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gs_scr100-mwskz IS INITIAL.
*    MESSAGE s000 WITH '세금코드를 입력하세요!'(z05) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
************************************************************************
***> HEADER 데이터
************************************************************************
*  DATA: ls_header LIKE zdtv3t_ap_head,
*        lv_seq(6) TYPE n.
*
*  CONSTANTS: lc_erdat LIKE zdtv3t_ap_head-erdat VALUE '20210329',
*             lc_erzet LIKE zdtv3t_ap_head-erzet VALUE '130000'.
*
*  ls_header-bukrs =  p_bukrs.
*  ls_header-issue_date =  gs_scr100-bldat.
*  ls_header-bupla =  gs_scr100-bupla.
*
*  SELECT MAX( inv_seq )
*    FROM zdtv3t_ap_head
*   WHERE bukrs = @p_bukrs
*     AND issue_date = @gs_scr100-bldat
*     AND bupla = @gs_scr100-bupla
*     AND erdat >= @lc_erdat
*     AND erzet >= @lc_erzet
*     AND su_id = @gs_scr100-stceg
*    INTO @DATA(lv_max_seq).
*
*  lv_seq = lv_max_seq+18(6).
*  lv_seq = lv_seq + 1.
*
*  ls_header-inv_seq = sy-datum && gs_scr100-lifnr && lv_seq.
*
*  ls_header-type_code = '0101'(z06).
*  ls_header-purp_code = '01'(z07).
*
**> 공급자 정보
*  DATA: ls_lfa1      TYPE lfa1.
*
*  IF NOT gs_scr100-diff_lifnr IS INITIAL.
*    CALL FUNCTION 'WY_LFA1_SINGLE_READ'
*      EXPORTING
*        pi_lifnr = gs_scr100-diff_lifnr
*      IMPORTING
*        po_lfa1  = ls_lfa1.
*
*    ls_header-su_id = ls_lfa1-stcd2.
*  ELSE.
*    CALL FUNCTION 'WY_LFA1_SINGLE_READ'
*      EXPORTING
*        pi_lifnr = gs_scr100-lifnr
*      IMPORTING
*        po_lfa1  = ls_lfa1.
*
*    ls_header-su_id = gs_scr100-stceg.
*  ENDIF.
*
*  ls_header-su_name = ls_lfa1-name1.    "사업자명
*  ls_header-su_addr = ls_lfa1-ort01 && ls_lfa1-stras.  "주소
*
*  SELECT person_email1, person_email2, repres, gestyp, indtyp
*    FROM idkr_venvat
*   WHERE lifnr = @ls_lfa1-lifnr
*     AND datab <= @sy-datum
*   ORDER BY datab DESCENDING
*    INTO TABLE @DATA(lt_lfa1_etc).
*
*  SELECT SINGLE intad
*    FROM lfb1
*   WHERE lifnr = @ls_lfa1-lifnr
*     AND bukrs = @gs_scr100-bukrs
*    INTO @ls_header-su_email.
*
*  READ TABLE lt_lfa1_etc INTO DATA(ls_lfa1_etc) INDEX 1.
*  IF sy-subrc EQ 0.
*    ls_header-su_repres = ls_lfa1_etc-repres.  "대표자명
*    IF ls_header-su_repres IS INITIAL.
*      ls_header-su_repres = ls_lfa1-lifnr.
*    ENDIF.
*
*    ls_header-su_bustype = ls_lfa1_etc-indtyp.  "업태
*    ls_header-su_indtype = ls_lfa1_etc-gestyp.  "업종
*  ELSE.
*    ls_header-su_repres = gs_scr100-lifnr.
*  ENDIF.
*
*
*
**> 공급받는자 정보
*  SELECT SINGLE taxnumber2, businessplacename, taxinvoicerepresentativename,
*                addressname, industrytype, businesstype
*    FROM pkrbupla
*   WHERE branch = @gs_scr100-bupla
*     AND companycode = @gs_scr100-bukrs
*    INTO @DATA(ls_pkrbupla).
*
*  ls_header-ip_typecode = '01'(z07).  "사업자등록번호로 등록.
*  ls_header-ip_id = ls_pkrbupla-taxnumber2.  "사업자등록번호.
*  ls_header-ip_name = ls_pkrbupla-businessplacename.  "사업체명.
*  ls_header-ip_repres = ls_pkrbupla-taxinvoicerepresentativename.  "대표자명.
*  ls_header-ip_addr = ls_pkrbupla-addressname.  "주소.
*  ls_header-ip_bustype = ls_pkrbupla-industrytype.  "업태.
*  ls_header-ip_indtype = ls_pkrbupla-businesstype.  "업종
*
*  ls_header-ip_email1 = 'aaa@email.com'(z09).
*  ls_header-ip_persname2 = sy-uname.
*
*
**> 금액 정보
*  ls_header-chargetotal = gs_scr100-dmbtr2.
*  ls_header-taxtotal = gs_scr100-wmwst2.
*  ls_header-grandtotal = ls_header-chargetotal + ls_header-taxtotal.
*
**> 상태
*  ls_header-issue_id = 'ID'(z10) && ls_header-inv_seq.
*  ls_header-status = '2797'(z11).
*  ls_header-proc_step = '32'(z12).
*  ls_header-nts_send_flag = '31'(z13).
*
**> LOG
*  ls_header-erdat = sy-datum.
*  ls_header-erzet = sy-uzeit.
*  ls_header-ernam = sy-uname.
*
*  MODIFY zdtv3t_ap_head FROM ls_header.
*
*
***********************************************************************
**> ITEM 데이터
***********************************************************************
*  DATA: lt_item     LIKE TABLE OF zdtv3t_ap_item,
*        ls_item     LIKE zdtv3t_ap_item,
*        lv_good_seq LIKE zdtv3t_ap_item-good_seq.
*
*  CONSTANTS: lc_10 TYPE i VALUE '10'.
*
*  LOOP AT gt_head INTO DATA(ls_head).
*    CLEAR ls_item.
*    MOVE-CORRESPONDING ls_header TO ls_item.
*
*    lv_good_seq = lv_good_seq + 1.
*
*    ls_item-good_seq = lv_good_seq.
*    ls_item-good_date = ls_head-budat.
*    ls_item-good_name = ls_head-matnr_tx.
*    ls_item-good_quan = ls_head-menge.
*    ls_item-good_uniamount = ls_head-netpr.
*    ls_item-good_invamount = ls_head-dmbtr.
*
*    IF gs_scr100-wmwst2 NE 0.
*      TRY.
*          ls_item-good_taxamount = ls_item-good_invamount / lc_10.
*        CATCH cx_sy_zerodivide.
*      ENDTRY.
*    ENDIF.
*
*    APPEND ls_item TO lt_item.
*  ENDLOOP.
*
*  MODIFY zdtv3t_ap_item FROM TABLE lt_item.
*
***********************************************************************
**> 매입 추가정보
***********************************************************************
*  DATA: ls_etc LIKE zdtv3t_ap_ext.
*
*  MOVE-CORRESPONDING ls_header TO ls_etc.
*
*  MODIFY zdtv3t_ap_ext FROM ls_etc.
*
*  IF sy-subrc EQ 0.
*    MESSAGE s003 WITH '저장 성공:'(z14) ls_header-inv_seq.
*  ENDIF.
*
*  COMMIT WORK.
*
*  PERFORM refresh_all.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  GL_ACCOUNT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM gl_account_data TABLES et_gl_acc STRUCTURE zsmm_incinv_create_gl_account
                  USING  is_head LIKE gs_head
                         iv_item_no.

  "의제매입세 GL ACC 세금코드 : Y2
  CONSTANTS: lc_saknr LIKE zsmm_incinv_create_gl_account-gl_account VALUE '11110201',
             lc_mwskz LIKE zsmm_incinv_create_gl_account-tax_code VALUE 'Y2',
             lc_sgtxt LIKE zsmm_incinv_create_gl_account-item_text VALUE '의제매입세'.

  DATA: ls_gl_acc LIKE zsmm_incinv_create_gl_account.

*--------------------------------------------------------------------*
*> 의제매입세 차변
*--------------------------------------------------------------------*
  CLEAR ls_gl_acc.
  ls_gl_acc-invoice_doc_item  = iv_item_no.  "ITEM NO

  "G/L 계정
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lc_saknr
    IMPORTING
      output = ls_gl_acc-gl_account.

  "의제매입세
  PERFORM bapi_currency_conv_to_external USING    is_head-svat_amt
                                                  gs_scr100-waers
                                         CHANGING ls_gl_acc-item_amount.

  ls_gl_acc-db_cr_ind  = 'S'.                "차/대변 구분
  ls_gl_acc-comp_code  = gs_scr100-bukrs.    "회사코드
  ls_gl_acc-tax_code  = lc_mwskz.            "세금코드
  ls_gl_acc-item_text  = lc_sgtxt.           "ITEM TEXT

  "과세표준액
  PERFORM bapi_currency_conv_to_external USING    is_head-dmbtr
                                                  gs_scr100-waers
                                         CHANGING ls_gl_acc-tax_base_amount.

  APPEND ls_gl_acc TO et_gl_acc.

ENDFORM. " GL_ACCOUNT_DATA
*&---------------------------------------------------------------------*
*& Form MATERIAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM material_data TABLES et_matdata STRUCTURE zsmm_incinv_create_material
                  USING  is_head LIKE gs_head
                         iv_item_no.

  "의제매입세 자재 세금코드 : YY
  CONSTANTS: lc_mwskz LIKE zsmm_incinv_create_material-tax_code VALUE 'YY'.

  DATA: ls_matdata LIKE zsmm_incinv_create_material.

*--------------------------------------------------------------------*
*> 의제매입세 대변
*--------------------------------------------------------------------*
  CLEAR ls_matdata.
  ls_matdata-invoice_doc_item  = iv_item_no.     "ITEM NO
  ls_matdata-val_area  = is_head-werks.          "평가 영역
  ls_matdata-valuation_type  = is_head-bwtar.    "평가 유형
  ls_matdata-db_cr_ind  = 'H'.                   "차/대변 구분
  "의제매입세
  PERFORM bapi_currency_conv_to_external USING    is_head-svat_amt
                                                  gs_scr100-waers
                                         CHANGING ls_matdata-item_amount.

  ls_matdata-quantity  = is_head-menge.          "입고수량
  ls_matdata-base_uom  = is_head-meins.          "수량 단위
  ls_matdata-tax_code  = lc_mwskz.               "세금코드
  ls_matdata-material  = is_head-matnr.          "자재 번호
  APPEND ls_matdata TO et_matdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  DIRECT_MAPPING_INVOICE
*&---------------------------------------------------------------------*
*       생성된 전표를 MAPPING 하기 위한 프로그램을 호출한다.
*----------------------------------------------------------------------*
FORM direct_mapping_invoice CHANGING cv_mapping_sucss.

**  DATA LR_LIFNR  TYPE RANGE OF LIFNR.
**  DATA LR_GJAHR  TYPE RANGE OF GJAHR   .
**  DATA LR_BLART  TYPE RANGE OF BLART   .
**  DATA LR_BUDAT  TYPE RANGE OF BUDAT   .
**  DATA LR_BELNR  TYPE RANGE OF BELNR_D .
**  DATA LR_USNAM  TYPE RANGE OF USNAM   .
**  DATA LR_STCD2  TYPE RANGE OF ZDTV3E_SU_ID.
*
*  DATA lv_belnr  TYPE bkpf-belnr.
*  DATA lv_gjahr  TYPE bkpf-gjahr.
*  DATA lv_awkey  TYPE bkpf-awkey.
*  DATA lv_lifnr  TYPE lfa1-lifnr.
*  DATA lv_stcd2  TYPE lfa1-stcd2.
*
*  DATA: ls_return   TYPE zdtv3s_return,
*        lt_ap_table TYPE TABLE OF zdtv3t_ap_ext_d,
*        ls_ap_table TYPE zdtv3t_ap_ext_d.
*
*  lv_awkey = gs_head-belnr2 && gs_head-gjahr2.
*  CONDENSE lv_awkey NO-GAPS.
*
*
**-회계전표번호
*  SELECT SINGLE belnr gjahr
*          INTO (lv_belnr,
*                lv_gjahr)
*          FROM bkpf
*         WHERE bukrs = p_bukrs
*           AND blart = gc_re
*           AND bldat = gs_scr100-bldat
*           AND budat = gs_scr100-budat
*           AND awkey = lv_awkey
*           AND cpudt = sy-datum
*           AND usnam = sy-uname.
*
*
**  _G_APPEND3 LR_GJAHR 'I' GC_EQ LV_GJAHR.
**  _G_APPEND3 LR_BLART 'I' GC_EQ GC_RE.
**  _G_APPEND3 LR_BUDAT 'I' GC_EQ GS_SCR100-BUDAT.
**  _G_APPEND3 LR_BELNR 'I' GC_EQ LV_BELNR.
**  _G_APPEND3 LR_USNAM 'I' GC_EQ SY-UNAME.
*
*  "송장발행처
*  SELECT SINGLE b~lifnr b~stcd2
*            INTO (lv_lifnr,
*                  lv_stcd2)
*            FROM rbkp AS a INNER JOIN lfa1 AS b
*              ON a~lifnr = b~lifnr
*           WHERE a~belnr EQ gs_scr100-belnr(10)
*             AND a~gjahr EQ gs_scr100-belnr+11(*).
*
**  _G_APPEND3 LR_LIFNR 'I' GC_EQ LV_LIFNR.
**  _G_APPEND3 LR_STCD2 'I' GC_EQ LV_STCD2.
*
***U4> 2022.05.02 : 전자세금계산서 맵핑 시 영/면세 구분 - START - 제거(원복)
*  SELECT SINGLE a~inv_seq
*           FROM zdtv3t_ap_head AS a INNER JOIN zdtv3t_ap_ext AS b
*             ON a~bukrs        = b~bukrs          AND
*                a~issue_date   = b~issue_date     AND
*                a~bupla        = b~bupla          AND
*                a~inv_seq      = b~inv_seq
*          WHERE a~bukrs        = @gs_scr100-bukrs
*            AND a~bupla        = @gs_scr100-bupla
*            AND a~su_id        = @lv_stcd2
*            AND a~issue_date   = @gs_scr100-budat
*            AND b~use_doc      = @space
*            AND a~chargetotal  = @gs_scr100-dmbtr
*    INTO @DATA(lv_inv_seq).
*
*  CHECK sy-subrc EQ 0.
*
**재무운영팀에서 영세/면세 세금계산서 MAPPING 처리 프로그램을 수
**정예정으로 MM 기능구현 불필요하여 로직 막음...
**  DATA LV_INV_SEQ TYPE ZDTV3T_AP_HEAD-INV_SEQ.
***> OLD
**  IF GS_CONFIG_E1013-FIELD10 IS INITIAL.
**    SELECT SINGLE A~INV_SEQ
**             FROM ZDTV3T_AP_HEAD AS A INNER JOIN ZDTV3T_AP_EXT AS B
**               ON A~BUKRS        = B~BUKRS          AND
**                  A~ISSUE_DATE   = B~ISSUE_DATE     AND
**                  A~BUPLA        = B~BUPLA          AND
**                  A~INV_SEQ      = B~INV_SEQ
**            WHERE A~BUKRS        = @GS_SCR100-BUKRS
**              AND A~BUPLA        = @GS_SCR100-BUPLA
**              AND A~SU_ID        = @LV_STCD2
**              AND A~ISSUE_DATE   = @GS_SCR100-BUDAT
**              AND B~USE_DOC      = @SPACE
**              AND A~CHARGETOTAL  = @GS_SCR100-DMBTR
**      INTO @LV_INV_SEQ.
**
**    CHECK SY-SUBRC EQ 0.
**  ELSE.
***> NEW
**    CALL METHOD ZCL_MM_COMMON=>GET_EACC_INV_SEQ(
**      EXPORTING
**        IV_BUKRS     = GS_SCR100-BUKRS
**        IV_BUPLA     = GS_SCR100-BUPLA
**        IV_STCD2     = LV_STCD2
**        IV_BUDAT     = GS_SCR100-BUDAT
**        IV_MWSKZ     = GS_SCR100-MWSKZ
**        IV_DMBTR     = CONV DMBTR( GS_SCR100-DMBTR )
**      IMPORTING
**        ES_EACC_DATA = DATA(LS_EACC_DATA)
**      RECEIVING
**        RV_SUBRC     = DATA(LV_SUBRC) ).
**
**    CHECK LV_SUBRC EQ 0.
**
**    LV_INV_SEQ = LS_EACC_DATA-INV_SEQ.
**  ENDIF.
**
***U4> 2022.05.02 : 전자세금계산서 맵핑 시 영/면세 구분 - END
*
*  CLEAR ls_ap_table.
*  ls_ap_table-inv_seq = lv_inv_seq.
*  ls_ap_table-ext_seq = 1.
*  ls_ap_table-zgjah = lv_gjahr.
*  ls_ap_table-belnr = lv_belnr.
*  ls_ap_table-bukrs = gs_scr100-bukrs.
*  ls_ap_table-budat = gs_scr100-budat.
*  ls_ap_table-blart = gc_re.
*  ls_ap_table-mwskz = gs_scr100-mwskz.
*  ls_ap_table-dmbtr = gs_scr100-total.
*
*  APPEND ls_ap_table TO lt_ap_table.
*
*  call function 'ZDTV3SAP_USER_DOCUMAP'
*    EXPORTING
*      inv_seq  = ls_ap_table-inv_seq
**     ISSUE_ID =
*      inv_type = 'AP'
**     INV_DEL  =
*    IMPORTING
*      return   = ls_return
*    TABLES
*      it_data  = lt_ap_table.
*
*  IF ls_return-type EQ 'S'.
*    cv_mapping_sucss = 'S'.
*    MESSAGE i003 WITH '전자세금계산서 맵핑이 완료되었습니다:'(m41)
*                      space ls_ap_table-inv_seq.
*  ELSE.
*    cv_mapping_sucss = 'E'.
*  ENDIF.


ENDFORM. "DIRECT_MAPPING_INVOICE
*&---------------------------------------------------------------------*
*& Form AP_SPLIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM ap_split_data TABLES et_ap_split STRUCTURE zsmm_incinv_create_vendorsplit.

  CONSTANTS: lc_zterm_xx01 TYPE dzterm VALUE 'XX01',
             lc_zterm_xx03 TYPE dzterm VALUE 'XX03'.

  DATA: ls_ap_split TYPE zsmm_incinv_create_vendorsplit,
        lv_amount   TYPE rm08m-differenz.

  CLEAR et_ap_split.

  "외상매입금 (입고금액 - 선급금)
  CLEAR: ls_ap_split.

  "잔액 AP (공급가 + 세액 - 선급금 - 지체상금)
  lv_amount = ( gs_scr100-dmbtr2 + gs_scr100-wmwst2 ) - gs_scr100-dwpayc.
  lv_amount = lv_amount - gs_scr100-delayf.

  PERFORM bapi_currency_conv_to_external USING    lv_amount
                                                  gs_scr100-waers
                                         CHANGING ls_ap_split-split_amount.

  ls_ap_split-tax_code = gs_scr100-mwskz.       "AP 세금코드
  ls_ap_split-pmnttrms = gs_scr100-zterm.       "지급조건
  ls_ap_split-pymt_meth = gs_scr100-zlsch.      "지급방법
  APPEND ls_ap_split TO et_ap_split.

  "선급금
  IF gs_scr100-dwpayc NE 0.
    CLEAR: ls_ap_split.
    PERFORM bapi_currency_conv_to_external USING    gs_scr100-dwpayc
                                                      gs_scr100-waers
                                             CHANGING ls_ap_split-split_amount.

    IF NOT gs_scr100-dwtax IS INITIAL.
      ls_ap_split-tax_code = gs_scr100-dwtax.       "선급금 세금코드
    ELSE.
      ls_ap_split-tax_code = gs_scr100-mwskz.       "일반 세금코드
    ENDIF.

    ls_ap_split-pmnttrms = lc_zterm_xx01.         "지급조건 (선급반제)
    ls_ap_split-pymt_meth = gs_scr100-zlsch.      "지급방법
    APPEND ls_ap_split TO et_ap_split.
  ENDIF.

  "지체상금
  IF gs_scr100-delayf NE 0.
    CLEAR: ls_ap_split.
    ls_ap_split-split_amount = gs_scr100-delayf.  "지체상금
    PERFORM bapi_currency_conv_to_external USING    gs_scr100-delayf
                                                      gs_scr100-waers
                                             CHANGING ls_ap_split-split_amount.

    ls_ap_split-tax_code = gs_scr100-mwskz.       "선급금 세금코드
    ls_ap_split-pmnttrms = lc_zterm_xx03.         "지급조건 (지체상금 반제)
    ls_ap_split-pymt_meth = gs_scr100-zlsch.      "지급방법
    APPEND ls_ap_split TO et_ap_split.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_REQ_RE_AP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_req_re_ap TABLES it_item STRUCTURE zsmm_incinv_create_item.

*  DATA lv_belnr  TYPE bkpf-belnr.
*  DATA lv_gjahr  TYPE bkpf-gjahr.
*  DATA lv_awkey  TYPE bkpf-awkey.
*  DATA lv_lifnr  TYPE lfa1-lifnr.
*  DATA lv_stcd2  TYPE lfa1-stcd2.
*
*  lv_awkey = gs_scr100-belnr.
*  CONDENSE lv_awkey NO-GAPS.
*
*  "금액이 마이너스면 역발행 하지 않음 (반품포함)
*  CHECK gs_scr100-dmbtr2 > 0.
*
**-회계전표번호
*  SELECT SINGLE belnr gjahr
*          INTO (lv_belnr,
*                lv_gjahr)
*          FROM bkpf
*         WHERE bukrs = p_bukrs
*           AND blart = gc_re
*           AND bldat = gs_scr100-bldat
*           AND budat = gs_scr100-budat
*           AND awkey = lv_awkey
*           AND cpudt = sy-datum
*           AND usnam = sy-uname.
*
*
*  "송장발행처
*  SELECT SINGLE b~lifnr b~stcd2
*            INTO (lv_lifnr,
*                  lv_stcd2)
*            FROM rbkp AS a INNER JOIN lfa1 AS b
*              ON a~lifnr = b~lifnr
*           WHERE a~belnr EQ gs_scr100-belnr(10)
*             AND a~gjahr EQ gs_scr100-belnr+11(*).
*
*  CHECK sy-subrc EQ 0.
*
***********************************************************************
**> HEADER DATA 구성
***********************************************************************
*  DATA: lt_ap_header TYPE TABLE OF zdtv3s_ap_hd,
*        ls_ap_header TYPE zdtv3s_ap_hd,
*        ls_lfa1      TYPE lfa1.
*
*  CONSTANTS:
*    lc_type_code_0101 TYPE zdtv3s_ap_hd-type_code VALUE '0101',  "일반
*    lc_type_code_0102 TYPE zdtv3s_ap_hd-type_code VALUE '0102', "영세
*    lc_purp_code_02   TYPE zdtv3s_ap_hd-purp_code VALUE '02', "청구
*    lc_ip_typecode_01 TYPE zdtv3s_ap_hd-ip_typecode VALUE '01', "사업자등록으로 진행
*    lc_type_code_0301 TYPE zdtv3s_ap_hd-type_code VALUE '0301'.  "일반계산서
*
*  CLEAR: ls_ap_header, lt_ap_header.
*  ls_ap_header-bukrs = gs_scr100-bukrs.
*  ls_ap_header-issue_date = gs_scr100-bldat.
*  ls_ap_header-bupla = gs_scr100-bupla.
*  ls_ap_header-inv_sign = 'X'. "역발행 구분
*
**U5> 역발행시 의제매입 존재하거나 면세면 일반계산서로 발행
**  IF GS_SCR100-WMWST2 EQ 0.
**    LS_AP_HEADER-TYPE_CODE = LC_TYPE_CODE_0102. "영세.
**  ELSE.
**    LS_AP_HEADER-TYPE_CODE = LC_TYPE_CODE_0101. "일반
**  ENDIF.
*
*  IF gv_incl_taxim_y = 'Y' OR   "의제매입대상이거나
*     gs_scr100-mwskz = 'Y0' OR  "면세이면..
*     gs_scr100-mwskz = 'Y1' OR
*     gs_scr100-mwskz = 'Y2'.
*    ls_ap_header-type_code = lc_type_code_0301. "일반계산서
*  ELSE.
*    IF gs_scr100-wmwst2 EQ 0.
*      ls_ap_header-type_code = lc_type_code_0102. "영세.
*    ELSE.
*      ls_ap_header-type_code = lc_type_code_0101. "일반
*    ENDIF.
*  ENDIF.
*
*
*
*  ls_ap_header-purp_code = '02'. "무조건 청구
*
**> 공급자 정보
**U2> 2022.04.22 : 송장발행처 존재 시 우선.
**  LS_AP_HEADER-SU_ID = GS_SCR100-STCEG.
**
**  CALL FUNCTION 'WY_LFA1_SINGLE_READ'
**    EXPORTING
**      PI_LIFNR = GS_SCR100-LIFNR
**    IMPORTING
**      PO_LFA1  = LS_LFA1.
*
*  IF NOT gs_scr100-diff_lifnr IS INITIAL.
*    CALL FUNCTION 'WY_LFA1_SINGLE_READ'
*      EXPORTING
*        pi_lifnr = gs_scr100-diff_lifnr
*      IMPORTING
*        po_lfa1  = ls_lfa1.
*
*    ls_ap_header-su_id = ls_lfa1-stcd2. "송장발행처 사업자번호
*  ELSE.
*    CALL FUNCTION 'WY_LFA1_SINGLE_READ'
*      EXPORTING
*        pi_lifnr = gs_scr100-lifnr
*      IMPORTING
*        po_lfa1  = ls_lfa1.
*
*    ls_ap_header-su_id = gs_scr100-stceg. "PO 공급업체 사업자번호
*  ENDIF.
*
*  ls_ap_header-su_name = ls_lfa1-name1.    "사업자명
*  ls_ap_header-su_addr = ls_lfa1-ort01 && ls_lfa1-stras.  "주소
*
*  SELECT person_email1, person_email2, repres, gestyp, indtyp
*    FROM idkr_venvat
*   WHERE lifnr = @ls_lfa1-lifnr
*     AND datab <= @sy-datum
*   ORDER BY datab DESCENDING
*    INTO TABLE @DATA(lt_lfa1_etc).
*
*  SELECT SINGLE intad
*    FROM lfb1
*   WHERE lifnr = @ls_lfa1-lifnr
*     AND bukrs = @gs_scr100-bukrs
*    INTO @ls_ap_header-su_email.
*
*  READ TABLE lt_lfa1_etc INTO DATA(ls_lfa1_etc) INDEX 1.
*  IF sy-subrc EQ 0.
*    ls_ap_header-su_repres = ls_lfa1_etc-repres.  "대표자명
*
*    ls_ap_header-su_bustype = ls_lfa1_etc-indtyp.  "업태
*    ls_ap_header-su_indtype = ls_lfa1_etc-gestyp.  "업종
*  ENDIF.
*
*
**> 공급받는 자
*  SELECT SINGLE taxnumber2, businessplacename, taxinvoicerepresentativename,
*                addressname, industrytype, businesstype
*    FROM pkrbupla
*   WHERE branch = @gs_scr100-bupla
*     AND companycode = @gs_scr100-bukrs
*    INTO @DATA(ls_pkrbupla).
*
*  ls_ap_header-ip_typecode = lc_ip_typecode_01.  "사업자등록번호로 등록.
*  ls_ap_header-ip_id = ls_pkrbupla-taxnumber2.  "사업자등록번호.
*  ls_ap_header-ip_name = ls_pkrbupla-businessplacename.  "사업체명.
*  ls_ap_header-ip_repres = ls_pkrbupla-taxinvoicerepresentativename.  "대표자명.
*  ls_ap_header-ip_addr = ls_pkrbupla-addressname.  "주소.
*  ls_ap_header-ip_bustype = ls_pkrbupla-industrytype.  "업태.
*  ls_ap_header-ip_indtype = ls_pkrbupla-businesstype.  "업종
*  ls_ap_header-ip_email1 = gs_scr100-zexpen_eml.  "담당자 이메일
*
*  ls_ap_header-chargetotal = gs_scr100-dmbtr2.  "총 공급가액
*  ls_ap_header-taxtotal = gs_scr100-wmwst2.  "총 세액 합계
*  ls_ap_header-grandtotal = gs_scr100-total2.  "총액(공급가액 + 세액)
*  APPEND ls_ap_header TO lt_ap_header.
*
***********************************************************************
**> ITEM DATA 구성
***********************************************************************
*  DATA: lt_ap_item         TYPE TABLE OF zdtv3s_ap_im,
*        ls_ap_item         TYPE zdtv3s_ap_im,
*        lv_good_seq        TYPE zdtv3s_ap_im-good_seq,
*        lv_item_line_c(10) TYPE c,
*        lv_netpr           TYPE ekpo-netpr.
*
*  CLEAR: ls_ap_item, lt_ap_item.
*
*
*  DATA(lv_item_line) = lines( it_item ).
*  DATA: lv_taxamount_total LIKE gs_scr100-wmwst2.
*
*  DATA: BEGIN OF ls_max_seq,
*          good_seq       LIKE ls_ap_item-good_seq,
*          good_taxamount LIKE ls_ap_item-good_taxamount,
*        END OF ls_max_seq.
*
*  LOOP AT gt_head INTO gs_head WHERE cbox   EQ abap_true.
*
*    ADD 1 TO lv_good_seq.
*
*    ls_ap_item-good_seq = lv_good_seq.    "품목 일련번호
*    ls_ap_item-good_date = gs_head-budat. "입고일자
*
*    "물품명
*    IF lv_item_line > 99.
*      "99 라인이 초과되면 한개의 라인만 구성.
*      lv_item_line_c = lv_item_line.
*      CONDENSE lv_item_line_c.
*      CONCATENATE gs_head-matnr_tx '외' lv_item_line_c '건'
*             INTO ls_ap_item-good_name SEPARATED BY space.
*      EXIT.
*    ELSE.
*      ls_ap_item-good_name = gs_head-matnr_tx.
*    ENDIF.
*
*    ls_ap_item-good_quan = gs_head-menge. "수량
*    ls_ap_item-good_invamount = gs_head-dmbtr_krw. "공급 가액
*
*    "단가 (공급가액 / 수량 : PER 단가 적용이 있을 수 있으므로 계산)
*    CLEAR: lv_netpr.
*
*    TRY.
*        lv_netpr = gs_head-dmbtr_krw / gs_head-menge.
*      CATCH cx_sy_zerodivide.
*    ENDTRY.
*
*    PERFORM bapi_currency_conv_to_external USING    lv_netpr
*                                                      gs_scr100-waers
*                                             CHANGING ls_ap_item-good_uniamount.
*
*    "세액
*    PERFORM get_tax_amount_by_item CHANGING gs_head
*                                            ls_ap_item-good_taxamount.
*
*    lv_taxamount_total = lv_taxamount_total + ls_ap_item-good_taxamount.
*
**> 단수차이 보정을 위해 제일 많은 금액 ITEM NO 지정
*    IF ls_max_seq-good_taxamount < ls_ap_item-good_taxamount.
*      ls_max_seq-good_seq = ls_ap_item-good_seq.
*    ENDIF.
*
*    APPEND ls_ap_item TO lt_ap_item.
*  ENDLOOP.
*
**> LINE 이 99 가 넘으면 ITEM 1건만 전송
*  IF lv_item_line > 99..
*    ls_ap_item-good_invamount = ls_ap_header-chargetotal.
*    ls_ap_item-good_taxamount = ls_ap_header-taxtotal.
*    APPEND ls_ap_item TO lt_ap_item.
*  ENDIF.
*
**> 단수차이 보정
*  DATA lv_diff_taxamt TYPE rm08m-differenz.
*  SORT lt_ap_item BY good_seq.
*
*  IF lv_taxamount_total NE gs_scr100-wmwst2.
*    READ TABLE lt_ap_item INTO ls_ap_item WITH KEY good_seq = ls_max_seq-good_seq
*                                          BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      lv_diff_taxamt = gs_scr100-wmwst2 - lv_taxamount_total.
*      ls_ap_item-good_taxamount = ls_ap_item-good_taxamount + lv_diff_taxamt.
*      MODIFY lt_ap_item FROM ls_ap_item INDEX sy-tabix.
*    ENDIF.
*  ENDIF.
*
***********************************************************************
**> Extend 구성 (맵핑을 위한 데이터)
***********************************************************************
*  DATA: lt_ap_ex   TYPE TABLE OF zdtv3s_ap_ex,
*        ls_ap_ex   TYPE zdtv3s_ap_ex,
*        lt_ap_ex_d TYPE TABLE OF zdtv3s_ap_ex_d,
*        ls_ap_ex_d TYPE zdtv3s_ap_ex_d.
*
*  CLEAR: ls_ap_ex.
*  ls_ap_ex-use_doc = 'X'.
*  APPEND ls_ap_ex TO lt_ap_ex.
*
*  CLEAR ls_ap_ex_d.
*  ls_ap_ex_d-ext_seq = 1. "일련번호
*  ls_ap_ex_d-belnr = lv_belnr. "회계전표번호
*  ls_ap_ex_d-budat = gs_scr100-budat. "전표의 전기일
*  ls_ap_ex_d-bldat = gs_scr100-bldat. "전표의 증빙일
*  ls_ap_ex_d-blart = gc_re. "전표유형
*  ls_ap_ex_d-mwskz = gs_scr100-mwskz. "세금코드
*  ls_ap_ex_d-dmbtr = gs_scr100-dmbtr2. "현지통화금액
*  ls_ap_ex_d-zgjah = lv_gjahr. "회계연도
*
*  APPEND ls_ap_ex_d TO lt_ap_ex_d.
*
*
***********************************************************************
**> 역발행 생성 시작
***********************************************************************
*  CONSTANTS: lc_msgno     TYPE sy-msgno VALUE '003',
*             lc_eacc_code TYPE zdtv3s_return-code VALUE '13016'.
*
*  DATA: lt_return    TYPE TABLE OF zdtv3s_return,
*        lt_msg_popup TYPE TABLE OF bapiret2.
*
*  call function 'ZDTV3SAP_CRD_RAP_INV'
*    TABLES
*      inv_hd     = lt_ap_header
*      inv_im     = lt_ap_item
*      inv_ex     = lt_ap_ex
*      inv_ex_d   = lt_ap_ex_d
*      return     = lt_return
*    EXCEPTIONS
*      fail_issue = 1
*      OTHERS     = 2.
*
*  SORT lt_return BY type code.
*  READ TABLE lt_return INTO DATA(ls_return)
*                       WITH KEY type = 'S'
*                                code = lc_eacc_code
*                       BINARY SEARCH.
*  IF sy-subrc EQ 0.
*    MESSAGE i003 WITH '역발행 전자세금계산서가 생성되었습니다:'(m42) space ls_return-inv_seq.
*  ELSE.
*    MESSAGE s053 WITH '역발행 전자세금계산서 생성'(m43) DISPLAY LIKE 'E'.
*
*    SELECT a~message AS message_v1,
*           a~type, @gc_msgid AS id, @lc_msgno AS number
*      FROM @lt_return AS a
*      INTO CORRESPONDING FIELDS OF TABLE @lt_msg_popup.
*
*    CALL METHOD zcl_mm_common=>show_bapi_message( it_return = lt_msg_popup[] ).
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DOWNPAYMENT_LIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_downpayment_list .

  DATA: lv_fwste   TYPE bset-fwste,
        lt_tax     LIKE TABLE OF rtax1u15,
        lv_dp_exit TYPE i,
        lv_tabix   TYPE sy-tabix.

  CLEAR gt_disp_pop.

  LOOP AT gt_head INTO DATA(ls_head).
    CLEAR gs_disp_pop.
    gs_disp_pop-ebeln = ls_head-ebeln.
    gs_disp_pop-dmbtr = ls_head-dmbtr.
    COLLECT gs_disp_pop INTO gt_disp_pop.
  ENDLOOP.

  LOOP AT gt_disp_pop INTO gs_disp_pop.
    lv_tabix = sy-tabix.

    "통화
    gs_disp_pop-waers = gs_scr100-waers.

    "세액 계산.
    CLEAR: lv_fwste, lt_tax.
    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        i_bukrs = gs_scr100-bukrs
        i_mwskz = gs_scr100-mwskz
        i_waers = gs_disp_pop-waers
        i_wrbtr = gs_disp_pop-dmbtr
      IMPORTING
        e_fwste = lv_fwste
      TABLES
        t_mwdat = lt_tax
      EXCEPTIONS
        OTHERS  = 13.

    IF sy-subrc EQ 0.
      gs_disp_pop-wmwst = lv_fwste.
    ENDIF.

    "선급금 정보
    READ TABLE gt_po_etc_amt INTO DATA(ls_po_etc_amt)
                             WITH KEY ebeln = gs_disp_pop-ebeln
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      gs_disp_pop-downpay_req = ls_po_etc_amt-downpay_req.
      gs_disp_pop-downpay_amount = ls_po_etc_amt-downpay_amount + ls_po_etc_amt-downpay_tax.
      gs_disp_pop-downpay_clear = ls_po_etc_amt-downpay_clear + ls_po_etc_amt-clear_tax.
      gs_disp_pop-downpay_rem = gs_disp_pop-downpay_amount + gs_disp_pop-downpay_clear.

      ADD 1 TO lv_dp_exit.
    ENDIF.

    MODIFY gt_disp_pop FROM gs_disp_pop INDEX lv_tabix.
  ENDLOOP.

  IF lv_dp_exit EQ 0.
    MESSAGE s005 WITH '선급금 데이터'(t56).
    EXIT.
  ENDIF.

  CALL SCREEN '71' STARTING AT 05 05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_0071
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_grid_0071 .

*> 선급금 현황 조회
  DATA: ls_field TYPE lvc_s_fcat.

  DEFINE _l_set_fcat.
    CLEAR : ls_field.
    ls_field-fieldname    = &1.
    ls_field-col_pos      = &2.
    ls_field-key             = &3.
    ls_field-coltext        = &4.
    ls_field-do_sum = &5.
    ls_field-outputlen      = &6.
    ls_field-cfieldname      = &7.
    ls_field-col_opt      = 'X'.
    APPEND ls_field TO Gt_de_field.
  END-OF-DEFINITION.

  IF grf_de_con IS NOT INITIAL.
    CALL METHOD grf_de_con->free.
    CLEAR grf_de_con.
    CLEAR grf_de_grid.
    CLEAR : gt_de_field[].
  ENDIF.

  " Create a custom container control for our ALV Control
  CREATE OBJECT grf_de_con
    EXPORTING
      container_name = 'CC0071'
    EXCEPTIONS
      OTHERS         = 1.

  " Create ALV
  CREATE OBJECT grf_de_grid
    EXPORTING
      i_parent = grf_de_con.

  " Layout
  gs_de_layout-sel_mode = 'A'.
  gs_de_layout-no_toolbar = 'X'.
  gs_de_layout-no_rowmark = 'X'.

  _l_set_fcat: 'EBELN' 1 'X' '구매오더번호' '' '10' '',
               'WAERS' 2 '' '통화' '' '10' '',
               'DMBTR' 3 '' '공급가액' 'X' '20' 'WAERS',
               'WMWST' 4 '' '세액' 'X' '20' 'WAERS',
               'DOWNPAY_REQ' 5 '' '선급요청액' 'X' '20' 'WAERS',
               'DOWNPAY_AMOUNT' 6 '' '선급액' 'X' '20' 'WAERS',
               'DOWNPAY_CLEAR' 7 '' '선급반제액' 'X' '20' 'WAERS',
               'DOWNPAY_REM' 8 '' '선급잔액' 'X' '20' 'WAERS'.

  " display.
  CALL METHOD grf_de_grid->set_table_for_first_display
    EXPORTING
      is_layout       = gs_de_layout
    CHANGING
      it_outtab       = gt_disp_pop[]
      it_fieldcatalog = gt_de_field.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HELP_F4_BVTYP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM help_f4_bvtyp .


  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread.

  DATA: lv_diff_lifnr TYPE lfa1-lifnr,
        lv_empfk      TYPE lfa1-lifnr.

  CONSTANTS: lc_title(15) TYPE c VALUE '은행계좌정보',
             lc_retfield  TYPE fieldname VALUE 'BVTYP',
             lc_scr       TYPE help_info-dynprofld VALUE 'GS_SCR100-BVTYP'.

*U1> 2022.04.14 : 대체수취인 우선으로 검색
*  SELECT DISTINCT BPBANKACCOUNTINTERNALID AS BVTYP,
*         BANK, BANKA, BANKACCOUNT,
*         BANKACCOUNTHOLDERNAME
*    FROM ZCCMM_BANKACCNT
*   WHERE SUPPLIER = @GS_SCR100-LIFNR
*     AND BANKACCOUNT IN @GR_BANKN
*    INTO CORRESPONDING FIELDS OF TABLE @LT_DATA.

  PERFORM get_dynp_value USING 'GS_SCR100-DIFF_LIFNR' CHANGING lv_diff_lifnr.
  PERFORM get_dynp_value USING 'GS_SCR100-EMPFK' CHANGING lv_empfk.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_diff_lifnr
    IMPORTING
      output = lv_diff_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_empfk
    IMPORTING
      output = lv_empfk.

  IF lv_diff_lifnr NE gs_scr100-diff_lifnr.
    gs_scr100-diff_lifnr = lv_diff_lifnr.
    PERFORM set_init_empfk.
    PERFORM set_init_bank_acc.
  ELSE.
    IF lv_empfk NE gs_scr100-empfk.
      gs_scr100-empfk = lv_empfk.
      PERFORM set_init_bank_acc.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = lc_scr        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = gt_bank_acc
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    READ TABLE gt_bank_acc INTO DATA(ls_bank_acc)
                       WITH KEY bvtyp = ls_return-fieldval
                       BINARY SEARCH.

    "코드
    CLEAR ls_update.
    ls_update-fieldname = lc_scr.
    ls_update-fieldvalue  = ls_bank_acc-bvtyp.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_ZTERM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_f4_zterm.


  DATA lv_zterm LIKE t052-zterm.

  CALL FUNCTION 'FI_F4_ZTERM'
    EXPORTING
      i_koart = 'K'          "공급업체
      i_zterm = gs_scr100-zterm
      i_xshow = space
    IMPORTING
      e_zterm = lv_zterm.

  IF NOT lv_zterm IS INITIAL.
    gs_scr100-zterm = lv_zterm.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOUBLE_CLICK_START
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM double_click_start .

  DATA lv_field(20) TYPE c.

  CHECK ok_code         EQ gc_pick AND
        gs_scr100-belnr IS NOT INITIAL.

  GET CURSOR FIELD lv_field.
  CASE lv_field.
    WHEN 'GS_SCR100-BELNR'.
      SET PARAMETER ID 'RBN' FIELD gs_scr100-belnr(10).
      SET PARAMETER ID 'GJR' FIELD gs_scr100-belnr+11(*).
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT_HEAD_ETC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM build_fieldcat_head_etc USING iv_flag
                                       iv_flag2
                              CHANGING cs_lvc_fcat TYPE lvc_s_fcat.

  CASE cs_lvc_fcat-fieldname.
    WHEN 'SAKTO'.
      cs_lvc_fcat-col_pos    = 52.
      cs_lvc_fcat-coltext    = TEXT-f39.  "GL 계정
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'SAKTO_TX'.
      cs_lvc_fcat-col_pos    = 53.
      cs_lvc_fcat-coltext    = TEXT-f40.  "GL 계정명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'PRCTR'.
      cs_lvc_fcat-col_pos    = 54.
      cs_lvc_fcat-coltext    = TEXT-f41.  "손익센터
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'PRCTR_TX'.
      cs_lvc_fcat-col_pos    = 55.
      cs_lvc_fcat-coltext    = TEXT-f42.  "손익센터명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'KOSTL'.
      cs_lvc_fcat-col_pos    = 56.
      cs_lvc_fcat-coltext    = TEXT-f43.  "코스트센터
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'KOSTL_TX'.
      cs_lvc_fcat-col_pos    = 57.
      cs_lvc_fcat-coltext    = TEXT-f44.  "코스트센터명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'FISTL'.
      cs_lvc_fcat-col_pos    = 58.
      cs_lvc_fcat-coltext    = TEXT-f45.  "Fund Center
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'FISTL_TX'.
      cs_lvc_fcat-col_pos    = 59.
      cs_lvc_fcat-coltext    = TEXT-f46.  "Fund Center명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'VBELN'.
      cs_lvc_fcat-col_pos    = 61.
      cs_lvc_fcat-coltext    = TEXT-f47.  "판매오더
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'VBELP'.
      cs_lvc_fcat-col_pos    = 62.
      cs_lvc_fcat-coltext    = TEXT-f48.  "판매오더품번
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 12.

    WHEN 'ANLN1'.
      cs_lvc_fcat-col_pos    = 63.
      cs_lvc_fcat-coltext    = TEXT-f49.  "자산번호
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'ANLN1_TX'.
      cs_lvc_fcat-col_pos    = 64.
      cs_lvc_fcat-coltext    = TEXT-f50.  "자산명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'PSPNR'.
      cs_lvc_fcat-col_pos    = 65.
      cs_lvc_fcat-coltext    = TEXT-f51.  "WBS 요소
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'PSPNR_TX'.
      cs_lvc_fcat-col_pos    = 66.
      cs_lvc_fcat-coltext    = TEXT-f52.  "WBS 요소명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'AUFNR'.
      cs_lvc_fcat-col_pos    = 67.
      cs_lvc_fcat-coltext    = TEXT-f53.  "Internal Order
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 12.

    WHEN 'AUFNR_TX'.
      cs_lvc_fcat-col_pos    = 68.
      cs_lvc_fcat-coltext    = TEXT-f54.  "Internal Order명
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 20.

    WHEN 'PEINH'.
      cs_lvc_fcat-col_pos    = 71.
      cs_lvc_fcat-coltext    = TEXT-f72.  "가격단위
      cs_lvc_fcat-outputlen  = 8.

    WHEN 'BPRME'.
      cs_lvc_fcat-col_pos    = 72.
      cs_lvc_fcat-coltext    = TEXT-f73.  "오더가격단위
      cs_lvc_fcat-outputlen  = 8.

    WHEN 'NETPR'.
      cs_lvc_fcat-col_pos    = 81.
      cs_lvc_fcat-coltext    = TEXT-f61.  "오더단가
      cs_lvc_fcat-outputlen  = 15.

    WHEN 'BISMT'.
      cs_lvc_fcat-col_pos    = 62.
      cs_lvc_fcat-coltext    = TEXT-f62.  "기존자재번호
      cs_lvc_fcat-no_out     = abap_true.
      cs_lvc_fcat-outputlen  = 35.

    WHEN 'PO_TEXT'.
      cs_lvc_fcat-col_pos    = 83.
      cs_lvc_fcat-coltext    = TEXT-f74.  "자재 PO 텍스트
      cs_lvc_fcat-no_out     = iv_flag.
      cs_lvc_fcat-outputlen  = 20.


    WHEN 'ZORDER_PERSON_NAME'.
      cs_lvc_fcat-col_pos    = 84.
      cs_lvc_fcat-coltext    = TEXT-f66.  "발주자
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'ZORDER_DEPART_NAME'.
      cs_lvc_fcat-col_pos    = 85.
      cs_lvc_fcat-coltext    = TEXT-f68.  "발주부서
      cs_lvc_fcat-outputlen  = 15.

    WHEN 'BSART'.
      cs_lvc_fcat-col_pos    = 87.
      cs_lvc_fcat-coltext    = TEXT-f69.  "구매문서유형
      cs_lvc_fcat-outputlen  = 6.
      cs_lvc_fcat-no_out  = 'X'.

    WHEN 'BELNR2'.
      cs_lvc_fcat-col_pos    = 91.
      cs_lvc_fcat-coltext    = TEXT-f79.  "송장전표번호
      cs_lvc_fcat-outputlen  = 10.
      cs_lvc_fcat-hotspot  = abap_true.

    WHEN 'GJAHR2'.
      cs_lvc_fcat-col_pos    = 92.
      cs_lvc_fcat-coltext    = TEXT-f80.  "송장전표년도
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'INV_SEQ'.
      cs_lvc_fcat-col_pos    = 93.
      cs_lvc_fcat-coltext    = TEXT-f82.  "전자세금계산서 번호
      cs_lvc_fcat-outputlen  = 10.

    WHEN 'MESG'.
      cs_lvc_fcat-col_pos    = 93.
      cs_lvc_fcat-coltext    = TEXT-f57.  "메시지
      cs_lvc_fcat-tech       = iv_flag2.
      cs_lvc_fcat-outputlen  = 10.

    WHEN OTHERS.
      cs_lvc_fcat-tech     = abap_true.


  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TAX_AMOUNT_BY_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_HEAD
*&---------------------------------------------------------------------*
FORM get_tax_amount_by_item CHANGING cs_head STRUCTURE gs_head
                                      cv_tax_amount.

  DATA: lv_dmbtr TYPE bseg-wrbtr,
        lv_fwste TYPE bset-fwste,
        lt_tax   TYPE TABLE OF rtax1u15.

  lv_dmbtr = cs_head-dmbtr.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = gs_scr100-bukrs
      i_mwskz = gs_scr100-mwskz
      i_waers = gs_scr100-waers
      i_wrbtr = lv_dmbtr
    IMPORTING
      e_fwste = lv_fwste
    TABLES
      t_mwdat = lt_tax
    EXCEPTIONS
      OTHERS  = 13.

  IF sy-subrc EQ 0.
    cv_tax_amount  = lv_fwste.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PRINT_CHANGING_FEE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_print_changing_fee .

  "물품 일반과 자율납품만 인쇄교체비 출력
  CHECK p_rp1 = 'X' OR p_rp3 = 'X'.
  CHECK p_bukrs EQ gc_bukrs_1101.

  CONSTANTS: lc_zpri TYPE kschl VALUE 'ZPRI'.

  CLEAR: gs_scr100-prtch.
  DATA(lt_tmp) = gt_head[].
  SORT lt_tmp BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING ebeln ebelp.
  IF NOT lt_tmp[] IS INITIAL.
    IF p_rp1 = 'X'.
      SELECT ebeln, ebelp, zpri
        FROM zsvcmm_price
         FOR ALL ENTRIES IN @lt_tmp
       WHERE ebeln = @lt_tmp-ebeln
         AND ebelp = @lt_tmp-ebelp
        INTO TABLE @DATA(lt_po_price).
    ELSE.
      SELECT schedulingagreement AS ebeln,
             schedulingagreementitem AS ebelp,
             conditionratevalue AS zpri
        FROM ischagrmtitmcnd
         FOR ALL ENTRIES IN @lt_tmp
       WHERE schedulingagreement = @lt_tmp-ebeln
         AND schedulingagreementitem = @lt_tmp-ebelp
         AND conditiontype = @lc_zpri
        INTO CORRESPONDING FIELDS OF TABLE @lt_po_price.
    ENDIF.
    FREE lt_tmp.
  ENDIF.

  LOOP AT lt_po_price INTO DATA(ls_po_price).
    gs_scr100-prtch = gs_scr100-prtch + ls_po_price-zpri.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONDITION_BY_IMG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_condition_by_img CHANGING cv_chk.

  CONSTANTS: lc_msgno_000 TYPE sy-msgno VALUE '000'.
  CONSTANTS: lc_chk(4)    VALUE '1101'.


  DATA: lv_rate     TYPE /ile/tmperc,
        lv_rate_amt TYPE rm08m-differenz,
        lv_max_amt  TYPE rm08m-differenz,
        lv_min_amt  TYPE rm08m-differenz,
        lv_amt_char TYPE char50,
        lv_msg      TYPE bapi_msg,
        lv_base_amt LIKE gs_scr100-total,
        lv_gap_amt  LIKE gs_scr100-total2.

*  CHECK SY-SUBRC EQ 0.
  CHECK NOT gs_config_e1013-field1 IS INITIAL.

*> 은행 계좌 필수 여부
  IF gs_config_e1013-field4 = 'Y' AND gs_scr100-bvtyp IS INITIAL.
    MESSAGE s017 WITH '공급업체은행계좌'(t40) DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

  CHECK cv_chk IS INITIAL.

*> 역발행인 경우 입고금액과 다르면 메시지 처리함.
  IF p_r2 = 'X' AND
     NOT gs_config_e1013-field7 IS INITIAL AND
     gs_scr100-dmbtr NE gs_scr100-dmbtr2.
    IF gs_config_e1013-field7 = 'E'.
      DATA(lv_msgty) = 'S'.
    ELSE.
      lv_msgty = gs_config_e1013-field7.
    ENDIF.

    MESSAGE ID gc_msgid
            TYPE lv_msgty
            NUMBER lc_msgno_000
            WITH '헤더와 품목금액 차이로 역발행 계산서 발행 오류가 예상됩니다.'(m60)
            DISPLAY LIKE gs_config_e1013-field7.


    IF gs_config_e1013-field7 = 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_scr100-total < 0.
    lv_base_amt = gs_scr100-total * -1.
  ELSE.
    lv_base_amt = gs_scr100-total.
  ENDIF.

  IF gs_scr100-total2 < 0.
    lv_gap_amt = gs_scr100-total2 * -1.
  ELSE.
    lv_gap_amt = gs_scr100-total2.
  ENDIF.

*> 허용률 금액
  IF NOT gs_config_e1013-field2 IS INITIAL.

    "입고기준 총액(공급가액 + 부가세) 허용률 금액
    lv_rate = gs_config_e1013-field2.

    TRY.
        lv_rate_amt = lv_base_amt * ( lv_rate / 100 ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    lv_max_amt = lv_base_amt + lv_rate_amt.
    lv_min_amt = lv_base_amt - lv_rate_amt.

    "입고기준 허용률 금액이 총액(KRW) 보다 크면 ERROR
    IF lv_gap_amt > lv_max_amt.
      WRITE: lv_max_amt TO lv_amt_char CURRENCY gs_scr100-waers.
      CONDENSE lv_amt_char.
      CONCATENATE lv_amt_char '이하 입력해주세요.' INTO lv_msg SEPARATED BY space.
      MESSAGE s098 WITH '최대 가용율 총액'(m45) lv_msg DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.

    "입고기준 허용률 금액이 총액(KRW) 보다 작으면 ERROR
    IF lv_gap_amt < lv_min_amt.
      WRITE: lv_min_amt TO lv_amt_char CURRENCY gs_scr100-waers.
      CONDENSE lv_amt_char.
      CONCATENATE lv_amt_char '이상 입력해주세요.' INTO lv_msg SEPARATED BY space.
      MESSAGE s098 WITH '최소 가용율 총액'(m46) lv_msg.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*> 허용금액
  IF NOT gs_config_e1013-field3 IS INITIAL.

    "입고기준 총액(공급가액 + 부가세) 허용률 금액
    CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
      EXPORTING
        currency    = gs_scr100-waers
        idoc_amount = gs_config_e1013-field3
      IMPORTING
        sap_amount  = lv_rate_amt.

    lv_max_amt = lv_base_amt + lv_rate_amt.
    lv_min_amt = lv_base_amt - lv_rate_amt.

    "입고기준 허용률 금액이 총액(KRW) 보다 크면 ERROR
    IF lv_gap_amt > lv_max_amt.
      WRITE: lv_max_amt TO lv_amt_char CURRENCY gs_scr100-waers.
      CONDENSE lv_amt_char.
      CONCATENATE lv_amt_char '이하 입력해주세요.' INTO lv_msg SEPARATED BY space.
      MESSAGE s098 WITH '최대 가용 총액'(m47) lv_msg DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.

    "입고기준 허용률 금액이 총액(KRW) 보다 작으면 ERROR
    IF lv_gap_amt < lv_min_amt.
      WRITE: lv_min_amt TO lv_amt_char CURRENCY gs_scr100-waers.
      CONDENSE lv_amt_char.
      CONCATENATE lv_amt_char '이상 입력해주세요.' INTO lv_msg SEPARATED BY space.
      MESSAGE s098 WITH '최소 가용 총액'(m48) lv_msg.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*U4> 입고전기월과 송장전기월이 다를때 메시지 처리.
*220530 KIG 입고전기월과 송장전기월이 다를때 예외처리 통제 목적
*사별분리 KTG, MM CONFIG에 BSART 유형이 없으면 입고전기월과 송장전기월이 달라도
*처리 가능
  zcl_mm_common=>common_config(
         EXPORTING  is_common = VALUE #( m = 'C1' d = 'PUORD' s = '1050' )
         IMPORTING et_outtab  = DATA(lt_config_bsart) ).

  IF lt_config_bsart[] IS NOT INITIAL AND
     gs_scr100-bukrs = gc_bukrs_1101.
    SORT lt_config_bsart BY field1.

    LOOP AT gt_head INTO DATA(ls_head) WHERE cbox EQ abap_true.
      READ TABLE lt_config_bsart INTO DATA(ls_config_bsart)
                                 WITH KEY field1 = ls_head-bsart.
      CHECK sy-subrc <> 0.
      gs_config_e1013-field9 = lc_chk.
    ENDLOOP.
  ENDIF.
*220530 KIG 입고전기월과 송장전기월이 다를때 예외처리 통제 목적

  IF NOT gv_diff_gr_month IS INITIAL AND
    NOT gs_config_e1013-field9 IS INITIAL.

    IF gs_config_e1013-field9 = 'E'.
      DATA(lv_msgty_9) = 'S'.
    ELSE.
      lv_msgty_9 = gs_config_e1013-field9.
    ENDIF.

    CHECK gs_config_e1013-field9 <> lc_chk.

    MESSAGE ID gc_msgid
            TYPE lv_msgty_9
            NUMBER lc_msgno_000
            WITH '송장 전기월과 입고 전기월이 다른 입고문서가 존재합니다.'(m61)
            DISPLAY LIKE gs_config_e1013-field9.

    IF gs_config_e1013-field9 = 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONDITION_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_condition_header CHANGING cv_chk.

  DATA: lv_amount(50) TYPE c.

  "작성일자를 입력하세요.
  IF gs_scr100-bldat IS INITIAL.
    MESSAGE s017 WITH TEXT-m13 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

  "전기일을 입력하세요.
  IF gs_scr100-budat IS INITIAL.
    MESSAGE s017 WITH TEXT-m14 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

  "기산일을 입력하세요.
  IF gs_scr100-zfbdt IS INITIAL.
    MESSAGE s017 WITH TEXT-m33 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

  "지급조건을 입력하세요.
  IF gs_scr100-zterm IS INITIAL.
    MESSAGE s017 WITH TEXT-m35 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

  "[ value ] 사업장 를 확인하세요!
  IF gs_scr100-bupla IS NOT INITIAL.
    READ TABLE gt_bupla INTO gs_bupla WITH KEY bupla = gs_scr100-bupla
                                      BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE s040 WITH gs_scr100-bupla TEXT-m36 DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE s017 WITH TEXT-m36 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.


  "세금코드를 입력하세요.
  IF gs_scr100-mwskz IS INITIAL.
    MESSAGE s017 WITH TEXT-m22 DISPLAY LIKE 'E'.
    cv_chk = abap_true.
    EXIT.
  ENDIF.

*> 지체상금인 경우 금액 초과로직 추가.
*  IF P_RP4 = 'X' AND  GS_SCR100-DELAYF NE 0.
*    IF GS_SCR100-DELAYF > GS_SCR100-DELAYF_M.
*      WRITE: GS_SCR100-DELAYF_M TO LV_AMOUNT CURRENCY GS_SCR100-WAERS.
*      CONDENSE LV_AMOUNT.
*      MESSAGE S080 WITH '지체상금액'(T50) LV_AMOUNT DISPLAY LIKE 'E'.
*      CV_CHK = ABAP_TRUE.
*      EXIT.
*    ENDIF.
*  ENDIF.

  "지체상금률 미존재 시 지체상금액 입력 불가
  IF p_rp4 = 'X'.
    IF gs_scr100-delayf_x IS INITIAL.
      IF gs_scr100-delayf NE 0.
        MESSAGE s005 WITH '구매오더에 지체상금률'(m58) DISPLAY LIKE 'E'.
        cv_chk = abap_true.
        EXIT.
      ENDIF.
    ELSE. "지체상금률 존재 시 공급가액보다는 적어야 함.
      IF gs_scr100-delayf > gs_scr100-dmbtr2..
        MESSAGE s081 WITH '지체상금액이 공급가액'(m59) DISPLAY LIKE 'E'.
        cv_chk = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

*> 선급금인 경우 금액 초과로직 추가.
  IF gs_scr100-dwpayc NE 0.
    IF gs_scr100-dwpayc > gs_scr100-dwpayc_m.
      WRITE: gs_scr100-dwpayc_m TO lv_amount CURRENCY gs_scr100-waers.
      CONDENSE lv_amount.
      MESSAGE s080 WITH '선급금액'(t49) lv_amount DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.

    IF gs_scr100-dwpayc > gs_scr100-total2.
      MESSAGE s081 WITH '공급가'(t45) lv_amount DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  DATA: lv_split_line_c(10) TYPE c,
        lv_sel_line_c(10)   TYPE c.

  IF gs_post_split-sel_line NE 0.
*> 지체상금 또는 선급금 존재 시 라인 분할기능 사용 불가.
    IF gs_scr100-delayf NE 0 OR gs_scr100-dwpayc NE 0.
      lv_split_line_c = gs_post_split-base_line.
      lv_sel_line_c = gs_post_split-sel_line.
      CONDENSE: lv_split_line_c, lv_sel_line_c.
      DATA(lv_split_msg) = '최대 허용 라인:'(m50) && lv_split_line_c &&
                           '/ 현재 선택 라인:'(m51) && lv_sel_line_c.
      MESSAGE s098 WITH '선택 라인 수'(m49) lv_split_msg DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.

*> 차이금액 발생 시 처리 불가(어떤 자재에 배부해야 하는지 모호함..)
    IF gs_scr100-differenz NE 0.
      MESSAGE s092 WITH '차이금액 발생'(m52) '분할송장 처리'(m53) DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_IV_NO_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_iv_no_split .

  DATA ls_header    LIKE zsmm_incinv_create_header.
  DATA lt_item      LIKE TABLE OF zsmm_incinv_create_item.
  DATA ls_item      LIKE zsmm_incinv_create_item.
  DATA lt_tax       LIKE TABLE OF zsmm_incinv_create_tax.
  DATA ls_tax       LIKE zsmm_incinv_create_tax.
  DATA lt_gl_acc   LIKE TABLE OF zsmm_incinv_create_gl_account.
  DATA lt_matdata   LIKE TABLE OF zsmm_incinv_create_material.
  DATA lt_ap_split   LIKE TABLE OF zsmm_incinv_create_vendorsplit.

  DATA lt_return    LIKE TABLE OF bapiret2.
  DATA ls_return    LIKE bapiret2.
  DATA ls_result    LIKE zsmm_incinv_create_result.

  DATA lv_itemno    TYPE rblgp.
  DATA lv_tabix     TYPE sytabix.
*  DATA LV_CHK(1)    TYPE C.
  DATA lv_dmbtr     LIKE ekbe-wrbtr.

*  DATA LV_TEXT  TYPE STRING.

  DATA lv_map_sucss TYPE c.

*(+)U6 START
  DATA : lt_accountingdata LIKE TABLE OF zsmm_incinv_create_account.

*(+)U6 END
  CLEAR : gt_log.
  CLEAR   : gs_scr100-belnr,
            gv_incl_taxim_y. "U5

  "화면상단 전표번호 신호등 초기화
  PERFORM set_signal_status USING    space
                             CHANGING gs_scr100-status.

*-헤더
  ls_header-invoice_type = 'IV'.

  IF gs_scr100-dmbtr2 < 0.                   "Credit Memo
    ls_header-invoice_type = 'CM'.
  ENDIF.

  ls_header-doc_type        = gc_re.
  ls_header-doc_date        = gs_scr100-bldat. "증빙일
  ls_header-pstng_date      = gs_scr100-budat. "전기일
  ls_header-comp_code       = gs_scr100-bukrs. "회사코드
  ls_header-currency        = gs_scr100-waers. "통화

  IF gs_scr100-zlsch EQ 'K'. "국책과제
    ls_header-housebankid = gs_scr100-hbkid. "거래은행
    ls_header-paymt_ref   = gs_scr100-hktid. "계정 ID
  ENDIF.

  "공급가액 + 세액
  PERFORM bapi_currency_conv_to_external USING    gs_scr100-total2
                                                   gs_scr100-waers
                                          CHANGING ls_header-gross_amount.
  ls_header-gross_amount    = abs( ls_header-gross_amount ).

  ls_header-bline_date      = gs_scr100-zfbdt. "기산일
  ls_header-pmnttrms        = gs_scr100-zterm. "지급조건
  ls_header-business_place  = gs_scr100-bupla. "사업장

  "지급방법: 표준에서 자동 설정 예정이므로 전송하지 않음.
*  LS_HEADER-PYMT_METH       = GS_SCR100-ZLSCH. "지급방법

  ls_header-partner_bk      = gs_scr100-bvtyp. "파트너은행유형

  "비계획 운송비용
  IF gs_scr100-differenz IS NOT INITIAL.
    PERFORM bapi_currency_conv_to_external USING    gs_scr100-differenz
                                                    gs_scr100-waers
                                           CHANGING ls_header-del_costs.
  ENDIF.

  "환율
  IF p_r12 EQ abap_true.
    ls_header-exch_rate = gs_scr100-kursf.
  ENDIF.

  ls_header-vatdate = ls_header-doc_date. "부가세일자를 증빙일로 설정.

*U1> 2022.04.14 : 대체수취인 기능 추가
  IF gs_scr100-empfk IS NOT INITIAL.
    ls_header-payee_payer = gs_scr100-empfk. "대체수취인
  ENDIF.

*U1> 2022.04.14 : 송장발행처 기능 추가
  IF gs_scr100-diff_lifnr IS NOT INITIAL.
    ls_header-diff_inv = gs_scr100-diff_lifnr. "송장발행처
  ENDIF.

*-세액
  CLEAR: lt_tax, ls_tax.
  ls_tax-tax_code = gs_scr100-mwskz.
  PERFORM bapi_currency_conv_to_external USING    gs_scr100-wmwst2
                                                  gs_scr100-waers
                                         CHANGING ls_tax-tax_amount.
  ls_tax-tax_amount = abs( ls_tax-tax_amount ).
  APPEND ls_tax TO lt_tax.

*-품목
* (+)u6 START
  PERFORM get_po_data.
* (+)U6 END
  LOOP AT gt_head INTO gs_head WHERE cbox   EQ abap_true
                     AND belnr2 EQ space.

    CLEAR: ls_result.

    lv_tabix = sy-tabix.

    ADD 1 TO lv_itemno.

    "사업장
    IF ls_header-business_place IS INITIAL AND gs_scr100-bupla IS INITIAL..
      PERFORM business_place USING    gs_head-werks
                             CHANGING ls_header-business_place.
    ENDIF.

    IF gs_head-taxim EQ 'Y'.  "의제매입세 처리
      PERFORM gl_account_data TABLES lt_gl_acc
                               USING  gs_head
                                      lv_itemno.

      PERFORM material_data TABLES lt_matdata
                               USING  gs_head
                                      lv_itemno.

      gv_incl_taxim_y = 'Y'.  "U5 (의제매입 포함여부)
    ENDIF.

    ls_item-invoice_doc_item = lv_itemno.
    ls_item-po_number        = gs_head-ebeln.             "구매오더번호
    ls_item-po_item          = gs_head-ebelp.             "구매오더품번
    ls_item-tax_code         = gs_scr100-mwskz.           "세금코드

    "금액
    PERFORM adjust_amount USING    gs_head
                           CHANGING lv_dmbtr.

    PERFORM bapi_currency_conv_to_external USING    lv_dmbtr
                                                    gs_scr100-waers
                                           CHANGING ls_item-item_amount.


    ls_item-item_amount = abs( ls_item-item_amount ).

    ls_item-ref_doc        = gs_head-lfbnr.             "참조전표번호
    ls_item-ref_doc_year   = gs_head-lfgja.             "참조회계년도
    ls_item-ref_doc_it     = gs_head-lfpos.             "참조전표품번
    ls_item-quantity       = abs( gs_head-menge ).      "수량
    ls_item-po_unit        = gs_head-meins.             "단위
    ls_item-po_pr_uom      = gs_head-bprme.

    APPEND ls_item TO lt_item.
    CLEAR  ls_item.

*   (+)U6 START
    PERFORM set_accountingdata TABLES lt_accountingdata   USING lv_itemno.

* (+)U6 END
  ENDLOOP.

  IF lt_item[] IS INITIAL.
    "송장 처리할 데이터가 존재하지 않습니다.
    MESSAGE s022 WITH TEXT-m34.    EXIT.
  ENDIF.

  IF ls_header-header_txt IS INITIAL.
    ls_header-header_txt = gs_scr100-bktxt. "비고
  ENDIF.

  "FI 요청: 사번 설정
*  SELECT SINGLE employ_no
*    FROM zsvmm_user_info
*   WHERE company = @ls_header-comp_code
*     AND user_id = @sy-uname
*    INTO @ls_header-ref_doc_no.

  "선급금 또는 지체상금 입력되어 있으면 AP SPLIT
  IF gs_scr100-dwpayc NE 0 OR gs_scr100-delayf NE 0.
    PERFORM ap_split_data TABLES lt_ap_split.
  ENDIF.

  call function 'ZFMM_INCOMINGINVOICE_CREATE'
    EXPORTING
      is_headerdata          = ls_header
    IMPORTING
      es_invoicedocnumber    = ls_result
    TABLES
      it_taxdata             = lt_tax
      it_vendoritemsplitdata = lt_ap_split
      it_itemdata            = lt_item
      it_glaccountdata       = lt_gl_acc
      it_materialdata        = lt_matdata
      it_accountingdata      = lt_accountingdata    "(+)U6
      et_return              = lt_return.


  CLEAR: ls_return.
  IF sy-subrc EQ 0 AND
     ls_result-inv_doc_no IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    "I/V 번호 : XXX 을 생성하였습니다.
    CONCATENATE TEXT-m16 ls_result-inv_doc_no TEXT-m17
           INTO ls_return-message SEPARATED BY space.

    APPEND ls_return TO lt_return.
    MESSAGE s000 WITH ls_return-message.
    PERFORM set_signal_status USING    'S'
                              CHANGING gs_scr100-status.

    CONCATENATE ls_result-inv_doc_no ls_result-fisc_year
           INTO gs_scr100-belnr SEPARATED BY space.

    gs_head-belnr2 = ls_result-inv_doc_no.
    gs_head-gjahr2 = ls_result-fisc_year.

*    MODIFY GT_HEAD FROM GS_HEAD TRANSPORTING BELNR2 GJAHR2
*                          WHERE CBOX   EQ ABAP_TRUE
*                            AND BELNR2 EQ SPACE.

    SORT gt_head_all BY gjahr belnr buzei ebeln ebelp lfgja lfbnr lfpos.

    LOOP AT gt_head ASSIGNING FIELD-SYMBOL(<ls_head>)
                    WHERE cbox EQ abap_true
                      AND belnr2 EQ space.
      <ls_head>-belnr2 = ls_result-inv_doc_no.
      <ls_head>-gjahr2 = ls_result-fisc_year.

      READ TABLE gt_head_all ASSIGNING FIELD-SYMBOL(<ls_head_all>)
                             WITH KEY gjahr = <ls_head>-gjahr
                                      belnr = <ls_head>-belnr
                                      buzei = <ls_head>-buzei
                                      ebeln = <ls_head>-ebeln
                                      ebelp = <ls_head>-ebelp
                                      lfgja = <ls_head>-lfgja
                                      lfbnr = <ls_head>-lfbnr
                                      lfpos = <ls_head>-lfpos
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_head_all>-belnr2 = ls_result-inv_doc_no.
        <ls_head_all>-gjahr2 = ls_result-fisc_year.
      ENDIF.
    ENDLOOP.

    PERFORM refresh_alv USING gv_grid_head.

    "종이세금계산서가 아니고 원화이면 전자세금계산서 연계
    READ TABLE gt_paper_tax WITH KEY mwskz = gs_scr100-mwskz
                            BINARY SEARCH
                            TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0 AND p_r11 = 'X'.
*> 정발행
      IF p_r1 EQ 'X'.
        "전자세금계산서와 맵핑, 단 조건에 맞지 않으면 화면 이동하여 맵핑
        CLEAR lv_map_sucss.
        PERFORM direct_mapping_invoice CHANGING lv_map_sucss.
        "맵핑안되면 화면이동하여 직접 맵핑.
        IF lv_map_sucss NE 'S'.
          PERFORM mapping_invoice.
        ENDIF.

*> 역발행
      ELSE.
        PERFORM create_req_re_ap TABLES lt_item.
      ENDIF.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    "송장 처리에 실패하였습니다. 메시지를 확인하세요.
    MESSAGE s055 DISPLAY LIKE 'E' WITH TEXT-m03.
    PERFORM set_signal_status USING    'E'
                              CHANGING gs_scr100-status.
  ENDIF.

  gt_log[] = lt_return[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_IV_SPLIT_BY_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_iv_split_by_line .

  DATA ls_header    LIKE zsmm_incinv_create_header.

  DATA: lt_item_bapi LIKE TABLE OF zsmm_incinv_create_item,
        ls_item_bapi LIKE zsmm_incinv_create_item.

  DATA: BEGIN OF ls_item.
          include structure zsmm_incinv_create_item.
  DATA:   split_key TYPE char10,
          matnr_tx  TYPE makt-maktx,
        END OF ls_item,
        lt_item_all LIKE TABLE OF ls_item.

  DATA lt_tax       LIKE TABLE OF zsmm_incinv_create_tax.
  DATA ls_tax       LIKE zsmm_incinv_create_tax.

  DATA lt_return    LIKE TABLE OF bapiret2.
  DATA ls_return    LIKE bapiret2.
  DATA ls_result    LIKE zsmm_incinv_create_result.

*(+)U6 START
  DATA : lt_accountingdata LIKE TABLE OF zsmm_incinv_create_account,
         ls_accountingdata LIKE  zsmm_incinv_create_account.
  DATA : BEGIN OF ls_account_sum ,
            ebeln LIKE ekbe_ma-ebeln,
            ebelp LIKE ekbe_ma-ebelp,
            zekkn LIKE ekbe_ma-zekkn,
            shkzg LIKE ekbe_ma-shkzg,
            menge LIKE ekbe_ma-menge,
            wrbtr LIKE ekbe_ma-wrbtr,
         END OF  ls_account_sum.
  DATA : lt_account_sum LIKE TABLE OF ls_account_sum.
*(+)U6 END

  DATA lv_itemno    TYPE rblgp.
  DATA lv_tabix     TYPE sytabix.
  DATA lv_dmbtr     LIKE ekbe-wrbtr.

  DATA lv_map_sucss TYPE c.

  DATA: lv_split_key       TYPE char10,
        lv_split_max       TYPE i,
        lv_split_dmbtr_sum TYPE rm08m-differenz,
        lv_split_wmwst_sum TYPE rm08m-differenz,
        lv_split_dmbtr_rem TYPE rm08m-differenz,
        lv_split_wmwst_rem TYPE rm08m-differenz.

  DATA: lv_total TYPE i,
        lv_succe TYPE i,
        lv_error TYPE i.

  CLEAR : gt_log.
  CLEAR   : gs_scr100-belnr, gt_bapi_split, gs_bapi_split.

  "화면상단 전표번호 신호등 초기화
  PERFORM set_signal_status USING    space
                             CHANGING gs_scr100-status.

**********************************************************************
*-헤더 기본
**********************************************************************
  PERFORM set_bapi_header_by_split CHANGING ls_header.

**********************************************************************
*-품목
**********************************************************************
  lv_split_key = '1'.
* (+)u6 START
  PERFORM get_po_data.
* (+)u6 END
  LOOP AT gt_head INTO gs_head WHERE cbox   EQ abap_true
                     AND belnr2 EQ space.

    lv_tabix = sy-tabix.

    "사업장
    IF ls_header-business_place IS INITIAL AND gs_scr100-bupla IS INITIAL.
      PERFORM business_place USING    gs_head-werks
                             CHANGING ls_header-business_place.
    ENDIF.

*    LS_ITEM-INVOICE_DOC_ITEM = LV_ITEMNO.
    ls_item-po_number        = gs_head-ebeln.             "구매오더번호
    ls_item-po_item          = gs_head-ebelp.             "구매오더품번
    ls_item-tax_code         = gs_scr100-mwskz.           "세금코드

    "금액
    PERFORM adjust_amount USING    gs_head
                           CHANGING lv_dmbtr.
    PERFORM bapi_currency_conv_to_external USING    lv_dmbtr
                                                    gs_scr100-waers
                                           CHANGING ls_item-item_amount.

    ls_item-item_amount = abs( ls_item-item_amount ).

    ls_item-ref_doc        = gs_head-lfbnr.             "참조전표번호
    ls_item-ref_doc_year   = gs_head-lfgja.             "참조회계년도
    ls_item-ref_doc_it     = gs_head-lfpos.             "참조전표품번
    ls_item-quantity       = abs( gs_head-menge ).      "수량
    ls_item-po_unit        = gs_head-meins.             "단위
    ls_item-po_pr_uom      = gs_head-bprme.

    ls_item-split_key      = lv_split_key.
    ls_item-matnr_tx      = gs_head-matnr_tx.
    APPEND ls_item TO lt_item_all.

    gs_head-split_key = lv_split_key.
    MODIFY gt_head FROM gs_head INDEX lv_tabix.

    gs_bapi_split-dmbtr = lv_dmbtr.
    gs_bapi_split-split_key = lv_split_key.
    COLLECT gs_bapi_split INTO gt_bapi_split.

    ADD 1 TO lv_split_max.

    "최대라인 차면.. 새로운 SPLIT KEY 지정.
    IF lv_split_max = gs_post_split-base_line.
      ADD 1 TO lv_split_key.
      CONDENSE lv_split_key.
      CLEAR: lv_split_max, gs_bapi_split.
    ENDIF.

    CLEAR: ls_item, gs_bapi_split.
  ENDLOOP.

  IF lt_item_all[] IS INITIAL.
    "송장 처리할 데이터가 존재하지 않습니다.
    MESSAGE s022 WITH TEXT-m34.
    EXIT.
  ENDIF.

***********************************************************************
*> 단수 차이 검증.. (차이 발생 시 첫번째 송장에 취합)
***********************************************************************
  LOOP AT gt_bapi_split ASSIGNING FIELD-SYMBOL(<ls_bapi_split>).
    "세액 계산
    PERFORM calculate_tax_for_split USING <ls_bapi_split>-dmbtr
                                    CHANGING <ls_bapi_split>-wmwst.
    lv_split_dmbtr_sum = lv_split_dmbtr_sum + <ls_bapi_split>-dmbtr.
    lv_split_wmwst_sum = lv_split_wmwst_sum + <ls_bapi_split>-wmwst.
    <ls_bapi_split>-total = <ls_bapi_split>-dmbtr + <ls_bapi_split>-wmwst.
  ENDLOOP.

  IF lv_split_dmbtr_sum NE gs_scr100-dmbtr2.
    lv_split_dmbtr_rem = gs_scr100-dmbtr2 - lv_split_dmbtr_sum.
  ENDIF.

  IF lv_split_wmwst_sum NE gs_scr100-wmwst2.
    lv_split_wmwst_rem = gs_scr100-wmwst2 - lv_split_wmwst_sum.
  ENDIF.

  READ TABLE gt_bapi_split ASSIGNING <ls_bapi_split> INDEX 1.
  IF sy-subrc EQ 0.
    <ls_bapi_split>-dmbtr = <ls_bapi_split>-dmbtr + lv_split_dmbtr_rem.
    <ls_bapi_split>-wmwst = <ls_bapi_split>-wmwst + lv_split_wmwst_rem.
    <ls_bapi_split>-total = <ls_bapi_split>-dmbtr + <ls_bapi_split>-wmwst.
  ENDIF.


***********************************************************************
**-BAPI 수행
***********************************************************************
  SORT lt_item_all BY split_key.

  LOOP AT gt_bapi_split ASSIGNING <ls_bapi_split>.
    ADD 1 TO lv_total.
    CLEAR : lt_accountingdata.  "(+)U6
    "공급가액 + 세액
    CLEAR ls_header-gross_amount.
    PERFORM bapi_currency_conv_to_external USING    <ls_bapi_split>-total
                                                     gs_scr100-waers
                                            CHANGING ls_header-gross_amount.
    ls_header-gross_amount    = abs( ls_header-gross_amount ).

*-세액
    CLEAR: lt_tax, ls_tax.
    ls_tax-tax_code = gs_scr100-mwskz.

    PERFORM bapi_currency_conv_to_external USING    <ls_bapi_split>-wmwst
                                                    gs_scr100-waers
                                           CHANGING ls_tax-tax_amount.
    ls_tax-tax_amount = abs( ls_tax-tax_amount ).
    APPEND ls_tax TO lt_tax.

*> 품목 추출
    CLEAR: lt_item_bapi, ls_item_bapi, lv_itemno.
    READ TABLE lt_item_all WITH KEY split_key = <ls_bapi_split>-split_key
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
    LOOP AT lt_item_all INTO DATA(ls_item_all) FROM sy-tabix.
      IF <ls_bapi_split>-split_key NE ls_item_all-split_key.
        EXIT.
      ENDIF.
      CLEAR ls_item_bapi.

      "대표 자재명 설정.
      IF <ls_bapi_split>-matnr_tx IS INITIAL.
        <ls_bapi_split>-matnr_tx = ls_item_all-matnr_tx.
      ENDIF.

      MOVE-CORRESPONDING ls_item_all TO ls_item_bapi.
      ADD 1 TO lv_itemno.
      ls_item_bapi-invoice_doc_item = lv_itemno.

      APPEND ls_item_bapi TO lt_item_bapi.
*     (+)U6 START
*      SELECT SINGLE VRTKZ INTO @DATA(LV_VRTKZ)
*        FROM EKPO
*       WHERE EBELN = @LS_ITEM_ALL-PO_NUMBER
*         AND EBELP = @LS_ITEM_ALL-PO_ITEM
*         AND VRTKZ <> @SPACE.
      SORT gt_ekpo BY ebeln ebelp .
      READ TABLE gt_ekpo INTO DATA(ls_ekpo) WITH KEY ebeln = ls_item_all-po_number
                                                     ebelp = ls_item_all-po_item BINARY SEARCH.

      CHECK ls_ekpo-vrtkz <> space.
*
*      SELECT EBELN, EBELP,ZEKKN, SHKZG, MENGE, WRBTR   "C~GJAHR, C~BELNR, C~BUZEI,
*        FROM EKBE_MA
*       WHERE EBELN = @LS_ITEM_ALL-PO_NUMBER
*         AND EBELP = @LS_ITEM_ALL-PO_ITEM
*         AND VGABE = '1'
*         AND GJAHR = @LS_ITEM_ALL-REF_DOC_YEAR
*         AND BELNR = @LS_ITEM_ALL-REF_DOC
*         AND BUZEI = @LS_ITEM_ALL-REF_DOC_IT
*         INTO TABLE @DATA(LT_ACCOUNT).

*      SORT LT_ACCOUNT BY EBELN EBELP ZEKKN.


      CLEAR :  lt_account_sum[].", LT_ACCOUNTINGDATA[].
      LOOP AT gt_ekbe_ma  INTO DATA(ls_ekbe_ma) WHERE ebeln = ls_item_all-po_number
                                                  AND ebelp = ls_item_all-po_item
                                                  AND gjahr = ls_item_all-ref_doc_year
                                                  AND belnr = ls_item_all-ref_doc
                                                  AND buzei = ls_item_all-ref_doc_it.
        ls_account_sum = CORRESPONDING #( ls_ekbe_ma ) .
        IF ls_ekbe_ma-shkzg  = 'H'.
          ls_account_sum-menge = ls_ekbe_ma-menge * -1.
          ls_account_sum-wrbtr = ls_ekbe_ma-wrbtr * -1.
        ENDIF.
        COLLECT ls_account_sum INTO lt_account_sum.
        CLEAR ls_account_sum.
      ENDLOOP.

      LOOP AT lt_account_sum INTO ls_account_sum.
        ls_accountingdata-invoice_doc_item = lv_itemno.
        ls_accountingdata-serial_no        = ls_account_sum-zekkn.
        PERFORM bapi_currency_conv_to_external USING    ls_account_sum-wrbtr
                                                        gs_scr100-waers
                                               CHANGING ls_accountingdata-item_amount.
*        LS_ACCOUNTINGDATA-ITEM_AMOUNT      = LS_ACCOUNT_SUM-WRBTR.
        ls_accountingdata-quantity         = ls_account_sum-menge.
        ls_accountingdata-po_unit          = ls_item_all-po_unit.
        APPEND ls_accountingdata TO lt_accountingdata.
        CLEAR : ls_accountingdata.
      ENDLOOP.
*     (+)U6 END
    ENDLOOP.

    <ls_bapi_split>-item_cnt = lv_itemno.

    call function 'ZFMM_INCOMINGINVOICE_CREATE'
      EXPORTING
        is_headerdata       = ls_header
      IMPORTING
        es_invoicedocnumber = ls_result
      TABLES
        it_taxdata          = lt_tax
*       IT_VENDORITEMSPLITDATA = LT_AP_SPLIT
        it_itemdata         = lt_item_bapi
        it_accountingdata   = lt_accountingdata    "(+)U6
*       IT_GLACCOUNTDATA    = LT_GL_ACC
*       IT_MATERIALDATA     = LT_MATDATA
        et_return           = lt_return.

    CLEAR: ls_return.
    IF sy-subrc EQ 0 AND
       ls_result-inv_doc_no IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CONCATENATE ls_result-inv_doc_no ls_result-fisc_year
             INTO gs_scr100-belnr SEPARATED BY space.

      <ls_bapi_split>-belnr = ls_result-inv_doc_no.
      <ls_bapi_split>-gjahr = ls_result-fisc_year.
      <ls_bapi_split>-awkey = <ls_bapi_split>-belnr && <ls_bapi_split>-gjahr.
      CONDENSE <ls_bapi_split>-awkey NO-GAPS.

      ADD 1 TO lv_succe.

    ELSE.
      ADD 1 TO lv_error.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      "송장 처리에 실패하였습니다. 메시지를 확인하세요.
*      MESSAGE S055 DISPLAY LIKE 'E' WITH TEXT-M03.
*      PERFORM SET_SIGNAL_STATUS USING    'E'
*                                CHANGING GS_SCR100-STATUS.
    ENDIF.

    APPEND LINES OF lt_return TO gt_log.

    CLEAR: lt_return, ls_result.
  ENDLOOP.

*--------------------------------------------------------------------*
*> 전자 세금계산서 맵핑
*--------------------------------------------------------------------*
  "종이세금계산서가 아니고 원화이면 전자세금계산서 연계
  READ TABLE gt_paper_tax WITH KEY mwskz = gs_scr100-mwskz
                          BINARY SEARCH
                          TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0 AND p_r11 = 'X'.
*> 정발행
    IF p_r1 EQ 'X'.
      "ERROR 가 하나라도 없으면 전자세금계산서 MAPPING
      DATA lv_mapping_sucss TYPE c.
      CLEAR lv_mapping_sucss.
      IF lv_error EQ 0.
        "전자세금계산서와 맵핑, 단 조건에 맞지 않으면 화면 이동하여 맵핑
        PERFORM direct_mapping_iv_split CHANGING lv_mapping_sucss.
        "맵핑안되면 화면이동하여 직접 맵핑.
        IF lv_map_sucss NE 'S'.
          PERFORM move_to_mapping_iv_split.
        ENDIF.
      ENDIF.
*> 역발행
    ELSE.
      PERFORM create_req_re_bapi_split.
    ENDIF.
  ENDIF.

  PERFORM item_list_update_by_split.

  IF lv_succe NE 0.
    MESSAGE s137 WITH '송장문서 생성'(m56) lv_total lv_succe lv_error.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  DIRECT_MAPPING_INVOICE
*&---------------------------------------------------------------------*
*       생성된 전표를 MAPPING 하기 위한 프로그램을 호출한다.
*----------------------------------------------------------------------*
FORM direct_mapping_iv_split CHANGING cv_mapping_sucss.

**  DATA LR_LIFNR  TYPE RANGE OF LIFNR.
**  DATA LR_BLART  TYPE RANGE OF BLART   .
**  DATA LR_BUDAT  TYPE RANGE OF BUDAT   .
**  DATA LR_USNAM  TYPE RANGE OF USNAM   .
**  DATA LR_STCD2  TYPE RANGE OF ZDTV3E_SU_ID.
*
*  DATA lv_lifnr  TYPE lfa1-lifnr.
*  DATA lv_stcd2  TYPE lfa1-stcd2.
*
*  DATA: ls_return   TYPE zdtv3s_return,
*        lt_ap_table TYPE TABLE OF zdtv3t_ap_ext_d,
*        ls_ap_table TYPE zdtv3t_ap_ext_d,
*        lv_ext_seq  TYPE zdtv3t_ap_ext_d-ext_seq.
*
**  LV_AWKEY = GS_HEAD-BELNR2 && GS_HEAD-GJAHR2.
**  CONDENSE LV_AWKEY NO-GAPS.
*
*
**-회계전표번호
*  DATA(lt_tmp_bapi) = gt_bapi_split[].
*  SORT lt_tmp_bapi BY awkey.
*  DELETE ADJACENT DUPLICATES FROM lt_tmp_bapi COMPARING awkey.
*  IF NOT lt_tmp_bapi[] IS INITIAL.
*    SELECT belnr, gjahr, awkey
*      FROM bkpf
*       FOR ALL ENTRIES IN @lt_tmp_bapi
*     WHERE awkey = @lt_tmp_bapi-awkey
*       AND bukrs = @p_bukrs
*           AND blart = @gc_re
*           AND bldat = @gs_scr100-bldat
*           AND budat = @gs_scr100-budat
*           AND cpudt = @sy-datum
*           AND usnam = @sy-uname
*      INTO TABLE @DATA(lt_bkpf).
*    FREE lt_tmp_bapi.
*    SORT lt_bkpf BY awkey.
*  ENDIF.
*
**  _G_APPEND3 LR_BLART 'I' GC_EQ GC_RE.
**  _G_APPEND3 LR_BUDAT 'I' GC_EQ GS_SCR100-BUDAT.
**  _G_APPEND3 LR_USNAM 'I' GC_EQ SY-UNAME.
*
*  "송장발행처
*  READ TABLE gt_bapi_split INTO DATA(ls_bapi_split) INDEX 1.
*  SELECT SINGLE b~lifnr b~stcd2
*            INTO (lv_lifnr,
*                  lv_stcd2)
*            FROM rbkp AS a INNER JOIN lfa1 AS b
*              ON a~lifnr = b~lifnr
*           WHERE a~belnr EQ ls_bapi_split-belnr
*             AND a~gjahr EQ ls_bapi_split-gjahr.
*
**  _G_APPEND3 LR_LIFNR 'I' GC_EQ LV_LIFNR.
**  _G_APPEND3 LR_STCD2 'I' GC_EQ LV_STCD2.
*
**U4> 2022.05.02 : 전자세금계산서 맵핑 시 영/면세 구분 - START - 제거(원복)
*  SELECT SINGLE a~inv_seq
*           FROM zdtv3t_ap_head AS a INNER JOIN zdtv3t_ap_ext AS b
*             ON a~bukrs        = b~bukrs          AND
*                a~issue_date   = b~issue_date     AND
*                a~bupla        = b~bupla          AND
*                a~inv_seq      = b~inv_seq
*          WHERE a~bukrs        = @gs_scr100-bukrs
*            AND a~su_id        = @lv_stcd2
*            AND a~issue_date   = @gs_scr100-budat
*            AND b~use_doc      = @space
*            AND a~chargetotal  = @gs_scr100-dmbtr
*    INTO @DATA(lv_inv_seq).
*
*  CHECK sy-subrc EQ 0.
*
**재무운영팀에서 영세/면세 세금계산서 MAPPING 처리 프로그램을 수
**정예정으로 MM 기능구현 불필요하여 로직 막음...
**  DATA LV_INV_SEQ TYPE ZDTV3T_AP_HEAD-INV_SEQ.
**  IF GS_CONFIG_E1013-FIELD10 IS INITIAL.
***> OLD
**    SELECT SINGLE A~INV_SEQ
**             FROM ZDTV3T_AP_HEAD AS A INNER JOIN ZDTV3T_AP_EXT AS B
**               ON A~BUKRS        = B~BUKRS          AND
**                  A~ISSUE_DATE   = B~ISSUE_DATE     AND
**                  A~BUPLA        = B~BUPLA          AND
**                  A~INV_SEQ      = B~INV_SEQ
**            WHERE A~BUKRS        = @GS_SCR100-BUKRS
**              AND A~SU_ID        = @LV_STCD2
**              AND A~ISSUE_DATE   = @GS_SCR100-BUDAT
**              AND B~USE_DOC      = @SPACE
**              AND A~CHARGETOTAL  = @GS_SCR100-DMBTR
**      INTO @LV_INV_SEQ.
**
**    CHECK SY-SUBRC EQ 0.
**  ELSE.
***> NEW
**    CALL METHOD ZCL_MM_COMMON=>GET_EACC_INV_SEQ(
**      EXPORTING
**        IV_BUKRS     = GS_SCR100-BUKRS
**        IV_BUPLA     = GS_SCR100-BUPLA
**        IV_STCD2     = LV_STCD2
**        IV_BUDAT     = GS_SCR100-BUDAT
**        IV_MWSKZ     = GS_SCR100-MWSKZ
**        IV_DMBTR     = CONV DMBTR( GS_SCR100-DMBTR )
**      IMPORTING
**        ES_EACC_DATA = DATA(LS_EACC_DATA)
**      RECEIVING
**        RV_SUBRC     = DATA(LV_SUBRC) ).
**
**    CHECK LV_SUBRC EQ 0.
**
**    LV_INV_SEQ = LS_EACC_DATA-INV_SEQ.
**  ENDIF.
*
*  CLEAR: lv_ext_seq.
*  LOOP AT gt_bapi_split INTO ls_bapi_split.
*    CLEAR ls_ap_table.
*    ADD 1 TO lv_ext_seq.
*    ls_ap_table-inv_seq = lv_inv_seq.
*    ls_ap_table-ext_seq = lv_ext_seq.
*    ls_ap_table-zgjah = ls_bapi_split-gjahr.
*    ls_ap_table-belnr = ls_bapi_split-belnr.
*    ls_ap_table-bukrs = gs_scr100-bukrs.
*    ls_ap_table-budat = gs_scr100-budat.
*    ls_ap_table-blart = gc_re.
*    ls_ap_table-mwskz = gs_scr100-mwskz.
*    ls_ap_table-dmbtr = ls_bapi_split-total.
*
*    APPEND ls_ap_table TO lt_ap_table.
*  ENDLOOP.
*
*
*  call function 'ZDTV3SAP_USER_DOCUMAP'
*    EXPORTING
*      inv_seq  = ls_ap_table-inv_seq
**     ISSUE_ID =
*      inv_type = 'AP'
**     INV_DEL  =
*    IMPORTING
*      return   = ls_return
*    TABLES
*      it_data  = lt_ap_table.
*
*  IF ls_return-type EQ 'S'.
*    cv_mapping_sucss = 'S'.
*    MESSAGE i003 WITH '전자세금계산서 맵핑이 완료되었습니다:'(m41)
*                      space ls_ap_table-inv_seq.
*  ELSE.
*    cv_mapping_sucss = 'E'.
*  ENDIF.


ENDFORM. "DIRECT_MAPPING_INVOICE
*&---------------------------------------------------------------------*
*&      FORM  MOVE_TO_MAPPING_IV_SPLIT
*&---------------------------------------------------------------------*
*       생성된 전표를 MAPPING 하기 위한 프로그램을 호출한다.
*----------------------------------------------------------------------*
FORM move_to_mapping_iv_split.

  DATA lr_lifnr  TYPE RANGE OF lifnr.
  DATA lr_gjahr  TYPE RANGE OF gjahr   .
  DATA lr_bldat  TYPE RANGE OF bldat   .
  DATA lr_blart  TYPE RANGE OF blart   .
  DATA lr_budat  TYPE RANGE OF budat   .
  DATA lr_belnr  TYPE RANGE OF belnr_d .
  DATA lr_usnam  TYPE RANGE OF usnam   .
*  DATA lr_stcd2  TYPE RANGE OF zdtv3e_su_id.
*  DATA lr_email  TYPE RANGE OF zdtv3e_ip_email1.

  DATA lv_belnr  TYPE bkpf-belnr.
  DATA lv_gjahr  TYPE bkpf-gjahr.
*  DATA LV_AWKEY  TYPE BKPF-AWKEY.
  DATA lv_text   TYPE string.
  DATA lv_chk(1) TYPE c.
  DATA lv_date_f TYPE p0001-begda.
  DATA lv_date_t TYPE p0001-begda.
  DATA lv_bldat  TYPE p0001-begda.
  DATA lv_lifnr  TYPE lfa1-lifnr.
  DATA lv_stcd2  TYPE lfa1-stcd2.


  IF p_r1 EQ abap_true.  "정발행인 경우
    lv_text = TEXT-m19.  "전표 Mapping 화면으로 이동 하시겠습니까?
  ELSE.
    lv_text = TEXT-m28.  "역발행 요청 화면으로 이동 하시겠습니까?
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = '확인'
      text_question         = lv_text
      text_button_1         = 'Yes'
      icon_button_1         = 'ICON_ALLOW'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_REJECT'
      default_button        = '2'
      display_cancel_button = space
      popup_type            = 'ICON_MESSAGE_QUESTION'
    IMPORTING
      answer                = lv_chk
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_chk EQ '1'.

*-회계전표번호
  DATA(lt_tmp_bapi) = gt_bapi_split[].
  SORT lt_tmp_bapi BY awkey.
  DELETE ADJACENT DUPLICATES FROM lt_tmp_bapi COMPARING awkey.
  IF NOT lt_tmp_bapi[] IS INITIAL.
    SELECT belnr, gjahr, awkey
      FROM bkpf
       FOR ALL ENTRIES IN @lt_tmp_bapi
     WHERE awkey = @lt_tmp_bapi-awkey
       AND bukrs = @p_bukrs
           AND blart = @gc_re
           AND bldat = @gs_scr100-bldat
           AND budat = @gs_scr100-budat
           AND cpudt = @sy-datum
           AND usnam = @sy-uname
      INTO TABLE @DATA(lt_bkpf).
    FREE lt_tmp_bapi.
    SORT lt_bkpf BY awkey.
  ENDIF.

  LOOP AT lt_bkpf INTO DATA(ls_bkpf).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_bkpf-belnr ) TO lr_belnr.
  ENDLOOP.


  _g_append3 lr_gjahr 'I' gc_eq lv_gjahr.
  _g_append3 lr_blart 'I' gc_eq gc_re.
  _g_append3 lr_budat 'I' gc_eq gs_scr100-budat.
  _g_append3 lr_belnr 'I' gc_eq lv_belnr.
  _g_append3 lr_usnam 'I' gc_eq sy-uname.

  "송장발행처
  READ TABLE gt_bapi_split INTO DATA(ls_bapi_split) INDEX 1.
  SELECT SINGLE b~lifnr b~stcd2
            INTO (lv_lifnr,
                  lv_stcd2)
            FROM rbkp AS a INNER JOIN lfa1 AS b
              ON a~lifnr = b~lifnr
           WHERE a~belnr EQ ls_bapi_split-belnr
             AND a~gjahr EQ ls_bapi_split-gjahr.

  _g_append3 lr_lifnr 'I' gc_eq lv_lifnr.
*  _g_append3 lr_stcd2 'I' gc_eq lv_stcd2.


  lv_bldat = gs_scr100-bldat.
  IF lv_bldat IS INITIAL.
    lv_bldat = sy-datum.
  ENDIF.
  "작성일 기준 이전 15일
  PERFORM get_calculate_date USING    lv_bldat '-'
                              CHANGING lv_date_f.

  "작성일 기준 이후 15일
  PERFORM get_calculate_date USING    lv_bldat '+'
                              CHANGING lv_date_t.
  _g_append4 lr_bldat 'I' gc_bt lv_date_f lv_date_t.

  CASE p_r1.
    WHEN abap_true.  "정발행
      SUBMIT zdtv3_ap_p01 WITH p_bukrs EQ p_bukrs    "회사코드
                          WITH s_datum IN lr_bldat       "기준일자
*                          WITH s_stcd2 IN lr_stcd2       "사업자번호
                          WITH s_blart IN lr_blart       "전표유형
                          WITH s_budat IN lr_budat       "전표전기일
                          WITH s_bln   IN lr_belnr       "전표번호
*                          WITH s_email IN lr_email       "이메일
                          "VIA SELECTION-SCREEN
                           AND RETURN.

    WHEN OTHERS.     "역발행
      "별도 화면 이동 존재
  ENDCASE.


ENDFORM. " MAPPING_INVOICE
*&---------------------------------------------------------------------*
*& Form CREATE_REQ_RE_BAPI_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_req_re_bapi_split.

**  DATA LV_BELNR  TYPE BKPF-BELNR.
**  DATA LV_GJAHR  TYPE BKPF-GJAHR.
**  DATA LV_AWKEY  TYPE BKPF-AWKEY.
*  DATA lv_lifnr  TYPE lfa1-lifnr.
*  DATA lv_stcd2  TYPE lfa1-stcd2.
*
*  "금액이 마이너스면 역발행 하지 않음 (반품포함)
*  CHECK gs_scr100-dmbtr2 > 0.
*
**-회계전표번호
*  DATA(lt_tmp_bapi) = gt_bapi_split[].
*  SORT lt_tmp_bapi BY awkey.
*  DELETE ADJACENT DUPLICATES FROM lt_tmp_bapi COMPARING awkey.
*  IF NOT lt_tmp_bapi[] IS INITIAL.
*    SELECT belnr, gjahr, awkey
*      FROM bkpf
*       FOR ALL ENTRIES IN @lt_tmp_bapi
*     WHERE awkey = @lt_tmp_bapi-awkey
*       AND bukrs = @p_bukrs
*           AND blart = @gc_re
*           AND bldat = @gs_scr100-bldat
*           AND budat = @gs_scr100-budat
*           AND cpudt = @sy-datum
*           AND usnam = @sy-uname
*      INTO TABLE @DATA(lt_bkpf).
*    FREE lt_tmp_bapi.
*    SORT lt_bkpf BY awkey.
*  ENDIF.
*
*
*  "송장발행처
*  READ TABLE gt_bapi_split INTO DATA(ls_bapi_split) INDEX 1.
*  SELECT SINGLE b~lifnr b~stcd2
*            INTO (lv_lifnr,
*                  lv_stcd2)
*            FROM rbkp AS a INNER JOIN lfa1 AS b
*              ON a~lifnr = b~lifnr
*           WHERE a~belnr EQ ls_bapi_split-belnr
*             AND a~gjahr EQ ls_bapi_split-gjahr.
*
*  CHECK sy-subrc EQ 0.
*
*  DATA: lt_ap_header TYPE TABLE OF zdtv3s_ap_hd,
*        ls_ap_header TYPE zdtv3s_ap_hd,
*        ls_lfa1      TYPE lfa1.
*
*  DATA: lt_ap_item     TYPE TABLE OF zdtv3s_ap_im,
*        ls_ap_item     TYPE zdtv3s_ap_im,
*        lv_item_line_c TYPE char5.
*
*  DATA: lt_ap_ex   TYPE TABLE OF zdtv3s_ap_ex,
*        ls_ap_ex   TYPE zdtv3s_ap_ex,
*        lt_ap_ex_d TYPE TABLE OF zdtv3s_ap_ex_d,
*        ls_ap_ex_d TYPE zdtv3s_ap_ex_d.
*
*  CONSTANTS: lc_msgid     TYPE sy-msgid VALUE 'ZMM01',
*             lc_msgno     TYPE sy-msgno VALUE '003'.
*  CONSTANTS: lc_eacc_code TYPE zdtv3s_return-code VALUE '13016'.
*
*  DATA: lt_return    TYPE TABLE OF zdtv3s_return.
*  DATA: lt_msg_popup TYPE TABLE OF bapiret2,
*        lv_total     TYPE i,
*        lv_succe     TYPE i,
*        lv_error     TYPE i.
*
***********************************************************************
**> HEADER DATA 구성
***********************************************************************
*  CONSTANTS:
*    lc_type_code_0101 TYPE zdtv3s_ap_hd-type_code VALUE '0101',  "일반
*    lc_type_code_0102 TYPE zdtv3s_ap_hd-type_code VALUE '0102', "영세
*    lc_purp_code_02   TYPE zdtv3s_ap_hd-purp_code VALUE '02', "청구
*    lc_ip_typecode_01 TYPE zdtv3s_ap_hd-ip_typecode VALUE '01', "사업자등록으로 진행
*    lc_type_code_0301 TYPE zdtv3s_ap_hd-type_code VALUE '0301'.  "일반계산서
*
*  CLEAR: ls_ap_header, lt_ap_header.
*  ls_ap_header-bukrs = gs_scr100-bukrs.
*  ls_ap_header-issue_date = gs_scr100-bldat.
*  ls_ap_header-bupla = gs_scr100-bupla.
*  ls_ap_header-inv_sign = 'X'. "역발행 구분
*
**U5> 역발행시 면세면 일반계산서로 발행
**  IF GS_SCR100-WMWST2 EQ 0.
**    LS_AP_HEADER-TYPE_CODE = LC_TYPE_CODE_0102. "영세.
**  ELSE.
**    LS_AP_HEADER-TYPE_CODE = LC_TYPE_CODE_0101. "일반
**  ENDIF.
*
*  IF gs_scr100-mwskz = 'Y0' OR  "면세이면..
*     gs_scr100-mwskz = 'Y1' OR
*     gs_scr100-mwskz = 'Y2'.
*    ls_ap_header-type_code = lc_type_code_0301. "일반계산서
*  ELSE.
*    IF gs_scr100-wmwst2 EQ 0.
*      ls_ap_header-type_code = lc_type_code_0102. "영세.
*    ELSE.
*      ls_ap_header-type_code = lc_type_code_0101. "일반
*    ENDIF.
*  ENDIF.
*
*  ls_ap_header-purp_code = '02'. "무조건 청구
*
**> 공급자 정보
*  ls_ap_header-su_id = gs_scr100-stceg.
*
*  CALL FUNCTION 'WY_LFA1_SINGLE_READ'
*    EXPORTING
*      pi_lifnr = gs_scr100-lifnr
*    IMPORTING
*      po_lfa1  = ls_lfa1.
*
*  ls_ap_header-su_name = ls_lfa1-name1.    "사업자명
*  ls_ap_header-su_addr = ls_lfa1-ort01 && ls_lfa1-stras.  "주소
*
*  SELECT person_email1, person_email2, repres, gestyp, indtyp
*    FROM idkr_venvat
*   WHERE lifnr = @ls_lfa1-lifnr
*     AND datab <= @sy-datum
*   ORDER BY datab DESCENDING
*    INTO TABLE @DATA(lt_lfa1_etc).
*
*  SELECT SINGLE intad
*    FROM lfb1
*   WHERE lifnr = @gs_scr100-lifnr
*     AND bukrs = @gs_scr100-bukrs
*    INTO @ls_ap_header-su_email.
*
*  READ TABLE lt_lfa1_etc INTO DATA(ls_lfa1_etc) INDEX 1.
*  IF sy-subrc EQ 0.
*    ls_ap_header-su_repres = ls_lfa1_etc-repres.  "대표자명
*
*    ls_ap_header-su_bustype = ls_lfa1_etc-indtyp.  "업태
*    ls_ap_header-su_indtype = ls_lfa1_etc-gestyp.  "업종
*  ENDIF.
*
*
**> 공급받는 자
*  SELECT SINGLE taxnumber2, businessplacename, taxinvoicerepresentativename,
*                addressname, industrytype, businesstype
*    FROM pkrbupla
*   WHERE branch = @gs_scr100-bupla
*     AND companycode = @gs_scr100-bukrs
*    INTO @DATA(ls_pkrbupla).
*
*  ls_ap_header-ip_typecode = lc_ip_typecode_01.  "사업자등록번호로 등록.
*  ls_ap_header-ip_id = ls_pkrbupla-taxnumber2.  "사업자등록번호.
*  ls_ap_header-ip_name = ls_pkrbupla-businessplacename.  "사업체명.
*  ls_ap_header-ip_repres = ls_pkrbupla-taxinvoicerepresentativename.  "대표자명.
*  ls_ap_header-ip_addr = ls_pkrbupla-addressname.  "주소.
*  ls_ap_header-ip_bustype = ls_pkrbupla-industrytype.  "업태.
*  ls_ap_header-ip_indtype = ls_pkrbupla-businesstype.  "업종
*  ls_ap_header-ip_email1 = gs_scr100-zexpen_eml.  "담당자 이메일
*
*  LOOP AT gt_bapi_split ASSIGNING FIELD-SYMBOL(<ls_bapi_split>).
*    ls_ap_header-chargetotal = <ls_bapi_split>-dmbtr.  "총 공급가액
*    ls_ap_header-taxtotal = <ls_bapi_split>-wmwst.  "총 세액 합계
*    ls_ap_header-grandtotal = <ls_bapi_split>-total.  "총액(공급가액 + 세액)
*    APPEND ls_ap_header TO lt_ap_header.
*
***********************************************************************
**> ITEM DATA 구성 (기본적으로 100 건이 넘기 때문에 무조건 1건만 보냄)
***********************************************************************
*    CLEAR: ls_ap_item, lt_ap_item.
*
*    ls_ap_item-good_seq = '1'.    "품목 일련번호
*    ls_ap_item-good_date = gs_head-budat. "입고일자
*
*    "물품명
*    lv_item_line_c = <ls_bapi_split>-item_cnt.
*    CONDENSE lv_item_line_c.
*    CONCATENATE <ls_bapi_split>-matnr_tx '외' lv_item_line_c '건'
*           INTO ls_ap_item-good_name SEPARATED BY space.
*
*    ls_ap_item-good_invamount = ls_ap_header-chargetotal.
*    ls_ap_item-good_taxamount = ls_ap_header-taxtotal.
*    APPEND ls_ap_item TO lt_ap_item.
*
***********************************************************************
**> Extend 구성 (맵핑을 위한 데이터)
***********************************************************************
*    CLEAR: ls_ap_ex, lt_ap_ex.
*
*    ls_ap_ex-use_doc = 'X'.
*    APPEND ls_ap_ex TO lt_ap_ex.
*
*    CLEAR: ls_ap_ex_d, lt_ap_ex_d.
*    ls_ap_ex_d-ext_seq = 1. "일련번호
*
*    READ TABLE lt_bkpf INTO DATA(ls_bkpf)
*                       WITH KEY awkey = <ls_bapi_split>-awkey
*                       BINARY SEARCH.
*
*    ls_ap_ex_d-belnr = ls_bkpf-belnr. "회계전표번호
*    ls_ap_ex_d-budat = gs_scr100-budat. "전표의 전기일
*    ls_ap_ex_d-bldat = gs_scr100-bldat. "전표의 증빙일
*    ls_ap_ex_d-blart = gc_re. "전표유형
*    ls_ap_ex_d-mwskz = gs_scr100-mwskz. "세금코드
*    ls_ap_ex_d-dmbtr = <ls_bapi_split>-dmbtr. "현지통화금액
*    ls_ap_ex_d-zgjah = <ls_bapi_split>-gjahr. "회계연도
*
*    APPEND ls_ap_ex_d TO lt_ap_ex_d.
*
***********************************************************************
**> 역발행 생성 시작
***********************************************************************
*    CLEAR: lt_return.
*    call function 'ZDTV3SAP_CRD_RAP_INV'
*      TABLES
*        inv_hd     = lt_ap_header
*        inv_im     = lt_ap_item
*        inv_ex     = lt_ap_ex
*        inv_ex_d   = lt_ap_ex_d
*        return     = lt_return
*      EXCEPTIONS
*        fail_issue = 1
*        OTHERS     = 2.
*
*    ADD 1 TO lv_total.
*
*    SORT lt_return BY type code.
*    READ TABLE lt_return INTO DATA(ls_return)
*                         WITH KEY type = 'S'
*                                  code = lc_eacc_code
*                         BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      <ls_bapi_split>-inv_seq = ls_return-inv_seq.
*      MESSAGE s003 WITH '역발행 전자세금계산서가 생성되었습니다:'(m42) space ls_return-inv_seq.
*      APPEND VALUE #( type = 'S' id = lc_msgid number = lc_msgno
*                      message_v1 = '역발행 전자세금계산서가 생성되었습니다:'(m42)
*                      message_v2 = ls_return-inv_seq
*              ) TO lt_msg_popup.
*      ADD 1 TO lv_succe.
*    ELSE.
*      MESSAGE s053 WITH '역발행 전자세금계산서 생성'(m43) DISPLAY LIKE 'E'.
*
*      LOOP AT lt_return INTO ls_return.
*        APPEND VALUE #( message_v1 = ls_return-message
*                        type = ls_return-type
*                        id = lc_msgid
*                        number = lc_msgno
*                ) TO lt_msg_popup.
*      ENDLOOP.
*      ADD 1 TO lv_error.
*    ENDIF.
*
*  ENDLOOP.
*
*  MESSAGE i137 WITH '역발행 전자세금계산서'(m55) lv_total lv_succe lv_error.
*
*  CALL METHOD zcl_mm_common=>show_bapi_message( it_return = lt_msg_popup[] ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALCULATE_TAX_FOR_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_POST_SPLIT_DMBTR
*&      <-- LS_POST_SPLIT_WMWST
*&---------------------------------------------------------------------*
FORM calculate_tax_for_split USING iv_dmbtr
                              CHANGING cv_wmwst.

  DATA lt_tax   LIKE TABLE OF rtax1u15.
  DATA lv_fwste LIKE bset-fwste.
  DATA lv_dmbtr LIKE bseg-dmbtr.

  lv_dmbtr = iv_dmbtr.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = gs_scr100-bukrs
      i_mwskz = gs_scr100-mwskz
      i_waers = gs_scr100-waers
      i_wrbtr = lv_dmbtr
    IMPORTING
      e_fwste = lv_fwste
    TABLES
      t_mwdat = lt_tax
    EXCEPTIONS
      OTHERS  = 13.

  IF sy-subrc EQ 0.
    cv_wmwst = lv_fwste.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_LIST_UPDATE_BY_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM item_list_update_by_split.

  SORT gt_bapi_split BY split_key.

  SORT gt_head_all BY gjahr belnr buzei ebeln ebelp lfgja lfbnr lfpos.

  LOOP AT gt_head ASSIGNING FIELD-SYMBOL(<ls_head>)
                  WHERE cbox EQ abap_true
                    AND belnr2 EQ space.

    READ TABLE gt_bapi_split INTO DATA(ls_bapi_split)
                             WITH KEY split_key = <ls_head>-split_key
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      <ls_head>-belnr2 = ls_bapi_split-belnr.
      <ls_head>-gjahr2 = ls_bapi_split-gjahr.
*      <ls_head>-inv_seq = ls_bapi_split-inv_seq.

      READ TABLE gt_head_all ASSIGNING FIELD-SYMBOL(<ls_head_all>)
                             WITH KEY gjahr = <ls_head>-gjahr
                                      belnr = <ls_head>-belnr
                                      buzei = <ls_head>-buzei
                                      ebeln = <ls_head>-ebeln
                                      ebelp = <ls_head>-ebelp
                                      lfgja = <ls_head>-lfgja
                                      lfbnr = <ls_head>-lfbnr
                                      lfpos = <ls_head>-lfpos
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_head_all>-belnr2 = ls_bapi_split-belnr.
        <ls_head_all>-gjahr2 = ls_bapi_split-gjahr.
*        <ls_head_all>-inv_seq = ls_bapi_split-inv_seq.
      ENDIF.

      "대표 1건만 설정.
      IF gs_scr100-belnr IS INITIAL.
        gs_scr100-belnr = ls_bapi_split-belnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT gs_scr100-belnr IS INITIAL.
    PERFORM set_signal_status USING    'S'
                              CHANGING gs_scr100-status.
  ENDIF.

  PERFORM refresh_alv USING gv_grid_head.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BAPI_HEADER_BY_SPLIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_HEADER
*&---------------------------------------------------------------------*
FORM set_bapi_header_by_split CHANGING cs_header TYPE zsmm_incinv_create_header.


  cs_header-invoice_type = 'IV'.

  IF gs_scr100-dmbtr2 < 0.                   "Credit Memo
    cs_header-invoice_type = 'CM'.
  ENDIF.

  cs_header-doc_type        = gc_re.
  cs_header-doc_date        = gs_scr100-bldat. "증빙일
  cs_header-pstng_date      = gs_scr100-budat. "전기일
  cs_header-comp_code       = gs_scr100-bukrs. "회사코드
  cs_header-currency        = gs_scr100-waers. "통화

  IF gs_scr100-zlsch EQ 'K'. "국책과제
    cs_header-housebankid = gs_scr100-hbkid. "거래은행
    cs_header-paymt_ref   = gs_scr100-hktid. "계정 ID
  ENDIF.

  cs_header-bline_date      = gs_scr100-zfbdt. "기산일
  cs_header-pmnttrms        = gs_scr100-zterm. "지급조건
  cs_header-business_place = gs_scr100-bupla.  "사업장

  "지급방법: 표준에서 자동 설정 예정이므로 전송하지 않음.
*  CS_HEADER-PYMT_METH       = GS_SCR100-ZLSCH. "지급방법

  cs_header-partner_bk      = gs_scr100-bvtyp. "파트너은행유형

  IF cs_header-header_txt IS INITIAL.
    cs_header-header_txt = gs_scr100-bktxt. "비고
  ENDIF.

  "환율
  IF p_r12 EQ abap_true.
    cs_header-exch_rate = gs_scr100-kursf.
  ENDIF.

  cs_header-vatdate = cs_header-doc_date. "부가세일자를 증빙일로 설정.

*U1> 2022.04.14 : 대체수취인 기능 추가
  IF gs_scr100-empfk IS NOT INITIAL.
    cs_header-payee_payer = gs_scr100-empfk. "대체수취인
  ENDIF.

*U1> 2022.04.14 : 송장발행처 기능 추가
  IF gs_scr100-diff_lifnr IS NOT INITIAL.
    cs_header-diff_inv = gs_scr100-diff_lifnr. "송장발행처
  ENDIF.

  "FI 요청: 사번 설정
*  SELECT SINGLE employ_no
*    FROM zsvmm_user_info
*   WHERE company = @cs_header-comp_code
*     AND user_id = @sy-uname
*    INTO @cs_header-ref_doc_no.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form P5_CALCULATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM p5_calculation .

  DATA: lv_net_value TYPE bseg-dmbtr,
        lv_total_amt TYPE bseg-wrbtr,
        lv_vat_amt   TYPE bset-fwste.

  CHECK gs_head-mwskz IN gr_mwskz_p.

*> 기준 통화 재계산
  CLEAR: lv_net_value, lv_total_amt, lv_vat_amt.

  lv_total_amt = gs_head-dmbtr.

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs = p_bukrs
      i_mwskz = gs_head-mwskz
      i_waers = gs_head-waers
      i_wrbtr = lv_total_amt
    IMPORTING
      e_fwnav = lv_vat_amt.

  lv_net_value = gs_head-dmbtr - lv_vat_amt.
  gs_head-dmbtr = lv_net_value.

*> Local 통화 재계산
  CLEAR: lv_net_value, lv_total_amt, lv_vat_amt.

  lv_total_amt = gs_head-dmbtr_krw.

  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs = p_bukrs
      i_mwskz = gs_head-mwskz
      i_waers = gv_local_cur
      i_wrbtr = lv_total_amt
    IMPORTING
      e_fwnav = lv_vat_amt.

  lv_net_value = gs_head-dmbtr_krw - lv_vat_amt.
  gs_head-dmbtr_krw = lv_net_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HELP_F4_EMPFK
*&---------------------------------------------------------------------*
*& U1. 2022.04.14 : 대체수취인 기능 추가
*&---------------------------------------------------------------------*
FORM help_f4_empfk .

  DATA: lt_return     TYPE TABLE OF ddshretval,
        lt_update     TYPE TABLE OF dynpread,
        ls_update     TYPE dynpread,
        lv_diff_lifnr TYPE lfa1-lifnr,
        lv_empfk      TYPE lfza-empfk.

  DATA: BEGIN OF ls_data,
          empfk TYPE lfza-empfk,
          name1 TYPE lfa1-name1,
        END OF ls_data,
        lt_data LIKE TABLE OF ls_data.

  CONSTANTS: lc_title(15) TYPE c VALUE '대체수취인',
             lc_retfield  TYPE fieldname VALUE 'EMPFK',
             lc_scr       TYPE help_info-dynprofld VALUE 'GS_SCR100-EMPFK',
             lc_scr_name  TYPE help_info-dynprofld VALUE 'GS_SCR100-EMPFK_TX'.

*> 송장발행처 검색
  PERFORM get_dynp_value USING 'GS_SCR100-DIFF_LIFNR' CHANGING lv_diff_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_diff_lifnr
    IMPORTING
      output = lv_diff_lifnr.

  "송장발행처 다르면 대체수취인 대상 재갱신
  IF lv_diff_lifnr NE gs_scr100-diff_lifnr.
    gs_scr100-diff_lifnr = lv_diff_lifnr.
    PERFORM set_init_empfk.
  ENDIF.

  CHECK NOT gv_empfk_used IS INITIAL.

  MOVE-CORRESPONDING gt_tax_lfza TO lt_data[].

  SORT lt_data BY empfk.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = lc_scr        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = lt_data
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_return-fieldval
      IMPORTING
        output = lv_empfk.

    READ TABLE lt_data INTO ls_data
                       WITH KEY empfk = lv_empfk
                       BINARY SEARCH.

    "코드
    CLEAR ls_update.
    ls_update-fieldname = lc_scr.
    ls_update-fieldvalue  = ls_data-empfk.
    APPEND ls_update TO lt_update.

    "명
    CLEAR ls_update.
    ls_update-fieldname = lc_scr_name.
    ls_update-fieldvalue  = ls_data-name1.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EMPFK_NM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_empfk_nm .

  CLEAR: gs_scr100-empfk_tx.

  IF gs_scr100-empfk IS NOT INITIAL.

    SORT gt_tax_lfza BY empfk.
    READ TABLE gt_tax_lfza INTO DATA(ls_tax_lfza)
                           WITH KEY empfk = gs_scr100-empfk
                           BINARY SEARCH.
    IF sy-subrc NE 0.
*   대체수취인 ( & ) 데이터가 존재하지 않습니다.
      MESSAGE e022 WITH '대체수취인'(f83) gs_scr100-empfk.
      EXIT.
    ELSE.
      gs_scr100-empfk_tx = ls_tax_lfza-name1.
    ENDIF.
  ENDIF.

*> 계좌도 정보 갱신
  PERFORM set_init_bank_acc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_EMPFK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_init_empfk .

  DATA: lv_basic_lifnr TYPE lfa1-lifnr.

  CLEAR: gv_empfk_used, gt_tax_lfza, gs_tax_lfza.

*> 송장발행처가 있으면 우선.
  IF NOT gs_scr100-diff_lifnr IS INITIAL.
    lv_basic_lifnr = gs_scr100-diff_lifnr.
  ELSE.
    lv_basic_lifnr = gs_scr100-lifnr.
  ENDIF.

  "대체수취인 사용여부 판단
  SELECT SINGLE xlfzb
    FROM lfb1
   WHERE lifnr  EQ  @lv_basic_lifnr
     AND bukrs  EQ  @p_bukrs
    INTO @gv_empfk_used.

  "대체수취인 목록 검색
  IF NOT gv_empfk_used IS INITIAL.
    SELECT a~lifnr, a~empfk, b~name1
      FROM lfza AS a  INNER JOIN lfa1 AS b
                         ON b~lifnr  EQ  a~empfk
      WHERE a~lifnr  EQ  @lv_basic_lifnr
        AND a~bukrs  EQ  @p_bukrs
       INTO CORRESPONDING FIELDS OF TABLE @gt_tax_lfza.
    SORT gt_tax_lfza BY empfk.
  ENDIF.

  "기본값 설정
  READ TABLE gt_tax_lfza INTO DATA(ls_tax_lfza) INDEX 1.
  IF NOT ls_tax_lfza-empfk IS INITIAL.
    gs_scr100-empfk = ls_tax_lfza-empfk.
    gs_scr100-empfk_tx = ls_tax_lfza-name1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_DIFF_LIFNR
*&---------------------------------------------------------------------*
*& U2> 2022.04.14 : 송장발행처 기능 추가.
*&---------------------------------------------------------------------*
FORM set_init_diff_lifnr .

  CLEAR: gv_diff_lifnr_used, gt_diff_lifnr, gs_diff_lifnr.

  DATA: lv_lifnr_ext      TYPE char20,
        lt_tmp_diff_lifnr LIKE TABLE OF gs_diff_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_scr100-lifnr
    IMPORTING
      output = lv_lifnr_ext.

  "송장발행처 추출.
  zcl_mm_common=>common_config(
     EXPORTING  is_common =  VALUE #( m = 'E1' d = 'E1010' s = 'E1016' )
                                     it_where = VALUE #(
                                                         ( field = 1 value = p_bukrs )
                                                         ( field = 2 value = lv_lifnr_ext )
                                                       )
     IMPORTING et_outtab = DATA(lt_config) ).

  LOOP AT lt_config INTO DATA(ls_config).
    CLEAR gs_diff_lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_config-field3
      IMPORTING
        output = gs_diff_lifnr-lifnr.

    APPEND gs_diff_lifnr TO lt_tmp_diff_lifnr.
  ENDLOOP.

  "송장발행처명 추출
  SORT lt_tmp_diff_lifnr BY lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_tmp_diff_lifnr COMPARING lifnr.
  IF NOT lt_tmp_diff_lifnr[] IS INITIAL.
    SELECT lifnr, name1
      FROM lfa1
       FOR ALL ENTRIES IN @lt_tmp_diff_lifnr
     WHERE lifnr = @lt_tmp_diff_lifnr-lifnr
      INTO CORRESPONDING FIELDS OF TABLE @gt_diff_lifnr.
    FREE lt_tmp_diff_lifnr.
    SORT gt_diff_lifnr BY lifnr.

    "송장발행처 기능 사용.
    IF NOT gt_diff_lifnr[] IS INITIAL.
      gv_diff_lifnr_used = 'X'.
    ENDIF.
  ENDIF.

  "송장발행처 기본값 설정.
  IF gs_scr100-diff_lifnr IS INITIAL.
    READ TABLE gt_diff_lifnr INTO gs_diff_lifnr INDEX 1.
    gs_scr100-diff_lifnr = gs_diff_lifnr-lifnr.
    gs_scr100-diff_lifnr_tx = gs_diff_lifnr-name1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONDITION_BY_ETC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_condition_by_etc CHANGING cv_chk.

*U1> 2022.04.14 : 송장발행처 점검
  IF NOT gs_scr100-diff_lifnr IS INITIAL.
    SORT gt_diff_lifnr BY lifnr.
    READ TABLE gt_diff_lifnr WITH KEY lifnr = gs_scr100-diff_lifnr
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
*     송장발행처 ( & ) 데이터가 존재하지 않습니다.
      MESSAGE s022 WITH '송장발행처'(f84) gs_scr100-diff_lifnr DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*U1> 2022.04.14 : 대체수취인 점검.
  IF NOT gs_scr100-empfk IS INITIAL.
    SORT gt_tax_lfza BY empfk.
    READ TABLE gt_tax_lfza WITH KEY empfk = gs_scr100-empfk
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
*     대체수취인 ( & ) 데이터가 존재하지 않습니다.
      MESSAGE s022 WITH '대체수취인'(f83) gs_scr100-empfk DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*U1> 2022.04.14 : 계좌점검.
  IF NOT gs_scr100-bvtyp IS INITIAL.
*    SELECT DISTINCT BPBANKACCOUNTINTERNALID AS BVTYP
*      FROM ZCCMM_BANKACCNT
*     WHERE SUPPLIER = @GS_SCR100-LIFNR
*       AND BANKACCOUNT IN @GR_BANKN
*       AND BPBANKACCOUNTINTERNALID = @GS_SCR100-BVTYP
*      INTO TABLE @DATA(LT_DATA).
    SORT gt_bank_acc BY bvtyp.
    READ TABLE gt_bank_acc WITH KEY bvtyp = gs_scr100-bvtyp
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      MESSAGE s015 WITH '공급업체은행계좌'(t40) gs_scr100-bvtyp gv_bukrs_code
                   DISPLAY LIKE 'E'.
      cv_chk = abap_true.
      EXIT.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DIFF_LIFNR_NM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_diff_lifnr_nm .

  CLEAR: gs_scr100-diff_lifnr_tx.


*  SORT GT_DIFF_LIFNR BY LIFNR.
  IF NOT gs_scr100-diff_lifnr IS INITIAL.
    SORT gt_diff_lifnr BY lifnr.
    READ TABLE gt_diff_lifnr INTO DATA(ls_diff_lifnr)
                             WITH KEY lifnr = gs_scr100-diff_lifnr
                             BINARY SEARCH.
    IF sy-subrc NE 0.
*   송장발행처 ( & ) 데이터가 존재하지 않습니다.
      MESSAGE e022 WITH '송장발행처'(f84) gs_scr100-diff_lifnr.
      EXIT.
    ELSE.
      gs_scr100-diff_lifnr_tx = ls_diff_lifnr-name1.
    ENDIF.
  ENDIF.

*> 송장발행처가 바뀌면 대체수취인 정보도 입력된 송장발행처 기준으로 변경
  PERFORM set_init_empfk.

*> 계좌도 갱신
  PERFORM set_init_bank_acc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HELP_F4_DIFF_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM help_f4_diff_lifnr .

  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread,
        lv_lifnr  TYPE lfa1-lifnr.

  CONSTANTS: lc_title(15) TYPE c VALUE '송장발행처',
             lc_retfield  TYPE fieldname VALUE 'LIFNR',
             lc_scr       TYPE help_info-dynprofld VALUE 'GS_SCR100-DIFF_LIFNR',
             lc_scr_name  TYPE help_info-dynprofld VALUE 'GS_SCR100-DIFF_LIFNR_TX'.

  CHECK NOT gv_diff_lifnr_used IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = lc_scr        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = gt_diff_lifnr
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_return-fieldval
      IMPORTING
        output = lv_lifnr.

    READ TABLE gt_diff_lifnr INTO DATA(ls_diff_lifnr)
                       WITH KEY lifnr = lv_lifnr
                       BINARY SEARCH.

    "코드
    CLEAR ls_update.
    ls_update-fieldname = lc_scr.
    ls_update-fieldvalue  = ls_diff_lifnr-lifnr.
    APPEND ls_update TO lt_update.

    "명
    CLEAR ls_update.
    ls_update-fieldname = lc_scr_name.
    ls_update-fieldvalue  = ls_diff_lifnr-name1.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_BANK_ACC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_init_bank_acc .

  DATA: lv_basic_lifnr TYPE lfa1-lifnr.

  CLEAR: gt_bank_acc, gs_bank_acc.

  "우선순위를 대체수취인 -> 송장발행처 -> 공급업체 순으로 검색
  IF NOT gs_scr100-empfk IS INITIAL.
    lv_basic_lifnr = gs_scr100-empfk.
  ELSE.
    IF NOT gs_scr100-diff_lifnr IS INITIAL.
      lv_basic_lifnr = gs_scr100-diff_lifnr.
    ELSE.
      lv_basic_lifnr = gs_scr100-lifnr.
    ENDIF.
  ENDIF.

  SELECT DISTINCT bpbankaccountinternalid AS bvtyp,
         bank, banka, bankaccount,
         bankaccountholdername
    FROM zccmm_bankaccnt
   WHERE supplier = @lv_basic_lifnr
     AND bankaccount IN @gr_bankn
    INTO CORRESPONDING FIELDS OF TABLE @gt_bank_acc.

  SORT gt_bank_acc BY bvtyp.

*U3> 2022.04.28 : 은행계좌 기본값은 MM 공통 IMG 로 결정.
*U4> GV로 변경.. 사용처가 많음..
* ZCL_MM_COMMON=>COMMON_CONFIG(
*    EXPORTING  IS_COMMON =  VALUE #( M = 'E1' D = 'E1010' S = 'E1013' )
*                                    IT_WHERE = VALUE #(
*                                                        ( FIELD = 1 VALUE = P_BUKRS )
*                                                      )
*    IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
*  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  "기본값 설정
  IF NOT gs_config_e1013-field1 IS INITIAL.
    IF gs_config_e1013-field8 = 'Y'.
      READ TABLE gt_bank_acc INTO DATA(ls_bank_acc) INDEX 1.
      IF sy-subrc EQ 0.
        gs_scr100-bvtyp = ls_bank_acc-bvtyp.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HELP_F4_BUPLA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM help_f4_bupla .


  DATA: lt_return TYPE TABLE OF ddshretval,
        lt_update TYPE TABLE OF dynpread,
        ls_update TYPE dynpread.

  CONSTANTS: lc_title(15)   TYPE c VALUE '사업장',
             lc_retfield    TYPE fieldname VALUE 'BUPLA',
             lc_scr_name    TYPE screen-name VALUE 'GS_SCR100-BUPLA',
             lc_scr_name_tx TYPE screen-name VALUE 'GS_SCR100-BUPLA_TX'.

  SORT gt_bupla BY bupla.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = lc_title
      retfield          = lc_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = lc_scr_name        "retfield 가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = gt_bupla
      return_tab        = lt_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    READ TABLE gt_bupla INTO DATA(ls_bupla)
                        WITH KEY bupla = ls_return-fieldval
                        BINARY SEARCH.

    CLEAR ls_update.
    ls_update-fieldname = lc_scr_name.
    ls_update-fieldvalue  = ls_bupla-bupla.
    APPEND ls_update TO lt_update.

    CLEAR ls_update.
    ls_update-fieldname = lc_scr_name_tx.
    ls_update-fieldvalue  = ls_bupla-name.
    APPEND ls_update TO lt_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_update. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BUPLA_NM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_bupla_nm .

  CLEAR: gs_scr100-bupla_tx.

  IF NOT gs_scr100-bupla IS INITIAL.
    SORT gt_bupla BY bupla.
    READ TABLE gt_bupla INTO DATA(ls_bupla)
                             WITH KEY bupla = gs_scr100-bupla
                             BINARY SEARCH.
    IF sy-subrc NE 0.
*   사업장 ( & ) 데이터가 존재하지 않습니다.
      MESSAGE e022 WITH '사업장'(m36) gs_scr100-bupla.
      EXIT.
    ELSE.
      gs_scr100-bupla_tx = ls_bupla-name.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ACCOUNTINGDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACCOUNTINGDATA
*&---------------------------------------------------------------------*
FORM set_accountingdata TABLES pt_accountingdata STRUCTURE zsmm_incinv_create_account
                          USING pv_itemno.

  DATA :  ls_accountingdata LIKE  zsmm_incinv_create_account.
  DATA : BEGIN OF ls_account_sum ,
            ebeln LIKE ekbe_ma-ebeln,
            ebelp LIKE ekbe_ma-ebelp,
            zekkn LIKE ekbe_ma-zekkn,
*            SHKZG LIKE EKBE_MA-SHKZG,
            menge LIKE ekbe_ma-menge,
            wrbtr LIKE ekbe_ma-wrbtr,
         END OF  ls_account_sum.
  DATA : lt_account_sum LIKE TABLE OF ls_account_sum.

  SORT gt_ekpo BY ebeln ebelp .
  READ TABLE gt_ekpo INTO DATA(ls_ekpo) WITH KEY ebeln = gs_head-ebeln
                                                 ebelp = gs_head-ebelp  BINARY SEARCH.
  CHECK ls_ekpo-vrtkz <> space.


  CLEAR :  lt_account_sum[].

  LOOP AT gt_ekbe_ma INTO DATA(ls_ekbe_ma) WHERE ebeln = gs_head-ebeln
                                             AND ebelp = gs_head-ebelp
                                             AND gjahr = gs_head-lfgja
                                             AND belnr = gs_head-lfbnr
                                             AND buzei = gs_head-lfpos.
    ls_account_sum = CORRESPONDING #( ls_ekbe_ma ) .

    IF ls_ekbe_ma-shkzg  = 'H'.
      ls_account_sum-menge = ls_ekbe_ma-menge * -1.
      ls_account_sum-wrbtr = ls_ekbe_ma-wrbtr * -1.
    ENDIF.
    COLLECT ls_account_sum INTO lt_account_sum.
    CLEAR ls_account_sum.
  ENDLOOP.


  LOOP AT lt_account_sum INTO ls_account_sum.
    ls_accountingdata-invoice_doc_item = pv_itemno.
    ls_accountingdata-serial_no        = ls_account_sum-zekkn.
    PERFORM bapi_currency_conv_to_external USING ls_account_sum-wrbtr
                                                 gs_scr100-waers
                                        CHANGING ls_accountingdata-item_amount.

    ls_accountingdata-quantity         = ls_account_sum-menge.
    ls_accountingdata-po_unit          = gs_head-meins.
    APPEND ls_accountingdata TO pt_accountingdata.
    CLEAR : ls_accountingdata.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PO_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_po_data .

  CLEAR gt_temp.
  gt_temp = VALUE #( FOR ls_temp IN gt_head WHERE ( cbox = abap_true AND
                                                    belnr2 =  space )
                                                   ( ls_temp ) ).

  SORT gt_temp BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM gt_temp COMPARING ebeln ebelp.

  CLEAR : gt_ekpo, gt_ekbe_ma.
  IF gt_temp[] IS NOT INITIAL.
    SELECT ebeln ebelp vrtkz
      INTO TABLE gt_ekpo
      FROM ekpo
       FOR ALL ENTRIES IN gt_temp
     WHERE ebeln = gt_temp-ebeln
       AND ebelp = gt_temp-ebelp
       AND vrtkz <> space .
  ENDIF.

  CLEAR gt_temp.
  gt_temp = VALUE #( FOR ls_temp IN gt_head WHERE ( cbox = abap_true AND
                                                    belnr2 =  space )
                                                   ( ls_temp ) ).

  SORT gt_temp BY ebeln ebelp lfgja lfbnr lfpos.
  DELETE ADJACENT DUPLICATES FROM gt_temp COMPARING ebeln ebelp lfgja lfbnr lfpos.

  IF gt_temp[] IS NOT INITIAL.
    SELECT ebeln ebelp zekkn  shkzg menge wrbtr gjahr belnr buzei
      INTO TABLE gt_ekbe_ma
      FROM ekbe_ma
       FOR ALL ENTRIES IN gt_temp
     WHERE ebeln = gt_temp-ebeln
       AND ebelp = gt_temp-ebelp
       AND vgabe = '1'
       AND gjahr = gt_temp-lfgja
       AND belnr = gt_temp-lfbnr
       AND buzei = gt_temp-lfpos.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_COMPLIANCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_compliance USING iv_chk_C.

 DATA lv_chk(1)    TYPE c.
 DATA lv_text_c  TYPE string.

  SELECT bukrs, ebeln, zqm_person
  FROM ekko
  FOR ALL ENTRIES IN @gt_head
  WHERE ebeln = @gt_head-ebeln
  INTO TABLE @DATA(lt_ekko).

  LOOP AT gt_head INTO gs_head.
   READ TABLE lt_ekko INTO DATA(ls_ekko)
                              WITH KEY ebeln = gs_head-ebeln.
   IF sy-subrc EQ 0.
    CONCATENATE '[구매 컴플라이언스 위반] PO:'
    ls_ekko-ebeln '검수자와 지출처리자가 동일합니다. 그래도 진행하시겠습니까?'
    INTO lv_text_c SEPARATED BY space.
    TRANSLATE ls_ekko-zqm_person TO UPPER CASE.

    IF ls_ekko-zqm_person = sy-uname.
     CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
       titlebar              = '확인'
       text_question         = lv_text_c
       text_button_1         = 'Yes'
       icon_button_1         = 'ICON_ALLOW'
       text_button_2         = 'No'
       icon_button_2         = 'ICON_REJECT'
       default_button        = '2'
       display_cancel_button = space
       popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
       answer                = lv_chk
      EXCEPTIONS
       text_not_found        = 1
      OTHERS                 = 2.

      IF lv_chk = '2'.
       iv_chk_c = abap_true.
      ENDIF.
      EXIT.
    ENDIF.
   ENDIF.
  ENDLOOP.
ENDFORM.
