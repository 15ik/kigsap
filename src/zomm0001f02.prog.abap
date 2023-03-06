*&---------------------------------------------------------------------*
*& Include          ZOMM0001F02
*&---------------------------------------------------------------------*
***************************************************************************
*  Start ALV Event Area
*
***************************************************************************
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*

***************************************************************************
*  End ALV Event Area
*
***************************************************************************
*&---------------------------------------------------------------------*
*& Form set_grid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_grid .

*---------------------------------------------------
* 그리드가 여러개일 경우 PARMA으로 구분하여 사용
*---------------------------------------------------

  IF grf_docking_con IS INITIAL.

* Creating Docing container instance
    PERFORM create_container.   "각 구간별 Container Split
*--------------------------------
* Create Alv Grid
*--------------------------------
*-- Top of page. ( header text 사용 안함)
    PERFORM create_alv_grid.

*-- Detail Grid.
    PERFORM create_alv_grid_dtl.

*--------------------------------
* Tree Grid..
*--------------------------------

    PERFORM build_fieldcatalog.

* create Hierarchy-header
    DATA ls_hierarchy_header TYPE treev_hhdr.
    PERFORM build_hierarchy_header CHANGING ls_hierarchy_header.

* create tree control
    CREATE OBJECT grf_tree
      EXPORTING
        parent                      = grf_body
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = ''
        no_html_header              = 'X'
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

*  saving variants
    DATA: ls_variant TYPE disvariant.
    ls_variant-report = sy-repid.

* excludeing function
    DATA : lt_tree_toolbar TYPE ui_functions.
    PERFORM excluding_toolbar TABLES lt_tree_toolbar.

* create emty tree-control
    CALL METHOD grf_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header  = ls_hierarchy_header
*       i_logo               = l_logo
        i_background_id      = 'ALV_TREE'
        i_save               = 'A'
        it_toolbar_excluding = lt_tree_toolbar
        is_variant           = ls_variant
      CHANGING
        it_outtab            = gt_tree
        it_fieldcatalog      = gt_tree_field.

* create hierarchy
    PERFORM create_hierarchy.

* Extend funtions of standard toolbar
    PERFORM change_toolbar.

* Tree event
    PERFORM register_events.

* this method must be called to send the data to the frontend
    CALL METHOD grf_tree->frontend_update.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container .

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT grf_docking_con
    EXPORTING
      repid     = sy-repid " 프로그램명 id
      dynnr     = sy-dynnr " 화면번호 (Screen) "
      side      = grf_docking_con->dock_at_top "
      extension = 10000.

*----------------------------------------------------
* Split Container (1 Row:header 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(lrf_splitter) = NEW cl_gui_splitter_container( parent  = grf_docking_con
                                                      no_autodef_progid_dynnr = 'X'
                                                      rows    = 2
                                                      columns = 1 ).
  lrf_splitter->set_row_mode( mode = cl_gui_splitter_container=>type_movable ).
  lrf_splitter->set_row_height( id = 1 height = 0 ).   " 조회안되도록 설정
  lrf_splitter->set_border( border = space ).

**--------------------------------
** Set Header Cocntainer
**--------------------------------
  DATA(lrf_cont) = lrf_splitter->get_container( row = 1 column = 1 ).

  DATA(lrf_splitter_html) = NEW cl_gui_splitter_container( parent  = lrf_cont
                                                           no_autodef_progid_dynnr = 'X'
                                                           rows    = 1
                                                           columns = 1 ).
  grf_head = lrf_splitter_html->get_container( row = 1 column = 1 ).

*--------------------------------
* Set Body Cocntainer
*--------------------------------
  DATA(lrf_grid_cont) = lrf_splitter->get_container( row = 2 column = 1 ).

  DATA(lrf_split_bdy) = NEW cl_gui_splitter_container( parent  = lrf_grid_cont
                                                       no_autodef_progid_dynnr = 'X'
                                                       rows    = 1
                                                       columns = 2 ).
*-- Column 비율
  lrf_split_bdy->set_column_width( EXPORTING id = 1 width = 20 ).
  lrf_split_bdy->set_column_width( EXPORTING id = 2 width = 80 ).

*-- Set Alv Container..
  grf_body    = lrf_split_bdy->get_container( row = 1 column = 1 ).
  grf_body_dtl = lrf_split_bdy->get_container( row = 1 column = 2 ).



ENDFORM. " CREATE_CONTAINER

*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC_FCAT  text
*----------------------------------------------------------------------*
FORM alv_grid_fcat_modify.

  FIELD-SYMBOLS :<ls_1>, <ls_2>, <ls_3>.
  DATA : lv_field  TYPE c LENGTH 30,
         lv_field1 TYPE c LENGTH 30,
         lv_field2 TYPE c LENGTH 30,
         lv_field3 TYPE c LENGTH 30.

  DATA : lv_count TYPE c LENGTH 2 VALUE 1.

  DATA: lv_tabname   TYPE ddobjname,
        lv_fieldname TYPE  dfies-fieldname,
        lt_tab       LIKE STANDARD TABLE OF dfies WITH HEADER LINE.

  DATA : ls_fcat TYPE lvc_s_fcat.

  DEFINE _l_h_set_fcat.
    CLEAR : ls_fcat.
    ls_fcat-fieldname    = &1.
    ls_fcat-col_pos      = &2.
    ls_fcat-key             = &3.
    ls_fcat-tech            = &4.
    ls_fcat-ref_table      = &5.
    ls_fcat-ref_field       = &6.
    APPEND ls_fcat TO gt_dtl_fcat.
  END-OF-DEFINITION.

  DEFINE _l_set_fcat.
    CLEAR : ls_fcat.
    ls_fcat-fieldname    = &1.
    ls_fcat-col_pos      = &2.
    ls_fcat-edit             = &3.
    ls_fcat-coltext        = &4.
*    ls_fcat-just             = 'C'.
    ls_fcat-ref_table      = &5.
    ls_fcat-ref_field       = &6.
    ls_fcat-datatype      = &7.
    ls_fcat-inttype         = &8.
    ls_fcat-intlen          = &9.

    IF ls_fcat-coltext = space.
       ls_fcat-no_out = abap_true.
    ENDIF.

    APPEND ls_fcat TO gt_dtl_fcat.

  END-OF-DEFINITION.

*  _l_h_set_fcat: 'ZMAIN_CAT'   lv_col   'X'  'X',
*                      'ZMIDD_CAT'   lv_col   'X'  'X' ,
*                      'ZSMAL_CAT'  lv_col   'X'  'X' .

  _l_h_set_fcat : 'STATU'   0  'X'  'X' 'ZSCN00003' 'STATU' ,
                        'CELLS'   0   'X'  'X' 'ZSCN00003' 'CELLS',
                        'CELLC'   0   'X'  'X' 'ZSCN00003' 'CELLC',
                        'MSGTB'   0   'X'  'X' 'ZSCN00003' 'MSGTB',
                        'DCFLG'   0   'X'  'X' 'ZSCN00003' 'DCFLG'.

  CLEAR : gv_pos.
  DO 13 TIMES.

    CLEAR : lv_field1.
    CONCATENATE 'GS_FCAT_DTL-FIELD_NAME' lv_count INTO lv_field1.
    ASSIGN (lv_field1) TO <ls_1>.

    CLEAR : lv_field2.
    CONCATENATE 'GS_FCAT_DTL-REFTABLE' lv_count INTO lv_field2.
    ASSIGN (lv_field2) TO <ls_2>.

    CLEAR : lv_field3.
    CONCATENATE 'GS_FCAT_DTL-FIELDNAME' lv_count INTO lv_field3.
    ASSIGN (lv_field3) TO <ls_3>.

    READ TABLE gt_pos_name INTO gs_pos_name WITH KEY pos = lv_count BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_field = gs_pos_name-name_c.

      IF gs_pos_name-name NE <ls_3>.
        <ls_3> = gs_pos_name-name.
      ENDIF.
    ENDIF.

    CLEAR : lt_tab[], lt_tab, lv_tabname, lv_fieldname.
    lv_tabname = <ls_2>.
    lv_fieldname = <ls_3>.
    IF lv_tabname IS NOT INITIAL AND lv_fieldname IS NOT INITIAL.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = lv_tabname
          fieldname      = lv_fieldname
        TABLES
          dfies_tab      = lt_tab
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      READ TABLE lt_tab INDEX 1.
    ENDIF.

    IF <ls_1> IS NOT INITIAL.
      ADD 1 TO gv_pos.
      _l_set_fcat: lv_field  gv_pos    'X' <ls_1>  <ls_2>  <ls_3>  lt_tab-datatype lt_tab-inttype  lt_tab-intlen.
      ADD 1 TO lv_count.
    ENDIF.

    UNASSIGN : <ls_1>, <ls_2>, <ls_3>.
  ENDDO.

ENDFORM. " FIELDCATALOG_MODIFY
*&---------------------------------------------------------------------*
*& Form evt_grid_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- E_OBJECT
*&---------------------------------------------------------------------*
FORM evt_grid_toolbar USING iv_name
                       CHANGING ct_toolbar TYPE ttb_button.

  DEFINE _l_add_toolbar.
    ls_add_toolbar-function    = &1.
    ls_add_toolbar-icon        = &2.
    ls_add_toolbar-quickinfo   = &3.
    ls_add_toolbar-butn_type   = &4.
    ls_add_toolbar-disabled    = &5.
    ls_add_toolbar-text        = &6.

    APPEND ls_add_toolbar TO ct_toolbar.
  END-OF-DEFINITION.

*-------------------------------------------------
*-- 추가 User Toolbar (그리드명으로 구분하여 사용)
*-------------------------------------------------
  DATA:ls_add_toolbar TYPE stb_button.

  CASE iv_name.
    WHEN 'GRID_DTL'.
      IF gv_bmode = space.
        _l_add_toolbar  'CHAN' icon_toggle_display_change 'Change<->Display' '' '' ''.
      ELSE.
        _l_add_toolbar  'CHAN' icon_display 'Change<->Display' '' '' ''.
        _l_add_toolbar  'INST' '' '신규 엔트리' '' '' '신규 엔트리'.
        _l_add_toolbar  'COPY' icon_system_copy '라인 복사' '' '' ''.
        _l_add_toolbar  'DELE' icon_delete_row '라인 삭제' '' '' ''.
        _l_add_toolbar  'SAVE' icon_system_save '저장' '' '' '저장'.
        _l_add_toolbar  'DEFI'  '' '필드 속성 정의' '' '' '필드 속성 정의'.
      ENDIF.
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
FORM evt_grid_f4 USING iv_fieldname
                  CHANGING ct_f4
                           cv_title .

  CASE iv_fieldname.
    WHEN 'CDVAL'.
*      _g_set_value : cv_title          'F4 Code Value'.
*
*      PERFORM f4_cdval_grid USING    iv_fieldname
*                            CHANGING ct_f4.
  ENDCASE.

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
    ls_sort-level     = &8.

  APPEND ls_sort TO ct_sort.
  end-OF-DEFINITION.

  DATA:ls_sort TYPE lvc_s_sort.

*  _l_append_sort: 1 'SPMON' 'X' '' '' '' '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- RT_F4
*&---------------------------------------------------------------------*
FORM alv_set_f4 CHANGING ct_field TYPE zcl_cn_alv_grid=>tt_field.

*----------------------------
* Set F4 Field..
*----------------------------
  ct_field = VALUE #( ( fieldname = 'SPMON' )
                      ( fieldname = 'CDVAL' ) ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_alv_grid_dtl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_alv_grid_dtl .
  DEFINE _l_add_field.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:ls_toolbtn TYPE zscn00004,
       lt_dftvl   TYPE zcl_cn_alv_grid=>tt_field.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  ls_toolbtn-btn_add    = 'X'.       "Add Row
*  ls_toolbtn-btn_madd   = 'X'.       "Multi Add Row
*  ls_toolbtn-btn_del    = 'X'.       "Delete Row
*  ls_toolbtn-btn_rec    = 'X'.       "Recovery Row
**  ls_toolbtn-btn_exld   = 'X'.       "Excel Download
**  ls_toolbtn-btn_exlu   = 'X'.       "Excel Upload
*  ls_toolbtn-mlti_lines = gv_mrow.   "Multi Row
**-- History Table..
*  ls_toolbtn-btn_hist   = 'X'.       "History Button
*  _g_set_value:ls_toolbtn-hist_tabnm 'ZTMM00002'.  " 그리드별 마스터 Table Name..

*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드(고정Value)
*--------------------------------------------------

  CREATE OBJECT grf_grid_dtl
    EXPORTING
      iv_name    = 'GRID_DTL'   "다수의 그리드일 경우 식별하기 위함..
      irf_parent = grf_body_dtl
      it_dftvl   = lt_dftvl
      is_toolbtn = ls_toolbtn.    "User 기본 Toolbar Button

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_dtl_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM evt_dtl_data_changed USING irf_data_changed
                                  TYPE REF TO cl_alv_changed_data_protocol.

  CONSTANTS : lc_fieldname(5) VALUE 'STATU'.

  FIELD-SYMBOLS : <ls_1>, <ls_2>.
  DATA : lv_field(30), lv_pos(2).

  LOOP AT irf_data_changed->mt_good_cells INTO DATA(ls_lvc_modi).

    CHECK ls_lvc_modi-fieldname NE lc_fieldname.

    ASSIGN COMPONENT ls_lvc_modi-fieldname OF STRUCTURE <gs_wa> TO <ls_1>.

    CALL METHOD irf_data_changed->get_cell_value
      EXPORTING
        i_row_id    = ls_lvc_modi-row_id
        i_fieldname = ls_lvc_modi-fieldname
      IMPORTING
        e_value     = <ls_1>.

*      READ TABLE gt_disp_dtl ASSIGNING FIELD-SYMBOL(<ls_disp>) INDEX ls_lvc_modi-row_id.
    READ TABLE gt_dtl_fcat INTO DATA(ls_fcat) WITH KEY fieldname = ls_lvc_modi-fieldname.
    lv_pos = ls_fcat-col_pos.

    READ TABLE gt_disp_dtl INTO DATA(ls_dtl) INDEX ls_lvc_modi-row_id.
    CONCATENATE 'LS_DTL-FIELD' lv_pos INTO lv_field.
    ASSIGN (lv_field) TO <ls_2>.
    IF <ls_1> NE <ls_2>.
      <ls_2> = <ls_1>.

      MODIFY gt_disp_dtl FROM ls_dtl INDEX ls_lvc_modi-row_id.
      IF sy-subrc NE 0.
        INSERT ls_dtl INTO TABLE gt_disp_dtl.
      ENDIF.

      gv_change = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_dtl_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM evt_dtl_double_click USING iv_row
                                 iv_column.

  CASE iv_column.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      READ TABLE gt_disp_dtl INTO DATA(ls_disp) INDEX iv_row.
      grf_grid_dtl->show_msgtb( it_msgtb = ls_disp-msgtb ).
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_DTL_changed_finished
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM evt_dtl_changed_finished USING it_good_cells TYPE lvc_t_modi.

  grf_grid_dtl->refresh_grid_display( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_arow
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM btn_add_row USING iv_ucomm.

  DATA : ls_dtl    TYPE ts_disp_dtl,
         lt_rows   TYPE  lvc_t_row,
         lt_row_no TYPE  lvc_t_roid.

  FIELD-SYMBOLS : <ls_f>.
  DATA : lv_field(30), lv_count(2), lv_fcount(2).
*
**--------------------
** Set Default Value.. (변동 되는 고정필드)
**--------------------

  LOOP AT gt_dtl_fcat INTO DATA(ls_fcat).
    IF ls_fcat-no_out IS INITIAL.
      DATA(lv_check) = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_check IS INITIAL.
    MESSAGE s000 WITH '필드 속성을 정의하세요'(m18) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING gs_tree TO ls_dtl.

  CASE iv_ucomm.
    WHEN 'COPY'.
      CALL METHOD grf_grid_dtl->get_selected_rows
        IMPORTING
          et_index_rows = lt_rows
          et_row_no     = lt_row_no.
      IF lt_rows[] IS NOT INITIAL.
        DESCRIBE TABLE lt_rows LINES DATA(lv_line).
        IF lv_line > 1.
          MESSAGE s000 WITH '한 개의 라인을 선택하세요'(m28) DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        READ TABLE lt_rows INTO DATA(ls_rows) INDEX 1.
        READ TABLE gt_disp_dtl INTO DATA(ls_disp_dtl) INDEX ls_rows-index.
        IF sy-subrc EQ 0.
          ls_dtl-field1 = ls_disp_dtl-field1.
          ls_dtl-field2 = ls_disp_dtl-field2.
          ls_dtl-field3 = ls_disp_dtl-field3.
          ls_dtl-field4 = ls_disp_dtl-field4.
          ls_dtl-field5 = ls_disp_dtl-field5.
          ls_dtl-field6 = ls_disp_dtl-field6.
          ls_dtl-field7 = ls_disp_dtl-field7.
          ls_dtl-field8 = ls_disp_dtl-field8.
          ls_dtl-field9 = ls_disp_dtl-field9.
          ls_dtl-field10 = ls_disp_dtl-field10.
          ls_dtl-field11 = ls_disp_dtl-field11.
          ls_dtl-field12 = ls_disp_dtl-field12.
          ls_dtl-field13 = ls_disp_dtl-field13.
        ENDIF.
      ELSE.

        MESSAGE s000 WITH '복사할 라인을 선택하세요'(m19) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
  ENDCASE.
*
  lv_count = 1.
  lv_fcount = 6.
  DO gv_pos TIMES.
    CONCATENATE 'LS_DTL-FIELD' lv_count INTO lv_field.
    ASSIGN (lv_field) TO <ls_f>.

    READ TABLE gt_dtl_fcat INTO ls_fcat INDEX lv_fcount.
    ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <gs_wa> TO <gs_field>.  "필드 내용을 동적으로 담기.
    <gs_field> = <ls_f>.
    ADD 1 TO lv_count.
    ADD 1 TO lv_fcount.
  ENDDO.

  APPEND <gs_wa> TO <gt_ta>.

**--------------------
**-- Insert Row..
**--------------------
  APPEND ls_dtl TO gt_disp_dtl.
*  SORT gt_disp_dtl BY field1 field2 field3 field4 field5
*                                 field6 field7 field8 field9 field10.

*
**--------------------
**-- Refresh..
**--------------------
  gv_change = 'X'.
  CALL METHOD grf_grid_dtl->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fieldcatalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcatalog .

* get fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZTMM00001'
    CHANGING
      ct_fieldcat      = gt_tree_field.

  SORT gt_tree_field BY scrtext_l.

* change fieldcatalog
  DATA: ls_fieldcatalog TYPE lvc_s_fcat.
  LOOP AT gt_tree_field INTO ls_fieldcatalog.
    ls_fieldcatalog-no_out = 'X'.
    ls_fieldcatalog-key    = ''.
    MODIFY gt_tree_field FROM ls_fieldcatalog.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_hierarchy_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- L_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING es_hierarchy_header TYPE treev_hhdr.

  es_hierarchy_header-heading = 'CBO Config'(c01).
  es_hierarchy_header-tooltip =  'Sub영역'(c02).
  es_hierarchy_header-width = 13.
  es_hierarchy_header-width_pix = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_hierarchy
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_hierarchy .

  DATA : ls_disp2 TYPE ts_disp.

* get data
  CLEAR : gt_disp[].
  PERFORM select_ztmm00001 TABLES gt_disp.

* add data to tree
  DATA: lv_main_key TYPE lvc_nkey,
        lv_midd_key TYPE lvc_nkey,
        lv_smal_key TYPE lvc_nkey.
  LOOP AT gt_disp INTO DATA(ls_disp).

    IF ls_disp-zmain_cat NE ls_disp2-zmain_cat.
*    ON CHANGE OF ls_disp-zmain_cat.
      PERFORM add_main_line USING  ls_disp
                                       ''
                              CHANGING lv_main_key.
*    ENDON.
    ENDIF.

    IF ls_disp-zmidd_cat NE ls_disp2-zmidd_cat.
*    ON CHANGE OF ls_disp-zmidd_cat.
      PERFORM add_midd_line USING    ls_disp
                                      lv_main_key
                              CHANGING lv_midd_key.
*    ENDON.
    ENDIF.
    PERFORM add_smal_line USING  ls_disp
                                    lv_midd_key
                            CHANGING lv_smal_key.
    ls_disp2 = ls_disp.
  ENDLOOP.

  CLEAR : ls_disp.
* expand first node initially
  IF gt_disp[] IS NOT INITIAL.
    CALL METHOD grf_tree->expand_node
      EXPORTING
        i_node_key = lv_main_key.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_main_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      --> P_
*&      <-- L_MAIN_KEY
*&---------------------------------------------------------------------*
FORM add_main_line USING is_disp TYPE ts_disp
                               iv_relat_key TYPE lvc_nkey
                     CHANGING  cv_node_key TYPE lvc_nkey.

  DATA: lv_node_text TYPE lvc_value,
        ls_disp      TYPE ztmm00001,
        ls_node      TYPE lvc_s_layn.

  ls_disp-zmain_cat = is_disp-zmain_cat.

* add node
  SELECT SINGLE ddtext INTO @DATA(lv_text)
    FROM dd07t
   WHERE domname = @gc_main
        AND domvalue_l = @p_main
        AND ddlanguage = @sy-langu.
  CONCATENATE is_disp-zmain_cat lv_text
                 INTO lv_node_text SEPARATED BY space.

  ls_node-n_image   = space.
  ls_node-exp_image = space.
  ls_node-isfolder = 'X'.

  CALL METHOD grf_tree->add_node
    EXPORTING
      i_relat_node_key = iv_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_outtab_line   = ls_disp
      is_node_layout   = ls_node
    IMPORTING
      e_new_node_key   = cv_node_key.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_midd_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      --> L_MAIN_KEY
*&      <-- L_MIDD_KEY
*&---------------------------------------------------------------------*
FORM add_midd_line USING is_disp TYPE ts_disp
                               iv_relat_key TYPE lvc_nkey
                     CHANGING  cv_node_key TYPE lvc_nkey.

  DATA: ls_node      TYPE lvc_s_layn,
        lv_node_text TYPE lvc_value,
        ls_disp      TYPE ztmm00001.

  ls_disp-zmain_cat = is_disp-zmain_cat.
  ls_disp-zmidd_cat = is_disp-zmidd_cat.
  ls_disp-zmidd_cat_dec = is_disp-zmidd_cat_dec.

*  layout.
  ls_node-isfolder = 'X'.

* add node
  CONCATENATE is_disp-zmidd_cat is_disp-zmidd_cat_dec
                 INTO lv_node_text SEPARATED BY space.

  CALL METHOD grf_tree->add_node
    EXPORTING
      i_relat_node_key = iv_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = lv_node_text
      is_outtab_line   = ls_disp
      is_node_layout   = ls_node
    IMPORTING
      e_new_node_key   = cv_node_key.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_smal_line
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      --> L_MIDD_KEY
*&      <-- L_SMAL_KEY
*&---------------------------------------------------------------------*
FORM add_smal_line USING is_disp TYPE ts_disp
                               iv_relat_key TYPE lvc_nkey
                     CHANGING  cv_node_key TYPE lvc_nkey.

  DATA: lv_node_text TYPE lvc_value,
        ls_node      TYPE lvc_s_layn,
        ls_disp      TYPE ztmm00001.

  CHECK  is_disp-zsmal_cat IS NOT INITIAL.
  ls_disp-zmain_cat = is_disp-zmain_cat.
  ls_disp-zmidd_cat = is_disp-zmidd_cat.
  ls_disp-zsmal_cat = is_disp-zsmal_cat.
  ls_disp-zmidd_cat_dec = is_disp-zmidd_cat_dec.
  ls_disp-zsmal_cat_dec = is_disp-zsmal_cat_dec.

  CONCATENATE is_disp-zsmal_cat is_disp-zsmal_cat_dec
                 INTO lv_node_text SEPARATED BY space.

  CALL METHOD grf_tree->add_node
    EXPORTING
      i_relat_node_key = iv_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ls_disp
      i_node_text      = lv_node_text
      is_node_layout   = ls_node
    IMPORTING
      e_new_node_key   = cv_node_key.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form change_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_toolbar .

  CALL METHOD grf_tree->get_toolbar_object
    IMPORTING
      er_toolbar = grf_tree_toolbar.

  CHECK NOT grf_tree_toolbar IS INITIAL. "could happen if you do not use the
  "standard toolbar

* add seperator to toolbar
*  CALL METHOD grf_tree_toolbar->add_button
*    EXPORTING
*      fcode     = ''
*      icon      = ''
*      butn_type = cntb_btype_sep.

* add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD grf_tree_toolbar->add_button
    EXPORTING
      fcode     = 'CHAN'
      icon      = '@3I@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = TEXT-f04.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form register_events
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM register_events .

  DATA: lt_events               TYPE cntl_simple_events,
        lrf_tool_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
        lrf_event_receiver      TYPE REF TO lcl_tree_event_receiver.

  CALL METHOD grf_tree->get_registered_events
    IMPORTING
      events = lt_events.

* Tree events:
  SORT lt_events BY eventid.
  PERFORM append_event TABLES lt_events
           USING :cl_gui_column_tree=>eventid_node_context_menu_req,
                      cl_gui_column_tree=>eventid_node_double_click.
*  cl_gui_column_tree=>eventid_header_context_men_req,
*  cl_gui_column_tree=>eventid_expand_no_children.
*  cl_gui_column_tree=>eventid_checkbox_change.
*  cl_gui_column_tree=>eventid_item_context_menu_req.
*  cl_gui_column_tree=>eventid_header_click.

* register events on frontend
  CALL METHOD grf_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  CREATE OBJECT lrf_event_receiver.
  SET HANDLER lrf_event_receiver->handle_node_cm_req FOR grf_tree.
  SET HANDLER lrf_event_receiver->handle_node_cm_sel FOR grf_tree.
  SET HANDLER lrf_event_receiver->handle_node_click FOR grf_tree.

  CREATE OBJECT lrf_tool_event_receiver.
  SET HANDLER lrf_tool_event_receiver->on_function_selected FOR grf_tree_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form excluding_toolbar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_TREE_TOOLBAR
*&---------------------------------------------------------------------*
FORM excluding_toolbar TABLES ct_tree_toolbar.

  PERFORM append_exclude_functions
        TABLES  ct_tree_toolbar
         USING :
  cl_gui_alv_tree=>mc_fc_calculate,
  cl_gui_alv_tree=>mc_fc_print_back,
  cl_gui_alv_tree=>mc_fc_current_variant.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form append_exclude_functions
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PT_TREE_TOOLBAR
*&      --> CL_GUI_ALV_GRID=>MC_FC_SUBTOT
*&---------------------------------------------------------------------*
FORM append_exclude_functions TABLES ct_table
                                                    USING iv_value TYPE any.

  DATA : ls_exclude TYPE ui_func.

  ls_exclude = iv_value.
  APPEND ls_exclude TO ct_table.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form append_Event
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EVENTS
*&      --> CL_GUI_COLUMN_TREE=>EVENTID_HE
*&---------------------------------------------------------------------*
FORM append_event TABLES ct_events TYPE cntl_simple_events
                   USING    iv_event.

  DATA:  ls_event   TYPE cntl_simple_event.

  READ TABLE ct_events WITH KEY eventid = iv_event BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    ls_event-eventid = iv_event.
    APPEND ls_event TO ct_events.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form detail_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_TREE
*&---------------------------------------------------------------------*
FORM detail_screen USING is_tree.

*   세부데이터 조회.
  CLEAR : gt_disp_dtl[].
  PERFORM select_detail USING is_tree.

*  필드카탈로그 검색.
  CLEAR : gs_fcat_dtl.
  PERFORM select_detail_fcat USING is_tree.

*   필드카탈로그 구성.
  CLEAR : gt_dtl_fcat[].
  PERFORM alv_grid_fcat_modify.

*****************************
*  ALV CREATE
  PERFORM detail_alv.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form make_top_txt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_DOCU
*&---------------------------------------------------------------------*
FORM make_top_txt USING iv_dd TYPE REF TO cl_dd_document.

  DATA: lv_txt TYPE sdydo_text_element.

  SELECT SINGLE ddtext INTO @DATA(lv_main)
    FROM dd07t
   WHERE domname = @gc_main
        AND domvalue_l = @p_main
        AND ddlanguage = @sy-langu.
  CONCATENATE  p_main lv_main  INTO lv_txt SEPARATED BY space.

  PERFORM set_dd_table_layo USING iv_dd go_ta '' go_col1 go_col2.
  PERFORM set_dd_table_txt USING go_ta go_col1 go_col2 '대분류' lv_txt.

  CALL METHOD iv_dd->merge_document.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_dd_table_layo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_DD
*&      --> GO_TA
*&      --> P_
*&      --> GO_COL1
*&      --> GO_COL2
*&---------------------------------------------------------------------*
FORM set_dd_table_layo USING iv_dd TYPE REF TO cl_dd_document
                                iv_tab TYPE REF TO cl_dd_table_element
                                iv_width
                                io_col1 TYPE REF TO cl_dd_area
                                io_col2 TYPE REF TO cl_dd_area.

  DATA: lv_width TYPE sdydo_value.

  IF iv_width = ''.
    lv_width = '120'.
  ELSE.
    lv_width = iv_width.
  ENDIF.

  CALL METHOD iv_dd->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = iv_tab.

  CALL METHOD iv_tab->add_column
    EXPORTING
      width  = iv_width
    IMPORTING
      column = io_col1.

  CALL METHOD iv_tab->add_column
    IMPORTING
      column = io_col2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_dd_table_txt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_TA
*&      --> GO_COL1
*&      --> GO_COL2
*&      --> P_
*&      --> LV_TXT
*&---------------------------------------------------------------------*
FORM set_dd_table_txt USING io_tab TYPE REF TO cl_dd_table_element
                              io_col1 TYPE REF TO cl_dd_area
                              io_col2 TYPE REF TO cl_dd_area
                              iv_head iv_txt.

  DATA : lv_text     TYPE sdydo_text_element.

  lv_text = iv_head.
  CALL METHOD io_col1->add_text
    EXPORTING
      text         = lv_text
      sap_fontsize = cl_dd_area=>list_normal.

  lv_text = iv_txt.
  CALL METHOD io_col2->add_text
    EXPORTING
      text         = lv_text
      sap_fontsize = cl_dd_area=>list_normal.

  CALL METHOD io_tab->new_row.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fcat_define
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DE_FIELD
*&---------------------------------------------------------------------*
FORM fcat_define TABLES ct_de_field STRUCTURE lvc_s_fcat.

  DATA : ls_field TYPE lvc_s_fcat.

  DEFINE _l_set_fcat.
    CLEAR : ls_field.
    ls_field-fieldname    = &1.
    ls_field-col_pos      = &2.
    ls_field-key             = &3.

   CASE ls_field-col_pos.
     WHEN 1.
     ls_field-edit             = space.
       WHEN OTHERS.
        IF gt_disp_dtl[] IS NOT INITIAL.
          IF ls_field-col_pos EQ 2.
         ls_field-edit             = 'X'.
          ELSE.
         ls_field-edit             = space.
          ENDIF.
        ELSE.
         ls_field-edit             = 'X'.
        ENDIF.
    ENDCASE.

    ls_field-coltext        = &4.
    ls_field-just             = &5.
*    ls_field-f4availabl     = &7.
    ls_field-ref_table = &6.
    ls_field-ref_field = &7.
    ls_field-outputlen      = &8.
    APPEND ls_field TO ct_de_field.
  END-OF-DEFINITION.

  _l_set_fcat: 'FIELD'             1   'X'   '코드'            'C'  '' ''          '5',
                    'FIELD_NAME'   2   ''     '필드명'        'C'  '' ''          '15',
                    'REFTABLE'      3   ''      '참조테이블'  'C'  'DD08L' 'TABNAME'       '10',
                    'FIELDNAME'     4   ''     '참조필드'     'C'  'DD08L' 'FIELDNAME'     '15'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_regist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM event_regist .

  DATA : lrf_de_event TYPE REF TO lcl_de_event.

  CALL METHOD grf_de_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.      "Enter Key만을 수정으로 간주한다.

  CALL METHOD grf_de_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.  "Enter Key + F4를 수정으로 간주한다


  CREATE OBJECT lrf_de_event.
  SET HANDLER lrf_de_event->handle_data_changed FOR grf_de_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ER_DATA_CHANGED
*&      --> E_UCOMM
*&---------------------------------------------------------------------*
FORM event_data USING iv_data_changed TYPE REF TO cl_alv_changed_data_protocol
                          iv_ucomm.

  DATA  : ls_mod_cells TYPE lvc_s_modi.

  DATA: lv_tabname   TYPE ddobjname,
        lv_fieldname TYPE  dfies-fieldname,
        lt_tab       LIKE STANDARD TABLE OF dfies WITH HEADER LINE.

  CLEAR : gv_err.

  DEFINE _l_cell_value.
    CALL METHOD iv_data_changed->get_cell_value
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = &1
      IMPORTING
        e_value     = &2.
  end-OF-DEFINITION.

  DEFINE _l_modify_cell.
    CALL METHOD iv_data_changed->modify_cell
      EXPORTING
        i_row_id    = ls_mod_cells-row_id
        i_fieldname = &1
        i_value     = &2.
  end-OF-DEFINITION.


  LOOP AT iv_data_changed->mt_good_cells INTO ls_mod_cells.

    CASE ls_mod_cells-fieldname.

        DATA  : lv_field_name TYPE char30.
      WHEN 'FIELD_NAME'.
        _l_cell_value : 'FIELD_NAME'  lv_field_name.
        IF gt_disp_dtl[] IS NOT INITIAL.
          IF lv_field_name IS INITIAL.
            MESSAGE s000 WITH '필드명을  삭제할 수 없습니다.'(m20) DISPLAY LIKE 'E'.
            READ TABLE gt_de_fcat INTO DATA(ls_fcat) INDEX ls_mod_cells-row_id.
            _l_modify_cell : 'FIELD_NAME' ls_fcat-field_name.
            gv_err = abap_true.
          ENDIF.
        ENDIF.

      WHEN 'REFTABLE'.

        CHECK ls_mod_cells-value IS NOT INITIAL.

*        SELECT SINGLE tabname
*          INTO @DATA(lv_name)
*          FROM dd02l
*         WHERE tabname = @ls_mod_cells-value.

        lv_tabname =   ls_mod_cells-value.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = lv_tabname
          TABLES
            dfies_tab      = lt_tab
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          _l_modify_cell : 'REFTABLE' space.
          MESSAGE s000 WITH '존재하지 않는 테이블이 있습니다.'(m10) DISPLAY LIKE 'E'.
          gv_err = abap_true.
        ELSE.
          _l_modify_cell : 'REFTABLE' ls_mod_cells-value.
        ENDIF.


      WHEN 'FIELDNAME'.

        CHECK ls_mod_cells-value IS NOT INITIAL.


*           테이블명 가져오기
        CLEAR : lv_tabname.
        _l_cell_value : 'REFTABLE'  lv_tabname.

        IF lv_tabname IS INITIAL.
          _l_modify_cell : 'FIELDNAME' space.
          MESSAGE s000 WITH '테이블을 입력하세요.'(m21)  DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*        SELECT SINGLE fieldname
*          INTO @DATA(lv_field)
*          FROM ddftx
*        WHERE tabname = @lv_tabname
*           AND fieldname = @ls_mod_cells-value
*           AND ddlanguage = @sy-langu.

        lv_fieldname =   ls_mod_cells-value.
        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = lv_tabname
            fieldname      = lv_fieldname
          TABLES
            dfies_tab      = lt_tab
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          _l_modify_cell : 'FIELDNAME' space.
          MESSAGE s000 WITH '테이블에 존재하지 않는 필드가 있습니다.'(m11) DISPLAY LIKE 'E'.
          gv_err = abap_true.
        ELSE.
          _l_modify_cell : 'FIELDNAME' ls_mod_cells-value.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETAIL_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM detail_alv .

  DATA: lo_dtrf TYPE REF TO data.
  DATA: lo_struc TYPE REF TO data.
  DATA :
    lt_alvf_cat TYPE TABLE OF lvc_s_fcat,
    ls_alv_cat  LIKE LINE OF lt_alvf_cat.

  FIELD-SYMBOLS : <ls_f>.
  DATA : lv_field(30), lv_count(2), lv_fcount(2).


  SORT gt_dtl_fcat BY col_pos.

  LOOP AT gt_dtl_fcat INTO DATA(ls_fcat).
    CLEAR : ls_alv_cat.
    ls_alv_cat-fieldname = ls_fcat-fieldname.
    ls_alv_cat-ref_table = ls_fcat-ref_table.
    ls_alv_cat-ref_field = ls_fcat-ref_field.
    ls_alv_cat-no_out = ls_fcat-no_out.
    APPEND ls_alv_cat TO lt_alvf_cat.
  ENDLOOP.

*Using function CREATE_DYNAMIC_TABLE to create the internal table
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_alvf_cat
    IMPORTING
      ep_table        = lo_dtrf.

  ASSIGN lo_dtrf->* TO <gt_ta>.
  CREATE DATA lo_struc LIKE LINE OF <gt_ta>.

  ASSIGN lo_struc->* TO <gs_wa>. "fs에 동적 스트럭쳐 선언

  LOOP AT gt_disp_dtl INTO DATA(ls_dtl).

    lv_count = 1.
    lv_fcount = 6.
    DO gv_pos TIMES.
      CONCATENATE 'LS_DTL-FIELD' lv_count INTO lv_field.
      ASSIGN (lv_field) TO <ls_f>.

      READ TABLE gt_dtl_fcat INTO ls_fcat INDEX lv_fcount.
      ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <gs_wa> TO <gs_field>.  "필드 내용을 동적으로 담기.
      <gs_field> = <ls_f>.
      ADD 1 TO lv_count.
      ADD 1 TO lv_fcount.
    ENDDO.

    APPEND <gs_wa> TO <gt_ta>.

  ENDLOOP.

*-- Detail Grid
  grf_grid_dtl->set_grid(  EXPORTING it_fcat = gt_dtl_fcat   "Dynamic Alv일경우 사용
                           CHANGING  ct_data = <gt_ta> ). "gt_disp_dtl .

  READ TABLE  gt_dtl_fcat WITH KEY no_out = space TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH '필드 속성을 정의하세요'(m18) DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
