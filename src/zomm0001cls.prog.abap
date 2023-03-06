*&---------------------------------------------------------------------*
*& Include          ZOMM0001CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class LCL_CUST_ALV_GRID
*&---------------------------------------------------------------------*
CLASS lcl_cust_alv_grid DEFINITION INHERITING FROM zcl_cn_alv_grid.

  PUBLIC SECTION.
    METHODS:
*---------------------------
* Event Area
*---------------------------
      evt_on_data_changed       REDEFINITION,
      evt_user_command           REDEFINITION,
      evt_double_click                REDEFINITION,
      evt_on_toolbar                  REDEFINITION,
      evt_on_f4                         REDEFINITION,
      evt_data_changed_finished REDEFINITION.


  PROTECTED SECTION.
    METHODS :
*---------------------------
* Alv Grid Area
*---------------------------
      on_save_variant            REDEFINITION,
      on_exclud_std_toolbar   REDEFINITION,
      on_set_drop_down         REDEFINITION,
      on_set_event                 REDEFINITION,
      on_set_fcat                   REDEFINITION,
      on_set_layout                REDEFINITION,
      on_set_line_style           REDEFINITION,
      on_set_f4                      REDEFINITION,
      on_set_sort                   REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
CLASS lcl_cust_alv_grid IMPLEMENTATION.
************************************************************************
* Event Area
************************************************************************
*=======================================================================
* METHOD evt_on_data_changed.
* Data Chagned Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_data_chaged
*=======================================================================
  METHOD evt_on_data_changed.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
      WHEN 'GRID_DTL'.
        PERFORM evt_dtl_data_changed USING irf_data_changed.
      WHEN OTHERS.
    ENDCASE.

*    CALL METHOD cl_gui_cfw=>flush
*      EXCEPTIONS
*        cntl_system_error = 1
*        cntl_error        = 2.

  ENDMETHOD.
*=======================================================================
* METHOD evt_user_command.
* Alv User BUtton Event
* 각 그리드별로 구분하여 사용
* Naming Rule BTN_xxx(E_UCOMM명)
*=======================================================================
  METHOD evt_user_command.
*-----------------------------------------------------------------------
* 공통에서 기본사용되는(Change,add row, del_row, recovery, excel..)
*-----------------------------------------------------------------------

    TRY.
        CASE me->m_name.
          WHEN 'GRID_DTL'.
*------------------------------------------------------------
* Alv Child (Rifght)
*------------------------------------------------------------
            CASE e_ucomm.
              WHEN 'CHAN'.
                IF gv_bmode = space.
                  gv_bmode = 'X'.
                  grf_grid_dtl->set_edit_mode( 'X' ).
                ELSE.
                  gv_bmode = space.
                  grf_grid_dtl->set_edit_mode( '' ).
                ENDIF.

                CALL METHOD grf_grid_dtl->refresh_grid_display( ).

              WHEN 'INST' OR 'COPY'.
                PERFORM btn_add_row USING e_ucomm.

              WHEN 'DELE'.
                PERFORM btn_del_row.

              WHEN 'SAVE'.

                grf_grid_dtl->check_changed_data(  ).
                PERFORM save_ztmm00002.
                CALL METHOD grf_grid_dtl->refresh_grid_display( ).

              WHEN 'DEFI'.
                CLEAR : gt_de_fcat[].
                PERFORM select_define_field TABLES gt_de_fcat.
                IF gt_de_fcat[] IS NOT INITIAL.
                  CALL SCREEN 0300 STARTING AT 40 3
                                                ENDING    AT 92 12.

                  grf_grid_dtl->set_edit_mode( 'X' ).
                  CALL METHOD grf_grid_dtl->refresh_grid_display( ).
                ENDIF.

              WHEN OTHERS.
                CALL METHOD super->evt_user_command
                  EXPORTING
                    e_ucomm = e_ucomm
                    sender  = me.
            ENDCASE.
          WHEN OTHERS.
        ENDCASE.

      CATCH cx_root INTO gs_root.
        gs_msg = gs_root->get_text( ).
        MESSAGE s000 WITH gs_msg DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
*=======================================================================
* METHOD evt_on_toolbar.
* Alv Grid Button
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_toolbar
*=======================================================================
  METHOD evt_on_toolbar.

    CASE me->m_name.
      WHEN 'GRID_DTL'.
        PERFORM evt_grid_toolbar USING    me->m_name
                                 CHANGING ct_add_toolbar.

    ENDCASE.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_on_f4.
* Alv Field F4 Field 및 Text 세팅
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_F4
* 단, F4 테이블은 METHOD 안에서 선언하고 Perform문 안에서도 동일한 Type으로 선언하여 데이타를 Move
*===================================================================================================
  METHOD evt_on_f4.
*--------------------------------------
* F4로 사용하고자 하는 Type 선언
*--------------------------------------
    TYPES:BEGIN OF lty_f4,          "Perform 안에서도 동일하게 선언
            cdval TYPE char2,
            text  TYPE ltext,
          END OF lty_f4.

    FIELD-SYMBOLS:<lt_f4_list> TYPE table.

*----------------------------------------------------------
* Month 달력은 예외적이여서 별도 필드명으로 구분하여 처리
*----------------------------------------------------------
    CASE iv_fieldname.
      WHEN 'SPMON'.
        _g_set_value : ev_month_display  'X'.
        RETURN.
    ENDCASE.

*--------------------------------------
* Field 별로 Type을 지정하여 사용
*--------------------------------------
    CASE iv_fieldname.
      WHEN 'CDVAL'.
        CREATE DATA ao_f4 TYPE TABLE OF lty_f4.
        ASSIGN ao_f4->* TO <lt_f4_list>.
    ENDCASE.

*--------------------------------------
* 그리드 별로 구분하여 처리
*--------------------------------------
    CASE me->m_name.
      WHEN 'ALV_GRID'.

*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_grid_f4 USING    iv_fieldname
                            CHANGING <lt_f4_list>
                                     cv_title.
    ENDCASE.

    CHECK ao_f4 IS BOUND.
    eo_f4_list = ao_f4.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_double_click.
* Double Click Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_double_click
*===================================================================================================
  METHOD evt_double_click.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
      WHEN 'GRID_DTL'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_dtl_double_click USING e_row-index
                                            e_column.
    ENDCASE.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_data_changed_finished.
* Data Changed Finished Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_finished
*===================================================================================================
  METHOD evt_data_changed_finished.

    CHECK e_modified IS NOT INITIAL.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
      WHEN 'GRID_DTL'.
        PERFORM evt_dtl_changed_finished USING et_good_cells.
    ENDCASE.

  ENDMETHOD.
************************************************************************************************
* ALV Grid 내에서 User Define Area
*
************************************************************************************************
*===============================================================================================
*  METHOD on_save_variant.
* Save Variant User Area
* Variant Maintenace 기능 세팅
*===============================================================================================
  METHOD on_save_variant.

    CASE iv_name.
      WHEN 'ALV_GRID' OR 'DTL_GIRD'.
        cv_default = 'X'.
        cv_save    = 'A'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_exclud_std_toolbar.
* ON_EXCLUD_STD_TOOLBAR User Area
* Default Standard Toolbar 에서 제거 하고자 하는경우
* Ex)Sum,Total은 금액이나 수량이 없는 경우는 필요가 없으므로 이 영역에서 제거한다
*===============================================================================================
  METHOD on_exclud_std_toolbar.

    CASE iv_name.
      WHEN 'ALV_GRID'.
*        ct_ex_toolbar = VALUE #( ( cl_gui_alv_grid=>mc_fc_average )
*                                 ( cl_gui_alv_grid=>mc_fc_sum     )
*                                 ( cl_gui_alv_grid=>mc_fc_subtot  ) ).
      WHEN 'GRID_DTL'.
*        ct_ex_toolbar = VALUE #( ( cl_gui_alv_grid=>mc_fc_average )
*                                 ( cl_gui_alv_grid=>mc_fc_sum     )
*                                 ( cl_gui_alv_grid=>mc_fc_subtot  ) ).
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_drop_down.
* Drop DOwn List User Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_drop_down
*===============================================================================================
  METHOD on_set_drop_down.

    CASE iv_name.
      WHEN 'ALV_GRID'.
      WHEN 'GRID_DTL'.
*        PERFORM avl_grid_set_drop_down CHANGING ct_drop.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_event.
* Event Handler Setting Area
* 각 그리드별로 구분하여 사용
*===============================================================================================
  METHOD on_set_event.

    CALL METHOD me->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CASE iv_name.
      WHEN 'ALV_GRID'.
*        SET HANDLER :
*                      me->evt_toolbar                 FOR me,
*                      me->evt_data_changed            FOR me,
**                      me->evt_top_of_page             FOR me,
*                      me->evt_double_click            FOR me,
*                      me->evt_user_command            FOR me,
*                      me->evt_data_changed_finished   FOR me,
*                      me->evt_onf4                    FOR me.
      WHEN 'GRID_DTL'.
        SET HANDLER :
                      me->evt_toolbar                 FOR me,
                      me->evt_data_changed            FOR me,
                      me->evt_data_changed_finished   FOR me,
                      me->evt_double_click            FOR me,
                      me->evt_user_command            FOR me,
                      me->evt_onf4                    FOR me.
    ENDCASE.
  ENDMETHOD.
*===============================================================================================
* METHOD on_set_fcat .
* Field Catalog User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_fcat_modify
*===============================================================================================
  METHOD on_set_fcat .

*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**--------------------------
** Modify Field Catalog
** 저장전 중복체크를 위한 Table key 세팅을 만드시!!
**--------------------------
*        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
*      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_fcat_modify CHANGING ct_fcat.
*    ENDCASE.
  ENDMETHOD.
*===============================================================================================
* METHOD on_set_layout.
* Layout User Setting Area
* Defalut Lyaout에서 변경이 필요한경우
* 각 그리드별로 구분하여 사용
*===============================================================================================
  METHOD on_set_layout.

    CASE iv_name.
      WHEN 'ALV_GRID'.
* cs_layout-xxx = 'xx'.
      WHEN 'GRID_DTL'.
    ENDCASE.
  ENDMETHOD.
*===============================================================================================
* METHOD on_set_line_style.
* Line Style User Setting Area
* Field Color 및 Edit& Disabled 세팅
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_line_style
*===============================================================================================
  METHOD on_set_line_style.

    CASE iv_name.
      WHEN 'ALV_GRID'.
*        PERFORM alv_grid_set_line_style.
      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_set_line_style.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_f4.
* F4를 사용하기 위해  필드명을 등록하기 위한  User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_F4
*===============================================================================================
  METHOD on_set_f4.

    CASE iv_name.
      WHEN 'ALV_GRID'.
        PERFORM alv_set_f4 CHANGING ct_field.
      WHEN 'GRID_DTL'.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_sort.
* Sort 필드 User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_sort
*===============================================================================================
  METHOD on_set_sort.

    CASE iv_name.
      WHEN 'ALV_GRID'.
        PERFORM alv_set_sort CHANGING ct_sort .
      WHEN 'GRID_DTL'.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
* mouse button on a node.
    METHODS: handle_node_cm_req
      FOR EVENT node_context_menu_request OF cl_gui_alv_tree
      IMPORTING node_key menu.

* build up context menu.
    METHODS: handle_node_cm_sel
      FOR EVENT node_context_menu_selected OF cl_gui_alv_tree
      IMPORTING node_key fcode sender.

    METHODS: handle_node_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key.

ENDCLASS.

CLASS lcl_tree_event_receiver IMPLEMENTATION.

  METHOD handle_node_cm_req.

* In this case the standard menu is cleared.
    CALL METHOD menu->clear.

*     메뉴 활성화 여부 체크
    IF gv_tmode NE 1.
      MESSAGE s000 WITH '조회 모드입니다.'(m01) DISPLAY LIKE 'S'.
      EXIT.
    ENDIF.

    DATA: lt_selected_nodes TYPE lvc_t_nkey,
          lv_selected_node  TYPE lvc_nkey.

    CALL METHOD grf_tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected_nodes.
    READ TABLE lt_selected_nodes INTO lv_selected_node INDEX 1.
    IF sy-subrc EQ 0.

      READ TABLE gt_tree INTO DATA(ls_tree) INDEX lv_selected_node.

      IF ls_tree-zsmal_cat IS INITIAL.
        CALL METHOD menu->add_function
          EXPORTING
            fcode = 'CRET'
            text  = TEXT-f05.
      ENDIF.

      CHECK lv_selected_node NE 1.
      CALL METHOD menu->add_function
        EXPORTING
          fcode = 'MODI'
          text  = TEXT-f06.
      CALL METHOD menu->add_function
        EXPORTING
          fcode = 'DELE'
          text  = TEXT-f07.

    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.


  METHOD handle_node_cm_sel.
    DATA: lt_selected_nodes TYPE lvc_t_nkey,
          lv_selected_node  TYPE lvc_nkey.

    CASE fcode.
      WHEN 'CRET'.
        gv_ncheck = 'C'.
        CLEAR : gs_tree_add.
        CALL SCREEN 0200 STARTING AT 30 3
                                      ENDING AT 84 4.

      WHEN 'MODI'.
        gv_ncheck = 'M'.
        CLEAR : gs_tree_add.
        CALL SCREEN 0200 STARTING AT 30 3
                                      ENDING AT 84 4.

      WHEN 'DELE'.

*        DATA: lt_selected_nodes TYPE lvc_t_nkey.
        CALL METHOD grf_tree->get_selected_nodes
          CHANGING
            ct_selected_nodes = lt_selected_nodes.
        READ TABLE lt_selected_nodes INTO lv_selected_node INDEX 1.

        CHECK grf_grid_dtl->pop_to_msg( iv_type  = 'A'
                                    iv_title = zcl_cn_alv_grid=>ac_msg_title3 "삭제확인
                                    iv_text1 = zcl_cn_alv_grid=>ac_msg_del1  "삭제하시겠습니까?
                                    iv_text2 = zcl_cn_alv_grid=>ac_msg_del2 ) EQ abap_true. " YES

        READ TABLE gt_tree INTO DATA(ls_tree) INDEX lv_selected_node.

        IF ls_tree-zsmal_cat IS NOT INITIAL.  "소분류

*          데이터 확인
          SELECT SINGLE zsmal_cat
            INTO @DATA(lv_zsmal_cat)
            FROM ztmm00002
           WHERE zmain_cat = @ls_tree-zmain_cat
              AND zmidd_cat = @ls_tree-zmidd_cat
              AND zsmal_cat = @ls_tree-zsmal_cat.
          IF sy-subrc EQ 0.
            MESSAGE s000 WITH '관련 데이터를 먼저 삭제하세요'(m02) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          DELETE FROM ztmm00001 WHERE zmain_cat = @ls_tree-zmain_cat
                                                      AND zmidd_cat = @ls_tree-zmidd_cat
                                                      AND zsmal_cat = @ls_tree-zsmal_cat.
          IF sy-subrc EQ 0.
            COMMIT WORK.
            MESSAGE s000 WITH '소분류가 삭제되었습니다.'(m04).
          ENDIF.

        ELSE.  "중분류

*          데이터 확인
          SELECT SINGLE zsmal_cat
            INTO @lv_zsmal_cat
            FROM ztmm00002
           WHERE zmain_cat = @ls_tree-zmain_cat
              AND zmidd_cat = @ls_tree-zmidd_cat
              AND zsmal_cat = @space.
          IF sy-subrc EQ 0.
            MESSAGE s000 WITH '관련 데이터를 먼저 삭제하세요'(m02)  DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.


          SELECT SINGLE zsmal_cat
            INTO @lv_zsmal_cat
            FROM ztmm00001
           WHERE zmain_cat = @ls_tree-zmain_cat
              AND zmidd_cat = @ls_tree-zmidd_cat
              AND zsmal_cat NE @space.
          IF sy-subrc EQ 0.
            MESSAGE s000 WITH '관련 데이터를 먼저 삭제하세요'(m02)  DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          DELETE FROM ztmm00001 WHERE zmain_cat = @ls_tree-zmain_cat
                                                      AND zmidd_cat = @ls_tree-zmidd_cat
                                                      AND zsmal_cat = @ls_tree-zsmal_cat.
          IF sy-subrc EQ 0.
            COMMIT WORK.
            MESSAGE s000 WITH '중분류가 삭제되었습니다.'(m03) .
          ENDIF.

        ENDIF.

*    세부사항 필드정보 초기화.
        IF grf_de_con IS NOT INITIAL.
          CLEAR : gv_sel_node.
          CALL METHOD grf_de_grid->free.
          CALL METHOD grf_de_con->free.
          CLEAR grf_de_con.
          CLEAR grf_de_grid.
          CLEAR : gt_de_field[].
          CLEAR : gt_dtl_fcat[].
        ENDIF.

*      노드 삭제처리.
        CALL METHOD sender->delete_subtree
          EXPORTING
            i_node_key = node_key.

*        선택한 노드가 삭제되면 선택취소.
        IF node_key = gv_sel_node.
          CLEAR : gv_sel_node.
        ENDIF.

        CLEAR : ls_tree.
        ls_tree-zmain_cat = 'D'.
        INSERT ls_tree INTO gt_tree INDEX node_key.

        CALL METHOD sender->frontend_update.

    ENDCASE.

  ENDMETHOD.


  METHOD: handle_node_click.

    DATA : lv_sel_node TYPE lvc_nkey.
    CONSTANTS : lc_display(4) VALUE '@10@'.

    DATA : ls_node_layout TYPE lvc_s_lacn,
           ls_outtab_line.

*    선택한 노트 찾기.
    CALL METHOD grf_tree->get_last_child
      EXPORTING
        i_node_key       = node_key
      IMPORTING
        e_child_node_key = lv_sel_node.

    IF lv_sel_node IS NOT INITIAL.
      MESSAGE s000 WITH '세부항목을 클릭하세요.'(m05) DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.


    IF gv_change IS NOT INITIAL.
      DATA : lv_msg1 TYPE string, lv_msg2 TYPE string.
      lv_msg1 = zcl_cn_alv_grid=>ac_msg_exit && '..' && zcl_cn_alv_grid=>ac_msg_save && '.'.
      lv_msg2 = TEXT-m27.
      CHECK grf_grid_dtl->pop_to_msg( iv_type  = 'A'
                                  iv_title = zcl_cn_alv_grid=>ac_msg_exit  "변경정보 확인
                                  iv_text1 = lv_msg1 "계속하시겠습니까?
                                  iv_text2 = lv_msg2 ) EQ abap_true. " YES
      PERFORM save_ztmm00002.
      CLEAR : gv_change.
    ENDIF.

    READ TABLE gt_tree INTO DATA(ls_tree) INDEX node_key.
    CHECK ls_tree-zsmal_cat IS NOT INITIAL.

    "이전 노드 아이콘 초기화
    IF gv_sel_node IS NOT INITIAL.
      CLEAR : ls_node_layout.
      ls_node_layout-style = space.
      ls_node_layout-u_style = 'X'.
      ls_node_layout-n_image = space.
      ls_node_layout-u_n_image = 'X'.
      CALL METHOD grf_tree->change_node
        EXPORTING
          i_node_key     = gv_sel_node
          i_outtab_line  = ls_outtab_line
          is_node_layout = ls_node_layout.

    ENDIF.

*    선택한 노트 저장.
    gv_sel_node = node_key.

*    선택 노트 조회 처리.
    CLEAR : ls_node_layout.

    ls_node_layout-style = cl_gui_column_tree=>style_emphasized_a.
    ls_node_layout-u_style = 'X'.

    ls_node_layout-n_image = lc_display.
    ls_node_layout-u_n_image = 'X'.

*   아이콘 변경 처리.
    CALL METHOD grf_tree->change_node
      EXPORTING
        i_node_key     = node_key
        i_outtab_line  = ls_outtab_line
        is_node_layout = ls_node_layout.

    CALL METHOD grf_tree->frontend_update.

    CLEAR gs_tree.
    READ TABLE gt_tree INTO gs_tree  INDEX node_key.

    gv_bmode = space.
    grf_grid_dtl->set_edit_mode( '' ).

*   세부사항 조회.
    PERFORM detail_screen USING gs_tree.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_toolbar_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS : on_function_selected
      FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.
*
  METHOD on_function_selected.
    DEFINE _l_button.

      CALL METHOD grf_tree_toolbar->delete_button
        EXPORTING
          fcode = 'CHAN'.

      CALL METHOD grf_tree_toolbar->add_button
        EXPORTING
          fcode     = 'CHAN'
          icon      =  &1
          butn_type = cntb_btype_button
          text      = ''
          quickinfo = TEXT-f04.
    end-OF-DEFINITION.

* §5. Query the function codes of the toolbar in your implementation.
    CASE fcode.
      WHEN 'CHAN'.
        IF gv_tmode = 0.
          gv_tmode = 1.
          _l_button '@10@'.
        ELSE.
          gv_tmode = 0.
          _l_button '@3I@'.
        ENDIF.

        CALL METHOD grf_tree->frontend_update.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_de_event DEFINITION.
  PUBLIC SECTION.
    METHODS :
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_ucomm.

ENDCLASS.

CLASS lcl_de_event IMPLEMENTATION.
  METHOD handle_data_changed.
    PERFORM event_data USING er_data_changed e_ucomm.

  ENDMETHOD.

ENDCLASS.
