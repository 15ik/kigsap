*&---------------------------------------------------------------------*
*& Include          ZRMM4430CLS
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
      evt_user_command          REDEFINITION,
      evt_double_click          REDEFINITION,
      evt_on_toolbar            REDEFINITION,
      evt_on_f4                 REDEFINITION,
      evt_data_changed_finished REDEFINITION,
      evt_hotspot_click         REDEFINITION.


  PROTECTED SECTION.
*---------------------------
* Alv Grid Area
*---------------------------
    METHODS :
      on_save_variant           REDEFINITION,
      on_exclud_std_toolbar     REDEFINITION,
      on_set_drop_down          REDEFINITION,
      on_set_event              REDEFINITION,
      on_set_fcat               REDEFINITION,
      on_set_layout             REDEFINITION,
      on_set_line_style         REDEFINITION,
      on_set_f4                 REDEFINITION,
      on_set_sort               REDEFINITION.

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
        PERFORM evt_grid_data_changed USING irf_data_changed
                                            iv_onf4
                                            iv_onf4_before
                                            iv_onf4_after
                                            iv_ucomm.
      WHEN OTHERS.
    ENDCASE.

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
    CASE me->m_name.
      WHEN 'ALV_GRID'.
        CASE e_ucomm.
          WHEN 'OK_CODE'.
          WHEN 'BTN_ON_TOP_DOWN'.
            PERFORM btn_on_top_down_init.   "Top Down
          WHEN 'BTN_ON_BOTTOM_UP'.
            PERFORM btn_on_bottom_up_init.  "Bottom Up
          WHEN 'BTN_ON_ALL'.
            PERFORM btn_on_all_init.        "ALL
          WHEN 'BTN_ON_STD'.
            PERFORM btn_on_std_display.     "STD Display
          WHEN OTHERS.
            CALL METHOD super->evt_user_command
              EXPORTING
                e_ucomm = e_ucomm
                sender  = me.
        ENDCASE.

      WHEN 'ALV_ITEM'.

        CASE e_ucomm.
          WHEN 'OK_CODE'.
*          WHEN 'BTN_ON_PROCESS'.
*            PERFORM BTN_ON_PROCESS_INIT.  "입고재처리
          WHEN OTHERS.
            CALL METHOD super->evt_user_command
              EXPORTING
                e_ucomm = e_ucomm
                sender  = me.
        ENDCASE.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*=======================================================================
* METHOD evt_on_toolbar.
* Alv Grid Button
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_toolbar
*=======================================================================
  METHOD evt_on_toolbar.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
        PERFORM evt_grid_toolbar USING  me->m_name
                                 CHANGING ct_add_toolbar.

*      WHEN 'ALV_ITEM'.
*        PERFORM EVT_ITEM_GRID_TOOLBAR USING  ME->M_NAME
*                                      CHANGING CT_ADD_TOOLBAR.

      WHEN OTHERS.
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
        _g_set_value : cv_title          'Month Calendar',  "생략시 "'F4 Code List'
                       ev_month_display  'X'.
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
                                     cv_title
                                     ev_month_display .
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
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_grid_double_click USING e_row-index
                                            e_column.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_hotspot_click.
* Double Click Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_double_click
*===================================================================================================
  METHOD evt_hotspot_click.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_grid_hotspot_click USING es_row_no-row_id
                                             e_column_id.
      WHEN OTHERS.
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
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_grid_changed_finished USING et_good_cells.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
************************************************************************************************
* ALV Grid 내에서 User Define Area
************************************************************************************************
*===============================================================================================
*  METHOD on_save_variant.
* Save Variant User Area
* Variant Maintenace 기능 세팅
*===============================================================================================
  METHOD on_save_variant.

    CASE iv_name.
      WHEN 'ALV_GRID'.
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
*        CT_EX_TOOLBAR = VALUE #( ( CL_GUI_ALV_GRID=>MC_FC_AVERAGE )
*                                 ( CL_GUI_ALV_GRID=>MC_FC_SUM     )
*                                 ( CL_GUI_ALV_GRID=>MC_FC_SUBTOT  ) ).
      WHEN OTHERS.
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
*        PERFORM AVL_GRID_SET_DROP_DOWN CHANGING CT_DROP.
      WHEN OTHERS.
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
        SET HANDLER :
                      me->evt_toolbar                 FOR me,
                      me->evt_data_changed            FOR me,
                      me->evt_double_click            FOR me,
                      me->evt_user_command            FOR me,
                      me->evt_data_changed_finished   FOR me,
                      me->evt_onf4                    FOR me,
                      me->evt_hotspot_click           FOR me.

      WHEN 'ALV_ITEM'.
        SET HANDLER :
                      me->evt_toolbar                 FOR me,
                      me->evt_data_changed            FOR me,
                      me->evt_double_click            FOR me,
                      me->evt_user_command            FOR me,
                      me->evt_data_changed_finished   FOR me,
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

    CASE iv_name.
*--------------------------
* Modify Field Catalog
* 저장전 중복체크를 위한 Table key 세팅을 만드시!!
*--------------------------
      WHEN 'ALV_GRID'.
        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
      WHEN 'ALV_ITEM'.
        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
      WHEN OTHERS.
    ENDCASE.
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
        cs_layout-cwidth_opt = 'X'.
      WHEN 'ALV_ITEM'.
        cs_layout-cwidth_opt = 'X'.
      WHEN OTHERS.
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
        PERFORM alv_grid_set_line_style USING it_fcat.

      WHEN 'ALV_ITEM'.
        PERFORM alv_grid_set_line_style USING it_fcat.
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
      WHEN 'ALV_ITEM'.
        PERFORM alv_set_f4 CHANGING ct_field.

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
      WHEN 'ALV_ITEM'.
        PERFORM alv_set_sort CHANGING ct_sort .
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
