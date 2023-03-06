*&---------------------------------------------------------------------*
*& Include          ZRMM3050CLS
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
      evt_button_clik           REDEFINITION.

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
      on_set_sort               REDEFINITION,
      on_chk_data               REDEFINITION,
      on_info_text              REDEFINITION,
      on_set_attatch_info       REDEFINITION.

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
*        PERFORM EVT_GRID_DATA_CHANGED USING IRF_DATA_CHANGED
*                                            IV_ONF4
*                                            IV_ONF4_BEFORE
*                                            IV_ONF4_AFTER
*                                            IV_UCOMM.
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
      WHEN 'ALV_GRID' OR 'HIS_HEAD' OR 'HIS_ITEM' OR 'ALV_SUBC'.
        CASE e_ucomm.
          WHEN 'BTN_DETAIL'.
            PERFORM submit_zrmm3020.

*          WHEN 'BTN_TRANS'.
*            PERFORM popup_transfer_person.
*
          WHEN 'BTN_CLOSE_MULTI'.   "발주종료 Multi
            PERFORM close_order_multi.

          WHEN 'BTN_CLOSE_SINGLE'.  "발주종료 Single: 종료 사유 입력
            PERFORM close_order_single.

          WHEN 'BTN_CHANGE'.
            PERFORM submit_po_change.

          WHEN 'BTN_CHANGE_MASS'.   "변경
            PERFORM popup_change_mass_order.

*          WHEN 'BTN_HISTORY'.   "변경이력
*            PERFORM popup_po_history.

          WHEN 'BTN_COMP_DIS'.   "구성품 조회
            PERFORM popup_comp_display.

          WHEN 'BTN_EXPAND' OR 'BTN_COLLAPSE'.
            PERFORM comp_expand.

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
        PERFORM evt_grid_toolbar CHANGING ct_add_toolbar.

      WHEN 'ALV_SUBC'.
        PERFORM evt_subc_grid_toolbar CHANGING ct_add_toolbar.

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
*        PERFORM EVT_GRID_F4 USING    IV_FIELDNAME
*                            CHANGING <LT_F4_LIST>
*                                     CV_TITLE
*                                     EV_MONTH_DISPLAY .
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
*        PERFORM evt_grid_double_click USING e_row-index
*                                            e_column.
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
*        PERFORM EVT_GRID_CHANGED_FINISHED USING ET_GOOD_CELLS.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_button_click.
* CELL 별 BUTTON EVENT
*===================================================================================================
  METHOD evt_button_clik.

    CASE me->m_name.
      WHEN 'ALV_GRID'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM evt_grid_button_click USING es_col_id
                                            es_row_no.

      WHEN 'ALV_SUBC'.
        PERFORM evt_subc_button_click USING es_col_id
                                            es_row_no.
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
      WHEN 'ALV_GRID' OR 'HIS_HEAD' OR 'HIS_ITEM'.
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
      WHEN 'ALV_SUBC'.
        ct_ex_toolbar = VALUE #( ( cl_gui_alv_grid=>mc_fc_average )
                                 ( cl_gui_alv_grid=>mc_fc_sum     )
                                 ( cl_gui_alv_grid=>mc_fc_subtot  )
                                 ( cl_gui_alv_grid=>mc_fg_sort  )
                                 ( cl_gui_alv_grid=>mc_fc_filter  ) ).
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
        SET HANDLER : me->evt_toolbar                 FOR me,
                      me->evt_user_command            FOR me,
                      me->evt_double_click            FOR me,
                      me->evt_button_clik             FOR me.

      WHEN 'HIS_HEAD' OR 'HIS_ITEM'.
        SET HANDLER : me->evt_toolbar                 FOR me,
                      me->evt_user_command            FOR me.

      WHEN 'ALV_SUBC'.
        SET HANDLER : me->evt_button_clik                 FOR me,
                                me->evt_toolbar                 FOR me,
                                me->evt_user_command            FOR me.

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
      WHEN 'ALV_GRID'.
*--------------------------
* Modify Field Catalog
* 저장전 중복체크를 위한 Table key 세팅을 반드시!!
*--------------------------
        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.

      WHEN 'HIS_HEAD'.
        PERFORM alv_his_head_fcat_modify CHANGING ct_fcat.

      WHEN 'HIS_ITEM'.
        PERFORM alv_his_item_fcat_modify CHANGING ct_fcat.

      WHEN 'ALV_SUBC'.
        PERFORM alv_sub_comp_fcat_modify CHANGING ct_fcat.

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
      WHEN 'ALV_GRID' OR 'HIS_HEAD' OR 'HIS_ITEM'.
        cs_layout-zebra      = 'X'.
        cs_layout-cwidth_opt = space.

      WHEN 'ALV_SUBC'.
        cs_layout-totals_bef = abap_true.
        cs_layout-cwidth_opt = space.

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

      WHEN 'ALV_SUBC'.
        PERFORM alv_subc_set_line_style USING it_fcat.
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
*        PERFORM ALV_SET_F4 CHANGING CT_FIELD.
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
      WHEN 'ALV_SUBC'.
        PERFORM alv_set_sort CHANGING ct_sort.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_chk_data.
* Save 전 Display Check Data
* 각 그리드별로 구분하여 사용
* Naming Rule alv_xxx(그리드명)_set_sort
*===============================================================================================
  METHOD on_chk_data.

*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**        PERFORM ALV_CHK_DATA CHANGING CS_DATA
**                                      CV_ERR.
*    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD ON_INFO_TEXT.
* Status Description 구성
* 각 그리드별로 구분하여 사용
* Naming Rule alv_xxx(그리드명)_set_info
*===============================================================================================
  METHOD on_info_text.

    CASE iv_name.
      WHEN 'ALV_GRID'.

*        PERFORM ALV_GRID_SET_INFO USING    IV_FNAME IV_STATU
*                                  CHANGING CV_TEXT.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD ON_SET_ATTATCH_INFO.
* 첨부파일 Object Setting & Attatch Icon Field Name
* 각 그리드별로 구분하여 사용
* Naming Rule alv_xxx(그리드명)_set_attatch_info
*===============================================================================================
  METHOD on_set_attatch_info.

    CASE iv_name.
      WHEN 'ALV_GRID'.
*        PERFORM ALV_GRID_SET_ATTATCH_INFO USING    IS_DATA
*                                          CHANGING CS_OBJECT
*                                                   CV_ATTCH_FNAME.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
