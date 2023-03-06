*&---------------------------------------------------------------------*
*& Include          ZRMM4400CLS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Class LCL_CUST_ALV_GRID
*&---------------------------------------------------------------------*
CLASS LCL_CUST_ALV_GRID DEFINITION INHERITING FROM ZCL_CN_ALV_GRID.

  PUBLIC SECTION.
    METHODS:
*---------------------------
* Event Area
*---------------------------
      EVT_ON_DATA_CHANGED       REDEFINITION,
      EVT_USER_COMMAND          REDEFINITION,
      EVT_DOUBLE_CLICK          REDEFINITION,
      EVT_ON_TOOLBAR            REDEFINITION,
      EVT_ON_F4                 REDEFINITION,
      EVT_DATA_CHANGED_FINISHED REDEFINITION,
      EVT_HOTSPOT_CLICK         REDEFINITION.


  PROTECTED SECTION.
*---------------------------
* Alv Grid Area
*---------------------------
    METHODS :
      ON_SAVE_VARIANT           REDEFINITION,
      ON_EXCLUD_STD_TOOLBAR     REDEFINITION,
      ON_SET_DROP_DOWN          REDEFINITION,
      ON_SET_EVENT              REDEFINITION,
      ON_SET_FCAT               REDEFINITION,
      ON_SET_LAYOUT             REDEFINITION,
      ON_SET_LINE_STYLE         REDEFINITION,
      ON_SET_F4                 REDEFINITION,
      ON_SET_SORT               REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
CLASS LCL_CUST_ALV_GRID IMPLEMENTATION.
************************************************************************
* Event Area
************************************************************************
*=======================================================================
* METHOD evt_on_data_changed.
* Data Chagned Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_data_chaged
*=======================================================================
  METHOD EVT_ON_DATA_CHANGED.

    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
        PERFORM EVT_GRID_DATA_CHANGED USING IRF_DATA_CHANGED
                                            IV_ONF4
                                            IV_ONF4_BEFORE
                                            IV_ONF4_AFTER
                                            IV_UCOMM.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*=======================================================================
* METHOD evt_user_command.
* Alv User BUtton Event
* 각 그리드별로 구분하여 사용
* Naming Rule BTN_xxx(E_UCOMM명)
*=======================================================================
  METHOD EVT_USER_COMMAND.
*-----------------------------------------------------------------------
* 공통에서 기본사용되는(Change,add row, del_row, recovery, excel..)
*-----------------------------------------------------------------------
    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
        CASE E_UCOMM.
          WHEN 'OK_CODE'.
          WHEN 'BTN_ON_ZFDLV'.
            PERFORM BTN_ON_ZFDLV_INIT.  "배치 변경
          WHEN 'BTN_ON_QI_AV'.
            PERFORM BTN_ON_QI_AV_INIT.  "품질-->가용
          WHEN 'BTN_ON_AV_QI'.
            PERFORM BTN_ON_AV_QI_INIT.  "가용-->품질
          WHEN OTHERS.
            CALL METHOD SUPER->EVT_USER_COMMAND
              EXPORTING
                E_UCOMM = E_UCOMM
                SENDER  = ME.
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
  METHOD EVT_ON_TOOLBAR.

    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
        PERFORM EVT_GRID_TOOLBAR USING  ME->M_NAME
                                 CHANGING CT_ADD_TOOLBAR.

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
  METHOD EVT_ON_F4.
*--------------------------------------
* F4로 사용하고자 하는 Type 선언
*--------------------------------------
    TYPES:BEGIN OF LTY_F4,          "Perform 안에서도 동일하게 선언
            CDVAL TYPE CHAR2,
            TEXT  TYPE LTEXT,
          END OF LTY_F4.

    FIELD-SYMBOLS:<LT_F4_LIST> TYPE TABLE.

*----------------------------------------------------------
* Month 달력은 예외적이여서 별도 필드명으로 구분하여 처리
*----------------------------------------------------------
    CASE IV_FIELDNAME.
      WHEN 'SPMON'.
        _G_SET_VALUE : CV_TITLE          'Month Calendar',  "생략시 "'F4 Code List'
                       EV_MONTH_DISPLAY  'X'.
        RETURN.
    ENDCASE.

*--------------------------------------
* Field 별로 Type을 지정하여 사용
*--------------------------------------
    CASE IV_FIELDNAME.
      WHEN 'CDVAL'.
        CREATE DATA AO_F4 TYPE TABLE OF LTY_F4.
        ASSIGN AO_F4->* TO <LT_F4_LIST>.
    ENDCASE.

*--------------------------------------
* 그리드 별로 구분하여 처리
*--------------------------------------
    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.

*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM EVT_GRID_F4 USING    IV_FIELDNAME
                            CHANGING <LT_F4_LIST>
                                     CV_TITLE
                                     EV_MONTH_DISPLAY .
    ENDCASE.

    CHECK AO_F4 IS BOUND.
    EO_F4_LIST = AO_F4.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_double_click.
* Double Click Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_double_click
*===================================================================================================
  METHOD EVT_DOUBLE_CLICK.

    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM EVT_GRID_DOUBLE_CLICK USING E_ROW-INDEX
                                            E_COLUMN.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===================================================================================================
* METHOD evt_data_changed_finished.
* Data Changed Finished Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_finished
*===================================================================================================
  METHOD EVT_DATA_CHANGED_FINISHED.

    CHECK E_MODIFIED IS NOT INITIAL.

    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM EVT_GRID_CHANGED_FINISHED USING ET_GOOD_CELLS.
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
  METHOD ON_SAVE_VARIANT.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        CV_DEFAULT = 'X'.
        CV_SAVE    = 'A'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_exclud_std_toolbar.
* ON_EXCLUD_STD_TOOLBAR User Area
* Default Standard Toolbar 에서 제거 하고자 하는경우
* Ex)Sum,Total은 금액이나 수량이 없는 경우는 필요가 없으므로 이 영역에서 제거한다
*===============================================================================================
  METHOD ON_EXCLUD_STD_TOOLBAR.

    CASE IV_NAME.
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
  METHOD ON_SET_DROP_DOWN.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        PERFORM AVL_GRID_SET_DROP_DOWN CHANGING CT_DROP.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_event.
* Event Handler Setting Area
* 각 그리드별로 구분하여 사용
*===============================================================================================
  METHOD ON_SET_EVENT.

    CALL METHOD ME->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        SET HANDLER :
                      ME->EVT_TOOLBAR                 FOR ME,
                      ME->EVT_DATA_CHANGED            FOR ME,
                      ME->EVT_DOUBLE_CLICK            FOR ME,
                      ME->EVT_USER_COMMAND            FOR ME,
                      ME->EVT_DATA_CHANGED_FINISHED   FOR ME,
                      ME->EVT_ONF4                    FOR ME,
                      ME->EVT_HOTSPOT_CLICK           FOR ME.
    ENDCASE.
  ENDMETHOD.
*===============================================================================================
* METHOD on_set_fcat .
* Field Catalog User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_fcat_modify
*===============================================================================================
  METHOD ON_SET_FCAT .

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
*--------------------------
* Modify Field Catalog
* 저장전 중복체크를 위한 Table key 세팅을 만드시!!
*--------------------------
        PERFORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
*===============================================================================================
* METHOD on_set_layout.
* Layout User Setting Area
* Defalut Lyaout에서 변경이 필요한경우
* 각 그리드별로 구분하여 사용
*===============================================================================================
  METHOD ON_SET_LAYOUT.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        CS_LAYOUT-ZEBRA = 'X'.
        CS_LAYOUT-CWIDTH_OPT = ' '.
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
  METHOD ON_SET_LINE_STYLE.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        PERFORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_f4.
* F4를 사용하기 위해  필드명을 등록하기 위한  User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_F4
*===============================================================================================
  METHOD ON_SET_F4.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        PERFORM ALV_SET_F4 CHANGING CT_FIELD.
    ENDCASE.

  ENDMETHOD.
*===============================================================================================
* METHOD on_set_sort.
* Sort 필드 User Setting Area
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_set_sort
*===============================================================================================
  METHOD ON_SET_SORT.

    CASE IV_NAME.
      WHEN 'ALV_GRID'.
        PERFORM ALV_SET_SORT CHANGING CT_SORT .
    ENDCASE.

  ENDMETHOD.

*===================================================================================================
* METHOD evt_hotspot_click.
* Double Click Event
* 각 그리드별로 구분하여 사용
* Naming Rule evt_xxx(그리드명)_double_click
*===================================================================================================
  METHOD EVT_HOTSPOT_CLICK.

    CASE ME->M_NAME.
      WHEN 'ALV_GRID'.
*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
        PERFORM EVT_GRID_HOTSPOT_CLICK USING ES_ROW_NO-ROW_ID
                                             E_COLUMN_ID.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
