*&---------------------------------------------------------------------*
*& Include zrmm2030cls
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
      EVT_USER_COMMAND          REDEFINITION,
      EVT_DOUBLE_CLICK          REDEFINITION,
      EVT_ON_TOOLBAR            REDEFINITION,
      EVT_HOTSPOT_CLICK         REDEFINITION.


  PROTECTED SECTION.
*---------------------------
* Alv Grid Area
*---------------------------
    METHODS :
      ON_SAVE_VARIANT           REDEFINITION,
      ON_EXCLUD_STD_TOOLBAR     REDEFINITION,
      ON_SET_EVENT              REDEFINITION,
      ON_SET_FCAT               REDEFINITION,
      ON_SET_LAYOUT             REDEFINITION,
      ON_SET_LINE_STYLE         REDEFINITION,
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

          WHEN OTHERS.
            CALL METHOD SUPER->EVT_USER_COMMAND
              EXPORTING
                E_UCOMM = E_UCOMM
                SENDER  = ME.
        ENDCASE.
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
        PERFORM EVT_GRID_TOOLBAR      CHANGING CT_ADD_TOOLBAR.
      WHEN OTHERS.
    ENDCASE.

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
      WHEN 'ALV_GRID' OR 'ALV_GRID_0200'.
        SET HANDLER :
                      ME->EVT_TOOLBAR                 FOR ME,
                      ME->EVT_DOUBLE_CLICK            FOR ME,
                      ME->EVT_USER_COMMAND            FOR ME,
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
        PERFORM ALV_GRID_FCAT_MODIFY           CHANGING CT_FCAT.
      WHEN 'ALV_GRID_0200'.
        PERFORM ALV_GRID_0200_FCAT_MODIFY      CHANGING CT_FCAT.
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
        CS_LAYOUT-CWIDTH_OPT = SPACE.
      WHEN 'ALV_GRID_0200'.
        CS_LAYOUT-CWIDTH_OPT = SPACE.
        CS_LAYOUT-SEL_MODE  = 'B'.
        CS_LAYOUT-ZEBRA     = 'X'.
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
        PERFORM ALV_GRID_SET_LINE_STYLE      USING IT_FCAT.
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
        PERFORM ALV_SET_SORT CHANGING CT_SORT.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
