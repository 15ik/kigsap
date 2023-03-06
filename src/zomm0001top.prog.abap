*&---------------------------------------------------------------------*
*& Include          ZOMM0001TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES :ztmm00001.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : lcl_cust_alv_grid DEFINITION DEFERRED.

CLASS : cl_gui_column_tree DEFINITION LOAD.
CLASS : cl_gui_cfw DEFINITION LOAD.


*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS : gc_main(11) VALUE 'ZD_MAIN_CAT'.


*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPE-POOLS : cntl.
*--------------------------------
* F4 Type 선언
*--------------------------------
"*- Alv Dispaly

TYPES: BEGIN OF ts_tree.
         include structure ztmm00001.
TYPES: END OF ts_tree.


TYPES: BEGIN OF ts_disp.
         include TYPE ztmm00001.
         include TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
TYPES: END OF ts_disp.


TYPES: BEGIN OF ts_fcat_dtl.
         include TYPE ztmm00001.
TYPES: END OF ts_fcat_dtl.

TYPES: BEGIN OF ts_disp_dtl.
         include TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         include TYPE ztmm00002.
TYPES: END OF ts_disp_dtl.

TYPES : BEGIN OF ts_de_fcat,
          field      TYPE char10,
          field_name TYPE char30,
          reftable   TYPE char30,
          fieldname  TYPE char30,
        END OF ts_de_fcat.

TYPES : BEGIN OF ts_field,
          field1  TYPE ztmm00002-field1,
          field2  TYPE ztmm00002-field2,
          field3  TYPE ztmm00002-field3,
          field4  TYPE ztmm00002-field4,
          field5  TYPE ztmm00002-field5,
          field6  TYPE ztmm00002-field6,
          field7  TYPE ztmm00002-field7,
          field8  TYPE ztmm00002-field8,
          field9  TYPE ztmm00002-field9,
          field10 TYPE ztmm00002-field10,
          field11 TYPE ztmm00002-field11,
          field12 TYPE ztmm00002-field12,
          field13 TYPE ztmm00002-field13,
        END OF ts_field.

TYPES : BEGIN OF ts_pos_name,
          pos    TYPE char2,
          name   TYPE char30,
          name_c TYPE char30,
        END OF ts_pos_name.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
*DATA:grf_co TYPE REF TO ycl_co_common.

*---------------------
*-- ALV Object
*---------------------
DATA : grf_docking_con TYPE REF TO cl_gui_docking_container.

DATA : grf_head TYPE REF TO cl_gui_container,
       grf_body     TYPE REF TO cl_gui_container,
       grf_body_dtl TYPE REF TO cl_gui_container,
       grf_tree     TYPE REF TO cl_gui_alv_tree.

DATA : "grf_grid_head TYPE REF TO cl_gui_alv_grid,
       grf_grid_dtl  TYPE REF TO lcl_cust_alv_grid.       " 우측(Detial) ALV Grid


DATA : gt_tree_field TYPE lvc_t_fcat, "Fieldcatalog
       gt_dtl_fcat      TYPE lvc_t_fcat, "Fieldcatalog
       grf_tree_toolbar TYPE REF TO cl_gui_toolbar.

DATA: go_document TYPE REF TO cl_dd_document.

DATA: go_ta TYPE REF TO cl_dd_table_element,
      go_col1 TYPE REF TO cl_dd_area,
      go_col2 TYPE REF TO cl_dd_area.
*DATA: gv_dd_text     TYPE sdydo_text_element.

DATA:gs_variant TYPE disvariant, " Variant
     gs_fcat    TYPE lvc_s_fcat.   " Fieldcatalog MODIFY  .

DATA : grf_de_con TYPE REF TO cl_gui_custom_container,
       grf_de_grid  TYPE REF TO cl_gui_alv_grid,
       gt_de_field  TYPE lvc_t_fcat, "Fieldcatalog
       gs_de_layout TYPE lvc_s_layo.
*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_tree TYPE TABLE OF ts_tree,
      gs_tree     TYPE  ts_tree,
      gt_disp     TYPE TABLE OF ts_disp,
      gs_fcat_dtl TYPE ts_fcat_dtl,
      gt_disp_dtl TYPE TABLE OF ts_disp_dtl,
      gt_de_fcat  TYPE TABLE OF ts_de_fcat,

      gt_pos_name TYPE TABLE OF ts_pos_name,
      gs_pos_name TYPE ts_pos_name.

DATA : BEGIN OF gs_tree_add,
         cat TYPE char5,
         dec TYPE char40,
       END OF gs_tree_add.

*DATA : BEGIN OF  gt_field OCCURS 0,
*         pos    TYPE char2,
*         name   TYPE char30,
*         name_c TYPE char30,
*       END OF gt_field.

DATA: ok_code TYPE sy-ucomm,
      gv_ok_code TYPE sy-ucomm,
*      gv_dbl_row TYPE i,         "Double Click
*      gv_tabix   TYPE i,
*      gv_mrow    TYPE i           VALUE 10,                   "Multi Row
      gv_mode.

DATA : gv_tmode VALUE 0,
       gv_bmode,
       gv_ncheck,
       gv_err,
       gv_change.

DATA:gs_msgtb TYPE zscn00001.

DATA : gs_root TYPE REF TO cx_root,
       gs_msg      TYPE string,
       gv_sel_node TYPE lvc_nkey,
       gv_pos      TYPE char2.


FIELD-SYMBOLS : <gt_ta> TYPE table.
FIELD-SYMBOLS : <gs_wa> TYPE any.
FIELD-SYMBOLS : <gs_field> TYPE any.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _g_time_er.
  &1-ernam = sy-uname.
  &1-erdat = sy-datum.
  &1-erzet = sy-uzeit.
end-OF-DEFINITION.

DEFINE _g_time_ae.
  &1-aenam = sy-uname.
  &1-aedat = sy-datum.
  &1-aezet = sy-uzeit.
end-OF-DEFINITION.

DEFINE _g_set_value.

  &1 = &2.

END-OF-DEFINITION .

DEFINE _g_init.
  REFRESH:&1.

END-OF-DEFINITION .
DEFINE _g_get_cell_value.
  CALL METHOD irf_data_changed->get_cell_value
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
    IMPORTING
      e_value     = &3.

END-OF-DEFINITION .
DEFINE _g_modify_cell.
  CALL METHOD irf_data_changed->modify_cell
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
      i_value     = &3.

END-OF-DEFINITION .
DEFINE _g_set_msgtb.

  CLEAR:gs_msgtb.

    gs_msgtb = VALUE #( fieldname = &3   msgty = 'E'  arbgb = &4   txtnr = &5
                        msgv1     = &6   msgv2 = &7   msgv3 = &8  ).

  grf_grid_dtl->set_msgtb( EXPORTING iv_delete = &1
                                 is_msgtb  = gs_msgtb
                       CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _g_set_timestmap.


  zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = &1
                                  CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _g_set_value.
  &1 = &2.
END-OF-DEFINITION .
