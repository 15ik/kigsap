*&---------------------------------------------------------------------*
*& Include          ZOMM0001O01
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA(gt_fcode) = zcl_cn_alv_grid=>get_gui_statu( gv_mode ).

  SET PF-STATUS '0100' EXCLUDING gt_fcode.
  SET TITLEBAR '0100' WITH TEXT-t01.

ENDMODULE. " STATUS_0100 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_ALV_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv_screen OUTPUT.

  PERFORM set_grid.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.

ENDMODULE. " SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  DATA : gv_text TYPE char5.

  CONSTANTS : gc_create(5) VALUE '생성',
              gc_modi(5)   VALUE  '변경'.

  CASE gv_ncheck.
    WHEN 'C'.
      gv_text = gc_create.

    WHEN 'M'.
      gv_text = gc_modi.

      DATA: gt_selected_nodes TYPE lvc_t_nkey,
            gv_selected_node  TYPE lvc_nkey.
      CALL METHOD grf_tree->get_selected_nodes
        CHANGING
          ct_selected_nodes = gt_selected_nodes.
      READ TABLE gt_selected_nodes INTO gv_selected_node INDEX 1.
      READ TABLE gt_tree INTO gs_tree INDEX gv_selected_node.
      IF gs_tree-zsmal_cat IS NOT INITIAL.
        gs_tree_add-cat = gs_tree-zsmal_cat.
        gs_tree_add-dec = gs_tree-zsmal_cat_dec.
      ELSE.
        gs_tree_add-cat = gs_tree-zmidd_cat.
        gs_tree_add-dec = gs_tree-zmidd_cat_dec.
      ENDIF.

      LOOP AT SCREEN.
        IF screen-name = 'GS_TREE_ADD-CAT'(f08).
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

  ENDCASE.

  SET PF-STATUS '0200'.
  SET TITLEBAR '0200' WITH gv_text.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS '0300'.
  SET TITLEBAR '0300'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_alv_screen_0300 OUTPUT.

  IF grf_de_con IS NOT INITIAL.
    CALL METHOD grf_de_con->free.
    CLEAR grf_de_con.
    CLEAR grf_de_grid.
    CLEAR : gt_de_field[].
  ENDIF.

  " Create a custom container control for our ALV Control
  CREATE OBJECT grf_de_con
    EXPORTING
      container_name = 'C_300'
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc NE 0.
    MESSAGE i000 WITH '팝업 생성 오류'(m26).
    RETURN.
  ENDIF.

  " Create ALV
  CREATE OBJECT grf_de_grid
    EXPORTING
      i_parent = grf_de_con.

  " Layout
  gs_de_layout-sel_mode = 'A'.
  gs_de_layout-no_toolbar = 'X'.
  gs_de_layout-no_rowmark = 'X'.

  PERFORM fcat_define TABLES gt_de_field.

  PERFORM event_regist.

  " display.
  CALL METHOD grf_de_grid->set_table_for_first_display
    EXPORTING
      is_layout       = gs_de_layout
    CHANGING
      it_outtab       = gt_de_fcat[]
      it_fieldcatalog = gt_de_field.

*  ENDIF.

ENDMODULE.
