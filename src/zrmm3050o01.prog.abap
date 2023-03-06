*&---------------------------------------------------------------------*
*& Include          ZRMM3050O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE set_status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100' WITH TEXT-t01.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE set_alv_screen OUTPUT.

* FCAT 필드 숨김
  CASE p_kalsk.
    WHEN gc_kalsk_do.
      gv_tech = gc_tech_x1.
    WHEN gc_kalsk_im.
      gv_tech = gc_tech_x2.
  ENDCASE.

  PERFORM set_grid.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR '0200' WITH TEXT-t02.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status_0300 OUTPUT.

  DATA: gt_fcode TYPE slis_t_extab.

  DEFINE _l_excluding_status.
    gt_fcode = VALUE #( BASE gt_fcode ( fcode = &1 ) ).
  END-OF-DEFINITION.

  IF gv_text_mode = gc_mode_change.
    CLEAR gt_fcode.
  ELSE.
    _l_excluding_status: 'OKAY'.
  ENDIF.

  SET PF-STATUS '0300' EXCLUDING gt_fcode.
  SET TITLEBAR '0300' WITH TEXT-t03.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_TEXT_SCREEN_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_text_screen_0300 OUTPUT.
  PERFORM set_text_editor.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status_0400 OUTPUT.

  SET PF-STATUS '0400'.

  CASE abap_true.
    WHEN p_ra.
      SET TITLEBAR '0400' WITH TEXT-t05.
    WHEN p_rb.
      SET TITLEBAR '0400' WITH TEXT-t04.
  ENDCASE.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_ra.
        IF screen-group1 = 'RB'.
          screen-active = 0.
        ELSEIF screen-group1 = 'RA'.
          IF gs_mass-abs IS INITIAL AND screen-group2 = 'ABS'.
            screen-active = 0.
          ENDIF.

          IF gs_mass-zma IS INITIAL AND screen-group2 = 'ZMA'.
            screen-active = 0.
          ENDIF.
        ENDIF.

      WHEN p_rb.
        IF screen-group1 = 'RA'.
          screen-active = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_STATUS_0500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status_0500 OUTPUT.

  SET PF-STATUS '0500'.
  SET TITLEBAR '0500' WITH TEXT-u07.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_alv_screen_0500 OUTPUT.

  PERFORM set_grid_history.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_STATUS_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status_0600 OUTPUT.

  SET PF-STATUS '0600'.
  SET TITLEBAR '0600' WITH TEXT-u08.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0600 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_alv_screen_0600 OUTPUT.

  PERFORM set_grid_sub_comp.

ENDMODULE.
