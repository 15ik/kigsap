*&---------------------------------------------------------------------*
*& Include          ZRMM2020O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100' WITH TEXT-T06.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN OUTPUT.

  PERFORM SET_GRID.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0200 OUTPUT.

  IF GRF_CUSTOM_CONTAINER_0200 IS INITIAL.

    CREATE OBJECT GRF_CUSTOM_CONTAINER_0200
      EXPORTING
        CONTAINER_NAME = GV_CONTAINER_0200 .

*-
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_000.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID_0200->SET_GRID( EXPORTING IV_VARI = 'VERI_0200' CHANGING  CT_DATA = GT_REQ_DT ).

  ELSE.
    GRF_GRID_0200->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDMODULE.
