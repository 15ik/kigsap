*&---------------------------------------------------------------------*
*& Include          ZOMM3992O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100' WITH TEXT-T01.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN OUTPUT.

  PERFORM SET_GRID.

ENDMODULE.
