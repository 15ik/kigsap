*&---------------------------------------------------------------------*
*& Include          ZOMM3030O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_0100 OUTPUT.

*  DATA(GT_FCODE) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( EXPORTING IV_MODE = GV_MODE ).

  SET PF-STATUS '0100'. "EXCLUDING GT_FCODE.
  SET TITLEBAR '0100' WITH TEXT-T01.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN OUTPUT.

  PERFORM SET_GRID.

ENDMODULE.
