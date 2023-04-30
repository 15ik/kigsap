*&---------------------------------------------------------------------*
*& Include          ZOMM4990O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA(GT_FCODE) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE ).

  GT_FCODE = VALUE #( BASE GT_FCODE[] ( 'DISP' ) ( 'EDIT' ) ( 'SAVE' ) ).

  SET PF-STATUS '0100' EXCLUDING GT_FCODE.
  SET TITLEBAR  '0100' WITH TEXT-T01.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN OUTPUT.

  PERFORM SET_GRID.
  GV_MODE = 'X'.
  CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = GV_MODE ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

*  DATA(GT_FCODE) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE ).

  GT_FCODE = VALUE #( BASE GT_FCODE[] ( 'DISP' ) ( 'EDIT' ) ( 'SAVE' ) ).

  SET PF-STATUS '0100' EXCLUDING GT_FCODE.
  CASE 'X'.
    WHEN P_RD1B.
  SET TITLEBAR  '0200' WITH TEXT-T02.
    WHEN P_RD1C.
  SET TITLEBAR  '0200' WITH TEXT-T03.
    WHEN P_RD1D.
  SET TITLEBAR  '0200' WITH TEXT-T04.
    WHEN P_RD1E.
  SET TITLEBAR  '0200' WITH TEXT-T05.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0200 OUTPUT.

  PERFORM SET_GRID_0200.
  GV_MODE = 'X'.
  CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = GV_MODE ).
ENDMODULE.
