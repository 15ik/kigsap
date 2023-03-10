*&---------------------------------------------------------------------*
*& Include          ZRMM4400O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA(GT_FCODE) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE ).
  GT_FCODE = VALUE #( ( 'DISP' ) ( 'EDIT' ) ( 'SAVE' ) ).

  SET PF-STATUS '0100' EXCLUDING GT_FCODE.
  SET TITLEBAR '0100' WITH TEXT-TT1.
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
  DATA(GT_FCODE_0200) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE_DETAIL ).

  SET PF-STATUS '0200' EXCLUDING GT_FCODE_0200.
  SET TITLEBAR '0200' WITH TEXT-T08.    "품질-->가용
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0200 OUTPUT.

  PERFORM SET_GRID_0200.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  DATA(GT_FCODE_0300) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE_DETAIL ).

  SET PF-STATUS '0200' EXCLUDING GT_FCODE_0300.
  SET TITLEBAR '0200' WITH TEXT-T10.    "가용-->품질
ENDMODULE.
