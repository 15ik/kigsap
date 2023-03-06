*&---------------------------------------------------------------------*
*& Include          ZRMM3090O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZRMM3090O01
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA(GT_FCODE) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE ).

  SET PF-STATUS '0100' EXCLUDING GT_FCODE.
  SET TITLEBAR '0100' WITH TEXT-T01.

ENDMODULE. " STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ALV_SCREEN OUTPUT.

  PERFORM SET_GRID.

ENDMODULE. " SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.

  DATA(GT_FCODE_0200) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE_0200 ).

  SET PF-STATUS '0200' EXCLUDING GT_FCODE_0200.

  SET TITLEBAR '0200' WITH GV_0200_TITLE.

ENDMODULE. " SET_ALV_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ALV_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0200 OUTPUT.

  PERFORM SET_GRID_0200.

ENDMODULE. " SET_ALV_SCREEN OUTPUT
