*&---------------------------------------------------------------------*
*& Include          ZRMM1020O01
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
  SET TITLEBAR  '0100' WITH TEXT-T03.

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

  DATA(GT_FCODE_0200) = ZCL_CN_ALV_GRID=>GET_GUI_STATU( GV_MODE ).

  IF GV_MULTI_VER IS INITIAL.
    GT_FCODE_0200 = VALUE #( BASE GT_FCODE_0200[] ( 'BTN_VER' ) ).
    SET PF-STATUS '0200' EXCLUDING GT_FCODE_0200.
  ENDIF.

  SET PF-STATUS '0200' EXCLUDING GT_FCODE_0200.
  SET TITLEBAR '0200'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0200 OUTPUT.

  PERFORM SET_GRID_0200.

ENDMODULE.
