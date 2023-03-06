*&---------------------------------------------------------------------*
*& Include          ZRMM1020I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR : OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM CHECK_EXIT.
    WHEN 'DISP' OR 'EDIT'.
      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  GV_OK_CODE_0200 = OK_CODE_0200.
  CLEAR : OK_CODE_0200.

  CASE GV_OK_CODE_0200.
    WHEN 'BTN_VER'.  "생산버젼적용
      PERFORM BTN_VER_INIT.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
