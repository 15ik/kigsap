*&---------------------------------------------------------------------*
*& Include          ZRMM5010I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR : OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM CHECK_EXIT.
    WHEN 'EDIT' OR 'DISP'.
      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  GV_OK_CODE_200 = OK_CODE_200.
  CLEAR : OK_CODE_200.

  CASE GV_OK_CODE_200.
    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  GV_OK_CODE_300 = OK_CODE_300.
  CLEAR : OK_CODE_300, GV_CONTINUE_300.

  CASE GV_OK_CODE_300.
    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
    WHEN 'POST_CANCEL'.
      GS_SCR_300-STGRD = UF05A-STGRD.
      GV_CONTINUE_300 = 'X'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
