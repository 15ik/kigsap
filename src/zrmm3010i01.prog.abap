*&---------------------------------------------------------------------*
*& Include          ZRMM3010I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM CHECK_EXIT.
  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
