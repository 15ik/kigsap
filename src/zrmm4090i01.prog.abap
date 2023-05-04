*&---------------------------------------------------------------------*
*& Include          ZRMM4090I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.
  GV_OK_CODE = OK_CODE.
  CLEAR : OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM CHECK_EXIT.

    WHEN 'CLEAR'.
      PERFORM REFRESH_DATA_110.
*    WHEN 'DISP' OR 'EDIT'.
*      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.
*    WHEN 'SAVE'.
*      PERFORM SAVE_DATA.
  ENDCASE.
ENDMODULE.
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

    WHEN 'CLEAR'.
      PERFORM REFRESH_DATA_100.
*    WHEN 'DISP' OR 'EDIT'.
*      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.
*    WHEN 'SAVE'.
*      PERFORM SAVE_DATA.
  ENDCASE.
ENDMODULE.
