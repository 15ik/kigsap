*&---------------------------------------------------------------------*
*& Include          ZRMM4400I01
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

    WHEN 'CLEAR'.
      PERFORM REFRESH_DATA_110.

*    WHEN 'DISP' OR 'EDIT'.
*      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.
*    WHEN 'SAVE'.
*      PERFORM SAVE_DATA.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  GV_OK_CODE_DETAIL = OK_CODE_DETAIL.
  CLEAR : OK_CODE_DETAIL.

  CASE GV_OK_CODE_DETAIL.
    WHEN 'CHANGE'.
      PERFORM CHANGE_STOCK_QI_AV.
      LEAVE TO SCREEN 0.

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

  GV_OK_CODE_DETAIL = OK_CODE_DETAIL.
  CLEAR : OK_CODE_DETAIL.

  CASE GV_OK_CODE_DETAIL.
    WHEN 'CHANGE'.
      PERFORM CHANGE_STOCK_AV_QI.
      LEAVE TO SCREEN 0.

    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
