*&---------------------------------------------------------------------*
*& Include          ZRMM3020I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  GV_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM CHECK_EXIT.

    WHEN 'ALL' OR 'LIFNR' OR 'MATNR' OR 'MATKL' OR 'ORDER' OR 'DEPTO'.
      PERFORM SORT_BY_CRITERIA USING GV_OK_CODE.

    WHEN 'BKLAS'. "계정별 미착현황
      PERFORM POPUP_BKLAS_SUM.

    WHEN OTHERS.
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
