*&---------------------------------------------------------------------*
*& Include          ZRMM3030I01
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

    WHEN 'ALL' OR 'LIFNR' OR 'MATNR' OR 'LIMAT' OR 'DEPT_QM'.
      PERFORM SORT_BY_CRITERIA USING GV_OK_CODE.

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
