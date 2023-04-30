*&---------------------------------------------------------------------*
*& Include          ZOMM3992I01
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

    WHEN 'SAVE'.
      PERFORM CREATE_PR.

  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
