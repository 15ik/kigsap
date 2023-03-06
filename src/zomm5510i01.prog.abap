*&---------------------------------------------------------------------*
*& Include          ZOMM5510I01
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

    WHEN 'CLEAR'. "Clearing
      PERFORM CLEARING_PRE_PAYMENT.

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
    WHEN 'OKAY'.
      PERFORM CREATE_CLEARING_DOCUMENT.

    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
