*&---------------------------------------------------------------------*
*& Include          ZOMM3030I01
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

*    WHEN 'DISP' OR 'EDIT'.
*      PERFORM SET_CHANGE_MODE CHANGING GV_MODE.

    WHEN 'REFR'.
      PERFORM REFRESH_DATA.

  ENDCASE.

  CLEAR GV_OK_CODE.

ENDMODULE.
