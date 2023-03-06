*&---------------------------------------------------------------------*
*& Include          ZRMM3050I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  gv_ok_code = ok_code.
  CLEAR ok_code.

  CASE gv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM check_exit.
  ENDCASE.

  CLEAR gv_ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  gv_ok_code = ok_code.
  CLEAR ok_code.

  CASE gv_ok_code.
    WHEN 'OKAY'.
*      IF gs_trans-employ_no IS INITIAL.
*        MESSAGE s017 WITH TEXT-f08 DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.

      PERFORM transfer_person.

    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  gv_ok_code = ok_code.
  CLEAR ok_code.

  CASE gv_ok_code.
    WHEN 'OKAY'.
      PERFORM change_mass_order.

    WHEN 'CLOSE'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR gv_ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_DATA_0400  INPUT
*&---------------------------------------------------------------------*
MODULE check_input_data_0400 INPUT.

* 삭제 체크 시 Clear
  IF gs_mass-bstae_del = 'X'.
    CLEAR: gs_mass-bstae.
  ENDIF.

  IF gs_mass-uebto_del = 'X'.
    CLEAR: gs_mass-uebto.
  ENDIF.

  IF gs_mass-absgr IS NOT INITIAL.
    SELECT SINGLE absgr_txt
        INTO @gs_mass-atext
      FROM t165m
      WHERE spras = @sy-langu
         AND absgr = @gs_mass-absgr.
    IF sy-subrc NE 0.
      CLEAR : gs_mass-atext.
      MESSAGE s022(zmm01) WITH TEXT-c28 gs_mass-absgr DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  IF gs_mass-lgort IS NOT INITIAL.
    SELECT SINGLE lgort
        INTO @DATA(gv_lgort)
      FROM t001l
      WHERE werks = @gs_mass-werks
         AND lgort = @gs_mass-lgort.
    IF sy-subrc NE 0.
      CLEAR : gs_mass-lgort.
      MESSAGE s022(zmm01) WITH TEXT-d27 gs_mass-lgort DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  gv_ok_code = ok_code.
  CLEAR ok_code.

  CASE gv_ok_code.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR gv_ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.

  gv_ok_code = ok_code.
  CLEAR ok_code.

  CASE gv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM check_exit.
  ENDCASE.

  CLEAR gv_ok_code.

ENDMODULE.
