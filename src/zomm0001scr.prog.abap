*&---------------------------------------------------------------------*
*& Include          ZOMM0001SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_main TYPE ztmm00001-zmain_cat
                                               MEMORY ID zmo OBLIGATORY .
SELECTION-SCREEN END OF BLOCK blk1.

*SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-b02 .
*  PARAMETERS:     pa_del AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK blk2.


*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*
* CHECK_AUTHORITY ##### ## ### ##### ##
AT SELECTION-SCREEN ON p_main.
  PERFORM check_zmain_cat USING p_main.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* Screen Parameter Control
*----------------------------------------------------------------------*
*  CALL METHOD zcl_coz_rad=>set_disable.
