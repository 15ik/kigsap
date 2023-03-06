*&---------------------------------------------------------------------*
*& Include          ZRMM2020SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_PLSCN LIKE PLSC-PLSCN  OBLIGATORY.

  SELECT-OPTIONS: S_WERKS  FOR T001W-WERKS OBLIGATORY NO INTERVALS,
                  S_BERID  FOR PPH_DBVM-BERID NO INTERVALS,
                  S_MATNR  FOR MARA-MATNR,
                  S_MATKL  FOR MARA-MATKL,
                  S_DISPO  FOR V_MARC_MD-DISPO,
                  S_EKGRP  FOR V_MARC_MD-EKGRP,
                  S_MMSTA  FOR V_MARC_MD-MMSTA,
                  S_PROD   FOR MARA-MATNR NO INTERVALS.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T01.
    SELECTION-SCREEN POSITION 19.
    SELECT-OPTIONS: S_SPMON  FOR S194-SPMON NO-EXTENSION.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK1 AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN POSITION 6.
    SELECTION-SCREEN COMMENT (20) TEXT-T02 FOR FIELD P_CHK1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(20) TEXT-T03.
    SELECTION-SCREEN POSITION 24.
    PARAMETERS: P_CHK2 AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN POSITION 27.
    SELECTION-SCREEN COMMENT (10) TEXT-T04 FOR FIELD P_CHK2.
    SELECTION-SCREEN POSITION 39.
    PARAMETERS: P_CHK3 AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN POSITION 45.
    SELECTION-SCREEN COMMENT (10) TEXT-T05 FOR FIELD P_CHK3.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(22) TEXT-T12.
    SELECTION-SCREEN POSITION 28.
    PARAMETERS: P_CHK4 AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK2.

**SELECTION-SCREEN FUNCTION KEY 1.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*

**---------------------------------------------------------------------*
** AT SELECTION-SCREEN ON VALUE-REQUEST FOR
**---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SPMON-LOW.
  ZCL_MM_COMMON=>GET_SEARCH_HELP( EXPORTING IV_FNAME  = 'S_SPMON-LOW'
                           IMPORTING EV_VALUE = S_SPMON-LOW  ).
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SPMON-HIGH.
  ZCL_MM_COMMON=>GET_SEARCH_HELP( EXPORTING IV_FNAME  = 'S_SPMON-HIGH'
                           IMPORTING EV_VALUE = S_SPMON-HIGH  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BERID-LOW.
  ZCL_MM_COMMON=>GET_SEARCH_HELP( EXPORTING IV_FNAME      = 'S_BERID-LOW'
                                            IV_FILT_VAL_A = '02'
                                  IMPORTING EV_VALUE      = S_BERID-LOW  ).
**----------------------------------------------------------------------*
** AT SELECTION-SCREEN
**----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CASE SY-UCOMM.
    PERFORM RUN_LTP.
  ENDCASE.

**----------------------------------------------------------------------*
** AT SELECTION-SCREEN OUTPUT
**----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
