*&---------------------------------------------------------------------*
*& Include          ZRMM2130SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T01.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_WERKS FOR T001W-WERKS NO INTERVALS OBLIGATORY MODIF ID KEY.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T23.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_BERID  FOR MDLV-BERID  NO INTERVALS.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T02.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_MATNR FOR MARA-MATNR.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T03.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_MATKL FOR MARA-MATKL.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T04.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_DISPO FOR V_MARC_MD-DISPO.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T05.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_EKGRP FOR V_MARC_MD-EKGRP.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(10) TEXT-T06.
    SELECTION-SCREEN POSITION 22.
    SELECT-OPTIONS: S_PROD FOR MARA-MATNR NO INTERVALS.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_RD1A RADIOBUTTON GROUP RD1 DEFAULT 'X'.
    SELECTION-SCREEN POSITION 7.
    SELECTION-SCREEN COMMENT 7(10) TEXT-T07 FOR FIELD P_RD1A. "기말재고
    SELECTION-SCREEN POSITION 25.
    PARAMETERS: P_RD1B RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 28(12) TEXT-T08 FOR FIELD P_RD1B. "예상 기말재고
    SELECTION-SCREEN POSITION 48.
    PARAMETERS: P_RD1C RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 51(08) TEXT-T21 FOR FIELD P_RD1B. "상세조회
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN ULINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK1 AS CHECKBOX.
    SELECTION-SCREEN POSITION 7.
    SELECTION-SCREEN COMMENT 7(30) TEXT-T09 FOR FIELD P_CHK1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK2 AS CHECKBOX.
    SELECTION-SCREEN POSITION 7.
    SELECTION-SCREEN COMMENT 7(32) TEXT-T10 FOR FIELD P_CHK2.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK3 AS CHECKBOX DEFAULT 'X'.
    SELECTION-SCREEN POSITION 7.
    SELECTION-SCREEN COMMENT 24(14) TEXT-T22 FOR FIELD P_CHK3.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK4 AS CHECKBOX.
    SELECTION-SCREEN POSITION 7.
    SELECTION-SCREEN COMMENT 24(30) TEXT-T24 FOR FIELD P_CHK4.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK2.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BERID-LOW.
  ZCL_MM_COMMON=>GET_SEARCH_HELP( EXPORTING IV_FNAME      = 'S_BERID-LOW'
                                            IV_FILT_VAL_A = '02'
                                  IMPORTING EV_VALUE      = S_BERID-LOW  ).

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SELECTION_SCREEN_OUTPUT.
