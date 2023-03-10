*&---------------------------------------------------------------------*
*& Include          ZRMM1020SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  SELECT-OPTIONS: S_WERKS FOR V_MARC_MD-WERKS OBLIGATORY,
                  S_MATNR FOR MARA-MATNR,
                  S_MATKL FOR MARA-MATKL,
                  S_DISPO FOR V_MARC_MD-DISPO,
                  S_EKGRP FOR V_MARC_MD-EKGRP,
                  S_LAEDA FOR MARA-LAEDA,
                  S_MMSTA FOR V_MARC_MD-MMSTA,
                  S_CHKDT FOR SY-DATLO  DEFAULT SY-DATLO OBLIGATORY NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    "구매정보레코드에 생산버전 미 적용 자재만 조회
    PARAMETERS: P_CHK1 AS CHECKBOX.
    SELECTION-SCREEN POSITION 6.
    SELECTION-SCREEN COMMENT (25) TEXT-T01 FOR FIELD P_CHK1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK2.

SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-B03.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    "백그라운드 실행
    PARAMETERS: P_CHK2 AS CHECKBOX.
    SELECTION-SCREEN POSITION 6.
    SELECTION-SCREEN COMMENT (25) TEXT-T02 FOR FIELD P_CHK2.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK3.
