*&---------------------------------------------------------------------*
*& Include          ZRMM4400SCR
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------
" 기본 선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_BUKRS LIKE T001-BUKRS OBLIGATORY.     "회사코드
  SELECTION-SCREEN COMMENT 40(25) GV_BUTXT FOR FIELD P_BUKRS.
  SELECT-OPTIONS: S_WERKS FOR T001W-WERKS,            "플랜트
                  S_LGORT FOR T001L-LGORT MODIF ID SC2,             "저장위치
                  S_EKGRP FOR MARC-EKGRP,                           "구매그룹
                  S_DISPO FOR T024D-DISPO,                          "MRP 관리자
                  S_LIFNR FOR LFA1-LIFNR  MODIF ID SC1,             "업체코드
                  S_MATKL FOR MARA-MATKL,                           "자재그룹
                  S_MATNR FOR MARA-MATNR,                           "자재
                  S_CHARG FOR MCH1-CHARG.                           "배치
*                  S_BKLAS FOR MBEW-BKLAS.                           "평가클래스 *U11 개발
SELECTION-SCREEN END OF BLOCK BLK1.


"-----------------------------------------------------------------------
" 추가 선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    SELECTION-SCREEN COMMENT (10) TEXT-T01. "잔여 유효기간
    PARAMETERS: P_MHDRZ LIKE MARA-MHDRZ.
    SELECTION-SCREEN POSITION 18.
    SELECTION-SCREEN COMMENT (5) TEXT-T02. "(일)
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK2.


"-----------------------------------------------------------------------
" 작업 구분
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-B03.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD1A RADIOBUTTON GROUP RD1 DEFAULT 'X' USER-COMMAND RA.
    SELECTION-SCREEN COMMENT 4(11) TEXT-T03 FOR FIELD P_RD1A. "Total 재고
    SELECTION-SCREEN POSITION 18.
    PARAMETERS: P_RD1B RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 24(10) TEXT-T04 FOR FIELD P_RD1B. "저장위치 재고
    SELECTION-SCREEN POSITION 35.
    PARAMETERS: P_RD1C RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 36(15) TEXT-T05 FOR FIELD P_RD1C. "업체 SC 재고
    SELECTION-SCREEN POSITION 54.
    PARAMETERS: P_RD1D RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 55(21) TEXT-T06 FOR FIELD P_RD1D.  "업체 위탁 재고
    SELECTION-SCREEN POSITION 78.
    PARAMETERS: P_RD1E RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 79(21) TEXT-T09 FOR FIELD P_RD1E.  "고객 판매 재고
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK3.

"-----------------------------------------------------------------------
" 출력 Layout
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLKV WITH FRAME TITLE TEXT-B04.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BLKV.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF SY-UCOMM = GC_RA.
    _G_INIT : S_WERKS, S_LGORT, S_LIFNR, S_MATNR, S_CHARG, S_EKGRP, S_DISPO, S_MATKL.
    CLEAR : P_MHDRZ, P_VAR.
  ENDIF.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*
* CHECK_AUTHORITY

AT SELECTION-SCREEN ON P_BUKRS.
  PERFORM BUKRS_CHECK USING P_BUKRS.

AT SELECTION-SCREEN ON S_WERKS.
  PERFORM WERKS_CHECK USING S_WERKS-LOW.
  PERFORM WERKS_CHECK USING S_WERKS-HIGH.

AT SELECTION-SCREEN ON S_LGORT.
  PERFORM LGORT_CHECK USING S_LGORT-LOW.
  PERFORM LGORT_CHECK USING S_LGORT-HIGH.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WERKS-LOW.
  PERFORM VARIANT_F4_1000 USING 'WERKS' 'GV_NAME1' 'LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WERKS-HIGH.
  PERFORM VARIANT_F4_1000 USING 'WERKS' 'GV_NAME1' 'HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGORT-LOW.
  PERFORM VARIANT_F4_1000 USING 'LGORT' 'GV_LGOBE' 'LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGORT-HIGH.
  PERFORM VARIANT_F4_1000 USING 'LGORT' 'GV_LGOBE' 'HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-LOW.
  PERFORM VARIANT_F4_1000 USING 'DISPO' 'GV_DISPO' 'LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-HIGH.
  PERFORM VARIANT_F4_1000 USING 'DISPO' 'GV_DISPO' 'HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM VARIANT_F4 USING GS_VARIANT CHANGING P_VAR.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SELECTION_SCREEN_OUTPUT.

  IF P_BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT FROM T001 INTO @GV_BUTXT
    WHERE BUKRS = @P_BUKRS.
  ENDIF.
