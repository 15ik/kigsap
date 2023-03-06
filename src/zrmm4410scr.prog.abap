*&---------------------------------------------------------------------*
*& Include          ZRMM4410SCR
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------
" 기본 선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_BUKRS LIKE T001-BUKRS OBLIGATORY.       "회사코드
  SELECTION-SCREEN COMMENT 40(25) GV_BUTXT FOR FIELD P_BUKRS.
  SELECT-OPTIONS: S_DATE FOR SY-DATLO OBLIGATORY,       "기간
                  S_WERKS FOR T001W-WERKS,              "플랜트
                  S_LGORT FOR T001L-LGORT,              "저장위치
                  S_LIFNR FOR LFA1-LIFNR,               "업체코드
                  S_MATKL FOR MARA-MATKL,               "자재그룹
                  S_MATNR FOR MARA-MATNR,               "자재
                  S_CHARG FOR MCH1-CHARG.               "배치
SELECTION-SCREEN END OF BLOCK BLK1.

"-----------------------------------------------------------------------
" 출력 Layout
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLKV WITH FRAME TITLE TEXT-B02.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BLKV.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM VARIANT_F4 USING GS_VARIANT CHANGING P_VAR.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SELECTION_SCREEN_OUTPUT.
