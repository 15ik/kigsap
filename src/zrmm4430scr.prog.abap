*&---------------------------------------------------------------------*
*& Include          ZRMM4430SCR
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------
" 기본선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS:     P_BUKRS LIKE T001-BUKRS OBLIGATORY.       "회사코드
  SELECTION-SCREEN COMMENT 40(25) GV_BUTXT FOR FIELD P_BUKRS.
  SELECT-OPTIONS: S_MATNR FOR MARA-MATNR OBLIGATORY,        "자재코드
                  S_CHARG FOR MCH1-CHARG,        "배치번호
                  S_HSDAT FOR MCH1-HSDAT,                   "제조일
                  S_VFDAT FOR MCH1-VFDAT,                   "사용기한
                  S_LICHN FOR MCH1-LICHA,                   "제조처LOT
                  S_LIFNR FOR MCH1-LIFNR,                   "공급업체
                  S_KUNNR FOR KNA1-KUNNR,                   "고객번호
                  S_MAKER FOR AUSP-ATWRT.             "메이커
SELECTION-SCREEN END OF BLOCK BLK1.

"-----------------------------------------------------------------------
" 조회
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B03.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 4.
    PARAMETERS: P_CHK1 AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 6(30) TEXT-T01. "최종 데이터 조회
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK2.

"-----------------------------------------------------------------------
" 출력 Layout
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLKV WITH FRAME TITLE TEXT-B02.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT DEFAULT '/DEFAULT'.
SELECTION-SCREEN END OF BLOCK BLKV.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM VARIANT_F4 USING GS_VARIANT CHANGING P_VAR.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
