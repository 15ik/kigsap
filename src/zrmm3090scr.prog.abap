*&---------------------------------------------------------------------*
*& Include          ZRMM3090SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.


  "회사코드
  PARAMETERS: P_BUKRS TYPE T001-BUKRS OBLIGATORY.
*                                      AS LISTBOX
*                                      VISIBLE LENGTH 23
*                                      USER-COMMAND C1
*                                      MODIF ID EXC.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.
  PARAMETERS: P_KALSK TYPE LFM1-KALSK. "내외자구분
*  PARAMETERS: P_KALSK TYPE LFM1-KALSK AS LISTBOX VISIBLE LENGTH 18 OBLIGATORY. "내외자구분

  SELECT-OPTIONS: S_EINDT FOR ZSVCMM_EKET1-EINDT,                    "납품일
                  S_LIFNR FOR ZSVCMM_EKET1-LIFNR ,                   "공급업체
                  S_BSART FOR ZSVCMM_EKET1-BSART,                   "구매문서유형
                  S_EKGRP FOR ZSVCMM_EKET1-EKGRP,                    "구매그룹
                  S_WERKS FOR ZSVCMM_EKET1-WERKS,                    "플랜트
                  S_EBELN FOR ZSVCMM_EKET1-EBELN,                    "구매문서
                  S_ORPSN FOR ZSVCMM_EKET1-ZORDER_PERSON,            "발주담당자
                  S_ORDPT FOR ZSVCMM_EKET1-ZORDER_DEPARTMENT,        "발주 부서
                  S_EXPSN FOR ZSVCMM_EKET1-ZEXPEN_PERSON,            "지출발의 담당자
                  S_EXDPT FOR ZSVCMM_EKET1-ZEXPEN_DEPARTMENT,        "지출발의 부서
                  S_AEDAT FOR ZSVCMM_EKET1-AEDAT,                    "생성일
                  S_MATNR FOR ZSVCMM_EKET1-MATNR,                    "자재
                  S_MATKL FOR ZSVCMM_EKET1-MATKL,                    "자재그룹
                  S_PSTYP FOR ZSVCMM_EKET1-PSTYP,                    "품목범주
                  S_BSTAE FOR ZSVCMM_EKET1-BSTAE,                    "확인관리카
                  S_KNTTP FOR ZSVCMM_EKET1-KNTTP.                    "계정지정범주
  SELECTION-SCREEN skip.
  PARAMETERS : p_chk AS CHECKBOX.     "(+)U2                         " 발주금지 품목만 조회
SELECTION-SCREEN END OF BLOCK BLK2.

SELECTION-SCREEN BEGIN OF BLOCK BLK4 WITH FRAME TITLE TEXT-B04.
  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS P_RP1 RADIOBUTTON GROUP RG1 DEFAULT 'X' USER-COMMAND R1.
    SELECTION-SCREEN COMMENT (15) TEXT-T11 FOR FIELD P_RP1. "전체

    SELECTION-SCREEN POSITION 33.
    PARAMETERS P_RP2 RADIOBUTTON GROUP RG1.
    SELECTION-SCREEN COMMENT (10) TEXT-T12 FOR FIELD P_RP2. "미입고
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK4.

SELECTION-SCREEN BEGIN OF BLOCK BLK03 WITH FRAME TITLE TEXT-B03.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BLK03.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
* 문서유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-LOW.
  PERFORM SET_F4_BSART USING 'S_BSART-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-HIGH.
  PERFORM SET_F4_BSART USING 'S_BSART-HIGH'.

**> 발주자
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORPSN-LOW.
*  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORPSN-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORPSN-HIGH.
*  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORPSN-HIGH'.
*
**> 발주부서
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPT-LOW.
*  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDPT-LOW'
*                                      SPACE.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPT-HIGH.
*  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDPT-HIGH'
*                                      SPACE.
*
**> 지출발의 담당자
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPSN-LOW.
*  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPSN-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPSN-HIGH.
*  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPSN-HIGH'.
*
**> 지출발의부서
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXDPT-LOW.
*  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXDPT-LOW'
*                                      SPACE.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXDPT-HIGH.
*  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXDPT-HIGH'
*                                      SPACE.
*
*> 구매그룹
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_SEL_SCR_EKGRP USING 'S_EKGRP-LOW'
                                      SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_SEL_SCR_EKGRP USING 'S_EKGRP-HIGH'
                                      SPACE.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM VARIANT_F4 USING GS_VARIANT CHANGING P_VAR.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_F4_LISTBOX.
  PERFORM SET_SEL_SCR_OUTPUT.
