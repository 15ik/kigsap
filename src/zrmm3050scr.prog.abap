*&---------------------------------------------------------------------*
*& Include          ZRMM3050SCR
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

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (31) TEXT-F02.
    "개요
    PARAMETERS: P_RA RADIOBUTTON GROUP G1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT (10) TEXT-F03 FOR FIELD P_RA.

    "상세
    PARAMETERS: P_RB RADIOBUTTON GROUP G1.
    SELECTION-SCREEN COMMENT (10) TEXT-F04 FOR FIELD P_RB.
  SELECTION-SCREEN END OF LINE.

  "내외자구분
  PARAMETERS: P_KALSK TYPE LFM1-KALSK.
*  PARAMETERS: P_KALSK TYPE LFM1-KALSK AS LISTBOX
*                                      VISIBLE LENGTH 18
*                                      OBLIGATORY
*                                      USER-COMMAND C1.

  "결재상태
*  PARAMETERS: P_WFSTS TYPE ZE_ZWF_STATUS AS LISTBOX
*                                            VISIBLE LENGTH 18
*                                            OBLIGATORY.

  SELECT-OPTIONS: S_LIFNR FOR EKKO-LIFNR,                     "공급업체
                  S_BSART FOR EKKO-BSART,                     "구매문서유형
                  S_EKGRP FOR EKKO-EKGRP,                     "구매그룹
                  S_WERKS FOR EKPO-WERKS,                     "플랜트
                  S_EBELN FOR EKKO-EBELN,                     "계약번호
                  S_KONNR FOR EKPO-KONNR,                     "계약번호
                  S_ORDER FOR EKKO-ZORDER_PERSON,             "발주담당자
                  S_DEPTO FOR EKKO-ZORDER_DEPARTMENT,         "발주부서
                  S_EXPEN FOR EKKO-ZEXPEN_PERSON,             "지출발의담당자
                  S_DEPTE FOR EKKO-ZEXPEN_DEPARTMENT,         "지출발의부서
                  S_MANGE FOR EKKO-ZEMANAGE2 MODIF ID MNG,    "관리번호(KT&G, 외자일 경우만 활성화)
                  S_BEDAT FOR EKKO-BEDAT,                     "발주일
                  S_MATNR FOR EKPO-MATNR,                     "자재
                  S_MATKL FOR EKPO-MATKL,                     "자재그룹
                  S_EPSTP FOR T163Y-EPSTP,                    "품목범주(External)
                  S_KNTTP FOR EKPO-KNTTP.                     "계정지정범주

  PARAMETERS: P_LOEKZ AS CHECKBOX.  "구매오더 삭제건 포함

SELECTION-SCREEN END OF BLOCK BLK2.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

** 문서유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-LOW.
  PERFORM SET_F4_BSART USING 'S_BSART-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-HIGH.
  PERFORM SET_F4_BSART USING 'S_BSART-HIGH'.

* 구매그룹 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-HIGH'.

* 발주담당자 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-LOW.
*  PERFORM SET_F4_PERSN USING 'S_ORDER-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-HIGH.
*  PERFORM SET_F4_PERSN USING 'S_ORDER-HIGH'.
*
** 발주부서 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-LOW.
*  PERFORM SET_F4_DEPAT USING 'S_DEPTO-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-HIGH.
*  PERFORM SET_F4_DEPAT USING 'S_DEPTO-HIGH'.
*
** 지출발의담당자 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPEN-LOW.
*  PERFORM SET_F4_PERSN USING 'S_EXPEN-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPEN-HIGH.
*  PERFORM SET_F4_PERSN USING 'S_EXPEN-HIGH'.
*
** 지출발의부서 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTE-LOW.
*  PERFORM SET_F4_DEPAT USING 'S_DEPTE-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTE-HIGH.
*  PERFORM SET_F4_DEPAT USING 'S_DEPTE-HIGH'.
*
* 품목범주 F4 (External)
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EPSTP-LOW.
  PERFORM SET_F4_EPSTP USING 'S_EPSTP-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EPSTP-HIGH.
  PERFORM SET_F4_EPSTP USING 'S_EPSTP-HIGH'.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.

*----------------------------------------------------------------------*
* Screen Parameter Control
*----------------------------------------------------------------------*

  PERFORM SET_SEL_SCREEN.
