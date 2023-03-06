*&---------------------------------------------------------------------*
*& Include          ZRMM3020SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  "회사코드
  PARAMETERS: P_BUKRS TYPE EKKO-BUKRS OBLIGATORY.
*  PARAMETERS: P_BUKRS TYPE EKKO-BUKRS AS LISTBOX VISIBLE LENGTH 23 OBLIGATORY.

  SELECT-OPTIONS: S_WERKS FOR EKPO-WERKS,               "플랜트
                  S_BEDAT FOR EKKO-BEDAT MODIF ID ADT,  "발주일
                  S_BUDAT FOR EKBE-BUDAT MODIF ID BDT,  "전기일
                  S_BSART FOR EKKO-BSART,               "구매문서 유형
                  S_EKGRP FOR EKKO-EKGRP,               "구매그룹
                  S_EBELN FOR EKKO-EBELN,               "구매오더번호
                  S_KONNR FOR EKPO-KONNR,               "계약번호
                  S_MANGE FOR EKKO-ZEMANAGE2,           "관리번호
                  S_PSTYP FOR EKPO-PSTYP,               "품목범주
                  S_KNTTP FOR EKPO-KNTTP,               "계정지정범주
                  S_MATKL FOR EKPO-MATKL,               "자재그룹
                  S_MATNR FOR EKPO-MATNR,               "자재
                  S_ORDER FOR EKKO-ZORDER_PERSON,       "발주담당자
                  S_DEPTO FOR EKKO-ZORDER_DEPARTMENT,   "발주부서
                  S_LIFNR FOR EKKO-LIFNR.               "공급업체

  PARAMETERS: P_KALSK TYPE LFM1-KALSK AS LISTBOX VISIBLE LENGTH 18. "내외자구분

  SELECT-OPTIONS: S_MTART FOR EKPO-MTART,               "자재유형
                  S_BKLAS FOR MBEW-BKLAS.               "평가클래스

  PARAMETERS: P_LOEKZ AS CHECKBOX,  "구매오더 삭제건 포함
              P_EXRET AS CHECKBOX,  "반품/무상 제외
              P_EXSCH AS CHECKBOX.  "자율납품/위탁제외

SELECTION-SCREEN END OF BLOCK BLK1.


SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.

  SELECTION-SCREEN BEGIN OF LINE.

    PARAMETERS P_RA RADIOBUTTON GROUP R1 DEFAULT 'X' USER-COMMAND RAD.
    SELECTION-SCREEN COMMENT (12) TEXT-F01 FOR FIELD P_RA.  "발주일기준

    SELECTION-SCREEN POSITION 20.
    PARAMETERS P_RB RADIOBUTTON GROUP R1.
    SELECTION-SCREEN COMMENT (6) TEXT-F02 FOR FIELD P_RB.   "전기일 기준

*U1> 입고기준/송장기준 검색 기능 추가
    SELECTION-SCREEN POSITION 40.
    PARAMETERS P_GR RADIOBUTTON GROUP R1.
    SELECTION-SCREEN COMMENT (10) TEXT-F03 FOR FIELD P_GR.   "입고기준

    SELECTION-SCREEN POSITION 60.
    PARAMETERS P_IR RADIOBUTTON GROUP R1.
    SELECTION-SCREEN COMMENT (10) TEXT-F04 FOR FIELD P_IR.   "송장기준

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK2.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

* 문서유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-LOW.
  PERFORM SET_F4_BSART USING 'S_BSART-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-HIGH.
  PERFORM SET_F4_BSART USING 'S_BSART-HIGH'.

* 구매그룹 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-HIGH'.

** 발주담당자 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-LOW.
*  PERFORM SET_F4_ORDER USING 'S_ORDER-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-HIGH.
*  PERFORM SET_F4_ORDER USING 'S_ORDER-HIGH'.
*
** 발주부서 F4
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-LOW.
*  PERFORM SET_F4_DEPTO USING 'S_DEPTO-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-HIGH.
*  PERFORM SET_F4_DEPTO USING 'S_DEPTO-HIGH'.
*
**---------------------------------------------------------------------*
** AT SELECTION-SCREEN OUTPUT
**---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.
  PERFORM SET_SEL_SCREEN.
**----------------------------------------------------------------------*
** Screen Parameter Control
**----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM SET_SEL_SCREEN_DATA.
