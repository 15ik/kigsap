*&---------------------------------------------------------------------*
*& Include          ZRMM3040SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  SELECTION-SCREEN BEGIN OF LINE.
    "회사코드
    SELECTION-SCREEN COMMENT (10) TEXT-F01.
    PARAMETERS: P_BUKRS TYPE EKKO-BUKRS AS LISTBOX
                                        VISIBLE LENGTH 23
                                        OBLIGATORY
                                        MODIF ID EXC
                                        USER-COMMAND BUK.

    "구매조직
    SELECTION-SCREEN COMMENT 50(10) TEXT-F02.
    PARAMETERS: P_EKORG TYPE EKKO-EKORG AS LISTBOX
                                        VISIBLE LENGTH 23
                                        OBLIGATORY
                                        MODIF ID EXC.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.

  "내외자구분
  PARAMETERS: P_KALSK TYPE LFM1-KALSK AS LISTBOX
                                      VISIBLE LENGTH 18
                                      OBLIGATORY
                                      USER-COMMAND KAL.

  SELECT-OPTIONS: S_LIFNR  FOR ZSVBMMINFOPRICE-LIFNR,                                   "공급업체
                  S_MATNR  FOR ZSVBMMINFOPRICE-MATNR,                                   "자재
                  S_INFNR  FOR ZSVBMMINFOPRICE-INFNR,                                   "구매정보레코드
                  S_ESOKZ  FOR ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY,            "정보레코드범주
                  S_EKGRP  FOR ZSVBMMINFOPRICE-EKGRP,                                   "구매그룹
                  S_WERKS  FOR ZSVBMMINFOPRICE-PLANT,                                   "플랜트
                  S_MATKL  FOR ZSVBMMINFOPRICE-MATKL,                                   "자재그룹
                  S_EXPRF  FOR ZSVBMMINFOPRICE-CONFIRM_PRICE,                           "단가유형
                  S_BSTAE  FOR ZSVBMMINFOPRICE-BSTAE,                                   "납품서생성
                  S_CNTRNO FOR ZTMM30110-CNTR_NO                        MODIF ID DO1,   "계약번호
                  S_ORDER  FOR ZTMM30110-ZORDER_PERSON                  MODIF ID DO1,   "계약담당자
                  S_DEPTO  FOR ZTMM30110-ZORDER_DEPARTMENT              MODIF ID DO1,   "계약부서
                  S_BEDAT  FOR ZTMM30110-BEDAT                          MODIF ID DO1,   "계약일자
                  S_DATAB  FOR ZSVBMMINFOPRICE-CONDITIONVALIDITYSTARTDATE MODIF ID DO1,   "계약시작일
                  S_DATBI  FOR ZSVBMMINFOPRICE-CONDITIONVALIDITYENDDATE MODIF ID DO1.   "계약만료일

  PARAMETERS: P_VALID AS CHECKBOX MODIF ID DO1,  "유효단가품목(Only)
              P_ZPRI  AS CHECKBOX MODIF ID DO2.  "인쇄교체비(Only)

SELECTION-SCREEN END OF BLOCK BLK2.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

* 구매그룹 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-HIGH'.

* 단가유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPRF-LOW.
  PERFORM SET_F4_EXPRF USING 'S_EXPRF-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPRF-HIGH.
  PERFORM SET_F4_EXPRF USING 'S_EXPRF-HIGH'.

* 계약담당자 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-LOW.
  PERFORM SET_F4_PERSN USING 'S_ORDER-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDER-HIGH.
  PERFORM SET_F4_PERSN USING 'S_ORDER-HIGH'.

* 계약부서 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-LOW.
  PERFORM SET_F4_DEPAT USING 'S_DEPTO-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DEPTO-HIGH.
  PERFORM SET_F4_DEPAT USING 'S_DEPTO-HIGH'.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.

*----------------------------------------------------------------------*
* Screen Parameter Control
*----------------------------------------------------------------------*

  PERFORM SET_SEL_SCREEN.
