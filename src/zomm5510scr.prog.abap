*&---------------------------------------------------------------------*
*& Include          ZOMM5510SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  "회사코드
  PARAMETERS: P_BUKRS TYPE T001-BUKRS AS LISTBOX
                                      VISIBLE LENGTH 23
                                      OBLIGATORY
                                      MODIF ID EXC
                                      USER-COMMAND BUK.

  "구매조직
  PARAMETERS: P_EKORG TYPE EKKO-EKORG AS LISTBOX
                                      VISIBLE LENGTH 23
                                      OBLIGATORY
                                      MODIF ID EXC.

  "송장처리자
  PARAMETERS: P_PERSN TYPE RBKP-USNAM OBLIGATORY
                                      MODIF ID EXC.
  SELECTION-SCREEN COMMENT 46(30) GV_PERNM FOR FIELD P_PERSN.


  SELECT-OPTIONS: S_LIFNR FOR EKKO-LIFNR,               "공급업체
                  S_EKGRP FOR EKKO-EKGRP,               "구매그룹
                  S_BUDAT FOR EKBE-BUDAT,               "송장전기일
                  S_ORDER FOR EKKO-ZORDER_PERSON,       "발주담당자
                  S_DEPTO FOR EKKO-ZORDER_DEPARTMENT,   "발주부서
                  S_EXPEN FOR EKKO-ZEXPEN_PERSON,       "지출발의담당자
                  S_DEPTE FOR EKKO-ZEXPEN_DEPARTMENT,   "지출발의부서
                  S_EBELN FOR EKKO-EBELN,               "구매오더번호
                  S_BSART FOR EKKO-BSART.               "구매문서 유형

SELECTION-SCREEN END OF BLOCK BLK1.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*
* CHECK_AUTHORITY

AT SELECTION-SCREEN ON P_PERSN.
  PERFORM CHECK_PERSN USING P_PERSN CHANGING GV_PERNM.

**---------------------------------------------------------------------*
** AT SELECTION-SCREEN ON VALUE-REQUEST FOR
**---------------------------------------------------------------------*
*
** 송장처리자 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PERSN.
  PERFORM SET_F4_PERSN USING 'P_PERSN'.

* 구매그룹 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_EKGRP USING 'S_EKGRP-HIGH'.
*
** 발주담당자 F4
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
* 문서유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-LOW.
  PERFORM SET_F4_BSART USING 'S_BSART-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-HIGH.
  PERFORM SET_F4_BSART USING 'S_BSART-HIGH'.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.

*----------------------------------------------------------------------*
* Screen Parameter Control
*----------------------------------------------------------------------*

  PERFORM SET_SEL_SCREEN.
