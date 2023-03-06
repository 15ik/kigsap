*&---------------------------------------------------------------------*
*& Include          ZRMM5010SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (6) TEXT-S01.
    PARAMETERS:     P_BUKRS TYPE T001-BUKRS OBLIGATORY.
*                                            MEMORY ID BUK
*                                            AS LISTBOX VISIBLE LENGTH 20
*                                            MODIF ID MST.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.
  SELECT-OPTIONS: S_BUPLA FOR RBKP-BUPLA,                 "사업장
*                  S_WFSTS FOR ZTCN00009-APPSTATUS,        "결재상태
                  S_LIFNR FOR RBKP-LIFNR,                 "공급업체
                  S_STCD2 FOR LFA1-STCD2,                 "사업자번호
                  S_WERKS FOR RSEG-WERKS,                 "플랜트
                  S_EKGRP FOR EKKO-EKGRP,                 "구매그룹
                  S_ORPSN FOR EKKO-ZORDER_PERSON,         "발주자
                  S_ORDPT FOR EKKO-ZORDER_DEPARTMENT,     "발주부서
                  S_EXPSN FOR EKKO-ZEXPEN_PERSON,         "지출발의 담당자
                  S_EXDPT FOR EKKO-ZEXPEN_DEPARTMENT,     "지출발의부서
                  S_BLDAT FOR RBKP-BLDAT,                 "세금계산서작성일
                  S_BUDAT FOR RBKP-BUDAT OBLIGATORY,      "전기일자
                  S_BELNR FOR RBKP-BELNR,                 "송장문서
                  S_USNAM FOR RBKP-ERFNAM,                "송장처리자
*                  S_ISSID FOR ZDTV3T_AP_HEAD-ISSUE_ID,    "전자세금계산서 승인번호
                  S_EBELN FOR RSEG-EBELN,                 "구매오더
                  S_MATNR FOR RSEG-MATNR,                 "자재코드
                  S_BSART FOR EKKO-BSART.                 "구매문서유형
SELECTION-SCREEN END OF BLOCK BLK2.


SELECTION-SCREEN BEGIN OF BLOCK BLK03 WITH FRAME TITLE TEXT-B03.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BLK03.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
*> 사업장
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BUPLA-LOW.
  PERFORM SET_F4_SEL_SCR_BUPLA USING 'S_BUPLA-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BUPLA-HIGH.
  PERFORM SET_F4_SEL_SCR_BUPLA USING 'S_BUPLA-HIGH'.

*> 발주자
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORPSN-LOW.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORPSN-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORPSN-HIGH.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORPSN-HIGH'.

*> 발주부서
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPT-LOW.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDPT-LOW'
                                      SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPT-HIGH.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDPT-HIGH'
                                      SPACE.

*> 지출발의 담당자
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPSN-LOW.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPSN-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPSN-HIGH.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPSN-HIGH'.

*> 지출발의부서
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXDPT-LOW.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXDPT-LOW'
                                      SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXDPT-HIGH.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXDPT-HIGH'
                                      SPACE.

*> 구매그룹
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-LOW.
  PERFORM SET_F4_SEL_SCR_EKGRP USING 'S_EKGRP-LOW'
                                      SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EKGRP-HIGH.
  PERFORM SET_F4_SEL_SCR_EKGRP USING 'S_EKGRP-HIGH'
                                      SPACE.

*> 결재상태
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WFSTS-LOW.
*  PERFORM SET_F4_SEL_SCR_WFSTS USING 'S_WFSTS-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_WFSTS-HIGH.
*  PERFORM SET_F4_SEL_SCR_WFSTS USING 'S_WFSTS-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VAR.
  PERFORM VARIANT_F4 USING GS_VARIANT CHANGING P_VAR.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_F4_LISTBOX.
  PERFORM SET_SEL_SCR_OUTPUT.
