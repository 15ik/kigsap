*&---------------------------------------------------------------------*
*& Include          ZOMM3030SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  "회사코드
  PARAMETERS: P_BUKRS TYPE T001-BUKRS AS LISTBOX
                                      VISIBLE LENGTH 23
                                      OBLIGATORY
                                      MODIF ID EXC.

  "구매조직
  PARAMETERS: P_EKORG TYPE EBAN-EKORG AS LISTBOX
                                      VISIBLE LENGTH 23
                                      OBLIGATORY
                                      MODIF ID EXC.

  "요청자
  PARAMETERS: P_PERSN TYPE ZTMM30010-ZREQUESTER OBLIGATORY
                                                MODIF ID EXC.
  SELECTION-SCREEN COMMENT 46(30) GV_PERNM FOR FIELD P_PERSN.

  "요청부서
  PARAMETERS: P_DEPAT TYPE ZTMM30010-ZPEQ_DEPARTMENT OBLIGATORY
                                                     MODIF ID EXC.
  SELECTION-SCREEN COMMENT 46(30) GV_DEPNM FOR FIELD P_DEPAT.

  "결재상태
  PARAMETERS: P_WFSTS TYPE ZE_ZWF_STATUS AS LISTBOX OBLIGATORY
                                         VISIBLE LENGTH 10.

  "구매요청유형
  SELECT-OPTIONS: S_BSART FOR EBAN-BSART. "NO-EXTENSION.

  "구매요청일
  SELECT-OPTIONS: S_BADAT FOR EBAN-BADAT. "NO-EXTENSION.

  "구매요청번호
  SELECT-OPTIONS: S_BANFN FOR EBAN-BANFN. "NO-EXTENSION.

  "품목 상세보기
  PARAMETERS: P_ITEM AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK BLK1.

PARAMETERS: P_APPR AS CHECKBOX.   "결재상신 바로가기(체크하면 조회된 품목 전체 결재상신 수행)

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN FUNCTION KEY 2.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

  CASE SY-UCOMM.
    WHEN 'FC01'.  "결재선 관리(개인)
      SET PARAMETER ID 'BUK'        FIELD P_BUKRS.
      SET PARAMETER ID 'ZAPVPSTYPE' FIELD GC_APV_TYPE.
      SET PARAMETER ID 'ZAPVERNAM'  FIELD P_PERSN.

      CALL TRANSACTION 'ZOCN0210_MM' AND SKIP FIRST SCREEN.

    WHEN 'FC02'.  "결재선 관리(전체)
      SET PARAMETER ID 'BUK'        FIELD P_BUKRS.
      SET PARAMETER ID 'ZAPVPSTYPE' FIELD GC_APV_TYPE.
      SET PARAMETER ID 'ZAPVERNAM'  FIELD SPACE.

      CALL TRANSACTION 'ZOCN0210_MM' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
  ENDCASE.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*---------------------------------------------------------------------*
* CHECK_AUTHORITY

*AT SELECTION-SCREEN ON P_BUKRS.
*  ZCL_CO_CHECK=>CHECK_AUTHORITY( EXPORTING IV_CASE   = '1'  ).

AT SELECTION-SCREEN ON P_PERSN.
  PERFORM CHECK_PERSN USING P_PERSN.

AT SELECTION-SCREEN ON P_DEPAT.
  PERFORM CHECK_DEPAT USING P_DEPAT.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*

* 구매요청유형 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-LOW.
  PERFORM SET_F4_BSART USING 'S_BSART-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BSART-HIGH.
  PERFORM SET_F4_BSART USING 'S_BSART-HIGH'.

* 발주담당 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PERSN.
  PERFORM SET_F4_PERSN USING 'P_PERSN'.

* 발주부서 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_DEPAT.
  PERFORM SET_F4_DEPAT USING 'P_DEPAT'
                             'GV_DEPNM'.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.

*----------------------------------------------------------------------*
* Screen Parameter Control
*----------------------------------------------------------------------*

  PERFORM SET_SEL_SCREEN.
