*&---------------------------------------------------------------------*
*& Include          ZOMM3992SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  "회사코드
  PARAMETERS: P_BUKRS TYPE T001-BUKRS AS LISTBOX OBLIGATORY
                                      VISIBLE LENGTH 23.

  "구매조직
  PARAMETERS: P_EKORG TYPE EBAN-EKORG NO-DISPLAY.

  "업로드 파일
  PARAMETERS: P_FILE TYPE RLGRAP-FILENAME.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN FUNCTION KEY 1.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

*------------------------------
* SMW0 관련
*------------------------------
  CASE SY-UCOMM.
    WHEN 'FC01'.  "양식 다운로드
*U1> 언어키 = ‘EN’ 으로 로그인 경우 영문 Sample File을 다운 - START
*      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID    = GC_OBJID
*                                                      IV_FILENAME = TEXT-U02 ).
      IF SY-LANGU = GC_LANGU_EN.
        ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID    = GC_OBJID_EN
                                                        IV_FILENAME = TEXT-U02 ).
      ELSE.
        ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID    = GC_OBJID
                                                        IV_FILENAME = TEXT-U02 ).
      ENDIF.
*U1> 언어키 = ‘EN’ 으로 로그인 경우 영문 Sample File을 다운 - END

    WHEN OTHERS.
  ENDCASE.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
* 업로드 파일 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  PERFORM FILE_OPEN CHANGING P_FILE.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SET_LIST_BOX.
