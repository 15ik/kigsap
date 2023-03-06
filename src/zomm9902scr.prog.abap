*&---------------------------------------------------------------------*
*& Include          ZOMM9902SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (10) TEXT-F04.
    PARAMETERS:     P_BUKRS TYPE EKKO-BUKRS OBLIGATORY
                                            MEMORY ID BUK
                                            AS LISTBOX VISIBLE LENGTH 20
                                            MODIF ID MST.

    SELECTION-SCREEN COMMENT 44(6) TEXT-F05.
    PARAMETERS:     P_EKORG TYPE EKKO-EKORG OBLIGATORY
                                            MEMORY ID EKO
                                            AS LISTBOX VISIBLE LENGTH 20
                                            MODIF ID MST.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (10) TEXT-F01.
    "국내 PO
    PARAMETERS: P_DOPO RADIOBUTTON GROUP G1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT (15) TEXT-F02 FOR FIELD P_DOPO.

    "수입 PO
    PARAMETERS: P_IMPO RADIOBUTTON GROUP G1.
    SELECTION-SCREEN COMMENT (20) TEXT-F03 FOR FIELD P_IMPO.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT (10) TEXT-F06.
    "업로드 파일
    PARAMETERS: P_FILE TYPE RLGRAP-FILENAME.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK1.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN FUNCTION KEY 2.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

*------------------------------
* SMW0 관련
*------------------------------
  CASE SY-UCOMM.
    WHEN 'FC01'.  "양식 다운로드(국내발주)
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID    = GC_OBJID_DO
                                                      IV_FILENAME = TEXT-U03 ).
    WHEN 'FC02'.  "양식 다운로드(수입발주)
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID    = GC_OBJID_IM
                                                      IV_FILENAME = TEXT-U04 ).

    WHEN OTHERS.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_F4_LISTBOX.
*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
* 업로드 파일 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  PERFORM FILE_OPEN CHANGING P_FILE.
