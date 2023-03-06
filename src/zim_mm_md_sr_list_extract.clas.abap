class ZIM_MM_MD_SR_LIST_EXTRACT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MD_SR_LIST_EXTRACT .
protected section.
private section.
ENDCLASS.



CLASS ZIM_MM_MD_SR_LIST_EXTRACT IMPLEMENTATION.


  method IF_EX_MD_SR_LIST_EXTRACT~EXTRACT_SR_LIST_INFOS.
*&---------------------------------------------------------------------*
*& Module      : MM
*& 생성자      :
*& 생성일      :
*& Description : MRP,LTP 데이타 집계 BADI
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
***************************************************************************************************************
*1. Z001 (플랜트-모든자재)
*   ZTMM2001H 삭제(코드마스터) -> ZTMM20010 삭제 -> ZTMM20010 등록 -> ZTMM2001H 등록
*  * 플랜트 별 1회 삭제
*
*2. Z002 (플랜트-입력자재)
*   ZTMM20010 삭제 -> ZTMM20010 등록 -> ZTMM2001H 등록
*  * 플랜트-자재별 삭제
*
*3. Z003 (플랜트-모든자재)
*   ZTMM2002H 삭제(코드마스터) -> ZTMM20020 삭제 -> ZTMM20020 등록 -> ZTMM2002H 등록
*  * 플랜트 별 1회 삭제
*
*4. Z004 (플랜트-입력자재)
*   ZTMM20020 삭제 -> ZTMM20020 등록 -> ZTMM2002H 등록
*  * 플랜트-자재별 삭제
***************************************************************************************************************

   DATA: LS_M20010 TYPE ZTMM20010,
         LT_M20010 TYPE TABLE OF ZTMM20010,
         LS_M2001H TYPE ZTMM2001H,
         LT_M2001H TYPE TABLE OF ZTMM2001H,
         LS_M20020 TYPE ZTMM20020,
         LT_M20020 TYPE TABLE OF ZTMM20020,
         LS_M2002H TYPE ZTMM2002H,
         LT_M2002H TYPE TABLE OF ZTMM2002H,
         LS_MDPSX  TYPE MDPS.

   DATA: LV_INDEX    TYPE I,
         LV_DAYS     TYPE T5A4A-DLYDY,
         LV_MONTHS   TYPE T5A4A-DLYMO,
         LV_YEARS    TYPE T5A4A-DLYYR,
         LV_DEL_DATE TYPE SY-DATLO.

   DATA: LT_CFG_MRP TYPE TABLE OF ZTMM00002, LT_CFG_LTP TYPE TABLE OF ZTMM00002.
*
   STATICS: SV_CLEANUP_DONE TYPE BOOLEAN,
            SV_IV_WERKS     TYPE RANGE OF WERKS_D,
            SV_MAX_SEQ      TYPE ZE_MMDSEQ.

   CONSTANTS : LC_Z001 TYPE MD_EXMOD VALUE 'Z001', "MRP 데이타집계  ZTMM20010, ZTMM2001H 신규생성
               LC_Z002 TYPE MD_EXMOD VALUE 'Z002', "MRP 데이타집계  ZTMM2001H 차수증가, ZTMM20010 Modify
               LC_Z003 TYPE MD_EXMOD VALUE 'Z003', "LTP 데이터 집계 ZTMM20020, ZTMM2002H 신규생성
               LC_Z004 TYPE MD_EXMOD VALUE 'Z004'. "LTP 데이터 집계 ZTMM2002H 차수증가, ZTMM20020 Modify

*-
   CLEAR  : LT_M20010, LT_M20020, LT_M20010[], LT_M20020[], LT_M2001H, LT_M2002H, LT_M2001H[], LT_M2002H[],
            LT_CFG_MRP, LT_CFG_MRP[], LT_CFG_LTP, LT_CFG_LTP[].

   CHECK IV_EXMOD = LC_Z001 OR IV_EXMOD = LC_Z002 OR IV_EXMOD = LC_Z003 OR IV_EXMOD = LC_Z004.

***----------------------------------------------------------------------------------------------------------***
***--> Cleanup
***----------------------------------------------------------------------------------------------------------***
   SORT SV_IV_WERKS BY LOW.
   READ TABLE SV_IV_WERKS INTO DATA(LS_WERKS) WITH KEY LOW = IV_WERKS BINARY SEARCH.
   IF SY-SUBRC NE 0.
     CLEAR SV_CLEANUP_DONE.
   ENDIF.

********************************************************************************************************************************
   IF IV_EXMOD = LC_Z001 AND SV_CLEANUP_DONE IS INITIAL.
********************************************************************************************************************************
     "초기화 및 HISTORY 삭제 : 이전 데이타 삭제
     ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00014' )
                             IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'MRP' ) )
                             IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
     LT_CFG_MRP[] = CORRESPONDING #( LT_CONFIG[] ).
     READ TABLE LT_CFG_MRP INTO DATA(LS_CFG_MRP) INDEX 1.

     IF LS_CFG_MRP-FIELD2 = 'Y' AND LS_CFG_MRP-FIELD3 IS NOT INITIAL.
       LV_MONTHS = LS_CFG_MRP-FIELD3. CLEAR LV_DEL_DATE.
       CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
           DATE      = SY-DATLO  "로컬일자
           DAYS      = LV_DAYS
           MONTHS    = LV_MONTHS
           SIGNUM    = '-'
           YEARS     = LV_YEARS
         IMPORTING
           CALC_DATE = LV_DEL_DATE.

       "History -> 이전 데이타 삭제        "
       SELECT SPTAG, DASEQ, PLSCN, MATNR, WERKS, BERID, SEQNO
              FROM ZTMM2001H WHERE SPTAG < @LV_DEL_DATE AND WERKS = @IV_WERKS INTO CORRESPONDING FIELDS OF TABLE @LT_M2001H.
       IF LT_M2001H[] IS NOT INITIAL.
         DELETE ZTMM2001H FROM TABLE LT_M2001H.
         CALL FUNCTION 'DB_COMMIT'.
       ENDIF.

     ENDIF.

     "Z001 일때만 ZTMM20010 / 1H 삭제
     SELECT PLSCN, MATNR, WERKS, BERID, SEQNO FROM ZTMM20010
    INTO CORRESPONDING FIELDS OF TABLE @LT_M20010 WHERE WERKS = @IV_WERKS.

     SV_IV_WERKS[] = VALUE #( BASE SV_IV_WERKS[] ( LOW = IV_WERKS ) ).
     SV_CLEANUP_DONE = 'X'.

********************************************************************************************************************************
   ELSEIF IV_EXMOD = LC_Z002.
********************************************************************************************************************************
     SELECT PLSCN, MATNR, WERKS, BERID, SEQNO FROM ZTMM20010
      INTO CORRESPONDING FIELDS OF TABLE @LT_M20010 WHERE MATNR = @IV_MATNR AND WERKS = @IV_WERKS.

********************************************************************************************************************************
   ELSEIF IV_EXMOD = LC_Z003 AND SV_CLEANUP_DONE IS INITIAL.
********************************************************************************************************************************
     "초기화 및 HISTORY 삭제 : 이전 데이타 삭제
     ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00014' )
                             IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'LTP' ) )
                             IMPORTING ET_OUTTAB = LT_CONFIG ).
     LT_CFG_LTP[] = CORRESPONDING #( LT_CONFIG[] ).
     READ TABLE LT_CFG_LTP INTO DATA(LS_CFG_LTP) INDEX 1.

     IF LS_CFG_MRP-FIELD2 = 'Y' AND LS_CFG_MRP-FIELD3 IS NOT INITIAL.
       LV_MONTHS = LS_CFG_MRP-FIELD3. CLEAR LV_DEL_DATE.
       CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
           DATE      = SY-DATLO  "로컬일자
           DAYS      = LV_DAYS
           MONTHS    = LV_MONTHS
           SIGNUM    = '-'
           YEARS     = LV_YEARS
         IMPORTING
           CALC_DATE = LV_DEL_DATE.

       "History -> 이전 데이타 삭제        "
       SELECT SPTAG, DASEQ, PLSCN, MATNR, WERKS, BERID
              FROM ZTMM2002H WHERE SPTAG < @LV_DEL_DATE AND WERKS = @IV_WERKS AND PLSCN = @IV_PLSCN
              INTO CORRESPONDING FIELDS OF TABLE @LT_M2002H.
       IF LT_M2002H[] IS NOT INITIAL.
         DELETE ZTMM2002H FROM TABLE LT_M2002H.
         CALL FUNCTION 'DB_COMMIT'.
       ENDIF.

     ENDIF.

     "Z003 일때만 ZTMM20020 / 2H 삭제
     SELECT PLSCN, MATNR, WERKS, BERID, SEQNO FROM ZTMM20020
      INTO CORRESPONDING FIELDS OF TABLE @LT_M20020 WHERE WERKS = @IV_WERKS AND PLSCN = @IV_PLSCN.

     SV_IV_WERKS[] = VALUE #( BASE SV_IV_WERKS[] ( LOW = IV_WERKS ) ).
     SV_CLEANUP_DONE = 'X'.

********************************************************************************************************************************
   ELSEIF IV_EXMOD = LC_Z004.
********************************************************************************************************************************
     SELECT PLSCN, MATNR, WERKS, BERID, SEQNO FROM ZTMM20020 WHERE PLSCN = @IV_PLSCN AND MATNR = @IV_MATNR AND WERKS = @IV_WERKS
     INTO CORRESPONDING FIELDS OF TABLE @LT_M20020.

   ENDIF.
***----------------------------------------------------------------------------------------------------------***
   "Z001
   IF LT_M20010[] IS NOT INITIAL.
     DELETE ZTMM20010 FROM TABLE LT_M20010.
     CALL FUNCTION 'DB_COMMIT'.
   ENDIF.
   "Z003
   IF LT_M20020[] IS NOT INITIAL.
     DELETE ZTMM20020 FROM TABLE LT_M20020.
     CALL FUNCTION 'DB_COMMIT'.
   ENDIF.

*-
   IF     SV_MAX_SEQ IS INITIAL AND ( IV_EXMOD = LC_Z001 OR IV_EXMOD = LC_Z002 ).
     SELECT SINGLE MAX( DASEQ ) FROM ZTMM2001H  WHERE SPTAG = @SY-DATLO INTO @SV_MAX_SEQ.
     SV_MAX_SEQ = SV_MAX_SEQ + 1.

   ELSEIF SV_MAX_SEQ IS INITIAL AND ( IV_EXMOD = LC_Z003 OR IV_EXMOD = LC_Z004 ).
     SELECT SINGLE MAX( DASEQ ) FROM ZTMM2002H  WHERE SPTAG = @SY-DATLO INTO @SV_MAX_SEQ.
     SV_MAX_SEQ = SV_MAX_SEQ + 1.
   ENDIF.

***----------------------------------------------------------------------------------------------------------***
***- 생성 및 Modify ==> Z001, Z002
***----------------------------------------------------------------------------------------------------------***

   IF     IV_EXMOD = LC_Z001 OR IV_EXMOD = LC_Z002.

     CLEAR : LS_M20010, LV_INDEX, LS_M2001H.
     CLEAR : LT_M20010, LT_M20010[], LT_M2001H, LT_M2001H[].

     LS_M20010-MANDT = SY-MANDT.
     LS_M20010-PLSCN = IV_PLSCN.  "Planning Object
     LS_M20010-MATNR = IS_MDKP-MATNR.
     LS_M20010-WERKS = IS_MDKP-PLWRK.
     LS_M20010-BERID = IS_CM61B-BERID.

     LOOP AT IT_MDPSX INTO LS_MDPSX.

       LV_INDEX = LV_INDEX + 1.
       LS_M20010-SEQNO = LV_INDEX.   "Key field for Documents
       LS_M20010-DAT00 = LS_MDPSX-DAT00.
       LS_M20010-DELNR = LS_MDPSX-DELNR.
       LS_M20010-DELPS = LS_MDPSX-DELPS.
       LS_M20010-DELET = LS_MDPSX-DELET.
       MOVE-CORRESPONDING LS_MDPSX TO LS_M20010.

       LS_M20010-ERDAT = LS_M20010-AEDAT = SY-DATUM.
       LS_M20010-ERZET = LS_M20010-AEZET = SY-UZEIT.
       LS_M20010-ERNAM = LS_M20010-AENAM = SY-UNAME.

       APPEND LS_M20010 TO LT_M20010.

       "HISTORY TABLE
       MOVE-CORRESPONDING LS_M20010 TO LS_M2001H.
       LS_M2001H-SPTAG = SY-DATLO.
       LS_M2001H-DASEQ = SV_MAX_SEQ.

       APPEND LS_M2001H TO LT_M2001H.

     ENDLOOP.

     IF LT_M20010[] IS NOT INITIAL.
       MODIFY ZTMM20010 FROM TABLE LT_M20010.
       CALL FUNCTION 'DB_COMMIT'.
     ENDIF.

     IF LT_M2001H[] IS NOT INITIAL.
       INSERT ZTMM2001H FROM TABLE LT_M2001H.
       CALL FUNCTION 'DB_COMMIT'.
     ENDIF.

***----------------------------------------------------------------------------------------------------------***
***- 생성 및 Modify ==> Z003, Z004
***----------------------------------------------------------------------------------------------------------***

   ELSEIF IV_EXMOD = LC_Z003 OR IV_EXMOD = LC_Z004.

     CLEAR : LS_M20020, LV_INDEX, LS_M2002H.
     CLEAR : LT_M20020, LT_M20020[], LT_M2002H, LT_M2002H[].

     LS_M20020-MANDT = SY-MANDT.
     LS_M20020-PLSCN = IV_PLSCN.  "Planning Object
     LS_M20020-MATNR = IV_MATNR.
     LS_M20020-WERKS = IS_MDKP-PLWRK.
     LS_M20020-BERID = IS_CM61B-BERID.

     LOOP AT IT_MDPSX INTO LS_MDPSX.

       LV_INDEX = LV_INDEX + 1.
       LS_M20020-SEQNO = LV_INDEX.   "Key field for Documents
       LS_M20020-DAT00 = LS_MDPSX-DAT00.
       LS_M20020-DELNR = LS_MDPSX-DELNR.
       LS_M20020-DELPS = LS_MDPSX-DELPS.
       LS_M20020-DELET = LS_MDPSX-DELET.
       MOVE-CORRESPONDING LS_MDPSX TO LS_M20020.

       LS_M20020-ERDAT = LS_M20020-AEDAT = SY-DATUM.
       LS_M20020-ERZET = LS_M20020-AEZET = SY-UZEIT.
       LS_M20020-ERNAM = LS_M20020-AENAM = SY-UNAME.

       APPEND LS_M20020 TO LT_M20020.

       "HISTORY TABLE
       MOVE-CORRESPONDING LS_M20020 TO LS_M2002H.
       LS_M2002H-SPTAG = SY-DATLO.
       LS_M2002H-DASEQ = SV_MAX_SEQ.

       APPEND LS_M2002H TO LT_M2002H.

     ENDLOOP.

     IF LT_M20020[] IS NOT INITIAL.
       MODIFY ZTMM20020 FROM TABLE LT_M20020.
       CALL FUNCTION 'DB_COMMIT'.
     ENDIF.

     IF LT_M2002H[] IS NOT INITIAL.
       INSERT ZTMM2002H FROM TABLE LT_M2002H.
       CALL FUNCTION 'DB_COMMIT'.
     ENDIF.

   ENDIF.

  endmethod.
ENDCLASS.
