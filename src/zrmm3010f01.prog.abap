*&---------------------------------------------------------------------*
*& Include          ZRMM3010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
FORM INITIALIZATION.

* 예외처리 유저 점검

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-REPID )
                                                        ( FIELD = 2 VALUE = 'EX01' )
                                                        ( FIELD = 3 VALUE = SY-UNAME ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EXC_USER = 'X'.
  ENDIF.

* 사용자 기본값
*  IF GV_EXC_USER IS INITIAL.
  PERFORM SET_INIT_VALUES.
*  ENDIF.

* PR 상태 기본값
  P_WFSTS = GC_ALL.

* 요청일 기본값 (1개월 전)
  DATA: LV_SDATE TYPE SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 1
      YEARS     = 0
    IMPORTING
      CALC_DATE = LV_SDATE.

  S_BADAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATLO ) ).

* 삭제지시자 기본값
  S_LOEKZ[] = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '' ) ).

*  생성지시자 기본값(MRP생성 건 제외)
  S_ESTKZ[] = VALUE #( ( SIGN = 'I' OPTION = 'NE' LOW = 'B' ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM SET_INIT_VALUES.

* 회사코드 기본값
*  SELECT SINGLE COMPANY, COMPANY_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE USER_ID EQ @SY-UNAME
*    INTO @DATA(LS_USER_INFO).
*
*  IF SY-SUBRC EQ 0.
*    P_BUKRS = LS_USER_INFO-COMPANY.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_authority .

*  CHECK P_BUKRS IS NOT INITIAL.

*U3> 권한 설정.
* 권한체크 룰
*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
**     IV_USER  = SY-UNAME.
**     IV_OBJECT                   = 'ZMM_COMMON'
*      IV_BUKRS = P_BUKRS.
*     IV_EKORG                    =
*     IV_LGORT                    =
*     IV_WERKS                    =
*     IV_ZEXSPA                   =
*     IV_ZPODEP                   =
*     IV_ZPRDEP                   =
* TABLES
*     IT_BUKRS                    =
*     IT_EKORG                    =
*     IT_LGORT                    =
*     IT_WERKS                    =
*     IT_ZEXSPA                   =
*     IT_ZPODEP                   =
*     IT_ZPRDEP                   =
* EXCEPTIONS
*     NO_ID_DATA_FOUND            = 1
*     AUTHORIZATION_FAILURE       = 2
*     NO_INPUT_AUTH_VALUE         = 3
*     NO_DATA_FOUND               = 4
*     MANDATORYFIELDISMISS        = 5
*     OTHERS  = 6
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA: LR_WFSTS    TYPE RANGE OF ZSVCMMPR_RELEASE-WF_STAUS.
*        LR_URGEN    TYPE RANGE OF ZSVCMMPR_RELEASE-ZURGENT,
*        LR_PREIN    TYPE RANGE OF ZSVCMMPR_RELEASE-ZPRE_INPUT,
*        LR_ZRECEIPT TYPE RANGE OF ZSVCMMPR_RELEASE-ZRECEIPT.

  CONSTANTS LC_ETENR TYPE EKET-ETENR VALUE '0001'.


  PERFORM GET_APP_STATUS_BY_SPRAS.
*--------------------------------
* 검색조건 설정
*--------------------------------
* PR 상태 검색조건 설정
  IF P_WFSTS IS INITIAL OR P_WFSTS EQ GC_ALL.
    CLEAR LR_WFSTS.
  ELSE.

    READ TABLE GT_WF_STATUS INTO DATA(LS_WF_STATUS)
                            WITH KEY NAME = P_WFSTS
                            BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LR_WFSTS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = LS_WF_STATUS-CODE ) ).
    ELSE.
      LR_WFSTS = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_WFSTS ) ).
    ENDIF.

  ENDIF.

* 구매조직 기본값
  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 2 VALUE = P_BUKRS ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).


  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EKORG = LS_CONFIG-FIELD4.
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
  CLEAR GT_DISP.

  SELECT A~WF_STAUS,
         B~BUKRS,
         A~BSART AS BSART_PR,
         A~BANFN,
         A~BNFPO,
         A~MATNR,
         A~TXZ01,
         A~MENGE AS MENGE_PR,
         A~MEINS AS MEINS_PR,
         A~PSTYP,
         A~BSMNG,
         A~GR_QTY,
         A~WERKS,
         C~WF_STATUS,
         C~BSTYP AS BSTYP_PO,
         C~BSART AS BSART_PO,
         C~EBELN,
         C~EBELP,
         C~MENGE AS MENGE_PO,
         C~MEINS AS MEINS_PO,
         C~NETPR,
         C~WAERS,
         C~SUM_GRQTY,
         A~BADAT,
*         A~ZPEQ_DEPARTMENT,
*         A~ZREQUESTER,
*         A~ZPRTITLE,
         A~BATXT,
         A~PURREQNITEMCURRENCY AS PUR_CURR,
         A~PREIS,
         A~PEINH,
         A~LFDAT,
         A~PLANTNAME,
         A~LGORT,
         A~LGOBE,
         A~LIFNR,
         E~NAME1 AS LIFNR_TEXT,
         A~FLIEF,
         F~NAME1 AS FLIEF_TEXT,
         A~EKGRP,
         A~EKNAM,
*         A~ZORDER_PERSON,
*         A~ZORDER_DEPARTMENT,
         A~MATKL,
         A~MATERIALGROUPNAME AS MATKL_NAME,
*         A~ZNOPRICE,
*         A~ZURGENT,
*         A~ZPRE_INPUT,
         D~SALESORDER,
         D~SALESORDERITEM,
         D~GLACCOUNT,
         D~COSTCENTER,
         D~MASTERFIXEDASSET AS FIXEDASSET,
         D~ORDERID,
         D~WBSELEMENT
*    ,
*         CASE WHEN A~ZRECEIPT = 'S' THEN @ICON_LED_GREEN END AS ZRECEIPT
    FROM ZSVCMMPR_RELEASE AS A INNER JOIN T001K AS B
                                       ON B~BWKEY = A~WERKS
                                LEFT JOIN ZSVCMM_POSCHEDU AS C
                                       ON C~BANFN = A~BANFN
                                      AND C~BNFPO = A~BNFPO
*                                      AND C~ETENR = @LC_ETENR
                                      AND C~LOEKZ = @SPACE
                                LEFT JOIN CPRACCTASSGMT AS D
                                       ON D~PURCHASEREQUISITIONITEM = A~BNFPO
                                      AND D~PURCHASEREQUISITION     = A~BANFN
                                LEFT JOIN LFA1 AS E
                                       ON E~LIFNR = A~LIFNR
                                LEFT JOIN LFA1 AS F
                                       ON F~LIFNR = A~FLIEF
   WHERE B~BUKRS            = @P_BUKRS
     AND A~WF_STAUS        IN @LR_WFSTS
     AND A~BSART           IN @S_BSART
     AND A~BADAT           IN @S_BADAT
     AND A~BANFN           IN @S_BANFN
     AND A~MATNR           IN @S_MATNR
*     AND A~ZPEQ_DEPARTMENT IN @S_RQDEPT
*     AND A~ZREQUESTER      IN @S_RQUSER
     AND A~PSTYP           IN @S_PSTYP
     AND A~KNTTP           IN @S_KNTTP
     AND A~EKGRP           IN @S_EKGRP
     AND A~WERKS           IN @S_WERKS
     AND A~DISPO           IN @S_DISPO
     AND A~MATKL           IN @S_MATKL
     AND A~ESTKZ           IN @S_ESTKZ
*     AND A~ZURGENT         IN @LR_URGEN
*     AND A~ZPRE_INPUT      IN @LR_PREIN
     AND A~EBAKZ           IN @S_EBAKZ
     AND A~FIXKZ           IN @S_FIXKZ
     AND A~LOEKZ           IN @S_LOEKZ
*     AND COALESCE( A~ZRECEIPT, 'N' ) IN @LR_ZRECEIPT
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISP.

  SORT GT_DISP BY BSART_PR BANFN BNFPO.

* 개요보기 선택 시
  IF P_RA = 'X'.
    DELETE ADJACENT DUPLICATES FROM GT_DISP COMPARING BSART_PR BANFN BNFPO.
  ENDIF.

*--------------------------------
* TEXT 및 기타 Data
*--------------------------------
  PERFORM GET_OTHER_DATA.

*--------------------------------
* 첨부파일 Data
*--------------------------------
  PERFORM GET_ATTACH_DATA.

*--------------------------------
* 전자결재 Data
*--------------------------------
*  PERFORM GET_APV_DATA.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_APP_STATUS_BY_SPRAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_APP_STATUS_BY_SPRAS .

  CLEAR: GT_WF_STATUS, GS_WF_STATUS.

  APPEND VALUE #( CODE = GC_WFSTS_1 NAME = TEXT-L01 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_2 NAME = TEXT-L02 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_3 NAME = TEXT-L03 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_4 NAME = TEXT-L04 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_5 NAME = TEXT-L05 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_6 NAME = TEXT-L06 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_7 NAME = TEXT-L07 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_8 NAME = TEXT-L08 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_9 NAME = TEXT-L09 ) TO GT_WF_STATUS.
  APPEND VALUE #( CODE = GC_WFSTS_10 NAME = TEXT-L10 ) TO GT_WF_STATUS.

  SORT GT_WF_STATUS BY NAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OTHER_DATA
*&---------------------------------------------------------------------*
FORM GET_OTHER_DATA.

*  CLEAR: GT_USER_INFO, GT_COND, GT_DEPT_INFO, GT_MRO.
*
** User Info.
*  SELECT USER_ID,
*         EMPLOY_NO,
*         EMPLOY_NAME,
*         DEPARTMENT,
*         DEPART_NAME
*    INTO CORRESPONDING FIELDS OF TABLE @GT_USER_INFO
*    FROM ZSVMM_USER_INFO
*   WHERE COMPANY = @P_BUKRS.
*
*  SORT GT_USER_INFO BY EMPLOY_NO.
*
** 부서정보
*  SELECT ORGN_CD, ORGN_NM
*    FROM ZTCN00001
*   WHERE BUKRS = @P_BUKRS
*    INTO CORRESPONDING FIELDS OF TABLE @GT_DEPT_INFO.
*  SORT GT_DEPT_INFO BY ORGN_CD.
*
** 유효단가 유무 체크
*  DATA(LT_FAE) = GT_DISP.
*  SORT LT_FAE BY MATNR.
*  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING MATNR.
*
*  IF LT_FAE IS NOT INITIAL.
*    SELECT MATNR
*      FROM ZSVBMMINFOPRICE
*       FOR ALL ENTRIES IN @LT_FAE
*     WHERE MATNR = @LT_FAE-MATNR
*       AND PURCHASINGORGANIZATION = @GV_EKORG
**U3> T2 TIMEZONE 설정 - START
**       AND CONDITIONVALIDITYSTARTDATE <= @SY-DATUM
**       AND CONDITIONVALIDITYENDDATE >= @SY-DATUM
*       AND CONDITIONVALIDITYSTARTDATE <= @SY-DATLO
*       AND CONDITIONVALIDITYENDDATE >= @SY-DATLO
**U3> T2 TIMEZONE 설정 - END
*       AND CONDITIONTYPE = @GC_COND_TYPE
*       AND CONFIRM_PRICE = @GC_CONFIRM
*      INTO CORRESPONDING FIELDS OF TABLE @GT_COND.
*
*    SORT GT_COND BY MATNR.
*  ENDIF.
*
** MRO 단가
*  SELECT MANDT, BUKRS, ZMRO_CATALOG, ZMRO_CATALOG_DE, MRO_MATERIAL,
*        ZZSPEC, MEINS, PREIS, WAERS, NAME_ZPERS1, NAME_ZPERS2, NAME_ZPERS3,
*        EHFND_TEXT1333, ZMRO_SITE, ZOTHER1, ZOTHER2, ZOTHER3,
*        ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
*  INTO CORRESPONDING FIELDS OF TABLE @GT_MRO
*  FROM ZTMM10020
*  WHERE BUKRS = @P_BUKRS
*  AND MRO_MATERIAL IN @S_MATNR.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ATTACH_DATA
*&---------------------------------------------------------------------*
FORM GET_ATTACH_DATA.

  DATA: LT_ATTA TYPE SORTED TABLE OF TY_ATTA WITH UNIQUE KEY INSTID_A.

  CLEAR GT_ATTA.

  LT_ATTA = CORRESPONDING #( GT_DISP DISCARDING DUPLICATES MAPPING INSTID_A = BANFN ).

  IF LT_ATTA IS NOT INITIAL.

    SELECT INSTID_A
      INTO TABLE @GT_ATTA
      FROM SRGBTBREL
       FOR ALL ENTRIES IN @LT_ATTA
     WHERE INSTID_A = @LT_ATTA-INSTID_A
       AND TYPEID_A = @GC_GOS_TYPEID
       AND CATID_A  = @GC_GOS_CATID.

    FREE LT_ATTA.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processing_data .

*  DATA LS_USER_INFO TYPE ZSVMM_USER_INFO.

  DATA: LV_BANFN TYPE EBAN-BANFN,
        LV_BNFPO TYPE EBAN-BNFPO.


  CHECK GT_DISP IS NOT INITIAL.

  SORT GT_WF_STATUS BY CODE.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    "결재문서 ICON
*    READ TABLE GT_APV WITH KEY WFOBJECT = <LS_DISP>-BANFN
*                               BINARY SEARCH
*                               INTO DATA(LS_APV).
*    IF SY-SUBRC = 0.
*      <LS_DISP>-APV_STAT  = ICON_VIEWER_OPTICAL_ARCHIVE.
*      <LS_DISP>-APVIFKEY  = LS_APV-APVIFKEY.
*      <LS_DISP>-APVSTATUS = LS_APV-APVSTATUS.
*    ENDIF.

    "발주상태 ICON (요청수량과 PO수량 비교)
    IF <LS_DISP>-BSMNG EQ 0.
      <LS_DISP>-STATUS = ICON_RED_LIGHT.
    ELSE.
      IF <LS_DISP>-MENGE_PR > <LS_DISP>-BSMNG.
        <LS_DISP>-STATUS = ICON_YELLOW_LIGHT.
      ELSE.
        <LS_DISP>-STATUS = ICON_GREEN_LIGHT.
      ENDIF.
    ENDIF.

*    "User Info. for PR
*    CLEAR LS_USER_INFO.
*    READ TABLE GT_USER_INFO WITH KEY EMPLOY_NO  = <LS_DISP>-ZREQUESTER
*                                     BINARY SEARCH
*                                     INTO LS_USER_INFO.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-EMPLOY_NAME_PR = LS_USER_INFO-EMPLOY_NAME.
*    ENDIF.
*
*    READ TABLE GT_DEPT_INFO WITH KEY ORGN_CD = <LS_DISP>-ZPEQ_DEPARTMENT
*                            BINARY SEARCH
*                            INTO DATA(LS_DEPT_INFO).
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-DEPART_NAME_PR = LS_DEPT_INFO-ORGN_NM.
*    ENDIF.
*
*    "User Info. for PO
*    CLEAR LS_USER_INFO.
*    READ TABLE GT_USER_INFO WITH KEY EMPLOY_NO  = <LS_DISP>-ZORDER_PERSON
*                                     BINARY SEARCH
*                                     INTO LS_USER_INFO.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-EMPLOY_NAME_PO = LS_USER_INFO-EMPLOY_NAME.
*    ENDIF.
*
*    READ TABLE GT_DEPT_INFO WITH KEY ORGN_CD = <LS_DISP>-ZORDER_DEPARTMENT
*                            BINARY SEARCH
*                            INTO LS_DEPT_INFO.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-DEPART_NAME_PO = LS_DEPT_INFO-ORGN_NM.
*    ENDIF.

    "임가공 ICON
    IF <LS_DISP>-PSTYP = '3'.
      <LS_DISP>-TOLL_MANUF = ICON_TREE.
    ENDIF.

    "추정금액(요청수량*추정단가)
    IF <LS_DISP>-PEINH NE 0.
      <LS_DISP>-BAPRE = <LS_DISP>-MENGE_PR * ( <LS_DISP>-PREIS / <LS_DISP>-PEINH ).
    ENDIF.

    "첨부파일
    READ TABLE GT_ATTA WITH KEY INSTID_A = <LS_DISP>-BANFN
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      <LS_DISP>-AFILE = ICON_ATTACHMENT.
    ENDIF.

    "유효단가 유무 체크
    READ TABLE GT_COND WITH KEY MATNR = <LS_DISP>-MATNR
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      <LS_DISP>-VALID_COND = 'X'.
    ENDIF.

* 상세보기 선택 시 PR 기준 중복라인 Data 숨김
    IF P_RB = 'X'.
      IF LV_BANFN EQ <LS_DISP>-BANFN AND LV_BNFPO EQ <LS_DISP>-BNFPO.
        <LS_DISP>-ZDUPL = 'X'.
      ENDIF.

      LV_BANFN = <LS_DISP>-BANFN.
      LV_BNFPO = <LS_DISP>-BNFPO.
    ENDIF.

    READ TABLE GT_WF_STATUS INTO DATA(LS_WF_STATUS)
                            WITH KEY CODE = <LS_DISP>-WF_STAUS
                            BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      <LS_DISP>-WF_STAUS_TEXT = LS_WF_STATUS-NAME.
    ELSE.
      <LS_DISP>-WF_STAUS_TEXT = <LS_DISP>-WF_STAUS.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE GT_DISP LINES DATA(LV_TCNT).
  MESSAGE S011(ZMM01) WITH LV_TCNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_WERKS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_WERKS USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '플랜트',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'WERKS'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


* 회사코드 Check
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT A~WERKS,
         A~NAME1
    FROM T001W AS A INNER JOIN T001K AS B
                            ON A~WERKS = B~BWKEY
   WHERE B~BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_T001W).

  SORT LT_T001W BY WERKS.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T001W
                                              LT_RETURN
                                       USING  TEXT-C14  "LC_TITLE (플랜트)
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_T001W WITH KEY WERKS = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T001W).

    <LV_SCR_VALUE> = LS_T001W-WERKS.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_T001W-WERKS ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- P_BUKRS
*&---------------------------------------------------------------------*
FORM DYNP_VALUES_READ USING IV_SCR_NAME
                      CHANGING EV_SCR_VALUE.

  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD,
        LS_DYNPFIELDS TYPE DYNPREAD.

  LS_DYNPFIELDS = VALUE #( FIELDNAME  = IV_SCR_NAME ).
  APPEND LS_DYNPFIELDS TO LT_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-CPROG
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = LT_DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.

  IF SY-SUBRC = 0.
    SORT LT_DYNPFIELDS BY FIELDNAME.

    READ TABLE LT_DYNPFIELDS WITH KEY FIELDNAME = IV_SCR_NAME
                             BINARY SEARCH
                             INTO LS_DYNPFIELDS.
    IF SY-SUBRC = 0.
      EV_SCR_VALUE = LS_DYNPFIELDS-FIELDVALUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_T001W
*&      --> LT_RETURN
*&      --> TEXT_C14
*&      --> LC_RETFIELD
*&      --> IV_SCR_NAME
*&---------------------------------------------------------------------*
FORM F4IF_INT_TABLE_VALUE_REQUEST TABLES IT_HELP_TAB
                                         CT_RETURN
                                  USING  IV_TITLE
                                         IV_RETFIELD
                                         IV_SCR_NAME.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = IV_TITLE
      RETFIELD          = IV_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "Retern Field가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = IT_HELP_TAB
      RETURN_TAB        = CT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DYNPFIELDS
*&---------------------------------------------------------------------*
FORM DYNP_VALUES_UPDATE TABLES IT_DYNPFIELDS STRUCTURE DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = SY-CPROG
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = IT_DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      UNDEFIND_ERROR       = 7
      OTHERS               = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_BSART USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.


  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'BSART'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* Get Data
  SELECT BSTYP,
         BSART,
         BATXT
    FROM T161T
   WHERE SPRAS = @SY-LANGU
     AND BSTYP = 'B'        "구매요청 문서유형
    INTO TABLE @DATA(LT_T161T).

  SORT LT_T161T BY BSART.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T161T
                                              LT_RETURN
                                       USING  TEXT-X03    "LC_TITLE
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_T161T WITH KEY BSART = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T161T).

    <LV_SCR_VALUE> = LS_T161T-BSART.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME  = IV_SCR_NAME FIELDVALUE = LS_T161T-BSART ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_EKGRP USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


* 회사코드 체크
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT DISTINCT EKGRP, EKNAM
    FROM ZSVCMM_ORG
   WHERE BUKRS = @P_BUKRS
    INTO TABLE @DATA(LT_ORG_INFO).

  SORT LT_ORG_INFO BY EKGRP.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_ORG_INFO
                                              LT_RETURN
                                       USING  TEXT-X05    "LC_TITLE (구매그룹 정보)
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_ORG_INFO WITH KEY EKGRP = LS_RETURN-FIELDVAL
                           BINARY SEARCH
                           INTO DATA(LS_ORG_INFO).

    <LV_SCR_VALUE> = LS_ORG_INFO-EKGRP.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_ORG_INFO-EKGRP ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX.

* 회사코드
  PERFORM SET_LIST_BOX_BUKRS USING 'P_BUKRS'.

* PR 상태
  PERFORM SET_LIST_BOX_WFSTS USING 'P_WFSTS'.

* SRM/타시스템 전송여부
  PERFORM SET_LIST_BOX_ZRECEIPT USING 'P_ZRECEI'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_BUKRS
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_BUKRS USING IV_FNAME.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES.     "Key, Text

* 회사코드
  LV_NAME = IV_FNAME.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'BUKRS' ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  LT_LIST = CORRESPONDING #( LT_CONFIG MAPPING KEY = FIELD2  TEXT = FIELD3  ).

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_WFSTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_WFSTS USING IV_FNAME.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES.     "Key, Text
*        LS_LIST TYPE VRM_VALUE.

* PR 상태
  LV_NAME = IV_FNAME.

  ZCL_MM_COMMON=>COMMON_CONFIG(
    EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA110' )
                                   IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'WF_STATUS1' ) )
    IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

*  "ALL 강제 추가
*  LS_LIST-KEY  = GC_ALL.
*  LS_LIST-TEXT = GC_ALL.
*  INSERT LS_LIST INTO LT_LIST INDEX 1.

  LOOP AT LT_CONFIG INTO DATA(LS_CONFIG).
    CASE LS_CONFIG-FIELD2.
      WHEN GC_WFSTS_1. "결재대기
        APPEND VALUE #( KEY = TEXT-L01 ) TO LT_LIST.
      WHEN GC_WFSTS_2. "결재반려
        APPEND VALUE #( KEY = TEXT-L02 ) TO LT_LIST.
      WHEN GC_WFSTS_3. "결재중
        APPEND VALUE #( KEY = TEXT-L03 ) TO LT_LIST.
      WHEN GC_WFSTS_4. "반려
        APPEND VALUE #( KEY = TEXT-L04 ) TO LT_LIST.
      WHEN GC_WFSTS_5. "상신중
        APPEND VALUE #( KEY = TEXT-L05 ) TO LT_LIST.
      WHEN GC_WFSTS_6. "생성중
        APPEND VALUE #( KEY = TEXT-L06 ) TO LT_LIST.
      WHEN GC_WFSTS_7. "승인완료
        APPEND VALUE #( KEY = TEXT-L07 ) TO LT_LIST.
      WHEN GC_WFSTS_8. "접수대기
        APPEND VALUE #( KEY = TEXT-L08 ) TO LT_LIST.
      WHEN GC_WFSTS_9. "접수반려
        APPEND VALUE #( KEY = TEXT-L09 ) TO LT_LIST.
      WHEN GC_WFSTS_10. "접수완료
        APPEND VALUE #( KEY = TEXT-L10 ) TO LT_LIST.
    ENDCASE.
  ENDLOOP.

  APPEND VALUE #( KEY = GC_ALL ) TO LT_LIST. "ALL

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_ZRECEIPT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_ZRECEIPT USING IV_FNAME.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES,     "Key, Text
        LS_LIST TYPE VRM_VALUE.

  DEFINE _L_APPEND_VALUES.

    CLEAR: ls_list.
    ls_list-key  = &1.
    ls_list-text = &2.
    APPEND ls_list TO lt_list.

  END-OF-DEFINITION.

  LV_NAME = IV_FNAME.

  _L_APPEND_VALUES: ''  'ALL',
                    'S' TEXT-X01,    "전송완료   U3> 번역
                    'N' TEXT-X02.    "미전송     U3> 번역

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 EQ 'EXC'.
*      SCREEN-REQUIRED = '2'.

      IF GV_EXC_USER IS INITIAL.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.

    CASE SCREEN-NAME.
      WHEN 'S_BADAT-LOW'.
        SCREEN-REQUIRED = '1'.

      WHEN OTHERS.
    ENDCASE.

*    IF SCREEN-GROUP1 = 'DOM'.
*      IF GV_ORG1 = 'T1'.
*        SCREEN-ACTIVE = 1.
*      ELSE.
*        SCREEN-ACTIVE = 0.
*        CLEAR : P_URGEN, P_PREIN, P_ZRECEI.
*      ENDIF.
*    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
