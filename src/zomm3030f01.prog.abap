*&---------------------------------------------------------------------*
*& Include          ZOMM3030F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

  SSCRFIELDS-FUNCTXT_01 = TEXT-U90. "결재선관리(개인)
  SSCRFIELDS-FUNCTXT_02 = TEXT-U91. "결재선관리(전체)

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

* 요청일 기본값 (1개월 전)
  DATA: LV_SDATE TYPE SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATUM
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 1
      YEARS     = 0
    IMPORTING
      CALC_DATE = LV_SDATE.

  S_BADAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATUM ) ).

* 결재 상태 기본값
  IF P_WFSTS IS INITIAL.
    P_WFSTS = 'X'.  "결재 대기
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM SET_INIT_VALUES.

* 회사코드 / 발주담당 / 발주부서 기본값
  SELECT SINGLE COMPANY, COMPANY_NAME, EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME
    FROM ZSVMM_USER_INFO
   WHERE USER_ID EQ @SY-UNAME
    INTO @DATA(LS_USER_INFO).

  IF SY-SUBRC EQ 0.
    P_BUKRS  = LS_USER_INFO-COMPANY.

    P_PERSN  = LS_USER_INFO-EMPLOY_NO.
    GV_PERNM = LS_USER_INFO-EMPLOY_NAME.

    P_DEPAT  = LS_USER_INFO-DEPARTMENT.
    GV_DEPNM = LS_USER_INFO-DEPART_NAME.
  ENDIF.

* 구매조직 기본값
  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 2 VALUE = P_BUKRS ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    P_EKORG  = LS_CONFIG-FIELD4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PERSN
*&---------------------------------------------------------------------*
FORM CHECK_PERSN USING IV_PERSN.

  CLEAR GV_PERNM.

  SELECT SINGLE EMPLOY_NAME
    INTO @GV_PERNM
    FROM ZSVMM_USER_INFO
   WHERE EMPLOY_NO = @IV_PERSN
     AND COMPANY   = @P_BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DEPAT
*&---------------------------------------------------------------------*
FORM CHECK_DEPAT USING IV_DEPAT.

  CLEAR GV_DEPNM.

  SELECT SINGLE ORGN_NM
    INTO @GV_DEPNM
    FROM ZTCN00001
   WHERE ORGN_NM = @IV_DEPAT
     AND BUKRS  = @P_BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX.

  CONSTANTS: LC_BUKRS(7) VALUE 'P_BUKRS'.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES.     "Key, Text

* 회사코드
  LV_NAME = LC_BUKRS.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'BUKRS' ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  LT_LIST = CORRESPONDING #( LT_CONFIG MAPPING KEY = FIELD2  TEXT = FIELD3  ).

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
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
                                       USING  TEXT-X01 "구매 요청 문서 유형
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_T161T WITH KEY BSART = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T161T).

    <LV_SCR_VALUE> = LS_T161T-BSART.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_T161T-BSART ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_PERSN
*&---------------------------------------------------------------------*
FORM SET_F4_PERSN USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'EMPLOY_NO'.

  CONSTANTS: LC_ETC_FLD1 TYPE SCREEN-NAME VALUE 'GV_PERNM',
             LC_ETC_FLD2 TYPE SCREEN-NAME VALUE 'P_DEPAT',
             LC_ETC_FLD3 TYPE SCREEN-NAME VALUE 'GV_DEPNM'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

  DATA: LR_DEPARTMENT TYPE RANGE OF ZSVMM_USER_INFO-DEPARTMENT.


  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  PERFORM DYNP_VALUES_READ USING 'P_DEPAT' CHANGING P_DEPAT.

  IF NOT P_DEPAT IS INITIAL.
    LR_DEPARTMENT = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_DEPAT ) ).
  ENDIF.


* Get Data
  SELECT EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME
    FROM ZSVMM_USER_INFO
   WHERE COMPANY    EQ @P_BUKRS
     AND DEPARTMENT IN @LR_DEPARTMENT
    INTO TABLE @DATA(LT_USER_INFO).

  SORT LT_USER_INFO BY EMPLOY_NO.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_USER_INFO
                                              LT_RETURN
                                       USING  TEXT-X02 "담당자 정보
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_USER_INFO WITH KEY EMPLOY_NO = LS_RETURN-FIELDVAL
                            BINARY SEARCH
                            INTO DATA(LS_USER_INFO).

    <LV_SCR_VALUE> = LS_USER_INFO-EMPLOY_NO.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_USER_INFO-EMPLOY_NO )
                             ( FIELDNAME = LC_ETC_FLD1 FIELDVALUE = LS_USER_INFO-EMPLOY_NAME )
                             ( FIELDNAME = LC_ETC_FLD2 FIELDVALUE = LS_USER_INFO-DEPARTMENT )
                             ( FIELDNAME = LC_ETC_FLD3 FIELDVALUE = LS_USER_INFO-DEPART_NAME ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_DEPAT
*&---------------------------------------------------------------------*
FORM SET_F4_DEPAT USING IV_SCR_NAME
                        IV_SCR_DESC.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'DEPARTMENT'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


* Get Data
  SELECT DISTINCT ORGN_CD AS DEPARTMENT, ORGN_NM AS DEPART_NAME
    FROM ZTCN00001
   WHERE BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_DEPT_INFO).

  SORT LT_DEPT_INFO BY DEPARTMENT.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_DEPT_INFO
                                              LT_RETURN
                                       USING  TEXT-X03  "부서 정보
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_DEPT_INFO WITH KEY DEPARTMENT = LS_RETURN-FIELDVAL
                            BINARY SEARCH
                            INTO DATA(LS_DEPT_INFO).

    <LV_SCR_VALUE> = LS_DEPT_INFO-DEPARTMENT.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_DEPT_INFO-DEPARTMENT )
                             ( FIELDNAME = IV_SCR_DESC FIELDVALUE = LS_DEPT_INFO-DEPART_NAME ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
FORM DYNP_VALUES_READ USING    IV_SCR_NAME
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
*& Form DYNP_VALUES_UPDATE
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
*& Form F4IF_INT_TABLE_VALUE_REQUEST
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
*& Form SET_SEL_SCREEN
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

    IF SCREEN-NAME = 'P_APPR'.
      SCREEN-ACTIVE = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY.

* 로직 수정마다 권한때문에 Code Inspeciton 문제.. 할수 없이 적용.
  CHECK 1 <> 1.

  CALL FUNCTION 'ZFMM_AUTH_CHECK'
    EXPORTING
*      IV_USER = SY-UNAME.
*     IV_OBJECT                   = 'ZMM_COMMON'
     IV_BUKRS                    = P_BUKRS.
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
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: LR_FRGKZ TYPE RANGE OF EBAN-FRGKZ.

  DATA: LR_PERSN TYPE RANGE OF ZSVCMMPR_RELEASE-ZREQUESTER,
        LR_DEPAT TYPE RANGE OF ZSVCMMPR_RELEASE-ZPEQ_DEPARTMENT.

  DATA: LS_DISP  TYPE TY_DISP,
        LV_BANFN TYPE EBAN-BANFN,
        LV_BACNT TYPE I.

* 결재선 초기화
  SET PARAMETER ID 'BUK'        FIELD P_BUKRS.
  SET PARAMETER ID 'ZAPVPSTYPE' FIELD GC_APV_TYPE.
  SET PARAMETER ID 'ZAPVERNAM'  FIELD SPACE.

*--------------------------------
* 검색조건 설정
*--------------------------------
* 결재 상태 검색조건 설정
  IF P_WFSTS EQ 'A'.
    CLEAR LR_FRGKZ.
  ELSEIF P_WFSTS EQ 'P'.
    LR_FRGKZ = VALUE #( BASE LR_FRGKZ ( SIGN = 'I' OPTION = 'EQ' LOW = 'P' ) ).
    LR_FRGKZ = VALUE #( BASE LR_FRGKZ ( SIGN = 'I' OPTION = 'EQ' LOW = 'B' ) ).
  ELSE.
    LR_FRGKZ = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_WFSTS ) ).
  ENDIF.

* 요청자, 요청부서
  IF P_PERSN IS INITIAL OR P_PERSN = '*'.
    CLEAR LR_PERSN.
  ELSE.
    LR_PERSN = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_PERSN ) ).
  ENDIF.

  IF P_DEPAT IS INITIAL OR P_DEPAT = '*'.
    CLEAR LR_DEPAT.
  ELSE.
    LR_DEPAT = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_DEPAT ) ).
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
  CLEAR: GT_ITEM, GT_DISP.

  SELECT A~BUKRS, "C~BUKRS,
         A~BSART,
         A~BANFN,
         A~FRGKZ,
         CASE A~FRGKZ WHEN 'X' THEN '상신대기'
                      WHEN 'D' THEN '상신중'
                      WHEN 'P' THEN '승인완료'
                      WHEN 'B' THEN '승인완료'
                      WHEN 'J' THEN '반려'
         END AS WF_STATUS,
         A~BATXT,
         A~ZPRTITLE AS TITLE,

         A~BNFPO,
         A~PSTYP,
         A~MATNR,
         A~TXZ01,
         A~MENGE,
         A~MEINS,
         A~BADAT,
         A~LFDAT,
         A~WERKS,
         A~PLANTNAME,

         A~PREIS,
         A~PEINH,
         A~PURREQNITEMCURRENCY AS WAERS,
         A~ZURGENT_REASON,
         A~ZPRE_INPUT_REASON
    FROM ZSVCMMPR_RELEASE AS A INNER JOIN EBAN AS B
                                       ON A~BANFN = B~BANFN
                                      AND A~BNFPO = B~BNFPO
                                      AND B~LOEKZ = ''
*                                LEFT JOIN T024E AS C           "구매요청에 구매조직 없을 수 있음
*                                       ON C~EKORG = B~EKORG
   WHERE A~BUKRS             EQ @P_BUKRS
*     AND B~EKORG             EQ @P_EKORG
     AND A~ZREQUESTER        IN @LR_PERSN
     AND A~ZPEQ_DEPARTMENT   IN @LR_DEPAT
     AND A~FRGKZ             IN @LR_FRGKZ
     AND A~BSART             IN @S_BSART
     AND A~BADAT             IN @S_BADAT
     AND A~BANFN             IN @S_BANFN
     AND A~FRGKZ             NE @SPACE
     AND A~FRGST             IN ( '01', '04' )  "수작업, MRO만 결재 진행
     AND B~BSTYP             EQ 'B'             "구매요청 문서유형만
    INTO TABLE @DATA(LT_DATA).

  SORT LT_DATA BY BSART BANFN ASCENDING BNFPO DESCENDING.

* 품목 상세보기
  IF P_ITEM = 'X'.
    GT_ITEM = CORRESPONDING #( LT_DATA ).
    SORT GT_ITEM BY BANFN BNFPO.
  ENDIF.

* 정가 및 품목 수 계산
  LOOP AT LT_DATA INTO DATA(LS_DATA).

    IF LV_BANFN IS NOT INITIAL AND LV_BANFN NE LS_DATA-BANFN.
      LS_DISP-BACNT = LV_BACNT.

      APPEND LS_DISP TO GT_DISP.

      CLEAR: LS_DISP, LV_BACNT.
    ENDIF.

    MOVE-CORRESPONDING LS_DATA TO LS_DISP.

    LV_BACNT = LV_BACNT + 1.   "품목수

    IF LS_DATA-PEINH = 0.
      LS_DISP-BAPRE = LS_DISP-BAPRE.
    ELSE.
      LS_DISP-BAPRE = LS_DISP-BAPRE + ( ( LS_DATA-PREIS / LS_DATA-PEINH ) * LS_DATA-MENGE ).
    ENDIF.

    LV_BANFN = LS_DATA-BANFN.

  ENDLOOP.

  IF LS_DISP IS NOT INITIAL.
    LS_DISP-BACNT = LV_BACNT.
    APPEND LS_DISP TO GT_DISP.
  ENDIF.
*--------------------------------
* 첨부파일 Data
*--------------------------------
  PERFORM GET_ATTACH_DATA.

*--------------------------------
* 전자결재 Data
*--------------------------------
  PERFORM GET_APV_DATA.

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

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_APV_DATA
*&---------------------------------------------------------------------*
FORM GET_APV_DATA.

  TYPES: BEGIN OF LTY_PR,
           BANFN TYPE ZTMM30032-WFOBJECT,
         END OF LTY_PR.

  DATA: LT_PR TYPE TABLE OF LTY_PR.

  IF GT_DISP IS NOT INITIAL.
    MOVE-CORRESPONDING GT_DISP TO LT_PR.
    SORT LT_PR BY BANFN.
    DELETE ADJACENT DUPLICATES FROM LT_PR COMPARING BANFN.
  ENDIF.

  IF LT_PR[] IS NOT INITIAL.

    SELECT A~APVIFKEY,
           A~FIID,
           B~WFOBJECT,
           A~APVSTATUS
      FROM ZTMM30031 AS A INNER JOIN ZTMM30032 AS B
                                  ON A~APVIFKEY = B~APVIFKEY
       FOR ALL ENTRIES IN @LT_PR
     WHERE B~WFOBJECT = @LT_PR-BANFN
       AND B~MMWFOBJ  = 'PR'
*       AND A~APVSTATUS NE 'WITHDRAW'  "회수된 문서 제외
      INTO TABLE @GT_APV.

    SORT GT_APV BY WFOBJECT APVIFKEY DESCENDING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA.

  DATA: LT_DISP TYPE TABLE OF TY_DISP.

  DATA: LV_GARG TYPE SEQG3-GARG,
        LT_ENQ  TYPE TABLE OF SEQG3.

  CHECK GT_DISP IS NOT INITIAL.

  LT_DISP = GT_DISP.
  CLEAR GT_DISP.

  LOOP AT LT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

*U1> 결재상태 번역
    PERFORM SET_WF_STATUS USING <LS_DISP>-FRGKZ
                          CHANGING <LS_DISP>-WF_STATUS.

    "결재문서 ICON
    READ TABLE GT_APV WITH KEY WFOBJECT = <LS_DISP>-BANFN
                               BINARY SEARCH
                               INTO DATA(LS_APV).
    IF SY-SUBRC = 0.
      <LS_DISP>-APV_STAT  = ICON_VIEWER_OPTICAL_ARCHIVE.
      <LS_DISP>-APVIFKEY  = LS_APV-APVIFKEY.
      <LS_DISP>-APVSTATUS = LS_APV-APVSTATUS.
    ENDIF.

    "PR 문서 LOCK 체크
    LV_GARG = SY-MANDT && <LS_DISP>-BANFN.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        GNAME                 = 'EBAN'
        GARG                  = LV_GARG
      TABLES
        ENQ                   = LT_ENQ
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE        = 2
        OTHERS                = 3.

    IF LT_ENQ IS NOT INITIAL.
      <LS_DISP>-PR_LOCK = ICON_LOCKED.
    ENDIF.

    "결재대기는 PR LOCK 설정
    IF <LS_DISP>-FRGKZ = 'X'.
      CALL FUNCTION 'ENQUEUE_EMEBANE'
        EXPORTING
          BANFN          = <LS_DISP>-BANFN
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF SY-SUBRC EQ 0.
        CLEAR <LS_DISP>-PR_LOCK.
      ENDIF.
    ENDIF.

    "첨부파일
    READ TABLE GT_ATTA WITH KEY INSTID_A = <LS_DISP>-BANFN
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      <LS_DISP>-AFILE = ICON_ATTACHMENT.
    ENDIF.

    CLEAR: LV_GARG, LS_APV, LT_ENQ.

* 품목 상세보기
    IF P_ITEM = 'X'.
      READ TABLE GT_ITEM WITH KEY BANFN = <LS_DISP>-BANFN
                                  BINARY SEARCH
                                  TRANSPORTING NO FIELDS.

      LOOP AT GT_ITEM FROM SY-TABIX INTO DATA(LS_ITEM).
        IF LS_ITEM-BANFN <> <LS_DISP>-BANFN.
          EXIT.
        ENDIF.

        <LS_DISP>-BNFPO = LS_ITEM-BNFPO.

        IF LS_ITEM-PSTYP = '3'.
          <LS_DISP>-PSTYP_IND = 'X'.
        ENDIF.

        <LS_DISP>-MATNR = LS_ITEM-MATNR.
        <LS_DISP>-TXZ01 = LS_ITEM-TXZ01.
        <LS_DISP>-MENGE = LS_ITEM-MENGE.
        <LS_DISP>-MEINS = LS_ITEM-MEINS.
        <LS_DISP>-BADAT = LS_ITEM-BADAT.
        <LS_DISP>-LFDAT = LS_ITEM-LFDAT.
        <LS_DISP>-WERKS = LS_ITEM-WERKS.
        <LS_DISP>-PLANTNAME = LS_ITEM-PLANTNAME.

        APPEND <LS_DISP> TO GT_DISP.

        CLEAR: <LS_DISP>-BNFPO,
               <LS_DISP>-PSTYP,
               <LS_DISP>-PSTYP_IND,
               <LS_DISP>-MATNR,
               <LS_DISP>-TXZ01,
               <LS_DISP>-MENGE,
               <LS_DISP>-MEINS,
               <LS_DISP>-BADAT,
               <LS_DISP>-LFDAT,
               <LS_DISP>-WERKS,
               <LS_DISP>-PLANTNAME.
      ENDLOOP.

    ELSE.
      APPEND <LS_DISP> TO GT_DISP.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE GT_DISP LINES DATA(LV_TCNT).
  MESSAGE S011(ZMM01) WITH LV_TCNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
FORM CHECK_EXIT.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
FORM REFRESH_DATA.

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_PM_REQUEST
*&---------------------------------------------------------------------*
FORM UPDATE_PM_REQUEST.

  DATA: LS_PRHEADER  TYPE ZSMM_PRHEADER,
        LS_PRHEADERX TYPE ZSMM_PRHEADERX,
        LT_PRITEM    TYPE TABLE OF ZSMM_PRITEM,
        LT_PRITEMX   TYPE TABLE OF ZSMM_PRITEMX,
        LT_RETURN    TYPE TABLE OF BAPIRET2.

  CONSTANTS: LC_BSART_NB TYPE EBAN-BSART VALUE 'NB',
             LC_ESTKZ_F  TYPE EBAN-ESTKZ VALUE 'F'.

  DATA(LT_FAE) = GT_APV_PR.
  SORT LT_FAE BY BANFN.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING BANFN.

  IF LT_FAE IS NOT INITIAL.
    SELECT A~BANFN,
           A~BNFPO,
           A~BSART,
           A~ESTKZ,
           A~MATNR,
           A~WERKS,
           B~ABLAD
      INTO TABLE @DATA(LT_DATA)
      FROM EBAN AS A INNER JOIN EBKN AS B
                             ON A~BANFN = B~BANFN
                            AND A~BNFPO = B~BNFPO
       FOR ALL ENTRIES IN @LT_FAE
     WHERE A~BANFN = @LT_FAE-BANFN
       AND A~BSART = @LC_BSART_NB
       AND A~ESTKZ = @LC_ESTKZ_F.
  ENDIF.

  CHECK LT_DATA IS NOT INITIAL.

  LOOP AT LT_DATA INTO DATA(LS_DATA).

    IF LS_DATA-ABLAD IS NOT INITIAL.

      CLEAR: LS_PRHEADER, LS_PRHEADERX, LT_PRITEM, LT_PRITEMX, LT_RETURN.

      LS_PRHEADER  = VALUE #( BANFN = LS_DATA-BANFN
                              BSART = LS_DATA-BSART
                              ESTKZ = LS_DATA-ESTKZ ).

      LS_PRHEADERX  = VALUE #( BANFN = 'X'
                               BSART = 'X'
                               ESTKZ = 'X' ).

      APPEND VALUE #( BNFPO = LS_DATA-BNFPO
                      MATNR = LS_DATA-MATNR
                      TXZ01 = LS_DATA-ABLAD
                      WERKS = LS_DATA-WERKS ) TO LT_PRITEM.

      APPEND VALUE #( BNFPO = LS_DATA-BNFPO
                      MATNR = 'X'
                      TXZ01 = 'X'
                      WERKS = 'X'           ) TO LT_PRITEMX.

      CALL FUNCTION 'ZFIMM_PR_CHANGE'
        EXPORTING
          IS_PRHEADER  = LS_PRHEADER
          IS_PRHEADERX = LS_PRHEADERX
        TABLES
          IT_PRITEM    = LT_PRITEM
          IT_PRITEMX   = LT_PRITEMX
          ET_RETURN    = LT_RETURN.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_DRAFT
*&---------------------------------------------------------------------*
FORM SUBMIT_DRAFT.

  TYPES: BEGIN OF LTY_CHK,
           BSART TYPE ZSVCMMPR_RELEASE-BSART,
           WAERS TYPE ZSVCMMPR_RELEASE-PURREQNITEMCURRENCY,
         END OF LTY_CHK.

  DATA: LV_CHK,
        LT_CHK TYPE TABLE OF LTY_CHK,
        LS_CHK TYPE LTY_CHK.

  CHECK GT_DISP_APV IS NOT INITIAL.

  CLEAR: GT_APV_PR.

*-----------------------------
* Validation Check
*-----------------------------
* 결재상신 처리 기준: 동일한 문서유형인 구매요청만 함께 상신 가능
  LOOP AT GT_DISP_APV INTO DATA(LS_DISP).

    "PR 잠김 체크
    IF LS_DISP-PR_LOCK IS NOT INITIAL.
      LV_CHK = 'A'. "PR 잠김이면 상신 불가능
    ENDIF.

    "결재 상태 체크
    IF LS_DISP-BSART = 'MRO'.
      IF LS_DISP-FRGKZ NE 'X'.
        LV_CHK = 'D'. "결재 대기 상태만 상신 가능(MRO)
      ENDIF.
    ELSE.
      IF LS_DISP-FRGKZ NE 'X' AND LS_DISP-FRGKZ NE 'J'.
        LV_CHK = 'B'. "결재 대기 또는 반려 상태만 상신 가능
      ENDIF.
    ENDIF.


    IF LS_DISP-FRGKZ = 'X' AND ( LS_DISP-APVSTATUS = GC_APV_DRAFT OR LS_DISP-APVSTATUS = GC_APV_APPROVAL ).
      LV_CHK = 'C'. "PR 결재 대기지만 전자결재 진행중인 상태면 상신 불가능
    ENDIF.

    MOVE-CORRESPONDING LS_DISP TO LS_CHK.
    COLLECT LS_CHK INTO LT_CHK.

    "결재 대상 PR 번호
    GT_APV_PR = VALUE #( BASE GT_APV_PR ( BANFN  = LS_DISP-BANFN ) ).

    CLEAR: LS_DISP, LS_CHK.
  ENDLOOP.

  IF LV_CHK EQ 'A'.
    MESSAGE I000 WITH TEXT-M02 DISPLAY LIKE 'E'.  "PR 문서가 잠겨 있어 상신 할 수 없습니다.
    EXIT.
  ENDIF.

  IF LV_CHK EQ 'B'.
    MESSAGE I000 WITH TEXT-M03 DISPLAY LIKE 'E'.  "결재 대기 또는 반려 상태만 상신 가능합니다.
    EXIT.
  ENDIF.

  IF LV_CHK EQ 'D'.
    MESSAGE I000 WITH TEXT-M03 DISPLAY LIKE 'E'.  "결재 대기 상태만 상신 가능합니다.(MRO)
    EXIT.
  ENDIF.

  IF LV_CHK EQ 'C'.
    MESSAGE I000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "이미 결재진행중인 문서가 있습니다.
    EXIT.
  ENDIF.

  IF LINES( LT_CHK ) > 1.
    MESSAGE I000 WITH TEXT-M01 DISPLAY LIKE 'E'.  "동일한 문서유형인 구매요청만 함께 상신 가능합니다.
    EXIT.
  ENDIF.

*-----------------------------
* PM 구매요청 Text Update
* PM에서 생성된 구매요청에 대한 자재내역을 하역지점 TEXT로 업데이트
*-----------------------------
  IF P_APPR IS INITIAL.
    PERFORM UPDATE_PM_REQUEST.
  ENDIF.

*-----------------------------
* 전자결재 상신
*-----------------------------
  CLEAR GS_APV_RET.

  CALL FUNCTION 'ZFMM_APV_PR_SUBMIT'
    EXPORTING
      IV_BUKRS     = P_BUKRS
      IV_PERSN     = P_PERSN
      IV_DEPAT     = P_DEPAT
    IMPORTING
      EV_RESULT    = GS_APV_RET-RESULT
      EV_MESSAGE   = GS_APV_RET-MESSAGE
      EV_APVIFKEY  = GS_APV_RET-APVIFKEY
      EV_FIID      = GS_APV_RET-FIID
      EV_APVPSTYPE = GS_APV_RET-APVPSTYPE
    TABLES
      IT_BANFN     = GT_APV_PR.

  IF GS_APV_RET-RESULT EQ 'E'.
    MESSAGE I000 WITH GS_APV_RET-MESSAGE DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------------
* Refresh
*-----------------------------
  IF P_APPR IS INITIAL.
    PERFORM REFRESH_DATA.

    GRF_GRID->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_WF_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DISP>_FRGKZ
*&      <-- <LS_DISP>_WF_STATUS
*&---------------------------------------------------------------------*
FORM SET_WF_STATUS  USING    IV_FRGKZ
                    CHANGING CV_WF_STATUS.

  CASE IV_FRGKZ.
    WHEN 'X'.
      CV_WF_STATUS = TEXT-X04.  "상신대기
    WHEN 'D'.
      CV_WF_STATUS = TEXT-X05.  "상신중
    WHEN 'P' OR 'B'.
      CV_WF_STATUS = TEXT-X06.  "승인완료
    WHEN 'J'.
      CV_WF_STATUS = TEXT-X07.  "반려
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
