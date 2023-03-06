*&---------------------------------------------------------------------*
*& Include          ZOMM5510F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

* 예외처리 유저 점검
  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-REPID )
*                                                        ( FIELD = 2 VALUE = 'EX01' )
                                                        ( FIELD = 3 VALUE = SY-UNAME ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EXC_USER = LS_CONFIG-FIELD2.
  ENDIF.

* 사용자 기본값
*  IF GV_EXC_USER IS INITIAL.
  PERFORM SET_INIT_VALUES.
*  ENDIF.

* 송장전기일 기본값 (1개월 전)
  DATA: LV_SDATE TYPE SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO "U1 SY-DATUM
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 1
      YEARS     = 0
    IMPORTING
      CALC_DATE = LV_SDATE.

  S_BUDAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATLO ) ). "U1 SY-DATUM

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM SET_INIT_VALUES.

* 회사코드 / 담당자(사번X -> ID) 기본값
*  SELECT SINGLE COMPANY, COMPANY_NAME, USER_ID, EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE USER_ID EQ @SY-UNAME
*    INTO @DATA(LS_USER_INFO).
*
*  IF SY-SUBRC EQ 0.
*    P_BUKRS  = LS_USER_INFO-COMPANY.
*
*    P_PERSN  = LS_USER_INFO-USER_ID. "LS_USER_INFO-EMPLOY_NO
*    GV_PERNM = LS_USER_INFO-EMPLOY_NAME.
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
FORM CHECK_AUTHORITY.


*  DATA: LV_BUKRS TYPE BUKRS.
*  LV_BUKRS = P_BUKRS.
*
*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      IV_USER  = SY-UNAME
**     IV_OBJECT                   = 'ZMM_COMMON'
*      IV_BUKRS = LV_BUKRS.
**     IV_EKORG =
**     IV_LGORT =
**      IV_WERKS =
**     IV_ZEXSPA                   =
**     IV_ZPODEP                   =
**     IV_ZPRDEP                   =
** TABLES
**     IT_BUKRS                    =
**     IT_EKORG                    =
**     IT_LGORT                    =
**     IT_WERKS                    =
**     IT_ZEXSPA                   =
**     IT_ZPODEP                   =
**     IT_ZPRDEP                   =
** EXCEPTIONS
**     NO_ID_DATA_FOUND            = 1
**     AUTHORIZATION_FAILURE       = 2
**     NO_INPUT_AUTH_VALUE         = 3
**     NO_DATA_FOUND               = 4
**     MANDATORYFIELDISMISS        = 5
**     OTHERS  = 6
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_01.

  DATA: LR_PERSN TYPE RANGE OF EKBE-ERNAM.

  DATA: LR_AWKEY TYPE RANGE OF BKPF-AWKEY,
        LS_AWKEY LIKE LINE OF LR_AWKEY,
        LS_EBELN LIKE LINE OF GR_EBELN.

  DATA: LV_TYPE TYPE CHAR1.

*  DATA: LT_IN  TYPE TABLE OF ZSFI50010,
*        LS_IN  TYPE ZSFI50010,
*        LT_OUT TYPE TABLE OF ZSFI50009,
*        LS_OUT TYPE ZSFI50009.

*--------------------------------
* 검색조건 설정
*--------------------------------
* 송장처리자 설정
  IF P_PERSN IS INITIAL OR P_PERSN = '*'.
    CLEAR LR_PERSN.
  ELSE.
    LR_PERSN = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_PERSN ) ).
  ENDIF.

  SELECT SINGLE LAND1 FROM T001 WHERE BUKRS = @P_BUKRS INTO @DATA(LV_LAND1). "U1
*--------------------------------
* 선급 반제용 송장문서 추출
*--------------------------------
  CLEAR GT_DISP_01.

* 1) 송장문서 추출
  SELECT A~BUKRS,
         C~BELNR,
         C~GJAHR,
         C~BUDAT,
         C~BLDAT,
         D~SRMWWR,
         C~WAERS,
         D~ZTERM,
         E~TEXT1 AS ZTERM_TEXT,
         D~MWSKZ,
         D~ZLSCH,
         F~TEXT1 AS ZLSCH_TEXT,
         A~EBELN,
         C~BUPLA
    FROM EKKO AS A INNER JOIN EKBE AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN RBKP AS C
                           ON C~BELNR = B~BELNR
                          AND C~GJAHR = B~GJAHR
                          AND C~STBLG IS INITIAL        "-->역분개 전표에 값이 있으면 제외(취소된 전표)
                   INNER JOIN RBVS AS D
                           ON D~BELNR = B~BELNR
                          AND D~GJAHR = B~GJAHR
                          AND D~ZTERM = @GC_ZTERM_XX01  "-->선급금 AP만 반제대상
                    LEFT JOIN T052U AS E
                           ON E~SPRAS = @SY-LANGU
                          AND E~ZTERM = D~ZTERM
                    LEFT JOIN T042Z AS F
                           ON F~LAND1 = @LV_LAND1   "U1 GC_LAND1_KR
                          AND F~ZLSCH = D~ZLSCH
   WHERE A~BUKRS  = @P_BUKRS
     AND A~EKORG  = @P_EKORG
     AND B~ERNAM IN @LR_PERSN
     AND A~LIFNR IN @S_LIFNR
     AND A~EKGRP IN @S_EKGRP
     AND B~BUDAT IN @S_BUDAT
     AND A~ZORDER_PERSON     IN @S_ORDER
     AND A~ZORDER_DEPARTMENT IN @S_DEPTO
     AND A~ZEXPEN_PERSON     IN @S_EXPEN
     AND A~ZEXPEN_DEPARTMENT IN @S_DEPTE
     AND A~EBELN IN @S_EBELN
     AND A~BSART IN @S_BSART
     AND A~DPPCT IS NOT INITIAL
     AND B~VGABE  = '2'
     AND B~SHKZG  = 'S'
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISP_01.

  CHECK GT_DISP_01 IS NOT INITIAL.

  "송장번호 기준으로 중복 제거(PO에서 하나의 ITEM에만 선급 전표를 치므로)
  SORT GT_DISP_01 BY BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM GT_DISP_01 COMPARING BELNR GJAHR.


* 2) FI 전표 추출
  LOOP AT GT_DISP_01 ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    "전표 오브젝트 키
    CONCATENATE <LS_DISP>-BELNR <LS_DISP>-GJAHR INTO <LS_DISP>-AWKEY.

    LS_AWKEY+0(3) = 'IEQ'.
    LS_AWKEY-LOW  = <LS_DISP>-AWKEY.
    APPEND LS_AWKEY TO LR_AWKEY.

    "구매오더번호(선급금 전표 추출 시 사용)
    LS_EBELN+0(3) = 'IEQ'.
    LS_EBELN-LOW  = <LS_DISP>-EBELN.
    APPEND LS_EBELN TO GR_EBELN.

  ENDLOOP.

  IF LR_AWKEY IS NOT INITIAL.

    SELECT A~AWKEY,
           A~BELNR,
           A~GJAHR,
           B~BUZEI,
           B~LIFNR
      INTO TABLE @DATA(LT_FI_DOC)
      FROM BKPF AS A INNER JOIN BSEG AS B
                             ON A~BUKRS = B~BUKRS
                            AND A~BELNR = B~BELNR
                            AND A~GJAHR = B~GJAHR
                            AND B~ZTERM = @GC_ZTERM_XX01
     WHERE A~BUKRS  = @P_BUKRS
       AND A~AWKEY IN @LR_AWKEY.

    SORT LT_FI_DOC BY AWKEY.

  ENDIF.


* 3) 전표 반제처리 체크
  LOOP AT GT_DISP_01 ASSIGNING <LS_DISP>.

    READ TABLE LT_FI_DOC WITH KEY AWKEY = <LS_DISP>-AWKEY
                                  BINARY SEARCH
                                  INTO DATA(LS_FI_DOC).
*    IF SY-SUBRC = 0.

*      CLEAR: LV_TYPE, LT_IN, LS_IN, LT_OUT, LS_OUT.
*
*      LS_IN-LIFNR = LS_FI_DOC-LIFNR.
*      APPEND LS_IN TO LT_IN.

*      call function 'ZFFI_AP_DOWNPAYMENT02'
*        EXPORTING
*          IV_BUKRS = P_BUKRS
*          IV_BELNR = LS_FI_DOC-BELNR
*          IV_GJAHR = LS_FI_DOC-GJAHR
*        IMPORTING
*          EV_TYPE  = LV_TYPE
*        TABLES
*          IT_IN    = LT_IN
*          IT_OUT   = LT_OUT.

*      "전표번호와 항번에 해당하는 라인의 REM_AMT가 0이 아니면 화면에 출력
*      IF LV_TYPE NE 'E'.
*
*        SORT LT_OUT BY BELNR GJAHR BUZEI.
*
*        READ TABLE LT_OUT WITH KEY BELNR = LS_FI_DOC-BELNR
*                                   GJAHR = LS_FI_DOC-GJAHR
*                                   BUZEI = LS_FI_DOC-BUZEI
*                                   BINARY SEARCH
*                                   INTO LS_OUT.

*        IF SY-SUBRC = 0.
*          IF LS_OUT-REM_AMT = 0.
*            <LS_DISP>-ZDELE = 'X'.
*          ELSE.
*            <LS_DISP>-BELNR_FI = LS_OUT-BELNR.
*            <LS_DISP>-GJAHR_FI = LS_OUT-GJAHR.
*            <LS_DISP>-BUZEI_FI = LS_OUT-BUZEI.
*          ENDIF.

*        ELSE.
*          <LS_DISP>-ZDELE = 'X'.
*        ENDIF.
*
*      ELSE.
*        <LS_DISP>-ZDELE = 'X'.
*      ENDIF.

*    ENDIF.

  ENDLOOP.

  DELETE GT_DISP_01 WHERE ZDELE = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_02
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_02.

  CHECK GR_EBELN IS NOT INITIAL.

  SORT GR_EBELN BY LOW.
  DELETE ADJACENT DUPLICATES FROM GR_EBELN COMPARING LOW.

*--------------------------------
* 선급금 전표 추출
*--------------------------------
  CLEAR GT_DISP_02.

* 1) PO번호로 선급금 전표 추출
  SELECT B~BUKRS,
         A~BELNR,
         A~GJAHR,
         A~BUZEI,
         A~WRBTR,
         D~TAX_AMOUNT AS TAX_AMT,
         A~WAERS,
         A~EBELN,
         B~LIFNR,
         E~NAME1,
         D~MWSKZ,
         D~BUPLA
    FROM EKBE AS A INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                               INNER JOIN BKPF AS F
                           ON B~BUKRS = F~BUKRS
                          AND A~BELNR = F~BELNR
                          AND A~GJAHR = F~GJAHR
                   INNER JOIN BSEG AS C
                           ON B~BUKRS = C~BUKRS
                          AND A~BELNR = C~BELNR
                          AND A~GJAHR = C~GJAHR
                          AND RIGHT( A~BUZEI, 3 ) = C~BUZEI
                    LEFT JOIN ZCBMM_DOWNPAYT AS D
                           ON D~BELNR = A~BELNR
                          AND D~GJAHR = A~GJAHR
                          AND D~BUKRS = B~BUKRS
                    LEFT JOIN LFA1 AS E
                           ON E~LIFNR = B~LIFNR
   WHERE A~EBELN IN @GR_EBELN
     AND A~VGABE  = '4'   "선금
     AND A~SHKZG  = 'S'
     AND F~STBLG IS INITIAL
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISP_02.

* 2) 선급전표별 반제전표의 반제금액 합
* 전체 반제
  DATA(LT_FAE) = GT_DISP_02.
  SORT LT_FAE BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING BUKRS BELNR GJAHR.

  IF LT_FAE IS NOT INITIAL.
    SELECT A~BELNR,
           A~GJAHR,
           A~BUZEI,
           A~AUGBL,
           SUM( C~WRBTR ) AS CLE_AMT,
           SUM( D~TAX_AMOUNT ) AS TAX_AMT
      FROM BSAK_VIEW AS A INNER JOIN @LT_FAE AS B
                                  ON B~BELNR = A~BELNR
                                 AND B~GJAHR = A~GJAHR
                                 AND B~BUKRS = A~BUKRS
                           LEFT JOIN EKBE AS C
                                  ON C~BELNR = A~AUGBL
                                 AND C~VGABE = A~UMSKZ
                           LEFT JOIN ZCBMM_DOWNPAYT AS D
                                  ON D~BELNR = A~AUGBL
                                 AND D~GJAHR = A~GJAHR
                                 AND D~BUKRS = A~BUKRS
                                 AND D~PROCESSFLOWNODEDOCCATEGORY = A~UMSKZ
    WHERE A~UMSKZ = 'C'   "반제
    GROUP BY A~BELNR,
             A~GJAHR,
             A~BUZEI,
             A~AUGBL
     INTO TABLE @DATA(LT_CLE_ALL).

    SORT LT_CLE_ALL BY BELNR GJAHR BUZEI.
  ENDIF.

* 부분 반제
  FREE LT_FAE.
  LT_FAE = GT_DISP_02.
  SORT LT_FAE BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING BUKRS BELNR GJAHR.

  IF LT_FAE IS NOT INITIAL.
    SELECT A~REBZG AS BELNR,
           A~REBZJ AS GJAHR,
           A~REBZZ AS BUZEI,
           A~BELNR AS AUGBL,
           SUM( C~WRBTR ) AS CLE_AMT,
           SUM( D~TAX_AMOUNT ) AS TAX_AMT
      FROM BSIK_REC_VIEW AS A INNER JOIN @LT_FAE AS B
                                      ON B~BELNR = A~REBZG
                                     AND B~GJAHR = A~REBZJ
                                     AND B~BUKRS = A~BUKRS
                               LEFT JOIN EKBE AS C
                                      ON C~BELNR = A~BELNR
                                     AND C~VGABE = A~UMSKZ
                               LEFT JOIN ZCBMM_DOWNPAYT AS D
                                      ON D~BELNR = A~BELNR
                                     AND D~GJAHR = A~GJAHR
                                     AND D~BUKRS = A~BUKRS
                                     AND D~PROCESSFLOWNODEDOCCATEGORY = A~UMSKZ
    WHERE A~UMSKZ = 'C'   "반제
    GROUP BY A~REBZG,
             A~REBZJ,
             A~REBZZ,
             A~BELNR
     INTO TABLE @DATA(LT_CLE_PAR).

    SORT LT_CLE_PAR BY BELNR GJAHR BUZEI.
  ENDIF.

* 3) 반제금액, 선급잔액 계산
  LOOP AT GT_DISP_02 ASSIGNING FIELD-SYMBOL(<LS_DISP>).

* 선급금 총액
    <LS_DISP>-TOT_AMT = <LS_DISP>-WRBTR + <LS_DISP>-TAX_AMT.

* 선급반제금액
    "전체 반제
    READ TABLE LT_CLE_ALL WITH KEY BELNR = <LS_DISP>-BELNR
                                   GJAHR = <LS_DISP>-GJAHR
                                   BUZEI = <LS_DISP>-BUZEI
                                   BINARY SEARCH
                                   INTO DATA(LS_CLE_ALL).
    IF SY-SUBRC = 0.
      <LS_DISP>-CLE_AMT = LS_CLE_ALL-CLE_AMT - LS_CLE_ALL-TAX_AMT.
    ENDIF.

    "부분반제
    READ TABLE LT_CLE_PAR WITH KEY BELNR = <LS_DISP>-BELNR
                                   GJAHR = <LS_DISP>-GJAHR
                                   BUZEI = <LS_DISP>-BUZEI
                                   BINARY SEARCH
                                   INTO DATA(LS_CLE_PAR).
    IF SY-SUBRC = 0.
      <LS_DISP>-CLE_AMT = <LS_DISP>-CLE_AMT + LS_CLE_PAR-CLE_AMT - LS_CLE_PAR-TAX_AMT.
    ENDIF.

* 선급잔액
    <LS_DISP>-REM_AMT = <LS_DISP>-TOT_AMT - <LS_DISP>-CLE_AMT.

    IF <LS_DISP>-REM_AMT = 0.
      <LS_DISP>-ZDELE = 'X'.
    ENDIF.

    CLEAR: LS_CLE_ALL, LS_CLE_PAR.
  ENDLOOP.

  DELETE GT_DISP_02 WHERE ZDELE = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PERSN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_PERSN
*&      <-- GV_PERNM
*&---------------------------------------------------------------------*
FORM CHECK_PERSN USING IV_PERSN
                 CHANGING CV_PERNM.

  CLEAR CV_PERNM.

*  SELECT SINGLE EMPLOY_NAME
*    INTO @CV_PERNM
*    FROM ZSVMM_USER_INFO
*   WHERE EMPLOY_NO = @IV_PERSN
*     AND COMPANY   = @P_BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_PERSN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_PERSN USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '담당자 정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EMPLOY_NO'.

  DATA: LV_TITLE(40) TYPE C.
  LV_TITLE = TEXT-X03. "U1 담당자 정보
  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

** Get Data
*  SELECT EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE COMPANY    EQ @P_BUKRS
*    INTO TABLE @DATA(LT_USER_INFO).
*
*  SORT LT_USER_INFO BY EMPLOY_NO.
*
** Search Help
*  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_USER_INFO
*                                              LT_RETURN
*                                       USING  LV_TITLE
*                                              LC_RETFIELD
*                                              IV_SCR_NAME.
*
** Return
*  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.
*
*  IF SY-SUBRC = 0.
*    READ TABLE LT_USER_INFO WITH KEY EMPLOY_NO = LS_RETURN-FIELDVAL
*                            BINARY SEARCH
*                            INTO DATA(LS_USER_INFO).
*
*    <LV_SCR_VALUE> = LS_USER_INFO-EMPLOY_NO.
*
*    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_USER_INFO-EMPLOY_NO ) ).
*
*    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
*  ENDIF.

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
*&      --> LT_USER_INFO
*&      --> LT_RETURN
*&      --> LV_TITLE
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
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_EKGRP USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '구매그룹 정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.

  DATA: LV_TITLE(40) TYPE C.
  LV_TITLE = TEXT-X02. "U1 구매그룹 정보
  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


* 회사코드, 구매조직 체크
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM DYNP_VALUES_READ USING 'P_EKORG' CHANGING P_EKORG.

  IF P_EKORG IS INITIAL.
    MESSAGE S017 WITH TEXT-F02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT DISTINCT EKGRP, EKNAM
    FROM ZSVCMM_ORG
   WHERE BUKRS = @P_BUKRS
     AND EKORG = @P_EKORG
    INTO TABLE @DATA(LT_ORG_INFO).

  SORT LT_ORG_INFO BY EKGRP.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_ORG_INFO
                                              LT_RETURN
                                       USING  LV_TITLE
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
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_BSART USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '구매 문서 유형',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'BSART'.

  DATA: LV_TITLE(40) TYPE C.
  LV_TITLE = TEXT-X01. "U1 구매 문서 유형

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* Get Data
  SELECT BSTYP,
         BSART,
         BATXT
    FROM T161T
   WHERE SPRAS  = @SY-LANGU
     AND BSTYP = 'F' "IN ( 'F', 'K' )        "계약/PO 문서유형
    INTO TABLE @DATA(LT_T161T).

  SORT LT_T161T BY BSTYP BSART.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T161T
                                              LT_RETURN
                                       USING  LV_TITLE
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    SORT LT_T161T BY BSART.

    READ TABLE LT_T161T WITH KEY BSART = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T161T).

    <LV_SCR_VALUE> = LS_T161T-BSART.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_T161T-BSART ) ).

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
  PERFORM SET_LIST_BOX_BUKRS.

* 구매조직
  PERFORM SET_LIST_BOX_EKORG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_BUKRS
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_BUKRS.

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
*& Form SET_LIST_BOX_EKORG
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_EKORG.

  CONSTANTS: LC_EKORG(7) VALUE 'P_EKORG',
             LC_BUKRS(5) VALUE 'BUKRS'.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES.     "Key, Text

* 구매조직
  LV_NAME = LC_EKORG.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = LC_BUKRS ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  LT_LIST = CORRESPONDING #( LT_CONFIG MAPPING KEY = FIELD4  TEXT = FIELD5  ).

  _G_SET_VALUES: LV_NAME LT_LIST.

* 구매조직 기본값
  IF P_EKORG IS INITIAL AND P_BUKRS IS NOT INITIAL.
    DATA(LS_CONFIG) = VALUE #( LT_CONFIG[ FIELD2 = P_BUKRS ] OPTIONAL ).
    P_EKORG = LS_CONFIG-FIELD4.
  ENDIF.

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

  CONSTANTS: LC_P_BUKRS TYPE SCREEN-NAME VALUE 'P_BUKRS'.

  CHECK SY-DYNNR = GC_DYNNR_1000.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'EXC'.
*      SCREEN-REQUIRED = '2'.
*
*      IF GV_EXC_USER IS INITIAL.
*        SCREEN-INPUT = 0.
*      ELSE.
*        SCREEN-INPUT = 1.
*      ENDIF.
*      CASE GV_EXC_USER.
*        WHEN 'EX01'.
*          SCREEN-INPUT = 1.
*        WHEN 'EX02'.
*          IF SCREEN-NAME EQ LC_P_BUKRS.
*            SCREEN-INPUT = 0.
*          ENDIF.
*        WHEN OTHERS.
*          SCREEN-INPUT = 0.
*      ENDCASE.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEARING_PRE_PAYMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CLEARING_PRE_PAYMENT.

  DATA: LT_EBELN  TYPE TABLE OF EKKO-EBELN,
        LT_CHECK  TYPE TABLE OF TY_CHECK,
        LS_CHECK  TYPE TY_CHECK,
        LV_SUM_01 TYPE RBVS-SRMWWR,
        LV_SUM_02 TYPE EKBE-WRBTR.

  CLEAR: GT_ROWS_01, GT_ROWS_02, GV_TAX_CHK.

  "선급 반제용 송장문서 선택
  CALL METHOD GRF_GRID_01->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS_01.
  DELETE GT_ROWS_01 WHERE ROWTYPE IS NOT INITIAL.

  "선급금 전표 선택
  CALL METHOD GRF_GRID_02->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS_02.
  DELETE GT_ROWS_02 WHERE ROWTYPE IS NOT INITIAL.

  IF GT_ROWS_01 IS INITIAL OR GT_ROWS_02 IS INITIAL.
    MESSAGE I000 WITH TEXT-M01 DISPLAY LIKE 'E'.  "선급 반제용 송장문서와 선급금 전표를 모두 선택하세요.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
  LOOP AT GT_ROWS_01 INTO DATA(LS_ROWS_01).
    READ TABLE GT_DISP_01 INTO DATA(LS_DISP_01) INDEX LS_ROWS_01-INDEX.

    IF SY-SUBRC = 0.
      "반제금액 SUM
      LV_SUM_01 = LV_SUM_01 + LS_DISP_01-SRMWWR.

      COLLECT LS_DISP_01-EBELN INTO LT_EBELN.
    ENDIF.

  ENDLOOP.

  LOOP AT GT_ROWS_02 INTO DATA(LS_ROWS_02).
    READ TABLE GT_DISP_02 INTO DATA(LS_DISP_02) INDEX LS_ROWS_02-INDEX.

    IF SY-SUBRC = 0.
      "선급금 총액 SUM
      LV_SUM_02 = LV_SUM_02 + LS_DISP_02-TOT_AMT.

      IF LS_DISP_02-TAX_AMT IS NOT INITIAL. "선급금에 부가세 있는지 체크
        GV_TAX_CHK = 'X'.
      ENDIF.

      COLLECT LS_DISP_02-EBELN INTO LT_EBELN.

      CLEAR LS_CHECK.
      MOVE-CORRESPONDING LS_DISP_02 TO LS_CHECK.
      COLLECT LS_CHECK INTO LT_CHECK.
    ENDIF.

  ENDLOOP.

  IF LINES( LT_EBELN ) > 1.
    MESSAGE I000 WITH TEXT-M02 DISPLAY LIKE 'E'.  "동일한 한 건의 구매오더에 대한 처리만 가능합니다.
    EXIT.
  ENDIF.

  IF LINES( LT_CHECK ) > 1.
    MESSAGE I000 WITH TEXT-M06 DISPLAY LIKE 'E'.  "대상 문서의 사업장, 세금코드, 통화키가 모두 같아야합니다.
    EXIT.
  ENDIF.

  IF LV_SUM_01 > LV_SUM_02. "반제대상 금액이 작아야함
    MESSAGE I000 WITH TEXT-M03 DISPLAY LIKE 'E'.  "반제대상 금액이 선급요청 금액보다 큽니다.
    EXIT.
  ENDIF.

*-----------------------------
* 선급금 반제 실행 Popup
*-----------------------------
  CLEAR: GV_BUDAT, GV_BLDAT, GV_TAX_AMT, GV_WAERS.

*  READ TABLE GT_DISP_01 INTO LS_DISP_01 INDEX 1. 2022.11.01 주석처리

  GV_BUDAT = LS_DISP_01-BUDAT.
  GV_BLDAT = LS_DISP_01-BLDAT.

  IF GV_TAX_CHK = 'X'.
    GV_TAX_AMT = LV_SUM_01 / '1.1' * '0.1'.
  ENDIF.

  GV_WAERS = LS_DISP_01-WAERS.

  CALL SCREEN '0200' STARTING AT 55 05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CLEARING_DOCUMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CREATE_CLEARING_DOCUMENT
*&---------------------------------------------------------------------*
FORM CREATE_CLEARING_DOCUMENT.

  DATA: LV_BUKRS TYPE BKPF-BUKRS,
        LV_BLART TYPE BKPF-BLART VALUE 'KG',
        LV_WAERS TYPE BKPF-WAERS,
        LV_LIFNR TYPE EKKO-LIFNR,
        LV_XBLNR TYPE BKPF-XBLNR,
        LV_BKTXT TYPE BKPF-BKTXT, " VALUE 'MM 선급전표 반제',
        LV_ZFLAG TYPE CHAR1.  "ZFI_ZFLAG

*        LT_IN_A  TYPE TABLE OF ZSFI50014,
*        LT_IN_B  TYPE TABLE OF ZSFI50016,
*        LT_IN_C  TYPE TABLE OF ZSFI50030,
*        LT_OUT   TYPE TABLE OF ZSFI50013.

  DATA: LV_BELNR TYPE BELNR_D,
        LV_GJAHR TYPE GJAHR.

  LV_BKTXT = TEXT-X05. "U1 MM 선급전표 반제

  IF GV_BUDAT IS INITIAL OR GV_BLDAT IS INITIAL.
    MESSAGE I000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "전표의 전기일과 증빙일을 모두 입력하세요.
    EXIT.
  ENDIF.

  IF GV_TAX_CHK = 'X' AND GV_TAX_AMT IS INITIAL.
    MESSAGE I000 WITH TEXT-M05 DISPLAY LIKE 'E'.  "선급금의 선급부가세가 존재할 경우 필수입니다.
    EXIT.
  ENDIF.

* 해지 대상 선급정보(선급금 전표)
  LOOP AT GT_ROWS_02 INTO DATA(LS_ROWS_02).
    READ TABLE GT_DISP_02 INTO DATA(LS_DISP_02) INDEX LS_ROWS_02-INDEX.

    IF SY-SUBRC = 0.
      LV_LIFNR = LS_DISP_02-LIFNR.

*      APPEND VALUE #( LIFNR = LS_DISP_02-LIFNR
*                      GJAHR = LS_DISP_02-GJAHR
*                      BELNR = LS_DISP_02-BELNR
*                      BUZEI = LS_DISP_02-BUZEI
*                      UMSKZ = 'C' )
*                      TO LT_IN_A.
*
*      "부가세 반제
*      IF LT_IN_C IS INITIAL AND GV_TAX_AMT IS NOT INITIAL.
*        LV_ZFLAG = 'X'.
*
*        APPEND VALUE #( MWSKZ = LS_DISP_02-MWSKZ     "세금코드
*                        BUPLA = LS_DISP_02-BUPLA     "사업장
*                        WRBTR = GV_TAX_AMT
*                        WAERS = LS_DISP_02-WAERS )
*                        TO LT_IN_C.
*      ENDIF.
    ENDIF.

  ENDLOOP.

* 반제 대상 AP 정보(선급 반제용 송장문서)
  LOOP AT GT_ROWS_01 INTO DATA(LS_ROWS_01).
    READ TABLE GT_DISP_01 INTO DATA(LS_DISP_01) INDEX LS_ROWS_01-INDEX.

    IF SY-SUBRC = 0.
      LV_BUKRS = LS_DISP_01-BUKRS.
      LV_WAERS = LS_DISP_01-WAERS.
*      LV_XBLNR = LS_DISP_01-EBELN.

*      APPEND VALUE #( LIFNR = LV_LIFNR              "선급금 전표 업체와 동일하게 처리
*                      GJAHR = LS_DISP_01-GJAHR_FI
*                      BELNR = LS_DISP_01-BELNR_FI
*                      BUZEI = LS_DISP_01-BUZEI_FI )
*                      TO LT_IN_B.
    ENDIF.

  ENDLOOP.

** 전표 생성자 사번
*  SELECT SINGLE EMPLOY_NO
*    INTO @LV_XBLNR
*    FROM ZSVMM_USER_INFO
*   WHERE USER_ID = @SY-UNAME.
*
*  call function 'ZFFI_AP_DOWNPAYMENT05'
*    EXPORTING
*      IV_BUKRS = LV_BUKRS
*      IV_BLART = LV_BLART
*      IV_WAERS = LV_WAERS
*      IV_BUDAT = GV_BUDAT
*      IV_BLDAT = GV_BLDAT
*      IV_XBLNR = LV_XBLNR   "구매오더번호->사번으로 변경 요청
*      IV_BKTXT = LV_BKTXT
*      IV_ZFLAG = LV_ZFLAG
*    TABLES
*      IT_IN_A  = LT_IN_A
*      IT_IN_B  = LT_IN_B
*      IT_IN_C  = LT_IN_C
*      ET_OUT   = LT_OUT.

*  READ TABLE LT_OUT WITH KEY TYPE = 'E' INTO DATA(LS_OUT).
*
*  IF SY-SUBRC = 0.
*    MESSAGE I000 WITH LS_OUT-MESSAGE DISPLAY LIKE 'E'.
*
*  ELSE.
*    CLEAR: LV_BELNR, LV_GJAHR, LS_OUT.
*
*    "전표번호 추출
*    LOOP AT LT_OUT INTO LS_OUT.
*      IF LS_OUT-BELNR IS NOT INITIAL.
*        LV_BELNR = LS_OUT-BELNR.
*        LV_GJAHR = LS_OUT-GJAHR.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*    "전표조회
*    SET PARAMETER ID 'BLN' FIELD LV_BELNR.
*    SET PARAMETER ID 'BUK' FIELD LV_BUKRS.
*    SET PARAMETER ID 'GJR' FIELD LV_GJAHR.
*
*    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*
*  ENDIF.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM REFRESH_DATA.

  GRF_GRID_01->REFRESH_GRID_DISPLAY( ).
  GRF_GRID_02->REFRESH_GRID_DISPLAY( ).

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA.

  PERFORM GET_DATA_01.

  PERFORM GET_DATA_02.

ENDFORM.
