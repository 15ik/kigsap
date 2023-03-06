*&---------------------------------------------------------------------*
*& Include          ZRMM3020F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

* 사용자 기본값
*  PERFORM SET_INIT_VALUES.

* 발주일, 전기일 기본값 (1개월 전)
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

  S_BEDAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATUM ) ).
  S_BUDAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATUM ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM SET_INIT_VALUES.

** 회사코드 기본값
*  SELECT SINGLE COMPANY, COMPANY_NAME, EMPLOY_NO, DEPARTMENT
*    FROM ZSVMM_USER_INFO
*   WHERE USER_ID EQ @SY-UNAME
*    INTO @DATA(LS_USER_INFO).
*
*  IF SY-SUBRC EQ 0.
*    P_BUKRS = LS_USER_INFO-COMPANY.
*
**    발주담당자자동입력 로직( 타프로그램에서 submit 시 제외)
*    IF SY-TCODE EQ SY-CPROG.
*      S_ORDER[] = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = LS_USER_INFO-EMPLOY_NO ) ).
*      S_DEPTO[] = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = LS_USER_INFO-DEPARTMENT ) ).
*    ENDIF.
*
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
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY.

* 권한체크 룰(추후 적용)
  CHECK 1 <> 1.

*> By Code inspection
*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      IV_USER = SY-UNAME.
*     IV_OBJECT                   = 'ZMM_COMMON'
*     IV_BUKRS                    = P_BUKRS.
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
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.

*--------------------------------
* 입력 값 체크
*--------------------------------
* 발주일 기준
  IF P_RA = 'X' AND S_BEDAT[] IS INITIAL.
    MESSAGE S000 WITH TEXT-M01 DISPLAY LIKE 'E'. "발주일을 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

* 전기일 기준
  IF P_RB = 'X' AND S_BUDAT[] IS INITIAL.
    MESSAGE S000 WITH TEXT-M02 DISPLAY LIKE 'E'. "전기일을 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

* 입고 전기일 기준
  IF P_GR = 'X' AND S_BUDAT[] IS INITIAL.
    MESSAGE S000 WITH TEXT-M03 DISPLAY LIKE 'E'. "입고 전기일을 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

* 송장 전기일 기준
  IF P_IR = 'X' AND S_BUDAT[] IS INITIAL.
    MESSAGE S000 WITH TEXT-M04 DISPLAY LIKE 'E'. "송장 전기일을 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

*--------------------------------
* 검색조건 설정
*--------------------------------
* 내외자구분 검색조건 설정
  IF P_KALSK IS INITIAL.
    CLEAR GR_KALSK.
  ELSE.
    GR_KALSK = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_KALSK ) ).
  ENDIF.

* 구매오더 삭제건 포함 검색조건 설정
  IF P_LOEKZ IS INITIAL.
    GR_LOEKZ = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) ).
  ELSE.
    CLEAR GR_LOEKZ.
  ENDIF.

* 반품/무상 제외 검색조건 설정
  IF P_EXRET IS INITIAL.
    CLEAR: GR_RETPO, GR_REPOS.
  ELSE.
    GR_RETPO = VALUE #( ( SIGN = 'E' OPTION = 'EQ' LOW = 'X' ) ).
    GR_REPOS = VALUE #( ( SIGN = 'E' OPTION = 'EQ' LOW = SPACE ) ).
  ENDIF.

* 자율납품/위탁 제외 검색조건 설정
  CLEAR GR_BSTYP.
  IF P_EXSCH IS INITIAL.
    GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'F' )
                        ( SIGN = 'I' OPTION = 'EQ' LOW = 'L' ) ). "구매오더, 자율납품
  ELSE.
    GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'F' ) ). "구매오더
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
  CLEAR: GT_DATA, GT_DISP, GT_ALL, GT_LIFNR_SUM, GT_MATNR_SUM, GT_ORDER_SUM, GT_DEPTO_SUM,
         GV_LOCAL_WAERS.


*U2(T2)> 'KRW' 는 Local Currency 로 전환 - Start
  SELECT SINGLE WAERS
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @GV_LOCAL_WAERS.
  IF GV_LOCAL_WAERS IS INITIAL.
    GV_LOCAL_WAERS = GC_CUKY_KRW.
  ENDIF.
*U2(T2)> 'KRW' 는 Local Currency 로 전환 - END

  CASE 'X'.
    WHEN P_RA.  "발주일 기준
      PERFORM GET_DATA_BEDAT.

    WHEN P_RB.  "전기일 기준
      PERFORM GET_DATA_BUDAT.

*U1> 입고기준/송장기준 검색 기능 추가
    WHEN P_GR.  "입고 기준
      PERFORM GET_DATA_BY_GR.

    WHEN P_IR.  "송장 기준
      PERFORM GET_DATA_BY_IR.
  ENDCASE.

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
*& Form GET_DATA_BEDAT
*&---------------------------------------------------------------------*
FORM GET_DATA_BEDAT.

  PERFORM GET_DATA_BSTYP_L. "SA는 품목납품일 기준으로 조회

  PERFORM GET_DATA_BSTYP_F. "PO는 생성일 기준으로 조회

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BSTYP_L
*&---------------------------------------------------------------------*
FORM GET_DATA_BSTYP_L.

* EKPO 기준
  SELECT B~BUKRS,
         B~EKORG,
         B~BEDAT,
         B~BSTYP,
         B~BSART,
         B~LIFNR,
         D~KALSK,
         A~EBELN,
         A~EBELP,

         B~ZEMANAGE2,
         A~MATKL,
         A~MATNR,
         A~TXZ01,
         A~BWTAR,
         F~BKLAS,

         E~BUDAT,
         A~ELIKZ,
         A~MENGE,
         A~MEINS,

         A~NETPR,
         A~NETWR,
         B~WAERS,

         B~ZTERM,
         A~INFNR,
         B~ZORDER_PERSON,
*         G~EMPLOY_NAME AS ZORDER_PERSON_NAME,
         B~ZORDER_DEPARTMENT,
*         G1~ORGN_NM AS ZORDER_DEPARTMENT_NAME,

         A~KNTTP,

         A~RETPO,
         A~REPOS,

         E~GJAHR AS GJAHR_EKBE,
         E~BELNR AS BELNR_EKBE,
         E~MENGE AS MENGE_EKBE,
         E~WRBTR AS WRBTR_EKBE,
         E~DMBTR AS DMBTR_EKBE,
         E~AREWR AS AREWR_EKBE,
         E~WAERS AS WAERS_EKBE,

         A~KONNR,
         A~KTPNR,

         E~WERKS,
         E~VGABE,
         E~SHKZG,
         E~BWART,
         A~PSTYP,
         B~EKGRP,
         A~LOEKZ  "U2(T2)> LOEKZ 추가
    FROM EKPO AS A INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN EKET AS C
                           ON A~EBELN = C~EBELN
                          AND A~EBELP = C~EBELP
                   INNER JOIN LFM1 AS D
                           ON B~LIFNR = D~LIFNR
                          AND B~EKORG = D~EKORG
                    LEFT JOIN EKBE AS E
                           ON A~EBELN = E~EBELN
                          AND A~EBELP = E~EBELP
                          AND E~VGABE IN ('1','2','3')   "입고, 송장, 차후 차변/대변
                    LEFT JOIN MBEW AS F
                           ON A~MATNR = F~MATNR
                          AND A~WERKS = F~BWKEY
                          AND A~BWTAR = F~BWTAR
*                    LEFT JOIN ZSVMM_USER_INFO AS G
*                           ON B~ZORDER_PERSON     = G~EMPLOY_NO
*                          AND B~BUKRS = G~COMPANY
*                    LEFT JOIN ZTCN00001 AS G1
*                           ON G1~ORGN_CD = B~ZORDER_DEPARTMENT
*                          AND G1~BUKRS = B~BUKRS
   WHERE A~WERKS             IN @S_WERKS
     AND A~MATKL             IN @S_MATKL
     AND A~MATNR             IN @S_MATNR
     AND A~EBELN             IN @S_EBELN
     AND A~KONNR             IN @S_KONNR
     AND B~BSTYP              = 'L'
     AND C~EINDT             IN @S_BEDAT
     AND B~BUKRS              = @P_BUKRS
     AND B~BSART             IN @S_BSART
     AND B~EKGRP             IN @S_EKGRP
     AND B~LIFNR             IN @S_LIFNR
     AND B~ZORDER_PERSON     IN @S_ORDER
     AND B~ZORDER_DEPARTMENT IN @S_DEPTO
     AND B~BSTYP             IN @GR_BSTYP
     AND B~ZEMANAGE2         IN @S_MANGE
     AND A~MTART             IN @S_MTART
     AND A~LOEKZ             IN @GR_LOEKZ
     AND A~PSTYP             IN @S_PSTYP
     AND A~KNTTP             IN @S_KNTTP
     AND A~RETPO             IN @GR_RETPO
     AND A~REPOS             IN @GR_REPOS
     AND D~KALSK             IN @GR_KALSK
     AND F~BKLAS             IN @S_BKLAS
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

  SORT GT_DATA BY EBELN EBELP VGABE GJAHR_EKBE BELNR_EKBE.

* EKET-ETENR로 인한 중복 제거
  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING EBELN EBELP VGABE GJAHR_EKBE BELNR_EKBE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BSTYP_F
*&---------------------------------------------------------------------*
FORM GET_DATA_BSTYP_F.

* EKPO 기준
  SELECT B~BUKRS,
         B~EKORG,
         B~BEDAT,
         B~BSTYP,
         B~BSART,
         B~LIFNR,
         C~KALSK,
         A~EBELN,
         A~EBELP,
         B~ZEMANAGE2,
         A~MATKL,
         A~MATNR,
         A~TXZ01,
         A~BWTAR,
         E~BKLAS,

         D~BUDAT,
         A~ELIKZ,
         A~MENGE,
         A~MEINS,

         A~NETPR,
         A~NETWR,
         B~WAERS,

         B~ZTERM,
         A~INFNR,
         B~ZORDER_PERSON,
*         F~EMPLOY_NAME AS ZORDER_PERSON_NAME,
         B~ZORDER_DEPARTMENT,
*         F1~ORGN_NM AS ZORDER_DEPARTMENT_NAME,

         A~KNTTP,

         A~RETPO,
         A~REPOS,

         D~MENGE AS MENGE_EKBE,
         D~WRBTR AS WRBTR_EKBE,
         D~DMBTR AS DMBTR_EKBE,
         D~AREWR AS AREWR_EKBE,
         D~WAERS AS WAERS_EKBE,

         A~KONNR,
         A~KTPNR,

         D~WERKS,
         D~VGABE,
         D~SHKZG,
         D~BWART,
         A~PSTYP,
         B~EKGRP,
         A~LOEKZ  "U2(T2)> LOEKZ 추가
    FROM EKPO AS A INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN LFM1 AS C
                           ON B~LIFNR = C~LIFNR
                          AND B~EKORG = C~EKORG
                    LEFT JOIN EKBE AS D
                           ON A~EBELN = D~EBELN
                          AND A~EBELP = D~EBELP
                          AND D~VGABE IN ('1','2','3')   "입고, 송장, 차후 차변/대변
                    LEFT JOIN MBEW AS E
                           ON A~MATNR = E~MATNR
                          AND A~WERKS = E~BWKEY
                          AND A~BWTAR = E~BWTAR
*                    LEFT JOIN ZSVMM_USER_INFO AS F
*                           ON B~ZORDER_PERSON     = F~EMPLOY_NO
*                          AND B~BUKRS = F~COMPANY
*                    LEFT JOIN ZTCN00001 AS F1
*                           ON F1~ORGN_CD = B~ZORDER_DEPARTMENT
*                          AND F1~BUKRS = B~BUKRS
   WHERE A~WERKS             IN @S_WERKS
     AND A~MATKL             IN @S_MATKL
     AND A~MATNR             IN @S_MATNR
     AND A~EBELN             IN @S_EBELN
     AND A~KONNR             IN @S_KONNR
     AND B~BSTYP              = 'F'
     AND B~BEDAT             IN @S_BEDAT
     AND B~BUKRS              = @P_BUKRS
     AND B~BSART             IN @S_BSART
     AND B~EKGRP             IN @S_EKGRP
     AND B~LIFNR             IN @S_LIFNR
     AND B~ZORDER_PERSON     IN @S_ORDER
     AND B~ZORDER_DEPARTMENT IN @S_DEPTO
     AND B~BSTYP             IN @GR_BSTYP
     AND B~ZEMANAGE2         IN @S_MANGE
     AND A~MTART             IN @S_MTART
     AND A~LOEKZ             IN @GR_LOEKZ
     AND A~PSTYP             IN @S_PSTYP
     AND A~KNTTP             IN @S_KNTTP
     AND A~RETPO             IN @GR_RETPO
     AND A~REPOS             IN @GR_REPOS
     AND C~KALSK             IN @GR_KALSK
     AND E~BKLAS             IN @S_BKLAS
  APPENDING CORRESPONDING FIELDS OF TABLE @GT_DATA.

  SORT GT_DATA BY EBELN EBELP VGABE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BUDAT
*&---------------------------------------------------------------------*
FORM GET_DATA_BUDAT.

* EKBE 기준
  SELECT B~BUKRS,
         B~EKORG,
         B~BEDAT,
         B~BSTYP,
         B~BSART,
         B~LIFNR,
         D~KALSK,
         A~EBELN,
         A~EBELP,
         B~ZEMANAGE2,
         C~MATKL,
         C~MATNR,
         C~TXZ01,
         C~BWTAR,
         E~BKLAS,

         A~BUDAT,
         C~ELIKZ,
         C~MENGE,
         C~MEINS,

         C~NETPR,
         C~NETWR,
         B~WAERS,

         B~ZTERM,
         C~INFNR,
         B~ZORDER_PERSON,
*         F~EMPLOY_NAME AS ZORDER_PERSON_NAME,
         B~ZORDER_DEPARTMENT,
*         F1~ORGN_NM AS ZORDER_DEPARTMENT_NAME,

         C~KNTTP,

         C~RETPO,
         C~REPOS,

         A~MENGE AS MENGE_EKBE,
         A~WRBTR AS WRBTR_EKBE,
         A~DMBTR AS DMBTR_EKBE,
         A~AREWR AS AREWR_EKBE,
         A~WAERS AS WAERS_EKBE,

         C~KONNR,
         C~KTPNR,

         A~WERKS,
         A~VGABE,
         A~SHKZG,
         A~BWART,
         C~PSTYP,
         B~EKGRP,
         C~LOEKZ  "U2(T2)> LOEKZ 추가
    FROM EKBE AS A INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN EKPO AS C
                           ON A~EBELN = C~EBELN
                          AND A~EBELP = C~EBELP
                   INNER JOIN LFM1 AS D
                           ON B~LIFNR = D~LIFNR
                          AND B~EKORG = D~EKORG
                    LEFT JOIN MBEW AS E
                           ON A~MATNR = E~MATNR
                          AND A~WERKS = E~BWKEY
                          AND C~BWTAR = E~BWTAR
*                    LEFT JOIN ZSVMM_USER_INFO AS F
*                           ON B~ZORDER_PERSON     = F~EMPLOY_NO
*                          AND B~BUKRS = F~COMPANY
*                    LEFT JOIN ZTCN00001 AS F1
*                           ON F1~ORGN_CD = B~ZORDER_DEPARTMENT
*                          AND F1~BUKRS = B~BUKRS
   WHERE A~VGABE             IN ('1','2','3')   "입고, 송장, 차후 차변/대변
     AND A~WERKS             IN @S_WERKS
     AND A~MATNR             IN @S_MATNR
     AND A~EBELN             IN @S_EBELN
     AND A~BUDAT             IN @S_BUDAT
     AND B~BUKRS              = @P_BUKRS
     AND B~BSART             IN @S_BSART
     AND B~EKGRP             IN @S_EKGRP
     AND B~LIFNR             IN @S_LIFNR
     AND B~ZORDER_PERSON     IN @S_ORDER
     AND B~ZORDER_DEPARTMENT IN @S_DEPTO
     AND B~BSTYP             IN @GR_BSTYP
     AND B~ZEMANAGE2         IN @S_MANGE
     AND C~KONNR             IN @S_KONNR
     AND C~MATKL             IN @S_MATKL
     AND C~MTART             IN @S_MTART
     AND C~LOEKZ             IN @GR_LOEKZ
     AND C~PSTYP             IN @S_PSTYP
     AND C~KNTTP             IN @S_KNTTP
*     AND c~pstyp             NE '2'           "위탁 제외
     AND C~RETPO             IN @GR_RETPO
     AND C~REPOS             IN @GR_REPOS
     AND D~KALSK             IN @GR_KALSK
     AND E~BKLAS             IN @S_BKLAS
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.

  SORT GT_DATA BY EBELN EBELP VGABE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BY_GR
*&---------------------------------------------------------------------*
FORM GET_DATA_BY_GR.

  CLEAR GT_GET_BASIC_PO.

**********************************************************************
* EKBE 기준
**********************************************************************
  SELECT A~EBELN, A~EBELP
    FROM MATDOC AS A INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN EKPO AS C
                           ON A~EBELN = C~EBELN
                          AND A~EBELP = C~EBELP
                   INNER JOIN LFM1 AS D
                           ON B~LIFNR = D~LIFNR
                          AND B~EKORG = D~EKORG
                    LEFT JOIN MBEW AS E
                           ON A~MATNR = E~MATNR
                          AND A~WERKS = E~BWKEY
                          AND C~BWTAR = E~BWTAR
   WHERE A~WERKS             IN @S_WERKS
     AND A~MATNR             IN @S_MATNR
     AND A~EBELN             IN @S_EBELN
     AND A~BUDAT             IN @S_BUDAT
     AND B~BUKRS              = @P_BUKRS
     AND B~BSART             IN @S_BSART
     AND B~EKGRP             IN @S_EKGRP
     AND B~LIFNR             IN @S_LIFNR
     AND B~ZORDER_PERSON     IN @S_ORDER
     AND B~ZORDER_DEPARTMENT IN @S_DEPTO
     AND B~BSTYP             IN @GR_BSTYP
     AND B~ZEMANAGE2         IN @S_MANGE
     AND C~KONNR             IN @S_KONNR
     AND C~MATKL             IN @S_MATKL
     AND C~MTART             IN @S_MTART
     AND C~LOEKZ             IN @GR_LOEKZ
     AND C~PSTYP             IN @S_PSTYP
     AND C~KNTTP             IN @S_KNTTP
     AND C~RETPO             IN @GR_RETPO
     AND C~REPOS             IN @GR_REPOS
     AND D~KALSK             IN @GR_KALSK
     AND E~BKLAS             IN @S_BKLAS
     AND A~CANCELLED NE 'X'   "취소건 제외
     AND A~CANCELLATION_TYPE NE '2' "취소건 제외
     AND A~REVERSAL_MOVEMENT NE 'X' "취소건 제외
    INTO CORRESPONDING FIELDS OF TABLE @GT_GET_BASIC_PO.

  SORT GT_GET_BASIC_PO BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM GT_GET_BASIC_PO COMPARING EBELN EBELP.

  IF NOT GT_GET_BASIC_PO[] IS INITIAL.
    PERFORM GET_DATA_BSTYP_L_01. "SA는 품목납품일 기준으로 조회

    PERFORM GET_DATA_BSTYP_F_01. "PO는 생성일 기준으로 조회
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BY_IR
*&---------------------------------------------------------------------*
FORM GET_DATA_BY_IR.

  CLEAR GT_GET_BASIC_PO.

**********************************************************************
* EKBE 기준
**********************************************************************
  SELECT A~EBELN, A~EBELP
    FROM SUPPLIERINVOICE AS Z JOIN RSEG AS A
     ON A~BELNR = Z~BELNR AND
        A~GJAHR = Z~GJAHR
         INNER JOIN EKKO AS B
                           ON A~EBELN = B~EBELN
                   INNER JOIN EKPO AS C
                           ON A~EBELN = C~EBELN
                          AND A~EBELP = C~EBELP
                   INNER JOIN LFM1 AS D
                           ON B~LIFNR = D~LIFNR
                          AND B~EKORG = D~EKORG
                    LEFT JOIN MBEW AS E
                           ON A~MATNR = E~MATNR
                          AND A~WERKS = E~BWKEY
                          AND C~BWTAR = E~BWTAR
   WHERE A~WERKS             IN @S_WERKS
     AND A~MATNR             IN @S_MATNR
     AND A~EBELN             IN @S_EBELN
     AND Z~BUDAT             IN @S_BUDAT
     AND A~BUKRS              = @P_BUKRS
     AND B~BSART             IN @S_BSART
     AND B~EKGRP             IN @S_EKGRP
     AND B~LIFNR             IN @S_LIFNR
     AND B~ZORDER_PERSON     IN @S_ORDER
     AND B~ZORDER_DEPARTMENT IN @S_DEPTO
     AND B~BSTYP             IN @GR_BSTYP
     AND B~ZEMANAGE2         IN @S_MANGE
     AND C~KONNR             IN @S_KONNR
     AND C~MATKL             IN @S_MATKL
     AND C~MTART             IN @S_MTART
     AND C~LOEKZ             IN @GR_LOEKZ
     AND C~PSTYP             IN @S_PSTYP
     AND C~KNTTP             IN @S_KNTTP
     AND C~RETPO             IN @GR_RETPO
     AND C~REPOS             IN @GR_REPOS
     AND D~KALSK             IN @GR_KALSK
     AND E~BKLAS             IN @S_BKLAS
     AND Z~STBLG             EQ @SPACE  "취소건 제외
    INTO CORRESPONDING FIELDS OF TABLE @GT_GET_BASIC_PO.

  SORT GT_GET_BASIC_PO BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM GT_GET_BASIC_PO COMPARING EBELN EBELP.

  PERFORM GET_DATA_BSTYP_L_01. "SA는 품목납품일 기준으로 조회

  PERFORM GET_DATA_BSTYP_F_01. "PO는 생성일 기준으로 조회

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OTHER_DATA
*&---------------------------------------------------------------------*
FORM GET_OTHER_DATA.

  CLEAR: GT_LFA1, GT_TMKKT, GT_MAKT, GT_T161T, GT_T163I, GT_EKKN, GT_T023T.

* 공급업체명
  DATA(LT_TMP) = GT_DATA[].
  SORT LT_TMP BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING LIFNR.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT LIFNR,
           NAME1
      INTO CORRESPONDING FIELDS OF TABLE @GT_LFA1
      FROM LFA1
       FOR ALL ENTRIES IN @LT_TMP
     WHERE LIFNR = @LT_TMP-LIFNR.
    FREE LT_TMP.

    SORT GT_LFA1 BY LIFNR.
  ENDIF.

* 구매문서유형 내역
  LT_TMP = GT_DATA[].
  SORT LT_TMP BY BSART BSTYP.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BSART BSTYP.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BSART,
           BSTYP,
           BATXT
      INTO CORRESPONDING FIELDS OF TABLE @GT_T161T
      FROM T161T
       FOR ALL ENTRIES IN @LT_TMP
     WHERE SPRAS = @SY-LANGU
       AND BSART = @LT_TMP-BSART
       AND BSTYP = @LT_TMP-BSTYP.
    FREE LT_TMP.
    SORT GT_T161T BY BSART BSTYP.
  ENDIF.

* 지급조건 내역
  LT_TMP = GT_DATA[].
  SORT LT_TMP BY ZTERM.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING ZTERM.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT ZTERM,
           TEXT1
      INTO CORRESPONDING FIELDS OF TABLE @GT_T052U
      FROM T052U
       FOR ALL ENTRIES IN @LT_TMP
     WHERE SPRAS = @SY-LANGU
       AND ZTERM = @LT_TMP-ZTERM.
    FREE LT_TMP.

    SORT GT_T052U BY ZTERM.
  ENDIF.

* 계정지정범주 내역
  LT_TMP = GT_DATA[].
  SORT LT_TMP BY KNTTP.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING KNTTP.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT KNTTP,
           KNTTX
      INTO CORRESPONDING FIELDS OF TABLE @GT_T163I
      FROM T163I
       FOR ALL ENTRIES IN @LT_TMP
     WHERE SPRAS = @SY-LANGU
       AND KNTTP = @LT_TMP-KNTTP.
    FREE LT_TMP.

    SORT GT_T163I BY KNTTP.
  ENDIF.


* 자재그룹명
  LT_TMP = GT_DATA[].
  SORT LT_TMP BY MATKL.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING MATKL.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT MATKL,
           WGBEZ60
      INTO CORRESPONDING FIELDS OF TABLE @GT_T023T
      FROM T023T
       FOR ALL ENTRIES IN @LT_TMP
     WHERE SPRAS = @SY-LANGU
       AND MATKL = @LT_TMP-MATKL.
    FREE LT_TMP.

    SORT GT_T023T BY MATKL.
  ENDIF.

  IF GT_DATA IS NOT INITIAL.
* 스키마그룹명(내외자 구분)
    SELECT KALSK,
           KALSB
      INTO CORRESPONDING FIELDS OF TABLE @GT_TMKKT
      FROM TMKKT
     WHERE SPRAS = @SY-LANGU.

    SORT GT_TMKKT BY KALSK.

* 계정지정 정보
    PERFORM GET_EKKN_INFO.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EKKN_INFO
*&---------------------------------------------------------------------*
FORM GET_EKKN_INFO.

  DATA(LT_DATA) = GT_DATA.
  SORT LT_DATA BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING EBELN EBELP.

  IF LT_DATA IS NOT INITIAL.

    SELECT EBELN,
           EBELP,
           KOKRS,
           SAKTO,
           PRCTR,
           KOSTL,
           FISTL,
           FIPOS,
           VBELN,
           VBELP,
           ANLN1,
           PS_PSP_PNR AS PSPNR,
           AUFNR
      INTO CORRESPONDING FIELDS OF TABLE @GT_EKKN
      FROM EKKN
       FOR ALL ENTRIES IN @LT_DATA
     WHERE EBELN = @LT_DATA-EBELN
       AND EBELP = @LT_DATA-EBELP.

  ENDIF.

*--------------------------------
* Get Text Data
*--------------------------------
* GL 계정
  DATA(LT_TMP_EKKN) = GT_EKKN[].
  SORT LT_TMP_EKKN BY SAKTO.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING SAKTO.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT SAKNR,
           TXT20
      INTO TABLE @DATA(LT_SKAT)
      FROM SKAT
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE SPRAS = @SY-LANGU
       AND KTOPL = @GC_KTOPL_K000
       AND SAKNR = @LT_TMP_EKKN-SAKTO.
    FREE LT_TMP_EKKN.

    SORT LT_SKAT BY SAKNR.
  ENDIF.

* 손익센터
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY PRCTR KOKRS.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING PRCTR KOKRS.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT PRCTR,
           KOKRS,
           KTEXT
      INTO TABLE @DATA(LT_CEPCT)
      FROM CEPCT
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE SPRAS = @SY-LANGU
       AND PRCTR = @LT_TMP_EKKN-PRCTR
       AND KOKRS = @LT_TMP_EKKN-KOKRS.
    FREE LT_TMP_EKKN.

    SORT LT_CEPCT BY PRCTR KOKRS.
  ENDIF.

* 코스트센터
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY KOKRS KOSTL.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING KOKRS KOSTL.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT KOSTL,
           KOKRS,
           KTEXT
      INTO TABLE @DATA(LT_CSKT)
      FROM CSKT
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE SPRAS = @SY-LANGU
       AND KOKRS = @LT_TMP_EKKN-KOKRS
       AND KOSTL = @LT_TMP_EKKN-KOSTL.
    FREE LT_TMP_EKKN.

    SORT LT_CSKT BY KOSTL KOKRS.
  ENDIF.

* Func Center
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY KOKRS FISTL.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING KOKRS FISTL.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT FIKRS,
           FICTR,
           BEZEICH
      INTO TABLE @DATA(LT_FMFCTRT)
      FROM FMFCTRT
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE SPRAS   = @SY-LANGU
       AND FIKRS   = @LT_TMP_EKKN-KOKRS
       AND FICTR   = @LT_TMP_EKKN-FISTL
       AND DATBIS >= @SY-DATUM
       AND DATAB  <= @SY-DATUM.
    FREE LT_TMP_EKKN.
    SORT LT_FMFCTRT BY FIKRS FICTR.
  ENDIF.

* 자산번호
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY KOKRS ANLN1.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING KOKRS ANLN1.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT ANLN1,
           BUKRS,
           TXT50
      INTO TABLE @DATA(LT_ANLA)
      FROM ANLA
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE BUKRS = @LT_TMP_EKKN-KOKRS
       AND ANLN1 = @LT_TMP_EKKN-ANLN1.
    FREE LT_TMP_EKKN.
    SORT LT_ANLA BY ANLN1 BUKRS.
  ENDIF.


* WBS 요소
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY PSPNR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING PSPNR.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT PSPNR,
           POST1
      INTO TABLE @DATA(LT_PRPS)
      FROM PRPS
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE PSPNR = @LT_TMP_EKKN-PSPNR.
    FREE LT_TMP_EKKN.
    SORT LT_PRPS BY PSPNR.
  ENDIF.

* Internal Order
  LT_TMP_EKKN = GT_EKKN[].
  SORT LT_TMP_EKKN BY AUFNR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP_EKKN COMPARING AUFNR.
  IF NOT LT_TMP_EKKN[] IS INITIAL.
    SELECT AUFNR,
           KTEXT
      INTO TABLE @DATA(LT_AUFK)
      FROM AUFK
       FOR ALL ENTRIES IN @LT_TMP_EKKN
     WHERE AUFNR = @LT_TMP_EKKN-AUFNR.
    FREE LT_TMP_EKKN.
    SORT LT_AUFK BY AUFNR.
  ENDIF.

*--------------------------------
* Modify Text Field
*--------------------------------
  LOOP AT GT_EKKN ASSIGNING FIELD-SYMBOL(<LS_EKKN>).

    "GL 계정
    READ TABLE LT_SKAT WITH KEY SAKNR = <LS_EKKN>-SAKTO
                                BINARY SEARCH
                                INTO DATA(LS_SKAT).
    IF SY-SUBRC = 0.
      <LS_EKKN>-SAKTO_TEXT = LS_SKAT-TXT20.
    ENDIF.

    "손익센터
    READ TABLE LT_CEPCT WITH KEY PRCTR = <LS_EKKN>-PRCTR
                                 KOKRS = <LS_EKKN>-KOKRS
                                 BINARY SEARCH
                                 INTO DATA(LS_CEPCT).
    IF SY-SUBRC = 0.
      <LS_EKKN>-PRCTR_TEXT = LS_CEPCT-KTEXT.
    ENDIF.

    "코스트센터
    READ TABLE LT_CSKT WITH KEY KOSTL = <LS_EKKN>-KOSTL
                                KOKRS = <LS_EKKN>-KOKRS
                                BINARY SEARCH
                                INTO DATA(LS_CSKT).
    IF SY-SUBRC = 0.
      <LS_EKKN>-KOSTL_TEXT = LS_CSKT-KTEXT.
    ENDIF.

    "Fund Center
    READ TABLE LT_FMFCTRT WITH KEY FIKRS = <LS_EKKN>-FISTL
                                   FICTR = <LS_EKKN>-KOKRS
                                   BINARY SEARCH
                                   INTO DATA(LS_FMFCTRT).
    IF SY-SUBRC = 0.
      <LS_EKKN>-FISTL_TEXT = LS_FMFCTRT-BEZEICH.
    ENDIF.

    "자산번호
    READ TABLE LT_ANLA WITH KEY ANLN1 = <LS_EKKN>-ANLN1
                                BUKRS = <LS_EKKN>-KOKRS
                                BINARY SEARCH
                                INTO DATA(LS_ANLA).
    IF SY-SUBRC = 0.
      <LS_EKKN>-ANLN1_TEXT = LS_ANLA-TXT50.
    ENDIF.

    "WBS 요소
    READ TABLE LT_PRPS WITH KEY PSPNR = <LS_EKKN>-PSPNR
                                BINARY SEARCH
                                INTO DATA(LS_PRPS).
    IF SY-SUBRC = 0.
      <LS_EKKN>-PSPNR_TEXT = LS_PRPS-POST1.
    ENDIF.

    "Internal Order
    READ TABLE LT_AUFK WITH KEY AUFNR = <LS_EKKN>-AUFNR
                                BINARY SEARCH
                                INTO DATA(LS_AUFK).
    IF SY-SUBRC = 0.
      <LS_EKKN>-AUFNR_TEXT = LS_AUFK-KTEXT.
    ENDIF.

  ENDLOOP.

  SORT GT_EKKN BY EBELN EBELP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ATTACH_DATA
*&---------------------------------------------------------------------*
FORM GET_ATTACH_DATA.

  DATA: LT_ATTA TYPE SORTED TABLE OF TY_ATTA WITH UNIQUE KEY INSTID_A.

  CLEAR GT_ATTA.

  LT_ATTA = CORRESPONDING #( GT_DATA DISCARDING DUPLICATES MAPPING INSTID_A = EBELN ).

  IF LT_ATTA IS NOT INITIAL.

    SELECT TYPEID_A,
           INSTID_A
      INTO TABLE @GT_ATTA
      FROM SRGBTBREL
       FOR ALL ENTRIES IN @LT_ATTA
     WHERE INSTID_A = @LT_ATTA-INSTID_A
       AND TYPEID_A IN ( @GC_GOS_TYPEID_PO, @GC_GOS_TYPEID_CO )
       AND CATID_A  = @GC_GOS_CATID.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_APV_DATA
*&---------------------------------------------------------------------*
FORM GET_APV_DATA.

*  TYPES: BEGIN OF LTY_PO,
*           EBELN TYPE ZTMM30032-WFOBJECT,
*         END OF LTY_PO.
*
*  DATA: LT_PO TYPE TABLE OF LTY_PO.
*
*  IF GT_DATA IS NOT INITIAL.
*    MOVE-CORRESPONDING GT_DATA TO LT_PO.
*    SORT LT_PO BY EBELN.
*    DELETE ADJACENT DUPLICATES FROM LT_PO COMPARING EBELN.
*  ENDIF.
*
*  IF LT_PO[] IS NOT INITIAL.
*
*    SELECT A~APVIFKEY,
*           A~FIID,
*           B~WFOBJECT,
*           A~APVSTATUS
*      FROM ZTMM30031 AS A INNER JOIN ZTMM30032 AS B
*                                  ON A~APVIFKEY = B~APVIFKEY
*       FOR ALL ENTRIES IN @LT_PO
*     WHERE B~WFOBJECT = @LT_PO-EBELN
*       AND B~MMWFOBJ  = 'PO'
**       AND A~APVSTATUS NE 'WITHDRAW'  "회수된 문서 제외
*      INTO TABLE @GT_APV.
*
*    SORT GT_APV BY WFOBJECT APVIFKEY DESCENDING.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BSTYP_L_01
*&---------------------------------------------------------------------*
FORM GET_DATA_BSTYP_L_01.

* EKPO 기준
  IF NOT GT_GET_BASIC_PO[] IS INITIAL.
    SELECT B~BUKRS,
           B~EKORG,
           B~BEDAT,
           B~BSTYP,
           B~BSART,
           B~LIFNR,
           D~KALSK,
           A~EBELN,
           A~EBELP,

           B~ZEMANAGE2,
           A~MATKL,
           A~MATNR,
           A~TXZ01,
           A~BWTAR,
           F~BKLAS,

           E~BUDAT,
           A~ELIKZ,
           A~MENGE,
           A~MEINS,

           A~NETPR,
           A~NETWR,
           B~WAERS,

           B~ZTERM,
           A~INFNR,
           B~ZORDER_PERSON,
*           G~EMPLOY_NAME AS ZORDER_PERSON_NAME,
           B~ZORDER_DEPARTMENT,
*           G1~ORGN_NM AS ZORDER_DEPARTMENT_NAME,

           A~KNTTP,

           A~RETPO,
           A~REPOS,

           E~GJAHR AS GJAHR_EKBE,
           E~BELNR AS BELNR_EKBE,
           E~MENGE AS MENGE_EKBE,
           E~WRBTR AS WRBTR_EKBE,
           E~DMBTR AS DMBTR_EKBE,
           E~AREWR AS AREWR_EKBE,
           E~WAERS AS WAERS_EKBE,

           A~KONNR,
           A~KTPNR,

           E~WERKS,
           E~VGABE,
           E~SHKZG,
           E~BWART,
           A~PSTYP,
           B~EKGRP,
           A~LOEKZ  "U2(T2)> LOEKZ 추가
      FROM EKPO AS A INNER JOIN EKKO AS B
                             ON A~EBELN = B~EBELN
                     INNER JOIN EKET AS C
                             ON A~EBELN = C~EBELN
                            AND A~EBELP = C~EBELP
                     INNER JOIN LFM1 AS D
                             ON B~LIFNR = D~LIFNR
                            AND B~EKORG = D~EKORG
                      LEFT JOIN EKBE AS E
                             ON A~EBELN = E~EBELN
                            AND A~EBELP = E~EBELP
                            AND E~VGABE IN ('1','2','3')   "입고, 송장, 차후 차변/대변
                      LEFT JOIN MBEW AS F
                             ON A~MATNR = F~MATNR
                            AND A~WERKS = F~BWKEY
                            AND A~BWTAR = F~BWTAR
*                      LEFT JOIN ZSVMM_USER_INFO AS G
*                             ON B~ZORDER_PERSON     = G~EMPLOY_NO
*                            AND B~BUKRS = G~COMPANY
*                      LEFT JOIN ZTCN00001 AS G1
*                             ON G1~ORGN_CD = B~ZORDER_DEPARTMENT
*                            AND G1~BUKRS = B~BUKRS
      FOR ALL ENTRIES IN @GT_GET_BASIC_PO
     WHERE A~EBELN = @GT_GET_BASIC_PO-EBELN
       AND A~EBELP = @GT_GET_BASIC_PO-EBELP
       AND B~BSTYP              = 'L'
       AND C~EINDT             IN @S_BEDAT
      INTO CORRESPONDING FIELDS OF TABLE @GT_DATA.
  ENDIF.
  SORT GT_DATA BY EBELN EBELP VGABE GJAHR_EKBE BELNR_EKBE.

* EKET-ETENR로 인한 중복 제거
  DELETE ADJACENT DUPLICATES FROM GT_DATA COMPARING EBELN EBELP VGABE GJAHR_EKBE BELNR_EKBE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BSTYP_F_01
*&---------------------------------------------------------------------*
FORM GET_DATA_BSTYP_F_01.

* EKPO 기준
  IF NOT GT_GET_BASIC_PO[] IS INITIAL.
    SELECT B~BUKRS,
           B~EKORG,
           B~BEDAT,
           B~BSTYP,
           B~BSART,
           B~LIFNR,
           C~KALSK,
           A~EBELN,
           A~EBELP,
           B~ZEMANAGE2,
           A~MATKL,
           A~MATNR,
           A~TXZ01,
           A~BWTAR,
           E~BKLAS,

           D~BUDAT,
           A~ELIKZ,
           A~MENGE,
           A~MEINS,

           A~NETPR,
           A~NETWR,
           B~WAERS,

           B~ZTERM,
           A~INFNR,
           B~ZORDER_PERSON,
*           F~EMPLOY_NAME AS ZORDER_PERSON_NAME,
           B~ZORDER_DEPARTMENT,
*           F1~ORGN_NM AS ZORDER_DEPARTMENT_NAME,

           A~KNTTP,

           A~RETPO,
           A~REPOS,

           D~GJAHR AS GJAHR_EKBE,
           D~BELNR AS BELNR_EKBE,
           D~MENGE AS MENGE_EKBE,
           D~WRBTR AS WRBTR_EKBE,
           D~DMBTR AS DMBTR_EKBE,
           D~AREWR AS AREWR_EKBE,
           D~WAERS AS WAERS_EKBE,

           A~KONNR,
           A~KTPNR,

           D~WERKS,
           D~VGABE,
           D~SHKZG,
           D~BWART,
           A~PSTYP,
           B~EKGRP,
           A~LOEKZ  "U2(T2)> LOEKZ 추가
      FROM EKPO AS A INNER JOIN EKKO AS B
                             ON A~EBELN = B~EBELN
                     INNER JOIN LFM1 AS C
                             ON B~LIFNR = C~LIFNR
                            AND B~EKORG = C~EKORG
                      LEFT JOIN EKBE AS D
                             ON A~EBELN = D~EBELN
                            AND A~EBELP = D~EBELP
                            AND D~VGABE IN ('1','2','3')   "입고, 송장, 차후 차변/대변
                      LEFT JOIN MBEW AS E
                             ON A~MATNR = E~MATNR
                            AND A~WERKS = E~BWKEY
                            AND A~BWTAR = E~BWTAR
*                      LEFT JOIN ZSVMM_USER_INFO AS F
*                             ON B~ZORDER_PERSON     = F~EMPLOY_NO
*                            AND B~BUKRS = F~COMPANY
*                      LEFT JOIN ZTCN00001 AS F1
*                             ON F1~ORGN_CD = B~ZORDER_DEPARTMENT
*                            AND F1~BUKRS = B~BUKRS
      FOR ALL ENTRIES IN @GT_GET_BASIC_PO
     WHERE A~EBELN = @GT_GET_BASIC_PO-EBELN
       AND A~EBELP = @GT_GET_BASIC_PO-EBELP
       AND B~BSTYP              = 'F'
       AND B~BEDAT             IN @S_BEDAT
    APPENDING CORRESPONDING FIELDS OF TABLE @GT_DATA.
  ENDIF.

  SORT GT_DATA BY EBELN EBELP VGABE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA.

  DATA: LS_DISP TYPE TY_DISP.

  DATA: LV_MENGE_GR(16)   TYPE P DECIMALS 3,
        LV_WRBTR_GR(16)   TYPE P DECIMALS 2,
        LV_DMBTR_GR(16)   TYPE P DECIMALS 2,
        LV_MENGE_IV(16)   TYPE P DECIMALS 3,
        LV_WRBTR_IV(16)   TYPE P DECIMALS 2,
        LV_DMBTR_IV(16)   TYPE P DECIMALS 2,
        LV_DMBTR_GRIV(16) TYPE P DECIMALS 2.

  DATA LV_END.

  DATA: LS_LIFNR_SUM TYPE TY_LIFNR_SUM,
        LS_MATNR_SUM TYPE TY_MATNR_SUM,
        LS_MATKL_SUM TYPE TY_MATKL_SUM,
        LS_ORDER_SUM TYPE TY_ORDER_SUM,
        LS_DEPTO_SUM TYPE TY_DEPTO_SUM,
        LS_BKLAS_SUM TYPE TY_BKLAS_SUM.


  CHECK GT_DATA IS NOT INITIAL.

  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<LS_DATA>).

    AT NEW EBELP.
      CLEAR: LV_MENGE_GR,   LV_WRBTR_GR, LV_DMBTR_GR,
             LV_MENGE_IV,   LV_WRBTR_IV, LV_DMBTR_IV,
             LV_DMBTR_GRIV, LV_END.
    ENDAT.

    IF <LS_DATA>-RETPO = 'X'.
      <LS_DATA>-MENGE = <LS_DATA>-MENGE * -1.
    ENDIF.

* 차/대변 지시자에 따른 부호 조정
    IF <LS_DATA>-SHKZG = 'H'.
      <LS_DATA>-MENGE_EKBE = <LS_DATA>-MENGE_EKBE * -1.
      <LS_DATA>-WRBTR_EKBE = <LS_DATA>-WRBTR_EKBE * -1.
      <LS_DATA>-DMBTR_EKBE = <LS_DATA>-DMBTR_EKBE * -1.
      <LS_DATA>-AREWR_EKBE = <LS_DATA>-AREWR_EKBE * -1.
    ENDIF.


    CASE <LS_DATA>-VGABE.
      WHEN '1'.  "입고

        LV_MENGE_GR = LV_MENGE_GR + <LS_DATA>-MENGE_EKBE.
        LV_WRBTR_GR = LV_WRBTR_GR + <LS_DATA>-WRBTR_EKBE.
        LV_DMBTR_GR = LV_DMBTR_GR + <LS_DATA>-DMBTR_EKBE.

      WHEN '2'.  "송장

        LV_MENGE_IV = LV_MENGE_IV + <LS_DATA>-MENGE_EKBE.
        LV_DMBTR_IV = LV_DMBTR_IV + <LS_DATA>-DMBTR_EKBE.

        "송장금액합은 PO와 같은 통화만 더함 (원화는 모두 더함)
        IF <LS_DATA>-WAERS = <LS_DATA>-WAERS_EKBE.
          LV_WRBTR_IV = LV_WRBTR_IV + <LS_DATA>-WRBTR_EKBE.
        ENDIF.

      WHEN '3'.  "차후차변/대변 : 수량제외

        "송장금액합은 PO와 같은 통화만 더함 (원화는 모두 더함)
        IF <LS_DATA>-WAERS = <LS_DATA>-WAERS_EKBE.
          LV_WRBTR_IV = LV_WRBTR_IV + <LS_DATA>-WRBTR_EKBE.
        ENDIF.

        LV_DMBTR_IV = LV_DMBTR_IV + <LS_DATA>-DMBTR_EKBE.

      WHEN OTHERS.
    ENDCASE.


    "GR/IR계정 반제값
    IF <LS_DATA>-AREWR_EKBE IS NOT INITIAL.
      LV_DMBTR_GRIV = LV_DMBTR_GRIV + ( <LS_DATA>-DMBTR_EKBE - <LS_DATA>-AREWR_EKBE ).
    ENDIF.

    AT END OF EBELP.
      LV_END = 'X'.
    ENDAT.

    CHECK LV_END = 'X'.

    LS_DISP = <LS_DATA>.


* 입고 SUM
    LS_DISP-MENGE_GR_SUM = LV_MENGE_GR.
    LS_DISP-WRBTR_GR_SUM = LV_WRBTR_GR.
    LS_DISP-DMBTR_GR_SUM = LV_DMBTR_GR.

    "미입고수량 (발주수량 - 입고수량)
    LS_DISP-MENGE_OP_GR_SUM = LS_DISP-MENGE - LV_MENGE_GR.

    IF LS_DISP-MENGE_OP_GR_SUM < 0.
      LS_DISP-MENGE_OP_GR_SUM = 0.
    ELSE.
      IF LS_DISP-ELIKZ = 'X'. "납품완료인 경우
        LS_DISP-MENGE_OP_GR_SUM = 0.
      ENDIF.
    ENDIF.

* 송장 SUM
    LS_DISP-MENGE_IV_SUM = LV_MENGE_IV.
    LS_DISP-WRBTR_IV_SUM = LV_WRBTR_IV.
    LS_DISP-DMBTR_IV_SUM = LV_DMBTR_IV.
*    LS_DISP-WAERS_EKBE   = LS_DISP-WAERS.

* 미착 SUM
    LS_DISP-MENGE_OP_SUM = LV_MENGE_IV - LV_MENGE_GR.
    LS_DISP-WRBTR_OP_SUM = LV_WRBTR_IV - LV_WRBTR_GR.
    LS_DISP-DMBTR_OP_SUM = LV_DMBTR_IV - LV_DMBTR_GR.

    "미착수량이 0 인 경우
    IF LS_DISP-MENGE_OP_SUM IS INITIAL.
      LS_DISP-WRBTR_OP_SUM = LS_DISP-DMBTR_OP_SUM = 0.
      LV_DMBTR_GRIV        = 0.
    ENDIF.

    "미착금액원화가 0 인 경우
    IF LS_DISP-DMBTR_OP_SUM IS INITIAL.
      LS_DISP-WRBTR_OP_SUM = 0.
    ENDIF.

* GR/IR반제차이
    LS_DISP-DMBTR_GRIR_SUM = LV_DMBTR_GRIV.

* 무상품목지시자 (송장수령 안하면 무상품목)
    IF LS_DISP-REPOS = 'X'.
      CLEAR LS_DISP-REPOS.
    ELSE.
      LS_DISP-REPOS = 'X'.
      CLEAR LS_DISP-MENGE_OP_SUM.
    ENDIF.

* 납품완료지시자
    IF LS_DISP-ELIKZ = 'X'.
      LS_DISP-ELIKZ_ICON = ICON_OKAY.
    ENDIF.

* 상태 (미착수량에 대한 상태)
    IF LS_DISP-MENGE_OP_SUM = 0.
      LS_DISP-STATUS = ICON_LED_GREEN.
    ELSE.
      LS_DISP-STATUS = ICON_LED_RED.
    ENDIF.

* 내역
    PERFORM SET_TEXT_FIELD CHANGING LS_DISP.

* 첨부파일
    PERFORM FILE_FIND USING LS_DISP-BSTYP LS_DISP-EBELN
                             CHANGING LS_DISP-AFILE.

* 결재문서 ICON
*    READ TABLE GT_APV WITH KEY WFOBJECT = LS_DISP-EBELN
*                               BINARY SEARCH
*                               INTO DATA(LS_APV).
*    IF SY-SUBRC = 0.
*      LS_DISP-APV_STAT = ICON_VIEWER_OPTICAL_ARCHIVE.
*    ENDIF.
*
*    LOOP AT GT_ZTMM45071 INTO DATA(LS_45071) WHERE EBELN = LS_DISP-EBELN
*                                                                      AND EBELP = LS_DISP-EBELP.
*      LS_DISP-ACTEX_SUM = LS_DISP-ACTEX_SUM + LS_45071-ACTEX.
*
*    ENDLOOP.

*U2(T2)> 삭제지시자 체크박스 설정을 위함. - START
    IF NOT LS_DISP-LOEKZ IS INITIAL.
      LS_DISP-LOEKZ = 'X'.
    ENDIF.
*U2(T2)> 삭제지시자 체크박스 설정을 위함. - END

    APPEND LS_DISP TO GT_DISP.

*--------------------------------
* 합계 기준 별 Collect
*--------------------------------
    CLEAR: LS_LIFNR_SUM, LS_MATNR_SUM, LS_MATKL_SUM, LS_ORDER_SUM, LS_DEPTO_SUM, LS_BKLAS_SUM.

    MOVE-CORRESPONDING LS_DISP TO: LS_LIFNR_SUM, LS_MATNR_SUM, LS_MATKL_SUM,
                                   LS_ORDER_SUM, LS_DEPTO_SUM, LS_BKLAS_SUM.

*'KRW' 대신 Local Currency 로 적용
*    LS_BKLAS_SUM-WAERS = GC_CUKY_KRW.
    LS_BKLAS_SUM-WAERS = GV_LOCAL_WAERS.

    COLLECT: LS_LIFNR_SUM INTO GT_LIFNR_SUM,
             LS_MATNR_SUM INTO GT_MATNR_SUM,
             LS_MATKL_SUM INTO GT_MATKL_SUM,
             LS_ORDER_SUM INTO GT_ORDER_SUM,
             LS_DEPTO_SUM INTO GT_DEPTO_SUM,
             LS_BKLAS_SUM INTO GT_BKLAS_SUM.

  ENDLOOP.

  GV_CRITERIA = GC_CRITERIA_ALL.
  GT_ALL = GT_DISP.

  DESCRIBE TABLE GT_DISP LINES DATA(LV_TCNT).
  MESSAGE S011(ZMM01) WITH LV_TCNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT_FIELD
*&---------------------------------------------------------------------*
FORM SET_TEXT_FIELD CHANGING CS_DISP TYPE TY_DISP.

* 공급업체명
  READ TABLE GT_LFA1 WITH KEY LIFNR = CS_DISP-LIFNR
                              BINARY SEARCH
                              INTO DATA(LS_LFA1).
  IF SY-SUBRC EQ 0.
    CS_DISP-LIFNR_TEXT = LS_LFA1-NAME1.
  ENDIF.

* 구매문서유형 내역
  READ TABLE GT_T161T WITH KEY BSART = CS_DISP-BSART
                               BSTYP = CS_DISP-BSTYP
                               BINARY SEARCH
                               INTO DATA(LS_T161T).
  IF SY-SUBRC EQ 0.
    CS_DISP-BATXT = LS_T161T-BATXT.
  ENDIF.

* 지급조건 내역
  READ TABLE GT_T052U WITH KEY ZTERM = CS_DISP-ZTERM
                               BINARY SEARCH
                               INTO DATA(LS_T052U).
  IF SY-SUBRC = 0.
    CS_DISP-VTEXT = LS_T052U-TEXT1.
  ENDIF.

* 내외자구분
  READ TABLE GT_TMKKT WITH KEY KALSK = CS_DISP-KALSK
                               BINARY SEARCH
                               INTO DATA(LS_TMKKT).
  IF SY-SUBRC = 0.
    CS_DISP-KALSB = LS_TMKKT-KALSB.
  ENDIF.

* 계정지정범주 내역
  READ TABLE GT_T163I WITH KEY KNTTP = CS_DISP-KNTTP
                               BINARY SEARCH
                               INTO DATA(LS_T163I).
  IF SY-SUBRC EQ 0.
    CS_DISP-KNTTX = LS_T163I-KNTTX.
  ENDIF.

* 계정지정정보
  READ TABLE GT_EKKN WITH KEY EBELN = CS_DISP-EBELN
                              EBELP = CS_DISP-EBELP
                              BINARY SEARCH
                              INTO DATA(LS_EKKN).
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING LS_EKKN TO CS_DISP.
  ENDIF.

* 자재그룹명
  READ TABLE GT_T023T WITH KEY MATKL = CS_DISP-MATKL
                               BINARY SEARCH
                               INTO DATA(LS_T023T).
  IF SY-SUBRC EQ 0.
    CS_DISP-WGBEZ = LS_T023T-WGBEZ60.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILE_FIND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_BSTYP
*&      --> LS_DISP_EBELN
*&      <-- LS_DISP_AFILE
*&---------------------------------------------------------------------*
FORM FILE_FIND USING IV_BSTYP IV_EBELN
                CHANGING CV_AFILE.

  CASE IV_BSTYP.
    WHEN 'F'. "구매오더
      READ TABLE GT_ATTA WITH KEY TYPEID_A = GC_GOS_TYPEID_PO
                                  INSTID_A = IV_EBELN
                                  TRANSPORTING NO FIELDS.
    WHEN 'K'. "계약
      READ TABLE GT_ATTA WITH KEY TYPEID_A = GC_GOS_TYPEID_CO
                                  INSTID_A = IV_EBELN
                                  TRANSPORTING NO FIELDS.
    WHEN OTHERS.
      READ TABLE GT_ATTA WITH KEY TYPEID_A = SPACE
                                  INSTID_A = IV_EBELN
                                  TRANSPORTING NO FIELDS.
  ENDCASE.

  IF SY-SUBRC = 0.
    CV_AFILE = ICON_ATTACHMENT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
FORM SET_F4_BSART USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '구매 문서 유형',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'BSART'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* Get Data
  SELECT BSTYP,
         BSART,
         BATXT
    FROM T161T
   WHERE SPRAS  = @SY-LANGU
     AND BSTYP IN ( 'F', 'L' )        "계약/PO 문서유형
    INTO TABLE @DATA(LT_T161T).

  SORT LT_T161T BY BSTYP BSART.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T161T
                                              LT_RETURN
                                       USING  LC_TITLE
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
*& Form F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_T161T
*&      --> LT_RETURN
*&      --> LC_TITLE
*&      --> LC_RETFIELD
*&      --> IV_SCR_NAME
*&---------------------------------------------------------------------*
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
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
FORM SET_F4_EKGRP USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '구매그룹 정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.

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
                                       USING  LC_TITLE
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
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- P_BUKRS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_READ
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
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX.

* 회사코드
  PERFORM SET_LIST_BOX_BUKRS USING 'P_BUKRS'.

* 내외자구분
  PERFORM SET_LIST_BOX_KALSK USING 'P_KALSK'.

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
    EXPORTING
      IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
      IT_WHERE  = VALUE #( ( FIELD = 1 VALUE = 'BUKRS' ) )
    IMPORTING
      ET_OUTTAB = DATA(LT_CONFIG) ).

  LT_LIST = CORRESPONDING #( LT_CONFIG MAPPING KEY = FIELD2  TEXT = FIELD3  ).

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_KALSK
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX_KALSK USING IV_FNAME.

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

  SELECT KALSK,
         KALSB
    INTO TABLE @DATA(LT_HELP)
    FROM TMKKT
   WHERE SPRAS = @SY-LANGU.

  LOOP AT LT_HELP INTO DATA(LS_HELP).

    IF LS_HELP-KALSK IS INITIAL.
      LS_HELP-KALSB = 'ALL'.
    ENDIF.

    _L_APPEND_VALUES: LS_HELP-KALSK  LS_HELP-KALSB.

  ENDLOOP.

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
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SEL_SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

* 사용자 아이디에 따른 필드 처리.
  ZCL_MM_COMMON=>COMMON_CONFIG(
    EXPORTING
      IS_COMMON = VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
      IT_WHERE  = VALUE #( ( FIELD = 1 VALUE = SY-CPROG )
                                      ( FIELD = 3 VALUE = SY-UNAME )  )
    IMPORTING
      ET_OUTTAB = DATA(LT_CONFIG) ).
  DATA(LS_CONFIG) = VALUE #( LT_CONFIG[ 1 ] OPTIONAL ).


  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
      WHEN 'ADT'. "발주일 기준
        IF P_RB = 'X'.
          SCREEN-ACTIVE = 0.
        ENDIF.

      WHEN 'BDT'. "전기일 기준
        IF P_RA = 'X'.
          SCREEN-ACTIVE = 0.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

*    IF LS_CONFIG-FIELD2 IS NOT INITIAL.
*      CASE LS_CONFIG-FIELD2.
**        WHEN 'EX01'.
*        WHEN 'EX02'.
*          IF SCREEN-NAME = 'P_BUKRS'.
*            SCREEN-INPUT = 0.
*          ENDIF.
*        WHEN 'EX03'.
*          IF SCREEN-NAME = 'P_BUKRS' OR
*              SCREEN-NAME+0(7) = 'S_DEPTO'.
*            SCREEN-INPUT = 0.
*          ENDIF.
*      ENDCASE.
*
*    ELSE.
*      IF SCREEN-NAME = 'P_BUKRS'  OR
**              SCREEN-NAME+0(7) = 'S_ORDER' OR
*          SCREEN-NAME+0(7) = 'S_DEPTO'.
*        SCREEN-INPUT = 0.
*      ENDIF.
*
*    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCREEN_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_SCREEN_DATA .
**U1> 입고기준/송장기준 검색 기능 선택 시 발주일 기본값 제거
  IF P_GR = 'X' OR P_IR = 'X'.
    CLEAR: S_BEDAT, S_BEDAT[].
  ENDIF.
ENDFORM.
