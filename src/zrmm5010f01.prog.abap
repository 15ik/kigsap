*&---------------------------------------------------------------------*
*& Include          ZRMM5010F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .

  CLEAR: GT_RSEG, GT_DISP, GT_DISP_DTL, GT_DISP_DTL_ALL,
         GV_WAERS_LOCAL.

  SELECT SINGLE WAERS
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @GV_WAERS_LOCAL.

  PERFORM GET_AP_DATA.

*> 회계전표 검색데이터 설정 및 공급가액 계산
  LOOP AT GT_RSEG INTO DATA(LS_RSEG).
    LS_RSEG-AWKEY = LS_RSEG-BELNR && LS_RSEG-GJAHR.
    LS_RSEG-DMBTR = LS_RSEG-RMWWR - LS_RSEG-WMWST1.
    MODIFY GT_RSEG FROM LS_RSEG.
  ENDLOOP.

*> 회계전표를 검색하기 위함.
  DATA(LT_RBKP) = GT_RSEG[].
  SORT LT_RBKP BY BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_RBKP COMPARING BELNR GJAHR.

  CHECK LT_RBKP[] IS NOT INITIAL.

*> 회계전표조회
  DATA(LT_TMP) = LT_RBKP[].
  SORT LT_TMP BY AWKEY.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING AWKEY.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BELNR, GJAHR, AWKEY, BKTXT
      FROM BKPF
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BUKRS EQ @P_BUKRS
       AND BLART EQ @GC_RE
       AND AWKEY EQ @LT_TMP-AWKEY
      INTO TABLE @DATA(LT_BKPF).

    FREE LT_TMP.
    "전표는 빠른순으로.. 2개이상이 될 수 있음...
    SORT LT_BKPF BY AWKEY GJAHR BELNR.
  ENDIF.

*> 전자세금계산서 조회를 위함.
*  DATA(LT_TMP_BKPF) = LT_BKPF[].
*  SORT LT_TMP_BKPF BY BELNR GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_BKPF COMPARING BELNR GJAHR.
*  IF NOT LT_TMP_BKPF[] IS INITIAL.
*    SELECT  A~BELNR AS ED_BELNR,    "FI 회계전표
*            A~ZGJAH AS ED_GJAHR,     "FI 회계년도
*            A~INV_SEQ,            "IV SEQ
*            C~ISSUE_ID,           "승인번호
*            C~TYPE_CODE,          "세금계산서 종류
*            C~DESC_TEXT1,         "
*            C~INV_SIGN,           "발행구문
*            C~AMEND_CODE,         "수정사유
*            C~STATUS AS ED_STATUS,             "발행상태
*            C~NTS_SEND_FLAG,      "국세청전송여부
*            B~USE_DOC             "맵핑여부
*            FROM ZDTV3T_AP_EXT_D AS A INNER JOIN ZDTV3T_AP_EXT  AS B
*              ON A~BUKRS       = B~BUKRS      AND
*                 A~ISSUE_DATE  = B~ISSUE_DATE AND
*                 A~BUPLA       = B~BUPLA      AND
*                 A~INV_SEQ     = B~INV_SEQ
*                                      INNER JOIN ZDTV3T_AP_HEAD AS C
*              ON A~BUKRS       = C~BUKRS      AND
*                 A~ISSUE_DATE  = C~ISSUE_DATE AND
*                 A~BUPLA       = C~BUPLA      AND
*                 A~INV_SEQ     = C~INV_SEQ
*             FOR ALL ENTRIES IN @LT_TMP_BKPF
*           WHERE A~BUKRS    EQ @P_BUKRS
*             AND A~BLART    EQ @GC_RE
*             AND A~BELNR    EQ @LT_TMP_BKPF-BELNR
*             AND A~ZGJAH    EQ @LT_TMP_BKPF-GJAHR
*             AND B~USE_DOC  EQ @ABAP_TRUE
*             AND C~ISSUE_ID IN @S_ISSID
*      INTO TABLE @DATA(LT_DTV3T).
*
*  ENDIF.

  SORT: LT_BKPF     BY AWKEY,
*        LT_DTV3T    BY ED_BELNR ED_GJAHR  ASCENDING,
        GT_RSEG BY BELNR GJAHR.

  LOOP AT LT_RBKP INTO DATA(LS_RBKP).

    CLEAR: GS_DISP.

    MOVE-CORRESPONDING LS_RBKP TO GS_DISP.

    READ TABLE LT_BKPF INTO DATA(LS_BKPF)
                       WITH KEY AWKEY = LS_RBKP-AWKEY
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISP-BELNR_FI = LS_BKPF-BELNR.
      GS_DISP-BKTXT_FI = LS_BKPF-BKTXT.
*> 전자세금계산서 조회를 위함.
*      READ TABLE LT_DTV3T INTO DATA(LS_DTV3T)
*                        WITH KEY ED_BELNR = LS_BKPF-BELNR
*                                 ED_GJAHR = LS_RBKP-GJAHR
*                        BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        MOVE-CORRESPONDING LS_DTV3T TO GS_DISP.
*      ELSE.
*        IF LS_RBKP-ABSGR = '02'.
*          GS_DISP-INV_SIGN = 'X'. "역발행
*        ENDIF.
*      ENDIF.
    ENDIF.
*
*    IF NOT GS_DISP-ISSUE_ID IN S_ISSID.
*      CONTINUE.
*    ENDIF.

*> IT_SELECT 필터용 데이터 설정.
    READ TABLE GT_RSEG WITH KEY BELNR = LS_RBKP-BELNR
                                GJAHR = LS_RBKP-GJAHR
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    LOOP AT GT_RSEG INTO GS_RSEG FROM SY-TABIX.
      IF GS_RSEG-BELNR NE LS_RBKP-BELNR OR
         GS_RSEG-GJAHR NE LS_RBKP-GJAHR.
        EXIT.
      ENDIF.
      DATA(LV_RSEG_TABIX) = SY-TABIX.

*      GS_RSEG-INV_SEQ = GS_DISP-INV_SEQ.
*      GS_RSEG-ISSUE_ID = GS_DISP-ISSUE_ID.
      GS_RSEG-BELNR_FI = GS_DISP-BELNR_FI.

*> 세금코드 없으면 RSEG 것으로 표시
      IF GS_DISP-MWSKZ1 IS INITIAL.
        GS_DISP-MWSKZ1 = GS_RSEG-MWSKZ_I.
      ENDIF.

      MODIFY GT_RSEG FROM GS_RSEG INDEX LV_RSEG_TABIX.
    ENDLOOP.

    APPEND GS_DISP TO GT_DISP.
    CLEAR  GS_DISP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_AP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_AP_DATA .

  DATA: LS_TMP LIKE GS_RSEG,
        LT_TMP LIKE TABLE OF GS_RSEG.

  DATA: LR_BSART TYPE RANGE OF EKKO-BSART.

  IF NOT S_WERKS[] IS INITIAL OR
     NOT S_MATNR[] IS INITIAL OR
     NOT S_EKGRP[] IS INITIAL OR
     NOT S_ORPSN[] IS INITIAL OR
     NOT S_ORDPT[] IS INITIAL OR
     NOT S_EXPSN[] IS INITIAL OR
     NOT S_EXDPT[] IS INITIAL OR
     NOT S_EBELN[] IS INITIAL.
    DATA(LV_ITEM_SEARCH) = 'X'.
  ENDIF.

  APPEND VALUE #( SIGN = 'E' OPTION = 'EQ' LOW = GC_BSART_PSIN ) TO LR_BSART.
  APPEND VALUE #( SIGN = 'E' OPTION = 'EQ' LOW = GC_BSART_PSIM ) TO LR_BSART.
  APPEND LINES OF S_BSART TO LR_BSART.

  CLEAR: GT_RSEG.

*-송장 데이터 조회
  SELECT
    A~BELNR,   "송장전표
    A~GJAHR,   "송장년도
    A~BUDAT,   "전기일
    A~BUPLA,   "사업장
    A~LIFNR,   "공급업체
    A~WMWST1, "부가세
    A~MWSKZ1, "세금코드
    A~RMWWR,  "계약금액
    A~WAERS,  "통화
    A~ERFNAM, "송장처리자

    A~BUKRS,   "회사코드
    A~BLDAT,   "증빙일
    A~STBLG,   "역분개 송장전표
    A~STJAH,   "역분개 송장년도

    A~ZFBDT,   "기준일
    A~ZTERM,   "지급조건
    A~ZLSCH,   "지급방법
    A~BVTYP,   "은행 계좌
    A~HBKID,   "HOUSE BANK
    A~KURSF,   "환율
    A~BEZNK,   "비계획 비용
    A~BKTXT,   "HEADER TEXT
    A~VGART,   "송장,차후 차/대변, 대변메모 구분1.(TS 유형)
    B~TBTKZ,   "송장,차후 차/대변, 대변메모 구분2.(후속 차/대변 지시자)

    C~EKGRP,   "구매그룹
    C~ABSGR,   "PO 의 세금계산서 발행방식
    C~ZEXPEN_PERSON, "지출결의자
    C~ZEXPEN_DEPARTMENT, "지출결의부서
    C~BSTYP,
    C~BSART,
    C~LIFNR AS LIFNR_PO, "공급업체 - 송장발행처와 다를 수 있음 (U1)


    D~NAME1 AS LIFNR_TX,   "공급업체명
    D~STCD2, "사업자 등록번호

    B~EBELN,   "PO 번호
    B~EBELP,   "PO 품목
    B~SHKZG,   "차/대변 구분
    B~BUZEI,   "송장 ITEM
    B~MWSKZ AS MWSKZ_I, "ITEM 세금코드

    E~KALSK

*    F~VGABE "TRANSACTION 유형

   FROM SUPPLIERINVOICE AS A JOIN RSEG AS B
     ON A~BELNR = B~BELNR AND
        A~GJAHR = B~GJAHR
                  INNER JOIN EKKO AS C
     ON B~EBELN = C~EBELN
                  INNER JOIN LFA1 AS D
     ON A~LIFNR = D~LIFNR
                  INNER JOIN LFM1 AS E
     ON A~LIFNR = E~LIFNR AND
        C~EKORG = E~EKORG
  WHERE A~BUKRS     EQ @P_BUKRS    "회사코드
    AND A~BUPLA     IN @S_BUPLA "사업장
    AND A~LIFNR     IN @S_LIFNR  "공급업체
    AND D~STCD2     IN @S_STCD2  "사업자번호
    AND B~WERKS     IN @S_WERKS "플랜트
    AND C~EKGRP     IN @S_EKGRP  "구매그룹
    AND C~ZORDER_PERSON     IN @S_ORPSN  "발주자
    AND C~ZORDER_DEPARTMENT IN @S_ORDPT  "발주부서
    AND C~ZEXPEN_PERSON     IN @S_EXPSN  "지출발의자
    AND C~ZEXPEN_DEPARTMENT IN @S_EXDPT  "지출발의부서
    AND A~BLDAT     IN @S_BLDAT  "세금계산서 작성일
    AND A~BUDAT     IN @S_BUDAT  "전기일
    AND A~BELNR     IN @S_BELNR  "송장번호
    AND A~ERFNAM    IN @S_USNAM "송장처리자
    AND B~EBELN     IN @S_EBELN
    AND B~MATNR     IN @S_MATNR
    AND A~STBLG     EQ @SPACE
    AND C~BSART     IN @LR_BSART  "수입관련 제외
*    AND E~KALSK     EQ 'DO'
    INTO CORRESPONDING FIELDS OF TABLE @GT_RSEG.

  LOOP AT GT_RSEG INTO DATA(LS_RSEG).
    "공사용역을 제외한 나머지는 국내 송장만 출력
    IF LS_RSEG-BSART NE GC_BSART_PSIC AND LS_RSEG-KALSK NE 'DO'.
      DELETE GT_RSEG.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING LS_RSEG TO LS_TMP.
    APPEND LS_TMP TO LT_TMP.
  ENDLOOP.

*> ITEM 검색조건때문에 한번 더 검색함.
  IF LV_ITEM_SEARCH = 'X'.

    SORT LT_TMP BY BELNR GJAHR.
    DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BELNR GJAHR.
    IF NOT LT_TMP[] IS INITIAL.
      CLEAR GT_RSEG.
      SELECT
        A~BELNR,   "송장전표
        A~GJAHR,   "송장년도
        A~BUDAT,   "전기일
        A~BLDAT,   "증빙일
        A~BUPLA,   "사업장
        A~LIFNR,   "공급업체
        A~WMWST1, "부가세
        A~MWSKZ1, "세금코드
        A~RMWWR,  "계약금액
        A~WAERS,  "통화
        A~ERFNAM, "송장처리자

        A~BUKRS,   "회사코드
        A~BLDAT,   "증빙일
        A~STBLG,   "역분개 송장전표
        A~STJAH,   "역분개 송장년도

        A~ZFBDT,   "기준일
        A~ZTERM,   "지급조건
        A~ZLSCH,   "지급방법
        A~BVTYP,   "은행 계좌
        A~HBKID,   "HOUSE BANK
        A~KURSF,   "환율
        A~BEZNK,   "비계획 비용
        A~BKTXT,   "HEADER TEXT
        A~VGART,   "송장,차후 차/대변, 대변메모 구분1.(TS 유형)
        B~TBTKZ,   "송장,차후 차/대변, 대변메모 구분2.(후속 차/대변 지시자)

        C~EKGRP,   "구매그룹
        C~ABSGR,   "PO 의 세금계산서 발행방식
        C~ZEXPEN_PERSON, "지출결의자
        C~ZEXPEN_DEPARTMENT, "지출결의부서
        C~BSTYP,
        C~BSART,
        C~LIFNR AS LIFNR_PO, "공급업체 - 송장발행처와 다를 수 있음 (U1)

        D~NAME1 AS LIFNR_TX,   "공급업체명
        D~STCD2, "사업자 등록번호

        B~EBELN,   "PO 번호
        B~EBELP,   "PO 품목
        B~SHKZG,   "차/대변 구분
        B~BUZEI,   "송장 ITEM
        B~MWSKZ AS MWSKZ_I, "ITEM 세금코드

        E~KALSK

*      F~VGABE "TRANSACTION 유형

       FROM SUPPLIERINVOICE AS A JOIN RSEG AS B
         ON A~BELNR = B~BELNR AND
            A~GJAHR = B~GJAHR
                      INNER JOIN EKKO AS C
         ON B~EBELN = C~EBELN
                      INNER JOIN LFA1 AS D
         ON A~LIFNR = D~LIFNR
                      INNER JOIN LFM1 AS E
         ON A~LIFNR = E~LIFNR AND
            C~EKORG = E~EKORG
        FOR ALL ENTRIES IN @LT_TMP
      WHERE A~BELNR     = @LT_TMP-BELNR
        AND A~GJAHR     = @LT_TMP-GJAHR
        AND A~BUKRS     EQ @P_BUKRS    "회사코드
        INTO CORRESPONDING FIELDS OF TABLE @GT_RSEG.
      FREE LT_TMP.
    ENDIF.
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
FORM PROCESSING_DATA .

  DATA(LT_TMP) = GT_DISP[].

  "선급 반제금액, 지체상금액 계산을 위함.
  SORT LT_TMP BY BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BELNR GJAHR.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BELNR, GJAHR, SRMWWR, ZLSCH, MWSKZ, ZTERM, BVTYP
      FROM RBVS
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BELNR = @LT_TMP-BELNR
       AND GJAHR = @LT_TMP-GJAHR
      INTO TABLE @DATA(LT_RBVS).
    FREE LT_TMP.
    SORT LT_RBVS BY BELNR GJAHR.
  ENDIF.

  "대금지급여부
  DATA: LR_ZTERM_DWPAY LIKE RANGE OF BSAK_REC-ZTERM.
  APPEND VALUE #( SIGN = 'I' OPTION = 'CP' LOW = 'XX*' ) TO LR_ZTERM_DWPAY.

  LT_TMP = GT_DISP[].
  SORT LT_TMP BY BELNR_FI GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BELNR_FI GJAHR.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BELNR, GJAHR, ZTERM, AUGDT, AUGBL
      FROM BSAK_REC
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BELNR = @LT_TMP-BELNR_FI
       AND GJAHR = @LT_TMP-GJAHR
       AND BUKRS = @P_BUKRS
       AND NOT ZTERM IN @LR_ZTERM_DWPAY
      INTO TABLE @DATA(LT_BSAK_REC).
    SORT LT_BSAK_REC BY BELNR GJAHR.

    SELECT BELNR, GJAHR, ZTERM
      FROM BSEG
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BELNR = @LT_TMP-BELNR_FI
       AND GJAHR = @LT_TMP-GJAHR
       AND BUKRS = @P_BUKRS
       AND KOART = 'K'
      INTO TABLE @DATA(LT_BSEG).
    SORT LT_BSEG BY BELNR GJAHR.

"FI 일반전표관리-전자결재정보
*    SELECT BELNR, GJAHR, APVIFKEY, FIID
*      FROM ZTFI10002
*       FOR ALL ENTRIES IN @LT_TMP
*     WHERE BELNR = @LT_TMP-BELNR_FI
*       AND GJAHR = @LT_TMP-GJAHR
*       AND BUKRS = @P_BUKRS
*      INTO TABLE @DATA(LT_ZTFI1002).
*    FREE LT_TMP.
*    SORT LT_ZTFI1002 BY BELNR GJAHR.

    FREE LT_TMP.
  ENDIF.

  "지출결의자 및 EMAIL
  "ZSVMM_USER_INFO 와 같은 사용자 정보 정의 필요

*  DATA: LT_TMP_USER_EXPEN TYPE TABLE OF ZSVMM_USER_INFO.
*
*  LT_TMP_USER_EXPEN = CORRESPONDING #( GT_DISP MAPPING EMPLOY_NO = ZEXPEN_PERSON ).
*  SORT LT_TMP_USER_EXPEN BY EMPLOY_NO.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_USER_EXPEN COMPARING EMPLOY_NO.
*  IF NOT LT_TMP_USER_EXPEN[] IS INITIAL.
*    SELECT EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME, EML
*      FROM ZSVMM_USER_INFO
*       FOR ALL ENTRIES IN @LT_TMP_USER_EXPEN
*     WHERE EMPLOY_NO = @LT_TMP_USER_EXPEN-EMPLOY_NO
*       AND COMPANY = @P_BUKRS
*      INTO TABLE @DATA(LT_USER_EXPEN).
*    SORT LT_USER_EXPEN BY EMPLOY_NO.
*    FREE LT_TMP_USER_EXPEN.
*  ENDIF.

  "FI 전자결재상태
  "FI 전표관리 등의 프로그램 정의 필요

*  DATA(LT_TMP_APP) = LT_ZTFI1002[].
*  SORT LT_TMP_APP BY APVIFKEY FIID.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_APP  COMPARING APVIFKEY FIID.
*  IF NOT LT_TMP_APP[] IS INITIAL.
*    SELECT APVIFKEY, FIID, APPSTATUS, ZPSEQ
*      FROM ZTCN00009
*       FOR ALL ENTRIES IN @LT_TMP_APP
*     WHERE APVIFKEY = @LT_TMP_APP-APVIFKEY
*       AND FIID = @LT_TMP_APP-FIID
*      INTO TABLE @DATA(LT_ZTCN00009).
*    FREE LT_TMP_APP.
*    SORT LT_ZTCN00009 BY APVIFKEY FIID ZPSEQ DESCENDING.
*  ENDIF.

  PERFORM PROCESSING_DATA_ETC_GET.

  LOOP AT GT_DISP INTO GS_DISP.
    DATA(LV_TABIX) = SY-TABIX.

    "FI 결재상태
*    READ TABLE LT_ZTFI1002 INTO DATA(LS_ZTFI1002)
*                           WITH KEY BELNR = GS_DISP-BELNR_FI
*                                    GJAHR = GS_DISP-GJAHR
*                           BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      GS_DISP-APVIFKEY = LS_ZTFI1002-APVIFKEY.
*      GS_DISP-FIID = LS_ZTFI1002-FIID.
*
*      READ TABLE LT_ZTCN00009 INTO DATA(LS_ZTCN00009)
*                             WITH KEY APVIFKEY = GS_DISP-APVIFKEY
*                                      FIID = GS_DISP-FIID
*                             BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        GS_DISP-APPSTATUS = LS_ZTCN00009-APPSTATUS.
*      ENDIF.
*    ENDIF.

*    IF NOT S_WFSTS[] IS INITIAL AND
*       NOT GS_DISP-APPSTATUS IN S_WFSTS.
*      DELETE GT_DISP.
*      CONTINUE.
*    ENDIF.

    READ TABLE LT_RBVS WITH KEY BELNR = GS_DISP-BELNR
                                GJAHR = GS_DISP-GJAHR
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    LOOP AT LT_RBVS INTO DATA(LS_RBVS) FROM SY-TABIX.
      IF LS_RBVS-BELNR NE GS_DISP-BELNR OR
         LS_RBVS-GJAHR NE GS_DISP-GJAHR.
        EXIT.
      ENDIF.

      "선급 반제금액
      IF LS_RBVS-ZTERM = GC_ZTERM_XX01.
        GS_DISP-DWPAYC = GS_DISP-DWPAYC + LS_RBVS-SRMWWR.
      ENDIF.

      "지체상금 공제금액
      IF LS_RBVS-ZTERM = GC_ZTERM_XX03.
        GS_DISP-DELAYF = GS_DISP-DELAYF + LS_RBVS-SRMWWR.
      ENDIF.
    ENDLOOP.

    "송장처리자
    READ TABLE GT_SAP_USER INTO DATA(LS_SAP_USER)
                          WITH KEY BNAME = GS_DISP-ERFNAM
                          BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISP-ERFNAM_TX = LS_SAP_USER-NAME_TEXT.
    ENDIF.

    "지출결의자 정보
*    READ TABLE LT_USER_EXPEN INTO DATA(LS_USER_EXPEN)
*                          WITH KEY EMPLOY_NO = GS_DISP-ZEXPEN_PERSON
*                          BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      GS_DISP-ZEXPEN_PERSON_NAME = LS_USER_EXPEN-EMPLOY_NAME.
*      GS_DISP-ZEXPEN_DEPARTMENT = LS_USER_EXPEN-DEPARTMENT.
*      GS_DISP-ZEXPEN_DEPART_NAME = LS_USER_EXPEN-DEPART_NAME.
*      GS_DISP-ZEXPEN_EML = LS_USER_EXPEN-EML.
*    ENDIF.

    "지급여부
    READ TABLE LT_BSAK_REC INTO DATA(LS_BSAK_REC)
                           WITH KEY BELNR = GS_DISP-BELNR_FI
                                    GJAHR = GS_DISP-GJAHR
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GS_DISP-PAYSTS = ICON_LED_GREEN.
      GS_DISP-AUGDT = LS_BSAK_REC-AUGDT.
      GS_DISP-AUGBL = LS_BSAK_REC-AUGBL.
    ENDIF.

    "지급조건
    READ TABLE LT_BSEG WITH KEY BELNR = GS_DISP-BELNR_FI
                                GJAHR = GS_DISP-GJAHR
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    LOOP AT LT_BSEG INTO DATA(LS_BSEG) FROM SY-TABIX.
      IF LS_BSEG-BELNR NE GS_DISP-BELNR_FI OR
         LS_BSEG-GJAHR NE GS_DISP-GJAHR.
        EXIT.
      ENDIF.

      IF GS_DISP-ZTERM IS INITIAL.
        GS_DISP-ZTERM = LS_BSEG-ZTERM.
      ELSE.
        IF GS_DISP-ZTERM(2) EQ 'XX'.
          GS_DISP-ZTERM = LS_BSEG-ZTERM.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "BY CODE INSPECTION
    PERFORM PROCESSING_DATA_ETC_SET.

    MODIFY GT_DISP FROM GS_DISP INDEX LV_TABIX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_ETC_GET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_ETC_GET .

  CLEAR GT_SAP_USER.
  "송장처리자 명
  DATA(LT_TMP) = GT_DISP[].
  SORT LT_TMP BY ERFNAM.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING ERFNAM.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BNAME, NAME_TEXT
      FROM V_USERNAME
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BNAME = @LT_TMP-ERFNAM
      INTO CORRESPONDING FIELDS OF TABLE @GT_SAP_USER.
    FREE LT_TMP.
    SORT GT_SAP_USER BY BNAME.
  ENDIF.

  "전자세금계산서 상태내역
*  CLEAR GT_ED_STATUS.
*  SELECT STATUS, STATUS_TEXT
*    FROM ZDTV3T_STATUS
*   WHERE STATUS NE @SPACE
*    INTO CORRESPONDING FIELDS OF TABLE @GT_ED_STATUS.
*  SORT GT_ED_STATUS BY STATUS.

  "사업장명
  CLEAR GT_BUPLA.
  SELECT BRANCH AS BUPLA,
         NAME
    FROM J_1BBRANCH
   WHERE BUKRS EQ @P_BUKRS
    INTO CORRESPONDING FIELDS OF TABLE @GT_BUPLA.
  SORT GT_BUPLA BY BUPLA.

  "지급조건명
  CLEAR GT_ZTERM.
  SELECT ZTERM, TEXT1
    FROM ZSVCMM_PAYTERM
   WHERE ZTERM NE @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_ZTERM.
  SORT GT_ZTERM BY ZTERM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_ETC_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISP
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_ETC_SET.

*  IF GS_DISP-APVIFKEY IS INITIAL.
*    GS_DISP-APPSTATUS_TX = '상신대기'(T17).
*  ELSE.
*    PERFORM GET_DOMAIN_TEXT USING 'ZAPVSTATUS'
*                                  GS_DISP-APPSTATUS
*                            CHANGING GS_DISP-APPSTATUS_TX.
*  ENDIF.

  "세금계산서수정사유 존재 시 무조건 정발행
*  IF NOT GS_DISP-AMEND_CODE IS INITIAL.
*    CLEAR: GS_DISP-INV_SIGN.
*  ENDIF.

*  IF GS_DISP-INV_SIGN = 'X'.
*    GS_DISP-INV_SIGN_TX = '역발행'(T12). "역발행
*  ELSE.
*    GS_DISP-INV_SIGN_TX = '정발행'(T11). "정발행
*  ENDIF.

  "실지급액 : 계약금액-선급금반제금액-지체상금공제금액
  GS_DISP-ESTPAY = GS_DISP-RMWWR - ( GS_DISP-DWPAYC + GS_DISP-DELAYF ).

  "세금계산서 종류
*  PERFORM GET_DOMAIN_TEXT USING 'ZDTV3D_TYPE_CODE'
*                                GS_DISP-TYPE_CODE
*                          CHANGING GS_DISP-TYPE_CODE_TX.

  "세금계산서 상태내역
*  READ TABLE GT_ED_STATUS INTO GS_ED_STATUS
*                          WITH KEY STATUS = GS_DISP-ED_STATUS
*                          BINARY SEARCH.
*  IF SY-SUBRC EQ 0.
*    GS_DISP-ED_STATUS_TX = GS_ED_STATUS-STATUS_TEXT.
*  ENDIF.


  "사업장명
  READ TABLE GT_BUPLA INTO DATA(LS_BUPLA)
                      WITH KEY BUPLA = GS_DISP-BUPLA
                      BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISP-BUPLA_TX = LS_BUPLA-NAME.
  ENDIF.

  "지급조건명
  READ TABLE GT_ZTERM INTO DATA(LS_ZTERM)
                      WITH KEY ZTERM = GS_DISP-ZTERM
                      BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    GS_DISP-ZTERM_TX = LS_ZTERM-TEXT1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DOMAIN_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_DOMAIN_TEXT USING IV_DOMNAME TYPE ANY
                              IV_DOMVALUE TYPE ANY
                     CHANGING CV_DDTEXT   TYPE ANY.

  DATA: LV_DDTEXT TYPE DD07V-DDTEXT.

  CLEAR CV_DDTEXT.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      I_DOMNAME  = CONV DD07V-DOMNAME( IV_DOMNAME )
      I_DOMVALUE = CONV DD07V-DOMVALUE_L( IV_DOMVALUE )
    IMPORTING
      E_DDTEXT   = LV_DDTEXT
    EXCEPTIONS
      NOT_EXIST  = 1.

  CV_DDTEXT = LV_DDTEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_GR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_GR_DATA .

  DATA: LT_TMP_DETAIL TYPE TABLE OF TS_DISP_DTL.

  MOVE-CORRESPONDING GT_DISP[] TO LT_TMP_DETAIL[].

  PERFORM SELECT_EKBE TABLES LT_TMP_DETAIL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_EKBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TMP_DETAIL
*&---------------------------------------------------------------------*
FORM SELECT_EKBE TABLES IT_TMP_DETAIL STRUCTURE GS_DISP_DTL.

  DATA: BEGIN OF LS_TAX_SUM,
          BELNR TYPE EKBE-BELNR,
          GJAHR TYPE EKBE-GJAHR,
          WMWST TYPE SUPPLINV-WMWST1,
        END OF LS_TAX_SUM,
        LT_TAX_SUM LIKE TABLE OF LS_TAX_SUM,
        LV_DEF_TAX TYPE SUPPLINV-WMWST1.

  DATA(LT_TMP) = IT_TMP_DETAIL[].
  SORT LT_TMP BY BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BELNR GJAHR.

  IF NOT LT_TMP[] IS INITIAL.
    SELECT A~BELNR, "송장전표
           A~GJAHR, "송장년도
           A~BUZEI, "송장 ITEM NO
           A~VGABE, "트랜잭션 유형
           A~BUDAT, "입고일
           A~LFGJA, "자재문서년도
           A~LFBNR, "자재문서
           A~LFPOS, "자재문서품목
           A~MATNR, "자재
           A~MENGE, "입고수량
           B~MEINS, "단위
           A~WRBTR, "입고금액
           A~WAERS, "통화
*           F~MWSKZ,  "세금코드
*           A~WMWST1 AS WMWST,  "세액
           A~EBELN, "PO 번호
           A~EBELP, "PO 품목
           B~NETPR AS NETPR_PO, "PO 단가
           B~PEINH, "PO 가격단위
           B~TXZ01 AS MAKTX,  "자재내역
           B~MENGE AS MENGE_PO,
           C~BSTYP,  "PO 범주
           A~REEWR   "불공제세액의 금액
           FROM EKBE AS A INNER JOIN EKPO AS B
             ON A~EBELN = B~EBELN AND
                A~EBELP = B~EBELP
                          INNER JOIN EKKO AS C
             ON B~EBELN = C~EBELN
                          INNER JOIN LFM1 AS D
             ON C~LIFNR = D~LIFNR AND
                C~EKORG = D~EKORG
                          INNER JOIN LFA1 AS E
             ON C~LIFNR = E~LIFNR
           FOR ALL ENTRIES IN @LT_TMP
          WHERE A~BELNR   EQ @LT_TMP-BELNR
            AND A~GJAHR   EQ @LT_TMP-GJAHR
*            AND A~VGABE   EQ '2'
      INTO CORRESPONDING FIELDS OF TABLE @GT_DISP_DTL_ALL.
    FREE LT_TMP.
  ENDIF.

  LT_TMP = GT_DISP_DTL_ALL[].
  SORT LT_TMP BY BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BELNR GJAHR.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT BELNR, GJAHR, BUZEI, MWSKZ, WRBTR
      FROM RSEG
       FOR ALL ENTRIES IN @LT_TMP
     WHERE BELNR = @LT_TMP-BELNR
       AND GJAHR = @LT_TMP-GJAHR
      INTO TABLE @DATA(LT_RSEG).
    FREE LT_TMP.
    SORT LT_RSEG BY BELNR GJAHR.
  ENDIF.

*> 실비정보
*  LT_TMP = GT_DISP_DTL_ALL[].
*  SORT LT_TMP BY LFBNR LFGJA LFPOS.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING LFBNR LFGJA LFPOS.
*  IF NOT LT_TMP[] IS INITIAL.
*    SELECT MBLNR, MJAHR, ZEILE, MIDAM, ACTEX
*      FROM ZSVCMM_REALCOST
*       FOR ALL ENTRIES IN @LT_TMP
*     WHERE MBLNR = @LT_TMP-LFBNR
*       AND MJAHR = @LT_TMP-LFGJA
*       AND ZEILE = @LT_TMP-LFPOS
*      INTO TABLE @DATA(LT_ZSVCMM_REALCOST).
*    FREE LT_TMP.
*    SORT LT_ZSVCMM_REALCOST BY MBLNR MJAHR ZEILE.
*  ENDIF.

*> 불공제세액 세금코드
*  CONSTANTS: LC_KSCHL_KRNT TYPE A003-KSCHL VALUE 'KRNT',
*             LC_KR(2)      TYPE C VALUE 'KR'.
*  SELECT MWSKZ
*    FROM A003
*   WHERE ALAND = @LC_KR
*     AND KSCHL = @LC_KSCHL_KRNT
*    INTO TABLE @DATA(LT_MWSKZ_P).

  SORT: LT_RSEG BY BELNR GJAHR BUZEI,
        GT_DISP BY BELNR GJAHR.
*        GT_ZPRI_PRICE BY EBELN EBELP.
*        LT_MWSKZ_P BY MWSKZ.

  LOOP AT GT_DISP_DTL_ALL INTO DATA(LS_DISP_DTL).
    GV_TABIX = SY-TABIX.

    READ TABLE LT_RSEG INTO DATA(LS_RSEG)
                       WITH KEY BELNR = LS_DISP_DTL-BELNR
                                GJAHR = LS_DISP_DTL-GJAHR
                                BUZEI = LS_DISP_DTL-BUZEI
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LS_DISP_DTL-MWSKZ = LS_RSEG-MWSKZ.

      "불공제세액이면
*      READ TABLE LT_MWSKZ_P WITH KEY MWSKZ = LS_DISP_DTL-MWSKZ
*                            BINARY SEARCH
*                            TRANSPORTING NO FIELDS.
*      IF SY-SUBRC EQ 0.
*        LS_DISP_DTL-WRBTR = LS_DISP_DTL-REEWR.
*      ENDIF.
    ENDIF.

    "품목 단가 : 금액 / 수량으로 표시
    TRY.
        LS_DISP_DTL-NETPR = LS_DISP_DTL-WRBTR / LS_DISP_DTL-MENGE.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    "세액
    PERFORM GET_TAX_AMOUNT_BY_ITEM USING LS_DISP_DTL-WRBTR
                                            LS_DISP_DTL-MWSKZ
                                            LS_DISP_DTL-WAERS
                                   CHANGING LS_DISP_DTL-WMWST.

    "실비/검수금액
*    READ TABLE LT_ZSVCMM_REALCOST INTO DATA(LS_ZSVCMM_REALCOST)
*                       WITH KEY MBLNR = LS_DISP_DTL-LFBNR
*                                MJAHR = LS_DISP_DTL-LFGJA
*                                ZEILE = LS_DISP_DTL-LFPOS
*                       BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      LS_DISP_DTL-MIDAM = LS_ZSVCMM_REALCOST-MIDAM.
*      LS_DISP_DTL-ACTEX = LS_ZSVCMM_REALCOST-ACTEX.
*    ENDIF.

*> 인쇄교체비
*    READ TABLE GT_ZPRI_PRICE INTO DATA(LS_ZPRI_PRICE)
*                             WITH KEY EBELN = LS_DISP_DTL-EBELN
*                                      EBELP = LS_DISP_DTL-EBELP
*                             BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      "인쇄교체비 단가(인쇄교체비 / PO 수량)
*      TRY.
*          LS_DISP_DTL-PRTNET = LS_ZPRI_PRICE-ZPRI / LS_DISP_DTL-MENGE_PO.
*        CATCH CX_SY_ZERODIVIDE.
*      ENDTRY.
*
*
*      "인쇄교체비 정가(인쇄교체비 단가 * 입고수량)
*      LS_DISP_DTL-PRTAMT = LS_DISP_DTL-PRTNET * LS_DISP_DTL-MENGE.
*    ENDIF.

*> MAIN ALV 에 UPDATE
    READ TABLE GT_DISP INTO DATA(LS_DISP)
                       WITH KEY BELNR = LS_DISP_DTL-BELNR
                                GJAHR = LS_DISP_DTL-GJAHR
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      DATA(LV_TABIX) = SY-TABIX.

*      "실비
*      LS_DISP-ACTPAY = LS_DISP-ACTPAY + LS_DISP_DTL-ACTEX.
*
*      "인쇄교체비
*      LS_DISP-PRTCHG = LS_DISP-PRTCHG + LS_DISP_DTL-PRTAMT.

      IF LS_DISP-VGABE IS INITIAL.
        LS_DISP-VGABE = LS_DISP_DTL-VGABE.
      ENDIF.

      PERFORM SET_FIDOC_TYPE_DESC CHANGING LS_DISP.

      MODIFY GT_DISP FROM LS_DISP INDEX LV_TABIX.
    ENDIF.

    PERFORM SET_MINUS_DATA_ITEM USING LS_DISP
                                CHANGING LS_DISP_DTL.

    MODIFY GT_DISP_DTL_ALL FROM LS_DISP_DTL INDEX GV_TABIX.

*> 전표별 세액 SUM
    CLEAR LS_TAX_SUM.
    MOVE-CORRESPONDING LS_DISP_DTL TO LS_TAX_SUM.
    COLLECT LS_TAX_SUM INTO LT_TAX_SUM.
  ENDLOOP.

  SORT: GT_DISP_DTL_ALL BY BELNR GJAHR WRBTR DESCENDING,
        LT_TAX_SUM BY BELNR GJAHR.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).
    "금액 등 - 표시
    IF <LS_DISP>-FIDOC_TYPE = '대변메모'(T15) OR
       <LS_DISP>-FIDOC_TYPE = '차후대변'(T16).
      PERFORM SET_MINUS_AMOUNT CHANGING <LS_DISP>.
    ENDIF.

    "단수차이 보정
    CLEAR LV_DEF_TAX.
    READ TABLE LT_TAX_SUM INTO LS_TAX_SUM
                          WITH KEY BELNR = <LS_DISP>-BELNR
                                   GJAHR = <LS_DISP>-GJAHR
                          BINARY SEARCH.
    IF SY-SUBRC EQ 0 AND LS_TAX_SUM-WMWST NE <LS_DISP>-WMWST1.
      LV_DEF_TAX = <LS_DISP>-WMWST1 - LS_TAX_SUM-WMWST.
      READ TABLE GT_DISP_DTL_ALL ASSIGNING FIELD-SYMBOL(<LS_DISP_DTL_ALL>)
                                  WITH KEY BELNR = <LS_DISP>-BELNR
                                           GJAHR = <LS_DISP>-GJAHR
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <LS_DISP_DTL_ALL>-WMWST = <LS_DISP_DTL_ALL>-WMWST + LV_DEF_TAX.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT GT_DISP_DTL_ALL BY BELNR GJAHR BUZEI.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TAX_AMOUNT_BY_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_TAX_AMOUNT_BY_ITEM USING IV_AMOUNT
                                      IV_MWSKZ
                                      IV_WAERS
                             CHANGING CV_TAX_AMOUNT.

  DATA: LV_DMBTR TYPE BSEG-WRBTR,
        LV_MWSKZ TYPE BSEG-MWSKZ,
        LV_WAERS TYPE BKPF-WAERS,
        LV_FWSTE TYPE BSET-FWSTE,
        LT_TAX   TYPE TABLE OF RTAX1U15.

  LV_DMBTR = IV_AMOUNT.
  LV_MWSKZ = IV_MWSKZ.
  LV_WAERS = IV_WAERS.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      I_BUKRS = P_BUKRS
      I_MWSKZ = LV_MWSKZ
      I_WAERS = LV_WAERS
      I_WRBTR = LV_DMBTR
    IMPORTING
      E_FWSTE = LV_FWSTE
    TABLES
      T_MWDAT = LT_TAX
    EXCEPTIONS
      OTHERS  = 13.

  IF SY-SUBRC EQ 0.
    CV_TAX_AMOUNT  = LV_FWSTE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIDOC_TYPE_DESC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_FIDOC_TYPE_DESC CHANGING CS_DISP TYPE TS_DISP.

  "전표구분
  IF CS_DISP-FIDOC_TYPE IS INITIAL.
    CASE CS_DISP-VGABE.
      WHEN '2'.
        IF CS_DISP-SHKZG = 'S'.
          CS_DISP-FIDOC_TYPE = '송장'(T13).
        ELSE.
          CS_DISP-FIDOC_TYPE = '대변메모'(T15).
        ENDIF.
      WHEN '3'.
        IF CS_DISP-SHKZG = 'S'.
          CS_DISP-FIDOC_TYPE = '차후차변'(T14).
        ELSE.
          CS_DISP-FIDOC_TYPE = '차후대변'(T16).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_MINUS_AMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_DISP
*&---------------------------------------------------------------------*
FORM SET_MINUS_AMOUNT CHANGING CS_DISP TYPE TS_DISP.

  CS_DISP-DMBTR = CS_DISP-DMBTR * -1.
  CS_DISP-WMWST1 = CS_DISP-WMWST1 * -1.
  CS_DISP-RMWWR = CS_DISP-RMWWR * -1.
  CS_DISP-DWPAYC = CS_DISP-DWPAYC * -1.
  CS_DISP-DELAYF = CS_DISP-DELAYF * -1.
  CS_DISP-ESTPAY = CS_DISP-ESTPAY * -1.
  CS_DISP-PRTCHG = CS_DISP-PRTCHG * -1.
  CS_DISP-ACTPAY = CS_DISP-ACTPAY * -1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_MINUS_DATA_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      <-- LS_DISP_DTL
*&---------------------------------------------------------------------*
FORM SET_MINUS_DATA_ITEM USING IS_DISP TYPE TS_DISP
                          CHANGING CS_DISP_DTL TYPE TS_DISP_DTL.

  CASE IS_DISP-FIDOC_TYPE.
    WHEN '대변메모'(T15).
      CS_DISP_DTL-MENGE = CS_DISP_DTL-MENGE * -1.
      CS_DISP_DTL-WRBTR = CS_DISP_DTL-WRBTR * -1.
      CS_DISP_DTL-NETPR = CS_DISP_DTL-NETPR * -1.
      CS_DISP_DTL-WMWST = CS_DISP_DTL-WMWST * -1.
      CS_DISP_DTL-NETPR_PO = CS_DISP_DTL-NETPR_PO * -1.
*      CS_DISP_DTL-MIDAM = CS_DISP_DTL-MIDAM * -1.
*      CS_DISP_DTL-ACTEX = CS_DISP_DTL-ACTEX * -1.

    WHEN '차후차변'(T14).
      CLEAR CS_DISP_DTL-MENGE.

    WHEN '차후대변'(T16).
      CLEAR CS_DISP_DTL-MENGE.

      CS_DISP_DTL-WRBTR = CS_DISP_DTL-WRBTR * -1.
      CS_DISP_DTL-NETPR = CS_DISP_DTL-NETPR * -1.
      CS_DISP_DTL-WMWST = CS_DISP_DTL-WMWST * -1.
      CS_DISP_DTL-NETPR_PO = CS_DISP_DTL-NETPR_PO * -1.
*      CS_DISP_DTL-MIDAM = CS_DISP_DTL-MIDAM * -1.
*      CS_DISP_DTL-ACTEX = CS_DISP_DTL-ACTEX * -1.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .

  DATA:LV_ANS.

**--------------------------------
** 화면 OFF전 변경 데이타 확인
**--------------------------------
*  PERFORM CHECK_CHANGED_DATA USING    'E'
*                             CHANGING LV_ANS.

  CHECK LV_ANS IS INITIAL.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_change_mode
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_MODE
*&---------------------------------------------------------------------*
FORM SET_CHANGE_MODE CHANGING CV_MODE.

  DATA:LV_ANS,
       LV_LOCK.

  IF CV_MODE = 'X'.  "Edit 모드일경우 ..
*--------------------------------
* 모드 변경전 변경 데이타 확인
*--------------------------------
    PERFORM CHECK_CHANGED_DATA USING 'C' CHANGING LV_ANS.

    CHECK LV_ANS IS INITIAL.
  ENDIF.

*-------------------------
*-- 최신 Data
*-------------------------
  PERFORM REFRESH_DATA.


*----------------------------------------------------------------
* Mode 변경
* Grid가 2개이상일 경우 반드시 Header 에서 Lock 정보를 받아온다
*----------------------------------------------------------------
  CALL METHOD GRF_GRID->SET_CHANGE_MODE(
    CHANGING
      CV_MODE = CV_MODE
      CV_LOCK = LV_LOCK ).


  CALL METHOD GRF_GRID_DTL->SET_CHANGE_MODE(
    CHANGING
      CV_MODE = CV_MODE
      CV_LOCK = LV_LOCK ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_changed_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ANS
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_DATA USING IV_TYPE
                         CHANGING CV_ANS.

*  CHECK GRF_GRID->GET_STATUS( )     EQ ABAP_TRUE OR
*        GRF_GRID_DTL->GET_STATUS( ) EQ ABAP_TRUE .

  CASE IV_TYPE.
    WHEN 'E'.  "Exit
      DATA(LV_MSG) = ZCL_CN_ALV_GRID=>AC_MSG_EXIT2.  "'화면에서 OFF 하시겠습니까?'
    WHEN 'C'.  "화면 Change
      LV_MSG = ZCL_CN_ALV_GRID=>AC_MSG_CONT.        "계속 하시겠습니까?
  ENDCASE.

  CV_ANS = 'X'.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE2   "'Exit Confirm'
                              IV_TEXT1 = ZCL_CN_ALV_GRID=>AC_MSG_EXIT     "'변경된 데이타가 있습니다'
                              IV_TEXT2 = LV_MSG )                         "'화면에서 Off 하시겠습니까?' )
                              EQ ABAP_TRUE. " YES

  CV_ANS = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form refresh_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA .

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

*------------------------------
* Set Variant
*------------------------------
  GS_VARIANT-REPORT   = SY-CPROG.
  GS_VARIANT-USERNAME = SY-UNAME.

*>예외처리 유저 점검.
  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING  IS_COMMON =  VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
                                     IT_WHERE = VALUE #(
                                                         ( FIELD = 1 VALUE = 'ZRMM5010' )
                                                         ( FIELD = 2 VALUE = 'EX01' )
                                                         ( FIELD = 3 VALUE = SY-UNAME )
                                                       )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EXC_USER = 'X'.
  ENDIF.

  IF GV_EXC_USER IS INITIAL.
*> 회사 / 접수자 / 발주부서 기본값 설정.
*    SELECT SINGLE EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME, COMPANY, COMPANY_NAME
*      FROM ZSVMM_USER_INFO
*     WHERE USER_ID = @SY-UNAME
*      INTO @DATA(LS_USER_INIT).
*    IF SY-SUBRC EQ 0.
*      P_BUKRS = LS_USER_INIT-COMPANY.
*    ENDIF.
  ENDIF.

*> 요청일 기본값 설정. (현재일 10일 이하이면 전월설정, 10일 이후면 당월 설정)
  DATA: LV_START_DATE TYPE SY-DATUM,
        LV_END_DATE   TYPE SY-DATUM.

  IF SY-DATUM+6(2) < 10.
    "전월 계산.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        DATE      = SY-DATUM
        SIGNUM    = '-'
        DAYS      = 0
        MONTHS    = 1
        YEARS     = 0
      IMPORTING
        CALC_DATE = LV_START_DATE.

    "전월 말일 계산
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = LV_START_DATE
      IMPORTING
        LAST_DAY_OF_MONTH = LV_END_DATE.

    APPEND VALUE #( SIGN = 'I' OPTION = 'BT' LOW = LV_START_DATE(6) && '01' HIGH = LV_END_DATE )
        TO S_BUDAT.

  ELSE.
    APPEND VALUE #( SIGN = 'I' OPTION = 'BT' LOW = SY-DATUM(6) && '01' HIGH = SY-DATUM )
        TO S_BUDAT.
  ENDIF.

*> 송장처리자 기본값 설정.
  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = SY-UNAME ) TO S_USNAM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_BUPLA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_BUPLA USING IV_SCR_NAME..


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '사업장',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'BUPLA'.

  PERFORM GET_DYNP_VALUE USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT BRANCH AS BUPLA,
         NAME
    FROM J_1BBRANCH
   WHERE BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_BUPLA).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_BUPLA
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_EMPLOY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_EMPLOY USING IV_SCR_NAME.


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL,
        LT_UPDATE TYPE TABLE OF DYNPREAD,
        LS_UPDATE TYPE DYNPREAD.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '유저정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EMPLOY_NO'.

  PERFORM GET_DYNP_VALUE USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE COMPANY EQ @P_BUKRS
*    INTO TABLE @DATA(LT_USER_INFO).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
*      VALUE_TAB         = LT_USER_INFO
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

*  SORT LT_USER_INFO BY EMPLOY_NO.

  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
*    READ TABLE LT_USER_INFO INTO DATA(LS_USER_INFO)
*                        WITH KEY EMPLOY_NO = LS_RETURN-FIELDVAL
*                        BINARY SEARCH.
*
*    "사번
*    CLEAR LS_UPDATE.
*    LS_UPDATE-FIELDNAME = IV_SCR_NAME.
*    LS_UPDATE-FIELDVALUE  = LS_USER_INFO-EMPLOY_NO.
*    APPEND LS_UPDATE TO LT_UPDATE.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME     = SY-CPROG
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = LT_UPDATE. "
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_DEPART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_DEPART USING IV_SCR_NAME
                                  IV_SCR_DESC.


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL,
        LT_UPDATE TYPE TABLE OF DYNPREAD,
        LS_UPDATE TYPE DYNPREAD.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '부서정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'DEPARTMENT'.

  PERFORM GET_DYNP_VALUE USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  SELECT DISTINCT
*         DEPARTMENT, DEPART_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE COMPANY EQ @P_BUKRS
*    INTO TABLE @DATA(LT_DEPART_INFO).
*
*  SORT LT_DEPART_INFO BY DEPARTMENT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
*      VALUE_TAB         = LT_DEPART_INFO
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
*    READ TABLE LT_DEPART_INFO INTO DATA(LS_DEPART_INFO)
*                        WITH KEY DEPARTMENT = LS_RETURN-FIELDVAL
*                        BINARY SEARCH.

*    "부서
*    CLEAR LS_UPDATE.
*    LS_UPDATE-FIELDNAME = IV_SCR_NAME.
*    LS_UPDATE-FIELDVALUE  = LS_DEPART_INFO-DEPARTMENT.
*    APPEND LS_UPDATE TO LT_UPDATE.
*
*    "부서명
*    IF NOT IV_SCR_DESC IS INITIAL.
*      CLEAR LS_UPDATE.
*      LS_UPDATE-FIELDNAME = IV_SCR_DESC.
*      LS_UPDATE-FIELDVALUE  = LS_DEPART_INFO-DEPART_NAME.
*      APPEND LS_UPDATE TO LT_UPDATE.
*    ENDIF.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME     = SY-CPROG
        DYNUMB     = SY-DYNNR
      TABLES
        DYNPFIELDS = LT_UPDATE. "
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_dynp_value
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_DYNP_VALUE USING IV_SCR_NAME
                     CHANGING EV_VALUE.

  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD,
        LS_DYNPFIELDS TYPE DYNPREAD.

  LS_DYNPFIELDS-FIELDNAME = IV_SCR_NAME.
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
    READ TABLE LT_DYNPFIELDS INTO LS_DYNPFIELDS
                             WITH KEY FIELDNAME = IV_SCR_NAME
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      EV_VALUE = LS_DYNPFIELDS-FIELDVALUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_f4_sel_scr_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_EKGRP USING IV_SCR_NAME
                                  IV_SCR_DESC.


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '구매그룹',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.


  PERFORM GET_DYNP_VALUE USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH '회사코드'(f01) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT DISTINCT
         EKGRP, EKNAM, BUKRS
    FROM ZSVCMM_ORG
   WHERE BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_ZSVCMM_ORG).

  SORT LT_ZSVCMM_ORG BY EKGRP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_ZSVCMM_ORG
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_WFSTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_WFSTS USING IV_SCR_NAME.


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL.

  DATA: BEGIN OF LS_F4_LIST,
          APPSTATUS TYPE char10,
*          APPSTATUS TYPE ZTCN00009-APPSTATUS,
          TEXT      TYPE DD07T-DDTEXT,
        END OF LS_F4_LIST,
        LT_F4_LIST LIKE TABLE OF LS_F4_LIST.

  CONSTANTS: LC_TITLE(15) TYPE C VALUE '결재상태',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'APPSTATUS',
             LC_DOMNAME   TYPE DD07T-DOMNAME VALUE 'ZAPVSTATUS'.


  SELECT DOMVALUE_L AS APPSTATUS,
         DDTEXT AS TEXT
    FROM DD07T
   WHERE DOMNAME    EQ @LC_DOMNAME
     AND DDLANGUAGE EQ @SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE @LT_F4_LIST.

  APPEND VALUE #( APPSTATUS = SPACE TEXT = '상신대기'(T17) ) TO LT_F4_LIST.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_F4_LIST
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form variant_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIANT
*&      <-- P_VAR
*&---------------------------------------------------------------------*
FORM VARIANT_F4 USING IS_VARIANT TYPE ANY
                 CHANGING  CV_VARIANT_SCREEN TYPE ANY.

  DATA: LS_VARIANT TYPE DISVARIANT,
        LV_EXIT(1) TYPE C.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = IS_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = LV_EXIT
      ES_VARIANT    = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE I031(ZPCA01).  "'No found layout available for F4.'
  ENDIF.

  IF LV_EXIT IS INITIAL.
    CV_VARIANT_SCREEN = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_f4_listbox
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_F4_LISTBOX .

*>회사코드 List box
  PERFORM SET_F4_BUKRS_LISTBOX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_f4_bukrs_listbox
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_F4_BUKRS_LISTBOX.

  DATA: LV_NAME TYPE  VRM_ID,
        LT_LIST TYPE  VRM_VALUES.

  CONSTANTS: LC_SCR_NAME TYPE SCREEN-NAME VALUE 'P_BUKRS'.

  LV_NAME =  LC_SCR_NAME.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING  IS_COMMON =  VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                     IT_WHERE = VALUE #(
                                                         ( FIELD = 1 VALUE = 'BUKRS' )
                                                       )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  SELECT A~FIELD2 AS KEY, A~FIELD3 AS TEXT
    FROM @LT_CONFIG AS A
   WHERE A~FIELD2 NE @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @LT_LIST.

  SORT  LT_LIST  BY  KEY.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = LV_NAME
      VALUES = LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCR_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_SCR_OUTPUT .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'MST'.
      IF GV_EXC_USER IS INITIAL.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_POSTING_CANCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_POSTING_CANCEL .


  DATA: LV_TOTAL TYPE I,
        LV_SUCCE TYPE I,
        LV_ERROR TYPE I.

  DATA: BEGIN OF LS_RBKP,
          BELNR    LIKE RBKP-BELNR,
          GJAHR    LIKE RBKP-GJAHR,
          BELNR_CC LIKE RBKP-BELNR,
          GJAHR_CC LIKE RBKP-GJAHR,
          RETURN   TYPE BAPIRET2_T,
        END OF LS_RBKP,
        LT_RBKP LIKE TABLE OF LS_RBKP.
  DATA: LT_RETURN        TYPE TABLE OF BAPIRET2,
        LS_RETURN_COMMIT TYPE BAPIRET2.

*  DATA: LR_ED_STATUS TYPE RANGE OF ZDTV3T_AP_HEAD-STATUS. "세금계산서상태

  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = DATA(LT_SELIDX).

  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.

  IF LT_SELIDX[] IS INITIAL.
    MESSAGE S006 DISPLAY LIKE 'I'. EXIT.
  ENDIF.

*2210 [매입] 역발행요청중
*2211 [매입] 역발행요청완료
*2213 [매입] 역발행요청실패
*2215 [매입] 역발행요청승인
*2216 [매입] 역발행요청반려
*2220 [매입] 역발행요청취소중
*2221 [매입] 역발행요청취소 -
*2223 [매입] 역발행요청취소실패

*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2210' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2223' ) TO LR_ED_STATUS.


*> 검색대상 자재 추출
  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      "역발행 시 진행 상태 확인
*      IF LS_DISP-INV_SIGN = 'X' AND ( LS_DISP-ED_STATUS IN LR_ED_STATUS ).
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S035 WITH '전자세금계산서 상태'(M06) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      IF NOT LS_DISP-ISSUE_ID IS INITIAL.
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S023 WITH '전자세금계산서 맵핑완료'(M01) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.

      IF NOT LS_DISP-BELNR_CC IS INITIAL.
        _G_ADD_1 LV_ERROR.
        MESSAGE S043 WITH '이미 송장취소 전표'(M02) LS_DISP-BELNR_CC DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      IF NOT LS_DISP-PAYSTS IS INITIAL.
        _G_ADD_1 LV_ERROR.
        MESSAGE S023 WITH '대금지급 완료된 건'(M05) DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING LS_DISP TO LS_RBKP.
      APPEND LS_RBKP TO LT_RBKP.
      CLEAR LS_RBKP.
    ENDIF.
  ENDLOOP.

  CHECK LV_ERROR IS INITIAL.

  CALL SCREEN 0300 STARTING AT 05 05.

  CHECK GV_CONTINUE_300 = 'X'.

  LOOP AT LT_RBKP INTO LS_RBKP.

    GV_TABIX = SY-TABIX.

    _G_ADD_1 LV_TOTAL.

    CLEAR LT_RETURN.

    SET UPDATE TASK LOCAL.

    "*-1) Message Init
    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXCEPTIONS
        LOG_NOT_ACTIVE       = 1
        WRONG_IDENTIFICATION = 2
        OTHERS               = 3.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        INVOICEDOCNUMBER          = LS_RBKP-BELNR
        FISCALYEAR                = LS_RBKP-GJAHR
        REASONREVERSAL            = GS_SCR_300-STGRD
        POSTINGDATE               = GS_SCR_300-BUDAT
      IMPORTING
        INVOICEDOCNUMBER_REVERSAL = LS_RBKP-BELNR_CC
        FISCALYEAR_REVERSAL       = LS_RBKP-GJAHR_CC
      TABLES
        RETURN                    = LT_RETURN.

    READ TABLE LT_RETURN WITH KEY TYPE = 'E'
                         TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      _G_ADD_1 LV_ERROR.
      MOVE-CORRESPONDING LT_RETURN[] TO LS_RBKP-RETURN[].
    ELSE.
      IF NOT LS_RBKP-BELNR_CC IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT   = 'X'
          IMPORTING
            RETURN = LS_RETURN_COMMIT.

        IF LS_RETURN_COMMIT-TYPE = 'E'.
          _G_ADD_1 LV_ERROR.
          APPEND LS_RETURN_COMMIT TO LS_RBKP-RETURN.
        ELSE.
          _G_ADD_1 LV_SUCCE.
        ENDIF.
      ELSE.
        _G_ADD_1 LV_ERROR.
        MOVE-CORRESPONDING LT_RETURN[] TO LS_RBKP-RETURN[].
      ENDIF.
    ENDIF.

    MODIFY LT_RBKP FROM LS_RBKP INDEX GV_TABIX.
  ENDLOOP.

  _G_WAIT_1_SECOND.

  SORT LT_RBKP BY BELNR GJAHR.

  LOOP AT LT_SELIDX INTO LS_SELIDX.
    READ TABLE GT_DISP INTO LS_DISP INDEX LS_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      GV_TABIX = SY-TABIX.

      READ TABLE LT_RBKP INTO LS_RBKP
                            WITH KEY BELNR = LS_DISP-BELNR
                                     GJAHR = LS_DISP-GJAHR
                            BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CLEAR: LS_DISP-MSGYN, LS_DISP-MSGTB.
        IF NOT LS_RBKP-BELNR_CC IS INITIAL.
          LS_DISP-BELNR_CC = LS_RBKP-BELNR_CC.
          LS_DISP-GJAHR_CC = LS_RBKP-GJAHR_CC.
          CLEAR LS_DISP-MSGYN.
        ELSE.
          PERFORM CONV_MSG_V1 TABLES LT_RETURN
                              CHANGING LS_DISP-MSGTB.
          LS_DISP-MSGYN = ICON_MESSAGE_ERROR.
        ENDIF.

        MODIFY GT_DISP FROM LS_DISP INDEX GV_TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE GT_DISP WHERE BELNR_CC NE SPACE.

*--> 결과 메시지 출력.
  IF LV_TOTAL = LV_SUCCE.
    MESSAGE S009 WITH LV_TOTAL.
  ELSE.
    MESSAGE S014 WITH LV_TOTAL LV_SUCCE LV_ERROR.
  ENDIF.

*--> Refresh Grid..
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_MAPPING_EXCUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_MAPPING_EXCUTE .

  " 맵핑 화면으로 이동.
*  CALL TRANSACTION 'ZDTV3_AP_P01'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_MAPPING_CANCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_MAPPING_CANCEL .

*  DATA: LV_TOTAL TYPE I,
*        LV_SUCCE TYPE I,
*        LV_ERROR TYPE I.
*
*  DATA: BEGIN OF LS_MAPPING,
*          BELNR    LIKE BKPF-BELNR,
*          GJAHR    LIKE BKPF-GJAHR,
*          BELNR_FI LIKE BKPF-BELNR,
*          GJAHR_FI LIKE BKPF-GJAHR,
*          INV_SEQ  LIKE ZDTV3T_AP_HEAD-INV_SEQ,
*          MSGTY    TYPE C,
*          MSGTX    TYPE BAPI_MSG,
*          RETURN   TYPE BAPIRET2_T,
*        END OF LS_MAPPING,
*        LT_MAPPING LIKE TABLE OF LS_MAPPING.
*
*  DATA: LS_RETURN TYPE ZDTV3S_RETURN,
*        LT_MAP_FI TYPE TABLE OF ZDTV3T_AP_EXT_D.
*
*  DATA: LR_ED_STATUS TYPE RANGE OF ZDTV3T_AP_HEAD-STATUS.
*
*  CLEAR: GT_MAPPING, GS_MAPPING.
*
*  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = DATA(LT_SELIDX).
*
*  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
*
*  IF LT_SELIDX[] IS INITIAL.
*    MESSAGE S006 DISPLAY LIKE 'I'. EXIT.
*  ENDIF.
*
**2210 [매입] 역발행요청중
**2211 [매입] 역발행요청완료
**2213 [매입] 역발행요청실패 -
**2215 [매입] 역발행요청승인
**2216 [매입] 역발행요청반려 -
**2220 [매입] 역발행요청취소중
**2221 [매입] 역발행요청취소 -
**2223 [매입] 역발행요청취소실패
*
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2213' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2216' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2221' ) TO LR_ED_STATUS.
*
**> 검색대상 자재 추출
*  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
*    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
*    IF SY-SUBRC = 0.
**      IF LS_DISP-INV_SEQ IS INITIAL.
**        _G_ADD_1 LV_ERROR.
**        MESSAGE S023 WITH '맵핑 되지 않은 전자세금계산서'(M04) DISPLAY LIKE 'E'.
**        CONTINUE.
**      ENDIF.
*
*      "역발행 시 진행 상태 확인
*      IF LS_DISP-INV_SIGN = 'X' AND NOT ( LS_DISP-ED_STATUS IN LR_ED_STATUS ).
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S035 WITH '전자세금계산서 상태'(M06) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      IF NOT LS_DISP-PAYSTS IS INITIAL.
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S023 WITH '대금지급 완료된 건'(M05) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      IF LS_DISP-WAERS NE GV_WAERS_LOCAL.
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S023 WITH '외화 지급 건'(M08) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      MOVE-CORRESPONDING LS_DISP TO LS_MAPPING.
*      APPEND LS_MAPPING TO LT_MAPPING.
*      CLEAR LS_MAPPING.
*    ENDIF.
*  ENDLOOP.
*
*  IF LINES( LT_MAPPING ) > 1.
*    _G_ADD_1 LV_ERROR.
*    MESSAGE S007 DISPLAY LIKE 'E'.
*  ENDIF.
*
*  CHECK LV_ERROR IS INITIAL.
*
*  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
*                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE "저장확인
*                              IV_TEXT1 = CONV #( '전자세금계산서 맵핑을 취소합니다' )
*                              IV_TEXT2 = CONV #( '계속 진행하시겠습니까?' ) )
*                              EQ ABAP_TRUE. " YES
*
*  LOOP AT LT_MAPPING INTO LS_MAPPING.
*
*    GV_TABIX = SY-TABIX.
*
*    CLEAR: LT_MAP_FI, LS_RETURN.
*
*    _G_ADD_1 LV_TOTAL.
*
*    APPEND VALUE #( INV_SEQ = LS_MAPPING-INV_SEQ
*                    EXT_SEQ = '1'
*                    ZGJAH   = LS_MAPPING-GJAHR_FI
*                    BELNR   = LS_MAPPING-BELNR_FI
*                    BUKRS   = P_BUKRS
*      ) TO LT_MAP_FI.
*
*    call function 'ZDTV3SAP_USER_DOCUMAP'
*      EXPORTING
*        INV_SEQ  = LS_MAPPING-INV_SEQ
**       ISSUE_ID =
*        INV_TYPE = 'AP'
*        INV_DEL  = 'X'
*      IMPORTING
*        RETURN   = LS_RETURN
*      TABLES
*        IT_DATA  = LT_MAP_FI.
*
*    LS_MAPPING-MSGTY = LS_RETURN-TYPE.
*
*    IF LS_MAPPING-MSGTY = 'S'.
*      _G_ADD_1 LV_SUCCE.
*    ELSE.
*      _G_ADD_1 LV_ERROR.
*      LS_MAPPING-MSGTX = LS_RETURN-MESSAGE.
*    ENDIF.
*
*    MODIFY LT_MAPPING FROM LS_MAPPING INDEX GV_TABIX.
*  ENDLOOP.
*
*  _G_WAIT_1_SECOND.
*
**--------------------------------------------------------------------*
**> 결과 UPDATE
**--------------------------------------------------------------------*
**> 전자세금계산서 조회를 위함.
*  DATA(LT_TMP_BKPF) = LT_MAPPING[].
*  SORT LT_TMP_BKPF BY BELNR_FI GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_BKPF COMPARING BELNR_FI GJAHR.
*  IF NOT LT_TMP_BKPF[] IS INITIAL.
*    SELECT  A~BELNR AS ED_BELNR,    "FI 회계전표
*            A~ZGJAH AS ED_GJAHR,     "FI 회계년도
*            A~INV_SEQ,            "IV SEQ
*            C~ISSUE_ID,           "승인번호
*            C~TYPE_CODE,          "세금계산서 종류
*            C~DESC_TEXT1,         "
*            C~INV_SIGN,           "발행구문
*            C~AMEND_CODE,         "수정사유
*            C~STATUS AS ED_STATUS,             "발행상태
*            C~NTS_SEND_FLAG,      "국세청전송여부
*            B~USE_DOC             "맵핑여부
*            FROM ZDTV3T_AP_EXT_D AS A INNER JOIN ZDTV3T_AP_EXT  AS B
*              ON A~BUKRS       = B~BUKRS      AND
*                 A~ISSUE_DATE  = B~ISSUE_DATE AND
*                 A~BUPLA       = B~BUPLA      AND
*                 A~INV_SEQ     = B~INV_SEQ
*                                      INNER JOIN ZDTV3T_AP_HEAD AS C
*              ON A~BUKRS       = C~BUKRS      AND
*                 A~ISSUE_DATE  = C~ISSUE_DATE AND
*                 A~BUPLA       = C~BUPLA      AND
*                 A~INV_SEQ     = C~INV_SEQ
*             FOR ALL ENTRIES IN @LT_TMP_BKPF
*           WHERE A~BUKRS    EQ @P_BUKRS
*             AND A~BLART    EQ @GC_RE
*             AND A~BELNR    EQ @LT_TMP_BKPF-BELNR_FI
*             AND A~ZGJAH    EQ @LT_TMP_BKPF-GJAHR
*             AND B~USE_DOC  EQ @ABAP_TRUE
*      INTO TABLE @DATA(LT_DTV3T).
*    FREE LT_TMP_BKPF.
*
*    SORT: LT_DTV3T BY ED_BELNR ED_GJAHR  ASCENDING.
*  ENDIF.
*
*  SORT LT_MAPPING BY BELNR GJAHR.
*
*  LOOP AT LT_SELIDX INTO LS_SELIDX.
*    READ TABLE GT_DISP INTO LS_DISP INDEX LS_SELIDX-INDEX.
*    IF SY-SUBRC = 0.
*      GV_TABIX = SY-TABIX.
*
*      READ TABLE LT_MAPPING INTO LS_MAPPING
*                            WITH KEY BELNR = LS_DISP-BELNR
*                                     GJAHR = LS_DISP-GJAHR
*                            BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        CLEAR: LS_DISP-MSGYN, LS_DISP-MSGTB.
*        IF LS_MAPPING-MSGTY = 'S'.
*          "성공 시 전자세금계산서 관련 REFRESH
*          READ TABLE LT_DTV3T INTO DATA(LS_DTV3T)
*                            WITH KEY ED_BELNR = LS_DISP-BELNR_FI
*                                     ED_GJAHR = LS_DISP-GJAHR
*                            BINARY SEARCH.
*          IF SY-SUBRC EQ 0.
*            MOVE-CORRESPONDING LS_DTV3T TO LS_DISP.
*          ELSE.
*            PERFORM SET_CLEAR_ED_DATA CHANGING LS_DISP.
*          ENDIF.
*
*          CLEAR LS_DISP-MSGYN.
*        ELSE.
*          APPEND VALUE #( ARBGB = 'ZMM01'
*                          TXTNR = '001'
*                          MSGTY = 'E'
*                          MSGV1 = LS_MAPPING-MSGTX )
*              TO LS_DISP-MSGTB.
*
*          LS_DISP-MSGYN = ICON_MESSAGE_ERROR.
*        ENDIF.
*
*        MODIFY GT_DISP FROM LS_DISP INDEX GV_TABIX.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*
**--> 결과 메시지 출력.
*  IF LV_TOTAL = LV_SUCCE.
*    MESSAGE S009 WITH LV_TOTAL.
*  ELSE.
*    MESSAGE S014 WITH LV_TOTAL LV_SUCCE LV_ERROR.
*  ENDIF.
*
**--> Refresh Grid..
*  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_DETAIL_POPUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_DETAIL_POPUP .

  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = DATA(LT_SELIDX).

  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.

  IF LT_SELIDX[] IS INITIAL.
    MESSAGE S006(ZMM01) DISPLAY LIKE 'I'. EXIT.
  ENDIF.

  CLEAR GT_DISP_DTL.
  SORT GT_DISP_DTL_ALL BY BELNR GJAHR.

*> 검색대상 자재 추출
  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      READ TABLE GT_DISP_DTL_ALL WITH KEY BELNR = LS_DISP-BELNR
                                          GJAHR = LS_DISP-GJAHR
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
      LOOP AT GT_DISP_DTL_ALL INTO DATA(LS_DISP_DTL) FROM SY-TABIX.
        IF LS_DISP_DTL-BELNR NE LS_DISP-BELNR OR
           LS_DISP_DTL-GJAHR NE LS_DISP-GJAHR.
          EXIT.
        ENDIF.

        APPEND LS_DISP_DTL TO GT_DISP_DTL.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF GT_DISP_DTL[] IS INITIAL.
    MESSAGE S070 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN '0200' STARTING AT 01 01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_REISSUE_CANCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_REISSUE_CANCEL .


*  DATA: LV_TOTAL TYPE I,
*        LV_SUCCE TYPE I,
*        LV_ERROR TYPE I.
*
*  DATA: BEGIN OF LS_MAPPING,
*          GJAHR    LIKE RSEG-GJAHR,
*          BELNR_FI LIKE BKPF-BELNR,
*          LIFNR    TYPE EKKO-LIFNR,
*          BUPLA    TYPE RBKP-BUPLA,
*          INV_SEQ  TYPE ZDTV3S_AP_HD-INV_SEQ,
*          MSGTB    TYPE ZYCN00001,
*        END OF LS_MAPPING,
*        LT_MAPPING LIKE TABLE OF LS_MAPPING.
*
*  DATA: LR_ED_STATUS TYPE RANGE OF ZDTV3T_AP_HEAD-STATUS.
*
*  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = DATA(LT_SELIDX).
*
*  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
*
*  IF LT_SELIDX[] IS INITIAL.
*    MESSAGE S006 DISPLAY LIKE 'I'. EXIT.
*  ENDIF.
*
**2210 [매입] 역발행요청중
**2211 [매입] 역발행요청완료
**2213 [매입] 역발행요청실패
**2215 [매입] 역발행요청승인
**2216 [매입] 역발행요청반려
**2220 [매입] 역발행요청취소중
**2221 [매입] 역발행요청취소
**2223 [매입] 역발행요청취소실패
*
**> 역발행 회수 가용 상태
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2215' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2216' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2220' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2221' ) TO LR_ED_STATUS.
*  APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = '2223' ) TO LR_ED_STATUS.
*
*  CLEAR: LV_TOTAL, LV_SUCCE, LV_ERROR.
*
**> 검색대상 자재 추출
*  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
*    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
*    IF SY-SUBRC = 0.
*      "역발행 시 상태 확인
*      IF LS_DISP-INV_SIGN = 'X' AND LS_DISP-ED_STATUS IN LR_ED_STATUS.
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S035 WITH '전자세금계산서 상태'(M06) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      IF LS_DISP-WAERS NE GV_WAERS_LOCAL.
*        _G_ADD_1 LV_ERROR.
*        MESSAGE S023 WITH '외화 지급 건'(M08) DISPLAY LIKE 'E'.
*        CONTINUE.
*      ENDIF.
*
*      MOVE-CORRESPONDING LS_DISP TO LS_MAPPING.
*      APPEND LS_MAPPING TO LT_MAPPING.
*      CLEAR LS_MAPPING.
*    ENDIF.
*  ENDLOOP.
*
*  IF LINES( LT_MAPPING ) > 1.
*    _G_ADD_1 LV_ERROR.
*    MESSAGE S007 DISPLAY LIKE 'E'.
*  ENDIF.
*
**> 역발행 회수 프로그램 호출
*  SUBMIT ZDTV3_AP WITH &0000001 = P_BUKRS
*                  WITH &0000002 = LS_DISP-BLDAT
*                  WITH &0000003 = LS_DISP-INV_SEQ
*                  WITH &0000006 = LS_DISP-ISSUE_ID
*                  WITH P_STAT7 = SPACE
*                  WITH P_STAT6 = SPACE
*                  WITH P_STAT8 = 'X'
*                  AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_REISSUE_SEND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_REISSUE_SEND .

*  DATA: LV_TOTAL TYPE I,
*        LV_SUCCE TYPE I,
*        LV_ERROR TYPE I.
*
*  DATA: LT_RETURN   TYPE TABLE OF ZDTV3S_RETURN,
*        LT_LFA1_ETC TYPE TABLE OF IDKR_VENVAT,
*        LT_LFB1 TYPE TABLE OF LFB1.
*
*  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = DATA(LT_SELIDX).
*
*  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
*
*  PERFORM CHECK_FOR_REISSUE_SEND TABLES LT_SELIDX
*                                 CHANGING LV_ERROR.
*
*  CHECK LV_ERROR IS INITIAL.
*
*  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
*                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE "저장확인
*                              IV_TEXT1 = CONV #( '역발행 정보를 전송합니다' )
*                              IV_TEXT2 = CONV #( '계속 진행하시겠습니까?' ) )
*                              EQ ABAP_TRUE. " YES
*
**> 공급업체
*  DATA(LT_TMP) = GT_MAPPING[].
*  SORT LT_TMP BY LIFNR.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING LIFNR.
*  IF NOT LT_TMP[] IS INITIAL.
*    SELECT LIFNR, DATAB, STCD1, STCD2, GESTYP, INDTYP, REPRES,
*           BUSINESS_PLACE, DEPARTMENT_NAME, PERSON_NAME,
*           PERSON_EMAIL1, PERSON_EMAIL2
*      FROM IDKR_VENVAT
*       FOR ALL ENTRIES IN @LT_TMP
*     WHERE LIFNR = @LT_TMP-LIFNR
*       AND DATAB <= @SY-DATUM
*      INTO CORRESPONDING FIELDS OF TABLE @LT_LFA1_ETC.
*    FREE LT_TMP.
*    SORT LT_LFA1_ETC BY LIFNR DATAB DESCENDING.
*  ENDIF.
*
**> 공급업체 EMAIL 주소
*  LT_TMP = GT_MAPPING[].
*  SORT LT_TMP BY LIFNR.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING LIFNR.
*  IF NOT LT_TMP[] IS INITIAL.
*    SELECT LIFNR, INTAD
*      FROM LFB1
*       FOR ALL ENTRIES IN @LT_TMP
*     WHERE LIFNR = @LT_TMP-LIFNR
*       AND BUKRS = @P_BUKRS
*      INTO CORRESPONDING FIELDS OF TABLE @LT_LFB1.
*    FREE LT_TMP.
*    SORT LT_LFB1 BY LIFNR.
*  ENDIF.
*
**> 공급받는 업체
*  CLEAR: GT_PKRBUPLA, GS_PKRBUPLA.
*  LT_TMP = GT_MAPPING[].
*  SORT LT_TMP BY BUPLA.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING BUPLA.
*  IF NOT LT_TMP[] IS INITIAL.
*    SELECT TAXNUMBER2, BUSINESSPLACENAME, TAXINVOICEREPRESENTATIVENAME,
*           ADDRESSNAME, INDUSTRYTYPE, BUSINESSTYPE
*      FROM PKRBUPLA
*      FOR ALL ENTRIES IN @LT_TMP
*     WHERE BRANCH = @LT_TMP-BUPLA
*       AND COMPANYCODE = @P_BUKRS
*      INTO CORRESPONDING FIELDS OF TABLE @GT_PKRBUPLA.
*
*    FREE LT_TMP.
*    SORT GT_PKRBUPLA BY BRANCH.
*  ENDIF.
*
*  DATA(LT_HEADER) = GT_DISP[].
*  DATA(LT_ITEM) = GT_DISP_DTL_ALL[].
*
*  SORT: LT_HEADER BY BELNR GJAHR,
*        LT_ITEM BY BELNR GJAHR.
*
**--------------------------------------------------------------------*
**> 선택된 라인에 대해 역발행 생성 시작
**--------------------------------------------------------------------*
*  LOOP AT GT_MAPPING INTO DATA(LS_MAPPING).
*    DATA(LV_TABIX) = SY-TABIX.
*
*    ADD 1 TO LV_TOTAL.
*
*    PERFORM SEND_REQ_RE_AP TABLES LT_HEADER
*                                    LT_ITEM
*                                    LT_LFA1_ETC
*                                    LT_LFB1
*                                    LT_RETURN
*                             USING LS_MAPPING-BELNR
*                                   LS_MAPPING-GJAHR
*                             CHANGING LS_MAPPING-INV_SEQ.
*
*    IF NOT LS_MAPPING-INV_SEQ IS INITIAL.
*      _G_ADD_1 LV_SUCCE.
*    ELSE.
*      _G_ADD_1 LV_ERROR.
*
*      PERFORM CONV_MSG_V2 TABLES LT_RETURN
*                          CHANGING LS_MAPPING-MSGTB.
*    ENDIF.
*
*    MODIFY GT_MAPPING FROM LS_MAPPING INDEX LV_TABIX.
*  ENDLOOP.
*
*  _G_WAIT_1_SECOND.
*
**--------------------------------------------------------------------*
**> 결과 UPDATE
**--------------------------------------------------------------------*
**> 전자세금계산서 조회를 위함.
*  DATA(LT_TMP_BKPF) = GT_MAPPING[].
*  SORT LT_TMP_BKPF BY BELNR_FI GJAHR.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_BKPF COMPARING BELNR_FI GJAHR.
*  IF NOT LT_TMP_BKPF[] IS INITIAL.
*    SELECT  A~BELNR AS ED_BELNR,    "FI 회계전표
*            A~ZGJAH AS ED_GJAHR,     "FI 회계년도
*            A~INV_SEQ,            "IV SEQ
*            C~ISSUE_ID,           "승인번호
*            C~TYPE_CODE,          "세금계산서 종류
*            C~DESC_TEXT1,         "
*            C~INV_SIGN,           "발행구문
*            C~AMEND_CODE,         "수정사유
*            C~STATUS AS ED_STATUS,             "발행상태
*            C~NTS_SEND_FLAG,      "국세청전송여부
*            B~USE_DOC             "맵핑여부
*            FROM ZDTV3T_AP_EXT_D AS A INNER JOIN ZDTV3T_AP_EXT  AS B
*              ON A~BUKRS       = B~BUKRS      AND
*                 A~ISSUE_DATE  = B~ISSUE_DATE AND
*                 A~BUPLA       = B~BUPLA      AND
*                 A~INV_SEQ     = B~INV_SEQ
*                                      INNER JOIN ZDTV3T_AP_HEAD AS C
*              ON A~BUKRS       = C~BUKRS      AND
*                 A~ISSUE_DATE  = C~ISSUE_DATE AND
*                 A~BUPLA       = C~BUPLA      AND
*                 A~INV_SEQ     = C~INV_SEQ
*             FOR ALL ENTRIES IN @LT_TMP_BKPF
*           WHERE A~BUKRS    EQ @P_BUKRS
*             AND A~BLART    EQ @GC_RE
*             AND A~BELNR    EQ @LT_TMP_BKPF-BELNR_FI
*             AND A~ZGJAH    EQ @LT_TMP_BKPF-GJAHR
*             AND B~USE_DOC  EQ @ABAP_TRUE
*      INTO TABLE @DATA(LT_DTV3T).
*    FREE LT_TMP_BKPF.
*
*    SORT: LT_DTV3T BY ED_BELNR ED_GJAHR  ASCENDING.
*  ENDIF.
*
*  SORT GT_MAPPING BY BELNR GJAHR.
*
*  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
*    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
*    IF SY-SUBRC = 0.
*      GV_TABIX = SY-TABIX.
*
*      READ TABLE GT_MAPPING INTO LS_MAPPING
*                            WITH KEY BELNR = LS_DISP-BELNR
*                                     GJAHR = LS_DISP-GJAHR
*                            BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        CLEAR: LS_DISP-MSGYN, LS_DISP-MSGTB.
*        IF NOT LS_MAPPING-INV_SEQ IS INITIAL.
*
*          READ TABLE LT_DTV3T INTO DATA(LS_DTV3T)
*                            WITH KEY ED_BELNR = LS_DISP-BELNR_FI
*                                     ED_GJAHR = LS_DISP-GJAHR
*                            BINARY SEARCH.
*          IF SY-SUBRC EQ 0.
*            MOVE-CORRESPONDING LS_DTV3T TO LS_DISP.
*
*            "세금계산서 종류
*            PERFORM GET_DOMAIN_TEXT USING 'ZDTV3D_TYPE_CODE'
*                                          LS_DISP-TYPE_CODE
*                                    CHANGING LS_DISP-TYPE_CODE_TX.
*
*            "상태내역
*            READ TABLE GT_ED_STATUS INTO GS_ED_STATUS
*                                    WITH KEY STATUS = GS_DISP-ED_STATUS
*                                    BINARY SEARCH.
*            IF SY-SUBRC EQ 0.
*              GS_DISP-ED_STATUS_TX = GS_ED_STATUS-STATUS_TEXT.
*            ENDIF.
*
*          ENDIF.
*
*          CLEAR LS_DISP-MSGYN.
*        ELSE.
*          LS_DISP-MSGTB = LS_MAPPING-MSGTB.
*
*          LS_DISP-MSGYN = ICON_MESSAGE_ERROR.
*        ENDIF.
*
*        MODIFY GT_DISP FROM LS_DISP INDEX GV_TABIX.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
**--> 결과 메시지 출력.
*  IF LV_TOTAL = LV_SUCCE.
*    MESSAGE S009 WITH LV_TOTAL.
*  ELSE.
*    MESSAGE S014 WITH LV_TOTAL LV_SUCCE LV_ERROR.
*  ENDIF.
*
**--> Refresh Grid..
*  GRF_GRID->REFRESH_GRID_DISPLAY( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_APPROVAL_LINK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_APPROVAL_LINK .


*  DATA : LR_WAERS LIKE RANGE OF BKPF-WAERS,        "통화키
*         LR_BLART LIKE RANGE OF BKPF-BLART,        "전표유형
*         LR_BLDAT LIKE RANGE OF BKPF-BLDAT,        "증빌일자
*         LR_BUDAT LIKE RANGE OF BKPF-BUDAT,        "전기일자
*         LR_CPUDT LIKE RANGE OF BKPF-CPUDT,        "입력일자
*         LR_USNAM LIKE RANGE OF BKPF-USNAM,        "입력자
*         LR_BELNR LIKE RANGE OF BKPF-BELNR,
*         LR_GJAHR LIKE RANGE OF BKPF-GJAHR,
*         LV_ERROR TYPE I,
*         LV_INFOM TYPE C.
*
*  CONSTANTS: LC_SCR_1000 TYPE SY-DYNNR VALUE '1000'.
*
*  DATA(LV_SYSID) = SY-UNAME.
*
*  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = DATA(LT_SELIDX).
*
*  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
*
*  IF LT_SELIDX[] IS INITIAL.
*    MESSAGE S006 DISPLAY LIKE 'I'. EXIT.
*  ENDIF.
*
**> 결재 상신 화면 이동 제어 시 예외 ID
*  ZCL_MM_COMMON=>COMMON_CONFIG(
*     EXPORTING  IS_COMMON =  VALUE #( M = 'E1' D = 'E1010' S = 'E1012' )
*                                     IT_WHERE = VALUE #(
*                                                         ( FIELD = 3 VALUE = SY-REPID )
*                                                       )
*     IMPORTING ET_OUTTAB = DATA(LT_APP_EXCEP) ).
*
*  SORT LT_APP_EXCEP BY FIELD1 FIELD2.
*
**> 검색대상 자재 추출
*  CLEAR LV_INFOM.
*  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
*    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_SELIDX-INDEX.
*    IF SY-SUBRC = 0.
*
*      "값이 있으면 모두 가능 (I/F ID 등 포함)
*      READ TABLE LT_APP_EXCEP WITH KEY FIELD1 = LS_DISP-ERFNAM
*                                       FIELD2 = SPACE
*                              BINARY SEARCH
*                              TRANSPORTING NO FIELDS.
*      IF SY-SUBRC NE 0.
*        "기본적으로 송장처리자는 LOGIN ID 와 일치되어야만 가능.
*        READ TABLE LT_APP_EXCEP INTO DATA(LS_APP_EXCEP)
*                                WITH KEY FIELD1 = LS_DISP-ERFNAM
*                                         FIELD2 = SY-UNAME
*                                BINARY SEARCH.
*        IF SY-SUBRC NE 0.
*          IF LS_DISP-ERFNAM NE LV_SYSID.
*            LV_INFOM = 'X'.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      APPEND VALUE #( SIGN = 'I'  OPTION = 'EQ'  LOW = LS_DISP-BELNR_FI ) TO LR_BELNR.
*      APPEND VALUE #( SIGN = 'I'  OPTION = 'EQ'  LOW = LS_DISP-GJAHR ) TO LR_GJAHR.
*      APPEND VALUE #( SIGN = 'I'  OPTION = 'EQ'  LOW = LS_DISP-BUDAT ) TO LR_BUDAT.
*    ENDIF.
*  ENDLOOP.
*
*  CHECK LV_ERROR IS INITIAL.
*
*  IF LV_INFOM = 'X'.
*    MESSAGE I128 WITH SY-UNAME.
*  ENDIF.
*
**> 전자결재 프로그램 호출
*  IF LR_BELNR[] IS INITIAL.
*    MESSAGE S033 WITH '결재상신 대상'(M09) DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  SUBMIT ZOFI1100 USING SELECTION-SCREEN LC_SCR_1000
*                   WITH P_BUKRS  EQ  P_BUKRS
*                   WITH S_BELNR  IN  LR_BELNR
*                   WITH S_GJAHR  IN  LR_GJAHR
*                   WITH P_RLDNR  EQ  '0L'
*                   WITH S_WAERS  IN  LR_WAERS
*                   WITH S_BLART  IN  LR_BLART
*                   WITH S_BLDAT  IN  LR_BLDAT
*                   WITH S_BUDAT  IN  LR_BUDAT
*                   WITH S_CPUDT  IN  LR_CPUDT
*                   WITH S_USNAM  IN  LR_USNAM
*                   WITH P_BKTX   EQ  SPACE
*                   WITH P_ALL    EQ  'X'
*                   WITH P_POST   EQ  'X'
*                   WITH P_PARK   EQ  'X'
*                   WITH P_REVC   EQ  'X'
*                    AND RETURN.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_MSG_V1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_RETURN
*&      <-- LS_DISP_MSGTB
*&---------------------------------------------------------------------*
FORM CONV_MSG_V1 TABLES IT_RETURN STRUCTURE BAPIRET2
                  CHANGING EV_MSGTB TYPE ZYCN00001.

  CONSTANTS: LC_FIELD TYPE FIELDNAME VALUE 'BELNR',
             LC_MSGID TYPE SY-MSGID VALUE 'ZMM01',
             LC_MSGNO TYPE SY-MSGNO VALUE '001'.

  LOOP AT IT_RETURN INTO DATA(LS_RETURN).
    CLEAR: GS_MSGTB.

    IF NOT ( LS_RETURN-TYPE EQ 'E' OR LS_RETURN-TYPE EQ 'S' ).
      CONTINUE.
    ENDIF.

    GS_MSGTB = VALUE #( FIELDNAME = LC_FIELD
                        MSGTY = LS_RETURN-TYPE
                        ARBGB = LS_RETURN-ID
                        TXTNR = LS_RETURN-NUMBER
                        MSGV1 = LS_RETURN-MESSAGE_V1
                        MSGV2 = LS_RETURN-MESSAGE_V2
                        MSGV3 = LS_RETURN-MESSAGE_V3
                        MSGV4 = LS_RETURN-MESSAGE_V4  ).

    IF GS_MSGTB-ARBGB IS INITIAL.
      GS_MSGTB-ARBGB = LC_MSGID.
      GS_MSGTB-TXTNR = LC_MSGNO.
      GS_MSGTB-MSGV1 = LS_RETURN-MESSAGE.
    ENDIF.

    APPEND GS_MSGTB TO EV_MSGTB.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_APVIF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_APVIFKEY
*&---------------------------------------------------------------------*
FORM POPUP_TO_APVIF USING IV_APVIFKEY.

*  DATA: LV_APVIFKEY TYPE ZAPVIFKEY,
*        LV_RESULT   TYPE BAPI_MTYPE,
*        LV_MESSAGE  TYPE  BAPI_MSG,
*        LT_APVKEY   TYPE TABLE OF ZSCN00219.
*
*  LV_APVIFKEY = IV_APVIFKEY.
*
*  call function 'ZFCN_APV_STATUS_MONI'
*    EXPORTING
*      IV_BUKRS    = P_BUKRS
*      IV_APVIFKEY = LV_APVIFKEY
*      IV_POPUP    = 'X'
*    IMPORTING
*      EV_RESULT   = LV_RESULT
*      EV_MESSAGE  = LV_MESSAGE
*    TABLES
*      IT_APVKEY   = LT_APVKEY.
***         ET_DATA           =
**
*  IF LV_RESULT = 'E'.
*    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
*  ENDIF.

ENDFORM.
