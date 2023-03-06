*----------------------------------------------------------------------*
***INCLUDE LZKIG_CN_0010F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_EINA_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EINA_DATA TABLES IT_EINA STRUCTURE ZSMM_INFOR_MAINT_EINA
                             ET_BAPI_T_EINA TYPE MEWIEINA_MIG_T.

  DATA: LS_BAPI_T_EINA TYPE MEWIEINA_MIG.

  CLEAR ET_BAPI_T_EINA.

  LOOP AT IT_EINA INTO DATA(LS_EINA).

*> INFO DATA SET
    CLEAR LS_BAPI_T_EINA.

*> 수신받은 INFO 번호가 있으면 이미 설정된 기준데이터 검색.
    IF NOT LS_EINA-INFNR IS INITIAL.
      READ TABLE GT_EINA_OLD INTO DATA(LS_EINA_OLD)
                             WITH KEY INFNR = LS_EINA-INFNR
                             BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        "공급업체 누락 시 설정.
        IF LS_EINA-ELIFN IS INITIAL.
          LS_BAPI_T_EINA-VENDOR = LS_EINA_OLD-LIFNR.
        ELSE.
          LS_BAPI_T_EINA-VENDOR = LS_EINA-ELIFN.  "공급업체번호
        ENDIF.

        "자재 누락 시 설정.
        IF LS_EINA-MATNR IS INITIAL.
          LS_BAPI_T_EINA-MATERIAL = LS_EINA_OLD-MATNR.
        ELSE.
          LS_BAPI_T_EINA-MATERIAL = LS_EINA-MATNR.  "자재 번호
        ENDIF.

        "자재그룹 누락 시 설정.
        IF LS_EINA-MATKL IS INITIAL.
          LS_BAPI_T_EINA-MAT_GRP = LS_EINA_OLD-MATKL.
        ELSE.
          LS_BAPI_T_EINA-MAT_GRP = LS_EINA-MATKL.  "자재 그룹
        ENDIF.

      ENDIF.

      LS_BAPI_T_EINA-INFO_REC = LS_EINA-INFNR.  "구매 정보 레코드 번호
    ELSE.
      LS_BAPI_T_EINA-VENDOR = LS_EINA-ELIFN.  "공급업체번호
      LS_BAPI_T_EINA-MATERIAL = LS_EINA-MATNR.  "자재 번호
      LS_BAPI_T_EINA-MAT_GRP = LS_EINA-MATKL.  "자재 그룹
    ENDIF.

    LS_BAPI_T_EINA-DELETE_IND = LS_EINA-ILOEA.  "구매 정보: 삭제 표시된 일반 데이터
    LS_BAPI_T_EINA-CREATED_AT = LS_EINA-ERDAT.  "레코드생성일
    LS_BAPI_T_EINA-CREATED_BY = LS_EINA-ERNAM.  "오브젝트 생성자 이름
    LS_BAPI_T_EINA-SHORT_TEXT = LS_EINA-EINATX.  "구매 정보 레코드에 대한 내역
    LS_BAPI_T_EINA-PO_UNIT = LS_EINA-BSTME.  "구매 오더 단위
    LS_BAPI_T_EINA-CONV_NUM1 = LS_EINA-BPUMZ.  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
    LS_BAPI_T_EINA-CONV_DEN1 = LS_EINA-BPUMN.  "오더 가격 단위에서 오더 단위로 환산하는 분모
    LS_BAPI_T_EINA-VEND_MAT = LS_EINA-IDNLF.  "공급업체가 사용하는 자재 번호
    LS_BAPI_T_EINA-SALES_PERS = LS_EINA-VERKF.  "질의의 경우 책임 영업사원
    LS_BAPI_T_EINA-CERT_NO = LS_EINA-URZNR.  "증명서 번호
    LS_BAPI_T_EINA-CERT_VALID = LS_EINA-URZDT.  "원산지 증명서 효력 종료
    LS_BAPI_T_EINA-CERT_CTRY = LS_EINA-ULAND.  "원산지 증명서 발행국
    LS_BAPI_T_EINA-CERT_TYPE = LS_EINA-URZTP.  "증명서 범주
    LS_BAPI_T_EINA-BASE_UOM = LS_EINA-MEINS.  "기본 단위
    LS_BAPI_T_EINA-VAR_ORD_UN = LS_EINA-VABME.  "가변 구매 오더 단위 활성
    LS_BAPI_T_EINA-BACK_AGREE = LS_EINA-RUECK.  "반품 계약
    LS_BAPI_T_EINA-SUPPL_FROM = LS_EINA-LIFAB.  "가용(납품가능) 시작일
    LS_BAPI_T_EINA-SUPPL_TO = LS_EINA-LIFBI.  "가용(납품가능) 종료일
    LS_BAPI_T_EINA-PRE_VENDOR = LS_EINA-KOLIF.  "이전 공급업체
    LS_BAPI_T_EINA-NORM_VEND = LS_EINA-RELIF.  "기본 공급업체

    APPEND LS_BAPI_T_EINA TO ET_BAPI_T_EINA.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EINAX_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EINAX_DATA TABLES IT_EINAX STRUCTURE ZSMM_INFOR_MAINT_EINAX
                              ET_BAPI_T_EINAX TYPE MEWIEINAX_T.

  DATA: LS_BAPI_T_EINAX TYPE MEWIEINAX_TY.

  CLEAR ET_BAPI_T_EINAX.

  LOOP AT IT_EINAX INTO DATA(LS_EINAX).
*> INFO DATA SET
    CLEAR LS_BAPI_T_EINAX.

    LS_BAPI_T_EINAX-INFO_RECN = LS_EINAX-INFNR.  "구매 정보 레코드 번호

    IF NOT LS_BAPI_T_EINAX-INFO_RECN IS INITIAL.
      LS_BAPI_T_EINAX-INFO_REC = 'X'.
    ENDIF.

    LS_BAPI_T_EINAX-MATERIAL = LS_EINAX-MATNR.  "자재 번호
    LS_BAPI_T_EINAX-MAT_GRP = LS_EINAX-MATKL.  "자재 그룹
    LS_BAPI_T_EINAX-VENDOR = LS_EINAX-ELIFN.  "공급업체 계정 번호
    LS_BAPI_T_EINAX-DELETE_IND = LS_EINAX-ILOEA.  "구매 정보: 삭제 표시된 일반 데이터
    LS_BAPI_T_EINAX-CREATED_AT = LS_EINAX-ERDAT.  "레코드생성일
    LS_BAPI_T_EINAX-CREATED_BY = LS_EINAX-ERNAM.  "오브젝트 생성자 이름
    LS_BAPI_T_EINAX-SHORT_TEXT = LS_EINAX-EINATX.  "구매 정보 레코드에 대한 내역
    LS_BAPI_T_EINAX-PO_UNIT = LS_EINAX-BSTME.  "구매 오더 단위
    LS_BAPI_T_EINAX-CONV_NUM1 = LS_EINAX-BPUMZ.  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
    LS_BAPI_T_EINAX-CONV_DEN1 = LS_EINAX-BPUMN.  "오더 가격 단위에서 오더 단위로 환산하는 분모
    LS_BAPI_T_EINAX-VEND_MAT = LS_EINAX-IDNLF.  "공급업체가 사용하는 자재 번호
    LS_BAPI_T_EINAX-SALES_PERS = LS_EINAX-VERKF.  "질의의 경우 책임 영업사원
    LS_BAPI_T_EINAX-CERT_NO = LS_EINAX-URZNR.  "증명서 번호
    LS_BAPI_T_EINAX-CERT_VALID = LS_EINAX-URZDT.  "원산지 증명서 효력 종료
    LS_BAPI_T_EINAX-CERT_CTRY = LS_EINAX-ULAND.  "원산지 증명서 발행국
    LS_BAPI_T_EINAX-CERT_TYPE = LS_EINAX-URZTP.  "증명서 범주
    LS_BAPI_T_EINAX-BASE_UOM = LS_EINAX-MEINS.  "기본 단위
    LS_BAPI_T_EINAX-VAR_ORD_UN = LS_EINAX-VABME.  "가변 구매 오더 단위 활성
    LS_BAPI_T_EINAX-BACK_AGREE = LS_EINAX-RUECK.  "반품 계약
    LS_BAPI_T_EINAX-SUPPL_FROM = LS_EINAX-LIFAB.  "가용(납품가능) 시작일
    LS_BAPI_T_EINAX-SUPPL_TO = LS_EINAX-LIFBI.  "가용(납품가능) 종료일
    LS_BAPI_T_EINAX-PRE_VENDOR = LS_EINAX-KOLIF.  "이전 공급업체
    LS_BAPI_T_EINAX-NORM_VEND = LS_EINAX-RELIF.  "기본 공급업체

    APPEND LS_BAPI_T_EINAX TO ET_BAPI_T_EINAX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EINE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EINE_DATA TABLES IT_EINE STRUCTURE ZSMM_INFOR_MAINT_EINE
                             ET_BAPI_T_EINE TYPE MEWIEINE_T.

  DATA: LS_BAPI_T_EINE TYPE MEWIEINE_TY.

  CLEAR ET_BAPI_T_EINE.

  LOOP AT IT_EINE INTO DATA(LS_EINE).

*> INFO DATA SET
    CLEAR: LS_BAPI_T_EINE, GS_MODE.
    GS_MODE = CORRESPONDING #( LS_EINE ).

*> 수신받은 INFO 번호가 있으면 이미 설정된 기준데이터 검색.
    READ TABLE GT_EINE_OLD INTO DATA(LS_EINE_OLD)
                           WITH KEY INFNR = LS_EINE-INFNR
                                    EKORG = LS_EINE-EKORG
                                    ESOKZ = LS_EINE-ESOKZ
                                    WERKS = LS_EINE-WERKS
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      GS_MODE-MODE = 'U'. "변경모드

      "구매그룹 누락 시 설정.
      IF LS_EINE-EKGRP IS INITIAL.
        LS_BAPI_T_EINE-PUR_GROUP = LS_EINE_OLD-EKGRP.
      ELSE.
        LS_BAPI_T_EINE-PUR_GROUP = LS_EINE-EKGRP.
      ENDIF.
    ELSE.
      LS_BAPI_T_EINE-PUR_GROUP = LS_EINE-EKGRP.
      GS_MODE-MODE = 'C'. "생성모드
    ENDIF.

    APPEND GS_MODE TO GT_MODE.

    LS_BAPI_T_EINE-INFO_REC = LS_EINE-INFNR.  "구매 정보 레코드 번호
    LS_BAPI_T_EINE-PURCH_ORG = LS_EINE-EKORG.  "구매 조직
    LS_BAPI_T_EINE-INFO_TYPE = LS_EINE-ESOKZ.  "구매정보레코드 범주
    LS_BAPI_T_EINE-PLANT = LS_EINE-WERKS.  "플랜트

    LS_BAPI_T_EINE-DELETE_IND = LS_EINE-ILOEE.  "구매 정보: 삭제 표시된 구매 조직 데이터
    LS_BAPI_T_EINE-CREATED_AT = LS_EINE-ERDAT.  "레코드생성일
    LS_BAPI_T_EINE-CREATED_BY = LS_EINE-ERNAM.  "오브젝트 생성자 이름
    LS_BAPI_T_EINE-CURRENCY = LS_EINE-WAERS.  "통화 키
    LS_BAPI_T_EINE-MIN_PO_QTY = LS_EINE-MINBM.  "최소 구매 오더 수량
    LS_BAPI_T_EINE-NRM_PO_QTY = LS_EINE-NORBM.  "표준 구매 오더 수량
    LS_BAPI_T_EINE-PLND_DELRY = LS_EINE-PLIFZ.  "계획 납품 소요 시간(일)
    LS_BAPI_T_EINE-OVERDELTOL = LS_EINE-UEBTO.  "초과 납품 허용 한도
    LS_BAPI_T_EINE-UNLIMITED = LS_EINE-UEBTK.  "지시자: 무제한 초과 납품 허용
    LS_BAPI_T_EINE-UNDER_TOL = LS_EINE-UNTTO.  "미달 납품 허용 한도
    LS_BAPI_T_EINE-NET_PRICE = LS_EINE-NETPR.  "구매정보레코드의 단가
    LS_BAPI_T_EINE-PRICE_UNIT = LS_EINE-EPEIN.  "가격단위
    LS_BAPI_T_EINE-ORDERPR_UN = LS_EINE-BBPRM.  "오더 가격 단위(구매)
    LS_BAPI_T_EINE-PRICE_DATE = LS_EINE-PREDT.  "가격결정일
    LS_BAPI_T_EINE-CONV_NUM1 = LS_EINE-BPUMZ.  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
    LS_BAPI_T_EINE-CONV_DEN1 = LS_EINE-BPUMN.  "오더 가격 단위에서 오더 단위로 환산하는 분모
    LS_BAPI_T_EINE-EFF_PRICE = LS_EINE-EFFPR.  "구매정보레코드에서 유효가격
    LS_BAPI_T_EINE-ACKN_REQD = LS_EINE-KZABS.  "발주 확인 필요
    LS_BAPI_T_EINE-TAX_CODE = LS_EINE-MWSKZ.  "부가가치세 코드
    LS_BAPI_T_EINE-VAL_TYPE = LS_EINE-BWTAR.  "평가 유형
    LS_BAPI_T_EINE-SHIPPING = LS_EINE-EVERS.  "출하 지시
    LS_BAPI_T_EINE-EXP_IMP_P = LS_EINE-EXPRF.  "단가유형
    LS_BAPI_T_EINE-CONF_CTRL = LS_EINE-BSTAE.  "확인 관리 키
    LS_BAPI_T_EINE-PRICEDATE = LS_EINE-MEPRF.  "가격결정일 제어
    LS_BAPI_T_EINE-INCOTERMS1 = LS_EINE-INCO1.  "인도 조건(파트 1)
    LS_BAPI_T_EINE-INCOTERMS2 = LS_EINE-INCO2.  "인코텀스(파트 2)
    LS_BAPI_T_EINE-VERSION = LS_EINE-VERID.  "생산 버전
    LS_BAPI_T_EINE-MAX_PO_QTY = LS_EINE-MAXBM.  "최대 구매 오더 수량
    LS_BAPI_T_EINE-ROUND_PROF = LS_EINE-RDPRF.  "반올림 프로파일
    LS_BAPI_T_EINE-UNIT_GROUP = LS_EINE-MEGRU.  "단위 그룹
    LS_BAPI_T_EINE-INCOTERMSV = LS_EINE-INCOV.  "인코텀스 버전
    LS_BAPI_T_EINE-INCOTERMS2L = LS_EINE-INCO2_L.  "인코텀스 장소 1
    LS_BAPI_T_EINE-INCOTERMS3L = LS_EINE-INCO3_L.  "인코텀스 장소 2
    LS_BAPI_T_EINE-AUTO_SOURCE = LS_EINE-AUT_SOURCE.  "자동 소싱 관련
    LS_BAPI_T_EINE-GR_BASEDIV = LS_EINE-WEBRE.  "GR BASE IV

    APPEND LS_BAPI_T_EINE TO ET_BAPI_T_EINE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_EINEX_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_EINEX_DATA TABLES IT_EINEX STRUCTURE ZSMM_INFOR_MAINT_EINEX
                              ET_BAPI_T_EINEX TYPE MEWIEINEX_T.


  DATA: LS_BAPI_T_EINEX TYPE MEWIEINEX_TY.

  CLEAR ET_BAPI_T_EINEX.

  LOOP AT IT_EINEX INTO DATA(LS_EINEX).
*> INFO DATA SET
    CLEAR LS_BAPI_T_EINEX.

    LS_BAPI_T_EINEX-INFO_RECN = LS_EINEX-INFNR.  "구매 정보 레코드 번호

    IF NOT LS_BAPI_T_EINEX-INFO_RECN IS INITIAL.
      LS_BAPI_T_EINEX-INFO_REC = 'X'.
    ENDIF.

*> KT&G는 하나의 INFO 번호는 다른 조직 및 PLANT 등을 가질 수 없다!!
    READ TABLE GT_MODE INTO GS_MODE
                       WITH KEY INFNR = LS_EINEX-INFNR
                       BINARY SEARCH.

    IF GS_MODE-MODE EQ 'U'.
      CLEAR: LS_BAPI_T_EINEX-PURCH_ORG, LS_BAPI_T_EINEX-INFO_TYPE, LS_BAPI_T_EINEX-PLANT.
    ELSE.
      LS_BAPI_T_EINEX-PURCH_ORG = LS_EINEX-EKORG.  "구매 조직
      LS_BAPI_T_EINEX-INFO_TYPE = LS_EINEX-ESOKZ.  "구매정보레코드 범주
      LS_BAPI_T_EINEX-PLANT = LS_EINEX-WERKS.  "플랜트
    ENDIF.

    LS_BAPI_T_EINEX-DELETE_IND = LS_EINEX-ILOEE.  "구매 정보: 삭제 표시된 구매 조직 데이터
    LS_BAPI_T_EINEX-CREATED_AT = LS_EINEX-ERDAT.  "레코드생성일
    LS_BAPI_T_EINEX-CREATED_BY = LS_EINEX-ERNAM.  "오브젝트 생성자 이름
    LS_BAPI_T_EINEX-PUR_GROUP = LS_EINEX-EKGRP.  "구매 그룹
    LS_BAPI_T_EINEX-CURRENCY = LS_EINEX-WAERS.  "통화 키
    LS_BAPI_T_EINEX-MIN_PO_QTY = LS_EINEX-MINBM.  "최소 구매 오더 수량
    LS_BAPI_T_EINEX-NRM_PO_QTY = LS_EINEX-NORBM.  "표준 구매 오더 수량
    LS_BAPI_T_EINEX-PLND_DELRY = LS_EINEX-PLIFZ.  "계획 납품 소요 시간(일)
    LS_BAPI_T_EINEX-OVERDELTOL = LS_EINEX-UEBTO.  "초과 납품 허용 한도
    LS_BAPI_T_EINEX-UNLIMITED = LS_EINEX-UEBTK.  "지시자: 무제한 초과 납품 허용
    LS_BAPI_T_EINEX-UNDER_TOL = LS_EINEX-UNTTO.  "미달 납품 허용 한도
    LS_BAPI_T_EINEX-NET_PRICE = LS_EINEX-NETPR.  "구매정보레코드의 단가
    LS_BAPI_T_EINEX-PRICE_UNIT = LS_EINEX-EPEIN.  "가격단위
    LS_BAPI_T_EINEX-ORDERPR_UN = LS_EINEX-BBPRM.  "오더 가격 단위(구매)
    LS_BAPI_T_EINEX-PRICE_DATE = LS_EINEX-PREDT.  "가격결정일
    LS_BAPI_T_EINEX-CONV_NUM1 = LS_EINEX-BPUMZ.  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
    LS_BAPI_T_EINEX-CONV_DEN1 = LS_EINEX-BPUMN.  "오더 가격 단위에서 오더 단위로 환산하는 분모
    LS_BAPI_T_EINEX-EFF_PRICE = LS_EINEX-EFFPR.  "구매정보레코드에서 유효가격
    LS_BAPI_T_EINEX-ACKN_REQD = LS_EINEX-KZABS.  "발주 확인 필요
    LS_BAPI_T_EINEX-TAX_CODE = LS_EINEX-MWSKZ.  "부가가치세 코드
    LS_BAPI_T_EINEX-VAL_TYPE = LS_EINEX-BWTAR.  "평가 유형
    LS_BAPI_T_EINEX-SHIPPING = LS_EINEX-EVERS.  "출하 지시
    LS_BAPI_T_EINEX-EXP_IMP_P = LS_EINEX-EXPRF.  "단가유형
    LS_BAPI_T_EINEX-CONF_CTRL = LS_EINEX-BSTAE.  "확인 관리 키
    LS_BAPI_T_EINEX-PRICEDATE = LS_EINEX-MEPRF.  "가격결정일 제어
    LS_BAPI_T_EINEX-INCOTERMS1 = LS_EINEX-INCO1.  "인도 조건(파트 1)
    LS_BAPI_T_EINEX-INCOTERMS2 = LS_EINEX-INCO2.  "인코텀스(파트 2)
    LS_BAPI_T_EINEX-VERSION = LS_EINEX-VERID.  "생산 버전
    LS_BAPI_T_EINEX-MAX_PO_QTY = LS_EINEX-MAXBM.  "최대 구매 오더 수량
    LS_BAPI_T_EINEX-ROUND_PROF = LS_EINEX-RDPRF.  "반올림 프로파일
    LS_BAPI_T_EINEX-UNIT_GROUP = LS_EINEX-MEGRU.  "단위 그룹
    LS_BAPI_T_EINEX-INCOTERMSV = LS_EINEX-INCOV.  "인코텀스 버전
    LS_BAPI_T_EINEX-INCOTERMS2L = LS_EINEX-INCO2_L.  "인코텀스 장소 1
    LS_BAPI_T_EINEX-INCOTERMS3L = LS_EINEX-INCO3_L.  "인코텀스 장소 2
    LS_BAPI_T_EINEX-AUTO_SOURCE = LS_EINEX-AUT_SOURCE.  "자동 소싱 관련
    LS_BAPI_T_EINEX-GR_BASEDIV = LS_EINEX-WEBRE.  "GR BASE IV

    APPEND LS_BAPI_T_EINEX TO ET_BAPI_T_EINEX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TXT_LINES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_TXT_LINES TABLES IT_TXT_LINES STRUCTURE ZSMM_INFOR_MAINT_TXT_LINES
                              ET_BAPI_TXT_LINES TYPE MEWIPIRTEXT_TT.


  DATA: LS_BAPI_TXT_LINES TYPE MEWIPIRTEXT_TY.

  CLEAR ET_BAPI_TXT_LINES.

  LOOP AT IT_TXT_LINES INTO DATA(LS_TXT_LINES).

*> INFO DATA SET
    CLEAR LS_BAPI_TXT_LINES.

    LS_BAPI_TXT_LINES-INFO_REC = LS_TXT_LINES-INFNR.  "구매 정보 레코드 번호
    LS_BAPI_TXT_LINES-EINE_INDX = LS_TXT_LINES-NUM.  "순번
    LS_BAPI_TXT_LINES-TDOBJECT = LS_TXT_LINES-TDOBJECT.  "텍스트: 어플리케이션 오브젝트
    LS_BAPI_TXT_LINES-TDSPRAS = LS_TXT_LINES-SPRAS.  "언어 키
    LS_BAPI_TXT_LINES-TDFORMAT = LS_TXT_LINES-TDFORMAT.  "태그열
    LS_BAPI_TXT_LINES-TDLINE = LS_TXT_LINES-TDLINE.  "텍스트 라인

    APPEND LS_BAPI_TXT_LINES TO ET_BAPI_TXT_LINES.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COND_VALIDITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_COND_VALIDITY TABLES IT_COND_VALIDITY STRUCTURE ZSMM_INFOR_MAINT_COND_VALIDITY
                                 ET_BAPI_COND_VALIDITY TYPE MEWIVALIDITY_TT.


  DATA: LS_BAPI_COND_VALIDITY TYPE MEWIVALIDITY_TY.

  CLEAR ET_BAPI_COND_VALIDITY.

  LOOP AT IT_COND_VALIDITY INTO DATA(LS_COND_VALIDITY).

*> INFO DATA SET
    CLEAR LS_BAPI_COND_VALIDITY.

    LS_BAPI_COND_VALIDITY-INFO_REC = LS_COND_VALIDITY-INFO_REC.  "구매 정보 레코드 번호
    LS_BAPI_COND_VALIDITY-EINE_INDX = LS_COND_VALIDITY-EINE_INDX.  "순번
    LS_BAPI_COND_VALIDITY-SERIAL_ID = LS_COND_VALIDITY-SERIAL_ID.  "조건 레코드 번호
    LS_BAPI_COND_VALIDITY-BASE_UOM = LS_COND_VALIDITY-BASE_UOM.  "기본 단위
    LS_BAPI_COND_VALIDITY-PLANT = LS_COND_VALIDITY-PLANT.  "플랜트
    LS_BAPI_COND_VALIDITY-VALID_FROM = LS_COND_VALIDITY-VALID_FROM.  "조건 레코드의 효력 시작일
    LS_BAPI_COND_VALIDITY-VALID_TO = LS_COND_VALIDITY-VALID_TO.  "조건 레코드의 효력 종료일


    APPEND LS_BAPI_COND_VALIDITY TO ET_BAPI_COND_VALIDITY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CONDITION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_CONDITION TABLES IT_CONDITION STRUCTURE ZSMM_INFOR_MAINT_CONDITION
                             ET_BAPI_CONDITION STRUCTURE MEWICONDITION_TY.

  DATA: LS_BAPI_CONDITION TYPE MEWICONDITION_TY.

  CLEAR ET_BAPI_CONDITION.

  LOOP AT IT_CONDITION INTO DATA(LS_CONDITION).

*> INFO DATA SET
    CLEAR LS_BAPI_CONDITION.

    LS_BAPI_CONDITION-INFO_REC = LS_CONDITION-INFO_REC.  "구매 정보 레코드 번호
    LS_BAPI_CONDITION-EINE_INDX = LS_CONDITION-EINE_INDX.  "순번
    LS_BAPI_CONDITION-SERIAL_ID = LS_CONDITION-SERIAL_ID.  "조건 레코드 번호
    LS_BAPI_CONDITION-COND_COUNT = LS_CONDITION-COND_COUNT.  "조건순번
    LS_BAPI_CONDITION-DELETION_IND = LS_CONDITION-DELETION_IND.  "조건 레코드에 대한 삭제 지시자
    LS_BAPI_CONDITION-COND_TYPE = LS_CONDITION-COND_TYPE.  "조건유형
    LS_BAPI_CONDITION-SCALE_CURR = LS_CONDITION-SCALE_CURR.  "통화 키
    LS_BAPI_CONDITION-COND_VALUE = LS_CONDITION-COND_VALUE.  "조건 금액
    LS_BAPI_CONDITION-CURRENCY = LS_CONDITION-CURRENCY.  "통화 키
    LS_BAPI_CONDITION-COND_P_UNT = LS_CONDITION-COND_P_UNT.  "조건 가격결정 단위
    LS_BAPI_CONDITION-COND_UNIT = LS_CONDITION-COND_UNIT.  "조건 단위
    LS_BAPI_CONDITION-NUMERATOR = LS_CONDITION-NUMERATOR.  "조건 단위를 기본 단위로 변환 시 사용할 분자
    LS_BAPI_CONDITION-DENOMINATOR = LS_CONDITION-DENOMINATOR.  "조건 단위를 기본 단위로 변환 시 사용할 분모
    LS_BAPI_CONDITION-BASE_UOM = LS_CONDITION-BASE_UOM.  "기본 단위
    LS_BAPI_CONDITION-VENDOR_NO = LS_CONDITION-VENDOR_NO.  "공급업체 또는 채권자의 계정 번호
    LS_BAPI_CONDITION-CHANGE_ID = LS_CONDITION-CHANGE_ID.  "변경 유형

    APPEND LS_BAPI_CONDITION TO ET_BAPI_CONDITION.
  ENDLOOP.

ENDFORM.
