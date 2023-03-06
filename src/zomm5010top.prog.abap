*&---------------------------------------------------------------------*
*& Include          ZOMM5010TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Type-Pools                                                           *
*----------------------------------------------------------------------*
TYPE-POOLS : ABAP, SLIS.

CONTROLS: ITEM_DETAIL TYPE TABSTRIP,
          HEADER_DETAIL TYPE TABSTRIP.

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DYN_0020,
         BUTTON(64),
       END OF TY_DYN_0020.

TYPES: BEGIN OF TY_DYN_0030,
         LABEL(80),
       END   OF TY_DYN_0030.

TYPES: BEGIN OF TY_DYNPRO_FLOW,
         DYNPRO_CALLING TYPE SY-DYNNR,
         DYNPRO_CALLED  TYPE SY-DYNNR,
         DYNPRO_FLOW    TYPE I,
       END OF TY_DYNPRO_FLOW.

TYPES: BEGIN OF TY_PAPER_TAX,
         MWSKZ TYPE ZTMM00002-FIELD1,
         TEXT  TYPE ZTMM00002-FIELD2,
       END OF TY_PAPER_TAX.

*----------------------------------------------------------------------*
* Include                                                              *
*----------------------------------------------------------------------*
INCLUDE : <ICON>, <SYMBOL>.

*----------------------------------------------------------------------*
* Tables                                                               *
*----------------------------------------------------------------------*
TABLES : EKKO, EKBE, LFA1, T001, RBKP, MLAN, EKPO.


*----------------------------------------------------------------------*
* Internal table
*----------------------------------------------------------------------*
DATA: GS_LVC_FCAT TYPE LVC_S_FCAT,
      GT_UI_FUNC  TYPE UI_FUNCTIONS,
      GS_VARIANT  TYPE DISVARIANT.


DATA : BEGIN OF GS_EXPAND,
         H TYPE ABAP_BOOL,
         I TYPE ABAP_BOOL,
         D TYPE ABAP_BOOL,
       END OF GS_EXPAND.

DATA: GT_DYNPRO_FLOW TYPE TABLE OF TY_DYNPRO_FLOW,
      GS_DYNPRO_FLOW TYPE          TY_DYNPRO_FLOW.

DATA:
  BEGIN OF GS_LEFT,
    LIFNR        LIKE INVFO-LIFNR,   "업체
    LIFNR_TX     LIKE LFA1-NAME1,    "업체명
    WAERS        LIKE INVFO-WAERS,   "통화
    MWSKZ        LIKE INVFO-MWSKZ,   "세금
    MENGE        LIKE INVFO-MENGE,   "총수량
    WMWST        LIKE INVFO-WMWST,   "총세액
    DMBTR        LIKE INVFO-DMBTR,   "총금액
    LIGHT        LIKE ICON-NAME,             "전자세금계산서 수령여부
    ERROR(1)     TYPE C,
    LINECOLOR(4) TYPE C,
    STCD2        TYPE LFA1-STCD2,
  END OF GS_LEFT,
  GT_LEFT LIKE TABLE OF GS_LEFT.

DATA:
  BEGIN OF GS_SCR100,
    BUKRS          LIKE INVFO-BUKRS,      "회사코드
    BKTXT          LIKE INVFO-BKTXT,      "비고
    LIFNR          LIKE INVFO-LIFNR,      "공급업체
    STCEG          LIKE INVFO-STCEG,      "사업자번호
    BLDAT          LIKE INVFO-BLDAT,      "송장일자
    BUDAT          LIKE INVFO-BUDAT,      "전기일
    MWSKZ          LIKE INVFO-MWSKZ,      "세금코드
    ZTERM          LIKE INVFO-ZTERM,      "지급조건
    ZLSCH          LIKE INVFO-ZLSCH,      "지급방법
    ZFBDT          LIKE INVFO-ZFBDT,      "기산일
    NETDT          LIKE INVFO-NETDT,      "만기일
    ZBD1T          TYPE DZBD1T,           "현금할인기간
    BVTYP          LIKE INVFO-BVTYP,      "공급업체은행(파트너은행유형)
    LIFRE          LIKE LFA1-LIFNR,       "송장발행처(운송업체)
    DMBTR          LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "입고기준 공급가액
    WMWST          LIKE RM08M-DIFFERENZ, "INVFO-WMWST,  "입고기준 부가세액
    TOTAL          LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "입고기준 총액
    DMBTR2         LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "공급가
    WMWST2         LIKE RM08M-DIFFERENZ, "INVFO-WMWST,  "세액
    TOTAL2         LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "총액
    TOTAL2_KRW(20) TYPE C,                "총액(KRW)
    DIFFERENZ      LIKE RM08M-DIFFERENZ,  "잔액
    STATUS         LIKE ICONS-TEXT,       "상태
    XMWST          LIKE INVFO-XMWST,      "세금계산 체크박스
    WAERS          LIKE INVFO-WAERS,      "현지통화
    BELNR(15)      TYPE C,                "송장번호 + 회계년도
    WWERT          LIKE INVFO-WWERT,      "환율적용일자
    KURSF          TYPE INVFO-KURSF,      "환율
    WAERS2         LIKE INVFO-WAERS,      "전표통화
    HBKID          LIKE INVFO-HBKID,      "거래은행
    HKTID          LIKE INVFO-HKTID,      "계정 ID
    BUPLA          LIKE INVFO-BUPLA,      "사업장

    DWPAYC         LIKE RM08M-DIFFERENZ,   "선급반제금액
    DWPAYC_M       LIKE RM08M-DIFFERENZ,   "선급반제금액 MAX
    DWTAX          LIKE INVFO-MWSKZ,       "선급금 세금코드
    DELAYF         LIKE RM08M-DIFFERENZ,   "지체상금
    DELAYF_M       LIKE RM08M-DIFFERENZ,   "지체상금 MAX
    DELAYF_X       TYPE C,   "지체상금률 존재여부

    PRTCH          LIKE RM08M-DIFFERENZ,   "인쇄교체비
*    ACTEX          LIKE ZSVCMM_REALCOST-ACTEX,    "실비 금액

    LABEL1(10)     TYPE C,                "공급가 라벨
    LABEL2(10)     TYPE C,                "세액 라벨
    LABEL3(10)     TYPE C,                "총액 라벨
    LABEL4(10)     TYPE C,                "차이 라벨
    LABEL5(10)     TYPE C,                "선급금반제금액
    LABEL6(10)     TYPE C,                "지체상금금액

*    ZEXPEN_EML     LIKE ZSVMM_USER_INFO-EML, "지출결의 담당자 이메일 주소
    BUPLA_TX       TYPE J_1BBRANCH-NAME,   "사업장명

    EMPFK          TYPE LFZA-EMPFK,                "U1. 대체수취인
    EMPFK_TX       TYPE LFA1-NAME1,             "U1. 대체수취인명
    DIFF_LIFNR     TYPE LIFRE,                "U1. 다른 송장 발행처
    DIFF_LIFNR_TX  TYPE LFA1-NAME1,        "U1. 다른 송장 발행처명

  END OF GS_SCR100.

DATA GS_SCR100_BACK LIKE GS_SCR100.


DATA:
  BEGIN OF GS_HEAD,
    CBOX(1)                TYPE C,             "선택


*/-CONTROL 레벨 사용하기 때문에 인터널테이블 구조변경 주의
    EBELN2                 LIKE EKBE-EBELN,    "구매오더번호
    EBELP2                 LIKE EKBE-EBELP,    "구매오더품번
    KSCHL2                 LIKE EKBZ-KSCHL,    "조건유형

    LIFNR                  LIKE EKKO-LIFNR,    "공급업체/운송업체
    LIFNR_TX               LIKE LFA1-NAME1,    "공급업체명
    WAERS                  LIKE EKBE-WAERS,    "통화
    MWSKZ                  LIKE EKPO-MWSKZ,    "세금코드

    GJAHR                  LIKE EKBE-GJAHR,    "전표연도
    BELNR                  LIKE EKBE-BELNR,    "입고번호


*-/CONTROL 레벨
    BUZEI                  LIKE EKBE-BUZEI,    "입고품목
    BUDAT                  LIKE EKBE-BUDAT,    "입고일자
    CHARG                  LIKE EKBE-CHARG,    "배치
    EBELN                  LIKE EKBE-EBELN,    "구매오더번호
    EBELP                  LIKE EKBE-EBELP,    "구매오더품번
    MENGE                  LIKE EKBE-MENGE,    "수량
    MEINS                  LIKE EKPO-MEINS,    "단위
    MENGE2                 LIKE EKBE-MENGE,    "수량
    MEINS2                 LIKE EKPO-MEINS,    "단위
    PEINH                  LIKE EKPO-PEINH,    "가격단위
    NETPR                  LIKE EKPO-NETPR,    "오더단가
    BPRME                  LIKE EKPO-BPRME,    "오더가격단위
    TAXIM                  LIKE MLAN-TAXIM,    "세금지시자
    TAXM1_T                LIKE TMKM1T-TAXIB,    "세금지시자 내역
    DMBTR                  LIKE EKBE-WRBTR,    "입고금액 또는 월말단가확정금액
    DMBTR_KRW              LIKE EKBE-DMBTR,    "입고금액(KRW)
    DMBTR_KRW_NEW          LIKE EKBE-DMBTR,    "입고금액(KRW):내자,기타수수료에서 사용
    WRBTR                  LIKE EKBE-WRBTR,    "전표통화
    AREWR                  LIKE EKBZ-AREWR,    "GR/IR 반제값(현지통화)
    AREWW                  LIKE EKBZ-AREWW,    "GR/IR 반제값(전표통화)
    DPAMT                  LIKE EKKO-DPAMT,    "선급금
    DPPCT                  LIKE EKKO-DPPCT,    "선급율(%)
    SVAT_AMT               LIKE EKBE-DMBTR,    "의제매입세
*    ACTEX                  LIKE ZSVCMM_REALCOST-ACTEX,    "실비 금액
    VGABE                  LIKE EKBE-VGABE,    "트랜잭션유형
    BWART                  LIKE EKBE-BWART,    "이동유형
    SHKZG                  LIKE EKBE-SHKZG,    "차/대지시자
    XBLNR                  LIKE EKBE-XBLNR,    "참조전표번호
    LFGJA                  LIKE EKBE-LFGJA,    "전표년도
    LFBNR                  LIKE EKBE-LFBNR,    "참조전표번호
    LFPOS                  LIKE EKBE-LFPOS,    "참조전표품목
    WERKS                  LIKE EKBE-WERKS,    "플랜트
    WERKS_TX               LIKE T001W-NAME1,   "플랜트내역
    MATNR                  LIKE EKBE-MATNR,    "자재번호
    MATNR_TX               LIKE EKPO-TXZ01,    "자재내역
    EKORG                  LIKE EKKO-EKORG,    "구매조직
    BSART                  LIKE EKKO-BSART,    "문서유형
    BSTYP                  LIKE EKKO-BSTYP,    "문서범주
    LATE_RATE              LIKE EKKO-LATE_RATE, "지체상금
    KNTTP                  LIKE EKPO-KNTTP,    "계정지정범주
    PSTYP                  LIKE EKPO-PSTYP,    "품목범주
    ZTERM                  LIKE EKKO-ZTERM,    "지급조건
    CPUDT                  LIKE EKBE-CPUDT,    "전표입력일
    CPUTM                  LIKE EKBE-CPUTM,    "입력시간
    STCD2                  LIKE LFA1-STCD2,    "사업자번호
    INFNR                  LIKE EINA-LIFNR,    "정보레코드번호
    KBETR                  LIKE KONP-KBETR,    "정보레코드단가
    KPEIN                  LIKE KONP-KPEIN,    "가격단위
    KONWA                  LIKE KONP-KONWA,    "통화
    BELNR2                 LIKE RBKP-BELNR,    "송장번호
    GJAHR2                 LIKE RBKP-GJAHR,    "회계년도
    STUNR                  LIKE EKBZ-STUNR,    "단계번호
    ZAEHK                  LIKE EKBZ-ZAEHK,    "조건카운터
    KSCHL                  LIKE EKBZ-KSCHL,    "조건유형
    BRTWR                  LIKE EKPO-BRTWR,
    LEBRE                  LIKE EKPO-LEBRE,    "서비스기준 송장검증 지시자
    TXZ01                  LIKE EKPO-TXZ01,    "내역

    KOKRS                  LIKE EKKN-KOKRS,    "관리회계영역
    SAKTO                  LIKE EKKN-SAKTO,    "GL 계정
    SAKTO_TX               LIKE SKAT-TXT20,    "GL 계정명
    PRCTR                  LIKE EKKN-PRCTR,    "손익센터
    PRCTR_TX               LIKE CEPCT-KTEXT,   "손익센터명
    KOSTL                  LIKE EKKN-KOSTL,    "코스트센터
    KOSTL_TX               LIKE CSKT-KTEXT,    "코스트센터명
    FISTL                  LIKE EKKN-FISTL,    "Fund Center
    FISTL_TX               LIKE FMFCTRT-BEZEICH, "Fund Center명
    VBELN                  LIKE EKKN-VBELN,    "판매문서번호
    VBELP                  LIKE EKKN-VBELP,    "판매문서품목
    ANLN1                  LIKE EKKN-ANLN1,    "자산번호
    ANLN1_TX               LIKE ANLA-TXT50,    "자산번호 내역
    PSPNR                  LIKE EKKN-PS_PSP_PNR, "WBS
    PSPNR_TX               LIKE PRPS-POST1,     "WBS명
    AUFNR                  LIKE EKKN-AUFNR,     "Internal Order
    AUFNR_TX               LIKE AUFK-KTEXT,     "Internal Order 내역
    USER0                  LIKE AUFK-USER0,     "과제 계좌번호
    KNUMV                  LIKE EKKI-KNUMV,
    ABSGR                  LIKE EKKO-ABSGR,     "세금계산서방식( 02: 역발행, 그외 정발행)


    BISMT                  LIKE MARA-BISMT,     "기존자재번호

    ZORDER_PERSON          LIKE EKKO-ZORDER_PERSON,   "발주자자
    ZORDER_PERSON_NAME(40) TYPE C,              "발주자명
    ZORDER_DEPARTMENT      LIKE EKKO-ZORDER_DEPARTMENT,   "발주부서
    ZORDER_DEPART_NAME(40) TYPE C,              "발주부서명

    ZEXPEN_PERSON          LIKE EKKO-ZEXPEN_PERSON,   "지출결의자
    ZEXPEN_PERSON_NAME(40) TYPE C,              "지출결의자명
    ZEXPEN_DEPARTMENT      LIKE EKKO-ZEXPEN_DEPARTMENT,   "지출결의부서
    ZEXPEN_DEPART_NAME(40) TYPE C,              "지출결의부서명
*    ZEXPEN_EML             LIKE ZSVMM_USER_INFO-EML, "지출결의 담당자 EMAIL

    EKGRP                  LIKE EKKO-EKGRP,     "구매그룹
    BWTAR                  LIKE EKBE-BWTAR,     "평가 유형
    MATKL                  LIKE EKPO-MATKL,     "자재그룹
    SPLIT_KEY              TYPE CHAR10,         "분할 송장 판단을 위한 KEY
*    INV_SEQ                TYPE ZDTV3S_AP_HD-INV_SEQ, "전자세금계산서 번호

    PO_TEXT(300)           TYPE C,              "자재 구매오더텍스트
    MESG(220)              TYPE C,              "정보레코드관련 에러메시지
    LIFNR_LAND             TYPE LFA1-LAND1,     "공급업체 국가
  END OF GS_HEAD,
  GT_HEAD LIKE TABLE OF GS_HEAD.

DATA GT_HEAD_ALL LIKE TABLE OF GS_HEAD.
DATA GT_EKBE LIKE TABLE OF GS_HEAD.
DATA GV_LAST_CURSOR_61 TYPE SCREEN-NAME.

*-계정지정데이터
TYPES: BEGIN OF TY_EKKN,
         EBELN    LIKE EKKN-EBELN,      "구매오더번호
         EBELP    LIKE EKKN-EBELP,      "구매오더품번
         KOKRS    LIKE EKKN-KOKRS,      "관리회계영역
         SAKTO    LIKE EKKN-SAKTO,      "GL 계정
         SAKTO_TX LIKE SKAT-TXT20,
         PRCTR    LIKE EKKN-PRCTR,      "손익센터
         PRCTR_TX LIKE CEPCT-KTEXT,
         KOSTL    LIKE EKKN-KOSTL,      "코스트센터
         KOSTL_TX LIKE CSKT-KTEXT,
         FISTL    LIKE EKKN-FISTL,      "Fund Center
         FISTL_TX LIKE FMFCTRT-BEZEICH,
         VBELN    LIKE EKKN-VBELN,      "판매문서번호
         VBELP    LIKE EKKN-VBELP,      "판매문서품목
         ANLN1    LIKE EKKN-ANLN1,      "자산번호
         ANLN1_TX LIKE ANLA-TXT50,
         PSPNR    LIKE EKKN-PS_PSP_PNR, "WBS
         PSPNR_TX LIKE PRPS-POST1,
         AUFNR    LIKE EKKN-AUFNR,      "Internal Order
         AUFNR_TX LIKE AUFK-KTEXT,
         USER0    LIKE AUFK-USER0,      "과제계좌번호
       END OF TY_EKKN.

DATA: GT_EKKN TYPE TABLE OF TY_EKKN,
      GS_EKKN TYPE  TY_EKKN.

DATA: BEGIN OF GS_PREPAID,
        EBELN LIKE EKBE-EBELN,
        EBELP LIKE EKBE-EBELP,
        VGABE LIKE EKBE-VGABE,
        SHKZG LIKE EKBE-SHKZG,
        DMBTR LIKE EKBE-DMBTR,
      END OF GS_PREPAID,
      GT_PREPAID LIKE TABLE OF GS_PREPAID.


DATA GT_LOG LIKE TABLE OF BAPIRET2.

DATA:
  BEGIN OF GS_BUPLA,  "사업장
    BUPLA LIKE J_1BBRANCH-BRANCH,
    NAME  LIKE J_1BBRANCH-NAME,
  END OF GS_BUPLA,
  GT_BUPLA LIKE TABLE OF GS_BUPLA,

  BEGIN OF GS_MWSKZ,  "세금
    MWSKZ TYPE T007S-MWSKZ,
    TEXT1 TYPE T007S-TEXT1,
  END OF GS_MWSKZ,
  GT_MWSKZ LIKE TABLE OF GS_MWSKZ,

  BEGIN OF GS_ZLSCH,  "지급방법
    ZLSCH TYPE T042ZT-ZLSCH,
    TEXT2 TYPE T042ZT-TEXT2,
  END OF GS_ZLSCH,
  GT_ZLSCH LIKE TABLE OF GS_ZLSCH.

DATA GS_T052 LIKE T052.

*> 플랜트별 사업장 설정을 위함.
DATA: BEGIN OF GS_T001W,
        WERKS      LIKE T001W-WERKS,
        J_1BBRANCH LIKE T001W-J_1BBRANCH,
      END OF GS_T001W,
      GT_T001W LIKE TABLE OF GS_T001W.

DATA: GR_BSART TYPE RANGE OF EKKO-BSART,
      GR_BSTYP   TYPE RANGE OF EKKO-BSTYP,
      GR_DPPCT   TYPE RANGE OF EKKO-DPPCT,
      GR_ABSGR   TYPE RANGE OF EKKO-ABSGR,
      GR_VGABE   TYPE RANGE OF VGABE,
      GR_BUPLA   TYPE RANGE OF J_1BBRANC_,
      GR_WERKS   TYPE RANGE OF WERKS_D,
      GR_MWSKZ_P TYPE RANGE OF EKPO-MWSKZ. "불공제 세액 세금코드


DATA: GT_MAPPING LIKE TABLE OF DSELC,
      GS_MAPPING LIKE DSELC.
DATA GT_RETURN_TAB LIKE TABLE OF DDSHRETVAL.

DATA: BEGIN OF GS_HBKID,
        HBKID LIKE T012T-HBKID,
        HKTID LIKE T012T-HKTID,
        TEXT1 LIKE T012T-TEXT1,
      END OF GS_HBKID,
      GT_HBKID LIKE TABLE OF GS_HBKID.


*DATA GT_USERNM TYPE TABLE OF ZSVMM_USER_INFO.

DATA: BEGIN OF GS_PO_ETC_AMT,
        EBELN          LIKE EKKO-EBELN,
        DPPCT          LIKE EKKO-DPPCT,
        LATE_RATE      LIKE EKKO-LATE_RATE,
        DOWNPAY_REQ    LIKE ZSVCMM_DOWNPAY-DOWNPAY_REQ,
        DOWNPAY_AMOUNT LIKE ZSVCMM_DOWNPAY-DOWNPAY_AMOUNT,
        DOWNPAY_TAX    LIKE ZSVCMM_DOWNPAY-DOWNPAY_TAX,
        DOWNPAY_CLEAR  LIKE ZSVCMM_DOWNPAY-DOWNPAY_CLEAR,
        CLEAR_TAX      LIKE ZSVCMM_DOWNPAY-CLEAR_TAX,
        CBOX           TYPE C,
      END OF GS_PO_ETC_AMT,
      GT_PO_ETC_AMT LIKE TABLE OF GS_PO_ETC_AMT.

*> 선급금 현황 조회
TYPES: BEGIN OF TY_DATA_DPAMT_HIS,
         EBELN          TYPE EKKO-EBELN, "구매오더번호
         WAERS          TYPE EKKO-WAERS, "통화
         DMBTR          TYPE BSEG-WRBTR, "공급액
         WMWST          TYPE BSEG-WRBTR, "세액
         DOWNPAY_REQ    TYPE ZSVCMM_DOWNPAY-DOWNPAY_REQ, "선급요청액
         DOWNPAY_AMOUNT TYPE ZSVCMM_DOWNPAY-DOWNPAY_AMOUNT, "선급금
         DOWNPAY_CLEAR  TYPE ZSVCMM_DOWNPAY-DOWNPAY_CLEAR, "선급반제액
         DOWNPAY_REM    TYPE ZSVCMM_DOWNPAY-DOWNPAY_CLEAR, "선급잔액
       END OF TY_DATA_DPAMT_HIS.

DATA: GT_DISP_POP TYPE TABLE OF TY_DATA_DPAMT_HIS,
      GS_DISP_POP TYPE TY_DATA_DPAMT_HIS.

*U1> 계좌 우선순위 추가. (대체수취인 -> 송장발행처 -> 공급업체 순)
DATA: BEGIN OF GS_BANK_ACC,
        BVTYP                 TYPE ZCCMM_BANKACCNT-BPBANKACCOUNTINTERNALID,
        BANK                  TYPE ZCCMM_BANKACCNT-BANK,
        BANKA                 TYPE ZCCMM_BANKACCNT-BANKA,
        BANKACCOUNT           TYPE ZCCMM_BANKACCNT-BANKACCOUNT,
        BANKACCOUNTHOLDERNAME TYPE ZCCMM_BANKACCNT-BANKACCOUNTHOLDERNAME,
      END OF GS_BANK_ACC,
      GT_BANK_ACC LIKE TABLE OF GS_BANK_ACC.

*U1> 대체수취인 기능 추가.
DATA: BEGIN OF GS_TAX_LFZA,
        LIFNR TYPE LFZA-LIFNR,
        EMPFK TYPE LFZA-EMPFK,
        NAME1 TYPE LFA1-NAME1,
      END OF GS_TAX_LFZA,
      GT_TAX_LFZA LIKE TABLE OF GS_TAX_LFZA.

*U1> 다른 송장 발행처 추가
DATA: BEGIN OF GS_DIFF_LIFNR,
        LIFNR TYPE LFA1-LIFNR,
        NAME1 TYPE LFA1-NAME1,
      END OF GS_DIFF_LIFNR,
      GT_DIFF_LIFNR LIKE TABLE OF GS_DIFF_LIFNR.

*U4> 송장전기월과 입고전기월이 다를때 메시지 표시하기 위함.
DATA: GV_DIFF_GR_MONTH TYPE C,
      GS_CONFIG_E1013 TYPE ZTMM00002.  "U4 (글로벌로 선언.. 사용처가 많음)
*--------------------------------------------------------------------*
*> 송장 분할 기능
*--------------------------------------------------------------------*
DATA: GT_PAPER_TAX TYPE TABLE OF TY_PAPER_TAX,
      BEGIN OF GS_POST_SPLIT,
        USE       TYPE C,
        BASE_LINE TYPE I,
        SEL_LINE  TYPE I,
      END OF GS_POST_SPLIT.

DATA: BEGIN OF GS_BAPI_SPLIT,
        DMBTR     TYPE RM08M-DIFFERENZ,
        WMWST     TYPE RM08M-DIFFERENZ,
        TOTAL     TYPE RM08M-DIFFERENZ,
        SPLIT_KEY TYPE CHAR10,
        BELNR     TYPE RBKP-BELNR,
        GJAHR     TYPE RBKP-GJAHR,
        AWKEY     TYPE BKPF-AWKEY,
        ITEM_CNT  TYPE I, "ITEM 개수 (역발행 용)
        MATNR_TX  TYPE MAKT-MAKTX, "대표 자재 명 (역발행 용)
*        INV_SEQ   TYPE ZDTV3S_AP_HD-INV_SEQ,
      END OF GS_BAPI_SPLIT,
      GT_BAPI_SPLIT LIKE TABLE OF GS_BAPI_SPLIT.

DATA: GV_INCL_TAXIM_Y TYPE C. "U5> Y 이면 의제매입 포함 / 그외 미포함

* (+)u6 START
  DATA : GT_TEMP  LIKE TABLE OF GS_HEAD.

  TYPES : BEGIN OF TY_EKPO ,
           EBELN LIKE EKPO-EBELN,
           EBELP LIKE EKPO-EBELP,
           VRTKZ LIKE EKPO-VRTKZ,
         END OF TY_EKPO.
  DATA GT_EKPO TYPE TABLE OF TY_EKPO.
  TYPES : BEGIN OF TY_EKBE_MA,
           EBELN LIKE EKBE_MA-EBELN,
           EBELP LIKE EKBE_MA-EBELP,
           ZEKKN LIKE EKBE_MA-ZEKKN,
           SHKZG LIKE EKBE_MA-SHKZG,
           MENGE LIKE EKBE_MA-MENGE,
           WRBTR LIKE EKBE_MA-WRBTR,
           GJAHR LIKE EKBE_MA-GJAHR,
           BELNR LIKE EKBE_MA-BELNR,
           BUZEI LIKE EKBE_MA-BUZEI,
         END OF TY_EKBE_MA.
  DATA GT_EKBE_MA TYPE TABLE OF TY_EKBE_MA.
* (+)u6 END
*----------------------------------------------------------------------*
* Global Variable
*----------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM,
      GV_VISIBLE_N       TYPE ABAP_BOOL,
      GV_DYNAMIC_TXT     TYPE GUI_TEXT,
      GV_TAB1_TITLE(128) TYPE C,
      GV_LAND1           TYPE T001-LAND1,
      GV_LOCAL_CUR       TYPE T001-WAERS,
      GV_EMPFK_USED      TYPE C,
      GV_DIFF_LIFNR_USED TYPE C,
      OK_CODE_POPUP      TYPE SY-UCOMM.

DATA: GS_DYN_0020 TYPE TY_DYN_0020,
      GS_DYN_0021 TYPE TY_DYN_0020,
      GS_DYN_0022 TYPE TY_DYN_0020,
      GS_DYN_0030 TYPE TY_DYN_0030.

DATA: GV_CALL_SUBSCREEN TYPE SYDYNNR,
      GV_EXC_USER(10)   TYPE C. "EX01 은 회사코드도 변경 EX02 는 부서만 변경

DATA: GV_BUKRS_CODE(5) TYPE C,
      GR_BANKN         TYPE RANGE OF ZCCMM_BANKACCNT-BANKACCOUNT.

DATA : GRF_DE_CON TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GRF_DE_GRID  TYPE REF TO CL_GUI_ALV_GRID,
       GT_DE_FIELD  TYPE LVC_T_FCAT, "Fieldcatalog
       GS_DE_LAYOUT TYPE LVC_S_LAYO.
*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
DEFINE _G_APPEND.

  &1-sign   = &2.
  &1-option = &3.
  &1-low    = &4.
  append &1.
  clear  &1.

END-OF-DEFINITION.

DEFINE _G_APPEND2.
  &1-sign   = &2.
  &1-option = &3.
  &1-low    = &4.
  &1-high   = &5.
  append &1.
  clear  &1.
END-OF-DEFINITION.
DEFINE _G_APPEND3.

  &1 = VALUE #( BASE &1 ( SIGN = &2 OPTION = &3 LOW = &4 ) ).

END-OF-DEFINITION.
DEFINE _G_APPEND4.

  &1 = VALUE #( BASE &1 ( SIGN = &2 OPTION = &3 LOW = &4 HIGH = &5 ) ).

END-OF-DEFINITION.
*----------------------------------------------------------------------*
* Global Constants
*----------------------------------------------------------------------*
CONSTANTS :
  GC_EQ(2)          TYPE C                     VALUE 'EQ',
  GC_NE(2)          TYPE C                     VALUE 'NE',
  GC_BT(2)          TYPE C                     VALUE 'BT',
  GC_CP(2)          TYPE C                     VALUE 'CP',
  GC_I(1)           TYPE C                     VALUE 'I',

  GC_MASK(6)        TYPE C                     VALUE '. / - ',

  GC_STRUC_LOG      LIKE DD02L-TABNAME         VALUE 'BAPIRET2',
  GC_COLINFO        TYPE LVC_S_LAYO-CTAB_FNAME VALUE 'COLINFO',
  GC_CELLTAB        TYPE LVC_S_LAYO-STYLEFNAME VALUE 'CELLTAB',
  GC_LINEINFO       TYPE LVC_S_LAYO-INFO_FNAME VALUE 'LINEINFO',

  GC_CONTAINER_HEAD TYPE SCRFNAME              VALUE 'CONTAINER_HEAD',
  GC_CONTAINER_TAB1 TYPE SCRFNAME              VALUE 'CONTAINER_TAB1',

  GC_01(2)          TYPE C                     VALUE '01',
  GC_02(2)          TYPE C                     VALUE '02',
  GC_10(2)          TYPE C                     VALUE '10',
  GC_102(3)         TYPE C                     VALUE '102',
  GC_122(3)         TYPE C                     VALUE '122',
  GC_123(3)         TYPE C                     VALUE '123',
  GC_161(3)         TYPE C                     VALUE '161',
  GC_162(3)         TYPE C                     VALUE '162',
  GC_1000(4)        TYPE C                     VALUE '1000',

  GC_LE(2)          TYPE C                     VALUE 'LE',
  GC_RE(2)          TYPE C                     VALUE 'RE',
  GC_KR(2)          TYPE C                     VALUE 'KR',

  GC_KRW            TYPE TCURC-WAERS           VALUE 'KRW',

  GC_C111(4)        TYPE C                     VALUE 'C111',

  GC_EBELN(5)       TYPE C                     VALUE 'EBELN',
  GC_EBELP(5)       TYPE C                     VALUE 'EBELP',
  GC_VGABE(5)       TYPE C                     VALUE 'VGABE',

  GC_F0001(5)       TYPE C                     VALUE 'F0001',
  GC_F0002(5)       TYPE C                     VALUE 'F0002',
  GC_BELNR(5)       TYPE C                     VALUE 'BELNR',
  GC_BELNR2         TYPE LVC_FNAME             VALUE 'BELNR2',
  GC_LIFNR(5)       TYPE C                     VALUE 'LIFNR',
  GC_LIFNR_TX(8)    TYPE C                     VALUE 'LIFNR_TX',
  GC_TAXKR(5)       TYPE C                     VALUE 'TAXKR',
  GC_BUPLA          TYPE DFIES-FIELDNAME       VALUE 'BUPLA',
  GC_MWSKZ          TYPE DFIES-FIELDNAME       VALUE 'MWSKZ',
  GC_ZLSCH          TYPE DFIES-FIELDNAME       VALUE 'ZLSCH',
  GC_HBKID(5)       TYPE C                     VALUE 'HBKID',
  GC_ZCOLOR(6)      TYPE C                     VALUE 'ZCOLOR',
  GC_LIFRE(15)      TYPE C                     VALUE 'GS_SCR100-LIFRE',
  GC_HBKID2(15)     TYPE C                     VALUE 'GS_SCR100-HBKID',
  GC_HKTID2(15)     TYPE C                     VALUE 'GS_SCR100-HKTID',

  GC_BEST           TYPE THEAD-TDID            VALUE 'BEST',
  GC_EKPO           TYPE THEAD-TDOBJECT        VALUE 'EKPO',
  GC_MATERIAL       TYPE THEAD-TDOBJECT        VALUE 'MATERIAL',

  GC_0002           TYPE SYDYNNR               VALUE '0002',
  GC_0003           TYPE SYDYNNR               VALUE '0003',
  GC_0030           TYPE SYDYNNR               VALUE '0030',
  GC_0040           TYPE SYDYNNR               VALUE '0040',
  GC_0053           TYPE SYDYNNR               VALUE '0053',
  GC_0060           TYPE SYDYNNR               VALUE '0060',
  GC_0061           TYPE SYDYNNR               VALUE '0061',
  GC_PICK           TYPE SYUCOMM               VALUE 'PICK',
  GC_DUMMY          TYPE SYUCOMM               VALUE 'DUMMY',
  GC_CLEAR          TYPE SYUCOMM               VALUE 'CLEAR',
  GC_ENTER          TYPE SYUCOMM               VALUE 'ENTER',
  GC_SAVE           TYPE SYUCOMM               VALUE 'SAVE',

  GC_MEV0020BUTTON  TYPE SYUCOMM               VALUE 'MEV0020BUTTON',
  GC_MEV0021BUTTON  TYPE SYUCOMM               VALUE 'MEV0021BUTTON',
  GC_MEV0022BUTTON  TYPE SYUCOMM               VALUE 'MEV0022BUTTON',
  GC_EXPANDCLOSE    TYPE SYUCOMM               VALUE 'EXPANDCLOSE',
  GC_REFRESH_HEAD   TYPE SYUCOMM               VALUE 'REFRESH_HEAD',
  GC_CHANGE         TYPE SYUCOMM               VALUE 'CHANGE',
  GC_CALCAMT        TYPE SYUCOMM               VALUE 'CALCAMT',
  GC_DYN            TYPE SYUCOMM               VALUE 'DYN',
  GC_STABLE         TYPE LVC_S_STBL            VALUE 'XX',
  GC_HISTORY        TYPE SYUCOMM               VALUE 'HISTORY',
  GC_OVERVIEW       TYPE SYUCOMM               VALUE 'OVERVIEW',
  GC_MODE           TYPE SYUCOMM               VALUE 'MODE',
  GC_INIT           TYPE SYUCOMM               VALUE 'INIT',
  GC_FTAX2          TYPE SYUCOMM               VALUE 'FTAX2',
  GC_ISSUE          TYPE SYUCOMM               VALUE 'ISSUE',
  GC_ISSUE_R        TYPE SYUCOMM               VALUE 'ISSUE_R',
  GC_CHG_KALSK      TYPE SYUCOMM               VALUE 'CHG_KALSK',
  GC_DWPAY_LOG      TYPE SYUCOMM               VALUE 'DWPAY_LOG',
  GC_CRT_EACC       TYPE SYUCOMM               VALUE 'CRT_EACC',


  GC_TREE           TYPE DISVARIANT-HANDLE     VALUE 'TREE',
  GC_HEAD           TYPE DISVARIANT-HANDLE     VALUE 'HEAD',
  GC_ITEM           TYPE DISVARIANT-HANDLE     VALUE 'ITEM',
  GC_TAB1           TYPE DISVARIANT-HANDLE     VALUE 'TAB1',
  GC_CHANGED        TYPE FIELDNAME             VALUE 'CHANGED',

  GC_STRUC_LEFT     LIKE DD02L-TABNAME         VALUE 'GS_LEFT',
  GC_STRUC_HEAD     LIKE DD02L-TABNAME         VALUE 'GS_HEAD',
  GC_STRUC_TAB1     LIKE DD02L-TABNAME         VALUE 'GS_TAB1',

  GC_EXCLUD(15)     TYPE C                     VALUE 'GT_UI_FUNC[]',
  GC_CODE           TYPE DFIES-FIELDNAME       VALUE 'CODE',
  GC_ICON_MF(4)     TYPE C                     VALUE '@MF@',
  GC_ICON_RED(3)    TYPE C                     VALUE '@5C',

  GC_EKBE           TYPE SE16N_TAB             VALUE 'EKBE',
  GC_EKBZ           TYPE SE16N_TAB             VALUE 'EKBZ',
  GC_LINECOLOR(9)   TYPE C                     VALUE 'LINECOLOR'.

CONSTANTS: GC_BUKRS_1101 TYPE T001-BUKRS VALUE '1101',
           GC_BUKRS_2101  TYPE T001-BUKRS VALUE '2101',
           GC_BUKRS_3101  TYPE T001-BUKRS VALUE '3101',
           GC_CODE_KTG(3) TYPE C VALUE 'KTG',
           GC_CODE_KGC(3) TYPE C VALUE 'KGC',
           GC_CODE_YJP(3) TYPE C VALUE 'YJP',
           GC_ABSGR_01    TYPE EKKO-ABSGR VALUE '01',      "정발행
           GC_ABSGR_02    TYPE EKKO-ABSGR VALUE '02',      "역발행
           GC_MSGID       TYPE SY-MSGID VALUE 'ZMM01'.
