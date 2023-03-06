*&---------------------------------------------------------------------*
*&  Include           ZOMM5020TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: ICON.

*TABLES: ZDTV3T_AP_HEAD, "매입 전자세금계산서 Head
*        ZDTV3T_AP_EXT_D,  "매입 추가정보 상세
TABLES: J_1BBRANCH,       "사업장
        ADRC,             "주소
        LFA1,
        RBKP,
        EKKO,
        EKPO.

*----------------------------------------------------------------------*
*       Internal tables / Structure                                    *
*----------------------------------------------------------------------*
DATA: GT_LVC_FCAT_T TYPE LVC_T_FCAT,
      GT_LVC_FCAT_B TYPE LVC_T_FCAT,
      GS_LVC_FCAT   TYPE LVC_S_FCAT,
      GS_LVC_LAYO_T TYPE LVC_S_LAYO,
      GS_LVC_LAYO_B TYPE LVC_S_LAYO,
      GT_UI_FUNC    TYPE UI_FUNCTIONS,
      GT_FUNC_0110 TYPE UI_FUNCTIONS.



*-세금계산서 헤더(상단 ALV)
DATA:
  BEGIN OF GS_HEADER,
    CBOX(4)          TYPE C,                           "선택
    LIGHT(20)        TYPE C,                           "결재상태(신호등)
    STATUS(5)        TYPE C,             "결재상태코드
    STATUS_TX(20)    TYPE C,                           "결재상태명
*    USE_DOC          LIKE ZDTV3T_AP_EXT-USE_DOC,       "전표매핑여부
    BELNR            LIKE RBKP-BELNR,                  "송장번호
*    BELNR2           LIKE ZDTV3T_AP_EXT_D-BELNR,       "회계전표번호
*    GJAHR            LIKE ZDTV3T_AP_EXT_D-ZGJAH,       "회계년도
    IVDOC_TYPE(20)   TYPE C,                           "전표 구분 (일반, 차후차/대변, 대변메모)
    TAXIV_TYPE(20)   TYPE C,                           "세금계산서 구분 (정상전자/수정))
*    BLDAT            LIKE ZDTV3T_AP_EXT_D-BLDAT,       "작성일
*    BUDAT            LIKE ZDTV3T_AP_EXT_D-BUDAT,       "전기일
*    BUPLA            LIKE ZDTV3T_AP_EXT_D-BUPLA,       "사업장
    LIFNR            LIKE RBKP-LIFNR,                  "공급업체
    LIFNR_TX         LIKE LFA1-NAME1,                  "공급업체명
*    INV_SIGN         LIKE ZDTV3T_AP_HEAD-INV_SIGN,     "세금계산서작성방법
    INV_SIGN_TX(20)  TYPE C,                           "세금계산서작성방법
*    STCD2            LIKE ZDTV3T_AP_HEAD-SU_ID,        "사업자번호
*    CHARGETOTAL      LIKE ZDTV3T_AP_HEAD-CHARGETOTAL,  "공급가액
*    TAXTOTAL         LIKE ZDTV3T_AP_HEAD-TAXTOTAL,     "부가세액
*    GRANDTOTAL       LIKE ZDTV3T_AP_HEAD-GRANDTOTAL,   "총액
*    ISSUE_ID         LIKE ZDTV3T_AP_HEAD-ISSUE_ID,     "세금계산서 승인번호
*    TYPE_CODE        LIKE ZDTV3T_AP_HEAD-TYPE_CODE,    "세금계산서종류
    TYPE_CODE_TX(40) TYPE C,                           "세금계산서종류 내역
    ERFNAM           LIKE RBKP-ERFNAM,                 "송장처리자
    ERFNAM_TX(20)    TYPE C,                           "송장처리자명
*    DESC_TEXT1       LIKE ZDTV3T_AP_HEAD-DESC_TEXT1,   "비고
*    MESG             LIKE ZDTV3T_AP_HEAD-DESC_TEXT1,   "메시지
*    NTS_SEND_FLAG    LIKE ZDTV3T_AP_HEAD-NTS_SEND_FLAG, "국세청 전송여부

    WAERS            LIKE RBKP-WAERS,                  "통화
*    INV_SEQ          LIKE ZDTV3T_AP_HEAD-INV_SEQ,      "세금계산서 일련번호
    AWKEY            LIKE BKPF-AWKEY,                  "참조번호
    STBLG            LIKE RBKP-STBLG,                  "역분개문서번호
    STJAH            LIKE RBKP-STJAH,                  "역분개문서 회계연도
    USE_DOC_ICON(4)  TYPE C,                           "전표매핑여부 아이콘표시
    BUKRS            LIKE RBKP-BUKRS,
    WSKTO            LIKE RBKP-WSKTO,                  "현금할인
    MWSKZ1           LIKE RBKP-MWSKZ1,                 "부가가치세 코드
    ZFBDT            LIKE RBKP-ZFBDT,                  "지급일
    NETDT            LIKE RBKP-ZFBDT,                  "만기일
    ZTERM            LIKE RBKP-ZTERM,                  "지급조건
    ZLSCH            LIKE RBKP-ZLSCH,                  "지급방법
    BVTYP            LIKE RBKP-BVTYP,                  "파트너은행유형
    HBKID            LIKE RBKP-HBKID,                  "거래은행단축키
    KURSF            LIKE RBKP-KURSF,                  "환율
    BEZNK            LIKE RBKP-BEZNK,                  "비계획운송비용
    ZETNO(35)        TYPE C,           "기타참조번호
    ZSTAT(2)            TYPE C,           "전자세금계산서 상태
    EKGRP            LIKE EKKO-EKGRP,                  "구매그룹
    ZLCDT            LIKE SY-DATUM,
    ZLCTM            LIKE SY-UZEIT,
    BKTXT            LIKE RBKP-BKTXT,                  "헤더텍스트 (사유 내역)
    VGART            LIKE RBKP-VGART,                  "송장,차후 차/대변, 대변메모 구분1.
    TBTKZ            LIKE RSEG-TBTKZ,                  "송장,차후 차/대변, 대변메모 구분2.
  END OF GS_HEADER,
  GT_HEADER LIKE TABLE OF GS_HEADER.



*-세금계산서 상세(하단 ALV)
DATA:
  BEGIN OF GS_ITEM,
    LIGHT(4)       TYPE C,             "상태
    CBOX(1)        TYPE C,             "선택
    EBELN          LIKE EKBE-EBELN,    "구매오더번호
    EBELP          LIKE EKBE-EBELP,    "구매오더품번
    MENGE          LIKE EKBE-MENGE,    "수량
    MENGE_OLD      LIKE EKBE-MENGE,    "수량
    MEINS          LIKE EKPO-MEINS,    "단위
    DMBTR          LIKE EKBE-DMBTR,    "변경전 입고금액(KRW)
    DMBTR_NEW      LIKE EKBE-REFWR,    "변경후 입고금액(KRW)
    WRBTR          LIKE EKBE-WRBTR,    "전표통화
    WRBTR_NEW      LIKE EKBE-REFWR,    "변경후 전표통화
    NETPR          LIKE EKPO-NETPR,    "단가
    WAERS          LIKE EKBE-WAERS,    "통화
    MATNR          LIKE EKBE-MATNR,    "자재번호
    MATNR_TX       LIKE MAKT-MAKTX,    "자재내역
    WERKS          LIKE RSEG-WERKS,    "플랜트
    WERKS_TX       LIKE T001W-NAME1,
    EKORG          LIKE EKKO-EKORG,    "구매조직
    BSART          LIKE EKKO-BSART,    "문서유형
    KNTTP          LIKE EKPO-KNTTP,    "계정지정범주
    PSTYP          LIKE EKPO-PSTYP,    "품목범주
    MWSKZ          LIKE EKPO-MWSKZ,    "세금코드
    ZTERM          LIKE EKKO-ZTERM,    "지급조건
    STUNR          LIKE EKBZ-STUNR,    "단계번호
    ZAEHK          LIKE EKBZ-ZAEHK,    "조건카운터
    KSCHL          LIKE EKBZ-KSCHL,    "조건유형
    BRTWR          LIKE EKPO-BRTWR,    "오더 총액(PO 통화)
    LEBRE          LIKE EKPO-LEBRE,    "서비스기준 송장검증 지시자
    SHKZG          LIKE EKBE-SHKZG,    "차/대지시자
    XBLNR          LIKE EKBE-XBLNR,    "참조전표번호
    LFGJA          LIKE EKBE-LFGJA,    "전표년도
    LFBNR          LIKE EKBE-LFBNR,    "참조전표번호
    LFPOS          LIKE EKBE-LFPOS,    "참조전표품목
    BELNR2         LIKE RBKP-BELNR,    "송장번호
    GJAHR2         LIKE RBKP-GJAHR,    "회계년도
    ZORDER_PERSON        LIKE EKKO-ZORDER_PERSON,  "발주자
    ZORDER_PERSON_TX(20) TYPE C,
    TXZ01          TYPE EKPO-TXZ01,    "내역
    BUDAT          TYPE EKBE-BUDAT,    "입고일
    CELLTAB        TYPE LVC_T_STYL,    "셀잠금
  END OF GS_ITEM,
  GT_ITEM LIKE TABLE OF GS_ITEM.

DATA GT_ITEM_OLD LIKE TABLE OF GS_ITEM.
DATA: GT_LOG LIKE TABLE OF BAPIRET2.

DATA:
  BEGIN OF GS_SES,
    LIFNR      LIKE EKKO-LIFNR,         "공급업체
    LIFNR_TX   LIKE LFA1-NAME1,         "공급업체명
    LBLNI      LIKE ESSR-LBLNI,         "서비스번호
    KTEXT1     LIKE ESLL-KTEXT1,        "서비스명
    BRTWR      LIKE ESLL-BRTWR,         "총금액
    WAERS      LIKE ESLH-WAERS,         "통화
    USERF1_NUM LIKE ESLL-USERF1_NUM,    "공정률
    SAKTO      LIKE ESKN-SAKTO,         "G/L 계정
    KOSTL      LIKE ESKN-KOSTL,         "코스트센터
    KOSTL_TX   LIKE CSKT-KTEXT,         "코스트센터명
    GROSS_VAL  LIKE BAPIESLL-GROSS_VAL, "총금액
    EXT_LINE   LIKE BAPIESLL-EXT_LINE,  "라인번호
  END OF GS_SES,
  GT_SES LIKE TABLE OF GS_SES.


DATA:
  BEGIN OF GS_MAKTX,
    MATNR LIKE MAKT-MATNR,
    MAKTX LIKE MAKT-MAKTX,
  END OF GS_MAKTX,
  GT_MAKTX LIKE TABLE OF GS_MAKTX.

TYPES: BEGIN OF TY_STATUS,
       KEY(2) TYPE C,
       TEXT(40) TYPE C,
       END OF TY_STATUS.

TYPES: BEGIN OF TY_TAXTYPE,
       KEY(2) TYPE C,
       TEXT(40) TYPE C,
       END OF TY_TAXTYPE.

TYPES: BEGIN OF TY_REASON,
       KEY(2) TYPE C,
       TEXT(40) TYPE C,
       END OF TY_REASON.

TYPES: BEGIN OF TY_PAPER_TAX,
       MWSKZ TYPE ZTMM00002-FIELD1,
       TEXT TYPE ZTMM00002-FIELD2,
       END OF TY_PAPER_TAX.

*DATA GT_USERNM TYPE TABLE OF ZSVMM_USER_INFO.
DATA GT_STATUS TYPE TABLE OF TY_STATUS.
DATA: GT_TAXTYPE TYPE TABLE OF TY_TAXTYPE.
DATA GT_REASON TYPE TABLE OF TY_REASON.
DATA GT_PAPER_TAX TYPE TABLE OF TY_PAPER_TAX.

DATA: GR_BSART TYPE RANGE OF EKKO-BSART,
      GR_BSTYP TYPE RANGE OF EKKO-BSTYP,
      GR_DPPCT TYPE RANGE OF EKKO-DPPCT.
*      GR_INV_SIGN TYPE RANGE OF ZDTV3T_AP_HEAD-INV_SIGN.

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
  GT_MWSKZ LIKE TABLE OF GS_MWSKZ.

DATA GT_MAPPING LIKE TABLE OF DSELC.
DATA GT_RETURN_TAB LIKE TABLE OF DDSHRETVAL.

DATA: BEGIN OF GS_HBKID,
        HBKID LIKE T012T-HBKID,
        HKTID LIKE T012T-HKTID,
        TEXT1 LIKE T012T-TEXT1,
      END OF GS_HBKID,
      GT_HBKID LIKE TABLE OF GS_HBKID,

      BEGIN OF GS_ZLSCH,  "지급방법
        ZLSCH TYPE T042ZT-ZLSCH,
        TEXT2 TYPE T042ZT-TEXT2,
      END OF GS_ZLSCH,
      GT_ZLSCH LIKE TABLE OF GS_ZLSCH.

DATA GS_T052 LIKE T052.


DATA GR_BLART TYPE RANGE OF BLART.

*DATA GR_WF_STATUS TYPE RANGE OF ZAPVAPPSTS.
DATA GR_BUPLA TYPE RANGE OF J_1BBRANC_.
DATA GR_WERKS TYPE RANGE OF WERKS_D.

DATA: GV_BUKRS_CODE(5) TYPE C,
      GR_BANKN TYPE RANGE OF ZCCMM_BANKACCNT-BANKACCOUNT.


DATA:
  BEGIN OF GS_SCR110,
    BUKRS          LIKE INVFO-BUKRS,      "회사코드
    LIFNR          LIKE INVFO-LIFNR,      "송장발행처(운송업체)
    BLDAT          LIKE INVFO-BLDAT,      "송장일자
    BUDAT          LIKE INVFO-BUDAT,      "전기일
    MWSKZ          LIKE INVFO-MWSKZ,      "세금코드
    ZTERM          LIKE INVFO-ZTERM,      "지급조건
    ZLSCH          LIKE INVFO-ZLSCH,      "지급방법
    ZFBDT          LIKE INVFO-ZFBDT,      "기산일
    NETDT          LIKE INVFO-NETDT,      "만기일
    ZBD1T          TYPE DZBD1T,           "현금할인기간
    BVTYP          LIKE INVFO-BVTYP,      "공급업체은행(파트너은행유형)
    DMBTR          LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "입고기준 공급가액
    WMWST          LIKE RM08M-DIFFERENZ, "INVFO-WMWST,  "입고기준 부가세액
    TOTAL          LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "입고기준 총액
    DMBTR2         LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "공급가
    WMWST2         LIKE RM08M-DIFFERENZ, "INVFO-WMWST,  "세액
    TOTAL2         LIKE RM08M-DIFFERENZ, "INVFO-DMBTR,  "총액
    TOTAL2_KRW(20) TYPE C,                "총액(KRW)
    DIFFERENZ      LIKE RM08M-DIFFERENZ,  "잔액
    XMWST          LIKE INVFO-XMWST,      "세금계산 체크박스
    WAERS          LIKE INVFO-WAERS,      "현지통화
    DATE           LIKE INVFO-WWERT,      "환율적용일자
    KURSF          TYPE INVFO-KURSF,      "환율
    WAERS2         LIKE INVFO-WAERS,      "전표통화
    HBKID          LIKE INVFO-HBKID,      "거래은행
    HKTID          LIKE INVFO-HKTID,      "계정 ID
    BUPLA          LIKE INVFO-BUPLA,      "사업장

    LABEL1(10)     TYPE C,                "공급가 라벨
    LABEL2(10)     TYPE C,                "세액 라벨
    LABEL3(10)     TYPE C,                "총액 라벨
    LABEL4(10)     TYPE C,                "차이 라벨

    LIFNR_TX      LIKE LFA1-NAME1,        "공급업체 명
    BELNR         LIKE RBKP-BELNR,
    GJAHR         LIKE RBKP-GJAHR,
*    BELNR2        LIKE ZDTV3T_AP_EXT_D-BELNR,       "회계전표번호
  END OF GS_SCR110.

* (+)U2 START
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
           BELNR LIKE EKBE_MA-BELNR,
           SHKZG LIKE EKBE_MA-SHKZG,
           MENGE LIKE EKBE_MA-MENGE,
           WRBTR LIKE EKBE_MA-WRBTR,
           LFGJA LIKE EKBE_MA-LFGJA,
           LFBNR LIKE EKBE_MA-LFBNR,
           LFPOS LIKE EKBE_MA-LFPOS,
         END OF TY_EKBE_MA.
  DATA GT_EKBE_MA TYPE TABLE OF TY_EKBE_MA.

* (+)U2 END
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
CLASS LCL_ALV_RECEIVER DEFINITION DEFERRED.
DATA GV_EVENT_RECEIVER TYPE REF TO LCL_ALV_RECEIVER.

DATA GV_GRID_T TYPE REF TO CL_GUI_ALV_GRID.
DATA GV_GRID_B TYPE REF TO CL_GUI_ALV_GRID.
DATA GV_DOCKING TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA GV_SPLITTER TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA GV_CONTAINER_T TYPE REF TO CL_GUI_CONTAINER.
DATA GV_CONTAINER_B TYPE REF TO CL_GUI_CONTAINER.


DATA OK_CODE TYPE SYUCOMM.
DATA OK_CODE_POPUP TYPE SYUCOMM.
DATA GV_OK_CODE TYPE SYUCOMM.
DATA GV_REPID TYPE SYREPID.
DATA GV_DYNNR TYPE SYDYNNR.
DATA GV_REASON TYPE DD07D-DOMVALUE.
DATA GV_FLAG(1) TYPE C.
DATA GV_EXC_USER(5) TYPE C.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS:
  GC_I(1) TYPE C VALUE 'I',
  GC_01(2)        TYPE C              VALUE '01', "기재사항의 착오/정정
  GC_02(2)        TYPE C              VALUE '02', "공급가액 변동
  GC_03(2)        TYPE C              VALUE '03', "환입
  GC_04(2)        TYPE C              VALUE '04', "계약의 해제
  GC_05(2)        TYPE C              VALUE '05',
  GC_06(2)        TYPE C              VALUE '06', "착오에 의한 이중발급

  GC_10(2)        TYPE C              VALUE '10',
  GC_20(2)        TYPE C              VALUE '20',
  GC_30(2)        TYPE C              VALUE '30',
  GC_18(2)        TYPE C              VALUE '18',

  GC_SYM1(1)      TYPE C              VALUE '☞',
  GC_SYM2(1)      TYPE C              VALUE '*',
  GC_SYM3(1)      TYPE C              VALUE '(',
  GC_SYM4(1)      TYPE C              VALUE ')',

  GC_XX(2)        TYPE C              VALUE 'XX',
  GC_EQ(2)        TYPE C              VALUE 'EQ',
  GC_NE(2)        TYPE C              VALUE 'NE',
  GC_BT(2)        TYPE C              VALUE 'BT',

  GC_KR(2)        TYPE C              VALUE 'KR',
  GC_EA(2)        TYPE C              VALUE 'EA',
  GC_RA(2)        TYPE C              VALUE 'RA',
  GC_RE(2)        TYPE C              VALUE 'RE',
  GC_RL(2)        TYPE C              VALUE 'RL',
  GC_AC(2)        TYPE C              VALUE 'AC',
  GC_AP(2)        TYPE C              VALUE 'AP',
  GC_IV(2)        TYPE C              VALUE 'IV',
  GC_NG(2)        TYPE C              VALUE 'NG',
  GC_LE(2)        TYPE C              VALUE 'LE',
  GC_LP(2)        TYPE C              VALUE 'LP',
  GC_MP(2)        TYPE C              VALUE 'MP',

  GC_KRW          TYPE TCURC-WAERS    VALUE 'KRW',

  GC_REQU(4)      TYPE C              VALUE 'REQU',
  GC_0001(4)      TYPE C              VALUE '0001',
  GC_RBKP(4)      TYPE C              VALUE 'RBKP',

  GC_C601(4)      TYPE C              VALUE 'C601',
  GC_C111(4)      TYPE C              VALUE 'C111',

  GC_CBOX(5)      TYPE C       VALUE 'CBOX',
  GC_BLDAT(5)     TYPE C       VALUE 'BLDAT',
  GC_BKTXT(5)     TYPE C       VALUE 'BKTXT',
  GC_MENGE(5)     TYPE C       VALUE 'MENGE',
  GC_DMBTR(5)     TYPE C       VALUE 'DMBTR',
  GC_WRBTR(5)     TYPE C       VALUE 'WRBTR',
  GC_DMBTR_NEW(9) TYPE C       VALUE 'DMBTR_NEW',
  GC_WRBTR_NEW(9) TYPE C       VALUE 'WRBTR_NEW',

  GC_F0001(5)     TYPE C                VALUE 'F0001',
  GC_F0002(5)     TYPE C                VALUE 'F0002',
  GC_BUPLA        TYPE DFIES-FIELDNAME  VALUE 'BUPLA',
  GC_MWSKZ        TYPE DFIES-FIELDNAME  VALUE 'MWSKZ',
  GC_ZLSCH        TYPE DFIES-FIELDNAME  VALUE 'ZLSCH',
  GC_HBKID(5)     TYPE C                VALUE 'HBKID',
  GC_HBKID2(15)   TYPE C                VALUE 'GS_SCR100-HBKID',
  GC_HKTID2(15)   TYPE C                VALUE 'GS_SCR100-HKTID',

  GC_CELLTAB      TYPE LVC_S_LAYO-STYLEFNAME VALUE 'CELLTAB',

  GC_STRUC_LOG    LIKE DD02L-TABNAME  VALUE 'BAPIRET2',
  GC_STRUC_T      LIKE DD02L-TABNAME  VALUE 'GS_HEADER',
  GC_STRUC_B      LIKE DD02L-TABNAME  VALUE 'GS_ITEM',
  GC_EXCLUD(15)   TYPE C              VALUE 'GT_UI_FUNC[]',
  GC_ZDTV3D_TYPE  TYPE DD01L-DOMNAME  VALUE 'ZDTV3D_TYPE_CODE',
  GC_ZDTV3D_APPEND TYPE DD01L-DOMNAME  VALUE 'ZDTV3D_AMEND_CODE',
  GC_TEMP_CREATE TYPE SY-UCOMM VALUE 'TEMP_CREATE'.

CONSTANTS: GC_BUKRS_1101 TYPE T001-BUKRS VALUE '1101',
           GC_BUKRS_2101 TYPE T001-BUKRS VALUE '2101',
           GC_BUKRS_3101 TYPE T001-BUKRS VALUE '3101',
           GC_CODE_KTG(3) TYPE C VALUE 'KTG',
           GC_CODE_KGC(3) TYPE C VALUE 'KGC',
           GC_CODE_YJP(3) TYPE C VALUE 'YJP'.

*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
DEFINE _G_M_APPEND.

  APPEND VALUE #( SIGN = &2 OPTION = &3 LOW = &4 HIGH = &5 ) TO &1.

END-OF-DEFINITION.
DEFINE _G_M_APPEND2.

  APPEND VALUE #( SIGN = &2 OPTION = &3 LOW = &4 ) TO &1.

END-OF-DEFINITION.
