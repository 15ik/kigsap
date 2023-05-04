*&---------------------------------------------------------------------*
*& Include          ZRMM4090TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : T001, LFA1, CDHDR, LIKP, EKKO.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_IF_ID_0069          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0069', "KTNG WMS
           GC_IF_ID_0070          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0070', "KGC WMS 부여
           GC_IF_ID_0100          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0100', "KGC WMS 원주
           GC_IF_ID_0071          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0071', "KGC 3PL
           GC_IF_ID_0072          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0072', "YJP WMS
           GC_IF_ID_0073          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0073', "KTNG WMS
           GC_IF_ID_0074          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0074', "KTNG WMS
           GC_IF_ID_0075          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0075', "KGC WMS 부여
           GC_IF_ID_0076          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0076', "KGC WMS 원주
           GC_IF_ID_0077          TYPE ZSCN_IF_HEADER-IF_ID VALUE 'MM-0077', "YJP WMS

           GC_INFO_KTNG           TYPE ZSCN_IF_HEADER-ADDITIONAL_INFO VALUE 'KTNG WMS',
           GC_INFO_KGC_2001       TYPE ZSCN_IF_HEADER-ADDITIONAL_INFO VALUE 'KGC WMS-부여',
           GC_INFO_KGC_2002       TYPE ZSCN_IF_HEADER-ADDITIONAL_INFO VALUE 'KGC WMS-원주',
           GC_INFO_KGC_2050       TYPE ZSCN_IF_HEADER-ADDITIONAL_INFO VALUE 'KGC 3PL',
           GC_INFO_YJP            TYPE ZSCN_IF_HEADER-ADDITIONAL_INFO VALUE 'YJP WMS',

           GC_CD_9999             TYPE ZE_RST_CD VALUE '9999',
           GC_CD_0000             TYPE ZE_RST_CD VALUE '0000',
           GC_00000000            TYPE ZTMM40300-ERDAT VALUE '00000000',

           GC_WERKS_2001          TYPE ZTMM40300-WERKS VALUE '2001',
           GC_WERKS_2002          TYPE ZTMM40300-WERKS VALUE '2002',
           GC_WERKS_2050          TYPE ZTMM40300-WERKS VALUE '2050',

           GC_1101                TYPE EKKO-BUKRS VALUE '1101',
           GC_2101                TYPE EKKO-BUKRS VALUE '2101',
           GC_3101                TYPE EKKO-BUKRS VALUE '3101',

           GC_0001                TYPE EKET-ETENR VALUE '0001',
           GC_0004                TYPE EKPO-BSTAE VALUE '0004',
           GC_MSG_INB_FAIL        TYPE ZTMM40300-ZMESSAGE VALUE 'INBOUND DELIVERY 정보 전송 실패',
           GC_MSG_INB_SUCCESS     TYPE ZTMM40300-ZMESSAGE VALUE 'INBOUND DELIVERY 정보 전송 성공',
           GC_MSG_PO_FAIL(200)    TYPE C VALUE '구매오더 정보 전송 실패',
           GC_MSG_PO_SUCCESS(200) TYPE C VALUE '구매오더 정보 전송 성공'.


*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
*-- Inbound Delivery
TYPES: BEGIN OF TY_IDHEADER,
*-- I/D HEADER
         VBELN     TYPE ZTMM40300-VBELN,      "납품문서
         BUKRS     TYPE ZTMM40300-BUKRS,      "회사코드
         BUTXT     TYPE T001-BUTXT,           "회사명
         LFDAT     TYPE ZTMM40300-LFDAT,      "납품요청일
         LIFNR     TYPE ZTMM40300-LIFNR,      "업체코드
         NAME1     TYPE LFA1-NAME1,           "업체명
         SUBLIFNR  TYPE ZTMM40300-SUBLIFNR,   "거점
         SUBNAME   TYPE LFA1-NAME1,           "거점명
         ZTRANS    TYPE ZE_TRANSCHECK,        "배차대상여부
         ZTRFEE    TYPE ZE_TRFEECHECK,        "운송비정산대상
         VSBED     TYPE ZTMM40300-VSBED,      "출하조건
         WERKS     TYPE ZTMM40300-WERKS,      "플랜트
         NAME2     TYPE T001W-NAME1,          "플랜트명
         LGORT     TYPE ZTMM40300-LGORT,      "저장위치
         LGOBE1    TYPE T001L-LGOBE,          "저장위치명
         ZWERKS    TYPE ZTMM40300-ZWERKS,     "최종납품플랜트
         NAME3     TYPE T001W-NAME1,          "최종납품플랜트명
         ZLGORT    TYPE ZTMM40300-ZLGORT,     "최종납품저장위치
         LGOBE2    TYPE T001L-LGOBE,          "최종납품저장위치명
         ZSTATUS   TYPE ZTMM40300-ZSTATUS,    "상태
         ZMESSAGE  TYPE ZTMM40300-ZMESSAGE,   "메시지
         ZCONFIRM  TYPE ZTMM40300-ZCONFIRM,   "확정여부
         ZTRMEMO   TYPE ZE_TRMEMO,            "운송비고
         ZGRMEMO   TYPE ZE_GRMEMO,            "입고메모
         LOEKZ     TYPE ZTMM40300-LOEKZ,      "삭제
         ZWMSIFID  TYPE ZTMM40300-ZWMSIFID,   "전송I/F
         ZWMSTRCID TYPE ZTMM40300-ZWMSTRCID,  "전송 TRC ID
         ZFLAG(1)  TYPE C,                    "IUD
         ZTFLAG    TYPE ZTMM40300-ZTFLAG, "전송 지시자
         ERDAT     TYPE ZTMM40300-ERDAT,      "생성일
         ERZET     TYPE ZTMM40300-ERZET,      "생성시간
         ERNAM     TYPE ZTMM40300-ERNAM,      "생성자
         AEDAT     TYPE ZTMM40300-AEDAT,      "변경일
         AEZET     TYPE ZTMM40300-AEZET,      "변경시간
         AENAM     TYPE ZTMM40300-AENAM,      "변경자
       END OF TY_IDHEADER.

"*- Alv Dispaly
TYPES: BEGIN OF TS_IDHEADER.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_IDHEADER.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_IDHEADER.

*-
TYPES: BEGIN OF TY_IDITEM,
*-- I/D ITEM
         VBELN       TYPE ZTMM40301-VBELN,      "납품문서
         POSNR       TYPE ZTMM40301-POSNR,      "항번
         MATNR       TYPE ZTMM40301-MATNR,      "자재코드
         MAKTX       TYPE MAKT-MAKTX,           "자재내역
         WERKS       TYPE ZTMM40301-WERKS,      "플랜트
         NAME1       TYPE T001W-NAME1,          "플랜트명
         LGORT       TYPE ZTMM40301-LGORT,      "저장위치
         LGOBE       TYPE T001L-LGOBE,          "저장위치명
         LFIMG       TYPE ZTMM40301-LFIMG,      "수량
         VRKME       TYPE ZTMM40301-VRKME,      "단위
         VGBEL       TYPE ZTMM40301-VGBEL,      "PO 번호
         VGPOS       TYPE ZTMM40301-VGPOS,      "PO 항번
         PSTYP       TYPE ZTMM40301-PSTYP,      "ItemCategory
         CHARG       TYPE ZTMM40301-CHARG,      "배치
         BWTAR       TYPE ZTMM40301-BWTAR,      "평가유형
         HSDAT       TYPE ZTMM40301-HSDAT,      "제조일
         LICHN       TYPE ZTMM40301-LICHN,      "제조처 LOT
         VFDAT       TYPE ZTMM40301-VFDAT,      "유효기간
         ZMAKER      TYPE ZTMM40301-ZMAKER,     "MAKER
         ZBRESV1     TYPE ZTMM40301-ZBRESV1,    "예비 1
         ZBRESV2     TYPE ZTMM40301-ZBRESV2,    "예비 2
         ZBRESV3     TYPE ZTMM40301-ZBRESV3,    "예비 3
         ZBRESV4     TYPE ZTMM40301-ZBRESV4,    "예비 4
         ZBRESV5     TYPE ZTMM40301-ZBRESV5,    "예비 5
         ZBRESV6     TYPE ZTMM40301-ZBRESV6,    "예비 6
         ZBRESV7     TYPE ZTMM40301-ZBRESV7,    "예비 7
         ZBRESV8     TYPE ZTMM40301-ZBRESV8,    "예비 8
         ZBRESV9     TYPE ZTMM40301-ZBRESV9,    "예비 9
         ZBRESV10    TYPE ZTMM40301-ZBRESV10,   "예비 10
         ZPALQTY     TYPE ZTMM40301-ZPALQTY,    "파렛트 수량
         ZINSPECTNO  TYPE ZTMM40301-ZINSPECTNO, "검사의뢰번호
         ZDOCPATH1   TYPE ZTMM40301-ZDOCPATH1,  "문서 1
         ZDOCPATH2   TYPE ZTMM40301-ZDOCPATH2,  "문서 2
         ZDOCPATH3   TYPE ZTMM40301-ZDOCPATH3,  "문서 3
         LOEKZ       TYPE ZTMM40301-LOEKZ,      "삭제지시자
         ZTFLAG      TYPE ZE_INSTAT4,           "전송상태
         ZFLAG(1)    TYPE C,                    "IUD
         ERDAT       TYPE ZTMM40301-ERDAT,      "생성일
         ERZET       TYPE ZTMM40301-ERZET,      "생성시간
         ERNAM       TYPE ZTMM40301-ERNAM,      "생성자
         AEDAT       TYPE ZTMM40301-AEDAT,      "변경일
         AEZET       TYPE ZTMM40301-AEZET,      "변경시간
         AENAM       TYPE ZTMM40301-AENAM,      "변경자
         ZINSPECTION TYPE ZTMM40301-ZINSPECTION,"사전검사대상
         ZINSPRESULT TYPE ZTMM40301-ZINSPRESULT,"사전검사완료여부
       END OF TY_IDITEM.

"*- Alv Dispaly
TYPES: BEGIN OF TS_IDITEM.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_IDITEM.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_IDITEM.

*--

*-- PO
TYPES: BEGIN OF TY_POHEADER,
*-- PO HEADER
         EBELN      TYPE EKKO-EBELN,                "구매오더번호
         BUKRS      TYPE EKKO-BUKRS,                "회사코드
         BUTXT      TYPE T001-BUTXT,                "회사명
         BSART      TYPE EKKO-BSART,                "구매오더유형
         ZAEDAT     TYPE EKKO-AEDAT,                "생성일
         UDATE      TYPE ZSVCMMPOTRHIST-UDATE,      "변경일
         UTIME      TYPE ZSVCMMPOTRHIST-UTIME,      "변경시간
         LIFNR      TYPE EKKO-LIFNR,                "업체코드
         NAME1      TYPE LFA1-NAME1,                "업체명
         ZEBELN(10) TYPE C,                         "대체 PO 번호
         LOEKZ      TYPE EKPO-LOEKZ,                "삭제지시자
         ZFLAG(1)   TYPE C,                         "IUD
         ZSTATUS(1) TYPE C,                         "상태
         ZMSG(200)  TYPE C,                         "메시지
       END OF TY_POHEADER.

"*- Alv Dispaly
TYPES: BEGIN OF TS_POHEADER.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_POHEADER.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_POHEADER.


TYPES: BEGIN OF TY_POITEM,
*-- PO ITEM
         EBELN     TYPE EKPO-EBELN,                "구매오더번호
         EBELP     TYPE EKPO-EBELP,                "항번
         MATNR     TYPE EKPO-MATNR,                "자재코드
         TXZ01     TYPE EKPO-TXZ01,                "자재명
         WERKS     TYPE EKPO-WERKS,                "플랜트
         NAME1     TYPE T001W-NAME1,               "플랜트명
         LGORT     TYPE EKPO-LGORT,                "저장위치
         LGOBE     TYPE T001L-LGOBE,               "저장위치명
         LOEKZ     TYPE EKPO-LOEKZ,                "삭제필드
         CHARG     TYPE EKET-CHARG,                "배치
         BWTAR     TYPE EKPO-BWTAR,                "평가유형
         RETPO     TYPE EKPO-RETPO,                "반품
         EINDT     TYPE EKET-EINDT,                "납품예정일
         MENGE     TYPE EKPO-MENGE,                "수량
         PO_MENGE     TYPE EKPO-MENGE,
         MEINS     TYPE EKPO-MEINS,                "단위
         ELIKZ     TYPE EKPO-ELIKZ,                "납품완료
         INSMK    TYPE EKPO-INSMK,            "재고상태
         ZEBELN(1) TYPE C,                         "대체 PO번호
         ZEBELP(5) TYPE N,                         "대체 PO번호 항번
         ZFLAG(1)  TYPE C,                         "IUD
       END OF TY_POITEM.

"*- Alv Dispaly
TYPES: BEGIN OF TS_POITEM.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_POITEM.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES:   CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_POITEM.

TYPES : TT_IDHEADER TYPE TABLE OF TS_IDHEADER,
        TT_IDITEM   TYPE TABLE OF TS_IDITEM,
        TT_POHEADER TYPE TABLE OF TS_POHEADER,
        TT_POITEM   TYPE TABLE OF TS_POITEM.
*--
*----------------------------------------------------------------------*
* ALV Object
*----------------------------------------------------------------------*
DATA : GRF_DOCKING_CON     TYPE REF TO CL_GUI_DOCKING_CONTAINER.


DATA : GRF_HEAD      TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY      TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY_ITEM TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID,       " ALV Gridv
       GRF_ITEM TYPE REF TO LCL_CUST_ALV_GRID.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA : GT_IDHEADER TYPE STANDARD TABLE OF TS_IDHEADER,
       GT_IDITEM   TYPE STANDARD TABLE OF TS_IDITEM,
       GT_DISP_ID  TYPE STANDARD TABLE OF TS_IDITEM.

DATA : GT_DATA     TYPE STANDARD TABLE OF TS_POHEADER,
       GT_POHEADER TYPE STANDARD TABLE OF TS_POHEADER,
       GT_POITEM   TYPE STANDARD TABLE OF TS_POITEM,
       GT_DISP_PO  TYPE STANDARD TABLE OF TS_POITEM.

DATA : GT_ZTMM40304 TYPE TABLE OF ZTMM40304,
       GT_ZTMM40305 TYPE TABLE OF ZTMM40305.

DATA : GS_OUT_HEADER TYPE ZSCN_IF_HEADER,
       GS_IN_HEADER  TYPE ZSCN_IF_HEADER.

DATA : GV_TCNT TYPE SY-TABIX.

DATA : OK_CODE    TYPE SY-UCOMM,
       GV_MODE    TYPE C VALUE '',                          "Display :D  Edit:E (화면편집)
       GV_OK_CODE TYPE SY-UCOMM.

DATA : GT_FCAT TYPE LVC_T_FCAT.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_INIT.

  CLEAR:&1, &1[].

END-OF-DEFINITION .

DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .

**> 값 단위 별 변환
DEFINE _G_CONV_DATA_SAP_TO_EAI.

  ZCL_MM_COMMON=>CONV_DATA_SAP_TO_EAI( EXPORTING IV_VALUE  = &1
                                                 IV_UNIT = &2
                                       IMPORTING EV_VALUE   = &3 ).

END-OF-DEFINITION.
**> 구조체 별 변환
DEFINE _G_CONV_STRC_SAP_TO_EAI.

  ZCL_MM_COMMON=>CONV_STRUCTURE_SAP_TO_EAI( EXPORTING IS_STRUCTURE  = &1
                                IMPORTING ES_STRUCTURE   = &2 ).

END-OF-DEFINITION.

DEFINE _G_EAI_LOG.

  "IF Header Log처리
  ZCL_CN_ABAP_UTIL=>SET_EAI_END( IS_IF_HEADER = ES_HEADER ).

END-OF-DEFINITION.
