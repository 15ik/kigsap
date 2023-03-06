*&---------------------------------------------------------------------*
*& Include          ZRMM4430TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : T001, MARA, MCH1, KNA1, AUSP.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA,
         MVTTYPE(20)  TYPE C,                             "구분그룹     "U2 - 영문화를 위한 필드 길이 조정
         BUKRS        TYPE ZSVBMMMATDOCB-BUKRS,           "회사코드
         BUTXT        TYPE ZSVBMMMATDOCB-BUTXT,           "회사명
         BUDAT        TYPE ZSVBMMMATDOCB-BUDAT,           "전기일
         CPUDT        TYPE ZSVBMMMATDOCB-CPUDT,           "입력일
         CPUTM        TYPE ZSVBMMMATDOCB-CPUTM,           "입력시간
         ZNUM(3)      TYPE N,                             "Seq
         MATNR        TYPE ZSVBMMMATDOCB-MATNR,           "자재코드
         MAKTX        TYPE ZSVBMMMATDOCB-MAKTX,           "자재명
         CHARG        TYPE ZSVBMMMATDOCB-CHARG,           "배치
         BWART        TYPE ZSVBMMMATDOCB-BWART,           "이동유형
         BTEXT        TYPE ZSVBMMMATDOCB-BTEXT,           "이동유형명
         SOBKZ        TYPE ZSVBMMMATDOCB-SOBKZ,           "특별재고지시자
         MENGE        TYPE ZSVBMMMATDOCB-MENGE,           "수량
         ERFMG        TYPE ZSVBMMMATDOCB-ERFMG,           "이동수량
         ERFME        TYPE ZSVBMMMATDOCB-ERFME,           "입력단위
         MEINS        TYPE ZSVBMMMATDOCB-MEINS,           "기본단위
         WERKS        TYPE ZSVBMMMATDOCB-WERKS,           "플랜트
         WERKSNAME    TYPE ZSVBMMMATDOCB-WERKSNAME,       "플랜트명
         LGORT        TYPE ZSVBMMMATDOCB-LGORT,           "저장위치
         LGORTNAME    TYPE ZSVBMMMATDOCB-LGORTNAME,       "저장위치명
         UMWRK        TYPE ZSVBMMMATDOCB-UMWRK,           "관전플랜트
         UMWRKNAME    TYPE ZSVBMMMATDOCB-UMWRKNAME,       "관전플랜트명
         UMLGO        TYPE ZSVBMMMATDOCB-UMLGO,           "관전저장위치
         UMLGONAME    TYPE ZSVBMMMATDOCB-UMLGONAME,       "관전저장위치명
         MJAHR        TYPE ZSVBMMMATDOCB-MJAHR,           "자재연도
         MBLNR        TYPE ZSVBMMMATDOCB-MBLNR,           "자재문서
         ZEILE        TYPE ZSVBMMMATDOCB-ZEILE,           "자재문서항번
         BWTAR        TYPE ZSVBMMMATDOCB-BWTAR,           "평가유형
         LIFNR        TYPE ZSVBMMMATDOCB-LIFNR,           "공급업체
         LIFNRNAME    TYPE ZSVBMMMATDOCB-LIFNRNAME,       "업체명
         KUNNR        TYPE ZSVBMMMATDOCB-KUNNR,           "고객
         KUNNRNAME    TYPE ZSVBMMMATDOCB-KUNNRNAME,       "고객명
         HSDAT        TYPE ZSVBMMMATDOCB-HSDAT,           "제조일
         VFDAT        TYPE ZSVBMMMATDOCB-VFDAT,           "사용기한
         LICHN        TYPE ZSVBMMMATDOCB-LICHN,           "제조처LOT
         ZMAKER       TYPE ZSVBMMMATDOCB-ZMAKER,          "메이커
         WATERRATIO   TYPE ZSVBMMMATDOCB-WATERRATIO,      "수분함량
         EBELN        TYPE ZSVBMMMATDOCB-EBELN,           "구매오더
         EBELP        TYPE ZSVBMMMATDOCB-EBELP,           "항번
         AUFNR        TYPE ZSVBMMMATDOCB-AUFNR,           "생산오더
         AUFPS        TYPE ZSVBMMMATDOCB-AUFPS,           "오더품목번호
         VBELN_IM     TYPE ZSVBMMMATDOCB-VBELN_IM,        "납품서
         VBELP_IM     TYPE ZSVBMMMATDOCB-VBELP_IM,        "항번
         USNAM        TYPE ZSVBMMMATDOCB-USNAM,           "사용자
         LBBSA_SID    TYPE ZSVBMMMATDOCB-LBBSA_SID,       "자재이동재고유형(재고ID)
         KZBEW        TYPE ZSVBMMMATDOCB-KZBEW,           "이동지시자
         BSTAUS_SG    TYPE ZSVBMMMATDOCB-BSTAUS_SG,       "재고특징
         BSTTYP_SG    TYPE ZSVBMMMATDOCB-BSTTYP_SG,       "재고범주
         BERID        TYPE ZSVBMMMATDOCB-BERID,           "MRP 영역
         WAERS        TYPE ZSVBMMMATDOCB-WAERS,           "통화
         DMBTR        TYPE ZSVBMMMATDOCB-DMBTR,           "금액(현지 통화)
         RSNUM        TYPE ZSVBMMMATDOCB-RSNUM,           "예약/종속 소요량의 번호
         RSPOS        TYPE ZSVBMMMATDOCB-RSPOS,           "예약/종속 소요량에 대한 품목 번호
         SHKZG        TYPE ZSVBMMMATDOCB-SHKZG,           "차대구분
         ELIKZ        TYPE ZSVBMMMATDOCB-ELIKZ,           "납품완료지시자
         KOKRS        TYPE ZSVBMMMATDOCB-KOKRS,           "관리회계 영역
         KZVBR        TYPE ZSVBMMMATDOCB-KZVBR,           "소비 전기
         PRCTR        TYPE ZSVBMMMATDOCB-PRCTR,           "손익센터
         AUFPL        TYPE ZSVBMMMATDOCB-AUFPL,           "오더에 있는 작업 라우팅 번호
         APLZL        TYPE ZSVBMMMATDOCB-APLZL,           "내부카운터
         SAKTO        TYPE ZSVBMMMATDOCB-SAKTO,           "G/L계정번호
         TCODE2       TYPE ZSVBMMMATDOCB-TCODE2,          "TCODE
       END OF TY_DATA.
" MANDT, CANCELLED, REVERSAL_MOVEMENT, KZZUG 사용 X


TYPES : BEGIN OF TY_MVTTYPE,
         MVT_KO(30) TYPE C,
         MVT_EN(30) TYPE C,
       END OF TY_MVTTYPE.



"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
         NEW,                    "신규데이타
       END OF  TS_DISP.

TYPES : TT_ITEM TYPE TABLE OF TS_DISP.
*----------------------------------------------------------------------*
* ALV Object
*----------------------------------------------------------------------*
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.


DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY      TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY_ITEM TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID, " ALV Gridv
       GRF_ITEM TYPE REF TO LCL_CUST_ALV_GRID.


*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA : GT_DATA TYPE TABLE OF TY_DATA,
       GT_DISP      TYPE TABLE OF TS_DISP,
       GT_DISP_ITEM TYPE TABLE OF TS_DISP,
       GT_MVTTYPE   TYPE TABLE OF TY_MVTTYPE.

DATA : OK_CODE TYPE SY-UCOMM,
       GV_MODE    TYPE C VALUE '',                          "Display :D  Edit:E (화면편집)
       GV_OK_CODE TYPE SY-UCOMM.

DATA : GV_TCNT TYPE SY-TABIX.

DATA : GS_VARIANT TYPE DISVARIANT. " Variant

DATA: GV_LANGU TYPE SY-LANGU.

CONSTANTS : GC_C110(10) VALUE 'C110',
            GC_C200(10) VALUE 'C200',
            GC_C201(10) VALUE 'C201',
            GC_C210(10) VALUE 'C210',
            GC_C300(10) VALUE 'C300',
            GC_C310(10) VALUE 'C310',
            GC_C400(10) VALUE 'C400',
            GC_C500(10) VALUE 'C500',
            GC_C510(10) VALUE 'C510',
            GC_C600(10) VALUE 'C600',
            GC_C610(10) VALUE 'C610',
            GC_C700(10) VALUE 'C700',
            GC_C710(10) VALUE 'C710',
            GC_101(3) VALUE '101',
            GC_102(3) VALUE '102',
            GC_161(3) VALUE '161',
            GC_162(3) VALUE '162',
            GC_261(3) VALUE '261',
            GC_262(3) VALUE '262',
            GC_541(3) VALUE '541',
            GC_542(3) VALUE '542',
            GC_543(3) VALUE '543',
            GC_544(3) VALUE '544'.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_INIT.

  CLEAR:&1, &1[].

END-OF-DEFINITION .

DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .
