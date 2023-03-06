*&---------------------------------------------------------------------*
*& Include          ZOMM9910TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES : SSCRFIELDS, ZTMM99120, ZTMM99130, ZTMM99150.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_FC01(4) TYPE C VALUE 'FC01',
           GC_RA(2)          TYPE C VALUE 'RA',

           GC_MARA           TYPE STRING VALUE 'MARA',
           GC_MCH1           TYPE STRING VALUE 'MCH1',
           GC_NUMC           TYPE STRING VALUE 'NUMC',

           GC_3001           TYPE ZTMM99120-WERKS VALUE '3001',
           GC_3101           TYPE ZTMM99120-BUKRS VALUE '3101',
           GC_2101           TYPE ZTMM99120-BUKRS VALUE '2101',

           GC_0001           TYPE ZTMM99120-ZEILE VALUE '0001',
           GC_00000000       TYPE ZTMM99110-ZSEQ VALUE '0000000000',

           GC_KGCBATCH1      TYPE ZTMM99110-CLASS VALUE 'ZMM_KGCBATCH1',
           GC_KGCBATCH2      TYPE ZTMM99110-CLASS VALUE 'ZMM_KGCBATCH2',
           GC_KGCBATCH3      TYPE ZTMM99110-CLASS VALUE 'ZMM_KGCBATCH3',
           GC_KGCBATCH4      TYPE ZTMM99110-CLASS VALUE 'ZMM_KGCBATCH4',
           GC_ZPP_KGC1       TYPE ZTMM99110-CLASS VALUE 'ZPP_KGC1',
           GC_ZPP_YJP        TYPE ZTMM99110-CLASS VALUE 'ZPP_YJP',
           GC_YJPBATCH1      TYPE ZTMM99110-CLASS VALUE 'ZMM_YJPBATCH1',
           GC_YJPBATCH2      TYPE ZTMM99110-CLASS VALUE 'ZMM_YJPBATCH2',
           GC_YJPBATCH3      TYPE ZTMM99110-CLASS VALUE 'ZMM_YJPBATCH3',
           GC_YJPBATCH4      TYPE ZTMM99110-CLASS VALUE 'ZMM_YJPBATCH4',

           GC_ZMM_MAKER      TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZMM_MAKER',
           GC_ZCOMP_RATIO    TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_COMP_RATIO',
           GC_ZCOMP_MOIST    TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_COMP_MOIST',
           GC_ZCOMP_NET      TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_COMP_NET',
           GC_ZCOMP_DRY      TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_COMP_DRY',
           GC_CUST_LOTNO     TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_CUST_LOTNO',
           GC_ZWATER_CONTENT TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZWATER_CONTENT',
           GC_ZPROD_DATE     TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPROD_DATE',
           GC_ZEXPIRY_DATE   TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZEXPIRY_DATE',
           GC_ZEXPIRY_DATE2  TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZPPYJ_EXPIRYDATE',
           GC_ZFMFRPN        TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZFMFRPN'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*

*-- 배치 Excel Upload
TYPES: BEGIN OF TS_UPD_A,
         MATNR       TYPE ZTMM99110-MATNR,        "자재코드
         LIFNR       TYPE ZTMM99110-LIFNR,        "공급업체
         HSDAT       TYPE ZTMM99110-HSDAT,        "제조일자
         VFDAT       TYPE ZTMM99110-VFDAT,        "사용기간
         LICHA       TYPE ZTMM99110-LICHA,        "제조 LOT
         ZMM_MAKER   TYPE ZTMM99110-ZMM_MAKER,    "제조처
         ZCOMP_RATIO TYPE ZTMM99110-ZCOMP_RATIO,  "역가계산비율
         ZCOMP_MOIST TYPE ZTMM99110-ZCOMP_MOIST,  "수분비율
         ZCOMP_NET   TYPE ZTMM99110-ZCOMP_NET,    "함량
         ZCOMP_DRY   TYPE ZTMM99110-ZCOMP_DRY,    "건조중량
         ZLOTNO2     TYPE ZTMM99110-ZLOTNO2,      "수탁 LOTNO
         EX_CHARG    TYPE ZTMM99110-CHARG,        "외부배치번호
         CHARG       TYPE ZTMM99110-CHARG,        "배치번호
         ZMESSAGE    TYPE ZTMM99110-ZMESSAGE.     "메시지

TYPES: END OF TS_UPD_A.

*-- 창고 Excel Upload
TYPES: BEGIN OF TS_UPD_B,
         WERKS  TYPE ZTMM99120-WERKS,        "플랜트
         LGORT  TYPE ZTMM99120-LGORT,        "저장위치
         MATNR  TYPE ZTMM99120-MATNR,        "자재코드
         CHARG  TYPE ZTMM99120-CHARG,        "배치
         BWTAR  TYPE ZTMM99120-BWTAR,        "평가유형
         ERFMG  TYPE ZTMM99120-ERFMG,        "재고수량
         ZMEINS TYPE ZTMM99120-ZMEINS,       "재고단위
         INSMK  TYPE ZTMM99120-INSMK,        "재고유형
         EXBWR  TYPE ZTMM99120-EXBWR.        "평가금액
TYPES: END OF TS_UPD_B.

*-- 사급 Excel Upload
TYPES: BEGIN OF TS_UPD_C,
         WERKS  TYPE ZTMM99120-WERKS,        "플랜트
         LIFNR  TYPE ZTMM99130-LIFNR,        "업체코드
         MATNR  TYPE ZTMM99120-MATNR,        "자재코드
         CHARG  TYPE ZTMM99120-CHARG,        "배치
         BWTAR  TYPE ZTMM99120-BWTAR,        "평가유형
         ERFMG  TYPE ZTMM99120-ERFMG,        "재고수량
         ZMEINS TYPE ZTMM99120-ZMEINS,       "재고단위
         INSMK  TYPE ZTMM99120-INSMK,        "재고유형
         EXBWR  TYPE ZTMM99120-EXBWR.        "평가금액
TYPES: END OF TS_UPD_C.

*-- 위탁 Excel Upload
TYPES: BEGIN OF TS_UPD_D,
         WERKS  TYPE ZTMM99120-WERKS,        "플랜트
         LGORT  TYPE ZTMM99120-LGORT,        "저장위치
         LIFNR  TYPE ZTMM99130-LIFNR,        "업체코드
         MATNR  TYPE ZTMM99120-MATNR,        "자재코드
         CHARG  TYPE ZTMM99120-CHARG,        "배치
         BWTAR  TYPE ZTMM99120-BWTAR,        "평가유형
         ERFMG  TYPE ZTMM99120-ERFMG,        "재고수량
         ZMEINS TYPE ZTMM99120-ZMEINS,       "재고단위
         INSMK  TYPE ZTMM99120-INSMK,        "재고유형
         EXBWR  TYPE ZTMM99120-EXBWR.        "평가금액
TYPES: END OF TS_UPD_D.

*-- 고객 Excel Upload
TYPES: BEGIN OF TS_UPD_E,
         WERKS  TYPE ZTMM99120-WERKS,        "플랜트
         LGORT  TYPE ZTMM99120-LGORT,        "저장위치
         KUNNR  TYPE ZTMM99150-KUNNR,        "고객코드
         MATNR  TYPE ZTMM99120-MATNR,        "자재코드
         VBELN  TYPE ZTMM99150-VBELN,        "판매오더
         POSNR  TYPE ZTMM99150-POSNR,        "판매오더항번
         CHARG  TYPE ZTMM99120-CHARG,        "배치
         BWTAR  TYPE ZTMM99120-BWTAR,        "평가유형
         ERFMG  TYPE ZTMM99120-ERFMG,        "재고수량
         ZMEINS TYPE ZTMM99120-ZMEINS,       "재고단위
         INSMK  TYPE ZTMM99120-INSMK,        "재고유형
         EXBWR  TYPE ZTMM99120-EXBWR.        "평가금액
TYPES: END OF TS_UPD_E.


TYPES: BEGIN OF TY_BATCH,
         STATUS      TYPE ICON-ID,                "상태
         BUKRS       TYPE ZTMM99110-BUKRS,        "회사코드
         ERNAM       TYPE ZTMM99110-ERNAM,        "작업자
         ZSEQ        TYPE ZTMM99110-ZSEQ,         "순번
         MATNR       TYPE ZTMM99110-MATNR,        "자재코드
         MAKTX       TYPE MAKT-MAKTX,             "자재명
         XCHPF       TYPE ZTMM99110-XCHPF,        "배치대상
         CLASS       TYPE ZTMM99110-CLASS,        "배치클래스
         ZCHECK      TYPE ZTMM99110-ZCHECK,       "구매관리
         EX_CHARG    TYPE ZTMM99110-CHARG,        "외부배치번호
         CHARG       TYPE ZTMM99110-CHARG,        "배치번호
         LIFNR       TYPE ZTMM99110-LIFNR,        "공급업체
         HSDAT       TYPE ZTMM99110-HSDAT,        "제조일자
         VFDAT       TYPE ZTMM99110-VFDAT,        "사용기간
         LICHA       TYPE ZTMM99110-LICHA,        "제조 LOT
         ZMM_MAKER   TYPE ZTMM99110-ZMM_MAKER,    "제조처
         ZCOMP_RATIO TYPE ZTMM99110-ZCOMP_RATIO,  "역가계산비율
         ZCOMP_MOIST TYPE ZTMM99110-ZCOMP_MOIST,  "수분비율
         ZCOMP_NET   TYPE ZTMM99110-ZCOMP_NET,    "함량
         ZCOMP_DRY   TYPE ZTMM99110-ZCOMP_DRY,    "건조중량
         ZLOTNO2     TYPE ZTMM99110-ZLOTNO2,      "수탁 LOTNO
         ZMESSAGE    TYPE ZTMM99110-ZMESSAGE,     "메시지
         DUP_CHECK   TYPE ZTMM99110-ZSEQ.         "중복체크
TYPES:END OF TY_BATCH.

TYPES: BEGIN OF TS_BATCH.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_BATCH.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30), "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_BATCH.

TYPES: BEGIN OF TY_DATA,
         STATUS   TYPE ICON-ID,             "상태

         BUKRS    TYPE ZTMM99120-BUKRS,     "회사코드
         WERKS    TYPE ZTMM99120-WERKS,     "플랜트
         NAME1_W  TYPE T001W-NAME1,         "플랜트명
         LGORT    TYPE ZTMM99120-LGORT,     "저장위치
         LGOBE    TYPE T001L-LGOBE,         "저장위치명
         LIFNR    TYPE ZTMM99130-LIFNR,     "공급업체
         NAME1_L  TYPE LFA1-NAME1,          "공급업체명
         KUNNR    TYPE ZTMM99150-KUNNR,     "고객
         NAME1_K  TYPE KNA1-NAME1,          "고객명
         ZSEQ     TYPE ZTMM99120-ZSEQ,      "순번
         ERNAM    TYPE ZTMM99120-ERNAM,     "작성자
         MATNR    TYPE ZTMM99120-MATNR,     "자재코드
         MAKTX    TYPE MAKT-MAKTX,          "자재명
         BESKZ    TYPE ZTMM99120-BESKZ,     "조달유형
         SOBSL    TYPE ZTMM99120-SOBSL,     "특별조달
         MMSTA    TYPE ZTMM99120-MMSTA,     "자재상태
         BKLAS    TYPE ZTMM99120-BKLAS,     "평가클래스
         XCHPF    TYPE ZTMM99120-XCHPF,     "배치여부
         CHARG    TYPE ZTMM99120-CHARG,     "배치
         BWTTY    TYPE ZTMM99120-BWTTY,     "평가유형여부
         BWTAR    TYPE ZTMM99120-BWTAR,     "평가유형
         VPRSV    TYPE ZTMM99120-VPRSV,     "가격지시자
         BWART    TYPE ZTMM99120-BWART,     "이동유형
         ERFMG    TYPE ZTMM99120-ERFMG,     "입력수량
         ZMEINS   TYPE ZTMM99120-ZMEINS,    "입력단위
         MEINS    TYPE ZTMM99120-MEINS,     "기본단위
         INSMK    TYPE ZTMM99120-INSMK,     "재고유형
         VBELN    TYPE ZTMM99150-VBELN,     "판매오더
         POSNR    TYPE ZTMM99150-POSNR,     "판매오더항목
         EXBWR    TYPE ZTMM99120-EXBWR,     "평가금액
         WAERS    TYPE ZTMM99120-WAERS,     "통화
         MJAHR    TYPE ZTMM99120-MJAHR,     "연도
         MBLNR    TYPE ZTMM99120-MBLNR,     "자재문서
         BUDAT    TYPE ZTMM99120-BUDAT,     "전기일
         ZEILE    TYPE ZTMM99120-ZEILE,     "항번
         MJAHR_R  TYPE ZTMM99120-MJAHR_R,   "연도
         MBLNR_R  TYPE ZTMM99120-MBLNR_R,   "취소자재문서
         ZEILE_R  TYPE ZTMM99120-ZEILE_R,   "항번
         ZSTATUS  TYPE ZTMM99120-ZSTATUS,   "상태
         ZMESSAGE TYPE ZTMM99120-ZMESSAGE,  "메시지
         INDEX_C  TYPE ZTMM99120-ZSEQ.      "인덱스
TYPES:END OF TY_DATA.

TYPES: BEGIN OF TS_DISP.
         INCLUDE TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: CDLST(30), "필수필드임 (테이블에 있을경우는 생략)
       END OF  TS_DISP.

" 엑셀 다운로드
TYPES : BEGIN OF TS_EXCEL_BATCH,
          MATNR       TYPE CHAR40,       "자재코드
          LIFNR       TYPE CHAR10,       "공급업체
          HSDAT       TYPE CHAR8,        "제조일자
          VFDAT       TYPE CHAR8,        "사용기간
          LICHA       TYPE CHAR20,       "제조 LOT
          ZMM_MAKER   TYPE CHAR30,       "제조처
          ZCOMP_RATIO TYPE CHAR8,        "역가계산비율
          ZCOMP_MOIST TYPE CHAR8,        "수분비율
          ZCOMP_NET   TYPE CHAR8,        "함량
          ZCOMP_DRY   TYPE CHAR8,        "건조중량
          ZLOTNO2     TYPE CHAR30,      "수탁 LOTNO
          CHARG       TYPE CHAR10,       "외부배치번호
          ZMESSAGE    TYPE CHAR200,      "메시지
        END OF TS_EXCEL_BATCH.

" 엑셀 다운로드
TYPES : BEGIN OF TS_EXCEL_STOCK,
          WERKS    TYPE CHAR4,        "플랜트
          LGORT    TYPE CHAR4,        "저장위치
          MATNR    TYPE CHAR40,       "자재코드
          CHARG    TYPE CHAR10,       "배치
          LIFNR    TYPE CHAR10,       "공급업체
          KUNNR    TYPE CHAR10,       "고객
          VBELN    TYPE CHAR10,       "판매오더
          POSNR    TYPE CHAR10,       "항번
          BWTAR    TYPE CHAR10,       "평가유형
          ERFMG    TYPE CHAR20,       "재고수량
          ZMEINS   TYPE CHAR10,       "재고단위
          INSMK    TYPE CHAR1,        "재고유형
          EXBWR    TYPE CHAR20,       "평가금액
          ZMESSAGE TYPE CHAR200,      "메시지
        END OF TS_EXCEL_STOCK.

TYPES : BEGIN OF TS_MARA,
          MATNR TYPE MARA-MATNR,       "자재코드
          MAKTX TYPE MAKT-MAKTX,       "자재명
          WERKS TYPE MARC-WERKS,       "플랜트
          NAME1 TYPE T001W-NAME1,      "플랜트명
          LGORT TYPE T001L-LGORT,      "저장위치
          LGOBE TYPE T001L-LGOBE,      "저장위치명
          XCHPF TYPE MARC-XCHPF,       "배치여부
          BESKZ TYPE MARC-BESKZ,       "조달유형
          SOBSL TYPE MARC-SOBSL,       "특별조달
          MMSTA TYPE MARC-MMSTA,       "자재상태
          BWTTY TYPE MARC-BWTTY,       "평가유형여부
          BKLAS TYPE MBEW-BKLAS,       "평가클래스
          BWTAR TYPE MBEW-BWTAR,       "평가유형
          VPRSV TYPE MBEW-VPRSV,       "가격지시자
          MEINS TYPE MARA-MEINS,       "기본단위
        END OF TS_MARA.

TYPES: TS_DATA(1500) TYPE C.
*----------------------------------------------------------------------*
* ALV Object
*----------------------------------------------------------------------*

DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " ALV Gridv

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA : GT_UPD_A TYPE TABLE OF TS_UPD_A,
       GT_BATCH TYPE TABLE OF TS_BATCH.

DATA: GT_UPD_B TYPE TABLE OF TS_UPD_B,
      GT_UPD_C TYPE TABLE OF TS_UPD_C,
      GT_UPD_D TYPE TABLE OF TS_UPD_D,
      GT_UPD_E TYPE TABLE OF TS_UPD_E,
      GT_DISP  TYPE TABLE OF TS_DISP.
*      GT_DISP_SUB TYPE TABLE OF TS_DISP.

*DATA : GT_ZTMM99120 TYPE TABLE OF ZTMM99120,
*       GT_ZTMM99130 TYPE TABLE OF ZTMM99130,
*       GT_ZTMM99140 TYPE TABLE OF ZTMM99140,
*       GT_ZTMM99150 TYPE TABLE OF ZTMM99150.


DATA : GV_TCNT TYPE SY-TABIX,
       GV_WAERS TYPE WAERS.

DATA: OK_CODE TYPE SY-UCOMM,
      GV_MODE    TYPE C VALUE '',   "Display :D  Edit:E (화면편집)
      GV_OK_CODE TYPE SY-UCOMM.

DATA: GS_MSGTB TYPE ZSCN00001.


" 엑셀
DATA: GT_EXCEL_BAT TYPE TABLE OF TS_EXCEL_BATCH,
      GS_EXCEL_BAT TYPE TS_EXCEL_BATCH,
      GT_EXCEL_STO TYPE TABLE OF TS_EXCEL_STOCK,
      GS_EXCEL_STO TYPE TS_EXCEL_STOCK,
      GT_DATA      TYPE TABLE OF TS_DATA,
      GS_DATA      TYPE TS_DATA.


DATA: GO_EXCEL TYPE OLE2_OBJECT, " EXCEL OBJECT
      GO_WORKBOOK  TYPE OLE2_OBJECT, " WORKBOOK
      GO_WORKSHEET TYPE OLE2_OBJECT,
      GO_RANGE     TYPE OLE2_OBJECT,
      GO_CELL1     TYPE OLE2_OBJECT,
      GO_CELL2     TYPE OLE2_OBJECT,
      GO_BUFFER    TYPE OLE2_OBJECT,
      GO_ROW       TYPE OLE2_OBJECT.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
************************************************************************
DEFINE _G_APPEND_VALUE.
  &1-OPTION = &2.
  &1-SIGN   = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1.
END-OF-DEFINITION.

************************************************************************
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .

DEFINE _G_INIT.
  CLEAR: &1, &1[].

END-OF-DEFINITION .
DEFINE _G_GET_CELL_VALUE.
  CALL METHOD IRF_DATA_CHANGED->GET_CELL_VALUE
    EXPORTING
      I_ROW_ID    = &1-ROW_ID
      I_FIELDNAME = &2
    IMPORTING
      E_VALUE     = &3.

END-OF-DEFINITION .
DEFINE _G_MODIFY_CELL.
  CALL METHOD IRF_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = &1-ROW_ID
      I_FIELDNAME = &2
      I_VALUE     = &3.

END-OF-DEFINITION .

DEFINE _G_WAIT_1_SECOND.

  WAIT UP TO '0.2' SECONDS.

END-OF-DEFINITION .

DEFINE _G_ADD_1.

  &1 = &1 + 1.

END-OF-DEFINITION .
