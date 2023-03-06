*&---------------------------------------------------------------------*
*& Include          ZOMM5510TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: EKKO, EKBE.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000'.

* GOS 관련 상수
CONSTANTS: GC_GOS_TYPEID TYPE SWO_OBJTYP VALUE 'BUS2012', "SWO1 등록한 OBJECT TYPE
           GC_GOS_CATID  TYPE SIBFCATID  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C110 TYPE LVC_EMPHSZ VALUE 'C110', "파란색
           GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300', "노란색
           GC_EMPHSZ_C500 TYPE LVC_EMPHSZ VALUE 'C500', "초록색
           GC_EMPHSZ_C700 TYPE LVC_EMPHSZ VALUE 'C700'. "빨간색

CONSTANTS: GC_ZTERM_XX01 TYPE RBVS-ZTERM VALUE 'XX01',
           GC_LAND1_KR   TYPE T042Z-LAND1 VALUE 'KR'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_DATA_01,
         BUKRS      TYPE EKKO-BUKRS,      "회사
         BELNR      TYPE RBKP-BELNR,      "송장문서번호
         GJAHR      TYPE RBKP-GJAHR,      "회계연도
         BUDAT      TYPE RBKP-BUDAT,      "전기일
         BLDAT      TYPE RBKP-BLDAT,      "증빙일
         AWKEY      TYPE BKPF-AWKEY,
         BELNR_FI   TYPE BKPF-BELNR,      "FI전표번호
         GJAHR_FI   TYPE BKPF-GJAHR,      "FI회계연도
         BUZEI_FI   TYPE BSEG-BUZEI,      "FI개별항목
         SRMWWR     TYPE RBVS-SRMWWR,     "반제금액
         WAERS      TYPE RBKP-WAERS,      "통화
         ZTERM      TYPE RBVS-ZTERM,      "지급조건
         ZTERM_TEXT TYPE T052U-TEXT1,     "지급조건내역
         MWSKZ      TYPE RBVS-MWSKZ,      "세금코드
         ZLSCH      TYPE RBVS-ZLSCH,      "지급방법
         ZLSCH_TEXT TYPE T042Z-TEXT1,     "지급방법내역
         EBELN      TYPE EKBE-EBELN,      "PO번호
         BUPLA      TYPE RBKP-BUPLA,      "사업장
       END OF TY_DATA_01.

TYPES: BEGIN OF TY_DATA_02,
         BUKRS    TYPE BKPF-BUKRS,       "회사
         BELNR    TYPE EKBE-BELNR,       "선급전표번호
         GJAHR    TYPE EKBE-GJAHR,       "회계연도
         BUZEI    TYPE BSEG-BUZEI,       "개별항목
         WRBTR    TYPE EKBE-WRBTR,       "선급 공급가액
         TAX_AMT  TYPE BSEG-WRBTR,       "선급 부가세 ZSVBMM_DOWNPAYT-TAX_AMOUNT,
         TOT_AMT  TYPE BSEG-WRBTR,       "선급금 총액
         WAERS    TYPE EKBE-WAERS,       "통화
         CLE_AMT  TYPE EKBE-WRBTR,       "선급반제금액
         REM_AMT  TYPE EKBE-WRBTR,       "선급잔액
         EBELN    TYPE EKBE-EBELN,       "PO번호
         LIFNR    TYPE EKKO-LIFNR,       "공급업체
         NAME1    TYPE LFA1-NAME1,       "업체명
         MWSKZ    TYPE BSEG-MWSKZ,       "세금코드
         BUPLA    TYPE BSEG-BUPLA,       "사업장
       END OF TY_DATA_02.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP_01.
         INCLUDE TYPE ZSCN00003.      "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA_01.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: CDLST(30),
         ZDELE,                       "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP_01.

"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP_02.
         INCLUDE TYPE ZSCN00003.      "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA_02.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: CDLST(30),
         ZDELE,                       "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP_02.

TYPES: BEGIN OF TY_CHECK,
         BUPLA TYPE ZSVBMM_DOWNPAYT-BUPLA,
         MWSKZ TYPE ZSVBMM_DOWNPAYT-MWSKZ,
         WAERS TYPE ZSVBMM_DOWNPAYT-CURRENCY,
       END OF TY_CHECK.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: GRF_HEAD TYPE REF TO CL_GUI_CONTAINER, "Header Information Object
      GRF_BODY_01 TYPE REF TO CL_GUI_CONTAINER,    "ALV Object
      GRF_BODY_02 TYPE REF TO CL_GUI_CONTAINER.

DATA: GRF_GRID_01 TYPE REF TO LCL_CUST_ALV_GRID, " ALV Grid
      GRF_GRID_02 TYPE REF TO LCL_CUST_ALV_GRID.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP_01 TYPE TABLE OF TY_DISP_01,
      GT_DISP_02 TYPE TABLE OF TY_DISP_02.

DATA: GR_EBELN TYPE RANGE OF EKKO-EBELN.

DATA: GV_BUDAT TYPE BKPF-BUDAT,
      GV_BLDAT   TYPE BKPF-BLDAT,
      GV_TAX_AMT TYPE BSEG-WRBTR,
      GV_WAERS   TYPE RBKP-WAERS,
      GV_TAX_CHK.     "선급금 부가세 유무 체크

DATA: OK_CODE TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      GV_MROW    TYPE I VALUE 10.    "Multi Row

* Selected Rows
DATA: GT_ROWS_01 TYPE LVC_T_ROW,
      GT_ROWS_02 TYPE LVC_T_ROW.

DATA: GV_EXC_USER(5) TYPE C.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION.

DEFINE _G_SET_VALUES.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = &1
      VALUES = &2.

END-OF-DEFINITION.
