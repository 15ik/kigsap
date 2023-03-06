*&---------------------------------------------------------------------*
*& Include          ZRMM3030TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: EKKO, EKPO, MKPF, MSEG, MLAN.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: LCL_CUST_ALV_GRID DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: GC_DYNNR_1000 TYPE SY-DYNNR VALUE '1000'.

CONSTANTS: GC_BUKRS_1101 TYPE T001-BUKRS VALUE '1101',
           GC_ABSGR_01   TYPE EKKO-ABSGR VALUE '01',      "정발행
           GC_ABSGR_02   TYPE EKKO-ABSGR VALUE '02'.      "역발행

* ALV 관련 상수
CONSTANTS: GC_EMPHSZ_C110 TYPE LVC_EMPHSZ VALUE 'C110', "파란색
           GC_EMPHSZ_C300 TYPE LVC_EMPHSZ VALUE 'C300', "노란색
           GC_EMPHSZ_C500 TYPE LVC_EMPHSZ VALUE 'C500', "초록색
           GC_EMPHSZ_C700 TYPE LVC_EMPHSZ VALUE 'C700'. "빨간색

CONSTANTS: GC_CRITERIA_ALL TYPE FIELDNAME VALUE 'ALL',
           GC_CRITERIA_LIFNR   TYPE FIELDNAME VALUE 'LIFNR',
           GC_CRITERIA_MATNR   TYPE FIELDNAME VALUE 'MATNR',
           GC_CRITERIA_LIMAT   TYPE FIELDNAME VALUE 'LIMAT',
           GC_CRITERIA_DEPT_QM TYPE FIELDNAME VALUE 'DEPT_QM'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
"*- Alv Dispaly
TYPES: BEGIN OF TY_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
TYPES: BUDAT_GR TYPE BUDAT, "입고일(전기일)
         LIFNR        TYPE LIFNR,                       "공급업체
         LIFNR_TEXT   TYPE NAME1_GP,                    "공급업체명
         MBLNR_GR     TYPE MBLNR,                       "입고문서
         MJAHR_GR     TYPE MJAHR,                       "연도
         ZEILE_GR     TYPE MBLPO,                       "항목
         BKTXT        TYPE BKTXT,
         MATNR        TYPE MATNR,                       "자재
         TXZ01        TYPE TXZ01,                       "자재내역
         CHARG        TYPE CHARG_D,                     "배치
         BWTAR        TYPE BWTAR_D,                     "평가유형

         MENGE_GR     TYPE MENGE_D,                     "입고수량
         MEINS_GR     TYPE MEINS,                       "단위
         WRBTR_GR     TYPE WRBTR,                       "입고금액
         WAERS_GR     TYPE WAERS,                       "통화

         MENGE_RE     TYPE MENGE_D,                     "반품수량
         WRBTR_RE     TYPE WRBTR,                       "반품금액

         MENGE_IV     TYPE MENGE_D,                     "송장수량
         WRBTR_IV     TYPE WRBTR,                       "송장금액
         WAERS_IV     TYPE WAERS,                       "통화
         NETPR_IV     TYPE WRBTR,                       "송장단가(송장금액/송장수량)
         STATUS       TYPE ICON-ID,                     "상태
         MENGE_DIF    TYPE MENGE_D,                     "송장예정수량
         WRBTR_DIF    TYPE WRBTR,                       "송장예정금액

         EBELN        TYPE EBELN,                       "구매문서번호
         EBELP        TYPE EBELP,                       "항목
*         DEPT_QM      TYPE EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
         BSTYP        TYPE BSTYP,                       "구매문서유형
         PSTYP        TYPE PSTYP,                       "품목범주
         PSTYP_IND,
         VBELN_IM     TYPE VBELN_VL,                    "납품문서번호
         VBELP_IM     TYPE POSNR_VL,                    "항목
         LFIMG        TYPE LFIMG,                       "납품수량
         NETPR_PO     TYPE BPREI,                       "구매오더단가
         WAERS_PO     TYPE WAERS,                       "통화
         PEINH        TYPE EPEIN,                       "가격단위
         BPRME        TYPE BPRME,                       "오더가격단위
         WERKS        TYPE WERKS_D,                     "플랜트
         WERKS_TEXT   TYPE NAME1,                       "플랜트명
         EINDT        TYPE EKET-EINDT,                  "납품일정
         "Dcflg=> C:Create U:Update T:라인삭제 '':Original
         CDLST(30),
         ZDELE,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF TY_DISP.

* 입고 상세 Popup
TYPES: BEGIN OF TY_DETL,
         BUDAT_GR   TYPE BUDAT,           "입고일(전기일)
         LIFNR      TYPE LIFNR,           "공급업체
         LIFNR_TEXT TYPE NAME1_GP,        "공급업체명
         MBLNR_GR   TYPE MBLNR,           "입고문서
         MJAHR_GR   TYPE MJAHR,           "연도
         ZEILE_GR   TYPE MBLPO,           "항목
         MATNR      TYPE MATNR,           "자재
         TXZ01      TYPE TXZ01,           "자재내역
         CHARG      TYPE CHARG_D,         "배치
         BWTAR      TYPE BWTAR_D,         "평가유형
         MENGE_GR   TYPE MENGE_D,         "입고수량
         MEINS_GR   TYPE MEINS,           "단위
         WRBTR_GR   TYPE WRBTR,           "입고금액
         WAERS_GR   TYPE WAERS,           "통화

         MBLNR_RE   TYPE MBLNR,           "반품문서
         MJAHR_RE   TYPE MJAHR,           "연도
         ZEILE_RE   TYPE MBLPO,           "항목
         MENGE_RE   TYPE MENGE_D,         "반품수량
         WRBTR_RE   TYPE WRBTR,           "반품금액

         BELNR      TYPE BELNR_D,         "송장문서
         GJAHR      TYPE GJAHR,           "연도
         BUZEI      TYPE RBLGP,           "항목
         BUDAT_IV   TYPE BUDAT,           "전기일
         BLDAT_IV   TYPE BLDAT,           "증빙일
         VGABENM    TYPE CHAR30,          "송장유형
         MENGE_IV   TYPE MENGE_D,         "송장수량
         WRBTR_IV   TYPE WRBTR,           "송장금액
         WAERS_IV   TYPE WAERS,           "통화

         EBELN      TYPE EBELN,           "구매문서번호
         EBELP      TYPE EBELP,           "항목
         BSTYP      TYPE BSTYP,           "구매문서유형
         VBELN_IM   TYPE VBELN_VL,        "납품문서번호
         VBELP_IM   TYPE POSNR_VL,        "항목
         LFIMG      TYPE LFIMG,           "납품수량
         NETPR_PO   TYPE BPREI,           "구매오더단가
         WAERS_PO   TYPE WAERS,           "통화
         PEINH      TYPE EPEIN,           "가격단위
         BPRME      TYPE BPRME,           "오더가격단위
       END OF TY_DETL.

* 업체별 SUM
TYPES: BEGIN OF TY_LIFNR_SUM,
         LIFNR      TYPE LIFNR,
         LIFNR_TEXT TYPE NAME1_GP,
*         MENGE_GR   TYPE MENGE_D,
*         MEINS_GR   TYPE MEINS,
         WRBTR_GR   TYPE WRBTR,
         WAERS_GR   TYPE WAERS,
         MENGE_RE   TYPE MENGE_D,
         WRBTR_RE   TYPE WRBTR,
         WRBTR_IV   TYPE WRBTR,
         WAERS_IV   TYPE WAERS,
         MENGE_IV   TYPE MENGE_D,
         NETPR_IV   TYPE NETPR,
         MENGE_DIF  TYPE MENGE_D,
         WRBTR_DIF  TYPE WRBTR,
       END OF TY_LIFNR_SUM.

* 자재별 SUM
TYPES: BEGIN OF TY_MATNR_SUM,
         MATNR     TYPE MATNR,
         TXZ01     TYPE TXZ01,
         MENGE_GR  TYPE MENGE_D,
         MEINS_GR  TYPE MEINS,
         WRBTR_GR  TYPE WRBTR,
         WAERS_GR  TYPE WAERS,
         MENGE_RE  TYPE MENGE_D,
         WRBTR_RE  TYPE WRBTR,
         WRBTR_IV  TYPE WRBTR,
         WAERS_IV  TYPE WAERS,
         MENGE_IV  TYPE MENGE_D,
         NETPR_IV  TYPE NETPR,
         MENGE_DIF TYPE MENGE_D,
         WRBTR_DIF TYPE WRBTR,
       END OF TY_MATNR_SUM.

* 업체/자재별 SUM
TYPES: BEGIN OF TY_LIMAT_SUM,
         LIFNR      TYPE LIFNR,
         LIFNR_TEXT TYPE NAME1_GP,
         MATNR      TYPE MATNR,
         TXZ01      TYPE TXZ01,
         MENGE_GR   TYPE MENGE_D,
         MEINS_GR   TYPE MEINS,
         WRBTR_GR   TYPE WRBTR,
         WAERS_GR   TYPE WAERS,
         MENGE_RE   TYPE MENGE_D,
         WRBTR_RE   TYPE WRBTR,
         WRBTR_IV   TYPE WRBTR,
         WAERS_IV   TYPE WAERS,
         MENGE_IV   TYPE MENGE_D,
         NETPR_IV   TYPE NETPR,
         MENGE_DIF  TYPE MENGE_D,
         WRBTR_DIF  TYPE WRBTR,
       END OF TY_LIMAT_SUM.

* 검수부서별 SUM
TYPES: BEGIN OF TY_DEPT_QM_SUM,
*         DEPT_QM      TYPE EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
         MENGE_GR     TYPE MENGE_D,
         MEINS_GR     TYPE MEINS,
         WRBTR_GR     TYPE WRBTR,
         WAERS_GR     TYPE WAERS,
         MENGE_RE     TYPE MENGE_D,
         WRBTR_RE     TYPE WRBTR,
         WRBTR_IV     TYPE WRBTR,
         WAERS_IV     TYPE WAERS,
         MENGE_IV     TYPE MENGE_D,
         NETPR_IV     TYPE WRBTR,"NETPR, "U1 2022.08.19
         MENGE_DIF    TYPE MENGE_D,
         WRBTR_DIF    TYPE WRBTR,
       END OF TY_DEPT_QM_SUM.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: GRF_HEAD TYPE REF TO CL_GUI_CONTAINER, "Header Information Object
      GRF_BODY TYPE REF TO CL_GUI_CONTAINER.    "ALV Object

DATA: GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID. " ALV Grid

DATA: GT_FCAT TYPE LVC_T_FCAT.

* 입고 상세 Popup
DATA: GRF_CONT_DETL TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRF_GRID_DETL TYPE REF TO LCL_CUST_ALV_GRID.        " ALV Grid

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP TYPE TABLE OF TY_DISP,
      GT_ALL  TYPE TABLE OF TY_DISP.

DATA: GT_DETL TYPE TABLE OF TY_DETL.

DATA: GT_LIFNR_SUM TYPE TABLE OF TY_LIFNR_SUM,
      GT_MATNR_SUM   TYPE TABLE OF TY_MATNR_SUM,
      GT_LIMAT_SUM   TYPE TABLE OF TY_LIMAT_SUM,
      GT_DEPT_QM_SUM TYPE TABLE OF TY_DEPT_QM_SUM.

DATA: GR_ABSGR TYPE RANGE OF EKKO-ABSGR,
      GR_BSTYP TYPE RANGE OF EKKO-BSTYP,
      GR_BSART TYPE RANGE OF EKKO-BSART,
      GR_DPPCT TYPE RANGE OF EKKO-DPPCT.  "선금 비율

DATA: GV_CRITERIA TYPE FIELDNAME,
      GV_TECH(3).

DATA: OK_CODE TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      GV_MROW    TYPE I VALUE 10.    "Multi Row

DATA: GV_TABIX TYPE SY-TABIX.

* Selected Rows
*DATA: GT_ROWS TYPE LVC_T_ROW.

DATA: GV_EXC_USER TYPE C,
      GV_LOCAL_LAND1 TYPE T001-LAND1. "U2> LOCAL LAND

*----------------------------------------------------------------------*
* Get Data에서 사용
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_MAIN,
         MBLNR        TYPE MBLNR,
         MJAHR        TYPE MJAHR,
         ZEILE        TYPE MBLPO,
         SEQ          TYPE CATS_TABIX,
         BUDAT        TYPE BUDAT,
         MBLNR1       TYPE MBLNR,
         MJAHR1       TYPE MJAHR,
         ZEILE1       TYPE MBLPO,
         BKTXT       TYPE BKTXT,
         BUDAT1       TYPE BUDAT,
         SHKZG1       TYPE SHKZG,
         MATNR        TYPE MATNR,
         CHARG        TYPE CHARG_D,
         BWTAR        TYPE BWTAR_D,
         TXZ01        TYPE TXZ01,
         MAKTX        TYPE MAKTX,
         MENGE        TYPE MENGE_D,
         MEINS        TYPE MEINS,
         ERFMG        TYPE ERFMG,
         ERFME        TYPE ERFME,
         WRBTR        TYPE WRBTR,
         WAERS        TYPE WAERS,
         MBLNR2       TYPE MBLNR,
         MJAHR2       TYPE MJAHR,
         ZEILE2       TYPE MBLPO,
         BUDAT2       TYPE BUDAT,
         MENGE2       TYPE MENGE_D,
         WRBTR2       TYPE WRBTR,
         SHKZG2       TYPE SHKZG,
         BELNR        TYPE BELNR_D,
         GJAHR        TYPE GJAHR,
         BUZEI        TYPE RBLGP,
         BUDAT3       TYPE BUDAT,
         BLDAT3       TYPE BLDAT,
         WRBTR3       TYPE WRBTR,
         WAERS3       TYPE WAERS,
         NETPR3       TYPE WRBTR,
         MENGE3       TYPE MENGE_D,
         SHKZG3       TYPE SHKZG,
         EBELN        TYPE EBELN,
         EBELP        TYPE EBELP,
         EPSTP        TYPE EPSTP,
         PSTYP        TYPE PSTYP,
         VBELN_IM     TYPE VBELN_VL,
         VBELP_IM     TYPE POSNR_VL,
         LFIMG        TYPE LFIMG,
         BSTYP        TYPE BSTYP,
         WEBRE        TYPE WEBRE,
         MENGE_DIFF   TYPE MENGE_D,
         WRBTR_DIFF   TYPE WRBTR,
         LIFNR        TYPE LIFNR,
         LIFNRNM      TYPE NAME1_GP,
         VGABE        TYPE VGABE,
         VGABENM      TYPE CHAR30,
         NETPR        TYPE BPREI,
         PO_WAERS     TYPE WAERS,
         PEINH        TYPE EPEIN,
         BPRME        TYPE BPRME,
         KBETR        TYPE KBETR_KOND,
         KPEIN        TYPE KPEIN,
         KONWA        TYPE KONWA,
         WERKS        TYPE WERKS_D,
         WERKSNM      TYPE NAME1,
*         DEPT_QM      TYPE EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
       END OF TY_MAIN.

* 결과 Table
DATA: GT_HDATA TYPE TABLE OF TY_MAIN,
      GT_DDATA TYPE TABLE OF TY_MAIN.

DATA: GT_MAIN TYPE TABLE OF TY_MAIN,
      GS_MAIN TYPE TY_MAIN.


TYPES: BEGIN OF TY_DATA, "101, 102, 122
         MBLNR        LIKE MSEG-MBLNR,
         MJAHR        LIKE MSEG-MJAHR,
         ZEILE        LIKE MSEG-ZEILE,
         SEQ          TYPE CATS_TABIX,
         BUDAT        LIKE MKPF-BUDAT,
         BKTXT        TYPE MKPF-BKTXT,
         MATNR        LIKE MSEG-MATNR,
         TXZ01        TYPE TXZ01,
         MENGE        LIKE MSEG-MENGE,
         MEINS        LIKE MSEG-MEINS,
         ERFMG        TYPE ERFMG,
         ERFME        TYPE ERFME,
         BPMNG        TYPE BPMNG,
         BPRME        TYPE BPRME,
         EBELN        LIKE MSEG-EBELN,
         EBELP        LIKE MSEG-EBELP,
         VBELN_IM     LIKE MSEG-VBELN_IM,
         VBELP_IM     LIKE MSEG-VBELP_IM,
         LFBNR        LIKE MSEG-LFBNR,
         LFBJA        LIKE MSEG-LFBJA,
         LFPOS        LIKE MSEG-LFPOS,
         BWART        LIKE MSEG-BWART,
         SHKZG        LIKE MSEG-SHKZG,
         KZBEW        LIKE MSEG-KZBEW,
         VGART        LIKE MKPF-VGART,
         KZZUG        LIKE MSEG-KZZUG,
         SOBKZ        LIKE MSEG-SOBKZ,
         WRBTR        LIKE EKBE-WRBTR,
         WAERS        LIKE EKBE-WAERS,
         LFIMG        LIKE LIPS-LFIMG,
         BSTYP        TYPE BSTYP,
         WEBRE        TYPE WEBRE,
         CHARG        TYPE CHARG_D,
         BWTAR        TYPE BWTAR_D,
         LIFNR        TYPE LIFNR,
         LIFNRNM      TYPE NAME1_GP,
         BSART        TYPE ESART,
         WERKS        TYPE WERKS_D,
         MEINS_PO     TYPE MEINS,
         BPRME_PO     TYPE BBPRM,
         NETPR        LIKE EKPO-NETPR,
         TCODE2_MKPF  LIKE MSEG-TCODE2_MKPF,
         EKORG        TYPE EKORG,
         INFNR        TYPE INFNR,
         PSTYP        TYPE PSTYP,
         PEINH        LIKE EKPO-PEINH,
         PO_WAERS     TYPE WAERS,
         WERKSNM      TYPE NAME1,
*         DEPT_QM      TYPE EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
       END OF TY_DATA.

DATA: GT_DATA TYPE TABLE OF TY_DATA.

DATA: GT_101 TYPE TABLE OF TY_DATA,
      GT_101_B TYPE TABLE OF TY_DATA.

TYPES: BEGIN OF TY_102, "102, 122, ZZZ
         LFBNR        LIKE MSEG-LFBNR,
         LFBJA        LIKE MSEG-LFBJA,
         LFPOS        LIKE MSEG-LFPOS,
         SEQ          TYPE CATS_TABIX,
         MBLNR        LIKE MSEG-MBLNR,
         MJAHR        LIKE MSEG-MJAHR,
         ZEILE        LIKE MSEG-ZEILE,
         BKTXT        LIKE MKPF-BKTXT,
         BUDAT        LIKE MKPF-BUDAT,
         MATNR        LIKE MSEG-MATNR,
         TXZ01        TYPE TXZ01,
         MENGE        LIKE MSEG-MENGE,
         MEINS        LIKE MSEG-MEINS,
         ERFMG        TYPE ERFMG,
         ERFME        TYPE ERFME,
         BPMNG        TYPE BPMNG,
         BPRME        TYPE BPRME,
         EBELN        LIKE MSEG-EBELN,
         EBELP        LIKE MSEG-EBELP,
         VBELN_IM     LIKE MSEG-VBELN_IM,
         VBELP_IM     LIKE MSEG-VBELP_IM,
         BWART        LIKE MSEG-BWART,
         SHKZG        LIKE MSEG-SHKZG,
         KZBEW        LIKE MSEG-KZBEW,
         VGART        LIKE MKPF-VGART,
         KZZUG        LIKE MSEG-KZZUG,
         SOBKZ        LIKE MSEG-SOBKZ,
         WRBTR        LIKE EKBE-WRBTR,
         WAERS        LIKE EKBE-WAERS,
         LFIMG        LIKE LIPS-LFIMG,
         BSTYP        TYPE BSTYP,
         WEBRE        TYPE WEBRE,
         CHARG        TYPE CHARG_D,
         BWTAR        TYPE BWTAR_D,
         LIFNR        TYPE LIFNR,
         LIFNRNM      TYPE NAME1_GP,
         BSART        TYPE ESART,
         WERKS        TYPE WERKS_D,
         MEINS_PO     TYPE MEINS,
         BPRME_PO     TYPE BBPRM,
         NETPR        LIKE EKPO-NETPR,
         EKORG        TYPE EKORG,
         INFNR        TYPE INFNR,
         PSTYP        TYPE PSTYP,
         PEINH        LIKE EKPO-PEINH,
         PO_WAERS     TYPE WAERS,
         WERKSNM      TYPE NAME1,
*         DEPT_QM      TYPE EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
       END OF TY_102.

DATA: GT_102 TYPE TABLE OF TY_102,
      GT_102_B TYPE TABLE OF TY_102.

TYPES: BEGIN OF TY_SMBLN,
         SMBLN LIKE MSEG-SMBLN,
         SJAHR LIKE MSEG-SJAHR,
         SMBLP LIKE MSEG-SMBLP,
       END OF TY_SMBLN.

DATA: GT_SMBLN TYPE TABLE OF TY_SMBLN.

TYPES: BEGIN OF TY_MBLNR,
         MBLNR TYPE MBLNR,
         MJAHR TYPE MJAHR,
         ZEILE TYPE MBLPO,
       END OF TY_MBLNR.

DATA: GT_MBLNR TYPE TABLE OF TY_MBLNR.


DATA: GT_EKBE_101 TYPE TABLE OF EKBE,
      GT_EKBE_102 TYPE TABLE OF EKBE.

TYPES: BEGIN OF TY_PO,
         EBELN TYPE EBELN,
         EBELP TYPE EBELP,
       END OF TY_PO.

DATA: GT_PO TYPE TABLE OF TY_PO.

TYPES: BEGIN OF TY_EKPO,
         EBELN        TYPE EBELN,
         EBELP        TYPE EBELP,
         TXZ01        TYPE TXZ01,
         BSTYP        TYPE BSTYP,
         MEINS        TYPE MEINS,
         BPRME        TYPE BBPRM,
         NETPR        LIKE EKPO-NETPR,
         LIFNR        TYPE LIFNR,
         EKORG        TYPE EKORG,
         INFNR        TYPE INFNR,
         PSTYP        TYPE PSTYP,
         WERKS        TYPE WERKS_D,
         PEINH        LIKE EKPO-PEINH,
         PO_WAERS     TYPE WAERS,
         WERKSNM      TYPE NAME1,
*         DEPT_QM      TYPE ZIAMORGCD, "EKPO-ZQM_DEPARTMENT,
*         DEPT_QM_NAME TYPE ZSVMM_USER_INFO-DEPART_NAME,
       END OF TY_EKPO.

DATA: GT_EKPO TYPE TABLE OF TY_EKPO.

TYPES: BEGIN OF TY_EKET,
       EBELN TYPE EKET-EBELN,
       EBELP TYPE EKET-EBELP,
       ETENR TYPE EKET-ETENR,
       EINDT TYPE EKET-EINDT,
       END OF TY_EKET.

DATA: GT_EKET TYPE TABLE OF TY_EKET.


TYPES: BEGIN OF TY_DO,
         VBELN_IM LIKE MSEG-VBELN_IM,
         VBELP_IM LIKE MSEG-VBELP_IM,
       END OF TY_DO.

DATA: GT_DO TYPE TABLE OF TY_DO.

TYPES: BEGIN OF TY_LIPS,
         VBELN_IM LIKE MSEG-VBELN_IM,
         VBELP_IM LIKE MSEG-VBELP_IM,
         LFIMG    LIKE LIPS-LFIMG,
       END OF TY_LIPS.

DATA: GT_LIPS TYPE TABLE OF TY_LIPS.

TYPES: BEGIN OF TY_LIFNR,
         LIFNR TYPE LIFNR,
       END OF TY_LIFNR.

DATA: GT_LIFNR TYPE TABLE OF TY_LIFNR.

TYPES: BEGIN OF TY_LIFNR_NM,
         LIFNR TYPE LIFNR,
         NAME1 TYPE NAME1_GP,
       END OF TY_LIFNR_NM.

DATA: GT_LIFNR_NM TYPE TABLE OF TY_LIFNR_NM.

TYPES: BEGIN OF TY_IV1,
         LFBNR   LIKE RSEG-LFBNR,
         LFGJA   LIKE RSEG-LFGJA,
         LFPOS   LIKE RSEG-LFPOS,
         SEQ     TYPE CATS_TABIX,
         BELNR   LIKE RSEG-BELNR,
         GJAHR   LIKE RSEG-GJAHR,
         BUZEI   LIKE RSEG-BUZEI,
         BUZEI2  LIKE EKBE-BUZEI,
         BUDAT   TYPE BUDAT,
         BLDAT   TYPE BLDAT,
         VGART   LIKE RBKP-VGART,
         LIFNR   TYPE LIFNR,
         STBLG   LIKE RBKP-STBLG,
         STJAH   LIKE RBKP-STJAH,
         EBELN   LIKE RSEG-EBELN,
         EBELP   LIKE RSEG-EBELP,
         SHKZG   LIKE RSEG-SHKZG,
         WRBTR   LIKE RSEG-WRBTR,
         WAERS   LIKE RBKP-WAERS,
         MENGE   TYPE MENGE_D,
         MEINS   TYPE MEINS,
         XBLNR   LIKE RSEG-XBLNR,
         PSTYP   LIKE RSEG-PSTYP,
         VGABE   LIKE EKBE-VGABE,
         VGABENM TYPE CHAR30,
         EKORG   TYPE EKORG,
         BSART   TYPE ESART,
         INFNR   TYPE INFNR,
         WERKS   TYPE WERKS_D,
         MATNR   TYPE MATNR,
       END OF TY_IV1.

DATA: GT_IV1 TYPE TABLE OF TY_IV1.

TYPES: BEGIN OF TY_EBKE_IV,
         EBELN TYPE EBELN,
         EBELP TYPE EBELP,
         ZEKKN TYPE DZEKKN,
         VGABE TYPE VGABE,
         GJAHR TYPE MJAHR,
         BELNR TYPE MBLNR,
         BUZEI TYPE MBLPO,
         MENGE TYPE MENGE_D,
         WRBTR TYPE WRBTR,
       END OF TY_EBKE_IV.

DATA: GT_EBKE_IV TYPE TABLE OF TY_EBKE_IV.

TYPES: BEGIN OF TY_102_SUM, "102, 122
         LFBNR LIKE MSEG-LFBNR,
         LFBJA LIKE MSEG-LFBJA,
         LFPOS LIKE MSEG-LFPOS,
         MENGE LIKE MSEG-MENGE,
         MEINS LIKE MSEG-MEINS,
         WRBTR LIKE EKBE-WRBTR,
         WAERS LIKE EKBE-WAERS,
       END OF TY_102_SUM.

DATA: GT_102_SUM TYPE TABLE OF TY_102_SUM.

TYPES: BEGIN OF TY_IV1_SUM,
         LFBNR LIKE RSEG-LFBNR,
         LFGJA LIKE RSEG-LFGJA,
         LFPOS LIKE RSEG-LFPOS,
         WRBTR LIKE RSEG-WRBTR,
         WAERS LIKE RBKP-WAERS,
         MENGE TYPE MENGE_D,
         MEINS TYPE MEINS,
       END OF TY_IV1_SUM.

DATA: GT_IV1_SUM TYPE TABLE OF TY_IV1_SUM,
      GT_IV2_SUM TYPE TABLE OF TY_IV1_SUM.

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
