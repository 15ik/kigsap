*&---------------------------------------------------------------------*
*& Include          ZRMM5010TOP
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------
* 주의:ALV Display Table은 반드시 Types 선언으로 해주세요
*      인터널 테이블은 Header Line Table 금지
*-----------------------------------------------------------------------------
TABLES: T001,
        RBKP,
        RSEG,
        EKKO,
        LFA1,
*        ZTCN00009, "전자결재 상태 이력
*        ZDTV3T_AP_HEAD, " Unipost 매입전자세금계산서 Head
        UF05A.


*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS : LCL_CUST_ALV_GRID DEFINITION DEFERRED.


*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: GC_RE TYPE BKPF-BLART VALUE 'RE',
           GC_BSART_PSIC TYPE EKKO-BSART VALUE 'PSIC',
           GC_BSART_PSIN TYPE EKKO-BSART VALUE 'PSIN',
           GC_BSART_PSIM TYPE EKKO-BSART VALUE 'PSIM'.

CONSTANTS: GC_ZTERM_XX01 TYPE RBKP-ZTERM VALUE 'XX01', "선급금 반제금액
           GC_ZTERM_XX03 TYPE RBKP-ZTERM VALUE 'XX03'.  "지체상금 공제액

*CONSTANTS: GC_WAERS_KRW TYPE EKKO-WAERS VALUE 'KRW'.
*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
*>Main
TYPES: BEGIN OF TY_DATA,
*         APPSTATUS              TYPE ZTCN00009-APPSTATUS,    "전자결재 상태
*         APPSTATUS_TX(20)       TYPE C,    "전표상태(결재)
*         APVIFKEY               TYPE ZTCN00009-APVIFKEY,    "전자결재 연동KEY (FI)
*         FIID                   TYPE ZTCN00009-FIID,    "전자결재 연동KEY 2(FI)
*         INV_SIGN               TYPE ZDTV3T_AP_HEAD-INV_SIGN, "발행구분(정발행/역발행)
*         INV_SIGN_TX(20)        TYPE C,  "발행구분 내역
*         TYPE_CODE              TYPE ZDTV3T_AP_HEAD-TYPE_CODE, "세금계산서종류
*         TYPE_CODE_TX(40)       TYPE C,  "세금계산서종류 내역
         BELNR                  TYPE SUPPLINV-BELNR, "송장번호
         GJAHR                  TYPE SUPPLINV-GJAHR, "송장년도
         BELNR_FI               TYPE BKPF-BELNR, "회계전표번호
         BUDAT                  TYPE SUPPLINV-BUDAT, "전기일
         BLDAT                  TYPE RBKP-BLDAT,    "증빙일
         BUPLA                  TYPE SUPPLINV-BUPLA, "사업장
         BUPLA_TX               TYPE J_1BBRANCH-NAME,  "사업장명
         LIFNR                  TYPE SUPPLINV-LIFNR, "공급업체
         LIFNR_TX               TYPE LFA1-NAME1, "공급업체명
         STCD2                  TYPE LFA1-STCD2, "사업자 등록번호
         DMBTR                  TYPE BSEG-DMBTR, "공급가액(계약금액-부가세로 계산)
         WMWST1                 TYPE SUPPLINV-WMWST1, "부가세
         MWSKZ1                 TYPE RSEG-MWSKZ, "세금코드
         RMWWR                  TYPE BSEG-DMBTR, "계약금액(VAT포함)
*         ISSUE_ID               TYPE ZDTV3T_AP_HEAD-ISSUE_ID, "승인번호
         DWPAYC                 TYPE BSEG-DMBTR, "선급금 반제금액
         DELAYF                 TYPE BSEG-DMBTR, "지체상금공제금액
         ESTPAY                 TYPE BSEG-DMBTR, "실지급예정금액
         WAERS                  TYPE SUPPLINV-WAERS, "통화
         PAYSTS                 TYPE ICON-NAME, "대금지급여부
         AUGDT                  TYPE BSAK_REC-AUGDT,  "반제일
         PRTCHG                 TYPE BSEG-DMBTR, "인쇄교체비(계약금액포함)
         ACTPAY                 TYPE BSEG-DMBTR, "실비지급(계약금액포함)
         FIDOC_TYPE(10)         TYPE C, "전표구분
         ERFNAM                 TYPE SUPPLINV-ERFNAM, "송장처리자
         ERFNAM_TX              TYPE V_USERNAME-NAME_TEXT, "송장처리자명
*         AMEND_CODE             TYPE ZDTV3T_AP_HEAD-AMEND_CODE, "수정사유코드
*         ED_STATUS              TYPE ZDTV3T_AP_HEAD-STATUS, "세금계산서상태
*         ED_STATUS_TX(40)       TYPE C, "세금계산서상태내역
*         NTS_SEND_FLAG          TYPE ZDTV3T_AP_HEAD-NTS_SEND_FLAG, "국세청전송유무
         BSART                  TYPE EKKO-BSART,  "대표 PO 유형
         BSART_TX               TYPE T161T-BATXT, "PO 유형 내역
         ZTERM                  TYPE BSEG-ZTERM,  "지급조건
         ZTERM_TX               TYPE ZSVCMM_PAYTERM-TEXT1, "지급조건 내역
         MSGYN                  TYPE ICON-NAME,  "ERROR 메시지 표시
         BKTXT_FI               TYPE BKPF-BKTXT,  "회계 HEADER TEXT

*U1>2022.04.22 : PO 공급업체 추가(송장발행처와 다를 수 있음)
         LIFNR_PO               LIKE EKKO-LIFNR,

*> NO-DISPLAY
*         INV_SEQ                TYPE ZDTV3T_AP_HEAD-INV_SEQ, "전자세금계산서 KEY
         VGART                  TYPE RBKP-VGART,
         TBTKZ                  TYPE RSEG-TBTKZ,
         BELNR_CC               TYPE RBKP-BELNR,    "송장취소 번호
         GJAHR_CC               TYPE RBKP-GJAHR,    "송장취소 년도
         AUGBL                  TYPE BSAK_REC-AUGBL,  "반제전표번호

         ZEXPEN_PERSON          LIKE EKKO-ZEXPEN_PERSON,   "지출결의자
         ZEXPEN_PERSON_NAME(40) TYPE C,              "지출결의자명
         ZEXPEN_DEPARTMENT      LIKE EKKO-ZEXPEN_DEPARTMENT,   "지출결의부서
         ZEXPEN_DEPART_NAME(40) TYPE C,              "지출결의부서명
*         ZEXPEN_EML             LIKE ZSVMM_USER_INFO-EML, "지출결의 담당자 EMAIL
         VGABE                  LIKE EKBE-VGABE,
         SHKZG                  LIKE RSEG-SHKZG,

       END OF TY_DATA.

*> 상세 거래명세서 List
TYPES: BEGIN OF TY_DETAIL,
         BUDAT    TYPE EKBE-BUDAT, "입고일
         EBELN    TYPE EKBE-EBELN, "PO번호
         EBELP    TYPE EKBE-EBELP, "품목
         LFGJA    TYPE EKBE-LFGJA, "자재문서년도
         LFBNR    TYPE EKBE-LFBNR, "자재문서
         LFPOS    TYPE EKBE-LFPOS, "항목
         MATNR    TYPE EKBE-MATNR, "자재코드
         MAKTX    TYPE MAKT-MAKTX, "자재내역
         MENGE    TYPE EKBE-MENGE, "입고수량
         MEINS    TYPE EKPO-MEINS, "단위
         WRBTR    TYPE EKBE-WRBTR, "입고금액
         WAERS    TYPE RBKP-WAERS, "통화
         NETPR    TYPE MSEG-DMBTR, "금액(품목금액/입고수량으로 계산)
         MWSKZ    TYPE RSEG-MWSKZ, "세금코드
         WMWST    TYPE SUPPLINV-WMWST1, "세액
         NETPR_PO TYPE EKPO-NETPR, "단가
         BELNR    TYPE RSEG-BELNR, "송장번호
         GJAHR    TYPE RSEG-GJAHR, "송장년도
         BUZEI    TYPE RSEG-BUZEI, "송장 ITEM NO
         PEINH    TYPE EKPO-PEINH, "PO 가격단위
         BSTYP    TYPE EKKO-BSTYP, "PO 범주
         MENGE_PO TYPE EKPO-MENGE, "PO 수량
         VGABE    TYPE EKBE-VGABE,  "트랜잭션유형
       END OF TY_DETAIL.


TYPES: BEGIN OF TY_RSEG,
         BUKRS             TYPE SUPPLIERINVOICE-BUKRS,
         BELNR             TYPE SUPPLIERINVOICE-BELNR,
         GJAHR             TYPE SUPPLIERINVOICE-GJAHR,
         BELNR_FI          TYPE BKPF-BELNR,
         BUDAT             TYPE SUPPLIERINVOICE-BUDAT,
         BLDAT             TYPE SUPPLIERINVOICE-BLDAT,
         BUPLA             TYPE SUPPLIERINVOICE-BUPLA,
         LIFNR             TYPE SUPPLIERINVOICE-LIFNR,
         WMWST1            TYPE SUPPLIERINVOICE-WMWST1,
         MWSKZ1            TYPE SUPPLIERINVOICE-MWSKZ1,
         MWSKZ_I           TYPE RSEG-MWSKZ,
         RMWWR             TYPE SUPPLIERINVOICE-RMWWR,
         ERFNAM            TYPE SUPPLIERINVOICE-ERFNAM,
         STBLG             TYPE SUPPLIERINVOICE-STBLG,
         STJAH             TYPE SUPPLIERINVOICE-STJAH,
         ZFBDT             TYPE SUPPLIERINVOICE-ZFBDT,
         ZTERM             TYPE SUPPLIERINVOICE-ZTERM,
         ZLSCH             TYPE SUPPLIERINVOICE-ZLSCH,
         BVTYP             TYPE SUPPLIERINVOICE-BVTYP,
         HBKID             TYPE SUPPLIERINVOICE-HBKID,
         KURSF             TYPE SUPPLIERINVOICE-KURSF,
         BEZNK             TYPE SUPPLIERINVOICE-BEZNK,
         BKTXT             TYPE SUPPLIERINVOICE-BKTXT,
         EKGRP             TYPE EKKO-EKGRP,
         WAERS             TYPE SUPPLIERINVOICE-WAERS,
         VGART             TYPE SUPPLIERINVOICE-VGART,

         TBTKZ             TYPE RSEG-TBTKZ,
         ABSGR             TYPE EKKO-ABSGR,
         NAME1             TYPE LFA1-NAME1,
         LIFNR_TX          TYPE LFA1-NAME1,
         STCD2             TYPE LFA1-STCD2,
         EBELN             TYPE EKPO-EBELN,
         EBELP             TYPE EKPO-EBELP,
         ZPRI              TYPE ZCCMM_PRICE-ZPRI,
         AWKEY             TYPE BKPF-AWKEY,
         DMBTR             TYPE SUPPLIERINVOICE-RMWWR, "공급가액(계약금액 - 부가세)
*         INV_SEQ           TYPE ZDTV3T_AP_HEAD-INV_SEQ, "전자세금계산서 KEY
*         ISSUE_ID          TYPE ZDTV3T_AP_HEAD-ISSUE_ID, "승인번호
         MATNR             TYPE RSEG-MATNR,
         WERKS             TYPE EKPO-WERKS,
         ZEXPEN_PERSON     LIKE EKKO-ZEXPEN_PERSON,   "지출결의자
         ZEXPEN_DEPARTMENT LIKE EKKO-ZEXPEN_DEPARTMENT,   "지출결의부서
         BSTYP             LIKE EKKO-BSTYP,
         BSART             LIKE EKKO-BSART,
         KALSK             LIKE LFM1-KALSK,
         VGABE             LIKE EKBE-VGABE,
         SHKZG             LIKE RSEG-SHKZG,
         BUZEI             LIKE RSEG-BUZEI,

*U1>2022.04.22 : PO 공급업체 추가(송장발행처와 다를 수 있음)
         LIFNR_PO          LIKE EKKO-LIFNR,

         LINE_DEL          TYPE C,
       END OF TY_RSEG.
*--------------------------------
* F4 Type 선언
*--------------------------------
"*- Alv Dispaly
TYPES: BEGIN OF TS_DISP.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DATA.     "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: ZDELE TYPE CHAR1, "Table에 없을 경우 임시로 필수
       END OF  TS_DISP.

TYPES: BEGIN OF TS_DISP_DTL.
         include TYPE ZSCN00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE TY_DETAIL.
"Dcflg=> C:Create U:Update T:라인삭제 '' :Original
TYPES: END OF TS_DISP_DTL.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*
*DATA:grf_co TYPE REF TO ycl_co_common.


*---------------------
*-- ALV Object
*---------------------
DATA : GRF_DOCKING_CON TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_DOCKING_CON_200 TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA : GRF_HEAD TYPE REF TO CL_GUI_CONTAINER,
       GRF_BODY TYPE REF TO CL_GUI_CONTAINER.

DATA : GRF_GRID TYPE REF TO LCL_CUST_ALV_GRID, " 상(Header) ALV Grid
       GRF_GRID_DTL TYPE REF TO LCL_CUST_ALV_GRID.       " 하(Detial) ALV Grid


*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: GT_DISP TYPE TABLE OF TS_DISP,
      GS_DISP TYPE TS_DISP.

DATA: GT_DISP_DTL TYPE TABLE OF TS_DISP_DTL,
      GS_DISP_DTL     TYPE TS_DISP_DTL,
      GT_DISP_DTL_ALL TYPE TABLE OF TS_DISP_DTL.

DATA:GS_VARIANT TYPE DISVARIANT. " Variant
*     GS_FCAT    TYPE LVC_S_FCAT.   " Fieldcatalog MODIFY  .

DATA: OK_CODE TYPE SY-UCOMM,
      GV_OK_CODE TYPE SY-UCOMM,
*      GV_DBL_ROW TYPE I,         "Double Click
      GV_TABIX   TYPE I,
      GV_MODE,
      GV_MROW    TYPE I           VALUE 10.                   "Multi Row


DATA:GS_MSGTB TYPE ZSCN00001.

*>>> 개별 프로그램 전용
DATA: GV_EXC_USER TYPE C.
*      GV_BUTXT    TYPE T001-BUTXT.

DATA: OK_CODE_200 TYPE SY-UCOMM,
      GV_OK_CODE_200 TYPE SY-UCOMM.

DATA: OK_CODE_300 TYPE SY-UCOMM,
      GV_OK_CODE_300  TYPE SY-UCOMM,
      GV_CONTINUE_300 TYPE C.

DATA: GT_RSEG TYPE TABLE OF TY_RSEG,
      GS_RSEG TYPE TY_RSEG.

*>전표 취소 POPUP
DATA: BEGIN OF GS_SCR_300,
        STGRD LIKE UF05A-STGRD, "취소사유
        BUDAT LIKE RBKP-BUDAT,  "전기일
      END OF GS_SCR_300.

*> 공급받는자 정보
DATA: BEGIN OF GS_PKRBUPLA,
        BRANCH                       TYPE PKRBUPLA-BRANCH,
        COMPANYCODE                  TYPE PKRBUPLA-COMPANYCODE,
        TAXNUMBER2                   TYPE PKRBUPLA-TAXNUMBER2,
        BUSINESSPLACENAME            TYPE PKRBUPLA-BUSINESSPLACENAME,
        TAXINVOICEREPRESENTATIVENAME TYPE PKRBUPLA-TAXINVOICEREPRESENTATIVENAME,
        ADDRESSNAME                  TYPE PKRBUPLA-ADDRESSNAME,
        INDUSTRYTYPE                 TYPE PKRBUPLA-INDUSTRYTYPE,
        BUSINESSTYPE                 TYPE PKRBUPLA-BUSINESSTYPE,
      END OF GS_PKRBUPLA,
      GT_PKRBUPLA LIKE TABLE OF GS_PKRBUPLA.

*> 송장처리자명
DATA: BEGIN OF GS_SAP_USER,
        BNAME     TYPE V_USERNAME-BNAME,
        NAME_TEXT TYPE V_USERNAME-NAME_TEXT,
      END OF GS_SAP_USER,
      GT_SAP_USER LIKE TABLE OF GS_SAP_USER.

*> 전자세금계산서 상태내역
*DATA: BEGIN OF GS_ED_STATUS,
*        STATUS      TYPE ZDTV3T_STATUS-STATUS,
*        STATUS_TEXT TYPE ZDTV3T_STATUS-STATUS_TEXT,
*      END OF GS_ED_STATUS,
*      GT_ED_STATUS LIKE TABLE OF GS_ED_STATUS.

*> 사업장명
DATA: BEGIN OF GS_BUPLA,
        BUPLA TYPE J_1BBRANCH-BRANCH,
        NAME  TYPE J_1BBRANCH-NAME,
      END OF GS_BUPLA,
      GT_BUPLA LIKE TABLE OF GS_BUPLA.

*> 지급조건명
DATA: GT_ZTERM LIKE TABLE OF ZSVCMM_PAYTERM.

*> 전자세금계산서 맵핑 대상
DATA: BEGIN OF GS_MAPPING,
        BELNR    LIKE RSEG-BELNR,
        GJAHR    LIKE RSEG-GJAHR,
        BELNR_FI LIKE BKPF-BELNR,
        LIFNR    TYPE EKKO-LIFNR,
        BUPLA    TYPE RBKP-BUPLA,
*        INV_SEQ  TYPE ZDTV3S_AP_HD-INV_SEQ,
        MSGTB    TYPE ZYCN00001,
      END OF GS_MAPPING,
      GT_MAPPING LIKE TABLE OF GS_MAPPING.

DATA: GV_WAERS_LOCAL TYPE T001-WAERS.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _G_SET_VALUE.

  &1 = &2.

END-OF-DEFINITION .

DEFINE _G_INIT.
  REFRESH:&1.

END-OF-DEFINITION .
DEFINE _G_GET_CELL_VALUE.
  CALL METHOD irf_data_changed->get_cell_value
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
    IMPORTING
      e_value     = &3.

END-OF-DEFINITION .
DEFINE _G_MODIFY_CELL.
  CALL METHOD irf_data_changed->modify_cell
    EXPORTING
      i_row_id    = &1-row_id
      i_fieldname = &2
      i_value     = &3.

END-OF-DEFINITION .
DEFINE _G_SET_MSGTB.

  CLEAR:gs_msgtb.

    gs_msgtb = VALUE #( fieldname = &3   msgty = 'E'  arbgb = &4   txtnr = &5
                        msgv1     = &6   msgv2 = &7   msgv3 = &8  ).

  grf_grid->set_msgtb( EXPORTING iv_delete = &1
                                 is_msgtb  = gs_msgtb
                       CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _G_SET_TIMESTMAP.


  zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = &1
                                  CHANGING  cs_data   = &2 ).

END-OF-DEFINITION .
DEFINE _G_SET_VALUE.
  &1 = &2.
END-OF-DEFINITION .
DEFINE _G_ADD_1.

  &1 = &1 + 1.

END-OF-DEFINITION .
DEFINE _G_WAIT_1_SECOND.

  WAIT UP TO '0.5' SECONDS.

END-OF-DEFINITION .
