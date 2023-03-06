*&---------------------------------------------------------------------*
*& Report ZOMM5010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZOMM5010 MESSAGE-ID ZMM01.

include ZOMM5010TOP.
include ZOMM5010CLS.
include ZOMM5010SCR.
include ZOMM5010O01.
include ZOMM5010I01.
include ZOMM5010F01.

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZE_SET.

************************************************************************
* AT SELECTION-SCREEN OUTPUT                                           *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SCREEN_MODIFY.
  PERFORM SET_F4_LISTBOX.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  PERFORM SET_SEL_CREATE_OPTION.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
*> 발주자
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPS-LOW.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORDPS-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDPS-HIGH.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_ORDPS-HIGH'.

*> 발주부서
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDDP-LOW.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDDP-LOW' SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORDDP-HIGH.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_ORDDP-HIGH' SPACE.

*> 지출발의자
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPPS-LOW.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPPS-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPPS-HIGH.
  PERFORM SET_F4_SEL_SCR_EMPLOY USING 'S_EXPPS-HIGH'.

*> 지출발의부서
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPDP-LOW.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXPDP-LOW' SPACE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_EXPDP-HIGH.
  PERFORM SET_F4_SEL_SCR_DEPART USING 'S_EXPDP-HIGH' SPACE.

*> 사업장
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BUPLA-LOW.
  PERFORM SET_F4_SEL_SCR_BUPLA USING 'S_BUPLA-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BUPLA-HIGH.
  PERFORM SET_F4_SEL_SCR_BUPLA USING 'S_BUPLA-HIGH'.

*> 세금지시자
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TAXIM-LOW.
  PERFORM SET_F4_SEL_SCR_TAXIM USING 'S_TAXIM-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TAXIM-HIGH.
  PERFORM SET_F4_SEL_SCR_TAXIM USING 'S_TAXIM-HIGH'.
************************************************************************
*START-OF-SELECTION.
************************************************************************
START-OF-SELECTION.
  PERFORM START_OF_SELECTION.

************************************************************************
*END-OF-SELECTION.
************************************************************************
END-OF-SELECTION.
  PERFORM END_OF_SELECTION.
