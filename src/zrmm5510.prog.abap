*&---------------------------------------------------------------------*
*& Report ZRMM5510
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM5510
*& T-CODE              : ZRMM5510
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 구매오더별 선급금 현황
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자     변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM5510 MESSAGE-ID ZMM01.

include ZRMM5510TOP. "Top Include
include ZRMM5510CLS. "ALV/TREE Class
include ZRMM5510SCR. "Selection Screen
include ZRMM5510O01. "Process Before Output
include ZRMM5510I01. "Process After Input
include ZRMM5510F01. "Business Logic Routine
include ZRMM5510F02. "ALV /Tree Logic Routine
