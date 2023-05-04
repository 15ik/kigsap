*&---------------------------------------------------------------------*
*& Include          ZRMM4090F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_screen_output .

  LOOP AT SCREEN.

    CASE 'X'.
      WHEN p_rd1b.                "PO

        IF screen-group1 EQ 'SC2'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.

        IF screen-group1 EQ 'SC1'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.

      WHEN p_rd1a.                "Inbound Delivery

        IF screen-group1 EQ 'SC2'.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.

        IF screen-group1 EQ 'SC1'.
          screen-input = 1.
          screen-invisible = 0.
        ENDIF.

        IF screen-name EQ 'P_CHK3'
        OR screen-name EQ 'P_CHK4'.

          screen-active = 1.
          screen-input = 0.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  "기간 DEFAULT SETTING: 당월 1일 ~ 당일
  DATA: lv_start_date TYPE sy-datum.

  lv_start_date = sy-datum.
  lv_start_date+6(2) = '01'.

  s_udate[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_start_date high = sy-datum ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_authority .

*  CALL FUNCTION 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      iv_user = sy-uname.
**     IV_OBJECT                   = 'ZMM_COMMON'
**     IV_BUKRS                    =
**     IV_EKORG                    =
**     IV_LGORT                    =
**     IV_WERKS                    =
**     IV_ZEXSPA                   =
**     IV_ZPODEP                   =
**     IV_ZPRDEP                   =
** TABLES
**     IT_BUKRS                    =
**     IT_EKORG                    =
**     IT_LGORT                    =
**     IT_WERKS                    =
**     IT_ZEXSPA                   =
**     IT_ZPODEP                   =
**     IT_ZPRDEP                   =
** EXCEPTIONS
**     NO_ID_DATA_FOUND            = 1
**     AUTHORIZATION_FAILURE       = 2
**     NO_INPUT_AUTH_VALUE         = 3
**     NO_DATA_FOUND               = 4
**     MANDATORYFIELDISMISS        = 5
**     OTHERS  = 6
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_inb.

*-
  _g_init: gt_idheader, gt_iditem.
*-

  PERFORM main_select_inb.
  PERFORM processing_data_inb.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_SELECT_INB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main_select_inb .

  SELECT FROM zcbmminbinfo
    FIELDS
    vbeln, bukrs, butxt, lfdat, lifnr, name1, sublifnr, subname, ztrans, ztrfee, vsbed, werks,
    name2, lgort, lgobe1, zwerks, name3, zlgort, lgobe2, zstatus, zmessage, zconfirm, loekz,
    zwmsifid, zwmstrcid, erdat, erzet, ernam, aedat, aezet, aenam, ztflag, zgrmemo, ztrmemo

  WHERE bukrs = @p_bukrs
    AND lifnr IN @s_lifnr
    AND aedat BETWEEN @s_udate-low AND @s_udate-high
    AND vbeln IN @s_vbeln
    AND zstatus IN ('C', 'F','X') "전송안된 Confirm 상태, 전송실패 상태만 조회
  INTO CORRESPONDING FIELDS OF TABLE @gt_idheader.

  SELECT FROM zcbmminbinfo
    FIELDS
    vbeln, bukrs, butxt, lfdat, lifnr, name1, sublifnr, subname, ztrans, ztrfee, vsbed, werks,
    name2, lgort, lgobe1, zwerks, name3, zlgort, lgobe2, zstatus, zmessage, zconfirm, loekz,
    zwmsifid, zwmstrcid, erdat, erzet, ernam, aedat, aezet, aenam, ztflag, zgrmemo, ztrmemo

  WHERE bukrs = @p_bukrs
    AND lifnr IN @s_lifnr
    AND aedat BETWEEN @s_udate-low AND @s_udate-high
    AND vbeln IN @s_vbeln
    AND zstatus =  'L' "전송후 삭제 상태
    AND ztflag = 'X'
  APPENDING CORRESPONDING FIELDS OF TABLE @gt_idheader.

  IF gt_idheader[] IS NOT INITIAL.

    SORT gt_idheader BY vbeln.
    DELETE ADJACENT DUPLICATES FROM gt_idheader COMPARING vbeln.

    SELECT FROM ztmm40301 AS a INNER JOIN makt AS b ON a~matnr = b~matnr
                               INNER JOIN t001w AS c ON a~werks = c~werks
                               INNER JOIN t001l AS d ON a~werks = d~werks AND a~lgort = d~lgort
      FIELDS
       a~vbeln ,a~posnr ,a~matnr ,b~maktx ,a~werks ,c~name1 ,a~lgort ,d~lgobe ,a~lfimg ,a~vrkme ,a~vgbel ,a~vgpos ,a~pstyp ,a~charg
      ,a~bwtar ,a~hsdat ,a~lichn ,a~vfdat ,a~zmaker ,a~zbresv1 ,a~zbresv2 ,a~zbresv3 ,a~zbresv4 ,a~zbresv5 ,a~zbresv6 ,a~zbresv7 ,a~zbresv8 ,a~zbresv9 ,a~zbresv10
      ,a~zpalqty ,a~zinspectno ,a~zdocpath1 ,a~zdocpath2 ,a~zdocpath3 ,a~loekz, a~ztflag, a~erdat ,a~erzet ,a~ernam ,a~aedat ,a~aezet ,a~aenam ,a~zdele
      ,a~zinspection, a~zinspresult

      FOR ALL ENTRIES IN @gt_idheader
      WHERE a~vbeln = @gt_idheader-vbeln
        AND b~spras = '3'
      INTO CORRESPONDING FIELDS OF TABLE @gt_iditem.

    IF gt_iditem[] IS NOT INITIAL.
      SORT gt_iditem BY vbeln posnr.
      DELETE ADJACENT DUPLICATES FROM gt_iditem COMPARING vbeln posnr.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_INB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processing_data_inb .

  DATA : lt_ztmm40300 TYPE TABLE OF ztmm40300,
         lt_ztmm40301 TYPE TABLE OF ztmm40301.

*-
  _g_init: lt_ztmm40300, lt_ztmm40301.

  IF gt_idheader[] IS NOT INITIAL.
    SORT gt_idheader BY vbeln.
    DELETE ADJACENT DUPLICATES FROM gt_idheader COMPARING vbeln.

**- Log Header
*    SELECT FROM ZTMM40300
*      FIELDS
*         VBELN, BUKRS, LFDAT, LIFNR, SUBLIFNR, ZTRANS, ZTRFEE, VSBED, WERKS, LGORT, ZWERKS,
*         ZLGORT, ZSTATUS,ZTFLAG, ZMESSAGE, ZCONFIRM, ZTRMEMO, ZGRMEMO, LOEKZ, ZWMSIFID, ZWMSTRCID,
*         ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
*
*      FOR ALL ENTRIES IN @GT_IDHEADER
*      WHERE VBELN = @GT_IDHEADER-VBELN
*      INTO CORRESPONDING FIELDS OF TABLE @LT_ZTMM40300.
*
*    IF LT_ZTMM40300[] IS NOT INITIAL.
*
*      SORT LT_ZTMM40300 BY VBELN.
*      DELETE ADJACENT DUPLICATES FROM LT_ZTMM40300 COMPARING VBELN.
*
**- Log Item
*      SELECT FROM ZTMM40301
*        FIELDS
*        WERKS, LGORT, LFIMG, VRKME, VGBEL, VGPOS, PSTYP, CHARG, BWTAR, HSDAT, LICHN, VFDAT, ZKEYIN, ZMAKER,
*        ZBRESV1, ZBRESV2, ZBRESV3, ZBRESV4, ZBRESV5, ZBRESV6, ZBRESV7, ZBRESV8, ZBRESV9, ZBRESV10, ZPALQTY, ZINSPECTNO,
*        ZDOCPATH1, ZDOCPATH2, ZDOCPATH3, LOEKZ, ZTFLAG, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE, ZINSPECTION, ZINSPRESULT
*
*        FOR ALL ENTRIES IN @LT_ZTMM40300
*        WHERE VBELN = @LT_ZTMM40300-VBELN
*        INTO CORRESPONDING FIELDS OF TABLE @LT_ZTMM40301.
*      ENDIF.


*- HEADER 상태 결정
    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>).
      IF <ls_idheader>-loekz = 'L' AND <ls_idheader>-ztflag = 'X' AND <ls_idheader>-zstatus = 'L' AND p_chk2 IS INITIAL.  " 이미 전송된 건 중 삭제 건이 있으면
        <ls_idheader>-zflag = 'D'.
      ELSEIF <ls_idheader>-loekz = 'L' AND <ls_idheader>-ztflag = 'X' AND <ls_idheader>-zstatus = 'X' AND p_chk2 IS NOT INITIAL.
        <ls_idheader>-zflag = 'D'.
      ELSEIF <ls_idheader>-loekz = 'L' AND <ls_idheader>-ztflag = ''.  " 미전송된 건 중 삭제 건이 있으면
        <ls_idheader>-zflag = ''.
      ELSE.

        READ TABLE gt_iditem TRANSPORTING NO FIELDS WITH KEY vbeln = <ls_idheader>-vbeln  ztflag = 'X'.

        IF sy-subrc NE 0. " 전송 결과가 없는 신규 품목
          <ls_idheader>-zflag = 'I'.

        ELSE.

*          READ TABLE GT_IDITEM TRANSPORTING NO FIELDS WITH KEY VBELN = <LS_IDHEADER>-VBELN  LOEKZ = ''.

*          IF SY-SUBRC NE 0. "품목 중 삭제 안된 품목이 없는 경우
*            <LS_IDHEADER>-ZFLAG = 'D'.
*
*          ELSE.

          READ TABLE gt_iditem TRANSPORTING NO FIELDS WITH KEY vbeln = <ls_idheader>-vbeln  ztflag = ''.

          IF sy-subrc NE 0.

            <ls_idheader>-zflag = ''. "전송 대상 X
          ELSE.
            <ls_idheader>-zflag = 'U'.  " 일부 전송된 정보 있으면 변경으로 전송.
          ENDIF.

*          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF p_chk2 IS INITIAL. " 전송 완료 건 포함 조회 아닐 경우, 대상 건만 조회

      "전송 대상 건중, 입고 완료건은 대상에서 제외_2022.02.04 수정
      DATA(lt_idheader) = gt_idheader[].
      SORT lt_idheader BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lt_idheader COMPARING vbeln.

      IF lt_idheader[] IS NOT INITIAL.
        SELECT FROM likp
          FIELDS vbeln, wbstk
          FOR ALL ENTRIES IN @lt_idheader
          WHERE vbeln = @lt_idheader-vbeln
            AND wbstk = 'C'
          INTO TABLE @DATA(lt_likp).
      ENDIF.

      LOOP AT lt_likp INTO DATA(ls_likp).
        READ TABLE gt_idheader INTO DATA(ls_del) WITH KEY vbeln = ls_likp-vbeln BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE TABLE gt_idheader FROM ls_del.

          LOOP AT gt_iditem INTO DATA(ls_del_item) WHERE vbeln = ls_likp-vbeln.
            IF ls_del_item IS NOT INITIAL.
              DELETE TABLE gt_iditem FROM ls_del_item.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.


      LOOP AT gt_idheader INTO DATA(ls_idheader) WHERE zflag = ''.
        DELETE TABLE gt_idheader FROM ls_idheader.

        LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = ls_idheader-vbeln.
          IF ls_iditem IS NOT INITIAL.
            DELETE TABLE gt_iditem FROM ls_iditem.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

*- ITEM 상태 결정

    LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>).

      IF <ls_iditem>-ztflag = 'X'. "전송 이력 O
        <ls_iditem>-zflag = ''.    "KTNG 전송대상 X
      ELSEIF <ls_iditem>-ztflag = ''. "전송 이력 X
        <ls_iditem>-zflag = 'I'.    "신규
      ELSE.
        <ls_iditem>-zflag = 'U'.    "그 이외는 변경
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_po .

*-
  _g_init: gt_data, gt_poheader, gt_poitem.
*-

  IF p_chk2 IS INITIAL. "전송 완료 건 조회 X

    PERFORM main_select_po.
    PERFORM processing_data_po.

  ELSE. "전송 완료 건 포함

    PERFORM main_select_all_po.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_SELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main_select_po.

  DATA : ls_poheader TYPE ts_poheader.

  SELECT FROM zsvcmmpotrhist( p_bukrs = @p_bukrs )
    FIELDS
    ebeln, bukrs, butxt, bsart, zaedat, udate, utime, lifnr, name1" ZSTATUS AS ZFLAG, ZMSG

    WHERE bukrs = @p_bukrs
      AND lifnr IN @s_lifnr
      AND udate BETWEEN @s_udate-low AND @s_udate-high
      AND ebeln IN @s_ebeln
*      AND ZSTATUS IN ('E', '') "전송안된 Confirm 상태, 전송실패 상태만 조회
     INTO CORRESPONDING FIELDS OF TABLE @gt_data.

  DATA(lt_data) = gt_data[].

  IF lt_data[] IS NOT INITIAL.

    SORT lt_data BY bukrs ebeln.
    DELETE ADJACENT DUPLICATES FROM lt_data COMPARING bukrs ebeln.

    SELECT FROM ztmm40304
      FIELDS ebeln, bukrs, zstatus, zmsg, erdat, erzet, ernam
    FOR ALL ENTRIES IN @lt_data
      WHERE ebeln = @lt_data-ebeln
        AND bukrs = @lt_data-bukrs
"        AND ZSTATUS IN ('E', '')
      INTO TABLE @DATA(lt_ztmm40304).

    SORT lt_ztmm40304 BY ebeln bukrs erdat DESCENDING erzet DESCENDING ernam DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_ztmm40304 COMPARING ebeln bukrs erdat erzet ernam.

    LOOP AT gt_data INTO DATA(ls_data).

      MOVE-CORRESPONDING ls_data TO ls_poheader.

      READ TABLE lt_ztmm40304 INTO DATA(ls_ztmm40304) WITH KEY ebeln = ls_data-ebeln
                                                               bukrs = ls_data-bukrs.
      IF sy-subrc = 0.
        ls_poheader-zflag = ls_ztmm40304-zstatus.
        ls_poheader-zmsg = ls_ztmm40304-zmsg.
      ENDIF.

      IF ls_poheader-zflag NE 'S'.
        APPEND ls_poheader TO gt_poheader. CLEAR ls_poheader.
      ENDIF.

    ENDLOOP.
  ENDIF.

  IF gt_poheader[] IS NOT INITIAL.

    SORT gt_poheader BY ebeln.
    DELETE ADJACENT DUPLICATES FROM gt_poheader COMPARING ebeln.

*- PO ITEM
    SELECT FROM ekpo AS a INNER JOIN eket AS b ON a~ebeln = b~ebeln AND a~ebelp = b~ebelp
                        INNER JOIN t001w AS c ON a~werks = c~werks
*                        INNER JOIN zttm01010 AS e ON a~werks = e~werks AND a~lgort = e~lgort
                        LEFT OUTER JOIN t001l AS d ON a~werks = d~werks AND a~lgort = d~lgort
    FIELDS
      a~ebeln, a~ebelp, a~matnr, a~txz01, a~werks, c~name1, a~lgort, d~lgobe, a~loekz, b~charg,
      a~bwtar, a~retpo, b~eindt, a~menge, a~meins, a~elikz , a~insmk

    FOR ALL ENTRIES IN @gt_poheader
    WHERE a~ebeln = @gt_poheader-ebeln
      AND b~etenr = @gc_0001
      AND a~bstae <> @gc_0004
*      AND e~zwms = 'X'
      AND a~elikz = ''
    INTO CORRESPONDING FIELDS OF TABLE @gt_poitem.

    IF gt_poitem[] IS NOT INITIAL.
      SORT gt_poitem BY ebeln ebelp.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN_SELECT_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main_select_all_po.


  SELECT FROM ztmm40304 AS a INNER JOIN t001 AS b ON a~bukrs = b~bukrs
                             INNER JOIN lfa1 AS c ON a~lifnr = c~lifnr
    FIELDS
    a~ebeln, a~bukrs, b~butxt, a~bsart, a~zaedat, a~udate, a~utime, a~lifnr, c~name1,
    a~zebeln, a~loekz, a~zstatus AS zflag, a~zmsg

   WHERE a~bukrs = @p_bukrs
     AND a~lifnr IN @s_lifnr
     AND udate BETWEEN @s_udate-low AND @s_udate-high
     AND ebeln IN @s_ebeln
    INTO CORRESPONDING FIELDS OF TABLE @gt_poheader.

  IF gt_poheader[] IS NOT INITIAL.
    SORT gt_poheader BY ebeln.
    DELETE ADJACENT DUPLICATES FROM gt_poheader COMPARING ebeln.

*- PO ITEM
    SELECT FROM     ztmm40305 AS a
                               INNER JOIN t001w AS b ON a~werks = b~werks
                               INNER JOIN t001l AS c ON a~werks = c~werks AND a~lgort = c~lgort
                               INNER JOIN ekpo AS d ON a~ebeln = d~ebeln AND a~ebelp = d~ebelp      "2022.03.04 추가
    FIELDS
      a~ebeln, a~ebelp, a~matnr, a~txz01, a~werks, b~name1, a~lgort, c~lgobe, a~charg,
      a~bwtar, a~retpo, a~eindt, a~menge, a~meins, a~elikz, a~zebeln, a~zebelp, d~loekz,
      d~menge AS po_menge  " U3      T0210054     2022.05.19
    FOR ALL ENTRIES IN @gt_poheader
    WHERE a~ebeln = @gt_poheader-ebeln
    INTO CORRESPONDING FIELDS OF TABLE @gt_poitem.
  ENDIF.


  DATA(lt_poitem) = gt_poitem[].

*- 21.07.12 전송완료건 포함 조회 시에도 전송 가능 추가
  LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>).

    IF p_chk3 = 'X'.    "삭제 PO 재전송

      IF <ls_poitem>-loekz = 'L'.
        <ls_poitem>-zflag = 'D'.
      ELSE.
        DELETE TABLE gt_poitem FROM <ls_poitem>.
      ENDIF.

    ELSE.

      IF <ls_poitem>-loekz = 'L'.
        <ls_poitem>-zflag = 'D'.
      ELSE.
        <ls_poitem>-zflag = 'I'.
      ENDIF.

    ENDIF.

*&    U3      T0210054     2022.05.19
    IF p_chk4 = 'X'.
      <ls_poitem>-menge = <ls_poitem>-po_menge.
    ENDIF.

  ENDLOOP.

  "삭제 건에 해당하는 Header 제외하고 Delete.
  IF p_chk3 = 'X'.
    LOOP AT gt_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln.
      IF sy-subrc NE 0.
        DELETE TABLE gt_poheader FROM <ls_poheader>.
      ELSE.

        READ TABLE lt_poitem TRANSPORTING NO FIELDS WITH KEY loekz = ''
                                                             ebeln = <ls_poheader>-ebeln.

        IF sy-subrc NE 0.  "삭제 품목 존재, 변경 안된 품목 존재 시 U
          <ls_poheader>-loekz = 'L'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processing_data_po.

*-
  _g_init: gt_ztmm40304, gt_ztmm40305.

  IF gt_poheader[] IS NOT INITIAL.

    SORT gt_poheader BY ebeln.
    DELETE ADJACENT DUPLICATES FROM gt_poheader COMPARING ebeln.

*- Log Header
    SELECT FROM ztmm40304
    FIELDS
      ebeln, bukrs, zdest, bsart, zaedat, lifnr, zebeln, udate, utime, loekz, zstatus, zmsg,
      erdat, erzet, ernam, aedat, aezet, aenam, zdele
    FOR ALL ENTRIES IN @gt_poheader
    WHERE ebeln = @gt_poheader-ebeln
      AND zstatus = 'S'
    INTO CORRESPONDING FIELDS OF TABLE @gt_ztmm40304.

    IF gt_ztmm40304[] IS NOT INITIAL.
      SORT gt_ztmm40304 BY ebeln.
      DELETE ADJACENT DUPLICATES FROM gt_ztmm40304 COMPARING ebeln.

*- Log Item
      SELECT FROM ztmm40305
      FIELDS
        ebeln, ebelp, matnr, txz01, werks, lgort, charg, bwtar, retpo, eindt, menge, meins, elikz,
        zebeln, zebelp, loekz, erdat, erzet, ernam, aedat, aezet, aenam, zdele
      FOR ALL ENTRIES IN @gt_ztmm40304
      WHERE ebeln = @gt_ztmm40304-ebeln
      INTO CORRESPONDING FIELDS OF TABLE @gt_ztmm40305.

    ENDIF.

    IF gt_ztmm40305[] IS NOT INITIAL.
      SORT gt_ztmm40305 BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM gt_ztmm40305 COMPARING ebeln ebelp.
    ENDIF.

  ENDIF.

*- 품목 정보 비교

  LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>).

    READ TABLE gt_ztmm40305 INTO DATA(ls_ztmm40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                             ebelp = <ls_poitem>-ebelp BINARY SEARCH.

    IF sy-subrc NE 0. " 전송 결과가 없는 경우
      IF <ls_poitem>-loekz = ''.
        <ls_poitem>-zflag = 'I'.
      ELSE.
*        <LS_POITEM>-ZFLAG = ''.
        DELETE TABLE gt_poitem FROM <ls_poitem>.
      ENDIF.

    ELSEIF <ls_poitem>-loekz = 'L' AND ls_ztmm40305-loekz = ''.

      <ls_poitem>-zflag = 'D'.

    ELSEIF <ls_poitem>-matnr <> ls_ztmm40305-matnr OR <ls_poitem>-txz01 <> ls_ztmm40305-txz01 OR <ls_poitem>-werks <> ls_ztmm40305-werks OR
         <ls_poitem>-lgort <> ls_ztmm40305-lgort OR <ls_poitem>-charg <> ls_ztmm40305-charg OR <ls_poitem>-bwtar <> ls_ztmm40305-bwtar OR
         <ls_poitem>-retpo <> ls_ztmm40305-retpo OR <ls_poitem>-eindt <> ls_ztmm40305-eindt OR <ls_poitem>-menge <> ls_ztmm40305-menge OR
         <ls_poitem>-meins <> ls_ztmm40305-meins OR <ls_poitem>-elikz <> ls_ztmm40305-elikz OR <ls_poitem>-zebeln <> ls_ztmm40305-zebeln OR
         <ls_poitem>-zebelp <> ls_ztmm40305-zebelp OR <ls_poitem>-loekz <> ls_ztmm40305-loekz.

      <ls_poitem>-zflag = 'U'.

    ENDIF.
  ENDLOOP.

*- HEADER 전송 FLAG 설정

  LOOP AT gt_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    READ TABLE gt_ztmm40304 TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

    IF sy-subrc NE 0. " 전송 결과가 없는 경우 신규
      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY zflag = 'I' ebeln = <ls_poheader>-ebeln.
      IF sy-subrc = 0.
        <ls_poheader>-zflag = 'I'.
      ELSE.
        <ls_poheader>-zflag = ''.
      ENDIF.
    ELSE.

      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY zflag = 'U' ebeln = <ls_poheader>-ebeln.

      IF sy-subrc = 0.  "전송 이력 존재 & 업데이트 품목 O

        <ls_poheader>-zflag = 'U'.

      ENDIF.

      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY zflag = 'I' ebeln = <ls_poheader>-ebeln.

      IF sy-subrc = 0.  "전송 이력 존재 & 업데이트 품목 O

        <ls_poheader>-zflag = 'U'.

      ENDIF.

      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY zflag = 'D' ebeln = <ls_poheader>-ebeln.

      IF sy-subrc = 0.  "삭제 품목 존재

        READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY zflag = '' ebeln = <ls_poheader>-ebeln.

        IF sy-subrc = 0.  "삭제 품목 존재, 변경 안된 품목 존재 시 U

          <ls_poheader>-zflag = 'U'.

        ELSE. "전체 품목 삭제 시 D

          <ls_poheader>-zflag = 'D'.
          <ls_poheader>-loekz = 'L'.

        ENDIF.
      ENDIF.
**** 이것만 제거하면 전송완료된 정보까지 포함
****          IF <LS_POHEADER>-ZFLAG = 'S'. <LS_POHEADER>-ZFLAG = ''.  ENDIF.
**** 이것만 제거하면 전송완료된 정보까지 포함
    ENDIF.
  ENDLOOP.

  LOOP AT gt_poheader INTO DATA(ls_poheader).

    IF ls_poheader-zflag = ''.
      LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE ebeln = ls_poheader-ebeln.
        DELETE TABLE gt_poitem FROM ls_poitem.
      ENDLOOP.

      DELETE TABLE gt_poheader FROM ls_poheader.

    ELSE.
      READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = ls_poheader-ebeln.
      IF sy-subrc NE 0. "POITEM X or 납품완료상태
        DELETE TABLE gt_poheader FROM ls_poheader.
      ENDIF.
    ENDIF.

*    READ TABLE GT_POITEM INTO DATA(LS_POITEM) WITH KEY EBELN = LS_POHEADER-EBELN.
*
*    IF SY-SUBRC = 0.
*      DELETE TABLE GT_POITEM FROM LS_POITEM.
*      DELETE TABLE GT_POHEADER FROM LS_POHEADER.
*    ELSE.
*      DELETE TABLE GT_POHEADER FROM LS_POHEADER.
*    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BACKGROUND_PROCESSING_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM background_processing_po .

  CASE p_bukrs.
    WHEN gc_1101.
      PERFORM po_info_ktng_wms.
    WHEN gc_2101.
      PERFORM po_info_kgc_wms.
    WHEN gc_3101.
      PERFORM po_info_yjp_wms.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KTNG_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM po_info_ktng_wms .

  DATA : ls_poheader_ktng    TYPE zsmm_po_wmsheader_ktng,
         ls_poheader_ktng_if TYPE zsmm_po_wmsheader_ktng,
         lt_poheader_ktng_if TYPE TABLE OF zsmm_po_wmsheader_ktng,

         ls_poitem_ktng      TYPE zsmm_po_wmsitem_ktng,
         ls_poitem_ktng_if   TYPE zsmm_po_wmsitem_ktng,
         lt_poitem_ktng_if   TYPE TABLE OF zsmm_po_wmsitem_ktng,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

  _g_init : lt_ztmm40304, lt_ztmm40305.

  IF gt_poheader[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
    LOOP AT gt_poheader INTO DATA(ls_poheader).

      CLEAR : ls_poheader_ktng, ls_poheader_ktng_if.
      _g_init: lt_poheader_ktng_if, lt_poitem_ktng_if.

      IF ls_poheader-zflag = 'D'.
        ls_poheader-loekz = 'L'.
      ELSE.
        ls_poheader-loekz = ''.
      ENDIF.

      MOVE-CORRESPONDING ls_poheader TO ls_poheader_ktng.
      ls_poheader_ktng-aedat = ls_poheader-zaedat.

*      _g_conv_strc_sap_to_eai ls_poheader_ktng ls_poheader_ktng_if.

      IF ls_poheader_ktng_if-lifnr IS NOT INITIAL.
        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
          EXPORTING
            iv_bp = ls_poheader_ktng_if-lifnr
          IMPORTING
            ev_bp = ls_poheader_ktng_if-lifnr.
      ENDIF.

      APPEND ls_poheader_ktng_if TO lt_poheader_ktng_if.

*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
      LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE ebeln = ls_poheader-ebeln
                                               AND zflag NE ''.

        CLEAR : ls_poitem_ktng, ls_poitem_ktng_if.

        MOVE-CORRESPONDING ls_poitem TO ls_poitem_ktng.

        IF ls_poitem_ktng-meins IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input          = ls_poitem_ktng-meins
              language       = sy-langu
            IMPORTING
              output         = ls_poitem_ktng-meins
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
        ENDIF.

*        _g_conv_strc_sap_to_eai ls_poitem_ktng ls_poitem_ktng_if.

        APPEND ls_poitem_ktng_if TO lt_poitem_ktng_if.

      ENDLOOP.

**********************************************************************
*> I/F 실행
**********************************************************************

      CLEAR : gs_out_header, gs_in_header.

      gs_out_header-if_id = gc_if_id_0074.                  "MM-0074
      gs_out_header-additional_info = gc_info_ktng.     "KTNG WMS

*> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).

*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'KTG20' ).

      CALL FUNCTION 'ZFIMM_PO_INFO_KTNG_WMS' "DESTINATION lv_rfcdest
        EXPORTING
          is_header   = gs_out_header
        IMPORTING
          es_header   = gs_in_header
        TABLES
          it_poheader = lt_poheader_ktng_if
          it_poitem   = lt_poitem_ktng_if.


      CLEAR ls_ztmm40304.

*> HEADER 이력 MODIFY
      MOVE-CORRESPONDING ls_poheader TO ls_ztmm40304.

      ls_ztmm40304-zdest = '1'.

      IF ls_poheader-zflag = 'D'.
        ls_ztmm40304-loekz = 'L'.
      ELSE.
        ls_ztmm40304-loekz = ''.
      ENDIF.

      IF gs_in_header-rst_cd = gc_cd_9999.
        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.

        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = ls_poheader-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

*> ITEM 이력 MODIFY
      LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE ebeln = ls_poheader-ebeln.

        CLEAR ls_ztmm40305.

        MOVE-CORRESPONDING <ls_poitem> TO ls_ztmm40305.

        ls_ztmm40305-charg = ''.

        READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                             ebelp = <ls_poitem>-ebelp BINARY SEARCH.

        IF sy-subrc = 0.
          ls_ztmm40305-loekz = ls_40304-loekz.

          ls_ztmm40305-erdat = ls_40304-erdat.
          ls_ztmm40305-erzet = ls_40304-erzet.
          ls_ztmm40305-ernam = ls_40304-ernam.
        ELSE.
          ls_ztmm40305-erdat = sy-datum.
          ls_ztmm40305-erzet = sy-uzeit.
          ls_ztmm40305-ernam = sy-uname.
        ENDIF.

        ls_ztmm40305-aedat = sy-datum.
        ls_ztmm40305-aezet = sy-uzeit.
        ls_ztmm40305-aenam = sy-uname.

        APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

      ENDLOOP.
    ENDLOOP.

    TRY.
        MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
        MESSAGE s013(zmm01). "DB Update System Error !
    ENDTRY.

    TRY.
        MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
        MESSAGE s013(zmm01). "DB Update System Error !
    ENDTRY.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_exit .
*  DATA:LV_ANS.

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------
*  PERFORM CHECK_CHANGED_DATA USING 'E' CHANGING LV_ANS.

*  CHECK LV_ANS IS INITIAL.

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAIN_SELECT_ALL_INB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM MAIN_SELECT_ALL_INB .
*
*  SELECT FROM ZCBMMINBINFO
*    FIELDS
*         VBELN, BUKRS, BUTXT, LFDAT, LIFNR, NAME1, SUBLIFNR, SUBNAME, ZTRANS, ZTRFEE, VSBED,
*         WERKS, NAME2, LGORT, LGOBE1, ZWERKS, NAME3, ZLGORT, LGOBE2, ZSTATUS, ZMESSAGE, ZCONFIRM,
*         LOEKZ, ZWMSIFID, ZWMSTRCID, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM
*   WHERE BUKRS = @P_BUKRS
*     AND LIFNR IN @S_LIFNR
*     AND AEDAT BETWEEN @S_UDATE-LOW AND @S_UDATE-HIGH
*     AND VBELN IN @S_VBELN
*     AND ZSTATUS IN ('C', 'X', 'F', 'L') "전송안된 Confirm 상태, 전송실패 상태만 조회, 에러발생, 납품서삭제
*    INTO CORRESPONDING FIELDS OF TABLE @GT_IDHEADER.
*
*  IF GT_IDHEADER[] IS NOT INITIAL.
*    SORT GT_IDHEADER BY VBELN.
*    DELETE ADJACENT DUPLICATES FROM GT_IDHEADER COMPARING VBELN.
*
**- ITEM
*    SELECT FROM ZTMM40301 AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
*                               INNER JOIN T001W AS C ON A~WERKS = C~WERKS
*                               INNER JOIN T001L AS D ON A~WERKS = D~WERKS AND A~LGORT = D~LGORT
*    FIELDS
*      A~VBELN, A~POSNR, A~MATNR, B~MAKTX, A~WERKS, C~NAME1, A~LGORT, D~LGOBE, A~LFIMG, A~VRKME, A~VGBEL, A~VGPOS, A~PSTYP, A~CHARG, A~BWTAR, A~HSDAT,
*      A~LICHN, A~VFDAT, A~ZMAKER, A~ZBRESV1, A~ZBRESV2, A~ZBRESV3, A~ZBRESV4, A~ZBRESV5, A~ZBRESV6, A~ZBRESV7, A~ZBRESV8, A~ZBRESV9, A~ZBRESV10,
*      A~ZPALQTY, A~ZINSPECTNO, A~ZDOCPATH1, A~ZDOCPATH2, A~ZDOCPATH3, A~LOEKZ, A~ZTFLAG, A~ERDAT, A~ERZET, A~ERNAM, A~AEDAT, A~AEZET, A~AENAM
*    FOR ALL ENTRIES IN @GT_IDHEADER
*    WHERE A~VBELN = @GT_IDHEADER-VBELN
*      AND B~SPRAS = '3'
*    INTO CORRESPONDING FIELDS OF TABLE @GT_IDITEM.
*  ENDIF.
*
**- 21.07.12 전송완료건 포함 조회 시에도 전송 가능 추가
*  LOOP AT GT_IDITEM ASSIGNING FIELD-SYMBOL(<LS_IDITEM>).
*    IF <LS_IDITEM>-LOEKZ = 'L'.
*      <LS_IDITEM>-ZFLAG = 'D'.
*    ELSE.
*      <LS_IDITEM>-ZFLAG = 'I'.
*    ENDIF.
*  ENDLOOP.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form BACKGROUND_PROCESSING_INB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM background_processing_inb.

  CASE p_bukrs.
    WHEN gc_1101.
      PERFORM id_info_ktng_wms.
    WHEN gc_2101.
      PERFORM id_info_kgc_wms.
    WHEN gc_3101.
      PERFORM id_info_yjp_wms.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_YJP_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM po_info_yjp_wms .

  DATA : ls_poheader_yjp    TYPE zsmm_po_wmsheader,
         ls_poheader_yjp_if TYPE zsmm_po_wmsheader,
         lt_poheader_yjp_if TYPE TABLE OF zsmm_po_wmsheader,

         ls_poitem_yjp      TYPE zsmm_po_wmsitem_yjp,
         ls_poitem_yjp_if   TYPE zsmm_po_wmsitem_yjp,
         lt_poitem_yjp_if   TYPE TABLE OF zsmm_po_wmsitem_yjp,

         ls_ztmm40304       TYPE ztmm40304,
         lt_ztmm40304       TYPE TABLE OF ztmm40304,
         ls_ztmm40305       TYPE ztmm40305,
         lt_ztmm40305       TYPE TABLE OF ztmm40305.

  _g_init : lt_ztmm40304, lt_ztmm40305.

  IF gt_poheader[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
    LOOP AT gt_poheader INTO DATA(ls_poheader).

      CLEAR : ls_poheader_yjp, ls_poheader_yjp_if.
      _g_init: lt_poheader_yjp_if, lt_poitem_yjp_if.

      MOVE-CORRESPONDING ls_poheader TO ls_poheader_yjp.
      ls_poheader_yjp-aedat = ls_poheader-zaedat.

*      _g_conv_strc_sap_to_eai ls_poheader_yjp ls_poheader_yjp_if.

      IF ls_poheader-zflag = 'D'.
        ls_poheader_yjp_if-loekz = 'L'.
      ELSE.
        ls_poheader_yjp_if-loekz = ''.
      ENDIF.

      IF ls_poheader_yjp_if-lifnr IS NOT INITIAL.
        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
          EXPORTING
            iv_bp = ls_poheader_yjp_if-lifnr
          IMPORTING
            ev_bp = ls_poheader_yjp_if-lifnr.
      ENDIF.

      APPEND ls_poheader_yjp_if TO lt_poheader_yjp_if.


*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
      LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE ebeln = ls_poheader-ebeln
                                               AND zflag NE ''.

        CLEAR : ls_poitem_yjp, ls_poitem_yjp_if.

        MOVE-CORRESPONDING ls_poitem TO ls_poitem_yjp.

        IF ls_poitem_yjp-meins IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input          = ls_poitem_yjp-meins
              language       = sy-langu
            IMPORTING
              output         = ls_poitem_yjp-meins
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
        ENDIF.

*        _g_conv_strc_sap_to_eai ls_poitem_yjp ls_poitem_yjp_if.

        APPEND ls_poitem_yjp_if TO lt_poitem_yjp_if.

      ENDLOOP.


**********************************************************************
*> I/F 실행
**********************************************************************

      CLEAR : gs_out_header, gs_in_header.

      gs_out_header-if_id = gc_if_id_0077.                  "MM-0074
      gs_out_header-additional_info = gc_info_yjp. "YJP WMS

*> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'YJP10' ).

      CALL FUNCTION 'ZFIMM_PO_INFO_YJP_WMS' "DESTINATION lv_rfcdest
        EXPORTING
          is_header   = gs_out_header
        IMPORTING
          es_header   = gs_in_header
        TABLES
          it_poheader = lt_poheader_yjp_if
          it_poitem   = lt_poitem_yjp_if.

      CLEAR ls_ztmm40304.

*> HEADER 이력 MODIFY
      MOVE-CORRESPONDING ls_poheader TO ls_ztmm40304.

      ls_ztmm40304-zdest = '4'.

      IF gs_in_header-rst_cd = gc_cd_9999.
        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.
        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = ls_poheader-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

*> HEADER 이력 MODIFY
      LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE ebeln = ls_poheader-ebeln.

        CLEAR ls_ztmm40305.

        MOVE-CORRESPONDING <ls_poitem> TO ls_ztmm40305.

        ls_ztmm40305-bwtar = ''.

        READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                             ebelp = <ls_poitem>-ebelp BINARY SEARCH.

        IF sy-subrc = 0.
          ls_ztmm40305-loekz = ls_40304-loekz.

          ls_ztmm40305-erdat = ls_40304-erdat.
          ls_ztmm40305-erzet = ls_40304-erzet.
          ls_ztmm40305-ernam = ls_40304-ernam.
        ELSE.
          ls_ztmm40305-erdat = sy-datum.
          ls_ztmm40305-erzet = sy-uzeit.
          ls_ztmm40305-ernam = sy-uname.
        ENDIF.

        ls_ztmm40305-aedat = sy-datum.
        ls_ztmm40305-aezet = sy-uzeit.
        ls_ztmm40305-aenam = sy-uname.

        APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

      ENDLOOP.
    ENDLOOP.

    TRY.
        MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
        MESSAGE s013(zmm01). "DB Update System Error !
    ENDTRY.

    TRY.
        MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
        MESSAGE s013(zmm01). "DB Update System Error !
    ENDTRY.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KGC_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM po_info_kgc_wms.

*  READ TABLE gt_idheader WITH KEY zwerks = gc_werks_2001 TRANSPORTING NO FIELDS.
  READ TABLE gt_poitem WITH KEY werks = gc_werks_2001 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    PERFORM po_info_kgc_wms_2001. "부여
  ENDIF.

*  READ TABLE gt_idheader WITH KEY zwerks = gc_werks_2002 TRANSPORTING NO FIELDS.
  READ TABLE gt_poitem WITH KEY werks = gc_werks_2002 TRANSPORTING NO FIELDS.

  IF sy-subrc = 0.
    PERFORM po_info_kgc_wms_2002. "원주
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KGC_WMS_2001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM po_info_kgc_wms_2001.

  DATA : ls_poheader_kgc1    TYPE zsmm_po_wmsheader_kgc,
         ls_poheader_kgc1_if TYPE zsmm_po_wmsheader_kgc,
         lt_poheader_kgc1_if TYPE TABLE OF zsmm_po_wmsheader_kgc,

         ls_poitem_kgc1      TYPE zsmm_po_wmsitem_kgc,
         ls_poitem_kgc1_if   TYPE zsmm_po_wmsitem_kgc,
         lt_poitem_kgc1_if   TYPE TABLE OF zsmm_po_wmsitem_kgc,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_kgc1_if, lt_poitem_kgc1_if.
*-

  IF gt_poheader[] IS NOT INITIAL.

    "2001- 부여
*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE werks = gc_werks_2001.

      CLEAR : ls_poitem_kgc1, ls_poitem_kgc1_if.

      MOVE-CORRESPONDING <ls_poitem> TO ls_poitem_kgc1.

      IF ls_poitem_kgc1-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_kgc1-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_kgc1-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_kgc1 ls_poitem_kgc1_if.

      ls_poitem_kgc1_if-zgrmenge = ''.

      APPEND ls_poitem_kgc1_if TO lt_poitem_kgc1_if.

      IF lt_poitem_kgc1_if[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
        READ TABLE gt_poheader INTO DATA(ls_poheader) WITH KEY ebeln = <ls_poitem>-ebeln.

        IF ls_poheader IS NOT INITIAL.

          CLEAR : ls_poheader_kgc1, ls_poheader_kgc1_if.

          MOVE-CORRESPONDING ls_poheader TO ls_poheader_kgc1.
          ls_poheader_kgc1-aedat = ls_poheader-zaedat.

*          _g_conv_strc_sap_to_eai ls_poheader_kgc1 ls_poheader_kgc1_if.

          IF ls_poheader-zflag = 'D'.
            ls_poheader_kgc1_if-loekz = 'L'.
          ELSE.
            ls_poheader_kgc1_if-loekz = ''.
          ENDIF.

          IF ls_poheader_kgc1_if-lifnr IS NOT INITIAL.
            CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
              EXPORTING
                iv_bp = ls_poheader_kgc1_if-lifnr
              IMPORTING
                ev_bp = ls_poheader_kgc1_if-lifnr.
          ENDIF.

          APPEND ls_poheader_kgc1_if TO lt_poheader_kgc1_if.

          CLEAR : ls_poheader.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_poheader_kgc1_if COMPARING ALL FIELDS.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0075.                      "MM-0075
  gs_out_header-additional_info = gc_info_kgc_2001.         "KGC WMS-부여

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES70' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_KGC_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_kgc1_if
      it_poitem   = lt_poitem_kgc1_if.


*> HEADER 이력 MODIFY
  LOOP AT gt_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

      ls_ztmm40304-zdest = '2'.

      IF gs_in_header-rst_cd = gc_cd_9999.

        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.

        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

    ENDIF.
  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> ITEM 이력 MODIFY
  LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE werks = gc_werks_2001.

    MOVE-CORRESPONDING ls_poitem TO ls_ztmm40305.

    ls_ztmm40305-matnr = ls_poitem-matnr.
    ls_ztmm40305-bwtar = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                         ebelp = <ls_poitem>-ebelp BINARY SEARCH.

    IF sy-subrc = 0.

      ls_ztmm40305-loekz = ls_40304-loekz.
      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KGC_WMS_2002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM po_info_kgc_wms_2002.
  " KGC1 - 부여플랜트 , KGC2 - 원주플랜트

  DATA : ls_poheader_kgc2    TYPE zsmm_po_wmsheader_kgc,
         ls_poheader_kgc2_if TYPE zsmm_po_wmsheader_kgc,
         lt_poheader_kgc2_if TYPE TABLE OF zsmm_po_wmsheader_kgc,

         ls_poitem_kgc2      TYPE zsmm_po_wmsitem_kgc,
         ls_poitem_kgc2_if   TYPE zsmm_po_wmsitem_kgc,
         lt_poitem_kgc2_if   TYPE TABLE OF zsmm_po_wmsitem_kgc,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_kgc2_if, lt_poitem_kgc2_if.
*-

  IF gt_poheader[] IS NOT INITIAL.

    "2002- 원주
*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT gt_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE werks = gc_werks_2002.

      CLEAR : ls_poitem_kgc2, ls_poitem_kgc2_if.

      MOVE-CORRESPONDING <ls_poitem> TO ls_poitem_kgc2.

      IF ls_poitem_kgc2-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_kgc2-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_kgc2-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_kgc2 ls_poitem_kgc2_if.

      ls_poitem_kgc2_if-zgrmenge = ''.

      APPEND ls_poitem_kgc2_if TO lt_poitem_kgc2_if.

      IF lt_poitem_kgc2_if[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
        READ TABLE gt_poheader INTO DATA(ls_poheader) WITH KEY ebeln = <ls_poitem>-ebeln.

        IF ls_poheader IS NOT INITIAL.

          CLEAR : ls_poheader_kgc2, ls_poheader_kgc2_if.

          MOVE-CORRESPONDING ls_poheader TO ls_poheader_kgc2.
          ls_poheader_kgc2-aedat = ls_poheader-zaedat.

*          _g_conv_strc_sap_to_eai ls_poheader_kgc2 ls_poheader_kgc2_if.

          IF ls_poheader-zflag = 'D'.
            ls_poheader_kgc2_if-loekz = 'L'.
          ELSE.
            ls_poheader_kgc2_if-loekz = ''.
          ENDIF.


          IF ls_poheader_kgc2_if-lifnr IS NOT INITIAL.
            CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
              EXPORTING
                iv_bp = ls_poheader_kgc2_if-lifnr
              IMPORTING
                ev_bp = ls_poheader_kgc2_if-lifnr.
          ENDIF.

          APPEND ls_poheader_kgc2_if TO lt_poheader_kgc2_if.

          CLEAR : ls_poheader.

        ENDIF.
      ENDIF.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_poheader_kgc2_if COMPARING ALL FIELDS.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0076.                      "MM-0076
  gs_out_header-additional_info = gc_info_kgc_2002.         "KGC WMS-원주

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES80' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_KGC_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_kgc2_if
      it_poitem   = lt_poitem_kgc2_if.

*> HEADER 이력 MODIFY
  LOOP AT gt_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    READ TABLE gt_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln.
    IF sy-subrc = 0.

      MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

      ls_ztmm40304-zdest = '3'.
      IF gs_in_header-rst_cd = gc_cd_9999.

        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.

        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

    ENDIF.
  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> HEADER 이력 MODIFY
  LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE werks = gc_werks_2002.

    MOVE-CORRESPONDING ls_poitem TO ls_ztmm40305.

    ls_ztmm40305-matnr = ls_poitem-matnr.
    ls_ztmm40305-bwtar = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = ls_poitem-ebeln
                                                         ebelp = ls_poitem-ebelp BINARY SEARCH.

    IF sy-subrc = 0.

      ls_ztmm40305-loekz = ls_40304-loekz.
      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KTNG_WMS_S
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM ID_INFO_KTNG_WMS_S.
*
*  DATA : LS_INBHEADER_KTNG    TYPE ZSMM_INB_HEADER_KTNG,
*         LS_INBHEADER_KTNG_IF TYPE ZSMM_INB_HEADER_KTNG,
*         LT_INBHEADER_KTNG_IF TYPE TABLE OF ZSMM_INB_HEADER_KTNG,
*
*         LS_INBITEM_KTNG      TYPE ZSMM_INB_ITEM,
*         LS_INBITEM_KTNG_IF   TYPE ZSMM_INB_ITEM,
*         LT_INBITEM_KTNG_IF   TYPE TABLE OF ZSMM_INB_ITEM,
*
*         LS_ZTMM40300         TYPE ZTMM40300,
*         LT_ZTMM40300         TYPE TABLE OF ZTMM40300,
*         LS_ZTMM40301         TYPE ZTMM40301,
*         LT_ZTMM40301         TYPE TABLE OF ZTMM40301.
*
**-
*  CLEAR : GS_OUT_HEADER, GS_IN_HEADER.
*  _G_INIT: LT_INBHEADER_KTNG_IF, LT_INBITEM_KTNG_IF.
**-
*
*  IF GT_IDHEADER[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT GT_IDHEADER ASSIGNING FIELD-SYMBOL(<LS_IDHEADER>).
*
*      CLEAR : LS_INBHEADER_KTNG, LS_INBHEADER_KTNG_IF.
*
*      IF <LS_IDHEADER>-ZFLAG = 'D'.
*        <LS_IDHEADER>-LOEKZ = 'L'.
*      ELSE.
*        <LS_IDHEADER>-LOEKZ = ''.
*      ENDIF.
*
*      MOVE-CORRESPONDING <LS_IDHEADER> TO LS_INBHEADER_KTNG.
*      _G_CONV_STRC_SAP_TO_EAI LS_INBHEADER_KTNG LS_INBHEADER_KTNG_IF.
*
*      IF LS_INBHEADER_KTNG_IF-LIFNR IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            IV_BP = LS_INBHEADER_KTNG_IF-LIFNR
*          IMPORTING
*            EV_BP = LS_INBHEADER_KTNG_IF-LIFNR.
*      ENDIF.
*
*      APPEND LS_INBHEADER_KTNG_IF TO LT_INBHEADER_KTNG_IF.
*
*    ENDLOOP.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*    LOOP AT GT_IDITEM ASSIGNING FIELD-SYMBOL(<LS_IDITEM>) WHERE ZFLAG NE ''.
*
*      CLEAR : LS_INBITEM_KTNG, LS_INBITEM_KTNG_IF.
*
*      MOVE-CORRESPONDING <LS_IDITEM> TO LS_INBITEM_KTNG.
*
*      _G_CONV_STRC_SAP_TO_EAI LS_INBITEM_KTNG LS_INBITEM_KTNG_IF.
*
*      APPEND LS_INBITEM_KTNG_IF TO LT_INBITEM_KTNG_IF.
*
*
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  GS_OUT_HEADER-IF_ID = GC_IF_ID_0069.                      "MM-0074
*  GS_OUT_HEADER-ADDITIONAL_INFO = GC_INFO_KTNG. "KTNG WMS
*
**> EAI LOG START
*  ZCL_CN_ABAP_UTIL=>GET_EAI_START( CHANGING  CS_HEADER = GS_OUT_HEADER ).
*
*  DATA(LV_RFCDEST) = ZCL_CN_ABAP_UTIL=>GET_DESTINATION( IV_SYSID_S = SY-SYSID
*                                                        IV_SYSID_T = 'KTG20' ).
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_KTNG_WMS' DESTINATION LV_RFCDEST
*    EXPORTING
*      IS_HEADER    = GS_OUT_HEADER
*    IMPORTING
*      ES_HEADER    = GS_IN_HEADER
*    TABLES
*      IT_INBHEADER = LT_INBHEADER_KTNG_IF
*      IT_INBITEM   = LT_INBITEM_KTNG_IF.
*
**> HEADER 이력 MODIFY
*  LOOP AT GT_IDHEADER INTO DATA(LS_IDHEADER).
*
*    MOVE-CORRESPONDING LS_IDHEADER TO LS_ZTMM40300.
*
*    IF GS_IN_HEADER-RST_CD = GC_CD_9999.
*
*      LS_ZTMM40300-ZSTATUS = 'F'.
*      LS_ZTMM40300-ZMESSAGE = GC_MSG_INB_FAIL && '(' && GS_IN_HEADER-RST_MSG && ')'.
*      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      LS_ZTMM40300-ZWMSIFID = GS_IN_HEADER-IF_ID.
*      LS_ZTMM40300-ZWMSTRCID = GS_IN_HEADER-IF_TRC_ID.
*
*      LS_ZTMM40300-ZSTATUS = 'X'.
*      LS_ZTMM40300-ZTFLAG = 'X'.
*      LS_ZTMM40300-ZMESSAGE = GC_MSG_INB_SUCCESS.
*      MESSAGE S000 DISPLAY LIKE 'S' WITH TEXT-M08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*    IF LS_IDHEADER-ERDAT NE GC_00000000.
*      LS_ZTMM40300-ERDAT = LS_IDHEADER-ERDAT.
*      LS_ZTMM40300-ERZET = LS_IDHEADER-ERZET.
*      LS_ZTMM40300-ERNAM = LS_IDHEADER-ERNAM.
*    ELSE.
*      LS_ZTMM40300-ERDAT = SY-DATUM.
*      LS_ZTMM40300-ERZET = SY-UZEIT.
*      LS_ZTMM40300-ERNAM = SY-UNAME.
*    ENDIF.
*
*    LS_ZTMM40300-AEDAT = SY-DATUM.
*    LS_ZTMM40300-AEZET = SY-UZEIT.
*    LS_ZTMM40300-AENAM = SY-UNAME.
*
*    APPEND LS_ZTMM40300 TO LT_ZTMM40300. CLEAR LS_ZTMM40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ZTMM40300 FROM TABLE LT_ZTMM40300.
*      IF SY-SUBRC = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE S002(ZCN00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH CX_SY_OPEN_SQL_DB INTO DATA(LO_OREF).
*      MESSAGE S013(ZMM01). "DB Update System Error !
*  ENDTRY.
*
*
**> ITEM 이력 MODIFY
*  LOOP AT GT_IDITEM INTO DATA(LS_IDITEM).
*
*    MOVE-CORRESPONDING LS_IDITEM TO LS_ZTMM40301.
*
*    IF GS_IN_HEADER-RST_CD = GC_CD_9999.
*      LS_ZTMM40301-ZTFLAG = ''.
*    ELSE.
*      LS_ZTMM40301-ZTFLAG = 'X'.
*    ENDIF.
*
*    IF LS_IDITEM-ERDAT NE GC_00000000.
*      LS_ZTMM40301-ERDAT = LS_IDITEM-ERDAT.
*      LS_ZTMM40301-ERZET = LS_IDITEM-ERZET.
*      LS_ZTMM40301-ERNAM = LS_IDITEM-ERNAM.
*    ELSE.
*      LS_ZTMM40301-ERDAT = SY-DATUM.
*      LS_ZTMM40301-ERZET = SY-UZEIT.
*      LS_ZTMM40301-ERNAM = SY-UNAME.
*    ENDIF.
*
*    LS_ZTMM40301-AEDAT = SY-DATUM.
*    LS_ZTMM40301-AEZET = SY-UZEIT.
*    LS_ZTMM40301-AENAM = SY-UNAME.
*
*    APPEND LS_ZTMM40301 TO LT_ZTMM40301. CLEAR LS_ZTMM40301.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ZTMM40301 FROM TABLE LT_ZTMM40301.
*      IF SY-SUBRC = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE S002(ZCN00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH CX_SY_OPEN_SQL_DB INTO DATA(LO_OREF2).
*      MESSAGE S013(ZMM01). "DB Update System Error !
*  ENDTRY.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_YJP_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_yjp_wms .

*  DATA : ls_inbheader_yjp    TYPE zsmm_inb_header,
*         ls_inbheader_yjp_if TYPE zsmm_inb_header,
*         lt_inbheader_yjp_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_yjp      TYPE zsmm_inb_item_yjp,
*         ls_inbitem_yjp_if   TYPE zsmm_inb_item_yjp,
*         lt_inbitem_yjp_if   TYPE TABLE OF zsmm_inb_item_yjp,
*
*         ls_batchc_yjp       TYPE zsmm_inb_batch,
*         ls_batchc_yjp_if    TYPE zsmm_inb_batch,
*         lt_batchc_yjp_if    TYPE TABLE OF zsmm_inb_batch,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
*  _g_init : lt_ztmm40300, lt_ztmm40301.
*
*  IF gt_idheader[] IS NOT INITIAL.
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>).
*
*      CLEAR : ls_inbheader_yjp, ls_inbheader_yjp_if.
*      _g_init: lt_inbheader_yjp_if, lt_inbitem_yjp_if, lt_batchc_yjp_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_yjp.
*      _g_conv_strc_sap_to_eai ls_inbheader_yjp ls_inbheader_yjp_if.
*
*      IF ls_inbheader_yjp_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_yjp_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_yjp_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_yjp_if TO lt_inbheader_yjp_if.
**--------------------------------------------------------------------*
**> ITEM/BATCH DATA SET
**--------------------------------------------------------------------*
*      LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_yjp, ls_inbitem_yjp_if, ls_batchc_yjp, ls_batchc_yjp_if.
*
**> ITEM
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_yjp.
*
*        IF ls_inbitem_yjp-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_yjp-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_yjp-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_yjp ls_inbitem_yjp_if.
*
*        APPEND ls_inbitem_yjp_if TO lt_inbitem_yjp_if.
*
**> BATCH
*        MOVE-CORRESPONDING <ls_iditem> TO ls_batchc_yjp.
*
*        _g_conv_strc_sap_to_eai ls_batchc_yjp ls_batchc_yjp_if.
*
*        APPEND ls_batchc_yjp_if TO lt_batchc_yjp_if.
*
*      ENDLOOP.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
**-
*      CLEAR : gs_out_header, gs_in_header.
**-
*
*      gs_out_header-if_id = gc_if_id_0072.                  "MM-0072
*      gs_out_header-additional_info = gc_info_yjp.  "YJP WMS
*
**> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'YJP10' ).
*
*
*      CALL FUNCTION 'ZFIMM_ID_INFO_YJP_WMS' DESTINATION lv_rfcdest
*        EXPORTING
*          is_header    = gs_out_header
*        IMPORTING
*          es_header    = gs_in_header
*        TABLES
*          it_inbheader = lt_inbheader_yjp_if
*          it_inbitem   = lt_inbitem_yjp_if
*          it_batchc    = lt_batchc_yjp_if.
*
**> HEADER 이력 MODIFY
*      CLEAR ls_ztmm40300.
*      MOVE-CORRESPONDING <ls_idheader> TO ls_ztmm40300.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*
*        ls_ztmm40300-zstatus = 'F'.
*        ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*      ELSE.
*        ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*        ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*        ls_ztmm40300-zstatus = 'X'.
*        ls_ztmm40300-ztflag = 'X'.
*        ls_ztmm40300-zmessage = gc_msg_inb_success.
*        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*      ENDIF.
*
*      IF <ls_idheader>-erdat NE gc_00000000.
*        ls_ztmm40300-erdat = <ls_idheader>-erdat.
*        ls_ztmm40300-erzet = <ls_idheader>-erzet.
*        ls_ztmm40300-ernam = <ls_idheader>-ernam.
*      ELSE.
*        ls_ztmm40300-erdat = sy-datum.
*        ls_ztmm40300-erzet = sy-uzeit.
*        ls_ztmm40300-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40300-aedat = sy-datum.
*      ls_ztmm40300-aezet = sy-uzeit.
*      ls_ztmm40300-aenam = sy-uname.
*
*      APPEND ls_ztmm40300 TO lt_ztmm40300.
*
**> ITEM 이력 MODIFY
*      LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR ls_ztmm40301.
*
*        READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*        IF sy-subrc = 0.
*
*          MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*          IF gs_in_header-rst_cd = gc_cd_9999.
*            ls_ztmm40301-ztflag = ''.
*          ELSE.
*            ls_ztmm40301-ztflag = 'X'.
*          ENDIF.
*
*          IF ls_iditem-erdat NE gc_00000000.
*            ls_ztmm40301-erdat = ls_iditem-erdat.
*            ls_ztmm40301-erzet = ls_iditem-erzet.
*            ls_ztmm40301-ernam = ls_iditem-ernam.
*          ELSE.
*            ls_ztmm40301-erdat = sy-datum.
*            ls_ztmm40301-erzet = sy-uzeit.
*            ls_ztmm40301-ernam = sy-uname.
*          ENDIF.
*
*          ls_ztmm40301-aedat = sy-datum.
*          ls_ztmm40301-aezet = sy-uzeit.
*          ls_ztmm40301-aenam = sy-uname.
*
*          APPEND ls_ztmm40301 TO lt_ztmm40301.
*
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    TRY.
*        MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*    TRY.
*        MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms .

  READ TABLE gt_idheader WITH KEY zwerks = gc_werks_2001 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    PERFORM id_info_kgc_wms_2001. "부여
  ENDIF.

  READ TABLE gt_idheader WITH KEY zwerks = gc_werks_2002 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    PERFORM id_info_kgc_wms_2002. "원주
  ENDIF.

  READ TABLE gt_idheader WITH KEY zwerks = gc_werks_2050 TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    PERFORM id_info_kgc_wms_2050. "3PL
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2001 .

*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_wms,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_wms,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_wms,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
*  _g_init : lt_ztmm40300, lt_ztmm40301.
*
*  IF gt_idheader[] IS NOT INITIAL.
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2001.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*      _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if.
*
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
*      ENDLOOP.
*
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
**-
*      CLEAR : gs_out_header, gs_in_header.
**-
*
*      gs_out_header-if_id = gc_if_id_0070.                  "MM-0070
*      gs_out_header-additional_info = gc_info_kgc_2001.     "KGC WMS-부여
*
**> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'MES70' ).
*
*
*      CALL FUNCTION 'ZFIMM_ID_INFO_KGC_WMS' DESTINATION lv_rfcdest
*        EXPORTING
*          is_header    = gs_out_header
*        IMPORTING
*          es_header    = gs_in_header
*        TABLES
*          it_inbheader = lt_inbheader_kgc_if
*          it_inbitem   = lt_inbitem_kgc_if.
*
**> HEADER 이력 MODIFY
*      CLEAR ls_ztmm40300.
*      MOVE-CORRESPONDING <ls_idheader> TO ls_ztmm40300.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*
*        ls_ztmm40300-zstatus = 'F'.
*        ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*      ELSE.
*        ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*        ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*        ls_ztmm40300-zstatus = 'X'.
*        ls_ztmm40300-ztflag = 'X'.
*        ls_ztmm40300-zmessage = gc_msg_inb_success.
*        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*      ENDIF.
*
*      IF <ls_idheader>-erdat NE gc_00000000.
*        ls_ztmm40300-erdat = <ls_idheader>-erdat.
*        ls_ztmm40300-erzet = <ls_idheader>-erzet.
*        ls_ztmm40300-ernam = <ls_idheader>-ernam.
*      ELSE.
*        ls_ztmm40300-erdat = sy-datum.
*        ls_ztmm40300-erzet = sy-uzeit.
*        ls_ztmm40300-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40300-aedat = sy-datum.
*      ls_ztmm40300-aezet = sy-uzeit.
*      ls_ztmm40300-aenam = sy-uname.
*
*      APPEND ls_ztmm40300 TO lt_ztmm40300.
*
**> ITEM 이력 MODIFY
*      LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR ls_ztmm40301.
*        READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*        IF sy-subrc = 0.
*
*          MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*          IF gs_in_header-rst_cd = gc_cd_9999.
*            ls_ztmm40301-ztflag = ''.
*          ELSE.
*            ls_ztmm40301-ztflag = 'X'.
*          ENDIF.
*
*          IF ls_iditem-erdat NE gc_00000000.
*            ls_ztmm40301-erdat = ls_iditem-erdat.
*            ls_ztmm40301-erzet = ls_iditem-erzet.
*            ls_ztmm40301-ernam = ls_iditem-ernam.
*          ELSE.
*            ls_ztmm40301-erdat = sy-datum.
*            ls_ztmm40301-erzet = sy-uzeit.
*            ls_ztmm40301-ernam = sy-uname.
*          ENDIF.
*
*          ls_ztmm40301-aedat = sy-datum.
*          ls_ztmm40301-aezet = sy-uzeit.
*          ls_ztmm40301-aenam = sy-uname.
*
*          APPEND ls_ztmm40301 TO lt_ztmm40301.
*
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    TRY.
*        MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*    TRY.
*        MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2002 .

*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_wms,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_wms,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_wms,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
*  _g_init : lt_ztmm40300, lt_ztmm40301.
*
*  IF gt_idheader[] IS NOT INITIAL.
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2002.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*      _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if.
*
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
*      ENDLOOP.
*
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
**-
*      CLEAR : gs_out_header, gs_in_header.
**-
*
*      gs_out_header-if_id = gc_if_id_0100.                  "MM-0100
*      gs_out_header-additional_info = gc_info_kgc_2002.     "KGC WMS 원주
*
**> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'MES80' ).
*
*
*      CALL FUNCTION 'ZFIMM_ID_INFO_KGC_WMS' DESTINATION lv_rfcdest
*        EXPORTING
*          is_header    = gs_out_header
*        IMPORTING
*          es_header    = gs_in_header
*        TABLES
*          it_inbheader = lt_inbheader_kgc_if
*          it_inbitem   = lt_inbitem_kgc_if.
*
**> HEADER 이력 MODIFY
*      CLEAR ls_ztmm40300.
*      MOVE-CORRESPONDING <ls_idheader> TO ls_ztmm40300.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*
*        ls_ztmm40300-zstatus = 'F'.
*        ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*      ELSE.
*        ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*        ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*        ls_ztmm40300-zstatus = 'X'.
*        ls_ztmm40300-ztflag = 'X'.
*        ls_ztmm40300-zmessage = gc_msg_inb_success.
*        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*      ENDIF.
*
*      IF <ls_idheader>-erdat NE gc_00000000.
*        ls_ztmm40300-erdat = <ls_idheader>-erdat.
*        ls_ztmm40300-erzet = <ls_idheader>-erzet.
*        ls_ztmm40300-ernam = <ls_idheader>-ernam.
*      ELSE.
*        ls_ztmm40300-erdat = sy-datum.
*        ls_ztmm40300-erzet = sy-uzeit.
*        ls_ztmm40300-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40300-aedat = sy-datum.
*      ls_ztmm40300-aezet = sy-uzeit.
*      ls_ztmm40300-aenam = sy-uname.
*
*      APPEND ls_ztmm40300 TO lt_ztmm40300.
*
**> ITEM 이력 MODIFY
*      LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR ls_ztmm40301.
*        READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*        IF sy-subrc = 0.
*
*          MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*          IF gs_in_header-rst_cd = gc_cd_9999.
*            ls_ztmm40301-ztflag = ''.
*          ELSE.
*            ls_ztmm40301-ztflag = 'X'.
*          ENDIF.
*
*          IF ls_iditem-erdat NE gc_00000000.
*            ls_ztmm40301-erdat = ls_iditem-erdat.
*            ls_ztmm40301-erzet = ls_iditem-erzet.
*            ls_ztmm40301-ernam = ls_iditem-ernam.
*          ELSE.
*            ls_ztmm40301-erdat = sy-datum.
*            ls_ztmm40301-erzet = sy-uzeit.
*            ls_ztmm40301-ernam = sy-uname.
*          ENDIF.
*
*          ls_ztmm40301-aedat = sy-datum.
*          ls_ztmm40301-aezet = sy-uzeit.
*          ls_ztmm40301-aenam = sy-uname.
*
*          APPEND ls_ztmm40301 TO lt_ztmm40301.
*
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    TRY.
*        MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*    TRY.
*        MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2050
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2050 .

*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_wms,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_wms,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_wms,
*
*         ls_batchc_kgc       TYPE zsmm_inb_batch_kgc,
*         ls_batchc_kgc_if    TYPE zsmm_inb_batch_kgc,
*         lt_batchc_kgc_if    TYPE TABLE OF zsmm_inb_batch_kgc,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
*  _g_init : lt_ztmm40300, lt_ztmm40301.
*
*  IF gt_idheader[] IS NOT INITIAL.
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2050.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*      _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if, lt_batchc_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if, ls_batchc_kgc, ls_batchc_kgc_if.
*
**> ITEM
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
*
**> BATCH
*        MOVE-CORRESPONDING <ls_iditem> TO ls_batchc_kgc.
*
*        _g_conv_strc_sap_to_eai ls_batchc_kgc ls_batchc_kgc_if.
*
*        APPEND ls_batchc_kgc_if TO lt_batchc_kgc_if.
*
*      ENDLOOP.
*
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
**-
*      CLEAR : gs_out_header, gs_in_header.
**-
*
*      gs_out_header-if_id = gc_if_id_0071.                  "MM-0071
*      gs_out_header-additional_info = gc_info_kgc_2050.     "KGC 3PL
*
**> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'KGC20' ).
*
*
*      SORT lt_batchc_kgc_if BY  matnr charg hsdat lichn vfdat zmaker.
*      DELETE ADJACENT DUPLICATES FROM  lt_batchc_kgc_if
*      COMPARING  matnr charg hsdat lichn vfdat zmaker.
*
*      CALL FUNCTION 'ZFIMM_ID_INFO_KGC_3PL' DESTINATION lv_rfcdest
*        EXPORTING
*          is_header    = gs_out_header
*        IMPORTING
*          es_header    = gs_in_header
*        TABLES
*          it_inbheader = lt_inbheader_kgc_if
*          it_inbitem   = lt_inbitem_kgc_if
*          it_batchc    = lt_batchc_kgc_if.
*
**> HEADER 이력 MODIFY
*      CLEAR ls_ztmm40300.
*      MOVE-CORRESPONDING <ls_idheader> TO ls_ztmm40300.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*
*        ls_ztmm40300-zstatus = 'F'.
*        ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*      ELSE.
*        ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*        ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*        ls_ztmm40300-zstatus = 'X'.
*        ls_ztmm40300-ztflag = 'X'.
*        ls_ztmm40300-zmessage = gc_msg_inb_success.
*        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*      ENDIF.
*
*      IF <ls_idheader>-erdat NE gc_00000000.
*        ls_ztmm40300-erdat = <ls_idheader>-erdat.
*        ls_ztmm40300-erzet = <ls_idheader>-erzet.
*        ls_ztmm40300-ernam = <ls_idheader>-ernam.
*      ELSE.
*        ls_ztmm40300-erdat = sy-datum.
*        ls_ztmm40300-erzet = sy-uzeit.
*        ls_ztmm40300-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40300-aedat = sy-datum.
*      ls_ztmm40300-aezet = sy-uzeit.
*      ls_ztmm40300-aenam = sy-uname.
*
*      APPEND ls_ztmm40300 TO lt_ztmm40300.
*
**> ITEM 이력 MODIFY
*      LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR ls_ztmm40301.
*        READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*        IF sy-subrc = 0.
*
*          MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*          IF gs_in_header-rst_cd = gc_cd_9999.
*            ls_ztmm40301-ztflag = ''.
*          ELSE.
*            ls_ztmm40301-ztflag = 'X'.
*          ENDIF.
*
*          IF ls_iditem-erdat NE gc_00000000.
*            ls_ztmm40301-erdat = ls_iditem-erdat.
*            ls_ztmm40301-erzet = ls_iditem-erzet.
*            ls_ztmm40301-ernam = ls_iditem-ernam.
*          ELSE.
*            ls_ztmm40301-erdat = sy-datum.
*            ls_ztmm40301-erzet = sy-uzeit.
*            ls_ztmm40301-ernam = sy-uname.
*          ENDIF.
*
*          ls_ztmm40301-aedat = sy-datum.
*          ls_ztmm40301-aezet = sy-uzeit.
*          ls_ztmm40301-aenam = sy-uname.
*
*          APPEND ls_ztmm40301 TO lt_ztmm40301.
*
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    TRY.
*        MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*    TRY.
*        MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_PROCESS_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM btn_on_process_trans .

  DATA: lt_idheader TYPE TABLE OF ts_idheader,
        lt_iditem   TYPE TABLE OF ts_iditem.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_selidx).

  IF lt_selidx[] IS INITIAL.
    MESSAGE s000 WITH '예정 정보 전송대상을 선택하세요.'(m05) DISPLAY LIKE 'E'. EXIT.
  ENDIF.

*> 전송대상 추출

  LOOP AT lt_selidx INTO DATA(ls_selidx).

    READ TABLE gt_idheader INTO DATA(ls_idheader) INDEX ls_selidx-index.
    IF sy-subrc = 0.

      APPEND ls_idheader TO lt_idheader.

      LOOP AT gt_iditem INTO DATA(ls_iditem) WHERE vbeln = ls_idheader-vbeln.

        APPEND ls_iditem TO lt_iditem. CLEAR ls_iditem.
      ENDLOOP.
    ENDIF.

    CLEAR ls_idheader.

  ENDLOOP.

  CHECK grf_grid->pop_to_msg( iv_type  = 'A'
                              iv_title = CONV string( TEXT-u01 )      "예정 정보 전송
                              iv_text1 = CONV #( '납품서 정보를 전송합니다.' )
                              iv_text2 = CONV #( '전송하시겠습니까?' ) )
                              EQ abap_true. " YES
  CASE p_bukrs.
    WHEN gc_1101.

      PERFORM id_info_ktng_wms_trans TABLES lt_idheader lt_iditem.

    WHEN gc_2101.

      READ TABLE lt_idheader WITH KEY zwerks = gc_werks_2001 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        PERFORM id_info_kgc_wms_2001_trans TABLES lt_idheader lt_iditem.
      ENDIF.

      READ TABLE lt_idheader WITH KEY zwerks = gc_werks_2002 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        PERFORM id_info_kgc_wms_2002_trans TABLES lt_idheader lt_iditem.
      ENDIF.

      READ TABLE lt_idheader WITH KEY zwerks = gc_werks_2050 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        PERFORM id_info_kgc_wms_2050_trans TABLES lt_idheader lt_iditem.
      ENDIF.

    WHEN gc_3101.

      PERFORM id_info_yjp_wms_trans TABLES lt_idheader lt_iditem.

  ENDCASE.

* REFRESH DISPLAY
  PERFORM get_data_inb.
  grf_grid->refresh_grid_display( ).



ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KTNG_WMS_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_IDHEADER
*&      --> LT_IDITEM
*&---------------------------------------------------------------------*
FORM id_info_ktng_wms_trans  TABLES it_idheader TYPE tt_idheader
                                    it_iditem TYPE tt_iditem.


*  DATA : ls_inbheader_ktng    TYPE zsmm_inb_header_ktng,
*         ls_inbheader_ktng_if TYPE zsmm_inb_header_ktng,
*         lt_inbheader_ktng_if TYPE TABLE OF zsmm_inb_header_ktng,
*
*         ls_inbitem_ktng      TYPE zsmm_inb_item,
*         ls_inbitem_ktng_if   TYPE zsmm_inb_item,
*         lt_inbitem_ktng_if   TYPE TABLE OF zsmm_inb_item,
*
*         ls_ztmm40300         TYPE ztmm40300,
*         lt_ztmm40300         TYPE TABLE OF ztmm40300,
*         ls_ztmm40301         TYPE ztmm40301,
*         lt_ztmm40301         TYPE TABLE OF ztmm40301.
*
**-
*  CLEAR : gs_out_header, gs_in_header.
*  _g_init: lt_inbheader_ktng_if, lt_inbitem_ktng_if.
**-
*
*  IF it_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>).
*
*      CLEAR : ls_inbheader_ktng, ls_inbheader_ktng_if.
*
*      IF <ls_idheader>-zflag = 'D'.
*        <ls_idheader>-loekz = 'L'.
*      ELSE.
*        <ls_idheader>-loekz = ''.
*      ENDIF.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_ktng.
*      _g_conv_strc_sap_to_eai ls_inbheader_ktng ls_inbheader_ktng_if.
*
*      IF ls_inbheader_ktng_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_ktng_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_ktng_if-lifnr.
*      ENDIF.
*
*
*      APPEND ls_inbheader_ktng_if TO lt_inbheader_ktng_if.
*
*    ENDLOOP.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) .
*      "WHERE ZFLAG NE ''. "삭제 정보 전송을 위한 주석처리
*
*      CLEAR : ls_inbitem_ktng, ls_inbitem_ktng_if.
*
*      MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_ktng.
*
*      IF ls_inbitem_ktng-vrkme IS NOT INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            input          = ls_inbitem_ktng-vrkme
*            language       = sy-langu
*          IMPORTING
*            output         = ls_inbitem_ktng-vrkme
*          EXCEPTIONS
*            unit_not_found = 1
*            OTHERS         = 2.
*      ENDIF.
*
*      _g_conv_strc_sap_to_eai ls_inbitem_ktng ls_inbitem_ktng_if.
*
*      APPEND ls_inbitem_ktng_if TO lt_inbitem_ktng_if.
*
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  gs_out_header-if_id = gc_if_id_0069.                      "MM-0074
*  gs_out_header-additional_info = gc_info_ktng. "KTNG WMS
*
**> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'KTG20' ).
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_KTNG_WMS' DESTINATION lv_rfcdest
*    EXPORTING
*      is_header    = gs_out_header
*    IMPORTING
*      es_header    = gs_in_header
*    TABLES
*      it_inbheader = lt_inbheader_ktng_if
*      it_inbitem   = lt_inbitem_ktng_if.
*
**> HEADER 이력 MODIFY
*  LOOP AT it_idheader INTO DATA(ls_idheader).
*
*    MOVE-CORRESPONDING ls_idheader TO ls_ztmm40300.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*      ls_ztmm40300-zstatus = 'F'.
*      ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*      ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*      ls_ztmm40300-zstatus = 'X'.
*      ls_ztmm40300-ztflag = 'X'.
*      ls_ztmm40300-zmessage = gc_msg_inb_success.
*      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*
*    IF ls_idheader-erdat NE gc_00000000.
*      ls_ztmm40300-erdat = ls_idheader-erdat.
*      ls_ztmm40300-erzet = ls_idheader-erzet.
*      ls_ztmm40300-ernam = ls_idheader-ernam.
*    ELSE.
*      ls_ztmm40300-erdat = sy-datum.
*      ls_ztmm40300-erzet = sy-uzeit.
*      ls_ztmm40300-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40300-aedat = sy-datum.
*    ls_ztmm40300-aezet = sy-uzeit.
*    ls_ztmm40300-aenam = sy-uname.
*
*    APPEND ls_ztmm40300 TO lt_ztmm40300. CLEAR ls_ztmm40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.
*
**> ITEM 이력 MODIFY
*  LOOP AT it_iditem INTO DATA(ls_iditem).
*
*    MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*      ls_ztmm40301-ztflag = ''.
*    ELSE.
*      ls_ztmm40301-ztflag = 'X'.
*    ENDIF.
*
*    IF ls_iditem-erdat NE gc_00000000.
*      ls_ztmm40301-erdat = ls_iditem-erdat.
*      ls_ztmm40301-erzet = ls_iditem-erzet.
*      ls_ztmm40301-ernam = ls_iditem-ernam.
*    ELSE.
*      ls_ztmm40301-erdat = sy-datum.
*      ls_ztmm40301-erzet = sy-uzeit.
*      ls_ztmm40301-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40301-aedat = sy-datum.
*    ls_ztmm40301-aezet = sy-uzeit.
*    ls_ztmm40301-aenam = sy-uname.
*
*    APPEND ls_ztmm40301 TO lt_ztmm40301. CLEAR ls_ztmm40301.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_YJP_WMS_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_IDHEADER
*&      --> LT_IDITEM
*&---------------------------------------------------------------------*
FORM id_info_yjp_wms_trans  TABLES it_idheader TYPE tt_idheader
                                   it_iditem TYPE tt_iditem.


*  DATA : ls_inbheader_yjp    TYPE zsmm_inb_header,
*         ls_inbheader_yjp_if TYPE zsmm_inb_header,
*         lt_inbheader_yjp_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_yjp      TYPE zsmm_inb_item_yjp,
*         ls_inbitem_yjp_if   TYPE zsmm_inb_item_yjp,
*         lt_inbitem_yjp_if   TYPE TABLE OF zsmm_inb_item_yjp,
*
*         ls_batchc_yjp       TYPE zsmm_inb_batch,
*         ls_batchc_yjp_if    TYPE zsmm_inb_batch,
*         lt_batchc_yjp_if    TYPE TABLE OF zsmm_inb_batch,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
**-
*  CLEAR : gs_out_header, gs_in_header.
*  _g_init: lt_inbheader_yjp_if, lt_inbitem_yjp_if, lt_batchc_yjp_if.
**-
*
*  IF it_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>).
*
*      CLEAR : ls_inbheader_yjp, ls_inbheader_yjp_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_yjp.
*      _g_conv_strc_sap_to_eai ls_inbheader_yjp ls_inbheader_yjp_if.
*
*      IF ls_inbheader_yjp_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_yjp_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_yjp_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_yjp_if TO lt_inbheader_yjp_if.
*
*    ENDLOOP.
**--------------------------------------------------------------------*
**> ITEM/BATCH DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>).
*
*      CLEAR : ls_inbitem_yjp, ls_inbitem_yjp_if, ls_batchc_yjp, ls_batchc_yjp_if.
*
**> ITEM
*      MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_yjp.
*
*      IF ls_inbitem_yjp-vrkme IS NOT INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            input          = ls_inbitem_yjp-vrkme
*            language       = sy-langu
*          IMPORTING
*            output         = ls_inbitem_yjp-vrkme
*          EXCEPTIONS
*            unit_not_found = 1
*            OTHERS         = 2.
*      ENDIF.
*
*      _g_conv_strc_sap_to_eai ls_inbitem_yjp ls_inbitem_yjp_if.
*
*      APPEND ls_inbitem_yjp_if TO lt_inbitem_yjp_if.
*
**> BATCH
*      MOVE-CORRESPONDING <ls_iditem> TO ls_batchc_yjp.
*
*      _g_conv_strc_sap_to_eai ls_batchc_yjp ls_batchc_yjp_if.
*
*      APPEND ls_batchc_yjp_if TO lt_batchc_yjp_if.
*
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  gs_out_header-if_id = gc_if_id_0072.                      "MM-0072
*  gs_out_header-additional_info = gc_info_yjp.  "YJP WMS
*
**> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'YJP10' ).
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_YJP_WMS' DESTINATION lv_rfcdest
*    EXPORTING
*      is_header    = gs_out_header
*    IMPORTING
*      es_header    = gs_in_header
*    TABLES
*      it_inbheader = lt_inbheader_yjp_if
*      it_inbitem   = lt_inbitem_yjp_if
*      it_batchc    = lt_batchc_yjp_if.
*
**> HEADER 이력 MODIFY
*  LOOP AT it_idheader INTO DATA(ls_idheader).
*
*    MOVE-CORRESPONDING ls_idheader TO ls_ztmm40300.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*
*      ls_ztmm40300-zstatus = 'F'.
*      ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*      ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*      ls_ztmm40300-zstatus = 'X'.
*      ls_ztmm40300-ztflag = 'X'.
*      ls_ztmm40300-zmessage = gc_msg_inb_success.
*      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*
*    IF ls_idheader-erdat NE gc_00000000.
*      ls_ztmm40300-erdat = ls_idheader-erdat.
*      ls_ztmm40300-erzet = ls_idheader-erzet.
*      ls_ztmm40300-ernam = ls_idheader-ernam.
*    ELSE.
*      ls_ztmm40300-erdat = sy-datum.
*      ls_ztmm40300-erzet = sy-uzeit.
*      ls_ztmm40300-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40300-aedat = sy-datum.
*    ls_ztmm40300-aezet = sy-uzeit.
*    ls_ztmm40300-aenam = sy-uname.
*
*    APPEND ls_ztmm40300 TO lt_ztmm40300. CLEAR ls_ztmm40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.
*
**> ITEM 이력 MODIFY
*
*  LOOP AT it_iditem INTO DATA(ls_iditem).
*
*    MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*      ls_ztmm40301-ztflag = ''.
*    ELSE.
*      ls_ztmm40301-ztflag = 'X'.
*    ENDIF.
*
*    IF ls_iditem-erdat NE gc_00000000.
*      ls_ztmm40301-erdat = ls_iditem-erdat.
*      ls_ztmm40301-erzet = ls_iditem-erzet.
*      ls_ztmm40301-ernam = ls_iditem-ernam.
*    ELSE.
*      ls_ztmm40301-erdat = sy-datum.
*      ls_ztmm40301-erzet = sy-uzeit.
*      ls_ztmm40301-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40301-aedat = sy-datum.
*    ls_ztmm40301-aezet = sy-uzeit.
*    ls_ztmm40301-aenam = sy-uname.
*
*    APPEND ls_ztmm40301 TO lt_ztmm40301. CLEAR ls_ztmm40301.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2001_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_IDHEADER
*&      --> LT_IDITEM
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2001_trans  TABLES it_idheader TYPE tt_idheader
                                        it_iditem TYPE tt_iditem.


*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_wms,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_wms,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_wms,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
**-
*  CLEAR : gs_out_header, gs_in_header.
*  _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if.
**-
*
*  IF it_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2001.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT it_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if.
*
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  gs_out_header-if_id = gc_if_id_0070.                      "MM-0070
*  gs_out_header-additional_info = gc_info_kgc_2001. "KGC WMS
*
**> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES70' ).
*
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_KGC_WMS' DESTINATION lv_rfcdest
*    EXPORTING
*      is_header    = gs_out_header
*    IMPORTING
*      es_header    = gs_in_header
*    TABLES
*      it_inbheader = lt_inbheader_kgc_if
*      it_inbitem   = lt_inbitem_kgc_if.
*
**> HEADER 이력 MODIFY
*  LOOP AT it_idheader INTO DATA(ls_idheader) WHERE zwerks = gc_werks_2001.
*
*    MOVE-CORRESPONDING ls_idheader TO ls_ztmm40300.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*
*      ls_ztmm40300-zstatus = 'F'.
*      ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*      ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*      ls_ztmm40300-zstatus = 'X'.
*      ls_ztmm40300-ztflag = 'X'.
*      ls_ztmm40300-zmessage = gc_msg_inb_success.
*      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*
*    IF ls_idheader-erdat NE gc_00000000.
*      ls_ztmm40300-erdat = ls_idheader-erdat.
*      ls_ztmm40300-erzet = ls_idheader-erzet.
*      ls_ztmm40300-ernam = ls_idheader-ernam.
*    ELSE.
*      ls_ztmm40300-erdat = sy-datum.
*      ls_ztmm40300-erzet = sy-uzeit.
*      ls_ztmm40300-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40300-aedat = sy-datum.
*    ls_ztmm40300-aezet = sy-uzeit.
*    ls_ztmm40300-aenam = sy-uname.
*
*    APPEND ls_ztmm40300 TO lt_ztmm40300. CLEAR ls_ztmm40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.
*
**> ITEM 이력 MODIFY
*  LOOP AT it_iditem INTO DATA(ls_iditem).
*
*    READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*    IF sy-subrc = 0.
*
*      MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*        ls_ztmm40301-ztflag = ''.
*      ELSE.
*        ls_ztmm40301-ztflag = 'X'.
*      ENDIF.
*
*      IF ls_iditem-erdat NE gc_00000000.
*        ls_ztmm40301-erdat = ls_iditem-erdat.
*        ls_ztmm40301-erzet = ls_iditem-erzet.
*        ls_ztmm40301-ernam = ls_iditem-ernam.
*      ELSE.
*        ls_ztmm40301-erdat = sy-datum.
*        ls_ztmm40301-erzet = sy-uzeit.
*        ls_ztmm40301-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40301-aedat = sy-datum.
*      ls_ztmm40301-aezet = sy-uzeit.
*      ls_ztmm40301-aenam = sy-uname.
*
*      APPEND ls_ztmm40301 TO lt_ztmm40301. CLEAR ls_ztmm40301.
*    ENDIF.
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2002_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_IDHEADER
*&      --> LT_IDITEM
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2002_trans  TABLES it_idheader TYPE tt_idheader
                                        it_iditem TYPE tt_iditem.


*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_wms,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_wms,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_wms,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
**-
*  CLEAR : gs_out_header, gs_in_header.
*  _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if.
**-
*
*  IF it_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2002.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
*
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT it_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if.
*
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  gs_out_header-if_id = gc_if_id_0100.                      "MM-0070
*  gs_out_header-additional_info = gc_info_kgc_2002. "KGC WMS
*
**> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES80' ).
*
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_KGC_WMS' DESTINATION lv_rfcdest
*    EXPORTING
*      is_header    = gs_out_header
*    IMPORTING
*      es_header    = gs_in_header
*    TABLES
*      it_inbheader = lt_inbheader_kgc_if
*      it_inbitem   = lt_inbitem_kgc_if.
*
**> HEADER 이력 MODIFY
*  LOOP AT it_idheader INTO DATA(ls_idheader) WHERE zwerks = gc_werks_2002.
*
*    MOVE-CORRESPONDING ls_idheader TO ls_ztmm40300.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*
*      ls_ztmm40300-zstatus = 'F'.
*      ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*      ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*      ls_ztmm40300-zstatus = 'X'.
*      ls_ztmm40300-ztflag = 'X'.
*      ls_ztmm40300-zmessage = gc_msg_inb_success.
*      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*
*    IF ls_idheader-erdat NE gc_00000000.
*      ls_ztmm40300-erdat = ls_idheader-erdat.
*      ls_ztmm40300-erzet = ls_idheader-erzet.
*      ls_ztmm40300-ernam = ls_idheader-ernam.
*    ELSE.
*      ls_ztmm40300-erdat = sy-datum.
*      ls_ztmm40300-erzet = sy-uzeit.
*      ls_ztmm40300-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40300-aedat = sy-datum.
*    ls_ztmm40300-aezet = sy-uzeit.
*    ls_ztmm40300-aenam = sy-uname.
*
*    APPEND ls_ztmm40300 TO lt_ztmm40300. CLEAR ls_ztmm40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.
*
**> ITEM 이력 MODIFY
*  LOOP AT it_iditem INTO DATA(ls_iditem).
*
*    READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*    IF sy-subrc = 0.
*
*      MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*        ls_ztmm40301-ztflag = ''.
*      ELSE.
*        ls_ztmm40301-ztflag = 'X'.
*      ENDIF.
*
*      IF ls_iditem-erdat NE gc_00000000.
*        ls_ztmm40301-erdat = ls_iditem-erdat.
*        ls_ztmm40301-erzet = ls_iditem-erzet.
*        ls_ztmm40301-ernam = ls_iditem-ernam.
*      ELSE.
*        ls_ztmm40301-erdat = sy-datum.
*        ls_ztmm40301-erzet = sy-uzeit.
*        ls_ztmm40301-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40301-aedat = sy-datum.
*      ls_ztmm40301-aezet = sy-uzeit.
*      ls_ztmm40301-aenam = sy-uname.
*
*      APPEND ls_ztmm40301 TO lt_ztmm40301. CLEAR ls_ztmm40301.
*    ENDIF.
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KGC_WMS_2050_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_IDHEADER
*&      --> LT_IDITEM
*&---------------------------------------------------------------------*
FORM id_info_kgc_wms_2050_trans   TABLES it_idheader TYPE tt_idheader
                                         it_iditem TYPE tt_iditem.


*  DATA : ls_inbheader_kgc    TYPE zsmm_inb_header,
*         ls_inbheader_kgc_if TYPE zsmm_inb_header,
*         lt_inbheader_kgc_if TYPE TABLE OF zsmm_inb_header,
*
*         ls_inbitem_kgc      TYPE zsmm_inb_item_kgc_3pl,
*         ls_inbitem_kgc_if   TYPE zsmm_inb_item_kgc_3pl,
*         lt_inbitem_kgc_if   TYPE TABLE OF zsmm_inb_item_kgc_3pl,
*
*         ls_batchc_kgc       TYPE zsmm_inb_batch_kgc,
*         ls_batchc_kgc_if    TYPE zsmm_inb_batch_kgc,
*         lt_batchc_kgc_if    TYPE TABLE OF zsmm_inb_batch_kgc,
*
*         ls_ztmm40300        TYPE ztmm40300,
*         lt_ztmm40300        TYPE TABLE OF ztmm40300,
*         ls_ztmm40301        TYPE ztmm40301,
*         lt_ztmm40301        TYPE TABLE OF ztmm40301.
*
**-
*  CLEAR : gs_out_header, gs_in_header.
*  _g_init: lt_inbheader_kgc_if, lt_inbitem_kgc_if.
**-
*
*  IF it_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT it_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>) WHERE zwerks = gc_werks_2050.
*
*      CLEAR : ls_inbheader_kgc, ls_inbheader_kgc_if.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_kgc.
*      _g_conv_strc_sap_to_eai ls_inbheader_kgc ls_inbheader_kgc_if.
*
*      IF ls_inbheader_kgc_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_kgc_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_kgc_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_kgc_if TO lt_inbheader_kgc_if.
*
**--------------------------------------------------------------------*
**> ITEM/BATCH DATA SET
**--------------------------------------------------------------------*
*      LOOP AT it_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln.
*
*        CLEAR : ls_inbitem_kgc, ls_inbitem_kgc_if, ls_batchc_kgc, ls_batchc_kgc_if.
*
**> ITEM
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_kgc.
*
*        IF ls_inbitem_kgc-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_kgc-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_kgc-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_kgc ls_inbitem_kgc_if.
*
*        APPEND ls_inbitem_kgc_if TO lt_inbitem_kgc_if.
*
**> BATCH
*        MOVE-CORRESPONDING <ls_iditem> TO ls_batchc_kgc.
*
*        _g_conv_strc_sap_to_eai ls_batchc_kgc ls_batchc_kgc_if.
*
*        APPEND ls_batchc_kgc_if TO lt_batchc_kgc_if.
*
*
*      ENDLOOP.
*    ENDLOOP.
*  ENDIF.
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*  gs_out_header-if_id = gc_if_id_0071.                      "MM-0071
*  gs_out_header-additional_info = gc_info_kgc_2050. "KGC WMS
*
**> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'KGC20' ).
*
*  SORT lt_batchc_kgc_if BY matnr charg.
*  DELETE ADJACENT DUPLICATES FROM lt_batchc_kgc_if COMPARING matnr charg.
*
*  CALL FUNCTION 'ZFIMM_ID_INFO_KGC_3PL' DESTINATION lv_rfcdest
*    EXPORTING
*      is_header    = gs_out_header
*    IMPORTING
*      es_header    = gs_in_header
*    TABLES
*      it_inbheader = lt_inbheader_kgc_if
*      it_inbitem   = lt_inbitem_kgc_if
*      it_batchc    = lt_batchc_kgc_if.
*
**> HEADER 이력 MODIFY
*  LOOP AT it_idheader INTO DATA(ls_idheader) WHERE zwerks = gc_werks_2050.
*
*    MOVE-CORRESPONDING ls_idheader TO ls_ztmm40300.
*
*    IF gs_in_header-rst_cd = gc_cd_9999.
*
*      ls_ztmm40300-zstatus = 'F'.
*      ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*    ELSE.
*      ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*      ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*      ls_ztmm40300-zstatus = 'X'.
*      ls_ztmm40300-ztflag = 'X'.
*      ls_ztmm40300-zmessage = gc_msg_inb_success.
*      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*    ENDIF.
*
*
*    IF ls_idheader-erdat NE gc_00000000.
*      ls_ztmm40300-erdat = ls_idheader-erdat.
*      ls_ztmm40300-erzet = ls_idheader-erzet.
*      ls_ztmm40300-ernam = ls_idheader-ernam.
*    ELSE.
*      ls_ztmm40300-erdat = sy-datum.
*      ls_ztmm40300-erzet = sy-uzeit.
*      ls_ztmm40300-ernam = sy-uname.
*    ENDIF.
*
*    ls_ztmm40300-aedat = sy-datum.
*    ls_ztmm40300-aezet = sy-uzeit.
*    ls_ztmm40300-aenam = sy-uname.
*
*    APPEND ls_ztmm40300 TO lt_ztmm40300. CLEAR ls_ztmm40300.
*
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.
*
**> ITEM 이력 MODIFY
*  LOOP AT it_iditem INTO DATA(ls_iditem).
*
*    READ TABLE lt_ztmm40300 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_iditem-vbeln.
*    IF sy-subrc = 0.
*
*      MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*        ls_ztmm40301-ztflag = ''.
*      ELSE.
*        ls_ztmm40301-ztflag = 'X'.
*      ENDIF.
*
*      IF ls_iditem-erdat NE gc_00000000.
*        ls_ztmm40301-erdat = ls_iditem-erdat.
*        ls_ztmm40301-erzet = ls_iditem-erzet.
*        ls_ztmm40301-ernam = ls_iditem-ernam.
*      ELSE.
*        ls_ztmm40301-erdat = sy-datum.
*        ls_ztmm40301-erzet = sy-uzeit.
*        ls_ztmm40301-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40301-aedat = sy-datum.
*      ls_ztmm40301-aezet = sy-uzeit.
*      ls_ztmm40301-aenam = sy-uname.
*
*      APPEND ls_ztmm40301 TO lt_ztmm40301. CLEAR ls_ztmm40301.
*    ENDIF.
*  ENDLOOP.
*
*  TRY.
*      MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*      MESSAGE s013(zmm01). "DB Update System Error !
*  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_PROCESS_TRANS_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM btn_on_process_trans_po .

  DATA: lt_poheader TYPE TABLE OF ts_poheader,
        lt_poitem   TYPE TABLE OF ts_poitem.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_selidx).

  IF lt_selidx[] IS INITIAL.
    MESSAGE s000 WITH '예정 정보 전송대상을 선택하세요.'(m05) DISPLAY LIKE 'E'. EXIT.
  ENDIF.

*> 전송대상 추출

  LOOP AT lt_selidx INTO DATA(ls_selidx).

    READ TABLE gt_poheader INTO DATA(ls_poheader) INDEX ls_selidx-index.
    IF sy-subrc = 0.

      APPEND ls_poheader TO lt_poheader.

      LOOP AT gt_poitem INTO DATA(ls_poitem) WHERE ebeln = ls_poheader-ebeln.

        APPEND ls_poitem TO lt_poitem. CLEAR ls_poitem.
      ENDLOOP.
    ENDIF.

    CLEAR ls_poheader.

  ENDLOOP.

  CHECK grf_grid->pop_to_msg( iv_type  = 'A'
                              iv_title = CONV string( TEXT-u01 )      "예정 정보 전송
                              iv_text1 = CONV #( '구매오더 정보를 전송합니다.' )
                              iv_text2 = CONV #( '전송하시겠습니까?' ) )
                              EQ abap_true. " YES
  CASE p_bukrs.
    WHEN gc_1101.

      PERFORM po_info_ktng_wms_trans TABLES lt_poheader lt_poitem.

    WHEN gc_2101.

      READ TABLE lt_poitem WITH KEY werks = gc_werks_2001 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        PERFORM po_info_kgc_wms_2001_trans TABLES lt_poheader lt_poitem.
      ENDIF.

      READ TABLE lt_poitem WITH KEY werks = gc_werks_2002 TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.
        PERFORM po_info_kgc_wms_2002_trans TABLES lt_poheader lt_poitem.
      ENDIF.

    WHEN gc_3101.

      PERFORM po_info_yjp_wms_trans TABLES lt_poheader lt_poitem.

  ENDCASE.

* REFRESH DISPLAY
  PERFORM get_data_po.
  grf_grid->refresh_grid_display( ).



ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KTNG_WMS_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_POHEADER
*&      --> LT_POITEM
*&---------------------------------------------------------------------*
FORM po_info_ktng_wms_trans   TABLES it_poheader TYPE tt_poheader
                                     it_poitem   TYPE tt_poitem.


  DATA : ls_poheader_ktng    TYPE zsmm_po_wmsheader_ktng,
         ls_poheader_ktng_if TYPE zsmm_po_wmsheader_ktng,
         lt_poheader_ktng_if TYPE TABLE OF zsmm_po_wmsheader_ktng,

         ls_poitem_ktng      TYPE zsmm_po_wmsitem_ktng,
         ls_poitem_ktng_if   TYPE zsmm_po_wmsitem_ktng,
         lt_poitem_ktng_if   TYPE TABLE OF zsmm_po_wmsitem_ktng,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_ktng_if, lt_poitem_ktng_if.
*-

  IF it_poheader[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poheader INTO DATA(ls_poheader).

      CLEAR : ls_poheader_ktng, ls_poheader_ktng_if.

      IF ls_poheader-zflag = 'D'.
        ls_poheader-loekz = 'L'.
      ELSEIF p_chk2 IS NOT INITIAL AND ls_poheader-loekz = 'L'.
        ls_poheader-zflag = 'D'.
      ELSE.
        ls_poheader-loekz = ''.
      ENDIF.

      MOVE-CORRESPONDING ls_poheader TO ls_poheader_ktng.
      ls_poheader_ktng-aedat = ls_poheader-zaedat.

*      _g_conv_strc_sap_to_eai ls_poheader_ktng ls_poheader_ktng_if.

      IF ls_poheader_ktng_if-lifnr IS NOT INITIAL.
        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
          EXPORTING
            iv_bp = ls_poheader_ktng_if-lifnr
          IMPORTING
            ev_bp = ls_poheader_ktng_if-lifnr.
      ENDIF.

      APPEND ls_poheader_ktng_if TO lt_poheader_ktng_if.

    ENDLOOP.
*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poitem INTO DATA(ls_poitem).
      "WHERE ZFLAG NE ''. "삭제 정보 전송을 위한 주석처리

      CLEAR : ls_poitem_ktng, ls_poitem_ktng_if.

      MOVE-CORRESPONDING ls_poitem TO ls_poitem_ktng.

      IF ls_poitem_ktng-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_ktng-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_ktng-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_ktng ls_poitem_ktng_if.

      APPEND ls_poitem_ktng_if TO lt_poitem_ktng_if.

    ENDLOOP.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0074.                      "MM-0074
  gs_out_header-additional_info = gc_info_ktng. "KTNG WMS

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'KTG20' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_KTNG_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_ktng_if
      it_poitem   = lt_poitem_ktng_if.

*> HEADER 이력 MODIFY
  LOOP AT it_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

    ls_ztmm40304-zdest = '1'.

    IF <ls_poheader>-zflag = 'D'.
      ls_ztmm40304-loekz = 'L'.
    ELSE.
      ls_ztmm40304-loekz = ''.
    ENDIF.

    IF gs_in_header-rst_cd = gc_cd_9999.
      ls_ztmm40304-zstatus = 'E'.
      ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
    ELSE.

      ls_ztmm40304-zstatus = 'S'.
      ls_ztmm40304-zmsg = gc_msg_po_success.
      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
    ENDIF.

    READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

    IF sy-subrc = 0.
      ls_ztmm40304-erdat = ls_40304-erdat.
      ls_ztmm40304-erzet = ls_40304-erzet.
      ls_ztmm40304-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40304-erdat = sy-datum.
      ls_ztmm40304-erzet = sy-uzeit.
      ls_ztmm40304-ernam = sy-uname.
    ENDIF.

    ls_ztmm40304-aedat = sy-datum.
    ls_ztmm40304-aezet = sy-uzeit.
    ls_ztmm40304-aenam = sy-uname.

    APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> HEADER 이력 MODIFY
  LOOP AT it_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>).

    MOVE-CORRESPONDING <ls_poitem> TO ls_ztmm40305.

    ls_ztmm40305-charg = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                         ebelp = <ls_poitem>-ebelp BINARY SEARCH.

    IF sy-subrc = 0.
      ls_ztmm40305-loekz = ls_40304-loekz.

      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_YJP_WMS_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_POHEADER
*&      --> LT_POITEM
*&---------------------------------------------------------------------*
FORM po_info_yjp_wms_trans  TABLES it_poheader TYPE tt_poheader
                                   it_poitem   TYPE tt_poitem.


  DATA : ls_poheader_yjp    TYPE zsmm_po_wmsheader,
         ls_poheader_yjp_if TYPE zsmm_po_wmsheader,
         lt_poheader_yjp_if TYPE TABLE OF zsmm_po_wmsheader,

         ls_poitem_yjp      TYPE zsmm_po_wmsitem_yjp,
         ls_poitem_yjp_if   TYPE zsmm_po_wmsitem_yjp,
         lt_poitem_yjp_if   TYPE TABLE OF zsmm_po_wmsitem_yjp,

         ls_ztmm40304       TYPE ztmm40304,
         lt_ztmm40304       TYPE TABLE OF ztmm40304,
         ls_ztmm40305       TYPE ztmm40305,
         lt_ztmm40305       TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_yjp_if, lt_poitem_yjp_if.
*-

  IF it_poheader[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poheader INTO DATA(ls_poheader).

      CLEAR : ls_poheader_yjp, ls_poheader_yjp_if.

      MOVE-CORRESPONDING ls_poheader TO ls_poheader_yjp.
      ls_poheader_yjp-aedat = ls_poheader-zaedat.

*      _g_conv_strc_sap_to_eai ls_poheader_yjp ls_poheader_yjp_if.

      IF ls_poheader-zflag = 'D'.
        ls_poheader_yjp_if-loekz = 'L'.
      ELSEIF p_chk2 IS NOT INITIAL AND ls_poheader_yjp_if-loekz = 'L'.
        ls_poheader-zflag = 'D'.
      ELSE.
        ls_poheader_yjp_if-loekz = ''.
      ENDIF.

      IF ls_poheader_yjp_if-lifnr IS NOT INITIAL.
        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
          EXPORTING
            iv_bp = ls_poheader_yjp_if-lifnr
          IMPORTING
            ev_bp = ls_poheader_yjp_if-lifnr.
      ENDIF.

      APPEND ls_poheader_yjp_if TO lt_poheader_yjp_if.

    ENDLOOP.

*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poitem INTO DATA(ls_poitem) WHERE zflag NE ''.

      CLEAR : ls_poitem_yjp, ls_poitem_yjp_if.

      MOVE-CORRESPONDING ls_poitem TO ls_poitem_yjp.

      IF ls_poitem_yjp-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_yjp-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_yjp-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_yjp ls_poitem_yjp_if.

      APPEND ls_poitem_yjp_if TO lt_poitem_yjp_if.

    ENDLOOP.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0077.                      "MM-0074
  gs_out_header-additional_info = gc_info_yjp. "YJP WMS

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'YJP10' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_YJP_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_yjp_if
      it_poitem   = lt_poitem_yjp_if.

*> HEADER 이력 MODIFY

  LOOP AT it_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

    ls_ztmm40304-zdest = '4'.

    IF gs_in_header-rst_cd = gc_cd_9999.
      ls_ztmm40304-zstatus = 'E'.
      ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
      MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
    ELSE.
      ls_ztmm40304-zstatus = 'S'.
      ls_ztmm40304-zmsg = gc_msg_po_success.
      MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
    ENDIF.

    READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

    IF sy-subrc = 0.
      ls_ztmm40304-erdat = ls_40304-erdat.
      ls_ztmm40304-erzet = ls_40304-erzet.
      ls_ztmm40304-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40304-erdat = sy-datum.
      ls_ztmm40304-erzet = sy-uzeit.
      ls_ztmm40304-ernam = sy-uname.
    ENDIF.

    ls_ztmm40304-aedat = sy-datum.
    ls_ztmm40304-aezet = sy-uzeit.
    ls_ztmm40304-aenam = sy-uname.

    APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.

  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> HEADER 이력 MODIFY
  LOOP AT it_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>).

    MOVE-CORRESPONDING <ls_poitem> TO ls_ztmm40305.

    ls_ztmm40305-bwtar = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = <ls_poitem>-ebeln
                                                         ebelp = <ls_poitem>-ebelp BINARY SEARCH.

    IF sy-subrc = 0.
      ls_ztmm40305-loekz = ls_40304-loekz.

      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KGC_WMS_2001_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_POHEADER
*&      --> LT_POITEM
*&---------------------------------------------------------------------*
FORM po_info_kgc_wms_2001_trans   TABLES it_poheader TYPE tt_poheader
                                         it_poitem   TYPE tt_poitem.


  DATA : ls_poheader_kgc1    TYPE zsmm_po_wmsheader_kgc,
         ls_poheader_kgc1_if TYPE zsmm_po_wmsheader_kgc,
         lt_poheader_kgc1_if TYPE TABLE OF zsmm_po_wmsheader_kgc,

         ls_poitem_kgc1      TYPE zsmm_po_wmsitem_kgc,
         ls_poitem_kgc1_if   TYPE zsmm_po_wmsitem_kgc,
         lt_poitem_kgc1_if   TYPE TABLE OF zsmm_po_wmsitem_kgc,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_kgc1_if, lt_poitem_kgc1_if.
*-

  IF it_poheader[] IS NOT INITIAL.

    "2001- 부여
*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE werks = gc_werks_2001.

      CLEAR : ls_poitem_kgc1, ls_poitem_kgc1_if.

      MOVE-CORRESPONDING <ls_poitem> TO ls_poitem_kgc1.

      IF ls_poitem_kgc1-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_kgc1-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_kgc1-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_kgc1 ls_poitem_kgc1_if.

      ls_poitem_kgc1_if-zgrmenge = ''.

      APPEND ls_poitem_kgc1_if TO lt_poitem_kgc1_if.

      IF lt_poitem_kgc1_if[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
        READ TABLE it_poheader INTO DATA(ls_poheader) WITH KEY ebeln = <ls_poitem>-ebeln.

        IF ls_poheader IS NOT INITIAL.

          CLEAR : ls_poheader_kgc1, ls_poheader_kgc1_if.

          MOVE-CORRESPONDING ls_poheader TO ls_poheader_kgc1.
          ls_poheader_kgc1-aedat = ls_poheader-zaedat.

*          _g_conv_strc_sap_to_eai ls_poheader_kgc1 ls_poheader_kgc1_if.

          IF ls_poheader-zflag = 'D'.
            ls_poheader_kgc1_if-loekz = 'L'.
          ELSEIF p_chk2 IS NOT INITIAL AND ls_poheader_kgc1_if-loekz = 'L'.
            ls_poheader-zflag = 'D'.
          ELSE.
            ls_poheader_kgc1_if-loekz = ''.
          ENDIF.


          IF ls_poheader_kgc1_if-lifnr IS NOT INITIAL.
            CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
              EXPORTING
                iv_bp = ls_poheader_kgc1_if-lifnr
              IMPORTING
                ev_bp = ls_poheader_kgc1_if-lifnr.
          ENDIF.

          APPEND ls_poheader_kgc1_if TO lt_poheader_kgc1_if.

          CLEAR : ls_poheader.

        ENDIF.

      ENDIF.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_poheader_kgc1_if COMPARING ALL FIELDS.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0075.                      "MM-0075
  gs_out_header-additional_info = gc_info_kgc_2001.  "KGC WMS-부여

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES70' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_KGC_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_kgc1_if
      it_poitem   = lt_poitem_kgc1_if.

*> HEADER 이력 MODIFY
  LOOP AT it_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    READ TABLE it_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

      ls_ztmm40304-zdest = '2'.
      IF gs_in_header-rst_cd = gc_cd_9999.

        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.

        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.
    ENDIF.
  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> HEADER 이력 MODIFY
  LOOP AT it_poitem INTO DATA(ls_poitem) WHERE werks = gc_werks_2001.

    MOVE-CORRESPONDING ls_poitem TO ls_ztmm40305.

    ls_ztmm40305-matnr = ls_poitem-matnr.
    ls_ztmm40305-bwtar = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = ls_poitem-ebeln
                                                         ebelp = ls_poitem-ebelp BINARY SEARCH.

    IF sy-subrc = 0.

      ls_ztmm40305-loekz = ls_40304-loekz.
      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PO_INFO_KGC_WMS_2002_TRANS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_POHEADER
*&      --> LT_POITEM
*&---------------------------------------------------------------------*
FORM po_info_kgc_wms_2002_trans   TABLES it_poheader TYPE tt_poheader
                                         it_poitem   TYPE tt_poitem.

  " KGC1 - 부여플랜트 , KGC2 - 원주플랜트

  DATA : ls_poheader_kgc2    TYPE zsmm_po_wmsheader_kgc,
         ls_poheader_kgc2_if TYPE zsmm_po_wmsheader_kgc,
         lt_poheader_kgc2_if TYPE TABLE OF zsmm_po_wmsheader_kgc,

         ls_poitem_kgc2      TYPE zsmm_po_wmsitem_kgc,
         ls_poitem_kgc2_if   TYPE zsmm_po_wmsitem_kgc,
         lt_poitem_kgc2_if   TYPE TABLE OF zsmm_po_wmsitem_kgc,

         ls_ztmm40304        TYPE ztmm40304,
         lt_ztmm40304        TYPE TABLE OF ztmm40304,
         ls_ztmm40305        TYPE ztmm40305,
         lt_ztmm40305        TYPE TABLE OF ztmm40305.

*-
  CLEAR : gs_out_header, gs_in_header.
  _g_init: lt_poheader_kgc2_if, lt_poitem_kgc2_if.
*-

  IF it_poheader[] IS NOT INITIAL.

    "2002- 원주
*--------------------------------------------------------------------*
*> ITEM DATA SET
*--------------------------------------------------------------------*
    LOOP AT it_poitem ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE werks = gc_werks_2002.

      CLEAR : ls_poitem_kgc2, ls_poitem_kgc2_if.

      MOVE-CORRESPONDING <ls_poitem> TO ls_poitem_kgc2.

      IF ls_poitem_kgc2-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = ls_poitem_kgc2-meins
            language       = sy-langu
          IMPORTING
            output         = ls_poitem_kgc2-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.

*      _g_conv_strc_sap_to_eai ls_poitem_kgc2 ls_poitem_kgc2_if.

      ls_poitem_kgc2_if-zgrmenge = ''.

      APPEND ls_poitem_kgc2_if TO lt_poitem_kgc2_if.

      IF lt_poitem_kgc2_if[] IS NOT INITIAL.

*--------------------------------------------------------------------*
*> HEADER DATA SET
*--------------------------------------------------------------------*
        READ TABLE it_poheader INTO DATA(ls_poheader) WITH KEY ebeln = <ls_poitem>-ebeln.

        IF ls_poheader IS NOT INITIAL.

          CLEAR : ls_poheader_kgc2, ls_poheader_kgc2_if.

          MOVE-CORRESPONDING ls_poheader TO ls_poheader_kgc2.
          ls_poheader_kgc2-aedat = ls_poheader-zaedat.

*          _g_conv_strc_sap_to_eai ls_poheader_kgc2 ls_poheader_kgc2_if.

          IF ls_poheader-zflag = 'D'.
            ls_poheader_kgc2_if-loekz = 'L'.
          ELSEIF p_chk2 IS NOT INITIAL AND ls_poheader_kgc2_if-loekz = 'L'.
            ls_poheader-zflag = 'D'.
          ELSE.
            ls_poheader_kgc2_if-loekz = ''.
          ENDIF.

          IF ls_poheader_kgc2_if-lifnr IS NOT INITIAL.
            CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
              EXPORTING
                iv_bp = ls_poheader_kgc2_if-lifnr
              IMPORTING
                ev_bp = ls_poheader_kgc2_if-lifnr.
          ENDIF.

          APPEND ls_poheader_kgc2_if TO lt_poheader_kgc2_if.

          CLEAR : ls_poheader.

        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_poheader_kgc2_if COMPARING ALL FIELDS.

  ENDIF.

**********************************************************************
*> I/F 실행
**********************************************************************

  gs_out_header-if_id = gc_if_id_0076.                      "MM-0076
  gs_out_header-additional_info = gc_info_kgc_2002.         "KGC WMS-원주

*> EAI LOG START
*  zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*  DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                        iv_sysid_t = 'MES80' ).

  CALL FUNCTION 'ZFIMM_PO_INFO_KGC_WMS' "DESTINATION lv_rfcdest
    EXPORTING
      is_header   = gs_out_header
    IMPORTING
      es_header   = gs_in_header
    TABLES
      it_poheader = lt_poheader_kgc2_if
      it_poitem   = lt_poitem_kgc2_if.

*> HEADER 이력 MODIFY
  LOOP AT it_poheader ASSIGNING FIELD-SYMBOL(<ls_poheader>).

    READ TABLE it_poitem TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_poheader>-ebeln.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING <ls_poheader> TO ls_ztmm40304.

      ls_ztmm40304-zdest = '3'.
      IF gs_in_header-rst_cd = gc_cd_9999.

        ls_ztmm40304-zstatus = 'E'.
        ls_ztmm40304-zmsg = gc_msg_po_fail && '(' && gs_in_header-rst_msg && ')'.
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m02.  "E - 구매오더 정보 전송 실패
      ELSE.

        ls_ztmm40304-zstatus = 'S'.
        ls_ztmm40304-zmsg = gc_msg_po_success.
        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m03.  "S - 구매오더 정보 전송 성공
      ENDIF.

      READ TABLE gt_ztmm40304 INTO DATA(ls_40304) WITH KEY ebeln = <ls_poheader>-ebeln BINARY SEARCH.

      IF sy-subrc = 0.
        ls_ztmm40304-erdat = ls_40304-erdat.
        ls_ztmm40304-erzet = ls_40304-erzet.
        ls_ztmm40304-ernam = ls_40304-ernam.
      ELSE.
        ls_ztmm40304-erdat = sy-datum.
        ls_ztmm40304-erzet = sy-uzeit.
        ls_ztmm40304-ernam = sy-uname.
      ENDIF.

      ls_ztmm40304-aedat = sy-datum.
      ls_ztmm40304-aezet = sy-uzeit.
      ls_ztmm40304-aenam = sy-uname.

      APPEND ls_ztmm40304 TO lt_ztmm40304. CLEAR ls_ztmm40304.
    ENDIF.
  ENDLOOP.

  TRY.
      MODIFY ztmm40304 FROM TABLE lt_ztmm40304.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.

*> HEADER 이력 MODIFY
  LOOP AT it_poitem INTO DATA(ls_poitem) WHERE werks = gc_werks_2002.

    MOVE-CORRESPONDING ls_poitem TO ls_ztmm40305.

    ls_ztmm40305-matnr = ls_poitem-matnr.
    ls_ztmm40305-bwtar = ''.

    READ TABLE gt_ztmm40305 INTO DATA(ls_40305) WITH KEY ebeln = ls_poitem-ebeln
                                                         ebelp = ls_poitem-ebelp BINARY SEARCH.

    IF sy-subrc = 0.

      ls_ztmm40305-loekz = ls_40304-loekz.
      ls_ztmm40305-erdat = ls_40304-erdat.
      ls_ztmm40305-erzet = ls_40304-erzet.
      ls_ztmm40305-ernam = ls_40304-ernam.
    ELSE.
      ls_ztmm40305-erdat = sy-datum.
      ls_ztmm40305-erzet = sy-uzeit.
      ls_ztmm40305-ernam = sy-uname.
    ENDIF.

    ls_ztmm40305-aedat = sy-datum.
    ls_ztmm40305-aezet = sy-uzeit.
    ls_ztmm40305-aenam = sy-uname.

    APPEND ls_ztmm40305 TO lt_ztmm40305. CLEAR ls_ztmm40305.

  ENDLOOP.

  TRY.
      MODIFY ztmm40305 FROM TABLE lt_ztmm40305.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.

    CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
      MESSAGE s013(zmm01). "DB Update System Error !
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_data_100.
  PERFORM get_data_inb.
  grf_grid->refresh_grid_display( ).

  _g_init : gt_disp_id.
  grf_item->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_110
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_data_110 .

  PERFORM get_data_po.
  grf_grid->refresh_grid_display( ).

  _g_init : gt_disp_po.
  grf_item->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ID_INFO_KTNG_WMS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM id_info_ktng_wms.


*  DATA : ls_inbheader_ktng    TYPE zsmm_inb_header_ktng,
*         ls_inbheader_ktng_if TYPE zsmm_inb_header_ktng,
*         lt_inbheader_ktng_if TYPE TABLE OF zsmm_inb_header_ktng,
*
*         ls_inbitem_ktng      TYPE zsmm_inb_item,
*         ls_inbitem_ktng_if   TYPE zsmm_inb_item,
*         lt_inbitem_ktng_if   TYPE TABLE OF zsmm_inb_item,
*
*         ls_ztmm40300         TYPE ztmm40300,
*         lt_ztmm40300         TYPE TABLE OF ztmm40300,
*         ls_ztmm40301         TYPE ztmm40301,
*         lt_ztmm40301         TYPE TABLE OF ztmm40301.
*
*  _g_init : lt_ztmm40300, lt_ztmm40301.
*
*  IF gt_idheader[] IS NOT INITIAL.
*
**--------------------------------------------------------------------*
**> HEADER DATA SET
**--------------------------------------------------------------------*
*    LOOP AT gt_idheader ASSIGNING FIELD-SYMBOL(<ls_idheader>).
*
*      CLEAR : ls_inbheader_ktng, ls_inbheader_ktng_if.
*      _g_init: lt_inbheader_ktng_if, lt_inbitem_ktng_if.
*
*      IF <ls_idheader>-zflag = 'D'.
*        <ls_idheader>-loekz = 'L'.
*      ELSE.
*        <ls_idheader>-loekz = ''.
*      ENDIF.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_inbheader_ktng.
*      _g_conv_strc_sap_to_eai ls_inbheader_ktng ls_inbheader_ktng_if.
*
*      IF ls_inbheader_ktng_if-lifnr IS NOT INITIAL.
*        CALL FUNCTION 'ZFMD_BP_CONV_FOR_LEGACY'
*          EXPORTING
*            iv_bp = ls_inbheader_ktng_if-lifnr
*          IMPORTING
*            ev_bp = ls_inbheader_ktng_if-lifnr.
*      ENDIF.
*
*      APPEND ls_inbheader_ktng_if TO lt_inbheader_ktng_if.
*
**--------------------------------------------------------------------*
**> ITEM DATA SET
**--------------------------------------------------------------------*
*      LOOP AT gt_iditem ASSIGNING FIELD-SYMBOL(<ls_iditem>) WHERE vbeln = <ls_idheader>-vbeln
*                                                              AND zflag NE ''.
*
*        CLEAR : ls_inbitem_ktng, ls_inbitem_ktng_if.
*
*        MOVE-CORRESPONDING <ls_iditem> TO ls_inbitem_ktng.
*
*        IF ls_inbitem_ktng-vrkme IS NOT INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*            EXPORTING
*              input          = ls_inbitem_ktng-vrkme
*              language       = sy-langu
*            IMPORTING
*              output         = ls_inbitem_ktng-vrkme
*            EXCEPTIONS
*              unit_not_found = 1
*              OTHERS         = 2.
*        ENDIF.
*
*        _g_conv_strc_sap_to_eai ls_inbitem_ktng ls_inbitem_ktng_if.
*
*        APPEND ls_inbitem_ktng_if TO lt_inbitem_ktng_if.
*
*      ENDLOOP.
*
*
***********************************************************************
**> I/F 실행
***********************************************************************
*
*      CLEAR : gs_out_header, gs_in_header.
*
*      gs_out_header-if_id = gc_if_id_0069.                  "MM-0074
*      gs_out_header-additional_info = gc_info_ktng. "KTNG WMS
*
**> EAI LOG START
*      zcl_cn_abap_util=>get_eai_start( CHANGING cs_header = gs_out_header ).
*
*      DATA(lv_rfcdest) = zcl_cn_abap_util=>get_destination( iv_sysid_s = sy-sysid
*                                                            iv_sysid_t = 'KTG20' ).
*
*      CALL FUNCTION 'ZFIMM_ID_INFO_KTNG_WMS' DESTINATION lv_rfcdest
*        EXPORTING
*          is_header    = gs_out_header
*        IMPORTING
*          es_header    = gs_in_header
*        TABLES
*          it_inbheader = lt_inbheader_ktng_if
*          it_inbitem   = lt_inbitem_ktng_if.
*
*      CLEAR ls_ztmm40300.
*
*      MOVE-CORRESPONDING <ls_idheader> TO ls_ztmm40300.
*
*      IF gs_in_header-rst_cd = gc_cd_9999.
*
*        ls_ztmm40300-zstatus = 'F'.
*        ls_ztmm40300-zmessage = gc_msg_inb_fail && '(' && gs_in_header-rst_msg && ')'.
*        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m07.  "INBOUND DELIVERY 정보 전송 실패
*      ELSE.
*        ls_ztmm40300-zwmsifid = gs_in_header-if_id.
*        ls_ztmm40300-zwmstrcid = gs_in_header-if_trc_id.
*
*        ls_ztmm40300-zstatus = 'X'.
*        ls_ztmm40300-ztflag = 'X'.
*        ls_ztmm40300-zmessage = gc_msg_inb_success.
*        MESSAGE s000 DISPLAY LIKE 'S' WITH TEXT-m08.  "INBOUND DELIVERY 정보 전송 성공
*      ENDIF.
*
*      IF <ls_idheader>-erdat NE gc_00000000.
*        ls_ztmm40300-erdat = <ls_idheader>-erdat.
*        ls_ztmm40300-erzet = <ls_idheader>-erzet.
*        ls_ztmm40300-ernam = <ls_idheader>-ernam.
*      ELSE.
*        ls_ztmm40300-erdat = sy-datum.
*        ls_ztmm40300-erzet = sy-uzeit.
*        ls_ztmm40300-ernam = sy-uname.
*      ENDIF.
*
*      ls_ztmm40300-aedat = sy-datum.
*      ls_ztmm40300-aezet = sy-uzeit.
*      ls_ztmm40300-aenam = sy-uname.
*
*      APPEND ls_ztmm40300 TO lt_ztmm40300.
*
*
**> ITEM 이력 MODIFY
*      LOOP AT gt_iditem INTO DATA(ls_iditem)  WHERE vbeln = <ls_idheader>-vbeln
*                                                AND zflag NE ''.
*
*        CLEAR ls_ztmm40301.
*
*        MOVE-CORRESPONDING ls_iditem TO ls_ztmm40301.
*
*        IF gs_in_header-rst_cd = gc_cd_9999.
*          ls_ztmm40301-ztflag = ''.
*        ELSE.
*          ls_ztmm40301-ztflag = 'X'.
*        ENDIF.
*
*        IF ls_iditem-erdat NE gc_00000000.
*          ls_ztmm40301-erdat = ls_iditem-erdat.
*          ls_ztmm40301-erzet = ls_iditem-erzet.
*          ls_ztmm40301-ernam = ls_iditem-ernam.
*        ELSE.
*          ls_ztmm40301-erdat = sy-datum.
*          ls_ztmm40301-erzet = sy-uzeit.
*          ls_ztmm40301-ernam = sy-uname.
*        ENDIF.
*
*        ls_ztmm40301-aedat = sy-datum.
*        ls_ztmm40301-aezet = sy-uzeit.
*        ls_ztmm40301-aenam = sy-uname.
*
*        APPEND ls_ztmm40301 TO lt_ztmm40301.
*      ENDLOOP.
*    ENDLOOP.
*
*    TRY.
*        MODIFY ztmm40300 FROM TABLE lt_ztmm40300.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*    TRY.
*        MODIFY ztmm40301 FROM TABLE lt_ztmm40301.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*        MESSAGE s002(zcn00).  "데이타가 성공적으로 저장되었습니다.
*
*      CATCH cx_sy_open_sql_db INTO DATA(lo_oref2).
*        MESSAGE s013(zmm01). "DB Update System Error !
*    ENDTRY.
*
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  CASE 'X'.
    WHEN p_rd1a.  "Inbound Delivery 기준

      PERFORM get_data_inb.

      IF p_chk1 IS INITIAL. "백그라운드 처리 X

        IF gt_idheader[] IS NOT INITIAL.
          DESCRIBE TABLE gt_idheader LINES gv_tcnt.
          MESSAGE s011(zmm01) WITH gv_tcnt. " & 건의 데이타가 조회되었습니다.
          CALL SCREEN '0100'.
        ELSE.
          MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m01.  "조회 조건에 맞는 데이터가 없습니다.
        ENDIF.

      ELSE. "백그라운드 처리

        IF p_chk2 IS INITIAL.
          IF gt_idheader[] IS NOT INITIAL.
            PERFORM background_processing_inb.
          ELSE.
            MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m04.  "백그라운드 처리 할 대상이 없습니다.
          ENDIF.
        ELSE.
          MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m06.  "백그라운드 처리는 전송 가능 대상 건만 가능합니다.
        ENDIF.
      ENDIF.

    WHEN p_rd1b.    "PO기준

      IF p_chk3 = 'X' AND p_chk2 IS INITIAL.  " 삭제 PO 재전송 = 'X' and 전송 결과 조회 = ''
        MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m09. EXIT. "전송결과 조회 선택 시에만 실행이 가능합니다.
      ENDIF.

      PERFORM get_data_po.

      IF p_chk1 IS INITIAL. "백그라운드 처리 X

        IF gt_poheader[] IS NOT INITIAL.
          DESCRIBE TABLE gt_poheader LINES gv_tcnt.
          MESSAGE s011(zmm01) WITH gv_tcnt. " & 건의 데이타가 조회되었습니다.
          CALL SCREEN '0110'.
        ELSE.
          MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m01.  "조회 조건에 맞는 데이터가 없습니다.
        ENDIF.

      ELSE. "백그라운드 처리

        IF p_chk2 IS INITIAL.
          IF gt_poheader[] IS NOT INITIAL.
            PERFORM background_processing_po.
          ELSE.
            MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m04.  "백그라운드 처리 할 대상이 없습니다.
          ENDIF.

        ELSE.

          IF p_chk3 = 'X' OR p_chk4 = 'X'. " 전송결과조회 = 'X' AND 삭제 PO 재전송 = 'X'
            IF gt_poheader[] IS NOT INITIAL.
              PERFORM background_processing_po.
            ELSE.
              MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m04.  "백그라운드 처리 할 대상이 없습니다.
            ENDIF.
          ELSE.
            MESSAGE s000 DISPLAY LIKE 'E' WITH TEXT-m06.  "백그라운드 처리는 전송 가능 대상 건만 가능합니다.
          ENDIF.

        ENDIF.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN_CHK4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_screen_chk4 .

  CASE sy-ucomm.
    WHEN 'P4'.

      IF p_chk4 = 'X'.
        p_chk2 = 'X'.
      ENDIF.

    WHEN 'P2'.

      IF p_chk2 = space.
        p_chk4 = space.
      ENDIF.

  ENDCASE.

ENDFORM.
