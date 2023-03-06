FUNCTION-POOL ZKIG_CN_0001.                 "MESSAGE-ID ..

DATA : gv_authority LIKE bapiflag-bapiflag,
       gv_price_from_po LIKE  bapiflag-bapiflag,
       gv_purchaseorder LIKE  bapimepoheader-po_number,
       gv_header        LIKE  bapimepoheader.

DATA : gs_poheader LIKE bapimepoheader,
       gs_poheaderx LIKE  bapimepoheaderx.

DATA : gt_return LIKE TABLE OF bapiret2,

       gt_poitem                 LIKE TABLE OF bapimepoitem,
       gt_poitemx                LIKE TABLE OF bapimepoitemx,

       gt_poaddrdelivery         LIKE TABLE OF bapimepoaddrdelivery,

       gt_poschedule             LIKE TABLE OF bapimeposchedule,
       gt_poschedulex            LIKE TABLE OF bapimeposchedulx,

       gt_poaccount              LIKE TABLE OF bapimepoaccount,
       gt_poaccountx             LIKE TABLE OF bapimepoaccountx,

       "[U2 변경시작 2022.07.14].
       gt_poaccountprofitsegment LIKE TABLE OF bapimepoaccountprofitsegment,
       "[U2 변경종료 2022.07.14].

       gt_pocond                 LIKE TABLE OF bapimepocond,
       gt_pocondx                LIKE TABLE OF bapimepocondx,

       gt_extensionin            LIKE TABLE OF bapiparex,

       gt_potextheader           LIKE TABLE OF bapimepotextheader,

       gt_popartner              LIKE TABLE OF bapiekkop,

       gt_pocomponents           LIKE TABLE OF bapimepocomponent,
       gt_pocomponentsx          LIKE TABLE OF bapimepocomponentx,

       gt_poshipping             LIKE TABLE OF bapiitemship,
       gt_poshippingx            LIKE TABLE OF bapiitemshipx,

       gs_return_commit          LIKE bapiret2.


DEFINE _g_return.

  ct_return[] = VALUE #( ( type = 'E' message  = &1  id = 'ZMM01') ).

end-OF-DEFINITION.

* INCLUDE LZKIG_CN_0001D...                  " Local class definition
