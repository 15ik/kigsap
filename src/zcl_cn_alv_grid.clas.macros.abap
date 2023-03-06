*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE _g_set_msgtb.
  CLEAR as_msgtb.

  as_msgtb = VALUE #( fieldname = &3 msgty = 'E' arbgb = &4 txtnr = &5
                      msgv1 = &6  msgv2 = &7  msgv3 = &8 ).
  me->set_msgtb( EXPORTING iv_delete = &1 is_msgtb = as_msgtb
                 CHANGING  cs_data = &2 ).

END-OF-DEFINITION.
DEFINE _g_set_value.
  &1 = &2.
END-OF-DEFINITION.
