FUNCTION-POOL ZKIG_CN_0010.                 "MESSAGE-ID ..

*> 자재, VENDOR 누락 시 자동 설정하기 위함.
DATA: GT_EINA_OLD TYPE TABLE OF EINA,
      GT_EINE_OLD TYPE TABLE OF EINE.


DATA: BEGIN OF GS_MODE,
      INFNR LIKE EINE-INFNR,
      EKORG LIKE EINE-EKORG,
      ESOKZ LIKE EINE-ESOKZ,
      WERKS LIKE EINE-WERKS,
      MODE,
      END OF GS_MODE,
      GT_MODE LIKE TABLE OF GS_MODE.

* INCLUDE LZKIG_CN_0010D...                  " Local class definition
