@AbapCatalog.sqlViewName: 'ZCCMM_BANKACCNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '공급업체 계좌 정보'

define view ZSVCMM_BANKACCOUNT as select from zsvcmm_bank_acc as A


   {
       key A.supplier,
       key A.bankcountry,

        bank,
        banka,
        bankaccount,
        bpbankaccountinternalid,
        bankaccountholdername,
        bankcontrolkey,
        bankdetailreference,
        iban,
        bankvaliditystartdate,
        bankvalidityenddate,

      tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                     $session.client,
                     'NULL' )  as currentdate,
        bankaccountname


} where currentdate <= bankvalidityenddate and currentdate >= bankvaliditystartdate
