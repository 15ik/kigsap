@AbapCatalog.sqlViewName: 'ZSVCMM_BANK_ACC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '공급업체 은행 계좌 유효기간'
define view ZCCMM_BANK_ACC as select from I_BusinessPartnerBankTP as A
{
       key A.BusinessPartner as Supplier,
       key A.BankCountryKey as BankCountry,

        BankNumber as Bank,
        BankName as banka,
        BankAccount,
        BankIdentification as BPBankAccountInternalID,
        BankAccountHolderName,
        BankControlKey,
        BankAccountReferenceText as BankDetailReference,
        IBAN,
        BankValidityStartDate,
        BankValidityEndDate,

      tstmp_to_dats( tstmp_current_utctimestamp(),
                     abap_system_timezone( $session.client,'NULL' ),
                     $session.client,
                     'NULL' )  as currentdate,
        BankAccountName  
}
