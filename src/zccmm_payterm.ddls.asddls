@AbapCatalog.sqlViewName: 'ZSVCMM_PAYTERM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MM 지급조건용'
define view zccmm_payterm as select from t052
left outer join t052u
on t052.zterm = t052u.zterm
and t052u.spras = '3'
{
    key t052u.zterm,
    text1
}
where t052.koart = 'K' or t052.koart = ''
