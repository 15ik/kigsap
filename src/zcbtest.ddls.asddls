@AbapCatalog.sqlViewName: 'ZVTEST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'zcbtest'
define view zcbtest as select from ekko as _head
association[1..*] to ekpo as _item on _head.ebeln = _item.ebeln
{
    key _head.ebeln,
    _item.ebelp,
    _item.werks,
    _item.lgort 
}
