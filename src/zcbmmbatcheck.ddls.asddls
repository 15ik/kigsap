@AbapCatalog.sqlViewName: 'ZSVBMMBATCHECK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]배치 최종 번호 체크'

define view ZCBMMBATCHECK 
as 
select from mch1 
         association [1]   to nriv as nriv  on nriv.object = 'BATCH_CLT'  and nriv.nrrangenr = '01'
{
  key matnr,
      charg,
      LENGTH(charg) as LEN,      
      
      CONCAT(cast(INSTR(charg, '-' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'A' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'B' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'C' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'D' ) as abap.char(11)),      
      CONCAT(cast(INSTR(charg, 'E' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'F' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'G' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'H' ) as abap.char(11)),  
      CONCAT(cast(INSTR(charg, 'I' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'J' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'K' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'L' ) as abap.char(11)),  
      CONCAT(cast(INSTR(charg, 'M' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'N' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'O' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'P' ) as abap.char(11)),  
      CONCAT(cast(INSTR(charg, 'Q' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'R' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'S' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'T' ) as abap.char(11)),  
      CONCAT(cast(INSTR(charg, 'U' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'V' ) as abap.char(11)),
      CONCAT(cast(INSTR(charg, 'W' ) as abap.char(11)),      
      CONCAT(cast(INSTR(charg, 'X' ) as abap.char(11)),        
      CONCAT(cast( INSTR(charg, 'Y' ) as abap.char( 11)) ,  cast(INSTR(charg, 'Z' ) as abap.char(11)))))))))))))))))))))))))))) as TYPECHECK                                                 
      
} 

