000100 01  URET-WDA811.                                                         
000200*                                 RETUR SPÄRRAR                           
000300*                                 UNDANTAGS DATA FÖR RETURER              
000400*                                 FYSISK NYCKEL: WDA811KY                 
000500*                                 (TEELMT + IDELMT-RET)                   
000600     03 URET-TEELMT          PIC X(16).                                   
000700*                                 NAMN FÖR ETT DATAELEMENT                
000800*                                 NAME OF AN ITEM                         
000900     03 URET-IDELMT-RET.                                                  
001000*                                 EJ TILLÅTET FÖR RETURER                 
001100*                                 WHAT IS NOT ALLOWED FOR RETURNS         
001200        05 URET-IDELMT       PIC X(16).                                   
001300*                                 DATAELEMENTIDENTITET / VÄRDE            
001400*                                 VALUE OF AN ITEM                        
001500        05 URET-IDARTNR-FILLER REDEFINES URET-IDELMT.                     
001600           07 URET-IDARTNR   PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900           07 FILLER         PIC X(7).                                    
002000        05 URET-IDLEVNR-FILLER REDEFINES URET-IDELMT.                     
002100           07 URET-IDLEVNR   PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002400           07 FILLER         PIC X(11).                                   
002500        05 URET-IDFKNGRP-FILLER REDEFINES URET-IDELMT.                    
002600           07 URET-IDFKNGRP  PIC 9(4).                                    
002700*                                 FUNKTIONSGRUPP                          
002800*                                 FUNCTION GROUP                          
002900           07 FILLER         PIC X(12).                                   
003000        05 URET-KDFARLIG-FILLER REDEFINES URET-IDELMT.                    
003100           07 URET-KDFARLIG  PIC 9.                                       
003200*                                 KOD FÖR FARLIGT GODS                    
003300*                                 DANGEROUS GOODS CODE                    
003400           07 FILLER         PIC X(15).                                   
003500        05 URET-KDSORT-FILLER REDEFINES URET-IDELMT.                      
003600           07 URET-KDSORT    PIC X(2).                                    
003700*                                 SORT-KOD                                
003800*                                 UNIT OF MEASURE                         
003900           07 FILLER         PIC X(14).                                   
004000        05 URET-KDPRODSL-FILLER REDEFINES URET-IDELMT.                    
004100           07 URET-KDPRODSL  PIC 9(2).                                    
004200*                                 PRODUKTSLAG                             
004300*                                 PRODUCT GROUP                           
004400           07 FILLER         PIC X(14).                                   
004500*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
