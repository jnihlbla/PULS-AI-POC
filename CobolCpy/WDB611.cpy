000100 01  URV-WDB611.                                                          
000200*                                 DC STYRREGISTER                         
000300*                                 UNDANTAG/URVAL AV DIV. ELEMENT          
000400*                                 FYSISK NYCKEL: WDB611KY                 
000500*                                 (TEELMT + IDELMT-GRP)                   
000600     03 URV-TEELMT           PIC X(16).                                   
000700*                                 NAMN FÖR ETT DATAELEMENT                
000800*                                 NAME OF AN ITEM                         
000900     03 URV-IDELMT-GRP.                                                   
001000*                                 ATT LEVERERA/ELLER EJ FRÅN DC           
001100*                                 WHAT TO HANDLE/OR NOT AT THE DC         
001200        05 URV-FILLER        PIC X(20).                                   
001300        05 URV-IDARTNR-EXCP-FILLER REDEFINES URV-FILLER.                  
001400           07 URV-IDARTNR-EXCP                                            
001500                             PIC 9(9).                                    
001600*                                 ARTIKEL RETURNERS EJ TILL DC            
001700*                                 PART NOT ALLOWED AT DC                  
001800           07 FILLER         PIC X(11).                                   
001900        05 URV-IDFKNGRP-EXCP-FILLER REDEFINES URV-FILLER.                 
002000           07 URV-IDFKNGRP-EXCP                                           
002100                             PIC 9(4).                                    
002200*                                 FUNKTIONSGR. EJ TILLÅTET PÅ DC          
002300*                                 FUNC.GROUP NOT ALLOWED AT DC            
002400           07 FILLER         PIC X(16).                                   
002500        05 URV-KDANMORS-RET-FILLER REDEFINES URV-FILLER.                  
002600           07 URV-KDANMORS-RET                                            
002700                             PIC X(2).                                    
002800*                                 ANM.ORSKAK TILLÅTEN PÅ DC               
002900*                                 DISCR.REPORT CODE ALLOWED AT DC         
003000           07 FILLER         PIC X(18).                                   
003100        05 URV-IDDC-EXCP-FILLER REDEFINES URV-FILLER.                     
003200           07 URV-IDDC-EXCP  PIC X(2).                                    
003300*                                 DC FÖR RETUR EJ TILLÅTET                
003400*                                 DC NOT ALLOWED FOR RETURN               
003500           07 FILLER         PIC X(18).                                   
003600*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
