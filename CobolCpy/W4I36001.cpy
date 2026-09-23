000100 01  MID-W4I36001.                                                        
000200*                                 MID-COPYTEXT PGM W40360                 
000300*                                 ORDERBEKRÄFTELSE FÖR UTSKRIVNA          
000400*                                 DIREKTLEVERANSRADER                     
000500*                                                                         
000600     03 MID-DABEKDAT         PIC 9(8).                                    
000700*                                 ORDERBEKRÄFTELSEDATUM                   
000800*                                                                         
000900     03 MID-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDDISTR          PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR         PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDLEVNR          PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MID-IDORDNR7         PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MID-IDPRODNR         PIC X(7).                                    
002000*                                 PRODUKTIONSNUMMER                       
002100     03 MID-TIBEKR           PIC 9(6).                                    
002200*                                 KLOCKSLAG FÖR ORDERBEKRÄFTELSE          
002300     03 MID-IDUSER           PIC X(8).                                    
002400*                                 ANVÄNDARENS SÄKERHETS ID                
002500     03 MID-RAD              OCCURS 10 TIMES.                             
002600        05 MID-IDARTBET      PIC X(17).                                   
002700*                                 ARTIKELBETECKNING EFTERMARKNAD          
002800        05 MID-IDARTPRE      PIC X(3).                                    
002900*                                 IDENTIFIERARE ARTIKELSORTIMENT          
003000        05 MID-IDARTNR       PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200        05 MID-IDRADNR       PIC X(4).                                    
003300*                                 RADNUMMER                               
003400        05 MID-KDORDBEK      PIC X(2).                                    
003500*                                 ORDERBEKRÄFTELSEKOD                     
003600        05 MID-KVBEART       PIC 9(6).                                    
003700*                                 BESTÄLLT ANTAL STYCKEN                  
003800        05 MID-DALEVDAT      PIC X(8).                                    
003900*                                 FÖRSENAT LEVERANSDATUM                  
004000*** END OF VILMAII-COPY LENGTH= 543 BYTES                                 
