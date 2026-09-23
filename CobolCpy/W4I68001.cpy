000100 01  MID-W4I68001.                                                        
000200*                                 MID-COPYTEXT PGM W40680                 
000300*                                 ANNULLATIONSBEGÄRAN                     
000400*                                 DIREKTLEVERANSRADER                     
000500*                                                                         
000600     03 MID-DABEKDAT         PIC X(8).                                    
000700*                                 ORDERBEKRÄFTELSEDATUM                   
000800*                                                                         
000900     03 MID-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDDISTR          PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR         PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDLEVNR          PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MID-IDORDNR7         PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MID-IDPRODNR         PIC 9(7).                                    
002000*                                 PRODUKTIONSNUMMER                       
002100     03 MID-IDPLKLST         PIC 9(3).                                    
002200*                                 PLOCKLISTNUMMER                         
002300     03 MID-TIBEKR           PIC X(6).                                    
002400*                                 KLOCKSLAG FÖR ORDERBEKRÄFTELSE          
002500     03 MID-RAD              OCCURS 1 TIMES.                              
002600        05 MID-IDARTNR       PIC 9(9).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MID-IDRADNR       PIC 9(4).                                    
002900*                                 RADNUMMER                               
003000        05 MID-KVBEART       PIC 9(6).                                    
003100*                                 BESTÄLLT ANTAL STYCKEN                  
003200*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
