000100 01  MID-W4I67301.                                                        
000200*                                 MID-COPYTEXT FÖR W40673                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDTRPTNR-UT      PIC X(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 MID-IDLBBET-IN       PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 MID-IDLBBET-UT       PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100     03 MID-FLFARLIG-IN      PIC X.                                       
001200*                                 FARLIGT GODS-FLAGGA                     
001300     03 MID-FLFARLIG-UT      PIC X.                                       
001400*                                 FARLIGT GODS-FLAGGA                     
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-UPD              OCCURS 26 TIMES.                             
002000*                                 MID-COPYTEXT FÖR W40673                 
002100        05 MID-IDDISTR-UPD   PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300        05 MID-IDKUNDNR-UPD  PIC 9(6).                                    
002400*                                 KUNDNUMMER                              
002500        05 MID-IDORDNR7-UPD  PIC 9(7).                                    
002600*                                 ORDERNUMMER                             
002700        05 MID-IDKOLLI-UPD   PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900*** END OF VILMAII-COPY LENGTH= 608 BYTES                                 
