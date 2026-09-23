000100 01  MID-W4I22801.                                                        
000200*                                 MID-COPYTEXT FÖR W4022800               
000300     03 MID-IDDISTR-IN       PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR7-IN      PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MID-IDORDNR7-UT      PIC 9(7).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-FLORDLEV-IN      PIC X.                                       
001600*                                 FLAGGA LEVERANSORDERNUMMER              
001700     03 MID-FLORDLEV-UT      PIC X.                                       
001800*                                 FLAGGA LEVERANSORDERNUMMER              
001900     03 MID-RAD              OCCURS 15 TIMES                              
002000                             INDEXED MID-IX.                              
002100*                                                                         
002200        05 MID-KDBEHX-EDIT   PIC X.                                       
002300*                                 BEHANDLINGSKOD-X                        
002400*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
