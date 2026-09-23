000100 01  MID-W2I34901.                                                        
000200*                                 MID-COPYTEXT FÖR W2034900               
000300     03 MID-IDDC-SEND-IN     PIC X(2).                                    
000400*                                 SÄNDANDE LAGER                          
000500     03 MID-IDDC-REC-IN      PIC X(2).                                    
000600*                                 MOTTAGANDE LAGER                        
000700     03 MID-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MID-IDDC-SEND-UT     PIC X(2).                                    
001000*                                 SÄNDANDE LAGER                          
001100     03 MID-IDDC-REC-UT      PIC X(2).                                    
001200*                                 MOTTAGANDE LAGER                        
001300     03 MID-KDARBTYP-UT      PIC X(8).                                    
001400*                                 TYP AV ARBETE                           
001500     03 MID-INPUT.                                                        
001600        05 MID-RAD           OCCURS 22 TIMES.                             
001700           07 MID-CMD        PIC X.                                       
001800        05 MID-NY-IDDC       PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 MID-NY-IDDISTR    PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200        05 MID-NY-IDKUNDNR   PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
