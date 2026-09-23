000100 01  MID-W4I36201.                                                        
000200*                                 COPYTEXT FÖR MID W4I36201               
000300     03 MID-IDHLOTAB-IN      PIC X(2).                                    
000400*                                 HLOTABELLSIDENTITET                     
000500     03 MID-IDHLOTAB-UT      PIC X(2).                                    
000600*                                 HLOTABELLSIDENTITET                     
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDHLO-IN         PIC X(2).                                    
001200*                                 HUVUDLAGEROMRÅDE                        
001300     03 MID-IDHLO-UT         PIC X(2).                                    
001400*                                 HUVUDLAGEROMRÅDE                        
001500     03 MID-INPUT.                                                        
001600*                                 INDATA FÖR UPPDATERING                  
001700        05 MID-W4I36201-002-GRP                                           
001800                             OCCURS 5 TIMES.                              
001900*                                 INDATA FÖR UPPDATERING                  
002000           07 MID-ADLAGOMR-UPDATE                                         
002100                             PIC 9(2).                                    
002200*                                 LAGEROMRÅDE                             
002300           07 MID-IDHLO-UPDATE                                            
002400                             PIC 9(2).                                    
002500*                                 HUVUDLAGEROMRÅDE                        
002600*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
