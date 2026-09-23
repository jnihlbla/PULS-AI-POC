000100 01  MID-W6I31201.                                                        
000200*                                 COPYTEXT FÖR MID W6I31201               
000300*                                                                         
000400     03 MID-IDDC             PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-KDSTOR-IN        PIC X(3).                                    
000700*                                 STORAGE CODE                            
000800     03 MID-KDSTOR-UT        PIC X(3).                                    
000900*                                 STORAGE CODE                            
001000     03 MID-KDSTOR-SPAR      PIC X(3).                                    
001100*                                 STORAGE CODE                            
001200     03 MID-KDSTOR-MAIN      PIC X(3).                                    
001300*                                 STORAGE CODE                            
001400     03 MID-DISTORD-MAIN     PIC X(5).                                    
001500*                                 STORAGE DEPTH                           
001600     03 MID-DISTORB-MAIN     PIC X(5).                                    
001700*                                 LAGERPLATS BREDD                        
001800     03 MID-DISTORH-MAIN     PIC X(5).                                    
001900*                                 STORAGE HEIGHT                          
002000     03 MID-TESTORAGE-MAIN   PIC X(18).                                   
002100*                                 STORAGE INFORMATION                     
002200     03 MID-KDVSOP-MAIN      OCCURS 5 TIMES                               
002300                             PIC 9(3).                                    
002400*                                 VSOP-KOD                                
002500     03 MID-KDANDR-MAIN      PIC X.                                       
002600*                                 ÄNDRINGSKOD                             
002700*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
