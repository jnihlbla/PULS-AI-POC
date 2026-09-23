000100 01  6316-WDGX6316.                                                       
000200*                                 SDC  + NDC STORAGE TAB                  
000300*                                 FYSISK NYCKEL:                          
000400*                                 KDFREQ                                  
000500     03 6316-KDSTOR          PIC X(3).                                    
000600*                                 STORAGE CODE                            
000700*                                 STORAGE CODE                            
000800     03 6316-DISTORD         PIC 9(3)V9(1).                               
000900*                                 STORAGE DEPTH                           
001000*                                 STORAGE DEPTH                           
001100     03 6316-DISTORH         PIC 9(3)V9(1).                               
001200*                                 STORAGE HEIGHT                          
001300*                                 STORAGE HEIGHT                          
001400     03 6316-DISTORB         PIC 9(3)V9(1).                               
001500*                                 LAGERPLATS BREDD                        
001600*                                 STORAGE WIDTH                           
001700     03 6316-TESTORAGE       PIC X(18).                                   
001800*                                 STORAGE INFORMATION                     
001900*                                 STORAGE INFORMATION                     
002000     03 6316-KDVSOP          OCCURS 5 TIMES                               
002100                             PIC S9(3)           COMP-3.                  
002200*                                 VSOP-KOD                                
002300*                                 VSOP-CODE                               
002400*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
