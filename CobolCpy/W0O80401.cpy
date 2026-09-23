000100 01  W0O80401.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W0O80401                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 KDFRAKT-IN           PIC X(3).                                    
000900*                                 FRAKTSÄTT DC TILL KUND                  
001000     03 UPPDAT-IN            PIC X.                                       
001100*                                 UPPDATERINGSTYP                         
001200     03 KDFRAKT-UT           PIC X(3).                                    
001300*                                 FRAKTSÄTT DC TILL KUND                  
001400     03 UPPDAT-UT            PIC X.                                       
001500*                                 UPPDATERINGSTYP                         
001600     03 FRAKTTEXT-GRUPP.                                                  
001700        05 FRAKTTEXTER       OCCURS 6 TIMES.                              
001800           07 FRAKTTEXT-ATTR PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000           07 FRAKTTEXT      PIC X(20).                                   
002100*                                 FRAKTTEXTER SVE, ENG, FRA               
002200*                                 SPA, TYS, ITA                           
002300     03 MESSAGE-RAD23.                                                    
002400        05 MESSAGE-C1        PIC X(40).                                   
002500        05 MESSAGE-C2        PIC X(40).                                   
002600*** END OF VILMAII-COPY LENGTH= 264 BYTES                                 
