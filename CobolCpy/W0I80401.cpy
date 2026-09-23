000100 01  W0I80401.                                                            
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W0I80401                                
000400     03 KDFRAKT-IN           PIC X(3).                                    
000500*                                 FRAKTSÄTT DC TILL KUND                  
000600     03 KDFRAKT-UT           PIC X(3).                                    
000700*                                 FRAKTSÄTT DC TILL KUND                  
000800     03 UPPDAT-IN            PIC X.                                       
000900*                                 UPPDATERINGSTYP                         
001000     03 UPPDAT-UT            PIC X.                                       
001100*                                 UPPDATERINGSTYP                         
001200     03 FRAKTTEXT-GRUPP.                                                  
001300        05 FRAKTTEXTER       OCCURS 6 TIMES.                              
001400           07 FRAKTTEXT      PIC X(20).                                   
001500*                                 FRAKTTEXTER SVE, ENG, FRA               
001600*                                 SPA, TYS, ITA                           
001700*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
