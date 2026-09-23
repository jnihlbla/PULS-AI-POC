000100 01  REQU-W40668I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W40668                
000300*                                                                         
000400     03 REQU-TIDATUM-KEY     PIC X(6).                                    
000500*                                 DATUM ENLIGT KDDATFORM                  
000600     03 REQU-IDDC-KEY        PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 REQU-KVRADER         PIC 9(5).                                    
000900*                                 ANTAL RADER                             
001000     03 REQU-INPUT           OCCURS 500 TIMES.                            
001100*                                                                         
001200        05 REQU-KDSVAR-LINE  PIC X.                                       
001300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001400        05 REQU-IDTRPTNR-LINE                                             
001500                             PIC 9(3).                                    
001600*                                 TRANSPORTIDENTITET                      
001700        05 REQU-IDLBBET-LINE PIC X(12).                                   
001800*                                 LASTBÄRARBETECKNING                     
001900*** END OF VILMAII-COPY LENGTH= 8013 BYTES                                
