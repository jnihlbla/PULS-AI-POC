000100 01  REQU-WL0192I1.                                                       
000200*                                 REQUEST TO PGM WL0192                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-TIDATUM-KEY     PIC X(6).                                    
000700*                                 DATUM ENLIGT KDDATFORM                  
000800*                                 DATE AS SPECIFIED BY KDDATFORM          
000900     03 REQU-KVRADER         PIC 9(5).                                    
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 REQU-INPUT           OCCURS 500 TIMES.                            
001300*                                                                         
001400        05 REQU-KDSVAR       PIC X.                                       
001500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001600*                                 RETURN CODE FROM PROGRAM                
001700        05 REQU-IDTRPTNR     PIC 9(3).                                    
001800*                                 TRANSPORTIDENTITET                      
001900*                                 TRANSPORT IDENTITY                      
002000        05 REQU-IDLBBET      PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*                                 TRAILER NUMBER                          
002300*** END OF VILMAII-COPY LENGTH= 8013 BYTES                                
