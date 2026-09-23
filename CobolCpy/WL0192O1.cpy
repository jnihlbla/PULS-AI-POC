000100 01  RESP-WL0192O1.                                                       
000200*                                 RESPONSE   TO WL0192.                   
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-TIDATUM-KEY     PIC X(6).                                    
000700*                                 DATUM ENLIGT KDDATFORM                  
000800*                                 DATE AS SPECIFIED BY KDDATFORM          
000900     03 RESP-KVRADER         PIC Z(4)9.                                   
001000*                                 ANTAL RADER                             
001100*                                 NUMBER OF LINES                         
001200     03 RESP-RADER           OCCURS 500 TIMES.                            
001300*                                                                         
001400        05 RESP-KDSVAR       PIC X.                                       
001500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001600*                                 RETURN CODE FROM PROGRAM                
001700        05 RESP-IDTRPTNR     PIC Z(2)9.                                   
001800*                                 TRANSPORTIDENTITET                      
001900*                                 TRANSPORT IDENTITY                      
002000        05 RESP-IDLBBET      PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*                                 TRAILER NUMBER                          
002300        05 RESP-IDMSG-ERROR-LINE                                          
002400                             PIC X(3).                                    
002500*                                 FELMEDDELANDE ID                        
002600*                                 ERROR MESSAGE ID                        
002700*** END OF VILMAII-COPY LENGTH= 9513 BYTES                                
