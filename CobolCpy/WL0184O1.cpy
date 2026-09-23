000100 01  RESP-WL0184O1.                                                       
000200*                                 RESPONS FROM PGM WL0184                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 RESP-IDTRPTNR-KEY    PIC 9(3).                                    
000700*                                 TRANSPORTIDENTITET                      
000800*                                 TRANSPORT IDENTITY                      
000900     03 RESP-KVRADER-MAX     PIC 9(5).                                    
001000*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001100*                                 EDAN.                                   
001200     03 RESP-RADER           OCCURS 1 TO 500 TIMES                        
001300                             DEPENDING ON RESP-KVRADER-MAX.               
001400*                                                                         
001500        05 RESP-IDTRPTNR     PIC 9(3).                                    
001600*                                 TRANSPORTIDENTITET                      
001700*                                 TRANSPORT IDENTITY                      
001800        05 RESP-IDLBBET      PIC X(12).                                   
001900*                                 LASTBÄRARBETECKNING                     
002000*                                 TRAILER NUMBER                          
002100*** END OF VILMAII-COPY LENGTH= 7510 BYTES                                
