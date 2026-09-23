000100 01  RESP-W60134O1.                                                       
000200*                                 RESPCOPYTEXT TILL W6013400              
000300*                                                                         
000400     03 RESP-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RESP-LINE            OCCURS 500 TIMES.                            
000700        05 RESP-ADINLOMR     PIC X(4).                                    
000800*                                 INLEVERANSOMRÅDE                        
000900        05 RESP-KVRADER-TOT  PIC 9(5).                                    
001000*                                 TOTALT ANTAL RADER                      
001100        05 RESP-KVRADER-PRIO PIC 9(5).                                    
001200*                                 ANTAL PRIORITERADE RADER                
001300*** END OF VILMAII-COPY LENGTH= 7005 BYTES                                
