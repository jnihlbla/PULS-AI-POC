000100 01  RESP-WL0180O1.                                                       
000200     03 RESP-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 RESP-IDTRPTNR-KEY    PIC Z(2)9.                                   
000500*                                 TRANSPORT IDENTITY                      
000600     03 RESP-IDLBBET-KEY     PIC X(12).                                   
000700*                                 TRAILER NUMBER                          
000800     03 RESP-TISKEPPN-KEY    PIC 9(6).                                    
000900*                                 SHIPPING DATE    (YYMMDD)               
001000     03 RESP-KVRADER         PIC Z(4)9.                                   
001100*                                 NUMBER OF LINES                         
001200     03 RESP-OUTPUT          OCCURS 500 TIMES.                            
001300        05 RESP-KDCMD        PIC X(4).                                    
001400        05 RESP-IDLBBET      PIC X(12).                                   
001500*                                 TRAILER NUMBER                          
001600        05 RESP-TISKEPPN     PIC X(6).                                    
001700*                                 SHIPPING DATE    (YYMMDD)               
001800        05 RESP-TISKPTID     PIC 9(6).                                    
001900        05 RESP-IDSHIPM      PIC Z(6)9.                                   
002000*                                 SHIPMENT NO                             
002100        05 RESP-IDMSG-ERROR-LINE                                          
002200                             PIC X(3).                                    
002300*                                 ERROR MESSAGE ID                        
002400*** END OF VILMAII-COPY LENGTH= 19028 BYTES                               
