000100 01  RESP-WL0181O1.                                                       
000200     03 RESP-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000500*                                 DISTRICT NUMBER                         
000600     03 RESP-IDKUNDNR-KEY    PIC Z(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 RESP-WL0182I1.                                                    
000900        05 RESP-IDSHIPM-KEY  PIC Z(6)9.                                   
001000*                                 SHIPMENT NO                             
001100        05 RESP-IDTRPTNR-KEY PIC Z(2)9.                                   
001200*                                 TRANSPORT IDENTITY                      
001300     03 RESP-KVRADER         PIC Z(4)9.                                   
001400*                                 NUMBER OF LINES                         
001500     03 RESP-SHIPMENT        OCCURS 500 TIMES.                            
001600        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
001700*                                 CUSTOMER NO                             
001800        05 RESP-IDSHIPM      PIC Z(6)9.                                   
001900*                                 SHIPMENT NO                             
002000        05 RESP-IDTRPTNR     PIC Z(2)9.                                   
002100*                                 TRANSPORT IDENTITY                      
002200        05 RESP-IDLBBET      PIC X(12).                                   
002300*                                 TRAILER NUMBER                          
002400        05 RESP-TISKPTID     PIC 9(6).                                    
002500        05 RESP-TISKEPPN     PIC 9(6).                                    
002600*                                 SHIPPING DATE    (YYMMDD)               
002700*** END OF VILMAII-COPY LENGTH= 20027 BYTES                               
