000100 01  REQU-WL0181I1.                                                       
000200     03 REQU-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000500*                                 DISTRICT NUMBER                         
000600     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 REQU-KVRADER         PIC 9(5).                                    
000900*                                 NUMBER OF LINES                         
001000     03 REQU-INPUT           OCCURS 500 TIMES.                            
001100        05 REQU-IDSHIPM      PIC 9(7).                                    
001200*                                 SHIPMENT NO                             
001300        05 REQU-IDTRPTNR     PIC 9(3).                                    
001400*                                 TRANSPORT IDENTITY                      
001500*** END OF VILMAII-COPY LENGTH= 5017 BYTES                                
