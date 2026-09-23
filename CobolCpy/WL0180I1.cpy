000100 01  REQU-WL0180I1.                                                       
000200     03 REQU-IDDC-KEY        PIC X(2).                                    
000300*                                 WAREHOUSE IDENTIFIER                    
000400     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000500*                                 TRANSPORT IDENTITY                      
000600     03 REQU-IDLBBET-KEY     PIC X(12).                                   
000700*                                 TRAILER NUMBER                          
000800     03 REQU-TISKEPPN-KEY    PIC 9(6).                                    
000900*                                 SHIPPING DATE    (YYMMDD)               
001000     03 REQU-KVRADER         PIC 9(5).                                    
001100*                                 NUMBER OF LINES                         
001200     03 REQU-INPUT           OCCURS 500 TIMES.                            
001300        05 REQU-KDCMD        PIC X(4).                                    
001400        05 REQU-IDSHIPM      PIC 9(7).                                    
001500*                                 SHIPMENT NO                             
001600*** END OF VILMAII-COPY LENGTH= 5528 BYTES                                
