000100 01  W27185.                                                              
000200*                                 ANVÄNDS FÖR LEDTIDSUPPFÖLJNING          
000300*                                 I REFILL-SYSTEMET                       
000400     03 VECKA                PIC 9(4).                                    
000500*                                 ÅR - VECKA  (ÅÅVV)                      
000600*                                 YEAR - WEEK  (YYWW)                     
000700     03 IDDC-RECV            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDDC-SEND            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 TOT-DAGAR            PIC 9(8).                                    
001400     03 PRODUKT-REC-BIN      PIC 9(8).                                    
001500     03 AVERAGE              PIC 9(4)V9(1).                               
001600     03 90PROC               PIC 9(3).                                    
001700     03 PRODUKT-TOT          PIC 9(8).                                    
001800     03 AVERAGE-LT           PIC 9(3).                                    
001900     03 90PROC-LT            PIC 9(3).                                    
002000*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  
