000100 01  MID-W2I11701.                                                        
000200*                                 MID-COPYTEXT FÖR W2011700               
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDDC-IN          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INFO-RAD         OCCURS 10 TIMES                              
000800                             INDEXED MID-INFO-IND.                        
000900*                                 RADINFORMATION                          
001000        05 MID-INFO-IDLEVNR  PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200        05 MID-INFO-IDDC     PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MID-IDLEVNR          PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 MID-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MID-KDVECKOSL        PIC X.                                       
001900*                                 KOD    VECKOSLUTSSÄNDNING               
002000*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
