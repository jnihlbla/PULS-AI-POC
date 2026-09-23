000100 01  MID-W2I11301.                                                        
000200*                                 MID-COPYTEXT FÖR W2011300               
000300     03 MID-IDLEVNR-START-IN PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-START-UT PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-INFO-IDLEVNR     OCCURS 10 TIMES                              
000800                             INDEXED MID-INFO-IND                         
000900                             PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-IDLEVNR          PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-KDVECKOSL        PIC X.                                       
001400*                                 KOD    VECKOSLUTSSÄNDNING               
001500     03 MID-TISEND-PER       PIC 9(6).                                    
001600*                                 BEGÄRD ÖVERFÖRINGSDATUM                 
001700*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
