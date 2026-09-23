000100 01  RESP-W90314O1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9031400          
000300*                                                                         
000400     03 RESP-IDAPIORDREF     PIC X(23).                                   
000500*                                 API ORDERID(DIS+KND+ORD+DAT)            
000600     03 RESP-IDAPIORDREF-REP REDEFINES RESP-IDAPIORDREF.                  
000700        05 RESP-IDDISTR      PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900        05 RESP-IDKUNDNR     PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100        05 RESP-IDORDNR7     PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300        05 RESP-TIREGDAT     PIC 9(6).                                    
001400*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001500     03 RESP-IDMFSINF        PIC X(3).                                    
001600*                                 MFS INFO. MEDDELANDE NUMMER             
001700     03 RESP-TEMFSINF        PIC X(55).                                   
001800*                                 INFORMATIONSMEDDELANDE                  
001900*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
