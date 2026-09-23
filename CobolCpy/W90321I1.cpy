000100 01  REQU-W90321I1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9032100          
000300     03 REQU-IDAPIORDREF     PIC X(23).                                   
000400*                                 API ORDERID(DIS+KND+ORD+DAT)            
000500     03 REQU-IDAPIORDREF-REP REDEFINES REQU-IDAPIORDREF.                  
000600        05 REQU-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 REQU-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 REQU-IDORDNR7     PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200        05 REQU-TIREGDAT     PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001400*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
