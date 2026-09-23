000100 01  RESP-W40704O1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W4070400          
000300*                                                                         
000400     03 RESP-IDAPIDISCREF    PIC X(17).                                   
000500*                                 API DISCRID(DIS+KND+DRNO)               
000600     03 RESP-IDAPIDISCREF-REP REDEFINES RESP-IDAPIDISCREF.                
000700        05 RESP-IDDISTR      PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900        05 RESP-IDKUNDNR     PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100        05 RESP-IDRAPPNR     PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 RESP-IDMFSINF        PIC X(3).                                    
001400*                                 MFS INFO. MEDDELANDE NUMMER             
001500     03 RESP-TEMFSINF        PIC X(55).                                   
001600*                                 INFORMATIONSMEDDELANDE                  
001700*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
