000100 01  REQU-W40712I1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W4071200          
000300*                                                                         
000400     03 REQU-IDAPIDISCREF    PIC X(17).                                   
000500*                                 API DISCRID(DIS+KND+DRNO)               
000600     03 REQU-IDAPIDISCREF-REP REDEFINES REQU-IDAPIDISCREF.                
000700        05 REQU-IDDISTR      PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900        05 REQU-IDKUNDNR     PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100        05 REQU-IDRAPPNR     PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
