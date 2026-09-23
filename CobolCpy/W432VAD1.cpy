000100 01  W432VAD1-CTX.                                                        
000200*                                 URVAL FRÅN KUNDREGISTRET                
000300*                                 TILL VADIS                              
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 IDLANDX2             PIC X(2).                                    
001200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001300     03 DASTADAT             PIC 9(8).                                    
001400*                                 GENERELLT STARTDATUM                    
001500     03 DASTODAT             PIC 9(8).                                    
001600*                                 GENERELLT STOPPDATUM                    
001700     03 BEGMT-RAD1           PIC X(35).                                   
001800*                                 GODSMOTTAGARNAMN RAD 1                  
001900*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
