000100 01  W3714C.                                                              
000200*                                 BYTESUPPFÖLJNINGEN, UPPGIFTER           
000300*                                                                         
000400*                                 OM ALLA BYTESRAPPORTER I STATUS         
000500*                                  FYRA                                   
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDDISTR              PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 KDBYTSTA             PIC X.                                       
001400*                                 STATUSKOD BYTESOBJEKT                   
001500*                                 STATUSCODE EXCH CORES                   
001600     03 KVRETUR              PIC S9(7)           COMP-3.                  
001700*                                 ANTAL I RETUR                           
001800*                                 QUANTITY IN RETURN                      
001900*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
