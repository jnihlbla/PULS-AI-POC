000100 01  W3714A.                                                              
000200*                                 BYTESUPPF÷LJNINGEN, UPPGIFTER           
000300*                                                                         
000400*                                 OM ALLA BYTESRAPPORTER I STATUS         
000500*                                  TV≈                                    
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDDISTR              PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 IDBYTRAP             PIC S9(7)           COMP-3.                  
001400*                                 RAPPORTNUMMER  BYTES                    
001500*                                 REPORTNUMBER   EXCHANGE                 
001600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800*                                 CUSTOMER NO                             
001900     03 KDBYTSTA             PIC X.                                       
002000*                                 STATUSKOD BYTESOBJEKT                   
002100*                                 STATUSCODE EXCH CORES                   
002200     03 KVRETUR              PIC S9(7)           COMP-3.                  
002300*                                 ANTAL I RETUR                           
002400*                                 QUANTITY IN RETURN                      
002500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002700*                                 REGISTRATION DATE (YYMMDD)              
002800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
