000100 01  LAST-W411LAST.                                                       
000200*                                 LÄNKAREA TILL W411LAST -                
000300*                                 KONTROLLERA ENHETSLAST                  
000400     03 LAST-INDATA.                                                      
000500        05 LAST-ADLAGOMR     PIC S9(3)           COMP-3.                  
000600*                                 LAGEROMRÅDE                             
000700        05 LAST-FLFORBI      PIC X.                                       
000800*                                 FÖRBIORDERFLAGGA                        
000900        05 LAST-FLOVRLEV     PIC X.                                       
001000*                                 ÖVERLEVERANS                            
001100        05 LAST-FLORDSPE     PIC X.                                       
001200*                                 SPECIALORDERFLAGGA                      
001300        05 LAST-IDLEVNR      PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500        05 LAST-IDDC         PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700        05 LAST-KDFDKRAV     PIC S9(3)           COMP-3.                  
001800*                                 TRANSPORTFÖRPACKNINGSKOD                
001900        05 LAST-KVPREAVB     PIC S9(7)           COMP-3.                  
002000*                                 PREL-AVB KVANT                          
002100        05 LAST-KVQPACK-3    PIC S9(5)           COMP-3.                  
002200*                                 ANTAL I Q3 FÖRPACKNING                  
002300        05 LAST-KVQPACK-4    PIC S9(5)           COMP-3.                  
002400*                                 ANTAL I Q4 FÖRPACKNING                  
002500     03 LAST-UTDATA.                                                      
002600        05 LAST-ADGANG-UT    PIC S9(3)           COMP-3.                  
002700*                                 GÅNG                                    
002800        05 LAST-ADLAGOMR-UT  PIC S9(3)           COMP-3.                  
002900*                                 LAGEROMRÅDE                             
003000        05 LAST-KVANTAL-UT   PIC S9(7)           COMP-3.                  
003100*                                 ANTAL                                   
003200        05 LAST-KVBEART-UT   PIC S9(7)           COMP-3.                  
003300*                                 BESTÄLLT ANTAL STYCKEN                  
003400*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
