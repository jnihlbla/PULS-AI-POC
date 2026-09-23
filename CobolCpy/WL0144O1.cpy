000100 01  RESP-WL0144O1.                                                       
000200*                                 RESPONS FROM PGM WL0144                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001000*                                 ARTIKELNUMMER                           
001100     03 RESP-IDORDNR7-KEY    PIC Z(6)9.                                   
001200*                                 ORDERNUMMER                             
001300     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-IDDB-ENTER      PIC X(6).                                    
001800*                                 DATABAS                                 
001900     03 RESP-FLALLDC         PIC X.                                       
002000*                                 JA/NEJ-FLAGGA                           
002100     03 RESP-KVRADER         PIC Z(4)9.                                   
002200*                                 ANTAL RADER                             
002300     03 RESP-RAD             OCCURS 500 TIMES.                            
002400*                                                                         
002500        05 RESP-IDDC-RAD     PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 RESP-IDDISTR-RAD  PIC Z(3)9.                                   
002800*                                 DISTRIKTNUMMER                          
002900        05 RESP-IDKUNDNR-RAD PIC Z(5)9.                                   
003000*                                 KUNDNUMMER                              
003100        05 RESP-IDORDNR7-RAD PIC Z(6)9.                                   
003200*                                 ORDERNUMMER                             
003300        05 RESP-KDFRAKT-RAD  PIC Z9.                                      
003400*                                 FRAKTSÄTT DC TILL KUND                  
003500        05 RESP-KDORDKL-RAD  PIC 9.                                       
003600*                                 ORDERKLASS                              
003700        05 RESP-KDORDSTA-RAD PIC X(2).                                    
003800*                                 VOLVOORDERSTATUS                        
003900        05 RESP-KVBEART-Q-RAD                                             
004000                             PIC Z(5)9.                                   
004100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004200        05 RESP-TIREGDAT-RAD PIC 9(6).                                    
004300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004400        05 RESP-IDLEVNR-RAD  PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600        05 RESP-IDORDNR7-URS-RAD                                          
004700                             PIC Z(6)9.                                   
004800*                                 ORDERNUMMER                             
004900        05 RESP-IDVIN-RAD    PIC X(17).                                   
005000*                                 VIN ID FORDON                           
005100*** END OF VILMAII-COPY LENGTH= 32551 BYTES                               
