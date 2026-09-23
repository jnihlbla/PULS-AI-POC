000100 01  W1O10801.                                                            
000200*                                 COPYTEXT FÖR MOD W1O10801.              
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 RADER-SKIP           PIC 9(3).                                    
001000*                                 ANTAL RADER SOM LÄSES FÖRBI.            
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 STRECK               PIC X.                                       
001400     03 AREA.                                                             
001500*                                 AREA NOLLSTÄLLS MELLAN VARVEN.          
001600        05 REKSIFFR          PIC X.                                       
001700*                                 KONTROLLSIFFRA                          
001800        05 IDLEVNR           OCCURS 14 TIMES                              
001900                             PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 KDFTAG            PIC Z.                                       
002200*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
002300*                                 IDFTG                                   
002400        05 FLTLVM            OCCURS 14 TIMES                              
002500                             PIC X.                                       
002600*                                 TILLVERKARMÄRKNING                      
002700        05 IDBENR            OCCURS 14 TIMES                              
002800                             PIC Z.                                       
002900*                                 BENÄMNINGSNUMMER                        
003000        05 BELEV             OCCURS 14 TIMES                              
003100                             PIC X(30).                                   
003200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003300        05 TIV-UPPDAT        PIC Z(4)9.                                   
003400*                                 DATUM FÖR SENASTE UPPDATERING           
003500*                                 (ÅÅVVD)                                 
003600        05 MESSAGE-RAD23     PIC X(79).                                   
003700*                                 MEDDELANDEFÄLT PÅ RAD 23                
003800*** END OF VILMAII-COPY LENGTH= 670 BYTES                                 
