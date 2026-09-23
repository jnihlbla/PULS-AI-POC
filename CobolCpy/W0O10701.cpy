000100 01  MOD-W0O10701.                                                        
000200*                                 MOD-COPYTEXT FÖR W0010700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-BELEVART-IN      PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MOD-BELEVART-UT      PIC X(30).                                   
001800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001900     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 MOD-IDLEVART-NEXT    PIC X(30).                                   
002200*                                 LEVERANTÖRENS ARTNR                     
002300     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MOD-IDBENR-NEXT      PIC 9.                                       
002600*                                 BENÄMNINGSNUMMER                        
002700     03 MOD-KDFTAG-RAD5      PIC 9.                                       
002800*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
002900*                                 IDFTG                                   
003000     03 MOD-TIV-UPPDAT-RAD5  PIC 9(5).                                    
003100*                                 DATUM FÖR SENASTE UPPDATERING           
003200*                                 (ÅÅVVD)                                 
003300     03 MOD-RADER            OCCURS 9 TIMES.                              
003400        05 MOD-IDARTNR       PIC Z(9).                                    
003500*                                 ARTIKELNUMMER                           
003600        05 MOD-KDFTAG        PIC 9.                                       
003700*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
003800*                                 IDFTG                                   
003900        05 MOD-IDLEVNR       PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100        05 MOD-IDBENR        PIC 9.                                       
004200*                                 BENÄMNINGSNUMMER                        
004300        05 MOD-BELEVART      PIC X(30).                                   
004400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004500        05 MOD-FLTLVM        PIC X.                                       
004600*                                 TILLVERKARMÄRKNING                      
004700     03 MOD-IDARTNR-UP-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDARTNR-UP       PIC X(9).                                    
005000*                                 ARTIKELNUMMER                           
005100     03 MOD-KDFTAG-UP-ATTR   PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDFTAG-UP        PIC 9.                                       
005400*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
005500*                                 IDFTG                                   
005600     03 MOD-IDLEVNR-UP-ATTR  PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-IDLEVNR-UP       PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000     03 MOD-IDBENR-UP-ATTR   PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-IDBENR-UP        PIC 9.                                       
006300*                                 BENÄMNINGSNUMMER                        
006400     03 MOD-BELEVART-UP-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-BELEVART-UP      PIC X(30).                                   
006700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006800     03 MOD-FLTLVM-UP-ATTR   PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-FLTLVM-UP        PIC X.                                       
007100*                                 TILLVERKARMÄRKNING                      
007200     03 MOD-KDCMD-UP-ATTR    PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-KDCMD-UP         PIC X.                                       
007500*                                 RAD-UPPDATERINGSKOMMANDO                
007600*                                  BLANK  = INGENTING                     
007700*                                  D , B  = DELETE                        
007800*                                  R , Ä  = REPLACE                       
007900*                                  I,N,A  = INSERT                        
008000*                                  S , V  = SELECT                        
008100*                                  P , P  = PRINT                         
008200*                                  C , K  = COPY                          
008300     03 MOD-TEMFSINF         PIC X(55).                                   
008400*                                 INFORMATIONSMEDDELANDE                  
008500*** END OF VILMAII-COPY LENGTH= 723 BYTES                                 
