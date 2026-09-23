000100 01  MID-W0I10701.                                                        
000200*                                 MID-COPYTEXT FÖR W00107                 
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-BELEVART-IN      PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300     03 MID-BELEVART-UT      PIC X(30).                                   
001400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001500     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 MID-IDLEVART-NEXT    PIC X(30).                                   
001800*                                 LEVERANTÖRENS ARTNR                     
001900     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-IDBENR-NEXT      PIC 9.                                       
002200*                                 BENÄMNINGSNUMMER                        
002300     03 MID-KDFTAG-RAD5      PIC 9.                                       
002400*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
002500*                                 IDFTG                                   
002600     03 MID-TIV-UPPDAT-RAD5  PIC 9(5).                                    
002700*                                 DATUM FÖR SENASTE UPPDATERING           
002800*                                 (ÅÅVVD)                                 
002900     03 MID-INPUT.                                                        
003000*                                 CROSS-INDEX                             
003100        05 MID-IDARTNR-UP    PIC X(9).                                    
003200*                                 ARTIKELNUMMER                           
003300        05 MID-KDFTAG-UP     PIC X.                                       
003400*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
003500*                                 IDFTG                                   
003600        05 MID-IDLEVNR-UP    PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800        05 MID-IDBENR-UP     PIC X.                                       
003900*                                 BENÄMNINGSNUMMER                        
004000        05 MID-BELEVART-UP   PIC X(30).                                   
004100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004200        05 MID-FLTLVM-UP     PIC X.                                       
004300*                                 TILLVERKARMÄRKNING                      
004400        05 MID-KDCMD-UP      PIC X.                                       
004500         88 MID-KDCMD-INGENTING                                           
004600                             VALUE ' '.                                   
004700         88 MID-KDCMD-DELETE VALUE 'D'                                    
004800                             'B'.                                         
004900         88 MID-KDCMD-REPLACE                                             
005000                             VALUE 'R'                                    
005100                             'Ä'.                                         
005200         88 MID-KDCMD-INSERT VALUE 'I'                                    
005300                             'N'                                          
005400                             'A'.                                         
005500         88 MID-KDCMD-SELECT VALUE 'S'                                    
005600                             'V'.                                         
005700         88 MID-KDCMD-PRINT  VALUE 'P'                                    
005800                             'P'.                                         
005900         88 MID-KDCMD-COPY   VALUE 'C'                                    
006000                             'K'.                                         
006100*                                 RAD-UPPDATERINGSKOMMANDO                
006200*                                  BLANK  = INGENTING                     
006300*                                  D , B  = DELETE                        
006400*                                  R , Ä  = REPLACE                       
006500*                                  I,N,A  = INSERT                        
006600*                                  S , V  = SELECT                        
006700*                                  P , P  = PRINT                         
006800*                                  C , K  = COPY                          
006900*** END OF VILMAII-COPY LENGTH= 187 BYTES                                 
