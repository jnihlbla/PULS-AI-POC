000100 01  MID-W90401I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9401                  
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
002300     03 MID-FILLER           PIC 9.                                       
002400     03 MID-FILLER           PIC 9(5).                                    
002500     03 MID-INPUT.                                                        
002600*                                 CROSS-INDEX                             
002700        05 MID-IDARTNR-UP    PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MID-KDFTAG-UP     PIC X.                                       
003000*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
003100*                                 IDFTG                                   
003200        05 MID-IDLEVNR-UP    PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400        05 MID-IDBENR-UP     PIC X.                                       
003500*                                 BENÄMNINGSNUMMER                        
003600        05 MID-BELEVART-UP   PIC X(30).                                   
003700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003800        05 MID-FLTLVM-UP     PIC X.                                       
003900*                                 TILLVERKARMÄRKNING                      
004000        05 MID-KDCMD-UP      PIC X.                                       
004100         88 MID-KDCMD-INGENTING                                           
004200                             VALUE ' '.                                   
004300         88 MID-KDCMD-DELETE VALUE 'D'                                    
004400                             'B'.                                         
004500         88 MID-KDCMD-REPLACE                                             
004600                             VALUE 'R'                                    
004700                             'Ä'.                                         
004800         88 MID-KDCMD-INSERT VALUE 'I'                                    
004900                             'N'                                          
005000                             'A'.                                         
005100         88 MID-KDCMD-SELECT VALUE 'S'                                    
005200                             'V'.                                         
005300         88 MID-KDCMD-PRINT  VALUE 'P'                                    
005400                             'P'.                                         
005500         88 MID-KDCMD-COPY   VALUE 'C'                                    
005600                             'K'.                                         
005700*                                 RAD-UPPDATERINGSKOMMANDO                
005800*                                  BLANK  = INGENTING                     
005900*                                  D , B  = DELETE                        
006000*                                  R , Ä  = REPLACE                       
006100*                                  I,N,A  = INSERT                        
006200*                                  S , V  = SELECT                        
006300*                                  P , P  = PRINT                         
006400*                                  C , K  = COPY                          
006500*** END OF VILMAII-COPY LENGTH= 187 BYTES                                 
