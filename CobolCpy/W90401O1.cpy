000100 01  MOD-W90401O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9040100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-FILLER           PIC X(5).                                    
001000     03 MOD-FILLER           PIC X(30).                                   
001100     03 MOD-FILLER           PIC X(9).                                    
001200     03 MOD-FILLER           PIC X(5).                                    
001300     03 MOD-FILLER           PIC X(30).                                   
001400     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 MOD-IDLEVART-NEXT    PIC X(30).                                   
001700*                                 LEVERANTÖRENS ARTNR                     
001800     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-IDBENR-NEXT      PIC 9.                                       
002100*                                 BENÄMNINGSNUMMER                        
002200     03 MOD-FILLER           PIC 9.                                       
002300     03 MOD-FILLER           PIC Z(4)9.                                   
002400     03 MOD-RADER            OCCURS 9 TIMES.                              
002500        05 MOD-IDARTNR       PIC Z(9).                                    
002600*                                 ARTIKELNUMMER                           
002700        05 MOD-KDFTAG        PIC 9.                                       
002800*                                 FÖRETAGSKOD HAR UTGÅTTBYT TILL          
002900*                                 IDFTG                                   
003000        05 MOD-IDLEVNR       PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200        05 MOD-IDBENR        PIC 9.                                       
003300*                                 BENÄMNINGSNUMMER                        
003400        05 MOD-BELEVART      PIC X(30).                                   
003500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003600        05 MOD-FLTLVM        PIC X.                                       
003700*                                 TILLVERKARMÄRKNING                      
003800     03 MOD-IDARTNR-UP-ATTR  PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FILLER           PIC X(9).                                    
004100     03 MOD-KDFTAG-UP-ATTR   PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FILLER           PIC 9.                                       
004400     03 MOD-IDLEVNR-UP-ATTR  PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-FILLER           PIC X(5).                                    
004700     03 MOD-IDBENR-UP-ATTR   PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-FILLER           PIC 9.                                       
005000     03 MOD-BELEVART-UP-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-FILLER           PIC X(30).                                   
005300     03 MOD-FLTLVM-UP-ATTR   PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-FILLER           PIC X.                                       
005600     03 MOD-KDCMD-UP-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-FILLER           PIC X.                                       
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 723 BYTES                                 
