000100 01  MOD-W1O52201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1522.             
000300*                                 BESTÄLLNING AV KATALOG-/                
000400*                                 GRUPP-LÅN.                              
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000800*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000900     03 MOD-IDCATNR-LAES-ATTR                                             
001000                             PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200     03 MOD-IDCATNR-LAES     PIC X(5).                                    
001300*                                 KATALOG-ID                              
001400     03 MOD-IDCATNR-SKRIV-ATTR                                            
001500                             PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDCATNR-SKRIV    PIC X(5).                                    
001800*                                 KATALOG-ID                              
001900     03 MOD-IDCATGRP-LAES-ATTR                                            
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-IDCATGRP-LAES    PIC X(2).                                    
002300*                                 KATALOG-GRUPP                           
002400     03 MOD-IDCATGRP-SKRIV-ATTR                                           
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-IDCATGRP-SKRIV   PIC X(2).                                    
002800*                                 KATALOG-GRUPP                           
002900     03 MOD-KDCATPUB-R-LAES-ATTR                                          
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KDCATPUB-R-LAES  PIC X(3).                                    
003300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003400     03 MOD-KDCATPUB-R-SKRIV-FOM-ATTR                                     
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDCATPUB-R-SKRIV-FOM                                          
003800                             PIC X(3).                                    
003900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004000     03 MOD-KDCATPUB-R-SKRIV-TOM-ATTR                                     
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KDCATPUB-R-SKRIV-TOM                                          
004400                             PIC X(3).                                    
004500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004600     03 MOD-MESSAGE-RAD10    PIC X(25).                                   
004700     03 MOD-FLTABORT-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-FLTABORT         PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 165 BYTES                                 
