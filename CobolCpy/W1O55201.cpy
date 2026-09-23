000100 01  MOD-W1O55201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 LÅN, KOLUMN/RADER                       
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-FROM-ATTR                                             
000900                             PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDCATNR-FROM     PIC Z(4)9.                                   
001200*                                 KATALOG-ID                              
001300     03 MOD-IDCATNR-TO-ATTR  PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDCATNR-TO       PIC Z(4)9.                                   
001600*                                 KATALOG-ID                              
001700     03 MOD-IDCATGRP-FROM-ATTR                                            
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-IDCATGRP-FROM    PIC Z9.                                      
002100*                                 KATALOG-GRUPP                           
002200     03 MOD-IDCATGRP-TO-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-IDCATGRP-TO      PIC Z9.                                      
002500*                                 KATALOG-GRUPP                           
002600     03 MOD-IDCATAVS-FROM-ATTR                                            
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDCATAVS-FROM    PIC Z(3)9.                                   
003000*                                 KATALOG-AVSNITT                         
003100     03 MOD-IDCATAVS-TO-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-IDCATAVS-TO      PIC Z(3)9.                                   
003400*                                 KATALOG-AVSNITT                         
003500     03 MOD-IDKOL-FROM-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDKOL-FROM       PIC X.                                       
003800*                                 KOLUMN-ID (A-E)                         
003900     03 MOD-IDKOL-TO-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDKOL-TO         PIC X.                                       
004200*                                 KOLUMN-ID (A-E)                         
004300     03 MOD-IDCATRAD-FROM-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDCATRAD-FROM    PIC Z(3)9.                                   
004700*                                 RADNUMMER                               
004800     03 MOD-IDCATRAD-START-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-IDCATRAD-START   PIC Z(3)9.                                   
005200*                                 RADNUMMER                               
005300     03 MOD-IDCATRAD-TO-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-IDCATRAD-TO      PIC Z(3)9.                                   
005600*                                 RADNUMMER                               
005700     03 MOD-IDCATPOS-FROM-ATTR                                            
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDCATPOS-FROM    PIC X(3).                                    
006100*                                 POSITIONSNUMMER                         
006200     03 MOD-IDCATPOS-TO-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-IDCATPOS-TO      PIC X(3).                                    
006500*                                 POSITIONSNUMMER                         
006600     03 MOD-KDCATPUB-R-FROM-FOM-ATTR                                      
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-KDCATPUB-R-FROM-FOM                                           
007000                             PIC X(3).                                    
007100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
007200     03 MOD-KDCATPUB-R-TO-FOM-ATTR                                        
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-KDCATPUB-R-TO-FOM                                             
007600                             PIC X(3).                                    
007700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
007800     03 MOD-KDCATPUB-R-TO-TOM-ATTR                                        
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-KDCATPUB-R-TO-TOM                                             
008200                             PIC X(3).                                    
008300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
008400     03 MOD-IDCATRAD-SPAR    PIC Z(3)9.                                   
008500*                                 RADNUMMER                               
008600     03 MOD-KDCATPUB-SPAR    PIC X(6).                                    
008700*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
008800     03 MOD-TEMFSINF         PIC X(55).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
009000*** END OF VILMAII-COPY LENGTH= 192 BYTES                                 
