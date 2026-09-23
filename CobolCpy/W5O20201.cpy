000100 01  MOD-W5O20201.                                                        
000200*                                 MOD-COPYTEXT FÖR W5020200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-RADER            OCCURS 12 TIMES.                             
001200        05 MOD-KDBEH-ATTR    PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 MOD-KDBEH         PIC X.                                       
001500*                                 BEHANDLINGSKOD                          
001600        05 MOD-IDARTNR       PIC Z(8)9.                                   
001700*                                 ARTIKELNUMMER                           
001800        05 MOD-BEART         PIC X(25).                                   
001900*                                 ARTIKELBENÄMNING                        
002000        05 MOD-KDPRODSL      PIC 9(2).                                    
002100*                                 PRODUKTSLAG                             
002200        05 MOD-KDPSLLOC-OLD  PIC 9(2).                                    
002300*                                 PRODUKTSLAG LOKALT                      
002400        05 MOD-KDPSLLOC-NEW-ATTR                                          
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-KDPSLLOC-NEW  PIC 9(2).                                    
002800*                                 PRODUKTSLAG LOKALT                      
002900     03 MOD-UPD.                                                          
003000        05 MOD-IDARTNR-UPD-ATTR                                           
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDARTNR-UPD   PIC X(9).                                    
003400*                                 ARTIKELNUMMER                           
003500        05 MOD-BEART-UPD     PIC X(25).                                   
003600*                                 ARTIKELBENÄMNING                        
003700        05 MOD-KDPRODSL-UPD  PIC 9(2).                                    
003800*                                 PRODUKTSLAG                             
003900        05 MOD-KDPSLLOC-OLD-UPD                                           
004000                             PIC 9(2).                                    
004100*                                 PRODUKTSLAG LOKALT                      
004200        05 MOD-KDPSLLOC-UPD-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDPSLLOC-UPD  PIC 9(2).                                    
004600*                                 PRODUKTSLAG LOKALT                      
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 701 BYTES                                 
