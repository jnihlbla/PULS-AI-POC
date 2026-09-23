000100 01  MOD-W5O30901.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5309              
000300*                                 INVENTERINGSKÖ 2                        
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-ADLAGOMR-IN      PIC X(2).                                    
001300*                                 LAGEROMRÅDE                             
001400     03 MOD-ADLAGOMR-UT      PIC X(2).                                    
001500*                                 LAGEROMRÅDE                             
001600     03 MOD-ADGANG-IN        PIC X(2).                                    
001700*                                 GÅNG                                    
001800     03 MOD-ADGANG-UT        PIC X(2).                                    
001900*                                 GÅNG                                    
002000     03 MOD-ADPLATS-IN       PIC X(5).                                    
002100*                                 LAGERPLATSNUMMER                        
002200     03 MOD-ADPLATS-UT       PIC X(5).                                    
002300*                                 LAGERPLATSNUMMER                        
002400     03 MOD-IDPRTOMG-IN      PIC 9.                                       
002500*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
002600     03 MOD-IDPRTOMG-UT      PIC 9.                                       
002700*                                 PRINT OMGÅNG FÖR AUT.JUSTERING          
002800     03 MOD-FLINVSKR-IN      PIC X.                                       
002900*                                 INVENTERINGSANMODAN UTSKRIVEN           
003000     03 MOD-FLINVSKR-UT      PIC X.                                       
003100*                                 INVENTERINGSANMODAN UTSKRIVEN           
003200     03 MOD-KVINVSKR-ATTR    PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KVINVSKR         PIC Z9.                                      
003500*                                 BEGÄRDA INVENTERINGSUNDERLAG            
003600     03 MOD-KVANT-ART        PIC Z(3).                                    
003700     03 MOD-RAD-INFO         OCCURS 10 TIMES.                             
003800*                                 RAD-INFO                                
003900        05 MOD-ADLAGOMR-UTSKR                                             
004000                             PIC Z9.                                      
004100*                                 LAGEROMRÅDE                             
004200        05 MOD-ADGANG-UTSKR  PIC Z9.                                      
004300*                                 GÅNG                                    
004400        05 MOD-ADPLATS-UTSKR PIC Z(4)9.                                   
004500*                                 LAGERPLATSNUMMER                        
004600        05 MOD-KDINVPRIO-UTSKR                                            
004700                             PIC Z.                                       
004800*                                 INVENTERING PRIORITET                   
004900        05 MOD-KDVVKL-UTSKR  PIC 9.                                       
005000*                                 VOLYMVÄRDESKLASS                        
005100        05 MOD-IDARTNR-UTSKR PIC Z(9).                                    
005200*                                 ARTIKELNUMMER                           
005300        05 MOD-KDINVKAT-UTSKR                                             
005400                             PIC Z9.                                      
005500*                                 INVENTERINGSKATEGORI                    
005600        05 MOD-FLINVSKR-UTSKR                                             
005700                             PIC X.                                       
005800*                                 INVENTERINGSANMODAN UTSKRIVEN           
005900        05 MOD-KDPRODSL-UTSKR                                             
006000                             PIC Z9.                                      
006100*                                 PRODUKTSLAG                             
006200        05 MOD-IDFKNGRP-UTSKR                                             
006300                             PIC Z(3)9.                                   
006400*                                 FUNKTIONSGRUPP                          
006500        05 MOD-TIREGDAT-UTSKR                                             
006600                             PIC 9(6).                                    
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800        05 MOD-LISTNR        PIC X(6).                                    
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 542 BYTES                                 
