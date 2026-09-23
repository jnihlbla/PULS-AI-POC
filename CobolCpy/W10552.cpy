000100 01  SOEK-W10552-CTX.                                                     
000200*                                 JÄMFÖRELSEAREA                          
000300*                                 FÖR PROGRAM W10552                      
000400     03 SOEK-IDCATRAD        PIC 9(4).                                    
000500*                                 RADNUMMER                               
000600     03 SOEK-KDCATPUB-FOM    PIC X(6).                                    
000700*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
000800     03 SOEK-KDCATPUB-TOM    PIC X(6).                                    
000900*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
001000     03 SOEK-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 SOEK-KDPS            PIC X(2).                                    
001300*                                 ARTIKELSTATUS                           
001400     03 SOEK-KVPUNKT         PIC S9              COMP-3.                  
001500*                                 ANTAL INDRAGNINGSPUNKTER                
001600     03 SOEK-IDTTEXNR        PIC S9(5)           COMP-3.                  
001700*                                 TILLÄGGSTEXT-NR                         
001800     03 SOEK-TEKATANM        PIC X(23).                                   
001900*                                 ANMÄRKNINGSTEXT                         
002000     03 SOEK-KDHOM           PIC S9              COMP-3.                  
002100*                                 HOMONYMKOD                              
002200     03 SOEK-BEART           PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 SOEK-IDRUBNR         OCCURS 3 TIMES                               
002500                             PIC S9(5)           COMP-3.                  
002600*                                 RUBRIKNUMMER                            
002700     03 SOEK-IDFOTNR         OCCURS 3 TIMES                               
002800                             PIC S9(5)           COMP-3.                  
002900*                                 FOTNOTSNUMMER                           
003000*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
