000100 01  MOD-W4O70501.                                                        
000200*                                 MOD-COPYTEXT FÖR W4070500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-RELANDCO-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-RELANDCO         PIC X(6).                                    
002200*                                 LANDING COST PROCENT                    
002300     03 MOD-PRFRAKT-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-PRFRAKT          PIC X(10).                                   
002600*                                 FRAKTKOSTNAD                            
002700     03 MOD-PRFOERS-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-PRFOERS          PIC X(10).                                   
003000*                                 FÖRSÄKRINGSPREMIE                       
003100     03 MOD-PRLEGKST-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-PRLEGKST         PIC X(10).                                   
003400*                                 LEGALISERINSKOSTNAD                     
003500     03 MOD-IDORDNR5-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDORDNR5         PIC X(5).                                    
003800*                                 ORDERNUMMER                             
003900     03 MOD-IDFAKT-ATTR      PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDFAKT           PIC X(7).                                    
004200*                                 FAKTURANUMMER                           
004300     03 MOD-IDPRODNR-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDPRODNR         PIC X(7).                                    
004600*                                 PRODUKTIONSNUMMER                       
004700     03 MOD-KOLLI-FROM-TO    OCCURS 13 TIMES.                             
004800*                                 KOLLI INTERVALL                         
004900        05 MOD-IDKOLLI-FOM-ATTR                                           
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-IDKOLLI-FOM   PIC X(5).                                    
005300*                                 KOLLINUMMER                             
005400        05 MOD-IDKOLLI-TOM-ATTR                                           
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDKOLLI-TOM   PIC X(5).                                    
005800*                                 KOLLINUMMER                             
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 384 BYTES                                 
