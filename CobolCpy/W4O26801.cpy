000100 01  MOD-W4O26801.                                                        
000200*                                 MOD-COPYTEXT FÖR W4026800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR-UT       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-GRUPP1           OCCURS 4 TIMES.                              
002400*                                 BETALN-TEXT GRUPP                       
002500        05 MOD-TEBETVIL-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-TEBETVIL      PIC X(72).                                   
002800*                                 BETALNINGSVILLKOR                       
002900     03 MOD-GRUPP2           OCCURS 4 TIMES.                              
003000*                                 GILTIGH-TEXT GRUPP                      
003100        05 MOD-TEGILTIG-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-TEGILTIG      PIC X(72).                                   
003400*                                 GILTIGHETSVILLKOR                       
003500     03 MOD-GRUPP3           OCCURS 3 TIMES.                              
003600*                                 LEV-TEXT GRUPP                          
003700        05 MOD-TELEVVIL-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-TELEVVIL      PIC X(72).                                   
004000*                                 LEVERANSVILLKOR                         
004100     03 MOD-TEPACK-ATTR      PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-TEPACK           PIC X(72).                                   
004400*                                 PACKNINGSVILLKOR                        
004500     03 MOD-GRUPP4           OCCURS 4 TIMES.                              
004600*                                 ÖVRIGT-TEXT GRUPP                       
004700        05 MOD-TEFRITT-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-TEFRITT       PIC X(72).                                   
005000*                                 ÖVRIGA VILLKOR                          
005100     03 MOD-TEMFSINF         PIC X(61).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END COPY W4O26801C0  LENGTH=1341                                      
