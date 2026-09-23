000100 01  MOD-W4O41601.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDDC-DAY-IN      PIC X(2).                                    
001200*                                 IDENTIFIERARE DAGORDERLAGER             
001300     03 MOD-IDDC-DAY-UT      PIC X(2).                                    
001400*                                 IDENTIFIERARE DAGORDERLAGER             
001500     03 MOD-IDDC-BULK-IN     PIC X(2).                                    
001600*                                 IDENTIFIERARE BULKORDERLAGER            
001700     03 MOD-IDDC-BULK-UT     PIC X(2).                                    
001800*                                 IDENTIFIERARE BULKORDERLAGER            
001900     03 MOD-FLLDCKND-IN      PIC X.                                       
002000*                                 FL LDC-KUND                             
002100     03 MOD-FLLDCKND-UT      PIC X.                                       
002200*                                 FL LDC-KUND                             
002300     03 MOD-RAD-INFO         OCCURS 13 TIMES.                             
002400*                                 RADINFORMATION                          
002500        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700        05 MOD-IDDC-BULK     PIC X(2).                                    
002800*                                 IDENTIFIERARE BULKORDERLAGER            
002900        05 MOD-IDDC-DAY      PIC X(2).                                    
003000*                                 IDENTIFIERARE DAGORDERLAGER             
003100        05 MOD-IDDC-VOR      PIC X(2).                                    
003200*                                 IDENTIFIERARE VORORDERLAGER             
003300        05 MOD-FLLDCKND      PIC X.                                       
003400*                                 FL LDC-KUND                             
003500     03 MOD-TEMFSINF         PIC X(55).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*** END OF VILMAII-COPY LENGTH= 286 BYTES                                 
