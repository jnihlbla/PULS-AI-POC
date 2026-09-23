000100 01  MID-W4I26801.                                                        
000200*                                 MID-COPYTEXT FÖR W4026800               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR7-IN      PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR7-UT      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-INPUT.                                                        
002000*                                 UPPDATERINGSFÄLT                        
002100        05 MID-TEBETVIL      OCCURS 4 TIMES                               
002200                             PIC X(72).                                   
002300*                                 BETALNINGSVILLKOR                       
002400        05 MID-TEGILTIG      OCCURS 4 TIMES                               
002500                             PIC X(72).                                   
002600*                                 GILTIGHETSVILLKOR                       
002700        05 MID-TELEVVIL      OCCURS 3 TIMES                               
002800                             PIC X(72).                                   
002900*                                 LEVERANSVILLKOR                         
003000        05 MID-TEPACK        PIC X(72).                                   
003100*                                 PACKNINGSVILLKOR                        
003200        05 MID-TEFRITT       OCCURS 4 TIMES                               
003300                             PIC X(72).                                   
003400*                                 ÖVRIGA VILLKOR                          
003500*** END COPY W4I26801C0  LENGTH=1204                                      
