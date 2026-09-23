000100 01  MOD-W90408O1.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-FILLER           PIC X(9).                                    
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-FILLER           PIC X(3).                                    
000900     03 MOD-FILLER           PIC X(3).                                    
001000     03 MOD-FILLER           PIC X(3).                                    
001100     03 MOD-DIERS-ERS-ATTR   PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-FILLER           PIC X(7).                                    
001400     03 MOD-KDERS-ATTR       PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-FILLER           PIC 9(2).                                    
001700     03 MOD-IDAO-ATTR        PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-FILLER           PIC X(10).                                   
002000     03 MOD-TIERSDAT-PREL-ATTR                                            
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-FILLER           PIC 9(5).                                    
002400     03 MOD-UTRAD            OCCURS 9 TIMES.                              
002500        05 MOD-IDKORTNR-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-FILLER        PIC X(3).                                    
002800        05 MOD-FILLER        PIC X.                                       
002900        05 MOD-IDARTNR-TILLK-ATTR                                         
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-FILLER        PIC X(9).                                    
003300        05 MOD-DIERS-TILLK-ATTR                                           
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-FILLER        PIC X(7).                                    
003700        05 MOD-FILLER        PIC X(25).                                   
003800        05 MOD-BEERS-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-FILLER        PIC X(20).                                   
004100     03 MOD-TEARTNOT-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FILLER           PIC X(40).                                   
004400     03 MOD-FILLER           PIC X(8).                                    
004500     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FILLER           PIC X.                                       
004800     03 MOD-FILLER           PIC 9(2).                                    
004900     03 MOD-FILLER           PIC 9(2).                                    
005000     03 MOD-FILLER           PIC X(5).                                    
005100     03 MOD-FILLER           PIC X(5).                                    
005200     03 MOD-FILLER           PIC X(5).                                    
005300     03 MOD-FILLER           PIC X(5).                                    
005400     03 MOD-TEMFSINF         PIC X(61).                                   
005500*** END OF VILMAII-COPY LENGTH= 898 BYTES                                 
