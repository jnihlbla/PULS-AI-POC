000100 01  MOD-W90432O1.                                                        
000200*                                 MOD FÖR PROGRAM W90432                  
000300*                                 PROGRAMMET VISAR OCH UPPDATERAR         
000400*                                 FP-EMBALLAGE                            
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-FILLER           PIC X(9).                                    
001000     03 MOD-FILLER           PIC X(9).                                    
001100     03 MOD-FILLER           PIC X(2).                                    
001200     03 MOD-FILLER           PIC X(9).                                    
001300     03 MOD-FILLER           PIC X(2).                                    
001400     03 MOD-FILLER           PIC 9(4).                                    
001500     03 MOD-FILLER           PIC X.                                       
001600     03 MOD-FILLER           PIC X(6).                                    
001700     03 MOD-FILLER           PIC X(8).                                    
001800     03 MOD-EMB-Q.                                                        
001900*                                 RADER SOM VISAR EMBALLAGE ARTIK         
002000*                                 LAR                                     
002100        05 MOD-FILLER        PIC X(9).                                    
002200        05 MOD-FILLER        PIC X(2).                                    
002300        05 MOD-FILLER        PIC X(9).                                    
002400        05 MOD-FILLER        PIC X(5).                                    
002500        05 MOD-FILLER        PIC X(2).                                    
002600        05 MOD-FILLER        PIC X(5).                                    
002700        05 MOD-FILLER        PIC X(3).                                    
002800        05 MOD-FILLER        PIC X(2).                                    
002900        05 MOD-FILLER        PIC X(3).                                    
003000        05 MOD-FILLER        PIC X(9).                                    
003100        05 MOD-FILLER        PIC X(2).                                    
003200        05 MOD-FILLER        PIC X(9).                                    
003300        05 MOD-FILLER        PIC X(5).                                    
003400        05 MOD-FILLER        PIC X(2).                                    
003500        05 MOD-FILLER        PIC X(5).                                    
003600        05 MOD-FILLER        PIC X(3).                                    
003700        05 MOD-FILLER        PIC X(2).                                    
003800        05 MOD-FILLER        PIC X(3).                                    
003900        05 MOD-FILLER        PIC X(9).                                    
004000        05 MOD-FILLER        PIC X(2).                                    
004100        05 MOD-FILLER        PIC X(9).                                    
004200        05 MOD-FILLER        PIC X(5).                                    
004300        05 MOD-FILLER        PIC X(2).                                    
004400        05 MOD-FILLER        PIC X(5).                                    
004500        05 MOD-KDEMBKOD-EMBQ2-UT                                          
004600                             PIC X(3).                                    
004700        05 MOD-KDEMBKOD-EMBQ2-IN-ATTR                                     
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-FILLER        PIC X(3).                                    
005100        05 MOD-FILLER        PIC X(5).                                    
005200        05 MOD-FILLER        PIC X(2).                                    
005300        05 MOD-FILLER        PIC X(5).                                    
005400     03 MOD-EMB-X            OCCURS 10 TIMES.                             
005500*                                 RADER SOM VISAR EXTRA EMBALLAGE         
005600*                                  ARTIKLAR                               
005700        05 MOD-FILLER        PIC X(9).                                    
005800        05 MOD-FILLER        PIC X(2).                                    
005900        05 MOD-FILLER        PIC X(9).                                    
006000        05 MOD-FILLER        PIC X(5).                                    
006100        05 MOD-FILLER        PIC X(2).                                    
006200        05 MOD-FILLER        PIC X(5).                                    
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 601 BYTES                                 
