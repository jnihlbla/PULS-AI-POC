000100 01  MID-W6I18201.                                                        
000200*                                 MID-COPYTEXT FÖR W6018200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-KEY-IN      PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-KEY-UT      PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-IDARTNR-KOPI     PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-EMB-Q.                                                        
001400*                                 RADER SOM VISAR EMBALLAGE ARTIK         
001500*                                 LAR                                     
001600        05 MID-IDARTNR-EMBQ0-IN                                           
001700                             PIC X(9).                                    
001800*                                 EMBALLAGEARTIKELNR FÖR Q0               
001900        05 MID-KVQPACK-EMBQ0-IN                                           
002000                             PIC X(5).                                    
002100*                                 ANTAL I Q0 FÖRPACKNING                  
002200        05 MID-KDEMBKOD-EMBQ0-IN                                          
002300                             PIC X(3).                                    
002400*                                 EMBALLAGEKOD 0                          
002500        05 MID-IDARTNR-EMBQ1-IN                                           
002600                             PIC X(9).                                    
002700*                                 EMBALLAGEARTIKELNR FÖR Q1               
002800        05 MID-KVQPACK-EMBQ1-IN                                           
002900                             PIC X(5).                                    
003000*                                 ANTAL I Q1 FÖRPACKNING                  
003100        05 MID-KDEMBKOD-EMBQ1-IN                                          
003200                             PIC X(3).                                    
003300*                                 EMBALLAGEKOD 1                          
003400        05 MID-IDARTNR-EMBQ2-IN                                           
003500                             PIC X(9).                                    
003600*                                 EMBALLAGEARTIKELNR FÖR Q2               
003700        05 MID-KVQPACK-EMBQ2-IN                                           
003800                             PIC X(5).                                    
003900*                                 ANTAL I Q2 FÖRPACKNING                  
004000        05 MID-KDEMBKOD-EMBQ2-IN                                          
004100                             PIC X(3).                                    
004200*                                 EMBALLAGEKOD 2                          
004300        05 MID-KVQPACK-EMBQ3-IN                                           
004400                             PIC X(5).                                    
004500*                                 ANTAL I Q3 FÖRPACKNING                  
004600        05 MID-KVQPACK-EMBQ4-IN                                           
004700                             PIC X(5).                                    
004800*                                 ANTAL I Q4 FÖRPACKNING                  
004900     03 MID-EMB-X            OCCURS 10 TIMES.                             
005000*                                 RADER SOM VISAR EXTRA EMBALLAGE         
005100*                                  ARTIKLAR                               
005200        05 MID-IDARTNR-EMBX-IN                                            
005300                             PIC X(9).                                    
005400*                                 EMBALLAGE-ARTIKELNUMMER                 
005500        05 MID-KVQPACK-EMBX-IN                                            
005600                             PIC X(5).                                    
005700*                                 ANTAL I FÖRPACKNING                     
005800*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
005900*** END OF VILMAII-COPY LENGTH= 232 BYTES                                 
