000100 01  MOD-W2O10701.                                                        
000200*                                 MOD-COPYTEXT FÖR W2010700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-VAL-IN           PIC X.                                       
001200     03 MOD-VAL-UT           PIC X(7).                                    
001300     03 MOD-BEART-ENG        PIC X(25).                                   
001400*                                 ENGELSK ARTIKELBENÄMNING                
001500     03 MOD-KVOI-PER         OCCURS 6 TIMES.                              
001600        05 MOD-AARTAL        PIC X(4).                                    
001700        05 MOD-KVOI          OCCURS 12 TIMES                              
001800                             PIC Z(6)9.                                   
001900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002000        05 MOD-KVOI-TOT      PIC Z(6)9.                                   
002100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002200     03 MOD-TEMFSINF         PIC X(55).                                   
002300*                                 INFORMATIONSMEDDELANDE                  
002400*** END OF VILMAII-COPY LENGTH= 720 BYTES                                 
