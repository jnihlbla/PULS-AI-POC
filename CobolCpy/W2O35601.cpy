000100 01  MOD-W2O35601.                                                        
000200*                                 MOD-COPYTEXT FÖR W2035600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BEART-ENG        PIC X(25).                                   
001600*                                 ENGELSK ARTIKELBENÄMNING                
001700     03 MOD-KVOI-PER         OCCURS 6 TIMES.                              
001800        05 MOD-AARTAL        PIC X(4).                                    
001900        05 MOD-KVOI          OCCURS 12 TIMES                              
002000                             PIC Z(6)9.                                   
002100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002200        05 MOD-KVOI-TOT      PIC Z(6)9.                                   
002300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002400     03 MOD-TEMFSINF         PIC X(55).                                   
002500*                                 INFORMATIONSMEDDELANDE                  
002600*** END OF VILMAII-COPY LENGTH= 716 BYTES                                 
