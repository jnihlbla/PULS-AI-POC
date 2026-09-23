000100 01  MOD-W2O40701.                                                        
000200*                                 MOD-COPYTEXT FÖR W2040700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-GROUP.                                                
001000        05 MOD-IDARTNR-UT    PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MOD-HYPHEN-UT     PIC X.                                       
001300        05 MOD-REKSIFFR-UT   PIC X.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-TYPE-IN          PIC X.                                       
002000     03 MOD-TYPE-UT          PIC X(11).                                   
002100     03 MOD-BEART-ENG        PIC X(25).                                   
002200*                                 ENGELSK ARTIKELBENÄMNING                
002300     03 MOD-KVOI-PER         OCCURS 6 TIMES.                              
002400        05 MOD-AARTAL        PIC X(4).                                    
002500        05 MOD-KVOI          OCCURS 12 TIMES                              
002600                             PIC Z(6)9.                                   
002700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002800        05 MOD-KVOI-TOT      PIC Z(6)9.                                   
002900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003000     03 MOD-TEMFSINF         PIC X(55).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END OF VILMAII-COPY LENGTH= 730 BYTES                                 
