000100 01  MOD-W2O10501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2010500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-DASPSEA-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-DASPSEA          PIC X(6).                                    
001400     03 MOD-RENSA-IX-ATTR    PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-RENSA-IX         PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-DAMANSEA         PIC X(6).                                    
001900     03 MOD-SEASON-ARTIKEL   PIC X.                                       
002000     03 MOD-BEART-ENG        PIC X(25).                                   
002100*                                 ENGELSK ARTIKELBENÄMNING                
002200     03 MOD-OSAKERHET        PIC X(5).                                    
002300     03 MOD-ANT-HIST-AR      PIC X.                                       
002400     03 MOD-VALID.                                                        
002500        05 MOD-VALID-2       OCCURS 12 TIMES.                             
002600           07 MOD-VALIX      PIC Z(2)9.                                   
002700           07 MOD-VALANT     PIC Z(6)9.                                   
002800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002900        05 MOD-TOT-VALANT    PIC Z(6)9.                                   
003000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003100     03 MOD-SIMULERING.                                                   
003200        05 MOD-SIMULERING-2  OCCURS 12 TIMES.                             
003300           07 MOD-SIMIX-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500           07 MOD-SIMIX      PIC Z(2)9.                                   
003600           07 MOD-SIMANT-ATTR                                             
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900           07 MOD-SIMANT     PIC Z(6)9.                                   
004000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004100        05 MOD-TOT-SIMANT    PIC Z(6)9.                                   
004200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004300     03 MOD-HISTORIK.                                                     
004400        05 MOD-HISTORIK-2    OCCURS 12 TIMES.                             
004500           07 MOD-HISIX      PIC Z(2)9.                                   
004600           07 MOD-HISANT     PIC Z(6)9.                                   
004700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004800        05 MOD-TOT-HISANT    PIC Z(6)9.                                   
004900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005000     03 MOD-TEMFSINF         PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*** END OF VILMAII-COPY LENGTH= 596 BYTES                                 
