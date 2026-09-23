000100 01  MOD-W2O35501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2035500               
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
001500     03 MOD-DASPSEA-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-DASPSEA          PIC X(6).                                    
001800     03 MOD-TIMANSEA         PIC X(6).                                    
001900*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
002000     03 MOD-SEASON-ARTIKEL   PIC X.                                       
002100     03 MOD-BEART-ENG        PIC X(25).                                   
002200*                                 ENGELSK ARTIKELBENÄMNING                
002300     03 MOD-OSAKERHET        PIC X(5).                                    
002400     03 MOD-ANT-HIST-AR      PIC X.                                       
002500     03 MOD-ANT-HIST-MAN     PIC X(2).                                    
002600     03 MOD-VALID.                                                        
002700        05 MOD-VALID-2       OCCURS 12 TIMES.                             
002800           07 MOD-VALIX      PIC Z(2)9.                                   
002900           07 MOD-VALANT     PIC Z(6)9.                                   
003000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003100        05 MOD-TOT-VALANT    PIC Z(6)9.                                   
003200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003300     03 MOD-SIMULERING.                                                   
003400        05 MOD-SIMULERING-2  OCCURS 12 TIMES.                             
003500           07 MOD-SIMIX-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700           07 MOD-SIMIX      PIC Z(2)9.                                   
003800           07 MOD-SIMANT-ATTR                                             
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100           07 MOD-SIMANT     PIC Z(6)9.                                   
004200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004300        05 MOD-TOT-SIMANT    PIC Z(6)9.                                   
004400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004500     03 MOD-HISTORIK.                                                     
004600        05 MOD-HISTORIK-2    OCCURS 12 TIMES.                             
004700           07 MOD-HISIX      PIC Z(2)9.                                   
004800           07 MOD-HISANT     PIC Z(6)9.                                   
004900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005000        05 MOD-TOT-HISANT    PIC Z(6)9.                                   
005100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 598 BYTES                                 
