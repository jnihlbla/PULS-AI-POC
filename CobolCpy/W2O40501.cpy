000100 01  MOD-W2O40501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2040500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-TISPSEA-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-TISPSEA          PIC X(6).                                    
001800*                                 SÄSONG SPÄRRAD TOM    ÅÅMMDD            
001900     03 MOD-TIMANSEA         PIC X(6).                                    
002000*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
002100     03 MOD-FLSEASON         PIC X.                                       
002200*                                 SÄSONG PÅ ARTIKEL                       
002300     03 MOD-BEART-ENG        PIC X(25).                                   
002400*                                 ENGELSK ARTIKELBENÄMNING                
002500     03 MOD-REOSAEK          PIC Z(2)9.9.                                 
002600*                                 SEASONAL UNCERTAINTY FACTOR             
002700     03 MOD-ANT-HIST-AR      PIC 9.                                       
002800     03 MOD-ANT-HIST-MAN     PIC 9(2).                                    
002900     03 MOD-VALID.                                                        
003000        05 MOD-VALID-2       OCCURS 12 TIMES.                             
003100           07 MOD-RESEASON-VALIX                                          
003200                             PIC Z(2)9.                                   
003300*                                 SÄSONGSINDEX                            
003400           07 MOD-KVOI-VALANT                                             
003500                             PIC Z(6)9.                                   
003600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003700        05 MOD-KVOI-TOT-VALANT                                            
003800                             PIC Z(6)9.                                   
003900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
004000     03 MOD-SIMULERING.                                                   
004100        05 MOD-SIMULERING-2  OCCURS 12 TIMES.                             
004200           07 MOD-RESEASON-SIMIX-ATTR                                     
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500           07 MOD-RESEASON-SIMIX                                          
004600                             PIC Z(2)9.                                   
004700*                                 SÄSONGSINDEX                            
004800           07 MOD-KVOI-SIMANT-ATTR                                        
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100           07 MOD-KVOI-SIMANT                                             
005200                             PIC Z(6)9.                                   
005300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005400        05 MOD-KVOI-TOT-SIMANT                                            
005500                             PIC Z(6)9.                                   
005600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005700     03 MOD-HISTORIK.                                                     
005800        05 MOD-HISTORIK-2    OCCURS 12 TIMES.                             
005900           07 MOD-RESEASON-HISIX                                          
006000                             PIC Z(2)9.                                   
006100*                                 SÄSONGSINDEX                            
006200           07 MOD-KVOI-HISANT                                             
006300                             PIC Z(6)9.                                   
006400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
006500        05 MOD-KVOI-TOT-HISANT                                            
006600                             PIC Z(6)9.                                   
006700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
006800     03 MOD-TEMFSINF         PIC X(55).                                   
006900*                                 INFORMATIONSMEDDELANDE                  
007000*** END OF VILMAII-COPY LENGTH= 591 BYTES                                 
