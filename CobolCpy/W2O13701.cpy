000100 01  MOD-W2O13701.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-FILLER           OCCURS 12 TIMES.                             
001400        05 MOD-TIPP          PIC 9(2).                                    
001500*                                 PLANERINGSPERIOD (PP)                   
001600*                                 12 PER ÅR                               
001700     03 MOD-FILLER.                                                       
001800        05 MOD-KVOI-CDC      OCCURS 12 TIMES                              
001900                             PIC Z(5)9.                                   
002000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002100        05 MOD-KVOI-DC-REF   OCCURS 12 TIMES                              
002200                             PIC Z(5)9.                                   
002300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002400        05 MOD-KVOI-DC-KUND  OCCURS 12 TIMES                              
002500                             PIC Z(5)9.                                   
002600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002700     03 MOD-FILLER.                                                       
002800        05 MOD-KVOI-TOT-12-CDC                                            
002900                             PIC Z(6)9.                                   
003000        05 MOD-KVOI-TOT-12-DC-REF                                         
003100                             PIC Z(6)9.                                   
003200        05 MOD-KVOI-TOT-12-DC-KUND                                        
003300                             PIC Z(6)9.                                   
003400     03 MOD-HISTORIK.                                                     
003500        05 MOD-RESEASON-HIST OCCURS 12 TIMES                              
003600                             PIC 9.9(2).                                  
003700*                                 SÄSONGSINDEX                            
003800     03 MOD-MASKINELL.                                                    
003900        05 MOD-KVPB-PER-MASK OCCURS 12 TIMES                              
004000                             PIC Z(5)9.                                   
004100        05 MOD-KVPB-MASK     PIC Z(5)9.9.                                 
004200*                                 PERIODBEHOV (PROGNOS)                   
004300        05 MOD-RESEASON-MASK OCCURS 12 TIMES                              
004400                             PIC 9.9(2).                                  
004500*                                 SÄSONGSINDEX                            
004600     03 MOD-MANUELL.                                                      
004700        05 MOD-KVPB-MAN      PIC Z(5)9.9.                                 
004800*                                 PERIODBEHOV (PROGNOS)                   
004900        05 MOD-TIPBPLAN-MAN  PIC X(6).                                    
005000        05 MOD-RESEASON-MAN  OCCURS 12 TIMES                              
005100                             PIC 9.9(2).                                  
005200*                                 SÄSONGSINDEX                            
005300        05 MOD-TISEASON-MAN  PIC X(6).                                    
005400     03 MOD-SIMULERING.                                                   
005500        05 MOD-KVPB-PER-SIM-GRP                                           
005600                             OCCURS 12 TIMES.                             
005700           07 MOD-KVPB-PER-SIM-ATTR                                       
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000           07 MOD-KVPB-PER-SIM                                            
006100                             PIC Z(5)9.                                   
006200        05 MOD-RESEASON-SIM-GRP                                           
006300                             OCCURS 12 TIMES.                             
006400           07 MOD-RESEASON-SIM-ATTR                                       
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700           07 MOD-RESEASON-SIM                                            
006800                             PIC X(4).                                    
006900        05 MOD-TISEASON-SIM-ATTR                                          
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-TISEASON-SIM  PIC X(6).                                    
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 823 BYTES                                 
