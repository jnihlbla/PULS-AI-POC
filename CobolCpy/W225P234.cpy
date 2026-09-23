000100 01  W225P234.                                                            
000200*                                 POSTTYP 234 INFO FRÅN WDE2 OCH          
000300*                                 W22509 FRÅN W22509 HAR INNE             
000400*                                 VARANDE VECKAS INFO HÄMTATS             
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 KDCLPOST             PIC S9              COMP-3.                  
001100*                                 CENTRALLAGERPOST                        
001200     03 CDC-INFO.                                                         
001300        05 FILLER            OCCURS 3 TIMES.                              
001400           07 KVAVBRAD-CDC-1-2                                            
001500                             PIC S9(7)V9(2)      COMP-3.                  
001600*                                 AVBOKADE RADER                          
001700*                                 ORDERKLASS 1 OCH 2                      
001800           07 KVAVBRAD-CDC-3-4                                            
001900                             PIC S9(7)V9(2)      COMP-3.                  
002000*                                 AVBOKADE RADER                          
002100*                                 ORDERKLASS 3 - 4                        
002200           07 KVFYSAVV-CDC-1-2                                            
002300                             PIC S9(5)V9(2)      COMP-3.                  
002400*                                 FYSISK AVVIKELSE RADER                  
002500*                                 ORDERKLASS 1 OCH 2                      
002600           07 KVFYSAVV-CDC-3-4                                            
002700                             PIC S9(5)V9(2)      COMP-3.                  
002800*                                 FYSISK AVVIKELSE RADER                  
002900*                                 ORDERKLASS 3 - 4                        
003000           07 KVINORD-CDC-1-2                                             
003100                             PIC S9(7)           COMP-3.                  
003200*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
003300           07 KVINORD-CDC-3-4                                             
003400                             PIC S9(7)           COMP-3.                  
003500*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
003600           07 KVRORAD-CDC-1-2                                             
003700                             PIC S9(7)V9(2)      COMP-3.                  
003800*                                 RESTNOTERADE RADER                      
003900*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
004000*                                 3)                                      
004100           07 KVRORAD-CDC-3-4                                             
004200                             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 RESTNOTERADE RADER                      
004400*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
004500        05 KVRORAD-CDC-TOT   PIC S9(7)V9(2)      COMP-3.                  
004600*                                                   (KVRORAD-003)         
004700*                                 TOTALT ANTAL RESTORDERRADER             
004800        05 KVRORAD-CDC-1-2-VECKA                                          
004900                             PIC S9(7)V9(2)      COMP-3.                  
005000*                                 RESTNOTERADE RADER                      
005100*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
005200*                                 3)                                      
005300        05 KVRORAD-CDC-3-4-VECKA                                          
005400                             PIC S9(7)V9(2)      COMP-3.                  
005500*                                 RESTNOTERADE RADER                      
005600*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
005700        05 KVRORAD-KVAR-V1-CDC                                            
005800                             PIC S9(7)V9(2)      COMP-3.                  
005900*                                             KVRORAD-KVAR-V1-003         
006000*                                 RESTNOTERADE RADER VECKA1               
006100        05 KVRORAD-KVAR-IV-CDC                                            
006200                             PIC S9(7)V9(2)      COMP-3.                  
006300*                                             KVRORAD-KVAR-IV-003         
006400*                                 RESTNOTERADE RADER                      
006500*                                 INNEVARANDE VECKA                       
006600        05 KVRORAD-KVAR-P-CDC                                             
006700                             PIC S9(7)V9(2)      COMP-3.                  
006800*                                              KVRORAD-KVAR-P-003         
006900*                                 RESTNOTERADE RADER PERIOD               
007000        05 KVEJRO-CDC-1-2    PIC S9(5)V9(2)      COMP-3.                  
007100*                                 OLEVERERAT EJ RESTNOTERAT               
007200*                                 ORDERKLASS 1 OCH 2                      
007300        05 KVEJRO-CDC-3-4    PIC S9(5)V9(2)      COMP-3.                  
007400*                                 OLEVERERAT EJ RESTNOTERAT               
007500*                                 ORDERKLASS 3 - 4                        
007600        05 KVROS-CDC-1-2     PIC S9(7)           COMP-3.                  
007700*                                 RESTORDERSALDO                          
007800*                                 ORDERKLASS 1 OCH 2                      
007900        05 KVROS-CDC-3-4     PIC S9(7)           COMP-3.                  
008000*                                 RESTORDERSALDO                          
008100*                                 ORDERKLASS 3 - 4                        
008200        05 TIRODAT-ORDER-CDC PIC S9(5)           COMP-3.                  
008300*                                               TIRODAT-ORDER-002         
008400*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
008500     03 SDC-INFO.                                                         
008600        05 FILLER            OCCURS 3 TIMES.                              
008700           07 KVAVBRAD-SDC-1-2                                            
008800                             PIC S9(7)V9(2)      COMP-3.                  
008900*                                 AVBOKADE RADER                          
009000*                                 ORDERKLASS 1 OCH 2                      
009100           07 KVAVBRAD-SDC-3-4                                            
009200                             PIC S9(7)V9(2)      COMP-3.                  
009300*                                 AVBOKADE RADER                          
009400*                                 ORDERKLASS 3 - 4                        
009500           07 KVFYSAVV-SDC-1-2                                            
009600                             PIC S9(5)V9(2)      COMP-3.                  
009700*                                 FYSISK AVVIKELSE RADER                  
009800*                                 ORDERKLASS 1 OCH 2                      
009900           07 KVFYSAVV-SDC-3-4                                            
010000                             PIC S9(5)V9(2)      COMP-3.                  
010100*                                 FYSISK AVVIKELSE RADER                  
010200*                                 ORDERKLASS 3 - 4                        
010300           07 KVINORD-SDC-1-2                                             
010400                             PIC S9(7)           COMP-3.                  
010500*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
010600           07 KVINORD-SDC-3-4                                             
010700                             PIC S9(7)           COMP-3.                  
010800*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
010900           07 KVRORAD-SDC-1-2                                             
011000                             PIC S9(7)V9(2)      COMP-3.                  
011100*                                 RESTNOTERADE RADER                      
011200*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
011300*                                 3)                                      
011400           07 KVRORAD-SDC-3-4                                             
011500                             PIC S9(7)V9(2)      COMP-3.                  
011600*                                 RESTNOTERADE RADER                      
011700*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
011800        05 KVRORAD-SDC-TOT   PIC S9(7)V9(2)      COMP-3.                  
011900*                                                   (KVRORAD-003)         
012000*                                 TOTALT ANTAL RESTORDERRADER             
012100        05 KVRORAD-SDC-1-2-VECKA                                          
012200                             PIC S9(7)V9(2)      COMP-3.                  
012300*                                 RESTNOTERADE RADER                      
012400*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
012500*                                 3)                                      
012600        05 KVRORAD-SDC-3-4-VECKA                                          
012700                             PIC S9(7)V9(2)      COMP-3.                  
012800*                                 RESTNOTERADE RADER                      
012900*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
013000        05 KVRORAD-KVAR-V1-SDC                                            
013100                             PIC S9(7)V9(2)      COMP-3.                  
013200*                                             KVRORAD-KVAR-V1-003         
013300*                                 RESTNOTERADE RADER VECKA1               
013400        05 KVRORAD-KVAR-IV-SDC                                            
013500                             PIC S9(7)V9(2)      COMP-3.                  
013600*                                             KVRORAD-KVAR-IV-003         
013700*                                 RESTNOTERADE RADER                      
013800*                                 INNEVARANDE VECKA                       
013900        05 KVRORAD-KVAR-P-SDC                                             
014000                             PIC S9(7)V9(2)      COMP-3.                  
014100*                                              KVRORAD-KVAR-P-003         
014200*                                 RESTNOTERADE RADER PERIOD               
014300        05 KVEJRO-SDC-1-2    PIC S9(5)V9(2)      COMP-3.                  
014400*                                 OLEVERERAT EJ RESTNOTERAT               
014500*                                 ORDERKLASS 1 OCH 2                      
014600        05 KVEJRO-SDC-3-4    PIC S9(5)V9(2)      COMP-3.                  
014700*                                 OLEVERERAT EJ RESTNOTERAT               
014800*                                 ORDERKLASS 3 - 4                        
014900        05 KVROS-SDC-1-2     PIC S9(7)           COMP-3.                  
015000*                                 RESTORDERSALDO                          
015100*                                 ORDERKLASS 1 OCH 2                      
015200        05 KVROS-SDC-3-4     PIC S9(7)           COMP-3.                  
015300*                                 RESTORDERSALDO                          
015400*                                 ORDERKLASS 3 - 4                        
015500        05 TIRODAT-ORDER-SDC PIC S9(5)           COMP-3.                  
015600*                                               TIRODAT-ORDER-002         
015700*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
015800*** END OF VILMAII-COPY LENGTH= 323 BYTES                                 
