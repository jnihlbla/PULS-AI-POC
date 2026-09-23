000100 01  W225P233.                                                            
000200*                                 POSTTYP 233 REGISTER ÖVER               
000300*                                 AVBOKADE RADER MM INDEX1= 1:A           
000400*                                 VECKAN INOM PERIODEN INDEX2=            
000500*                                 INNEVARANDE VECKA INDEX3= SUMMA         
000600*                                 AV ALLA VECKOR INOM PERIODEN            
000700*                                                                         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 CDC-INFO.                                                         
001300        05 FILLER            OCCURS 3 TIMES.                              
001400           07 KVAVBRAD-CDC-0 PIC S9(7)V9(2)      COMP-3.                  
001500*                                 AVBOKADE RADER                          
001600*                                 ORDERKLASS 0                            
001700           07 KVAVBRAD-CDC-1-2                                            
001800                             PIC S9(7)V9(2)      COMP-3.                  
001900*                                 AVBOKADE RADER                          
002000*                                 ORDERKLASS 1 OCH 2                      
002100           07 KVAVBRAD-CDC-3-4                                            
002200                             PIC S9(7)V9(2)      COMP-3.                  
002300*                                 AVBOKADE RADER                          
002400*                                 ORDERKLASS 3 - 4                        
002500           07 KVFYSAVV-CDC-0 PIC S9(5)V9(2)      COMP-3.                  
002600*                                 FYSISK AVVIKELSE RADER                  
002700*                                 ORDERKLASS 0                            
002800           07 KVFYSAVV-CDC-1-2                                            
002900                             PIC S9(5)V9(2)      COMP-3.                  
003000*                                 FYSISK AVVIKELSE RADER                  
003100*                                 ORDERKLASS 1 OCH 2                      
003200           07 KVFYSAVV-CDC-3-4                                            
003300                             PIC S9(5)V9(2)      COMP-3.                  
003400*                                 FYSISK AVVIKELSE RADER                  
003500*                                 ORDERKLASS 3 - 4                        
003600           07 KVINORD-CDC-0  PIC S9(7)           COMP-3.                  
003700*                                 ORDERINGÅNG ORDERKLASS 0                
003800           07 KVINORD-CDC-1-2                                             
003900                             PIC S9(7)           COMP-3.                  
004000*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
004100           07 KVINORD-CDC-3-4                                             
004200                             PIC S9(7)           COMP-3.                  
004300*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
004400           07 KVRORAD-CDC-0  PIC S9(7)V9(2)      COMP-3.                  
004500*                                 RESTNOTERADE RADER                      
004600*                                 ORDERKLASS 0      (KVRORAD-003)         
004700           07 KVRORAD-CDC-1-2                                             
004800                             PIC S9(7)V9(2)      COMP-3.                  
004900*                                 RESTNOTERADE RADER                      
005000*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
005100*                                 3)                                      
005200           07 KVRORAD-CDC-3-4                                             
005300                             PIC S9(7)V9(2)      COMP-3.                  
005400*                                 RESTNOTERADE RADER                      
005500*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
005600           07 KVRORAD-CDC-0-VECKA                                         
005700                             PIC S9(7)V9(2)      COMP-3.                  
005800*                                 RESTNOTERADE RADER                      
005900*                                 ORDERKLASS 0      (KVRORAD-003)         
006000           07 KVRORAD-CDC-1-2-VECKA                                       
006100                             PIC S9(7)V9(2)      COMP-3.                  
006200*                                 RESTNOTERADE RADER                      
006300*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
006400*                                 3)                                      
006500           07 KVRORAD-CDC-3-4-VECKA                                       
006600                             PIC S9(7)V9(2)      COMP-3.                  
006700*                                 RESTNOTERADE RADER                      
006800*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
006900           07 KVEJRO-CDC-0   PIC S9(5)V9(2)      COMP-3.                  
007000*                                 OLEVERERAT EJ RESTNOTERAT               
007100*                                 ORDERKLASS 0                            
007200           07 KVEJRO-CDC-1-2 PIC S9(5)V9(2)      COMP-3.                  
007300*                                 OLEVERERAT EJ RESTNOTERAT               
007400*                                 ORDERKLASS 1 OCH 2                      
007500           07 KVEJRO-CDC-3-4 PIC S9(5)V9(2)      COMP-3.                  
007600*                                 OLEVERERAT EJ RESTNOTERAT               
007700*                                 ORDERKLASS 3 - 4                        
007800     03 SDC-INFO.                                                         
007900        05 FILLER            OCCURS 3 TIMES.                              
008000           07 KVAVBRAD-SDC-0 PIC S9(7)V9(2)      COMP-3.                  
008100*                                 AVBOKADE RADER                          
008200*                                 ORDERKLASS 0                            
008300           07 KVAVBRAD-SDC-1-2                                            
008400                             PIC S9(7)V9(2)      COMP-3.                  
008500*                                 AVBOKADE RADER                          
008600*                                 ORDERKLASS 1 OCH 2                      
008700           07 KVAVBRAD-SDC-3-4                                            
008800                             PIC S9(7)V9(2)      COMP-3.                  
008900*                                 AVBOKADE RADER                          
009000*                                 ORDERKLASS 3 - 4                        
009100           07 KVFYSAVV-SDC-0 PIC S9(5)V9(2)      COMP-3.                  
009200*                                 FYSISK AVVIKELSE RADER                  
009300*                                 ORDERKLASS 0                            
009400           07 KVFYSAVV-SDC-1-2                                            
009500                             PIC S9(5)V9(2)      COMP-3.                  
009600*                                 FYSISK AVVIKELSE RADER                  
009700*                                 ORDERKLASS 1 OCH 2                      
009800           07 KVFYSAVV-SDC-3-4                                            
009900                             PIC S9(5)V9(2)      COMP-3.                  
010000*                                 FYSISK AVVIKELSE RADER                  
010100*                                 ORDERKLASS 3 - 4                        
010200           07 KVINORD-SDC-0  PIC S9(7)           COMP-3.                  
010300*                                 ORDERINGÅNG ORDERKLASS 0                
010400           07 KVINORD-SDC-1-2                                             
010500                             PIC S9(7)           COMP-3.                  
010600*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
010700           07 KVINORD-SDC-3-4                                             
010800                             PIC S9(7)           COMP-3.                  
010900*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
011000           07 KVRORAD-SDC-0  PIC S9(7)V9(2)      COMP-3.                  
011100*                                 RESTNOTERADE RADER                      
011200*                                 ORDERKLASS 0      (KVRORAD-003)         
011300           07 KVRORAD-SDC-1-2                                             
011400                             PIC S9(7)V9(2)      COMP-3.                  
011500*                                 RESTNOTERADE RADER                      
011600*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
011700*                                 3)                                      
011800           07 KVRORAD-SDC-3-4                                             
011900                             PIC S9(7)V9(2)      COMP-3.                  
012000*                                 RESTNOTERADE RADER                      
012100*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
012200           07 KVRORAD-SDC-0-VECKA                                         
012300                             PIC S9(7)V9(2)      COMP-3.                  
012400*                                 RESTNOTERADE RADER                      
012500*                                 ORDERKLASS 0      (KVRORAD-003)         
012600           07 KVRORAD-SDC-1-2-VECKA                                       
012700                             PIC S9(7)V9(2)      COMP-3.                  
012800*                                 RESTNOTERADE RADER                      
012900*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
013000*                                 3)                                      
013100           07 KVRORAD-SDC-3-4-VECKA                                       
013200                             PIC S9(7)V9(2)      COMP-3.                  
013300*                                 RESTNOTERADE RADER                      
013400*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
013500           07 KVEJRO-SDC-0   PIC S9(5)V9(2)      COMP-3.                  
013600*                                 OLEVERERAT EJ RESTNOTERAT               
013700*                                 ORDERKLASS 0                            
013800           07 KVEJRO-SDC-1-2 PIC S9(5)V9(2)      COMP-3.                  
013900*                                 OLEVERERAT EJ RESTNOTERAT               
014000*                                 ORDERKLASS 1 OCH 2                      
014100           07 KVEJRO-SDC-3-4 PIC S9(5)V9(2)      COMP-3.                  
014200*                                 OLEVERERAT EJ RESTNOTERAT               
014300*                                 ORDERKLASS 3 - 4                        
014400*** END OF VILMAII-COPY LENGTH= 494 BYTES                                 
