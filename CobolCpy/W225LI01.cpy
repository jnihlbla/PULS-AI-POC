000100 01  W225LI01.                                                            
000200*                                 OBS MOTSVARANDE E-COPYTEXT              
000300*                                 MÅSTE ÄNDRAS SAMTIDIGT                  
000400*                                 LISTRECORD FÖR SERVICE GRAD PER         
000500*                                 ANSKAFFARE                              
000600*                                                                         
000700     03 SORTARGUMENT.                                                     
000800        05 IDLISTA           PIC S9(3)           COMP-3.                  
000900*                                 LISTNUMMER                              
001000        05 IDANSK            PIC S9(3)           COMP-3.                  
001100*                                 ANSKAFFARNUMMER                         
001200        05 IDLEVNR           PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400        05 IDARTNR           PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600     03 CDC-INFO.                                                         
001700        05 KDPRIO-CDC        PIC S9              COMP-3.                  
001800*                                 PRIORITETSKOD                           
001900        05 FLTOPP-CDC        PIC X.                                       
002000*                                 TOPP-200-ARTIKEL                        
002100        05 KVROS-CDC-1-2     PIC S9(7)           COMP-3.                  
002200*                                 RESTORDERSALDO                          
002300*                                 ORDERKLASS 1 OCH 2                      
002400        05 KVROS-CDC-3-4     PIC S9(7)           COMP-3.                  
002500*                                 RESTORDERSALDO                          
002600*                                 ORDERKLASS 3 - 4                        
002700        05 KVDISPL-CDC       PIC S9(7)           COMP-3.                  
002800*                                 DISPONIBELT LAGER                       
002900        05 KVAKS-TILLG-CDC   PIC S9(7)           COMP-3.                  
003000*                                 AK-TILLGÅNGAR                           
003100        05 KVUTRS-CDC        PIC S9(7)           COMP-3.                  
003200*                                 UTREDNINGSSALDO                         
003300        05 KVSPANT-CDC       PIC S9(7)           COMP-3.                  
003400*                                 SPÄRRAT ANTAL                           
003500        05 KVSLAGER-CDC      PIC S9(7)           COMP-3.                  
003600*                                 SÄKERHETSLAGER                          
003700        05 TIAVIDAT-CDC-SEN  PIC S9(7)           COMP-3.                  
003800*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
003900        05 KVAVIS-CDC        PIC S9(7)           COMP-3.                  
004000*                                 AVISERAT ANTAL                          
004100        05 KVPB-TOT-CDC      PIC S9(6)V9(1)      COMP-3.                  
004200*                                 TOTALT PERIODBEHOV                      
004300        05 TIPBDAT-CDC       PIC S9(7)           COMP-3.                  
004400*                                 PROGNOSDATUM ÅÅMMDD TIPBDAT-002         
004500        05 KDERS-CDC         PIC S9(3)           COMP-3.                  
004600*                                 ERSÄTTNINGSKOD                          
004700        05 TIINVDAT-CDC      PIC S9(5)           COMP-3.                  
004800*                                 INVENTERINGSDATUM                       
004900        05 TIRODAT-CDC       PIC S9(5)           COMP-3.                  
005000*                                 RESTORDERDATUM      TIRODAT-002         
005100*                                 (AAVVD)                                 
005200        05 TIRODAT-ORDER-CDC PIC S9(5)           COMP-3.                  
005300*                                               TIRODAT-ORDER-002         
005400*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
005500        05 SUROBEL-CDC       PIC S9(7)V9(2)      COMP-3.                  
005600*                                 RESTORDERVÄRDE STANDARDPRIS             
005700        05 KDSPARR-CDC       PIC X.                                       
005800*                                 SPÄRRKOD                                
005900     03 SDC-INFO.                                                         
006000        05 KDPRIO-SDC        PIC S9              COMP-3.                  
006100*                                 PRIORITETSKOD                           
006200        05 FLTOPP-SDC        PIC X.                                       
006300*                                 TOPP-200-ARTIKEL                        
006400        05 KVROS-SDC-1-2     PIC S9(7)           COMP-3.                  
006500*                                 RESTORDERSALDO                          
006600*                                 ORDERKLASS 1 OCH 2                      
006700        05 KVROS-SDC-3-4     PIC S9(7)           COMP-3.                  
006800*                                 RESTORDERSALDO                          
006900*                                 ORDERKLASS 3 - 4                        
007000        05 KVDISPL-SDC       PIC S9(7)           COMP-3.                  
007100*                                 DISPONIBELT LAGER                       
007200        05 KVAKS-TILLG-SDC   PIC S9(7)           COMP-3.                  
007300*                                 AK-TILLGÅNGAR                           
007400        05 KVUTRS-SDC        PIC S9(7)           COMP-3.                  
007500*                                 UTREDNINGSSALDO                         
007600        05 KVSPANT-SDC       PIC S9(7)           COMP-3.                  
007700*                                 SPÄRRAT ANTAL                           
007800        05 KVSLAGER-SDC      PIC S9(7)           COMP-3.                  
007900*                                 SÄKERHETSLAGER                          
008000        05 TIAVIDAT-SDC-SEN  PIC S9(7)           COMP-3.                  
008100*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
008200        05 KVAVIS-SDC        PIC S9(7)           COMP-3.                  
008300*                                 AVISERAT ANTAL                          
008400        05 KVPB-TOT-SDC      PIC S9(6)V9(1)      COMP-3.                  
008500*                                 TOTALT PERIODBEHOV                      
008600        05 TIPBDAT-SDC       PIC S9(7)           COMP-3.                  
008700*                                 PROGNOSDATUM ÅÅMMDD TIPBDAT-002         
008800        05 KDERS-SDC         PIC S9(3)           COMP-3.                  
008900*                                 ERSÄTTNINGSKOD                          
009000        05 TIINVDAT-SDC      PIC S9(5)           COMP-3.                  
009100*                                 INVENTERINGSDATUM                       
009200        05 TIRODAT-SDC       PIC S9(5)           COMP-3.                  
009300*                                 RESTORDERDATUM      TIRODAT-002         
009400*                                 (AAVVD)                                 
009500        05 TIRODAT-ORDER-SDC PIC S9(5)           COMP-3.                  
009600*                                               TIRODAT-ORDER-002         
009700*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
009800        05 SUROBEL-SDC       PIC S9(7)V9(2)      COMP-3.                  
009900*                                 RESTORDERVÄRDE STANDARDPRIS             
010000        05 KDSPARR-SDC       PIC X.                                       
010100*                                 SPÄRRKOD                                
010200     03 CDC-SERVICE-INFO.                                                 
010300        05 KVRORAD-CDC-TOT   PIC S9(7)V9(2)      COMP-3.                  
010400*                                                   (KVRORAD-003)         
010500*                                 TOTALT ANTAL RESTORDERRADER             
010600        05 KVRORAD-CDC-1-2   PIC S9(7)V9(2)      COMP-3.                  
010700*                                 RESTNOTERADE RADER                      
010800*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
010900*                                 3)                                      
011000        05 KVRORAD-CDC-3-4   PIC S9(7)V9(2)      COMP-3.                  
011100*                                 RESTNOTERADE RADER                      
011200*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
011300        05 KVRORAD-CDC-1-2-VECKA                                          
011400                             PIC S9(7)V9(2)      COMP-3.                  
011500*                                 RESTNOTERADE RADER                      
011600*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
011700*                                 3)                                      
011800        05 KVRORAD-CDC-3-4-VECKA                                          
011900                             PIC S9(7)V9(2)      COMP-3.                  
012000*                                 RESTNOTERADE RADER                      
012100*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
012200        05 KVAVBRAD-CDC-1-2  PIC S9(7)V9(2)      COMP-3.                  
012300*                                 AVBOKADE RADER                          
012400*                                 ORDERKLASS 1 OCH 2                      
012500        05 KVAVBRAD-CDC-3-4  PIC S9(7)V9(2)      COMP-3.                  
012600*                                 AVBOKADE RADER                          
012700*                                 ORDERKLASS 3 - 4                        
012800        05 KVFYSAVV-CDC-1-2  PIC S9(5)V9(2)      COMP-3.                  
012900*                                 FYSISK AVVIKELSE RADER                  
013000*                                 ORDERKLASS 1 OCH 2                      
013100        05 KVFYSAVV-CDC-3-4  PIC S9(5)V9(2)      COMP-3.                  
013200*                                 FYSISK AVVIKELSE RADER                  
013300*                                 ORDERKLASS 3 - 4                        
013400        05 KVINORD-CDC-1-2   PIC S9(7)           COMP-3.                  
013500*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
013600        05 KVINORD-CDC-3-4   PIC S9(7)           COMP-3.                  
013700*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
013800        05 KVEJRO-CDC-1-2    PIC S9(5)V9(2)      COMP-3.                  
013900*                                 OLEVERERAT EJ RESTNOTERAT               
014000*                                 ORDERKLASS 1 OCH 2                      
014100        05 KVEJRO-CDC-3-4    PIC S9(5)V9(2)      COMP-3.                  
014200*                                 OLEVERERAT EJ RESTNOTERAT               
014300*                                 ORDERKLASS 3 - 4                        
014400     03 SDC-INFO.                                                         
014500        05 KVRORAD-SDC-TOT   PIC S9(7)V9(2)      COMP-3.                  
014600*                                                   (KVRORAD-003)         
014700*                                 TOTALT ANTAL RESTORDERRADER             
014800        05 KVRORAD-SDC-1-2   PIC S9(7)V9(2)      COMP-3.                  
014900*                                 RESTNOTERADE RADER                      
015000*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
015100*                                 3)                                      
015200        05 KVRORAD-SDC-3-4   PIC S9(7)V9(2)      COMP-3.                  
015300*                                 RESTNOTERADE RADER                      
015400*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
015500        05 KVRORAD-SDC-1-2-VECKA                                          
015600                             PIC S9(7)V9(2)      COMP-3.                  
015700*                                 RESTNOTERADE RADER                      
015800*                                 ORDERKLASS 1 OCH 2  (KVRORAD-00         
015900*                                 3)                                      
016000        05 KVRORAD-SDC-3-4-VECKA                                          
016100                             PIC S9(7)V9(2)      COMP-3.                  
016200*                                 RESTNOTERADE RADER                      
016300*                                 ORDERKLASS 3 - 4  (KVRORAD-003)         
016400        05 KVAVBRAD-SDC-1-2  PIC S9(7)V9(2)      COMP-3.                  
016500*                                 AVBOKADE RADER                          
016600*                                 ORDERKLASS 1 OCH 2                      
016700        05 KVAVBRAD-SDC-3-4  PIC S9(7)V9(2)      COMP-3.                  
016800*                                 AVBOKADE RADER                          
016900*                                 ORDERKLASS 3 - 4                        
017000        05 KVFYSAVV-SDC-1-2  PIC S9(5)V9(2)      COMP-3.                  
017100*                                 FYSISK AVVIKELSE RADER                  
017200*                                 ORDERKLASS 1 OCH 2                      
017300        05 KVFYSAVV-SDC-3-4  PIC S9(5)V9(2)      COMP-3.                  
017400*                                 FYSISK AVVIKELSE RADER                  
017500*                                 ORDERKLASS 3 - 4                        
017600        05 KVINORD-SDC-1-2   PIC S9(7)           COMP-3.                  
017700*                                 ORDERINGÅNG ORDERKLASS 1 OCH 2          
017800        05 KVINORD-SDC-3-4   PIC S9(7)           COMP-3.                  
017900*                                 ORDERINGÅNG ORDERKLASS 3 - 4            
018000        05 KVEJRO-SDC-1-2    PIC S9(5)V9(2)      COMP-3.                  
018100*                                 OLEVERERAT EJ RESTNOTERAT               
018200*                                 ORDERKLASS 1 OCH 2                      
018300        05 KVEJRO-SDC-3-4    PIC S9(5)V9(2)      COMP-3.                  
018400*                                 OLEVERERAT EJ RESTNOTERAT               
018500*                                 ORDERKLASS 3 - 4                        
018600     03 LEVERANS-INFO.                                                    
018700        05 KVAVIS-LEVBESK-1  PIC S9(7)           COMP-3.                  
018800*                                 AVISERAT ANTAL                          
018900        05 TIAVIDAT-LEVBESK-1                                             
019000                             PIC S9(7)           COMP-3.                  
019100*                                 AVISERINGSDATUM (YYMMDD)                
019200        05 KVAVIS-LEVBESK-2  PIC S9(7)           COMP-3.                  
019300*                                 AVISERAT ANTAL                          
019400        05 TIAVIDAT-LEVBESK-2                                             
019500                             PIC S9(7)           COMP-3.                  
019600*                                 AVISERINGSDATUM (YYMMDD)                
019700        05 KVAVIS-LEVBESK-3  PIC S9(7)           COMP-3.                  
019800*                                 AVISERAT ANTAL                          
019900        05 TIAVIDAT-LEVBESK-3                                             
020000                             PIC S9(7)           COMP-3.                  
020100*                                 AVISERINGSDATUM (YYMMDD)                
020200     03 GEMENSAM-URVALS-INFO.                                             
020300        05 KDHF              PIC S9              COMP-3.                  
020400*                                 HUVUDFÖRRÅDSMÄRKNING                    
020500        05 KDPROD            PIC S9(3)           COMP-3.                  
020600*                                 PRODUKTIONSKOD                          
020700        05 KDGK              PIC S9              COMP-3.                  
020800*                                 GODSMOTTAGAREKOD                        
020900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
021000*                                 PRODUKTSLAG                             
021100        05 TIFINLV           PIC S9(5)           COMP-3.                  
021200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
021300        05 KDCLPOST          PIC S9              COMP-3.                  
021400*                                 CENTRALLAGERPOST                        
021500        05 BEART-SVE         PIC X(25).                                   
021600*                                 SVENSK ARTIKELBENÄMNING                 
021700        05 IDLKTO            PIC S9(7)           COMP-3.                  
021800*                                 LAGERKONTO (FFHHHUU)                    
021900        05 KDVVKL            PIC S9              COMP-3.                  
022000*                                 VOLYMVÄRDESKLASS                        
022100        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
022200*                                 FUNKTIONSGRUPP                          
022300        05 IDAETNR           PIC S9(3)           COMP-3.                  
022400*                                 ÄNDRINGSTILLFÄLLENUMMER                 
022500        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
022600*                                 ARTIKELSTANDARDPRIS                     
022700        05 KVBR              PIC S9(7)           COMP-3.                  
022800*                                 BESTÄLLNINGSREST                        
022900        05 KDLTK             PIC S9              COMP-3.                  
023000*                                 LAGERTILLHÖRIGHETSKOD                   
023100        05 KDUART            PIC X.                                       
023200*                                 UNDANTAGSARTIKEL                        
023300*** END OF VILMAII-COPY LENGTH= 338 BYTES                                 
