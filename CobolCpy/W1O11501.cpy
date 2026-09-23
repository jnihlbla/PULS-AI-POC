000100 01  MOD-W1O11501-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W1011500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDPROJ-ATTR      PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDPROJ           PIC X(4).                                    
001400*                                 PARTS PROJEKTIDENTITET                  
001500     03 MOD-IDPROJK-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDPROJK          PIC X(4).                                    
001800*                                 PROJEKTIDENTITET KONSTRUKTION           
001900     03 MOD-IDPROJUP         PIC X(8).                                    
002000*                                 PROJEKTUPPDRAG                          
002100     03 MOD-IDAO-ATTR        PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-IDAO             PIC X(10).                                   
002400*                                 ÄNDRINGSORDERNUMMER                     
002500     03 MOD-IDRITUTG-ATTR    PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-IDRITUTG         PIC X(3).                                    
002800*                                 RITNINGSUTGÅVA                          
002900     03 MOD-TISTABER-ATTR    PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-TISTABER         PIC 9(4).                                    
003200*                                 ÅR - VECKA  (ÅÅVV)                      
003300     03 MOD-IDAVD-ATTR       PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDAVD            PIC Z(4)9.                                   
003600*                                 DEN ANSTÄLLDES AVDELNING/               
003700*                                 KOSTNADSSTÄLLE                          
003800     03 MOD-KVARTAR1-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KVARTAR1         PIC Z(8)9.                                   
004100*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 1         
004200     03 MOD-TIRITB-ATTR      PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-TIRITB           PIC 9(4).                                    
004500*                                 ÅR - VECKA  (ÅÅVV)                      
004600     03 MOD-FLRITB-ATTR      PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLRITB           PIC X.                                       
004900*                                 KLARMARKERING B-RITNING                 
005000     03 MOD-TISLUBER-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-TISLUBER         PIC 9(4).                                    
005300*                                 ÅR - VECKA  (ÅÅVV)                      
005400     03 MOD-IDPROENH-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDPROENH         PIC X(8).                                    
005700*                                 PRODUKTIONSENHET                        
005800     03 MOD-KVARTAR2-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KVARTAR2         PIC Z(8)9.                                   
006100*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 2         
006200     03 MOD-TIRITC-ATTR      PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-TIRITC           PIC 9(4).                                    
006500*                                 ÅR - VECKA  (ÅÅVV)                      
006600     03 MOD-FLRITC-ATTR      PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-FLRITC           PIC X.                                       
006900*                                 KLARMARKERING C-RITNING                 
007000     03 MOD-TIPLAKOP-ATTR    PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-TIPLAKOP         PIC 9(4).                                    
007300*                                 ÅR - VECKA  (ÅÅVV)                      
007400     03 MOD-PRARTSTD-ATTR    PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
007700*                                 ARTIKELSTANDARDPRIS                     
007800     03 MOD-KDSTAINK-ATTR    PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KDSTAINK         PIC 9.                                       
008100*                                 STATUS PRIS FRÅN INKÖP                  
008200     03 MOD-KVARTAR3-ATTR    PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-KVARTAR3         PIC Z(8)9.                                   
008500*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 3         
008600     03 MOD-TIRITP-ATTR      PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-TIRITP           PIC 9(4).                                    
008900*                                 ÅR - VECKA  (ÅÅVV)                      
009000     03 MOD-FLRITP-ATTR      PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-FLRITP           PIC X.                                       
009300*                                 KLARMARKERING P-RITNING                 
009400     03 MOD-TIANSKREG        PIC 9(4).                                    
009500*                                 ÅR - VECKA  (ÅÅVV)                      
009600     03 MOD-IDRITN-ATTR      PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-IDRITN           PIC X(10).                                   
009900*                                 RITNINGSNUMMER                          
010000     03 MOD-TIBEST           PIC 9(4).                                    
010100*                                 ÅR - VECKA  (ÅÅVV)                      
010200     03 MOD-IDFKNGRP-ATTR    PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
010500*                                 FUNKTIONSGRUPP                          
010600     03 MOD-IDINK-ATTR       PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-IDINK            PIC X(4).                                    
010900*                                 INKÖPARNUMMER                           
011000     03 MOD-KDSORT-ATTR      PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-KDSORT           PIC X(2).                                    
011300*                                 SORT-KOD                                
011400     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-IDLEVNR          PIC X(5).                                    
011700*                                 LEVERANTÖRNUMMER                        
011800     03 MOD-IDLEVNR-FORB1-ATTR                                            
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-IDLEVNR-FORB1    PIC X(5).                                    
012200*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
012300     03 MOD-IDLEVNR-FORB2-ATTR                                            
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-IDLEVNR-FORB2    PIC X(5).                                    
012700*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
012800     03 MOD-IDLEVNR-FORB3-ATTR                                            
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-IDLEVNR-FORB3    PIC X(5).                                    
013200*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
013300     03 MOD-IDLEVNR-FORB4-ATTR                                            
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600     03 MOD-IDLEVNR-FORB4    PIC X(5).                                    
013700*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
013800     03 MOD-IDLEVNR-FORB5-ATTR                                            
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-IDLEVNR-FORB5    PIC X(5).                                    
014200*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
014300     03 MOD-IDKAT-ATTR       PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-IDKAT            PIC X(5).                                    
014600*                                 KATALOGBETECKNING                       
014700     03 MOD-TIUPG-ATTR       PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900     03 MOD-TIUPG            PIC 9(4).                                    
015000*                                 ÅR - VECKA  (ÅÅVV)                      
015100     03 MOD-FLUPG-ATTR       PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300     03 MOD-FLUPG            PIC X.                                       
015400*                                 FLAGGA UTFALLSPROV GODKÄNT              
015500     03 MOD-TISERLEV1-ATTR   PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700     03 MOD-TISERLEV1        PIC 9(4).                                    
015800*                                 ÅR - VECKA  (ÅÅVV)                      
015900     03 MOD-TISERLEV2-ATTR   PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 MOD-TISERLEV2        PIC 9(4).                                    
016200*                                 ÅR - VECKA  (ÅÅVV)                      
016300     03 MOD-TISERLEV3-ATTR   PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500     03 MOD-TISERLEV3        PIC 9(4).                                    
016600*                                 ÅR - VECKA  (ÅÅVV)                      
016700     03 MOD-TISERLEV4-ATTR   PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900     03 MOD-TISERLEV4        PIC 9(4).                                    
017000*                                 ÅR - VECKA  (ÅÅVV)                      
017100     03 MOD-TISERLEV5-ATTR   PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300     03 MOD-TISERLEV5        PIC 9(4).                                    
017400*                                 ÅR - VECKA  (ÅÅVV)                      
017500     03 MOD-TILEVBEG         PIC 9(4).                                    
017600*                                 ÅR - VECKA  (ÅÅVV)                      
017700     03 MOD-KVLEVBEG         PIC Z(6)9.                                   
017800*                                 BEGÄRT ANTAL ATT LEVERERAS              
017900     03 MOD-KVPROG           PIC Z(6)9.                                   
018000*                                 ÅRSPROGNOS                              
018100     03 MOD-BEART-ATTR       PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-BEART            PIC X(25).                                   
018400*                                 ARTIKELBENÄMNING                        
018500     03 MOD-TETEKNIK-ATTR    PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-TETEKNIK         PIC X(50).                                   
018800*                                 TEKNISK INFO FRÅN PV/LV                 
018900     03 MOD-KDRESBED-UT-ATTR PIC X(2).                                    
019000*                                 MFS ATTRIBUTFÄLT                        
019100     03 MOD-KDRESBED-UT      PIC X.                                       
019200*                                 RESERVDELSBEDÖMNINGSKOD                 
019300     03 MOD-TINEDBRY-UT-ATTR PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500     03 MOD-TINEDBRY-UT      PIC 9(4).                                    
019600*                                 ÅR - VECKA  (ÅÅVV)                      
019700     03 MOD-KDRESBED-IN-ATTR PIC X(2).                                    
019800*                                 MFS ATTRIBUTFÄLT                        
019900     03 MOD-KDRESBED-IN      PIC X(2).                                    
020000*                                 MFS BEHANDLING AV INPUTFÄLT             
020100     03 MOD-TINEDBRY-IN-ATTR PIC X(2).                                    
020200*                                 MFS ATTRIBUTFÄLT                        
020300     03 MOD-TINEDBRY-IN      PIC X(2).                                    
020400*                                 MFS BEHANDLING AV INPUTFÄLT             
020500     03 MOD-AVSL-NOT-IN-UT-ATTR                                           
020600                             PIC X(2).                                    
020700*                                 MFS ATTRIBUTFÄLT                        
020800     03 MOD-AVSL-NOT-IN-UT   PIC X(40).                                   
020900*                                 ARTIKEL NOTERING                        
021000     03 MOD-SLAECK-9KOMPL-IN-ATTR                                         
021100                             PIC X(2).                                    
021200*                                 MFS ATTRIBUTFÄLT                        
021300     03 MOD-SLAECK-9KOMPL-IN PIC X(2).                                    
021400*                                 MFS BEHANDLING AV INPUTFÄLT             
021500     03 MOD-TEMFSINF         PIC X(55).                                   
021600*                                 INFORMATIONSMEDDELANDE                  
021700*** END OF VILMAII-COPY LENGTH= 541 BYTES                                 
