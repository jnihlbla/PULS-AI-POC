000100 01  W2O10301.                                                            
000200*                                 COPYTEXT FÖR MOD W2O10301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEXT-PLANTYP-ATTR    PIC X(2).                                    
000600*                                 MFS ATTRIBUTFÄLT                        
000700     03 TEXT-PLANTYP         PIC X(8).                                    
000800     03 TIOMSPEC             PIC X(6).                                    
000900*                                                    TIOMSPEC-002         
001000*                                 OMSPEC-DATUM (ÅÅVV) ELLER               
001100*                                 LEVERANSPLANEDATUM (ÅÅMMDD)             
001200     03 MESSAGE              PIC X(40).                                   
001300*                                 MEDDELANDEFÄLT PÅ RAD 1                 
001400     03 IDARTNR-IN-ATTR      PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 IDARTNR-IN           PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 KDBEHX-PLAN-IN-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 KDBEHX-PLAN-IN       PIC X.                                       
002100*                                 BEHANDLINGSKOD-X                        
002200     03 KDBEHX-PLAN-UT       PIC X.                                       
002300*                                 BEHANDLINGSKOD-X                        
002400     03 IDLEVNR-IN-ATTR      PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 IDLEVNR-IN           PIC X(2).                                    
002700*                                 MFS BEHANDLING AV INPUTFÄLT             
002800     03 IDARTNR.                                                          
002900        05 IDARTNR-UT        PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100        05 STRECK-1          PIC X.                                       
003200        05 REKSIFFR          PIC X.                                       
003300*                                 KONTROLLSIFFRA                          
003400     03 IDLEVNR-UT           PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600     03 IDLEVNR-SHIP-UT-ATTR PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 IDLEVNR-SHIP-UT      PIC X(5).                                    
003900*                                 SKEPPANDE LEVERANTÖR                    
004000     03 BEART-SVE            PIC X(25).                                   
004100*                                 SVENSK ARTIKELBENÄMNING                 
004200     03 KDOTFREK             PIC X.                                       
004300*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
004400     03 AREA-OUTPUT.                                                      
004500*                                                                         
004600        05 TEXT-ORSAK1-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 TEXT-ORSAK1       PIC X(8).                                    
004900        05 TEXT-ORSAK2-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 TEXT-ORSAK2       PIC X(8).                                    
005200        05 DATA-GRP1.                                                     
005300           07 IDANSK         PIC Z(2)9.                                   
005400*                                 ANSKAFFARNUMMER                         
005500           07 FILLERX2       PIC X(2).                                    
005600           07 KDAVT          PIC 9.                                       
005700*                                 AVTALSMÄRKNING                          
005800           07 FILLERX2       PIC X(2).                                    
005900           07 KVVECKOR-LT    PIC Z(2).                                    
006000*                                 ANTAL VECKOR LEDTID                     
006100        05 TIFINLV           PIC 9(5).                                    
006200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006300        05 TIURPROD          PIC 9(4).                                    
006400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006500        05 KDLPSP-ATTR       PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 KDLPSP            PIC Z.                                       
006800*                                 LEVERANSPLANESPÄRR                      
006900        05 TILPSP-ATTR       PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 TILPSP            PIC X(4).                                    
007200*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
007300        05 FLSEASON          PIC X.                                       
007400*                                 SÄSONG PÅ ARTIKEL                       
007500        05 FLTREND           PIC X.                                       
007600*                                 FLAGGA BERÄKN TREND NYTT SÄTT           
007700        05 DATA-GRP2-ATTR    PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 DATA-GRP2.                                                     
008000           07 KVBEST-PL      PIC Z(6).                                    
008100*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
008200           07 FILLER         PIC X.                                       
008300           07 KVBR           PIC Z(7).                                    
008400*                                 BESTÄLLNINGSREST                        
008500        05 DATA-GRP4.                                                     
008600           07 KVPB-PLAN-ATTR PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800           07 KVPB-PLAN      PIC Z(6).Z.                                  
008900*                                 PLANERAT PERIODBEHOV                    
009000           07 KVPB-SATS      PIC Z(6).Z.                                  
009100*                                 SATS-PERIODBEHOV                        
009200        05 RAD-TEXT          OCCURS 4 TIMES.                              
009300           07 TEXT-UPPLYSNING-ATTR                                        
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600           07 TEXT-UPPLYSNING                                             
009700                             PIC X(19).                                   
009800        05 RAD-DATA          OCCURS 5 TIMES.                              
009900           07 AVROP-GAM-ATTR PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100           07 AVROP-GAM.                                                  
010200              09 KVAVROP-GAM PIC Z(7).                                    
010300*                                 AVROPSKVANTITET                         
010400              09 SKILJETECKEN-GAM                                         
010500                             PIC X.                                       
010600              09 TIAVROP-AVS-GAM                                          
010700                             PIC X(4).                                    
010800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
010900*                                 (ÅÅVV)                                  
011000        05 RADER             OCCURS 10 TIMES.                             
011100*                                 AVROPSTABELL   NYA AVROP                
011200           07 PERIOD-ATTR    PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400           07 PERIOD.                                                     
011500              09 PERIOD-AA   PIC 9(2).                                    
011600*                                 ÅR    (ÅÅ)                              
011700              09 PERIOD-PP   PIC 9(2).                                    
011800*                                 REDOVISNINGSPERIOD                      
011900*                                 12 PER ÅR                               
012000              09 PERIOD-PARENTES                                          
012100                             PIC X.                                       
012200*                                 TECKEN                                  
012300           07 AVROP-TABELL   OCCURS 5 TIMES.                              
012400              09 AVROP-TAB-ATTR                                           
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700              09 AVROP-TAB.                                               
012800                 11 KVAVROP-TAB                                           
012900                             PIC Z(6).                                    
013000*                                 AVROPSKVANTITET                         
013100                 11 SKILJETECKEN-TAB                                      
013200                             PIC X.                                       
013300                 11 TIAVROP-AVS-TAB                                       
013400                             PIC Z(2).                                    
013500*                                                 TIAVROP-AVS-002         
013600*                                 AVSÄNDNINGSDATUM (VV)                   
013700                 11 PARENTES-TAB                                          
013800                             PIC X.                                       
013900     03 AREA-INPUT.                                                       
014000*                                                                         
014100        05 KOMKOD-IN-ATTR    PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 KOMKOD-IN         PIC X(4).                                    
014400*                                 KOMMENTARKOD                            
014500        05 AVROP-INPUT       OCCURS 4 TIMES.                              
014600           07 TIAVROP-AVS-IN-ATTR                                         
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900           07 TIAVROP-AVS-IN PIC X(4).                                    
015000*                                 AVROPSVECKA   (ÅÅVV)                    
015100           07 KVAVROP-IN-ATTR                                             
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400           07 KVAVROP-IN     PIC X(4).                                    
015500*                                                     KVAVROP-002         
015600*                                 BEGÄRD AVROPSKVANTITET                  
015700        05 KVBEST-PL-IN-ATTR PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900        05 KVBEST-PL-IN      PIC X(4).                                    
016000*                                                   KVBEST-PL-002         
016100*                                 BEGÄRD KÖPKVANT PÅ PLAN                 
016200        05 KDOMSPEC-IN-ATTR  PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400        05 KDOMSPEC-IN       PIC X(4).                                    
016500*                                                    KDOMSPEC-002         
016600*                                 BEGÄRD OMSPEC AV PLAN                   
016700        05 TILPSP-IN-ATTR    PIC X(2).                                    
016800*                                 MFS ATTRIBUTFÄLT                        
016900        05 TILPSP-IN         PIC X(4).                                    
017000*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
017100        05 KDLEVPLF-IN-ATTR  PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300        05 KDLEVPLF-IN       PIC X(4).                                    
017400        05 FLJIT-IN-ATTR     PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600        05 FLJIT-IN          PIC X(4).                                    
017700     03 FILLER REDEFINES AREA-INPUT.                                      
017800        05 FILLER            OCCURS 14 TIMES.                             
017900           07 ATTR           PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100           07 FAELT          PIC X(4).                                    
018200     03 LINE23.                                                           
018300        05 MESSAGE-BOTTOM    PIC X(56).                                   
018400     03 TEARTNOT1-IN-ATTR    PIC X(2).                                    
018500*                                 MFS ATTRIBUTFÄLT                        
018600     03 TEARTNOT1-IN         PIC X(40).                                   
018700*                                 ARTIKEL NOTERING                        
018800     03 TEARTNOT2-IN-ATTR    PIC X(2).                                    
018900*                                 MFS ATTRIBUTFÄLT                        
019000     03 TEARTNOT2-IN         PIC X(40).                                   
019100*                                 ARTIKEL NOTERING                        
019200     03 KDERS-ATTR           PIC X(2).                                    
019300*                                 MFS BEHANDLING AV INPUTFÄLT             
019400     03 KDERS                PIC Z9.                                      
019500*                                 ERSÄTTNINGSKOD                          
019600     03 REPLACES             PIC X(9).                                    
019700*                                 ARTIKELNUMMER                           
019800     03 REPL-BY              PIC X(9).                                    
019900*                                 ARTIKELNUMMER                           
020000     03 TELPORSX-ATTR        PIC X(2).                                    
020100*                                 MFS BEHANDLING AV INPUTFÄLT             
020200     03 TELPORSX             PIC X(10).                                   
020300*                                 LEVERANSPLANEORSAK VARNING              
020400*** END OF VILMAII-COPY LENGTH= 1287 BYTES                                
