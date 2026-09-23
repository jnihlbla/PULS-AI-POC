000100 01  W21710-SORT.                                                         
000200*                                 COPY-TEXT FÖR SORT I W21710             
000300     03 IDLEVNR              PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 IDLEVNR-SHIP         PIC X(5).                                    
000600*                                 SKEPPANDE LEVERANTÖR                    
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 BELEV                PIC X(35).                                   
001000*                                 LEVERANTÖRSNAMN                         
001100     03 BEART-SVE            PIC X(25).                                   
001200*                                 SVENSK ARTIKELBENÄMNING                 
001300     03 BEART-ENG            PIC X(25).                                   
001400*                                 ENGELSK ARTIKELBENÄMNING                
001500     03 KVLS-CDC             PIC S9(7)           COMP-3.                  
001600*                                 LAGERSALDO                              
001700     03 KVLS-SDC-LDC         PIC S9(7)           COMP-3.                  
001800*                                 LAGERSALDO                              
001900     03 KVLS-NDC             PIC S9(7)           COMP-3.                  
002000*                                 LAGERSALDO                              
002100     03 KVRESS-CDC           PIC S9(7)           COMP-3.                  
002200*                                 RESERVERAT ANTAL ARTIKLAR               
002300     03 KVRESS-SDC-LDC       PIC S9(7)           COMP-3.                  
002400*                                 RESERVERAT ANTAL ARTIKLAR               
002500     03 KVRESS-NDC           PIC S9(7)           COMP-3.                  
002600*                                 RESERVERAT ANTAL ARTIKLAR               
002700     03 KVAKS                PIC S9(7)           COMP-3.                  
002800*                                 ANKOMSTSALDO                            
002900     03 KVSLAGER             PIC S9(7)           COMP-3.                  
003000*                                 SÄKERHETSLAGER                          
003100     03 RESLJUST             PIC S9(2)V9(1)      COMP-3.                  
003200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
003300     03 TISLJUST             PIC S9(5)           COMP-3.                  
003400*                                 VECKA DÅ JUSTERING AV SÄKER-            
003500*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
003600     03 KVMP                 PIC S9(7)           COMP-3.                  
003700*                                 MAXPUNKT                                
003800     03 KVROS                PIC S9(7)           COMP-3.                  
003900*                                 RESTORDERSALDO                          
004000     03 KVRORAD              PIC S9(5)           COMP-3.                  
004100*                                 ANTAL RESTORDER-RADER                   
004200     03 KVVORKO              PIC S9(7)           COMP-3.                  
004300*                                 VOR-KÖ KVANT                            
004400     03 KVSLAP-SUM           PIC S9(7)           COMP-3.                  
004500*                                 AVROPSKVANTITET                         
004600     03 KVAVIS-NOT-REC       PIC S9(7)           COMP-3.                  
004700*                                 AVISERAT ANTAL                          
004800     03 TILEVBSK-AVS         PIC S9(7)           COMP-3.                  
004900*                                 LEVERANSBESKED  (ÅÅMMDD)                
005000     03 KVAVIS-BSKKVAR       PIC S9(7)           COMP-3.                  
005100*                                 AVISERAT ANTAL                          
005200     03 TILEVBSK-INL-C1      PIC S9(7)           COMP-3.                  
005300*                                 LEVERANSBESKED  (ÅÅMMDD)                
005400     03 TILEVBSK-DISP-C1     PIC S9(7)           COMP-3.                  
005500*                                 LEVERANSBESKED  (ÅÅMMDD)                
005600     03 TIBORT-INFO          PIC S9(7)           COMP-3.                  
005700*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
005800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
005900*                                 PRODUKTSLAG                             
006000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006100*                                 FUNKTIONSGRUPP                          
006200     03 FLFORP-SI            PIC X.                                       
006300*                                 ALLMÄN FLAGGA                           
006400     03 KVAVIS-FORAVIS       PIC S9(7)           COMP-3.                  
006500*                                 AVISERAT ANTAL                          
006600     03 SENASTEINLEV         OCCURS 5 TIMES.                              
006700        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
006800*                                 AVISERINGSDATUM (YYMMDD)                
006900        05 IDFS-SEN          PIC X(8).                                    
007000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007100        05 KVANTAL-SEN       PIC S9(7)           COMP-3.                  
007200*                                 ANTAL                                   
007300     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
007400*                                 SEPARAT PERIODBEHOV                     
007500     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
007600*                                 TOTALT PERIODBEHOV                      
007700     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
007800*                                 SATS-PERIODBEHOV                        
007900     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
008000*                                 PLANERAT PERIODBEHOV                    
008100     03 DAPBPLAN             PIC 9(8).                                    
008200*                                 DATUM KVPB-PLAN GILTIG TOM              
008300     03 KVOI-12-RULL         PIC S9(7)           COMP-3.                  
008400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
008500     03 KVOI-YEAR-0          PIC S9(7)           COMP-3.                  
008600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
008700     03 KVOI-YEAR-1          PIC S9(7)           COMP-3.                  
008800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
008900     03 KVOI-YEAR-2          PIC S9(7)           COMP-3.                  
009000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009100     03 KVOI-YEAR-3          PIC S9(7)           COMP-3.                  
009200*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009300     03 KVOI-YEAR-4          PIC S9(7)           COMP-3.                  
009400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009500     03 KVOI-YEAR-5          PIC S9(7)           COMP-3.                  
009600*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009700     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
009800*                                 ANTAL VECKOR LEDTID                     
009900     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
010000*                                 BESTÄLLNINGSPRIS I KRONOR               
010100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
010200*                                 ARTIKELSTANDARDPRIS                     
010300     03 KVBR-VALID-LEV       PIC S9(7)           COMP-3.                  
010400*                                 BESTÄLLNINGSREST                        
010500     03 KVBR-OVR-LEV         PIC S9(7)           COMP-3.                  
010600*                                 BESTÄLLNINGSREST                        
010700     03 KDAVT                PIC S9              COMP-3.                  
010800*                                 AVTALSMÄRKNING                          
010900     03 KDLEVPLF             PIC X.                                       
011000*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
011100     03 FLJIT                PIC X.                                       
011200*                                 JUST-IN-TIME FLAGGA                     
011300     03 IDPROJ               PIC X(4).                                    
011400*                                 PARTS PROJEKTIDENTITET                  
011500     03 IDKAT                OCCURS 3 TIMES                               
011600                             PIC X(5).                                    
011700*                                 KATALOGBETECKNING                       
011800     03 TIFINLV              PIC S9(5)           COMP-3.                  
011900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
012000     03 TIURPROD             PIC S9(5)           COMP-3.                  
012100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
012200     03 DASPSEA              PIC 9(8).                                    
012300*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
012400     03 SEASON-ARTIKEL       PIC X.                                       
012500*                                 ALLMÄN FLAGGA                           
012600     03 OSAKERHET            PIC 9(3)V9(1).                               
012700     03 TIREFSTO             PIC S9(7)           COMP-3.                  
012800*                                 BEORDRINGSSTOPPAD T.OM.                 
012900     03 FLIART               PIC X.                                       
013000*                                 ARTIKELN INGÅR I SATS                   
013100     03 KVPB-SUM-VV          PIC S9(8)V9(1)      COMP-3.                  
013200     03 KVAVROP-SUM-VV       PIC S9(8)           COMP-3.                  
013300     03 KDVVKL               PIC S9              COMP-3.                  
013400*                                 VOLYMVÄRDESKLASS                        
013500     03 KVMAD-SEP            PIC S9(6)V9(1)      COMP-3.                  
013600*                                 SEPARAT PROGNOSFEL                      
013700     03 KDPRISKL             PIC X.                                       
013800*                                 PRISKLASS                               
013900     03 KDFREKKL             PIC X.                                       
014000*                                 FREKVENSKLASS                           
014100     03 KVAVBRAD             PIC S9(7)V9(2)      COMP-3.                  
014200*                                 AVBOKADE RADER                          
014300     03 KVINORD              PIC S9(7)           COMP-3.                  
014400*                                 ANTAL INKOMNA ORDERRADER                
014500     03 SERVG                PIC S9(7)V9(2)      COMP-3.                  
014600     03 KDSORT               PIC X(2).                                    
014700*                                 SORT-KOD                                
014800     03 KVEOQ                PIC S9(7)           COMP-3.                  
014900*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
015000*                                 ET                                      
015100     03 KVPALL               PIC S9(7)           COMP-3.                  
015200*                                 ANTAL I PALL                            
015300     03 KVQ                  PIC S9(7)           COMP-3.                  
015400*                                 EKONOMISK HEMTAGNINGSKVANTITET          
015500     03 FLMANQ               PIC X.                                       
015600*                                 MANUELL HEMTAGNINGSKVANTITET            
015700     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
015800*                                 ANTAL I Q0 FÖRPACKNING                  
015900     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
016000*                                 ANTAL I Q1 FÖRPACKNING                  
016100     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
016200*                                 ANTAL I Q2 FÖRPACKNING                  
016300     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
016400*                                 ANTAL I Q3 FÖRPACKNING                  
016500     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
016600*                                 ANTAL I Q4 FÖRPACKNING                  
016700     03 KVULOAD              PIC S9(7)           COMP-3.                  
016800*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
016900     03 KVSPARR-KVAL-CDC     PIC S9(7)           COMP-3.                  
017000*                                 SPÄRRAT ANTAL KVALITETSFEL              
017100     03 FLNYBER              PIC X.                                       
017200*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
017300     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
017400*                                 EMBALLAGEARTIKELNR FÖR Q0               
017500     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
017600*                                 EMBALLAGEARTIKELNR FÖR Q1               
017700     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
017800*                                 EMBALLAGEARTIKELNR FÖR Q2               
017900     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
018000*                                 EMBALLAGEARTIKELNR FÖR Q3               
018100     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
018200*                                 EMBALLAGEARTIKELNR FÖR Q4               
018300     03 BEFT                 PIC S9(3)           COMP-3.                  
018400*                                 FÖRPACKNINGSTYP                         
018500     03 PRDIRLON             PIC S9(4)V9(3)      COMP-3.                  
018600*                                 DIREKT LÖN                              
018700     03 PRDMTRL              PIC S9(6)V9(3)      COMP-3.                  
018800*                                 DIREKT MATERIAL                         
018900     03 KDFORP               PIC S9(5)           COMP-3.                  
019000*                                 FÖRPACKNINGSKOD                         
019100     03 KVSPANT              PIC S9(7)           COMP-3.                  
019200*                                 SPÄRRAT ANTAL                           
019300     03 KDKRSTA              PIC X.                                       
019400*                                 KONTROLLRAPPORT STATUS                  
019500     03 IDKR                 PIC 9(5).                                    
019600*                                 KONTROLLRAPPORT NUMMER                  
019700     03 KVBUFF-KARANTN       PIC S9(7)           COMP-3.                  
019800*                                 FÖRÄDLAT BUFFERSALDO                    
019900     03 KDERS                PIC S9(3)           COMP-3.                  
020000*                                 ERSÄTTNINGSKOD                          
020100     03 IDKAMP               PIC X(7).                                    
020200*                                 SERVICEKAMPANJ                          
020300     03 IDKAMP-GRP           PIC X(7).                                    
020400*                                 ID FÖR KAMPANJGRUPPER                   
020500     03 TISTADAT-KAMP        PIC S9(7)           COMP-3.                  
020600*                                 STARTDATUM FÖR KAMPANJ                  
020700     03 TISTODAT-KAMP        PIC S9(7)           COMP-3.                  
020800*                                 STOPPDATUM FÖR KAMPANJ                  
020900     03 KDKAMP               PIC X.                                       
021000*                                                                         
021100     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
021200*                                 LAGEROMRÅDE                             
021300     03 ADGANG               PIC S9(3)           COMP-3.                  
021400*                                 GÅNG                                    
021500     03 ADLAGOMR-SVS         PIC S9(3)           COMP-3.                  
021600*                                 LAGEROMRÅDE                             
021700     03 ADGANG-SVS           PIC S9(3)           COMP-3.                  
021800*                                 GÅNG                                    
021900     03 ADLAGOMR-VOHT        PIC S9(3)           COMP-3.                  
022000*                                 LAGEROMRÅDE                             
022100     03 ADGANG-VOHT          PIC S9(3)           COMP-3.                  
022200*                                 GÅNG                                    
022300     03 ADLAGOMR-CD          PIC S9(3)           COMP-3.                  
022400*                                 LAGEROMRÅDE                             
022500     03 ADGANG-CD            PIC S9(3)           COMP-3.                  
022600*                                 GÅNG                                    
022700     03 ADPLATS              PIC S9(5)           COMP-3.                  
022800*                                 LAGERPLATSNUMMER                        
022900     03 ADINPORT             PIC X(8).                                    
023000*                                 AVLASTNINGSPORT                         
023100     03 IDINK                PIC X(4).                                    
023200*                                 INKÖPARNUMMER                           
023300     03 IDANSK               PIC S9(3)           COMP-3.                  
023400*                                 ANSKAFFARNUMMER                         
023500     03 IDBERED              PIC S9(3)           COMP-3.                  
023600*                                 BEREDARENUMMER                          
023700     03 KDUART               PIC X.                                       
023800*                                 UNDANTAGSARTIKEL                        
023900     03 VKART                PIC S9(7)           COMP-3.                  
024000*                                 ARTIKELVIKT (G)                         
024100     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
024200*                                 ARTIKELVOLYM (CM3)                      
024300     03 KDARTURS             PIC X(2).                                    
024400*                                 ARTIKELURSPRUNGSKOD                     
024500     03 KVPB-TREND           PIC S9(6)V9(1)      COMP-3.                  
024600*                                 PERIODTRENDVÄRDE                        
024700     03 KVVECKOR-TREND       PIC S9(3)           COMP-3.                  
024800*                                 ANTAL VECKOR TRENDVÄRDE                 
024900     03 TIDATUM-TREND        PIC S9(7)           COMP-3.                  
025000*                                 JUSTERAD TREND AAMMDD                   
025100     03 TILEVDAG             OCCURS 5 TIMES                               
025200                             PIC S9              COMP-3.                  
025300*                                 AVSÄNDNINGSDAG INOM VECKA               
025400     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
025500*                                 DIREKTLEVERANSANDEL                     
025600     03 TITPO                PIC S9(7)           COMP-3.                  
025700*                                 PLANERAD ORDERDATUM                     
025800     03 KVART                PIC S9(7)           COMP-3.                  
025900*                                 ANTAL ARTNR PER BRYTBEGREPP             
026000     03 FLLSRDEL             PIC X.                                       
026100*                                 LEVERERAS SOM RESDEL                    
026200     03 KDOTFREK             PIC X.                                       
026300*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
026400*** END OF VILMAII-COPY LENGTH= 610 BYTES                                 
