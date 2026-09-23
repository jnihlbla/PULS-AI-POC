000100 01  W91042.                                                              
000200*                                 COPY-TEXT FÖR FILEN W91042              
000300*                                 FRAMSTÄLLS EFTER NATTKÖRNING            
000400     03 FD-ARTIKEL-ROT.                                                   
000500*                                 ARTIKELUPPGIFTER FRÅN ARTREG            
000600*                                 UPPGIFTER SOM ALLTID FINNS              
000700        05 IDPTYP            PIC X(3).                                    
000800*                                 POSTTYP                                 
000900        05 IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
001200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001300        05 FLERS             PIC X.                                       
001400*                                 TILLKOMMANDE ARTIKEL ?                  
001500        05 FLIART            PIC X.                                       
001600*                                 ARTIKELN INGÅR I SATS                   
001700        05 IDAO              OCCURS 2 TIMES                               
001800                             PIC X(10).                                   
001900*                                 ÄNDRINGSORDERNUMMER                     
002000        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
002100*                                 FUNKTIONSGRUPP                          
002200        05 IDFTG             PIC 9(2).                                    
002300         88 FTG-US           VALUE 53.                                    
002400         88 FTG-CA           VALUE 54.                                    
002500         88 FTG-PV           VALUE 57.                                    
002600         88 FTG-CN           VALUE 60.                                    
002700*                                 FÖRETAGSID EKONOM REDOVISNING           
002800        05 IDLEVNR           PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003100*                                 PRODUKTSLAG                             
003200        05 KDSORT            PIC X(2).                                    
003300*                                 SORT-KOD                                
003400        05 REKSIFFR          PIC S9              COMP-3.                  
003500*                                 KONTROLLSIFFRA                          
003600        05 TIERSDAT          PIC S9(5)           COMP-3.                  
003700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003800        05 TIFINLV           PIC S9(5)           COMP-3.                  
003900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004000        05 TIREGDAT          PIC S9(7)           COMP-3.                  
004100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004200     03 FD-ARTIKEL-INFO.                                                  
004300*                                 ARTIKELUPPGIFTER                        
004400*                                 ENBART FÖR LEVANDE ARTIKLAR             
004500        05 BEFT              PIC S9(3)           COMP-3.                  
004600*                                 FÖRPACKNINGSTYP                         
004700        05 FLGEMART          PIC X.                                       
004800*                                 FLAGGA GEMENSAM ARTIKEL                 
004900        05 FLLSRDEL          PIC X.                                       
005000*                                 LEVERERAS SOM RESDEL                    
005100        05 FLSPECPR          PIC X.                                       
005200*                                 SPECIALPRISFLAGGA                       
005300        05 FLTPO1            PIC X.                                       
005400*                                 ARTIKELN GODKÄND FÖR TPO1               
005500        05 IDANSK            PIC S9(3)           COMP-3.                  
005600*                                 ANSKAFFARNUMMER                         
005700        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
005800*                                 EMBALLAGEARTIKELNR FÖR Q0               
005900        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
006000*                                 EMBALLAGEARTIKELNR FÖR Q1               
006100        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
006200*                                 EMBALLAGEARTIKELNR FÖR Q2               
006300        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
006400*                                 EMBALLAGEARTIKELNR FÖR Q3               
006500        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
006600*                                 EMBALLAGEARTIKELNR FÖR Q4               
006700        05 IDBERED           PIC S9(3)           COMP-3.                  
006800*                                 BEREDARENUMMER                          
006900        05 IDINK             PIC X(4).                                    
007000*                                 INKÖPARNUMMER                           
007100        05 IDLKTO            PIC S9(7)           COMP-3.                  
007200*                                 LAGERKONTO (FFHHHUU)                    
007300        05 FILLER            PIC X(2).                                    
007400        05 IDPROENH          OCCURS 3 TIMES                               
007500                             PIC X(8).                                    
007600*                                 PRODUKTIONSENHET                        
007700        05 IDPROJ            PIC X(4).                                    
007800*                                 PARTS PROJEKTIDENTITET                  
007900        05 IDRITN            PIC X(10).                                   
008000*                                 RITNINGSNUMMER                          
008100        05 IDSTATNR-TABELL   OCCURS 6 TIMES.                              
008200           07 IDSTATNR       PIC S9(9)           COMP-3.                  
008300*                                 STATISTISKT NUMMER                      
008400*                                 1 = NORSKT                              
008500*                                 2 = ENGELSKT                            
008600*                                 3 = BELGISKT                            
008700*                                 4 = PERUANSKT                           
008800*                                 5 = SVENSKT                             
008900*                                 6 =                                     
009000        05 KDAGE             PIC X.                                       
009100*                                 AGE-CODE                                
009200        05 KDARTHNT          PIC S9(7)           COMP-3.                  
009300*                                 HANTERINGSKOD                           
009400        05 KDARTURS          PIC X(2).                                    
009500*                                 ARTIKELURSPRUNGSKOD                     
009600        05 KDBPSR            PIC S9              COMP-3.                  
009700*                                 BASLAGERFÖRSLAGSNIVÅ                    
009800        05 KDERS             PIC S9(3)           COMP-3.                  
009900*                                 ERSÄTTNINGSKOD                          
010000        05 KDFARLIG          PIC S9              COMP-3.                  
010100*                                 KOD FÖR FARLIGT GODS                    
010200        05 KDFORP.                                                        
010300*                                 FÖRPACKNINGSKOD                         
010400           07 KDFORPPL       PIC 9.                                       
010500*                                 FÖRPACKNINGSPLATS                       
010600           07 KDFORPGP       PIC 9(2).                                    
010700*                                 FÖRPACKNINGSGRUPP                       
010800           07 KDFORPUF       PIC 9.                                       
010900*                                 UPPRÄKNINGSFAKTOR                       
011000        05 KDFREKKL          PIC X.                                       
011100*                                 FREKVENSKLASS                           
011200        05 KDGK              PIC S9              COMP-3.                  
011300*                                 GODSMOTTAGAREKOD                        
011400        05 KDHF              PIC S9              COMP-3.                  
011500*                                 HUVUDFÖRRÅDSMÄRKNING                    
011600        05 KDLTK             PIC S9              COMP-3.                  
011700*                                 LAGERTILLHÖRIGHETSKOD                   
011800        05 KDPRISKL          PIC X.                                       
011900*                                 PRISKLASS                               
012000        05 KDPRTILL          PIC S9              COMP-3.                  
012100*                                 PRISTILLÄMPNINGSKOD                     
012200        05 KDPSLLOC          PIC 9(2).                                    
012300*                                 PRODUKTSLAG LOKALT                      
012400        05 KDSRA             PIC S9(3)           COMP-3.                  
012500*                                 SRA-KOD                                 
012600        05 KDTIPPR           PIC S9              COMP-3.                  
012700*                                 TIPPAT PRIS KOD                         
012800        05 KDUART            PIC X.                                       
012900*                                 UNDANTAGSARTIKEL                        
013000        05 KDVVKL            PIC S9              COMP-3.                  
013100*                                 VOLYMVÄRDESKLASS                        
013200        05 KDVTH             PIC S9              COMP-3.                  
013300*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
013400        05 KDYTBEH           PIC S9(3)           COMP-3.                  
013500*                                 YTBEHANDLINGSKOD                        
013600        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
013700*                                 SENAST AVISERAT ANTAL                   
013800        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
013900*                                 FRYSTID FÖR TPO-ORDER                   
014000        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
014100*                                 ANTAL I Q0 FÖRPACKNING                  
014200        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
014300*                                 ANTAL I Q1 FÖRPACKNING                  
014400        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
014500*                                 ANTAL I Q2 FÖRPACKNING                  
014600        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
014700*                                 ANTAL I Q3 FÖRPACKNING                  
014800        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
014900*                                 ANTAL I Q4 FÖRPACKNING                  
015000        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
015100*                                 ARTIKELNS SJÄLVKOSTNAD                  
015200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
015300*                                 ARTIKELSTANDARDPRIS                     
015400        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
015500*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
015600        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
015700*                                 DIREKT LÖN                              
015800        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
015900*                                 DIREKT MATERIAL                         
016000        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
016100*                                 INKÖPSPRIS                              
016200        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
016300*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
016400        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
016500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
016600        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
016700*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
016800        05 TIURPROD          PIC S9(5)           COMP-3.                  
016900*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
017000        05 VKART             PIC S9(7)           COMP-3.                  
017100*                                 ARTIKELVIKT (G)                         
017200        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
017300*                                 ARTIKELVOLYM NETTO (CM3)                
017400        05 ADINLOMR-BOA      PIC X(4).                                    
017500*                                 BUFFERTOMRÅDE-ALTERNATIVT               
017600     03 FD-CDC-INFO.                                                      
017700*                                   INFO SOM GÄLLER ENBART                
017800*                                   FÖR CDC                               
017900*                                                                         
018000        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
018100*                                 LAGEROMRÅDE                             
018200        05 ADGANG            PIC S9(3)           COMP-3.                  
018300*                                 GÅNG                                    
018400        05 ADPLATS           PIC S9(5)           COMP-3.                  
018500*                                 LAGERPLATSNUMMER                        
018600        05 KDVSOP            PIC S9(3)           COMP-3.                  
018700*                                 VSOP-KOD                                
018800        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
018900*                                 SATS-PERIODBEHOV                        
019000        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
019100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
019200     03 FD-DC-INFO           OCCURS 6 TIMES.                              
019300*                                   INFO SOM GÄLLER CDC OCH SDC           
019400*                                   SAMT NDC                              
019500*                                   INDEX 1 = CDC                         
019600*                                   INDEX 2 = SDC EUROPA SUMMA            
019700*                                   INDEX 3 = NDC USA    SUMMA            
019800*                                   INDEX 4 = NDC CANADA                  
019900*                                   INDEX 5 = NDC PACIFIC                 
020000*                                   INDEX 6 = NDC AND LDC CHINA           
020100*                                                                         
020200*                                                                         
020300        05 KVAKS             PIC S9(7)           COMP-3.                  
020400*                                 ANKOMSTSALDO                            
020500        05 KVEFRS            PIC S9(7)           COMP-3.                  
020600*                                 EJ FAKTURERAT ANTAL STYCK               
020700        05 KVLS              PIC S9(7)           COMP-3.                  
020800*                                 LAGERSALDO                              
020900        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
021000*                                 SEPARAT PERIODBEHOV                     
021100        05 KVOKS             PIC S9(7)           COMP-3.                  
021200*                                 ORDERKÖSALDO                            
021300        05 KVRESS            PIC S9(7)           COMP-3.                  
021400*                                 RESERVERAT ANTAL ARTIKLAR               
021500        05 KVROS             PIC S9(7)           COMP-3.                  
021600*                                 RESTORDERSALDO                          
021700     03 FD-DIVERSE.                                                       
021800*                                   INFO SOM FÖR ARTIKEL                  
021900*                                   OBS URVALD FÖR SPEC SYFTE             
022000*                                                                         
022100        05 KDSTATUS-PR       PIC S9              COMP-3.                  
022200*                                 STATUS PÅ DETTA PRIS                    
022300*                                 0 = PRELIMINÄR  1 = DEFINITIV           
022400        05 PRARTBES-PR       PIC S9(7)V9(2)      COMP-3.                  
022500*                                 DETTA BESTÄLLNINGSPRIS (KR)             
022600        05 KDNOTTYP          PIC S9              COMP-3.                  
022700*                                 NOTERINGSTYP                            
022800        05 TEARTNOT          PIC X(40).                                   
022900*                                 ARTIKEL NOTERING                        
023000        05 TIPRLIST          PIC S9(7)           COMP-3.                  
023100*                                 PRISLISTEDATUM (AAMMDD)                 
023200     03 WDD3-INFO.                                                        
023300*                                   DATAELEMENT FRÅN WDD3                 
023400*                                                                         
023500        05 BEART-TABELL      OCCURS 10 TIMES.                             
023600           07 IDSKYLT        PIC X(3).                                    
023700*                                 NATIONALITETSTECKEN                     
023800*                                 SPRÅKIDENTIFIKATION                     
023900           07 BEART          PIC X(25).                                   
024000*                                 ARTIKELBENÄMNING                        
024100     03 WDD2-INFO.                                                        
024200*                                   DATAELEMENT FRÅN WDD2                 
024300*                                   NYPON                                 
024400        05 IDARTNR-MOTSV     PIC S9(9)           COMP-3.                  
024500*                                 MOTSVARANDE ARTIKEL                     
024600        05 TEORSAK           PIC X(50).                                   
024700*                                 INFO OM SLAG AV ÅTGÄRD                  
024800     03 FD-KAT-INFO          OCCURS 3 TIMES.                              
024900*                                   INFO SOM GÄLLER ENBART                
025000*                                   FÖR CDC , IDKAT                       
025100*                                                                         
025200        05 IDKAT             PIC X(5).                                    
025300*                                 KATALOGBETECKNING                       
025400     03 FD-PROJ-INFO.                                                     
025500*                                   INFO SOM GÄLLER PROJEKTINFO           
025600*                                   INOM PV                               
025700*                                                                         
025800        05 IDPROJUP          PIC X(8).                                    
025900*                                 PROJEKTUPPDRAG                          
026000     03 FD-FMC-INFO.                                                      
026100*                                   INFO SOM GÄLLER GEMENSAM              
026200*                                   ARTIKEL VCC/FORD                      
026300*                                                                         
026400        05 FLGEMFMC          PIC X.                                       
026500*                                 GEMENSAM FORD/MPNR ARTIKEL              
026600*** END OF VILMAII-COPY LENGTH= 873 BYTES                                 
