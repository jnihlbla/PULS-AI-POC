000100 01  NDCCN-W22418.                                                        
000200*                                 UTDRAG UR W01160(CDC)                   
000300*                                       OCH W01184(NDC)                   
000400*                                                                         
000500     03 NDCCN-IDARTNR        PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 NDCCN-IDDC           PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 NDCCN-ADART.                                                      
001200*                                 ARTIKELADRESS I LAGRET                  
001300*                                 PARTS-ADRESS                            
001400        05 NDCCN-ADLAGOMR    PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600*                                 AREA                                    
001700        05 NDCCN-ADGANG      PIC S9(3)           COMP-3.                  
001800*                                 GÅNG                                    
001900*                                 AISLE                                   
002000        05 NDCCN-ADPLATS     PIC S9(5)           COMP-3.                  
002100*                                 LAGERPLATSNUMMER                        
002200*                                 LOCATION                                
002300     03 NDCCN-ADLAGOMR-CD    PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
002500*                                 AREA ADDRESS CROSS DOCKING WARE         
002600*                                 HOUSE                                   
002700     03 NDCCN-BEFT           PIC S9(3)           COMP-3.                  
002800*                                 FÖRPACKNINGSTYP                         
002900*                                 PACKAGING TYPE                          
003000     03 NDCCN-DAPBPLAN       PIC 9(8).                                    
003100*                                 DATUM KVPB-PLAN GILTIG TOM              
003200*                                 DATE KVPB-PLAN VALID UNTIL              
003300     03 NDCCN-DAPUBL         PIC 9(8).                                    
003400*                                 PUBLICERINGSDATUM PER ART/LAND          
003500*                                 DATE OF PUBLISHING PART/COUNTRY         
003600     03 NDCCN-DASEASON       PIC 9(8).                                    
003700*                                 DATUM RESEASON-LEDTID GILTIG TO         
003800*                                 M                                       
003900*                                 DATE RESEASON-LEDTID VALID UNTI         
004000*                                 L                                       
004100     03 NDCCN-DASPSEA        PIC 9(8).                                    
004200*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
004300*                                 DATE SEASON BLOCKED TO YYYYMMDD         
004400     03 NDCCN-FLCDREL        PIC X.                                       
004500*                                 OMGÅENDE RELEASE AV CD-REFILL           
004600*                                 IMMEDIATE RELEASE OF CD-REFILL          
004700     03 NDCCN-FLFLYG         PIC X.                                       
004800*                                 FLYGARTIKEL                             
004900*                                 PART NUMBER SENT BY AIR                 
005000     03 NDCCN-FLJIT          PIC X.                                       
005100*                                 JUST-IN-TIME FLAGGA                     
005200*                                 JUST-IN-TIME FLAG                       
005300     03 NDCCN-FLORDSP        PIC X.                                       
005400*                                 ORDERSPÄRR                              
005500*                                 ORDER BLOCKED                           
005600     03 NDCCN-FLORDSP-EJRO   PIC X.                                       
005700*                                 ORDERSPÄRR EJ RESTNOTERING              
005800*                                 ORDER BLOCK NO BACKORDERING             
005900     03 NDCCN-FLREFBEO       PIC X.                                       
006000*                                 AUTOMATISK REFILL BEORDRING?            
006100*                                 AUTOMATIC REFILL ORDERING?              
006200     03 NDCCN-FLREFLARM      PIC X.                                       
006300*                                 ONORMAL ORDERINGÅNG                     
006400*                                 ABNORMAL SALES                          
006500     03 NDCCN-FLSKROT-BEORD  PIC X.                                       
006600*                                 SKROTNING BEORDRAD AV ANSK              
006700*                                 SCRAPPING ORDERED BY PROCURER           
006800     03 NDCCN-FLSPBULK       PIC X.                                       
006900*                                 FLAGGA SPÄRR MOT BULKORDER              
007000*                                 FLAG BULKORDER STOP                     
007100     03 NDCCN-FLWILSON       PIC X.                                       
007200*                                 WILSONFORMEL                            
007300*                                 FLAG TO USE WILSON OR NOT               
007400     03 NDCCN-IDANSK         PIC S9(3)           COMP-3.                  
007500*                                 ANSKAFFARNUMMER                         
007600*                                 PROCURER NO.                            
007700     03 NDCCN-IDARTNR-EMBQ0  PIC S9(9)           COMP-3.                  
007800*                                 EMBALLAGEARTIKELNR FÖR Q0               
007900     03 NDCCN-IDARTNR-EMBQ1  PIC S9(9)           COMP-3.                  
008000*                                 EMBALLAGEARTIKELNR FÖR Q1               
008100     03 NDCCN-IDARTNR-EMBQ2  PIC S9(9)           COMP-3.                  
008200*                                 EMBALLAGEARTIKELNR FÖR Q2               
008300     03 NDCCN-IDDC-REF       PIC X(2).                                    
008400*                                 SÄNDANDE LAGER FÖR REFILL               
008500*                                 SENDING WAREHOUSE FOR REFILL            
008600     03 NDCCN-IDINK          PIC X(4).                                    
008700*                                 INKÖPARNUMMER                           
008800*                                 PURCHASE IDENTIFICATION NUMBER          
008900     03 NDCCN-IDLANDX2       PIC X(2).                                    
009000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
009100*                                 2-LETTER CODE FOR COUNTRY               
009200     03 NDCCN-IDLEVNR        PIC X(5).                                    
009300*                                 LEVERANTÖRNUMMER                        
009400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009500     03 NDCCN-IDPERSON-BUY   PIC S9(3)           COMP-3.                  
009600*                                 PERSONKOD REFILLANSVARIG                
009700*                                 REFILL RESPONSIBLE ID                   
009800     03 NDCCN-IDPLANGR-AG    PIC S9              COMP-3.                  
009900*                                 PLANERINGSGRUPP ANSKAFFARE              
010000     03 NDCCN-IDPSN-DC       PIC 9(3).                                    
010100*                                 PROPER SHIPPING NAME PER DC             
010200*                                 PROPER SHIPPING NAME DC                 
010300     03 NDCCN-IDREFTAB       PIC X.                                       
010400*                                 IDENTITET REFILLTABELL                  
010500*                                 REFILLINGTABLE IDENTIFIER               
010600     03 NDCCN-IDUSER-SPKVAL  PIC X(8).                                    
010700*                                 ANVÄNDAR-ID KVALITETSPÄRR               
010800*                                 USER ID QUALITY ERROR                   
010900     03 NDCCN-KDARTURS       PIC X(2).                                    
011000*                                 ARTIKELURSPRUNGSKOD                     
011100*                                 COUNTRY OF ORIGIN                       
011200     03 NDCCN-KDAVT          PIC S9              COMP-3.                  
011300*                                 AVTALSMÄRKNING                          
011400*                                 AGREEMENT CODE                          
011500     03 NDCCN-KDLEVPLF       PIC X.                                       
011600*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
011700*                                 CODE FOR APPROVAL OF SCHEDULE P         
011800*                                 ROPOSAL                                 
011900     03 NDCCN-KDLEVSP        PIC S9(3)           COMP-3.                  
012000*                                 SPÄRRKOD LEVERANS                       
012100*                                 DELIVERY BLOCKING CODE                  
012200     03 NDCCN-KDLPSP         PIC S9              COMP-3.                  
012300*                                 LEVERANSPLANESPÄRR                      
012400     03 NDCCN-KDOPPLAN       PIC X.                                       
012500*                                 OPTIMAL PLAN INOM FRYSTID               
012600*                                 OPTIMAL PLAN WITHIN FREEZTIME           
012700     03 NDCCN-KDREFSTA       PIC X.                                       
012800*                                 STATUS REFILLARTIKEL                    
012900*                                 STATUS REFILLPART                       
013000     03 NDCCN-KVAKS-PAV      PIC S9(7)           COMP-3.                  
013100*                                 DEL AV AK PÅ VÄG                        
013200*                                 PART OF AK ON ITS WAY                   
013300     03 NDCCN-KVAKS-SDC      PIC S9(7)           COMP-3.                  
013400*                                 DEL AV AK SOM LIGGER I SDC              
013500*                                 PART OF AK IN THE SDC                   
013600     03 NDCCN-KVPBREOI       PIC S9(6)V9(1)      COMP-3.                  
013700*                                 PERIODBEHOV FÖR REFILL OI               
013800*                                 PERIOD REQUIREM. REFILLING OI           
013900     03 NDCCN-KVBEART        PIC S9(7)           COMP-3.                  
014000*                                 BESTÄLLT ANTAL STYCKEN                  
014100*                                 ORDERED QUANTITY                        
014200     03 NDCCN-KVDAGAR-CDBEH  PIC S9(3)           COMP-3.                  
014300*                                 NO. OF DAYS TO BE USED WHEN             
014400*                                 CALCULATING CD REFILLORDERS             
014500     03 NDCCN-KVDAGAR-FFH    PIC S9(3)           COMP-3.                  
014600*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
014700     03 NDCCN-KVDAGAR-INLEV  PIC S9(3)           COMP-3.                  
014800*                                 INLEVERANSTID     (ANTAL DAGAR)         
014900     03 NDCCN-KVDAGAR-MANLT  PIC S9(3)           COMP-3.                  
015000*                                 ANTAL DAGAR                             
015100     03 NDCCN-KVEFRS         PIC S9(7)           COMP-3.                  
015200*                                 EJ FAKTURERAT ANTAL STYCK               
015300*                                 ORDERED NOT INVOICED QTY                
015400     03 NDCCN-KVEOQ          PIC S9(7)           COMP-3.                  
015500*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
015600*                                 ET                                      
015700     03 NDCCN-KVINVS         PIC S9(7)           COMP-3.                  
015800*                                 INVENTERINGSSALDO                       
015900*                                 STOCK-TAKING BALANCE                    
016000     03 NDCCN-KVLS           PIC S9(7)           COMP-3.                  
016100*                                 LAGERSALDO                              
016200*                                 STOCK BALANCE                           
016300     03 NDCCN-KVOKS-BULK     PIC S9(7)           COMP-3.                  
016400*                                 ORDERKÖSALDO, KLASS 2-4                 
016500*                                 ORDER QUEUE BALANCE, CLASS 2-4          
016600     03 NDCCN-KVOKS-DAG      PIC S9(7)           COMP-3.                  
016700*                                 ORDERKÖSALDO, KLASS 1                   
016800*                                 ORDER QUEUE BALANCE, CLASS 1            
016900     03 NDCCN-KVPALL         PIC S9(7)           COMP-3.                  
017000*                                 ANTAL I PALL                            
017100*                                 QUANTITY IN PALLET                      
017200     03 NDCCN-KVPB-PLAN      PIC S9(6)V9(1)      COMP-3.                  
017300*                                 PLANERAT PERIODBEHOV                    
017400*                                 PLANNED PERIOD REQUIREMENTS             
017500     03 NDCCN-KVPB-REF       PIC S9(6)V9(1)      COMP-3.                  
017600*                                 PERIODBEHOV REFILLING                   
017700*                                 FORECAST REFILLING                      
017800     03 NDCCN-KVPB-TREND     PIC S9(6)V9(1)      COMP-3.                  
017900*                                 PERIODTRENDVÄRDE                        
018000     03 NDCCN-KVREFBER       PIC S9(7)           COMP-3.                  
018100*                                 BERÄKNAD REFILLINGKVANTITET             
018200*                                 CALCULATED REFILLING QUANTITY           
018300     03 NDCCN-KVREFOVL       PIC S9(7)           COMP-3.                  
018400*                                 BERÄKNAD ÖVERLAGERPUNKT                 
018500*                                 CALCULATED OVERSTOCK POINT              
018600     03 NDCCN-KVREFPKT       PIC S9(7)           COMP-3.                  
018700*                                 BERÄKNAD PÅFYLLNADSPUNKT                
018800*                                 CALCULATED REFILLING POINT              
018900     03 NDCCN-KVRESS         PIC S9(7)           COMP-3.                  
019000*                                 RESERVERAT ANTAL ARTIKLAR               
019100*                                 QUANTITY RESERVED ITEMS                 
019200     03 NDCCN-KVRETUR-BEORD  PIC S9(7)           COMP-3.                  
019300*                                 ANTAL SENASTE RETURORDER                
019400*                                 QUANTITY LAST RETURNORDER               
019500     03 NDCCN-KVROS-BULK     PIC S9(7)           COMP-3.                  
019600*                                 RESTORDERSALDO, KLASS 2-4               
019700*                                 BACK ORDER BALANCE, CLASS 2-4           
019800     03 NDCCN-KVROS-DAG      PIC S9(7)           COMP-3.                  
019900*                                 RESTORDERSALDO, KLASS 1                 
020000*                                 BACK ORDER BALANCE, CLASS 1             
020100     03 NDCCN-KVSKROT        PIC S9(7)           COMP-3.                  
020200*                                 ANTAL SENASTE SKROTORDER                
020300*                                 QUANTITY LAST SCRAPORDER                
020400     03 NDCCN-KVSLAGER       PIC S9(7)           COMP-3.                  
020500*                                 SÄKERHETSLAGER                          
020600*                                 SAFETY STOCK                            
020700     03 NDCCN-KVSLUTKP       PIC S9(7)           COMP-3.                  
020800*                                 SLUTKÖPSSALDO                           
020900     03 NDCCN-KVSPANT        PIC S9(7)           COMP-3.                  
021000*                                 SPÄRRAT ANTAL                           
021100*                                 BLOCKED QTY                             
021200     03 NDCCN-KVSPARR-KVAL   PIC S9(7)           COMP-3.                  
021300*                                 SPÄRRAT ANTAL KVALITETSFEL              
021400*                                 BLOCKED QUANTITY QUALITY ERROR          
021500     03 NDCCN-KVULOAD        PIC S9(7)           COMP-3.                  
021600*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
021700*                                 MIN LOAD FROM SUPPLIER                  
021800     03 NDCCN-KVUTRS         PIC S9(7)           COMP-3.                  
021900*                                 UTREDNINGSSALDO                         
022000*                                 INVESTIGATION BALANCE                   
022100     03 NDCCN-KVVECKOR-FT    PIC S9(3)           COMP-3.                  
022200*                                 ANTAL VECKOR FRYSNINGSTID               
022300     03 NDCCN-KVVECKOR-LT    PIC S9(3)           COMP-3.                  
022400*                                 ANTAL VECKOR LEDTID                     
022500     03 NDCCN-KVVECKOR-TREND PIC S9(3)           COMP-3.                  
022600*                                 ANTAL VECKOR TRENDVÄRDE                 
022700     03 NDCCN-PRARTSJK       PIC S9(7)V9(2)      COMP-3.                  
022800*                                 ARTIKELNS SJÄLVKOSTNAD                  
022900*                                 COST OF SALES                           
023000     03 NDCCN-PRAVCOST       PIC S9(7)V9(2)      COMP-3.                  
023100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
023200*                                 AVERAGE COST FOREIGN CURRENCY           
023300     03 NDCCN-RESEASON       OCCURS 12 TIMES                              
023400                             PIC S9V9(2)         COMP-3.                  
023500*                                 SÄSONGSINDEX                            
023600     03 NDCCN-RESEASON-PLAN  OCCURS 12 TIMES                              
023700                             PIC S9V9(2)         COMP-3.                  
023800*                                 SÄSONGSINDEX INKLUSIVE REFILL           
023900     03 NDCCN-RETREND        PIC S9(2)V9(1)      COMP-3.                  
024000*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
024100*                                 DIFF PREL - AVERAGE FORECAST            
024200     03 NDCCN-TEKVAL         PIC X(10).                                   
024300*                                 KVALITETSNOTERING SPÄRR                 
024400*                                 QUALITY NOTE BLOCKING                   
024500     03 NDCCN-TIAVCOST       PIC S9(7)           COMP-3.                  
024600*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
024700*                                 AVERAGE COST CALCULATION DATE           
024800     03 NDCCN-TIDATUM-TREND  PIC S9(7)           COMP-3.                  
024900*                                 JUSTERAD TREND AAMMDD                   
025000*                                 LAST TREND CHANGE  YYMMDD               
025100     03 NDCCN-TIINVDAT       PIC S9(5)           COMP-3.                  
025200*                                 INVENTERINGSDATUM                       
025300*                                 STOCKTAKING DATE                        
025400     03 NDCCN-TILEVDAG       OCCURS 5 TIMES                               
025500                             PIC S9              COMP-3.                  
025600*                                 AVSÄNDNINGSDAG INOM VECKA               
025700*                                 DELIVERY WEEK DAY                       
025800     03 NDCCN-TILPSP         PIC S9(5)           COMP-3.                  
025900*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
026000     03 NDCCN-TIMANSEA       PIC 9(6).                                    
026100*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
026200*                                 DATE FOR MANUAL SEASON (YYMMDD)         
026300     03 NDCCN-TIOMSPEC       PIC S9(5)           COMP-3.                  
026400*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
026500     03 NDCCN-TIORDREG       PIC S9(7)           COMP-3.                  
026600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
026700*                                 ORDER REGISTRATION DATE  YYMMDD         
026800     03 NDCCN-TIPBREOI       PIC 9(6).                                    
026900*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
027000*                                 DATE MAN. FC REFILL OI (YYMMDD)         
027100     03 NDCCN-TIREFMPB       PIC S9(7)           COMP-3.                  
027200*                                 DATUM MANUELL PROGNOS REFILLING         
027300*                                 DATE MANUAL FORECAST REFILLING          
027400     03 NDCCN-TIREFPAF       PIC S9(7)           COMP-3.                  
027500*                                 DATUM MANUELL PÅFYLLNADSKVANT           
027600*                                 DATE MANUAL REFILLING QTY               
027700     03 NDCCN-TIREFPKT       PIC S9(7)           COMP-3.                  
027800*                                 DATUM MANUELL REFILLPUNKT               
027900*                                 DATE MANUAL REFILLING POINT             
028000     03 NDCCN-TIREFSTA       PIC S9(7)           COMP-3.                  
028100*                                 DATUM AKT/PASS REFILLARTIKEL            
028200*                                 DATE ACT/PASS REFILLPART                
028300     03 NDCCN-TIREFSTO       PIC S9(7)           COMP-3.                  
028400*                                 BEORDRINGSSTOPPAD T.OM.                 
028500*                                 STOPPED FOR ORDERING UNTIL              
028600     03 NDCCN-TIREFSTO-LOC   PIC S9(7)           COMP-3.                  
028700*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
028800*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
028900     03 NDCCN-TIRETUR-BEORD  PIC S9(7)           COMP-3.                  
029000*                                 DATUM RETUR BEORDRING                   
029100*                                 DATE ISSUE OF RETURN ORDER              
029200     03 NDCCN-TISKROT        PIC S9(7)           COMP-3.                  
029300*                                 SKROTNINGSDATUM                         
029400*                                 DATE OF SCRAPPING                       
029500     03 NDCCN-TISKROT-BEORD  PIC S9(7)           COMP-3.                  
029600*                                 BEORDRAD SKROTNINGSDATUM                
029700*                                 DATE OF SCRAPPING DECISION              
029800     03 NDCCN-TISLUTKP       PIC S9(7)           COMP-3.                  
029900*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
030000*                                 CALC OF ATR-BAL IS TO COMMENCE          
030100     03 NDCCN-TISPARR-KVAL   PIC 9(6).                                    
030200*                                 SPÄRRAD DATUM KVALITETSFEL              
030300*                                 BLOCKED DATE QUALITY ERROR              
030400     03 NDCCN-VKART          PIC S9(7)           COMP-3.                  
030500*                                 ARTIKELVIKT (G)                         
030600*                                 PART WEIGHT (G)                         
030700     03 NDCCN-VLARTNTO       PIC S9(8)V9(1)      COMP-3.                  
030800*                                 ARTIKELVOLYM NETTO (CM3)                
030900*                                 PART NET VOLUME    (CM3)                
031000     03 NDCCN-IDLEVNR-SHIP   PIC X(5).                                    
031100*                                 SKEPPANDE LEVERANTÖR                    
031200*                                 SHIPPING SUPPLIER                       
031300     03 NDCCN-PRMATRL        PIC S9(7)V9(2)      COMP-3.                  
031400*                                 FAST PRIS UNDER LÖPANDE ÅR              
031500*                                 MATERIAL PRICE FOR ACTUAL YEAR          
031600     03 NDCCN-TIERSDAT-VIPS  PIC S9(5)           COMP-3.                  
031700*                                 DATUM NÄR ERS. INFO TILL VIPS           
031800*                                 SEND DATE OF SUPERS. TO VIPS            
031900     03 NDCCN-TIMANSEC       PIC S9(7)           COMP-3.                  
032000*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
032100*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
032200     03 NDCCN-KDERS          PIC S9(3)           COMP-3.                  
032300*                                 ERSÄTTNINGSKOD                          
032400*                                 SUPERSESSION CODE                       
032500     03 NDCCN-KDPRODSL       PIC S9(3)           COMP-3.                  
032600*                                 PRODUKTSLAG                             
032700*                                 PRODUCT GROUP                           
032800     03 NDCCN-TIFINLV        PIC S9(5)           COMP-3.                  
032900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
033000*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
033100     03 NDCCN-KDERS-UTG      PIC S9(3)           COMP-3.                  
033200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
033300*                                 OBSOLETION SUPERSESSION CODE            
033400     03 NDCCN-KVTILLG-TOT    PIC S9(7)           COMP-3.                  
033500*                                 LAGERTILLGÅNG CDC TOTALT                
033600     03 NDCCN-REDIRLEV       PIC S9V9(2)         COMP-3.                  
033700*                                 DIREKTLEVERANSANDEL                     
033800     03 NDCCN-TISTODAT-LARM  PIC S9(7)           COMP-3.                  
033900*                                 STOPPDATUM FÖR LARM-223                 
034000*                                 STOP DATE FOR ALARM-223                 
034100*** END OF VILMAII-COPY LENGTH= 439 BYTES                                 
