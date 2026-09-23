000100 01  SLAG-W01184.                                                         
000200*                                 UTDRAG UR WDK711,K712 OCH K722          
000300*                                                                         
000400     03 SLAG-IDARTNR         PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 SLAG-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 SLAG-ADART.                                                       
001100*                                 ARTIKELADRESS I LAGRET                  
001200*                                 PARTS-ADRESS                            
001300        05 SLAG-ADLAGOMR     PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600        05 SLAG-ADGANG       PIC S9(3)           COMP-3.                  
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900        05 SLAG-ADPLATS      PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 SLAG-ADLAGOMR-CD     PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
002400*                                 AREA ADDRESS CROSS DOCKING WARE         
002500*                                 HOUSE                                   
002600     03 SLAG-BEFT            PIC S9(3)           COMP-3.                  
002700*                                 FÖRPACKNINGSTYP                         
002800*                                 PACKAGING TYPE                          
002900     03 SLAG-DAPBPLAN        PIC 9(8).                                    
003000*                                 DATUM KVPB-PLAN GILTIG TOM              
003100*                                 DATE KVPB-PLAN VALID UNTIL              
003200     03 SLAG-DAPUBL          PIC 9(8).                                    
003300*                                 PUBLICERINGSDATUM PER ART/LAND          
003400*                                 DATE OF PUBLISHING PART/COUNTRY         
003500     03 SLAG-DASEASON        PIC 9(8).                                    
003600*                                 DATUM RESEASON-LEDTID GILTIG TO         
003700*                                 M                                       
003800*                                 DATE RESEASON-LEDTID VALID UNTI         
003900*                                 L                                       
004000     03 SLAG-DASPSEA         PIC 9(8).                                    
004100*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
004200*                                 DATE SEASON BLOCKED TO YYYYMMDD         
004300     03 SLAG-FLCDREL         PIC X.                                       
004400*                                 OMGÅENDE RELEASE AV CD-REFILL           
004500*                                 IMMEDIATE RELEASE OF CD-REFILL          
004600     03 SLAG-FLFLYG          PIC X.                                       
004700*                                 FLYGARTIKEL                             
004800*                                 PART NUMBER SENT BY AIR                 
004900     03 SLAG-FLJIT           PIC X.                                       
005000*                                 JUST-IN-TIME FLAGGA                     
005100*                                 JUST-IN-TIME FLAG                       
005200     03 SLAG-FLORDSP         PIC X.                                       
005300*                                 ORDERSPÄRR                              
005400*                                 ORDER BLOCKED                           
005500     03 SLAG-FLORDSP-EJRO    PIC X.                                       
005600*                                 ORDERSPÄRR EJ RESTNOTERING              
005700*                                 ORDER BLOCK NO BACKORDERING             
005800     03 SLAG-FLREFBEO        PIC X.                                       
005900*                                 AUTOMATISK REFILL BEORDRING?            
006000*                                 AUTOMATIC REFILL ORDERING?              
006100     03 SLAG-FLREFLARM       PIC X.                                       
006200*                                 ONORMAL ORDERINGÅNG                     
006300*                                 ABNORMAL SALES                          
006400     03 SLAG-FLSKROT-BEORD   PIC X.                                       
006500*                                 SKROTNING BEORDRAD AV ANSK              
006600*                                 SCRAPPING ORDERED BY PROCURER           
006700     03 SLAG-FLSPBULK        PIC X.                                       
006800*                                 FLAGGA SPÄRR MOT BULKORDER              
006900*                                 FLAG BULKORDER STOP                     
007000     03 SLAG-FLWILSON        PIC X.                                       
007100*                                 WILSONFORMEL                            
007200*                                 FLAG TO USE WILSON OR NOT               
007300     03 SLAG-IDANSK          PIC S9(3)           COMP-3.                  
007400*                                 ANSKAFFARNUMMER                         
007500*                                 PROCURER NO.                            
007600     03 SLAG-IDARTNR-EMBQ0   PIC S9(9)           COMP-3.                  
007700*                                 EMBALLAGEARTIKELNR FÖR Q0               
007800     03 SLAG-IDARTNR-EMBQ1   PIC S9(9)           COMP-3.                  
007900*                                 EMBALLAGEARTIKELNR FÖR Q1               
008000     03 SLAG-IDARTNR-EMBQ2   PIC S9(9)           COMP-3.                  
008100*                                 EMBALLAGEARTIKELNR FÖR Q2               
008200     03 SLAG-IDDC-REF        PIC X(2).                                    
008300*                                 SÄNDANDE LAGER FÖR REFILL               
008400*                                 SENDING WAREHOUSE FOR REFILL            
008500     03 SLAG-IDINK           PIC X(4).                                    
008600*                                 INKÖPARNUMMER                           
008700*                                 PURCHASE IDENTIFICATION NUMBER          
008800     03 SLAG-IDLANDX2        PIC X(2).                                    
008900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
009000*                                 2-LETTER CODE FOR COUNTRY               
009100     03 SLAG-IDLEVNR         PIC X(5).                                    
009200*                                 LEVERANTÖRNUMMER                        
009300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009400     03 SLAG-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
009500*                                 PERSONKOD REFILLANSVARIG                
009600*                                 REFILL RESPONSIBLE ID                   
009700     03 SLAG-IDPLANGR-AG     PIC S9              COMP-3.                  
009800*                                 PLANERINGSGRUPP ANSKAFFARE              
009900     03 SLAG-IDPSN-DC        PIC 9(3).                                    
010000*                                 PROPER SHIPPING NAME PER XDC            
010100*                                 PROPER SHIPPING NAME XDC                
010200     03 SLAG-IDREFTAB        PIC X.                                       
010300*                                 IDENTITET REFILLTABELL                  
010400*                                 REFILLINGTABLE IDENTIFIER               
010500     03 SLAG-IDUSER-SPKVAL   PIC X(8).                                    
010600*                                 ANVÄNDAR-ID KVALITETSPÄRR               
010700*                                 USER ID QUALITY ERROR                   
010800     03 SLAG-KDARTURS        PIC X(2).                                    
010900*                                 ARTIKELURSPRUNGSKOD                     
011000*                                 COUNTRY OF ORIGIN                       
011100     03 SLAG-KDAVT           PIC S9              COMP-3.                  
011200*                                 AVTALSMÄRKNING                          
011300*                                 AGREEMENT CODE                          
011400     03 SLAG-KDLEVPLF        PIC X.                                       
011500*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
011600*                                 CODE FOR APPROVAL OF SCHEDULE P         
011700*                                 ROPOSAL                                 
011800     03 SLAG-KDLEVSP         PIC S9(3)           COMP-3.                  
011900*                                 SPÄRRKOD LEVERANS                       
012000*                                 DELIVERY BLOCKING CODE                  
012100     03 SLAG-KDLPSP          PIC S9              COMP-3.                  
012200*                                 LEVERANSPLANESPÄRR                      
012300     03 SLAG-KDOPPLAN        PIC X.                                       
012400*                                 OPTIMAL PLAN INOM FRYSTID               
012500*                                 OPTIMAL PLAN WITHIN FREEZTIME           
012600     03 SLAG-KDREFSTA        PIC X.                                       
012700*                                 STATUS REFILLARTIKEL                    
012800*                                 STATUS REFILLPART                       
012900     03 SLAG-KVAKS-PAV       PIC S9(7)           COMP-3.                  
013000*                                 DEL AV AK PÅ VÄG                        
013100*                                 PART OF AK ON ITS WAY                   
013200     03 SLAG-KVAKS-SDC       PIC S9(7)           COMP-3.                  
013300*                                 DEL AV AK SOM LIGGER I SDC              
013400*                                 PART OF AK IN THE SDC                   
013500     03 SLAG-KVPBREOI        PIC S9(6)V9(1)      COMP-3.                  
013600*                                 PERIODBEHOV FÖR REFILL OI               
013700*                                 PERIOD REQUIREM. REFILLING OI           
013800     03 SLAG-KVBEART         PIC S9(7)           COMP-3.                  
013900*                                 BESTÄLLT ANTAL STYCKEN                  
014000*                                 ORDERED QUANTITY                        
014100     03 SLAG-KVDAGAR-CDBEH   PIC S9(3)           COMP-3.                  
014200*                                 NO. OF DAYS TO BE USED WHEN             
014300*                                 CALCULATING CD REFILLORDERS             
014400     03 SLAG-KVDAGAR-FFH     PIC S9(3)           COMP-3.                  
014500*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
014600     03 SLAG-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
014700*                                 INLEVERANSTID     (ANTAL DAGAR)         
014800     03 SLAG-KVDAGAR-MANLT   PIC S9(3)           COMP-3.                  
014900*                                 ANTAL DAGAR                             
015000     03 SLAG-KVEFRS          PIC S9(7)           COMP-3.                  
015100*                                 EJ FAKTURERAT ANTAL STYCK               
015200*                                 ORDERED NOT INVOICED QTY                
015300     03 SLAG-KVEOQ           PIC S9(7)           COMP-3.                  
015400*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
015500*                                 ET                                      
015600     03 SLAG-KVINVS          PIC S9(7)           COMP-3.                  
015700*                                 INVENTERINGSSALDO                       
015800*                                 STOCK-TAKING BALANCE                    
015900     03 SLAG-KVLS            PIC S9(7)           COMP-3.                  
016000*                                 LAGERSALDO                              
016100*                                 STOCK BALANCE                           
016200     03 SLAG-KVOKS-BULK      PIC S9(7)           COMP-3.                  
016300*                                 ORDERKÖSALDO, KLASS 2-4                 
016400*                                 ORDER QUEUE BALANCE, CLASS 2-4          
016500     03 SLAG-KVOKS-DAG       PIC S9(7)           COMP-3.                  
016600*                                 ORDERKÖSALDO, KLASS 1                   
016700*                                 ORDER QUEUE BALANCE, CLASS 1            
016800     03 SLAG-KVPALL          PIC S9(7)           COMP-3.                  
016900*                                 ANTAL I PALL                            
017000*                                 QUANTITY IN PALLET                      
017100     03 SLAG-KVPB-PLAN       PIC S9(6)V9(1)      COMP-3.                  
017200*                                 PLANERAT PERIODBEHOV                    
017300*                                 PLANNED PERIOD REQUIREMENTS             
017400     03 SLAG-KVPB-REF        PIC S9(6)V9(1)      COMP-3.                  
017500*                                 PERIODBEHOV REFILLING                   
017600*                                 FORECAST REFILLING                      
017700     03 SLAG-KVPB-TREND      PIC S9(6)V9(1)      COMP-3.                  
017800*                                 PERIODTRENDVÄRDE                        
017900     03 SLAG-KVREFBER        PIC S9(7)           COMP-3.                  
018000*                                 BERÄKNAD REFILLINGKVANTITET             
018100*                                 CALCULATED REFILLING QUANTITY           
018200     03 SLAG-KVREFOVL        PIC S9(7)           COMP-3.                  
018300*                                 BERÄKNAD ÖVERLAGERPUNKT                 
018400*                                 CALCULATED OVERSTOCK POINT              
018500     03 SLAG-KVREFPKT        PIC S9(7)           COMP-3.                  
018600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
018700*                                 CALCULATED REFILLING POINT              
018800     03 SLAG-KVRESS          PIC S9(7)           COMP-3.                  
018900*                                 RESERVERAT ANTAL ARTIKLAR               
019000*                                 QUANTITY RESERVED ITEMS                 
019100     03 SLAG-KVRETUR-BEORD   PIC S9(7)           COMP-3.                  
019200*                                 ANTAL SENASTE RETURORDER                
019300*                                 QUANTITY LAST RETURNORDER               
019400     03 SLAG-KVROS-BULK      PIC S9(7)           COMP-3.                  
019500*                                 RESTORDERSALDO, KLASS 2-4               
019600*                                 BACK ORDER BALANCE, CLASS 2-4           
019700     03 SLAG-KVROS-DAG       PIC S9(7)           COMP-3.                  
019800*                                 RESTORDERSALDO, KLASS 1                 
019900*                                 BACK ORDER BALANCE, CLASS 1             
020000     03 SLAG-KVSKROT         PIC S9(7)           COMP-3.                  
020100*                                 ANTAL SENASTE SKROTORDER                
020200*                                 QUANTITY LAST SCRAPORDER                
020300     03 SLAG-KVSLAGER        PIC S9(7)           COMP-3.                  
020400*                                 SÄKERHETSLAGER                          
020500*                                 SAFETY STOCK                            
020600     03 SLAG-KVSLUTKP        PIC S9(7)           COMP-3.                  
020700*                                 SLUTKÖPSSALDO                           
020800     03 SLAG-KVSPANT         PIC S9(7)           COMP-3.                  
020900*                                 SPÄRRAT ANTAL                           
021000*                                 BLOCKED QTY                             
021100     03 SLAG-KVSPARR-KVAL    PIC S9(7)           COMP-3.                  
021200*                                 SPÄRRAT ANTAL KVALITETSFEL              
021300*                                 BLOCKED QUANTITY QUALITY ERROR          
021400     03 SLAG-KVULOAD         PIC S9(7)           COMP-3.                  
021500*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
021600*                                 MIN LOAD FROM SUPPLIER                  
021700     03 SLAG-KVUTRS          PIC S9(7)           COMP-3.                  
021800*                                 UTREDNINGSSALDO                         
021900*                                 INVESTIGATION BALANCE                   
022000     03 SLAG-KVVECKOR-FT     PIC S9(3)           COMP-3.                  
022100*                                 ANTAL VECKOR FRYSNINGSTID               
022200     03 SLAG-KVVECKOR-LT     PIC S9(3)           COMP-3.                  
022300*                                 ANTAL VECKOR LEDTID                     
022400     03 SLAG-KVVECKOR-TREND  PIC S9(3)           COMP-3.                  
022500*                                 ANTAL VECKOR TRENDVÄRDE                 
022600     03 SLAG-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
022700*                                 ARTIKELNS SJÄLVKOSTNAD                  
022800*                                 COST OF SALES                           
022900     03 SLAG-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
023000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
023100*                                 AVERAGE COST FOREIGN CURRENCY           
023200     03 SLAG-RESEASON        OCCURS 12 TIMES                              
023300                             PIC S9V9(2)         COMP-3.                  
023400*                                 SÄSONGSINDEX                            
023500     03 SLAG-RESEASON-PLAN   OCCURS 12 TIMES                              
023600                             PIC S9V9(2)         COMP-3.                  
023700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
023800     03 SLAG-RETREND         PIC S9(2)V9(1)      COMP-3.                  
023900*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
024000*                                 DIFF PREL - AVERAGE FORECAST            
024100     03 SLAG-TEKVAL          PIC X(10).                                   
024200*                                 KVALITETSNOTERING SPÄRR                 
024300*                                 QUALITY NOTE BLOCKING                   
024400     03 SLAG-TIAVCOST        PIC S9(7)           COMP-3.                  
024500*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
024600*                                 AVERAGE COST CALCULATION DATE           
024700     03 SLAG-TIDATUM-TREND   PIC S9(7)           COMP-3.                  
024800*                                 JUSTERAD TREND AAMMDD                   
024900*                                 LAST TREND CHANGE  YYMMDD               
025000     03 SLAG-TIINVDAT        PIC S9(5)           COMP-3.                  
025100*                                 INVENTERINGSDATUM                       
025200*                                 STOCKTAKING DATE                        
025300     03 SLAG-TILEVDAG        OCCURS 5 TIMES                               
025400                             PIC S9              COMP-3.                  
025500*                                 AVSÄNDNINGSDAG INOM VECKA               
025600*                                 DELIVERY WEEK DAY                       
025700     03 SLAG-TILPSP          PIC S9(5)           COMP-3.                  
025800*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
025900     03 SLAG-TIMANSEA        PIC 9(6).                                    
026000*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
026100*                                 DATE FOR MANUAL SEASON (YYMMDD)         
026200     03 SLAG-TIOMSPEC        PIC S9(5)           COMP-3.                  
026300*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
026400     03 SLAG-TIORDREG        PIC S9(7)           COMP-3.                  
026500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
026600*                                 ORDER REGISTRATION DATE  YYMMDD         
026700     03 SLAG-TIPBREOI        PIC 9(6).                                    
026800*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
026900*                                 DATE MAN. FC REFILL OI (YYMMDD)         
027000     03 SLAG-TIREFMPB        PIC S9(7)           COMP-3.                  
027100*                                 DATUM MANUELL PROGNOS REFILLING         
027200*                                 DATE MANUAL FORECAST REFILLING          
027300     03 SLAG-TIREFPAF        PIC S9(7)           COMP-3.                  
027400*                                 DATUM MANUELL PÅFYLLNADSKVANT           
027500*                                 DATE MANUAL REFILLING QTY               
027600     03 SLAG-TIREFPKT        PIC S9(7)           COMP-3.                  
027700*                                 DATUM MANUELL REFILLPUNKT               
027800*                                 DATE MANUAL REFILLING POINT             
027900     03 SLAG-TIREFSTA        PIC S9(7)           COMP-3.                  
028000*                                 DATUM AKT/PASS REFILLARTIKEL            
028100*                                 DATE ACT/PASS REFILLPART                
028200     03 SLAG-TIREFSTO        PIC S9(7)           COMP-3.                  
028300*                                 BEORDRINGSSTOPPAD T.OM.                 
028400*                                 STOPPED FOR ORDERING UNTIL              
028500     03 SLAG-TIREFSTO-LOC    PIC S9(7)           COMP-3.                  
028600*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
028700*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
028800     03 SLAG-TIRETUR-BEORD   PIC S9(7)           COMP-3.                  
028900*                                 DATUM RETUR BEORDRING                   
029000*                                 DATE ISSUE OF RETURN ORDER              
029100     03 SLAG-TISKROT         PIC S9(7)           COMP-3.                  
029200*                                 SKROTNINGSDATUM                         
029300*                                 DATE OF SCRAPPING                       
029400     03 SLAG-TISKROT-BEORD   PIC S9(7)           COMP-3.                  
029500*                                 BEORDRAD SKROTNINGSDATUM                
029600*                                 DATE OF SCRAPPING DECISION              
029700     03 SLAG-TISLUTKP        PIC S9(7)           COMP-3.                  
029800*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
029900*                                 CALC OF ATR-BAL IS TO COMMENCE          
030000     03 SLAG-TISPARR-KVAL    PIC 9(6).                                    
030100*                                 SPÄRRAD DATUM KVALITETSFEL              
030200*                                 BLOCKED DATE QUALITY ERROR              
030300     03 SLAG-VKART           PIC S9(7)           COMP-3.                  
030400*                                 ARTIKELVIKT (G)                         
030500*                                 PART WEIGHT (G)                         
030600     03 SLAG-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
030700*                                 ARTIKELVOLYM (CM3)                      
030800*                                 PART VOLUME    (CM3)                    
030900     03 SLAG-IDLEVNR-SHIP    PIC X(5).                                    
031000*                                 SKEPPANDE LEVERANTÖR                    
031100*                                 SHIPPING SUPPLIER                       
031200     03 SLAG-PRMATRL         PIC S9(7)V9(2)      COMP-3.                  
031300*                                 FAST PRIS UNDER LÖPANDE ÅR              
031400*                                 MATERIAL PRICE FOR ACTUAL YEAR          
031500     03 SLAG-TIERSDAT-VIPS   PIC S9(5)           COMP-3.                  
031600*                                 DATUM NÄR ERS. INFO TILL VIPS           
031700*                                 SEND DATE OF SUPERS. TO VIPS            
031800     03 SLAG-TIMANSEC        PIC S9(7)           COMP-3.                  
031900*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
032000*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
032100     03 SLAG-KDMATRPR        PIC X.                                       
032200*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
032300*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
032400     03 SLAG-KVPB-JUST1      PIC S9(6)V9(1)      COMP-3.                  
032500*                                 PERIODBEHOVSJUSTERING-1                 
032600*                                 PERIOD REQUIREMENTS-1                   
032700     03 SLAG-TIPBJUST-1      PIC S9(5)           COMP-3.                  
032800*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
032900*                                 FIRST PB-JUST DATE YYWW                 
033000     03 SLAG-KVPB-JUST2      PIC S9(6)V9(1)      COMP-3.                  
033100*                                 PERIODBEHOVSJUSTERING-2                 
033200*                                 PERIOD REQUIREMENTS-2                   
033300     03 SLAG-TIPBJUST-2      PIC S9(5)           COMP-3.                  
033400*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
033500*                                 SECOND PB-JUST DATE YYWW                
033600     03 SLAG-IDLEVNR-FRAM    PIC X(5).                                    
033700*                                 FRAMTIDA LEVERANTÖRNUMMER               
033800*                                 THE SUPPLIER NAME IN THE FUTURE         
033900     03 SLAG-IDLEVNR-SHIP-FRAM                                            
034000                             PIC X(5).                                    
034100*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
034200*                                 THE SHIP.SUPPLIER IN THE FUTURE         
034300     03 SLAG-TILEVDAT        PIC S9(7)           COMP-3.                  
034400*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
034500*                                 DATE FOR FUTURE SUPPLIER                
034600     03 SLAG-TIMANLED        PIC S9(7)           COMP-3.                  
034700*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
034800*                                 END DATE MAN.LEAD TIME (YYMMDD)         
034900     03 SLAG-TISTODAT-LARM   PIC S9(7)           COMP-3.                  
035000*                                 STOPPDATUM FÖR LARM-223                 
035100*                                 STOP DATE FOR ALARM-223                 
035200     03 SLAG-FLREFILL        PIC X.                                       
035300*                                 REFILLARTIKEL                           
035400*                                 REFILLPART                              
035500     03 SLAG-FLREFNYO        PIC X.                                       
035600*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
035700*                                 WAIT FOR NEXT DEMAND                    
035800     03 SLAG-TISKROT-AUTO    PIC S9(7)           COMP-3.                  
035900*                                 STOPDATE AUTO-SKROTNING                 
036000*                                 STOP DATE AUTOSCRAPPING                 
036100     03 SLAG-TIUPPDAT-EMB    PIC S9(7)           COMP-3.                  
036200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
036300*                                 UPDATING DATE     (YYMMDD)              
036400     03 SLAG-FLBUYUPD        PIC X.                                       
036500*                                 OM IDPERSONKOD ÄR LÅST                  
036600*                                 IF BUYER UPDATE IS LOCKED               
036700     03 SLAG-FLTABUPD        PIC X.                                       
036800*                                 OM REFILLTABELL ÄR LÅST                 
036900*                                 IF REFILLINGTABLE UPDATE LOCKED         
037000     03 SLAG-REPPFAKT        PIC S9V9(2)         COMP-3.                  
037100*                                 PREPLANNED FACTOR                       
037200     03 SLAG-FLPB-FLYTT      PIC X.                                       
037300*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
037400*                                  REDA PÅ KOPIERING AV PROGNOS           
037500*                                 FLAG                                    
037600     03 SLAG-FLLARM-BUF      PIC X.                                       
037700*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
037800     03 SLAG-FILLER          PIC X(10).                                   
037900*** END OF VILMAII-COPY LENGTH= 483 BYTES                                 
