000100 01  SLAG-W01184X.                                                        
000200*                                 THIS IS COPY OF W01184 IN EDITE         
000300*                                 D FORMAT                                
000400*                                 TO CREATE WXTR FILE TO SEND IT          
000500*                                 TO AZURE DATALAKE                       
000600     03 SLAG-IDARTNR         PIC Z(7)9                                    
000700                             VALUE ZEROS.                                 
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 SLAG-IDDC            PIC X(2)                                     
001100                             VALUE SPACES.                                
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SLAG-ADART.                                                       
001500*                                 ARTIKELADRESS I LAGRET                  
001600*                                 PARTS-ADRESS                            
001700        05 SLAG-ADLAGOMR     PIC Z9                                       
001800                             VALUE ZEROS.                                 
001900*                                 LAGEROMRÅDE                             
002000*                                 AREA                                    
002100        05 SLAG-ADGANG       PIC Z9                                       
002200                             VALUE ZEROS.                                 
002300*                                 GÅNG                                    
002400*                                 AISLE                                   
002500        05 SLAG-ADPLATS      PIC Z(4)9                                    
002600                             VALUE ZEROS.                                 
002700*                                 LAGERPLATSNUMMER                        
002800*                                 LOCATION                                
002900     03 SLAG-ADLAGOMR-CD     PIC Z9                                       
003000                             VALUE ZEROS.                                 
003100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003200*                                 AREA ADDRESS CROSS DOCKING WARE         
003300*                                 HOUSE                                   
003400     03 SLAG-BEFT            PIC Z9                                       
003500                             VALUE ZEROS.                                 
003600*                                 FÖRPACKNINGSTYP                         
003700*                                 PACKAGING TYPE                          
003800     03 SLAG-DAPBPLAN        PIC 9(8)                                     
003900                             VALUE ZEROS.                                 
004000*                                 DATUM KVPB-PLAN GILTIG TOM              
004100*                                 DATE KVPB-PLAN VALID UNTIL              
004200     03 SLAG-DAPUBL          PIC 9(8)                                     
004300                             VALUE ZEROS.                                 
004400*                                 PUBLICERINGSDATUM PER ART/LAND          
004500*                                 DATE OF PUBLISHING PART/COUNTRY         
004600     03 SLAG-DASEASON        PIC 9(8)                                     
004700                             VALUE ZEROS.                                 
004800*                                 DATUM RESEASON-LEDTID GILTIG TO         
004900*                                 M                                       
005000*                                 DATE RESEASON-LEDTID VALID UNTI         
005100*                                 L                                       
005200     03 SLAG-DASPSEA         PIC 9(8)                                     
005300                             VALUE ZEROS.                                 
005400*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
005500*                                 DATE SEASON BLOCKED TO YYYYMMDD         
005600     03 SLAG-FLCDREL         PIC X                                        
005700                             VALUE SPACE.                                 
005800*                                 OMGÅENDE RELEASE AV CD-REFILL           
005900*                                 IMMEDIATE RELEASE OF CD-REFILL          
006000     03 SLAG-FLFLYG          PIC X                                        
006100                             VALUE SPACE.                                 
006200*                                 FLYGARTIKEL                             
006300*                                 PART NUMBER SENT BY AIR                 
006400     03 SLAG-FLJIT           PIC X                                        
006500                             VALUE SPACE.                                 
006600*                                 JUST-IN-TIME FLAGGA                     
006700*                                 JUST-IN-TIME FLAG                       
006800     03 SLAG-FLORDSP         PIC X                                        
006900                             VALUE SPACE.                                 
007000*                                 ORDERSPÄRR                              
007100*                                 ORDER BLOCKED                           
007200     03 SLAG-FLORDSP-EJRO    PIC X                                        
007300                             VALUE SPACE.                                 
007400*                                 ORDERSPÄRR EJ RESTNOTERING              
007500*                                 ORDER BLOCK NO BACKORDERING             
007600     03 SLAG-FLREFBEO        PIC X                                        
007700                             VALUE SPACE.                                 
007800*                                 AUTOMATISK REFILL BEORDRING?            
007900*                                 AUTOMATIC REFILL ORDERING?              
008000     03 SLAG-FLREFLARM       PIC X                                        
008100                             VALUE SPACE.                                 
008200*                                 ONORMAL ORDERINGÅNG                     
008300*                                 ABNORMAL SALES                          
008400     03 SLAG-FLSKROT-BEORD   PIC X                                        
008500                             VALUE SPACE.                                 
008600*                                 SKROTNING BEORDRAD AV ANSK              
008700*                                 SCRAPPING ORDERED BY PROCURER           
008800     03 SLAG-FLSPBULK        PIC X                                        
008900                             VALUE SPACE.                                 
009000*                                 FLAGGA SPÄRR MOT BULKORDER              
009100*                                 FLAG BULKORDER STOP                     
009200     03 SLAG-FLWILSON        PIC X                                        
009300                             VALUE SPACE.                                 
009400*                                 WILSONFORMEL                            
009500*                                 FLAG TO USE WILSON OR NOT               
009600     03 SLAG-IDANSK          PIC Z(2)9                                    
009700                             VALUE ZEROS.                                 
009800*                                 ANSKAFFARNUMMER                         
009900*                                 PROCURER NO.                            
010000     03 SLAG-IDARTNR-EMBQ0   PIC Z(7)9                                    
010100                             VALUE ZEROS.                                 
010200*                                 EMBALLAGEARTIKELNR FÖR Q0               
010300     03 SLAG-IDARTNR-EMBQ1   PIC Z(7)9                                    
010400                             VALUE ZEROS.                                 
010500*                                 EMBALLAGEARTIKELNR FÖR Q1               
010600     03 SLAG-IDARTNR-EMBQ2   PIC Z(7)9                                    
010700                             VALUE ZEROS.                                 
010800*                                 EMBALLAGEARTIKELNR FÖR Q2               
010900     03 SLAG-IDDC-REF        PIC X(2)                                     
011000                             VALUE SPACES.                                
011100*                                 SÄNDANDE LAGER FÖR REFILL               
011200*                                 SENDING WAREHOUSE FOR REFILL            
011300     03 SLAG-IDINK           PIC X(4)                                     
011400                             VALUE SPACES.                                
011500*                                 INKÖPARNUMMER                           
011600*                                 PURCHASE IDENTIFICATION NUMBER          
011700     03 SLAG-IDLANDX2        PIC X(2)                                     
011800                             VALUE SPACES.                                
011900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
012000*                                 2-LETTER CODE FOR COUNTRY               
012100     03 SLAG-IDLEVNR         PIC X(5)                                     
012200                             VALUE SPACES.                                
012300*                                 LEVERANTÖRNUMMER                        
012400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
012500     03 SLAG-IDPERSON-BUY    PIC Z(2)9                                    
012600                             VALUE ZEROS.                                 
012700*                                 PERSONKOD REFILLANSVARIG                
012800*                                 REFILL RESPONSIBLE ID                   
012900     03 SLAG-IDPLANGR-AG     PIC 9                                        
013000                             VALUE ZERO.                                  
013100*                                 PLANERINGSGRUPP ANSKAFFARE              
013200     03 SLAG-IDPSN-DC        PIC 9(3)                                     
013300                             VALUE ZEROS.                                 
013400*                                 PROPER SHIPPING NAME PER XDC            
013500*                                 PROPER SHIPPING NAME XDC                
013600     03 SLAG-IDREFTAB        PIC X                                        
013700                             VALUE SPACE.                                 
013800*                                 IDENTITET REFILLTABELL                  
013900*                                 REFILLINGTABLE IDENTIFIER               
014000     03 SLAG-IDUSER-SPKVAL   PIC X(8)                                     
014100                             VALUE SPACES.                                
014200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
014300*                                 USER ID QUALITY ERROR                   
014400     03 SLAG-KDARTURS        PIC X(2)                                     
014500                             VALUE SPACES.                                
014600*                                 ARTIKELURSPRUNGSKOD                     
014700*                                 COUNTRY OF ORIGIN                       
014800     03 SLAG-KDAVT           PIC 9                                        
014900                             VALUE ZERO.                                  
015000*                                 AVTALSMÄRKNING                          
015100*                                 AGREEMENT CODE                          
015200     03 SLAG-KDLEVPLF        PIC X                                        
015300                             VALUE SPACE.                                 
015400*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
015500*                                 CODE FOR APPROVAL OF SCHEDULE P         
015600*                                 ROPOSAL                                 
015700     03 SLAG-KDLEVSP         PIC Z9                                       
015800                             VALUE ZEROS.                                 
015900*                                 SPÄRRKOD LEVERANS                       
016000*                                 DELIVERY BLOCKING CODE                  
016100     03 SLAG-KDLPSP          PIC 9                                        
016200                             VALUE ZERO.                                  
016300*                                 LEVERANSPLANESPÄRR                      
016400     03 SLAG-KDOPPLAN        PIC X                                        
016500                             VALUE SPACE.                                 
016600*                                 OPTIMAL PLAN INOM FRYSTID               
016700*                                 OPTIMAL PLAN WITHIN FREEZTIME           
016800     03 SLAG-KDREFSTA        PIC X                                        
016900                             VALUE SPACE.                                 
017000*                                 STATUS REFILLARTIKEL                    
017100*                                 STATUS REFILLPART                       
017200     03 SLAG-KVAKS-PAV       PIC -(7)9                                    
017300                             VALUE ZEROS.                                 
017400*                                 DEL AV AK PÅ VÄG                        
017500*                                 PART OF AK ON ITS WAY                   
017600     03 SLAG-KVAKS-SDC       PIC -(7)9                                    
017700                             VALUE ZEROS.                                 
017800*                                 DEL AV AK SOM LIGGER I SDC              
017900*                                 PART OF AK IN THE SDC                   
018000     03 SLAG-KVPBREOI        PIC Z(5)9.9                                  
018100                             VALUE ZEROS.                                 
018200*                                 PERIODBEHOV FÖR REFILL OI               
018300*                                 PERIOD REQUIREM. REFILLING OI           
018400     03 SLAG-KVBEART         PIC Z(5)9                                    
018500                             VALUE ZEROS.                                 
018600*                                 BESTÄLLT ANTAL STYCKEN                  
018700*                                 ORDERED QUANTITY                        
018800     03 SLAG-KVDAGAR-CDBEH   PIC Z9                                       
018900                             VALUE ZEROS.                                 
019000*                                 NO. OF DAYS TO BE USED WHEN             
019100*                                 CALCULATING CD REFILLORDERS             
019200     03 SLAG-KVDAGAR-FFH     PIC Z9                                       
019300                             VALUE ZEROS.                                 
019400*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
019500     03 SLAG-KVDAGAR-INLEV   PIC Z9                                       
019600                             VALUE ZEROS.                                 
019700*                                 INLEVERANSTID     (ANTAL DAGAR)         
019800     03 SLAG-KVDAGAR-MANLT   PIC Z(2)9                                    
019900                             VALUE ZEROS.                                 
020000*                                 ANTAL DAGAR                             
020100     03 SLAG-KVEFRS          PIC -(7)9                                    
020200                             VALUE ZEROS.                                 
020300*                                 EJ FAKTURERAT ANTAL STYCK               
020400*                                 ORDERED NOT INVOICED QTY                
020500     03 SLAG-KVEOQ           PIC Z(6)9                                    
020600                             VALUE ZEROS.                                 
020700*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
020800*                                 ET                                      
020900     03 SLAG-KVINVS          PIC Z(6)9                                    
021000                             VALUE ZEROS.                                 
021100*                                 INVENTERINGSSALDO                       
021200*                                 STOCK-TAKING BALANCE                    
021300     03 SLAG-KVLS            PIC -(7)9                                    
021400                             VALUE ZEROS.                                 
021500*                                 LAGERSALDO                              
021600*                                 STOCK BALANCE                           
021700     03 SLAG-KVOKS-BULK      PIC -(6)9                                    
021800                             VALUE ZEROS.                                 
021900*                                 ORDERKÖSALDO, KLASS 2-4                 
022000*                                 ORDER QUEUE BALANCE, CLASS 2-4          
022100     03 SLAG-KVOKS-DAG       PIC -(6)9                                    
022200                             VALUE ZEROS.                                 
022300*                                 ORDERKÖSALDO, KLASS 1                   
022400*                                 ORDER QUEUE BALANCE, CLASS 1            
022500     03 SLAG-KVPALL          PIC Z(6)9                                    
022600                             VALUE ZEROS.                                 
022700*                                 ANTAL I PALL                            
022800*                                 QUANTITY IN PALLET                      
022900     03 SLAG-KVPB-PLAN       PIC Z(5)9.9                                  
023000                             VALUE ZEROS.                                 
023100*                                 PLANERAT PERIODBEHOV                    
023200*                                 PLANNED PERIOD REQUIREMENTS             
023300     03 SLAG-KVPB-REF        PIC Z(5)9.9                                  
023400                             VALUE ZEROS.                                 
023500*                                 PERIODBEHOV REFILLING                   
023600*                                 FORECAST REFILLING                      
023700     03 SLAG-KVPB-TREND      PIC Z(5)9.9                                  
023800                             VALUE ZEROS.                                 
023900*                                 PERIODTRENDVÄRDE                        
024000     03 SLAG-KVREFBER        PIC Z(6)9                                    
024100                             VALUE ZEROS.                                 
024200*                                 BERÄKNAD REFILLINGKVANTITET             
024300*                                 CALCULATED REFILLING QUANTITY           
024400     03 SLAG-KVREFOVL        PIC Z(6)9                                    
024500                             VALUE ZEROS.                                 
024600*                                 BERÄKNAD ÖVERLAGERPUNKT                 
024700*                                 CALCULATED OVERSTOCK POINT              
024800     03 SLAG-KVREFPKT        PIC Z(6)9                                    
024900                             VALUE ZEROS.                                 
025000*                                 BERÄKNAD PÅFYLLNADSPUNKT                
025100*                                 CALCULATED REFILLING POINT              
025200     03 SLAG-KVRESS          PIC Z(6)9                                    
025300                             VALUE ZEROS.                                 
025400*                                 RESERVERAT ANTAL ARTIKLAR               
025500*                                 QUANTITY RESERVED ITEMS                 
025600     03 SLAG-KVRETUR-BEORD   PIC -(6)9                                    
025700                             VALUE ZEROS.                                 
025800*                                 ANTAL SENASTE RETURORDER                
025900*                                 QUANTITY LAST RETURNORDER               
026000     03 SLAG-KVROS-BULK      PIC -(7)9                                    
026100                             VALUE ZEROS.                                 
026200*                                 RESTORDERSALDO, KLASS 2-4               
026300*                                 BACK ORDER BALANCE, CLASS 2-4           
026400     03 SLAG-KVROS-DAG       PIC -(7)9                                    
026500                             VALUE ZEROS.                                 
026600*                                 RESTORDERSALDO, KLASS 1                 
026700*                                 BACK ORDER BALANCE, CLASS 1             
026800     03 SLAG-KVSKROT         PIC Z(6)9                                    
026900                             VALUE ZEROS.                                 
027000*                                 ANTAL SENASTE SKROTORDER                
027100*                                 QUANTITY LAST SCRAPORDER                
027200     03 SLAG-KVSLAGER        PIC Z(5)9                                    
027300                             VALUE ZEROS.                                 
027400*                                 SÄKERHETSLAGER                          
027500*                                 SAFETY STOCK                            
027600     03 SLAG-KVSLUTKP        PIC Z(6)9                                    
027700                             VALUE ZEROS.                                 
027800*                                 SLUTKÖPSSALDO                           
027900     03 SLAG-KVSPANT         PIC -(6)9                                    
028000                             VALUE ZEROS.                                 
028100*                                 SPÄRRAT ANTAL                           
028200*                                 BLOCKED QTY                             
028300     03 SLAG-KVSPARR-KVAL    PIC Z(6)9                                    
028400                             VALUE ZEROS.                                 
028500*                                 SPÄRRAT ANTAL KVALITETSFEL              
028600*                                 BLOCKED QUANTITY QUALITY ERROR          
028700     03 SLAG-KVULOAD         PIC Z(6)9                                    
028800                             VALUE ZEROS.                                 
028900*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
029000*                                 MIN LOAD FROM SUPPLIER                  
029100     03 SLAG-KVUTRS          PIC -(7)9                                    
029200                             VALUE ZEROS.                                 
029300*                                 UTREDNINGSSALDO                         
029400*                                 INVESTIGATION BALANCE                   
029500     03 SLAG-KVVECKOR-FT     PIC Z9                                       
029600                             VALUE ZEROS.                                 
029700*                                 ANTAL VECKOR FRYSNINGSTID               
029800     03 SLAG-KVVECKOR-LT     PIC Z9                                       
029900                             VALUE ZEROS.                                 
030000*                                 ANTAL VECKOR LEDTID                     
030100     03 SLAG-KVVECKOR-TREND  PIC Z9                                       
030200                             VALUE ZEROS.                                 
030300*                                 ANTAL VECKOR TRENDVÄRDE                 
030400     03 SLAG-PRARTSJK        PIC Z(6)9.9(2)                               
030500                             VALUE ZEROS.                                 
030600*                                 ARTIKELNS SJÄLVKOSTNAD                  
030700*                                 COST OF SALES                           
030800     03 SLAG-PRAVCOST        PIC Z(6)9.9(2)                               
030900                             VALUE ZEROS.                                 
031000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
031100*                                 AVERAGE COST FOREIGN CURRENCY           
031200     03 SLAG-RESEASON        OCCURS 12 TIMES                              
031300                             PIC 9.9(2)                                   
031400                             VALUE ZEROS.                                 
031500*                                 SÄSONGSINDEX                            
031600     03 SLAG-RESEASON-PLAN   OCCURS 12 TIMES                              
031700                             PIC 9.9(2)                                   
031800                             VALUE ZEROS.                                 
031900*                                 SÄSONGSINDEX INKLUSIVE REFILL           
032000     03 SLAG-RETREND         PIC Z9.9                                     
032100                             VALUE ZEROS.                                 
032200*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
032300*                                 DIFF PREL - AVERAGE FORECAST            
032400     03 SLAG-TEKVAL          PIC X(10)                                    
032500                             VALUE SPACES.                                
032600*                                 KVALITETSNOTERING SPÄRR                 
032700*                                 QUALITY NOTE BLOCKING                   
032800     03 SLAG-TIAVCOST        PIC 9(6)                                     
032900                             VALUE ZEROS.                                 
033000*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
033100*                                 AVERAGE COST CALCULATION DATE           
033200     03 SLAG-TIDATUM-TREND   PIC 9(6)                                     
033300                             VALUE ZEROS.                                 
033400*                                 JUSTERAD TREND AAMMDD                   
033500*                                 LAST TREND CHANGE  YYMMDD               
033600     03 SLAG-TIINVDAT        PIC 9(5)                                     
033700                             VALUE ZEROS.                                 
033800*                                 INVENTERINGSDATUM                       
033900*                                 STOCKTAKING DATE                        
034000     03 SLAG-TILEVDAG        OCCURS 5 TIMES                               
034100                             PIC 9                                        
034200                             VALUE ZERO.                                  
034300*                                 AVSÄNDNINGSDAG INOM VECKA               
034400*                                 DELIVERY WEEK DAY                       
034500     03 SLAG-TILPSP          PIC 9(4)                                     
034600                             VALUE ZEROS.                                 
034700*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
034800     03 SLAG-TIMANSEA        PIC 9(6)                                     
034900                             VALUE ZEROS.                                 
035000*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
035100*                                 DATE FOR MANUAL SEASON (YYMMDD)         
035200     03 SLAG-TIOMSPEC        PIC 9(4)                                     
035300                             VALUE ZEROS.                                 
035400*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
035500     03 SLAG-TIORDREG        PIC 9(6)                                     
035600                             VALUE ZEROS.                                 
035700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
035800*                                 ORDER REGISTRATION DATE  YYMMDD         
035900     03 SLAG-TIPBREOI        PIC 9(6)                                     
036000                             VALUE ZEROS.                                 
036100*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
036200*                                 DATE MAN. FC REFILL OI (YYMMDD)         
036300     03 SLAG-TIREFMPB        PIC 9(6)                                     
036400                             VALUE ZEROS.                                 
036500*                                 DATUM MANUELL PROGNOS REFILLING         
036600*                                 DATE MANUAL FORECAST REFILLING          
036700     03 SLAG-TIREFPAF        PIC 9(6)                                     
036800                             VALUE ZEROS.                                 
036900*                                 DATUM MANUELL PÅFYLLNADSKVANT           
037000*                                 DATE MANUAL REFILLING QTY               
037100     03 SLAG-TIREFPKT        PIC 9(6)                                     
037200                             VALUE ZEROS.                                 
037300*                                 DATUM MANUELL REFILLPUNKT               
037400*                                 DATE MANUAL REFILLING POINT             
037500     03 SLAG-TIREFSTA        PIC 9(6)                                     
037600                             VALUE ZEROS.                                 
037700*                                 DATUM AKT/PASS REFILLARTIKEL            
037800*                                 DATE ACT/PASS REFILLPART                
037900     03 SLAG-TIREFSTO        PIC 9(6)                                     
038000                             VALUE ZEROS.                                 
038100*                                 BEORDRINGSSTOPPAD T.OM.                 
038200*                                 STOPPED FOR ORDERING UNTIL              
038300     03 SLAG-TIREFSTO-LOC    PIC 9(6)                                     
038400                             VALUE ZEROS.                                 
038500*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
038600*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
038700     03 SLAG-TIRETUR-BEORD   PIC 9(6)                                     
038800                             VALUE ZEROS.                                 
038900*                                 DATUM RETUR BEORDRING                   
039000*                                 DATE ISSUE OF RETURN ORDER              
039100     03 SLAG-TISKROT         PIC 9(6)                                     
039200                             VALUE ZEROS.                                 
039300*                                 SKROTNINGSDATUM                         
039400*                                 DATE OF SCRAPPING                       
039500     03 SLAG-TISKROT-BEORD   PIC 9(6)                                     
039600                             VALUE ZEROS.                                 
039700*                                 BEORDRAD SKROTNINGSDATUM                
039800*                                 DATE OF SCRAPPING DECISION              
039900     03 SLAG-TISLUTKP        PIC Z(6)                                     
040000                             VALUE ZEROS.                                 
040100*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
040200*                                 CALC OF ATR-BAL IS TO COMMENCE          
040300     03 SLAG-TISPARR-KVAL    PIC 9(6)                                     
040400                             VALUE ZEROS.                                 
040500*                                 SPÄRRAD DATUM KVALITETSFEL              
040600*                                 BLOCKED DATE QUALITY ERROR              
040700     03 SLAG-VKART           PIC Z(6)9                                    
040800                             VALUE ZEROS.                                 
040900*                                 ARTIKELVIKT (G)                         
041000*                                 PART WEIGHT (G)                         
041100     03 SLAG-VLARTNTO        PIC Z(7)9.9                                  
041200                             VALUE ZEROS.                                 
041300*                                 ARTIKELVOLYM (CM3)                      
041400*                                 PART VOLUME    (CM3)                    
041500     03 SLAG-IDLEVNR-SHIP    PIC X(5)                                     
041600                             VALUE SPACES.                                
041700*                                 SKEPPANDE LEVERANTÖR                    
041800*                                 SHIPPING SUPPLIER                       
041900     03 SLAG-PRMATRL         PIC Z(6)9.9(2)                               
042000                             VALUE ZEROS.                                 
042100*                                 FAST PRIS UNDER LÖPANDE ÅR              
042200*                                 MATERIAL PRICE FOR ACTUAL YEAR          
042300     03 SLAG-TIERSDAT-VIPS   PIC 9(5)                                     
042400                             VALUE ZEROS.                                 
042500*                                 DATUM NÄR ERS. INFO TILL VIPS           
042600*                                 SEND DATE OF SUPERS. TO VIPS            
042700     03 SLAG-TIMANSEC        PIC 9(6)                                     
042800                             VALUE ZEROS.                                 
042900*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
043000*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
043100     03 SLAG-KDMATRPR        PIC X                                        
043200                             VALUE SPACE.                                 
043300*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
043400*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
043500     03 SLAG-KVPB-JUST1      PIC Z(5)9.9                                  
043600                             VALUE ZEROS.                                 
043700*                                 PERIODBEHOVSJUSTERING-1                 
043800*                                 PERIOD REQUIREMENTS-1                   
043900     03 SLAG-TIPBJUST-1      PIC 9(4)                                     
044000                             VALUE ZEROS.                                 
044100*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
044200*                                 FIRST PB-JUST DATE YYWW                 
044300     03 SLAG-KVPB-JUST2      PIC Z(5)9.9                                  
044400                             VALUE ZEROS.                                 
044500*                                 PERIODBEHOVSJUSTERING-2                 
044600*                                 PERIOD REQUIREMENTS-2                   
044700     03 SLAG-TIPBJUST-2      PIC 9(4)                                     
044800                             VALUE ZEROS.                                 
044900*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
045000*                                 SECOND PB-JUST DATE YYWW                
045100     03 SLAG-IDLEVNR-FRAM    PIC X(5)                                     
045200                             VALUE SPACES.                                
045300*                                 FRAMTIDA LEVERANTÖRNUMMER               
045400*                                 THE SUPPLIER NAME IN THE FUTURE         
045500     03 SLAG-IDLEVNR-SHIP-FRAM                                            
045600                             PIC X(5)                                     
045700                             VALUE SPACES.                                
045800*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
045900*                                 THE SHIP.SUPPLIER IN THE FUTURE         
046000     03 SLAG-TILEVDAT        PIC 9(6)                                     
046100                             VALUE ZEROS.                                 
046200*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
046300*                                 DATE FOR FUTURE SUPPLIER                
046400     03 SLAG-TIMANLED        PIC 9(6)                                     
046500                             VALUE ZEROS.                                 
046600*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
046700*                                 END DATE MAN.LEAD TIME (YYMMDD)         
046800     03 SLAG-TISTODAT-LARM   PIC 9(6)                                     
046900                             VALUE ZEROS.                                 
047000*                                 STOPPDATUM FÖR LARM-223                 
047100*                                 STOP DATE FOR ALARM-223                 
047200     03 SLAG-FLREFILL        PIC X                                        
047300                             VALUE SPACE.                                 
047400*                                 REFILLARTIKEL                           
047500*                                 REFILLPART                              
047600     03 SLAG-FLREFNYO        PIC X                                        
047700                             VALUE SPACE.                                 
047800*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
047900*                                 WAIT FOR NEXT DEMAND                    
048000     03 SLAG-TISKROT-AUTO    PIC 9(6)                                     
048100                             VALUE ZEROS.                                 
048200*                                 STOPDATE AUTO-SKROTNING                 
048300*                                 STOP DATE AUTOSCRAPPING                 
048400     03 SLAG-TIUPPDAT-EMB    PIC 9(6)                                     
048500                             VALUE ZEROS.                                 
048600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
048700*                                 UPDATING DATE     (YYMMDD)              
048800     03 SLAG-FLBUYUPD        PIC X                                        
048900                             VALUE SPACE.                                 
049000*                                 OM IDPERSONKOD ÄR LÅST                  
049100*                                 IF BUYER UPDATE IS LOCKED               
049200     03 SLAG-FLTABUPD        PIC X                                        
049300                             VALUE SPACE.                                 
049400*                                 OM REFILLTABELL ÄR LÅST                 
049500*                                 IF REFILLINGTABLE UPDATE LOCKED         
049600     03 SLAG-REPPFAKT        PIC 9.9(2)                                   
049700                             VALUE ZEROS.                                 
049800*                                 PREPLANNED FACTOR                       
049900     03 SLAG-FLPB-FLYTT      PIC X                                        
050000                             VALUE SPACE.                                 
050100*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
050200*                                  REDA PÅ KOPIERING AV PROGNOS           
050300*                                 FLAG                                    
050400     03 SLAG-FLLARM-BUF      PIC X                                        
050500                             VALUE SPACE.                                 
050600*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
050700     03 SLAG-FILLER          PIC X(10)                                    
050800                             VALUE SPACES.                                
050900*** END OF VILMAII-COPY LENGTH= 722 BYTES                                 
