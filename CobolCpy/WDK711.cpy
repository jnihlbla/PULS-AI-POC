000100 01  SLAG-WDK711.                                                         
000200*                                 S-LAGER ARTIKEL REGISTER                
000300*                                 LAGERINFORMATION                        
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 SLAG-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 SLAG-ADART.                                                       
000900*                                 ARTIKELADRESS I LAGRET                  
001000*                                 PARTS-ADRESS                            
001100        05 SLAG-ADLAGOMR     PIC S9(3)           COMP-3.                  
001200*                                 LAGEROMRÅDE                             
001300*                                 AREA                                    
001400        05 SLAG-ADGANG       PIC S9(3)           COMP-3.                  
001500*                                 GÅNG                                    
001600*                                 AISLE                                   
001700        05 SLAG-ADPLATS      PIC S9(5)           COMP-3.                  
001800*                                 LAGERPLATSNUMMER                        
001900*                                 LOCATION                                
002000     03 SLAG-ADLAGOMR-CD     PIC S9(3)           COMP-3.                  
002100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
002200*                                 AREA ADDRESS CROSS DOCKING WARE         
002300*                                 HOUSE                                   
002400     03 SLAG-DAREFESC        PIC 9(8).                                    
002500*                                 DATUM ESCLÅSTPROGNOS REFILLING          
002600*                                 DATE ESCLOCKFORECAST REFILLING          
002700     03 SLAG-DASPSEA         PIC 9(8).                                    
002800*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
002900*                                 DATE SEASON BLOCKED TO YYYYMMDD         
003000     03 SLAG-FLCDCBEH        PIC X.                                       
003100*                                 REFILLBEHOV                             
003200*                                 REFILL NEEDED                           
003300     03 SLAG-FLCDREL         PIC X.                                       
003400*                                 OMGÅENDE RELEASE AV CD-REFILL           
003500*                                 IMMEDIATE RELEASE OF CD-REFILL          
003600     03 SLAG-FLFLYG          PIC X.                                       
003700*                                 FLYGARTIKEL                             
003800*                                 PART NUMBER SENT BY AIR                 
003900     03 SLAG-FLORDSP         PIC X.                                       
004000*                                 ORDERSPÄRR                              
004100*                                 ORDER BLOCKED                           
004200     03 SLAG-FLORDSP-EJRO    PIC X.                                       
004300*                                 ORDERSPÄRR EJ RESTNOTERING              
004400*                                 ORDER BLOCK NO BACKORDERING             
004500     03 SLAG-FLPB-FLYTT      PIC X.                                       
004600*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
004700*                                  REDA PÅ KOPIERING AV PROGNOS           
004800*                                 FLAG                                    
004900     03 SLAG-FLREFBEO        PIC X.                                       
005000*                                 AUTOMATISK REFILL BEORDRING?            
005100*                                 AUTOMATIC REFILL ORDERING?              
005200     03 SLAG-FLREFLARM       PIC X.                                       
005300*                                 ONORMAL ORDERINGÅNG                     
005400*                                 ABNORMAL SALES                          
005500     03 SLAG-FLSKROT-AUTO    PIC X.                                       
005600*                                 SKROTNING AUTOMATISKT BEORDRAD          
005700*                                 SCRAPPING AUTOMATICALLY ORDERED         
005800     03 SLAG-FLSKROT-BEORD   PIC X.                                       
005900*                                 SKROTNING BEORDRAD AV ANSK              
006000*                                 SCRAPPING ORDERED BY PROCURER           
006100     03 SLAG-FLSPBULK        PIC X.                                       
006200*                                 FLAGGA SPÄRR MOT BULKORDER              
006300*                                 FLAG BULKORDER STOP                     
006400     03 SLAG-FLWILSON        PIC X.                                       
006500*                                 WILSONFORMEL                            
006600*                                 FLAG TO USE WILSON OR NOT               
006700     03 SLAG-IDDC-REF        PIC X(2).                                    
006800*                                 SÄNDANDE LAGER FÖR REFILL               
006900*                                 SENDING WAREHOUSE FOR REFILL            
007000     03 SLAG-IDLEVNR         PIC X(5).                                    
007100*                                 LEVERANTÖRNUMMER                        
007200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007300     03 SLAG-FILLERX1        PIC X.                                       
007400     03 SLAG-FLBUYUPD        PIC X.                                       
007500*                                 OM IDPERSONKOD ÄR LÅST                  
007600*                                 IF BUYER UPDATE IS LOCKED               
007700     03 SLAG-FLTABUPD        PIC X.                                       
007800*                                 OM REFILLTABELL ÄR LÅST                 
007900*                                 IF REFILLINGTABLE UPDATE LOCKED         
008000     03 SLAG-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
008100*                                 PERSONKOD REFILLANSVARIG                
008200*                                 REFILL RESPONSIBLE ID                   
008300     03 SLAG-IDREFTAB        PIC X.                                       
008400*                                 IDENTITET REFILLTABELL                  
008500*                                 REFILLINGTABLE IDENTIFIER               
008600     03 SLAG-IDUSER-SPKVAL   PIC X(8).                                    
008700*                                 ANVÄNDAR-ID KVALITETSPÄRR               
008800*                                 USER ID QUALITY ERROR                   
008900     03 SLAG-KDLEVSP         PIC S9(3)           COMP-3.                  
009000*                                 SPÄRRKOD LEVERANS                       
009100*                                 DELIVERY BLOCKING CODE                  
009200     03 SLAG-KDREFSTA        PIC X.                                       
009300*                                 STATUS REFILLARTIKEL                    
009400*                                 STATUS REFILLPART                       
009500     03 SLAG-KVAKS-PAV       PIC S9(7)           COMP-3.                  
009600*                                 DEL AV AK PÅ VÄG                        
009700*                                 PART OF AK ON ITS WAY                   
009800     03 SLAG-KVAKS-SDC       PIC S9(7)           COMP-3.                  
009900*                                 DEL AV AK SOM LIGGER I SDC              
010000*                                 PART OF AK IN THE SDC                   
010100     03 SLAG-KVBEART         PIC S9(7)           COMP-3.                  
010200*                                 BESTÄLLT ANTAL STYCKEN                  
010300*                                 ORDERED QUANTITY                        
010400     03 SLAG-KVDAGAR-CDBEH   PIC S9(3)           COMP-3.                  
010500*                                 NO. OF DAYS TO BE USED WHEN             
010600*                                 CALCULATING CD REFILLORDERS             
010700     03 SLAG-KVDAGAR-MANLT   PIC S9(3)           COMP-3.                  
010800*                                 ANTAL DAGAR                             
010900     03 SLAG-KVEFRS          PIC S9(7)           COMP-3.                  
011000*                                 EJ FAKTURERAT ANTAL STYCK               
011100*                                 ORDERED NOT INVOICED QTY                
011200     03 SLAG-KVINVS          PIC S9(7)           COMP-3.                  
011300*                                 INVENTERINGSSALDO                       
011400*                                 STOCK-TAKING BALANCE                    
011500     03 SLAG-KVLS            PIC S9(7)           COMP-3.                  
011600*                                 LAGERSALDO                              
011700*                                 STOCK BALANCE                           
011800     03 SLAG-KVOKS-BULK      PIC S9(7)           COMP-3.                  
011900*                                 ORDERKÖSALDO, KLASS 2-4                 
012000*                                 ORDER QUEUE BALANCE, CLASS 2-4          
012100     03 SLAG-KVOKS-DAG       PIC S9(7)           COMP-3.                  
012200*                                 ORDERKÖSALDO, KLASS 1                   
012300*                                 ORDER QUEUE BALANCE, CLASS 1            
012400     03 SLAG-KVPB-HIST       PIC S9(6)V9(1)      COMP-3.                  
012500*                                 PB (PROGNOS) HISTORISKT CDC             
012600*                                 HISTORIC REQUIREMENTS CDC               
012700     03 SLAG-KVPB-REF        PIC S9(6)V9(1)      COMP-3.                  
012800*                                 PERIODBEHOV REFILLING                   
012900*                                 FORECAST REFILLING                      
013000     03 SLAG-KVPBREOI        PIC S9(6)V9(1)      COMP-3.                  
013100*                                 PERIODBEHOV FÖR REFILL OI               
013200*                                 PERIOD REQUIREM. REFILLING OI           
013300     03 SLAG-KVPBREOI-HIST   PIC S9(6)V9(1)      COMP-3.                  
013400*                                 HIST MANUELT PB FÖR REFILL OI           
013500*                                 HIST MANUAL PER REQ REFILL OI           
013600     03 SLAG-KVREFBER        PIC S9(7)           COMP-3.                  
013700*                                 BERÄKNAD REFILLINGKVANTITET             
013800*                                 CALCULATED REFILLING QUANTITY           
013900     03 SLAG-KVREFOVL        PIC S9(7)           COMP-3.                  
014000*                                 BERÄKNAD ÖVERLAGERPUNKT                 
014100*                                 CALCULATED OVERSTOCK POINT              
014200     03 SLAG-KVREFPKT        PIC S9(7)           COMP-3.                  
014300*                                 BERÄKNAD PÅFYLLNADSPUNKT                
014400*                                 CALCULATED REFILLING POINT              
014500     03 SLAG-KVRESS          PIC S9(7)           COMP-3.                  
014600*                                 RESERVERAT ANTAL ARTIKLAR               
014700*                                 QUANTITY RESERVED ITEMS                 
014800     03 SLAG-KVRETUR-BEORD   PIC S9(7)           COMP-3.                  
014900*                                 ANTAL SENASTE RETURORDER                
015000*                                 QUANTITY LAST RETURNORDER               
015100     03 SLAG-KVROS-BULK      PIC S9(7)           COMP-3.                  
015200*                                 RESTORDERSALDO, KLASS 2-4               
015300*                                 BACK ORDER BALANCE, CLASS 2-4           
015400     03 SLAG-KVROS-DAG       PIC S9(7)           COMP-3.                  
015500*                                 RESTORDERSALDO, KLASS 1                 
015600*                                 BACK ORDER BALANCE, CLASS 1             
015700     03 SLAG-KVSKROT         PIC S9(7)           COMP-3.                  
015800*                                 ANTAL SENASTE SKROTORDER                
015900*                                 QUANTITY LAST SCRAPORDER                
016000     03 SLAG-KVSPARR-KVAL    PIC S9(7)           COMP-3.                  
016100*                                 SPÄRRAT ANTAL KVALITETSFEL              
016200*                                 BLOCKED QUANTITY QUALITY ERROR          
016300     03 SLAG-KVUTRS          PIC S9(7)           COMP-3.                  
016400*                                 UTREDNINGSSALDO                         
016500*                                 INVESTIGATION BALANCE                   
016600     03 SLAG-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
016700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
016800*                                 AVERAGE COST FOREIGN CURRENCY           
016900     03 SLAG-RESEASON        OCCURS 12 TIMES                              
017000                             PIC S9V9(2)         COMP-3.                  
017100*                                 SÄSONGSINDEX                            
017200     03 SLAG-RETREND         PIC S9(2)V9(1)      COMP-3.                  
017300*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
017400*                                 DIFF PREL - AVERAGE FORECAST            
017500     03 SLAG-TEKVAL          PIC X(10).                                   
017600*                                 KVALITETSNOTERING SPÄRR                 
017700*                                 QUALITY NOTE BLOCKING                   
017800     03 SLAG-TIAVCOST        PIC S9(7)           COMP-3.                  
017900*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
018000*                                 AVERAGE COST CALCULATION DATE           
018100     03 SLAG-TIINVDAT        PIC S9(5)           COMP-3.                  
018200*                                 INVENTERINGSDATUM                       
018300*                                 STOCKTAKING DATE                        
018400     03 SLAG-TIMANSEA        PIC 9(6).                                    
018500*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
018600*                                 DATE FOR MANUAL SEASON (YYMMDD)         
018700     03 SLAG-TIORDREG        PIC S9(7)           COMP-3.                  
018800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
018900*                                 ORDER REGISTRATION DATE  YYMMDD         
019000     03 SLAG-TIREFMPB        PIC S9(7)           COMP-3.                  
019100*                                 DATUM MANUELL PROGNOS REFILLING         
019200*                                 DATE MANUAL FORECAST REFILLING          
019300     03 SLAG-TIREFPAF        PIC S9(7)           COMP-3.                  
019400*                                 DATUM MANUELL PÅFYLLNADSKVANT           
019500*                                 DATE MANUAL REFILLING QTY               
019600     03 SLAG-TIREFPKT        PIC S9(7)           COMP-3.                  
019700*                                 DATUM MANUELL REFILLPUNKT               
019800*                                 DATE MANUAL REFILLING POINT             
019900     03 SLAG-TIREFSTA        PIC S9(7)           COMP-3.                  
020000*                                 DATUM AKT/PASS REFILLARTIKEL            
020100*                                 DATE ACT/PASS REFILLPART                
020200     03 SLAG-TIREFSTO        PIC S9(7)           COMP-3.                  
020300*                                 BEORDRINGSSTOPPAD T.OM.                 
020400*                                 STOPPED FOR ORDERING UNTIL              
020500     03 SLAG-TIRETUR-BEORD   PIC S9(7)           COMP-3.                  
020600*                                 DATUM RETUR BEORDRING                   
020700*                                 DATE ISSUE OF RETURN ORDER              
020800     03 SLAG-TISKROT         PIC S9(7)           COMP-3.                  
020900*                                 SKROTNINGSDATUM                         
021000*                                 DATE OF SCRAPPING                       
021100     03 SLAG-TISKROT-AUTO    PIC S9(7)           COMP-3.                  
021200*                                 STOPDATE AUTO-SKROTNING                 
021300*                                 STOP DATE AUTOSCRAPPING                 
021400     03 SLAG-TISKROT-BEORD   PIC S9(7)           COMP-3.                  
021500*                                 BEORDRAD SKROTNINGSDATUM                
021600*                                 DATE OF SCRAPPING DECISION              
021700     03 SLAG-TISPARR-KVAL    PIC 9(6).                                    
021800*                                 SPÄRRAD DATUM KVALITETSFEL              
021900*                                 BLOCKED DATE QUALITY ERROR              
022000     03 SLAG-TIPBREOI        PIC 9(6).                                    
022100*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
022200*                                 DATE MAN. FC REFILL OI (YYMMDD)         
022300     03 SLAG-DAREFESC-REOI   PIC 9(8).                                    
022400*                                 DATUM ESCLÅSTPROGNOS KVPBREOI           
022500*                                 DATE ESCLOCKFORECAST KVPBREOI           
022600     03 SLAG-RETREND-REOI    PIC S9(2)V9(1)      COMP-3.                  
022700*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
022800*                                 DIFF PREL - AVERAGE FORECAST            
022900     03 SLAG-TIDATUM-CROSS   PIC S9(7)           COMP-3.                  
023000*                                 STARTDAG CROSS DOCKING FÖRDRÖJN         
023100*                                 START DATE CROSS DELAY CALCULAT         
023200     03 SLAG-TISTODAT-LARM   PIC S9(7)           COMP-3.                  
023300*                                 STOPPDATUM FÖR LARM-223                 
023400*                                 STOP DATE FOR ALARM-223                 
023500     03 SLAG-FLREFILL        PIC X.                                       
023600*                                 REFILLARTIKEL                           
023700*                                 REFILLPART                              
023800     03 SLAG-FLREFNYO        PIC X.                                       
023900*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
024000*                                 WAIT FOR NEXT DEMAND                    
024100     03 SLAG-REPPFAKT        PIC S9V9(2)         COMP-3.                  
024200*                                 PREPLANNED FACTOR                       
024300     03 SLAG-FILLER          PIC X(7).                                    
024400*** END OF VILMAII-COPY LENGTH= 290 BYTES                                 
