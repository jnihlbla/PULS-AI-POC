000100 01  SLAG-W01184EX.                                                       
000200*                                 THIS IS COPY OF W01184 IN EDITE         
000300*                                 D FORMAT                                
000400*                                 TO CREATE WXTR FILE TO SEND IT          
000500*                                 TO AZURE DATALAKE                       
000600     03 SLAG-IDARTNR         PIC Z(7)9                                    
000700                             VALUE ZEROS.                                 
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 SLAG-FILL1           PIC X                                        
001100                             VALUE SPACE.                                 
001200     03 SLAG-IDDC            PIC X(2)                                     
001300                             VALUE SPACES.                                
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SLAG-FILL2           PIC X                                        
001700                             VALUE SPACE.                                 
001800     03 SLAG-ADLAGOMR        PIC Z9                                       
001900                             VALUE ZEROS.                                 
002000*                                 LAGEROMRÅDE                             
002100*                                 AREA                                    
002200     03 SLAG-FILL3           PIC X                                        
002300                             VALUE SPACE.                                 
002400     03 SLAG-ADGANG          PIC Z9                                       
002500                             VALUE ZEROS.                                 
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800     03 SLAG-FILL4           PIC X                                        
002900                             VALUE SPACE.                                 
003000     03 SLAG-ADPLATS         PIC Z(4)9                                    
003100                             VALUE ZEROS.                                 
003200*                                 LAGERPLATSNUMMER                        
003300*                                 LOCATION                                
003400     03 SLAG-FILL5           PIC X                                        
003500                             VALUE SPACE.                                 
003600     03 SLAG-ADLAGOMR-CD     PIC Z9                                       
003700                             VALUE ZEROS.                                 
003800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003900*                                 AREA ADDRESS CROSS DOCKING WARE         
004000*                                 HOUSE                                   
004100     03 SLAG-FILL6           PIC X                                        
004200                             VALUE SPACE.                                 
004300     03 SLAG-BEFT            PIC Z9                                       
004400                             VALUE ZEROS.                                 
004500*                                 FÖRPACKNINGSTYP                         
004600*                                 PACKAGING TYPE                          
004700     03 SLAG-FILL7           PIC X                                        
004800                             VALUE SPACE.                                 
004900     03 SLAG-DAPBPLAN        PIC 9(8)                                     
005000                             VALUE ZEROS.                                 
005100*                                 DATUM KVPB-PLAN GILTIG TOM              
005200*                                 DATE KVPB-PLAN VALID UNTIL              
005300     03 SLAG-FILL8           PIC X                                        
005400                             VALUE SPACE.                                 
005500     03 SLAG-DAPUBL          PIC 9(8)                                     
005600                             VALUE ZEROS.                                 
005700*                                 PUBLICERINGSDATUM PER ART/LAND          
005800*                                 DATE OF PUBLISHING PART/COUNTRY         
005900     03 SLAG-FILL9           PIC X                                        
006000                             VALUE SPACE.                                 
006100     03 SLAG-DASEASON        PIC 9(8)                                     
006200                             VALUE ZEROS.                                 
006300*                                 DATUM RESEASON-LEDTID GILTIG TO         
006400*                                 M                                       
006500*                                 DATE RESEASON-LEDTID VALID UNTI         
006600*                                 L                                       
006700     03 SLAG-FILL10          PIC X                                        
006800                             VALUE SPACE.                                 
006900     03 SLAG-DASPSEA         PIC 9(8)                                     
007000                             VALUE ZEROS.                                 
007100*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
007200*                                 DATE SEASON BLOCKED TO YYYYMMDD         
007300     03 SLAG-FILL11          PIC X                                        
007400                             VALUE SPACE.                                 
007500     03 SLAG-FLCDREL         PIC X                                        
007600                             VALUE SPACE.                                 
007700*                                 OMGÅENDE RELEASE AV CD-REFILL           
007800*                                 IMMEDIATE RELEASE OF CD-REFILL          
007900     03 SLAG-FILL12          PIC X                                        
008000                             VALUE SPACE.                                 
008100     03 SLAG-FLFLYG          PIC X                                        
008200                             VALUE SPACE.                                 
008300*                                 FLYGARTIKEL                             
008400*                                 PART NUMBER SENT BY AIR                 
008500     03 SLAG-FILL13          PIC X                                        
008600                             VALUE SPACE.                                 
008700     03 SLAG-FLJIT           PIC X                                        
008800                             VALUE SPACE.                                 
008900*                                 JUST-IN-TIME FLAGGA                     
009000*                                 JUST-IN-TIME FLAG                       
009100     03 SLAG-FILL14          PIC X                                        
009200                             VALUE SPACE.                                 
009300     03 SLAG-FLORDSP         PIC X                                        
009400                             VALUE SPACE.                                 
009500*                                 ORDERSPÄRR                              
009600*                                 ORDER BLOCKED                           
009700     03 SLAG-FILL15          PIC X                                        
009800                             VALUE SPACE.                                 
009900     03 SLAG-FLORDSP-EJRO    PIC X                                        
010000                             VALUE SPACE.                                 
010100*                                 ORDERSPÄRR EJ RESTNOTERING              
010200*                                 ORDER BLOCK NO BACKORDERING             
010300     03 SLAG-FILL16          PIC X                                        
010400                             VALUE SPACE.                                 
010500     03 SLAG-FLREFBEO        PIC X                                        
010600                             VALUE SPACE.                                 
010700*                                 AUTOMATISK REFILL BEORDRING?            
010800*                                 AUTOMATIC REFILL ORDERING?              
010900     03 SLAG-FILL17          PIC X                                        
011000                             VALUE SPACE.                                 
011100     03 SLAG-FLREFLARM       PIC X                                        
011200                             VALUE SPACE.                                 
011300*                                 ONORMAL ORDERINGÅNG                     
011400*                                 ABNORMAL SALES                          
011500     03 SLAG-FILL18          PIC X                                        
011600                             VALUE SPACE.                                 
011700     03 SLAG-FLSKROT-BEORD   PIC X                                        
011800                             VALUE SPACE.                                 
011900*                                 SKROTNING BEORDRAD AV ANSK              
012000*                                 SCRAPPING ORDERED BY PROCURER           
012100     03 SLAG-FILL19          PIC X                                        
012200                             VALUE SPACE.                                 
012300     03 SLAG-FLSPBULK        PIC X                                        
012400                             VALUE SPACE.                                 
012500*                                 FLAGGA SPÄRR MOT BULKORDER              
012600*                                 FLAG BULKORDER STOP                     
012700     03 SLAG-FILL20          PIC X                                        
012800                             VALUE SPACE.                                 
012900     03 SLAG-FLWILSON        PIC X                                        
013000                             VALUE SPACE.                                 
013100*                                 WILSONFORMEL                            
013200*                                 FLAG TO USE WILSON OR NOT               
013300     03 SLAG-FILL21          PIC X                                        
013400                             VALUE SPACE.                                 
013500     03 SLAG-IDANSK          PIC Z(2)9                                    
013600                             VALUE ZEROS.                                 
013700*                                 ANSKAFFARNUMMER                         
013800*                                 PROCURER NO.                            
013900     03 SLAG-FILL22          PIC X                                        
014000                             VALUE SPACE.                                 
014100     03 SLAG-IDARTNR-EMBQ0   PIC Z(7)9                                    
014200                             VALUE ZEROS.                                 
014300*                                 EMBALLAGEARTIKELNR FÖR Q0               
014400     03 SLAG-FILL23          PIC X                                        
014500                             VALUE SPACE.                                 
014600     03 SLAG-IDARTNR-EMBQ1   PIC Z(7)9                                    
014700                             VALUE ZEROS.                                 
014800*                                 EMBALLAGEARTIKELNR FÖR Q1               
014900     03 SLAG-FILL24          PIC X                                        
015000                             VALUE SPACE.                                 
015100     03 SLAG-IDARTNR-EMBQ2   PIC Z(7)9                                    
015200                             VALUE ZEROS.                                 
015300*                                 EMBALLAGEARTIKELNR FÖR Q2               
015400     03 SLAG-FILL25          PIC X                                        
015500                             VALUE SPACE.                                 
015600     03 SLAG-IDDC-REF        PIC X(2)                                     
015700                             VALUE SPACES.                                
015800*                                 SÄNDANDE LAGER FÖR REFILL               
015900*                                 SENDING WAREHOUSE FOR REFILL            
016000     03 SLAG-FILL26          PIC X                                        
016100                             VALUE SPACE.                                 
016200     03 SLAG-IDINK           PIC X(4)                                     
016300                             VALUE SPACES.                                
016400*                                 INKÖPARNUMMER                           
016500*                                 PURCHASE IDENTIFICATION NUMBER          
016600     03 SLAG-FILL27          PIC X                                        
016700                             VALUE SPACE.                                 
016800     03 SLAG-IDLANDX2        PIC X(2)                                     
016900                             VALUE SPACES.                                
017000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
017100*                                 2-LETTER CODE FOR COUNTRY               
017200     03 SLAG-FILL28          PIC X                                        
017300                             VALUE SPACE.                                 
017400     03 SLAG-IDLEVNR         PIC X(5)                                     
017500                             VALUE SPACES.                                
017600*                                 LEVERANTÖRNUMMER                        
017700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017800     03 SLAG-FILL29          PIC X                                        
017900                             VALUE SPACE.                                 
018000     03 SLAG-IDPERSON-BUY    PIC Z(2)9                                    
018100                             VALUE ZEROS.                                 
018200*                                 PERSONKOD REFILLANSVARIG                
018300*                                 REFILL RESPONSIBLE ID                   
018400     03 SLAG-FILL30          PIC X                                        
018500                             VALUE SPACE.                                 
018600     03 SLAG-IDPLANGR-AG     PIC 9                                        
018700                             VALUE ZERO.                                  
018800*                                 PLANERINGSGRUPP ANSKAFFARE              
018900     03 SLAG-FILL31          PIC X                                        
019000                             VALUE SPACE.                                 
019100     03 SLAG-IDPSN-DC        PIC 9(3)                                     
019200                             VALUE ZEROS.                                 
019300*                                 PROPER SHIPPING NAME PER XDC            
019400*                                 PROPER SHIPPING NAME XDC                
019500     03 SLAG-FILL32          PIC X                                        
019600                             VALUE SPACE.                                 
019700     03 SLAG-IDREFTAB        PIC X                                        
019800                             VALUE SPACE.                                 
019900*                                 IDENTITET REFILLTABELL                  
020000*                                 REFILLINGTABLE IDENTIFIER               
020100     03 SLAG-FILL33          PIC X                                        
020200                             VALUE SPACE.                                 
020300     03 SLAG-IDUSER-SPKVAL   PIC X(8)                                     
020400                             VALUE SPACES.                                
020500*                                 ANVÄNDAR-ID KVALITETSPÄRR               
020600*                                 USER ID QUALITY ERROR                   
020700     03 SLAG-FILL34          PIC X                                        
020800                             VALUE SPACE.                                 
020900     03 SLAG-KDARTURS        PIC X(2)                                     
021000                             VALUE SPACES.                                
021100*                                 ARTIKELURSPRUNGSKOD                     
021200*                                 COUNTRY OF ORIGIN                       
021300     03 SLAG-FILL35          PIC X                                        
021400                             VALUE SPACE.                                 
021500     03 SLAG-KDAVT           PIC 9                                        
021600                             VALUE ZERO.                                  
021700*                                 AVTALSMÄRKNING                          
021800*                                 AGREEMENT CODE                          
021900     03 SLAG-FILL36          PIC X                                        
022000                             VALUE SPACE.                                 
022100     03 SLAG-KDLEVPLF        PIC X                                        
022200                             VALUE SPACE.                                 
022300*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
022400*                                 CODE FOR APPROVAL OF SCHEDULE P         
022500*                                 ROPOSAL                                 
022600     03 SLAG-FILL37          PIC X                                        
022700                             VALUE SPACE.                                 
022800     03 SLAG-KDLEVSP         PIC Z9                                       
022900                             VALUE ZEROS.                                 
023000*                                 SPÄRRKOD LEVERANS                       
023100*                                 DELIVERY BLOCKING CODE                  
023200     03 SLAG-FILL38          PIC X                                        
023300                             VALUE SPACE.                                 
023400     03 SLAG-KDLPSP          PIC 9                                        
023500                             VALUE ZERO.                                  
023600*                                 LEVERANSPLANESPÄRR                      
023700     03 SLAG-FILL39          PIC X                                        
023800                             VALUE SPACE.                                 
023900     03 SLAG-KDOPPLAN        PIC X                                        
024000                             VALUE SPACE.                                 
024100*                                 OPTIMAL PLAN INOM FRYSTID               
024200*                                 OPTIMAL PLAN WITHIN FREEZTIME           
024300     03 SLAG-FILL40          PIC X                                        
024400                             VALUE SPACE.                                 
024500     03 SLAG-KDREFSTA        PIC X                                        
024600                             VALUE SPACE.                                 
024700*                                 STATUS REFILLARTIKEL                    
024800*                                 STATUS REFILLPART                       
024900     03 SLAG-FILL41          PIC X                                        
025000                             VALUE SPACE.                                 
025100     03 SLAG-KVAKS-PAV       PIC -(7)9                                    
025200                             VALUE ZEROS.                                 
025300*                                 DEL AV AK PÅ VÄG                        
025400*                                 PART OF AK ON ITS WAY                   
025500     03 SLAG-FILL42          PIC X                                        
025600                             VALUE SPACE.                                 
025700     03 SLAG-KVAKS-SDC       PIC -(7)9                                    
025800                             VALUE ZEROS.                                 
025900*                                 DEL AV AK SOM LIGGER I SDC              
026000*                                 PART OF AK IN THE SDC                   
026100     03 SLAG-FILL43          PIC X                                        
026200                             VALUE SPACE.                                 
026300     03 SLAG-KVPBREOI        PIC Z(5)9.9                                  
026400                             VALUE ZEROS.                                 
026500*                                 PERIODBEHOV FÖR REFILL OI               
026600*                                 PERIOD REQUIREM. REFILLING OI           
026700     03 SLAG-FILL44          PIC X                                        
026800                             VALUE SPACE.                                 
026900     03 SLAG-KVBEART         PIC Z(5)9                                    
027000                             VALUE ZEROS.                                 
027100*                                 BESTÄLLT ANTAL STYCKEN                  
027200*                                 ORDERED QUANTITY                        
027300     03 SLAG-FILL45          PIC X                                        
027400                             VALUE SPACE.                                 
027500     03 SLAG-KVDAGAR-CDBEH   PIC Z9                                       
027600                             VALUE ZEROS.                                 
027700*                                 NO. OF DAYS TO BE USED WHEN             
027800*                                 CALCULATING CD REFILLORDERS             
027900     03 SLAG-FILL46          PIC X                                        
028000                             VALUE SPACE.                                 
028100     03 SLAG-KVDAGAR-FFH     PIC Z9                                       
028200                             VALUE ZEROS.                                 
028300*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
028400     03 SLAG-FILL47          PIC X                                        
028500                             VALUE SPACE.                                 
028600     03 SLAG-KVDAGAR-INLEV   PIC Z9                                       
028700                             VALUE ZEROS.                                 
028800*                                 INLEVERANSTID     (ANTAL DAGAR)         
028900     03 SLAG-FILL48          PIC X                                        
029000                             VALUE SPACE.                                 
029100     03 SLAG-KVDAGAR-MANLT   PIC Z(2)9                                    
029200                             VALUE ZEROS.                                 
029300*                                 ANTAL DAGAR                             
029400     03 SLAG-FILL49          PIC X                                        
029500                             VALUE SPACE.                                 
029600     03 SLAG-KVEFRS          PIC -(7)9                                    
029700                             VALUE ZEROS.                                 
029800*                                 EJ FAKTURERAT ANTAL STYCK               
029900*                                 ORDERED NOT INVOICED QTY                
030000     03 SLAG-FILL50          PIC X                                        
030100                             VALUE SPACE.                                 
030200     03 SLAG-KVEOQ           PIC Z(6)9                                    
030300                             VALUE ZEROS.                                 
030400*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
030500*                                 ET                                      
030600     03 SLAG-FILL51          PIC X                                        
030700                             VALUE SPACE.                                 
030800     03 SLAG-KVINVS          PIC Z(6)9                                    
030900                             VALUE ZEROS.                                 
031000*                                 INVENTERINGSSALDO                       
031100*                                 STOCK-TAKING BALANCE                    
031200     03 SLAG-FILL52          PIC X                                        
031300                             VALUE SPACE.                                 
031400     03 SLAG-KVLS            PIC -(7)9                                    
031500                             VALUE ZEROS.                                 
031600*                                 LAGERSALDO                              
031700*                                 STOCK BALANCE                           
031800     03 SLAG-FILL53          PIC X                                        
031900                             VALUE SPACE.                                 
032000     03 SLAG-KVOKS-BULK      PIC -(6)9                                    
032100                             VALUE ZEROS.                                 
032200*                                 ORDERKÖSALDO, KLASS 2-4                 
032300*                                 ORDER QUEUE BALANCE, CLASS 2-4          
032400     03 SLAG-FILL54          PIC X                                        
032500                             VALUE SPACE.                                 
032600     03 SLAG-KVOKS-DAG       PIC -(6)9                                    
032700                             VALUE ZEROS.                                 
032800*                                 ORDERKÖSALDO, KLASS 1                   
032900*                                 ORDER QUEUE BALANCE, CLASS 1            
033000     03 SLAG-FILL55          PIC X                                        
033100                             VALUE SPACE.                                 
033200     03 SLAG-KVPALL          PIC Z(6)9                                    
033300                             VALUE ZEROS.                                 
033400*                                 ANTAL I PALL                            
033500*                                 QUANTITY IN PALLET                      
033600     03 SLAG-FILL56          PIC X                                        
033700                             VALUE SPACE.                                 
033800     03 SLAG-KVPB-PLAN       PIC Z(5)9.9                                  
033900                             VALUE ZEROS.                                 
034000*                                 PLANERAT PERIODBEHOV                    
034100*                                 PLANNED PERIOD REQUIREMENTS             
034200     03 SLAG-FILL57          PIC X                                        
034300                             VALUE SPACE.                                 
034400     03 SLAG-KVPB-REF        PIC Z(5)9.9                                  
034500                             VALUE ZEROS.                                 
034600*                                 PERIODBEHOV REFILLING                   
034700*                                 FORECAST REFILLING                      
034800     03 SLAG-FILL58          PIC X                                        
034900                             VALUE SPACE.                                 
035000     03 SLAG-KVPB-TREND      PIC Z(5)9.9                                  
035100                             VALUE ZEROS.                                 
035200*                                 PERIODTRENDVÄRDE                        
035300     03 SLAG-FILL59          PIC X                                        
035400                             VALUE SPACE.                                 
035500     03 SLAG-KVREFBER        PIC Z(6)9                                    
035600                             VALUE ZEROS.                                 
035700*                                 BERÄKNAD REFILLINGKVANTITET             
035800*                                 CALCULATED REFILLING QUANTITY           
035900     03 SLAG-FILL60          PIC X                                        
036000                             VALUE SPACE.                                 
036100     03 SLAG-KVREFOVL        PIC Z(6)9                                    
036200                             VALUE ZEROS.                                 
036300*                                 BERÄKNAD ÖVERLAGERPUNKT                 
036400*                                 CALCULATED OVERSTOCK POINT              
036500     03 SLAG-FILL61          PIC X                                        
036600                             VALUE SPACE.                                 
036700     03 SLAG-KVREFPKT        PIC Z(6)9                                    
036800                             VALUE ZEROS.                                 
036900*                                 BERÄKNAD PÅFYLLNADSPUNKT                
037000*                                 CALCULATED REFILLING POINT              
037100     03 SLAG-FILL62          PIC X                                        
037200                             VALUE SPACE.                                 
037300     03 SLAG-KVRESS          PIC Z(6)9                                    
037400                             VALUE ZEROS.                                 
037500*                                 RESERVERAT ANTAL ARTIKLAR               
037600*                                 QUANTITY RESERVED ITEMS                 
037700     03 SLAG-FILL63          PIC X                                        
037800                             VALUE SPACE.                                 
037900     03 SLAG-KVRETUR-BEORD   PIC -(6)9                                    
038000                             VALUE ZEROS.                                 
038100*                                 ANTAL SENASTE RETURORDER                
038200*                                 QUANTITY LAST RETURNORDER               
038300     03 SLAG-FILL64          PIC X                                        
038400                             VALUE SPACE.                                 
038500     03 SLAG-KVROS-BULK      PIC -(7)9                                    
038600                             VALUE ZEROS.                                 
038700*                                 RESTORDERSALDO, KLASS 2-4               
038800*                                 BACK ORDER BALANCE, CLASS 2-4           
038900     03 SLAG-FILL65          PIC X                                        
039000                             VALUE SPACE.                                 
039100     03 SLAG-KVROS-DAG       PIC -(7)9                                    
039200                             VALUE ZEROS.                                 
039300*                                 RESTORDERSALDO, KLASS 1                 
039400*                                 BACK ORDER BALANCE, CLASS 1             
039500     03 SLAG-FILL66          PIC X                                        
039600                             VALUE SPACE.                                 
039700     03 SLAG-KVSKROT         PIC Z(6)9                                    
039800                             VALUE ZEROS.                                 
039900*                                 ANTAL SENASTE SKROTORDER                
040000*                                 QUANTITY LAST SCRAPORDER                
040100     03 SLAG-FILL67          PIC X                                        
040200                             VALUE SPACE.                                 
040300     03 SLAG-KVSLAGER        PIC Z(5)9                                    
040400                             VALUE ZEROS.                                 
040500*                                 SÄKERHETSLAGER                          
040600*                                 SAFETY STOCK                            
040700     03 SLAG-FILL68          PIC X                                        
040800                             VALUE SPACE.                                 
040900     03 SLAG-KVSLUTKP        PIC Z(6)9                                    
041000                             VALUE ZEROS.                                 
041100*                                 SLUTKÖPSSALDO                           
041200     03 SLAG-FILL69          PIC X                                        
041300                             VALUE SPACE.                                 
041400     03 SLAG-KVSPANT         PIC -(6)9                                    
041500                             VALUE ZEROS.                                 
041600*                                 SPÄRRAT ANTAL                           
041700*                                 BLOCKED QTY                             
041800     03 SLAG-FILL70          PIC X                                        
041900                             VALUE SPACE.                                 
042000     03 SLAG-KVSPARR-KVAL    PIC Z(6)9                                    
042100                             VALUE ZEROS.                                 
042200*                                 SPÄRRAT ANTAL KVALITETSFEL              
042300*                                 BLOCKED QUANTITY QUALITY ERROR          
042400     03 SLAG-FILL71          PIC X                                        
042500                             VALUE SPACE.                                 
042600     03 SLAG-KVULOAD         PIC Z(6)9                                    
042700                             VALUE ZEROS.                                 
042800*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
042900*                                 MIN LOAD FROM SUPPLIER                  
043000     03 SLAG-FILL72          PIC X                                        
043100                             VALUE SPACE.                                 
043200     03 SLAG-KVUTRS          PIC -(7)9                                    
043300                             VALUE ZEROS.                                 
043400*                                 UTREDNINGSSALDO                         
043500*                                 INVESTIGATION BALANCE                   
043600     03 SLAG-FILL73          PIC X                                        
043700                             VALUE SPACE.                                 
043800     03 SLAG-KVVECKOR-FT     PIC Z9                                       
043900                             VALUE ZEROS.                                 
044000*                                 ANTAL VECKOR FRYSNINGSTID               
044100     03 SLAG-FILL74          PIC X                                        
044200                             VALUE SPACE.                                 
044300     03 SLAG-KVVECKOR-LT     PIC Z9                                       
044400                             VALUE ZEROS.                                 
044500*                                 ANTAL VECKOR LEDTID                     
044600     03 SLAG-FILL75          PIC X                                        
044700                             VALUE SPACE.                                 
044800     03 SLAG-KVVECKOR-TREND  PIC Z9                                       
044900                             VALUE ZEROS.                                 
045000*                                 ANTAL VECKOR TRENDVÄRDE                 
045100     03 SLAG-FILL76          PIC X                                        
045200                             VALUE SPACE.                                 
045300     03 SLAG-PRARTSJK        PIC Z(6)9.9(2)                               
045400                             VALUE ZEROS.                                 
045500*                                 ARTIKELNS SJÄLVKOSTNAD                  
045600*                                 COST OF SALES                           
045700     03 SLAG-FILL77          PIC X                                        
045800                             VALUE SPACE.                                 
045900     03 SLAG-PRAVCOST        PIC Z(6)9.9(2)                               
046000                             VALUE ZEROS.                                 
046100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
046200*                                 AVERAGE COST FOREIGN CURRENCY           
046300     03 SLAG-FILL78          PIC X                                        
046400                             VALUE SPACE.                                 
046500     03 SLAG-RESEASON1       PIC 9.9(2)                                   
046600                             VALUE ZEROS.                                 
046700*                                 SÄSONGSINDEX                            
046800     03 SLAG-FILL79          PIC X                                        
046900                             VALUE SPACE.                                 
047000     03 SLAG-RESEASON2       PIC 9.9(2)                                   
047100                             VALUE ZEROS.                                 
047200*                                 SÄSONGSINDEX                            
047300     03 SLAG-FILL80          PIC X                                        
047400                             VALUE SPACE.                                 
047500     03 SLAG-RESEASON3       PIC 9.9(2)                                   
047600                             VALUE ZEROS.                                 
047700*                                 SÄSONGSINDEX                            
047800     03 SLAG-FILL81          PIC X                                        
047900                             VALUE SPACE.                                 
048000     03 SLAG-RESEASON4       PIC 9.9(2)                                   
048100                             VALUE ZEROS.                                 
048200*                                 SÄSONGSINDEX                            
048300     03 SLAG-FILL82          PIC X                                        
048400                             VALUE SPACE.                                 
048500     03 SLAG-RESEASON5       PIC 9.9(2)                                   
048600                             VALUE ZEROS.                                 
048700*                                 SÄSONGSINDEX                            
048800     03 SLAG-FILL83          PIC X                                        
048900                             VALUE SPACE.                                 
049000     03 SLAG-RESEASON6       PIC 9.9(2)                                   
049100                             VALUE ZEROS.                                 
049200*                                 SÄSONGSINDEX                            
049300     03 SLAG-FILL84          PIC X                                        
049400                             VALUE SPACE.                                 
049500     03 SLAG-RESEASON7       PIC 9.9(2)                                   
049600                             VALUE ZEROS.                                 
049700*                                 SÄSONGSINDEX                            
049800     03 SLAG-FILL85          PIC X                                        
049900                             VALUE SPACE.                                 
050000     03 SLAG-RESEASON8       PIC 9.9(2)                                   
050100                             VALUE ZEROS.                                 
050200*                                 SÄSONGSINDEX                            
050300     03 SLAG-FILL86          PIC X                                        
050400                             VALUE SPACE.                                 
050500     03 SLAG-RESEASON9       PIC 9.9(2)                                   
050600                             VALUE ZEROS.                                 
050700*                                 SÄSONGSINDEX                            
050800     03 SLAG-FILL87          PIC X                                        
050900                             VALUE SPACE.                                 
051000     03 SLAG-RESEASON10      PIC 9.9(2)                                   
051100                             VALUE ZEROS.                                 
051200*                                 SÄSONGSINDEX                            
051300     03 SLAG-FILL88          PIC X                                        
051400                             VALUE SPACE.                                 
051500     03 SLAG-RESEASON11      PIC 9.9(2)                                   
051600                             VALUE ZEROS.                                 
051700*                                 SÄSONGSINDEX                            
051800     03 SLAG-FILL89          PIC X                                        
051900                             VALUE SPACE.                                 
052000     03 SLAG-RESEASON12      PIC 9.9(2)                                   
052100                             VALUE ZEROS.                                 
052200*                                 SÄSONGSINDEX                            
052300     03 SLAG-FILL90          PIC X                                        
052400                             VALUE SPACE.                                 
052500     03 SLAG-RESEASON-PLAN1  PIC 9.9(2)                                   
052600                             VALUE ZEROS.                                 
052700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
052800     03 SLAG-FILL91          PIC X                                        
052900                             VALUE SPACE.                                 
053000     03 SLAG-RESEASON-PLAN2  PIC 9.9(2)                                   
053100                             VALUE ZEROS.                                 
053200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
053300     03 SLAG-FILL92          PIC X                                        
053400                             VALUE SPACE.                                 
053500     03 SLAG-RESEASON-PLAN3  PIC 9.9(2)                                   
053600                             VALUE ZEROS.                                 
053700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
053800     03 SLAG-FILL93          PIC X                                        
053900                             VALUE SPACE.                                 
054000     03 SLAG-RESEASON-PLAN4  PIC 9.9(2)                                   
054100                             VALUE ZEROS.                                 
054200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
054300     03 SLAG-FILL94          PIC X                                        
054400                             VALUE SPACE.                                 
054500     03 SLAG-RESEASON-PLAN5  PIC 9.9(2)                                   
054600                             VALUE ZEROS.                                 
054700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
054800     03 SLAG-FILL95          PIC X                                        
054900                             VALUE SPACE.                                 
055000     03 SLAG-RESEASON-PLAN6  PIC 9.9(2)                                   
055100                             VALUE ZEROS.                                 
055200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
055300     03 SLAG-FILL96          PIC X                                        
055400                             VALUE SPACE.                                 
055500     03 SLAG-RESEASON-PLAN7  PIC 9.9(2)                                   
055600                             VALUE ZEROS.                                 
055700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
055800     03 SLAG-FILL97          PIC X                                        
055900                             VALUE SPACE.                                 
056000     03 SLAG-RESEASON-PLAN8  PIC 9.9(2)                                   
056100                             VALUE ZEROS.                                 
056200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
056300     03 SLAG-FILL98          PIC X                                        
056400                             VALUE SPACE.                                 
056500     03 SLAG-RESEASON-PLAN9  PIC 9.9(2)                                   
056600                             VALUE ZEROS.                                 
056700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
056800     03 SLAG-FILL99          PIC X                                        
056900                             VALUE SPACE.                                 
057000     03 SLAG-RESEASON-PLAN10 PIC 9.9(2)                                   
057100                             VALUE ZEROS.                                 
057200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
057300     03 SLAG-FILL100         PIC X                                        
057400                             VALUE SPACE.                                 
057500     03 SLAG-RESEASON-PLAN11 PIC 9.9(2)                                   
057600                             VALUE ZEROS.                                 
057700*                                 SÄSONGSINDEX INKLUSIVE REFILL           
057800     03 SLAG-FILL101         PIC X                                        
057900                             VALUE SPACE.                                 
058000     03 SLAG-RESEASON-PLAN12 PIC 9.9(2)                                   
058100                             VALUE ZEROS.                                 
058200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
058300     03 SLAG-FILL102         PIC X                                        
058400                             VALUE SPACE.                                 
058500     03 SLAG-RETREND         PIC Z9.9                                     
058600                             VALUE ZEROS.                                 
058700*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
058800*                                 DIFF PREL - AVERAGE FORECAST            
058900     03 SLAG-FILL103         PIC X                                        
059000                             VALUE SPACE.                                 
059100     03 SLAG-TEKVAL          PIC X(10)                                    
059200                             VALUE SPACES.                                
059300*                                 KVALITETSNOTERING SPÄRR                 
059400*                                 QUALITY NOTE BLOCKING                   
059500     03 SLAG-FILL104         PIC X                                        
059600                             VALUE SPACE.                                 
059700     03 SLAG-TIAVCOST        PIC 9(6)                                     
059800                             VALUE ZEROS.                                 
059900*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
060000*                                 AVERAGE COST CALCULATION DATE           
060100     03 SLAG-FILL105         PIC X                                        
060200                             VALUE SPACE.                                 
060300     03 SLAG-TIDATUM-TREND   PIC 9(6)                                     
060400                             VALUE ZEROS.                                 
060500*                                 JUSTERAD TREND AAMMDD                   
060600*                                 LAST TREND CHANGE  YYMMDD               
060700     03 SLAG-FILL106         PIC X                                        
060800                             VALUE SPACE.                                 
060900     03 SLAG-TIINVDAT        PIC 9(5)                                     
061000                             VALUE ZEROS.                                 
061100*                                 INVENTERINGSDATUM                       
061200*                                 STOCKTAKING DATE                        
061300     03 SLAG-FILL107         PIC X                                        
061400                             VALUE SPACE.                                 
061500     03 SLAG-TIINVDAG1       PIC 9                                        
061600                             VALUE ZERO.                                  
061700*                                 AVSÄNDNINGSDAG INOM VECKA               
061800*                                 DELIVERY WEEK DAY                       
061900     03 SLAG-FILL108         PIC X                                        
062000                             VALUE SPACE.                                 
062100     03 SLAG-TIINVDAG2       PIC 9                                        
062200                             VALUE ZERO.                                  
062300*                                 AVSÄNDNINGSDAG INOM VECKA               
062400*                                 DELIVERY WEEK DAY                       
062500     03 SLAG-FILL109         PIC X                                        
062600                             VALUE SPACE.                                 
062700     03 SLAG-TIINVDAG3       PIC 9                                        
062800                             VALUE ZERO.                                  
062900*                                 AVSÄNDNINGSDAG INOM VECKA               
063000*                                 DELIVERY WEEK DAY                       
063100     03 SLAG-FILL110         PIC X                                        
063200                             VALUE SPACE.                                 
063300     03 SLAG-TIINVDAG4       PIC 9                                        
063400                             VALUE ZERO.                                  
063500*                                 AVSÄNDNINGSDAG INOM VECKA               
063600*                                 DELIVERY WEEK DAY                       
063700     03 SLAG-FILL111         PIC X                                        
063800                             VALUE SPACE.                                 
063900     03 SLAG-TIINVDAG5       PIC 9                                        
064000                             VALUE ZERO.                                  
064100*                                 AVSÄNDNINGSDAG INOM VECKA               
064200*                                 DELIVERY WEEK DAY                       
064300     03 SLAG-FILL112         PIC X                                        
064400                             VALUE SPACE.                                 
064500     03 SLAG-TILPSP          PIC 9(4)                                     
064600                             VALUE ZEROS.                                 
064700*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
064800     03 SLAG-FILL113         PIC X                                        
064900                             VALUE SPACE.                                 
065000     03 SLAG-TIMANSEA        PIC 9(6)                                     
065100                             VALUE ZEROS.                                 
065200*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
065300*                                 DATE FOR MANUAL SEASON (YYMMDD)         
065400     03 SLAG-FILL114         PIC X                                        
065500                             VALUE SPACE.                                 
065600     03 SLAG-TIOMSPEC        PIC 9(4)                                     
065700                             VALUE ZEROS.                                 
065800*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
065900     03 SLAG-FILL115         PIC X                                        
066000                             VALUE SPACE.                                 
066100     03 SLAG-TIORDREG        PIC 9(6)                                     
066200                             VALUE ZEROS.                                 
066300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
066400*                                 ORDER REGISTRATION DATE  YYMMDD         
066500     03 SLAG-FILL116         PIC X                                        
066600                             VALUE SPACE.                                 
066700     03 SLAG-TIPBREOI        PIC 9(6)                                     
066800                             VALUE ZEROS.                                 
066900*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
067000*                                 DATE MAN. FC REFILL OI (YYMMDD)         
067100     03 SLAG-FILL117         PIC X                                        
067200                             VALUE SPACE.                                 
067300     03 SLAG-TIREFMPB        PIC 9(6)                                     
067400                             VALUE ZEROS.                                 
067500*                                 DATUM MANUELL PROGNOS REFILLING         
067600*                                 DATE MANUAL FORECAST REFILLING          
067700     03 SLAG-FILL118         PIC X                                        
067800                             VALUE SPACE.                                 
067900     03 SLAG-TIREFPAF        PIC 9(6)                                     
068000                             VALUE ZEROS.                                 
068100*                                 DATUM MANUELL PÅFYLLNADSKVANT           
068200*                                 DATE MANUAL REFILLING QTY               
068300     03 SLAG-FILL119         PIC X                                        
068400                             VALUE SPACE.                                 
068500     03 SLAG-TIREFPKT        PIC 9(6)                                     
068600                             VALUE ZEROS.                                 
068700*                                 DATUM MANUELL REFILLPUNKT               
068800*                                 DATE MANUAL REFILLING POINT             
068900     03 SLAG-FILL120         PIC X                                        
069000                             VALUE SPACE.                                 
069100     03 SLAG-TIREFSTA        PIC 9(6)                                     
069200                             VALUE ZEROS.                                 
069300*                                 DATUM AKT/PASS REFILLARTIKEL            
069400*                                 DATE ACT/PASS REFILLPART                
069500     03 SLAG-FILL121         PIC X                                        
069600                             VALUE SPACE.                                 
069700     03 SLAG-TIREFSTO        PIC 9(6)                                     
069800                             VALUE ZEROS.                                 
069900*                                 BEORDRINGSSTOPPAD T.OM.                 
070000*                                 STOPPED FOR ORDERING UNTIL              
070100     03 SLAG-FILL122         PIC X                                        
070200                             VALUE SPACE.                                 
070300     03 SLAG-TIREFSTO-LOC    PIC 9(6)                                     
070400                             VALUE ZEROS.                                 
070500*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
070600*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
070700     03 SLAG-FILL123         PIC X                                        
070800                             VALUE SPACE.                                 
070900     03 SLAG-TIRETUR-BEORD   PIC 9(6)                                     
071000                             VALUE ZEROS.                                 
071100*                                 DATUM RETUR BEORDRING                   
071200*                                 DATE ISSUE OF RETURN ORDER              
071300     03 SLAG-FILL124         PIC X                                        
071400                             VALUE SPACE.                                 
071500     03 SLAG-TISKROT         PIC 9(6)                                     
071600                             VALUE ZEROS.                                 
071700*                                 SKROTNINGSDATUM                         
071800*                                 DATE OF SCRAPPING                       
071900     03 SLAG-FILL125         PIC X                                        
072000                             VALUE SPACE.                                 
072100     03 SLAG-TISKROT-BEORD   PIC 9(6)                                     
072200                             VALUE ZEROS.                                 
072300*                                 BEORDRAD SKROTNINGSDATUM                
072400*                                 DATE OF SCRAPPING DECISION              
072500     03 SLAG-FILL126         PIC X                                        
072600                             VALUE SPACE.                                 
072700     03 SLAG-TISLUTKP        PIC Z(6)                                     
072800                             VALUE ZEROS.                                 
072900*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
073000*                                 CALC OF ATR-BAL IS TO COMMENCE          
073100     03 SLAG-FILL127         PIC X                                        
073200                             VALUE SPACE.                                 
073300     03 SLAG-TISPARR-KVAL    PIC 9(6)                                     
073400                             VALUE ZEROS.                                 
073500*                                 SPÄRRAD DATUM KVALITETSFEL              
073600*                                 BLOCKED DATE QUALITY ERROR              
073700     03 SLAG-FILL128         PIC X                                        
073800                             VALUE SPACE.                                 
073900     03 SLAG-VKART           PIC Z(6)9                                    
074000                             VALUE ZEROS.                                 
074100*                                 ARTIKELVIKT (G)                         
074200*                                 PART WEIGHT (G)                         
074300     03 SLAG-FILL129         PIC X                                        
074400                             VALUE SPACE.                                 
074500     03 SLAG-VLARTNTO        PIC Z(7)9.9                                  
074600                             VALUE ZEROS.                                 
074700*                                 ARTIKELVOLYM (CM3)                      
074800*                                 PART VOLUME    (CM3)                    
074900     03 SLAG-FILL130         PIC X                                        
075000                             VALUE SPACE.                                 
075100     03 SLAG-IDLEVNR-SHIP    PIC X(5)                                     
075200                             VALUE SPACES.                                
075300*                                 SKEPPANDE LEVERANTÖR                    
075400*                                 SHIPPING SUPPLIER                       
075500     03 SLAG-FILL131         PIC X                                        
075600                             VALUE SPACE.                                 
075700     03 SLAG-PRMATRL         PIC Z(6)9.9(2)                               
075800                             VALUE ZEROS.                                 
075900*                                 FAST PRIS UNDER LÖPANDE ÅR              
076000*                                 MATERIAL PRICE FOR ACTUAL YEAR          
076100     03 SLAG-FILL132         PIC X                                        
076200                             VALUE SPACE.                                 
076300     03 SLAG-TIERSDAT-VIPS   PIC 9(5)                                     
076400                             VALUE ZEROS.                                 
076500*                                 DATUM NÄR ERS. INFO TILL VIPS           
076600*                                 SEND DATE OF SUPERS. TO VIPS            
076700     03 SLAG-FILL133         PIC X                                        
076800                             VALUE SPACE.                                 
076900     03 SLAG-TIMANSEC        PIC 9(6)                                     
077000                             VALUE ZEROS.                                 
077100*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
077200*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
077300     03 SLAG-FILL134         PIC X                                        
077400                             VALUE SPACE.                                 
077500     03 SLAG-KDMATRPR        PIC X                                        
077600                             VALUE SPACE.                                 
077700*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
077800*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
077900     03 SLAG-FILL135         PIC X                                        
078000                             VALUE SPACE.                                 
078100     03 SLAG-KVPB-JUST1      PIC Z(5)9.9                                  
078200                             VALUE ZEROS.                                 
078300*                                 PERIODBEHOVSJUSTERING-1                 
078400*                                 PERIOD REQUIREMENTS-1                   
078500     03 SLAG-FILL136         PIC X                                        
078600                             VALUE SPACE.                                 
078700     03 SLAG-TIPBJUST-1      PIC 9(4)                                     
078800                             VALUE ZEROS.                                 
078900*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
079000*                                 FIRST PB-JUST DATE YYWW                 
079100     03 SLAG-FILL137         PIC X                                        
079200                             VALUE SPACE.                                 
079300     03 SLAG-KVPB-JUST2      PIC Z(5)9.9                                  
079400                             VALUE ZEROS.                                 
079500*                                 PERIODBEHOVSJUSTERING-2                 
079600*                                 PERIOD REQUIREMENTS-2                   
079700     03 SLAG-FILL138         PIC X                                        
079800                             VALUE SPACE.                                 
079900     03 SLAG-TIPBJUST-2      PIC 9(4)                                     
080000                             VALUE ZEROS.                                 
080100*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
080200*                                 SECOND PB-JUST DATE YYWW                
080300     03 SLAG-FILL139         PIC X                                        
080400                             VALUE SPACE.                                 
080500     03 SLAG-IDLEVNR-FRAM    PIC X(5)                                     
080600                             VALUE SPACES.                                
080700*                                 FRAMTIDA LEVERANTÖRNUMMER               
080800*                                 THE SUPPLIER NAME IN THE FUTURE         
080900     03 SLAG-FILL140         PIC X                                        
081000                             VALUE SPACE.                                 
081100     03 SLAG-IDLEVNR-SHIP-FRAM                                            
081200                             PIC X(5)                                     
081300                             VALUE SPACES.                                
081400*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
081500*                                 THE SHIP.SUPPLIER IN THE FUTURE         
081600     03 SLAG-FILL141         PIC X                                        
081700                             VALUE SPACE.                                 
081800     03 SLAG-TILEVDAT        PIC 9(6)                                     
081900                             VALUE ZEROS.                                 
082000*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
082100*                                 DATE FOR FUTURE SUPPLIER                
082200     03 SLAG-FILL142         PIC X                                        
082300                             VALUE SPACE.                                 
082400     03 SLAG-TIMANLED        PIC 9(6)                                     
082500                             VALUE ZEROS.                                 
082600*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
082700*                                 END DATE MAN.LEAD TIME (YYMMDD)         
082800     03 SLAG-FILL143         PIC X                                        
082900                             VALUE SPACE.                                 
083000     03 SLAG-TISTODAT-LARM   PIC 9(6)                                     
083100                             VALUE ZEROS.                                 
083200*                                 STOPPDATUM FÖR LARM-223                 
083300*                                 STOP DATE FOR ALARM-223                 
083400     03 SLAG-FILL144         PIC X                                        
083500                             VALUE SPACE.                                 
083600     03 SLAG-FLREFILL        PIC X                                        
083700                             VALUE SPACE.                                 
083800*                                 REFILLARTIKEL                           
083900*                                 REFILLPART                              
084000     03 SLAG-FILL145         PIC X                                        
084100                             VALUE SPACE.                                 
084200     03 SLAG-FLREFNYO        PIC X                                        
084300                             VALUE SPACE.                                 
084400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
084500*                                 WAIT FOR NEXT DEMAND                    
084600     03 SLAG-FILL146         PIC X                                        
084700                             VALUE SPACE.                                 
084800     03 SLAG-TISKROT-AUTO    PIC 9(6)                                     
084900                             VALUE ZEROS.                                 
085000*                                 STOPDATE AUTO-SKROTNING                 
085100*                                 STOP DATE AUTOSCRAPPING                 
085200     03 SLAG-FILL147         PIC X                                        
085300                             VALUE SPACE.                                 
085400     03 SLAG-TIUPPDAT-EMB    PIC 9(6)                                     
085500                             VALUE ZEROS.                                 
085600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
085700*                                 UPDATING DATE     (YYMMDD)              
085800     03 SLAG-FILL148         PIC X                                        
085900                             VALUE SPACE.                                 
086000     03 SLAG-FLBUYUPD        PIC X                                        
086100                             VALUE SPACE.                                 
086200*                                 OM IDPERSONKOD ÄR LÅST                  
086300*                                 IF BUYER UPDATE IS LOCKED               
086400     03 SLAG-FILL149         PIC X                                        
086500                             VALUE SPACE.                                 
086600     03 SLAG-FLTABUPD        PIC X                                        
086700                             VALUE SPACE.                                 
086800*                                 OM REFILLTABELL ÄR LÅST                 
086900*                                 IF REFILLINGTABLE UPDATE LOCKED         
087000     03 SLAG-FILL150         PIC X                                        
087100                             VALUE SPACE.                                 
087200     03 SLAG-REPPFAKT        PIC 9.9(2)                                   
087300                             VALUE ZEROS.                                 
087400*                                 PREPLANNED FACTOR                       
087500     03 SLAG-FILL151         PIC X                                        
087600                             VALUE SPACE.                                 
087700     03 SLAG-FLPB-FLYTT      PIC X                                        
087800                             VALUE SPACE.                                 
087900*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
088000*                                  REDA PÅ KOPIERING AV PROGNOS           
088100*                                 FLAG                                    
088200     03 SLAG-FILL152         PIC X                                        
088300                             VALUE SPACE.                                 
088400     03 SLAG-FLLARM-BUF      PIC X                                        
088500                             VALUE SPACE.                                 
088600*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
088700     03 SLAG-FILL153         PIC X                                        
088800                             VALUE SPACE.                                 
088900*** END OF VILMAII-COPY LENGTH= 865 BYTES                                 
