000100 01  WDK7-W005WDK7.                                                       
000200*                                 PARAMETRAR TILL W005WDK7-PGM            
000300*                                 UPPLÄGG AV WDK7-SEGMENT                 
000400*                                 ENLIGT "IDSEGM"(ETT SEGM/ANROP)         
000500*                                 EXEMPEL PÅ ANROP:                       
000600*                                 MOVE ALL "+"  TO WDK7-W005WDK7          
000700*                                 MOVE "WDK7NN" TO WDK7-IDSEGM            
000800*                                 MOVE NN    TO WDK7-IDARTNR-KFB          
000900*                                      OCH                                
001000*                                 MOVE XX    TO WDK7-IDDC-KFB             
001100*                                      ELLER:   WDK7-IDLANDX2-KFB         
001200*                                 MOVE VÄRDEN TO WDK7-AREOR               
001300*                                 CALL W005WDK7 USING                     
001400*                                               WDK7-W005WDK7             
001500*                                               WDB6-PCB                  
001600*                                               WDK6-PCB                  
001700*                                               WDK7-PCB                  
001800*                                 -EFTER ANROPET BRA ATT FLYTTA:          
001900*                                  WDK7-WDK7XX TILL HUVUD-PGM IO:         
002000*                                  EX: WDK7-WDK711 TO SLAG-WDK711         
002100*                                 OBS!! OBLIGATORISKA KFB-FÄLT:           
002200*                                 - NYCKLAR I FB-FÄLT TILL ALLA           
002300*                                   PARENT SEGM FÖR "IDSEGM"              
002400*                                   EXEMPEL:                              
002500*                                   A) "IDSEGM" ÄR WDK722:                
002600*                                       WDK7-IDARTNR-KFB +                
002700*                                       WDK7-IDDC-KFB                     
002800*                                   B) "IDSEGM" ÄR WDK726:                
002900*                                       WDK7-IDARTNR-KFB +                
003000*                                       WDK7-IDLANDX2-KFB                 
003100*                                 - NYCKELFÄLT I IO-AREAN                 
003200*                                   EX: WDK7-IDDC ELLER                   
003300*                                       WDK7-IDLANDX2, OSV.               
003400*                                 -------------------------------         
003500*                                 PARAMETERS TO W005WDK7-PGM              
003600*                                 CREATES WDK7-SEGMENT ACCORDING          
003700*                                 TO "IDSEGM" (ONE SEGM/CALL)             
003800*                                 EXAMPLE OF CALL:                        
003900*                                 MOVE ALL "+"  TO WDK7-W005WDK7          
004000*                                 MOVE "WDK7NN" TO WDK7-IDSEGM            
004100*                                 MOVE NN   TO WDK7-IDARTNR-KFB           
004200*                                      AND                                
004300*                                 MOVE XX   TO WDK7-IDDC-KFB              
004400*                                      OR:     WDK7-IDLANDX2-KFB          
004500*                                 MOVE VALUES TO WDK7-AREAS               
004600*                                 CALL W005WDK7 USING                     
004700*                                               WDK7-W005WDK7             
004800*                                               WDB6-PCB                  
004900*                                               WDK6-PCB                  
005000*                                               WDK7-PCB                  
005100*                                 -AFTER THE CALL GOOD TO MOVE:           
005200*                                  WDK7-WDK7XX TILL MAIN-PGMS IO:         
005300*                                  EX: WDK7-WDK711 TO SLAG-WDK711         
005400*                                 NOTE!! MANDATORY KFB-FIELDS:            
005500*                                 - KEYS IN FB-FIELDS TO ALL              
005600*                                   PARENT SEGM OF "IDSEGM"               
005700*                                   EXEMPEL:                              
005800*                                   A) "IDSEGM" IS WDK722:                
005900*                                       WDK7-IDARTNR-KFB +                
006000*                                       WDK7-IDDC-KFB                     
006100*                                   B) "IDSEGM" IS WDK726:                
006200*                                       WDK7-IDARTNR-KFB +                
006300*                                       WDK7-IDLANDX2-KFB                 
006400*                                 - KEY FIELDS IN IO-AREA                 
006500*                                   EX: WDK7-IDDC OR                      
006600*                                       WDK7-IDLANDX2, ETC.               
006700*                                 -------------------------------         
006800     03 WDK7-IDSEGM          PIC X(6).                                    
006900*                                 SEGMENT                                 
007000     03 WDK7-IDARTNR-KFB     PIC S9(9)           COMP-3.                  
007100*                                 ARTIKELNUMMER                           
007200*                                 PART NUMBER                             
007300     03 WDK7-IDDC-KFB        PIC X(2).                                    
007400*                                 IDENTIFIERARE LAGER                     
007500*                                 WAREHOUSE IDENTIFIER                    
007600     03 WDK7-IDLANDX2-KFB    PIC X(2).                                    
007700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
007800*                                 2-LETTER CODE FOR COUNTRY               
007900     03 WDK7-WDK711.                                                      
008000*                                 S-LAGER ARTIKEL REGISTER                
008100*                                 LAGERINFORMATION                        
008200*                                 FYSISK NYCKEL: IDDC                     
008300        05 WDK7-IDDC         PIC X(2).                                    
008400*                                 IDENTIFIERARE LAGER                     
008500*                                 WAREHOUSE IDENTIFIER                    
008600        05 WDK7-ADART.                                                    
008700*                                 ARTIKELADRESS I LAGRET                  
008800*                                 PARTS-ADRESS                            
008900           07 WDK7-ADLAGOMR  PIC S9(3)           COMP-3.                  
009000*                                 LAGEROMRÅDE                             
009100*                                 AREA                                    
009200           07 WDK7-ADGANG    PIC S9(3)           COMP-3.                  
009300*                                 GÅNG                                    
009400*                                 AISLE                                   
009500           07 WDK7-ADPLATS   PIC S9(5)           COMP-3.                  
009600*                                 LAGERPLATSNUMMER                        
009700*                                 LOCATION                                
009800        05 WDK7-ADLAGOMR-CD  PIC S9(3)           COMP-3.                  
009900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
010000*                                 AREA ADDRESS CROSS DOCKING WARE         
010100*                                 HOUSE                                   
010200        05 WDK7-DAREFESC     PIC 9(8).                                    
010300*                                 DATUM ESCLÅSTPROGNOS REFILLING          
010400*                                 DATE ESCLOCKFORECAST REFILLING          
010500        05 WDK7-DASPSEA      PIC 9(8).                                    
010600*                                 SÄSONG SPÄRRAD TOM  ÅÅÅÅMMDD            
010700*                                 DATE SEASON BLOCKED TO YYYYMMDD         
010800        05 WDK7-FLCDCBEH     PIC X.                                       
010900*                                 REFILLBEHOV                             
011000*                                 REFILL NEEDED                           
011100        05 WDK7-FLCDREL      PIC X.                                       
011200*                                 OMGÅENDE RELEASE AV CD-REFILL           
011300*                                 IMMEDIATE RELEASE OF CD-REFILL          
011400        05 WDK7-FLFLYG       PIC X.                                       
011500*                                 FLYGARTIKEL                             
011600*                                 PART NUMBER SENT BY AIR                 
011700        05 WDK7-FLORDSP      PIC X.                                       
011800*                                 ORDERSPÄRR                              
011900*                                 ORDER BLOCKED                           
012000        05 WDK7-FLORDSP-EJRO PIC X.                                       
012100*                                 ORDERSPÄRR EJ RESTNOTERING              
012200*                                 ORDER BLOCK NO BACKORDERING             
012300        05 WDK7-FLPB-FLYTT   PIC X.                                       
012400*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
012500*                                  REDA PÅ KOPIERING AV PROGNOS           
012600*                                 FLAG                                    
012700        05 WDK7-FLREFBEO     PIC X.                                       
012800*                                 AUTOMATISK REFILL BEORDRING?            
012900*                                 AUTOMATIC REFILL ORDERING?              
013000        05 WDK7-FLREFLARM    PIC X.                                       
013100*                                 ONORMAL ORDERINGÅNG                     
013200*                                 ABNORMAL SALES                          
013300        05 WDK7-FLSKROT-AUTO PIC X.                                       
013400*                                 SKROTNING AUTOMATISKT BEORDRAD          
013500*                                 SCRAPPING AUTOMATICALLY ORDERED         
013600        05 WDK7-FLSKROT-BEORD                                             
013700                             PIC X.                                       
013800*                                 SKROTNING BEORDRAD AV ANSK              
013900*                                 SCRAPPING ORDERED BY PROCURER           
014000        05 WDK7-FLSPBULK     PIC X.                                       
014100*                                 FLAGGA SPÄRR MOT BULKORDER              
014200*                                 FLAG BULKORDER STOP                     
014300        05 WDK7-FLWILSON     PIC X.                                       
014400*                                 WILSONFORMEL                            
014500*                                 FLAG TO USE WILSON OR NOT               
014600        05 WDK7-IDDC-REF     PIC X(2).                                    
014700*                                 SÄNDANDE LAGER FÖR REFILL               
014800*                                 SENDING WAREHOUSE FOR REFILL            
014900        05 WDK7-IDLEVNR      PIC X(5).                                    
015000*                                 LEVERANTÖRNUMMER                        
015100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
015200        05 WDK7-FILLERX1     PIC X.                                       
015300        05 WDK7-FLBUYUPD     PIC X.                                       
015400*                                 OM IDPERSONKOD ÄR LÅST                  
015500*                                 IF BUYER UPDATE IS LOCKED               
015600        05 WDK7-FLTABUPD     PIC X.                                       
015700*                                 OM REFILLTABELL ÄR LÅST                 
015800*                                 IF REFILLINGTABLE UPDATE LOCKED         
015900        05 WDK7-IDPERSON-BUY PIC S9(3)           COMP-3.                  
016000*                                 PERSONKOD REFILLANSVARIG                
016100*                                 REFILL RESPONSIBLE ID                   
016200        05 WDK7-IDREFTAB     PIC X.                                       
016300*                                 IDENTITET REFILLTABELL                  
016400*                                 REFILLINGTABLE IDENTIFIER               
016500        05 WDK7-IDUSER-SPKVAL                                             
016600                             PIC X(8).                                    
016700*                                 ANVÄNDAR-ID KVALITETSPÄRR               
016800*                                 USER ID QUALITY ERROR                   
016900        05 WDK7-KDLEVSP      PIC S9(3)           COMP-3.                  
017000*                                 SPÄRRKOD LEVERANS                       
017100*                                 DELIVERY BLOCKING CODE                  
017200        05 WDK7-KDREFSTA     PIC X.                                       
017300*                                 STATUS REFILLARTIKEL                    
017400*                                 STATUS REFILLPART                       
017500        05 WDK7-KVAKS-PAV    PIC S9(7)           COMP-3.                  
017600*                                 DEL AV AK PÅ VÄG                        
017700*                                 PART OF AK ON ITS WAY                   
017800        05 WDK7-KVAKS-SDC    PIC S9(7)           COMP-3.                  
017900*                                 DEL AV AK SOM LIGGER I SDC              
018000*                                 PART OF AK IN THE SDC                   
018100        05 WDK7-KVBEART      PIC S9(7)           COMP-3.                  
018200*                                 BESTÄLLT ANTAL STYCKEN                  
018300*                                 ORDERED QUANTITY                        
018400        05 WDK7-KVDAGAR-CDBEH                                             
018500                             PIC S9(3)           COMP-3.                  
018600*                                 NO. OF DAYS TO BE USED WHEN             
018700*                                 CALCULATING CD REFILLORDERS             
018800        05 WDK7-KVDAGAR-MANLT                                             
018900                             PIC S9(3)           COMP-3.                  
019000*                                 ANTAL DAGAR                             
019100        05 WDK7-KVEFRS       PIC S9(7)           COMP-3.                  
019200*                                 EJ FAKTURERAT ANTAL STYCK               
019300*                                 ORDERED NOT INVOICED QTY                
019400        05 WDK7-KVINVS       PIC S9(7)           COMP-3.                  
019500*                                 INVENTERINGSSALDO                       
019600*                                 STOCK-TAKING BALANCE                    
019700        05 WDK7-KVLS         PIC S9(7)           COMP-3.                  
019800*                                 LAGERSALDO                              
019900*                                 STOCK BALANCE                           
020000        05 WDK7-KVOKS-BULK   PIC S9(7)           COMP-3.                  
020100*                                 ORDERKÖSALDO, KLASS 2-4                 
020200*                                 ORDER QUEUE BALANCE, CLASS 2-4          
020300        05 WDK7-KVOKS-DAG    PIC S9(7)           COMP-3.                  
020400*                                 ORDERKÖSALDO, KLASS 1                   
020500*                                 ORDER QUEUE BALANCE, CLASS 1            
020600        05 WDK7-KVPB-HIST    PIC S9(6)V9(1)      COMP-3.                  
020700*                                 PB (PROGNOS) HISTORISKT CDC             
020800*                                 HISTORIC REQUIREMENTS CDC               
020900        05 WDK7-KVPB-REF     PIC S9(6)V9(1)      COMP-3.                  
021000*                                 PERIODBEHOV REFILLING                   
021100*                                 FORECAST REFILLING                      
021200        05 WDK7-KVPBREOI     PIC S9(6)V9(1)      COMP-3.                  
021300*                                 PERIODBEHOV FÖR REFILL OI               
021400*                                 PERIOD REQUIREM. REFILLING OI           
021500        05 WDK7-KVPBREOI-HIST                                             
021600                             PIC S9(6)V9(1)      COMP-3.                  
021700*                                 HIST MANUELT PB FÖR REFILL OI           
021800*                                 HIST MANUAL PER REQ REFILL OI           
021900        05 WDK7-KVREFBER     PIC S9(7)           COMP-3.                  
022000*                                 BERÄKNAD REFILLINGKVANTITET             
022100*                                 CALCULATED REFILLING QUANTITY           
022200        05 WDK7-KVREFOVL     PIC S9(7)           COMP-3.                  
022300*                                 BERÄKNAD ÖVERLAGERPUNKT                 
022400*                                 CALCULATED OVERSTOCK POINT              
022500        05 WDK7-KVREFPKT     PIC S9(7)           COMP-3.                  
022600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
022700*                                 CALCULATED REFILLING POINT              
022800        05 WDK7-KVRESS       PIC S9(7)           COMP-3.                  
022900*                                 RESERVERAT ANTAL ARTIKLAR               
023000*                                 QUANTITY RESERVED ITEMS                 
023100        05 WDK7-KVRETUR-BEORD                                             
023200                             PIC S9(7)           COMP-3.                  
023300*                                 ANTAL SENASTE RETURORDER                
023400*                                 QUANTITY LAST RETURNORDER               
023500        05 WDK7-KVROS-BULK   PIC S9(7)           COMP-3.                  
023600*                                 RESTORDERSALDO, KLASS 2-4               
023700*                                 BACK ORDER BALANCE, CLASS 2-4           
023800        05 WDK7-KVROS-DAG    PIC S9(7)           COMP-3.                  
023900*                                 RESTORDERSALDO, KLASS 1                 
024000*                                 BACK ORDER BALANCE, CLASS 1             
024100        05 WDK7-KVSKROT      PIC S9(7)           COMP-3.                  
024200*                                 ANTAL SENASTE SKROTORDER                
024300*                                 QUANTITY LAST SCRAPORDER                
024400        05 WDK7-KVSPARR-KVAL PIC S9(7)           COMP-3.                  
024500*                                 SPÄRRAT ANTAL KVALITETSFEL              
024600*                                 BLOCKED QUANTITY QUALITY ERROR          
024700        05 WDK7-KVUTRS       PIC S9(7)           COMP-3.                  
024800*                                 UTREDNINGSSALDO                         
024900*                                 INVESTIGATION BALANCE                   
025000        05 WDK7-PRAVCOST     PIC S9(7)V9(2)      COMP-3.                  
025100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
025200*                                 AVERAGE COST FOREIGN CURRENCY           
025300        05 WDK7-RESEASON     OCCURS 12 TIMES                              
025400                             PIC S9V9(2)         COMP-3.                  
025500*                                 SÄSONGSINDEX                            
025600        05 WDK7-RETREND      PIC S9(2)V9(1)      COMP-3.                  
025700*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
025800*                                 DIFF PREL - AVERAGE FORECAST            
025900        05 WDK7-TEKVAL       PIC X(10).                                   
026000*                                 KVALITETSNOTERING SPÄRR                 
026100*                                 QUALITY NOTE BLOCKING                   
026200        05 WDK7-TIAVCOST     PIC S9(7)           COMP-3.                  
026300*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
026400*                                 AVERAGE COST CALCULATION DATE           
026500        05 WDK7-TIINVDAT     PIC S9(5)           COMP-3.                  
026600*                                 INVENTERINGSDATUM                       
026700*                                 STOCKTAKING DATE                        
026800        05 WDK7-TIMANSEA     PIC 9(6).                                    
026900*                                 DATUM MANUELL SÄSONG (ÅÅMMDD)           
027000*                                 DATE FOR MANUAL SEASON (YYMMDD)         
027100        05 WDK7-TIORDREG     PIC S9(7)           COMP-3.                  
027200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
027300*                                 ORDER REGISTRATION DATE  YYMMDD         
027400        05 WDK7-TIREFMPB     PIC S9(7)           COMP-3.                  
027500*                                 DATUM MANUELL PROGNOS REFILLING         
027600*                                 DATE MANUAL FORECAST REFILLING          
027700        05 WDK7-TIREFPAF     PIC S9(7)           COMP-3.                  
027800*                                 DATUM MANUELL PÅFYLLNADSKVANT           
027900*                                 DATE MANUAL REFILLING QTY               
028000        05 WDK7-TIREFPKT     PIC S9(7)           COMP-3.                  
028100*                                 DATUM MANUELL REFILLPUNKT               
028200*                                 DATE MANUAL REFILLING POINT             
028300        05 WDK7-TIREFSTA     PIC S9(7)           COMP-3.                  
028400*                                 DATUM AKT/PASS REFILLARTIKEL            
028500*                                 DATE ACT/PASS REFILLPART                
028600        05 WDK7-TIREFSTO     PIC S9(7)           COMP-3.                  
028700*                                 BEORDRINGSSTOPPAD T.OM.                 
028800*                                 STOPPED FOR ORDERING UNTIL              
028900        05 WDK7-TIRETUR-BEORD                                             
029000                             PIC S9(7)           COMP-3.                  
029100*                                 DATUM RETUR BEORDRING                   
029200*                                 DATE ISSUE OF RETURN ORDER              
029300        05 WDK7-TISKROT      PIC S9(7)           COMP-3.                  
029400*                                 SKROTNINGSDATUM                         
029500*                                 DATE OF SCRAPPING                       
029600        05 WDK7-TISKROT-AUTO PIC S9(7)           COMP-3.                  
029700*                                 STOPDATE AUTO-SKROTNING                 
029800*                                 STOP DATE AUTOSCRAPPING                 
029900        05 WDK7-TISKROT-BEORD                                             
030000                             PIC S9(7)           COMP-3.                  
030100*                                 BEORDRAD SKROTNINGSDATUM                
030200*                                 DATE OF SCRAPPING DECISION              
030300        05 WDK7-TISPARR-KVAL PIC 9(6).                                    
030400*                                 SPÄRRAD DATUM KVALITETSFEL              
030500*                                 BLOCKED DATE QUALITY ERROR              
030600        05 WDK7-TIPBREOI     PIC 9(6).                                    
030700*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
030800*                                 DATE MAN. FC REFILL OI (YYMMDD)         
030900        05 WDK7-DAREFESC-REOI                                             
031000                             PIC 9(8).                                    
031100*                                 DATUM ESCLÅSTPROGNOS KVPBREOI           
031200*                                 DATE ESCLOCKFORECAST KVPBREOI           
031300        05 WDK7-RETREND-REOI PIC S9(2)V9(1)      COMP-3.                  
031400*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
031500*                                 DIFF PREL - AVERAGE FORECAST            
031600        05 WDK7-TIDATUM-CROSS                                             
031700                             PIC S9(7)           COMP-3.                  
031800*                                 STARTDAG CROSS DOCKING FÖRDRÖJN         
031900*                                 START DATE CROSS DELAY CALCULAT         
032000        05 WDK7-TISTODAT-LARM                                             
032100                             PIC S9(7)           COMP-3.                  
032200*                                 STOPPDATUM FÖR LARM-223                 
032300*                                 STOP DATE FOR ALARM-223                 
032400        05 WDK7-FLREFILL     PIC X.                                       
032500*                                 REFILLARTIKEL                           
032600*                                 REFILLPART                              
032700        05 WDK7-FLREFNYO     PIC X.                                       
032800*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
032900*                                 WAIT FOR NEXT DEMAND                    
033000        05 WDK7-REPPFAKT     PIC S9V9(2)         COMP-3.                  
033100*                                 PREPLANNED FACTOR                       
033200        05 WDK7-FILLER       PIC X(7).                                    
033300     03 WDK7-WDK712-FILLER REDEFINES WDK7-WDK711.                         
033400        05 WDK7-WDK712.                                                   
033500*                                 ARTIKELINFO NDC LÄNDER                  
033600*                                 GÄLLER ALLA DC INOM ETT LAND            
033700*                                 FYSISK NYCKEL: IDLANDX2                 
033800           07 WDK7-IDLANDX2  PIC X(2).                                    
033900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
034000*                                 2-LETTER CODE FOR COUNTRY               
034100           07 WDK7-BEFT      PIC S9(3)           COMP-3.                  
034200*                                 FÖRPACKNINGSTYP                         
034300*                                 PACKAGING TYPE                          
034400           07 WDK7-DAPUBL    PIC 9(8).                                    
034500*                                 PUBLICERINGSDATUM PER ART/LAND          
034600*                                 DATE OF PUBLISHING PART/COUNTRY         
034700           07 WDK7-IDARTNR-EMBQ0                                          
034800                             PIC S9(9)           COMP-3.                  
034900*                                 EMBALLAGEARTIKELNR FÖR Q0               
035000           07 WDK7-IDARTNR-EMBQ1                                          
035100                             PIC S9(9)           COMP-3.                  
035200*                                 EMBALLAGEARTIKELNR FÖR Q1               
035300           07 WDK7-IDARTNR-EMBQ2                                          
035400                             PIC S9(9)           COMP-3.                  
035500*                                 EMBALLAGEARTIKELNR FÖR Q2               
035600           07 WDK7-IDUSER-EMB                                             
035700                             PIC X(8).                                    
035800*                                 ANVÄNDARENS SÄKERHETS ID                
035900*                                 USER SECURITY-IDENTITY                  
036000           07 WDK7-KDARTURS  PIC X(2).                                    
036100*                                 ARTIKELURSPRUNGSKOD                     
036200*                                 COUNTRY OF ORIGIN                       
036300           07 WDK7-KVDAGAR-INLEV                                          
036400                             PIC S9(3)           COMP-3.                  
036500*                                 INLEVERANSTID     (ANTAL DAGAR)         
036600           07 WDK7-PRARTSJK  PIC S9(7)V9(2)      COMP-3.                  
036700*                                 ARTIKELNS SJÄLVKOSTNAD                  
036800*                                 COST OF SALES                           
036900           07 WDK7-PRMATRL   PIC S9(7)V9(2)      COMP-3.                  
037000*                                 FAST PRIS UNDER LÖPANDE ÅR              
037100*                                 MATERIAL PRICE FOR ACTUAL YEAR          
037200           07 WDK7-VKART     PIC S9(7)           COMP-3.                  
037300*                                 ARTIKELVIKT (G)                         
037400*                                 PART WEIGHT (G)                         
037500           07 WDK7-VLARTNTO  PIC S9(8)V9(1)      COMP-3.                  
037600*                                 ARTIKELVOLYM (CM3)                      
037700*                                 PART VOLUME    (CM3)                    
037800           07 WDK7-TIERSDAT-VIPS                                          
037900                             PIC S9(5)           COMP-3.                  
038000*                                 DATUM NÄR ERS. INFO TILL VIPS           
038100*                                 SEND DATE OF SUPERS. TO VIPS            
038200           07 WDK7-TIUPPDAT-EMB                                           
038300                             PIC S9(7)           COMP-3.                  
038400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
038500*                                 UPDATING DATE     (YYMMDD)              
038600           07 WDK7-KDMATRPR  PIC X.                                       
038700*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
038800*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
038900           07 WDK7-KVQPACK-3 PIC S9(5)           COMP-3.                  
039000*                                 ANTAL I Q3 FÖRPACKNING                  
039100*                                 QUANTITY IN BULK PACK Q3                
039200           07 WDK7-IDPSN-DC  PIC 9(3).                                    
039300*                                 PROPER SHIPPING NAME PER XDC            
039400*                                 PROPER SHIPPING NAME XDC                
039500           07 WDK7-FLMSKUPD  PIC X.                                       
039600*                                 MASKINELL UPPDATERING J/N               
039700           07 WDK7-FLREFERAL PIC X.                                       
039800*                                 REFERALARTIKEL I LANDET                 
039900*                                 REFERALPART IN COUNTRY                  
040000           07 WDK7-FILLER    PIC X.                                       
040100        05 FILLER            PIC X(215).                                  
040200     03 WDK7-WDK721-FILLER REDEFINES WDK7-WDK711.                         
040300        05 WDK7-WDK721.                                                   
040400*                                 LAGERINFORMATION                        
040500*                                 FYSISK NYCKEL KDSEGKEY                  
040600*                                  ALLTID = "1"                           
040700           07 WDK7-KDSEGKEY  PIC X.                                       
040800*                                 TEKNISK SEGMENT-NYCKEL                  
040900*                                 TECHNICAL SEGMENT KEY                   
041000           07 WDK7-DAORDSP   PIC 9(8).                                    
041100*                                 ANGER DATUM NÄR STATUS PÅ ORDER         
041200*                                 SPÄRRFLAGGA ÄNDRAS                      
041300*                                 YYYYMMDD                                
041400*                                 STATES THE DATE WHEN FLAG OF BL         
041500*                                 OCK ORDER IS CHANGED                    
041600           07 WDK7-DASPBULK  PIC 9(8).                                    
041700*                                 ANGER DATUM NÄR STATUS PÅ FLAGG         
041800*                                 A FÖR ATT                               
041900*                                 SPÄRRA BULKORDER ÄNDRAS                 
042000*                                 YYYYMMDD                                
042100*                                 STATES THE DATE WHEN FLAG OF BL         
042200*                                 OCK BULKORDER IS CHANGED                
042300           07 WDK7-DAORDSP-EJRO                                           
042400                             PIC 9(8).                                    
042500*                                 ANGER DATUM NÄR FLAGGA FÖR ORDE         
042600*                                 RSPÄRR UTAN RESTNOTERING ÄNDRAS         
042700*                                 YYYYMMDD                                
042800*                                 STATES THE DATE WHEN FLAG OF OR         
042900*                                 DER BLOCK                               
043000*                                 WITHOUT BACKORDERING IS CHANGED         
043100           07 WDK7-IDUSER-ORDSP                                           
043200                             PIC X(8).                                    
043300*                                 ANVÄNDAR-ID SPÄRRA ORDER                
043400*                                 USER ID ORDER BLOCK                     
043500           07 WDK7-IDUSER-SPBULK                                          
043600                             PIC X(8).                                    
043700*                                 ANVÄNDAR-ID SPÄRRA BULKORDER            
043800*                                 USER ID BULKORDER BLOCK                 
043900           07 WDK7-IDUSER-ORDSP-EJRO                                      
044000                             PIC X(8).                                    
044100*                                 ANVÄNDAR-ID ORDERSPÄRR UTAN RES         
044200*                                 TNOTERING                               
044300*                                 USER ID ORDER BLOCK WITHOUT BAC         
044400*                                 KORDERING                               
044500           07 WDK7-TEARTNOT-ORDER                                         
044600                             PIC X(70).                                   
044700*                                 ARTIKEL NOTERING ORDER                  
044800*                                 PART REMARKS NOTE ORDER                 
044900           07 WDK7-FILLER    PIC X(21).                                   
045000        05 FILLER            PIC X(150).                                  
045100     03 WDK7-WDK722-FILLER REDEFINES WDK7-WDK711.                         
045200        05 WDK7-WDK722.                                                   
045300*                                 ANSKAFFARINFO PER CN-DC                 
045400*                                 FYSISK NYCKEL KDSEGKEY                  
045500*                                  ALLTID = "1"                           
045600           07 WDK7-KDSEGKEY  PIC X.                                       
045700*                                 TEKNISK SEGMENT-NYCKEL                  
045800*                                 TECHNICAL SEGMENT KEY                   
045900           07 WDK7-DAPBPLAN  PIC 9(8).                                    
046000*                                 DATUM KVPB-PLAN GILTIG TOM              
046100*                                 DATE KVPB-PLAN VALID UNTIL              
046200           07 WDK7-DASEASON  PIC 9(8).                                    
046300*                                 DATUM RESEASON-LEDTID GILTIG TO         
046400*                                 M                                       
046500*                                 DATE RESEASON-LEDTID VALID UNTI         
046600*                                 L                                       
046700           07 WDK7-FLJIT     PIC X.                                       
046800*                                 JUST-IN-TIME FLAGGA                     
046900*                                 JUST-IN-TIME FLAG                       
047000           07 WDK7-IDANSK    PIC S9(3)           COMP-3.                  
047100*                                 ANSKAFFARNUMMER                         
047200*                                 PROCURER NO.                            
047300           07 WDK7-IDINK     PIC X(4).                                    
047400*                                 INKÖPARNUMMER                           
047500*                                 PURCHASE IDENTIFICATION NUMBER          
047600           07 WDK7-IDLEVNR-FRAM                                           
047700                             PIC X(5).                                    
047800*                                 FRAMTIDA LEVERANTÖRNUMMER               
047900*                                 THE SUPPLIER NAME IN THE FUTURE         
048000           07 WDK7-IDLEVNR-SHIP                                           
048100                             PIC X(5).                                    
048200*                                 SKEPPANDE LEVERANTÖR                    
048300*                                 SHIPPING SUPPLIER                       
048400           07 WDK7-IDPLANGR-AG                                            
048500                             PIC S9              COMP-3.                  
048600*                                 PLANERINGSGRUPP ANSKAFFARE              
048700           07 WDK7-KDAVT     PIC S9              COMP-3.                  
048800*                                 AVTALSMÄRKNING                          
048900*                                 AGREEMENT CODE                          
049000           07 WDK7-KDLEVPLF  PIC X.                                       
049100*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
049200*                                 CODE FOR APPROVAL OF SCHEDULE P         
049300*                                 ROPOSAL                                 
049400           07 WDK7-KDLPSP    PIC S9              COMP-3.                  
049500*                                 LEVERANSPLANESPÄRR                      
049600           07 WDK7-FILLERX1  PIC X.                                       
049700           07 WDK7-KDOPPLAN  PIC X.                                       
049800*                                 OPTIMAL PLAN INOM FRYSTID               
049900*                                 OPTIMAL PLAN WITHIN FREEZTIME           
050000           07 WDK7-KVDAGAR-FFH                                            
050100                             PIC S9(3)           COMP-3.                  
050200*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
050300           07 WDK7-KVEOQ     PIC S9(7)           COMP-3.                  
050400*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
050500*                                 ET                                      
050600           07 WDK7-KVPB-JUST1                                             
050700                             PIC S9(6)V9(1)      COMP-3.                  
050800*                                 PERIODBEHOVSJUSTERING-1                 
050900*                                 PERIOD REQUIREMENTS-1                   
051000           07 WDK7-KVPB-JUST2                                             
051100                             PIC S9(6)V9(1)      COMP-3.                  
051200*                                 PERIODBEHOVSJUSTERING-2                 
051300*                                 PERIOD REQUIREMENTS-2                   
051400           07 WDK7-KVPB-PLAN PIC S9(6)V9(1)      COMP-3.                  
051500*                                 PLANERAT PERIODBEHOV                    
051600*                                 PLANNED PERIOD REQUIREMENTS             
051700           07 WDK7-KVPB-TREND                                             
051800                             PIC S9(6)V9(1)      COMP-3.                  
051900*                                 PERIODTRENDVÄRDE                        
052000           07 WDK7-KVPALL    PIC S9(7)           COMP-3.                  
052100*                                 ANTAL I PALL                            
052200*                                 QUANTITY IN PALLET                      
052300           07 WDK7-KVSLAGER  PIC S9(7)           COMP-3.                  
052400*                                 SÄKERHETSLAGER                          
052500*                                 SAFETY STOCK                            
052600           07 WDK7-KVSLUTKP  PIC S9(7)           COMP-3.                  
052700*                                 SLUTKÖPSSALDO                           
052800           07 WDK7-KVSPANT   PIC S9(7)           COMP-3.                  
052900*                                 SPÄRRAT ANTAL                           
053000*                                 BLOCKED QTY                             
053100           07 WDK7-KVULOAD   PIC S9(7)           COMP-3.                  
053200*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
053300*                                 MIN LOAD FROM SUPPLIER                  
053400           07 WDK7-KVVECKOR-LT                                            
053500                             PIC S9(3)           COMP-3.                  
053600*                                 ANTAL VECKOR LEDTID                     
053700           07 WDK7-KVVECKOR-FT                                            
053800                             PIC S9(3)           COMP-3.                  
053900*                                 ANTAL VECKOR FRYSNINGSTID               
054000           07 WDK7-KVVECKOR-TREND                                         
054100                             PIC S9(3)           COMP-3.                  
054200*                                 ANTAL VECKOR TRENDVÄRDE                 
054300           07 WDK7-RESEASON-PLAN                                          
054400                             OCCURS 12 TIMES                              
054500                             PIC S9V9(2)         COMP-3.                  
054600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
054700           07 WDK7-TIDATUM-TREND                                          
054800                             PIC S9(7)           COMP-3.                  
054900*                                 JUSTERAD TREND AAMMDD                   
055000*                                 LAST TREND CHANGE  YYMMDD               
055100           07 WDK7-TILPSP    PIC S9(5)           COMP-3.                  
055200*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
055300           07 WDK7-TILEVDAG  OCCURS 5 TIMES                               
055400                             PIC S9              COMP-3.                  
055500*                                 AVSÄNDNINGSDAG INOM VECKA               
055600*                                 DELIVERY WEEK DAY                       
055700           07 WDK7-TILEVDAT  PIC S9(7)           COMP-3.                  
055800*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
055900*                                 DATE FOR FUTURE SUPPLIER                
056000           07 WDK7-TIMANLED  PIC S9(7)           COMP-3.                  
056100*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
056200*                                 END DATE MAN.LEAD TIME (YYMMDD)         
056300           07 WDK7-TIMANSEC  PIC S9(7)           COMP-3.                  
056400*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
056500*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
056600           07 WDK7-TIOMSPEC  PIC S9(5)           COMP-3.                  
056700*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
056800           07 WDK7-TIREFSTO-LOC                                           
056900                             PIC S9(7)           COMP-3.                  
057000*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
057100*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
057200           07 WDK7-TISLUTKP  PIC S9(7)           COMP-3.                  
057300*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
057400*                                 CALC OF ATR-BAL IS TO COMMENCE          
057500           07 WDK7-TIPBJUST-1                                             
057600                             PIC S9(5)           COMP-3.                  
057700*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
057800*                                 FIRST PB-JUST DATE YYWW                 
057900           07 WDK7-TIPBJUST-2                                             
058000                             PIC S9(5)           COMP-3.                  
058100*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
058200*                                 SECOND PB-JUST DATE YYWW                
058300           07 WDK7-IDLEVNR-SHIP-FRAM                                      
058400                             PIC X(5).                                    
058500*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
058600*                                 THE SHIP.SUPPLIER IN THE FUTURE         
058700           07 WDK7-FLLARM-BUF                                             
058800                             PIC X.                                       
058900*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
059000           07 WDK7-FILLER    PIC X(11).                                   
059100        05 FILLER            PIC X(120).                                  
059200     03 WDK7-WDK723-FILLER REDEFINES WDK7-WDK711.                         
059300        05 WDK7-WDK723.                                                   
059400*                                 AVTALSINFORMATION FÖR NDC:ER            
059500*                                 FYSISK NYCKEL: WDK723KY                 
059600*                                 (IDAVTAL + IDLEVNR-AVT)                 
059700           07 WDK7-IDAVTAL   PIC S9(13)          COMP-3.                  
059800*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
059900*                                 PPP   = INKÖPARNR (PREFIX)              
060000*                                 BBBBB = BESTÄLLARNR                     
060100*                                 SSS   = SUFFIX                          
060200           07 WDK7-IDLEVNR-AVT                                            
060300                             PIC X(5).                                    
060400*                                 LEVERANTÖR ENLIGT AVTAL                 
060500           07 WDK7-IDLEVNR-SHIP                                           
060600                             PIC X(5).                                    
060700*                                 SKEPPANDE LEVERANTÖR                    
060800*                                 SHIPPING SUPPLIER                       
060900           07 WDK7-TIAVTAL   PIC S9(7)           COMP-3.                  
061000*                                 AVTALSDATUM  (ÅÅMMDD)                   
061100        05 FILLER            PIC X(269).                                  
061200     03 WDK7-WDK724-FILLER REDEFINES WDK7-WDK711.                         
061300        05 WDK7-WDK724.                                                   
061400*                                 INFO OM GAMMALT ELLER KOMMANDE          
061500*                                 BESTÄLLNINGS PRIS FÖR NDC:ER            
061600*                                 FYSISK NYCKEL: WDK724KY                 
061700*                                 (DAPRLIST-9KOMPL + IDLEVNR-PR)          
061800           07 WDK7-DAPRLIST-9KOMPL                                        
061900                             PIC 9(8).                                    
062000*                                 PRISLISTEDATUM (AAAAMMDD) 9KOMP         
062100*                                 PRICE LIST DATE (AAAAMMDD) 9COM         
062200           07 WDK7-IDLEVNR-PR                                             
062300                             PIC X(5).                                    
062400*                                 LEVERANTÖRNR FÖR DETTA PRIS             
062500*                                 SUPPLIER NUMBER FOR THIS PRICE          
062600           07 WDK7-IDUSER    PIC X(8).                                    
062700*                                 ANVÄNDARENS SÄKERHETS ID                
062800*                                 USER SECURITY-IDENTITY                  
062900           07 WDK7-KDFPKPRI  PIC X.                                       
063000*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
063100*                                 IF PACKING INCLUDED IN PRICE            
063200           07 WDK7-KDPRURSP  PIC X.                                       
063300*                                 PRISHÄRSTAMNING BESTÄLLNING             
063400*                                 ORIGINATE ORDER PRICE                   
063500           07 WDK7-KDVALISO  PIC X(3).                                    
063600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
063700*                                 CURRENCY CODE BY ISO-STANDARD.          
063800           07 WDK7-PRARTBES-PR                                            
063900                             PIC S9(7)V9(2)      COMP-3.                  
064000*                                 DETTA BESTÄLLNINGSPRIS (KR)             
064100           07 WDK7-PRARTBEL-PR                                            
064200                             PIC S9(8)V9(5)      COMP-3.                  
064300*                                 DETTA BESTÄLLNINGSPRIS                  
064400*                                 (I LEVERANTÖRENS VALUTA)                
064500           07 WDK7-SUINLEV-PR                                             
064600                             PIC S9(3)           COMP-3.                  
064700*                                 ANTAL INLEV. TILL DETTA PRIS            
064800           07 WDK7-TIREGDAT  PIC S9(7)           COMP-3.                  
064900*                                 REGISTRERINGSDATUM (ÅÅMMDD OR Å         
065000*                                 ÅÅÅ-MM-DD)                              
065100*                                 REGISTRATION DATE (YYMMDD OR YY         
065200*                                 YY-MM-DD)                               
065300        05 FILLER            PIC X(246).                                  
065400     03 WDK7-WDK725-FILLER REDEFINES WDK7-WDK711.                         
065500        05 WDK7-WDK725.                                                   
065600*                                 BESTÄLLNINGSINFORMATION                 
065700*                                 GÄLLER NDC:ER                           
065800*                                 SÖKBEGREPP IDBEST                       
065900           07 WDK7-IDBEST    PIC S9(13)          COMP-3.                  
066000*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
066100*                                 PPP   = (PREFIX) INKÖPARNR              
066200*                                 BBBBBB= BESTÄLLARNR                     
066300*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
066400           07 WDK7-IDLEVNR-BEST                                           
066500                             PIC X(5).                                    
066600*                                 LEVERANTÖR ENL. BESTÄLLNING             
066700           07 WDK7-KDBEH-BEST                                             
066800                             PIC S9              COMP-3.                  
066900*                                 BEHANDLINGSKOD BESTÄLLNING              
067000           07 WDK7-TIBEST    PIC S9(7)           COMP-3.                  
067100*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
067200        05 FILLER            PIC X(273).                                  
067300     03 WDK7-WDK726-FILLER REDEFINES WDK7-WDK711.                         
067400        05 WDK7-WDK726.                                                   
067500*                                 EMBALLAGE                               
067600*                                 FYSISK NYCKEL KDEMBAL                   
067700           07 WDK7-KDEMBKEY  PIC X(3).                                    
067800*                                 NYCKEL FÖR ATT SÄRSKILJA EMABAL         
067900*                                 LAGE ÅT                                 
068000*                                 KEY TO SEPERATE PACKAGES                
068100           07 WDK7-IDARTNR-EMB                                            
068200                             PIC S9(9)           COMP-3.                  
068300*                                 EMBALLAGE-ARTIKELNUMMER                 
068400        05 FILLER            PIC X(282).                                  
068500     03 WDK7-WDK727-FILLER REDEFINES WDK7-WDK711.                         
068600        05 WDK7-WDK727.                                                   
068700*                                 FRAMTIDA PROGNOSER XDC                  
068800*                                 FYSISK NYCKEL KDSEGKEY                  
068900*                                  ALLTID = "1"                           
069000           07 WDK7-KDSEGKEY  PIC X.                                       
069100*                                 TEKNISK SEGMENT-NYCKEL                  
069200*                                 TECHNICAL SEGMENT KEY                   
069300           07 WDK7-KVPB-JUST OCCURS 2 TIMES                               
069400                             PIC S9(6)V9(1)      COMP-3.                  
069500*                                 PERIODBEHOVSJUSTERING                   
069600           07 WDK7-TIPBJUST  OCCURS 2 TIMES                               
069700                             PIC S9(5)           COMP-3.                  
069800*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
069900        05 FILLER            PIC X(275).                                  
070000     03 WDK7-WDK728-FILLER REDEFINES WDK7-WDK711.                         
070100        05 WDK7-WDK728.                                                   
070200*                                 CUSTOMS TRACKING SEGMENT                
070300*                                 FYSISK NYCKEL DAINLEV                   
070400           07 WDK7-DAINLEV   PIC 9(16).                                   
070500*                                 INLEVERANS NUMMER                       
070600*                                 CONSIGNMENT IDENTITY                    
070700*                                 (YYYYMMDD+HHMMSSTH)                     
070800           07 WDK7-KVANTMOT  PIC S9(7)           COMP-3.                  
070900*                                 ANTAL MOTTAGET                          
071000*                                 QUANTITY RECEIVED                       
071100           07 WDK7-KVAVIS    PIC S9(7)           COMP-3.                  
071200*                                 AVISERAT ANTAL                          
071300*                                 QUANTITY NOTIFIED                       
071400           07 WDK7-KVTRACK-KVAR                                           
071500                             PIC S9(7)           COMP-3.                  
071600*                                 ANTAL KVAR PER TRACKING-ID              
071700*                                 REMAINING QTY OF TRACKING-ID            
071800           07 WDK7-IDTRACK   PIC X(25).                                   
071900*                                 TRACKING ID FROM CUSTOMS                
072000*                                 CUSTOMS TRACKING ID                     
072100        05 FILLER            PIC X(237).                                  
072200*** END OF VILMAII-COPY LENGTH= 305 BYTES                                 
