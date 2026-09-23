000100 01  WNDCPART.                                                            
000200*                                 PRIMÄREXTRAKT                           
000300*                                                                         
000400*                                 LAGERBAND                               
000500*                                 COPY-TEXT FÖR EXTRAKT WNDCDAY           
000600*                                                       WNDCWEEK          
000700*                                                       WNDCACC           
000800*                                                       WNDCPLAN          
000900*                                                       WNDCYEAR          
001000*                                 FRAMSTÄLLS VARJE :                      
001100*                                            MORGON (DAGLIGEN)            
001200*                                            VECKOSLUT                    
001300*                                            REDOVISNINGSPERIOD           
001400*                                            PLANERINGSPERIOD             
001500*                                            ÅRS-SLUT                     
001600*                                                                         
001700*                                 DÄR IDDC  = NDC-NA                      
002200*                                                                         
002300*                                 1 POST/ARTIKELNUMMER/IDDC               
002400*                                                                         
002500*                                 PRIMARY EXTRACT                         
002600*                                                                         
002700*                                 PART INFORMATION FILE FOR NDC           
002800*                                 COPY-TEXT FOR EXTRACT WNDCDAY           
002900*                                                       WNDCWEEK          
003000*                                                       WNDCACC           
003100*                                                       WNDCPLAN          
003200*                                                       WNDCYEAR          
003300*                                                                         
003400*                                 CREATED EVERY :                         
003500*                                               MORNING (DAILY)           
003600*                                               WEEKEND                   
003700*                                               ACCOUNTPERIOD             
003800*                                               PLANNINGPERIOD            
003900*                                               YEAR                      
004000*                                                                         
004100*                                 WHERE IDDC = NDC-NA                     
004600*                                                                         
004700*                                 1 RECORD/PARTNUMBER/IDDC                
004800*                                                                         
004900     03 IDARTNR              PIC S9(9)           COMP-3.                  
005000*                                 ARTIKELNUMMER                           
005100*                                 PART NUMBER                             
005200     03 REKSIFFR             PIC S9              COMP-3.                  
005300*                                 KONTROLLSIFFRA                          
005400*                                 PART NO CHECK DIGIT                     
005500     03 IDDC                 PIC X(2).                                    
005600*                                 IDENTIFIERARE LAGER                     
005700*                                 WAREHOUSE IDENTIFIER                    
005800     03 ARTIKEL-INFO.                                                     
005900*                                 ARTIKELUPPGIFTER                        
006000*                                                                         
006100*                                 PART INFORMATION                        
006200        05 BEART-SVE         PIC X(25).                                   
006300*                                 SVENSK ARTIKELBENÄMNING                 
006400        05 BEART-ENG         PIC X(25).                                   
006500*                                 ENGELSK ARTIKELBENÄMNING                
006600        05 BEFT              PIC S9(3)           COMP-3.                  
006700*                                 FÖRPACKNINGSTYP                         
006800*                                 PACKAGING TYPE                          
006900        05 FLAVRART          PIC X.                                       
007000*                                 AVROPSARTIKEL                           
007100        05 FLERS             PIC X.                                       
007200*                                 TILLKOMMANDE ARTIKEL ?                  
007300        05 FLGEMART          PIC X.                                       
007400*                                 FLAGGA GEMENSAM ARTIKEL                 
007500*                                 COMMON PART FLAG                        
007600        05 FLIART            PIC X.                                       
007700*                                 ARTIKELN INGÅR I SATS                   
007800*                                 PART IN KIT                             
007900        05 FLJIT             PIC X.                                       
008000*                                 JUST-IN-TIME FLAGGA                     
008100*                                 JUST-IN-TIME FLAG                       
008200        05 FLLSRDEL          PIC X.                                       
008300*                                 LEVERERAS SOM RESDEL                    
008400        05 FLTPO1            PIC X.                                       
008500*                                 ARTIKELN GODKÄND FÖR TPO1               
008600*                                 TPO1 ALLOWED FOR ARTICLE                
008700        05 IDANSK            PIC S9(3)           COMP-3.                  
008800*                                 ANSKAFFARNUMMER                         
008900*                                 PROCURER NO.                            
009000        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
009100*                                 EMBALLAGEARTIKELNR FÖR Q0               
009200        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
009300*                                 EMBALLAGEARTIKELNR FÖR Q1               
009400        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
009500*                                 EMBALLAGEARTIKELNR FÖR Q2               
009600        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
009700*                                 EMBALLAGEARTIKELNR FÖR Q3               
009800        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
009900*                                 EMBALLAGEARTIKELNR FÖR Q4               
010000        05 IDBERED           PIC S9(3)           COMP-3.                  
010100*                                 BEREDARENUMMER                          
010200        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
010300*                                 FUNKTIONSGRUPP                          
010400*                                 FUNCTION GROUP                          
010500        05 FILLERX2          PIC X(2).                                    
010600        05 IDLEVNR           PIC X(5).                                    
010700*                                 LEVERANTÖRNUMMER                        
010800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010900        05 IDLKTO            PIC S9(7)           COMP-3.                  
011000*                                 LAGERKONTO (FFHHHUU)                    
011100*                                 STOCK ACCOUNT (CCMMMSS)                 
011200        05 IDPROJ            PIC X(4).                                    
011300*                                 PARTS PROJEKTIDENTITET                  
011400*                                 PARTS PROJECT IDENTITY                  
011500        05 IDPSN             PIC 9(3).                                    
011600*                                 PROPER SHIPPING NAME                    
011700*                                 PROPER SHIPPING NAME                    
011800        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
011900*                                 PERSONKOD REFILLANSVARIG                
012000*                                 REFILL RESPONSIBLE ID                   
012100        05 IDPSN-DC          PIC 9(3).                                    
012200*                                 PROPER SHIPPING NAME PER XDC            
012300*                                 PROPER SHIPPING NAME XDC                
012400        05 FILLER            PIC X(3).                                    
012500        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
012600*                                 STATISTISKT NUMMER                      
012700*                                 1 = NORSKT                              
012800*                                 2 = ENGELSKT                            
012900*                                 3 = BELGISKT                            
013000*                                 4 = PERUANSKT                           
013100*                                 5 = SVENSKT                             
013200*                                 6 =                                     
013300*                                 STATISTICAL NO.                         
013400        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
013500*                                 STATISTISKT NUMMER                      
013600*                                 1 = NORSKT                              
013700*                                 2 = ENGELSKT                            
013800*                                 3 = BELGISKT                            
013900*                                 4 = PERUANSKT                           
014000*                                 5 = SVENSKT                             
014100*                                 6 =                                     
014200*                                 STATISTICAL NO.                         
014300        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
014400*                                 STATISTISKT NUMMER                      
014500*                                 1 = NORSKT                              
014600*                                 2 = ENGELSKT                            
014700*                                 3 = BELGISKT                            
014800*                                 4 = PERUANSKT                           
014900*                                 5 = SVENSKT                             
015000*                                 6 =                                     
015100*                                 STATISTICAL NO.                         
015200        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
015300*                                 STATISTISKT NUMMER                      
015400*                                 1 = NORSKT                              
015500*                                 2 = ENGELSKT                            
015600*                                 3 = BELGISKT                            
015700*                                 4 = PERUANSKT                           
015800*                                 5 = SVENSKT                             
015900*                                 6 =                                     
016000*                                 STATISTICAL NO.                         
016100        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
016200*                                 STATISTISKT NUMMER                      
016300*                                 1 = NORSKT                              
016400*                                 2 = ENGELSKT                            
016500*                                 3 = BELGISKT                            
016600*                                 4 = PERUANSKT                           
016700*                                 5 = SVENSKT                             
016800*                                 6 =                                     
016900*                                 STATISTICAL NO.                         
017000        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
017100*                                 STATISTISKT NUMMER                      
017200*                                 1 = NORSKT                              
017300*                                 2 = ENGELSKT                            
017400*                                 3 = BELGISKT                            
017500*                                 4 = PERUANSKT                           
017600*                                 5 = SVENSKT                             
017700*                                 6 =                                     
017800*                                 STATISTICAL NO.                         
017900        05 KDAGE             PIC X.                                       
018000*                                 AGE-CODE                                
018100*                                 AGE-CODE                                
018200        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018300*                                 HANTERINGSKOD                           
018400*                                 HANDLING CODE                           
018500        05 KDARTURS          PIC X(2).                                    
018600*                                 ARTIKELURSPRUNGSKOD                     
018700*                                 COUNTRY OF ORIGIN                       
018800        05 KDBPSR            PIC S9              COMP-3.                  
018900*                                 BASLAGERFÖRSLAGSNIVÅ                    
019000*                                 BASIC PART STOCK RECOMMENDATION         
019100        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019200*                                 EMBALLAGEKOD 0                          
019300        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019400*                                 EMBALLAGEKOD 1                          
019500        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
019600*                                 EMBALLAGEKOD 2                          
019700        05 KDERS             PIC S9(3)           COMP-3.                  
019800*                                 ERSÄTTNINGSKOD                          
019900*                                 SUPERSESSION CODE                       
020000        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
020100*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020200*                                 OBSOLETION SUPERSESSION CODE            
020300        05 KDFARLIG          PIC S9              COMP-3.                  
020400*                                 KOD FÖR FARLIGT GODS                    
020500*                                 DANGEROUS GOODS CODE                    
020600        05 KDGK              PIC S9              COMP-3.                  
020700*                                 GODSMOTTAGAREKOD                        
020800*                                 GOODS RECEIVING WAREHOUSE CODE          
020900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
021000*                                 PRODUKTSLAG                             
021100*                                 PRODUCT GROUP                           
021200        05 KDSORT            PIC X(2).                                    
021300*                                 SORT-KOD                                
021400*                                 UNIT OF MEASURE                         
021500        05 KDSPEEMB          PIC 9.                                       
021600*                                 SPECIALEMBALLAGEKOD                     
021700*                                 SPECIAL PACKING CODE                    
021800        05 KDSRA             PIC S9(3)           COMP-3.                  
021900*                                 SRA-KOD                                 
022000*                                 SRA CODE                                
022100        05 KDUART            PIC X.                                       
022200*                                 UNDANTAGSARTIKEL                        
022300*                                 EXECPTION PARTS                         
022400        05 KDVVKL            PIC S9              COMP-3.                  
022500*                                 VOLYMVÄRDESKLASS                        
022600*                                 VOLUME VALUE CLASS                      
022700        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022800*                                 YTBEHANDLINGSKOD                        
022900*                                                                         
023000        05 KVPALL            PIC S9(7)           COMP-3.                  
023100*                                 ANTAL I PALL                            
023200*                                 QUANTITY IN PALLET                      
023300        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023400*                                 ANTAL I Q0 FÖRPACKNING                  
023500        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
023600*                                 ANTAL I Q1 FÖRPACKNING                  
023700*                                 QUANTITY IN BULK PACK Q1                
023800        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023900*                                 ANTAL I Q2 FÖRPACKNING                  
024000*                                 QUANTITY IN BULK PACK Q2                
024100        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024200*                                 ANTAL I Q3 FÖRPACKNING                  
024300*                                 QUANTITY IN BULK PACK Q3                
024400        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024500*                                 ANTAL I Q4 FÖRPACKNING                  
024600*                                 QUANTITY IN BULK PACK Q4                
024700        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024800*                                 BESTÄLLNINGSPRIS I KRONOR               
024900*                                 ORDER PRICE SWEDISH CURRENCY            
025000        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
025100*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025200*                                 GROSS-PRICE EXPORT                      
025300*                                  (FOB-GROSS)                            
025400        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025500*                                 ARTIKELNS SJÄLVKOSTNAD                  
025600*                                 COST OF SALES                           
025700        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025800*                                 ARTIKELSTANDARDPRIS                     
025900*                                 STANDARD PRICE                          
026000        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
026100*                                 INKÖPSPRIS                              
026200*                                 PURCHASE PRICE                          
026300        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026400*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026500*                                 DATE OF SUPERSESSION (YYWWD)            
026600        05 TIFINLV           PIC S9(5)           COMP-3.                  
026700*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026800*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026900        05 VKART             PIC S9(7)           COMP-3.                  
027000*                                 ARTIKELVIKT (G)                         
027100*                                 PART WEIGHT (G)                         
027200        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027300*                                 ARTIKELVOLYM NETTO (CM3)                
027400*                                 PART NET VOLUME    (CM3)                
027500        05 IDKAT-1           PIC X(5).                                    
027600*                                 KATALOGBETECKNING                       
027700        05 IDKAT-2           PIC X(5).                                    
027800*                                 KATALOGBETECKNING                       
027900        05 IDKAT-3           PIC X(5).                                    
028000*                                 KATALOGBETECKNING                       
028100        05 IDINK             PIC X(4).                                    
028200*                                 INKÖPARNUMMER                           
028300*                                 PURCHASE IDENTIFICATION NUMBER          
028400        05 FILLER            PIC X(74).                                   
028500     03 NDC-INFO.                                                         
028600*                                 INFO SOM GÄLLER ENBART NDC              
028700*                                                                         
028800*                                                                         
028900*                                 INFO VALID ONLY FOR NDC                 
029000        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029100*                                 LAGEROMRÅDE                             
029200*                                 AREA                                    
029300        05 ADGANG            PIC S9(3)           COMP-3.                  
029400*                                 GÅNG                                    
029500*                                 AISLE                                   
029600        05 ADPLATS           PIC S9(5)           COMP-3.                  
029700*                                 LAGERPLATSNUMMER                        
029800*                                 LOCATION                                
029900        05 DAPUBL            PIC 9(8).                                    
030000*                                 PUBLICERINGSDATUM PER ART/LAND          
030100*                                 DATE OF PUBLISHING PART/COUNTRY         
030200        05 FLFLYG            PIC X.                                       
030300*                                 FLYGARTIKEL                             
030400*                                 PART NUMBER SENT BY AIR                 
030500        05 FLREFBEO          PIC X.                                       
030600*                                 AUTOMATISK REFILL BEORDRING?            
030700*                                 AUTOMATIC REFILL ORDERING?              
030800        05 FLREFILL          PIC X.                                       
030900*                                 REFILLARTIKEL                           
031000*                                 REFILLPART                              
031100        05 FLREFNYO          PIC X.                                       
031200*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031300*                                 WAIT FOR NEXT DEMAND                    
031400        05 FLSKROT-BEORD     PIC X.                                       
031500*                                 SKROTNING BEORDRAD AV ANSK              
031600*                                 SCRAPPING ORDERED BY PROCURER           
031700        05 FLWILSON          PIC X.                                       
031800*                                 WILSONFORMEL                            
031900*                                 FLAG TO USE WILSON OR NOT               
032000        05 FLORDSP           PIC X.                                       
032100*                                 ORDERSPÄRR                              
032200*                                 ORDER BLOCKED                           
032300        05 FLORDSP-EJRO      PIC X.                                       
032400*                                 ORDERSPÄRR EJ RESTNOTERING              
032500*                                 ORDER BLOCK NO BACKORDERING             
032600        05 FLSEASON          PIC X.                                       
032700*                                 SÄSONG PÅ ARTIKEL                       
032800*                                 SEASON MARK PER PART                    
032900        05 FLSPBULK          PIC X.                                       
033000*                                 FLAGGA SPÄRR MOT BULKORDER              
033100*                                 FLAG BULKORDER STOP                     
033200        05 IDLEVNR-NDC       PIC X(5).                                    
033300*                                 LEVERANTÖRNUMMER                        
033400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
033500        05 KDFREKKL          PIC X.                                       
033600*                                 FREKVENSKLASS                           
033700*                                 FREQ. CLASS                             
033800        05 KDLEVSP           PIC S9(3)           COMP-3.                  
033900*                                 SPÄRRKOD LEVERANS                       
034000*                                 DELIVERY BLOCKING CODE                  
034100        05 KDPRISKL          PIC X.                                       
034200*                                 PRISKLASS                               
034300*                                 PRICE CLASS                             
034400        05 KDPSLLOC          PIC 9(2).                                    
034500*                                 PRODUKTSLAG LOKALT                      
034600*                                 PRODUCT GROUP LOCAL                     
034700        05 KDREFSTA          PIC X.                                       
034800*                                 STATUS REFILLARTIKEL                    
034900*                                 STATUS REFILLPART                       
035000        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
035100*                                 DEL AV AK PÅ VÄG                        
035200*                                 PART OF AK ON ITS WAY                   
035300        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
035400*                                 DEL AV AK SOM LIGGER I SDC              
035500*                                 PART OF AK IN THE SDC                   
035600        05 KVBEART           PIC S9(7)           COMP-3.                  
035700*                                 BESTÄLLT ANTAL STYCKEN                  
035800*                                 ORDERED QUANTITY                        
035900        05 KVEFRS            PIC S9(7)           COMP-3.                  
036000*                                 EJ FAKTURERAT ANTAL STYCK               
036100*                                 ORDERED NOT INVOICED QTY                
036200        05 KVINVS            PIC S9(7)           COMP-3.                  
036300*                                 INVENTERINGSSALDO                       
036400*                                 STOCK-TAKING BALANCE                    
036500        05 KVLS              PIC S9(7)           COMP-3.                  
036600*                                 LAGERSALDO                              
036700*                                 STOCK BALANCE                           
036800        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
036900*                                 ORDERINGÅNG LEV FRÅN CDC                
037000*                                 ORDERED PCS PER TIME UNIT               
037100*                                 FORWARDED TO CDC                        
037200        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
037300*                                 ORDERINGÅNG TILL SDC                    
037400*                                 ORDERED PCS PER TIME UNIT               
037500*                                 ORDERED FROM SDC                        
037600        05 KVOI-IAAR         PIC S9(7)           COMP-3.                  
037700*                                 ORDERINGÅNG TILL DC INNEV ÅR            
037800*                                 ORDERED PCS PER TIME UNIT               
037900*                                 ORDERED FROM DC                         
038000        05 KVOI-INNEV        PIC S9(7)           COMP-3.                  
038100*                                 ORDERINGÅNG TILL DC INNEV PER           
038200*                                 ORDERED PCS THIS PERIOD                 
038300*                                 ORDERED FROM DC                         
038400        05 KVOI-FOREG        OCCURS 2 TIMES                               
038500                             PIC S9(7)           COMP-3.                  
038600*                                 ORDERINGÅNG TILL DC FÖREG ÅR            
038700*                                 ORDERED PCS LAST YEAR                   
038800*                                 ORDERED FROM DC                         
038900        05 KVOI-RP           OCCURS 12 TIMES                              
039000                             PIC S9(7)           COMP-3.                  
039100*                                 ORDERINGÅNG TILL DC                     
039200*                                 ORDERED PCS PER TIME UNIT               
039300*                                 ORDERED FROM DC                         
039400*                                 WEIGHTENED BY 4.33                      
039500        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
039600*                                 ORDERKÖSALDO, KLASS 2-4                 
039700*                                 ORDER QUEUE BALANCE, CLASS 2-4          
039800        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
039900*                                 ORDERKÖSALDO, KLASS 1                   
040000*                                 ORDER QUEUE BALANCE, CLASS 1            
040100        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
040200*                                 ORDERTRÄFFAR LEV FRÅN CDC               
040300*                                 ORDERHITS ON SDC FORWARDED              
040400*                                 TO CDC                                  
040500        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
040600*                                 ORDERTRÄFFAR PÅ SDC                     
040700*                                 ORDERHITS ON SDC                        
040800        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
040900*                                 PERIODBEHOV REFILLING                   
041000*                                 FORECAST REFILLING                      
041100        05 KVPB-REF-US       PIC S9(6)V9(1)      COMP-3.                  
041200*                                 PERIODBEHOV REFILLING USA               
041300*                                 FORECAST REFILLING US                   
041400        05 KVREFBER          PIC S9(7)           COMP-3.                  
041500*                                 BERÄKNAD REFILLINGKVANTITET             
041600*                                 CALCULATED REFILLING QUANTITY           
041700        05 KVREFOVL          PIC S9(7)           COMP-3.                  
041800*                                 BERÄKNAD ÖVERLAGERPUNKT                 
041900*                                 CALCULATED OVERSTOCK POINT              
042000        05 KVREFPKT          PIC S9(7)           COMP-3.                  
042100*                                 BERÄKNAD PÅFYLLNADSPUNKT                
042200*                                 CALCULATED REFILLING POINT              
042300        05 KVRESS            PIC S9(7)           COMP-3.                  
042400*                                 RESERVERAT ANTAL ARTIKLAR               
042500*                                 QUANTITY RESERVED ITEMS                 
042600        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
042700*                                 ANTAL SENASTE RETURORDER                
042800*                                 QUANTITY LAST RETURNORDER               
042900        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
043000*                                 RESTORDERSALDO, KLASS 2-4               
043100*                                 BACK ORDER BALANCE, CLASS 2-4           
043200        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
043300*                                 RESTORDERSALDO, KLASS 1                 
043400*                                 BACK ORDER BALANCE, CLASS 1             
043500        05 KVSKROT           PIC S9(7)           COMP-3.                  
043600*                                 ANTAL SENASTE SKROTORDER                
043700*                                 QUANTITY LAST SCRAPORDER                
043800        05 KVUTRS            PIC S9(7)           COMP-3.                  
043900*                                 UTREDNINGSSALDO                         
044000*                                 INVESTIGATION BALANCE                   
044100        05 PRAIRCO           PIC S9(7)V9(2)      COMP-3.                  
044200*                                 FLYGKOST PER ARTIKEL/DC SEK             
044300*                                 AIR COST PER PART/DC SEK                
044400        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
044500*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
044600*                                 AVERAGE COST FOREIGN CURRENCY           
044700        05 REOSAEK           PIC S9(3)V9(1)      COMP-3.                  
044800*                                 SEASONAL UNCERTAINTY FACTOR             
044900        05 RESEASON          OCCURS 12 TIMES                              
045000                             PIC S9V9(2)         COMP-3.                  
045100*                                 SÄSONGSINDEX                            
045200        05 TIINVDAT          PIC S9(5)           COMP-3.                  
045300*                                 INVENTERINGSDATUM                       
045400*                                 STOCKTAKING DATE                        
045500        05 TIORDREG          PIC S9(7)           COMP-3.                  
045600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
045700*                                 ORDER REGISTRATION DATE  YYMMDD         
045800        05 TIREFEFT          PIC S9(7)           COMP-3.                  
045900*                                 DATUM SENAST EFTERFRÅGAD                
046000*                                 DATE LATEST DEMAND                      
046100        05 TIREFMPB          PIC S9(7)           COMP-3.                  
046200*                                 DATUM MANUELL PROGNOS REFILLING         
046300*                                 DATE MANUAL FORECAST REFILLING          
046400        05 TIREFPAF          PIC S9(7)           COMP-3.                  
046500*                                 DATUM MANUELL PÅFYLLNADSKVANT           
046600*                                 DATE MANUAL REFILLING QTY               
046700        05 TIREFPKT          PIC S9(7)           COMP-3.                  
046800*                                 DATUM MANUELL REFILLPUNKT               
046900*                                 DATE MANUAL REFILLING POINT             
047000        05 TIREFSTA          PIC S9(7)           COMP-3.                  
047100*                                 DATUM AKT/PASS REFILLARTIKEL            
047200*                                 DATE ACT/PASS REFILLPART                
047300        05 TIREFSTO          PIC S9(7)           COMP-3.                  
047400*                                 BEORDRINGSSTOPPAD T.OM.                 
047500*                                 STOPPED FOR ORDERING UNTIL              
047600        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
047700*                                 DATUM RETUR BEORDRING                   
047800*                                 DATE ISSUE OF RETURN ORDER              
047900        05 TISKROT           PIC S9(7)           COMP-3.                  
048000*                                 SKROTNINGSDATUM                         
048100*                                 DATE OF SCRAPPING                       
048200        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
048300*                                 BEORDRAD SKROTNINGSDATUM                
048400*                                 DATE OF SCRAPPING DECISION              
048500        05 TISPARR-KVAL      PIC 9(6).                                    
048600*                                 SPÄRRAD DATUM KVALITETSFEL              
048700*                                 BLOCKED DATE QUALITY ERROR              
048800        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
048900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
049000*                                 AREA ADDRESS CROSS DOCKING WARE         
049100*                                 HOUSE                                   
049200        05 FLCDREL           PIC X.                                       
049300*                                 OMGÅENDE RELEASE AV CD-REFILL           
049400*                                 IMMEDIATE RELEASE OF CD-REFILL          
049500        05 KVDAGAR-CDBEH     PIC S9(3)           COMP-3.                  
049600*                                 NO. OF DAYS TO BE USED WHEN             
049700*                                 CALCULATING CD REFILLORDERS             
049800        05 KVOT-NDC-INNEV    PIC S9(7)           COMP-3.                  
049900*                                 ORDERTRÄFFAR LEV FRÅN NDC               
050000*                                 ORDERHITS ON NDC                        
050100        05 KVOT-NDC-RULL     PIC S9(7)           COMP-3.                  
050200*                                 ORDERTRÄFFAR LEV FRÅN NDC               
050300*                                 ORDERHITS ON NDC                        
050400        05 KVOT-MISS-INNEV   PIC S9(7)           COMP-3.                  
050500*                                 ORDERTRÄFFAR LEV FRÅN CDC               
050600*                                 ORDERHITS ON SDC FORWARDED              
050700*                                 TO CDC                                  
050800        05 KVOT-MISS-RULL    PIC S9(7)           COMP-3.                  
050900*                                 ORDERTRÄFFAR LEV FRÅN CDC               
051000*                                 ORDERHITS ON SDC FORWARDED              
051100*                                 TO CDC                                  
051200        05 TIINLINL          PIC S9(7)           COMP-3.                  
051300*                                 RAPPORTERINGSDATUM INLAGD (R32)         
051400*                                 DATE OF REPORTED IN STOCK (R32)         
051500*** END OF VILMAII-COPY LENGTH= 642 BYTES                                 
