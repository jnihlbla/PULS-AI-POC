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
001700*                                 DÄR IDDC  = 41 NDC RUTHERFORD           
001800*                                             42 NDC ATLANTA              
001900*                                             43 NDC LOS ANGELES          
002000*                                             51 NDC TORONTO              
002100*                                                                         
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
004100*                                 WHERE IDDC = 41 NDC RUTHERFORD          
004200*                                              42 NDC ATLANTA             
004300*                                              43 NDC LOS ANGELES         
004400*                                              51 NDC TORONTO             
004500*                                                                         
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
010500        05 IDINK             PIC S9(3)           COMP-3.                  
010600*                                 INKÖPARNUMMER                           
010700*                                 PURCHASE IDENTIFICATION NUMBER          
010800        05 IDLEVNR           PIC X(5).                                    
010900*                                 LEVERANTÖRNUMMER                        
011000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011100        05 IDLKTO            PIC S9(7)           COMP-3.                  
011200*                                 LAGERKONTO (FFHHHUU)                    
011300*                                 STOCK ACCOUNT (CCMMMSS)                 
011400        05 IDPROJ            PIC X(4).                                    
011500*                                 PARTS PROJEKTIDENTITET                  
011600*                                 PARTS PROJECT IDENTITY                  
011700        05 IDPSN             PIC 9(3).                                    
011800*                                 PROPER SHIPPING NAME                    
011900*                                 PROPER SHIPPING NAME                    
012000        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
012100*                                 PERSONKOD REFILLANSVARIG                
012200*                                 REFILL RESPONSIBLE ID                   
012300        05 IDPSN-US          PIC 9(3).                                    
012400*                                 PROPER SHIPPING NAME USA                
012500*                                 PROPER SHIPPING NAME USA                
012600        05 IDPSN-CA          PIC 9(3).                                    
012700*                                 PROPER SHIPPING NAME KANADA             
012800*                                 PROPER SHIPPING NAME CANADA             
012900        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
013000*                                 STATISTISKT NUMMER                      
013100*                                 1 = NORSKT                              
013200*                                 2 = ENGELSKT                            
013300*                                 3 = BELGISKT                            
013400*                                 4 = PERUANSKT                           
013500*                                 5 = SVENSKT                             
013600*                                 6 =                                     
013700*                                 STATISTICAL NO.                         
013800        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
013900*                                 STATISTISKT NUMMER                      
014000*                                 1 = NORSKT                              
014100*                                 2 = ENGELSKT                            
014200*                                 3 = BELGISKT                            
014300*                                 4 = PERUANSKT                           
014400*                                 5 = SVENSKT                             
014500*                                 6 =                                     
014600*                                 STATISTICAL NO.                         
014700        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
014800*                                 STATISTISKT NUMMER                      
014900*                                 1 = NORSKT                              
015000*                                 2 = ENGELSKT                            
015100*                                 3 = BELGISKT                            
015200*                                 4 = PERUANSKT                           
015300*                                 5 = SVENSKT                             
015400*                                 6 =                                     
015500*                                 STATISTICAL NO.                         
015600        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
015700*                                 STATISTISKT NUMMER                      
015800*                                 1 = NORSKT                              
015900*                                 2 = ENGELSKT                            
016000*                                 3 = BELGISKT                            
016100*                                 4 = PERUANSKT                           
016200*                                 5 = SVENSKT                             
016300*                                 6 =                                     
016400*                                 STATISTICAL NO.                         
016500        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
016600*                                 STATISTISKT NUMMER                      
016700*                                 1 = NORSKT                              
016800*                                 2 = ENGELSKT                            
016900*                                 3 = BELGISKT                            
017000*                                 4 = PERUANSKT                           
017100*                                 5 = SVENSKT                             
017200*                                 6 =                                     
017300*                                 STATISTICAL NO.                         
017400        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
017500*                                 STATISTISKT NUMMER                      
017600*                                 1 = NORSKT                              
017700*                                 2 = ENGELSKT                            
017800*                                 3 = BELGISKT                            
017900*                                 4 = PERUANSKT                           
018000*                                 5 = SVENSKT                             
018100*                                 6 =                                     
018200*                                 STATISTICAL NO.                         
018300        05 KDAGE             PIC X.                                       
018400*                                 AGE-CODE                                
018500*                                 AGE-CODE                                
018600        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018700*                                 HANTERINGSKOD                           
018800*                                 HANDLING CODE                           
018900        05 KDARTURS          PIC X(2).                                    
019000*                                 ARTIKELURSPRUNGSKOD                     
019100*                                 COUNTRY OF ORIGIN                       
019200        05 KDBPSR            PIC S9              COMP-3.                  
019300*                                 BASLAGERFÖRSLAGSNIVÅ                    
019400*                                 BASIC PART STOCK RECOMMENDATION         
019500        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019600*                                 EMBALLAGEKOD 0                          
019700        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019800*                                 EMBALLAGEKOD 1                          
019900        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
020000*                                 EMBALLAGEKOD 2                          
020100        05 KDERS             PIC S9(3)           COMP-3.                  
020200*                                 ERSÄTTNINGSKOD                          
020300*                                 SUPERSESSION CODE                       
020400        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
020500*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020600*                                 OBSOLETION SUPERSESSION CODE            
020700        05 KDFARLIG          PIC S9              COMP-3.                  
020800*                                 KOD FÖR FARLIGT GODS                    
020900*                                 DANGEROUS GOODS CODE                    
021000        05 KDGK              PIC S9              COMP-3.                  
021100*                                 GODSMOTTAGAREKOD                        
021200*                                 GOODS RECEIVING WAREHOUSE CODE          
021300        05 KDPRODSL          PIC S9(3)           COMP-3.                  
021400*                                 PRODUKTSLAG                             
021500*                                 PRODUCT GROUP                           
021600        05 KDSORT            PIC X(2).                                    
021700*                                 SORT-KOD                                
021800*                                 UNIT OF MEASURE                         
021900        05 KDSPEEMB          PIC 9.                                       
022000*                                 SPECIALEMBALLAGEKOD                     
022100*                                 SPECIAL PACKING CODE                    
022200        05 KDSRA             PIC S9(3)           COMP-3.                  
022300*                                 SRA-KOD                                 
022400*                                 SRA CODE                                
022500        05 KDUART            PIC X.                                       
022600*                                 UNDANTAGSARTIKEL                        
022700*                                 EXECPTION PARTS                         
022800        05 KDVVKL            PIC S9              COMP-3.                  
022900*                                 VOLYMVÄRDESKLASS                        
023000*                                 VOLUME VALUE CLASS                      
023100        05 KDYTBEH           PIC S9(3)           COMP-3.                  
023200*                                 YTBEHANDLINGSKOD                        
023300*                                                                         
023400        05 KVPALL            PIC S9(7)           COMP-3.                  
023500*                                 ANTAL I PALL                            
023600*                                 QUANTITY IN PALLET                      
023700        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023800*                                 ANTAL I Q0 FÖRPACKNING                  
023900        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
024000*                                 ANTAL I Q1 FÖRPACKNING                  
024100*                                 QUANTITY IN BULK PACK Q1                
024200        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
024300*                                 ANTAL I Q2 FÖRPACKNING                  
024400*                                 QUANTITY IN BULK PACK Q2                
024500        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024600*                                 ANTAL I Q3 FÖRPACKNING                  
024700*                                 QUANTITY IN BULK PACK Q3                
024800        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024900*                                 ANTAL I Q4 FÖRPACKNING                  
025000*                                 QUANTITY IN BULK PACK Q4                
025100        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
025200*                                 BESTÄLLNINGSPRIS I KRONOR               
025300*                                 ORDER PRICE SWEDISH CURRENCY            
025400        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
025500*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025600*                                 GROSS-PRICE EXPORT                      
025700*                                  (FOB-GROSS)                            
025800        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025900*                                 ARTIKELNS SJÄLVKOSTNAD                  
026000*                                 COST OF SALES                           
026100        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
026200*                                 ARTIKELSTANDARDPRIS                     
026300*                                 STANDARD PRICE                          
026400        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
026500*                                 INKÖPSPRIS                              
026600*                                 PURCHASE PRICE                          
026700        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026800*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026900*                                 DATE OF SUPERSESSION (YYWWD)            
027000        05 TIFINLV           PIC S9(5)           COMP-3.                  
027100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
027200*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
027300        05 VKART             PIC S9(7)           COMP-3.                  
027400*                                 ARTIKELVIKT (G)                         
027500*                                 PART WEIGHT (G)                         
027600        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027700*                                 ARTIKELVOLYM NETTO (CM3)                
027800*                                 PART NET VOLUME    (CM3)                
027900        05 IDKAT-1           PIC X(5).                                    
028000*                                 KATALOGBETECKNING                       
028100        05 IDKAT-2           PIC X(5).                                    
028200*                                 KATALOGBETECKNING                       
028300        05 IDKAT-3           PIC X(5).                                    
028400*                                 KATALOGBETECKNING                       
028500        05 FILLER            PIC X(78).                                   
028600     03 NDC-INFO.                                                         
028700*                                 INFO SOM GÄLLER ENBART NDC              
028800*                                                                         
028900*                                                                         
029000*                                 INFO VALID ONLY FOR NDC                 
029100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029200*                                 LAGEROMRÅDE                             
029300*                                 AREA                                    
029400        05 ADGANG            PIC S9(3)           COMP-3.                  
029500*                                 GÅNG                                    
029600*                                 AISLE                                   
029700        05 ADPLATS           PIC S9(5)           COMP-3.                  
029800*                                 LAGERPLATSNUMMER                        
029900*                                 LOCATION                                
030000        05 DAPUBL            PIC 9(8).                                    
030100*                                 PUBLICERINGSDATUM PER ART/DC            
030200*                                 YYYYMMDD                                
030300*                                 DATE OF PUBLISHING PER PART/DC          
030400        05 FLFLYG            PIC X.                                       
030500*                                 FLYGARTIKEL                             
030600*                                 PART NUMBER SENT BY AIR                 
030700        05 FLREFBEO          PIC X.                                       
030800*                                 AUTOMATISK REFILL BEORDRING?            
030900*                                 AUTOMATIC REFILL ORDERING?              
031000        05 FLREFILL          PIC X.                                       
031100*                                 REFILLARTIKEL                           
031200*                                 REFILLPART                              
031300        05 FLREFNYO          PIC X.                                       
031400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031500*                                 WAIT FOR NEXT DEMAND                    
031600        05 FLSKROT-BEORD     PIC X.                                       
031700*                                 SKROTNING BEORDRAD AV ANSK              
031800*                                 SCRAPPING ORDERED BY PROCURER           
031900        05 FLWILSON          PIC X.                                       
032000*                                 WILSONFORMEL                            
032100*                                 FLAG TO USE WILSON OR NOT               
032200        05 FLORDSP           PIC X.                                       
032300*                                 ORDERSPÄRR                              
032400*                                 ORDER BLOCKED                           
032500        05 FLORDSP-EJRO      PIC X.                                       
032600*                                 ORDERSPÄRR EJ RESTNOTERING              
032700*                                 ORDER BLOCK NO BACKORDERING             
032800        05 FLSEASON          PIC X.                                       
032900*                                 SÄSONG PÅ ARTIKEL                       
033000*                                 SEASON MARK PER PART                    
033100        05 FLSPBULK          PIC X.                                       
033200*                                 FLAGGA SPÄRR MOT BULKORDER              
033300*                                 FLAG BULKORDER STOP                     
033400        05 IDLEVNR-NDC       PIC X(5).                                    
033500*                                 LEVERANTÖRNUMMER                        
033600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
033700        05 KDFREKKL          PIC X.                                       
033800*                                 FREKVENSKLASS                           
033900*                                 FREQ. CLASS                             
034000        05 KDLEVSP           PIC S9(3)           COMP-3.                  
034100*                                 SPÄRRKOD LEVERANS                       
034200*                                 DELIVERY BLOCKING CODE                  
034300        05 KDPRISKL          PIC X.                                       
034400*                                 PRISKLASS                               
034500*                                 PRICE CLASS                             
034600        05 KDPSLLOC          PIC 9(2).                                    
034700*                                 PRODUKTSLAG LOKALT                      
034800*                                 PRODUCT GROUP LOCAL                     
034900        05 KDREFSTA          PIC X.                                       
035000*                                 STATUS REFILLARTIKEL                    
035100*                                 STATUS REFILLPART                       
035200        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
035300*                                 DEL AV AK PÅ VÄG                        
035400*                                 PART OF AK ON ITS WAY                   
035500        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
035600*                                 DEL AV AK SOM LIGGER I SDC              
035700*                                 PART OF AK IN THE SDC                   
035800        05 KVBEART           PIC S9(7)           COMP-3.                  
035900*                                 BESTÄLLT ANTAL STYCKEN                  
036000*                                 ORDERED QUANTITY                        
036100        05 KVEFRS            PIC S9(7)           COMP-3.                  
036200*                                 EJ FAKTURERAT ANTAL STYCK               
036300*                                 ORDERED NOT INVOICED QTY                
036400        05 KVINVS            PIC S9(7)           COMP-3.                  
036500*                                 INVENTERINGSSALDO                       
036600*                                 STOCK-TAKING BALANCE                    
036700        05 KVLS              PIC S9(7)           COMP-3.                  
036800*                                 LAGERSALDO                              
036900*                                 STOCK BALANCE                           
037000        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
037100*                                 ORDERINGÅNG LEV FRÅN CDC                
037200*                                 ORDERED PCS PER TIME UNIT               
037300*                                 FORWARDED TO CDC                        
037400        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
037500*                                 ORDERINGÅNG TILL SDC                    
037600*                                 ORDERED PCS PER TIME UNIT               
037700*                                 ORDERED FROM SDC                        
037800        05 KVOI-IAAR         PIC S9(7)           COMP-3.                  
037900*                                 ORDERINGÅNG TILL DC INNEV ÅR            
038000*                                 ORDERED PCS PER TIME UNIT               
038100*                                 ORDERED FROM DC                         
038200        05 KVOI-INNEV        PIC S9(7)           COMP-3.                  
038300*                                 ORDERINGÅNG TILL DC INNEV PER           
038400*                                 ORDERED PCS THIS PERIOD                 
038500*                                 ORDERED FROM DC                         
038600        05 KVOI-FOREG        OCCURS 2 TIMES                               
038700                             PIC S9(7)           COMP-3.                  
038800*                                 ORDERINGÅNG TILL DC FÖREG ÅR            
038900*                                 ORDERED PCS LAST YEAR                   
039000*                                 ORDERED FROM DC                         
039100        05 KVOI-RP           OCCURS 12 TIMES                              
039200                             PIC S9(7)           COMP-3.                  
039300*                                 ORDERINGÅNG TILL DC                     
039400*                                 ORDERED PCS PER TIME UNIT               
039500*                                 ORDERED FROM DC                         
039600*                                 WEIGHTENED BY 4.33                      
039700        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
039800*                                 ORDERKÖSALDO, KLASS 2-4                 
039900*                                 ORDER QUEUE BALANCE, CLASS 2-4          
040000        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
040100*                                 ORDERKÖSALDO, KLASS 1                   
040200*                                 ORDER QUEUE BALANCE, CLASS 1            
040300        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
040400*                                 ORDERTRÄFFAR LEV FRÅN CDC               
040500*                                 ORDERHITS ON SDC FORWARDED              
040600*                                 TO CDC                                  
040700        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
040800*                                 ORDERTRÄFFAR PÅ SDC                     
040900*                                 ORDERHITS ON SDC                        
041000        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
041100*                                 PERIODBEHOV REFILLING                   
041200*                                 FORECAST REFILLING                      
041300        05 KVPB-REF-US       PIC S9(6)V9(1)      COMP-3.                  
041400*                                 PERIODBEHOV REFILLING USA               
041500*                                 FORECAST REFILLING US                   
041600        05 KVREFBER          PIC S9(7)           COMP-3.                  
041700*                                 BERÄKNAD REFILLINGKVANTITET             
041800*                                 CALCULATED REFILLING QUANTITY           
041900        05 KVREFOVL          PIC S9(7)           COMP-3.                  
042000*                                 BERÄKNAD ÖVERLAGERPUNKT                 
042100*                                 CALCULATED OVERSTOCK POINT              
042200        05 KVREFPKT          PIC S9(7)           COMP-3.                  
042300*                                 BERÄKNAD PÅFYLLNADSPUNKT                
042400*                                 CALCULATED REFILLING POINT              
042500        05 KVRESS            PIC S9(7)           COMP-3.                  
042600*                                 RESERVERAT ANTAL ARTIKLAR               
042700*                                 QUANTITY RESERVED ITEMS                 
042800        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
042900*                                 ANTAL SENASTE RETURORDER                
043000*                                 QUANTITY LAST RETURNORDER               
043100        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
043200*                                 RESTORDERSALDO, KLASS 2-4               
043300*                                 BACK ORDER BALANCE, CLASS 2-4           
043400        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
043500*                                 RESTORDERSALDO, KLASS 1                 
043600*                                 BACK ORDER BALANCE, CLASS 1             
043700        05 KVSKROT           PIC S9(7)           COMP-3.                  
043800*                                 ANTAL SENASTE SKROTORDER                
043900*                                 QUANTITY LAST SCRAPORDER                
044000        05 KVUTRS            PIC S9(7)           COMP-3.                  
044100*                                 UTREDNINGSSALDO                         
044200*                                 INVESTIG.BALANCE                        
044300        05 PRAIRCO           PIC S9(7)V9(2)      COMP-3.                  
044400*                                 FLYGKOST PER ARTIKEL/DC SEK             
044500*                                 AIR COST PER PART/DC SEK                
044600        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
044700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
044800*                                 AVERAGE COST FOREIGN CURRENCY           
044900        05 REOSAEK           PIC S9(3)V9(1)      COMP-3.                  
045000*                                 SEASONAL UNCERTAINTY FACTOR             
045100        05 RESEASON          OCCURS 12 TIMES                              
045200                             PIC S9V9(2)         COMP-3.                  
045300*                                 SÄSONGSINDEX                            
045400        05 TIINVDAT          PIC S9(5)           COMP-3.                  
045500*                                 INVENTERINGSDATUM                       
045600*                                 STOCKTAKING DATE                        
045700        05 TIORDREG          PIC S9(7)           COMP-3.                  
045800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
045900*                                 ORDER REGISTRATION DATE  YYMMDD         
046000        05 TIREFEFT          PIC S9(7)           COMP-3.                  
046100*                                 DATUM SENAST EFTERFRÅGAD                
046200*                                 DATE LATEST DEMAND                      
046300        05 TIREFMPB          PIC S9(7)           COMP-3.                  
046400*                                 DATUM MANUELL PROGNOS REFILLING         
046500*                                 DATE MANUAL FORECAST REFILLING          
046600        05 TIREFPAF          PIC S9(7)           COMP-3.                  
046700*                                 DATUM MANUELL PÅFYLLNADSKVANT           
046800*                                 DATE MANUAL REFILLING QTY               
046900        05 TIREFPKT          PIC S9(7)           COMP-3.                  
047000*                                 DATUM MANUELL REFILLPUNKT               
047100*                                 DATE MANUAL REFILLING POINT             
047200        05 TIREFSTA          PIC S9(7)           COMP-3.                  
047300*                                 DATUM AKT/PASS REFILLARTIKEL            
047400*                                 DATE ACT/PASS REFILLPART                
047500        05 TIREFSTO          PIC S9(7)           COMP-3.                  
047600*                                 BEORDRINGSSTOPPAD T.OM.                 
047700*                                 STOPPED FOR ORDERING UNTIL              
047800        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
047900*                                 DATUM RETUR BEORDRING                   
048000*                                 DATE ISSUE OF RETURN ORDER              
048100        05 TISKROT           PIC S9(7)           COMP-3.                  
048200*                                 SKROTNINGSDATUM                         
048300*                                 DATE OF SCRAPPING                       
048400        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
048500*                                 BEORDRAD SKROTNINGSDATUM                
048600*                                 DATE OF SCRAPPING DECISION              
048700        05 TISPARR-KVAL      PIC 9(6).                                    
048800*                                 SPÄRRAD DATUM KVALITETSFEL              
048900*                                 BLOCKED DATE QUALITY ERROR              
049000        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
049100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
049200*                                 AREA ADDRESS CROSS DOCKING WARE         
049300*                                 HOUSE                                   
049400        05 FLCDREL           PIC X.                                       
049500*                                 OMGÅENDE RELEASE AV CD-REFILL           
049600*                                 IMMEDIATE RELEASE OF CD-REFILL          
049700        05 KVDAGAR-CDBEH     PIC S9(3)           COMP-3.                  
049800*                                 NO. OF DAYS TO BE USED WHEN             
049900*                                 CALCULATING CD REFILLORDERS             
050000        05 FILLER            PIC X(15).                                   
050100*** END OF VILMAII-COPY LENGTH= 637 BYTES                                 
