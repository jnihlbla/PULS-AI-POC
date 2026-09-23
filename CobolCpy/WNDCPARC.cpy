000100 01  WNDCPARC.                                                            
000200*                                 PRIMÄREXTRAKT                           
000300*                                                                         
000400*                                 LAGERBAND                               
000500*                                 COPY-TEXT FÖR EXTRAKT WNDCDAY           
000600*                                                       WNDCWEEK          
000700*                                                       WNDCACC           
000800*                                                       WNDCPLAN          
000900*                                                       WNDCYEAR          
001000*                                 MED UTÖKAD INFO FÖR KINA                
001100*                                 FRAMSTÄLLS VARJE :                      
001200*                                            MORGON (DAGLIGEN)            
001300*                                            VECKOSLUT                    
001400*                                            REDOVISNINGSPERIOD           
001500*                                            PLANERINGSPERIOD             
001600*                                            ÅRS-SLUT                     
001700*                                                                         
001800*                                 DÄR IDDC  = NDC-CN                      
002100*                                                                         
002200*                                 1 POST/ARTIKELNUMMER/IDDC               
002300*                                                                         
002400*                                 PRIMARY EXTRACT                         
002500*                                                                         
002600*                                 PART INFORMATION FILE FOR NDC           
002700*                                 COPY-TEXT FOR EXTRACT WNDCDAY           
002800*                                                       WNDCWEEK          
002900*                                                       WNDCACC           
003000*                                                       WNDCPLAN          
003100*                                                       WNDCYEAR          
003200*                                 WITH EXCEEDED INFO FOR CHINA            
003300*                                                                         
003400*                                 CREATED EVERY :                         
003500*                                               MORNING (DAILY)           
003600*                                               WEEKEND                   
003700*                                               ACCOUNTPERIOD             
003800*                                               PLANNINGPERIOD            
003900*                                               YEAR                      
004000*                                                                         
004100*                                 WHERE IDDC = NDC-CN                     
004400*                                                                         
004500*                                 1 RECORD/PARTNUMBER/IDDC                
004600*                                                                         
004700     03 IDARTNR              PIC S9(9)           COMP-3.                  
004800*                                 ARTIKELNUMMER                           
004900*                                 PART NUMBER                             
005000     03 REKSIFFR             PIC S9              COMP-3.                  
005100*                                 KONTROLLSIFFRA                          
005200*                                 PART NO CHECK DIGIT                     
005300     03 IDDC                 PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500*                                 WAREHOUSE IDENTIFIER                    
005600     03 ARTIKEL-INFO.                                                     
005700*                                 ARTIKELUPPGIFTER                        
005800*                                                                         
005900*                                 PART INFORMATION                        
006000        05 BEART-SVE         PIC X(25).                                   
006100*                                 SVENSK ARTIKELBENÄMNING                 
006200        05 BEART-ENG         PIC X(25).                                   
006300*                                 ENGELSK ARTIKELBENÄMNING                
006400        05 BEFT              PIC S9(3)           COMP-3.                  
006500*                                 FÖRPACKNINGSTYP                         
006600*                                 PACKAGING TYPE                          
006700        05 FLAVRART          PIC X.                                       
006800*                                 AVROPSARTIKEL                           
006900        05 FLERS             PIC X.                                       
007000*                                 TILLKOMMANDE ARTIKEL ?                  
007100        05 FLGEMART          PIC X.                                       
007200*                                 FLAGGA GEMENSAM ARTIKEL                 
007300*                                 COMMON PART FLAG                        
007400        05 FLIART            PIC X.                                       
007500*                                 ARTIKELN INGÅR I SATS                   
007600*                                 PART IN KIT                             
007700        05 FLJIT             PIC X.                                       
007800*                                 JUST-IN-TIME FLAGGA                     
007900*                                 JUST-IN-TIME FLAG                       
008000        05 FLLSRDEL          PIC X.                                       
008100*                                 LEVERERAS SOM RESDEL                    
008200        05 FLTPO1            PIC X.                                       
008300*                                 ARTIKELN GODKÄND FÖR TPO1               
008400*                                 TPO1 ALLOWED FOR ARTICLE                
008500        05 IDANSK            PIC S9(3)           COMP-3.                  
008600*                                 ANSKAFFARNUMMER                         
008700*                                 PROCURER NO.                            
008800        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
008900*                                 EMBALLAGEARTIKELNR FÖR Q0               
009000        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
009100*                                 EMBALLAGEARTIKELNR FÖR Q1               
009200        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
009300*                                 EMBALLAGEARTIKELNR FÖR Q2               
009400        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
009500*                                 EMBALLAGEARTIKELNR FÖR Q3               
009600        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
009700*                                 EMBALLAGEARTIKELNR FÖR Q4               
009800        05 IDBERED           PIC S9(3)           COMP-3.                  
009900*                                 BEREDARENUMMER                          
010000        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
010100*                                 FUNKTIONSGRUPP                          
010200*                                 FUNCTION GROUP                          
010300        05 FILLERX2          PIC X(2).                                    
010400        05 IDLEVNR           PIC X(5).                                    
010500*                                 LEVERANTÖRNUMMER                        
010600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010700        05 IDLKTO            PIC S9(7)           COMP-3.                  
010800*                                 LAGERKONTO (FFHHHUU)                    
010900*                                 STOCK ACCOUNT (CCMMMSS)                 
011000        05 IDPROJ            PIC X(4).                                    
011100*                                 PARTS PROJEKTIDENTITET                  
011200*                                 PARTS PROJECT IDENTITY                  
011300        05 IDPSN             PIC 9(3).                                    
011400*                                 PROPER SHIPPING NAME                    
011500*                                 PROPER SHIPPING NAME                    
011600        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
011700*                                 PERSONKOD REFILLANSVARIG                
011800*                                 REFILL RESPONSIBLE ID                   
011900        05 IDPSN-DC          PIC 9(3).                                    
012000*                                 PROPER SHIPPING NAME PER XDC            
012100*                                 PROPER SHIPPING NAME XDC                
012200        05 FILLER            PIC X(3).                                    
012300        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
012400*                                 STATISTISKT NUMMER                      
012500*                                 1 = NORSKT                              
012600*                                 2 = ENGELSKT                            
012700*                                 3 = BELGISKT                            
012800*                                 4 = PERUANSKT                           
012900*                                 5 = SVENSKT                             
013000*                                 6 =                                     
013100*                                 STATISTICAL NO.                         
013200        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
013300*                                 STATISTISKT NUMMER                      
013400*                                 1 = NORSKT                              
013500*                                 2 = ENGELSKT                            
013600*                                 3 = BELGISKT                            
013700*                                 4 = PERUANSKT                           
013800*                                 5 = SVENSKT                             
013900*                                 6 =                                     
014000*                                 STATISTICAL NO.                         
014100        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
014200*                                 STATISTISKT NUMMER                      
014300*                                 1 = NORSKT                              
014400*                                 2 = ENGELSKT                            
014500*                                 3 = BELGISKT                            
014600*                                 4 = PERUANSKT                           
014700*                                 5 = SVENSKT                             
014800*                                 6 =                                     
014900*                                 STATISTICAL NO.                         
015000        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
015100*                                 STATISTISKT NUMMER                      
015200*                                 1 = NORSKT                              
015300*                                 2 = ENGELSKT                            
015400*                                 3 = BELGISKT                            
015500*                                 4 = PERUANSKT                           
015600*                                 5 = SVENSKT                             
015700*                                 6 =                                     
015800*                                 STATISTICAL NO.                         
015900        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
016000*                                 STATISTISKT NUMMER                      
016100*                                 1 = NORSKT                              
016200*                                 2 = ENGELSKT                            
016300*                                 3 = BELGISKT                            
016400*                                 4 = PERUANSKT                           
016500*                                 5 = SVENSKT                             
016600*                                 6 =                                     
016700*                                 STATISTICAL NO.                         
016800        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
016900*                                 STATISTISKT NUMMER                      
017000*                                 1 = NORSKT                              
017100*                                 2 = ENGELSKT                            
017200*                                 3 = BELGISKT                            
017300*                                 4 = PERUANSKT                           
017400*                                 5 = SVENSKT                             
017500*                                 6 =                                     
017600*                                 STATISTICAL NO.                         
017700        05 KDAGE             PIC X.                                       
017800*                                 AGE-CODE                                
017900*                                 AGE-CODE                                
018000        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018100*                                 HANTERINGSKOD                           
018200*                                 HANDLING CODE                           
018300        05 KDARTURS          PIC X(2).                                    
018400*                                 ARTIKELURSPRUNGSKOD                     
018500*                                 COUNTRY OF ORIGIN                       
018600        05 KDBPSR            PIC S9              COMP-3.                  
018700*                                 BASLAGERFÖRSLAGSNIVÅ                    
018800*                                 BASIC PART STOCK RECOMMENDATION         
018900        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019000*                                 EMBALLAGEKOD 0                          
019100        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019200*                                 EMBALLAGEKOD 1                          
019300        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
019400*                                 EMBALLAGEKOD 2                          
019500        05 KDERS             PIC S9(3)           COMP-3.                  
019600*                                 ERSÄTTNINGSKOD                          
019700*                                 SUPERSESSION CODE                       
019800        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
019900*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020000*                                 OBSOLETION SUPERSESSION CODE            
020100        05 KDFARLIG          PIC S9              COMP-3.                  
020200*                                 KOD FÖR FARLIGT GODS                    
020300*                                 DANGEROUS GOODS CODE                    
020400        05 KDGK              PIC S9              COMP-3.                  
020500*                                 GODSMOTTAGAREKOD                        
020600*                                 GOODS RECEIVING WAREHOUSE CODE          
020700        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020800*                                 PRODUKTSLAG                             
020900*                                 PRODUCT GROUP                           
021000        05 KDSORT            PIC X(2).                                    
021100*                                 SORT-KOD                                
021200*                                 UNIT OF MEASURE                         
021300        05 KDSPEEMB          PIC 9.                                       
021400*                                 SPECIALEMBALLAGEKOD                     
021500*                                 SPECIAL PACKING CODE                    
021600        05 KDSRA             PIC S9(3)           COMP-3.                  
021700*                                 SRA-KOD                                 
021800*                                 SRA CODE                                
021900        05 KDUART            PIC X.                                       
022000*                                 UNDANTAGSARTIKEL                        
022100*                                 EXECPTION PARTS                         
022200        05 KDVVKL            PIC S9              COMP-3.                  
022300*                                 VOLYMVÄRDESKLASS                        
022400*                                 VOLUME VALUE CLASS                      
022500        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022600*                                 YTBEHANDLINGSKOD                        
022700*                                                                         
022800        05 KVPALL            PIC S9(7)           COMP-3.                  
022900*                                 ANTAL I PALL                            
023000*                                 QUANTITY IN PALLET                      
023100        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023200*                                 ANTAL I Q0 FÖRPACKNING                  
023300        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
023400*                                 ANTAL I Q1 FÖRPACKNING                  
023500*                                 QUANTITY IN BULK PACK Q1                
023600        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023700*                                 ANTAL I Q2 FÖRPACKNING                  
023800*                                 QUANTITY IN BULK PACK Q2                
023900        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024000*                                 ANTAL I Q3 FÖRPACKNING                  
024100*                                 QUANTITY IN BULK PACK Q3                
024200        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024300*                                 ANTAL I Q4 FÖRPACKNING                  
024400*                                 QUANTITY IN BULK PACK Q4                
024500        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024600*                                 BESTÄLLNINGSPRIS I KRONOR               
024700*                                 ORDER PRICE SWEDISH CURRENCY            
024800        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
024900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025000*                                 GROSS-PRICE EXPORT                      
025100*                                  (FOB-GROSS)                            
025200        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025300*                                 ARTIKELNS SJÄLVKOSTNAD                  
025400*                                 COST OF SALES                           
025500        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025600*                                 ARTIKELSTANDARDPRIS                     
025700*                                 STANDARD PRICE                          
025800        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
025900*                                 INKÖPSPRIS                              
026000*                                 PURCHASE PRICE                          
026100        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026300*                                 DATE OF SUPERSESSION (YYWWD)            
026400        05 TIFINLV           PIC S9(5)           COMP-3.                  
026500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026600*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026700        05 VKART             PIC S9(7)           COMP-3.                  
026800*                                 ARTIKELVIKT (G)                         
026900*                                 PART WEIGHT (G)                         
027000        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027100*                                 ARTIKELVOLYM NETTO (CM3)                
027200*                                 PART NET VOLUME    (CM3)                
027300        05 IDKAT-1           PIC X(5).                                    
027400*                                 KATALOGBETECKNING                       
027500        05 IDKAT-2           PIC X(5).                                    
027600*                                 KATALOGBETECKNING                       
027700        05 IDKAT-3           PIC X(5).                                    
027800*                                 KATALOGBETECKNING                       
027900        05 IDINK             PIC X(4).                                    
028000*                                 INKÖPARNUMMER                           
028100*                                 PURCHASE IDENTIFICATION NUMBER          
028200        05 FILLER            PIC X(74).                                   
028300     03 NDC-INFO.                                                         
028400*                                 INFO SOM GÄLLER ENBART NDC              
028500*                                                                         
028600*                                                                         
028700*                                 INFO VALID ONLY FOR NDC                 
028800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028900*                                 LAGEROMRÅDE                             
029000*                                 AREA                                    
029100        05 ADGANG            PIC S9(3)           COMP-3.                  
029200*                                 GÅNG                                    
029300*                                 AISLE                                   
029400        05 ADPLATS           PIC S9(5)           COMP-3.                  
029500*                                 LAGERPLATSNUMMER                        
029600*                                 LOCATION                                
029700        05 DAPUBL            PIC 9(8).                                    
029800*                                 PUBLICERINGSDATUM PER ART/LAND          
029900*                                 DATE OF PUBLISHING PART/COUNTRY         
030000        05 FLFLYG            PIC X.                                       
030100*                                 FLYGARTIKEL                             
030200*                                 PART NUMBER SENT BY AIR                 
030300        05 FLREFBEO          PIC X.                                       
030400*                                 AUTOMATISK REFILL BEORDRING?            
030500*                                 AUTOMATIC REFILL ORDERING?              
030600        05 FLREFILL          PIC X.                                       
030700*                                 REFILLARTIKEL                           
030800*                                 REFILLPART                              
030900        05 FLREFNYO          PIC X.                                       
031000*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031100*                                 WAIT FOR NEXT DEMAND                    
031200        05 FLSKROT-BEORD     PIC X.                                       
031300*                                 SKROTNING BEORDRAD AV ANSK              
031400*                                 SCRAPPING ORDERED BY PROCURER           
031500        05 FLWILSON          PIC X.                                       
031600*                                 WILSONFORMEL                            
031700*                                 FLAG TO USE WILSON OR NOT               
031800        05 FLORDSP           PIC X.                                       
031900*                                 ORDERSPÄRR                              
032000*                                 ORDER BLOCKED                           
032100        05 FLORDSP-EJRO      PIC X.                                       
032200*                                 ORDERSPÄRR EJ RESTNOTERING              
032300*                                 ORDER BLOCK NO BACKORDERING             
032400        05 FLSEASON          PIC X.                                       
032500*                                 SÄSONG PÅ ARTIKEL                       
032600*                                 SEASON MARK PER PART                    
032700        05 FLSPBULK          PIC X.                                       
032800*                                 FLAGGA SPÄRR MOT BULKORDER              
032900*                                 FLAG BULKORDER STOP                     
033000        05 IDLEVNR-NDC       PIC X(5).                                    
033100*                                 LEVERANTÖRNUMMER                        
033200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
033300        05 KDFREKKL          PIC X.                                       
033400*                                 FREKVENSKLASS                           
033500*                                 FREQ. CLASS                             
033600        05 KDLEVSP           PIC S9(3)           COMP-3.                  
033700*                                 SPÄRRKOD LEVERANS                       
033800*                                 DELIVERY BLOCKING CODE                  
033900        05 KDPRISKL          PIC X.                                       
034000*                                 PRISKLASS                               
034100*                                 PRICE CLASS                             
034200        05 KDPSLLOC          PIC 9(2).                                    
034300*                                 PRODUKTSLAG LOKALT                      
034400*                                 PRODUCT GROUP LOCAL                     
034500        05 KDREFSTA          PIC X.                                       
034600*                                 STATUS REFILLARTIKEL                    
034700*                                 STATUS REFILLPART                       
034800        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
034900*                                 DEL AV AK PÅ VÄG                        
035000*                                 PART OF AK ON ITS WAY                   
035100        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
035200*                                 DEL AV AK SOM LIGGER I SDC              
035300*                                 PART OF AK IN THE SDC                   
035400        05 KVBEART           PIC S9(7)           COMP-3.                  
035500*                                 BESTÄLLT ANTAL STYCKEN                  
035600*                                 ORDERED QUANTITY                        
035700        05 KVEFRS            PIC S9(7)           COMP-3.                  
035800*                                 EJ FAKTURERAT ANTAL STYCK               
035900*                                 ORDERED NOT INVOICED QTY                
036000        05 KVINVS            PIC S9(7)           COMP-3.                  
036100*                                 INVENTERINGSSALDO                       
036200*                                 STOCK-TAKING BALANCE                    
036300        05 KVLS              PIC S9(7)           COMP-3.                  
036400*                                 LAGERSALDO                              
036500*                                 STOCK BALANCE                           
036600        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
036700*                                 ORDERINGÅNG LEV FRÅN CDC                
036800*                                 ORDERED PCS PER TIME UNIT               
036900*                                 FORWARDED TO CDC                        
037000        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
037100*                                 ORDERINGÅNG TILL SDC                    
037200*                                 ORDERED PCS PER TIME UNIT               
037300*                                 ORDERED FROM SDC                        
037400        05 KVOI-IAAR         PIC S9(7)           COMP-3.                  
037500*                                 ORDERINGÅNG TILL DC INNEV ÅR            
037600*                                 ORDERED PCS PER TIME UNIT               
037700*                                 ORDERED FROM DC                         
037800        05 KVOI-INNEV        PIC S9(7)           COMP-3.                  
037900*                                 ORDERINGÅNG TILL DC INNEV PER           
038000*                                 ORDERED PCS THIS PERIOD                 
038100*                                 ORDERED FROM DC                         
038200        05 KVOI-FOREG        OCCURS 2 TIMES                               
038300                             PIC S9(7)           COMP-3.                  
038400*                                 ORDERINGÅNG TILL DC FÖREG ÅR            
038500*                                 ORDERED PCS LAST YEAR                   
038600*                                 ORDERED FROM DC                         
038700        05 KVOI-RP           OCCURS 12 TIMES                              
038800                             PIC S9(7)           COMP-3.                  
038900*                                 ORDERINGÅNG TILL DC                     
039000*                                 ORDERED PCS PER TIME UNIT               
039100*                                 ORDERED FROM DC                         
039200*                                 WEIGHTENED BY 4.33                      
039300        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
039400*                                 ORDERKÖSALDO, KLASS 2-4                 
039500*                                 ORDER QUEUE BALANCE, CLASS 2-4          
039600        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
039700*                                 ORDERKÖSALDO, KLASS 1                   
039800*                                 ORDER QUEUE BALANCE, CLASS 1            
039900        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
040000*                                 ORDERTRÄFFAR LEV FRÅN CDC               
040100*                                 ORDERHITS ON SDC FORWARDED              
040200*                                 TO CDC                                  
040300        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
040400*                                 ORDERTRÄFFAR PÅ SDC                     
040500*                                 ORDERHITS ON SDC                        
040600        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
040700*                                 PERIODBEHOV REFILLING                   
040800*                                 FORECAST REFILLING                      
040900        05 KVPB-REF-US       PIC S9(6)V9(1)      COMP-3.                  
041000*                                 PERIODBEHOV REFILLING USA               
041100*                                 FORECAST REFILLING US                   
041200        05 KVREFBER          PIC S9(7)           COMP-3.                  
041300*                                 BERÄKNAD REFILLINGKVANTITET             
041400*                                 CALCULATED REFILLING QUANTITY           
041500        05 KVREFOVL          PIC S9(7)           COMP-3.                  
041600*                                 BERÄKNAD ÖVERLAGERPUNKT                 
041700*                                 CALCULATED OVERSTOCK POINT              
041800        05 KVREFPKT          PIC S9(7)           COMP-3.                  
041900*                                 BERÄKNAD PÅFYLLNADSPUNKT                
042000*                                 CALCULATED REFILLING POINT              
042100        05 KVRESS            PIC S9(7)           COMP-3.                  
042200*                                 RESERVERAT ANTAL ARTIKLAR               
042300*                                 QUANTITY RESERVED ITEMS                 
042400        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
042500*                                 ANTAL SENASTE RETURORDER                
042600*                                 QUANTITY LAST RETURNORDER               
042700        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
042800*                                 RESTORDERSALDO, KLASS 2-4               
042900*                                 BACK ORDER BALANCE, CLASS 2-4           
043000        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
043100*                                 RESTORDERSALDO, KLASS 1                 
043200*                                 BACK ORDER BALANCE, CLASS 1             
043300        05 KVSKROT           PIC S9(7)           COMP-3.                  
043400*                                 ANTAL SENASTE SKROTORDER                
043500*                                 QUANTITY LAST SCRAPORDER                
043600        05 KVUTRS            PIC S9(7)           COMP-3.                  
043700*                                 UTREDNINGSSALDO                         
043800*                                 INVESTIGATION BALANCE                   
043900        05 PRAIRCO           PIC S9(7)V9(2)      COMP-3.                  
044000*                                 FLYGKOST PER ARTIKEL/DC SEK             
044100*                                 AIR COST PER PART/DC SEK                
044200        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
044300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
044400*                                 AVERAGE COST FOREIGN CURRENCY           
044500        05 REOSAEK           PIC S9(3)V9(1)      COMP-3.                  
044600*                                 SEASONAL UNCERTAINTY FACTOR             
044700        05 RESEASON          OCCURS 12 TIMES                              
044800                             PIC S9V9(2)         COMP-3.                  
044900*                                 SÄSONGSINDEX                            
045000        05 TIINVDAT          PIC S9(5)           COMP-3.                  
045100*                                 INVENTERINGSDATUM                       
045200*                                 STOCKTAKING DATE                        
045300        05 TIORDREG          PIC S9(7)           COMP-3.                  
045400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
045500*                                 ORDER REGISTRATION DATE  YYMMDD         
045600        05 TIREFEFT          PIC S9(7)           COMP-3.                  
045700*                                 DATUM SENAST EFTERFRÅGAD                
045800*                                 DATE LATEST DEMAND                      
045900        05 TIREFMPB          PIC S9(7)           COMP-3.                  
046000*                                 DATUM MANUELL PROGNOS REFILLING         
046100*                                 DATE MANUAL FORECAST REFILLING          
046200        05 TIREFPAF          PIC S9(7)           COMP-3.                  
046300*                                 DATUM MANUELL PÅFYLLNADSKVANT           
046400*                                 DATE MANUAL REFILLING QTY               
046500        05 TIREFPKT          PIC S9(7)           COMP-3.                  
046600*                                 DATUM MANUELL REFILLPUNKT               
046700*                                 DATE MANUAL REFILLING POINT             
046800        05 TIREFSTA          PIC S9(7)           COMP-3.                  
046900*                                 DATUM AKT/PASS REFILLARTIKEL            
047000*                                 DATE ACT/PASS REFILLPART                
047100        05 TIREFSTO          PIC S9(7)           COMP-3.                  
047200*                                 BEORDRINGSSTOPPAD T.OM.                 
047300*                                 STOPPED FOR ORDERING UNTIL              
047400        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
047500*                                 DATUM RETUR BEORDRING                   
047600*                                 DATE ISSUE OF RETURN ORDER              
047700        05 TISKROT           PIC S9(7)           COMP-3.                  
047800*                                 SKROTNINGSDATUM                         
047900*                                 DATE OF SCRAPPING                       
048000        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
048100*                                 BEORDRAD SKROTNINGSDATUM                
048200*                                 DATE OF SCRAPPING DECISION              
048300        05 TISPARR-KVAL      PIC 9(6).                                    
048400*                                 SPÄRRAD DATUM KVALITETSFEL              
048500*                                 BLOCKED DATE QUALITY ERROR              
048600        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
048700*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
048800*                                 AREA ADDRESS CROSS DOCKING WARE         
048900*                                 HOUSE                                   
049000        05 FLCDREL           PIC X.                                       
049100*                                 OMGÅENDE RELEASE AV CD-REFILL           
049200*                                 IMMEDIATE RELEASE OF CD-REFILL          
049300        05 KVDAGAR-CDBEH     PIC S9(3)           COMP-3.                  
049400*                                 NO. OF DAYS TO BE USED WHEN             
049500*                                 CALCULATING CD REFILLORDERS             
049600        05 KVOT-NDC-INNEV    PIC S9(7)           COMP-3.                  
049700*                                 ORDERTRÄFFAR LEV FRÅN NDC               
049800*                                 ORDERHITS ON NDC                        
049900        05 KVOT-NDC-RULL     PIC S9(7)           COMP-3.                  
050000*                                 ORDERTRÄFFAR LEV FRÅN NDC               
050100*                                 ORDERHITS ON NDC                        
050200        05 KVOT-MISS-INNEV   PIC S9(7)           COMP-3.                  
050300*                                 ORDERTRÄFFAR LEV FRÅN CDC               
050400*                                 ORDERHITS ON SDC FORWARDED              
050500*                                 TO CDC                                  
050600        05 KVOT-MISS-RULL    PIC S9(7)           COMP-3.                  
050700*                                 ORDERTRÄFFAR LEV FRÅN CDC               
050800*                                 ORDERHITS ON SDC FORWARDED              
050900*                                 TO CDC                                  
051000        05 TIINLINL          PIC S9(7)           COMP-3.                  
051100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
051200*                                 DATE OF REPORTED IN STOCK (R32)         
051300     03 CN-INFO.                                                          
051400*                                 INFO SOM GÄLLER ENBART KINA-NDC         
051500*                                                                         
051600*                                                                         
051700*                                 INFO VALID ONLY FOR NDC CHINA           
051800        05 PRMATRL           PIC S9(7)V9(2)      COMP-3.                  
051900*                                 FAST PRIS UNDER LÖPANDE ÅR              
052000*                                 MATERIAL PRICE FOR ACTUAL YEAR          
052100        05 KDMATRPR          PIC X.                                       
052200*                                 KOD OM MAT.PRIS TIPPAT/KÖP              
052300*                                 CODE MAT.PRICE PROGNOSE/PURCH.          
052400        05 KVPALL-CN         PIC S9(7)           COMP-3.                  
052500*                                 ANTAL I PALL                            
052600*                                 QUANTITY IN PALLET                      
052700        05 KVULOAD           PIC S9(7)           COMP-3.                  
052800*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
052900*                                 MIN LOAD FROM SUPPLIER                  
053000        05 KVSLAGER          PIC S9(7)           COMP-3.                  
053100*                                 SÄKERHETSLAGER                          
053200*                                 SAFETY STOCK                            
053300        05 TIMANSEC          PIC S9(7)           COMP-3.                  
053400*                                 DATUM MANUELL SÄK-LAGER(ÅÅMMDD)         
053500*                                 DATE MAN. SAFETY STOCK (YYMMDD)         
053600        05 KVEOQ             PIC S9(7)           COMP-3.                  
053700*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
053800*                                 ET                                      
053900        05 KDOPPLAN          PIC X.                                       
054000*                                 OPTIMAL PLAN INOM FRYSTID               
054100*                                 OPTIMAL PLAN WITHIN FREEZTIME           
054200        05 KDLEVPLF          PIC X.                                       
054300*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
054400*                                 CODE FOR APPROVAL OF SCHEDULE P         
054500*                                 ROPOSAL                                 
054600        05 FLJIT-CN          PIC X.                                       
054700*                                 JUST-IN-TIME FLAGGA                     
054800*                                 JUST-IN-TIME FLAG                       
054900        05 KDLPSP            PIC S9              COMP-3.                  
055000*                                 LEVERANSPLANESPÄRR                      
055100        05 TILPSP            PIC S9(5)           COMP-3.                  
055200*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
055300        05 KVPB-PLAN         PIC S9(6)V9(1)      COMP-3.                  
055400*                                 PLANERAT PERIODBEHOV                    
055500*                                 PLANNED PERIOD REQUIREMENTS             
055600        05 DAPBPLAN          PIC 9(8).                                    
055700*                                 DATUM KVPB-PLAN GILTIG TOM              
055800*                                 DATE KVPB-PLAN VALID UNTIL              
055900        05 RESEASON-PLAN     OCCURS 12 TIMES                              
056000                             PIC S9V9(2)         COMP-3.                  
056100*                                 SÄSONGSINDEX INKLUSIVE REFILL           
056200        05 DASEASON          PIC 9(8).                                    
056300*                                 DATUM RESEASON-LEDTID GILTIG TO         
056400*                                 M                                       
056500*                                 DATE RESEASON-LEDTID VALID UNTI         
056600*                                 L                                       
056700        05 KVPB-JUST1        PIC S9(6)V9(1)      COMP-3.                  
056800*                                 PERIODBEHOVSJUSTERING-1                 
056900*                                 PERIOD REQUIREMENTS-1                   
057000        05 TIPBJUST-1        PIC S9(5)           COMP-3.                  
057100*                                 DATUM FÖRSTA PB-JUSTERING ÅÅVV          
057200*                                 FIRST PB-JUST DATE YYWW                 
057300        05 KVPB-JUST2        PIC S9(6)V9(1)      COMP-3.                  
057400*                                 PERIODBEHOVSJUSTERING-2                 
057500*                                 PERIOD REQUIREMENTS-2                   
057600        05 TIPBJUST-2        PIC S9(5)           COMP-3.                  
057700*                                 DATUM ANDRA PB-JUSTERING ÅÅVV           
057800*                                 SECOND PB-JUST DATE YYWW                
057900        05 KVPB-TREND        PIC S9(6)V9(1)      COMP-3.                  
058000*                                 PERIODTRENDVÄRDE                        
058100        05 KVVECKOR-TREND    PIC S9(3)           COMP-3.                  
058200*                                 ANTAL VECKOR TRENDVÄRDE                 
058300        05 TIDATUM-TREND     PIC S9(7)           COMP-3.                  
058400*                                 JUSTERAD TREND AAMMDD                   
058500*                                 LAST TREND CHANGE  YYMMDD               
058600        05 KVPBREOI          PIC S9(6)V9(1)      COMP-3.                  
058700*                                 PERIODBEHOV FÖR REFILL OI               
058800*                                 PERIOD REQUIREM. REFILLING OI           
058900        05 TIPBREOI          PIC 9(6).                                    
059000*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
059100*                                 DATE MAN. FC REFILL OI (YYMMDD)         
059200        05 KVOI-REF-RP       OCCURS 12 TIMES                              
059300                             PIC S9(7)           COMP-3.                  
059400*                                 ORDERINGÅNG TILL DC REFILL              
059500*                                 ORDERED PCS PER TIME UNIT               
059600*                                 ORDERED FROM DC                         
059700*                                 WEIGHTENED BY 4.33                      
059800        05 KVOT-REF-RULL     PIC S9(7)           COMP-3.                  
059900*                                 ORDERTRÄFF, REF-RULL                    
060000*                                 ORDER HITS FROM REF-RULL ORDERS         
060100        05 KDAVT             PIC S9              COMP-3.                  
060200*                                 AVTALSMÄRKNING                          
060300*                                 AGREEMENT CODE                          
060400        05 TIREFSTO-LOC      PIC S9(7)           COMP-3.                  
060500*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
060600*                                 STOPPED REF.UNTIL DATE(NDC>LDC)         
060700        05 IDANSK-CN         PIC S9(3)           COMP-3.                  
060800*                                 ANSKAFFARNUMMER                         
060900*                                 PROCURER NO.                            
061000        05 IDINK-CN          PIC X(4).                                    
061100*                                 INKÖPARNUMMER                           
061200*                                 PURCHASE IDENTIFICATION NUMBER          
061300        05 IDLEVNR-SHIP      PIC X(5).                                    
061400*                                 SKEPPANDE LEVERANTÖR                    
061500*                                 SHIPPING SUPPLIER                       
061600        05 IDLEVNR-FRAM      PIC X(5).                                    
061700*                                 FRAMTIDA LEVERANTÖRNUMMER               
061800*                                 THE SUPPLIER NAME IN THE FUTURE         
061900        05 IDLEVNR-SHIP-FRAM PIC X(5).                                    
062000*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
062100*                                 THE SHIP.SUPPLIER IN THE FUTURE         
062200        05 TILEVDAT          PIC S9(7)           COMP-3.                  
062300*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
062400*                                 DATE FOR FUTURE SUPPLIER                
062500        05 IDPLANGR-AG       PIC S9              COMP-3.                  
062600*                                 PLANERINGSGRUPP ANSKAFFARE              
062700        05 TILEVDAG          OCCURS 5 TIMES                               
062800                             PIC S9              COMP-3.                  
062900*                                 AVSÄNDNINGSDAG INOM VECKA               
063000*                                 DELIVERY WEEK DAY                       
063100        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
063200*                                 INLEVERANSTID     (ANTAL DAGAR)         
063300        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
063400*                                 ANTAL VECKOR LEDTID                     
063500        05 TIMANLED          PIC S9(7)           COMP-3.                  
063600*                                 SLUTDATUM MAN. LEDTID (ÅÅMMDD)          
063700*                                 END DATE MAN.LEAD TIME (YYMMDD)         
063800        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
063900*                                 SLUTKÖPSSALDO                           
064000        05 TISLUTKP          PIC S9(7)           COMP-3.                  
064100*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
064200*                                 CALC OF ATR-BAL IS TO COMMENCE          
064300        05 TIERSDAT-VIPS     PIC S9(5)           COMP-3.                  
064400*                                 DATUM NÄR ERS. INFO TILL VIPS           
064500*                                 SEND DATE OF SUPERS. TO VIPS            
064600        05 KVSPANT           PIC S9(7)           COMP-3.                  
064700*                                 SPÄRRAT ANTAL                           
064800*                                 BLOCKED QTY                             
064900        05 VKART-CN          PIC S9(7)           COMP-3.                  
065000*                                 ARTIKELVIKT (G)                         
065100*                                 PART WEIGHT (G)                         
065200        05 VLARTNTO-CN       PIC S9(8)V9(1)      COMP-3.                  
065300*                                 ARTIKELVOLYM NETTO (CM3)                
065400*                                 PART NET VOLUME    (CM3)                
065500        05 KDARTURS-CN       PIC X(2).                                    
065600*                                 ARTIKELURSPRUNGSKOD                     
065700*                                 COUNTRY OF ORIGIN                       
065800*** END OF VILMAII-COPY LENGTH= 875 BYTES                                 
