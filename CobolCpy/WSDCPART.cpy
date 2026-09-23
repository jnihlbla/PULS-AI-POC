000100 01  WSDCPART.                                                            
000200*                                 PRIMÄREXTRAKT                           
000300*                                                                         
000400*                                 LAGERBAND                               
000500*                                 COPY-TEXT FÖR EXTRAKT WSDCDAY           
000600*                                                       WSDCWEEK          
000700*                                                       WSDCACC           
000800*                                                       WSDCPLAN          
000900*                                                       WSDCYEAR          
001000*                                 FRAMSTÄLLS VARJE :                      
001100*                                            MORGON (DAGLIGEN)            
001200*                                            VECKOSLUT                    
001300*                                            REDOVISNINGSPERIOD           
001400*                                            PLANERINGSPERIOD             
001500*                                            ÅRS-SLUT                     
001600*                                                                         
001700*                                 DÄR IDDC  = 21 SDC BORN                 
001800*                                             22 SDC LES MUREAUX          
001900*                                             23 SDC ENGLAND              
002000*                                             24 SDC SPANIEN              
002100*                                             25 SDC ITALIEN              
002200*                                                                         
002300*                                                                         
002400*                                                                         
002500*                                 INNEHÅLLER SDC INFORMATION              
002600*                                 1 POST/ARTIKELNUMMER/IDDC               
002700*                                                                         
002800*                                                                         
002900*                                 PRIMARY EXTRACT                         
003000*                                                                         
003100*                                 PART-INFORMATION-FILE FOR SDC           
003200*                                 COPY-TEXT FOR EXTRACT WSDCDAY           
003300*                                                       WSDCWEEK          
003400*                                                       WSDCACC           
003500*                                                       WSDCPLAN          
003600*                                                       WSDCYEAR          
003700*                                                                         
003800*                                 CREATED EVERY :                         
003900*                                               MORNING (DAILY)           
004000*                                               WEEKEND                   
004100*                                               ACCOUNTPERIOD             
004200*                                               PLANNINGPERIOD            
004300*                                               YEAR                      
004400*                                                                         
004500*                                 WHERE IDDC=21 SDC BORN                  
004600*                                            22 SDC LES MUREAUX           
004700*                                            23 SDC GREAT BRITAIN         
004800*                                            24 SDC SPAIN                 
004900*                                            25 SDC ITALY                 
005000*                                                                         
005100*                                                                         
005200*                                 CONTAINS SDC INFORMATION.               
005300*                                 1 RECORD/PARTNUMBER/IDDC                
005400*                                                                         
005500*                                                                         
005600     03 IDARTNR              PIC S9(9)           COMP-3.                  
005700*                                 ARTIKELNUMMER                           
005800*                                 PART NUMBER                             
005900     03 REKSIFFR             PIC S9              COMP-3.                  
006000*                                 KONTROLLSIFFRA                          
006100*                                 PART NO CHECK DIGIT                     
006200     03 IDDC                 PIC X(2).                                    
006300*                                 IDENTIFIERARE LAGER                     
006400*                                 WAREHOUSE IDENTIFIER                    
006500     03 ARTIKEL-INFO.                                                     
006600*                                 ARTIKELUPPGIFTER                        
006700*                                                                         
006800*                                 PART INFORMATION                        
006900        05 BEART-SVE         PIC X(25).                                   
007000*                                 SVENSK ARTIKELBENÄMNING                 
007100        05 BEART-ENG         PIC X(25).                                   
007200*                                 ENGELSK ARTIKELBENÄMNING                
007300        05 BEFT              PIC S9(3)           COMP-3.                  
007400*                                 FÖRPACKNINGSTYP                         
007500*                                 PACKAGING TYPE                          
007600        05 FLAVRART          PIC X.                                       
007700*                                 AVROPSARTIKEL                           
007800        05 FLERS             PIC X.                                       
007900*                                 TILLKOMMANDE ARTIKEL ?                  
008000        05 FLGEMART          PIC X.                                       
008100*                                 FLAGGA GEMENSAM ARTIKEL                 
008200*                                 COMMON PART FLAG                        
008300        05 FLIART            PIC X.                                       
008400*                                 ARTIKELN INGÅR I SATS                   
008500*                                 PART IN KIT                             
008600        05 FLJIT             PIC X.                                       
008700*                                 JUST-IN-TIME FLAGGA                     
008800*                                 JUST-IN-TIME FLAG                       
008900        05 FLLSRDEL          PIC X.                                       
009000*                                 LEVERERAS SOM RESDEL                    
009100        05 FLTPO1            PIC X.                                       
009200*                                 ARTIKELN GODKÄND FÖR TPO1               
009300*                                 TPO1 ALLOWED FOR ARTICLE                
009400        05 IDANSK            PIC S9(3)           COMP-3.                  
009500*                                 ANSKAFFARNUMMER                         
009600*                                 PROCURER NO.                            
009700        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
009800*                                 EMBALLAGEARTIKELNR FÖR Q0               
009900        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
010000*                                 EMBALLAGEARTIKELNR FÖR Q1               
010100        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
010200*                                 EMBALLAGEARTIKELNR FÖR Q2               
010300        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
010400*                                 EMBALLAGEARTIKELNR FÖR Q3               
010500        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
010600*                                 EMBALLAGEARTIKELNR FÖR Q4               
010700        05 IDBERED           PIC S9(3)           COMP-3.                  
010800*                                 BEREDARENUMMER                          
010900        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
011000*                                 FUNKTIONSGRUPP                          
011100*                                 FUNCTION GROUP                          
011200        05 FILLERX2          PIC X(2).                                    
011300        05 IDLEVNR           PIC X(5).                                    
011400*                                 LEVERANTÖRNUMMER                        
011500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011600        05 IDLKTO            PIC S9(7)           COMP-3.                  
011700*                                 LAGERKONTO (FFHHHUU)                    
011800*                                 STOCK ACCOUNT (CCMMMSS)                 
011900        05 IDPROJ            PIC X(4).                                    
012000*                                 PARTS PROJEKTIDENTITET                  
012100*                                 PARTS PROJECT IDENTITY                  
012200        05 IDPSN             PIC 9(3).                                    
012300*                                 PROPER SHIPPING NAME                    
012400*                                 PROPER SHIPPING NAME                    
012500        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
012600*                                 PERSONKOD REFILLANSVARIG                
012700*                                 REFILL RESPONSIBLE ID                   
012800        05 IDPSN-DC          PIC 9(3).                                    
012900*                                 PROPER SHIPPING NAME PER XDC            
013000*                                 PROPER SHIPPING NAME XDC                
013100        05 FILLER            PIC X(3).                                    
013200        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
013300*                                 STATISTISKT NUMMER                      
013400*                                 1 = NORSKT                              
013500*                                 2 = ENGELSKT                            
013600*                                 3 = BELGISKT                            
013700*                                 4 = PERUANSKT                           
013800*                                 5 = SVENSKT                             
013900*                                 6 =                                     
014000*                                 STATISTICAL NO.                         
014100        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
014200*                                 STATISTISKT NUMMER                      
014300*                                 1 = NORSKT                              
014400*                                 2 = ENGELSKT                            
014500*                                 3 = BELGISKT                            
014600*                                 4 = PERUANSKT                           
014700*                                 5 = SVENSKT                             
014800*                                 6 =                                     
014900*                                 STATISTICAL NO.                         
015000        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
015100*                                 STATISTISKT NUMMER                      
015200*                                 1 = NORSKT                              
015300*                                 2 = ENGELSKT                            
015400*                                 3 = BELGISKT                            
015500*                                 4 = PERUANSKT                           
015600*                                 5 = SVENSKT                             
015700*                                 6 =                                     
015800*                                 STATISTICAL NO.                         
015900        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
016000*                                 STATISTISKT NUMMER                      
016100*                                 1 = NORSKT                              
016200*                                 2 = ENGELSKT                            
016300*                                 3 = BELGISKT                            
016400*                                 4 = PERUANSKT                           
016500*                                 5 = SVENSKT                             
016600*                                 6 =                                     
016700*                                 STATISTICAL NO.                         
016800        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
016900*                                 STATISTISKT NUMMER                      
017000*                                 1 = NORSKT                              
017100*                                 2 = ENGELSKT                            
017200*                                 3 = BELGISKT                            
017300*                                 4 = PERUANSKT                           
017400*                                 5 = SVENSKT                             
017500*                                 6 =                                     
017600*                                 STATISTICAL NO.                         
017700        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
017800*                                 STATISTISKT NUMMER                      
017900*                                 1 = NORSKT                              
018000*                                 2 = ENGELSKT                            
018100*                                 3 = BELGISKT                            
018200*                                 4 = PERUANSKT                           
018300*                                 5 = SVENSKT                             
018400*                                 6 =                                     
018500*                                 STATISTICAL NO.                         
018600        05 KDAGE             PIC X.                                       
018700*                                 AGE-CODE                                
018800*                                 AGE-CODE                                
018900        05 KDARTHNT          PIC S9(7)           COMP-3.                  
019000*                                 HANTERINGSKOD                           
019100*                                 HANDLING CODE                           
019200        05 KDARTURS          PIC X(2).                                    
019300*                                 ARTIKELURSPRUNGSKOD                     
019400*                                 COUNTRY OF ORIGIN                       
019500        05 KDBPSR            PIC S9              COMP-3.                  
019600*                                 BASLAGERFÖRSLAGSNIVÅ                    
019700*                                 BASIC PART STOCK RECOMMENDATION         
019800        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019900*                                 EMBALLAGEKOD 0                          
020000        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
020100*                                 EMBALLAGEKOD 1                          
020200        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
020300*                                 EMBALLAGEKOD 2                          
020400        05 KDERS             PIC S9(3)           COMP-3.                  
020500*                                 ERSÄTTNINGSKOD                          
020600*                                 SUPERSESSION CODE                       
020700        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
020800*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020900*                                 OBSOLETION SUPERSESSION CODE            
021000        05 KDFARLIG          PIC S9              COMP-3.                  
021100*                                 KOD FÖR FARLIGT GODS                    
021200*                                 DANGEROUS GOODS CODE                    
021300        05 KDGK              PIC S9              COMP-3.                  
021400*                                 GODSMOTTAGAREKOD                        
021500*                                 GOODS RECEIVING WAREHOUSE CODE          
021600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
021700*                                 PRODUKTSLAG                             
021800*                                 PRODUCT GROUP                           
021900        05 KDSORT            PIC X(2).                                    
022000*                                 SORT-KOD                                
022100*                                 UNIT OF MEASURE                         
022200        05 KDSPEEMB          PIC 9.                                       
022300*                                 SPECIALEMBALLAGEKOD                     
022400*                                 SPECIAL PACKING CODE                    
022500        05 KDSRA             PIC S9(3)           COMP-3.                  
022600*                                 SRA-KOD                                 
022700*                                 SRA CODE                                
022800        05 KDUART            PIC X.                                       
022900*                                 UNDANTAGSARTIKEL                        
023000*                                 EXECPTION PARTS                         
023100        05 KDVVKL            PIC S9              COMP-3.                  
023200*                                 VOLYMVÄRDESKLASS                        
023300*                                 VOLUME VALUE CLASS                      
023400        05 KDYTBEH           PIC S9(3)           COMP-3.                  
023500*                                 YTBEHANDLINGSKOD                        
023600*                                                                         
023700        05 KVPALL            PIC S9(7)           COMP-3.                  
023800*                                 ANTAL I PALL                            
023900*                                 QUANTITY IN PALLET                      
024000        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
024100*                                 ANTAL I Q0 FÖRPACKNING                  
024200        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
024300*                                 ANTAL I Q1 FÖRPACKNING                  
024400*                                 QUANTITY IN BULK PACK Q1                
024500        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
024600*                                 ANTAL I Q2 FÖRPACKNING                  
024700*                                 QUANTITY IN BULK PACK Q2                
024800        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024900*                                 ANTAL I Q3 FÖRPACKNING                  
025000*                                 QUANTITY IN BULK PACK Q3                
025100        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
025200*                                 ANTAL I Q4 FÖRPACKNING                  
025300*                                 QUANTITY IN BULK PACK Q4                
025400        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
025500*                                 BESTÄLLNINGSPRIS I KRONOR               
025600*                                 ORDER PRICE SWEDISH CURRENCY            
025700        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
025800*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025900*                                 GROSS-PRICE EXPORT                      
026000*                                  (FOB-GROSS)                            
026100        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
026200*                                 ARTIKELNS SJÄLVKOSTNAD                  
026300*                                 COST OF SALES                           
026400        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
026500*                                 ARTIKELSTANDARDPRIS                     
026600*                                 STANDARD PRICE                          
026700        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
026800*                                 INKÖPSPRIS                              
026900*                                 PURCHASE PRICE                          
027000        05 TIERSDAT          PIC S9(5)           COMP-3.                  
027100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
027200*                                 DATE OF SUPERSESSION (YYWWD)            
027300        05 TIFINLV           PIC S9(5)           COMP-3.                  
027400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
027500*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
027600        05 VKART             PIC S9(7)           COMP-3.                  
027700*                                 ARTIKELVIKT (G)                         
027800*                                 PART WEIGHT (G)                         
027900        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
028000*                                 ARTIKELVOLYM (CM3)                      
028100*                                 PART VOLUME    (CM3)                    
028200        05 IDKAT-1           PIC X(5).                                    
028300*                                 KATALOGBETECKNING                       
028400        05 IDKAT-2           PIC X(5).                                    
028500*                                 KATALOGBETECKNING                       
028600        05 IDKAT-3           PIC X(5).                                    
028700*                                 KATALOGBETECKNING                       
028800        05 IDINK             PIC X(4).                                    
028900*                                 INKÖPARNUMMER                           
029000*                                 PURCHASE IDENTIFICATION NUMBER          
029100        05 FILLER            PIC X(74).                                   
029200     03 SDC-INFO.                                                         
029300*                                 INFO SOM GÄLLER ENBART SDC              
029400*                                                                         
029500*                                                                         
029600*                                 INFO VALID ONLY FOR SDC                 
029700        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029800*                                 LAGEROMRÅDE                             
029900*                                 AREA                                    
030000        05 ADGANG            PIC S9(3)           COMP-3.                  
030100*                                 GÅNG                                    
030200*                                 AISLE                                   
030300        05 ADPLATS           PIC S9(5)           COMP-3.                  
030400*                                 LAGERPLATSNUMMER                        
030500*                                 LOCATION                                
030600        05 FLREFBEO          PIC X.                                       
030700*                                 AUTOMATISK REFILL BEORDRING?            
030800*                                 AUTOMATIC REFILL ORDERING?              
030900        05 FLREFILL          PIC X.                                       
031000*                                 REFILLARTIKEL                           
031100*                                 REFILLPART                              
031200        05 FLREFNYO          PIC X.                                       
031300*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031400*                                 WAIT FOR NEXT DEMAND                    
031500        05 FLSEASON          PIC X.                                       
031600*                                 SÄSONG PÅ ARTIKEL                       
031700*                                 SEASON MARK PER PART                    
031800        05 FLSKROT-BEORD     PIC X.                                       
031900*                                 SKROTNING BEORDRAD AV ANSK              
032000*                                 SCRAPPING ORDERED BY PROCURER           
032100        05 FLWILSON          PIC X.                                       
032200*                                 WILSONFORMEL                            
032300*                                 FLAG TO USE WILSON OR NOT               
032400        05 KDFREKKL          PIC X.                                       
032500*                                 FREKVENSKLASS                           
032600*                                 FREQ. CLASS                             
032700        05 KDLEVSP           PIC S9(3)           COMP-3.                  
032800*                                 SPÄRRKOD LEVERANS                       
032900*                                 DELIVERY BLOCKING CODE                  
033000        05 KDPRISKL          PIC X.                                       
033100*                                 PRISKLASS                               
033200*                                 PRICE CLASS                             
033300        05 KDREFSTA          PIC X.                                       
033400*                                 STATUS REFILLARTIKEL                    
033500*                                 STATUS REFILLPART                       
033600        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
033700*                                 DEL AV AK PÅ VÄG                        
033800*                                 PART OF AK ON ITS WAY                   
033900        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
034000*                                 DEL AV AK SOM LIGGER I SDC              
034100*                                 PART OF AK IN THE SDC                   
034200        05 KVBEART           PIC S9(7)           COMP-3.                  
034300*                                 BESTÄLLT ANTAL STYCKEN                  
034400*                                 ORDERED QUANTITY                        
034500        05 KVEFRS            PIC S9(7)           COMP-3.                  
034600*                                 EJ FAKTURERAT ANTAL STYCK               
034700*                                 ORDERED NOT INVOICED QTY                
034800        05 KVINVS            PIC S9(7)           COMP-3.                  
034900*                                 INVENTERINGSSALDO                       
035000*                                 STOCK-TAKING BALANCE                    
035100        05 KVLS              PIC S9(7)           COMP-3.                  
035200*                                 LAGERSALDO                              
035300*                                 STOCK BALANCE                           
035400        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
035500*                                 ORDERINGÅNG LEV FRÅN CDC                
035600*                                 ORDERED PCS PER TIME UNIT               
035700*                                 FORWARDED TO CDC                        
035800        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
035900*                                 ORDERINGÅNG TILL SDC                    
036000*                                 ORDERED PCS PER TIME UNIT               
036100*                                 ORDERED FROM SDC                        
036200        05 KVOI-RP           OCCURS 12 TIMES                              
036300                             PIC S9(7)           COMP-3.                  
036400*                                 ORDERINGÅNG TILL DC                     
036500*                                 ORDERED PCS PER TIME UNIT               
036600*                                 ORDERED FROM DC                         
036700*                                 WEIGHTENED BY 4.33                      
036800        05 KVOI-FOREG        OCCURS 2 TIMES                               
036900                             PIC S9(7)           COMP-3.                  
037000*                                 ORDERINGÅNG TILL DC FÖREG ÅR            
037100*                                 ORDERED PCS LAST YEAR                   
037200*                                 ORDERED FROM DC                         
037300        05 KVOI-INNEV        PIC S9(7)           COMP-3.                  
037400*                                 ORDERINGÅNG TILL DC INNEV PER           
037500*                                 ORDERED PCS THIS PERIOD                 
037600*                                 ORDERED FROM DC                         
037700        05 KVOI-IAAR         PIC S9(7)           COMP-3.                  
037800*                                 ORDERINGÅNG TILL DC INNEV ÅR            
037900*                                 ORDERED PCS PER TIME UNIT               
038000*                                 ORDERED FROM DC                         
038100        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
038200*                                 ORDERKÖSALDO, KLASS 1                   
038300*                                 ORDER QUEUE BALANCE, CLASS 1            
038400        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
038500*                                 ORDERTRÄFFAR LEV FRÅN CDC               
038600*                                 ORDERHITS ON SDC FORWARDED              
038700*                                 TO CDC                                  
038800        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
038900*                                 ORDERTRÄFFAR PÅ SDC                     
039000*                                 ORDERHITS ON SDC                        
039100        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
039200*                                 PERIODBEHOV REFILLING                   
039300*                                 FORECAST REFILLING                      
039400        05 KVREFBER          PIC S9(7)           COMP-3.                  
039500*                                 BERÄKNAD REFILLINGKVANTITET             
039600*                                 CALCULATED REFILLING QUANTITY           
039700        05 KVREFOVL          PIC S9(7)           COMP-3.                  
039800*                                 BERÄKNAD ÖVERLAGERPUNKT                 
039900*                                 CALCULATED OVERSTOCK POINT              
040000        05 KVREFPKT          PIC S9(7)           COMP-3.                  
040100*                                 BERÄKNAD PÅFYLLNADSPUNKT                
040200*                                 CALCULATED REFILLING POINT              
040300        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
040400*                                 ANTAL SENASTE RETURORDER                
040500*                                 QUANTITY LAST RETURNORDER               
040600        05 KVSKROT           PIC S9(7)           COMP-3.                  
040700*                                 ANTAL SENASTE SKROTORDER                
040800*                                 QUANTITY LAST SCRAPORDER                
040900        05 KVUTRS            PIC S9(7)           COMP-3.                  
041000*                                 UTREDNINGSSALDO                         
041100*                                 INVESTIGATION BALANCE                   
041200        05 REOSAEK           PIC S9(3)V9(1)      COMP-3.                  
041300*                                 SEASONAL UNCERTAINTY FACTOR             
041400        05 RESEASON          OCCURS 12 TIMES                              
041500                             PIC S9V9(2)         COMP-3.                  
041600*                                 SÄSONGSINDEX                            
041700        05 TIINVDAT          PIC S9(5)           COMP-3.                  
041800*                                 INVENTERINGSDATUM                       
041900*                                 STOCKTAKING DATE                        
042000        05 TIORDREG          PIC S9(7)           COMP-3.                  
042100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
042200*                                 ORDER REGISTRATION DATE  YYMMDD         
042300        05 TIREFEFT          PIC S9(7)           COMP-3.                  
042400*                                 DATUM SENAST EFTERFRÅGAD                
042500*                                 DATE LATEST DEMAND                      
042600        05 TIREFMPB          PIC S9(7)           COMP-3.                  
042700*                                 DATUM MANUELL PROGNOS REFILLING         
042800*                                 DATE MANUAL FORECAST REFILLING          
042900        05 TIREFPAF          PIC S9(7)           COMP-3.                  
043000*                                 DATUM MANUELL PÅFYLLNADSKVANT           
043100*                                 DATE MANUAL REFILLING QTY               
043200        05 TIREFPKT          PIC S9(7)           COMP-3.                  
043300*                                 DATUM MANUELL REFILLPUNKT               
043400*                                 DATE MANUAL REFILLING POINT             
043500        05 TIREFSTA          PIC S9(7)           COMP-3.                  
043600*                                 DATUM AKT/PASS REFILLARTIKEL            
043700*                                 DATE ACT/PASS REFILLPART                
043800        05 TIREFSTO          PIC S9(7)           COMP-3.                  
043900*                                 BEORDRINGSSTOPPAD T.OM.                 
044000*                                 STOPPED FOR ORDERING UNTIL              
044100        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
044200*                                 DATUM RETUR BEORDRING                   
044300*                                 DATE ISSUE OF RETURN ORDER              
044400        05 TISKROT           PIC S9(7)           COMP-3.                  
044500*                                 SKROTNINGSDATUM                         
044600*                                 DATE OF SCRAPPING                       
044700        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
044800*                                 BEORDRAD SKROTNINGSDATUM                
044900*                                 DATE OF SCRAPPING DECISION              
045000        05 TISPARR-KVAL      PIC 9(6).                                    
045100*                                 SPÄRRAD DATUM KVALITETSFEL              
045200*                                 BLOCKED DATE QUALITY ERROR              
045300        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
045400*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
045500*                                 AREA ADDRESS CROSS DOCKING WARE         
045600*                                 HOUSE                                   
045700        05 FLCDREL           PIC X.                                       
045800*                                 OMGÅENDE RELEASE AV CD-REFILL           
045900*                                 IMMEDIATE RELEASE OF CD-REFILL          
046000        05 KVDAGAR-CDBEH     PIC S9(3)           COMP-3.                  
046100*                                 NO. OF DAYS TO BE USED WHEN             
046200*                                 CALCULATING CD REFILLORDERS             
046300        05 KVOT-SDC-INNEV    PIC S9(7)           COMP-3.                  
046400*                                 ORDERTRÄFFAR LEV FRÅN SDC               
046500*                                 ORDERHITS ON SDC                        
046600        05 KVOT-SDC-RULL     PIC S9(7)           COMP-3.                  
046700*                                 ORDERTRÄFFAR LEV FRÅN SDC               
046800*                                 ORDERHITS ON SDC                        
046900        05 KVOT-MISS-INNEV   PIC S9(7)           COMP-3.                  
047000*                                 ORDERTRÄFFAR LEV FRÅN CDC               
047100*                                 ORDERHITS ON SDC FORWARDED              
047200*                                 TO CDC                                  
047300        05 KVOT-MISS-RULL    PIC S9(7)           COMP-3.                  
047400*                                 ORDERTRÄFFAR LEV FRÅN CDC               
047500*                                 ORDERHITS ON SDC FORWARDED              
047600*                                 TO CDC                                  
047700        05 TIINLINL          PIC S9(7)           COMP-3.                  
047800*                                 RAPPORTERINGSDATUM INLAGD (R32)         
047900*                                 DATE OF REPORTED IN STOCK (R32)         
048000        05 IDREFTAB          PIC X.                                       
048100*                                 IDENTITET REFILLTABELL                  
048200*                                 REFILLINGTABLE IDENTIFIER               
048300        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
048400*                                 ORDERKÖSALDO, KLASS 2-4                 
048500*                                 ORDER QUEUE BALANCE, CLASS 2-4          
048600        05 FLBUYUPD          PIC X.                                       
048700*                                 OM IDPERSONKOD ÄR LÅST                  
048800*                                 IF BUYER UPDATE IS LOCKED               
048900        05 FLTABUPD          PIC X.                                       
049000*                                 OM REFILLTABELL ÄR LÅST                 
049100*                                 IF REFILLINGTABLE UPDATE LOCKED         
049200        05 FILLER            PIC X.                                       
049300*** END OF VILMAII-COPY LENGTH= 601 BYTES                                 
