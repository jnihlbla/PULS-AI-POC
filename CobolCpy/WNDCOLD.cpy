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
010800        05 IDLEVNR           PIC S9(5)           COMP-3.                  
010900*                                 LEVERANTÖRNUMMER                        
011000*                                 SUPPLIER NUMBER (VENDORNUMBER)          
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
027900        05 FILLER            PIC X(93).                                   
028000     03 NDC-INFO.                                                         
028100*                                 INFO SOM GÄLLER ENBART NDC              
028200*                                                                         
028300*                                                                         
028400*                                 INFO VALID ONLY FOR NDC                 
028500        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028600*                                 LAGEROMRÅDE                             
028700*                                 AREA                                    
028800        05 ADGANG            PIC S9(3)           COMP-3.                  
028900*                                 GÅNG                                    
029000*                                 AISLE                                   
029100        05 ADPLATS           PIC S9(5)           COMP-3.                  
029200*                                 LAGERPLATSNUMMER                        
029300*                                 LOCATION                                
029400        05 FLREFBEO          PIC X.                                       
029500*                                 AUTOMATISK REFILL BEORDRING?            
029600*                                 AUTOMATIC REFILL ORDERING?              
029700        05 FLREFILL          PIC X.                                       
029800*                                 REFILLARTIKEL                           
029900*                                 REFILLPART                              
030000        05 FLREFNYO          PIC X.                                       
030100*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
030200*                                 WAIT FOR NEXT DEMAND                    
030300        05 FLSKROT-BEORD     PIC X.                                       
030400*                                 SKROTNING BEORDRAD AV ANSK              
030500*                                 SCRAPPING ORDERED BY PROCURER           
030600        05 FLWILSON          PIC X.                                       
030700*                                 WILSONFORMEL                            
030800*                                 FLAG TO USE WILSON OR NOT               
030900        05 FLORDSP           PIC X.                                       
031000*                                 ORDERSPÄRR                              
031100*                                 ORDER BLOCKED                           
031200        05 FLSPBULK          PIC X.                                       
031300*                                 FLAGGA SPÄRR MOT BULKORDER              
031400*                                 FLAG BULKORDER STOP                     
031500        05 IDLEVNR-NDC       PIC S9(5)           COMP-3.                  
031600*                                 LEVERANTÖRNUMMER                        
031700*                                 SUPPLIER NUMBER (VENDORNUMBER)          
031800        05 KDFREKKL          PIC X.                                       
031900*                                 FREKVENSKLASS                           
032000*                                 FREQ. CLASS                             
032100        05 KDLEVSP           PIC S9(3)           COMP-3.                  
032200*                                 SPÄRRKOD LEVERANS                       
032300*                                 DELIVERY BLOCKING CODE                  
032400        05 KDPRISKL          PIC X.                                       
032500*                                 PRISKLASS                               
032600*                                 PRICE CLASS                             
032700        05 KDPSLLOC          PIC 9(2).                                    
032800*                                 PRODUKTSLAG LOKALT                      
032900*                                 PRODUCT GROUP LOCAL                     
033000        05 KDREFSTA          PIC X.                                       
033100*                                 STATUS REFILLARTIKEL                    
033200*                                 STATUS REFILLPART                       
033300        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
033400*                                 DEL AV AK PÅ VÄG                        
033500*                                 PART OF AK ON ITS WAY                   
033600        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
033700*                                 DEL AV AK SOM LIGGER I SDC              
033800*                                 PART OF AK IN THE SDC                   
033900        05 KVBEART           PIC S9(7)           COMP-3.                  
034000*                                 BESTÄLLT ANTAL STYCKEN                  
034100*                                 ORDERED QUANTITY                        
034200        05 KVEFRS            PIC S9(7)           COMP-3.                  
034300*                                 EJ FAKTURERAT ANTAL STYCK               
034400*                                 ORDERED NOT INVOICED QTY                
034500        05 KVINVS            PIC S9(7)           COMP-3.                  
034600*                                 INVENTERINGSSALDO                       
034700*                                 STOCK-TAKING BALANCE                    
034800        05 KVLS              PIC S9(7)           COMP-3.                  
034900*                                 LAGERSALDO                              
035000*                                 STOCK BALANCE                           
035100        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
035200*                                 ORDERINGÅNG LEV FRÅN CDC                
035300*                                 ORDERED PCS PER TIME UNIT               
035400*                                 FORWARDED TO CDC                        
035500        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
035600*                                 ORDERINGÅNG TILL SDC                    
035700*                                 ORDERED PCS PER TIME UNIT               
035800*                                 ORDERED FROM SDC                        
035900        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
036000*                                 ORDERKÖSALDO, KLASS 2-4                 
036100*                                 ORDER QUEUE BALANCE, CLASS 2-4          
036200        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
036300*                                 ORDERKÖSALDO, KLASS 1                   
036400*                                 ORDER QUEUE BALANCE, CLASS 1            
036500        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
036600*                                 ORDERTRÄFFAR LEV FRÅN CDC               
036700*                                 ORDERHITS ON SDC FORWARDED              
036800*                                 TO CDC                                  
036900        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
037000*                                 ORDERTRÄFFAR PÅ SDC                     
037100*                                 ORDERHITS ON SDC                        
037200        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
037300*                                 PERIODBEHOV REFILLING                   
037400*                                 FORECAST REFILLING                      
037500        05 KVPB-REF-US       PIC S9(6)V9(1)      COMP-3.                  
037600*                                 PERIODBEHOV REFILLING USA               
037700*                                 FORECAST REFILLING US                   
037800        05 KVREFBER          PIC S9(7)           COMP-3.                  
037900*                                 BERÄKNAD REFILLINGKVANTITET             
038000*                                 CALCULATED REFILLING QUANTITY           
038100        05 KVREFOVL          PIC S9(7)           COMP-3.                  
038200*                                 BERÄKNAD ÖVERLAGERPUNKT                 
038300*                                 CALCULATED OVERSTOCK POINT              
038400        05 KVREFPKT          PIC S9(7)           COMP-3.                  
038500*                                 BERÄKNAD PÅFYLLNADSPUNKT                
038600*                                 CALCULATED REFILLING POINT              
038700        05 KVRESS            PIC S9(7)           COMP-3.                  
038800*                                 RESERVERAT ANTAL ARTIKLAR               
038900*                                 QUANTITY RESERVED ITEMS                 
039000        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
039100*                                 ANTAL SENASTE RETURORDER                
039200*                                 QUANTITY LAST RETURNORDER               
039300        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
039400*                                 RESTORDERSALDO, KLASS 2-4               
039500*                                 BACK ORDER BALANCE, CLASS 2-4           
039600        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
039700*                                 RESTORDERSALDO, KLASS 1                 
039800*                                 BACK ORDER BALANCE, CLASS 1             
039900        05 KVSKROT           PIC S9(7)           COMP-3.                  
040000*                                 ANTAL SENASTE SKROTORDER                
040100*                                 QUANTITY LAST SCRAPORDER                
040200        05 KVUTRS            PIC S9(7)           COMP-3.                  
040300*                                 UTREDNINGSSALDO                         
040400*                                 INVESTIG.BALANCE                        
040500        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
040600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
040700*                                 AVERAGE COST FOREIGN CURRENCY           
040800        05 RESEASON          OCCURS 12 TIMES                              
040900                             PIC S9V9(2)         COMP-3.                  
041000*                                 SÄSONGSINDEX                            
041100        05 TIINVDAT          PIC S9(5)           COMP-3.                  
041200*                                 INVENTERINGSDATUM                       
041300*                                 STOCKTAKING DATE                        
041400        05 TIORDREG          PIC S9(7)           COMP-3.                  
041500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
041600*                                 ORDER REGISTRATION DATE  YYMMDD         
041700        05 TIREFEFT          PIC S9(7)           COMP-3.                  
041800*                                 DATUM SENAST EFTERFRÅGAD                
041900*                                 DATE LATEST DEMAND                      
042000        05 TIREFMPB          PIC S9(7)           COMP-3.                  
042100*                                 DATUM MANUELL PROGNOS REFILLING         
042200*                                 DATE MANUAL FORECAST REFILLING          
042300        05 TIREFPAF          PIC S9(7)           COMP-3.                  
042400*                                 DATUM MANUELL PÅFYLLNADSKVANT           
042500*                                 DATE MANUAL REFILLING QTY               
042600        05 TIREFPKT          PIC S9(7)           COMP-3.                  
042700*                                 DATUM MANUELL REFILLPUNKT               
042800*                                 DATE MANUAL REFILLING POINT             
042900        05 TIREFSTA          PIC S9(7)           COMP-3.                  
043000*                                 DATUM AKT/PASS REFILLARTIKEL            
043100*                                 DATE ACT/PASS REFILLPART                
043200        05 TIREFSTO          PIC S9(7)           COMP-3.                  
043300*                                 BEORDRINGSSTOPPAD T.OM.                 
043400*                                 STOPPED FOR ORDERING UNTIL              
043500        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
043600*                                 DATUM RETUR BEORDRING                   
043700*                                 DATE ISSUE OF RETURN ORDER              
043800        05 TISKROT           PIC S9(7)           COMP-3.                  
043900*                                 SKROTNINGSDATUM                         
044000*                                 DATE OF SCRAPPING                       
044100        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
044200*                                 BEORDRAD SKROTNINGSDATUM                
044300*                                 DATE OF SCRAPPING DECISION              
044400        05 FILLER            PIC X(26).                                   
044500*** END OF VILMAII-COPY LENGTH= 550 BYTES                                 
