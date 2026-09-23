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
008400        05 FLSPECPR          PIC X.                                       
008500*                                 SPECIALPRISFLAGGA                       
008600*                                 SPECIAL PRICE FLAG                      
008700        05 FLTPO1            PIC X.                                       
008800*                                 ARTIKELN GODKÄND FÖR TPO1               
008900*                                 TPO1 ALLOWED FOR ARTICLE                
009000        05 IDANSK            PIC S9(3)           COMP-3.                  
009100*                                 ANSKAFFARNUMMER                         
009200*                                 PROCURER NO.                            
009300        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
009400*                                 EMBALLAGEARTIKELNR FÖR Q0               
009500        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
009600*                                 EMBALLAGEARTIKELNR FÖR Q1               
009700        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
009800*                                 EMBALLAGEARTIKELNR FÖR Q2               
009900        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
010000*                                 EMBALLAGEARTIKELNR FÖR Q3               
010100        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
010200*                                 EMBALLAGEARTIKELNR FÖR Q4               
010300        05 IDBERED           PIC S9(3)           COMP-3.                  
010400*                                 BEREDARENUMMER                          
010500        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
010600*                                 FUNKTIONSGRUPP                          
010700*                                 FUNCTION GROUP                          
010800        05 IDINK             PIC S9(3)           COMP-3.                  
010900*                                 INKÖPARNUMMER                           
011000*                                 PURCHASE IDENTIFICATION NUMBER          
011100        05 IDLEVNR           PIC S9(5)           COMP-3.                  
011200*                                 LEVERANTÖRNUMMER                        
011300*                                 SUPPLIER NUMBER (VENDORNUMBER)          
011400        05 IDLKTO            PIC S9(7)           COMP-3.                  
011500*                                 LAGERKONTO (FFHHHUU)                    
011600*                                 STOCK ACCOUNT (CCMMMSS)                 
011700        05 IDPROJ            PIC X(4).                                    
011800*                                 PARTS PROJEKTIDENTITET                  
011900*                                 PARTS PROJECT IDENTITY                  
012000        05 IDPSN             PIC 9(3).                                    
012100*                                 PROPER SHIPPING NAME                    
012200*                                 PROPER SHIPPING NAME                    
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
027300        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
027400*                                 PERSONKOD REFILLANSVARIG                
027500*                                 REFILL RESPONSIBLE ID                   
027600        05 IDPSN-US          PIC 9(3).                                    
027700*                                 PROPER SHIPPING NAME USA                
027800*                                 PROPER SHIPPING NAME USA                
027900        05 IDPSN-CA          PIC 9(3).                                    
028000*                                 PROPER SHIPPING NAME KANADA             
028100*                                 PROPER SHIPPING NAME CANADA             
028200        05 FILLER            PIC X(92).                                   
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
029700        05 FLREFBEO          PIC X.                                       
029800*                                 AUTOMATISK REFILL BEORDRING?            
029900*                                 AUTOMATIC REFILL ORDERING?              
030000        05 FLREFILL          PIC X.                                       
030100*                                 REFILLARTIKEL                           
030200*                                 REFILLPART                              
030300        05 FLREFNYO          PIC X.                                       
030400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
030500*                                 WAIT FOR NEXT DEMAND                    
030600        05 FLSKROT-BEORD     PIC X.                                       
030700*                                 SKROTNING BEORDRAD AV ANSK              
030800*                                 SCRAPPING ORDERED BY PROCURER           
030900        05 IDLEVNR-NDC       PIC S9(5)           COMP-3.                  
031000*                                 LEVERANTÖRNUMMER                        
031100*                                 SUPPLIER NUMBER (VENDORNUMBER)          
031200        05 KDFREKKL          PIC X.                                       
031300*                                 FREKVENSKLASS                           
031400*                                 FREQ. CLASS                             
031500        05 KDLEVSP           PIC S9(3)           COMP-3.                  
031600*                                 SPÄRRKOD LEVERANS                       
031700*                                 DELIVERY BLOCKING CODE                  
031800        05 KDPRISKL          PIC X.                                       
031900*                                 PRISKLASS                               
032000*                                 PRICE CLASS                             
032100        05 KDPSLLOC          PIC 9(2).                                    
032200*                                 PRODUKTSLAG LOKALT                      
032300*                                 PRODUCT GROUP LOCAL                     
032400        05 KDREFSTA          PIC X.                                       
032500*                                 STATUS REFILLARTIKEL                    
032600*                                 STATUS REFILLPART                       
032700        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
032800*                                 DEL AV AK PÅ VÄG                        
032900*                                 PART OF AK ON ITS WAY                   
033000        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
033100*                                 DEL AV AK SOM LIGGER I SDC              
033200*                                 PART OF AK IN THE SDC                   
033300        05 KVBEART           PIC S9(7)           COMP-3.                  
033400*                                 BESTÄLLT ANTAL STYCKEN                  
033500*                                 ORDERED QUANTITY                        
033600        05 KVEFRS            PIC S9(7)           COMP-3.                  
033700*                                 EJ FAKTURERAT ANTAL STYCK               
033800*                                 ORDERED NOT INVOICED QTY                
033900        05 KVINVS            PIC S9(7)           COMP-3.                  
034000*                                 INVENTERINGSSALDO                       
034100*                                 STOCK-TAKING BALANCE                    
034200        05 KVLS              PIC S9(7)           COMP-3.                  
034300*                                 LAGERSALDO                              
034400*                                 STOCK BALANCE                           
034500        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
034600*                                 ORDERINGÅNG LEV FRÅN CDC                
034700*                                 ORDERED PCS PER TIME UNIT               
034800*                                 FORWARDED TO CDC                        
034900        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
035000*                                 ORDERINGÅNG TILL SDC                    
035100*                                 ORDERED PCS PER TIME UNIT               
035200*                                 ORDERED FROM SDC                        
035300        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
035400*                                 ORDERKÖSALDO, KLASS 2-4                 
035500*                                 ORDER QUEUE BALANCE, CLASS 2-4          
035600        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
035700*                                 ORDERKÖSALDO, KLASS 1                   
035800*                                 ORDER QUEUE BALANCE, CLASS 1            
035900        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
036000*                                 ORDERTRÄFFAR LEV FRÅN CDC               
036100*                                 ORDERHITS ON SDC FORWARDED              
036200*                                 TO CDC                                  
036300        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
036400*                                 ORDERTRÄFFAR PÅ SDC                     
036500*                                 ORDERHITS ON SDC                        
036600        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
036700*                                 PERIODBEHOV REFILLING                   
036800*                                 FORECAST REFILLING                      
036900        05 KVREFBER          PIC S9(7)           COMP-3.                  
037000*                                 BERÄKNAD REFILLINGKVANTITET             
037100*                                 CALCULATED REFILLING QUANTITY           
037200        05 KVREFOVL          PIC S9(7)           COMP-3.                  
037300*                                 BERÄKNAD ÖVERLAGERPUNKT                 
037400*                                 CALCULATED OVERSTOCK POINT              
037500        05 KVREFPKT          PIC S9(7)           COMP-3.                  
037600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
037700*                                 CALCULATED REFILLING POINT              
037800        05 KVRESS            PIC S9(7)           COMP-3.                  
037900*                                 RESERVERAT ANTAL ARTIKLAR               
038000*                                 QUANTITY RESERVED ITEMS                 
038100        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
038200*                                 ANTAL SENASTE RETURORDER                
038300*                                 QUANTITY LAST RETURNORDER               
038400        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
038500*                                 RESTORDERSALDO, KLASS 2-4               
038600*                                 BACK ORDER BALANCE, CLASS 2-4           
038700        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
038800*                                 RESTORDERSALDO, KLASS 1                 
038900*                                 BACK ORDER BALANCE, CLASS 1             
039000        05 KVSKROT           PIC S9(7)           COMP-3.                  
039100*                                 ANTAL SENASTE SKROTORDER                
039200*                                 QUANTITY LAST SCRAPORDER                
039300        05 KVUTRS            PIC S9(7)           COMP-3.                  
039400*                                 UTREDNINGSSALDO                         
039500*                                 INVESTIG.BALANCE                        
039600        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
039700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
039800*                                 AVERAGE COST FOREIGN CURRENCY           
039900        05 TIINVDAT          PIC S9(5)           COMP-3.                  
040000*                                 INVENTERINGSDATUM                       
040100*                                 STOCKTAKING DATE                        
040200        05 TIORDREG          PIC S9(7)           COMP-3.                  
040300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
040400*                                 ORDER REGISTRATION DATE  YYMMDD         
040500        05 TIREFEFT          PIC S9(7)           COMP-3.                  
040600*                                 DATUM SENAST EFTERFRÅGAD                
040700*                                 DATE LATEST DEMAND                      
040800        05 TIREFMPB          PIC S9(7)           COMP-3.                  
040900*                                 DATUM MANUELL PROGNOS REFILLING         
041000*                                 DATE MANUAL FORECAST REFILLING          
041100        05 TIREFPAF          PIC S9(7)           COMP-3.                  
041200*                                 DATUM MANUELL PÅFYLLNADSKVANT           
041300*                                 DATE MANUAL REFILLING QTY               
041400        05 TIREFPKT          PIC S9(7)           COMP-3.                  
041500*                                 DATUM MANUELL REFILLPUNKT               
041600*                                 DATE MANUAL REFILLING POINT             
041700        05 TIREFSTA          PIC S9(7)           COMP-3.                  
041800*                                 DATUM AKT/PASS REFILLARTIKEL            
041900*                                 DATE ACT/PASS REFILLPART                
042000        05 TIREFSTO          PIC S9(7)           COMP-3.                  
042100*                                 BEORDRINGSSTOPPAD T.OM.                 
042200*                                 STOPPED FOR ORDERING UNTIL              
042300        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
042400*                                 DATUM RETUR BEORDRING                   
042500*                                 DATE ISSUE OF RETURN ORDER              
042600        05 TISKROT           PIC S9(7)           COMP-3.                  
042700*                                 SKROTNINGSDATUM                         
042800*                                 DATE OF SCRAPPING                       
042900        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
043000*                                 BEORDRAD SKROTNINGSDATUM                
043100*                                 DATE OF SCRAPPING DECISION              
043200        05 FILLER            PIC X(57).                                   
043300*** END OF VILMAII-COPY LENGTH= 550 BYTES                                 
