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
009100        05 FLSPECPR          PIC X.                                       
009200*                                 SPECIALPRISFLAGGA                       
009300*                                 SPECIAL PRICE FLAG                      
009400        05 FLTPO1            PIC X.                                       
009500*                                 ARTIKELN GODKÄND FÖR TPO1               
009600*                                 TPO1 ALLOWED FOR ARTICLE                
009700        05 IDANSK            PIC S9(3)           COMP-3.                  
009800*                                 ANSKAFFARNUMMER                         
009900*                                 PROCURER NO.                            
010000        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
010100*                                 EMBALLAGEARTIKELNR FÖR Q0               
010200        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
010300*                                 EMBALLAGEARTIKELNR FÖR Q1               
010400        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
010500*                                 EMBALLAGEARTIKELNR FÖR Q2               
010600        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
010700*                                 EMBALLAGEARTIKELNR FÖR Q3               
010800        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
010900*                                 EMBALLAGEARTIKELNR FÖR Q4               
011000        05 IDBERED           PIC S9(3)           COMP-3.                  
011100*                                 BEREDARENUMMER                          
011200        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
011300*                                 FUNKTIONSGRUPP                          
011400*                                 FUNCTION GROUP                          
011500        05 IDINK             PIC S9(3)           COMP-3.                  
011600*                                 INKÖPARNUMMER                           
011700*                                 PURCHASE IDENTIFICATION NUMBER          
011800        05 IDLEVNR           PIC S9(5)           COMP-3.                  
011900*                                 LEVERANTÖRNUMMER                        
012000*                                 SUPPLIER NUMBER (VENDORNUMBER)          
012100        05 IDLKTO            PIC S9(7)           COMP-3.                  
012200*                                 LAGERKONTO (FFHHHUU)                    
012300*                                 STOCK ACCOUNT (CCMMMSS)                 
012400        05 IDPROJ            PIC X(4).                                    
012500*                                 PARTS PROJEKTIDENTITET                  
012600*                                 PARTS PROJECT IDENTITY                  
012700        05 IDPSN             PIC 9(3).                                    
012800*                                 PROPER SHIPPING NAME                    
012900*                                 PROPER SHIPPING NAME                    
013000        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
013100*                                 STATISTISKT NUMMER                      
013200*                                 1 = NORSKT                              
013300*                                 2 = ENGELSKT                            
013400*                                 3 = BELGISKT                            
013500*                                 4 = PERUANSKT                           
013600*                                 5 = SVENSKT                             
013700*                                 6 =                                     
013800*                                 STATISTICAL NO.                         
013900        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
014000*                                 STATISTISKT NUMMER                      
014100*                                 1 = NORSKT                              
014200*                                 2 = ENGELSKT                            
014300*                                 3 = BELGISKT                            
014400*                                 4 = PERUANSKT                           
014500*                                 5 = SVENSKT                             
014600*                                 6 =                                     
014700*                                 STATISTICAL NO.                         
014800        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
014900*                                 STATISTISKT NUMMER                      
015000*                                 1 = NORSKT                              
015100*                                 2 = ENGELSKT                            
015200*                                 3 = BELGISKT                            
015300*                                 4 = PERUANSKT                           
015400*                                 5 = SVENSKT                             
015500*                                 6 =                                     
015600*                                 STATISTICAL NO.                         
015700        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
015800*                                 STATISTISKT NUMMER                      
015900*                                 1 = NORSKT                              
016000*                                 2 = ENGELSKT                            
016100*                                 3 = BELGISKT                            
016200*                                 4 = PERUANSKT                           
016300*                                 5 = SVENSKT                             
016400*                                 6 =                                     
016500*                                 STATISTICAL NO.                         
016600        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
016700*                                 STATISTISKT NUMMER                      
016800*                                 1 = NORSKT                              
016900*                                 2 = ENGELSKT                            
017000*                                 3 = BELGISKT                            
017100*                                 4 = PERUANSKT                           
017200*                                 5 = SVENSKT                             
017300*                                 6 =                                     
017400*                                 STATISTICAL NO.                         
017500        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
017600*                                 STATISTISKT NUMMER                      
017700*                                 1 = NORSKT                              
017800*                                 2 = ENGELSKT                            
017900*                                 3 = BELGISKT                            
018000*                                 4 = PERUANSKT                           
018100*                                 5 = SVENSKT                             
018200*                                 6 =                                     
018300*                                 STATISTICAL NO.                         
018400        05 KDAGE             PIC X.                                       
018500*                                 AGE-CODE                                
018600*                                 AGE-CODE                                
018700        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018800*                                 HANTERINGSKOD                           
018900*                                 HANDLING CODE                           
019000        05 KDARTURS          PIC X(2).                                    
019100*                                 ARTIKELURSPRUNGSKOD                     
019200*                                 COUNTRY OF ORIGIN                       
019300        05 KDBPSR            PIC S9              COMP-3.                  
019400*                                 BASLAGERFÖRSLAGSNIVÅ                    
019500*                                 BASIC PART STOCK RECOMMENDATION         
019600        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019700*                                 EMBALLAGEKOD 0                          
019800        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019900*                                 EMBALLAGEKOD 1                          
020000        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
020100*                                 EMBALLAGEKOD 2                          
020200        05 KDERS             PIC S9(3)           COMP-3.                  
020300*                                 ERSÄTTNINGSKOD                          
020400*                                 SUPERSESSION CODE                       
020500        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
020600*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020700*                                 OBSOLETION SUPERSESSION CODE            
020800        05 KDFARLIG          PIC S9              COMP-3.                  
020900*                                 KOD FÖR FARLIGT GODS                    
021000*                                 DANGEROUS GOODS CODE                    
021100        05 KDGK              PIC S9              COMP-3.                  
021200*                                 GODSMOTTAGAREKOD                        
021300*                                 GOODS RECEIVING WAREHOUSE CODE          
021400        05 KDPRODSL          PIC S9(3)           COMP-3.                  
021500*                                 PRODUKTSLAG                             
021600*                                 PRODUCT GROUP                           
021700        05 KDSORT            PIC X(2).                                    
021800*                                 SORT-KOD                                
021900*                                 UNIT OF MEASURE                         
022000        05 KDSPEEMB          PIC 9.                                       
022100*                                 SPECIALEMBALLAGEKOD                     
022200*                                 SPECIAL PACKING CODE                    
022300        05 KDSRA             PIC S9(3)           COMP-3.                  
022400*                                 SRA-KOD                                 
022500*                                 SRA CODE                                
022600        05 KDUART            PIC X.                                       
022700*                                 UNDANTAGSARTIKEL                        
022800*                                 EXECPTION PARTS                         
022900        05 KDVVKL            PIC S9              COMP-3.                  
023000*                                 VOLYMVÄRDESKLASS                        
023100*                                 VOLUME VALUE CLASS                      
023200        05 KDYTBEH           PIC S9(3)           COMP-3.                  
023300*                                 YTBEHANDLINGSKOD                        
023400*                                                                         
023500        05 KVPALL            PIC S9(7)           COMP-3.                  
023600*                                 ANTAL I PALL                            
023700*                                 QUANTITY IN PALLET                      
023800        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023900*                                 ANTAL I Q0 FÖRPACKNING                  
024000        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
024100*                                 ANTAL I Q1 FÖRPACKNING                  
024200*                                 QUANTITY IN BULK PACK Q1                
024300        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
024400*                                 ANTAL I Q2 FÖRPACKNING                  
024500*                                 QUANTITY IN BULK PACK Q2                
024600        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024700*                                 ANTAL I Q3 FÖRPACKNING                  
024800*                                 QUANTITY IN BULK PACK Q3                
024900        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
025000*                                 ANTAL I Q4 FÖRPACKNING                  
025100*                                 QUANTITY IN BULK PACK Q4                
025200        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
025300*                                 BESTÄLLNINGSPRIS I KRONOR               
025400*                                 ORDER PRICE SWEDISH CURRENCY            
025500        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
025600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025700*                                 GROSS-PRICE EXPORT                      
025800*                                  (FOB-GROSS)                            
025900        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
026000*                                 ARTIKELNS SJÄLVKOSTNAD                  
026100*                                 COST OF SALES                           
026200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
026300*                                 ARTIKELSTANDARDPRIS                     
026400*                                 STANDARD PRICE                          
026500        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
026600*                                 INKÖPSPRIS                              
026700*                                 PURCHASE PRICE                          
026800        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026900*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
027000*                                 DATE OF SUPERSESSION (YYWWD)            
027100        05 TIFINLV           PIC S9(5)           COMP-3.                  
027200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
027300*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
027400        05 VKART             PIC S9(7)           COMP-3.                  
027500*                                 ARTIKELVIKT (G)                         
027600*                                 PART WEIGHT (G)                         
027700        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027800*                                 ARTIKELVOLYM NETTO (CM3)                
027900*                                 PART NET VOLUME    (CM3)                
028000        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
028100*                                 PERSONKOD REFILLANSVARIG                
028200*                                 REFILL RESPONSIBLE ID                   
028300        05 IDPSN-US          PIC 9(3).                                    
028400*                                 PROPER SHIPPING NAME USA                
028500*                                 PROPER SHIPPING NAME USA                
028600        05 IDPSN-CA          PIC 9(3).                                    
028700*                                 PROPER SHIPPING NAME KANADA             
028800*                                 PROPER SHIPPING NAME CANADA             
028900        05 FILLER            PIC X(92).                                   
029000     03 SDC-INFO.                                                         
029100*                                 INFO SOM GÄLLER ENBART SDC              
029200*                                                                         
029300*                                                                         
029400*                                 INFO VALID ONLY FOR SDC                 
029500        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029600*                                 LAGEROMRÅDE                             
029700*                                 AREA                                    
029800        05 ADGANG            PIC S9(3)           COMP-3.                  
029900*                                 GÅNG                                    
030000*                                 AISLE                                   
030100        05 ADPLATS           PIC S9(5)           COMP-3.                  
030200*                                 LAGERPLATSNUMMER                        
030300*                                 LOCATION                                
030400        05 FLREFBEO          PIC X.                                       
030500*                                 AUTOMATISK REFILL BEORDRING?            
030600*                                 AUTOMATIC REFILL ORDERING?              
030700        05 FLREFILL          PIC X.                                       
030800*                                 REFILLARTIKEL                           
030900*                                 REFILLPART                              
031000        05 FLREFNYO          PIC X.                                       
031100*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031200*                                 WAIT FOR NEXT DEMAND                    
031300        05 FLSKROT-BEORD     PIC X.                                       
031400*                                 SKROTNING BEORDRAD AV ANSK              
031500*                                 SCRAPPING ORDERED BY PROCURER           
031600        05 KDFREKKL          PIC X.                                       
031700*                                 FREKVENSKLASS                           
031800*                                 FREQ. CLASS                             
031900        05 KDLEVSP           PIC S9(3)           COMP-3.                  
032000*                                 SPÄRRKOD LEVERANS                       
032100*                                 DELIVERY BLOCKING CODE                  
032200        05 KDPRISKL          PIC X.                                       
032300*                                 PRISKLASS                               
032400*                                 PRICE CLASS                             
032500        05 KDREFSTA          PIC X.                                       
032600*                                 STATUS REFILLARTIKEL                    
032700*                                 STATUS REFILLPART                       
032800        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
032900*                                 DEL AV AK PÅ VÄG                        
033000*                                 PART OF AK ON ITS WAY                   
033100        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
033200*                                 DEL AV AK SOM LIGGER I SDC              
033300*                                 PART OF AK IN THE SDC                   
033400        05 KVBEART           PIC S9(7)           COMP-3.                  
033500*                                 BESTÄLLT ANTAL STYCKEN                  
033600*                                 ORDERED QUANTITY                        
033700        05 KVEFRS            PIC S9(7)           COMP-3.                  
033800*                                 EJ FAKTURERAT ANTAL STYCK               
033900*                                 ORDERED NOT INVOICED QTY                
034000        05 KVINVS            PIC S9(7)           COMP-3.                  
034100*                                 INVENTERINGSSALDO                       
034200*                                 STOCK-TAKING BALANCE                    
034300        05 KVLS              PIC S9(7)           COMP-3.                  
034400*                                 LAGERSALDO                              
034500*                                 STOCK BALANCE                           
034600        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
034700*                                 ORDERINGÅNG LEV FRÅN CDC                
034800*                                 ORDERED PCS PER TIME UNIT               
034900*                                 FORWARDED TO CDC                        
035000        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
035100*                                 ORDERINGÅNG TILL SDC                    
035200*                                 ORDERED PCS PER TIME UNIT               
035300*                                 ORDERED FROM SDC                        
035400        05 KVOKS             PIC S9(7)           COMP-3.                  
035500*                                 ORDERKÖSALDO                            
035600*                                 ORDER QUEUE BALANCE                     
035700        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
035800*                                 ORDERTRÄFFAR LEV FRÅN CDC               
035900*                                 ORDERHITS ON SDC FORWARDED              
036000*                                 TO CDC                                  
036100        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
036200*                                 ORDERTRÄFFAR PÅ SDC                     
036300*                                 ORDERHITS ON SDC                        
036400        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
036500*                                 PERIODBEHOV REFILLING                   
036600*                                 FORECAST REFILLING                      
036700        05 KVREFBER          PIC S9(7)           COMP-3.                  
036800*                                 BERÄKNAD REFILLINGKVANTITET             
036900*                                 CALCULATED REFILLING QUANTITY           
037000        05 KVREFOVL          PIC S9(7)           COMP-3.                  
037100*                                 BERÄKNAD ÖVERLAGERPUNKT                 
037200*                                 CALCULATED OVERSTOCK POINT              
037300        05 KVREFPKT          PIC S9(7)           COMP-3.                  
037400*                                 BERÄKNAD PÅFYLLNADSPUNKT                
037500*                                 CALCULATED REFILLING POINT              
037600        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
037700*                                 ANTAL SENASTE RETURORDER                
037800*                                 QUANTITY LAST RETURNORDER               
037900        05 KVSKROT           PIC S9(7)           COMP-3.                  
038000*                                 ANTAL SENASTE SKROTORDER                
038100*                                 QUANTITY LAST SCRAPORDER                
038200        05 KVUTRS            PIC S9(7)           COMP-3.                  
038300*                                 UTREDNINGSSALDO                         
038400*                                 INVESTIG.BALANCE                        
038500        05 TIINVDAT          PIC S9(5)           COMP-3.                  
038600*                                 INVENTERINGSDATUM                       
038700*                                 STOCKTAKING DATE                        
038800        05 TIORDREG          PIC S9(7)           COMP-3.                  
038900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
039000*                                 ORDER REGISTRATION DATE  YYMMDD         
039100        05 TIREFEFT          PIC S9(7)           COMP-3.                  
039200*                                 DATUM SENAST EFTERFRÅGAD                
039300*                                 DATE LATEST DEMAND                      
039400        05 TIREFMPB          PIC S9(7)           COMP-3.                  
039500*                                 DATUM MANUELL PROGNOS REFILLING         
039600*                                 DATE MANUAL FORECAST REFILLING          
039700        05 TIREFPAF          PIC S9(7)           COMP-3.                  
039800*                                 DATUM MANUELL PÅFYLLNADSKVANT           
039900*                                 DATE MANUAL REFILLING QTY               
040000        05 TIREFPKT          PIC S9(7)           COMP-3.                  
040100*                                 DATUM MANUELL REFILLPUNKT               
040200*                                 DATE MANUAL REFILLING POINT             
040300        05 TIREFSTA          PIC S9(7)           COMP-3.                  
040400*                                 DATUM AKT/PASS REFILLARTIKEL            
040500*                                 DATE ACT/PASS REFILLPART                
040600        05 TIREFSTO          PIC S9(7)           COMP-3.                  
040700*                                 BEORDRINGSSTOPPAD T.OM.                 
040800*                                 STOPPED FOR ORDERING UNTIL              
040900        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
041000*                                 DATUM RETUR BEORDRING                   
041100*                                 DATE ISSUE OF RETURN ORDER              
041200        05 TISKROT           PIC S9(7)           COMP-3.                  
041300*                                 SKROTNINGSDATUM                         
041400*                                 DATE OF SCRAPPING                       
041500        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
041600*                                 BEORDRAD SKROTNINGSDATUM                
041700*                                 DATE OF SCRAPPING DECISION              
041800        05 FILLER            PIC X(33).                                   
041900*** END OF VILMAII-COPY LENGTH= 500 BYTES                                 
