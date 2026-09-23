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
011200        05 IDINK             PIC S9(3)           COMP-3.                  
011300*                                 INKÖPARNUMMER                           
011400*                                 PURCHASE IDENTIFICATION NUMBER          
011500        05 IDLEVNR           PIC S9(5)           COMP-3.                  
011600*                                 LEVERANTÖRNUMMER                        
011700*                                 SUPPLIER NUMBER (VENDORNUMBER)          
011800        05 IDLKTO            PIC S9(7)           COMP-3.                  
011900*                                 LAGERKONTO (FFHHHUU)                    
012000*                                 STOCK ACCOUNT (CCMMMSS)                 
012100        05 IDPROJ            PIC X(4).                                    
012200*                                 PARTS PROJEKTIDENTITET                  
012300*                                 PARTS PROJECT IDENTITY                  
012400        05 IDPSN             PIC 9(3).                                    
012500*                                 PROPER SHIPPING NAME                    
012600*                                 PROPER SHIPPING NAME                    
012700        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
012800*                                 PERSONKOD REFILLANSVARIG                
012900*                                 REFILL RESPONSIBLE ID                   
013000        05 IDPSN-US          PIC 9(3).                                    
013100*                                 PROPER SHIPPING NAME USA                
013200*                                 PROPER SHIPPING NAME USA                
013300        05 IDPSN-CA          PIC 9(3).                                    
013400*                                 PROPER SHIPPING NAME KANADA             
013500*                                 PROPER SHIPPING NAME CANADA             
013600        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
013700*                                 STATISTISKT NUMMER                      
013800*                                 1 = NORSKT                              
013900*                                 2 = ENGELSKT                            
014000*                                 3 = BELGISKT                            
014100*                                 4 = PERUANSKT                           
014200*                                 5 = SVENSKT                             
014300*                                 6 =                                     
014400*                                 STATISTICAL NO.                         
014500        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
014600*                                 STATISTISKT NUMMER                      
014700*                                 1 = NORSKT                              
014800*                                 2 = ENGELSKT                            
014900*                                 3 = BELGISKT                            
015000*                                 4 = PERUANSKT                           
015100*                                 5 = SVENSKT                             
015200*                                 6 =                                     
015300*                                 STATISTICAL NO.                         
015400        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
015500*                                 STATISTISKT NUMMER                      
015600*                                 1 = NORSKT                              
015700*                                 2 = ENGELSKT                            
015800*                                 3 = BELGISKT                            
015900*                                 4 = PERUANSKT                           
016000*                                 5 = SVENSKT                             
016100*                                 6 =                                     
016200*                                 STATISTICAL NO.                         
016300        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
016400*                                 STATISTISKT NUMMER                      
016500*                                 1 = NORSKT                              
016600*                                 2 = ENGELSKT                            
016700*                                 3 = BELGISKT                            
016800*                                 4 = PERUANSKT                           
016900*                                 5 = SVENSKT                             
017000*                                 6 =                                     
017100*                                 STATISTICAL NO.                         
017200        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
017300*                                 STATISTISKT NUMMER                      
017400*                                 1 = NORSKT                              
017500*                                 2 = ENGELSKT                            
017600*                                 3 = BELGISKT                            
017700*                                 4 = PERUANSKT                           
017800*                                 5 = SVENSKT                             
017900*                                 6 =                                     
018000*                                 STATISTICAL NO.                         
018100        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
018200*                                 STATISTISKT NUMMER                      
018300*                                 1 = NORSKT                              
018400*                                 2 = ENGELSKT                            
018500*                                 3 = BELGISKT                            
018600*                                 4 = PERUANSKT                           
018700*                                 5 = SVENSKT                             
018800*                                 6 =                                     
018900*                                 STATISTICAL NO.                         
019000        05 KDAGE             PIC X.                                       
019100*                                 AGE-CODE                                
019200*                                 AGE-CODE                                
019300        05 KDARTHNT          PIC S9(7)           COMP-3.                  
019400*                                 HANTERINGSKOD                           
019500*                                 HANDLING CODE                           
019600        05 KDARTURS          PIC X(2).                                    
019700*                                 ARTIKELURSPRUNGSKOD                     
019800*                                 COUNTRY OF ORIGIN                       
019900        05 KDBPSR            PIC S9              COMP-3.                  
020000*                                 BASLAGERFÖRSLAGSNIVÅ                    
020100*                                 BASIC PART STOCK RECOMMENDATION         
020200        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
020300*                                 EMBALLAGEKOD 0                          
020400        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
020500*                                 EMBALLAGEKOD 1                          
020600        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
020700*                                 EMBALLAGEKOD 2                          
020800        05 KDERS             PIC S9(3)           COMP-3.                  
020900*                                 ERSÄTTNINGSKOD                          
021000*                                 SUPERSESSION CODE                       
021100        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
021200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
021300*                                 OBSOLETION SUPERSESSION CODE            
021400        05 KDFARLIG          PIC S9              COMP-3.                  
021500*                                 KOD FÖR FARLIGT GODS                    
021600*                                 DANGEROUS GOODS CODE                    
021700        05 KDGK              PIC S9              COMP-3.                  
021800*                                 GODSMOTTAGAREKOD                        
021900*                                 GOODS RECEIVING WAREHOUSE CODE          
022000        05 KDPRODSL          PIC S9(3)           COMP-3.                  
022100*                                 PRODUKTSLAG                             
022200*                                 PRODUCT GROUP                           
022300        05 KDSORT            PIC X(2).                                    
022400*                                 SORT-KOD                                
022500*                                 UNIT OF MEASURE                         
022600        05 KDSPEEMB          PIC 9.                                       
022700*                                 SPECIALEMBALLAGEKOD                     
022800*                                 SPECIAL PACKING CODE                    
022900        05 KDSRA             PIC S9(3)           COMP-3.                  
023000*                                 SRA-KOD                                 
023100*                                 SRA CODE                                
023200        05 KDUART            PIC X.                                       
023300*                                 UNDANTAGSARTIKEL                        
023400*                                 EXECPTION PARTS                         
023500        05 KDVVKL            PIC S9              COMP-3.                  
023600*                                 VOLYMVÄRDESKLASS                        
023700*                                 VOLUME VALUE CLASS                      
023800        05 KDYTBEH           PIC S9(3)           COMP-3.                  
023900*                                 YTBEHANDLINGSKOD                        
024000*                                                                         
024100        05 KVPALL            PIC S9(7)           COMP-3.                  
024200*                                 ANTAL I PALL                            
024300*                                 QUANTITY IN PALLET                      
024400        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
024500*                                 ANTAL I Q0 FÖRPACKNING                  
024600        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
024700*                                 ANTAL I Q1 FÖRPACKNING                  
024800*                                 QUANTITY IN BULK PACK Q1                
024900        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
025000*                                 ANTAL I Q2 FÖRPACKNING                  
025100*                                 QUANTITY IN BULK PACK Q2                
025200        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
025300*                                 ANTAL I Q3 FÖRPACKNING                  
025400*                                 QUANTITY IN BULK PACK Q3                
025500        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
025600*                                 ANTAL I Q4 FÖRPACKNING                  
025700*                                 QUANTITY IN BULK PACK Q4                
025800        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
025900*                                 BESTÄLLNINGSPRIS I KRONOR               
026000*                                 ORDER PRICE SWEDISH CURRENCY            
026100        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
026200*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
026300*                                 GROSS-PRICE EXPORT                      
026400*                                  (FOB-GROSS)                            
026500        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
026600*                                 ARTIKELNS SJÄLVKOSTNAD                  
026700*                                 COST OF SALES                           
026800        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
026900*                                 ARTIKELSTANDARDPRIS                     
027000*                                 STANDARD PRICE                          
027100        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
027200*                                 INKÖPSPRIS                              
027300*                                 PURCHASE PRICE                          
027400        05 TIERSDAT          PIC S9(5)           COMP-3.                  
027500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
027600*                                 DATE OF SUPERSESSION (YYWWD)            
027700        05 TIFINLV           PIC S9(5)           COMP-3.                  
027800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
027900*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
028000        05 VKART             PIC S9(7)           COMP-3.                  
028100*                                 ARTIKELVIKT (G)                         
028200*                                 PART WEIGHT (G)                         
028300        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
028400*                                 ARTIKELVOLYM NETTO (CM3)                
028500*                                 PART NET VOLUME    (CM3)                
028600        05 FILLER            PIC X(93).                                   
028700     03 SDC-INFO.                                                         
028800*                                 INFO SOM GÄLLER ENBART SDC              
028900*                                                                         
029000*                                                                         
029100*                                 INFO VALID ONLY FOR SDC                 
029200        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029300*                                 LAGEROMRÅDE                             
029400*                                 AREA                                    
029500        05 ADGANG            PIC S9(3)           COMP-3.                  
029600*                                 GÅNG                                    
029700*                                 AISLE                                   
029800        05 ADPLATS           PIC S9(5)           COMP-3.                  
029900*                                 LAGERPLATSNUMMER                        
030000*                                 LOCATION                                
030100        05 FLREFBEO          PIC X.                                       
030200*                                 AUTOMATISK REFILL BEORDRING?            
030300*                                 AUTOMATIC REFILL ORDERING?              
030400        05 FLREFILL          PIC X.                                       
030500*                                 REFILLARTIKEL                           
030600*                                 REFILLPART                              
030700        05 FLREFNYO          PIC X.                                       
030800*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
030900*                                 WAIT FOR NEXT DEMAND                    
031000        05 FLSKROT-BEORD     PIC X.                                       
031100*                                 SKROTNING BEORDRAD AV ANSK              
031200*                                 SCRAPPING ORDERED BY PROCURER           
031300        05 FLWILSON          PIC X.                                       
031400*                                 WILSONFORMEL                            
031500*                                 FLAG TO USE WILSON OR NOT               
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
041800        05 FILLER            PIC X(32).                                   
041900*** END OF VILMAII-COPY LENGTH= 500 BYTES                                 
