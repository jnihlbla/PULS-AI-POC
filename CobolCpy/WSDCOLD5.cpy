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
011500        05 IDLEVNR           PIC X(5).                                    
011600*                                 LEVERANTÖRNUMMER                        
011700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
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
028600        05 IDKAT-1           PIC X(5).                                    
028700*                                 KATALOGBETECKNING                       
028800        05 IDKAT-2           PIC X(5).                                    
028900*                                 KATALOGBETECKNING                       
029000        05 IDKAT-3           PIC X(5).                                    
029100*                                 KATALOGBETECKNING                       
029200        05 FILLER            PIC X(78).                                   
029300     03 SDC-INFO.                                                         
029400*                                 INFO SOM GÄLLER ENBART SDC              
029500*                                                                         
029600*                                                                         
029700*                                 INFO VALID ONLY FOR SDC                 
029800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
029900*                                 LAGEROMRÅDE                             
030000*                                 AREA                                    
030100        05 ADGANG            PIC S9(3)           COMP-3.                  
030200*                                 GÅNG                                    
030300*                                 AISLE                                   
030400        05 ADPLATS           PIC S9(5)           COMP-3.                  
030500*                                 LAGERPLATSNUMMER                        
030600*                                 LOCATION                                
030700        05 FLREFBEO          PIC X.                                       
030800*                                 AUTOMATISK REFILL BEORDRING?            
030900*                                 AUTOMATIC REFILL ORDERING?              
031000        05 FLREFILL          PIC X.                                       
031100*                                 REFILLARTIKEL                           
031200*                                 REFILLPART                              
031300        05 FLREFNYO          PIC X.                                       
031400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
031500*                                 WAIT FOR NEXT DEMAND                    
031600        05 FLSEASON          PIC X.                                       
031700*                                 SÄSONG PÅ ARTIKEL                       
031800*                                 SEASON MARK PER PART                    
031900        05 FLSKROT-BEORD     PIC X.                                       
032000*                                 SKROTNING BEORDRAD AV ANSK              
032100*                                 SCRAPPING ORDERED BY PROCURER           
032200        05 FLWILSON          PIC X.                                       
032300*                                 WILSONFORMEL                            
032400*                                 FLAG TO USE WILSON OR NOT               
032500        05 KDFREKKL          PIC X.                                       
032600*                                 FREKVENSKLASS                           
032700*                                 FREQ. CLASS                             
032800        05 KDLEVSP           PIC S9(3)           COMP-3.                  
032900*                                 SPÄRRKOD LEVERANS                       
033000*                                 DELIVERY BLOCKING CODE                  
033100        05 KDPRISKL          PIC X.                                       
033200*                                 PRISKLASS                               
033300*                                 PRICE CLASS                             
033400        05 KDREFSTA          PIC X.                                       
033500*                                 STATUS REFILLARTIKEL                    
033600*                                 STATUS REFILLPART                       
033700        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
033800*                                 DEL AV AK PÅ VÄG                        
033900*                                 PART OF AK ON ITS WAY                   
034000        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
034100*                                 DEL AV AK SOM LIGGER I SDC              
034200*                                 PART OF AK IN THE SDC                   
034300        05 KVBEART           PIC S9(7)           COMP-3.                  
034400*                                 BESTÄLLT ANTAL STYCKEN                  
034500*                                 ORDERED QUANTITY                        
034600        05 KVEFRS            PIC S9(7)           COMP-3.                  
034700*                                 EJ FAKTURERAT ANTAL STYCK               
034800*                                 ORDERED NOT INVOICED QTY                
034900        05 KVINVS            PIC S9(7)           COMP-3.                  
035000*                                 INVENTERINGSSALDO                       
035100*                                 STOCK-TAKING BALANCE                    
035200        05 KVLS              PIC S9(7)           COMP-3.                  
035300*                                 LAGERSALDO                              
035400*                                 STOCK BALANCE                           
035500        05 KVOI-CDC-RULL     PIC S9(7)           COMP-3.                  
035600*                                 ORDERINGÅNG LEV FRÅN CDC                
035700*                                 ORDERED PCS PER TIME UNIT               
035800*                                 FORWARDED TO CDC                        
035900        05 KVOI-RULL         PIC S9(7)           COMP-3.                  
036000*                                 ORDERINGÅNG TILL SDC                    
036100*                                 ORDERED PCS PER TIME UNIT               
036200*                                 ORDERED FROM SDC                        
036300        05 KVOI-RP           OCCURS 12 TIMES                              
036400                             PIC S9(7)           COMP-3.                  
036500*                                 ORDERINGÅNG TILL DC                     
036600*                                 ORDERED PCS PER TIME UNIT               
036700*                                 ORDERED FROM DC                         
036800*                                 WEIGHTENED BY 4.33                      
036900        05 KVOI-FOREG        OCCURS 2 TIMES                               
037000                             PIC S9(7)           COMP-3.                  
037100*                                 ORDERINGÅNG TILL DC FÖREG ÅR            
037200*                                 ORDERED PCS LAST YEAR                   
037300*                                 ORDERED FROM DC                         
037400        05 KVOI-INNEV        PIC S9(7)           COMP-3.                  
037500*                                 ORDERINGÅNG TILL DC INNEV PER           
037600*                                 ORDERED PCS THIS PERIOD                 
037700*                                 ORDERED FROM DC                         
037800        05 KVOI-IAAR         PIC S9(7)           COMP-3.                  
037900*                                 ORDERINGÅNG TILL DC INNEV ÅR            
038000*                                 ORDERED PCS PER TIME UNIT               
038100*                                 ORDERED FROM DC                         
038200        05 KVOKS             PIC S9(7)           COMP-3.                  
038300*                                 ORDERKÖSALDO                            
038400*                                 ORDER QUEUE BALANCE                     
038500        05 KVOT-CDC-RULL     PIC S9(7)           COMP-3.                  
038600*                                 ORDERTRÄFFAR LEV FRÅN CDC               
038700*                                 ORDERHITS ON SDC FORWARDED              
038800*                                 TO CDC                                  
038900        05 KVOT-RULL         PIC S9(7)           COMP-3.                  
039000*                                 ORDERTRÄFFAR PÅ SDC                     
039100*                                 ORDERHITS ON SDC                        
039200        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
039300*                                 PERIODBEHOV REFILLING                   
039400*                                 FORECAST REFILLING                      
039500        05 KVREFBER          PIC S9(7)           COMP-3.                  
039600*                                 BERÄKNAD REFILLINGKVANTITET             
039700*                                 CALCULATED REFILLING QUANTITY           
039800        05 KVREFOVL          PIC S9(7)           COMP-3.                  
039900*                                 BERÄKNAD ÖVERLAGERPUNKT                 
040000*                                 CALCULATED OVERSTOCK POINT              
040100        05 KVREFPKT          PIC S9(7)           COMP-3.                  
040200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
040300*                                 CALCULATED REFILLING POINT              
040400        05 KVRETUR-BEORD     PIC S9(7)           COMP-3.                  
040500*                                 ANTAL SENASTE RETURORDER                
040600*                                 QUANTITY LAST RETURNORDER               
040700        05 KVSKROT           PIC S9(7)           COMP-3.                  
040800*                                 ANTAL SENASTE SKROTORDER                
040900*                                 QUANTITY LAST SCRAPORDER                
041000        05 KVUTRS            PIC S9(7)           COMP-3.                  
041100*                                 UTREDNINGSSALDO                         
041200*                                 INVESTIG.BALANCE                        
041300        05 REOSAEK           PIC S9(3)V9(1)      COMP-3.                  
041400*                                 SEASONAL UNCERTAINTY FACTOR             
041500        05 RESEASON          OCCURS 12 TIMES                              
041600                             PIC S9V9(2)         COMP-3.                  
041700*                                 SÄSONGSINDEX                            
041800        05 TIINVDAT          PIC S9(5)           COMP-3.                  
041900*                                 INVENTERINGSDATUM                       
042000*                                 STOCKTAKING DATE                        
042100        05 TIORDREG          PIC S9(7)           COMP-3.                  
042200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
042300*                                 ORDER REGISTRATION DATE  YYMMDD         
042400        05 TIREFEFT          PIC S9(7)           COMP-3.                  
042500*                                 DATUM SENAST EFTERFRÅGAD                
042600*                                 DATE LATEST DEMAND                      
042700        05 TIREFMPB          PIC S9(7)           COMP-3.                  
042800*                                 DATUM MANUELL PROGNOS REFILLING         
042900*                                 DATE MANUAL FORECAST REFILLING          
043000        05 TIREFPAF          PIC S9(7)           COMP-3.                  
043100*                                 DATUM MANUELL PÅFYLLNADSKVANT           
043200*                                 DATE MANUAL REFILLING QTY               
043300        05 TIREFPKT          PIC S9(7)           COMP-3.                  
043400*                                 DATUM MANUELL REFILLPUNKT               
043500*                                 DATE MANUAL REFILLING POINT             
043600        05 TIREFSTA          PIC S9(7)           COMP-3.                  
043700*                                 DATUM AKT/PASS REFILLARTIKEL            
043800*                                 DATE ACT/PASS REFILLPART                
043900        05 TIREFSTO          PIC S9(7)           COMP-3.                  
044000*                                 BEORDRINGSSTOPPAD T.OM.                 
044100*                                 STOPPED FOR ORDERING UNTIL              
044200        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
044300*                                 DATUM RETUR BEORDRING                   
044400*                                 DATE ISSUE OF RETURN ORDER              
044500        05 TISKROT           PIC S9(7)           COMP-3.                  
044600*                                 SKROTNINGSDATUM                         
044700*                                 DATE OF SCRAPPING                       
044800        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
044900*                                 BEORDRAD SKROTNINGSDATUM                
045000*                                 DATE OF SCRAPPING DECISION              
045100        05 TISPARR-KVAL      PIC 9(6).                                    
045200*                                 SPÄRRAD DATUM KVALITETSFEL              
045300*                                 BLOCKED DATE QUALITY ERROR              
045400        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
045500*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
045600*                                 AREA ADDRESS CROSS DOCKING WARE         
045700*                                 HOUSE                                   
045800        05 FLCDREL           PIC X.                                       
045900*                                 OMGÅENDE RELEASE AV CD-REFILL           
046000*                                 IMMEDIATE RELEASE OF CD-REFILL          
046100        05 KVDAGAR-CDBEH     PIC S9(3)           COMP-3.                  
046200*                                 NO. OF DAYS TO BE USED WHEN             
046300*                                 CALCULATING CD REFILLORDERS             
046400        05 FILLER            PIC X(21).                                   
046500*** END OF VILMAII-COPY LENGTH= 594 BYTES                                 
