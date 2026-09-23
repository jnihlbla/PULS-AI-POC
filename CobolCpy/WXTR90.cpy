000100 01  WXTR90.                                                              
000200*                                 PRE-EXTRAKT                             
000300*                                                                         
000400*                                 UTGÖR GRUND FÖR FRAMSTÄLLNING           
000500*                                 AV LAGERBAND FÖR W*DCDAY                
000600*                                                  W*DCWEEK               
000700*                                                  W*DCACC                
000800*                                                  W*DCPLAN               
000900*                                                  W*DCYEAR               
001000*                                 DÄR * STÅR FÖR C, S ELLER N.            
001100*                                                                         
001200*                                 FRAMSTÄLLS VARJE                        
001300*                                            MORGON (DAGLIGEN)            
001400*                                            VECKOSLUT                    
001500*                                            REDOVISNINGSPERIOD           
001600*                                            PLANERINGSPERIOD             
001700*                                            ÅRS-SLUT                     
001800*                                                                         
001900*                                 1 POST/ARTIKELNUMMER                    
002000*                                                                         
002100*                                 PRIMARY EXTRACT                         
002200*                                                                         
002300*                                 PART INFORMATION FILE USED TO           
002400*                                 PRODUCE EXTRACTS W*DCDAY                
002500*                                                  W*DCWEEK               
002600*                                                  W*DCACC                
002700*                                                  W*DCPLAN               
002800*                                                  W*DCYEAR               
002900*                                 WHERE * STANDS FOR C, S OR N.           
003000*                                                                         
003100*                                 CREATED EVERY                           
003200*                                               MORNING (DAILY)           
003300*                                               WEEKEND                   
003400*                                               ACCOUNTPERIOD             
003500*                                               PLANNINGPERIOD            
003600*                                               YEAR                      
003700*                                                                         
003800*                                 1 RECORD/PART NUMBER                    
003900*                                                                         
004000     03 IDARTNR              PIC S9(9)           COMP-3.                  
004100*                                 ARTIKELNUMMER                           
004200*                                 PART NUMBER                             
004300     03 REKSIFFR             PIC S9              COMP-3.                  
004400*                                 KONTROLLSIFFRA                          
004500*                                 PART NO CHECK DIGIT                     
004600     03 IDDC                 PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800*                                 WAREHOUSE IDENTIFIER                    
004900     03 ARTIKEL-INFO.                                                     
005000*                                 ARTIKELSPECIFIKA UPPGIFTER FÖR          
005100*                                 LAGERBANDEN.                            
005200*                                 PART SPECIFIC INFORMATIONS FOR          
005300*                                 PART MASTERS.                           
005400        05 BEART-SVE         PIC X(25).                                   
005500*                                 SVENSK ARTIKELBENÄMNING                 
005600        05 BEART-ENG         PIC X(25).                                   
005700*                                 ENGELSK ARTIKELBENÄMNING                
005800        05 BEFT              PIC S9(3)           COMP-3.                  
005900*                                 FÖRPACKNINGSTYP                         
006000*                                 PACKAGING TYPE                          
006100        05 FLAVRART          PIC X.                                       
006200*                                 AVROPSARTIKEL                           
006300        05 FLERS             PIC X.                                       
006400*                                 TILLKOMMANDE ARTIKEL ?                  
006500        05 FLGEMART          PIC X.                                       
006600*                                 FLAGGA GEMENSAM ARTIKEL                 
006700*                                 COMMON PART FLAG                        
006800        05 FLIART            PIC X.                                       
006900*                                 ARTIKELN INGÅR I SATS                   
007000*                                 PART IN KIT                             
007100        05 FLJIT             PIC X.                                       
007200*                                 JUST-IN-TIME FLAGGA                     
007300*                                 JUST-IN-TIME FLAG                       
007400        05 FLLSRDEL          PIC X.                                       
007500*                                 LEVERERAS SOM RESDEL                    
007600        05 FLTPO1            PIC X.                                       
007700*                                 ARTIKELN GODKÄND FÖR TPO1               
007800*                                 TPO1 ALLOWED FOR ARTICLE                
007900        05 IDANSK            PIC S9(3)           COMP-3.                  
008000*                                 ANSKAFFARNUMMER                         
008100*                                 PROCURER NO.                            
008200        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
008300*                                 EMBALLAGEARTIKELNR FÖR Q0               
008400        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
008500*                                 EMBALLAGEARTIKELNR FÖR Q1               
008600        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
008700*                                 EMBALLAGEARTIKELNR FÖR Q2               
008800        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
008900*                                 EMBALLAGEARTIKELNR FÖR Q3               
009000        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
009100*                                 EMBALLAGEARTIKELNR FÖR Q4               
009200        05 IDBERED           PIC S9(3)           COMP-3.                  
009300*                                 BEREDARENUMMER                          
009400        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
009500*                                 FUNKTIONSGRUPP                          
009600*                                 FUNCTION GROUP                          
009700        05 IDINK             PIC X(4).                                    
009800*                                 INKÖPARNUMMER                           
009900*                                 PURCHASE IDENTIFICATION NUMBER          
010000        05 IDLEVNR           PIC X(5).                                    
010100*                                 LEVERANTÖRNUMMER                        
010200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010300        05 IDLKTO            PIC S9(7)           COMP-3.                  
010400*                                 LAGERKONTO (FFHHHUU)                    
010500*                                 STOCK ACCOUNT (CCMMMSS)                 
010600        05 IDPROJ            PIC X(4).                                    
010700*                                 PARTS PROJEKTIDENTITET                  
010800*                                 PARTS PROJECT IDENTITY                  
010900        05 IDPSN             PIC 9(3).                                    
011000*                                 PROPER SHIPPING NAME                    
011100*                                 PROPER SHIPPING NAME                    
011200        05 IDPSN-DC          PIC 9(3).                                    
011300*                                 PROPER SHIPPING NAME PER XDC            
011400*                                 PROPER SHIPPING NAME XDC                
011500        05 FILLER            PIC X(3).                                    
011600        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
011700*                                 STATISTISKT NUMMER                      
011800*                                 1 = NORSKT                              
011900*                                 2 = ENGELSKT                            
012000*                                 3 = BELGISKT                            
012100*                                 4 = PERUANSKT                           
012200*                                 5 = SVENSKT                             
012300*                                 6 =                                     
012400*                                 STATISTICAL NO.                         
012500        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
012600*                                 STATISTISKT NUMMER                      
012700*                                 1 = NORSKT                              
012800*                                 2 = ENGELSKT                            
012900*                                 3 = BELGISKT                            
013000*                                 4 = PERUANSKT                           
013100*                                 5 = SVENSKT                             
013200*                                 6 =                                     
013300*                                 STATISTICAL NO.                         
013400        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
013500*                                 STATISTISKT NUMMER                      
013600*                                 1 = NORSKT                              
013700*                                 2 = ENGELSKT                            
013800*                                 3 = BELGISKT                            
013900*                                 4 = PERUANSKT                           
014000*                                 5 = SVENSKT                             
014100*                                 6 =                                     
014200*                                 STATISTICAL NO.                         
014300        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
014400*                                 STATISTISKT NUMMER                      
014500*                                 1 = NORSKT                              
014600*                                 2 = ENGELSKT                            
014700*                                 3 = BELGISKT                            
014800*                                 4 = PERUANSKT                           
014900*                                 5 = SVENSKT                             
015000*                                 6 =                                     
015100*                                 STATISTICAL NO.                         
015200        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
015300*                                 STATISTISKT NUMMER                      
015400*                                 1 = NORSKT                              
015500*                                 2 = ENGELSKT                            
015600*                                 3 = BELGISKT                            
015700*                                 4 = PERUANSKT                           
015800*                                 5 = SVENSKT                             
015900*                                 6 =                                     
016000*                                 STATISTICAL NO.                         
016100        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
016200*                                 STATISTISKT NUMMER                      
016300*                                 1 = NORSKT                              
016400*                                 2 = ENGELSKT                            
016500*                                 3 = BELGISKT                            
016600*                                 4 = PERUANSKT                           
016700*                                 5 = SVENSKT                             
016800*                                 6 =                                     
016900*                                 STATISTICAL NO.                         
017000        05 IDKAT-1           PIC X(5).                                    
017100*                                 KATALOGBETECKNING                       
017200        05 IDKAT-2           PIC X(5).                                    
017300*                                 KATALOGBETECKNING                       
017400        05 IDKAT-3           PIC X(5).                                    
017500*                                 KATALOGBETECKNING                       
017600        05 KDAGE             PIC X.                                       
017700*                                 AGE-CODE                                
017800*                                 AGE-CODE                                
017900        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018000*                                 HANTERINGSKOD                           
018100*                                 HANDLING CODE                           
018200        05 KDARTURS          PIC X(2).                                    
018300*                                 ARTIKELURSPRUNGSKOD                     
018400*                                 COUNTRY OF ORIGIN                       
018500        05 KDBPSR            PIC S9              COMP-3.                  
018600*                                 BASLAGERFÖRSLAGSNIVÅ                    
018700*                                 BASIC PART STOCK RECOMMENDATION         
018800        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
018900*                                 EMBALLAGEKOD 0                          
019000        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019100*                                 EMBALLAGEKOD 1                          
019200        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
019300*                                 EMBALLAGEKOD 2                          
019400        05 KDERS             PIC S9(3)           COMP-3.                  
019500*                                 ERSÄTTNINGSKOD                          
019600*                                 SUPERSESSION CODE                       
019700        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
019800*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
019900*                                 OBSOLETION SUPERSESSION CODE            
020000        05 KDFARLIG          PIC S9              COMP-3.                  
020100*                                 KOD FÖR FARLIGT GODS                    
020200*                                 DANGEROUS GOODS CODE                    
020300        05 KDGK              PIC S9              COMP-3.                  
020400*                                 GODSMOTTAGAREKOD                        
020500*                                 GOODS RECEIVING WAREHOUSE CODE          
020600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020700*                                 PRODUKTSLAG                             
020800*                                 PRODUCT GROUP                           
020900        05 KDSORT            PIC X(2).                                    
021000*                                 SORT-KOD                                
021100*                                 UNIT OF MEASURE                         
021200        05 KDSPEEMB          PIC 9.                                       
021300*                                 SPECIALEMBALLAGEKOD                     
021400*                                 SPECIAL PACKING CODE                    
021500        05 KDSRA             PIC S9(3)           COMP-3.                  
021600*                                 SRA-KOD                                 
021700*                                 SRA CODE                                
021800        05 KDUART            PIC X.                                       
021900*                                 UNDANTAGSARTIKEL                        
022000*                                 EXECPTION PARTS                         
022100        05 KDVVKL            PIC S9              COMP-3.                  
022200*                                 VOLYMVÄRDESKLASS                        
022300*                                 VOLUME VALUE CLASS                      
022400        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022500*                                 YTBEHANDLINGSKOD                        
022600*                                                                         
022700        05 KVPALL            PIC S9(7)           COMP-3.                  
022800*                                 ANTAL I PALL                            
022900*                                 QUANTITY IN PALLET                      
023000        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023100*                                 ANTAL I Q0 FÖRPACKNING                  
023200        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
023300*                                 ANTAL I Q1 FÖRPACKNING                  
023400*                                 QUANTITY IN BULK PACK Q1                
023500        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023600*                                 ANTAL I Q2 FÖRPACKNING                  
023700*                                 QUANTITY IN BULK PACK Q2                
023800        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
023900*                                 ANTAL I Q3 FÖRPACKNING                  
024000*                                 QUANTITY IN BULK PACK Q3                
024100        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024200*                                 ANTAL I Q4 FÖRPACKNING                  
024300*                                 QUANTITY IN BULK PACK Q4                
024400        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024500*                                 BESTÄLLNINGSPRIS I KRONOR               
024600*                                 ORDER PRICE SWEDISH CURRENCY            
024700        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
024800*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
024900*                                 GROSS-PRICE EXPORT                      
025000*                                  (FOB-GROSS)                            
025100        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025200*                                 ARTIKELNS SJÄLVKOSTNAD                  
025300*                                 COST OF SALES                           
025400        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025500*                                 ARTIKELSTANDARDPRIS                     
025600*                                 STANDARD PRICE                          
025700        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
025800*                                 INKÖPSPRIS                              
025900*                                 PURCHASE PRICE                          
026000        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026200*                                 DATE OF SUPERSESSION (YYWWD)            
026300        05 TIFINLV           PIC S9(5)           COMP-3.                  
026400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026500*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026600        05 VKART             PIC S9(7)           COMP-3.                  
026700*                                 ARTIKELVIKT (G)                         
026800*                                 PART WEIGHT (G)                         
026900        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027000*                                 ARTIKELVOLYM (CM3)                      
027100*                                 PART VOLUME    (CM3)                    
027200        05 TISKROT           PIC S9(7)           COMP-3.                  
027300*                                 SKROTNINGSDATUM                         
027400*                                 DATE OF SCRAPPING                       
027500        05 FLCDART           PIC X.                                       
027600*                                 CROSS-DOCKING PART                      
027700*                                 CROSS-DOCKING PART                      
027800        05 ADLAGOMR-CD-1     PIC S9(3)           COMP-3.                  
027900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
028000*                                 AREA ADDRESS CROSS DOCKING WARE         
028100*                                 HOUSE                                   
028200        05 ADGANG-CD-1       PIC S9(3)           COMP-3.                  
028300*                                 GÅNG                                    
028400*                                 AISLE                                   
028500        05 ADPLATS-CD-1      PIC S9(5)           COMP-3.                  
028600*                                 LAGERPLATSNUMMER                        
028700*                                 LOCATION                                
028800        05 ADLAGOMR-CD-2     PIC S9(3)           COMP-3.                  
028900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
029000*                                 AREA ADDRESS CROSS DOCKING WARE         
029100*                                 HOUSE                                   
029200        05 ADGANG-CD-2       PIC S9(3)           COMP-3.                  
029300*                                 GÅNG                                    
029400*                                 AISLE                                   
029500        05 ADPLATS-CD-2      PIC S9(5)           COMP-3.                  
029600*                                 LAGERPLATSNUMMER                        
029700*                                 LOCATION                                
029800        05 ADLAGOMR-CD-3     PIC S9(3)           COMP-3.                  
029900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
030000*                                 AREA ADDRESS CROSS DOCKING WARE         
030100*                                 HOUSE                                   
030200        05 ADGANG-CD-3       PIC S9(3)           COMP-3.                  
030300*                                 GÅNG                                    
030400*                                 AISLE                                   
030500        05 ADPLATS-CD-3      PIC S9(5)           COMP-3.                  
030600*                                 LAGERPLATSNUMMER                        
030700*                                 LOCATION                                
030800        05 ADLAGOMR-CD-4     PIC S9(3)           COMP-3.                  
030900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
031000*                                 AREA ADDRESS CROSS DOCKING WARE         
031100*                                 HOUSE                                   
031200        05 ADGANG-CD-4       PIC S9(3)           COMP-3.                  
031300*                                 GÅNG                                    
031400*                                 AISLE                                   
031500        05 ADPLATS-CD-4      PIC S9(5)           COMP-3.                  
031600*                                 LAGERPLATSNUMMER                        
031700*                                 LOCATION                                
031800        05 KVLS-CD-1         PIC S9(7)           COMP-3.                  
031900*                                 LAGERSALDO CD                           
032000*                                 STOCK BALANCE CD                        
032100        05 KVLS-CD-2         PIC S9(7)           COMP-3.                  
032200*                                 LAGERSALDO CD                           
032300*                                 STOCK BALANCE CD                        
032400        05 KVLS-CD-3         PIC S9(7)           COMP-3.                  
032500*                                 LAGERSALDO CD                           
032600*                                 STOCK BALANCE CD                        
032700        05 KVLS-CD-4         PIC S9(7)           COMP-3.                  
032800*                                 LAGERSALDO CD                           
032900*                                 STOCK BALANCE CD                        
033000        05 KVRESS-CD-1       PIC S9(7)           COMP-3.                  
033100*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
033200*                                  DOCKING LAGER                          
033300*                                 QUANTITY RESERVED ITEMS IN CROS         
033400*                                 S DOCKING WAREHOUSE                     
033500        05 KVRESS-CD-2       PIC S9(7)           COMP-3.                  
033600*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
033700*                                  DOCKING LAGER                          
033800*                                 QUANTITY RESERVED ITEMS IN CROS         
033900*                                 S DOCKING WAREHOUSE                     
034000        05 KVRESS-CD-3       PIC S9(7)           COMP-3.                  
034100*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
034200*                                  DOCKING LAGER                          
034300*                                 QUANTITY RESERVED ITEMS IN CROS         
034400*                                 S DOCKING WAREHOUSE                     
034500        05 KVRESS-CD-4       PIC S9(7)           COMP-3.                  
034600*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
034700*                                  DOCKING LAGER                          
034800*                                 QUANTITY RESERVED ITEMS IN CROS         
034900*                                 S DOCKING WAREHOUSE                     
035000        05 FILLER            PIC X(11).                                   
035100     03 LAGERINFO.                                                        
035200*                                 LAGERSPECIFIKA UPPGIFTER FÖR            
035300*                                 LAGERBANDEN.                            
035400*                                                                         
035500*                                 WAREHOUSE SPECIFIC INFORMA-             
035600*                                 TIONS FOR PART MASTERS.                 
035700        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
035800*                                 LAGEROMRÅDE                             
035900*                                 AREA                                    
036000        05 ADGANG            PIC S9(3)           COMP-3.                  
036100*                                 GÅNG                                    
036200*                                 AISLE                                   
036300        05 ADPLATS           PIC S9(5)           COMP-3.                  
036400*                                 LAGERPLATSNUMMER                        
036500*                                 LOCATION                                
036600        05 KDOTFREK          PIC X.                                       
036700*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
036800*                                 ORDER HIT FREQUENCY FOR PART            
036900        05 FLMANAT           PIC X.                                       
037000*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
037100        05 FLMANBK           PIC X.                                       
037200*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
037300        05 FLMANGK           PIC X.                                       
037400*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
037500        05 FLMANKP           PIC X.                                       
037600*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
037700        05 FLMANLT           PIC X.                                       
037800*                                 MANUELLT SATT LEDTID ?                  
037900        05 FLMANOSK          PIC X.                                       
038000*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
038100*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
038200        05 FLMANQ            PIC X.                                       
038300*                                 MANUELL HEMTAGNINGSKVANTITET            
038400        05 FLMPB             PIC X.                                       
038500*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
038600        05 FLREFILL          PIC X.                                       
038700*                                 REFILLARTIKEL                           
038800*                                 REFILLPART                              
038900        05 FLSKROT-BEORD     PIC X.                                       
039000*                                 SKROTNING BEORDRAD AV ANSK              
039100*                                 SCRAPPING ORDERED BY PROCURER           
039200        05 FLTOPP            PIC X.                                       
039300*                                 TOPP-200-ARTIKEL                        
039400*                                 TOP 200 PART                            
039500        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
039600*                                 AVINUMMER SENASTE INLEVERANS            
039700        05 IDLEVNR-SEN       PIC X(5).                                    
039800*                                 SENASTE LEVERANTÖR                      
039900        05 IDPLANGR-AG       PIC S9              COMP-3.                  
040000*                                 PLANERINGSGRUPP ANSKAFFARE              
040100        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
040200*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
040300        05 IDPROENH-1        PIC X(8).                                    
040400*                                 PRODUKTIONSENHET                        
040500*                                 PRODUCTION UNIT                         
040600        05 IDPROENH-2        PIC X(8).                                    
040700*                                 PRODUKTIONSENHET                        
040800*                                 PRODUCTION UNIT                         
040900        05 IDPROENH-3        PIC X(8).                                    
041000*                                 PRODUKTIONSENHET                        
041100*                                 PRODUCTION UNIT                         
041200        05 IDPROJUP          PIC X(8).                                    
041300*                                 PROJEKTUPPDRAG                          
041400*                                 PROJECT ASSIGNMENT                      
041500        05 IDRITN            PIC X(10).                                   
041600*                                 RITNINGSNUMMER                          
041700*                                 DRAWING NUMBER                          
041800        05 KDAVT             PIC S9              COMP-3.                  
041900*                                 AVTALSMÄRKNING                          
042000*                                 AGREEMENT CODE                          
042100        05 KDFORP.                                                        
042200*                                 FÖRPACKNINGSKOD                         
042300*                                 PACKAGING CODE                          
042400           07 KDFORPPL       PIC 9.                                       
042500*                                 FÖRPACKNINGSPLATS                       
042600*                                 PREPACKING PLACE                        
042700           07 KDFORPGP       PIC 9(2).                                    
042800*                                 FÖRPACKNINGSGRUPP                       
042900*                                 PREPACKING GROUP                        
043000           07 KDFORPUF       PIC 9.                                       
043100*                                 UPPRÄKNINGSFAKTOR                       
043200*                                 ENUMERATION                             
043300        05 KDFREKKL          PIC X.                                       
043400*                                 FREKVENSKLASS                           
043500*                                 FREQ. CLASS                             
043600        05 KDHF              PIC S9              COMP-3.                  
043700*                                 HUVUDFÖRRÅDSMÄRKNING                    
043800*                                 CODE MAIN STORAGE                       
043900        05 KDKG              PIC S9              COMP-3.                  
044000*                                 KURANSGRUPP                             
044100*                                 TURNOVER CODE                           
044200        05 KDKSP             PIC S9              COMP-3.                  
044300*                                 KÖPSPÄRR                                
044400*                                 PURCHASE BLOCKING CODE                  
044500        05 KDLEVSP           PIC S9(3)           COMP-3.                  
044600*                                 SPÄRRKOD LEVERANS                       
044700*                                 DELIVERY BLOCKING CODE                  
044800        05 KDLPSP            PIC S9              COMP-3.                  
044900*                                 LEVERANSPLANESPÄRR                      
045000        05 KDPRISKL          PIC X.                                       
045100*                                 PRISKLASS                               
045200*                                 PRICE CLASS                             
045300        05 KDPSLLOC          PIC 9(2).                                    
045400*                                 PRODUKTSLAG LOKALT                      
045500*                                 PRODUCT GROUP LOCAL                     
045600        05 KDTIPPR           PIC S9              COMP-3.                  
045700*                                 TIPPAT PRIS KOD                         
045800*                                 ESTIMATED PRICE CODE                    
045900        05 KDVSOP            PIC S9(3)           COMP-3.                  
046000*                                 VSOP-KOD                                
046100*                                 VSOP-CODE                               
046200        05 KDVTH             PIC S9              COMP-3.                  
046300*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
046400*                                 CODE FOR COST RESPONSIBILITY            
046500        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
046600*                                 DEL AV AK SOM LIGGER I CDC              
046700*                                 PART OF AK IN THE CDC                   
046800        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
046900*                                 DEL AV AK PÅ VÄG                        
047000*                                 PART OF AK ON ITS WAY                   
047100        05 KVAKS-T           PIC S9(7)           COMP-3.                  
047200*                                 DEL AV AK I EN TERMINAL                 
047300*                                 PART OF AK IN A TERMINAL                
047400        05 KVAP              PIC S9(7)           COMP-3.                  
047500*                                 ANNULLATIONSPUNKT                       
047600        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
047700*                                 SENAST AVISERAT ANTAL                   
047800        05 KVBK              PIC S9(7)           COMP-3.                  
047900*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
048000        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
048100*                                 TOT BEST REST                           
048200        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
048300*                                 INLEVERANSTID     (ANTAL DAGAR)         
048400        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
048500*                                 DAGAR TULL- OCH TRANSPORT-TID           
048600        05 KVEFRS            PIC S9(7)           COMP-3.                  
048700*                                 EJ FAKTURERAT ANTAL STYCK               
048800*                                 ORDERED NOT INVOICED QTY                
048900        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
049000*                                 FRYSTID FÖR TPO-ORDER                   
049100*                                 FREEZTIME FOR TPO                       
049200        05 KVINVS            PIC S9(7)           COMP-3.                  
049300*                                 INVENTERINGSSALDO                       
049400*                                 STOCK-TAKING BALANCE                    
049500        05 KVKP              PIC S9(7)           COMP-3.                  
049600*                                 KÖPPUNKT                                
049700        05 KVLAAN            PIC S9(7)           COMP-3.                  
049800*                                 LÅNESALDO                               
049900        05 KVLS              PIC S9(7)           COMP-3.                  
050000*                                 LAGERSALDO                              
050100*                                 STOCK BALANCE                           
050200        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
050300*                                 SEPARAT PROGNOSFEL                      
050400        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
050500*                                 TOTALT PROGNOSFEL                       
050600        05 KVMP              PIC S9(7)           COMP-3.                  
050700*                                 MAXPUNKT                                
050800*                                 MAXIMUM POINT                           
050900        05 KVOKS             PIC S9(7)           COMP-3.                  
051000*                                 ORDERKÖSALDO                            
051100*                                 ORDER QUEUE BALANCE                     
051200        05 KVOVERF           PIC S9(7)           COMP-3.                  
051300*                                 ÖVERFÖRINGSSALDO                        
051400        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
051500*                                 SATS-PERIODBEHOV                        
051600*                                 KIT PERIOD REQUIREMENTS                 
051700        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
051800*                                 SEPARAT PERIODBEHOV                     
051900*                                 SEPARATE PERIOD REQUIREMENTS            
052000        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
052100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
052200*                                 PERIODICAL DEMAND TPO1 AND TPO2         
052300*                                                                         
052400        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
052500*                                 PREL-AVB KVANT, KLASS 2-4               
052600*                                 PREL-RES QUANT, CLASS 2-4               
052700        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
052800*                                 PREL-AVB KVANT, KLASS 1                 
052900*                                 PREL-RES QUANT, CLASS 1                 
053000        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
053100*                                 PREL-AVB KVANT, VOR                     
053200*                                 PREL-RES QUANT, VOR                     
053300        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
053400*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
053500*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
053600        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
053700*                                 PRELIMINÄR RO-KVANT, KLASS 1            
053800*                                 PRELIMINARY BO-QUANT, CLASS 1           
053900        05 KVQ               PIC S9(7)           COMP-3.                  
054000*                                 EKONOMISK HEMTAGNINGSKVANTITET          
054100        05 KVRESS            PIC S9(7)           COMP-3.                  
054200*                                 RESERVERAT ANTAL ARTIKLAR               
054300*                                 QUANTITY RESERVED ITEMS                 
054400        05 KVROS             PIC S9(7)           COMP-3.                  
054500*                                 RESTORDERSALDO                          
054600*                                 BACKORDER QTY                           
054700        05 KVSLAGER          PIC S9(7)           COMP-3.                  
054800*                                 SÄKERHETSLAGER                          
054900*                                 SAFETY STOCK                            
055000        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
055100*                                 SLUTKÖPSSALDO                           
055200        05 KVSPANT           PIC S9(7)           COMP-3.                  
055300*                                 SPÄRRAT ANTAL                           
055400*                                 BLOCKED QTY                             
055500        05 KVUTRS            PIC S9(7)           COMP-3.                  
055600*                                 UTREDNINGSSALDO                         
055700*                                 INVESTIGATION BALANCE                   
055800        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
055900*                                 ANTAL VECKOR ANSKAFFNINGSTID            
056000        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
056100*                                 ANTAL VECKOR BESTÄLLNINGSTID            
056200        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
056300*                                 ANTAL VECKOR FRYSNINGSTID               
056400        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
056500*                                 ANTAL VECKOR LEDTID                     
056600        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
056700*                                 DIREKT LÖN                              
056800*                                 SURCHARGE COSTS                         
056900        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
057000*                                 DIREKT MATERIAL                         
057100*                                 SURCHARGE PACKING MATERIAL              
057200        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
057300*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
057400*                                 SUPPLIERS PACKING AND HANDLING          
057500        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
057600*                                 ORDERSÄRKOSTNAD                         
057700*                                 REMAINING OVERHEAD SURCHARGE            
057800        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
057900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
058000*                                 REMAINING OVERHEAD SURCHARGE            
058100        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
058200*                                 DIREKTLEVERANSANDEL                     
058300        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
058400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
058500*                                 ADJUSTMENT ALGORITM                     
058600        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
058700*                                 TPO-KVANTITET, TOTAL                    
058800*                                 TPO-QUANTITY, TOTAL                     
058900        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
059000*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
059100        05 TIINVDAT          PIC S9(5)           COMP-3.                  
059200*                                 INVENTERINGSDATUM                       
059300*                                 STOCKTAKING DATE                        
059400        05 TILPSP            PIC S9(5)           COMP-3.                  
059500*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
059600        05 TIREGDAT          PIC S9(7)           COMP-3.                  
059700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
059800*                                 REGISTRATION DATE (YYMMDD)              
059900        05 TISLJUST          PIC S9(5)           COMP-3.                  
060000*                                 VECKA DÅ JUSTERING AV SÄKER-            
060100*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
060200        05 TIURPROD          PIC S9(5)           COMP-3.                  
060300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
060400*                                 OUT OF PRODUCTION DATE (YYWW)           
060500     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
060600*                                 PERSONKOD REFILLANSVARIG                
060700*                                 REFILL RESPONSIBLE ID                   
060800     03 KVPB-REF-US          PIC S9(6)V9(1)      COMP-3.                  
060900*                                 PERIODBEHOV REFILLING USA               
061000*                                 FORECAST REFILLING US                   
061100     03 KVPB-NDC-TOT         PIC S9(6)V9(1)      COMP-3.                  
061200*                                 PB-NDC TOTALT FÖR SAMLTLIGA NDC         
061300*                                 :ER                                     
061400     03 KVPB-SDC-TOT         PIC S9(6)V9(1)      COMP-3.                  
061500*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
061600*                                 :ER                                     
061700     03 FILLER               PIC X(8).                                    
061800*** END OF VILMAII-COPY LENGTH= 639 BYTES                                 
