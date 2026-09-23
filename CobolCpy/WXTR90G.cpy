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
009700        05 IDINK             PIC S9(3)           COMP-3.                  
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
011200        05 IDPSN-US          PIC 9(3).                                    
011300*                                 PROPER SHIPPING NAME USA                
011400*                                 PROPER SHIPPING NAME USA                
011500        05 IDPSN-CA          PIC 9(3).                                    
011600*                                 PROPER SHIPPING NAME KANADA             
011700*                                 PROPER SHIPPING NAME CANADA             
011800        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
011900*                                 STATISTISKT NUMMER                      
012000*                                 1 = NORSKT                              
012100*                                 2 = ENGELSKT                            
012200*                                 3 = BELGISKT                            
012300*                                 4 = PERUANSKT                           
012400*                                 5 = SVENSKT                             
012500*                                 6 =                                     
012600*                                 STATISTICAL NO.                         
012700        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
012800*                                 STATISTISKT NUMMER                      
012900*                                 1 = NORSKT                              
013000*                                 2 = ENGELSKT                            
013100*                                 3 = BELGISKT                            
013200*                                 4 = PERUANSKT                           
013300*                                 5 = SVENSKT                             
013400*                                 6 =                                     
013500*                                 STATISTICAL NO.                         
013600        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
013700*                                 STATISTISKT NUMMER                      
013800*                                 1 = NORSKT                              
013900*                                 2 = ENGELSKT                            
014000*                                 3 = BELGISKT                            
014100*                                 4 = PERUANSKT                           
014200*                                 5 = SVENSKT                             
014300*                                 6 =                                     
014400*                                 STATISTICAL NO.                         
014500        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
014600*                                 STATISTISKT NUMMER                      
014700*                                 1 = NORSKT                              
014800*                                 2 = ENGELSKT                            
014900*                                 3 = BELGISKT                            
015000*                                 4 = PERUANSKT                           
015100*                                 5 = SVENSKT                             
015200*                                 6 =                                     
015300*                                 STATISTICAL NO.                         
015400        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
015500*                                 STATISTISKT NUMMER                      
015600*                                 1 = NORSKT                              
015700*                                 2 = ENGELSKT                            
015800*                                 3 = BELGISKT                            
015900*                                 4 = PERUANSKT                           
016000*                                 5 = SVENSKT                             
016100*                                 6 =                                     
016200*                                 STATISTICAL NO.                         
016300        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
016400*                                 STATISTISKT NUMMER                      
016500*                                 1 = NORSKT                              
016600*                                 2 = ENGELSKT                            
016700*                                 3 = BELGISKT                            
016800*                                 4 = PERUANSKT                           
016900*                                 5 = SVENSKT                             
017000*                                 6 =                                     
017100*                                 STATISTICAL NO.                         
017200        05 IDKAT-1           PIC X(5).                                    
017300*                                 KATALOGBETECKNING                       
017400        05 IDKAT-2           PIC X(5).                                    
017500*                                 KATALOGBETECKNING                       
017600        05 IDKAT-3           PIC X(5).                                    
017700*                                 KATALOGBETECKNING                       
017800        05 KDAGE             PIC X.                                       
017900*                                 AGE-CODE                                
018000*                                 AGE-CODE                                
018100        05 KDARTHNT          PIC S9(7)           COMP-3.                  
018200*                                 HANTERINGSKOD                           
018300*                                 HANDLING CODE                           
018400        05 KDARTURS          PIC X(2).                                    
018500*                                 ARTIKELURSPRUNGSKOD                     
018600*                                 COUNTRY OF ORIGIN                       
018700        05 KDBPSR            PIC S9              COMP-3.                  
018800*                                 BASLAGERFÖRSLAGSNIVÅ                    
018900*                                 BASIC PART STOCK RECOMMENDATION         
019000        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
019100*                                 EMBALLAGEKOD 0                          
019200        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
019300*                                 EMBALLAGEKOD 1                          
019400        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
019500*                                 EMBALLAGEKOD 2                          
019600        05 KDERS             PIC S9(3)           COMP-3.                  
019700*                                 ERSÄTTNINGSKOD                          
019800*                                 SUPERSESSION CODE                       
019900        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
020000*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
020100*                                 OBSOLETION SUPERSESSION CODE            
020200        05 KDFARLIG          PIC S9              COMP-3.                  
020300*                                 KOD FÖR FARLIGT GODS                    
020400*                                 DANGEROUS GOODS CODE                    
020500        05 KDGK              PIC S9              COMP-3.                  
020600*                                 GODSMOTTAGAREKOD                        
020700*                                 GOODS RECEIVING WAREHOUSE CODE          
020800        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020900*                                 PRODUKTSLAG                             
021000*                                 PRODUCT GROUP                           
021100        05 KDSORT            PIC X(2).                                    
021200*                                 SORT-KOD                                
021300*                                 UNIT OF MEASURE                         
021400        05 KDSPEEMB          PIC 9.                                       
021500*                                 SPECIALEMBALLAGEKOD                     
021600*                                 SPECIAL PACKING CODE                    
021700        05 KDSRA             PIC S9(3)           COMP-3.                  
021800*                                 SRA-KOD                                 
021900*                                 SRA CODE                                
022000        05 KDUART            PIC X.                                       
022100*                                 UNDANTAGSARTIKEL                        
022200*                                 EXECPTION PARTS                         
022300        05 KDVVKL            PIC S9              COMP-3.                  
022400*                                 VOLYMVÄRDESKLASS                        
022500*                                 VOLUME VALUE CLASS                      
022600        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022700*                                 YTBEHANDLINGSKOD                        
022800*                                                                         
022900        05 KVPALL            PIC S9(7)           COMP-3.                  
023000*                                 ANTAL I PALL                            
023100*                                 QUANTITY IN PALLET                      
023200        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
023300*                                 ANTAL I Q0 FÖRPACKNING                  
023400        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
023500*                                 ANTAL I Q1 FÖRPACKNING                  
023600*                                 QUANTITY IN BULK PACK Q1                
023700        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023800*                                 ANTAL I Q2 FÖRPACKNING                  
023900*                                 QUANTITY IN BULK PACK Q2                
024000        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
024100*                                 ANTAL I Q3 FÖRPACKNING                  
024200*                                 QUANTITY IN BULK PACK Q3                
024300        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024400*                                 ANTAL I Q4 FÖRPACKNING                  
024500*                                 QUANTITY IN BULK PACK Q4                
024600        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024700*                                 BESTÄLLNINGSPRIS I KRONOR               
024800*                                 ORDER PRICE SWEDISH CURRENCY            
024900        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
025000*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
025100*                                 GROSS-PRICE EXPORT                      
025200*                                  (FOB-GROSS)                            
025300        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025400*                                 ARTIKELNS SJÄLVKOSTNAD                  
025500*                                 COST OF SALES                           
025600        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025700*                                 ARTIKELSTANDARDPRIS                     
025800*                                 STANDARD PRICE                          
025900        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
026000*                                 INKÖPSPRIS                              
026100*                                 PURCHASE PRICE                          
026200        05 TIERSDAT          PIC S9(5)           COMP-3.                  
026300*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026400*                                 DATE OF SUPERSESSION (YYWWD)            
026500        05 TIFINLV           PIC S9(5)           COMP-3.                  
026600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026700*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026800        05 VKART             PIC S9(7)           COMP-3.                  
026900*                                 ARTIKELVIKT (G)                         
027000*                                 PART WEIGHT (G)                         
027100        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
027200*                                 ARTIKELVOLYM NETTO (CM3)                
027300*                                 PART NET VOLUME    (CM3)                
027400        05 TISKROT           PIC S9(7)           COMP-3.                  
027500*                                 SKROTNINGSDATUM                         
027600*                                 DATE OF SCRAPPING                       
027700        05 FLCDART           PIC X.                                       
027800*                                 CROSS-DOCKING PART                      
027900*                                 CROSS-DOCKING PART                      
028000        05 ADLAGOMR-CD-1     PIC S9(3)           COMP-3.                  
028100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
028200*                                 AREA ADDRESS CROSS DOCKING WARE         
028300*                                 HOUSE                                   
028400        05 ADGANG-CD-1       PIC S9(3)           COMP-3.                  
028500*                                 GÅNG                                    
028600*                                 AISLE                                   
028700        05 ADPLATS-CD-1      PIC S9(5)           COMP-3.                  
028800*                                 LAGERPLATSNUMMER                        
028900*                                 LOCATION                                
029000        05 ADLAGOMR-CD-2     PIC S9(3)           COMP-3.                  
029100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
029200*                                 AREA ADDRESS CROSS DOCKING WARE         
029300*                                 HOUSE                                   
029400        05 ADGANG-CD-2       PIC S9(3)           COMP-3.                  
029500*                                 GÅNG                                    
029600*                                 AISLE                                   
029700        05 ADPLATS-CD-2      PIC S9(5)           COMP-3.                  
029800*                                 LAGERPLATSNUMMER                        
029900*                                 LOCATION                                
030000        05 ADLAGOMR-CD-3     PIC S9(3)           COMP-3.                  
030100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
030200*                                 AREA ADDRESS CROSS DOCKING WARE         
030300*                                 HOUSE                                   
030400        05 ADGANG-CD-3       PIC S9(3)           COMP-3.                  
030500*                                 GÅNG                                    
030600*                                 AISLE                                   
030700        05 ADPLATS-CD-3      PIC S9(5)           COMP-3.                  
030800*                                 LAGERPLATSNUMMER                        
030900*                                 LOCATION                                
031000        05 ADLAGOMR-CD-4     PIC S9(3)           COMP-3.                  
031100*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
031200*                                 AREA ADDRESS CROSS DOCKING WARE         
031300*                                 HOUSE                                   
031400        05 ADGANG-CD-4       PIC S9(3)           COMP-3.                  
031500*                                 GÅNG                                    
031600*                                 AISLE                                   
031700        05 ADPLATS-CD-4      PIC S9(5)           COMP-3.                  
031800*                                 LAGERPLATSNUMMER                        
031900*                                 LOCATION                                
032000        05 KVLS-CD-1         PIC S9(7)           COMP-3.                  
032100*                                 LAGERSALDO CD                           
032200*                                 STOCK BALANCE CD                        
032300        05 KVLS-CD-2         PIC S9(7)           COMP-3.                  
032400*                                 LAGERSALDO CD                           
032500*                                 STOCK BALANCE CD                        
032600        05 KVLS-CD-3         PIC S9(7)           COMP-3.                  
032700*                                 LAGERSALDO CD                           
032800*                                 STOCK BALANCE CD                        
032900        05 KVLS-CD-4         PIC S9(7)           COMP-3.                  
033000*                                 LAGERSALDO CD                           
033100*                                 STOCK BALANCE CD                        
033200        05 KVRESS-CD-1       PIC S9(7)           COMP-3.                  
033300*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
033400*                                  DOCKING LAGER                          
033500*                                 QUANTITY RESERVED ITEMS IN CROS         
033600*                                 S DOCKING WAREHOUSE                     
033700        05 KVRESS-CD-2       PIC S9(7)           COMP-3.                  
033800*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
033900*                                  DOCKING LAGER                          
034000*                                 QUANTITY RESERVED ITEMS IN CROS         
034100*                                 S DOCKING WAREHOUSE                     
034200        05 KVRESS-CD-3       PIC S9(7)           COMP-3.                  
034300*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
034400*                                  DOCKING LAGER                          
034500*                                 QUANTITY RESERVED ITEMS IN CROS         
034600*                                 S DOCKING WAREHOUSE                     
034700        05 KVRESS-CD-4       PIC S9(7)           COMP-3.                  
034800*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
034900*                                  DOCKING LAGER                          
035000*                                 QUANTITY RESERVED ITEMS IN CROS         
035100*                                 S DOCKING WAREHOUSE                     
035200     03 LAGERINFO.                                                        
035300*                                 LAGERSPECIFIKA UPPGIFTER FÖR            
035400*                                 LAGERBANDEN.                            
035500*                                                                         
035600*                                 WAREHOUSE SPECIFIC INFORMA-             
035700*                                 TIONS FOR PART MASTERS.                 
035800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
035900*                                 LAGEROMRÅDE                             
036000*                                 AREA                                    
036100        05 ADGANG            PIC S9(3)           COMP-3.                  
036200*                                 GÅNG                                    
036300*                                 AISLE                                   
036400        05 ADPLATS           PIC S9(5)           COMP-3.                  
036500*                                 LAGERPLATSNUMMER                        
036600*                                 LOCATION                                
036700        05 FLMANAT           PIC X.                                       
036800*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
036900        05 FLMANBK           PIC X.                                       
037000*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
037100        05 FLMANGK           PIC X.                                       
037200*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
037300        05 FLMANKP           PIC X.                                       
037400*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
037500        05 FLMANLT           PIC X.                                       
037600*                                 MANUELLT SATT LEDTID ?                  
037700        05 FLMANOSK          PIC X.                                       
037800*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
037900*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
038000        05 FLMANQ            PIC X.                                       
038100*                                 MANUELL HEMTAGNINGSKVANTITET            
038200        05 FLMPB             PIC X.                                       
038300*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
038400        05 FLREFILL          PIC X.                                       
038500*                                 REFILLARTIKEL                           
038600*                                 REFILLPART                              
038700        05 FLSKROT-BEORD     PIC X.                                       
038800*                                 SKROTNING BEORDRAD AV ANSK              
038900*                                 SCRAPPING ORDERED BY PROCURER           
039000        05 FLTOPP            PIC X.                                       
039100*                                 TOPP-200-ARTIKEL                        
039200*                                 TOP 200 PART                            
039300        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
039400*                                 AVINUMMER SENASTE INLEVERANS            
039500        05 IDLEVNR-SEN       PIC X(5).                                    
039600*                                 SENASTE LEVERANTÖR                      
039700        05 IDPLANGR-AG       PIC S9              COMP-3.                  
039800*                                 PLANERINGSGRUPP ANSKAFFARE              
039900        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
040000*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
040100        05 IDPROENH-1        PIC X(8).                                    
040200*                                 PRODUKTIONSENHET                        
040300*                                 PRODUCTION UNIT                         
040400        05 IDPROENH-2        PIC X(8).                                    
040500*                                 PRODUKTIONSENHET                        
040600*                                 PRODUCTION UNIT                         
040700        05 IDPROENH-3        PIC X(8).                                    
040800*                                 PRODUKTIONSENHET                        
040900*                                 PRODUCTION UNIT                         
041000        05 IDPROJUP          PIC X(8).                                    
041100*                                 PROJEKTUPPDRAG                          
041200*                                 PROJECT ASSIGNMENT                      
041300        05 IDRITN            PIC X(10).                                   
041400*                                 RITNINGSNUMMER                          
041500*                                 DRAWING NUMBER                          
041600        05 KDAVT             PIC S9              COMP-3.                  
041700*                                 AVTALSMÄRKNING                          
041800*                                 AGREEMENT CODE                          
041900        05 KDFORP.                                                        
042000*                                 FÖRPACKNINGSKOD                         
042100*                                 PACKAGING CODE                          
042200           07 KDFORPPL       PIC 9.                                       
042300*                                 FÖRPACKNINGSPLATS                       
042400*                                 PREPACKING PLACE                        
042500           07 KDFORPGP       PIC 9(2).                                    
042600*                                 FÖRPACKNINGSGRUPP                       
042700*                                 PREPACKING GROUP                        
042800           07 KDFORPUF       PIC 9.                                       
042900*                                 UPPRÄKNINGSFAKTOR                       
043000*                                 ENUMERATION                             
043100        05 KDFREKKL          PIC X.                                       
043200*                                 FREKVENSKLASS                           
043300*                                 FREQ. CLASS                             
043400        05 KDHF              PIC S9              COMP-3.                  
043500*                                 HUVUDFÖRRÅDSMÄRKNING                    
043600*                                 CODE MAIN STORAGE                       
043700        05 KDKG              PIC S9              COMP-3.                  
043800*                                 KURANSGRUPP                             
043900*                                 TURNOVER CODE                           
044000        05 KDKSP             PIC S9              COMP-3.                  
044100*                                 KÖPSPÄRR                                
044200*                                 PURCHASE BLOCKING CODE                  
044300        05 KDLEVSP           PIC S9(3)           COMP-3.                  
044400*                                 SPÄRRKOD LEVERANS                       
044500*                                 DELIVERY BLOCKING CODE                  
044600        05 KDLPSP            PIC S9              COMP-3.                  
044700*                                 LEVERANSPLANESPÄRR                      
044800        05 KDPRISKL          PIC X.                                       
044900*                                 PRISKLASS                               
045000*                                 PRICE CLASS                             
045100        05 KDPSLLOC          PIC 9(2).                                    
045200*                                 PRODUKTSLAG LOKALT                      
045300*                                 PRODUCT GROUP LOCAL                     
045400        05 KDTIPPR           PIC S9              COMP-3.                  
045500*                                 TIPPAT PRIS KOD                         
045600*                                 ESTIMATED PRICE CODE                    
045700        05 KDVSOP            PIC S9(3)           COMP-3.                  
045800*                                 VSOP-KOD                                
045900*                                 VSOP-CODE                               
046000        05 KDVTH             PIC S9              COMP-3.                  
046100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
046200*                                 CODE FOR COST RESPONSIBILITY            
046300        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
046400*                                 DEL AV AK SOM LIGGER I CDC              
046500*                                 PART OF AK IN THE CDC                   
046600        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
046700*                                 DEL AV AK PÅ VÄG                        
046800*                                 PART OF AK ON ITS WAY                   
046900        05 KVAKS-T           PIC S9(7)           COMP-3.                  
047000*                                 DEL AV AK I EN TERMINAL                 
047100*                                 PART OF AK IN A TERMINAL                
047200        05 KVAP              PIC S9(7)           COMP-3.                  
047300*                                 ANNULLATIONSPUNKT                       
047400        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
047500*                                 SENAST AVISERAT ANTAL                   
047600        05 KVBK              PIC S9(7)           COMP-3.                  
047700*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
047800        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
047900*                                 TOT BEST REST                           
048000        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
048100*                                 INLEVERANSTID     (ANTAL DAGAR)         
048200        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
048300*                                 DAGAR TULL- OCH TRANSPORT-TID           
048400        05 KVEFRS            PIC S9(7)           COMP-3.                  
048500*                                 EJ FAKTURERAT ANTAL STYCK               
048600*                                 ORDERED NOT INVOICED QTY                
048700        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
048800*                                 FRYSTID FÖR TPO-ORDER                   
048900*                                 FREEZTIME FOR TPO                       
049000        05 KVINVS            PIC S9(7)           COMP-3.                  
049100*                                 INVENTERINGSSALDO                       
049200*                                 STOCK-TAKING BALANCE                    
049300        05 KVKP              PIC S9(7)           COMP-3.                  
049400*                                 KÖPPUNKT                                
049500        05 KVLAAN            PIC S9(7)           COMP-3.                  
049600*                                 LÅNESALDO                               
049700        05 KVLS              PIC S9(7)           COMP-3.                  
049800*                                 LAGERSALDO                              
049900*                                 STOCK BALANCE                           
050000        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
050100*                                 SEPARAT PROGNOSFEL                      
050200        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
050300*                                 TOTALT PROGNOSFEL                       
050400        05 KVMP              PIC S9(7)           COMP-3.                  
050500*                                 MAXPUNKT                                
050600*                                 MAXIMUM POINT                           
050700        05 KVOKS             PIC S9(7)           COMP-3.                  
050800*                                 ORDERKÖSALDO                            
050900*                                 ORDER QUEUE BALANCE                     
051000        05 KVOVERF           PIC S9(7)           COMP-3.                  
051100*                                 ÖVERFÖRINGSSALDO                        
051200        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
051300*                                 SATS-PERIODBEHOV                        
051400*                                 KIT PERIOD REQUIREMENTS                 
051500        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
051600*                                 SEPARAT PERIODBEHOV                     
051700*                                 SEPARATE PERIOD REQUIREMENTS            
051800        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
051900*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
052000*                                 PERIODICAL DEMAND TPO1 AND TPO2         
052100*                                                                         
052200        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
052300*                                 PREL-AVB KVANT, KLASS 2-4               
052400*                                 PREL-RES QUANT, CLASS 2-4               
052500        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
052600*                                 PREL-AVB KVANT, KLASS 1                 
052700*                                 PREL-RES QUANT, CLASS 1                 
052800        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
052900*                                 PREL-AVB KVANT, VOR                     
053000*                                 PREL-RES QUANT, VOR                     
053100        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
053200*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
053300*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
053400        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
053500*                                 PRELIMINÄR RO-KVANT, KLASS 1            
053600*                                 PRELIMINARY BO-QUANT, CLASS 1           
053700        05 KVQ               PIC S9(7)           COMP-3.                  
053800*                                 EKONOMISK HEMTAGNINGSKVANTITET          
053900        05 KVRESS            PIC S9(7)           COMP-3.                  
054000*                                 RESERVERAT ANTAL ARTIKLAR               
054100*                                 QUANTITY RESERVED ITEMS                 
054200        05 KVROS             PIC S9(7)           COMP-3.                  
054300*                                 RESTORDERSALDO                          
054400*                                 BACKORDER QTY                           
054500        05 KVSLAGER          PIC S9(7)           COMP-3.                  
054600*                                 SÄKERHETSLAGER                          
054700*                                 SAFETY STOCK                            
054800        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
054900*                                 SLUTKÖPSSALDO                           
055000        05 KVSPANT           PIC S9(7)           COMP-3.                  
055100*                                 SPÄRRAT ANTAL                           
055200*                                 BLOCKED QTY                             
055300        05 KVUTRS            PIC S9(7)           COMP-3.                  
055400*                                 UTREDNINGSSALDO                         
055500*                                 INVESTIG.BALANCE                        
055600        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
055700*                                 ANTAL VECKOR ANSKAFFNINGSTID            
055800        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
055900*                                 ANTAL VECKOR BESTÄLLNINGSTID            
056000        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
056100*                                 ANTAL VECKOR FRYSNINGSTID               
056200        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
056300*                                 ANTAL VECKOR LEDTID                     
056400        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
056500*                                 DIREKT LÖN                              
056600*                                 SURCHARGE COSTS                         
056700        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
056800*                                 DIREKT MATERIAL                         
056900*                                 SURCHARGE PACKING MATERIAL              
057000        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
057100*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
057200*                                 SUPPLIERS PACKING AND HANDLING          
057300        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
057400*                                 ORDERSÄRKOSTNAD                         
057500*                                 REMAINING OVERHEAD SURCHARGE            
057600        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
057700*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
057800*                                 REMAINING OVERHEAD SURCHARGE            
057900        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
058000*                                 DIREKTLEVERANSANDEL                     
058100        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
058200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
058300*                                 ADJUSTMENT ALGORITM                     
058400        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
058500*                                 TPO-KVANTITET, TOTAL                    
058600*                                 TPO-QUANTITY, TOTAL                     
058700        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
058800*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
058900        05 TIINVDAT          PIC S9(5)           COMP-3.                  
059000*                                 INVENTERINGSDATUM                       
059100*                                 STOCKTAKING DATE                        
059200        05 TILPSP            PIC S9(5)           COMP-3.                  
059300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
059400        05 TIREGDAT          PIC S9(7)           COMP-3.                  
059500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
059600*                                 REGISTRATION DATE (YYMMDD)              
059700        05 TISLJUST          PIC S9(5)           COMP-3.                  
059800*                                 VECKA DÅ JUSTERING AV SÄKER-            
059900*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
060000        05 TIURPROD          PIC S9(5)           COMP-3.                  
060100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
060200     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
060300*                                 PERSONKOD REFILLANSVARIG                
060400*                                 REFILL RESPONSIBLE ID                   
060500     03 KVPB-REF-US          PIC S9(6)V9(1)      COMP-3.                  
060600*                                 PERIODBEHOV REFILLING USA               
060700*                                 FORECAST REFILLING US                   
060800     03 KVPB-NDC-TOT         PIC S9(6)V9(1)      COMP-3.                  
060900*                                 PB-NDC TOTALT FÖR SAMLTLIGA NDC         
061000*                                 :ER                                     
061100     03 KVPB-SDC-TOT         PIC S9(6)V9(1)      COMP-3.                  
061200*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
061300*                                 :ER                                     
061400     03 DAPUBL-US            PIC 9(8).                                    
061500*                                 PUBLICERINGSDATUM PER ART/DC US         
061600*                                 A                                       
061700*                                 DATE OF PUBLISHING PER PART/DC          
061800*                                 US                                      
061900*** END OF VILMAII-COPY LENGTH= 625 BYTES                                 
