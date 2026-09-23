000100 01  WCDCPART.                                                            
000200*                                 PRIMÄREXTRAKT                           
000300*                                                                         
000400*                                 LAGERBAND MED CDC INFORMATION           
000500*                                 COPY-TEXT FÖR EXTRAKT WCDCDAY           
000600*                                                       WCDCWEEK          
000700*                                                       WCDCACC           
000800*                                                       WCDCPLAN          
000900*                                                       WCDCYEAR          
001000*                                                                         
001100*                                 FRAMSTÄLLS VARJE :                      
001200*                                            MORGON (DAGLIGEN)            
001300*                                            VECKOSLUT                    
001400*                                            REDOVISNINGSPERIOD           
001500*                                            PLANERINGSPERIOD             
001600*                                            ÅRS-SLUT                     
001700*                                                                         
001800*                                 INNEHÅLLER CDC INFORMATION              
001900*                                 1 POST/ARTIKELNUMMER                    
002000*                                                                         
002100*                                                                         
002200*                                 PRIMARY EXTRACT                         
002300*                                                                         
002400*                                 PART-INFORMATION-FILE FOR CDC           
002500*                                 COPY-TEXT FOR EXTRACT WCDCDAY           
002600*                                                       WCDCWEEK          
002700*                                                       WCDCACC           
002800*                                                       WCDCPLAN          
002900*                                                       WCDCYEAR          
003000*                                                                         
003100*                                 CREATED EVERY :                         
003200*                                               MORNING (DAILY)           
003300*                                               WEEKEND                   
003400*                                               ACCOUNTPERIOD             
003500*                                               PLANNINGPERIOD            
003600*                                               YEAR                      
003700*                                                                         
003800*                                 CONTAINS CDC INFORMATION.               
003900*                                 1 RECORD/PARTNUMBER                     
004000*                                                                         
004100*                                                                         
004200     03 IDARTNR              PIC S9(9)           COMP-3.                  
004300*                                 ARTIKELNUMMER                           
004400*                                 PART NUMBER                             
004500     03 REKSIFFR             PIC S9              COMP-3.                  
004600*                                 KONTROLLSIFFRA                          
004700*                                 PART NO CHECK DIGIT                     
004800     03 IDDC                 PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000*                                 WAREHOUSE IDENTIFIER                    
005100     03 ARTIKEL-INFO.                                                     
005200*                                 ARTIKELUPPGIFTER                        
005300*                                                                         
005400*                                 PART INFORMATION                        
005500        05 BEART-SVE         PIC X(25).                                   
005600*                                 SVENSK ARTIKELBENÄMNING                 
005700        05 BEART-ENG         PIC X(25).                                   
005800*                                 ENGELSK ARTIKELBENÄMNING                
005900        05 BEFT              PIC S9(3)           COMP-3.                  
006000*                                 FÖRPACKNINGSTYP                         
006100*                                 PACKAGING TYPE                          
006200        05 FLAVRART          PIC X.                                       
006300*                                 AVROPSARTIKEL                           
006400        05 FLERS             PIC X.                                       
006500*                                 TILLKOMMANDE ARTIKEL ?                  
006600        05 FLGEMART          PIC X.                                       
006700*                                 FLAGGA GEMENSAM ARTIKEL                 
006800*                                 COMMON PART FLAG                        
006900        05 FLIART            PIC X.                                       
007000*                                 ARTIKELN INGÅR I SATS                   
007100*                                 PART IN KIT                             
007200        05 FLJIT             PIC X.                                       
007300*                                 JUST-IN-TIME FLAGGA                     
007400*                                 JUST-IN-TIME FLAG                       
007500        05 FLLSRDEL          PIC X.                                       
007600*                                 LEVERERAS SOM RESDEL                    
007700        05 FLTPO1            PIC X.                                       
007800*                                 ARTIKELN GODKÄND FÖR TPO1               
007900*                                 TPO1 ALLOWED FOR ARTICLE                
008000        05 IDANSK            PIC S9(3)           COMP-3.                  
008100*                                 ANSKAFFARNUMMER                         
008200*                                 PROCURER NO.                            
008300        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
008400*                                 EMBALLAGEARTIKELNR FÖR Q0               
008500        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
008600*                                 EMBALLAGEARTIKELNR FÖR Q1               
008700        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
008800*                                 EMBALLAGEARTIKELNR FÖR Q2               
008900        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
009000*                                 EMBALLAGEARTIKELNR FÖR Q3               
009100        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
009200*                                 EMBALLAGEARTIKELNR FÖR Q4               
009300        05 IDBERED           PIC S9(3)           COMP-3.                  
009400*                                 BEREDARENUMMER                          
009500        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
009600*                                 FUNKTIONSGRUPP                          
009700*                                 FUNCTION GROUP                          
009800        05 IDINK             PIC S9(3)           COMP-3.                  
009900*                                 INKÖPARNUMMER                           
010000*                                 PURCHASE IDENTIFICATION NUMBER          
010100        05 IDLEVNR           PIC X(5).                                    
010200*                                 LEVERANTÖRNUMMER                        
010300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010400        05 IDLKTO            PIC S9(7)           COMP-3.                  
010500*                                 LAGERKONTO (FFHHHUU)                    
010600*                                 STOCK ACCOUNT (CCMMMSS)                 
010700        05 IDPROJ            PIC X(4).                                    
010800*                                 PARTS PROJEKTIDENTITET                  
010900*                                 PARTS PROJECT IDENTITY                  
011000        05 IDPSN             PIC 9(3).                                    
011100*                                 PROPER SHIPPING NAME                    
011200*                                 PROPER SHIPPING NAME                    
011300        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
011400*                                 PERSONKOD REFILLANSVARIG                
011500*                                 REFILL RESPONSIBLE ID                   
011600        05 IDPSN-US          PIC 9(3).                                    
011700*                                 PROPER SHIPPING NAME USA                
011800*                                 PROPER SHIPPING NAME USA                
011900        05 IDPSN-CA          PIC 9(3).                                    
012000*                                 PROPER SHIPPING NAME KANADA             
012100*                                 PROPER SHIPPING NAME CANADA             
012200        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
012300*                                 STATISTISKT NUMMER                      
012400*                                 1 = NORSKT                              
012500*                                 2 = ENGELSKT                            
012600*                                 3 = BELGISKT                            
012700*                                 4 = PERUANSKT                           
012800*                                 5 = SVENSKT                             
012900*                                 6 =                                     
013000*                                 STATISTICAL NO.                         
013100        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
013200*                                 STATISTISKT NUMMER                      
013300*                                 1 = NORSKT                              
013400*                                 2 = ENGELSKT                            
013500*                                 3 = BELGISKT                            
013600*                                 4 = PERUANSKT                           
013700*                                 5 = SVENSKT                             
013800*                                 6 =                                     
013900*                                 STATISTICAL NO.                         
014000        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
014100*                                 STATISTISKT NUMMER                      
014200*                                 1 = NORSKT                              
014300*                                 2 = ENGELSKT                            
014400*                                 3 = BELGISKT                            
014500*                                 4 = PERUANSKT                           
014600*                                 5 = SVENSKT                             
014700*                                 6 =                                     
014800*                                 STATISTICAL NO.                         
014900        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
015000*                                 STATISTISKT NUMMER                      
015100*                                 1 = NORSKT                              
015200*                                 2 = ENGELSKT                            
015300*                                 3 = BELGISKT                            
015400*                                 4 = PERUANSKT                           
015500*                                 5 = SVENSKT                             
015600*                                 6 =                                     
015700*                                 STATISTICAL NO.                         
015800        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
015900*                                 STATISTISKT NUMMER                      
016000*                                 1 = NORSKT                              
016100*                                 2 = ENGELSKT                            
016200*                                 3 = BELGISKT                            
016300*                                 4 = PERUANSKT                           
016400*                                 5 = SVENSKT                             
016500*                                 6 =                                     
016600*                                 STATISTICAL NO.                         
016700        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
016800*                                 STATISTISKT NUMMER                      
016900*                                 1 = NORSKT                              
017000*                                 2 = ENGELSKT                            
017100*                                 3 = BELGISKT                            
017200*                                 4 = PERUANSKT                           
017300*                                 5 = SVENSKT                             
017400*                                 6 =                                     
017500*                                 STATISTICAL NO.                         
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
027000*                                 ARTIKELVOLYM NETTO (CM3)                
027100*                                 PART NET VOLUME    (CM3)                
027200        05 IDKAT-1           PIC X(5).                                    
027300*                                 KATALOGBETECKNING                       
027400        05 IDKAT-2           PIC X(5).                                    
027500*                                 KATALOGBETECKNING                       
027600        05 IDKAT-3           PIC X(5).                                    
027700*                                 KATALOGBETECKNING                       
027800        05 FILLER            PIC X(78).                                   
027900     03 CDC-INFO.                                                         
028000*                                 INFO SOM GÄLLER ENBART CDC              
028100*                                                                         
028200*                                                                         
028300*                                 INFO VALID ONLY FOR CDC                 
028400        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028500*                                 LAGEROMRÅDE                             
028600*                                 AREA                                    
028700        05 ADGANG            PIC S9(3)           COMP-3.                  
028800*                                 GÅNG                                    
028900*                                 AISLE                                   
029000        05 ADPLATS           PIC S9(5)           COMP-3.                  
029100*                                 LAGERPLATSNUMMER                        
029200*                                 LOCATION                                
029300        05 FLMANAT           PIC X.                                       
029400*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
029500        05 FLMANBK           PIC X.                                       
029600*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
029700        05 FLMANGK           PIC X.                                       
029800*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
029900        05 FLMANKP           PIC X.                                       
030000*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
030100        05 FLMANLT           PIC X.                                       
030200*                                 MANUELLT SATT LEDTID ?                  
030300        05 FLMANOSK          PIC X.                                       
030400*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
030500*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
030600        05 FLMANQ            PIC X.                                       
030700*                                 MANUELL HEMTAGNINGSKVANTITET            
030800        05 FLMPB             PIC X.                                       
030900*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
031000        05 FLREFILL          PIC X.                                       
031100*                                 REFILLARTIKEL                           
031200*                                 REFILLPART                              
031300        05 FLSKROT-BEORD     PIC X.                                       
031400*                                 SKROTNING BEORDRAD AV ANSK              
031500*                                 SCRAPPING ORDERED BY PROCURER           
031600        05 FLTOPP            PIC X.                                       
031700*                                 TOPP-200-ARTIKEL                        
031800*                                 TOP 200 PART                            
031900        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
032000*                                 AVINUMMER SENASTE INLEVERANS            
032100        05 IDLEVNR-SEN       PIC X(5).                                    
032200*                                 SENASTE LEVERANTÖR                      
032300        05 IDPLANGR-AG       PIC S9              COMP-3.                  
032400*                                 PLANERINGSGRUPP ANSKAFFARE              
032500        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
032600*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
032700        05 IDPROENH-1        PIC X(8).                                    
032800*                                 PRODUKTIONSENHET                        
032900*                                 PRODUCTION UNIT                         
033000        05 IDPROENH-2        PIC X(8).                                    
033100*                                 PRODUKTIONSENHET                        
033200*                                 PRODUCTION UNIT                         
033300        05 IDPROENH-3        PIC X(8).                                    
033400*                                 PRODUKTIONSENHET                        
033500*                                 PRODUCTION UNIT                         
033600        05 IDPROJUP          PIC X(8).                                    
033700*                                 PROJEKTUPPDRAG                          
033800*                                 PROJECT ASSIGNMENT                      
033900        05 IDRITN            PIC X(10).                                   
034000*                                 RITNINGSNUMMER                          
034100*                                 DRAWING NUMBER                          
034200        05 KDAVT             PIC S9              COMP-3.                  
034300*                                 AVTALSMÄRKNING                          
034400*                                 AGREEMENT CODE                          
034500        05 KDFORP.                                                        
034600*                                 FÖRPACKNINGSKOD                         
034700*                                 PACKAGING CODE                          
034800           07 KDFORPPL       PIC 9.                                       
034900*                                 FÖRPACKNINGSPLATS                       
035000*                                 PREPACKING PLACE                        
035100           07 KDFORPGP       PIC 9(2).                                    
035200*                                 FÖRPACKNINGSGRUPP                       
035300*                                 PREPACKING GROUP                        
035400           07 KDFORPUF       PIC 9.                                       
035500*                                 UPPRÄKNINGSFAKTOR                       
035600*                                 ENUMERATION                             
035700        05 KDFREKKL          PIC X.                                       
035800*                                 FREKVENSKLASS                           
035900*                                 FREQ. CLASS                             
036000        05 KDHF              PIC S9              COMP-3.                  
036100*                                 HUVUDFÖRRÅDSMÄRKNING                    
036200*                                 CODE MAIN STORAGE                       
036300        05 KDKG              PIC S9              COMP-3.                  
036400*                                 KURANSGRUPP                             
036500*                                 TURNOVER CODE                           
036600        05 KDKSP             PIC S9              COMP-3.                  
036700*                                 KÖPSPÄRR                                
036800*                                 PURCHASE BLOCKING CODE                  
036900        05 KDLEVSP           PIC S9(3)           COMP-3.                  
037000*                                 SPÄRRKOD LEVERANS                       
037100*                                 DELIVERY BLOCKING CODE                  
037200        05 KDLPSP            PIC S9              COMP-3.                  
037300*                                 LEVERANSPLANESPÄRR                      
037400        05 KDPRISKL          PIC X.                                       
037500*                                 PRISKLASS                               
037600*                                 PRICE CLASS                             
037700        05 KDTIPPR           PIC S9              COMP-3.                  
037800*                                 TIPPAT PRIS KOD                         
037900*                                 ESTIMATED PRICE CODE                    
038000        05 KDVSOP            PIC S9(3)           COMP-3.                  
038100*                                 VSOP-KOD                                
038200*                                 VSOP-CODE                               
038300        05 KDVTH             PIC S9              COMP-3.                  
038400*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
038500*                                 CODE FOR COST RESPONSIBILITY            
038600        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
038700*                                 DEL AV AK SOM LIGGER I CDC              
038800*                                 PART OF AK IN THE CDC                   
038900        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
039000*                                 DEL AV AK PÅ VÄG                        
039100*                                 PART OF AK ON ITS WAY                   
039200        05 KVAKS-T           PIC S9(7)           COMP-3.                  
039300*                                 DEL AV AK I EN TERMINAL                 
039400*                                 PART OF AK IN A TERMINAL                
039500        05 KVAP              PIC S9(7)           COMP-3.                  
039600*                                 ANNULLATIONSPUNKT                       
039700        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
039800*                                 SENAST AVISERAT ANTAL                   
039900        05 KVBK              PIC S9(7)           COMP-3.                  
040000*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
040100        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
040200*                                 TOT BEST REST                           
040300        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
040400*                                 INLEVERANSTID     (ANTAL DAGAR)         
040500        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
040600*                                 DAGAR TULL- OCH TRANSPORT-TID           
040700        05 KVEFRS            PIC S9(7)           COMP-3.                  
040800*                                 EJ FAKTURERAT ANTAL STYCK               
040900*                                 ORDERED NOT INVOICED QTY                
041000        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
041100*                                 FRYSTID FÖR TPO-ORDER                   
041200*                                 FREEZTIME FOR TPO                       
041300        05 KVINVS            PIC S9(7)           COMP-3.                  
041400*                                 INVENTERINGSSALDO                       
041500*                                 STOCK-TAKING BALANCE                    
041600        05 KVKP              PIC S9(7)           COMP-3.                  
041700*                                 KÖPPUNKT                                
041800        05 KVLAAN            PIC S9(7)           COMP-3.                  
041900*                                 LÅNESALDO                               
042000        05 KVLS              PIC S9(7)           COMP-3.                  
042100*                                 LAGERSALDO                              
042200*                                 STOCK BALANCE                           
042300        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
042400*                                 SEPARAT PROGNOSFEL                      
042500        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
042600*                                 TOTALT PROGNOSFEL                       
042700        05 KVMP              PIC S9(7)           COMP-3.                  
042800*                                 MAXPUNKT                                
042900*                                 MAXIMUM POINT                           
043000        05 KVOKS             PIC S9(7)           COMP-3.                  
043100*                                 ORDERKÖSALDO                            
043200*                                 ORDER QUEUE BALANCE                     
043300        05 KVOVERF           PIC S9(7)           COMP-3.                  
043400*                                 ÖVERFÖRINGSSALDO                        
043500        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
043600*                                 SATS-PERIODBEHOV                        
043700*                                 KIT PERIOD REQUIREMENTS                 
043800        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
043900*                                 SEPARAT PERIODBEHOV                     
044000*                                 SEPARATE PERIOD REQUIREMENTS            
044100        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
044200*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
044300*                                 PERIODICAL DEMAND TPO1 AND TPO2         
044400*                                                                         
044500        05 KVPB-NDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
044600*                                 PB-NDC TOTALT FÖR SAMLTLIGA NDC         
044700*                                 :ER                                     
044800        05 KVPB-SDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
044900*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
045000*                                 :ER                                     
045100        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
045200*                                 PREL-AVB KVANT, KLASS 2-4               
045300*                                 PREL-RES QUANT, CLASS 2-4               
045400        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
045500*                                 PREL-AVB KVANT, KLASS 1                 
045600*                                 PREL-RES QUANT, CLASS 1                 
045700        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
045800*                                 PREL-AVB KVANT, VOR                     
045900*                                 PREL-RES QUANT, VOR                     
046000        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
046100*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
046200*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
046300        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
046400*                                 PRELIMINÄR RO-KVANT, KLASS 1            
046500*                                 PRELIMINARY BO-QUANT, CLASS 1           
046600        05 KVQ               PIC S9(7)           COMP-3.                  
046700*                                 EKONOMISK HEMTAGNINGSKVANTITET          
046800        05 KVRESS            PIC S9(7)           COMP-3.                  
046900*                                 RESERVERAT ANTAL ARTIKLAR               
047000*                                 QUANTITY RESERVED ITEMS                 
047100        05 KVROS             PIC S9(7)           COMP-3.                  
047200*                                 RESTORDERSALDO                          
047300*                                 BACKORDER QTY                           
047400        05 KVSLAGER          PIC S9(7)           COMP-3.                  
047500*                                 SÄKERHETSLAGER                          
047600*                                 SAFETY STOCK                            
047700        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
047800*                                 SLUTKÖPSSALDO                           
047900        05 KVSPANT           PIC S9(7)           COMP-3.                  
048000*                                 SPÄRRAT ANTAL                           
048100*                                 BLOCKED QTY                             
048200        05 KVUTRS            PIC S9(7)           COMP-3.                  
048300*                                 UTREDNINGSSALDO                         
048400*                                 INVESTIG.BALANCE                        
048500        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
048600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
048700        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
048800*                                 ANTAL VECKOR BESTÄLLNINGSTID            
048900        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
049000*                                 ANTAL VECKOR FRYSNINGSTID               
049100        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
049200*                                 ANTAL VECKOR LEDTID                     
049300        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
049400*                                 DIREKT LÖN                              
049500*                                 SURCHARGE COSTS                         
049600        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
049700*                                 DIREKT MATERIAL                         
049800*                                 SURCHARGE PACKING MATERIAL              
049900        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
050000*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
050100*                                 SUPPLIERS PACKING AND HANDLING          
050200        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
050300*                                 ORDERSÄRKOSTNAD                         
050400*                                 REMAINING OVERHEAD SURCHARGE            
050500        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
050600*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
050700*                                 REMAINING OVERHEAD SURCHARGE            
050800        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
050900*                                 DIREKTLEVERANSANDEL                     
051000        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
051100*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
051200*                                 ADJUSTMENT ALGORITM                     
051300        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
051400*                                 TPO-KVANTITET, TOTAL                    
051500*                                 TPO-QUANTITY, TOTAL                     
051600        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
051700*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
051800        05 TIINVDAT          PIC S9(5)           COMP-3.                  
051900*                                 INVENTERINGSDATUM                       
052000*                                 STOCKTAKING DATE                        
052100        05 TILPSP            PIC S9(5)           COMP-3.                  
052200*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
052300        05 TIREGDAT          PIC S9(7)           COMP-3.                  
052400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
052500*                                 REGISTRATION DATE (YYMMDD)              
052600        05 TISLJUST          PIC S9(5)           COMP-3.                  
052700*                                 VECKA DÅ JUSTERING AV SÄKER-            
052800*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
052900        05 TIURPROD          PIC S9(5)           COMP-3.                  
053000*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
053100        05 TISKROT           PIC S9(7)           COMP-3.                  
053200*                                 SKROTNINGSDATUM                         
053300*                                 DATE OF SCRAPPING                       
053400        05 FLCDART           PIC X.                                       
053500*                                 CROSS-DOCKING PART                      
053600*                                 CROSS-DOCKING PART                      
053700        05 ADLAGOMR-CD-1     PIC S9(3)           COMP-3.                  
053800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
053900*                                 AREA ADDRESS CROSS DOCKING WARE         
054000*                                 HOUSE                                   
054100        05 ADGANG-CD-1       PIC S9(3)           COMP-3.                  
054200*                                 GÅNG                                    
054300*                                 AISLE                                   
054400        05 ADPLATS-CD-1      PIC S9(5)           COMP-3.                  
054500*                                 LAGERPLATSNUMMER                        
054600*                                 LOCATION                                
054700        05 ADLAGOMR-CD-2     PIC S9(3)           COMP-3.                  
054800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
054900*                                 AREA ADDRESS CROSS DOCKING WARE         
055000*                                 HOUSE                                   
055100        05 ADGANG-CD-2       PIC S9(3)           COMP-3.                  
055200*                                 GÅNG                                    
055300*                                 AISLE                                   
055400        05 ADPLATS-CD-2      PIC S9(5)           COMP-3.                  
055500*                                 LAGERPLATSNUMMER                        
055600*                                 LOCATION                                
055700        05 ADLAGOMR-CD-3     PIC S9(3)           COMP-3.                  
055800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
055900*                                 AREA ADDRESS CROSS DOCKING WARE         
056000*                                 HOUSE                                   
056100        05 ADGANG-CD-3       PIC S9(3)           COMP-3.                  
056200*                                 GÅNG                                    
056300*                                 AISLE                                   
056400        05 ADPLATS-CD-3      PIC S9(5)           COMP-3.                  
056500*                                 LAGERPLATSNUMMER                        
056600*                                 LOCATION                                
056700        05 ADLAGOMR-CD-4     PIC S9(3)           COMP-3.                  
056800*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
056900*                                 AREA ADDRESS CROSS DOCKING WARE         
057000*                                 HOUSE                                   
057100        05 ADGANG-CD-4       PIC S9(3)           COMP-3.                  
057200*                                 GÅNG                                    
057300*                                 AISLE                                   
057400        05 ADPLATS-CD-4      PIC S9(5)           COMP-3.                  
057500*                                 LAGERPLATSNUMMER                        
057600*                                 LOCATION                                
057700        05 KVLS-CD-1         PIC S9(7)           COMP-3.                  
057800*                                 LAGERSALDO CD                           
057900*                                 STOCK BALANCE CD                        
058000        05 KVLS-CD-2         PIC S9(7)           COMP-3.                  
058100*                                 LAGERSALDO CD                           
058200*                                 STOCK BALANCE CD                        
058300        05 KVLS-CD-3         PIC S9(7)           COMP-3.                  
058400*                                 LAGERSALDO CD                           
058500*                                 STOCK BALANCE CD                        
058600        05 KVLS-CD-4         PIC S9(7)           COMP-3.                  
058700*                                 LAGERSALDO CD                           
058800*                                 STOCK BALANCE CD                        
058900        05 KVRESS-CD-1       PIC S9(7)           COMP-3.                  
059000*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
059100*                                  DOCKING LAGER                          
059200*                                 QUANTITY RESERVED ITEMS IN CROS         
059300*                                 S DOCKING WAREHOUSE                     
059400        05 KVRESS-CD-2       PIC S9(7)           COMP-3.                  
059500*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
059600*                                  DOCKING LAGER                          
059700*                                 QUANTITY RESERVED ITEMS IN CROS         
059800*                                 S DOCKING WAREHOUSE                     
059900        05 KVRESS-CD-3       PIC S9(7)           COMP-3.                  
060000*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
060100*                                  DOCKING LAGER                          
060200*                                 QUANTITY RESERVED ITEMS IN CROS         
060300*                                 S DOCKING WAREHOUSE                     
060400        05 KVRESS-CD-4       PIC S9(7)           COMP-3.                  
060500*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
060600*                                  DOCKING LAGER                          
060700*                                 QUANTITY RESERVED ITEMS IN CROS         
060800*                                 S DOCKING WAREHOUSE                     
060900        05 FILLER            PIC X(22).                                   
061000*** END OF VILMAII-COPY LENGTH= 711 BYTES                                 
