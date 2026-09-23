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
007700        05 FLSPECPR          PIC X.                                       
007800*                                 SPECIALPRISFLAGGA                       
007900*                                 SPECIAL PRICE FLAG                      
008000        05 FLTPO1            PIC X.                                       
008100*                                 ARTIKELN GODKÄND FÖR TPO1               
008200*                                 TPO1 ALLOWED FOR ARTICLE                
008300        05 IDANSK            PIC S9(3)           COMP-3.                  
008400*                                 ANSKAFFARNUMMER                         
008500*                                 PROCURER NO.                            
008600        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
008700*                                 EMBALLAGEARTIKELNR FÖR Q0               
008800        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
008900*                                 EMBALLAGEARTIKELNR FÖR Q1               
009000        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
009100*                                 EMBALLAGEARTIKELNR FÖR Q2               
009200        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
009300*                                 EMBALLAGEARTIKELNR FÖR Q3               
009400        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
009500*                                 EMBALLAGEARTIKELNR FÖR Q4               
009600        05 IDBERED           PIC S9(3)           COMP-3.                  
009700*                                 BEREDARENUMMER                          
009800        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
009900*                                 FUNKTIONSGRUPP                          
010000*                                 FUNCTION GROUP                          
010100        05 IDINK             PIC S9(3)           COMP-3.                  
010200*                                 INKÖPARNUMMER                           
010300*                                 PURCHASE IDENTIFICATION NUMBER          
010400        05 IDLEVNR           PIC S9(5)           COMP-3.                  
010500*                                 LEVERANTÖRNUMMER                        
010600*                                 SUPPLIER NUMBER (VENDORNUMBER)          
010700        05 IDLKTO            PIC S9(7)           COMP-3.                  
010800*                                 LAGERKONTO (FFHHHUU)                    
010900*                                 STOCK ACCOUNT (CCMMMSS)                 
011000        05 IDPROJ            PIC X(4).                                    
011100*                                 PARTS PROJEKTIDENTITET                  
011200*                                 PARTS PROJECT IDENTITY                  
011300        05 IDPSN             PIC 9(3).                                    
011400*                                 PROPER SHIPPING NAME                    
011500*                                 PROPER SHIPPING NAME                    
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
017000        05 KDAGE             PIC X.                                       
017100*                                 AGE-CODE                                
017200*                                 AGE-CODE                                
017300        05 KDARTHNT          PIC S9(7)           COMP-3.                  
017400*                                 HANTERINGSKOD                           
017500*                                 HANDLING CODE                           
017600        05 KDARTURS          PIC X(2).                                    
017700*                                 ARTIKELURSPRUNGSKOD                     
017800*                                 COUNTRY OF ORIGIN                       
017900        05 KDBPSR            PIC S9              COMP-3.                  
018000*                                 BASLAGERFÖRSLAGSNIVÅ                    
018100*                                 BASIC PART STOCK RECOMMENDATION         
018200        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
018300*                                 EMBALLAGEKOD 0                          
018400        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
018500*                                 EMBALLAGEKOD 1                          
018600        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
018700*                                 EMBALLAGEKOD 2                          
018800        05 KDERS             PIC S9(3)           COMP-3.                  
018900*                                 ERSÄTTNINGSKOD                          
019000*                                 SUPERSESSION CODE                       
019100        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
019200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
019300*                                 OBSOLETION SUPERSESSION CODE            
019400        05 KDFARLIG          PIC S9              COMP-3.                  
019500*                                 KOD FÖR FARLIGT GODS                    
019600*                                 DANGEROUS GOODS CODE                    
019700        05 KDGK              PIC S9              COMP-3.                  
019800*                                 GODSMOTTAGAREKOD                        
019900*                                 GOODS RECEIVING WAREHOUSE CODE          
020000        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020100*                                 PRODUKTSLAG                             
020200*                                 PRODUCT GROUP                           
020300        05 KDSORT            PIC X(2).                                    
020400*                                 SORT-KOD                                
020500*                                 UNIT OF MEASURE                         
020600        05 KDSPEEMB          PIC 9.                                       
020700*                                 SPECIALEMBALLAGEKOD                     
020800*                                 SPECIAL PACKING CODE                    
020900        05 KDSRA             PIC S9(3)           COMP-3.                  
021000*                                 SRA-KOD                                 
021100*                                 SRA CODE                                
021200        05 KDUART            PIC X.                                       
021300*                                 UNDANTAGSARTIKEL                        
021400*                                 EXECPTION PARTS                         
021500        05 KDVVKL            PIC S9              COMP-3.                  
021600*                                 VOLYMVÄRDESKLASS                        
021700*                                 VOLUME VALUE CLASS                      
021800        05 KDYTBEH           PIC S9(3)           COMP-3.                  
021900*                                 YTBEHANDLINGSKOD                        
022000*                                                                         
022100        05 KVPALL            PIC S9(7)           COMP-3.                  
022200*                                 ANTAL I PALL                            
022300*                                 QUANTITY IN PALLET                      
022400        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
022500*                                 ANTAL I Q0 FÖRPACKNING                  
022600        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
022700*                                 ANTAL I Q1 FÖRPACKNING                  
022800*                                 QUANTITY IN BULK PACK Q1                
022900        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023000*                                 ANTAL I Q2 FÖRPACKNING                  
023100*                                 QUANTITY IN BULK PACK Q2                
023200        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
023300*                                 ANTAL I Q3 FÖRPACKNING                  
023400*                                 QUANTITY IN BULK PACK Q3                
023500        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
023600*                                 ANTAL I Q4 FÖRPACKNING                  
023700*                                 QUANTITY IN BULK PACK Q4                
023800        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
023900*                                 BESTÄLLNINGSPRIS I KRONOR               
024000*                                 ORDER PRICE SWEDISH CURRENCY            
024100        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
024200*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
024300*                                 GROSS-PRICE EXPORT                      
024400*                                  (FOB-GROSS)                            
024500        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
024600*                                 ARTIKELNS SJÄLVKOSTNAD                  
024700*                                 COST OF SALES                           
024800        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
024900*                                 ARTIKELSTANDARDPRIS                     
025000*                                 STANDARD PRICE                          
025100        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
025200*                                 INKÖPSPRIS                              
025300*                                 PURCHASE PRICE                          
025400        05 TIERSDAT          PIC S9(5)           COMP-3.                  
025500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
025600*                                 DATE OF SUPERSESSION (YYWWD)            
025700        05 TIFINLV           PIC S9(5)           COMP-3.                  
025800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
025900*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026000        05 VKART             PIC S9(7)           COMP-3.                  
026100*                                 ARTIKELVIKT (G)                         
026200*                                 PART WEIGHT (G)                         
026300        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
026400*                                 ARTIKELVOLYM NETTO (CM3)                
026500*                                 PART NET VOLUME    (CM3)                
026600        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
026700*                                 PERSONKOD REFILLANSVARIG                
026800*                                 REFILL RESPONSIBLE ID                   
026900        05 IDPSN-US          PIC 9(3).                                    
027000*                                 PROPER SHIPPING NAME USA                
027100*                                 PROPER SHIPPING NAME USA                
027200        05 IDPSN-CA          PIC 9(3).                                    
027300*                                 PROPER SHIPPING NAME KANADA             
027400*                                 PROPER SHIPPING NAME CANADA             
027500        05 FILLER            PIC X(92).                                   
027600     03 CDC-INFO.                                                         
027700*                                 INFO SOM GÄLLER ENBART CDC              
027800*                                                                         
027900*                                                                         
028000*                                 INFO VALID ONLY FOR CDC                 
028100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028200*                                 LAGEROMRÅDE                             
028300*                                 AREA                                    
028400        05 ADGANG            PIC S9(3)           COMP-3.                  
028500*                                 GÅNG                                    
028600*                                 AISLE                                   
028700        05 ADPLATS           PIC S9(5)           COMP-3.                  
028800*                                 LAGERPLATSNUMMER                        
028900*                                 LOCATION                                
029000        05 FLMANAT           PIC X.                                       
029100*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
029200        05 FLMANBK           PIC X.                                       
029300*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
029400        05 FLMANGK           PIC X.                                       
029500*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
029600        05 FLMANKP           PIC X.                                       
029700*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
029800        05 FLMANLT           PIC X.                                       
029900*                                 MANUELLT SATT LEDTID ?                  
030000        05 FLMANOSK          PIC X.                                       
030100*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
030200*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
030300        05 FLMANQ            PIC X.                                       
030400*                                 MANUELL HEMTAGNINGSKVANTITET            
030500        05 FLMPB             PIC X.                                       
030600*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
030700        05 FLREFILL          PIC X.                                       
030800*                                 REFILLARTIKEL                           
030900*                                 REFILLPART                              
031000        05 FLSKROT-BEORD     PIC X.                                       
031100*                                 SKROTNING BEORDRAD AV ANSK              
031200*                                 SCRAPPING ORDERED BY PROCURER           
031300        05 FLTOPP            PIC X.                                       
031400*                                 TOPP-200-ARTIKEL                        
031500*                                 TOP 200 PART                            
031600        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
031700*                                 AVINUMMER SENASTE INLEVERANS            
031800        05 IDLEVNR-SEN       PIC S9(5)           COMP-3.                  
031900*                                 SENASTE LEVERANTÖR                      
032000        05 IDPLANGR-AG       PIC S9              COMP-3.                  
032100*                                 PLANERINGSGRUPP ANSKAFFARE              
032200        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
032300*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
032400        05 IDPROENH-1        PIC X(8).                                    
032500*                                 PRODUKTIONSENHET                        
032600*                                 PRODUCTION UNIT                         
032700        05 IDPROENH-2        PIC X(8).                                    
032800*                                 PRODUKTIONSENHET                        
032900*                                 PRODUCTION UNIT                         
033000        05 IDPROENH-3        PIC X(8).                                    
033100*                                 PRODUKTIONSENHET                        
033200*                                 PRODUCTION UNIT                         
033300        05 IDPROJUP          PIC X(8).                                    
033400*                                 PROJEKTUPPDRAG                          
033500*                                 PROJECT ASSIGNMENT                      
033600        05 IDRITN            PIC X(10).                                   
033700*                                 RITNINGSNUMMER                          
033800*                                 DRAWING NUMBER                          
033900        05 KDAVT             PIC S9              COMP-3.                  
034000*                                 AVTALSMÄRKNING                          
034100*                                 AGREEMENT CODE                          
034200        05 KDFORP.                                                        
034300*                                 FÖRPACKNINGSKOD                         
034400*                                 PACKAGING CODE                          
034500           07 KDFORPPL       PIC 9.                                       
034600*                                 FÖRPACKNINGSPLATS                       
034700*                                 PREPACKING PLACE                        
034800           07 KDFORPGP       PIC 9(2).                                    
034900*                                 FÖRPACKNINGSGRUPP                       
035000*                                 PREPACKING GROUP                        
035100           07 KDFORPUF       PIC 9.                                       
035200*                                 UPPRÄKNINGSFAKTOR                       
035300*                                 ENUMERATION                             
035400        05 KDFREKKL          PIC X.                                       
035500*                                 FREKVENSKLASS                           
035600*                                 FREQ. CLASS                             
035700        05 KDHF              PIC S9              COMP-3.                  
035800*                                 HUVUDFÖRRÅDSMÄRKNING                    
035900*                                 CODE MAIN STORAGE                       
036000        05 KDKG              PIC S9              COMP-3.                  
036100*                                 KURANSGRUPP                             
036200*                                 TURNOVER CODE                           
036300        05 KDKSP             PIC S9              COMP-3.                  
036400*                                 KÖPSPÄRR                                
036500*                                 PURCHASE BLOCKING CODE                  
036600        05 KDLEVSP           PIC S9(3)           COMP-3.                  
036700*                                 SPÄRRKOD LEVERANS                       
036800*                                 DELIVERY BLOCKING CODE                  
036900        05 KDLPSP            PIC S9              COMP-3.                  
037000*                                 LEVERANSPLANESPÄRR                      
037100        05 KDPRISKL          PIC X.                                       
037200*                                 PRISKLASS                               
037300*                                 PRICE CLASS                             
037400        05 KDTIPPR           PIC S9              COMP-3.                  
037500*                                 TIPPAT PRIS KOD                         
037600*                                 ESTIMATED PRICE CODE                    
037700        05 KDVSOP            PIC S9(3)           COMP-3.                  
037800*                                 VSOP-KOD                                
037900*                                 VSOP-CODE                               
038000        05 KDVTH             PIC S9              COMP-3.                  
038100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
038200*                                 CODE FOR COST RESPONSIBILITY            
038300        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
038400*                                 DEL AV AK SOM LIGGER I CDC              
038500*                                 PART OF AK IN THE CDC                   
038600        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
038700*                                 DEL AV AK PÅ VÄG                        
038800*                                 PART OF AK ON ITS WAY                   
038900        05 KVAKS-T           PIC S9(7)           COMP-3.                  
039000*                                 DEL AV AK I EN TERMINAL                 
039100*                                 PART OF AK IN A TERMINAL                
039200        05 KVAP              PIC S9(7)           COMP-3.                  
039300*                                 ANNULLATIONSPUNKT                       
039400        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
039500*                                 SENAST AVISERAT ANTAL                   
039600        05 KVBK              PIC S9(7)           COMP-3.                  
039700*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
039800        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
039900*                                 TOT BEST REST                           
040000        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
040100*                                 INLEVERANSTID     (ANTAL DAGAR)         
040200        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
040300*                                 DAGAR TULL- OCH TRANSPORT-TID           
040400        05 KVEFRS            PIC S9(7)           COMP-3.                  
040500*                                 EJ FAKTURERAT ANTAL STYCK               
040600*                                 ORDERED NOT INVOICED QTY                
040700        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
040800*                                 FRYSTID FÖR TPO-ORDER                   
040900*                                 FREEZTIME FOR TPO                       
041000        05 KVINVS            PIC S9(7)           COMP-3.                  
041100*                                 INVENTERINGSSALDO                       
041200*                                 STOCK-TAKING BALANCE                    
041300        05 KVKP              PIC S9(7)           COMP-3.                  
041400*                                 KÖPPUNKT                                
041500        05 KVLAAN            PIC S9(7)           COMP-3.                  
041600*                                 LÅNESALDO                               
041700        05 KVLS              PIC S9(7)           COMP-3.                  
041800*                                 LAGERSALDO                              
041900*                                 STOCK BALANCE                           
042000        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
042100*                                 SEPARAT PROGNOSFEL                      
042200        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
042300*                                 TOTALT PROGNOSFEL                       
042400        05 KVMP              PIC S9(7)           COMP-3.                  
042500*                                 MAXPUNKT                                
042600*                                 MAXIMUM POINT                           
042700        05 KVOKS             PIC S9(7)           COMP-3.                  
042800*                                 ORDERKÖSALDO                            
042900*                                 ORDER QUEUE BALANCE                     
043000        05 KVOVERF           PIC S9(7)           COMP-3.                  
043100*                                 ÖVERFÖRINGSSALDO                        
043200        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
043300*                                 SATS-PERIODBEHOV                        
043400*                                 KIT PERIOD REQUIREMENTS                 
043500        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
043600*                                 SEPARAT PERIODBEHOV                     
043700*                                 SEPARATE PERIOD REQUIREMENTS            
043800        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
043900*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
044000*                                 PERIODICAL DEMAND TPO1 AND TPO2         
044100*                                                                         
044200        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
044300*                                 PREL-AVB KVANT, KLASS 2-4               
044400*                                 PREL-RES QUANT, CLASS 2-4               
044500        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
044600*                                 PREL-AVB KVANT, KLASS 1                 
044700*                                 PREL-RES QUANT, CLASS 1                 
044800        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
044900*                                 PREL-AVB KVANT, VOR                     
045000*                                 PREL-RES QUANT, VOR                     
045100        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
045200*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
045300*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
045400        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
045500*                                 PRELIMINÄR RO-KVANT, KLASS 1            
045600*                                 PRELIMINARY BO-QUANT, CLASS 1           
045700        05 KVQ               PIC S9(7)           COMP-3.                  
045800*                                 EKONOMISK HEMTAGNINGSKVANTITET          
045900        05 KVRESS            PIC S9(7)           COMP-3.                  
046000*                                 RESERVERAT ANTAL ARTIKLAR               
046100*                                 QUANTITY RESERVED ITEMS                 
046200        05 KVROS             PIC S9(7)           COMP-3.                  
046300*                                 RESTORDERSALDO                          
046400*                                 BACKORDER QTY                           
046500        05 KVSLAGER          PIC S9(7)           COMP-3.                  
046600*                                 SÄKERHETSLAGER                          
046700*                                 SAFETY STOCK                            
046800        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
046900*                                 SLUTKÖPSSALDO                           
047000        05 KVSPANT           PIC S9(7)           COMP-3.                  
047100*                                 SPÄRRAT ANTAL                           
047200*                                 BLOCKED QTY                             
047300        05 KVUTRS            PIC S9(7)           COMP-3.                  
047400*                                 UTREDNINGSSALDO                         
047500*                                 INVESTIG.BALANCE                        
047600        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
047700*                                 ANTAL VECKOR ANSKAFFNINGSTID            
047800        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
047900*                                 ANTAL VECKOR BESTÄLLNINGSTID            
048000        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
048100*                                 ANTAL VECKOR FRYSNINGSTID               
048200        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
048300*                                 ANTAL VECKOR LEDTID                     
048400        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
048500*                                 DIREKT LÖN                              
048600*                                 SURCHARGE COSTS                         
048700        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
048800*                                 DIREKT MATERIAL                         
048900*                                 SURCHARGE PACKING MATERIAL              
049000        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
049100*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
049200*                                 SUPPLIERS PACKING AND HANDLING          
049300        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
049400*                                 ORDERSÄRKOSTNAD                         
049500*                                 REMAINING OVERHEAD SURCHARGE            
049600        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
049700*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
049800*                                 REMAINING OVERHEAD SURCHARGE            
049900        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
050000*                                 DIREKTLEVERANSANDEL                     
050100        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
050200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
050300*                                 ADJUSTMENT ALGORITM                     
050400        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
050500*                                 TPO-KVANTITET, TOTAL                    
050600*                                 TPO-QUANTITY, TOTAL                     
050700        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
050800*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
050900        05 TIINVDAT          PIC S9(5)           COMP-3.                  
051000*                                 INVENTERINGSDATUM                       
051100*                                 STOCKTAKING DATE                        
051200        05 TILPSP            PIC S9(5)           COMP-3.                  
051300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
051400        05 TIREGDAT          PIC S9(7)           COMP-3.                  
051500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
051600*                                 REGISTRATION DATE (YYMMDD)              
051700        05 TISLJUST          PIC S9(5)           COMP-3.                  
051800*                                 VECKA DÅ JUSTERING AV SÄKER-            
051900*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
052000        05 TIURPROD          PIC S9(5)           COMP-3.                  
052100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
052200        05 FILLER            PIC X(30).                                   
052300*** END OF VILMAII-COPY LENGTH= 642 BYTES                                 
