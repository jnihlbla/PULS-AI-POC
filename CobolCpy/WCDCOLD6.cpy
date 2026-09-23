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
009800        05 FILLER            PIC X(2).                                    
009900        05 IDLEVNR           PIC X(5).                                    
010000*                                 LEVERANTÖRNUMMER                        
010100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010200        05 IDLKTO            PIC S9(7)           COMP-3.                  
010300*                                 LAGERKONTO (FFHHHUU)                    
010400*                                 STOCK ACCOUNT (CCMMMSS)                 
010500        05 IDPROJ            PIC X(4).                                    
010600*                                 PARTS PROJEKTIDENTITET                  
010700*                                 PARTS PROJECT IDENTITY                  
010800        05 IDPSN             PIC 9(3).                                    
010900*                                 PROPER SHIPPING NAME                    
011000*                                 PROPER SHIPPING NAME                    
011100        05 IDPERSON-BUY      PIC S9(3)           COMP-3.                  
011200*                                 PERSONKOD REFILLANSVARIG                
011300*                                 REFILL RESPONSIBLE ID                   
011400        05 IDPSN-US          PIC 9(3).                                    
011500*                                 PROPER SHIPPING NAME USA                
011600*                                 PROPER SHIPPING NAME USA                
011700        05 IDPSN-CA          PIC 9(3).                                    
011800*                                 PROPER SHIPPING NAME KANADA             
011900*                                 PROPER SHIPPING NAME CANADA             
012000        05 IDSTATNR-1        PIC S9(9)           COMP-3.                  
012100*                                 STATISTISKT NUMMER                      
012200*                                 1 = NORSKT                              
012300*                                 2 = ENGELSKT                            
012400*                                 3 = BELGISKT                            
012500*                                 4 = PERUANSKT                           
012600*                                 5 = SVENSKT                             
012700*                                 6 =                                     
012800*                                 STATISTICAL NO.                         
012900        05 IDSTATNR-2        PIC S9(9)           COMP-3.                  
013000*                                 STATISTISKT NUMMER                      
013100*                                 1 = NORSKT                              
013200*                                 2 = ENGELSKT                            
013300*                                 3 = BELGISKT                            
013400*                                 4 = PERUANSKT                           
013500*                                 5 = SVENSKT                             
013600*                                 6 =                                     
013700*                                 STATISTICAL NO.                         
013800        05 IDSTATNR-3        PIC S9(9)           COMP-3.                  
013900*                                 STATISTISKT NUMMER                      
014000*                                 1 = NORSKT                              
014100*                                 2 = ENGELSKT                            
014200*                                 3 = BELGISKT                            
014300*                                 4 = PERUANSKT                           
014400*                                 5 = SVENSKT                             
014500*                                 6 =                                     
014600*                                 STATISTICAL NO.                         
014700        05 IDSTATNR-4        PIC S9(9)           COMP-3.                  
014800*                                 STATISTISKT NUMMER                      
014900*                                 1 = NORSKT                              
015000*                                 2 = ENGELSKT                            
015100*                                 3 = BELGISKT                            
015200*                                 4 = PERUANSKT                           
015300*                                 5 = SVENSKT                             
015400*                                 6 =                                     
015500*                                 STATISTICAL NO.                         
015600        05 IDSTATNR-5        PIC S9(9)           COMP-3.                  
015700*                                 STATISTISKT NUMMER                      
015800*                                 1 = NORSKT                              
015900*                                 2 = ENGELSKT                            
016000*                                 3 = BELGISKT                            
016100*                                 4 = PERUANSKT                           
016200*                                 5 = SVENSKT                             
016300*                                 6 =                                     
016400*                                 STATISTICAL NO.                         
016500        05 IDSTATNR-6        PIC S9(9)           COMP-3.                  
016600*                                 STATISTISKT NUMMER                      
016700*                                 1 = NORSKT                              
016800*                                 2 = ENGELSKT                            
016900*                                 3 = BELGISKT                            
017000*                                 4 = PERUANSKT                           
017100*                                 5 = SVENSKT                             
017200*                                 6 =                                     
017300*                                 STATISTICAL NO.                         
017400        05 KDAGE             PIC X.                                       
017500*                                 AGE-CODE                                
017600*                                 AGE-CODE                                
017700        05 KDARTHNT          PIC S9(7)           COMP-3.                  
017800*                                 HANTERINGSKOD                           
017900*                                 HANDLING CODE                           
018000        05 KDARTURS          PIC X(2).                                    
018100*                                 ARTIKELURSPRUNGSKOD                     
018200*                                 COUNTRY OF ORIGIN                       
018300        05 KDBPSR            PIC S9              COMP-3.                  
018400*                                 BASLAGERFÖRSLAGSNIVÅ                    
018500*                                 BASIC PART STOCK RECOMMENDATION         
018600        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
018700*                                 EMBALLAGEKOD 0                          
018800        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
018900*                                 EMBALLAGEKOD 1                          
019000        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
019100*                                 EMBALLAGEKOD 2                          
019200        05 KDERS             PIC S9(3)           COMP-3.                  
019300*                                 ERSÄTTNINGSKOD                          
019400*                                 SUPERSESSION CODE                       
019500        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
019600*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
019700*                                 OBSOLETION SUPERSESSION CODE            
019800        05 KDFARLIG          PIC S9              COMP-3.                  
019900*                                 KOD FÖR FARLIGT GODS                    
020000*                                 DANGEROUS GOODS CODE                    
020100        05 KDGK              PIC S9              COMP-3.                  
020200*                                 GODSMOTTAGAREKOD                        
020300*                                 GOODS RECEIVING WAREHOUSE CODE          
020400        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020500*                                 PRODUKTSLAG                             
020600*                                 PRODUCT GROUP                           
020700        05 KDSORT            PIC X(2).                                    
020800*                                 SORT-KOD                                
020900*                                 UNIT OF MEASURE                         
021000        05 KDSPEEMB          PIC 9.                                       
021100*                                 SPECIALEMBALLAGEKOD                     
021200*                                 SPECIAL PACKING CODE                    
021300        05 KDSRA             PIC S9(3)           COMP-3.                  
021400*                                 SRA-KOD                                 
021500*                                 SRA CODE                                
021600        05 KDUART            PIC X.                                       
021700*                                 UNDANTAGSARTIKEL                        
021800*                                 EXECPTION PARTS                         
021900        05 KDVVKL            PIC S9              COMP-3.                  
022000*                                 VOLYMVÄRDESKLASS                        
022100*                                 VOLUME VALUE CLASS                      
022200        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022300*                                 YTBEHANDLINGSKOD                        
022400*                                                                         
022500        05 KVPALL            PIC S9(7)           COMP-3.                  
022600*                                 ANTAL I PALL                            
022700*                                 QUANTITY IN PALLET                      
022800        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
022900*                                 ANTAL I Q0 FÖRPACKNING                  
023000        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
023100*                                 ANTAL I Q1 FÖRPACKNING                  
023200*                                 QUANTITY IN BULK PACK Q1                
023300        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023400*                                 ANTAL I Q2 FÖRPACKNING                  
023500*                                 QUANTITY IN BULK PACK Q2                
023600        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
023700*                                 ANTAL I Q3 FÖRPACKNING                  
023800*                                 QUANTITY IN BULK PACK Q3                
023900        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
024000*                                 ANTAL I Q4 FÖRPACKNING                  
024100*                                 QUANTITY IN BULK PACK Q4                
024200        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024300*                                 BESTÄLLNINGSPRIS I KRONOR               
024400*                                 ORDER PRICE SWEDISH CURRENCY            
024500        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
024600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
024700*                                 GROSS-PRICE EXPORT                      
024800*                                  (FOB-GROSS)                            
024900        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
025000*                                 ARTIKELNS SJÄLVKOSTNAD                  
025100*                                 COST OF SALES                           
025200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025300*                                 ARTIKELSTANDARDPRIS                     
025400*                                 STANDARD PRICE                          
025500        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
025600*                                 INKÖPSPRIS                              
025700*                                 PURCHASE PRICE                          
025800        05 TIERSDAT          PIC S9(5)           COMP-3.                  
025900*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
026000*                                 DATE OF SUPERSESSION (YYWWD)            
026100        05 TIFINLV           PIC S9(5)           COMP-3.                  
026200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026300*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026400        05 VKART             PIC S9(7)           COMP-3.                  
026500*                                 ARTIKELVIKT (G)                         
026600*                                 PART WEIGHT (G)                         
026700        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
026800*                                 ARTIKELVOLYM NETTO (CM3)                
026900*                                 PART NET VOLUME    (CM3)                
027000        05 IDKAT-1           PIC X(5).                                    
027100*                                 KATALOGBETECKNING                       
027200        05 IDKAT-2           PIC X(5).                                    
027300*                                 KATALOGBETECKNING                       
027400        05 IDKAT-3           PIC X(5).                                    
027500*                                 KATALOGBETECKNING                       
027600        05 IDINK             PIC X(4).                                    
027700*                                 INKÖPARNUMMER                           
027800*                                 PURCHASE IDENTIFICATION NUMBER          
027900        05 FILLER            PIC X(74).                                   
028000     03 CDC-INFO.                                                         
028100*                                 INFO SOM GÄLLER ENBART CDC              
028200*                                                                         
028300*                                                                         
028400*                                 INFO VALID ONLY FOR CDC                 
028500        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028600*                                 LAGEROMRÅDE                             
028700*                                 AREA                                    
028800        05 ADGANG            PIC S9(3)           COMP-3.                  
028900*                                 GÅNG                                    
029000*                                 AISLE                                   
029100        05 ADPLATS           PIC S9(5)           COMP-3.                  
029200*                                 LAGERPLATSNUMMER                        
029300*                                 LOCATION                                
029400        05 FLMANAT           PIC X.                                       
029500*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
029600        05 FLMANBK           PIC X.                                       
029700*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
029800        05 FLMANGK           PIC X.                                       
029900*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
030000        05 FLMANKP           PIC X.                                       
030100*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
030200        05 FLMANLT           PIC X.                                       
030300*                                 MANUELLT SATT LEDTID ?                  
030400        05 FLMANOSK          PIC X.                                       
030500*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
030600*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
030700        05 FLMANQ            PIC X.                                       
030800*                                 MANUELL HEMTAGNINGSKVANTITET            
030900        05 FLMPB             PIC X.                                       
031000*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
031100        05 FLREFILL          PIC X.                                       
031200*                                 REFILLARTIKEL                           
031300*                                 REFILLPART                              
031400        05 FLSKROT-BEORD     PIC X.                                       
031500*                                 SKROTNING BEORDRAD AV ANSK              
031600*                                 SCRAPPING ORDERED BY PROCURER           
031700        05 FLTOPP            PIC X.                                       
031800*                                 TOPP-200-ARTIKEL                        
031900*                                 TOP 200 PART                            
032000        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
032100*                                 AVINUMMER SENASTE INLEVERANS            
032200        05 IDLEVNR-SEN       PIC X(5).                                    
032300*                                 SENASTE LEVERANTÖR                      
032400        05 IDPLANGR-AG       PIC S9              COMP-3.                  
032500*                                 PLANERINGSGRUPP ANSKAFFARE              
032600        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
032700*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
032800        05 IDPROENH-1        PIC X(8).                                    
032900*                                 PRODUKTIONSENHET                        
033000*                                 PRODUCTION UNIT                         
033100        05 IDPROENH-2        PIC X(8).                                    
033200*                                 PRODUKTIONSENHET                        
033300*                                 PRODUCTION UNIT                         
033400        05 IDPROENH-3        PIC X(8).                                    
033500*                                 PRODUKTIONSENHET                        
033600*                                 PRODUCTION UNIT                         
033700        05 IDPROJUP          PIC X(8).                                    
033800*                                 PROJEKTUPPDRAG                          
033900*                                 PROJECT ASSIGNMENT                      
034000        05 IDRITN            PIC X(10).                                   
034100*                                 RITNINGSNUMMER                          
034200*                                 DRAWING NUMBER                          
034300        05 KDAVT             PIC S9              COMP-3.                  
034400*                                 AVTALSMÄRKNING                          
034500*                                 AGREEMENT CODE                          
034600        05 KDFORP.                                                        
034700*                                 FÖRPACKNINGSKOD                         
034800*                                 PACKAGING CODE                          
034900           07 KDFORPPL       PIC 9.                                       
035000*                                 FÖRPACKNINGSPLATS                       
035100*                                 PREPACKING PLACE                        
035200           07 KDFORPGP       PIC 9(2).                                    
035300*                                 FÖRPACKNINGSGRUPP                       
035400*                                 PREPACKING GROUP                        
035500           07 KDFORPUF       PIC 9.                                       
035600*                                 UPPRÄKNINGSFAKTOR                       
035700*                                 ENUMERATION                             
035800        05 KDFREKKL          PIC X.                                       
035900*                                 FREKVENSKLASS                           
036000*                                 FREQ. CLASS                             
036100        05 KDHF              PIC S9              COMP-3.                  
036200*                                 HUVUDFÖRRÅDSMÄRKNING                    
036300*                                 CODE MAIN STORAGE                       
036400        05 KDKG              PIC S9              COMP-3.                  
036500*                                 KURANSGRUPP                             
036600*                                 TURNOVER CODE                           
036700        05 KDKSP             PIC S9              COMP-3.                  
036800*                                 KÖPSPÄRR                                
036900*                                 PURCHASE BLOCKING CODE                  
037000        05 KDLEVSP           PIC S9(3)           COMP-3.                  
037100*                                 SPÄRRKOD LEVERANS                       
037200*                                 DELIVERY BLOCKING CODE                  
037300        05 KDLPSP            PIC S9              COMP-3.                  
037400*                                 LEVERANSPLANESPÄRR                      
037500        05 KDPRISKL          PIC X.                                       
037600*                                 PRISKLASS                               
037700*                                 PRICE CLASS                             
037800        05 KDTIPPR           PIC S9              COMP-3.                  
037900*                                 TIPPAT PRIS KOD                         
038000*                                 ESTIMATED PRICE CODE                    
038100        05 KDVSOP            PIC S9(3)           COMP-3.                  
038200*                                 VSOP-KOD                                
038300*                                 VSOP-CODE                               
038400        05 KDVTH             PIC S9              COMP-3.                  
038500*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
038600*                                 CODE FOR COST RESPONSIBILITY            
038700        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
038800*                                 DEL AV AK SOM LIGGER I CDC              
038900*                                 PART OF AK IN THE CDC                   
039000        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
039100*                                 DEL AV AK PÅ VÄG                        
039200*                                 PART OF AK ON ITS WAY                   
039300        05 KVAKS-T           PIC S9(7)           COMP-3.                  
039400*                                 DEL AV AK I EN TERMINAL                 
039500*                                 PART OF AK IN A TERMINAL                
039600        05 KVAP              PIC S9(7)           COMP-3.                  
039700*                                 ANNULLATIONSPUNKT                       
039800        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
039900*                                 SENAST AVISERAT ANTAL                   
040000        05 KVBK              PIC S9(7)           COMP-3.                  
040100*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
040200        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
040300*                                 TOT BEST REST                           
040400        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
040500*                                 INLEVERANSTID     (ANTAL DAGAR)         
040600        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
040700*                                 DAGAR TULL- OCH TRANSPORT-TID           
040800        05 KVEFRS            PIC S9(7)           COMP-3.                  
040900*                                 EJ FAKTURERAT ANTAL STYCK               
041000*                                 ORDERED NOT INVOICED QTY                
041100        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
041200*                                 FRYSTID FÖR TPO-ORDER                   
041300*                                 FREEZTIME FOR TPO                       
041400        05 KVINVS            PIC S9(7)           COMP-3.                  
041500*                                 INVENTERINGSSALDO                       
041600*                                 STOCK-TAKING BALANCE                    
041700        05 KVKP              PIC S9(7)           COMP-3.                  
041800*                                 KÖPPUNKT                                
041900        05 KVLAAN            PIC S9(7)           COMP-3.                  
042000*                                 LÅNESALDO                               
042100        05 KVLS              PIC S9(7)           COMP-3.                  
042200*                                 LAGERSALDO                              
042300*                                 STOCK BALANCE                           
042400        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
042500*                                 SEPARAT PROGNOSFEL                      
042600        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
042700*                                 TOTALT PROGNOSFEL                       
042800        05 KVMP              PIC S9(7)           COMP-3.                  
042900*                                 MAXPUNKT                                
043000*                                 MAXIMUM POINT                           
043100        05 KVOKS             PIC S9(7)           COMP-3.                  
043200*                                 ORDERKÖSALDO                            
043300*                                 ORDER QUEUE BALANCE                     
043400        05 KVOVERF           PIC S9(7)           COMP-3.                  
043500*                                 ÖVERFÖRINGSSALDO                        
043600        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
043700*                                 SATS-PERIODBEHOV                        
043800*                                 KIT PERIOD REQUIREMENTS                 
043900        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
044000*                                 SEPARAT PERIODBEHOV                     
044100*                                 SEPARATE PERIOD REQUIREMENTS            
044200        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
044300*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
044400*                                 PERIODICAL DEMAND TPO1 AND TPO2         
044500*                                                                         
044600        05 KVPB-NDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
044700*                                 PB-NDC TOTALT FÖR SAMLTLIGA NDC         
044800*                                 :ER                                     
044900        05 KVPB-SDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
045000*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
045100*                                 :ER                                     
045200        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
045300*                                 PREL-AVB KVANT, KLASS 2-4               
045400*                                 PREL-RES QUANT, CLASS 2-4               
045500        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
045600*                                 PREL-AVB KVANT, KLASS 1                 
045700*                                 PREL-RES QUANT, CLASS 1                 
045800        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
045900*                                 PREL-AVB KVANT, VOR                     
046000*                                 PREL-RES QUANT, VOR                     
046100        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
046200*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
046300*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
046400        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
046500*                                 PRELIMINÄR RO-KVANT, KLASS 1            
046600*                                 PRELIMINARY BO-QUANT, CLASS 1           
046700        05 KVQ               PIC S9(7)           COMP-3.                  
046800*                                 EKONOMISK HEMTAGNINGSKVANTITET          
046900        05 KVRESS            PIC S9(7)           COMP-3.                  
047000*                                 RESERVERAT ANTAL ARTIKLAR               
047100*                                 QUANTITY RESERVED ITEMS                 
047200        05 KVROS             PIC S9(7)           COMP-3.                  
047300*                                 RESTORDERSALDO                          
047400*                                 BACKORDER QTY                           
047500        05 KVSLAGER          PIC S9(7)           COMP-3.                  
047600*                                 SÄKERHETSLAGER                          
047700*                                 SAFETY STOCK                            
047800        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
047900*                                 SLUTKÖPSSALDO                           
048000        05 KVSPANT           PIC S9(7)           COMP-3.                  
048100*                                 SPÄRRAT ANTAL                           
048200*                                 BLOCKED QTY                             
048300        05 KVUTRS            PIC S9(7)           COMP-3.                  
048400*                                 UTREDNINGSSALDO                         
048500*                                 INVESTIG.BALANCE                        
048600        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
048700*                                 ANTAL VECKOR ANSKAFFNINGSTID            
048800        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
048900*                                 ANTAL VECKOR BESTÄLLNINGSTID            
049000        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
049100*                                 ANTAL VECKOR FRYSNINGSTID               
049200        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
049300*                                 ANTAL VECKOR LEDTID                     
049400        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
049500*                                 DIREKT LÖN                              
049600*                                 SURCHARGE COSTS                         
049700        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
049800*                                 DIREKT MATERIAL                         
049900*                                 SURCHARGE PACKING MATERIAL              
050000        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
050100*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
050200*                                 SUPPLIERS PACKING AND HANDLING          
050300        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
050400*                                 ORDERSÄRKOSTNAD                         
050500*                                 REMAINING OVERHEAD SURCHARGE            
050600        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
050700*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
050800*                                 REMAINING OVERHEAD SURCHARGE            
050900        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
051000*                                 DIREKTLEVERANSANDEL                     
051100        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
051200*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
051300*                                 ADJUSTMENT ALGORITM                     
051400        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
051500*                                 TPO-KVANTITET, TOTAL                    
051600*                                 TPO-QUANTITY, TOTAL                     
051700        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
051800*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
051900        05 TIINVDAT          PIC S9(5)           COMP-3.                  
052000*                                 INVENTERINGSDATUM                       
052100*                                 STOCKTAKING DATE                        
052200        05 TILPSP            PIC S9(5)           COMP-3.                  
052300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
052400        05 TIREGDAT          PIC S9(7)           COMP-3.                  
052500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
052600*                                 REGISTRATION DATE (YYMMDD)              
052700        05 TISLJUST          PIC S9(5)           COMP-3.                  
052800*                                 VECKA DÅ JUSTERING AV SÄKER-            
052900*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
053000        05 TIURPROD          PIC S9(5)           COMP-3.                  
053100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
053200        05 TISKROT           PIC S9(7)           COMP-3.                  
053300*                                 SKROTNINGSDATUM                         
053400*                                 DATE OF SCRAPPING                       
053500        05 FLCDART           PIC X.                                       
053600*                                 CROSS-DOCKING PART                      
053700*                                 CROSS-DOCKING PART                      
053800        05 ADLAGOMR-CD-1     PIC S9(3)           COMP-3.                  
053900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
054000*                                 AREA ADDRESS CROSS DOCKING WARE         
054100*                                 HOUSE                                   
054200        05 ADGANG-CD-1       PIC S9(3)           COMP-3.                  
054300*                                 GÅNG                                    
054400*                                 AISLE                                   
054500        05 ADPLATS-CD-1      PIC S9(5)           COMP-3.                  
054600*                                 LAGERPLATSNUMMER                        
054700*                                 LOCATION                                
054800        05 ADLAGOMR-CD-2     PIC S9(3)           COMP-3.                  
054900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
055000*                                 AREA ADDRESS CROSS DOCKING WARE         
055100*                                 HOUSE                                   
055200        05 ADGANG-CD-2       PIC S9(3)           COMP-3.                  
055300*                                 GÅNG                                    
055400*                                 AISLE                                   
055500        05 ADPLATS-CD-2      PIC S9(5)           COMP-3.                  
055600*                                 LAGERPLATSNUMMER                        
055700*                                 LOCATION                                
055800        05 ADLAGOMR-CD-3     PIC S9(3)           COMP-3.                  
055900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
056000*                                 AREA ADDRESS CROSS DOCKING WARE         
056100*                                 HOUSE                                   
056200        05 ADGANG-CD-3       PIC S9(3)           COMP-3.                  
056300*                                 GÅNG                                    
056400*                                 AISLE                                   
056500        05 ADPLATS-CD-3      PIC S9(5)           COMP-3.                  
056600*                                 LAGERPLATSNUMMER                        
056700*                                 LOCATION                                
056800        05 ADLAGOMR-CD-4     PIC S9(3)           COMP-3.                  
056900*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
057000*                                 AREA ADDRESS CROSS DOCKING WARE         
057100*                                 HOUSE                                   
057200        05 ADGANG-CD-4       PIC S9(3)           COMP-3.                  
057300*                                 GÅNG                                    
057400*                                 AISLE                                   
057500        05 ADPLATS-CD-4      PIC S9(5)           COMP-3.                  
057600*                                 LAGERPLATSNUMMER                        
057700*                                 LOCATION                                
057800        05 KVLS-CD-1         PIC S9(7)           COMP-3.                  
057900*                                 LAGERSALDO CD                           
058000*                                 STOCK BALANCE CD                        
058100        05 KVLS-CD-2         PIC S9(7)           COMP-3.                  
058200*                                 LAGERSALDO CD                           
058300*                                 STOCK BALANCE CD                        
058400        05 KVLS-CD-3         PIC S9(7)           COMP-3.                  
058500*                                 LAGERSALDO CD                           
058600*                                 STOCK BALANCE CD                        
058700        05 KVLS-CD-4         PIC S9(7)           COMP-3.                  
058800*                                 LAGERSALDO CD                           
058900*                                 STOCK BALANCE CD                        
059000        05 KVRESS-CD-1       PIC S9(7)           COMP-3.                  
059100*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
059200*                                  DOCKING LAGER                          
059300*                                 QUANTITY RESERVED ITEMS IN CROS         
059400*                                 S DOCKING WAREHOUSE                     
059500        05 KVRESS-CD-2       PIC S9(7)           COMP-3.                  
059600*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
059700*                                  DOCKING LAGER                          
059800*                                 QUANTITY RESERVED ITEMS IN CROS         
059900*                                 S DOCKING WAREHOUSE                     
060000        05 KVRESS-CD-3       PIC S9(7)           COMP-3.                  
060100*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
060200*                                  DOCKING LAGER                          
060300*                                 QUANTITY RESERVED ITEMS IN CROS         
060400*                                 S DOCKING WAREHOUSE                     
060500        05 KVRESS-CD-4       PIC S9(7)           COMP-3.                  
060600*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
060700*                                  DOCKING LAGER                          
060800*                                 QUANTITY RESERVED ITEMS IN CROS         
060900*                                 S DOCKING WAREHOUSE                     
061000        05 FILLER            PIC X(22).                                   
061100*** END OF VILMAII-COPY LENGTH= 711 BYTES                                 
