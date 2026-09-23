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
009800        05 FILLERX2          PIC X(2).                                    
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
011400        05 IDPSN-DC          PIC 9(3).                                    
011500*                                 PROPER SHIPPING NAME PER XDC            
011600*                                 PROPER SHIPPING NAME XDC                
011700        05 FILLER            PIC X(3).                                    
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
017200        05 KDAGE             PIC X.                                       
017300*                                 AGE-CODE                                
017400*                                 AGE-CODE                                
017500        05 KDARTHNT          PIC S9(7)           COMP-3.                  
017600*                                 HANTERINGSKOD                           
017700*                                 HANDLING CODE                           
017800        05 KDARTURS          PIC X(2).                                    
017900*                                 ARTIKELURSPRUNGSKOD                     
018000*                                 COUNTRY OF ORIGIN                       
018100        05 KDBPSR            PIC S9              COMP-3.                  
018200*                                 BASLAGERFÖRSLAGSNIVÅ                    
018300*                                 BASIC PART STOCK RECOMMENDATION         
018400        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
018500*                                 EMBALLAGEKOD 0                          
018600        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
018700*                                 EMBALLAGEKOD 1                          
018800        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
018900*                                 EMBALLAGEKOD 2                          
019000        05 KDERS             PIC S9(3)           COMP-3.                  
019100*                                 ERSÄTTNINGSKOD                          
019200*                                 SUPERSESSION CODE                       
019300        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
019400*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
019500*                                 OBSOLETION SUPERSESSION CODE            
019600        05 KDFARLIG          PIC S9              COMP-3.                  
019700*                                 KOD FÖR FARLIGT GODS                    
019800*                                 DANGEROUS GOODS CODE                    
019900        05 KDGK              PIC S9              COMP-3.                  
020000*                                 GODSMOTTAGAREKOD                        
020100*                                 GOODS RECEIVING WAREHOUSE CODE          
020200        05 KDPRODSL          PIC S9(3)           COMP-3.                  
020300*                                 PRODUKTSLAG                             
020400*                                 PRODUCT GROUP                           
020500        05 KDSORT            PIC X(2).                                    
020600*                                 SORT-KOD                                
020700*                                 UNIT OF MEASURE                         
020800        05 KDSPEEMB          PIC 9.                                       
020900*                                 SPECIALEMBALLAGEKOD                     
021000*                                 SPECIAL PACKING CODE                    
021100        05 KDSRA             PIC S9(3)           COMP-3.                  
021200*                                 SRA-KOD                                 
021300*                                 SRA CODE                                
021400        05 KDUART            PIC X.                                       
021500*                                 UNDANTAGSARTIKEL                        
021600*                                 EXECPTION PARTS                         
021700        05 KDVVKL            PIC S9              COMP-3.                  
021800*                                 VOLYMVÄRDESKLASS                        
021900*                                 VOLUME VALUE CLASS                      
022000        05 KDYTBEH           PIC S9(3)           COMP-3.                  
022100*                                 YTBEHANDLINGSKOD                        
022200*                                                                         
022300        05 KVPALL            PIC S9(7)           COMP-3.                  
022400*                                 ANTAL I PALL                            
022500*                                 QUANTITY IN PALLET                      
022600        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
022700*                                 ANTAL I Q0 FÖRPACKNING                  
022800        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
022900*                                 ANTAL I Q1 FÖRPACKNING                  
023000*                                 QUANTITY IN BULK PACK Q1                
023100        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
023200*                                 ANTAL I Q2 FÖRPACKNING                  
023300*                                 QUANTITY IN BULK PACK Q2                
023400        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
023500*                                 ANTAL I Q3 FÖRPACKNING                  
023600*                                 QUANTITY IN BULK PACK Q3                
023700        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
023800*                                 ANTAL I Q4 FÖRPACKNING                  
023900*                                 QUANTITY IN BULK PACK Q4                
024000        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
024100*                                 BESTÄLLNINGSPRIS I KRONOR               
024200*                                 ORDER PRICE SWEDISH CURRENCY            
024300        05 PRARTBTO-EXP      PIC S9(7)V9(2)      COMP-3.                  
024400*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
024500*                                 GROSS-PRICE EXPORT                      
024600*                                  (FOB-GROSS)                            
024700        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
024800*                                 ARTIKELNS SJÄLVKOSTNAD                  
024900*                                 COST OF SALES                           
025000        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
025100*                                 ARTIKELSTANDARDPRIS                     
025200*                                 STANDARD PRICE                          
025300        05 PRINK             PIC S9(7)V9(2)      COMP-3.                  
025400*                                 INKÖPSPRIS                              
025500*                                 PURCHASE PRICE                          
025600        05 TIERSDAT          PIC S9(5)           COMP-3.                  
025700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
025800*                                 DATE OF SUPERSESSION (YYWWD)            
025900        05 TIFINLV           PIC S9(5)           COMP-3.                  
026000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
026100*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
026200        05 VKART             PIC S9(7)           COMP-3.                  
026300*                                 ARTIKELVIKT (G)                         
026400*                                 PART WEIGHT (G)                         
026500        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
026600*                                 ARTIKELVOLYM (CM3)                      
026700*                                 PART VOLUME    (CM3)                    
026800        05 IDKAT-1           PIC X(5).                                    
026900*                                 KATALOGBETECKNING                       
027000        05 IDKAT-2           PIC X(5).                                    
027100*                                 KATALOGBETECKNING                       
027200        05 IDKAT-3           PIC X(5).                                    
027300*                                 KATALOGBETECKNING                       
027400        05 IDINK             PIC X(4).                                    
027500*                                 INKÖPARNUMMER                           
027600*                                 PURCHASE IDENTIFICATION NUMBER          
027700        05 FILLER            PIC X(74).                                   
027800     03 CDC-INFO.                                                         
027900*                                 INFO SOM GÄLLER ENBART CDC              
028000*                                                                         
028100*                                                                         
028200*                                 INFO VALID ONLY FOR CDC                 
028300        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
028400*                                 LAGEROMRÅDE                             
028500*                                 AREA                                    
028600        05 ADGANG            PIC S9(3)           COMP-3.                  
028700*                                 GÅNG                                    
028800*                                 AISLE                                   
028900        05 ADPLATS           PIC S9(5)           COMP-3.                  
029000*                                 LAGERPLATSNUMMER                        
029100*                                 LOCATION                                
029200        05 FLMANAT           PIC X.                                       
029300*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
029400        05 FLMANBK           PIC X.                                       
029500*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
029600        05 FLMANGK           PIC X.                                       
029700*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
029800        05 FLMANKP           PIC X.                                       
029900*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
030000        05 FLMANLT           PIC X.                                       
030100*                                 MANUELLT SATT LEDTID ?                  
030200        05 FLMANOSK          PIC X.                                       
030300*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
030400*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
030500        05 FLMANQ            PIC X.                                       
030600*                                 MANUELL HEMTAGNINGSKVANTITET            
030700        05 FLMPB             PIC X.                                       
030800*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
030900        05 FLREFILL          PIC X.                                       
031000*                                 REFILLARTIKEL                           
031100*                                 REFILLPART                              
031200        05 FLSKROT-BEORD     PIC X.                                       
031300*                                 SKROTNING BEORDRAD AV ANSK              
031400*                                 SCRAPPING ORDERED BY PROCURER           
031500        05 FLTOPP            PIC X.                                       
031600*                                 TOPP-200-ARTIKEL                        
031700*                                 TOP 200 PART                            
031800        05 IDAVINR-SEN       PIC S9(7)           COMP-3.                  
031900*                                 AVINUMMER SENASTE INLEVERANS            
032000        05 IDLEVNR-SEN       PIC X(5).                                    
032100*                                 SENASTE LEVERANTÖR                      
032200        05 IDPLANGR-AG       PIC S9              COMP-3.                  
032300*                                 PLANERINGSGRUPP ANSKAFFARE              
032400        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
032500*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
032600        05 IDPROENH-1        PIC X(8).                                    
032700*                                 PRODUKTIONSENHET                        
032800*                                 PRODUCTION UNIT                         
032900        05 IDPROENH-2        PIC X(8).                                    
033000*                                 PRODUKTIONSENHET                        
033100*                                 PRODUCTION UNIT                         
033200        05 IDPROENH-3        PIC X(8).                                    
033300*                                 PRODUKTIONSENHET                        
033400*                                 PRODUCTION UNIT                         
033500        05 IDPROJUP          PIC X(8).                                    
033600*                                 PROJEKTUPPDRAG                          
033700*                                 PROJECT ASSIGNMENT                      
033800        05 IDRITN            PIC X(10).                                   
033900*                                 RITNINGSNUMMER                          
034000*                                 DRAWING NUMBER                          
034100        05 KDAVT             PIC S9              COMP-3.                  
034200*                                 AVTALSMÄRKNING                          
034300*                                 AGREEMENT CODE                          
034400        05 KDFORP.                                                        
034500*                                 FÖRPACKNINGSKOD                         
034600*                                 PACKAGING CODE                          
034700           07 KDFORPPL       PIC 9.                                       
034800*                                 FÖRPACKNINGSPLATS                       
034900*                                 PREPACKING PLACE                        
035000           07 KDFORPGP       PIC 9(2).                                    
035100*                                 FÖRPACKNINGSGRUPP                       
035200*                                 PREPACKING GROUP                        
035300           07 KDFORPUF       PIC 9.                                       
035400*                                 UPPRÄKNINGSFAKTOR                       
035500*                                 ENUMERATION                             
035600        05 KDFREKKL          PIC X.                                       
035700*                                 FREKVENSKLASS                           
035800*                                 FREQ. CLASS                             
035900        05 KDHF              PIC S9              COMP-3.                  
036000*                                 HUVUDFÖRRÅDSMÄRKNING                    
036100*                                 CODE MAIN STORAGE                       
036200        05 KDKG              PIC S9              COMP-3.                  
036300*                                 KURANSGRUPP                             
036400*                                 TURNOVER CODE                           
036500        05 KDKSP             PIC S9              COMP-3.                  
036600*                                 KÖPSPÄRR                                
036700*                                 PURCHASE BLOCKING CODE                  
036800        05 KDLEVSP           PIC S9(3)           COMP-3.                  
036900*                                 SPÄRRKOD LEVERANS                       
037000*                                 DELIVERY BLOCKING CODE                  
037100        05 KDLPSP            PIC S9              COMP-3.                  
037200*                                 LEVERANSPLANESPÄRR                      
037300        05 KDPRISKL          PIC X.                                       
037400*                                 PRISKLASS                               
037500*                                 PRICE CLASS                             
037600        05 KDTIPPR           PIC S9              COMP-3.                  
037700*                                 TIPPAT PRIS KOD                         
037800*                                 ESTIMATED PRICE CODE                    
037900        05 KDVSOP            PIC S9(3)           COMP-3.                  
038000*                                 VSOP-KOD                                
038100*                                 VSOP-CODE                               
038200        05 KDVTH             PIC S9              COMP-3.                  
038300*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
038400*                                 CODE FOR COST RESPONSIBILITY            
038500        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
038600*                                 DEL AV AK SOM LIGGER I CDC              
038700*                                 PART OF AK IN THE CDC                   
038800        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
038900*                                 DEL AV AK PÅ VÄG                        
039000*                                 PART OF AK ON ITS WAY                   
039100        05 KVAKS-T           PIC S9(7)           COMP-3.                  
039200*                                 DEL AV AK I EN TERMINAL                 
039300*                                 PART OF AK IN A TERMINAL                
039400        05 KVAP              PIC S9(7)           COMP-3.                  
039500*                                 ANNULLATIONSPUNKT                       
039600        05 KVAVIS-SEN        PIC S9(7)           COMP-3.                  
039700*                                 SENAST AVISERAT ANTAL                   
039800        05 KVBK              PIC S9(7)           COMP-3.                  
039900*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
040000        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
040100*                                 TOT BEST REST                           
040200        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
040300*                                 INLEVERANSTID     (ANTAL DAGAR)         
040400        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
040500*                                 DAGAR TULL- OCH TRANSPORT-TID           
040600        05 KVEFRS            PIC S9(7)           COMP-3.                  
040700*                                 EJ FAKTURERAT ANTAL STYCK               
040800*                                 ORDERED NOT INVOICED QTY                
040900        05 KVFRYSTI          PIC S9(3)           COMP-3.                  
041000*                                 FRYSTID FÖR TPO-ORDER                   
041100*                                 FREEZTIME FOR TPO                       
041200        05 KVINVS            PIC S9(7)           COMP-3.                  
041300*                                 INVENTERINGSSALDO                       
041400*                                 STOCK-TAKING BALANCE                    
041500        05 KVKP              PIC S9(7)           COMP-3.                  
041600*                                 KÖPPUNKT                                
041700        05 KVLAAN            PIC S9(7)           COMP-3.                  
041800*                                 LÅNESALDO                               
041900        05 KVLS              PIC S9(7)           COMP-3.                  
042000*                                 LAGERSALDO                              
042100*                                 STOCK BALANCE                           
042200        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
042300*                                 SEPARAT PROGNOSFEL                      
042400        05 KVMAD-TOT         PIC S9(6)V9(1)      COMP-3.                  
042500*                                 TOTALT PROGNOSFEL                       
042600        05 KVMP              PIC S9(7)           COMP-3.                  
042700*                                 MAXPUNKT                                
042800*                                 MAXIMUM POINT                           
042900        05 KVOKS             PIC S9(7)           COMP-3.                  
043000*                                 ORDERKÖSALDO                            
043100*                                 ORDER QUEUE BALANCE                     
043200        05 KVOVERF           PIC S9(7)           COMP-3.                  
043300*                                 ÖVERFÖRINGSSALDO                        
043400        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
043500*                                 SATS-PERIODBEHOV                        
043600*                                 KIT PERIOD REQUIREMENTS                 
043700        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
043800*                                 SEPARAT PERIODBEHOV                     
043900*                                 SEPARATE PERIOD REQUIREMENTS            
044000        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
044100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
044200*                                 PERIODICAL DEMAND TPO1 AND TPO2         
044300*                                                                         
044400        05 KVPB-NDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
044500*                                 PB-NDC TOTALT FÖR SAMLTLIGA NDC         
044600*                                 :ER                                     
044700        05 KVPB-SDC-TOT      PIC S9(6)V9(1)      COMP-3.                  
044800*                                 PB-SDC TOTALT FÖR SAMLTLIGA SDC         
044900*                                 :ER                                     
045000        05 KVPREAVB-BULK     PIC S9(7)           COMP-3.                  
045100*                                 PREL-AVB KVANT, KLASS 2-4               
045200*                                 PREL-RES QUANT, CLASS 2-4               
045300        05 KVPREAVB-DAG      PIC S9(7)           COMP-3.                  
045400*                                 PREL-AVB KVANT, KLASS 1                 
045500*                                 PREL-RES QUANT, CLASS 1                 
045600        05 KVPREAVB-VOR      PIC S9(7)           COMP-3.                  
045700*                                 PREL-AVB KVANT, VOR                     
045800*                                 PREL-RES QUANT, VOR                     
045900        05 KVPRERO-BULK      PIC S9(7)           COMP-3.                  
046000*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
046100*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
046200        05 KVPRERO-DAG       PIC S9(7)           COMP-3.                  
046300*                                 PRELIMINÄR RO-KVANT, KLASS 1            
046400*                                 PRELIMINARY BO-QUANT, CLASS 1           
046500        05 KVQ               PIC S9(7)           COMP-3.                  
046600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
046700        05 KVRESS            PIC S9(7)           COMP-3.                  
046800*                                 RESERVERAT ANTAL ARTIKLAR               
046900*                                 QUANTITY RESERVED ITEMS                 
047000        05 KVROS             PIC S9(7)           COMP-3.                  
047100*                                 RESTORDERSALDO                          
047200*                                 BACKORDER QTY                           
047300        05 KVSLAGER          PIC S9(7)           COMP-3.                  
047400*                                 SÄKERHETSLAGER                          
047500*                                 SAFETY STOCK                            
047600        05 KVSLUTKP          PIC S9(7)           COMP-3.                  
047700*                                 SLUTKÖPSSALDO                           
047800        05 KVSPANT           PIC S9(7)           COMP-3.                  
047900*                                 SPÄRRAT ANTAL                           
048000*                                 BLOCKED QTY                             
048100        05 KVUTRS            PIC S9(7)           COMP-3.                  
048200*                                 UTREDNINGSSALDO                         
048300*                                 INVESTIGATION BALANCE                   
048400        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
048500*                                 ANTAL VECKOR ANSKAFFNINGSTID            
048600        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
048700*                                 ANTAL VECKOR BESTÄLLNINGSTID            
048800        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
048900*                                 ANTAL VECKOR FRYSNINGSTID               
049000        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
049100*                                 ANTAL VECKOR LEDTID                     
049200        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
049300*                                 DIREKT LÖN                              
049400*                                 SURCHARGE COSTS                         
049500        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
049600*                                 DIREKT MATERIAL                         
049700*                                 SURCHARGE PACKING MATERIAL              
049800        05 PRLFKST           PIC S9(3)V9(2)      COMP-3.                  
049900*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
050000*                                 SUPPLIERS PACKING AND HANDLING          
050100        05 PRORDSK           PIC S9(5)V9(2)      COMP-3.                  
050200*                                 ORDERSÄRKOSTNAD                         
050300*                                 REMAINING OVERHEAD SURCHARGE            
050400        05 PROVRPAL          PIC S9(4)V9(3)      COMP-3.                  
050500*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
050600*                                 REMAINING OVERHEAD SURCHARGE            
050700        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
050800*                                 DIREKTLEVERANSANDEL                     
050900        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
051000*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
051100*                                 ADJUSTMENT ALGORITM                     
051200        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
051300*                                 TPO-KVANTITET, TOTAL                    
051400*                                 TPO-QUANTITY, TOTAL                     
051500        05 TIAVIDAT-SEN      PIC S9(7)           COMP-3.                  
051600*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
051700        05 TIINVDAT          PIC S9(5)           COMP-3.                  
051800*                                 INVENTERINGSDATUM                       
051900*                                 STOCKTAKING DATE                        
052000        05 TILPSP            PIC S9(5)           COMP-3.                  
052100*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
052200        05 TIREGDAT          PIC S9(7)           COMP-3.                  
052300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
052400*                                 REGISTRATION DATE (YYMMDD)              
052500        05 TISLJUST          PIC S9(5)           COMP-3.                  
052600*                                 VECKA DÅ JUSTERING AV SÄKER-            
052700*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
052800        05 TIURPROD          PIC S9(5)           COMP-3.                  
052900*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
053000*                                 OUT OF PRODUCTION DATE (YYWW)           
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
060900        05 KDOTFREK          PIC X.                                       
061000*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
061100*                                 ORDER HIT FREQUENCY FOR PART            
061200        05 FILLER            PIC X(21).                                   
061300*** END OF VILMAII-COPY LENGTH= 711 BYTES                                 
