000100 01  CLAG-W01160.                                                         
000200*                                 UTDRAG UR WDK601 OCH                    
000300*                                 WDK611                                  
000400*                                 OBS IDFS-SEN X(8) WDK611                
000500*                                 ERSÄTTS PÅ W01160 AV                    
000600*                                 IDAVINR-SEN S9(7)                       
000700*                                                                         
000800     03 CLAG-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 CLAG-FLERS           PIC X.                                       
001200*                                 TILLKOMMANDE ARTIKEL ?                  
001300     03 CLAG-FLIART          PIC X.                                       
001400*                                 ARTIKELN INGÅR I SATS                   
001500*                                 PART IN KIT                             
001600     03 CLAG-IDAO            OCCURS 5 TIMES                               
001700                             PIC X(10).                                   
001800*                                 ÄNDRINGSORDERNUMMER                     
001900*                                 DESIGN CHANGE NOTICE                    
002000     03 CLAG-IDFKNGRP        PIC S9(5)           COMP-3.                  
002100*                                 FUNKTIONSGRUPP                          
002200*                                 FUNCTION GROUP                          
002300     03 CLAG-IDFTG           PIC 9(2).                                    
002400*                                 FÖRETAGSID EKONOM REDOVISNING           
002500*                                 COMPANY IDENTITY ACCOUNTING             
002600     03 CLAG-IDLEVNR         PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 CLAG-KDERS-UTG       PIC S9(3)           COMP-3.                  
003000*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003100*                                 OBSOLETION SUPERSESSION CODE            
003200     03 CLAG-KDPRODSL        PIC S9(3)           COMP-3.                  
003300*                                 PRODUKTSLAG                             
003400*                                 PRODUCT GROUP                           
003500     03 CLAG-KDSORT          PIC X(2).                                    
003600*                                 SORT-KOD                                
003700*                                 UNIT OF MEASURE                         
003800     03 CLAG-REKSIFFR        PIC S9              COMP-3.                  
003900*                                 KONTROLLSIFFRA                          
004000*                                 PART NO CHECK DIGIT                     
004100     03 CLAG-TIERSDAT        PIC S9(5)           COMP-3.                  
004200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
004300*                                 DATE OF SUPERSESSION (YYWWD)            
004400     03 CLAG-TIFINLV         PIC S9(5)           COMP-3.                  
004500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004600*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004700     03 CLAG-TIREGDAT        PIC S9(7)           COMP-3.                  
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900*                                 REGISTRATION DATE (YYMMDD)              
005000     03 CLAG-ADART.                                                       
005100*                                 ARTIKELADRESS I LAGRET                  
005200*                                 PARTS-ADRESS                            
005300        05 CLAG-ADLAGOMR     PIC S9(3)           COMP-3.                  
005400*                                 LAGEROMRÅDE                             
005500*                                 AREA                                    
005600        05 CLAG-ADGANG       PIC S9(3)           COMP-3.                  
005700*                                 GÅNG                                    
005800*                                 AISLE                                   
005900        05 CLAG-ADPLATS      PIC S9(5)           COMP-3.                  
006000*                                 LAGERPLATSNUMMER                        
006100*                                 LOCATION                                
006200     03 CLAG-ADART-SVS.                                                   
006300*                                 ARTIKELADRESS I SVS-LAGRET              
006400*                                 PARTS-ADRESS IN SVS WAREHOUSE           
006500        05 CLAG-ADLAGOMR-SVS PIC S9(3)           COMP-3.                  
006600*                                 LAGEROMRÅDE                             
006700*                                 AREA                                    
006800        05 CLAG-ADGANG-SVS   PIC S9(3)           COMP-3.                  
006900*                                 GÅNG                                    
007000*                                 AISLE                                   
007100        05 CLAG-ADPLATS-SVS  PIC S9(5)           COMP-3.                  
007200*                                 LAGERPLATSNUMMER                        
007300*                                 LOCATION                                
007400     03 CLAG-ADINPORT        PIC X(8).                                    
007500*                                 AVLASNINGSPORT                          
007600*                                 LOADING GATE                            
007700     03 CLAG-BEFT            PIC S9(3)           COMP-3.                  
007800*                                 FÖRPACKNINGSTYP                         
007900*                                 PACKAGING TYPE                          
008000     03 CLAG-DAPUBL-US       PIC 9(8).                                    
008100*                                 PUBLICERINGSDATUM PER ART/DC US         
008200*                                 A                                       
008300*                                 DATE OF PUBLISHING PER PART/DC          
008400*                                 US                                      
008500     03 CLAG-DAXPOINT        PIC 9(8).                                    
008600*                                 POÄNGÄNDRINGSDATUM (ÅÅÅÅMMDD)           
008700*                                 POINT CHANGE DATE (YYYYMMDD)            
008800     03 CLAG-FLAVRART        PIC X.                                       
008900*                                 AVROPSARTIKEL                           
009000     03 CLAG-FLFSP           PIC X.                                       
009100*                                 FÖRDELNINGSSPÄRR                        
009200*                                 BLOCKED FOR SPLIT                       
009300     03 CLAG-FLGEMART        PIC X.                                       
009400*                                 FLAGGA GEMENSAM ARTIKEL                 
009500*                                 COMMON PART FLAG                        
009600     03 CLAG-FLJIT           PIC X.                                       
009700*                                 JUST-IN-TIME FLAGGA                     
009800*                                 JUST-IN-TIME FLAG                       
009900     03 CLAG-FLLARM-BUF      PIC X.                                       
010000*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
010100     03 CLAG-FLLSRDEL        PIC X.                                       
010200*                                 LEVERERAS SOM RESDEL                    
010300     03 CLAG-FLLTKSP         PIC X.                                       
010400*                                 SPÄRR UTLEVERANS C2-LAGER               
010500     03 CLAG-FLMANAT         PIC X.                                       
010600*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
010700     03 CLAG-FLMANBK         PIC X.                                       
010800*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
010900     03 CLAG-FLMANGK         PIC X.                                       
011000*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
011100     03 CLAG-FLMANKP         PIC X.                                       
011200*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
011300     03 CLAG-FLMANLT         PIC X.                                       
011400*                                 MANUELLT SATT LEDTID ?                  
011500     03 CLAG-FLMANOSK        PIC X.                                       
011600*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
011700*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
011800     03 CLAG-FLMANPB         PIC X.                                       
011900*                                 MANUELLT REGISTRERAT PB-TPO             
012000*                                 MANUALLY REGISTRATED PB-TPO             
012100     03 CLAG-FLMANQ          PIC X.                                       
012200*                                 MANUELL HEMTAGNINGSKVANTITET            
012300     03 CLAG-FLMANOPP        PIC X.                                       
012400*                                 MANUELLT SATT GODK. AV OP-PLAN?         
012500*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
012600     03 CLAG-FLMARKSP        PIC X.                                       
012700*                                 MARKNADSSPÄRR                           
012800*                                 MARKET BLOCKING CODE                    
012900     03 CLAG-FLMPB           PIC X.                                       
013000*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
013100     03 CLAG-KDOPPLAN        PIC X.                                       
013200*                                 OPTIMAL PLAN INOM FRYSTID               
013300*                                 OPTIMAL PLAN WITHIN FREEZTIME           
013400     03 CLAG-FLOREGPB        PIC X.                                       
013500*                                 OREGELBUNDEN PROGNOS (PB) ?             
013600     03 CLAG-FLRADREF        PIC X.                                       
013700*                                 KOMPLETTERANDE INFO. KRÄVS              
013800*                                 ADDITIONAL INFORMATION REQUIRED         
013900     03 CLAG-FLREFILL        PIC X.                                       
014000*                                 REFILLARTIKEL                           
014100*                                 REFILLPART                              
014200     03 CLAG-FLRELSP         PIC X.                                       
014300*                                 RELEASEBLOCKAD ARTIKEL .                
014400*                                 BLOCKED PART                            
014500     03 CLAG-FLSKROT-BEORD   PIC X.                                       
014600*                                 SKROTNING BEORDRAD AV ANSK              
014700*                                 SCRAPPING ORDERED BY PROCURER           
014800     03 CLAG-FLSPKOST        PIC X.                                       
014900*                                 SPECIELLA KOSTNADER FINS                
015000*                                 SPECIAL COSTS EXIST                     
015100     03 CLAG-FLTOPP          PIC X.                                       
015200*                                 TOPP-200-ARTIKEL                        
015300*                                 TOP 200 PART                            
015400     03 CLAG-FLTPO1          PIC X.                                       
015500*                                 ARTIKELN GODKÄND FÖR TPO1               
015600*                                 TPO1 ALLOWED FOR ARTICLE                
015700     03 CLAG-IDANSK          PIC S9(3)           COMP-3.                  
015800*                                 ANSKAFFARNUMMER                         
015900*                                 PROCURER NO.                            
016000     03 CLAG-IDARTNR-EMBQ0   PIC S9(9)           COMP-3.                  
016100*                                 EMBALLAGEARTIKELNR FÖR Q0               
016200     03 CLAG-IDARTNR-EMBQ1   PIC S9(9)           COMP-3.                  
016300*                                 EMBALLAGEARTIKELNR FÖR Q1               
016400     03 CLAG-IDARTNR-EMBQ2   PIC S9(9)           COMP-3.                  
016500*                                 EMBALLAGEARTIKELNR FÖR Q2               
016600     03 CLAG-IDARTNR-EMBQ3   PIC S9(9)           COMP-3.                  
016700*                                 EMBALLAGEARTIKELNR FÖR Q3               
016800     03 CLAG-IDARTNR-EMBQ4   PIC S9(9)           COMP-3.                  
016900*                                 EMBALLAGEARTIKELNR FÖR Q4               
017000     03 CLAG-IDBERED         PIC S9(3)           COMP-3.                  
017100*                                 BEREDARENUMMER                          
017200     03 CLAG-IDAVINR-SEN     PIC S9(7)           COMP-3.                  
017300*                                 AVINUMMER SENASTE INLEVERANS            
017400     03 CLAG-IDINK           PIC X(4).                                    
017500*                                 INKÖPARNUMMER                           
017600*                                 PURCHASE IDENTIFICATION NUMBER          
017700     03 CLAG-IDKAT           OCCURS 3 TIMES                               
017800                             PIC X(5).                                    
017900*                                 KATALOGBETECKNING                       
018000     03 CLAG-IDLEVNR-SEN     PIC X(5).                                    
018100*                                 SENASTE LEVERANTÖR                      
018200     03 CLAG-IDLKTO          PIC S9(7)           COMP-3.                  
018300*                                 LAGERKONTO (FFHHHUU)                    
018400*                                 STOCK ACCOUNT (CCMMMSS)                 
018500     03 CLAG-IDPERSON-BUYNA  PIC S9(3)           COMP-3.                  
018600*                                 PERSONKOD REFILLANSVARIG                
018700*                                 REFILL RESPONSIBLE ID                   
018800     03 CLAG-IDPLANGR-AG     PIC S9              COMP-3.                  
018900*                                 PLANERINGSGRUPP ANSKAFFARE              
019000     03 CLAG-IDPLANGR-LEV    PIC S9              COMP-3.                  
019100*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
019200     03 CLAG-IDPROENH        OCCURS 3 TIMES                               
019300                             PIC X(8).                                    
019400*                                 PRODUKTIONSENHET                        
019500*                                 PRODUCTION UNIT                         
019600     03 CLAG-IDPROJ          PIC X(4).                                    
019700*                                 PARTS PROJEKTIDENTITET                  
019800*                                 PARTS PROJECT IDENTITY                  
019900     03 CLAG-IDPROJUP        PIC X(8).                                    
020000*                                 PROJEKTUPPDRAG                          
020100*                                 PROJECT ASSIGNMENT                      
020200     03 CLAG-IDPSN           PIC 9(3).                                    
020300*                                 PROPER SHIPPING NAME                    
020400*                                 PROPER SHIPPING NAME                    
020500     03 CLAG-IDPSN-US        PIC 9(3).                                    
020600*                                 PROPER SHIPPING NAME USA                
020700*                                 PROPER SHIPPING NAME USA                
020800     03 CLAG-IDRITN          PIC X(10).                                   
020900*                                 RITNINGSNUMMER                          
021000*                                 DRAWING NUMBER                          
021100     03 CLAG-IDSTATNR        OCCURS 6 TIMES                               
021200                             PIC S9(9)           COMP-3.                  
021300*                                 STATISTISKT NUMMER                      
021400*                                 1 = NORSKT                              
021500*                                 2 = ENGELSKT                            
021600*                                 3 = BELGISKT                            
021700*                                 4 = PERUANSKT                           
021800*                                 5 = SVENSKT                             
021900*                                 6 =                                     
022000*                                 STATISTICAL NO.                         
022100     03 CLAG-IDUSER-SPKVAL   PIC X(8).                                    
022200*                                 ANVÄNDAR-ID KVALITETSPÄRR               
022300*                                 USER ID QUALITY ERROR                   
022400     03 CLAG-KDAGE           PIC X.                                       
022500*                                 AGE-CODE                                
022600*                                 AGE-CODE                                
022700     03 CLAG-KDARTHNT        PIC S9(7)           COMP-3.                  
022800*                                 HANTERINGSKOD                           
022900*                                 HANDLING CODE                           
023000     03 CLAG-KDARTURS        PIC X(2).                                    
023100*                                 ARTIKELURSPRUNGSKOD                     
023200*                                 COUNTRY OF ORIGIN                       
023300     03 CLAG-KDAVT           PIC S9              COMP-3.                  
023400*                                 AVTALSMÄRKNING                          
023500*                                 AGREEMENT CODE                          
023600     03 CLAG-KDBPSR          PIC S9              COMP-3.                  
023700*                                 BASLAGERFÖRSLAGSNIVÅ                    
023800*                                 BASIC PART STOCK RECOMMENDATION         
023900     03 CLAG-KDEFFMAN        PIC X.                                       
024000*                                 EMIL-KOD                                
024100*                                 EMIL-CODE                               
024200     03 CLAG-KDEMBKOD-0      PIC S9(3)           COMP-3.                  
024300*                                 EMBALLAGEKOD 0                          
024400     03 CLAG-KDEMBKOD-1      PIC S9(3)           COMP-3.                  
024500*                                 EMBALLAGEKOD 1                          
024600     03 CLAG-KDEMBKOD-2      PIC S9(3)           COMP-3.                  
024700*                                 EMBALLAGEKOD 2                          
024800     03 CLAG-KDERS           PIC S9(3)           COMP-3.                  
024900*                                 ERSÄTTNINGSKOD                          
025000*                                 SUPERSESSION CODE                       
025100     03 CLAG-KDEXCHA         PIC S9(3)           COMP-3.                  
025200*                                 EXCHANGE ACCOUNT CODE                   
025300     03 CLAG-KDFARLIG        PIC S9              COMP-3.                  
025400*                                 KOD FÖR FARLIGT GODS                    
025500*                                 DANGEROUS GOODS CODE                    
025600     03 CLAG-KDFORP.                                                      
025700*                                 FÖRPACKNINGSKOD                         
025800*                                 PACKAGING CODE                          
025900        05 CLAG-KDFORPPL     PIC 9.                                       
026000*                                 FÖRPACKNINGSPLATS                       
026100*                                 PREPACKING PLACE                        
026200        05 CLAG-KDFORPGP     PIC 9(2).                                    
026300*                                 FÖRPACKNINGSGRUPP                       
026400*                                 PREPACKING GROUP                        
026500        05 CLAG-KDFORPUF     PIC 9.                                       
026600*                                 UPPRÄKNINGSFAKTOR                       
026700*                                 ENUMERATION                             
026800     03 CLAG-KDFREKKL        PIC X.                                       
026900*                                 FREKVENSKLASS                           
027000*                                 FREQ. CLASS                             
027100     03 CLAG-KDGK            PIC S9              COMP-3.                  
027200*                                 GODSMOTTAGAREKOD                        
027300*                                 GOODS RECEIVING WAREHOUSE CODE          
027400     03 CLAG-KDHF            PIC S9              COMP-3.                  
027500*                                 HUVUDFÖRRÅDSMÄRKNING                    
027600*                                 CODE MAIN STORAGE                       
027700     03 CLAG-KDKG            PIC S9              COMP-3.                  
027800*                                 KURANSGRUPP                             
027900*                                 TURNOVER CODE                           
028000     03 CLAG-KDKSP           PIC S9              COMP-3.                  
028100*                                 KÖPSPÄRR                                
028200*                                 PURCHASE BLOCKING CODE                  
028300     03 CLAG-KDLEVPLF        PIC X.                                       
028400*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
028500*                                 CODE FOR APPROVAL OF SCHEDULE P         
028600*                                 ROPOSAL                                 
028700     03 CLAG-KDLEVSP         PIC S9(3)           COMP-3.                  
028800*                                 SPÄRRKOD LEVERANS                       
028900*                                 DELIVERY BLOCKING CODE                  
029000     03 CLAG-KDLPSP          PIC S9              COMP-3.                  
029100*                                 LEVERANSPLANESPÄRR                      
029200     03 CLAG-KDLTK           PIC S9              COMP-3.                  
029300*                                 LAGERTILLHÖRIGHETSKOD                   
029400*                                 STOCK BELONGING CODE                    
029500     03 CLAG-KDPRISKL        PIC X.                                       
029600*                                 PRISKLASS                               
029700*                                 PRICE CLASS                             
029800     03 CLAG-KDPSLLOC        PIC 9(2).                                    
029900*                                 PRODUKTSLAG LOKALT                      
030000*                                 PRODUCT GROUP LOCAL                     
030100     03 CLAG-KDSPEEMB        PIC 9.                                       
030200*                                 SPECIALEMBALLAGEKOD                     
030300*                                 SPECIAL PACKING CODE                    
030400     03 CLAG-KDSRA           PIC S9(3)           COMP-3.                  
030500*                                 SRA-KOD                                 
030600*                                 SRA CODE                                
030700     03 CLAG-KDTIPPR         PIC S9              COMP-3.                  
030800*                                 TIPPAT PRIS KOD                         
030900*                                 ESTIMATED PRICE CODE                    
031000     03 CLAG-KDTULLRE        PIC S9              COMP-3.                  
031100*                                 TULLRESTITUTION MÄRKNING                
031200*                                 CUSTOMS RESTITUAT.                      
031300     03 CLAG-KDUART          PIC X.                                       
031400*                                 UNDANTAGSARTIKEL                        
031500*                                 EXECPTION PARTS                         
031600     03 CLAG-KDVTH           PIC S9              COMP-3.                  
031700*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
031800*                                 CODE FOR COST RESPONSIBILITY            
031900     03 CLAG-KDVVKL          PIC S9              COMP-3.                  
032000*                                 VOLYMVÄRDESKLASS                        
032100*                                 VOLUME VALUE CLASS                      
032200     03 CLAG-KDVSOP          PIC S9(3)           COMP-3.                  
032300*                                 VSOP-KOD                                
032400*                                 VSOP-CODE                               
032500     03 CLAG-KDYTBEH         PIC S9(3)           COMP-3.                  
032600*                                 YTBEHANDLINGSKOD                        
032700*                                                                         
032800     03 CLAG-KVAKS-CDC       PIC S9(7)           COMP-3.                  
032900*                                 DEL AV AK SOM LIGGER I CDC              
033000*                                 PART OF AK IN THE CDC                   
033100     03 CLAG-KVAKS-PAV       PIC S9(7)           COMP-3.                  
033200*                                 DEL AV AK PÅ VÄG                        
033300*                                 PART OF AK ON ITS WAY                   
033400     03 CLAG-KVAKS-T         PIC S9(7)           COMP-3.                  
033500*                                 DEL AV AK I EN TERMINAL                 
033600*                                 PART OF AK IN A TERMINAL                
033700     03 CLAG-KVAP            PIC S9(7)           COMP-3.                  
033800*                                 ANNULLATIONSPUNKT                       
033900     03 CLAG-KVAVIS-SEN      PIC S9(7)           COMP-3.                  
034000*                                 SENAST AVISERAT ANTAL                   
034100     03 CLAG-KVBK            PIC S9(7)           COMP-3.                  
034200*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
034300     03 CLAG-KVDAGAR-FFH     PIC S9(3)           COMP-3.                  
034400*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
034500     03 CLAG-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
034600*                                 INLEVERANSTID     (ANTAL DAGAR)         
034700     03 CLAG-KVDAGAR-TT      PIC S9(3)           COMP-3.                  
034800*                                 DAGAR TULL- OCH TRANSPORT-TID           
034900     03 CLAG-KVEFRS          PIC S9(7)           COMP-3.                  
035000*                                 EJ FAKTURERAT ANTAL STYCK               
035100*                                 ORDERED NOT INVOICED QTY                
035200     03 CLAG-KVFRYSTI        PIC S9(3)           COMP-3.                  
035300*                                 FRYSTID FÖR TPO-ORDER                   
035400*                                 FREEZTIME FOR TPO                       
035500     03 CLAG-KVINVS          PIC S9(7)           COMP-3.                  
035600*                                 INVENTERINGSSALDO                       
035700*                                 STOCK-TAKING BALANCE                    
035800     03 CLAG-KVKP            PIC S9(7)           COMP-3.                  
035900*                                 KÖPPUNKT                                
036000     03 CLAG-KVLAAN          PIC S9(7)           COMP-3.                  
036100*                                 LÅNESALDO                               
036200     03 CLAG-KVLS            PIC S9(7)           COMP-3.                  
036300*                                 LAGERSALDO                              
036400*                                 STOCK BALANCE                           
036500     03 CLAG-KVLS-SVS        PIC S9(7)           COMP-3.                  
036600*                                 LAGERSALDO SVS                          
036700*                                 STOCK BALANCE SVS                       
036800     03 CLAG-KVMAD-SEP       PIC S9(6)V9(1)      COMP-3.                  
036900*                                 SEPARAT PROGNOSFEL                      
037000     03 CLAG-KVMAD-TOT       PIC S9(6)V9(1)      COMP-3.                  
037100*                                 TOTALT PROGNOSFEL                       
037200     03 CLAG-KVMP            PIC S9(7)           COMP-3.                  
037300*                                 MAXPUNKT                                
037400*                                 MAXIMUM POINT                           
037500     03 CLAG-KVOVERF         PIC S9(7)           COMP-3.                  
037600*                                 ÖVERFÖRINGSSALDO                        
037700     03 CLAG-KVPALL          PIC S9(7)           COMP-3.                  
037800*                                 ANTAL I PALL                            
037900*                                 QUANTITY IN PALLET                      
038000     03 CLAG-KVPB-HIST       PIC S9(6)V9(1)      COMP-3.                  
038100*                                 PB (PROGNOS) HISTORISKT CDC             
038200*                                 HISTORIC REQUIREMENTS CDC               
038300     03 CLAG-KVPB-SATS       PIC S9(6)V9(1)      COMP-3.                  
038400*                                 SATS-PERIODBEHOV                        
038500*                                 KIT PERIOD REQUIREMENTS                 
038600     03 CLAG-KVPB-SEP        PIC S9(6)V9(1)      COMP-3.                  
038700*                                 SEPARAT PERIODBEHOV                     
038800*                                 SEPARATE PERIOD REQUIREMENTS            
038900     03 CLAG-KVPB-TPO        PIC S9(6)V9(1)      COMP-3.                  
039000*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
039100*                                 PERIODICAL DEMAND TPO1 AND TPO2         
039200*                                                                         
039300     03 CLAG-KVPB-VESL       PIC S9(6)V9(1)      COMP-3.                  
039400*                                 GÄLLANDE PB VID VECKOSLUT               
039500     03 CLAG-KVPOINT         PIC S9(7)           COMP-3.                  
039600*                                 POINT VALUE                             
039700     03 CLAG-KVQ             PIC S9(7)           COMP-3.                  
039800*                                 EKONOMISK HEMTAGNINGSKVANTITET          
039900     03 CLAG-KVQ-JUST        PIC S9(7)           COMP-3.                  
040000*                                 NY EKON HEMTAGNINGSKVANTITET            
040100     03 CLAG-KVQPACK-0       PIC S9(5)           COMP-3.                  
040200*                                 ANTAL I Q0 FÖRPACKNING                  
040300     03 CLAG-KVQPACK-1       PIC S9(5)           COMP-3.                  
040400*                                 ANTAL I Q1 FÖRPACKNING                  
040500*                                 QUANTITY IN BULK PACK Q1                
040600     03 CLAG-KVQPACK-2       PIC S9(5)           COMP-3.                  
040700*                                 ANTAL I Q2 FÖRPACKNING                  
040800*                                 QUANTITY IN BULK PACK Q2                
040900     03 CLAG-KVQPACK-3       PIC S9(5)           COMP-3.                  
041000*                                 ANTAL I Q3 FÖRPACKNING                  
041100*                                 QUANTITY IN BULK PACK Q3                
041200     03 CLAG-KVQPACK-4       PIC S9(5)           COMP-3.                  
041300*                                 ANTAL I Q4 FÖRPACKNING                  
041400*                                 QUANTITY IN BULK PACK Q4                
041500     03 CLAG-KVRESS          PIC S9(7)           COMP-3.                  
041600*                                 RESERVERAT ANTAL ARTIKLAR               
041700*                                 QUANTITY RESERVED ITEMS                 
041800     03 CLAG-KVRETUR         PIC S9(7)           COMP-3.                  
041900*                                 ANTAL I RETUR                           
042000*                                 QUANTITY IN RETURN                      
042100     03 CLAG-KVROS           PIC S9(7)           COMP-3.                  
042200*                                 RESTORDERSALDO                          
042300*                                 BACKORDER QTY                           
042400     03 CLAG-KVSLAGER        PIC S9(7)           COMP-3.                  
042500*                                 SÄKERHETSLAGER                          
042600*                                 SAFETY STOCK                            
042700     03 CLAG-KVSPANT         PIC S9(7)           COMP-3.                  
042800*                                 SPÄRRAT ANTAL                           
042900*                                 BLOCKED QTY                             
043000     03 CLAG-KVSPARR-KVAL    PIC S9(7)           COMP-3.                  
043100*                                 SPÄRRAT ANTAL KVALITETSFEL              
043200*                                 BLOCKED QUANTITY QUALITY ERROR          
043300     03 CLAG-KVSLUTKP        PIC S9(7)           COMP-3.                  
043400*                                 SLUTKÖPSSALDO                           
043500     03 CLAG-KVUTJFEL        PIC S9(6)V9(1)      COMP-3.                  
043600*                                 UTJÄMNAT FEL                            
043700     03 CLAG-KVUTRS          PIC S9(7)           COMP-3.                  
043800*                                 UTREDNINGSSALDO                         
043900*                                 INVESTIG.BALANCE                        
044000     03 CLAG-KVVECKOR-AT     PIC S9(3)           COMP-3.                  
044100*                                 ANTAL VECKOR ANSKAFFNINGSTID            
044200     03 CLAG-KVVECKOR-BT     PIC S9(3)           COMP-3.                  
044300*                                 ANTAL VECKOR BESTÄLLNINGSTID            
044400     03 CLAG-KVVECKOR-FT     PIC S9(3)           COMP-3.                  
044500*                                 ANTAL VECKOR FRYSNINGSTID               
044600     03 CLAG-KVVECKOR-LT     PIC S9(3)           COMP-3.                  
044700*                                 ANTAL VECKOR LEDTID                     
044800     03 CLAG-PRARTBES        PIC S9(7)V9(2)      COMP-3.                  
044900*                                 BESTÄLLNINGSPRIS I KRONOR               
045000*                                 ORDER PRICE SWEDISH CURRENCY            
045100     03 CLAG-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
045200*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
045300*                                 GROSS-PRICE EXPORT                      
045400*                                  (FOB-GROSS)                            
045500     03 CLAG-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
045600*                                 ARTIKELNS SJÄLVKOSTNAD                  
045700*                                 COST OF SALES                           
045800     03 CLAG-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
045900*                                 ARTIKELSTANDARDPRIS                     
046000*                                 STANDARD PRICE                          
046100     03 CLAG-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
046200*                                 DIREKT LÖN                              
046300*                                 SURCHARGE COSTS                         
046400     03 CLAG-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
046500*                                 DIREKT MATERIAL                         
046600*                                 SURCHARGE PACKING MATERIAL              
046700     03 CLAG-PRINK           PIC S9(7)V9(2)      COMP-3.                  
046800*                                 INKÖPSPRIS                              
046900*                                 PURCHASE PRICE                          
047000     03 CLAG-PRLFKST         PIC S9(3)V9(2)      COMP-3.                  
047100*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
047200*                                 SUPPLIERS PACKING AND HANDLING          
047300     03 CLAG-PRORDSK         PIC S9(5)V9(2)      COMP-3.                  
047400*                                 ORDERSÄRKOSTNAD                         
047500*                                 REMAINING OVERHEAD SURCHARGE            
047600     03 CLAG-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
047700*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
047800*                                 REMAINING OVERHEAD SURCHARGE            
047900     03 CLAG-PRREF           PIC S9(7)V9(2)      COMP-3.                  
048000*                                 REFERENCE PRICE                         
048100     03 CLAG-REDIRLEV        PIC S9V9(2)         COMP-3.                  
048200*                                 DIREKTLEVERANSANDEL                     
048300     03 CLAG-RESLJUST        PIC S9(2)V9(1)      COMP-3.                  
048400*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
048500*                                 ADJUSTMENT ALGORITM                     
048600     03 CLAG-RETULF          PIC S9(3)V9(4)      COMP-3.                  
048700*                                 TULLFAKTOR                              
048800*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
048900     03 CLAG-RVPROFEL        PIC S9(3)           COMP-3.                  
049000*                                 ANTAL STORA PROGNOSFEL                  
049100     03 CLAG-RVPROURS        PIC S9(3)           COMP-3.                  
049200*                                 ANTAL PROGNOSFEL I FÖLJD                
049300     03 CLAG-TIAVIDAT-SEN    PIC S9(7)           COMP-3.                  
049400*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
049500     03 CLAG-TIBESRPT        PIC S9(5)           COMP-3.                  
049600*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
049700*                                 (ÅÅVV)                                  
049800     03 CLAG-TIBESRPT-PAAM   PIC S9(5)           COMP-3.                  
049900*                                 PÅMINNELSEDATUM FÖR                     
050000*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
050100     03 CLAG-TIDISPIN        PIC S9(7)           COMP-3.                  
050200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
050300*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
050400     03 CLAG-TIINVDAT        PIC S9(5)           COMP-3.                  
050500*                                 INVENTERINGSDATUM                       
050600*                                 STOCKTAKING DATE                        
050700     03 CLAG-TILPSP          PIC S9(5)           COMP-3.                  
050800*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
050900     03 CLAG-TILTK           PIC S9(5)           COMP-3.                  
051000*                                 LTK-ÄNDRINGSDATUM                       
051100     03 CLAG-TIOMSPEC        PIC S9(5)           COMP-3.                  
051200*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
051300     03 CLAG-TIPBDAT         PIC S9(5)           COMP-3.                  
051400*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
051500     03 CLAG-TIQJUST         PIC S9(5)           COMP-3.                  
051600*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
051700     03 CLAG-TIREFSTO        PIC S9(7)           COMP-3.                  
051800*                                 BEORDRINGSSTOPPAD T.OM.                 
051900*                                 STOPPED FOR ORDERING UNTIL              
052000     03 CLAG-TIRODAT         PIC S9(7)           COMP-3.                  
052100*                                 RESTORDERDATUM         (ÅÅMMDD)         
052200*                                 BACK ORDER DATE        (YYMMDD)         
052300     03 CLAG-TISLJUST        PIC S9(5)           COMP-3.                  
052400*                                 VECKA DÅ JUSTERING AV SÄKER-            
052500*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
052600     03 CLAG-TISLUTKP        PIC S9(7)           COMP-3.                  
052700*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
052800*                                 CALC OF ATR-BAL IS TO COMMENCE          
052900     03 CLAG-TISPARR-KVAL    PIC 9(6).                                    
053000*                                 SPÄRRAD DATUM KVALITETSFEL              
053100*                                 BLOCKED DATE QUALITY ERROR              
053200     03 CLAG-TIURPROD        PIC S9(5)           COMP-3.                  
053300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
053400     03 CLAG-VKART           PIC S9(7)           COMP-3.                  
053500*                                 ARTIKELVIKT (G)                         
053600*                                 PART WEIGHT (G)                         
053700     03 CLAG-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
053800*                                 ARTIKELVOLYM NETTO (CM3)                
053900*                                 PART NET VOLUME    (CM3)                
054000     03 CLAG-LAND-MOMSKOD    OCCURS 4 TIMES.                              
054100        05 CLAG-IDLANDX2     PIC X(2).                                    
054200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
054300*                                 2-LETTER CODE FOR COUNTRY               
054400        05 CLAG-KDVAT        PIC X(2).                                    
054500*                                 MOMSKOD                                 
054600*                                 VAT CODE                                
054700     03 CLAG-BEHOVSPLANER.                                                
054800*                                 PLANERADE BEHOV                         
054900        05 CLAG-DAPBPLAN     PIC 9(8).                                    
055000*                                 DATUM KVPB-PLAN GILTIG TOM              
055100*                                 DATE KVPB-PLAN VALID UNTIL              
055200        05 CLAG-DASEASON     PIC 9(8).                                    
055300*                                 DATUM RESEASON-LEDTID GILTIG TO         
055400*                                 M                                       
055500*                                 DATE RESEASON-LEDTID VALID UNTI         
055600*                                 L                                       
055700        05 CLAG-KVPB-PLAN    PIC S9(6)V9(1)      COMP-3.                  
055800*                                 PLANERAT PERIODBEHOV                    
055900*                                 PLANNED PERIOD REQUIREMENTS             
056000        05 CLAG-RESEASON-PLAN                                             
056100                             OCCURS 12 TIMES                              
056200                             PIC S9V9(2)         COMP-3.                  
056300*                                 SÄSONGSINDEX INKLUSIVE REFILL           
056400     03 CLAG-TISKROT         PIC S9(7)           COMP-3.                  
056500*                                 SKROTNINGSDATUM                         
056600*                                 DATE OF SCRAPPING                       
056700     03 CLAG-FLCDART         PIC X.                                       
056800*                                 CROSS-DOCKING PART                      
056900*                                 CROSS-DOCKING PART                      
057000     03 CLAG-ADART-CD        OCCURS 4 TIMES.                              
057100*                                 ARTIKELADRESS I CD-LAGRET               
057200*                                 PARTS-ADRESS IN CD WAREHOUSE            
057300        05 CLAG-ADLAGOMR-CD  PIC S9(3)           COMP-3.                  
057400*                                 LAGEROMRÅDE                             
057500*                                 AREA                                    
057600        05 CLAG-ADGANG-CD    PIC S9(3)           COMP-3.                  
057700*                                 GÅNG                                    
057800*                                 AISLE                                   
057900        05 CLAG-ADPLATS-CD   PIC S9(5)           COMP-3.                  
058000*                                 LAGERPLATSNUMMER                        
058100*                                 LOCATION                                
058200     03 CLAG-KVLS-CD         OCCURS 4 TIMES                               
058300                             PIC S9(7)           COMP-3.                  
058400*                                 LAGERSALDO CD                           
058500*                                 STOCK BALANCE CD                        
058600     03 CLAG-KVRESS-CD       OCCURS 4 TIMES                               
058700                             PIC S9(7)           COMP-3.                  
058800*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
058900*                                  DOCKING LAGER                          
059000*                                 QUANTITY RESERVED ITEMS IN CROS         
059100*                                 S DOCKING WAREHOUSE                     
059200*** END OF VILMAII-COPY LENGTH= 775 BYTES                                 
