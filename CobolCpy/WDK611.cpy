000100 01  CLAG-WDK611.                                                         
000200*                                 ARTIKEL-CLAGER INFO                     
000300*                                 FYSISK NYCKEL KDSEGKEY                  
000400     03 CLAG-KDSEGKEY        PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 CLAG-ADART-CD        OCCURS 4 TIMES.                              
000800*                                 ARTIKELADRESS I CD-LAGRET               
000900*                                 PARTS-ADRESS IN CD WAREHOUSE            
001000        05 CLAG-ADLAGOMR-CD  PIC S9(3)           COMP-3.                  
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300        05 CLAG-ADGANG-CD    PIC S9(3)           COMP-3.                  
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600        05 CLAG-ADPLATS-CD   PIC S9(5)           COMP-3.                  
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900     03 CLAG-ADART.                                                       
002000*                                 ARTIKELADRESS I LAGRET                  
002100*                                 PARTS-ADRESS                            
002200        05 CLAG-ADLAGOMR     PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500        05 CLAG-ADGANG       PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800        05 CLAG-ADPLATS      PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100     03 CLAG-ADART-SVS.                                                   
003200*                                 ARTIKELADRESS I SVS-LAGRET              
003300*                                 PARTS-ADRESS IN SVS WAREHOUSE           
003400        05 CLAG-ADLAGOMR-SVS PIC S9(3)           COMP-3.                  
003500*                                 LAGEROMRÅDE                             
003600*                                 AREA                                    
003700        05 CLAG-ADGANG-SVS   PIC S9(3)           COMP-3.                  
003800*                                 GÅNG                                    
003900*                                 AISLE                                   
004000        05 CLAG-ADPLATS-SVS  PIC S9(5)           COMP-3.                  
004100*                                 LAGERPLATSNUMMER                        
004200*                                 LOCATION                                
004300     03 CLAG-ADINLOMR-BOA    PIC X(4).                                    
004400*                                 BUFFERTOMRÅDE-ALTERNATIVT               
004500*                                 BUFFER AREA ALTERNATIVE                 
004600     03 CLAG-ADINPORT        PIC X(8).                                    
004700*                                 AVLASTNINGSPORT                         
004800*                                 LOADING GATE                            
004900     03 CLAG-BEFT            PIC S9(3)           COMP-3.                  
005000*                                 FÖRPACKNINGSTYP                         
005100*                                 PACKAGING TYPE                          
005200     03 CLAG-DAPBPLAN        PIC 9(8).                                    
005300*                                 DATUM KVPB-PLAN GILTIG TOM              
005400*                                 DATE KVPB-PLAN VALID UNTIL              
005500     03 CLAG-DASEASON        PIC 9(8).                                    
005600*                                 DATUM RESEASON-LEDTID GILTIG TO         
005700*                                 M                                       
005800*                                 DATE RESEASON-LEDTID VALID UNTI         
005900*                                 L                                       
006000     03 CLAG-DAXPOINT        PIC 9(8).                                    
006100*                                 POÄNGÄNDRINGSDATUM (ÅÅÅÅMMDD)           
006200*                                 POINT CHANGE DATE (YYYYMMDD)            
006300     03 CLAG-FLAVRART        PIC X.                                       
006400*                                 AVROPSARTIKEL                           
006500     03 CLAG-FLCDART         PIC X.                                       
006600*                                 CROSS-DOCKING PART                      
006700*                                 CROSS-DOCKING PART                      
006800     03 CLAG-FLEJBUFF        PIC X.                                       
006900*                                 EJ BUFFERTSTYRNING                      
007000*                                 NO BUFFER STEERING                      
007100     03 CLAG-FLFSP           PIC X.                                       
007200*                                 FÖRDELNINGSSPÄRR                        
007300*                                 BLOCKED FOR SPLIT                       
007400     03 CLAG-FLGEMART        PIC X.                                       
007500*                                 FLAGGA GEMENSAM ARTIKEL                 
007600*                                 COMMON PART FLAG                        
007700     03 CLAG-FLJIT           PIC X.                                       
007800*                                 JUST-IN-TIME FLAGGA                     
007900*                                 JUST-IN-TIME FLAG                       
008000     03 CLAG-FLLARM-BUF      PIC X.                                       
008100*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
008200     03 CLAG-FLLSRDEL        PIC X.                                       
008300*                                 LEVERERAS SOM RESDEL                    
008400     03 CLAG-FLLTKSP         PIC X.                                       
008500*                                 SPÄRR UTLEVERANS C2-LAGER               
008600     03 CLAG-FLMANAT         PIC X.                                       
008700*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
008800     03 CLAG-FLMANBK         PIC X.                                       
008900*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
009000     03 CLAG-FLMANGK         PIC X.                                       
009100*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
009200     03 CLAG-FLMANKP         PIC X.                                       
009300*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
009400     03 CLAG-FLMANLT         PIC X.                                       
009500*                                 MANUELLT SATT LEDTID ?                  
009600     03 CLAG-FLMANOPP        PIC X.                                       
009700*                                 MANUELLT SATT GODK. AV OP-PLAN?         
009800*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
009900     03 CLAG-FLMANOSK        PIC X.                                       
010000*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
010100*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
010200     03 CLAG-FLMANPB         PIC X.                                       
010300*                                 MANUELLT REGISTRERAT PB-TPO             
010400*                                 MANUALLY REGISTRATED PB-TPO             
010500     03 CLAG-FLMANQ          PIC X.                                       
010600*                                 MANUELL HEMTAGNINGSKVANTITET            
010700     03 CLAG-FLMARKSP        PIC X.                                       
010800*                                 MARKNADSSPÄRR                           
010900*                                 MARKET BLOCKING CODE                    
011000     03 CLAG-FLMPB           PIC X.                                       
011100*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
011200     03 CLAG-FLNYBER         PIC X.                                       
011300*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
011400     03 CLAG-FLOREGPB        PIC X.                                       
011500*                                 OREGELBUNDEN PROGNOS (PB) ?             
011600     03 CLAG-FLRADREF        PIC X.                                       
011700*                                 KOMPLETTERANDE INFO. KRÄVS              
011800*                                 ADDITIONAL INFORMATION REQUIRED         
011900     03 CLAG-FLREFILL        PIC X.                                       
012000*                                 REFILLARTIKEL                           
012100*                                 REFILLPART                              
012200     03 CLAG-FLRELSP         PIC X.                                       
012300*                                 RELEASEBLOCKAD ARTIKEL .                
012400*                                 BLOCKED PART                            
012500     03 CLAG-FLSKROT-AUTO    PIC X.                                       
012600*                                 SKROTNING AUTOMATISKT BEORDRAD          
012700*                                 SCRAPPING AUTOMATICALLY ORDERED         
012800     03 CLAG-FLSKROT-BEORD   PIC X.                                       
012900*                                 SKROTNING BEORDRAD AV ANSK              
013000*                                 SCRAPPING ORDERED BY PROCURER           
013100     03 CLAG-FLSKROT-BEV     PIC X.                                       
013200*                                 BEVAKNING 2 ÅR INFÖR SKROTNING          
013300*                                 2 YEARS BEFORE SCRAPPING CHECK          
013400     03 CLAG-FLSKROT-SL      PIC X.                                       
013500*                                 SÄKERHETSLAGER NOLL INFÖR SKROT         
013600*                                 STOCK TO ZERO BEFORE SCRAPPING          
013700     03 CLAG-FLSKROT-WLC     PIC X.                                       
013800*                                 SISTA AVROP FÖRE SKROT                  
013900*                                 LAST CALL BEFORE SCRAPPING              
014000     03 CLAG-FLSPKOST        PIC X.                                       
014100*                                 SPECIELLA KOSTNADER FINS                
014200*                                 SPECIAL COSTS EXIST                     
014300     03 CLAG-FLTOPP          PIC X.                                       
014400*                                 TOPP-200-ARTIKEL                        
014500*                                 TOP 200 PART                            
014600     03 CLAG-FLTPO1          PIC X.                                       
014700*                                 ARTIKELN GODKÄND FÖR TPO1               
014800*                                 TPO1 ALLOWED FOR ARTICLE                
014900     03 CLAG-IDANSK          PIC S9(3)           COMP-3.                  
015000*                                 ANSKAFFARNUMMER                         
015100*                                 PROCURER NO.                            
015200     03 CLAG-IDARTNR-EMBQ0   PIC S9(9)           COMP-3.                  
015300*                                 EMBALLAGEARTIKELNR FÖR Q0               
015400     03 CLAG-IDARTNR-EMBQ1   PIC S9(9)           COMP-3.                  
015500*                                 EMBALLAGEARTIKELNR FÖR Q1               
015600     03 CLAG-IDARTNR-EMBQ2   PIC S9(9)           COMP-3.                  
015700*                                 EMBALLAGEARTIKELNR FÖR Q2               
015800     03 CLAG-IDARTNR-EMBQ3   PIC S9(9)           COMP-3.                  
015900*                                 EMBALLAGEARTIKELNR FÖR Q3               
016000     03 CLAG-IDARTNR-EMBQ4   PIC S9(9)           COMP-3.                  
016100*                                 EMBALLAGEARTIKELNR FÖR Q4               
016200     03 CLAG-IDBERED         PIC S9(3)           COMP-3.                  
016300*                                 BEREDARENUMMER                          
016400     03 CLAG-IDDC-REF        PIC X(2).                                    
016500*                                 SÄNDANDE LAGER FÖR REFILL               
016600*                                 SENDING WAREHOUSE FOR REFILL            
016700     03 CLAG-IDFS-SEN        PIC X(8).                                    
016800*                                 FÖLJESEDELSNUMMER SENASTE INLEV         
016900*                                 ADVICE NOTE NUMBER ODETTE               
017000     03 CLAG-IDINK           PIC X(4).                                    
017100*                                 INKÖPARNUMMER                           
017200*                                 PURCHASE IDENTIFICATION NUMBER          
017300     03 CLAG-IDKAT           OCCURS 3 TIMES                               
017400                             PIC X(5).                                    
017500*                                 KATALOGBETECKNING                       
017600     03 CLAG-IDLEVNR-SEN     PIC X(5).                                    
017700*                                 SENASTE LEVERANTÖR                      
017800     03 CLAG-IDLEVNR-SHIP    PIC X(5).                                    
017900*                                 SKEPPANDE LEVERANTÖR                    
018000*                                 SHIPPING SUPPLIER                       
018100     03 CLAG-IDLKTO          PIC S9(7)           COMP-3.                  
018200*                                 LAGERKONTO (FFHHHUU)                    
018300*                                 STOCK ACCOUNT (CCMMMSS)                 
018400     03 CLAG-IDPLANGR-AG     PIC S9              COMP-3.                  
018500*                                 PLANERINGSGRUPP ANSKAFFARE              
018600     03 CLAG-IDPLANGR-LEV    PIC S9              COMP-3.                  
018700*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
018800     03 CLAG-IDPROENH        OCCURS 3 TIMES                               
018900                             PIC X(8).                                    
019000*                                 PRODUKTIONSENHET                        
019100*                                 PRODUCTION UNIT                         
019200     03 CLAG-IDPROJ          PIC X(4).                                    
019300*                                 PARTS PROJEKTIDENTITET                  
019400*                                 PARTS PROJECT IDENTITY                  
019500     03 CLAG-IDPROJUP        PIC X(8).                                    
019600*                                 PROJEKTUPPDRAG                          
019700*                                 PROJECT ASSIGNMENT                      
019800     03 CLAG-IDPSN           PIC 9(3).                                    
019900*                                 PROPER SHIPPING NAME                    
020000*                                 PROPER SHIPPING NAME                    
020100     03 CLAG-IDRITN          PIC X(10).                                   
020200*                                 RITNINGSNUMMER                          
020300*                                 DRAWING NUMBER                          
020400     03 CLAG-IDSTATNR        OCCURS 6 TIMES                               
020500                             PIC S9(9)           COMP-3.                  
020600*                                 STATISTISKT NUMMER                      
020700*                                 1 = NORSKT                              
020800*                                 2 = ENGELSKT                            
020900*                                 3 = BELGISKT                            
021000*                                 4 = PERUANSKT                           
021100*                                 5 = SVENSKT                             
021200*                                 6 =                                     
021300*                                 STATISTICAL NO.                         
021400     03 CLAG-IDUSER-EMB      PIC X(8).                                    
021500*                                 ANVÄNDARENS SÄKERHETS ID                
021600*                                 USER SECURITY-IDENTITY                  
021700     03 CLAG-IDUSER-SPKVAL   PIC X(8).                                    
021800*                                 ANVÄNDAR-ID KVALITETSPÄRR               
021900*                                 USER ID QUALITY ERROR                   
022000     03 CLAG-KDAGE           PIC X.                                       
022100*                                 AGE-CODE                                
022200*                                 AGE-CODE                                
022300     03 CLAG-KDARTHNT        PIC S9(7)           COMP-3.                  
022400*                                 HANTERINGSKOD                           
022500*                                 HANDLING CODE                           
022600     03 CLAG-KDARTURS        PIC X(2).                                    
022700*                                 ARTIKELURSPRUNGSKOD                     
022800*                                 COUNTRY OF ORIGIN                       
022900     03 CLAG-KDAVT           PIC S9              COMP-3.                  
023000*                                 AVTALSMÄRKNING                          
023100*                                 AGREEMENT CODE                          
023200     03 CLAG-KDBPSR          PIC S9              COMP-3.                  
023300*                                 BASLAGERFÖRSLAGSNIVÅ                    
023400*                                 BASIC PART STOCK RECOMMENDATION         
023500     03 CLAG-KDEFFMAN        PIC X.                                       
023600*                                 EMIL-KOD                                
023700*                                 EMIL-CODE                               
023800     03 CLAG-KDEMBKOD-0      PIC S9(3)           COMP-3.                  
023900*                                 EMBALLAGEKOD 0                          
024000     03 CLAG-KDEMBKOD-1      PIC S9(3)           COMP-3.                  
024100*                                 EMBALLAGEKOD 1                          
024200     03 CLAG-KDEMBKOD-2      PIC S9(3)           COMP-3.                  
024300*                                 EMBALLAGEKOD 2                          
024400     03 CLAG-KDEMBVOL        PIC X.                                       
024500*                                 VOLUME UPDATED CODE                     
024600*                                 VOLUME UPDATED CODE                     
024700     03 CLAG-KDERS           PIC S9(3)           COMP-3.                  
024800*                                 ERSÄTTNINGSKOD                          
024900*                                 SUPERSESSION CODE                       
025000     03 CLAG-KDEXCHA         PIC S9(3)           COMP-3.                  
025100*                                 EXCHANGE ACCOUNT CODE                   
025200     03 CLAG-KDFARLIG        PIC S9              COMP-3.                  
025300*                                 KOD FÖR FARLIGT GODS                    
025400*                                 DANGEROUS GOODS CODE                    
025500     03 CLAG-KDFORP.                                                      
025600*                                 FÖRPACKNINGSKOD                         
025700*                                 PACKAGING CODE                          
025800        05 CLAG-KDFORPPL     PIC 9.                                       
025900*                                 FÖRPACKNINGSPLATS                       
026000*                                 PREPACKING PLACE                        
026100        05 CLAG-KDFORPGP     PIC 9(2).                                    
026200*                                 FÖRPACKNINGSGRUPP                       
026300*                                 PREPACKING GROUP                        
026400        05 CLAG-KDFORPUF     PIC 9.                                       
026500*                                 UPPRÄKNINGSFAKTOR                       
026600*                                 ENUMERATION                             
026700     03 CLAG-KDFREKKL        PIC X.                                       
026800*                                 FREKVENSKLASS                           
026900*                                 FREQ. CLASS                             
027000     03 CLAG-KDGK            PIC S9              COMP-3.                  
027100*                                 GODSMOTTAGAREKOD                        
027200*                                 GOODS RECEIVING WAREHOUSE CODE          
027300     03 CLAG-KDHF            PIC S9              COMP-3.                  
027400*                                 HUVUDFÖRRÅDSMÄRKNING                    
027500*                                 CODE MAIN STORAGE                       
027600     03 CLAG-KDKG            PIC S9              COMP-3.                  
027700*                                 KURANSGRUPP                             
027800*                                 TURNOVER CODE                           
027900     03 CLAG-KDKSP           PIC S9              COMP-3.                  
028000*                                 KÖPSPÄRR                                
028100*                                 PURCHASE BLOCKING CODE                  
028200     03 CLAG-KDLEVPLF        PIC X.                                       
028300*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
028400*                                 CODE FOR APPROVAL OF SCHEDULE P         
028500*                                 ROPOSAL                                 
028600     03 CLAG-KDLEVSP         PIC S9(3)           COMP-3.                  
028700*                                 SPÄRRKOD LEVERANS                       
028800*                                 DELIVERY BLOCKING CODE                  
028900     03 CLAG-KDLPSP          PIC S9              COMP-3.                  
029000*                                 LEVERANSPLANESPÄRR                      
029100     03 CLAG-KDLTK           PIC S9              COMP-3.                  
029200*                                 LAGERTILLHÖRIGHETSKOD                   
029300*                                 STOCK BELONGING CODE                    
029400     03 CLAG-KDOPPLAN        PIC X.                                       
029500*                                 OPTIMAL PLAN INOM FRYSTID               
029600*                                 OPTIMAL PLAN WITHIN FREEZTIME           
029700     03 CLAG-KDPCOO          PIC X.                                       
029800*                                 FÖRMÅNS AVTAL URSPRUNGSLAND             
029900*                                 PREF.AGREEEMENT COUNTRY ORIGIN          
030000     03 CLAG-KDPRISKL        PIC X.                                       
030100*                                 PRISKLASS                               
030200*                                 PRICE CLASS                             
030300     03 CLAG-KDPSLLOC        PIC 9(2).                                    
030400*                                 PRODUKTSLAG LOKALT                      
030500*                                 PRODUCT GROUP LOCAL                     
030600     03 CLAG-KDSPEEMB        PIC 9.                                       
030700*                                 SPECIALEMBALLAGEKOD                     
030800*                                 SPECIAL PACKING CODE                    
030900     03 CLAG-KDSRA           PIC S9(3)           COMP-3.                  
031000*                                 SRA-KOD                                 
031100*                                 SRA CODE                                
031200     03 CLAG-KDTIPPR         PIC S9              COMP-3.                  
031300*                                 TIPPAT PRIS KOD                         
031400*                                 ESTIMATED PRICE CODE                    
031500     03 CLAG-KDTULLRE        PIC S9              COMP-3.                  
031600*                                 TULLRESTITUTION MÄRKNING                
031700*                                 CUSTOMS RESTITUAT.                      
031800     03 CLAG-KDUART          PIC X.                                       
031900*                                 UNDANTAGSARTIKEL                        
032000*                                 EXECPTION PARTS                         
032100     03 CLAG-KDVSOP          PIC S9(3)           COMP-3.                  
032200*                                 VSOP-KOD                                
032300*                                 VSOP-CODE                               
032400     03 CLAG-KDVTH           PIC S9              COMP-3.                  
032500*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
032600*                                 CODE FOR COST RESPONSIBILITY            
032700     03 CLAG-KDVVKL          PIC S9              COMP-3.                  
032800*                                 VOLYMVÄRDESKLASS                        
032900*                                 VOLUME VALUE CLASS                      
033000     03 CLAG-KDYTBEH         PIC S9(3)           COMP-3.                  
033100*                                 YTBEHANDLINGSKOD                        
033200*                                                                         
033300     03 CLAG-KVAKS-CDC       PIC S9(7)           COMP-3.                  
033400*                                 DEL AV AK SOM LIGGER I CDC              
033500*                                 PART OF AK IN THE CDC                   
033600     03 CLAG-KVAKS-PAV       PIC S9(7)           COMP-3.                  
033700*                                 DEL AV AK PÅ VÄG                        
033800*                                 PART OF AK ON ITS WAY                   
033900     03 CLAG-KVAKS-T         PIC S9(7)           COMP-3.                  
034000*                                 DEL AV AK I EN TERMINAL                 
034100*                                 PART OF AK IN A TERMINAL                
034200     03 CLAG-KVAP            PIC S9(7)           COMP-3.                  
034300*                                 ANNULLATIONSPUNKT                       
034400     03 CLAG-KVAVIS-SEN      PIC S9(7)           COMP-3.                  
034500*                                 SENAST AVISERAT ANTAL                   
034600     03 CLAG-KVAVROP-TOT     PIC S9(7)           COMP-3.                  
034700*                                 ALLA AVROP MED KOD = 2                  
034800     03 CLAG-KVBEART         PIC S9(7)           COMP-3.                  
034900*                                 BESTÄLLT ANTAL STYCKEN                  
035000*                                 ORDERED QUANTITY                        
035100     03 CLAG-KVBK            PIC S9(7)           COMP-3.                  
035200*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
035300     03 CLAG-KVDAGAR-FFH     PIC S9(3)           COMP-3.                  
035400*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
035500     03 CLAG-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
035600*                                 INLEVERANSTID     (ANTAL DAGAR)         
035700     03 CLAG-KVDAGAR-TT      PIC S9(3)           COMP-3.                  
035800*                                 DAGAR TULL- OCH TRANSPORT-TID           
035900     03 CLAG-KVEFRS          PIC S9(7)           COMP-3.                  
036000*                                 EJ FAKTURERAT ANTAL STYCK               
036100*                                 ORDERED NOT INVOICED QTY                
036200     03 CLAG-KVEOQ           PIC S9(7)           COMP-3.                  
036300*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
036400*                                 ET                                      
036500     03 CLAG-KVFRYSTI        PIC S9(3)           COMP-3.                  
036600*                                 FRYSTID FÖR TPO-ORDER                   
036700*                                 FREEZTIME FOR TPO                       
036800     03 CLAG-KVINVS          PIC S9(7)           COMP-3.                  
036900*                                 INVENTERINGSSALDO                       
037000*                                 STOCK-TAKING BALANCE                    
037100     03 CLAG-KVKP            PIC S9(7)           COMP-3.                  
037200*                                 KÖPPUNKT                                
037300     03 CLAG-KVLAAN          PIC S9(7)           COMP-3.                  
037400*                                 LÅNESALDO                               
037500     03 CLAG-KVLS            PIC S9(7)           COMP-3.                  
037600*                                 LAGERSALDO                              
037700*                                 STOCK BALANCE                           
037800     03 CLAG-KVLS-CD         OCCURS 4 TIMES                               
037900                             PIC S9(7)           COMP-3.                  
038000*                                 LAGERSALDO CD                           
038100*                                 STOCK BALANCE CD                        
038200     03 CLAG-KVLS-SVS        PIC S9(7)           COMP-3.                  
038300*                                 LAGERSALDO SVS                          
038400*                                 STOCK BALANCE SVS                       
038500     03 CLAG-KVMAD-SEP       PIC S9(6)V9(1)      COMP-3.                  
038600*                                 SEPARAT PROGNOSFEL                      
038700     03 CLAG-KVMAD-TOT       PIC S9(6)V9(1)      COMP-3.                  
038800*                                 TOTALT PROGNOSFEL                       
038900     03 CLAG-KVMAXPL         PIC S9(7)           COMP-3.                  
039000*                                 MAX ANTAL (STYCK) PÅ PLOCKPLATS         
039100*                                 MAX ALLOWED QTY IN PICKING LOC          
039200     03 CLAG-KVMP            PIC S9(7)           COMP-3.                  
039300*                                 MAXPUNKT                                
039400*                                 MAXIMUM POINT                           
039500     03 CLAG-KVOI-OVR        PIC S9(7)           COMP-3.                  
039600*                                 ORDERINGÅNG LEV ÖVRIGT                  
039700*                                 ORDERED PCS PER TIME UNIT OTHER         
039800     03 CLAG-KVOI-PLOCK      PIC S9(7)           COMP-3.                  
039900*                                 ORDERINGÅNG LEV FRÅN PLOCKPLATS         
040000*                                 ORDERED FROM PICKING AREA               
040100     03 CLAG-KVOVERF         PIC S9(7)           COMP-3.                  
040200*                                 ÖVERFÖRINGSSALDO                        
040300     03 CLAG-KVPALL          PIC S9(7)           COMP-3.                  
040400*                                 ANTAL I PALL                            
040500*                                 QUANTITY IN PALLET                      
040600     03 CLAG-KVPB-HIST       PIC S9(6)V9(1)      COMP-3.                  
040700*                                 PB (PROGNOS) HISTORISKT CDC             
040800*                                 HISTORIC REQUIREMENTS CDC               
040900     03 CLAG-KVPB-PLAN       PIC S9(6)V9(1)      COMP-3.                  
041000*                                 PLANERAT PERIODBEHOV                    
041100*                                 PLANNED PERIOD REQUIREMENTS             
041200     03 CLAG-KVPB-PLAN-JUST1 PIC S9(6)V9(1)      COMP-3.                  
041300*                                 PLANERAT PERIODBEHOV JUST1              
041400*                                 PLANNED PERIOD REQUIREMENT ADJ1         
041500     03 CLAG-KVPB-PLAN-JUST2 PIC S9(6)V9(1)      COMP-3.                  
041600*                                 PLANERAT PERIODBEHOV JUST2              
041700*                                 PLANNED PERIOD REQUIREMENT ADJ2         
041800     03 CLAG-KVPB-SATS       PIC S9(6)V9(1)      COMP-3.                  
041900*                                 SATS-PERIODBEHOV                        
042000*                                 KIT PERIOD REQUIREMENTS                 
042100     03 CLAG-KVPB-SEP        PIC S9(6)V9(1)      COMP-3.                  
042200*                                 SEPARAT PERIODBEHOV                     
042300*                                 SEPARATE PERIOD REQUIREMENTS            
042400     03 CLAG-KVPB-TPO        PIC S9(6)V9(1)      COMP-3.                  
042500*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
042600*                                 PERIODICAL DEMAND TPO1 AND TPO2         
042700*                                                                         
042800     03 CLAG-KVPB-TREND      PIC S9(6)V9(1)      COMP-3.                  
042900*                                 PERIODTRENDVÄRDE                        
043000     03 CLAG-KVPB-VESL       PIC S9(6)V9(1)      COMP-3.                  
043100*                                 GÄLLANDE PB VID VECKOSLUT               
043200     03 CLAG-KVPOINT         PIC S9(7)           COMP-3.                  
043300*                                 POINT VALUE                             
043400     03 CLAG-KVQ             PIC S9(7)           COMP-3.                  
043500*                                 EKONOMISK HEMTAGNINGSKVANTITET          
043600     03 CLAG-KVQ-JUST        PIC S9(7)           COMP-3.                  
043700*                                 NY EKON HEMTAGNINGSKVANTITET            
043800     03 CLAG-KVQPACK-0       PIC S9(5)           COMP-3.                  
043900*                                 ANTAL I Q0 FÖRPACKNING                  
044000     03 CLAG-KVQPACK-1       PIC S9(5)           COMP-3.                  
044100*                                 ANTAL I Q1 FÖRPACKNING                  
044200*                                 QUANTITY IN BULK PACK Q1                
044300     03 CLAG-KVQPACK-2       PIC S9(5)           COMP-3.                  
044400*                                 ANTAL I Q2 FÖRPACKNING                  
044500*                                 QUANTITY IN BULK PACK Q2                
044600     03 CLAG-KVQPACK-3       PIC S9(5)           COMP-3.                  
044700*                                 ANTAL I Q3 FÖRPACKNING                  
044800*                                 QUANTITY IN BULK PACK Q3                
044900     03 CLAG-KVQPACK-4       PIC S9(5)           COMP-3.                  
045000*                                 ANTAL I Q4 FÖRPACKNING                  
045100*                                 QUANTITY IN BULK PACK Q4                
045200     03 CLAG-KVREFBER-PLOCK  PIC S9(7)           COMP-3.                  
045300*                                 BERÄKNAD REFILLINGKVANT-PLOCK           
045400*                                 CALCULATED REFILLING QTY-PICK           
045500     03 CLAG-KVREFOVL-TOT    PIC S9(7)           COMP-3.                  
045600*                                 BERÄKNAD ÖVERLAGER CHILD DC:N           
045700*                                 CALCULATED OVERSTOCK ALL XDC            
045800     03 CLAG-KVREFPKT-PLOCK  PIC S9(7)           COMP-3.                  
045900*                                 BERÄKNAD PÅFYLLNADSPUNKT-PLOCK          
046000*                                 CALCULATED REFILLING POINT-PICK         
046100     03 CLAG-KVRESS          PIC S9(7)           COMP-3.                  
046200*                                 RESERVERAT ANTAL ARTIKLAR               
046300*                                 QUANTITY RESERVED ITEMS                 
046400     03 CLAG-KVRESS-CD       OCCURS 4 TIMES                               
046500                             PIC S9(7)           COMP-3.                  
046600*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
046700*                                  DOCKING LAGER                          
046800*                                 QUANTITY RESERVED ITEMS IN CROS         
046900*                                 S DOCKING WAREHOUSE                     
047000     03 CLAG-KVRETUR         PIC S9(7)           COMP-3.                  
047100*                                 ANTAL I RETUR                           
047200*                                 QUANTITY IN RETURN                      
047300     03 CLAG-KVRETUR-TOT     PIC S9(7)           COMP-3.                  
047400*                                 ANTAL RETURER TOTALT                    
047500*                                 QUANTITY TOTAL RETURNS                  
047600     03 CLAG-KVROS           PIC S9(7)           COMP-3.                  
047700*                                 RESTORDERSALDO                          
047800*                                 BACKORDER QTY                           
047900     03 CLAG-KVSLAGER        PIC S9(7)           COMP-3.                  
048000*                                 SÄKERHETSLAGER                          
048100*                                 SAFETY STOCK                            
048200     03 CLAG-KVSLAGER-OPT    PIC S9(7)           COMP-3.                  
048300*                                 OPTIMALT SÄKERHETSLAGER                 
048400*                                 OPT SAFETY STOCK                        
048500     03 CLAG-KVSLUTKP        PIC S9(7)           COMP-3.                  
048600*                                 SLUTKÖPSSALDO                           
048700     03 CLAG-KVSPANT         PIC S9(7)           COMP-3.                  
048800*                                 SPÄRRAT ANTAL                           
048900*                                 BLOCKED QTY                             
049000     03 CLAG-KVSPARR-KVAL    PIC S9(7)           COMP-3.                  
049100*                                 SPÄRRAT ANTAL KVALITETSFEL              
049200*                                 BLOCKED QUANTITY QUALITY ERROR          
049300     03 CLAG-KVTILLG-TOT     PIC S9(7)           COMP-3.                  
049400*                                 LAGERTILLGÅNG CDC TOTALT                
049500     03 CLAG-KVULOAD         PIC S9(7)           COMP-3.                  
049600*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
049700*                                 MIN LOAD FROM SUPPLIER                  
049800     03 CLAG-KVUTJFEL        PIC S9(6)V9(1)      COMP-3.                  
049900*                                 UTJÄMNAT FEL                            
050000     03 CLAG-KVUTRS          PIC S9(7)           COMP-3.                  
050100*                                 UTREDNINGSSALDO                         
050200*                                 INVESTIGATION BALANCE                   
050300     03 CLAG-KVVECKOR-AT     PIC S9(3)           COMP-3.                  
050400*                                 ANTAL VECKOR ANSKAFFNINGSTID            
050500     03 CLAG-KVVECKOR-BT     PIC S9(3)           COMP-3.                  
050600*                                 ANTAL VECKOR BESTÄLLNINGSTID            
050700     03 CLAG-KVVECKOR-FT     PIC S9(3)           COMP-3.                  
050800*                                 ANTAL VECKOR FRYSNINGSTID               
050900     03 CLAG-KVVECKOR-LT     PIC S9(3)           COMP-3.                  
051000*                                 ANTAL VECKOR LEDTID                     
051100     03 CLAG-KVVECKOR-LVAR   PIC S9(2)V9(1)      COMP-3.                  
051200*                                 VARIANS I LEDTIDEN                      
051300*                                                                         
051400     03 CLAG-KVVECKOR-TREND  PIC S9(3)           COMP-3.                  
051500*                                 ANTAL VECKOR TRENDVÄRDE                 
051600     03 CLAG-KVVORKO         PIC S9(7)           COMP-3.                  
051700*                                 VOR-KÖ KVANT                            
051800*                                 VOR-QUEUE QUANT                         
051900     03 CLAG-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
052000*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
052100*                                 GROSS-PRICE EXPORT                      
052200*                                  (FOB-GROSS)                            
052300     03 CLAG-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
052400*                                 ARTIKELNS SJÄLVKOSTNAD                  
052500*                                 COST OF SALES                           
052600     03 CLAG-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
052700*                                 ARTIKELSTANDARDPRIS                     
052800*                                 STANDARD PRICE                          
052900     03 CLAG-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
053000*                                 DIREKT LÖN                              
053100*                                 SURCHARGE COSTS                         
053200     03 CLAG-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
053300*                                 DIREKT MATERIAL                         
053400*                                 SURCHARGE PACKING MATERIAL              
053500     03 CLAG-PRHEMTAG        PIC S9(7)V9(2)      COMP-3.                  
053600*                                 HEMTAGNINGSKOSTNAD                      
053700*                                 TRANSPORT COST                          
053800     03 CLAG-PRINK           PIC S9(7)V9(2)      COMP-3.                  
053900*                                 INKÖPSPRIS                              
054000*                                 PURCHASE PRICE                          
054100     03 CLAG-PRLFKST         PIC S9(3)V9(2)      COMP-3.                  
054200*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
054300*                                 SUPPLIERS PACKING AND HANDLING          
054400     03 CLAG-PRORDSK         PIC S9(5)V9(2)      COMP-3.                  
054500*                                 ORDERSÄRKOSTNAD                         
054600*                                 REMAINING OVERHEAD SURCHARGE            
054700     03 CLAG-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
054800*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
054900*                                 REMAINING OVERHEAD SURCHARGE            
055000     03 CLAG-PRREF           PIC S9(7)V9(2)      COMP-3.                  
055100*                                 REFERENCE PRICE                         
055200     03 CLAG-REDIRLEV        PIC S9V9(2)         COMP-3.                  
055300*                                 DIREKTLEVERANSANDEL                     
055400     03 CLAG-RESEASON-PLAN   OCCURS 12 TIMES                              
055500                             PIC S9V9(2)         COMP-3.                  
055600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
055700     03 CLAG-RESLJUST        PIC S9(2)V9(1)      COMP-3.                  
055800*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
055900*                                 ADJUSTMENT ALGORITM                     
056000     03 CLAG-RETULF          PIC S9(3)V9(4)      COMP-3.                  
056100*                                 TULLFAKTOR                              
056200*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
056300     03 CLAG-RVPROFEL        PIC S9(3)           COMP-3.                  
056400*                                 ANTAL STORA PROGNOSFEL                  
056500     03 CLAG-RVPROURS        PIC S9(3)           COMP-3.                  
056600*                                 ANTAL PROGNOSFEL I FÖLJD                
056700     03 CLAG-TIAVIDAT-SEN    PIC S9(7)           COMP-3.                  
056800*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
056900     03 CLAG-TIBESRPT        PIC S9(5)           COMP-3.                  
057000*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
057100*                                 (ÅÅVV)                                  
057200     03 CLAG-TIBESRPT-PAAM   PIC S9(5)           COMP-3.                  
057300*                                 PÅMINNELSEDATUM FÖR                     
057400*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
057500     03 CLAG-TIDATUM-TREND   PIC S9(7)           COMP-3.                  
057600*                                 JUSTERAD TREND AAMMDD                   
057700*                                 LAST TREND CHANGE  YYMMDD               
057800     03 CLAG-TIDISPIN        PIC S9(7)           COMP-3.                  
057900*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
058000*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
058100     03 CLAG-TIGILTIG-PCOO   PIC S9(7)           COMP-3.                  
058200*                                 GILTIGHETSDATUM PCOO                    
058300*                                                                         
058400*                                 DATE OF VALIDITY FOR PCOO               
058500     03 CLAG-TIINVDAT        PIC S9(5)           COMP-3.                  
058600*                                 INVENTERINGSDATUM                       
058700*                                 STOCKTAKING DATE                        
058800     03 CLAG-TILEVDAG        OCCURS 5 TIMES                               
058900                             PIC S9              COMP-3.                  
059000*                                 AVSÄNDNINGSDAG INOM VECKA               
059100*                                 DELIVERY WEEK DAY                       
059200     03 CLAG-TILPSP          PIC S9(5)           COMP-3.                  
059300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
059400     03 CLAG-TILTK           PIC S9(5)           COMP-3.                  
059500*                                 LTK-ÄNDRINGSDATUM                       
059600     03 CLAG-TIMAIL-KVAL     PIC 9(6).                                    
059700*                                 MAIL DATUM                              
059800*                                 MAIL DATE                               
059900     03 CLAG-TIOMSPEC        PIC S9(5)           COMP-3.                  
060000*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
060100     03 CLAG-TIPBDAT         PIC S9(5)           COMP-3.                  
060200*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
060300     03 CLAG-TIPBLOCK        PIC S9(7)           COMP-3.                  
060400*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
060500*                                 ÅMMDD                                   
060600*                                 FUTURE DATE FOR FORECAST TO BE          
060700*                                 BLOCKED                                 
060800*                                 INCREASED FORECAST IS ALLOWED           
060900     03 CLAG-TIPBPLAN-JUST1-FOM                                           
061000                             PIC S9(7)           COMP-3.                  
061100*                                 FOM PB-PLAN DATUM - JUST1               
061200*                                 FROM PB-PLAN DATE - ADJ1                
061300     03 CLAG-TIPBPLAN-JUST1-TOM                                           
061400                             PIC S9(7)           COMP-3.                  
061500*                                 TOM PB-PLAN DATUM - JUST1               
061600*                                 UNTIL PB-PLAN DATE - ADJ1               
061700     03 CLAG-TIPBPLAN-JUST2-FOM                                           
061800                             PIC S9(7)           COMP-3.                  
061900*                                 FOM PB-PLAN DATUM - JUST2               
062000*                                 FROM PB-PLAN DATE - ADJ2                
062100     03 CLAG-TIPBPLAN-JUST2-TOM                                           
062200                             PIC S9(7)           COMP-3.                  
062300*                                 TOM PB-PLAN DATUM - JUST2               
062400*                                 UNTIL PB-PLAN DATE - ADJ2               
062500     03 CLAG-TIQJUST         PIC S9(5)           COMP-3.                  
062600*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
062700     03 CLAG-TIREFSTO        PIC S9(7)           COMP-3.                  
062800*                                 BEORDRINGSSTOPPAD T.OM.                 
062900*                                 STOPPED FOR ORDERING UNTIL              
063000     03 CLAG-TIRODAT         PIC S9(7)           COMP-3.                  
063100*                                 RESTORDERDATUM         (ÅÅMMDD)         
063200*                                 BACK ORDER DATE        (YYMMDD)         
063300     03 CLAG-TISKPREL        PIC S9(5)           COMP-3.                  
063400*                                 PREL. SKROTNINGSDATUM (AAVV)            
063500*                                 PREL DATE OF SCRAPPING (YYWW)           
063600     03 CLAG-TISKROT         PIC S9(7)           COMP-3.                  
063700*                                 SKROTNINGSDATUM                         
063800*                                 DATE OF SCRAPPING                       
063900     03 CLAG-TISKROT-AUTO    PIC S9(7)           COMP-3.                  
064000*                                 STOPDATE AUTO-SKROTNING                 
064100*                                 STOP DATE AUTOSCRAPPING                 
064200     03 CLAG-TISLJUST        PIC S9(5)           COMP-3.                  
064300*                                 VECKA DÅ JUSTERING AV SÄKER-            
064400*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
064500     03 CLAG-TISLUTKP        PIC S9(7)           COMP-3.                  
064600*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
064700*                                 CALC OF ATR-BAL IS TO COMMENCE          
064800     03 CLAG-TISPARR-KVAL    PIC 9(6).                                    
064900*                                 SPÄRRAD DATUM KVALITETSFEL              
065000*                                 BLOCKED DATE QUALITY ERROR              
065100     03 CLAG-TISTODAT-LARM   PIC S9(7)           COMP-3.                  
065200*                                 STOPPDATUM FÖR LARM-223                 
065300*                                 STOP DATE FOR ALARM-223                 
065400     03 CLAG-TISTOREF        PIC S9(5)           COMP-3.                  
065500*                                 STOPPTIDPUNKT FÖR REFILLORDRAR          
065600*                                 STOP TIME FOR REFILL ORDERS             
065700     03 CLAG-TIUPPDAT-EMB    PIC S9(7)           COMP-3.                  
065800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
065900*                                 UPDATING DATE     (YYMMDD)              
066000     03 CLAG-VKART           PIC S9(7)           COMP-3.                  
066100*                                 ARTIKELVIKT (G)                         
066200*                                 PART WEIGHT (G)                         
066300     03 CLAG-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
066400*                                 ARTIKELVOLYM (CM3)                      
066500*                                 PART VOLUME    (CM3)                    
066600     03 CLAG-KDUVKNTO        PIC X.                                       
066700*                                 KOD HUR NETTOVIKT UPPDATERAD            
066800*                                 CODE FOR HOW NET WEIGHT UPDATED         
066900     03 CLAG-IDUSER-VUPD     PIC X(8).                                    
067000*                                 USER MANUELL UPPD VIKT/VOL/VSOP         
067100*                                 USER MAN UPD OF VOL/WEIGHT/VSOP         
067200     03 CLAG-VKART-NTO       PIC S9(9)           COMP-3.                  
067300*                                 ARTIKELNS NETTOVIKT                     
067400*                                 PART NET WEIGHT                         
067500     03 CLAG-TIUPPDAT-VUPD   PIC S9(7)           COMP-3.                  
067600*                                 UPPDATERING ARTIKELNS VIKT              
067700*                                 UPDATING DATE OF PART WEIGHT            
067800     03 CLAG-KDOTFREK        PIC X.                                       
067900*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
068000*                                 ORDER HIT FREQUENCY FOR PART            
068100     03 CLAG-FLAUTREL        PIC X.                                       
068200*                                 AUT. SPÄRR FÖR VOR RELEASE              
068300*                                 AUTOMATIC BLOCK FOR AUT RELEASE         
068400*** END OF VILMAII-COPY LENGTH= 840 BYTES                                 
