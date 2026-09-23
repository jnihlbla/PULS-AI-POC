000100 01  CLAG-W01160.                                                         
000200*                                 A COPY OF WDK601 OCH WDK611             
000300*                                  (ALSO CALLED LAGERBAND)                
000400     03 CLAG-WDK601.                                                      
000500*                                 ARTIKELINFORMATION                      
000600*                                 FYSISK NYCKEL IDARTNR                   
000700*                                 SÖKBEGREPP    KDERS (-UTG)              
000800        05 CLAG-IDARTNR      PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100        05 CLAG-FLERS        PIC X.                                       
001200*                                 TILLKOMMANDE ARTIKEL ?                  
001300        05 CLAG-FLIART       PIC X.                                       
001400*                                 ARTIKELN INGÅR I SATS                   
001500*                                 PART IN KIT                             
001600        05 CLAG-IDAO         OCCURS 5 TIMES                               
001700                             PIC X(10).                                   
001800*                                 ÄNDRINGSORDERNUMMER                     
001900*                                 DESIGN CHANGE NOTICE                    
002000        05 CLAG-IDFKNGRP     PIC S9(5)           COMP-3.                  
002100*                                 FUNKTIONSGRUPP                          
002200*                                 FUNCTION GROUP                          
002300        05 CLAG-IDFTG        PIC 9(2).                                    
002400*                                 FÖRETAGSID EKONOM REDOVISNING           
002500*                                 COMPANY IDENTITY ACCOUNTING             
002600        05 CLAG-IDLEVNR      PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900        05 CLAG-KDERS-UTG    PIC S9(3)           COMP-3.                  
003000*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003100*                                 OBSOLETION SUPERSESSION CODE            
003200        05 CLAG-KDPRODSL     PIC S9(3)           COMP-3.                  
003300*                                 PRODUKTSLAG                             
003400*                                 PRODUCT GROUP                           
003500        05 CLAG-KDSORT       PIC X(2).                                    
003600*                                 SORT-KOD                                
003700*                                 UNIT OF MEASURE                         
003800        05 CLAG-REKSIFFR     PIC S9              COMP-3.                  
003900*                                 KONTROLLSIFFRA                          
004000*                                 PART NO CHECK DIGIT                     
004100        05 CLAG-TIERSDAT     PIC S9(5)           COMP-3.                  
004200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
004300*                                 DATE OF SUPERSESSION (YYWWD)            
004400        05 CLAG-TIFINLV      PIC S9(5)           COMP-3.                  
004500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004600*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
004700        05 CLAG-TIREGDAT     PIC S9(7)           COMP-3.                  
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900*                                 REGISTRATION DATE (YYMMDD)              
005000        05 CLAG-TIURPROD     PIC S9(5)           COMP-3.                  
005100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
005200*                                 OUT OF PRODUCTION DATE (YYWW)           
005300        05 CLAG-TISOP        PIC S9(5)           COMP-3.                  
005400*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
005500*                                 START OF PRODUCTION,(YYWWD D=1)         
005600        05 CLAG-FLBRAND      PIC X.                                       
005700*                                 ARTIKEL MED VARUMÄRKESBILD              
005800*                                 PARTS WITH THE BRAND IMAGE              
005900        05 CLAG-FLBSNES      PIC X.                                       
006000*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
006100*                                 PARTS WITH MORE BUSINESS VALUE          
006200        05 CLAG-KDSOP        PIC X.                                       
006300*                                 VISAR NÄR START DAT ART GÄLLER          
006400*                                 START OF PARTS CAN BE APPLIED           
006500        05 CLAG-KVEOP        PIC 9(2).                                    
006600*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
006700*                                 YEARS TO KEEP PART AFTER EOP            
006800        05 CLAG-KDANSKSEG    PIC S9(5)           COMP-3.                  
006900*                                 KOD FÖR ANSKAFFNINGSSEGMENT             
007000*                                 CODE FOR PROCUREMENT SEGMENT            
007100        05 CLAG-IDCDS        PIC X(8).                                    
007200*                                 ANVÄNDARENS CDS ID                      
007300*                                 USER CDS SECURITY-IDENTITY              
007400        05 CLAG-KDARTSYS     PIC X(2).                                    
007500*                                 KOD FÖR SYST. ÄGARE AV ARTIKEL          
007600*                                 COD FOR OWNER SYSTEM OF A PART          
007700        05 CLAG-FILLERX2     PIC X(2).                                    
007800     03 CLAG-WDK611.                                                      
007900*                                 ARTIKEL-CLAGER INFO                     
008000*                                 FYSISK NYCKEL KDSEGKEY                  
008100        05 CLAG-KDSEGKEY     PIC X.                                       
008200*                                 TEKNISK SEGMENT-NYCKEL                  
008300*                                 TECHNICAL SEGMENT KEY                   
008400        05 CLAG-ADART-CD     OCCURS 4 TIMES.                              
008500*                                 ARTIKELADRESS I CD-LAGRET               
008600*                                 PARTS-ADRESS IN CD WAREHOUSE            
008700           07 CLAG-ADLAGOMR-CD                                            
008800                             PIC S9(3)           COMP-3.                  
008900*                                 LAGEROMRÅDE                             
009000*                                 AREA                                    
009100           07 CLAG-ADGANG-CD PIC S9(3)           COMP-3.                  
009200*                                 GÅNG                                    
009300*                                 AISLE                                   
009400           07 CLAG-ADPLATS-CD                                             
009500                             PIC S9(5)           COMP-3.                  
009600*                                 LAGERPLATSNUMMER                        
009700*                                 LOCATION                                
009800        05 CLAG-ADART.                                                    
009900*                                 ARTIKELADRESS I LAGRET                  
010000*                                 PARTS-ADRESS                            
010100           07 CLAG-ADLAGOMR  PIC S9(3)           COMP-3.                  
010200*                                 LAGEROMRÅDE                             
010300*                                 AREA                                    
010400           07 CLAG-ADGANG    PIC S9(3)           COMP-3.                  
010500*                                 GÅNG                                    
010600*                                 AISLE                                   
010700           07 CLAG-ADPLATS   PIC S9(5)           COMP-3.                  
010800*                                 LAGERPLATSNUMMER                        
010900*                                 LOCATION                                
011000        05 CLAG-ADART-SVS.                                                
011100*                                 ARTIKELADRESS I SVS-LAGRET              
011200*                                 PARTS-ADRESS IN SVS WAREHOUSE           
011300           07 CLAG-ADLAGOMR-SVS                                           
011400                             PIC S9(3)           COMP-3.                  
011500*                                 LAGEROMRÅDE                             
011600*                                 AREA                                    
011700           07 CLAG-ADGANG-SVS                                             
011800                             PIC S9(3)           COMP-3.                  
011900*                                 GÅNG                                    
012000*                                 AISLE                                   
012100           07 CLAG-ADPLATS-SVS                                            
012200                             PIC S9(5)           COMP-3.                  
012300*                                 LAGERPLATSNUMMER                        
012400*                                 LOCATION                                
012500        05 CLAG-ADINLOMR-BOA PIC X(4).                                    
012600*                                 BUFFERTOMRÅDE-ALTERNATIVT               
012700*                                 BUFFER AREA ALTERNATIVE                 
012800        05 CLAG-ADINPORT     PIC X(8).                                    
012900*                                 AVLASTNINGSPORT                         
013000*                                 LOADING GATE                            
013100        05 CLAG-BEFT         PIC S9(3)           COMP-3.                  
013200*                                 FÖRPACKNINGSTYP                         
013300*                                 PACKAGING TYPE                          
013400        05 CLAG-DAPBPLAN     PIC 9(8).                                    
013500*                                 DATUM KVPB-PLAN GILTIG TOM              
013600*                                 DATE KVPB-PLAN VALID UNTIL              
013700        05 CLAG-DASEASON     PIC 9(8).                                    
013800*                                 DATUM RESEASON-LEDTID GILTIG TO         
013900*                                 M                                       
014000*                                 DATE RESEASON-LEDTID VALID UNTI         
014100*                                 L                                       
014200        05 CLAG-DAXPOINT     PIC 9(8).                                    
014300*                                 POÄNGÄNDRINGSDATUM (ÅÅÅÅMMDD)           
014400*                                 POINT CHANGE DATE (YYYYMMDD)            
014500        05 CLAG-FLAVRART     PIC X.                                       
014600*                                 AVROPSARTIKEL                           
014700        05 CLAG-FLCDART      PIC X.                                       
014800*                                 CROSS-DOCKING PART                      
014900*                                 CROSS-DOCKING PART                      
015000        05 CLAG-FLEJBUFF     PIC X.                                       
015100*                                 EJ BUFFERTSTYRNING                      
015200*                                 NO BUFFER STEERING                      
015300        05 CLAG-FLFSP        PIC X.                                       
015400*                                 FÖRDELNINGSSPÄRR                        
015500*                                 BLOCKED FOR SPLIT                       
015600        05 CLAG-FLGEMART     PIC X.                                       
015700*                                 FLAGGA GEMENSAM ARTIKEL                 
015800*                                 COMMON PART FLAG                        
015900        05 CLAG-FLJIT        PIC X.                                       
016000*                                 JUST-IN-TIME FLAGGA                     
016100*                                 JUST-IN-TIME FLAG                       
016200        05 CLAG-FLLARM-BUF   PIC X.                                       
016300*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
016400        05 CLAG-FLLSRDEL     PIC X.                                       
016500*                                 LEVERERAS SOM RESDEL                    
016600        05 CLAG-FLLTKSP      PIC X.                                       
016700*                                 SPÄRR UTLEVERANS C2-LAGER               
016800        05 CLAG-FLMANAT      PIC X.                                       
016900*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
017000        05 CLAG-FLMANBK      PIC X.                                       
017100*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
017200        05 CLAG-FLMANGK      PIC X.                                       
017300*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
017400        05 CLAG-FLMANKP      PIC X.                                       
017500*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
017600        05 CLAG-FLMANLT      PIC X.                                       
017700*                                 MANUELLT SATT LEDTID ?                  
017800        05 CLAG-FLMANOPP     PIC X.                                       
017900*                                 MANUELLT SATT GODK. AV OP-PLAN?         
018000*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
018100        05 CLAG-FLMANOSK     PIC X.                                       
018200*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
018300*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
018400        05 CLAG-FLMANPB      PIC X.                                       
018500*                                 MANUELLT REGISTRERAT PB-TPO             
018600*                                 MANUALLY REGISTRATED PB-TPO             
018700        05 CLAG-FLMANQ       PIC X.                                       
018800*                                 MANUELL HEMTAGNINGSKVANTITET            
018900        05 CLAG-FLMARKSP     PIC X.                                       
019000*                                 MARKNADSSPÄRR                           
019100*                                 MARKET BLOCKING CODE                    
019200        05 CLAG-FLMPB        PIC X.                                       
019300*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
019400        05 CLAG-FLNYBER      PIC X.                                       
019500*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
019600        05 CLAG-FLOREGPB     PIC X.                                       
019700*                                 OREGELBUNDEN PROGNOS (PB) ?             
019800        05 CLAG-FLRADREF     PIC X.                                       
019900*                                 KOMPLETTERANDE INFO. KRÄVS              
020000*                                 ADDITIONAL INFORMATION REQUIRED         
020100        05 CLAG-FLREFILL     PIC X.                                       
020200*                                 REFILLARTIKEL                           
020300*                                 REFILLPART                              
020400        05 CLAG-FLRELSP      PIC X.                                       
020500*                                 RELEASEBLOCKAD ARTIKEL .                
020600*                                 BLOCKED PART                            
020700        05 CLAG-FLSKROT-AUTO PIC X.                                       
020800*                                 SKROTNING AUTOMATISKT BEORDRAD          
020900*                                 SCRAPPING AUTOMATICALLY ORDERED         
021000        05 CLAG-FLSKROT-BEORD                                             
021100                             PIC X.                                       
021200*                                 SKROTNING BEORDRAD AV ANSK              
021300*                                 SCRAPPING ORDERED BY PROCURER           
021400        05 CLAG-FLSKROT-BEV  PIC X.                                       
021500*                                 BEVAKNING 2 ÅR INFÖR SKROTNING          
021600*                                 2 YEARS BEFORE SCRAPPING CHECK          
021700        05 CLAG-FLSKROT-SL   PIC X.                                       
021800*                                 SÄKERHETSLAGER NOLL INFÖR SKROT         
021900*                                 STOCK TO ZERO BEFORE SCRAPPING          
022000        05 CLAG-FLSKROT-WLC  PIC X.                                       
022100*                                 SISTA AVROP FÖRE SKROT                  
022200*                                 LAST CALL BEFORE SCRAPPING              
022300        05 CLAG-FLSPKOST     PIC X.                                       
022400*                                 SPECIELLA KOSTNADER FINS                
022500*                                 SPECIAL COSTS EXIST                     
022600        05 CLAG-FLTOPP       PIC X.                                       
022700*                                 TOPP-200-ARTIKEL                        
022800*                                 TOP 200 PART                            
022900        05 CLAG-FLTPO1       PIC X.                                       
023000*                                 ARTIKELN GODKÄND FÖR TPO1               
023100*                                 TPO1 ALLOWED FOR ARTICLE                
023200        05 CLAG-IDANSK       PIC S9(3)           COMP-3.                  
023300*                                 ANSKAFFARNUMMER                         
023400*                                 PROCURER NO.                            
023500        05 CLAG-IDARTNR-EMBQ0                                             
023600                             PIC S9(9)           COMP-3.                  
023700*                                 EMBALLAGEARTIKELNR FÖR Q0               
023800        05 CLAG-IDARTNR-EMBQ1                                             
023900                             PIC S9(9)           COMP-3.                  
024000*                                 EMBALLAGEARTIKELNR FÖR Q1               
024100        05 CLAG-IDARTNR-EMBQ2                                             
024200                             PIC S9(9)           COMP-3.                  
024300*                                 EMBALLAGEARTIKELNR FÖR Q2               
024400        05 CLAG-IDARTNR-EMBQ3                                             
024500                             PIC S9(9)           COMP-3.                  
024600*                                 EMBALLAGEARTIKELNR FÖR Q3               
024700        05 CLAG-IDARTNR-EMBQ4                                             
024800                             PIC S9(9)           COMP-3.                  
024900*                                 EMBALLAGEARTIKELNR FÖR Q4               
025000        05 CLAG-IDBERED      PIC S9(3)           COMP-3.                  
025100*                                 BEREDARENUMMER                          
025200        05 CLAG-IDDC-REF     PIC X(2).                                    
025300*                                 SÄNDANDE LAGER FÖR REFILL               
025400*                                 SENDING WAREHOUSE FOR REFILL            
025500        05 CLAG-IDFS-SEN     PIC X(8).                                    
025600*                                 FÖLJESEDELSNUMMER SENASTE INLEV         
025700*                                 ADVICE NOTE NUMBER ODETTE               
025800        05 CLAG-IDINK        PIC X(4).                                    
025900*                                 INKÖPARNUMMER                           
026000*                                 PURCHASE IDENTIFICATION NUMBER          
026100        05 CLAG-IDKAT        OCCURS 3 TIMES                               
026200                             PIC X(5).                                    
026300*                                 KATALOGBETECKNING                       
026400        05 CLAG-IDLEVNR-SEN  PIC X(5).                                    
026500*                                 SENASTE LEVERANTÖR                      
026600        05 CLAG-IDLEVNR-SHIP PIC X(5).                                    
026700*                                 SKEPPANDE LEVERANTÖR                    
026800*                                 SHIPPING SUPPLIER                       
026900        05 CLAG-IDLKTO       PIC S9(7)           COMP-3.                  
027000*                                 LAGERKONTO (FFHHHUU)                    
027100*                                 STOCK ACCOUNT (CCMMMSS)                 
027200        05 CLAG-IDPLANGR-AG  PIC S9              COMP-3.                  
027300*                                 PLANERINGSGRUPP ANSKAFFARE              
027400        05 CLAG-IDPLANGR-LEV PIC S9              COMP-3.                  
027500*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
027600        05 CLAG-IDPROENH     OCCURS 3 TIMES                               
027700                             PIC X(8).                                    
027800*                                 PRODUKTIONSENHET                        
027900*                                 PRODUCTION UNIT                         
028000        05 CLAG-IDPROJ       PIC X(4).                                    
028100*                                 PARTS PROJEKTIDENTITET                  
028200*                                 PARTS PROJECT IDENTITY                  
028300        05 CLAG-IDPROJUP     PIC X(8).                                    
028400*                                 PROJEKTUPPDRAG                          
028500*                                 PROJECT ASSIGNMENT                      
028600        05 CLAG-IDPSN        PIC 9(3).                                    
028700*                                 PROPER SHIPPING NAME                    
028800*                                 PROPER SHIPPING NAME                    
028900        05 CLAG-IDRITN       PIC X(10).                                   
029000*                                 RITNINGSNUMMER                          
029100*                                 DRAWING NUMBER                          
029200        05 CLAG-IDSTATNR     OCCURS 6 TIMES                               
029300                             PIC S9(9)           COMP-3.                  
029400*                                 STATISTISKT NUMMER                      
029500*                                 1 = NORSKT                              
029600*                                 2 = ENGELSKT                            
029700*                                 3 = BELGISKT                            
029800*                                 4 = PERUANSKT                           
029900*                                 5 = SVENSKT                             
030000*                                 6 =                                     
030100*                                 STATISTICAL NO.                         
030200        05 CLAG-IDUSER-EMB   PIC X(8).                                    
030300*                                 ANVÄNDARENS SÄKERHETS ID                
030400*                                 USER SECURITY-IDENTITY                  
030500        05 CLAG-IDUSER-SPKVAL                                             
030600                             PIC X(8).                                    
030700*                                 ANVÄNDAR-ID KVALITETSPÄRR               
030800*                                 USER ID QUALITY ERROR                   
030900        05 CLAG-KDAGE        PIC X.                                       
031000*                                 AGE-CODE                                
031100*                                 AGE-CODE                                
031200        05 CLAG-KDARTHNT     PIC S9(7)           COMP-3.                  
031300*                                 HANTERINGSKOD                           
031400*                                 HANDLING CODE                           
031500        05 CLAG-KDARTURS     PIC X(2).                                    
031600*                                 ARTIKELURSPRUNGSKOD                     
031700*                                 COUNTRY OF ORIGIN                       
031800        05 CLAG-KDAVT        PIC S9              COMP-3.                  
031900*                                 AVTALSMÄRKNING                          
032000*                                 AGREEMENT CODE                          
032100        05 CLAG-KDBPSR       PIC S9              COMP-3.                  
032200*                                 BASLAGERFÖRSLAGSNIVÅ                    
032300*                                 BASIC PART STOCK RECOMMENDATION         
032400        05 CLAG-KDEFFMAN     PIC X.                                       
032500*                                 EMIL-KOD                                
032600*                                 EMIL-CODE                               
032700        05 CLAG-KDEMBKOD-0   PIC S9(3)           COMP-3.                  
032800*                                 EMBALLAGEKOD 0                          
032900        05 CLAG-KDEMBKOD-1   PIC S9(3)           COMP-3.                  
033000*                                 EMBALLAGEKOD 1                          
033100        05 CLAG-KDEMBKOD-2   PIC S9(3)           COMP-3.                  
033200*                                 EMBALLAGEKOD 2                          
033300        05 CLAG-KDEMBVOL     PIC X.                                       
033400*                                 VOLUME UPDATED CODE                     
033500*                                 VOLUME UPDATED CODE                     
033600        05 CLAG-KDERS        PIC S9(3)           COMP-3.                  
033700*                                 ERSÄTTNINGSKOD                          
033800*                                 SUPERSESSION CODE                       
033900        05 CLAG-KDEXCHA      PIC S9(3)           COMP-3.                  
034000*                                 EXCHANGE ACCOUNT CODE                   
034100        05 CLAG-KDFARLIG     PIC S9              COMP-3.                  
034200*                                 KOD FÖR FARLIGT GODS                    
034300*                                 DANGEROUS GOODS CODE                    
034400        05 CLAG-KDFORP.                                                   
034500*                                 FÖRPACKNINGSKOD                         
034600*                                 PACKAGING CODE                          
034700           07 CLAG-KDFORPPL  PIC 9.                                       
034800*                                 FÖRPACKNINGSPLATS                       
034900*                                 PREPACKING PLACE                        
035000           07 CLAG-KDFORPGP  PIC 9(2).                                    
035100*                                 FÖRPACKNINGSGRUPP                       
035200*                                 PREPACKING GROUP                        
035300           07 CLAG-KDFORPUF  PIC 9.                                       
035400*                                 UPPRÄKNINGSFAKTOR                       
035500*                                 ENUMERATION                             
035600        05 CLAG-KDFREKKL     PIC X.                                       
035700*                                 FREKVENSKLASS                           
035800*                                 FREQ. CLASS                             
035900        05 CLAG-KDGK         PIC S9              COMP-3.                  
036000*                                 GODSMOTTAGAREKOD                        
036100*                                 GOODS RECEIVING WAREHOUSE CODE          
036200        05 CLAG-KDHF         PIC S9              COMP-3.                  
036300*                                 HUVUDFÖRRÅDSMÄRKNING                    
036400*                                 CODE MAIN STORAGE                       
036500        05 CLAG-KDKG         PIC S9              COMP-3.                  
036600*                                 KURANSGRUPP                             
036700*                                 TURNOVER CODE                           
036800        05 CLAG-KDKSP        PIC S9              COMP-3.                  
036900*                                 KÖPSPÄRR                                
037000*                                 PURCHASE BLOCKING CODE                  
037100        05 CLAG-KDLEVPLF     PIC X.                                       
037200*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
037300*                                 CODE FOR APPROVAL OF SCHEDULE P         
037400*                                 ROPOSAL                                 
037500        05 CLAG-KDLEVSP      PIC S9(3)           COMP-3.                  
037600*                                 SPÄRRKOD LEVERANS                       
037700*                                 DELIVERY BLOCKING CODE                  
037800        05 CLAG-KDLPSP       PIC S9              COMP-3.                  
037900*                                 LEVERANSPLANESPÄRR                      
038000        05 CLAG-KDLTK        PIC S9              COMP-3.                  
038100*                                 LAGERTILLHÖRIGHETSKOD                   
038200*                                 STOCK BELONGING CODE                    
038300        05 CLAG-KDOPPLAN     PIC X.                                       
038400*                                 OPTIMAL PLAN INOM FRYSTID               
038500*                                 OPTIMAL PLAN WITHIN FREEZTIME           
038600        05 CLAG-KDPCOO       PIC X.                                       
038700*                                 FÖRMÅNS AVTAL URSPRUNGSLAND             
038800*                                 PREF.AGREEEMENT COUNTRY ORIGIN          
038900        05 CLAG-KDPRISKL     PIC X.                                       
039000*                                 PRISKLASS                               
039100*                                 PRICE CLASS                             
039200        05 CLAG-KDPSLLOC     PIC 9(2).                                    
039300*                                 PRODUKTSLAG LOKALT                      
039400*                                 PRODUCT GROUP LOCAL                     
039500        05 CLAG-KDSPEEMB     PIC 9.                                       
039600*                                 SPECIALEMBALLAGEKOD                     
039700*                                 SPECIAL PACKING CODE                    
039800        05 CLAG-KDSRA        PIC S9(3)           COMP-3.                  
039900*                                 SRA-KOD                                 
040000*                                 SRA CODE                                
040100        05 CLAG-KDTIPPR      PIC S9              COMP-3.                  
040200*                                 TIPPAT PRIS KOD                         
040300*                                 ESTIMATED PRICE CODE                    
040400        05 CLAG-KDTULLRE     PIC S9              COMP-3.                  
040500*                                 TULLRESTITUTION MÄRKNING                
040600*                                 CUSTOMS RESTITUAT.                      
040700        05 CLAG-KDUART       PIC X.                                       
040800*                                 UNDANTAGSARTIKEL                        
040900*                                 EXECPTION PARTS                         
041000        05 CLAG-KDVSOP       PIC S9(3)           COMP-3.                  
041100*                                 VSOP-KOD                                
041200*                                 VSOP-CODE                               
041300        05 CLAG-KDVTH        PIC S9              COMP-3.                  
041400*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
041500*                                 CODE FOR COST RESPONSIBILITY            
041600        05 CLAG-KDVVKL       PIC S9              COMP-3.                  
041700*                                 VOLYMVÄRDESKLASS                        
041800*                                 VOLUME VALUE CLASS                      
041900        05 CLAG-KDYTBEH      PIC S9(3)           COMP-3.                  
042000*                                 YTBEHANDLINGSKOD                        
042100*                                                                         
042200        05 CLAG-KVAKS-CDC    PIC S9(7)           COMP-3.                  
042300*                                 DEL AV AK SOM LIGGER I CDC              
042400*                                 PART OF AK IN THE CDC                   
042500        05 CLAG-KVAKS-PAV    PIC S9(7)           COMP-3.                  
042600*                                 DEL AV AK PÅ VÄG                        
042700*                                 PART OF AK ON ITS WAY                   
042800        05 CLAG-KVAKS-T      PIC S9(7)           COMP-3.                  
042900*                                 DEL AV AK I EN TERMINAL                 
043000*                                 PART OF AK IN A TERMINAL                
043100        05 CLAG-KVAP         PIC S9(7)           COMP-3.                  
043200*                                 ANNULLATIONSPUNKT                       
043300        05 CLAG-KVAVIS-SEN   PIC S9(7)           COMP-3.                  
043400*                                 SENAST AVISERAT ANTAL                   
043500        05 CLAG-KVAVROP-TOT  PIC S9(7)           COMP-3.                  
043600*                                 ALLA AVROP MED KOD = 2                  
043700        05 CLAG-KVBEART      PIC S9(7)           COMP-3.                  
043800*                                 BESTÄLLT ANTAL STYCKEN                  
043900*                                 ORDERED QUANTITY                        
044000        05 CLAG-KVBK         PIC S9(7)           COMP-3.                  
044100*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
044200        05 CLAG-KVDAGAR-FFH  PIC S9(3)           COMP-3.                  
044300*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
044400        05 CLAG-KVDAGAR-INLEV                                             
044500                             PIC S9(3)           COMP-3.                  
044600*                                 INLEVERANSTID     (ANTAL DAGAR)         
044700        05 CLAG-KVDAGAR-TT   PIC S9(3)           COMP-3.                  
044800*                                 DAGAR TULL- OCH TRANSPORT-TID           
044900        05 CLAG-KVEFRS       PIC S9(7)           COMP-3.                  
045000*                                 EJ FAKTURERAT ANTAL STYCK               
045100*                                 ORDERED NOT INVOICED QTY                
045200        05 CLAG-KVEOQ        PIC S9(7)           COMP-3.                  
045300*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
045400*                                 ET                                      
045500        05 CLAG-KVFRYSTI     PIC S9(3)           COMP-3.                  
045600*                                 FRYSTID FÖR TPO-ORDER                   
045700*                                 FREEZTIME FOR TPO                       
045800        05 CLAG-KVINVS       PIC S9(7)           COMP-3.                  
045900*                                 INVENTERINGSSALDO                       
046000*                                 STOCK-TAKING BALANCE                    
046100        05 CLAG-KVKP         PIC S9(7)           COMP-3.                  
046200*                                 KÖPPUNKT                                
046300        05 CLAG-KVLAAN       PIC S9(7)           COMP-3.                  
046400*                                 LÅNESALDO                               
046500        05 CLAG-KVLS         PIC S9(7)           COMP-3.                  
046600*                                 LAGERSALDO                              
046700*                                 STOCK BALANCE                           
046800        05 CLAG-KVLS-CD      OCCURS 4 TIMES                               
046900                             PIC S9(7)           COMP-3.                  
047000*                                 LAGERSALDO CD                           
047100*                                 STOCK BALANCE CD                        
047200        05 CLAG-KVLS-SVS     PIC S9(7)           COMP-3.                  
047300*                                 LAGERSALDO SVS                          
047400*                                 STOCK BALANCE SVS                       
047500        05 CLAG-KVMAD-SEP    PIC S9(6)V9(1)      COMP-3.                  
047600*                                 SEPARAT PROGNOSFEL                      
047700        05 CLAG-KVMAD-TOT    PIC S9(6)V9(1)      COMP-3.                  
047800*                                 TOTALT PROGNOSFEL                       
047900        05 CLAG-KVMAXPL      PIC S9(7)           COMP-3.                  
048000*                                 MAX ANTAL (STYCK) PÅ PLOCKPLATS         
048100*                                 MAX ALLOWED QTY IN PICKING LOC          
048200        05 CLAG-KVMP         PIC S9(7)           COMP-3.                  
048300*                                 MAXPUNKT                                
048400*                                 MAXIMUM POINT                           
048500        05 CLAG-KVOI-OVR     PIC S9(7)           COMP-3.                  
048600*                                 ORDERINGÅNG LEV ÖVRIGT                  
048700*                                 ORDERED PCS PER TIME UNIT OTHER         
048800        05 CLAG-KVOI-PLOCK   PIC S9(7)           COMP-3.                  
048900*                                 ORDERINGÅNG LEV FRÅN PLOCKPLATS         
049000*                                 ORDERED FROM PICKING AREA               
049100        05 CLAG-KVOVERF      PIC S9(7)           COMP-3.                  
049200*                                 ÖVERFÖRINGSSALDO                        
049300        05 CLAG-KVPALL       PIC S9(7)           COMP-3.                  
049400*                                 ANTAL I PALL                            
049500*                                 QUANTITY IN PALLET                      
049600        05 CLAG-KVPB-HIST    PIC S9(6)V9(1)      COMP-3.                  
049700*                                 PB (PROGNOS) HISTORISKT CDC             
049800*                                 HISTORIC REQUIREMENTS CDC               
049900        05 CLAG-KVPB-PLAN    PIC S9(6)V9(1)      COMP-3.                  
050000*                                 PLANERAT PERIODBEHOV                    
050100*                                 PLANNED PERIOD REQUIREMENTS             
050200        05 CLAG-KVPB-PLAN-JUST1                                           
050300                             PIC S9(6)V9(1)      COMP-3.                  
050400*                                 PLANERAT PERIODBEHOV JUST1              
050500*                                 PLANNED PERIOD REQUIREMENT ADJ1         
050600        05 CLAG-KVPB-PLAN-JUST2                                           
050700                             PIC S9(6)V9(1)      COMP-3.                  
050800*                                 PLANERAT PERIODBEHOV JUST2              
050900*                                 PLANNED PERIOD REQUIREMENT ADJ2         
051000        05 CLAG-KVPB-SATS    PIC S9(6)V9(1)      COMP-3.                  
051100*                                 SATS-PERIODBEHOV                        
051200*                                 KIT PERIOD REQUIREMENTS                 
051300        05 CLAG-KVPB-SEP     PIC S9(6)V9(1)      COMP-3.                  
051400*                                 SEPARAT PERIODBEHOV                     
051500*                                 SEPARATE PERIOD REQUIREMENTS            
051600        05 CLAG-KVPB-TPO     PIC S9(6)V9(1)      COMP-3.                  
051700*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
051800*                                 PERIODICAL DEMAND TPO1 AND TPO2         
051900*                                                                         
052000        05 CLAG-KVPB-TREND   PIC S9(6)V9(1)      COMP-3.                  
052100*                                 PERIODTRENDVÄRDE                        
052200        05 CLAG-KVPB-VESL    PIC S9(6)V9(1)      COMP-3.                  
052300*                                 GÄLLANDE PB VID VECKOSLUT               
052400        05 CLAG-KVPOINT      PIC S9(7)           COMP-3.                  
052500*                                 POINT VALUE                             
052600        05 CLAG-KVQ          PIC S9(7)           COMP-3.                  
052700*                                 EKONOMISK HEMTAGNINGSKVANTITET          
052800        05 CLAG-KVQ-JUST     PIC S9(7)           COMP-3.                  
052900*                                 NY EKON HEMTAGNINGSKVANTITET            
053000        05 CLAG-KVQPACK-0    PIC S9(5)           COMP-3.                  
053100*                                 ANTAL I Q0 FÖRPACKNING                  
053200        05 CLAG-KVQPACK-1    PIC S9(5)           COMP-3.                  
053300*                                 ANTAL I Q1 FÖRPACKNING                  
053400*                                 QUANTITY IN BULK PACK Q1                
053500        05 CLAG-KVQPACK-2    PIC S9(5)           COMP-3.                  
053600*                                 ANTAL I Q2 FÖRPACKNING                  
053700*                                 QUANTITY IN BULK PACK Q2                
053800        05 CLAG-KVQPACK-3    PIC S9(5)           COMP-3.                  
053900*                                 ANTAL I Q3 FÖRPACKNING                  
054000*                                 QUANTITY IN BULK PACK Q3                
054100        05 CLAG-KVQPACK-4    PIC S9(5)           COMP-3.                  
054200*                                 ANTAL I Q4 FÖRPACKNING                  
054300*                                 QUANTITY IN BULK PACK Q4                
054400        05 CLAG-KVREFBER-PLOCK                                            
054500                             PIC S9(7)           COMP-3.                  
054600*                                 BERÄKNAD REFILLINGKVANT-PLOCK           
054700*                                 CALCULATED REFILLING QTY-PICK           
054800        05 CLAG-KVREFOVL-TOT PIC S9(7)           COMP-3.                  
054900*                                 BERÄKNAD ÖVERLAGER CHILD DC:N           
055000*                                 CALCULATED OVERSTOCK ALL XDC            
055100        05 CLAG-KVREFPKT-PLOCK                                            
055200                             PIC S9(7)           COMP-3.                  
055300*                                 BERÄKNAD PÅFYLLNADSPUNKT-PLOCK          
055400*                                 CALCULATED REFILLING POINT-PICK         
055500        05 CLAG-KVRESS       PIC S9(7)           COMP-3.                  
055600*                                 RESERVERAT ANTAL ARTIKLAR               
055700*                                 QUANTITY RESERVED ITEMS                 
055800        05 CLAG-KVRESS-CD    OCCURS 4 TIMES                               
055900                             PIC S9(7)           COMP-3.                  
056000*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
056100*                                  DOCKING LAGER                          
056200*                                 QUANTITY RESERVED ITEMS IN CROS         
056300*                                 S DOCKING WAREHOUSE                     
056400        05 CLAG-KVRETUR      PIC S9(7)           COMP-3.                  
056500*                                 ANTAL I RETUR                           
056600*                                 QUANTITY IN RETURN                      
056700        05 CLAG-KVRETUR-TOT  PIC S9(7)           COMP-3.                  
056800*                                 ANTAL RETURER TOTALT                    
056900*                                 QUANTITY TOTAL RETURNS                  
057000        05 CLAG-KVROS        PIC S9(7)           COMP-3.                  
057100*                                 RESTORDERSALDO                          
057200*                                 BACKORDER QTY                           
057300        05 CLAG-KVSLAGER     PIC S9(7)           COMP-3.                  
057400*                                 SÄKERHETSLAGER                          
057500*                                 SAFETY STOCK                            
057600        05 CLAG-KVSLAGER-OPT PIC S9(7)           COMP-3.                  
057700*                                 OPTIMALT SÄKERHETSLAGER                 
057800*                                 OPT SAFETY STOCK                        
057900        05 CLAG-KVSLUTKP     PIC S9(7)           COMP-3.                  
058000*                                 SLUTKÖPSSALDO                           
058100        05 CLAG-KVSPANT      PIC S9(7)           COMP-3.                  
058200*                                 SPÄRRAT ANTAL                           
058300*                                 BLOCKED QTY                             
058400        05 CLAG-KVSPARR-KVAL PIC S9(7)           COMP-3.                  
058500*                                 SPÄRRAT ANTAL KVALITETSFEL              
058600*                                 BLOCKED QUANTITY QUALITY ERROR          
058700        05 CLAG-KVTILLG-TOT  PIC S9(7)           COMP-3.                  
058800*                                 LAGERTILLGÅNG CDC TOTALT                
058900        05 CLAG-KVULOAD      PIC S9(7)           COMP-3.                  
059000*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
059100*                                 MIN LOAD FROM SUPPLIER                  
059200        05 CLAG-KVUTJFEL     PIC S9(6)V9(1)      COMP-3.                  
059300*                                 UTJÄMNAT FEL                            
059400        05 CLAG-KVUTRS       PIC S9(7)           COMP-3.                  
059500*                                 UTREDNINGSSALDO                         
059600*                                 INVESTIGATION BALANCE                   
059700        05 CLAG-KVVECKOR-AT  PIC S9(3)           COMP-3.                  
059800*                                 ANTAL VECKOR ANSKAFFNINGSTID            
059900        05 CLAG-KVVECKOR-BT  PIC S9(3)           COMP-3.                  
060000*                                 ANTAL VECKOR BESTÄLLNINGSTID            
060100        05 CLAG-KVVECKOR-FT  PIC S9(3)           COMP-3.                  
060200*                                 ANTAL VECKOR FRYSNINGSTID               
060300        05 CLAG-KVVECKOR-LT  PIC S9(3)           COMP-3.                  
060400*                                 ANTAL VECKOR LEDTID                     
060500        05 CLAG-KVVECKOR-LVAR                                             
060600                             PIC S9(2)V9(1)      COMP-3.                  
060700*                                 VARIANS I LEDTIDEN                      
060800*                                                                         
060900        05 CLAG-KVVECKOR-TREND                                            
061000                             PIC S9(3)           COMP-3.                  
061100*                                 ANTAL VECKOR TRENDVÄRDE                 
061200        05 CLAG-KVVORKO      PIC S9(7)           COMP-3.                  
061300*                                 VOR-KÖ KVANT                            
061400*                                 VOR-QUEUE QUANT                         
061500        05 CLAG-PRARTBTO-EXP PIC S9(7)V9(2)      COMP-3.                  
061600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
061700*                                 GROSS-PRICE EXPORT                      
061800*                                  (FOB-GROSS)                            
061900        05 CLAG-PRARTSJK     PIC S9(7)V9(2)      COMP-3.                  
062000*                                 ARTIKELNS SJÄLVKOSTNAD                  
062100*                                 COST OF SALES                           
062200        05 CLAG-PRARTSTD     PIC S9(7)V9(2)      COMP-3.                  
062300*                                 ARTIKELSTANDARDPRIS                     
062400*                                 STANDARD PRICE                          
062500        05 CLAG-PRDIRLON     PIC S9(4)V9(3)      COMP-3.                  
062600*                                 DIREKT LÖN                              
062700*                                 SURCHARGE COSTS                         
062800        05 CLAG-PRDMTRL      PIC S9(6)V9(3)      COMP-3.                  
062900*                                 DIREKT MATERIAL                         
063000*                                 SURCHARGE PACKING MATERIAL              
063100        05 CLAG-PRHEMTAG     PIC S9(7)V9(2)      COMP-3.                  
063200*                                 HEMTAGNINGSKOSTNAD                      
063300*                                 TRANSPORT COST                          
063400        05 CLAG-PRINK        PIC S9(7)V9(2)      COMP-3.                  
063500*                                 INKÖPSPRIS                              
063600*                                 PURCHASE PRICE                          
063700        05 CLAG-PRLFKST      PIC S9(3)V9(2)      COMP-3.                  
063800*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
063900*                                 SUPPLIERS PACKING AND HANDLING          
064000        05 CLAG-PRORDSK      PIC S9(5)V9(2)      COMP-3.                  
064100*                                 ORDERSÄRKOSTNAD                         
064200*                                 REMAINING OVERHEAD SURCHARGE            
064300        05 CLAG-PROVRPAL     PIC S9(4)V9(3)      COMP-3.                  
064400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
064500*                                 REMAINING OVERHEAD SURCHARGE            
064600        05 CLAG-PRREF        PIC S9(7)V9(2)      COMP-3.                  
064700*                                 REFERENCE PRICE                         
064800        05 CLAG-REDIRLEV     PIC S9V9(2)         COMP-3.                  
064900*                                 DIREKTLEVERANSANDEL                     
065000        05 CLAG-RESEASON-PLAN                                             
065100                             OCCURS 12 TIMES                              
065200                             PIC S9V9(2)         COMP-3.                  
065300*                                 SÄSONGSINDEX INKLUSIVE REFILL           
065400        05 CLAG-RESLJUST     PIC S9(2)V9(1)      COMP-3.                  
065500*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
065600*                                 ADJUSTMENT ALGORITM                     
065700        05 CLAG-RETULF       PIC S9(3)V9(4)      COMP-3.                  
065800*                                 TULLFAKTOR                              
065900*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
066000        05 CLAG-RVPROFEL     PIC S9(3)           COMP-3.                  
066100*                                 ANTAL STORA PROGNOSFEL                  
066200        05 CLAG-RVPROURS     PIC S9(3)           COMP-3.                  
066300*                                 ANTAL PROGNOSFEL I FÖLJD                
066400        05 CLAG-TIAVIDAT-SEN PIC S9(7)           COMP-3.                  
066500*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
066600        05 CLAG-TIBESRPT     PIC S9(5)           COMP-3.                  
066700*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
066800*                                 (ÅÅVV)                                  
066900        05 CLAG-TIBESRPT-PAAM                                             
067000                             PIC S9(5)           COMP-3.                  
067100*                                 PÅMINNELSEDATUM FÖR                     
067200*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
067300        05 CLAG-TIDATUM-TREND                                             
067400                             PIC S9(7)           COMP-3.                  
067500*                                 JUSTERAD TREND AAMMDD                   
067600*                                 LAST TREND CHANGE  YYMMDD               
067700        05 CLAG-TIDISPIN     PIC S9(7)           COMP-3.                  
067800*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
067900*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
068000        05 CLAG-TIGILTIG-PCOO                                             
068100                             PIC S9(7)           COMP-3.                  
068200*                                 GILTIGHETSDATUM PCOO                    
068300*                                                                         
068400*                                 DATE OF VALIDITY FOR PCOO               
068500        05 CLAG-TIINVDAT     PIC S9(5)           COMP-3.                  
068600*                                 INVENTERINGSDATUM                       
068700*                                 STOCKTAKING DATE                        
068800        05 CLAG-TILEVDAG     OCCURS 5 TIMES                               
068900                             PIC S9              COMP-3.                  
069000*                                 AVSÄNDNINGSDAG INOM VECKA               
069100*                                 DELIVERY WEEK DAY                       
069200        05 CLAG-TILPSP       PIC S9(5)           COMP-3.                  
069300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
069400        05 CLAG-TILTK        PIC S9(5)           COMP-3.                  
069500*                                 LTK-ÄNDRINGSDATUM                       
069600        05 CLAG-TIMAIL-KVAL  PIC 9(6).                                    
069700*                                 MAIL DATUM                              
069800*                                 MAIL DATE                               
069900        05 CLAG-TIOMSPEC     PIC S9(5)           COMP-3.                  
070000*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
070100        05 CLAG-TIPBDAT      PIC S9(5)           COMP-3.                  
070200*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
070300        05 CLAG-TIPBLOCK     PIC S9(7)           COMP-3.                  
070400*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
070500*                                 ÅMMDD                                   
070600*                                 FUTURE DATE FOR FORECAST TO BE          
070700*                                 BLOCKED                                 
070800*                                 INCREASED FORECAST IS ALLOWED           
070900        05 CLAG-TIPBPLAN-JUST1-FOM                                        
071000                             PIC S9(7)           COMP-3.                  
071100*                                 FOM PB-PLAN DATUM - JUST1               
071200*                                 FROM PB-PLAN DATE - ADJ1                
071300        05 CLAG-TIPBPLAN-JUST1-TOM                                        
071400                             PIC S9(7)           COMP-3.                  
071500*                                 TOM PB-PLAN DATUM - JUST1               
071600*                                 UNTIL PB-PLAN DATE - ADJ1               
071700        05 CLAG-TIPBPLAN-JUST2-FOM                                        
071800                             PIC S9(7)           COMP-3.                  
071900*                                 FOM PB-PLAN DATUM - JUST2               
072000*                                 FROM PB-PLAN DATE - ADJ2                
072100        05 CLAG-TIPBPLAN-JUST2-TOM                                        
072200                             PIC S9(7)           COMP-3.                  
072300*                                 TOM PB-PLAN DATUM - JUST2               
072400*                                 UNTIL PB-PLAN DATE - ADJ2               
072500        05 CLAG-TIQJUST      PIC S9(5)           COMP-3.                  
072600*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
072700        05 CLAG-TIREFSTO     PIC S9(7)           COMP-3.                  
072800*                                 BEORDRINGSSTOPPAD T.OM.                 
072900*                                 STOPPED FOR ORDERING UNTIL              
073000        05 CLAG-TIRODAT      PIC S9(7)           COMP-3.                  
073100*                                 RESTORDERDATUM         (ÅÅMMDD)         
073200*                                 BACK ORDER DATE        (YYMMDD)         
073300        05 CLAG-TISKPREL     PIC S9(5)           COMP-3.                  
073400*                                 PREL. SKROTNINGSDATUM (AAVV)            
073500*                                 PREL DATE OF SCRAPPING (YYWW)           
073600        05 CLAG-TISKROT      PIC S9(7)           COMP-3.                  
073700*                                 SKROTNINGSDATUM                         
073800*                                 DATE OF SCRAPPING                       
073900        05 CLAG-TISKROT-AUTO PIC S9(7)           COMP-3.                  
074000*                                 STOPDATE AUTO-SKROTNING                 
074100*                                 STOP DATE AUTOSCRAPPING                 
074200        05 CLAG-TISLJUST     PIC S9(5)           COMP-3.                  
074300*                                 VECKA DÅ JUSTERING AV SÄKER-            
074400*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
074500        05 CLAG-TISLUTKP     PIC S9(7)           COMP-3.                  
074600*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
074700*                                 CALC OF ATR-BAL IS TO COMMENCE          
074800        05 CLAG-TISPARR-KVAL PIC 9(6).                                    
074900*                                 SPÄRRAD DATUM KVALITETSFEL              
075000*                                 BLOCKED DATE QUALITY ERROR              
075100        05 CLAG-TISTODAT-LARM                                             
075200                             PIC S9(7)           COMP-3.                  
075300*                                 STOPPDATUM FÖR LARM-223                 
075400*                                 STOP DATE FOR ALARM-223                 
075500        05 CLAG-TISTOREF     PIC S9(5)           COMP-3.                  
075600*                                 STOPPTIDPUNKT FÖR REFILLORDRAR          
075700*                                 STOP TIME FOR REFILL ORDERS             
075800        05 CLAG-TIUPPDAT-EMB PIC S9(7)           COMP-3.                  
075900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
076000*                                 UPDATING DATE     (YYMMDD)              
076100        05 CLAG-VKART        PIC S9(7)           COMP-3.                  
076200*                                 ARTIKELVIKT (G)                         
076300*                                 PART WEIGHT (G)                         
076400        05 CLAG-VLARTNTO     PIC S9(8)V9(1)      COMP-3.                  
076500*                                 ARTIKELVOLYM (CM3)                      
076600*                                 PART VOLUME    (CM3)                    
076700        05 CLAG-KDUVKNTO     PIC X.                                       
076800*                                 KOD HUR NETTOVIKT UPPDATERAD            
076900*                                 CODE FOR HOW NET WEIGHT UPDATED         
077000        05 CLAG-IDUSER-VUPD  PIC X(8).                                    
077100*                                 USER MANUELL UPPD VIKT/VOL/VSOP         
077200*                                 USER MAN UPD OF VOL/WEIGHT/VSOP         
077300        05 CLAG-VKART-NTO    PIC S9(9)           COMP-3.                  
077400*                                 ARTIKELNS NETTOVIKT                     
077500*                                 PART NET WEIGHT                         
077600        05 CLAG-TIUPPDAT-VUPD                                             
077700                             PIC S9(7)           COMP-3.                  
077800*                                 UPPDATERING ARTIKELNS VIKT              
077900*                                 UPDATING DATE OF PART WEIGHT            
078000        05 CLAG-KDOTFREK     PIC X.                                       
078100*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
078200*                                 ORDER HIT FREQUENCY FOR PART            
078300        05 CLAG-FLAUTREL     PIC X.                                       
078400*                                 AUT. SPÄRR FÖR VOR RELEASE              
078500*                                 AUTOMATIC BLOCK FOR AUT RELEASE         
078600*** END OF VILMAII-COPY LENGTH= 950 BYTES                                 
