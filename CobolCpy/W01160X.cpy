000100 01  CLAG-W01160X.                                                        
000200*                                 A COPY OF W01160 - TO CREATE WX         
000300*                                 TR FILE IN EDITABLE FORMAT              
000400     03 CLAG-WDK601.                                                      
000500*                                 ARTIKELINFORMATION                      
000600*                                 FYSISK NYCKEL IDARTNR                   
000700*                                 SÖKBEGREPP    KDERS (-UTG)              
000800        05 CLAG-IDARTNR      PIC Z(7)9                                    
000900                             VALUE ZEROS.                                 
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200        05 CLAG-FLERS        PIC X                                        
001300                             VALUE SPACE.                                 
001400*                                 TILLKOMMANDE ARTIKEL ?                  
001500        05 CLAG-FLIART       PIC X                                        
001600                             VALUE SPACE.                                 
001700*                                 ARTIKELN INGÅR I SATS                   
001800*                                 PART IN KIT                             
001900        05 CLAG-IDAO         OCCURS 5 TIMES                               
002000                             PIC X(10)                                    
002100                             VALUE SPACES.                                
002200*                                 ÄNDRINGSORDERNUMMER                     
002300*                                 DESIGN CHANGE NOTICE                    
002400        05 CLAG-IDFKNGRP     PIC Z(3)9                                    
002500                             VALUE ZEROS.                                 
002600*                                 FUNKTIONSGRUPP                          
002700*                                 FUNCTION GROUP                          
002800        05 CLAG-IDFTG        PIC 9(2)                                     
002900                             VALUE ZEROS.                                 
003000*                                 FÖRETAGSID EKONOM REDOVISNING           
003100*                                 COMPANY IDENTITY ACCOUNTING             
003200        05 CLAG-IDLEVNR      PIC X(5)                                     
003300                             VALUE SPACES.                                
003400*                                 LEVERANTÖRNUMMER                        
003500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003600        05 CLAG-KDERS-UTG    PIC Z9                                       
003700                             VALUE ZEROS.                                 
003800*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003900*                                 OBSOLETION SUPERSESSION CODE            
004000        05 CLAG-KDPRODSL     PIC Z9                                       
004100                             VALUE ZEROS.                                 
004200*                                 PRODUKTSLAG                             
004300*                                 PRODUCT GROUP                           
004400        05 CLAG-KDSORT       PIC X(2)                                     
004500                             VALUE SPACES.                                
004600*                                 SORT-KOD                                
004700*                                 UNIT OF MEASURE                         
004800        05 CLAG-REKSIFFR     PIC 9                                        
004900                             VALUE ZERO.                                  
005000*                                 KONTROLLSIFFRA                          
005100*                                 PART NO CHECK DIGIT                     
005200        05 CLAG-TIERSDAT     PIC 9(5)                                     
005300                             VALUE ZEROS.                                 
005400*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
005500*                                 DATE OF SUPERSESSION (YYWWD)            
005600        05 CLAG-TIFINLV      PIC Z(4)9                                    
005700                             VALUE ZEROS.                                 
005800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005900*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
006000        05 CLAG-TIREGDAT     PIC 9(6)                                     
006100                             VALUE ZEROS.                                 
006200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006300*                                 REGISTRATION DATE (YYMMDD)              
006400        05 CLAG-TIURPROD     PIC 9(4)                                     
006500                             VALUE ZEROS.                                 
006600*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006700*                                 OUT OF PRODUCTION DATE (YYWW)           
006800        05 CLAG-TISOP        PIC Z(4)9                                    
006900                             VALUE ZEROS.                                 
007000*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
007100*                                 START OF PRODUCTION,(YYWWD D=1)         
007200        05 CLAG-FLBRAND      PIC X                                        
007300                             VALUE SPACE.                                 
007400*                                 ARTIKEL MED VARUMÄRKESBILD              
007500*                                 PARTS WITH THE BRAND IMAGE              
007600        05 CLAG-FLBSNES      PIC X                                        
007700                             VALUE SPACE.                                 
007800*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
007900*                                 PARTS WITH MORE BUSINESS VALUE          
008000        05 CLAG-KDSOP        PIC X                                        
008100                             VALUE SPACE.                                 
008200*                                 VISAR NÄR START DAT ART GÄLLER          
008300*                                 START OF PARTS CAN BE APPLIED           
008400        05 CLAG-KVEOP        PIC 9(2)                                     
008500                             VALUE ZEROS.                                 
008600*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
008700*                                 YEARS TO KEEP PART AFTER EOP            
008800     03 CLAG-WDK611.                                                      
008900*                                 ARTIKEL-CLAGER INFO                     
009000*                                 FYSISK NYCKEL KDSEGKEY                  
009100        05 CLAG-KDSEGKEY     PIC X                                        
009200                             VALUE SPACE.                                 
009300*                                 TEKNISK SEGMENT-NYCKEL                  
009400*                                 TECHNICAL SEGMENT KEY                   
009500        05 CLAG-ADART-CD     OCCURS 4 TIMES.                              
009600*                                 ARTIKELADRESS I CD-LAGRET               
009700*                                 PARTS-ADRESS IN CD WAREHOUSE            
009800           07 CLAG-ADLAGOMR-CD                                            
009900                             PIC Z9                                       
010000                             VALUE ZEROS.                                 
010100*                                 LAGEROMRÅDE                             
010200*                                 AREA                                    
010300           07 CLAG-ADGANG-CD PIC Z9                                       
010400                             VALUE ZEROS.                                 
010500*                                 GÅNG                                    
010600*                                 AISLE                                   
010700           07 CLAG-ADPLATS-CD                                             
010800                             PIC Z(4)9                                    
010900                             VALUE ZEROS.                                 
011000*                                 LAGERPLATSNUMMER                        
011100*                                 LOCATION                                
011200        05 CLAG-ADART.                                                    
011300*                                 ARTIKELADRESS I LAGRET                  
011400*                                 PARTS-ADRESS                            
011500           07 CLAG-ADLAGOMR  PIC Z9                                       
011600                             VALUE ZEROS.                                 
011700*                                 LAGEROMRÅDE                             
011800*                                 AREA                                    
011900           07 CLAG-ADGANG    PIC Z9                                       
012000                             VALUE ZEROS.                                 
012100*                                 GÅNG                                    
012200*                                 AISLE                                   
012300           07 CLAG-ADPLATS   PIC Z(4)9                                    
012400                             VALUE ZEROS.                                 
012500*                                 LAGERPLATSNUMMER                        
012600*                                 LOCATION                                
012700        05 CLAG-ADART-SVS.                                                
012800*                                 ARTIKELADRESS I SVS-LAGRET              
012900*                                 PARTS-ADRESS IN SVS WAREHOUSE           
013000           07 CLAG-ADLAGOMR-SVS                                           
013100                             PIC Z9                                       
013200                             VALUE ZEROS.                                 
013300*                                 LAGEROMRÅDE                             
013400*                                 AREA                                    
013500           07 CLAG-ADGANG-SVS                                             
013600                             PIC Z9                                       
013700                             VALUE ZEROS.                                 
013800*                                 GÅNG                                    
013900*                                 AISLE                                   
014000           07 CLAG-ADPLATS-SVS                                            
014100                             PIC Z(4)9                                    
014200                             VALUE ZEROS.                                 
014300*                                 LAGERPLATSNUMMER                        
014400*                                 LOCATION                                
014500        05 CLAG-ADINLOMR-BOA PIC X(4)                                     
014600                             VALUE SPACES.                                
014700*                                 BUFFERTOMRÅDE-ALTERNATIVT               
014800*                                 BUFFER AREA ALTERNATIVE                 
014900        05 CLAG-ADINPORT     PIC X(8)                                     
015000                             VALUE SPACES.                                
015100*                                 AVLASTNINGSPORT                         
015200*                                 LOADING GATE                            
015300        05 CLAG-BEFT         PIC Z9                                       
015400                             VALUE ZEROS.                                 
015500*                                 FÖRPACKNINGSTYP                         
015600*                                 PACKAGING TYPE                          
015700        05 CLAG-DAPBPLAN     PIC 9(8)                                     
015800                             VALUE ZEROS.                                 
015900*                                 DATUM KVPB-PLAN GILTIG TOM              
016000*                                 DATE KVPB-PLAN VALID UNTIL              
016100        05 CLAG-DASEASON     PIC 9(8)                                     
016200                             VALUE ZEROS.                                 
016300*                                 DATUM RESEASON-LEDTID GILTIG TO         
016400*                                 M                                       
016500*                                 DATE RESEASON-LEDTID VALID UNTI         
016600*                                 L                                       
016700        05 CLAG-DAXPOINT     PIC 9(8)                                     
016800                             VALUE ZEROS.                                 
016900*                                 POÄNGÄNDRINGSDATUM (ÅÅÅÅMMDD)           
017000*                                 POINT CHANGE DATE (YYYYMMDD)            
017100        05 CLAG-FLAVRART     PIC X                                        
017200                             VALUE SPACE.                                 
017300*                                 AVROPSARTIKEL                           
017400        05 CLAG-FLCDART      PIC X                                        
017500                             VALUE SPACE.                                 
017600*                                 CROSS-DOCKING PART                      
017700*                                 CROSS-DOCKING PART                      
017800        05 CLAG-FLEJBUFF     PIC X                                        
017900                             VALUE SPACE.                                 
018000*                                 EJ BUFFERTSTYRNING                      
018100*                                 NO BUFFER STEERING                      
018200        05 CLAG-FLFSP        PIC X                                        
018300                             VALUE SPACE.                                 
018400*                                 FÖRDELNINGSSPÄRR                        
018500*                                 BLOCKED FOR SPLIT                       
018600        05 CLAG-FLGEMART     PIC X                                        
018700                             VALUE SPACE.                                 
018800*                                 FLAGGA GEMENSAM ARTIKEL                 
018900*                                 COMMON PART FLAG                        
019000        05 CLAG-FLJIT        PIC X                                        
019100                             VALUE SPACE.                                 
019200*                                 JUST-IN-TIME FLAGGA                     
019300*                                 JUST-IN-TIME FLAG                       
019400        05 CLAG-FLLARM-BUF   PIC X                                        
019500                             VALUE SPACE.                                 
019600*                                 BUFFERT-LARMRAPPORT UTFÄRDAD            
019700        05 CLAG-FLLSRDEL     PIC X                                        
019800                             VALUE SPACE.                                 
019900*                                 LEVERERAS SOM RESDEL                    
020000        05 CLAG-FLLTKSP      PIC X                                        
020100                             VALUE SPACE.                                 
020200*                                 SPÄRR UTLEVERANS C2-LAGER               
020300        05 CLAG-FLMANAT      PIC X                                        
020400                             VALUE SPACE.                                 
020500*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
020600        05 CLAG-FLMANBK      PIC X                                        
020700                             VALUE SPACE.                                 
020800*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
020900        05 CLAG-FLMANGK      PIC X                                        
021000                             VALUE SPACE.                                 
021100*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
021200        05 CLAG-FLMANKP      PIC X                                        
021300                             VALUE SPACE.                                 
021400*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
021500        05 CLAG-FLMANLT      PIC X                                        
021600                             VALUE SPACE.                                 
021700*                                 MANUELLT SATT LEDTID ?                  
021800        05 CLAG-FLMANOPP     PIC X                                        
021900                             VALUE SPACE.                                 
022000*                                 MANUELLT SATT GODK. AV OP-PLAN?         
022100*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
022200        05 CLAG-FLMANOSK     PIC X                                        
022300                             VALUE SPACE.                                 
022400*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
022500*                                 ACCEPTANCE OF OP-PLAN CHANGED?          
022600        05 CLAG-FLMANPB      PIC X                                        
022700                             VALUE SPACE.                                 
022800*                                 MANUELLT REGISTRERAT PB-TPO             
022900*                                 MANUALLY REGISTRATED PB-TPO             
023000        05 CLAG-FLMANQ       PIC X                                        
023100                             VALUE SPACE.                                 
023200*                                 MANUELL HEMTAGNINGSKVANTITET            
023300        05 CLAG-FLMARKSP     PIC X                                        
023400                             VALUE SPACE.                                 
023500*                                 MARKNADSSPÄRR                           
023600*                                 MARKET BLOCKING CODE                    
023700        05 CLAG-FLMPB        PIC X                                        
023800                             VALUE SPACE.                                 
023900*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
024000        05 CLAG-FLNYBER      PIC X                                        
024100                             VALUE SPACE.                                 
024200*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
024300        05 CLAG-FLOREGPB     PIC X                                        
024400                             VALUE SPACE.                                 
024500*                                 OREGELBUNDEN PROGNOS (PB) ?             
024600        05 CLAG-FLRADREF     PIC X                                        
024700                             VALUE SPACE.                                 
024800*                                 KOMPLETTERANDE INFO. KRÄVS              
024900*                                 ADDITIONAL INFORMATION REQUIRED         
025000        05 CLAG-FLREFILL     PIC X                                        
025100                             VALUE SPACE.                                 
025200*                                 REFILLARTIKEL                           
025300*                                 REFILLPART                              
025400        05 CLAG-FLRELSP      PIC X                                        
025500                             VALUE SPACE.                                 
025600*                                 RELEASEBLOCKAD ARTIKEL .                
025700*                                 BLOCKED PART                            
025800        05 CLAG-FLSKROT-AUTO PIC X                                        
025900                             VALUE SPACE.                                 
026000*                                 SKROTNING AUTOMATISKT BEORDRAD          
026100*                                 SCRAPPING AUTOMATICALLY ORDERED         
026200        05 CLAG-FLSKROT-BEORD                                             
026300                             PIC X                                        
026400                             VALUE SPACE.                                 
026500*                                 SKROTNING BEORDRAD AV ANSK              
026600*                                 SCRAPPING ORDERED BY PROCURER           
026700        05 CLAG-FLSKROT-BEV  PIC X                                        
026800                             VALUE SPACE.                                 
026900*                                 BEVAKNING 2 ÅR INFÖR SKROTNING          
027000*                                 2 YEARS BEFORE SCRAPPING CHECK          
027100        05 CLAG-FLSKROT-SL   PIC X                                        
027200                             VALUE SPACE.                                 
027300*                                 SÄKERHETSLAGER NOLL INFÖR SKROT         
027400*                                 STOCK TO ZERO BEFORE SCRAPPING          
027500        05 CLAG-FLSKROT-WLC  PIC X                                        
027600                             VALUE SPACE.                                 
027700*                                 SISTA AVROP FÖRE SKROT                  
027800*                                 LAST CALL BEFORE SCRAPPING              
027900        05 CLAG-FLSPKOST     PIC X                                        
028000                             VALUE SPACE.                                 
028100*                                 SPECIELLA KOSTNADER FINS                
028200*                                 SPECIAL COSTS EXIST                     
028300        05 CLAG-FLTOPP       PIC X                                        
028400                             VALUE SPACE.                                 
028500*                                 TOPP-200-ARTIKEL                        
028600*                                 TOP 200 PART                            
028700        05 CLAG-FLTPO1       PIC X                                        
028800                             VALUE SPACE.                                 
028900*                                 ARTIKELN GODKÄND FÖR TPO1               
029000*                                 TPO1 ALLOWED FOR ARTICLE                
029100        05 CLAG-IDANSK       PIC Z(2)9                                    
029200                             VALUE ZEROS.                                 
029300*                                 ANSKAFFARNUMMER                         
029400*                                 PROCURER NO.                            
029500        05 CLAG-IDARTNR-EMBQ0                                             
029600                             PIC Z(7)9                                    
029700                             VALUE ZEROS.                                 
029800*                                 EMBALLAGEARTIKELNR FÖR Q0               
029900        05 CLAG-IDARTNR-EMBQ1                                             
030000                             PIC Z(7)9                                    
030100                             VALUE ZEROS.                                 
030200*                                 EMBALLAGEARTIKELNR FÖR Q1               
030300        05 CLAG-IDARTNR-EMBQ2                                             
030400                             PIC Z(7)9                                    
030500                             VALUE ZEROS.                                 
030600*                                 EMBALLAGEARTIKELNR FÖR Q2               
030700        05 CLAG-IDARTNR-EMBQ3                                             
030800                             PIC Z(7)9                                    
030900                             VALUE ZEROS.                                 
031000*                                 EMBALLAGEARTIKELNR FÖR Q3               
031100        05 CLAG-IDARTNR-EMBQ4                                             
031200                             PIC Z(7)9                                    
031300                             VALUE ZEROS.                                 
031400*                                 EMBALLAGEARTIKELNR FÖR Q4               
031500        05 CLAG-IDBERED      PIC Z9                                       
031600                             VALUE ZEROS.                                 
031700*                                 BEREDARENUMMER                          
031800        05 CLAG-IDDC-REF     PIC X(2)                                     
031900                             VALUE SPACES.                                
032000*                                 SÄNDANDE LAGER FÖR REFILL               
032100*                                 SENDING WAREHOUSE FOR REFILL            
032200        05 CLAG-IDFS-SEN     PIC X(8)                                     
032300                             VALUE SPACES.                                
032400*                                 FÖLJESEDELSNUMMER SENASTE INLEV         
032500*                                 ADVICE NOTE NUMBER ODETTE               
032600        05 CLAG-IDINK        PIC X(4)                                     
032700                             VALUE SPACES.                                
032800*                                 INKÖPARNUMMER                           
032900*                                 PURCHASE IDENTIFICATION NUMBER          
033000        05 CLAG-IDKAT        OCCURS 3 TIMES                               
033100                             PIC X(5)                                     
033200                             VALUE SPACES.                                
033300*                                 KATALOGBETECKNING                       
033400        05 CLAG-IDLEVNR-SEN  PIC X(5)                                     
033500                             VALUE SPACES.                                
033600*                                 SENASTE LEVERANTÖR                      
033700        05 CLAG-IDLEVNR-SHIP PIC X(5)                                     
033800                             VALUE SPACES.                                
033900*                                 SKEPPANDE LEVERANTÖR                    
034000*                                 SHIPPING SUPPLIER                       
034100        05 CLAG-IDLKTO       PIC 9(7)                                     
034200                             VALUE ZEROS.                                 
034300*                                 LAGERKONTO (FFHHHUU)                    
034400*                                 STOCK ACCOUNT (CCMMMSS)                 
034500        05 CLAG-IDPLANGR-AG  PIC 9                                        
034600                             VALUE ZERO.                                  
034700*                                 PLANERINGSGRUPP ANSKAFFARE              
034800        05 CLAG-IDPLANGR-LEV PIC 9                                        
034900                             VALUE ZERO.                                  
035000*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
035100        05 CLAG-IDPROENH     OCCURS 3 TIMES                               
035200                             PIC X(8)                                     
035300                             VALUE SPACES.                                
035400*                                 PRODUKTIONSENHET                        
035500*                                 PRODUCTION UNIT                         
035600        05 CLAG-IDPROJ       PIC X(4)                                     
035700                             VALUE SPACES.                                
035800*                                 PARTS PROJEKTIDENTITET                  
035900*                                 PARTS PROJECT IDENTITY                  
036000        05 CLAG-IDPROJUP     PIC X(8)                                     
036100                             VALUE SPACES.                                
036200*                                 PROJEKTUPPDRAG                          
036300*                                 PROJECT ASSIGNMENT                      
036400        05 CLAG-IDPSN        PIC 9(3)                                     
036500                             VALUE ZEROS.                                 
036600*                                 PROPER SHIPPING NAME                    
036700*                                 PROPER SHIPPING NAME                    
036800        05 CLAG-IDRITN       PIC X(10)                                    
036900                             VALUE SPACES.                                
037000*                                 RITNINGSNUMMER                          
037100*                                 DRAWING NUMBER                          
037200        05 CLAG-IDSTATNR     OCCURS 6 TIMES                               
037300                             PIC Z(8)9                                    
037400                             VALUE ZEROS.                                 
037500*                                 STATISTISKT NUMMER                      
037600*                                 1 = NORSKT                              
037700*                                 2 = ENGELSKT                            
037800*                                 3 = BELGISKT                            
037900*                                 4 = PERUANSKT                           
038000*                                 5 = SVENSKT                             
038100*                                 6 =                                     
038200*                                 STATISTICAL NO.                         
038300        05 CLAG-IDUSER-EMB   PIC X(8)                                     
038400                             VALUE SPACES.                                
038500*                                 ANVÄNDARENS SÄKERHETS ID                
038600*                                 USER SECURITY-IDENTITY                  
038700        05 CLAG-IDUSER-SPKVAL                                             
038800                             PIC X(8)                                     
038900                             VALUE SPACES.                                
039000*                                 ANVÄNDAR-ID KVALITETSPÄRR               
039100*                                 USER ID QUALITY ERROR                   
039200        05 CLAG-KDAGE        PIC X                                        
039300                             VALUE SPACE.                                 
039400*                                 AGE-CODE                                
039500*                                 AGE-CODE                                
039600        05 CLAG-KDARTHNT     PIC Z(5)9                                    
039700                             VALUE ZEROS.                                 
039800*                                 HANTERINGSKOD                           
039900*                                 HANDLING CODE                           
040000        05 CLAG-KDARTURS     PIC X(2)                                     
040100                             VALUE SPACES.                                
040200*                                 ARTIKELURSPRUNGSKOD                     
040300*                                 COUNTRY OF ORIGIN                       
040400        05 CLAG-KDAVT        PIC 9                                        
040500                             VALUE ZERO.                                  
040600*                                 AVTALSMÄRKNING                          
040700*                                 AGREEMENT CODE                          
040800        05 CLAG-KDBPSR       PIC 9                                        
040900                             VALUE ZERO.                                  
041000*                                 BASLAGERFÖRSLAGSNIVÅ                    
041100*                                 BASIC PART STOCK RECOMMENDATION         
041200        05 CLAG-KDEFFMAN     PIC X                                        
041300                             VALUE SPACE.                                 
041400*                                 EMIL-KOD                                
041500*                                 EMIL-CODE                               
041600        05 CLAG-KDEMBKOD-0   PIC Z(2)9                                    
041700                             VALUE ZEROS.                                 
041800*                                 EMBALLAGEKOD 0                          
041900        05 CLAG-KDEMBKOD-1   PIC Z(2)9                                    
042000                             VALUE ZEROS.                                 
042100*                                 EMBALLAGEKOD 1                          
042200        05 CLAG-KDEMBKOD-2   PIC Z(2)9                                    
042300                             VALUE ZEROS.                                 
042400*                                 EMBALLAGEKOD 2                          
042500        05 CLAG-KDEMBVOL     PIC X                                        
042600                             VALUE SPACE.                                 
042700*                                 VOLUME UPDATED CODE                     
042800*                                 VOLUME UPDATED CODE                     
042900        05 CLAG-KDERS        PIC Z9                                       
043000                             VALUE ZEROS.                                 
043100*                                 ERSÄTTNINGSKOD                          
043200*                                 SUPERSESSION CODE                       
043300        05 CLAG-KDEXCHA      PIC 9(3)                                     
043400                             VALUE ZEROS.                                 
043500*                                 EXCHANGE ACCOUNT CODE                   
043600        05 CLAG-KDFARLIG     PIC 9                                        
043700                             VALUE ZERO.                                  
043800*                                 KOD FÖR FARLIGT GODS                    
043900*                                 DANGEROUS GOODS CODE                    
044000        05 CLAG-KDFORP.                                                   
044100*                                 FÖRPACKNINGSKOD                         
044200*                                 PACKAGING CODE                          
044300           07 CLAG-KDFORPPL  PIC 9                                        
044400                             VALUE ZERO.                                  
044500*                                 FÖRPACKNINGSPLATS                       
044600*                                 PREPACKING PLACE                        
044700           07 CLAG-KDFORPGP  PIC 9(2)                                     
044800                             VALUE ZEROS.                                 
044900*                                 FÖRPACKNINGSGRUPP                       
045000*                                 PREPACKING GROUP                        
045100           07 CLAG-KDFORPUF  PIC 9                                        
045200                             VALUE ZERO.                                  
045300*                                 UPPRÄKNINGSFAKTOR                       
045400*                                 ENUMERATION                             
045500        05 CLAG-KDFREKKL     PIC X                                        
045600                             VALUE SPACE.                                 
045700*                                 FREKVENSKLASS                           
045800*                                 FREQ. CLASS                             
045900        05 CLAG-KDGK         PIC 9                                        
046000                             VALUE ZERO.                                  
046100*                                 GODSMOTTAGAREKOD                        
046200*                                 GOODS RECEIVING WAREHOUSE CODE          
046300        05 CLAG-KDHF         PIC 9                                        
046400                             VALUE ZERO.                                  
046500*                                 HUVUDFÖRRÅDSMÄRKNING                    
046600*                                 CODE MAIN STORAGE                       
046700        05 CLAG-KDKG         PIC 9                                        
046800                             VALUE ZERO.                                  
046900*                                 KURANSGRUPP                             
047000*                                 TURNOVER CODE                           
047100        05 CLAG-KDKSP        PIC 9                                        
047200                             VALUE ZERO.                                  
047300*                                 KÖPSPÄRR                                
047400*                                 PURCHASE BLOCKING CODE                  
047500        05 CLAG-KDLEVPLF     PIC X                                        
047600                             VALUE SPACE.                                 
047700*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
047800*                                 CODE FOR APPROVAL OF SCHEDULE P         
047900*                                 ROPOSAL                                 
048000        05 CLAG-KDLEVSP      PIC Z9                                       
048100                             VALUE ZEROS.                                 
048200*                                 SPÄRRKOD LEVERANS                       
048300*                                 DELIVERY BLOCKING CODE                  
048400        05 CLAG-KDLPSP       PIC 9                                        
048500                             VALUE ZERO.                                  
048600*                                 LEVERANSPLANESPÄRR                      
048700        05 CLAG-KDLTK        PIC 9                                        
048800                             VALUE ZERO.                                  
048900*                                 LAGERTILLHÖRIGHETSKOD                   
049000*                                 STOCK BELONGING CODE                    
049100        05 CLAG-KDOPPLAN     PIC X                                        
049200                             VALUE SPACE.                                 
049300*                                 OPTIMAL PLAN INOM FRYSTID               
049400*                                 OPTIMAL PLAN WITHIN FREEZTIME           
049500        05 CLAG-KDPCOO       PIC X                                        
049600                             VALUE SPACE.                                 
049700*                                 FÖRMÅNS AVTAL URSPRUNGSLAND             
049800*                                 PREF.AGREEEMENT COUNTRY ORIGIN          
049900        05 CLAG-KDPRISKL     PIC X                                        
050000                             VALUE SPACE.                                 
050100*                                 PRISKLASS                               
050200*                                 PRICE CLASS                             
050300        05 CLAG-KDPSLLOC     PIC 9(2)                                     
050400                             VALUE ZEROS.                                 
050500*                                 PRODUKTSLAG LOKALT                      
050600*                                 PRODUCT GROUP LOCAL                     
050700        05 CLAG-KDSPEEMB     PIC 9                                        
050800                             VALUE ZEROS.                                 
050900*                                 SPECIALEMBALLAGEKOD                     
051000*                                 SPECIAL PACKING CODE                    
051100        05 CLAG-KDSRA        PIC Z9                                       
051200                             VALUE ZEROS.                                 
051300*                                 SRA-KOD                                 
051400*                                 SRA CODE                                
051500        05 CLAG-KDTIPPR      PIC 9                                        
051600                             VALUE ZERO.                                  
051700*                                 TIPPAT PRIS KOD                         
051800*                                 ESTIMATED PRICE CODE                    
051900        05 CLAG-KDTULLRE     PIC 9                                        
052000                             VALUE ZERO.                                  
052100*                                 TULLRESTITUTION MÄRKNING                
052200*                                 CUSTOMS RESTITUAT.                      
052300        05 CLAG-KDUART       PIC X                                        
052400                             VALUE SPACE.                                 
052500*                                 UNDANTAGSARTIKEL                        
052600*                                 EXECPTION PARTS                         
052700        05 CLAG-KDVSOP       PIC Z(2)9                                    
052800                             VALUE ZEROS.                                 
052900*                                 VSOP-KOD                                
053000*                                 VSOP-CODE                               
053100        05 CLAG-KDVTH        PIC 9                                        
053200                             VALUE ZERO.                                  
053300*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
053400*                                 CODE FOR COST RESPONSIBILITY            
053500        05 CLAG-KDVVKL       PIC 9                                        
053600                             VALUE ZERO.                                  
053700*                                 VOLYMVÄRDESKLASS                        
053800*                                 VOLUME VALUE CLASS                      
053900        05 CLAG-KDYTBEH      PIC Z9                                       
054000                             VALUE ZEROS.                                 
054100*                                 YTBEHANDLINGSKOD                        
054200*                                                                         
054300        05 CLAG-KVAKS-CDC    PIC -(7)9                                    
054400                             VALUE ZEROS.                                 
054500*                                 DEL AV AK SOM LIGGER I CDC              
054600*                                 PART OF AK IN THE CDC                   
054700        05 CLAG-KVAKS-PAV    PIC -(7)9                                    
054800                             VALUE ZEROS.                                 
054900*                                 DEL AV AK PÅ VÄG                        
055000*                                 PART OF AK ON ITS WAY                   
055100        05 CLAG-KVAKS-T      PIC -(7)9                                    
055200                             VALUE ZEROS.                                 
055300*                                 DEL AV AK I EN TERMINAL                 
055400*                                 PART OF AK IN A TERMINAL                
055500        05 CLAG-KVAP         PIC Z(6)9                                    
055600                             VALUE ZEROS.                                 
055700*                                 ANNULLATIONSPUNKT                       
055800        05 CLAG-KVAVIS-SEN   PIC Z(5)9                                    
055900                             VALUE ZEROS.                                 
056000*                                 SENAST AVISERAT ANTAL                   
056100        05 CLAG-KVAVROP-TOT  PIC Z(6)9                                    
056200                             VALUE ZEROS.                                 
056300*                                 ALLA AVROP MED KOD = 2                  
056400        05 CLAG-KVBEART      PIC Z(5)9                                    
056500                             VALUE ZEROS.                                 
056600*                                 BESTÄLLT ANTAL STYCKEN                  
056700*                                 ORDERED QUANTITY                        
056800        05 CLAG-KVBK         PIC Z(6)9                                    
056900                             VALUE ZEROS.                                 
057000*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
057100        05 CLAG-KVDAGAR-FFH  PIC Z9                                       
057200                             VALUE ZEROS.                                 
057300*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
057400        05 CLAG-KVDAGAR-INLEV                                             
057500                             PIC Z9                                       
057600                             VALUE ZEROS.                                 
057700*                                 INLEVERANSTID     (ANTAL DAGAR)         
057800        05 CLAG-KVDAGAR-TT   PIC Z9                                       
057900                             VALUE ZEROS.                                 
058000*                                 DAGAR TULL- OCH TRANSPORT-TID           
058100        05 CLAG-KVEFRS       PIC -(7)9                                    
058200                             VALUE ZEROS.                                 
058300*                                 EJ FAKTURERAT ANTAL STYCK               
058400*                                 ORDERED NOT INVOICED QTY                
058500        05 CLAG-KVEOQ        PIC Z(6)9                                    
058600                             VALUE ZEROS.                                 
058700*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
058800*                                 ET                                      
058900        05 CLAG-KVFRYSTI     PIC Z9                                       
059000                             VALUE ZEROS.                                 
059100*                                 FRYSTID FÖR TPO-ORDER                   
059200*                                 FREEZTIME FOR TPO                       
059300        05 CLAG-KVINVS       PIC Z(6)9                                    
059400                             VALUE ZEROS.                                 
059500*                                 INVENTERINGSSALDO                       
059600*                                 STOCK-TAKING BALANCE                    
059700        05 CLAG-KVKP         PIC Z(6)9                                    
059800                             VALUE ZEROS.                                 
059900*                                 KÖPPUNKT                                
060000        05 CLAG-KVLAAN       PIC Z(6)9                                    
060100                             VALUE ZEROS.                                 
060200*                                 LÅNESALDO                               
060300        05 CLAG-KVLS         PIC -(7)9                                    
060400                             VALUE ZEROS.                                 
060500*                                 LAGERSALDO                              
060600*                                 STOCK BALANCE                           
060700        05 CLAG-KVLS-CD      OCCURS 4 TIMES                               
060800                             PIC -(7)9                                    
060900                             VALUE ZEROS.                                 
061000*                                 LAGERSALDO CD                           
061100*                                 STOCK BALANCE CD                        
061200        05 CLAG-KVLS-SVS     PIC -(7)9                                    
061300                             VALUE ZEROS.                                 
061400*                                 LAGERSALDO SVS                          
061500*                                 STOCK BALANCE SVS                       
061600        05 CLAG-KVMAD-SEP    PIC Z(5)9.9                                  
061700                             VALUE ZEROS.                                 
061800*                                 SEPARAT PROGNOSFEL                      
061900        05 CLAG-KVMAD-TOT    PIC Z(5)9.9                                  
062000                             VALUE ZEROS.                                 
062100*                                 TOTALT PROGNOSFEL                       
062200        05 CLAG-KVMAXPL      PIC Z(5)9                                    
062300                             VALUE ZEROS.                                 
062400*                                 MAX ANTAL (STYCK) PÅ PLOCKPLATS         
062500*                                 MAX ALLOWED QTY IN PICKING LOC          
062600        05 CLAG-KVMP         PIC Z(6)9                                    
062700                             VALUE ZEROS.                                 
062800*                                 MAXPUNKT                                
062900*                                 MAXIMUM POINT                           
063000        05 CLAG-KVOI-OVR     PIC Z(6)9                                    
063100                             VALUE ZEROS.                                 
063200*                                 ORDERINGÅNG LEV ÖVRIGT                  
063300*                                 ORDERED PCS PER TIME UNIT OTHER         
063400        05 CLAG-KVOI-PLOCK   PIC Z(6)9                                    
063500                             VALUE ZEROS.                                 
063600*                                 ORDERINGÅNG LEV FRÅN PLOCKPLATS         
063700*                                 ORDERED FROM PICKING AREA               
063800        05 CLAG-KVOVERF      PIC Z(6)9                                    
063900                             VALUE ZEROS.                                 
064000*                                 ÖVERFÖRINGSSALDO                        
064100        05 CLAG-KVPALL       PIC Z(6)9                                    
064200                             VALUE ZEROS.                                 
064300*                                 ANTAL I PALL                            
064400*                                 QUANTITY IN PALLET                      
064500        05 CLAG-KVPB-HIST    PIC Z(5)9.9                                  
064600                             VALUE ZEROS.                                 
064700*                                 PB (PROGNOS) HISTORISKT CDC             
064800*                                 HISTORIC REQUIREMENTS CDC               
064900        05 CLAG-KVPB-PLAN    PIC Z(5)9.9                                  
065000                             VALUE ZEROS.                                 
065100*                                 PLANERAT PERIODBEHOV                    
065200*                                 PLANNED PERIOD REQUIREMENTS             
065300        05 CLAG-KVPB-PLAN-JUST1                                           
065400                             PIC Z(5)9.9                                  
065500                             VALUE ZEROS.                                 
065600*                                 PLANERAT PERIODBEHOV JUST1              
065700*                                 PLANNED PERIOD REQUIREMENT ADJ1         
065800        05 CLAG-KVPB-PLAN-JUST2                                           
065900                             PIC Z(5)9.9                                  
066000                             VALUE ZEROS.                                 
066100*                                 PLANERAT PERIODBEHOV JUST2              
066200*                                 PLANNED PERIOD REQUIREMENT ADJ2         
066300        05 CLAG-KVPB-SATS    PIC Z(5)9.9                                  
066400                             VALUE ZEROS.                                 
066500*                                 SATS-PERIODBEHOV                        
066600*                                 KIT PERIOD REQUIREMENTS                 
066700        05 CLAG-KVPB-SEP     PIC Z(5)9.9                                  
066800                             VALUE ZEROS.                                 
066900*                                 SEPARAT PERIODBEHOV                     
067000*                                 SEPARATE PERIOD REQUIREMENTS            
067100        05 CLAG-KVPB-TPO     PIC Z(5)9.9                                  
067200                             VALUE ZEROS.                                 
067300*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
067400*                                 PERIODICAL DEMAND TPO1 AND TPO2         
067500*                                                                         
067600        05 CLAG-KVPB-TREND   PIC Z(5)9.9                                  
067700                             VALUE ZEROS.                                 
067800*                                 PERIODTRENDVÄRDE                        
067900        05 CLAG-KVPB-VESL    PIC Z(5)9.9                                  
068000                             VALUE ZEROS.                                 
068100*                                 GÄLLANDE PB VID VECKOSLUT               
068200        05 CLAG-KVPOINT      PIC Z(5)9                                    
068300                             VALUE ZEROS.                                 
068400*                                 POINT VALUE                             
068500        05 CLAG-KVQ          PIC Z(6)9                                    
068600                             VALUE ZEROS.                                 
068700*                                 EKONOMISK HEMTAGNINGSKVANTITET          
068800        05 CLAG-KVQ-JUST     PIC Z(6)9                                    
068900                             VALUE ZEROS.                                 
069000*                                 NY EKON HEMTAGNINGSKVANTITET            
069100        05 CLAG-KVQPACK-0    PIC -(5)9                                    
069200                             VALUE ZEROS.                                 
069300*                                 ANTAL I Q0 FÖRPACKNING                  
069400        05 CLAG-KVQPACK-1    PIC -(5)9                                    
069500                             VALUE ZEROS.                                 
069600*                                 ANTAL I Q1 FÖRPACKNING                  
069700*                                 QUANTITY IN BULK PACK Q1                
069800        05 CLAG-KVQPACK-2    PIC -(5)9                                    
069900                             VALUE ZEROS.                                 
070000*                                 ANTAL I Q2 FÖRPACKNING                  
070100*                                 QUANTITY IN BULK PACK Q2                
070200        05 CLAG-KVQPACK-3    PIC -(5)9                                    
070300                             VALUE ZEROS.                                 
070400*                                 ANTAL I Q3 FÖRPACKNING                  
070500*                                 QUANTITY IN BULK PACK Q3                
070600        05 CLAG-KVQPACK-4    PIC -(5)9                                    
070700                             VALUE ZEROS.                                 
070800*                                 ANTAL I Q4 FÖRPACKNING                  
070900*                                 QUANTITY IN BULK PACK Q4                
071000        05 CLAG-KVREFBER-PLOCK                                            
071100                             PIC Z(6)9                                    
071200                             VALUE ZEROS.                                 
071300*                                 BERÄKNAD REFILLINGKVANT-PLOCK           
071400*                                 CALCULATED REFILLING QTY-PICK           
071500        05 CLAG-KVREFOVL-TOT PIC Z(6)9                                    
071600                             VALUE ZEROS.                                 
071700*                                 BERÄKNAD ÖVERLAGER CHILD DC:N           
071800*                                 CALCULATED OVERSTOCK ALL XDC            
071900        05 CLAG-KVREFPKT-PLOCK                                            
072000                             PIC Z(6)9                                    
072100                             VALUE ZEROS.                                 
072200*                                 BERÄKNAD PÅFYLLNADSPUNKT-PLOCK          
072300*                                 CALCULATED REFILLING POINT-PICK         
072400        05 CLAG-KVRESS       PIC Z(6)9                                    
072500                             VALUE ZEROS.                                 
072600*                                 RESERVERAT ANTAL ARTIKLAR               
072700*                                 QUANTITY RESERVED ITEMS                 
072800        05 CLAG-KVRESS-CD    OCCURS 4 TIMES                               
072900                             PIC Z(6)9                                    
073000                             VALUE ZEROS.                                 
073100*                                 RESERVERAT ANTAL ARTIKLAR CROSS         
073200*                                  DOCKING LAGER                          
073300*                                 QUANTITY RESERVED ITEMS IN CROS         
073400*                                 S DOCKING WAREHOUSE                     
073500        05 CLAG-KVRETUR      PIC -(6)9                                    
073600                             VALUE ZEROS.                                 
073700*                                 ANTAL I RETUR                           
073800*                                 QUANTITY IN RETURN                      
073900        05 CLAG-KVRETUR-TOT  PIC -(6)9                                    
074000                             VALUE ZEROS.                                 
074100*                                 ANTAL RETURER TOTALT                    
074200*                                 QUANTITY TOTAL RETURNS                  
074300        05 CLAG-KVROS        PIC -(7)9                                    
074400                             VALUE ZEROS.                                 
074500*                                 RESTORDERSALDO                          
074600*                                 BACKORDER QTY                           
074700        05 CLAG-KVSLAGER     PIC Z(5)9                                    
074800                             VALUE ZEROS.                                 
074900*                                 SÄKERHETSLAGER                          
075000*                                 SAFETY STOCK                            
075100        05 CLAG-KVSLAGER-OPT PIC Z(5)9                                    
075200                             VALUE ZEROS.                                 
075300*                                 OPTIMALT SÄKERHETSLAGER                 
075400*                                 OPT SAFETY STOCK                        
075500        05 CLAG-KVSLUTKP     PIC Z(6)9                                    
075600                             VALUE ZEROS.                                 
075700*                                 SLUTKÖPSSALDO                           
075800        05 CLAG-KVSPANT      PIC -(6)9                                    
075900                             VALUE ZEROS.                                 
076000*                                 SPÄRRAT ANTAL                           
076100*                                 BLOCKED QTY                             
076200        05 CLAG-KVSPARR-KVAL PIC Z(6)9                                    
076300                             VALUE ZEROS.                                 
076400*                                 SPÄRRAT ANTAL KVALITETSFEL              
076500*                                 BLOCKED QUANTITY QUALITY ERROR          
076600        05 CLAG-KVTILLG-TOT  PIC Z(6)9                                    
076700                             VALUE ZEROS.                                 
076800*                                 LAGERTILLGÅNG CDC TOTALT                
076900        05 CLAG-KVULOAD      PIC Z(6)9                                    
077000                             VALUE ZEROS.                                 
077100*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
077200*                                 MIN LOAD FROM SUPPLIER                  
077300        05 CLAG-KVUTJFEL     PIC Z(5)9.9                                  
077400                             VALUE ZEROS.                                 
077500*                                 UTJÄMNAT FEL                            
077600        05 CLAG-KVUTRS       PIC -(7)9                                    
077700                             VALUE ZEROS.                                 
077800*                                 UTREDNINGSSALDO                         
077900*                                 INVESTIGATION BALANCE                   
078000        05 CLAG-KVVECKOR-AT  PIC Z9                                       
078100                             VALUE ZEROS.                                 
078200*                                 ANTAL VECKOR ANSKAFFNINGSTID            
078300        05 CLAG-KVVECKOR-BT  PIC Z9                                       
078400                             VALUE ZEROS.                                 
078500*                                 ANTAL VECKOR BESTÄLLNINGSTID            
078600        05 CLAG-KVVECKOR-FT  PIC Z9                                       
078700                             VALUE ZEROS.                                 
078800*                                 ANTAL VECKOR FRYSNINGSTID               
078900        05 CLAG-KVVECKOR-LT  PIC Z9                                       
079000                             VALUE ZEROS.                                 
079100*                                 ANTAL VECKOR LEDTID                     
079200        05 CLAG-KVVECKOR-LVAR                                             
079300                             PIC 9(2).9                                   
079400                             VALUE ZEROS.                                 
079500*                                 VARIANS I LEDTIDEN                      
079600*                                                                         
079700        05 CLAG-KVVECKOR-TREND                                            
079800                             PIC Z9                                       
079900                             VALUE ZEROS.                                 
080000*                                 ANTAL VECKOR TRENDVÄRDE                 
080100        05 CLAG-KVVORKO      PIC -(6)9                                    
080200                             VALUE ZEROS.                                 
080300*                                 VOR-KÖ KVANT                            
080400*                                 VOR-QUEUE QUANT                         
080500        05 CLAG-PRARTBTO-EXP PIC Z(6)9.9(2)                               
080600                             VALUE ZEROS.                                 
080700*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
080800*                                 GROSS-PRICE EXPORT                      
080900*                                  (FOB-GROSS)                            
081000        05 CLAG-PRARTSJK     PIC Z(6)9.9(2)                               
081100                             VALUE ZEROS.                                 
081200*                                 ARTIKELNS SJÄLVKOSTNAD                  
081300*                                 COST OF SALES                           
081400        05 CLAG-PRARTSTD     PIC Z(6)9.9(2)                               
081500                             VALUE ZEROS.                                 
081600*                                 ARTIKELSTANDARDPRIS                     
081700*                                 STANDARD PRICE                          
081800        05 CLAG-PRDIRLON     PIC Z(3)9.9(3)                               
081900                             VALUE ZEROS.                                 
082000*                                 DIREKT LÖN                              
082100*                                 SURCHARGE COSTS                         
082200        05 CLAG-PRDMTRL      PIC Z(5)9.9(3)                               
082300                             VALUE ZEROS.                                 
082400*                                 DIREKT MATERIAL                         
082500*                                 SURCHARGE PACKING MATERIAL              
082600        05 CLAG-PRHEMTAG     PIC Z(6)9.9(2)                               
082700                             VALUE ZEROS.                                 
082800*                                 HEMTAGNINGSKOSTNAD                      
082900*                                 TRANSPORT COST                          
083000        05 CLAG-PRINK        PIC Z(6)9.9(2)                               
083100                             VALUE ZEROS.                                 
083200*                                 INKÖPSPRIS                              
083300*                                 PURCHASE PRICE                          
083400        05 CLAG-PRLFKST      PIC Z(2)9.9(2)                               
083500                             VALUE ZEROS.                                 
083600*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
083700*                                 SUPPLIERS PACKING AND HANDLING          
083800        05 CLAG-PRORDSK      PIC Z(4)9.9(2)                               
083900                             VALUE ZEROS.                                 
084000*                                 ORDERSÄRKOSTNAD                         
084100*                                 REMAINING OVERHEAD SURCHARGE            
084200        05 CLAG-PROVRPAL     PIC Z(3)9.9(3)                               
084300                             VALUE ZEROS.                                 
084400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
084500*                                 REMAINING OVERHEAD SURCHARGE            
084600        05 CLAG-PRREF        PIC Z(6)9.9(2)                               
084700                             VALUE ZEROS.                                 
084800*                                 REFERENCE PRICE                         
084900        05 CLAG-REDIRLEV     PIC 9.9(2)                                   
085000                             VALUE ZEROS.                                 
085100*                                 DIREKTLEVERANSANDEL                     
085200        05 CLAG-RESEASON-PLAN                                             
085300                             OCCURS 12 TIMES                              
085400                             PIC 9.9(2)                                   
085500                             VALUE ZEROS.                                 
085600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
085700        05 CLAG-RESLJUST     PIC 9.9                                      
085800                             VALUE ZEROS.                                 
085900*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
086000*                                 ADJUSTMENT ALGORITM                     
086100        05 CLAG-RETULF       PIC Z(2)9.9(4)                               
086200                             VALUE ZEROS.                                 
086300*                                 TULLFAKTOR                              
086400*                                 CCY EXCH RATE INCL FREIGHT/DUTY         
086500        05 CLAG-RVPROFEL     PIC Z9                                       
086600                             VALUE ZEROS.                                 
086700*                                 ANTAL STORA PROGNOSFEL                  
086800        05 CLAG-RVPROURS     PIC Z9                                       
086900                             VALUE ZEROS.                                 
087000*                                 ANTAL PROGNOSFEL I FÖLJD                
087100        05 CLAG-TIAVIDAT-SEN PIC 9(6)                                     
087200                             VALUE ZEROS.                                 
087300*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
087400        05 CLAG-TIBESRPT     PIC 9(4)                                     
087500                             VALUE ZEROS.                                 
087600*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
087700*                                 (ÅÅVV)                                  
087800        05 CLAG-TIBESRPT-PAAM                                             
087900                             PIC 9(4)                                     
088000                             VALUE ZEROS.                                 
088100*                                 PÅMINNELSEDATUM FÖR                     
088200*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
088300        05 CLAG-TIDATUM-TREND                                             
088400                             PIC 9(6)                                     
088500                             VALUE ZEROS.                                 
088600*                                 JUSTERAD TREND AAMMDD                   
088700*                                 LAST TREND CHANGE  YYMMDD               
088800        05 CLAG-TIDISPIN     PIC 9(6)                                     
088900                             VALUE ZEROS.                                 
089000*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
089100*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
089200        05 CLAG-TIGILTIG-PCOO                                             
089300                             PIC 9(6)                                     
089400                             VALUE ZEROS.                                 
089500*                                 GILTIGHETSDATUM PCOO                    
089600*                                                                         
089700*                                 DATE OF VALIDITY FOR PCOO               
089800        05 CLAG-TIINVDAT     PIC 9(5)                                     
089900                             VALUE ZEROS.                                 
090000*                                 INVENTERINGSDATUM                       
090100*                                 STOCKTAKING DATE                        
090200        05 CLAG-TILEVDAG     OCCURS 5 TIMES                               
090300                             PIC 9                                        
090400                             VALUE ZERO.                                  
090500*                                 AVSÄNDNINGSDAG INOM VECKA               
090600*                                 DELIVERY WEEK DAY                       
090700        05 CLAG-TILPSP       PIC 9(4)                                     
090800                             VALUE ZEROS.                                 
090900*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
091000        05 CLAG-TILTK        PIC 9(4)                                     
091100                             VALUE ZEROS.                                 
091200*                                 LTK-ÄNDRINGSDATUM                       
091300        05 CLAG-TIMAIL-KVAL  PIC 9(6)                                     
091400                             VALUE ZEROS.                                 
091500*                                 MAIL DATUM                              
091600*                                 MAIL DATE                               
091700        05 CLAG-TIOMSPEC     PIC 9(4)                                     
091800                             VALUE ZEROS.                                 
091900*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
092000        05 CLAG-TIPBDAT      PIC 9(5)                                     
092100                             VALUE ZEROS.                                 
092200*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
092300        05 CLAG-TIPBLOCK     PIC 9(6)                                     
092400                             VALUE ZEROS.                                 
092500*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
092600*                                 ÅMMDD                                   
092700*                                 FUTURE DATE FOR FORECAST TO BE          
092800*                                 BLOCKED                                 
092900*                                 INCREASED FORECAST IS ALLOWED           
093000        05 CLAG-TIPBPLAN-JUST1-FOM                                        
093100                             PIC 9(6)                                     
093200                             VALUE ZEROS.                                 
093300*                                 FOM PB-PLAN DATUM - JUST1               
093400*                                 FROM PB-PLAN DATE - ADJ1                
093500        05 CLAG-TIPBPLAN-JUST1-TOM                                        
093600                             PIC 9(6)                                     
093700                             VALUE ZEROS.                                 
093800*                                 TOM PB-PLAN DATUM - JUST1               
093900*                                 UNTIL PB-PLAN DATE - ADJ1               
094000        05 CLAG-TIPBPLAN-JUST2-FOM                                        
094100                             PIC 9(6)                                     
094200                             VALUE ZEROS.                                 
094300*                                 FOM PB-PLAN DATUM - JUST2               
094400*                                 FROM PB-PLAN DATE - ADJ2                
094500        05 CLAG-TIPBPLAN-JUST2-TOM                                        
094600                             PIC 9(6)                                     
094700                             VALUE ZEROS.                                 
094800*                                 TOM PB-PLAN DATUM - JUST2               
094900*                                 UNTIL PB-PLAN DATE - ADJ2               
095000        05 CLAG-TIQJUST      PIC 9(4)                                     
095100                             VALUE ZEROS.                                 
095200*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
095300        05 CLAG-TIREFSTO     PIC 9(6)                                     
095400                             VALUE ZEROS.                                 
095500*                                 BEORDRINGSSTOPPAD T.OM.                 
095600*                                 STOPPED FOR ORDERING UNTIL              
095700        05 CLAG-TIRODAT      PIC 9(6)                                     
095800                             VALUE ZEROS.                                 
095900*                                 RESTORDERDATUM         (ÅÅMMDD)         
096000*                                 BACK ORDER DATE        (YYMMDD)         
096100        05 CLAG-TISKPREL     PIC 9(4)                                     
096200                             VALUE ZEROS.                                 
096300*                                 PREL. SKROTNINGSDATUM (AAVV)            
096400*                                 PREL DATE OF SCRAPPING (YYWW)           
096500        05 CLAG-TISKROT      PIC 9(6)                                     
096600                             VALUE ZEROS.                                 
096700*                                 SKROTNINGSDATUM                         
096800*                                 DATE OF SCRAPPING                       
096900        05 CLAG-TISKROT-AUTO PIC 9(6)                                     
097000                             VALUE ZEROS.                                 
097100*                                 STOPDATE AUTO-SKROTNING                 
097200*                                 STOP DATE AUTOSCRAPPING                 
097300        05 CLAG-TISLJUST     PIC 9(4)                                     
097400                             VALUE ZEROS.                                 
097500*                                 VECKA DÅ JUSTERING AV SÄKER-            
097600*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
097700        05 CLAG-TISLUTKP     PIC Z(6)                                     
097800                             VALUE ZEROS.                                 
097900*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
098000*                                 CALC OF ATR-BAL IS TO COMMENCE          
098100        05 CLAG-TISPARR-KVAL PIC 9(6)                                     
098200                             VALUE ZEROS.                                 
098300*                                 SPÄRRAD DATUM KVALITETSFEL              
098400*                                 BLOCKED DATE QUALITY ERROR              
098500        05 CLAG-TISTODAT-LARM                                             
098600                             PIC 9(6)                                     
098700                             VALUE ZEROS.                                 
098800*                                 STOPPDATUM FÖR LARM-223                 
098900*                                 STOP DATE FOR ALARM-223                 
099000        05 CLAG-TISTOREF     PIC 9(5)                                     
099100                             VALUE ZEROS.                                 
099200*                                 STOPPTIDPUNKT FÖR REFILLORDRAR          
099300*                                 STOP TIME FOR REFILL ORDERS             
099400        05 CLAG-TIUPPDAT-EMB PIC 9(6)                                     
099500                             VALUE ZEROS.                                 
099600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
099700*                                 UPDATING DATE     (YYMMDD)              
099800        05 CLAG-VKART        PIC Z(6)9                                    
099900                             VALUE ZEROS.                                 
100000*                                 ARTIKELVIKT (G)                         
100100*                                 PART WEIGHT (G)                         
100200        05 CLAG-VLARTNTO     PIC Z(7)9.9                                  
100300                             VALUE ZEROS.                                 
100400*                                 ARTIKELVOLYM (CM3)                      
100500*                                 PART VOLUME    (CM3)                    
100600        05 CLAG-KDUVKNTO     PIC X                                        
100700                             VALUE SPACE.                                 
100800*                                 KOD HUR NETTOVIKT UPPDATERAD            
100900*                                 CODE FOR HOW NET WEIGHT UPDATED         
101000        05 CLAG-IDUSER-VUPD  PIC X(8)                                     
101100                             VALUE SPACES.                                
101200*                                 USER MANUELL UPPD VIKT/VOL/VSOP         
101300*                                 USER MAN UPD OF VOL/WEIGHT/VSOP         
101400        05 CLAG-VKART-NTO    PIC 9(8)                                     
101500                             VALUE ZEROS.                                 
101600*                                 ARTIKELNS NETTOVIKT                     
101700*                                 PART NET WEIGHT                         
101800        05 CLAG-TIUPPDAT-VUPD                                             
101900                             PIC 9(6)                                     
102000                             VALUE ZEROS.                                 
102100*                                 UPPDATERING ARTIKELNS VIKT              
102200*                                 UPDATING DATE OF PART WEIGHT            
102300        05 CLAG-KDOTFREK     PIC X                                        
102400                             VALUE SPACE.                                 
102500*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
102600*                                 ORDER HIT FREQUENCY FOR PART            
102700        05 CLAG-FLAUTREL     PIC X                                        
102800                             VALUE SPACE.                                 
102900*                                 AUT. SPÄRR FÖR VOR RELEASE              
103000*                                 AUTOMATIC BLOCK FOR AUT RELEASE         
103100*** END OF VILMAII-COPY LENGTH= 1365 BYTES                                
