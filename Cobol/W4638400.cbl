001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W4638400.                                                
001200 AUTHOR.         BO HAMMARIN, GDC GROUP.                                  
001300 DATE-WRITTEN.   APRIL-99.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER INFILEN MED EDI-ORDER-RESPONSE POSTER,                     
001800*        OMFORMAR DEN TILL ETT ENHETLIGT POSTUTSEENDE OCH                 
001900*        SKRIVER UT ORDERBEKRÄFTELSER FÖR VIDARE BEARBETNING.             
001910*        SKRIVER ÄVEN UT EDI-LOGG FÖR VIDARE BEARBETNING.                 
001911*                                                                         
001912*        PGM HANTERAR FÖLJANDE HÄNDELSER;                                 
001920*        - ARTIKEL OKÄND HOS LEVERANTÖREN (KOD 22+83/EDI-KOD 7)           
001930*        - LEVERANS AV ARTIKEL FÖRSENAD   (KOD 96   /EDI-KOD 3)           
001940*        - ANNULLATION EJ MÖJLIG          (KOD 20   /EDI-KOD 4)           
001950*        - ANNULLATION UTFÖRD             (KOD 83   /EDI-KOD 2)           
002000*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401                                                                          
003402*          --- INFIL MED EDI-ORDER-RESPONSE                               
003403     SELECT W46383                     ASSIGN TO W46384D1.                
003404                                                                          
003405*          --- UTFIL TILL DISPATCHERHANTERING                             
003406     SELECT W46384                     ASSIGN TO W46384D2.                
003600     EJECT                                                                
003610                                                                          
003630*          --- UTFIL TILL LOGG AV TYP ORDER-RESPONSE                      
003640     SELECT W46386                     ASSIGN TO W46384D3.                
003650     EJECT                                                                
003660                                                                          
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004001                                                                          
004009 FD  W46383                                                               
004010     RECORDING       V                                                    
004011     BLOCK CONTAINS  0.                                                   
004012 01  EDI-POST                    PIC X(1005).                             
004100     EJECT                                                                
004110                                                                          
004120 FD  W46384                                                               
004130     RECORDING       F                                                    
004140     BLOCK CONTAINS  0.                                                   
004151*01  POST -COPY W46384     -PRE DISP-   -L.                               
004160     EJECT                                                                
004170                                                                          
004190 FD  W46386                                                               
004191     RECORDING       F                                                    
004192     BLOCK CONTAINS  0.                                                   
004193 01  UT-LOGG.                                                             
004194*    03 -COPY W46341  -L.                                                 
004195     EJECT                                                                
004196                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4638400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004839 77  WS-RECTYPE                  PIC X(3).                                
004840 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004850     88  FIRST-TIME                          VALUE 'J'.                   
004860 77  W46383-EOF-SW               PIC X       VALUE 'N'.                   
004870     88  END-OF-W46383                       VALUE 'J'.                   
004900     EJECT                                                                
004910                                                                          
004930 01  WS-NUM-FAELT.                                                        
004931     03  WS-NUM-1                PIC 9(1).                                
004932     03  WS-NUM-2                PIC 9(2).                                
004933     03  WS-NUM-3                PIC 9(3).                                
004934     03  WS-NUM-4                PIC 9(4).                                
004935     03  WS-NUM-5                PIC 9(5).                                
004936     03  WS-NUM-6                PIC 9(6).                                
004937     03  WS-NUM-7                PIC 9(7).                                
004938     03  WS-NUM-8                PIC 9(8).                                
004939     03  WS-NUM-9                PIC 9(9).                                
004940                                                                          
004950 01  WS-TEST-DATUM               PIC X(8).                                
005420 01  WS-DAGENS-DATUM-8           PIC 9(8).                                
005421 01  WS-DAGENS-DATUM-6           PIC 9(6).                                
005425 01  WS-DAGENS-KLOCKA.                                                    
005426     03  WS-DAGENS-KLOCKA-1-4    PIC 9(4).                                
005427     03  WS-DAGENS-KLOCKA-5-9    PIC 9(5).                                
005428 01  WS-DAGENS-KLOCKA-2.                                                  
005429     03  WS-DAGENS-KLOCKA-1-6    PIC 9(6).                                
005430     03  WS-DAGENS-KLOCKA-7-9    PIC 9(3).                                
005431                                                                          
005440 01  WS-DAGENS-DATUM-KLOCKA.                                              
005450     03  WS-DAGENS-DATUM-ALFA    PIC X(8).                                
005460     03  WS-DAGENS-KLOCKA-ALFA   PIC X(4).                                
005617                                                                          
005618 01  WS-IDDISTR-IDKUNDNR.                                                 
005619     03  WS-IDDISTR-ALFA         PIC X(5).                                
005620     03  WS-IDKUNDNR-ALFA        PIC X(7).                                
005622     EJECT                                                                
005623                                                                          
005630 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
006130     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400     EJECT                                                                
006410                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800                                                                          
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202                                                                          
007203*    --- PARAMETRAR TILL POSTSUM                                          
007204*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007402     EJECT                                                                
007403                                                                          
007404*    --- PARAMETRAR TILL WDATKONV                                         
007405*                                                                         
007406*01  -COPY WDATAREA                                                       
007407     EJECT                                                                
007408                                                                          
007409*    --- PARAMETRAR TILL W009CIA                                          
007410*01  -COPY W009CIA                                                        
007411     EJECT                                                                
007412                                                                          
007413 01  UT-AREA-LOGG-START          PIC X(24)   VALUE                        
007414                                 'UT-AREA-LOGG-START  '.                  
007415*01  AREA -COPY W46341     -PRE UTL-                                      
007416     EJECT                                                                
007417                                                                          
007418 01  UT-AREA-START               PIC X(24)   VALUE                        
007419                                 'UT-AREA-START  '.                       
007420*01  AREA -COPY W46384     -PRE UT-                                       
007421     EJECT                                                                
007422                                                                          
007423 01  IN-AREOR-START              PIC X(24)   VALUE                        
007424                                'IN-AREOR-START  '.                       
007425 01  IN-AREA.                                                             
007426     03 FILLER                   PIC X(231).                              
007427 01  IN-WEDIORDR REDEFINES IN-AREA.                                       
007428*    03  -COPY WEDIORDR                                                   
007429     EJECT                                                                
007430 01  IN-WEDIUNH  REDEFINES IN-AREA.                                       
007431*    03  -COPY WEDIUNH0                                                   
007432     EJECT                                                                
007433 01  IN-WEDIBGM  REDEFINES IN-AREA.                                       
007434*    03  -COPY WEDIBGM0                                                   
007435     EJECT                                                                
007436 01  IN-WEDIDTM  REDEFINES IN-AREA.                                       
007437*    03  -COPY WEDIDTM1                                                   
007438     EJECT                                                                
007439 01  IN-WEDIRFF  REDEFINES IN-AREA.                                       
007440*    03  -COPY WEDIRFF1                                                   
007441     EJECT                                                                
007442 01  IN-WEDINAD  REDEFINES IN-AREA.                                       
007443*    03  -COPY WEDINAD2                                                   
007444     EJECT                                                                
007445 01  IN-WEDIQTY  REDEFINES IN-AREA.                                       
007446*    03  -COPY WEDIQTY2                                                   
007447     EJECT                                                                
007448 01  IN-WEDILIN  REDEFINES IN-AREA.                                       
007449*    03  -COPY WEDILIN0                                                   
007450     EJECT                                                                
007451 01  IN-WEDIUNB  REDEFINES IN-AREA.                                       
007452*    03  -COPY WEDIUNB1                                                   
007453     EJECT                                                                
007454 01  IN-WEDIUNS1 REDEFINES IN-AREA.                                       
007455*    03  -COPY WEDIUNS1                                                   
007456     EJECT                                                                
007900                                                                          
010901 PROCEDURE DIVISION.                                                      
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-LAES-W46383                                              
011520     PERFORM C-NOLLST-DISP                                                
011600                                                                          
011602     PERFORM UNTIL END-OF-W46383                                          
011700       IF ORDR-IDPTYP = 'UNS'                                             
011701         PERFORM S02-SKRIV-W46384                                         
011702         PERFORM D-BYGG-LOGG                                              
011703         PERFORM S03-SKRIV-W46386                                         
011704         PERFORM C-NOLLST-DISP                                            
011705       ELSE                                                               
011710         PERFORM B-BYGG-ORDER-RESPONSE                                    
011720       END-IF                                                             
011800       PERFORM S01-LAES-W46383                                            
012400     END-PERFORM                                                          
012600                                                                          
012800     PERFORM Z-FINIT                                                      
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013210                                                                          
013300 A-INIT SECTION.                                                          
013410     OPEN INPUT  W46383                                                   
013502     OPEN OUTPUT W46384                                                   
013503                 W46386                                                   
013600                                                                          
013700     ACCEPT WS-DAGENS-KLOCKA         FROM TIME                            
013701     ACCEPT WS-DAGENS-KLOCKA-2       FROM TIME                            
013710     MOVE WS-DAGENS-KLOCKA           TO WS-DAGENS-KLOCKA-ALFA             
013800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM-8                 
013801     MOVE WS-DAGENS-DATUM-8          TO WS-DAGENS-DATUM-ALFA              
013802     MOVE FUNCTION CURRENT-DATE(1:6) TO WS-DAGENS-DATUM-6                 
013804                                                                          
013810     MOVE IDPGM                      TO POSTSUM-PROGNAMN                  
014000     .                                                                    
014100     EJECT                                                                
014101                                                                          
014110 B-BYGG-ORDER-RESPONSE SECTION.                                           
014120     EVALUATE ORDR-IDPTYP                                                 
014718        WHEN 'BGM'                                                        
014720              PERFORM BA-BYGG-BGM                                         
014722        WHEN 'DTM'                                                        
014723              PERFORM BB-BYGG-DTM-XX                                      
014728        WHEN 'RFF'                                                        
014729              PERFORM BC-BYGG-RFF                                         
014731        WHEN 'NAD'                                                        
014732              PERFORM BD-BYGG-NAD-XX                                      
014736        WHEN 'LIN'                                                        
014737              PERFORM BE-BYGG-LIN                                         
014741        WHEN 'QTY'                                                        
014742              PERFORM BF-BYGG-QTY                                         
014743        WHEN 'UNB'                                                        
014744              PERFORM BG-SKAPA-SNDDAT-SNDTIME                             
014750     END-EVALUATE                                                         
014752     .                                                                    
014753     EJECT                                                                
014754                                                                          
014760 BA-BYGG-BGM SECTION.                                                     
014800                                                                          
015105     IF BGM-DOC-NAME-CODE = '220'                                         
015106       UNSTRING BGM-DOCNO   DELIMITED BY SPACE                            
015107                            INTO UT-IDPRODNR                              
015108       IF UT-IDPRODNR NOT NUMERIC                                         
015109         DISPLAY 'PRODNR EJ NUMERISKT'                                    
015110         MOVE RKOD-ABEND-UTAN-DUMP        TO RKOD-ABEND                   
015111         PERFORM S99-ABEND                                                
015112       END-IF                                                             
015113       MOVE UT-IDPRODNR                 TO UTL-IDPRODNR                   
015138     ELSE                                                                 
015139       DISPLAY 'FELAKTIG BGM-CODE: ' BGM-DOC-NAME-CODE                    
015140       MOVE RKOD-ABEND-UTAN-DUMP        TO RKOD-ABEND                     
015141       PERFORM S99-ABEND                                                  
015142     END-IF                                                               
015143     .                                                                    
015144     EJECT                                                                
015145                                                                          
015146 BB-BYGG-DTM-XX SECTION.                                                  
015148     MOVE ZERO                     TO UTL-DALEVDAT                        
015149     IF DTM1-QUAL = '137'                                                 
015150       MOVE DTM1-DATE-TIME (1:8)   TO WS-TEST-DATUM                       
015151       PERFORM S04-TESTA-DATUM                                            
015152       MOVE DTM1-DATE-TIME (1:8)   TO UT-DABEKDAT                         
015153                                      UTL-DABEKDAT                        
015154       IF DTM1-DATE-TIME (9:6) NUMERIC                                    
015155         MOVE DTM1-DATE-TIME (9:6) TO UT-TIBEKR                           
015156                                      UTL-TIBEKR                          
015157       ELSE                                                               
015158         DISPLAY 'FELAKTIG TID   '                                        
015159         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
015160         PERFORM S99-ABEND                                                
015161       END-IF                                                             
015162     ELSE                                                                 
015163       IF DTM1-QUAL = '55'                                                
015164         MOVE DTM1-DATE-TIME (1:8) TO WS-TEST-DATUM                       
015165         PERFORM S04-TESTA-DATUM                                          
015166         MOVE DTM1-DATE-TIME (1:8) TO UT-DALEVDAT                         
015167                                      UTL-DALEVDAT                        
015168       ELSE                                                               
015169         DISPLAY 'FELAKTIG DTM-QUAL: ' DTM1-QUAL                          
015170         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
015171         PERFORM S99-ABEND                                                
015172       END-IF                                                             
015180     END-IF                                                               
015191     .                                                                    
015192     EJECT                                                                
015193                                                                          
015194 BC-BYGG-RFF SECTION.                                                     
015195     IF RFF1-QUAL = 'CR'                                                  
015196       UNSTRING RFF1-REFNO  DELIMITED BY SPACE                            
015197                            INTO UT-IDORDNR7                              
015198       IF UT-IDORDNR7 NOT NUMERIC                                         
015199         DISPLAY 'ORDNR EJ NUMERISKT'                                     
015200         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
015201         PERFORM S99-ABEND                                                
015202       END-IF                                                             
015203       MOVE UT-IDORDNR7            TO UTL-IDORDNR7                        
015204     ELSE                                                                 
015205       DISPLAY 'FELAKTIG RFF-QUAL: ' RFF1-QUAL                            
015206       MOVE RKOD-ABEND-UTAN-DUMP   TO RKOD-ABEND                          
015207       PERFORM S99-ABEND                                                  
015208     END-IF                                                               
015209     .                                                                    
015210     EJECT                                                                
015211                                                                          
015212 BD-BYGG-NAD-XX SECTION.                                                  
015213     IF NAD2-QUAL = 'BY'                                                  
015214       MOVE NAD2-COUNTRY-CODE            TO UT-IDDC                       
015215                                            UTL-IDDC                      
015216     ELSE                                                                 
015217       IF NAD2-QUAL = 'SE'                                                
015218         UNSTRING NAD2-PARTY-ID   DELIMITED BY SPACE                      
015219                                  INTO UT-IDLEVNR                         
015231         MOVE UT-IDLEVNR                 TO UTL-IDLEVNR                   
015232       ELSE                                                               
015233         IF NAD2-QUAL = 'CN'                                              
015234           MOVE NAD2-PARTY-ID (1:4)      TO UT-IDDISTR                    
015235                                            UTL-IDDISTR                   
015236           MOVE NAD2-PARTY-ID (5:6)      TO UT-IDKUNDNR                   
015237                                            UTL-IDKUNDNR                  
015238         ELSE                                                             
015239           DISPLAY 'FELAKTIG NAD-QUAL: ' NAD2-QUAL                        
015240           MOVE RKOD-ABEND-UTAN-DUMP     TO RKOD-ABEND                    
015241           PERFORM S99-ABEND                                              
015242         END-IF                                                           
015243       END-IF                                                             
015244     END-IF                                                               
015245     .                                                                    
015246     EJECT                                                                
015247                                                                          
015248 BE-BYGG-LIN SECTION.                                                     
015249     IF FIRST-TIME                                                        
015250         MOVE NEJ TO FIRST-TIME-SW                                        
015251     ELSE                                                                 
015252         PERFORM S02-SKRIV-W46384                                         
015253         PERFORM S05-NOLLST-LIN                                           
015254     END-IF                                                               
015255                                                                          
015256     UNSTRING LIN-LINENO  DELIMITED BY SPACE                              
015257                          INTO UT-IDRADNR                                 
015258     IF UT-IDRADNR  NOT NUMERIC                                           
015259       DISPLAY 'RADNR  EJ NUMERISKT'                                      
015260       MOVE RKOD-ABEND-UTAN-DUMP    TO RKOD-ABEND                         
015261       PERFORM S99-ABEND                                                  
015262     END-IF                                                               
015263     MOVE UT-IDRADNR                TO UTL-IDRADNR                        
015264                                                                          
015265     UNSTRING LIN-ITEMNO  DELIMITED BY SPACE                              
015266                          INTO UT-IDARTNR                                 
015267     IF UT-IDARTNR NOT NUMERIC                                            
015268       DISPLAY 'ARTNR EJ NUMERISKT'                                       
015269       MOVE RKOD-ABEND-UTAN-DUMP    TO RKOD-ABEND                         
015270       PERFORM S99-ABEND                                                  
015280     END-IF                                                               
015287                                                                          
015288     MOVE 'VO '                     TO CIA-IDARTPRE-IN                    
015289     MOVE UT-IDARTNR                TO CIA-IDARTBET-IN                    
015290     CALL W009CIA USING CIA-W009CIA                                       
015291     IF CIA-KDSVAR NOT = 'F'                                              
015292       MOVE CIA-IDARTPRE-UT         TO UT-IDARTPRE                        
015293       MOVE CIA-IDARTBET-UT         TO UT-IDARTBET                        
015294     ELSE                                                                 
015295       DISPLAY 'ARTIKEL ' UT-IDARTNR ' FUNKAR EJ I W009CIA'               
015296       MOVE RKOD-ABEND-UTAN-DUMP    TO RKOD-ABEND                         
015297       PERFORM S99-ABEND                                                  
015298     END-IF                                                               
015299     MOVE UT-IDARTNR                TO UTL-IDARTNR                        
015300                                                                          
015301* ANNULLATION                                                             
015302     IF LIN-ACTION-REQ = '2'                                              
015303       MOVE 83                      TO UT-KDORDBEK                        
015304                                       UTL-KDORDBEK                       
015305     ELSE                                                                 
015306* FÖRSENING                                                               
015307       IF LIN-ACTION-REQ = '3'                                            
015308         MOVE 96                    TO UT-KDORDBEK                        
015309                                       UTL-KDORDBEK                       
015310       ELSE                                                               
015311* OKÄND ARTIKEL                                                           
015312         IF LIN-ACTION-REQ = '7'                                          
015313           MOVE 22                  TO UT-KDORDBEK                        
015314                                       UTL-KDORDBEK                       
015315         ELSE                                                             
015316* ANNULLATION EJ MÖJLIG                                                   
015317           IF LIN-ACTION-REQ = '4'                                        
015318             MOVE 20                TO UT-KDORDBEK                        
015319                                       UTL-KDORDBEK                       
015320           ELSE                                                           
015321             IF LIN-ACTION-REQ = '10'                                     
015322               MOVE 67              TO UT-KDORDBEK                        
015323                                       UTL-KDORDBEK                       
015324             ELSE                                                         
015325               DISPLAY 'FELAKTIG ORSAKSKOD'                               
015326               MOVE RKOD-ABEND-UTAN-DUMP                                  
015327                                    TO RKOD-ABEND                         
015328               PERFORM S99-ABEND                                          
015329             END-IF                                                       
015330           END-IF                                                         
015331         END-IF                                                           
015332       END-IF                                                             
015333     END-IF                                                               
015334     .                                                                    
015335     EJECT                                                                
015336                                                                          
015337 BF-BYGG-QTY SECTION.                                                     
015338     MOVE QTY2-QUANTITY              TO UT-KVBEART                        
015339                                        UTL-KVANTAL                       
015340     .                                                                    
015341     EJECT                                                                
015342                                                                          
015343 BG-SKAPA-SNDDAT-SNDTIME SECTION.                                         
015344                                                                          
015345     IF UNB1-DATE > 501231                                                
015346       COMPUTE UTL-DASNDDAT = 19000000 + UNB1-DATE                        
015347     ELSE                                                                 
015348       COMPUTE UTL-DASNDDAT = 20000000 + UNB1-DATE                        
015349     END-IF                                                               
015350                                                                          
015351     MOVE UNB1-TIME                  TO UTL-TISNDTID                      
015352     .                                                                    
015353     EJECT                                                                
015354 C-NOLLST-DISP SECTION.                                                   
015355     MOVE SPACE                      TO UT-IDDC                           
015356                                        UT-IDLEVNR                        
015357     MOVE JA                         TO FIRST-TIME-SW                     
015359     MOVE ZERO                       TO UT-IDDISTR                        
015360                                        UT-IDKUNDNR                       
015361                                        UT-IDORDNR7                       
015362                                        UT-IDPRODNR                       
015363                                        UT-IDRADNR                        
015364                                        UT-IDARTNR                        
015365                                        UT-IDARTPRE                       
015366                                        UT-IDARTBET                       
015367                                        UT-KVBEART                        
015368                                        UT-KDORDBEK                       
015369                                        UT-DALEVDAT                       
015370                                        UT-DABEKDAT                       
015371                                        UT-TIBEKR                         
015372     .                                                                    
015373     EJECT                                                                
015374                                                                          
015375 D-BYGG-LOGG SECTION.                                                     
015376     MOVE 'RSP'                   TO UTL-IDPTYP                           
015377     MOVE WS-DAGENS-DATUM-8       TO UTL-DAREGDAT                         
015378     MOVE WS-DAGENS-KLOCKA-1-6    TO UTL-TIREGTID                         
015379     MOVE SPACE                   TO UTL-IDSUPREF                         
015380                                     UTL-BERADREF                         
015381                                     UTL-KDVIA                            
015382     MOVE ZERO                    TO UTL-IDKOLLI                          
015383                                     UTL-DAFAKT                           
015384                                     UTL-DAPACKN                          
015385                                     UTL-DASKEPPN                         
015386                                     UTL-DASUPREF                         
015387                                     UTL-TIPACTID                         
015388                                     UTL-TISUPTID                         
015389                                     UTL-KDORDKL                          
015390                                                                          
015391     .                                                                    
015392     EJECT                                                                
015393                                                                          
015394 Z-FINIT SECTION.                                                         
015395     CLOSE W46383                                                         
015396           W46384                                                         
015397           W46386                                                         
015398                                                                          
015399     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015401     .                                                                    
015402     EJECT                                                                
015403                                                                          
015404 S01-LAES-W46383  SECTION.                                                
015405     READ W46383 INTO IN-AREA                                             
015406     AT END                                                               
015407        MOVE HIGH-VALUE   TO IN-AREA                                      
015408        SET END-OF-W46383 TO TRUE                                         
015409                                                                          
015410     NOT AT END                                                           
015411        MOVE 'W46383'   TO POSTSUM-FDNAMN                                 
015412        MOVE 'W46384D1' TO POSTSUM-DDNAMN2                                
015413        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
015414        CALL POSTSUM USING POSTSUM-PARM                                   
015415     END-READ                                                             
015416     .                                                                    
015417     EJECT                                                                
015418 S02-SKRIV-W46384 SECTION.                                                
015419     WRITE DISP-POST FROM UT-AREA                                         
015420                                                                          
015421     MOVE 'DISP'       TO POSTSUM-TRANSTYP                                
015422     MOVE 'W46384'     TO POSTSUM-FDNAMN                                  
015423     MOVE 'W46384D2'   TO POSTSUM-DDNAMN2                                 
015424     CALL POSTSUM USING POSTSUM-PARM                                      
015425                                                                          
015426* OM ARTIKEL ÄR OKÄND (22) SKALL ÄVEN ANNULLATION GÖRAS (83)              
015427*    IF UT-KDORDBEK = '22'                                                
015428*      MOVE 83         TO UT-KDORDBEK                                     
015429*                         UTL-KDORDBEK                                    
015430*      WRITE DISP-POST FROM UT-AREA                                       
015431*                                                                         
015432*      MOVE 'DISP'     TO POSTSUM-TRANSTYP                                
015433*      MOVE 'W46384'   TO POSTSUM-FDNAMN                                  
015434*      MOVE 'W46384D2' TO POSTSUM-DDNAMN2                                 
015435*      CALL POSTSUM USING POSTSUM-PARM                                    
015436*    END-IF                                                               
015437     .                                                                    
015438     EJECT                                                                
015439                                                                          
015440 S03-SKRIV-W46386 SECTION.                                                
015441     WRITE UT-LOGG   FROM UTL-W46341                                      
015442                                                                          
015443     MOVE 'LOGG'       TO POSTSUM-TRANSTYP                                
015444     MOVE 'W46386'     TO POSTSUM-FDNAMN                                  
015445     MOVE 'W46384D3'   TO POSTSUM-DDNAMN2                                 
015446     CALL POSTSUM USING POSTSUM-PARM                                      
015447     .                                                                    
015448     EJECT                                                                
015449                                                                          
015450 S04-TESTA-DATUM SECTION.                                                 
015451     MOVE "AAMMDD" TO DAT-KDDATFORM                                       
015452     MOVE WS-TEST-DATUM (3:6)   TO DAT-I-TIDATUM                          
015453     CALL WDATKONV USING                                                  
015454          DAT-KDDATFORM,                                                  
015455          DAT-I-TIDATUM,                                                  
015456          DAT-O-TIDATUM,                                                  
015457          DAT-KDSVAR                                                      
015458                                                                          
015459     IF DAT-KDSVAR-OK                                                     
015460       IF WS-DAGENS-DATUM-ALFA (1:2) NOT = WS-TEST-DATUM (1:2)            
015461         DISPLAY 'FELAKTIGT DATUM   '                                     
015462         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                          
015463         PERFORM S99-ABEND                                                
015464       END-IF                                                             
015465     ELSE                                                                 
015466       DISPLAY 'FELAKTIGT DATUM   '                                       
015467       MOVE RKOD-ABEND-UTAN-DUMP   TO RKOD-ABEND                          
015468       PERFORM S99-ABEND                                                  
015469     END-IF                                                               
015470     .                                                                    
015471     EJECT                                                                
015472                                                                          
015473 S05-NOLLST-LIN  SECTION.                                                 
015474     MOVE ZERO                       TO UT-IDRADNR                        
015475                                        UT-IDARTNR                        
015476                                        UT-IDARTPRE                       
015477                                        UT-IDARTBET                       
015478                                        UT-KVBEART                        
015479                                        UT-KDORDBEK                       
015480                                        UT-DALEVDAT                       
015481     .                                                                    
015482     EJECT                                                                
015483                                                                          
015484 S99-ABEND SECTION.                                                       
015485     MOVE 'S' TO POSTSUM-OPKOD                                            
015486     CALL POSTSUM USING POSTSUM-PARM                                      
015490     CALL ABEND USING RKOD-ABEND                                          
015500     .                                                                    
