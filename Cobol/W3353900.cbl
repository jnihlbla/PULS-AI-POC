000100 ID  DIVISION.                                                            
000200     SKIP3                                                                
000300 PROGRAM-ID.    W3353900.                                                 
000400*                                                                         
000500*AUTHOR.        RONNY STENHOLM                                            
000600*DATE-WRITTEN.  FEB  1994.                                                
000700                                                                          
000800*    REMARKS :                                                            
000900*                                                                         
001000*                                                                         
001100*   USABENÄMNINGAR PÅGÅR                                                  
001200*                                                                         
001300*   FUNKTION: MATCHAR REGISTER MED 91042-FIL SKAPAR ARTIKEL INFO          
001400*             TILL MARKNADSBOLAGEN PÅ DE ÄNDRADE ELLER NYA                
001500*             ARTIKLARNA.                                                 
001600*             DETTA PGM ANVÄNDS VID DEN S.K. REFRESHINGEN                 
001700*             DÅ SAMTLIGA ARTIKLAR SOM UPPFYLLER VILLKOREN TAS MED        
001800*             KDERS = 52 TAS INTE MED.                                    
001900*             ÄNDRAD CPY-TEXT FÖR FILE W91042 PGA NYA FÄLT TILL           
002000*             MB. FILEN HETER W91042 I PGM MEN ANVÄNDER CPY W33539        
002100*                                                                         
002200*       OBS ATT DET FINNS TRE PGM SOM HAR SNARLIK LOGIK                   
002300*           FÖR ATT SÄNDA ARTINFO TILL MARKNADSBOLAGEN                    
002400*           W33539 W33538 OCH W33560                                      
002500*           OM NÅGOT AV DESSA ÄNDRAS TÄNK DÅ LITE PÅ DE ANDRA             
002600*           OCKSÅ.                                                        
002700*                                                                         
002800*ETRACKER/4436924 PRICE VILL HA ALLA POSTTYPER ALLTID, HAR TIDGARE        
002900*           FÅTT POSTTYP 402 OCH 403 ENDAST OM DE VARIT STÖRRE ÄN         
003000*           SPACE.080422/EÖ                                               
003100*                                                                         
003200*    ABENDKODER:                                                          
003300*                                                                         
003400*        U0016    - OM RETURKOD FRÅN SORT                                 
003500     EJECT                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100     SKIP2                                                                
004200*- - - - - - - - - - - - INFILER:                                         
004300     SELECT W91042                       ASSIGN TO W33539D1.              
004400     SELECT REG-IN                       ASSIGN TO W33539D2.              
004500     SKIP2                                                                
004600*- - - - - - - - - - - - UTFILER:                                         
004700     SELECT REG-UT                       ASSIGN TO W33539D3.              
004800     SELECT W33539                       ASSIGN TO W33539D4.              
004900     SKIP2                                                                
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200     SKIP2                                                                
005300 FILE SECTION.                                                            
005400     SKIP3                                                                
005500 FD  W91042                                                               
005600     RECORDING      F                                                     
005700     BLOCK CONTAINS 0.                                                    
005800     SKIP2                                                                
005900*01  FILLER -COPY W33539     -L.                                          
006000     SKIP2                                                                
006100 FD  REG-IN                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS 0.                                                    
006400     SKIP2                                                                
006500*01  FILLER -COPY W33538     -L.                                          
006600     SKIP2                                                                
006700 FD  REG-UT                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS 0.                                                    
007000     SKIP2                                                                
007100*01  REG-UT-POST -COPY W33538     -L.                                     
007200     SKIP2                                                                
007300 FD  W33539                                                               
007400     RECORDING       V                                                    
007500     BLOCK CONTAINS 0.                                                    
007600     SKIP2                                                                
007700*01  INFO-POST1 -COPY W335401A   -L.                                      
007800     SKIP2                                                                
007900*01  INFO-POST2 -COPY W335402A   -L.                                      
008000     SKIP2                                                                
008100*01  INFO-POST3 -COPY W335405A   -L.                                      
008200     SKIP2                                                                
008300 WORKING-STORAGE SECTION.                                                 
008400*    -COPY WY2000W2                                                       
008500     SKIP3                                                                
008600*    -COPY WY2000W3                                                       
008700     SKIP3                                                                
008800*    -COPY WWPRODSL                                                       
008900     SKIP3                                                                
009000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3353900'.               
009100     SKIP2                                                                
009200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
009300                                                                          
009400 77  JA                          PIC X(1)    VALUE 'J'.                   
009500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
009600     SKIP2                                                                
009700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
009800                                                                          
009900 77  W91042-EOF                  PIC X(1)    VALUE 'N'.                   
010000 77  REG-IN-EOF                  PIC X(1)    VALUE 'N'.                   
010100     SKIP2                                                                
010200*- - - - - - - - - - - - - -  DIVERSE VARIABLER                           
010300 77  BEARBETNING                 PIC X(1).                                
010400 77  ART-ANDRAD                  PIC X(1).                                
010500 77  SKRIV-ERS                   PIC X(1).                                
010600 77  WS-PRHANTK                  PIC S9(5)V9(2)      COMP-3.              
010700 77  VECKOR-TILL-TIFINLV         PIC S9(3) COMP-3 VALUE ZERO.             
010800 77  AAR-SEDAN-TIERSDAT          PIC S9(3) COMP-3 VALUE ZERO.             
010900 77  IX                          PIC S9(2) COMP-3 VALUE ZERO.             
011000 77  IX-RAD                      PIC S9(5) COMP-3 VALUE ZERO.             
011100 77  IX-KOLUMN                   PIC S9(5) COMP-3 VALUE ZERO.             
011200 77  IX-STATNR                   PIC S9(3) COMP-3 VALUE ZERO.             
011300 77  KDHBLKRV                    PIC S9(5) COMP-3.                        
011400     EJECT                                                                
011500*- - - - - - - - - - - - - -  TILLÄGG 971210                              
011600 01  WS-PLUS-VECKOR              PIC S9(3) COMP-3 VALUE ZERO.             
011700 01  WS-TIFINLV                  PIC 9(5).                                
011800 01  FILLER REDEFINES WS-TIFINLV.                                         
011900     03  WS-TIFINLV-AAVV         PIC 9(4).                                
012000     03  FILLER                  PIC 9(1).                                
012100 01  WS-AKT-AAVV                 PIC 9(4).                                
012200 01  FILLER REDEFINES WS-AKT-AAVV.                                        
012300     03  WS-AKT-AA               PIC 9(2).                                
012400     03  WS-AKT-VV               PIC 9(2).                                
012500 01  WS-AKT-AAVV-NUM             PIC S9(5) COMP-3.                        
012600*- - - - - - - - - - - - - -  ÅR-VECKA                                    
012700 01  AAR-VECKA                   PIC 9(5).                                
012800 01  FILLER REDEFINES AAR-VECKA.                                          
012900     03  AAR                     PIC 99.                                  
013000     03  VECKA                   PIC 99.                                  
013100     03  FILLER                  PIC  9.                                  
013200     SKIP3                                                                
013300 01  AA53D.                                                               
013400     03  AA                      PIC 99.                                  
013500     03  FILLER                  PIC 999 VALUE 531.                       
013600     SKIP3                                                                
013700 01  AAAAVVD-WEEK                PIC 9(7) VALUE ZERO.                     
013800 01  FILLER REDEFINES AAAAVVD-WEEK.                                       
013900     03  SS-WEEK                 PIC 99.                                  
014000     03  AA-WEEK                 PIC 99.                                  
014100     03  VV-WEEK                 PIC 99.                                  
014200     03  D-WEEK                  PIC  9.                                  
014300     SKIP3                                                                
014400 01  DATUM-10AR                  PIC 9(7) VALUE ZERO.                     
014500     SKIP3                                                                
014600 01  WS-TIERSDAT                 PIC 9(7)    VALUE ZERO.                  
014700 01  FILLER REDEFINES WS-TIERSDAT.                                        
014800     03  WS-TIERSDAT-SEKEL       PIC 99.                                  
014900     03  WS-TIERSDAT-AAVVD       PIC 9(5).                                
015000     EJECT                                                                
015100 01  DYNAMISKA-SUBPROGRAM.                                                
015200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015600     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
015700     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
015800     SKIP3                                                                
015900*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
016000                                                                          
016100 01  RETURKODER.                                                          
016200     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
016300     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
016400     03  RKOD-ABEND-MED-DUMP     PIC S9(4)  COMP SYNC VALUE +1000.        
016500     EJECT                                                                
016600*- - - - - - - - - - - - - - - - PARAMETRAR TILL DATKORT                  
016700*                                                                         
016800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016900     SKIP2                                                                
017000*01  -COPY WDATKORT                                                       
017100     EJECT                                                                
017200*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
017300*                                                                         
017400*01  -COPY WDATAREA                                                       
017500     EJECT                                                                
017600                                                                          
017700* VARIABLER TILL SUBPROGRAM W400ARTU                                      
017800*01  -COPY W400ARTU                                                       
017900     SKIP2                                                                
018000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
018100                                                                          
018200*01  -COPY W0005       -PRE  POSTSUM-.                                    
018300     EJECT                                                                
018400******************************************************************        
018500*         W91042-AREA                                            *        
018600******************************************************************        
018700 01  W91042-AREA.                                                         
018800*    03  FILLER -COPY W33539   -PRE W91042-.                              
018900     EJECT                                                                
019000******************************************************************        
019100*         REG-IN-AREA                                            *        
019200******************************************************************        
019300 01  REG-IN-AREA.                                                         
019400*    03  FILLER -COPY W33538     -PRE REG-IN-.                            
019500     EJECT                                                                
019600******************************************************************        
019700*         ART-INFO TILL MB                                       *        
019800******************************************************************        
019900 01  ART-INFO-AREA1.                                                      
020000*    03  FILLER -COPY W335401A -PRE INFO1-.                               
020100     EJECT                                                                
020200 01  ART-INFO-AREA2.                                                      
020300*    03  FILLER -COPY W335402A -PRE INFO2-.                               
020400     EJECT                                                                
020500 01  ART-INFO-AREA3.                                                      
020600*    03  FILLER -COPY W335405A -PRE INFO3-.                               
020700     EJECT                                                                
020800******************************************************************        
020900*         REG-UT-AREA                                            *        
021000******************************************************************        
021100 01  REG-UT-AREA.                                                         
021200*    03  FILLER -COPY W33538     -PRE REG-UT-.                            
021300     EJECT                                                                
021400                                                                          
021500 PROCEDURE DIVISION.                                                      
021600                                                                          
021700     PERFORM A-INIT                                                       
021800     PERFORM S01-LAS-W91042                                               
021900     PERFORM S02-LAS-REG-IN                                               
022000     PERFORM UNTIL W91042-EOF = JA                                        
022100       PERFORM B-SOLLA-POSTER                                             
022200       IF BEARBETNING = JA                                                
022300                                                                          
022400             IF  W91042-PRARTSJK  > ZERO AND                              
022500                 W91042-PRARTSTD  > ZERO                                  
022600                                                                          
022700               PERFORM C-FLYTTA-FAELT                                     
022800               PERFORM S10-SKRIV-REG-UT                                   
022900               PERFORM S11-SKRIV-INFO-401                                 
023000               PERFORM S12-SKRIV-TEXT-402                                 
023100               PERFORM S13-SKRIV-BEN-403                                  
023200             END-IF                                                       
023300             PERFORM S01-LAS-W91042                                       
023400       ELSE                                                               
023500         PERFORM S01-LAS-W91042                                           
023600       END-IF                                                             
023700     END-PERFORM                                                          
023800     PERFORM Z-FINIT                                                      
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT SECTION.                                                          
024400     SKIP2                                                                
024500     OPEN INPUT  REG-IN W91042                                            
024600     OPEN OUTPUT REG-UT                                                   
024700                 W33539                                                   
024800                                                                          
024900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025000     .                                                                    
025100     EJECT                                                                
025200 B-SOLLA-POSTER SECTION.                                                  
025300     SKIP2                                                                
025400******************************************************************        
025500*                                                                         
025600*    SELEKTERAR BORT FÖLJANDE POSTER:                                     
025700*    -OM MER ÄN 20 VECKOR TILL FÖRSTA INLEVERANS                          
025800*    -OM KDUART =  'M' ELLER 'A'                                          
025900*    -OM KDPRODSL = 0, 19, 28, 2971,72,73 ELLER 74                        
026000*    -OM IDFKNGRP = 0                                                     
026100*    -OM KDERS = 52 ****UTGÅR******                                       
026200*    -OM TIERSDAT ÄLDRE ÄN 4 ÅR                                           
026300*                                                                         
026400******************************************************************        
026500     SKIP2                                                                
026600     MOVE D-AAR           TO WS-AKT-AA                                    
026700     MOVE D-VECKA         TO WS-AKT-VV                                    
026800     MOVE WS-AKT-AAVV     TO WS-AKT-AAVV-NUM                              
026900     MOVE +30             TO WS-PLUS-VECKOR                               
027000     CALL W009VADD USING WS-AKT-AAVV-NUM WS-PLUS-VECKOR                   
027100     MOVE W91042-TIFINLV  TO WS-TIFINLV                                   
027200     MOVE WS-TIFINLV-AAVV TO TMP1-YYWW                                    
027300     MOVE WS-AKT-AAVV-NUM TO TMP2-YYWW                                    
027400     PERFORM WY2000P3                                                     
027500     IF TMP1-YYWW > TMP2-YYWW                                             
027600       MOVE NEJ           TO BEARBETNING                                  
027700     ELSE                                                                 
027800       MOVE JA            TO BEARBETNING                                  
027900     END-IF                                                               
028000*                                                                         
028100     IF W91042-TIERSDAT = 0                                               
028200       MOVE 9999999         TO WS-TIERSDAT                                
028300     ELSE                                                                 
028400       IF W91042-TIERSDAT > 50000                                         
028500         MOVE 19            TO WS-TIERSDAT-SEKEL                          
028600       ELSE                                                               
028700         MOVE 20            TO WS-TIERSDAT-SEKEL                          
028800       END-IF                                                             
028900       MOVE W91042-TIERSDAT TO WS-TIERSDAT-AAVVD                          
029000       MOVE 20              TO  SS-WEEK                                   
029100       MOVE D-AAR           TO  AA-WEEK                                   
029200       MOVE D-VECKA         TO  VV-WEEK                                   
029300       MOVE D-DAGNR         TO  D-WEEK                                    
029400       COMPUTE DATUM-10AR = AAAAVVD-WEEK - 10000                          
029500     END-IF                                                               
029600                                                                          
029700     IF BEARBETNING = JA                                                  
029800       MOVE W91042-KDPRODSL      TO TEST-KDPRODSL                         
029900       IF W91042-IDFKNGRP = 0                                             
030000       OR W91042-KDERS-UTG > 0                                            
030100       OR W91042-KDERS    = +52                                           
030200       OR W91042-KDPRODSL = 0                                             
030300       OR KDPRODSL-VOLVO-EMB                                              
030400       OR KDPRODSL-BIMA                                                   
030500*  ÖKAT FRÅN 4 TILL 8 ÅR  TILL 10 ÅR**''                                  
030600       OR WS-TIERSDAT < DATUM-10AR                                        
030700         MOVE NEJ TO BEARBETNING                                          
030800       END-IF                                                             
030900     END-IF                                                               
031000                                                                          
031100     .                                                                    
031200     EJECT                                                                
031300 C-FLYTTA-FAELT SECTION.                                                  
031400     SKIP2                                                                
031500     MOVE '401'                  TO INFO1-IDPTYP                          
031600     MOVE 'A'                    TO INFO1-IDVTYP                          
031700     MOVE W91042-IDARTNR         TO INFO1-IDARTNR                         
031800                                    REG-UT-ART-IDARTNR                    
031900     MOVE W91042-IDFKNGRP        TO INFO1-IDFKNGRP                        
032000                                    REG-UT-ART-IDFKNGRP                   
032100     MOVE W91042-KDSRA           TO INFO1-KDSRA                           
032200                                    REG-UT-ART-KDSRA                      
032300     MOVE W91042-KVQPACK-0       TO INFO1-KVQPACK-0                       
032400                                    REG-UT-ART-KVQPACK-0                  
032500     MOVE W91042-KDARTURS        TO ARTU-KDARTURS                         
032600     MOVE 0                      TO ARTU-IDDISTR                          
032700     MOVE SPACE                  TO ARTU-IDDC                             
032800     PERFORM S031-CALL-W400ARTU                                           
032900     MOVE ARTU-KDARTURS-NUM      TO INFO1-KDARTURS-NUM                    
033000     MOVE W91042-KDARTURS        TO REG-UT-ART-KDARTURS                   
033100     MOVE W91042-KDPRODSL        TO INFO1-KDPRODSL                        
033200                                    REG-UT-ART-KDPRODSL                   
033300     MOVE W91042-VLARTNTO        TO INFO1-VLARTNTO                        
033400                                    REG-UT-ART-VLARTNTO                   
033500     MOVE W91042-VKART           TO INFO1-VKART                           
033600                                    REG-UT-ART-VKART                      
033700     MOVE W91042-IDSTATNR(3)     TO INFO1-IDSTATNR                        
033800                                    REG-UT-ART-IDSTATNR                   
033900     MOVE W91042-KDVSOP          TO INFO1-KDVSOP                          
034000                                    REG-UT-ART-KDVSOP                     
034100     MOVE W91042-KDSORT          TO INFO1-KDSORT                          
034200                                    REG-UT-ART-KDSORT                     
034300     MOVE W91042-KDERS           TO INFO1-KDERS                           
034400                                    REG-UT-ART-KDERS                      
034500     MOVE W91042-KDBPSR          TO INFO1-KDBPSR                          
034600                                    REG-UT-ART-KDBPSR                     
034700     MOVE ZERO                   TO INFO1-KDBBCL                          
034800                                    REG-UT-ART-KDBBCL                     
034900     MOVE W91042-IDLEVNR         TO INFO1-IDLEVNR                         
035000                                    REG-UT-ART-IDLEVNR                    
035100     MOVE W91042-PRARTSJK        TO INFO1-PRARTSJK                        
035200                                    REG-UT-ART-PRARTSJK                   
035300     MOVE W91042-PRARTSTD        TO INFO1-PRARTSTD                        
035400                                    REG-UT-ART-PRARTSTD                   
035500     MOVE W91042-FLIART          TO INFO1-FLIART                          
035600                                    REG-UT-ART-FLIART                     
035700     MOVE W91042-IDPROJ          TO INFO1-IDPROJ                          
035800                                    REG-UT-ART-IDPROJ                     
035900     MOVE W91042-IDAO(1)         TO INFO1-IDAO(1)                         
036000                                    REG-UT-ART-IDAO(1)                    
036100     MOVE W91042-IDAO(2)         TO INFO1-IDAO(2)                         
036200                                    REG-UT-ART-IDAO(2)                    
036300                                                                          
036400     MOVE W91042-KDPSLLOC        TO INFO1-KDPSLLOC                        
036500                                    REG-UT-ART-KDPSLLOC                   
036600*  RÄKNAR UT DATUM ÅÅVVD TILL ÅÅMMDD                                      
036700*  TIFINLV   ÅÅVVD   OBS                                                  
036800*  TIFINLEV  ÅÅMMDD  OBS                                                  
036900       MOVE W91042-TIFINLV   TO DAT-I-TIDATUM                             
037000       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
037100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
037200                           DAT-O-TIDATUM DAT-KDSVAR                       
037300     IF DAT-KDSVAR-FEL                                                    
037400       MOVE ZERO                   TO INFO1-TIFINLEV                      
037500     ELSE                                                                 
037600       MOVE DAT-TIAAMMDD           TO INFO1-TIFINLEV                      
037700     END-IF                                                               
037800*                                                                         
037900     MOVE W91042-TIFINLV         TO REG-UT-ART-TIFINLV                    
038000     MOVE W91042-IDANSK          TO INFO1-IDANSK                          
038100                                    REG-UT-ART-IDANSK                     
038200     COMPUTE INFO1-KVPB = W91042-KVPB-SEP(1) +                            
038300                          W91042-KVPB-SEP(2) +                            
038400                          W91042-KVPB-SEP(3) +                            
038500                          W91042-KVPB-SEP(4) +                            
038600                          W91042-KVPB-SEP(5) +                            
038700                          W91042-KVPB-SEP(6)                              
038800     MOVE W91042-KDVVKL          TO INFO1-KDVVKL                          
038900                                    REG-UT-ART-KDVVKL                     
039000     MOVE SPACE                  TO INFO1-BELEVART                        
039100                                                                          
039200     MOVE W91042-KDTIPPR         TO INFO1-KDTIPPR                         
039300                                    REG-UT-ART-KDTIPPR                    
039400     MOVE W91042-IDINK           TO INFO1-IDINK                           
039500                                    REG-UT-ART-IDINK                      
039600     MOVE W91042-TIREGDAT        TO INFO1-TIREGDAT                        
039700     MOVE W91042-KDUART          TO INFO1-KDUART                          
039800                                    REG-UT-ART-KDUART                     
039900     MOVE W91042-IDARTNR-MOTSV   TO INFO1-IDARTNR-MOTSV                   
040000                                    REG-UT-ART-IDARTNR-MOTSV              
040100     MOVE W91042-PRINK           TO INFO1-PRINK                           
040200                                    REG-UT-ART-PRINK                      
040300     MOVE W91042-IDRITN          TO INFO1-IDRITN                          
040400                                    REG-UT-ART-IDRITN                     
040500     COMPUTE WS-PRHANTK   =   W91042-PRDIRLON +                           
040600               W91042-PRDMTRL + W91042-PROVRPAL                           
040700                                                                          
040800     MOVE WS-PRHANTK             TO INFO1-PRHANTK                         
040900                                    REG-UT-ART-PRHANTK                    
041000     MOVE W91042-KDAGE           TO INFO1-KDAGE                           
041100                                    REG-UT-ART-KDAGE                      
041200     MOVE SPACE                  TO INFO1-KDRAB                           
041300     MOVE ZERO                   TO INFO1-PRARTBEL                        
041400     MOVE W91042-IDKAT(1)        TO INFO1-IDKAT(1)                        
041500                                    REG-UT-ART-IDKAT(1)                   
041600     MOVE W91042-IDKAT(2)        TO INFO1-IDKAT(2)                        
041700                                    REG-UT-ART-IDKAT(2)                   
041800     MOVE W91042-IDKAT(3)        TO INFO1-IDKAT(3)                        
041900                                    REG-UT-ART-IDKAT(3)                   
042000     MOVE W91042-FLLSRDEL        TO INFO1-FLLSRDEL                        
042100                                    REG-UT-ART-FLLSRDEL                   
042200     MOVE W91042-IDPROJUP        TO INFO1-IDPROJUP                        
042300                                    REG-UT-ART-IDPROJUP                   
042400     MOVE W91042-FLGEMFMC        TO INFO1-FLGEMFMC                        
042500                                    REG-UT-ART-FLGEMFMC                   
042600     MOVE W91042-TIURPROD        TO INFO1-TIURPROD                        
042700                                    REG-UT-ART-TIURPROD                   
042800                                                                          
042900     MOVE '402'                TO INFO2-IDPTYP                            
043000     MOVE 'A'                  TO INFO2-IDVTYP                            
043100     MOVE W91042-TEORSAK       TO INFO2-TEORSAK                           
043200                                  REG-UT-ART-TEORSAK                      
043300     MOVE W91042-TEARTNOT-3      TO INFO2-TEARTNOT-3                      
043400                                  REG-UT-ART-TEARTNOT-3                   
043500     MOVE W91042-TEARTNOT-7      TO INFO2-TEARTNOT-7                      
043600                                  REG-UT-ART-TEARTNOT-7                   
043700     MOVE '403'                TO INFO3-IDPTYP                            
043800     MOVE 'A'                  TO INFO3-IDVTYP                            
043900     MOVE W91042-BEART(4)      TO INFO3-BEART(1)                          
044000                                  REG-UT-ART-BEART(1)                     
044100     MOVE W91042-BEART(8)      TO INFO3-BEART(2)                          
044200                                  REG-UT-ART-BEART(2)                     
044300     MOVE W91042-BEART(10)     TO INFO3-BEART(3)                          
044400                                  REG-UT-ART-BEART(3)                     
044500     MOVE ZERO                 TO REG-UT-ART-KVKORT                       
044600     .                                                                    
044700                                                                          
044800     EJECT                                                                
044900 S01-LAS-W91042 SECTION.                                                  
045000     SKIP2                                                                
045100     READ W91042 INTO W91042-AREA                                         
045200                      AT END MOVE JA TO W91042-EOF                        
045300                      MOVE 999999999 TO W91042-IDARTNR                    
045400     END-READ                                                             
045500     IF W91042-EOF = NEJ                                                  
045600       MOVE 'W91042'             TO POSTSUM-FDNAMN                        
045700       MOVE 'W33539D1'           TO POSTSUM-DDNAMN2                       
045800       MOVE ZERO                 TO POSTSUM-TRANSTYP                      
045900       CALL POSTSUM   USING POSTSUM-PARM                                  
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 S02-LAS-REG-IN SECTION.                                                  
046400     SKIP2                                                                
046500     READ REG-IN INTO REG-IN-AREA                                         
046600                      AT END MOVE JA TO REG-IN-EOF                        
046700                      MOVE 999999999 TO REG-IN-ART-IDARTNR                
046800     END-READ                                                             
046900     IF REG-IN-EOF = NEJ                                                  
047000       MOVE 'REG-IN'            TO POSTSUM-FDNAMN                         
047100       MOVE 'W33539D2'          TO POSTSUM-DDNAMN2                        
047200       MOVE 'REGIN'             TO POSTSUM-TRANSTYP                       
047300       CALL POSTSUM   USING POSTSUM-PARM                                  
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 S10-SKRIV-REG-UT SECTION.                                                
047800     SKIP2                                                                
047900     WRITE REG-UT-POST FROM REG-UT-AREA                                   
048000     MOVE 'REG-UT'               TO POSTSUM-FDNAMN                        
048100     MOVE 'W33539D3'             TO POSTSUM-DDNAMN2                       
048200     MOVE 'REGUT'                TO POSTSUM-TRANSTYP                      
048300     CALL POSTSUM      USING POSTSUM-PARM                                 
048400     .                                                                    
048500     EJECT                                                                
048600 S11-SKRIV-INFO-401      SECTION.                                         
048700     SKIP2                                                                
048800*                                                                         
048900     WRITE INFO-POST1 FROM ART-INFO-AREA1                                 
049000     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
049100     MOVE 'W33539D4'             TO POSTSUM-DDNAMN2                       
049200     MOVE '401'                  TO POSTSUM-TRANSTYP                      
049300     CALL POSTSUM      USING POSTSUM-PARM                                 
049400     .                                                                    
049500                                                                          
049600 S12-SKRIV-TEXT-402      SECTION.                                         
049700     SKIP2                                                                
049800*                                                                         
049900     WRITE INFO-POST2 FROM ART-INFO-AREA2                                 
050000     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
050100     MOVE 'W33539D4'             TO POSTSUM-DDNAMN2                       
050200     MOVE '402'                  TO POSTSUM-TRANSTYP                      
050300     CALL POSTSUM      USING POSTSUM-PARM                                 
050400     .                                                                    
050500 S13-SKRIV-BEN-403       SECTION.                                         
050600     SKIP2                                                                
050700*                                                                         
050800     WRITE INFO-POST3 FROM ART-INFO-AREA3                                 
050900     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
051000     MOVE 'W33539D4'             TO POSTSUM-DDNAMN2                       
051100     MOVE '403'                  TO POSTSUM-TRANSTYP                      
051200     CALL POSTSUM      USING POSTSUM-PARM                                 
051300     .                                                                    
051400     EJECT                                                                
051500 S031-CALL-W400ARTU  SECTION.                                             
051600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
051700* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
051800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
051900     SKIP2                                                                
052000                                                                          
052100                                                                          
052200     CALL W400ARTU USING ARTU-W400ARTU                                    
052300                                                                          
052400     .                                                                    
052500     EJECT                                                                
052600 Z-FINIT SECTION.                                                         
052700     SKIP2                                                                
052800     CLOSE REG-IN                                                         
052900           W91042                                                         
053000           REG-UT                                                         
053100           W33539                                                         
053200                                                                          
053300     SKIP2                                                                
053400     MOVE 'S' TO POSTSUM-OPKOD                                            
053500     CALL POSTSUM USING POSTSUM-PARM                                      
053600     .                                                                    
053700     EJECT                                                                
053800*    -COPY WY2000P2                                                       
053900     EJECT                                                                
054000*    -COPY WY2000P3                                                       
