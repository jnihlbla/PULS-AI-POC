000100 ID  DIVISION.                                                            
000200     SKIP3                                                                
000300 PROGRAM-ID.    W3353800.                                                 
000400*                                                                         
000500*AUTHOR.        RONNY STENHOLM                                            
000600*DATE-WRITTEN.  FEB  1994.                                                
000700                                                                          
000800*                                                                         
000900*                                                                         
001000*   USABENÄMNINGAR PÅGÅR                                                  
001100*                                                                         
001200*                                                                         
001300*   FUNKTION: MATCHAR REGISTER MED 91042-FIL SKAPAR ARTIKEL INFO          
001400*             TILL MARKNADSBOLAGEN PÅ DE ÄNDRADE ELLER NYA                
001500*             ARTIKLARNA.                                                 
001600*                                                                         
001700*       PROGRAM READS WDD7                                                
001800*                                                                         
001900*       OBS ATT DET FINNS TRE PGM SOM HAR SNARLIK LOGIK                   
002000*           FÖR ATT SÄNDA ARTINFO TILL MARKNADSBOLAGEN                    
002100*           W33539 W33538 OCH W33560                                      
002200*           OM NÅGOT AV DESSA ÄNDRAS TÄNK DÅ LITE PÅ DE ANDRA             
002300*           OCKSÅ.                                                        
002400*           ÄNDRAD CPY-TXT FÖR FILEN W91042 PGA NYA FÄLT TILL             
002500*           MB. FILEN HETER W91042 I PGM MEN ANVÄNDER CTX W33539.         
002600*                                                                         
002700*ETRACKER/4436924 PRICE VILL HA ALLA POSTTYPER ALLTID, HAR TIDGARE        
002800*           FÅTT POSTTYP 402 OCH 403 ENDAST OM DE VARIT STÖRRE ÄN         
002900*           SPACE.080422/EÖ                                               
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*                                                                         
003300*        U0016    - OM RETURKOD FRÅN SORT                                 
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     SKIP2                                                                
004100*- - - - - - - - - - - - INFILER:                                         
004200     SELECT W91042                       ASSIGN TO W33538D1.              
004300     SELECT W33548                       ASSIGN TO W33538D6.              
004400     SELECT REG-IN                       ASSIGN TO W33538D2.              
004500     SKIP2                                                                
004600*- - - - - - - - - - - - UTFILER:                                         
004700     SELECT REG-UT                       ASSIGN TO W33538D3.              
004800     SELECT W33539                       ASSIGN TO W33538D4.              
004900     SELECT W33549                       ASSIGN TO W33538D5.              
005000     SKIP2                                                                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP2                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  W91042                                                               
005700     RECORDING      F                                                     
005800     BLOCK CONTAINS 0.                                                    
005900     SKIP2                                                                
006000*01  FILLER -COPY W33539     -L.                                          
006100     SKIP2                                                                
006200 FD  W33548                                                               
006300     RECORDING      F                                                     
006400     BLOCK CONTAINS 0.                                                    
006500     SKIP2                                                                
006600*01  FILLER -COPY W33548     -L.                                          
006700     SKIP2                                                                
006800 FD  REG-IN                                                               
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS 0.                                                    
007100     SKIP2                                                                
007200*01  FILLER -COPY W33538     -L.                                          
007300     SKIP2                                                                
007400 FD  REG-UT                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS 0.                                                    
007700     SKIP2                                                                
007800*01  REG-UT-POST -COPY W33538     -L.                                     
007900     SKIP2                                                                
008000 FD  W33539                                                               
008100     RECORDING       V                                                    
008200     BLOCK CONTAINS 0.                                                    
008300     SKIP2                                                                
008400*01  INFO-POST1 -COPY W335401A   -L.                                      
008500     SKIP2                                                                
008600*01  INFO-POST2 -COPY W335402A   -L.                                      
008700     SKIP2                                                                
008800*01  INFO-POST3 -COPY W335405A   -L.                                      
008900     SKIP2                                                                
009000 FD  W33549                                                               
009100     RECORDING       F                                                    
009200     BLOCK CONTAINS 0.                                                    
009300     SKIP2                                                                
009400*01  ERS-POST -COPY W33549    -L.                                         
009500     EJECT                                                                
009600 WORKING-STORAGE SECTION.                                                 
009700*    -COPY WY2000W2                                                       
009800     SKIP3                                                                
009900*    -COPY WY2000W3                                                       
010000     SKIP3                                                                
010100*    -COPY WWPRODSL                                                       
010200     SKIP3                                                                
010300 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3353800'.               
010400     SKIP2                                                                
010500*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
010600                                                                          
010700 77  JA                          PIC X(1)    VALUE 'J'.                   
010800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010900     SKIP2                                                                
011000*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
011100                                                                          
011200 77  W33539-EOF                  PIC X(1)    VALUE 'N'.                   
011300 77  W33548-EOF                  PIC X(1)    VALUE 'N'.                   
011400 77  W91042-EOF                  PIC X(1)    VALUE 'N'.                   
011500 77  REG-IN-EOF                  PIC X(1)    VALUE 'N'.                   
011600     SKIP2                                                                
011700*- - - - - - - - - - - - - -  DIVERSE VARIABLER                           
011800 77  BEARBETNING                 PIC X(1).                                
011900 77  ART-ANDRAD                  PIC X(1).                                
012000 77  SKRIV-ERS                   PIC X(1).                                
012100 77  WS-PRHANTK                  PIC S9(5)V9(2)      COMP-3.              
012200 77  VECKOR-TILL-TIFINLV         PIC S9(3) COMP-3 VALUE ZERO.             
012300 77  AAR-SEDAN-TIERSDAT          PIC S9(3) COMP-3 VALUE ZERO.             
012400 77  IX                          PIC S9(2) COMP-3 VALUE ZERO.             
012500 77  IX-RAD                      PIC S9(5) COMP-3 VALUE ZERO.             
012600 77  IX-KOLUMN                   PIC S9(5) COMP-3 VALUE ZERO.             
012700 77  IX-STATNR                   PIC S9(3) COMP-3 VALUE ZERO.             
012800 77  KDHBLKRV                    PIC S9(5) COMP-3.                        
012900 77  WS-WDD701-KVKORT            PIC S9(3) COMP-3 VALUE ZERO.             
013000     EJECT                                                                
013100*- - - - - - - - - - - - - -  TILLÄGG 971210                              
013200 01  WS-PLUS-VECKOR              PIC S9(3) COMP-3 VALUE ZERO.             
013300 01  WS-TIFINLV                  PIC 9(5).                                
013400 01  FILLER REDEFINES WS-TIFINLV.                                         
013500     03  WS-TIFINLV-AAVV         PIC 9(4).                                
013600     03  FILLER                  PIC 9(1).                                
013700 01  WS-AKT-AAVV                 PIC 9(4).                                
013800 01  FILLER REDEFINES WS-AKT-AAVV.                                        
013900     03  WS-AKT-AA               PIC 9(2).                                
014000     03  WS-AKT-VV               PIC 9(2).                                
014100 01  WS-AKT-AAVV-NUM             PIC S9(5) COMP-3.                        
014200*- - - - - - - - - - - - - -  ÅR-VECKA                                    
014300 01  AAR-VECKA                   PIC 9(5).                                
014400 01  FILLER REDEFINES AAR-VECKA.                                          
014500     03  AAR                     PIC 99.                                  
014600     03  VECKA                   PIC 99.                                  
014700     03  FILLER                  PIC  9.                                  
014800     SKIP3                                                                
014900 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
015000 01  FILLER REDEFINES DAGENS-DATUM.                                       
015100     03  DAGENS-DATUM-YYYY       PIC 9(4).                                
015200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015400     EJECT                                                                
015500 01  AA53D.                                                               
015600     03  AA                      PIC 99.                                  
015700     03  FILLER                  PIC 999 VALUE 531.                       
015800     SKIP3                                                                
015900 01  AAAAVVD-WEEK                PIC 9(7) VALUE ZERO.                     
016000 01  FILLER REDEFINES AAAAVVD-WEEK.                                       
016100     03  SS-WEEK                 PIC 99.                                  
016200     03  AA-WEEK                 PIC 99.                                  
016300     03  VV-WEEK                 PIC 99.                                  
016400     03  D-WEEK                  PIC  9.                                  
016500     SKIP3                                                                
016600 01  DATUM-10AR                  PIC 9(7) VALUE ZERO.                     
016700     SKIP3                                                                
016800 01  WS-TIERSDAT                 PIC 9(7)    VALUE ZERO.                  
016900 01  FILLER REDEFINES WS-TIERSDAT.                                        
017000     03  WS-TIERSDAT-SEKEL       PIC 99.                                  
017100     03  WS-TIERSDAT-AAVVD       PIC 9(5).                                
017200     SKIP3                                                                
017300 01  DYNAMISKA-SUBPROGRAM.                                                
017400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
017600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017800     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
017900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
018000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018200     SKIP3                                                                
018300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
018400                                                                          
018500 01  FELTEXT.                                                             
018600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018800                                                                          
018900 01  RETURKODER.                                                          
019000     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
019100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
019200     03  RKOD-ABEND-MED-DUMP     PIC S9(4)  COMP SYNC VALUE +1000.        
019300     EJECT                                                                
019400*- - - - - - - - - - - - - - - - PARAMETRAR TILL DATKORT                  
019500*                                                                         
019600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
019700     SKIP2                                                                
019800*01  -COPY WDATKORT                                                       
019900     EJECT                                                                
020000*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
020100*                                                                         
020200*01  -COPY WDATAREA                                                       
020300     EJECT                                                                
020400                                                                          
020500* VARIABLER TILL SUBPROGRAM W400ARTU                                      
020600*01  -COPY W400ARTU                                                       
020700     SKIP2                                                                
020800*--- - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
020900                                                                          
021000*01  -COPY W0005       -PRE  POSTSUM-.                                    
021100     EJECT                                                                
021200******************************************************************        
021300*         W91042-AREA                                            *        
021400******************************************************************        
021500 01  W91042-AREA.                                                         
021600*    03  FILLER -COPY W33539   -PRE W91042-.                              
021700     EJECT                                                                
021800******************************************************************        
021900*         REG-IN-AREA                                            *        
022000******************************************************************        
022100 01  REG-IN-AREA.                                                         
022200*    03  FILLER -COPY W33538     -PRE REG-IN-.                            
022300     EJECT                                                                
022400******************************************************************        
022500*         ART-INFO TILL MB                                       *        
022600******************************************************************        
022700 01  ART-INFO-AREA1.                                                      
022800*    03  FILLER -COPY W335401A -PRE INFO1-.                               
022900     EJECT                                                                
023000 01  ART-INFO-AREA2.                                                      
023100*    03  FILLER -COPY W335402A -PRE INFO2-.                               
023200     EJECT                                                                
023300 01  ART-INFO-AREA3.                                                      
023400*    03  FILLER -COPY W335405A -PRE INFO3-.                               
023500     EJECT                                                                
023600******************************************************************        
023700*         REG-UT-AREA                                            *        
023800******************************************************************        
023900 01  REG-UT-AREA.                                                         
024000*    03  FILLER -COPY W33538     -PRE REG-UT-.                            
024100     EJECT                                                                
024200******************************************************************        
024300*         ERS-UT-AREA                                            *        
024400******************************************************************        
024500 01  ERS-UT-AREA.                                                         
024600*    03  FILLER -COPY W33549     -PRE ERS-.                               
024700******************************************************************        
024800*         W33548-AREA                                            *        
024900******************************************************************        
025000 01  W33548-AREA.                                                         
025100*    03  FILLER -COPY W33548   -PRE W33548-.                              
025200     EJECT                                                                
025300 01  NYCKLAR-TILL-DLI.                                                    
025400     03  W-IDARTNR-X.                                                     
025500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025600     SKIP2                                                                
025700*    --- STATUS-KOD FRÅN IMS                                              
025800 01  STATUS-WS                   PIC XX.                                  
025900     88  SEGMENT-FINNS                       VALUE '  '.                  
026000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
026300     SKIP2                                                                
026400 01  GODK-STATUSKODER.                                                    
026500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026600     SKIP3                                                                
026700 01  SSA1                        PIC X(64).                               
026800 01  SSA2                        PIC X(64).                               
026900     EJECT                                                                
027000*    --- IMS FUNKTIONSKODER                                               
027100*01  -COPY W0003                                                          
027200     EJECT                                                                
027300*    ---  DLI INPUT-OUTPUT AREA                                           
027400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
027500 01  DLI-IO-WDD701.                                                       
027600*    03  -COPY WDD701  -PRE WDD701-                                       
027700     EJECT                                                                
027800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD702'.                      
027900 01  DLI-IO-WDD702.                                                       
028000*    03  -COPY WDD702  -PRE WDD702-                                       
028100     EJECT                                                                
028200 LINKAGE SECTION.                                                         
028300*                                                                         
028400*01  -COPY W0008  -PRE WDD7-                                              
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700 PROCEDURE DIVISION USING WDD7-PCB.                                       
028800                                                                          
028900 MAIN SECTION.                                                            
029000     ENTRY 'DLITCBL' USING WDD7-PCB.                                      
029100                                                                          
029200     PERFORM A-INIT                                                       
029300     PERFORM S01-LAS-W91042                                               
029400     PERFORM S02-LAS-REG-IN                                               
029500     PERFORM S03-LAS-W33548                                               
029600     PERFORM UNTIL W91042-EOF = JA                                        
029700       PERFORM B-SOLLA-POSTER                                             
029800       IF BEARBETNING = JA                                                
029900         IF W91042-IDARTNR = REG-IN-ART-IDARTNR                           
030000           MOVE NEJ TO ART-ANDRAD                                         
030100           PERFORM D-JAMFOR-FILER                                         
030200           IF ART-ANDRAD = JA                                             
030300             IF W91042-KDERS NOT = +52                                    
030400               PERFORM C-FLYTTA-FAELT                                     
030500               PERFORM S10-SKRIV-REG-UT                                   
030600               PERFORM S11-SKRIV-INFO-401                                 
030700             END-IF                                                       
030800             IF SKRIV-ERS = JA                                            
030900               PERFORM S14-SKRIV-ERSAETTNING                              
031000               MOVE NEJ TO SKRIV-ERS                                      
031100             END-IF                                                       
031200             PERFORM S12-SKRIV-TEXT-402                                   
031300             PERFORM S13-SKRIV-BEN-403                                    
031400             PERFORM S01-LAS-W91042                                       
031500             PERFORM S02-LAS-REG-IN                                       
031600           ELSE                                                           
031700             IF W91042-KDERS NOT = +52                                    
031800               PERFORM C-FLYTTA-FAELT                                     
031900               PERFORM S10-SKRIV-REG-UT                                   
032000             END-IF                                                       
032100             PERFORM S01-LAS-W91042                                       
032200             PERFORM S02-LAS-REG-IN                                       
032300           END-IF                                                         
032400         ELSE                                                             
032500           EVALUATE TRUE                                                  
032600           WHEN W91042-IDARTNR < REG-IN-ART-IDARTNR                       
032700                                                                          
032800             IF  W91042-PRARTSJK  > ZERO                                  
032900                 AND W91042-PRARTSTD  > ZERO                              
033000                 AND W91042-KDERS NOT = +52                               
033100                                                                          
033200               PERFORM C-FLYTTA-FAELT                                     
033300               PERFORM S10-SKRIV-REG-UT                                   
033400               PERFORM S11-SKRIV-INFO-401                                 
033500               PERFORM S12-SKRIV-TEXT-402                                 
033600               PERFORM S13-SKRIV-BEN-403                                  
033700             END-IF                                                       
033800             PERFORM S01-LAS-W91042                                       
033900           WHEN W91042-IDARTNR > REG-IN-ART-IDARTNR                       
034000             PERFORM S02-LAS-REG-IN                                       
034100           END-EVALUATE                                                   
034200         END-IF                                                           
034300       ELSE                                                               
034400         PERFORM S01-LAS-W91042                                           
034500       END-IF                                                             
034600     END-PERFORM                                                          
034700     PERFORM Z-FINIT                                                      
034800     MOVE ZERO TO RETURN-CODE                                             
034900     GOBACK                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 A-INIT SECTION.                                                          
035300     SKIP2                                                                
035400     OPEN INPUT  REG-IN                                                   
035500                 W91042                                                   
035600                 W33548                                                   
035700     OPEN OUTPUT REG-UT                                                   
035800                 W33539                                                   
035900                 W33549                                                   
036000     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
036100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036200     MOVE '401'                TO INFO1-IDPTYP                            
036300     MOVE 'A'                  TO INFO1-IDVTYP                            
036400     MOVE '402'                TO INFO2-IDPTYP                            
036500     MOVE 'A'                  TO INFO2-IDVTYP                            
036600     MOVE '403'                TO INFO3-IDPTYP                            
036700     MOVE 'A'                  TO INFO3-IDVTYP                            
036800     .                                                                    
036900     EJECT                                                                
037000 B-SOLLA-POSTER SECTION.                                                  
037100     SKIP2                                                                
037200******************************************************************        
037300*                                                                         
037400*    SELEKTERAR BORT FÖLJANDE POSTER:                                     
037500*    -OM MER ÄN 20 VECKOR TILL FÖRSTA INLEVERANS                          
037600*    -OM KDPRODSL = 0, 19, 29,71,72,73, ELLER 74                          
037700*    -OM IDFKNGRP = 0                                                     
037800*    -OM TIERSDAT ÄLDRE ÄN 10 ÅR                                          
037900*                                                                         
038000******************************************************************        
038100     SKIP2                                                                
038200     MOVE D-AAR             TO WS-AKT-AA                                  
038300     MOVE D-VECKA           TO WS-AKT-VV                                  
038400     MOVE WS-AKT-AAVV       TO WS-AKT-AAVV-NUM                            
038500     MOVE +30               TO WS-PLUS-VECKOR                             
038600     CALL W009VADD USING WS-AKT-AAVV-NUM WS-PLUS-VECKOR                   
038700     MOVE W91042-TIFINLV    TO WS-TIFINLV                                 
038800     MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                                  
038900     MOVE WS-AKT-AAVV-NUM   TO TMP2-YYWW                                  
039000     PERFORM WY2000P3                                                     
039100     IF TMP1-YYWW > TMP2-YYWW                                             
039200       MOVE NEJ             TO BEARBETNING                                
039300     ELSE                                                                 
039400       MOVE JA              TO BEARBETNING                                
039500     END-IF                                                               
039600*                                                                         
039700     IF W91042-TIERSDAT = 0                                               
039800       MOVE 9999999         TO WS-TIERSDAT                                
039900     ELSE                                                                 
040000       IF W91042-TIERSDAT > 50000                                         
040100         MOVE 19            TO WS-TIERSDAT-SEKEL                          
040200       ELSE                                                               
040300         MOVE 20            TO WS-TIERSDAT-SEKEL                          
040400       END-IF                                                             
040500       MOVE W91042-TIERSDAT TO WS-TIERSDAT-AAVVD                          
040600       MOVE 20              TO  SS-WEEK                                   
040700       MOVE D-AAR           TO  AA-WEEK                                   
040800       MOVE D-VECKA         TO  VV-WEEK                                   
040900       MOVE D-DAGNR         TO  D-WEEK                                    
041000       COMPUTE DATUM-10AR = AAAAVVD-WEEK - 10000                          
041100     END-IF                                                               
041200     IF BEARBETNING = JA                                                  
041300       MOVE W91042-KDPRODSL      TO TEST-KDPRODSL                         
041400       IF W91042-IDFKNGRP = 0                                             
041500       OR W91042-KDERS-UTG > 0                                            
041600       OR W91042-KDPRODSL = 0                                             
041700       OR KDPRODSL-VOLVO-EMB                                              
041800       OR KDPRODSL-BIMA                                                   
041900*  ÖKAT FRÅN 4 TILL 8 ÅR  TILL 10 ÅR**''                                  
042000       OR WS-TIERSDAT < DATUM-10AR                                        
042100         MOVE NEJ TO BEARBETNING                                          
042200       END-IF                                                             
042300     END-IF                                                               
042400*                                                                         
042500     .                                                                    
042600     EJECT                                                                
042700 C-FLYTTA-FAELT SECTION.                                                  
042800     SKIP2                                                                
042900* POSTTYP OCH VERTYP UPPDATERAS I A-INIT.                                 
043000*     '401'  INFO1-IDPTYP                                                 
043100     PERFORM CA-FIND-REPL-PART-CHANGE                                     
043200     MOVE W91042-IDARTNR         TO INFO1-IDARTNR                         
043300                                    REG-UT-ART-IDARTNR                    
043400     MOVE W91042-IDFKNGRP        TO INFO1-IDFKNGRP                        
043500                                    REG-UT-ART-IDFKNGRP                   
043600     MOVE W91042-KDSRA           TO INFO1-KDSRA                           
043700                                    REG-UT-ART-KDSRA                      
043800     MOVE W91042-KVQPACK-0       TO INFO1-KVQPACK-0                       
043900                                    REG-UT-ART-KVQPACK-0                  
044000**** CALL W400ARTUR                                                       
044100     MOVE W91042-KDARTURS        TO ARTU-KDARTURS                         
044200     MOVE 0                      TO ARTU-IDDISTR                          
044300     MOVE SPACE                  TO ARTU-IDDC                             
044400     PERFORM S031-CALL-W400ARTU                                           
044500     MOVE ARTU-KDARTURS-NUM      TO INFO1-KDARTURS-NUM                    
044600     MOVE W91042-KDARTURS        TO REG-UT-ART-KDARTURS                   
044700     MOVE W91042-KDPRODSL        TO INFO1-KDPRODSL                        
044800                                    REG-UT-ART-KDPRODSL                   
044900     MOVE W91042-VLARTNTO        TO INFO1-VLARTNTO                        
045000                                    REG-UT-ART-VLARTNTO                   
045100     MOVE W91042-VKART           TO INFO1-VKART                           
045200                                    REG-UT-ART-VKART                      
045300     MOVE W91042-IDSTATNR(3)     TO INFO1-IDSTATNR                        
045400                                    REG-UT-ART-IDSTATNR                   
045500     MOVE W91042-KDVSOP          TO INFO1-KDVSOP                          
045600                                    REG-UT-ART-KDVSOP                     
045700     MOVE W91042-KDSORT          TO INFO1-KDSORT                          
045800                                    REG-UT-ART-KDSORT                     
045900     MOVE W91042-KDERS           TO INFO1-KDERS                           
046000                                    REG-UT-ART-KDERS                      
046100                                                                          
046200     MOVE W91042-TIERSDAT        TO REG-UT-ART-TIERSDAT                   
046300                                                                          
046400     MOVE W91042-KDBPSR          TO INFO1-KDBPSR                          
046500                                    REG-UT-ART-KDBPSR                     
046600     MOVE ZERO                   TO INFO1-KDBBCL                          
046700                                    REG-UT-ART-KDBBCL                     
046800     MOVE W91042-IDLEVNR         TO INFO1-IDLEVNR                         
046900                                    REG-UT-ART-IDLEVNR                    
047000     MOVE W91042-PRARTSJK        TO INFO1-PRARTSJK                        
047100                                    REG-UT-ART-PRARTSJK                   
047200     MOVE W91042-PRARTSTD        TO INFO1-PRARTSTD                        
047300                                    REG-UT-ART-PRARTSTD                   
047400     MOVE W91042-FLIART          TO INFO1-FLIART                          
047500                                    REG-UT-ART-FLIART                     
047600     MOVE W91042-IDPROJ          TO INFO1-IDPROJ                          
047700                                    REG-UT-ART-IDPROJ                     
047800     MOVE W91042-IDAO(1)         TO INFO1-IDAO(1)                         
047900                                    REG-UT-ART-IDAO(1)                    
048000     MOVE W91042-IDAO(2)         TO INFO1-IDAO(2)                         
048100                                    REG-UT-ART-IDAO(2)                    
048200     MOVE W91042-KDPSLLOC        TO INFO1-KDPSLLOC                        
048300                                    REG-UT-ART-KDPSLLOC                   
048400                                                                          
048500*  RÄKNAR UT DATUM ÅÅVVD TILL ÅÅMMDD                                      
048600*  TIFINLV   ÅÅVVD   OBS                                                  
048700*  TIFINLEV  ÅÅMMDD  OBS                                                  
048800       MOVE W91042-TIFINLV   TO DAT-I-TIDATUM                             
048900       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
049000       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
049100                           DAT-O-TIDATUM DAT-KDSVAR                       
049200     IF DAT-KDSVAR-FEL                                                    
049300       MOVE ZERO                   TO INFO1-TIFINLEV                      
049400     ELSE                                                                 
049500       MOVE DAT-TIAAMMDD           TO INFO1-TIFINLEV                      
049600     END-IF                                                               
049700*                                                                         
049800     MOVE W91042-TIFINLV         TO REG-UT-ART-TIFINLV                    
049900     MOVE W91042-IDANSK          TO INFO1-IDANSK                          
050000                                    REG-UT-ART-IDANSK                     
050100     COMPUTE INFO1-KVPB = W91042-KVPB-SEP(1) +                            
050200                          W91042-KVPB-SEP(2) +                            
050300                          W91042-KVPB-SEP(3) +                            
050400                          W91042-KVPB-SEP(4) +                            
050500                          W91042-KVPB-SEP(5) +                            
050600                          W91042-KVPB-SEP(6)                              
050700                                                                          
050800     MOVE W91042-KDVVKL          TO INFO1-KDVVKL                          
050900                                    REG-UT-ART-KDVVKL                     
051000     MOVE SPACE                  TO INFO1-BELEVART                        
051100                                                                          
051200     MOVE W91042-KDTIPPR         TO INFO1-KDTIPPR                         
051300                                    REG-UT-ART-KDTIPPR                    
051400     MOVE W91042-IDINK           TO INFO1-IDINK                           
051500                                    REG-UT-ART-IDINK                      
051600     MOVE W91042-TIREGDAT        TO INFO1-TIREGDAT                        
051700     MOVE W91042-KDUART          TO INFO1-KDUART                          
051800                                    REG-UT-ART-KDUART                     
051900     MOVE W91042-IDARTNR-MOTSV   TO INFO1-IDARTNR-MOTSV                   
052000                                    REG-UT-ART-IDARTNR-MOTSV              
052100     MOVE W91042-PRINK           TO INFO1-PRINK                           
052200                                    REG-UT-ART-PRINK                      
052300     MOVE W91042-IDRITN          TO INFO1-IDRITN                          
052400                                    REG-UT-ART-IDRITN                     
052500                                                                          
052600     MOVE ZERO                   TO WS-PRHANTK                            
052700     COMPUTE WS-PRHANTK = W91042-PRDIRLON +                               
052800                          W91042-PRDMTRL  + W91042-PROVRPAL               
052900     MOVE WS-PRHANTK             TO INFO1-PRHANTK                         
053000                                    REG-UT-ART-PRHANTK                    
053100                                                                          
053200     MOVE W91042-KDAGE           TO INFO1-KDAGE                           
053300                                    REG-UT-ART-KDAGE                      
053400                                                                          
053500     MOVE W91042-IDKAT(1)        TO INFO1-IDKAT(1)                        
053600                                    REG-UT-ART-IDKAT(1)                   
053700     MOVE W91042-IDKAT(2)        TO INFO1-IDKAT(2)                        
053800                                    REG-UT-ART-IDKAT(2)                   
053900     MOVE W91042-IDKAT(3)        TO INFO1-IDKAT(3)                        
054000                                    REG-UT-ART-IDKAT(3)                   
054100     MOVE SPACE                  TO INFO1-KDRAB                           
054200     MOVE ZERO                   TO INFO1-PRARTBEL                        
054300     MOVE W91042-FLLSRDEL        TO INFO1-FLLSRDEL                        
054400                                    REG-UT-ART-FLLSRDEL                   
054500     MOVE W91042-IDPROJUP        TO INFO1-IDPROJUP                        
054600                                    REG-UT-ART-IDPROJUP                   
054700     MOVE W91042-FLGEMFMC        TO INFO1-FLGEMFMC                        
054800                                    REG-UT-ART-FLGEMFMC                   
054900     MOVE W91042-TIURPROD        TO INFO1-TIURPROD                        
055000                                    REG-UT-ART-TIURPROD                   
055100     MOVE WS-WDD701-KVKORT       TO REG-UT-ART-KVKORT                     
055200                                                                          
055300                                                                          
055400*     '402'  INFO2-IDPTYP                                                 
055500     MOVE W91042-TEORSAK         TO INFO2-TEORSAK                         
055600                                  REG-UT-ART-TEORSAK                      
055700     MOVE W91042-TEARTNOT-3      TO INFO2-TEARTNOT-3                      
055800                                  REG-UT-ART-TEARTNOT-3                   
055900     MOVE W91042-TEARTNOT-7      TO INFO2-TEARTNOT-7                      
056000                                  REG-UT-ART-TEARTNOT-7                   
056100                                                                          
056200*     '403'  INFO3-IDPTYP                                                 
056300     MOVE W91042-BEART(04)     TO INFO3-BEART(1)                          
056400                                  REG-UT-ART-BEART(1)                     
056500     MOVE W91042-BEART(08)     TO INFO3-BEART(2)                          
056600                                  REG-UT-ART-BEART(2)                     
056700     MOVE W91042-BEART(10)     TO INFO3-BEART(3)                          
056800                                  REG-UT-ART-BEART(3)                     
056900     .                                                                    
057000                                                                          
057100     EJECT                                                                
057200                                                                          
057300 CA-FIND-REPL-PART-CHANGE SECTION.                                        
057400                                                                          
057500*****READ WDD7 TO FIND IF THERE IS A CHANGE IN REPALCEMENT PART           
057600*****FOR THE SUPERSESSION PART                                            
057700     MOVE W91042-IDARTNR  TO W-IDARTNR                                    
057800     PERFORM IMS-GU-WDD701                                                
057900     IF SEGMENT-FINNS                                                     
058000       MOVE WDD701-KVKORT TO WS-WDD701-KVKORT                             
058100     ELSE                                                                 
058200       MOVE ZERO          TO WS-WDD701-KVKORT                             
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 D-JAMFOR-FILER SECTION.                                                  
058800     SKIP2                                                                
058900******************************************************************        
059000*                                                                         
059100*    JÄMFÖR REG-IN OCH W91042-POSTER MED SAMMA ARTIKELNUMMER              
059200*    OM POSTERNA ÄR OLIKA SKRIVS POSTEN PÅ BÅDE W33538 OCH REG-UT         
059300*    ANNARS BARA PÅ REG-UT                                                
059400*                                                                         
059500******************************************************************        
059600*                                                                         
059700     MOVE ZERO TO WS-PRHANTK                                              
059800     COMPUTE WS-PRHANTK   =   W91042-PRDIRLON +                           
059900               W91042-PRDMTRL + W91042-PROVRPAL                           
060000                                                                          
060100     IF W91042-IDFKNGRP          NOT = REG-IN-ART-IDFKNGRP                
060200     OR W91042-KDPRODSL          NOT = REG-IN-ART-KDPRODSL                
060300     OR W91042-KDPSLLOC          NOT = REG-IN-ART-KDPSLLOC                
060400     OR W91042-KDSRA             NOT = REG-IN-ART-KDSRA                   
060500     OR W91042-KVQPACK-0         NOT = REG-IN-ART-KVQPACK-0               
060600     OR W91042-KDARTURS          NOT = REG-IN-ART-KDARTURS                
060700     OR W91042-VLARTNTO          NOT = REG-IN-ART-VLARTNTO                
060800     OR W91042-VKART             NOT = REG-IN-ART-VKART                   
060900     OR W91042-KDVSOP            NOT = REG-IN-ART-KDVSOP                  
061000     OR W91042-KDBPSR            NOT = REG-IN-ART-KDBPSR                  
061100     OR W91042-IDLEVNR           NOT = REG-IN-ART-IDLEVNR                 
061200     OR W91042-IDSTATNR(3)       NOT = REG-IN-ART-IDSTATNR                
061300     OR W91042-KDVSOP            NOT = REG-IN-ART-KDVSOP                  
061400     OR W91042-KDSORT            NOT = REG-IN-ART-KDSORT                  
061500     OR W91042-KDERS             NOT = REG-IN-ART-KDERS                   
061600     OR W91042-PRARTSJK          NOT = REG-IN-ART-PRARTSJK                
061700     OR W91042-PRARTSTD          NOT = REG-IN-ART-PRARTSTD                
061800     OR W91042-FLIART            NOT = REG-IN-ART-FLIART                  
061900     OR W91042-IDPROJ            NOT = REG-IN-ART-IDPROJ                  
062000     OR W91042-IDAO(1)           NOT = REG-IN-ART-IDAO(1)                 
062100     OR W91042-IDAO(2)           NOT = REG-IN-ART-IDAO(2)                 
062200     OR W91042-TIFINLV           NOT = REG-IN-ART-TIFINLV                 
062300     OR W91042-IDANSK            NOT = REG-IN-ART-IDANSK                  
062400     OR W91042-KDVVKL            NOT = REG-IN-ART-KDVVKL                  
062500     OR W91042-KDTIPPR           NOT = REG-IN-ART-KDTIPPR                 
062600     OR W91042-IDINK             NOT = REG-IN-ART-IDINK                   
062700     OR W91042-KDUART            NOT = REG-IN-ART-KDUART                  
062800     OR W91042-IDARTNR-MOTSV     NOT = REG-IN-ART-IDARTNR-MOTSV           
062900     OR W91042-PRINK             NOT = REG-IN-ART-PRINK                   
063000     OR W91042-IDRITN            NOT = REG-IN-ART-IDRITN                  
063100     OR WS-PRHANTK               NOT = REG-IN-ART-PRHANTK                 
063200     OR W91042-BEART(4)          NOT = REG-IN-ART-BEART(1)                
063300     OR W91042-BEART(8)          NOT = REG-IN-ART-BEART(2)                
063400     OR W91042-BEART(10)         NOT = REG-IN-ART-BEART(3)                
063500     OR W91042-KDAGE             NOT = REG-IN-ART-KDAGE                   
063600     OR W91042-IDKAT(1)          NOT = REG-IN-ART-IDKAT(1)                
063700     OR W91042-IDKAT(2)          NOT = REG-IN-ART-IDKAT(2)                
063800     OR W91042-IDKAT(3)          NOT = REG-IN-ART-IDKAT(3)                
063900     OR W91042-FLLSRDEL          NOT = REG-IN-ART-FLLSRDEL                
064000     OR W91042-IDPROJUP          NOT = REG-IN-ART-IDPROJUP                
064100     OR W91042-FLGEMFMC          NOT = REG-IN-ART-FLGEMFMC                
064200     OR W91042-TIURPROD          NOT = REG-IN-ART-TIURPROD                
064300       IF  W91042-PRARTSJK  > ZERO AND                                    
064400           W91042-PRARTSTD  > ZERO                                        
064500         MOVE JA TO ART-ANDRAD                                            
064600       END-IF                                                             
064700     END-IF                                                               
064800     IF W91042-KDERS            = REG-IN-ART-KDERS                        
064900     AND W91042-TIERSDAT        = REG-IN-ART-TIERSDAT                     
065000**** FIND IF REPLACEMENT PART FOR THE SUPESEDED PART IS ADDED.            
065100**** IF THERE IS A ADDITION IN REPLACEMENT PART, SEND THE PART NO         
065200**** TO BE SENT IN DAILY FILE EVEN IF THERE IS NO CHANGE IN               
065300**** KDERS                                                                
065400       PERFORM CA-FIND-REPL-PART-CHANGE                                   
065500       IF WS-WDD701-KVKORT > REG-IN-ART-KVKORT                            
065600         MOVE JA TO SKRIV-ERS                                             
065700         MOVE W91042-IDARTNR   TO ERS-IDARTNR                             
065800         MOVE W91042-KDERS     TO ERS-KDERS-NEW                           
065900         MOVE REG-IN-ART-KDERS TO ERS-KDERS-OLD                           
066000         MOVE W91042-TIERSDAT  TO ERS-TIERSDAT                            
066100       ELSE                                                               
066200         CONTINUE                                                         
066300       END-IF                                                             
066400     ELSE                                                                 
066500       MOVE JA TO SKRIV-ERS                                               
066600       MOVE W91042-IDARTNR     TO ERS-IDARTNR                             
066700       MOVE W91042-KDERS       TO ERS-KDERS-NEW                           
066800       MOVE REG-IN-ART-KDERS   TO ERS-KDERS-OLD                           
066900       MOVE W91042-TIERSDAT    TO ERS-TIERSDAT                            
067000     END-IF                                                               
067100                                                                          
067200     PERFORM UNTIL W33548-EOF = JA                                        
067300                   OR W33548-IDARTNR > W91042-IDARTNR                     
067400          IF W33548-IDARTNR = W91042-IDARTNR                              
067500             IF W33548-FLAMCDIF = 'N'                                     
067600************************************************                          
067700*** FÖRÄNDRAD ARTIKEL PÅ NDC 41,42,43 OCH 51 ***                          
067800************************************************                          
067900                IF  W91042-PRARTSJK  > ZERO AND                           
068000                    W91042-PRARTSTD  > ZERO                               
068100                    MOVE JA TO ART-ANDRAD                                 
068200                END-IF                                                    
068300                PERFORM S03-LAS-W33548                                    
068400             ELSE                                                         
068500*****************************************                                 
068600*** NY ARTIKEL PÅ NDC 41,42,43 OCH 51 ***                                 
068700*              OM W33548-FLAMCDIF = 'J' *                                 
068800*****************************************                                 
068900                  IF  W91042-PRARTSJK  > ZERO AND                         
069000                      W91042-PRARTSTD  > ZERO                             
069100                      MOVE JA TO ART-ANDRAD                               
069200                  END-IF                                                  
069300                  PERFORM S03-LAS-W33548                                  
069400             END-IF                                                       
069500          ELSE                                                            
069600             IF W33548-IDARTNR < W91042-IDARTNR                           
069700                PERFORM S03-LAS-W33548                                    
069800             END-IF                                                       
069900          END-IF                                                          
070000     END-PERFORM                                                          
070100                                                                          
070200     .                                                                    
070300     EJECT                                                                
070400 S01-LAS-W91042 SECTION.                                                  
070500     SKIP2                                                                
070600     READ W91042 INTO W91042-AREA                                         
070700                      AT END MOVE JA TO W91042-EOF                        
070800                      MOVE 999999999 TO W91042-IDARTNR                    
070900     END-READ                                                             
071000     IF W91042-EOF = NEJ                                                  
071100       MOVE 'W91042'             TO POSTSUM-FDNAMN                        
071200       MOVE 'W33538D1'           TO POSTSUM-DDNAMN2                       
071300       MOVE ZERO                 TO POSTSUM-TRANSTYP                      
071400       CALL POSTSUM   USING POSTSUM-PARM                                  
071500     END-IF                                                               
071600     .                                                                    
071700     EJECT                                                                
071800 S02-LAS-REG-IN SECTION.                                                  
071900     SKIP2                                                                
072000     READ REG-IN INTO REG-IN-AREA                                         
072100                      AT END MOVE JA TO REG-IN-EOF                        
072200                      MOVE 999999999 TO REG-IN-ART-IDARTNR                
072300     END-READ                                                             
072400     IF REG-IN-EOF = NEJ                                                  
072500       MOVE 'REG-IN'            TO POSTSUM-FDNAMN                         
072600       MOVE 'W33538D2'          TO POSTSUM-DDNAMN2                        
072700       MOVE 'REGIN'             TO POSTSUM-TRANSTYP                       
072800       CALL POSTSUM   USING POSTSUM-PARM                                  
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 S03-LAS-W33548 SECTION.                                                  
073300     SKIP2                                                                
073400     READ W33548 INTO W33548-AREA                                         
073500                      AT END MOVE JA TO W33548-EOF                        
073600                      MOVE 999999999 TO W33548-IDARTNR                    
073700     END-READ                                                             
073800     IF W33548-EOF = NEJ                                                  
073900       MOVE 'W33548'             TO POSTSUM-FDNAMN                        
074000       MOVE 'W33538D6'           TO POSTSUM-DDNAMN2                       
074100       MOVE ZERO                 TO POSTSUM-TRANSTYP                      
074200       CALL POSTSUM   USING POSTSUM-PARM                                  
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 S10-SKRIV-REG-UT SECTION.                                                
074700     SKIP2                                                                
074800     WRITE REG-UT-POST FROM REG-UT-AREA                                   
074900     MOVE 'REG-UT'               TO POSTSUM-FDNAMN                        
075000     MOVE 'W33538D3'             TO POSTSUM-DDNAMN2                       
075100     MOVE 'REGUT'                TO POSTSUM-TRANSTYP                      
075200     CALL POSTSUM      USING POSTSUM-PARM                                 
075300     .                                                                    
075400     EJECT                                                                
075500 S11-SKRIV-INFO-401      SECTION.                                         
075600     SKIP2                                                                
075700*                                                                         
075800     WRITE INFO-POST1 FROM ART-INFO-AREA1                                 
075900     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
076000     MOVE 'W33538D4'             TO POSTSUM-DDNAMN2                       
076100     MOVE '401'                  TO POSTSUM-TRANSTYP                      
076200     CALL POSTSUM      USING POSTSUM-PARM                                 
076300     .                                                                    
076400                                                                          
076500 S12-SKRIV-TEXT-402      SECTION.                                         
076600     SKIP2                                                                
076700*                                                                         
076800     WRITE INFO-POST2 FROM ART-INFO-AREA2                                 
076900     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
077000     MOVE 'W33538D4'             TO POSTSUM-DDNAMN2                       
077100     MOVE '402'                  TO POSTSUM-TRANSTYP                      
077200     CALL POSTSUM      USING POSTSUM-PARM                                 
077300     .                                                                    
077400 S13-SKRIV-BEN-403       SECTION.                                         
077500     SKIP2                                                                
077600*                                                                         
077700     WRITE INFO-POST3 FROM ART-INFO-AREA3                                 
077800     MOVE 'W33539'               TO POSTSUM-FDNAMN                        
077900     MOVE 'W33538D4'             TO POSTSUM-DDNAMN2                       
078000     MOVE '403'                  TO POSTSUM-TRANSTYP                      
078100     CALL POSTSUM      USING POSTSUM-PARM                                 
078200     .                                                                    
078300     EJECT                                                                
078400 S14-SKRIV-ERSAETTNING SECTION.                                           
078500     SKIP2                                                                
078600*                                                                         
078700     WRITE ERS-POST FROM ERS-UT-AREA                                      
078800     MOVE 'W33549'               TO POSTSUM-FDNAMN                        
078900     MOVE 'W33538D5'             TO POSTSUM-DDNAMN2                       
079000     MOVE 'ERS'                  TO POSTSUM-TRANSTYP                      
079100     CALL POSTSUM      USING POSTSUM-PARM                                 
079200     .                                                                    
079300     EJECT                                                                
079400 S031-CALL-W400ARTU  SECTION.                                             
079500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
079600* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
079700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
079800     SKIP2                                                                
079900                                                                          
080000                                                                          
080100     CALL W400ARTU USING ARTU-W400ARTU                                    
080200                                                                          
080300     .                                                                    
080400     EJECT                                                                
080500 Z-FINIT SECTION.                                                         
080600     SKIP2                                                                
080700     CLOSE REG-IN                                                         
080800           W91042                                                         
080900           W33548                                                         
081000           REG-UT                                                         
081100           W33539                                                         
081200           W33549                                                         
081300     SKIP2                                                                
081400     MOVE 'S' TO POSTSUM-OPKOD                                            
081500     CALL POSTSUM USING POSTSUM-PARM                                      
081600     .                                                                    
081700     EJECT                                                                
081800*    -COPY WY2000P2                                                       
081900     EJECT                                                                
082000*    -COPY WY2000P3                                                       
082100 IMS-GU-WDD701 SECTION.                                                   
082200                                                                          
082300     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
082400          DELIMITED BY SIZE INTO SSA1                                     
082500     MOVE '  GE' TO GODK-STATUSKODER                                      
082600     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
082700     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
083000     SKIP2                                                                
083100 IMS-STATUSKONTROLL SECTION.                                              
083200     SKIP2                                                                
083300     SET STATUS-IX TO 1                                                   
083400     SEARCH GODK-STATUS                                                   
083500       AT END                                                             
083600         STRING 'FELAKTIG STATUSKOD FRÅN IMS : ' STATUS-WS                
083700         DELIMITED BY SIZE INTO FELTEXT                                   
083800         CALL FELLOG                                                      
083900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
084000         CONTINUE                                                         
084100     END-SEARCH                                                           
084200     .                                                                    
