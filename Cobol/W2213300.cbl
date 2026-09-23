000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2213300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/09/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE OUTPUT FILE - CALL OFF INFORMATION TO TMS                 
000900*                                                                         
001000*        THE PROGRAM READS     WDD9                                       
001100*                              WDK6                                       
001110*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INPUT FILE FROM W221P060                                   
002500     SELECT W22160                     ASSIGN TO W22133D1.                
002600     SKIP2                                                                
002700*          --- FILE TO TMS                                                
002800     SELECT W22133                     ASSIGN TO W22133D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W22160                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W22160      -PRE  IN-  -L.                                     
003900     SKIP3                                                                
004000 FD  W22133                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  OUT-W2213301 -COPY W2213301      -L.                                 
004310*01  OUT-W2213302 -COPY W2213302      -L.                                 
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2213300'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005001 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005002 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005005 77  TRAFF                       PIC X       VALUE 'N'.                   
005006 77  W-WRITE-W22133-HEAD         PIC X       VALUE 'N'.                   
005007                                                                          
005008 01  W-DAAVROP-LEVDAG            PIC 9(07).                               
005009 01  FILLER REDEFINES W-DAAVROP-LEVDAG.                                   
005010     03 W-DAAVROP-LEVDAG-CC      PIC 9(02).                               
005011     03 W-DAAVROP-LEVDAG-AAVVD   PIC 9(05).                               
005012                                                                          
005013 01  W-DAAVROP-AVS.                                                       
005014     03 FILLER                   PIC X(02).                               
005015     03 W-DAAVROP-AVS-AAVVD      PIC 9(05).                               
005016     03 FILLER REDEFINES W-DAAVROP-AVS-AAVVD.                             
005018        05 W-DAAVROP-AVS-AAVV    PIC 9(04).                               
005019        05 W-TILEVDAG-AVS        PIC 9(01).                               
005020                                                                          
005021 01  W-DEAVROP-AVS-TOT.                                                   
005022     03 W-DEAVROP-AVS.                                                    
005023        05 W-DEAVROP-AVS-CC      PIC 9(02) VALUE 20.                      
005024        05 W-DEAVROP-AVS-AAMMDD  PIC 9(06).                               
005025        05 FILLER REDEFINES W-DEAVROP-AVS-AAMMDD.                         
005026           07 W-DEAVROP-AVS-AA   PIC 9(02).                               
005027           07 W-DEAVROP-AVS-MM   PIC 9(02).                               
005028           07 W-DEAVROP-AVS-DD   PIC 9(02).                               
005029     03 W-DEAVROP-AVS-TIME       PIC 9(04).                               
005030                                                                          
005031 01  W-TIAVRDAT-INL-TOT.                                                  
005032     03 W-TIAVRDAT-INL.                                                   
005033        05 W-TIAVRDAT-CC         PIC 9(02) VALUE 20.                      
005034        05 W-TIAVRDAT-INL-AAMMDD PIC 9(06).                               
005040     03 W-TIAVRDAT-INL-TIME      PIC 9(04).                               
005100                                                                          
005110 01  W-HEAD-DAGILTIG-FOM.                                                 
005130     03 W-DAGLIG-FOM-CC            PIC 9(02) VALUE 20.                    
005140     03 W-HEAD-DAGILTIG-FOM-AAMMDD PIC 9(06).                             
005150                                                                          
005200 77  W22160-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W22160                       VALUE 'J'.                   
005400     EJECT                                                                
005410*01  -COPY WWDCKONS                                                       
005420     EJECT                                                                
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006210     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
006211     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006220     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETRAR TILL WDATKONV                                         
006900 01  FILLER                  PIC X(16)  VALUE 'WDATAREA********'.         
006901                                                                          
006902 01  -COPY WDATAREA                                                       
006903     EJECT                                                                
006904*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
006905 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
006906*01 -COPY WZ20DAYS                                                        
006907     EJECT                                                                
006908*01  -COPY W009CIA                                                        
006909     EJECT                                                                
006910*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006920                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                 'IN-AREA-START  '.                       
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W22160     -PRE IN-                                       
008700     EJECT                                                                
008800 01  OUT-AREA-START              PIC X(24)   VALUE                        
008900                                 'OUT-AREA-START  '.                      
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W2213301   -PRE OUT-HEAD-                                 
009210*01  AREA -COPY W2213302   -PRE OUT-ROW-                                  
009300     EJECT                                                                
009400*    --- AREAS FOR IMS-SECTIONS                                           
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  KEYS-FOR-DLI.                                                        
010000     03  W-IDARTNR-X.                                                     
010100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010200     03  W-IDLEVNR-X.                                                     
010300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
010310     03  W-KDAVROP-X.                                                     
010320         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
010400     03  W-WDD901KY-X.                                                    
010500         05  W-IDARTNR-WDD9      PIC S9(9)   VALUE ZERO COMP-3.           
010600         05  W-IDDC-WDD9         PIC X(2)    VALUE SPACE.                 
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FOUND                       VALUE '  '.                  
011000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011200     SKIP2                                                                
011300 01  GOOD-STATUSCODES.                                                    
011400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNCTION CODES                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
012602 01  DLI-IO-WDD902.                                                       
012603*    03  -COPY WDD902                                                     
012604     EJECT                                                                
012605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
012606 01  DLI-IO-WDD905.                                                       
012607*    03  -COPY WDD905 -PRE WDD905-                                        
012608     EJECT                                                                
012610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012620 01  DLI-IO-WDK601.                                                       
012630*    03  -COPY WDK601                                                     
012640     EJECT                                                                
012650 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012660 01  DLI-IO-WDK611.                                                       
012670*    03  -COPY WDK611                                                     
012680     EJECT                                                                
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK623'.                      
012710 01  DLI-IO-WDK623.                                                       
012720*    03  -COPY WDK623                                                     
012730     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900                                                                          
013000                                                                          
013100*01  -COPY W0008  -PRE WDD9-                                              
013200     05  FILLER                  PIC X.                                   
013300     EJECT                                                                
013400*01  -COPY W0008  -PRE WDK6-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700 PROCEDURE DIVISION  USING WDD9-PCB WDK6-PCB.                             
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING WDD9-PCB WDK6-PCB.                             
014100                                                                          
014200     PERFORM A-INIT                                                       
014300                                                                          
014400     PERFORM S01-READ-W22160                                              
014500     PERFORM UNTIL END-OF-W22160                                          
014600                                                                          
014700       PERFORM B-CREATE-TMS-FILE                                          
014800                                                                          
014900       PERFORM S01-READ-W22160                                            
015000     END-PERFORM                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000                                                                          
016100     OPEN INPUT  W22160                                                   
016200                                                                          
016300     OPEN OUTPUT W22133                                                   
016400                                                                          
016500     ACCEPT TODAYS-DATE  FROM DATE                                        
016600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016700     .                                                                    
016800     EJECT                                                                
016900 B-CREATE-TMS-FILE SECTION.                                               
016910     MOVE 'B-CREATE-TMS-FILE'       TO CURRENT-SECTION                    
017000                                                                          
017010* HEAD                                                                    
017100     MOVE IN-IDARTNR                TO W-IDARTNR-WDD9                     
017300     MOVE WC-CDC-SE                 TO W-IDDC-WDD9                        
017500     MOVE IN-IDLEVNR                TO W-IDLEVNR                          
017600     PERFORM IMS-GU-WDD902                                                
017710     IF SEGMENT-FOUND                                                     
017800        INITIALIZE OUT-HEAD-AREA                                          
017900        MOVE 'IL_CO_HEAD'           TO OUT-HEAD-IDPTYP-015                
018000        MOVE '01'                   TO OUT-HEAD-IDVTYP                    
018100        MOVE 'PULS'                 TO OUT-HEAD-IDSYSTEM-SEND             
018200        MOVE 'BP2TW'                TO OUT-HEAD-IDINK                     
018300        MOVE IN-IDLEVNR             TO OUT-HEAD-IDLEVNR                   
018400        MOVE 'BATCH'                TO OUT-HEAD-IDDELTYP                  
018511        MOVE 'VO'                   TO CIA-IDARTPRE-IN                    
018512        MOVE IN-IDARTNR             TO CIA-IDARTBET-IN                    
018513        CALL W009CIA USING CIA-W009CIA                                    
018514        IF CIA-KDSVAR = 'F'                                               
018515           MOVE IN-IDARTNR          TO OUT-HEAD-IDARTNR-010               
018517        ELSE                                                              
018518           MOVE CIA-IDARTBET-UT     TO OUT-HEAD-IDARTNR-010               
018519        END-IF                                                            
018600        MOVE TODAYS-DATE            TO W-HEAD-DAGILTIG-FOM-AAMMDD         
018610        MOVE W-HEAD-DAGILTIG-FOM    TO OUT-HEAD-DAGILTIG-FOM              
018700        MOVE SPACE                  TO OUT-HEAD-DAGILTIG-TOM              
019410        MOVE IN-ADINPORT            TO OUT-HEAD-ADINPORT                  
019500                                                                          
019530        MOVE IN-IDLEVNR             TO OUT-HEAD-IDLEVNR-SHIP              
019531        MOVE IN-IDARTNR             TO W-IDARTNR                          
019540        PERFORM IMS-GU-WDK601                                             
019550        IF SEGMENT-FOUND                                                  
019600          PERFORM IMS-GNP-WDK611                                          
019601          IF SEGMENT-FOUND                                                
019603            IF IN-IDLEVNR = ART-IDLEVNR                                   
019604              MOVE CLAG-IDLEVNR-SHIP    TO OUT-HEAD-IDLEVNR-SHIP          
019606            ELSE                                                          
019610              PERFORM IMS-GNP-WDK623                                      
019700              MOVE NOO                  TO TRAFF                          
019800              PERFORM UNTIL SEGMENT-MISSING OR TRAFF = YES                
019900                IF AVT-IDLEVNR-AVT = IN-IDLEVNR                           
020000                  MOVE AVT-IDLEVNR-SHIP TO OUT-HEAD-IDLEVNR-SHIP          
020010                  MOVE YES              TO TRAFF                          
020300                END-IF                                                    
020400                PERFORM IMS-GNP-WDK623                                    
020500              END-PERFORM                                                 
020510            END-IF                                                        
020511          END-IF                                                          
020520        END-IF                                                            
020600                                                                          
020700        MOVE YES                     TO W-WRITE-W22133-HEAD               
020701                                                                          
020702* DEMAND                                                                  
020710        PERFORM IMS-GNP-WDD905                                            
020711        IF SEGMENT-MISSING                                                
020714           PERFORM S11-WRITE-W22133-HEAD                                  
020715        ELSE                                                              
020716           PERFORM UNTIL SEGMENT-MISSING                                  
020717             COMPUTE W-DAAVROP-LEVDAG =                                   
020718                    (10 * WDD905-DAAVROP-AVS) + WDD905-TILEVDAG           
020719                                                                          
020720             MOVE 'YYWWD'             TO DAYS-KDDATFMT1                   
020721             MOVE W-DAAVROP-LEVDAG-AAVVD TO DAYS-TIDATE1                  
020724                                                                          
020725             MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                   
020726             MOVE SPACE               TO DAYS-IDCALEND                    
020727                                         DAYS-TIDATE2                     
020728             MOVE ZERO                TO DAYS-KVDAYS                      
020729                                                                          
020730             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
020752             IF DAYS-KDRC = +0                                            
020753              IF DAYS-TIDATE2(1:6) = TODAYS-DATE                          
020754              OR DAYS-TIDATE2(1:6) > TODAYS-DATE                          
020756                IF W-WRITE-W22133-HEAD = YES                              
020757                   PERFORM S11-WRITE-W22133-HEAD                          
020758                   MOVE NOO              TO W-WRITE-W22133-HEAD           
020759                END-IF                                                    
020760                INITIALIZE OUT-ROW-AREA                                   
020761                MOVE 'IL_CO_DEMAND'      TO OUT-ROW-IDPTYP-015            
020762                MOVE '01'                TO OUT-ROW-IDVTYP                
020763                MOVE WDD905-KVAVROP      TO OUT-ROW-KVAVROP               
020764                                                                          
020765                MOVE 'YYWWD'             TO DAYS-KDDATFMT1                
020766                MOVE WDD905-DAAVROP-AVS  TO W-DAAVROP-AVS                 
020767                MOVE WDD905-TILEVDAG     TO W-TILEVDAG-AVS                
020768                MOVE W-DAAVROP-AVS-AAVVD TO DAYS-TIDATE1                  
020769                                                                          
020770                MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                
020771                MOVE SPACE               TO DAYS-IDCALEND                 
020772                                            DAYS-TIDATE2                  
020773                MOVE ZERO                TO DAYS-KVDAYS                   
020774                                                                          
020775                CALL WZ20DAYS USING DAYS-WZ20DAYS                         
020776                IF DAYS-KDRC = +0                                         
020777                  MOVE DAYS-TIDATE2(1:6) TO W-DEAVROP-AVS-AAMMDD          
020778                  MOVE '0000'            TO W-DEAVROP-AVS-TIME            
020779                  MOVE W-DEAVROP-AVS-TOT TO OUT-ROW-DAAVROP-STA           
020780                  MOVE '2359'            TO W-DEAVROP-AVS-TIME            
020781                  MOVE W-DEAVROP-AVS-TOT TO OUT-ROW-DAAVROP-STO           
020782                ELSE                                                      
020783                  MOVE ZERO              TO OUT-ROW-DAAVROP-STA           
020784                  MOVE ZERO              TO OUT-ROW-DAAVROP-STO           
020785                END-IF                                                    
020809                                                                          
020810                MOVE WDD905-TIAVRDAT-INL TO W-TIAVRDAT-INL-AAMMDD         
020811                MOVE '0000'              TO W-TIAVRDAT-INL-TIME           
020812                MOVE W-TIAVRDAT-INL-TOT  TO OUT-ROW-DASNDTID-STA          
020813                MOVE '2359'              TO W-TIAVRDAT-INL-TIME           
020814                MOVE W-TIAVRDAT-INL-TOT  TO OUT-ROW-DASNDTID-STO          
020815                                                                          
020816                PERFORM S11-WRITE-W22133-ROW                              
020817              END-IF                                                      
020818                                                                          
020819              PERFORM IMS-GNP-WDD905                                      
020820             ELSE                                                         
020822              MOVE 'WRONG RETURNCODE FROM WZ20DAYS'                       
020823                                       TO ERROR-TEXT-STR                  
020824              MOVE 32 TO RKOD-ABEND                                       
020826              PERFORM S99-ABEND                                           
020830             END-IF                                                       
020900           END-PERFORM                                                    
021010        END-IF                                                            
021020     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021700 Z-FINIT SECTION.                                                         
021710     MOVE 'Z-FINIT          '       TO CURRENT-SECTION                    
021800     CLOSE W22160                                                         
021900           W22133                                                         
022000     SKIP2                                                                
022100     MOVE 'S' TO POSTSUM-OPKOD                                            
022200     CALL POSTSUM USING POSTSUM-PARM                                      
022300     .                                                                    
022400     EJECT                                                                
022500 S01-READ-W22160  SECTION.                                                
022510     MOVE 'S01-READ-W22160  '       TO CURRENT-SECTION                    
022520                                                                          
022600     READ W22160 INTO IN-AREA                                             
022700     AT END                                                               
022800        MOVE HIGH-VALUE TO IN-AREA                                        
022900        SET END-OF-W22160 TO TRUE                                         
023000                                                                          
023100     NOT AT END                                                           
023200        MOVE 'W22160' TO POSTSUM-FDNAMN                                   
023300        MOVE 'W22133D1' TO POSTSUM-DDNAMN2                                
023400*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
023500        MOVE SPACE     TO POSTSUM-TRANSTYP                                
023600        CALL POSTSUM USING POSTSUM-PARM                                   
023700     END-READ                                                             
023800     .                                                                    
023900     EJECT                                                                
024000 S11-WRITE-W22133-HEAD SECTION.                                           
024010     MOVE 'S11-WRITE-W22133-HEAD '  TO CURRENT-SECTION                    
024100                                                                          
024200     WRITE OUT-W2213301 FROM OUT-HEAD-AREA                                
024300                                                                          
024400     MOVE 'HEAD'          TO POSTSUM-TRANSTYP                             
024500     MOVE 'W22133'        TO POSTSUM-FDNAMN                               
024600     MOVE 'W22133D2'      TO POSTSUM-DDNAMN2                              
024700     CALL POSTSUM      USING POSTSUM-PARM                                 
024800     .                                                                    
024801                                                                          
024810 S11-WRITE-W22133-ROW  SECTION.                                           
024811     MOVE 'S11-WRITE-W22133-ROW  '  TO CURRENT-SECTION                    
024820                                                                          
024830     WRITE OUT-W2213302 FROM OUT-ROW-AREA                                 
024840                                                                          
024850     MOVE 'DMAN'          TO POSTSUM-TRANSTYP                             
024860     MOVE 'W22133'        TO POSTSUM-FDNAMN                               
024870     MOVE 'W22133D2'      TO POSTSUM-DDNAMN2                              
024880     CALL POSTSUM      USING POSTSUM-PARM                                 
024890     .                                                                    
024900     EJECT                                                                
025000 S99-ABEND SECTION.                                                       
025100                                                                          
025200     SKIP2                                                                
025300     MOVE 'S' TO POSTSUM-OPKOD                                            
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     CALL ABEND USING RKOD-ABEND                                          
025600     .                                                                    
025700     EJECT                                                                
025800* --- IMS SECTIONS  ---                                                   
025900                                                                          
026000     EJECT                                                                
027100 IMS-GU-WDD902 SECTION.                                                   
027110     MOVE 'IMS-GU-WDD902   ' TO CURRENT-IMS-SECTION                       
027200                                                                          
027300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
027400            DELIMITED BY SIZE INTO SSA1                                   
027500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
027600            DELIMITED BY SIZE INTO SSA2                                   
027700     MOVE '  GE' TO GOOD-STATUSCODES                                      
027800     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
027900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
028000     PERFORM IMS-STATUSCHECK                                              
028100     .                                                                    
028200     EJECT                                                                
028210 IMS-GNP-WDD905 SECTION.                                                  
028211     MOVE 'IMS-GU-WDD905   ' TO CURRENT-IMS-SECTION                       
028220                                                                          
028230     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
028240                            DELIMITED BY SIZE INTO SSA1                   
028241     MOVE '  GE' TO GOOD-STATUSCODES                                      
028260     CALL CBLTDLI USING GNP   WDD9-PCB DLI-IO-WDD905 SSA1                 
028270     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
028280     PERFORM IMS-STATUSCHECK                                              
028290     .                                                                    
028291     EJECT                                                                
028292 IMS-GU-WDK601 SECTION.                                                   
028293     MOVE 'IMS-GU-WDK601  '  TO CURRENT-IMS-SECTION                       
028294                                                                          
028295     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
028296     DELIMITED BY SIZE INTO SSA1                                          
028297     MOVE '  GE' TO GOOD-STATUSCODES                                      
028298     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
028299     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSCHECK                                              
028301     .                                                                    
028302     SKIP2                                                                
028303 IMS-GNP-WDK611 SECTION.                                                  
028310     MOVE 'IMS-GNP-WDK611  '  TO CURRENT-IMS-SECTION                      
028320                                                                          
028700     MOVE 'WDK611  ' TO SSA1                                              
028800     MOVE '  GE' TO GOOD-STATUSCODES                                      
028900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
029000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
029100     PERFORM IMS-STATUSCHECK                                              
029200     .                                                                    
029300     SKIP2                                                                
029400 IMS-GNP-WDK623 SECTION.                                                  
029410     MOVE 'IMS-GNP-WDK623  ' TO CURRENT-IMS-SECTION                       
029500                                                                          
029600     MOVE 'WDK623  ' TO SSA1                                              
029700     MOVE '  GE' TO GOOD-STATUSCODES                                      
029800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1                   
029900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030000     PERFORM IMS-STATUSCHECK                                              
030100     .                                                                    
030200     EJECT                                                                
030300 IMS-STATUSCHECK SECTION.                                                 
030400                                                                          
030500     SET STATUS-IX TO 1                                                   
030600     SEARCH GOOD-STATUS                                                   
030700       AT END                                                             
030800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030900           DELIMITED BY SIZE INTO ERROR-TEXT                              
031000         DISPLAY ERROR-TEXT                                               
031100         CALL FELLOG                                                      
031200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
031300         CONTINUE                                                         
031400     END-SEARCH                                                           
031500     .                                                                    
