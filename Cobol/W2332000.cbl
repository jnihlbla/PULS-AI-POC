000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2332000.                                                
000300 AUTHOR.         IDK, GÖTEBORG.                                           
000400 DATE-WRITTEN.   NOV 1978.                                                
000500     REMARKS.                                                             
000600*        PROGRAMMET SOM ÄR EN EXIT TILL HJÄLPPROGRAMMET IMS               
000700*        FAST SCAN UTILITY (FSU) LÄSER GÄLLANDE LEVERANS-                 
000800*        PLANER OCH LÄGGER UT AVROP PÅ UTFILEN.                           
000900                                                                          
001000*        ÄNDRAT TILL SB/COBOL-II    131015                                
001100                                                                          
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700*--------------------------------------- AVROP                            
001800*                                        OUTPUT                           
001900     SKIP1                                                                
002000         SELECT AVROPFIL1      ASSIGN TO W23320D1.                        
002100         SELECT UTFIL          ASSIGN TO W23320D2.                        
002110         SELECT AVROPFIL2      ASSIGN TO W23320D3.                        
002120         SELECT AVROPFIL3      ASSIGN TO W23320D4.                        
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500 FD  AVROPFIL1                                                            
002600     RECORDING F                                                          
002700     BLOCK 0                                                              
002800     LABEL RECORD STANDARD.                                               
002900     SKIP1                                                                
003000*01  POST    -COPY W233LI20      -PRE AVROP1- -L.                         
003100*++INCLUDE W233LI20                                                       
003200     SKIP1                                                                
003300 FD  UTFIL                                                                
003400     RECORDING F                                                          
003500     BLOCK 0                                                              
003600     LABEL RECORD STANDARD.                                               
003700     SKIP1                                                                
003800*01  POST    -COPY W233AVR       -PRE UT-    -L.                          
003900*++INCLUDE W233AVR                                                        
004000     EJECT                                                                
004010 FD  AVROPFIL2                                                            
004020     RECORDING V                                                          
004030     BLOCK 0                                                              
004040     LABEL RECORD STANDARD.                                               
004050     SKIP1                                                                
004060*01  POST    -COPY W23320        -PRE AVROP2- -L.                         
004080     SKIP1                                                                
004090 FD  AVROPFIL3                                                            
004091     RECORDING F                                                          
004092     BLOCK 0                                                              
004093     LABEL RECORD STANDARD.                                               
004094                                                                          
004095*01  POST    -COPY W233LI2X      -PRE AVROP3- -L.                         
004096     EJECT                                                                
004097                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP3                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004410 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004420 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004500 01  FELTEXT.                                                             
004600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004800 01  KONSTANTER.                                                          
004900  03 JA                      PIC X       VALUE 'J'.                       
005000  03 NEJ                     PIC X       VALUE 'N'.                       
005100     SKIP3                                                                
005200 01  WS-IDARTNR              PIC 9(8)    VALUE ZERO.                      
005210 01  WS-IDDC                 PIC X(2)    VALUE SPACE.                     
005300 01  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                      
005400 01  WS-DAAVROP-AVS          PIC 9(6).                                    
005500 01  FILLER   REDEFINES WS-DAAVROP-AVS.                                   
005600     03  WS-DAAVROP-SS       PIC 9(2).                                    
005700     03  WS-DAAVROP-AAVV     PIC 9(4).                                    
005801     EJECT                                                                
005810*    -COPY WWDCKONS                                                       
005820     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000     03  POSTSUM     PIC X(8)    VALUE 'POSTSUM '.                        
006100     03  CBLTDLI     PIC X(8)    VALUE 'CBLTDLI '.                        
006200     03  FELLOG      PIC X(8)    VALUE 'FELLOG'.                          
006300     03  WDATKONV    PIC X(8)    VALUE 'WDATKONV'.                        
006400     EJECT                                                                
006500*01      -COPY WDATAREA.                                                  
006600     EJECT                                                                
006700*--------------------------------------- PARAMETRAR TILL POSTSUM          
006800     SKIP1                                                                
006900*01          -COPY W0005         -PRE POSTSUM-                            
007000*++INCLUDE W0005                                                          
007100     EJECT                                                                
007200*----------------------------------- AREA FÖR POST PÅ AVROPFIL1           
007300     SKIP1                                                                
007400*01  AREA    -COPY W233LI20      -PRE AVROP1-                             
007500*++INCLUDE W233LI20                                                       
007600     EJECT                                                                
007610*----------------------------------- AREA FÖR POST PÅ AVROPFIL2           
007620     SKIP1                                                                
007630*01  AREA    -COPY W23320        -PRE AVROP2-                             
007640*++INCLUDE W233LI20                                                       
007650     EJECT                                                                
007660*----------------------------------- AREA FÖR POST PÅ AVROPFIL3           
007670     SKIP1                                                                
007680*01  AREA    -COPY W233LI2X      -PRE AVROP3-                             
007691     EJECT                                                                
007700*----------------------------------- AREA FÖR POST PÅ UTFIL               
007800     SKIP1                                                                
007900*01  AREA    -COPY W233AVR       -PRE UT-                                 
008000*++INCLUDE W233AVR                                                        
008100     EJECT                                                                
008110 01  KEYS-FOR-DLI.                                                        
008120*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
008130                                                                          
008140     03  W-IDARTNR-X.                                                     
008150         05  W-IDARTNR          PIC S9(9)           COMP-3.               
008170     03 W-IDDC-X.                                                         
008180         05  W-IDDC            PIC X(2)    VALUE SPACE.                   
008190                                                                          
008200 01  IMS-WS.                                                              
008300     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
008400*----------------------------------STATUSKODER FRÅN IMS                   
008500     03  STATUS-WS   PIC XX.                                              
008600         88  SEGMENT-FINNS       VALUE '  '.                              
008610         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
008700         88  SEGMENT-SLUT        VALUE 'GB'.                              
008800                                                                          
008900     03  GODK-STATUSKODER.                                                
009000      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
009100                                                                          
009110 01  SSA1                       PIC X(64).                                
009120 01  SSA2                       PIC X(64).                                
009200     EJECT                                                                
009300*----------------------------------IMS-CALL FUNKTIONER                    
009400*01              -COPY W0003                                              
009500*++INCLUDE W0003                                                          
009600     EJECT                                                                
009700 01  DLI-IO-AREA             PIC X(200).                                  
009800     EJECT                                                                
009900*01  WDD901      -COPY WDD901     -PRE LEV-      -RED DLI-IO-AREA         
010000*++INCLUDE WDD901                                                         
010100     EJECT                                                                
010200*01  WDD902      -COPY WDD902     -PRE BEST-     -RED DLI-IO-AREA         
010300*++INCLUDE WDD902                                                         
010400     EJECT                                                                
010500*01  WDD905      -COPY WDD905     -PRE AV-       -RED DLI-IO-AREA         
010600*++INCLUDE WDD905                                                         
010700     EJECT                                                                
010710 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK601'.         
010720 01  DLI-IO-WDK601.                                                       
010730*    03 -COPY WDK601                                                      
010740     EJECT                                                                
010750 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK611'.         
010760 01  DLI-IO-WDK611.                                                       
010770*    03 -COPY WDK611                                                      
010780     EJECT                                                                
010790 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK623'.         
010791 01  DLI-IO-WDK623.                                                       
010792*    03 -COPY WDK623                                                      
010793     EJECT                                                                
010794 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK711'.         
010795 01  DLI-IO-WDK711.                                                       
010796*    03 -COPY WDK711                                                      
010797     EJECT                                                                
010798 01  DLI-IO-WDK722.                                                       
010799*    03 -COPY WDK722                                                      
010800     EJECT                                                                
010801 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK723'.         
010802 01  DLI-IO-WDK723.                                                       
010803*    03 -COPY WDK723                                                      
010804     EJECT                                                                
010810 LINKAGE SECTION.                                                         
010900*01    -COPY W0008         -PRE WDD9-                                     
011000*++INCLUDE W0008                                                          
011100          05  FILLER      PIC   XX.                                       
011101     EJECT                                                                
011110*01    -COPY W0008         -PRE WDK6-                                     
011130          05  FILLER      PIC   X.                                        
011131     EJECT                                                                
011140*01    -COPY W0008         -PRE WDK7-                                     
011150          05  FILLER      PIC   X.                                        
011200     EJECT                                                                
011300 PROCEDURE DIVISION   USING WDD9-PCB WDK6-PCB WDK7-PCB.                   
011400     ENTRY 'CBLTDLI'  USING WDD9-PCB WDK6-PCB WDK7-PCB.                   
011500     PERFORM A-INITIERA                                                   
011600     PERFORM IMS-GET-WDD9                                                 
011700     PERFORM UNTIL SEGMENT-SLUT                                           
011800         EVALUATE WDD9-SEG-NAME-FB                                        
011900            WHEN 'WDD901  '                                               
012000                            PERFORM B-BEHANDLA-WDD901                     
012100            WHEN 'WDD902  '                                               
012200                            PERFORM C-BEHANDLA-WDD902                     
012300            WHEN 'WDD905  '                                               
012400                            PERFORM D-BEHANDLA-WDD905                     
012500         END-EVALUATE                                                     
012600         PERFORM IMS-GET-WDD9                                             
012700     END-PERFORM                                                          
012800     PERFORM Z-FINIT                                                      
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300******************************************************************        
013400*                                                                *        
013500*        INITIERA.                                               *        
013600*                                                                *        
013700******************************************************************        
013800 A-INITIERA SECTION.                                                      
013900     SKIP1                                                                
014000     OPEN OUTPUT AVROPFIL1                                                
014100                 UTFIL                                                    
014110                 AVROPFIL2                                                
014120                 AVROPFIL3                                                
014200     MOVE 'AVR'              TO AVROP1-IDPTYP                             
014300                                UT-IDPTYP                                 
014400     MOVE 'W23320'           TO POSTSUM-PROGNAMN                          
014500     .                                                                    
014600     EJECT                                                                
014700******************************************************************        
014800*                                                                *        
014900*        SPARA ARTIKELNR.                                        *        
015000*                                                                *        
015100******************************************************************        
015200 B-BEHANDLA-WDD901 SECTION.                                               
015300     SKIP1                                                                
015400     MOVE LEV-IDARTNR TO AVROP1-IDARTNR                                   
015500                         UT-IDARTNR                                       
015510     MOVE LEV-IDARTNR TO WS-IDARTNR                                       
015511                         W-IDARTNR                                        
015520     MOVE WS-IDARTNR  TO AVROP2-IDARTNR                                   
015530                                                                          
015600     MOVE LEV-IDDC    TO AVROP1-IDDC                                      
015610                         AVROP2-IDDC                                      
015700                         WS-IDDC                                          
015800     .                                                                    
015900     EJECT                                                                
016000******************************************************************        
016100*                                                                *        
016200*        SPARA LEVERANTÖR.                                       *        
016300*                                                                *        
016400******************************************************************        
016500 C-BEHANDLA-WDD902 SECTION.                                               
016600     SKIP1                                                                
016700     MOVE BEST-IDLEVNR       TO AVROP1-IDLEVNR                            
016710                                AVROP2-IDLEVNR                            
016800                                UT-IDLEVNR                                
016900     .                                                                    
017000     EJECT                                                                
017100******************************************************************        
017200*                                                                *        
017300*        SKRIV UTFIL OCH AVROPFIL1 OCH AVROPSFIL2 OM KDAVROP=2   *        
017400*        ORDERINGÅNG I UTPOST. OBS ATT ENDAST SISTA ÅR-          *        
017500*        SIFFRAN OCH PERIOD JÄMFÖRS.                             *        
017700*                                                                *        
017710******************************************************************        
017800 D-BEHANDLA-WDD905 SECTION.                                               
017900     SKIP1                                                                
018000     IF AV-KDAVROP = 2                                                    
018100        MOVE AV-TIAVRDAT-DISP     TO DAT-I-TIDATUM                        
018200        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
018300        CALL WDATKONV USING          DAT-KDDATFORM                        
018400                                     DAT-I-TIDATUM                        
018500                                     DAT-O-TIDATUM                        
018600                                     DAT-KDSVAR                           
018700        IF DAT-KDSVAR-FEL                                                 
018800          MOVE 'FEL VID ANROP TILL DATKONV 1' TO FELTEXT-STR              
018900          CALL FELLOG                                                     
019000        ELSE                                                              
019100          MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                            
019200        END-IF                                                            
019300        MOVE WS-TIAAVV       TO AVROP1-TIAVROP-DISP                       
019400                                UT-TIAVROP-DISP                           
019500                                                                          
019600        MOVE AV-KVAVROP      TO AVROP1-KVAVROP                            
019610                                AVROP2-KVAVROP                            
019700                                UT-KVAVROP                                
019800        MOVE AV-DAAVROP-AVS  TO WS-DAAVROP-AVS                            
019900        MOVE WS-DAAVROP-AAVV TO AVROP1-TIAVROP-AVS                        
020000                                UT-TIAVROP-AVS                            
020001        MOVE '20'            TO AVROP2-DAAVROP-AVS(1:2)                   
020010        MOVE WS-DAAVROP-AAVV TO AVROP2-DAAVROP-AVS(3:4)                   
020100        MOVE AV-TILEVDAG     TO AVROP1-TILEVDAG                           
020200                                UT-TILEVDAG                               
020300                                                                          
020400        MOVE AV-TIAVRDAT-INL      TO DAT-I-TIDATUM                        
020500        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
020600        CALL WDATKONV USING          DAT-KDDATFORM                        
020700                                     DAT-I-TIDATUM                        
020800                                     DAT-O-TIDATUM                        
020900                                     DAT-KDSVAR                           
021000        IF DAT-KDSVAR-FEL                                                 
021100          MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT-STR              
021200          CALL FELLOG                                                     
021300        ELSE                                                              
021400          MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                            
021500        END-IF                                                            
021600        MOVE WS-TIAAVV            TO AVROP1-TIAVROP-INL                   
021700                                     UT-TIAVROP-INL                       
021800                                                                          
021802        MOVE '20'                 TO AVROP2-DAAVROP-INL(1:2)              
021803        MOVE WS-TIAAVV            TO AVROP2-DAAVROP-INL(3:4)              
021810        PERFORM DA-IDLEVNR-SHIP                                           
021820                                                                          
021900        PERFORM S01-SKRIV-AVROPFIL1                                       
021910        PERFORM S02-SKRIV-AVROPFIL2                                       
021911        PERFORM DB-INFO-TO-AVROPFIL3                                      
021912        PERFORM S04-SKRIV-AVROPFIL3                                       
022000        IF WS-IDDC = '11'                                                 
022100           PERFORM S03-SKRIV-UTFIL                                        
022200        END-IF                                                            
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022510 DA-IDLEVNR-SHIP SECTION.                                                 
022512                                                                          
022514     MOVE AVROP2-IDDC                 TO WS-IDDC                          
022515     IF AVROP2-IDDC = WC-CDC-SE                                           
022517       MOVE AVROP2-IDLEVNR            TO AVROP2-IDLEVNR-SHIP              
022519       PERFORM IMS-GU-WDK601                                              
022520       IF SEGMENT-FINNS                                                   
022521         PERFORM IMS-GNP-WDK611                                           
022522         IF SEGMENT-FINNS                                                 
022523           IF AVROP2-IDLEVNR = ART-IDLEVNR                                
022524             MOVE CLAG-IDLEVNR-SHIP   TO AVROP2-IDLEVNR-SHIP              
022530           ELSE                                                           
022531             PERFORM IMS-GNP-WDK623                                       
022532             PERFORM UNTIL SEGMENT-SAKNAS                                 
022533                        OR AVROP2-IDLEVNR = AVT-IDLEVNR-AVT               
022534               PERFORM IMS-GNP-WDK623                                     
022535             END-PERFORM                                                  
022536             IF SEGMENT-FINNS                                             
022538               MOVE AVT-IDLEVNR-SHIP  TO AVROP2-IDLEVNR-SHIP              
022543             END-IF                                                       
022544           END-IF                                                         
022545         END-IF                                                           
022546       END-IF                                                             
022547     ELSE                                                                 
022548       MOVE AVROP2-IDLEVNR            TO AVROP2-IDLEVNR-SHIP              
022550       MOVE AVROP2-IDDC               TO W-IDDC                           
022551       PERFORM IMS-GU-WDK711                                              
022552       IF SEGMENT-FINNS                                                   
022553         PERFORM IMS-GNP-WDK722                                           
022554         IF SEGMENT-FINNS                                                 
022555           IF AVROP2-IDLEVNR = SLAG-IDLEVNR                               
022561             MOVE XLAG-IDLEVNR-SHIP   TO AVROP2-IDLEVNR-SHIP              
022562           ELSE                                                           
022563             PERFORM IMS-GNP-WDK723                                       
022564             PERFORM UNTIL SEGMENT-SAKNAS                                 
022565                        OR AVROP2-IDLEVNR = SAVT-IDLEVNR-AVT              
022566               PERFORM IMS-GNP-WDK723                                     
022567             END-PERFORM                                                  
022568             IF SEGMENT-FINNS                                             
022569               MOVE SAVT-IDLEVNR-SHIP TO AVROP2-IDLEVNR-SHIP              
022575             END-IF                                                       
022576           END-IF                                                         
022577         END-IF                                                           
022578       END-IF                                                             
022579     END-IF                                                               
022580     .                                                                    
022590     EJECT                                                                
022591 DB-INFO-TO-AVROPFIL3 SECTION.                                            
022592                                                                          
022593     MOVE AVROP1-IDPTYP        TO AVROP3-IDPTYP                           
022594     MOVE AVROP1-IDARTNR       TO AVROP3-IDARTNR                          
022596     MOVE AVROP1-IDDC          TO AVROP3-IDDC                             
022597     MOVE AVROP1-IDLEVNR       TO AVROP3-IDLEVNR                          
022598     MOVE AVROP1-TIAVROP-AVS   TO AVROP3-TIAVROP-AVS                      
022599     MOVE AVROP1-TIAVROP-INL   TO AVROP3-TIAVROP-INL                      
022600     MOVE AVROP1-TIAVROP-DISP  TO AVROP3-TIAVROP-DISP                     
022601     MOVE AVROP1-KVAVROP       TO AVROP3-KVAVROP                          
022602     MOVE AVROP1-TILEVDAG      TO AVROP3-TILEVDAG                         
022606     .                                                                    
022607     EJECT                                                                
022610******************************************************************        
022700*                                                                *        
022800*        SKRIV EN POST PÅ AVROPFIL1.                             *        
022900*                                                                *        
023000******************************************************************        
023100 S01-SKRIV-AVROPFIL1 SECTION.                                             
023200     SKIP1                                                                
023300     WRITE AVROP1-POST        FROM AVROP1-AREA                            
023400     SKIP1                                                                
023500     MOVE 'AVROPFIL1'        TO POSTSUM-FDNAMN                            
023600     MOVE 'W23320D1'         TO POSTSUM-DDNAMN2                           
023700     MOVE AVROP1-IDPTYP      TO POSTSUM-TRANSTYP                          
023800     CALL POSTSUM USING POSTSUM-PARM                                      
023900     SKIP1                                                                
024000     .                                                                    
024100     EJECT                                                                
024101 S04-SKRIV-AVROPFIL3 SECTION.                                             
024102     WRITE AVROP3-POST        FROM AVROP3-AREA                            
024103     SKIP1                                                                
024104     MOVE 'AVROPFIL3'        TO POSTSUM-FDNAMN                            
024105     MOVE 'W23320D4'         TO POSTSUM-DDNAMN2                           
024106     MOVE AVROP3-IDPTYP      TO POSTSUM-TRANSTYP                          
024107     CALL POSTSUM USING POSTSUM-PARM                                      
024108     SKIP1                                                                
024109     .                                                                    
024110     EJECT                                                                
024111******************************************************************        
024120*                                                                *        
024130*        SKRIV EN POST PÅ AVROPFIL2.                             *        
024140*                                                                *        
024150******************************************************************        
024160 S02-SKRIV-AVROPFIL2 SECTION.                                             
024170     SKIP1                                                                
024180     WRITE AVROP2-POST        FROM AVROP2-AREA                            
024190     SKIP1                                                                
024191     MOVE 'AVROPFIL2'        TO POSTSUM-FDNAMN                            
024192     MOVE 'W23320D3'         TO POSTSUM-DDNAMN2                           
024193     MOVE SPACE              TO POSTSUM-TRANSTYP                          
024194     CALL POSTSUM USING POSTSUM-PARM                                      
024195     SKIP1                                                                
024196     .                                                                    
024197     EJECT                                                                
024200******************************************************************        
024300*                                                                *        
024400*        SKRIV EN POST PÅ UTFIL.                                 *        
024500*                                                                *        
024600******************************************************************        
024700 S03-SKRIV-UTFIL SECTION.                                                 
024800     SKIP1                                                                
024900     WRITE UT-POST           FROM UT-AREA                                 
025000     SKIP1                                                                
025100     MOVE 'UTFIL'            TO POSTSUM-FDNAMN                            
025200     MOVE 'W23320D2'         TO POSTSUM-DDNAMN2                           
025300     MOVE UT-IDPTYP          TO POSTSUM-TRANSTYP                          
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     SKIP1                                                                
025600     .                                                                    
025700     EJECT                                                                
025800 IMS-GET-WDD9 SECTION.                                                    
025900     SKIP3                                                                
026000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026100     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-AREA                           
026200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
026300     PERFORM IMS-STATUSKONTROLL                                           
026400     .                                                                    
026401                                                                          
026402 IMS-GU-WDK601          SECTION.                                          
026403     MOVE 'IMS-GU-WDK601   '    TO DBS-SECTION                            
026404                                                                          
026405     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
026406            DELIMITED BY SIZE INTO SSA1                                   
026407     MOVE '  GE' TO GODK-STATUSKODER                                      
026408     CALL CBLTDLI USING GU     WDK6-PCB DLI-IO-WDK601 SSA1                
026409     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026410     PERFORM IMS-STATUSKONTROLL                                           
026411     .                                                                    
026412                                                                          
026413 IMS-GNP-WDK611 SECTION.                                                  
026414     MOVE 'IMS-GU-WDK611   '    TO DBS-SECTION                            
026415                                                                          
026420     MOVE 'WDK611   ' TO SSA1                                             
026440     MOVE '  GE' TO GODK-STATUSKODER                                      
026450     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
026460     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026470     PERFORM IMS-STATUSKONTROLL                                           
026480     .                                                                    
026481                                                                          
026490 IMS-GNP-WDK623 SECTION.                                                  
026491     MOVE 'IMS-GU-WDK623   '    TO DBS-SECTION                            
026492                                                                          
026493     MOVE 'WDK623   ' TO SSA1                                             
026494     MOVE '  GE' TO GODK-STATUSKODER                                      
026495     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1                   
026496     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
026497     PERFORM IMS-STATUSKONTROLL                                           
026498     .                                                                    
026499                                                                          
026500 IMS-GU-WDK711 SECTION.                                                   
026501     MOVE 'IMS-GU-WDK711   '    TO DBS-SECTION                            
026502                                                                          
026505     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
026506          DELIMITED BY SIZE   INTO SSA1                                   
026507     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
026508          DELIMITED BY SIZE   INTO SSA2                                   
026509     MOVE '  GE' TO GODK-STATUSKODER                                      
026510     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
026511     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
026512     PERFORM IMS-STATUSKONTROLL                                           
026513     .                                                                    
026514                                                                          
026515 IMS-GNP-WDK722 SECTION.                                                  
026516     MOVE 'IMS-GU-WDK722   '    TO DBS-SECTION                            
026517                                                                          
026520     MOVE 'WDK722 '             TO SSA1                                   
026521     MOVE '  GE' TO GODK-STATUSKODER                                      
026522     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK722 SSA1                   
026523     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
026524     PERFORM IMS-STATUSKONTROLL                                           
026525     .                                                                    
026526                                                                          
026527 IMS-GNP-WDK723 SECTION.                                                  
026528     MOVE 'IMS-GU-WDK723   '    TO DBS-SECTION                            
026529                                                                          
026530     MOVE 'WDK723 '             TO SSA1                                   
026531     MOVE '  GE' TO GODK-STATUSKODER                                      
026532     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
026533     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
026534     PERFORM IMS-STATUSKONTROLL                                           
026535     .                                                                    
026536                                                                          
026540 IMS-STATUSKONTROLL SECTION.                                              
026600     SKIP3                                                                
026700     SET STATUS-IX TO 1                                                   
026800     SEARCH GODK-STATUS AT END CALL FELLOG                                
026900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
027000       CONTINUE                                                           
027100     END-SEARCH                                                           
027200     .                                                                    
027300     EJECT                                                                
027400 Z-FINIT SECTION.                                                         
027500     MOVE 'S'             TO POSTSUM-OPKOD                                
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     CLOSE AVROPFIL1                                                      
027800           UTFIL                                                          
027810           AVROPFIL2                                                      
027820           AVROPFIL3                                                      
027900     EJECT                                                                
028000     .                                                                    
