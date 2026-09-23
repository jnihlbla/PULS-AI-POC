000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2223900.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/12/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CALCULATE FORECAST                                               
000900*                                                                         
001000*        THE PROGRAM READS     WDK6                                       
001100*                                                                         
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
002400*          --- FILE TO UPDATE WDK629                                      
002500     SELECT W22237                     ASSIGN TO W22239D1.                
002600*          --- FILE TO UPDATE WDK611                                      
002700     SELECT W22239                     ASSIGN TO W22239D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W22237                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  RECORD -COPY W2223702 -PRE  IN- -  -L.                               
003800     EJECT                                                                
003900 FD  W22239                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  RECORD -COPY W22239   -PRE  OUT-   -L.                               
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004501                                                                          
004510*    -COPY WY2000W3                                                       
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2223900'.            
004800 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004900 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005400     EJECT                                                                
005500 77  W22237-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W22237                       VALUE 'J'.                   
005700 01  W-ARBETSAREOR.                                                       
005800     03  W-SUTPO-PB              PIC S9(7)   COMP-3.                      
005900     03  WS-KVFRYSTIPLUS1        PIC S9(3)   COMP-3.                      
006000     03  WS-TIFINLV-AAVV         PIC S9(5)   COMP-3.                      
006010     03  WS-ANT-VECKOR           PIC S9(3)   VALUE +0   COMP-3.           
006100                                                                          
006200     03  W-DATUM-AAVV            PIC 9(4).                                
006300     03  W-DAT-AAVV              REDEFINES W-DATUM-AAVV.                  
006400         05  W-DATUM-AA          PIC 9(2).                                
006500         05  W-DATUM-VV          PIC 9(2).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007400     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
007500     SKIP2                                                                
007600*                            *** PARAMETRAR TILL DATUMKORT                
007700 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22140'.                  
007800 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
007900*01  -COPY WDATKORT.                                                      
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600     SKIP2                                                                
008700 01  ERROR-TEXT.                                                          
008800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  OUT-AREA-START              PIC X(24)   VALUE                        
009600                                 'OUT-AREA-START  '.                      
009700                                                                          
009800*01  AREA -COPY W2223702   -PRE IN-                                       
009900     EJECT                                                                
010000*    --- AREAS FOR IMS-SECTIONS                                           
010100*                                                                         
010200     EJECT                                                                
010300 01  OUT-AREA-START              PIC X(24)   VALUE                        
010400                                 'OUT-AREA-START  '.                      
010500                                                                          
010600*01  AREA -COPY W22239     -PRE OUT-                                      
010700     EJECT                                                                
010800*    --- AREAS FOR IMS-SECTIONS                                           
010900*                                                                         
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011200     SKIP3                                                                
011300 01  KEYS-FOR-DLI.                                                        
012000                                                                          
012100     03  W-IDARTNR-X.                                                     
012200         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
012300                                                                          
012400      03  W-DABEHOV-X-MIN.                                                
012500         05  W-ANT-DABEHOV-MIN   PIC 9(6)   VALUE ZERO.                   
012600      03  W-DABEHOV-X-MAX.                                                
012700         05  W-ANT-DABEHOV-MAX   PIC 9(6)   VALUE ZERO.                   
012701      03  W-ANT-TIBEHOV-MIN      PIC S9(5) COMP-3 VALUE ZERO.             
012702      03  W-ANT-TIBEHOV-MAX      PIC S9(5) COMP-3 VALUE ZERO.             
012710                                                                          
012800*    --- STATUS-KOD FRÅN IMS                                              
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FOUND                       VALUE '  '.                  
013100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013300     88  SEGMENT-END                         VALUE 'GB'.                  
013400     SKIP2                                                                
013500 01  GOOD-STATUSCODES.                                                    
013600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNCTION CODES                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014600 01  DLI-IO-WDK601.                                                       
014700*    03  -COPY WDK601                                                     
014800     EJECT                                                                
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
015000 01  DLI-IO-WDK611.                                                       
015100*    03  -COPY WDK611                                                     
015200     EJECT                                                                
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
015400 01  DLI-IO-WDK901.                                                       
015500*    03  -COPY WDK901                                                     
015600     EJECT                                                                
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
015800 01  DLI-IO-WDK911.                                                       
015900*    03  -COPY WDK911                                                     
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200                                                                          
016600*01  -COPY W0008  -PRE WDK6-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900*01  -COPY W0008  -PRE WDK9-                                              
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200                                                                          
017300 PROCEDURE DIVISION  USING WDK6-PCB WDK9-PCB.                             
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING WDK6-PCB WDK9-PCB.                             
017600                                                                          
017700                                                                          
017800     PERFORM A-INIT                                                       
017900                                                                          
018000     PERFORM S01-READ-W22237                                              
018100     PERFORM UNTIL END-OF-W22237                                          
018200                                                                          
018300       PERFORM B-CALCULATE-PB-TPO                                         
018400                                                                          
018500       PERFORM S01-READ-W22237                                            
018600                                                                          
018700     END-PERFORM                                                          
018800                                                                          
018900                                                                          
019000     PERFORM Z-FINIT                                                      
019100                                                                          
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 A-INIT SECTION.                                                          
019700     MOVE 'A-INIT                  ' TO CURRENT-SECTION                   
019800                                                                          
019900     OPEN INPUT  W22237                                                   
020000          OUTPUT W22239                                                   
020100                                                                          
020200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020300     MOVE D-AAR        TO W-DATUM-AA                                      
020400     MOVE D-VECKA      TO W-DATUM-VV                                      
020500                                                                          
020600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 B-CALCULATE-PB-TPO  SECTION.                                             
021100     MOVE 'B-CALCULATE-PB-TPO      ' TO CURRENT-SECTION                   
021200                                                                          
021300     MOVE IN-IDARTNR             TO W-IDARTNR                             
021400     PERFORM IMS-GU-WDK601                                                
021500     IF SEGMENT-FOUND                                                     
021600        PERFORM IMS-GNP-WDK611                                            
021700        PERFORM UNTIL SEGMENT-MISSING                                     
021800          MOVE +0                TO WS-KVFRYSTIPLUS1                      
021900          MOVE +0                TO WS-TIFINLV-AAVV                       
022000          COMPUTE WS-KVFRYSTIPLUS1 = CLAG-KVFRYSTI + 1                    
022100          COMPUTE WS-KVFRYSTIPLUS1 = WS-KVFRYSTIPLUS1 * -1                
022200          COMPUTE WS-TIFINLV-AAVV  = ART-TIFINLV / 10                     
022300          CALL W009VADD USING WS-TIFINLV-AAVV WS-KVFRYSTIPLUS1            
022400                                                                          
022500          MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                             
022600          MOVE W-DATUM-AAVV      TO TMP2-YYWW                             
022700          PERFORM WY2000P3                                                
022800          IF TMP1-YYWW > TMP2-YYWW                                        
022900             CONTINUE                                                     
023000          ELSE                                                            
023100             MOVE +0             TO W-SUTPO-PB                            
023200             PERFORM IMS-GU-WDK901                                        
023300             IF SEGMENT-FOUND                                             
023301                PERFORM BA-KEY-DABEHOV                                    
023400                PERFORM IMS-GNP-WDK911                                    
023500                PERFORM UNTIL SEGMENT-MISSING                             
023600                   COMPUTE W-SUTPO-PB =                                   
023700                           W-SUTPO-PB + ANT-SUTPO-PB                      
023800                   PERFORM IMS-GNP-WDK911                                 
023900                END-PERFORM                                               
024000                                                                          
024010                MOVE IN-IDARTNR     TO OUT-IDARTNR                        
024100                IF W-SUTPO-PB > +0                                        
024210                   COMPUTE OUT-KVPB-TPO ROUNDED =                         
024300                           W-SUTPO-PB * 4.33 / 18                         
024400                ELSE                                                      
024500                   MOVE +0          TO OUT-KVPB-TPO                       
024600                END-IF                                                    
024610                MOVE 'N'            TO OUT-FLMANPB                        
024630                PERFORM S11-WRITE-W22239                                  
024700             END-IF                                                       
025100          END-IF                                                          
025400                                                                          
025500          PERFORM IMS-GNP-WDK611                                          
025600        END-PERFORM                                                       
025700     END-IF                                                               
025800                                                                          
025900     .                                                                    
026000     EJECT                                                                
026010 BA-KEY-DABEHOV SECTION.                                                  
026020     MOVE 'BA-KEY-DABEHOV          ' TO CURRENT-SECTION                   
026030                                                                          
026031*** LÄSER TPO-BEHOV FROM NÄSTA VECKA OCH 18 VECKOR FRAMÅT                 
026032*** DESSA TPO-BEHOV LIGGER TILL GRUND FÖR BERÄKNING AV KVPB-TPO           
026034                                                                          
026035     MOVE W-DATUM-AAVV    TO W-ANT-TIBEHOV-MIN                            
026036     MOVE +1              TO WS-ANT-VECKOR                                
026039     CALL W009VADD USING W-ANT-TIBEHOV-MIN WS-ANT-VECKOR                  
026040     MOVE +17               TO WS-ANT-VECKOR                              
026041     MOVE W-ANT-TIBEHOV-MIN TO W-ANT-TIBEHOV-MAX                          
026044     CALL W009VADD USING W-ANT-TIBEHOV-MAX WS-ANT-VECKOR                  
026045                                                                          
026046     MOVE W-ANT-TIBEHOV-MIN TO W-ANT-DABEHOV-MIN                          
026049     IF W-ANT-TIBEHOV-MIN NOT = ZERO                                      
026050       IF W-ANT-TIBEHOV-MIN < 5000                                        
026051         MOVE 20            TO W-ANT-DABEHOV-MIN (1:2)                    
026052       ELSE                                                               
026053         IF W-ANT-TIBEHOV-MIN < 9999                                      
026054           MOVE 19          TO W-ANT-DABEHOV-MIN (1:2)                    
026055         ELSE                                                             
026056           MOVE 999999      TO W-ANT-DABEHOV-MIN                          
026057         END-IF                                                           
026058       END-IF                                                             
026059     END-IF                                                               
026063                                                                          
026064     MOVE W-ANT-TIBEHOV-MAX TO W-ANT-DABEHOV-MAX                          
026066     IF W-ANT-TIBEHOV-MAX NOT = ZERO                                      
026067       IF W-ANT-TIBEHOV-MAX < 5000                                        
026068         MOVE 20            TO W-ANT-DABEHOV-MAX (1:2)                    
026069       ELSE                                                               
026070         IF W-ANT-TIBEHOV-MAX < 9999                                      
026071           MOVE 19          TO W-ANT-DABEHOV-MAX (1:2)                    
026072         ELSE                                                             
026073           MOVE 999999      TO W-ANT-DABEHOV-MAX                          
026074         END-IF                                                           
026075       END-IF                                                             
026076     END-IF                                                               
026080     .                                                                    
026090     EJECT                                                                
026100 Z-FINIT SECTION.                                                         
026200     MOVE 'Z-FINIT                 ' TO CURRENT-SECTION                   
026300                                                                          
026400     CLOSE W22237                                                         
026500           W22239                                                         
026600                                                                          
026700     MOVE 'S' TO POSTSUM-OPKOD                                            
026800     CALL POSTSUM USING POSTSUM-PARM                                      
026900     .                                                                    
027000     EJECT                                                                
027100 S01-READ-W22237 SECTION.                                                 
027200     MOVE 'S01-READ-W22237        ' TO CURRENT-SECTION                    
027210                                                                          
027300     READ W22237 INTO IN-AREA                                             
027400     AT END                                                               
027500        SET END-OF-W22237 TO TRUE                                         
027600                                                                          
027700     NOT AT END                                                           
027800        MOVE 'W22237'   TO POSTSUM-FDNAMN                                 
027900        MOVE 'W22239D1' TO POSTSUM-DDNAMN2                                
028000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
028100        CALL POSTSUM USING POSTSUM-PARM                                   
028200                                                                          
028300     END-READ                                                             
028600     .                                                                    
028700     EJECT                                                                
028800 S11-WRITE-W22239   SECTION.                                              
028900     MOVE 'S11-WRITE-W22239       ' TO CURRENT-SECTION                    
029000                                                                          
029100     WRITE OUT-RECORD FROM OUT-AREA                                       
029200                                                                          
029300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029400     MOVE 'W22237'   TO POSTSUM-FDNAMN                                    
029500     MOVE 'W22239D2' TO POSTSUM-DDNAMN2                                   
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     .                                                                    
029800                                                                          
029900 S99-ABEND SECTION.                                                       
030000     MOVE 'S99-ABEND               ' TO CURRENT-SECTION                   
030100                                                                          
030200     SKIP2                                                                
030300     MOVE 'S' TO POSTSUM-OPKOD                                            
030400     CALL POSTSUM USING POSTSUM-PARM                                      
030500     CALL ABEND USING RKOD-ABEND                                          
030600     .                                                                    
030700     EJECT                                                                
030800* --- IMS SECTIONS  ---                                                   
030900                                                                          
031000     EJECT                                                                
031100 IMS-GU-WDK601 SECTION.                                                   
031200     MOVE 'IMS-GU-WDK601    ' TO DBS-SECTION                              
031300                                                                          
031400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     MOVE '  GE'           TO GOOD-STATUSCODES                            
031700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
031800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSCHECK                                              
032000     .                                                                    
032100                                                                          
032200 IMS-GNP-WDK611       SECTION.                                            
032300     MOVE 'IMS-GNP-WDK611   ' TO DBS-SECTION                              
032400                                                                          
032500     MOVE 'WDK611  '        TO SSA1                                       
032600     MOVE '  GE'            TO GOOD-STATUSCODES                           
032700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
032800     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
032900     PERFORM IMS-STATUSCHECK                                              
033000     .                                                                    
033100                                                                          
033200 IMS-GU-WDK901 SECTION.                                                   
033300     MOVE 'IMS-GU-WDK901    ' TO DBS-SECTION                              
033400                                                                          
033500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
033600          DELIMITED BY SIZE INTO SSA1                                     
033700     MOVE '  GE'              TO GOOD-STATUSCODES                         
033800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
033900     MOVE WDK9-STATUS-CODE    TO STATUS-WS                                
034000     PERFORM IMS-STATUSCHECK                                              
034100     .                                                                    
034200                                                                          
034300 IMS-GNP-WDK911         SECTION.                                          
034400     MOVE 'IMS-GNP-WDK911   ' TO DBS-SECTION                              
034500                                                                          
034600     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-X-MIN                         
034700                    '&DABEHOV <=' W-DABEHOV-X-MAX ')'                     
034800            DELIMITED BY SIZE INTO SSA1                                   
034900     MOVE '  GE'                TO GOOD-STATUSCODES                       
035000     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1                   
035100     MOVE WDK9-STATUS-CODE      TO STATUS-WS                              
035200     PERFORM IMS-STATUSCHECK                                              
035300     .                                                                    
035400     EJECT                                                                
035500 IMS-STATUSCHECK SECTION.                                                 
035600                                                                          
035700     SET STATUS-IX TO 1                                                   
035800     SEARCH GOOD-STATUS                                                   
035900       AT END                                                             
036000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036100           DELIMITED BY SIZE INTO ERROR-TEXT                              
036200         DISPLAY ERROR-TEXT                                               
036300         CALL FELLOG                                                      
036400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036500         CONTINUE                                                         
036600     END-SEARCH                                                           
036700     .                                                                    
036800     EJECT                                                                
036900*    -COPY WY2000P3                                                       
037000     EJECT                                                                
