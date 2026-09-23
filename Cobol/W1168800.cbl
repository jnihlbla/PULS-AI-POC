000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1168800.                                                
000400 AUTHOR.         SUSANNE OLSSON.                                          
000500 DATE-WRITTEN.   DECEMBER 2011.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR TOTALFIL ERSÄTTNINGAR KINA NDC'R                          
001100*        SOM SKICKAS TILL VIPS.                                           
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000*                                                                         
002100*    E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                         
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- ERSATTA ARTIKLAR                                           
003200     SELECT W11679                     ASSIGN TO W11688D1.                
003500*          --- KINA ARTIKLAR                                              
003600     SELECT W1168A                     ASSIGN TO W11688D2.                
003610*          --- TOTALFIL                                                   
003620     SELECT W11688                     ASSIGN TO W11688D3.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W11679                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500*01  POST -COPY W11679  -PRE  IN-      -L.                                
004600                                                                          
004700 FD  W11688                                                               
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000*01  POST -COPY W11636 -PRE  UT-      -L.                                 
005100                                                                          
005200 FD  W1168A                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500*01  POST -COPY W1168A                -L.                                 
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP2                                                                
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W1168800'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
006500 77  WS-FL-TYP1                  PIC X.                                   
006600 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
006700     SKIP2                                                                
006800 01  WS-KDERS-NUM                PIC 9(2).                                
006900 01  WS-KDERS REDEFINES WS-KDERS-NUM.                                     
007000     03 WS-KDERSPOS1             PIC X.                                   
007100     03 WS-KDERSPOS2             PIC X.                                   
007200                                                                          
007300 01  WS-IDARTNR.                                                          
007400     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
007500     03 WS-IDARTNR-ALFA          PIC X(9).                                
007600                                                                          
007700 01  WS-IDARTNR-TILLK.                                                    
007800     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
007900     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
008000                                                                          
008010*01  -COPY WWPRODSL                                                       
008020                                                                          
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400                                                                          
008500 77  W11679-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W11679                       VALUE 'J'.                   
008700                                                                          
008800 77  W1168A-EOF-SW               PIC X       VALUE 'N'.                   
008900     88  END-OF-W1168A                       VALUE 'J'.                   
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009700     EJECT                                                                
009780                                                                          
009781 01  WS-MQ-LINE1.                                                         
009782    03  FILLER                  PIC X(337)  VALUE                         
009783                                '¤MQMPROP Vidb_Source=Full'.              
009784 01  WS-MQ-LINE2.                                                         
009785    03  FILLER                  PIC X(337)  VALUE                         
009786                                '¤MQMPROP LoadType=Full'.                 
009787 01  WS-MQ-LINE3.                                                         
009788    03  FILLER                  PIC X(16)   VALUE                         
009789                                '¤MQMPROP Market='.                       
009790    03  WS-MQ-IDLANDX2          PIC X(2)    VALUE SPACE.                  
009791    03  FILLER                  PIC X(319)  VALUE SPACE.                  
009792    EJECT                                                                 
009903*    ---- PARAMETRAR TILL WDATKONV                                        
009904 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
010000*01  -COPY WDATAREA                                                       
010100*    --- PARAMETRAR TILL POSTSUM                                          
010200*                                                                         
010300*01  -COPY W0005   -PRE  POSTSUM-                                         
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                             'IN-AREA-START'.             
010700*01  AREA -COPY W11679     -PRE IN-                                       
010800     EJECT                                                                
010900 01  W1168A-AREA-START             PIC X(24)   VALUE                      
011000                                             'W1168A-AREA-START'.         
011100*01  AREA -COPY W1168A     -PRE IN8A-                                     
011200     EJECT                                                                
011300 01  UT-AREA-START               PIC X(24)   VALUE                        
011400                                             'UT-AREA-START'.             
011500*01  AREA -COPY W11636     -PRE UT-                                       
011600     EJECT                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011800     03  W-IDARTNR-X.                                                     
011900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012700     88  IMS-EJ-OK                           VALUE 'XD'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013700     EJECT                                                                
013800*    ---  DLI INPUT-OUTPUT AREA                                           
013900                                                                          
014000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
014100 01  DLI-IO-WDK601.                                                       
014200*    03  -COPY WDK601                                                     
014300     EJECT                                                                
014400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
014500 01  DLI-IO-WDD701.                                                       
014600*    03  -COPY WDD701  -PRE ERSA-                                         
014700     EJECT                                                                
014800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD702'.                      
014900 01  DLI-IO-WDD702.                                                       
015000*    03  -COPY WDD702  -PRE ERSA-                                         
015100     EJECT                                                                
015200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD704'.                      
015300 01  DLI-IO-WDD704.                                                       
015400*    03  -COPY WDD704  -PRE ERSA-                                         
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*                                                                         
015800*01  -COPY W0008  -PRE WDK6-                                              
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100*01  -COPY W0008  -PRE WDD7-                                              
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400 PROCEDURE DIVISION  USING WDK6-PCB WDD7-PCB.                             
016500 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING WDK6-PCB WDD7-PCB.                             
016700                                                                          
016800     SKIP2                                                                
016900     PERFORM A-INIT                                                       
017000     PERFORM S01-LAES-W11679                                              
017100     PERFORM S04-LAES-W1168A                                              
017200                                                                          
017300     PERFORM UNTIL END-OF-W11679 AND                                      
017400                   END-OF-W1168A                                          
017500       IF IN-IDARTNR = IN8A-IDARTNR                                       
017600          IF IN-KDERS < 20                                                
017700             PERFORM B-BEARBETA                                           
017800          ELSE                                                            
017900            IF IN-KDERS > 20                                              
018000               IF IN-FLERSATT = 'J'                                       
018100                  PERFORM B-BEARBETA                                      
018200               ELSE                                                       
018300                  IF IN-KDERS = 21 OR 24 OR 29                            
018400                     SUBTRACT 10 FROM IN-KDERS                            
018500                     PERFORM B-BEARBETA                                   
018600                  END-IF                                                  
018700               END-IF                                                     
018800            END-IF                                                        
018900          END-IF                                                          
019000          PERFORM S01-LAES-W11679                                         
019100          PERFORM S04-LAES-W1168A                                         
019200       ELSE                                                               
019300          IF IN-IDARTNR > IN8A-IDARTNR                                    
019400             PERFORM S04-LAES-W1168A                                      
019500          ELSE                                                            
019600             PERFORM S01-LAES-W11679                                      
019700          END-IF                                                          
019800       END-IF                                                             
019900     END-PERFORM                                                          
020000                                                                          
020100     PERFORM Z-FINIT                                                      
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN INPUT  W11679                                                   
020900                 W1168A                                                   
021000     OPEN OUTPUT W11688                                                   
021100                                                                          
021200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021300     .                                                                    
021400     EJECT                                                                
021500 B-BEARBETA SECTION.                                                      
021600                                                                          
021700     MOVE IN-KDERS        TO WS-KDERS-NUM                                 
021800     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
021900                             W-IDARTNR                                    
022000     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
022100     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
022200     MOVE WS-IDARTNR      TO UT-IDARTNR20                                 
022300                                                                          
022400     PERFORM IMS-GU-WDK601                                                
022500     IF SEGMENT-FINNS                                                     
022600        MOVE ART-TIERSDAT TO DAT-I-TIDATUM                                
022700        MOVE 'AAVVD'      TO DAT-KDDATFORM                                
022800        PERFORM S02-WDATKONV                                              
022900                                                                          
023000        IF DAT-KDSVAR-OK                                                  
023100           MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002                           
023200        ELSE                                                              
023300           MOVE 0            TO UT-TIERSDAT-002                           
023400        END-IF                                                            
023500        MOVE ART-KDPRODSL    TO UT-KDPRODSL                               
023510                                TEST-KDPRODSL                             
023600     ELSE                                                                 
023700        MOVE 0               TO UT-TIERSDAT-002                           
023800        MOVE 0               TO UT-KDPRODSL                               
023810                                TEST-KDPRODSL                             
023900     END-IF                                                               
024000                                                                          
024010     IF NOT KDPRODSL-LYNK                                                 
024100       MOVE 'N'      TO UT-FLDEL                                          
024200       MOVE IN-KDERS TO UT-KDERS                                          
024300       IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR               
024400                         '7' OR '8'                                       
024500          MOVE 1     TO UT-KDARTUTG                                       
024600       ELSE                                                               
024700          IF WS-KDERSPOS2 = '2' OR '5'                                    
024800             MOVE 2  TO UT-KDARTUTG                                       
024900          END-IF                                                          
025000       END-IF                                                             
025100       IF IN-KDERS > 20                                                   
025200         MOVE 'F'      TO UT-KDUTGSTA                                     
025300       ELSE                                                               
025400         MOVE 'P'      TO UT-KDUTGSTA                                     
025500       END-IF                                                             
025600                                                                          
025700       PERFORM IMS-GU-WDD701                                              
025800       IF SEGMENT-FINNS                                                   
025900          IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                      
026000             IF ERSA-KVKORT = 1                                           
026100                MOVE 'S' TO UT-KDUTGSTR                                   
026200             ELSE                                                         
026300                MOVE 'M' TO UT-KDUTGSTR                                   
026400             END-IF                                                       
026500           ELSE                                                           
026600             IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                   
026700                MOVE 'V' TO UT-KDUTGSTR                                   
026800             END-IF                                                       
026900           END-IF                                                         
027000                                                                          
027100           PERFORM IMS-GNP-WDD704                                         
027200           IF SEGMENT-FINNS                                               
027300              IF IN-KDERS > 20                                            
027400                 CONTINUE                                                 
027500              ELSE                                                        
027600                 IF IN-KDERS > 10 AND < 20                                
027700                    MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM           
027800                    MOVE 'AAVVD' TO DAT-KDDATFORM                         
027900                    PERFORM S02-WDATKONV                                  
028000                    IF DAT-KDSVAR-OK                                      
028100                       MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002               
028200                    ELSE                                                  
028300                       MOVE 0            TO UT-TIERSDAT-002               
028400                    END-IF                                                
028500                 ELSE                                                     
028600                    IF IN-KDERS > 0 AND < 10                              
028700                       MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM            
028800                       MOVE 'AAVVD'      TO DAT-KDDATFORM                 
028900                       PERFORM S02-WDATKONV                               
029000                       IF DAT-KDSVAR-OK                                   
029100                          MOVE DAT-TIAAMMDD TO UT-TIERSDAT-002            
029200                       ELSE                                               
029300                          MOVE 0            TO UT-TIERSDAT-002            
029400                       END-IF                                             
029500                    END-IF                                                
029600                 END-IF                                                   
029700              END-IF                                                      
029800           END-IF                                                         
029900                                                                          
030000           IF IN-KDERS = 29 OR 52 OR 09 OR 19                             
030100             MOVE 0        TO UT-IDKORTNR                                 
030200             MOVE 0        TO UT-KDTEXTGR                                 
030300             MOVE 'N'      TO UT-FLTEXT                                   
030400             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
030500             MOVE 0        TO UT-DIERS-TILLK                              
030600             MOVE SPACE    TO UT-KDUTGSTR                                 
030700             PERFORM S03-SKRIV-W11688                                     
030800           ELSE                                                           
030900             PERFORM IMS-GNP-WDD702-FIRST                                 
031000             IF SEGMENT-FINNS                                             
031100               MOVE 1   TO WS-KDTEXTGR-RAKNARE                            
031200               MOVE 'N' TO WS-FL-TYP1                                     
031300                                                                          
031400               PERFORM UNTIL SEGMENT-SAKNAS                               
031500                                                                          
031600                 MOVE ERSA-IDKORTNR TO UT-IDKORTNR                        
031700                                                                          
031800                 IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'        
031900                   MOVE 0        TO UT-KDTEXTGR                           
032000                 ELSE                                                     
032100                   IF ERSA-FLTEXT = 'N'                                   
032200                     MOVE 'J'  TO WS-FL-TYP1                              
032300                   END-IF                                                 
032400                   IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'              
032500                     ADD  +1   TO WS-KDTEXTGR-RAKNARE                     
032600                     MOVE 'N'  TO WS-FL-TYP1                              
032700                   END-IF                                                 
032800                   MOVE WS-KDTEXTGR-RAKNARE TO UT-KDTEXTGR                
032900                 END-IF                                                   
033000                                                                          
033100                 MOVE ERSA-FLTEXT     TO UT-FLTEXT                        
033200                 IF ERSA-FLTEXT = 'N'                                     
033300                   MOVE ERSA-IDARTNR-TILLK TO WS-IDARTNR-NUM              
033400                   MOVE WS-IDARTNR-NUM                                    
033500                              TO WS-IDARTNR-TILLK-ALFA                    
033600                   INSPECT WS-IDARTNR-TILLK-ALFA                          
033700                              REPLACING LEADING ZERO BY SPACE             
033800                   MOVE WS-IDARTNR-TILLK TO UT-IDARTNR20-TILLK            
033900                   MOVE ERSA-DIERS-TILLK TO UT-DIERS-TILLK                
034000                 ELSE                                                     
034100                   IF ERSA-FLTEXT = 'J'                                   
034200                     MOVE ERSA-BEERS  TO UT-IDARTNR20-TILLK               
034300                     MOVE  0     TO UT-DIERS-TILLK                        
034400                    END-IF                                                
034500                 END-IF                                                   
034600                                                                          
034700                 PERFORM S03-SKRIV-W11688                                 
034800                 PERFORM IMS-GNP-WDD702                                   
034900               END-PERFORM                                                
035000             END-IF                                                       
035100          END-IF                                                          
035200       ELSE                                                               
035300          IF IN-KDERS = 29 OR 52                                          
035400             MOVE 0        TO UT-IDKORTNR                                 
035500             MOVE 0        TO UT-KDTEXTGR                                 
035600             MOVE 'N'      TO UT-FLTEXT                                   
035700             MOVE SPACE    TO UT-IDARTNR20-TILLK                          
035800             MOVE 0        TO UT-DIERS-TILLK                              
035900             MOVE SPACE    TO UT-KDUTGSTR                                 
036000                                                                          
036100             PERFORM S03-SKRIV-W11688                                     
036200          END-IF                                                          
036300       END-IF                                                             
036310     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 Z-FINIT SECTION.                                                         
036700                                                                          
036800     CLOSE W11679                                                         
036900           W11688                                                         
037000           W1168A                                                         
037100     MOVE 'S'     TO POSTSUM-OPKOD                                        
037200     CALL POSTSUM USING POSTSUM-PARM                                      
037300     .                                                                    
037400     EJECT                                                                
037500 S01-LAES-W11679 SECTION.                                                 
037600                                                                          
037700     READ W11679 INTO IN-AREA                                             
037800     AT END                                                               
037900        SET END-OF-W11679 TO TRUE                                         
038000        MOVE 999999999 TO IN-IDARTNR                                      
038100                                                                          
038200     NOT AT END                                                           
038300        MOVE 'W11679'     TO    POSTSUM-FDNAMN                            
038400        MOVE 'W11688D1'   TO    POSTSUM-DDNAMN2                           
038410        MOVE IN-IDLANDX2  TO    POSTSUM-TRANSTYP                          
038500        CALL POSTSUM      USING POSTSUM-PARM                              
038600     END-READ                                                             
038700     .                                                                    
038800     EJECT                                                                
038900 S02-WDATKONV SECTION.                                                    
039000     SKIP2                                                                
039100     CALL WDATKONV USING DAT-KDDATFORM                                    
039200                         DAT-I-TIDATUM                                    
039300                         DAT-O-TIDATUM                                    
039400                         DAT-KDSVAR                                       
039500     .                                                                    
039600     EJECT                                                                
039700 S03-SKRIV-W11688 SECTION.                                                
039710                                                                          
039720     IF IN8A-IDLANDX2 NOT = WS-MQ-IDLANDX2                                
039730       WRITE UT-POST           FROM WS-MQ-LINE1                           
039731       MOVE SPACE                TO UT-POST                               
039732       WRITE UT-POST           FROM WS-MQ-LINE2                           
039733       MOVE SPACE                TO UT-POST                               
039750       MOVE IN8A-IDLANDX2        TO WS-MQ-IDLANDX2                        
039760       WRITE UT-POST           FROM WS-MQ-LINE3                           
039761       MOVE SPACE                TO UT-POST                               
039770     END-IF                                                               
039800                                                                          
039900     MOVE 'WB1'          TO UT-IDPTYP                                     
040000     WRITE UT-POST       FROM UT-AREA                                     
040100     MOVE 'W11688 '      TO POSTSUM-FDNAMN                                
040200     MOVE 'W11688D3'     TO POSTSUM-DDNAMN2                               
040210     MOVE IN8A-IDLANDX2  TO POSTSUM-TRANSTYP                              
040300     CALL POSTSUM        USING POSTSUM-PARM                               
040400     .                                                                    
040500     SKIP3                                                                
040600 S04-LAES-W1168A SECTION.                                                 
040700                                                                          
040800     READ W1168A INTO IN8A-AREA                                           
040900     AT END                                                               
041000        MOVE 999999999 TO IN8A-IDARTNR                                    
041100        SET END-OF-W1168A TO TRUE                                         
041200                                                                          
041300     NOT AT END                                                           
041400        MOVE 'W1168A'       TO POSTSUM-FDNAMN                             
041500        MOVE 'W11688D2'     TO POSTSUM-DDNAMN2                            
041510        MOVE IN8A-IDLANDX2  TO POSTSUM-TRANSTYP                           
041600        CALL POSTSUM USING POSTSUM-PARM                                   
041700     END-READ                                                             
041800     .                                                                    
041900     EJECT                                                                
042000* --- IMS SEKTIONER ---                                                   
042100     SKIP3                                                                
042200 IMS-GU-WDK601 SECTION.                                                   
042300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
042400          DELIMITED BY SIZE INTO SSA1                                     
042500     MOVE '  GE' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
042700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     .                                                                    
043000     SKIP3                                                                
043100 IMS-GU-WDD701 SECTION.                                                   
043200     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
043300          DELIMITED BY SIZE INTO SSA1                                     
043400     MOVE '  GE' TO GODK-STATUSKODER                                      
043500     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
043600     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900     SKIP3                                                                
044000 IMS-GNP-WDD702 SECTION.                                                  
044100     STRING 'WDD702   '                                                   
044200          DELIMITED BY SIZE INTO SSA1                                     
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
044500     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800     EJECT                                                                
044900 IMS-GNP-WDD704 SECTION.                                                  
045000     STRING 'WDD704   '                                                   
045100          DELIMITED BY SIZE INTO SSA1                                     
045200     MOVE '  GE' TO GODK-STATUSKODER                                      
045300     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD704 SSA1                   
045400     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700     SKIP3                                                                
045800 IMS-GNP-WDD702-FIRST SECTION.                                            
045900     MOVE 'WDD702  *F' TO SSA1                                            
046000     MOVE '  GE' TO GODK-STATUSKODER                                      
046100     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
046200     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     .                                                                    
046500     EJECT                                                                
046600 IMS-STATUSKONTROLL SECTION.                                              
046700     SKIP2                                                                
046800     SET STATUS-IX TO 1                                                   
046900     SEARCH GODK-STATUS                                                   
047000       AT END                                                             
047100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047200           DELIMITED BY SIZE INTO FELTEXT                                 
047300         DISPLAY FELTEXT                                                  
047400         CALL FELLOG                                                      
047500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047600         CONTINUE                                                         
047700     END-SEARCH                                                           
047800     .                                                                    
