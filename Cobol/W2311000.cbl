000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W23110.                                              
000300 AUTHOR.             P-O WOLLHAG - MULTIDATA.                             
000400 DATE-WRITTEN.       DEC-78.                                              
000500     REMARKS.                                                             
000600*        PROGRAMMET FRAMSTÄLLER STATISTIKINFORMATION                      
000700*        FRÅN INLEVERANS - HISTORIKREGISTRET.                             
000800*                                                                         
000900*        ÄNDRAT FRÅN FSU,OS/VS-COBOL TILL SB,COBOL-II GUNNEL E.           
001000*                                                                         
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400         SELECT STAT-FILE ASSIGN TO UT-S-W23110D1.                        
001500     EJECT                                                                
001600 DATA DIVISION.                                                           
001700 FILE SECTION.                                                            
001800 FD      STAT-FILE                                                        
001900         LABEL RECORD STANDARD                                            
002000         RECORDING MODE F                                                 
002100         BLOCK CONTAINS 0 RECORDS.                                        
002200*                                                                         
002300*01  UT-REC -COPY W231229 -L.                                             
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 01  WS-IDARTNR          PIC S9(9)                   COMP-3.              
002900 01  WS-DAINLEV          PIC 9(16).                                       
003000                                                                          
003100 01  WS-IDKONTO1         PIC 9(11).                                       
003200 01  WS-IDKONTO2 REDEFINES WS-IDKONTO1.                                   
003300     10      FILLER          PIC 9.                                       
003400     10      WS-KTO-SIFF1    PIC 9(1).                                    
003500     10      FILLER          PIC 9(9).                                    
003600                                                                          
003700 01  FILLER              PIC X(8)        VALUE 'DATUM   '.                
003800 01  TIAA-IDAG           PIC 9(2)        VALUE ZERO.                      
003910 01  TIRP-IDAG           PIC 9(2)        VALUE ZERO.                      
004000 01  WS-TIDATUM          PIC 9(16)       VALUE ZERO.                      
004100 01  FILLER REDEFINES WS-TIDATUM.                                         
004200     03  FILLER          PIC 9(2).                                        
004300     03  WS-TIAAMMDD     PIC 9(6).                                        
004400     03  FILLER          PIC 9(8).                                        
004500 01  WS-AAMMDD.                                                           
004600     05  WS-AA           PIC 9(2)        VALUE ZERO.                      
004700     05  WS-MM           PIC 9(2)        VALUE ZERO.                      
004800     05  WS-DD           PIC 9(2)        VALUE ZERO.                      
004900                                                                          
005000 01  FELLOGG             PIC X(8)        VALUE 'FELLOGG '.                
005100 01  WDATKONV            PIC X(8)        VALUE 'WDATKONV'.                
005200 01  DATKORT             PIC X(8)        VALUE 'DATKORT '.                
005300 01  CBLTDLI             PIC X(8)        VALUE 'CBLTDLI '.                
005400 01  FELLOG              PIC X(8)        VALUE 'FELLOG  '.                
005500*                                                                         
005600 01  DATUM-OK            PIC X(1).                                        
005700 01  JA                  PIC X(1)        VALUE 'J'.                       
005800 01  NEJ                 PIC X(1)        VALUE 'N'.                       
005900     EJECT                                                                
006000 01  PARAM-DATKORT.                                                       
006100     05  KALLANDE-PGM    PIC X(6)        VALUE 'W23110'.                  
006200     05  SOEK-ID         PIC X(6)        VALUE 'WDATUM'.                  
006300     05  MOTTAGANDE-FLT.                                                  
006400*        10    -COPY WDATKORT                                             
006600     EJECT                                                                
006700*    -COPY WDATAREA                                                       
006900     EJECT                                                                
007000*    -COPY W231229 -PRE STAT-.                                            
007200     EJECT                                                                
007300 01  IMS-WS.                                                              
007400     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
007500*----------------------------------STATUSKODER FRÅN IMS                   
007600     03  STATUS-WS   PIC XX.                                              
007700         88  SEGMENT-FINNS       VALUE '  '.                              
007800         88  SEGMENT-SLUT        VALUE 'GB'.                              
007900                                                                          
008000     03  GODK-STATUSKODER.                                                
008100      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
008200                                                                          
008300     EJECT                                                                
008400*----------------------------------IMS-CALL FUNKTIONER                    
008500*01              -COPY W0003                                              
008700     EJECT                                                                
008800 01  FILLER          PIC X(16)   VALUE 'DL1-IO-AREA'.                     
008900                                                                          
009000 01  DL1-IO-AREA         PIC X(150).                                      
009100*                                                                         
009200*01  WDL201 -COPY WDL201 -RED DL1-IO-AREA                                 
009400     EJECT                                                                
009500*01  WDL211 -COPY WDL211 -RED DL1-IO-AREA                                 
009700     EJECT                                                                
009800*01  WDL221 -COPY WDL221 -RED DL1-IO-AREA                                 
010000     EJECT                                                                
010100*01  WDL222 -COPY WDL222 -RED DL1-IO-AREA                                 
010300     EJECT                                                                
010400*01  WDL223 -COPY WDL223 -RED DL1-IO-AREA                                 
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800*01    -COPY W0008         -PRE WDL2-                                     
011000          05  FILLER      PIC   XX.                                       
011100     EJECT                                                                
011200 PROCEDURE DIVISION USING WDL2-PCB.                                       
011300     ENTRY 'CBLTDLI'  USING WDL2-PCB.                                     
011400     PERFORM AB-INIT                                                      
011500     PERFORM IMS-GET-WDL2                                                 
011600     PERFORM UNTIL SEGMENT-SLUT                                           
011700         EVALUATE WDL2-SEG-NAME-FB                                        
011800           WHEN  'WDL201  '                                               
011900                     MOVE ART-IDARTNR TO WS-IDARTNR                       
012000           WHEN  'WDL211  '                                               
012100                     MOVE INL-DAINLEV TO WS-DAINLEV                       
012200           WHEN  'WDL221  '                                               
012300                     PERFORM AC-BEHANDLA-R32                              
012400           WHEN  'WDL222  '                                               
012500                     PERFORM AD-BEHANDLA-R33-R34                          
012600           WHEN  'WDL223  '                                               
012700                     PERFORM AE-BEHANDLA-R40                              
012800         END-EVALUATE                                                     
012900         PERFORM IMS-GET-WDL2                                             
013000     END-PERFORM                                                          
013100     CLOSE STAT-FILE                                                      
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 AB-INIT SECTION.                                                         
013700                                                                          
013800********************************************                              
013900*    LÄSNING AV DATUM - OPEN STAT-FILE     *                              
014000********************************************                              
014100                                                                          
014200     MOVE '229' TO STAT-IDPTYP                                            
014300     CALL DATKORT USING KALLANDE-PGM                                      
014400                        SOEK-ID                                           
014500                        MOTTAGANDE-FLT                                    
014600                                                                          
014700     MOVE D-AAR                 TO WS-AA                                  
014800     MOVE D-MAANAD              TO WS-MM                                  
014900     MOVE D-DAG                 TO WS-DD                                  
015000                                                                          
015100     MOVE WS-AAMMDD             TO DAT-I-TIDATUM                          
015200     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
015300                                                                          
015400     CALL WDATKONV USING           DAT-KDDATFORM                          
015500                                   DAT-I-TIDATUM                          
015600                                   DAT-O-TIDATUM                          
015700                                   DAT-KDSVAR                             
015800     IF DAT-KDSVAR-OK                                                     
015900         MOVE DAT-TIAA    TO TIAA-IDAG                                    
016000         MOVE DAT-TIRP    TO TIRP-IDAG                                    
016100     ELSE                                                                 
016200         CALL FELLOGG                                                     
016300     END-IF                                                               
016400     OPEN OUTPUT STAT-FILE                                                
016500     EJECT                                                                
016600**************************                                                
016700*    SKAPA STATISTIKPOST *                                                
016800**************************                                                
016900                                                                          
017000     .                                                                    
017100 AC-BEHANDLA-R32   SECTION.                                               
017200                                                                          
017300     IF MOT-IDPTYP = 'R32'                                                
017400         PERFORM ACA-KOLLA-DATUM                                          
017500         IF DATUM-OK = JA                                                 
017600             PERFORM ACB-SKAPA-R32-STAT                                   
017700         END-IF                                                           
017800     END-IF                                                               
017900                                                                          
018000                                                                          
018100     .                                                                    
018200 ACA-KOLLA-DATUM   SECTION.                                               
018300                                                                          
018400     MOVE NEJ TO DATUM-OK                                                 
018500     MOVE MOT-TIUPPDAT       TO DAT-I-TIDATUM                             
018600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
018700     CALL WDATKONV USING        DAT-KDDATFORM                             
018800                                DAT-I-TIDATUM                             
018900                                DAT-O-TIDATUM                             
019000                                DAT-KDSVAR                                
019100                                                                          
019200     IF DAT-KDSVAR-OK                                                     
019300         IF DAT-TIAA = TIAA-IDAG AND DAT-TIRP = TIRP-IDAG                 
019400             MOVE JA TO DATUM-OK                                          
019500         END-IF                                                           
019600     ELSE                                                                 
019700         MOVE NEJ TO DATUM-OK                                             
019800     END-IF                                                               
019900     EJECT                                                                
020000*******************                                                       
020100*    SKAPA R32-STAT                                                       
020200*******************                                                       
020300                                                                          
020400     .                                                                    
020500 ACB-SKAPA-R32-STAT  SECTION.                                             
020600                                                                          
020700     IF MOT-KVANTMOT NOT = ZERO                                           
020800        MOVE WS-IDARTNR TO STAT-IDARTNR                                   
020900        MOVE +1           TO STAT-KDCLAGER                                
021000        IF MOT-KDRT = +88 OR +99                                          
021100           MOVE '2' TO STAT-KDUPPD                                        
021200           MOVE MOT-KVANTMOT TO STAT-KVANTAL                              
021300        ELSE                                                              
021400           MOVE '1' TO STAT-KDUPPD                                        
021500           COMPUTE STAT-KVANTAL = MOT-KVANTMOT - MOT-KVFORDEL             
021600        END-IF                                                            
021700        PERFORM S01-RORELSE                                               
021800        WRITE UT-REC FROM STAT-W231229                                    
021900        IF MOT-KVRETUR NOT = ZERO                                         
022000            MOVE MOT-KVRETUR TO STAT-KVANTAL                              
022100            MOVE +6 TO STAT-KDRORELS                                      
022200            MOVE '2' TO STAT-KDUPPD                                       
022300            WRITE UT-REC FROM STAT-W231229                                
022400        END-IF                                                            
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 AD-BEHANDLA-R33-R34  SECTION.                                            
022900                                                                          
023000     PERFORM ADA-KOLLA-DATUM                                              
023100     IF DATUM-OK = JA                                                     
023200         IF DIR-IDPTYP = 'R33'                                            
023300             PERFORM ADB-SKAPA-R33-STAT                                   
023400         ELSE                                                             
023500             IF DIR-IDPTYP = 'R34'                                        
023600                PERFORM ADC-SKAPA-R34-STAT                                
023700             END-IF                                                       
023800         END-IF                                                           
023900     END-IF                                                               
024000                                                                          
024100     .                                                                    
024200 ADA-KOLLA-DATUM   SECTION.                                               
024300                                                                          
024400     MOVE NEJ TO DATUM-OK                                                 
024500     COMPUTE WS-TIDATUM = 9999999999999999 - WS-DAINLEV                   
024600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
024700     MOVE WS-TIAAMMDD        TO DAT-I-TIDATUM                             
024800     CALL WDATKONV USING        DAT-KDDATFORM                             
024900                                DAT-I-TIDATUM                             
025000                                DAT-O-TIDATUM                             
025100                                DAT-KDSVAR                                
025200                                                                          
025300     IF DAT-KDSVAR-OK                                                     
025400         IF DAT-TIAA = TIAA-IDAG AND DAT-TIRP = TIRP-IDAG                 
025500             MOVE JA TO DATUM-OK                                          
025600         END-IF                                                           
025700     ELSE                                                                 
025800         MOVE NEJ TO DATUM-OK                                             
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 ADB-SKAPA-R33-STAT  SECTION.                                             
026300     MOVE WS-IDARTNR TO STAT-IDARTNR                                      
026400     MOVE +1           TO STAT-KDCLAGER                                   
026500     MOVE '1' TO STAT-KDUPPD                                              
026600     MOVE +3 TO STAT-KDRORELS                                             
026700     MOVE DIR-KVAVIS TO STAT-KVANTAL                                      
026800     WRITE UT-REC FROM STAT-W231229                                       
026900     .                                                                    
027000     EJECT                                                                
027100 ADC-SKAPA-R34-STAT  SECTION.                                             
027200     MOVE WS-IDARTNR TO STAT-IDARTNR                                      
027300     MOVE +1           TO STAT-KDCLAGER                                   
027400     MOVE '1' TO STAT-KDUPPD                                              
027500     MOVE DIR-KVAVIS TO STAT-KVANTAL                                      
027600     IF DIR-KDRT < +6                                                     
027700         MOVE +3 TO STAT-KDRORELS                                         
027800     ELSE                                                                 
027900         MOVE +4 TO STAT-KDRORELS                                         
028000     END-IF                                                               
028100     WRITE UT-REC FROM STAT-W231229                                       
028200     .                                                                    
028300     EJECT                                                                
028400 AE-BEHANDLA-R40      SECTION.                                            
028500                                                                          
028600     PERFORM AEA-KOLLA-DATUM                                              
028700     IF DATUM-OK = JA                                                     
028800         PERFORM AEB-SKAPA-R40-STAT                                       
028900     END-IF                                                               
029000                                                                          
029100     .                                                                    
029200 AEA-KOLLA-DATUM   SECTION.                                               
029300                                                                          
029400     MOVE NEJ TO DATUM-OK                                                 
029500     COMPUTE WS-TIDATUM = 9999999999999999 - WS-DAINLEV                   
029600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
029700     MOVE WS-TIAAMMDD        TO DAT-I-TIDATUM                             
029800     CALL WDATKONV USING        DAT-KDDATFORM                             
029900                                DAT-I-TIDATUM                             
030000                                DAT-O-TIDATUM                             
030100                                DAT-KDSVAR                                
030200                                                                          
030300     IF DAT-KDSVAR-OK                                                     
030400         IF DAT-TIAA = TIAA-IDAG AND DAT-TIRP = TIRP-IDAG                 
030500             MOVE JA TO DATUM-OK                                          
030600         END-IF                                                           
030700     ELSE                                                                 
030800         MOVE NEJ TO DATUM-OK                                             
030900     END-IF                                                               
031000     EJECT                                                                
031100                                                                          
031200     .                                                                    
031300 AEB-SKAPA-R40-STAT  SECTION.                                             
031400     MOVE WS-IDARTNR TO STAT-IDARTNR                                      
031500     MOVE +1           TO STAT-KDCLAGER                                   
031600     MOVE '2' TO STAT-KDUPPD                                              
031700     MOVE +5 TO STAT-KDRORELS                                             
031800     MOVE RET-KVRETUR TO STAT-KVANTAL                                     
031900     WRITE UT-REC FROM STAT-W231229                                       
032000     EJECT                                                                
032100****************************************                                  
032200*    BESTÄM RÖRELSETYP FÖR R32         *                                  
032300****************************************                                  
032400                                                                          
032500     .                                                                    
032600 IMS-GET-WDL2 SECTION.                                                    
032700     SKIP3                                                                
032800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
032900     CALL CBLTDLI USING GN WDL2-PCB DL1-IO-AREA                           
033000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300 IMS-STATUSKONTROLL SECTION.                                              
033400     SKIP3                                                                
033500     SET STATUS-IX TO 1                                                   
033600     SEARCH GODK-STATUS AT END CALL FELLOG                                
033700       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
033800       CONTINUE                                                           
033900     END-SEARCH                                                           
034000     .                                                                    
034100 S01-RORELSE SECTION.                                                     
034200     IF MOT-KDRT < +6                                                     
034300         MOVE +3 TO STAT-KDRORELS                                         
034400     ELSE                                                                 
034500         IF MOT-KDRT = +88 OR +99                                         
034600             MOVE +2 TO STAT-KDRORELS                                     
034700         ELSE                                                             
034800            IF MOT-KDRT = +8                                              
034900                MOVE MOT-IDKONTO TO WS-IDKONTO1                           
035000                IF WS-KTO-SIFF1 = 7                                       
035100                    MOVE +2 TO STAT-KDRORELS                              
035200                ELSE                                                      
035300                    MOVE +3 TO STAT-KDRORELS                              
035400                END-IF                                                    
035500            ELSE                                                          
035600                MOVE +4 TO STAT-KDRORELS                                  
035700            END-IF                                                        
035800         END-IF                                                           
035900     END-IF                                                               
036000     EJECT                                                                
036100     .                                                                    
