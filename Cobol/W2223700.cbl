000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2223700.                                                
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
002500     SELECT W2223701                   ASSIGN TO W22237D1.                
002600*          --- FILE TO UPDATE WDK611                                      
002700     SELECT W2223702                   ASSIGN TO W22237D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W2223701                                                             
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  RECORD -COPY W2223701 -PRE  OUT1-  -L.                               
003800     EJECT                                                                
003900 FD  W2223702                                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  RECORD -COPY W2223702 -PRE  OUT2-  -L.                               
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2223700'.            
004800 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004900 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  PLAN-IX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
005300 77  PLAN-IX-MAX                 PIC S9(4)   COMP SYNC VALUE +12.         
005400     EJECT                                                                
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
006800     SKIP2                                                                
006900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  ERROR-TEXT.                                                          
007600     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300*------------------------------------- PARAMETRAR TILL KVPB-PLAN          
008400*01  -COPY W222PBTO                                                       
008500     EJECT                                                                
008600 01  OUT-AREA-START1             PIC X(24)   VALUE                        
008700                                 'OUT-AREA-START1 '.                      
008800                                                                          
008900*01  AREA -COPY W2223701   -PRE OUT1-                                     
009000     EJECT                                                                
009100*    --- AREAS FOR IMS-SECTIONS                                           
009200*                                                                         
009300     EJECT                                                                
009400 01  OUT-AREA-START2             PIC X(24)   VALUE                        
009500                                 'OUT-AREA-START2 '.                      
009600                                                                          
009700*01  AREA -COPY W2223702   -PRE OUT2-                                     
009800     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-FOR-DLI.                                                        
010500     03  W-WDK6H1KY-MIN-X.                                                
010600       05  W-IDDC-REF-MIN      PIC X(02)     VALUE LOW-VALUE.             
010700       05  FILLER              PIC X(05)     VALUE LOW-VALUE.             
010800     03  W-WDK6H1KY-MAX-X.                                                
010900       05  W-IDDC-REF-MAX      PIC X(02)     VALUE HIGH-VALUE.            
011000       05  FILLER              PIC X(05)     VALUE HIGH-VALUE.            
011100                                                                          
011200     03  W-IDARTNR-X.                                                     
011300         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
011400                                                                          
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FOUND                       VALUE '  '.                  
011800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012000     88  SEGMENT-END                         VALUE 'GB'.                  
012100     SKIP2                                                                
012200 01  GOOD-STATUSCODES.                                                    
012300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNCTION CODES                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6H1'.                      
013300 01  DLI-IO-WDK6H1.                                                       
013400*    03  -COPY WDK6H1                                                     
013500     EJECT                                                                
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013700 01  DLI-IO-WDK611.                                                       
013800*    03  -COPY WDK611                                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0008  -PRE K6H1-                                              
014300     05  FILLER                    PIC X.                                 
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE WDK6-                                              
014600     05  FILLER                    PIC X.                                 
014700     EJECT                                                                
014800 01  PBTO-WDK6-PCB                 PIC X.                                 
014900 01  PBTO-WDK7-PCB                 PIC X.                                 
015000 01  PBTO-ARTM-PCB                 PIC X.                                 
015100 01  PBTO-2501-PCB                 PIC X.                                 
015200 01  PBTO-WDB6R-PCB                PIC X.                                 
015300 01  PBTO-WDK7R-PCB                PIC X.                                 
015400 01  PBTO-WDB6-PCB                 PIC X.                                 
015500 01  PBTO-WDD7A-PCB                PIC X.                                 
015600 01  PBTO-WDK7E-PCB                PIC X.                                 
015700 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
015800 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
015900 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
016000 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
016100 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
016200 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
016300 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
016400 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
016500                                                                          
016600 PROCEDURE DIVISION  USING K6H1-PCB WDK6-PCB                              
016700           PBTO-WDK6-PCB  PBTO-WDK7-PCB                                   
016800           PBTO-ARTM-PCB  PBTO-2501-PCB PBTO-WDB6R-PCB                    
016900           PBTO-WDK7R-PCB PBTO-WDB6-PCB                                   
017000           PBTO-WDD7A-PCB PBTO-WDK7E-PCB                                  
017100           PBTO-W222-UTIL-WDK6-PCB                                        
017200           PBTO-W222-UTIL-WDK7-PCB                                        
017300           PBTO-W222-UTIL-WDB6-PCB                                        
017400           PBTO-W222-UTUP-WDK7-PCB                                        
017500           PBTO-W222-UTUP-WDB6-PCB                                        
017600           PBTO-W222-UTUP-UTIL-WDK6-PCB                                   
017700           PBTO-W222-UTUP-UTIL-WDK7-PCB                                   
017800           PBTO-W222-UTUP-UTIL-WDB6-PCB                                   
017900           .                                                              
018000 MAIN SECTION.                                                            
018100     ENTRY 'DLITCBL' USING K6H1-PCB WDK6-PCB                              
018200           PBTO-WDK6-PCB  PBTO-WDK7-PCB                                   
018300           PBTO-ARTM-PCB  PBTO-2501-PCB PBTO-WDB6R-PCB                    
018400           PBTO-WDK7R-PCB PBTO-WDB6-PCB                                   
018500           PBTO-WDD7A-PCB PBTO-WDK7E-PCB                                  
018600           PBTO-W222-UTIL-WDK6-PCB                                        
018700           PBTO-W222-UTIL-WDK7-PCB                                        
018800           PBTO-W222-UTIL-WDB6-PCB                                        
018900           PBTO-W222-UTUP-WDK7-PCB                                        
019000           PBTO-W222-UTUP-WDB6-PCB                                        
019100           PBTO-W222-UTUP-UTIL-WDK6-PCB                                   
019200           PBTO-W222-UTUP-UTIL-WDK7-PCB                                   
019300           PBTO-W222-UTUP-UTIL-WDB6-PCB                                   
019400           .                                                              
019500                                                                          
019600                                                                          
019700     PERFORM A-INIT                                                       
019800                                                                          
019900     PERFORM IMS-GN-WDK6H1                                                
020000     PERFORM UNTIL SEGMENT-MISSING                                        
020100                                                                          
020200       PERFORM B-CALCULATE-PB-PLAN                                        
020300                                                                          
020400       PERFORM C-CALCULATE-PB-TPO                                         
020500                                                                          
020600       PERFORM IMS-GN-WDK6H1                                              
020700                                                                          
020800     END-PERFORM                                                          
020900                                                                          
021000                                                                          
021100     PERFORM Z-FINIT                                                      
021200                                                                          
021300     MOVE ZERO TO RETURN-CODE                                             
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800     MOVE 'A-INIT                  ' TO CURRENT-SECTION                   
021900                                                                          
022000     OPEN OUTPUT W2223701                                                 
022100                 W2223702                                                 
022200                                                                          
022300     ACCEPT TODAYS-DATE  FROM DATE                                        
022400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022500     .                                                                    
022600     EJECT                                                                
022700 B-CALCULATE-PB-PLAN SECTION.                                             
022800     MOVE 'B-CALCULATE-PB-PLAN     ' TO CURRENT-SECTION                   
022900                                                                          
023000     MOVE SEQH-IDARTNR         TO W-IDARTNR                               
023100     PERFORM IMS-GU-WDK611                                                
023200     IF SEGMENT-FOUND                                                     
023300       IF CLAG-KDERS > +20                                                
023400         PERFORM BA-INIT-KVPB-PLAN                                        
023500       ELSE                                                               
023600         MOVE SEQH-IDARTNR     TO PBTO-IDARTNR                            
023700         CALL W222PBTO USING PBTO-W222PBTO                                
023800                             PBTO-WDK6-PCB                                
023900                             PBTO-WDK7-PCB                                
024000                             PBTO-ARTM-PCB                                
024100                             PBTO-2501-PCB                                
024200                             PBTO-WDB6R-PCB                               
024300                             PBTO-WDK7R-PCB                               
024400                             PBTO-WDB6-PCB                                
024500                             PBTO-WDD7A-PCB                               
024600                             PBTO-WDK7E-PCB                               
024700                             PBTO-W222-UTIL-WDK6-PCB                      
024800                             PBTO-W222-UTIL-WDK7-PCB                      
024900                             PBTO-W222-UTIL-WDB6-PCB                      
025000                             PBTO-W222-UTUP-WDK7-PCB                      
025100                             PBTO-W222-UTUP-WDB6-PCB                      
025200                             PBTO-W222-UTUP-UTIL-WDK6-PCB                 
025300                             PBTO-W222-UTUP-UTIL-WDK7-PCB                 
025400                             PBTO-W222-UTUP-UTIL-WDB6-PCB                 
025500                                                                          
025600                                                                          
025700         IF PBTO-KDSVAR = YES                                             
025800           MOVE +1             TO PLAN-IX                                 
025900           PERFORM UNTIL PLAN-IX > PLAN-IX-MAX                            
026000             MOVE PBTO-RESEASON-PLAN (PLAN-IX)                            
026100                               TO OUT1-RESEASON-PLAN (PLAN-IX)            
026200             ADD +1            TO PLAN-IX                                 
026300           END-PERFORM                                                    
026400           MOVE PBTO-KVPB-PLAN TO OUT1-KVPB-PLAN                          
026500         ELSE                                                             
026600           PERFORM BA-INIT-KVPB-PLAN                                      
026700         END-IF                                                           
026800       END-IF                                                             
026900       MOVE SEQH-IDARTNR       TO OUT1-IDARTNR                            
027000       MOVE SEQH-IDDC-REF      TO OUT1-IDDC-REF                           
027100       PERFORM S11A-WRITE-W2223701                                        
027200     END-IF                                                               
027300                                                                          
027400     .                                                                    
027500     EJECT                                                                
027600 BA-INIT-KVPB-PLAN    SECTION.                                            
027700     MOVE 'BA-INIT-KVPB-PLAN       ' TO CURRENT-SECTION                   
027800                                                                          
027900     MOVE +1                   TO PLAN-IX                                 
028000     PERFORM UNTIL PLAN-IX > PLAN-IX-MAX                                  
028100       MOVE +1.00              TO OUT1-RESEASON-PLAN (PLAN-IX)            
028200       ADD +1                  TO PLAN-IX                                 
028300     END-PERFORM                                                          
028400     MOVE +0                   TO OUT1-KVPB-PLAN                          
028500     .                                                                    
028600     EJECT                                                                
028700                                                                          
028800 C-CALCULATE-PB-TPO SECTION.                                              
028900     MOVE 'C-CALCULATE-PB-TPO      ' TO CURRENT-SECTION                   
029000                                                                          
029100     MOVE SEQH-IDARTNR       TO OUT2-IDARTNR                              
029200     PERFORM S11B-WRITE-W2223702                                          
029300     .                                                                    
029400     EJECT                                                                
029500                                                                          
029600 Z-FINIT SECTION.                                                         
029700     MOVE 'Z-FINIT                 ' TO CURRENT-SECTION                   
029800                                                                          
029900     CLOSE W2223701                                                       
030000           W2223702                                                       
030100                                                                          
030200     MOVE 'S' TO POSTSUM-OPKOD                                            
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     EJECT                                                                
030600 S11A-WRITE-W2223701 SECTION.                                             
030700     MOVE 'S11A-WRITE-W2223701     ' TO CURRENT-SECTION                   
030800                                                                          
030900     WRITE OUT1-RECORD FROM OUT1-AREA                                     
031000                                                                          
031100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
031200     MOVE 'W2223701' TO POSTSUM-FDNAMN                                    
031300     MOVE 'W22237D1' TO POSTSUM-DDNAMN2                                   
031400     CALL POSTSUM USING POSTSUM-PARM                                      
031500     .                                                                    
031600                                                                          
031700 S11B-WRITE-W2223702 SECTION.                                             
031800     MOVE 'S11B-WRITE-W2223702     ' TO CURRENT-SECTION                   
031900                                                                          
032000     WRITE OUT2-RECORD FROM OUT2-AREA                                     
032100                                                                          
032200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
032300     MOVE 'W2223702' TO POSTSUM-FDNAMN                                    
032400     MOVE 'W22237D2' TO POSTSUM-DDNAMN2                                   
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     .                                                                    
032700     EJECT                                                                
032800 S99-ABEND SECTION.                                                       
032900     MOVE 'S99-ABEND               ' TO CURRENT-SECTION                   
033000                                                                          
033100     SKIP2                                                                
033200     MOVE 'S' TO POSTSUM-OPKOD                                            
033300     CALL POSTSUM USING POSTSUM-PARM                                      
033400     CALL ABEND USING RKOD-ABEND                                          
033500     .                                                                    
033600     EJECT                                                                
033700* --- IMS SECTIONS  ---                                                   
033800                                                                          
033900     EJECT                                                                
034000 IMS-GN-WDK6H1 SECTION.                                                   
034100     MOVE 'IMS-GN-WDK6H1    ' TO DBS-SECTION                              
034200                                                                          
034300     STRING 'WDK6H1  (WDK6H1KY=>' W-WDK6H1KY-MIN-X                        
034400                    '&WDK6H1KY<=' W-WDK6H1KY-MAX-X  ')'                   
034500            DELIMITED BY SIZE INTO SSA1                                   
034600     MOVE '  GE'              TO GOOD-STATUSCODES                         
034700     CALL CBLTDLI USING GN K6H1-PCB DLI-IO-WDK6H1 SSA1                    
034800     MOVE K6H1-STATUS-CODE    TO STATUS-WS                                
034900     PERFORM IMS-STATUSCHECK                                              
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-GU-WDK611       SECTION.                                             
035300     MOVE 'IMS-GU-WDK611    ' TO DBS-SECTION                              
035400                                                                          
035500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
035600          DELIMITED BY SIZE INTO SSA1                                     
035700     MOVE 'WDK611  '        TO SSA2                                       
035800     MOVE '  GE'              TO GOOD-STATUSCODES                         
035900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
036000     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
036100     PERFORM IMS-STATUSCHECK                                              
036200     .                                                                    
036300 IMS-STATUSCHECK SECTION.                                                 
036400                                                                          
036500     SET STATUS-IX TO 1                                                   
036600     SEARCH GOOD-STATUS                                                   
036700       AT END                                                             
036800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036900           DELIMITED BY SIZE INTO ERROR-TEXT                              
037000         DISPLAY ERROR-TEXT                                               
037100         CALL FELLOG                                                      
037200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
037300         CONTINUE                                                         
037400     END-SEARCH                                                           
037500     .                                                                    
