000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247200.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/10/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        INPUT FILE FROM SCREEN 2115 WITH SUPPLIER SHIP.                  
001100*        UPDATE KVVECKOR-LT ON WDK722 FOR ALL PARTNO                      
001200*        CONNECTED TO THE SUPPLIER SHIP.                                  
001300*                                                                         
001600*        THE PROGRAM UPDATES WDK7 AND WDG3 H-TYP=2213/14                  
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INPUT FILE FROM W2247400                                   
002700     SELECT W22472                     ASSIGN TO W22472D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W22472                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W2247401      -L.                                              
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W2247200'.            
004110 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004120 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004130                                                                          
004140 01  W-KVVECKOR-LT               PIC X(02).                               
004150 01  FILLER REDEFINES W-KVVECKOR-LT.                                      
004160     03  W-KVVECKOR-LT-NUM       PIC 9(02).                               
004170                                                                          
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004810                                                                          
004900 77  YES                         PIC X       VALUE 'Y'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005010 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
005100                                                                          
005101 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005110 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005120 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005130                                                                          
005200 01  ERROR-TEXT.                                                          
005300     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005500                                                                          
005600 77  W22472-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W22472                       VALUE 'Y'.                   
005800     EJECT                                                                
006500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES TODAYS-DATE.                                        
006700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006900     03  TODAYS-DATE-DAY         PIC 9(2).                                
007000     EJECT                                                                
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007510     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W2247401     -PRE IN-                                     
008600*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  KEYS-TILL-DLI.                                                       
009100     03  W-IDARTNR-X.                                                     
009200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009300     03  W-IDDC-X.                                                        
009400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009500                                                                          
009510     03  W-WDG3KEY-2213-X.                                                
009520         05  W-IDHTYP-2213       PIC X(4)    VALUE '2213'.                
009530         05  W-IDDC-2213         PIC X(2)    VALUE SPACE.                 
009540         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009550                                                                          
009560     03  W-WDGXKEY-4579-X.                                                
009570         05  W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
009580         05  W-IDPGM             PIC X(8)    VALUE 'W2247200'.            
009590         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
009591                                                                          
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FOUND                       VALUE '  '.                  
009900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010200     88  IMS-NOT-OK                          VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GOOD-STATUSCODES.                                                    
010500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010810 01  SSA3                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNCTION CODES                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
011500 01  DLI-IO-WDK722.                                                       
011600*    03  -COPY WDK722                                                     
011700     EJECT                                                                
011800                                                                          
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
012000 01  DLI-IO-WDGX2214.                                                     
012100*    03  -COPY WDGX2214                                                   
012200     EJECT                                                                
012210*-ÅTERSTARTSREGISTER WDR4                                                 
012220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
012230 01  DLI-IO-WDGX4580.                                                     
012240*    03  -COPY WDGX4580                                                   
012250     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01  -COPY W0009   -PRE MSG-                                              
012600                                                                          
012700*01  -COPY W0008  -PRE WDK7-                                              
012800     05  FILLER                  PIC X.                                   
012810     EJECT                                                                
012820*01  -COPY W0008  -PRE WDG3-                                              
012830     05  FILLER                  PIC X.                                   
012840     EJECT                                                                
012850*01  -COPY W0008  -PRE 4579-                                              
012860     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDG3-PCB 4579-PCB.            
013100 MAIN SECTION.                                                            
013200     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDG3-PCB 4579-PCB.            
013300                                                                          
013400     PERFORM A-INIT                                                       
013500                                                                          
013510     PERFORM IMS-LAS-ATERSTART                                            
013520     IF 4580-KVPOST > +0                                                  
013530        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
013540     ELSE                                                                 
013550       PERFORM S01-READ-W22472                                            
013560     END-IF                                                               
013570                                                                          
013700     PERFORM UNTIL END-OF-W22472                                          
014100       PERFORM H-UPDATE-WDK722                                            
015800                                                                          
015900       PERFORM S01-READ-W22472                                            
015910                                                                          
016000     END-PERFORM                                                          
016100                                                                          
016200                                                                          
016300     PERFORM Z-FINIT                                                      
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000     SKIP2                                                                
017100                                                                          
017200     PERFORM IMS-RESTART                                                  
017300                                                                          
017400     OPEN INPUT W22472                                                    
017500                                                                          
017600                                                                          
017700     ACCEPT TODAYS-DATE  FROM DATE                                        
017800                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018200 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
018300     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
018400                                                                          
018410     PERFORM S01-READ-W22472                                              
018420     PERFORM UNTIL END-OF-W22472 OR                                       
018430                   W-KVPOST-IN = 4580-KVPOST                              
018440        PERFORM S01-READ-W22472                                           
018450     END-PERFORM                                                          
018460                                                                          
018470     IF END-OF-W22472                                                     
018480        MOVE 'INPUTFIL EOF = YES, VID ÅTERSTART'                          
018490                      TO ERROR-TEXT                                       
018491        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
018492     END-IF                                                               
018493     .                                                                    
018494     EJECT                                                                
018495 H-UPDATE-WDK722 SECTION.                                                 
018496     MOVE 'H-UPDATE-WDK722          ' TO CURRENT-SECTION                  
018497                                                                          
018499     MOVE IN-IDARTNR                TO W-IDARTNR                          
018500     MOVE IN-IDDC                   TO W-IDDC                             
018501     PERFORM IMS-GHU-WDK722                                               
018502     IF SEGMENT-FOUND                                                     
018504                                                                          
018505        IF XLAG-TIMANLED = ZERO                                           
018506        OR XLAG-TIMANLED < TODAYS-DATE                                    
018507           MOVE IN-KVVECKOR-LT      TO W-KVVECKOR-LT                      
018508           MOVE W-KVVECKOR-LT-NUM   TO XLAG-KVVECKOR-LT                   
018509           COMPUTE XLAG-KVVECKOR-FT ROUNDED =                             
018510                  (XLAG-KVDAGAR-FFH / 5) + W-KVVECKOR-LT-NUM              
018511                                                                          
018513           MOVE ZERO                TO XLAG-TIMANLED                      
018515                                                                          
018516           PERFORM IMS-REPL-WDK722                                        
018517                                                                          
018518           MOVE IN-IDDC             TO W-IDDC-2213                        
018519           MOVE IN-IDARTNR          TO 2214-IDARTNR                       
018520           PERFORM IMS-ISRT-WDGX-2214                                     
018521           ADD +1                   TO CHKP-ANT                           
018522                                                                          
018523           IF CHKP-ANT > CHKP-MAX                                         
018524              PERFORM X-TAKE-CHECKPOINT                                   
018525           END-IF                                                         
018526        END-IF                                                            
018527     END-IF                                                               
018528     .                                                                    
018529     EJECT                                                                
018530 Z-FINIT SECTION.                                                         
018531                                                                          
018540     CLOSE W22472                                                         
018600     SKIP2                                                                
018700     MOVE 'S' TO POSTSUM-OPKOD                                            
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018801                                                                          
018810     PERFORM IMS-LAS-ATERSTART                                            
018820                                                                          
018830     MOVE +0                           TO 4580-KVPOST                     
018840     MOVE TODAYS-DATE                  TO 4580-TIUPPDAT                   
018850     ACCEPT 4580-TIUPPTID FROM TIME                                       
018860                                                                          
018870     PERFORM IMS-REPL-ATERSTART                                           
018900     .                                                                    
019000     EJECT                                                                
019100 S01-READ-W22472  SECTION.                                                
019200     SKIP2                                                                
019300     READ W22472 INTO IN-AREA                                             
019400     AT END                                                               
019600        SET END-OF-W22472 TO TRUE                                         
019700                                                                          
019800     NOT AT END                                                           
019900        MOVE 'W22472'   TO POSTSUM-FDNAMN                                 
020000        MOVE 'W22472D1' TO POSTSUM-DDNAMN2                                
020100        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
020200        CALL POSTSUM USING POSTSUM-PARM                                   
020300                                                                          
020400        ADD +1          TO W-KVPOST-IN                                    
020500     END-READ                                                             
020600     .                                                                    
020700     EJECT                                                                
020800 X-TAKE-CHECKPOINT   SECTION.                                             
020900                                                                          
020901*    UPPDATERA ÅTERSTARTREGISTRET                                         
020902     PERFORM IMS-LAS-ATERSTART                                            
020903                                                                          
020904     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
020905     ACCEPT 4580-TIUPPDAT FROM DATE                                       
020906     ACCEPT 4580-TIUPPTID FROM TIME                                       
020907                                                                          
020908     PERFORM IMS-REPL-ATERSTART                                           
020909                                                                          
020910*    TAG CHECKPOINT                                                       
020911     PERFORM IMS-CHECKPOINT                                               
020912                                                                          
020913     MOVE +0                TO CHKP-ANT                                   
021500     .                                                                    
021600     EJECT                                                                
021700* --- IMS SECTIONS  ---                                                   
021800                                                                          
021900     EJECT                                                                
023800 IMS-RESTART SECTION.                                                     
023810     MOVE 'IMS-RESTART '  TO DBS-SECTION                                  
023900     SKIP2                                                                
024000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024100     MOVE '  ' TO GOOD-STATUSCODES                                        
024200     CALL CBLTDLI USING XRST MSG-PCB                                      
024300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024400                        CHKP-AREA-LENGTH CHKP-AREA                        
024500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024600     PERFORM IMS-STATUSCHECK                                              
024700     .                                                                    
024800     SKIP3                                                                
024900 IMS-CHECKPOINT SECTION.                                                  
024910     MOVE 'IMS-CHECKPOINT '     TO DBS-SECTION                            
024920                                                                          
025100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025200     MOVE '  XD' TO GOOD-STATUSCODES                                      
025300     CALL CBLTDLI USING CHKP MSG-PCB                                      
025400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025500                        CHKP-AREA-LENGTH CHKP-AREA                        
025600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025700     PERFORM IMS-STATUSCHECK                                              
025800                                                                          
025900     IF IMS-NOT-OK                                                        
026000       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
026010                                    TO ERROR-TEXT-STR                     
026100       DISPLAY ERROR-TEXT                                                 
026200       CALL FELLOG                                                        
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 IMS-GHU-WDK722 SECTION.                                                  
026700     MOVE 'IMS-GU-WDK722   ' TO DBS-SECTION                               
026800                                                                          
026900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
027000          DELIMITED BY SIZE INTO SSA1                                     
027100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
027200          DELIMITED BY SIZE INTO SSA2                                     
027300     MOVE 'WDK722 '           TO SSA3                                     
027400     MOVE '  GE'              TO GOOD-STATUSCODES                         
027500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
027600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
027710     PERFORM IMS-STATUSCHECK                                              
027800     .                                                                    
027900                                                                          
028000 IMS-REPL-WDK722 SECTION.                                                 
028010     MOVE 'IMS-REPL-WDK722 ' TO DBS-SECTION                               
028100                                                                          
028200     MOVE '  ' TO GOOD-STATUSCODES                                        
028300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
028400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
028500     PERFORM IMS-STATUSCHECK                                              
028600     .                                                                    
028700     EJECT                                                                
028800 IMS-ISRT-WDGX-2214 SECTION.                                              
028810     MOVE 'IMS-ISRT-WDGX-2214 '   TO DBS-SECTION                          
028900                                                                          
029000     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
029100             DELIMITED BY SIZE INTO SSA1                                  
029200     MOVE 'WDG302 '              TO SSA2                                  
029300     MOVE '  II'                 TO GOOD-STATUSCODES                      
029400     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2214 SSA1 SSA2           
029500     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
029600     PERFORM IMS-STATUSCHECK                                              
029700     .                                                                    
029800     EJECT                                                                
029810 IMS-LAS-ATERSTART SECTION.                                               
029820     MOVE 'IMS-LAS-ATERSTART    ' TO DBS-SECTION                          
029830                                                                          
029840     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
029850                    DELIMITED BY SIZE INTO SSA1                           
029860     MOVE 'WDR470 '        TO SSA2                                        
029870     MOVE '  '             TO GOOD-STATUSCODES                            
029880     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
029890     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
029891     PERFORM IMS-STATUSCHECK                                              
029892     .                                                                    
029893                                                                          
029894 IMS-REPL-ATERSTART SECTION.                                              
029895     MOVE 'IMS-REPL-ATERSTART   ' TO DBS-SECTION                          
029896                                                                          
029897     MOVE '  '             TO GOOD-STATUSCODES                            
029898     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
029899     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
029900     PERFORM IMS-STATUSCHECK                                              
029901     .                                                                    
029902                                                                          
029910 IMS-STATUSCHECK SECTION.                                                 
030000     SKIP2                                                                
030100     SET STATUS-IX TO 1                                                   
030200     SEARCH GOOD-STATUS                                                   
030300       AT END                                                             
030400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030500           DELIMITED BY SIZE INTO ERROR-TEXT                              
030600         DISPLAY ERROR-TEXT                                               
030700         CALL FELLOG                                                      
030800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030900         CONTINUE                                                         
031000     END-SEARCH                                                           
031100     .                                                                    
