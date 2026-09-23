000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122900.                                                
000300 AUTHOR.         SRINADH NADIMPALLI.                                      
000400 DATE-WRITTEN.   23/01/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CALCULATES PROPOSED ETA FOR ALL SEA CONTAINERS.                  
000900*                                                                         
001000*        THE PROGRAM READS     WDB6                                       
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
002400*          --- INPUT FILE 1                                               
002500     SELECT W612281                    ASSIGN TO W61229D1.                
002600     SKIP2                                                                
002700*          --- INPUT FILE 2                                               
002800     SELECT W612282                    ASSIGN TO W61229D2.                
002900     SKIP2                                                                
003000*          --- OUTPUT FILE                                                
003100     SELECT W61229                     ASSIGN TO W61229D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W612281                                                              
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W6122801      -L.                                              
004200     SKIP3                                                                
004300 FD  W612282                                                              
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W6122802      -L.                                              
004800     SKIP3                                                                
004900 FD  W61229                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  RECORD -COPY W61229 -PRE  OUT-  -L.                                  
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W6122900'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000 01  RECORD-FOUND                PIC X       VALUE 'N'.                   
006100     88 REC-OK                               VALUE 'J'.                   
006200     88 REC-NOK                              VALUE 'N'.                   
006300                                                                          
006400 77  W612281-EOF-SW               PIC X       VALUE 'N'.                  
006500     88  END-OF-W612281                       VALUE 'J'.                  
006600                                                                          
006700 77  W612282-EOF-SW               PIC X       VALUE 'N'.                  
006800     88  END-OF-W612282                       VALUE 'J'.                  
006900     EJECT                                                                
007000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES TODAYS-DATE.                                        
007200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007400     03  TODAYS-DATE-DAY         PIC 9(2).                                
007500     EJECT                                                                
007600 01  YESTER-DATE                 PIC 9(6)    VALUE ZERO.                  
007700 01  NEXT-DATE                   PIC 9(6)    VALUE ZERO.                  
007710 01  WS-DABERANK-TMP.                                                     
007720     03 WS-DABERANK-TMP-1        PIC 9(2)    VALUE ZERO.                  
007730     03 WS-DABERANK-TMP-2        PIC 9(6)    VALUE ZERO.                  
007800 01  WS-TBL-LEADTIMES.                                                    
007900     03 WS-TBL-LEAD OCCURS 100.                                           
008000        05 WS-IDDC-REC1 PIC X(02).                                        
008100        05 WS-IDDC-SEND1 PIC X(02).                                       
008200        05 WS-KVDLTID-BOAT2DC PIC 9(03).                                  
008300        05 WS-KVDLTID-BOATINS PIC 9(03).                                  
008400        05 WS-KVDLTID-CUST PIC 9(03).                                     
008500        05 WS-KVDLTID-CUSTWAIT PIC 9(03).                                 
008600        05 WS-KVDLTID-CUST2DC PIC 9(03).                                  
008700        05 WS-KVDLTID-AIRETA PIC 9(03).                                   
008800 01  WS-TBL-REC-WH.                                                       
008900     03 WS-TBL-REC OCCURS 1 TO 500 DEPENDING ON ANTAL.                    
009000        05 WS-IDDC-REC          PIC X(02).                                
009100        05 WS-IDLBBET           PIC X(12).                                
009200        05 WS-IDDC-SEND         PIC X(02).                                
009300        05 WS-KDTRPSTA          PIC X.                                    
009400        05 WS-KDTRPSTA-SORT     PIC X.                                    
009500        05 WS-KVRADER-PRIO      PIC 9(05).                                
009600        05 WS-KVRADER-FAKT      PIC 9(05).                                
009700        05 WS-KVRADER-MOT       PIC 9(05).                                
009800        05 WS-DABERANK          PIC 9(8).                                 
009900        05 WS-DABERANK-DISCH    PIC 9(6).                                 
009910        05 WS-DABERANK-PROP     PIC 9(6).                                 
010000        05 WS-FLAMANETA         PIC X.                                    
010100        05 WS-ETA-POD-DEP       PIC 9(6).                                 
010200        05 WS-ETA-READY-FOR-WH  PIC 9(6) VALUE 0.                         
010300        05 WS-DABERANK-PROP-NEW PIC 9(6) VALUE 0.                         
010400 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
010500 77  INDX2                       PIC S9(3)  VALUE +0    COMP SYNC.        
010600 77  MAX-INDX                    PIC S9(3)  VALUE +500  COMP SYNC.        
010700 77  MAX-INDX2                   PIC S9(3)  VALUE +100  COMP SYNC.        
010800 77  WS-CNT-CONTAINER            PIC S9(3)  VALUE +0    COMP SYNC.        
010900 77  ANTAL                       PIC S9(3)  VALUE +0    COMP SYNC.        
011000 77  SAVE-IDDC-REC               PIC X(02).                               
011100 77  WS-LEADTIMES-AIR            PIC 9(05)  VALUE 0.                      
011200 77  WS-LEADTIMES-BOAT           PIC 9(05)  VALUE 0.                      
011300 77  WS-MAX-CAP-DAY              PIC 9(03)V9(3).                          
011310 77  WS-MAX-CAP-DAY-INT          PIC 9(03).                               
011400 77  WS-MIN-ETA-FOR-WH           PIC 9(06).                               
011500 77  WS-MAX-ETA-FOR-WH           PIC 9(06).                               
011600 77  WS-NEXT-WDAY                PIC 9(06).                               
011700 77  WS-SUM-CONT-CAP             PIC 9(3)V9(2).                           
011800 01  GENERAL-SUBPROGRAMS.                                                 
011900*                                                                         
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
012410     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
012500     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
012600     SKIP2                                                                
012700 01  FELTEXT.                                                             
012800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013000     EJECT                                                                
013100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
013200                                                                          
013300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013600     SKIP2                                                                
013700 01  ERROR-TEXT.                                                          
013800     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
013900     03  ERROR-TEXT-STR          PIC X(71)   VALUE SPACE.                 
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL POSTSUM                                          
014200*                                                                         
014300*01  -COPY W0005   -PRE  POSTSUM-                                         
014310     EJECT                                                                
014320*    --- PARAMETRAR TILL WDAGKONV                                         
014330*                                                                         
014340*01  -COPY WDAGAREA                                                       
014400     EJECT                                                                
014500*01  -COPY WORKAREA                                                       
014600     EJECT                                                                
014700 01  WZ20DAYS                    PIC X(8) VALUE 'WZ20DAYS'.               
014800     SKIP3                                                                
014900*    -COPY WZ20DAYS                                                       
015000     EJECT                                                                
015100 01  IN1-AREA-START              PIC X(24)   VALUE                        
015200                                 'IN1-AREA-START  '.                      
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W6122801     -PRE IN1-                                    
015600     EJECT                                                                
015700 01  IN2-AREA-START              PIC X(24)   VALUE                        
015800                                 'IN2-AREA-START  '.                      
015900     SKIP2                                                                
016000                                                                          
016100*01  AREA -COPY W6122802     -PRE IN2-                                    
016200     EJECT                                                                
016300 01  OUT-AREA-START              PIC X(24)   VALUE                        
016400                                 'OUT-AREA-START  '.                      
016500     SKIP2                                                                
016600                                                                          
016700*01  AREA -COPY W61229     -PRE OUT-                                      
016800     EJECT                                                                
016900*    --- AREAS FOR IMS-SECTIONS                                           
017000*                                                                         
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  KEYS-FOR-DLI.                                                        
017500     03  W-IDDC-REC-X.                                                    
017600         05  W-IDDC-REC          PIC X(2)    VALUE SPACE.                 
017700     03  W-IDDC-SEND-X.                                                   
017800         05  W-IDDC-SEND         PIC X(2)    VALUE SPACE.                 
017900     SKIP2                                                                
018000*    --- STATUS-KOD FRÅN IMS                                              
018100 01  STATUS-WS                   PIC XX.                                  
018200     88  SEGMENT-FOUND                       VALUE '  '.                  
018300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018500     SKIP2                                                                
018600 01  GOOD-STATUSCODES.                                                    
018700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  SSA1                        PIC X(64).                               
019000 01  SSA2                        PIC X(64).                               
019100     EJECT                                                                
019200*    --- IMS FUNCTION CODES                                               
019300*01  -COPY W0003                                                          
019400     EJECT                                                                
019500*    ---  DLI INPUT-OUTPUT AREA                                           
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
019700 01  DLI-IO-WDB601.                                                       
019800*    03  -COPY WDB601                                                     
019900     EJECT                                                                
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
020100 01  DLI-IO-WDB616.                                                       
020200*    03  -COPY WDB616                                                     
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500                                                                          
020600                                                                          
020700*01  -COPY W0008  -PRE WDB6-                                              
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING WDB6-PCB.                                      
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
021300                                                                          
021400     PERFORM A-INIT                                                       
021500                                                                          
021600     PERFORM S02-READ-W612282                                             
021700     MOVE IN2-IDDC-REC TO SAVE-IDDC-REC                                   
021800     MOVE +1 TO INDX                                                      
021900     PERFORM UNTIL END-OF-W612282                                         
022000      PERFORM UNTIL (IN2-IDDC-REC NOT = SAVE-IDDC-REC) OR                 
022100                    END-OF-W612282                                        
022200        PERFORM B-FILL-TBL-RECWH                                          
022300        PERFORM C-SRCH-TBL-LEADTIMES                                      
022400        MOVE IN2-IDDC-REC TO SAVE-IDDC-REC                                
022500       PERFORM S02-READ-W612282                                           
022600       ADD +1 TO INDX                                                     
022700      END-PERFORM                                                         
022800      PERFORM D-CALC-MAXCAPDY                                             
022900      PERFORM E-LOOP-SEACONT                                              
023000      PERFORM F-FILL-OUTAREA                                              
023100      MOVE IN2-IDDC-REC TO SAVE-IDDC-REC                                  
023200     END-PERFORM                                                          
023300                                                                          
023400     PERFORM Z-FINIT                                                      
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 A-INIT SECTION.                                                          
024100                                                                          
024200     OPEN INPUT  W612281                                                  
024300                 W612282                                                  
024400                                                                          
024500     OPEN OUTPUT W61229                                                   
024600                                                                          
024700     PERFORM S01-READ-W612281                                             
024800     MOVE 1 TO INDX2                                                      
024900     PERFORM UNTIL END-OF-W612281                                         
025000       MOVE IN1-IDDC-REC TO W-IDDC-REC                                    
025100       MOVE IN1-IDDC-SEND TO W-IDDC-SEND                                  
025200       PERFORM IMS-GU-WDB616                                              
025300       IF SEGMENT-FOUND                                                   
025400        PERFORM AA-FILL-TBL-LEADTIMES                                     
025500       END-IF                                                             
025600       PERFORM S01-READ-W612281                                           
025700     END-PERFORM                                                          
025800     ACCEPT TODAYS-DATE  FROM DATE                                        
025900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026000     MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                          
026100     MOVE SPACES               TO DAYS-TIDATE1                            
026200     MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                          
026300     MOVE TODAYS-DATE          TO DAYS-TIDATE2                            
026400     MOVE 1                    TO DAYS-KVDAYS                             
026500     MOVE SPACE                TO DAYS-IDCALEND                           
026600                                                                          
026700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
026800                                                                          
026900     IF DAYS-KDRC = ZERO                                                  
027000       MOVE DAYS-TIDATE1(1:6) TO YESTER-DATE                              
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 AA-FILL-TBL-LEADTIMES SECTION.                                           
027500     MOVE IN1-IDDC-REC TO WS-IDDC-REC1(INDX2)                             
027600     MOVE IN1-IDDC-SEND TO WS-IDDC-SEND1(INDX2)                           
027700     MOVE REF-KVDLTID-BOAT2DC TO    WS-KVDLTID-BOAT2DC(INDX2)             
027800     MOVE REF-KVDLTID-BOATINS TO    WS-KVDLTID-BOATINS(INDX2)             
027900     MOVE REF-KVDLTID-CUST    TO  WS-KVDLTID-CUST(INDX2)                  
028000     MOVE REF-KVDLTID-CUSTWAIT TO   WS-KVDLTID-CUSTWAIT(INDX2)            
028100     MOVE REF-KVDLTID-CUST2DC TO    WS-KVDLTID-CUST2DC(INDX2)             
028200     MOVE REF-KVDLTID-AIRETA  TO    WS-KVDLTID-AIRETA(INDX2)              
028300     ADD +1 TO INDX2                                                      
028400     .                                                                    
028500     EJECT                                                                
028600 B-FILL-TBL-RECWH SECTION.                                                
028700     MOVE IN2-IDDC-REC         TO    WS-IDDC-REC(INDX)                    
028800     MOVE IN2-IDLBBET          TO    WS-IDLBBET(INDX)                     
028900     MOVE IN2-IDDC-SEND        TO    WS-IDDC-SEND(INDX)                   
029000     MOVE IN2-KDTRPSTA         TO    WS-KDTRPSTA(INDX)                    
029100     MOVE IN2-KDTRPSTA-SORT    TO    WS-KDTRPSTA-SORT(INDX)               
029200     MOVE IN2-KVRADER-PRIO     TO    WS-KVRADER-PRIO(INDX)                
029300     MOVE IN2-KVRADER-FAKT     TO    WS-KVRADER-FAKT(INDX)                
029400     MOVE IN2-KVRADER-MOT      TO    WS-KVRADER-MOT(INDX)                 
029500     MOVE IN2-DABERANK         TO    WS-DABERANK(INDX)                    
029600     MOVE IN2-DABERANK-DISCH   TO    WS-DABERANK-DISCH(INDX)              
029610     MOVE IN2-DABERANK-PROP    TO    WS-DABERANK-PROP(INDX)               
029700     MOVE IN2-FLMANETA         TO    WS-FLAMANETA(INDX)                   
029800     MOVE IN2-ETA-POD-DEP      TO    WS-ETA-POD-DEP(INDX)                 
029900     MOVE 0                    TO    WS-ETA-READY-FOR-WH(INDX)            
030000     MOVE 0                    TO    WS-DABERANK-PROP-NEW(INDX)           
030100     .                                                                    
030200     EJECT                                                                
030300 C-SRCH-TBL-LEADTIMES SECTION.                                            
030400     MOVE 1 TO INDX2                                                      
030500     MOVE NOO TO RECORD-FOUND                                             
030600     PERFORM UNTIL INDX2 > MAX-INDX2 OR REC-OK                            
030700       IF IN2-IDDC-REC = WS-IDDC-REC1(INDX2)AND                           
030800          IN2-IDDC-SEND = WS-IDDC-SEND1(INDX2)                            
030900          MOVE WS-KVDLTID-AIRETA(INDX2) TO WS-LEADTIMES-AIR               
031000          MOVE YES TO RECORD-FOUND                                        
031100       END-IF                                                             
031200       IF REC-OK                                                          
031300        PERFORM CA-CALC-ETA-READYFORWH                                    
031400       END-IF                                                             
031410       MOVE ZERO TO WS-LEADTIMES-AIR                                      
031420       MOVE ZERO TO WS-LEADTIMES-BOAT                                     
031500       ADD +1 TO INDX2                                                    
031600     END-PERFORM                                                          
031700     .                                                                    
031800     EJECT                                                                
031900 CA-CALC-ETA-READYFORWH SECTION.                                          
032000     IF IN2-KDTRPSTA = 'R' OR 'M' OR 'L'                                  
032100      MOVE TODAYS-DATE TO WS-ETA-READY-FOR-WH(INDX)                       
032200     ELSE                                                                 
032300      IF IN2-ETA-POD-DEP > ZERO                                           
032310        IF WS-KVDLTID-BOAT2DC(INDX2) > 0 OR                               
032320           WS-KVDLTID-BOATINS(INDX2) > 0                                  
032400          COMPUTE WS-LEADTIMES-BOAT =                                     
032500                              (WS-KVDLTID-BOAT2DC(INDX2) +                
032600                              WS-KVDLTID-BOATINS(INDX2))                  
032900                                                                          
033000          MOVE WS-ETA-POD-DEP(INDX) TO WORK-TIAAMMDD-FOM                  
033200          MOVE 002               TO WORK-KDCALL                           
033300          MOVE WS-IDDC-REC(INDX) TO WORK-IDDC                             
033400          MOVE WS-LEADTIMES-BOAT TO WORK-KVWORKD                          
033500          CALL WORKDAY            USING WORK-KDCALL                       
033600                                        WORK-DATE-AREA                    
033700                                        WORK-KDSVAR                       
033800          IF WORK-KDSVAR-OK                                               
033900            MOVE WORK-TIAAMMDD-TOM TO WS-ETA-READY-FOR-WH(INDX)           
034000          ELSE                                                            
034100            MOVE 'FEL I WORKDAY' TO FELTEXT-STR                           
034200            DISPLAY FELTEXT                                               
034300            MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                         
034400            PERFORM S99-ABEND                                             
034500          END-IF                                                          
034510        ELSE                                                              
034511          MOVE WS-ETA-POD-DEP(INDX) TO WS-ETA-READY-FOR-WH(INDX)          
034512        END-IF                                                            
034600      ELSE                                                                
034700       MOVE  WS-DABERANK(INDX) TO WS-ETA-READY-FOR-WH(INDX)               
034800      END-IF                                                              
034900     END-IF                                                               
035000     IF WS-ETA-READY-FOR-WH(INDX) < TODAYS-DATE                           
035100       MOVE TODAYS-DATE TO WS-ETA-READY-FOR-WH(INDX)                      
035200     END-IF                                                               
035300     IF (WS-ETA-READY-FOR-WH(INDX) < WS-MIN-ETA-FOR-WH) OR                
035400        WS-MIN-ETA-FOR-WH = 0                                             
035500       MOVE WS-ETA-READY-FOR-WH(INDX) TO WS-MIN-ETA-FOR-WH                
035600     END-IF                                                               
035700     IF (WS-ETA-READY-FOR-WH(INDX) > WS-MAX-ETA-FOR-WH) OR                
035800        WS-MAX-ETA-FOR-WH = 0                                             
035900       MOVE WS-ETA-READY-FOR-WH(INDX) TO WS-MAX-ETA-FOR-WH                
036000     END-IF                                                               
036200     ADD 1 TO WS-CNT-CONTAINER                                            
036300     .                                                                    
036400     EJECT                                                                
036500 D-CALC-MAXCAPDY SECTION.                                                 
036600     SUBTRACT 1 FROM INDX                                                 
036700     IF WS-MIN-ETA-FOR-WH > 0 AND WS-MAX-ETA-FOR-WH > 0                   
037000       MOVE WS-MIN-ETA-FOR-WH  TO WORK-TIAAMMDD-FOM                       
037100       MOVE WS-MAX-ETA-FOR-WH  TO WORK-TIAAMMDD-TOM                       
037200       MOVE 001                TO WORK-KDCALL                             
037300       MOVE WS-IDDC-REC(INDX) TO WORK-IDDC                                
037400       CALL WORKDAY            USING WORK-KDCALL                          
037500                                     WORK-DATE-AREA                       
037600                                     WORK-KDSVAR                          
037700       IF WORK-KDSVAR-OK                                                  
038100        COMPUTE WS-MAX-CAP-DAY ROUNDED =                                  
038200                     (WS-CNT-CONTAINER / WORK-KVWORKD) + 0.5              
038300        ADD 0.999 TO WS-MAX-CAP-DAY GIVING WS-MAX-CAP-DAY-INT             
038500       ELSE                                                               
038600        MOVE 'FEL I WORKDAY' TO FELTEXT-STR                               
038700        DISPLAY FELTEXT                                                   
038800        MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                             
038900        PERFORM S99-ABEND                                                 
039000       END-IF                                                             
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 E-LOOP-SEACONT SECTION.                                                  
039500     MOVE INDX TO ANTAL                                                   
039600     SORT WS-TBL-REC    ASCENDING WS-KDTRPSTA-SORT                        
039700                        ASCENDING WS-ETA-READY-FOR-WH                     
039800                        DESCENDING WS-KVRADER-PRIO                        
039900     MOVE YESTER-DATE TO WORK-TIAAMMDD-FOM                                
040000     MOVE YESTER-DATE TO WORK-TIAAMMDD-TOM                                
040100     MOVE 001                TO WORK-KDCALL                               
040200     MOVE WS-IDDC-REC(INDX) TO WORK-IDDC                                  
040300     CALL WORKDAY            USING WORK-KDCALL                            
040400                                   WORK-DATE-AREA                         
040500                                   WORK-KDSVAR                            
040600      IF WORK-KDSVAR-OK                                                   
040700       MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO WS-NEXT-WDAY                    
040800      ELSE                                                                
040900       MOVE 'FEL I WORKDAY' TO FELTEXT-STR                                
041000       DISPLAY FELTEXT                                                    
041100       MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                              
041200       PERFORM S99-ABEND                                                  
041300      END-IF                                                              
041400     MOVE 1 TO INDX                                                       
041500     MOVE 0 TO WS-SUM-CONT-CAP                                            
041600     PERFORM UNTIL INDX > ANTAL                                           
041700      PERFORM UNTIL (WS-ETA-READY-FOR-WH(INDX) > WS-NEXT-WDAY) OR         
041800                     INDX > ANTAL                                         
041900       IF WS-ETA-READY-FOR-WH(INDX) <= WS-NEXT-WDAY AND                   
042000          WS-DABERANK-PROP-NEW (INDX) = ZERO                              
042100          IF WS-KDTRPSTA(INDX) = 'R' OR 'M' OR 'L'                        
042200            COMPUTE WS-SUM-CONT-CAP =                                     
042300                              (WS-SUM-CONT-CAP +                          
042400          (1 - (WS-KVRADER-MOT(INDX) / WS-KVRADER-FAKT(INDX))))           
042500          ELSE                                                            
042600            ADD +1 TO WS-SUM-CONT-CAP                                     
042700          END-IF                                                          
042800          IF WS-SUM-CONT-CAP <= WS-MAX-CAP-DAY-INT                        
042900            MOVE WS-NEXT-WDAY TO WS-DABERANK-PROP-NEW(INDX)               
043000          ELSE                                                            
043100            MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                   
043200            MOVE WS-NEXT-WDAY         TO DAYS-TIDATE1                     
043300            MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                   
043400            MOVE SPACES               TO DAYS-TIDATE2                     
043500            MOVE 1                    TO DAYS-KVDAYS                      
043600            MOVE SPACE                TO DAYS-IDCALEND                    
043700                                                                          
043800            CALL WZ20DAYS USING DAYS-WZ20DAYS                             
043900                                                                          
044000            IF DAYS-KDRC = ZERO                                           
044100              MOVE DAYS-TIDATE2(1:6) TO NEXT-DATE                         
044200            END-IF                                                        
044300            MOVE NEXT-DATE      TO WS-ETA-READY-FOR-WH(INDX)              
044400          END-IF                                                          
044500       END-IF                                                             
044600       ADD +1 TO INDX                                                     
044700      END-PERFORM                                                         
044800      IF INDX > ANTAL AND WS-DABERANK-PROP-NEW(ANTAL) > 0                 
044900       CONTINUE                                                           
045000      ELSE                                                                
045100       SORT WS-TBL-REC    ASCENDING WS-KDTRPSTA-SORT                      
045200                          ASCENDING WS-ETA-READY-FOR-WH                   
045300                          DESCENDING WS-KVRADER-PRIO                      
045400       MOVE WS-NEXT-WDAY TO WORK-TIAAMMDD-FOM                             
045500       MOVE WS-NEXT-WDAY TO WORK-TIAAMMDD-TOM                             
045600       MOVE 001                TO WORK-KDCALL                             
045700       MOVE SAVE-IDDC-REC     TO WORK-IDDC                                
045800       CALL WORKDAY            USING WORK-KDCALL                          
045900                                     WORK-DATE-AREA                       
046000                                     WORK-KDSVAR                          
046100       IF WORK-KDSVAR-OK                                                  
046200        MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO WS-NEXT-WDAY                   
046300       ELSE                                                               
046400        MOVE 'FEL I WORKDAY' TO FELTEXT-STR                               
046500        DISPLAY FELTEXT                                                   
046600        MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                             
046700        PERFORM S99-ABEND                                                 
046800       END-IF                                                             
046900       MOVE 1 TO INDX                                                     
047000       MOVE 0 TO WS-SUM-CONT-CAP                                          
047100      END-IF                                                              
047200     END-PERFORM                                                          
047300     .                                                                    
047400     EJECT                                                                
047410 F-FILL-OUTAREA SECTION.                                                  
047411     MOVE 1 TO INDX                                                       
047412     PERFORM UNTIL INDX > ANTAL                                           
047414       MOVE WS-IDDC-REC(INDX) TO OUT-IDDC-REC                             
047415       MOVE WS-IDLBBET(INDX)  TO OUT-IDLBBET                              
047416       MOVE WS-DABERANK-PROP-NEW(INDX) TO OUT-DABERANK-PROP               
047417                                                                          
047418       MOVE WS-DABERANK(INDX) TO WS-DABERANK-TMP                          
047419       IF (WS-KDTRPSTA(INDX) = 'R' OR 'M' OR 'L' OR 'C' OR 'A') OR        
047419          WS-DABERANK-TMP-2 = OUT-DABERANK-PROP                           
047420          MOVE ZERO TO OUT-DABERANK                                       
047421       ELSE                                                               
047422         IF WS-DABERANK-TMP-2 < WS-DABERANK-DISCH(INDX)                   
047423           MOVE 20                         TO OUT-DABERANK(1:2)           
047424           MOVE WS-DABERANK-PROP-NEW(INDX) TO OUT-DABERANK(3:6)           
047425         ELSE                                                             
047426           MOVE TODAYS-DATE  TO DAG-TIAAMMDD-FOM                          
047427           MOVE +10          TO DAG-KVKALDAG                              
047428           MOVE 002          TO DAG-KDCALL                                
047429           CALL WDAGKONV USING  DAG-KDCALL                                
047430                                DAG-DATUM-AREA                            
047431                                DAG-KDSVAR                                
047432           IF DAG-KDSVAR = SPACE                                          
047433             CONTINUE                                                     
047434           ELSE                                                           
047435             MOVE 'FEL FRÅN WDAGKONV' TO FELTEXT-STR                      
047436             DISPLAY FELTEXT                                              
047437             MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                        
047438             PERFORM S99-ABEND                                            
047439           END-IF                                                         
047440                                                                          
047441           IF WS-DABERANK-TMP-2 >= TODAYS-DATE AND                        
047442              WS-DABERANK-TMP-2 <= DAG-TIAAMMDD-TOM                       
047443             MOVE ZERO TO OUT-DABERANK                                    
047444           ELSE                                                           
047445             MOVE 20                         TO OUT-DABERANK(1:2)         
047446             MOVE WS-DABERANK-PROP-NEW(INDX) TO OUT-DABERANK(3:6)         
047447           END-IF                                                         
047448         END-IF                                                           
047449       END-IF                                                             
047450                                                                          
047451       PERFORM S11-WRITE-W61229                                           
047452       ADD 1 TO INDX                                                      
047453     END-PERFORM                                                          
047454     MOVE 1 TO INDX                                                       
047455               INDX2                                                      
047456     MOVE ZERO TO WS-MIN-ETA-FOR-WH                                       
047457                  WS-MAX-ETA-FOR-WH                                       
047458                  WS-CNT-CONTAINER                                        
047459                  WS-MAX-CAP-DAY                                          
047460                  WS-MAX-CAP-DAY-INT                                      
047461     MOVE LOW-VALUE TO WS-TBL-REC-WH                                      
047462     .                                                                    
047470     EJECT                                                                
047500 Z-FINIT SECTION.                                                         
047600     CLOSE W612281                                                        
047700           W612282                                                        
047800           W61229                                                         
047900     SKIP2                                                                
048000     MOVE 'S' TO POSTSUM-OPKOD                                            
048100     CALL POSTSUM USING POSTSUM-PARM                                      
048200     .                                                                    
048300     EJECT                                                                
048400 S01-READ-W612281  SECTION.                                               
048500     READ W612281 INTO IN1-AREA                                           
048600     AT END                                                               
048700        MOVE HIGH-VALUE TO IN1-AREA                                       
048800        SET END-OF-W612281 TO TRUE                                        
048900                                                                          
049000     NOT AT END                                                           
049100        MOVE 'W612281' TO POSTSUM-FDNAMN                                  
049200        MOVE 'W61229D1' TO POSTSUM-DDNAMN2                                
049300*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
049400        MOVE SPACES TO POSTSUM-TRANSTYP                                   
049500        CALL POSTSUM USING POSTSUM-PARM                                   
049600     END-READ                                                             
049700     .                                                                    
049800     EJECT                                                                
049900 S02-READ-W612282  SECTION.                                               
050000     READ W612282 INTO IN2-AREA                                           
050100     AT END                                                               
050200        MOVE HIGH-VALUE TO IN2-AREA                                       
050300        SET END-OF-W612282 TO TRUE                                        
050400                                                                          
050500     NOT AT END                                                           
050600        MOVE 'W612282' TO POSTSUM-FDNAMN                                  
050700        MOVE 'W61229D2' TO POSTSUM-DDNAMN2                                
050800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
050900        MOVE SPACES TO POSTSUM-TRANSTYP                                   
051000        CALL POSTSUM USING POSTSUM-PARM                                   
051100     END-READ                                                             
051200     .                                                                    
051300     EJECT                                                                
051400 S11-WRITE-W61229 SECTION.                                                
051500                                                                          
052700                                                                          
052710     WRITE OUT-RECORD FROM OUT-AREA                                       
052800     MOVE 'W61229' TO POSTSUM-FDNAMN                                      
052900     MOVE 'W61229D3' TO POSTSUM-DDNAMN2                                   
052910     MOVE SPACES TO POSTSUM-TRANSTYP                                      
053000     CALL POSTSUM USING POSTSUM-PARM                                      
053800     .                                                                    
053900     EJECT                                                                
054000 S99-ABEND SECTION.                                                       
054100                                                                          
054200     SKIP2                                                                
054300     MOVE 'S' TO POSTSUM-OPKOD                                            
054400     CALL POSTSUM USING POSTSUM-PARM                                      
054500     CALL ABEND USING RKOD-ABEND                                          
054600     .                                                                    
054700     EJECT                                                                
054800* --- IMS SECTIONS  ---                                                   
054900                                                                          
055000     EJECT                                                                
055100 IMS-GU-WDB616 SECTION.                                                   
055200                                                                          
055300     STRING 'WDB601  (IDDC     =' W-IDDC-REC-X ')'                        
055400          DELIMITED BY SIZE INTO SSA1                                     
055500     STRING 'WDB616  (IDDCREF  =' W-IDDC-SEND-X ')'                       
055600          DELIMITED BY SIZE INTO SSA2                                     
055700     MOVE '  GE' TO GOOD-STATUSCODES                                      
055800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-WDB616 SSA1 SSA2              
055900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
056000     PERFORM IMS-STATUSCHECK                                              
056100     .                                                                    
056200     EJECT                                                                
056300 IMS-STATUSCHECK SECTION.                                                 
056400                                                                          
056500     SET STATUS-IX TO 1                                                   
056600     SEARCH GOOD-STATUS                                                   
056700       AT END                                                             
056800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056900           DELIMITED BY SIZE INTO ERROR-TEXT                              
057000         DISPLAY ERROR-TEXT                                               
057100         CALL FELLOG                                                      
057200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
057300         CONTINUE                                                         
057400     END-SEARCH                                                           
057500     .                                                                    
