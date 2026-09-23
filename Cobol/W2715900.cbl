000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2715900.                                                
000400 AUTHOR.         UMESH JAIN.                                              
000500 DATE-WRITTEN.   09/05/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PROGRAM TO TRANSFER DAMAGED PAINTED BUMPERS FROM CDC             
001000*        TO GENT(DC=21)                                                   
001100*                                                                         
001200*        THE PROGRAM READS   TABLE TP4TRAN                                
001300*        THE PROGRAM READS     W01160                                     
001400*        THE PROGRAM READS     WDK7                                       
001500*        THE PROGRAM READS     WDB6                                       
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*    CHANGE LOG:                                                          
002200*                                                                         
002300*      28/01/14 - DATTA ARUP      - ENABLE TO START AREA 88               
002400*                                   ORDERS UPTO 5 DAYS.                   
002500*                                   SCR 10200572                          
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- UNLOAD OF WDK6 DATABASE                                    
003500     SELECT W01160                     ASSIGN TO W27159D1.                
003600     SKIP2                                                                
003700*          --- PAINTED BUMPERS TO BE TRANSFERRED FROM CDC                 
003800     SELECT W27159                     ASSIGN TO W27159D2.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300 FD  W01160                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY W01160      -L.                                                
004800     SKIP3                                                                
004900 FD  W27159                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W27111   -PRE  UT-  -L.                                   
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W2715900'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005900 77  NOO                         PIC X       VALUE 'N'.                   
006000 77  WS-OMRAKTAL-21              PIC 9(1)V9(2) VALUE 0.10.                
006100 77  WS-OMRAKTAL-22              PIC 9(1)V9(2) VALUE 0.04.                
006200 77  WS-OMRAKTAL-23              PIC 9(1)V9(2) VALUE 0.04.                
006300 77  WS-OMRAKTAL-3A              PIC 9(1)V9(2) VALUE 0.04.                
006400 77  WS-OMRAKTAL-24              PIC 9(1)V9(2) VALUE 0.02.                
006500 77  WS-OMRAKTAL-25              PIC 9(1)V9(2) VALUE 0.04.                
006600 77  WS-OMRAKTAL-26              PIC 9(1)V9(2) VALUE 0.04.                
006700 77  WS-PROGNOS               PIC S9(6)V9(1) VALUE ZERO COMP-3.           
006800 77  WS-DISP-BALANCE             PIC S9(7)   VALUE ZERO COMP-3.           
006900 01  SW-AREA88-TRANSF            PIC X(1).                                
007000     88 AREA88-TRANSF                        VALUE 'Y'.                   
007100 01  WS-FLTRLO88-NUM.                                                     
007200     03 WS-FLTRLO88-MAN-NUM      PIC 9(1)    VALUE ZERO.                  
007300     03 WS-FLTRLO88-TIS-NUM      PIC 9(1)    VALUE ZERO.                  
007400     03 WS-FLTRLO88-ONS-NUM      PIC 9(1)    VALUE ZERO.                  
007500     03 WS-FLTRLO88-TOR-NUM      PIC 9(1)    VALUE ZERO.                  
007600     03 WS-FLTRLO88-FRE-NUM      PIC 9(1)    VALUE ZERO.                  
007700 01  CHKP-VAR.                                                            
007800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
007900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
008000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
008100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
008200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
008300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
008400     EJECT                                                                
008500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES TODAYS-DATE.                                        
008700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008900     03  TODAYS-DATE-DAY         PIC 9(2).                                
009000     EJECT                                                                
009100 01  TODAYS-TIAAVVD          PIC  9(5)   VALUE ZERO.                      
009200 01  FILLER REDEFINES TODAYS-TIAAVVD.                                     
009300     03 TODAYS-TIAAVVD-AA    PIC  9(2).                                   
009400     03 TODAYS-TIAAVVD-VV    PIC  9(2).                                   
009500     03 TODAYS-TIAAVVD-D     PIC  9(1).                                   
009600                                                                          
009700 01 NYCKLAR-TP4TRAN.                                                      
009800     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
009900     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
010000*                                                                         
010100 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
010200     88  END-OF-W01160                       VALUE 'Y'.                   
010300                                                                          
010400*      --- VALID IDDC CODES                                               
010500*01    -COPY WWDC99                                                       
010600*                                                                         
010700 01  GENERAL-SUBPROGRAMS.                                                 
010800*                                                                         
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
011500     SKIP2                                                                
011600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011700                                                                          
011800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
012200     SKIP2                                                                
012300 01  ERROR-TEXT.                                                          
012400     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
012500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
012800 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
012900*   -COPY W005WDK7                                                        
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL POSTSUM                                          
013200*                                                                         
013300*01  -COPY W0005   -PRE  POSTSUM-                                         
013400     EJECT                                                                
013500 01  UT-AREA-START               PIC X(24)   VALUE                        
013600                                 'UT-AREA-START  '.                       
013700     SKIP2                                                                
013800                                                                          
013900 01  W01160-AREA-START           PIC X(24)   VALUE                        
014000                                             'W01160-AREA-START'.         
014100*01  -COPY WDATAREA                                                       
014200     EJECT                                                                
014300                                                                          
014400*01  AREA -COPY W01160       -PRE W01160-                                 
014500     EJECT                                                                
014600*01  AREA -COPY W27111       -PRE UT-                                     
014700     EJECT                                                                
014800*    --- AREAS FOR IMS-SECTIONS                                           
014900*                                                                         
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015200     SKIP3                                                                
015300 01  KEYS-FOR-DLI.                                                        
015400     03  W-IDDC-B6-X.                                                     
015500         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
015600     03  W-IDDC-X.                                                        
015700         05 W-IDDC-K7            PIC X(2)    VALUE SPACE.                 
015800     03  W-IDARTNR-K9-X.                                                  
015900         05  W-IDARTNR-K9        PIC S9(9)   VALUE ZERO COMP-3.           
016000     03  W-IDARTNR-X.                                                     
016100         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
016200     03  W-KDSEGKEY-X.                                                    
016300         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016400     SKIP2                                                                
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FOUND                       VALUE '  '.                  
016800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017000     88  IMS-NOT-OK                          VALUE 'XD'.                  
017100     SKIP2                                                                
017200 01  GOOD-STATUSCODES.                                                    
017300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500 01  SSA1                        PIC X(64).                               
017600 01  SSA2                        PIC X(64).                               
017700     EJECT                                                                
017800*    --- IMS FUNCTION CODES                                               
017900*01  -COPY W0003                                                          
018000     EJECT                                                                
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
018300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018400                                                                          
018500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
018600 01  DB2-WS.                                                              
018700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
018800         88  CURSOR-OK                       VALUE 000.                   
018900         88  LINES-FOUND                     VALUE 000.                   
019000         88  LINES-MISSING                   VALUE 100.                   
019100         88  RESOURCE-WRONG                  VALUE 904.                   
019200     03  GOOD-SQLCODECODES.                                               
019300         05  GOOD-SQLCODE OCCURS 5                                        
019400             INDEXED BY SQLCODE-IX PIC 9(3).                              
019500*                                                                         
019600*    ---  DLI INPUT-OUTPUT AREA                                           
019700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019800     SKIP3                                                                
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
020000 01  DLI-IO-WDB601.                                                       
020100*    03  -COPY WDB601                                                     
020200     EJECT                                                                
020300                                                                          
020400 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
020500 01  DLI-IO-WDK711.                                                       
020600*    03  -COPY WDK711                                                     
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK901'.        
021000 01  DLI-IO-WDK901.                                                       
021100*    03  -COPY WDK901                                                     
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
021500*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
021600     EJECT                                                                
021700     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000                                                                          
022100                                                                          
022200*01  -COPY W0008  -PRE MSG-                                               
022300     05  FILLER                  PIC X.                                   
022400                                                                          
022500*01  -COPY W0008  -PRE WDK6-                                              
022600     05  FILLER                  PIC X.                                   
022700                                                                          
022800*01  -COPY W0008  -PRE WDK7-                                              
022900     05  FILLER                  PIC X.                                   
023000                                                                          
023100*01  -COPY W0008  -PRE WDB6-                                              
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE WDK9-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDB6-PCB WDK7-PCB             
023800                           WDK9-PCB.                                      
023900 MAIN SECTION.                                                            
024000     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDB6-PCB WDK7-PCB             
024100                           WDK9-PCB.                                      
024200                                                                          
024300     PERFORM A-INIT                                                       
024400     PERFORM B-GET-DAY-OF-WEEK                                            
024500     IF AREA88-TRANSF                                                     
024600       PERFORM S01-READ-W01160                                            
024700       PERFORM UNTIL END-OF-W01160                                        
024800         IF CHKP-ANT > CHKP-MAX                                           
024900           PERFORM X-TAKE-CHECKPOINT                                      
025000         END-IF                                                           
025100         PERFORM S10-CALCULATE-BALANCE                                    
025200         IF W01160-CLAG-ADLAGOMR = 88 AND WS-DISP-BALANCE  > 0            
025300           PERFORM DB2-SELECT-TP4TRAN                                     
025400           IF LINES-FOUND                                                 
025500             PERFORM C-CREATE-OUTPUT-FILE                                 
025600             PERFORM S02-WRITE-W27159                                     
025700           END-IF                                                         
025800         END-IF                                                           
025900         PERFORM S01-READ-W01160                                          
026000       END-PERFORM                                                        
026100     END-IF                                                               
026200     PERFORM Z-FINIT                                                      
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900     OPEN INPUT  W01160                                                   
027000     OPEN OUTPUT W27159                                                   
027100                                                                          
027200     ACCEPT TODAYS-DATE  FROM DATE                                        
027300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027400                                                                          
027500     INITIALIZE GOOD-SQLCODECODES                                         
027600     PERFORM AA-GET-TID-FLTRLO88-DC11                                     
027700     .                                                                    
027800     EJECT                                                                
027900 AA-GET-TID-FLTRLO88-DC11 SECTION.                                        
028000     MOVE '11' TO W-IDDC-B6                                               
028100     PERFORM IMS-GU-WDB601                                                
028200     IF SEGMENT-FOUND                                                     
028300       IF DCS-KDDC = 'C '                                                 
028400          IF DCS-FLTRLO88-MAN = 'Y'                                       
028500             MOVE 7         TO WS-FLTRLO88-MAN-NUM                        
028600          END-IF                                                          
028700          IF DCS-FLTRLO88-TIS = 'Y'                                       
028800             MOVE 2         TO WS-FLTRLO88-TIS-NUM                        
028900          END-IF                                                          
029000          IF DCS-FLTRLO88-ONS = 'Y'                                       
029100             MOVE 3         TO WS-FLTRLO88-ONS-NUM                        
029200          END-IF                                                          
029300          IF DCS-FLTRLO88-TOR = 'Y'                                       
029400             MOVE 4         TO WS-FLTRLO88-TOR-NUM                        
029500          END-IF                                                          
029600          IF DCS-FLTRLO88-FRE = 'Y'                                       
029700             MOVE 5         TO WS-FLTRLO88-FRE-NUM                        
029800          END-IF                                                          
029900       END-IF                                                             
030000     END-IF                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 B-GET-DAY-OF-WEEK SECTION.                                               
030400     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
030500     MOVE TODAYS-DATE       TO DAT-I-TIDATUM                              
030600                                                                          
030700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
030800                     DAT-O-TIDATUM DAT-KDSVAR                             
030900                                                                          
031000     IF DAT-KDSVAR-OK                                                     
031100       MOVE DAT-TIAAVVD    TO TODAYS-TIAAVVD                              
031200       PERFORM BA-CHECK-AREA88-TRANSF-DAY                                 
031300     ELSE                                                                 
031400       DISPLAY 'ERROR FROM DATKONV IN B-GET-DAY-OF-WEEK'                  
031500       CALL FELLOG                                                        
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 BA-CHECK-AREA88-TRANSF-DAY SECTION.                                      
032000     MOVE 'N'                   TO SW-AREA88-TRANSF                       
032100                                                                          
032200     IF WS-FLTRLO88-MAN-NUM     =  TODAYS-TIAAVVD-D                       
032300        MOVE 'Y'                TO SW-AREA88-TRANSF                       
032400     ELSE                                                                 
032500      IF WS-FLTRLO88-TIS-NUM    =  TODAYS-TIAAVVD-D                       
032600         MOVE 'Y'               TO SW-AREA88-TRANSF                       
032700      ELSE                                                                
032800       IF WS-FLTRLO88-ONS-NUM   =  TODAYS-TIAAVVD-D                       
032900          MOVE 'Y'              TO SW-AREA88-TRANSF                       
033000       ELSE                                                               
033100        IF WS-FLTRLO88-TOR-NUM  =  TODAYS-TIAAVVD-D                       
033200           MOVE 'Y'             TO SW-AREA88-TRANSF                       
033300        ELSE                                                              
033400         IF WS-FLTRLO88-FRE-NUM =  TODAYS-TIAAVVD-D                       
033500            MOVE 'Y'            TO SW-AREA88-TRANSF                       
033600         END-IF                                                           
033700        END-IF                                                            
033800       END-IF                                                             
033900      END-IF                                                              
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 C-CREATE-OUTPUT-FILE SECTION.                                            
034400                                                                          
034500     MOVE '21'                TO W-IDDC-X                                 
034600     MOVE W01160-CLAG-IDARTNR TO W-IDARTNR-K7                             
034700     PERFORM IMS-GU-WDK711                                                
034800     IF SEGMENT-FOUND                                                     
034900       MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY                          
035000       MOVE SLAG-ADLAGOMR     TO UT-ADLAGOMR-SDC                          
035100       MOVE SLAG-ADGANG       TO UT-ADGANG-SDC                            
035200       MOVE SLAG-ADPLATS      TO UT-ADPLATS-SDC                           
035300       MOVE SLAG-IDLEVNR      TO UT-IDLEVNR                               
035400     ELSE                                                                 
035500       MOVE ALL '+'      TO WDK7-W005WDK7                                 
035600       MOVE 'WDK711'     TO WDK7-IDSEGM                                   
035700       MOVE W-IDARTNR-K7 TO WDK7-IDARTNR-KFB                              
035800       MOVE W-IDDC-X     TO WDK7-IDDC-KFB                                 
035900                            WDK7-IDDC                                     
036000       PERFORM CA-INDATA-WDK711                                           
036100                                                                          
036200       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
036300                                         WDK7-PCB                         
036400                                                                          
036500       ADD +1 TO CHKP-ANT                                                 
036600       MOVE WDK7-IDPERSON-BUY TO UT-IDPERSON-BUY                          
036700       MOVE WDK7-ADLAGOMR     TO UT-ADLAGOMR-SDC                          
036800       MOVE WDK7-ADGANG       TO UT-ADGANG-SDC                            
036900       MOVE WDK7-ADPLATS      TO UT-ADPLATS-SDC                           
037000       MOVE WDK7-IDLEVNR IN WDK7-WDK711                                   
037100                              TO UT-IDLEVNR                               
037200     END-IF                                                               
037300     MOVE TP4TRAN-IDDC-REC    TO UT-IDDC                                  
037400     MOVE 'T'                 TO UT-KDREFTYP                              
037500     MOVE W01160-CLAG-IDARTNR TO UT-IDARTNR                               
037600     MOVE TP4TRAN-IDDISTR     TO UT-IDDISTR                               
037700     MOVE W01160-CLAG-ADLAGOMR TO UT-ADLAGOMR-CDC                         
037800     MOVE W01160-CLAG-ADGANG  TO UT-ADGANG-CDC                            
037900     MOVE W01160-CLAG-ADPLATS TO UT-ADPLATS-CDC                           
038000     MOVE WS-DISP-BALANCE     TO UT-KVBEART                               
038100     MOVE 'O'                 TO UT-KDREFORS                              
038200     MOVE 0                   TO UT-KDREFTXT                              
038300     MOVE 0                   TO UT-KDFRAKT                               
038400     MOVE 0                   TO UT-KVBEART-CD                            
038500     MOVE 0                   TO UT-ADLAGOMR-CD                           
038600     MOVE 0                   TO UT-ADGANG-CD                             
038700     MOVE 0                   TO UT-ADPLATS-CD                            
038800     MOVE TP4TRAN-IDKUNDNR    TO UT-IDKUNDNR                              
038900     MOVE TP4TRAN-IDDC-SEND   TO UT-IDDC-REF                              
039000     .                                                                    
039100     EJECT                                                                
039200 CA-INDATA-WDK711 SECTION.                                                
039300                                                                          
039400     MOVE 'A'             TO WDK7-KDREFSTA                                
039500     MOVE TODAYS-DATE     TO WDK7-TIREFSTA                                
039600                             WDK7-TIREFMPB                                
039700     COMPUTE WS-PROGNOS = W01160-CLAG-KVPB-SEP * 0.73                     
039800     EVALUATE TRUE                                                        
039900        WHEN SDC-NL                                                       
040000           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-21            
040100*       WHEN '22'                                                         
040200*          COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-22            
040300*       WHEN SDC-GB                                                       
040400*          COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-23            
040500        WHEN LDC-GB-3A                                                    
040600           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-3A            
040700        WHEN SDC-ES                                                       
040800           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-24            
040900        WHEN SDC-IT                                                       
041000           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-25            
041100        WHEN SDC-AT                                                       
041200           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-26            
041300     END-EVALUATE                                                         
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 Z-FINIT SECTION.                                                         
041800     CLOSE W01160                                                         
041900           W27159                                                         
042000     SKIP2                                                                
042100     MOVE 'S' TO POSTSUM-OPKOD                                            
042200     CALL POSTSUM USING POSTSUM-PARM                                      
042300     .                                                                    
042400     EJECT                                                                
042500** READ INPUT FILE W01160                                                 
042600 S01-READ-W01160  SECTION.                                                
042700     SKIP2                                                                
042800     READ W01160 INTO W01160-AREA                                         
042900     AT END                                                               
043000        SET END-OF-W01160 TO TRUE                                         
043100                                                                          
043200     NOT AT END                                                           
043300        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
043400        MOVE 'W61321D1' TO POSTSUM-DDNAMN2                                
043500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
043600        CALL POSTSUM USING POSTSUM-PARM                                   
043700     END-READ                                                             
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100 S02-WRITE-W27159 SECTION.                                                
044200     WRITE UT-POST FROM UT-AREA                                           
044300                                                                          
044400     MOVE ' '       TO POSTSUM-TRANSTYP                                   
044500     MOVE 'W27159' TO POSTSUM-FDNAMN                                      
044600     MOVE 'W27159D2' TO POSTSUM-DDNAMN2                                   
044700     CALL POSTSUM USING POSTSUM-PARM                                      
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 S10-CALCULATE-BALANCE SECTION.                                           
045200                                                                          
045300     MOVE ZERO                   TO WS-DISP-BALANCE                       
045400     MOVE W01160-CLAG-IDARTNR    TO W-IDARTNR-K9                          
045500     PERFORM IMS-GU-WDK901                                                
045600     IF SEGMENT-FOUND                                                     
045700        COMPUTE WS-DISP-BALANCE   = W01160-CLAG-KVLS                      
045800                                  - ART-KVOKS-DAG                         
045900                                  - ART-KVOKS-BULK                        
046000     ELSE                                                                 
046100        MOVE W01160-CLAG-KVLS    TO WS-DISP-BALANCE                       
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 S99-ABEND SECTION.                                                       
046600     SKIP2                                                                
046700     MOVE 'S' TO POSTSUM-OPKOD                                            
046800     CALL POSTSUM USING POSTSUM-PARM                                      
046900     CALL ABEND USING RKOD-ABEND                                          
047000     .                                                                    
047100     EJECT                                                                
047200* --- IMS SECTIONS  ---                                                   
047300                                                                          
047400 X-TAKE-CHECKPOINT   SECTION.                                             
047500                                                                          
047600* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
047700* --- SAVE DATABASE KEYS IF NECESSARY                                     
047800     PERFORM IMS-CHECKPOINT                                               
047900     MOVE ZERO TO CHKP-ANT                                                
048000* --- REREAD DATABASE IF NECESSARY                                        
048100     .                                                                    
048200     EJECT                                                                
048300* --- IMS SECTIONS  ---                                                   
048400                                                                          
048500     EJECT                                                                
048600 IMS-GU-WDK711 SECTION.                                                   
048700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
048800          DELIMITED BY SIZE INTO SSA1                                     
048900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
049000          DELIMITED BY SIZE INTO SSA2                                     
049100     MOVE '  GE' TO GOOD-STATUSCODES                                      
049200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
049300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSCHECK                                              
049500     .                                                                    
049600     EJECT                                                                
049700 IMS-GU-WDB601 SECTION.                                                   
049800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     MOVE '  GE' TO GOOD-STATUSCODES                                      
050100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
050200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSCHECK                                              
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GU-WDK901 SECTION.                                                   
050700     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-K9-X ')'                      
050800            DELIMITED BY SIZE INTO SSA1                                   
050900     MOVE '  GE' TO GOOD-STATUSCODES                                      
051000     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
051100     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSCHECK                                              
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-RESTART SECTION.                                                     
051600     SKIP2                                                                
051700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
051800     MOVE '  ' TO GOOD-STATUSCODES                                        
051900     CALL CBLTDLI USING XRST MSG-PCB                                      
052000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
052100                        CHKP-AREA-LENGTH CHKP-AREA                        
052200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
052300     PERFORM IMS-STATUSCHECK                                              
052400     .                                                                    
052500     SKIP3                                                                
052600 IMS-CHECKPOINT SECTION.                                                  
052700     SKIP2                                                                
052800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
052900     MOVE '  XD' TO GOOD-STATUSCODES                                      
053000     CALL CBLTDLI USING CHKP MSG-PCB                                      
053100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
053200                        CHKP-AREA-LENGTH CHKP-AREA                        
053300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053400     PERFORM IMS-STATUSCHECK                                              
053500                                                                          
053600     IF IMS-NOT-OK                                                        
053700       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
053800       DISPLAY ERROR-TEXT                                                 
053900       CALL FELLOG                                                        
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 IMS-STATUSCHECK SECTION.                                                 
054400     SET STATUS-IX TO 1                                                   
054500     SEARCH GOOD-STATUS                                                   
054600       AT END                                                             
054700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054800           DELIMITED BY SIZE INTO ERROR-TEXT                              
054900         DISPLAY ERROR-TEXT                                               
055000         CALL FELLOG                                                      
055100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
055200         CONTINUE                                                         
055300     END-SEARCH                                                           
055400     .                                                                    
055500     EJECT                                                                
055600 DB2-SELECT-TP4TRAN     SECTION.                                          
055700     MOVE 000100  TO GOOD-SQLCODECODES                                    
055800     MOVE '11'    TO WS-IDDC-SEND                                         
055900     MOVE '21'    TO WS-IDDC-REC                                          
056000                                                                          
056100     EXEC SQL                                                             
056200           SELECT  IDDC_SEND                                              
056300                  ,IDDC_REC                                               
056400                  ,IDDISTR                                                
056500                  ,IDKUNDNR                                               
056600                                                                          
056700           INTO   :TP4TRAN-IDDC-SEND                                      
056800                 ,:TP4TRAN-IDDC-REC                                       
056900                 ,:TP4TRAN-IDDISTR                                        
057000                 ,:TP4TRAN-IDKUNDNR                                       
057100                                                                          
057200           FROM    TP4TRAN                                                
057300                                                                          
057400           WHERE IDDC_SEND = :WS-IDDC-SEND                                
057500           AND   IDDC_REC  = :WS-IDDC-REC                                 
057600           AND   KDARBTYP  = 'ESC'                                        
057700     END-EXEC                                                             
057800                                                                          
057900     MOVE SQLCODE TO SQLCODE-WS                                           
058000     PERFORM DB2-STATUS-CHECK                                             
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 DB2-STATUS-CHECK  SECTION.                                               
058500     SET SQLCODE-IX TO 1                                                  
058600     SEARCH GOOD-SQLCODE                                                  
058700       AT END                                                             
058800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
058900          DELIMITED BY SIZE INTO ERROR-TEXT                               
059000          CALL ABEND USING RKOD-ABEND-DB2                                 
059100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
059200     END-SEARCH                                                           
059300     .                                                                    
