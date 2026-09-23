000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2219600.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/09/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*    CREATE/DELETE ALERT ON SCREEN 2171/2471 WHEN TOTAL DEMAND            
001000*    WITHIN ANY WEEK DURING CURRENT WEEK + LEADTIME FOR THE MAIN          
001100*    SUPPLIER IS LARGER THAN THE TOTAL ASSETS ON THE DC UP                
001200*    UNTIL THAT WEEK.                                                     
001300*                                                                         
001400*    INSERT/UPDATE WDGX2223/24                                            
001500                                                                          
001510****************************************************************          
001520*    ÄNDRINGAR:                                                           
001521*                                                                         
001522*    20160629                                                             
001530*    ETRACKER: 10273782 BACKORDER ALARM TAKES AWAY 223-ALARM              
001540*    INGER STENING                                                        
001550*                                                                         
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INPUT FILE FROM W22194 OR W2219B - INSERT/REPLACE          
002400     SELECT W2219601                   ASSIGN TO W22196D1.                
002500*          --- INPUT FILE FROM W22194 OR W2219B - DELETE                  
002600     SELECT W2219602                   ASSIGN TO W22196D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W2219601                                                             
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W2219401    -PRE  IN1- -L.                                     
003700     EJECT                                                                
003800 FD  W2219602                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W2219401    -PRE  IN2- -L.                                     
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W2219600'.            
004700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004800 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004900                                                                          
004910 01  W-TIREGDAT                  PIC 9(06).                               
004911 01  W-YYWWD.                                                             
004920     03  W-YYWWD-NUM             PIC 9(05).                               
004930     03  W-YYWWD REDEFINES W-YYWWD-NUM.                                   
004940         05 W-YYWW               PIC 9(04).                               
004950         05 W-TODAYS-DAGNR       PIC 9(01).                               
004960                                                                          
005000 01  CHKP-VAR.                                                            
005100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005600     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005700                                                                          
005800 01  NEXT-KLOCKSLAG.                                                      
005900   03  WS-NXT-KL                 PIC 9(6).                                
006000   03  FILLER                    REDEFINES WS-NXT-KL.                     
006100     05  WS-NXT-KL-TT            PIC 9(2).                                
006200     05  WS-NXT-KL-MM            PIC 9(2).                                
006300     05  WS-NXT-KL-SS            PIC 9(2).                                
006400                                                                          
006500 77  YES                         PIC X       VALUE 'J'.                   
006600 77  NOO                         PIC X       VALUE 'N'.                   
006700     SKIP2                                                                
006710 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006720 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +1000.            
006730 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 01  ERR-TEXT.                                                            
006900     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007000     03  ERR-TEXT-STR            PIC X(72)   VALUE SPACE.                 
007100                                                                          
007200 77  W2219601-EOF-SW             PIC X       VALUE 'N'.                   
007300     88  END-OF-W2219601                     VALUE 'Y'.                   
007400 77  W2219602-EOF-SW             PIC X       VALUE 'N'.                   
007500     88  END-OF-W2219602                     VALUE 'Y'.                   
007600     EJECT                                                                
007700 77  DAGENS-DATUM-Y2K            PIC 9(8)    VALUE ZERO.                  
007800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES TODAYS-DATE.                                        
008000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008200     03  TODAYS-DATE-DAY         PIC 9(2).                                
008300     EJECT                                                                
008400 01  GENERAL-SUBPROGRAMS.                                                 
008500*                                                                         
008600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008801     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
008810     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008820     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008900     EJECT                                                                
008910*01  -COPY WDATAREA                                                       
008920     EJECT                                                                
008930 01  FILLER                  PIC X(16) VALUE 'WZ20DAYS        '.          
008940*    ---PARAMETRAR TILL WZ20DAYS                                          
008950*01  -COPY WZ20DAYS                                                       
008960     EJECT                                                                
008970*01  -COPY WWDC99                                                         
008980     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400 01  IN-AREA-START               PIC X(24)   VALUE                        
009500                                             'IN-AREA-START'.             
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W2219401   -PRE IN-                                       
009900*                                                                         
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  KEYS-TILL-DLI.                                                       
010400                                                                          
010500     03  W-WDGXKEY-2223-X.                                                
010600         05  W-IDHTYP-2223       PIC X(4)    VALUE '2223'.                
010700         05  W-IDANSK-2223       PIC S9(3)   COMP-3 VALUE ZERO.           
010800         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
010900     03  W-IDDC-X.                                                        
011000         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
011100     03  W-KDLARM-X.                                                      
011200         05  W-KDLARM            PIC S9(3)   COMP-3 VALUE ZERO.           
011300     03  W-IDARTNR-X.                                                     
011310         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011320                                                                          
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FOUND                       VALUE '  '.                  
011700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
012000     88  IMS-NOT-OK                          VALUE 'XD'.                  
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
013200                                                                          
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
013400 01  DLI-IO-WDGX2223.                                                     
013500*    03  -COPY WDGX2223                                                   
013600     EJECT                                                                
013700                                                                          
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
013900 01  DLI-IO-WDGX2224.                                                     
014000*    03  -COPY WDGX2224                                                   
014100     EJECT                                                                
014101                                                                          
014110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014120 01  DLI-IO-WDK611.                                                       
014130*    03  -COPY WDK611                                                     
014140     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400*01  -COPY W0009  -PRE MSG-                                               
014500                                                                          
014600*01  -COPY W0008  -PRE WDR5-                                              
014700     05  FILLER                  PIC X.                                   
014710*01  -COPY W0008  -PRE WDK6-                                              
014720     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION  USING MSG-PCB WDR5-PCB WDK6-PCB.                     
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB WDR5-PCB WDK6-PCB.                     
015200                                                                          
015300     PERFORM A-INIT                                                       
015400                                                                          
015500* DELETE ALERT                                                            
015600     PERFORM S01A-READ-W2219602                                           
015700     PERFORM UNTIL END-OF-W2219602                                        
015800       IF CHKP-ANT > CHKP-MAX                                             
015900         PERFORM X-TAKE-CHECKPOINT                                        
016000       END-IF                                                             
016100                                                                          
016200       PERFORM HA-DELETE-ALERT                                            
016300                                                                          
016400       PERFORM S01A-READ-W2219602                                         
016500     END-PERFORM                                                          
016600                                                                          
016700* INSERT/REPLACE ALERT                                                    
016800     PERFORM S01B-READ-W2219601                                           
016900     PERFORM UNTIL END-OF-W2219601                                        
017000       IF CHKP-ANT > CHKP-MAX                                             
017100         PERFORM X-TAKE-CHECKPOINT                                        
017200       END-IF                                                             
017300                                                                          
017400       PERFORM HB-CREATE-ALERT                                            
017500                                                                          
017600       PERFORM S01B-READ-W2219601                                         
017700     END-PERFORM                                                          
017800                                                                          
017900     PERFORM Z-FINIT                                                      
018000                                                                          
018100     MOVE ZERO TO RETURN-CODE                                             
018200     GOBACK                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 A-INIT SECTION.                                                          
018600     MOVE 'A-INIT              ' TO CURRENT-SECTION                       
018700                                                                          
018800     PERFORM IMS-RESTART                                                  
018900                                                                          
019000     OPEN INPUT W2219601                                                  
019100                W2219602                                                  
019200                                                                          
019300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
019400                                                                          
019500     ACCEPT TODAYS-DATE  FROM DATE                                        
019600                                                                          
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800     .                                                                    
019900     EJECT                                                                
020000 HA-DELETE-ALERT SECTION.                                                 
020100     MOVE 'HA-DELETE-ALERT      ' TO CURRENT-SECTION                      
020200                                                                          
021020     MOVE IN-IDANSK            TO W-IDANSK-2223                           
021040     PERFORM IMS-GU-WDGX2223                                              
021050     IF SEGMENT-FOUND                                                     
021060        MOVE IN-IDARTNR           TO W-IDARTNR                            
021070        MOVE IN-IDDC              TO W-IDDC                               
021080        MOVE IN-KDLARM            TO W-KDLARM                             
021090        PERFORM IMS-GHNP-WDGX2224                                         
021091        IF SEGMENT-FOUND                                                  
021092          PERFORM IMS-DLET-WDGX2224                                       
021093        END-IF                                                            
021094     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300                                                                          
021400 HB-CREATE-ALERT SECTION.                                                 
021500     MOVE 'HB-CREATE-ALERT      ' TO CURRENT-SECTION                      
021600                                                                          
021700     MOVE IN-IDANSK            TO W-IDANSK-2223                           
021800     PERFORM IMS-GU-WDGX2223                                              
021900     IF SEGMENT-FOUND                                                     
022000**** LARM 223 SKAPAS INTE OM 210 LARM FINNS ***                           
022100       MOVE '210'                  TO W-KDLARM                            
022200       MOVE IN-IDARTNR             TO W-IDARTNR                           
022300       MOVE IN-IDDC                TO W-IDDC                              
022400       PERFORM IMS-GNP-WDGX2224                                           
022500       IF SEGMENT-MISSING                                                 
022600          PERFORM HBB-CREATE-224-ALARM                                    
022700       END-IF                                                             
022800     ELSE                                                                 
022820       MOVE '2223'                 TO 2223-IDHTYP                         
022830       MOVE IN-IDANSK              TO 2223-IDANSK                         
022832       MOVE LOW-VALUE              TO 2223-LOW-VALUE                      
022840       PERFORM IMS-ISRT-WDGX2223                                          
022900       PERFORM HBB-CREATE-224-ALARM                                       
023000     END-IF                                                               
023100     .                                                                    
023200 HBB-CREATE-224-ALARM SECTION.                                            
023300     MOVE 'HBB-CREATE-224-ALARM ' TO CURRENT-SECTION                      
024120                                                                          
024130     PERFORM IMS-GU-WDGX2223                                              
024140                                                                          
024200     MOVE IN-KDLARM            TO W-KDLARM                                
024300     MOVE IN-IDARTNR           TO W-IDARTNR                               
024400     MOVE IN-IDDC              TO W-IDDC                                  
024500     PERFORM IMS-GHNP-WDGX2224                                            
024600     IF SEGMENT-MISSING                                                   
024700       PERFORM HBBA-BUILD-WDGX2224                                        
024800       PERFORM IMS-ISRT-WDGX2224                                          
024900       PERFORM UNTIL (NOT SEGMENT-FOUND-EXISTS)                           
025000         MOVE 2224-TISENBEK-KL TO WS-NXT-KL                               
025100         PERFORM HBBB-NXT-SEKUND                                          
025200         MOVE WS-NXT-KL        TO 2224-TISENBEK-KL                        
025300         PERFORM IMS-ISRT-WDGX2224                                        
025400       END-PERFORM                                                        
025500     ELSE                                                                 
025600       MOVE TODAYS-DATE        TO 2224-TIREGDAT                           
025700       PERFORM IMS-REPL-WDGX2224                                          
025800     END-IF                                                               
025801                                                                          
025802     MOVE IN-IDDC              TO WS-IDDC                                 
025803     IF CDC-SE                                                            
025810        PERFORM HBBC-UPDATE-TISTODAT-ALARM                                
025820     END-IF                                                               
025900                                                                          
026000**** LARM 222 BORTTAGES NÄR LARM 223 SKAPAS FÖR SAMMA ARTNR ***           
026100     MOVE '222'                TO W-KDLARM                                
026200     MOVE IN-IDARTNR           TO W-IDARTNR                               
026300     MOVE IN-IDDC              TO W-IDDC                                  
026400     PERFORM IMS-GHNP-WDGX2224-FIRST                                      
026500     IF SEGMENT-FOUND                                                     
026600       PERFORM IMS-DLET-WDGX2224                                          
026710     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000                                                                          
027100 HBBA-BUILD-WDGX2224 SECTION.                                             
027200     MOVE 'HBBA-BUILD-WDGX2224  ' TO CURRENT-SECTION                      
027300                                                                          
027530     MOVE 'AAVVD '             TO DAT-KDDATFORM                           
027531     MOVE IN-TIBEHOV-FIRST     TO W-YYWW                                  
027532     MOVE 1                    TO W-TODAYS-DAGNR                          
027533     MOVE W-YYWWD-NUM          TO DAT-I-TIDATUM                           
027536     CALL WDATKONV USING DAT-KDDATFORM                                    
027537                   DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR                 
027538     IF DAT-KDSVAR-FEL                                                    
027539        MOVE FUNCTION CURRENT-DATE(3:6)  TO  2224-TISENBEK-DAG            
027540        MOVE FUNCTION CURRENT-DATE(11:6) TO  2224-TISENBEK-KL             
027542     ELSE                                                                 
027543        MOVE DAT-TIAAMMDD                TO  2224-TISENBEK-DAG            
027544        MOVE FUNCTION CURRENT-DATE(11:6) TO  2224-TISENBEK-KL             
027546     END-IF                                                               
027550                                                                          
027600     MOVE IN-KDLARM           TO 2224-KDLARM                              
027700     MOVE IN-IDARTNR          TO 2224-IDARTNR                             
027800     MOVE IN-IDDC             TO 2224-IDDC                                
027900     MOVE IN-FLNYLARM         TO 2224-FLNYLARM                            
028000     MOVE IN-IDDISTR          TO 2224-IDDISTR                             
028100     MOVE IN-IDKUNDNR         TO 2224-IDKUNDNR                            
028200     MOVE IN-IDKUNDNR         TO 2224-IDKUNDRF                            
028300     MOVE IN-IDLOPNR          TO 2224-IDLOPNR                             
028400     MOVE TODAYS-DATE         TO 2224-TIREGDAT                            
028500     MOVE IN-IDTRANS          TO 2224-IDTRANS                             
028600     MOVE IN-KDMFSFOR         TO 2224-KDMFSFOR                            
028700     MOVE IN-IDKR             TO 2224-IDKR                                
028800     MOVE IN-IDLEVNR          TO 2224-IDLEVNR                             
028900                                                                          
029000     .                                                                    
029100     EJECT                                                                
029200 HBBB-NXT-SEKUND SECTION.                                                 
029300     MOVE 'HBBB-NXT-SEKUND      ' TO CURRENT-SECTION                      
029400*                                                                         
029500*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
029600*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
029700*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
029800*                                                                         
029900     ADD 1                   TO WS-NXT-KL-SS                              
030000     IF  WS-NXT-KL-SS > 59                                                
030100       MOVE ZERO             TO WS-NXT-KL-SS                              
030200       ADD 1                 TO WS-NXT-KL-MM                              
030300       IF  WS-NXT-KL-MM > 59                                              
030400         MOVE ZERO           TO WS-NXT-KL-MM                              
030500         ADD 1               TO WS-NXT-KL-TT                              
030600         IF  WS-NXT-KL-TT > 23                                            
030700           MOVE ZERO         TO WS-NXT-KL-TT                              
030800         END-IF                                                           
030900       END-IF                                                             
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031210 HBBC-UPDATE-TISTODAT-ALARM SECTION.                                      
031220     MOVE 'HBBC-UPDATE-TISTODAT-ALARM'  TO CURRENT-SECTION                
031222                                                                          
031224     PERFORM IMS-GU-WDK611                                                
031225     IF SEGMENT-FOUND                                                     
031229        IF CLAG-TISTODAT-LARM > 2224-TIREGDAT                             
031230           CONTINUE                                                       
031231        ELSE                                                              
031232           INITIALIZE DAYS-WZ20DAYS                                       
031233           MOVE 2224-TIREGDAT            TO W-TIREGDAT                    
031234           MOVE W-TIREGDAT               TO DAYS-TIDATE1                  
031235           MOVE 7                        TO DAYS-KVDAYS                   
031236           MOVE 'YYMMDD'                 TO DAYS-KDDATFMT1                
031237           MOVE 'YYMMDD'                 TO DAYS-KDDATFMT2                
031238           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
031239                                                                          
031240           IF DAYS-KDRC = 8                                               
031241             STRING 'FEL VID ANROP TILL WZ20DAYS'                         
031242             DELIMITED BY SIZE INTO ERR-TEXT-STR                          
031243             DISPLAY ERR-TEXT                                             
031244             CALL ABEND USING RKOD-ABEND-NO-DUMP                          
031245           ELSE                                                           
031246             MOVE DAYS-TIDATE2(1:6)      TO CLAG-TISTODAT-LARM            
031247             DISPLAY 'DAYS-TIDATE2 ' DAYS-TIDATE2                         
031248                ' CLAG-TISTODAT-LARM ' CLAG-TISTODAT-LARM                 
031249             PERFORM IMS-REPL-WDK611                                      
031250           END-IF                                                         
031253        END-IF                                                            
031254     END-IF                                                               
031255     .                                                                    
031260     EJECT                                                                
031300 Z-FINIT SECTION.                                                         
031400     MOVE 'Z-FINIT             ' TO CURRENT-SECTION                       
031500                                                                          
031600     CLOSE W2219601                                                       
031700           W2219602                                                       
031800                                                                          
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300 S01A-READ-W2219602 SECTION.                                              
032400     MOVE 'S01A-READ-W2219602     ' TO CURRENT-SECTION                    
032500                                                                          
032600     READ W2219602 INTO IN-AREA                                           
032700     AT END                                                               
032800        SET END-OF-W2219602 TO TRUE                                       
032900                                                                          
033000     NOT AT END                                                           
033100        MOVE '      '   TO POSTSUM-FDNAMN                                 
033200        MOVE 'W22196D2' TO POSTSUM-DDNAMN2                                
033300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
033400        CALL POSTSUM USING POSTSUM-PARM                                   
033500     END-READ                                                             
033600     .                                                                    
033700     EJECT                                                                
033800 S01B-READ-W2219601 SECTION.                                              
033900     MOVE 'S01B-READ-W2219601     ' TO CURRENT-SECTION                    
034000                                                                          
034100     READ W2219601 INTO IN-AREA                                           
034200     AT END                                                               
034300        SET END-OF-W2219601 TO TRUE                                       
034400                                                                          
034500     NOT AT END                                                           
034600        MOVE '      '   TO POSTSUM-FDNAMN                                 
034700        MOVE 'W22196D1' TO POSTSUM-DDNAMN2                                
034800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
034900        CALL POSTSUM USING POSTSUM-PARM                                   
035000     END-READ                                                             
035100     .                                                                    
035200     EJECT                                                                
035300 X-TAKE-CHECKPOINT   SECTION.                                             
035400     MOVE 'X-TAKE-CHECKPOINT   ' TO CURRENT-SECTION                       
035500                                                                          
035600* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
035700* --- SAVE DATABASE KEYS IF NECESSARY                                     
035800     PERFORM IMS-CHECKPOINT                                               
035900     MOVE ZERO TO CHKP-ANT                                                
036000* --- REREAD DATABASE IF NECESSARY                                        
036100     .                                                                    
036200     EJECT                                                                
036300* --- IMS SECTIONS  ---                                                   
036301                                                                          
036310 IMS-GU-WDGX2223 SECTION.                                                 
036320     MOVE 'IMS-GU-WDGX2223   '  TO DBS-SECTION                            
036330                                                                          
036340     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
036350          DELIMITED BY SIZE INTO SSA1                                     
036360     MOVE '  GE'              TO GOOD-STATUSCODES                         
036370     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2223 SSA1                  
036380     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
036390     PERFORM IMS-STATUSCHECK                                              
036391     .                                                                    
036400                                                                          
037600 IMS-GNP-WDGX2224 SECTION.                                                
037700     MOVE 'IMS-GHNP-WDGX2224  '  TO DBS-SECTION                           
037800                                                                          
037900     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-X                             
038000                    '&IDDC     =' W-IDDC-X                                
038100                    '&KDLARM   =' W-KDLARM-X ')'                          
038200          DELIMITED BY SIZE INTO SSA1                                     
038300     MOVE '  GE'           TO GOOD-STATUSCODES                            
038400     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2224 SSA1                 
038500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
038600     PERFORM IMS-STATUSCHECK                                              
038700     .                                                                    
038800                                                                          
038900 IMS-GHU-WDGX2223 SECTION.                                                
039000     MOVE 'IMS-GHU-WDGX2223   '  TO DBS-SECTION                           
039100                                                                          
039200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
039300          DELIMITED BY SIZE INTO SSA1                                     
039400     MOVE '  GE'              TO GOOD-STATUSCODES                         
039500     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2223 SSA1                 
039600     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
039700     PERFORM IMS-STATUSCHECK                                              
039800     .                                                                    
039900                                                                          
040000 IMS-GHNP-WDGX2224 SECTION.                                               
040100     MOVE 'IMS-GHNP-WDGX2224  '  TO DBS-SECTION                           
040200                                                                          
040300     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-X                             
040400                    '&IDDC     =' W-IDDC-X                                
040500                    '&KDLARM   =' W-KDLARM-X ')'                          
040600          DELIMITED BY SIZE INTO SSA1                                     
040700     MOVE '  GE'           TO GOOD-STATUSCODES                            
040800     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX2224 SSA1                
040900     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
041000     PERFORM IMS-STATUSCHECK                                              
041100     .                                                                    
041200                                                                          
041300 IMS-GHNP-WDGX2224-FIRST SECTION.                                         
041400     MOVE 'IMS-GHNP-WDGX2224-FIRST  '  TO DBS-SECTION                     
041500                                                                          
041600     STRING 'WDR550  *F(IDARTNR  =' W-IDARTNR-X                           
041700                      '&IDDC     =' W-IDDC-X                              
041800                      '&KDLARM   =' W-KDLARM-X ')'                        
041900          DELIMITED BY SIZE INTO SSA1                                     
042000     MOVE '  GE'           TO GOOD-STATUSCODES                            
042100     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX2224 SSA1                
042200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
042300     PERFORM IMS-STATUSCHECK                                              
042400     .                                                                    
042500                                                                          
042600 IMS-ISRT-WDGX2223 SECTION.                                               
042700     MOVE 'IMS-ISRT-WDGX2223  '  TO DBS-SECTION                           
042800                                                                          
042900     MOVE 'WDR501   '      TO SSA1                                        
043000     MOVE '  II'           TO GOOD-STATUSCODES                            
043100     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2223 SSA1                
043200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSCHECK                                              
043400     ADD +1                TO CHKP-ANT                                    
043500     .                                                                    
043600                                                                          
043700 IMS-ISRT-WDGX2224 SECTION.                                               
043800     MOVE 'IMS-ISRT-WDGX2224  '  TO DBS-SECTION                           
043900                                                                          
044000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
044100          DELIMITED BY SIZE INTO SSA1                                     
044200     MOVE 'WDR550   '      TO SSA2                                        
044300     MOVE '  II'           TO GOOD-STATUSCODES                            
044400     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2224 SSA1 SSA2           
044500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSCHECK                                              
044700     ADD +1  TO CHKP-ANT                                                  
044800     .                                                                    
044900                                                                          
045000 IMS-REPL-WDGX2224 SECTION.                                               
045100     MOVE 'IMS-REPL-WDGX2224  '  TO DBS-SECTION                           
045200                                                                          
045300     MOVE '  '             TO GOOD-STATUSCODES                            
045400     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX2224                     
045500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSCHECK                                              
045700     ADD +1  TO CHKP-ANT                                                  
045800     .                                                                    
046000 IMS-DLET-WDGX2224 SECTION.                                               
046100     MOVE 'IMS-DLET-WDGX2224  '  TO DBS-SECTION                           
046200                                                                          
046300     MOVE '  '                   TO GOOD-STATUSCODES                      
046400     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX2224                     
046500     MOVE WDR5-STATUS-CODE       TO STATUS-WS                             
046600     PERFORM IMS-STATUSCHECK                                              
046700     .                                                                    
046800                                                                          
046900     EJECT                                                                
046910 IMS-GU-WDK611 SECTION.                                                   
046911     MOVE 'IMS-GU-WDK611      '  TO DBS-SECTION                           
046912                                                                          
046913     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
046914       DELIMITED BY SIZE INTO SSA1                                        
046915     MOVE 'WDK611   '      TO SSA2                                        
046916     MOVE '  GE'           TO GOOD-STATUSCODES                            
046917     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
046918     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
046919     PERFORM IMS-STATUSCHECK                                              
046920     .                                                                    
046922                                                                          
046925 IMS-REPL-WDK611 SECTION.                                                 
046926     MOVE 'IMS-REPL-WDK611    '  TO DBS-SECTION                           
046927                                                                          
046928     MOVE '  '             TO GOOD-STATUSCODES                            
046929     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
046930     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
046931     PERFORM IMS-STATUSCHECK                                              
046934     .                                                                    
046940     EJECT                                                                
047000 IMS-RESTART SECTION.                                                     
047100     MOVE 'IMS-RESTART        '  TO DBS-SECTION                           
047200                                                                          
047300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
047400     MOVE '  '            TO GOOD-STATUSCODES                             
047500     CALL CBLTDLI USING XRST MSG-PCB                                      
047600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
047700                        CHKP-AREA-LENGTH CHKP-AREA                        
047800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047900     PERFORM IMS-STATUSCHECK                                              
048000     .                                                                    
048100     SKIP3                                                                
048200 IMS-CHECKPOINT SECTION.                                                  
048300     MOVE 'IMS-CHECKPOINT     '  TO DBS-SECTION                           
048400                                                                          
048500     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
048600     MOVE '  XD'          TO GOOD-STATUSCODES                             
048700     CALL CBLTDLI USING CHKP MSG-PCB                                      
048800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
048900                        CHKP-AREA-LENGTH CHKP-AREA                        
049000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049100     PERFORM IMS-STATUSCHECK                                              
049200                                                                          
049300     IF IMS-NOT-OK                                                        
049400       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERR-TEXT-STR        
049500       DISPLAY ERR-TEXT                                                   
049600       CALL FELLOG                                                        
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 IMS-STATUSCHECK SECTION.                                                 
050100     SKIP2                                                                
050200     SET STATUS-IX TO 1                                                   
050300     SEARCH GOOD-STATUS                                                   
050400       AT END                                                             
050500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
050600           DELIMITED BY SIZE INTO ERR-TEXT                                
050700         DISPLAY ERR-TEXT                                                 
050800         CALL FELLOG                                                      
050900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
051000         CONTINUE                                                         
051100     END-SEARCH                                                           
051200     .                                                                    
