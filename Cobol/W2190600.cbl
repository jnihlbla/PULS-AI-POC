000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2190600.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   16/10/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        ERASE CAMPAIGN INFORMATION                                       
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDM2                                       
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- ERASE CAMPAIGN INFORMATION                                 
002200     SELECT W21907                     ASSIGN TO W21906D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W21907                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W21907      -L.                                                
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2190600'.            
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004400 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
004410 01  W-KVRESS                    PIC S9(7)   COMP-3 VALUE +0.             
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004800 77  CURRENT-IMS-SECTION         PIC X(80)   VALUE SPACE.                 
004900     SKIP2                                                                
005000*    --- PARAMETERS TO ABEND                                              
005100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005400 01  ERROR-TEXT.                                                          
005500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W21907-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W21907                       VALUE 'Y'.                   
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 01  WS-TIRES-TIAAAAVV           PIC 9(6).                                
006800 01  FILLER REDEFINES WS-TIRES-TIAAAAVV.                                  
006900     03 WS-SEKEL-TIRES           PIC 9(2).                                
007000     03 WS-YEAR-TIRES            PIC 9(2).                                
007100     03 WS-WEEK-TIRES            PIC 9(2).                                
007200 01  FILLER REDEFINES WS-TIRES-TIAAAAVV.                                  
007300     03 WS-TIAAAAVV-TIRES        PIC 9(6).                                
007400                                                                          
007500 01  W-IDDOKTYP-RED              PIC X(8)   VALUE SPACE.                  
007600 01  FILLER REDEFINES W-IDDOKTYP-RED.                                     
007700   03  W-DOKTYP                  PIC X(6).                                
007710   03  W-IDDC-DOKTYP             PIC X(2).                                
007720                                                                          
007730 01  W-IDDOK-ALFA                PIC X(7)   VALUE SPACE.                  
007740                                                                          
007800 01  GENERAL-SUBPROGRAMS.                                                 
007900*                                                                         
008000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008500     EJECT                                                                
008600*   -COPY WDATAREA                                                        
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  IN-AREA-START               PIC X(24)   VALUE                        
009300                                             'IN-AREA-START'.             
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W21907     -PRE IN-                                       
009700*                                                                         
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  KEYS-TILL-DLI.                                                       
010200                                                                          
010300     03  W-WDGXKEY-4579-X.                                                
010400         05  W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
010500         05  W-IDPGM             PIC X(8)    VALUE 'W2190600'.            
010600         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
010700                                                                          
010800     03  W-WDM201-X.                                                      
010900         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
011000         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
011130                                                                          
011200     03  W-WDM211-IDARTNR-X.                                              
011300         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
011400                                                                          
011500     03  W-WDK601-X.                                                      
011600         05  W-K601-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
011700                                                                          
011800     03  W-WDK611-X.                                                      
011900         05  W-K611-KDSEGKEY     PIC  X       VALUE '1'.                  
012000                                                                          
012100     03  W-WDK901-X.                                                      
012200         05  W-K901-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
012300                                                                          
012400     03  W-WDK911KY-X.                                                    
012500         05  W-ANT-DABEHOV       PIC  9(6)    VALUE ZERO.                 
012501                                                                          
012510      03 W-WDP501KY-X.                                                    
012520         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
012530         05  W-IDDOKTYP          PIC X(8)   VALUE SPACE.                  
012540         05  W-IDDOK             PIC X(8)   VALUE SPACE.                  
012550                                                                          
012600     SKIP2                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FOUND                       VALUE '  '.                  
013000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
013300     88  IMS-NOT-OK                          VALUE 'XD'.                  
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
014500                                                                          
014600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
014700 01  DLI-IO-WDM201.                                                       
014800*    03 -COPY WDM201                                                      
014900     EJECT                                                                
015000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
015100 01  DLI-IO-WDM211.                                                       
015200*    03 -COPY WDM211                                                      
015300     EJECT                                                                
015400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
015500 01  DLI-IO-WDM221.                                                       
015600*    03 -COPY WDM221                                                      
015700     EJECT                                                                
015800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK611'.         
015900 01  DLI-IO-WDK611.                                                       
016000*    03 -COPY WDK611                                                      
016100     EJECT                                                                
016200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK901'.         
016300 01  DLI-IO-WDK901.                                                       
016400*    03 -COPY WDK901                                                      
016500     EJECT                                                                
016600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDK911'.         
016700 01  DLI-IO-WDK911.                                                       
016800*    03 -COPY WDK911                                                      
016900     EJECT                                                                
016910 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDP501'.         
016920 01  DLI-IO-WDP501.                                                       
016930*    03 -COPY WDP501                                                      
016940     EJECT                                                                
017000*-ÅTERSTARTSREGISTER WDR4                                                 
017100 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDGX4580'.         
017200 01  DLI-IO-WDGX4580.                                                     
017300*    03  -COPY WDGX4580                                                   
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600                                                                          
017700*01  -COPY W0009   -PRE MSG-                                              
017800                                                                          
017900*01  -COPY W0008  -PRE WDM2-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200*01  -COPY W0008  -PRE WDK6-                                              
018300     05  FILLER                  PIC X.                                   
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDK9-                                              
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018710*01  -COPY W0008  -PRE WDP5-                                              
018720     05  FILLER                  PIC X.                                   
018730     EJECT                                                                
018800*01  -COPY W0008  -PRE 4579-                                              
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100 PROCEDURE DIVISION  USING MSG-PCB WDM2-PCB WDK6-PCB WDK9-PCB             
019210                           WDP5-PCB 4579-PCB.                             
019300 MAIN SECTION.                                                            
019400     ENTRY 'DLITCBL' USING MSG-PCB WDM2-PCB WDK6-PCB WDK9-PCB             
019410                           WDP5-PCB 4579-PCB.                             
019600                                                                          
019700     PERFORM A-INIT                                                       
019800                                                                          
019900     PERFORM IMS-LAS-ATERSTART                                            
021000                                                                          
021100     IF 4580-KVPOST > +0                                                  
021200        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
021300     ELSE                                                                 
021400        PERFORM S01-READ-W21907                                           
021500     END-IF                                                               
021600                                                                          
021700     PERFORM UNTIL END-OF-W21907                                          
021800                                                                          
022100       IF IN-IDPTYP = 'DEL'                                               
022200          PERFORM J-DELETE-WDM2                                           
022301       ELSE                                                               
022310          IF IN-IDPTYP = 'REP'                                            
022320             PERFORM H-UPDATE-WDM211                                      
022400          END-IF                                                          
022500       END-IF                                                             
022510                                                                          
022600       IF CHKP-ANT > CHKP-MAX                                             
022700         PERFORM X-TAKE-CHECKPOINT                                        
022800       END-IF                                                             
022900                                                                          
023000       PERFORM S01-READ-W21907                                            
023100     END-PERFORM                                                          
023200                                                                          
023300                                                                          
023400     PERFORM Z-FINIT                                                      
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 A-INIT SECTION.                                                          
024100     SKIP2                                                                
024200                                                                          
024300     PERFORM IMS-RESTART                                                  
024400                                                                          
024500     OPEN INPUT W21907                                                    
024900                                                                          
025000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025100     .                                                                    
025200     EJECT                                                                
025300 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
025400     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
025500                                                                          
025600                                                                          
025700     PERFORM S01-READ-W21907                                              
025800     PERFORM UNTIL END-OF-W21907 OR                                       
025900                   W-KVPOST-IN = 4580-KVPOST                              
026000        PERFORM S01-READ-W21907                                           
026100     END-PERFORM                                                          
026200                                                                          
026300     IF END-OF-W21907                                                     
026400        MOVE 'INPUTFIL EOF = YES, VID ÅTERSTART'                          
026500                      TO ERROR-TEXT                                       
026600        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 H-UPDATE-WDM211 SECTION.                                                 
027100     MOVE 'H-UPDATE-WDM211          ' TO CURRENT-SECTION                  
027200                                                                          
027300     MOVE IN-IDKAMPRF       TO W-KAMP-IDKAMPRF                            
027400     MOVE IN-IDDC           TO W-KAMP-IDDC                                
027500     MOVE IN-IDARTNR        TO W-KART-IDARTNR                             
027600     PERFORM IMS-GHU-WDM211                                               
027700     IF SEGMENT-FOUND                                                     
027822        COMPUTE W-KVRESS = KART-KVRESS-KAMP - KART-KVBEART-KUND           
027827                                                                          
027828        COMPUTE KART-KVRESS-KAMP = KART-KVRESS-KAMP - W-KVRESS            
027829        COMPUTE KART-KVRESS-ART  = KART-KVRESS-ART  - W-KVRESS            
027900        PERFORM IMS-REPL-WDM211                                           
028001                                                                          
028003        MOVE IN-IDARTNR        TO W-K601-IDARTNR                          
028020        PERFORM IMS-GHU-WDK611                                            
028030        IF SEGMENT-FOUND                                                  
028034           COMPUTE CLAG-KVRESS = CLAG-KVRESS - W-KVRESS                   
028050           PERFORM IMS-REPL-WDK611                                        
028070        END-IF                                                            
028080     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 J-DELETE-WDM2 SECTION.                                                   
028400     MOVE 'J-DELETE-WDM21           ' TO CURRENT-SECTION                  
028500                                                                          
028600     MOVE IN-IDKAMPRF       TO W-KAMP-IDKAMPRF                            
028610                               W-IDDOK                                    
028700     MOVE IN-IDDC           TO W-KAMP-IDDC                                
028710                               W-IDDC-DOKTYP                              
028800     PERFORM IMS-GU-WDM201                                                
028900     IF SEGMENT-FOUND                                                     
029000        PERFORM IMS-GNP-WDM211                                            
029010        PERFORM UNTIL SEGMENT-MISSING                                     
029100           MOVE KART-IDARTNR    TO W-KART-IDARTNR                         
029200                                                                          
029300           IF SEGMENT-FOUND                                               
029400              IF KART-KVRESS-KAMP > ZERO                                  
029500                 PERFORM JA-UPDATE-WDK611                                 
029600              ELSE                                                        
029700                 PERFORM JB-UPDATE-WDK901-WDK911                          
029800              END-IF                                                      
029910           END-IF                                                         
029911           PERFORM IMS-GNP-WDM211                                         
029920        END-PERFORM                                                       
030000                                                                          
030100        PERFORM IMS-GHU-WDM201                                            
030200        IF SEGMENT-FOUND                                                  
030210           MOVE KAMP-IDKAMPRF     TO W-IDDOK-ALFA                         
030211           MOVE ZERO              TO W-IDDOK (1:1)                        
030220           MOVE W-IDDOK-ALFA      TO W-IDDOK (2:7)                        
030230           INSPECT W-IDDOK REPLACING LEADING ZERO BY SPACE                
030300           PERFORM IMS-DLET-WDM201                                        
030301** IF YOU DELETE THE CAMPAIGN REF THEN ALSO DELETE THE                    
030302** CAMPAIGN REF NOTES ON 0553(WDP501)                                     
030303*          INSPECT W-IDDOK REPLACING                                      
030304*               LEADING SPACE BY ZERO                                     
030310           MOVE 'S  '             TO W-IDSKYLT                            
030320           MOVE 'CAMPRE'          TO W-DOKTYP                             
030330           MOVE W-IDDOKTYP-RED    TO W-IDDOKTYP                           
030340           PERFORM IMS-GET-WDP501                                         
030350           IF SEGMENT-FOUND                                               
030351             PERFORM IMS-DLET-WDP501                                      
030360           END-IF                                                         
030400           ADD +1            TO CHKP-ANT                                  
030500        END-IF                                                            
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 JA-UPDATE-WDK611 SECTION.                                                
031000     MOVE 'JA-UPDATE-WDK611          ' TO CURRENT-SECTION                 
031100                                                                          
031200     MOVE KART-IDARTNR        TO W-K601-IDARTNR                           
031300     PERFORM IMS-GHU-WDK611                                               
031400     IF SEGMENT-FOUND                                                     
031500        COMPUTE CLAG-KVRESS = CLAG-KVRESS - KART-KVRESS-ART               
031600        PERFORM IMS-REPL-WDK611                                           
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 JB-UPDATE-WDK901-WDK911   SECTION.                                       
032100      MOVE 'JB-UPDATE-WDK901-WDK911   ' TO CURRENT-SECTION                
032200                                                                          
032300     MOVE KART-IDARTNR        TO W-K901-IDARTNR                           
032400     PERFORM IMS-GHU-WDK901                                               
032500     IF SEGMENT-FOUND                                                     
032600        COMPUTE ART-SUTPO-TOT = ART-SUTPO-TOT -                           
032700                                KART-KVBEART-KAMP                         
032800        PERFORM IMS-REPL-WDK901                                           
032900     END-IF                                                               
033000                                                                          
033100     PERFORM JBA-TIRES-AAAAWW                                             
033200     IF WS-TIRES-TIAAAAVV > ZERO                                          
033300        MOVE KART-IDARTNR      TO W-K901-IDARTNR                          
033400        MOVE WS-TIRES-TIAAAAVV TO W-ANT-DABEHOV                           
033500        PERFORM IMS-GHU-WDK911                                            
033600        IF SEGMENT-FOUND                                                  
033700           IF KART-KVBEART-KAMP > +0                                      
033800              COMPUTE ANT-SUTPO-EJPB =                                    
033900                      ANT-SUTPO-EJPB - KART-KVBEART-KAMP                  
034000              IF ANT-SUTPO-EJPB = +0 AND ANT-SUTPO-PB = +0                
034100                 PERFORM IMS-DLET-WDK911                                  
034200              ELSE                                                        
034300                 PERFORM IMS-REPL-WDK911                                  
034400              END-IF                                                      
034500           END-IF                                                         
034600        END-IF                                                            
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000                                                                          
035100 JBA-TIRES-AAAAWW SECTION.                                                
035200     MOVE 'JBA-TIRES-AAAAWW         ' TO CURRENT-SECTION                  
035300                                                                          
035400     IF KART-TIRES > ZERO                                                 
035500       MOVE KART-TIRES            TO DAT-I-TIDATUM                        
035600       MOVE 'AAMMDD'              TO DAT-KDDATFORM                        
035700       CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                   
035800                           DAT-O-TIDATUM, DAT-KDSVAR                      
035900                                                                          
036000       IF DAT-KDSVAR-OK                                                   
036100         MOVE DAT-TIAA-VECKA      TO WS-YEAR-TIRES                        
036200         MOVE DAT-TIVV            TO WS-WEEK-TIRES                        
036300         MOVE DAT-TISEKEL         TO WS-SEKEL-TIRES                       
036400       END-IF                                                             
036500     ELSE                                                                 
036600       MOVE ZERO                  TO WS-TIRES-TIAAAAVV                    
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 Z-FINIT SECTION.                                                         
037100                                                                          
037200     CLOSE W21907                                                         
037300     SKIP2                                                                
037400     MOVE 'S' TO POSTSUM-OPKOD                                            
037500     CALL POSTSUM USING POSTSUM-PARM                                      
037600                                                                          
037700     PERFORM IMS-LAS-ATERSTART                                            
037800                                                                          
037900     MOVE +0                           TO 4580-KVPOST                     
038000     MOVE TODAYS-DATE                  TO 4580-TIUPPDAT                   
038100     ACCEPT 4580-TIUPPTID FROM TIME                                       
038200                                                                          
038300     PERFORM IMS-REPL-ATERSTART                                           
038400     .                                                                    
038500     EJECT                                                                
038600 S01-READ-W21907  SECTION.                                                
038700     SKIP2                                                                
038800     READ W21907 INTO IN-AREA                                             
038900     AT END                                                               
039000        SET END-OF-W21907 TO TRUE                                         
039100                                                                          
039200     NOT AT END                                                           
039300        MOVE 'W21907'   TO POSTSUM-FDNAMN                                 
039400        MOVE 'W21906D1' TO POSTSUM-DDNAMN2                                
039500        MOVE 'W219'     TO POSTSUM-TRANSTYP                               
039600        CALL POSTSUM USING POSTSUM-PARM                                   
039700                                                                          
039800     END-READ                                                             
039900     .                                                                    
040000     EJECT                                                                
040100 X-TAKE-CHECKPOINT   SECTION.                                             
040200                                                                          
040300*    UPPDATERA ÅTERSTARTREGISTRET                                         
040400     PERFORM IMS-LAS-ATERSTART                                            
040500                                                                          
040600     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
040700     ACCEPT 4580-TIUPPDAT FROM DATE                                       
040800     ACCEPT 4580-TIUPPTID FROM TIME                                       
040900                                                                          
041000     PERFORM IMS-REPL-ATERSTART                                           
041100                                                                          
041200* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
041300* --- SAVE DATABASE KEYS IF NECESSARY                                     
041400     PERFORM IMS-CHECKPOINT                                               
041500     MOVE ZERO TO CHKP-ANT                                                
041600* --- REREAD DATABASE IF NECESSARY                                        
041700     .                                                                    
041800     EJECT                                                                
041900* --- IMS SECTIONS  ---                                                   
042000                                                                          
042100     EJECT                                                                
042200 IMS-GU-WDM201 SECTION.                                                   
042300     MOVE 'IMS-GU-WDM201      ' TO CURRENT-IMS-SECTION                    
042400                                                                          
042500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
042600          DELIMITED BY SIZE INTO SSA1                                     
042700     MOVE '  GE'              TO GOOD-STATUSCODES                         
042800     CALL CBLTDLI USING GU  WDM2-PCB DLI-IO-WDM201 SSA1                   
042900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
043000     PERFORM IMS-STATUSCHECK                                              
043100     .                                                                    
043200                                                                          
043300 IMS-GNP-WDM211 SECTION.                                                  
043400     MOVE 'IMS-GNP-WDM211      ' TO CURRENT-IMS-SECTION                   
043500                                                                          
043600     MOVE 'WDM211  '          TO SSA1                                     
043700     MOVE '  GE'              TO GOOD-STATUSCODES                         
043800     CALL CBLTDLI USING GHNP WDM2-PCB DLI-IO-WDM211 SSA1                  
043900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
044000     PERFORM IMS-STATUSCHECK                                              
044100     .                                                                    
044200                                                                          
044300 IMS-GHU-WDM201 SECTION.                                                  
044400     MOVE 'IMS-GHU-WDM201      ' TO CURRENT-IMS-SECTION                   
044500                                                                          
044600     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
044700          DELIMITED BY SIZE INTO SSA1                                     
044800     MOVE '  GE'              TO GOOD-STATUSCODES                         
044900     CALL CBLTDLI USING GHU  WDM2-PCB DLI-IO-WDM201 SSA1                  
045000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
045100     PERFORM IMS-STATUSCHECK                                              
045200     .                                                                    
045300     SKIP3                                                                
045400 IMS-GHU-WDM211 SECTION.                                                  
045500     MOVE 'IMS-GHU-WDM211      ' TO CURRENT-IMS-SECTION                   
045600                                                                          
045700     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
045800          DELIMITED BY SIZE INTO SSA1                                     
045900     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
046000          DELIMITED BY SIZE INTO SSA2                                     
046100     MOVE '  GE'              TO GOOD-STATUSCODES                         
046200     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
046300     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
046400     PERFORM IMS-STATUSCHECK                                              
046500     .                                                                    
046600                                                                          
046700 IMS-REPL-WDM211 SECTION.                                                 
046800     MOVE 'IMS-REPL-WDM211     ' TO CURRENT-IMS-SECTION                   
046900                                                                          
047000     MOVE '  '             TO GOOD-STATUSCODES                            
047100     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
047200     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
047300     PERFORM IMS-STATUSCHECK                                              
047400     .                                                                    
047500                                                                          
047600 IMS-DLET-WDM201 SECTION.                                                 
047700     MOVE 'IMS-DLET-WDM201      ' TO CURRENT-IMS-SECTION                  
047800                                                                          
047900     MOVE '  '             TO GOOD-STATUSCODES                            
048000     CALL CBLTDLI USING DLET WDM2-PCB DLI-IO-WDM201                       
048100     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSCHECK                                              
048300     .                                                                    
048400     EJECT                                                                
048500 IMS-GHU-WDK611 SECTION.                                                  
048600     MOVE 'IMS-GHU-WDK611             ' TO CURRENT-IMS-SECTION            
048700                                                                          
048800     STRING 'WDK601  (IDARTNR  =' W-WDK601-X ')'                          
048900          DELIMITED BY SIZE INTO SSA1                                     
049000     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
049110     MOVE '  GE'           TO GOOD-STATUSCODES                            
049200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
049300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSCHECK                                              
049500     .                                                                    
049600 IMS-REPL-WDK611 SECTION.                                                 
049700     MOVE 'IMS-REPL-WDK611            ' TO CURRENT-IMS-SECTION            
049800                                                                          
049900     MOVE '  '             TO GOOD-STATUSCODES                            
050000     CALL CBLTDLI USING  REPL WDK6-PCB DLI-IO-WDK611                      
050100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSCHECK                                              
050300     .                                                                    
050400                                                                          
050500 IMS-GHU-WDK901 SECTION.                                                  
050600     MOVE 'IMS-GHU-WDK901             ' TO CURRENT-IMS-SECTION            
050700                                                                          
050800     STRING 'WDK901  (IDARTNR  =' W-WDK901-X ')'                          
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     MOVE '  GE'           TO GOOD-STATUSCODES                            
051100     CALL CBLTDLI       USING GHU WDK9-PCB DLI-IO-WDK901 SSA1             
051200     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
051300     PERFORM IMS-STATUSCHECK                                              
051400     .                                                                    
051500                                                                          
051600 IMS-REPL-WDK901 SECTION.                                                 
051700     MOVE 'IMS-REPL-WDK901            ' TO CURRENT-IMS-SECTION            
051800                                                                          
051900     MOVE '  '             TO GOOD-STATUSCODES                            
052000     CALL CBLTDLI       USING REPL WDK9-PCB DLI-IO-WDK901                 
052100     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
052200     PERFORM IMS-STATUSCHECK                                              
052300     .                                                                    
052400                                                                          
052500 IMS-GHU-WDK911 SECTION.                                                  
052600     MOVE 'IMS-GHU-WDK911             ' TO CURRENT-IMS-SECTION            
052700                                                                          
052800     STRING 'WDK901  (IDARTNR  =' W-WDK901-X ')'                          
052900          DELIMITED BY SIZE INTO SSA1                                     
053000     STRING 'WDK911  (DABEHOV  =' W-WDK911KY-X ')'                        
053100          DELIMITED BY SIZE INTO SSA2                                     
053200     MOVE '  GE'              TO GOOD-STATUSCODES                         
053300     CALL CBLTDLI          USING GHU WDK9-PCB DLI-IO-WDK911 SSA1          
053400                                                            SSA2          
053500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSCHECK                                              
053700     .                                                                    
053800                                                                          
053900 IMS-REPL-WDK911 SECTION.                                                 
054000     MOVE 'IMS-REPL-WDK911            ' TO CURRENT-IMS-SECTION            
054100                                                                          
054200     MOVE '  '             TO GOOD-STATUSCODES                            
054300     CALL CBLTDLI       USING REPL WDK9-PCB DLI-IO-WDK911                 
054400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUSCHECK                                              
054600     .                                                                    
054700                                                                          
054800 IMS-DLET-WDK911 SECTION.                                                 
054900     MOVE 'IMS-DLET-WDK911            ' TO CURRENT-IMS-SECTION            
055000                                                                          
055100     MOVE '  '             TO GOOD-STATUSCODES                            
055200     CALL CBLTDLI       USING DLET WDK9-PCB DLI-IO-WDK911                 
055300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSCHECK                                              
055500     .                                                                    
055501                                                                          
055502 IMS-GET-WDP501 SECTION.                                                  
055503     MOVE 'GET-WDP501      ' TO CURRENT-IMS-SECTION                       
055504                                                                          
055506     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
055507          DELIMITED BY SIZE INTO SSA1                                     
055508     MOVE '  GE' TO GOOD-STATUSCODES                                      
055509     CALL CBLTDLI USING GHU WDP5-PCB DLI-IO-WDP501 SSA1                   
055510     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
055511     PERFORM IMS-STATUSCHECK                                              
055512     .                                                                    
055513     SKIP3                                                                
055514                                                                          
055515 IMS-DLET-WDP501 SECTION.                                                 
055520     MOVE 'DLET-WDP501     ' TO CURRENT-IMS-SECTION                       
055530                                                                          
055550     MOVE '  ' TO GOOD-STATUSCODES                                        
055560     CALL CBLTDLI USING DLET WDP5-PCB DLI-IO-WDP501                       
055570     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
055580     PERFORM IMS-STATUSCHECK                                              
055590     .                                                                    
055591     SKIP3                                                                
055600                                                                          
055700 IMS-RESTART SECTION.                                                     
055800     SKIP2                                                                
055900     MOVE SPACE            TO CHKP-MSG-IO-AREA                            
056000     MOVE '  '             TO GOOD-STATUSCODES                            
056100     CALL CBLTDLI USING XRST MSG-PCB                                      
056200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
056300                        CHKP-AREA-LENGTH CHKP-AREA                        
056400     MOVE MSG-STATUS-CODE  TO STATUS-WS                                   
056500     PERFORM IMS-STATUSCHECK                                              
056600     .                                                                    
056700     SKIP3                                                                
056800 IMS-CHECKPOINT SECTION.                                                  
056900     SKIP2                                                                
057000     MOVE SPACE            TO CHKP-MSG-IO-AREA                            
057100     MOVE '  XD' TO GOOD-STATUSCODES                                      
057200     CALL CBLTDLI USING CHKP MSG-PCB                                      
057300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057400                        CHKP-AREA-LENGTH CHKP-AREA                        
057500     MOVE MSG-STATUS-CODE  TO STATUS-WS                                   
057600     PERFORM IMS-STATUSCHECK                                              
057700                                                                          
057800     IF IMS-NOT-OK                                                        
057900       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERROR-TEXT          
058000       DISPLAY ERROR-TEXT                                                 
058100       CALL FELLOG                                                        
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 IMS-LAS-ATERSTART SECTION.                                               
058600     MOVE 'IMS-LAS-ATERSTART    ' TO CURRENT-IMS-SECTION                  
058700                                                                          
058800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
058900                    DELIMITED BY SIZE INTO SSA1                           
059000     MOVE 'WDR470 '        TO SSA2                                        
059100     MOVE '  '             TO GOOD-STATUSCODES                            
059200     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
059300     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
059400     PERFORM IMS-STATUSCHECK                                              
059500     .                                                                    
061000                                                                          
061100 IMS-REPL-ATERSTART SECTION.                                              
061200     MOVE 'IMS-REPL-ATERSTART   ' TO CURRENT-IMS-SECTION                  
061300                                                                          
061400     MOVE '  '             TO GOOD-STATUSCODES                            
061500     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
061600     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
061700     PERFORM IMS-STATUSCHECK                                              
061800     .                                                                    
061900                                                                          
062000 IMS-STATUSCHECK SECTION.                                                 
062100     SKIP2                                                                
062200     SET STATUS-IX TO 1                                                   
062300     SEARCH GOOD-STATUS                                                   
062400       AT END                                                             
062500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062600           DELIMITED BY SIZE INTO ERROR-TEXT                              
062700         DISPLAY ERROR-TEXT                                               
062800         CALL FELLOG                                                      
062900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
063000         CONTINUE                                                         
063100     END-SEARCH                                                           
063200     .                                                                    
