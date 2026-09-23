000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2245500.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/03/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE OUTPUT FILE TO DELETE AND REPLADE WDD9 FOR NDC            
000900*                                                                         
001000*        THE PROGRAM READS     WDD9                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001510*                                                                         
001520* CHANGE LOG:                                                             
001530* 2015-09-17   ETRACKER 10209749    (TAG BORT WDD903)                     
001540*              ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.            
001550*                                                                         
001551* 2017-09-14   ETRACKER 10299286  LOCAL SOURCING USA                      
001552*              CCID: 10302687 SAMMA CCID SOM EXTENDED REFILL USA          
001553*                                                                         
001560*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- WDD9 RECORDS TO BE DELETED                                 
002500     SELECT W22455                     ASSIGN TO W22455D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W22455                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W2245501 -PRE  OUT-  -L.                                
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 01  PROGRAM-NAME                PIC X(6)    VALUE 'W22455'.              
004000 77  IMS-SEKTION                 PIC X(30).                               
004100 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 01  WORK-AREA.                                                           
004600     05  W-TIAAVV-AKTUELL        PIC 9(4).                                
004700     05  FILLER REDEFINES W-TIAAVV-AKTUELL.                               
004800         10  W-TIAA-AKTUELL      PIC 9(2).                                
004900         10  W-TIVV-AKTUELL      PIC 9(2).                                
005000     05  WS-WDD905-DELETE        PIC X(01)   VALUE 'N'.                   
005100     05  WS-WDD924-LEV-FLSENLEV  PIC X(01).                               
005200     05  WS-WDD905-TIAVRDAT-INL.                                          
005300         07 WS-WDD905-TIAVRDAT-INL-AAVV                                   
005400                                 PIC 9(04).                               
005500         07 FILLER               PIC S9(1).                               
006000     05  W-TIAAVV-BORTTAG        PIC S9(5)           COMP-3.              
006100     05  W-TIAAVV-BORTTAG-AVROP  PIC S9(5)           COMP-3.              
006400     05  WS-TIAAVVD.                                                      
006500         10  WS-TIAAVV           PIC S9(4).                               
006600         10  FILLER              PIC S9(1).                               
006700     05  WS-IDARTNR              PIC S9(9)           COMP-3.              
006800     05  WS-IDDC                 PIC X(2).                                
006900     05  WS-IDLEVNR              PIC X(5).                                
007000     05  W-AAVV                  PIC S9(5).                               
007100     05  W-AAVV-X  REDEFINES W-AAVV.                                      
007200         10  W-NOLL              PIC 9.                                   
007300         10  W-AA                PIC 9(2).                                
007400         10  W-VV                PIC 9(2).                                
007500     05  DAGENS-DATUM-PACK       PIC S9(7)           COMP-3.              
007600     05  TODAYS-DATE-VECKA       PIC 9(4).                                
007700     05  FILLER REDEFINES TODAYS-DATE-VECKA.                              
007800         07  TODAYS-DATE-TIAA    PIC 9(2).                                
007900         07  TODAYS-DATE-TIVV    PIC 9(2).                                
008000     05  TODAYS-DATE             PIC 9(6)    VALUE ZERO.                  
008100     05  FILLER REDEFINES TODAYS-DATE.                                    
008200         07  TODAYS-DATE-YEAR    PIC 9(2).                                
008300         07  TODAYS-DATE-MONTH   PIC 9(2).                                
008400         07  TODAYS-DATE-DAY     PIC 9(2).                                
008410     05  W-IDLOPNRM-CHECK-X.                                              
008420         10  W-IDLOPNRM-1-2   PIC 9(2).                                   
008430         10  W-IDLOPNRM-3-9   PIC 9(7).                                   
008440     05  W-IDLOPNRM-CHECK REDEFINES W-IDLOPNRM-CHECK-X PIC 9(9).          
008500     EJECT                                                                
008510 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
008520     10  W-IDLOPNRM           PIC 9(8).                                   
008530     10  FILLER REDEFINES W-IDLOPNRM.                                     
008540         20  W-VVDLLLL.                                                   
008550             30  W-DAT-VV     PIC 9(2).                                   
008560             30  W-DAT-DAG    PIC 9.                                      
008570             30  W-LOPNR      PIC 9(4).                                   
008580         20  W-KTRLSIFFRA     PIC 9(1).                                   
008590     10  CHECK-FLTB           PIC S9       VALUE +7   COMP  SYNC.         
008591     10  CHECK-FLTC           PIC 9(7)     VALUE 2121212.                 
008592     10  CHECK-FLTD           PIC S9       VALUE +7   COMP  SYNC.         
008593     10  CHECK-FLTF           PIC 9(2)     VALUE 10.                      
008594     10  CHECK-FLTG           PIC X(1)     VALUE 'B'.                     
008595                                                                          
008600 01  SPAR-AREA.                                                           
008700     05  SPAR-WDD901-IDARTNR     PIC S9(9)           COMP-3.              
008800     05  SPAR-WDD901-IDDC        PIC X(2).                                
008900     05  SPAR-WDD902-IDLEVNR     PIC X(5).                                
009000     05  SPAR-WDD905-TIAVRDAT-INL-AAVV                                    
009100                                 PIC S9(5)           COMP-3.              
009110     05  SPAR-WDD924-TILEVBSK-INL-AAVV                                    
009120                                 PIC S9(5)           COMP-3.              
009300     05  SPAR-WDD905-KDAVROP     PIC S9(1)           COMP-3.              
009700     EJECT                                                                
009810*01  -COPY WWDC99 -PRE DC-                                                
009900     EJECT                                                                
010000 01  GENERAL-SUBPROGRAMS.                                                 
010100*                                                                         
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
010910     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
011000     SKIP2                                                                
011100*--------------------------------------- PARAMETRAR TILL W009VADD         
011200 01  W009VADD-DATUM          PIC S9(5)               COMP-3.              
011300                                                                          
011400 01  W009VADD-ANTAL          PIC S9(3)               COMP-3.              
011500     EJECT                                                                
011600*----------------------------------------PARAMETRAR TILL WDATKONV         
011700 01  FILLER                  PIC X(16)   VALUE 'WDATKONV        '.        
011800*01   -COPY WDATAREA.                                                     
011900     EJECT                                                                
012000*----------------------------------------PARAMETRAR TILL WDATUM           
012100 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
012200     SKIP2                                                                
012300*01  -COPY WDATKORT                                                       
012400     EJECT                                                                
012500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012800     SKIP2                                                                
012900 01  ERROR-TEXT.                                                          
013000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
013100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
013200     EJECT                                                                
013300     EJECT                                                                
013400*    --- PARAMETRAR TILL POSTSUM                                          
013500*                                                                         
013600*01  -COPY W0005   -PRE  POSTSUM-                                         
013700     EJECT                                                                
013800 01  OUT-AREA-START              PIC X(24)   VALUE                        
013900                                 'OUT-AREA-START  '.                      
014000     SKIP2                                                                
014100                                                                          
014200*01  AREA -COPY W2245501     -PRE OUT-                                    
014300     EJECT                                                                
014400*    --- AREAS FOR IMS-SECTIONS                                           
014500*                                                                         
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014800                                                                          
014900 01  KEYS-FOR-DLI.                                                        
015000     03  W-W6D1BSEQ-X.                                                    
015100         05  W-W6D1BSEQ-IDLOPNRM PIC S9(9)   VALUE ZERO COMP-3.           
015200*                                                                         
015210 01  KEYS-FOR-DLI.                                                        
015220     03  W-WDD901-X.                                                      
015230         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015240         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
015250     03  W-WDD902-X.                                                      
015260         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
015290     03  W-WDD905-X.                                                      
015291         05  W-DAAVROP           PIC  9(6)   VALUE ZERO.                  
015293         05  W-TILEVDAG          PIC S9(1)   VALUE ZERO COMP-3.           
015295     03  W-WDD906-X.                                                      
015296         05  W-IDLOPNRM-PL       PIC S9(9)   VALUE ZERO COMP-3.           
015297     03  W-WDD907-X.                                                      
015298         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
015299     03  W-WDD924-X.                                                      
015300         05  W-DALEVBSK-AVS      PIC  9(8)   VALUE ZERO.                  
015301     03  W-WDD925-X.                                                      
015302         05  W-IDLEVBSK          PIC S9(1)   VALUE ZERO COMP-3.           
015303                                                                          
015310*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FOUND                       VALUE '  '.                  
015600     88  SEGMENT-MISSING                     VALUE 'GB'.                  
015700                                                                          
015800 01  GOOD-STATUSCODES.                                                    
015900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNCTION CODES                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800                                                                          
016810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD9'.                        
016820 01  DLI-IO-WDD9.                                                         
016830      03 IO-WDD9      PIC X(100).                                         
016840 *    03 WDD901 -COPY WDD901 -PRE WDD901- -RED IO-WDD9.                   
017300 *    03 WDD902 -COPY WDD902 -PRE WDD902- -RED IO-WDD9.                   
017500 *    03 WDD905 -COPY WDD905 -PRE WDD905- -RED IO-WDD9.                   
017600 *    03 WDD905 -COPY WDD906 -PRE WDD906- -RED IO-WDD9.                   
017700 *    03 WDD924 -COPY WDD924 -PRE WDD924- -RED IO-WDD9.                   
017800 *    03 WDD925 -COPY WDD925 -PRE WDD925- -RED IO-WDD9.                   
017900     EJECT                                                                
018000 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
018100 01  DLI-IO-W6D111.                                                       
018200*    03  -COPY W6D111  -PRE W6D111-                                       
018300     EJECT                                                                
018400 LINKAGE SECTION.                                                         
018600*01  -COPY W0008  -PRE WDD9-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE W6D1B-                                             
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200 PROCEDURE DIVISION  USING WDD9-PCB W6D1B-PCB.                            
019300 MAIN SECTION.                                                            
019400     ENTRY 'DLITCBL' USING WDD9-PCB W6D1B-PCB.                            
019500                                                                          
019600     PERFORM A-INIT                                                       
019700                                                                          
019800     PERFORM IMS-GN-WDD9                                                  
019900     PERFORM UNTIL SEGMENT-MISSING                                        
020010                                                                          
020100       IF WS-WDD905-DELETE = JA                                           
020200          MOVE NEJ                   TO WS-WDD905-DELETE                  
020210          IF WDD9-SEG-NAME-FB NOT = 'WDD906'                              
020300             MOVE 'WDD905'           TO OUT-SEG-NAME-FB                   
020400             MOVE 'D'                TO OUT-KDUPD                         
020500             MOVE W-WDD905-X         TO OUT-IDKEY-WDD905                  
020600             MOVE SPACE              TO OUT-FLSENLEV                      
020610             PERFORM S11-WRITE-W22455                                     
020900          END-IF                                                          
021000       END-IF                                                             
021100                                                                          
021200       EVALUATE WDD9-SEG-NAME-FB                                          
021300         WHEN 'WDD901'                                                    
021400           PERFORM S02-INIT-WORK-AREA                                     
021410           MOVE WDD901-IDDC          TO DC-WS-IDDC                        
021510           IF DC-NDC-CN OR DC-NDC-US                                      
021600              MOVE WDD901-IDARTNR    TO W-IDARTNR                         
021700              MOVE WDD901-IDDC       TO W-IDDC                            
021710              MOVE W-WDD901-X        TO OUT-IDKEY-WDD901                  
021800              MOVE SPACE             TO OUT-IDKEY-WDD902                  
021900              MOVE SPACE             TO OUT-IDKEY-WDD905                  
022000              MOVE SPACE             TO OUT-IDKEY-WDD906                  
022100              MOVE SPACE             TO OUT-IDKEY-WDD907                  
022200              MOVE SPACE             TO OUT-IDKEY-WDD924                  
022300              MOVE SPACE             TO OUT-IDKEY-WDD925                  
022400              MOVE SPACE             TO OUT-FLSENLEV                      
022410                                                                          
022500              MOVE WDD901-IDARTNR    TO SPAR-WDD901-IDARTNR               
022510              MOVE WDD901-IDDC       TO SPAR-WDD901-IDDC                  
022600           END-IF                                                         
022700                                                                          
022800         WHEN 'WDD902'                                                    
022910           IF DC-NDC-CN OR DC-NDC-US                                      
023000              MOVE WDD902-IDLEVNR    TO SPAR-WDD902-IDLEVNR               
023010              MOVE WDD902-IDLEVNR    TO W-IDLEVNR                         
023100              MOVE W-WDD902-X        TO OUT-IDKEY-WDD902                  
023200           END-IF                                                         
023300                                                                          
023900         WHEN 'WDD905'                                                    
024010           IF DC-NDC-CN OR DC-NDC-US                                      
024100              PERFORM C-WDD905-AVROP-AVBOK                                
024200           END-IF                                                         
024300                                                                          
024400         WHEN 'WDD906'                                                    
024510           IF DC-NDC-CN OR DC-NDC-US                                      
024600              PERFORM D-WDD906-AVROP-AVBOK                                
024700           END-IF                                                         
024800                                                                          
024900         WHEN 'WDD924'                                                    
025010           IF DC-NDC-CN OR DC-NDC-US                                      
025100              PERFORM E-WDD924-TILEVBSK                                   
025200           END-IF                                                         
025300                                                                          
025400         WHEN 'WDD925'                                                    
025510           IF DC-NDC-CN OR DC-NDC-US                                      
025600              PERFORM F-WDD925-IDLEVBSK                                   
025700           END-IF                                                         
025800                                                                          
025900       END-EVALUATE                                                       
025910                                                                          
026000       PERFORM IMS-GN-WDD9                                                
026100     END-PERFORM                                                          
026200     PERFORM Z-FINIT                                                      
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026810     MOVE 'A-INIT     '     TO CURRENT-SECTION                            
026900                                                                          
027000     OPEN OUTPUT W22455                                                   
027100                                                                          
027200     MOVE PROGRAM-NAME      TO POSTSUM-PROGNAMN                           
027210                                                                          
027300     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
027400     MOVE ZERO              TO W-NOLL                                     
027500     MOVE D-AAR             TO W-TIAA-AKTUELL                             
027600     MOVE D-VECKA           TO W-TIVV-AKTUELL                             
027900     MOVE W-TIAAVV-AKTUELL  TO W009VADD-DATUM                             
028000     MOVE -6                TO W009VADD-ANTAL                             
028100     CALL W009VADD       USING W009VADD-DATUM W009VADD-ANTAL              
028200     MOVE W009VADD-DATUM    TO W-TIAAVV-BORTTAG                           
028300                                                                          
028400     MOVE ZERO              TO WS-IDARTNR                                 
028500     MOVE SPACE             TO WS-IDDC                                    
028600     MOVE SPACE             TO WS-IDLEVNR                                 
028700                                                                          
028800     MOVE W-TIAAVV-AKTUELL  TO W009VADD-DATUM                             
028900     MOVE -7                TO W009VADD-ANTAL                             
029000     CALL W009VADD       USING W009VADD-DATUM W009VADD-ANTAL              
029100     MOVE W009VADD-DATUM    TO W-TIAAVV-BORTTAG-AVROP                     
029200     MOVE D-AAR             TO TODAYS-DATE-YEAR                           
029300     MOVE D-MAANAD          TO TODAYS-DATE-MONTH                          
029400     MOVE D-DAG             TO TODAYS-DATE-DAY                            
029500     MOVE TODAYS-DATE       TO DAGENS-DATUM-PACK                          
029600     .                                                                    
029700     EJECT                                                                
031100 C-WDD905-AVROP-AVBOK SECTION.                                            
031110     MOVE 'C-WDD905-AVROP-AVBOK' TO CURRENT-SECTION                       
031120                                                                          
031150     MOVE WDD905-DAAVROP-AVS   TO W-DAAVROP                               
031170     MOVE WDD905-TILEVDAG      TO W-TILEVDAG                              
031171     MOVE W-WDD905-X           TO OUT-IDKEY-WDD905                        
031180                                                                          
031210     IF WDD905-TIAVRDAT-INL = +0                                          
031220       MOVE +0                 TO SPAR-WDD905-TIAVRDAT-INL-AAVV           
031230     ELSE                                                                 
031300       MOVE WDD905-TIAVRDAT-INL TO DAT-I-TIDATUM                          
031400       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
031500       CALL WDATKONV USING        DAT-KDDATFORM                           
031600                                  DAT-I-TIDATUM                           
031700                                  DAT-O-TIDATUM                           
031800                                  DAT-KDSVAR                              
031900       IF DAT-KDSVAR-FEL                                                  
032000         MOVE ' FEL VID ANROP TILL DATKONV 2'                             
032100                               TO ERROR-TEXT-STR                          
032200         DISPLAY ERROR-TEXT                                               
032300         CALL FELLOG                                                      
032400       ELSE                                                               
032500         MOVE DAT-TIAAVV-GRP   TO WS-TIAAVV                               
032501         MOVE WS-TIAAVV        TO SPAR-WDD905-TIAVRDAT-INL-AAVV           
032600       END-IF                                                             
032700     END-IF                                                               
032804                                                                          
032810     MOVE WDD905-KDAVROP       TO SPAR-WDD905-KDAVROP                     
032811                                                                          
032900     IF  SPAR-WDD905-TIAVRDAT-INL-AAVV < W-TIAAVV-BORTTAG-AVROP           
033000        IF  WDD905-KDAVROP = 9                                            
033100            MOVE JA            TO WS-WDD905-DELETE                        
033200        ELSE                                                              
033300          IF  WDD905-KVAVROP <= ZERO                                      
033400             MOVE 'WDD905'     TO OUT-SEG-NAME-FB                         
033500             MOVE 'D'          TO OUT-KDUPD                               
033600             MOVE W-WDD905-X   TO OUT-IDKEY-WDD905                        
033700             MOVE SPACE        TO OUT-FLSENLEV                            
033800             PERFORM S11-WRITE-W22455                                     
033900          END-IF                                                          
034000        END-IF                                                            
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 D-WDD906-AVROP-AVBOK SECTION.                                            
034410     MOVE 'D-WDD906-AVROP-AVBOK'     TO CURRENT-SECTION                   
034500                                                                          
034600     IF  SPAR-WDD905-KDAVROP = 9                                          
034700     AND SPAR-WDD905-TIAVRDAT-INL-AAVV < W-TIAAVV-BORTTAG-AVROP           
034920         MOVE WDD906-IDLOPNRM-PL   TO W-IDLOPNRM-CHECK                    
034940         MOVE W-IDLOPNRM-3-9       TO W-VVDLLLL                           
034950         CALL CHECK USING W-VVDLLLL    CHECK-FLTB CHECK-FLTC              
034960              CHECK-FLTD  W-KTRLSIFFRA CHECK-FLTF CHECK-FLTG              
034970                                                                          
034980         MOVE W-IDLOPNRM           TO W-W6D1BSEQ-IDLOPNRM                 
035000         PERFORM IMS-GU-W6D111                                            
035100         IF SEGMENT-MISSING                                               
035110            MOVE 'WDD906'          TO OUT-SEG-NAME-FB                     
035120            MOVE 'D'               TO OUT-KDUPD                           
035121            MOVE WDD906-IDLOPNRM-PL                                       
035122                                   TO W-IDLOPNRM-PL                       
035130            MOVE W-WDD906-X        TO OUT-IDKEY-WDD906                    
035140            MOVE SPACE             TO OUT-FLSENLEV                        
035150            PERFORM S11-WRITE-W22455                                      
035160         ELSE                                                             
035200            IF W6D111-ART-FLKLAR = JA                                     
035300               MOVE 'WDD906'       TO OUT-SEG-NAME-FB                     
035400               MOVE 'D'            TO OUT-KDUPD                           
035410               MOVE WDD906-IDLOPNRM-PL                                    
035420                                   TO W-IDLOPNRM-PL                       
035510               MOVE W-WDD906-X     TO OUT-IDKEY-WDD906                    
035600               MOVE SPACE          TO OUT-FLSENLEV                        
035700               PERFORM S11-WRITE-W22455                                   
035800            END-IF                                                        
035810         END-IF                                                           
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 E-WDD924-TILEVBSK SECTION.                                               
036210     MOVE 'E-WDD924-TILEVBSK   '     TO CURRENT-SECTION                   
036300     SKIP1                                                                
036400******************************************************************        
036500*                                                                *        
036600*    BEHANDLA LEVERANSBESKED                                     *        
036700*    UPPDATERA FLSENLEV PÅ LEVERANSBESKED VARS BERÄKNADE         *        
036800*    INLEVERANSVECKA ÄR UPPNÅDD                                  *        
036900*    OM NÅGOT LEVERANSBESKED ÄR FÖRSENAT MED MER ÄN 6 VECKOR,    *        
037000*    BORTTAGES ALL LEVERANSBESKED PÅ DENNA ARTIKEL/LEVERANTÖR    *        
037500*                                                                *        
037600******************************************************************        
037743                                                                          
037800     IF  SPAR-WDD901-IDARTNR = WS-IDARTNR                                 
037900     AND SPAR-WDD901-IDDC    = WS-IDDC                                    
038000     AND SPAR-WDD902-IDLEVNR = WS-IDLEVNR                                 
038100       MOVE 'WDD924'                 TO OUT-SEG-NAME-FB                   
038200       MOVE 'D'                      TO OUT-KDUPD                         
038210       MOVE WDD924-LEV-DALEVBSK-AVS                                       
038220                                     TO W-DALEVBSK-AVS                    
038300       MOVE W-WDD924-X               TO OUT-IDKEY-WDD924                  
038400       MOVE SPACE                    TO OUT-FLSENLEV                      
038500       PERFORM S11-WRITE-W22455                                           
038600     ELSE                                                                 
038700       MOVE WDD924-LEV-TILEVBSK-INL  TO DAT-I-TIDATUM                     
038800       PERFORM S01-KONVERTERA-TILEVBSK                                    
038900       MOVE WS-TIAAVV                TO                                   
038910                                    SPAR-WDD924-TILEVBSK-INL-AAVV         
039000       IF  WDD924-LEV-FLSENLEV = JA                                       
039200         IF SPAR-WDD924-TILEVBSK-INL-AAVV < W-TIAAVV-BORTTAG              
039300           MOVE SPAR-WDD901-IDARTNR  TO WS-IDARTNR                        
039310           MOVE SPAR-WDD901-IDDC     TO WS-IDDC                           
039400           MOVE SPAR-WDD902-IDLEVNR  TO WS-IDLEVNR                        
039500           MOVE 'WDD924'             TO OUT-SEG-NAME-FB                   
039600           MOVE 'D'                  TO OUT-KDUPD                         
039610           MOVE WDD924-LEV-DALEVBSK-AVS                                   
039620                                     TO W-DALEVBSK-AVS                    
039700           MOVE W-WDD924-X           TO OUT-IDKEY-WDD924                  
039800           MOVE SPACE                TO OUT-FLSENLEV                      
039900           PERFORM S11-WRITE-W22455                                       
040000         END-IF                                                           
040100       ELSE                                                               
040300         IF SPAR-WDD924-TILEVBSK-INL-AAVV <= W-TIAAVV-AKTUELL             
040400           MOVE 'WDD924'             TO OUT-SEG-NAME-FB                   
040500           MOVE 'R'                  TO OUT-KDUPD                         
040510           MOVE WDD924-LEV-DALEVBSK-AVS                                   
040520                                     TO W-DALEVBSK-AVS                    
040600           MOVE W-WDD924-X           TO OUT-IDKEY-WDD924                  
040700           MOVE JA                   TO OUT-FLSENLEV                      
040800           PERFORM S11-WRITE-W22455                                       
040900         END-IF                                                           
041100       END-IF                                                             
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
048000                                                                          
048100 F-WDD925-IDLEVBSK SECTION.                                               
048110     MOVE 'F-WDD925-IDLEVBSK            ' TO CURRENT-SECTION              
048200                                                                          
048300     IF WDD925-INFO-IDLEVBSK = 2                                          
048400         IF WDD925-INFO-TIBORT < DAGENS-DATUM-PACK                        
048410           MOVE 'WDD925'        TO OUT-SEG-NAME-FB                        
048500           MOVE 'D'             TO OUT-KDUPD                              
048510           MOVE WDD925-INFO-IDLEVBSK                                      
048520                                TO W-IDLEVBSK                             
048600           MOVE W-WDD925-X      TO OUT-IDKEY-WDD925                       
048610           MOVE SPACE           TO OUT-FLSENLEV                           
048620           PERFORM S11-WRITE-W22455                                       
048700         END-IF                                                           
048800      END-IF                                                              
048900     .                                                                    
049000     EJECT                                                                
049100 Z-FINIT SECTION.                                                         
049110     MOVE 'Z-FINIT                      ' TO CURRENT-SECTION              
049200     CLOSE W22455                                                         
049300     SKIP2                                                                
049400     MOVE 'S' TO POSTSUM-OPKOD                                            
049500     CALL POSTSUM USING POSTSUM-PARM                                      
049600     .                                                                    
049700     EJECT                                                                
049710 S01-KONVERTERA-TILEVBSK SECTION.                                         
049720     MOVE 'S01-KONVERTERA-TILEVBSK      ' TO CURRENT-SECTION              
049730                                                                          
049740     IF DAT-I-TIDATUM > 0                                                 
049750       MOVE 'AAMMDD'            TO DAT-KDDATFORM                          
049760       CALL WDATKONV         USING DAT-KDDATFORM                          
049770                                   DAT-I-TIDATUM                          
049780                                   DAT-O-TIDATUM                          
049790                                   DAT-KDSVAR                             
049791       IF DAT-KDSVAR-OK                                                   
049792         MOVE DAT-TIAAVVD       TO WS-TIAAVVD                             
049793       ELSE                                                               
049794         MOVE ZERO              TO WS-TIAAVVD                             
049795       END-IF                                                             
049796     ELSE                                                                 
049797       MOVE ZERO                TO WS-TIAAVVD                             
049798     END-IF                                                               
049799     IF WS-TIAAVVD = ZERO                                                 
049800       MOVE WDD924-LEV-DALEVBSK-AVS TO DAT-I-TIDATUM                      
049801       MOVE 'AAMMDD'            TO DAT-KDDATFORM                          
049802       CALL WDATKONV         USING DAT-KDDATFORM                          
049803       DAT-I-TIDATUM                                                      
049804       DAT-O-TIDATUM                                                      
049805       DAT-KDSVAR                                                         
049806       IF DAT-KDSVAR-OK                                                   
049807         MOVE DAT-TIAAVVD       TO WS-TIAAVVD                             
049808       ELSE                                                               
049809         MOVE ZERO              TO WS-TIAAVVD                             
049810       END-IF                                                             
049811       MOVE WS-TIAAVV           TO W009VADD-DATUM                         
049812       MOVE +2                  TO W009VADD-ANTAL                         
049813       CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                  
049814       MOVE W009VADD-DATUM      TO WS-TIAAVV                              
049815     END-IF                                                               
049816     .                                                                    
049817     EJECT                                                                
049818 S02-INIT-WORK-AREA SECTION.                                              
049820     MOVE 'S02-INIT-WORK-AREA           ' TO CURRENT-SECTION              
049900                                                                          
050000     MOVE LOW-VALUE             TO SPAR-AREA                              
050100     MOVE SPACE                 TO OUT-IDKEY-WDD902                       
050200     MOVE SPACE                 TO OUT-IDKEY-WDD905                       
050300     MOVE SPACE                 TO OUT-IDKEY-WDD906                       
050400     MOVE SPACE                 TO OUT-IDKEY-WDD907                       
050500     MOVE SPACE                 TO OUT-IDKEY-WDD924                       
050600     MOVE SPACE                 TO OUT-IDKEY-WDD925                       
050700     MOVE SPACE                 TO OUT-FLSENLEV                           
050800     .                                                                    
050900                                                                          
051000 S11-WRITE-W22455 SECTION.                                                
051010     MOVE 'S11-WRITE-W22455             ' TO CURRENT-SECTION              
051100                                                                          
051200     WRITE OUT-RECORD   FROM OUT-AREA                                     
051300                                                                          
051400     MOVE OUT-SEG-NAME-FB TO POSTSUM-TRANSTYP                             
051500     MOVE 'W22455'        TO POSTSUM-FDNAMN                               
051600     MOVE 'W22455D1'      TO POSTSUM-DDNAMN2                              
051700     CALL POSTSUM      USING POSTSUM-PARM                                 
051800     .                                                                    
051900     EJECT                                                                
052000 S99-ABEND SECTION.                                                       
052100                                                                          
052200     SKIP2                                                                
052300     MOVE 'S' TO POSTSUM-OPKOD                                            
052400     CALL POSTSUM USING POSTSUM-PARM                                      
052500     CALL ABEND USING RKOD-ABEND                                          
052600     .                                                                    
052700     EJECT                                                                
056300                                                                          
056400* --- IMS SECTIONS  ---                                                   
056500                                                                          
056600                                                                          
056700 IMS-GN-WDD9 SECTION.                                                     
056800     MOVE 'IMS-GN-WDD901        ' TO IMS-SEKTION                          
056900                                                                          
057000     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-WDD9                           
057100     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
057200     MOVE '  GAGKGB'             TO GOOD-STATUSCODES                      
057300     PERFORM IMS-STATUSCHECK                                              
057400     .                                                                    
057500                                                                          
057600 IMS-GU-W6D111 SECTION.                                                   
057610     MOVE 'IMS-GU-W6D111        ' TO IMS-SEKTION                          
057700                                                                          
057800     STRING 'W6D111  (W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
057900          DELIMITED BY SIZE     INTO SSA1                                 
058000     MOVE '  GE'                  TO GOOD-STATUSCODES                     
058100     CALL CBLTDLI USING GU W6D1B-PCB DLI-IO-W6D111 SSA1                   
058200     MOVE W6D1B-STATUS-CODE       TO STATUS-WS                            
058300     PERFORM IMS-STATUSCHECK                                              
058400                                                                          
058500     .                                                                    
058600     SKIP3                                                                
058700 IMS-STATUSCHECK SECTION.                                                 
058800                                                                          
058900     SET STATUS-IX TO 1                                                   
059000     SEARCH GOOD-STATUS                                                   
059100       AT END                                                             
059200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059300           DELIMITED BY SIZE INTO ERROR-TEXT                              
059400         DISPLAY ERROR-TEXT                                               
059500         CALL FELLOG                                                      
059600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
059700         CONTINUE                                                         
059800     END-SEARCH                                                           
059900     .                                                                    
060000                                                                          
