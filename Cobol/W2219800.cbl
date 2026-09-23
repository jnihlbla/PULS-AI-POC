000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2219800.                                                
000300 AUTHOR.         RAGUR SATHEESH.                                          
000400 DATE-WRITTEN.   18/06/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        IT CREATES A CSV W22198 FILE WITH SCORECARDS FOR PERFORMA        
000900*        NCE FOLLOW-UP.                                                   
001000*                                                                         
001100*        THE PROGRAM READS     WDD9 WDK9                                  
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INPUT FILE                                                 
002600     SELECT W01160                     ASSIGN TO W22198D1.                
002700     SKIP2                                                                
002800*          --- OUTPUT FILE                                                
002900     SELECT W22198                     ASSIGN TO W22198D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W01160                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W01160      -L.                                                
004000     SKIP3                                                                
004100 FD  W22198                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     EJECT                                                                
004500 01  UT-RECORD      PIC X(397).                                           
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -COPY WY2000W1                                                       
005000                                                                          
005100*    -COPY WY2000W2                                                       
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W2219800'.            
005400 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005500 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005600                                                                          
005700 77  YES                         PIC X       VALUE 'J'.                   
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006100 77  FIRST-SW                    PIC X       VALUE 'Y'.                   
006200     88  FIRST-LINE                          VALUE 'Y'.                   
006300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W01160                       VALUE 'J'.                   
006500 77  WS-TELEVBSK-SW              PIC X       VALUE 'N'.                   
006600     88  SW-TELEVBSK-FOUND                   VALUE 'J'.                   
006700     88  SW-TELEVBSK-NOT-FOUND               VALUE 'N'.                   
006800     EJECT                                                                
006900 01  ARBETSFAELT.                                                         
007000     03  WS-TIBORT                 PIC 9(5)     VALUE ZERO.               
007100     03  WS-TELEVBSK               PIC X(80)    VALUE SPACE.              
007200     03  WS-DALEVBSK-AVS           PIC 9(8)     VALUE ZERO.               
007300     03  FILLER  REDEFINES WS-DALEVBSK-AVS.                               
007400         05  WS-DALEVBSK-SS        PIC 9(2).                              
007500         05  WS-DALEVBSK-AAMMDD    PIC 9(6).                              
007600     03 WS-KVAKS                   PIC S9(7)    VALUE ZERO COMP-3.        
007700     03 WS-KVAVIS                  PIC S9(7)    VALUE ZERO COMP-3.        
007800     03 W-DISPONIBELT              PIC S9(7)    VALUE ZERO COMP-3.        
007900     03 W-ARB-SALDO                PIC S9(7)    VALUE ZERO COMP-3.        
008000     03 WS-PREV-IDARTNR            PIC S9(9)    VALUE ZERO COMP-3.        
008100     03 W-TIAAAAVV.                                                       
008200       05 W-TISEKEL                PIC  9(2)    VALUE ZERO.               
008300       05 W-TIAA                   PIC  9(2)    VALUE ZERO.               
008400       05 W-TIVV                   PIC  9(2)    VALUE ZERO.               
008500     03 W-DADISPIN                 PIC  9(6)    VALUE ZERO.               
008600     03 WS-TPO-NAESTA-INLEV        PIC S9(9)    VALUE ZERO COMP-3.        
008700     03 WS-KVOKS-TOT               PIC S9(9)    VALUE ZERO COMP-3.        
008800     EJECT                                                                
008900 01  TODAYS-DATE                   PIC 9(6)     VALUE ZERO.               
009000 01  FILLER REDEFINES TODAYS-DATE.                                        
009100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
009200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
009300     03  TODAYS-DATE-DAY         PIC 9(2).                                
009400     EJECT                                                                
009500*    ----- VALID IDDC CODES                                               
009600*01    -COPY WWDCKONS                                                     
009700       EJECT                                                              
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900*                                                                         
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010400     SKIP2                                                                
010500*                        ****    DYNAMISKA SUBPROGRAM                     
010600 01      DYNAMISKA-SUBPROGRAM.                                            
010700   03    WDATKONV        PIC X(8)    VALUE 'WDATKONV'.                    
010800   03    W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
010900*                        ****    PARAMETRAR TILL WDATKONV                 
011000 01  FILLER             PIC X(16) VALUE 'WDATAREA     '.                  
011100*01   -COPY WDATAREA.                                                     
011200     EJECT                                                                
011300*01      -COPY WMSGINIT                                                   
011400     SKIP3                                                                
011500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011600                                                                          
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012000     SKIP2                                                                
012100 01  ERROR-TEXT.                                                          
012200     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
012300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900 01  IN-AREA-START               PIC X(24)   VALUE                        
013000                                 'IN-AREA-START  '.                       
013100     SKIP2                                                                
013200                                                                          
013300*01  AREA -COPY W01160     -PRE IN-                                       
013400     EJECT                                                                
013500 01  UT-RAD-START               PIC X(24)   VALUE                         
013600                                 'UT-RAD-START  '.                        
013700     EJECT                                                                
013800 01  UT-HEADER.                                                           
013900     03 FILLER      PIC X(07)   VALUE 'ARTNR  '.                          
014000     03 FILLER      PIC X(01)   VALUE ';'.                                
014100     03 FILLER      PIC X(07)   VALUE 'IDLEVNR'.                          
014200     03 FILLER      PIC X(01)   VALUE ';'.                                
014300     03 FILLER      PIC X(06)   VALUE 'IDANSK'.                           
014400     03 FILLER      PIC X(01)   VALUE ';'.                                
014500     03 FILLER      PIC X(05)   VALUE 'KVROS'.                            
014600     03 FILLER      PIC X(01)   VALUE ';'.                                
014700     03 FILLER      PIC X(08)   VALUE 'ARBSALDO'.                         
014800     03 FILLER      PIC X(01)   VALUE ';'.                                
014900     03 FILLER      PIC X(05)   VALUE 'DATUT'.                            
015000     03 FILLER      PIC X(01)   VALUE ';'.                                
015100     03 FILLER      PIC X(07)   VALUE 'KVANTUT'.                          
015200     03 FILLER      PIC X(01)   VALUE ';'.                                
015300     03 FILLER      PIC X(05)   VALUE 'INLUT'.                            
015400     03 FILLER      PIC X(01)   VALUE ';'.                                
015500     03 FILLER      PIC X(04)   VALUE 'DISP'.                             
015600     03 FILLER      PIC X(01)   VALUE ';'.                                
015700     03 FILLER      PIC X(04)   VALUE 'FORA'.                             
015800     03 FILLER      PIC X(01)   VALUE ';'.                                
015900     03 FILLER      PIC X(08)   VALUE 'LEVBSKE1'.                         
016000     03 FILLER      PIC X(01)   VALUE ';'.                                
016100     03 FILLER      PIC X(08)   VALUE 'LEVBSKE2'.                         
016200     03 FILLER      PIC X(01)   VALUE ';'.                                
016300     03 FILLER      PIC X(08)   VALUE 'LEVBSKE3'.                         
016400     03 FILLER      PIC X(01)   VALUE ';'.                                
016500     03 FILLER      PIC X(08)   VALUE 'LEVBSKE4'.                         
016600     03 FILLER      PIC X(01)   VALUE ';'.                                
016700     03 FILLER      PIC X(07)   VALUE 'BSKBORT'.                          
016800     EJECT                                                                
016900*                                                                         
017000 01  UT-RAD.                                                              
017100     03 UT-IDARTNR              PIC Z(7)9  VALUE ZEROES.                  
017200     03 FILLER                  PIC X(01)  VALUE ';'.                     
017300     03 UT-IDLEVNR              PIC X(05)  VALUE SPACES.                  
017400     03 FILLER                  PIC X(01)  VALUE ';'.                     
017500     03 UT-IDANSK               PIC Z(03)  VALUE ZEROES.                  
017600     03 FILLER                  PIC X(01)  VALUE ';'.                     
017700     03 UT-KVROS                PIC -(7)9  VALUE ZEROES.                  
017800     03 FILLER                  PIC X(01)  VALUE ';'.                     
017900     03 UT-ARB-SALDO            PIC -(7)9  VALUE ZEROES.                  
018000     03 FILLER                  PIC X(01)  VALUE ';'.                     
018100     03 UT-TILEVBSK-AVS-UT      PIC Z(05)  VALUE ZEROES.                  
018200     03 FILLER                  PIC X(01)  VALUE ';'.                     
018300     03 UT-KVAVIS-UT            PIC Z(07)  VALUE ZEROES.                  
018400     03 FILLER                  PIC X(01)  VALUE ';'.                     
018500     03 UT-TILEVBSK-INL-C1-UT   PIC Z(05)  VALUE ZEROES.                  
018600     03 FILLER                  PIC X(01)  VALUE ';'.                     
018700     03 UT-TILEVBSK-DISP-C1-UT  PIC Z(05)  VALUE ZEROES.                  
018800     03 FILLER                  PIC X(01)  VALUE ';'.                     
018900     03 UT-FLFORAVI-C1-UT       PIC X(01)  VALUE SPACES.                  
019000     03 FILLER                  PIC X(01)  VALUE ';'.                     
019100     03 UT-TELEVBSK-EXT         PIC X(80)  VALUE SPACES.                  
019200     03 FILLER                  PIC X(01)  VALUE ';'.                     
019300     03 UT-TELEVBSK-EXT2        PIC X(80)  VALUE SPACES.                  
019400     03 FILLER                  PIC X(01)  VALUE ';'.                     
019500     03 UT-TELEVBSK-EXT3        PIC X(80)  VALUE SPACES.                  
019600     03 FILLER                  PIC X(01)  VALUE ';'.                     
019700     03 UT-TELEVBSK-EXT4        PIC X(80)  VALUE SPACES.                  
019800     03 FILLER                  PIC X(01)  VALUE ';'.                     
019900     03 UT-TIBORT               PIC Z(05)  VALUE ZEROES.                  
020000     EJECT                                                                
020100                                                                          
020200*    --- AREAS FOR IMS-SECTIONS                                           
020300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020400     SKIP3                                                                
020500 01  KEYS-FOR-DLI.                                                        
020600     03  W-WDD901KY-X.                                                    
020700         05 W-IDARTNR-X.                                                  
020800             07 W-IDARTNR        PIC S9(9) VALUE ZERO COMP-3.             
020900         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
021000     03  W-IDLEVNR-X.                                                     
021100         05  W-IDLEVNR           PIC X(5)  VALUE SPACE.                   
021200     03  W-IDLEVBSK-X.                                                    
021300         05  W-IDLEVBSK          PIC S9(1) VALUE ZERO  COMP-3.            
021400     03  W-DABEHOV-MIN-X.                                                 
021500         05  W-DABEHOV-MIN       PIC 9(6)  VALUE ZERO.                    
021600     03  W-DABEHOV-MAX-X.                                                 
021700         05  W-DABEHOV-MAX       PIC 9(6)  VALUE ZERO.                    
021800     SKIP2                                                                
021900*    --- STATUS-KOD FRÅN IMS                                              
022000 01  STATUS-WS                   PIC XX.                                  
022100     88  SEGMENT-FOUND                       VALUE '  '.                  
022200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
022300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
022500     SKIP2                                                                
022600 01  GOOD-STATUSCODES.                                                    
022700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(64).                               
023000 01  SSA2                        PIC X(64).                               
023100     EJECT                                                                
023200                                                                          
023300*    --- IMS FUNCTION CODES                                               
023400*01  -COPY W0003                                                          
023500     EJECT                                                                
023600                                                                          
023700*    ---  DLI INPUT-OUTPUT AREA                                           
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
023900 01  DLI-IO-WDD901.                                                       
024000*    03  -COPY WDD901 -PRE 901-                                           
024100      EJECT                                                               
024200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
024300 01  DLI-IO-WDD902.                                                       
024400*    03  -COPY WDD902 -PRE 902-                                           
024500      EJECT                                                               
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
024700 01  DLI-IO-WDD924.                                                       
024800*    03  -COPY WDD924 -PRE 924-                                           
024900      EJECT                                                               
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD925'.                      
025100 01  DLI-IO-WDD925.                                                       
025200*    03  -COPY WDD925 -PRE 925-                                           
025300     EJECT                                                                
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
025500 01  DLI-IO-WDK901.                                                       
025600*    03  -COPY WDK901 -PRE WDK9-                                          
025700     EJECT                                                                
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK911'.                      
025900 01  DLI-IO-WDK911.                                                       
026000*    03  -COPY WDK911 -PRE WDK9-                                          
026100     EJECT                                                                
026200 LINKAGE SECTION.                                                         
026300                                                                          
026400*01  -COPY W0008     -PRE WDD9-                                           
026500     05  FILLER           PIC X.                                          
026600     EJECT                                                                
026700*01  -COPY W0008     -PRE WDD9A-                                          
026800     05  FILLER           PIC X.                                          
026900     EJECT                                                                
027000*01  -COPY W0008     -PRE WDK9-                                           
027100     05  FILLER           PIC X.                                          
027200     EJECT                                                                
027300 PROCEDURE DIVISION  USING WDD9-PCB WDD9A-PCB WDK9-PCB.                   
027400 MAIN SECTION.                                                            
027500     ENTRY 'DLITCBL' USING WDD9-PCB WDD9A-PCB WDK9-PCB.                   
027600                                                                          
027700     PERFORM A-INIT                                                       
027800                                                                          
027900     PERFORM S01-READ-W01160                                              
028000     PERFORM UNTIL END-OF-W01160                                          
028100       PERFORM B-PROCESS                                                  
028200       PERFORM S01-READ-W01160                                            
028300     END-PERFORM                                                          
028400                                                                          
028500     PERFORM Z-FINIT                                                      
028600                                                                          
028700     MOVE ZERO                   TO RETURN-CODE                           
028800     GOBACK                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 A-INIT SECTION.                                                          
029200                                                                          
029300     OPEN INPUT  W01160                                                   
029400                                                                          
029500     OPEN OUTPUT W22198                                                   
029600                                                                          
029700     ACCEPT TODAYS-DATE        FROM DATE                                  
029800     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
029900     .                                                                    
030000     EJECT                                                                
030100 B-PROCESS SECTION.                                                       
030200     MOVE 'B-PROCESS '           TO CURRENT-SECTION                       
030300                                                                          
030400     INITIALIZE UT-RAD                                                    
030500                                                                          
030600     MOVE IN-CLAG-IDARTNR        TO W-IDARTNR                             
030700     MOVE WC-CDC-SE              TO W-IDDC                                
030800                                                                          
030900     PERFORM IMS-GU-WDD901                                                
031000     IF SEGMENT-FOUND                                                     
031100       PERFORM IMS-GNP-WDD902                                             
031200       PERFORM                                                            
031300         UNTIL SEGMENT-MISSING                                            
031400                                                                          
031500         MOVE 902-IDLEVNR        TO W-IDLEVNR                             
031600                                    UT-IDLEVNR                            
031700                                                                          
031800         PERFORM BA-READ-TELEVBSK                                         
031900                                                                          
032000         PERFORM IMS-GU-WDD902                                            
032100         PERFORM IMS-GNP-WDD924                                           
032200         IF SEGMENT-FOUND                                                 
032300           PERFORM UNTIL SEGMENT-MISSING                                  
032400             PERFORM BB-READ-LEVBSK                                       
032500             PERFORM BC-CALC-SALDO                                        
032600             PERFORM S11-WRITE-W22198                                     
032700             PERFORM IMS-GNP-WDD924                                       
032800           END-PERFORM                                                    
032900         ELSE                                                             
033000           IF SW-TELEVBSK-FOUND                                           
033100             PERFORM BC-CALC-SALDO                                        
033200             PERFORM S11-WRITE-W22198                                     
033300           END-IF                                                         
033400         END-IF                                                           
033500                                                                          
033600         PERFORM IMS-GNP-WDD902                                           
033700       END-PERFORM                                                        
033800                                                                          
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 BA-READ-TELEVBSK SECTION.                                                
034300                                                                          
034400     MOVE 'BA-READ-TELEVBSK   '  TO CURRENT-SECTION                       
034500                                                                          
034600     SET SW-TELEVBSK-NOT-FOUND   TO TRUE                                  
034700                                                                          
034800     PERFORM IMS-GU-WDD902                                                
034900     PERFORM IMS-GNP-WDD925                                               
035000     PERFORM UNTIL SEGMENT-MISSING                                        
035100       PERFORM BAA-CHECK-TIBORT                                           
035200                                                                          
035300* DO BELOW TO AVOID UNWANTED CELL BREAKS IN EXCEL                         
035400       INSPECT WS-TELEVBSK  REPLACING ALL  ';'    BY ':'                  
035500* DO BELOW TO AVOID UNWANTED LINE FIELDS IN EXCEL                         
035600       INSPECT WS-TELEVBSK  REPLACING ALL X'15'   BY '.'                  
035700* DO BELOW TO AVOID ADDITIONAL TEXT ADDED MY MAIL SYSTEM                  
035800       INSPECT WS-TELEVBSK  REPLACING ALL  'WWW.' BY 'WWW '               
035900                                                                          
036000       EVALUATE 925-INFO-IDLEVBSK                                         
036100         WHEN +2                                                          
036200           MOVE WS-TELEVBSK      TO UT-TELEVBSK-EXT                       
036300           MOVE WS-TIBORT        TO UT-TIBORT                             
036400         WHEN +4                                                          
036500           MOVE WS-TELEVBSK      TO UT-TELEVBSK-EXT2                      
036600           MOVE WS-TIBORT        TO UT-TIBORT                             
036700         WHEN +5                                                          
036800           MOVE WS-TELEVBSK      TO UT-TELEVBSK-EXT3                      
036900           MOVE WS-TIBORT        TO UT-TIBORT                             
037000         WHEN +6                                                          
037100           MOVE WS-TELEVBSK      TO UT-TELEVBSK-EXT4                      
037200           MOVE WS-TIBORT        TO UT-TIBORT                             
037300       END-EVALUATE                                                       
037400                                                                          
037500       MOVE SPACES               TO WS-TELEVBSK                           
037600       MOVE ZEROES               TO WS-TIBORT                             
037700       PERFORM IMS-GNP-WDD925                                             
037800     END-PERFORM                                                          
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200 BAA-CHECK-TIBORT SECTION.                                                
038300                                                                          
038400     MOVE 'BAA-CHECK-TIBORT   '  TO CURRENT-SECTION                       
038500                                                                          
038600     MOVE TODAYS-DATE            TO TMP1-YYMMDD                           
038700     MOVE 925-INFO-TIBORT        TO TMP2-YYMMDD                           
038800     PERFORM WY2000P1                                                     
038900     IF TMP1-YYMMDD <= TMP2-YYMMDD                                        
039000       SET SW-TELEVBSK-FOUND     TO TRUE                                  
039100       MOVE 925-INFO-TELEVBSK    TO WS-TELEVBSK                           
039200       MOVE 925-INFO-TIBORT      TO DAT-I-TIDATUM                         
039300       PERFORM S21-CALL-WDATKONV                                          
039400       MOVE DAT-TIAAVVD          TO WS-TIBORT                             
039500     END-IF                                                               
039600                                                                          
039700     IF UT-TIBORT > ZERO AND                                              
039800        WS-TIBORT > ZERO                                                  
039900       MOVE UT-TIBORT            TO TMP1-YYWWD                            
040000       MOVE WS-TIBORT            TO TMP2-YYWWD                            
040100       PERFORM WY2000P2                                                   
040200       IF TMP1-YYWWD > TMP2-YYWWD                                         
040300         CONTINUE                                                         
040400       ELSE                                                               
040500         MOVE UT-TIBORT          TO WS-TIBORT                             
040600       END-IF                                                             
040700     END-IF                                                               
040800                                                                          
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200 BB-READ-LEVBSK SECTION.                                                  
041300                                                                          
041400     MOVE 'BB-READ-LEVBSK    '   TO CURRENT-SECTION                       
041500                                                                          
041600     MOVE 924-LEV-DALEVBSK-AVS   TO WS-DALEVBSK-AVS                       
041700     MOVE WS-DALEVBSK-AAMMDD     TO DAT-I-TIDATUM                         
041800     PERFORM S21-CALL-WDATKONV                                            
041900     IF DAT-KDSVAR-OK                                                     
042000       MOVE DAT-TIAAVVD          TO UT-TILEVBSK-AVS-UT                    
042100     ELSE                                                                 
042200       MOVE ZERO                 TO UT-TILEVBSK-AVS-UT                    
042300     END-IF                                                               
042400                                                                          
042500     IF 924-LEV-KVAVIS-BSKKVAR > 0                                        
042600       MOVE 924-LEV-KVAVIS-BSKKVAR                                        
042700                                 TO UT-KVAVIS-UT                          
042800     END-IF                                                               
042900                                                                          
043000     IF 924-LEV-TILEVBSK-INL = 0                                          
043100       MOVE ZERO                 TO UT-TILEVBSK-INL-C1-UT                 
043200     ELSE                                                                 
043300       MOVE 924-LEV-TILEVBSK-INL TO DAT-I-TIDATUM                         
043400       PERFORM S21-CALL-WDATKONV                                          
043500       IF DAT-KDSVAR-OK                                                   
043600         MOVE DAT-TIAAVVD        TO UT-TILEVBSK-INL-C1-UT                 
043700       ELSE                                                               
043800         MOVE ZERO               TO UT-TILEVBSK-INL-C1-UT                 
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     IF 924-LEV-TILEVBSK-DISP = 0                                         
044300       MOVE ZERO                 TO UT-TILEVBSK-DISP-C1-UT                
044400     ELSE                                                                 
044500       MOVE 924-LEV-TILEVBSK-DISP                                         
044600                                 TO DAT-I-TIDATUM                         
044700       PERFORM S21-CALL-WDATKONV                                          
044800       IF DAT-KDSVAR-OK                                                   
044900         MOVE DAT-TIAAVVD        TO UT-TILEVBSK-DISP-C1-UT                
045000       ELSE                                                               
045100         MOVE ZERO               TO UT-TILEVBSK-DISP-C1-UT                
045200       END-IF                                                             
045300     END-IF                                                               
045400                                                                          
045500     MOVE 924-LEV-FLFORAVI       TO UT-FLFORAVI-C1-UT                     
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900 BC-CALC-SALDO SECTION.                                                   
046000                                                                          
046100     MOVE 'BC-CALC-SALDO      '  TO CURRENT-SECTION                       
046200                                                                          
046300     IF IN-CLAG-IDARTNR = WS-PREV-IDARTNR                                 
046400       MOVE W-ARB-SALDO          TO UT-ARB-SALDO                          
046500     ELSE                                                                 
046600       MOVE IN-CLAG-IDARTNR      TO UT-IDARTNR                            
046700                                    WS-PREV-IDARTNR                       
046800       MOVE IN-CLAG-IDANSK       TO UT-IDANSK                             
046900       MOVE IN-CLAG-KVROS        TO UT-KVROS                              
047000                                                                          
047100       PERFORM BCA-READ-WDK9                                              
047200                                                                          
047300       COMPUTE WS-KVAKS           = IN-CLAG-KVAKS-CDC                     
047400                                  + IN-CLAG-KVAKS-PAV                     
047500                                  + IN-CLAG-KVAKS-T                       
047600                                                                          
047700       COMPUTE W-DISPONIBELT      = IN-CLAG-KVLS                          
047800                                  - IN-CLAG-KVRESS                        
047900                                                                          
048000       COMPUTE W-ARB-SALDO        = W-DISPONIBELT                         
048100                                  - IN-CLAG-KVROS                         
048200                                  - WS-TPO-NAESTA-INLEV                   
048300                                  - WS-KVOKS-TOT                          
048400                                  + WS-KVAKS                              
048500                                                                          
048600       MOVE W-ARB-SALDO          TO UT-ARB-SALDO                          
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 BCA-READ-WDK9   SECTION.                                                 
049200                                                                          
049300     MOVE 'BCA-READ-WDK9      '  TO CURRENT-SECTION                       
049400                                                                          
049500     MOVE ZERO                   TO WS-KVOKS-TOT                          
049600                                    WS-TPO-NAESTA-INLEV                   
049700                                                                          
049800     PERFORM IMS-GU-WDK901                                                
049900                                                                          
050000     IF SEGMENT-FOUND                                                     
050100       COMPUTE WS-KVOKS-TOT       = WDK9-ART-KVOKS-BULK                   
050200                                  + WDK9-ART-KVOKS-DAG                    
050300                                  + WDK9-ART-KVOKS-VOR                    
050400       IF IN-CLAG-TIDISPIN = ZERO                                         
050500         MOVE WDK9-ART-SUTPO-TOT TO WS-TPO-NAESTA-INLEV                   
050600       ELSE                                                               
050700         PERFORM BCAA-READ-WDK911                                         
050800       END-IF                                                             
050900     END-IF                                                               
051000                                                                          
051100     .                                                                    
051200     EJECT                                                                
051300 BCAA-READ-WDK911 SECTION.                                                
051400                                                                          
051500     MOVE 'BCAA-READ-WDK911   '  TO CURRENT-SECTION                       
051600                                                                          
051700     MOVE ZERO                   TO W-DABEHOV-MIN                         
051800     PERFORM BCAAA-KONV-TIDISPIN                                          
051900     MOVE W-DADISPIN             TO W-DABEHOV-MAX                         
052000     PERFORM IMS-GNP-WDK911                                               
052100     PERFORM UNTIL SEGMENT-MISSING                                        
052200       COMPUTE WS-TPO-NAESTA-INLEV = WS-TPO-NAESTA-INLEV                  
052300                                   + WDK9-ANT-SUTPO-PB                    
052400                                   + WDK9-ANT-SUTPO-EJPB                  
052500       PERFORM IMS-GNP-WDK911                                             
052600     END-PERFORM                                                          
052700     .                                                                    
052800     EJECT                                                                
052900 BCAAA-KONV-TIDISPIN SECTION.                                             
053000                                                                          
053100     MOVE 'BCAAA-KONV-TIDISPIN'  TO CURRENT-SECTION                       
053200                                                                          
053300     MOVE IN-CLAG-TIDISPIN       TO DAT-I-TIDATUM                         
053400     PERFORM S21-CALL-WDATKONV                                            
053500                                                                          
053600     IF DAT-KDSVAR-OK                                                     
053700       MOVE DAT-TIAA-VECKA       TO W-TIAA                                
053800       MOVE DAT-TIVV             TO W-TIVV                                
053900       MOVE DAT-TISEKEL          TO W-TISEKEL                             
054000       MOVE W-TIAAAAVV           TO W-DADISPIN                            
054100     ELSE                                                                 
054200       MOVE ZERO                 TO W-DADISPIN                            
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600                                                                          
054700 S01-READ-W01160 SECTION.                                                 
054800     READ W01160               INTO IN-AREA                               
054900       AT END                                                             
055000         MOVE HIGH-VALUE         TO IN-AREA                               
055100         SET END-OF-W01160       TO TRUE                                  
055200                                                                          
055300       NOT AT END                                                         
055400         MOVE 'W01160'           TO POSTSUM-FDNAMN                        
055500         MOVE 'W22198D1'         TO POSTSUM-DDNAMN2                       
055600         MOVE SPACE              TO POSTSUM-TRANSTYP                      
055700         CALL POSTSUM         USING POSTSUM-PARM                          
055800     END-READ                                                             
055900     .                                                                    
056000     EJECT                                                                
056100 S21-CALL-WDATKONV         SECTION.                                       
056200     MOVE 'S21-CALL-WDATKONV       ' TO CURRENT-SECTION                   
056300                                                                          
056400     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
056500     CALL WDATKONV            USING DAT-KDDATFORM                         
056600                                    DAT-I-TIDATUM                         
056700                                    DAT-O-TIDATUM                         
056800                                    DAT-KDSVAR                            
056900     IF DAT-KDSVAR-FEL                                                    
057000       MOVE 'INVALID DATE - WDATKONV'                                     
057100                                 TO ERROR-TEXT                            
057200       CALL FELLOG                                                        
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600                                                                          
057700 S11-WRITE-W22198 SECTION.                                                
057800                                                                          
057900     IF FIRST-LINE                                                        
058000       WRITE UT-RECORD         FROM UT-HEADER                             
058100       MOVE NOO                  TO FIRST-SW                              
058200     END-IF                                                               
058300     WRITE UT-RECORD           FROM UT-RAD                                
058400                                                                          
058500     MOVE SPACES                 TO POSTSUM-TRANSTYP                      
058600     MOVE 'W22198'               TO POSTSUM-FDNAMN                        
058700     MOVE 'W22198D2'             TO POSTSUM-DDNAMN2                       
058800     CALL POSTSUM             USING POSTSUM-PARM                          
058900     .                                                                    
059000     EJECT                                                                
059100 Z-FINIT SECTION.                                                         
059200     CLOSE W01160                                                         
059300           W22198                                                         
059400     SKIP2                                                                
059500     MOVE 'S'                    TO POSTSUM-OPKOD                         
059600     CALL POSTSUM             USING POSTSUM-PARM                          
059700     .                                                                    
059800     EJECT                                                                
059900* --- IMS SECTIONS  ---                                                   
060000                                                                          
060100 IMS-GU-WDD901 SECTION.                                                   
060200     MOVE 'IMS-GU-WDD901  '      TO CURRENT-IMS-SECTION                   
060300                                                                          
060400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
060500             DELIMITED BY SIZE INTO SSA1                                  
060600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
060700     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
060800     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
060900     PERFORM IMS-STATUSCHECK                                              
061000     .                                                                    
061100     EJECT                                                                
061200 IMS-GNP-WDD902 SECTION.                                                  
061300     MOVE 'IMS-GNP-WDD902  '     TO CURRENT-IMS-SECTION                   
061400                                                                          
061500     MOVE 'WDD902   '            TO SSA1                                  
061600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
061700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
061800     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
061900     PERFORM IMS-STATUSCHECK                                              
062000     .                                                                    
062100     EJECT                                                                
062200 IMS-GU-WDD902 SECTION.                                                   
062300     MOVE 'IMS-GU-WDD902  '      TO CURRENT-IMS-SECTION                   
062400                                                                          
062500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
062600             DELIMITED BY SIZE INTO SSA1                                  
062700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
062800             DELIMITED BY SIZE INTO SSA2                                  
062900     MOVE SPACES                 TO GOOD-STATUSCODES                      
063000     CALL CBLTDLI USING GU WDD9A-PCB DLI-IO-WDD902 SSA1 SSA2              
063100     MOVE WDD9A-STATUS-CODE      TO STATUS-WS                             
063200     PERFORM IMS-STATUSCHECK                                              
063300     .                                                                    
063400     EJECT                                                                
063500 IMS-GNP-WDD924 SECTION.                                                  
063600     MOVE 'IMS-GNP-WDD924 '      TO CURRENT-IMS-SECTION                   
063700     MOVE 'WDD924   '            TO SSA1                                  
063800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
063900     CALL CBLTDLI USING GNP WDD9A-PCB DLI-IO-WDD924 SSA1                  
064000     MOVE WDD9A-STATUS-CODE      TO STATUS-WS                             
064100     PERFORM IMS-STATUSCHECK                                              
064200     .                                                                    
064300     EJECT                                                                
064400 IMS-GNP-WDD925 SECTION.                                                  
064500     MOVE 'IMS-GNP-WDD925 '      TO CURRENT-IMS-SECTION                   
064600     MOVE 'WDD925   '            TO SSA1                                  
064700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
064800     CALL CBLTDLI USING GNP WDD9A-PCB DLI-IO-WDD925 SSA1                  
064900     MOVE WDD9A-STATUS-CODE      TO STATUS-WS                             
065000     PERFORM IMS-STATUSCHECK                                              
065100     .                                                                    
065200     EJECT                                                                
065300 IMS-GU-WDK901 SECTION.                                                   
065400     MOVE 'IMS-GU-WDK901'        TO CURRENT-IMS-SECTION                   
065500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
065600             DELIMITED BY SIZE INTO SSA1                                  
065700     MOVE '  GE' TO GOOD-STATUSCODES                                      
065800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
065900     MOVE WDK9-STATUS-CODE       TO STATUS-WS                             
066000     PERFORM IMS-STATUSCHECK                                              
066100     .                                                                    
066200     SKIP3                                                                
066300 IMS-GNP-WDK911         SECTION.                                          
066400     MOVE 'IMS-GNP-WDK911'       TO CURRENT-IMS-SECTION                   
066500     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-MIN-X                         
066600                     '&DABEHOV <=' W-DABEHOV-MAX-X ')'                    
066700             DELIMITED BY SIZE INTO SSA1                                  
066800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
066900     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-WDK911 SSA1                   
067000     MOVE WDK9-STATUS-CODE       TO STATUS-WS                             
067100     PERFORM IMS-STATUSCHECK                                              
067200           .                                                              
067300           EJECT                                                          
067400 IMS-STATUSCHECK SECTION.                                                 
067500                                                                          
067600     SET STATUS-IX               TO 1                                     
067700     SEARCH GOOD-STATUS                                                   
067800       AT END                                                             
067900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
068000             DELIMITED BY SIZE INTO ERROR-TEXT                            
068100         DISPLAY ERROR-TEXT                                               
068200         CALL FELLOG                                                      
068300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
068400         CONTINUE                                                         
068500     END-SEARCH                                                           
068600     .                                                                    
068700     EJECT                                                                
068800*    -COPY WY2000P1                                                       
068900     EJECT                                                                
069000*    -COPY WY2000P2                                                       
