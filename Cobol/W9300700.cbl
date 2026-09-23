000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9300700.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   14/02/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE MONTHLY CURRENCY ON WDG2 (9305/9306/9308)                 
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDG2                                       
001200*        CREATES FILE FOR D&P                                             
001300*                                                                         
001400*    CHANGE LOG:                                                          
001500*      YY/MM/DD                                                           
001600*      14/02/11 - REDDY RAHUL     - INITIAL VERSION                       
001700*                                   SCR 10222254                          
001800*                                                                         
001900*      15/07/13 - REDDY RAHUL     - NEW FILE TO D&P (FORMATTED)           
002000*                                   ETRACKER 10257890                     
002100*                                                                         
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- INPUT FILE                                                 
003100     SELECT WF1022                     ASSIGN TO W93007D1.                
003200     SKIP2                                                                
003300*          --- OUTPUT FILE                                                
003400     SELECT W93007                     ASSIGN TO W93007D2.                
003500     SKIP2                                                                
003600*          --- OUTPUT FILE (FORMATTED)                                    
003700     SELECT W93008                     ASSIGN TO W93007D3.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  WF1022                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY WF10P002  -L.                                                  
004800     SKIP3                                                                
004900 FD  W93007                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W93001 -PRE UT- -L.                                       
005400     EJECT                                                                
005500 FD  W93008                                                               
005600     RECORDING       V                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900 01  UT2-POST                    PIC X(75).                               
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W9300700'.            
006400 01  CHKP-VAR.                                                            
006500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
007000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
007100 77  YES                         PIC X       VALUE 'J'.                   
007200 77  NOO                         PIC X       VALUE 'N'.                   
007300 77  W-IDLEGSEL                  PIC X(4)    VALUE SPACES.                
007400     SKIP2                                                                
007500 01  WS-DATE-YYYYMMDD.                                                    
007600     03  FILLER                  PIC 9(02).                               
007700     03  WS-DATE-YYMMDD          PIC 9(06).                               
007800                                                                          
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008200                                                                          
008300 77  WF1022-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-WF1022                       VALUE 'Y'.                   
008500                                                                          
008600 77  HEADER-WRITTEN-SW           PIC X       VALUE 'N'.                   
008700     88  HEADER-WRITTEN-NO                   VALUE 'N'.                   
008800     88  HEADER-WRITTEN-YES                  VALUE 'Y'.                   
008900     EJECT                                                                
009000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
009100     EJECT                                                                
009200*    -COPY WY2000W1                                                       
009300     SKIP3                                                                
009400*    -COPY WWIDFTG                                                        
009500     SKIP3                                                                
009600 01  GENERAL-SUBPROGRAMS.                                                 
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005   -PRE  POSTSUM-                                         
010500     EJECT                                                                
010600 01  IN-AREA-START               PIC X(24)   VALUE                        
010700                                             'IN-AREA-START'.             
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY WF10P002 -PRE IN-                                         
011100     EJECT                                                                
011200 01  UT-AREA-START               PIC X(24)   VALUE                        
011300                                             'UT-AREA-START'.             
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W93001   -PRE UT-                                         
011700*                                                                         
011800     EJECT                                                                
011900 01  UT2-AREA-START              PIC X(24)   VALUE                        
012000                                             'UT2-AREA-START'.            
012100     SKIP2                                                                
012200*                                                                         
012300 01  UT2-AREA                    PIC X(71)   VALUE SPACE.                 
012400                                                                          
012500 01  UT2-HEADER                  PIC X(71)   VALUE                        
012600      'COMPANY CODE;CURRENCY;CONVERT FACTOR;EXCHANGE RATE;START DA        
012700-     'TE;STOP DATE'.                                                     
012800                                                                          
012900 01  UT2-RECORD.                                                          
013000     03  UT2-SAP-IDFTG           PIC 9(2)    VALUE ZERO.                  
013100     03  FILLER                  PIC X       VALUE ';'.                   
013200     03  UT2-SAP-KDVALISO        PIC X(3)    VALUE SPACE.                 
013300     03  FILLER                  PIC X       VALUE ';'.                   
013400     03  UT2-SAP-REVALUTA-FROM   PIC 9(5)    VALUE ZERO.                  
013500     03  FILLER                  PIC X       VALUE ';'.                   
013600     03  UT2-SAP-REVALUTA-TO     PIC 9(5)    VALUE ZERO.                  
013700     03  FILLER                  PIC X       VALUE ';'.                   
013800     03  UT2-SAP-PRKURS          PIC 9(6).9(5)                            
013900                                             VALUE ZERO.                  
014000     03  FILLER                  PIC X       VALUE ';'.                   
014100     03  UT2-SAP-TISTADAT        PIC 9(6)    VALUE ZERO.                  
014200     03  FILLER                  PIC X       VALUE ';'.                   
014300     03  UT2-SAP-TISTODAT        PIC 9(6)    VALUE ZERO.                  
014400     03  FILLER                  PIC X(26)   VALUE SPACE.                 
014500*                                                                         
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014800     SKIP3                                                                
014900 01  KEYS-TILL-DLI.                                                       
015000     03  W-WDGXKEY-X.                                                     
015100         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
015200         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
015300         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
015400         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
015500     03  W-KDVALISO-X.                                                    
015600         05  W-KDVALISO          PIC X(3)    VALUE SPACE.                 
015700     03  W-TISTADA9-X.                                                    
015800         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
015900     SKIP2                                                                
016000*    --- STATUS-KOD FRÅN IMS                                              
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-FOUND                       VALUE '  '.                  
016300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016500     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
016600     88  IMS-NOT-OK                          VALUE 'XD'.                  
016700     SKIP2                                                                
016800 01  GOOD-STATUSCODES.                                                    
016900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300 01  SSA3                        PIC X(64).                               
017400     EJECT                                                                
017500*    --- IMS FUNCTION CODES                                               
017600*01  -COPY W0003                                                          
017700     EJECT                                                                
017800*    ---  DLI INPUT-OUTPUT AREA                                           
017900                                                                          
018000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9305'.                    
018100 01  DLI-IO-WDGX9305.                                                     
018200*    03  -COPY WDGX9305                                                   
018300     EJECT                                                                
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
018500 01  DLI-IO-WDGX9306.                                                     
018600*    03  -COPY WDGX9306                                                   
018700     EJECT                                                                
018800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
018900 01  DLI-IO-WDGX9308.                                                     
019000*    03  -COPY WDGX9308                                                   
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
019200 01  DLI-IO-WDB601.                                                       
019300*    03  -COPY WDB601                                                     
019400                                                                          
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700                                                                          
019800*01  -COPY W0009   -PRE MSG-                                              
019900                                                                          
020000*01  -COPY W0008  -PRE 9305-                                              
020100     05  FILLER                  PIC X.                                   
020200*01  -COPY W0008  -PRE WDB6-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500 PROCEDURE DIVISION  USING MSG-PCB 9305-PCB WDB6-PCB.                     
020600 MAIN SECTION.                                                            
020700     ENTRY 'DLITCBL' USING MSG-PCB 9305-PCB WDB6-PCB.                     
020800                                                                          
020900     SKIP2                                                                
021000     PERFORM A-INIT                                                       
021100     PERFORM S01-READ-WF1022                                              
021200     PERFORM UNTIL END-OF-WF1022                                          
021300       IF CHKP-ANT > CHKP-MAX                                             
021400         PERFORM X-TAKE-CHECKPOINT                                        
021500       END-IF                                                             
021600       PERFORM B-PROCESS                                                  
021700       PERFORM S01-READ-WF1022                                            
021800     END-PERFORM                                                          
021900                                                                          
022000     PERFORM Z-FINIT                                                      
022100                                                                          
022200     MOVE ZERO                   TO RETURN-CODE                           
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700     SKIP2                                                                
022800                                                                          
022900     PERFORM IMS-RESTART                                                  
023000                                                                          
023100     OPEN INPUT WF1022                                                    
023200                                                                          
023300     OPEN OUTPUT W93007                                                   
023400                 W93008                                                   
023500                                                                          
023600     ACCEPT TODAYS-DATE        FROM DATE                                  
023700                                                                          
023800     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
023900     .                                                                    
024000     EJECT                                                                
024100 B-PROCESS SECTION.                                                       
024200                                                                          
024300     IF IN-IDLEGSEL = 'VCCS'                                              
024400       MOVE 'SEK'                TO W-KDVALISO-HUV                        
024500     ELSE                                                                 
024600       MOVE IN-IDLEGSEL          TO W-IDLEGSEL                            
024700       PERFORM IMS-GU-WDB601-LEGSEL                                       
024800       IF SEGMENT-FOUND                                                   
024900         MOVE DCS-KDVALISO       TO W-KDVALISO-HUV                        
025000       END-IF                                                             
025100       IF IN-IDLEGSEL = 'VCAE'                                            
025200          MOVE 'AED'             TO W-KDVALISO-HUV                        
025300       END-IF                                                             
025400     END-IF                                                               
025500     MOVE IN-KDVALISO            TO W-KDVALISO                            
025600     PERFORM IMS-GU-WDGX9306                                              
025700     IF SEGMENT-MISSING                                                   
025800       MOVE IN-KDVALISO          TO 9306-KDVALISO                         
025900       PERFORM IMS-ISRT-WDGX9306                                          
026000       ADD +1                    TO CHKP-ANT                              
026100       MOVE IN-DASTADAT          TO WS-DATE-YYYYMMDD                      
026200       MOVE WS-DATE-YYMMDD       TO 9308-TISTADAT                         
026300       COMPUTE 9308-TISTADAT-9KOMPL =                                     
026400                   9999999 - 9308-TISTADAT                                
026500       MOVE IN-REVALUTA-TO       TO 9308-REVALUTA-TO                      
026600       MOVE IN-REVALUTA-FROM     TO 9308-REVALUTA-FROM                    
026700       MOVE IN-PRKURS            TO 9308-PRKURS                           
026800       MOVE IN-DAREGDAT          TO WS-DATE-YYYYMMDD                      
026900       MOVE WS-DATE-YYMMDD       TO 9308-TIREGDAT                         
027000       PERFORM IMS-ISRT-WDGX9308                                          
027100       ADD +1                    TO CHKP-ANT                              
027200     ELSE                                                                 
027300       MOVE IN-DASTADAT          TO WS-DATE-YYYYMMDD                      
027400       COMPUTE W-TISTADAT-9KOMPL =                                        
027500                   9999999 - WS-DATE-YYMMDD                               
027600       PERFORM IMS-GHNP-WDGX9308                                          
027700       IF SEGMENT-MISSING                                                 
027800         MOVE IN-DASTADAT        TO WS-DATE-YYYYMMDD                      
027900         MOVE WS-DATE-YYMMDD     TO 9308-TISTADAT                         
028000         COMPUTE 9308-TISTADAT-9KOMPL =                                   
028100                     9999999 - 9308-TISTADAT                              
028200         MOVE IN-REVALUTA-TO     TO 9308-REVALUTA-TO                      
028300         MOVE IN-REVALUTA-FROM   TO 9308-REVALUTA-FROM                    
028400         MOVE IN-PRKURS          TO 9308-PRKURS                           
028500         MOVE IN-DAREGDAT        TO WS-DATE-YYYYMMDD                      
028600         MOVE WS-DATE-YYMMDD     TO 9308-TIREGDAT                         
028700         PERFORM IMS-ISRT-WDGX9308                                        
028800         ADD +1                  TO CHKP-ANT                              
028900       ELSE                                                               
029000         MOVE TODAYS-DATE        TO TMP1-YYMMDD                           
029100         MOVE 9308-TISTADAT      TO TMP2-YYMMDD                           
029200         PERFORM WY2000P1                                                 
029300         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
029400           MOVE IN-DASTADAT      TO WS-DATE-YYYYMMDD                      
029500           MOVE WS-DATE-YYMMDD   TO 9308-TISTADAT                         
029600           COMPUTE 9308-TISTADAT-9KOMPL =                                 
029700                       9999999 - 9308-TISTADAT                            
029800           MOVE IN-REVALUTA-TO   TO 9308-REVALUTA-TO                      
029900           MOVE IN-REVALUTA-FROM TO 9308-REVALUTA-FROM                    
030000           MOVE IN-PRKURS        TO 9308-PRKURS                           
030100           MOVE IN-DAREGDAT      TO WS-DATE-YYYYMMDD                      
030200           MOVE WS-DATE-YYMMDD   TO 9308-TIREGDAT                         
030300           PERFORM IMS-REPL-WDGX9308                                      
030400           ADD +1                TO CHKP-ANT                              
030500         END-IF                                                           
030600       END-IF                                                             
030700     END-IF                                                               
030800                                                                          
030900     IF IN-IDLEGSEL ='VCCS'                                               
031000       MOVE WC-IDFTG-PV          TO UT-SAP-IDFTG                          
031100     ELSE                                                                 
031200       MOVE DCS-IDFTG            TO UT-SAP-IDFTG                          
031300                                    UT2-SAP-IDFTG                         
031400     END-IF                                                               
031500     MOVE 9306-KDVALISO          TO UT-SAP-KDVALISO                       
031600     MOVE 9308-REVALUTA-TO       TO UT-SAP-REVALUTA                       
031700     MOVE 9308-PRKURS            TO UT-SAP-PRKURS                         
031800     MOVE 9308-TISTADAT          TO UT-SAP-TISTADAT                       
031900     MOVE ZERO                   TO UT-SAP-TISTODAT                       
032000     MOVE SPACE                  TO UT-SAP-FILLERX                        
032100                                                                          
032300                                                                          
032400     IF IN-IDLEGSEL NOT ='VCCS'                                           
032500       MOVE 9306-KDVALISO        TO UT2-SAP-KDVALISO                      
032600       MOVE 9308-REVALUTA-TO     TO UT2-SAP-REVALUTA-TO                   
032700       MOVE 9308-REVALUTA-FROM   TO UT2-SAP-REVALUTA-FROM                 
032800       MOVE 9308-PRKURS          TO UT2-SAP-PRKURS                        
032900       MOVE 9308-TISTADAT        TO UT2-SAP-TISTADAT                      
033000       MOVE ZERO                 TO UT2-SAP-TISTODAT                      
033100                                                                          
033200       IF HEADER-WRITTEN-NO                                               
033300         MOVE UT2-HEADER         TO UT2-AREA                              
033400         SET HEADER-WRITTEN-YES  TO TRUE                                  
033500         PERFORM S12-WRITE-W93008                                         
033600       END-IF                                                             
033700                                                                          
033800       MOVE UT2-RECORD           TO UT2-AREA                              
033900                                                                          
034000       PERFORM S11-WRITE-W93007                                           
034100       PERFORM S12-WRITE-W93008                                           
034110     ELSE                                                                 
034120       PERFORM S11-WRITE-W93007                                           
034200     END-IF                                                               
034300                                                                          
034400     .                                                                    
034500     EJECT                                                                
034600 Z-FINIT SECTION.                                                         
034700                                                                          
034800     CLOSE WF1022                                                         
034900           W93007                                                         
035000           W93008                                                         
035100                                                                          
035200     MOVE 'S'                    TO POSTSUM-OPKOD                         
035300     CALL POSTSUM             USING POSTSUM-PARM                          
035400     .                                                                    
035500     EJECT                                                                
035600 S01-READ-WF1022  SECTION.                                                
035700     SKIP2                                                                
035800     READ WF1022               INTO IN-AREA                               
035900     AT END                                                               
036000       MOVE HIGH-VALUE           TO IN-AREA                               
036100       SET END-OF-WF1022         TO TRUE                                  
036200                                                                          
036300     NOT AT END                                                           
036400       MOVE 'WF1022'             TO POSTSUM-FDNAMN                        
036500       MOVE 'W93007D1'           TO POSTSUM-DDNAMN2                       
036600       CALL POSTSUM           USING POSTSUM-PARM                          
036700                                                                          
036800     END-READ                                                             
036900     .                                                                    
037000     EJECT                                                                
037100 S11-WRITE-W93007 SECTION.                                                
037200     SKIP2                                                                
037300     WRITE UT-POST             FROM UT-AREA                               
037400                                                                          
037500     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
037600     MOVE 'W93007 '              TO POSTSUM-FDNAMN                        
037700     MOVE 'W93007D2'             TO POSTSUM-DDNAMN2                       
037800     CALL POSTSUM             USING POSTSUM-PARM                          
037900     .                                                                    
038000     EJECT                                                                
038100 S12-WRITE-W93008 SECTION.                                                
038200     SKIP2                                                                
038300     WRITE UT2-POST            FROM UT2-AREA                              
038400                                                                          
038500     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
038600     MOVE 'W93008 '              TO POSTSUM-FDNAMN                        
038700     MOVE 'W93007D3'             TO POSTSUM-DDNAMN2                       
038800     CALL POSTSUM             USING POSTSUM-PARM                          
038900     .                                                                    
039000     EJECT                                                                
039100 X-TAKE-CHECKPOINT   SECTION.                                             
039200                                                                          
039300     PERFORM IMS-CHECKPOINT                                               
039400     MOVE ZERO                   TO CHKP-ANT                              
039500     .                                                                    
039600     EJECT                                                                
039700* --- IMS SECTIONS  ---                                                   
039800                                                                          
039900     EJECT                                                                
040000 IMS-GU-WDGX9306 SECTION.                                                 
040100                                                                          
040200     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
040300             DELIMITED BY SIZE INTO SSA1                                  
040400     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
040500             DELIMITED BY SIZE INTO SSA2                                  
040600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
040700     CALL CBLTDLI             USING GU                                    
040800                                    9305-PCB                              
040900                                    DLI-IO-WDGX9306                       
041000                                    SSA1 SSA2                             
041100     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
041200     PERFORM IMS-STATUSCHECK                                              
041300     .                                                                    
041400     SKIP3                                                                
041500 IMS-ISRT-WDGX9306 SECTION.                                               
041600                                                                          
041700     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
041800             DELIMITED BY SIZE INTO SSA1                                  
041900     MOVE 'WDGX9306 ' TO SSA2                                             
042000     MOVE '  '                   TO GOOD-STATUSCODES                      
042100     CALL CBLTDLI             USING ISRT                                  
042200                                    9305-PCB                              
042300                                    DLI-IO-WDGX9306                       
042400                                    SSA1 SSA2                             
042500     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
042600     PERFORM IMS-STATUSCHECK                                              
042700     .                                                                    
042800     EJECT                                                                
042900 IMS-GHNP-WDGX9308 SECTION.                                               
043000                                                                          
043100     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
043200             DELIMITED BY SIZE INTO SSA1                                  
043300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
043400     CALL CBLTDLI             USING GHNP                                  
043500                                    9305-PCB                              
043600                                    DLI-IO-WDGX9308                       
043700                                    SSA1                                  
043800     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
043900     PERFORM IMS-STATUSCHECK                                              
044000     .                                                                    
044100     SKIP3                                                                
044200 IMS-REPL-WDGX9308 SECTION.                                               
044300                                                                          
044400     MOVE '  '                   TO GOOD-STATUSCODES                      
044500     CALL CBLTDLI             USING REPL                                  
044600                                    9305-PCB                              
044700                                    DLI-IO-WDGX9308                       
044800     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
044900     PERFORM IMS-STATUSCHECK                                              
045000     .                                                                    
045100     EJECT                                                                
045200 IMS-ISRT-WDGX9308 SECTION.                                               
045300                                                                          
045400     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
045500             DELIMITED BY SIZE INTO SSA1                                  
045600     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
045700             DELIMITED BY SIZE INTO SSA2                                  
045800     MOVE 'WDGX9308 '            TO SSA3                                  
045900     MOVE '  '                   TO GOOD-STATUSCODES                      
046000     CALL CBLTDLI             USING ISRT                                  
046100                                    9305-PCB                              
046200                                    DLI-IO-WDGX9308                       
046300                                    SSA1 SSA2 SSA3                        
046400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
046500     PERFORM IMS-STATUSCHECK                                              
046600     .                                                                    
046700     SKIP3                                                                
046800 IMS-GU-WDB601-LEGSEL SECTION.                                            
046900     STRING 'WDB601  (IDLEGSEL =' W-IDLEGSEL ')'                          
047000            DELIMITED BY SIZE INTO SSA1                                   
047100     MOVE '  GB'                 TO GOOD-STATUSCODES                      
047200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
047300     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
047400     PERFORM IMS-STATUSCHECK                                              
047500     .                                                                    
047600     SKIP3                                                                
047700 IMS-RESTART SECTION.                                                     
047800     SKIP2                                                                
047900     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
048000     MOVE '  '                   TO GOOD-STATUSCODES                      
048100     CALL CBLTDLI             USING XRST                                  
048200                                    MSG-PCB                               
048300                                    CHKP-MSG-IO-AREA-LENGTH               
048400                                    CHKP-MSG-IO-AREA                      
048500                                    CHKP-AREA-LENGTH                      
048600                                    CHKP-AREA                             
048700     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
048800     PERFORM IMS-STATUSCHECK                                              
048900     .                                                                    
049000     SKIP3                                                                
049100 IMS-CHECKPOINT SECTION.                                                  
049200     SKIP2                                                                
049300     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
049400     MOVE '  XD'                 TO GOOD-STATUSCODES                      
049500     CALL CBLTDLI             USING CHKP                                  
049600                                    MSG-PCB                               
049700                                    CHKP-MSG-IO-AREA-LENGTH               
049800                                    CHKP-MSG-IO-AREA                      
049900                                    CHKP-AREA-LENGTH                      
050000                                    CHKP-AREA                             
050100     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
050200     PERFORM IMS-STATUSCHECK                                              
050300                                                                          
050400     IF IMS-NOT-OK                                                        
050500       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
050600                                 TO ERROR-TEXT-STR                        
050700       DISPLAY ERROR-TEXT                                                 
050800       CALL FELLOG                                                        
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 IMS-STATUSCHECK SECTION.                                                 
051300     SKIP2                                                                
051400     SET STATUS-IX               TO 1                                     
051500     SEARCH GOOD-STATUS                                                   
051600       AT END                                                             
051700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051800             DELIMITED BY SIZE INTO ERROR-TEXT                            
051900         DISPLAY ERROR-TEXT                                               
052000         CALL FELLOG                                                      
052100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
052200         CONTINUE                                                         
052300     END-SEARCH                                                           
052400     .                                                                    
052500*    -COPY WY2000P1                                                       
052600     EJECT                                                                
