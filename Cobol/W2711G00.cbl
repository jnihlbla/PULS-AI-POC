000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2711G00.                                                
000300 AUTHOR.         CHESTER COUCH.                                           
000400 DATE-WRITTEN.   21/10/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        USING THE DC AND STATUS FROM THE SYSIN AS SELECTION              
000900*        CRITERIA, READ THE REFILL PROPOSAL DATABASE AND CREATE           
001000*        AN OUTPUT FILE CONTAINING ALL THE PROPOSALS THAT MATCH           
001100*        THE SELECTION CRITERIA. IF SELECTION CRITERIA SPECIFIES          
001200*        APPROVED PROPOSALS (I.E. KDREFORS='O'), THEN THE SEGMENT         
001300*        KEYS WILL BE WRITTEN TO FILE W27113. THESE SEGEMENTS WILL        
001400*        BE DELETED IN A SUBSEQUENT JOB/PROGRAM.                          
001500*                                                                         
001600*        THE PROGRAM READS     WDE3                                       
001700*                                                                         
001800*    ABENDCODES:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- PROPOSAL EXTRACT FILE                                      
003100     SELECT W2711G                     ASSIGN TO W2711GD1.                
003200     SKIP2                                                                
003300*          --- PROPOSAL KEYS TO BE DELETED                                
003400     SELECT W27113                     ASSIGN TO W2711GD2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W2711G                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  RECORD -COPY W2711G -PRE  OUT-  -L.                                  
004500     EJECT                                                                
004600 FD  W27113                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  RECORD -COPY W27113 -PRE  OUTD-  -L.                                 
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W2711G00'.            
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005700     EJECT                                                                
005800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES TODAYS-DATE.                                        
006000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006200     03  TODAYS-DATE-DAY         PIC 9(2).                                
006300     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     SKIP2                                                                
007100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007200                                                                          
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007600     SKIP2                                                                
007700 01  ERROR-TEXT.                                                          
007800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT:'.            
007900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  PARM-SYSIN.                                                          
008600     03  PARM-IDDC            PIC X(2)  VALUE SPACE.                      
008700     03  PARM-KDREFORS        PIC X(1)  VALUE SPACE.                      
008800     03  PARM-IDMAIL          PIC X(60) VALUE SPACE.                      
008900     03  FILLER               PIC X(17).                                  
009000     EJECT                                                                
009100 01  OUT-AREA-START              PIC X(24)   VALUE                        
009200                                 'OUT-AREA-START  '.                      
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W2711G     -PRE OUT-.                                     
009600     EJECT                                                                
009700*    --- AREAS FOR IMS-SECTIONS                                           
009800*                                                                         
009900     EJECT                                                                
010000 01  OUTD-AREA-START              PIC X(24)   VALUE                       
010100                                 'OUTD-AREA-START  '.                     
010200     SKIP2                                                                
010300                                                                          
010400*01  AREA -COPY W27113     -PRE OUTD-.                                    
010500     EJECT                                                                
010600*    --- AREAS FOR IMS-SECTIONS                                           
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  KEYS-FOR-DLI.                                                        
011200     03 W-WDE301KY-MIN-X.                                                 
011300         05  W-IDDC-301-MIN      PIC X(2)  VALUE SPACE.                   
011400         05  W-IDPERSON-BUY-301-MIN                                       
011500                                 PIC S9(3) VALUE ZERO COMP-3.             
011600         05  W-KDREFTYP-301-MIN  PIC X     VALUE SPACE.                   
011700         05  W-IDARTNR-301-MIN   PIC S9(9) VALUE ZERO COMP-3.             
011800         05  W-IDDISTR-301-MIN   PIC S9(5) VALUE ZERO COMP-3.             
011900                                                                          
012000     03 W-WDE301KY-MAX-X.                                                 
012100         05  W-IDDC-301-MAX      PIC X(2)  VALUE HIGH-VALUE.              
012200         05  W-IDPERSON-BUY-301-MAX                                       
012300                                 PIC S9(3) VALUE +999 COMP-3.             
012400         05  W-KDREFTYP-301-MAX  PIC X     VALUE HIGH-VALUE.              
012500         05  W-IDARTNR-301-MAX   PIC S9(9)                                
012600                                         VALUE +999999999 COMP-3.         
012700         05  W-IDDISTR-301-MAX   PIC S9(5) VALUE +99999 COMP-3.           
012800     SKIP2                                                                
012900     03  W-WDGXKEY-2261-X.                                                
013000          05 W-IDHTYP            PIC X(4)    VALUE '2261'.                
013100          05 W-IDDC-2261         PIC X(2)    VALUE SPACE.                 
013200          05 FILLER              PIC X(24)   VALUE LOW-VALUE.             
013210     03  W-IDARTNR-2262-X.                                                
013220         05  W-IDARTNR-2262      PIC S9(9)   VALUE ZERO COMP-3.           
013300     SKIP2                                                                
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FOUND                       VALUE '  '.                  
013700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013800     SKIP2                                                                
013900 01  GOOD-STATUSCODES.                                                    
014000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01  SSA1                        PIC X(64).                               
014300 01  SSA2                        PIC X(64).                               
014400     EJECT                                                                
014500*    --- IMS FUNCTION CODES                                               
014600*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE301'.                      
015000 01  DLI-IO-WDE301.                                                       
015100*    03  -COPY WDE301                                                     
015200     EJECT                                                                
015300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDGX2262'.                    
015310 01  DLI-IO-WDGX2262.                                                     
015320*    03  -COPY WDGX2262                                                   
015330     EJECT                                                                
015340                                                                          
015400 LINKAGE SECTION.                                                         
015500                                                                          
015600                                                                          
015700*01  -COPY W0008  -PRE WDE3-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
015910*01  -COPY W0008  -PRE 2261-                                              
015920     05  FILLER                  PIC X.                                   
015930     EJECT                                                                
016000 PROCEDURE DIVISION  USING WDE3-PCB 2261-PCB.                             
016100 MAIN SECTION.                                                            
016200     ENTRY 'DLITCBL' USING WDE3-PCB 2261-PCB.                             
016300                                                                          
016400                                                                          
016500     PERFORM A-INIT                                                       
016600                                                                          
016700     MOVE PARM-IDDC              TO W-IDDC-301-MIN                        
016800                                    W-IDDC-301-MAX                        
016900     PERFORM IMS-GN-WDE3                                                  
017000     PERFORM UNTIL SEGMENT-MISSING                                        
017100       EVALUATE WDE3-SEG-NAME-FB                                          
017200         WHEN 'WDE301'                                                    
017210           IF  REF-KDREFORS  = PARM-KDREFORS                              
017220           AND (REF-KDREFTYP = 'A' OR 'B' OR 'C')                         
017300             PERFORM B-PREPARE-OUTPUT-FILES                               
017400             PERFORM S11-WRITE-W2711G                                     
017500             IF PARM-KDREFORS = 'O'                                       
017600               PERFORM S12-WRITE-W27113                                   
017700             END-IF                                                       
017710           END-IF                                                         
017800       END-EVALUATE                                                       
017900       PERFORM IMS-GN-WDE3                                                
018000     END-PERFORM                                                          
018100     PERFORM Z-FINIT                                                      
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800                                                                          
018900     OPEN OUTPUT W2711G                                                   
019000                 W27113                                                   
019100                                                                          
019200     ACCEPT TODAYS-DATE  FROM DATE                                        
019300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019400                                                                          
019500     ACCEPT PARM-SYSIN FROM SYSIN                                         
019600     UNSTRING PARM-SYSIN DELIMITED BY ','                                 
019700       INTO PARM-IDDC                                                     
019800            PARM-KDREFORS                                                 
019900            PARM-IDMAIL                                                   
020000                                                                          
020100     DISPLAY 'PARM-IDDC     : ' PARM-IDDC                                 
020200     DISPLAY 'PARM-KDREFORS : ' PARM-KDREFORS                             
020300     DISPLAY 'PARM-IDMAIL   : ' PARM-IDMAIL                               
020400     .                                                                    
020500     EJECT                                                                
020600 B-PREPARE-OUTPUT-FILES SECTION.                                          
020700     MOVE REF-IDDC          TO OUT-IDDC                                   
020800     MOVE REF-IDPERSON-BUY  TO OUT-IDPERSON-BUY                           
020900     MOVE REF-KDREFTYP      TO OUT-KDREFTYP                               
021000     MOVE REF-IDARTNR       TO OUT-IDARTNR                                
021100     MOVE REF-KDREFORS      TO OUT-KDREFORS                               
021200     MOVE REF-KVBEART       TO OUT-KVBEART                                
021210                                                                          
021320     MOVE REF-IDDC          TO W-IDDC-2261                                
021321     MOVE REF-IDARTNR       TO W-IDARTNR-2262                             
021330     PERFORM IMS-GHU-WDGX2262                                             
021340     IF SEGMENT-FOUND                                                     
021350       MOVE 2262-TEREFMED   TO OUT-TEREFMED                               
021390     ELSE                                                                 
021391       MOVE SPACE           TO OUT-TEREFMED                               
021394     END-IF                                                               
021400                                                                          
021500     MOVE REF-IDDC          TO OUTD-IDDC                                  
021600     MOVE REF-IDPERSON-BUY  TO OUTD-IDPERSON-BUY                          
021700     MOVE REF-KDREFTYP      TO OUTD-KDREFTYP                              
021800     MOVE REF-IDARTNR       TO OUTD-IDARTNR                               
021900     MOVE REF-IDDISTR       TO OUTD-IDDISTR                               
021910     MOVE NOO               TO OUTD-FLREFNYO                              
022000     .                                                                    
022100     EJECT                                                                
022200 Z-FINIT SECTION.                                                         
022300     CLOSE W2711G                                                         
022400           W27113                                                         
022500     SKIP2                                                                
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900     EJECT                                                                
023000 S11-WRITE-W2711G SECTION.                                                
023100                                                                          
023200     WRITE OUT-RECORD  FROM OUT-AREA                                      
023300                                                                          
023400     MOVE 'LST'      TO POSTSUM-TRANSTYP                                  
023500     MOVE 'W2711G' TO POSTSUM-FDNAMN                                      
023600     MOVE 'W2711GD1' TO POSTSUM-DDNAMN2                                   
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023800     .                                                                    
023900     EJECT                                                                
024000 S12-WRITE-W27113 SECTION.                                                
024100                                                                          
024200     WRITE OUTD-RECORD FROM OUTD-AREA                                     
024300                                                                          
024400     MOVE 'DEL'      TO POSTSUM-TRANSTYP                                  
024500     MOVE 'W27113' TO POSTSUM-FDNAMN                                      
024600     MOVE 'W2711GD2' TO POSTSUM-DDNAMN2                                   
024700     CALL POSTSUM USING POSTSUM-PARM                                      
024800     .                                                                    
024900     EJECT                                                                
025000 S99-ABEND SECTION.                                                       
025100                                                                          
025200     SKIP2                                                                
025300     MOVE 'S' TO POSTSUM-OPKOD                                            
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     CALL ABEND USING RKOD-ABEND                                          
025600     .                                                                    
025700     EJECT                                                                
025800* --- IMS SECTIONS  ---                                                   
025900                                                                          
026000                                                                          
026100 IMS-GN-WDE3   SECTION.                                                   
026200                                                                          
026300     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
026400                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
026500          DELIMITED BY SIZE INTO SSA1                                     
026600     MOVE '  GE' TO GOOD-STATUSCODES                                      
026700**** MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
026800     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-WDE301 SSA1                    
026900     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
027000     PERFORM IMS-STATUSCHECK                                              
027100     .                                                                    
027200     EJECT                                                                
027210 IMS-GHU-WDGX2262 SECTION.                                                
027220                                                                          
027230     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2261-X ')'                    
027240          DELIMITED BY SIZE INTO SSA1                                     
027250     STRING 'WDGX2262(IDARTNR  =' W-IDARTNR-2262-X ')'                    
027260          DELIMITED BY SIZE INTO SSA2                                     
027261     MOVE '  GE' TO GOOD-STATUSCODES                                      
027280     CALL CBLTDLI USING GHU 2261-PCB DLI-IO-WDGX2262 SSA1 SSA2            
027290     MOVE 2261-STATUS-CODE TO STATUS-WS                                   
027292     PERFORM IMS-STATUSCHECK                                              
027294     .                                                                    
027295     SKIP3                                                                
027300 IMS-STATUSCHECK SECTION.                                                 
027400                                                                          
027500     SET STATUS-IX TO 1                                                   
027600     SEARCH GOOD-STATUS                                                   
027700       AT END                                                             
027800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
027900           DELIMITED BY SIZE INTO ERROR-TEXT                              
028000         DISPLAY ERROR-TEXT                                               
028100         CALL FELLOG                                                      
028200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
028300         CONTINUE                                                         
028400     END-SEARCH                                                           
028500     .                                                                    
