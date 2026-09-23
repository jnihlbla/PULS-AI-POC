000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4769400.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/02/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        IT CREATES THE FILE FOR IDSHIPM FOR WHICH WDE101-KDKLAR          
000900*        IS = 'J' OR 'B' AND TISKEPPN IS EXCEEDED BY 14 DAYS.             
001000*        IF 'B' DELETE WITHOUT DAY-CONTROL                                
001010*        IF 'A' DELETE WITHOUT DAY-CONTROL                                
001020*        IF 'L' DELETE WITHOUT DAY-CONTROL                                
001100*        THESE IDSHIPM NEEDS TO BE DELETED.                               
001200*        HERE FILE IS CREATED FOR THE SAME.                               
001300*        PROGRAM W4769500 READS THIS FILE AND DELETES THE                 
001400*        IDSHIPM.                                                         
001500*                                                                         
001600*        THE PROGRAM READS     WDE1                                       
001700*                              WDB9                                       
001800*                                                                         
001900*    ABENDCODES:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- OUTPUT FILE FOR WDE101-KDKLAR = 'J'                        
003200     SELECT W47694                     ASSIGN TO W47694D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W47694                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W4769001 -PRE  UT1-  -L.                                  
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W4769400'.            
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  RENSA-SW                    PIC X(1)    VALUE 'N'.                   
005100     88 RENSA                                VALUE 'J'.                   
005200                                                                          
005510 01  WS-IDSHIPM                  PIC 9(7)    VALUE ZERO.                  
005520                                                                          
005600 01  WS-DAGAR                    PIC S9(3)   VALUE ZERO    COMP-3.        
005700 01  WS-IDDISTR                  PIC S9(5)   VALUE ZERO    COMP-3.        
005900                                                                          
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007400     SKIP2                                                                
007500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  ERRTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008300     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'DAGKONV-PARM'.        
009000*                                                                         
009100*01  -COPY WDAGAREA                                                       
009200                                                                          
009300 01  UT1-AREA-START              PIC X(24)   VALUE                        
009400                                 'UT1-AREA-START  '.                      
009500     SKIP2                                                                
009600                                                                          
009700*01  AREA -COPY W4769001     -PRE UT1-                                    
009800     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-TILL-DLI.                                                       
010500     03  W-IDSHIPM-X.                                                     
010600         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
010700                                                                          
010800     03  W-WDB9A1KY-MIN.                                                  
010900         05  W-IDDISTR-A1-MIN    PIC S9(5)    VALUE ZERO  COMP-3.         
011000         05  W-IDDC-A1-MIN       PIC X(2)     VALUE LOW-VALUE.            
011100         05  W-IDKUND-A1-MIN     PIC X(10)    VALUE LOW-VALUE.            
011200         05  W-IDDC-REC-A1-MIN   PIC X(2)     VALUE LOW-VALUE.            
011300                                                                          
011400     03  W-WDB9A1KY-MAX.                                                  
011500         05  W-IDDISTR-A1-MAX    PIC S9(5)    VALUE +99999 COMP-3.        
011600         05  W-IDDC-A1-MAX       PIC X(2)     VALUE HIGH-VALUE.           
011700         05  W-IDKUND-A1-MAX     PIC X(10)    VALUE HIGH-VALUE.           
011800         05  W-IDDC-REC-A1-MAX   PIC X(2)     VALUE HIGH-VALUE.           
011900                                                                          
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-MISSING                     VALUE 'GB'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(120).                              
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
013600 01  DLI-IO-AREA.                                                         
013700     03  FILLER                  PIC X(100) VALUE SPACE.                  
013800                                                                          
013900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE101'.         
014000 01  DLI-IO-WDE101.                                                       
014100*    03  -COPY WDE101                                                     
014200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE111'.         
014300 01  DLI-IO-WDE111.                                                       
014400*    03  -COPY WDE111                                                     
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB9A1'.         
014700 01  DLI-IO-WDB9A1.                                                       
014800*    03  -COPY WDB9A1                                                     
014900                                                                          
015000 LINKAGE SECTION.                                                         
015100                                                                          
015200*01  -COPY W0008  -PRE WDE1-                                              
015300     05  FILLER                  PIC X.                                   
015400*01  -COPY W0008  -PRE WDB9A-                                             
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700                                                                          
015800 PROCEDURE DIVISION  USING WDE1-PCB WDB9A-PCB.                            
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING WDE1-PCB WDB9A-PCB.                            
016100                                                                          
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     PERFORM IMS-GN-WDE1                                                  
016500     PERFORM UNTIL SEGMENT-MISSING                                        
016600       EVALUATE WDE1-SEG-NAME-FB                                          
016700         WHEN 'WDE101'                                                    
016710           IF RENSA                                                       
016712               MOVE WS-IDSHIPM    TO UT1-DEL-IDSHIPM                      
016713               PERFORM S11-WRITE-W47694                                   
016720           END-IF                                                         
016800           MOVE DLI-IO-AREA       TO SHIP-WDE101                          
016810           MOVE SHIP-IDSHIPM      TO WS-IDSHIPM                           
016900           IF SHIP-KDKLAR = 'J' OR 'B' OR 'A' OR 'L'                      
017000             MOVE YES             TO RENSA-SW                             
017010           ELSE                                                           
017020             MOVE NOO             TO RENSA-SW                             
017100           END-IF                                                         
017200         WHEN 'WDE111'                                                    
017300           IF SHIP-KDKLAR = 'J'                                           
017310             IF RENSA                                                     
017400               MOVE DLI-IO-AREA     TO SGMT-WDE111                        
017500               PERFORM B-KOLLA-RENSNING                                   
017600             END-IF                                                       
018100           END-IF                                                         
018200                                                                          
018300       END-EVALUATE                                                       
018400       PERFORM IMS-GN-WDE1                                                
018500     END-PERFORM                                                          
018510     IF RENSA                                                             
018520       MOVE WS-IDSHIPM  TO UT1-DEL-IDSHIPM                                
018530       PERFORM S11-WRITE-W47694                                           
018540     END-IF                                                               
018600     PERFORM Z-FINIT                                                      
018700                                                                          
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019300                                                                          
019400     OPEN OUTPUT W47694                                                   
019500                                                                          
019600     ACCEPT TODAYS-DATE  FROM DATE                                        
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-RENSNING  SECTION.                                               
020700                                                                          
021200     MOVE 002                    TO DAG-KDCALL                            
021300     MOVE SHIP-TISKEPPN          TO DAG-TIAAMMDD-FOM                      
021400     MOVE 20                     TO DAG-TISEKEL-FOM                       
021500     MOVE 14                     TO DAG-KVKALDAG                          
021600     PERFORM BA-HITTA-DAGAR-PER-DISTR                                     
021700     CALL WDAGKONV  USING  DAG-KDCALL,                                    
021800                           DAG-DATUM-AREA,                                
021900                           DAG-KDSVAR                                     
022000     IF DAG-KDSVAR = SPACE                                                
022110       IF DAG-TIAAMMDD-TOM  < TODAYS-DATE                                 
022200         CONTINUE                                                         
022210       ELSE                                                               
022220         MOVE NOO                TO RENSA-SW                              
022300       END-IF                                                             
022400     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 BA-HITTA-DAGAR-PER-DISTR   SECTION.                                      
022900                                                                          
023000     IF  SGMT-IDDISTR = WS-IDDISTR                                        
023100       CONTINUE                                                           
023200     ELSE                                                                 
023300       MOVE SGMT-IDDISTR            TO W-IDDISTR-A1-MIN                   
023400                                       W-IDDISTR-A1-MAX                   
023500       PERFORM IMS-GU-WDB9A1                                              
023600       IF SEGMENT-FOUND                                                   
023700         MOVE SEQA-KVDAGAR          TO WS-DAGAR                           
023800       ELSE                                                               
023900         MOVE ZERO                  TO WS-DAGAR                           
024000       END-IF                                                             
024100       MOVE SGMT-IDDISTR            TO WS-IDDISTR                         
024200     END-IF                                                               
024300                                                                          
024400     IF WS-DAGAR > ZERO                                                   
024500       MOVE WS-DAGAR                TO DAG-KVKALDAG                       
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 Z-FINIT SECTION.                                                         
025000     CLOSE W47694                                                         
025100     SKIP2                                                                
025200     MOVE 'S' TO POSTSUM-OPKOD                                            
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600 S11-WRITE-W47694 SECTION.                                                
025700                                                                          
025800     WRITE UT1-POST FROM UT1-AREA                                         
025900                                                                          
026000     MOVE SPACE         TO POSTSUM-TRANSTYP                               
026100     MOVE 'W47694'      TO POSTSUM-FDNAMN                                 
026200     MOVE 'W47694D1'    TO POSTSUM-DDNAMN2                                
026300     CALL POSTSUM USING POSTSUM-PARM                                      
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700* --- IMS SECTIONS  ---                                                   
026800                                                                          
026900 IMS-GN-WDE1   SECTION.                                                   
027000                                                                          
027100     CALL CBLTDLI USING GN WDE1-PCB DLI-IO-AREA                           
027200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
027300     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
027400     PERFORM IMS-STATUSCHECK                                              
027500     .                                                                    
027600     EJECT                                                                
027700 IMS-GU-WDB9A1      SECTION.                                              
027800     STRING 'WDB9A1  (WDB9A1KY>=' W-WDB9A1KY-MIN                          
027900                    '&WDB9A1KY<=' W-WDB9A1KY-MAX ')'                      
028000               DELIMITED BY SIZE INTO SSA1                                
028100     MOVE '  GE'                   TO GOOD-STATUSCODES                    
028200     CALL CBLTDLI USING GU  WDB9A-PCB DLI-IO-WDB9A1 SSA1                  
028300     MOVE WDB9A-STATUS-CODE         TO STATUS-WS                          
028400     PERFORM IMS-STATUSCHECK                                              
028500     .                                                                    
028600     EJECT                                                                
028700 IMS-STATUSCHECK SECTION.                                                 
028800                                                                          
028900     SET STATUS-IX TO 1                                                   
029000     SEARCH GOOD-STATUS                                                   
029100       AT END                                                             
029200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029300           DELIMITED BY SIZE INTO ERRTEXT                                 
029400         DISPLAY ERRTEXT                                                  
029500         CALL FELLOG                                                      
029600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029700         CONTINUE                                                         
029800     END-SEARCH                                                           
029900     .                                                                    
