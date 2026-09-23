000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4769600.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/02/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        IT CREATES THE FILE FOR IDSHIPM FOR WHICH WDE201-KDFINDOC        
000900*        IS = 'INV' OR 'INT' AND TISKEPPN IS EXCEEDED BY 14 DAYS.         
000910*        'PROF' IS DELETED WITHOUT DAY-CONTROL                            
001000*        THESE IDSHIPM NEEDS TO BE DELETED.                               
001100*        HERE FILE IS CREATED FOR THE SAME.                               
001200*        PROGRAM W4769700 READS THIS FILE AND DELETES THE                 
001300*        IDSHIPM.                                                         
001400*                                                                         
001500*        THE PROGRAM READS     WDE2                                       
001600*                              WDB9                                       
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
003000*          --- OUTPUT FILE FOR WDE201-KDKLAR = 'J'                        
003100     SELECT W47696                     ASSIGN TO W47696D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W47696                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W4769001 -PRE  UT1-  -L.                                  
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W4769600'.            
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  RENSA-SW                    PIC X(1)    VALUE 'N'.                   
005000     88 RENSA                                VALUE 'J'.                   
005100                                                                          
005410 01  WS-IDSHIPM                  PIC 9(7)    VALUE ZERO.                  
005430                                                                          
005500 01  WS-DAGAR                    PIC S9(3)   VALUE ZERO    COMP-3.        
005600 01  WS-IDDISTR                  PIC S9(5)   VALUE ZERO    COMP-3.        
005800                                                                          
005900     EJECT                                                                
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
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FOUND                       VALUE '  '.                  
012400     88  SEGMENT-MISSING                     VALUE 'GB'.                  
012500     SKIP2                                                                
012600 01  GOOD-STATUSCODES.                                                    
012700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(120).                              
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNCTION CODES                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
013700 01  DLI-IO-AREA.                                                         
013800     03  FILLER                  PIC X(100)  VALUE SPACE.                 
013900                                                                          
014000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE201'.         
014100 01  DLI-IO-WDE201.                                                       
014200*    03  -COPY WDE201                                                     
014300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE211'.         
014400 01  DLI-IO-WDE211.                                                       
014500*    03  -COPY WDE211                                                     
014600*                                                                         
014700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB9A1'.         
014800 01  DLI-IO-WDB9A1.                                                       
014900*    03  -COPY WDB9A1                                                     
015000     EJECT                                                                
015100                                                                          
015200 LINKAGE SECTION.                                                         
015300                                                                          
015400*01  -COPY W0008  -PRE WDE2-                                              
015500     05  FILLER                  PIC X.                                   
015600                                                                          
015700*01  -COPY W0008  -PRE WDB9A-                                             
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000                                                                          
016100 PROCEDURE DIVISION  USING WDE2-PCB WDB9A-PCB.                            
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING WDE2-PCB WDB9A-PCB.                            
016400                                                                          
016500     PERFORM A-INIT                                                       
016600                                                                          
016700     PERFORM IMS-GN-WDE2                                                  
016800     PERFORM UNTIL SEGMENT-MISSING                                        
016900       EVALUATE WDE2-SEG-NAME-FB                                          
017000         WHEN 'WDE201'                                                    
017010           IF RENSA                                                       
017030             MOVE WS-IDSHIPM          TO UT1-DEL-IDSHIPM                  
017040             PERFORM S11-WRITE-W47696                                     
017060           END-IF                                                         
017090                                                                          
017100           MOVE DLI-IO-AREA            TO BILL-WDE201                     
017110           MOVE BILL-IDSHIPM           TO WS-IDSHIPM                      
017200           IF BILL-KDFINDOC = 'INV' OR 'INT' OR 'PROF'                    
017300             MOVE YES                  TO RENSA-SW                        
017400           END-IF                                                         
017500         WHEN 'WDE211'                                                    
017600           IF BILL-KDFINDOC = 'INV' OR 'INT'                              
017610             IF RENSA                                                     
017700               MOVE DLI-IO-AREA        TO BGMT-WDE211                     
017800               PERFORM B-KOLLA-RENSNING                                   
017900             END-IF                                                       
018400           END-IF                                                         
018500       END-EVALUATE                                                       
018600       PERFORM IMS-GN-WDE2                                                
018700     END-PERFORM                                                          
018710     IF RENSA                                                             
018730       MOVE WS-IDSHIPM      TO UT1-DEL-IDSHIPM                            
018740       PERFORM S11-WRITE-W47696                                           
018760     END-IF                                                               
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     OPEN OUTPUT W47696                                                   
019700                                                                          
019800     ACCEPT TODAYS-DATE  FROM DATE                                        
019900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-RENSNING  SECTION.                                               
020300                                                                          
020500     MOVE 002                    TO DAG-KDCALL                            
020600     MOVE BILL-TISKEPPN          TO DAG-TIAAMMDD-FOM                      
020700     MOVE 20                     TO DAG-TISEKEL-FOM                       
020800     MOVE 14                     TO DAG-KVKALDAG                          
020900     PERFORM BA-HITTA-DAGAR-PER-DISTR                                     
021000     CALL WDAGKONV  USING  DAG-KDCALL,                                    
021100                           DAG-DATUM-AREA,                                
021200                           DAG-KDSVAR                                     
021300     IF DAG-KDSVAR = SPACE                                                
021400       IF DAG-TIAAMMDD-TOM  < TODAYS-DATE                                 
021500         CONTINUE                                                         
021510       ELSE                                                               
021600         MOVE NOO                  TO RENSA-SW                            
021700       END-IF                                                             
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 BA-HITTA-DAGAR-PER-DISTR   SECTION.                                      
022200                                                                          
022610                                                                          
022700     IF  BGMT-IDDISTR = WS-IDDISTR                                        
022800       CONTINUE                                                           
022900     ELSE                                                                 
023000       MOVE BGMT-IDDISTR            TO W-IDDISTR-A1-MIN                   
023100                                       W-IDDISTR-A1-MAX                   
023200       PERFORM IMS-GU-WDB9A1                                              
023300       IF SEGMENT-FOUND                                                   
023400         MOVE SEQA-KVDAGAR          TO WS-DAGAR                           
023700       ELSE                                                               
023800         MOVE ZERO                  TO WS-DAGAR                           
023900       END-IF                                                             
024000       MOVE BGMT-IDDISTR            TO WS-IDDISTR                         
024100     END-IF                                                               
024200                                                                          
024300     IF WS-DAGAR > ZERO                                                   
024400       MOVE WS-DAGAR                TO DAG-KVKALDAG                       
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 Z-FINIT SECTION.                                                         
024900     CLOSE W47696                                                         
025000     SKIP2                                                                
025100     MOVE 'S' TO POSTSUM-OPKOD                                            
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 S11-WRITE-W47696 SECTION.                                                
025600                                                                          
025700     WRITE UT1-POST FROM UT1-AREA                                         
025800                                                                          
025900     MOVE SPACE         TO POSTSUM-TRANSTYP                               
026000     MOVE 'W47696'      TO POSTSUM-FDNAMN                                 
026100     MOVE 'W47696D1'    TO POSTSUM-DDNAMN2                                
026200     CALL POSTSUM USING POSTSUM-PARM                                      
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600* --- IMS SECTIONS  ---                                                   
026700                                                                          
026800 IMS-GN-WDE2   SECTION.                                                   
026900                                                                          
027000     CALL CBLTDLI USING GN WDE2-PCB DLI-IO-AREA                           
027100     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
027200     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
027300     PERFORM IMS-STATUSCHECK                                              
027400     .                                                                    
027500     EJECT                                                                
027600 IMS-GU-WDB9A1      SECTION.                                              
027700     STRING 'WDB9A1  (WDB9A1KY>=' W-WDB9A1KY-MIN                          
027800                    '&WDB9A1KY<=' W-WDB9A1KY-MAX ')'                      
027900               DELIMITED BY SIZE INTO SSA1                                
028000     MOVE '  GEGB'                 TO GOOD-STATUSCODES                    
028100     CALL CBLTDLI USING GU  WDB9A-PCB DLI-IO-WDB9A1 SSA1                  
028200     MOVE WDB9A-STATUS-CODE         TO STATUS-WS                          
028300     PERFORM IMS-STATUSCHECK                                              
028400     .                                                                    
028500     EJECT                                                                
028600 IMS-STATUSCHECK SECTION.                                                 
028700                                                                          
028800     SET STATUS-IX TO 1                                                   
028900     SEARCH GOOD-STATUS                                                   
029000       AT END                                                             
029100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029200           DELIMITED BY SIZE INTO ERRTEXT                                 
029300         DISPLAY ERRTEXT                                                  
029400         CALL FELLOG                                                      
029500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029600         CONTINUE                                                         
029700     END-SEARCH                                                           
029800     .                                                                    
