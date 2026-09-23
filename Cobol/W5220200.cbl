000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     W5220100.                                                
000600 AUTHOR.         NILSSON LINDA.                                           
000700 DATE-WRITTEN.   02/05/06.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        SELECTS DATA FROM DB2-TABLE T01IVW AND                           
001300*        CREATES A SEQUENCE-FILE FOR V.A.T.                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W52202T                                             
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- SEQUENCEFILE V.A.T.                                        
002700     SELECT W52202                     ASSIGN TO W52202D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W52202                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST    -COPY W522VAT    -L.                                         
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W5220200'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
004300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200     EJECT                                                                
005300                                                                          
005400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES DAGENS-DATUM.                                       
005600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005900     EJECT                                                                
006000                                                                          
006100*    --- WS-AREA FOR SEQUENCEFILE                                         
006200 01  WS-DAREGDAT                 PIC X(8)    VALUE SPACE.                 
006300 01  WS-TIREGTID                 PIC S9(7)   VALUE ZERO COMP-3.           
006400 01  WS-IDLOPNR                  PIC S9(5)   VALUE ZERO COMP-3.           
006500 01  WS-IDPTYP                   PIC X(3)    VALUE 'VAT'.                 
006600 01  WS-TIRP                     PIC S9(2)   VALUE ZERO COMP-3.           
006700 01  WS-TIRP-DISPLAY             PIC  9(2).                               
006800 01  WS-FLKLAR                   PIC X       VALUE SPACE.                 
006900 01  WS-IV-DATA                  PIC X(200)  VALUE SPACE.                 
007000*                                                                         
007100 01  WS-IDPTYP-HEAD              PIC X(3)    VALUE '2  '.                 
007200*                                                                         
007300 01  WS-BEFORMS                  PIC X(15)   VALUE                        
007400                                            'VAT            '.            
007500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008000     SKIP3                                                                
008100*    --- PARAMETERS TO ABEND                                              
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
008700     SKIP3                                                                
008800 01  MESSAGE-CODES.                                                       
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009000     EJECT                                                                
009100                                                                          
009200*    --- PARAMETRAR TILL DATKORT                                          
009300*                                                                         
009400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W52202'.              
009500                                                                          
009600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009700                                                                          
009800*01  -COPY WDATKORT                                                       
009900     EJECT                                                                
010000                                                                          
010100*01  -COPY WDATAREA                                                       
010200     EJECT                                                                
010300                                                                          
010400 01  VAT-AREA                    PIC X(24)   VALUE 'VAT-AREA'.            
010500                                                                          
010600*01  -COPY W522VAT -PRE VAT-                                              
010700     EJECT                                                                
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
011000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011100                                                                          
011200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011300 01  DB2-WS.                                                              
011400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011500         88  CURSOR-OK                       VALUE 000.                   
011600         88  LINES-FOUND                     VALUE 000.                   
011700         88  LINES-MISSING                   VALUE 100.                   
011800         88  DOUBLE-LINES                    VALUE 811.                   
011900         88  RESOURCE-WRONG                  VALUE 904.                   
012000     03  GOOD-SQLCODECODES.                                               
012100         05  GOOD-SQLCODE OCCURS 5                                        
012200             INDEXED BY SQLCODE-IX PIC 9(3).                              
012300     EJECT                                                                
012400                                                                          
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
012700                                                                          
012800*01  -COPY T01IVW   -PRE T01IVW-                                          
012900     EJECT                                                                
013000     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
013100     EJECT                                                                
013200                                                                          
013300 LINKAGE SECTION.                                                         
013400 PROCEDURE DIVISION.                                                      
013500 MAIN SECTION.                                                            
013600                                                                          
013700     PERFORM A-INIT                                                       
013800     PERFORM B-BEHANDLA-RADER                                             
013900     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     OPEN OUTPUT W52202                                                   
014700                                                                          
014800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014900     MOVE D-AAR           TO DAGENS-DATUM-AAR                             
015000     MOVE D-MAANAD        TO DAGENS-DATUM-MAANAD                          
015100     MOVE D-DAG           TO DAGENS-DATUM-DAG                             
015200                                                                          
015300     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
015400     MOVE DAGENS-DATUM    TO DAT-I-TIDATUM                                
015500     CALL WDATKONV USING                                                  
015600          DAT-KDDATFORM                                                   
015700          DAT-I-TIDATUM                                                   
015800          DAT-O-TIDATUM                                                   
015900          DAT-KDSVAR                                                      
016000     MOVE DAT-TIMM        TO WS-TIRP                                      
016100                             WS-TIRP-DISPLAY                              
016200     DISPLAY 'PERIOD='       WS-TIRP-DISPLAY                              
016300                                                                          
016400     INITIALIZE GOOD-SQLCODECODES                                         
016500     .                                                                    
016600     EJECT                                                                
016700 B-BEHANDLA-RADER SECTION.                                                
016800                                                                          
016900     PERFORM DB2-DCL-OPEN-CRS                                             
017000     PERFORM DB2-FETCH-CRS                                                
017100     PERFORM UNTIL LINES-MISSING                                          
017200       MOVE T01IVW-IV-DATA TO VAT-W522VAT                                 
017300       IF VAT-IDDISTR = '00071'                                           
017400                     OR '00081'                                           
017500                     OR '00082'                                           
017600                     OR '00090'                                           
017700         CONTINUE                                                         
017800       ELSE                                                               
017900         PERFORM S11-WRITE-W52202                                         
018000         PERFORM DB2-UPDATE-T01IVW                                        
018100       END-IF                                                             
018200       PERFORM DB2-FETCH-CRS                                              
018300     END-PERFORM                                                          
018400     PERFORM DB2-CLOSE-CRS                                                
018500     .                                                                    
018600     EJECT                                                                
018700 Z-FINIT SECTION.                                                         
018800                                                                          
018900     CLOSE W52202                                                         
019000     SKIP2                                                                
019100     .                                                                    
019200     EJECT                                                                
019300 S11-WRITE-W52202 SECTION.                                                
019400     SKIP2                                                                
019500     WRITE POST FROM VAT-W522VAT                                          
019600     .                                                                    
019700     EJECT                                                                
019800 DB2-DCL-OPEN-CRS SECTION.                                                
019900                                                                          
020000     MOVE 000100 TO GOOD-SQLCODECODES                                     
020100                                                                          
020200     EXEC SQL DECLARE T01IVW-CRS CURSOR WITH HOLD FOR                     
020300         SELECT DAREGDAT                                                  
020400              , TIREGTID                                                  
020500              , IDLOPNR                                                   
020600              , IDPTYP                                                    
020700              , TIRP                                                      
020800              , FLKLAR                                                    
020900              , IV_DATA                                                   
021000              , IV_DATA2                                                  
021100                                                                          
021200         FROM T01IVW                                                      
021300                                                                          
021400         WHERE TIRP   = :WS-TIRP                                          
021500           AND IDPTYP = :WS-IDPTYP                                        
021600           AND FLKLAR = 'N'                                               
021700                                                                          
021800         FOR UPDATE OF FLKLAR                                             
021900                                                                          
022000     END-EXEC                                                             
022100                                                                          
022200     MOVE 000100 TO GOOD-SQLCODECODES                                     
022300                                                                          
022400     EXEC SQL                                                             
022500        OPEN T01IVW-CRS                                                   
022600     END-EXEC                                                             
022700                                                                          
022800     MOVE SQLCODE TO SQLCODE-WS                                           
022900     PERFORM DB2-STATUS-CHECK                                             
023000     .                                                                    
023100     EJECT                                                                
023200 DB2-FETCH-CRS SECTION.                                                   
023300                                                                          
023400     MOVE 000100  TO GOOD-SQLCODECODES                                    
023500     EXEC SQL                                                             
023600         FETCH T01IVW-CRS                                                 
023700         INTO   :T01IVW-DAREGDAT                                          
023800              , :T01IVW-TIREGTID                                          
023900              , :T01IVW-IDLOPNR                                           
024000              , :T01IVW-IDPTYP                                            
024100              , :T01IVW-TIRP                                              
024200              , :T01IVW-FLKLAR                                            
024300              , :T01IVW-IV-DATA                                           
024400              , :T01IVW-IV-DATA2                                          
024500     END-EXEC                                                             
024600                                                                          
024700     MOVE SQLCODE TO SQLCODE-WS                                           
024800     PERFORM DB2-STATUS-CHECK                                             
024900     .                                                                    
025000     EJECT                                                                
025100 DB2-UPDATE-T01IVW SECTION.                                               
025200                                                                          
025300     MOVE 000     TO GOOD-SQLCODECODES                                    
025400     EXEC SQL                                                             
025500         UPDATE T01IVW                                                    
025600         SET FLKLAR = 'J'                                                 
025700         WHERE   CURRENT OF T01IVW-CRS                                    
025800     END-EXEC                                                             
025900                                                                          
026000     MOVE SQLCODE TO SQLCODE-WS                                           
026100     PERFORM DB2-STATUS-CHECK                                             
026200     .                                                                    
026300     EJECT                                                                
026400 DB2-CLOSE-CRS SECTION.                                                   
026500                                                                          
026600     EXEC SQL                                                             
026700         CLOSE T01IVW-CRS                                                 
026800     END-EXEC                                                             
026900     .                                                                    
027000     EJECT                                                                
027100 DB2-STATUS-CHECK SECTION.                                                
027200                                                                          
027300     SET SQLCODE-IX TO 1                                                  
027400     SEARCH GOOD-SQLCODE                                                  
027500       AT END                                                             
027600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
027700          DELIMITED BY SIZE INTO ERROR-TEXT                               
027800          CALL ABEND USING RKOD-ABEND-DB2                                 
027900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
028000       CONTINUE                                                           
028100     END-SEARCH                                                           
028200     .                                                                    
