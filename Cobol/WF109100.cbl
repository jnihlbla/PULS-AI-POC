000100 PROCESS DYNAM                                                            
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF109100.                                                
000600 AUTHOR.         MAMATHA SHETTY.                                          
000700 DATE-WRITTEN.   22/04/26.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        THE PROGRAM READS T01DHEA_ARC DB2 TABLE                          
001300*        AND T01DLIN_ARC DB2 TABLE TO CREATE NON VCCS REPORT              
001400*                                                                         
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- RECORD FROM T01DHEA_ARC                                    
002300     SELECT WF1091                     ASSIGN TO WF1091D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  WF1091                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  POST-REC -COPY WF1091   -L.                                          
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'WF109100'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
004100 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
004200 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
004210 01  WS-IDARTNR-FINANCE          PIC X(50).                               
004300 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
004400     EJECT                                                                
004500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004900     EJECT                                                                
005000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1091'.              
005100                                                                          
005200 01  MESSAGE-CODES.                                                       
005300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005400     EJECT                                                                
005500 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
005600     SKIP3                                                                
005700*    -COPY WZ20DAYS                                                       
005800     EJECT                                                                
005900                                                                          
006000*    --- PARAMETERS TO ABEND                                              
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  ERROR-TEXT.                                                          
006700     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
006800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007100 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700*01  AREA -COPY WF1091     -PRE UT-                                       
007800      EJECT                                                               
007900*                                                                         
008000*        WORK-AREAS FOR DB2-SECTIONS                                      
008100*                                                                         
008200 01  FILLER                    PIC X(16) VALUE 'T01DHEAARC-AREA'.         
008300*01  -COPY T01DHEA        -PRE DHEA-                                      
008400     EXEC SQL INCLUDE T01DHEA END-EXEC.                                   
008500     EJECT                                                                
008600 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA'.         
008700*01  -COPY T01DLIN        -PRE DLIN-                                      
008800     EXEC SQL INCLUDE T01DLIN END-EXEC.                                   
008900     EJECT                                                                
009000 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
009100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009200*                        **** STATUS-CODE FROM DB2                        
009300                                                                          
009400 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
009500 01  DB2-WS.                                                              
009600   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
009700     88  ROW-FOUND                         VALUE +000.                    
009800     88  ROW-MISSING                       VALUE +100.                    
009900   03  GOOD-SQLCODES.                                                     
010000     05  GOOD-SQLCODE OCCURS 5                                            
010100         INDEXED BY SQLCODE-IX PIC 999.                                   
010200     EJECT                                                                
010300 PROCEDURE DIVISION.                                                      
010400 MAIN SECTION.                                                            
010500                                                                          
010600     PERFORM A-INIT                                                       
010700                                                                          
010800     PERFORM B-EXECUTE                                                    
010900                                                                          
011000     PERFORM Z-FINISH                                                     
011100                                                                          
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600 A-INIT SECTION.                                                          
011700     OPEN OUTPUT WF1091                                                   
011800                                                                          
011900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
012000     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
012100     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
012200     MOVE 45                         TO DAYS-KVDAYS                       
012300     MOVE ' '                         TO DAYS-IDCALEND                    
012400     MOVE SPACE                       TO DAYS-TIDATE1                     
012500     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
012600     CALL WZ20DAYS USING                                                  
012700          DAYS-WZ20DAYS                                                   
012800     IF DAYS-KDRC = ZERO                                                  
012900*** IT'S PREVIUS MONTH THAT SHOULD BE PROCESSED                           
013000       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
013010       MOVE '31'                      TO WS-RUNDATUM-FROM (7:2)           
013100       MOVE WS-CURRENT-DATE           TO WS-RUNDATUM-TO                   
013200       MOVE '01'                      TO WS-RUNDATUM-TO(7:2)              
020028     END-IF                                                               
020029     .                                                                    
020032     EJECT                                                                
020033 B-EXECUTE SECTION.                                                       
020034     PERFORM DB2-OPEN-CRS1-DLIN                                           
020035     PERFORM DB2-FETCH-CRS1-DLIN                                          
020036     PERFORM UNTIL ROW-MISSING                                            
020037       PERFORM C-MOVE-OUTPUT                                              
020038       PERFORM S11-WRITE-WF1091                                           
020039       PERFORM DB2-FETCH-CRS1-DLIN                                        
020040     END-PERFORM                                                          
020041     PERFORM DB2-CLOSE-CRS1-DLIN                                          
020042     .                                                                    
020043     EJECT                                                                
020044 C-MOVE-OUTPUT SECTION.                                                   
020045     MOVE DHEA-IDFINDOC        TO UT-IDFINDOC                             
020046     MOVE DHEA-DAFINDOC        TO UT-DAFINDOC                             
020047     MOVE DLIN-IDARTNR-FINANCE TO WS-IDARTNR-FINANCE                      
020048     MOVE WS-IDARTNR-FINANCE(1:9) TO UT-IDARTNR-FINANCE                   
020049     MOVE DLIN-BEART           TO UT-BEART                                
020050     MOVE DLIN-KVLEVART        TO UT-KVLEVART                             
020051     MOVE DLIN-VKARTNTO        TO UT-VKARTNTO                             
020052     MOVE DLIN-IDSTATNR        TO UT-IDSTATNR                             
020053     MOVE DLIN-IDLEGSEL        TO UT-IDLEGSEL                             
020054     MOVE DLIN-IDREF           TO UT-IDREF                                
020057     .                                                                    
020058     EJECT                                                                
020059 S11-WRITE-WF1091 SECTION.                                                
020068     WRITE   POST-REC  FROM UT-WF1091                                     
020069     .                                                                    
020070     EJECT                                                                
020071 Z-FINISH SECTION.                                                        
020072     CLOSE WF1091                                                         
020073     .                                                                    
020074     EJECT                                                                
020116 DB2-OPEN-CRS1-DLIN SECTION.                                              
020117     MOVE 000100 TO GOOD-SQLCODES                                         
020118                                                                          
020119     EXEC SQL                                                             
020120        DECLARE CRS1 CURSOR FOR                                           
020121        SELECT IDFINDOC, DAFINDOC, IDARTNR_FINANCE, BEART,                
020122               KVLEVART, VKARTNTO, IDSTATNR ,A.IDLEGSEL ,A.IDREF          
020123                                                                          
020124        FROM   T01DLIN_ARC A,                                             
020125               T01DHEA_ARC B                                              
020128                                                                          
020129        WHERE  A.KDFINDOC = 'INV'                                         
020131        AND    B.IDLEGSEL     <> 'VCCS'                                   
020132        AND    A.IDLEGSEL     <> 'VCCS'                                   
020133        AND    B.DAEXDAT       = A.DAEXDAT                                
020134        AND    B.TIEXTID       = A.TIEXTID                                
020135        AND    B.KDVALISO      = A.KDVALISO                               
020136        AND    B.IDLANDX3_SEND = A.IDLANDX3_SEND                          
020137        AND    B.IDLEVNR       = A.IDLEVNR                                
020138        AND    B.IDPARTNR      = A.IDPARTNR                               
020139        AND    B.KDFINDOC      = A.KDFINDOC                               
020140        AND    B.FLSOFT        = A.FLSOFT                                 
020141        AND    B.FLFREE        = A.FLFREE                                 
020142        AND    B.FLPRIV        = A.FLPRIV                                 
020143        AND    B.IDBREAK_1     = A.IDBREAK_1                              
020144        AND    B.IDBREAK_2     = A.IDBREAK_2                              
020146        AND    B.DAFINDOC   >  :WS-RUNDATUM-FROM                          
020148        AND    B.DAFINDOC   <  :WS-RUNDATUM-TO                            
020151     END-EXEC                                                             
020152                                                                          
020153     EXEC SQL                                                             
020154             OPEN CRS1                                                    
020155     END-EXEC                                                             
020156                                                                          
020157     MOVE SQLCODE        TO SQLCODE-WS                                    
020158     PERFORM DB2-STATUS-CHECK                                             
020159     .                                                                    
020160 DB2-FETCH-CRS1-DLIN SECTION.                                             
020161     MOVE 000100         TO GOOD-SQLCODES                                 
020162                                                                          
020163     EXEC SQL                                                             
020164              FETCH CRS1                                                  
020165              INTO  :DHEA-IDFINDOC                                        
020166                  , :DHEA-DAFINDOC                                        
020167                  , :DLIN-IDARTNR-FINANCE                                 
020168                  , :DLIN-BEART                                           
020169                  , :DLIN-KVLEVART                                        
020170                  , :DLIN-VKARTNTO                                        
020171                  , :DLIN-IDSTATNR                                        
020172                  , :DLIN-IDLEGSEL                                        
020173                  , :DLIN-IDREF                                           
020174     END-EXEC                                                             
020175                                                                          
020176     MOVE SQLCODE        TO SQLCODE-WS                                    
020177     PERFORM DB2-STATUS-CHECK                                             
020178     .                                                                    
020179 DB2-CLOSE-CRS1-DLIN SECTION.                                             
020180                                                                          
020181     EXEC SQL                                                             
020182        CLOSE CRS1                                                        
020183     END-EXEC                                                             
020184     .                                                                    
020185 DB2-STATUS-CHECK  SECTION.                                               
020186                                                                          
020187     SET SQLCODE-IX TO 1                                                  
020188     SEARCH GOOD-SQLCODE                                                  
020189       AT END                                                             
020190          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
020191          DELIMITED BY SIZE INTO ERROR-TEXT                               
020192          CALL ABEND USING RKOD-ABEND-DB2                                 
020193       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
020194     END-SEARCH                                                           
020200     .                                                                    
