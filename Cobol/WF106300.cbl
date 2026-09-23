       PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF106300.                                                
000300 AUTHOR.         BARSHARANI BISHOYE.                                      
000400 DATE-WRITTEN.   19/08/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*****************************************************************         
000800*    FUNCTION:                                                  *         
000900*         SW DOWNLOAD REPORT FOR RUSSIA                         *         
001000*****************************************************************         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SELECT WF1063                     ASSIGN TO WF1063D1.                
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD  WF1063                                                               
002100     LABEL RECORD STANDARD                                                
002200     RECORDING  F                                                         
002300     BLOCK CONTAINS 0.                                                    
002400                                                                          
002500*01 POST-WF1063 -COPY WF1063   -L.                                        
002600 EJECT                                                                    
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'WF106300'.            
003100                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003910 77  WS-IX                       PIC S9(3)   COMP-3 VALUE ZERO.           
004000 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004010 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
004100 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
004200 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
004300                                                                          
004301 01  RKOD-ABEND-DB2              PIC S9(4)  VALUE +998 COMP SYNC.         
004323*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004324 01  GENERAL-SUBPROGRAMS.                                                 
004325     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004331 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1063'.              
004332                                                                          
004338 01  MESSAGE-CODES.                                                       
004339     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
004340     EJECT                                                                
004341 01  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.              
004342     SKIP3                                                                
004343*    -COPY WZ20DAYS                                                       
004344     EJECT                                                                
004345                                                                          
004347*01  -COPY WF1063                                                         
004350     EJECT                                                                
004400*WORK-AREAS FOR DB2-SECTIONS                                              
004500*                                                                         
005400 01  FILLER                    PIC X(16)   VALUE 'DLIN-AREA'.             
005600*01  -COPY T01DLIN    -PRE T01DLIN-                                       
005930                                                                          
005940     EXEC SQL INCLUDE T01DLIN  END-EXEC.                                  
005950     EJECT                                                                
005960 01  FILLER                    PIC X(16)  VALUE 'SQLCA-AREA'.             
005970     EXEC SQL INCLUDE SQLCA END-EXEC.                                     
006000***** STATUS-CODE FROM DB2                                                
006100                                                                          
006200 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
006300 01  DB2-WS.                                                              
006400   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
006500     88  CURSOR-OK                         VALUE +000.                    
006600     88  LINES-FOUND                       VALUE +000.                    
006700     88  LINES-MISSING                     VALUE +100.                    
006800     88  RESOURCE-WRONG                    VALUE 904.                     
006900                                                                          
007000   03  GOOD-SQLCODES.                                                     
007100     05  GOOD-SQLCODE OCCURS 5                                            
007200         INDEXED BY SQLCODE-IX PIC 999.                                   
007300     EJECT                                                                
007400                                                                          
007500 PROCEDURE DIVISION.                                                      
007600 MAIN SECTION.                                                            
007800                                                                          
007900     PERFORM A-INIT                                                       
008000                                                                          
008100     PERFORM B-EXECUTE                                                    
008200                                                                          
008300     PERFORM Z-FINISH                                                     
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008900                                                                          
009300     OPEN OUTPUT WF1063                                                   
009301     MOVE ZERO    TO WS-IX                                                
009302     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
009303     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
009304     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
009305     MOVE 2                           TO DAYS-KVDAYS                      
009306     MOVE ' '                         TO DAYS-IDCALEND                    
009307     MOVE SPACE                       TO DAYS-TIDATE1                     
009308     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
009309     CALL WZ20DAYS USING                                                  
009310          DAYS-WZ20DAYS                                                   
009311     IF DAYS-KDRC = ZERO                                                  
009312       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-TO                   
009313       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
009314       MOVE '01'                      TO WS-RUNDATUM-FROM(7:2)            
009315       MOVE '31'                      TO WS-RUNDATUM-TO(7:2)              
009316     END-IF                                                               
009398     INITIALIZE GOOD-SQLCODES                                             
009400     .                                                                    
009500     EJECT                                                                
009510 B-EXECUTE SECTION.                                                       
009520     PERFORM DB2-OPN-CRS1                                                 
009530     PERFORM DB2-FETCH-CRS1                                               
009535       PERFORM UNTIL LINES-MISSING                                        
009537         PERFORM C-MOVE-OUTPUT                                            
009538         PERFORM S11-WRITE-WF1063                                         
009541         PERFORM DB2-FETCH-CRS1                                           
009542     END-PERFORM                                                          
009543     PERFORM DB2-CLOSE-CRS1                                               
009544     .                                                                    
009545     EJECT                                                                
009546 C-MOVE-OUTPUT SECTION.                                                   
009547     MOVE T01DLIN-DAEXDAT         TO UT-DAEXDAT                           
009548     MOVE T01DLIN-DAREFDAT        TO UT-DAREFDAT                          
009549     MOVE T01DLIN-IDPARTNR        TO UT-IDPARTNR                          
009550     MOVE T01DLIN-IDEXCUST-1      TO UT-IDEXCUST-1                        
009551     MOVE T01DLIN-IDEXCUST-2      TO UT-IDEXCUST-2                        
009552     MOVE T01DLIN-IDREF           TO UT-IDREF                             
009553     MOVE T01DLIN-BEVOLREF        TO UT-BEVOLREF                          
009554     MOVE T01DLIN-IDARTNR-FINANCE TO UT-IDARTNR-FINANCE                   
009555     MOVE T01DLIN-BEART           TO UT-BEART                             
009556     MOVE T01DLIN-KVLEVART        TO UT-KVLEVART                          
009557     MOVE T01DLIN-PRARTNTO        TO UT-PRARTNTO                          
009558     MOVE T01DLIN-BELEVVIL        TO UT-BELEVVIL                          
009559     MOVE T01DLIN-IDLEVNR-ART     TO UT-IDLEVNR-ART                       
009560     .                                                                    
009561     EJECT                                                                
009562 S11-WRITE-WF1063 SECTION.                                                
009563       WRITE POST-WF1063   FROM   UT-WF1063                               
009570     .                                                                    
009580    EJECT                                                                 
009600 Z-FINISH SECTION.                                                        
009700      CLOSE WF1063                                                        
010100     .                                                                    
010200     EJECT                                                                
010210* --- DB2 SECTIONS  ---                                                   
010300  DB2-OPN-CRS1 SECTION.                                                   
010400     MOVE 000100 TO GOOD-SQLCODES                                         
010500     EXEC SQL                                                             
010600        DECLARE CRS1 CURSOR FOR                                           
010700        SELECT DAEXDAT                                                    
010800             , DAREFDAT                                                   
010900             , IDPARTNR                                                   
011000             , IDEXCUST_1                                                 
011100             , IDEXCUST_2                                                 
011200             , IDREF                                                      
011300             , BEVOLREF                                                   
011400             , IDARTNR_FINANCE                                            
011500             , BEART                                                      
011600             , KVLEVART                                                   
011700             , PRARTNTO                                                   
011800             , BELEVVIL                                                   
011900             , IDLEVNR_ART                                                
012000                                                                          
012100        FROM   T01DLIN                                                    
012200        WHERE  IDEXCUST_1 = '2697'                                        
012300        AND   (DAEXDAT    =  :WS-RUNDATUM-FROM                            
012301        OR     DAEXDAT    >  :WS-RUNDATUM-FROM)                           
012310        AND   (DAEXDAT    =  :WS-RUNDATUM-TO                              
012320        OR     DAEXDAT    <  :WS-RUNDATUM-TO)                             
012400     END-EXEC                                                             
012500                                                                          
012600     EXEC SQL                                                             
012700        OPEN CRS1                                                         
012800     END-EXEC                                                             
012900                                                                          
013000     MOVE SQLCODE        TO SQLCODE-WS                                    
013100     PERFORM DB2-STATUS-CHECK                                             
013200     .                                                                    
013210                                                                          
013220 DB2-FETCH-CRS1 SECTION.                                                  
013230     MOVE 000100         TO GOOD-SQLCODES                                 
013240                                                                          
013250     EXEC SQL                                                             
013260              FETCH CRS1                                                  
013261              INTO  :T01DLIN-DAEXDAT                                      
013262                  , :T01DLIN-DAREFDAT                                     
013263                  , :T01DLIN-IDPARTNR                                     
013264                  , :T01DLIN-IDEXCUST-1                                   
013265                  , :T01DLIN-IDEXCUST-2                                   
013266                  , :T01DLIN-IDREF                                        
013267                  , :T01DLIN-BEVOLREF                                     
013268                  , :T01DLIN-IDARTNR-FINANCE                              
013269                  , :T01DLIN-BEART                                        
013270                  , :T01DLIN-KVLEVART                                     
013271                  , :T01DLIN-PRARTNTO                                     
013272                  , :T01DLIN-BELEVVIL                                     
013273                  , :T01DLIN-IDLEVNR-ART                                  
013274     END-EXEC                                                             
013275                                                                          
013276     MOVE SQLCODE        TO SQLCODE-WS                                    
013277     PERFORM DB2-STATUS-CHECK                                             
013278     .                                                                    
013279                                                                          
013280 DB2-CLOSE-CRS1 SECTION.                                                  
013290                                                                          
013291     EXEC SQL                                                             
013292        CLOSE CRS1                                                        
013293     END-EXEC                                                             
013294     .                                                                    
013295                                                                          
013296 DB2-STATUS-CHECK  SECTION.                                               
013297                                                                          
013298     SET SQLCODE-IX TO 1                                                  
013299     SEARCH GOOD-SQLCODE                                                  
013300       AT END                                                             
013400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
013500          DELIMITED BY SIZE INTO ERROR-TEXT                               
013600          CALL ABEND USING RKOD-ABEND-DB2                                 
013700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
013800          CONTINUE                                                        
013900     END-SEARCH                                                           
014000     .                                                                    
