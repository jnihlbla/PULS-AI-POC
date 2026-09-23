000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF104400.                                                
000600 AUTHOR.         BARSHARANI BISHOYE.                                      
000700 DATE-WRITTEN.   20/09/21.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        THE PROGRAM READS T01DHEA_ARC DB2 TABLE                          
001210*        AND T01DLIN_ARC DB2 TABLE                                        
001300*                                                                         
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- RECORD FROM T01DHEA_ARC                                    
002800     SELECT WF1044                     ASSIGN TO WF1044D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  WF1044                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  RECORD -COPY WF1044 -PRE  UT-  -L.                                   
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'WF104400'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
004700 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
004800 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
004900     EJECT                                                                
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005400     EJECT                                                                
005500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1044'.              
005600                                                                          
005700 01  MESSAGE-CODES.                                                       
005800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005900     EJECT                                                                
006000 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
006100     SKIP3                                                                
006200*    -COPY WZ20DAYS                                                       
006300     EJECT                                                                
006400                                                                          
006500*    --- PARAMETERS TO ABEND                                              
006600                                                                          
006700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007000     SKIP2                                                                
007100 01  ERROR-TEXT.                                                          
007200     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
007300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007600 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200*01  -COPY WF1044  -PRE UT-                                               
008300*                                                                         
008400     EJECT                                                                
008500*                                                                         
008600*        WORK-AREAS FOR DB2-SECTIONS                                      
008700*                                                                         
008800 01  FILLER                    PIC X(16) VALUE 'T01DHEAARC-AREA'.         
008900*01  -COPY T01DHEA        -PRE DHEA-                                      
009000     EXEC SQL INCLUDE T01DHEA END-EXEC.                                   
009100     EJECT                                                                
009110 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA'.         
009120*01  -COPY T01DLIN        -PRE DLIN-                                      
009130     EXEC SQL INCLUDE T01DLIN END-EXEC.                                   
009140     EJECT                                                                
009200 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
009300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009400*                        **** STATUS-CODE FROM DB2                        
009500                                                                          
009600 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
009700 01  DB2-WS.                                                              
009800   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
009900     88  ROW-FOUND                         VALUE +000.                    
010000     88  ROW-MISSING                       VALUE +100.                    
010100   03  GOOD-SQLCODES.                                                     
010200     05  GOOD-SQLCODE OCCURS 5                                            
010300         INDEXED BY SQLCODE-IX PIC 999.                                   
010400     EJECT                                                                
010900 PROCEDURE DIVISION.                                                      
011000 MAIN SECTION.                                                            
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011500     PERFORM B-EXECUTE                                                    
011600                                                                          
011700     PERFORM Z-FINISH                                                     
011800                                                                          
011900     MOVE ZERO TO RETURN-CODE                                             
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300 A-INIT SECTION.                                                          
012400     OPEN OUTPUT WF1044                                                   
012500                                                                          
012600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
012700     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
012800     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
012900     MOVE 11                          TO DAYS-KVDAYS                      
013000     MOVE ' '                         TO DAYS-IDCALEND                    
013100     MOVE SPACE                       TO DAYS-TIDATE1                     
013200     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
013300     CALL WZ20DAYS USING                                                  
013400          DAYS-WZ20DAYS                                                   
013500     IF DAYS-KDRC = ZERO                                                  
013700       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
013800     END-IF                                                               
013900     INITIALIZE GOOD-SQLCODES                                             
014000     .                                                                    
014100     EJECT                                                                
014200 B-EXECUTE SECTION.                                                       
014310     PERFORM DB2-OPEN-CRS1-DHEA                                           
014320     PERFORM DB2-FETCH-CRS1-DHEA                                          
014330     PERFORM UNTIL ROW-MISSING                                            
014340       PERFORM C-MOVE-OUTPUT                                              
014350       PERFORM S11-WRITE-WF1044                                           
014360       PERFORM DB2-FETCH-CRS1-DHEA                                        
014370     END-PERFORM                                                          
014380     PERFORM DB2-CLOSE-CRS1-DHEA                                          
014390     .                                                                    
014391     EJECT                                                                
014410 C-MOVE-OUTPUT SECTION.                                                   
014420     MOVE DHEA-IDFINDOC    TO  UT-IDFINDOC                                
014430     MOVE DHEA-DAFINDOC    TO  UT-DAFINDOC                                
014431     MOVE DLIN-IDEXCUST-1  TO  UT-IDEXCUST                                
014432     MOVE DLIN-BEART       TO  UT-BEART                                   
014440     MOVE DLIN-PRARTNTO    TO  UT-PRARTNTO                                
014450     MOVE DLIN-KDVALISO    TO  UT-KDVALISO                                
014451     MOVE DLIN-KDFRAKT     TO  UT-KDFRAKT                                 
014460     .                                                                    
014470     EJECT                                                                
014500 S11-WRITE-WF1044 SECTION.                                                
014600                                                                          
014700     WRITE UT-RECORD FROM UT-WF1044                                       
014800                                                                          
014900     .                                                                    
015000     EJECT                                                                
015100 Z-FINISH SECTION.                                                        
015200     CLOSE WF1044                                                         
015300     .                                                                    
015400     EJECT                                                                
015410* --- DB2 SECTIONS  ---                                                   
015500 DB2-OPEN-CRS1-DHEA SECTION.                                              
015510     MOVE 000100 TO GOOD-SQLCODES                                         
015520     EXEC SQL                                                             
015530        DECLARE CRS1 CURSOR FOR                                           
015540        SELECT A.IDFINDOC                                                 
015550             , A.DAFINDOC                                                 
015551             , B.IDEXCUST_1                                               
015552             , B.BEART                                                    
015562             , B.PRARTNTO                                                 
015563             , B.KDVALISO                                                 
015564             , B.KDFRAKT                                                  
015570                                                                          
015580        FROM   T01DHEA_ARC A                                              
015581              ,T01DLIN_ARC B                                              
015590                                                                          
015592        WHERE    A.IDLEGSEL      = 'VCCS'                                 
015595        AND      A.IDLEGSEL      = B.IDLEGSEL                             
015596        AND      A.DAEXDAT       = B.DAEXDAT                              
015597        AND      A.TIEXTID       = B.TIEXTID                              
015598        AND      B.TIEXTID       = A.TIEXTID                              
015599        AND      B.KDVALISO      = A.KDVALISO                             
015601        AND      B.IDLANDX3_SEND = A.IDLANDX3_SEND                        
015602        AND      B.IDLEVNR       = A.IDLEVNR                              
015603        AND      B.IDPARTNR      = A.IDPARTNR                             
015604        AND      B.KDFINDOC      = A.KDFINDOC                             
015605        AND      B.FLSOFT        = A.FLSOFT                               
015606        AND      B.FLFREE        = A.FLFREE                               
015607        AND      B.FLPRIV        = A.FLPRIV                               
015608        AND      B.IDBREAK_1     = A.IDBREAK_1                            
015609        AND      B.IDBREAK_2     = A.IDBREAK_2                            
015610        AND     (B.BEART         = 'PACKING & HANDLING'                   
015611        OR       B.BEART         = 'INSURANCE'                            
015612        OR       B.BEART         = 'FREIGHT')                             
015613        AND     (B.IDDC          = '11'                                   
015614        OR       B.IDDC          = 'SE')                                  
015615        AND      A.DAFINDOC     > :WS-RUNDATUM-FROM                       
015616     END-EXEC                                                             
015617                                                                          
015618     EXEC SQL                                                             
015619        OPEN CRS1                                                         
015620     END-EXEC                                                             
015621                                                                          
015622     MOVE SQLCODE        TO SQLCODE-WS                                    
015623     PERFORM DB2-STATUS-CHECK                                             
015624     .                                                                    
015625 DB2-FETCH-CRS1-DHEA  SECTION.                                            
015626     MOVE 000100         TO GOOD-SQLCODES                                 
015627                                                                          
015628     EXEC SQL                                                             
015629              FETCH CRS1                                                  
015630              INTO  :DHEA-IDFINDOC                                        
015631                  , :DHEA-DAFINDOC                                        
015632                  , :DLIN-IDEXCUST-1                                      
015633                  , :DLIN-BEART                                           
015634                  , :DLIN-PRARTNTO                                        
015635                  , :DLIN-KDVALISO                                        
015636                  , :DLIN-KDFRAKT                                         
015637     END-EXEC                                                             
015638                                                                          
015639     MOVE SQLCODE        TO SQLCODE-WS                                    
015640     PERFORM DB2-STATUS-CHECK                                             
015641     .                                                                    
015642 DB2-CLOSE-CRS1-DHEA  SECTION.                                            
015643                                                                          
015644     EXEC SQL                                                             
015645        CLOSE CRS1                                                        
015646     END-EXEC                                                             
015647     .                                                                    
015650 DB2-STATUS-CHECK  SECTION.                                               
015700                                                                          
015800     SET SQLCODE-IX TO 1                                                  
015900     SEARCH GOOD-SQLCODE                                                  
016000       AT END                                                             
016100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
016200          DELIMITED BY SIZE INTO ERROR-TEXT                               
016300          CALL ABEND USING RKOD-ABEND-DB2                                 
016400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
016500     END-SEARCH                                                           
016600     .                                                                    
