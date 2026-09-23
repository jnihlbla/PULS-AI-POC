000100 PROCESS DYNAM                                                            
000201*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000301*                                                                         
000401 ID DIVISION.                                                             
000501 PROGRAM-ID.     WF104600.                                                
000601 AUTHOR.         BARSHARANI BISHOYE.                                      
000701 DATE-WRITTEN.   21/04/01.                                                
000801 DATE-COMPILED.                                                           
000901                                                                          
001001*                                                                         
001101*    FUNCTION:                                                            
001201*        CREATE FILES SCRAP REPORT                                        
001301*                                                                         
001401*                                                                         
001501*    ABENDCODES:                                                          
001601*        U0016 -  . . . .                                                 
001701*        U1000 -  . . . .                                                 
001801*                                                                         
001901                                                                          
002001     SKIP3                                                                
002101 ENVIRONMENT DIVISION.                                                    
002201     SKIP2                                                                
002301 INPUT-OUTPUT SECTION.                                                    
002401                                                                          
002501 FILE-CONTROL.                                                            
002601     SKIP2                                                                
002701*          --- RECORDS WITH CHINA                                         
002801     SELECT WF1046                     ASSIGN TO WF1046D1.                
002901     EJECT                                                                
003001 DATA DIVISION.                                                           
003101     SKIP3                                                                
003201 FILE SECTION.                                                            
003301     SKIP3                                                                
003401 FD  WF1046                                                               
003501     RECORDING       F                                                    
003601     BLOCK CONTAINS  0.                                                   
003701                                                                          
003801*01  RECORD -COPY WF1046  -PRE  UT-   -L.                                 
003901     EJECT                                                                
004001 WORKING-STORAGE SECTION.                                                 
004101                                                                          
004201 77  IDPGM                       PIC X(8)    VALUE 'WF104600'.            
004301 77  YES                         PIC X       VALUE 'J'.                   
004401 77  NOO                         PIC X       VALUE 'N'.                   
004501 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
004601 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
004701 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
004800 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
004900     EJECT                                                                
005001 01  GENERAL-SUBPROGRAMS.                                                 
005101*                                                                         
005201     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005301     EJECT                                                                
005401 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1046'.              
005501                                                                          
005601 01  MESSAGE-CODES.                                                       
005701     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005801     EJECT                                                                
005901 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
006001     SKIP3                                                                
006101*    -COPY WZ20DAYS                                                       
006201     EJECT                                                                
006301                                                                          
006401*    --- PARAMETRAR TILL ABEND                                            
006501*                                                                         
006601 01  ERROR-TEXT.                                                          
006701     03  FILLER                PIC X(10)    VALUE 'ERROR-TEXT'.           
006801     03  ERROR-TEXT-STR        PIC X(72)   VALUE SPACE.                   
006901 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007001 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007101     EJECT                                                                
007201*    --- PARAMETRAR TILL POSTSUM                                          
007301*                                                                         
007401*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007601*01  AREA -COPY WF1046     -PRE UT-                                       
007701     EJECT                                                                
007801*                                                                         
007901*        WORK-AREAS FOR DB2-SECTIONS                                      
008002*                                                                         
008003 01  FILLER                    PIC X(16) VALUE 'T01LSEL-AREA    '.        
008004*01  -COPY T01LSEL        -PRE LSEL-                                      
008005     EXEC SQL INCLUDE T01LSEL   END-EXEC.                                 
008006     EJECT                                                                
008802 01  FILLER                    PIC X(16) VALUE 'T01DHEAARC-AREA'.         
008803*01  -COPY T01DHEA        -PRE DHEA-                                      
008804     EXEC SQL INCLUDE T01DHEA END-EXEC.                                   
008805     EJECT                                                                
008806 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA'.         
008807*01  -COPY T01DLIN        -PRE DLIN-                                      
008808     EXEC SQL INCLUDE T01DLIN END-EXEC.                                   
008809     EJECT                                                                
008901 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
009001       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009101*                        **** STATUS-CODE FROM DB2                        
009201                                                                          
009301 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
009401 01  DB2-WS.                                                              
009501   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
009601     88  ROW-FOUND                         VALUE +000.                    
009701     88  ROW-MISSING                       VALUE +100.                    
009801   03  GOOD-SQLCODES.                                                     
009901     05  GOOD-SQLCODE OCCURS 5                                            
010001         INDEXED BY SQLCODE-IX PIC 999.                                   
010101     EJECT                                                                
010201 PROCEDURE DIVISION.                                                      
010301 MAIN SECTION.                                                            
010401                                                                          
010501      PERFORM A-INIT                                                      
010601                                                                          
010701      PERFORM B-EXECUTE                                                   
010801                                                                          
010901      PERFORM Z-FINISH                                                    
011001                                                                          
011101      MOVE ZERO TO RETURN-CODE                                            
011201      GOBACK                                                              
011301      .                                                                   
011401      EJECT                                                               
011501 A-INIT SECTION.                                                          
011601                                                                          
011701     OPEN OUTPUT WF1046                                                   
011801                                                                          
011901     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
012001     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
012101     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
012201     MOVE 45                          TO DAYS-KVDAYS                      
012301     MOVE ' '                         TO DAYS-IDCALEND                    
012401     MOVE SPACE                       TO DAYS-TIDATE1                     
012501     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
012601     CALL WZ20DAYS USING                                                  
012701          DAYS-WZ20DAYS                                                   
012801     IF DAYS-KDRC = ZERO                                                  
012901       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
013001       MOVE '31'                      TO WS-RUNDATUM-FROM(7:2)            
013101       MOVE WS-CURRENT-DATE           TO WS-RUNDATUM-TO                   
013201       MOVE '01'                      TO WS-RUNDATUM-TO(7:2)              
013300     END-IF                                                               
013400     INITIALIZE GOOD-SQLCODES                                             
013500     .                                                                    
013600     EJECT                                                                
013700 B-EXECUTE SECTION.                                                       
013710     PERFORM DB2-DCL-OPN-T01LSEL-CRS                                      
013720     PERFORM DB2-FETCH-T01LSEL-CRS                                        
013730                                                                          
013740     PERFORM UNTIL ROW-MISSING                                            
013750       IF LSEL-IDLEGSEL = 'VCCS'                                          
013751         PERFORM DB2-OPEN-CRS3-DLIN-DHEA                                  
013752         PERFORM DB2-FETCH-CRS3-DLIN-DHEA                                 
013754         PERFORM UNTIL ROW-MISSING                                        
013755           MOVE DLIN-IDLEGSEL TO  UT-CREATER-IDLEGSEL                     
013756           IF DLIN-IDPARTNR = '352343'                                    
013757           OR DLIN-IDPARTNR = '30957'                                     
013758           OR DLIN-IDPARTNR = '119669'                                    
013759           OR DLIN-IDPARTNR = '4113'                                      
013760           OR DLIN-IDPARTNR = '212321'                                    
013761           OR DLIN-IDPARTNR = '357931'                                    
013762           OR DLIN-IDPARTNR = '357225'                                    
013763           OR DLIN-IDPARTNR = '356624'                                    
013764           OR DLIN-IDPARTNR = '103543'                                    
013765           OR DLIN-IDPARTNR = '7908'                                      
013765           OR DLIN-IDPARTNR = '357347'                                    
013766             IF DLIN-IDPARTNR = '352343'                                  
013767               MOVE 'VCCN'   TO DLIN-IDLEGSEL                             
013768             END-IF                                                       
013769             IF DLIN-IDPARTNR = '30957'                                   
013770               MOVE 'VCIN'   TO DLIN-IDLEGSEL                             
013771             END-IF                                                       
013772             IF DLIN-IDPARTNR = '119669'                                  
013773               MOVE 'VCKR'   TO DLIN-IDLEGSEL                             
013774             END-IF                                                       
013775             IF DLIN-IDPARTNR = '4113'                                    
013776               MOVE 'VCUS'   TO DLIN-IDLEGSEL                             
013777             END-IF                                                       
013778             IF DLIN-IDPARTNR = '212321'                                  
013779               MOVE 'VCMY'   TO DLIN-IDLEGSEL                             
013780             END-IF                                                       
013781             IF DLIN-IDPARTNR = '357931'                                  
013782               MOVE 'VCMX'   TO DLIN-IDLEGSEL                             
013783             END-IF                                                       
013784             IF DLIN-IDPARTNR = '357225'                                  
013785               MOVE 'VCBR'   TO DLIN-IDLEGSEL                             
013786             END-IF                                                       
013787             IF DLIN-IDPARTNR = '356624'                                  
013788               MOVE 'VCTW'   TO DLIN-IDLEGSEL                             
013789             END-IF                                                       
013790             IF DLIN-IDPARTNR = '103543'                                  
013791               MOVE 'VCTR'   TO DLIN-IDLEGSEL                             
013792             END-IF                                                       
013793             IF DLIN-IDPARTNR = '7908'                                    
013794               MOVE 'VCTH'   TO DLIN-IDLEGSEL                             
013795             END-IF                                                       
013793             IF DLIN-IDPARTNR = '357347'                                  
013794               MOVE 'VCZA'   TO DLIN-IDLEGSEL                             
013795             END-IF                                                       
013796             PERFORM C-MOVE-OUTPUT                                        
013797             PERFORM S11-WRITE-WF1046                                     
013798           END-IF                                                         
013799           PERFORM DB2-FETCH-CRS3-DLIN-DHEA                               
013800         END-PERFORM                                                      
013801         PERFORM DB2-CLOSE-CRS3-DLIN-DHEA                                 
013802                                                                          
013803         PERFORM DB2-OPEN-CRS4-DLIN-DHEA-ARC                              
013804         PERFORM DB2-FETCH-CRS4-DLIN-DHEA-ARC                             
013805         PERFORM UNTIL ROW-MISSING                                        
013806           MOVE DLIN-IDLEGSEL TO  UT-CREATER-IDLEGSEL                     
013807           IF DLIN-IDPARTNR = '352343'                                    
013808           OR DLIN-IDPARTNR = '30957'                                     
013809           OR DLIN-IDPARTNR = '119669'                                    
013810           OR DLIN-IDPARTNR = '4113'                                      
013811           OR DLIN-IDPARTNR = '212321'                                    
013812           OR DLIN-IDPARTNR = '357931'                                    
013813           OR DLIN-IDPARTNR = '357225'                                    
013814           OR DLIN-IDPARTNR = '356624'                                    
013815           OR DLIN-IDPARTNR = '103543'                                    
013816           OR DLIN-IDPARTNR = '7908'                                      
013817             IF DLIN-IDPARTNR = '352343'                                  
013818               MOVE 'VCCN'   TO DLIN-IDLEGSEL                             
013819             END-IF                                                       
013820             IF DLIN-IDPARTNR = '30957'                                   
013821               MOVE 'VCIN'   TO DLIN-IDLEGSEL                             
013822             END-IF                                                       
013823             IF DLIN-IDPARTNR = '119669'                                  
013824               MOVE 'VCKR'   TO DLIN-IDLEGSEL                             
013825             END-IF                                                       
013826             IF DLIN-IDPARTNR = '4113'                                    
013827               MOVE 'VCUS'   TO DLIN-IDLEGSEL                             
013828             END-IF                                                       
013829             IF DLIN-IDPARTNR = '212321'                                  
013830               MOVE 'VCMY'   TO DLIN-IDLEGSEL                             
013831             END-IF                                                       
013832             IF DLIN-IDPARTNR = '357931'                                  
013833               MOVE 'VCMX'   TO DLIN-IDLEGSEL                             
013834             END-IF                                                       
013835             IF DLIN-IDPARTNR = '357225'                                  
013836               MOVE 'VCBR'   TO DLIN-IDLEGSEL                             
013837             END-IF                                                       
013838             IF DLIN-IDPARTNR = '356624'                                  
013839               MOVE 'VCTW'   TO DLIN-IDLEGSEL                             
013840             END-IF                                                       
013841             IF DLIN-IDPARTNR = '103543'                                  
013842               MOVE 'VCTR'   TO DLIN-IDLEGSEL                             
013843             END-IF                                                       
013844             IF DLIN-IDPARTNR = '7908'                                    
013845               MOVE 'VCTH'   TO DLIN-IDLEGSEL                             
013846             END-IF                                                       
013847             PERFORM C-MOVE-OUTPUT                                        
013848             PERFORM S11-WRITE-WF1046                                     
013849           END-IF                                                         
013850           PERFORM DB2-FETCH-CRS4-DLIN-DHEA-ARC                           
013851         END-PERFORM                                                      
013852         PERFORM DB2-CLOSE-CRS4-DLIN-DHEA-ARC                             
013853       ELSE                                                               
013860         PERFORM DB2-OPEN-CRS1-DLIN-DHEA                                  
013900         PERFORM DB2-FETCH-CRS1-DLIN-DHEA                                 
014000         PERFORM UNTIL ROW-MISSING                                        
014010           MOVE DLIN-IDLEGSEL TO  UT-CREATER-IDLEGSEL                     
014100           PERFORM C-MOVE-OUTPUT                                          
014201           PERFORM S11-WRITE-WF1046                                       
014300           PERFORM DB2-FETCH-CRS1-DLIN-DHEA                               
014400         END-PERFORM                                                      
014500         PERFORM DB2-CLOSE-CRS1-DLIN-DHEA                                 
014501                                                                          
014502         PERFORM DB2-OPEN-CRS2-DLIN-DHEA-ARC                              
014503         PERFORM DB2-FETCH-CRS2-DLIN-DHEA-ARC                             
014504         PERFORM UNTIL ROW-MISSING                                        
014505           MOVE DLIN-IDLEGSEL TO  UT-CREATER-IDLEGSEL                     
014506           PERFORM C-MOVE-OUTPUT                                          
014507           PERFORM S11-WRITE-WF1046                                       
014508           PERFORM DB2-FETCH-CRS2-DLIN-DHEA-ARC                           
014509         END-PERFORM                                                      
014510         PERFORM DB2-CLOSE-CRS2-DLIN-DHEA-ARC                             
014511       END-IF                                                             
014512                                                                          
014513       PERFORM DB2-FETCH-T01LSEL-CRS                                      
014520     END-PERFORM                                                          
014530     PERFORM DB2-CLOSE-T01LSEL-CRS                                        
014610     .                                                                    
014700     EJECT                                                                
014800 C-MOVE-OUTPUT SECTION.                                                   
014903     MOVE DLIN-IDLEGSEL        TO  UT-IDLEGSEL                            
014904     MOVE DHEA-IDFINDOC        TO  UT-IDFINDOC                            
015101     MOVE DHEA-DAFINDOC        TO  UT-DAFINDOC                            
015102     MOVE DLIN-IDFAKREF        TO  UT-IDFAKREF                            
015103     MOVE DLIN-DAFAKREF        TO  UT-DAFAKREF                            
015201     MOVE DHEA-KDVALISO        TO  UT-KDVALISO                            
015301     MOVE DLIN-IDEXCUST-1      TO  UT-IDEXCUST-1                          
015401     MOVE DLIN-IDEXCUST-2      TO  UT-IDEXCUST-2                          
015501     MOVE DLIN-IDREF           TO  UT-IDREF                               
015601     MOVE DLIN-DAREFDAT        TO  UT-DAREFDAT                            
015701     MOVE DLIN-IDACCNT-1       TO  UT-IDACCNT-1                           
015801     MOVE DLIN-IDACCNT-2       TO  UT-IDACCNT-2                           
015901     MOVE DLIN-BEVOLREF        TO  UT-BEVOLREF                            
016001     MOVE DLIN-IDARTNR-FINANCE TO  UT-IDARTNR-FINANCE                     
016101     MOVE DLIN-BEART           TO  UT-BEART                               
016201     MOVE DLIN-KVLEVART        TO  UT-KVLEVART                            
016301     MOVE DLIN-PRARTNTO        TO  UT-PRARTNTO                            
016401     MOVE DLIN-SUNTO           TO  UT-SUNTO                               
016402     MOVE DLIN-IDPARTNR        TO  UT-IDPARTNR                            
016500     .                                                                    
016600     EJECT                                                                
016701 S11-WRITE-WF1046 SECTION.                                                
016800                                                                          
016901     WRITE UT-RECORD FROM UT-WF1046                                       
017000                                                                          
017100     .                                                                    
017200     EJECT                                                                
017300 Z-FINISH SECTION.                                                        
017401     CLOSE WF1046                                                         
017500     .                                                                    
017600     EJECT                                                                
017610* --- DB2 SECTIONS  ---                                                   
017620 DB2-DCL-OPN-T01LSEL-CRS SECTION.                                         
017630     MOVE 000100 TO GOOD-SQLCODES                                         
017640                                                                          
017650     EXEC SQL                                                             
017660         DECLARE LSEL-CRS CURSOR FOR                                      
017670            SELECT  IDLEGSEL                                              
017680                                                                          
017690            FROM     T01LSEL                                              
017691                                                                          
017692     END-EXEC                                                             
017693                                                                          
017694     MOVE 000100 TO GOOD-SQLCODES                                         
017695                                                                          
017696     EXEC SQL                                                             
017697        OPEN LSEL-CRS                                                     
017698     END-EXEC                                                             
017699                                                                          
017700     MOVE SQLCODE TO SQLCODE-WS                                           
017701     PERFORM DB2-STATUS-CHECK                                             
017702     .                                                                    
017703                                                                          
017704 DB2-FETCH-T01LSEL-CRS SECTION.                                           
017705     MOVE 000100  TO GOOD-SQLCODES                                        
017706                                                                          
017707     EXEC SQL                                                             
017708         FETCH LSEL-CRS                                                   
017709                                                                          
017710         INTO :LSEL-IDLEGSEL                                              
017711     END-EXEC                                                             
017712                                                                          
017713     MOVE SQLCODE TO SQLCODE-WS                                           
017714     PERFORM DB2-STATUS-CHECK                                             
017715     .                                                                    
017716 DB2-CLOSE-T01LSEL-CRS SECTION.                                           
017717                                                                          
017718     EXEC SQL                                                             
017719        CLOSE LSEL-CRS                                                    
017720     END-EXEC                                                             
017721     .                                                                    
017722                                                                          
017730 DB2-OPEN-CRS1-DLIN-DHEA SECTION.                                         
017800     MOVE 000100 TO GOOD-SQLCODES                                         
017900     EXEC SQL                                                             
018000        DECLARE CRS1 CURSOR FOR                                           
018101        SELECT  B.IDLEGSEL                                                
018102               ,A.IDFINDOC                                                
018201               ,A.DAFINDOC                                                
018202               ,B.IDFAKREF                                                
018203               ,B.DAFAKREF                                                
018204               ,B.IDLEGSEL                                                
018301               ,A.KDVALISO                                                
018401               ,B.IDEXCUST_1                                              
018501               ,B.IDEXCUST_2                                              
018601               ,B.IDREF                                                   
018701               ,B.DAREFDAT                                                
018801               ,B.IDACCNT_1                                               
018901               ,B.IDACCNT_2                                               
019001               ,B.BEVOLREF                                                
019101               ,B.IDARTNR_FINANCE                                         
019201               ,B.BEART                                                   
019301               ,B.KVLEVART                                                
019401               ,B.PRARTNTO                                                
019501               ,B.SUNTO                                                   
019502               ,B.IDPARTNR                                                
019600                                                                          
019701        FROM  T01DHEA A                                                   
019801             ,T01DLIN B                                                   
019900                                                                          
020102        WHERE (B.IDEXCUST_1   = '8497'                                    
020103        OR    B.IDEXCUST_1    = '8490'                                    
020104        OR    B.IDEXCUST_1    = '8480')                                   
020105        AND   A.IDLEGSEL      = :LSEL-IDLEGSEL                            
020201        AND   A.IDLEGSEL      = B.IDLEGSEL                                
020301        AND   A.DAEXDAT       = B.DAEXDAT                                 
020401        AND   A.TIEXTID       = B.TIEXTID                                 
020501        AND   A.KDVALISO      = B.KDVALISO                                
020601        AND   A.IDLANDX3_SEND = B.IDLANDX3_SEND                           
020701        AND   A.IDLEVNR       = B.IDLEVNR                                 
020801        AND   A.IDPARTNR      = B.IDPARTNR                                
020901        AND   A.KDFINDOC      = B.KDFINDOC                                
021001        AND   A.FLSOFT        = B.FLSOFT                                  
021101        AND   A.FLFREE        = B.FLFREE                                  
021201        AND   A.FLPRIV        = B.FLPRIV                                  
021301        AND   A.IDBREAK_1     = B.IDBREAK_1                               
021401        AND   A.IDBREAK_2     = B.IDBREAK_2                               
021402        AND   A.DAFINDOC      > :WS-RUNDATUM-FROM                         
021403        AND   A.DAFINDOC      < :WS-RUNDATUM-TO                           
021410                                                                          
021530     END-EXEC                                                             
021600                                                                          
021700     EXEC SQL                                                             
021800        OPEN CRS1                                                         
021900     END-EXEC                                                             
022000                                                                          
022100     MOVE SQLCODE        TO SQLCODE-WS                                    
022200     PERFORM DB2-STATUS-CHECK                                             
022300     .                                                                    
022400 DB2-FETCH-CRS1-DLIN-DHEA  SECTION.                                       
022500     MOVE 000100         TO GOOD-SQLCODES                                 
022600                                                                          
022700     EXEC SQL                                                             
022800              FETCH CRS1                                                  
022901              INTO  :DLIN-IDLEGSEL                                        
022902                  , :DHEA-IDFINDOC                                        
023001                  , :DHEA-DAFINDOC                                        
023101                  , :DLIN-IDFAKREF                                        
023102                  , :DLIN-DAFAKREF                                        
023103                  , :DLIN-IDLEGSEL                                        
023104                  , :DHEA-KDVALISO                                        
023201                  , :DLIN-IDEXCUST-1                                      
023301                  , :DLIN-IDEXCUST-2                                      
023401                  , :DLIN-IDREF                                           
023501                  , :DLIN-DAREFDAT                                        
023601                  , :DLIN-IDACCNT-1                                       
023701                  , :DLIN-IDACCNT-2                                       
023801                  , :DLIN-BEVOLREF                                        
023901                  , :DLIN-IDARTNR-FINANCE                                 
024001                  , :DLIN-BEART                                           
024101                  , :DLIN-KVLEVART                                        
024201                  , :DLIN-PRARTNTO                                        
024301                  , :DLIN-SUNTO                                           
024302                  , :DLIN-IDPARTNR                                        
024400     END-EXEC                                                             
024500                                                                          
024600     MOVE SQLCODE        TO SQLCODE-WS                                    
024700     PERFORM DB2-STATUS-CHECK                                             
024800     .                                                                    
024900 DB2-CLOSE-CRS1-DLIN-DHEA  SECTION.                                       
025000                                                                          
025100     EXEC SQL                                                             
025200        CLOSE CRS1                                                        
025300     END-EXEC                                                             
025310                                                                          
025320     MOVE SQLCODE        TO SQLCODE-WS                                    
025330     PERFORM DB2-STATUS-CHECK                                             
025400     .                                                                    
025410 DB2-OPEN-CRS2-DLIN-DHEA-ARC SECTION.                                     
025420     MOVE 000100 TO GOOD-SQLCODES                                         
025430     EXEC SQL                                                             
025440        DECLARE CRS2 CURSOR FOR                                           
025450        SELECT  B.IDLEGSEL                                                
025460               ,A.IDFINDOC                                                
025470               ,A.DAFINDOC                                                
025471               ,B.IDFAKREF                                                
025472               ,B.DAFAKREF                                                
025473               ,B.IDLEGSEL                                                
025480               ,A.KDVALISO                                                
025490               ,B.IDEXCUST_1                                              
025491               ,B.IDEXCUST_2                                              
025492               ,B.IDREF                                                   
025493               ,B.DAREFDAT                                                
025494               ,B.IDACCNT_1                                               
025495               ,B.IDACCNT_2                                               
025496               ,B.BEVOLREF                                                
025497               ,B.IDARTNR_FINANCE                                         
025498               ,B.BEART                                                   
025499               ,B.KVLEVART                                                
025500               ,B.PRARTNTO                                                
025501               ,B.SUNTO                                                   
025502               ,B.IDPARTNR                                                
025503                                                                          
025504        FROM  T01DHEA_ARC A                                               
025505             ,T01DLIN_ARC B                                               
025506                                                                          
025507        WHERE (B.IDEXCUST_1   = '8497'                                    
025508        OR    B.IDEXCUST_1    = '8490'                                    
025509        OR    B.IDEXCUST_1    = '8480')                                   
025510        AND   A.IDLEGSEL      = :LSEL-IDLEGSEL                            
025511        AND   A.IDLEGSEL      = B.IDLEGSEL                                
025512        AND   A.DAEXDAT       = B.DAEXDAT                                 
025513        AND   A.TIEXTID       = B.TIEXTID                                 
025514        AND   A.KDVALISO      = B.KDVALISO                                
025515        AND   A.IDLANDX3_SEND = B.IDLANDX3_SEND                           
025516        AND   A.IDLEVNR       = B.IDLEVNR                                 
025517        AND   A.IDPARTNR      = B.IDPARTNR                                
025518        AND   A.KDFINDOC      = B.KDFINDOC                                
025519        AND   A.FLSOFT        = B.FLSOFT                                  
025520        AND   A.FLFREE        = B.FLFREE                                  
025521        AND   A.FLPRIV        = B.FLPRIV                                  
025522        AND   A.IDBREAK_1     = B.IDBREAK_1                               
025523        AND   A.IDBREAK_2     = B.IDBREAK_2                               
025524        AND   A.DAFINDOC      > :WS-RUNDATUM-FROM                         
025525        AND   A.DAFINDOC      < :WS-RUNDATUM-TO                           
025529                                                                          
025530     END-EXEC                                                             
025531                                                                          
025532     EXEC SQL                                                             
025533        OPEN CRS2                                                         
025534     END-EXEC                                                             
025535                                                                          
025536     MOVE SQLCODE        TO SQLCODE-WS                                    
025537     PERFORM DB2-STATUS-CHECK                                             
025538     .                                                                    
025539 DB2-FETCH-CRS2-DLIN-DHEA-ARC  SECTION.                                   
025540     MOVE 000100         TO GOOD-SQLCODES                                 
025541                                                                          
025542     EXEC SQL                                                             
025543              FETCH CRS2                                                  
025544              INTO  :DLIN-IDLEGSEL                                        
025545                  , :DHEA-IDFINDOC                                        
025546                  , :DHEA-DAFINDOC                                        
025547                  , :DLIN-IDFAKREF                                        
025548                  , :DLIN-DAFAKREF                                        
025549                  , :DLIN-IDLEGSEL                                        
025550                  , :DHEA-KDVALISO                                        
025551                  , :DLIN-IDEXCUST-1                                      
025552                  , :DLIN-IDEXCUST-2                                      
025553                  , :DLIN-IDREF                                           
025554                  , :DLIN-DAREFDAT                                        
025555                  , :DLIN-IDACCNT-1                                       
025556                  , :DLIN-IDACCNT-2                                       
025557                  , :DLIN-BEVOLREF                                        
025558                  , :DLIN-IDARTNR-FINANCE                                 
025559                  , :DLIN-BEART                                           
025560                  , :DLIN-KVLEVART                                        
025561                  , :DLIN-PRARTNTO                                        
025562                  , :DLIN-SUNTO                                           
025563                  , :DLIN-IDPARTNR                                        
025564     END-EXEC                                                             
025565                                                                          
025566     MOVE SQLCODE        TO SQLCODE-WS                                    
025567     PERFORM DB2-STATUS-CHECK                                             
025568     .                                                                    
025569 DB2-CLOSE-CRS2-DLIN-DHEA-ARC  SECTION.                                   
025570                                                                          
025571     EXEC SQL                                                             
025572        CLOSE CRS2                                                        
025573     END-EXEC                                                             
025574                                                                          
025575     MOVE SQLCODE        TO SQLCODE-WS                                    
025576     PERFORM DB2-STATUS-CHECK                                             
025577     .                                                                    
025578 DB2-OPEN-CRS3-DLIN-DHEA SECTION.                                         
025579     MOVE 000100 TO GOOD-SQLCODES                                         
025580     EXEC SQL                                                             
025581        DECLARE CRS3 CURSOR FOR                                           
025582        SELECT  B.IDLEGSEL                                                
025583               ,A.IDFINDOC                                                
025584               ,A.DAFINDOC                                                
025585               ,B.IDFAKREF                                                
025586               ,B.DAFAKREF                                                
025587               ,B.IDLEGSEL                                                
025588               ,A.KDVALISO                                                
025589               ,B.IDEXCUST_1                                              
025590               ,B.IDEXCUST_2                                              
025591               ,B.IDREF                                                   
025592               ,B.DAREFDAT                                                
025593               ,B.IDACCNT_1                                               
025594               ,B.IDACCNT_2                                               
025595               ,B.BEVOLREF                                                
025596               ,B.IDARTNR_FINANCE                                         
025597               ,B.BEART                                                   
025598               ,B.KVLEVART                                                
025599               ,B.PRARTNTO                                                
025600               ,B.SUNTO                                                   
025601               ,B.IDPARTNR                                                
025602                                                                          
025603        FROM  T01DHEA A                                                   
025604             ,T01DLIN B                                                   
025605                                                                          
025606        WHERE B.IDLEGSEL  = 'VCCS'                                        
025607        AND  (((B.IDEXCUST_1  = '8497'                                    
025608        OR    B.IDEXCUST_1    = '8490'                                    
025609        OR    B.IDEXCUST_1    = '8480')                                   
025610        AND  (B.IDPARTNR      = '30957'                                   
025611        OR    B.IDPARTNR      = '4113'                                    
025612        OR    B.IDPARTNR      = '212321'                                  
025613        OR    B.IDPARTNR      = '352343'                                  
025614        OR    B.IDPARTNR      = '119669'))                                
025615        OR    B.IDEXCUST_2    = '999')                                    
025616        AND   A.IDLEGSEL      = :LSEL-IDLEGSEL                            
025617        AND   A.IDLEGSEL      = B.IDLEGSEL                                
025618        AND   A.DAEXDAT       = B.DAEXDAT                                 
025619        AND   A.TIEXTID       = B.TIEXTID                                 
025620        AND   A.KDVALISO      = B.KDVALISO                                
025621        AND   A.IDLANDX3_SEND = B.IDLANDX3_SEND                           
025622        AND   A.IDLEVNR       = B.IDLEVNR                                 
025623        AND   A.IDPARTNR      = B.IDPARTNR                                
025624        AND   A.KDFINDOC      = B.KDFINDOC                                
025625        AND   A.FLSOFT        = B.FLSOFT                                  
025626        AND   A.FLFREE        = B.FLFREE                                  
025627        AND   A.FLPRIV        = B.FLPRIV                                  
025628        AND   A.IDBREAK_1     = B.IDBREAK_1                               
025629        AND   A.IDBREAK_2     = B.IDBREAK_2                               
025630        AND   A.DAFINDOC      > :WS-RUNDATUM-FROM                         
025631        AND   A.DAFINDOC      < :WS-RUNDATUM-TO                           
025632                                                                          
025633     END-EXEC                                                             
025634                                                                          
025635     EXEC SQL                                                             
025636        OPEN CRS3                                                         
025637     END-EXEC                                                             
025638                                                                          
025639     MOVE SQLCODE        TO SQLCODE-WS                                    
025640     PERFORM DB2-STATUS-CHECK                                             
025641     .                                                                    
025642 DB2-FETCH-CRS3-DLIN-DHEA  SECTION.                                       
025643     MOVE 000100         TO GOOD-SQLCODES                                 
025644                                                                          
025645     EXEC SQL                                                             
025646              FETCH CRS3                                                  
025647              INTO  :DLIN-IDLEGSEL                                        
025648                  , :DHEA-IDFINDOC                                        
025649                  , :DHEA-DAFINDOC                                        
025650                  , :DLIN-IDFAKREF                                        
025651                  , :DLIN-DAFAKREF                                        
025652                  , :DLIN-IDLEGSEL                                        
025653                  , :DHEA-KDVALISO                                        
025654                  , :DLIN-IDEXCUST-1                                      
025655                  , :DLIN-IDEXCUST-2                                      
025656                  , :DLIN-IDREF                                           
025657                  , :DLIN-DAREFDAT                                        
025658                  , :DLIN-IDACCNT-1                                       
025659                  , :DLIN-IDACCNT-2                                       
025660                  , :DLIN-BEVOLREF                                        
025661                  , :DLIN-IDARTNR-FINANCE                                 
025662                  , :DLIN-BEART                                           
025663                  , :DLIN-KVLEVART                                        
025664                  , :DLIN-PRARTNTO                                        
025665                  , :DLIN-SUNTO                                           
025666                  , :DLIN-IDPARTNR                                        
025667     END-EXEC                                                             
025668                                                                          
025669     MOVE SQLCODE        TO SQLCODE-WS                                    
025670     PERFORM DB2-STATUS-CHECK                                             
025671     .                                                                    
025672 DB2-CLOSE-CRS3-DLIN-DHEA  SECTION.                                       
025673                                                                          
025674     EXEC SQL                                                             
025675        CLOSE CRS3                                                        
025676     END-EXEC                                                             
025677                                                                          
025678     MOVE SQLCODE        TO SQLCODE-WS                                    
025679     PERFORM DB2-STATUS-CHECK                                             
025680     .                                                                    
025681 DB2-OPEN-CRS4-DLIN-DHEA-ARC SECTION.                                     
025682     MOVE 000100 TO GOOD-SQLCODES                                         
025683     EXEC SQL                                                             
025684        DECLARE CRS4 CURSOR FOR                                           
025685        SELECT  B.IDLEGSEL                                                
025686               ,A.IDFINDOC                                                
025687               ,A.DAFINDOC                                                
025688               ,B.IDFAKREF                                                
025689               ,B.DAFAKREF                                                
025690               ,B.IDLEGSEL                                                
025691               ,A.KDVALISO                                                
025692               ,B.IDEXCUST_1                                              
025693               ,B.IDEXCUST_2                                              
025694               ,B.IDREF                                                   
025695               ,B.DAREFDAT                                                
025696               ,B.IDACCNT_1                                               
025697               ,B.IDACCNT_2                                               
025698               ,B.BEVOLREF                                                
025699               ,B.IDARTNR_FINANCE                                         
025700               ,B.BEART                                                   
025701               ,B.KVLEVART                                                
025702               ,B.PRARTNTO                                                
025703               ,B.SUNTO                                                   
025704               ,B.IDPARTNR                                                
025705                                                                          
025706        FROM  T01DHEA_ARC A                                               
025707             ,T01DLIN_ARC B                                               
025708                                                                          
025709        WHERE B.IDLEGSEL  = 'VCCS'                                        
025710        AND  (((B.IDEXCUST_1  = '8497'                                    
025711        OR    B.IDEXCUST_1    = '8490'                                    
025712        OR    B.IDEXCUST_1    = '8480')                                   
025713        AND  (B.IDPARTNR      = '30957'                                   
025714        OR    B.IDPARTNR      = '4113'                                    
025715        OR    B.IDPARTNR      = '212321'                                  
025716        OR    B.IDPARTNR      = '352343'                                  
025717        OR    B.IDPARTNR      = '119669'))                                
025718        OR    B.IDEXCUST_2    = '999')                                    
025719        AND   A.IDLEGSEL      = :LSEL-IDLEGSEL                            
025720        AND   A.IDLEGSEL      = B.IDLEGSEL                                
025721        AND   A.DAEXDAT       = B.DAEXDAT                                 
025722        AND   A.TIEXTID       = B.TIEXTID                                 
025723        AND   A.KDVALISO      = B.KDVALISO                                
025724        AND   A.IDLANDX3_SEND = B.IDLANDX3_SEND                           
025725        AND   A.IDLEVNR       = B.IDLEVNR                                 
025726        AND   A.IDPARTNR      = B.IDPARTNR                                
025727        AND   A.KDFINDOC      = B.KDFINDOC                                
025728        AND   A.FLSOFT        = B.FLSOFT                                  
025729        AND   A.FLFREE        = B.FLFREE                                  
025730        AND   A.FLPRIV        = B.FLPRIV                                  
025731        AND   A.IDBREAK_1     = B.IDBREAK_1                               
025732        AND   A.IDBREAK_2     = B.IDBREAK_2                               
025733        AND   A.DAFINDOC      > :WS-RUNDATUM-FROM                         
025734        AND   A.DAFINDOC      < :WS-RUNDATUM-TO                           
025735                                                                          
025736     END-EXEC                                                             
025737                                                                          
025738     EXEC SQL                                                             
025739        OPEN CRS4                                                         
025740     END-EXEC                                                             
025741                                                                          
025742     MOVE SQLCODE        TO SQLCODE-WS                                    
025743     PERFORM DB2-STATUS-CHECK                                             
025744     .                                                                    
025745 DB2-FETCH-CRS4-DLIN-DHEA-ARC  SECTION.                                   
025746     MOVE 000100         TO GOOD-SQLCODES                                 
025747                                                                          
025748     EXEC SQL                                                             
025749              FETCH CRS4                                                  
025750              INTO  :DLIN-IDLEGSEL                                        
025751                  , :DHEA-IDFINDOC                                        
025752                  , :DHEA-DAFINDOC                                        
025753                  , :DLIN-IDFAKREF                                        
025754                  , :DLIN-DAFAKREF                                        
025755                  , :DLIN-IDLEGSEL                                        
025756                  , :DHEA-KDVALISO                                        
025757                  , :DLIN-IDEXCUST-1                                      
025758                  , :DLIN-IDEXCUST-2                                      
025759                  , :DLIN-IDREF                                           
025760                  , :DLIN-DAREFDAT                                        
025761                  , :DLIN-IDACCNT-1                                       
025762                  , :DLIN-IDACCNT-2                                       
025763                  , :DLIN-BEVOLREF                                        
025764                  , :DLIN-IDARTNR-FINANCE                                 
025765                  , :DLIN-BEART                                           
025766                  , :DLIN-KVLEVART                                        
025767                  , :DLIN-PRARTNTO                                        
025768                  , :DLIN-SUNTO                                           
025769                  , :DLIN-IDPARTNR                                        
025770     END-EXEC                                                             
025771                                                                          
025772     MOVE SQLCODE        TO SQLCODE-WS                                    
025773     PERFORM DB2-STATUS-CHECK                                             
025774     .                                                                    
025775 DB2-CLOSE-CRS4-DLIN-DHEA-ARC  SECTION.                                   
025776                                                                          
025777     EXEC SQL                                                             
025778        CLOSE CRS4                                                        
025779     END-EXEC                                                             
025780                                                                          
025781     MOVE SQLCODE        TO SQLCODE-WS                                    
025782     PERFORM DB2-STATUS-CHECK                                             
025783     .                                                                    
025784 DB2-STATUS-CHECK  SECTION.                                               
025785                                                                          
025790     SET SQLCODE-IX TO 1                                                  
025800     SEARCH GOOD-SQLCODE                                                  
025900       AT END                                                             
026000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
026100          DELIMITED BY SIZE INTO ERROR-TEXT                               
026200          CALL ABEND USING RKOD-ABEND-DB2                                 
026300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
026400     END-SEARCH                                                           
027000     .                                                                    
