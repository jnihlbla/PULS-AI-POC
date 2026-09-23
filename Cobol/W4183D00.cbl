000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4183D00.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   08/09/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET UPPDATERAR DB2-TABELL TP8LRET MED FAKTURAINFO         
001100*        FÖR HANDLING FEE RADER SOM ÄR FAKTURERADE I BILL-IT.             
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR TABELL TP8LRET                             
001400*                                                                         
001500*                                                                         
001600*    E'TRACKER 880053 DATED 20080903                                      
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- UPPDATERINGSRADER FRÅN W4183C00,HANDLING FEE               
002700     SELECT W418C1                     ASSIGN TO W4183DD1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W418C1                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W418C1      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W4183D00'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 77  W-W418C1-KVPOST-IN          PIC S9(3)   VALUE +0  COMP-3.            
004600     SKIP2                                                                
004610                                                                          
004700 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100 01  W-COUNTC                    PIC S9(7)  VALUE ZERO COMP-3.            
005200 01  W-COUNT1                    PIC S9(7)  VALUE ZERO COMP-3.            
005300                                                                          
005400 77  W418C1-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W418C1                       VALUE 'J'.                   
005600     EJECT                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300                                                                          
007400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007500 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
007700 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
007900     SKIP2                                                                
008000                                                                          
008100     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W418C1     -PRE IN-                                       
008700*                                                                         
008800     EJECT                                                                
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009400 01  DB2-WS.                                                              
009500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009600         88  CURSOR-OK                       VALUE 000.                   
009700         88  RADER-FINNS                     VALUE 000.                   
009800         88  RADER-SAKNAS                    VALUE 100.                   
009810         88  DUBBLETTER-FINNS                VALUE 811.                   
009900         88  ATKOMST-FEL                     VALUE 904.                   
010000                                                                          
010100     03  GODK-SQLCODEKODER.                                               
010200         05  GODK-SQLCODE OCCURS 5                                        
010300             INDEXED BY SQLCODE-IX PIC 9(3).                              
010400                                                                          
010500*    ---  DB2 HOST-COPYTEXTER                                             
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-AREA'.         
010800*01  -COPY TP8LRET -PRE LRET-                                             
010900     EJECT                                                                
011000                                                                          
011100 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-DCL '.         
011200     EXEC SQL INCLUDE TP8LRET END-EXEC.                                   
011300     EJECT                                                                
011400                                                                          
011500 LINKAGE SECTION.                                                         
011600 PROCEDURE DIVISION.                                                      
011700 MAIN SECTION.                                                            
011800                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012100     PERFORM S01-LAES-W418C1                                              
012200     PERFORM UNTIL END-OF-W418C1                                          
012300                                                                          
012400       PERFORM B-BEHANDLA                                                 
012500                                                                          
012600       PERFORM S01-LAES-W418C1                                            
012700     END-PERFORM                                                          
012800                                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     DISPLAY 'ANTAL COMMIT ' W-COUNT1                                     
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014100     OPEN INPUT W418C1                                                    
014200                                                                          
014300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014400     MOVE ZERO  TO W-COUNTC                                               
014500                                                                          
014600     INITIALIZE GODK-SQLCODEKODER                                         
014700     .                                                                    
014800     EJECT                                                                
014900 B-BEHANDLA  SECTION.                                                     
015000                                                                          
015100     IF W-COUNTC > 100                                                    
015200       PERFORM DB2-COMMIT-WORK                                            
015300     END-IF                                                               
015400                                                                          
015500     PERFORM  DB2-SELECT-TP8LRET-TAB                                      
015600     IF RADER-FINNS                                                       
015700       MOVE IN-DAFINDOC  TO LRET-DAFAKT                                   
015800       MOVE IN-IDFINDOC  TO LRET-IDFINDOC                                 
015810       MOVE IN-PRARTBTO  TO LRET-PRARTBTO                                 
015820       MOVE IN-PRARTBTO  TO LRET-PRARTNTO                                 
015830       MOVE IN-KDVALISO  TO LRET-KDVALISO                                 
015900       IF LRET-KDRAPPSTA = 'MS' OR 'MF'                                   
016000         MOVE 'MF'         TO LRET-KDRAPPSTA                              
016100       ELSE                                                               
016200         MOVE 'SF'         TO LRET-KDRAPPSTA                              
016300       END-IF                                                             
016400                                                                          
016500       PERFORM DB2-UPDATE-TP8LRET-TAB                                     
016600                                                                          
016700       ADD +1     TO W-COUNTC                                             
016710     ELSE                                                                 
016720       IF DUBBLETTER-FINNS                                                
016721         PERFORM DB2-OPEN-TP8LRET                                         
016722         PERFORM DB2-FETCH-TP8LRET                                        
016723         PERFORM UNTIL RADER-SAKNAS                                       
016724           MOVE IN-DAFINDOC  TO LRET-DAFAKT                               
016725           MOVE IN-IDFINDOC  TO LRET-IDFINDOC                             
016726           MOVE IN-PRARTBTO  TO LRET-PRARTBTO                             
016727           MOVE IN-PRARTBTO  TO LRET-PRARTNTO                             
016728           MOVE IN-KDVALISO  TO LRET-KDVALISO                             
016729           IF LRET-KDRAPPSTA = 'MS' OR 'MF'                               
016730             MOVE 'MF'         TO LRET-KDRAPPSTA                          
016731           ELSE                                                           
016732             MOVE 'SF'         TO LRET-KDRAPPSTA                          
016733           END-IF                                                         
016734                                                                          
016735           PERFORM DB2-UPDATE-TP8LRET-FETCH                               
016736                                                                          
016737           ADD +1     TO W-COUNTC                                         
016738           PERFORM DB2-FETCH-TP8LRET                                      
016739         END-PERFORM                                                      
016740         PERFORM DB2-CLOSE-TP8LRET                                        
016750       END-IF                                                             
016800     END-IF                                                               
016900                                                                          
017000     .                                                                    
017100     EJECT                                                                
017200 Z-FINIT SECTION.                                                         
017300                                                                          
017400                                                                          
017500     CLOSE W418C1                                                         
017600     SKIP2                                                                
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018000     EJECT                                                                
018100 S01-LAES-W418C1  SECTION.                                                
018200     SKIP2                                                                
018300     READ W418C1 INTO IN-AREA                                             
018400     AT END                                                               
018500*SO?    MOVE HIGH-VALUE TO IN-AREA                                        
018600        SET END-OF-W418C1 TO TRUE                                         
018700                                                                          
018800     NOT AT END                                                           
018900        MOVE 'W418C1'   TO POSTSUM-FDNAMN                                 
019000        MOVE 'W4183DD1' TO POSTSUM-DDNAMN2                                
019100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
019200        CALL POSTSUM USING POSTSUM-PARM                                   
019300                                                                          
019400        ADD 1 TO W-W418C1-KVPOST-IN                                       
019500     END-READ                                                             
019600     .                                                                    
019700     EJECT                                                                
019800* --- DB2 SEKTIONER ---                                                   
019900                                                                          
020000     EJECT                                                                
020100 DB2-SELECT-TP8LRET-TAB  SECTION.                                         
020200                                                                          
020300     EXEC SQL                                                             
020400         SELECT  DAFAKT                                                   
020500                ,IDFINDOC                                                 
020600                ,KDRAPPSTA                                                
020700                                                                          
020800                                                                          
020900         INTO   :LRET-DAFAKT                                              
021000               ,:LRET-IDFINDOC                                            
021100               ,:LRET-KDRAPPSTA                                           
021200                                                                          
021300                                                                          
021400         FROM    TP8LRET                                                  
021500                                                                          
021600         WHERE   IDPARTNR = :IN-IDPARTNR                                  
021700           AND   KDANMORS = :IN-KDANMORS                                  
021900           AND   IDREF    = :IN-IDREF                                     
022000           AND   IDEXCUST_1 = :IN-IDEXCUST-1                              
022100           AND   IDEXCUST_2 = :IN-IDEXCUST-2                              
022200                                                                          
022300     END-EXEC                                                             
022400                                                                          
022500     MOVE 000100811  TO GODK-SQLCODEKODER                                 
022600     MOVE SQLCODE TO SQLCODE-WS                                           
022700     PERFORM DB2-STATUS-KONTROLL                                          
022800     .                                                                    
022900     EJECT                                                                
023000 DB2-UPDATE-TP8LRET-TAB  SECTION.                                         
023100                                                                          
023200     EXEC SQL                                                             
023300         UPDATE TP8LRET                                                   
023400                                                                          
023500         SET DAFAKT    = :LRET-DAFAKT                                     
023600            ,IDFINDOC  = :LRET-IDFINDOC                                   
023700            ,KDRAPPSTA = :LRET-KDRAPPSTA                                  
023710            ,PRARTBTO  = :LRET-PRARTBTO                                   
023720            ,PRARTNTO  = :LRET-PRARTNTO                                   
023730            ,KDVALISO  = :LRET-KDVALISO                                   
023800                                                                          
023900         WHERE   IDPARTNR   = :IN-IDPARTNR                                
024000           AND   KDANMORS   = :IN-KDANMORS                                
024200           AND   IDREF      = :IN-IDREF                                   
024300           AND   IDEXCUST_1 = :IN-IDEXCUST-1                              
024400           AND   IDEXCUST_2 = :IN-IDEXCUST-2                              
024500                                                                          
024600     END-EXEC                                                             
024700                                                                          
024800     MOVE 000100  TO GODK-SQLCODEKODER                                    
024900     MOVE SQLCODE TO SQLCODE-WS                                           
025000     PERFORM DB2-STATUS-KONTROLL                                          
025100     .                                                                    
025200     EJECT                                                                
025210                                                                          
025220 DB2-COMMIT-WORK  SECTION.                                                
025500     MOVE ZERO                       TO GODK-SQLCODE (1)                  
025600     MOVE ZERO                       TO W-COUNTC                          
025700     ADD +1 TO W-COUNT1                                                   
025800                                                                          
025900     EXEC SQL                                                             
026000            COMMIT WORK                                                   
026100     END-EXEC                                                             
026200     MOVE SQLCODE TO SQLCODE-WS                                           
026300     PERFORM DB2-STATUS-KONTROLL                                          
026400     .                                                                    
026500     EJECT                                                                
026501                                                                          
026510 DB2-OPEN-TP8LRET          SECTION.                                       
026520     EXEC SQL                                                             
026530         DECLARE TP8LRET-CRS CURSOR FOR                                   
026540           SELECT   DAFAKT                                                
026541                   ,IDFINDOC                                              
026542                   ,KDRAPPSTA                                             
026543                   ,DAREGDAT                                              
026544                                                                          
026550           FROM    TP8LRET                                                
026552                                                                          
026553           WHERE   IDPARTNR   = :IN-IDPARTNR                              
026554             AND   KDANMORS   = :IN-KDANMORS                              
026555             AND   IDREF      = :IN-IDREF                                 
026556             AND   IDEXCUST_1 = :IN-IDEXCUST-1                            
026557             AND   IDEXCUST_2 = :IN-IDEXCUST-2                            
026560                                                                          
026561           ORDER BY                                                       
026562                  DAREGDAT                                                
026565     END-EXEC                                                             
026566                                                                          
026567     MOVE 000100  TO GODK-SQLCODEKODER                                    
026568     EXEC SQL OPEN TP8LRET-CRS END-EXEC                                   
026569     .                                                                    
026570     EJECT                                                                
026580                                                                          
026590 DB2-FETCH-TP8LRET          SECTION.                                      
026591     EXEC SQL                                                             
026592         FETCH TP8LRET-CRS INTO                                           
026593             :LRET-DAFAKT                                                 
026594            ,:LRET-IDFINDOC                                               
026595            ,:LRET-KDRAPPSTA                                              
026596            ,:LRET-DAREGDAT                                               
026597     END-EXEC                                                             
026598                                                                          
026599     MOVE 000100  TO GODK-SQLCODEKODER                                    
026600     MOVE SQLCODE TO SQLCODE-WS                                           
026601     PERFORM DB2-STATUS-KONTROLL                                          
026602     .                                                                    
026603     EJECT                                                                
026604                                                                          
026605 DB2-CLOSE-TP8LRET          SECTION.                                      
026606     EXEC SQL CLOSE TP8LRET-CRS END-EXEC                                  
026607     .                                                                    
026608     EJECT                                                                
026609                                                                          
026610 DB2-UPDATE-TP8LRET-FETCH SECTION.                                        
026611     EXEC SQL                                                             
026612         UPDATE TP8LRET                                                   
026613         SET  DAFAKT    = :LRET-DAFAKT                                    
026614             ,IDFINDOC  = :LRET-IDFINDOC                                  
026615             ,KDRAPPSTA = :LRET-KDRAPPSTA                                 
026616             ,PRARTBTO  = :LRET-PRARTBTO                                  
026617             ,PRARTNTO  = :LRET-PRARTNTO                                  
026618             ,KDVALISO  = :LRET-KDVALISO                                  
026619                                                                          
026620         WHERE   IDPARTNR   = :IN-IDPARTNR                                
026621           AND   KDANMORS   = :IN-KDANMORS                                
026622           AND   IDREF      = :IN-IDREF                                   
026623           AND   IDEXCUST_1 = :IN-IDEXCUST-1                              
026624           AND   IDEXCUST_2 = :IN-IDEXCUST-2                              
026625           AND   DAREGDAT   = :LRET-DAREGDAT                              
026626     END-EXEC                                                             
026627                                                                          
026628     MOVE 000     TO GODK-SQLCODEKODER                                    
026629     MOVE SQLCODE TO SQLCODE-WS                                           
026630     PERFORM DB2-STATUS-KONTROLL                                          
026631     .                                                                    
026632     EJECT                                                                
026633                                                                          
026640 DB2-STATUS-KONTROLL  SECTION.                                            
026700                                                                          
026800     SET SQLCODE-IX TO 1                                                  
026900     SEARCH GODK-SQLCODE                                                  
027000       AT END                                                             
027100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
027200          DELIMITED BY SIZE INTO FELTEXT                                  
027300          CALL ABEND USING RKOD-ABEND-DB2                                 
027400       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
027500     END-SEARCH                                                           
027600     .                                                                    
