000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4617200.                                                 
001000*AUTHOR.        ELAINE CURTSSON.                                          
001100*DATE-WRITTEN.  MAJ      1991.                                            
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (SCHWEIZ-PV)                        
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*                                                                         
002100*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002200*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002300*        (ENDAST ENGELSK TEXT).                                           
002400*                                                                         
002410*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*- - - - - - - - - - - - INFIL:                                           
003300*                        - -  FIL TILL VIPS                               
003400     SELECT W46172                       ASSIGN TO UT-S-W46172D1.         
003500     SKIP2                                                                
003600*- - - - - - - - - - - - UTFIL:                                           
003700*                        - -  FIL TILL VIPS                               
003800     SELECT W46192                       ASSIGN TO UT-S-W46172D2.         
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W46172                                                               
004600     RECORDING      V                                                     
004700     BLOCK CONTAINS 0.                                                    
004800     SKIP2                                                                
004810 01  INPOST          PIC X(211).                                          
004820     SKIP2                                                                
005600 FD  W46192                                                               
005700     RECORDING      V                                                     
005800     BLOCK CONTAINS 0.                                                    
005900 01  UTPOST                       PIC X(80).                              
005910     SKIP2                                                                
005920 01  RID-POST  -COPY W461RIDN     -L.                                     
005930     SKIP2                                                                
005940 01  RIE-POST  -COPY W461RIEN     -L.                                     
005950     SKIP2                                                                
005960 01  RIH-POST  -COPY W461RIHN     -L.                                     
005970     SKIP2                                                                
005980 01  RIO-POST  -COPY W461RIO2     -L.                                     
005990     SKIP2                                                                
005991 01  RKB-POST  -COPY W461RKBN     -L.                                     
005992     SKIP2                                                                
005993 01  RKC-POST  -COPY W461RKCN     -L.                                     
005994     SKIP2                                                                
005995 01  RKD-POST  -COPY W461RKDN     -L.                                     
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006110                                                                          
006200*    -- CHECKED BY WY2000                                                 
006700*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006800 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617200'.            
007000     SKIP2                                                                
007100*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007200                                                                          
007300 77  JA                          PIC X(1)    VALUE 'J'.                   
007400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007500     SKIP2                                                                
007600*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007700                                                                          
007800 77  W46172-EOF                  PIC X(1)    VALUE 'N'.                   
007900     SKIP2                                                                
008000*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008100                                                                          
008200 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008300 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008400                                                                          
008500*- - - - - - - - - - - - - -                                              
008600                                                                          
008700     EJECT                                                                
008800 01  DAGENS-DATUM.                                                        
008900   03  DAGENS-DATUM-AR           PIC 9(2).                                
009000   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009100   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009200                                                                          
009300 01  DAGENS-TID.                                                          
009400   03  DAGENS-TID-TIM            PIC 9(2).                                
009500   03  DAGENS-TID-MIN            PIC 9(2).                                
009600   03  DAGENS-TID-SEK            PIC 9(2).                                
009610*      --- VALID IDDC CODES                                               
009620*                                                                         
009630*01    -COPY WWDCKONS                                                     
009640       EJECT                                                              
009700                                                                          
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010000   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010200     SKIP3                                                                
010300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010400                                                                          
010500 01  RETURKODER.                                                          
010600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010900     EJECT                                                                
011000*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011100                                                                          
011200******************************************************************        
011300                                                                          
011400 01  W-ARBAREA.                                                           
011500 03  W-ARBAREA-X                 PIC X(211).                              
011600     SKIP2                                                                
011700*                                                                         
011792*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011793     EJECT                                                                
012006                                                                          
012007*01  -COPY W461RIFN        -PRE JFR-.                                     
012010     EJECT                                                                
012100*                             STARTKORT                                   
012200*01  -COPY W461RI0N                                                       
012400     EJECT                                                                
012500*                             SLUTKORT                                    
012600*01  -COPY W461RI9                                                        
012800     EJECT                                                                
012900*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013000                                                                          
013100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013200     SKIP2                                                                
013300*01  -COPY WDATKORT                                                       
013500     EJECT                                                                
013600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013700                                                                          
013800*01  -COPY W0005       -PRE  POSTSUM-.                                    
014000     EJECT                                                                
014100 PROCEDURE DIVISION.                                                      
014200     SKIP2                                                                
014300     PERFORM A-INIT                                                       
014400     PERFORM B-BEHANDLA                                                   
014500     PERFORM Z-FINIT                                                      
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900                                                                          
015000     SKIP3                                                                
015100 A-INIT SECTION.                                                          
015200     SKIP2                                                                
015300     OPEN INPUT W46172 OUTPUT W46192                                      
015400     SKIP2                                                                
015500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015600     SKIP2                                                                
015700*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015900     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016000     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016100     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016200     SKIP2                                                                
016300*- - - - - - - - - - - - - - - - TID                                      
016400*                                                                         
016500     ACCEPT   DAGENS-TID FROM TIME                                        
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 B-BEHANDLA SECTION.                                                      
017000     SKIP2                                                                
017100     PERFORM BA-START-KORT                                                
017200     PERFORM S01-LAS-W46172                                               
017300     PERFORM UNTIL                                                        
017400      NOT ( W46172-EOF = NEJ )                                            
017500       IF W-RIF-IDPTYP  = 'RIF'                                           
017600         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017700         PERFORM S01-LAS-W46172                                           
017800       ELSE                                                               
017900         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018000           ADD     +1           TO W-ANT-POSTER                           
018100           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
018300           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018400         END-IF                                                           
018500         IF W-RIF-IDPTYP = 'RIC'                                          
018600           PERFORM S01-LAS-W46172                                         
018700         ELSE                                                             
018710           EVALUATE    W-RIF-IDPTYP                                       
018720                                                                          
018730             WHEN  'RID'                                                  
018740                  WRITE RID-POST FROM INPOST                              
018750                                                                          
018760             WHEN  'RIE'                                                  
018770                  WRITE RIE-POST FROM INPOST                              
018780                                                                          
018790             WHEN  'RIH'                                                  
018800                  WRITE RIH-POST FROM INPOST                              
018810                                                                          
018820             WHEN  'RIO'                                                  
018830                  WRITE RIO-POST FROM INPOST                              
018840                                                                          
018841             WHEN  'RKB'                                                  
018842                  WRITE RKB-POST FROM INPOST                              
018843                                                                          
018844             WHEN  'RKC'                                                  
018845                  WRITE RKC-POST FROM INPOST                              
018846                                                                          
018847             WHEN  'RKD'                                                  
018848                  WRITE RKD-POST FROM INPOST                              
018849                                                                          
018850             WHEN  OTHER                                                  
018860                  WRITE UTPOST FROM INPOST                                
018885                                                                          
018886           END-EVALUATE                                                   
018887           ADD     +1           TO W-ANT-POSTER                           
018889           PERFORM S01-LAS-W46172                                         
018890         END-IF                                                           
018891       END-IF                                                             
018892     END-PERFORM                                                          
018893     PERFORM BB-SLUT-KORT                                                 
018894     .                                                                    
018895     EJECT                                                                
018896                                                                          
019800 BA-START-KORT SECTION.                                                   
019900     SKIP2                                                                
020000     MOVE     'RI0'          TO START-IDPTYP                              
020100     MOVE     2078           TO START-IDDISTR                             
020200     MOVE     WC-CDC-SE      TO START-IDDC                                
020300     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020400*                                                                         
020500     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020600*                                                                         
020700     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
020800     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021000     .                                                                    
021100     SKIP2                                                                
021200 BB-SLUT-KORT SECTION.                                                    
021300     SKIP2                                                                
021400     MOVE     'RI9'          TO SLUT-IDPTYP                               
021500     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021600*                                                                         
021700     WRITE    UTPOST         FROM  SLUT-W461RI9                           
021900     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022000     .                                                                    
022100     SKIP2                                                                
022200 S01-LAS-W46172 SECTION.                                                  
022300     SKIP2                                                                
022400     READ   W46172 INTO W-ARBAREA                                         
022500     AT END MOVE JA TO W46172-EOF                                         
022600     END-READ                                                             
022700                                                                          
022800     IF W46172-EOF = NEJ                                                  
022900                                                                          
023000       MOVE 'W46172'            TO POSTSUM-FDNAMN                         
023100       MOVE 'W46172D1'          TO POSTSUM-DDNAMN2                        
023200       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023300       CALL POSTSUM             USING POSTSUM-PARM                        
023400                                                                          
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 Z-FINIT SECTION.                                                         
023900     SKIP2                                                                
024000                                                                          
024100     CLOSE W46172 W46192                                                  
024200     SKIP2                                                                
024300*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024400*                                    SKRIVNA POSTER                       
024500                                                                          
024600     MOVE 'S' TO POSTSUM-OPKOD                                            
024700     CALL POSTSUM USING POSTSUM-PARM                                      
024800     .                                                                    
