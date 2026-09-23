000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4616400.                                                 
001000*AUTHOR.        CARINA VIKTORSSON.                                        
001100*DATE-WRITTEN.  NOV  1987.                                                
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (SPANIEN)                           
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIS OCH RKA  SKRIVS                                           
002100*                                                                         
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (SPANSK/ENGELSK TEXT)                                            
002500*                                                                         
002510*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46179                       ASSIGN TO UT-S-W46164D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W46199                       ASSIGN TO UT-S-W46164D2.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46179                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
004910 01  INPOST          PIC X(211).                                          
004920     SKIP2                                                                
005250 FD  W46199                                                               
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700 01  RID-POST  -COPY W461RIDN      -L.                                    
005800     SKIP2                                                                
005900 01  RIE-POST  -COPY W461RIEN      -L.                                    
006000     SKIP2                                                                
006010 01  RIH-POST  -COPY W461RIHN      -L.                                    
006020     SKIP2                                                                
006030 01  RIO-POST  -COPY W461RIO2      -L.                                    
006040     SKIP2                                                                
006050 01  RKB-POST  -COPY W461RKBN     -L.                                     
006060     SKIP2                                                                
006070 01  RKC-POST  -COPY W461RKCN     -L.                                     
006080     SKIP2                                                                
006090 01  RKD-POST  -COPY W461RKDN     -L.                                     
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4616400'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46179-EOF                  PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008200                                                                          
008300 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008400 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008500                                                                          
008600*- - - - - - - - - - - - - -                                              
008700                                                                          
008800     EJECT                                                                
008810*      --- VALID IDDC CODES                                               
008820*                                                                         
008830*01    -COPY WWDCKONS                                                     
008840       EJECT                                                              
008900 01  DAGENS-DATUM.                                                        
009000   03  DAGENS-DATUM-AR           PIC 9(2).                                
009100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009300                                                                          
009400 01  DAGENS-TID.                                                          
009500   03  DAGENS-TID-TIM            PIC 9(2).                                
009600   03  DAGENS-TID-MIN            PIC 9(2).                                
009700   03  DAGENS-TID-SEK            PIC 9(2).                                
009800                                                                          
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010100   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010300     SKIP3                                                                
010400*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010500                                                                          
010600 01  RETURKODER.                                                          
010700   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010900   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011000     EJECT                                                                
011100*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011200                                                                          
011300******************************************************************        
011400                                                                          
011500 01  W-ARBAREA.                                                           
011600 03  W-ARBAREA-X                 PIC X(211).                              
011700     SKIP2                                                                
011800*                                                                         
011892*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011893     EJECT                                                                
012106                                                                          
012107*01  -COPY W461RIFN        -PRE JFR-.                                     
012110     EJECT                                                                
012200*                             STARTKORT                                   
012300*01  -COPY W461RI0N                                                       
012500     EJECT                                                                
012600*                             SLUTKORT                                    
012700*01  -COPY W461RI9                                                        
012900     EJECT                                                                
013000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013100                                                                          
013200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013300     SKIP2                                                                
013400*01  -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013800                                                                          
013900*01  -COPY W0005       -PRE  POSTSUM-.                                    
014100     EJECT                                                                
014200 PROCEDURE DIVISION.                                                      
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500     PERFORM B-BEHANDLA                                                   
014600     PERFORM Z-FINIT                                                      
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000                                                                          
015100     SKIP3                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400     OPEN INPUT W46179 OUTPUT W46199                                      
015500     SKIP2                                                                
015600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015700     SKIP2                                                                
015800*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016000     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016100     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016200     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016300     SKIP2                                                                
016400*- - - - - - - - - - - - - - - - TID                                      
016500*                                                                         
016600     ACCEPT   DAGENS-TID FROM TIME                                        
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 B-BEHANDLA SECTION.                                                      
017100     SKIP2                                                                
017200     PERFORM BA-START-KORT                                                
017300     PERFORM S01-LAS-W46179                                               
017400     PERFORM UNTIL                                                        
017500      NOT ( W46179-EOF = NEJ )                                            
017600       IF W-RIF-IDPTYP  = 'RIF'                                           
017700         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017800         PERFORM S01-LAS-W46179                                           
017900       ELSE                                                               
018000         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018100           ADD     +1           TO W-ANT-POSTER                           
018300           WRITE UTPOST FROM JFR-RIF-W461RIFN-CTX                         
018400           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018500         END-IF                                                           
018600         IF W-RIF-IDPTYP = 'RIC' OR 'RIX' OR 'RIY' OR 'RIS' OR            
018610                           'RKA'                                          
018700           PERFORM S01-LAS-W46179                                         
018800         ELSE                                                             
018810           EVALUATE    W-RIF-IDPTYP                                       
018830                                                                          
018840             WHEN  'RID'                                                  
018850                  WRITE RID-POST  FROM INPOST                             
018860                                                                          
018870             WHEN  'RIE'                                                  
018880                  WRITE RIE-POST  FROM INPOST                             
018890                                                                          
018900             WHEN  'RIH'                                                  
018910                  WRITE RIH-POST  FROM INPOST                             
018920                                                                          
018930             WHEN  'RIO'                                                  
018940                  WRITE RIO-POST  FROM INPOST                             
018950                                                                          
018951             WHEN  'RKB'                                                  
018952                  WRITE RKB-POST  FROM INPOST                             
018953                                                                          
018954             WHEN  'RKC'                                                  
018955                  WRITE RKC-POST  FROM INPOST                             
018956                                                                          
018957             WHEN  'RKD'                                                  
018958                  WRITE RKD-POST  FROM INPOST                             
018959                                                                          
018960             WHEN  OTHER                                                  
018970                  WRITE UTPOST    FROM INPOST                             
018980                                                                          
018985                                                                          
018986           END-EVALUATE                                                   
018987           ADD     +1           TO W-ANT-POSTER                           
018990           PERFORM S01-LAS-W46179                                         
018991         END-IF                                                           
018992       END-IF                                                             
018993     END-PERFORM                                                          
018994     PERFORM BB-SLUT-KORT                                                 
018995     .                                                                    
018996     EJECT                                                                
018997                                                                          
019900 BA-START-KORT SECTION.                                                   
020000     SKIP2                                                                
020100     MOVE     'RI0'          TO START-IDPTYP                              
020200     MOVE     2178           TO START-IDDISTR                             
020300     MOVE     WC-CDC-SE      TO START-IDDC                                
020400     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020500*                                                                         
020600     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020700*                                                                         
020800     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
020900     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021100     .                                                                    
021200     SKIP2                                                                
021300 BB-SLUT-KORT SECTION.                                                    
021400     SKIP2                                                                
021500     MOVE     'RI9'          TO SLUT-IDPTYP                               
021600     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021700*                                                                         
021800     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022100     .                                                                    
022200     SKIP2                                                                
022300 S01-LAS-W46179 SECTION.                                                  
022400     SKIP2                                                                
022500     READ   W46179 INTO W-ARBAREA                                         
022600     AT END MOVE JA TO W46179-EOF                                         
022700     END-READ                                                             
022800                                                                          
022900     IF W46179-EOF = NEJ                                                  
023000                                                                          
023100       MOVE 'W46179'            TO POSTSUM-FDNAMN                         
023200       MOVE 'W46179D1'          TO POSTSUM-DDNAMN2                        
023300       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023400       CALL POSTSUM             USING POSTSUM-PARM                        
023500                                                                          
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 Z-FINIT SECTION.                                                         
024000     SKIP2                                                                
024100                                                                          
024200     CLOSE W46179 W46199                                                  
024300     SKIP2                                                                
024400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024500*                                    SKRIVNA POSTER                       
024600                                                                          
024700     MOVE 'S' TO POSTSUM-OPKOD                                            
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     .                                                                    
