000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4617000.                                                 
000400*AUTHOR.        BOSSE B                                                   
000500*DATE-WRITTEN.  NOV 1985.                                                 
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (SVERIGE-PV)                        
001200*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001300*        SKAPAR START OCH SLUTKORT                                        
001400*        PT RIC,RIQ,RIT,SKALL EJ SKRIVAS                                  
001500*                                                                         
001600*        FÖR PT RIF GÄLLER ATT ENDAST SISTA "RIF KORTET"                  
001700*        SKALL SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                 
001800*        (ENDAST ENGELSK TEXT).                                           
001900*    ABENDKODER:                                                          
002000*                                                                         
002100*                                                                         
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*- - - - - - - - - - - - INFIL:                                           
003000*                        - -  FIL TILL VIPS                               
003100     SELECT W46170                       ASSIGN TO UT-S-W46170D1.         
003200     SKIP2                                                                
003300*- - - - - - - - - - - - UTFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46190                       ASSIGN TO UT-S-W46170D2.         
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W46170                                                               
004200     RECORDING      V                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500 01  INPOST          PIC X(211).                                          
004600     SKIP2                                                                
004700 FD  W46190                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000 01  UTPOST                       PIC X(80).                              
005100     SKIP2                                                                
005200 01  RID-POST  -COPY W461RIDN      -L.                                    
005300     SKIP2                                                                
005400 01  RIE-POST  -COPY W461RIEN      -L.                                    
005500     SKIP2                                                                
005600 01  RIH-POST  -COPY W461RIHN      -L.                                    
005700     SKIP2                                                                
005800 01  RIO-POST  -COPY W461RIO2      -L.                                    
005900     SKIP2                                                                
006000 01  RKB-POST  -COPY W461RKBN     -L.                                     
006100     SKIP2                                                                
006200 01  RKC-POST  -COPY W461RKCN     -L.                                     
006300     SKIP2                                                                
006400 01  RKD-POST  -COPY W461RKDN     -L.                                     
006500     EJECT                                                                
006600 01  RIM-POST  -COPY W461RIM2     -L.                                     
006610     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900*    -- CHECKED BY WY2000                                                 
007000*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007100 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617000'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  W46170-EOF                  PIC X(1)    VALUE 'N'.                   
008100     SKIP2                                                                
008200*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008300                                                                          
008400 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008500 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008600                                                                          
008700*- - - - - - - - - - - - - -                                              
008800                                                                          
008900     EJECT                                                                
009000 01  DAGENS-DATUM.                                                        
009100   03  DAGENS-DATUM-AR           PIC 9(2).                                
009200   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009300   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDCKONS                                                     
009700       EJECT                                                              
009800                                                                          
009900 01  DAGENS-TID.                                                          
010000   03  DAGENS-TID-TIM            PIC 9(2).                                
010100   03  DAGENS-TID-MIN            PIC 9(2).                                
010200   03  DAGENS-TID-SEK            PIC 9(2).                                
010300                                                                          
010400 01  DYNAMISKA-SUBPROGRAM.                                                
010500   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010600   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010800     SKIP3                                                                
010900*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
011000                                                                          
011100 01  RETURKODER.                                                          
011200   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
011300   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
011400   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011500     EJECT                                                                
011600*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011700                                                                          
011800 01  W-ARBAREA.                                                           
011900 03  W-ARBAREA-X                 PIC X(211).                              
012000     SKIP2                                                                
012100*                                                                         
012200*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012300     EJECT                                                                
012400                                                                          
012500*01  -COPY W461RIFN        -PRE JFR-.                                     
012600     EJECT                                                                
012700*                             STARTKORT                                   
012800*01  -COPY W461RI0N                                                       
012900     EJECT                                                                
013000*                             SLUTKORT                                    
013100*01  -COPY W461RI9                                                        
013200     EJECT                                                                
013300*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013400                                                                          
013500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013600     SKIP2                                                                
013700*01  -COPY WDATKORT                                                       
013800     EJECT                                                                
013900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
014000                                                                          
014100*01  -COPY W0005       -PRE  POSTSUM-.                                    
014200     EJECT                                                                
014300 PROCEDURE DIVISION.                                                      
014400     SKIP2                                                                
014500     PERFORM A-INIT                                                       
014600     PERFORM B-BEHANDLA                                                   
014700     PERFORM Z-FINIT                                                      
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100                                                                          
015200     SKIP3                                                                
015300 A-INIT SECTION.                                                          
015400     SKIP2                                                                
015500     OPEN INPUT W46170 OUTPUT W46190                                      
015600     SKIP2                                                                
015700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015800     SKIP2                                                                
015900*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
016000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016100     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016200     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016300     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016400     MOVE SPACE TO JFR-RIF-W461RIFN-CTX                                   
016500     SKIP2                                                                
016600*- - - - - - - - - - - - - - - - TID                                      
016700*                                                                         
016800     ACCEPT   DAGENS-TID FROM TIME                                        
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200 B-BEHANDLA SECTION.                                                      
017300     SKIP2                                                                
017400     PERFORM BA-START-KORT                                                
017500     PERFORM S01-LAS-W46170                                               
017600     PERFORM UNTIL                                                        
017700      NOT ( W46170-EOF = NEJ )                                            
017800       IF W-RIF-IDPTYP  = 'RIF'                                           
017900         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
018000         PERFORM S01-LAS-W46170                                           
018100       ELSE                                                               
018200         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018300           ADD     +1           TO W-ANT-POSTER                           
018400           WRITE UTPOST FROM JFR-RIF-W461RIFN-CTX                         
018500           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018600         END-IF                                                           
018700         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RIT'                        
018800                            OR 'RIS' OR 'RKA'                             
018900           PERFORM S01-LAS-W46170                                         
019000         ELSE                                                             
019100           EVALUATE    W-RIF-IDPTYP                                       
019200                                                                          
019300             WHEN  'RID'                                                  
019400                  WRITE RID-POST  FROM INPOST                             
019500                                                                          
019600             WHEN  'RIE'                                                  
019700                  WRITE RIE-POST  FROM INPOST                             
019800                                                                          
019900             WHEN  'RIH'                                                  
020000                  WRITE RIH-POST  FROM INPOST                             
020100                                                                          
020200             WHEN  'RIO'                                                  
020300                  WRITE RIO-POST  FROM INPOST                             
020400                                                                          
020500             WHEN  'RKB'                                                  
020600                  WRITE RKB-POST  FROM INPOST                             
020700                                                                          
020800             WHEN  'RKC'                                                  
020900                  WRITE RKC-POST  FROM INPOST                             
021000                                                                          
021100             WHEN  'RKD'                                                  
021200                  WRITE RKD-POST  FROM INPOST                             
021300                                                                          
021310             WHEN  'RIM'                                                  
021320                  WRITE RIM-POST  FROM INPOST                             
021330                                                                          
021400             WHEN  OTHER                                                  
021500                  WRITE UTPOST    FROM INPOST                             
021600                                                                          
021700           END-EVALUATE                                                   
021800                                                                          
021900           ADD     +1           TO W-ANT-POSTER                           
022000           PERFORM S01-LAS-W46170                                         
022100         END-IF                                                           
022200       END-IF                                                             
022300     END-PERFORM                                                          
022400     PERFORM BB-SLUT-KORT                                                 
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 BA-START-KORT SECTION.                                                   
022900     SKIP2                                                                
023000     MOVE     'RI0'          TO START-IDPTYP                              
023100     MOVE     778            TO START-IDDISTR                             
023200     MOVE     WC-CDC-SE      TO START-IDDC                                
023300     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
023400*                                                                         
023500     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
023600*                                                                         
023700     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
023800     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
023900     .                                                                    
024000     EJECT                                                                
024100 BB-SLUT-KORT SECTION.                                                    
024200     SKIP2                                                                
024300     MOVE     'RI9'          TO SLUT-IDPTYP                               
024400     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
024500*                                                                         
024600     WRITE    UTPOST         FROM  SLUT-W461RI9                           
024700     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
024800     .                                                                    
024900     EJECT                                                                
025000 S01-LAS-W46170 SECTION.                                                  
025100     SKIP2                                                                
025200     READ   W46170 INTO W-ARBAREA                                         
025300     AT END MOVE JA TO W46170-EOF                                         
025400     END-READ                                                             
025500                                                                          
025600     IF W46170-EOF = NEJ                                                  
025700       MOVE 'W46170'            TO POSTSUM-FDNAMN                         
025800       MOVE 'W46170D1'          TO POSTSUM-DDNAMN2                        
025900       MOVE SPACE               TO POSTSUM-TRANSTYP                       
026000       CALL POSTSUM             USING POSTSUM-PARM                        
026100     END-IF                                                               
026200     .                                                                    
026300 Z-FINIT SECTION.                                                         
026400     SKIP2                                                                
026500                                                                          
026600     CLOSE W46170 W46190                                                  
026700     SKIP2                                                                
026800*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
026900*                                    SKRIVNA POSTER                       
027000                                                                          
027100     MOVE 'S' TO POSTSUM-OPKOD                                            
027200     CALL POSTSUM USING POSTSUM-PARM                                      
027300     .                                                                    
