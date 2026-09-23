000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4613F00.                                                 
000400*AUTHOR.        LENA BROMANDER.                                           
000500*DATE-WRITTEN.  DEC 2017.                                                 
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (INDIEN)                            
001200*        FILER INNEHÅLLER TRANSAR FÖR BÅDE DISTRIKT                       
001300*        6010   (TEST/PROD)                                               
001400*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001500*        SKAPAR START OCH SLUTKORT                                        
001600*                                                                         
001700*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001800*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001900*        (ENDAST ENGELSK TEXT).                                           
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
003100     SELECT W4613F                       ASSIGN TO UT-S-W4613FD1.         
003200     SKIP2                                                                
003300*- - - - - - - - - - - - UTFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W461IN                       ASSIGN TO UT-S-W4613FD2.         
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W4613F                                                               
004200     RECORDING      V                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500 01  INPOST               PIC X(211).                                     
004600                                                                          
004700 FD  W461IN                                                               
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS 0.                                                    
005000 01  UTPOST                       PIC X(80).                              
005100     SKIP2                                                                
005200 01  RID-POST      -COPY W461RIDN     -L.                                 
005300     SKIP2                                                                
005400 01  RIE-POST      -COPY W461RIEN     -L.                                 
005500     SKIP2                                                                
005600 01  RIH-POST      -COPY W461RIHN     -L.                                 
005700     SKIP2                                                                
005800 01  RIO-POST      -COPY W461RIO2     -L.                                 
005900     SKIP2                                                                
006000 01  RKB-POST      -COPY W461RKBN     -L.                                 
006100     SKIP2                                                                
006200 01  RKC-POST      -COPY W461RKCN     -L.                                 
006300     SKIP2                                                                
006400 01  RKD-POST      -COPY W461RKDN     -L.                                 
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613F00'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W4613F-EOF                  PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008200                                                                          
008300 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008400 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008500                                                                          
008600*- - - - - - - - - - - - - -                                              
008700                                                                          
008800     EJECT                                                                
008900*      --- VALID IDDC CODES                                               
009000*                                                                         
009100*01    -COPY WWDCKONS                                                     
009200       EJECT                                                              
009300 01  DAGENS-DATUM.                                                        
009400   03  DAGENS-DATUM-AR           PIC 9(2).                                
009500   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009600   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009700                                                                          
009800 01  DAGENS-TID.                                                          
009900   03  DAGENS-TID-TIM            PIC 9(2).                                
010000   03  DAGENS-TID-MIN            PIC 9(2).                                
010100   03  DAGENS-TID-SEK            PIC 9(2).                                
010200                                                                          
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010500   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010600   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010700     SKIP3                                                                
010800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010900                                                                          
011000 01  RETURKODER.                                                          
011100   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
011200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
011300   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011400     EJECT                                                                
011500*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011600                                                                          
011700******************************************************************        
011800                                                                          
011900 01  W-ARBAREA.                                                           
012000 03  W-ARBAREA-X                 PIC X(211).                              
012100     SKIP2                                                                
012200*                                                                         
012300*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012400     EJECT                                                                
012500                                                                          
012600*01  -COPY W461RIFN        -PRE JFR-.                                     
012700     EJECT                                                                
012800*                             STARTKORT                                   
012900*01  -COPY W461RI0N                                                       
013000     EJECT                                                                
013100*                             SLUTKORT                                    
013200*01  -COPY W461RI9                                                        
013300     EJECT                                                                
013400*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013500                                                                          
013600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013700     SKIP2                                                                
013800*01  -COPY WDATKORT                                                       
013900     EJECT                                                                
014000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
014100                                                                          
014200*01  -COPY W0005       -PRE  POSTSUM-.                                    
014300     EJECT                                                                
014400 PROCEDURE DIVISION.                                                      
014500     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014700     PERFORM B-BEHANDLA                                                   
014800     PERFORM Z-FINIT                                                      
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015300     SKIP3                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600     OPEN INPUT W4613F OUTPUT W461IN                                      
015700     SKIP2                                                                
015800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015900     SKIP2                                                                
016000*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
016100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016200     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016300     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016400     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
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
017500     PERFORM S01-LAS-W4613F                                               
017600     PERFORM UNTIL                                                        
017700      NOT ( W4613F-EOF = NEJ )                                            
017800       IF W-RIF-IDPTYP  = 'RIF'                                           
017900         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
018000         PERFORM S01-LAS-W4613F                                           
018100       ELSE                                                               
018200         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018300           ADD     +1           TO W-ANT-POSTER                           
018400           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
018500           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018600         END-IF                                                           
018800         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RKF' OR 'RKO'               
018900           PERFORM S01-LAS-W4613F                                         
019000         ELSE                                                             
019100           EVALUATE    W-RIF-IDPTYP                                       
019200                                                                          
019300             WHEN  'RID'                                                  
019400                  WRITE RID-POST FROM INPOST                              
019500                                                                          
019600             WHEN  'RIE'                                                  
019700                  WRITE RIE-POST FROM INPOST                              
019800                                                                          
019900             WHEN  'RIH'                                                  
020000                  WRITE RIH-POST FROM INPOST                              
020100                                                                          
020200             WHEN  'RIO'                                                  
020300                  WRITE RIO-POST FROM INPOST                              
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
021400             WHEN  OTHER                                                  
021500                  WRITE UTPOST   FROM INPOST                              
021600                                                                          
021700           END-EVALUATE                                                   
021800           ADD     +1           TO W-ANT-POSTER                           
021900           PERFORM S01-LAS-W4613F                                         
022000         END-IF                                                           
022100       END-IF                                                             
022200     END-PERFORM                                                          
022300     PERFORM BB-SLUT-KORT                                                 
022400     .                                                                    
022500     EJECT                                                                
022600                                                                          
022700                                                                          
022800 BA-START-KORT SECTION.                                                   
022900     SKIP2                                                                
023000     MOVE     'RI0'          TO START-IDPTYP                              
023100     MOVE     6010           TO START-IDDISTR                             
023200     MOVE     WC-CDC-SE      TO START-IDDC                                
023300     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
023400*                                                                         
023500     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
023600*                                                                         
023700     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
023800     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
023900     .                                                                    
024000     SKIP2                                                                
024100 BB-SLUT-KORT SECTION.                                                    
024200     SKIP2                                                                
024300     MOVE     'RI9'          TO SLUT-IDPTYP                               
024400     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
024500*                                                                         
024600     WRITE    UTPOST         FROM  SLUT-W461RI9                           
024700     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
024800     .                                                                    
024900     SKIP2                                                                
025000 S01-LAS-W4613F SECTION.                                                  
025100     SKIP2                                                                
025200     READ   W4613F INTO W-ARBAREA                                         
025300     AT END MOVE JA TO W4613F-EOF                                         
025400     END-READ                                                             
025500                                                                          
025600     IF W4613F-EOF = NEJ                                                  
025700                                                                          
025800       MOVE 'W4613F'            TO POSTSUM-FDNAMN                         
025900       MOVE 'W4613FD1'          TO POSTSUM-DDNAMN2                        
026000       MOVE SPACE               TO POSTSUM-TRANSTYP                       
026100       CALL POSTSUM             USING POSTSUM-PARM                        
026200                                                                          
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 Z-FINIT SECTION.                                                         
026700     SKIP2                                                                
026800                                                                          
026900     CLOSE W4613F W461IN                                                  
027000     SKIP2                                                                
027100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
027200*                                    SKRIVNA POSTER                       
027300                                                                          
027400     MOVE 'S' TO POSTSUM-OPKOD                                            
027500     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
