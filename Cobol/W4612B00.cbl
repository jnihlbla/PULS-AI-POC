000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4612B00.                                                 
000400*AUTHOR.        MARGARETA GABRIELSSON                                     
000500*DATE-WRITTEN.  SEPTEMBER 1995                                            
000600                                                                          
000700*REMARKS.                                                                 
000800*        IMPORTÖRSBEROENDE BEHANDLING (ENGLAND, NEW CONCEPT)              
000900*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001000*        SKAPAR START OCH SLUTKORT                                        
001100                                                                          
001200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001400*        (ENDAST ENGELSK TEXT).                                           
001500*    ABENDKODER:                                                          
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*- - - - - - - - - - - - INFIL:                                           
002500*                        - -  FIL TILL VIPS                               
002600     SELECT W4612B                       ASSIGN TO UT-S-W4612BD1.         
002700     SKIP2                                                                
002800*- - - - - - - - - - - - UTFIL:                                           
002900*                        - -  FIL TILL VIPS                               
003000     SELECT W4614B                       ASSIGN TO UT-S-W4612BD2.         
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W4612B                                                               
003700     RECORDING      V                                                     
003800     BLOCK CONTAINS 0.                                                    
003900     SKIP2                                                                
004000 01  INPOST         PIC X(211).                                           
004100     EJECT                                                                
004200 FD  W4614B                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS 0.                                                    
004500 01  UTPOST                       PIC X(80).                              
004600     SKIP2                                                                
004700 01  RID-POST      -COPY W461RIDN       -L.                               
004800     SKIP2                                                                
004900 01  RIE-POST      -COPY W461RIEN       -L.                               
005000     SKIP2                                                                
005100 01  RIH-POST      -COPY W461RIHN       -L.                               
005200     SKIP2                                                                
005420 01  RIO-POST      -COPY W461RIO2       -L.                               
005430     SKIP2                                                                
005440 01  RKB-POST      -COPY W461RKBN       -L.                               
005450     SKIP2                                                                
005460 01  RKC-POST      -COPY W461RKCN       -L.                               
005470     SKIP2                                                                
005480 01  RKD-POST      -COPY W461RKDN       -L.                               
005490     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
005900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612B00'.            
006000     SKIP2                                                                
006100*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006200                                                                          
006300 77  JA                          PIC X(1)    VALUE 'J'.                   
006400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006500     SKIP2                                                                
006600*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
006700                                                                          
006800 77  W4612B-EOF                  PIC X(1)    VALUE 'N'.                   
006900     SKIP2                                                                
007000*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
007100                                                                          
007200 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
007300 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
007400                                                                          
007500*- - - - - - - - - - - - - -                                              
007600                                                                          
007700     EJECT                                                                
007710*      --- VALID IDDC CODES                                               
007720*                                                                         
007730*01    -COPY WWDCKONS                                                     
007740       EJECT                                                              
007800 01  DAGENS-DATUM.                                                        
007900   03  DAGENS-DATUM-AR           PIC 9(2).                                
008000   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
008100   03  DAGENS-DATUM-DAG          PIC 9(2).                                
008200                                                                          
008300 01  DAGENS-TID.                                                          
008400   03  DAGENS-TID-TIM            PIC 9(2).                                
008500   03  DAGENS-TID-MIN            PIC 9(2).                                
008600   03  DAGENS-TID-SEK            PIC 9(2).                                
008700                                                                          
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009000   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
009100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP3                                                                
009300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009400                                                                          
009500 01  RETURKODER.                                                          
009600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009900     EJECT                                                                
010000*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010100                                                                          
010200******************************************************************        
010300                                                                          
010400 01  W-ARBAREA.                                                           
010500 03  W-ARBAREA-X                 PIC X(211).                              
010600     SKIP2                                                                
010700*                                                                         
010800*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
010900     EJECT                                                                
011000*                                                                         
011020     EJECT                                                                
011100*01  -COPY W461RIFN        -PRE JFR-.                                     
011200     EJECT                                                                
011300*                             STARTKORT                                   
011400*01  -COPY W461RI0N                                                       
011500     EJECT                                                                
011600*                             SLUTKORT                                    
011700*01  -COPY W461RI9                                                        
011800     EJECT                                                                
011900*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
012000                                                                          
012100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012200     SKIP2                                                                
012300*01  -COPY WDATKORT                                                       
012400     EJECT                                                                
012500*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
012600                                                                          
012700*01  -COPY W0005       -PRE  POSTSUM-.                                    
012800     EJECT                                                                
012900 PROCEDURE DIVISION.                                                      
013000     SKIP2                                                                
013100     PERFORM A-INIT                                                       
013200     PERFORM B-BEHANDLA                                                   
013300     PERFORM Z-FINIT                                                      
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700                                                                          
013800     SKIP3                                                                
013900 A-INIT SECTION.                                                          
014000     SKIP2                                                                
014100     OPEN INPUT W4612B OUTPUT W4614B                                      
014200     SKIP2                                                                
014300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014400     SKIP2                                                                
014500*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
014600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014700     MOVE D-AAR TO DAGENS-DATUM-AR                                        
014800     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
014900     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
015000     SKIP2                                                                
015100*- - - - - - - - - - - - - - - - TID                                      
015200*                                                                         
015300     ACCEPT   DAGENS-TID FROM TIME                                        
015400     .                                                                    
015500     EJECT                                                                
015600                                                                          
015700 B-BEHANDLA SECTION.                                                      
015800     SKIP2                                                                
015900     PERFORM BA-START-KORT                                                
016000     PERFORM S01-LAS-W4612B                                               
016100     PERFORM UNTIL                                                        
016200      NOT ( W4612B-EOF = NEJ )                                            
016300       IF W-RIF-IDPTYP  = 'RIF'                                           
016400         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
016500         PERFORM S01-LAS-W4612B                                           
016600       ELSE                                                               
016700         IF JFR-RIF-IDPTYP   = 'RIF'                                      
016800           ADD     +1           TO W-ANT-POSTER                           
016900           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
017000           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
017100         END-IF                                                           
017200         IF W-RIF-IDPTYP = 'RIC' OR 'RIS' OR 'RKA'                        
017300           PERFORM S01-LAS-W4612B                                         
017400         ELSE                                                             
017500           EVALUATE    W-RIF-IDPTYP                                       
017600                                                                          
017700             WHEN  'RID'                                                  
017800                  WRITE RID-POST FROM INPOST                              
017900                                                                          
018000             WHEN  'RIE'                                                  
018100                  WRITE RIE-POST FROM INPOST                              
018200                                                                          
018300             WHEN  'RIH'                                                  
018400                  WRITE RIH-POST FROM INPOST                              
018500                                                                          
018600             WHEN  'RIO'                                                  
018700                  WRITE RIO-POST FROM INPOST                              
018800                                                                          
018810             WHEN  'RKB'                                                  
018820                  WRITE RKB-POST  FROM INPOST                             
018830                                                                          
018840             WHEN  'RKC'                                                  
018850                  WRITE RKC-POST  FROM INPOST                             
018860                                                                          
018870             WHEN  'RKD'                                                  
018880                  WRITE RKD-POST  FROM INPOST                             
018890                                                                          
018900             WHEN  OTHER                                                  
019000                  WRITE UTPOST   FROM INPOST                              
019100                                                                          
019200           END-EVALUATE                                                   
019300                                                                          
019400           ADD     +1           TO W-ANT-POSTER                           
019500           PERFORM S01-LAS-W4612B                                         
019600         END-IF                                                           
019700       END-IF                                                             
019800     END-PERFORM                                                          
019900     PERFORM BB-SLUT-KORT                                                 
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300 BA-START-KORT SECTION.                                                   
020400     SKIP2                                                                
020500     MOVE     'RI0'          TO START-IDPTYP                              
020600     MOVE     1378           TO START-IDDISTR                             
020700     MOVE     WC-CDC-SE      TO START-IDDC                                
020800     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020900*                                                                         
021000     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
021100*                                                                         
021200     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
021300     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021400     .                                                                    
021500     SKIP2                                                                
021600 BB-SLUT-KORT SECTION.                                                    
021700     SKIP2                                                                
021800     MOVE     'RI9'          TO SLUT-IDPTYP                               
021900     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
022000*                                                                         
022100     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022200     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022300     .                                                                    
022400     SKIP2                                                                
022500 S01-LAS-W4612B SECTION.                                                  
022600     SKIP2                                                                
022700     READ   W4612B INTO W-ARBAREA                                         
022800     AT END MOVE JA TO W4612B-EOF                                         
022900     END-READ                                                             
023000                                                                          
023100     IF W4612B-EOF = NEJ                                                  
023200                                                                          
023300       MOVE 'W4612B'            TO POSTSUM-FDNAMN                         
023400       MOVE 'W4612BD1'          TO POSTSUM-DDNAMN2                        
023500       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023600       CALL POSTSUM             USING POSTSUM-PARM                        
023700                                                                          
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 Z-FINIT SECTION.                                                         
024200     SKIP2                                                                
024300                                                                          
024400     CLOSE W4612B W4614B                                                  
024500     SKIP2                                                                
024600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024700*                                    SKRIVNA POSTER                       
024800                                                                          
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
