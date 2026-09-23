000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4612C00.                                                 
000700*AUTHOR.        THOMAS LARSSON.                                           
000800*DATE-WRITTEN.  OKTOBER 1995.                                             
000900                                                                          
001000*    REMARKS.                                                             
001100*                                                                         
001200*    FUNKTION:                                                            
001300*                                                                         
001400*        IMPORTÖRSBEROENDE BEHANDLING (POLEN)                             
001500*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001600*        SKAPAR START OCH SLUTKORT                                        
001700*        PT RIC SKALL EJ SKRIVAS                                          
001800*                                                                         
001900*        ENDAST SISTA "RIF KORTET"                                        
002000*        SKALL SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                 
002100*        (ENDAST ENGELSK TEXT).                                           
002200*    ABENDKODER:                                                          
002300*                                                                         
002310*                                                                         
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*- - - - - - - - - - - - INFIL:                                           
003200*                        - -  FIL TILL VIPS                               
003300     SELECT W4612C                       ASSIGN TO UT-S-W4612CD1.         
003400     SKIP2                                                                
003500*- - - - - - - - - - - - UTFIL:                                           
003600*                        - -  FIL TILL VIPS                               
003700     SELECT W4614C                       ASSIGN TO UT-S-W4612CD2.         
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W4612C                                                               
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
004901 01  INPOST          PIC X(211).                                          
004920     SKIP2                                                                
005050 FD  W4614C                                                               
005100     RECORDING      V                                                     
005200     BLOCK CONTAINS 0.                                                    
005300 01  UTPOST                       PIC X(80).                              
005400     SKIP2                                                                
005410 01  RID-POST  -COPY W461RIDN     -L.                                     
005420     SKIP2                                                                
005430 01  RIE-POST  -COPY W461RIEN     -L.                                     
005440     SKIP2                                                                
005450 01  RIH-POST  -COPY W461RIHN     -L.                                     
005460     SKIP2                                                                
005470 01  RIO-POST  -COPY W461RIO2     -L.                                     
005471     SKIP2                                                                
005472 01  RKB-POST  -COPY W461RKBN     -L.                                     
005473     SKIP2                                                                
005474 01  RKC-POST  -COPY W461RKCN     -L.                                     
005475     SKIP2                                                                
005476 01  RKD-POST  -COPY W461RKDN     -L.                                     
005480     SKIP2                                                                
006000 WORKING-STORAGE SECTION.                                                 
006001                                                                          
006010*    -- CHECKED BY WY2000                                                 
006100*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006200 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612C00'.            
006300     SKIP2                                                                
006400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006500                                                                          
006600 77  JA                          PIC X(1)    VALUE 'J'.                   
006700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006800     SKIP2                                                                
006900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007000                                                                          
007100 77  W4612C-EOF                  PIC X(1)    VALUE 'N'.                   
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
007400                                                                          
007500 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
007600 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +50000.         
007700                                                                          
007800*- - - - - - - - - - - - - -                                              
007900                                                                          
008000     EJECT                                                                
008010*      --- VALID IDDC CODES                                               
008020*                                                                         
008030*01    -COPY WWDCKONS                                                     
008040       EJECT                                                              
008100 01  DAGENS-DATUM.                                                        
008200   03  DAGENS-DATUM-AR           PIC 9(2).                                
008300   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
008400   03  DAGENS-DATUM-DAG          PIC 9(2).                                
008500                                                                          
008600 01  DAGENS-TID.                                                          
008700   03  DAGENS-TID-TIM            PIC 9(2).                                
008800   03  DAGENS-TID-MIN            PIC 9(2).                                
008900   03  DAGENS-TID-SEK            PIC 9(2).                                
009000                                                                          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
009400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009500     SKIP3                                                                
009600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009700                                                                          
009800 01  RETURKODER.                                                          
009900   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010100   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010200     EJECT                                                                
010300*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010400                                                                          
010500******************************************************************        
010600                                                                          
010700 01  W-ARBAREA.                                                           
010800 03  W-ARBAREA-X                 PIC X(211).                              
010900     SKIP2                                                                
010910*                                                                         
010993*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
010994     EJECT                                                                
011126                                                                          
011127*01  -COPY W461RIFN        -PRE JFR-.                                     
011130     EJECT                                                                
011200*                             STARTKORT                                   
011300*01  -COPY W461RI0N                                                       
011400     EJECT                                                                
011500*                             SLUTKORT                                    
011600*01  -COPY W461RI9                                                        
011700     EJECT                                                                
011800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
011900                                                                          
012000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012100     SKIP2                                                                
012200*01  -COPY WDATKORT                                                       
012300     EJECT                                                                
012400*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
012500                                                                          
012600*01  -COPY W0005       -PRE  POSTSUM-.                                    
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900     SKIP2                                                                
013000     PERFORM A-INIT                                                       
013100     PERFORM B-BEHANDLA                                                   
013200     PERFORM Z-FINIT                                                      
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600                                                                          
013700     SKIP3                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000     OPEN INPUT W4612C OUTPUT W4614C                                      
014100     SKIP2                                                                
014200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014300     SKIP2                                                                
014400*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
014500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014600     MOVE D-AAR TO DAGENS-DATUM-AR                                        
014700     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
014800     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
014900     SKIP2                                                                
015000*- - - - - - - - - - - - - - - - TID                                      
015100*                                                                         
015200     ACCEPT   DAGENS-TID FROM TIME                                        
015300     .                                                                    
015400     EJECT                                                                
015500                                                                          
015600 B-BEHANDLA SECTION.                                                      
015700     SKIP2                                                                
015800     PERFORM BA-START-KORT                                                
015900     PERFORM S01-LAS-W4612C                                               
016000     PERFORM UNTIL                                                        
016100      NOT ( W4612C-EOF = NEJ )                                            
016200       IF W-RIF-IDPTYP  = 'RIF'                                           
016300         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
016400         PERFORM S01-LAS-W4612C                                           
016500       ELSE                                                               
016600         IF JFR-RIF-IDPTYP   = 'RIF'                                      
016700           ADD     +1           TO W-ANT-POSTER                           
016800           WRITE UTPOST FROM JFR-RIF-W461RIFN-CTX                         
017000           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
017100         END-IF                                                           
017200         IF W-RIF-IDPTYP = 'RIC' OR 'RIS' OR 'RKA'                        
017300           PERFORM S01-LAS-W4612C                                         
017400         ELSE                                                             
017410           EVALUATE    W-RIF-IDPTYP                                       
017420                                                                          
017430             WHEN  'RID'                                                  
017440                  WRITE RID-POST  FROM INPOST                             
017450                                                                          
017451             WHEN  'RIE'                                                  
017452                  WRITE RIE-POST  FROM INPOST                             
017453                                                                          
017454             WHEN  'RIH'                                                  
017455                  WRITE RIH-POST  FROM INPOST                             
017456                                                                          
017457             WHEN  'RIO'                                                  
017458                  WRITE RIO-POST  FROM INPOST                             
017459                                                                          
017460             WHEN  'RKB'                                                  
017461                  WRITE RKB-POST  FROM INPOST                             
017462                                                                          
017463             WHEN  'RKC'                                                  
017464                  WRITE RKC-POST  FROM INPOST                             
017465                                                                          
017466             WHEN  'RKD'                                                  
017467                  WRITE RKD-POST  FROM INPOST                             
017469                                                                          
017470             WHEN  OTHER                                                  
017480                  WRITE UTPOST    FROM INPOST                             
017585                                                                          
017586           END-EVALUATE                                                   
017587                                                                          
017588           ADD     +1           TO W-ANT-POSTER                           
017591           PERFORM S01-LAS-W4612C                                         
017592         END-IF                                                           
017593       END-IF                                                             
017594     END-PERFORM                                                          
017595     PERFORM BB-SLUT-KORT                                                 
017596     .                                                                    
017597     EJECT                                                                
018070                                                                          
018500 BA-START-KORT SECTION.                                                   
018600     SKIP2                                                                
018700     MOVE     'RI0'          TO START-IDPTYP                              
018800     MOVE     2878           TO START-IDDISTR                             
018900     MOVE     WC-CDC-SE      TO START-IDDC                                
019000     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019100*                                                                         
019200     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019300*                                                                         
019400     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
019500     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
019700     .                                                                    
019800     SKIP2                                                                
019900 BB-SLUT-KORT SECTION.                                                    
020000     SKIP2                                                                
020100     MOVE     'RI9'          TO SLUT-IDPTYP                               
020200     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020300*                                                                         
020400     WRITE    UTPOST         FROM  SLUT-W461RI9                           
020600     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
020700     .                                                                    
020800     SKIP2                                                                
020900 S01-LAS-W4612C SECTION.                                                  
021000     SKIP2                                                                
021100     READ   W4612C INTO W-ARBAREA                                         
021200     AT END MOVE JA TO W4612C-EOF                                         
021300     END-READ                                                             
021400                                                                          
021500     IF W4612C-EOF = NEJ                                                  
021600                                                                          
021700       MOVE 'W4612C'            TO POSTSUM-FDNAMN                         
021800       MOVE 'W4612CD1'          TO POSTSUM-DDNAMN2                        
021900       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022000       CALL POSTSUM             USING POSTSUM-PARM                        
022100                                                                          
022200     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500*                                                                         
022600 Z-FINIT SECTION.                                                         
022700     SKIP2                                                                
022800                                                                          
022900     CLOSE W4612C W4614C                                                  
023000     SKIP2                                                                
023100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023200*                                    SKRIVNA POSTER                       
023300                                                                          
023400     MOVE 'S' TO POSTSUM-OPKOD                                            
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     .                                                                    
