000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613600.                                                 
001000*AUTHOR.        AGNETA VON HORN.                                          
001100*DATE-WRITTEN.  FEBR 1989.                                                
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700*        IMPORTÖRSBEROENDE BEHANDLING (JAPAN)                             
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT.                            
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIS, RKA, RKB, RKC, RKD, RKE, RKF SKRIVS                      
002100                                                                          
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (ENGELSK TEXT)                                                   
002500*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46136                       ASSIGN TO UT-S-W46136D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W46156                       ASSIGN TO UT-S-W46136D2.         
004000     SELECT W4615G                       ASSIGN TO UT-S-W46136D3.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46136                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
004910 01  INPOST          PIC X(211).                                          
004920     SKIP2                                                                
005250 FD  W46156                                                               
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005610 01  RID-POST  -COPY W461RIDN     -L.                                     
005620     SKIP2                                                                
005630 01  RIE-POST  -COPY W461RIEN     -L.                                     
005640     SKIP2                                                                
005650 01  RIH-POST  -COPY W461RIHN     -L.                                     
005660     SKIP2                                                                
005670 01  RIO-POST  -COPY W461RIO2     -L.                                     
005680     SKIP2                                                                
005690 01  RKB-POST  -COPY W461RKBN     -L.                                     
005691     SKIP2                                                                
005692 01  RKC-POST  -COPY W461RKCN     -L.                                     
005693     SKIP2                                                                
005694 01  RKD-POST  -COPY W461RKDN     -L.                                     
005695     EJECT                                                                
005700 FD  W4615G                                                               
005800     RECORDING      V                                                     
005900     BLOCK CONTAINS 0.                                                    
006000 01  UTPOST2                      PIC X(80).                              
006010     SKIP2                                                                
006020 01  RID-POST2  -COPY W461RIDN     -L.                                    
006030     SKIP2                                                                
006040 01  RIE-POST2  -COPY W461RIEN     -L.                                    
006050     SKIP2                                                                
006060 01  RIH-POST2  -COPY W461RIHN     -L.                                    
006070     SKIP2                                                                
006080 01  RIO-POST2  -COPY W461RIO2     -L.                                    
006090     SKIP2                                                                
006091 01  RKB-POST2  -COPY W461RKBN     -L.                                    
006092     SKIP2                                                                
006093 01  RKC-POST2  -COPY W461RKCN     -L.                                    
006094     SKIP2                                                                
006095 01  RKD-POST2  -COPY W461RKDN     -L.                                    
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613600'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46136-EOF                  PIC X(1)    VALUE 'N'.                   
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
015400     OPEN INPUT W46136 OUTPUT W46156 W4615G                               
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
017300     PERFORM S01-LAS-W46136                                               
017400     PERFORM UNTIL W46136-EOF = JA                                        
018600       IF W-RIF-IDPTYP = 'RKB' OR 'RKC' OR 'RKD'                          
018700                      OR 'RKE' OR 'RKF'                                   
018800         EVALUATE    W-RIF-IDPTYP                                         
018801                                                                          
018802           WHEN  'RKB'                                                    
018803                WRITE RKB-POST  FROM INPOST                               
018804                WRITE RKB-POST2 FROM INPOST                               
018805                                                                          
018806           WHEN  'RKC'                                                    
018807                WRITE RKC-POST  FROM INPOST                               
018808                WRITE RKC-POST2 FROM INPOST                               
018809                                                                          
018810           WHEN  'RKD'                                                    
018811                WRITE RKD-POST  FROM INPOST                               
018812                WRITE RKD-POST2 FROM INPOST                               
018813                                                                          
018815           WHEN  OTHER                                                    
018816                WRITE  UTPOST   FROM INPOST                               
018817                WRITE  UTPOST2  FROM INPOST                               
018818                                                                          
018819         END-EVALUATE                                                     
018820         ADD     +1           TO W-ANT-POSTER                             
018821         PERFORM S01-LAS-W46136                                           
018822       ELSE                                                               
018823         PERFORM S01-LAS-W46136                                           
018824       END-IF                                                             
018825     END-PERFORM                                                          
018826     PERFORM BB-SLUT-KORT                                                 
018827     .                                                                    
018828     EJECT                                                                
018830                                                                          
020000 BA-START-KORT SECTION.                                                   
020100     SKIP2                                                                
020200     MOVE     'RI0'          TO START-IDPTYP                              
020300     MOVE     5222           TO START-IDDISTR                             
020400     MOVE     WC-CDC-SE      TO START-IDDC                                
020500     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020600*                                                                         
020700     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020800*                                                                         
020900     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
021000     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021100     WRITE    UTPOST2        FROM  START-W461RI0N-CTX                     
021200     .                                                                    
021300     SKIP2                                                                
021400 BB-SLUT-KORT SECTION.                                                    
021500     SKIP2                                                                
021600     MOVE     'RI9'          TO SLUT-IDPTYP                               
021700     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021800*                                                                         
021900     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022000     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
022100     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022200     .                                                                    
022300     SKIP2                                                                
022400 S01-LAS-W46136 SECTION.                                                  
022500     SKIP2                                                                
022600     READ   W46136 INTO W-ARBAREA                                         
022700     AT END MOVE JA TO W46136-EOF                                         
022800     END-READ                                                             
022900                                                                          
023000     IF W46136-EOF = NEJ                                                  
023100                                                                          
023200       MOVE 'W46136'            TO POSTSUM-FDNAMN                         
023300       MOVE 'W46136D1'          TO POSTSUM-DDNAMN2                        
023400       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023500       CALL POSTSUM             USING POSTSUM-PARM                        
023600                                                                          
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 Z-FINIT SECTION.                                                         
024100     SKIP2                                                                
024200                                                                          
024300     CLOSE W46136 W46156 W4615G                                           
024400     SKIP2                                                                
024500*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024600*                                    SKRIVNA POSTER                       
024700                                                                          
024800     MOVE 'S' TO POSTSUM-OPKOD                                            
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
