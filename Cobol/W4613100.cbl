000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613100.                                                 
001000*AUTHOR.        THOMAS NILSSON.                                           
001100*DATE-WRITTEN.  MARS 1987.                                                
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (AUSTRALIEN-PV)                     
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIS , RKA ,RKB ,RKC ,RKD ,RKE OCH RKF SKRIVS.                 
002100*                                                                         
002200*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46131                       ASSIGN TO UT-S-W46131D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W46152                       ASSIGN TO UT-S-W46131D2.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46131                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
004910 01  INPOST                       PIC X(211).                             
004920     SKIP2                                                                
005250 FD  W46152                                                               
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700*01  RID-POST  -COPY W461RIDN     -L.                                     
005800     SKIP2                                                                
005900*01  RIE-POST  -COPY W461RIEN     -L.                                     
006000     SKIP2                                                                
006100*01  RIH-POST  -COPY W461RIHN     -L.                                     
006110     SKIP2                                                                
006120*01  RIO-POST  -COPY W461RIO2     -L.                                     
006121     SKIP2                                                                
006122 01  RKB-POST  -COPY W461RKBN     -L.                                     
006123     SKIP2                                                                
006124 01  RKC-POST  -COPY W461RKCN     -L.                                     
006125     SKIP2                                                                
006126 01  RKD-POST  -COPY W461RKDN     -L.                                     
006130     SKIP2                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613100'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46131-EOF                  PIC X(1)    VALUE 'N'.                   
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
012180*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012190     EJECT                                                                
012200*03  FILLER  -COPY W461RKBN        -PRE W- -RED W-ARBAREA-X               
012300     EJECT                                                                
012310*03  FILLER  -COPY W461RKDN        -PRE W- -RED W-ARBAREA-X               
012320     EJECT                                                                
012321*03  FILLER  -COPY W461RKEN        -PRE W- -RED W-ARBAREA-X               
012322     EJECT                                                                
012323                                                                          
012330*                             STARTKORT                                   
012400*01  -COPY W461RI0N                                                       
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
015100     SKIP3                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400     OPEN INPUT W46131 OUTPUT W46152                                      
015500     SKIP2                                                                
015600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015700     SKIP2                                                                
015900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016000     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016100     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016200     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016300     SKIP2                                                                
016600     ACCEPT   DAGENS-TID FROM TIME                                        
016700     .                                                                    
016800     EJECT                                                                
017000 B-BEHANDLA SECTION.                                                      
017100     SKIP2                                                                
017200     PERFORM BA-START-KORT                                                
017300     PERFORM S01-LAS-W46131                                               
017400     PERFORM UNTIL W46131-EOF = JA                                        
017600       IF W-RIF-IDPTYP = 'RKB' OR 'RKC' OR 'RKD'                          
017700         OR 'RKE' OR 'RKF'                                                
017800         IF W-RIF-IDPTYP = 'RKB'                                          
017900           MOVE 7836 TO W-RKB-IDDISTR                                     
017901           WRITE RKB-POST FROM W-ARBAREA                                  
017902         ELSE                                                             
017903           IF W-RIF-IDPTYP = 'RKD'                                        
017904             MOVE 7836 TO W-RKD-IDDISTR                                   
017905             WRITE RKD-POST FROM W-ARBAREA                                
017906           ELSE                                                           
017907             IF W-RIF-IDPTYP = 'RKE'                                      
017908               MOVE 7836 TO W-RKE-IDDISTR                                 
017909               WRITE UTPOST FROM W-ARBAREA                                
017910             ELSE                                                         
017911               IF W-RIF-IDPTYP = 'RKC'                                    
017913                 WRITE RKC-POST FROM INPOST                               
017914               ELSE                                                       
017915                 WRITE UTPOST FROM INPOST                                 
017916               END-IF                                                     
017917             END-IF                                                       
017918           END-IF                                                         
017919         END-IF                                                           
017920         ADD     +1           TO W-ANT-POSTER                             
017921         PERFORM S01-LAS-W46131                                           
017922       ELSE                                                               
017923         PERFORM S01-LAS-W46131                                           
017924       END-IF                                                             
017925     END-PERFORM                                                          
017926     PERFORM BB-SLUT-KORT                                                 
017927     .                                                                    
017928     EJECT                                                                
017930                                                                          
018900 BA-START-KORT SECTION.                                                   
019000     SKIP2                                                                
019100     MOVE     'RI0'          TO START-IDPTYP                              
019200     MOVE     7836           TO START-IDDISTR                             
019300     MOVE     WC-CDC-SE      TO START-IDDC                                
019400     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019500*                                                                         
019600     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019700*                                                                         
019800     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
019900     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
020100     .                                                                    
020200     SKIP2                                                                
020300 BB-SLUT-KORT SECTION.                                                    
020400     SKIP2                                                                
020500     MOVE     'RI9'          TO SLUT-IDPTYP                               
020600     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020700*                                                                         
020800     WRITE    UTPOST         FROM  SLUT-W461RI9                           
021000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
021100     .                                                                    
021200     SKIP2                                                                
021300 S01-LAS-W46131 SECTION.                                                  
021400     SKIP2                                                                
021500     READ   W46131 INTO W-ARBAREA                                         
021600     AT END MOVE JA TO W46131-EOF                                         
021700     END-READ                                                             
021800                                                                          
021900     IF W46131-EOF = NEJ                                                  
022000                                                                          
022100       MOVE 'W46131'            TO POSTSUM-FDNAMN                         
022200       MOVE 'W46131D1'          TO POSTSUM-DDNAMN2                        
022300       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022400       CALL POSTSUM             USING POSTSUM-PARM                        
022500                                                                          
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 Z-FINIT SECTION.                                                         
023000     SKIP2                                                                
023100                                                                          
023200     CLOSE W46131 W46152                                                  
023300     SKIP2                                                                
023400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023500*                                    SKRIVNA POSTER                       
023600                                                                          
023700     MOVE 'S' TO POSTSUM-OPKOD                                            
023800     CALL POSTSUM USING POSTSUM-PARM                                      
023900     .                                                                    
