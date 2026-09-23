000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4613D00.                                                 
000400*AUTHOR.        STINA MOGREN.                                             
000500*DATE-WRITTEN.  NOV 2007.                                                 
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (PORTUGAL)                          
001110*        FILER INNEHÅLLER TRANSAR FÖR BÅDE DISTRIKT                       
001120*        1958                                                             
001200*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001300*        SKAPAR START OCH SLUTKORT                                        
001400*                                                                         
001500*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001600*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001700*        (ENDAST ENGELSK TEXT).                                           
001800*                                                                         
001810*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*- - - - - - - - - - - - INFIL:                                           
002700*                        - -  FIL TILL VIPS                               
002800     SELECT W4613D                       ASSIGN TO UT-S-W4613DD1.         
002900     SKIP2                                                                
003000*- - - - - - - - - - - - UTFIL:                                           
003100*                        - -  FIL TILL VIPS                               
003200     SELECT W461PT                       ASSIGN TO UT-S-W4613DD2.         
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W4613D                                                               
003900     RECORDING      V                                                     
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP2                                                                
004200 01  INPOST               PIC X(211).                                     
004300                                                                          
005150 FD  W461PT                                                               
005160     RECORDING       V                                                    
005170     BLOCK CONTAINS 0.                                                    
005180 01  UTPOST                       PIC X(80).                              
005181     SKIP2                                                                
005182 01  RID-POST      -COPY W461RIDN     -L.                                 
005183     SKIP2                                                                
005184 01  RIE-POST      -COPY W461RIEN     -L.                                 
005185     SKIP2                                                                
005186 01  RIH-POST      -COPY W461RIHN     -L.                                 
005187     SKIP2                                                                
005188 01  RIO-POST      -COPY W461RIO2     -L.                                 
005189     SKIP2                                                                
005190 01  RKB-POST      -COPY W461RKBN     -L.                                 
005191     SKIP2                                                                
005192 01  RKC-POST      -COPY W461RKCN     -L.                                 
005193     SKIP2                                                                
005194 01  RKD-POST      -COPY W461RKDN     -L.                                 
005195     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005201                                                                          
005210*    -- CHECKED BY WY2000                                                 
005300*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
005400 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613D00'.            
005500     SKIP2                                                                
005600*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
005700                                                                          
005800 77  JA                          PIC X(1)    VALUE 'J'.                   
005900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006000     SKIP2                                                                
006100*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
006200                                                                          
006300 77  W4613D-EOF                  PIC X(1)    VALUE 'N'.                   
006400     SKIP2                                                                
006500*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
006600                                                                          
006700 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
006800 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
006900                                                                          
007000*- - - - - - - - - - - - - -                                              
007100                                                                          
007200     EJECT                                                                
007210*      --- VALID IDDC CODES                                               
007220*                                                                         
007230*01    -COPY WWDCKONS                                                     
007240       EJECT                                                              
007300 01  DAGENS-DATUM.                                                        
007400   03  DAGENS-DATUM-AR           PIC 9(2).                                
007500   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
007600   03  DAGENS-DATUM-DAG          PIC 9(2).                                
007700                                                                          
007800 01  DAGENS-TID.                                                          
007900   03  DAGENS-TID-TIM            PIC 9(2).                                
008000   03  DAGENS-TID-MIN            PIC 9(2).                                
008100   03  DAGENS-TID-SEK            PIC 9(2).                                
008200                                                                          
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
008500   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
008600   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
008700     SKIP3                                                                
008800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
008900                                                                          
009000 01  RETURKODER.                                                          
009100   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
009200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
009300   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
009400     EJECT                                                                
009500*- - - - - - - - - - - - - -  ARBETSAREAOR                                
009600                                                                          
009700******************************************************************        
009800                                                                          
009900 01  W-ARBAREA.                                                           
010000 03  W-ARBAREA-X                 PIC X(211).                              
010100     SKIP2                                                                
010200*                                                                         
011300*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011400     EJECT                                                                
012000                                                                          
012001*01  -COPY W461RIFN        -PRE JFR-.                                     
012002     EJECT                                                                
012003*                             STARTKORT                                   
012004*01  -COPY W461RI0N                                                       
012005     EJECT                                                                
012006*                             SLUTKORT                                    
012007*01  -COPY W461RI9                                                        
012008     EJECT                                                                
012009*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
012010                                                                          
012020 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012030     SKIP2                                                                
012040*01  -COPY WDATKORT                                                       
012050     EJECT                                                                
012060*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
012070                                                                          
012080*01  -COPY W0005       -PRE  POSTSUM-.                                    
012090     EJECT                                                                
012100 PROCEDURE DIVISION.                                                      
012200     SKIP2                                                                
012300     PERFORM A-INIT                                                       
012400     PERFORM B-BEHANDLA                                                   
012500     PERFORM Z-FINIT                                                      
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900                                                                          
013000     SKIP3                                                                
013100 A-INIT SECTION.                                                          
013200     SKIP2                                                                
013300     OPEN INPUT W4613D OUTPUT W461PT                                      
013400     SKIP2                                                                
013500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
013600     SKIP2                                                                
013700*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
013800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013900     MOVE D-AAR TO DAGENS-DATUM-AR                                        
014000     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
014100     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
014200     SKIP2                                                                
014300*- - - - - - - - - - - - - - - - TID                                      
014400*                                                                         
014500     ACCEPT   DAGENS-TID FROM TIME                                        
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 B-BEHANDLA SECTION.                                                      
015000     SKIP2                                                                
015100     PERFORM BA-START-KORT                                                
015200     PERFORM S01-LAS-W4613D                                               
015300     PERFORM UNTIL                                                        
015400      NOT ( W4613D-EOF = NEJ )                                            
015500       IF W-RIF-IDPTYP  = 'RIF'                                           
015600         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
015700         PERFORM S01-LAS-W4613D                                           
015800       ELSE                                                               
015900         IF JFR-RIF-IDPTYP   = 'RIF'                                      
016100           ADD     +1           TO W-ANT-POSTER                           
016200           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
016300           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
016400         END-IF                                                           
016500         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ'                                 
016600           PERFORM S01-LAS-W4613D                                         
016700         ELSE                                                             
016800           EVALUATE    W-RIF-IDPTYP                                       
016900                                                                          
017000             WHEN  'RID'                                                  
017100                  WRITE RID-POST FROM INPOST                              
017200                                                                          
017300             WHEN  'RIE'                                                  
017510                  WRITE RIE-POST FROM INPOST                              
018901                                                                          
018902             WHEN  'RIH'                                                  
018903                  WRITE RIH-POST FROM INPOST                              
018905                                                                          
018906             WHEN  'RIO'                                                  
018907                  WRITE RIO-POST FROM INPOST                              
018908                                                                          
018909             WHEN  'RKB'                                                  
018910                  WRITE RKB-POST  FROM INPOST                             
018911                                                                          
018912             WHEN  'RKC'                                                  
018913                  WRITE RKC-POST  FROM INPOST                             
018914                                                                          
018915             WHEN  'RKD'                                                  
018916                  WRITE RKD-POST  FROM INPOST                             
018917                                                                          
018918             WHEN  OTHER                                                  
018919                  WRITE UTPOST   FROM INPOST                              
018920                                                                          
019008           END-EVALUATE                                                   
019009           ADD     +1           TO W-ANT-POSTER                           
019011           PERFORM S01-LAS-W4613D                                         
019012         END-IF                                                           
019013       END-IF                                                             
019014     END-PERFORM                                                          
019015     PERFORM BB-SLUT-KORT                                                 
019016     .                                                                    
019017     EJECT                                                                
019018                                                                          
019490                                                                          
019491 BA-START-KORT SECTION.                                                   
019492     SKIP2                                                                
019493     MOVE     'RI0'          TO START-IDPTYP                              
019494     MOVE     1958           TO START-IDDISTR                             
019495     MOVE     WC-CDC-SE      TO START-IDDC                                
019496     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019497*                                                                         
019500     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019600*                                                                         
019700     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
019800     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
019900     .                                                                    
020000     SKIP2                                                                
020100 BB-SLUT-KORT SECTION.                                                    
020200     SKIP2                                                                
020300     MOVE     'RI9'          TO SLUT-IDPTYP                               
020400     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020500*                                                                         
020600     WRITE    UTPOST         FROM  SLUT-W461RI9                           
020700     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
020800     .                                                                    
020900     SKIP2                                                                
021000 S01-LAS-W4613D SECTION.                                                  
021100     SKIP2                                                                
021200     READ   W4613D INTO W-ARBAREA                                         
021300     AT END MOVE JA TO W4613D-EOF                                         
021400     END-READ                                                             
021500                                                                          
021600     IF W4613D-EOF = NEJ                                                  
021700                                                                          
021800       MOVE 'W4613D'            TO POSTSUM-FDNAMN                         
021900       MOVE 'W4613DD1'          TO POSTSUM-DDNAMN2                        
022000       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022100       CALL POSTSUM             USING POSTSUM-PARM                        
022200                                                                          
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 Z-FINIT SECTION.                                                         
022700     SKIP2                                                                
022800                                                                          
022900     CLOSE W4613D W461PT                                                  
023000     SKIP2                                                                
023100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023200*                                    SKRIVNA POSTER                       
023300                                                                          
023400     MOVE 'S' TO POSTSUM-OPKOD                                            
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     .                                                                    
