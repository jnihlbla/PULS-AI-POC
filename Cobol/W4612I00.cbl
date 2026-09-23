000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4612I00.                                                 
001000*AUTHOR.        GERRY CARMICHAEL                                          
001100*DATE-WRITTEN.  MAY 1999.                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (MALAYSIA)                          
001710*        DISTRIKT 5627 (DIRLEV) OCH 5619 (IMPORTÖR)                       
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002100*                                                                         
002200*        ENDAST SISTA "RIF KORTET"                                        
002300*        SKALL SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                 
002400*        (ENDAST ENGELSK TEXT).                                           
002500*    ABENDKODER:                                                          
002600*                                                                         
002610*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - INFIL:                                           
003500*                        - -  FIL TILL VIPS                               
003600     SELECT W4612I                       ASSIGN TO UT-S-W4612ID1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W4612T                       ASSIGN TO UT-S-W4612ID2.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W4612I                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005200     SKIP2                                                                
005210 01  INPOST                       PIC X(211).                             
005220     SKIP2                                                                
005350 FD  W4612T                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005700     SKIP2                                                                
005710*01  RID-POST  -COPY W461RIDN     -L.                                     
005720     SKIP2                                                                
005730*01  RIE-POST  -COPY W461RIEN     -L.                                     
005740     SKIP2                                                                
005750*01  RIH-POST  -COPY W461RIHN     -L.                                     
005760     SKIP2                                                                
005770*01  RIO-POST  -COPY W461RIO2     -L.                                     
005771     SKIP2                                                                
005772 01  RKB-POST  -COPY W461RKBN     -L.                                     
005773     SKIP2                                                                
005774 01  RKC-POST  -COPY W461RKCN     -L.                                     
005775     SKIP2                                                                
005776 01  RKD-POST  -COPY W461RKDN     -L.                                     
005780     SKIP2                                                                
006300 WORKING-STORAGE SECTION.                                                 
006310                                                                          
006400*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612I00'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  W4612I-EOF                  PIC X(1)    VALUE 'N'.                   
008100     SKIP2                                                                
008200*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008300                                                                          
008400 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008500 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +50000.         
008600                                                                          
008700*- - - - - - - - - - - - - -                                              
008800                                                                          
008900     EJECT                                                                
008910*      --- VALID IDDC CODES                                               
008920*                                                                         
008930*01    -COPY WWDCKONS                                                     
008940       EJECT                                                              
009000 01  DAGENS-DATUM.                                                        
009100   03  DAGENS-DATUM-AR           PIC 9(2).                                
009200   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009300   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009400                                                                          
009500 01  DAGENS-TID.                                                          
009600   03  DAGENS-TID-TIM            PIC 9(2).                                
009700   03  DAGENS-TID-MIN            PIC 9(2).                                
009800   03  DAGENS-TID-SEK            PIC 9(2).                                
009900                                                                          
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
010200   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
010400     SKIP3                                                                
010500*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010600                                                                          
010700 01  RETURKODER.                                                          
010800   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010900   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
011000   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
011100     EJECT                                                                
011200*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011300                                                                          
011310******************************************************************        
011320                                                                          
011330 01  W-ARBAREA.                                                           
011340 03  W-ARBAREA-X                 PIC X(211).                              
011350     SKIP2                                                                
011360*                                                                         
011398*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011399     EJECT                                                                
011560                                                                          
012000*01  -COPY W461RIFN        -PRE JFR-.                                     
012200     EJECT                                                                
012300*                             STARTKORT                                   
012400*01  -COPY W461RI0N                                                       
012600     EJECT                                                                
012700*                             SLUTKORT                                    
012800*01  -COPY W461RI9                                                        
013000     EJECT                                                                
013100*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013200                                                                          
013300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013400     SKIP2                                                                
013500*01  -COPY WDATKORT                                                       
013700     EJECT                                                                
013800*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013900                                                                          
014000*01  -COPY W0005       -PRE  POSTSUM-.                                    
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
015500     OPEN INPUT W4612I OUTPUT W4612T                                      
015600     SKIP2                                                                
015700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015800     SKIP2                                                                
015900*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
016000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016100     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016200     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016300     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016400     SKIP2                                                                
016500*- - - - - - - - - - - - - - - - TID                                      
016600*                                                                         
016700     ACCEPT   DAGENS-TID FROM TIME                                        
016800     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100 B-BEHANDLA SECTION.                                                      
017200     SKIP2                                                                
017300     PERFORM BA-START-KORT                                                
017400     PERFORM S01-LAS-W4612I                                               
017500     PERFORM UNTIL                                                        
017600      NOT ( W4612I-EOF = NEJ )                                            
017700       IF W-RIF-IDPTYP  = 'RIF'                                           
017800         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017900         PERFORM S01-LAS-W4612I                                           
018000       ELSE                                                               
018100         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018200           ADD     +1           TO W-ANT-POSTER                           
018300           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
018500           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018600         END-IF                                                           
018700         IF W-RIF-IDPTYP = 'RIC' OR 'RIS' OR 'RKA'                        
018710                                                                          
018800           PERFORM S01-LAS-W4612I                                         
018900         ELSE                                                             
018910           EVALUATE    W-RIF-IDPTYP                                       
018920                                                                          
018993             WHEN  'RID'                                                  
018994                  WRITE RID-POST FROM INPOST                              
018995                                                                          
018996             WHEN  'RIE'                                                  
018997                  WRITE RIE-POST FROM INPOST                              
018998                                                                          
018999             WHEN  'RIH'                                                  
019000                  WRITE RIH-POST FROM INPOST                              
019010                                                                          
019020             WHEN  'RIO'                                                  
019030                  WRITE RIO-POST FROM INPOST                              
019040                                                                          
019041             WHEN  'RKB'                                                  
019042                  WRITE RKB-POST  FROM INPOST                             
019043                                                                          
019044             WHEN  'RKC'                                                  
019045                  WRITE RKC-POST  FROM INPOST                             
019046                                                                          
019047             WHEN  'RKD'                                                  
019048                  WRITE RKD-POST  FROM INPOST                             
019049                                                                          
019050             WHEN  OTHER                                                  
019060                  WRITE UTPOST   FROM INPOST                              
019085                                                                          
019086           END-EVALUATE                                                   
019087                                                                          
019088           ADD     +1           TO W-ANT-POSTER                           
019091           PERFORM S01-LAS-W4612I                                         
019092         END-IF                                                           
019093       END-IF                                                             
019094     END-PERFORM                                                          
019095     PERFORM BB-SLUT-KORT                                                 
019096     .                                                                    
019097     EJECT                                                                
019569                                                                          
020000 BA-START-KORT SECTION.                                                   
020100     SKIP2                                                                
020200     MOVE     'RI0'          TO START-IDPTYP                              
020300     MOVE     5627           TO START-IDDISTR                             
020400     MOVE     WC-CDC-SE      TO START-IDDC                                
020500     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020600*                                                                         
020700     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020800*                                                                         
020900     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
021000     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021200     .                                                                    
021300     SKIP2                                                                
021400 BB-SLUT-KORT SECTION.                                                    
021500     SKIP2                                                                
021600     MOVE     'RI9'          TO SLUT-IDPTYP                               
021700     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021800*                                                                         
021900     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022100     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022200     .                                                                    
022300     SKIP2                                                                
022400 S01-LAS-W4612I SECTION.                                                  
022500     SKIP2                                                                
022600     READ   W4612I INTO W-ARBAREA                                         
022700     AT END MOVE JA TO W4612I-EOF                                         
022800     END-READ                                                             
022900                                                                          
023000     IF W4612I-EOF = NEJ                                                  
023100                                                                          
023200       MOVE 'W4612I'            TO POSTSUM-FDNAMN                         
023300       MOVE 'W4612ID1'          TO POSTSUM-DDNAMN2                        
023400       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023500       CALL POSTSUM             USING POSTSUM-PARM                        
023600                                                                          
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000*                                                                         
024100 Z-FINIT SECTION.                                                         
024200     SKIP2                                                                
024300                                                                          
024400     CLOSE W4612I W4612T                                                  
024500     SKIP2                                                                
024600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024700*                                    SKRIVNA POSTER                       
024800                                                                          
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
