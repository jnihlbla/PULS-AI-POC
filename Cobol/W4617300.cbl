000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4617300.                                                 
001000*AUTHOR.        BOSSE BERNTSSON.                                          
001100*DATE-WRITTEN.  SEP 1985.                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (FRANKRIKE-PV)                      
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        FÖR PT RIC GÄLLER ATT DE EJ SKALL SKRIVAS                        
002100*                                                                         
002200*        FÖR PT RIF GÄLLER ATT ENDAST SISTA "RIF KORTET"                  
002300*        SKALL SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                 
002400*        (ENDAST ENGELSK TEXT).                                           
002500*    ABENDKODER:                                                          
002600*                                                                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - INFIL:                                           
003500*                        - -  FIL TILL VIPS                               
003600     SELECT W46173                       ASSIGN TO UT-S-W46173D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46193                       ASSIGN TO UT-S-W46173D2.         
004100     SELECT W4614J                       ASSIGN TO UT-S-W46173D3.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W46173                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005010 01  INPOST          PIC X(211).                                          
005020     SKIP2                                                                
005350 FD  W46193                                                               
005400     RECORDING      V                                                     
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005610     SKIP2                                                                
005620 01  RID-POST  -COPY W461RIDN     -L.                                     
005630     SKIP2                                                                
005640 01  RIE-POST  -COPY W461RIEN     -L.                                     
005650     SKIP2                                                                
005660 01  RIH-POST  -COPY W461RIHN     -L.                                     
005670     SKIP2                                                                
005680 01  RIO-POST  -COPY W461RIO2     -L.                                     
005690     SKIP2                                                                
005691 01  RKB-POST  -COPY W461RKBN     -L.                                     
005692     SKIP2                                                                
005693 01  RKC-POST  -COPY W461RKCN     -L.                                     
005694     SKIP2                                                                
005695 01  RKD-POST  -COPY W461RKDN     -L.                                     
005700     SKIP2                                                                
005800 FD  W4614J                                                               
005900     RECORDING      V                                                     
006000     BLOCK CONTAINS 0.                                                    
006100 01  UTPOST2                      PIC X(80).                              
006110     SKIP2                                                                
006120 01  RID-POST2 -COPY W461RIDN     -L.                                     
006130     SKIP2                                                                
006140 01  RIE-POST2 -COPY W461RIEN     -L.                                     
006150     SKIP2                                                                
006160 01  RIH-POST2 -COPY W461RIHN     -L.                                     
006170     SKIP2                                                                
006180 01  RIO-POST2 -COPY W461RIO2     -L.                                     
006190     SKIP2                                                                
006191 01  RKB-POST2 -COPY W461RKBN     -L.                                     
006192     SKIP2                                                                
006193 01  RKC-POST2 -COPY W461RKCN     -L.                                     
006194     SKIP2                                                                
006195 01  RKD-POST2 -COPY W461RKDN     -L.                                     
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006310                                                                          
006400*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617300'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  W46173-EOF                  PIC X(1)    VALUE 'N'.                   
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
009400                                                                          
009500 01  DAGENS-TID.                                                          
009600   03  DAGENS-TID-TIM            PIC 9(2).                                
009700   03  DAGENS-TID-MIN            PIC 9(2).                                
009800   03  DAGENS-TID-SEK            PIC 9(2).                                
009810*      --- VALID IDDC CODES                                               
009820*                                                                         
009830*01    -COPY WWDCKONS                                                     
009840       EJECT                                                              
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
011400******************************************************************        
011500                                                                          
011600 01  W-ARBAREA.                                                           
011700 03  W-ARBAREA-X                 PIC X(211).                              
011800     SKIP2                                                                
011900*                                                                         
011992*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
011993     EJECT                                                                
012206                                                                          
012207*01  -COPY W461RIFN        -PRE JFR-.                                     
012210     EJECT                                                                
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
015500     OPEN INPUT W46173 OUTPUT W46193 W4614J                               
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
017400     PERFORM S01-LAS-W46173                                               
017500     PERFORM UNTIL                                                        
017600      NOT ( W46173-EOF = NEJ )                                            
017700       IF W-RIF-IDPTYP  = 'RIF'                                           
017800         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017900         PERFORM S01-LAS-W46173                                           
018000       ELSE                                                               
018100         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018200           ADD     +1           TO W-ANT-POSTER                           
018300           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
018400           WRITE   UTPOST2  FROM JFR-RIF-W461RIFN-CTX                     
018500           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018600         END-IF                                                           
018700         IF W-RIF-IDPTYP = 'RIC'                                          
018900           PERFORM S01-LAS-W46173                                         
019000         ELSE                                                             
019010           EVALUATE    W-RIF-IDPTYP                                       
019020                                                                          
019030             WHEN  'RID'                                                  
019040                  WRITE RID-POST  FROM INPOST                             
019041                  WRITE RID-POST2 FROM INPOST                             
019050                                                                          
019060             WHEN  'RIE'                                                  
019070                  WRITE RIE-POST  FROM INPOST                             
019071                  WRITE RIE-POST2 FROM INPOST                             
019080                                                                          
019090             WHEN  'RIH'                                                  
019100                  WRITE RIH-POST  FROM INPOST                             
019101                  WRITE RIH-POST2 FROM INPOST                             
019110                                                                          
019120             WHEN  'RIO'                                                  
019130                  WRITE RIO-POST  FROM INPOST                             
019131                  WRITE RIO-POST2 FROM INPOST                             
019132                                                                          
019133             WHEN  'RKB'                                                  
019134                  WRITE RKB-POST  FROM INPOST                             
019135                  WRITE RKB-POST2 FROM INPOST                             
019136                                                                          
019137             WHEN  'RKC'                                                  
019138                  WRITE RKC-POST  FROM INPOST                             
019139                  WRITE RKC-POST2 FROM INPOST                             
019140                                                                          
019141             WHEN  'RKD'                                                  
019142                  WRITE RKD-POST  FROM INPOST                             
019143                  WRITE RKD-POST2 FROM INPOST                             
019144                                                                          
019150             WHEN  OTHER                                                  
019160                  WRITE UTPOST  FROM INPOST                               
019170                  WRITE UTPOST2 FROM INPOST                               
019185                                                                          
019186           END-EVALUATE                                                   
019187           ADD     +1           TO W-ANT-POSTER                           
019190           PERFORM S01-LAS-W46173                                         
019191         END-IF                                                           
019192       END-IF                                                             
019193     END-PERFORM                                                          
019194     PERFORM BB-SLUT-KORT                                                 
019195     .                                                                    
019196     EJECT                                                                
019197                                                                          
020100 BA-START-KORT SECTION.                                                   
020200     SKIP2                                                                
020300     MOVE     'RI0'          TO START-IDPTYP                              
020400     MOVE     1478           TO START-IDDISTR                             
020500     MOVE     WC-CDC-SE      TO START-IDDC                                
020600     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
020700                                                                          
020800     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
020900                                                                          
021000     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
021100     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
021200     WRITE    UTPOST2        FROM  START-W461RI0N-CTX                     
021300     .                                                                    
021400     SKIP2                                                                
021500 BB-SLUT-KORT SECTION.                                                    
021600     SKIP2                                                                
021700     MOVE     'RI9'          TO SLUT-IDPTYP                               
021800     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
021900                                                                          
022000     WRITE    UTPOST         FROM  SLUT-W461RI9                           
022100     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
022200     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
022300     .                                                                    
022400     SKIP2                                                                
022500 S01-LAS-W46173 SECTION.                                                  
022600     SKIP2                                                                
022700     READ   W46173 INTO W-ARBAREA                                         
022800     AT END MOVE JA TO W46173-EOF                                         
022900     END-READ                                                             
023000                                                                          
023100     IF W46173-EOF = NEJ                                                  
023200                                                                          
023300       MOVE 'W46173'            TO POSTSUM-FDNAMN                         
023400       MOVE 'W46173D1'          TO POSTSUM-DDNAMN2                        
023500       MOVE SPACE               TO POSTSUM-TRANSTYP                       
023600       CALL POSTSUM             USING POSTSUM-PARM                        
023700                                                                          
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 Z-FINIT SECTION.                                                         
024200     SKIP2                                                                
024300                                                                          
024400     CLOSE W46173 W46193 W4614J                                           
024500     SKIP2                                                                
024600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024700*                                    SKRIVNA POSTER                       
024800                                                                          
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
