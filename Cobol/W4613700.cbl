000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613700.                                                 
001000*AUTHOR.        AGNETA V H.                                               
001100*DATE-WRITTEN.  JUNI 1989                                                 
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION: *****************                                          
001600                                                                          
001700*        IMPORTÖRSBEROENDE BEHANDLING (ENGLAND)                           
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002100                                                                          
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (ENDAST ENGELSK TEXT).                                           
002500*    ABENDKODER:                                                          
002600                                                                          
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - INFIL:                                           
003500*                        - -  FIL TILL VIPS                               
003600     SELECT W46137                       ASSIGN TO UT-S-W46137D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46157                       ASSIGN TO UT-S-W46137D2.         
004100     SELECT W4615H                       ASSIGN TO UT-S-W46137D3.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W46137                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005020 01  INPOST         PIC X(80).                                            
005030     EJECT                                                                
005350 FD  W46157                                                               
005400     RECORDING      V                                                     
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005610     SKIP2                                                                
005620 01  RID-POST      -COPY W461RIDN       -L.                               
005630     SKIP2                                                                
005640 01  RIE-POST      -COPY W461RIEN       -L.                               
005650     SKIP2                                                                
005660 01  RIH-POST      -COPY W461RIHN       -L.                               
005670     SKIP2                                                                
005680 01  RIO-POST      -COPY W461RIO2       -L.                               
005700     SKIP2                                                                
005800 FD  W4615H                                                               
005900     RECORDING      V                                                     
006000     BLOCK CONTAINS 0.                                                    
006100 01  UTPOST2                      PIC X(80).                              
006110     SKIP2                                                                
006120 01  RID-POST      -COPY W461RIDN       -L.                               
006130     SKIP2                                                                
006140 01  RIE-POST      -COPY W461RIEN       -L.                               
006150     SKIP2                                                                
006160 01  RIH-POST      -COPY W461RIHN       -L.                               
006170     SKIP2                                                                
006180 01  RIO-POST      -COPY W461RIO2       -L.                               
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006310                                                                          
006400*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613700'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  W46137-EOF                  PIC X(1)    VALUE 'N'.                   
008100     SKIP2                                                                
008200*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008300                                                                          
008400 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008500 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
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
015500     OPEN INPUT W46137 OUTPUT W46157 W4615H                               
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
017400     PERFORM S01-LAS-W46137                                               
017500     PERFORM UNTIL                                                        
017600      NOT ( W46137-EOF = NEJ )                                            
017700       IF W-RIF-IDPTYP =  'RKF'                                           
017888         WRITE   UTPOST   FROM INPOST                                     
017889         WRITE   UTPOST2  FROM INPOST                                     
017890         ADD     +1           TO W-ANT-POSTER                             
017894       END-IF                                                             
017895       PERFORM S01-LAS-W46137                                             
017896     END-PERFORM                                                          
017897     PERFORM BB-SLUT-KORT                                                 
017898     .                                                                    
017899     EJECT                                                                
017900                                                                          
018900 BA-START-KORT SECTION.                                                   
019000     SKIP2                                                                
019100     MOVE     'RI0'          TO START-IDPTYP                              
019200     MOVE     1312           TO START-IDDISTR                             
019300     MOVE     WC-CDC-SE      TO START-IDDC                                
019400     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019500*                                                                         
019600     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019700*                                                                         
019800     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
019900     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
020000     WRITE    UTPOST2        FROM  START-W461RI0N-CTX                     
020100     .                                                                    
020200     SKIP2                                                                
020300 BB-SLUT-KORT SECTION.                                                    
020400     SKIP2                                                                
020500     MOVE     'RI9'          TO SLUT-IDPTYP                               
020600     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020700*                                                                         
020800     WRITE    UTPOST         FROM  SLUT-W461RI9                           
020900     WRITE    UTPOST2        FROM  SLUT-W461RI9                           
021000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
021100     .                                                                    
021200     SKIP2                                                                
021300 S01-LAS-W46137 SECTION.                                                  
021400     SKIP2                                                                
021500     READ   W46137 INTO W-ARBAREA                                         
021600     AT END MOVE JA TO W46137-EOF                                         
021700     END-READ                                                             
021800                                                                          
021900     IF W46137-EOF = NEJ                                                  
022000                                                                          
022100       MOVE 'W46137'            TO POSTSUM-FDNAMN                         
022200       MOVE 'W46137D1'          TO POSTSUM-DDNAMN2                        
022300       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022400       CALL POSTSUM             USING POSTSUM-PARM                        
022500                                                                          
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 Z-FINIT SECTION.                                                         
023000     SKIP2                                                                
023100                                                                          
023200     CLOSE W46137 W46157 W4615H                                           
023300     SKIP2                                                                
023400*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023500*                                    SKRIVNA POSTER                       
023600                                                                          
023700     MOVE 'S' TO POSTSUM-OPKOD                                            
023800     CALL POSTSUM USING POSTSUM-PARM                                      
023900     .                                                                    
