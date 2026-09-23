000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4617100.                                                 
000400*AUTHOR.        THOMAS NILSSON.                                           
000500*DATE-WRITTEN.  OCTOBER  1987.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (TYSKLAND-PV)                       
001200*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001300*        SKAPAR START OCH SLUTKORT                                        
001400*                                                                         
001500*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001600*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001700*        (ENDAST ENGELSK TEXT).                                           
001800*                                                                         
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*- - - - - - - - - - - - INFIL:                                           
002700*                        - -  FIL TILL VIPS                               
002800     SELECT W46171                       ASSIGN TO UT-S-W46171D1.         
002900     SKIP2                                                                
003000*- - - - - - - - - - - - UTFIL:                                           
003100*                        - -  FIL TILL VIPS                               
003200     SELECT W46191                       ASSIGN TO UT-S-W46171D2.         
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W46171                                                               
003900     RECORDING      V                                                     
004000     BLOCK CONTAINS 0.                                                    
004100     SKIP2                                                                
004200 01  INPOST          PIC X(211).                                          
004300     SKIP2                                                                
004400 FD  W46191                                                               
004500     RECORDING      V                                                     
004600     BLOCK CONTAINS 0.                                                    
004700 01  UTPOST                       PIC X(80).                              
004800     SKIP2                                                                
004900 01  RID-POST  -COPY W461RIDN      -L.                                    
005000     SKIP2                                                                
005100 01  RIE-POST  -COPY W461RIEN      -L.                                    
005200     SKIP2                                                                
005300 01  RIH-POST  -COPY W461RIHN      -L.                                    
005400     SKIP2                                                                
005500 01  RIO-POST  -COPY W461RIO2      -L.                                    
005600     SKIP2                                                                
005700 01  RKB-POST  -COPY W461RKBN      -L.                                    
005800     SKIP2                                                                
005900 01  RKC-POST  -COPY W461RKCN      -L.                                    
006000     SKIP2                                                                
006100 01  RKD-POST  -COPY W461RKDN      -L.                                    
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006700 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4617100'.            
006800     SKIP2                                                                
006900*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007000                                                                          
007100 77  JA                          PIC X(1)    VALUE 'J'.                   
007200 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007500                                                                          
007600 77  W46171-EOF                  PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
007900                                                                          
008000 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008100 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008200                                                                          
008300*- - - - - - - - - - - - - -                                              
008400                                                                          
008500     EJECT                                                                
008600 01  DAGENS-DATUM.                                                        
008700   03  DAGENS-DATUM-AR           PIC 9(2).                                
008800   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
008900   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009000                                                                          
009100 01  DAGENS-TID.                                                          
009200   03  DAGENS-TID-TIM            PIC 9(2).                                
009300   03  DAGENS-TID-MIN            PIC 9(2).                                
009400   03  DAGENS-TID-SEK            PIC 9(2).                                
009500*      --- VALID IDDC CODES                                               
009600*                                                                         
009700*01    -COPY WWDCKONS                                                     
009800       EJECT                                                              
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
012000*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012100     EJECT                                                                
012200                                                                          
012300*01  -COPY W461RIFN        -PRE JFR-.                                     
012400     EJECT                                                                
012500*                             STARTKORT                                   
012600*01  -COPY W461RI0N                                                       
012700     EJECT                                                                
012800*                             SLUTKORT                                    
012900*01  -COPY W461RI9                                                        
013000     EJECT                                                                
013100*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
013200                                                                          
013300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013400     SKIP2                                                                
013500*01  -COPY WDATKORT                                                       
013600     EJECT                                                                
013700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013800                                                                          
013900*01  -COPY W0005       -PRE  POSTSUM-.                                    
014000     EJECT                                                                
014100 PROCEDURE DIVISION.                                                      
014200     SKIP2                                                                
014210                                                                          
014300     PERFORM A-INIT                                                       
014400     PERFORM B-BEHANDLA                                                   
014500     PERFORM Z-FINIT                                                      
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900                                                                          
015000     SKIP3                                                                
015100 A-INIT SECTION.                                                          
015200     SKIP2                                                                
015300     OPEN INPUT W46171 OUTPUT W46191                                      
015400     SKIP2                                                                
015500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015600     SKIP2                                                                
015700*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015900     MOVE D-AAR TO DAGENS-DATUM-AR                                        
016000     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
016100     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
016200     SKIP2                                                                
016300*- - - - - - - - - - - - - - - - TID                                      
016400*                                                                         
016500     ACCEPT   DAGENS-TID FROM TIME                                        
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 B-BEHANDLA SECTION.                                                      
017000     SKIP2                                                                
017100     PERFORM BA-START-KORT                                                
017200     PERFORM S01-LAS-W46171                                               
017300     PERFORM UNTIL                                                        
017400      NOT ( W46171-EOF = NEJ )                                            
017600       IF W-RIF-IDPTYP  = 'RIF'                                           
017700         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
017800         PERFORM S01-LAS-W46171                                           
017900       ELSE                                                               
018000         IF JFR-RIF-IDPTYP   = 'RIF'                                      
018100           ADD     +1           TO W-ANT-POSTER                           
018200           WRITE UTPOST FROM JFR-RIF-W461RIFN-CTX                         
018300           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
018400         END-IF                                                           
018500         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RIS' OR 'RKA'               
018600           PERFORM S01-LAS-W46171                                         
018700         ELSE                                                             
018800           EVALUATE    W-RIF-IDPTYP                                       
018900                                                                          
019000             WHEN  'RID'                                                  
019100                  WRITE RID-POST  FROM INPOST                             
019200                                                                          
019300             WHEN  'RIE'                                                  
019400                  WRITE RIE-POST  FROM INPOST                             
019500                                                                          
019600             WHEN  'RIH'                                                  
019700                  WRITE RIH-POST  FROM INPOST                             
019800                                                                          
019900             WHEN  'RIO'                                                  
020000                  WRITE RIO-POST  FROM INPOST                             
020100                                                                          
020200             WHEN  'RKB'                                                  
020300                  WRITE RKB-POST  FROM INPOST                             
020400                                                                          
020500             WHEN  'RKC'                                                  
020600                  WRITE RKC-POST  FROM INPOST                             
020700                                                                          
020800             WHEN  'RKD'                                                  
020900                  WRITE RKD-POST  FROM INPOST                             
021000                                                                          
021100             WHEN  OTHER                                                  
021200                  WRITE UTPOST    FROM INPOST                             
021300                                                                          
021400           END-EVALUATE                                                   
021500***        MOVE SPACE      TO INPOST                                      
021600                                                                          
021700           ADD     +1           TO W-ANT-POSTER                           
021800           PERFORM S01-LAS-W46171                                         
021900         END-IF                                                           
022000       END-IF                                                             
022100     END-PERFORM                                                          
022200     PERFORM BB-SLUT-KORT                                                 
022300     .                                                                    
022400     EJECT                                                                
022500                                                                          
022600 BA-START-KORT SECTION.                                                   
022700     SKIP2                                                                
022800     MOVE     'RI0'          TO START-IDPTYP                              
022900     MOVE     2278           TO START-IDDISTR                             
023000     MOVE     WC-CDC-SE      TO START-IDDC                                
023100     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
023200*                                                                         
023300     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
023400*                                                                         
023500     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
023600     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
023700     .                                                                    
023800     SKIP2                                                                
023900 BB-SLUT-KORT SECTION.                                                    
024000     SKIP2                                                                
024100     MOVE     'RI9'          TO SLUT-IDPTYP                               
024200     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
024300*                                                                         
024400     WRITE    UTPOST         FROM  SLUT-W461RI9                           
024500     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
024600     .                                                                    
024700     SKIP2                                                                
024800 S01-LAS-W46171 SECTION.                                                  
024900     SKIP2                                                                
025000     READ   W46171 INTO W-ARBAREA                                         
025100     AT END MOVE JA TO W46171-EOF                                         
025200     END-READ                                                             
025210                                                                          
025400     IF W46171-EOF = NEJ                                                  
025500                                                                          
025600       MOVE 'W46171'            TO POSTSUM-FDNAMN                         
025700       MOVE 'W46171D1'          TO POSTSUM-DDNAMN2                        
025800       MOVE SPACE               TO POSTSUM-TRANSTYP                       
025900       CALL POSTSUM             USING POSTSUM-PARM                        
026000                                                                          
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 Z-FINIT SECTION.                                                         
026500     SKIP2                                                                
026600                                                                          
026700     CLOSE W46171 W46191                                                  
026800     SKIP2                                                                
026900*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
027000*                                    SKRIVNA POSTER                       
027100                                                                          
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
