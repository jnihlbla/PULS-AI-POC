000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4612800.                                                 
001000*AUTHOR.        LASSI OLGRENER.                                           
001100*DATE-WRITTEN.  JAN 1996.                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (FINLAND-1091)                      
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIC OCH RIT SKALL EJ SKRIVAS                                  
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
003400*- - - - - - - - - - - - INFIL: W4612Z OCH W4618A                         
003600     SELECT INFIL                        ASSIGN TO UT-S-W46128D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46148                       ASSIGN TO UT-S-W46128D2.         
004100     SELECT W4614A                       ASSIGN TO UT-S-W46128D3.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  INFIL                                                                
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005020 01  INPOST          PIC X(211).                                          
005030     SKIP2                                                                
005350 FD  W46148                                                               
005400     RECORDING      V                                                     
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005710     SKIP2                                                                
005720 01  RID-POST  -COPY W461RIDN     -L.                                     
005730     SKIP2                                                                
005740 01  RIE-POST  -COPY W461RIEN     -L.                                     
005750     SKIP2                                                                
005760 01  RIH-POST  -COPY W461RIHN     -L.                                     
005770     SKIP2                                                                
005780 01  RIO-POST  -COPY W461RIO2     -L.                                     
005790     SKIP2                                                                
005791 01  RKB-POST  -COPY W461RKBN     -L.                                     
005792     SKIP2                                                                
005793 01  RKC-POST  -COPY W461RKCN     -L.                                     
005794     SKIP2                                                                
005795 01  RKD-POST  -COPY W461RKDN     -L.                                     
005796     SKIP2                                                                
005800 FD  W4614A                                                               
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
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612800'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  INFIL-EOF                   PIC X(1)    VALUE 'N'.                   
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
010200   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
010300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
011100     EJECT                                                                
011200*- - - - - - - - - - - - - -  ARBETSAREAOR                                
011300                                                                          
011400******************************************************************        
011500                                                                          
011600 01  W-ARBAREA.                                                           
011700 03  W-ARBAREA-X                 PIC X(211).                              
011800     SKIP2                                                                
011900*                                                                         
012280*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012290     EJECT                                                                
012430*                             STARTKORT                                   
012500*01  -COPY W461RI0N                                                       
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
015500     OPEN INPUT INFIL OUTPUT W46148 W4614A                                
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
017400     PERFORM S01-LAS-INFIL                                                
017500     PERFORM UNTIL INFIL-EOF = JA                                         
017700       IF W-RIF-IDPTYP  = 'RIK' OR 'RIL' OR 'RIM' OR                      
017800                          'RIN' OR 'RIO' OR 'RIQ' OR 'RKO'                
017900         EVALUATE      W-RIF-IDPTYP                                       
018000                                                                          
019003           WHEN    'RIO'                                                  
019004                WRITE RIO-POST  FROM INPOST                               
019005                WRITE RIO-POST2 FROM INPOST                               
019006                                                                          
019021           WHEN    OTHER                                                  
019022                WRITE UTPOST  FROM INPOST                                 
019023                WRITE UTPOST2 FROM INPOST                                 
019075         END-EVALUATE                                                     
019076         ADD     +1           TO W-ANT-POSTER                             
019079       END-IF                                                             
019080       PERFORM S01-LAS-INFIL                                              
019081     END-PERFORM                                                          
019082     PERFORM BB-SLUT-KORT                                                 
019083     .                                                                    
019084     EJECT                                                                
019085                                                                          
020000 BA-START-KORT SECTION.                                                   
020100     SKIP2                                                                
020200     MOVE     'RI0'          TO START-IDPTYP                              
020300     MOVE     1091           TO START-IDDISTR                             
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
022400 S01-LAS-INFIL SECTION.                                                   
022500     SKIP2                                                                
022600     READ   INFIL INTO W-ARBAREA                                          
022700     AT END MOVE JA TO INFIL-EOF                                          
022800     END-READ                                                             
022900                                                                          
023000     IF INFIL-EOF = NEJ                                                   
023100                                                                          
023200       MOVE 'INFIL'             TO POSTSUM-FDNAMN                         
023300       MOVE 'W46128D1'          TO POSTSUM-DDNAMN2                        
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
024400     CLOSE INFIL W46148 W4614A                                            
024500     SKIP2                                                                
024600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
024700*                                    SKRIVNA POSTER                       
024800                                                                          
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
