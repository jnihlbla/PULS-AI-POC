000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613800.                                                 
001000*AUTHOR.        KATARINA KYMMER                                           
001100*DATE-WRITTEN.  MARS 1990                                                 
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION: *****************                                          
001600                                                                          
001700*        IMPORTÖRSBEROENDE BEHANDLING (USA)                               
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIC SKALL EJ SKRIVAS                                          
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
003600     SELECT W46138                       ASSIGN TO UT-S-W46138D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46158                       ASSIGN TO UT-S-W46138D2.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46138                                                               
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000 01  INPOST                       PIC X(80).                              
005100     SKIP2                                                                
005200 FD  W46158                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005600     SKIP2                                                                
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005810                                                                          
005900*    -- CHECKED BY WY2000                                                 
006400*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613800'.            
006700     SKIP2                                                                
006800*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006900                                                                          
007000 77  JA                          PIC X(1)    VALUE 'J'.                   
007100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007400                                                                          
007500 77  W46138-EOF                  PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
007800                                                                          
007900 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008000 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008100                                                                          
008200*- - - - - - - - - - - - - -                                              
008300                                                                          
008400     EJECT                                                                
008500 01  DAGENS-DATUM.                                                        
008600   03  DAGENS-DATUM-AR           PIC 9(2).                                
008700   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
008800   03  DAGENS-DATUM-DAG          PIC 9(2).                                
008900                                                                          
009000 01  DAGENS-TID.                                                          
009100   03  DAGENS-TID-TIM            PIC 9(2).                                
009200   03  DAGENS-TID-MIN            PIC 9(2).                                
009300   03  DAGENS-TID-SEK            PIC 9(2).                                
009400                                                                          
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009700   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
009800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009900     SKIP3                                                                
010000*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010100                                                                          
010200 01  RETURKODER.                                                          
010300   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010400   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010500   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010600     EJECT                                                                
010700*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010800                                                                          
010900 01  W-ARBAREA.                                                           
011000     SKIP2                                                                
011100*                                                                         
011200*03  -COPY W461RIF0        -PRE W-.                                       
011400     EJECT                                                                
011500*01  -COPY W461RIF0        -PRE JFR-.                                     
011700     EJECT                                                                
011800*                             STARTKORT                                   
011900*01  -COPY W461RI0                                                        
012100     EJECT                                                                
012200*                             SLUTKORT                                    
012300*01  -COPY W461RI9                                                        
012500     EJECT                                                                
012600*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
012700                                                                          
012800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012900     SKIP2                                                                
013000*01  -COPY WDATKORT                                                       
013200     EJECT                                                                
013300*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
013400                                                                          
013500*01  -COPY W0005       -PRE  POSTSUM-.                                    
013700     EJECT                                                                
013800 PROCEDURE DIVISION.                                                      
013900     SKIP2                                                                
014000     PERFORM A-INIT                                                       
014100     PERFORM B-BEHANDLA                                                   
014200     PERFORM Z-FINIT                                                      
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600                                                                          
014700     SKIP3                                                                
014800 A-INIT SECTION.                                                          
014900     SKIP2                                                                
015000     OPEN INPUT W46138 OUTPUT W46158                                      
015100     SKIP2                                                                
015200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
015300     SKIP2                                                                
015400*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
015500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015600     MOVE D-AAR TO DAGENS-DATUM-AR                                        
015700     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
015800     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
015900     SKIP2                                                                
016000*- - - - - - - - - - - - - - - - TID                                      
016100*                                                                         
016200     ACCEPT   DAGENS-TID FROM TIME                                        
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600 B-BEHANDLA SECTION.                                                      
016700     SKIP2                                                                
016800     PERFORM BA-START-KORT                                                
016900     PERFORM S01-LAS-W46138                                               
017000     PERFORM UNTIL                                                        
017100      NOT ( W46138-EOF = NEJ )                                            
017200       IF W-RIF-IDPTYP = 'RKA' OR 'RIS'                                   
017300         ADD     +1           TO W-ANT-POSTER                             
017400         WRITE   UTPOST   FROM INPOST                                     
017500         PERFORM S01-LAS-W46138                                           
017600       ELSE                                                               
017700         PERFORM S01-LAS-W46138                                           
017800       END-IF                                                             
017900     END-PERFORM                                                          
018000     PERFORM BB-SLUT-KORT                                                 
018100     .                                                                    
018200     EJECT                                                                
018300 BA-START-KORT SECTION.                                                   
018400     SKIP2                                                                
018500     MOVE     'RI0'          TO START-IDPTYP                              
018600     MOVE     7512           TO START-IDDISTR                             
018700     MOVE     1              TO START-KDCLAGER                            
018800     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
018900*                                                                         
019000     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019100*                                                                         
019200     DISPLAY  'START-KORT  ' START-W461RI0                                
019300     WRITE    UTPOST         FROM  START-W461RI0                          
019400     .                                                                    
019500     SKIP2                                                                
019600 BB-SLUT-KORT SECTION.                                                    
019700     SKIP2                                                                
019800     MOVE     'RI9'          TO SLUT-IDPTYP                               
019900     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020000*                                                                         
020100     WRITE    UTPOST         FROM  SLUT-W461RI9                           
020200     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
020300     .                                                                    
020400     SKIP2                                                                
020500 S01-LAS-W46138 SECTION.                                                  
020600     SKIP2                                                                
020700     READ   W46138 INTO W-ARBAREA                                         
020800     AT END MOVE JA TO W46138-EOF                                         
020900     END-READ                                                             
021000                                                                          
021100     IF W46138-EOF = NEJ                                                  
021200                                                                          
021300       MOVE 'W46138'            TO POSTSUM-FDNAMN                         
021400       MOVE 'W46138D1'          TO POSTSUM-DDNAMN2                        
021500       MOVE SPACE               TO POSTSUM-TRANSTYP                       
021600       CALL POSTSUM             USING POSTSUM-PARM                        
021700                                                                          
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 Z-FINIT SECTION.                                                         
022200     SKIP2                                                                
022300                                                                          
022400     CLOSE W46138 W46158                                                  
022500     SKIP2                                                                
022600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
022700*                                    SKRIVNA POSTER                       
022800                                                                          
022900     MOVE 'S' TO POSTSUM-OPKOD                                            
023000     CALL POSTSUM USING POSTSUM-PARM                                      
023100     .                                                                    
