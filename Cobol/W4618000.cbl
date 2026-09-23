000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4618000.                                                 
001000*AUTHOR.        ELAINE CURTSSON.                                          
001100*DATE-WRITTEN.  APRIL 1991.                                               
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (AUSTRALIEN DISTR 7838)             
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002100*                                                                         
002200*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
002300*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
002400*        (ENDAST ENGELSK TEXT).                                           
002500*                                                                         
002510*                                                                         
002600     EJECT                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*- - - - - - - - - - - - INFIL:                                           
003400*                        - -  FIL TILL VIPS                               
003500     SELECT W46180                       ASSIGN TO UT-S-W46180D1.         
003600     SKIP2                                                                
003700*- - - - - - - - - - - - UTFIL:                                           
003800*                        - -  FIL TILL VIPS                               
003900     SELECT W46185                       ASSIGN TO UT-S-W46180D2.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W46180                                                               
004700     RECORDING      V                                                     
004800     BLOCK CONTAINS 0.                                                    
004810     SKIP2                                                                
004900 01  INPOST                       PIC X(211).                             
005100     SKIP2                                                                
005250 FD  W46185                                                               
005300     RECORDING      V                                                     
005400     BLOCK CONTAINS 0.                                                    
005500 01  UTPOST                       PIC X(80).                              
005510     SKIP2                                                                
005520*01  RID-POST  -COPY W461RIDN     -L.                                     
005530     SKIP2                                                                
005540*01  RIE-POST  -COPY W461RIEN     -L.                                     
005550     SKIP2                                                                
005560*01  RIH-POST  -COPY W461RIHN     -L.                                     
005570     SKIP2                                                                
005580*01  RIO-POST  -COPY W461RIO2     -L.                                     
005590     SKIP2                                                                
005591 01  RKB-POST  -COPY W461RKBN     -L.                                     
005592     SKIP2                                                                
005593 01  RKC-POST  -COPY W461RKCN     -L.                                     
005594     SKIP2                                                                
005595 01  RKD-POST  -COPY W461RKDN     -L.                                     
005600     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006210                                                                          
006300*    -- CHECKED BY WY2000                                                 
006800*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4618000'.            
007100     SKIP2                                                                
007200*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007300                                                                          
007400 77  JA                          PIC X(1)    VALUE 'J'.                   
007500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007600     SKIP2                                                                
007700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007800                                                                          
007900 77  W46180-EOF                  PIC X(1)    VALUE 'N'.                   
008000     SKIP2                                                                
008100*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
008200                                                                          
008300 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
008400 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
008500                                                                          
008600*- - - - - - - - - - - - - -                                              
008700                                                                          
008800     EJECT                                                                
008900 01  DAGENS-DATUM.                                                        
009000   03  DAGENS-DATUM-AR           PIC 9(2).                                
009100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
009200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
009300                                                                          
009400 01  DAGENS-TID.                                                          
009500   03  DAGENS-TID-TIM            PIC 9(2).                                
009600   03  DAGENS-TID-MIN            PIC 9(2).                                
009700   03  DAGENS-TID-SEK            PIC 9(2).                                
009710*      --- VALID IDDC CODES                                               
009720*                                                                         
009730*01    -COPY WWDCKONS                                                     
009740       EJECT                                                              
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
012105     EJECT                                                                
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
015400     OPEN INPUT W46180 OUTPUT W46185                                      
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
016910 B-BEHANDLA SECTION.                                                      
016920     SKIP2                                                                
016930     PERFORM BA-START-KORT                                                
016940     PERFORM S01-LAS-W46180                                               
016950     PERFORM UNTIL W46180-EOF = JA                                        
016970       IF W-RIF-IDPTYP  = 'RIF'                                           
016980         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
016990         PERFORM S01-LAS-W46180                                           
016991       ELSE                                                               
016992         IF JFR-RIF-IDPTYP   = 'RIF'                                      
016993           ADD     +1           TO W-ANT-POSTER                           
016994           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
016995           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
016996         END-IF                                                           
016997         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RIS' OR 'RKA'               
016999           PERFORM S01-LAS-W46180                                         
017000         ELSE                                                             
017001           EVALUATE    W-RIF-IDPTYP                                       
017002                                                                          
017003             WHEN  'RID'                                                  
017004                  WRITE RID-POST FROM INPOST                              
017005                                                                          
017006             WHEN  'RIE'                                                  
017007                  WRITE RIE-POST FROM INPOST                              
017008                                                                          
017009             WHEN  'RIH'                                                  
017010                  WRITE RIH-POST FROM INPOST                              
017011                                                                          
017012             WHEN  'RIO'                                                  
017013                  WRITE RIO-POST FROM INPOST                              
017014                                                                          
017015             WHEN  'RKB'                                                  
017016                  WRITE RKB-POST  FROM INPOST                             
017017                                                                          
017018             WHEN  'RKC'                                                  
017019                  WRITE RKC-POST  FROM INPOST                             
017020                                                                          
017021             WHEN  'RKD'                                                  
017022                  WRITE RKD-POST  FROM INPOST                             
017024                                                                          
017025             WHEN  OTHER                                                  
017026                  WRITE UTPOST   FROM INPOST                              
017027                                                                          
017028           END-EVALUATE                                                   
017029           ADD     +1           TO W-ANT-POSTER                           
017030           PERFORM S01-LAS-W46180                                         
017031         END-IF                                                           
017032       END-IF                                                             
017033     END-PERFORM                                                          
017034     PERFORM BB-SLUT-KORT                                                 
017035     .                                                                    
017036     EJECT                                                                
017040                                                                          
019100 BA-START-KORT SECTION.                                                   
019200     SKIP2                                                                
019300     MOVE     'RI0'          TO START-IDPTYP                              
019400     MOVE     7838           TO START-IDDISTR                             
019500     MOVE     WC-CDC-SE      TO START-IDDC                                
019600     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
019700*                                                                         
019800     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
019900*                                                                         
020000     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
020100     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
020300     .                                                                    
020400     SKIP2                                                                
020500 BB-SLUT-KORT SECTION.                                                    
020600     SKIP2                                                                
020700     MOVE     'RI9'          TO SLUT-IDPTYP                               
020800     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
020900*                                                                         
021000     WRITE    UTPOST         FROM  SLUT-W461RI9                           
021200     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
021300     .                                                                    
021400     SKIP2                                                                
021500 S01-LAS-W46180 SECTION.                                                  
021600     SKIP2                                                                
021700     READ   W46180 INTO W-ARBAREA                                         
021800     AT END MOVE JA TO W46180-EOF                                         
021900     END-READ                                                             
022000                                                                          
022100     IF W46180-EOF = NEJ                                                  
022200                                                                          
022300       MOVE 'W46180'            TO POSTSUM-FDNAMN                         
022400       MOVE 'W46180D1'          TO POSTSUM-DDNAMN2                        
022500       MOVE SPACE               TO POSTSUM-TRANSTYP                       
022600       CALL POSTSUM             USING POSTSUM-PARM                        
022700                                                                          
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 Z-FINIT SECTION.                                                         
023200     SKIP2                                                                
023300                                                                          
023400     CLOSE W46180 W46185                                                  
023500     SKIP2                                                                
023600*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
023700*                                    SKRIVNA POSTER                       
023800                                                                          
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
