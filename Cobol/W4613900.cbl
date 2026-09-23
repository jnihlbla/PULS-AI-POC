000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4613900.                                                 
001000*AUTHOR.        BOSSE B                                                   
001100*DATE-WRITTEN.  FEB 1986.                                                 
001200                                                                          
001300*    REMARKS.                                                             
001400*                                                                         
001500*    FUNKTION:                                                            
001600*                                                                         
001700*        IMPORTÖRSBEROENDE BEHANDLING (NORGE-PV)                          
001800*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001900*        SKAPAR START OCH SLUTKORT                                        
002000*        PT RIC,RIQ,RIT,RIW  SKALL EJ SKRIVAS                             
002100*                                                                         
002200*        FÖR PT RIF GÄLLER ATT ENDAST SISTA "RIF KORTET"                  
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
003600     SELECT W46139                       ASSIGN TO UT-S-W46139D1.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - UTFIL:                                           
003900*                        - -  FIL TILL VIPS                               
004000     SELECT W46159                       ASSIGN TO UT-S-W46139D2.         
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP2                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W46139                                                               
004800     RECORDING      V                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005010 01  INPOST                       PIC X(211).                             
005347     EJECT                                                                
005350 FD  W46159                                                               
005400     RECORDING      V                                                     
005500     BLOCK CONTAINS 0.                                                    
005600 01  UTPOST                       PIC X(80).                              
005700     SKIP2                                                                
005800 01  RID-POST      -COPY W461RIDN       -L.                               
005900     SKIP2                                                                
006000 01  RIE-POST      -COPY W461RIEN       -L.                               
006100     SKIP2                                                                
006200 01  RIH-POST      -COPY W461RIHN       -L.                               
006210     SKIP2                                                                
006220 01  RIO-POST      -COPY W461RIO2       -L.                               
006221     SKIP2                                                                
006222 01  RKB-POST      -COPY W461RKBN       -L.                               
006223     SKIP2                                                                
006224 01  RKC-POST      -COPY W461RKCN       -L.                               
006225     SKIP2                                                                
006226 01  RKD-POST      -COPY W461RKDN       -L.                               
006230     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006310                                                                          
006400*    -- CHECKED BY WY2000                                                 
006900*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
007000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4613900'.            
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007400                                                                          
007500 77  JA                          PIC X(1)    VALUE 'J'.                   
007600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007700     SKIP2                                                                
007800*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007900                                                                          
008000 77  W46139-EOF                  PIC X(1)    VALUE 'N'.                   
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
013000*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
013100     EJECT                                                                
013200                                                                          
016470*01  -COPY W461RIFN        -PRE JFR-.                                     
016500     EJECT                                                                
016600*                             STARTKORT                                   
016700*01  -COPY W461RI0N                                                       
016900     EJECT                                                                
017000*                             SLUTKORT                                    
017100*01  -COPY W461RI9                                                        
017300     EJECT                                                                
017400*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
017500                                                                          
017600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
017700     SKIP2                                                                
017800*01  -COPY WDATKORT                                                       
018000     EJECT                                                                
018100*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
018200                                                                          
018300*01  -COPY W0005       -PRE  POSTSUM-.                                    
018500     EJECT                                                                
018600 PROCEDURE DIVISION.                                                      
018700     SKIP2                                                                
018800     PERFORM A-INIT                                                       
018900     PERFORM B-BEHANDLA                                                   
019000     PERFORM Z-FINIT                                                      
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400                                                                          
019500     SKIP3                                                                
019600 A-INIT SECTION.                                                          
019700     SKIP2                                                                
019800     OPEN INPUT W46139 OUTPUT W46159                                      
019900     SKIP2                                                                
020000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
020100     SKIP2                                                                
020200*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
020300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020400     MOVE D-AAR TO DAGENS-DATUM-AR                                        
020500     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
020600     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
020700     MOVE SPACE TO JFR-RIF-W461RIFN-CTX                                   
020800     SKIP2                                                                
020900*- - - - - - - - - - - - - - - - TID                                      
021000*                                                                         
021100     ACCEPT   DAGENS-TID FROM TIME                                        
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500 B-BEHANDLA SECTION.                                                      
021600     SKIP2                                                                
021700     PERFORM BA-START-KORT                                                
021800     PERFORM S01-LAS-W46139                                               
021900     PERFORM UNTIL                                                        
022000      NOT ( W46139-EOF = NEJ )                                            
022120       IF W-RIF-IDPTYP  = 'RIF'                                           
022130         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
022140         PERFORM S01-LAS-W46139                                           
022150       ELSE                                                               
022160         IF JFR-RIF-IDPTYP   = 'RIF'                                      
022170           ADD     +1           TO W-ANT-POSTER                           
022180           WRITE UTPOST FROM JFR-RIF-W461RIFN-CTX                         
022190           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
022200         END-IF                                                           
022300         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RIT' OR 'RIS' OR            
022310                           'RKA'                                          
022400           PERFORM S01-LAS-W46139                                         
022500         ELSE                                                             
022600           EVALUATE    W-RIF-IDPTYP                                       
022700                                                                          
022800             WHEN  'RID'                                                  
022900                  WRITE RID-POST  FROM INPOST                             
023000                                                                          
023100             WHEN  'RIE'                                                  
023200                  WRITE RIE-POST  FROM INPOST                             
023300                                                                          
023400             WHEN  'RIH'                                                  
023500                  WRITE RIH-POST  FROM INPOST                             
023600                                                                          
023700             WHEN  'RIO'                                                  
023800                  WRITE RIO-POST  FROM INPOST                             
023810                                                                          
023820             WHEN  'RKB'                                                  
023830                  WRITE RKB-POST  FROM INPOST                             
023840                                                                          
023850             WHEN  'RKC'                                                  
023860                  WRITE RKC-POST  FROM INPOST                             
023870                                                                          
023880             WHEN  'RKD'                                                  
023890                  WRITE RKD-POST  FROM INPOST                             
023900                                                                          
024000             WHEN  OTHER                                                  
024100                  WRITE UTPOST    FROM INPOST                             
024200                                                                          
024300           END-EVALUATE                                                   
024400                                                                          
024500           ADD     +1           TO W-ANT-POSTER                           
024600           PERFORM S01-LAS-W46139                                         
024700         END-IF                                                           
024800       END-IF                                                             
024900     END-PERFORM                                                          
026994     PERFORM BB-SLUT-KORT                                                 
026995     .                                                                    
026996     EJECT                                                                
026997                                                                          
027900 BA-START-KORT SECTION.                                                   
028000     SKIP2                                                                
028100     MOVE     'RI0'          TO START-IDPTYP                              
028200     MOVE     878            TO START-IDDISTR                             
028300     MOVE     WC-CDC-SE      TO START-IDDC                                
028400     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
028500*                                                                         
028600     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
028700*                                                                         
028800     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
028900     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
029100     .                                                                    
029200     EJECT                                                                
029300 BB-SLUT-KORT SECTION.                                                    
029400     SKIP2                                                                
029500     MOVE     'RI9'          TO SLUT-IDPTYP                               
029600     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
029700*                                                                         
029800     WRITE    UTPOST         FROM  SLUT-W461RI9                           
030000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
030100     .                                                                    
030200     EJECT                                                                
030300 S01-LAS-W46139 SECTION.                                                  
030400     SKIP2                                                                
030500     READ   W46139 INTO W-ARBAREA                                         
030600     AT END MOVE JA TO W46139-EOF                                         
030700     END-READ                                                             
030800                                                                          
030900     IF W46139-EOF = NEJ                                                  
031000       MOVE 'W46139'            TO POSTSUM-FDNAMN                         
031100       MOVE 'W46139D1'          TO POSTSUM-DDNAMN2                        
031200       MOVE SPACE               TO POSTSUM-TRANSTYP                       
031300       CALL POSTSUM             USING POSTSUM-PARM                        
031400     END-IF                                                               
031500     .                                                                    
031600 Z-FINIT SECTION.                                                         
031700     SKIP2                                                                
031800                                                                          
031900     CLOSE W46139 W46159                                                  
032000     SKIP2                                                                
032100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
032200*                                    SKRIVNA POSTER                       
032300                                                                          
032400     MOVE 'S' TO POSTSUM-OPKOD                                            
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     .                                                                    
