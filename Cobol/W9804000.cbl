000100 ID  DIVISION.                                                            
000200*TEST2                                                                    
000300     SKIP2                                                                
000400 PROGRAM-ID.    W9804000.                                                 
000500 AUTHOR.        KJELL ANDRE.                                              
000600     DATE-WRITTEN.  APRIL 1986.                                           
000700                                                                          
000800     REMARKS.                                                             
000900                                                                          
001000*    FUNKTION:                                                            
001100*        PROGRAMMET LÄSER EN FIL SOM SKAPAS AV SOPS PLAN-KOMMANDO         
001200*        INNEHÅLLANDE ALLA PROCESSER SOM SKA AKTIVERAS EN VISS            
001300*        DAG. PROGRAMMET GENERERAR UTIFRÅN DENNA, EN FIL MED              
001400*        UPPGIFTER OM ALLA RUTINER SOM GENERERAR OUTPUT PÅ                
001500*        VOLVODATA. DENNA FIL ANVÄNDS SEDAN AV VOLVODATAS SYSTEM          
001600*        FÖR ATT GENERERA COM-ETTIKETTER.                                 
001700*        PROGRAMMET LÄSER OCKSÅ FÖREGÅENDE GENERATION AV DENNA            
001800*        FIL FÖR ATT INTE GENERERA NYA POSTER MED SAMMA NYCKEL.           
001900*                                                                         
002000*    SUBPROGRAM:                                                          
002100*        WORKDAY   - FÖR BERÄKNING AV NÄSTA ARBETSDAG                     
002200*                                                                         
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*--- INFILER:                                                             
003100                                                                          
003200     SELECT PLANFIL                      ASSIGN TO W98040D1.              
003300     SELECT W98040-IN                    ASSIGN TO W98040D2.              
003400     SKIP2                                                                
003500*--- UTFILER:                                                             
003600                                                                          
003700     SELECT W98040-UT                    ASSIGN TO W98040D3.              
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  PLANFIL                                                              
004400     LABEL RECORD   STANDARD                                              
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  FILLER -COPY WSOPPLAN     -L.                                        
004900     EJECT                                                                
005000 FD  W98040-IN                                                            
005100     LABEL RECORD   STANDARD                                              
005200     RECORDING      F                                                     
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*    -COPY W98040       -L.                                               
005600     EJECT                                                                
005700 FD  W98040-UT                                                            
005800     LABEL RECORD   STANDARD                                              
005900     RECORDING      F                                                     
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200*01  POST -COPY W98040      -PRE UT-40- -L                                
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700*                                                                         
006800 01  PROGRAM-NAMN                PIC X(8)    VALUE 'W9804000'.            
006900     SKIP2                                                                
007000 01  GENERELLA-KONSTANTER.                                                
007100*                                                                         
007200     03  JA                      PIC X(1)    VALUE 'J'.                   
007300     03  NEJ                     PIC X(1)    VALUE 'N'.                   
007400     SKIP3                                                                
007500 01  W-TIFTID                    PIC S9(5).                               
007600 01  W-TIFTID-GRP REDEFINES W-TIFTID.                                     
007700     03  W-FDAG                  PIC 9.                                   
007800     03  W-TIFMINUT              PIC S9(4).                               
007900     EJECT                                                                
008000*      --- VALID IDDC CODES                                               
008100*                                                                         
008200*01    -COPY WWDCKONS                                                     
008300       EJECT                                                              
008400 01  DYNAMISKA-SUBPROGRAM.                                                
008500*                                                                         
008600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008800     SKIP3                                                                
008900 01  END-OF-FILE-SWITCHAR.                                                
009000*                                                                         
009100     03  PLANFIL-EOF             PIC X(1)    VALUE 'N'.                   
009200     03  W98040-EOF              PIC X(1)    VALUE 'N'.                   
009300                                                                          
009400 01  RKOD-DUMP                   PIC S9(4) COMP VALUE +1000.              
009500                                                                          
009600 01  PLANFIL-ID.                                                          
009700     03  PLANFIL-TIAPDAT         PIC 9(6).                                
009800     03  PLANFIL-IDRUTIN         PIC X(8).                                
009900                                                                          
010000 01  W98040-ID.                                                           
010100     03  W98040-TIAPDAT    PIC 9(6).                                      
010200     03  W98040-IDRUTIN    PIC X(8).                                      
010300     EJECT                                                                
010400*01  -COPY WORKAREA                                                       
010500     EJECT                                                                
010600 01  FILLER                      PIC X(24)   VALUE                        
010700                                            'PLANFIL START      '.        
010800                                                                          
010900*01  -COPY WSOPPLAN                                                       
011000     EJECT                                                                
011100 01  FILLER                      PIC X(24)   VALUE                        
011200                                            'W98040-IN START    '.        
011300     SKIP3                                                                
011400*01  40-AREA  -PRE IN-   -COPY W98040                                     
011500     EJECT                                                                
011600 01  FILLER                      PIC X(24)  VALUE                         
011700                                            'W98040-UT START    '.        
011800     SKIP3                                                                
011900*01  40-AREA  -PRE UT-  -COPY W98040                                      
012000     EJECT                                                                
012100 PROCEDURE DIVISION.                                                      
012200     SKIP2                                                                
012300     PERFORM A-INIT                                                       
012400                                                                          
012500     PERFORM B-LAS-RUTINER-MED-VD-OUTPUT                                  
012600     PERFORM C-LAS-GAMMAL-W98040                                          
012700     PERFORM UNTIL PLANFIL-EOF = JA                                       
012800        IF PLANFIL-ID = W98040-ID                                         
012900            PERFORM C-LAS-GAMMAL-W98040                                   
013000            PERFORM B-LAS-RUTINER-MED-VD-OUTPUT                           
013100        ELSE IF PLANFIL-ID < W98040-ID                                    
013200            PERFORM D-SKAPA-SKRIV-W98040                                  
013300            PERFORM B-LAS-RUTINER-MED-VD-OUTPUT                           
013400        ELSE                                                              
013500            PERFORM C-LAS-GAMMAL-W98040                                   
013600        END-IF                                                            
013700        END-IF                                                            
013800     END-PERFORM                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK.                                                              
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     SKIP2                                                                
014600     OPEN INPUT  PLANFIL  W98040-IN                                       
014700     OPEN OUTPUT W98040-UT                                                
014800     .                                                                    
014900     EJECT                                                                
015000 B-LAS-RUTINER-MED-VD-OUTPUT  SECTION.                                    
015100     SKIP2                                                                
015200     PERFORM BA-LAS-PLANFIL                                               
015300     PERFORM UNTIL PLANFIL-EOF = JA OR SOP-COM-OUTPUT                     
015400       PERFORM BA-LAS-PLANFIL                                             
015500     END-PERFORM                                                          
015600                                                                          
015700     IF PLANFIL-EOF = NEJ                                                 
015800       MOVE SOP-PROC-NAME TO PLANFIL-IDRUTIN                              
015900       MOVE SOP-ACTPASS-DATE TO PLANFIL-TIAPDAT                           
016000     ELSE                                                                 
016100       MOVE HIGH-VALUE TO PLANFIL-ID                                      
016200     END-IF                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 BA-LAS-PLANFIL           SECTION.                                        
016600     SKIP2                                                                
016700     READ PLANFIL INTO SOP-RECORD                                         
016800       AT END  MOVE JA TO PLANFIL-EOF                                     
016900     END-READ                                                             
017000     .                                                                    
017100     EJECT                                                                
017200 C-LAS-GAMMAL-W98040      SECTION.                                        
017300                                                                          
017400     READ W98040-IN INTO IN-40-AREA                                       
017500       AT END MOVE JA TO W98040-EOF                                       
017600     END-READ                                                             
017700                                                                          
017800     IF W98040-EOF = NEJ                                                  
017900       MOVE IN-40-IDRUTIN TO W98040-IDRUTIN                               
018000       MOVE IN-40-TIAPDAT TO W98040-TIAPDAT                               
018100     ELSE                                                                 
018200       MOVE HIGH-VALUE TO W98040-ID                                       
018300     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 D-SKAPA-SKRIV-W98040     SECTION.                                        
018700     SKIP2                                                                
018800     MOVE SOP-PROC-NAME      TO UT-40-IDRUTIN                             
018900     MOVE SOP-ACTPASS-DATE   TO UT-40-TIAPDAT                             
019000     MOVE 2100               TO UT-40-TIMINUT-START                       
019100     MOVE SOP-PROC-PRIO      TO UT-40-KDPROCPRIO                          
019200                                                                          
019300     MOVE SOP-OUT-TIME-LIMIT TO W-TIFTID                                  
019400     IF W-FDAG = 0                                                        
019500       MOVE UT-40-TIAPDAT    TO UT-40-TIFDAT                              
019600     ELSE                                                                 
019700       MOVE 002              TO  WORK-KDCALL                              
019800       MOVE WC-CDC-SE        TO  WORK-IDDC                                
019900       MOVE UT-40-TIAPDAT    TO  WORK-TIAAMMDD-FOM                        
020000       ADD 1  W-FDAG     GIVING  WORK-KVWORKD                             
020100       CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                      
020200                    WORK-KDSVAR                                           
020300       IF WORK-KDSVAR-FEL                                                 
020400         DISPLAY 'FEL VID ANROP TILL WORKDAY .'                           
020500         CALL ABEND USING RKOD-DUMP                                       
020600       END-IF                                                             
020700       MOVE  WORK-TIAAMMDD-TOM TO UT-40-TIFDAT  UT-40-TIFDAT-2            
020800     END-IF                                                               
020900     MOVE W-TIFMINUT       TO UT-40-TIFMINUT  UT-40-TIFMINUT-2            
021000                                                                          
021100     IF SOP-COM-OUTPUT                                                    
021200       MOVE 'COM '           TO UT-40-KDOPCARB                            
021300       MOVE SOP-ACTPASS-DATE TO UT-40-TIAPDAT-COM                         
021400       MOVE 2100             TO UT-40-TIMINUT-START-COM                   
021500       MOVE SOP-PROC-PRIO    TO UT-40-KDPROCPRIO-COM                      
021600       WRITE UT-40-POST FROM UT-40-AREA                                   
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 Z-FINIT SECTION.                                                         
022100     SKIP2                                                                
022200     CLOSE PLANFIL W98040-IN W98040-UT                                    
022300     .                                                                    
