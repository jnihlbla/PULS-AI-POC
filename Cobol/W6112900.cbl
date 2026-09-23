000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6112900.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   92/08/18.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED STATUSPOSTER FRÅN W6G3 (W6FILA) OCH                
001100*        RÄKNAR UT GENOMLOPPSTIDEN FÖR VARJE STATUS ÄNDRING.              
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- STATUSPOSTER FRÅN W6G3                                     
002700     SELECT W61128                     ASSIGN TO W61129D1.                
002800     SKIP2                                                                
002900*          --- SUMMAFIL MED PRODUCERAT IDAG OCH GENOMLOPPSTID             
003000     SELECT W61129                     ASSIGN TO W61129D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W61128                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  -COPY W6112801      -L.                                              
004100     SKIP3                                                                
004200 FD  W61129                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  POST -COPY W6112901 -PRE  UT-  -L.                                   
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004810                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(8)    VALUE 'W6112900'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  W-TOT-TIGLT-MM          PIC S9(9)       VALUE ZERO COMP-3.           
005600 77  W-TOT-TIGLT-PRIO-MM     PIC S9(9)       VALUE ZERO COMP-3.           
005700 77  W-OLD-IDLOPNRM          PIC S9(9)       VALUE ZERO COMP-3.           
005800 77  W-ADINLOMR              PIC X(4)        VALUE SPACE.                 
005900 77  W-KDINLUPF              PIC X(4)        VALUE SPACE.                 
006000 77  W-TIGLT-MM              PIC S9(7)       VALUE ZERO COMP-3.           
006100                                                                          
006200 01  W-NEW-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
006300 01  FILLER REDEFINES W-NEW-TIKLOCK.                                      
006400   05 W-NEW-HH               PIC 9(2).                                    
006500   05 W-NEW-MM               PIC 9(2).                                    
006600   05 FILLER                 PIC 9(4).                                    
006700                                                                          
006800 01  W-OLD-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
006900 01  FILLER REDEFINES W-OLD-TIKLOCK.                                      
007000   05 W-OLD-HH               PIC 9(2).                                    
007100   05 W-OLD-MM               PIC 9(2).                                    
007200   05 FILLER                 PIC 9(4).                                    
007300                                                                          
007400 01  W-GLT                   PIC 9(5)        VALUE ZERO.                  
007500 01  FILLER REDEFINES W-GLT.                                              
007600   05 W-GLT-HH               PIC 9(3).                                    
007700   05 W-GLT-MM               PIC 9(2).                                    
007800                                                                          
007900 77  W-TIKLOCK-MM            PIC S9(5)      COMP-3  VALUE ZERO.           
008000 77  W-TOT-MM                PIC S9(5)      COMP-3  VALUE ZERO.           
008100                                                                          
008200 77  W61128-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W61128                       VALUE 'J'.                   
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
009000     SKIP2                                                                
009100*    --- PARAMETRAR TILL ABEND                                            
009200                                                                          
009300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009500     SKIP2                                                                
009600 01  FELTEXT.                                                             
009700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400*01  -COPY WORKAREA                                                       
010500     EJECT                                                                
010600*    --- AREA FÖR BERÄKNING AV GLT PER PARTI                              
010700*01  AREA -COPY W6112801     -PRE W-SPAR1-                                
010800     EJECT                                                                
010900*    --- AREA FÖR BERÄKNING AV GLT PER HÄNDELSE PÅ W6G3                   
011000*01  AREA -COPY W6112801     -PRE W-SPAR2-                                
011100     EJECT                                                                
011200 01  IN-AREA-START               PIC X(24)   VALUE                        
011300                                 'IN-AREA-START  '.                       
011400                                                                          
011500*01  AREA -COPY W6112801     -PRE IN-                                     
011600     EJECT                                                                
011700 01  UT-AREA-START           PIC X(24)   VALUE                            
011800                                 'UT-AREA-START  '.                       
011900                                                                          
012000*01  AREA -COPY W6112901     -PRE UT-                                     
012100     EJECT                                                                
012200 PROCEDURE DIVISION.                                                      
012300     SKIP2                                                                
012400                                                                          
012500     PERFORM A-INIT                                                       
012600     PERFORM S01-LAES-W61128                                              
012700     PERFORM UNTIL END-OF-W61128                                          
012800         PERFORM B-BEHANDLA-STATUS-POST                                   
012900         PERFORM S01-LAES-W61128                                          
013000     END-PERFORM                                                          
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900                                                                          
014000     OPEN INPUT  W61128                                                   
014100                                                                          
014200     OPEN OUTPUT W61129                                                   
014300     SKIP2                                                                
014400                                                                          
014500     MOVE SPACE                TO W-SPAR1-AREA                            
014600                                  W-SPAR2-AREA                            
014700     MOVE ZERO                 TO W-SPAR1-IDLOPNRM                        
014710                                  W-SPAR1-IDRADNR                         
014720                                  W-SPAR1-IDDC                            
014800                                  W-SPAR2-IDLOPNRM                        
014900                                  W-SPAR2-IDRADNR                         
014930                                  W-SPAR2-IDDC                            
015000     .                                                                    
015100     EJECT                                                                
015200 B-BEHANDLA-STATUS-POST     SECTION.                                      
015300                                                                          
015400     IF IN-IDLOPNRM            = W-SPAR1-IDLOPNRM AND                     
015410        IN-IDRADNR             = W-SPAR1-IDRADNR  AND                     
015420        IN-IDDC                = W-SPAR1-IDDC                             
015500         CONTINUE                                                         
015600      ELSE                                                                
015700         MOVE IN-AREA          TO W-SPAR1-AREA                            
015800     END-IF                                                               
015900                                                                          
016000     IF IN-IDLOPNRM            = W-SPAR2-IDLOPNRM AND                     
016100        IN-IDRADNR             = W-SPAR2-IDRADNR  AND                     
016110        IN-IDDC                = W-SPAR2-IDDC                             
016200         PERFORM BA-SKAPA-HIST-POST                                       
016300     END-IF                                                               
016400                                                                          
016500     IF IN-KDINLSTA            =  'INL'                                   
016600         PERFORM BB-SKAPA-INL-HIST-POST                                   
016700     END-IF                                                               
016800                                                                          
016900     MOVE IN-AREA              TO W-SPAR2-AREA                            
017000     .                                                                    
017100     EJECT                                                                
017200 BA-SKAPA-HIST-POST             SECTION.                                  
017300                                                                          
017400     MOVE W-SPAR2-TIREGDAT     TO WORK-TIAAMMDD-FOM                       
017500     MOVE W-SPAR2-TIKLOCK      TO W-OLD-TIKLOCK                           
017600                                                                          
017700     PERFORM S20-BERAEKNA-ARBETSDAGAR                                     
017800                                                                          
017900     MOVE IN-TIKLOCK           TO W-NEW-TIKLOCK                           
018000     PERFORM S21-BERAEKNA-TIGLT                                           
018100                                                                          
018200     DIVIDE W-TOT-MM BY 60 GIVING W-GLT-HH REMAINDER W-GLT-MM             
018300     COMPUTE UT-TIGLT          =  W-GLT / 100                             
018400                                                                          
018500     MOVE W-SPAR2-IDDC         TO UT-IDDC                                 
018510     MOVE W-SPAR2-TIREGDAT     TO UT-TIREGDAT                             
018600     MOVE W-SPAR2-TIKLOCK      TO UT-TIKLOCK                              
018700     MOVE W-SPAR2-IDARTNR      TO UT-IDARTNR                              
018800     MOVE W-SPAR2-IDLOPNRM     TO UT-IDLOPNRM                             
018900     MOVE W-SPAR2-IDRADNR      TO UT-IDRADNR                              
019000     MOVE W-SPAR2-KDINLPRIO    TO UT-KDINLPRIO                            
019100     MOVE W-SPAR2-KDINLSTA     TO UT-KDINLSTA                             
019200     MOVE W-SPAR2-KVINLART     TO UT-KVINLART                             
019300     MOVE W-SPAR2-PRARTSTD     TO UT-PRARTSTD                             
019400                                                                          
019500     IF  W-SPAR2-KDINLUPF       = SPACE                                   
019600     AND W-SPAR2-KDINLSTA       = SPACE                                   
019700         MOVE 'R31 '           TO UT-KDINLUPF                             
019800     ELSE                                                                 
019900         MOVE W-SPAR2-KDINLUPF TO UT-KDINLUPF                             
020000     END-IF                                                               
020100     MOVE IN-KDINLUPF          TO UT-KDINLUPF-NXT                         
020200                                                                          
020300     PERFORM S11-SKRIV-W61129                                             
020400     .                                                                    
020500     EJECT                                                                
020600 BB-SKAPA-INL-HIST-POST      SECTION.                                     
020700                                                                          
020710     IF IN-IDLOPNRM            = W-SPAR2-IDLOPNRM AND                     
020720        IN-IDRADNR             = W-SPAR2-IDRADNR  AND                     
020730        IN-IDDC                = W-SPAR2-IDDC                             
020800       MOVE W-SPAR1-TIREGDAT     TO WORK-TIAAMMDD-FOM                     
020900       MOVE W-SPAR1-TIKLOCK      TO W-OLD-TIKLOCK                         
020910     ELSE                                                                 
020911       MOVE IN-TIREGDAT          TO WORK-TIAAMMDD-FOM                     
020912       MOVE IN-TIKLOCK           TO W-OLD-TIKLOCK                         
020913     END-IF                                                               
020920                                                                          
021000                                                                          
021100     PERFORM S20-BERAEKNA-ARBETSDAGAR                                     
021200                                                                          
021300     MOVE IN-TIKLOCK           TO W-NEW-TIKLOCK                           
021400     PERFORM S21-BERAEKNA-TIGLT                                           
021500                                                                          
021600     DIVIDE W-TOT-MM BY 60 GIVING W-GLT-HH REMAINDER W-GLT-MM             
021700     COMPUTE UT-TIGLT          =  W-GLT / 100                             
021800                                                                          
021900     MOVE IN-IDDC              TO UT-IDDC                                 
021910     MOVE IN-TIREGDAT          TO UT-TIREGDAT                             
022000     MOVE IN-TIKLOCK           TO UT-TIKLOCK                              
022100     MOVE IN-IDARTNR           TO UT-IDARTNR                              
022200     MOVE IN-IDLOPNRM          TO UT-IDLOPNRM                             
022300     MOVE IN-IDRADNR           TO UT-IDRADNR                              
022400     MOVE IN-KDINLPRIO         TO UT-KDINLPRIO                            
022500     MOVE IN-KDINLSTA          TO UT-KDINLSTA                             
022600     MOVE IN-KVINLART          TO UT-KVINLART                             
022700     MOVE IN-PRARTSTD          TO UT-PRARTSTD                             
022800     MOVE IN-KDINLUPF          TO UT-KDINLUPF                             
022900     MOVE SPACE                TO UT-KDINLUPF-NXT                         
023000                                                                          
023100     PERFORM S11-SKRIV-W61129                                             
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 Z-FINIT SECTION.                                                         
023600     CLOSE W61128                                                         
023700           W61129                                                         
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 S01-LAES-W61128  SECTION.                                                
024400     SKIP2                                                                
024500     READ W61128 INTO IN-AREA                                             
024600     AT END                                                               
024700        SET END-OF-W61128 TO TRUE                                         
024800                                                                          
024900     NOT AT END                                                           
025000        MOVE 'W61128'          TO POSTSUM-FDNAMN                          
025100        MOVE 'W61129D1'        TO POSTSUM-DDNAMN2                         
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025300     END-READ                                                             
025400     .                                                                    
025500     EJECT                                                                
025600 S11-SKRIV-W61129 SECTION.                                                
025700     SKIP2                                                                
025800     WRITE UT-POST FROM UT-AREA                                           
025900                                                                          
026000     MOVE 'W61129'             TO POSTSUM-FDNAMN                          
026100     MOVE 'W61129D2'           TO POSTSUM-DDNAMN2                         
026200     CALL POSTSUM USING POSTSUM-PARM                                      
026300     .                                                                    
026400     EJECT                                                                
026500 S20-BERAEKNA-ARBETSDAGAR    SECTION.                                     
026600                                                                          
026700     MOVE +001                 TO WORK-KDCALL                             
026701                                                                          
026702     IF IN-IDDC = '12'                                                    
026710       MOVE '91'                 TO WORK-IDDC                             
026711     ELSE                                                                 
026720       MOVE IN-IDDC              TO WORK-IDDC                             
026730     END-IF                                                               
026740                                                                          
026800     MOVE IN-TIREGDAT          TO WORK-TIAAMMDD-TOM                       
026900                                                                          
027000     CALL WORKDAY USING WORK-KDCALL                                       
027100               WORK-DATE-AREA WORK-KDSVAR                                 
027200                                                                          
027300     IF WORK-KDSVAR-OK                                                    
027400         COMPUTE WORK-KVWORKD   = WORK-KVWORKD - 1                        
027500     ELSE                                                                 
027600         MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                             
027700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 S21-BERAEKNA-TIGLT   SECTION.                                            
028200                                                                          
028300     COMPUTE W-TIKLOCK-MM      = ((W-NEW-HH - W-OLD-HH) * 60) +           
028400                                 (W-NEW-MM - W-OLD-MM)                    
028500     COMPUTE W-TOT-MM          = (WORK-KVWORKD * 492) +                   
028600                                  W-TIKLOCK-MM                            
028700     .                                                                    
028800     EJECT                                                                
