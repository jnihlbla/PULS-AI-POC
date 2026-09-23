000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5518300.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/07/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM DOWNLOADS WDF102 DATA FOR SE COUNTRY CODE           
000900*                                                                         
001000*        THE PROGRAM READS     WDF1                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- WDF1 DATA                                                  
002500     SELECT W55183                     ASSIGN TO W55183D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W55183                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W55183 -PRE  UT-  -L.                                   
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W5518300'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200 77  WS-KDVALLEV                 PIC 9(3)    VALUE ZERO.                  
004300 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
004400     EJECT                                                                
004500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004600 01  FILLER REDEFINES TODAYS-DATE.                                        
004700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004900     03  TODAYS-DATE-DAY         PIC 9(2).                                
005000     EJECT                                                                
005100 01  WS-VALID-DATE               PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES WS-VALID-DATE.                                      
005300     03  WS-VALID-YEAR           PIC 9(2).                                
005400     03  WS-VALID-MONTH          PIC 9(2).                                
005500     03  WS-VALID-DAY            PIC 9(2).                                
005600     EJECT                                                                
005700 01  GENERAL-SUBPROGRAMS.                                                 
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  ERROR-TEXT.                                                          
007100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL POSTSUM                                          
007500*                                                                         
007600*01  -COPY W0005   -PRE  POSTSUM-                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(8)    VALUE 'W553LVAL'.            
007900*01 -COPY W553LVAL                                                        
008000     EJECT                                                                
008100 01  UT-AREA-START               PIC X(24)   VALUE                        
008200                                 'UT-AREA-START  '.                       
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W55183     -PRE UT-                                       
008600     EJECT                                                                
008700*    --- AREAS FOR IMS-SECTIONS                                           
008800*                                                                         
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009100     SKIP3                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009500     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009600     SKIP2                                                                
009700 01  GOOD-STATUSCODES.                                                    
009800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNCTION CODES                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF1'.                        
010800 01  DLI-IO-WDF1.                                                         
010900     03  WDF1-AREA            PIC X(280).                                 
011000     03  FILLER   REDEFINES WDF1-AREA.                                    
011100*        05 -COPY  WDF101                                                 
011200     03  FILLER   REDEFINES WDF1-AREA.                                    
011300*        05 -COPY  WDF102                                                 
011400     03  FILLER   REDEFINES WDF1-AREA.                                    
011500*        05 -COPY  WDF106                                                 
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900                                                                          
012000*01  -COPY W0008  -PRE WDF1-                                              
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING WDF1-PCB.                                      
012400 MAIN SECTION.                                                            
012500     ENTRY 'DLITCBL' USING WDF1-PCB.                                      
012600                                                                          
012700                                                                          
012800     PERFORM A-INIT                                                       
012900                                                                          
013000     PERFORM IMS-GET-WDF1                                                 
013100     PERFORM UNTIL SEGMENT-MISSING                                        
013200       EVALUATE WDF1-SEG-NAME-FB                                          
013300         WHEN 'WDF101'                                                    
013400           MOVE LEV-IDLEVNR    TO UT-IDLEVNR                              
013500         WHEN 'WDF102'                                                    
013600           PERFORM B-CHECK-IDLAND                                         
013700         WHEN 'WDF106'                                                    
013800           MOVE ADR-BELEV      TO UT-BELEV                                
013810           MOVE ADR-IDLANDX2   TO UT-IDLAND                               
013900           PERFORM S11-WRITE-W55183                                       
014000       END-EVALUATE                                                       
014100                                                                          
014200       PERFORM IMS-GET-WDF1                                               
014300     END-PERFORM                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN OUTPUT W55183                                                   
015300                                                                          
015400     ACCEPT TODAYS-DATE  FROM DATE                                        
015500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     COMPUTE WS-VALID-YEAR = TODAYS-DATE-YEAR + 1                         
015700     MOVE '01'             TO WS-VALID-MONTH                              
015800     MOVE '01'             TO WS-VALID-DAY                                
015900     .                                                                    
016000     EJECT                                                                
016100 B-CHECK-IDLAND SECTION.                                                  
016200                                                                          
016300     MOVE ZERO                  TO WS-KDVALLEV                            
016400     MOVE SPACES                TO WS-IDLANDX2                            
016500                                                                          
016600     IF TULL-IDLANDX2 = 'SE'                                              
016700        MOVE TULL-IDLANDX2      TO UT-IDLAND-WDF1                         
016710        MOVE TULL-RETULF-1      TO UT-RETULF-1                            
016800        MOVE TULL-KDVALLEV      TO UT-KDVALLEV                            
016900                                   WS-KDVALLEV                            
017000                                                                          
017100        MOVE +1                 TO VAL-IX                                 
017200        PERFORM VAL-IX-MAX TIMES                                          
017300          IF WS-KDVALLEV = VAL-KDVALUTA(VAL-IX)                           
017400            MOVE VAL-IDLANDX2(VAL-IX)                                     
017500                                TO WS-IDLANDX2                            
017600          END-IF                                                          
017700          ADD +1                TO VAL-IX                                 
017800        END-PERFORM                                                       
017900        MOVE WS-IDLANDX2        TO UT-IDLANDX2                            
018100        MOVE '20'               TO UT-NEXT-YR-DATE(1:2)                   
018200        MOVE WS-VALID-DATE      TO UT-NEXT-YR-DATE(3:6)                   
018300                                                                          
018400     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 Z-FINIT SECTION.                                                         
018800     CLOSE W55183                                                         
018900     SKIP2                                                                
019000     MOVE 'S' TO POSTSUM-OPKOD                                            
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400 S11-WRITE-W55183 SECTION.                                                
019500                                                                          
019600     WRITE UT-RECORD FROM UT-AREA                                         
019700                                                                          
019800     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
019900     MOVE 'W55183' TO POSTSUM-FDNAMN                                      
020000     MOVE 'W55183D1' TO POSTSUM-DDNAMN2                                   
020100     CALL POSTSUM USING POSTSUM-PARM                                      
020200     .                                                                    
020300     EJECT                                                                
020400 S99-ABEND SECTION.                                                       
020500                                                                          
020600     SKIP2                                                                
020700     MOVE 'S' TO POSTSUM-OPKOD                                            
020800     CALL POSTSUM USING POSTSUM-PARM                                      
020900     CALL ABEND USING RKOD-ABEND                                          
021000     .                                                                    
021100     EJECT                                                                
021200* --- IMS SECTIONS  ---                                                   
021300                                                                          
021400                                                                          
021500 IMS-GET-WDF1   SECTION.                                                  
021600                                                                          
021700     CALL CBLTDLI USING GN WDF1-PCB DLI-IO-WDF1                           
021800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
021900     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
022000     PERFORM IMS-STATUSCHECK                                              
022100     .                                                                    
022200     EJECT                                                                
022300 IMS-STATUSCHECK SECTION.                                                 
022400                                                                          
022500     SET STATUS-IX TO 1                                                   
022600     SEARCH GOOD-STATUS                                                   
022700       AT END                                                             
022800         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
022900           DELIMITED BY SIZE INTO ERROR-TEXT                              
023000         DISPLAY ERROR-TEXT                                               
023100         CALL FELLOG                                                      
023200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
023300         CONTINUE                                                         
023400     END-SEARCH                                                           
023500     .                                                                    
