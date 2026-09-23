000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2715200.                                                
000300 AUTHOR.         JOHAN NIHLBLAD.                                          
000400 DATE-WRITTEN.   19/11/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROG TO READ WDK727 TO FIND WHAT PARTS TO CHANGE                 
000900*        FORECAST (KVPB-REF) WHEN FUTURE FORECAST IS SET.                 
001000*                                                                         
001100*        THE PROGRAM READS     WDK7                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- OUTPUT FILE                                                
002600     SELECT W27152                     ASSIGN TO W27152D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W27152                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W27153 -PRE  UT-  -L.                                   
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W2715200'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  WS-TIAAVV                   PIC 9(4)    VALUE ZERO.                  
004500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004600 01  FILLER REDEFINES TODAYS-DATE.                                        
004700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004900     03  TODAYS-DATE-DAY         PIC 9(2).                                
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
005800     SKIP2                                                                
005900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006400     SKIP2                                                                
006500 01  ERROR-TEXT.                                                          
006600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006700     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300*01  -COPY WZ20DAYS                                                       
007400     EJECT                                                                
007500 01  UT-AREA-START               PIC X(24)   VALUE                        
007600                                 'UT-AREA-START  '.                       
007700     SKIP2                                                                
007800 01  FILLER       PIC X(80).                                              
007900*01  AREA -COPY W27153     -PRE UT-                                       
008000     EJECT                                                                
008100*    --- AREAS FOR IMS-SECTIONS                                           
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  KEYS-FOR-DLI.                                                        
008700     03  W-IDARTNR-X.                                                     
008800         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008900     03  W-IDDC-X.                                                        
009000         05  W-IDDC              PIC X(2) VALUE SPACE.                    
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009500     88  SEGMENT-END                         VALUE 'GB'.                  
009600     SKIP2                                                                
009700 01  GOOD-STATUSCODES.                                                    
009800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200 01  SSA3                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNCTION CODES                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK727'.                      
010900 01  DLI-IO-WDK727.                                                       
011000*    03  -COPY WDK727                                                     
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300                                                                          
011400*01  -COPY W0008  -PRE WDK7-                                              
011500     05  KFB-ARTNR               PIC S9(9)           COMP-3.              
011600     05  KFB-IDDC                PIC X(2).                                
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING WDK7-PCB.                                      
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
012100                                                                          
012200                                                                          
012300     PERFORM A-INIT                                                       
012400     PERFORM IMS-GN-WDK727                                                
012500                                                                          
012600     PERFORM UNTIL SEGMENT-END                                            
012700       MOVE KFB-ARTNR           TO UT-IDARTNR                             
012800       MOVE KFB-IDDC            TO UT-IDDC                                
012900       IF PROG-TIPBJUST(1) = WS-TIAAVV                                    
013000       OR (PROG-TIPBJUST(1) = ZERO                                        
013100       AND PROG-TIPBJUST(2) = ZERO)                                       
013200         MOVE PROG-KVPB-JUST(1) TO UT-KVPB-JUST-1                         
013300         MOVE PROG-KVPB-JUST(2) TO UT-KVPB-JUST-2                         
013400         MOVE PROG-TIPBJUST(1)  TO UT-TIPBJUST-1                          
013500         MOVE PROG-TIPBJUST(2)  TO UT-TIPBJUST-2                          
013600         PERFORM S11-WRITE-W27152                                         
013700       END-IF                                                             
013800       PERFORM IMS-GN-WDK727                                              
013900     END-PERFORM                                                          
014000                                                                          
014100                                                                          
014200     PERFORM Z-FINIT                                                      
014300                                                                          
014400     MOVE ZERO TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900                                                                          
015000     OPEN OUTPUT W27152                                                   
015100                                                                          
015200     ACCEPT TODAYS-DATE  FROM DATE                                        
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400**MOVE +3 TO DAYS SO WE GET NEXT WEEKS WEEKNO, BECAUSE                    
015500**WE RUN WEEK BATCH ON SATURDAY AND FORECAST SHOULD BE                    
015600**VALID FROM MONDAY AFTER BATCH RUN.                                      
015700     MOVE +3                TO DAYS-KVDAYS                                
015800     MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                             
015900     MOVE TODAYS-DATE       TO DAYS-TIDATE1                               
016000                                                                          
016100     MOVE 'YYWW'            TO DAYS-KDDATFMT2                             
016200     MOVE SPACE             TO DAYS-TIDATE2                               
016300                                                                          
016400     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
016500*                                                                         
016600     IF DAYS-KDRC = +0                                                    
016700       MOVE DAYS-TIDATE2(1:4)       TO WS-TIAAVV                          
016800       DISPLAY 'WS-TIAAVV :' WS-TIAAVV                                    
016810       DISPLAY 'TODAYS DATE :' TODAYS-DATE                                
016900     ELSE                                                                 
017000       MOVE 'ERROR FROM WZ20DAYS E-SECT' TO ERROR-TEXT-STR                
017100       DISPLAY ERROR-TEXT                                                 
017200       DISPLAY DAYS-KDRC                                                  
017300       CALL FELLOG                                                        
017400     END-IF                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 Z-FINIT SECTION.                                                         
017800     CLOSE W27152                                                         
017900     SKIP2                                                                
018000     MOVE 'S' TO POSTSUM-OPKOD                                            
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018300     EJECT                                                                
018400 S11-WRITE-W27152 SECTION.                                                
018500                                                                          
018600     WRITE UT-RECORD FROM UT-AREA                                         
018700                                                                          
018800     MOVE 'W27152' TO POSTSUM-FDNAMN                                      
018900     MOVE 'W27152D1' TO POSTSUM-DDNAMN2                                   
019000     CALL POSTSUM USING POSTSUM-PARM                                      
019100     .                                                                    
019200     EJECT                                                                
019300 S99-ABEND SECTION.                                                       
019400                                                                          
019500     SKIP2                                                                
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     CALL ABEND USING RKOD-ABEND                                          
019900     .                                                                    
020000     EJECT                                                                
020100* --- IMS SECTIONS  ---                                                   
020200                                                                          
020300     EJECT                                                                
020400 IMS-GN-WDK727 SECTION.                                                   
020500                                                                          
020600     MOVE 'WDK727   '      TO SSA1                                        
020700     MOVE '  GB' TO GOOD-STATUSCODES                                      
020800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK727 SSA1                    
020900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
021000     PERFORM IMS-STATUSCHECK                                              
021100     .                                                                    
021200     EJECT                                                                
021300 IMS-STATUSCHECK SECTION.                                                 
021400                                                                          
021500     SET STATUS-IX TO 1                                                   
021600     SEARCH GOOD-STATUS                                                   
021700       AT END                                                             
021800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021900           DELIMITED BY SIZE INTO ERROR-TEXT                              
022000         DISPLAY ERROR-TEXT                                               
022100         CALL FELLOG                                                      
022200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022300         CONTINUE                                                         
022400     END-SEARCH                                                           
022500     .                                                                    
