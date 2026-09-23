000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5613100.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   18/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        READ WDL6 AND CREATE A FILE WITH INBOUNDSFOR PREVIOUS            
000900*        DAY FOR US/CHINA                                                 
001000*                                                                         
001100*        THE PROGRAM READS     WDL6                                       
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
002500*          --- PREVIOUS DAY INBOUND FOR US/CN                             
002600     SELECT W56131                     ASSIGN TO W56131D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W56131                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W56131 -PRE  UT-  -L.                                   
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W5613100'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
004400     EJECT                                                                
004500 01  TODAYS-DATE                 PIC 9(8)    VALUE ZERO.                  
004600 01  FILLER REDEFINES TODAYS-DATE.                                        
004700     03  TODAYS-DATE-CC          PIC 9(2).                                
004800     03  TODAYS-DATE-YYMMDD      PIC 9(6).                                
004900 01  YDAY-DATE                   PIC 9(8)    VALUE ZERO.                  
005000 01  FILLER REDEFINES YDAY-DATE.                                          
005100     03  YDAY-DATE-CC            PIC 9(2).                                
005200     03  YDAY-DATE-YYMMDD        PIC 9(6).                                
005300     EJECT                                                                
005400*      --- VALID IDDC CODES                                               
005500*                                                                         
005600*01    -COPY WWDC99                                                       
005700*                                                                         
005800*01    -COPY WWDCLAND                                                     
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006700     SKIP2                                                                
006800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007600     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200*01  -COPY WDAGAREA                                                       
008300     EJECT                                                                
008400 01  UT-AREA-START               PIC X(24)   VALUE                        
008500                                 'UT-AREA-START  '.                       
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W56131     -PRE UT-                                       
008900     EJECT                                                                
009000*    --- AREAS FOR IMS-SECTIONS                                           
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FOUND                       VALUE '  '.                  
009800     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009900     SKIP2                                                                
010000 01  GOOD-STATUSCODES.                                                    
010100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNCTION CODES                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011100     SKIP3                                                                
011200 01  DLI-IO-AREA.                                                         
011300     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
011400     SKIP3                                                                
011500     03  WDL601   REDEFINES IO-AREA.                                      
011600*        05  -COPY WDL601                                                 
011700     SKIP3                                                                
011800     03  WDL611   REDEFINES IO-AREA.                                      
011900*        05  -COPY WDL611                                                 
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0008  -PRE WDL6-                                              
012400     05  FILLER                  PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING WDL6-PCB.                                      
012700 MAIN SECTION.                                                            
012800     ENTRY 'DLITCBL' USING WDL6-PCB.                                      
012900                                                                          
013000     PERFORM A-INIT                                                       
013100                                                                          
013200     PERFORM IMS-GET-WDL6                                                 
013300     PERFORM UNTIL SEGMENT-MISSING                                        
013400       EVALUATE WDL6-SEG-NAME-FB                                          
013500         WHEN 'WDL601'                                                    
013600           MOVE ART-IDARTNR      TO UT-IDARTNR                            
013700         WHEN 'WDL611'                                                    
013800           PERFORM B-PROCESS                                              
013900       END-EVALUATE                                                       
014000       PERFORM IMS-GET-WDL6                                               
014100     END-PERFORM                                                          
014200     PERFORM Z-FINIT                                                      
014300                                                                          
014400     MOVE ZERO                   TO RETURN-CODE                           
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900                                                                          
015000     OPEN OUTPUT W56131                                                   
015100                                                                          
015200     ACCEPT TODAYS-DATE        FROM DATE YYYYMMDD                         
015300     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
015400                                                                          
015500     MOVE 003                    TO DAG-KDCALL                            
015600     MOVE TODAYS-DATE-CC         TO DAG-TISEKEL-TOM                       
015700     MOVE TODAYS-DATE-YYMMDD     TO DAG-TIAAMMDD-TOM                      
015800     MOVE 2                      TO DAG-KVKALDAG                          
015900     CALL WDAGKONV            USING DAG-KDCALL                            
016000                                    DAG-DATUM-AREA                        
016100                                    DAG-KDSVAR                            
016200                                                                          
016300     IF DAG-KDSVAR = SPACE                                                
016400       MOVE DAG-TISEKEL-FOM      TO YDAY-DATE-CC                          
016500       MOVE DAG-TIAAMMDD-FOM     TO YDAY-DATE-YYMMDD                      
016600     ELSE                                                                 
016700       MOVE ' INVALID RET CODE FROM WDAGKONV'                             
016800                                 TO ERROR-TEXT-STR                        
016900       DISPLAY ERROR-TEXT                                                 
017000       CALL FELLOG                                                        
017100     END-IF                                                               
017200     .                                                                    
017300     EJECT                                                                
017400                                                                          
017500 B-PROCESS SECTION.                                                       
017600     MOVE INL-IDDC               TO WS-IDDC                               
017700     MOVE INL-TIINLINL           TO WS-TIINLINL                           
017800     IF NDC-CN OR NDC-US                                                  
017900       IF WS-TIINLINL > 0 AND                                             
018000          WS-TIINLINL = YDAY-DATE-YYMMDD                                  
018100         MOVE INL-IDDC           TO UT-IDDC                               
018200         MOVE INL-IDKUNDRF       TO UT-IDKUNDRF                           
018300         MOVE INL-IDLEVNR        TO UT-IDLEVNR                            
018400         MOVE INL-KVAVIS         TO UT-KVAVIS                             
018500         MOVE INL-KVANTMOT       TO UT-KVANTMOT                           
018510         MOVE INL-TIINLINL       TO UT-TIINLINL                           
018520         MOVE INL-PRARTNTO       TO UT-PRARTNTO                           
018530         MOVE INL-PRKURS         TO UT-PRKURS                             
018540         MOVE INL-TIAVIDAT       TO UT-TIAVIDAT                           
018600                                                                          
018700         IF INL-IDLOPNRM = ZERO                                           
018800           MOVE INL-IDFAKT       TO UT-IDLOPNRM                           
018900         ELSE                                                             
019000           MOVE INL-IDLOPNRM     TO UT-IDLOPNRM                           
019100         END-IF                                                           
019200                                                                          
019300         PERFORM S01-SEARCH-IDLAND                                        
019400         PERFORM S11-WRITE-W56131                                         
019500       END-IF                                                             
019600     END-IF                                                               
019700     .                                                                    
019800     EJECT                                                                
019900                                                                          
020000 Z-FINIT SECTION.                                                         
020100     CLOSE W56131                                                         
020200     SKIP2                                                                
020300     MOVE 'S'                    TO POSTSUM-OPKOD                         
020400     CALL POSTSUM             USING POSTSUM-PARM                          
020500     .                                                                    
020600     EJECT                                                                
020700 S01-SEARCH-IDLAND SECTION.                                               
020800                                                                          
020900     SEARCH ALL DC-LAND                                                   
021000       AT END                                                             
021100         MOVE ' NO MATCH FOUND IN WWDCLAND'                               
021200                                 TO ERROR-TEXT-STR                        
021300         CALL FELLOG                                                      
021400       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC                             
021500         MOVE DCLAND-IDLANDX2(DCLAND-IX)                                  
021600                                 TO UT-IDLANDX2                           
021700     END-SEARCH                                                           
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 S11-WRITE-W56131 SECTION.                                                
022200                                                                          
022300     WRITE UT-RECORD           FROM UT-AREA                               
022400                                                                          
022500     MOVE 'W56131'               TO POSTSUM-FDNAMN                        
022600     MOVE 'W56131D1'             TO POSTSUM-DDNAMN2                       
022700     CALL POSTSUM             USING POSTSUM-PARM                          
022800     .                                                                    
022900     EJECT                                                                
023000 S99-ABEND SECTION.                                                       
023100                                                                          
023200     SKIP2                                                                
023300     MOVE 'S'                    TO POSTSUM-OPKOD                         
023400     CALL POSTSUM             USING POSTSUM-PARM                          
023500     CALL ABEND               USING RKOD-ABEND                            
023600     .                                                                    
023700     EJECT                                                                
023800* --- IMS SECTIONS  ---                                                   
023900                                                                          
024000 IMS-GET-WDL6   SECTION.                                                  
024100                                                                          
024200     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-AREA                           
024300     MOVE WDL6-STATUS-CODE       TO STATUS-WS                             
024400     MOVE '  GAGKGB'             TO GOOD-STATUSCODES                      
024500     PERFORM IMS-STATUSCHECK                                              
024600     .                                                                    
024700     EJECT                                                                
024800 IMS-STATUSCHECK SECTION.                                                 
024900                                                                          
025000     SET STATUS-IX TO 1                                                   
025100     SEARCH GOOD-STATUS                                                   
025200       AT END                                                             
025300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025400           DELIMITED BY SIZE INTO ERROR-TEXT                              
025500         DISPLAY ERROR-TEXT                                               
025600         CALL FELLOG                                                      
025700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
025800         CONTINUE                                                         
025900     END-SEARCH                                                           
026000     .                                                                    
