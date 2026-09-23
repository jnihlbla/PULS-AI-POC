000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4794500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/04/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM WRITES OUT A LIST OF VALID DISTRICTS FROM           
000900*        WDB601.                                                          
001000*                                                                         
001100*        THE PROGRAM READS     WDB6                                       
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
002500*          --- VALID DISTRICTS FROM WDB6                                  
002600     SELECT W47945                     ASSIGN TO W47945D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W47945                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     EJECT                                                                
003600*01  POST -COPY W47945 -PRE  W47945-  -L.                                 
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W4794500'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200 77  WS-REFILL                   PIC X(6)    VALUE 'REFILL'.              
004300 77  WS-RETUR                    PIC X(6)    VALUE 'RETURN'.              
004400 77  WS-QRETUR                   PIC X(14)   VALUE                        
004401                                             'QUALITY RETURN'.            
004410 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
004600     EJECT                                                                
004700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES TODAYS-DATE.                                        
004900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005100     03  TODAYS-DATE-DAY         PIC 9(2).                                
005200     EJECT                                                                
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400*                                                                         
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     SKIP2                                                                
006000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  ERROR-TEXT.                                                          
006700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  UT-AREA-START               PIC X(24)   VALUE                        
007500                                 'UT-AREA-START  '.                       
007600*01  AREA  -COPY W47945  -PRE UT-                                         
007700     SKIP2                                                                
007800     EJECT                                                                
007900*    --- AREAS FOR IMS-SECTIONS                                           
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  KEYS-FOR-DLI.                                                        
008500     03  W-IDDC-X.                                                        
008600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FOUND                       VALUE '  '.                  
009100     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009200     SKIP2                                                                
009300 01  GOOD-STATUSCODES.                                                    
009400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009500     SKIP3                                                                
009600 01  SSA1                        PIC X(64).                               
009700 01  SSA2                        PIC X(64).                               
009800     EJECT                                                                
009900*    --- IMS FUNCTION CODES                                               
010000*01  -COPY W0003                                                          
010100     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB6'.           
010400 01  DLI-IO-AREA                 PIC X(900).                              
010410 01  DLI-IO-AREA-601     REDEFINES DLI-IO-AREA.                           
010500*    03  -COPY WDB601                                                     
010600     EJECT                                                                
010620 01  DLI-IO-AREA-616     REDEFINES DLI-IO-AREA.                           
010630*    03  -COPY WDB616                                                     
010640     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010900                                                                          
011000*01  -COPY W0008  -PRE WDB6-                                              
011100     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011300 PROCEDURE DIVISION  USING WDB6-PCB.                                      
011400 MAIN SECTION.                                                            
011500     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
011600                                                                          
011700                                                                          
011800     PERFORM A-INIT                                                       
011900                                                                          
012000     PERFORM IMS-GET-WDB6                                                 
012100     PERFORM UNTIL SEGMENT-MISSING                                        
012200       EVALUATE WDB6-SEG-NAME-FB                                          
012300         WHEN 'WDB601'                                                    
012400           PERFORM B-MOVE-DATA                                            
012410         WHEN 'WDB616'                                                    
012420           PERFORM C-MOVE-DATA                                            
012500       END-EVALUATE                                                       
012600       PERFORM IMS-GET-WDB6                                               
012700     END-PERFORM                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500                                                                          
013600     OPEN OUTPUT W47945                                                   
013700                                                                          
013800     ACCEPT TODAYS-DATE  FROM DATE                                        
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014200 B-MOVE-DATA SECTION.                                                     
014300                                                                          
014400     MOVE DCS-IDDC              TO UT-IDDC                                
014500     MOVE DCS-IDLANDX2          TO UT-IDLANDX2                            
014510     MOVE DCS-IDLANDX2          TO WS-IDLANDX2                            
014600                                                                          
014700     IF DCS-IDDISTR-REFILL > 0                                            
014800        MOVE DCS-IDDISTR-REFILL TO UT-IDDISTR                             
014900        MOVE WS-REFILL          TO UT-DISTR-TYP                           
014910        MOVE '11'               TO UT-IDDC                                
014920        MOVE 'SE'               TO UT-IDLANDX2                            
015000        PERFORM S11-WRITE-W47945                                          
015100     END-IF                                                               
015200                                                                          
015300     IF DCS-IDDISTR-RETUR > 0                                             
015400     AND DCS-IDDISTR-RETUR NOT = DCS-IDDISTR-REFILL                       
015500        MOVE DCS-IDDISTR-RETUR  TO UT-IDDISTR                             
015600        MOVE WS-RETUR           TO UT-DISTR-TYP                           
015620        MOVE DCS-IDDC           TO UT-IDDC                                
015630        MOVE DCS-IDLANDX2       TO UT-IDLANDX2                            
015700        PERFORM S11-WRITE-W47945                                          
015800     END-IF                                                               
015900                                                                          
016000     IF DCS-IDDISTR-QRETUR > 0                                            
016100     AND DCS-IDDISTR-QRETUR NOT = DCS-IDDISTR-REFILL                      
016200     AND DCS-IDDISTR-QRETUR NOT = DCS-IDDISTR-RETUR                       
016300        MOVE DCS-IDDISTR-QRETUR TO UT-IDDISTR                             
016400        MOVE WS-QRETUR          TO UT-DISTR-TYP                           
016410        MOVE DCS-IDDC           TO UT-IDDC                                
016420        MOVE DCS-IDLANDX2       TO UT-IDLANDX2                            
016500        PERFORM S11-WRITE-W47945                                          
016600     END-IF                                                               
016700     .                                                                    
016800     EJECT                                                                
016801                                                                          
016810 C-MOVE-DATA SECTION.                                                     
016830     MOVE REF-IDDC-REF          TO UT-IDDC                                
016840     MOVE WS-IDLANDX2           TO UT-IDLANDX2                            
016850                                                                          
016860     IF REF-IDDISTR-REFILL > 0                                            
016870        MOVE REF-IDDISTR-REFILL TO UT-IDDISTR                             
016880        MOVE WS-REFILL          TO UT-DISTR-TYP                           
016891        PERFORM S11-WRITE-W47945                                          
016892     END-IF                                                               
016893                                                                          
016894     IF REF-IDDISTR-RETUR > 0                                             
016895     AND REF-IDDISTR-RETUR NOT = REF-IDDISTR-REFILL                       
016896        MOVE REF-IDDISTR-RETUR  TO UT-IDDISTR                             
016897        MOVE WS-RETUR           TO UT-DISTR-TYP                           
016898        PERFORM S11-WRITE-W47945                                          
016899     END-IF                                                               
016900                                                                          
016901     IF REF-IDDISTR-QRETUR > 0                                            
016902     AND REF-IDDISTR-QRETUR NOT = REF-IDDISTR-REFILL                      
016903     AND REF-IDDISTR-QRETUR NOT = REF-IDDISTR-RETUR                       
016904        MOVE REF-IDDISTR-QRETUR TO UT-IDDISTR                             
016905        MOVE WS-QRETUR          TO UT-DISTR-TYP                           
016906        PERFORM S11-WRITE-W47945                                          
016907     END-IF                                                               
016908     .                                                                    
016909     EJECT                                                                
016910 Z-FINIT SECTION.                                                         
017000     CLOSE W47945                                                         
017100     SKIP2                                                                
017200     MOVE 'S' TO POSTSUM-OPKOD                                            
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600 S11-WRITE-W47945 SECTION.                                                
017700                                                                          
017800     WRITE W47945-POST  FROM UT-AREA                                      
017900                                                                          
018000     MOVE 'W47945'   TO POSTSUM-FDNAMN                                    
018100     MOVE 'W47945D1' TO POSTSUM-DDNAMN2                                   
018200     CALL POSTSUM USING POSTSUM-PARM                                      
018300     .                                                                    
018400     EJECT                                                                
018500 S99-ABEND SECTION.                                                       
018600                                                                          
018700     SKIP2                                                                
018800     MOVE 'S'        TO POSTSUM-OPKOD                                     
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     CALL ABEND   USING RKOD-ABEND                                        
019100     .                                                                    
019200     EJECT                                                                
019300* --- IMS SECTIONS  ---                                                   
019400                                                                          
019500                                                                          
019600 IMS-GET-WDB6   SECTION.                                                  
019700                                                                          
019800     CALL CBLTDLI       USING GN WDB6-PCB DLI-IO-AREA                     
019900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
020000     MOVE '  GAGKGB'       TO GOOD-STATUSCODES                            
020100     PERFORM IMS-STATUSCHECK                                              
020200     .                                                                    
020300     EJECT                                                                
020400 IMS-STATUSCHECK SECTION.                                                 
020500                                                                          
020600     SET STATUS-IX TO 1                                                   
020700     SEARCH GOOD-STATUS                                                   
020800       AT END                                                             
020900         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
021000           DELIMITED BY SIZE INTO ERROR-TEXT                              
021100         DISPLAY ERROR-TEXT                                               
021200         CALL FELLOG                                                      
021300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
021400         CONTINUE                                                         
021500     END-SEARCH                                                           
021600     .                                                                    
