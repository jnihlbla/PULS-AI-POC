000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3719400.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   14/12/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        GENERATE REPORT FILE WITH DISTRICT AND CORE NUMBER FOR           
000900*        WHICH CHANGES HAVE BEEN MADE IN NES ADMIN SCREEN DURING          
001000*        THE LAST PERIOD.                                                 
001100*                                                                         
001200*        THE PROGRAM READS     WDA9                                       
001300*                                                                         
001400*    ABENDCODES:                                                          
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
002600*          --- NES ADMIN CHANGES IN LAST PERIOD                           
002700     SELECT W37194                     ASSIGN TO W37194D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W37194                                                               
003400     RECORDING       V                                                    
003500     BLOCK CONTAINS  0.                                                   
003600 01  UT-POST                     PIC X(16).                               
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W3719400'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000 01  WS-DAAARP-LIMIT             PIC 9(6)    VALUE 200000.                
005100 01  SW-HEADER-REC               PIC X       VALUE 'J'.                   
005200     88  HEADER-YES                          VALUE 'J'.                   
005300     88  HEADER-NO                           VALUE 'N'.                   
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
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
007400*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
007500*                                                                         
007600 01  PROGRAM-NAME                PIC X(6)    VALUE 'W37194'.              
007700     SKIP2                                                                
007800 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
007900     SKIP2                                                                
008000*01  -COPY WDATKORT                                                       
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600*01  -COPY WDATAREA                                                       
008700     EJECT                                                                
008800 01  UT-AREA-START               PIC X(24)   VALUE                        
008900                                 'UT-AREA-START  '.                       
009000     SKIP2                                                                
009100 01  UT-AREA.                                                             
009200     05  UT-IDARTNR              PIC Z(9).                                
009300     05  FILLER                  PIC X(1)      VALUE ';'.                 
009400     05  UT-IDDISTR              PIC Z(4).                                
009500     SKIP2                                                                
009600 01  UT-HEADER                   PIC X(16)                                
009700                               VALUE 'CORE NO;DISTRICT'.                  
009800     EJECT                                                                
009900*    --- AREAS FOR IMS-SECTIONS                                           
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  KEYS-FOR-DLI.                                                        
010500     03  W-DAAAPP-X.                                                      
010600         05  W-DAAAPP            PIC X(6)    VALUE SPACE.                 
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FOUND                       VALUE '  '.                  
011100     88  SEGMENT-MISSING                     VALUE 'GE' 'GB'.             
011200     SKIP2                                                                
011300 01  GOOD-STATUSCODES.                                                    
011400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNCTION CODES                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA921'.                      
012300 01  DLI-IO-WDA921.                                                       
012400*    03  -COPY WDA921                                                     
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700                                                                          
012800*01  -COPY W0008  -PRE WDA9-                                              
012900     05  KFB-IDARTNR             PIC S9(9) COMP-3.                        
013000     05  KFB-IDDISTR             PIC S9(5) COMP-3.                        
013100     05  KFB-DAAAPP              PIC 9(6).                                
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING WDA9-PCB.                                      
013400 MAIN SECTION.                                                            
013500     ENTRY 'DLITCBL' USING WDA9-PCB.                                      
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM                                                              
014000       UNTIL SEGMENT-MISSING                                              
014100       MOVE KFB-IDARTNR          TO UT-IDARTNR                            
014200       MOVE KFB-IDDISTR          TO UT-IDDISTR                            
014300       PERFORM S11-WRITE-W37194                                           
014400       PERFORM IMS-GN-WDA921                                              
014500     END-PERFORM                                                          
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800                                                                          
014900     MOVE ZERO                   TO RETURN-CODE                           
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-INIT SECTION.                                                          
015400                                                                          
015500     OPEN OUTPUT W37194                                                   
015600                                                                          
015700     CALL DATKORT             USING PROGRAM-NAME                          
015800                                    DATECARD-ID                           
015900                                    DATUMKORT                             
016000     MOVE D-AAR                  TO TODAYS-DATE-YEAR                      
016100     MOVE D-MAANAD               TO TODAYS-DATE-MONTH                     
016200     MOVE D-DAG                  TO TODAYS-DATE-DAY                       
016300     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
016400                                                                          
016500     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
016600     MOVE TODAYS-DATE            TO DAT-I-TIDATUM                         
016700                                                                          
016800     CALL WDATKONV            USING DAT-KDDATFORM                         
016900                                    DAT-I-TIDATUM                         
017000                                    DAT-O-TIDATUM                         
017100                                    DAT-KDSVAR                            
017200                                                                          
017300     IF DAT-KDSVAR-OK                                                     
017900       ADD DAT-TIAARP            TO WS-DAAARP-LIMIT                       
018000       DISPLAY 'PREVIOUS PERIOD = ' WS-DAAARP-LIMIT                       
018100     ELSE                                                                 
018200       DISPLAY 'ERROR IN WDATKONV'                                        
018300       CALL FELLOG                                                        
018400     END-IF                                                               
018500                                                                          
018600     MOVE WS-DAAARP-LIMIT        TO W-DAAAPP                              
018700     PERFORM IMS-GN-WDA921                                                
018800                                                                          
018900     .                                                                    
019000     EJECT                                                                
019100 Z-FINIT SECTION.                                                         
019200     CLOSE W37194                                                         
019300     SKIP2                                                                
019400     MOVE 'S'                    TO POSTSUM-OPKOD                         
019500     CALL POSTSUM             USING POSTSUM-PARM                          
019600     .                                                                    
019700     EJECT                                                                
019800 S11-WRITE-W37194 SECTION.                                                
019900                                                                          
020000     IF HEADER-YES                                                        
020100       WRITE UT-POST           FROM UT-HEADER                             
020200       SET HEADER-NO             TO TRUE                                  
020300     END-IF                                                               
020400                                                                          
020500     WRITE UT-POST             FROM UT-AREA                               
020600                                                                          
020700     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
020800     MOVE 'W37194'               TO POSTSUM-FDNAMN                        
020900     MOVE 'W37194D1'             TO POSTSUM-DDNAMN2                       
021000     CALL POSTSUM             USING POSTSUM-PARM                          
021100     .                                                                    
021200     EJECT                                                                
021300* --- IMS SECTIONS  ---                                                   
021400                                                                          
021500     EJECT                                                                
021600 IMS-GN-WDA921 SECTION.                                                   
021700                                                                          
021800     STRING 'WDA921  (DAAAPP   =' W-DAAAPP-X ')'                          
021900             DELIMITED BY SIZE INTO SSA1                                  
022000     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
022100     CALL CBLTDLI USING GN WDA9-PCB DLI-IO-WDA921 SSA1                    
022200     MOVE WDA9-STATUS-CODE       TO STATUS-WS                             
022300     PERFORM IMS-STATUSCHECK                                              
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-STATUSCHECK SECTION.                                                 
022700                                                                          
022800     SET STATUS-IX TO 1                                                   
022900     SEARCH GOOD-STATUS                                                   
023000       AT END                                                             
023100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023200           DELIMITED BY SIZE INTO ERROR-TEXT                              
023300         DISPLAY ERROR-TEXT                                               
023400         CALL FELLOG                                                      
023500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
023600         CONTINUE                                                         
023700     END-SEARCH                                                           
023800     .                                                                    
