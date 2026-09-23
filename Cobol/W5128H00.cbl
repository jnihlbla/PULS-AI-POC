000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5128H00.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   19/12/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE R30, R31 AND 310 REPORT FROM WDL2                         
000900*                                                                         
001000*        THE PROGRAM READS     WDL2                                       
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
002610*          --- WDL2 R30, R31 AND 310 EXTRACT                              
002620     SELECT W5128H                     ASSIGN TO W5128HD1.                
002630     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003100 FD  W5128H                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400*01  RECORD -COPY W5128H -PRE  UT-  -L.                                   
003500     SKIP3                                                                
003560                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W5128H00'.            
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100     EJECT                                                                
004200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004300 01  FILLER REDEFINES TODAYS-DATE.                                        
004400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004600     03  TODAYS-DATE-DAY         PIC 9(2).                                
004700     EJECT                                                                
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900*                                                                         
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005400     SKIP2                                                                
005500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  ERROR-TEXT.                                                          
006200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006810*01  -COPY WWDC99                                                         
006820     EJECT                                                                
006900 01  UT-AREA-START              PIC X(24)   VALUE                         
007000                                 'UT-AREA-START  '.                       
007100*01  AREA -COPY W5128H     -PRE UT-                                       
007200     SKIP2                                                                
007300     EJECT                                                                
007400*    --- AREAS FOR IMS-SECTIONS                                           
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900*    --- STATUS-KOD FRÅN IMS                                              
008000 01  STATUS-WS                   PIC XX.                                  
008100     88  SEGMENT-FOUND                       VALUE '  '.                  
008200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008400     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
008500     SKIP2                                                                
008600 01  GOOD-STATUSCODES.                                                    
008700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008800     SKIP3                                                                
008900 01  SSA1                        PIC X(64).                               
009000 01  SSA2                        PIC X(64).                               
009100     EJECT                                                                
009200*    --- IMS FUNCTION CODES                                               
009300*01  -COPY W0003                                                          
009400     EJECT                                                                
009500*    ---  DLI INPUT-OUTPUT AREA                                           
009600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
009700 01  DLI-IO-AREA.                                                         
009800     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
009810     SKIP3                                                                
009820     03  WDL201   REDEFINES IO-AREA.                                      
009830*        05  -COPY WDL201                                                 
009840     SKIP3                                                                
009850     03  WDL221   REDEFINES IO-AREA.                                      
009860*        05  -COPY WDL221                                                 
010000     EJECT                                                                
010100 LINKAGE SECTION.                                                         
010200                                                                          
010300                                                                          
010400*01  -COPY W0008  -PRE WDL2-                                              
010500     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010700 PROCEDURE DIVISION  USING WDL2-PCB.                                      
010800 MAIN SECTION.                                                            
010900     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
011000                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011400     PERFORM IMS-GN-WDL2                                                  
011500     PERFORM UNTIL SEGMENT-END-OF-DB                                      
011600       EVALUATE WDL2-SEG-NAME-FB                                          
011700         WHEN 'WDL201'                                                    
011800          MOVE ART-IDARTNR      TO UT-IDARTNR                             
011900                                                                          
012000         WHEN 'WDL221'                                                    
012100          IF MOT-IDPTYP = 'R30' OR 'R31' OR '310'                         
012200            PERFORM B-PROCESS                                             
012300          END-IF                                                          
012400       END-EVALUATE                                                       
012500       PERFORM IMS-GN-WDL2                                                
012600     END-PERFORM                                                          
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500                                                                          
013610     OPEN OUTPUT W5128H                                                   
013700                                                                          
013800     ACCEPT TODAYS-DATE  FROM DATE                                        
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014200 B-PROCESS SECTION.                                                       
014300                                                                          
014301     MOVE MOT-IDDC TO WS-IDDC                                             
014302     IF (MOT-KVAVIS NOT= ZERO) AND                                        
014303         NOT (NDC-NA OR LDC-CN OR XDC-NON-VCC-OWNED)                      
014315       MOVE MOT-IDPTYP   TO UT-IDPTYP                                     
014316       MOVE MOT-IDDC     TO UT-IDDC                                       
014317       MOVE MOT-KVAVIS   TO UT-KVAVIS                                     
014318       PERFORM S11-WRITE-W5128H                                           
014319     END-IF                                                               
014320     .                                                                    
014330     EJECT                                                                
014900 Z-FINIT SECTION.                                                         
015000     CLOSE W5128H                                                         
015100     SKIP2                                                                
015200     MOVE 'S' TO POSTSUM-OPKOD                                            
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600 S11-WRITE-W5128H SECTION.                                                
015700                                                                          
015800     WRITE UT-RECORD FROM UT-AREA                                         
015900                                                                          
016000     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
016100     MOVE 'W5128H'   TO POSTSUM-FDNAMN                                    
016200     MOVE 'W5128HD1' TO POSTSUM-DDNAMN2                                   
016300     CALL POSTSUM USING POSTSUM-PARM                                      
016400     .                                                                    
016500     EJECT                                                                
016600 S99-ABEND SECTION.                                                       
016700                                                                          
016800     SKIP2                                                                
016900     MOVE 'S' TO POSTSUM-OPKOD                                            
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     CALL ABEND USING RKOD-ABEND                                          
017200     .                                                                    
017300     EJECT                                                                
017400* --- IMS SECTIONS  ---                                                   
017500                                                                          
017600     EJECT                                                                
017700 IMS-GN-WDL2   SECTION.                                                   
017800                                                                          
017900     CALL CBLTDLI       USING GN WDL2-PCB DLI-IO-AREA                     
018000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
018100     MOVE '  GAGKGB'       TO GOOD-STATUSCODES                            
018200     PERFORM IMS-STATUSCHECK                                              
018300     .                                                                    
018400     EJECT                                                                
018500 IMS-STATUSCHECK SECTION.                                                 
018600                                                                          
018700     SET STATUS-IX TO 1                                                   
018800     SEARCH GOOD-STATUS                                                   
018900       AT END                                                             
019000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
019100           DELIMITED BY SIZE INTO ERROR-TEXT                              
019200         DISPLAY ERROR-TEXT                                               
019300         CALL FELLOG                                                      
019400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
019500         CONTINUE                                                         
019600     END-SEARCH                                                           
019700     .                                                                    
