000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4832200.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   10/03/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        EXTRACT DEALERS INFO                                             
000900*                                                                         
001000*        THE PROGRAM READS     WDB2 WDB1                                  
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
002400*          --- DEALER INFORMATION                                         
002500     SELECT W48322                     ASSIGN TO W48322D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W48322                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  RECORD -COPY W4832201 -PRE  UT-  -L.                                 
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W4832200'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200     EJECT                                                                
004300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES TODAYS-DATE.                                        
004500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004700     03  TODAYS-DATE-DAY         PIC 9(2).                                
004800     EJECT                                                                
004900 01  GENERAL-SUBPROGRAMS.                                                 
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005500     SKIP2                                                                
005600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005700                                                                          
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100     SKIP2                                                                
006200 01  ERROR-TEXT.                                                          
006300     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  UT-AREA-START               PIC X(24)   VALUE                        
007100                                 'UT-AREA-START  '.                       
007200     SKIP2                                                                
007300                                                                          
007400*01  AREA -COPY W4832201     -PRE UT-                                     
007500     EJECT                                                                
007600*    --- AREAS FOR IMS-SECTIONS                                           
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-FOR-DLI.                                                        
008200     03  W-WDB101KY-X.                                                    
008300         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
008400         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FOUND                       VALUE '  '.                  
008900     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009000     SKIP2                                                                
009100 01  GOOD-STATUSCODES.                                                    
009200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNCTION CODES                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB1'.                        
010200 01  DLI-IO-WDB101.                                                       
010300*    03  -COPY WDB101                                                     
010400     EJECT                                                                
010500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB2'.                        
010600 01  DLI-IO-WDB201.                                                       
010700*    03  -COPY WDB201                                                     
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100                                                                          
011200*01  -COPY W0008  -PRE WDB1-                                              
011300     05  FILLER                  PIC X.                                   
011400*                                                                         
011500*01  -COPY W0008  -PRE WDB2-                                              
011600     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING WDB1-PCB WDB2-PCB.                             
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING WDB1-PCB WDB2-PCB.                             
012100                                                                          
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     PERFORM IMS-GN-WDB2                                                  
012500     PERFORM UNTIL SEGMENT-MISSING                                        
012600       EVALUATE WDB2-SEG-NAME-FB                                          
012700         WHEN 'WDB201'                                                    
012800           MOVE GMT-FLLDCKND     TO UT-FLLDCKND                           
012900           MOVE GMT-IDDISTR      TO UT-IDDISTR                            
013000           MOVE GMT-IDKUNDNR     TO UT-IDKUNDNR                           
013100           MOVE GMT-IDPARTNR     TO W-IDPARTNR                            
013200           MOVE GMT-IDFTG        TO W-IDFTG                               
013300           PERFORM IMS-GU-WDB101                                          
013400           IF SEGMENT-FOUND                                               
013500             MOVE BET-IDLANDX2   TO UT-IDLANDX2                           
013600           ELSE                                                           
013700             MOVE SPACE          TO UT-IDLANDX2                           
013800           END-IF                                                         
013900           PERFORM S11-WRITE-W48322                                       
014000       END-EVALUATE                                                       
014100       PERFORM IMS-GN-WDB2                                                
014200     END-PERFORM                                                          
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 A-INIT SECTION.                                                          
015000     OPEN OUTPUT W48322                                                   
015100                                                                          
015200     ACCEPT TODAYS-DATE  FROM DATE                                        
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400     .                                                                    
015500     EJECT                                                                
015600 Z-FINIT SECTION.                                                         
015700     CLOSE W48322                                                         
015800     SKIP2                                                                
015900     MOVE 'S' TO POSTSUM-OPKOD                                            
016000     CALL POSTSUM USING POSTSUM-PARM                                      
016100     .                                                                    
016200     EJECT                                                                
016300 S11-WRITE-W48322 SECTION.                                                
016400     WRITE UT-RECORD FROM UT-AREA                                         
016500     MOVE 'W48322'   TO POSTSUM-FDNAMN                                    
016600     MOVE 'W48322D1' TO POSTSUM-DDNAMN2                                   
016700     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016900     EJECT                                                                
017000 S99-ABEND SECTION.                                                       
017100     MOVE 'S' TO POSTSUM-OPKOD                                            
017200     CALL POSTSUM USING POSTSUM-PARM                                      
017300     CALL ABEND USING RKOD-ABEND                                          
017400     .                                                                    
017500     EJECT                                                                
017600* --- IMS SECTIONS  ---                                                   
017700 IMS-GN-WDB2   SECTION.                                                   
017800     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201                         
017900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
018000     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
018100     PERFORM IMS-STATUSCHECK                                              
018200     .                                                                    
018300     EJECT                                                                
018400 IMS-GU-WDB101  SECTION.                                                  
018500     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
018600            DELIMITED BY SIZE INTO SSA1                                   
018700     MOVE '  GE' TO GOOD-STATUSCODES                                      
018800     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101  SSA1                   
018900     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
019000     PERFORM IMS-STATUSCHECK                                              
019100     .                                                                    
019200     EJECT                                                                
019300 IMS-STATUSCHECK SECTION.                                                 
019400     SET STATUS-IX TO 1                                                   
019500     SEARCH GOOD-STATUS                                                   
019600       AT END                                                             
019700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
019800           DELIMITED BY SIZE INTO ERROR-TEXT                              
019900         DISPLAY ERROR-TEXT                                               
020000         CALL FELLOG                                                      
020100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
020200         CONTINUE                                                         
020300     END-SEARCH                                                           
020400     .                                                                    
