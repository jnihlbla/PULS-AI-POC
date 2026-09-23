000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3714000.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   13/10/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*      READ WDM6, EXTRACT RECORDS WITH STATUS CODE = 2 OR 3.              
000900*                                                                         
001000*      THE PROGRAM READS     WDM6                                         
001100*                                                                         
001200*    CHANGE LOG:                                                          
001300*      13/10/16 - REDDY RAHUL     - INITIAL VERSION                       
001400*                                   SCR 10205485                          
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- EXCHANGE CORES WITH STATUS CODE 2 OR 3                     
002400     SELECT W37140                     ASSIGN TO W37140D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W37140                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300 01  UT-RECORD                   PIC X(45).                               
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W3714000'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 01  WS-HEADING.                                                          
004100     03  FILLER                  PIC X(04)   VALUE 'DIST'.                
004200     03  FILLER                  PIC X(01)   VALUE X'05'.                 
004300     03  FILLER                  PIC X(04)   VALUE 'CUST'.                
004400     03  FILLER                  PIC X(01)   VALUE X'05'.                 
004500     03  FILLER                  PIC X(06)   VALUE 'STATUS'.              
004600     03  FILLER                  PIC X(01)   VALUE X'05'.                 
004700     03  FILLER                  PIC X(06)   VALUE 'REPORT'.              
004800     03  FILLER                  PIC X(01)   VALUE X'05'.                 
004900     03  FILLER                  PIC X(08)   VALUE 'REG DATE'.            
005000     03  FILLER                  PIC X(01)   VALUE X'05'.                 
005100     03  FILLER                  PIC X(12)   VALUE 'DATE ARRIVAL'.        
005200                                                                          
005300 01  WS-HEADER-FLAG              PIC X       VALUE 'N'.                   
005400     88 WS-HEADER-WRITTEN                    VALUE 'J'.                   
005500     88 WS-HEADER-NOT-WRITTEN                VALUE 'N'.                   
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
007800 01  UT-AREA-START               PIC X(24)   VALUE                        
007900                                 'UT-AREA-START  '.                       
008000     SKIP2                                                                
008100 01  UT-AREA.                                                             
008200     03  UT-IDDISTR              PIC 9(5)    VALUE ZERO.                  
008300     03  FILLER                  PIC X       VALUE X'05'.                 
008400     03  UT-IDKUNDNR             PIC 9(7)    VALUE ZERO.                  
008500     03  FILLER                  PIC X       VALUE X'05'.                 
008600     03  UT-KDBYTSTA-RAPP        PIC X       VALUE SPACE.                 
008700     03  FILLER                  PIC X       VALUE X'05'.                 
008800     03  UT-IDBYTRAP             PIC 9(7)    VALUE ZERO.                  
008900     03  FILLER                  PIC X       VALUE X'05'.                 
009000     03  UT-DAREGDAT             PIC 9(8)    VALUE ZERO.                  
009100     03  FILLER                  PIC X       VALUE X'05'.                 
009200     03  UT-DAANKDAG             PIC 9(8)    VALUE ZERO.                  
009300     03  FILLER                  PIC X(4)    VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- AREAS FOR IMS-SECTIONS                                           
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  KEYS-FOR-DLI.                                                        
010100     03  W-WDM601KY-X.                                                    
010200         05  W-WDM601KY          PIC X(7)    VALUE SPACE.                 
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FOUND                       VALUE '  '.                  
010700     88  END-OF-TABLE                        VALUE 'GB'.                  
010800     SKIP2                                                                
010900 01  GOOD-STATUSCODES.                                                    
011000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNCTION CODES                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM601'.                      
011900 01  DLI-IO-WDM601.                                                       
012000*    03  -COPY WDM601                                                     
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0008  -PRE WDM6-                                              
012500     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION  USING WDM6-PCB.                                      
012800 MAIN SECTION.                                                            
012900     ENTRY 'DLITCBL' USING WDM6-PCB.                                      
013000                                                                          
013100     PERFORM A-INIT                                                       
013200     PERFORM B-PROCESS                                                    
013300     PERFORM Z-FINIT                                                      
013400     MOVE ZERO                   TO RETURN-CODE                           
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900                                                                          
014000     OPEN OUTPUT W37140                                                   
014100     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
014200     .                                                                    
014300     EJECT                                                                
014400 B-PROCESS SECTION.                                                       
014500                                                                          
014600     PERFORM IMS-GET-WDM601                                               
014700     PERFORM UNTIL END-OF-TABLE                                           
014900       IF RAPP-KDBYTSTA-RAPP = '2' OR '3'                                 
015000         IF WS-HEADER-NOT-WRITTEN                                         
015100           MOVE WS-HEADING       TO UT-RECORD                             
015200           PERFORM S11-WRITE-W37140                                       
015300           SET WS-HEADER-WRITTEN TO TRUE                                  
015400         END-IF                                                           
015500         MOVE RAPP-IDDISTR       TO UT-IDDISTR                            
015600         MOVE RAPP-IDKUNDNR      TO UT-IDKUNDNR                           
015700         MOVE RAPP-KDBYTSTA-RAPP TO UT-KDBYTSTA-RAPP                      
015800         MOVE RAPP-IDBYTRAP      TO UT-IDBYTRAP                           
015900         MOVE RAPP-DAREGDAT      TO UT-DAREGDAT                           
016000         MOVE RAPP-DAANKDAG      TO UT-DAANKDAG                           
016100         MOVE UT-AREA            TO UT-RECORD                             
016200         PERFORM S11-WRITE-W37140                                         
016300       END-IF                                                             
016400       PERFORM IMS-GET-WDM601                                             
016500     END-PERFORM                                                          
016600     .                                                                    
016700     EJECT                                                                
016800 Z-FINIT SECTION.                                                         
016900     CLOSE W37140                                                         
017000     SKIP2                                                                
017100     MOVE 'S'                    TO POSTSUM-OPKOD                         
017200     CALL POSTSUM             USING POSTSUM-PARM                          
017300     .                                                                    
017400     EJECT                                                                
017500 S11-WRITE-W37140 SECTION.                                                
017600                                                                          
017700     WRITE UT-RECORD                                                      
017800                                                                          
017900     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
018000     MOVE 'W37140'               TO POSTSUM-FDNAMN                        
018100     MOVE 'W37140D1'             TO POSTSUM-DDNAMN2                       
018200     CALL POSTSUM             USING POSTSUM-PARM                          
018300     .                                                                    
018400     EJECT                                                                
018500 S99-ABEND SECTION.                                                       
018600                                                                          
018700     SKIP2                                                                
018800     MOVE 'S'                    TO POSTSUM-OPKOD                         
018900     CALL POSTSUM             USING POSTSUM-PARM                          
019000     CALL ABEND               USING RKOD-ABEND                            
019100     .                                                                    
019200     EJECT                                                                
019300* --- IMS SECTIONS  ---                                                   
019400                                                                          
019500     EJECT                                                                
019600 IMS-GET-WDM601 SECTION.                                                  
019700                                                                          
019800     MOVE 'WDM601  '             TO SSA1                                  
019900     MOVE '  GB'                 TO GOOD-STATUSCODES                      
020000     CALL CBLTDLI USING GN WDM6-PCB DLI-IO-WDM601 SSA1                    
020100     MOVE WDM6-STATUS-CODE       TO STATUS-WS                             
020200     PERFORM IMS-STATUSCHECK                                              
020300     .                                                                    
020400     EJECT                                                                
020500 IMS-STATUSCHECK SECTION.                                                 
020600                                                                          
020700     SET STATUS-IX               TO 1                                     
020800     SEARCH GOOD-STATUS                                                   
020900       AT END                                                             
021000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021100           DELIMITED BY SIZE INTO ERROR-TEXT                              
021200         DISPLAY ERROR-TEXT                                               
021300         CALL FELLOG                                                      
021400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
021500         CONTINUE                                                         
021600     END-SEARCH                                                           
021700     .                                                                    
