000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4183F00.                                                
000300 AUTHOR.         SURESH GUDIVADA.                                         
000400 DATE-WRITTEN.   24/10/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TO CREATE A GENERIC MQ FILE WITH HEADER AND DETAIL               
000900*        RECORDS FOR THE RESPECTIVE MARKETS.                              
001000*                                                                         
001100*        THE PROGRAM READS     WDB2                                       
001200*                                                                         
001300*    INDATA: W418.W418S3.W418C3                                           
001400*                                                                         
001500*    UTDATA: W418.W418S3.W4183F                                           
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INDATA                                                     
002600     SELECT W418C3                     ASSIGN TO W4183FD1.                
002700     SKIP2                                                                
002800*          --- UTDATA                                                     
002900     SELECT W4183F                     ASSIGN TO W4183FD2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W418C3                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST -COPY W4183C -PRE  IN-  -L.                                     
004000     SKIP3                                                                
004100 FD  W4183F                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W4183F -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W4183F00'.            
005000 77  IDPGM-SEND                  PIC X(8)    VALUE 'W4183E00'.            
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  W418C3-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W418C3                       VALUE 'J'.                   
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
007000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
007100 01  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  IN-AREA-START               PIC X(24)   VALUE                        
007800                                 'IN-AREA-START  '.                       
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W4183C     -PRE IN-                                       
008200     EJECT                                                                
008300 01  UT-AREA-START               PIC X(24)   VALUE                        
008400                                 'UT-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W4183F     -PRE UT-                                       
008800     EJECT                                                                
008900*    --- AREAS FOR IMS-SECTIONS                                           
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  KEYS-FOR-DLI.                                                        
009500     03  W-IDGMT-X.                                                       
009600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
009700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
009800                                                                          
009900     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FOUND                       VALUE '  '.                  
010300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010500     SKIP2                                                                
010600 01  GOOD-STATUSCODES.                                                    
010700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNCTION CODES                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
011700 01  DLI-IO-WDB201.                                                       
011800*    03  -COPY WDB201                                                     
011900     EJECT                                                                
012000 01  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
012100 01  WS-IDDISTR-N                PIC S9(5)              COMP-3.           
012200 01  FILLER  REDEFINES WS-IDDISTR-N.                                      
012300     03  FILLER                  PIC X(1).                                
012400     03  WS-IDDISTR-X            PIC X(4).                                
012500 01  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
012600 01  WS-IDKUNDNR-N               PIC S9(7)              COMP-3.           
012700 01  FILLER  REDEFINES WS-IDKUNDNR-N.                                     
012800     03  FILLER                  PIC X(1).                                
012900     03  WS-IDKUNDNR-X           PIC X(6).                                
013000**   --- MQ HEADER WITH THE COUNTRY CODE INFO ---  **                     
013100 01  IN-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
013200 01  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
013300 01  WS-HEADER                   PIC X(16)   VALUE                        
013400                                 '¤MQMPROP Market='.                      
013500 01  UT-HEADER                   PIC X(18)   VALUE SPACE.                 
013600     EJECT                                                                
014000                                                                          
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0008  -PRE WDB2-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500*                                                                         
014600 PROCEDURE DIVISION  USING WDB2-PCB.                                      
014700 MAIN SECTION.                                                            
014800     ENTRY 'DLITCBL' USING WDB2-PCB.                                      
014900                                                                          
015000     PERFORM A-INIT                                                       
015100     PERFORM S01-READ-W418C3                                              
015200     PERFORM UNTIL END-OF-W418C3                                          
015300          PERFORM B-CHECK-INPUT                                           
015400          IF WS-IDLANDX2 NOT = IN-IDLANDX2                                
015500             PERFORM S02-WRITE-HEADER                                     
015600          END-IF                                                          
015700          PERFORM S03-WRITE-W4183F                                        
015800          PERFORM S01-READ-W418C3                                         
015900     END-PERFORM                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 A-INIT SECTION.                                                          
016800     OPEN INPUT  W418C3                                                   
016900                                                                          
017000     OPEN OUTPUT W4183F                                                   
017100                                                                          
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017300     INITIALIZE IN-AREA                                                   
017400                WS-IDDISTR                                                
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 B-CHECK-INPUT SECTION.                                                   
017900** ---- DISTRICT NUMBER ---- **                                           
018000     MOVE IN-IDDISTR                   TO WS-IDDISTR-X                    
018100     MOVE FUNCTION TRIM(WS-IDDISTR-X)  TO WS-IDDISTR-N                    
018200     MOVE WS-IDDISTR-N                 TO W-IDDISTR                       
018300** ---- CUSTOMER NUMBER ---- **                                           
018400     MOVE IN-IDKUNDNR                  TO WS-IDKUNDNR-X                   
018500     MOVE FUNCTION TRIM(WS-IDKUNDNR-X) TO WS-IDKUNDNR-N                   
018600     MOVE WS-IDKUNDNR-N                TO W-IDKUNDNR                      
018700                                                                          
018800     PERFORM IMS-GET-WDB201                                               
018900     IF SEGMENT-FOUND                                                     
019000        IF WS-IDDISTR-N    NOT = WS-IDDISTR                               
019100           MOVE GMT-IDLANDX2  TO IN-IDLANDX2                              
019200           MOVE IN-IDDISTR    TO WS-IDDISTR                               
019300        END-IF                                                            
019400     ELSE                                                                 
019500        DISPLAY 'District NO: ' WS-IDDISTR-N                              
019600        DISPLAY 'Customer NO: ' WS-IDKUNDNR-N                             
019700        DISPLAY 'Combination Missing in WDB2'                             
019800        MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                             
019900        PERFORM S99-ABEND                                                 
020000     END-IF                                                               
020100     EJECT                                                                
020200     .                                                                    
020300 S01-READ-W418C3  SECTION.                                                
020400     READ W418C3 INTO IN-AREA                                             
020500     AT END                                                               
020600        MOVE HIGH-VALUE   TO IN-AREA                                      
020700        SET END-OF-W418C3 TO TRUE                                         
020800                                                                          
020900     NOT AT END                                                           
021000        MOVE 'W418C3'   TO POSTSUM-FDNAMN                                 
021100        MOVE 'W4183FD1' TO POSTSUM-DDNAMN2                                
021200        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
021300        CALL POSTSUM USING POSTSUM-PARM                                   
021400     END-READ                                                             
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 S02-WRITE-HEADER SECTION.                                                
021900     INITIALIZE UT-HEADER                                                 
022000                                                                          
022100     STRING WS-HEADER      DELIMITED BY SIZE                              
022200            IN-IDLANDX2    DELIMITED BY SIZE                              
022300       INTO UT-HEADER                                                     
022400     END-STRING                                                           
022500     WRITE UT-POST    FROM UT-HEADER                                      
022600     MOVE IN-IDLANDX2   TO WS-IDLANDX2                                    
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 S03-WRITE-W4183F SECTION.                                                
023100     INITIALIZE UT-AREA                                                   
023200                                                                          
023300** ---- SEND-LINE-TO-VIPS --- **                                          
023400     MOVE 1                TO UT-IDMSGVER                                 
023500     MOVE 'R'              TO UT-KDPGMACT                                 
023600     MOVE IDPGM-SEND       TO UT-IDUSER                                   
023700*                                                                         
023800     MOVE IN-IDPTYP        TO UT-IDPTYP                                   
023900     MOVE IN-IDFINDOC      TO UT-IDFINDOC                                 
024000     MOVE IN-DAFINDOC      TO UT-DAFINDOC                                 
024100     MOVE IN-IDPARTNR      TO UT-IDPARTNR                                 
024200     MOVE IN-IDDISTR       TO UT-IDDISTR                                  
024300     MOVE IN-IDKUNDNR      TO UT-IDKUNDNR                                 
024400     MOVE IN-BEART         TO UT-BEART                                    
024500     MOVE IN-KVLEVART-VIPS TO UT-KVLEVART-VIPS                            
024600     MOVE IN-PRARTNTO-VIPS TO UT-PRARTNTO-VIPS                            
024700     MOVE IN-SUNTO-VIPS    TO UT-SUNTO-VIPS                               
024800     MOVE IN-SUVAT-VIPS    TO UT-SUVAT-VIPS                               
024900     MOVE IN-SUBTO-VIPS    TO UT-SUBTO-VIPS                               
025000     MOVE IN-KDVAT         TO UT-KDVAT                                    
025100     MOVE IN-KDVALISO-LOC  TO UT-KDVALISO-LOC                             
025200     MOVE IN-PRKURS        TO UT-PRKURS                                   
025300     MOVE IN-IDREF         TO UT-IDREF                                    
025400                                                                          
025500     WRITE UT-POST   FROM UT-AREA                                         
025600     MOVE UT-IDPTYP    TO POSTSUM-TRANSTYP                                
025700     MOVE 'W4183F'     TO POSTSUM-FDNAMN                                  
025800     MOVE 'W4183FD2'   TO POSTSUM-DDNAMN2                                 
025900     CALL POSTSUM   USING POSTSUM-PARM                                    
026000     .                                                                    
026100     EJECT                                                                
026200                                                                          
026300 S99-ABEND SECTION.                                                       
026400     SKIP2                                                                
026500     MOVE 'S'        TO POSTSUM-OPKOD                                     
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026700     CALL ABEND   USING RKOD-ABEND                                        
026800     .                                                                    
026900     EJECT                                                                
027000                                                                          
027100 Z-FINIT SECTION.                                                         
027200     CLOSE W418C3                                                         
027300           W4183F                                                         
027400     SKIP2                                                                
027500     MOVE 'S'        TO POSTSUM-OPKOD                                     
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000* --- IMS SECTIONS  ---                                                   
028100 IMS-GET-WDB201 SECTION.                                                  
028200                                                                          
028300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
028400          DELIMITED BY SIZE INTO SSA1                                     
028500     MOVE '  GE'     TO GOOD-STATUSCODES                                  
028600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
028700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSCHECK                                              
028900     .                                                                    
029000     EJECT                                                                
029100*                                                                         
029200 IMS-STATUSCHECK SECTION.                                                 
029300                                                                          
029400     SET STATUS-IX TO 1                                                   
029500     SEARCH GOOD-STATUS                                                   
029600       AT END                                                             
029700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
029800           DELIMITED BY SIZE INTO ERROR-TEXT                              
029900         DISPLAY ERROR-TEXT                                               
030000         CALL FELLOG                                                      
030100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030200         CONTINUE                                                         
030300     END-SEARCH                                                           
030400     .                                                                    
