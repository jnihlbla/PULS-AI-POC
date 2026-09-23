000100 PROCESS DYNAM                                                            
000200 PROCESS DYNAM                                                            
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     W5102700.                                                
000600 AUTHOR.         MAMATHA SHETTY.                                          
000700 DATE-WRITTEN.   22/06/19.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        THIS PROGRAM CREATES THE VAT FILE W51027 FROM W51067 AND         
001300*        WDK6 FILE                                                        
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INPUT WDK6 FILE                                            
002100     SELECT W01160    ASSIGN TO W51027D1.                                 
002200*          --- INPUT FILE                                                 
002300     SELECT W51067    ASSIGN TO W51027D2.                                 
002400*          --- OUTPUT FILE                                                
002500     SELECT W51027    ASSIGN TO W51027D3.                                 
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W01160                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300*01  -COPY W01160 -L.                                                     
003400                                                                          
003500     SKIP3                                                                
003600 FD  W51067                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  -COPY W51060 -L.                                                     
004000                                                                          
004100     SKIP3                                                                
004200 FD  W51027                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500*01  POST-REC -COPY W475INT  -L.                                          
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 01  UTRAD.                                                               
005000     03  UT-IDPTYP               PIC X(3).                                
005100     03  UT-IDLANDX2             PIC X(2).                                
005200     03  UT-TIFAKT               PIC S9(7)           COMP-3.              
005300     03  UT-IDARTNR              PIC S9(9)           COMP-3.              
005400     03  UT-KVLEVART             PIC S9(7)           COMP-3.              
005500     03  UT-SUFKTBEL             PIC S9(9)V9(2)      COMP-3.              
005600     03  UT-IDSTATNR             PIC S9(9)           COMP-3.              
005700     03  UT-KDARTURS             PIC X(2).                                
005800     03  UT-IDVAT                PIC X(17).                               
005900     03  UT-KDINTTYP             PIC S9(2)           COMP-3.              
006000 77  IDPGM                       PIC X(8)    VALUE 'W5102700'.            
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 01  WS-DATUM-YYMMDD             PIC 9(6).                                
006400     EJECT                                                                
006500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006600     EJECT                                                                
006700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51027'.              
006800                                                                          
006900 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
007000 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007100 01  W-IDDC-B6-X.                                                         
007200     05  W-IDDC-B6               PIC X(02)   VALUE SPACE.                 
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 01  W-KDTRADP                   PIC X(4).                                
008200 01  W51067-EOF                  PIC X       VALUE 'N'.                   
008300 01  W01160-EOF                  PIC X       VALUE 'N'.                   
008400     SKIP2                                                                
008500 01  ERROR-TEXT.                                                          
008600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900 01  KEYS-TILL-DLI.                                                       
009000     03  W-IDDC-X.                                                        
009100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009200     SKIP2                                                                
009300*    --- STATUS-CODE FOR IMS                                              
009400 01  STATUS-WS                               PIC XX.                      
009500     88  SEGMENT-FOUND                       VALUE '  '.                  
009600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009800     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
009900     88  IMS-NOT-OK                          VALUE 'XD'.                  
010000     SKIP2                                                                
010100 01  GOOD-STATUSCODES.                                                    
010200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(164).                              
010500     EJECT                                                                
010600 01  TEST-IDLANDX2          PIC X(2).                                     
010700*01  FILLER  -COPY WWLANDX2 -RED TEST-IDLANDX2.                           
010800     EJECT                                                                
010900*    -COPY W0005     -PRE POSTSUM-.                                       
011000*    -COPY WWDC99    -PRE CPY-.                                           
011100     EJECT                                                                
011200 01  FILLER              PIC X(130) VALUE 'IN-AREA'.                      
011300*01  IN-AREA -COPY W51060                                                 
011400      EJECT                                                               
011500 01  FILLER              PIC X(130) VALUE 'CLAG-AREA'.                    
011600*01  CLAG-AREA -COPY W01160                                               
011700      EJECT                                                               
011800*    --- IMS FUNCTION CODES                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
012300 01   DLI-IO-AREA-B6.                                                     
012500*    03  -COPY WDB601                                                     
012600      EJECT                                                               
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*01  -COPY W0008  -PRE WDB6-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING WDB6-PCB.                                      
013300 MAIN SECTION.                                                            
013400     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
013500                                                                          
013600     PERFORM A-INIT                                                       
013700     PERFORM S01-READ-W01160                                              
013800     PERFORM S01-READ-W51067                                              
013900     PERFORM UNTIL W01160-EOF = JA OR  W51067-EOF  = JA                   
014000         IF EKHT-IDARTNR = CLAG-IDARTNR                                   
014100           PERFORM EVALUATE-W51067                                        
014200           PERFORM S01-READ-W51067                                        
014300         ELSE                                                             
014400            IF EKHT-IDARTNR < CLAG-IDARTNR                                
014500              PERFORM S01-READ-W51067                                     
014600            ELSE                                                          
014700              PERFORM S01-READ-W01160                                     
014800            END-IF                                                        
014900         END-IF                                                           
015000     END-PERFORM                                                          
015100                                                                          
015200     PERFORM Z-FINISH                                                     
015300                                                                          
015400     MOVE +0 TO RETURN-CODE                                               
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900     OPEN INPUT W51067                                                    
016000                W01160                                                    
016100                                                                          
016200     OPEN OUTPUT W51027                                                   
016300                                                                          
016400     MOVE SPACE  TO UT-IDPTYP                                             
016500                    UT-IDLANDX2                                           
016600                    UT-KDARTURS                                           
016700                    UT-IDVAT                                              
016800                                                                          
016900     MOVE ZERO   TO UT-TIFAKT                                             
017000                    UT-IDARTNR                                            
017100                    UT-KVLEVART                                           
017200                    UT-SUFKTBEL                                           
017300                    UT-IDSTATNR                                           
017400                    UT-KDINTTYP                                           
017500                                                                          
017600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017700     .                                                                    
017800     EJECT                                                                
017900 Z-FINISH SECTION.                                                        
018000                                                                          
018100     CLOSE W51067                                                         
018200           W01160                                                         
018300           W51027                                                         
018400     .                                                                    
018500 EVALUATE-W51067 SECTION.                                                 
018600                                                                          
018700     IF EKHT-KDEKHHT = '503'                                              
018800       EVALUATE  EKHT-KDEKSHT                                             
018900        WHEN '501'                                                        
019000        WHEN '503'                                                        
019100           PERFORM B-SEARCH-IDDC-TAB1                                     
019200        WHEN '502'                                                        
019300        WHEN '504'                                                        
019400           PERFORM B-SEARCH-IDDC-TAB2                                     
019500        END-EVALUATE                                                      
019600     END-IF                                                               
019700     .                                                                    
019800 S01-READ-W51067 SECTION.                                                 
019900                                                                          
020000     READ W51067 INTO EKHT-W51060                                         
020100     AT END                                                               
020200        MOVE JA              TO W51067-EOF                                
020300     NOT AT END                                                           
020400        MOVE 'W51067'    TO POSTSUM-FDNAMN                                
020500        MOVE 'W51027D2'  TO POSTSUM-DDNAMN2                               
020600        MOVE SPACE       TO POSTSUM-TRANSTYP                              
020700        CALL POSTSUM USING POSTSUM-PARM                                   
020800     END-READ                                                             
020900     .                                                                    
021000 S01-READ-W01160 SECTION.                                                 
021100                                                                          
021200     READ W01160 INTO CLAG-W01160                                         
021300     AT END                                                               
021400        MOVE JA              TO W01160-EOF                                
021500     NOT AT END                                                           
021600        MOVE 'W01160'    TO POSTSUM-FDNAMN                                
021700        MOVE 'W51027D1'  TO POSTSUM-DDNAMN2                               
021800        MOVE SPACE       TO POSTSUM-TRANSTYP                              
021900        CALL POSTSUM USING POSTSUM-PARM                                   
022000     END-READ                                                             
022100     .                                                                    
022200      EJECT                                                               
022300 B-SEARCH-IDDC-TAB1 SECTION.                                              
022400                                                                          
022500     MOVE EKHT-IDDC-REC      TO   W-IDDC-B6                               
022600     MOVE 'SEPV'             TO   W-KDTRADP                               
022700     PERFORM IMS-GU-WDB601                                                
022800     IF SEGMENT-FOUND                                                     
022900       MOVE DCS-IDLANDX2    TO   TEST-IDLANDX2                            
023000        IF LANDX2-EU-IDLANDX2                                             
023100          MOVE DCS-IDVAT     TO   UT-IDVAT                                
023200          MOVE DCS-IDLANDX2  TO   UT-IDLANDX2                             
023300          PERFORM B-PROCESS-VAT1                                          
023400        ELSE                                                              
023500          CONTINUE                                                        
023600        END-IF                                                            
023700     END-IF                                                               
023800       .                                                                  
023900       EJECT                                                              
024000 B-PROCESS-VAT1 SECTION.                                                  
024100                                                                          
024200     MOVE EKHT-DAREGDAT(3:6)   TO  WS-DATUM-YYMMDD                        
024300     MOVE WS-DATUM-YYMMDD      TO  UT-TIFAKT                              
024400     MOVE EKHT-IDARTNR         TO  UT-IDARTNR                             
024500     MOVE ZERO                 TO  UT-IDSTATNR                            
024600     MOVE CLAG-KDARTURS        TO  UT-KDARTURS                            
024700     MOVE 21                   TO  UT-KDINTTYP                            
024800     MOVE 'INT'                TO  UT-IDPTYP                              
024900     COMPUTE UT-KVLEVART = EKHT-KVANTAL * -1                              
025000     COMPUTE UT-SUFKTBEL = (EKHT-KVANTAL * EKHT-PRARTSTD) * -1            
025100     WRITE POST-REC  FROM UTRAD                                           
025200                                                                          
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400      .                                                                   
025500      EJECT                                                               
025600 B-SEARCH-IDDC-TAB2 SECTION.                                              
025700                                                                          
025800     MOVE EKHT-IDDC-REC      TO   W-IDDC-B6                               
025900     MOVE 'SEPV'             TO   W-KDTRADP                               
026000     PERFORM IMS-GU-WDB601                                                
026100     IF SEGMENT-FOUND                                                     
026200       MOVE DCS-IDLANDX2    TO   TEST-IDLANDX2                            
026300       IF LANDX2-EU-IDLANDX2                                              
026400         MOVE DCS-IDVAT     TO   UT-IDVAT                                 
026500         MOVE DCS-IDLANDX2  TO   UT-IDLANDX2                              
026600         PERFORM B-PROCESS-VAT2                                           
026700       ELSE                                                               
026800         CONTINUE                                                         
026900       END-IF                                                             
027000     END-IF                                                               
027100       .                                                                  
027200       EJECT                                                              
027300 B-PROCESS-VAT2 SECTION.                                                  
027400                                                                          
027500     MOVE EKHT-DAREGDAT(3:6)   TO  WS-DATUM-YYMMDD                        
027600     MOVE WS-DATUM-YYMMDD      TO  UT-TIFAKT                              
027700     MOVE EKHT-IDARTNR         TO  UT-IDARTNR                             
027800     MOVE ZERO                 TO  UT-IDSTATNR                            
027900     MOVE CLAG-KDARTURS        TO  UT-KDARTURS                            
028000     MOVE 11                   TO  UT-KDINTTYP                            
028100     MOVE 'INT'                TO  UT-IDPTYP                              
028200     COMPUTE UT-KVLEVART = EKHT-KVANTAL                                   
028300     COMPUTE UT-SUFKTBEL = (EKHT-KVANTAL * EKHT-PRARTSTD)                 
028400     WRITE POST-REC  FROM UTRAD                                           
028500     CALL POSTSUM USING POSTSUM-PARM                                      
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 IMS-GU-WDB601 SECTION.                                                   
029000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X                             
029100                    '&KDTRADP  =' W-KDTRADP ')'                           
029200          DELIMITED BY SIZE INTO SSA1                                     
029300          DISPLAY SSA1                                                    
029400     MOVE '  GE'           TO GOOD-STATUSCODES                            
029500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B6 SSA1                   
029600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
029700     PERFORM IMS-STATUSCHECK                                              
029800     .                                                                    
029900     SKIP3                                                                
030000 IMS-STATUSCHECK SECTION.                                                 
030100     SKIP2                                                                
030200     SET STATUS-IX TO 1                                                   
030300     SEARCH GOOD-STATUS                                                   
030400       AT END                                                             
030500         STRING ' STATUS CODE FROM IMS: ' STATUS-WS                       
030600           DELIMITED BY SIZE INTO ERROR-TEXT                              
030700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030800         CONTINUE                                                         
030900     END-SEARCH                                                           
031000     .                                                                    
