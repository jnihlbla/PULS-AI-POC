000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W5136800.                                                 
000300 AUTHOR.        KARL-JOHAN HANSSON                                        
000400 DATE-WRITTEN.  SEPT 1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*     -  PROGRAMMET LÄSER WDH7  MED SB.                                   
001000*     -  SKAPAR EN FIL MED SAMTLIGA JUST. UNDER ÅRET FÖR                  
001100*         CDC, SDC, LDC OCH PACIFIC                                       
001200*     -  SKAPAR EN FIL FÖR CDC POSTER ETRACKER:6584085                    
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000     SELECT W51369                       ASSIGN TO UT-S-W51368D1.         
002100     EJECT                                                                
002200     SELECT W51368                       ASSIGN TO UT-S-W51368D2.         
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W51369                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS 0.                                                    
003100                                                                          
003200*01  UTPOST-69 -COPY W51369     -L.                                       
003300     EJECT                                                                
003400                                                                          
003500 FD  W51368                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*01  UTPOST-68 -COPY W51368     -L.                                       
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -COPY WY2000W1                                                       
004400                                                                          
004500 77  IDPGM                       PIC X(8) VALUE 'W5136800'.               
004600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
004700                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
005400                                                                          
005500 01  AAR-START                   PIC 9(7).                                
005600 01  PER-SLUT                    PIC 9(7).                                
005700     EJECT                                                                
005800*   --- VALID IDDC CODES                                                  
005900*01  -COPY WWDC99                                                         
006000     EJECT                                                                
006100*01  -COPY W0005      -PRE POSTSUM-                                       
006200 01  UT-TRANSID-69.                                                       
006300     03  FILLER                  PIC X(6)  VALUE 'W51369'.                
006400     03  FILLER                  PIC X(8)  VALUE 'W51368D1'.              
006500     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
006600     SKIP3                                                                
006700 01  UT-TRANSID-68.                                                       
006800     03  FILLER                  PIC X(6)  VALUE 'W51368'.                
006900     03  FILLER                  PIC X(8)  VALUE 'W51368D1'.              
007000     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
007100     SKIP3                                                                
007200 01  DATUMKORT-ID                PIC X(6)  VALUE 'WDATUM'.                
007300*01  -COPY WDATKORT                                                       
007400     EJECT                                                                
007500 01  FILLER                      PIC X(8)    VALUE 'UT-AREOR'.            
007600                                                                          
007700*01  POST -COPY W51369   -PRE UT-                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(8)    VALUE 'UT2-AREA'.            
008000                                                                          
008100*01  POST -COPY W51368   -PRE UT2-                                        
008200     EJECT                                                                
008300 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
008400                                                                          
008500 01  IMS-WS.                                                              
008600                                                                          
008700     03  STATUS-WS               PIC X(2).                                
008800        88  SEGMENT-FINNS                    VALUE '  '.                  
008900        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
008910        88  SEGMENT-SLUT                     VALUE 'GB'.                  
009000                                                                          
009100     03  GODK-STATUSKODER.                                                
009200         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
009300                                                                          
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009600 01  DLI-IO-AREA.                                                         
009700     03  IO-AREA              PIC X(150).                                 
009800     SKIP3                                                                
009900*    03  -COPY WDH701             -RED IO-AREA                            
010000     EJECT                                                                
010100*    03  -COPY WDH711             -RED IO-AREA                            
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400     SKIP3                                                                
010500*01  -COPY W0008   -PRE WDH7-                                             
010600         05  FILLER           PIC X.                                      
010700     EJECT                                                                
010800 PROCEDURE DIVISION USING  WDH7-PCB.                                      
010900     ENTRY 'DLITCBL' USING WDH7-PCB.                                      
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM IMS-GET-WDH7                                                 
011400                                                                          
011500     PERFORM UNTIL SEGMENT-SLUT                                           
011600                                                                          
011700         EVALUATE WDH7-SEG-NAME-FB                                        
011800                                                                          
011900            WHEN 'WDH701'                                                 
012000                    MOVE INVA-IDARTNR TO UT-IDARTNR                       
012100                    MOVE INVA-IDARTNR TO UT2-IDARTNR                      
012200                                                                          
012300            WHEN 'WDH711'  PERFORM B-SKAPA-SKRIV-POST                     
012400                                                                          
012500         END-EVALUATE                                                     
012600                                                                          
012700         PERFORM IMS-GET-WDH7                                             
012800     END-PERFORM                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100     MOVE ZERO TO RETURN-CODE                                             
013200                                                                          
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700                                                                          
013800     OPEN OUTPUT W51369                                                   
013900     OPEN OUTPUT W51368                                                   
014000                                                                          
014100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200                                                                          
014300     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
014400                                                                          
014500     MOVE ZERO              TO PER-SLUT                                   
014600     MOVE D-AAR             TO PER-SLUT(2:2)                              
014700     MOVE D-MAANAD          TO PER-SLUT(4:2)                              
014800     MOVE D-DAG             TO PER-SLUT(6:2)                              
014900                                                                          
015000     MOVE ZERO              TO AAR-START                                  
015100     MOVE D-AAR             TO AAR-START(2:2)                             
015200     MOVE 01                TO AAR-START(6:2)                             
015300                                                                          
015400     DISPLAY 'ÅRETS BÖRJAN,    AAMMDD: ' AAR-START                        
015500     DISPLAY 'PERIODENS SLUT   AAMMDD: ' PER-SLUT                         
015600     .                                                                    
015700     EJECT                                                                
015800 B-SKAPA-SKRIV-POST SECTION.                                              
015900                                                                          
016000     MOVE INVH-IDDC              TO WS-IDDC                               
016100                                                                          
016200     MOVE INVH-DAREGDAT-CLO(3:6) TO TMP1-YYMMDD                           
016300     MOVE AAR-START              TO TMP2-YYMMDD                           
016400     MOVE PER-SLUT               TO TMP3-YYMMDD                           
016500     PERFORM WY2000Q1                                                     
016600     IF TMP1-YYMMDD >= TMP2-YYMMDD AND                                    
016700        TMP1-YYMMDD <= TMP3-YYMMDD                                        
016800                                                                          
017100        MOVE INVH-IDDC          TO UT-IDDC                                
017200        MOVE INVH-KVJUSTKV      TO UT-KVJUSTKV                            
017300        MOVE INVH-KDJUSTYP      TO UT-KDJUSTYP                            
017400        MOVE INVH-PRARTSTD      TO UT-PRARTSTD                            
017500        MOVE INVH-FLAUTLSJ      TO UT-FLAUTLSJ                            
017600                                                                          
017700        PERFORM S02-SKRIV-W51369                                          
017800        IF CDC-SE                                                         
017900          MOVE INVH-IDDC          TO UT2-IDDC                             
018000          MOVE INVH-KVJUSTKV      TO UT2-KVJUSTKV                         
018100          MOVE INVH-KDJUSTYP      TO UT2-KDJUSTYP                         
018200          MOVE INVH-PRARTSTD      TO UT2-PRARTSTD                         
018300          MOVE INVH-FLAUTLSJ      TO UT2-FLAUTLSJ                         
018400          MOVE INVH-KVANTAL       TO UT2-KVANTAL                          
018500          PERFORM S02-SKRIV-W51368                                        
018600        END-IF                                                            
018800     END-IF                                                               
018900     .                                                                    
019000     SKIP3                                                                
019100 Z-FINIT  SECTION.                                                        
019200                                                                          
019300     CLOSE W51369                                                         
019400     CLOSE W51368                                                         
019500                                                                          
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     SKIP3                                                                
020000 S02-SKRIV-W51369     SECTION.                                            
020100                                                                          
020200     WRITE UTPOST-69 FROM UT-POST                                         
020300                                                                          
020400     MOVE UT-TRANSID-69   TO POSTSUM-TRANSID                              
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 S02-SKRIV-W51368     SECTION.                                            
020900                                                                          
021000     WRITE UTPOST-68 FROM UT2-POST                                        
021100                                                                          
021200     MOVE UT-TRANSID-68   TO POSTSUM-TRANSID                              
021300     CALL POSTSUM USING POSTSUM-PARM                                      
021400     .                                                                    
021500     EJECT                                                                
021600*         * I M S  S E C T I O N                                          
021700                                                                          
021800 IMS-GET-WDH7         SECTION.                                            
021900                                                                          
022000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022100     CALL CBLTDLI USING GN WDH7-PCB IO-AREA                               
022200     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     SKIP3                                                                
022600 IMS-STATUSKONTROLL   SECTION.                                            
022700                                                                          
022800     SET STATUS-IX TO 1                                                   
022900     SEARCH GODK-STATUS                                                   
023000          AT END CALL FELLOG                                              
023100          WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE               
023200     END-SEARCH                                                           
023300     .                                                                    
023400     EJECT                                                                
023500*    -COPY WY2000Q1                                                       
