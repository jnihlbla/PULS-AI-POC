000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4794700.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/05/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM GETS THE GOODS RECEIVING COUNTRY FROM               
000900*        WDB201 FOR THE INPUT DISTRICT                                    
001000*                                                                         
001100*        THE PROGRAM READS     WDB2                                       
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
002500*          --- VALID DISTRICTS - INPUT                                    
002600     SELECT W47947IN                   ASSIGN TO W47947D1.                
002700     SKIP2                                                                
002800*          --- DISTRICTS WITH COUNTRY - OUTPUT                            
002900     SELECT W47947OT                   ASSIGN TO W47947D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W47947IN                                                             
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W47945      -L.                                                
004000     SKIP3                                                                
004100 FD  W47947OT                                                             
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W47947 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W4794700'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  WS-IDDC                     PIC X(2)    VALUE SPACES.                
005300 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACES.                
005400 77  WS-BELAND-TO                PIC X(35)   VALUE SPACES.                
005500 77  WS-BELAND-FROM              PIC X(35)   VALUE SPACES.                
005600 77  WS-IDDISTR                  PIC 9(5)    VALUE ZERO.                  
005700                                                                          
005800 77  W47945-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W47945                       VALUE 'J'.                   
006000 77  WS-PARTNR-FOUND-SW          PIC X       VALUE 'N'.                   
006100     88  PARTNR-FOUND                        VALUE 'J'.                   
006200     EJECT                                                                
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     SKIP2                                                                
007000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007100                                                                          
007200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  ERROR-TEXT.                                                          
007700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  IN-AREA-START               PIC X(24)   VALUE                        
008500                                 'IN-AREA-START  '.                       
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W47945     -PRE IN-                                       
008900     EJECT                                                                
009000 01  UT-AREA-START               PIC X(24)   VALUE                        
009100                                 'UT-AREA-START  '.                       
009200     SKIP2                                                                
009300                                                                          
009400*01  AREA -COPY W47947     -PRE UT-                                       
009500     EJECT                                                                
009600*    --- AREAS FOR IMS-SECTIONS                                           
009700*                                                                         
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  KEYS-FOR-DLI.                                                        
010200     03 W-IDGMT-MIN-X.                                                    
010300        05 W-IDDISTR-MIN         PIC S9(5)  COMP-3.                       
010400        05 W-IDKUNDNR-MIN        PIC S9(7)  COMP-3 VALUE ZEROS.           
010500     03 W-IDGMT-MAX-X.                                                    
010600        05 W-IDDISTR-MAX         PIC S9(5)  COMP-3.                       
010700        05 W-IDKUNDNR-MAX        PIC S9(7)  COMP-3                        
010800                                             VALUE 9999999.               
010900     03 W-IDDC-X.                                                         
011000        05 W-IDDC-B6             PIC X(2).                                
011100     03 W-WDB101KY-X.                                                     
011200        05  W-IDPARTNR           PIC X(9)    VALUE SPACE.                 
011300        05  W-IDFTG              PIC 9(2)    VALUE ZERO.                  
011400     03 W-WDB1B1KY-LOW.                                                   
011500        05  W-IDLANDX2-LOW       PIC X(2)    VALUE SPACE.                 
011600        05  W-IDMARKBO-LOW       PIC X(1)    VALUE SPACE.                 
011700        05  W-IDPARTNR-LOW       PIC X(9)    VALUE LOW-VALUE.             
011800        05  W-IDFTG-LOW          PIC 9(2)    VALUE ZERO.                  
011900                                                                          
012000     03 W-WDB1B1KY-HIGH.                                                  
012100        05  W-IDLANDX2-HIGH      PIC X(2)    VALUE SPACE.                 
012200        05  W-IDMARKBO-HIGH      PIC X(1)    VALUE SPACE.                 
012300        05  W-IDPARTNR-HIGH      PIC X(9)    VALUE HIGH-VALUE.            
012400        05  W-IDFTG-HIGH         PIC 9(2)    VALUE 99.                    
012500     SKIP2                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FOUND                       VALUE '  '.                  
012900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013100     88  END-OF-DATABASE                     VALUE 'GB'.                  
013200     SKIP2                                                                
013300 01  GOOD-STATUSCODES.                                                    
013400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNCTION CODES                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
014400 01  DLI-IO-WDB201.                                                       
014500*    03  -COPY WDB201                                                     
014600     EJECT                                                                
014700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
014800 01  DLI-IO-WDB601.                                                       
014900*    03  -COPY WDB601                                                     
015000     EJECT                                                                
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
015200 01  DLI-IO-WDB101.                                                       
015300*    03  -COPY WDB101                                                     
015400     EJECT                                                                
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB1B1'.                      
015600 01  DLI-IO-WDB1B1.                                                       
015700*    03  -COPY WDB1B1                                                     
015800     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000                                                                          
016100                                                                          
016200*01  -COPY W0008  -PRE WDB2-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE WDB6-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE WDB1-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE WDB1B-                                             
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400 PROCEDURE DIVISION  USING WDB2-PCB WDB6-PCB WDB1-PCB WDB1B-PCB.          
017500 MAIN SECTION.                                                            
017600     ENTRY 'DLITCBL' USING WDB2-PCB WDB6-PCB WDB1-PCB WDB1B-PCB.          
017700                                                                          
017800     PERFORM A-INIT                                                       
017900                                                                          
018000     PERFORM S01-READ-W47945                                              
018100     PERFORM UNTIL END-OF-W47945                                          
018200       PERFORM B-GET-FROM-CTRY-NAME                                       
018300       PERFORM C-GET-TO-CTRY-NAME                                         
018400       PERFORM S11-WRITE-W47947                                           
018500       PERFORM S01-READ-W47945                                            
018600     END-PERFORM                                                          
018700                                                                          
018800     PERFORM Z-FINIT                                                      
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400 A-INIT SECTION.                                                          
019500                                                                          
019600     OPEN INPUT  W47947IN                                                 
019700                                                                          
019800     OPEN OUTPUT W47947OT                                                 
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     EJECT                                                                
020300 B-GET-FROM-CTRY-NAME SECTION.                                            
020400                                                                          
020430                                                                          
020500     IF IN-IDLANDX2 = WS-IDLANDX2                                         
020600     OR IN-IDDC = WS-IDDC                                                 
020700        MOVE WS-BELAND-FROM       TO UT-ADGMT-LAND-FROM                   
020800     ELSE                                                                 
020900        IF IN-IDLANDX2 = SPACES                                           
021000* IDLANDX2 IS BLANK IN THE INPUT FOR 'TRANSFER' DISTRICT TYPE.            
021100* GET IDLANDX2 FROM DB6 USING THE IDDC. THEN GET THE COUNTRY NAME         
021200                                                                          
021300           MOVE IN-IDDC           TO W-IDDC-B6                            
021310           PERFORM IMS-GU-WDB601                                          
021400           IF SEGMENT-FOUND                                               
021500              MOVE DCS-IDLANDX2   TO W-IDLANDX2-LOW                       
021600                                     W-IDLANDX2-HIGH                      
021700                                     WS-IDLANDX2                          
021800              PERFORM BA-GET-BELAND                                       
021900           END-IF                                                         
022000        ELSE                                                              
022100           MOVE IN-IDLANDX2       TO W-IDLANDX2-LOW                       
022200                                     W-IDLANDX2-HIGH                      
022300                                     WS-IDLANDX2                          
022400           PERFORM BA-GET-BELAND                                          
022500        END-IF                                                            
022600        MOVE IN-IDDC              TO WS-IDDC                              
022700     END-IF                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 BA-GET-BELAND SECTION.                                                   
023100                                                                          
023200* USING SEC.INDEX ON IDLANDX2, GET THE PARTNER                            
023300     PERFORM IMS-GU-WDB1B1                                                
023400     IF SEGMENT-FOUND                                                     
023500       MOVE SEQB-IDPARTNR           TO W-IDPARTNR                         
023600       MOVE SEQB-IDFTG              TO W-IDFTG                            
023700* USING PARTNER, GET THE COUNTRY NAME                                     
023800       PERFORM IMS-GU-WDB101                                              
023900       IF SEGMENT-FOUND                                                   
024000         MOVE BET-BELAND-SVE        TO UT-ADGMT-LAND-FROM                 
024100                                       WS-BELAND-FROM                     
024200       END-IF                                                             
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 C-GET-TO-CTRY-NAME SECTION.                                              
024700                                                                          
024800* INPUT IS SORTED ON DISTRICT. IF CURRENT DISTRICT IS SAME                
024900* AS PREVIOUS DISTRICT, JUST MOVE THE PREVIOUS CTRY NAME DIRECTLY         
025000     IF IN-IDDISTR = WS-IDDISTR                                           
025100       MOVE WS-BELAND-TO           TO UT-ADGMT-LAND-TO                    
025200     ELSE                                                                 
025300       MOVE IN-IDDISTR             TO W-IDDISTR-MIN                       
025400                                      W-IDDISTR-MAX                       
025500                                      WS-IDDISTR                          
025600       PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE                   
025700               OR PARTNR-FOUND                                            
025800* GET PARTNER FOR INPUT DISTRICT                                          
025900         PERFORM IMS-GN-WDB201                                            
026000         IF SEGMENT-FOUND                                                 
026100            IF GMT-IDPARTNR > SPACES                                      
026200               MOVE GMT-IDPARTNR   TO W-IDPARTNR                          
026300               MOVE GMT-IDFTG      TO W-IDFTG                             
026400               MOVE YES            TO WS-PARTNR-FOUND-SW                  
026500            END-IF                                                        
026600         END-IF                                                           
026700       END-PERFORM                                                        
026800                                                                          
026900       IF PARTNR-FOUND                                                    
027000* USING THE PARTNER, GET THE COUNTRY NAME                                 
027100         PERFORM IMS-GU-WDB101                                            
027200         IF SEGMENT-FOUND                                                 
027300            MOVE BET-BELAND-SVE    TO UT-ADGMT-LAND-TO                    
027400                                      WS-BELAND-TO                        
027500         END-IF                                                           
027600         MOVE NOO                  TO WS-PARTNR-FOUND-SW                  
027700       END-IF                                                             
027800     END-IF                                                               
027900                                                                          
028000     MOVE IN-IDDC                  TO UT-IDDC                             
028100     MOVE IN-IDDISTR               TO UT-IDDISTR                          
028200     MOVE IN-DISTR-TYP             TO UT-DISTR-TYP                        
028300                                                                          
028400     .                                                                    
028500     EJECT                                                                
028600 Z-FINIT SECTION.                                                         
028700     CLOSE W47947IN                                                       
028800           W47947OT                                                       
028900     SKIP2                                                                
029000     MOVE 'S' TO POSTSUM-OPKOD                                            
029100     CALL POSTSUM USING POSTSUM-PARM                                      
029200     .                                                                    
029300     EJECT                                                                
029400 S01-READ-W47945  SECTION.                                                
029500     READ W47947IN INTO IN-AREA                                           
029600     AT END                                                               
029700        MOVE HIGH-VALUE TO IN-AREA                                        
029800        SET END-OF-W47945 TO TRUE                                         
029900                                                                          
030000     NOT AT END                                                           
030100        MOVE 'W47945' TO POSTSUM-FDNAMN                                   
030200        MOVE 'W47947D1' TO POSTSUM-DDNAMN2                                
030300        CALL POSTSUM USING POSTSUM-PARM                                   
030400     END-READ                                                             
030500     .                                                                    
030600     EJECT                                                                
030700 S11-WRITE-W47947 SECTION.                                                
030800                                                                          
030900     WRITE UT-POST   FROM UT-AREA                                         
031000                                                                          
031100     MOVE 'W47947' TO POSTSUM-FDNAMN                                      
031200     MOVE 'W47947D2' TO POSTSUM-DDNAMN2                                   
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
031500     EJECT                                                                
031600 S99-ABEND SECTION.                                                       
031700                                                                          
031800     SKIP2                                                                
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     CALL ABEND USING RKOD-ABEND                                          
032200     .                                                                    
032300     EJECT                                                                
032400* --- IMS SECTIONS  ---                                                   
032500                                                                          
032600     EJECT                                                                
032700 IMS-GN-WDB201 SECTION.                                                   
032800                                                                          
032900     MOVE SPACES           TO SSA1                                        
033000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
033100                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
033200          DELIMITED BY SIZE INTO SSA1                                     
033300     MOVE '  GE' TO GOOD-STATUSCODES                                      
033400     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
033500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
033600     PERFORM IMS-STATUSCHECK                                              
033700     .                                                                    
033800     EJECT                                                                
033900 IMS-GU-WDB601 SECTION.                                                   
034000                                                                          
034100     MOVE SPACES           TO SSA1                                        
034200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
034300          DELIMITED BY SIZE INTO SSA1                                     
034400     MOVE '  GE' TO GOOD-STATUSCODES                                      
034500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
034600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
034700     PERFORM IMS-STATUSCHECK                                              
034800     .                                                                    
034900     EJECT                                                                
035000 IMS-GU-WDB101 SECTION.                                                   
035100                                                                          
035200     MOVE SPACES           TO SSA1                                        
035300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
035400          DELIMITED BY SIZE INTO SSA1                                     
035500     MOVE '  GE' TO GOOD-STATUSCODES                                      
035600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
035700     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
035800     PERFORM IMS-STATUSCHECK                                              
035900     .                                                                    
036000 IMS-GU-WDB1B1 SECTION.                                                   
036100                                                                          
036200     MOVE SPACES           TO SSA1                                        
036300     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
036400                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
036500          DELIMITED BY SIZE INTO SSA1                                     
036600     MOVE '  GE' TO GOOD-STATUSCODES                                      
036700     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
036800     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
036900     PERFORM IMS-STATUSCHECK                                              
037000     .                                                                    
037100 IMS-STATUSCHECK SECTION.                                                 
037200                                                                          
037300     SET STATUS-IX TO 1                                                   
037400     SEARCH GOOD-STATUS                                                   
037500       AT END                                                             
037600         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
037700           DELIMITED BY SIZE INTO ERROR-TEXT                              
037800         DISPLAY ERROR-TEXT                                               
037900         CALL FELLOG                                                      
038000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
038100         CONTINUE                                                         
038200     END-SEARCH                                                           
038300     .                                                                    
