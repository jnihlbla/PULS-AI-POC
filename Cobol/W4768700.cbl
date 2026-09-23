000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4768700.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   24/06/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READS WDL5A TO CREATE A CLEANING FILE                            
001000*        CHECK AGAINST WDR5 FOR EXCEEDED POST                             
001100*                                                                         
001200*        THE PROGRAM READS     WDL5A                                      
001300*        THE PROGRAM READS     WDR5                                       
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*          ---                                                            
002300     SELECT W47687                     ASSIGN TO W47687D1.                
002400*          --- INVOICES TO BE DELETED IN WDL5                             
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W47687                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  POST -COPY W47687    -PRE OUT-     -L.                               
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W4768700'.            
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004300 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004400                                                                          
004500 77  WS-CURR-IDDISTR             PIC S9(5)   VALUE ZERO COMP-3.           
004600 77  WS-FOM-DATE                 PIC  9(6)   VALUE ZERO.                  
004700 77  WS-KVDAGAR-MAX              PIC  9(3)   VALUE ZERO.                  
004800 77  WS-KVDAGAR-DEF-INT          PIC  9(3)   VALUE ZERO.                  
004900 77  WS-KVDAGAR-DEF              PIC  9(3)   VALUE ZERO.                  
005000                                                                          
005100 01  ERRTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005300     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400     EJECT                                                                
005500                                                                          
005600 77  DATE-SW                     PIC X       VALUE 'N'.                   
005700     88  DATE-BELOW-LIMIT                    VALUE 'Y'.                   
005800                                                                          
005900     SKIP2                                                                
006000*    -COPY WWDIST47                                                       
006100     EJECT                                                                
006200*                                                                         
006300 01  GENERAL-SUBPROGRAM.                                                  
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007100                                                                          
007200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007300                                                                          
007400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007600                                                                          
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800*                                                                         
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100*                                                                         
008200*    --- PARAMETRAR TILL WDATKONV                                         
008300*01  -COPY WDATAREA                                                       
008400     EJECT                                                                
008500*                                                                         
008600*    --- PARAMETRAR TILL WDAGKONV                                         
008700*01  -COPY WDAGAREA                                                       
008800     EJECT                                                                
008900 01  OUT-AREA-START              PIC X(24)   VALUE                        
009000                                 'OUT-AREA-START   '.                     
009100*01  AREA -COPY W47687   -PRE OUT-                                        
009200     EJECT                                                                
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-WDGXKEY-4127-X.                                                
009700         05  W-IDHTYP-4127       PIC  X(4)   VALUE '4127'.                
009800         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
009900                                                                          
010000     03  W-KEY4128-X.                                                     
010100         05  W-4128-IDDISTR-FKY  PIC S9(5)        COMP-3.                 
010200         05  W-4128-IDDISTR-TKY  PIC S9(5)        COMP-3.                 
010300         05  W-4128-IDKUNDNR-FKY PIC S9(7)        COMP-3.                 
010400         05  W-4128-IDKUNDNR-TKY PIC S9(7)        COMP-3.                 
010500         05  W-4128-KDANMORS-KY  PIC  X(2).                               
010600                                                                          
010700     03  W-4128-IDDISTR-X.                                                
010800         05  W-4128-IDDISTR      PIC S9(5)        COMP-3.                 
010900                                                                          
011000     03  W-IDDISTR-DEF-X.                                                 
011100         05  W-IDDISTR-DEF       PIC S9(5) VALUE 9999 COMP-3.             
011200                                                                          
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FOUND                       VALUE '  '.                  
011600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011700     88  BASE-EMPTY                          VALUE 'GB'.                  
011800                                                                          
011900 01  GOOD-STATUSCODES.                                                    
012000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100                                                                          
012200 01  ALL-SSA.                                                             
012300     03 SSA1                     PIC X(96).                               
012400     03 SSA2                     PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNCTION CODES                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000 01 FILLER          PIC X(16) VALUE 'DLI-IO-WDL5A1'.                      
013100 01  DLI-IO-WDL5A1.                                                       
013200*    03  -COPY WDL5A1                                                     
013300     EJECT                                                                
013400 01 FILLER          PIC X(16) VALUE 'DLI-IO-WDR501'.                      
013500 01  DLI-IO-WDR501.                                                       
013600*    03  -COPY WDGX01                                                     
013700                                                                          
013800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4128'.                    
013900 01  DLI-IO-WDGX4128.                                                     
014000*    03  -COPY WDGX4128                                                   
014100                                                                          
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500     EJECT                                                                
014600*01  -COPY W0008  -PRE WDL5A-                                             
014700     05  FILLER                  PIC X.                                   
014800     EJECT                                                                
014900*01  -COPY W0008  -PRE WDR5-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING WDL5A-PCB WDR5-PCB.                            
015300     ENTRY 'DLITCBL' USING WDL5A-PCB WDR5-PCB.                            
015400                                                                          
015500                                                                          
015600     PERFORM A-INIT                                                       
015700     PERFORM IMS-GN-WDL5A                                                 
015710     MOVE ZERO TO OUT-IDFAKT                                              
015800                                                                          
015900     PERFORM UNTIL BASE-EMPTY OR SEGMENT-MISSING                          
015910       IF SEQA-IDFAKT NOT = OUT-IDFAKT                                    
016000         PERFORM B-CONTROL-DATE                                           
016100                                                                          
016200         IF DATE-BELOW-LIMIT                                              
016300           MOVE SEQA-IDFAKT   TO OUT-IDFAKT                               
016400           MOVE SEQA-TIFAKT   TO OUT-TIFAKT                               
016500           MOVE SEQA-IDDISTR  TO OUT-IDDISTR                              
016600           MOVE WS-KVDAGAR-MAX   TO OUT-KVDAGAR                           
016700                                                                          
016800           PERFORM S11-WRITE-W47687                                       
016900           MOVE NOO             TO  DATE-SW                               
017000         END-IF                                                           
017010       END-IF                                                             
017100                                                                          
017200       PERFORM IMS-GN-WDL5A                                               
017300     END-PERFORM                                                          
017400                                                                          
017500     PERFORM Z-FINIT                                                      
017600                                                                          
017700     MOVE ZERO                  TO RETURN-CODE                            
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT                         SECTION.                                  
018200     MOVE 'A-INIT          '    TO CURRENT-SECTION                        
018300                                                                          
018400     OPEN OUTPUT W47687                                                   
018500                                                                          
018600     MOVE 'IDAG  '              TO DAT-KDDATFORM                          
018700                                                                          
018800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018900                     DAT-O-TIDATUM DAT-KDSVAR                             
019000                                                                          
019100     IF DAT-KDSVAR-OK                                                     
019200       CONTINUE                                                           
019300     ELSE                                                                 
019400       MOVE 'ERROR IN CONVERTING DATE-TODAY' TO ERRTEXT-STR               
019500       DISPLAY ERRTEXT                                                    
019600       CALL ABEND            USING RKOD-ABEND-NO-DUMP                     
019700     END-IF                                                               
019800                                                                          
019900     PERFORM AA-BERAKNA-DEF-LEDTIDER                                      
020000     .                                                                    
020100     EJECT                                                                
020200 AA-BERAKNA-DEF-LEDTIDER   SECTION.                                       
020300     MOVE 'AA-BER-DEF-TID  '    TO CURRENT-SECTION                        
020400                                                                          
020500     MOVE 1                      TO W-4128-IDDISTR-FKY                    
020600     MOVE 99                     TO W-4128-IDDISTR-TKY                    
020700     MOVE ZERO                   TO W-4128-IDKUNDNR-FKY                   
020800     MOVE 999999                 TO W-4128-IDKUNDNR-TKY                   
020900     MOVE SPACE                  TO W-4128-KDANMORS-KY                    
021000     PERFORM IMS-GU-WDGX4128                                              
021100                                                                          
021200     IF SEGMENT-FOUND                                                     
021300        COMPUTE WS-KVDAGAR-DEF-INT = 4128-KVDAGAR-LTRP                    
021400                                   + 4128-KVDAGAR-LEVANM                  
021500     END-IF                                                               
021600                                                                          
021700     MOVE 9999                   TO W-4128-IDDISTR-TKY                    
021800     PERFORM IMS-GU-WDGX4128                                              
021900                                                                          
022000     IF SEGMENT-FOUND                                                     
022100        COMPUTE WS-KVDAGAR-DEF = 4128-KVDAGAR-LTRP                        
022200                               + 4128-KVDAGAR-LEVANM                      
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 B-CONTROL-DATE                 SECTION.                                  
022700     MOVE 'B-CONTROL-DATE  '   TO CURRENT-SECTION                         
022800                                                                          
022900     IF SEQA-IDDISTR NOT = WS-CURR-IDDISTR                                
023000        MOVE SEQA-IDDISTR   TO WS-CURR-IDDISTR                            
023100        PERFORM BA-HAMTA-FOM-DATUM                                        
023200     END-IF                                                               
023300*                                                                         
023400     IF SEQA-TIFAKT < WS-FOM-DATE                                         
023500        MOVE YES               TO DATE-SW                                 
023600      ELSE                                                                
023700        MOVE NOO               TO DATE-SW                                 
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 BA-HAMTA-FOM-DATUM             SECTION.                                  
024200     MOVE 'BA-HAMTA-FOM-DAT'    TO CURRENT-SECTION                        
024300     MOVE ZERO               TO WS-KVDAGAR-MAX                            
024400                                                                          
024500     PERFORM IMS-GU-WDR501                                                
024600     MOVE SEQA-IDDISTR       TO W-4128-IDDISTR                            
024700                                                                          
024800     PERFORM IMS-GNP-WDGX4128                                             
024900     IF SEGMENT-FOUND                                                     
025000       PERFORM UNTIL SEGMENT-MISSING                                      
025100         IF (4128-KVDAGAR-LTRP + 4128-KVDAGAR-LEVANM)                     
025200           > WS-KVDAGAR-MAX                                               
025300           COMPUTE WS-KVDAGAR-MAX =                                       
025400             4128-KVDAGAR-LTRP + 4128-KVDAGAR-LEVANM                      
025500         END-IF                                                           
025600                                                                          
025700         IF (4128-KVDAGAR-LTRP-LDC + 4128-KVDAGAR-LEVANM-LDC)             
025800           > WS-KVDAGAR-MAX                                               
025900           COMPUTE WS-KVDAGAR-MAX =                                       
026000             4128-KVDAGAR-LTRP-LDC + 4128-KVDAGAR-LEVANM-LDC              
026100         END-IF                                                           
026200                                                                          
026300         PERFORM IMS-GNP-WDGX4128                                         
026400       END-PERFORM                                                        
026500     ELSE                                                                 
026600       MOVE SEQA-IDDISTR            TO DIST47-IDDISTR                     
026700       IF DIST47-INTERNA                                                  
026800          MOVE WS-KVDAGAR-DEF-INT   TO WS-KVDAGAR-MAX                     
026900       ELSE                                                               
027000          MOVE WS-KVDAGAR-DEF       TO WS-KVDAGAR-MAX                     
027100       END-IF                                                             
027200     END-IF                                                               
027300                                                                          
027400     MOVE DAT-TIAAMMDD   TO DAG-TIAAMMDD-TOM                              
027500     MOVE WS-KVDAGAR-MAX TO DAG-KVKALDAG                                  
027600     MOVE 003            TO DAG-KDCALL                                    
027700                                                                          
027800     CALL WDAGKONV   USING  DAG-KDCALL                                    
027900                            DAG-DATUM-AREA                                
028000                            DAG-KDSVAR                                    
028100                                                                          
028200     IF DAG-KDSVAR = SPACE                                                
028300       MOVE DAG-TIAAMMDD-FOM    TO WS-FOM-DATE                            
028400     ELSE                                                                 
028500       MOVE 'ERROR IN CONVERTING DATE' TO ERRTEXT-STR                     
028600       DISPLAY ERRTEXT                                                    
028700       CALL ABEND            USING RKOD-ABEND-NO-DUMP                     
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 Z-FINIT                        SECTION.                                  
029200     CLOSE W47687                                                         
029300                                                                          
029400     MOVE 'S'                   TO POSTSUM-OPKOD                          
029500     CALL POSTSUM            USING POSTSUM-PARM                           
029600     .                                                                    
029700     EJECT                                                                
029800 S11-WRITE-W47687               SECTION.                                  
029900                                                                          
030000     WRITE OUT-POST           FROM OUT-AREA                               
030100                                                                          
030200     MOVE 'OUT'                 TO POSTSUM-TRANSTYP                       
030300     MOVE 'W47687'              TO POSTSUM-FDNAMN                         
030400     MOVE 'W47687D1'            TO POSTSUM-DDNAMN2                        
030500     CALL POSTSUM            USING POSTSUM-PARM                           
030600     .                                                                    
030700     EJECT                                                                
030800* --- IMS SECTIONS  ---                                                   
030900                                                                          
031000 IMS-GN-WDL5A   SECTION.                                                  
031100                                                                          
031200     MOVE 'WDL5A1 ' TO SSA1                                               
031300     CALL CBLTDLI USING GN WDL5A-PCB DLI-IO-WDL5A1 SSA1                   
031400     MOVE WDL5A-STATUS-CODE TO STATUS-WS                                  
031500     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
031600     PERFORM IMS-STATUSCHECK                                              
031700     .                                                                    
031800                                                                          
031900 IMS-GU-WDR501 SECTION.                                                   
032000     MOVE 'IMS-GU-WDR501   '  TO CURRENT-IMS-SECTION                      
032100                                                                          
032200     MOVE SPACE               TO ALL-SSA                                  
032300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
032400          DELIMITED BY SIZE INTO SSA1                                     
032500     MOVE '  '                TO GOOD-STATUSCODES                         
032600     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR501 SSA1                    
032700     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
032800     PERFORM IMS-STATUSCHECK                                              
032900     .                                                                    
033000     EJECT                                                                
033100 IMS-GU-WDGX4128 SECTION.                                                 
033200     MOVE 'IMS-GHU-WDGX4128'  TO CURRENT-IMS-SECTION                      
033300                                                                          
033400     MOVE SPACE               TO ALL-SSA                                  
033500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4127-X ')'                    
033600          DELIMITED BY SIZE INTO SSA1                                     
033700     STRING 'WDGX4128(KY4128   =' W-KEY4128-X ')'                         
033800          DELIMITED BY SIZE INTO SSA2                                     
033900     MOVE '  GE'              TO GOOD-STATUSCODES                         
034000     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX4128 SSA1 SSA2             
034100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
034200     PERFORM IMS-STATUSCHECK                                              
034300     .                                                                    
034400     EJECT                                                                
034500 IMS-GNP-WDGX4128          SECTION.                                       
034600     MOVE 'IMS-GNP-WDGX4128'  TO CURRENT-IMS-SECTION                      
034700                                                                          
034800     MOVE SPACE               TO ALL-SSA                                  
034900     STRING 'WDGX4128(IDDISTRF<=' W-4128-IDDISTR-X                        
035000                    '&IDDISTRT>=' W-4128-IDDISTR-X                        
035100                    '&IDDISTRTNE' W-IDDISTR-DEF-X  ')'                    
035200          DELIMITED BY SIZE INTO SSA1                                     
035300     MOVE '  GE'              TO GOOD-STATUSCODES                         
035400     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4128 SSA1                 
035500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
035600     PERFORM IMS-STATUSCHECK                                              
035700     .                                                                    
035800                                                                          
035900 IMS-STATUSCHECK                SECTION.                                  
036000                                                                          
036100     SET STATUS-IX TO 1                                                   
036200     SEARCH GOOD-STATUS                                                   
036300       AT END                                                             
036400         MOVE 'NOT OK STATUSCODE' TO ERRTEXT-STR                          
036500         DISPLAY ERRTEXT STATUS-WS                                        
036600         CALL FELLOG                                                      
036700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036800         CONTINUE                                                         
036900     END-SEARCH                                                           
037000     .                                                                    
037100     EJECT                                                                
