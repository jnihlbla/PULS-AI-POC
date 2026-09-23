000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4122000.                                                
000300 AUTHOR.         PRIYASOPHIA GALBAO.                                      
000400 DATE-WRITTEN.   21/03/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ON DAILY BASIS SCAN THE SYSTEM FOR ORDERS WITH                   
000900*        REPAIR-DATE, DISTRICT 778, DC 11, TODAY'S RFS DATE               
001000*        AND THAT ARE IN 'S' STATUS.                                      
001100*                                                                         
001200*                                                                         
001300*        THE PROGRAM READS     WDE6A                                      
001300*                              WDE6                                       
001400*                              WDE4                                       
001500*                              WDQ2                                       
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800*          --- OUTFILE WITH 'S' STATUS ORDERS FOR TODAY'S RFS             
002900     SELECT W4122A                     ASSIGN TO W41220D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400 FD  W4122A                                                               
003500     LABEL RECORDS STANDARD                                               
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800*  01 W4122A-POST  -COPY W4122A -L                                        
003900                                                                          
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)   VALUE 'W4122000'.             
004400 77  CURRENT-SECTION             PIC X(25)  VALUE SPACE.                  
004500 77  CURRENT-IMS-SECTION         PIC X(25)  VALUE SPACE.                  
004500*77  INDX                        PIC S9(4)  VALUE 0 COMP-3.               
004500 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004800 77  W-IDDC                      PIC X(2)   VALUE '11'.                   
004800 77  WS-SAVE-SEQA-IDPRODNR       PIC S9(7)  COMP-3 VALUE 0.               
004800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP   VALUE +16.             
004800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP   VALUE +1000.           
004900     EJECT                                                                
005000 01  WS-READ-SW                  PIC X(1)   VALUE 'N'.                    
005100     88 FIRST-TIME-IDPRODNR-READ            VALUE 'J'.                    
005000*01  WS-WRITE-SW                 PIC X(1)   VALUE 'J'.                    
005100*    88 WRITE-S-ORDER-NO                    VALUE 'N'.                    
005200     EJECT                                                                
005200                                                                          
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI'.              
006100     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
006200     03  FELLOG                  PIC X(8)   VALUE 'FELLOG'.               
006200     03  WORKDAY                 PIC X(8)   VALUE 'WORKDAY '.             
006300     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
006400     SKIP2                                                                
006500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006600                                                                          
006700     SKIP2                                                                
006800     EJECT                                                                
006900 01  ERRTEXT.                                                             
007000     03  FILLER                  PIC X(8)   VALUE 'ERRTEXT'.              
007100     03  ERRTEXT-STR             PIC X(72)  VALUE SPACE.                  
007200     EJECT                                                                
007300*    --- PARAMETERS FOR SUBPROGRAM WORKDAY                                
007400*                                                                         
007500*01 -COPY WORKAREA                                                        
007200     EJECT                                                                
007300*    --- PARAMETERS FOR POSTSUM                                           
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  UT-AREA-START               PIC X(24)  VALUE                         
007800                                 'UT-AREA-START  '.                       
007900                                                                          
008000*01  AREA -COPY W4122A     -PRE UT-                                       
008100     EJECT                                                                
008200*    --- AREAS FOR IMS-SECTIONS                                           
008300*                                                                         
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
008600     SKIP3                                                                
008700 01  KEYS-FOR-DLI.                                                        
008800*    WDE6A1                                                               
008900     03  W-WDE6A1KY-MIN-X.                                                
009100       05  W-IDDISTR-MIN         PIC S9(5)  VALUE 778     COMP-3.         
009300       05  W-IDDC-MIN            PIC X(02)  VALUE '11'.                   
009500       05  FILLER                PIC X(11)  VALUE LOW-VALUE.              
008800                                                                          
008900     03  W-WDE6A1KY-MAX-X.                                                
009100       05  W-IDDISTR-MAX         PIC S9(5)  VALUE 778     COMP-3.         
009300       05  W-IDDC-MAX            PIC X(02)  VALUE '11'.                   
009500       05  FILLER                PIC X(11)  VALUE HIGH-VALUE.             
008900                                                                          
009000     03  W-KDFRAKT-X.                                                     
009000         05 W-KDFRAKT            PIC S9(3)  VALUE +88     COMP-3.         
009100                                                                          
009000     03  W-KDORDSTA-X.                                                    
009000         05 W-KDORDSTA           PIC S9     VALUE +5      COMP-3.         
009100                                                                          
009100*    WDE601                                                               
009100   03  W-WDE601-IDPRODNR-X.                                               
009100     05  W-VORD-IDPRODNR         PIC S9(7)  VALUE ZERO    COMP-3.         
009100                                                                          
009100*    WDE601                                                               
009100   03  W-WDE611-KDKOLSTA-X.                                               
009100     05  W-KOLLI-KDKOLSTA        PIC S9     VALUE 7       COMP-3.         
009100                                                                          
009900*    WDE4F                                                                
010000     03  W-WDE4FSEQ-X.                                                    
010100         05  W-E4F-IDPRODNR      PIC S9(7)    COMP-3.                     
010200         05  W-E4F-IDKOLLI       PIC S9(5)    COMP-3.                     
010300*    WDQ2                                                                 
010400     03  W-IDORDER-X.                                                     
010500         05  W-IDORDER-Q201      PIC S9(7) COMP-3.                        
010600     SKIP2                                                                
010700*    --- STATUS-KOD FRÅN IMS                                              
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FOUND                       VALUE '  '.                  
011000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
011200     SKIP2                                                                
011300 01 GOOD-STATUSCODES.                                                     
011400    03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
011500     SKIP3                                                                
011600 01  ALL-SSA.                                                             
011700     03  SSA1                   PIC X(400).                               
011900     EJECT                                                                
012000*    --- IMS FUNCTION CODES                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6A1'.                        
012500 01  DLI-IO-E6A1.                                                         
012600*  03  -COPY WDE6A1                                                       
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
012500 01  DLI-IO-E601.                                                         
012600*  03  -COPY WDE601                                                       
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
012500 01  DLI-IO-E611.                                                         
012600*  03  -COPY WDE611                                                       
012700 01  FILLER         PIC X(24) VALUE 'DLI-IO-E401'.                        
012800 01  DLI-IO-E401.                                                         
012900*  03  -COPY WDE401                                                       
013000 01  FILLER         PIC X(24) VALUE 'DLI-IO-E411'.                        
013100 01  DLI-IO-E411.                                                         
013200*  03  -COPY WDE411                                                       
013300 01  FILLER         PIC X(24) VALUE 'DLI-IO-E421'.                        
013400 01  DLI-IO-E421.                                                         
013500*  03  -COPY WDE421                                                       
013600 01  FILLER         PIC X(24) VALUE 'DLI-IO-Q201'.                        
013700 01  DLI-IO-Q201.                                                         
013800*  03  -COPY WDQ201                                                       
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014300*01  -COPY W0008  -PRE WDE6A-                                             
014400     05  FILLER                  PIC X.                                   
014300*01  -COPY W0008  -PRE WDE6-                                              
014400     05  FILLER                  PIC X.                                   
014500*01  -COPY W0008  -PRE WDE4-                                              
014600     05  FILLER                  PIC X.                                   
014700*01  -COPY W0008  -PRE WDQ2-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING WDE6A-PCB WDE6-PCB WDE4-PCB WDQ2-PCB.          
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING WDE6A-PCB WDE6-PCB WDE4-PCB WDQ2-PCB.          
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
020200     PERFORM IMS-GN-WDE6A                                                 
015600     PERFORM UNTIL SEGMENT-NOMORE OR SEGMENT-MISSING                      
015600       PERFORM B-PROCESS-STAT-S-ORDERS                                    
020200       PERFORM IMS-GN-WDE6A                                               
015700     END-PERFORM                                                          
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
016800                                                                          
016900     OPEN OUTPUT W4122A                                                   
017000                                                                          
017300     MOVE IDPGM                       TO POSTSUM-PROGNAMN                 
017400     .                                                                    
017500     EJECT                                                                
017600 B-PROCESS-STAT-S-ORDERS SECTION.                                         
017700     MOVE 'B-PROCESS-STAT-S-ORDERS  ' TO CURRENT-SECTION                  
017800                                                                          
018000     MOVE SEQA-IDPRODNR               TO W-VORD-IDPRODNR                  
018000     PERFORM IMS-GU-WDE601                                                
018000     IF VORD-KDORDKL = 3 AND                                              
018000        VORD-DARFS(1:8) = FUNCTION CURRENT-DATE(1:8)                      
018000        PERFORM IMS-GNP-WDE611                                            
018000        PERFORM UNTIL SEGMENT-MISSING                                     
018000          PERFORM BA-PROCESS-KOLLI                                        
018000          PERFORM IMS-GNP-WDE611                                          
020200        END-PERFORM                                                       
020200     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
017600 BA-PROCESS-KOLLI SECTION.                                                
017700     MOVE 'BA-PROCESS-KOLLI         ' TO CURRENT-SECTION                  
017800                                                                          
017800     MOVE SEQA-IDPRODNR               TO W-E4F-IDPRODNR                   
017800     MOVE KOLLI-IDKOLLI               TO W-E4F-IDKOLLI                    
017800     IF SEQA-IDPRODNR NOT = WS-SAVE-SEQA-IDPRODNR                         
017800        SET FIRST-TIME-IDPRODNR-READ  TO TRUE                             
017800     END-IF                                                               
017800     PERFORM IMS-GU-WDE411-FSEQ                                           
017800     PERFORM UNTIL SEGMENT-NOMORE OR SEGMENT-MISSING                      
017800       IF FIRST-TIME-IDPRODNR-READ                                        
017800          PERFORM IMS-GNP-WDE401                                          
017800          MOVE KORD-IDORDER           TO W-IDORDER-Q201                   
017800          PERFORM IMS-GU-WDQ201                                           
017800          MOVE 'N'                    TO WS-READ-SW                       
017800       END-IF                                                             
017800       IF OHUV-TIREPDAT > 0                                               
017800          PERFORM BAA-VALIDATE-TIREPDAT                                   
017800          IF OHUV-TIREPDAT = WORK-TIAAMMDD-NEXT-WORKDAY                   
017800*            PERFORM BAA-VALIDATE-CLEARING-DC                             
017800*            IF WS-WRITE-SW = 'J'                                         
017800                PERFORM IMS-GNP-WDE421                                    
017800                PERFORM BAB-CREATE-W4122A                                 
017800                PERFORM S01-WRITE-W4122A                                  
017800                PERFORM IMS-GN-WDE411-FSEQ                                
017800*            END-IF                                                       
017800*            MOVE 'J'                 TO WS-WRITE-SW                      
017800          ELSE                                                            
017800            SET SEGMENT-MISSING       TO TRUE                             
017800          END-IF                                                          
017800       ELSE                                                               
017800         SET SEGMENT-MISSING          TO TRUE                             
017800       END-IF                                                             
017800     END-PERFORM                                                          
017800     MOVE SEQA-IDPRODNR               TO WS-SAVE-SEQA-IDPRODNR            
021300     .                                                                    
017800     EJECT                                                                
017600*BAA-VALIDATE-CLEARING-DC SECTION.                                        
017700*    MOVE 'BAA-VALIDATE-CLEARING-DC ' TO CURRENT-SECTION                  
017800*                                                                         
017800*    PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 7                      
017800*      IF OHUV-IDDC-CLEAR(INDX) = '1C'                                    
017800*         SET WRITE-S-ORDER-NO        TO TRUE                             
017800*      END-IF                                                             
017800*    END-PERFORM                                                          
021300*    .                                                                    
017800     EJECT                                                                
020600 Z-FINIT SECTION.                                                         
020700     MOVE 'Z-FINIT                  ' TO CURRENT-SECTION                  
020800                                                                          
020900     CLOSE W4122A                                                         
021000                                                                          
021100     MOVE 'S'                         TO POSTSUM-OPKOD                    
021200     CALL POSTSUM                  USING POSTSUM-PARM                     
021300     .                                                                    
021400     EJECT                                                                
021400 BAA-VALIDATE-TIREPDAT SECTION.                                           
021400     MOVE 'BAA-VALIDATE-TIREPDAT    ' TO CURRENT-SECTION                  
021400                                                                          
021400     MOVE W-IDDC                      TO WORK-IDDC                        
021400     MOVE +002                        TO WORK-KDCALL                      
021400     MOVE +001                        TO WORK-KVWORKD                     
021400     MOVE VORD-DARFS(1:8)             TO WORK-TIAAMMDD-FOM                
021400     CALL WORKDAY                  USING WORK-KDCALL                      
021400                                         WORK-DATE-AREA                   
021400                                         WORK-KDSVAR                      
021400     IF WORK-KDSVAR-FEL                                                   
021400        MOVE 'SECT S01-1, DATUM SAKNAS I WORKDAY'                         
021400                                      TO FELTEXT                          
021400        CALL ABEND                 USING RKOD-ABEND-NO-DUMP               
021400     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
021500 BAB-CREATE-W4122A SECTION.                                               
021400     MOVE 'BAB-VALIDATE-TIREPDAT    ' TO CURRENT-SECTION                  
021600                                                                          
021700     MOVE SEQA-IDDISTR                TO UT-IDDISTR                       
021800     MOVE SEQA-IDKUNDNR               TO UT-IDKUNDNR                      
021900     MOVE KORD-IDORDNR5               TO UT-IDORDNR                       
022000     MOVE ORAD-IDARTNR                TO UT-IDARTNR                       
022100     MOVE KKOLLI-KVLEVART             TO UT-KVLEVART                      
022100     MOVE OHUV-TIREPDAT               TO UT-TIREPDAT                      
022200     MOVE ORAD-BERADREF               TO UT-BERADREF                      
022300     .                                                                    
022400     EJECT                                                                
022500 S01-WRITE-W4122A SECTION.                                                
021400     MOVE 'S01-WRITE-W4122A         ' TO CURRENT-SECTION                  
022600                                                                          
022700     WRITE W4122A-POST              FROM UT-AREA                          
022800                                                                          
022900     MOVE 'UT'                        TO POSTSUM-TRANSTYP                 
023000     MOVE 'W4122A'                    TO POSTSUM-FDNAMN                   
023100     MOVE 'W41220D1'                  TO POSTSUM-DDNAMN2                  
023200     CALL POSTSUM                  USING POSTSUM-PARM                     
023300     .                                                                    
023400     EJECT                                                                
023500* --- IMS SECTIONS  ---                                                   
023600                                                                          
023700     EJECT                                                                
023800 IMS-GN-WDE6A SECTION.                                                    
023900     MOVE 'IMS-GN-WDE6A        '      TO CURRENT-IMS-SECTION              
024000                                                                          
024100     STRING 'WDE6A1  (WDE6A1KY>='  W-WDE6A1KY-MIN-X                       
024200                    '&WDE6A1KY<='  W-WDE6A1KY-MAX-X                       
024400                    '&KDFRAKT NE'  W-KDFRAKT-X                            
024300                    '&KDORDSTALT'  W-KDORDSTA-X ')'                       
024500            DELIMITED BY SIZE INTO SSA1                                   
024600     MOVE '  GEGB'                    TO GOOD-STATUSCODES                 
024700     CALL CBLTDLI USING GN WDE6A-PCB DLI-IO-E6A1 SSA1                     
024800     MOVE WDE6A-STATUS-CODE           TO STATUS-WS                        
024900     PERFORM IMS-STATUS-CHECK                                             
025000     .                                                                    
025100     EJECT                                                                
023800 IMS-GU-WDE601 SECTION.                                                   
023900     MOVE 'IMS-GU-WDE601       '      TO CURRENT-IMS-SECTION              
023900                                                                          
024000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
024100            DELIMITED BY SIZE INTO SSA1                                   
024200     MOVE '  '                        TO GOOD-STATUSCODES                 
024300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
024400     MOVE WDE6-STATUS-CODE            TO STATUS-WS                        
024500     PERFORM IMS-STATUS-CHECK                                             
024600     .                                                                    
024700     EJECT                                                                
023800 IMS-GNP-WDE611 SECTION.                                                  
023900     MOVE 'IMS-GNP-WDE611      '      TO CURRENT-IMS-SECTION              
023900                                                                          
024000     MOVE SPACE                       TO ALL-SSA                          
024100                                                                          
024100     STRING 'WDE611  (KDKOLSTA =' W-WDE611-KDKOLSTA-X ')'                 
024200            DELIMITED BY SIZE INTO SSA1                                   
024300     MOVE '  GE'                      TO GOOD-STATUSCODES                 
024400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
024500     MOVE WDE6-STATUS-CODE            TO STATUS-WS                        
024600     PERFORM IMS-STATUS-CHECK                                             
024700     .                                                                    
025200 IMS-GU-WDE411-FSEQ SECTION.                                              
025300     MOVE 'IMS-GU-WDE411-FSEQ  '      TO CURRENT-IMS-SECTION              
025400                                                                          
025500     MOVE SPACE                       TO ALL-SSA                          
025600     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
025700            DELIMITED BY SIZE INTO SSA1                                   
025800     MOVE '  GE'                      TO GOOD-STATUSCODES                 
025900     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E411 SSA1                      
026000     MOVE WDE4-STATUS-CODE            TO STATUS-WS                        
026100     PERFORM IMS-STATUS-CHECK                                             
026200     .                                                                    
026300     SKIP3                                                                
026400 IMS-GNP-WDE401 SECTION.                                                  
026500     MOVE 'IMS-GNP-WDE401      '      TO CURRENT-IMS-SECTION              
026600                                                                          
026700     MOVE SPACE                       TO ALL-SSA                          
026800     MOVE 'WDE401'                    TO SSA1                             
026900     MOVE '    '                      TO GOOD-STATUSCODES                 
027000     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E401 SSA1                     
027100     MOVE WDE4-STATUS-CODE            TO STATUS-WS                        
027200     PERFORM IMS-STATUS-CHECK                                             
027300     .                                                                    
027400     SKIP2                                                                
027500 IMS-GNP-WDE421 SECTION.                                                  
027600     MOVE 'IMS-GNP-WDE421      '      TO CURRENT-IMS-SECTION              
027700                                                                          
027800     MOVE SPACE                       TO ALL-SSA                          
027900     STRING 'WDE421  (WDE421KY =' W-WDE4FSEQ-X ')'                        
028000            DELIMITED BY SIZE INTO SSA1                                   
028100     MOVE '    '                      TO GOOD-STATUSCODES                 
028200     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E421 SSA1                     
028300     MOVE WDE4-STATUS-CODE            TO STATUS-WS                        
028400     PERFORM IMS-STATUS-CHECK                                             
028500     .                                                                    
028600     EJECT                                                                
028700 IMS-GN-WDE411-FSEQ SECTION.                                              
028800     MOVE 'IMS-GN-WDE411-FSEQ  '      TO CURRENT-IMS-SECTION              
028900                                                                          
029000     MOVE SPACE                       TO ALL-SSA                          
029100     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
029200            DELIMITED BY SIZE INTO SSA1                                   
029300     MOVE '  GEGB'                    TO GOOD-STATUSCODES                 
029400     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-E411 SSA1                      
029500     MOVE WDE4-STATUS-CODE            TO STATUS-WS                        
029600     PERFORM IMS-STATUS-CHECK                                             
029700     .                                                                    
029800     EJECT                                                                
029900 IMS-GU-WDQ201 SECTION.                                                   
030000     MOVE 'IMS-GU-WDQ201       '      TO CURRENT-IMS-SECTION              
030100                                                                          
030200     MOVE SPACE                       TO ALL-SSA                          
030300     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X  ')'                        
030400            DELIMITED BY SIZE INTO SSA1                                   
030500     MOVE '    '                      TO GOOD-STATUSCODES                 
030600     CALL CBLTDLI USING GU  WDQ2-PCB DLI-IO-Q201 SSA1                     
030700     MOVE WDQ2-STATUS-CODE            TO STATUS-WS                        
030800     PERFORM IMS-STATUS-CHECK                                             
030900     .                                                                    
031000     EJECT                                                                
031100 IMS-STATUS-CHECK SECTION.                                                
031200                                                                          
031300     SET STATUS-IX                    TO 1                                
031400     SEARCH GOOD-STATUS                                                   
031500       AT END                                                             
031600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031700           DELIMITED BY SIZE INTO ERRTEXT                                 
031800         DISPLAY ERRTEXT                                                  
031900         CALL FELLOG                                                      
032000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
