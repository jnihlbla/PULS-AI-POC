000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4794000.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/03/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TO CREATE FOLLOW UP LIST WITH ORDER IN STATUS 'E'                
000900*                                                                         
001000*        THE PROGRAM READS     WDQ2                                       
001100*        THE PROGRAM READS     WDQ4                                       
001200*        THE PROGRAM READS     WDQ1                                       
001300*        THE PROGRAM READS     WDA5                                       
001400*        THE PROGRAM READS     WDQ3                                       
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- ORDERS WITH STATUS 'E'                                     
002900     SELECT W47940                     ASSIGN TO W47940D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W47940                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY W4794001 -PRE  OUT-  -L.                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4794000'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600     EJECT                                                                
004700 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES TODAYS-DATE.                                        
004900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005100     03  TODAYS-DATE-DAY         PIC 9(2).                                
005200     EJECT                                                                
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400*                                                                         
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     SKIP2                                                                
006000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  ERROR-TEXT.                                                          
006700     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  OUT-AREA-START              PIC X(24)   VALUE                        
007500                                 'OUT-AREA-START  '.                      
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY W4794001   -PRE OUT-                                      
007900     EJECT                                                                
008000*    --- AREAS FOR IMS-SECTIONS                                           
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  KEYS-FOR-DLI.                                                        
008600     03  W-WDQ101KY-FOM.                                                  
008700         05  W-IDORDER-Q1-F      PIC S9(7)    COMP-3.                     
008800         05  FILLER              PIC X(13)    VALUE LOW-VALUE.            
008900     03  W-WDQ101KY-TOM.                                                  
009000         05  W-IDORDER-Q1-T      PIC S9(7)    COMP-3.                     
009100         05  FILLER              PIC X(13)    VALUE HIGH-VALUE.           
009200     03  W-IDORDER-X.                                                     
009300         05  W-IDORDER           PIC S9(7)    COMP-3.                     
009400     03  W-WDQ301KY-FOM.                                                  
009500         05  W-IDORDER-Q3-F      PIC S9(7)    COMP-3.                     
009600         05  FILLER              PIC X(8)     VALUE LOW-VALUE.            
009700     03  W-WDQ301KY-TOM.                                                  
009800         05  W-IDORDER-Q3-T      PIC S9(7)    COMP-3.                     
009900         05  FILLER              PIC X(8)     VALUE HIGH-VALUE.           
010000     03  W-STATUS-U.                                                      
010100         05  W-KDODELSTA         PIC X(1)     VALUE 'U'.                  
010200     03  W-WDA501KY-FOM.                                                  
010300         05  W-IDDISTR-A5-F      PIC S9(5)    COMP-3.                     
010400         05  W-IDKUNDNR-A5-F     PIC S9(7)    COMP-3.                     
010500         05  W-IDKUNDRF-A5-F     PIC X(10).                               
010600         05  FILLER              PIC X(7)     VALUE LOW-VALUE.            
010700     03  W-WDA501KY-TOM.                                                  
010800         05  W-IDDISTR-A5-T      PIC S9(5)    COMP-3.                     
010900         05  W-IDKUNDNR-A5-T     PIC S9(7)    COMP-3.                     
011000         05  W-IDKUNDRF-A5-T     PIC X(10).                               
011100         05  FILLER              PIC X(7)     VALUE HIGH-VALUE.           
011200     03  W-WDQ401KY-FOM.                                                  
011300         05  W-IDORDER-Q4-F      PIC S9(7)    COMP-3.                     
011400         05  FILLER              PIC X(16)    VALUE LOW-VALUE.            
011500     03  W-WDQ401KY-TOM.                                                  
011600         05  W-IDORDER-Q4-T      PIC S9(7)    COMP-3.                     
011700         05  FILLER              PIC X(16)    VALUE HIGH-VALUE.           
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FOUND                       VALUE '  '.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     88  SEGMENT-NO-MORE                     VALUE 'GB'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(128).                              
012900 01  SSA2                        PIC X(128).                              
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ2'.                        
013600 01  DLI-IO-WDQ2.                                                         
013700     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
013800     SKIP3                                                                
013900     03  WLORQI01 REDEFINES IO-AREA.                                      
014000*        05  -COPY WDQ201                                                 
014100     SKIP3                                                                
014200     03  WLORQI11 REDEFINES IO-AREA.                                      
014300*        05  -COPY WDQ211                                                 
014400     SKIP3                                                                
014500     03  WLORQI12 REDEFINES IO-AREA.                                      
014600*        05  -COPY WDQ212                                                 
014700     SKIP3                                                                
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
014900 01  DLI-IO-WDQ401.                                                       
015000*    03  -COPY WDQ401                                                     
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ101'.                      
015200 01  DLI-IO-WDQ101.                                                       
015300*    03  -COPY WDQ101                                                     
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA501'.                      
015500 01  DLI-IO-WDA501.                                                       
015600*    03  -COPY WDA501                                                     
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
015800 01  DLI-IO-WDQ301.                                                       
015900*    03  -COPY WDQ301                                                     
016000     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200                                                                          
016300                                                                          
016400*01  -COPY W0008  -PRE WDQ2-                                              
016500     05  FILLER                  PIC X.                                   
016600                                                                          
016700*01  -COPY W0008  -PRE WDQ4-                                              
016800     05  FILLER                  PIC X.                                   
016900                                                                          
017000*01  -COPY W0008  -PRE WDQ1-                                              
017100     05  FILLER                  PIC X.                                   
017200                                                                          
017300*01  -COPY W0008  -PRE WDA5-                                              
017400     05  FILLER                  PIC X.                                   
017500                                                                          
017600*01  -COPY W0008  -PRE WDQ3-                                              
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900 PROCEDURE DIVISION  USING WDQ2-PCB WDQ4-PCB                              
018000                           WDQ1-PCB WDA5-PCB WDQ3-PCB.                    
018100                                                                          
018200 MAIN SECTION.                                                            
018300     ENTRY 'DLITCBL' USING WDQ2-PCB WDQ4-PCB                              
018400                           WDQ1-PCB WDA5-PCB WDQ3-PCB.                    
018500                                                                          
018600     PERFORM A-INIT                                                       
018700                                                                          
018800     PERFORM IMS-GN-WDQ2-SB                                               
018900                                                                          
019000     PERFORM UNTIL SEGMENT-NO-MORE                                        
019100       IF WDQ2-SEG-NAME-FB = 'WDQ201'                                     
019200         IF OHUV-FLKLAR = 'N' AND OHUV-TIREGDAT < TODAYS-DATE             
019300           PERFORM B-GET-IDPTYP                                           
019400           PERFORM C-WRITE-TO-FILE                                        
019500         END-IF                                                           
019600         PERFORM IMS-GN-WDQ2-SB                                           
019700       END-IF                                                             
019800     END-PERFORM                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN OUTPUT W47940                                                   
020900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021000                                                                          
021010     ACCEPT TODAYS-DATE FROM DATE                                         
021100     .                                                                    
021200     EJECT                                                                
021300 B-GET-IDPTYP SECTION.                                                    
021400                                                                          
021500     MOVE OHUV-IDORDER    TO W-IDORDER-Q4-F                               
021600                             W-IDORDER-Q4-T                               
021700                             W-IDORDER-Q1-F                               
021800                             W-IDORDER-Q1-T                               
021900                             W-IDORDER-Q3-F                               
022000                             W-IDORDER-Q3-T                               
022100                                                                          
022200     MOVE OHUV-IDDISTR    TO W-IDDISTR-A5-F                               
022300                             W-IDDISTR-A5-T                               
022400     MOVE OHUV-IDKUNDNR   TO W-IDKUNDNR-A5-F                              
022500                             W-IDKUNDNR-A5-T                              
022600     MOVE OHUV-IDKUNDRF   TO W-IDKUNDRF-A5-F                              
022700                             W-IDKUNDRF-A5-T                              
022800                                                                          
022900     PERFORM IMS-GET-WDQ401                                               
023000     IF SEGMENT-FOUND                                                     
023100       MOVE '001' TO OUT-IDPTYP                                           
023200     ELSE                                                                 
023300       PERFORM IMS-GET-WDQ101                                             
023400       IF SEGMENT-FOUND                                                   
023500         MOVE '003' TO OUT-IDPTYP                                         
023600       ELSE                                                               
023700         PERFORM IMS-GET-WDA501                                           
023800         IF SEGMENT-FOUND                                                 
023900           MOVE '002' TO OUT-IDPTYP                                       
024000         ELSE                                                             
024100           PERFORM IMS-GET-WDQ301                                         
024200           IF SEGMENT-FOUND                                               
024300             MOVE '004' TO OUT-IDPTYP                                     
024400           END-IF                                                         
024500         END-IF                                                           
024600       END-IF                                                             
024700     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 C-WRITE-TO-FILE SECTION.                                                 
025100                                                                          
025200     MOVE OHUV-IDORDER           TO OUT-IDORDER                           
025300     MOVE OHUV-IDDISTR           TO OUT-IDDISTR                           
025400     MOVE OHUV-IDKUNDNR          TO OUT-IDKUNDNR                          
025500     MOVE OHUV-IDKUNDRF          TO OUT-IDKUNDRF                          
025600     MOVE OHUV-IDSYSTEM          TO OUT-IDSYSTEM                          
025700     MOVE OHUV-IDUSER            TO OUT-IDUSER                            
025800     MOVE OHUV-KDORDKL           TO OUT-KDORDKL                           
025900     MOVE OHUV-TIREGDAT          TO OUT-TIREGDAT                          
026000     MOVE OHUV-IDDC-PRIM         TO OUT-IDDC                              
026100     PERFORM S11-WRITE-W47940                                             
026200     .                                                                    
026300     EJECT                                                                
026400 Z-FINIT SECTION.                                                         
026500     CLOSE W47940                                                         
026600     SKIP2                                                                
026700     MOVE 'S' TO POSTSUM-OPKOD                                            
026800     CALL POSTSUM USING POSTSUM-PARM                                      
026900     .                                                                    
027000     EJECT                                                                
027100 S11-WRITE-W47940 SECTION.                                                
027200                                                                          
027300     WRITE OUT-RECORD FROM OUT-AREA                                       
027400                                                                          
027500*    MOVE OUT-IDPTYP TO POSTSUM-TRANSTYP                                  
027600     MOVE 'W47940' TO POSTSUM-FDNAMN                                      
027700     MOVE 'W47940D1' TO POSTSUM-DDNAMN2                                   
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     .                                                                    
028000     EJECT                                                                
028100 S99-ABEND SECTION.                                                       
028200                                                                          
028300     SKIP2                                                                
028400     MOVE 'S' TO POSTSUM-OPKOD                                            
028500     CALL POSTSUM USING POSTSUM-PARM                                      
028600     CALL ABEND USING RKOD-ABEND                                          
028700     .                                                                    
028800     EJECT                                                                
028900* --- IMS SECTIONS  ---                                                   
029000                                                                          
029100     EJECT                                                                
029200 IMS-GN-WDQ2-SB SECTION.                                                  
029300                                                                          
029400     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-WDQ2                           
029500     MOVE '  GAGKGB'  TO GOOD-STATUSCODES                                 
029600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
029700     PERFORM IMS-STATUSCHECK                                              
029800     .                                                                    
029900     EJECT                                                                
030000 IMS-GET-WDQ401 SECTION.                                                  
030100                                                                          
030200     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-FOM                          
030300                    '&WDQ401KY<=' W-WDQ401KY-TOM ')'                      
030400          DELIMITED BY SIZE INTO SSA1                                     
030500     MOVE '  GE' TO GOOD-STATUSCODES                                      
030600     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
030700     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSCHECK                                              
030900     .                                                                    
031000     EJECT                                                                
031100 IMS-GET-WDQ101 SECTION.                                                  
031200                                                                          
031300     STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-FOM                          
031400                    '&WDQ101KY<=' W-WDQ101KY-TOM ')'                      
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     MOVE '  GE' TO GOOD-STATUSCODES                                      
031700     CALL CBLTDLI USING GU WDQ1-PCB DLI-IO-WDQ101 SSA1                    
031800     MOVE WDQ1-STATUS-CODE TO STATUS-WS                                   
031900     PERFORM IMS-STATUSCHECK                                              
032000     .                                                                    
032100     EJECT                                                                
032200 IMS-GET-WDA501 SECTION.                                                  
032300                                                                          
032400     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-FOM                          
032500                    '&WDA501KY<=' W-WDA501KY-TOM ')'                      
032600          DELIMITED BY SIZE INTO SSA1                                     
032700     MOVE '  GE' TO GOOD-STATUSCODES                                      
032800     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501 SSA1                    
032900     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
033000     PERFORM IMS-STATUSCHECK                                              
033100     .                                                                    
033200     EJECT                                                                
033300 IMS-GET-WDQ301 SECTION.                                                  
033400                                                                          
033500     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-FOM                          
033600                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
033700                    '&KDODELST =' W-STATUS-U     ')'                      
033800          DELIMITED BY SIZE INTO SSA1                                     
033900     MOVE '  GE' TO GOOD-STATUSCODES                                      
034000     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
034100     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
034200     PERFORM IMS-STATUSCHECK                                              
034300     .                                                                    
034400     EJECT                                                                
034500 IMS-STATUSCHECK SECTION.                                                 
034600                                                                          
034700     SET STATUS-IX TO 1                                                   
034800     SEARCH GOOD-STATUS                                                   
034900       AT END                                                             
035000         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
035100           DELIMITED BY SIZE INTO ERROR-TEXT                              
035200         DISPLAY ERROR-TEXT                                               
035300         CALL FELLOG                                                      
035400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
035500         CONTINUE                                                         
035600     END-SEARCH                                                           
035700     .                                                                    
