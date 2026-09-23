000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W411XDCA.                                                
000400 AUTHOR.         PRERNA DADHICH.                                          
000500 DATE-WRITTEN.   SUMMER 2022                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PRELIMINARY ALLOCATION OF ORDER LINES NDC/SDC                    
001000*        DIFFERENT RULES DEPENDING ON ORDER CLASS                         
001100*        ROW WILL BE REFFERED TO DIFFERENT DC IN TOTAL                    
001200*        NO ROW WILL BE SPLITTED                                          
001300*                                                                         
001400*        READING  WDB6 (01 16)                                            
001500*                 WDK6 (11)                                               
001600*                 WDK7 (11 12 22)                                         
001700*                 WDK9 (01)                                               
001800*                 WDL6 (01 11)                                            
001900*                 WDP7 (WLUSEA)                                           
002000*                 WDQ2 (01)                                               
002100*                 WDQ4B(01)                                               
002200*                 WDQ4 (01)                                               
002300*        UPDATING WDK7 (11)                                               
002400*                 WDR6 (01)                                               
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700 DATA DIVISION.                                                           
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100 77  IDPGM                       PIC X(8)    VALUE 'W411XDCA'.            
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003800 77  FIRST-DC                    PIC X(2)    VALUE SPACE.                 
003900 77  CURR-DC-IX                  PIC 9(3)    VALUE ZERO.                  
004000 77  IX-DCCLEAR-MAX              PIC 9(2)    VALUE 99.                    
004100 77  DC-IX                       PIC 9(3)    VALUE 01.                    
004200 77  CLEAR-IX                    PIC 9(1)    VALUE 1.                     
004300 77  WS-TIBERANK                 PIC 9(6)    VALUE ZERO.                  
004400 77  WS-KVAVIS                   PIC 9(6)    VALUE ZERO.                  
004500 77  WS-KVSPANT                  PIC 9(7)    VALUE ZERO.                  
004600 77  WS-DAPUBL                   PIC 9(6)    VALUE ZERO.                  
004700 77  WS-CDC-BLOCK                PIC X(1)    VALUE 'N' .                  
004800 77  WS-STOCK-SW                 PIC X(1)    VALUE 'N' .                  
004900 77  WS-BULK-SW                  PIC X(1)    VALUE 'N' .                  
005000 77  WS-PART-VALID               PIC X(1)    VALUE 'N' .                  
005100 77  WS-AVAILABLE                PIC S9(8)   VALUE ZERO.                  
005200 77  WS-AVAILABLE-HOME           PIC S9(8)   VALUE ZERO.                  
005300 77  WS-DISP-KVANT               PIC S9(8)   VALUE ZERO.                  
005400 77  WS-KVAKS                    PIC S9(8)   VALUE ZERO.                  
005500 77  WS-KVOKS-BULK               PIC 9(7)    VALUE ZERO.                  
005600 77  WS-KVOKS-DAY                PIC 9(7)    VALUE ZERO.                  
005700 77  WS-KVRESS                   PIC 9(7)    VALUE ZERO.                  
005800 77  WS-RETURNS                  PIC S9(8)   VALUE ZERO.                  
005900 77  WS-TIREGDAT                 PIC 9(6)    VALUE ZERO.                  
006000                                                                          
006100 77  WS-KDAKDISP                 PIC 9       VALUE 0.                     
006200     88  NO-AKS                              VALUE 0.                     
006300     88  FULL-AKS                            VALUE 1.                     
006400     88  LIMITED-AKS                         VALUE 2.                     
006500                                                                          
006600 77  CURRENT-LAND               PIC X       VALUE 'N'.                    
006700     88  CURRENT-LAND-CHINA                 VALUE 'J'.                    
006800     88  CURRENT-LAND-NOT-CHINA             VALUE 'N'.                    
006900                                                                          
007000 01  DISTR-ORDER-BLOCK-SW        PIC X       VALUE 'N'.                   
007100     88  DISTR-ORDER-BLOCK                   VALUE 'Y'.                   
007200                                                                          
007300 01  DISTR-REFILL-SW             PIC X       VALUE 'N'.                   
007400     88  DISTR-REFILL                        VALUE 'Y'.                   
007500     88  DISTR-REFILL-NO                     VALUE 'N'.                   
007600                                                                          
007700 01  DISTR-NO-STOCK-CHECK-SW     PIC X       VALUE 'N'.                   
007800     88  DISTR-NO-STOCK-CHECK                VALUE 'Y'.                   
007900                                                                          
008000 01  CLEAR-SW                    PIC X.                                   
008100     88  CLEARING                            VALUE 'J'.                   
008200     88  NO-CLEARING                         VALUE 'N'.                   
008300                                                                          
008310 01  DC-2-WRITTEN-SW             PIC X       VALUE 'N'.                   
008320     88  DC-2-WRITTEN                        VALUE 'Y'.                   
008330                                                                          
008400                                                                          
008500**INTERNAL REFRRAL TABLE                                                  
008600 01  DC-INFO.                                                             
008700     03  WORK-IDDC-INFO          OCCURS 99.                               
008800         05 DC-IDDC              PIC X(2).                                
008900         05 DC-PART-VALID        PIC X(1).                                
009000         05 DC-STOCK-AVAILABLE   PIC X(1).                                
009100         05 DC-KDORDBEK          PIC X(2).                                
009200         05 DC-FLLF              PIC X(1).                                
009300         05 DC-FLCLEAR           PIC X(1).                                
009400         05 DC-CLEARING          PIC X(1).                                
009500                                                                          
009600 01  STOCK-CHECK-SW              PIC X       VALUE 'N'.                   
009700     88  STOCK-CHECK-YES                     VALUE 'Y'.                   
009800     88  STOCK-CHECK-NO                      VALUE 'N'.                   
009900                                                                          
010000                                                                          
010100 01  CURRENT-TIME                PIC 9(8)    VALUE ZERO.                  
010200 01  FILLER REDEFINES CURRENT-TIME.                                       
010300     03  CURRENT-TTHHMM          PIC 9(6).                                
010400     03  FILLER                  PIC 9(2).                                
010500                                                                          
010600                                                                          
010700*      --- VALID IDDC CODES                                               
010800                                                                          
010900*01    -COPY WY2000W1                                                     
011000                                                                          
011100*01    -COPY WWDCKONS                                                     
011200                                                                          
011300*01    -COPY WWDC99                                                       
011400                                                                          
011500                                                                          
011600 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
011700 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011800     03  WS-TIHHMM               PIC 9(4).                                
011900     03  FILLER                  PIC 9(2).                                
012000                                                                          
012100 01  ERRORTEXT.                                                           
012200     03  FILLER                  PIC X(8)    VALUE 'ERRORTXT'.            
012300     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
012400                                                                          
012500                                                                          
012600                                                                          
012700 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
012800 01  FILLER REDEFINES TEST-IDDISTR.                                       
012900*    03   -COPY WWDIST07.                                                 
013000                                                                          
013100 01  FILLER REDEFINES TEST-IDDISTR.                                       
013200*    03   -COPY WWDIST18.                                                 
013300                                                                          
013400 01  FILLER REDEFINES TEST-IDDISTR.                                       
013500*    03   -COPY WWDIST35.                                                 
013600                                                                          
013700                                                                          
013800 01  DYNAMIC-SUBPROGRAM.                                                  
013900*                                                                         
014000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
014400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014500     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
014600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
014700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
014800                                                                          
014900*                                                                         
015000*    --- PARAMETER TO WZ20DAYS                                            
015100*    -COPY WZ20DAYS                                                       
015200                                                                          
015300*    --- PARAMETER TO WORKDAY                                             
015400*01  -COPY WORKAREA                                                       
015500                                                                          
015600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
015700*   -COPY W005WDK7                                                        
015800                                                                          
015900*    --- PARAMETER TO ABEND                                               
016000 01  RKOD-ABEND-WITHOUT-DUMP     PIC S9(4)   COMP VALUE +16.              
016100 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016200                                                                          
016300                                                                          
016400*    --- PARAMETER TO WDAGAREA                                            
016500 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
016600*01  -COPY WDAGAREA                                                       
016700                                                                          
016800*    --- PARAMETER TO W005INIT                                            
016900 01  FILLER                      PIC X(16) VALUE 'WMSGINIT'.              
017000*01  -COPY WMSGINIT                                                       
017100                                                                          
017200                                                                          
017300*01 -COPY WWBYT03                                                         
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017700                                                                          
017800 01  KEYS-TO-DLI.                                                         
017900     03  W-IDARTNR-X.                                                     
018000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018100     03  W-KDSEGKEY-X.                                                    
018200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018300     03  W-IDDC-X.                                                        
018400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018500     03  W-IDLAND-X.                                                      
018600         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
018700     03  W-IDLEVNR-1441-X.                                                
018800         05  W-IDLEVNR-1441      PIC X(5)    VALUE '1441 '.               
018900                                                                          
019000     03  W-WDQ4B1KY-MIN-X.                                                
019100         05  W-IDARTNR-Q4-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
019200         05  FILLER              PIC  X(32)  VALUE LOW-VALUE.             
019300                                                                          
019400     03  W-WDQ4B1KY-MAX-X.                                                
019500         05  W-IDARTNR-Q4-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
019600         05  FILLER              PIC  X(32)  VALUE HIGH-VALUE.            
019700                                                                          
019800     03  W-IDORDER-X.                                                     
019900         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
020000                                                                          
020100     03  W-IDDC-B6-X.                                                     
020200         05 W-IDDC-B6            PIC X(2).                                
020300     03  W-IDDC-REF-X.                                                    
020400         05 W-IDDC-REF           PIC X(2).                                
020500                                                                          
020600     03  W-WDQ401KY-X.                                                    
020700         05  W-ORAD-IDORDER      PIC S9(7)   VALUE ZERO COMP-3.           
020800         05  W-ORAD-IDDC         PIC  X(2)   VALUE ZERO.                  
020900         05  W-ORAD-ADLAGOMR     PIC S9(3)   VALUE ZERO COMP-3.           
021000         05  W-ORAD-ADGANG       PIC S9(3)   VALUE ZERO COMP-3.           
021100         05  W-ORAD-ADPLATS      PIC S9(5)   VALUE ZERO COMP-3.           
021200         05  W-ORAD-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
021300         05  W-ORAD-IDLOPNR      PIC S9(3)   VALUE ZERO COMP-3.           
021400     03  W-IDDC-Q4-X.                                                     
021500         05  W-IDDC-Q4           PIC  X(2)   VALUE SPACE.                 
021600                                                                          
021700     03  W-IDPTYP-X.                                                      
021800         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
021900     03  W-KDRT-07-X.                                                     
022000         05  W-KDRT-07           PIC S9(3)   VALUE 7    COMP-3.           
022100     03  W-KDRT-77-X.                                                     
022200         05  W-KDRT-77           PIC S9(3)   VALUE 77   COMP-3.           
022300                                                                          
022400                                                                          
022500*    --- STATUS CODE FROM IMS                                             
022600 01  STATUS-WS                   PIC XX.                                  
022700     88  SEGMENT-FOUND                       VALUE '  '.                  
022800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
022900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
023000     88  END-OF-DATA                         VALUE 'GB'.                  
023100                                                                          
023200 01  GOOD-STATUSCODES.                                                    
023300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023400                                                                          
023500 01  ALL-SSA.                                                             
023600     03 SSA1                     PIC X(64).                               
023700     03 SSA2                     PIC X(64).                               
023800     03 SSA3                     PIC X(64).                               
023900                                                                          
024000                                                                          
024100*    --- IMS FUNCTION CODES                                               
024200*01  -COPY W0003                                                          
024300                                                                          
024400*    ---  DLI INPUT-OUTPUT AREA                                           
024500                                                                          
024600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
024700 01  DLI-IO-WDB601.                                                       
024800*    03  -COPY WDB601                                                     
024900                                                                          
025000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB616'.         
025100 01  DLI-IO-WDB616.                                                       
025200*    03  -COPY WDB616                                                     
025300                                                                          
025400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
025500 01  DLI-IO-WDK601.                                                       
025600*    03  -COPY WDK601                                                     
025700                                                                          
025800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
025900 01  DLI-IO-WDK611.                                                       
026000*    03  -COPY WDK611                                                     
026100                                                                          
026200                                                                          
026300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
026400 01  DLI-IO-WDK711.                                                       
026500*    03  -COPY WDK711                                                     
026600                                                                          
026700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
026800 01  DLI-IO-WDK712.                                                       
026900*    03  -COPY WDK712                                                     
027000                                                                          
027100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK722'.         
027200 01  DLI-IO-WDK722.                                                       
027300*    03  -COPY WDK722                                                     
027400                                                                          
027500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK901'.         
027600 01  DLI-IO-WDK901.                                                       
027700*    03  -COPY WDK901                                                     
027800                                                                          
027900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL601'.         
028000 01  DLI-IO-WDL601.                                                       
028100*    03  -COPY WDL601                                                     
028200                                                                          
028300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL611'.         
028400 01  DLI-IO-WDL611.                                                       
028500*    03  -COPY WDL611                                                     
028600                                                                          
028700 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
028800 01   DLI-IO-AREA-R601.                                                   
028900*     03  -COPY WDR601                                                    
029000*     05  -COPY W414LOGG      -RED FIL-WDR601-DATA                        
029100                                                                          
029200                                                                          
029300 01  FILLER               PIC X(16)   VALUE 'WDQ201 AREA'.                
029400 01   DLI-IO-WDQ201.                                                      
029500*     03  -COPY WDQ201 -PRE Q2-                                           
029600                                                                          
029700 01  FILLER               PIC X(16)   VALUE 'WDQ401 AREA'.                
029800 01   DLI-IO-WDQ401.                                                      
029900*     03  -COPY WDQ401                                                    
030000                                                                          
030100 01  FILLER               PIC X(16)   VALUE 'WDQ4B1 AREA'.                
030200 01   DLI-IO-AREA-Q4B1.                                                   
030300*     03  -COPY WDQ4B1                                                    
030400                                                                          
030500                                                                          
030600                                                                          
030700 LINKAGE SECTION.                                                         
030800*    -COPY WDQ201                                                         
030900*    -COPY W411XDCA                                                       
031000                                                                          
031100*01  -COPY W0008  -PRE USEA-                                              
031200     05  FILLER                  PIC X.                                   
031300                                                                          
031400*01  -COPY W0008  -PRE WDB6-                                              
031500     05  FILLER                  PIC X.                                   
031600                                                                          
031700*01  -COPY W0008  -PRE WDK6-                                              
031800     05  FILLER                  PIC X.                                   
031900                                                                          
032000*01  -COPY W0008  -PRE WDK7-                                              
032100     05  FILLER                  PIC X.                                   
032200                                                                          
032300*01  -COPY W0008  -PRE WDK9-                                              
032400     05  FILLER                  PIC X.                                   
032500                                                                          
032600*01  -COPY W0008  -PRE WDL6-                                              
032700     05  FILLER                  PIC X.                                   
032800                                                                          
032900*01  -COPY W0008  -PRE WDQ4B-                                             
033000     05  FILLER                  PIC X.                                   
033100                                                                          
033200*01  -COPY W0008  -PRE WDQ2-                                              
033300     05  FILLER                  PIC X.                                   
033400                                                                          
033500*01  -COPY W0008  -PRE WDQ4-                                              
033600     05  FILLER                  PIC X.                                   
033700                                                                          
033800*01  -COPY W0008  -PRE WDR6-                                              
033900     05  FILLER                  PIC X.                                   
034000                                                                          
034100*01  -COPY W0008  -PRE WDB6-2-                                            
034200     05  FILLER                  PIC X.                                   
034300                                                                          
034400*01  -COPY W0008  -PRE WDK6-2-                                            
034500     05  FILLER                  PIC X.                                   
034600                                                                          
034700*01  -COPY W0008  -PRE WDK7-2-                                            
034800     05  FILLER                  PIC X.                                   
034900                                                                          
035000*01  -COPY W0008  -PRE WDK7-3-                                            
035100     05  FILLER                  PIC X.                                   
035200                                                                          
035300 PROCEDURE DIVISION  USING OHUV-WDQ201   XDCA-W411XDCA                    
035400                           USEA-PCB WDB6-PCB WDK6-PCB WDK7-PCB            
035500                           WDK9-PCB WDL6-PCB WDQ4B-PCB                    
035600                           WDQ2-PCB WDQ4-PCB WDR6-PCB                     
035700                           WDB6-2-PCB WDK6-2-PCB WDK7-2-PCB               
035800                           WDK7-3-PCB.                                    
035900                                                                          
036000 MAIN SECTION.                                                            
036100                                                                          
036200                                                                          
036300     PERFORM A-INIT                                                       
036400     MOVE 1                      TO CURR-DC-IX                            
036500     MOVE XDCA-IDARTNR           TO W-IDARTNR                             
036600     PERFORM UNTIL NO-CLEARING   OR                                       
036700                  CURR-DC-IX > IX-DCCLEAR-MAX                             
036800        MOVE XDCA-IDDC-CLEAR-IN(CURR-DC-IX)                               
036900                                      TO DC-IDDC(CURR-DC-IX)              
037000                                         W-IDDC                           
037100                                         WS-IDDC                          
037200                                                                          
037300                                                                          
037400        PERFORM B-ALLOCATE-DC                                             
037500        IF STOCK-CHECK-YES                                                
037600            MOVE 'N'             TO STOCK-CHECK-SW                        
037700            PERFORM C-CHECK-AVAILABLE-STOCK                               
037800        END-IF                                                            
037900        PERFORM D-CLEARING-TABLE-UPDATE                                   
038000        ADD 1 TO CURR-DC-IX                                               
038100     END-PERFORM                                                          
038200                                                                          
038300     IF XDCA-IDDC-RO NOT =SPACE                                           
038400        MOVE SLAG-ADLAGOMR     TO XDCA-ADLAGOMR                           
038500        MOVE SLAG-ADGANG       TO XDCA-ADGANG                             
038600        MOVE SLAG-ADPLATS      TO XDCA-ADPLATS                            
038700     END-IF                                                               
038800                                                                          
038900     IF XDCA-IDDC-TVS = SPACE AND                                         
039000        XDCA-IDDC NOT = FIRST-DC                                          
039100        PERFORM E-NEW-LOCAL-TIME                                          
039200     END-IF                                                               
039300                                                                          
039400     PERFORM F-ORDERHIT                                                   
039500     MOVE ZERO TO RETURN-CODE                                             
039600                                                                          
039700     GOBACK                                                               
039800     .                                                                    
039900                                                                          
040000                                                                          
040100                                                                          
040200 A-INIT SECTION.                                                          
040300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
040400                                                                          
040500     MOVE +1 TO CURR-DC-IX                                                
040600                                                                          
040700*INITIALIZE THE DC TABLE                                                  
040800     PERFORM UNTIL CURR-DC-IX > IX-DCCLEAR-MAX                            
040900        MOVE SPACE          TO DC-IDDC(CURR-DC-IX)                        
041000        MOVE ZERO           TO DC-STOCK-AVAILABLE(CURR-DC-IX)             
041100                               DC-KDORDBEK(CURR-DC-IX)                    
041200        MOVE SPACE          TO DC-FLLF(CURR-DC-IX)                        
041300                               DC-IDDC(CURR-DC-IX)                        
041400                               DC-PART-VALID(CURR-DC-IX)                  
041500                               DC-STOCK-AVAILABLE(CURR-DC-IX)             
041600                               DC-KDORDBEK(CURR-DC-IX)                    
041700                               DC-FLLF(CURR-DC-IX)                        
041800                               DC-FLCLEAR (CURR-DC-IX)                    
041900                               DC-CLEARING(CURR-DC-IX)                    
042000        ADD 1 TO CURR-DC-IX                                               
042100     END-PERFORM                                                          
042200                                                                          
042300     MOVE 'N' TO DISTR-ORDER-BLOCK-SW                                     
042400     MOVE 'N' TO DISTR-REFILL-SW                                          
042500     MOVE 'N' TO DISTR-NO-STOCK-CHECK-SW                                  
042600     MOVE 'N' TO WS-CDC-BLOCK                                             
042700     MOVE 'N' TO WS-BULK-SW                                               
042800     MOVE 'N' TO STOCK-CHECK-SW                                           
042900     MOVE 'J' TO CLEAR-SW                                                 
043000                                                                          
043100     MOVE XDCA-TIREGDAT      TO WS-TIREGDAT                               
043200     MOVE 999999             TO WS-TIBERANK                               
043300     MOVE ZERO               TO WS-KVAVIS                                 
043400                                                                          
043500     MOVE XDCA-IDDC          TO XDCA-IDDC-OUT                             
043600     MOVE SPACE              TO XDCA-IDDC-RO                              
043700     MOVE XDCA-TIREGDAT      TO XDCA-TIREGDAT-OUT                         
043800     MOVE XDCA-TIREGTID      TO XDCA-TIREGTID-OUT                         
043900     MOVE XDCA-VKART         TO XDCA-VKART-OUT                            
044000     MOVE ZERO               TO W-IDARTNR                                 
044100     MOVE OHUV-IDDISTR       TO TEST-IDDISTR                              
044200                                                                          
044300     IF DIST35-REFILL           OR                                        
044400        DIST35-NONVCC-REFILL    OR                                        
044410        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
044420        DIST35-NONVCC-VCC-TRANSFER OR                                     
044500        DIST35-REFILL-NA-JAP    OR                                        
044600        DIST35-REFILL-INOM-JP   OR                                        
044700        DIST35-REFILL-INOM-NDC                                            
044800        MOVE 'Y'             TO DISTR-REFILL-SW                           
044900     END-IF                                                               
045000                                                                          
045100     IF DIST18-SKROT-SDC        OR                                        
045200        DIST18-SCRAP-NDC-SC     OR                                        
045300        DIST35-JPAU-CDC-RETUR   OR                                        
045400        DIST35-NA-CDC-BB-RETURN OR                                        
045500        DIST35-CN-CDC-RETUR     OR                                        
045600        DIST35-CN-NDC-RETURNS   OR                                        
045700        DIST35-NA-TRANSFER      OR                                        
045800        DIST35-NA-NDC-RETURNS   OR                                        
045900        DIST35-CN-TRANSFER      OR                                        
046000        DIST35-PACIFIC-TRANSFER                                           
046100        MOVE 'N'             TO DISTR-ORDER-BLOCK-SW                      
046200     ELSE                                                                 
046300        MOVE 'Y'             TO DISTR-ORDER-BLOCK-SW                      
046400     END-IF                                                               
046500                                                                          
046600     IF DIST18-SCRAP-NDC-QUAL     OR                                      
046700        DIST18-SKROT-SDC          OR                                      
046800        DIST18-SKROT-KVAL-SDC     OR                                      
046900        DIST35-NA-CDC-QUAL-RETURN OR                                      
047000        DIST35-CN-CDC-RETUR-Q     OR                                      
047100        DIST35-NA-TRANSFER        OR                                      
047200        DIST35-NA-NDC-RETURNS     OR                                      
047300        DIST35-CN-TRANSFER        OR                                      
047400        DIST35-CN-NDC-RETURNS                                             
047500        MOVE 'Y' TO DISTR-NO-STOCK-CHECK-SW                               
047600     END-IF                                                               
047700     .                                                                    
047800 B-ALLOCATE-DC   SECTION.                                                 
047900******************************************************************        
048000*THIS IS FOR ALLOCATING DC                                       *        
048100******************************************************************        
048200     MOVE 'B-ALLOC-DC      '   TO CURRENT-SECTION                         
048300     MOVE  NEJ                 TO CLEAR-SW                                
048400                                                                          
048500                                                                          
048600* IF MOVED FROM IDDC-TVS THEN MOVE IT TO TABLE OR W-IDDC                  
048700     IF XDCA-IDDC-TVS NOT = SPACE                                         
048800       MOVE XDCA-IDDC-TVS      TO DC-IDDC(CURR-DC-IX)                     
048900                                  W-IDDC                                  
049000     END-IF                                                               
049100                                                                          
049200     PERFORM IMS-GU-WDB601                                                
049300* ON 4403 BULK-CLEAR IS PRESENT AND WE SHOULD TREAT                       
049400* ORDER CLASS 2,3,4 SHOULD BE TREATED AS  1 ORDER CLASS                   
049500* AND HOME-DC SWITCH WILL BE CONSIDER FINAL VALID OR NOT                  
049600     IF DCS-FLCLEAR-BULK = YES AND                                        
049700        OHUV-KDORDKL > 1 AND CURR-DC-IX = 1                               
049800        MOVE 'Y'               TO WS-BULK-SW                              
049900     END-IF                                                               
050000                                                                          
050100* MOVING WEIGHT AND VOLUME                                                
050200                                                                          
050300     MOVE DCS-IDLANDX2         TO W-IDLAND                                
050400     PERFORM IMS-GU-WDK712                                                
050500     IF SEGMENT-FOUND                                                     
050600        IF LART-KDARTURS > SPACE                                          
050700           MOVE LART-KDARTURS  TO XDCA-KDARTURS                           
050800        END-IF                                                            
050900        IF LART-VKART > ZERO AND                                          
051000           LART-VKART NOT = XDCA-VKART                                    
051100           MOVE LART-VKART     TO XDCA-VKART-OUT                          
051200                                  XDCA-VKART-NTO                          
051300        END-IF                                                            
051400        IF LART-VLARTNTO > 0                                              
051500           MOVE LART-VLARTNTO  TO XDCA-VLARTNTO                           
051600        END-IF                                                            
051700     ELSE                                                                 
051800        MOVE ZERO              TO LART-DAPUBL                             
051900     END-IF                                                               
052000* CHECK FOR PUBL WEEK IF THERE IS CDC-BLOCK FROM W411SPAR THEN            
052100* WE NEED TO CHECK NDC-PUBL AS WELL IF ZERO THEN SHOULD BE BLOCKED        
052200                                                                          
052300     IF XDCA-DAPUBL = 99999999 AND LART-DAPUBL = ZERO                     
052400         MOVE 'Y'              TO WS-CDC-BLOCK                            
052500     END-IF                                                               
052600     MOVE LART-DAPUBL          TO XDCA-DAPUBL                             
052700                                                                          
052800     PERFORM BA-CHECK-KVSPANT                                             
052900     PERFORM BB-CHECK-PART-VALID                                          
053000                                                                          
053100                                                                          
053200*PART IS IN THE RANGE  ON 0510                                            
053300     IF WS-PART-VALID = 'Y'                                               
053500                                                                          
053600* SETTING BACK-ORDER ON HOME DC IF WE ARE MOVING W-IDDC TO RO IN          
053700* E-VALID SECTION THEN I GUESS WE CAN REMOVE IT                           
053800        IF CURR-DC-IX = 1                                                 
053900            MOVE W-IDDC        TO XDCA-IDDC-RO                            
054000        ELSE                                                              
054100          IF CURR-DC-IX >1 AND XDCA-IDDC-RO = SPACE                       
054200            MOVE W-IDDC        TO XDCA-IDDC-RO                            
054300          END-IF                                                          
054400        END-IF                                                            
054500                                                                          
054600*                                                                         
054700* PART APPORVED FOR REFILL                                                
054800        MOVE SLAG-FLREFILL     TO DC-FLLF(CURR-DC-IX)                     
054900                                                                          
055000*IF IT IS A SPECIAL ORDER THEN IT SHOULD BE DELIVERED                     
055100        IF  CURR-DC-IX = 1 AND                                            
055200            OHUV-FLORDSPE = JA                                            
055300            MOVE NOO           TO DC-CLEARING(CURR-DC-IX)                 
055400        ELSE                                                              
055500           PERFORM BC-CHECK-PUBLWEEK-BLOCKS                               
055600        END-IF                                                            
055700* END OF PART-VALID                                                       
055800     END-IF                                                               
055900     .                                                                    
056000                                                                          
056100******************************************************************        
056200*KVSPANT IS ONLY FOR CHINA DC, NOW FOR ALL NDC                   *        
056300******************************************************************        
056400 BA-CHECK-KVSPANT       SECTION.                                          
056500     MOVE 'BA-CHECK-KVSPAN'    TO CURRENT-SECTION                         
056600                                                                          
056700     PERFORM IMS-GU-WDK722                                                
056800     IF SEGMENT-FOUND                                                     
056900        MOVE XLAG-KVSPANT      TO WS-KVSPANT                              
057000     ELSE                                                                 
057100        MOVE ZERO              TO WS-KVSPANT                              
057200     END-IF                                                               
057300     .                                                                    
057400******************************************************************        
057500*CALL FOR WDK7 AND SET SWITCH BASED ON THIS                               
057600******************************************************************        
057700 BB-CHECK-PART-VALID        SECTION.                                      
057800     MOVE 'BB-CHECK-PART-VALID' TO CURRENT-SECTION                        
057900                                                                          
058000     PERFORM IMS-GHU-WDK711                                               
058100     IF SEGMENT-FOUND                                                     
058200       MOVE 'Y'                TO WS-PART-VALID                           
058300                                  DC-PART-VALID(CURR-DC-IX)               
058400     ELSE                                                                 
058500*IF WE DONT FIND THE VALID DC THEN WE NEED TO CREATE A CODE FOR IT        
058600       MOVE NEJ                TO DC-FLLF(CURR-DC-IX)                     
058700       MOVE 'N'                TO WS-PART-VALID                           
058800                                  DC-PART-VALID(CURR-DC-IX)               
058900       IF CURR-DC-IX =1                                                   
059000         IF XDCA-IDDC-TVS NOT = SPACE                                     
059100           IF NDC-CA AND OHUV-FLFORBI = NEJ                               
059200              MOVE 55          TO DC-KDORDBEK(CURR-DC-IX)                 
059300                                  XDCA-KDORDBEK                           
059400           ELSE                                                           
059500              MOVE 53          TO DC-KDORDBEK(CURR-DC-IX)                 
059600                                  XDCA-KDORDBEK                           
059700           END-IF                                                         
059800         ELSE                                                             
059900           IF DCS-FLARTADD = 'Y'                                          
060000             IF (OHUV-KDORDKL = 0 AND                                     
060100                (OHUV-FLFORBI = JA OR                                     
060200                 XDCA-IDDC-TVS NOT = SPACE)) OR                           
060300                XDCA-IDDC-CLEAR-IN(CURR-DC-IX) = '11'                     
060400               CONTINUE                                                   
060500             ELSE                                                         
060600               MOVE ALL '+'        TO WDK7-W005WDK7                       
060700               MOVE 'WDK711'       TO WDK7-IDSEGM                         
060800               MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                    
060900               MOVE W-IDDC         TO WDK7-IDDC-KFB                       
061000                                      WDK7-IDDC                           
061100               IF BYT03-OBJEKT                                            
061200                 MOVE NEJ          TO WDK7-FLREFILL                       
061300               END-IF                                                     
061400               CALL W005WDK7 USING WDK7-W005WDK7 WDB6-2-PCB               
061500                                   WDK6-2-PCB WDK7-2-PCB                  
061600               PERFORM IMS-GHU-WDK711                                     
061700               IF SEGMENT-FOUND                                           
061800                 MOVE 'Y'        TO WS-PART-VALID                         
061900                                    DC-PART-VALID(CURR-DC-IX)             
062000               END-IF                                                     
062100               MOVE ALL '+'        TO WDK7-W005WDK7                       
062200               MOVE 'WDK721'       TO WDK7-IDSEGM                         
062300               MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                    
062400               MOVE W-IDDC         TO WDK7-IDDC-KFB                       
062500               MOVE '1'            TO WDK7-KDSEGKEY                       
062600                                   IN WDK7-WDK721                         
062700               MOVE FUNCTION CURRENT-DATE(1:8)                            
062800                                   TO WDK7-DAORDSP-EJRO                   
062900               MOVE 'AUTOINS'      TO WDK7-IDUSER-ORDSP-EJRO              
063000               CALL W005WDK7 USING WDK7-W005WDK7 WDB6-2-PCB               
063100                                                 WDK6-2-PCB               
063200                                                 WDK7-3-PCB               
063300             END-IF                                                       
063400           END-IF                                                         
063500           IF WS-PART-VALID = 'N'                                         
063600             IF XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                       
063700                                              = SPACE OR '11'             
063800                MOVE NEJ        TO CLEAR-SW                               
063900                MOVE 55          TO DC-KDORDBEK(CURR-DC-IX)               
064000                                    XDCA-KDORDBEK                         
064100             ELSE                                                         
064200                MOVE JA            TO CLEAR-SW                            
064300                                      DC-CLEARING(CURR-DC-IX)             
064400                MOVE 15            TO DC-KDORDBEK(CURR-DC-IX)             
064500                                      XDCA-KDORDBEK                       
064600             END-IF                                                       
064700           END-IF                                                         
064800         END-IF                                                           
064900       ELSE                                                               
065000*LAST READ OF THE CLEARING TABLE                                          
065100         IF CURR-DC-IX = IX-DCCLEAR-MAX   OR                              
065200            XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                           
065300                                          = SPACE OR '11'                 
065400           IF XDCA-IDDC-RO = SPACE                                        
065500*THIS MEANS THAT THERE WAS NO VALID DC IN CLEARING TABLE                  
065600              MOVE 55               TO DC-KDORDBEK(CURR-DC-IX)            
065700                                       XDCA-KDORDBEK                      
065800              MOVE XDCA-IDDC-CLEAR-IN(1)                                  
065900                                    TO XDCA-IDDC-OUT                      
066000           ELSE                                                           
066100*NO MORE CLEARING BUT FOUND VALID DC IN CLEARING FROM 4414                
066200              PERFORM BBA-CREATE-BACKORDER                                
066300           END-IF                                                         
066400         ELSE                                                             
066500           MOVE JA                  TO CLEAR-SW                           
066600                                       DC-CLEARING(CURR-DC-IX)            
066700           MOVE 15                  TO DC-KDORDBEK(CURR-DC-IX)            
066800                                       XDCA-KDORDBEK                      
066900         END-IF                                                           
067000       END-IF                                                             
067100     END-IF                                                               
067200     .                                                                    
067300                                                                          
067400******************************************************************        
067500*IF WE FIND PART VALID ON FIRST DC HENCE IDDC-RO IS NOT SPACE    *        
067600*THEN WE NEED TO SET BO ON THIS DC                               *        
067700*THERE ARE NO MORE CLEARING IN THE TABLE AND WE NEED TO SET      *        
067800*BACKORDER ON THE VALID DC                                       *        
067900******************************************************************        
068000                                                                          
068100 BBA-CREATE-BACKORDER  SECTION.                                           
068200     MOVE 'BBA-CREATE-BO   ' TO CURRENT-SECTION                           
068300                                                                          
068400     IF XDCA-KDERS < 20                                                   
068500       MOVE XDCA-IDDC-RO         TO W-IDDC                                
068600                                    XDCA-IDDC-OUT                         
068700*W-IDDC IS THE FIRST VALID DC IN THIS CASE                                
068800       PERFORM IMS-GHU-WDK711                                             
068900       IF OHUV-KDORDKL = +0                                               
069000         MOVE 92                 TO DC-KDORDBEK(CURR-DC-IX)               
069100                                    XDCA-KDORDBEK                         
069200         ADD XDCA-KVBEART-Q      TO SLAG-KVOKS-DAG                        
069300       ELSE                                                               
069400         IF OHUV-KDORDKL = +1                                             
069500           IF OHUV-FLRESTN = JA AND OHUV-FLPRELRO = JA                    
069600             MOVE 99             TO DC-KDORDBEK(CURR-DC-IX)               
069700                                    XDCA-KDORDBEK                         
069800           ELSE                                                           
069900             IF XDCA-IDDC-RO = XDCA-IDDC-CLEAR-IN(1)                      
070000               MOVE ZERO          TO DC-KDORDBEK(CURR-DC-IX)              
070100                                     XDCA-KDORDBEK                        
070200             END-IF                                                       
070300           END-IF                                                         
070400           ADD XDCA-KVBEART-Q    TO SLAG-KVOKS-DAG                        
070500         ELSE                                                             
070600           MOVE ZERO             TO DC-KDORDBEK(CURR-DC-IX)               
070700                                    XDCA-KDORDBEK                         
070800           ADD XDCA-KVBEART-Q    TO SLAG-KVOKS-BULK                       
070900         END-IF                                                           
071000       END-IF                                                             
071100       PERFORM S12-REPL-WDK7                                              
071200* W-DISP-HOME IS MOVED IN CASE OF FIRST DC                                
071300       MOVE WS-AVAILABLE-HOME    TO XDCA-KVPREAVB                         
071400       COMPUTE XDCA-KVPRERO       = XDCA-KVBEART-Q                        
071500                                  - WS-AVAILABLE-HOME                     
071600     END-IF                                                               
071700     .                                                                    
071800******************************************************************        
071900*PUBL WEEK CHECK WITH OTHER CHECKS AS WELL                      *         
072000******************************************************************        
072100 BC-CHECK-PUBLWEEK-BLOCKS  SECTION.                                       
072200     MOVE 'BC-CHECK-BLOCK' TO CURRENT-SECTION                             
072300                                                                          
072400      IF LART-DAPUBL > ZERO                                               
072500         MOVE SPACE             TO DAYS-TIDATE1                           
072600         MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                         
072700         MOVE LART-DAPUBL(3:6)  TO DAYS-TIDATE2                           
072800         MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                         
072900         MOVE 14                TO DAYS-KVDAYS                            
073000         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
073100         IF DAYS-KDRC NOT = ZERO                                          
073200            MOVE 'WZ20DAYS ERROR ADDING 14 DAYS'                          
073300                                TO ERRORTEXT-STR                          
073400            CALL ABEND USING RKOD-ABEND-WITH-DUMP                         
073500         END-IF                                                           
073600         MOVE DAYS-TIDATE1(1:6) TO TMP2-YYMMDD                            
073700      ELSE                                                                
073800         MOVE LART-DAPUBL(3:6)  TO TMP2-YYMMDD                            
073900      END-IF                                                              
074000      MOVE WS-TIREGDAT          TO TMP1-YYMMDD                            
074100      PERFORM WY2000P1                                                    
074200      IF ((TMP1-YYMMDD < TMP2-YYMMDD ) AND  DISTR-REFILL-NO)              
074300                                     OR                                   
074400            WS-CDC-BLOCK = 'Y'       OR                                   
074500           (SLAG-FLORDSP-EJRO = JA   AND DISTR-ORDER-BLOCK)               
074600                                                                          
074700            MOVE 55          TO DC-KDORDBEK(CURR-DC-IX)                   
074800                                XDCA-KDORDBEK                             
074900            MOVE NOO         TO DC-CLEARING(CURR-DC-IX)                   
075000                                                                          
075100      ELSE                                                                
075200         IF  NOT DISTR-NO-STOCK-CHECK                         AND         
075300             (SLAG-FLORDSP = 'J 'AND (OHUV-KDORDKL = +1       OR          
075400             (NDC-CN             AND XDCA-FLLDCKND = JA)))    OR          
075500                                                                          
075600           ((SLAG-KDLEVSP > 0    AND ((OHUV-KDORDKL = 0 OR 1) OR          
075700                      (NDC-CN    AND XDCA-FLLDCKND = JA))))   OR          
075800                                                                          
075900            (SLAG-FLSPBULK = 'J' AND OHUV-KDORDKL > 1 )                   
076000            IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR          
076100               XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                        
076200                                             = SPACE OR '11'  OR          
076300              (CURR-DC-IX = 1 AND XDCA-IDDC-TVS NOT = SPACE)              
076400                                                                          
076500*             GENERATE CODE 98/92 ETC                                     
076600               PERFORM BCA-CREATE-BO-CODE                                 
076700            ELSE                                                          
076800               IF CURR-DC-IX < IX-DCCLEAR-MAX                             
076900                  MOVE JA    TO CLEAR-SW                                  
077000                                DC-CLEARING(CURR-DC-IX)                   
077100                  MOVE 15    TO DC-KDORDBEK(CURR-DC-IX)                   
077200                                XDCA-KDORDBEK                             
077300               END-IF                                                     
077400            END-IF                                                        
077500         ELSE                                                             
077600* THERE IS NO BLOCK AND WE CAN PROCEED TO CHECK STOCK                     
077700            MOVE  YES         TO STOCK-CHECK-SW                           
077800                                                                          
077900         END-IF                                                           
078000      END-IF                                                              
078100     .                                                                    
078200******************************************************************        
078300*WE CREATE BACKORDER CODES  IF SPECIAL BLOCKS FOR CLASS 0 AND    *        
078400*FOR BULK WE CREATE ORDER WITHOUT ANY CODES                     *         
078500******************************************************************        
078600 BCA-CREATE-BO-CODE SECTION.                                              
078700     MOVE 'BCA-CREATE-BO ' TO CURRENT-SECTION                             
078800                                                                          
078900     IF XDCA-KDERS < 20                                                   
079000       IF OHUV-KDORDKL = 0                                                
079100            MOVE 92              TO DC-KDORDBEK(CURR-DC-IX)               
079200                                    XDCA-KDORDBEK                         
079300       ELSE                                                               
079400         IF OHUV-KDORDKL = +1                                             
079500           IF OHUV-FLRESTN = JA AND OHUV-FLPRELRO = JA                    
079600             MOVE 99             TO DC-KDORDBEK(CURR-DC-IX)               
079700                                    XDCA-KDORDBEK                         
079800           END-IF                                                         
079900         END-IF                                                           
080000       END-IF                                                             
080100*PD                                                                       
080200*IDDC-RO IS ON CURRENT-DC I.E FIRST DC                                    
080300       IF XDCA-IDDC-RO = W-IDDC                                           
080400         IF OHUV-KDORDKL > 1                                              
080500            ADD XDCA-KVBEART-Q   TO SLAG-KVOKS-BULK                       
080600         ELSE                                                             
080700            ADD XDCA-KVBEART-Q   TO SLAG-KVOKS-DAG                        
080800         END-IF                                                           
080900         MOVE XDCA-KVBEART-Q     TO XDCA-KVPRERO                          
081000       ELSE                                                               
081100*MOVE STOCK AT HOME DC                                                    
081200*IF IT IS FIRST TIME THEN THE W-DISP-HOME WILL BE 0                       
081300         MOVE WS-AVAILABLE-HOME  TO XDCA-KVPREAVB                         
081400         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
081500                              - WS-AVAILABLE-HOME                         
081600         MOVE XDCA-IDDC-RO       TO W-IDDC                                
081700         PERFORM IMS-GHU-WDK711                                           
081800         IF OHUV-KDORDKL > 1                                              
081900            ADD XDCA-KVBEART-Q   TO SLAG-KVOKS-BULK                       
082000         ELSE                                                             
082100            ADD XDCA-KVBEART-Q   TO SLAG-KVOKS-DAG                        
082200         END-IF                                                           
082300       END-IF                                                             
082400       PERFORM S12-REPL-WDK7                                              
082500       MOVE XDCA-IDDC-RO         TO XDCA-IDDC-OUT                         
082600*PD                                                                       
082700     END-IF                                                               
082800     .                                                                    
082900******************************************************************        
083000* CHECK STOCK FOR EACH ORDERCLASS                                *        
083100******************************************************************        
083200 C-CHECK-AVAILABLE-STOCK SECTION.                                         
083300     MOVE 'C-CHECK-AVAILABLE-STOCK' TO CURRENT-SECTION                    
083400     PERFORM CA-ADJUST-NEG-STOCK-VALUE                                    
083500     PERFORM CB-DECIDE-USE-OF-AKS                                         
083600*STOCK CHECK FOR DIFFRENT ORDER CLASS                                     
083700                                                                          
083800     IF OHUV-KDORDKL = +0                                                 
083900       PERFORM CC-CHECK-STOCK-VOR                                         
084000     END-IF                                                               
084100     IF OHUV-KDORDKL = 1                                                  
084200       PERFORM CD-CHECK-STOCK-DAY                                         
084300     END-IF                                                               
084400                                                                          
084500     IF OHUV-KDORDKL > 1 AND WS-BULK-SW = 'N'                             
084600        PERFORM CE-CHECK-DISP-BULK                                        
084700     END-IF                                                               
084800                                                                          
084900     IF WS-BULK-SW = 'Y'                                                  
085000       PERFORM CF-CHECK-STOCK-BULK-REFERALL                               
085100     END-IF                                                               
085200     .                                                                    
085300******************************************************************        
085400*STOCK VALUE SHOULD NOT BE ZERO                                  *        
085500******************************************************************        
085600 CA-ADJUST-NEG-STOCK-VALUE SECTION.                                       
085700     MOVE 'CA-ADJUST-NEG' TO CURRENT-SECTION                              
085800                                                                          
085900     IF SLAG-KVOKS-DAG < +0                                               
086000       MOVE +0                 TO WS-KVOKS-DAY                            
086100     ELSE                                                                 
086200       MOVE SLAG-KVOKS-DAG     TO WS-KVOKS-DAY                            
086300     END-IF                                                               
086400     IF SLAG-KVOKS-BULK < +0                                              
086500       MOVE +0                 TO WS-KVOKS-BULK                           
086600     ELSE                                                                 
086700       MOVE SLAG-KVOKS-BULK    TO WS-KVOKS-BULK                           
086800     END-IF                                                               
086900     IF SLAG-KVRESS < +0                                                  
087000       MOVE +0                 TO WS-KVRESS                               
087100     ELSE                                                                 
087200       MOVE SLAG-KVRESS        TO WS-KVRESS                               
087300     END-IF                                                               
087400     .                                                                    
087500******************************************************************        
087600*WE HAVE INCLUDE ARRIVAL DAY/BULK IN 4403 AND WE NEED TO USE THEM*        
087700*IN 0510 WE HAVE AKS SO TO INCLUDE THAT AMOUNT OR NOT SHOULD BE  *        
087800*BASED ON VALUE IN 4403                                          *        
087900*0:- NO INCLUDE AKS                                              *        
088000*1:- INCLUDE AKS                                                 *        
088100*2:- SEMI INCLUDE AKS (EG 20 IN AKS INCLUDE ONLY 10              *        
088200******************************************************************        
088300                                                                          
088400 CB-DECIDE-USE-OF-AKS SECTION.                                            
088500     MOVE 'CB-DECIDE-USE-AKS' TO CURRENT-SECTION                          
088600                                                                          
088700     IF OHUV-KDORDKL = 0 OR 1                                             
088800        MOVE DCS-KDAKDISP-DAG  TO WS-KDAKDISP                             
088900     ELSE                                                                 
089000        MOVE DCS-KDAKDISP-BULK TO WS-KDAKDISP                             
089100     END-IF                                                               
089200     IF NO-AKS                                                            
089300        MOVE ZERO              TO WS-KVAKS                                
089400     ELSE                                                                 
089500        IF FULL-AKS                                                       
089600           MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                
089700        ELSE                                                              
089800           PERFORM CAAA-COMPUTE-LIMITED-AKS                               
089900        END-IF                                                            
090000     END-IF                                                               
090100     .                                                                    
090200                                                                          
090300******************************************************************        
090400**2:- SEMI INCLUDE AKS (EG 20 IN AKS INCLUDE ONLY 10)            *        
090500******************************************************************        
090600 CAAA-COMPUTE-LIMITED-AKS SECTION.                                        
090700     MOVE 'CAAA-LIMITED-AKS' TO CURRENT-SECTION                           
090800                                                                          
090900     MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                      
091000     MOVE ZERO           TO WS-RETURNS                                    
091100                                                                          
091200     PERFORM IMS-GU-WDL601                                                
091300     IF SEGMENT-FOUND                                                     
091400        PERFORM IMS-GNP-WDL611                                            
091500        PERFORM UNTIL SEGMENT-MISSING                                     
091600           COMPUTE WS-RETURNS = WS-RETURNS +                              
091700                   INL-KVAVIS - INL-KVANTMOT                              
091800           PERFORM IMS-GNP-WDL611                                         
091900        END-PERFORM                                                       
092000     END-IF                                                               
092100                                                                          
092200     COMPUTE WS-KVAKS = WS-KVAKS - WS-RETURNS                             
092300*    THE AKS-VALUE TO ADD MUST NEVER BE NEGATIVE                          
092400     IF WS-KVAKS < ZERO                                                   
092500        MOVE ZERO TO WS-KVAKS                                             
092600     END-IF                                                               
092700     .                                                                    
092800                                                                          
092900******************************************************************        
093000*CHECK STOCK AVAILABLE FOR VOR ORDER                             *        
093100*AS THIS IS VOR AND HIGH PRIORITY ORDER WE CHECK ALL THE STOCK   *        
093200*ON THE WAY SHOULD BE ASSIGNED TO VOR ORDER                      *        
093300******************************************************************        
093400 CC-CHECK-STOCK-VOR SECTION.                                              
093500     MOVE 'CC-CHECK-STOCK-VOR' TO CURRENT-SECTION                         
093600     IF OHUV-FLFORBI = JA AND CURR-DC-IX = 1                              
093700        PERFORM CCA-SPECIAL-VOR-ORDER                                     
093800     ELSE                                                                 
093900** CODE IF IT IS NOT SPECIAL ORDER                                        
094000        PERFORM S07-CHECK-DISP                                            
094100        IF XDCA-KVBEART-Q > WS-AVAILABLE                                  
094200            IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR          
094300               XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                        
094400                                             = SPACE OR '11'  OR          
094500               XDCA-IDDC-TVS NOT = SPACE                                  
094600               PERFORM S02-FIX-VOR-LINE                                   
094700            ELSE                                                          
094800              MOVE JA          TO CLEAR-SW                                
094900                                  DC-CLEARING (CURR-DC-IX)                
095000              IF XDCA-IDDC-RO = W-IDDC                                    
095100                  MOVE 15      TO DC-KDORDBEK(CURR-DC-IX)                 
095200                                  XDCA-KDORDBEK                           
095300                  MOVE WS-AVAILABLE  TO WS-AVAILABLE-HOME                 
095400              END-IF                                                      
095500            END-IF                                                        
095600        ELSE                                                              
095710            PERFORM S14-UPDATE-STOCK                                      
095800        END-IF                                                            
095900     END-IF                                                               
096000     .                                                                    
096100******************************************************************        
096200*VOR  1 ORDERS CREATED FROM 4221                            *             
096300*CHECKING STOCK FROM ALL THE IN TRANSIT AND ALL             *             
096400******************************************************************        
096500 CCA-SPECIAL-VOR-ORDER   SECTION.                                         
096600         MOVE 'CCA-SPECIAL-VOR   ' TO CURRENT-SECTION                     
096700         IF SLAG-KVAKS-SDC > +0 OR SLAG-KVSPARR-KVAL > +0                 
096800            COMPUTE WS-AVAILABLE = SLAG-KVLS                              
096900                                 - SLAG-KVSPARR-KVAL                      
097000            IF WS-AVAILABLE < ZERO                                        
097100               MOVE ZERO TO WS-AVAILABLE                                  
097200            END-IF                                                        
097300            PERFORM S01-ANPASSA-DISP-TILL-KVANT                           
097400*CHECK THE NEW STOCK                                                      
097500            IF XDCA-KVBEART-Q > WS-AVAILABLE                              
097600              PERFORM S02-FIX-VOR-LINE                                    
097700            ELSE                                                          
097800              PERFORM S14-UPDATE-STOCK                                    
097900            END-IF                                                        
098000         ELSE                                                             
098100              PERFORM S14-UPDATE-STOCK                                    
098200         END-IF                                                           
098300      .                                                                   
098400******************************************************************        
098500*CHECK STOCK AVAILABLE FOR DAY ORDER                             *        
098600*ON FIRST DC WE ALWAYS CHECK IF THERE IS A REFILL ORDER          *        
098700******************************************************************        
098800 CD-CHECK-STOCK-DAY SECTION.                                              
098900      MOVE 'CD-CHECK-STOCK-DAY' TO CURRENT-SECTION                        
099000      PERFORM S07-CHECK-DISP                                              
099100      IF CURR-DC-IX = 1                                                   
099200         IF XDCA-KVBEART-Q > WS-AVAILABLE                                 
099300            PERFORM S03-CHECK-NEXT-REFILL                                 
099400         END-IF                                                           
099500           IF XDCA-KVBEART-Q > (WS-AVAILABLE + WS-KVAVIS)                 
099600             IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR         
099700                XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                       
099800                                              = SPACE OR '11'  OR         
099900                LART-FLREFERAL = JA                            OR         
099910                XDCA-IDDC-TVS NOT = SPACE                                 
100000                PERFORM S04-FIXA-DAY-RAD                                  
100100             ELSE                                                         
100200                MOVE JA           TO CLEAR-SW                             
100300                                     DC-CLEARING(CURR-DC-IX)              
100400                MOVE 15           TO DC-KDORDBEK(CURR-DC-IX)              
100500                                     XDCA-KDORDBEK                        
100600                MOVE WS-AVAILABLE TO WS-AVAILABLE-HOME                    
100700             END-IF                                                       
100800           ELSE                                                           
100900              PERFORM S14-UPDATE-STOCK                                    
101000           END-IF                                                         
101100      ELSE                                                                
101200         IF XDCA-KVBEART-Q > WS-AVAILABLE                                 
101300             IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR         
101400                XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                       
101500                                              = SPACE OR '11'             
101600                 PERFORM S04-FIXA-DAY-RAD                                 
101700             ELSE                                                         
101800                 MOVE JA            TO CLEAR-SW                           
101900                                       DC-CLEARING(CURR-DC-IX)            
102000               IF XDCA-IDDC-RO = W-IDDC                                   
102100                 MOVE 15            TO DC-KDORDBEK(CURR-DC-IX)            
102200                                       XDCA-KDORDBEK                      
102300                 MOVE WS-AVAILABLE  TO WS-AVAILABLE-HOME                  
102400               END-IF                                                     
102500             END-IF                                                       
102600         ELSE                                                             
102700            PERFORM S14-UPDATE-STOCK                                      
102800         END-IF                                                           
102900      END-IF                                                              
103000      .                                                                   
103100******************************************************************        
103200*CHECK STOCK AVAILABLE FOR   BULK ORDER                      *            
103300******************************************************************        
103400 CE-CHECK-DISP-BULK     SECTION.                                          
103500     MOVE 'CE-CHECK-DISP-BULK'  TO CURRENT-SECTION                        
103600*   IN NDCA THERE IS DIFFRENT LOGIC FOR US/CN BUT WE KEPT ONE             
103700*   LOGIC FOR BULK IN XDCA                                                
103800     IF XDCA-KDERS > ZERO                                                 
103900         PERFORM S07-CHECK-DISP                                           
104000       IF (XDCA-KDERS > ZERO AND WS-AVAILABLE > +0) OR                    
104100          (XDCA-KVBEART-Q   <  WS-AVAILABLE)                              
104200             PERFORM S14-UPDATE-STOCK                                     
104300       ELSE                                                               
104400          IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR            
104500             XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                          
104600                                           = SPACE OR '11'  OR            
104700             LART-FLREFERAL = JA                            OR            
104710             XDCA-IDDC-TVS NOT = SPACE                                    
104800             PERFORM S05-FIXA-BULK-RAD                                    
104900                                                                          
105000          ELSE                                                            
105100             MOVE JA             TO CLEAR-SW                              
105200                                    DC-CLEARING(CURR-DC-IX)               
105300             IF XDCA-IDDC-RO = W-IDDC                                     
105400                MOVE 15           TO DC-KDORDBEK(CURR-DC-IX)              
105500                                     XDCA-KDORDBEK                        
105600                MOVE WS-AVAILABLE TO WS-AVAILABLE-HOME                    
105700              END-IF                                                      
105800          END-IF                                                          
105900       END-IF                                                             
106000     ELSE                                                                 
106100         PERFORM S07-CHECK-DISP                                           
106200         IF XDCA-KVBEART-Q < WS-AVAILABLE                                 
106300             PERFORM S14-UPDATE-STOCK                                     
106400         ELSE                                                             
106500             PERFORM S05-FIXA-BULK-RAD                                    
106600         END-IF                                                           
106700     END-IF                                                               
106800     .                                                                    
106900******************************************************************        
107000*CHECK STOCK AVAILABLE FOR BULK ORDER WITH REFERRAL            *          
107100******************************************************************        
107200 CF-CHECK-STOCK-BULK-REFERALL SECTION.                                    
107300      MOVE 'CF-BULK-REFERALL' TO CURRENT-SECTION                          
107400      PERFORM S07-CHECK-DISP                                              
107500      IF XDCA-KVBEART-Q > WS-AVAILABLE                                    
107600         IF CURR-DC-IX = IX-DCCLEAR-MAX                    OR             
107700            XDCA-IDDC-CLEAR-IN (CURR-DC-IX + 1)                           
107800                                          = SPACE OR '11'  OR             
107900            LART-FLREFERAL = JA                            OR             
107910            XDCA-IDDC-TVS NOT = SPACE                                     
108000*IF   REFERRAL BLOCK THEN DO NOT PROCESS FURTHER               *          
108100            PERFORM S05-FIXA-BULK-RAD                                     
108200                                                                          
108300         ELSE                                                             
108400             MOVE JA        TO CLEAR-SW                                   
108500                               DC-CLEARING(CURR-DC-IX)                    
108600             IF XDCA-IDDC-RO = W-IDDC                                     
108700                 MOVE 15           TO DC-KDORDBEK(CURR-DC-IX)             
108800                                      XDCA-KDORDBEK                       
108900                 MOVE WS-AVAILABLE TO WS-AVAILABLE-HOME                   
109000             END-IF                                                       
109100         END-IF                                                           
109200      ELSE                                                                
109300         PERFORM S14-UPDATE-STOCK                                         
109400      END-IF                                                              
109500      .                                                                   
109600******************************************************************        
109700*FOR ALL THE UPDATE WE DO IN THE PROGRAM WE ARE UPDATING THE              
109800*WORKING STORAGE TABLE SO THAT WE CAN USE  IT HERE               *        
109900*THESE ARE USED IN REFILL SYSTEM AND THEY WANT ONLY TWO DC       *        
110000*HENCE CLEARING MORE THEN 2 DC                                   *        
110100******************************************************************        
110200                                                                          
110300 D-CLEARING-TABLE-UPDATE  SECTION.                                        
110400     MOVE 'D-CLEAR-TABLE '   TO CURRENT-SECTION                           
110500     IF OHUV-KDORDING < 3                                                 
110600      IF DIST35-REFILL-INOM-NDC OR DIST35-NONVCC-REFILL                   
110610      OR DIST35-NONVCC-NONVCC-TRANSFER                                    
110620      OR DIST35-NONVCC-VCC-TRANSFER                                       
110700         MOVE 'RE'           TO XDCA-KDOI                                 
110800         MOVE SPACE          TO DC-FLLF(CURR-DC-IX)                       
110900         MOVE SPACE          TO DC-FLCLEAR(CURR-DC-IX)                    
111000                                                                          
111100      ELSE                                                                
111200         MOVE 'XX'           TO XDCA-KDOI                                 
111300         IF   WS-PART-VALID = 'N'                                         
111400              MOVE 'N'       TO DC-FLLF(CURR-DC-IX)                       
111500         ELSE                                                             
111600              MOVE SLAG-FLREFILL                                          
111700                             TO DC-FLLF(CURR-DC-IX)                       
111800         END-IF                                                           
111900         IF CLEAR-SW = 'J'                                                
112000             MOVE JA         TO DC-FLCLEAR(CURR-DC-IX)                    
112100         ELSE                                                             
112200             MOVE NEJ        TO DC-FLCLEAR(CURR-DC-IX)                    
112300         END-IF                                                           
112400       END-IF                                                             
112500     END-IF                                                               
112600                                                                          
112700     .                                                                    
112800******************************************************************        
112900*ORDERHIT IS USED IN REFILL SECTION BASED ON ORDERHIT THEY FORCAS*        
113000*THE NEXT REFILL                                                 *        
113100*ORDERHIT: AMOUNT OF HIT                                         *        
113200*ORDERINTAKE :- NO OF PIECES                                     *        
113300*WE SEND KDOI HERE   AND IN 2109 PROGRAM ORDERINTAKE IS CALCULAT *        
113400******************************************************************        
113500 F-ORDERHIT    SECTION.                                                   
113600     MOVE 'F-ORDERHIT'       TO CURRENT-SECTION                           
113700     IF OHUV-KDORDING = 3                                                 
113800       MOVE SPACE            TO XDCA-KDOI                                 
113900     ELSE                                                                 
114000       IF XDCA-KDOI NOT = 'RE'                                            
114100          IF XDCA-KDOI = 'XX'                                             
114200            PERFORM FA-CHECK-KDOI                                         
114300          END-IF                                                          
114400       END-IF                                                             
114500     END-IF                                                               
114600                                                                          
114700     .                                                                    
114800                                                                          
114900******************************************************************        
115000*SENDING ONLY TWO DC FIRST SHOULD BE A FIRST HOME  DC            *        
115100*SECOND    SHOULD BE WHERE ORDER IS ALLOCATED                    *        
115200******************************************************************        
115300 FA-CHECK-KDOI   SECTION.                                                 
115400     MOVE 'FA-KOLLA-KDOI   ' TO CURRENT-SECTION                           
115500                                                                          
115600     MOVE XDCA-IDARTNR       TO W-IDARTNR                                 
115700     MOVE XDCA-IDDC-CLEAR-IN(1) TO FIRST-DC                               
115800                                                                          
115900     IF FIRST-DC NOT = DCS-IDDC                                           
116000        MOVE FIRST-DC  TO W-IDDC                                          
116100        PERFORM IMS-GU-WDB601                                             
116200     END-IF                                                               
116300                                                                          
116400                                                                          
116500     IF DCS-NDC-PF OR DCS-NDC-CN                                          
116600        PERFORM IMS-GU-WDK711                                             
116700        IF SEGMENT-FOUND                                                  
116800           IF SLAG-IDLEVNR NOT = '1441 '                                  
116900              MOVE 'LO'      TO XDCA-KDOI                                 
117000           END-IF                                                         
117100        END-IF                                                            
117200     ELSE                                                                 
117300        IF DCS-NDC-NA                                                     
117400           MOVE 'LO'         TO XDCA-KDOI                                 
117500                                                                          
117600           PERFORM IMS-GU-WDK701                                          
117700           IF SEGMENT-FOUND                                               
117800              PERFORM IMS-GNP-WDK711-1441                                 
117900              PERFORM UNTIL SEGMENT-MISSING                               
118000                   OR XDCA-KDOI = 'XX'                                    
118100                 MOVE SLAG-IDDC TO WS-IDDC                                
118200                 IF NDC-NA                                                
118300                    MOVE 'XX'   TO XDCA-KDOI                              
118400                 ELSE                                                     
118500                    PERFORM IMS-GNP-WDK711-1441                           
118600                 END-IF                                                   
118700              END-PERFORM                                                 
118800           END-IF                                                         
118900        ELSE                                                              
119000           IF DCS-CHINA                                                   
119100              MOVE 'LO'         TO XDCA-KDOI                              
119200                                                                          
119300              PERFORM IMS-GU-WDK701                                       
119400              IF SEGMENT-FOUND                                            
119500                 PERFORM IMS-GNP-WDK711-1441                              
119600                 PERFORM UNTIL SEGMENT-MISSING                            
119700                      OR XDCA-KDOI = 'XX'                                 
119800                    MOVE SLAG-IDDC TO WS-IDDC                             
119900                    IF NDC-CN                                             
120000                       MOVE 'XX'   TO XDCA-KDOI                           
120100                    ELSE                                                  
120200                       PERFORM IMS-GNP-WDK711-1441                        
120300                    END-IF                                                
120400                 END-PERFORM                                              
120500              END-IF                                                      
120600           END-IF                                                         
120700        END-IF                                                            
120800     END-IF                                                               
120900*FIRST DC SHOULD ALWAYS BE MOVED EVEN IF VALID OR NOT                     
120910     INITIALIZE XDCA-CLEARGROUP                                           
120920     MOVE NEJ                        TO DC-2-WRITTEN-SW                   
120930                                                                          
121000     MOVE  1                         TO DC-IX                             
121100     PERFORM UNTIL DC-IX > IX-DCCLEAR-MAX                                 
121110                OR DC-2-WRITTEN                                           
121200     IF DC-IX = 1                                                         
121300       MOVE DC-IDDC (1)              TO XDCA-IDDC-CLEAR(1)                
121400       MOVE DC-FLLF (1)              TO XDCA-FLLF(1)                      
121500       IF DC-FLCLEAR(1)= 'J'                                              
121600          MOVE JA                    TO XDCA-FLCLEAR(1)                   
121700       ELSE                                                               
121800          MOVE DC-FLCLEAR(1)         TO XDCA-FLCLEAR (1)                  
121900       END-IF                                                             
122000     ELSE                                                                 
122100*FOR SECOND DC WE SHOULD MOVE THE ALLOCATED DC TO CLEARING DC             
122200                                                                          
122300       IF XDCA-IDDC-OUT  = DC-IDDC(DC-IX)                                 
122400          MOVE  +2                   TO  CLEAR-IX                         
122500          MOVE DC-IDDC (DC-IX)       TO XDCA-IDDC-CLEAR(CLEAR-IX)         
122600          MOVE DC-FLLF (DC-IX)       TO XDCA-FLLF(CLEAR-IX)               
122700          MOVE DC-FLCLEAR(DC-IX)     TO XDCA-FLCLEAR(CLEAR-IX)            
122710          MOVE YES                   TO DC-2-WRITTEN-SW                   
122800       END-IF                                                             
122900     END-IF                                                               
123000     ADD +1                          TO DC-IX                             
123100     END-PERFORM                                                          
123110*RC COMMENTED THE BELOW CODE AS WE INITIALIZE XDCA-CLEARGROUP             
123200*RC  IF DC-IX > 2  AND DC-IX < 8                                          
123300*RC     MOVE SPACE                   TO XDCA-IDDC-CLEAR(DC-IX)            
123400*RC                                     XDCA-FLLF(DC-IX)                  
123500*RC                                     XDCA-FLCLEAR(DC-IX)               
123600*RC  END-IF                                                               
123700     .                                                                    
123800                                                                          
123900                                                                          
124000                                                                          
124100 E-NEW-LOCAL-TIME SECTION.                                                
124200         MOVE 'E-NEW-LOCAL-TIME' TO CURRENT-SECTION                       
124300                                                                          
124400         MOVE XDCA-IDDC-OUT      TO WS-IDDC                               
124500         MOVE ALL '+'            TO MSGI-WMSGINIT                         
124600         MOVE '013'              TO MSGI-KDCALL                           
124700         MOVE 'WIDDC   '         TO MSGI-IDUSER                           
124800         MOVE XDCA-IDDC-OUT      TO MSGI-IDUSER (6:2)                     
124900         MOVE 'W411'             TO MSGI-IDTRANS                          
125000                                                                          
125100         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
125200                                                                          
125300         MOVE MSGI-TILOKDAT      TO XDCA-TIREGDAT-OUT                     
125400         MOVE MSGI-TILOKTID      TO WS-TIHHMM                             
125500         MOVE WS-TIHHMMSS        TO XDCA-TIREGTID-OUT                     
125600         .                                                                
125700                                                                          
125800                                                                          
125900                                                                          
126000                                                                          
126100                                                                          
126200 S04-FIXA-DAY-RAD SECTION.                                                
126300     MOVE 'S04-FIXA-DAY-RAD' TO CURRENT-SECTION                           
126400                                                                          
126500     IF XDCA-KDERS < 20                                                   
126600       IF XDCA-IDDC-RO = W-IDDC                                           
126700         IF OHUV-FLRESTN = JA AND OHUV-FLPRELRO = JA                      
126800           MOVE 99               TO DC-KDORDBEK(CURR-DC-IX)               
126900                                    XDCA-KDORDBEK                         
127000         ELSE                                                             
127100           IF LART-FLREFERAL = JA                                         
127200              MOVE 98            TO DC-KDORDBEK(CURR-DC-IX)               
127300                                    XDCA-KDORDBEK                         
127400           END-IF                                                         
127500         END-IF                                                           
127600         MOVE WS-AVAILABLE       TO XDCA-KVPREAVB                         
127700         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
127800                              - WS-AVAILABLE                              
127900       ELSE                                                               
128000         MOVE WS-AVAILABLE-HOME  TO XDCA-KVPREAVB                         
128100         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
128200                              - WS-AVAILABLE-HOME                         
128300         IF OHUV-FLRESTN = JA AND OHUV-FLPRELRO = JA                      
128400           MOVE 99               TO DC-KDORDBEK(CURR-DC-IX)               
128500                                    XDCA-KDORDBEK                         
128600         ELSE                                                             
128700           IF LART-FLREFERAL = JA                                         
128800              MOVE 98            TO DC-KDORDBEK(CURR-DC-IX)               
128900                                    XDCA-KDORDBEK                         
129000           ELSE                                                           
129100             IF XDCA-IDDC-RO = XDCA-IDDC-CLEAR-IN(1)                      
129200                                                                          
129300               MOVE ZERO         TO DC-KDORDBEK(CURR-DC-IX)               
129400                                    XDCA-KDORDBEK                         
129500             END-IF                                                       
129600           END-IF                                                         
129700         END-IF                                                           
129800         MOVE XDCA-IDDC-RO       TO W-IDDC                                
129900         PERFORM IMS-GHU-WDK711                                           
130000       END-IF                                                             
130100       ADD XDCA-KVBEART-Q        TO SLAG-KVOKS-DAG                        
130200       PERFORM S12-REPL-WDK7                                              
130300       MOVE XDCA-IDDC-RO         TO XDCA-IDDC-OUT                         
130400     END-IF                                                               
130500     .                                                                    
130600******************************************************************        
130700*NEED TO CALCULATE STOCK FOR EACH ORDERCLASS                     *        
130800******************************************************************        
130900                                                                          
131000                                                                          
131100 S07-CHECK-DISP SECTION.                                                  
131200     IF OHUV-KDORDKL = 0                                                  
131300        COMPUTE WS-AVAILABLE = SLAG-KVLS                                  
131400                             + WS-KVAKS                                   
131500                             - WS-KVOKS-DAY                               
131600                             - SLAG-KVUTRS                                
131700                             - SLAG-KVSPARR-KVAL                          
131800     ELSE                                                                 
131900                                                                          
132000        COMPUTE WS-AVAILABLE = SLAG-KVLS                                  
132100                             + WS-KVAKS                                   
132200                             - WS-KVOKS-DAY                               
132300                             - SLAG-KVUTRS                                
132400                             - SLAG-KVSPARR-KVAL                          
132500                             - WS-KVRESS                                  
132600                             - WS-KVSPANT                                 
132700        IF OHUV-KDORDKL > +1                                              
132800           COMPUTE WS-AVAILABLE = WS-AVAILABLE                            
132900                                - WS-KVOKS-BULK                           
133000        END-IF                                                            
133100     END-IF                                                               
133200     IF WS-AVAILABLE < ZERO                                               
133300        MOVE ZERO      TO WS-AVAILABLE                                    
133400     END-IF                                                               
133500     PERFORM S01-ANPASSA-DISP-TILL-KVANT                                  
133600     .                                                                    
133700******************************************************************        
133800*SORTING AND QTY IN BULK-PACK                                 *           
133900******************************************************************        
134000 S01-ANPASSA-DISP-TILL-KVANT SECTION.                                     
134100     MOVE 'S01-ANPASSA-DISP' TO CURRENT-SECTION                           
134200                                                                          
134300     IF XDCA-KDSORT    = 'L ' AND                                         
134400        XDCA-KVQPACK-1 > ZERO                                             
134500                                                                          
134600       IF WS-AVAILABLE  < XDCA-KVBEART-Q                                  
134700          COMPUTE WS-DISP-KVANT =                                         
134800                  WS-AVAILABLE / XDCA-KVQPACK-1                           
134900          COMPUTE WS-DISP-KVANT =                                         
135000                  WS-DISP-KVANT * XDCA-KVQPACK-1                          
135100          MOVE WS-DISP-KVANT TO WS-AVAILABLE                              
135200       END-IF                                                             
135300     END-IF                                                               
135400     .                                                                    
135500******************************************************************        
135600*IF ORDERD QTY IS MORE THEN AVAILABLE WE CREATE BO CODE 98/92   *         
135700******************************************************************        
135800                                                                          
135900 S02-FIX-VOR-LINE SECTION.                                                
136000     MOVE 'S02-FIXA-VOR-LIN' TO CURRENT-SECTION                           
136100                                                                          
136200     IF XDCA-KDERS < 20                                                   
136300* IDDC-RO IS SET ON FIRST-DC                                              
136400       IF XDCA-IDDC-RO = W-IDDC                                           
136500         MOVE WS-AVAILABLE       TO XDCA-KVPREAVB                         
136600         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
136700                              - WS-AVAILABLE                              
136800       ELSE                                                               
136900         MOVE XDCA-IDDC-RO       TO W-IDDC                                
137000         PERFORM IMS-GHU-WDK711                                           
137100         MOVE WS-AVAILABLE-HOME  TO XDCA-KVPREAVB                         
137200         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
137300                              - WS-AVAILABLE-HOME                         
137400       END-IF                                                             
137500       ADD XDCA-KVBEART-Q        TO SLAG-KVOKS-DAG                        
137600       PERFORM S12-REPL-WDK7                                              
137700       MOVE 92                   TO DC-KDORDBEK(CURR-DC-IX)               
137800                                    XDCA-KDORDBEK                         
137900       MOVE XDCA-IDDC-RO         TO XDCA-IDDC-OUT                         
138000     END-IF                                                               
138100     .                                                                    
138200                                                                          
138300 S05-FIXA-BULK-RAD SECTION.                                               
138400     MOVE 'S05-FIX-BULK-RAD' TO CURRENT-SECTION                           
138500     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
138600                                                                          
138700     IF XDCA-KDERS < 20  OR (XDCA-KDERS> 20 AND (DIST35-REFILL            
138800                         OR  DIST35-REFILL-INOM-NDC                       
138900                         OR  DIST35-NONVCC-REFILL                         
138910                         OR  DIST35-NONVCC-NONVCC-TRANSFER                
138920                         OR  DIST35-NONVCC-VCC-TRANSFER                   
139000                         OR  DIST35-RETUR                                 
139100                         OR  DIST18-SKROT                                 
139200                         OR  DIST18-SCRAP-NDC))                           
139300       IF XDCA-IDDC-RO = W-IDDC                                           
139400         ADD XDCA-KVBEART-Q     TO SLAG-KVOKS-BULK                        
139500         PERFORM S12-REPL-WDK7                                            
139600         MOVE WS-AVAILABLE      TO XDCA-KVPREAVB                          
139700         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
139800                              - WS-AVAILABLE                              
139900       ELSE                                                               
140000         MOVE XDCA-IDDC-RO      TO W-IDDC                                 
140100         PERFORM IMS-GHU-WDK711                                           
140200         ADD XDCA-KVBEART-Q     TO SLAG-KVOKS-BULK                        
140300         PERFORM S12-REPL-WDK7                                            
140400         MOVE WS-AVAILABLE-HOME TO XDCA-KVPREAVB                          
140500         COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                            
140600                              - WS-AVAILABLE-HOME                         
140700       END-IF                                                             
140800       IF XDCA-IDDC-RO = XDCA-IDDC-CLEAR-IN(1)                            
140900                                                                          
141000         MOVE ZERO              TO DC-KDORDBEK(CURR-DC-IX)                
141100                                   XDCA-KDORDBEK                          
141200       END-IF                                                             
141300       MOVE XDCA-IDDC-RO        TO XDCA-IDDC-OUT                          
141400     END-IF                                                               
141500     .                                                                    
141600******************************************************************        
141700*FOR CLASS 1 ORDER WE READ THE LEAD TIME ON SCREEN 4414 HENCE    *        
141800*ONLY FOR CLASS 1 NOT FOR BULK                                   *        
141900******************************************************************        
142000 S03-CHECK-NEXT-REFILL SECTION.                                           
142100     MOVE 'S03-CHECK-REFILL' TO CURRENT-SECTION                           
142200                                                                          
142300     PERFORM IMS-GU-WDL601                                                
142400     IF SEGMENT-FOUND                                                     
142500       PERFORM IMS-GNP-WDL611-UNQUAL                                      
142600       IF SEGMENT-FOUND                                                   
142700         PERFORM UNTIL SEGMENT-MISSING                                    
142800         MOVE INL-TIBERANK    TO TMP1-YYMMDD                              
142900         MOVE WS-TIREGDAT     TO TMP2-YYMMDD                              
143000         MOVE WS-TIBERANK     TO TMP3-YYMMDD                              
143100         PERFORM WY2000Q1                                                 
143200           IF INL-IDDC = W-IDDC                 AND                       
143300             (INL-IDPTYP = '310' OR 'R30')      AND                       
143400             (TMP1-YYMMDD  >= TMP2-YYMMDD  )    AND                       
143500              TMP1-YYMMDD  <  TMP3-YYMMDD                                 
143600                                                                          
143700             MOVE INL-TIBERANK TO WS-TIBERANK                             
143800             MOVE INL-KVAVIS   TO WS-KVAVIS                               
143900           END-IF                                                         
144000           PERFORM IMS-GNP-WDL611-UNQUAL                                  
144100         END-PERFORM                                                      
144200       END-IF                                                             
144300     END-IF                                                               
144400     MOVE WS-TIBERANK   TO TMP1-YYMMDD                                    
144500     MOVE 999999        TO TMP2-YYMMDD                                    
144600     PERFORM WY2000P1                                                     
144700     IF TMP1-YYMMDD > ZERO AND TMP1-YYMMDD < TMP2-YYMMDD                  
144800       MOVE 001                  TO DAG-KDCALL                            
144900       MOVE XDCA-TIREGDAT        TO DAG-TIAAMMDD-FOM                      
145000       MOVE WS-TIBERANK          TO DAG-TIAAMMDD-TOM                      
145100                                                                          
145200       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
145300                           DAG-KDSVAR                                     
145400                                                                          
145500       IF DAG-KDSVAR = 'F'                                                
145600         MOVE 'ERROR FROM DAGKONV CCE-SECTION' TO ERRORTEXT               
145700         CALL ABEND USING RKOD-ABEND-WITHOUT-DUMP                         
145800       ELSE                                                               
145900         IF DAG-KVKALDAG NOT > OHUV-KVDAGAR-DOW                           
146000           MOVE 99              TO DC-KDORDBEK(CURR-DC-IX)                
146100                                   XDCA-KDORDBEK                          
146200         ELSE                                                             
146300           MOVE ZERO TO WS-KVAVIS                                         
146400         END-IF                                                           
146500       END-IF                                                             
146600     END-IF                                                               
146700     .                                                                    
146800******************************************************************        
146900*WHEN WE FIND THE STOCK WE UPDATE KVOKS IN WDK7                  *        
147000******************************************************************        
147100 S14-UPDATE-STOCK         SECTION.                                        
147200     MOVE 'S14-UPDATE-STOCK  ' TO CURRENT-SECTION                         
147300                                                                          
147400     IF OHUV-KDORDKL = 1 OR 0                                             
147500        ADD XDCA-KVBEART-Q TO SLAG-KVOKS-DAG                              
147600     ELSE                                                                 
147700        ADD XDCA-KVBEART-Q TO SLAG-KVOKS-BULK                             
147800     END-IF                                                               
147900     MOVE XDCA-KVBEART-Q   TO XDCA-KVPREAVB                               
148000     PERFORM S12-REPL-WDK7                                                
148100     IF WS-KVAVIS > ZERO AND OHUV-KDORDKL > 0 AND CURR-DC-IX = 1          
148200       MOVE WS-AVAILABLE  TO XDCA-KVPREAVB                                
148300       COMPUTE XDCA-KVPRERO = XDCA-KVBEART-Q                              
148400                            - WS-AVAILABLE                                
148500     END-IF                                                               
148600     MOVE W-IDDC          TO XDCA-IDDC-OUT                                
148700     MOVE 'Y'             TO DC-STOCK-AVAILABLE(CURR-DC-IX)               
148800                                                                          
148900                                                                          
149000     .                                                                    
149100******************************************************************        
149200*AFTER ALLOCATING LINE WE MUST UPDATE STOCK ON WDK7              *        
149300******************************************************************        
149400                                                                          
149500 S12-REPL-WDK7 SECTION.                                                   
149600       MOVE 'S12-REPL-WDK7   ' TO CURRENT-SECTION                         
149700                                                                          
149800       IF XDCA-KDCALL = +1                                                
149900          PERFORM IMS-REPL-WDK711                                         
150000          MOVE SLAG-KVOKS-DAG TO XDCA-KVOKS-DAG                           
150100          MOVE SLAG-KVOKS-BULK TO XDCA-KVOKS-BULK                         
150200       END-IF                                                             
150300       IF XDCA-KDCALL = +4                                                
150400          MOVE SLAG-KVOKS-DAG TO XDCA-KVOKS-DAG                           
150500          MOVE SLAG-KVOKS-BULK TO XDCA-KVOKS-BULK                         
150600       END-IF                                                             
150700                                                                          
150800     .                                                                    
150900******************************************************************        
151000*ALL IMS CALLS                                                   *        
151100******************************************************************        
151200                                                                          
151300 IMS-GU-WDB601 SECTION.                                                   
151400     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
151500                                                                          
151600     MOVE SPACE               TO ALL-SSA                                  
151700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
151800          DELIMITED BY SIZE INTO SSA1                                     
151900     MOVE '  '                TO GOOD-STATUSCODES                         
152000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
152100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
152200     PERFORM IMS-STATUSCHECK                                              
152300     .                                                                    
152400                                                                          
152500 IMS-GU-WDB616 SECTION.                                                   
152600     MOVE 'IMS-GU-WDB616   ' TO CURRENT-IMS-SECTION                       
152700                                                                          
152800     MOVE SPACE               TO ALL-SSA                                  
152900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
153000          DELIMITED BY SIZE INTO SSA1                                     
153100     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
153200          DELIMITED BY SIZE INTO SSA2                                     
153300     MOVE '  GE'              TO GOOD-STATUSCODES                         
153400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
153500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
153600     PERFORM IMS-STATUSCHECK                                              
153700     .                                                                    
153800                                                                          
153900                                                                          
154000 IMS-GU-WDK611 SECTION.                                                   
154100     MOVE 'IMS-GU-WDK611   ' TO CURRENT-IMS-SECTION                       
154200                                                                          
154300     MOVE SPACE               TO ALL-SSA                                  
154400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
154700          DELIMITED BY SIZE INTO SSA2                                     
154800     MOVE '  GE'              TO GOOD-STATUSCODES                         
154900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
155000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
155100     PERFORM IMS-STATUSCHECK                                              
155200     .                                                                    
155300                                                                          
155400 IMS-GU-WDK701 SECTION.                                                   
155500     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
155600                                                                          
155700     MOVE SPACE               TO ALL-SSA                                  
155800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
155900          DELIMITED BY SIZE INTO SSA1                                     
156000     MOVE '  GE'              TO GOOD-STATUSCODES                         
156100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1                    
156200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
156300     PERFORM IMS-STATUSCHECK                                              
156400     .                                                                    
156500                                                                          
156600 IMS-GHU-WDK711 SECTION.                                                  
156700     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
156800                                                                          
156900     MOVE SPACE               TO ALL-SSA                                  
157000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
157100          DELIMITED BY SIZE INTO SSA1                                     
157200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
157300          DELIMITED BY SIZE INTO SSA2                                     
157400     MOVE '  GE'              TO GOOD-STATUSCODES                         
157500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
157600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
157700     PERFORM IMS-STATUSCHECK                                              
157800     .                                                                    
157900                                                                          
158000 IMS-GU-WDK711 SECTION.                                                   
158100     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
158200                                                                          
158300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
158600          DELIMITED BY SIZE INTO SSA2                                     
158700     MOVE '  GE' TO GOOD-STATUSCODES                                      
158800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
158900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
159000     PERFORM IMS-STATUSCHECK                                              
159100     .                                                                    
159200     .                                                                    
159300                                                                          
159400 IMS-GNP-WDK711-1441 SECTION.                                             
159500     MOVE 'IMS-GNP-WDK711  ' TO CURRENT-IMS-SECTION                       
159600                                                                          
159700     MOVE SPACE               TO ALL-SSA                                  
159800     STRING 'WDK711  (IDDC     =' W-IDDC-X                                
159900                    '&IDLEVNR  =' W-IDLEVNR-1441-X ')'                    
160000          DELIMITED BY SIZE INTO SSA1                                     
160100     MOVE '  GE'              TO GOOD-STATUSCODES                         
160200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
160300     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
160400     PERFORM IMS-STATUSCHECK                                              
160500     .                                                                    
160600                                                                          
160700 IMS-GU-WDK712 SECTION.                                                   
160800     MOVE 'IMS-GU-WDK712   ' TO CURRENT-IMS-SECTION                       
160900                                                                          
161000     MOVE SPACE               TO ALL-SSA                                  
161100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
161200          DELIMITED BY SIZE INTO SSA1                                     
161300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
161400          DELIMITED BY SIZE INTO SSA2                                     
161500     MOVE '  GE'              TO GOOD-STATUSCODES                         
161600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
161700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
161800     PERFORM IMS-STATUSCHECK                                              
161900     .                                                                    
162000                                                                          
162100 IMS-GU-WDK722 SECTION.                                                   
162200     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
162300                                                                          
162400     MOVE SPACE               TO ALL-SSA                                  
162500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
162600          DELIMITED BY SIZE INTO SSA1                                     
162700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
162800          DELIMITED BY SIZE INTO SSA2                                     
162900     MOVE 'WDK722 '           TO SSA3                                     
163000     MOVE '  GE'              TO GOOD-STATUSCODES                         
163100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
163200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
163300     PERFORM IMS-STATUSCHECK                                              
163400     .                                                                    
163500                                                                          
163600 IMS-REPL-WDK711 SECTION.                                                 
163700     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
163800                                                                          
163900     MOVE SPACE              TO ALL-SSA                                   
164000     MOVE '    '             TO GOOD-STATUSCODES                          
164100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
164200     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
164300     PERFORM IMS-STATUSCHECK                                              
164400     .                                                                    
164500                                                                          
164600                                                                          
164700 IMS-GU-WDK901                 SECTION.                                   
164800     MOVE 'IMS-GU-WDK901   ' TO CURRENT-IMS-SECTION                       
164900                                                                          
165000     MOVE SPACE              TO ALL-SSA                                   
165100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
165200            DELIMITED BY SIZE INTO SSA1                                   
165300     MOVE '  GE'             TO GOOD-STATUSCODES                          
165400     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
165500     MOVE WDK9-STATUS-CODE   TO STATUS-WS                                 
165600     PERFORM IMS-STATUSCHECK                                              
165700     .                                                                    
165800                                                                          
165900                                                                          
166000 IMS-GU-WDL601 SECTION.                                                   
166100     MOVE 'IMS-GU-WDL601   ' TO CURRENT-IMS-SECTION                       
166200                                                                          
166300     MOVE SPACE              TO ALL-SSA                                   
166400     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
166500          DELIMITED BY SIZE INTO SSA1                                     
166600     MOVE '  GE'              TO GOOD-STATUSCODES                         
166700     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
166800     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
166900     PERFORM IMS-STATUSCHECK                                              
167000     .                                                                    
167100                                                                          
167200                                                                          
167300 IMS-GNP-WDL611 SECTION.                                                  
167400     MOVE 'IMS-GNP-WDL611  '  TO CURRENT-IMS-SECTION                      
167500                                                                          
167600     MOVE SPACE              TO ALL-SSA                                   
167700     STRING 'WDL611  (IDPTYP   =' W-IDPTYP-X                              
167800                    '&KDRT     =' W-KDRT-07-X                             
167900                    '!IDPTYP   =' W-IDPTYP-X                              
168000                    '&KDRT     =' W-KDRT-77-X ')'                         
168100          DELIMITED BY SIZE INTO SSA1                                     
168200     MOVE '  GE'              TO GOOD-STATUSCODES                         
168300     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
168400     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
168500     PERFORM IMS-STATUSCHECK                                              
168600     .                                                                    
168700                                                                          
168800 IMS-GNP-WDL611-UNQUAL SECTION.                                           
168900     MOVE 'IMS-GNP-WDL611-U' TO CURRENT-IMS-SECTION                       
169000                                                                          
169100     MOVE SPACE              TO ALL-SSA                                   
169200     MOVE 'WDL611  '         TO SSA1                                      
169300     MOVE '  GE'             TO GOOD-STATUSCODES                          
169400     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
169500     MOVE WDL6-STATUS-CODE   TO STATUS-WS                                 
169600     PERFORM IMS-STATUSCHECK                                              
169700     .                                                                    
169800                                                                          
169900 IMS-GU-WDQ201 SECTION.                                                   
170000     MOVE 'IMS-GU-WDQ201   ' TO CURRENT-IMS-SECTION                       
170100                                                                          
170200     MOVE SPACE               TO ALL-SSA                                  
170300     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
170400          DELIMITED BY SIZE INTO SSA1                                     
170500     MOVE '    '              TO GOOD-STATUSCODES                         
170600     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
170700     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
170800     PERFORM IMS-STATUSCHECK                                              
170900     .                                                                    
171000                                                                          
171100 IMS-GU-WDQ4B SECTION.                                                    
171200     MOVE 'IMS-GU-WDQ4B    ' TO CURRENT-IMS-SECTION                       
171300                                                                          
171400     MOVE SPACE              TO ALL-SSA                                   
171500     STRING 'WDQ4B1  (WDQ4B1KY=>' W-WDQ4B1KY-MIN-X                        
171600                    '&WDQ4B1KY=<' W-WDQ4B1KY-MAX-X                        
171700                    '&IDDC    = ' W-IDDC-Q4-X ')'                         
171800          DELIMITED BY SIZE INTO SSA1                                     
171900     MOVE '  GE'              TO GOOD-STATUSCODES                         
172000     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-AREA-Q4B1 SSA1                
172100     MOVE WDQ4B-STATUS-CODE   TO STATUS-WS                                
172200     PERFORM IMS-STATUSCHECK                                              
172300     .                                                                    
172400                                                                          
172500 IMS-GN-WDQ4B SECTION.                                                    
172600     MOVE 'IMS-GN-WDQ4B    ' TO CURRENT-IMS-SECTION                       
172700                                                                          
172800     MOVE SPACE              TO ALL-SSA                                   
172900     STRING 'WDQ4B1  (WDQ4B1KY=>' W-WDQ4B1KY-MIN-X                        
173000                    '&WDQ4B1KY=<' W-WDQ4B1KY-MAX-X                        
173100                    '&IDDC    = ' W-IDDC-Q4-X ')'                         
173200          DELIMITED BY SIZE INTO SSA1                                     
173300     MOVE '  GEGB'           TO GOOD-STATUSCODES                          
173400     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-AREA-Q4B1 SSA1                
173500     MOVE WDQ4B-STATUS-CODE  TO STATUS-WS                                 
173600     PERFORM IMS-STATUSCHECK                                              
173700     .                                                                    
173800 IMS-GU-WDQ401 SECTION.                                                   
173900                                                                          
174000     STRING 'WDQ401  (WDQ401KY =' W-WDQ401KY-X ')'                        
174100          DELIMITED BY SIZE INTO SSA1                                     
174200     MOVE '    '              TO GOOD-STATUSCODES                         
174300     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
174400     MOVE WDQ4-STATUS-CODE    TO STATUS-WS                                
174500     PERFORM IMS-STATUSCHECK                                              
174600     .                                                                    
174700 IMS-ISRT-WDR601 SECTION.                                                 
174800     MOVE 'IMS-ISRT-WDR601 ' TO CURRENT-IMS-SECTION                       
174900                                                                          
175000     MOVE SPACE              TO ALL-SSA                                   
175100     MOVE 'WDR601' TO SSA1                                                
175200     MOVE '  II'             TO GOOD-STATUSCODES                          
175300     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
175400     MOVE WDR6-STATUS-CODE   TO STATUS-WS                                 
175500     PERFORM IMS-STATUSCHECK                                              
175600     .                                                                    
175700                                                                          
175800                                                                          
175900 IMS-STATUSCHECK SECTION.                                                 
176000                                                                          
176100     SET STATUS-IX TO 1                                                   
176200     SEARCH GOOD-STATUS                                                   
176300       AT END                                                             
176400         STRING ' WRONG STATUS CODE FROM IMS: ' STATUS-WS                 
176500           DELIMITED BY SIZE INTO ERRORTEXT                               
176600         CALL FELLOG                                                      
176700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
176800         CONTINUE                                                         
176900     END-SEARCH                                                           
177000     .                                                                    
177100*    -COPY WY2000P1                                                       
178000*    -COPY WY2000Q1                                                       
