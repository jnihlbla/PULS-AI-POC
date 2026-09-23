000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   14/07/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM IS STARTED BY WL0188 TO UPDATE THE                  
000900*        DETAILS IN WDE211 AND WDE122 SEGMENT.                            
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDE1                                       
001200*        THE PROGRAM UPDATES   WDE2                                       
001300*                                                                         
001400*    ADDRESS : CARPARTS.NDC.CHANGETRANSPORT                               
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WL0191T / WL0191U                                   
001800*        REQU       : WL0191I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESP       : WL0191O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'WL019100'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  W-IDDC                      PIC X(02)   VALUE SPACE.                 
003900 77  W-IDDISTR                   PIC 9(04)   VALUE ZERO COMP-3.           
004000 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
004100 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
004200 77  W-KDVALUTA                  PIC 9(2)    VALUE ZERO.                  
004300 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
004400 77  WS-TIDPUNKT                 PIC 9(8)   VALUE ZERO.                   
004500 77  WS-DAGENS-DATUM             PIC 9(6)   VALUE ZERO.                   
004510 77  WS-BESLULEV                 PIC 9(5)   VALUE ZERO.                   
004520 77  WS-BESLULEV-TRIM            PIC Z(5).                                
004530 01  WS-COUNT                    PIC 9(2)   VALUE ZERO.                   
004540 01  WS-LEN                      PIC S9(4) COMP VALUE ZERO.               
004600 77  WS-DCS-IDFTG                PIC X(2)   VALUE SPACE.                  
004700 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004800 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
004900                                                                          
005000*01  -COPY WWDC99                                                         
005100                                                                          
005200 01  WS-TID-X.                                                            
005300     03 WS-TISKPTID                PIC 9(6)       VALUE ZERO.             
005400     03 WS-TISKPTID-GRP            REDEFINES WS-TISKPTID.                 
005500       05 WS-TISKPTID-HHMM         PIC 9(4).                              
005600       05 WS-TISKPTID-SS           PIC 9(2).                              
005700                                                                          
005800 77  FLSAMFAK-SW                 PIC X       VALUE 'N'.                   
005900     88  FLSAMFAK                            VALUE 'J'.                   
006000                                                                          
006100 77  WDE122-ISRT-SW              PIC X       VALUE 'N'.                   
006200     88  WDE122-ISRT                         VALUE 'J'.                   
006300                                                                          
006400 77  WDE211-ISRT-SW              PIC X       VALUE 'N'.                   
006500     88  WDE211-ISRT                         VALUE 'J'.                   
006600                                                                          
006700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006800     88  INDATA-OK                           VALUE 'J'.                   
006900     88  INDATA-WRONG                        VALUE 'N'.                   
007000                                                                          
007100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007200     88  KEYS-OK                             VALUE 'J'.                   
007300     88  KEYS-WRONG                          VALUE 'N'.                   
007400                                                                          
007500 77  INPUT-111-SW                PIC X       VALUE 'N'.                   
007600     88  INPUT-111                           VALUE 'J'.                   
007700                                                                          
007800 77  INPUT-122-SW                PIC X       VALUE 'N'.                   
007900     88  INPUT-122                           VALUE 'J'.                   
008000                                                                          
008100 77  INPUT-211-SW                PIC X       VALUE 'N'.                   
008200     88  INPUT-211                           VALUE 'J'.                   
008300                                                                          
008400                                                                          
008500*    --- PARAMETRAR TILL W930VAL                                          
008600*01 -COPY W930VAL                                                         
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL W510CURR                                         
008900*01 -COPY W510CURR                                                        
009000     EJECT                                                                
009100 01  TEST-IDDISTR               PIC S9(05)   VALUE ZERO  COMP-3.          
009200*01  FILLER  -COPY  WWDIST79  -RED  TEST-IDDISTR                          
009300*01  FILLER  -COPY  WWDIST35  -RED  TEST-IDDISTR                          
009400*01  FILLER  -COPY  WWDIST07  -RED  TEST-IDDISTR                          
009500*01  FILLER  -COPY  WWDIST92  -RED  TEST-IDDISTR                          
009600                                                                          
009700*   --- SUBPROGRAMS AND PARAMETER AREAS                                   
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010000     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
010100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010600     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
010700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
010800     EJECT                                                                
010900                                                                          
011000 01  MESSAGE-CODES.                                                       
011100     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
011200     03  ERR-PF11-NO-DATA        PIC X(3)    VALUE '014'.                 
011300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
011400                                                                          
011500     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
011600     03  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.                 
011700     03  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.                 
011800     03  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.                 
011900     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '024'.                 
012000     03  NOT-FOUND               PIC X(3)    VALUE '025'.                 
012100     03  ERR-MUST-ENTER          PIC X(3)    VALUE '026'.                 
012200     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
012300     EJECT                                                                
012400 01  ALL-SPACE.                                                           
012500     03 FILLER                   PIC X(80)   VALUE SPACE.                 
012600*    --- PARAMETERS TO ABEND                                              
012700                                                                          
012800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013100*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
013400*01 -COPY WDECAREA                                                        
013500     EJECT                                                                
013600*    --- AREA  FOR W476SHNO ---                                           
013700 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
013800*01 -COPY W476SHNO                                                        
013900                                                                          
014000*    --- AREA  FOR WDATKONV ---                                           
014100 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
014200*01   -COPY WDATAREA                                                      
014300                                                                          
014400*    --- AREA  FOR WZ01SUB  ---                                           
014500 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB'.             
014600*01  -COPY WZ01SUB                                                        
014700                                                                          
014800*    --- AREA  FOR WL01TIDZ ---                                           
014900 01  FILLER                      PIC X(08) VALUE 'WL01TIDZ'.              
015000*01  -COPY WL01TIDZ                                                       
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015300 01  REQU-AREA.                                                           
015400*    03  -COPY WZ01REQU                                                   
015500*    03  -COPY WL0191I1                                                   
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015800     SKIP3                                                                
015900 01  RESP-AREA.                                                           
016000*    03  -COPY WZ01RESP                                                   
016100*    03  -COPY WL0191O1                                                   
016200*                                                                         
016300 01  WS-SAVE-AREA.                                                        
016400     03  WS-PRFRAKT            PIC S9(7)V9(2)      COMP-3.                
016500     03  WS-PRFOERS            PIC S9(7)V9(2)      COMP-3.                
016600     03  WS-PRKURS-MAN         PIC S9(6)V9(5)      COMP-3.                
016700     03  WS-PRLEGKST           PIC S9(7)V9(2)      COMP-3.                
016800     03  WS-RELEGKST           PIC S9(2)V9(1)      COMP-3.                
016900     03  WS-PREMBHNT           PIC S9(7)V9(2)      COMP-3.                
017000     03  WS-REEMBHNT           PIC S9(2)V9(1)      COMP-3.                
017100     03  WS-PRAVDRAG           PIC S9(7)V9(2)      COMP-3.                
017200     03  WS-REAVDRAG           PIC S9(2)V9(1)      COMP-3.                
017300     03  WS-REFOERS            PIC S9(2)V9(3)      COMP-3.                
017400     03  WS-REOVKOFF           PIC S9(2)V9(1)      COMP-3.                
017500                                                                          
017600*                                                                         
017700     EJECT                                                                
017800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100     SKIP3                                                                
018200 01  KEYS-TO-DLI.                                                         
018300     03  W-IDSHIPM-X.                                                     
018400         05  W-IDSHIPM           PIC  9(07)  VALUE ZERO.                  
018500                                                                          
018600     03  W-WDE211KY-X.                                                    
018700         05  W-WDE211-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
018800         05  W-WDE211-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
018900                                                                          
019000     03  W-WDE211KY-MIN.                                                  
019100         05  W-WDE211-IDDISTR-MIN                                         
019200                                 PIC S9(05)  VALUE ZERO COMP-3.           
019300         05  FILLER              PIC  X(4)   VALUE LOW-VALUES.            
019400                                                                          
019500     03  W-WDE211KY-MAX.                                                  
019600         05  W-WDE211-IDDISTR-MAX                                         
019700                                 PIC S9(05)  VALUE ZERO COMP-3.           
019800         05  FILLER              PIC  X(4)   VALUE HIGH-VALUES.           
019900                                                                          
020000     03  W-WDE111KY-X.                                                    
020100         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
020200         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
020300                                                                          
020400     03  W-WDE111KY-MIN.                                                  
020500         05  W-WDE111-IDDISTR-MIN                                         
020600                                 PIC S9(05)  VALUE ZERO COMP-3.           
020700         05  FILLER              PIC  X(4)   VALUE LOW-VALUES.            
020800                                                                          
020900     03  W-WDE111KY-MAX.                                                  
021000         05  W-WDE111-IDDISTR-MAX                                         
021100                                 PIC S9(05)  VALUE ZERO COMP-3.           
021200         05  FILLER              PIC  X(4)   VALUE HIGH-VALUES.           
021300                                                                          
021400     03  W-IDGMT-MIN-X.                                                   
021500         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
021600         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
021700                                                                          
021800     03  W-IDGMT-MAX-X.                                                   
021900         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
022000         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
022100                                                                          
022200     03  W-WDB101KY-X.                                                    
022300         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
022400         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
022500                                                                          
022600     03  W-KDSEGKEY-X.                                                    
022700         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
022800                                                                          
022900     03  W-4495-X.                                                        
023000         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
023100         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
023200         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
023300         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
023400         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
023500                                                                          
023600     03  W-4498-MIN.                                                      
023700         05  W-4498-IDDISTR-MIN  PIC S9(05)  COMP-3.                      
023800         05  W-4498-IDKUNDNR-MIN PIC S9(07)  COMP-3.                      
023900         05  FILLER              PIC  X(18)  VALUE LOW-VALUE.             
024000                                                                          
024100     03  W-4498-MAX.                                                      
024200         05  W-4498-IDDISTR-MAX  PIC S9(05)  COMP-3.                      
024300         05  W-4498-IDKUNDNR-MAX PIC S9(07)  COMP-3.                      
024400         05  FILLER              PIC  X(18)  VALUE HIGH-VALUE.            
024500                                                                          
024600     03  W-IDDC-B6-X.                                                     
024700         05 W-IDDC-B6                  PIC X(2).                          
024800                                                                          
024900     SKIP2                                                                
025000*    --- STATUS-KOD FRÅN IMS                                              
025100 01  STATUS-WS                   PIC XX.                                  
025200     88  SEGMENT-FOUND                       VALUE '  '.                  
025300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025500     SKIP2                                                                
025600 01  GOOD-STATUSCODES.                                                    
025700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800     SKIP3                                                                
025900 01  SSA1                        PIC X(128).                              
026000 01  SSA2                        PIC X(128).                              
026100 01  SSA3                        PIC X(128).                              
026200     EJECT                                                                
026300*    --- IMS FUNCTION CODES                                               
026400*01  -COPY W0003                                                          
026500     EJECT                                                                
026600*    ---  DLI INPUT-OUTPUT AREA                                           
026700                                                                          
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
026900 01  DLI-IO-WDE101.                                                       
027000*    03  -COPY WDE101                                                     
027100                                                                          
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
027300 01  DLI-IO-WDE111.                                                       
027400*    03  -COPY WDE111                                                     
027500                                                                          
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE122'.                      
027700 01  DLI-IO-WDE122.                                                       
027800*    03  -COPY WDE122                                                     
027900                                                                          
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
028100 01  DLI-IO-WDE201.                                                       
028200*    03  -COPY WDE201                                                     
028300                                                                          
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
028500 01  DLI-IO-WDE211.                                                       
028600*    03  -COPY WDE211                                                     
028700                                                                          
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
028900 01  DLI-IO-WDB101.                                                       
029000*    03  -COPY WDB101                                                     
030000                                                                          
030100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
030200 01  DLI-IO-WDB201.                                                       
030300*    03  -COPY WDB201                                                     
030400                                                                          
030500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4496'.                    
030600 01  DLI-IO-WDGX4496.                                                     
030700*    03  -COPY WDGX4496                                                   
030800                                                                          
030900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
031000 01  DLI-IO-WDGX4498.                                                     
031100*    03  -COPY WDGX4498                                                   
031200                                                                          
031300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031400 01   DLI-IO-AREA-B601.                                                   
031500*     03  -COPY WDB601                                                    
031600                                                                          
031700     EJECT                                                                
031800 LINKAGE SECTION.                                                         
031900 01  MSG-PCB                     PIC X.                                   
032000*01  -COPY W0008   -PRE 4495-                                             
032100     05  FILLER                  PIC X.                                   
032200                                                                          
032300*01  -COPY W0008  -PRE WDE1-                                              
032400     05  FILLER                  PIC X.                                   
032500                                                                          
032600*01  -COPY W0008  -PRE WDE2-                                              
032700     05  FILLER                  PIC X.                                   
032800*01  -COPY W0008  -PRE WDG2-                                              
032900     05  FILLER                  PIC X.                                   
033000*01  -COPY W0008  -PRE WDB1-                                              
033100     05  FILLER                  PIC X.                                   
033200*01  -COPY W0008  -PRE WDB2-                                              
033300     05  FILLER                  PIC X.                                   
033400*01  -COPY W0008  -PRE WDB6-                                              
033500     05  FILLER                  PIC X.                                   
033600 01  SHNO-4517-PCB               PIC X.                                   
033700     EJECT                                                                
033800 PROCEDURE DIVISION  USING MSG-PCB  4495-PCB                              
033900                           WDE1-PCB WDE2-PCB WDG2-PCB                     
034000                           WDB1-PCB WDB2-PCB WDB6-PCB                     
034100                           SHNO-4517-PCB.                                 
034200                                                                          
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING MSG-PCB  4495-PCB                              
034500                           WDE1-PCB WDE2-PCB WDB6-PCB                     
034600                           WDB1-PCB WDB2-PCB                              
034700                           SHNO-4517-PCB.                                 
034800                                                                          
034900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
035000     IF SUB-KDRC = 0                                                      
035100       PERFORM A-INIT                                                     
035200       PERFORM B-CHECK-KEYS                                               
035300       IF KEYS-OK                                                         
035400         IF REQU-KDPGMACT = 'E'                                           
035500           PERFORM G-CHECK-INPUT                                          
035600           IF INDATA-OK                                                   
035700             PERFORM H-UPDATE                                             
035800           END-IF                                                         
035900         END-IF                                                           
036000         IF DIST79-DEALER-PRICE                                           
036100           PERFORM S02-ERASE-FIELDS                                       
036200         END-IF                                                           
036300         IF INDATA-OK                                                     
036400           PERFORM F-READ-SHOW-INFO                                       
036500         END-IF                                                           
036600       END-IF                                                             
036700       PERFORM S04-RETURN-RESPONSE                                        
036800     END-IF                                                               
036900                                                                          
037000     MOVE ZERO TO RETURN-CODE                                             
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 A-INIT SECTION.                                                          
037500                                                                          
037600     MOVE ALL '+'                    TO RESP-AREA                         
037700     MOVE SPACE                      TO RESP-IDMSG-ERROR                  
037800                                        RESP-IDMSG-INFO                   
037900                                        RESP-IDELMT-ERROR                 
038000     MOVE 001                        TO RESP-IDMSGVER                     
038100                                                                          
038200     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
038300     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
038400                                                                          
038500     ACCEPT WS-DAGENS-DATUM          FROM DATE                            
038600     ACCEPT WS-TIDPUNKT              FROM TIME                            
038700     MOVE HIGH-VALUE                 TO W-IDGMT-MAX-X                     
038800     .                                                                    
038900     EJECT                                                                
039000 B-CHECK-KEYS SECTION.                                                    
039100                                                                          
039200     MOVE YES TO KEYS-SW                                                  
039300                                                                          
039400     IF REQU-KDPGMACT = 'S' OR 'E'                                        
039500        CONTINUE                                                          
039600     ELSE                                                                 
039700        MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                       
039800        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
039900        MOVE NOO                TO KEYS-SW                                
040000     END-IF                                                               
040100                                                                          
040200     IF KEYS-OK                                                           
040300       IF REQU-IDTRPTNR-KEY = ALL '+'                                     
040400*        NO INPUT DATA IS ENTERED ***                                     
040500         MOVE ERR-MUST-ENTER  TO RESP-IDMSG-ERROR                         
040600         MOVE 'IDTRPTNR'      TO RESP-IDELMT-ERROR                        
040700         MOVE NOO             TO KEYS-SW                                  
040800       ELSE                                                               
040900         IF REQU-IDTRPTNR-KEY NOT NUMERIC                                 
041000           MOVE ERR-NOT-NUMERIC TO RESP-IDMSG-ERROR                       
041100           MOVE 'IDTRPTNR'   TO RESP-IDELMT-ERROR                         
041200           MOVE NOO          TO KEYS-SW                                   
041300         ELSE                                                             
041400           IF REQU-IDTRPTNR-KEY = ZERO                                    
041500             MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                  
041600             MOVE 'IDTRPTNR'   TO RESP-IDELMT-ERROR                       
041700             MOVE NOO          TO KEYS-SW                                 
041800           ELSE                                                           
041900             MOVE REQU-IDTRPTNR-KEY                                       
042000                               TO RESP-IDTRPTNR-KEY                       
042100                                  W-IDTRPTNR                              
042200           END-IF                                                         
042300         END-IF                                                           
042400       END-IF                                                             
042500     END-IF                                                               
042600                                                                          
042700     IF KEYS-OK                                                           
042800        IF REQU-IDLBBET-KEY = ALL '+'                                     
042900*          NO INPUT DATA IS ENTERED ***                                   
043000           MOVE ERR-MUST-ENTER  TO RESP-IDMSG-ERROR                       
043100           MOVE 'IDLBBET'       TO RESP-IDELMT-ERROR                      
043200           MOVE NOO             TO KEYS-SW                                
043300        ELSE                                                              
043400           MOVE REQU-IDLBBET-KEY                                          
043500                                TO RESP-IDLBBET-KEY                       
043600                                   W-IDLBBET                              
043700        END-IF                                                            
043800     END-IF                                                               
043900                                                                          
044000     IF KEYS-OK                                                           
044100        IF REQU-FLFARLIG-KEY = 'Y' OR 'N' OR 'M'                          
044200           MOVE REQU-FLFARLIG-KEY   TO RESP-FLFARLIG-KEY                  
044300        ELSE                                                              
044400           MOVE 'KDFARLIG'      TO RESP-IDELMT-ERROR                      
044500           MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                    
044600           MOVE NOO             TO KEYS-SW                                
044700        END-IF                                                            
044800     END-IF                                                               
044900                                                                          
045000*    IF REQU-KVRADER NUMERIC                                              
045100*      MOVE REQU-KVRADER             TO RESP-KVRADER                      
045200*    ELSE                                                                 
045300*      MOVE ZERO                     TO RESP-KVRADER                      
045400*    END-IF                                                               
045500                                                                          
045600     MOVE REQU-IDDC-KEY        TO W-IDDC-B6                               
045700     PERFORM IMS-GU-WDB601                                                
045800     MOVE DCS-IDFTG            TO WS-DCS-IDFTG                            
045900     IF DCS-KDDC NOT = SPACE                                              
046000       MOVE REQU-IDDC-KEY  TO W-IDDC                                      
046100                              W-IDDC-4495                                 
046200       MOVE REQU-IDDC-KEY  TO RESP-IDDC-KEY                               
046300     ELSE                                                                 
046400       MOVE NOO                TO KEYS-SW                                 
046500     END-IF                                                               
046600                                                                          
046700     MOVE REQU-IDSHIPM         TO W-IDSHIPM                               
046800     IF W-IDSHIPM NUMERIC AND W-IDSHIPM > ZERO                            
046900       MOVE REQU-IDSHIPM       TO RESP-IDSHIPM                            
047000     ELSE                                                                 
047100       MOVE ZERO               TO RESP-IDSHIPM                            
047200     END-IF                                                               
047300                                                                          
047400     IF KEYS-OK                                                           
047500       MOVE REQU-IDDISTR        TO RESP-IDDISTR                           
047600                                   W-IDDISTR                              
047700                                   TEST-IDDISTR                           
047800                                   W-4498-IDDISTR-MIN                     
047900                                   W-4498-IDDISTR-MAX                     
048000                                   W-WDE211-IDDISTR                       
048100                                   W-WDE211-IDDISTR-MIN                   
048200                                   W-WDE211-IDDISTR-MAX                   
048300                                   W-WDE111-IDDISTR                       
048400                                   W-WDE111-IDDISTR-MIN                   
048500                                   W-WDE111-IDDISTR-MAX                   
048600                                   W-IDDISTR-WDB2-MIN                     
048700                                   W-IDDISTR-WDB2-MAX                     
048800       MOVE REQU-IDKUNDNR       TO RESP-IDKUNDNR                          
048900                                   W-WDE211-IDKUNDNR                      
049000                                   W-WDE111-IDKUNDNR                      
049100                                   W-IDKUNDNR-WDB2-MIN                    
049200     END-IF                                                               
049300                                                                          
049400*    -- FIND OUT IF "SAMFAKTURERING"                                      
049500     PERFORM IMS-GU-WDB201                                                
049600     IF SEGMENT-FOUND                                                     
049700        IF DIST92-ITALY OR DIST92-GREECE                                  
049800          MOVE NOO                  TO FLSAMFAK-SW                        
049900        END-IF                                                            
050000        IF GMT-FLSAMFAK = YES                                             
050100          MOVE YES                  TO FLSAMFAK-SW                        
050200        END-IF                                                            
050300     ELSE                                                                 
050400        MOVE NOO                    TO FLSAMFAK-SW                        
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 F-READ-SHOW-INFO SECTION.                                                
050900                                                                          
051000     PERFORM IMS-GU-WDGX4496                                              
051100     IF SEGMENT-FOUND                                                     
051200        MOVE 4496-VKORDBTO-LASTB TO RESP-VKORDBTO                         
051300        MOVE 4496-VLORDBTO-LASTB TO RESP-VLORDBTO                         
051400        MOVE 4496-SUORDV-LASTB   TO RESP-SUORDV                           
051500     END-IF                                                               
051600                                                                          
051700     MOVE W-IDDISTR         TO TEST-IDDISTR                               
051800     PERFORM FB-HAMTA-KDVALISO-MC                                         
051900                                                                          
052000     IF W-IDSHIPM NUMERIC AND W-IDSHIPM > ZERO                            
052100       PERFORM FA-READ-BASICDATA                                          
052200                                                                          
052300       IF SEGMENT-MISSING                                                 
052400         PERFORM S03-ALL-SPACE-LINE                                       
052500       ELSE                                                               
052600         IF DIST79-DEALER-PRICE                                           
052700           PERFORM S02-ERASE-FIELDS                                       
052800           MOVE NOT-FOUND     TO RESP-IDMSG-ERROR                         
052900           MOVE 'DEALERPRICE'  TO RESP-IDELMT-ERROR                       
053000         ELSE                                                             
053100           MOVE TILL-PRFRAKT    TO RESP-PRFRAKT                           
053200           MOVE TILL-PRFOERS    TO RESP-PRFOERS                           
053300           MOVE TILL-REFOERS    TO RESP-REFOERS                           
053400           MOVE TILL-REOVKOFF   TO RESP-REOVKOFF                          
053500           MOVE TILL-PRLEGKST   TO RESP-PRLEGKST                          
053600           MOVE TILL-RELEGKST   TO RESP-RELEGKST                          
053700           MOVE TILL-PREMBHNT   TO RESP-PREMBHNT                          
053800           MOVE TILL-REEMBHNT   TO RESP-REEMBHNT                          
053900           MOVE TILL-PRAVDRAG   TO RESP-PRAVDRAG                          
054000           MOVE TILL-REAVDRAG   TO RESP-REAVDRAG                          
054100           MOVE TILL-KDVALISO-MAN                                         
054200                                TO RESP-KDVALISO-MAN                      
054300           MOVE TILL-PRKURS-MAN TO RESP-PRKURS-MAN                        
054400           MOVE ALL-SPACE       TO RESP-FLCREDL                           
054500         END-IF                                                           
054600                                                                          
054700         MOVE TILL-IDSIGILL     TO RESP-IDSIGILL                          
054800         MOVE TILL-TISKEPPN-MAN TO RESP-TISKEPPN-MAN                      
054900         MOVE TILL-IDLC         TO RESP-IDLC                              
055000         MOVE TILL-IDLICENS     TO RESP-IDLICENS                          
055100         MOVE TILL-IDBOKN       TO RESP-IDBOKN                            
055200         MOVE TILL-IDVCERT      TO RESP-IDVCERT                           
055300         MOVE TILL-BESLULEV     TO RESP-BESLULEV                          
055310         INSPECT RESP-BESLULEV REPLACING LEADING ZERO BY SPACE            
055400         MOVE ALL-SPACE         TO RESP-KDVALUTA-MAN                      
055500       END-IF                                                             
055600                                                                          
055700       IF FLSAMFAK                                                        
055800         PERFORM IMS-GHU-WDE211-DIST                                      
055900       ELSE                                                               
056000         PERFORM IMS-GHU-WDE211                                           
056100       END-IF                                                             
056200                                                                          
056300       IF SEGMENT-FOUND                                                   
056400         IF BGMT-FLSEPINV = 'J'                                           
056500           MOVE 'Y'             TO RESP-FLSEPINV                          
056600         ELSE                                                             
056700           MOVE BGMT-FLSEPINV   TO RESP-FLSEPINV                          
056800         END-IF                                                           
056900         IF BGMT-KDLEVVIL > ZERO                                          
057000           MOVE BGMT-KDLEVVIL   TO RESP-KDLEVVIL                          
057100         ELSE                                                             
057200           MOVE ALL-SPACE       TO RESP-KDLEVVIL                          
057300         END-IF                                                           
057400       ELSE                                                               
057500         MOVE ALL-SPACE         TO RESP-FLSEPINV                          
057600                                   RESP-KDLEVVIL                          
057700       END-IF                                                             
057800                                                                          
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 FA-READ-BASICDATA SECTION.                                               
058300                                                                          
058400     IF FLSAMFAK                                                          
058500       PERFORM IMS-GHU-WDE122-DIST                                        
058600     ELSE                                                                 
058700       PERFORM IMS-GHU-WDE122                                             
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200 FB-HAMTA-KDVALISO-MC   SECTION.                                          
059300                                                                          
059400     IF NOT DIST79-DEALER-PRICE                                           
059500       MOVE 'Currency   '           TO RESP-CURRENCY-MC                   
059600       IF DIST35-NONVCC-NONVCC-REFILL                                     
059610       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
059700       OR DCS-USA                                                         
059800       OR DCS-LAND-NON-VCC-OWNED                                          
059900         MOVE DCS-KDVALISO          TO RESP-KDVALISO-MC                   
060000       ELSE                                                               
061000         MOVE 'SEK'                 TO RESP-KDVALISO-MC                   
061100       END-IF                                                             
061200     END-IF                                                               
061300     .                                                                    
061400     EJECT                                                                
061500                                                                          
061600 G-CHECK-INPUT SECTION.                                                   
061700                                                                          
061800     MOVE NOO TO INPUT-122-SW                                             
061900     MOVE NOO TO INPUT-211-SW                                             
062000     MOVE YES TO INDATA-SW                                                
063000                                                                          
064000     MOVE ZERO TO   WS-PRKURS-MAN                                         
064100                    WS-PRFRAKT                                            
064200                    WS-PRLEGKST                                           
064300                    WS-RELEGKST                                           
064400                    WS-PREMBHNT                                           
064500                    WS-REEMBHNT                                           
064600                    WS-PRFOERS                                            
064700                    WS-REFOERS                                            
064800                    WS-REOVKOFF                                           
064900                    WS-PRAVDRAG                                           
065000                    WS-REAVDRAG                                           
065100     EJECT                                                                
065200     IF REQU-WL0191I1 = ALL '+'                                           
065300       MOVE ERR-PF11-NO-DATA  TO RESP-IDMSG-ERROR                         
065400       MOVE NOO               TO INDATA-SW                                
065500     ELSE                                                                 
065600       PERFORM GA-CHECK-SPACE-INPUT                                       
065700       IF NOT DIST79-DEALER-PRICE                                         
065800         IF REQU-FLCREDL NOT = '+'                                        
065900           IF REQU-FLCREDL = 'J' OR 'Y'                                   
066000             MOVE YES TO INPUT-122-SW                                     
066100           ELSE                                                           
066200             MOVE 'FLCREDL'   TO RESP-IDELMT-ERROR                        
066300             MOVE NOO TO INDATA-SW                                        
066400           END-IF                                                         
066500         END-IF                                                           
066600                                                                          
066700         IF REQU-VALUTA NOT = ALL '+'                                     
066800           MOVE YES TO INPUT-122-SW                                       
066900           IF REQU-KDVALUTA-MAN = ALL '+' OR                              
067000              REQU-PRKURS-MAN   = ALL '+' OR                              
067100              NOT (REQU-FLCREDL = 'J' OR 'Y')                             
067200             MOVE 'VALUTA'                TO RESP-IDELMT-ERROR            
067300             MOVE NOO TO INDATA-SW                                        
067400           ELSE                                                           
067500             IF REQU-KDVALUTA-MAN NOT NUMERIC                             
067600               MOVE 'KDVALUTA'            TO RESP-IDELMT-ERROR            
067700               MOVE NOO TO INDATA-SW                                      
067800             ELSE                                                         
067900               MOVE REQU-KDVALUTA-MAN     TO W-KDVALUTA                   
068000                                                                          
068100               MOVE '   '                 TO CURR-KDVALISO-ROW            
068200               MOVE +1                    TO TAB-IX                       
068300               PERFORM UNTIL TAB-IX > TAB-IX-MAX                          
068400                 IF W-KDVALUTA = TAB-KDVALUTA(TAB-IX)                     
068500                   MOVE TAB-KDVALISO(TAB-IX ) TO CURR-KDVALISO-ROW        
068600                   MOVE TAB-IX-MAX        TO TAB-IX                       
068700                 END-IF                                                   
068800                 ADD +1                   TO TAB-IX                       
068900               END-PERFORM                                                
069000                                                                          
069100               IF CURR-KDVALISO-ROW NOT = '   '                           
069200                 MOVE W-DATE-AAMM         TO CURR-TIAAMM                  
069300                 MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUv            
069400                 MOVE 'M'                 TO CURR-KDVALTYP                
069500                                                                          
069600                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
069700                 IF CURR-KDSVAR = ' '                                     
069800                   CONTINUE                                               
069900                 ELSE                                                     
070000                   MOVE 'KDVALUTA'        TO RESP-IDELMT-ERROR            
070100                   MOVE NOO TO INDATA-SW                                  
070200                 END-IF                                                   
070300               ELSE                                                       
070400                 MOVE 'KDVALUTA'          TO RESP-IDELMT-ERROR            
070500                 MOVE NOO TO INDATA-SW                                    
070600               END-IF                                                     
070700             END-IF                                                       
070800** CHECK ON PRKURS                                                        
070900             MOVE REQU-PRKURS-MAN TO DEC-IDFRIDATA                        
071000             MOVE 6 TO DEC-KVHELTAL                                       
071100             MOVE 4 TO DEC-KVDECIMAL                                      
071200             PERFORM GB-HANDLE-ALPHA-TO-NUM                               
071300                                                                          
071400             IF DEC-KDSVAR-OK                                             
071500               MOVE DEC-IDEDITDATA TO WS-PRKURS-MAN                       
071600             ELSE                                                         
071700               MOVE 'PRKURS'       TO RESP-IDELMT-ERROR                   
071800               MOVE NOO TO INDATA-SW                                      
071900             END-IF                                                       
072000           END-IF                                                         
072100         END-IF                                                           
072200                                                                          
072300         MOVE W-IDDISTR TO  TEST-IDDISTR                                  
072400         IF DIST35-RETUR                OR                                
072500            DIST35-CDC-NL-REFILL        OR                                
072600            DIST35-CDC-GB-REFILL        OR                                
072700            DIST35-CDC-GB-3A-REFILL     OR                                
072800            DIST35-CDC-ES-REFILL        OR                                
072900            DIST35-CDC-IT-REFILL        OR                                
073000            DIST35-CDC-AT-REFILL        OR                                
073100            DIST35-REFILL-JP            OR                                
073200            DIST35-REFILL-NA            OR                                
073300            DIST35-REFILL-NA-JAP        OR                                
073400            DIST35-REFILL-CN            OR                                
073500            DIST35-CDC-IN-REFILL        OR                                
073600            DIST35-CDC-KR-REFILL        OR                                
073700            DIST35-CDC-AE-REFILL        OR                                
073800            DIST35-CDC-MY-REFILL        OR                                
073900            DIST35-CDC-TH-REFILL        OR                                
074000            DIST35-CDC-TW-REFILL        OR                                
074100            DIST35-CDC-MX-REFILL        OR                                
074200            DIST35-CDC-BR-REFILL        OR                                
074300            DIST35-IN-CDC-RETUR         OR                                
074400            DIST35-KR-CDC-RETUR         OR                                
074500            DIST35-AE-CDC-RETUR         OR                                
074600            DIST35-TR-CDC-RETUR         OR                                
074610            DIST35-ZA-CDC-RETUR         OR                                
074700            DIST35-MY-CDC-RETUR         OR                                
074800            DIST07-NA-CUSTOMERS         OR                                
074900            DIST07-KINA                 OR                                
075000            DIST07-KINA-RET-DISCR       OR                                
075100            DIST07-INDIEN               OR                                
075200            DIST07-INDIEN-RET-DISCR     OR                                
075300            DIST07-KOREA                OR                                
075400            DIST07-KOREA-RET-DISCR      OR                                
075500            DIST07-TURKEY               OR                                
075600            DIST07-TURKEY-RET-DISCR     OR                                
075610            DIST07-S-AFRICA             OR                                
075620            DIST07-S-AFRICA-RET-DISCR   OR                                
075700            DIST07-MALAYSIA             OR                                
075800            DIST07-MALAYSIA-RET-DISCR   OR                                
075900            DIST07-THAILAND             OR                                
076000            DIST07-THAILAND-RET-DISCR   OR                                
076100            DIST07-TAIWAN               OR                                
076200            DIST07-TAIWAN-RET-DISCR     OR                                
076300            DIST07-MEXICO               OR                                
076400            DIST07-MEXICO-RET-DISCR     OR                                
076410            DIST07-BRAZIL               OR                                
076420            DIST07-BRAZIL-RET-DISCR                                       
076500                                                                          
076600           IF REQU-PRFRAKT NOT = ALL '+'                                  
076700             IF (DIST35-REFILL AND NOT (DIST35-REFILL-NA OR               
076800                                        DIST35-REFILL-CN OR               
076900                                        DIST35-CDC-IN-REFILL OR           
077000                                        DIST35-CDC-KR-REFILL OR           
077100                                        DIST35-CDC-AE-REFILL OR           
077200                                        DIST35-CDC-TR-REFILL OR           
077210                                        DIST35-CDC-ZA-REFILL OR           
077300                                        DIST35-CDC-MY-REFILL OR           
077400                                        DIST35-CDC-TH-REFILL OR           
077500                                        DIST35-CDC-TW-REFILL OR           
077600                                        DIST35-CDC-MX-REFILL OR           
077700                                        DIST35-CDC-BR-REFILL))            
077800                                           OR                             
077900                DIST35-RETUR               OR                             
078000                DIST35-REFILL-NA-JAP       OR                             
078100                DIST35-IN-CDC-RETUR        OR                             
078200                DIST35-KR-CDC-RETUR        OR                             
078300                DIST35-AE-CDC-RETUR        OR                             
078400                DIST35-TR-CDC-RETUR        OR                             
078410                DIST35-ZA-CDC-RETUR        OR                             
078500                DIST35-MY-CDC-RETUR        OR                             
078600                DIST07-INDIEN              OR                             
078700                DIST07-INDIEN-RET-DISCR    OR                             
078800                DIST07-KOREA               OR                             
078900                DIST07-KOREA-RET-DISCR     OR                             
079000                DIST07-TURKEY              OR                             
079100                DIST07-TURKEY-RET-DISCR    OR                             
079110                DIST07-S-AFRICA            OR                             
079120                DIST07-S-AFRICA-RET-DISCR  OR                             
079200                DIST07-MALAYSIA            OR                             
079300                DIST07-MALAYSIA-RET-DISCR  OR                             
079400                DIST07-THAILAND            OR                             
079500                DIST07-THAILAND-RET-DISCR  OR                             
079600                DIST07-TAIWAN              OR                             
079700                DIST07-TAIWAN-RET-DISCR    OR                             
079800                DIST07-MEXICO              OR                             
079900                DIST07-MEXICO-RET-DISCR    OR                             
079910                DIST07-BRAZIL              OR                             
079920                DIST07-BRAZIL-RET-DISCR                                   
080000               MOVE 'PRFRAKT'     TO RESP-IDELMT-ERROR                    
080100               MOVE NOO TO INDATA-SW                                      
080200             END-IF                                                       
080300           END-IF                                                         
080400                                                                          
080500           IF REQU-PRLEGKST NOT = ALL '+'                                 
080600             MOVE 'PRLEGKST'   TO RESP-IDELMT-ERROR                       
080700             MOVE NOO          TO INDATA-SW                               
080800           END-IF                                                         
080900                                                                          
081000           IF REQU-PREMBHNT NOT = ALL '+'                                 
081100             MOVE 'PREMBHNT'   TO RESP-IDELMT-ERROR                       
081200             MOVE NOO          TO INDATA-SW                               
081300           END-IF                                                         
081400                                                                          
081500           IF (REQU-PRFOERS NOT = ALL '+' AND NOT                         
081600              (DIST35-REFILL-CN OR DIST35-CDC-IN-REFILL OR                
081700                                   DIST35-CDC-KR-REFILL OR                
081800                                   DIST35-CDC-AE-REFILL OR                
081900                                   DIST35-CDC-TR-REFILL OR                
081910                                   DIST35-CDC-ZA-REFILL OR                
082000                                   DIST35-CDC-MY-REFILL OR                
082100                                   DIST35-CDC-TH-REFILL OR                
082200                                   DIST35-CDC-TW-REFILL OR                
082300                                   DIST35-CDC-MX-REFILL OR                
082400                                   DIST35-CDC-BR-REFILL))                 
082500             MOVE 'PRFOERS'    TO RESP-IDELMT-ERROR                       
082600             MOVE NOO          TO INDATA-SW                               
082700           END-IF                                                         
082800                                                                          
082900           IF REQU-PRAVDRAG NOT = ALL '+'                                 
083000             MOVE 'PRAVDRAG'   TO RESP-IDELMT-ERROR                       
083100             MOVE NOO          TO INDATA-SW                               
083200           END-IF                                                         
083300                                                                          
083400           IF REQU-RELEGKST NOT = ALL '+'                                 
083500             MOVE 'RELEGKST'   TO RESP-IDELMT-ERROR                       
083600             MOVE NOO          TO INDATA-SW                               
083700           END-IF                                                         
083800                                                                          
083900           IF REQU-REEMBHNT NOT = ALL '+'                                 
084000             IF (DIST35-REFILL AND NOT (DIST35-REFILL-NA OR               
084100                                        DIST35-REFILL-CN OR               
084200                                        DIST35-CDC-IN-REFILL OR           
084300                                        DIST35-CDC-KR-REFILL OR           
084400                                        DIST35-CDC-AE-REFILL OR           
084500                                        DIST35-CDC-TR-REFILL OR           
084510                                        DIST35-CDC-ZA-REFILL OR           
084600                                        DIST35-CDC-MY-REFILL OR           
084700                                        DIST35-CDC-TH-REFILL OR           
084800                                        DIST35-CDC-TW-REFILL OR           
084900                                        DIST35-CDC-MX-REFILL OR           
085000                                        DIST35-CDC-BR-REFILL))            
085100                               OR                                         
085200                DIST35-RETUR   OR                                         
085300                DIST35-REFILL-NA-JAP                                      
085400               MOVE 'REEMBHNT' TO RESP-IDELMT-ERROR                       
085500               MOVE NOO        TO INDATA-SW                               
085600             END-IF                                                       
085700           END-IF                                                         
085800                                                                          
085900           IF (REQU-REFOERS NOT = ALL '+' AND NOT                         
086000              (DIST35-REFILL-CN OR DIST35-CDC-IN-REFILL OR                
086100                                   DIST35-CDC-KR-REFILL OR                
086200                                   DIST35-CDC-AE-REFILL OR                
086300                                   DIST35-CDC-TR-REFILL OR                
086310                                   DIST35-CDC-ZA-REFILL OR                
086400                                   DIST35-CDC-MY-REFILL OR                
086500                                   DIST35-CDC-TH-REFILL OR                
086600                                   DIST35-CDC-TW-REFILL OR                
086700                                   DIST35-CDC-MX-REFILL OR                
086800                                   DIST35-CDC-BR-REFILL))                 
086900             MOVE 'REFOERS'    TO RESP-IDELMT-ERROR                       
087000             MOVE NOO          TO INDATA-SW                               
087100           END-IF                                                         
087200                                                                          
087300           IF REQU-REAVDRAG NOT = ALL '+'                                 
087400             MOVE 'REAVDRAG'   TO RESP-IDELMT-ERROR                       
087500             MOVE NOO          TO INDATA-SW                               
087600           END-IF                                                         
087700                                                                          
087800           IF (REQU-REOVKOFF NOT = ALL '+' AND NOT                        
087900              (DIST35-REFILL-CN OR DIST35-CDC-IN-REFILL OR                
088000                                   DIST35-CDC-KR-REFILL OR                
088100                                   DIST35-CDC-AE-REFILL OR                
088200                                   DIST35-CDC-TR-REFILL OR                
088210                                   DIST35-CDC-ZA-REFILL OR                
088300                                   DIST35-CDC-MY-REFILL OR                
088400                                   DIST35-CDC-TH-REFILL OR                
088500                                   DIST35-CDC-TW-REFILL OR                
088600                                   DIST35-CDC-MX-REFILL OR                
088700                                   DIST35-CDC-BR-REFILL))                 
088800             MOVE 'REOVKOFF'   TO RESP-IDELMT-ERROR                       
088900             MOVE NOO          TO INDATA-SW                               
089000           END-IF                                                         
089100         END-IF                                                           
089200                                                                          
089300         IF REQU-PRFRAKT NOT = ALL '+'                                    
089400           MOVE REQU-PRFRAKT    TO DEC-IDFRIDATA                          
089500           MOVE 7 TO DEC-KVHELTAL                                         
089600           MOVE 2 TO DEC-KVDECIMAL                                        
089700           PERFORM GB-HANDLE-ALPHA-TO-NUM                                 
089800                                                                          
089900           IF DEC-KDSVAR-OK                                               
090000             MOVE DEC-IDEDITDATA TO WS-PRFRAKT                            
090100             MOVE YES TO INPUT-122-SW                                     
090200             MOVE YES TO INPUT-211-SW                                     
090300           ELSE                                                           
090400             MOVE 'PRFRAKT'      TO RESP-IDELMT-ERROR                     
090500             MOVE NOO            TO INDATA-SW                             
090600           END-IF                                                         
090700         END-IF                                                           
090800                                                                          
090900         IF REQU-LEGKST NOT = ALL '+'                                     
091000           IF REQU-PRLEGKST NOT = ALL '+' AND                             
091100              REQU-RELEGKST NOT = ALL '+'                                 
091200             MOVE 'LEGKST'          TO RESP-IDELMT-ERROR                  
091300             MOVE NOO               TO INDATA-SW                          
091400           ELSE                                                           
091500             IF REQU-RELEGKST NOT = ALL '+'                               
091600               MOVE REQU-RELEGKST   TO DEC-IDFRIDATA                      
091700               MOVE 2 TO DEC-KVHELTAL                                     
091800               MOVE 1 TO DEC-KVDECIMAL                                    
091900               PERFORM GB-HANDLE-ALPHA-TO-NUM                             
092000                                                                          
092100               IF DEC-KDSVAR-OK                                           
092200                 MOVE DEC-IDEDITDATA TO WS-RELEGKST                       
092300                 MOVE YES TO INPUT-122-SW                                 
092400                 MOVE YES TO INPUT-211-SW                                 
092500               ELSE                                                       
092600                 MOVE 'RELEGKST'     TO RESP-IDELMT-ERROR                 
092700                 MOVE NOO            TO INDATA-SW                         
092800               END-IF                                                     
092900             END-IF                                                       
093000                                                                          
093100             IF REQU-PRLEGKST NOT = ALL '+'                               
093200               MOVE REQU-PRLEGKST   TO DEC-IDFRIDATA                      
093300               MOVE 7 TO DEC-KVHELTAL                                     
093400               MOVE 2 TO DEC-KVDECIMAL                                    
093500               PERFORM GB-HANDLE-ALPHA-TO-NUM                             
093600                                                                          
093700               IF DEC-KDSVAR-OK                                           
093800                 MOVE DEC-IDEDITDATA TO WS-PRLEGKST                       
093900                 MOVE YES TO INPUT-122-SW                                 
094000                 MOVE YES TO INPUT-211-SW                                 
094100               ELSE                                                       
094200                 MOVE 'PRLEGKST'     TO RESP-IDELMT-ERROR                 
094300                 MOVE NOO            TO INDATA-SW                         
094400               END-IF                                                     
094500             END-IF                                                       
094600           END-IF                                                         
094700         END-IF                                                           
094800                                                                          
094900         IF REQU-EMBHNT NOT = ALL '+'                                     
095000           IF REQU-PREMBHNT NOT = ALL '+'  AND                            
095100              REQU-REEMBHNT NOT = ALL '+'                                 
095200             MOVE 'EMBHNT'       TO RESP-IDELMT-ERROR                     
095300             MOVE NOO            TO INDATA-SW                             
095400           ELSE                                                           
095500             IF REQU-REEMBHNT NOT = ALL '+'                               
095600                MOVE REQU-REEMBHNT   TO DEC-IDFRIDATA                     
095700                MOVE 2 TO DEC-KVHELTAL                                    
095800                MOVE 1 TO DEC-KVDECIMAL                                   
095900                PERFORM GB-HANDLE-ALPHA-TO-NUM                            
096000                                                                          
096100                IF DEC-KDSVAR-OK                                          
096200                  MOVE DEC-IDEDITDATA TO WS-REEMBHNT                      
096300                  MOVE YES TO INPUT-122-SW                                
096400                  MOVE YES TO INPUT-211-SW                                
096500                ELSE                                                      
096600                  MOVE 'REEMBHNT'     TO RESP-IDELMT-ERROR                
096700                  MOVE NOO            TO INDATA-SW                        
096800                END-IF                                                    
096900             END-IF                                                       
097000                                                                          
097100             IF REQU-PREMBHNT NOT = ALL '+'                               
097200                MOVE REQU-PREMBHNT   TO DEC-IDFRIDATA                     
097300                MOVE 7 TO DEC-KVHELTAL                                    
097400                MOVE 2 TO DEC-KVDECIMAL                                   
097500                PERFORM GB-HANDLE-ALPHA-TO-NUM                            
097600                                                                          
097700                IF DEC-KDSVAR-OK                                          
097800                  MOVE DEC-IDEDITDATA TO WS-PREMBHNT                      
097900                  MOVE YES TO INPUT-122-SW                                
098000                  MOVE YES TO INPUT-211-SW                                
098100                ELSE                                                      
098200                  MOVE 'PREMBHNT'     TO RESP-IDELMT-ERROR                
098300                  MOVE NOO            TO INDATA-SW                        
098400                END-IF                                                    
098500             END-IF                                                       
098600           END-IF                                                         
098700         END-IF                                                           
098800                                                                          
098900         IF REQU-FOERS NOT = ALL '+'                                      
099000           IF REQU-PRFOERS NOT = ALL '+'  AND                             
099100              REQU-REFOERS-OVKOFF NOT = ALL '+'                           
099200             MOVE 'FOERS'        TO RESP-IDELMT-ERROR                     
099300             MOVE NOO            TO INDATA-SW                             
099400           ELSE                                                           
099500             IF REQU-PRFOERS NOT = ALL '+'                                
099600               MOVE REQU-PRFOERS    TO DEC-IDFRIDATA                      
099700               MOVE 7 TO DEC-KVHELTAL                                     
099800               MOVE 2 TO DEC-KVDECIMAL                                    
099900               PERFORM GB-HANDLE-ALPHA-TO-NUM                             
100000                                                                          
100100               IF DEC-KDSVAR-OK                                           
100200                 MOVE DEC-IDEDITDATA TO WS-PRFOERS                        
100300                 MOVE YES TO INPUT-122-SW                                 
100400                 MOVE YES TO INPUT-211-SW                                 
100500               ELSE                                                       
100600                 MOVE 'PRFOERS'      TO RESP-IDELMT-ERROR                 
100700                 MOVE NOO            TO INDATA-SW                         
100800               END-IF                                                     
100900             END-IF                                                       
101000                                                                          
101100             IF REQU-REFOERS-OVKOFF NOT = ALL '+'                         
101200               IF REQU-REFOERS = ALL '+'                                  
101300                 MOVE 'REFOERS'      TO RESP-IDELMT-ERROR                 
101400                 MOVE NOO            TO INDATA-SW                         
101500               ELSE                                                       
101600                 MOVE REQU-REFOERS    TO DEC-IDFRIDATA                    
101700                 MOVE 2 TO DEC-KVHELTAL                                   
101800                 MOVE 3 TO DEC-KVDECIMAL                                  
101900                 PERFORM GB-HANDLE-ALPHA-TO-NUM                           
102000                                                                          
102100                 IF DEC-KDSVAR-OK                                         
102200                   MOVE DEC-IDEDITDATA TO WS-REFOERS                      
102300                   MOVE YES TO INPUT-122-SW                               
102400                   MOVE YES TO INPUT-211-SW                               
102500                 ELSE                                                     
102600                   MOVE 'REFOERS'      TO RESP-IDELMT-ERROR               
102700                   MOVE NOO            TO INDATA-SW                       
102800                 END-IF                                                   
102900               END-IF                                                     
103000                                                                          
103100               IF REQU-REOVKOFF NOT = ALL '+'                             
103200                 MOVE REQU-REOVKOFF   TO DEC-IDFRIDATA                    
103300                 MOVE 2 TO DEC-KVHELTAL                                   
103400                 MOVE 1 TO DEC-KVDECIMAL                                  
103500                 PERFORM GB-HANDLE-ALPHA-TO-NUM                           
103600                                                                          
103700                 IF DEC-KDSVAR-OK                                         
103800                   MOVE DEC-IDEDITDATA TO WS-REOVKOFF                     
103900                   MOVE YES TO INPUT-122-SW                               
104000                   MOVE YES TO INPUT-211-SW                               
104100                 ELSE                                                     
104200                   MOVE 'REOVKOFF'     TO RESP-IDELMT-ERROR               
104300                   MOVE NOO            TO INDATA-SW                       
104400                 END-IF                                                   
104500               ELSE                                                       
104600                 MOVE 'REOVKOFF'       TO RESP-IDELMT-ERROR               
104700                 MOVE NOO              TO INDATA-SW                       
104800               END-IF                                                     
104900             END-IF                                                       
105000           END-IF                                                         
105100         END-IF                                                           
105200                                                                          
105300         IF REQU-AVDRAG        NOT = ALL '+'                              
105400           MOVE YES TO INPUT-122-SW                                       
105500           MOVE YES TO INPUT-211-SW                                       
105600           IF REQU-REAVDRAG NOT = ALL '+'   AND                           
105700              REQU-PRAVDRAG NOT = ALL '+'                                 
105800             MOVE 'AVDRAG'        TO RESP-IDELMT-ERROR                    
105900             MOVE NOO             TO INDATA-SW                            
106000           ELSE                                                           
106100             IF REQU-REAVDRAG NOT = ALL '+'                               
106200               MOVE REQU-REAVDRAG     TO DEC-IDFRIDATA                    
106300               MOVE 2 TO DEC-KVHELTAL                                     
106400               MOVE 1 TO DEC-KVDECIMAL                                    
106500               PERFORM GB-HANDLE-ALPHA-TO-NUM                             
106600                                                                          
106700               IF DEC-KDSVAR-OK                                           
106800                 MOVE DEC-IDEDITDATA  TO WS-REAVDRAG                      
106900                 MOVE YES             TO INPUT-122-SW                     
107000                 MOVE YES             TO INPUT-211-SW                     
107100               ELSE                                                       
107200                 MOVE 'REAVDRAG'      TO RESP-IDELMT-ERROR                
107300                 MOVE NOO             TO INDATA-SW                        
107400               END-IF                                                     
107500             END-IF                                                       
107600                                                                          
107700             IF REQU-PRAVDRAG NOT = ALL '+'                               
107800               MOVE REQU-PRAVDRAG     TO DEC-IDFRIDATA                    
107900               MOVE 7 TO DEC-KVHELTAL                                     
108000               MOVE 2 TO DEC-KVDECIMAL                                    
108100               PERFORM GB-HANDLE-ALPHA-TO-NUM                             
108200                                                                          
108300               IF DEC-KDSVAR-OK                                           
108400                 MOVE DEC-IDEDITDATA  TO WS-PRAVDRAG                      
108500                 MOVE YES             TO INPUT-122-SW                     
108600                 MOVE YES             TO INPUT-211-SW                     
108700               ELSE                                                       
108800                 MOVE 'PRAVDRAG'      TO RESP-IDELMT-ERROR                
108900                 MOVE NOO             TO INDATA-SW                        
109000               END-IF                                                     
109100             END-IF                                                       
109200           END-IF                                                         
109300         END-IF                                                           
109400       END-IF                                                             
109500                                                                          
109600       IF REQU-FLSEPINV NOT = ALL '+'                                     
109700         IF REQU-FLSEPINV = 'J' OR 'Y'                                    
109800           MOVE YES TO INPUT-211-SW                                       
109900         ELSE                                                             
110000           IF REQU-FLSEPINV = SPACE                                       
110100             MOVE YES TO INPUT-211-SW                                     
110200           ELSE                                                           
110300             MOVE 'FLSEPINV'            TO RESP-IDELMT-ERROR              
110400             MOVE NOO                   TO INDATA-SW                      
110500           END-IF                                                         
110600         END-IF                                                           
110700       END-IF                                                             
110800                                                                          
110900       IF REQU-IDSIGILL NOT = ALL '+'                                     
111000         MOVE YES TO INPUT-122-SW                                         
111100       END-IF                                                             
111200                                                                          
111300       IF REQU-TISKEPPN-MAN NOT = ALL '+'                                 
111400         IF REQU-TISKEPPN-MAN NOT NUMERIC                                 
111500           MOVE 'TISKEPPN'          TO RESP-IDELMT-ERROR                  
111600           MOVE NOO                 TO INDATA-SW                          
111700         ELSE                                                             
111800           PERFORM GC-CHECK-DATE                                          
111900           IF DAT-KDSVAR-OK                                               
112000             MOVE YES TO INPUT-122-SW                                     
112100           ELSE                                                           
112200             MOVE 'TISKEPPN'        TO RESP-IDELMT-ERROR                  
112300             MOVE NOO               TO INDATA-SW                          
112400           END-IF                                                         
112500         END-IF                                                           
112600       END-IF                                                             
112700                                                                          
112800       IF REQU-IDLC NOT = ALL '+'                                         
112900         MOVE YES TO INPUT-122-SW                                         
113000       END-IF                                                             
113100                                                                          
113200       IF REQU-IDLICENS NOT = ALL '+'                                     
113300         MOVE YES TO INPUT-122-SW                                         
113400       END-IF                                                             
113500                                                                          
113600       IF REQU-IDBOKN NOT = ALL '+'                                       
113700         MOVE YES TO INPUT-122-SW                                         
113800       END-IF                                                             
113900                                                                          
114000       IF REQU-IDVCERT NOT = ALL '+'                                      
114100         MOVE YES TO INPUT-122-SW                                         
114200       END-IF                                                             
114300                                                                          
114400       IF REQU-BESLULEV NOT = ALL '+'                                     
114405         IF FUNCTION TRIM(REQU-BESLULEV) IS NUMERIC                       
114406          MOVE FUNCTION TRIM(REQU-BESLULEV) TO WS-BESLULEV                
114409          IF WS-BESLULEV >= 1 AND WS-BESLULEV <= 99999                    
114420           MOVE YES TO INPUT-122-SW                                       
114421          ELSE                                                            
114422           MOVE 'BESLULEV'       TO RESP-IDELMT-ERROR                     
114423           MOVE NOO              TO INDATA-SW                             
114424          END-IF                                                          
114425         ELSE                                                             
114426          MOVE 'BESLULEV'       TO RESP-IDELMT-ERROR                      
114427          MOVE NOO              TO INDATA-SW                              
114428         END-IF                                                           
114600       END-IF                                                             
114700                                                                          
114800       IF REQU-KDLEVVIL NOT = ALL '+'                                     
114900         IF REQU-KDLEVVIL NUMERIC                                         
115000           MOVE YES TO INPUT-111-SW                                       
115100                       INPUT-211-SW                                       
115200         ELSE                                                             
115300           MOVE 'KDLEVVIL'       TO RESP-IDELMT-ERROR                     
115400           MOVE NOO              TO INDATA-SW                             
115500         END-IF                                                           
115600       END-IF                                                             
115700                                                                          
115800       IF INDATA-WRONG                                                    
115900         MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                      
116000       END-IF                                                             
116100     END-IF                                                               
116200     .                                                                    
116300     EJECT                                                                
116400 GA-CHECK-SPACE-INPUT SECTION.                                            
116500                                                                          
116600** SPACE INPUT WILL BE IGNORED                                            
116700                                                                          
116800     IF REQU-KDVALUTA-MAN = SPACE                                         
116900       MOVE ALL '+'  TO REQU-KDVALUTA-MAN                                 
117000     END-IF                                                               
117100     IF REQU-PRKURS-MAN = SPACE                                           
117200       MOVE ALL '+'  TO REQU-PRKURS-MAN                                   
117300     END-IF                                                               
117400     IF REQU-PRLEGKST = SPACE                                             
117500       MOVE ALL '+'  TO REQU-PRLEGKST                                     
117600     END-IF                                                               
117700     IF REQU-PREMBHNT = SPACE                                             
117800       MOVE ALL '+'  TO REQU-PREMBHNT                                     
117900     END-IF                                                               
118000     IF REQU-REEMBHNT = SPACE                                             
118100       MOVE ALL '+'  TO REQU-REEMBHNT                                     
118200     END-IF                                                               
118300     IF REQU-PRFOERS = SPACE                                              
118400       MOVE ALL '+'  TO REQU-PRFOERS                                      
118500     END-IF                                                               
118600     IF REQU-REFOERS = SPACE                                              
118700       MOVE ALL '+'  TO REQU-REFOERS                                      
118800     END-IF                                                               
118900     IF REQU-REOVKOFF = SPACE                                             
119000       MOVE ALL '+'  TO REQU-REOVKOFF                                     
119100     END-IF                                                               
119200     IF REQU-PRAVDRAG = SPACE                                             
119300       MOVE ALL '+'  TO REQU-PRAVDRAG                                     
119400     END-IF                                                               
119500     IF REQU-REAVDRAG = SPACE                                             
119600       MOVE ALL '+'  TO REQU-REAVDRAG                                     
119700     END-IF                                                               
119800     .                                                                    
119900     EJECT                                                                
120000                                                                          
120100 GB-HANDLE-ALPHA-TO-NUM SECTION.                                          
120200                                                                          
120300     CALL WDECEDIT USING DEC-IDFRIDATA                                    
120400                         DEC-IDEDITDATA                                   
120500                         DEC-KVHELTAL                                     
120600                         DEC-KVDECIMAL                                    
120700                         DEC-KDSVAR                                       
120800     .                                                                    
120900     SKIP2                                                                
121000                                                                          
121100 GC-CHECK-DATE SECTION.                                                   
121200                                                                          
121300     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
121400     MOVE REQU-TISKEPPN-MAN    TO DAT-I-TIDATUM                           
121500                                                                          
121600     CALL WDATKONV          USING DAT-KDDATFORM                           
121700                                  DAT-I-TIDATUM                           
121800                                  DAT-O-TIDATUM                           
121900                                  DAT-KDSVAR                              
122000     .                                                                    
122100     EJECT                                                                
122200                                                                          
122300 H-UPDATE SECTION.                                                        
122400                                                                          
122500     MOVE YES    TO WDE122-ISRT-SW                                        
122600                    WDE211-ISRT-SW                                        
122700     PERFORM HA-DC-LAND                                                   
122800                                                                          
122900     IF INPUT-122 OR INPUT-211 OR INPUT-111                               
123000** READ FIRST 4498 SEGMENT FOR IDDISTR                                    
123100       MOVE REQU-IDDISTR                                                  
123200                         TO W-4498-IDDISTR-MIN                            
123300                            W-4498-IDDISTR-MAX                            
123400                                                                          
123500       IF FLSAMFAK                                                        
123600         MOVE ZERO       TO W-4498-IDKUNDNR-MIN                           
123700         MOVE 9999999    TO W-4498-IDKUNDNR-MAX                           
123800       ELSE                                                               
123900         MOVE REQU-IDKUNDNR                                               
124000                         TO W-4498-IDKUNDNR-MIN                           
124100                            W-4498-IDKUNDNR-MAX                           
124200       END-IF                                                             
124300                                                                          
124400       PERFORM IMS-GU-WDGX4498                                            
124500       IF SEGMENT-FOUND                                                   
124600         MOVE 4498-IDKUNDNR TO W-WDE111-IDKUNDNR                          
124700                               W-WDE211-IDKUNDNR                          
124800         IF FLSAMFAK                                                      
124900            MOVE ZERO       TO W-WDE211-IDKUNDNR                          
125000         END-IF                                                           
125100         IF W-IDSHIPM  > ZERO                                             
125200           PERFORM HB-TEST-WDE122                                         
125300           IF INPUT-122                                                   
125400             PERFORM HC-UPDATE-WDE122                                     
125500           END-IF                                                         
125600                                                                          
125700           PERFORM IMS-GHU-WDE201                                         
125800           PERFORM IMS-GHNP-WDE211                                        
125900           PERFORM HD-TEST-WDE211                                         
126000           IF INPUT-211                                                   
126100             PERFORM HE-UPDATE-WDE211                                     
126200           END-IF                                                         
126300                                                                          
126400         ELSE                                                             
126500           PERFORM HF-CREATE-IDSHIPM                                      
126600           IF INPUT-122                                                   
126700             PERFORM HC-UPDATE-WDE122                                     
126800           END-IF                                                         
126900                                                                          
127000           IF INPUT-211                                                   
127100             PERFORM HE-UPDATE-WDE211                                     
127200           END-IF                                                         
127300         END-IF                                                           
127400         MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                          
127500                                                                          
127600       ELSE                                                               
127700         MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                         
127800         MOVE 'SEGMENT'       TO RESP-IDELMT-ERROR                        
127900                                                                          
128000       END-IF                                                             
128100     ELSE                                                                 
128200                                                                          
128300       MOVE ERR-PF11-NO-DATA  TO RESP-IDMSG-ERROR                         
128400                                                                          
128500     END-IF                                                               
128600                                                                          
128700     .                                                                    
128800     EJECT                                                                
128900                                                                          
129000 HA-DC-LAND  SECTION.                                                     
129100                                                                          
129200     IF W-IDDC NOT = W-IDDC-B6                                            
129300        MOVE W-IDDC TO W-IDDC-B6                                          
129400        PERFORM IMS-GU-WDB601                                             
129500     END-IF                                                               
129600     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
129700     .                                                                    
129800     EJECT                                                                
129900                                                                          
130000 HB-TEST-WDE122         SECTION.                                          
130100                                                                          
130200     MOVE YES    TO WDE122-ISRT-SW                                        
130300     PERFORM IMS-GHU-WDE101                                               
130400     PERFORM IMS-GHNP-WDE111                                              
130500     IF SEGMENT-FOUND                                                     
130600       IF INPUT-111                                                       
130700         IF REQU-KDLEVVIL NOT = ALL '+'                                   
130800           MOVE REQU-KDLEVVIL TO SGMT-KDLEVVIL                            
130900           PERFORM IMS-REPL-WDE111                                        
131000         END-IF                                                           
131100       END-IF                                                             
131200       PERFORM IMS-GHNP-WDE122                                            
131300     ELSE                                                                 
131400       MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                            
131500       MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                           
131600       MOVE  'N'              TO  SGMT-FLCOD                              
131700       MOVE  W-IDDC           TO  SGMT-IDDC                               
131800       MOVE  SPACE            TO  SGMT-IDPARTNR                           
131900       MOVE  ZERO             TO  SGMT-KDFKBIL                            
132000                                  SGMT-KDFORSKN                           
132100       IF INPUT-111                                                       
132200         IF REQU-KDLEVVIL NOT = ALL '+'                                   
132300           MOVE REQU-KDLEVVIL TO  SGMT-KDLEVVIL                           
132400         ELSE                                                             
132500           MOVE  -1           TO  SGMT-KDLEVVIL                           
132600         END-IF                                                           
132700       ELSE                                                               
132800         MOVE  -1             TO  SGMT-KDLEVVIL                           
132900       END-IF                                                             
133000                                                                          
133100       MOVE   9               TO  SGMT-KDORDKL-MAX                        
133200       MOVE  SPACE            TO  SGMT-KDVALISO                           
133300       MOVE  ZERO             TO  SGMT-PRKURS                             
133400       COMPUTE SGMT-TISKEPPN-9KOMPL                                       
133500                              =   W-9KOMPL - SHIP-TISKEPPN                
133600       PERFORM IMS-ISRT-WDE111                                            
133700       PERFORM IMS-GHU-WDE122                                             
133800     END-IF                                                               
133900                                                                          
134000     IF SEGMENT-FOUND                                                     
134100       MOVE NOO TO WDE122-ISRT-SW                                         
134200     ELSE                                                                 
134300       MOVE 1   TO TILL-KDSEGKEY                                          
134400                                                                          
134500       MOVE SPACE TO TILL-IDSIGILL                                        
134600                     TILL-BESLULEV                                        
134700                     TILL-IDBOKN                                          
134800                     TILL-IDLC                                            
134900                     TILL-IDLICENS                                        
135000                     TILL-IDVCERT                                         
135100                     TILL-KDVALISO-MAN                                    
135110                     TILL-FILLER                                          
135120                     TILL-FILLER1                                         
135200                                                                          
135300       MOVE ZERO TO  TILL-PRFRAKT                                         
135400                     TILL-PRFOERS                                         
135500                     TILL-PRKURS-MAN                                      
135600                     TILL-PRLEGKST                                        
135700                     TILL-RELEGKST                                        
135800                     TILL-PREMBHNT                                        
135900                     TILL-REEMBHNT                                        
136000                     TILL-PRAVDRAG                                        
136100                     TILL-REAVDRAG                                        
136200                     TILL-REFOERS                                         
136300                     TILL-REOVKOFF                                        
136400                     TILL-TISKEPPN-MAN                                    
136500     END-IF                                                               
136600     .                                                                    
136700     SKIP2                                                                
136800                                                                          
136900 HC-UPDATE-WDE122        SECTION.                                         
137000                                                                          
137100     IF NOT DIST79-DEALER-PRICE                                           
137200       IF REQU-PRFRAKT NOT = ALL '+'                                      
137300         MOVE WS-PRFRAKT TO TILL-PRFRAKT                                  
137400       END-IF                                                             
137500                                                                          
137600       IF REQU-FOERS NOT = ALL '+'                                        
137700         IF REQU-PRFOERS NOT = ALL '+'                                    
137800           MOVE WS-PRFOERS TO TILL-PRFOERS                                
137900           MOVE ZERO TO TILL-REFOERS                                      
138000                        TILL-REOVKOFF                                     
138100         ELSE                                                             
138200           MOVE WS-REFOERS  TO TILL-REFOERS                               
138300           MOVE WS-REOVKOFF TO TILL-REOVKOFF                              
138400           MOVE ZERO TO TILL-PRFOERS                                      
138500         END-IF                                                           
138600       END-IF                                                             
138700                                                                          
138800       IF REQU-LEGKST NOT = ALL '+'                                       
138900         IF REQU-PRLEGKST NOT = ALL '+'                                   
139000           MOVE WS-PRLEGKST TO TILL-PRLEGKST                              
139100           MOVE ZERO          TO TILL-RELEGKST                            
139200         END-IF                                                           
139300                                                                          
139400         IF REQU-RELEGKST NOT = ALL '+'                                   
139500           MOVE WS-RELEGKST TO TILL-RELEGKST                              
139600           MOVE ZERO          TO TILL-PRLEGKST                            
139700         END-IF                                                           
139800       END-IF                                                             
139900                                                                          
140000       IF REQU-EMBHNT NOT = ALL '+'                                       
140100         IF REQU-PREMBHNT NOT = ALL '+'                                   
140200           MOVE WS-PREMBHNT TO TILL-PREMBHNT                              
140300           MOVE ZERO          TO TILL-REEMBHNT                            
140400         ELSE                                                             
140500           MOVE WS-REEMBHNT TO TILL-REEMBHNT                              
140600           MOVE ZERO          TO TILL-PREMBHNT                            
140700         END-IF                                                           
140800       END-IF                                                             
140900                                                                          
141000       IF REQU-AVDRAG NOT = ALL '+'                                       
141100         IF REQU-PRAVDRAG NOT = ALL '+'                                   
141200           MOVE WS-PRAVDRAG TO TILL-PRAVDRAG                              
141300           MOVE ZERO          TO TILL-REAVDRAG                            
141400         END-IF                                                           
141500                                                                          
141600         IF REQU-REAVDRAG NOT = ALL '+'                                   
141700           MOVE WS-REAVDRAG TO TILL-REAVDRAG                              
141800           MOVE ZERO          TO TILL-PRAVDRAG                            
141900         END-IF                                                           
142000       END-IF                                                             
142100                                                                          
142200       IF REQU-VALUTA NOT = ALL '+'                                       
142300         MOVE   CURR-KDVALISO-ROW TO TILL-KDVALISO-MAN                    
142400         MOVE   WS-PRKURS-MAN   TO TILL-PRKURS-MAN                        
142500       END-IF                                                             
142600     END-IF                                                               
142700                                                                          
142800     IF REQU-IDSIGILL NOT = ALL '+'                                       
142900       MOVE REQU-IDSIGILL TO TILL-IDSIGILL                                
142910       MOVE SPACE TO TILL-FILLER                                          
143000     END-IF                                                               
143100                                                                          
143200     IF REQU-TISKEPPN-MAN NOT = ALL '+'                                   
143300       MOVE REQU-TISKEPPN-MAN TO TILL-TISKEPPN-MAN                        
143400     END-IF                                                               
143500                                                                          
143600     IF REQU-IDLC NOT = ALL '+'                                           
143700       MOVE REQU-IDLC TO TILL-IDLC                                        
143800     END-IF                                                               
143900                                                                          
144000     IF REQU-IDLICENS NOT = ALL '+'                                       
144100       MOVE REQU-IDLICENS TO TILL-IDLICENS                                
144200     END-IF                                                               
144300                                                                          
144400     IF REQU-IDBOKN NOT = ALL '+'                                         
144500       MOVE REQU-IDBOKN TO TILL-IDBOKN                                    
144600     END-IF                                                               
144700                                                                          
144800     IF REQU-IDVCERT NOT = ALL '+'                                        
144900       MOVE REQU-IDVCERT TO TILL-IDVCERT                                  
145000     END-IF                                                               
145100                                                                          
145200     IF REQU-BESLULEV NOT = ALL '+'                                       
145300       MOVE WS-BESLULEV TO TILL-BESLULEV                                  
145310       INSPECT TILL-BESLULEV REPLACING LEADING ZERO BY SPACE              
145320       MOVE SPACE TO TILL-FILLER1                                         
145400     END-IF                                                               
145500                                                                          
145600     IF WDE122-ISRT                                                       
145700       PERFORM IMS-ISRT-WDE122                                            
145800     ELSE                                                                 
145900       PERFORM IMS-REPL-WDE122                                            
146000     END-IF                                                               
146100                                                                          
146200     .                                                                    
146300     EJECT                                                                
146400 HD-TEST-WDE211         SECTION.                                          
146500                                                                          
146600     MOVE YES    TO WDE211-ISRT-SW                                        
146700                                                                          
146800     IF SEGMENT-FOUND                                                     
146900       MOVE NOO TO WDE211-ISRT-SW                                         
147000     ELSE                                                                 
147100       MOVE 4498-IDDISTR  TO BGMT-IDDISTR                                 
147200       MOVE 4498-IDKUNDNR TO BGMT-IDKUNDNR                                
147300                                                                          
147400       MOVE SPACE    TO BGMT-IDPARTNR                                     
147500                        BGMT-FLSEPINV                                     
147600       MOVE 'N'      TO BGMT-FLCOD                                        
147700                                                                          
147800       MOVE -1       TO BGMT-KDLEVVIL                                     
147900       MOVE ZERO     TO BGMT-PRAVDRAG                                     
148000                        BGMT-PREMBHNT                                     
148100                        BGMT-PRFOERS                                      
148200                        BGMT-PRFRAKT                                      
148300                        BGMT-PRLEGKST                                     
148400                        BGMT-REAVDRAG                                     
148500                        BGMT-REEMBHNT                                     
148600                        BGMT-REFOERS                                      
148700                        BGMT-RELEGKST                                     
148800                        BGMT-REOVKOFF                                     
148900     END-IF                                                               
149000                                                                          
149100     .                                                                    
149200     SKIP2                                                                
149300                                                                          
149400 HE-UPDATE-WDE211  SECTION.                                               
149500                                                                          
149600     MOVE W-IDDISTR TO  TEST-IDDISTR                                      
149700     IF REQU-FLSEPINV = 'Y' OR 'J'                                        
149800       MOVE 'J'          TO BGMT-FLSEPINV                                 
149900     ELSE                                                                 
150000       IF REQU-FLSEPINV = SPACE                                           
150100         MOVE SPACE      TO BGMT-FLSEPINV                                 
150200       END-IF                                                             
150300     END-IF                                                               
150400     IF DIST92-ITALY                                                      
150500       MOVE 'J'          TO BGMT-FLSEPINV                                 
150600     END-IF                                                               
150700                                                                          
150800     IF REQU-KDLEVVIL NOT = ALL '+'                                       
150900       MOVE REQU-KDLEVVIL TO BGMT-KDLEVVIL                                
151000     END-IF                                                               
151100                                                                          
151200     IF NOT DIST79-DEALER-PRICE                                           
151300       IF REQU-PRFRAKT NOT = ALL '+'                                      
151400         MOVE WS-PRFRAKT TO BGMT-PRFRAKT                                  
151500       END-IF                                                             
151600                                                                          
151700       IF REQU-FOERS NOT = ALL '+'                                        
151800         IF REQU-PRFOERS NOT = ALL '+'                                    
151900           MOVE WS-PRFOERS TO BGMT-PRFOERS                                
152000           MOVE ZERO TO BGMT-REFOERS                                      
152100                        BGMT-REOVKOFF                                     
152200         ELSE                                                             
152300           MOVE WS-REFOERS  TO BGMT-REFOERS                               
152400           MOVE WS-REOVKOFF TO BGMT-REOVKOFF                              
152500           MOVE ZERO TO BGMT-PRFOERS                                      
152600         END-IF                                                           
152700       END-IF                                                             
152800                                                                          
152900       IF REQU-LEGKST NOT = ALL '+'                                       
153000         IF REQU-PRLEGKST NOT = ALL '+'                                   
153100           MOVE WS-PRLEGKST TO BGMT-PRLEGKST                              
153200           MOVE ZERO          TO BGMT-RELEGKST                            
153300         END-IF                                                           
153400                                                                          
153500         IF REQU-RELEGKST NOT = ALL '+'                                   
153600           MOVE WS-RELEGKST TO BGMT-RELEGKST                              
153700           MOVE ZERO          TO BGMT-PRLEGKST                            
153800         END-IF                                                           
153900       END-IF                                                             
154000                                                                          
154100       IF REQU-EMBHNT NOT = ALL '+'                                       
154200         IF REQU-PREMBHNT NOT = ALL '+'                                   
154300           MOVE WS-PREMBHNT TO BGMT-PREMBHNT                              
154400           MOVE ZERO TO BGMT-REEMBHNT                                     
154500         ELSE                                                             
154600           MOVE WS-REEMBHNT TO BGMT-REEMBHNT                              
154700           MOVE ZERO          TO BGMT-PREMBHNT                            
154800         END-IF                                                           
154900       END-IF                                                             
155000                                                                          
155100       IF REQU-AVDRAG NOT = ALL '+'                                       
155200         IF REQU-PRAVDRAG NOT = ALL '+'                                   
155300           MOVE WS-PRAVDRAG TO BGMT-PRAVDRAG                              
155400         END-IF                                                           
155500                                                                          
155600         IF REQU-REAVDRAG NOT = ALL '+'                                   
155700           MOVE WS-REAVDRAG TO BGMT-REAVDRAG                              
155800         END-IF                                                           
155900       END-IF                                                             
156000     END-IF                                                               
156100                                                                          
156200     IF WDE211-ISRT                                                       
156300       PERFORM IMS-ISRT-WDE211                                            
156400     ELSE                                                                 
156500       PERFORM IMS-REPL-WDE211                                            
156600     END-IF                                                               
156700     .                                                                    
156800     EJECT                                                                
156900                                                                          
157000                                                                          
157100 HF-CREATE-IDSHIPM  SECTION.                                              
157200                                                                          
157300     CALL W476SHNO         USING SHNO-W476SHNO SHNO-4517-PCB              
157400                                                                          
157500     MOVE SHNO-IDSHIPM        TO W-IDSHIPM                                
157600                                 RESP-IDSHIPM                             
157700                                                                          
157800** CREATE WDE101                                                          
157900                                                                          
158000     MOVE W-IDSHIPM           TO SHIP-IDSHIPM                             
158100     MOVE W-IDTRPTNR          TO SHIP-IDTRPTNR                            
158200     MOVE W-IDLBBET           TO SHIP-IDLBBET                             
158300     MOVE W-IDDC              TO SHIP-IDDC                                
158400     MOVE W-IDLANDX2          TO SHIP-IDLANDX3-SEND                       
158500     MOVE SHIP-IDDC           TO WS-IDDC                                  
158600     IF NDC-CN OR LDC-CN                                                  
158700       IF 4498-KDFAKTYP = 'R' or 'G'                                      
158800         MOVE 'INV'           TO SHIP-KDFINDOC                            
158900       ELSE                                                               
159000         IF 4498-KDFAKTYP = 'N' OR 'K'                                    
159100           MOVE 'INT'         TO SHIP-KDFINDOC                            
159200         END-IF                                                           
159300       END-IF                                                             
159400     ELSE                                                                 
159500       IF 4498-KDFAKTYP = 'N' OR 'K'                                      
159600         MOVE 'INT'           TO SHIP-KDFINDOC                            
159700       ELSE                                                               
159800         MOVE 'INV'           TO SHIP-KDFINDOC                            
159900       END-IF                                                             
160000     END-IF                                                               
160100     MOVE '011'                       TO MSGI-KDCALL                      
160200     MOVE DCS-IDTIDZON                TO MSGI-IDTIDZON                    
160210     MOVE DCS-IDDC                    TO MSGI-IDDC                        
160300     MOVE WS-DAGENS-DATUM             TO MSGI-TILOKDAT                    
160400     MOVE WS-TIDPUNKT                 TO MSGI-TILOKTID                    
160500     CALL WL01TIDZ USING MSGI-WL01TIDZ                                    
160600     MOVE MSGI-TILOKDAT(1:6)          TO SHIP-TISKEPPN                    
160700     MOVE MSGI-TILOKTID(1:4)          TO WS-TISKPTID-HHMM                 
160800     MOVE FUNCTION CURRENT-DATE(13:2) TO WS-TISKPTID-SS                   
160900     MOVE WS-TISKPTID                 TO SHIP-TISKPTID                    
161000     MOVE ZERO                        TO SHIP-KVANTEX                     
161100                                         SHIP-SUNTO-TOT                   
161200                                         SHIP-PRKURS-BET                  
161300     MOVE REQU-FLSKRIV-NU             TO SHIP-FLSKRIV-NU                  
161400     MOVE 'A'                         TO SHIP-KDKLAR                      
161500     MOVE SPACE                       TO SHIP-IDDC-EXP                    
161600                                         SHIP-BELEVVIL                    
161700                                         SHIP-IDSYSTEM                    
161800                                         SHIP-KDVALISO-BET                
161900     MOVE '0'                         TO SHIP-KDFAKSTA-EXP                
161910                                                                          
161920     MOVE NOO                         TO SHIP-FLFARLIG                    
161930     MOVE SPACES                      TO SHIP-KDVALISO-EXP                
161940     MOVE ZEROES                      TO SHIP-SUORDV-EXP                  
161950                                         SHIP-SUORDV-FAKT                 
161960                                         SHIP-VKORDBTO-FAKT               
161970                                         SHIP-VLORDBTO-FAKT               
161980                                                                          
162000     PERFORM IMS-ISRT-WDE101                                              
162100                                                                          
162200** CREATE WDE111                                                          
162300                                                                          
162400     MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                              
162500     MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                             
162600     MOVE  'N'              TO  SGMT-FLCOD                                
162700     MOVE  W-IDDC           TO  SGMT-IDDC                                 
162800     MOVE  SPACE            TO  SGMT-IDPARTNR                             
162900     MOVE  ZERO             TO  SGMT-KDFKBIL                              
163000                                SGMT-KDFORSKN                             
163100     IF INPUT-111                                                         
163200       IF REQU-KDLEVVIL NOT = ALL '+'                                     
163300         MOVE REQU-KDLEVVIL TO  SGMT-KDLEVVIL                             
163400       ELSE                                                               
163500         MOVE -1            TO  SGMT-KDLEVVIL                             
163600       END-IF                                                             
163700     ELSE                                                                 
163800       MOVE  -1             TO  SGMT-KDLEVVIL                             
163900     END-IF                                                               
164000     MOVE   9               TO  SGMT-KDORDKL-MAX                          
164100     MOVE  SPACE            TO  SGMT-KDVALISO                             
164200     MOVE  ZERO             TO  SGMT-PRKURS                               
164300     COMPUTE SGMT-TISKEPPN-9KOMPL                                         
164400                            =   W-9KOMPL - SHIP-TISKEPPN                  
164500     PERFORM IMS-ISRT-WDE111                                              
164600                                                                          
164700** BUILD WDE122                                                           
164800                                                                          
164900     MOVE YES    TO WDE122-ISRT-SW                                        
165000     MOVE 1   TO TILL-KDSEGKEY                                            
165100                                                                          
165200     MOVE SPACE TO TILL-IDSIGILL                                          
165300                   TILL-BESLULEV                                          
165400                   TILL-IDBOKN                                            
165500                   TILL-IDLC                                              
165600                   TILL-IDLICENS                                          
165700                   TILL-IDVCERT                                           
165800                   TILL-KDVALISO-MAN                                      
165900                                                                          
166000     MOVE ZERO TO  TILL-PRFRAKT                                           
166100                   TILL-PRFOERS                                           
166200                   TILL-PRKURS-MAN                                        
166300                   TILL-PRLEGKST                                          
166400                   TILL-RELEGKST                                          
166500                   TILL-PREMBHNT                                          
166600                   TILL-REEMBHNT                                          
166700                   TILL-PRAVDRAG                                          
166800                   TILL-REAVDRAG                                          
166900                   TILL-REFOERS                                           
167000                   TILL-REOVKOFF                                          
167100                   TILL-TISKEPPN-MAN                                      
167200                                                                          
167300** BUILD AND ISRT WDE201                                                  
167400     MOVE W-IDSHIPM      TO BILL-IDSHIPM                                  
167500     MOVE W-IDDC         TO BILL-IDDC                                     
167600     MOVE W-IDLANDX2     TO BILL-IDLANDX3-SEND                            
167700     MOVE ZERO           TO BILL-IDLEVNR                                  
167800     MOVE BILL-IDDC      TO WS-IDDC                                       
167900     IF NDC-CN OR LDC-CN                                                  
168000       IF 4498-KDFAKTYP = 'R' OR 'G'                                      
168100         MOVE 'INV'      TO BILL-KDFINDOC                                 
168200       ELSE                                                               
168300         IF 4498-KDFAKTYP = 'N' OR 'K'                                    
168400           MOVE 'INT'      TO BILL-KDFINDOC                               
168500         END-IF                                                           
168600       END-IF                                                             
168700     ELSE                                                                 
168800       IF 4498-KDFAKTYP = 'N' OR 'K'                                      
168900         MOVE 'INT'      TO BILL-KDFINDOC                                 
169000       ELSE                                                               
169100         MOVE 'INV'      TO BILL-KDFINDOC                                 
169200       END-IF                                                             
169300     END-IF                                                               
169400     MOVE MSGI-TILOKDAT(1:6) TO BILL-TISKEPPN                             
169500     MOVE WS-TISKPTID        TO BILL-TISKPTID                             
169600     MOVE SPACE              TO BILL-IDDC-EXP                             
169700     PERFORM IMS-ISRT-WDE201                                              
169800                                                                          
169900** BUILD WDE211                                                           
170000     MOVE 4498-IDDISTR  TO BGMT-IDDISTR                                   
170100     IF FLSAMFAK                                                          
170200       MOVE ZERO          TO BGMT-IDKUNDNR                                
170300     ELSE                                                                 
170400       MOVE 4498-IDKUNDNR TO BGMT-IDKUNDNR                                
170500     END-IF                                                               
170600                                                                          
170700     MOVE SPACE    TO BGMT-IDPARTNR                                       
170800                      BGMT-FLSEPINV                                       
170900     MOVE 'N'      TO BGMT-FLCOD                                          
171000                                                                          
171100     MOVE -1       TO BGMT-KDLEVVIL                                       
171200     MOVE ZERO     TO BGMT-PRAVDRAG                                       
171300                      BGMT-PREMBHNT                                       
171400                      BGMT-PRFOERS                                        
171500                      BGMT-PRFRAKT                                        
171600                      BGMT-PRLEGKST                                       
171700                      BGMT-REAVDRAG                                       
171800                      BGMT-REEMBHNT                                       
171900                      BGMT-REFOERS                                        
172000                      BGMT-RELEGKST                                       
172100                      BGMT-REOVKOFF                                       
172200                                                                          
172300     MOVE YES    TO WDE211-ISRT-SW                                        
172400                                                                          
172500     .                                                                    
172600     EJECT                                                                
172700                                                                          
172800 S02-ERASE-FIELDS SECTION.                                                
172900                                                                          
173000     MOVE ALL-SPACE       TO RESP-PRFRAKT                                 
173100                             RESP-PRFOERS                                 
173200                             RESP-REFOERS                                 
173300                             RESP-REOVKOFF                                
173400                             RESP-PRLEGKST                                
173500                             RESP-RELEGKST                                
173600                             RESP-PREMBHNT                                
173700                             RESP-REEMBHNT                                
173800                             RESP-PRAVDRAG                                
173900                             RESP-REAVDRAG                                
174000                             RESP-FLCREDL                                 
174100                             RESP-KDVALISO-MAN                            
174200                             RESP-PRKURS-MAN                              
174300     .                                                                    
174400     EJECT                                                                
174500                                                                          
174600 S03-ALL-SPACE-LINE SECTION.                                              
174700                                                                          
174800     MOVE ALL-SPACE       TO RESP-FLSEPINV                                
174900                             RESP-PRFRAKT                                 
175000                             RESP-PRFOERS                                 
175100                             RESP-REFOERS                                 
175200                             RESP-REOVKOFF                                
175300                             RESP-PRLEGKST                                
175400                             RESP-RELEGKST                                
175500                             RESP-PREMBHNT                                
175600                             RESP-REEMBHNT                                
175700                             RESP-PRAVDRAG                                
175800                             RESP-REAVDRAG                                
175900                             RESP-FLCREDL                                 
176000                             RESP-KDVALISO-MAN                            
176100                             RESP-PRKURS-MAN                              
176200                             RESP-IDSIGILL                                
176300                             RESP-TISKEPPN-MAN                            
176400                             RESP-IDLC                                    
176500                             RESP-IDLICENS                                
176600                             RESP-IDBOKN                                  
176700                             RESP-IDVCERT                                 
176800                             RESP-BESLULEV                                
176900                             RESP-KDLEVVIL                                
177000                             RESP-VKORDBTO                                
177100                             RESP-VLORDBTO                                
177200                             RESP-SUORDV                                  
177300     .                                                                    
177400     SKIP3                                                                
177500*    --- DISPATCHER SECTIONS                                              
177600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
177700                                                                          
177800     MOVE 'GETARG'               TO SUB-KDFUNC                            
177900     MOVE 'CARPARTS.NDC.CHANGETRANSPORT'                                  
178000                                 TO SUB-ADDISPABS                         
178100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
178200                                                                          
178300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
178400                                                                          
178500     IF SUB-KDRC > 0                                                      
178600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
178700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
178800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
178900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
179000     END-IF                                                               
179100     .                                                                    
179200                                                                          
179300 S04-RETURN-RESPONSE SECTION.                                             
179400                                                                          
179500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
179600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
179700                                                                          
179800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
179900                                                                          
180000     IF SUB-KDRC > 0                                                      
180100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
180200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
180300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
180400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
180500     END-IF                                                               
180600     .                                                                    
180700                                                                          
180800                                                                          
180900* --- IMS SECTIONS ---                                                    
181000     SKIP3                                                                
181100 IMS-GHU-WDE101 SECTION.                                                  
181200                                                                          
181300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
181400          DELIMITED BY SIZE INTO SSA1                                     
181500     MOVE '  ' TO GOOD-STATUSCODES                                        
181600     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
181700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSCHECK                                              
181900     .                                                                    
182000     SKIP3                                                                
182100 IMS-GHNP-WDE111 SECTION.                                                 
182200                                                                          
182300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
182400          DELIMITED BY SIZE INTO SSA1                                     
182500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
182600          DELIMITED BY SIZE INTO SSA2                                     
182700     MOVE '  GE' TO GOOD-STATUSCODES                                      
182800     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
182900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
183000     PERFORM IMS-STATUSCHECK                                              
183100     .                                                                    
183200     SKIP3                                                                
183300 IMS-REPL-WDE111 SECTION.                                                 
183400                                                                          
183500     MOVE '  ' TO GOOD-STATUSCODES                                        
183600     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE111                       
183700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
183800     PERFORM IMS-STATUSCHECK                                              
183900     .                                                                    
184000     EJECT                                                                
184100                                                                          
184200 IMS-GHNP-WDE122 SECTION.                                                 
184300                                                                          
184400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
184500          DELIMITED BY SIZE INTO SSA1                                     
184600     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
184700          DELIMITED BY SIZE INTO SSA2                                     
184800     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
184900          DELIMITED BY SIZE INTO SSA3                                     
185000     MOVE '  GE' TO GOOD-STATUSCODES                                      
185100     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3        
185200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
185300     PERFORM IMS-STATUSCHECK                                              
185400     .                                                                    
185500     SKIP3                                                                
185600 IMS-GHU-WDE122 SECTION.                                                  
185700                                                                          
185800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
185900          DELIMITED BY SIZE INTO SSA1                                     
186000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
186100          DELIMITED BY SIZE INTO SSA2                                     
186200     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
186300          DELIMITED BY SIZE INTO SSA3                                     
186400     MOVE '  GE' TO GOOD-STATUSCODES                                      
186500     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3         
186600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
186700     PERFORM IMS-STATUSCHECK                                              
186800     .                                                                    
186900     SKIP3                                                                
187000 IMS-GHU-WDE122-DIST SECTION.                                             
187100                                                                          
187200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
187300          DELIMITED BY SIZE INTO SSA1                                     
187400     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
187500                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
187600          DELIMITED BY SIZE INTO SSA2                                     
187700     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
187800          DELIMITED BY SIZE INTO SSA3                                     
187900     MOVE '  GE' TO GOOD-STATUSCODES                                      
188000     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3         
188100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
188200     PERFORM IMS-STATUSCHECK                                              
188300     .                                                                    
188400     EJECT                                                                
188500 IMS-GHU-WDE201 SECTION.                                                  
188600                                                                          
188700     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
188800          DELIMITED BY SIZE INTO SSA1                                     
188900     MOVE '  ' TO GOOD-STATUSCODES                                        
189000     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
189100     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
189200     PERFORM IMS-STATUSCHECK                                              
189300     .                                                                    
189400     SKIP3                                                                
189500 IMS-GHNP-WDE211 SECTION.                                                 
189600                                                                          
189700     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
189800          DELIMITED BY SIZE INTO SSA1                                     
189900     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
190000          DELIMITED BY SIZE INTO SSA2                                     
190100     MOVE '  GE' TO GOOD-STATUSCODES                                      
190200     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
190300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
190400     PERFORM IMS-STATUSCHECK                                              
190500     .                                                                    
190600     SKIP3                                                                
190700 IMS-GHU-WDE211 SECTION.                                                  
190800                                                                          
190900     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
191000          DELIMITED BY SIZE INTO SSA1                                     
191100     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
191200          DELIMITED BY SIZE INTO SSA2                                     
191300     MOVE '  GE' TO GOOD-STATUSCODES                                      
191400     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
191500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
191600     PERFORM IMS-STATUSCHECK                                              
191700     .                                                                    
191800     SKIP3                                                                
191900 IMS-GHU-WDE211-DIST SECTION.                                             
192000                                                                          
192100     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
192200          DELIMITED BY SIZE INTO SSA1                                     
192300     STRING 'WDE211  (WDE211KY>=' W-WDE211KY-MIN                          
192400                    '&WDE211KY<=' W-WDE211KY-MAX ')'                      
192500          DELIMITED BY SIZE INTO SSA2                                     
192600     MOVE '  GE' TO GOOD-STATUSCODES                                      
192700     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
192800     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUSCHECK                                              
193000     .                                                                    
193100     SKIP3                                                                
193200 IMS-ISRT-WDE101 SECTION.                                                 
193300                                                                          
193400     MOVE 'WDE101  ' TO SSA1                                              
193500     MOVE '  ' TO GOOD-STATUSCODES                                        
193600     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
193700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
193800     PERFORM IMS-STATUSCHECK                                              
193900     .                                                                    
194000     EJECT                                                                
194100 IMS-ISRT-WDE111 SECTION.                                                 
194200                                                                          
194300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
194400          DELIMITED BY SIZE INTO SSA1                                     
194500     MOVE 'WDE111  ' TO SSA2                                              
194600     MOVE '  ' TO GOOD-STATUSCODES                                        
194700     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
194800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
194900     PERFORM IMS-STATUSCHECK                                              
195000     .                                                                    
195100     EJECT                                                                
195200 IMS-ISRT-WDE122 SECTION.                                                 
195300                                                                          
195400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
195500          DELIMITED BY SIZE INTO SSA1                                     
195600     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
195700          DELIMITED BY SIZE INTO SSA2                                     
195800     MOVE 'WDE122 ' TO SSA3                                               
195900     MOVE '  II' TO GOOD-STATUSCODES                                      
196000     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3        
196100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSCHECK                                              
196300     .                                                                    
196400     SKIP3                                                                
196500 IMS-REPL-WDE122 SECTION.                                                 
196600                                                                          
196700     MOVE '  ' TO GOOD-STATUSCODES                                        
196800     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE122                       
196900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
197000     PERFORM IMS-STATUSCHECK                                              
197100     .                                                                    
197200     EJECT                                                                
197300                                                                          
197400 IMS-ISRT-WDE201 SECTION.                                                 
197500                                                                          
197600     MOVE 'WDE201  ' TO SSA1                                              
197700     MOVE '  ' TO GOOD-STATUSCODES                                        
197800     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
197900     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
198000     PERFORM IMS-STATUSCHECK                                              
198100     .                                                                    
198200     EJECT                                                                
198300                                                                          
198400 IMS-ISRT-WDE211 SECTION.                                                 
198500                                                                          
198600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
198700          DELIMITED BY SIZE INTO SSA1                                     
198800     MOVE 'WDE211 ' TO SSA2                                               
198900     MOVE '  II' TO GOOD-STATUSCODES                                      
199000     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
199100     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
199200     PERFORM IMS-STATUSCHECK                                              
199300     .                                                                    
199400     SKIP3                                                                
199500                                                                          
199600 IMS-REPL-WDE211 SECTION.                                                 
199700                                                                          
199800     MOVE '  ' TO GOOD-STATUSCODES                                        
199900     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE211                       
200000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
200100     PERFORM IMS-STATUSCHECK                                              
200200     .                                                                    
200300     EJECT                                                                
200400 IMS-GU-WDB201 SECTION.                                                   
200500                                                                          
200600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
200700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
200800          DELIMITED BY SIZE INTO SSA1                                     
200900     MOVE '  GE'              TO GOOD-STATUSCODES                         
201000                                                                          
201100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
201200     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
201300     PERFORM IMS-STATUSCHECK                                              
201400     .                                                                    
201500     EJECT                                                                
201600 IMS-GU-WDB101 SECTION.                                                   
201700                                                                          
201800     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
201900          DELIMITED BY SIZE INTO SSA1                                     
202000     MOVE '  GE'              TO GOOD-STATUSCODES                         
202100                                                                          
202200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
202300     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
202400     PERFORM IMS-STATUSCHECK                                              
202500     .                                                                    
202600     EJECT                                                                
202700                                                                          
202800 IMS-GU-WDGX4496   SECTION.                                               
202900                                                                          
203000     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
203100                      DELIMITED BY SIZE INTO SSA1                         
203200     MOVE   'WDGX4496 '                   TO SSA2                         
203300     MOVE '  GE' TO GOOD-STATUSCODES                                      
203400     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4496 SSA1 SSA2             
203500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
203600     PERFORM IMS-STATUSCHECK                                              
203700     .                                                                    
203800                                                                          
203900 IMS-GU-WDGX4498   SECTION.                                               
204000                                                                          
204100     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
204200                      DELIMITED BY SIZE INTO SSA1                         
204300     STRING 'WDGX4498(WDGXKEY >=' W-4498-MIN                              
204400                    '&WDGXKEY <=' W-4498-MAX  ')'                         
204500                      DELIMITED BY SIZE INTO SSA2                         
204600     MOVE '  GE' TO GOOD-STATUSCODES                                      
204700     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4498 SSA1 SSA2             
204800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
204900     PERFORM IMS-STATUSCHECK                                              
205000     .                                                                    
205100     SKIP2                                                                
205200 IMS-GU-WDB601    SECTION.                                                
205300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
205400          DELIMITED BY SIZE INTO SSA1                                     
205500     MOVE '  GE' TO GOOD-STATUSCODES                                      
205600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
205700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
205800     PERFORM IMS-STATUSCHECK                                              
205900     IF SEGMENT-MISSING                                                   
206000         MOVE SPACE TO DCS-KDDC                                           
206100                       DCS-IDLANDX2                                       
206200     END-IF                                                               
206300     .                                                                    
206400 IMS-STATUSCHECK SECTION.                                                 
206500                                                                          
206600     SET STATUS-IX TO 1                                                   
206700     SEARCH GOOD-STATUS                                                   
206800       AT END                                                             
206900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
207000         DELIMITED BY SIZE INTO ERROR-TEXT                                
207100         CALL FELLOG                                                      
207200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
207300         CONTINUE                                                         
207400     END-SEARCH                                                           
208000     .                                                                    
