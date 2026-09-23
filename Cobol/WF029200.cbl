000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF029200                                                 
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   20070514.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ABNORMALVALUELIST                                
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT/DELETE ABNORMAL SELECT TABLE (T01PDEV)        
001200*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'P' PRINT                                             
001600*                                                                         
001700*        THE PROGRAM READS   TABLE T01LSEL                                
001800*        THE PROGRAM UPDATES TABLE T01PDEV                                
001810*        THE PROGRAM READS   TABLE T01SDEV                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WF0292U                                             
002200*        REQUEST:     WF0292I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WF0292O1                                            
002510*        MAIL HEADER: WF029201                                            
002511*        MAIL HEADER: WF029202                                            
002520*        MAIL LINES:  WF029203                                            
002530*        MAIL LINES:  WF029204                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'WF029200'.            
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004200 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004301                                                                          
004500*    --- CONSTANT WORK FIELDS                                             
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  WS-ADRESS                   PIC X(50)                                
004900          VALUE 'CARPARTS.BILLIT.ABNORMALVALUELIST'.                      
004910 77  WS-ADRESS2                  PIC X(50)                                
004920                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
005000 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005100 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005200 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005300 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
005310 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005400                                                                          
005500 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005600     88  KEYS-OK                             VALUE 'Y'.                   
005700     88  KEYS-WRONG                          VALUE 'N'.                   
005800                                                                          
005900 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006000     88  ACT-CODE-VALID                 VALUE 'S', 'U', 'P'.              
006100     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006200     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006300     88  ACT-CODE-PRINT                      VALUE 'P'.                   
006400                                                                          
006500*    --- WORK-FIELDS                                                      
006510 01  IX                             PIC S9(9)  VALUE ZERO BINARY.         
006520 01  WS-IX                          PIC S9(9)  VALUE ZERO BINARY.         
006521 01  MAX-ROW                        PIC S9(9)  VALUE 500  BINARY.         
006530 01  WS-COUNTER-T01SDEV             PIC S9(7) COMP-3 VALUE ZERO.          
006600 01  WS-CURRENT-DATE                PIC X(8)    VALUE SPACE.              
006700 01  WS-KVMANAD-NUM                 PIC S9(2) COMP-3 VALUE ZERO.          
006710 01  WS-BETEXT-A                    PIC X(20)   VALUE SPACE.              
006720 01  WS-BETEXT-B                    PIC X(20)   VALUE SPACE.              
006730 01  WS-BETEXT-C                    PIC X(20)   VALUE SPACE.              
006740 01  WS-BETEXT-D                    PIC X(20)   VALUE SPACE.              
006750 01  WS-BETEXT-E                    PIC X(20)   VALUE SPACE.              
006760 01  WS-BETEXT-F                    PIC X(20)   VALUE SPACE.              
006770 01  WS-BETEXT-G                    PIC X(20)   VALUE SPACE.              
006780 01  WS-REARTRAB                    PIC S9(2)V9(2) COMP-3.                
006790 01  WS-PRARTNTO-MIN                PIC S9(7)V9(2) COMP-3.                
006791 01  WS-PRARTNTO-MAX                PIC S9(7)V9(2) COMP-3.                
006792 01  WS-SUNTO-MIN                   PIC S9(11)V9(2) COMP-3.               
006793 01  WS-SUNTO-MAX                   PIC S9(11)V9(2) COMP-3.               
006794 01  WS-FLSOFT                      PIC X(1)    VALUE SPACE.              
006795 01  WS-FLFREE                      PIC X(1)    VALUE SPACE.              
006796 01  WS-DAREGDAT                    PIC X(8)    VALUE SPACE.              
006797 01  WS-DAUPPDAT                    PIC X(8)    VALUE SPACE.              
006798 01  WS-DAREGDAT-MIN-DATE           PIC X(8)    VALUE SPACE.              
006799 01  WS-DAREGDAT-MAX-DATE           PIC X(8)    VALUE SPACE.              
006800                                                                          
006900 01  WS-REARTRAB-RED                PIC Z(1)9.9(3).                       
007000 01  WS-REARTRAB-NUM                PIC S9(2)V9(3) COMP-3.                
007100 01  WS-REARTRAB-DEC                PIC 9(2)V9(3)  VALUE ZERO.            
007200 01  WS-REARTRAB-HELTAL             PIC 9(2).                             
007300                                                                          
007400 01  WS-REARTRAB-JUST2              PIC 9(2)V9(3)  VALUE ZERO.            
007500 01  WS-REARTRAB-JUST               PIC 9(2)V9(3)  VALUE ZERO.            
007600 01  FILLER REDEFINES WS-REARTRAB-JUST.                                   
007700     03  FILLER                     PIC 9(2)V9(2).                        
007800     03  WS-REARTRAB-SIST           PIC 9(1).                             
007900                                                                          
008000 01  WS-PRARTNTO-MIN-RED            PIC Z(6)9.9(3).                       
008100 01  WS-PRARTNTO-MIN-NUM            PIC S9(7)V9(3) COMP-3.                
008200 01  WS-PRARTNTO-MIN-DEC            PIC 9(7)V9(3)  VALUE ZERO.            
008300 01  WS-PRARTNTO-MIN-HELTAL         PIC 9(7).                             
008400                                                                          
008500 01  WS-PRARTNTO-MIN-JUST2          PIC 9(7)V9(3)  VALUE ZERO.            
008600 01  WS-PRARTNTO-MIN-JUST           PIC 9(7)V9(3)  VALUE ZERO.            
008700 01  FILLER REDEFINES WS-PRARTNTO-MIN-JUST.                               
008800     03  FILLER                     PIC 9(7)V9(2).                        
008900     03  WS-PRARTNTO-MIN-SIST       PIC 9(1).                             
009000                                                                          
009100 01  WS-PRARTNTO-MAX-RED            PIC Z(6)9.9(3).                       
009200 01  WS-PRARTNTO-MAX-NUM            PIC S9(7)V9(3) COMP-3.                
009300 01  WS-PRARTNTO-MAX-DEC            PIC 9(7)V9(3)  VALUE ZERO.            
009400 01  WS-PRARTNTO-MAX-HELTAL         PIC 9(7).                             
009500                                                                          
009600 01  WS-PRARTNTO-MAX-JUST2          PIC 9(7)V9(3)  VALUE ZERO.            
009700 01  WS-PRARTNTO-MAX-JUST           PIC 9(7)V9(3)  VALUE ZERO.            
009800 01  FILLER REDEFINES WS-PRARTNTO-MAX-JUST.                               
009900     03  FILLER                     PIC 9(7)V9(2).                        
010000     03  WS-PRARTNTO-MAX-SIST       PIC 9(1).                             
010100                                                                          
010200 01  WS-SUNTO-MIN-RED               PIC Z(10)9.9(2).                      
010300 01  WS-SUNTO-MIN-NUM               PIC S9(11)V9(3) COMP-3.               
010400 01  WS-SUNTO-MIN-DEC               PIC 9(11)V9(3)  VALUE ZERO.           
010500 01  WS-SUNTO-MIN-HELTAL            PIC 9(11).                            
010600                                                                          
010700 01  WS-SUNTO-MIN-JUST2             PIC 9(11)V9(3)  VALUE ZERO.           
010800 01  WS-SUNTO-MIN-JUST              PIC 9(11)V9(3)  VALUE ZERO.           
010900 01  FILLER REDEFINES WS-SUNTO-MIN-JUST.                                  
011000     03  FILLER                     PIC 9(11)V9(2).                       
011100     03  WS-SUNTO-MIN-SIST          PIC 9(1).                             
011200                                                                          
011300 01  WS-SUNTO-MAX-RED               PIC Z(10)9.9(2).                      
011400 01  WS-SUNTO-MAX-NUM               PIC S9(11)V9(3) COMP-3.               
011500 01  WS-SUNTO-MAX-DEC               PIC 9(11)V9(3)  VALUE ZERO.           
011600 01  WS-SUNTO-MAX-HELTAL            PIC 9(11).                            
011700                                                                          
011800 01  WS-SUNTO-MAX-JUST2             PIC 9(11)V9(3)  VALUE ZERO.           
011900 01  WS-SUNTO-MAX-JUST              PIC 9(11)V9(3)  VALUE ZERO.           
012000 01  FILLER REDEFINES WS-SUNTO-MAX-JUST.                                  
012100     03  FILLER                     PIC 9(11)V9(2).                       
012200     03  WS-SUNTO-MAX-SIST          PIC 9(1).                             
012300                                                                          
012400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012500 01  GENERAL-SUBPROGRAMS.                                                 
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012710     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012800     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
012900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013000     SKIP3                                                                
013203                                                                          
013210*    --- PARAMETERS TO ABEND                                              
013300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
013700                                                                          
013800 01  MESSAGE-CODES.                                                       
013900     03  ERROR-CODES.                                                     
014000         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
014100         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
014200         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
014300         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
014400         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
014500         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
014600         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
014700         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
014710         05  ERR-TO-MANY-LINES       PIC X(3)    VALUE '028'.             
014800         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
014900         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
015000     03  INFO-CODES.                                                      
015100         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
015200         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
015300         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
015400         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015800 01  -COPY WZ01SUB                                                        
015900     EJECT                                                                
015910 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
015920 01  -COPY WZ01SEND                                                       
015930     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
016200 01  -COPY WZ20DATE                                                       
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'DECEDIT    '.         
016600 01  -COPY WDECAREA                                                       
016610     EJECT                                                                
016700*                                                                         
016800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017000 01  REQU-AREA.                                                           
017100*    03  -COPY WZ01REQU                                                   
017200*    03  -COPY WF0292I1                                                   
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017500     SKIP3                                                                
017600 01  RESP-AREA.                                                           
017700*    03  -COPY WZ01RESP                                                   
017800*    03  -COPY WF0292O1                                                   
017900     EJECT                                                                
017901                                                                          
017902*    --- UT-AREOR                                                         
017903 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
017904                                'OUTPUT-AREA     '.                       
017905 01  HDR-AREA.                                                            
017906*    03  -COPY WZ01REQU -PRE MAIL-                                        
017907*    03  -COPY WZ04HDR                                                    
017908     EJECT                                                                
017909                                                                          
017910 01  DOC-HEAD-AREA1.                                                      
017911*    03  -COPY WF029201                                                   
017912 01  DOC-HEAD-AREA2.                                                      
017914*    03  -COPY WF029202                                                   
017915     EJECT                                                                
017916                                                                          
017917 01  DOC-LINE-AREA1.                                                      
017918*    03  -COPY WF029203                                                   
017919 01  DOC-LINE-AREA2.                                                      
017920*    03  -COPY WF029204                                                   
017921     EJECT                                                                
017930                                                                          
018000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
018100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018200                                                                          
018300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
018400 01  DB2-WS.                                                              
018500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
018600         88  CURSOR-OK                       VALUE 000.                   
018700         88  LINES-FOUND                     VALUE 000.                   
018800         88  LINES-MISSING                   VALUE 100.                   
018900         88  NULL-VALUE                      VALUE 305.                   
019000         88  RESOURCE-WRONG                  VALUE 904.                   
019100     03  GOOD-SQLCODECODES.                                               
019200         05  GOOD-SQLCODE OCCURS 5                                        
019300             INDEXED BY SQLCODE-IX PIC 9(3).                              
019400                                                                          
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
019700*01  -COPY T01LSEL -PRE T01LSEL-                                          
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'T01PDEV-AREA'.        
020000*01  -COPY T01PDEV -PRE T01PDEV-                                          
020100     EJECT                                                                
020110 01  FILLER                      PIC X(16)   VALUE 'SELECT-AREA '.        
020120*01  -COPY T01PDEV -PRE SELECT-                                           
020130     EJECT                                                                
020140 01  FILLER                      PIC X(16)   VALUE 'T01SDEV-AREA'.        
020150*01  -COPY T01SDEV -PRE T01SDEV-                                          
020160     EJECT                                                                
020200     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
020300     EJECT                                                                
020400     EXEC SQL INCLUDE T01PDEV END-EXEC.                                   
020500     EJECT                                                                
020510     EXEC SQL INCLUDE T01SDEV END-EXEC.                                   
020520     EJECT                                                                
020600 LINKAGE SECTION.                                                         
020900 PROCEDURE DIVISION.                                                      
021000 MAIN SECTION.                                                            
021100                                                                          
021200     PERFORM S80-FETCH-REQUEST-ARGUMENT                                   
021300     IF SUB-KDRC = 0                                                      
021400       PERFORM A-INIT                                                     
021500       PERFORM B-CHECK-KEYS                                               
021600       IF KEYS-OK                                                         
021700         PERFORM F-READ-SHOW-INFO                                         
021800       END-IF                                                             
021900       IF KEYS-WRONG                                                      
022000         PERFORM S03-MOVE-MISSING-TO-RESPOND                              
022100       END-IF                                                             
022200       PERFORM S81-RETURN-RESPONSE                                        
022300     END-IF                                                               
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800                                                                          
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100     INITIALIZE GOOD-SQLCODECODES                                         
023200     MOVE ALL '+' TO RESP-AREA                                            
023300     MOVE SPACE TO RESP-IDMSG-ERROR                                       
023400     MOVE SPACE TO RESP-IDMSG-INFO                                        
023500     MOVE SPACE TO RESP-IDELMT-ERROR                                      
023600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
024100 B-CHECK-KEYS SECTION.                                                    
024200     MOVE YES TO KEYS-SW                                                  
024300     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
024400                                                                          
024500     IF  REQU-IDMSGVER NUMERIC                                            
024600       IF REQU-IDLEGSEL-KEY > SPACE                                       
024700       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
024800       AND REQU-KDBEHX-KEY = 'L'                                          
024900       AND ACT-CODE-VALID                                                 
025000         CONTINUE                                                         
025100       ELSE                                                               
025200         MOVE NOO TO KEYS-SW                                              
025300       END-IF                                                             
025400     ELSE                                                                 
025500       MOVE NOO TO KEYS-SW                                                
025600     END-IF                                                               
025700                                                                          
025800     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
025900       MOVE NOO TO KEYS-SW                                                
026000     END-IF                                                               
026100                                                                          
026200     IF KEYS-WRONG                                                        
026300       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
026400       IF REQU-IDMSGVER NUMERIC                                           
026500         CONTINUE                                                         
026600       ELSE                                                               
026700         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
026800         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
026900       END-IF                                                             
027000       IF ACT-CODE-VALID                                                  
027100         CONTINUE                                                         
027200       ELSE                                                               
027300         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
027400         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
027500       END-IF                                                             
027600       IF REQU-IDUSER = SPACE OR = ALL '+'                                
027700         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
027800         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
027900       END-IF                                                             
027910       IF REQU-KDBEHX-KEY NOT = 'L'                                       
027920         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
027930         MOVE 'KDBEHX'     TO RESP-IDELMT-ERROR                           
027940       END-IF                                                             
028000     END-IF                                                               
028100                                                                          
028200     IF KEYS-OK                                                           
028300       PERFORM DB2-SELECT-T01LSEL                                         
028400       IF LINES-FOUND                                                     
028500         PERFORM DB2-SELECT-T01PDEV-SELECT                                
028600         IF LINES-FOUND                                                   
028700           CONTINUE                                                       
028800         ELSE                                                             
028900           MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                     
029000           MOVE 'KDBEHX'          TO RESP-IDELMT-ERROR                    
029100           MOVE NOO TO KEYS-SW                                            
029200         END-IF                                                           
029300       ELSE                                                               
029400         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
029500         MOVE 'IDLEGSEL'        TO RESP-IDELMT-ERROR                      
029600         MOVE NOO TO KEYS-SW                                              
029700       END-IF                                                             
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100                                                                          
030200*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
030300 F-READ-SHOW-INFO SECTION.                                                
030400     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
030500     MOVE REQU-KDBEHX-KEY    TO RESP-KDBEHX-KEY                           
030600     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
030700                                                                          
030800     PERFORM FA-READ-BASICDATA                                            
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
031300 FA-READ-BASICDATA SECTION.                                               
031400     IF ACT-CODE-SEARCH                                                   
031500       PERFORM FAA-SEARCH-T01PDEV                                         
031600     ELSE                                                                 
031700       IF ACT-CODE-UPDATE                                                 
031800         PERFORM FAB-UPDATE-T01PDEV                                       
031900       ELSE                                                               
032000         IF ACT-CODE-PRINT                                                
032100           PERFORM FAC-PRINT-T01PDEV                                      
032200         END-IF                                                           
032300       END-IF                                                             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700                                                                          
032800*** - SEARCH FOR RIGHT ABNORMAL SELECT AND MARK CURRENT LINE              
032900 FAA-SEARCH-T01PDEV SECTION.                                              
032910     MOVE ZERO TO RESP-KVRADER                                            
032920     MOVE ZERO TO RESP-KVRADER-TOT                                        
033000     PERFORM DB2-SELECT-T01PDEV-LIST                                      
033100                                                                          
033200     IF LINES-FOUND                                                       
033300       PERFORM S01-MOVE-TO-RESPOND                                        
033400     ELSE                                                                 
033410       PERFORM DB2-SELECT-T01PDEV-SELECT                                  
033420       IF LINES-FOUND                                                     
033430         PERFORM S02-MOVE-TO-RESPOND                                      
033440       ELSE                                                               
033500         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
033600         MOVE 'KDBEHX'   TO RESP-IDELMT-ERROR                             
033700         MOVE NOO        TO KEYS-SW                                       
033800       END-IF                                                             
033810     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200*** - CHECK IF UPDATE IS ON CURRENT LINE                                  
034300 FAB-UPDATE-T01PDEV SECTION.                                              
034400     MOVE ZERO TO RESP-KVRADER                                            
034401     MOVE ZERO TO RESP-KVRADER-TOT                                        
034410     PERFORM DB2-SELECT-T01PDEV-LIST                                      
034420                                                                          
034430     IF LINES-FOUND                                                       
034440       PERFORM FABA-CHECK-UPDATE-DATA                                     
034450       IF RESP-IDMSG-ERROR = SPACE                                        
034460         PERFORM DB2-UPDATE-T01PDEV                                       
034470         PERFORM DB2-SELECT-T01PDEV-LIST                                  
034490         PERFORM S08-MOVE-LINE-TO-RESPOND                                 
034495       END-IF                                                             
034496     ELSE                                                                 
034497       PERFORM FABB-CHECK-INSERT-DATA                                     
034498       IF RESP-IDMSG-ERROR = SPACE                                        
034499         PERFORM DB2-INSERT-T01PDEV                                       
034502         PERFORM DB2-SELECT-T01PDEV-LIST                                  
034504         PERFORM S08-MOVE-LINE-TO-RESPOND                                 
034509       END-IF                                                             
034510     END-IF                                                               
034520     .                                                                    
034600     EJECT                                                                
034700                                                                          
034800*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON SENDING COUNTRY             
034900 FABA-CHECK-UPDATE-DATA SECTION.                                          
035000     IF RESP-IDMSG-ERROR = SPACE                                          
035100       IF  REQU-FLPAYTE = 'N'                                             
035200       OR  REQU-FLPAYTE = 'Y'                                             
035300          IF REQU-FLPAYTE = 'Y'                                           
035400            MOVE 'J' TO REQU-FLPAYTE                                      
035500          END-IF                                                          
035600       ELSE                                                               
035700         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
035800         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
035900       END-IF                                                             
036000     END-IF                                                               
036100                                                                          
036200     IF RESP-IDMSG-ERROR = SPACE                                          
036300       IF  REQU-FLDELTE = 'N'                                             
036400       OR  REQU-FLDELTE = 'Y'                                             
036500          IF REQU-FLDELTE = 'Y'                                           
036600            MOVE 'J' TO REQU-FLDELTE                                      
036700          END-IF                                                          
036800       ELSE                                                               
036900         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
037000         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
037100       END-IF                                                             
037200     END-IF                                                               
037300                                                                          
037400     IF RESP-IDMSG-ERROR = SPACE                                          
037500       IF  REQU-FLSOFT  = 'N'                                             
037600       OR  REQU-FLSOFT  = 'Y'                                             
037700          IF REQU-FLSOFT = 'Y'                                            
037800            MOVE 'J' TO REQU-FLSOFT                                       
037900          END-IF                                                          
038000       ELSE                                                               
038100         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
038200         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
038300       END-IF                                                             
038400     END-IF                                                               
038500                                                                          
038600     IF RESP-IDMSG-ERROR = SPACE                                          
038700       IF  REQU-FLFREE  = 'N'                                             
038800       OR  REQU-FLFREE  = 'Y'                                             
038900          IF REQU-FLFREE = 'Y'                                            
039000            MOVE 'J' TO REQU-FLFREE                                       
039100          END-IF                                                          
039200       ELSE                                                               
039300         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
039400         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
039500       END-IF                                                             
039600     END-IF                                                               
039700                                                                          
039800     IF RESP-IDMSG-ERROR = SPACE                                          
039900       IF  REQU-FLSERV  = 'N'                                             
040000       OR  REQU-FLSERV  = 'Y'                                             
040100          IF REQU-FLSERV = 'Y'                                            
040200            MOVE 'J' TO REQU-FLSERV                                       
040300          END-IF                                                          
040400       ELSE                                                               
040500         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
040600         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
040700       END-IF                                                             
040800     END-IF                                                               
040900                                                                          
041000     IF RESP-IDMSG-ERROR = SPACE                                          
041100       IF  REQU-FLINVOIC = 'N'                                            
041200       OR  REQU-FLINVOIC = 'Y'                                            
041300          IF REQU-FLINVOIC = 'Y'                                          
041400            MOVE 'J' TO REQU-FLINVOIC                                     
041500          END-IF                                                          
041600       ELSE                                                               
041700         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
041800         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
041900       END-IF                                                             
042000     END-IF                                                               
042100                                                                          
042200     PERFORM S06-CHECK-NUMERIC-VALUES                                     
042300                                                                          
042400     IF RESP-IDMSG-ERROR = SPACE                                          
042410       IF REQU-DAREGDAT NUMERIC                                           
042420         IF REQU-DAREGDAT <= WS-CURRENT-DATE                              
042430           MOVE REQU-DAREGDAT TO DATE-TIDATE                              
042440           MOVE 'YYYYMMDD'    TO DATE-KDDATFMT                            
042450           CALL WZ20DATE USING DATE-WZ20DATE                              
042460           IF DATE-KDRC > ZERO                                            
042470             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
042480             MOVE 'FROM DATE'       TO RESP-IDELMT-ERROR                  
042490             MOVE NOO TO KEYS-SW                                          
042491           ELSE                                                           
042492             CONTINUE                                                     
042493           END-IF                                                         
042494         ELSE                                                             
042495           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
042496           MOVE 'FROM DATE'       TO RESP-IDELMT-ERROR                    
042497           MOVE NOO TO KEYS-SW                                            
042498         END-IF                                                           
042499       ELSE                                                               
042500         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
042501         MOVE 'FROM DATE'         TO RESP-IDELMT-ERROR                    
042502         MOVE NOO TO KEYS-SW                                              
042503       END-IF                                                             
042504     END-IF                                                               
042505     IF RESP-IDMSG-ERROR = SPACE                                          
042506       IF REQU-DAUPPDAT NUMERIC                                           
042507         IF REQU-DAUPPDAT <= WS-CURRENT-DATE                              
042508           MOVE REQU-DAUPPDAT TO DATE-TIDATE                              
042509           MOVE 'YYYYMMDD'    TO DATE-KDDATFMT                            
042510           CALL WZ20DATE USING DATE-WZ20DATE                              
042511           IF DATE-KDRC > ZERO                                            
042512             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
042513             MOVE 'TO DATE'         TO RESP-IDELMT-ERROR                  
042514             MOVE NOO TO KEYS-SW                                          
042515           ELSE                                                           
042516             CONTINUE                                                     
042517           END-IF                                                         
042518         ELSE                                                             
042519           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
042520           MOVE 'TO DATE'         TO RESP-IDELMT-ERROR                    
042521           MOVE NOO TO KEYS-SW                                            
042522         END-IF                                                           
042523       ELSE                                                               
042524         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
042525         MOVE 'TO DATE'           TO RESP-IDELMT-ERROR                    
042526         MOVE NOO TO KEYS-SW                                              
042527       END-IF                                                             
042528     END-IF                                                               
042529     IF RESP-IDMSG-ERROR = SPACE                                          
042530       IF REQU-DAREGDAT > REQU-DAUPPDAT                                   
042532         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
042533         MOVE 'FROM DATE'       TO RESP-IDELMT-ERROR                      
042534         MOVE NOO TO KEYS-SW                                              
042535       ELSE                                                               
042536         MOVE REQU-DAREGDAT     TO WS-DAREGDAT                            
042537         MOVE REQU-DAUPPDAT     TO WS-DAUPPDAT                            
042600       END-IF                                                             
042610     END-IF                                                               
042720     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000*** - VALIDATE REQUESTED FIELDS FOR INSERT ON SENDING COUNTRY             
043100 FABB-CHECK-INSERT-DATA SECTION.                                          
043200     IF RESP-IDMSG-ERROR = SPACE                                          
043300       IF  REQU-FLPAYTE = 'N'                                             
043400       OR  REQU-FLPAYTE = 'Y'                                             
043500          IF REQU-FLPAYTE = 'Y'                                           
043600            MOVE 'J' TO REQU-FLPAYTE                                      
043700          END-IF                                                          
043800       ELSE                                                               
043900         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
044000         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
044100       END-IF                                                             
044200     END-IF                                                               
044300                                                                          
044400     IF RESP-IDMSG-ERROR = SPACE                                          
044500       IF  REQU-FLDELTE = 'N'                                             
044600       OR  REQU-FLDELTE = 'Y'                                             
044700          IF REQU-FLDELTE = 'Y'                                           
044800            MOVE 'J' TO REQU-FLDELTE                                      
044900          END-IF                                                          
045000       ELSE                                                               
045010         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045020         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
045030       END-IF                                                             
045040     END-IF                                                               
045050                                                                          
045060     IF RESP-IDMSG-ERROR = SPACE                                          
045070       IF  REQU-FLSOFT  = 'N'                                             
045080       OR  REQU-FLSOFT  = 'Y'                                             
045090          IF REQU-FLSOFT = 'Y'                                            
045091            MOVE 'J' TO REQU-FLSOFT                                       
045092          END-IF                                                          
045093       ELSE                                                               
045094         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045095         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
045096       END-IF                                                             
045097     END-IF                                                               
045098                                                                          
045099     IF RESP-IDMSG-ERROR = SPACE                                          
045100       IF  REQU-FLFREE  = 'N'                                             
045110       OR  REQU-FLFREE  = 'Y'                                             
045120          IF REQU-FLFREE = 'Y'                                            
045130            MOVE 'J' TO REQU-FLFREE                                       
045140          END-IF                                                          
045150       ELSE                                                               
045160         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045170         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
045180       END-IF                                                             
045190     END-IF                                                               
045191                                                                          
045192     IF RESP-IDMSG-ERROR = SPACE                                          
045193       IF  REQU-FLSERV  = 'N'                                             
045194       OR  REQU-FLSERV  = 'Y'                                             
045195          IF REQU-FLSERV = 'Y'                                            
045196            MOVE 'J' TO REQU-FLSERV                                       
045197          END-IF                                                          
045198       ELSE                                                               
045199         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045200         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
045201       END-IF                                                             
045202     END-IF                                                               
045203                                                                          
045204     IF RESP-IDMSG-ERROR = SPACE                                          
045205       IF  REQU-FLINVOIC = 'N'                                            
045206       OR  REQU-FLINVOIC = 'Y'                                            
045207          IF REQU-FLINVOIC = 'Y'                                          
045208            MOVE 'J' TO REQU-FLINVOIC                                     
045209          END-IF                                                          
045210       ELSE                                                               
045211         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045212         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
045213       END-IF                                                             
045214     END-IF                                                               
045215                                                                          
045216     PERFORM S06-CHECK-NUMERIC-VALUES                                     
045217                                                                          
045218     IF RESP-IDMSG-ERROR = SPACE                                          
045219       MOVE REQU-DAREGDAT         TO T01PDEV-DAREGDAT                     
045220       MOVE REQU-DAUPPDAT         TO T01PDEV-DAUPPDAT                     
045221     END-IF                                                               
045222     .                                                                    
045223     EJECT                                                                
045224                                                                          
045230*** - START PRINTNIG TO D&P                                               
045300 FAC-PRINT-T01PDEV SECTION.                                               
045482     PERFORM DB2-SELECT-T01PDEV-LIST                                      
045483                                                                          
045484     IF LINES-FOUND                                                       
045485       PERFORM FABA-CHECK-UPDATE-DATA                                     
045486       IF RESP-IDMSG-ERROR = SPACE                                        
045488         PERFORM S09-MOVE-TO-RESPOND                                      
045490         PERFORM S10-MOVE-LINE-TO-RESPOND                                 
045495       END-IF                                                             
045496     ELSE                                                                 
045497       MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                            
045498       MOVE 'KDBEHX'       TO RESP-IDELMT-ERROR                           
045499       MOVE NOO TO KEYS-SW                                                
045500     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700                                                                          
058200*    --- MOVE TO OUTPUT SECTIONS                                          
058300 S01-MOVE-TO-RESPOND SECTION.                                             
058400     IF ACT-CODE-SEARCH                                                   
058500       IF T01PDEV-FLPAYTE = 'J'                                           
058600         MOVE 'Y'                 TO RESP-FLPAYTE                         
058700       ELSE                                                               
058800         MOVE T01PDEV-FLPAYTE     TO RESP-FLPAYTE                         
058900       END-IF                                                             
059000       IF T01PDEV-FLDELTE = 'J'                                           
059100         MOVE 'Y'                 TO RESP-FLDELTE                         
059200       ELSE                                                               
059300         MOVE T01PDEV-FLDELTE     TO RESP-FLDELTE                         
059400       END-IF                                                             
059500       MOVE T01PDEV-REARTRAB      TO RESP-REARTRAB                        
059600       MOVE T01PDEV-PRARTNTO-MIN  TO RESP-PRARTNTO-MIN                    
059700       MOVE T01PDEV-PRARTNTO-MAX  TO RESP-PRARTNTO-MAX                    
059800       MOVE T01PDEV-SUNTO-MIN     TO RESP-SUNTO-MIN                       
059900       MOVE T01PDEV-SUNTO-MAX     TO RESP-SUNTO-MAX                       
060000       IF T01PDEV-FLSOFT  = 'J'                                           
060100         MOVE 'Y'                 TO RESP-FLSOFT                          
060200       ELSE                                                               
060300         MOVE T01PDEV-FLSOFT      TO RESP-FLSOFT                          
060400       END-IF                                                             
060500       IF T01PDEV-FLFREE  = 'J'                                           
060600         MOVE 'Y'                 TO RESP-FLFREE                          
060700       ELSE                                                               
060800         MOVE T01PDEV-FLFREE      TO RESP-FLFREE                          
060900       END-IF                                                             
061000       IF T01PDEV-FLSERV  = 'J'                                           
061100         MOVE 'Y'                 TO RESP-FLSERV                          
061200       ELSE                                                               
061300         MOVE T01PDEV-FLSERV      TO RESP-FLSERV                          
061400       END-IF                                                             
061500       IF T01PDEV-FLINVOIC = 'J'                                          
061600         MOVE 'Y'                 TO RESP-FLINVOIC                        
061700       ELSE                                                               
061800         MOVE T01PDEV-FLINVOIC    TO RESP-FLINVOIC                        
061900       END-IF                                                             
062000       MOVE T01PDEV-DAREGDAT      TO RESP-DAREGDAT                        
062100       MOVE T01PDEV-DAUPPDAT      TO RESP-DAUPPDAT                        
062200       MOVE T01PDEV-IDUSER        TO RESP-IDUSER                          
062300     ELSE                                                                 
062400       IF ACT-CODE-UPDATE                                                 
062500         IF REQU-FLPAYTE = 'J'                                            
062600           MOVE 'Y'               TO RESP-FLPAYTE                         
062700         ELSE                                                             
062800           MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                         
062900         END-IF                                                           
063000         IF REQU-FLDELTE = 'J'                                            
063100           MOVE 'Y'               TO RESP-FLDELTE                         
063200         ELSE                                                             
063300           MOVE REQU-FLDELTE      TO RESP-FLDELTE                         
063400         END-IF                                                           
063500         MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                        
063600         MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                    
063700         MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                    
063800         MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                       
063900         MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                       
064000         IF REQU-FLSOFT  = 'J'                                            
064100           MOVE 'Y'               TO RESP-FLSOFT                          
064200         ELSE                                                             
064300           MOVE REQU-FLSOFT       TO RESP-FLSOFT                          
064400         END-IF                                                           
064500         IF REQU-FLFREE  = 'J'                                            
064600           MOVE 'Y'               TO RESP-FLFREE                          
064700         ELSE                                                             
064800           MOVE REQU-FLFREE       TO RESP-FLFREE                          
064900         END-IF                                                           
065000         IF REQU-FLSERV  = 'J'                                            
065100           MOVE 'Y'               TO RESP-FLSERV                          
065200         ELSE                                                             
065300           MOVE REQU-FLSERV       TO RESP-FLSERV                          
065400         END-IF                                                           
065500         IF REQU-FLINVOIC = 'J'                                           
065600           MOVE 'Y'               TO RESP-FLINVOIC                        
065700         ELSE                                                             
065800           MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                        
065900         END-IF                                                           
066000         MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                        
066100         MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                        
066200         MOVE REQU-IDUSER         TO RESP-IDUSER                          
066300       ELSE                                                               
066400         IF ACT-CODE-PRINT                                                
066500           IF REQU-FLPAYTE  = 'J'                                         
066600             MOVE 'Y'               TO RESP-FLPAYTE                       
066700           ELSE                                                           
066800             MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                       
066900           END-IF                                                         
067000           IF REQU-FLDELTE  = 'J'                                         
067100             MOVE 'Y'               TO RESP-FLDELTE                       
067200           ELSE                                                           
067300             MOVE REQU-FLDELTE      TO RESP-FLDELTE                       
067400           END-IF                                                         
067500           MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                      
067600           MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                  
067700           MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                  
067800           MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                     
067900           MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                     
068000           IF REQU-FLSOFT   = 'J'                                         
068100             MOVE 'Y'               TO RESP-FLSOFT                        
068200           ELSE                                                           
068300             MOVE REQU-FLSOFT       TO RESP-FLSOFT                        
068400           END-IF                                                         
068500           IF REQU-FLFREE   = 'J'                                         
068600             MOVE 'Y'               TO RESP-FLFREE                        
068700           ELSE                                                           
068800             MOVE REQU-FLFREE       TO RESP-FLFREE                        
068900           END-IF                                                         
069000           IF REQU-FLSERV   = 'J'                                         
069100             MOVE 'Y'               TO RESP-FLSERV                        
069200           ELSE                                                           
069300             MOVE REQU-FLSERV       TO RESP-FLSERV                        
069400           END-IF                                                         
069500           IF REQU-FLINVOIC = 'J'                                         
069600             MOVE 'Y'               TO RESP-FLINVOIC                      
069700           ELSE                                                           
069800             MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                      
069900           END-IF                                                         
070000           MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                      
070100           MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                      
070200           MOVE REQU-IDUSER         TO RESP-IDUSER                        
070300         END-IF                                                           
070400       END-IF                                                             
070500     END-IF                                                               
070600     .                                                                    
070700                                                                          
070710 S02-MOVE-TO-RESPOND SECTION.                                             
070720     IF ACT-CODE-SEARCH                                                   
070730       IF SELECT-FLPAYTE = 'J'                                            
070740         MOVE 'Y'                 TO RESP-FLPAYTE                         
070750       ELSE                                                               
070760         MOVE SELECT-FLPAYTE      TO RESP-FLPAYTE                         
070770       END-IF                                                             
070780       IF SELECT-FLDELTE = 'J'                                            
070790         MOVE 'Y'                 TO RESP-FLDELTE                         
070791       ELSE                                                               
070792         MOVE SELECT-FLDELTE      TO RESP-FLDELTE                         
070793       END-IF                                                             
070794       MOVE SELECT-REARTRAB       TO RESP-REARTRAB                        
070795       MOVE SELECT-PRARTNTO-MIN   TO RESP-PRARTNTO-MIN                    
070796       MOVE SELECT-PRARTNTO-MAX   TO RESP-PRARTNTO-MAX                    
070797       MOVE SELECT-SUNTO-MIN      TO RESP-SUNTO-MIN                       
070798       MOVE SELECT-SUNTO-MAX      TO RESP-SUNTO-MAX                       
070799       IF SELECT-FLSOFT  = 'J'                                            
070800         MOVE 'Y'                 TO RESP-FLSOFT                          
070801       ELSE                                                               
070802         MOVE SELECT-FLSOFT       TO RESP-FLSOFT                          
070803       END-IF                                                             
070804       IF SELECT-FLFREE  = 'J'                                            
070805         MOVE 'Y'                 TO RESP-FLFREE                          
070806       ELSE                                                               
070807         MOVE SELECT-FLFREE       TO RESP-FLFREE                          
070808       END-IF                                                             
070809       IF SELECT-FLSERV  = 'J'                                            
070810         MOVE 'Y'                 TO RESP-FLSERV                          
070811       ELSE                                                               
070812         MOVE SELECT-FLSERV       TO RESP-FLSERV                          
070813       END-IF                                                             
070814       IF SELECT-FLINVOIC = 'J'                                           
070815         MOVE 'Y'                 TO RESP-FLINVOIC                        
070816       ELSE                                                               
070817         MOVE SELECT-FLINVOIC     TO RESP-FLINVOIC                        
070818       END-IF                                                             
070819       MOVE WS-CURRENT-DATE       TO RESP-DAREGDAT                        
070820       MOVE WS-CURRENT-DATE       TO RESP-DAUPPDAT                        
070821       MOVE SELECT-IDUSER         TO RESP-IDUSER                          
070904     END-IF                                                               
070905     .                                                                    
070906                                                                          
070907 S03-MOVE-MISSING-TO-RESPOND SECTION.                                     
070910     MOVE SPACE             TO RESP-FLPAYTE                               
071000                               RESP-FLDELTE                               
071100                               RESP-FLSOFT                                
071200                               RESP-FLFREE                                
071300                               RESP-FLSERV                                
071400                               RESP-FLINVOIC                              
071500                               RESP-IDUSER                                
071600     MOVE ZERO              TO RESP-DAREGDAT                              
071700                               RESP-DAUPPDAT                              
071800                               RESP-REARTRAB                              
071900                               RESP-PRARTNTO-MIN                          
072000                               RESP-PRARTNTO-MAX                          
072100                               RESP-SUNTO-MIN                             
072200                               RESP-SUNTO-MAX                             
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600 S06-CHECK-NUMERIC-VALUES    SECTION.                                     
072700     IF RESP-IDMSG-ERROR = SPACE                                          
072800       IF REQU-REARTRAB(1:1) = '.'                                        
072900         IF REQU-REARTRAB(4:1) NUMERIC                                    
073000           MOVE REQU-REARTRAB(4:1) TO WS-REARTRAB-SIST                    
073100           MOVE SPACE              TO REQU-REARTRAB(4:1)                  
073200         END-IF                                                           
073300       END-IF                                                             
073400       IF REQU-REARTRAB(2:1) = '.'                                        
073500         IF REQU-REARTRAB(5:1) NUMERIC                                    
073600           MOVE REQU-REARTRAB(5:1) TO WS-REARTRAB-SIST                    
073700           MOVE SPACE              TO REQU-REARTRAB(5:1)                  
073800         END-IF                                                           
073900       END-IF                                                             
074000       MOVE REQU-REARTRAB          TO DEC-IDFRIDATA                       
074100       MOVE 2                      TO DEC-KVHELTAL                        
074200       MOVE 2                      TO DEC-KVDECIMAL                       
074300                                                                          
074400       CALL WDECEDIT USING DEC-WDECAREA                                   
074500                                                                          
074600       IF DEC-KDSVAR-OK                                                   
074700         MOVE DEC-IDEDITDATA       TO WS-REARTRAB-DEC                     
074800         IF WS-REARTRAB-DEC NUMERIC                                       
074900           MOVE WS-REARTRAB-DEC    TO WS-REARTRAB-HELTAL                  
075000           IF WS-REARTRAB-HELTAL > 99                                     
075100             MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
075200             MOVE 'REARTRAB'       TO RESP-IDELMT-ERROR                   
075300           ELSE                                                           
075400             MOVE WS-REARTRAB-DEC  TO WS-REARTRAB-NUM                     
075500             IF REQU-REARTRAB(1:1) = '.'                                  
075600               IF WS-REARTRAB-SIST > ZERO                                 
075700                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
075800                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
075900                                           WS-REARTRAB-JUST2              
076000               END-IF                                                     
076100             END-IF                                                       
076200             IF REQU-REARTRAB(2:1) = '.'                                  
076300               IF WS-REARTRAB-SIST > ZERO                                 
076400                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
076500                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
076600                                           WS-REARTRAB-JUST2              
076700               END-IF                                                     
076800             END-IF                                                       
076900             IF REQU-REARTRAB(3:1) = '.'                                  
077000               IF WS-REARTRAB-SIST > ZERO                                 
077100                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
077200                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
077300                                           WS-REARTRAB-JUST2              
077400               END-IF                                                     
077500             END-IF                                                       
077600             IF REQU-REARTRAB(4:1) = '.'                                  
077700               IF WS-REARTRAB-SIST > ZERO                                 
077800                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
077900                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
078000                                           WS-REARTRAB-JUST2              
078100               END-IF                                                     
078200             END-IF                                                       
078300           END-IF                                                         
078400         ELSE                                                             
078500           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
078600           MOVE 'REARTRAB'          TO RESP-IDELMT-ERROR                  
078700         END-IF                                                           
078800       ELSE                                                               
078900         MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                   
079000         MOVE 'REARTRAB'            TO RESP-IDELMT-ERROR                  
079100       END-IF                                                             
079200     END-IF                                                               
079300                                                                          
079400     IF RESP-IDMSG-ERROR = SPACE                                          
079500       IF REQU-PRARTNTO-MIN(1:1) = '.'                                    
079600         IF REQU-PRARTNTO-MIN(4:1) NUMERIC                                
079700           MOVE REQU-PRARTNTO-MIN(4:1) TO WS-PRARTNTO-MIN-SIST            
079800           MOVE SPACE                  TO REQU-PRARTNTO-MIN(4:1)          
079900         END-IF                                                           
080000       END-IF                                                             
080100       IF REQU-PRARTNTO-MIN(2:1) = '.'                                    
080200         IF REQU-PRARTNTO-MIN(5:1) NUMERIC                                
080300           MOVE REQU-PRARTNTO-MIN(5:1) TO WS-PRARTNTO-MIN-SIST            
080400           MOVE SPACE                  TO REQU-PRARTNTO-MIN(5:1)          
080500         END-IF                                                           
080600       END-IF                                                             
080700       IF REQU-PRARTNTO-MIN(3:1) = '.'                                    
080800         IF REQU-PRARTNTO-MIN(6:1) NUMERIC                                
080900           MOVE REQU-PRARTNTO-MIN(6:1) TO WS-PRARTNTO-MIN-SIST            
081000           MOVE SPACE                  TO REQU-PRARTNTO-MIN(6:1)          
081100         END-IF                                                           
081200       END-IF                                                             
081300       IF REQU-PRARTNTO-MIN(4:1) = '.'                                    
081400         IF REQU-PRARTNTO-MIN(7:1) NUMERIC                                
081500           MOVE REQU-PRARTNTO-MIN(7:1) TO WS-PRARTNTO-MIN-SIST            
081600           MOVE SPACE                  TO REQU-PRARTNTO-MIN(7:1)          
081700         END-IF                                                           
081800       END-IF                                                             
081900       IF REQU-PRARTNTO-MIN(5:1) = '.'                                    
082000         IF REQU-PRARTNTO-MIN(8:1) NUMERIC                                
082100           MOVE REQU-PRARTNTO-MIN(8:1) TO WS-PRARTNTO-MIN-SIST            
082200           MOVE SPACE                  TO REQU-PRARTNTO-MIN(8:1)          
082300         END-IF                                                           
082400       END-IF                                                             
082500       IF REQU-PRARTNTO-MIN(6:1) = '.'                                    
082600         IF REQU-PRARTNTO-MIN(9:1) NUMERIC                                
082700           MOVE REQU-PRARTNTO-MIN(9:1) TO WS-PRARTNTO-MIN-SIST            
082800           MOVE SPACE                  TO REQU-PRARTNTO-MIN(9:1)          
082900         END-IF                                                           
083000       END-IF                                                             
083100       IF REQU-PRARTNTO-MIN(7:1) = '.'                                    
083200         IF REQU-PRARTNTO-MIN(10:1) NUMERIC                               
083300           MOVE REQU-PRARTNTO-MIN(10:1) TO WS-PRARTNTO-MIN-SIST           
083400           MOVE SPACE                   TO REQU-PRARTNTO-MIN(10:1)        
083500         END-IF                                                           
083600       END-IF                                                             
083700       MOVE REQU-PRARTNTO-MIN           TO DEC-IDFRIDATA                  
083800       MOVE 7                           TO DEC-KVHELTAL                   
083900       MOVE 2                           TO DEC-KVDECIMAL                  
084000                                                                          
084100       CALL WDECEDIT USING DEC-WDECAREA                                   
084200                                                                          
084300       IF DEC-KDSVAR-OK                                                   
084400         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MIN-DEC            
084500         IF WS-PRARTNTO-MIN-DEC NUMERIC                                   
084600           MOVE WS-PRARTNTO-MIN-DEC     TO WS-PRARTNTO-MIN-HELTAL         
084700           IF WS-PRARTNTO-MIN-HELTAL > 9999999                            
084800             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
084900             MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR              
085000           ELSE                                                           
085100             MOVE WS-PRARTNTO-MIN-DEC   TO WS-PRARTNTO-MIN-NUM            
085200             IF REQU-PRARTNTO-MIN(1:1) = '.'                              
085300              IF WS-PRARTNTO-MIN-SIST > ZERO                              
085400               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
085500               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
085600                                             WS-PRARTNTO-MIN-JUST2        
085700              END-IF                                                      
085800             END-IF                                                       
085900             IF REQU-PRARTNTO-MIN(2:1) = '.'                              
086000              IF WS-PRARTNTO-MIN-SIST > ZERO                              
086100               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
086200               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
086300                                             WS-PRARTNTO-MIN-JUST2        
086400              END-IF                                                      
086500             END-IF                                                       
086600             IF REQU-PRARTNTO-MIN(3:1) = '.'                              
086700              IF WS-PRARTNTO-MIN-SIST > ZERO                              
086800               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
086900               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
087000                                             WS-PRARTNTO-MIN-JUST2        
087100              END-IF                                                      
087200             END-IF                                                       
087300             IF REQU-PRARTNTO-MIN(4:1) = '.'                              
087400              IF WS-PRARTNTO-MIN-SIST > ZERO                              
087500               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
087600               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
087700                                             WS-PRARTNTO-MIN-JUST2        
087800              END-IF                                                      
087900             END-IF                                                       
088000             IF REQU-PRARTNTO-MIN(5:1) = '.'                              
088100              IF WS-PRARTNTO-MIN-SIST > ZERO                              
088200               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
088300               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
088400                                             WS-PRARTNTO-MIN-JUST2        
088500              END-IF                                                      
088600             END-IF                                                       
088700             IF REQU-PRARTNTO-MIN(6:1) = '.'                              
088800              IF WS-PRARTNTO-MIN-SIST > ZERO                              
088900               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
089000               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
089100                                             WS-PRARTNTO-MIN-JUST2        
089200              END-IF                                                      
089300             END-IF                                                       
089400             IF REQU-PRARTNTO-MIN(7:1) = '.'                              
089500              IF WS-PRARTNTO-MIN-SIST > ZERO                              
089600               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
089700               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
089800                                             WS-PRARTNTO-MIN-JUST2        
089900              END-IF                                                      
090000             END-IF                                                       
090100             IF REQU-PRARTNTO-MIN(8:1) = '.'                              
090200              IF WS-PRARTNTO-MIN-SIST > ZERO                              
090300               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
090400               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
090500                                             WS-PRARTNTO-MIN-JUST2        
090600              END-IF                                                      
090700             END-IF                                                       
090800           END-IF                                                         
090900         ELSE                                                             
091000           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
091100           MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR                
091200         END-IF                                                           
091300       ELSE                                                               
091400         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
091500         MOVE 'PRARTNTO-MIN'          TO RESP-IDELMT-ERROR                
091600       END-IF                                                             
091700     END-IF                                                               
091800                                                                          
091900     IF RESP-IDMSG-ERROR = SPACE                                          
092000       IF REQU-PRARTNTO-MAX(1:1) = '.'                                    
092100         IF REQU-PRARTNTO-MAX(4:1) NUMERIC                                
092200           MOVE REQU-PRARTNTO-MAX(4:1) TO WS-PRARTNTO-MAX-SIST            
092300           MOVE SPACE                  TO REQU-PRARTNTO-MAX(4:1)          
092400         END-IF                                                           
092500       END-IF                                                             
092600       IF REQU-PRARTNTO-MAX(2:1) = '.'                                    
092700         IF REQU-PRARTNTO-MAX(5:1) NUMERIC                                
092800           MOVE REQU-PRARTNTO-MAX(5:1) TO WS-PRARTNTO-MAX-SIST            
092900           MOVE SPACE                  TO REQU-PRARTNTO-MAX(5:1)          
093000         END-IF                                                           
093100       END-IF                                                             
093200       IF REQU-PRARTNTO-MAX(3:1) = '.'                                    
093300         IF REQU-PRARTNTO-MAX(6:1) NUMERIC                                
093400           MOVE REQU-PRARTNTO-MAX(6:1) TO WS-PRARTNTO-MAX-SIST            
093500           MOVE SPACE                  TO REQU-PRARTNTO-MAX(6:1)          
093600         END-IF                                                           
093700       END-IF                                                             
093800       IF REQU-PRARTNTO-MAX(4:1) = '.'                                    
093900         IF REQU-PRARTNTO-MAX(7:1) NUMERIC                                
094000           MOVE REQU-PRARTNTO-MAX(7:1) TO WS-PRARTNTO-MAX-SIST            
094100           MOVE SPACE                  TO REQU-PRARTNTO-MAX(7:1)          
094200         END-IF                                                           
094300       END-IF                                                             
094400       IF REQU-PRARTNTO-MAX(5:1) = '.'                                    
094500         IF REQU-PRARTNTO-MAX(8:1) NUMERIC                                
094600           MOVE REQU-PRARTNTO-MAX(8:1) TO WS-PRARTNTO-MAX-SIST            
094700           MOVE SPACE                  TO REQU-PRARTNTO-MAX(8:1)          
094800         END-IF                                                           
094900       END-IF                                                             
095000       IF REQU-PRARTNTO-MAX(6:1) = '.'                                    
095100         IF REQU-PRARTNTO-MAX(9:1) NUMERIC                                
095200           MOVE REQU-PRARTNTO-MAX(9:1) TO WS-PRARTNTO-MAX-SIST            
095300           MOVE SPACE                  TO REQU-PRARTNTO-MAX(9:1)          
095400         END-IF                                                           
095500       END-IF                                                             
095600       IF REQU-PRARTNTO-MAX(7:1) = '.'                                    
095700         IF REQU-PRARTNTO-MAX(10:1) NUMERIC                               
095800           MOVE REQU-PRARTNTO-MAX(10:1) TO WS-PRARTNTO-MAX-SIST           
095900           MOVE SPACE                   TO REQU-PRARTNTO-MAX(10:1)        
096000         END-IF                                                           
096100       END-IF                                                             
096200       MOVE REQU-PRARTNTO-MAX           TO DEC-IDFRIDATA                  
096300       MOVE 7                           TO DEC-KVHELTAL                   
096400       MOVE 2                           TO DEC-KVDECIMAL                  
096500                                                                          
096600       CALL WDECEDIT USING DEC-WDECAREA                                   
096700                                                                          
096800       IF DEC-KDSVAR-OK                                                   
096900         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MAX-DEC            
097000         IF WS-PRARTNTO-MAX-DEC NUMERIC                                   
097100           MOVE WS-PRARTNTO-MAX-DEC     TO WS-PRARTNTO-MAX-HELTAL         
097200           IF WS-PRARTNTO-MAX-HELTAL > 9999999                            
097300             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
097400             MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR              
097500           ELSE                                                           
097600             MOVE WS-PRARTNTO-MAX-DEC   TO WS-PRARTNTO-MAX-NUM            
097700             IF REQU-PRARTNTO-MAX(1:1) = '.'                              
097800              IF WS-PRARTNTO-MAX-SIST > ZERO                              
097900               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
098000               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
098100                                             WS-PRARTNTO-MAX-JUST2        
098200              END-IF                                                      
098300             END-IF                                                       
098400             IF REQU-PRARTNTO-MAX(2:1) = '.'                              
098500              IF WS-PRARTNTO-MAX-SIST > ZERO                              
098600               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
098700               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
098800                                             WS-PRARTNTO-MAX-JUST2        
098900              END-IF                                                      
099000             END-IF                                                       
099100             IF REQU-PRARTNTO-MAX(3:1) = '.'                              
099200              IF WS-PRARTNTO-MAX-SIST > ZERO                              
099300               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
099400               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
099500                                             WS-PRARTNTO-MAX-JUST2        
099600              END-IF                                                      
099700             END-IF                                                       
099800             IF REQU-PRARTNTO-MAX(4:1) = '.'                              
099900              IF WS-PRARTNTO-MAX-SIST > ZERO                              
100000               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
100100               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
100200                                             WS-PRARTNTO-MAX-JUST2        
100300              END-IF                                                      
100400             END-IF                                                       
100500             IF REQU-PRARTNTO-MAX(5:1) = '.'                              
100600              IF WS-PRARTNTO-MAX-SIST > ZERO                              
100700               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
100800               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
100900                                             WS-PRARTNTO-MAX-JUST2        
101000              END-IF                                                      
101100             END-IF                                                       
101200             IF REQU-PRARTNTO-MAX(6:1) = '.'                              
101300              IF WS-PRARTNTO-MAX-SIST > ZERO                              
101400               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
101500               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
101600                                             WS-PRARTNTO-MAX-JUST2        
101700              END-IF                                                      
101800             END-IF                                                       
101900             IF REQU-PRARTNTO-MAX(7:1) = '.'                              
102000              IF WS-PRARTNTO-MAX-SIST > ZERO                              
102100               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
102200               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
102300                                             WS-PRARTNTO-MAX-JUST2        
102400              END-IF                                                      
102500             END-IF                                                       
102600             IF REQU-PRARTNTO-MAX(8:1) = '.'                              
102700              IF WS-PRARTNTO-MAX-SIST > ZERO                              
102800               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
102900               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
103000                                             WS-PRARTNTO-MAX-JUST2        
103100              END-IF                                                      
103200             END-IF                                                       
103300           END-IF                                                         
103400         ELSE                                                             
103500           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
103600           MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR                
103700         END-IF                                                           
103800       ELSE                                                               
103900         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
104000         MOVE 'PRARTNTO-MAX'          TO RESP-IDELMT-ERROR                
104100       END-IF                                                             
104200     END-IF                                                               
104300                                                                          
104400     IF RESP-IDMSG-ERROR = SPACE                                          
104500       IF REQU-SUNTO-MIN(1:1) = '.'                                       
104600         IF REQU-SUNTO-MIN(4:1) NUMERIC                                   
104700           MOVE REQU-SUNTO-MIN(4:1)    TO WS-SUNTO-MIN-SIST               
104800           MOVE SPACE                  TO REQU-SUNTO-MIN(4:1)             
104900         END-IF                                                           
105000       END-IF                                                             
105100       IF REQU-SUNTO-MIN(2:1) = '.'                                       
105200         IF REQU-SUNTO-MIN(5:1) NUMERIC                                   
105300           MOVE REQU-SUNTO-MIN(5:1)    TO WS-SUNTO-MIN-SIST               
105400           MOVE SPACE                  TO REQU-SUNTO-MIN(5:1)             
105500         END-IF                                                           
105600       END-IF                                                             
105700       IF REQU-SUNTO-MIN(3:1) = '.'                                       
105800         IF REQU-SUNTO-MIN(6:1) NUMERIC                                   
105900           MOVE REQU-SUNTO-MIN(6:1)    TO WS-SUNTO-MIN-SIST               
106000           MOVE SPACE                  TO REQU-SUNTO-MIN(6:1)             
106100         END-IF                                                           
106200       END-IF                                                             
106300       IF REQU-SUNTO-MIN(4:1) = '.'                                       
106400         IF REQU-SUNTO-MIN(7:1) NUMERIC                                   
106500           MOVE REQU-SUNTO-MIN(7:1)    TO WS-SUNTO-MIN-SIST               
106600           MOVE SPACE                  TO REQU-SUNTO-MIN(7:1)             
106700         END-IF                                                           
106800       END-IF                                                             
106900       IF REQU-SUNTO-MIN(5:1) = '.'                                       
107000         IF REQU-SUNTO-MIN(8:1) NUMERIC                                   
107100           MOVE REQU-SUNTO-MIN(8:1)    TO WS-SUNTO-MIN-SIST               
107200           MOVE SPACE                  TO REQU-SUNTO-MIN(8:1)             
107300         END-IF                                                           
107400       END-IF                                                             
107500       IF REQU-SUNTO-MIN(6:1) = '.'                                       
107600         IF REQU-SUNTO-MIN(9:1) NUMERIC                                   
107700           MOVE REQU-SUNTO-MIN(9:1)    TO WS-SUNTO-MIN-SIST               
107800           MOVE SPACE                  TO REQU-SUNTO-MIN(9:1)             
107900         END-IF                                                           
108000       END-IF                                                             
108100       IF REQU-SUNTO-MIN(7:1) = '.'                                       
108200         IF REQU-SUNTO-MIN(10:1) NUMERIC                                  
108300           MOVE REQU-SUNTO-MIN(10:1)    TO WS-SUNTO-MIN-SIST              
108400           MOVE SPACE                   TO REQU-SUNTO-MIN(10:1)           
108500         END-IF                                                           
108600       END-IF                                                             
108700       IF REQU-SUNTO-MIN(8:1) = '.'                                       
108800         IF REQU-SUNTO-MIN(11:1) NUMERIC                                  
108900           MOVE REQU-SUNTO-MIN(11:1)    TO WS-SUNTO-MIN-SIST              
109000           MOVE SPACE                   TO REQU-SUNTO-MIN(11:1)           
109100         END-IF                                                           
109200       END-IF                                                             
109300       IF REQU-SUNTO-MIN(9:1) = '.'                                       
109400         IF REQU-SUNTO-MIN(12:1) NUMERIC                                  
109500           MOVE REQU-SUNTO-MIN(12:1)    TO WS-SUNTO-MIN-SIST              
109600           MOVE SPACE                   TO REQU-SUNTO-MIN(12:1)           
109700         END-IF                                                           
109800       END-IF                                                             
109900       IF REQU-SUNTO-MIN(10:1) = '.'                                      
110000         IF REQU-SUNTO-MIN(13:1) NUMERIC                                  
110100           MOVE REQU-SUNTO-MIN(13:1)    TO WS-SUNTO-MIN-SIST              
110200           MOVE SPACE                   TO REQU-SUNTO-MIN(13:1)           
110300         END-IF                                                           
110400       END-IF                                                             
110500       IF REQU-SUNTO-MIN(11:1) = '.'                                      
110600         IF REQU-SUNTO-MIN(14:1) NUMERIC                                  
110700           MOVE REQU-SUNTO-MIN(14:1)    TO WS-SUNTO-MIN-SIST              
110800           MOVE SPACE                   TO REQU-SUNTO-MIN(14:1)           
110900         END-IF                                                           
111000       END-IF                                                             
111100       MOVE REQU-SUNTO-MIN              TO DEC-IDFRIDATA                  
111200       MOVE 11                          TO DEC-KVHELTAL                   
111300       MOVE 2                           TO DEC-KVDECIMAL                  
111400                                                                          
111500       CALL WDECEDIT USING DEC-WDECAREA                                   
111600                                                                          
111700       IF DEC-KDSVAR-OK                                                   
111800         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MIN-DEC               
111900         IF WS-SUNTO-MIN-DEC NUMERIC                                      
112000           MOVE WS-SUNTO-MIN-DEC        TO WS-SUNTO-MIN-HELTAL            
112100           IF WS-SUNTO-MIN-HELTAL > 99999999999                           
112200             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
112300             MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR              
112400           ELSE                                                           
112500             MOVE WS-SUNTO-MIN-DEC      TO WS-SUNTO-MIN-NUM               
112600             IF REQU-SUNTO-MIN(1:1) = '.'                                 
112700              IF WS-SUNTO-MIN-SIST > ZERO                                 
112800               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
112900               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
113000                                           WS-SUNTO-MIN-JUST2             
113100              END-IF                                                      
113200             END-IF                                                       
113300             IF REQU-SUNTO-MIN(2:1) = '.'                                 
113400              IF WS-SUNTO-MIN-SIST > ZERO                                 
113500               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
113600               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
113700                                           WS-SUNTO-MIN-JUST2             
113800              END-IF                                                      
113900             END-IF                                                       
114000             IF REQU-SUNTO-MIN(3:1) = '.'                                 
114100              IF WS-SUNTO-MIN-SIST > ZERO                                 
114200               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
114300               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
114400                                           WS-SUNTO-MIN-JUST2             
114500              END-IF                                                      
114600             END-IF                                                       
114700             IF REQU-SUNTO-MIN(4:1) = '.'                                 
114800              IF WS-SUNTO-MIN-SIST > ZERO                                 
114900               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
115000               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
115100                                           WS-SUNTO-MIN-JUST2             
115200              END-IF                                                      
115300             END-IF                                                       
115400             IF REQU-SUNTO-MIN(5:1) = '.'                                 
115500              IF WS-SUNTO-MIN-SIST > ZERO                                 
115600               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
115700               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
115800                                           WS-SUNTO-MIN-JUST2             
115900              END-IF                                                      
116000             END-IF                                                       
116100             IF REQU-SUNTO-MIN(6:1) = '.'                                 
116200              IF WS-SUNTO-MIN-SIST > ZERO                                 
116300               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
116400               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
116500                                           WS-SUNTO-MIN-JUST2             
116600              END-IF                                                      
116700             END-IF                                                       
116800             IF REQU-SUNTO-MIN(7:1) = '.'                                 
116900              IF WS-SUNTO-MIN-SIST > ZERO                                 
117000               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
117100               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
117200                                           WS-SUNTO-MIN-JUST2             
117300              END-IF                                                      
117400             END-IF                                                       
117500             IF REQU-SUNTO-MIN(8:1) = '.'                                 
117600              IF WS-SUNTO-MIN-SIST > ZERO                                 
117700               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
117800               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
117900                                           WS-SUNTO-MIN-JUST2             
118000              END-IF                                                      
118100             END-IF                                                       
118200             IF REQU-SUNTO-MIN(9:1) = '.'                                 
118300              IF WS-SUNTO-MIN-SIST > ZERO                                 
118400               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
118500               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
118600                                           WS-SUNTO-MIN-JUST2             
118700              END-IF                                                      
118800             END-IF                                                       
118900             IF REQU-SUNTO-MIN(10:1) = '.'                                
119000              IF WS-SUNTO-MIN-SIST > ZERO                                 
119100               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
119200               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
119300                                           WS-SUNTO-MIN-JUST2             
119400              END-IF                                                      
119500             END-IF                                                       
119600             IF REQU-SUNTO-MIN(11:1) = '.'                                
119700              IF WS-SUNTO-MIN-SIST > ZERO                                 
119800               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
119900               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
120000                                           WS-SUNTO-MIN-JUST2             
120100              END-IF                                                      
120200             END-IF                                                       
120300             IF REQU-SUNTO-MIN(12:1) = '.'                                
120400              IF WS-SUNTO-MIN-SIST > ZERO                                 
120500               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
120600               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
120700                                           WS-SUNTO-MIN-JUST2             
120800              END-IF                                                      
120900             END-IF                                                       
121000           END-IF                                                         
121100         ELSE                                                             
121200           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
121300           MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR                
121400         END-IF                                                           
121500       ELSE                                                               
121600         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
121700         MOVE 'SUNTO-MIN'             TO RESP-IDELMT-ERROR                
121800       END-IF                                                             
121900     END-IF                                                               
122000                                                                          
122100     IF RESP-IDMSG-ERROR = SPACE                                          
122200       IF REQU-SUNTO-MAX(1:1) = '.'                                       
122300         IF REQU-SUNTO-MAX(4:1) NUMERIC                                   
122400           MOVE REQU-SUNTO-MAX(4:1)    TO WS-SUNTO-MAX-SIST               
122500           MOVE SPACE                  TO REQU-SUNTO-MAX(4:1)             
122600         END-IF                                                           
122700       END-IF                                                             
122800       IF REQU-SUNTO-MAX(2:1) = '.'                                       
122900         IF REQU-SUNTO-MAX(5:1) NUMERIC                                   
123000           MOVE REQU-SUNTO-MAX(5:1)    TO WS-SUNTO-MAX-SIST               
123100           MOVE SPACE                  TO REQU-SUNTO-MAX(5:1)             
123200         END-IF                                                           
123300       END-IF                                                             
123400       IF REQU-SUNTO-MAX(3:1) = '.'                                       
123500         IF REQU-SUNTO-MAX(6:1) NUMERIC                                   
123600           MOVE REQU-SUNTO-MAX(6:1)    TO WS-SUNTO-MAX-SIST               
123700           MOVE SPACE                  TO REQU-SUNTO-MAX(6:1)             
123800         END-IF                                                           
123900       END-IF                                                             
124000       IF REQU-SUNTO-MAX(4:1) = '.'                                       
124100         IF REQU-SUNTO-MAX(7:1) NUMERIC                                   
124200           MOVE REQU-SUNTO-MAX(7:1)    TO WS-SUNTO-MAX-SIST               
124300           MOVE SPACE                  TO REQU-SUNTO-MAX(7:1)             
124400         END-IF                                                           
124500       END-IF                                                             
124600       IF REQU-SUNTO-MAX(5:1) = '.'                                       
124700         IF REQU-SUNTO-MAX(8:1) NUMERIC                                   
124800           MOVE REQU-SUNTO-MAX(8:1)    TO WS-SUNTO-MAX-SIST               
124900           MOVE SPACE                  TO REQU-SUNTO-MAX(8:1)             
125000         END-IF                                                           
125100       END-IF                                                             
125200       IF REQU-SUNTO-MAX(6:1) = '.'                                       
125300         IF REQU-SUNTO-MAX(9:1) NUMERIC                                   
125400           MOVE REQU-SUNTO-MAX(9:1)    TO WS-SUNTO-MAX-SIST               
125500           MOVE SPACE                  TO REQU-SUNTO-MAX(9:1)             
125600         END-IF                                                           
125700       END-IF                                                             
125800       IF REQU-SUNTO-MAX(7:1) = '.'                                       
125900         IF REQU-SUNTO-MAX(10:1) NUMERIC                                  
126000           MOVE REQU-SUNTO-MAX(10:1)    TO WS-SUNTO-MAX-SIST              
126100           MOVE SPACE                   TO REQU-SUNTO-MAX(10:1)           
126200         END-IF                                                           
126300       END-IF                                                             
126400       IF REQU-SUNTO-MAX(8:1) = '.'                                       
126500         IF REQU-SUNTO-MAX(11:1) NUMERIC                                  
126600           MOVE REQU-SUNTO-MAX(11:1)    TO WS-SUNTO-MAX-SIST              
126700           MOVE SPACE                   TO REQU-SUNTO-MAX(11:1)           
126800         END-IF                                                           
126900       END-IF                                                             
127000       IF REQU-SUNTO-MAX(9:1) = '.'                                       
127100         IF REQU-SUNTO-MAX(12:1) NUMERIC                                  
127200           MOVE REQU-SUNTO-MAX(12:1)    TO WS-SUNTO-MAX-SIST              
127300           MOVE SPACE                   TO REQU-SUNTO-MAX(12:1)           
127400         END-IF                                                           
127500       END-IF                                                             
127600       IF REQU-SUNTO-MAX(10:1) = '.'                                      
127700         IF REQU-SUNTO-MAX(13:1) NUMERIC                                  
127800           MOVE REQU-SUNTO-MAX(13:1)    TO WS-SUNTO-MAX-SIST              
127900           MOVE SPACE                   TO REQU-SUNTO-MAX(13:1)           
128000         END-IF                                                           
128100       END-IF                                                             
128200       IF REQU-SUNTO-MAX(11:1) = '.'                                      
128300         IF REQU-SUNTO-MAX(14:1) NUMERIC                                  
128400           MOVE REQU-SUNTO-MAX(14:1)    TO WS-SUNTO-MAX-SIST              
128500           MOVE SPACE                   TO REQU-SUNTO-MAX(14:1)           
128600         END-IF                                                           
128700       END-IF                                                             
128800       MOVE REQU-SUNTO-MAX              TO DEC-IDFRIDATA                  
128900       MOVE 11                          TO DEC-KVHELTAL                   
129000       MOVE 2                           TO DEC-KVDECIMAL                  
129100                                                                          
129200       CALL WDECEDIT USING DEC-WDECAREA                                   
129300                                                                          
129400       IF DEC-KDSVAR-OK                                                   
129500         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MAX-DEC               
129600         IF WS-SUNTO-MAX-DEC NUMERIC                                      
129700           MOVE WS-SUNTO-MAX-DEC        TO WS-SUNTO-MAX-HELTAL            
129800           IF WS-SUNTO-MAX-HELTAL > 99999999999                           
129900             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
130000             MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR              
130100           ELSE                                                           
130200             MOVE WS-SUNTO-MAX-DEC      TO WS-SUNTO-MAX-NUM               
130300             IF REQU-SUNTO-MAX(1:1) = '.'                                 
130400              IF WS-SUNTO-MAX-SIST > ZERO                                 
130500               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
130600               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
130700                                           WS-SUNTO-MAX-JUST2             
130800              END-IF                                                      
130900             END-IF                                                       
131000             IF REQU-SUNTO-MAX(2:1) = '.'                                 
131100              IF WS-SUNTO-MAX-SIST > ZERO                                 
131200               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
131300               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
131400                                           WS-SUNTO-MAX-JUST2             
131500              END-IF                                                      
131600             END-IF                                                       
131700             IF REQU-SUNTO-MAX(3:1) = '.'                                 
131800              IF WS-SUNTO-MAX-SIST > ZERO                                 
131900               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
132000               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
132100                                           WS-SUNTO-MAX-JUST2             
132200              END-IF                                                      
132300             END-IF                                                       
132400             IF REQU-SUNTO-MAX(4:1) = '.'                                 
132500              IF WS-SUNTO-MAX-SIST > ZERO                                 
132600               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
132700               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
132800                                           WS-SUNTO-MAX-JUST2             
132900              END-IF                                                      
133000             END-IF                                                       
133100             IF REQU-SUNTO-MAX(5:1) = '.'                                 
133200              IF WS-SUNTO-MAX-SIST > ZERO                                 
133300               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
133400               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
133500                                           WS-SUNTO-MAX-JUST2             
133600              END-IF                                                      
133700             END-IF                                                       
133800             IF REQU-SUNTO-MAX(6:1) = '.'                                 
133900              IF WS-SUNTO-MAX-SIST > ZERO                                 
134000               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
134100               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
134200                                           WS-SUNTO-MAX-JUST2             
134300              END-IF                                                      
134400             END-IF                                                       
134500             IF REQU-SUNTO-MAX(7:1) = '.'                                 
134600              IF WS-SUNTO-MAX-SIST > ZERO                                 
134700               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
134800               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
134900                                           WS-SUNTO-MAX-JUST2             
135000              END-IF                                                      
135100             END-IF                                                       
135200             IF REQU-SUNTO-MAX(8:1) = '.'                                 
135300              IF WS-SUNTO-MAX-SIST > ZERO                                 
135400               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
135500               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
135600                                           WS-SUNTO-MAX-JUST2             
135700              END-IF                                                      
135800             END-IF                                                       
135900             IF REQU-SUNTO-MAX(9:1) = '.'                                 
136000              IF WS-SUNTO-MAX-SIST > ZERO                                 
136100               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
136200               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
136300                                           WS-SUNTO-MAX-JUST2             
136400              END-IF                                                      
136500             END-IF                                                       
136600             IF REQU-SUNTO-MAX(10:1) = '.'                                
136700              IF WS-SUNTO-MAX-SIST > ZERO                                 
136800               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
136900               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
137000                                           WS-SUNTO-MAX-JUST2             
137100              END-IF                                                      
137200             END-IF                                                       
137300             IF REQU-SUNTO-MAX(11:1) = '.'                                
137400              IF WS-SUNTO-MAX-SIST > ZERO                                 
137500               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
137600               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
137700                                           WS-SUNTO-MAX-JUST2             
137800              END-IF                                                      
137900             END-IF                                                       
138000             IF REQU-SUNTO-MAX(12:1) = '.'                                
138100              IF WS-SUNTO-MAX-SIST > ZERO                                 
138200               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
138300               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
138400                                           WS-SUNTO-MAX-JUST2             
138500              END-IF                                                      
138600             END-IF                                                       
138700           END-IF                                                         
138800         ELSE                                                             
138900           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
139000           MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR                
139100         END-IF                                                           
139200       ELSE                                                               
139300         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
139400         MOVE 'SUNTO-MAX'             TO RESP-IDELMT-ERROR                
139500       END-IF                                                             
139600     END-IF                                                               
139700     .                                                                    
139800                                                                          
139835 S07-MOVE-TO-LINES        SECTION.                                        
139837     MOVE T01SDEV-DAREGDAT            TO RESP-DAREGDAT-LINE(IX)           
139838     MOVE T01SDEV-BETEXT              TO RESP-BETEXT-LINE(IX)             
139839     MOVE T01SDEV-IDFINDOC            TO RESP-IDFINDOC-LINE(IX)           
139840     MOVE T01SDEV-IDARTNR-FINANCE(1:9) TO RESP-IDARTNR-LINE(IX)           
139841     MOVE T01SDEV-BEART               TO RESP-BEART-LINE(IX)              
139842     .                                                                    
139843                                                                          
139844 S08-MOVE-LINE-TO-RESPOND SECTION.                                        
139845     MOVE ZERO TO IX                                                      
139846     IF T01PDEV-FLPAYTE = 'J'                                             
139847       MOVE 'PAYMENT TERMS MISS'      TO WS-BETEXT-A                      
139848     ELSE                                                                 
139849       MOVE '?'                       TO WS-BETEXT-A                      
139850     END-IF                                                               
139851     IF T01PDEV-FLDELTE = 'J'                                             
139852       MOVE 'DELIVERY TERMS MISS'     TO WS-BETEXT-B                      
139853     ELSE                                                                 
139854       MOVE '?'                       TO WS-BETEXT-B                      
139855     END-IF                                                               
139856     IF T01PDEV-REARTRAB > ZERO                                           
139857       MOVE 'DISCOUNT IS HIGH'        TO WS-BETEXT-C                      
139858       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
139859     ELSE                                                                 
139860       MOVE '?'                       TO WS-BETEXT-C                      
139861       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
139862     END-IF                                                               
139863     IF T01PDEV-PRARTNTO-MIN > ZERO                                       
139864       MOVE 'NET PRICE IS LOW'        TO WS-BETEXT-D                      
139865       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
139866     ELSE                                                                 
139867       MOVE '?'                       TO WS-BETEXT-D                      
139868       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
139869     END-IF                                                               
139870     IF T01PDEV-PRARTNTO-MAX > ZERO                                       
139871       MOVE 'NET PRICE IS HIGH'       TO WS-BETEXT-E                      
139872       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
139873     ELSE                                                                 
139874       MOVE '?'                       TO WS-BETEXT-E                      
139875       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
139876     END-IF                                                               
139877     IF T01PDEV-SUNTO-MIN        > ZERO                                   
139878       MOVE 'NET VALUE IS LOW'        TO WS-BETEXT-F                      
139879       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
139880     ELSE                                                                 
139881       MOVE '?'                       TO WS-BETEXT-F                      
139882       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
139883     END-IF                                                               
139884     IF T01PDEV-SUNTO-MAX        > ZERO                                   
139885       MOVE 'NET VALUE IS HIGH'       TO WS-BETEXT-G                      
139886       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
139887     ELSE                                                                 
139888       MOVE '?'                       TO WS-BETEXT-G                      
139889       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
139890     END-IF                                                               
139891     IF T01PDEV-FLSOFT = 'J'                                              
139892       MOVE 'J'                       TO WS-FLSOFT                        
139893     ELSE                                                                 
139894       MOVE 'N'                       TO WS-FLSOFT                        
139895     END-IF                                                               
139896     IF T01PDEV-FLFREE = 'J'                                              
139897       MOVE 'J'                       TO WS-FLFREE                        
139898     ELSE                                                                 
139899       MOVE 'N'                       TO WS-FLFREE                        
139900     END-IF                                                               
139901     MOVE T01PDEV-DAREGDAT            TO WS-DAREGDAT                      
139902     MOVE T01PDEV-DAUPPDAT            TO WS-DAUPPDAT                      
139903     IF T01PDEV-FLSERV = 'J'                                              
139904       IF T01PDEV-FLINVOIC = 'J'                                          
139905         PERFORM DB2-COUNT-T01SDEV-4                                      
139909         IF LINES-MISSING                                                 
139910           MOVE ERR-LINES-NOT-FOUND   TO RESP-IDMSG-ERROR                 
139911           MOVE ZERO                  TO WS-COUNTER-T01SDEV               
139914         END-IF                                                           
139915         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
139916         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
139917         IF WS-COUNTER-T01SDEV > 500                                      
139918           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
139919           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
139920           MOVE 500                   TO RESP-KVRADER                     
139921         END-IF                                                           
139922         PERFORM S01-MOVE-TO-RESPOND                                      
139923         PERFORM DB2-OPEN-T01SDEV-4                                       
139924         PERFORM DB2-FETCH-T01SDEV-4                                      
139925         MOVE +1 TO IX                                                    
139926         PERFORM UNTIL IX > MAX-ROW                                       
139927         OR LINES-MISSING                                                 
139928           PERFORM S07-MOVE-TO-LINES                                      
139929           PERFORM DB2-FETCH-T01SDEV-4                                    
139930           ADD +1 TO IX                                                   
139931         END-PERFORM                                                      
139932         PERFORM DB2-CLOSE-T01SDEV-4                                      
139933       ELSE                                                               
139934         PERFORM DB2-COUNT-T01SDEV-2                                      
139935         IF LINES-MISSING                                                 
139936           MOVE ERR-LINES-NOT-FOUND   TO RESP-IDMSG-ERROR                 
139937           MOVE ZERO                  TO WS-COUNTER-T01SDEV               
139938         END-IF                                                           
139939         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
139940         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
139941         IF WS-COUNTER-T01SDEV > 500                                      
139942           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
139943           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
139944           MOVE 500                   TO RESP-KVRADER                     
139945         END-IF                                                           
139946         PERFORM S01-MOVE-TO-RESPOND                                      
139947         PERFORM DB2-OPEN-T01SDEV-2                                       
139948         PERFORM DB2-FETCH-T01SDEV-2                                      
139949         MOVE +1 TO IX                                                    
139950         PERFORM UNTIL IX > MAX-ROW                                       
139951         OR LINES-MISSING                                                 
139952           PERFORM S07-MOVE-TO-LINES                                      
139953           PERFORM DB2-FETCH-T01SDEV-2                                    
139954           ADD +1 TO IX                                                   
139955         END-PERFORM                                                      
139956         PERFORM DB2-CLOSE-T01SDEV-2                                      
139957       END-IF                                                             
139958     ELSE                                                                 
139959       IF T01PDEV-FLINVOIC = 'J'                                          
139960         PERFORM DB2-COUNT-T01SDEV-3                                      
139961         IF LINES-MISSING                                                 
139962           MOVE ERR-LINES-NOT-FOUND   TO RESP-IDMSG-ERROR                 
139963           MOVE ZERO                  TO WS-COUNTER-T01SDEV               
139964         END-IF                                                           
139965         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
139966         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
139967         IF WS-COUNTER-T01SDEV > 500                                      
139968           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
139969           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
139970           MOVE 500                   TO RESP-KVRADER                     
139971         END-IF                                                           
139972         PERFORM S01-MOVE-TO-RESPOND                                      
139973         PERFORM DB2-OPEN-T01SDEV-3                                       
139974         PERFORM DB2-FETCH-T01SDEV-3                                      
139975         MOVE +1 TO IX                                                    
139976         PERFORM UNTIL IX > MAX-ROW                                       
139977         OR LINES-MISSING                                                 
139978           PERFORM S07-MOVE-TO-LINES                                      
139979           PERFORM DB2-FETCH-T01SDEV-3                                    
139980           ADD +1 TO IX                                                   
139981         END-PERFORM                                                      
139982         PERFORM DB2-CLOSE-T01SDEV-3                                      
139983       ELSE                                                               
139984         PERFORM DB2-COUNT-T01SDEV-1                                      
139985         IF LINES-MISSING                                                 
139986           MOVE ERR-LINES-NOT-FOUND   TO RESP-IDMSG-ERROR                 
139987           MOVE ZERO                  TO WS-COUNTER-T01SDEV               
139988         END-IF                                                           
139989         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
139990         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
139991         IF WS-COUNTER-T01SDEV > 500                                      
139992           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
139993           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
139994           MOVE 500                   TO RESP-KVRADER                     
139995         END-IF                                                           
139996         PERFORM S01-MOVE-TO-RESPOND                                      
139997         PERFORM DB2-OPEN-T01SDEV-1                                       
139998         PERFORM DB2-FETCH-T01SDEV-1                                      
139999         MOVE +1 TO IX                                                    
140000         PERFORM UNTIL IX > MAX-ROW                                       
140001         OR LINES-MISSING                                                 
140002           PERFORM S07-MOVE-TO-LINES                                      
140003           PERFORM DB2-FETCH-T01SDEV-1                                    
140004           ADD +1 TO IX                                                   
140005         END-PERFORM                                                      
140006         PERFORM DB2-CLOSE-T01SDEV-1                                      
140007       END-IF                                                             
140008     END-IF                                                               
140009     .                                                                    
140010                                                                          
140011 S09-MOVE-TO-RESPOND SECTION.                                             
140012     MOVE 1                          TO MAIL-REQU-IDMSGVER                
140013     MOVE 'R'                        TO MAIL-REQU-KDPGMACT                
140014     MOVE IDPGM                      TO MAIL-REQU-IDUSER                  
140015                                                                          
140016     MOVE 'WF0292-001'               TO HDR-IDOUTTYPE                     
140017     MOVE SPACE                      TO HDR-IDOUTREC                      
140018     MOVE REQU-IDLEGSEL-KEY          TO HDR-IDOUTREC(1:4)                 
140019     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
140020                                                                          
140021     IF T01PDEV-FLPAYTE  = 'J'                                            
140022       MOVE 'Y'                   TO MAIL-FLPAYTE                         
140023       IF SELECT-FLPAYTE = 'N'                                            
140024         MOVE 'N'                 TO MAIL-FLPAYTE                         
140025       END-IF                                                             
140026     ELSE                                                                 
140027       MOVE T01PDEV-FLPAYTE       TO MAIL-FLPAYTE                         
140028     END-IF                                                               
140029     IF T01PDEV-FLDELTE  = 'J'                                            
140030       MOVE 'Y'                   TO MAIL-FLDELTE                         
140031       IF SELECT-FLDELTE = 'N'                                            
140032         MOVE 'N'                 TO MAIL-FLDELTE                         
140033       END-IF                                                             
140034     ELSE                                                                 
140035       MOVE T01PDEV-FLDELTE       TO MAIL-FLDELTE                         
140036     END-IF                                                               
140037     MOVE T01PDEV-REARTRAB        TO MAIL-REARTRAB                        
140038     IF SELECT-REARTRAB > T01PDEV-REARTRAB                                
140039       MOVE SELECT-REARTRAB       TO MAIL-REARTRAB                        
140040     END-IF                                                               
140041     IF SELECT-REARTRAB = ZERO                                            
140042       MOVE SELECT-REARTRAB       TO MAIL-REARTRAB                        
140043     END-IF                                                               
140044     MOVE T01PDEV-PRARTNTO-MIN    TO MAIL-PRARTNTO-MIN                    
140045     IF SELECT-PRARTNTO-MIN < T01PDEV-PRARTNTO-MIN                        
140046       MOVE SELECT-PRARTNTO-MIN   TO MAIL-PRARTNTO-MIN                    
140047     END-IF                                                               
140048     IF SELECT-PRARTNTO-MIN = ZERO                                        
140049       MOVE SELECT-PRARTNTO-MIN   TO MAIL-PRARTNTO-MIN                    
140050     END-IF                                                               
140051     MOVE T01PDEV-PRARTNTO-MAX    TO MAIL-PRARTNTO-MAX                    
140052     IF SELECT-PRARTNTO-MAX > T01PDEV-PRARTNTO-MAX                        
140053       MOVE SELECT-PRARTNTO-MAX   TO MAIL-PRARTNTO-MAX                    
140054     END-IF                                                               
140055     IF SELECT-PRARTNTO-MAX = ZERO                                        
140056       MOVE SELECT-PRARTNTO-MAX   TO MAIL-PRARTNTO-MAX                    
140057     END-IF                                                               
140058     MOVE T01PDEV-SUNTO-MIN       TO MAIL-SUNTO-MIN                       
140059     IF SELECT-SUNTO-MIN < T01PDEV-SUNTO-MIN                              
140060       MOVE SELECT-SUNTO-MIN      TO MAIL-SUNTO-MIN                       
140061     END-IF                                                               
140062     IF SELECT-SUNTO-MIN = ZERO                                           
140063       MOVE SELECT-SUNTO-MIN      TO MAIL-SUNTO-MIN                       
140064     END-IF                                                               
140065     MOVE T01PDEV-SUNTO-MAX       TO MAIL-SUNTO-MAX                       
140066     IF SELECT-SUNTO-MAX > T01PDEV-SUNTO-MAX                              
140067       MOVE SELECT-SUNTO-MAX      TO MAIL-SUNTO-MAX                       
140068     END-IF                                                               
140069     IF SELECT-SUNTO-MAX = ZERO                                           
140070       MOVE SELECT-SUNTO-MAX      TO MAIL-SUNTO-MAX                       
140071     END-IF                                                               
140072     IF T01PDEV-FLSOFT   = 'J'                                            
140073       MOVE 'Y'                   TO MAIL-FLSOFT                          
140074       IF SELECT-FLSOFT  = 'N'                                            
140075         MOVE 'N'                 TO MAIL-FLSOFT                          
140076       END-IF                                                             
140077     ELSE                                                                 
140078       MOVE T01PDEV-FLSOFT        TO MAIL-FLSOFT                          
140079     END-IF                                                               
140080     IF T01PDEV-FLFREE   = 'J'                                            
140081       MOVE 'Y'                   TO MAIL-FLFREE                          
140082       IF SELECT-FLFREE  = 'N'                                            
140083         MOVE 'N'                 TO MAIL-FLFREE                          
140084       END-IF                                                             
140085     ELSE                                                                 
140086       MOVE T01PDEV-FLFREE        TO MAIL-FLFREE                          
140087     END-IF                                                               
140088     IF T01PDEV-FLSERV   = 'J'                                            
140089       MOVE 'Y'                   TO MAIL-FLSERV                          
140090       IF SELECT-FLSERV  = 'N'                                            
140091         MOVE 'N'                 TO MAIL-FLSERV                          
140092       END-IF                                                             
140093     ELSE                                                                 
140094       MOVE T01PDEV-FLSERV        TO MAIL-FLSERV                          
140095     END-IF                                                               
140096     IF T01PDEV-FLINVOIC = 'J'                                            
140097       MOVE 'Y'                   TO MAIL-FLINVOIC                        
140098     ELSE                                                                 
140099       MOVE T01PDEV-FLINVOIC      TO MAIL-FLINVOIC                        
140100     END-IF                                                               
140101     MOVE T01PDEV-DAREGDAT        TO MAIL-DAREGDAT                        
140102     MOVE T01PDEV-DAUPPDAT        TO MAIL-DAUPPDAT                        
140103     MOVE 'PAYMENT TERMS'         TO MAIL-BETEXT-01                       
140104     MOVE 'DELIVERY TERMS'        TO MAIL-BETEXT-02                       
140105     MOVE 'HIGH DISCOUNT (%)'     TO MAIL-BETEXT-03                       
140106     MOVE 'LOW NET PRICE (SEK)'   TO MAIL-BETEXT-04                       
140107     MOVE 'HIGH NET PRICE (SEK)'  TO MAIL-BETEXT-05                       
140108     MOVE 'LOW NET VALUE (SEK)'   TO MAIL-BETEXT-06                       
140109     MOVE 'HIGH NET VALUE (SEK)'  TO MAIL-BETEXT-07                       
140110     MOVE 'SOFTWARE'              TO MAIL-BETEXT-08                       
140111     MOVE 'FREEWARE'              TO MAIL-BETEXT-09                       
140112     MOVE 'SERVICES'              TO MAIL-BETEXT-10                       
140113     MOVE 'PULS INVOICES'         TO MAIL-BETEXT-11                       
140114     MOVE 'DATE FROM'             TO MAIL-BETEXT-12                       
140115     MOVE 'DATE TO'               TO MAIL-BETEXT-13                       
140116                                                                          
140117     MOVE 'LEGAL SELLER'          TO LINE-BETEXT-01                       
140118     MOVE 'ABNORMAL VALUE CAUSE'  TO LINE-BETEXT-02                       
140119     MOVE 'INVOICE DATE'          TO LINE-BETEXT-03                       
140120     MOVE 'INVOICE NO'            TO LINE-BETEXT-04                       
140121     MOVE 'PART NUMBER'           TO LINE-BETEXT-05                       
140122     MOVE 'PART DESCRIPTION'      TO LINE-BETEXT-06                       
140123     MOVE 'FINANCIAL CUSTOMER'    TO LINE-BETEXT-07                       
140124     MOVE 'CUSTOMER INFO 1'       TO LINE-BETEXT-08                       
140125     MOVE 'CUSTOMER INFO 2'       TO LINE-BETEXT-09                       
140126     MOVE 'ORDER REFERENCE'       TO LINE-BETEXT-10                       
140127     MOVE 'FINANCIAL DOCUMENT'    TO LINE-BETEXT-11                       
140128     MOVE 'SOFTWARE'              TO LINE-BETEXT-12                       
140129     MOVE 'FREEWARE'              TO LINE-BETEXT-13                       
140130     MOVE 'DISCOUNT (%)'          TO LINE-BETEXT-14                       
140131     MOVE 'NET PRICE (SEK)'       TO LINE-BETEXT-15                       
140132     MOVE 'NET VALUE (SEK)'       TO LINE-BETEXT-16                       
140133     MOVE 'CURRENCY CODE'         TO LINE-BETEXT-17                       
140134     MOVE 'NET PRICE'             TO LINE-BETEXT-18                       
140135     MOVE 'NET VALUE'             TO LINE-BETEXT-19                       
140136                                                                          
140137     PERFORM S90-SEND-OPEN                                                
140138     PERFORM S90-PUT-HEADER                                               
140139     PERFORM S90-PUT-DOC-HEAD1                                            
140140     PERFORM S90-PUT-DOC-HEAD2                                            
140141     PERFORM S90-PUT-DOC-LINE1                                            
140142     .                                                                    
140143                                                                          
140144 S10-MOVE-LINE-TO-RESPOND SECTION.                                        
140145     IF T01PDEV-FLPAYTE = 'J'                                             
140146       MOVE 'PAYMENT TERMS MISS'      TO WS-BETEXT-A                      
140147     ELSE                                                                 
140148       MOVE '?'                       TO WS-BETEXT-A                      
140149     END-IF                                                               
140150     IF T01PDEV-FLDELTE = 'J'                                             
140151       MOVE 'DELIVERY TERMS MISS'     TO WS-BETEXT-B                      
140152     ELSE                                                                 
140153       MOVE '?'                       TO WS-BETEXT-B                      
140154     END-IF                                                               
140155     IF T01PDEV-REARTRAB > ZERO                                           
140156       MOVE 'DISCOUNT IS HIGH'        TO WS-BETEXT-C                      
140157       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
140158     ELSE                                                                 
140159       MOVE '?'                       TO WS-BETEXT-C                      
140160       MOVE T01PDEV-REARTRAB          TO WS-REARTRAB                      
140161     END-IF                                                               
140162     IF T01PDEV-PRARTNTO-MIN > ZERO                                       
140163       MOVE 'NET PRICE IS LOW'        TO WS-BETEXT-D                      
140164       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
140165     ELSE                                                                 
140166       MOVE '?'                       TO WS-BETEXT-D                      
140167       MOVE T01PDEV-PRARTNTO-MIN      TO WS-PRARTNTO-MIN                  
140168     END-IF                                                               
140169     IF T01PDEV-PRARTNTO-MAX > ZERO                                       
140170       MOVE 'NET PRICE IS HIGH'       TO WS-BETEXT-E                      
140171       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
140172     ELSE                                                                 
140173       MOVE '?'                       TO WS-BETEXT-E                      
140174       MOVE T01PDEV-PRARTNTO-MAX      TO WS-PRARTNTO-MAX                  
140175     END-IF                                                               
140176     IF T01PDEV-SUNTO-MIN        > ZERO                                   
140177       MOVE 'NET VALUE IS LOW'        TO WS-BETEXT-F                      
140178       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
140179     ELSE                                                                 
140180       MOVE '?'                       TO WS-BETEXT-F                      
140181       MOVE T01PDEV-SUNTO-MIN         TO WS-SUNTO-MIN                     
140182     END-IF                                                               
140183     IF T01PDEV-SUNTO-MAX        > ZERO                                   
140184       MOVE 'NET VALUE IS HIGH'       TO WS-BETEXT-G                      
140185       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
140186     ELSE                                                                 
140187       MOVE '?'                       TO WS-BETEXT-G                      
140188       MOVE T01PDEV-SUNTO-MAX         TO WS-SUNTO-MAX                     
140189     END-IF                                                               
140190     IF T01PDEV-FLSOFT = 'J'                                              
140191       MOVE 'J'                       TO WS-FLSOFT                        
140192     ELSE                                                                 
140193       MOVE 'N'                       TO WS-FLSOFT                        
140194     END-IF                                                               
140195     IF T01PDEV-FLFREE = 'J'                                              
140196       MOVE 'J'                       TO WS-FLFREE                        
140197     ELSE                                                                 
140198       MOVE 'N'                       TO WS-FLFREE                        
140199     END-IF                                                               
140200     IF T01PDEV-FLSERV = 'J'                                              
140201       IF T01PDEV-FLINVOIC = 'J'                                          
140202         PERFORM DB2-COUNT-T01SDEV-4                                      
140203         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
140204         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
140205         IF WS-COUNTER-T01SDEV > 500                                      
140206           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
140207           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
140208           MOVE 500                   TO RESP-KVRADER                     
140209         END-IF                                                           
140210         PERFORM DB2-OPEN-T01SDEV-4                                       
140211         PERFORM DB2-FETCH-T01SDEV-4                                      
140212         MOVE +1 TO IX                                                    
140213         PERFORM UNTIL LINES-MISSING                                      
140214           PERFORM S11-MOVE-TO-LINES                                      
140215           IF IX > 500                                                    
140216             CONTINUE                                                     
140217           ELSE                                                           
140218             PERFORM S07-MOVE-TO-LINES                                    
140219           END-IF                                                         
140220           PERFORM DB2-FETCH-T01SDEV-4                                    
140221           ADD +1 TO IX                                                   
140222         END-PERFORM                                                      
140223         PERFORM DB2-CLOSE-T01SDEV-4                                      
140224       ELSE                                                               
140225         PERFORM DB2-COUNT-T01SDEV-2                                      
140226         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
140227         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
140228         IF WS-COUNTER-T01SDEV > 500                                      
140229           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
140230           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
140231           MOVE 500                   TO RESP-KVRADER                     
140232         END-IF                                                           
140233         PERFORM DB2-OPEN-T01SDEV-2                                       
140234         PERFORM DB2-FETCH-T01SDEV-2                                      
140235         MOVE +1 TO IX                                                    
140236         PERFORM UNTIL LINES-MISSING                                      
140237           PERFORM S11-MOVE-TO-LINES                                      
140238           IF IX > 500                                                    
140239             CONTINUE                                                     
140240           ELSE                                                           
140241             PERFORM S07-MOVE-TO-LINES                                    
140242           END-IF                                                         
140243           PERFORM DB2-FETCH-T01SDEV-2                                    
140244           ADD +1 TO IX                                                   
140245         END-PERFORM                                                      
140246         PERFORM DB2-CLOSE-T01SDEV-2                                      
140247       END-IF                                                             
140248     ELSE                                                                 
140249       IF T01PDEV-FLINVOIC = 'J'                                          
140250         PERFORM DB2-COUNT-T01SDEV-3                                      
140251         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
140252         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
140253         IF WS-COUNTER-T01SDEV > 500                                      
140254           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
140255           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
140256           MOVE 500                   TO RESP-KVRADER                     
140257         END-IF                                                           
140258         PERFORM DB2-OPEN-T01SDEV-3                                       
140259         PERFORM DB2-FETCH-T01SDEV-3                                      
140260         MOVE +1 TO IX                                                    
140261         PERFORM UNTIL LINES-MISSING                                      
140262           PERFORM S11-MOVE-TO-LINES                                      
140263           IF IX > 500                                                    
140264             CONTINUE                                                     
140265           ELSE                                                           
140266             PERFORM S07-MOVE-TO-LINES                                    
140267           END-IF                                                         
140268           PERFORM DB2-FETCH-T01SDEV-3                                    
140269           ADD +1 TO IX                                                   
140270         END-PERFORM                                                      
140271         PERFORM DB2-CLOSE-T01SDEV-3                                      
140272       ELSE                                                               
140273         PERFORM DB2-COUNT-T01SDEV-1                                      
140274         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER                     
140275         MOVE WS-COUNTER-T01SDEV      TO RESP-KVRADER-TOT                 
140276         IF WS-COUNTER-T01SDEV > 500                                      
140277           MOVE ERR-TO-MANY-LINES     TO RESP-IDMSG-ERROR                 
140278           MOVE 'KVRADER'             TO RESP-IDELMT-ERROR                
140279           MOVE 500                   TO RESP-KVRADER                     
140280         END-IF                                                           
140281         PERFORM DB2-OPEN-T01SDEV-1                                       
140282         PERFORM DB2-FETCH-T01SDEV-1                                      
140283         MOVE +1 TO IX                                                    
140284         PERFORM UNTIL LINES-MISSING                                      
140285           PERFORM S11-MOVE-TO-LINES                                      
140286           IF IX > 500                                                    
140287             CONTINUE                                                     
140288           ELSE                                                           
140289             PERFORM S07-MOVE-TO-LINES                                    
140290           END-IF                                                         
140291           PERFORM DB2-FETCH-T01SDEV-1                                    
140292           ADD +1 TO IX                                                   
140293         END-PERFORM                                                      
140294         PERFORM DB2-CLOSE-T01SDEV-1                                      
140295       END-IF                                                             
140296     END-IF                                                               
140297                                                                          
140298     PERFORM S90-SEND-CLOSE                                               
140299     .                                                                    
140300                                                                          
140301 S11-MOVE-TO-LINES        SECTION.                                        
140302     MOVE T01SDEV-IDLEGSEL             TO LINE-IDLEGSEL                   
140303     MOVE T01SDEV-BETEXT               TO LINE-BETEXT                     
140304     MOVE T01SDEV-DAREGDAT             TO LINE-DAREGDAT                   
140305     MOVE T01SDEV-IDFINDOC             TO LINE-IDFINDOC                   
140306     MOVE T01SDEV-IDARTNR-FINANCE(1:9) TO LINE-IDARTNR                    
140307     MOVE T01SDEV-BEART                TO LINE-BEART                      
140308     MOVE T01SDEV-IDPARTNR             TO LINE-IDPARTNR                   
140309     MOVE T01SDEV-IDEXCUST-1           TO LINE-IDEXCUST-1                 
140310     MOVE T01SDEV-IDEXCUST-2           TO LINE-IDEXCUST-2                 
140311     MOVE T01SDEV-IDREF                TO LINE-IDREF                      
140312     MOVE T01SDEV-KDFINDOC             TO LINE-KDFINDOC                   
140313     MOVE T01SDEV-FLSOFT               TO LINE-FLSOFT                     
140314     MOVE T01SDEV-FLFREE               TO LINE-FLFREE                     
140315     MOVE T01SDEV-REARTRAB             TO LINE-REARTRAB                   
140316     MOVE T01SDEV-PRARTNTO-SEK         TO LINE-PRARTNTO-SEK               
140317     MOVE T01SDEV-SUNTO-SEK            TO LINE-SUNTO-SEK                  
140318     MOVE T01SDEV-KDVALISO             TO LINE-KDVALISO                   
140319     MOVE T01SDEV-PRARTNTO             TO LINE-PRARTNTO                   
140320     MOVE T01SDEV-SUNTO                TO LINE-SUNTO                      
140321                                                                          
140322     PERFORM S90-PUT-DOC-LINE2                                            
140323     .                                                                    
140324                                                                          
140325 S80-FETCH-REQUEST-ARGUMENT SECTION.                                      
140326     MOVE 'GETARG'                   TO SUB-KDFUNC                        
140327     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
140328     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
140329                                                                          
140330     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
140331                                                                          
140332     IF SUB-KDRC > 0                                                      
140333       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
140334       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
140335       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140336       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140337     END-IF                                                               
140338     .                                                                    
140339                                                                          
140340 S81-RETURN-RESPONSE SECTION.                                             
140341     MOVE 'RETURN'                   TO SUB-KDFUNC                        
140342     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
140343                                                                          
140344     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
140345                                                                          
140346     IF SUB-KDRC > 0                                                      
140347       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
140348       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
140349       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140350       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140351     END-IF                                                               
140352     .                                                                    
140353     EJECT                                                                
140354                                                                          
140355 S90-SEND-OPEN SECTION.                                                   
140356     MOVE WS-ADRESS2                      TO SEND-ADDISPABS               
140357     MOVE 'OPEN'                          TO SEND-KDFUNC                  
140358     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140359                         SEND-OPEN-AREA                                   
140360     IF SEND-KDRC > ZERO                                                  
140361       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140362       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
140363       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140364       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140365     END-IF                                                               
140366     .                                                                    
140367                                                                          
140368 S90-PUT-HEADER SECTION.                                                  
140369     MOVE 'PUT'                           TO SEND-KDFUNC                  
140370*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140371     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
140372     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140373                         SEND-KVDLEN                                      
140374                         HDR-AREA                                         
140375     IF SEND-KDRC > ZERO                                                  
140376       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140377       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140378       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140379       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140380     END-IF                                                               
140381     .                                                                    
140382                                                                          
140383 S90-PUT-DOC-HEAD1 SECTION.                                               
140384     MOVE 'PUT'                           TO SEND-KDFUNC                  
140385*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140386     MOVE LENGTH OF DOC-HEAD-AREA1        TO SEND-KVDLEN                  
140387     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140388                         SEND-KVDLEN                                      
140389                         DOC-HEAD-AREA1                                   
140390     IF SEND-KDRC > ZERO                                                  
140391       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140392       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140393       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140394       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140395     END-IF                                                               
140396     .                                                                    
140397                                                                          
140398 S90-PUT-DOC-HEAD2 SECTION.                                               
140399     MOVE 'PUT'                           TO SEND-KDFUNC                  
140400*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140401     MOVE LENGTH OF DOC-HEAD-AREA2        TO SEND-KVDLEN                  
140402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140403                         SEND-KVDLEN                                      
140404                         DOC-HEAD-AREA2                                   
140405     IF SEND-KDRC > ZERO                                                  
140406       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140407       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140408       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140409       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140410     END-IF                                                               
140411     .                                                                    
140412                                                                          
140413 S90-PUT-DOC-LINE1 SECTION.                                               
140414     MOVE 'PUT'                           TO SEND-KDFUNC                  
140415*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140416     MOVE LENGTH OF DOC-LINE-AREA1        TO SEND-KVDLEN                  
140417     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140418                         SEND-KVDLEN                                      
140419                         DOC-LINE-AREA1                                   
140420     IF SEND-KDRC > ZERO                                                  
140421       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140422       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140423       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140424       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140425     END-IF                                                               
140426     .                                                                    
140427                                                                          
140428 S90-PUT-DOC-LINE2 SECTION.                                               
140429     MOVE 'PUT'                           TO SEND-KDFUNC                  
140430*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140431     MOVE LENGTH OF DOC-LINE-AREA2        TO SEND-KVDLEN                  
140432     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140433                         SEND-KVDLEN                                      
140434                         DOC-LINE-AREA2                                   
140435     IF SEND-KDRC > ZERO                                                  
140436       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
140437       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
140438       DELIMITED BY SIZE INTO ERROR-TEXT                                  
140439       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
140440     END-IF                                                               
140441     .                                                                    
140442                                                                          
140443 S90-SEND-CLOSE SECTION.                                                  
140444     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
140445*    MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
140446     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140447     .                                                                    
140448                                                                          
140449*    --- DB2 SECTIONS                                                     
140450 DB2-SELECT-T01LSEL       SECTION.                                        
140451     MOVE 000100 TO GOOD-SQLCODECODES                                     
140452                                                                          
140453     EXEC SQL                                                             
140460        SELECT  BELEGRAD_1                                                
140500                                                                          
140600        INTO   :T01LSEL-BELEGRAD-1                                        
140700                                                                          
140800        FROM    T01LSEL                                                   
140900                                                                          
141000        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
141100            AND KDSTATUS = :WS-CURRENT                                    
141200     END-EXEC                                                             
141300                                                                          
141400     MOVE SQLCODE TO SQLCODE-WS                                           
141500     PERFORM DB2-STATUS-CHECK                                             
141600     .                                                                    
141700                                                                          
143600 DB2-SELECT-T01PDEV-LIST SECTION.                                         
143700     MOVE 000100305  TO GOOD-SQLCODECODES                                 
143800                                                                          
143900     EXEC SQL                                                             
144000         SELECT  IDLEGSEL                                                 
144100                ,KDBEHX                                                   
144200                ,FLPAYTE                                                  
144300                ,FLDELTE                                                  
144400                ,REARTRAB                                                 
144500                ,PRARTNTO_MIN                                             
144600                ,PRARTNTO_MAX                                             
144700                ,SUNTO_MIN                                                
144800                ,SUNTO_MAX                                                
144900                ,FLSOFT                                                   
145000                ,FLFREE                                                   
145100                ,FLSERV                                                   
145200                ,FLINVOIC                                                 
145300                ,KVMANAD                                                  
145400                ,DAREGDAT                                                 
145500                ,DAUPPDAT                                                 
145600                ,IDUSER                                                   
145700                                                                          
145800         INTO  :T01PDEV-IDLEGSEL                                          
145900             , :T01PDEV-KDBEHX                                            
146000             , :T01PDEV-FLPAYTE                                           
146100             , :T01PDEV-FLDELTE                                           
146200             , :T01PDEV-REARTRAB                                          
146300             , :T01PDEV-PRARTNTO-MIN                                      
146400             , :T01PDEV-PRARTNTO-MAX                                      
146500             , :T01PDEV-SUNTO-MIN                                         
146600             , :T01PDEV-SUNTO-MAX                                         
146700             , :T01PDEV-FLSOFT                                            
146800             , :T01PDEV-FLFREE                                            
146900             , :T01PDEV-FLSERV                                            
147000             , :T01PDEV-FLINVOIC                                          
147100             , :T01PDEV-KVMANAD                                           
147200             , :T01PDEV-DAREGDAT                                          
147300             , :T01PDEV-DAUPPDAT                                          
147400             , :T01PDEV-IDUSER                                            
147500                                                                          
147600         FROM  T01PDEV                                                    
147700                                                                          
147800         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
147900         AND   KDBEHX   = :REQU-KDBEHX-KEY                                
148000     END-EXEC                                                             
148100                                                                          
148200     MOVE SQLCODE TO SQLCODE-WS                                           
148300     PERFORM DB2-STATUS-CHECK                                             
148400     .                                                                    
148500                                                                          
148510 DB2-SELECT-T01PDEV-SELECT SECTION.                                       
148520     MOVE 000100305  TO GOOD-SQLCODECODES                                 
148530                                                                          
148540     EXEC SQL                                                             
148550         SELECT  IDLEGSEL                                                 
148560                ,KDBEHX                                                   
148570                ,FLPAYTE                                                  
148580                ,FLDELTE                                                  
148590                ,REARTRAB                                                 
148591                ,PRARTNTO_MIN                                             
148592                ,PRARTNTO_MAX                                             
148593                ,SUNTO_MIN                                                
148594                ,SUNTO_MAX                                                
148595                ,FLSOFT                                                   
148596                ,FLFREE                                                   
148597                ,FLSERV                                                   
148598                ,FLINVOIC                                                 
148599                ,KVMANAD                                                  
148600                ,DAREGDAT                                                 
148601                ,DAUPPDAT                                                 
148602                ,IDUSER                                                   
148603                                                                          
148604         INTO  :SELECT-IDLEGSEL                                           
148605             , :SELECT-KDBEHX                                             
148606             , :SELECT-FLPAYTE                                            
148607             , :SELECT-FLDELTE                                            
148608             , :SELECT-REARTRAB                                           
148609             , :SELECT-PRARTNTO-MIN                                       
148610             , :SELECT-PRARTNTO-MAX                                       
148611             , :SELECT-SUNTO-MIN                                          
148612             , :SELECT-SUNTO-MAX                                          
148613             , :SELECT-FLSOFT                                             
148614             , :SELECT-FLFREE                                             
148615             , :SELECT-FLSERV                                             
148616             , :SELECT-FLINVOIC                                           
148617             , :SELECT-KVMANAD                                            
148618             , :SELECT-DAREGDAT                                           
148619             , :SELECT-DAUPPDAT                                           
148620             , :SELECT-IDUSER                                             
148621                                                                          
148622         FROM  T01PDEV                                                    
148623                                                                          
148624         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
148625         AND   KDBEHX   = 'S'                                             
148626     END-EXEC                                                             
148627                                                                          
148628     MOVE SQLCODE TO SQLCODE-WS                                           
148629     PERFORM DB2-STATUS-CHECK                                             
148630     .                                                                    
148631                                                                          
148632 DB2-SELECT-T01SDEV-MAX-DATE SECTION.                                     
148633     MOVE 000100305  TO GOOD-SQLCODECODES                                 
148634                                                                          
148635     EXEC SQL                                                             
148636         SELECT  MAX(DAREGDAT)                                            
148653                                                                          
148654         INTO  :WS-DAREGDAT-MAX-DATE                                      
148671                                                                          
148672         FROM  T01SDEV                                                    
148673                                                                          
148674         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
148676     END-EXEC                                                             
148677                                                                          
148678     MOVE SQLCODE TO SQLCODE-WS                                           
148679     PERFORM DB2-STATUS-CHECK                                             
148680     .                                                                    
148681                                                                          
148682 DB2-SELECT-T01SDEV-MIN-DATE SECTION.                                     
148683     MOVE 000100305  TO GOOD-SQLCODECODES                                 
148684                                                                          
148685     EXEC SQL                                                             
148686         SELECT  MIN(DAREGDAT)                                            
148687                                                                          
148688         INTO  :WS-DAREGDAT-MIN-DATE                                      
148689                                                                          
148690         FROM  T01SDEV                                                    
148691                                                                          
148692         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
148693     END-EXEC                                                             
148694                                                                          
148695     MOVE SQLCODE TO SQLCODE-WS                                           
148696     PERFORM DB2-STATUS-CHECK                                             
148697     .                                                                    
148698                                                                          
148699 DB2-UPDATE-T01PDEV SECTION.                                              
148700     MOVE 000     TO GOOD-SQLCODECODES                                    
148800                                                                          
148900     EXEC SQL                                                             
149000        UPDATE T01PDEV                                                    
149100           SET                                                            
149200                 IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
149300               , KDBEHX       = :REQU-KDBEHX-KEY                          
149400               , FLPAYTE      = :REQU-FLPAYTE                             
149500               , FLDELTE      = :REQU-FLDELTE                             
149600               , REARTRAB     = :WS-REARTRAB-NUM                          
149700               , PRARTNTO_MIN = :WS-PRARTNTO-MIN-NUM                      
149800               , PRARTNTO_MAX = :WS-PRARTNTO-MAX-NUM                      
149900               , SUNTO_MIN    = :WS-SUNTO-MIN-NUM                         
150000               , SUNTO_MAX    = :WS-SUNTO-MAX-NUM                         
150100               , FLSOFT       = :REQU-FLSOFT                              
150200               , FLFREE       = :REQU-FLFREE                              
150300               , FLSERV       = :REQU-FLSERV                              
150400               , FLINVOIC     = :REQU-FLINVOIC                            
150500               , KVMANAD      = :WS-KVMANAD-NUM                           
150600               , DAREGDAT     = :REQU-DAREGDAT                            
150700               , DAUPPDAT     = :REQU-DAUPPDAT                            
150800               , IDUSER       = :REQU-IDUSER                              
150900                                                                          
151000         WHERE   IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
151100         AND     KDBEHX       = :REQU-KDBEHX-KEY                          
151200     END-EXEC                                                             
151300                                                                          
151400     MOVE SQLCODE TO SQLCODE-WS                                           
151500     PERFORM DB2-STATUS-CHECK                                             
151600     .                                                                    
151700                                                                          
151800 DB2-INSERT-T01PDEV  SECTION.                                             
151900     MOVE 000   TO GOOD-SQLCODECODES                                      
152000                                                                          
152100     EXEC SQL                                                             
152200         INSERT INTO T01PDEV                                              
152300            (IDLEGSEL                                                     
152400            ,KDBEHX                                                       
152500            ,FLPAYTE                                                      
152600            ,FLDELTE                                                      
152700            ,REARTRAB                                                     
152800            ,PRARTNTO_MIN                                                 
152900            ,PRARTNTO_MAX                                                 
153000            ,SUNTO_MIN                                                    
153100            ,SUNTO_MAX                                                    
153200            ,FLSOFT                                                       
153300            ,FLFREE                                                       
153400            ,FLSERV                                                       
153500            ,FLINVOIC                                                     
153600            ,KVMANAD                                                      
153700            ,DAREGDAT                                                     
153800            ,DAUPPDAT                                                     
153900            ,IDUSER)                                                      
154000         VALUES                                                           
154100            (:REQU-IDLEGSEL-KEY                                           
154200            ,:REQU-KDBEHX-KEY                                             
154300            ,:REQU-FLPAYTE                                                
154400            ,:REQU-FLDELTE                                                
154500            ,:WS-REARTRAB-NUM                                             
154600            ,:WS-PRARTNTO-MIN-NUM                                         
154700            ,:WS-PRARTNTO-MAX-NUM                                         
154800            ,:WS-SUNTO-MIN-NUM                                            
154900            ,:WS-SUNTO-MAX-NUM                                            
155000            ,:REQU-FLSOFT                                                 
155100            ,:REQU-FLFREE                                                 
155200            ,:REQU-FLSERV                                                 
155300            ,:REQU-FLINVOIC                                               
155400            ,:WS-KVMANAD-NUM                                              
155500            ,:T01PDEV-DAREGDAT                                            
155600            ,:T01PDEV-DAUPPDAT                                            
155700            ,:REQU-IDUSER)                                                
155800     END-EXEC                                                             
155900     MOVE SQLCODE TO SQLCODE-WS                                           
156000     PERFORM DB2-STATUS-CHECK                                             
156100     .                                                                    
156200                                                                          
156210* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
156220 DB2-COUNT-T01SDEV-1 SECTION.                                             
156240     EXEC SQL                                                             
156250           SELECT COUNT(*)                                                
156260                                                                          
156270           INTO  :WS-COUNTER-T01SDEV                                      
156280                                                                          
156290           FROM   T01SDEV                                                 
156291                                                                          
156293           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156294           AND     (FLSOFT =    :WS-FLSOFT                                
156295           OR       FLSOFT LIKE 'N%')                                     
156296           AND     (FLFREE =    :WS-FLFREE                                
156297           OR       FLFREE LIKE 'N%')                                     
156298           AND      BEART > ' '                                           
156299           AND      DAREGDAT >= :WS-DAREGDAT                              
156300           AND      DAREGDAT <= :WS-DAUPPDAT                              
156301           AND    ((BETEXT   = :WS-BETEXT-A)                              
156302           OR      (BETEXT   = :WS-BETEXT-B)                              
156303           OR      (BETEXT   = :WS-BETEXT-C                               
156304           AND      REARTRAB > :WS-REARTRAB)                              
156305           OR      (BETEXT   = :WS-BETEXT-D                               
156306           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156307           OR      (BETEXT   = :WS-BETEXT-E                               
156308           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156309           OR      (BETEXT   = :WS-BETEXT-F                               
156310           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156311           OR      (BETEXT   = :WS-BETEXT-G                               
156312           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156313     END-EXEC                                                             
156314                                                                          
156315     MOVE 000100  TO GOOD-SQLCODECODES                                    
156316                                                                          
156317     MOVE SQLCODE TO SQLCODE-WS                                           
156318     PERFORM DB2-STATUS-CHECK                                             
156319     .                                                                    
156320                                                                          
156321 DB2-OPEN-T01SDEV-1 SECTION.                                              
156322     MOVE 000100 TO GOOD-SQLCODECODES                                     
156323                                                                          
156324     EXEC SQL                                                             
156325         DECLARE T01SDEV-1 CURSOR WITH HOLD FOR                           
156326                                                                          
156327           SELECT  IDLEGSEL                                               
156328                  ,DAREGDAT                                               
156329                  ,BETEXT                                                 
156330                  ,IDFINDOC                                               
156331                  ,IDARTNR_FINANCE                                        
156332                  ,BEART                                                  
156333                  ,IDPARTNR                                               
156334                  ,IDEXCUST_1                                             
156335                  ,IDEXCUST_2                                             
156336                  ,IDREF                                                  
156337                  ,KDFINDOC                                               
156338                  ,FLSOFT                                                 
156339                  ,FLFREE                                                 
156340                  ,REARTRAB                                               
156341                  ,PRARTNTO                                               
156342                  ,SUNTO                                                  
156343                  ,KDVALISO                                               
156344                  ,PRARTNTO_SEK                                           
156345                  ,SUNTO_SEK                                              
156346                                                                          
156347           FROM    T01SDEV                                                
156348                                                                          
156349           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156350           AND     (FLSOFT =    :WS-FLSOFT                                
156351           OR       FLSOFT LIKE 'N%')                                     
156352           AND     (FLFREE =    :WS-FLFREE                                
156353           OR       FLFREE LIKE 'N%')                                     
156354           AND      BEART > ' '                                           
156355           AND      DAREGDAT >= :WS-DAREGDAT                              
156356           AND      DAREGDAT <= :WS-DAUPPDAT                              
156357           AND    ((BETEXT   = :WS-BETEXT-A)                              
156358           OR      (BETEXT   = :WS-BETEXT-B)                              
156359           OR      (BETEXT   = :WS-BETEXT-C                               
156360           AND      REARTRAB > :WS-REARTRAB)                              
156361           OR      (BETEXT   = :WS-BETEXT-D                               
156362           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156363           OR      (BETEXT   = :WS-BETEXT-E                               
156364           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156365           OR      (BETEXT   = :WS-BETEXT-F                               
156366           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156367           OR      (BETEXT   = :WS-BETEXT-G                               
156368           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156369                                                                          
156370           ORDER BY IDLEGSEL                                              
156371     END-EXEC                                                             
156372                                                                          
156373     MOVE 000100  TO GOOD-SQLCODECODES                                    
156374                                                                          
156375     EXEC SQL                                                             
156376        OPEN T01SDEV-1                                                    
156377     END-EXEC                                                             
156378                                                                          
156379     MOVE SQLCODE TO SQLCODE-WS                                           
156380     PERFORM DB2-STATUS-CHECK                                             
156381     .                                                                    
156382                                                                          
156383 DB2-FETCH-T01SDEV-1 SECTION.                                             
156384     MOVE 000100  TO GOOD-SQLCODECODES                                    
156385                                                                          
156386     EXEC SQL                                                             
156387         FETCH T01SDEV-1                                                  
156388                                                                          
156389         INTO :T01SDEV-IDLEGSEL                                           
156390             ,:T01SDEV-DAREGDAT                                           
156391             ,:T01SDEV-BETEXT                                             
156392             ,:T01SDEV-IDFINDOC                                           
156393             ,:T01SDEV-IDARTNR-FINANCE                                    
156394             ,:T01SDEV-BEART                                              
156395             ,:T01SDEV-IDPARTNR                                           
156396             ,:T01SDEV-IDEXCUST-1                                         
156397             ,:T01SDEV-IDEXCUST-2                                         
156398             ,:T01SDEV-IDREF                                              
156399             ,:T01SDEV-KDFINDOC                                           
156400             ,:T01SDEV-FLSOFT                                             
156401             ,:T01SDEV-FLFREE                                             
156402             ,:T01SDEV-REARTRAB                                           
156403             ,:T01SDEV-PRARTNTO                                           
156404             ,:T01SDEV-SUNTO                                              
156405             ,:T01SDEV-KDVALISO                                           
156406             ,:T01SDEV-PRARTNTO-SEK                                       
156407             ,:T01SDEV-SUNTO-SEK                                          
156408                                                                          
156409     END-EXEC                                                             
156410                                                                          
156411     MOVE SQLCODE TO SQLCODE-WS                                           
156412     PERFORM DB2-STATUS-CHECK                                             
156413     .                                                                    
156414                                                                          
156415 DB2-CLOSE-T01SDEV-1 SECTION.                                             
156416     EXEC SQL                                                             
156417        CLOSE T01SDEV-1                                                   
156418     END-EXEC                                                             
156419     .                                                                    
156420                                                                          
156421* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
156422 DB2-COUNT-T01SDEV-2 SECTION.                                             
156423     EXEC SQL                                                             
156424           SELECT COUNT(*)                                                
156425                                                                          
156426           INTO  :WS-COUNTER-T01SDEV                                      
156427                                                                          
156428           FROM   T01SDEV                                                 
156429                                                                          
156430           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156431           AND     (FLSOFT =    :WS-FLSOFT                                
156432           OR       FLSOFT LIKE 'N%')                                     
156433           AND     (FLFREE =    :WS-FLFREE                                
156434           OR       FLFREE LIKE 'N%')                                     
156435           AND      DAREGDAT >= :WS-DAREGDAT                              
156436           AND      DAREGDAT <= :WS-DAUPPDAT                              
156437           AND    ((BETEXT   = :WS-BETEXT-A)                              
156438           OR      (BETEXT   = :WS-BETEXT-B)                              
156439           OR      (BETEXT   = :WS-BETEXT-C                               
156440           AND      REARTRAB > :WS-REARTRAB)                              
156441           OR      (BETEXT   = :WS-BETEXT-D                               
156442           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156443           OR      (BETEXT   = :WS-BETEXT-E                               
156444           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156445           OR      (BETEXT   = :WS-BETEXT-F                               
156446           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156447           OR      (BETEXT   = :WS-BETEXT-G                               
156448           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156449     END-EXEC                                                             
156450                                                                          
156451     MOVE 000100  TO GOOD-SQLCODECODES                                    
156452                                                                          
156453     MOVE SQLCODE TO SQLCODE-WS                                           
156454     PERFORM DB2-STATUS-CHECK                                             
156455     .                                                                    
156456                                                                          
156457 DB2-OPEN-T01SDEV-2 SECTION.                                              
156458     MOVE 000100 TO GOOD-SQLCODECODES                                     
156459                                                                          
156460     EXEC SQL                                                             
156461         DECLARE T01SDEV-2 CURSOR WITH HOLD FOR                           
156462                                                                          
156463           SELECT  IDLEGSEL                                               
156464                  ,DAREGDAT                                               
156465                  ,BETEXT                                                 
156466                  ,IDFINDOC                                               
156467                  ,IDARTNR_FINANCE                                        
156468                  ,BEART                                                  
156469                  ,IDPARTNR                                               
156470                  ,IDEXCUST_1                                             
156471                  ,IDEXCUST_2                                             
156472                  ,IDREF                                                  
156473                  ,KDFINDOC                                               
156474                  ,FLSOFT                                                 
156475                  ,FLFREE                                                 
156476                  ,REARTRAB                                               
156477                  ,PRARTNTO                                               
156478                  ,SUNTO                                                  
156479                  ,KDVALISO                                               
156480                  ,PRARTNTO_SEK                                           
156481                  ,SUNTO_SEK                                              
156482                                                                          
156483           FROM    T01SDEV                                                
156484                                                                          
156485           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156486           AND     (FLSOFT =    :WS-FLSOFT                                
156487           OR       FLSOFT LIKE 'N%')                                     
156488           AND     (FLFREE =    :WS-FLFREE                                
156489           OR       FLFREE LIKE 'N%')                                     
156491           AND      DAREGDAT >= :WS-DAREGDAT                              
156492           AND      DAREGDAT <= :WS-DAUPPDAT                              
156493           AND    ((BETEXT   = :WS-BETEXT-A)                              
156494           OR      (BETEXT   = :WS-BETEXT-B)                              
156495           OR      (BETEXT   = :WS-BETEXT-C                               
156496           AND      REARTRAB > :WS-REARTRAB)                              
156497           OR      (BETEXT   = :WS-BETEXT-D                               
156498           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156499           OR      (BETEXT   = :WS-BETEXT-E                               
156500           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156501           OR      (BETEXT   = :WS-BETEXT-F                               
156502           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156503           OR      (BETEXT   = :WS-BETEXT-G                               
156504           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156505                                                                          
156506           ORDER BY IDLEGSEL                                              
156507     END-EXEC                                                             
156508                                                                          
156509     MOVE 000100  TO GOOD-SQLCODECODES                                    
156510                                                                          
156511     EXEC SQL                                                             
156512        OPEN T01SDEV-2                                                    
156513     END-EXEC                                                             
156514                                                                          
156515     MOVE SQLCODE TO SQLCODE-WS                                           
156516     PERFORM DB2-STATUS-CHECK                                             
156517     .                                                                    
156518                                                                          
156519 DB2-FETCH-T01SDEV-2 SECTION.                                             
156520     MOVE 000100  TO GOOD-SQLCODECODES                                    
156521                                                                          
156522     EXEC SQL                                                             
156523         FETCH T01SDEV-2                                                  
156524                                                                          
156525         INTO :T01SDEV-IDLEGSEL                                           
156526             ,:T01SDEV-DAREGDAT                                           
156527             ,:T01SDEV-BETEXT                                             
156528             ,:T01SDEV-IDFINDOC                                           
156529             ,:T01SDEV-IDARTNR-FINANCE                                    
156530             ,:T01SDEV-BEART                                              
156531             ,:T01SDEV-IDPARTNR                                           
156532             ,:T01SDEV-IDEXCUST-1                                         
156533             ,:T01SDEV-IDEXCUST-2                                         
156534             ,:T01SDEV-IDREF                                              
156535             ,:T01SDEV-KDFINDOC                                           
156536             ,:T01SDEV-FLSOFT                                             
156537             ,:T01SDEV-FLFREE                                             
156538             ,:T01SDEV-REARTRAB                                           
156539             ,:T01SDEV-PRARTNTO                                           
156540             ,:T01SDEV-SUNTO                                              
156541             ,:T01SDEV-KDVALISO                                           
156542             ,:T01SDEV-PRARTNTO-SEK                                       
156543             ,:T01SDEV-SUNTO-SEK                                          
156544     END-EXEC                                                             
156545                                                                          
156546     MOVE SQLCODE TO SQLCODE-WS                                           
156547     PERFORM DB2-STATUS-CHECK                                             
156548     .                                                                    
156549                                                                          
156550 DB2-CLOSE-T01SDEV-2 SECTION.                                             
156551     EXEC SQL                                                             
156552        CLOSE T01SDEV-2                                                   
156553     END-EXEC                                                             
156554     .                                                                    
156555                                                                          
156556* * * * * * * * * *   - CURSOR-3 -   * * * * * * * * * * * * * * *        
156557 DB2-COUNT-T01SDEV-3 SECTION.                                             
156558     EXEC SQL                                                             
156559           SELECT COUNT(*)                                                
156560                                                                          
156561           INTO  :WS-COUNTER-T01SDEV                                      
156562                                                                          
156563           FROM   T01SDEV                                                 
156564                                                                          
156565           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156566           AND     (FLSOFT =    :WS-FLSOFT                                
156567           OR       FLSOFT LIKE 'N%')                                     
156568           AND     (FLFREE =    :WS-FLFREE                                
156569           OR       FLFREE LIKE 'N%')                                     
156570           AND      KDFINDOC = 'INV'                                      
156571           AND      BEART > ' '                                           
156572           AND      DAREGDAT >= :WS-DAREGDAT                              
156573           AND      DAREGDAT <= :WS-DAUPPDAT                              
156574           AND    ((BETEXT   = :WS-BETEXT-A)                              
156575           OR      (BETEXT   = :WS-BETEXT-B)                              
156576           OR      (BETEXT   = :WS-BETEXT-C                               
156577           AND      REARTRAB > :WS-REARTRAB)                              
156578           OR      (BETEXT   = :WS-BETEXT-D                               
156579           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156580           OR      (BETEXT   = :WS-BETEXT-E                               
156581           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156582           OR      (BETEXT   = :WS-BETEXT-F                               
156583           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156584           OR      (BETEXT   = :WS-BETEXT-G                               
156585           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156586     END-EXEC                                                             
156587                                                                          
156588     MOVE 000100  TO GOOD-SQLCODECODES                                    
156589                                                                          
156590     MOVE SQLCODE TO SQLCODE-WS                                           
156591     PERFORM DB2-STATUS-CHECK                                             
156592     .                                                                    
156593                                                                          
156594 DB2-OPEN-T01SDEV-3 SECTION.                                              
156595     MOVE 000100 TO GOOD-SQLCODECODES                                     
156596                                                                          
156597     EXEC SQL                                                             
156598         DECLARE T01SDEV-3 CURSOR WITH HOLD FOR                           
156599                                                                          
156600           SELECT  IDLEGSEL                                               
156601                  ,DAREGDAT                                               
156602                  ,BETEXT                                                 
156603                  ,IDFINDOC                                               
156604                  ,IDARTNR_FINANCE                                        
156605                  ,BEART                                                  
156606                  ,IDPARTNR                                               
156607                  ,IDEXCUST_1                                             
156608                  ,IDEXCUST_2                                             
156609                  ,IDREF                                                  
156610                  ,KDFINDOC                                               
156611                  ,FLSOFT                                                 
156612                  ,FLFREE                                                 
156613                  ,REARTRAB                                               
156614                  ,PRARTNTO                                               
156615                  ,SUNTO                                                  
156616                  ,KDVALISO                                               
156617                  ,PRARTNTO_SEK                                           
156618                  ,SUNTO_SEK                                              
156619                                                                          
156620           FROM    T01SDEV                                                
156621                                                                          
156622           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156623           AND     (FLSOFT =    :WS-FLSOFT                                
156624           OR       FLSOFT LIKE 'N%')                                     
156625           AND     (FLFREE =    :WS-FLFREE                                
156626           OR       FLFREE LIKE 'N%')                                     
156627           AND      KDFINDOC = 'INV'                                      
156628           AND      BEART > ' '                                           
156629           AND      DAREGDAT >= :WS-DAREGDAT                              
156630           AND      DAREGDAT <= :WS-DAUPPDAT                              
156631           AND    ((BETEXT   = :WS-BETEXT-A)                              
156632           OR      (BETEXT   = :WS-BETEXT-B)                              
156633           OR      (BETEXT   = :WS-BETEXT-C                               
156634           AND      REARTRAB > :WS-REARTRAB)                              
156635           OR      (BETEXT   = :WS-BETEXT-D                               
156636           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156637           OR      (BETEXT   = :WS-BETEXT-E                               
156638           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156639           OR      (BETEXT   = :WS-BETEXT-F                               
156640           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156641           OR      (BETEXT   = :WS-BETEXT-G                               
156642           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156643                                                                          
156644           ORDER BY IDLEGSEL                                              
156645     END-EXEC                                                             
156646                                                                          
156647     MOVE 000100  TO GOOD-SQLCODECODES                                    
156648                                                                          
156649     EXEC SQL                                                             
156650        OPEN T01SDEV-3                                                    
156651     END-EXEC                                                             
156652                                                                          
156653     MOVE SQLCODE TO SQLCODE-WS                                           
156654     PERFORM DB2-STATUS-CHECK                                             
156655     .                                                                    
156656                                                                          
156657 DB2-FETCH-T01SDEV-3 SECTION.                                             
156658     MOVE 000100  TO GOOD-SQLCODECODES                                    
156659                                                                          
156660     EXEC SQL                                                             
156661         FETCH T01SDEV-3                                                  
156662                                                                          
156663         INTO :T01SDEV-IDLEGSEL                                           
156664             ,:T01SDEV-DAREGDAT                                           
156665             ,:T01SDEV-BETEXT                                             
156666             ,:T01SDEV-IDFINDOC                                           
156667             ,:T01SDEV-IDARTNR-FINANCE                                    
156668             ,:T01SDEV-BEART                                              
156669             ,:T01SDEV-IDPARTNR                                           
156670             ,:T01SDEV-IDEXCUST-1                                         
156671             ,:T01SDEV-IDEXCUST-2                                         
156672             ,:T01SDEV-IDREF                                              
156673             ,:T01SDEV-KDFINDOC                                           
156674             ,:T01SDEV-FLSOFT                                             
156675             ,:T01SDEV-FLFREE                                             
156676             ,:T01SDEV-REARTRAB                                           
156677             ,:T01SDEV-PRARTNTO                                           
156678             ,:T01SDEV-SUNTO                                              
156679             ,:T01SDEV-KDVALISO                                           
156680             ,:T01SDEV-PRARTNTO-SEK                                       
156681             ,:T01SDEV-SUNTO-SEK                                          
156682     END-EXEC                                                             
156683                                                                          
156684     MOVE SQLCODE TO SQLCODE-WS                                           
156685     PERFORM DB2-STATUS-CHECK                                             
156686     .                                                                    
156687                                                                          
156688 DB2-CLOSE-T01SDEV-3 SECTION.                                             
156689     EXEC SQL                                                             
156690        CLOSE T01SDEV-3                                                   
156691     END-EXEC                                                             
156692     .                                                                    
156693                                                                          
156694* * * * * * * * * *   - CURSOR-4 -   * * * * * * * * * * * * * * *        
156695 DB2-COUNT-T01SDEV-4 SECTION.                                             
156696     EXEC SQL                                                             
156697           SELECT COUNT(*)                                                
156698                                                                          
156699           INTO  :WS-COUNTER-T01SDEV                                      
156700                                                                          
156701           FROM   T01SDEV                                                 
156702                                                                          
156703           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156704           AND     (FLSOFT =    :WS-FLSOFT                                
156705           OR       FLSOFT LIKE 'N%')                                     
156706           AND     (FLFREE =    :WS-FLFREE                                
156707           OR       FLFREE LIKE 'N%')                                     
156709           AND      KDFINDOC = 'INV'                                      
156710           AND      DAREGDAT >= :WS-DAREGDAT                              
156711           AND      DAREGDAT <= :WS-DAUPPDAT                              
156712           AND    ((BETEXT   = :WS-BETEXT-A)                              
156713           OR      (BETEXT   = :WS-BETEXT-B)                              
156714           OR      (BETEXT   = :WS-BETEXT-C                               
156715           AND      REARTRAB > :WS-REARTRAB)                              
156716           OR      (BETEXT   = :WS-BETEXT-D                               
156717           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156718           OR      (BETEXT   = :WS-BETEXT-E                               
156719           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156720           OR      (BETEXT   = :WS-BETEXT-F                               
156721           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156722           OR      (BETEXT   = :WS-BETEXT-G                               
156723           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156724     END-EXEC                                                             
156725                                                                          
156726     MOVE 000100  TO GOOD-SQLCODECODES                                    
156727                                                                          
156728     MOVE SQLCODE TO SQLCODE-WS                                           
156729     PERFORM DB2-STATUS-CHECK                                             
156730     .                                                                    
156731                                                                          
156732 DB2-OPEN-T01SDEV-4 SECTION.                                              
156733     MOVE 000100 TO GOOD-SQLCODECODES                                     
156734                                                                          
156735     EXEC SQL                                                             
156736         DECLARE T01SDEV-4 CURSOR WITH HOLD FOR                           
156737                                                                          
156738           SELECT  IDLEGSEL                                               
156739                  ,DAREGDAT                                               
156740                  ,BETEXT                                                 
156741                  ,IDFINDOC                                               
156742                  ,IDARTNR_FINANCE                                        
156743                  ,BEART                                                  
156744                  ,IDPARTNR                                               
156745                  ,IDEXCUST_1                                             
156746                  ,IDEXCUST_2                                             
156747                  ,IDREF                                                  
156748                  ,KDFINDOC                                               
156749                  ,FLSOFT                                                 
156750                  ,FLFREE                                                 
156751                  ,REARTRAB                                               
156752                  ,PRARTNTO                                               
156753                  ,SUNTO                                                  
156754                  ,KDVALISO                                               
156755                  ,PRARTNTO_SEK                                           
156756                  ,SUNTO_SEK                                              
156757                                                                          
156758           FROM    T01SDEV                                                
156759                                                                          
156760           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156761           AND     (FLSOFT =    :WS-FLSOFT                                
156762           OR       FLSOFT LIKE 'N%')                                     
156763           AND     (FLFREE =    :WS-FLFREE                                
156764           OR       FLFREE LIKE 'N%')                                     
156766           AND      KDFINDOC = 'INV'                                      
156767           AND      DAREGDAT >= :WS-DAREGDAT                              
156768           AND      DAREGDAT <= :WS-DAUPPDAT                              
156769           AND    ((BETEXT   = :WS-BETEXT-A)                              
156770           OR      (BETEXT   = :WS-BETEXT-B)                              
156771           OR      (BETEXT   = :WS-BETEXT-C                               
156772           AND      REARTRAB > :WS-REARTRAB)                              
156773           OR      (BETEXT   = :WS-BETEXT-D                               
156774           AND      PRARTNTO_SEK < :WS-PRARTNTO-MIN)                      
156775           OR      (BETEXT   = :WS-BETEXT-E                               
156776           AND      PRARTNTO_SEK > :WS-PRARTNTO-MAX)                      
156777           OR      (BETEXT   = :WS-BETEXT-F                               
156778           AND      SUNTO_SEK < :WS-SUNTO-MIN)                            
156779           OR      (BETEXT   = :WS-BETEXT-G                               
156780           AND      SUNTO_SEK > :WS-SUNTO-MAX))                           
156781                                                                          
156782           ORDER BY IDLEGSEL                                              
156783     END-EXEC                                                             
156784                                                                          
156785     MOVE 000100  TO GOOD-SQLCODECODES                                    
156786                                                                          
156787     EXEC SQL                                                             
156788        OPEN T01SDEV-4                                                    
156789     END-EXEC                                                             
156790                                                                          
156791     MOVE SQLCODE TO SQLCODE-WS                                           
156792     PERFORM DB2-STATUS-CHECK                                             
156793     .                                                                    
156794                                                                          
156795 DB2-FETCH-T01SDEV-4 SECTION.                                             
156796     MOVE 000100  TO GOOD-SQLCODECODES                                    
156797                                                                          
156798     EXEC SQL                                                             
156799         FETCH T01SDEV-4                                                  
156800                                                                          
156801         INTO :T01SDEV-IDLEGSEL                                           
156802             ,:T01SDEV-DAREGDAT                                           
156803             ,:T01SDEV-BETEXT                                             
156804             ,:T01SDEV-IDFINDOC                                           
156805             ,:T01SDEV-IDARTNR-FINANCE                                    
156806             ,:T01SDEV-BEART                                              
156807             ,:T01SDEV-IDPARTNR                                           
156808             ,:T01SDEV-IDEXCUST-1                                         
156809             ,:T01SDEV-IDEXCUST-2                                         
156810             ,:T01SDEV-IDREF                                              
156811             ,:T01SDEV-KDFINDOC                                           
156812             ,:T01SDEV-FLSOFT                                             
156813             ,:T01SDEV-FLFREE                                             
156814             ,:T01SDEV-REARTRAB                                           
156815             ,:T01SDEV-PRARTNTO                                           
156816             ,:T01SDEV-SUNTO                                              
156817             ,:T01SDEV-KDVALISO                                           
156818             ,:T01SDEV-PRARTNTO-SEK                                       
156819             ,:T01SDEV-SUNTO-SEK                                          
156820     END-EXEC                                                             
156821                                                                          
156822     MOVE SQLCODE TO SQLCODE-WS                                           
156823     PERFORM DB2-STATUS-CHECK                                             
156824     .                                                                    
156825                                                                          
156826 DB2-CLOSE-T01SDEV-4 SECTION.                                             
156827     EXEC SQL                                                             
156828        CLOSE T01SDEV-4                                                   
156829     END-EXEC                                                             
156830     .                                                                    
156831                                                                          
156832 DB2-STATUS-CHECK  SECTION.                                               
156833     SET SQLCODE-IX TO 1                                                  
156834     SEARCH GOOD-SQLCODE                                                  
156835       AT END                                                             
156836          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
156840          DELIMITED BY SIZE INTO ERROR-TEXT                               
156900          CALL ABEND USING RKOD-ABEND-DB2                                 
157000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
157100     END-SEARCH                                                           
157200     .                                                                    
