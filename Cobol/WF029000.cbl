000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF029000.                                                
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   20070514.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ABNORMALSELECTMAINTENACE                         
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT/DELETE ABNORMAL SELECT TABLE (T01PDEV)        
001200*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'I' INSERT                                            
001700*                                                                         
001800*        THE PROGRAM READS   TABLE T01LSEL                                
002200*        THE PROGRAM UPDATES TABLE T01PDEV                                
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: WF0290U                                             
002600*        REQUEST:     WF0290I1                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        RESPONSE:    WF0290O1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'WF029000'.            
004400                                                                          
004500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004600 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800                                                                          
004900*    --- CONSTANT WORK FIELDS                                             
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  WS-ADRESS                   PIC X(50)                                
005300          VALUE 'CARPARTS.BILLIT.ABNORMALSELECTMAINTENACE'.               
005400 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005500 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005600 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005700 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
005800                                                                          
005900 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006000     88  KEYS-OK                             VALUE 'Y'.                   
006100     88  KEYS-WRONG                          VALUE 'N'.                   
006200                                                                          
006300 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006400     88  ACT-CODE-VALID                 VALUE 'S', 'U', 'I'.              
006500     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006600     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006700     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006900                                                                          
007600*    --- WORK-FIELDS                                                      
007800 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
007803 01  WS-KVMANAD-NUM              PIC S9(2) COMP-3.                        
007804                                                                          
007810 01  WS-REARTRAB-RED             PIC Z(1)9.9(3).                          
007820 01  WS-REARTRAB-NUM             PIC S9(2)V9(3) COMP-3.                   
007830 01  WS-REARTRAB-DEC             PIC 9(2)V9(3)  VALUE ZERO.               
007840 01  WS-REARTRAB-HELTAL          PIC 9(2).                                
007900                                                                          
007910 01  WS-REARTRAB-JUST2          PIC 9(2)V9(3)  VALUE ZERO.                
007920 01  WS-REARTRAB-JUST           PIC 9(2)V9(3)  VALUE ZERO.                
007930 01  FILLER REDEFINES WS-REARTRAB-JUST.                                   
007940     03  FILLER                 PIC 9(2)V9(2).                            
007950     03  WS-REARTRAB-SIST       PIC 9(1).                                 
007951                                                                          
007960 01  WS-PRARTNTO-MIN-RED             PIC Z(6)9.9(3).                      
007970 01  WS-PRARTNTO-MIN-NUM             PIC S9(7)V9(3) COMP-3.               
007980 01  WS-PRARTNTO-MIN-DEC             PIC 9(7)V9(3)  VALUE ZERO.           
007990 01  WS-PRARTNTO-MIN-HELTAL          PIC 9(7).                            
007991                                                                          
007992 01  WS-PRARTNTO-MIN-JUST2          PIC 9(7)V9(3)  VALUE ZERO.            
007993 01  WS-PRARTNTO-MIN-JUST          PIC 9(7)V9(3)  VALUE ZERO.             
007994 01  FILLER REDEFINES WS-PRARTNTO-MIN-JUST.                               
007995     03  FILLER                 PIC 9(7)V9(2).                            
007996     03  WS-PRARTNTO-MIN-SIST       PIC 9(1).                             
007997                                                                          
007998 01  WS-PRARTNTO-MAX-RED             PIC Z(6)9.9(3).                      
007999 01  WS-PRARTNTO-MAX-NUM             PIC S9(7)V9(3) COMP-3.               
008000 01  WS-PRARTNTO-MAX-DEC             PIC 9(7)V9(3)  VALUE ZERO.           
008001 01  WS-PRARTNTO-MAX-HELTAL          PIC 9(7).                            
008002                                                                          
008003 01  WS-PRARTNTO-MAX-JUST2           PIC 9(7)V9(3)  VALUE ZERO.           
008004 01  WS-PRARTNTO-MAX-JUST            PIC 9(7)V9(3)  VALUE ZERO.           
008005 01  FILLER REDEFINES WS-PRARTNTO-MAX-JUST.                               
008006     03  FILLER                      PIC 9(7)V9(2).                       
008007     03  WS-PRARTNTO-MAX-SIST        PIC 9(1).                            
008008                                                                          
008009 01  WS-SUNTO-MIN-RED                PIC Z(10)9.9(2).                     
008010 01  WS-SUNTO-MIN-NUM                PIC S9(11)V9(3) COMP-3.              
008011 01  WS-SUNTO-MIN-DEC                PIC 9(11)V9(3)  VALUE ZERO.          
008012 01  WS-SUNTO-MIN-HELTAL             PIC 9(11).                           
008013                                                                          
008014 01  WS-SUNTO-MIN-JUST2              PIC 9(11)V9(3)  VALUE ZERO.          
008015 01  WS-SUNTO-MIN-JUST               PIC 9(11)V9(3)  VALUE ZERO.          
008016 01  FILLER REDEFINES WS-SUNTO-MIN-JUST.                                  
008017     03  FILLER                      PIC 9(11)V9(2).                      
008018     03  WS-SUNTO-MIN-SIST           PIC 9(1).                            
008019                                                                          
008020 01  WS-SUNTO-MAX-RED                PIC Z(10)9.9(2).                     
008021 01  WS-SUNTO-MAX-NUM                PIC S9(11)V9(3) COMP-3.              
008022 01  WS-SUNTO-MAX-DEC                PIC 9(11)V9(3)  VALUE ZERO.          
008023 01  WS-SUNTO-MAX-HELTAL             PIC 9(11).                           
008024                                                                          
008025 01  WS-SUNTO-MAX-JUST2              PIC 9(11)V9(3)  VALUE ZERO.          
008026 01  WS-SUNTO-MAX-JUST               PIC 9(11)V9(3)  VALUE ZERO.          
008027 01  FILLER REDEFINES WS-SUNTO-MAX-JUST.                                  
008028     03  FILLER                      PIC 9(11)V9(2).                      
008029     03  WS-SUNTO-MAX-SIST           PIC 9(1).                            
008030                                                                          
008040*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008400     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
008410     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008500     SKIP3                                                                
008600                                                                          
008700*    --- PARAMETERS TO ABEND                                              
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009200                                                                          
009300 01  MESSAGE-CODES.                                                       
009400     03  ERROR-CODES.                                                     
009500         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
009600         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
009800         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
009900         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
010000         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
010100         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
010200         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
010300         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
010400         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010500         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
010600     03  INFO-CODES.                                                      
010700         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
010800         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
010900         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
011000         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300     SKIP3                                                                
011400 01  -COPY WZ01SUB                                                        
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
011700     SKIP3                                                                
011800 01  -COPY WZ20DATE                                                       
011900     EJECT                                                                
011902 01  FILLER                      PIC X(16)   VALUE 'DECEDIT    '.         
011903     EJECT                                                                
011910 01  -COPY WDECAREA                                                       
012000*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012600     SKIP3                                                                
012700 01  REQU-AREA.                                                           
012800*    03  -COPY WZ01REQU                                                   
012900*    03  -COPY WF0290I1                                                   
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013200     SKIP3                                                                
013300 01  RESP-AREA.                                                           
013400*    03  -COPY WZ01RESP                                                   
013500*    03  -COPY WF0290O1                                                   
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013900                                                                          
014000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
014100 01  DB2-WS.                                                              
014200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
014300         88  CURSOR-OK                       VALUE 000.                   
014400         88  LINES-FOUND                     VALUE 000.                   
014500         88  LINES-MISSING                   VALUE 100.                   
014600         88  NULL-VALUE                      VALUE 305.                   
014700         88  RESOURCE-WRONG                  VALUE 904.                   
014800     03  GOOD-SQLCODECODES.                                               
014900         05  GOOD-SQLCODE OCCURS 5                                        
015000             INDEXED BY SQLCODE-IX PIC 9(3).                              
015100                                                                          
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
015400*01  -COPY T01LSEL -PRE T01LSEL-                                          
015500     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'T01PDEV-AREA'.        
016600*01  -COPY T01PDEV -PRE T01PDEV-                                          
016700     EJECT                                                                
016800     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
016900     EJECT                                                                
017000     EXEC SQL INCLUDE T01PDEV END-EXEC.                                   
017100     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900     EJECT                                                                
018000                                                                          
018100 PROCEDURE DIVISION.                                                      
018200 MAIN SECTION.                                                            
018300                                                                          
018400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018500     IF SUB-KDRC = 0                                                      
018600       PERFORM A-INIT                                                     
018700       PERFORM B-CHECK-KEYS                                               
018800       IF KEYS-OK                                                         
018900         PERFORM F-READ-SHOW-INFO                                         
019000       END-IF                                                             
019100       IF KEYS-WRONG                                                      
019200         PERFORM S04-MOVE-MISSING-TO-RESPOND                              
019300       END-IF                                                             
019400       PERFORM S02-RETURN-RESPONSE                                        
019500     END-IF                                                               
019600                                                                          
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000                                                                          
020100     EJECT                                                                
020200 A-INIT SECTION.                                                          
020300     INITIALIZE GOOD-SQLCODECODES                                         
020400     MOVE ALL '+' TO RESP-AREA                                            
020500     MOVE SPACE TO RESP-IDMSG-ERROR                                       
020600     MOVE SPACE TO RESP-IDMSG-INFO                                        
020700     MOVE SPACE TO RESP-IDELMT-ERROR                                      
020900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
021400 B-CHECK-KEYS SECTION.                                                    
021500     MOVE YES TO KEYS-SW                                                  
021600     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
021700                                                                          
021900     IF  REQU-IDMSGVER NUMERIC                                            
022000       IF REQU-IDLEGSEL-KEY > SPACE                                       
022100       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
022200       AND REQU-KDBEHX-KEY = 'S'                                          
022400       AND ACT-CODE-VALID                                                 
022600         CONTINUE                                                         
022700       ELSE                                                               
022800         MOVE NOO TO KEYS-SW                                              
022900       END-IF                                                             
023000     ELSE                                                                 
023100       MOVE NOO TO KEYS-SW                                                
023200     END-IF                                                               
023300                                                                          
023400     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
023500       MOVE NOO TO KEYS-SW                                                
023600     END-IF                                                               
023700                                                                          
023800     IF KEYS-WRONG                                                        
023900       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
024000       IF REQU-IDMSGVER NUMERIC                                           
024100         CONTINUE                                                         
024200       ELSE                                                               
024300         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
024400         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
024500       END-IF                                                             
024600       IF ACT-CODE-VALID                                                  
024700         CONTINUE                                                         
024800       ELSE                                                               
024900         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
025000         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
025100       END-IF                                                             
025200       IF REQU-IDUSER = SPACE OR = ALL '+'                                
025300         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
025400         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
025500       END-IF                                                             
025600     END-IF                                                               
025700                                                                          
026800     IF KEYS-OK                                                           
026900       PERFORM DB2-SELECT-T01LSEL                                         
027000       IF LINES-FOUND                                                     
027100         CONTINUE                                                         
027200       ELSE                                                               
027300         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
027400         MOVE 'IDLEGSEL'        TO RESP-IDELMT-ERROR                      
027500         MOVE NOO TO KEYS-SW                                              
027600       END-IF                                                             
027700     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
029900 F-READ-SHOW-INFO SECTION.                                                
030000     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
030100     MOVE REQU-KDBEHX-KEY    TO RESP-KDBEHX-KEY                           
030300     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
030400                                                                          
030500     PERFORM FA-READ-BASICDATA                                            
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
031000 FA-READ-BASICDATA SECTION.                                               
031100     IF ACT-CODE-SEARCH                                                   
031200       PERFORM FAA-SEARCH-T01PDEV                                         
031300     ELSE                                                                 
031400       IF ACT-CODE-UPDATE                                                 
031500         PERFORM FAB-UPDATE-T01PDEV                                       
031600       ELSE                                                               
031700         IF ACT-CODE-INSERT                                               
031800           PERFORM FAC-INSERT-T01PDEV                                     
032300         END-IF                                                           
032400       END-IF                                                             
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900*** - SEARCH FOR RIGHT ABNORMAL SELECT AND MARK CURRENT LINE              
033100 FAA-SEARCH-T01PDEV SECTION.                                              
033400     PERFORM DB2-SELECT-T01PDEV                                           
033600                                                                          
033700     IF LINES-FOUND                                                       
034100       PERFORM S03-MOVE-TO-RESPOND                                        
036500     ELSE                                                                 
036600       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
036700       MOVE 'KDBEHX'   TO RESP-IDELMT-ERROR                               
036800       MOVE NOO        TO KEYS-SW                                         
036900     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500*** - CHECK IF UPDATE IS ON CURRENT LINE                                  
037600 FAB-UPDATE-T01PDEV SECTION.                                              
038100     PERFORM FABB-UPD-CURRENT                                             
038600     .                                                                    
038700     EJECT                                                                
038800                                                                          
038900*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON SENDING COUNTRY             
039000 FABA-CHECK-UPDATE-DATA SECTION.                                          
040100     IF RESP-IDMSG-ERROR = SPACE                                          
040200       IF  REQU-FLPAYTE = 'N'                                             
040210       OR  REQU-FLPAYTE = 'Y'                                             
040300          IF REQU-FLPAYTE = 'Y'                                           
040310            MOVE 'J' TO REQU-FLPAYTE                                      
040320          END-IF                                                          
040400       ELSE                                                               
040500         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
040600         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
040700       END-IF                                                             
040800     END-IF                                                               
040900                                                                          
041000     IF RESP-IDMSG-ERROR = SPACE                                          
041100       IF  REQU-FLDELTE = 'N'                                             
041200       OR  REQU-FLDELTE = 'Y'                                             
041300          IF REQU-FLDELTE = 'Y'                                           
041310            MOVE 'J' TO REQU-FLDELTE                                      
041320          END-IF                                                          
041500       ELSE                                                               
041600         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
041700         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
041800       END-IF                                                             
041900     END-IF                                                               
041910                                                                          
041920     IF RESP-IDMSG-ERROR = SPACE                                          
041930       IF  REQU-FLSOFT  = 'N'                                             
041940       OR  REQU-FLSOFT  = 'Y'                                             
041950          IF REQU-FLSOFT = 'Y'                                            
041951            MOVE 'J' TO REQU-FLSOFT                                       
041952          END-IF                                                          
041970       ELSE                                                               
041980         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
041990         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
041991       END-IF                                                             
041992     END-IF                                                               
041993                                                                          
041994     IF RESP-IDMSG-ERROR = SPACE                                          
041995       IF  REQU-FLFREE  = 'N'                                             
041996       OR  REQU-FLFREE  = 'Y'                                             
041998          IF REQU-FLFREE = 'Y'                                            
041999            MOVE 'J' TO REQU-FLFREE                                       
042000          END-IF                                                          
042002       ELSE                                                               
042003         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
042004         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
042005       END-IF                                                             
042006     END-IF                                                               
042007                                                                          
042008     IF RESP-IDMSG-ERROR = SPACE                                          
042009       IF  REQU-FLSERV  = 'N'                                             
042010       OR  REQU-FLSERV  = 'Y'                                             
042012          IF REQU-FLSERV = 'Y'                                            
042013            MOVE 'J' TO REQU-FLSERV                                       
042014          END-IF                                                          
042016       ELSE                                                               
042017         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
042018         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
042019       END-IF                                                             
042020     END-IF                                                               
042021                                                                          
042022     IF RESP-IDMSG-ERROR = SPACE                                          
042023       IF  REQU-FLINVOIC = 'N'                                            
042024       OR  REQU-FLINVOIC = 'Y'                                            
042027          IF REQU-FLINVOIC = 'Y'                                          
042028            MOVE 'J' TO REQU-FLINVOIC                                     
042029          END-IF                                                          
042030       ELSE                                                               
042031         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
042032         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
042033       END-IF                                                             
042034     END-IF                                                               
042035                                                                          
042036     PERFORM S05-CHECK-NUMERIC-VALUES                                     
042040                                                                          
044342     IF RESP-IDMSG-ERROR = SPACE                                          
044343       MOVE REQU-KVMANAD          TO WS-KVMANAD-NUM                       
044344       IF WS-KVMANAD-NUM  NUMERIC                                         
044345         IF WS-KVMANAD-NUM < 10                                           
044346           CONTINUE                                                       
044347         ELSE                                                             
044348           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
044349           MOVE 'KVMANAD'         TO RESP-IDELMT-ERROR                    
044350         END-IF                                                           
044351       ELSE                                                               
044352         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
044353         MOVE 'KVMANAD'           TO RESP-IDELMT-ERROR                    
044354       END-IF                                                             
044355     END-IF                                                               
044356                                                                          
044357     IF RESP-IDMSG-ERROR = SPACE                                          
044358       MOVE WS-CURRENT-DATE       TO T01PDEV-DAUPPDAT                     
044360     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600                                                                          
044700*** - UPDATE CURRENT LINE ON T01PDEV.                                     
044800 FABB-UPD-CURRENT SECTION.                                                
044900     PERFORM DB2-SELECT-T01PDEV                                           
045000                                                                          
045100     IF LINES-FOUND                                                       
045110       PERFORM FABA-CHECK-UPDATE-DATA                                     
045120       IF RESP-IDMSG-ERROR = SPACE                                        
045200         PERFORM DB2-UPDATE-T01PDEV                                       
045300         PERFORM S03-MOVE-TO-RESPOND                                      
045700         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
045800       END-IF                                                             
045900     ELSE                                                                 
046000       MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                            
046100       MOVE 'KDBEHX'       TO RESP-IDELMT-ERROR                           
046200       MOVE NOO TO KEYS-SW                                                
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
049400                                                                          
049500*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
049600*** - IF CURRENT LINE EXIST WITH DELETE DATE, DELETE CURRENT LINE         
049700***   PHYSICAL AND INSERT NEW CURRENT LINE.                               
049800 FAC-INSERT-T01PDEV SECTION.                                              
050000     PERFORM DB2-SELECT-T01PDEV                                           
050100     IF LINES-FOUND                                                       
050200       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
051400     ELSE                                                                 
051500       PERFORM FACA-CHECK-INSERT-DATA                                     
051600       IF RESP-IDMSG-ERROR = SPACE                                        
051700         PERFORM DB2-INSERT-T01PDEV                                       
051800         PERFORM S03-MOVE-TO-RESPOND                                      
051900         MOVE INF-INSERT-OK        TO RESP-IDMSG-INFO                     
052000       END-IF                                                             
052100     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
052800*** - VALIDATE REQUESTED FIELDS FOR INSERT ON SENDING COUNTRY             
052900 FACA-CHECK-INSERT-DATA SECTION.                                          
053000     IF RESP-IDMSG-ERROR = SPACE                                          
053100       IF  REQU-FLPAYTE = 'N'                                             
053200       OR  REQU-FLPAYTE = 'Y'                                             
053410          IF REQU-FLPAYTE = 'Y'                                           
053420            MOVE 'J' TO REQU-FLPAYTE                                      
053430          END-IF                                                          
053500       ELSE                                                               
053600         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
053700         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
053800       END-IF                                                             
053900     END-IF                                                               
054000                                                                          
054100     IF RESP-IDMSG-ERROR = SPACE                                          
054200       IF  REQU-FLDELTE = 'N'                                             
054300       OR  REQU-FLDELTE = 'Y'                                             
054510          IF REQU-FLDELTE = 'Y'                                           
054520            MOVE 'J' TO REQU-FLDELTE                                      
054530          END-IF                                                          
054600       ELSE                                                               
054700         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
054800         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
054900       END-IF                                                             
055000     END-IF                                                               
055100                                                                          
055200     IF RESP-IDMSG-ERROR = SPACE                                          
055300       IF  REQU-FLSOFT  = 'N'                                             
055400       OR  REQU-FLSOFT  = 'Y'                                             
055610          IF REQU-FLSOFT = 'Y'                                            
055620            MOVE 'J' TO REQU-FLSOFT                                       
055630          END-IF                                                          
055700       ELSE                                                               
055800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055810         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
055820       END-IF                                                             
055830     END-IF                                                               
055840                                                                          
055850     IF RESP-IDMSG-ERROR = SPACE                                          
055860       IF  REQU-FLFREE  = 'N'                                             
055870       OR  REQU-FLFREE  = 'Y'                                             
055891          IF REQU-FLFREE = 'Y'                                            
055892            MOVE 'J' TO REQU-FLFREE                                       
055893          END-IF                                                          
055894       ELSE                                                               
055895         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055896         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
055897       END-IF                                                             
055898     END-IF                                                               
055899                                                                          
055900     IF RESP-IDMSG-ERROR = SPACE                                          
055901       IF  REQU-FLSERV  = 'N'                                             
055902       OR  REQU-FLSERV  = 'Y'                                             
055905          IF REQU-FLSERV = 'Y'                                            
055906            MOVE 'J' TO REQU-FLSERV                                       
055907          END-IF                                                          
055908       ELSE                                                               
055909         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055910         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
055911       END-IF                                                             
055912     END-IF                                                               
055913                                                                          
055914     IF RESP-IDMSG-ERROR = SPACE                                          
055915       IF  REQU-FLINVOIC = 'N'                                            
055916       OR  REQU-FLINVOIC = 'Y'                                            
055919          IF REQU-FLINVOIC = 'Y'                                          
055920            MOVE 'J' TO REQU-FLINVOIC                                     
055921          END-IF                                                          
055922       ELSE                                                               
055923         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
055924         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
055925       END-IF                                                             
055926     END-IF                                                               
055927                                                                          
055928     PERFORM S05-CHECK-NUMERIC-VALUES                                     
055930                                                                          
055969     IF RESP-IDMSG-ERROR = SPACE                                          
055970       MOVE REQU-KVMANAD          TO WS-KVMANAD-NUM                       
055971       IF WS-KVMANAD-NUM  NUMERIC                                         
055972         IF WS-KVMANAD-NUM < 10                                           
055973           CONTINUE                                                       
055974         ELSE                                                             
055975           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
055976           MOVE 'KVMANAD'         TO RESP-IDELMT-ERROR                    
055977         END-IF                                                           
055978       ELSE                                                               
055979         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
055980         MOVE 'KVMANAD'           TO RESP-IDELMT-ERROR                    
055981       END-IF                                                             
055988     END-IF                                                               
055989                                                                          
055990     IF RESP-IDMSG-ERROR = SPACE                                          
056000       MOVE WS-CURRENT-DATE       TO T01PDEV-DAREGDAT                     
056100       MOVE WS-ACTIVE             TO T01PDEV-DAUPPDAT                     
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
060700*    --- DISPATCHER SECTIONS                                              
060800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
060900     MOVE 'GETARG'                   TO SUB-KDFUNC                        
061000     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
061100     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
061200                                                                          
061300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
061400                                                                          
061500     IF SUB-KDRC > 0                                                      
061600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
061700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
061800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
061900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
062000     END-IF                                                               
062100     .                                                                    
062200                                                                          
062300 S02-RETURN-RESPONSE SECTION.                                             
062400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
062500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
062600                                                                          
062700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
062800                                                                          
062900     IF SUB-KDRC > 0                                                      
063000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
063100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
063200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
063300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
063400     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700                                                                          
063800*    --- MOVE TO OUTPUT SECTIONS                                          
063900 S03-MOVE-TO-RESPOND SECTION.                                             
064000     IF ACT-CODE-SEARCH                                                   
064010       IF T01PDEV-FLPAYTE = 'J'                                           
064100         MOVE 'Y'                 TO RESP-FLPAYTE                         
064101       ELSE                                                               
064110         MOVE T01PDEV-FLPAYTE     TO RESP-FLPAYTE                         
064120       END-IF                                                             
064130       IF T01PDEV-FLDELTE = 'J'                                           
064200         MOVE 'Y'                 TO RESP-FLDELTE                         
064201       ELSE                                                               
064210         MOVE T01PDEV-FLDELTE     TO RESP-FLDELTE                         
064220       END-IF                                                             
064300       MOVE T01PDEV-REARTRAB      TO RESP-REARTRAB                        
064400       MOVE T01PDEV-PRARTNTO-MIN  TO RESP-PRARTNTO-MIN                    
064410       MOVE T01PDEV-PRARTNTO-MAX  TO RESP-PRARTNTO-MAX                    
064500       MOVE T01PDEV-SUNTO-MIN     TO RESP-SUNTO-MIN                       
064510       MOVE T01PDEV-SUNTO-MAX     TO RESP-SUNTO-MAX                       
064520       IF T01PDEV-FLSOFT  = 'J'                                           
064600         MOVE 'Y'                 TO RESP-FLSOFT                          
064601       ELSE                                                               
064602         MOVE T01PDEV-FLSOFT      TO RESP-FLSOFT                          
064603       END-IF                                                             
064604       IF T01PDEV-FLFREE  = 'J'                                           
064610         MOVE 'Y'                 TO RESP-FLFREE                          
064611       ELSE                                                               
064612         MOVE T01PDEV-FLFREE      TO RESP-FLFREE                          
064613       END-IF                                                             
064614       IF T01PDEV-FLSERV  = 'J'                                           
064620         MOVE 'Y'                 TO RESP-FLSERV                          
064621       ELSE                                                               
064622         MOVE T01PDEV-FLSERV      TO RESP-FLSERV                          
064623       END-IF                                                             
064624       IF T01PDEV-FLINVOIC = 'J'                                          
064630         MOVE 'Y'                 TO RESP-FLINVOIC                        
064631       ELSE                                                               
064640         MOVE T01PDEV-FLINVOIC    TO RESP-FLINVOIC                        
064650       END-IF                                                             
064700       MOVE T01PDEV-KVMANAD       TO RESP-KVMANAD                         
064800       MOVE T01PDEV-DAREGDAT      TO RESP-DAREGDAT                        
064900       MOVE T01PDEV-DAUPPDAT      TO RESP-DAUPPDAT                        
065100       MOVE T01PDEV-IDUSER        TO RESP-IDUSER                          
065300     ELSE                                                                 
065400       IF ACT-CODE-UPDATE                                                 
065500         IF REQU-FLPAYTE = 'J'                                            
065600           MOVE 'Y'               TO RESP-FLPAYTE                         
065601         ELSE                                                             
065610           MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                         
065620         END-IF                                                           
065630         IF REQU-FLDELTE = 'J'                                            
065700           MOVE 'Y'               TO RESP-FLDELTE                         
065701         ELSE                                                             
065710           MOVE REQU-FLDELTE      TO RESP-FLDELTE                         
065720         END-IF                                                           
065800         MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                        
065900         MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                    
066000         MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                    
066100         MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                       
066110         MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                       
066111         IF REQU-FLSOFT  = 'J'                                            
066120           MOVE 'Y'               TO RESP-FLSOFT                          
066121         ELSE                                                             
066122           MOVE REQU-FLSOFT       TO RESP-FLSOFT                          
066123         END-IF                                                           
066124         IF REQU-FLFREE  = 'J'                                            
066130           MOVE 'Y'               TO RESP-FLFREE                          
066131         ELSE                                                             
066132           MOVE REQU-FLFREE       TO RESP-FLFREE                          
066133         END-IF                                                           
066134         IF REQU-FLSERV  = 'J'                                            
066140           MOVE 'Y'               TO RESP-FLSERV                          
066141         ELSE                                                             
066142           MOVE REQU-FLSERV       TO RESP-FLSERV                          
066143         END-IF                                                           
066144         IF REQU-FLINVOIC = 'J'                                           
066150           MOVE 'Y'               TO RESP-FLINVOIC                        
066151         ELSE                                                             
066152           MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                        
066153         END-IF                                                           
066160         MOVE WS-KVMANAD-NUM      TO RESP-KVMANAD                         
066200         MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                        
066300         MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                        
066500         MOVE REQU-IDUSER         TO RESP-IDUSER                          
067300       ELSE                                                               
067400         IF ACT-CODE-INSERT                                               
067500           IF REQU-FLPAYTE  = 'J'                                         
067600             MOVE 'Y'               TO RESP-FLPAYTE                       
067601           ELSE                                                           
067610             MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                       
067620           END-IF                                                         
067630           IF REQU-FLDELTE  = 'J'                                         
067700             MOVE 'Y'               TO RESP-FLDELTE                       
067701           ELSE                                                           
067710             MOVE REQU-FLDELTE      TO RESP-FLDELTE                       
067720           END-IF                                                         
067800           MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                      
067900           MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                  
068000           MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                  
068100           MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                     
068110           MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                     
068111           IF REQU-FLSOFT   = 'J'                                         
068120             MOVE 'Y'               TO RESP-FLSOFT                        
068121           ELSE                                                           
068122             MOVE REQU-FLSOFT       TO RESP-FLSOFT                        
068123           END-IF                                                         
068124           IF REQU-FLFREE   = 'J'                                         
068130             MOVE 'Y'               TO RESP-FLFREE                        
068131           ELSE                                                           
068132             MOVE REQU-FLFREE       TO RESP-FLFREE                        
068133           END-IF                                                         
068134           IF REQU-FLSERV   = 'J'                                         
068140             MOVE 'Y'               TO RESP-FLSERV                        
068141           ELSE                                                           
068142             MOVE REQU-FLSERV       TO RESP-FLSERV                        
068143           END-IF                                                         
068144           IF REQU-FLINVOIC = 'J'                                         
068150             MOVE 'Y'               TO RESP-FLINVOIC                      
068151           ELSE                                                           
068152             MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                      
068153           END-IF                                                         
068160           MOVE WS-KVMANAD-NUM      TO RESP-KVMANAD                       
068200           MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                      
068300           MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                      
068500           MOVE REQU-IDUSER         TO RESP-IDUSER                        
070300         END-IF                                                           
070400       END-IF                                                             
070500     END-IF                                                               
070600     .                                                                    
070700                                                                          
070830 S04-MOVE-MISSING-TO-RESPOND SECTION.                                     
070900     MOVE SPACE             TO RESP-FLPAYTE                               
071000                               RESP-FLDELTE                               
071100                               RESP-FLSOFT                                
071200                               RESP-FLFREE                                
071300                               RESP-FLSERV                                
071400                               RESP-FLINVOIC                              
071700                               RESP-IDUSER                                
071800     MOVE ZERO              TO RESP-DAREGDAT                              
071900                               RESP-DAUPPDAT                              
072000                               RESP-KVMANAD                               
072010                               RESP-REARTRAB                              
072020                               RESP-PRARTNTO-MIN                          
072030                               RESP-PRARTNTO-MAX                          
072040                               RESP-SUNTO-MIN                             
072050                               RESP-SUNTO-MAX                             
072100     .                                                                    
072200     EJECT                                                                
072300                                                                          
072310 S05-CHECK-NUMERIC-VALUES    SECTION.                                     
072311     IF RESP-IDMSG-ERROR = SPACE                                          
072312       IF REQU-REARTRAB(1:1) = '.'                                        
072313         IF REQU-REARTRAB(4:1) NUMERIC                                    
072314           MOVE REQU-REARTRAB(4:1) TO WS-REARTRAB-SIST                    
072315           MOVE SPACE              TO REQU-REARTRAB(4:1)                  
072316         END-IF                                                           
072317       END-IF                                                             
072318       IF REQU-REARTRAB(2:1) = '.'                                        
072319         IF REQU-REARTRAB(5:1) NUMERIC                                    
072320           MOVE REQU-REARTRAB(5:1) TO WS-REARTRAB-SIST                    
072321           MOVE SPACE              TO REQU-REARTRAB(5:1)                  
072322         END-IF                                                           
072323       END-IF                                                             
072324       MOVE REQU-REARTRAB          TO DEC-IDFRIDATA                       
072325       MOVE 2                      TO DEC-KVHELTAL                        
072326       MOVE 2                      TO DEC-KVDECIMAL                       
072327                                                                          
072328       CALL WDECEDIT USING DEC-WDECAREA                                   
072329                                                                          
072330       IF DEC-KDSVAR-OK                                                   
072331         MOVE DEC-IDEDITDATA       TO WS-REARTRAB-DEC                     
072332         IF WS-REARTRAB-DEC NUMERIC                                       
072333           MOVE WS-REARTRAB-DEC    TO WS-REARTRAB-HELTAL                  
072334           IF WS-REARTRAB-HELTAL > 99                                     
072335             MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
072336             MOVE 'REARTRAB'       TO RESP-IDELMT-ERROR                   
072337           ELSE                                                           
072338             MOVE WS-REARTRAB-DEC  TO WS-REARTRAB-NUM                     
072339             IF REQU-REARTRAB(1:1) = '.'                                  
072340               IF WS-REARTRAB-SIST > ZERO                                 
072341                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
072342                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
072343                                           WS-REARTRAB-JUST2              
072344               END-IF                                                     
072345             END-IF                                                       
072346             IF REQU-REARTRAB(2:1) = '.'                                  
072347               IF WS-REARTRAB-SIST > ZERO                                 
072348                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
072349                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
072350                                           WS-REARTRAB-JUST2              
072351               END-IF                                                     
072352             END-IF                                                       
072353             IF REQU-REARTRAB(3:1) = '.'                                  
072354               IF WS-REARTRAB-SIST > ZERO                                 
072355                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
072356                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
072357                                           WS-REARTRAB-JUST2              
072358               END-IF                                                     
072359             END-IF                                                       
072360             IF REQU-REARTRAB(4:1) = '.'                                  
072361               IF WS-REARTRAB-SIST > ZERO                                 
072362                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
072363                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
072364                                           WS-REARTRAB-JUST2              
072365               END-IF                                                     
072366             END-IF                                                       
072367           END-IF                                                         
072368         ELSE                                                             
072369           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
072370           MOVE 'REARTRAB'          TO RESP-IDELMT-ERROR                  
072371         END-IF                                                           
072372       ELSE                                                               
072373         MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                   
072374         MOVE 'REARTRAB'            TO RESP-IDELMT-ERROR                  
072375       END-IF                                                             
072376     END-IF                                                               
072377                                                                          
072378     IF RESP-IDMSG-ERROR = SPACE                                          
072379       IF REQU-PRARTNTO-MIN(1:1) = '.'                                    
072380         IF REQU-PRARTNTO-MIN(4:1) NUMERIC                                
072381           MOVE REQU-PRARTNTO-MIN(4:1) TO WS-PRARTNTO-MIN-SIST            
072382           MOVE SPACE                  TO REQU-PRARTNTO-MIN(4:1)          
072383         END-IF                                                           
072384       END-IF                                                             
072385       IF REQU-PRARTNTO-MIN(2:1) = '.'                                    
072386         IF REQU-PRARTNTO-MIN(5:1) NUMERIC                                
072387           MOVE REQU-PRARTNTO-MIN(5:1) TO WS-PRARTNTO-MIN-SIST            
072388           MOVE SPACE                  TO REQU-PRARTNTO-MIN(5:1)          
072389         END-IF                                                           
072390       END-IF                                                             
072391       IF REQU-PRARTNTO-MIN(3:1) = '.'                                    
072392         IF REQU-PRARTNTO-MIN(6:1) NUMERIC                                
072393           MOVE REQU-PRARTNTO-MIN(6:1) TO WS-PRARTNTO-MIN-SIST            
072394           MOVE SPACE                  TO REQU-PRARTNTO-MIN(6:1)          
072395         END-IF                                                           
072396       END-IF                                                             
072397       IF REQU-PRARTNTO-MIN(4:1) = '.'                                    
072398         IF REQU-PRARTNTO-MIN(7:1) NUMERIC                                
072399           MOVE REQU-PRARTNTO-MIN(7:1) TO WS-PRARTNTO-MIN-SIST            
072400           MOVE SPACE                  TO REQU-PRARTNTO-MIN(7:1)          
072401         END-IF                                                           
072402       END-IF                                                             
072403       IF REQU-PRARTNTO-MIN(5:1) = '.'                                    
072404         IF REQU-PRARTNTO-MIN(8:1) NUMERIC                                
072405           MOVE REQU-PRARTNTO-MIN(8:1) TO WS-PRARTNTO-MIN-SIST            
072406           MOVE SPACE                  TO REQU-PRARTNTO-MIN(8:1)          
072407         END-IF                                                           
072408       END-IF                                                             
072409       IF REQU-PRARTNTO-MIN(6:1) = '.'                                    
072410         IF REQU-PRARTNTO-MIN(9:1) NUMERIC                                
072411           MOVE REQU-PRARTNTO-MIN(9:1) TO WS-PRARTNTO-MIN-SIST            
072412           MOVE SPACE                  TO REQU-PRARTNTO-MIN(9:1)          
072413         END-IF                                                           
072414       END-IF                                                             
072415       IF REQU-PRARTNTO-MIN(7:1) = '.'                                    
072416         IF REQU-PRARTNTO-MIN(10:1) NUMERIC                               
072417           MOVE REQU-PRARTNTO-MIN(10:1) TO WS-PRARTNTO-MIN-SIST           
072418           MOVE SPACE                   TO REQU-PRARTNTO-MIN(10:1)        
072419         END-IF                                                           
072420       END-IF                                                             
072427       MOVE REQU-PRARTNTO-MIN           TO DEC-IDFRIDATA                  
072428       MOVE 7                           TO DEC-KVHELTAL                   
072429       MOVE 2                           TO DEC-KVDECIMAL                  
072430                                                                          
072431       CALL WDECEDIT USING DEC-WDECAREA                                   
072432                                                                          
072433       IF DEC-KDSVAR-OK                                                   
072434         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MIN-DEC            
072435         IF WS-PRARTNTO-MIN-DEC NUMERIC                                   
072436           MOVE WS-PRARTNTO-MIN-DEC     TO WS-PRARTNTO-MIN-HELTAL         
072437           IF WS-PRARTNTO-MIN-HELTAL > 9999999                            
072438             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
072439             MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR              
072440           ELSE                                                           
072441             MOVE WS-PRARTNTO-MIN-DEC   TO WS-PRARTNTO-MIN-NUM            
072442             IF REQU-PRARTNTO-MIN(1:1) = '.'                              
072443              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072444               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072445               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072446                                             WS-PRARTNTO-MIN-JUST2        
072447              END-IF                                                      
072448             END-IF                                                       
072449             IF REQU-PRARTNTO-MIN(2:1) = '.'                              
072450              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072460               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072461               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072462                                             WS-PRARTNTO-MIN-JUST2        
072463              END-IF                                                      
072464             END-IF                                                       
072465             IF REQU-PRARTNTO-MIN(3:1) = '.'                              
072466              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072467               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072468               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072469                                             WS-PRARTNTO-MIN-JUST2        
072470              END-IF                                                      
072471             END-IF                                                       
072472             IF REQU-PRARTNTO-MIN(4:1) = '.'                              
072473              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072474               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072475               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072476                                             WS-PRARTNTO-MIN-JUST2        
072477              END-IF                                                      
072478             END-IF                                                       
072479             IF REQU-PRARTNTO-MIN(5:1) = '.'                              
072480              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072481               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072482               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072483                                             WS-PRARTNTO-MIN-JUST2        
072484              END-IF                                                      
072485             END-IF                                                       
072486             IF REQU-PRARTNTO-MIN(6:1) = '.'                              
072487              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072488               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072489               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072490                                             WS-PRARTNTO-MIN-JUST2        
072491              END-IF                                                      
072492             END-IF                                                       
072493             IF REQU-PRARTNTO-MIN(7:1) = '.'                              
072494              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072495               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072496               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072497                                             WS-PRARTNTO-MIN-JUST2        
072498              END-IF                                                      
072499             END-IF                                                       
072500             IF REQU-PRARTNTO-MIN(8:1) = '.'                              
072501              IF WS-PRARTNTO-MIN-SIST > ZERO                              
072502               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
072503               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
072504                                             WS-PRARTNTO-MIN-JUST2        
072505              END-IF                                                      
072506             END-IF                                                       
072507           END-IF                                                         
072508         ELSE                                                             
072509           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
072510           MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR                
072511         END-IF                                                           
072512       ELSE                                                               
072513         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
072514         MOVE 'PRARTNTO-MIN'          TO RESP-IDELMT-ERROR                
072515       END-IF                                                             
072516     END-IF                                                               
072517                                                                          
072518     IF RESP-IDMSG-ERROR = SPACE                                          
072519       IF REQU-PRARTNTO-MAX(1:1) = '.'                                    
072520         IF REQU-PRARTNTO-MAX(4:1) NUMERIC                                
072521           MOVE REQU-PRARTNTO-MAX(4:1) TO WS-PRARTNTO-MAX-SIST            
072522           MOVE SPACE                  TO REQU-PRARTNTO-MAX(4:1)          
072523         END-IF                                                           
072524       END-IF                                                             
072525       IF REQU-PRARTNTO-MAX(2:1) = '.'                                    
072526         IF REQU-PRARTNTO-MAX(5:1) NUMERIC                                
072527           MOVE REQU-PRARTNTO-MAX(5:1) TO WS-PRARTNTO-MAX-SIST            
072528           MOVE SPACE                  TO REQU-PRARTNTO-MAX(5:1)          
072529         END-IF                                                           
072530       END-IF                                                             
072531       IF REQU-PRARTNTO-MAX(3:1) = '.'                                    
072532         IF REQU-PRARTNTO-MAX(6:1) NUMERIC                                
072533           MOVE REQU-PRARTNTO-MAX(6:1) TO WS-PRARTNTO-MAX-SIST            
072534           MOVE SPACE                  TO REQU-PRARTNTO-MAX(6:1)          
072535         END-IF                                                           
072536       END-IF                                                             
072537       IF REQU-PRARTNTO-MAX(4:1) = '.'                                    
072538         IF REQU-PRARTNTO-MAX(7:1) NUMERIC                                
072539           MOVE REQU-PRARTNTO-MAX(7:1) TO WS-PRARTNTO-MAX-SIST            
072540           MOVE SPACE                  TO REQU-PRARTNTO-MAX(7:1)          
072541         END-IF                                                           
072542       END-IF                                                             
072543       IF REQU-PRARTNTO-MAX(5:1) = '.'                                    
072544         IF REQU-PRARTNTO-MAX(8:1) NUMERIC                                
072545           MOVE REQU-PRARTNTO-MAX(8:1) TO WS-PRARTNTO-MAX-SIST            
072546           MOVE SPACE                  TO REQU-PRARTNTO-MAX(8:1)          
072547         END-IF                                                           
072548       END-IF                                                             
072549       IF REQU-PRARTNTO-MAX(6:1) = '.'                                    
072550         IF REQU-PRARTNTO-MAX(9:1) NUMERIC                                
072551           MOVE REQU-PRARTNTO-MAX(9:1) TO WS-PRARTNTO-MAX-SIST            
072552           MOVE SPACE                  TO REQU-PRARTNTO-MAX(9:1)          
072553         END-IF                                                           
072554       END-IF                                                             
072555       IF REQU-PRARTNTO-MAX(7:1) = '.'                                    
072556         IF REQU-PRARTNTO-MAX(10:1) NUMERIC                               
072557           MOVE REQU-PRARTNTO-MAX(10:1) TO WS-PRARTNTO-MAX-SIST           
072558           MOVE SPACE                   TO REQU-PRARTNTO-MAX(10:1)        
072559         END-IF                                                           
072560       END-IF                                                             
072567       MOVE REQU-PRARTNTO-MAX           TO DEC-IDFRIDATA                  
072568       MOVE 7                           TO DEC-KVHELTAL                   
072569       MOVE 2                           TO DEC-KVDECIMAL                  
072570                                                                          
072571       CALL WDECEDIT USING DEC-WDECAREA                                   
072572                                                                          
072573       IF DEC-KDSVAR-OK                                                   
072574         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MAX-DEC            
072575         IF WS-PRARTNTO-MAX-DEC NUMERIC                                   
072576           MOVE WS-PRARTNTO-MAX-DEC     TO WS-PRARTNTO-MAX-HELTAL         
072577           IF WS-PRARTNTO-MAX-HELTAL > 9999999                            
072578             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
072579             MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR              
072580           ELSE                                                           
072581             MOVE WS-PRARTNTO-MAX-DEC   TO WS-PRARTNTO-MAX-NUM            
072582             IF REQU-PRARTNTO-MAX(1:1) = '.'                              
072583              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072584               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072585               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072586                                             WS-PRARTNTO-MAX-JUST2        
072587              END-IF                                                      
072588             END-IF                                                       
072589             IF REQU-PRARTNTO-MAX(2:1) = '.'                              
072590              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072591               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072592               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072593                                             WS-PRARTNTO-MAX-JUST2        
072594              END-IF                                                      
072595             END-IF                                                       
072596             IF REQU-PRARTNTO-MAX(3:1) = '.'                              
072597              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072598               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072599               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072600                                             WS-PRARTNTO-MAX-JUST2        
072601              END-IF                                                      
072602             END-IF                                                       
072603             IF REQU-PRARTNTO-MAX(4:1) = '.'                              
072604              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072605               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072606               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072607                                             WS-PRARTNTO-MAX-JUST2        
072608              END-IF                                                      
072609             END-IF                                                       
072610             IF REQU-PRARTNTO-MAX(5:1) = '.'                              
072611              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072612               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072613               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072614                                             WS-PRARTNTO-MAX-JUST2        
072615              END-IF                                                      
072616             END-IF                                                       
072617             IF REQU-PRARTNTO-MAX(6:1) = '.'                              
072618              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072619               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072620               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072621                                             WS-PRARTNTO-MAX-JUST2        
072622              END-IF                                                      
072623             END-IF                                                       
072624             IF REQU-PRARTNTO-MAX(7:1) = '.'                              
072625              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072626               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072627               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072628                                             WS-PRARTNTO-MAX-JUST2        
072629              END-IF                                                      
072630             END-IF                                                       
072631             IF REQU-PRARTNTO-MAX(8:1) = '.'                              
072632              IF WS-PRARTNTO-MAX-SIST > ZERO                              
072633               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
072634               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
072635                                             WS-PRARTNTO-MAX-JUST2        
072636              END-IF                                                      
072637             END-IF                                                       
072638           END-IF                                                         
072639         ELSE                                                             
072640           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
072641           MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR                
072642         END-IF                                                           
072643       ELSE                                                               
072644         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
072645         MOVE 'PRARTNTO-MAX'          TO RESP-IDELMT-ERROR                
072646       END-IF                                                             
072647     END-IF                                                               
072648                                                                          
072649     IF RESP-IDMSG-ERROR = SPACE                                          
072650       IF REQU-SUNTO-MIN(1:1) = '.'                                       
072651         IF REQU-SUNTO-MIN(4:1) NUMERIC                                   
072652           MOVE REQU-SUNTO-MIN(4:1)    TO WS-SUNTO-MIN-SIST               
072653           MOVE SPACE                  TO REQU-SUNTO-MIN(4:1)             
072654         END-IF                                                           
072655       END-IF                                                             
072656       IF REQU-SUNTO-MIN(2:1) = '.'                                       
072657         IF REQU-SUNTO-MIN(5:1) NUMERIC                                   
072658           MOVE REQU-SUNTO-MIN(5:1)    TO WS-SUNTO-MIN-SIST               
072659           MOVE SPACE                  TO REQU-SUNTO-MIN(5:1)             
072660         END-IF                                                           
072661       END-IF                                                             
072662       IF REQU-SUNTO-MIN(3:1) = '.'                                       
072663         IF REQU-SUNTO-MIN(6:1) NUMERIC                                   
072664           MOVE REQU-SUNTO-MIN(6:1)    TO WS-SUNTO-MIN-SIST               
072665           MOVE SPACE                  TO REQU-SUNTO-MIN(6:1)             
072666         END-IF                                                           
072667       END-IF                                                             
072668       IF REQU-SUNTO-MIN(4:1) = '.'                                       
072669         IF REQU-SUNTO-MIN(7:1) NUMERIC                                   
072670           MOVE REQU-SUNTO-MIN(7:1)    TO WS-SUNTO-MIN-SIST               
072671           MOVE SPACE                  TO REQU-SUNTO-MIN(7:1)             
072672         END-IF                                                           
072673       END-IF                                                             
072674       IF REQU-SUNTO-MIN(5:1) = '.'                                       
072675         IF REQU-SUNTO-MIN(8:1) NUMERIC                                   
072676           MOVE REQU-SUNTO-MIN(8:1)    TO WS-SUNTO-MIN-SIST               
072677           MOVE SPACE                  TO REQU-SUNTO-MIN(8:1)             
072678         END-IF                                                           
072679       END-IF                                                             
072680       IF REQU-SUNTO-MIN(6:1) = '.'                                       
072681         IF REQU-SUNTO-MIN(9:1) NUMERIC                                   
072682           MOVE REQU-SUNTO-MIN(9:1)    TO WS-SUNTO-MIN-SIST               
072683           MOVE SPACE                  TO REQU-SUNTO-MIN(9:1)             
072684         END-IF                                                           
072685       END-IF                                                             
072686       IF REQU-SUNTO-MIN(7:1) = '.'                                       
072687         IF REQU-SUNTO-MIN(10:1) NUMERIC                                  
072688           MOVE REQU-SUNTO-MIN(10:1)    TO WS-SUNTO-MIN-SIST              
072689           MOVE SPACE                   TO REQU-SUNTO-MIN(10:1)           
072690         END-IF                                                           
072691       END-IF                                                             
072692       IF REQU-SUNTO-MIN(8:1) = '.'                                       
072693         IF REQU-SUNTO-MIN(11:1) NUMERIC                                  
072694           MOVE REQU-SUNTO-MIN(11:1)    TO WS-SUNTO-MIN-SIST              
072695           MOVE SPACE                   TO REQU-SUNTO-MIN(11:1)           
072696         END-IF                                                           
072697       END-IF                                                             
072698       IF REQU-SUNTO-MIN(9:1) = '.'                                       
072699         IF REQU-SUNTO-MIN(12:1) NUMERIC                                  
072700           MOVE REQU-SUNTO-MIN(12:1)    TO WS-SUNTO-MIN-SIST              
072701           MOVE SPACE                   TO REQU-SUNTO-MIN(12:1)           
072702         END-IF                                                           
072703       END-IF                                                             
072704       IF REQU-SUNTO-MIN(10:1) = '.'                                      
072705         IF REQU-SUNTO-MIN(13:1) NUMERIC                                  
072706           MOVE REQU-SUNTO-MIN(13:1)    TO WS-SUNTO-MIN-SIST              
072707           MOVE SPACE                   TO REQU-SUNTO-MIN(13:1)           
072708         END-IF                                                           
072709       END-IF                                                             
072710       IF REQU-SUNTO-MIN(11:1) = '.'                                      
072711         IF REQU-SUNTO-MIN(14:1) NUMERIC                                  
072712           MOVE REQU-SUNTO-MIN(14:1)    TO WS-SUNTO-MIN-SIST              
072713           MOVE SPACE                   TO REQU-SUNTO-MIN(14:1)           
072714         END-IF                                                           
072715       END-IF                                                             
072722       MOVE REQU-SUNTO-MIN              TO DEC-IDFRIDATA                  
072723       MOVE 11                          TO DEC-KVHELTAL                   
072724       MOVE 2                           TO DEC-KVDECIMAL                  
072725                                                                          
072726       CALL WDECEDIT USING DEC-WDECAREA                                   
072727                                                                          
072728       IF DEC-KDSVAR-OK                                                   
072729         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MIN-DEC               
072730         IF WS-SUNTO-MIN-DEC NUMERIC                                      
072731           MOVE WS-SUNTO-MIN-DEC        TO WS-SUNTO-MIN-HELTAL            
072732           IF WS-SUNTO-MIN-HELTAL > 99999999999                           
072733             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
072734             MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR              
072735           ELSE                                                           
072736             MOVE WS-SUNTO-MIN-DEC      TO WS-SUNTO-MIN-NUM               
072737             IF REQU-SUNTO-MIN(1:1) = '.'                                 
072738              IF WS-SUNTO-MIN-SIST > ZERO                                 
072739               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072740               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072741                                           WS-SUNTO-MIN-JUST2             
072742              END-IF                                                      
072743             END-IF                                                       
072744             IF REQU-SUNTO-MIN(2:1) = '.'                                 
072745              IF WS-SUNTO-MIN-SIST > ZERO                                 
072746               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072747               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072748                                           WS-SUNTO-MIN-JUST2             
072749              END-IF                                                      
072750             END-IF                                                       
072760             IF REQU-SUNTO-MIN(3:1) = '.'                                 
072770              IF WS-SUNTO-MIN-SIST > ZERO                                 
072780               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072790               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072791                                           WS-SUNTO-MIN-JUST2             
072792              END-IF                                                      
072793             END-IF                                                       
072794             IF REQU-SUNTO-MIN(4:1) = '.'                                 
072795              IF WS-SUNTO-MIN-SIST > ZERO                                 
072796               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072797               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072798                                           WS-SUNTO-MIN-JUST2             
072799              END-IF                                                      
072800             END-IF                                                       
072801             IF REQU-SUNTO-MIN(5:1) = '.'                                 
072802              IF WS-SUNTO-MIN-SIST > ZERO                                 
072803               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072804               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072805                                           WS-SUNTO-MIN-JUST2             
072806              END-IF                                                      
072807             END-IF                                                       
072808             IF REQU-SUNTO-MIN(6:1) = '.'                                 
072809              IF WS-SUNTO-MIN-SIST > ZERO                                 
072810               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072811               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072812                                           WS-SUNTO-MIN-JUST2             
072813              END-IF                                                      
072814             END-IF                                                       
072815             IF REQU-SUNTO-MIN(7:1) = '.'                                 
072816              IF WS-SUNTO-MIN-SIST > ZERO                                 
072817               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072818               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072819                                           WS-SUNTO-MIN-JUST2             
072820              END-IF                                                      
072821             END-IF                                                       
072822             IF REQU-SUNTO-MIN(8:1) = '.'                                 
072823              IF WS-SUNTO-MIN-SIST > ZERO                                 
072824               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072825               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072826                                           WS-SUNTO-MIN-JUST2             
072827              END-IF                                                      
072828             END-IF                                                       
072829             IF REQU-SUNTO-MIN(9:1) = '.'                                 
072830              IF WS-SUNTO-MIN-SIST > ZERO                                 
072831               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072832               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072833                                           WS-SUNTO-MIN-JUST2             
072834              END-IF                                                      
072835             END-IF                                                       
072836             IF REQU-SUNTO-MIN(10:1) = '.'                                
072837              IF WS-SUNTO-MIN-SIST > ZERO                                 
072838               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072839               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072840                                           WS-SUNTO-MIN-JUST2             
072841              END-IF                                                      
072842             END-IF                                                       
072843             IF REQU-SUNTO-MIN(11:1) = '.'                                
072844              IF WS-SUNTO-MIN-SIST > ZERO                                 
072845               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072846               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072847                                           WS-SUNTO-MIN-JUST2             
072848              END-IF                                                      
072849             END-IF                                                       
072850             IF REQU-SUNTO-MIN(12:1) = '.'                                
072851              IF WS-SUNTO-MIN-SIST > ZERO                                 
072852               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
072853               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
072854                                           WS-SUNTO-MIN-JUST2             
072855              END-IF                                                      
072856             END-IF                                                       
072857           END-IF                                                         
072858         ELSE                                                             
072859           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
072860           MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR                
072861         END-IF                                                           
072862       ELSE                                                               
072863         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
072864         MOVE 'SUNTO-MIN'             TO RESP-IDELMT-ERROR                
072865       END-IF                                                             
072866     END-IF                                                               
072867                                                                          
072868     IF RESP-IDMSG-ERROR = SPACE                                          
072869       IF REQU-SUNTO-MAX(1:1) = '.'                                       
072870         IF REQU-SUNTO-MAX(4:1) NUMERIC                                   
072871           MOVE REQU-SUNTO-MAX(4:1)    TO WS-SUNTO-MAX-SIST               
072872           MOVE SPACE                  TO REQU-SUNTO-MAX(4:1)             
072873         END-IF                                                           
072874       END-IF                                                             
072875       IF REQU-SUNTO-MAX(2:1) = '.'                                       
072876         IF REQU-SUNTO-MAX(5:1) NUMERIC                                   
072877           MOVE REQU-SUNTO-MAX(5:1)    TO WS-SUNTO-MAX-SIST               
072878           MOVE SPACE                  TO REQU-SUNTO-MAX(5:1)             
072879         END-IF                                                           
072880       END-IF                                                             
072881       IF REQU-SUNTO-MAX(3:1) = '.'                                       
072882         IF REQU-SUNTO-MAX(6:1) NUMERIC                                   
072883           MOVE REQU-SUNTO-MAX(6:1)    TO WS-SUNTO-MAX-SIST               
072884           MOVE SPACE                  TO REQU-SUNTO-MAX(6:1)             
072885         END-IF                                                           
072886       END-IF                                                             
072887       IF REQU-SUNTO-MAX(4:1) = '.'                                       
072888         IF REQU-SUNTO-MAX(7:1) NUMERIC                                   
072889           MOVE REQU-SUNTO-MAX(7:1)    TO WS-SUNTO-MAX-SIST               
072890           MOVE SPACE                  TO REQU-SUNTO-MAX(7:1)             
072891         END-IF                                                           
072892       END-IF                                                             
072893       IF REQU-SUNTO-MAX(5:1) = '.'                                       
072894         IF REQU-SUNTO-MAX(8:1) NUMERIC                                   
072895           MOVE REQU-SUNTO-MAX(8:1)    TO WS-SUNTO-MAX-SIST               
072896           MOVE SPACE                  TO REQU-SUNTO-MAX(8:1)             
072897         END-IF                                                           
072898       END-IF                                                             
072899       IF REQU-SUNTO-MAX(6:1) = '.'                                       
072900         IF REQU-SUNTO-MAX(9:1) NUMERIC                                   
072901           MOVE REQU-SUNTO-MAX(9:1)    TO WS-SUNTO-MAX-SIST               
072902           MOVE SPACE                  TO REQU-SUNTO-MAX(9:1)             
072903         END-IF                                                           
072904       END-IF                                                             
072905       IF REQU-SUNTO-MAX(7:1) = '.'                                       
072906         IF REQU-SUNTO-MAX(10:1) NUMERIC                                  
072907           MOVE REQU-SUNTO-MAX(10:1)    TO WS-SUNTO-MAX-SIST              
072908           MOVE SPACE                   TO REQU-SUNTO-MAX(10:1)           
072909         END-IF                                                           
072910       END-IF                                                             
072911       IF REQU-SUNTO-MAX(8:1) = '.'                                       
072912         IF REQU-SUNTO-MAX(11:1) NUMERIC                                  
072913           MOVE REQU-SUNTO-MAX(11:1)    TO WS-SUNTO-MAX-SIST              
072914           MOVE SPACE                   TO REQU-SUNTO-MAX(11:1)           
072915         END-IF                                                           
072916       END-IF                                                             
072917       IF REQU-SUNTO-MAX(9:1) = '.'                                       
072918         IF REQU-SUNTO-MAX(12:1) NUMERIC                                  
072919           MOVE REQU-SUNTO-MAX(12:1)    TO WS-SUNTO-MAX-SIST              
072920           MOVE SPACE                   TO REQU-SUNTO-MAX(12:1)           
072921         END-IF                                                           
072922       END-IF                                                             
072923       IF REQU-SUNTO-MAX(10:1) = '.'                                      
072924         IF REQU-SUNTO-MAX(13:1) NUMERIC                                  
072925           MOVE REQU-SUNTO-MAX(13:1)    TO WS-SUNTO-MAX-SIST              
072926           MOVE SPACE                   TO REQU-SUNTO-MAX(13:1)           
072927         END-IF                                                           
072928       END-IF                                                             
072929       IF REQU-SUNTO-MAX(11:1) = '.'                                      
072930         IF REQU-SUNTO-MAX(14:1) NUMERIC                                  
072931           MOVE REQU-SUNTO-MAX(14:1)    TO WS-SUNTO-MAX-SIST              
072932           MOVE SPACE                   TO REQU-SUNTO-MAX(14:1)           
072933         END-IF                                                           
072934       END-IF                                                             
072941       MOVE REQU-SUNTO-MAX              TO DEC-IDFRIDATA                  
072942       MOVE 11                          TO DEC-KVHELTAL                   
072943       MOVE 2                           TO DEC-KVDECIMAL                  
072944                                                                          
072945       CALL WDECEDIT USING DEC-WDECAREA                                   
072946                                                                          
072947       IF DEC-KDSVAR-OK                                                   
072948         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MAX-DEC               
072949         IF WS-SUNTO-MAX-DEC NUMERIC                                      
072950           MOVE WS-SUNTO-MAX-DEC        TO WS-SUNTO-MAX-HELTAL            
072951           IF WS-SUNTO-MAX-HELTAL > 99999999999                           
072952             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
072953             MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR              
072954           ELSE                                                           
072955             MOVE WS-SUNTO-MAX-DEC      TO WS-SUNTO-MAX-NUM               
072956             IF REQU-SUNTO-MAX(1:1) = '.'                                 
072957              IF WS-SUNTO-MAX-SIST > ZERO                                 
072958               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072959               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072960                                           WS-SUNTO-MAX-JUST2             
072961              END-IF                                                      
072962             END-IF                                                       
072963             IF REQU-SUNTO-MAX(2:1) = '.'                                 
072964              IF WS-SUNTO-MAX-SIST > ZERO                                 
072965               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072966               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072967                                           WS-SUNTO-MAX-JUST2             
072968              END-IF                                                      
072969             END-IF                                                       
072970             IF REQU-SUNTO-MAX(3:1) = '.'                                 
072971              IF WS-SUNTO-MAX-SIST > ZERO                                 
072972               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072973               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072974                                           WS-SUNTO-MAX-JUST2             
072975              END-IF                                                      
072976             END-IF                                                       
072977             IF REQU-SUNTO-MAX(4:1) = '.'                                 
072978              IF WS-SUNTO-MAX-SIST > ZERO                                 
072979               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072980               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072981                                           WS-SUNTO-MAX-JUST2             
072982              END-IF                                                      
072983             END-IF                                                       
072984             IF REQU-SUNTO-MAX(5:1) = '.'                                 
072985              IF WS-SUNTO-MAX-SIST > ZERO                                 
072986               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072987               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072988                                           WS-SUNTO-MAX-JUST2             
072989              END-IF                                                      
072990             END-IF                                                       
072991             IF REQU-SUNTO-MAX(6:1) = '.'                                 
072992              IF WS-SUNTO-MAX-SIST > ZERO                                 
072993               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
072994               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
072995                                           WS-SUNTO-MAX-JUST2             
072996              END-IF                                                      
072997             END-IF                                                       
072998             IF REQU-SUNTO-MAX(7:1) = '.'                                 
072999              IF WS-SUNTO-MAX-SIST > ZERO                                 
073000               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073001               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073002                                           WS-SUNTO-MAX-JUST2             
073003              END-IF                                                      
073004             END-IF                                                       
073005             IF REQU-SUNTO-MAX(8:1) = '.'                                 
073006              IF WS-SUNTO-MAX-SIST > ZERO                                 
073007               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073008               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073009                                           WS-SUNTO-MAX-JUST2             
073010              END-IF                                                      
073011             END-IF                                                       
073012             IF REQU-SUNTO-MAX(9:1) = '.'                                 
073013              IF WS-SUNTO-MAX-SIST > ZERO                                 
073014               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073015               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073016                                           WS-SUNTO-MAX-JUST2             
073017              END-IF                                                      
073018             END-IF                                                       
073019             IF REQU-SUNTO-MAX(10:1) = '.'                                
073020              IF WS-SUNTO-MAX-SIST > ZERO                                 
073021               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073022               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073023                                           WS-SUNTO-MAX-JUST2             
073024              END-IF                                                      
073025             END-IF                                                       
073026             IF REQU-SUNTO-MAX(11:1) = '.'                                
073027              IF WS-SUNTO-MAX-SIST > ZERO                                 
073028               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073029               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073030                                           WS-SUNTO-MAX-JUST2             
073031              END-IF                                                      
073032             END-IF                                                       
073033             IF REQU-SUNTO-MAX(12:1) = '.'                                
073034              IF WS-SUNTO-MAX-SIST > ZERO                                 
073035               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
073036               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
073037                                           WS-SUNTO-MAX-JUST2             
073038              END-IF                                                      
073039             END-IF                                                       
073040           END-IF                                                         
073041         ELSE                                                             
073042           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
073043           MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR                
073044         END-IF                                                           
073045       ELSE                                                               
073046         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
073047         MOVE 'SUNTO-MAX'             TO RESP-IDELMT-ERROR                
073048       END-IF                                                             
073049     END-IF                                                               
073050     .                                                                    
073051                                                                          
073052*    --- DB2 SECTIONS                                                     
073053 DB2-SELECT-T01LSEL       SECTION.                                        
073054     MOVE 000100 TO GOOD-SQLCODECODES                                     
073055                                                                          
073056     EXEC SQL                                                             
073060        SELECT  BELEGRAD_1                                                
073100                                                                          
073200        INTO   :T01LSEL-BELEGRAD-1                                        
073300                                                                          
073400        FROM    T01LSEL                                                   
073500                                                                          
073600        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
073700            AND KDSTATUS = :WS-CURRENT                                    
073800     END-EXEC                                                             
073900                                                                          
074000     MOVE SQLCODE TO SQLCODE-WS                                           
074100     PERFORM DB2-STATUS-CHECK                                             
074200     .                                                                    
074300                                                                          
085000 DB2-SELECT-T01PDEV SECTION.                                              
085100     MOVE 000100305  TO GOOD-SQLCODECODES                                 
085200                                                                          
085300     EXEC SQL                                                             
085400         SELECT  IDLEGSEL                                                 
085500                ,KDBEHX                                                   
085510                ,FLPAYTE                                                  
085520                ,FLDELTE                                                  
085530                ,REARTRAB                                                 
085540                ,PRARTNTO_MIN                                             
085550                ,PRARTNTO_MAX                                             
085560                ,SUNTO_MIN                                                
085570                ,SUNTO_MAX                                                
085580                ,FLSOFT                                                   
085590                ,FLFREE                                                   
085591                ,FLSERV                                                   
085592                ,FLINVOIC                                                 
085593                ,KVMANAD                                                  
085594                ,DAREGDAT                                                 
085595                ,DAUPPDAT                                                 
085596                ,IDUSER                                                   
085600                                                                          
085700         INTO  :T01PDEV-IDLEGSEL                                          
085800             , :T01PDEV-KDBEHX                                            
085810             , :T01PDEV-FLPAYTE                                           
085820             , :T01PDEV-FLDELTE                                           
085830             , :T01PDEV-REARTRAB                                          
085840             , :T01PDEV-PRARTNTO-MIN                                      
085850             , :T01PDEV-PRARTNTO-MAX                                      
085860             , :T01PDEV-SUNTO-MIN                                         
085870             , :T01PDEV-SUNTO-MAX                                         
085880             , :T01PDEV-FLSOFT                                            
085890             , :T01PDEV-FLFREE                                            
085891             , :T01PDEV-FLSERV                                            
085892             , :T01PDEV-FLINVOIC                                          
085893             , :T01PDEV-KVMANAD                                           
085894             , :T01PDEV-DAREGDAT                                          
085895             , :T01PDEV-DAUPPDAT                                          
085896             , :T01PDEV-IDUSER                                            
085900                                                                          
086000         FROM  T01PDEV                                                    
086100                                                                          
086200         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
086300         AND   KDBEHX   = :REQU-KDBEHX-KEY                                
086600     END-EXEC                                                             
086700                                                                          
086800     MOVE SQLCODE TO SQLCODE-WS                                           
086900     PERFORM DB2-STATUS-CHECK                                             
087000     .                                                                    
087100                                                                          
087200 DB2-UPDATE-T01PDEV SECTION.                                              
087300     MOVE 000     TO GOOD-SQLCODECODES                                    
087400                                                                          
087500     EXEC SQL                                                             
087600        UPDATE T01PDEV                                                    
087700           SET                                                            
087800                 IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
087900               , KDBEHX       = :REQU-KDBEHX-KEY                          
088000               , FLPAYTE      = :REQU-FLPAYTE                             
088100               , FLDELTE      = :REQU-FLDELTE                             
088200               , REARTRAB     = :WS-REARTRAB-NUM                          
088300               , PRARTNTO_MIN = :WS-PRARTNTO-MIN-NUM                      
088400               , PRARTNTO_MAX = :WS-PRARTNTO-MAX-NUM                      
088410               , SUNTO_MIN    = :WS-SUNTO-MIN-NUM                         
088420               , SUNTO_MAX    = :WS-SUNTO-MAX-NUM                         
088430               , FLSOFT       = :REQU-FLSOFT                              
088440               , FLFREE       = :REQU-FLFREE                              
088450               , FLSERV       = :REQU-FLSERV                              
088460               , FLINVOIC     = :REQU-FLINVOIC                            
088470               , KVMANAD      = :WS-KVMANAD-NUM                           
088480               , DAREGDAT     = :T01PDEV-DAREGDAT                         
088490               , DAUPPDAT     = :T01PDEV-DAUPPDAT                         
088500               , IDUSER       = :REQU-IDUSER                              
088600                                                                          
088700         WHERE   IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
088800         AND     KDBEHX       = :REQU-KDBEHX-KEY                          
089100     END-EXEC                                                             
089200                                                                          
089300     MOVE SQLCODE TO SQLCODE-WS                                           
089400     PERFORM DB2-STATUS-CHECK                                             
089500     .                                                                    
089600                                                                          
097900 DB2-INSERT-T01PDEV  SECTION.                                             
098000     MOVE 000   TO GOOD-SQLCODECODES                                      
098100                                                                          
098200     EXEC SQL                                                             
098300         INSERT INTO T01PDEV                                              
098400            (IDLEGSEL                                                     
098500            ,KDBEHX                                                       
098600            ,FLPAYTE                                                      
098610            ,FLDELTE                                                      
098620            ,REARTRAB                                                     
098630            ,PRARTNTO_MIN                                                 
098640            ,PRARTNTO_MAX                                                 
098650            ,SUNTO_MIN                                                    
098660            ,SUNTO_MAX                                                    
098670            ,FLSOFT                                                       
098680            ,FLFREE                                                       
098690            ,FLSERV                                                       
098691            ,FLINVOIC                                                     
098692            ,KVMANAD                                                      
098693            ,DAREGDAT                                                     
098694            ,DAUPPDAT                                                     
098695            ,IDUSER)                                                      
098700         VALUES                                                           
098800            (:REQU-IDLEGSEL-KEY                                           
098900            ,:REQU-KDBEHX-KEY                                             
099000            ,:REQU-FLPAYTE                                                
099100            ,:REQU-FLDELTE                                                
099110            ,:WS-REARTRAB-NUM                                             
099120            ,:WS-PRARTNTO-MIN-NUM                                         
099130            ,:WS-PRARTNTO-MAX-NUM                                         
099140            ,:WS-SUNTO-MIN-NUM                                            
099150            ,:WS-SUNTO-MAX-NUM                                            
099160            ,:REQU-FLSOFT                                                 
099170            ,:REQU-FLFREE                                                 
099180            ,:REQU-FLSERV                                                 
099190            ,:REQU-FLINVOIC                                               
099191            ,:WS-KVMANAD-NUM                                              
099200            ,:T01PDEV-DAREGDAT                                            
099210            ,:T01PDEV-DAUPPDAT                                            
099300            ,:REQU-IDUSER)                                                
099400     END-EXEC                                                             
099500     MOVE SQLCODE TO SQLCODE-WS                                           
099600     PERFORM DB2-STATUS-CHECK                                             
099700     .                                                                    
099800                                                                          
107000 DB2-STATUS-CHECK  SECTION.                                               
107100     SET SQLCODE-IX TO 1                                                  
107200     SEARCH GOOD-SQLCODE                                                  
107300       AT END                                                             
107400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
107500          DELIMITED BY SIZE INTO ERROR-TEXT                               
107600          CALL ABEND USING RKOD-ABEND-DB2                                 
107700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
107800     END-SEARCH                                                           
107900     .                                                                    
