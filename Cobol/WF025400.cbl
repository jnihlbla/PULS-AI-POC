000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF025400.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/03/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.LEGSELMAINTENANCE                                
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT LEGAL SELLER TABLE T01LSEL DEPENDING          
001200*        ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                     
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'I' INSERT                                            
001600*                                                                         
001700*        THE PROGRAM READS           TABLE T01COCO                        
001800*        THE PROGRAM INSERTS/UPDATES TABLE T01LSEL                        
001900*        THE PROGRAM INSERTS         TABLE T01PROC                        
001910*        THE PROGRAM READS           TABLE T01CURR                        
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WF0254U                                             
002300*        REQUEST:     WF0254I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WF0254O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WF025400'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
004910 77  IX-CNT                      PIC S9(1)   VALUE  +1 COMP-3.            
004920 77  IX-CNT-MAX                  PIC S9(1)   VALUE  +2 COMP-3.            
005000 77  WS-ADRESS                   PIC X(50)                                
005100                    VALUE 'CARPARTS.BILLIT.LEGSELMAINTENANCE'.            
005200 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005300 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005400 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
005500                                                                          
005600 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005700     88  KEYS-OK                             VALUE 'Y'.                   
005800     88  KEYS-WRONG                          VALUE 'N'.                   
005900                                                                          
006000 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006100     88  ACT-CODE-VALID                      VALUE 'S', 'U', 'I'.         
006200     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006300     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006400     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006500                                                                          
006600 77  KDINVFRQ-SW                 PIC X(4)    VALUE SPACE.                 
006700     88  KDINVFRQ-VALID              VALUE 'NOW ', 'DAY ', 'WEEK'.        
006800 77  KDAPPEND-SW                 PIC X(4)    VALUE SPACE.                 
006900     88  KDAPPEND-VALID              VALUE 'PGRP', 'VAT', 'ORDC'.         
007000                                                                          
007100*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0254O1                      
007200 01  MAP-KDSTATUS                PIC S9(3)   VALUE ZERO COMP-3.           
007300 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
007400 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
007500                                                                          
007600*    --- WORK-FIELDS                                                      
007700 01  WS-FLCONTROL                PIC X       VALUE SPACE.                 
007800 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
007900                                                                          
007910 01  WS-AREA.                                                             
007920     03 WS-DASTADAT-KEY          PIC X(8)    VALUE SPACE.                 
007930     03 WS-REVALUTA              PIC S9(3)      COMP-3.                   
007940     03 WS-PRKURS-NUM            PIC S9(6)V9(5) COMP-3.                   
007950     03 WS-IDMSG-INFO            PIC X(3)       VALUE SPACE.              
007960     03 WS-IDMSG-ERROR           PIC X(3)       VALUE SPACE.              
007970     03 WS-IDELMT-ERROR          PIC X(16)      VALUE SPACE.              
007980     03 WS-DATUM                 PIC X(8)    VALUE SPACE.                 
007990 01  WS-DATE-YYMMDD.                                                      
007991     03 WS-DATUM-YEAR           PIC 9(4).                                 
007992     03 WS-DATUM-MON-DAY        PIC 9(4).                                 
008000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008400     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
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
009700         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
009800         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
009900         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
010000         05  ERR-FIELD-NOT-FOUND     PIC X(3)    VALUE '025'.             
010100         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
010200         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010300         05  ERR-SYSTEM-ERROR        PIC X(3)    VALUE '099'.             
010400     03  INFO-CODES.                                                      
010500         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
010600         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
010700         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011000     SKIP3                                                                
011100 01  -COPY WZ01SUB                                                        
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
011400     SKIP3                                                                
011500 01  -COPY WZ20DATE                                                       
011600     EJECT                                                                
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
011900     SKIP3                                                                
012000 01  -COPY WF0254O1  -PRE MAP-                                            
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012300     SKIP3                                                                
012400 01  REQU-AREA.                                                           
012500*    03  -COPY WZ01REQU                                                   
012600*    03  -COPY WF0254I1                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012900     SKIP3                                                                
013000 01  RESP-AREA.                                                           
013100*    03  -COPY WZ01RESP                                                   
013200*    03  -COPY WF0254O1                                                   
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
013800 01  DB2-WS.                                                              
013900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
014000         88  CURSOR-OK                       VALUE 000.                   
014100         88  LINES-FOUND                     VALUE 000.                   
014200         88  LINES-MISSING                   VALUE 100.                   
014300         88  RESOURCE-WRONG                  VALUE 904.                   
014400     03  GOOD-SQLCODECODES.                                               
014500         05  GOOD-SQLCODE OCCURS 5                                        
014600             INDEXED BY SQLCODE-IX PIC 9(3).                              
014700                                                                          
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
015000*01  -COPY T01LSEL -PRE T01LSEL-                                          
015100                                                                          
015200 01  FILLER                      PIC X(16)   VALUE 'T01PROC-AREA'.        
015300*01  -COPY T01PROC -PRE T01PROC-                                          
015400                                                                          
015500 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
015600*01  -COPY T01COCO -PRE COCO-                                             
015700                                                                          
015710 01  FILLER                  PIC X(16)    VALUE 'T01CURR-AREA'.           
015720*01  -COPY T01CURR -PRE T01CURR-                                          
015730     EJECT                                                                
015740                                                                          
015800     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015900                                                                          
016000     EXEC SQL INCLUDE T01PROC END-EXEC.                                   
016100                                                                          
016200     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
016201                                                                          
016210     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
016220     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500     EJECT                                                                
016600 PROCEDURE DIVISION.                                                      
016700 MAIN SECTION.                                                            
016800                                                                          
016900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017000     IF SUB-KDRC = 0                                                      
017100       PERFORM A-INIT                                                     
017200       PERFORM B-CHECK-KEYS                                               
017300       IF KEYS-OK                                                         
017400         PERFORM F-READ-SHOW-INFO                                         
017500       END-IF                                                             
017600       PERFORM S02-RETURN-RESPONSE                                        
017700     END-IF                                                               
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     INITIALIZE GOOD-SQLCODECODES                                         
018600     MOVE ALL '+' TO RESP-AREA                                            
018700     INITIALIZE MAP-RESP-WF0254O1                                         
018800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
018810                                         WS-DATUM                         
018820                                         WS-DASTADAT-KEY                  
018830                                         WS-DATE-YYMMDD                   
018840     MOVE '0101'                      TO WS-DASTADAT-KEY(5:4)             
018900     MOVE SPACE TO RESP-IDMSG-ERROR                                       
019000                   RESP-IDELMT-ERROR                                      
019100                   RESP-IDMSG-INFO                                        
019200     .                                                                    
019300*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
019400 B-CHECK-KEYS SECTION.                                                    
019500                                                                          
019600     MOVE YES TO KEYS-SW                                                  
019700     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
019800                                                                          
019900     IF REQU-KDSTATUS-KEY NUMERIC                                         
020000     AND REQU-IDMSGVER NUMERIC                                            
020100       IF REQU-IDLEGSEL-KEY > SPACE                                       
020200       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
020300       AND ACT-CODE-VALID                                                 
020400       AND (REQU-KDSTATUS-KEY = WS-CURRENT OR WS-COMING)                  
020500         CONTINUE                                                         
020600       ELSE                                                               
020700         MOVE NOO TO KEYS-SW                                              
020800       END-IF                                                             
020900     ELSE                                                                 
021000       MOVE NOO TO KEYS-SW                                                
021100     END-IF                                                               
021200                                                                          
021300     IF KEYS-WRONG                                                        
021400       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
021500       IF ACT-CODE-VALID                                                  
021600         CONTINUE                                                         
021700       ELSE                                                               
021800         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
021900         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
022000       END-IF                                                             
022100       IF REQU-IDMSGVER NUMERIC                                           
022200         CONTINUE                                                         
022300       ELSE                                                               
022400         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
022500         MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                       
022600       END-IF                                                             
022700     END-IF                                                               
022800     .                                                                    
022900*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
023000 F-READ-SHOW-INFO SECTION.                                                
023100                                                                          
023200     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
023300     MOVE REQU-KDSTATUS-KEY TO RESP-KDSTATUS-KEY                          
023400                                                                          
023500     PERFORM FA-READ-BASICDATA                                            
023600     .                                                                    
023700*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
023800 FA-READ-BASICDATA SECTION.                                               
023900                                                                          
024000     IF RESP-IDMSG-ERROR = SPACE                                          
024100        IF  REQU-IDUSER > SPACE                                           
024200        AND REQU-IDUSER NOT = ALL '+'                                     
024300           CONTINUE                                                       
024400        ELSE                                                              
024500           MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                      
024600           MOVE 'IDUSER' TO RESP-IDELMT-ERROR                             
024700        END-IF                                                            
024800     END-IF                                                               
024900                                                                          
025000     IF RESP-IDMSG-ERROR = SPACE                                          
025100        IF ACT-CODE-SEARCH                                                
025200           PERFORM FAA-SEARCH-T01LSEL                                     
025300        ELSE                                                              
025400           PERFORM FAB-CHECK-REQU-DATA                                    
025500           IF RESP-IDMSG-ERROR = SPACE                                    
025600              IF ACT-CODE-UPDATE                                          
025700                 PERFORM FAC-UPDATE-T01LSEL                               
025800              ELSE                                                        
025900                 IF ACT-CODE-INSERT                                       
026000                    PERFORM FAD-INSERT-T01LSEL                            
026100                 END-IF                                                   
026200              END-IF                                                      
026300           END-IF                                                         
026400        END-IF                                                            
026500     END-IF                                                               
026600     .                                                                    
026700*** - SEARCH FOR RIGHT LEGAL SELLER AND MARK CURRENT LINE                 
026800*** - IF COMING LINE EXIST.                                               
026900 FAA-SEARCH-T01LSEL SECTION.                                              
027000                                                                          
027100     MOVE NOO TO WS-FLCONTROL                                             
027200                                                                          
027300     PERFORM DB2-DCL-OPN-T01LSEL-CRS-1                                    
027400     PERFORM DB2-FETCH-T01LSEL-CRS-1                                      
027500                                                                          
027600     IF LINES-FOUND                                                       
027700        PERFORM UNTIL LINES-MISSING                                       
027800           IF REQU-KDSTATUS-KEY = WS-CURRENT                              
027900              IF MAP-KDSTATUS = WS-CURRENT                                
028000                 PERFORM S03-MOVE-SEARCH-TO-RESPOND                       
028100                 MOVE YES TO WS-FLCONTROL                                 
028200              ELSE                                                        
028300                 IF MAP-KDSTATUS = WS-COMING                              
028400                    IF WS-FLCONTROL = YES                                 
028500                       MOVE YES TO RESP-FLCOMING                          
028600                       MOVE NOO TO WS-FLCONTROL                           
028700                    END-IF                                                
028800                 END-IF                                                   
028900              END-IF                                                      
029000           ELSE                                                           
029100              IF REQU-KDSTATUS-KEY = WS-COMING                            
029200                 IF MAP-KDSTATUS = WS-COMING                              
029300                    PERFORM S03-MOVE-SEARCH-TO-RESPOND                    
029400                    MOVE SPACE TO RESP-IDMSG-ERROR                        
029500                 ELSE                                                     
029600                    MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR          
029700                    MOVE 'COMMING VERSION'   TO RESP-IDELMT-ERROR         
029800                    PERFORM S05-MOVE-SPACE-TO-RESPOND                     
029900                 END-IF                                                   
030000              END-IF                                                      
030100           END-IF                                                         
030200           PERFORM DB2-FETCH-T01LSEL-CRS-1                                
030300        END-PERFORM                                                       
030400     ELSE                                                                 
030500        MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                      
030600        MOVE 'IDLEGSEL'          TO RESP-IDELMT-ERROR                     
030700        PERFORM S05-MOVE-SPACE-TO-RESPOND                                 
030800     END-IF                                                               
030900                                                                          
031000     PERFORM DB2-CLOSE-T01LSEL-CRS-1                                      
031100     .                                                                    
031200*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT ON LEGAL SELLER.        
031300 FAB-CHECK-REQU-DATA SECTION.                                             
031400                                                                          
031500* COMPULSORY FIELDS                                                       
031600     IF  REQU-BELEG-NAME1 > SPACE                                         
031700     AND REQU-BELEG-NAME1 NOT = ALL '+'                                   
031800        CONTINUE                                                          
031900     ELSE                                                                 
032000        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
032100        MOVE 'BELEG-NAME1' TO RESP-IDELMT-ERROR                           
032200     END-IF                                                               
032300                                                                          
032400     IF RESP-IDMSG-ERROR = SPACE                                          
032500        IF REQU-ADLEG-STREET > SPACE                                      
032600        AND REQU-ADLEG-STREET NOT = ALL '+'                               
032700          CONTINUE                                                        
032800        ELSE                                                              
032900          MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                    
033000          MOVE 'ADLEG-STREET' TO RESP-IDELMT-ERROR                        
033100        END-IF                                                            
033200     END-IF                                                               
033300                                                                          
033400     IF RESP-IDMSG-ERROR = SPACE                                          
033500        IF  REQU-ADLEG-PCODE > SPACE                                      
033600        AND REQU-ADLEG-PCODE NOT = ALL '+'                                
033700           CONTINUE                                                       
033800        ELSE                                                              
033900           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
034000           MOVE 'ADLEG-PCODE' TO RESP-IDELMT-ERROR                        
034100        END-IF                                                            
034200     END-IF                                                               
034300                                                                          
034400     IF RESP-IDMSG-ERROR = SPACE                                          
034500        IF  REQU-ADLEG-CITY > SPACE                                       
034600        AND REQU-ADLEG-CITY NOT = ALL '+'                                 
034700           CONTINUE                                                       
034800        ELSE                                                              
034900           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
035000           MOVE 'ADLEG-CITY' TO RESP-IDELMT-ERROR                         
035100        END-IF                                                            
035200     END-IF                                                               
035300                                                                          
035400     IF RESP-IDMSG-ERROR = SPACE                                          
035500        IF  REQU-IDLANDX3 > SPACE                                         
035600        AND REQU-IDLANDX3 NOT = ALL '+'                                   
035700           PERFORM DB2-SELECT-T01COCO-TAB                                 
035800           IF LINES-FOUND                                                 
035900              MOVE COCO-BELAND TO MAP-RESP-BELAND                         
036000           ELSE                                                           
036100              MOVE SPACE TO MAP-RESP-BELAND                               
036200              MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                
036300              MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                        
036400           END-IF                                                         
036500        ELSE                                                              
036600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
036700           MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                           
036800        END-IF                                                            
036900     END-IF                                                               
037000                                                                          
037100     IF RESP-IDMSG-ERROR = SPACE                                          
037200        IF  REQU-IDTFN > SPACE                                            
037300        AND REQU-IDTFN NOT = ALL '+'                                      
037400           CONTINUE                                                       
037500        ELSE                                                              
037600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
037700           MOVE 'IDTFN' TO RESP-IDELMT-ERROR                              
037800        END-IF                                                            
037900     END-IF                                                               
038000                                                                          
038100     IF RESP-IDMSG-ERROR = SPACE                                          
038200        IF  REQU-IDTFX > SPACE                                            
038300        AND REQU-IDTFX NOT = ALL '+'                                      
038400           CONTINUE                                                       
038500        ELSE                                                              
038600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
038700           MOVE 'IDTFX' TO RESP-IDELMT-ERROR                              
038800        END-IF                                                            
038900     END-IF                                                               
039000                                                                          
039100     IF RESP-IDMSG-ERROR = SPACE                                          
039200        IF  REQU-IDMAIL > SPACE                                           
039300        AND REQU-IDMAIL NOT = ALL '+'                                     
039400           CONTINUE                                                       
039500        ELSE                                                              
039600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
039700           MOVE 'IDMAIL' TO RESP-IDELMT-ERROR                             
039800        END-IF                                                            
039900     END-IF                                                               
040000                                                                          
040100     IF RESP-IDMSG-ERROR = SPACE                                          
040200        IF  REQU-BECONT > SPACE                                           
040300        AND REQU-BECONT NOT = ALL '+'                                     
040400           CONTINUE                                                       
040500        ELSE                                                              
040600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
040700           MOVE 'BECONT' TO RESP-IDELMT-ERROR                             
040800        END-IF                                                            
040900     END-IF                                                               
041000                                                                          
041100     IF RESP-IDMSG-ERROR = SPACE                                          
041200        IF  REQU-IDVAT > SPACE                                            
041300        AND REQU-IDVAT NOT = ALL '+'                                      
041400           CONTINUE                                                       
041500        ELSE                                                              
041600           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
041700           MOVE 'IDVAT' TO RESP-IDELMT-ERROR                              
041800        END-IF                                                            
041900     END-IF                                                               
042000                                                                          
042100     IF RESP-IDMSG-ERROR = SPACE                                          
042200        MOVE REQU-KDINVFRQ TO KDINVFRQ-SW                                 
042300        IF KDINVFRQ-VALID                                                 
042400           CONTINUE                                                       
042500        ELSE                                                              
042600           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
042700           MOVE 'KDINVFRQ' TO RESP-IDELMT-ERROR                           
042800        END-IF                                                            
042900     END-IF                                                               
043000                                                                          
043100     IF RESP-IDMSG-ERROR = SPACE                                          
043200        IF REQU-KDAPPEND > SPACE AND                                      
043300           REQU-KDAPPEND NOT = ALL '+'                                    
043400           MOVE REQU-KDAPPEND TO KDAPPEND-SW                              
043500           IF KDAPPEND-VALID                                              
043600              CONTINUE                                                    
043700           ELSE                                                           
043800              MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                  
043900              MOVE 'KDAPPEND' TO RESP-IDELMT-ERROR                        
044000           END-IF                                                         
044100        END-IF                                                            
044200     END-IF                                                               
044300                                                                          
044400     IF RESP-IDMSG-ERROR = SPACE                                          
044500        IF REQU-FLSLUT = YES OR NOO                                       
044600           IF REQU-FLSLUT = YES                                           
044700              MOVE JA TO REQU-FLSLUT                                      
044800           END-IF                                                         
044900        ELSE                                                              
045000           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
045100           MOVE 'FLSLUT' TO RESP-IDELMT-ERROR                             
045200        END-IF                                                            
045300     END-IF                                                               
045400                                                                          
045500     IF RESP-IDMSG-ERROR = SPACE                                          
045600        IF REQU-FLCUSUPD = YES OR NOO                                     
045700           IF REQU-FLCUSUPD = YES                                         
045800              MOVE JA TO REQU-FLCUSUPD                                    
045900           END-IF                                                         
046000        ELSE                                                              
046100           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
046200           MOVE 'FLCUSUPD' TO RESP-IDELMT-ERROR                           
046300        END-IF                                                            
046400     END-IF                                                               
046500                                                                          
046600     IF RESP-IDMSG-ERROR = SPACE                                          
046700        IF REQU-FLVATUPD = YES OR NOO                                     
046800           IF REQU-FLVATUPD = YES                                         
046900              MOVE JA TO REQU-FLVATUPD                                    
047000           END-IF                                                         
047100        ELSE                                                              
047200           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
047300           MOVE 'FLVATUPD' TO RESP-IDELMT-ERROR                           
047400        END-IF                                                            
047500     END-IF                                                               
047600                                                                          
047610     IF RESP-IDMSG-ERROR = SPACE                                          
047620        IF  REQU-KDVALISO > SPACE                                         
047630        AND REQU-KDVALISO NOT = ALL '+'                                   
047640           CONTINUE                                                       
047650        ELSE                                                              
047660           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
047670           MOVE 'KDVALISO' TO RESP-IDELMT-ERROR                           
047680        END-IF                                                            
047690     END-IF                                                               
047692                                                                          
047693     IF RESP-IDMSG-ERROR = SPACE                                          
047694        IF  REQU-KDTRADP > SPACE                                          
047695        AND REQU-KDTRADP NOT = ALL '+'                                    
047696           CONTINUE                                                       
047697        ELSE                                                              
047698           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
047699           MOVE 'KDTRADP' TO RESP-IDELMT-ERROR                            
047700        END-IF                                                            
047701     END-IF                                                               
047702                                                                          
047710     IF RESP-IDMSG-ERROR = SPACE                                          
047800        IF ACT-CODE-UPDATE                                                
047900           IF REQU-KDSTATUS-KEY = WS-CURRENT                              
048000             MOVE WS-CURRENT-DATE TO REQU-DAUPPDAT                        
048100             IF REQU-DAUPPDAT > WS-CURRENT-DATE                           
048200               MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                 
048300               MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR                
048400             ELSE                                                         
048500               IF REQU-DAUPPDAT < WS-CURRENT-DATE                         
048600                 MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR               
048700                 MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR              
048800               ELSE                                                       
048900                 MOVE WS-CURRENT-DATE TO REQU-DAUPPDAT                    
049000               END-IF                                                     
049100             END-IF                                                       
049200           ELSE                                                           
049300              IF REQU-DAUPPDAT NUMERIC                                    
049400                IF REQU-DAUPPDAT > WS-CURRENT-DATE                        
049500                  MOVE REQU-DAUPPDAT TO DATE-TIDATE                       
049600                  MOVE WS-DATE-FORMAT TO DATE-KDDATFMT                    
049700                  CALL WZ20DATE USING DATE-WZ20DATE                       
049800                  IF DATE-KDRC > ZERO                                     
049900                    MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR            
050000                    MOVE 'DAUPPDAT'        TO RESP-IDELMT-ERROR           
050100                  END-IF                                                  
050200                ELSE                                                      
050300                  MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR              
050400                  MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                    
050500                END-IF                                                    
050600              ELSE                                                        
050700                MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR              
050800                MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                      
050900              END-IF                                                      
051000           END-IF                                                         
051100        ELSE                                                              
051200           IF ACT-CODE-INSERT                                             
051300              IF REQU-KDSTATUS-KEY = WS-COMING                            
051400                MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR           
051500              ELSE                                                        
051600                IF REQU-DAUPPDAT = SPACE                                  
051700                OR REQU-DAUPPDAT = ALL '+'                                
051800                OR REQU-DAUPPDAT = '00000000'                             
051900                   MOVE '00000000' TO REQU-DAUPPDAT                       
052000                ELSE                                                      
052100                   MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR             
052200                   MOVE 'DAUPPDAT' TO RESP-IDELMT-ERROR                   
052300                END-IF                                                    
052400              END-IF                                                      
052500           END-IF                                                         
052600        END-IF                                                            
052700     END-IF                                                               
052800                                                                          
052900* RELATIONS BETWEEN FIELDS                                                
053000                                                                          
053100* SPACE OUT FIELDS THAT CONTENTS 'ALL PLUS'                               
053200     IF REQU-BELEG-NAME2 = ALL '+'                                        
053300        MOVE SPACE TO REQU-BELEG-NAME2                                    
053400     END-IF                                                               
053500                                                                          
053600     IF REQU-ADLEG-BOX = ALL '+'                                          
053700        MOVE SPACE TO REQU-ADLEG-BOX                                      
053800     END-IF                                                               
053900                                                                          
054000     IF REQU-IDBG = ALL '+'                                               
054100        MOVE SPACE TO REQU-IDBG                                           
054200     END-IF                                                               
054300                                                                          
054400     IF REQU-IDPG = ALL '+'                                               
054500        MOVE SPACE TO REQU-IDPG                                           
054600     END-IF                                                               
054700                                                                          
054800     .                                                                    
054900*** - CHECK IF UPDATE IS ON CURRENT OR COMING LINE                        
055000 FAC-UPDATE-T01LSEL SECTION.                                              
055100                                                                          
055200     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
055300        PERFORM FACA-UPD-CURRENT                                          
055400     ELSE                                                                 
055500        PERFORM FACB-UPD-COMING                                           
055600     END-IF                                                               
055700     .                                                                    
055800*** - UPDATE CURRENT LINE ON T01LSEL.                                     
055900 FACA-UPD-CURRENT SECTION.                                                
056000                                                                          
056100     PERFORM DB2-SELECT-T01LSEL-TAB-STA1                                  
056200                                                                          
056300     IF LINES-FOUND                                                       
056400        PERFORM DB2-UPDATE-T01LSEL-TAB-STA1                               
056500        PERFORM S04-MOVE-UPD-INS-TO-RESPOND                               
056600        IF REQU-FLCOMING = YES                                            
056700           MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                
056800        ELSE                                                              
056900           MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                          
057000        END-IF                                                            
057100     ELSE                                                                 
057200        MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                      
057300        MOVE 'IDLEGSEL'           TO RESP-IDELMT-ERROR                    
057400        PERFORM S05-MOVE-SPACE-TO-RESPOND                                 
057500     END-IF                                                               
057600     .                                                                    
057700*** - UPDATE COMING LINE ON T01LSEL. IF NOO COMING LINE EXIST BUT         
057800***   COMING LINE IS CHOOSED FOR UPDATE, PROGRAM WILL INSERT ONE          
057900***   COMING LINE WITH DATA FROM CURRENT LINE BUT USER WILL SEE           
058000***   THIS AS AN UPDATE.                                                  
058100 FACB-UPD-COMING SECTION.                                                 
058200                                                                          
058300     PERFORM DB2-SELECT-T01LSEL-TAB-STA2                                  
058400                                                                          
058500     IF LINES-FOUND                                                       
058600        PERFORM DB2-UPDATE-T01LSEL-TAB-STA2                               
058700        PERFORM S04-MOVE-UPD-INS-TO-RESPOND                               
058800        MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                   
058900     ELSE                                                                 
059000        PERFORM DB2-SELECT-T01LSEL-TAB-STA1                               
059100        IF LINES-FOUND                                                    
059200           PERFORM DB2-INSERT-T01LSEL-TAB-STA2                            
059300           PERFORM S04-MOVE-UPD-INS-TO-RESPOND                            
059400           MOVE INF-OTHER-VERSION-EXIST TO RESP-IDMSG-INFO                
059500        ELSE                                                              
059600           MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
059700        END-IF                                                            
059800     END-IF                                                               
059900     .                                                                    
060000*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
060100 FAD-INSERT-T01LSEL SECTION.                                              
060200                                                                          
060300     IF REQU-KDSTATUS-KEY = WS-CURRENT                                    
060400       PERFORM DB2-SELECT-T01LSEL-TAB-STA1                                
060500       IF LINES-FOUND                                                     
060600         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
060700         MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                       
060800       ELSE                                                               
060900         MOVE WS-CURRENT-DATE TO MAP-DAREGDAT                             
061000         PERFORM DB2-INSERT-T01LSEL-TAB-STA1                              
061100         PERFORM S04-MOVE-UPD-INS-TO-RESPOND                              
061200         PERFORM DB2-INSERT-T01PROC                                       
061220         PERFORM DB2-SELECT-T01CURR                                       
061230         IF LINES-FOUND                                                   
061240           MOVE ERR-ALREADY-EXIST TO WS-IDMSG-ERROR                       
061250           MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                        
061260           MOVE NOO            TO KEYS-SW                                 
061270         ELSE                                                             
061280           IF LINES-MISSING                                               
061290             PERFORM UNTIL IX-CNT > IX-CNT-MAX                            
061291               PERFORM DB2-INSERT-T01CURR                                 
061293                 COMPUTE WS-DATUM-YEAR = (WS-DATUM-YEAR - 2)              
061295                 MOVE  '0101'  TO WS-DATUM-MON-DAY                        
061296                 MOVE WS-DATE-YYMMDD TO WS-DASTADAT-KEY                   
061297               ADD 1 TO IX-CNT                                            
061298             END-PERFORM                                                  
061301           END-IF                                                         
061302         END-IF                                                           
061310         MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                            
061400       END-IF                                                             
061500     ELSE                                                                 
061600       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
061700     END-IF                                                               
061800     .                                                                    
061900                                                                          
062000*    --- DISPATCHER SECTIONS                                              
062100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
062200                                                                          
062300     MOVE 'GETARG'                   TO SUB-KDFUNC                        
062400     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
062500     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
062600                                                                          
062700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
062800                                                                          
062900     IF SUB-KDRC > 0                                                      
063000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
063100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
063200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
063300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
063400     END-IF                                                               
063500     .                                                                    
063600     SKIP3                                                                
063700 S02-RETURN-RESPONSE SECTION.                                             
063800                                                                          
063900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
064000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
064100                                                                          
064200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
064300                                                                          
064400     IF SUB-KDRC > 0                                                      
064500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
064600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
064700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
064800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200                                                                          
065300*    --- MOVE TO OUTPUT SECTIONS                                          
065400 S03-MOVE-SEARCH-TO-RESPOND SECTION.                                      
065500                                                                          
065600     MOVE MAP-RESP-BELEG-NAME1  TO RESP-BELEG-NAME1                       
065700     MOVE MAP-RESP-BELEG-NAME2  TO RESP-BELEG-NAME2                       
065800     MOVE MAP-RESP-ADLEG-STREET TO RESP-ADLEG-STREET                      
065900     MOVE MAP-RESP-ADLEG-BOX    TO RESP-ADLEG-BOX                         
066000     MOVE MAP-RESP-ADLEG-PCODE  TO RESP-ADLEG-PCODE                       
066100     MOVE MAP-RESP-ADLEG-CITY   TO RESP-ADLEG-CITY                        
066200     MOVE MAP-RESP-IDLANDX3     TO RESP-IDLANDX3                          
066300     PERFORM DB2-SELECT-T01COCO-TAB                                       
066400     MOVE COCO-BELAND           TO MAP-RESP-BELAND                        
066500     MOVE MAP-RESP-BELAND       TO RESP-BELAND                            
066600     MOVE MAP-RESP-IDTFN        TO RESP-IDTFN                             
066700     MOVE MAP-RESP-IDTFX        TO RESP-IDTFX                             
066800     MOVE MAP-RESP-IDMAIL       TO RESP-IDMAIL                            
066900     MOVE MAP-RESP-BECONT       TO RESP-BECONT                            
067000     MOVE MAP-RESP-IDVAT        TO RESP-IDVAT                             
067100     MOVE MAP-RESP-IDBG         TO RESP-IDBG                              
067200     MOVE MAP-RESP-IDPG         TO RESP-IDPG                              
067300     MOVE MAP-RESP-KDINVFRQ     TO RESP-KDINVFRQ                          
067400     MOVE MAP-RESP-KDAPPEND     TO RESP-KDAPPEND                          
067500     MOVE MAP-RESP-FLSLUT       TO RESP-FLSLUT                            
067600     IF RESP-FLSLUT = JA                                                  
067700       MOVE YES TO RESP-FLSLUT                                            
067800     END-IF                                                               
067900     MOVE MAP-RESP-FLCUSUPD     TO RESP-FLCUSUPD                          
068000     IF RESP-FLCUSUPD = JA                                                
068100       MOVE YES TO RESP-FLCUSUPD                                          
068200     END-IF                                                               
068300     MOVE MAP-RESP-FLVATUPD     TO RESP-FLVATUPD                          
068400     IF RESP-FLVATUPD = JA                                                
068500       MOVE YES TO RESP-FLVATUPD                                          
068600     END-IF                                                               
068700     MOVE MAP-DAREGDAT          TO RESP-DAREGDAT                          
068800     MOVE MAP-DAUPPDAT          TO RESP-DAUPPDAT                          
068900     MOVE MAP-RESP-IDUSER       TO RESP-IDUSER                            
069000     MOVE NOO                   TO RESP-FLCOMING                          
069010     MOVE MAP-RESP-KDVALISO     TO RESP-KDVALISO                          
069020     MOVE MAP-RESP-KDTRADP      TO RESP-KDTRADP                           
069100     .                                                                    
069200 S04-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
069300                                                                          
069400     MOVE REQU-BELEG-NAME1  TO RESP-BELEG-NAME1                           
069500     MOVE REQU-BELEG-NAME2  TO RESP-BELEG-NAME2                           
069600     MOVE REQU-ADLEG-STREET TO RESP-ADLEG-STREET                          
069700     MOVE REQU-ADLEG-BOX    TO RESP-ADLEG-BOX                             
069800     MOVE REQU-ADLEG-PCODE  TO RESP-ADLEG-PCODE                           
069900     MOVE REQU-ADLEG-CITY   TO RESP-ADLEG-CITY                            
070000     MOVE REQU-IDLANDX3     TO RESP-IDLANDX3                              
070100     MOVE MAP-RESP-BELAND   TO RESP-BELAND                                
070200     MOVE REQU-IDTFN        TO RESP-IDTFN                                 
070300     MOVE REQU-IDTFX        TO RESP-IDTFX                                 
070400     MOVE REQU-IDMAIL       TO RESP-IDMAIL                                
070500     MOVE REQU-IDVAT        TO RESP-IDVAT                                 
070600     MOVE REQU-IDBG         TO RESP-IDBG                                  
070700     MOVE REQU-IDPG         TO RESP-IDPG                                  
070800     MOVE REQU-KDINVFRQ     TO RESP-KDINVFRQ                              
070900     MOVE REQU-KDAPPEND     TO RESP-KDAPPEND                              
071000     MOVE REQU-FLSLUT       TO RESP-FLSLUT                                
071100     IF RESP-FLSLUT = JA                                                  
071200        MOVE YES TO RESP-FLSLUT                                           
071300     END-IF                                                               
071400     MOVE REQU-FLCUSUPD     TO RESP-FLCUSUPD                              
071500     IF RESP-FLCUSUPD = JA                                                
071600        MOVE YES TO RESP-FLCUSUPD                                         
071700     END-IF                                                               
071800     MOVE REQU-FLVATUPD     TO RESP-FLVATUPD                              
071900     IF RESP-FLVATUPD = JA                                                
072000        MOVE YES TO RESP-FLVATUPD                                         
072100     END-IF                                                               
072200     MOVE MAP-DAREGDAT      TO RESP-DAREGDAT                              
072300     MOVE REQU-DAUPPDAT     TO RESP-DAUPPDAT                              
072400     MOVE REQU-IDUSER       TO RESP-IDUSER                                
072500     MOVE REQU-FLCOMING     TO RESP-FLCOMING                              
072510     MOVE REQU-KDVALISO     TO RESP-KDVALISO                              
072520     MOVE REQU-KDTRADP      TO RESP-KDTRADP                               
072600                                                                          
072700* DONT SHOW DAREGDAT WHEN UPDATING ON COMING VERSION                      
072800     IF ACT-CODE-UPDATE                                                   
072900     AND REQU-KDSTATUS-KEY = WS-COMING                                    
073000       MOVE ZERO TO RESP-DAREGDAT                                         
073100     END-IF                                                               
073200     .                                                                    
073300 S05-MOVE-SPACE-TO-RESPOND SECTION.                                       
073400                                                                          
073500     MOVE SPACE             TO RESP-BELEG-NAME1                           
073600     MOVE SPACE             TO RESP-BELEG-NAME2                           
073700     MOVE SPACE             TO RESP-ADLEG-STREET                          
073800     MOVE SPACE             TO RESP-ADLEG-BOX                             
073900     MOVE SPACE             TO RESP-ADLEG-PCODE                           
074000     MOVE SPACE             TO RESP-ADLEG-CITY                            
074100     MOVE SPACE             TO RESP-IDLANDX3                              
074200     MOVE SPACE             TO RESP-BELAND                                
074300     MOVE SPACE             TO RESP-IDTFN                                 
074400     MOVE SPACE             TO RESP-IDTFX                                 
074500     MOVE SPACE             TO RESP-IDMAIL                                
074600     MOVE SPACE             TO RESP-BECONT                                
074700     MOVE SPACE             TO RESP-IDVAT                                 
074800     MOVE SPACE             TO RESP-IDBG                                  
074900     MOVE SPACE             TO RESP-IDPG                                  
075000     MOVE SPACE             TO RESP-KDINVFRQ                              
075100     MOVE SPACE             TO RESP-KDAPPEND                              
075200     MOVE SPACE             TO RESP-FLSLUT                                
075300     MOVE SPACE             TO RESP-FLCUSUPD                              
075400     MOVE SPACE             TO RESP-FLVATUPD                              
075410     MOVE SPACE             TO RESP-KDVALISO                              
075420     MOVE SPACE             TO RESP-KDTRADP                               
075500     MOVE ZERO              TO RESP-DAREGDAT                              
075600     MOVE ZERO              TO RESP-DAUPPDAT                              
075700     MOVE REQU-IDUSER       TO RESP-IDUSER                                
075800     MOVE NOO               TO RESP-FLCOMING                              
075900     .                                                                    
076000*    --- DB2 SECTIONS                                                     
076100 DB2-SELECT-T01COCO-TAB SECTION.                                          
076200                                                                          
076300     MOVE 000100  TO GOOD-SQLCODECODES                                    
076400     EXEC SQL                                                             
076500          SELECT  BELAND                                                  
076600                                                                          
076700          INTO :COCO-BELAND                                               
076800                                                                          
076900          FROM  T01COCO                                                   
077000                                                                          
077100          WHERE   IDLANDX3 = :REQU-IDLANDX3                               
077200     END-EXEC                                                             
077300                                                                          
077400     MOVE SQLCODE TO SQLCODE-WS                                           
077500     PERFORM DB2-STATUS-CHECK                                             
077600     .                                                                    
077700 DB2-DCL-OPN-T01LSEL-CRS-1 SECTION.                                       
077800                                                                          
077900     MOVE 000100 TO GOOD-SQLCODECODES                                     
078000                                                                          
078100     EXEC SQL                                                             
078200         DECLARE T01LSEL-CRS-1 CURSOR WITH HOLD FOR                       
078300                                                                          
078400           SELECT  KDSTATUS                                               
078500                 , BELEGRAD_1                                             
078600                 , BELEGRAD_2                                             
078700                 , ADLEG_STREET                                           
078800                 , ADLEG_BOX                                              
078900                 , ADLEG_PCODE                                            
079000                 , ADLEG_CITY                                             
079100                 , IDLANDX3                                               
079200                 , IDTFN                                                  
079300                 , IDTFX                                                  
079400                 , IDMAIL                                                 
079500                 , BECONT                                                 
079600                 , IDVAT                                                  
079700                 , IDBG                                                   
079800                 , IDPG                                                   
079900                 , KDINVFRQ                                               
080000                 , KDAPPEND                                               
080100                 , FLSLUT                                                 
080200                 , DAREGDAT                                               
080300                 , DAUPPDAT                                               
080400                 , IDUSER                                                 
080500                 , FLCUSUPD                                               
080600                 , FLVATUPD                                               
080610                 , KDVALISO                                               
080620                 , KDTRADP                                                
080700                                                                          
080800           FROM     T01LSEL                                               
080900                                                                          
081000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
081100                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
081200                                                                          
081300           ORDER BY IDLEGSEL                                              
081400                  , KDSTATUS                                              
081500     END-EXEC                                                             
081600                                                                          
081700     MOVE 000100 TO GOOD-SQLCODECODES                                     
081800                                                                          
081900     EXEC SQL                                                             
082000        OPEN T01LSEL-CRS-1                                                
082100     END-EXEC                                                             
082200                                                                          
082300     MOVE SQLCODE TO SQLCODE-WS                                           
082400     PERFORM DB2-STATUS-CHECK                                             
082500     .                                                                    
082600                                                                          
082700 DB2-FETCH-T01LSEL-CRS-1 SECTION.                                         
082800                                                                          
082900     MOVE 000100  TO GOOD-SQLCODECODES                                    
083000                                                                          
083100     EXEC SQL                                                             
083200         FETCH T01LSEL-CRS-1                                              
083300                                                                          
083400         INTO :MAP-KDSTATUS                                               
083500            , :MAP-RESP-BELEG-NAME1                                       
083600            , :MAP-RESP-BELEG-NAME2                                       
083700            , :MAP-RESP-ADLEG-STREET                                      
083800            , :MAP-RESP-ADLEG-BOX                                         
083900            , :MAP-RESP-ADLEG-PCODE                                       
084000            , :MAP-RESP-ADLEG-CITY                                        
084100            , :MAP-RESP-IDLANDX3                                          
084200            , :MAP-RESP-IDTFN                                             
084300            , :MAP-RESP-IDTFX                                             
084400            , :MAP-RESP-IDMAIL                                            
084500            , :MAP-RESP-BECONT                                            
084600            , :MAP-RESP-IDVAT                                             
084700            , :MAP-RESP-IDBG                                              
084800            , :MAP-RESP-IDPG                                              
084900            , :MAP-RESP-KDINVFRQ                                          
085000            , :MAP-RESP-KDAPPEND                                          
085100            , :MAP-RESP-FLSLUT                                            
085200            , :MAP-DAREGDAT                                               
085300            , :MAP-DAUPPDAT                                               
085400            , :MAP-RESP-IDUSER                                            
085500            , :MAP-RESP-FLCUSUPD                                          
085600            , :MAP-RESP-FLVATUPD                                          
085610            , :MAP-RESP-KDVALISO                                          
085620            , :MAP-RESP-KDTRADP                                           
085700     END-EXEC                                                             
085800                                                                          
085900     MOVE SQLCODE TO SQLCODE-WS                                           
086000     PERFORM DB2-STATUS-CHECK                                             
086100     .                                                                    
086200 DB2-CLOSE-T01LSEL-CRS-1  SECTION.                                        
086300                                                                          
086400     EXEC SQL                                                             
086500        CLOSE T01LSEL-CRS-1                                               
086600     END-EXEC                                                             
086700     .                                                                    
086800 DB2-SELECT-T01LSEL-TAB-STA1 SECTION.                                     
086900                                                                          
087000     MOVE 000100  TO GOOD-SQLCODECODES                                    
087100     EXEC SQL                                                             
087200         SELECT  KDSTATUS                                                 
087300               , BELEGRAD_1                                               
087400               , BELEGRAD_2                                               
087500               , ADLEG_STREET                                             
087600               , ADLEG_BOX                                                
087700               , ADLEG_PCODE                                              
087800               , ADLEG_CITY                                               
087900               , IDLANDX3                                                 
088000               , IDTFN                                                    
088100               , IDTFX                                                    
088200               , IDMAIL                                                   
088300               , BECONT                                                   
088400               , IDVAT                                                    
088500               , IDBG                                                     
088600               , IDPG                                                     
088700               , KDINVFRQ                                                 
088800               , KDAPPEND                                                 
088900               , FLSLUT                                                   
089000               , DAREGDAT                                                 
089100               , DAUPPDAT                                                 
089200               , FLCUSUPD                                                 
089300               , FLVATUPD                                                 
089310               , KDVALISO                                                 
089320               , KDTRADP                                                  
089400                                                                          
089500         INTO :MAP-KDSTATUS                                               
089600            , :MAP-RESP-BELEG-NAME1                                       
089700            , :MAP-RESP-BELEG-NAME2                                       
089800            , :MAP-RESP-ADLEG-STREET                                      
089900            , :MAP-RESP-ADLEG-BOX                                         
090000            , :MAP-RESP-ADLEG-PCODE                                       
090100            , :MAP-RESP-ADLEG-CITY                                        
090200            , :MAP-RESP-IDLANDX3                                          
090300            , :MAP-RESP-IDTFN                                             
090400            , :MAP-RESP-IDTFX                                             
090500            , :MAP-RESP-IDMAIL                                            
090600            , :MAP-RESP-BECONT                                            
090700            , :MAP-RESP-IDVAT                                             
090800            , :MAP-RESP-IDBG                                              
090900            , :MAP-RESP-IDPG                                              
091000            , :MAP-RESP-KDINVFRQ                                          
091100            , :MAP-RESP-KDAPPEND                                          
091200            , :MAP-RESP-FLSLUT                                            
091300            , :MAP-DAREGDAT                                               
091400            , :MAP-DAUPPDAT                                               
091500            , :MAP-RESP-FLCUSUPD                                          
091600            , :MAP-RESP-FLVATUPD                                          
091610            , :MAP-RESP-KDVALISO                                          
091620            , :MAP-RESP-KDTRADP                                           
091700                                                                          
091800         FROM    T01LSEL                                                  
091900                                                                          
092000         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
092100             AND KDSTATUS = :WS-CURRENT                                   
092200     END-EXEC                                                             
092300                                                                          
092400     MOVE SQLCODE TO SQLCODE-WS                                           
092500     PERFORM DB2-STATUS-CHECK                                             
092600     .                                                                    
092700 DB2-UPDATE-T01LSEL-TAB-STA1  SECTION.                                    
092800                                                                          
092900     MOVE 000     TO GOOD-SQLCODECODES                                    
093000     EXEC SQL                                                             
093100         UPDATE T01LSEL                                                   
093200             SET   BELEGRAD_1   = :REQU-BELEG-NAME1                       
093300                 , BELEGRAD_2   = :REQU-BELEG-NAME2                       
093400                 , ADLEG_STREET = :REQU-ADLEG-STREET                      
093500                 , ADLEG_BOX    = :REQU-ADLEG-BOX                         
093600                 , ADLEG_PCODE  = :REQU-ADLEG-PCODE                       
093700                 , ADLEG_CITY   = :REQU-ADLEG-CITY                        
093800                 , IDLANDX3     = :REQU-IDLANDX3                          
093900                 , IDTFN        = :REQU-IDTFN                             
094000                 , IDTFX        = :REQU-IDTFX                             
094100                 , IDMAIL       = :REQU-IDMAIL                            
094200                 , BECONT       = :REQU-BECONT                            
094300                 , IDVAT        = :REQU-IDVAT                             
094400                 , IDBG         = :REQU-IDBG                              
094500                 , IDPG         = :REQU-IDPG                              
094600                 , KDINVFRQ     = :REQU-KDINVFRQ                          
094700                 , KDAPPEND     = :REQU-KDAPPEND                          
094800                 , FLSLUT       = :REQU-FLSLUT                            
094900                 , DAREGDAT     = :MAP-DAREGDAT                           
095000                 , DAUPPDAT     = :REQU-DAUPPDAT                          
095100                 , IDUSER       = :REQU-IDUSER                            
095200                 , FLCUSUPD     = :REQU-FLCUSUPD                          
095300                 , FLVATUPD     = :REQU-FLVATUPD                          
095310                 , KDVALISO     = :REQU-KDVALISO                          
095320                 , KDTRADP      = :REQU-KDTRADP                           
095400                                                                          
095500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
095600             AND KDSTATUS = :WS-CURRENT                                   
095700     END-EXEC                                                             
095800                                                                          
095900     MOVE SQLCODE TO SQLCODE-WS                                           
096000     PERFORM DB2-STATUS-CHECK                                             
096100     .                                                                    
096200 DB2-SELECT-T01LSEL-TAB-STA2 SECTION.                                     
096300                                                                          
096400     MOVE 000100  TO GOOD-SQLCODECODES                                    
096500     EXEC SQL                                                             
096600           SELECT  KDSTATUS                                               
096700                 , BELEGRAD_1                                             
096800                 , BELEGRAD_2                                             
096900                 , ADLEG_STREET                                           
097000                 , ADLEG_BOX                                              
097100                 , ADLEG_PCODE                                            
097200                 , ADLEG_CITY                                             
097300                 , IDLANDX3                                               
097400                 , IDTFN                                                  
097500                 , IDTFX                                                  
097600                 , IDMAIL                                                 
097700                 , BECONT                                                 
097800                 , IDVAT                                                  
097900                 , IDBG                                                   
098000                 , IDPG                                                   
098100                 , KDINVFRQ                                               
098200                 , KDAPPEND                                               
098300                 , FLSLUT                                                 
098400                 , DAREGDAT                                               
098500                 , DAUPPDAT                                               
098600                 , FLCUSUPD                                               
098700                 , FLVATUPD                                               
098710                 , KDVALISO                                               
098720                 , KDTRADP                                                
098800                                                                          
098900           INTO :MAP-KDSTATUS                                             
099000              , :MAP-RESP-BELEG-NAME1                                     
099100              , :MAP-RESP-BELEG-NAME2                                     
099200              , :MAP-RESP-ADLEG-STREET                                    
099300              , :MAP-RESP-ADLEG-BOX                                       
099400              , :MAP-RESP-ADLEG-PCODE                                     
099500              , :MAP-RESP-ADLEG-CITY                                      
099600              , :MAP-RESP-IDLANDX3                                        
099700              , :MAP-RESP-IDTFN                                           
099800              , :MAP-RESP-IDTFX                                           
099900              , :MAP-RESP-IDMAIL                                          
100000              , :MAP-RESP-BECONT                                          
100100              , :MAP-RESP-IDVAT                                           
100200              , :MAP-RESP-IDBG                                            
100300              , :MAP-RESP-IDPG                                            
100400              , :MAP-RESP-KDINVFRQ                                        
100500              , :MAP-RESP-KDAPPEND                                        
100600              , :MAP-RESP-FLSLUT                                          
100700              , :MAP-DAREGDAT                                             
100800              , :MAP-DAUPPDAT                                             
100900              , :MAP-RESP-FLCUSUPD                                        
101000              , :MAP-RESP-FLVATUPD                                        
101010              , :MAP-RESP-KDVALISO                                        
101020              , :MAP-RESP-KDTRADP                                         
101100                                                                          
101200           FROM  T01LSEL                                                  
101300                                                                          
101400           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
101500               AND KDSTATUS = :WS-COMING                                  
101600     END-EXEC                                                             
101700                                                                          
101800     MOVE SQLCODE TO SQLCODE-WS                                           
101900     PERFORM DB2-STATUS-CHECK                                             
102000     .                                                                    
102100 DB2-UPDATE-T01LSEL-TAB-STA2  SECTION.                                    
102200                                                                          
102300     MOVE 000     TO GOOD-SQLCODECODES                                    
102400     EXEC SQL                                                             
102500         UPDATE T01LSEL                                                   
102600             SET   BELEGRAD_1   = :REQU-BELEG-NAME1                       
102700                 , BELEGRAD_2   = :REQU-BELEG-NAME2                       
102800                 , ADLEG_STREET = :REQU-ADLEG-STREET                      
102900                 , ADLEG_BOX    = :REQU-ADLEG-BOX                         
103000                 , ADLEG_PCODE  = :REQU-ADLEG-PCODE                       
103100                 , ADLEG_CITY   = :REQU-ADLEG-CITY                        
103200                 , IDLANDX3     = :REQU-IDLANDX3                          
103300                 , IDTFN        = :REQU-IDTFN                             
103400                 , IDTFX        = :REQU-IDTFX                             
103500                 , IDMAIL       = :REQU-IDMAIL                            
103600                 , BECONT       = :REQU-BECONT                            
103700                 , IDVAT        = :REQU-IDVAT                             
103800                 , IDBG         = :REQU-IDBG                              
103900                 , IDPG         = :REQU-IDPG                              
104000                 , KDINVFRQ     = :REQU-KDINVFRQ                          
104100                 , KDAPPEND     = :REQU-KDAPPEND                          
104200                 , FLSLUT       = :REQU-FLSLUT                            
104300                 , DAREGDAT     = :MAP-DAREGDAT                           
104400                 , DAUPPDAT     = :REQU-DAUPPDAT                          
104500                 , IDUSER       = :REQU-IDUSER                            
104600                 , FLCUSUPD     = :REQU-FLCUSUPD                          
104700                 , FLVATUPD     = :REQU-FLVATUPD                          
104710                 , KDVALISO     = :REQU-KDVALISO                          
104720                 , KDTRADP      = :REQU-KDTRADP                           
104800                                                                          
104900         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
105000             AND KDSTATUS = :WS-COMING                                    
105100     END-EXEC                                                             
105200                                                                          
105300     MOVE SQLCODE TO SQLCODE-WS                                           
105400     PERFORM DB2-STATUS-CHECK                                             
105500     .                                                                    
105600 DB2-INSERT-T01LSEL-TAB-STA1  SECTION.                                    
105700                                                                          
105800     MOVE 000   TO GOOD-SQLCODECODES                                      
105900     EXEC SQL                                                             
106000         INSERT INTO T01LSEL                                              
106100            (IDLEGSEL,KDSTATUS,BELEGRAD_1,BELEGRAD_2                      
106200            ,ADLEG_STREET,ADLEG_BOX,ADLEG_PCODE,ADLEG_CITY                
106300            ,IDLANDX3,IDTFN,IDTFX,IDMAIL,BECONT,IDVAT,IDBG,IDPG           
106400            ,KDINVFRQ,KDAPPEND,FLSLUT,FLCUSUPD,FLVATUPD                   
106500            ,DAREGDAT,DAUPPDAT,IDUSER,FLCURRCL,FLINTODO,KDVALISO          
106510            ,KDTRADP)                                                     
106600         VALUES                                                           
106700            (:REQU-IDLEGSEL-KEY,:WS-CURRENT                               
106800            ,:REQU-BELEG-NAME1,:REQU-BELEG-NAME2                          
106900            ,:REQU-ADLEG-STREET,:REQU-ADLEG-BOX,:REQU-ADLEG-PCODE         
107000            ,:REQU-ADLEG-CITY,:REQU-IDLANDX3                              
107100            ,:REQU-IDTFN,:REQU-IDTFX,:REQU-IDMAIL,:REQU-BECONT            
107200            ,:REQU-IDVAT,:REQU-IDBG,:REQU-IDPG                            
107300            ,:REQU-KDINVFRQ,:REQU-KDAPPEND                                
107400            ,:REQU-FLSLUT,:REQU-FLCUSUPD,:REQU-FLVATUPD                   
107500            ,:MAP-DAREGDAT,:REQU-DAUPPDAT,:REQU-IDUSER,'N','N'            
107600            ,:REQU-KDVALISO,:REQU-KDTRADP)                                
107700     END-EXEC                                                             
107800                                                                          
107900     MOVE SQLCODE TO SQLCODE-WS                                           
108000     PERFORM DB2-STATUS-CHECK                                             
108100     .                                                                    
108200 DB2-INSERT-T01LSEL-TAB-STA2  SECTION.                                    
108300                                                                          
108400     MOVE 000   TO GOOD-SQLCODECODES                                      
108500     EXEC SQL                                                             
108600         INSERT INTO T01LSEL                                              
108700            (IDLEGSEL,KDSTATUS,BELEGRAD_1,BELEGRAD_2                      
108800            ,ADLEG_STREET,ADLEG_BOX,ADLEG_PCODE,ADLEG_CITY                
108900            ,IDLANDX3,IDTFN,IDTFX,IDMAIL,BECONT,IDVAT,IDBG,IDPG           
109000            ,KDINVFRQ,KDAPPEND,FLSLUT,FLCUSUPD,FLVATUPD                   
109100            ,DAREGDAT,DAUPPDAT,IDUSER,FLCURRCL,FLINTODO,KDVALISO          
109110            ,KDTRADP)                                                     
109200         VALUES                                                           
109300            (:REQU-IDLEGSEL-KEY,:WS-COMING                                
109400            ,:REQU-BELEG-NAME1,:REQU-BELEG-NAME2                          
109500            ,:REQU-ADLEG-STREET,:REQU-ADLEG-BOX,:REQU-ADLEG-PCODE         
109600            ,:REQU-ADLEG-CITY,:REQU-IDLANDX3                              
109700            ,:REQU-IDTFN,:REQU-IDTFX,:REQU-IDMAIL,:REQU-BECONT            
109800            ,:REQU-IDVAT,:REQU-IDBG,:REQU-IDPG                            
109900            ,:REQU-KDINVFRQ,:REQU-KDAPPEND                                
110000            ,:REQU-FLSLUT,:REQU-FLCUSUPD,:REQU-FLVATUPD                   
110100            ,:MAP-DAREGDAT,:REQU-DAUPPDAT,:REQU-IDUSER,'N','N'            
110110            ,:REQU-KDVALISO,:REQU-KDTRADP)                                
110200     END-EXEC                                                             
110300                                                                          
110400     MOVE SQLCODE TO SQLCODE-WS                                           
110500     PERFORM DB2-STATUS-CHECK                                             
110600     .                                                                    
110700     EJECT                                                                
110800                                                                          
110900 DB2-INSERT-T01PROC SECTION.                                              
111000     MOVE 000   TO GOOD-SQLCODECODES                                      
111100     EXEC SQL                                                             
111200         INSERT INTO T01PROC                                              
111300            (IDSYSTEM                                                     
111400            ,IDLEGSEL                                                     
111500            ,KDBEH                                                        
111600            ,DAEXDAT                                                      
111700            ,TIEXTID)                                                     
111800         VALUES                                                           
111900            ('WF02'                                                       
112000            ,:REQU-IDLEGSEL-KEY                                           
112100            ,' '                                                          
112200            ,'00000000'                                                   
112300            ,0)                                                           
112400     END-EXEC                                                             
112500                                                                          
112600     MOVE SQLCODE TO SQLCODE-WS                                           
112700     PERFORM DB2-STATUS-CHECK                                             
112800     .                                                                    
112900     EJECT                                                                
113000                                                                          
113010 DB2-SELECT-T01CURR SECTION.                                              
113020     MOVE 000100  TO GOOD-SQLCODECODES                                    
113030                                                                          
113040     EXEC SQL                                                             
113050         SELECT  IDLEGSEL                                                 
113060                ,KDVALISO                                                 
113070                ,DASTADAT                                                 
113080                ,PRKURS                                                   
113090                ,REVALUTA                                                 
113091                ,DAREGDAT                                                 
113092                ,REVALUTA-FROM                                            
113093                ,REVALUTA-TO                                              
113094                ,PRKURS-NEW                                               
113095                ,IDUSER                                                   
113096                                                                          
113097         INTO   :T01CURR-IDLEGSEL                                         
113098               ,:T01CURR-KDVALISO                                         
113099               ,:T01CURR-DASTADAT                                         
113100               ,:T01CURR-PRKURS                                           
113101               ,:T01CURR-REVALUTA                                         
113102               ,:T01CURR-DAREGDAT                                         
113103               ,:T01CURR-REVALUTA-FROM                                    
113104               ,:T01CURR-REVALUTA-TO                                      
113105               ,:T01CURR-PRKURS-NEW                                       
113106               ,:T01CURR-IDUSER                                           
113107                                                                          
113108         FROM    T01CURR                                                  
113109                                                                          
113110         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
113111         AND     KDVALISO = :REQU-KDVALISO                                
113112         AND     DASTADAT = :WS-DASTADAT-KEY                              
113113     END-EXEC                                                             
113114                                                                          
113115     MOVE SQLCODE TO SQLCODE-WS                                           
113116     PERFORM DB2-STATUS-CHECK                                             
113117     .                                                                    
113118     EJECT                                                                
113119                                                                          
113120 DB2-INSERT-T01CURR SECTION.                                              
113121     MOVE 000   TO GOOD-SQLCODECODES                                      
113122                                                                          
113123     EXEC SQL                                                             
113124         INSERT                                                           
113125                                                                          
113126         INTO     T01CURR                                                 
113127                                                                          
113128                 (IDLEGSEL                                                
113129                 ,KDVALISO                                                
113130                 ,DASTADAT                                                
113131                 ,PRKURS                                                  
113132                 ,REVALUTA                                                
113133                 ,DAREGDAT                                                
113134                 ,REVALUTA-FROM                                           
113135                 ,REVALUTA-TO                                             
113136                 ,PRKURS-NEW                                              
113137                 ,IDUSER)                                                 
113138                                                                          
113139         VALUES (:REQU-IDLEGSEL-KEY                                       
113140                ,:REQU-KDVALISO                                           
113141                ,:WS-DASTADAT-KEY                                         
113142                ,'1'                                                      
113143                ,'1'                                                      
113144                ,:WS-DATUM                                                
113145                ,'1'                                                      
113146                ,'1'                                                      
113147                ,'1'                                                      
113148                ,:REQU-IDUSER)                                            
113149     END-EXEC                                                             
113150                                                                          
113151     MOVE SQLCODE TO SQLCODE-WS                                           
113152     PERFORM DB2-STATUS-CHECK                                             
113153     .                                                                    
113154     EJECT                                                                
113155                                                                          
113160 DB2-STATUS-CHECK  SECTION.                                               
113200                                                                          
113300     SET SQLCODE-IX TO 1                                                  
113400     SEARCH GOOD-SQLCODE                                                  
113500       AT END                                                             
113600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
113700          DELIMITED BY SIZE INTO ERROR-TEXT                               
113800          CALL ABEND USING RKOD-ABEND-DB2                                 
113900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
114000     END-SEARCH                                                           
114100     .                                                                    
