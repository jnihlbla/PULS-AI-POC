000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034710.                                                
000300 AUTHOR.         ABRAHAMSON SHARON.                                       
000400 DATE-WRITTEN.   96/11/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM ALLOWS THE USER TO CHANGE THE TRANSPORT             
000900*        NUMBER FOR ALL OR CHOSEN CASES WITHIN AN ORDER.                  
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*        THE PROGRAM READS     WDE4A                                      
001400*        THE PROGRAM READS     WDE6                                       
001500*        THE PROGRAM READS     WDGX4402 (WDR1)                            
001600*        THE PROGRAM UPDATES   WDE6                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: W4T347                                              
002000*        MID:        W4I34701                                             
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        MOD:         W4O34701                                            
002400                                                                          
002500* CHANGE LOG:                                                             
002600*                                                                         
002700*      28/05/21 - LOVISH KANSAL   - NEW PROG WITH BUSINES LOGIC.          
002800*                                   COMMON FOR CLASSIC (W4034700)         
002900*                                   AND WEB (W4W34700) INTERFACE.         
003000*                                                                         
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4034710'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
004200                                                                          
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*    --- INDEX FOR SCROLL LINES                                           
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900 77  WS-IDPRODNR                 PIC S9(7)  VALUE ZERO COMP-3.            
005000 77  WS-IDKOLLI                  PIC S9(5)  VALUE ZERO COMP-3.            
005100                                                                          
005200 01    WS-IDDISTR                         PIC X(4).                       
005300 01    IDDISTR-WS REDEFINES WS-IDDISTR    PIC 9(4).                       
005400     SKIP2                                                                
005500 01    WS-IDKUNDNR                        PIC X(6).                       
005600 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR  PIC 9(6).                       
005700     SKIP2                                                                
005800 01    WS-IDORDNR                         PIC X(5).                       
005900 01    IDORDNR-WS REDEFINES WS-IDORDNR    PIC 9(5).                       
006000 01  ALL-SPACE.                                                           
006100     03 FILLER                   PIC X(50)     VALUE SPACE.               
006200 01  ALL-PLUS.                                                            
006300     03 FILLER                   PIC X(50)     VALUE ALL '+'.             
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006600     88  INDATA-OK                           VALUE 'Y'.                   
006700     88  INDATA-WRONG                        VALUE 'N'.                   
006800                                                                          
006900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007000     88  KEYS-OK                             VALUE 'Y'.                   
007100     88  KEYS-WRONG                          VALUE 'N'.                   
007200                                                                          
007300 77  UPDATE-SW                   PIC X       VALUE 'Y'.                   
007400     88  UPDATE-E611                         VALUE 'Y'.                   
007500                                                                          
007600 77  DATA-ENTERED-SW             PIC X       VALUE 'N'.                   
007700     88  DATA-ENTERED-YES                    VALUE 'J'.                   
007800     88  DATA-ENTERED-NO                     VALUE 'N'.                   
007900                                                                          
008000 01  SW-CHANGE-ALL-CASES         PIC X       VALUE 'N'.                   
008100 01  SW-DO-NOT-CONTINUE          PIC X       VALUE 'N'.                   
008200 01  SW-DKO                      PIC X       VALUE 'Y'.                   
008300 01  W-IDDISTR                   PIC 9(4).                                
008400 01  W-IDKUNDNR                  PIC 9(6).                                
008500 01  W-IDKUNDRF                  PIC 9(5).                                
008600                                                                          
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERAL-SUBPROGRAM.                                                  
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     EJECT                                                                
009200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009300*01 -COPY WMEDAREA                                                        
009400     SKIP3                                                                
009500 01  MESSAGE-CODES.                                                       
009600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
009700     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
009800     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '025'.                 
009900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
010100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
010200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
010300     03  INF-LAST-PAGE           PIC X(3)    VALUE '012'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010500     03  ERR-NUMERIC-FLDS        PIC X(3)    VALUE '024'.                 
010600     03  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.                 
010700     03  ERR-TRANSPORT-MISSING   PIC X(3)    VALUE '087'.                 
010800     EJECT                                                                
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011100     SKIP3                                                                
011200*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011900 01  KEYS-TO-DLI.                                                         
012000*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
012100                                                                          
012200     03  W-WDE4A1KY-MIN-X.                                                
012300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
012400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
012500         05  W-IDKUNDRF-MIN      PIC X(5)    VALUE SPACE.                 
012600         05  W-IDKUNDRF-MIN-2    PIC X(5)    VALUE SPACE.                 
012700         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
012800         05  W-IDPLKLST-MIN      PIC S9(3)   VALUE ZERO COMP-3.           
012900                                                                          
013000     03  W-WDE4A1KY-MAX-X.                                                
013100         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
013200         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
013300         05  W-IDKUNDRF-MAX      PIC X(5)    VALUE SPACE.                 
013400         05  W-IDKUNDRF-MAX-2    PIC X(5)    VALUE SPACE.                 
013500         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
013600         05  W-IDPLKLST-MAX      PIC S9(3)   VALUE ZERO COMP-3.           
013700                                                                          
013800     03  W-WDE420-KEYSEQ-MIN-X.                                           
013900         05  W-420-IDPRODNR-MIN        PIC S9(7)    COMP-3.               
014000         05  W-420-IDPURAD-MIN         PIC S9(5)    COMP-3.               
014100     SKIP2                                                                
014200     03  W-WDE420-KEYSEQ-MAX-X.                                           
014300         05  W-420-IDPRODNR-MAX        PIC S9(7)    COMP-3.               
014400         05  W-420-IDPURAD-MAX         PIC S9(5)    COMP-3.               
014500                                                                          
014600     03  W-IDPLKLST-X.                                                    
014700          05  W-IDPLKLST         PIC S9(3)   COMP-3 VALUE ZERO.           
014800                                                                          
014900     03  W-IDPRODNR-X.                                                    
015000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
015100     03  W-IDKOLLI-X.                                                     
015200         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
015300     SKIP2                                                                
015400     03  W-4401-WDGXKEY-X.                                                
015500         05  FILLER                PIC X(4)  VALUE '4401'.                
015600         05  FILLER                PIC X(26) VALUE LOW-VALUE.             
015700                                                                          
015800     03  W-4402-KY4402-MIN-X.                                             
015900         05  W-4402-IDDC-MIN       PIC X(2).                              
016000         05  W-4402-IDDISTR-MIN    PIC S9(5)   COMP-3.                    
016100         05  W-4402-IDKUNDNR-MIN   PIC S9(7)   COMP-3.                    
016200         05  W-4402-KDFRAKT-MIN    PIC S9(3)   COMP-3.                    
016300         05  W-4402-KDORDKLX-MIN   PIC X(1).                              
016400                                                                          
016500     03  W-4402-KY4402-MAX-X.                                             
016600         05  W-4402-IDDC-MAX       PIC X(2).                              
016700         05  W-4402-IDDISTR-MAX    PIC S9(5)   COMP-3.                    
016800         05  W-4402-IDKUNDNR-MAX   PIC S9(7)   COMP-3.                    
016900         05  W-4402-KDFRAKT-MAX    PIC S9(3)   COMP-3.                    
017000         05  FILLER                PIC X(1)    VALUE HIGH-VALUE.          
017100                                                                          
017200     03  W-4402-IDTRPTNR-X.                                               
017300         05  W-4402-IDTRPTNR     PIC S9(3)   COMP-3.                      
017400                                                                          
017500*    --- STATUS-KOD FRÅN IMS                                              
017600 01  STATUS-WS                   PIC XX.                                  
017700     88  SEGMENT-FOUND                       VALUE '  '.                  
017800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018000     SKIP2                                                                
018100 01  GOOD-STATUSCODES.                                                    
018200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018300     SKIP3                                                                
018400 01  SSA1                        PIC X(96).                               
018500 01  SSA2                        PIC X(96).                               
018600     EJECT                                                                
018700*    --- IMS FUNCTION CODES                                               
018800*01  -COPY W0003                                                          
018900     EJECT                                                                
019000*    ---  DLI INPUT-OUTPUT AREA                                           
019100                                                                          
019200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE4A01'.                     
019300 01  DLI-IO-WDE4A01.                                                      
019400*    03  -COPY WDE4A1  -PRE WDE4A-                                        
019500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE601'.                      
019600 01  DLI-IO-WDE601.                                                       
019700*    03  -COPY WDE601  -PRE WDE6-                                         
019800     EJECT                                                                
019900 01  FILLER         PIC X(24) VALUE 'DLI-IO-XXDM'.                        
020000 01  DLI-IO-XXDM.                                                         
020100*    03  -COPY WDGX4402                                                   
020200     EJECT                                                                
020300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE611'.                      
020400 01  DLI-IO-WDE611.                                                       
020500*    03  -COPY WDE611  -PRE WDE6-                                         
020600     EJECT                                                                
020700 01    DLI-IO-AREA.                                                       
020800   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
020900     SKIP2                                                                
021000*  03    WDE401   -COPY WDE401              -RED IO-AREA.                 
021100     SKIP2                                                                
021200*  03    WDE411   -COPY WDE411              -RED IO-AREA.                 
021300     SKIP2                                                                
021400*  03    WDE601   -COPY WDE601              -RED IO-AREA.                 
021500 LINKAGE SECTION.                                                         
021600 01  REQU-AREA.                                                           
021700*    03 -COPY WZ01REQU                                                    
021800*    03 -COPY W40347I1                                                    
021900     EJECT                                                                
022000 01  RESP-AREA.                                                           
022100*    03 -COPY WZ01RESP                                                    
022200*    03 -COPY W40347O1                                                    
022300     EJECT                                                                
022400 01  MAX-KVRADER                 PIC S9(4) COMP.                          
022500*                                                                         
022600*01  -COPY W0008  -PRE WDE4A-                                             
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE WDE43-                                             
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDE6-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE XXDM-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
023900                           WDE4A-PCB WDE43-PCB WDE6-PCB XXDM-PCB.         
024000 MAIN SECTION.                                                            
024100     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
024200                           WDE4A-PCB WDE43-PCB WDE6-PCB XXDM-PCB.         
024300                                                                          
024400     PERFORM A-INIT                                                       
024500     PERFORM B-CHECK-KEYS                                                 
024600     IF KEYS-OK                                                           
024700       IF REQU-UPDATE                                                     
024800         PERFORM G-CHECK-INPUT                                            
024900         IF INDATA-OK                                                     
025000           PERFORM H-UPDATE                                               
025100         END-IF                                                           
025200       ELSE                                                               
025300         IF REQU-FIRST                                                    
025400           PERFORM C-FIRST-PAGE                                           
025500         ELSE                                                             
025600           IF REQU-NEXT                                                   
025700             PERFORM D-NEXT-PAGE                                          
025800           ELSE                                                           
025900             PERFORM E-SAME-PAGE                                          
026000           END-IF                                                         
026100         END-IF                                                           
026200       END-IF                                                             
026300                                                                          
026400       IF SEGMENT-MISSING                                                 
026500         CONTINUE                                                         
026600       ELSE                                                               
026700            PERFORM F-READ-SHOW-INFO                                      
026800       END-IF                                                             
026900     END-IF                                                               
027000                                                                          
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     IF REQU-KVRADER NOT NUMERIC                                          
027700        MOVE ZERO TO REQU-KVRADER                                         
027800     END-IF                                                               
027900                                                                          
028000     MOVE ALL '+'                TO RESP-W40347O1                         
028100                                                                          
028200     MOVE 001                    TO RESP-IDMSGVER                         
028300     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
028400                                    RESP-IDMSG-INFO                       
028500                                    RESP-IDELMT-ERROR                     
028600                                                                          
028700     MOVE REQU-KVRADER           TO RESP-KVRADER                          
028800     MOVE REQU-IDPRODNR-START    TO RESP-IDPRODNR-START                   
028900     MOVE REQU-IDKOLLI-START     TO RESP-IDKOLLI-START                    
029000     MOVE ZERO                   TO RESP-IDKOLLI-NEXT                     
029100                                                                          
029200     MOVE 'N'                    TO SW-CHANGE-ALL-CASES                   
029300                                                                          
029400     PERFORM MFS-ERASE-LINE-FIELD-OUT                                     
029500     PERFORM MFS-FORM-ATTR                                                
029600                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 B-CHECK-KEYS SECTION.                                                    
030000                                                                          
030100     MOVE LOW-VALUE            TO W-4402-KY4402-MIN-X                     
030200     MOVE HIGH-VALUE           TO W-4402-KY4402-MAX-X                     
030300                                                                          
030400     MOVE LOW-VALUE   TO W-WDE4A1KY-MIN-X                                 
030500     MOVE HIGH-VALUE  TO W-WDE4A1KY-MAX-X                                 
030600     MOVE YES TO KEYS-SW                                                  
030700                                                                          
030800     IF REQU-IDPRODNR-KEY NUMERIC AND REQU-IDPRODNR-KEY > ZERO            
030900        MOVE NOO   TO SW-DKO                                              
031000     ELSE                                                                 
031100*    -- CHECK OF DISTR                                                    
031200       IF REQU-IDDISTR-KEY   NUMERIC AND REQU-IDDISTR-KEY >  ZERO         
031300       AND REQU-IDKUNDNR-KEY NUMERIC AND REQU-IDKUNDNR-KEY > ZERO         
031400       AND REQU-IDORDNR-KEY  NUMERIC AND REQU-IDORDNR-KEY >  ZERO         
031500         CONTINUE                                                         
031600       ELSE                                                               
031700        MOVE NOO   TO KEYS-SW                                             
031800       END-IF                                                             
031900     END-IF                                                               
032000                                                                          
032100     IF KEYS-WRONG                                                        
032200       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
032300       MOVE 'KEY'      TO RESP-IDELMT-ERROR                               
032400       MOVE ZERO                 TO RESP-KVRADER                          
032500       PERFORM MFS-ERASE-FIELD-IN                                         
032600       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
032700     ELSE                                                                 
032800        IF SW-DKO = 'N'                                                   
032900          MOVE REQU-IDPRODNR-KEY TO WS-IDPRODNR                           
033000        ELSE                                                              
033100         MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                             
033200                                    IDDISTR-WS                            
033300         MOVE REQU-IDKUNDNR-KEY  TO W-IDKUNDNR                            
033400                                    IDKUNDNR-WS                           
033500         MOVE REQU-IDORDNR-KEY   TO W-IDKUNDRF                            
033600                                    IDORDNR-WS                            
033700        END-IF                                                            
033800        IF SW-DKO = 'N'                                                   
033900          IF REQU-IDDISTR-KEY NOT = ALL '+'                               
034000            MOVE REQU-IDDISTR-KEY TO W-4402-IDDISTR-MIN                   
034100                                        W-4402-IDDISTR-MAX                
034200          END-IF                                                          
034300*         MOVE ZERO              TO W-4402-IDKUNDNR-MIN                   
034400*                                     W-4402-IDKUNDNR-MAX                 
034500          IF REQU-IDKUNDNR-KEY NOT = ALL '+'                              
034600            MOVE REQU-IDKUNDNR-KEY TO W-4402-IDKUNDNR-MIN                 
034700                                        W-4402-IDKUNDNR-MAX               
034800          END-IF                                                          
034900                                                                          
035000          MOVE REQU-IDDC-KEY     TO W-4402-IDDC-MIN                       
035100                                     W-4402-IDDC-MAX                      
035200          CONTINUE                                                        
035300        ELSE                                                              
035400          MOVE REQU-IDDISTR-KEY  TO W-4402-IDDISTR-MIN                    
035500                                      W-4402-IDDISTR-MAX                  
035600*         MOVE ZERO              TO W-4402-IDKUNDNR-MIN                   
035700*                                     W-4402-IDKUNDNR-MAX                 
035800         MOVE REQU-IDKUNDNR-KEY TO W-4402-IDKUNDNR-MIN                    
035900                                      W-4402-IDKUNDNR-MAX                 
036000          MOVE REQU-IDDC-KEY     TO W-4402-IDDC-MIN                       
036100                                     W-4402-IDDC-MAX                      
036200        END-IF                                                            
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 C-FIRST-PAGE SECTION.                                                    
036700                                                                          
036800     MOVE INF-FIRST-PAGE      TO RESP-IDMSG-ERROR                         
036900     PERFORM MFS-ERASE-FIELD-IN                                           
037000     .                                                                    
037100     EJECT                                                                
037200 D-NEXT-PAGE SECTION.                                                     
037300                                                                          
037400     MOVE REQU-IDPRODNR-START TO WS-IDPRODNR                              
037500                                 W-IDPRODNR                               
037600     MOVE REQU-IDKOLLI-START  TO WS-IDKOLLI                               
037700                                 W-IDKOLLI                                
037800     PERFORM MFS-ERASE-FIELD-IN                                           
037900     .                                                                    
038000     EJECT                                                                
038100 E-SAME-PAGE SECTION.                                                     
038200                                                                          
038300     IF REQU-IDPRODNR-START NOT = ALL '+' AND                             
038400        REQU-IDPRODNR-START IS NUMERIC                                    
038500       MOVE REQU-IDPRODNR-START  TO WS-IDPRODNR                           
038600     END-IF                                                               
038700                                                                          
038800     IF REQU-IDKOLLI-START NOT = ALL '+' AND                              
038900        REQU-IDKOLLI-START IS NUMERIC                                     
039000       MOVE REQU-IDKOLLI-START   TO WS-IDKOLLI                            
039100     END-IF                                                               
039200                                                                          
039300     PERFORM EB-CHECK-INPUT-FIELDS                                        
039400                                                                          
039500     IF DATA-ENTERED-YES                                                  
039600       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
039700       PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                  
039800       PERFORM MFS-READ-IN-AGAIN                                          
039900     ELSE                                                                 
040000       PERFORM MFS-ERASE-FIELD-IN                                         
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 EB-CHECK-INPUT-FIELDS SECTION.                                           
040500                                                                          
040600     IF REQU-FLTRPTCHG      = ALL '+' AND                                 
040700        REQU-IDTRPTNR       = ALL '+' AND                                 
040800        REQU-KDFRAKT        = ALL '+'                                     
040900       SET DATA-ENTERED-NO       TO TRUE                                  
041000     ELSE                                                                 
041100       IF REQU-FLTRPTCHG    = ALL '+' OR                                  
041200          REQU-IDTRPTNR     = ALL '+' OR                                  
041300          REQU-KDFRAKT      = ALL '+'                                     
041400         SET DATA-ENTERED-NO     TO TRUE                                  
041500       ELSE                                                               
041600         SET DATA-ENTERED-YES    TO TRUE                                  
041700       END-IF                                                             
041800     END-IF                                                               
041900                                                                          
042000     PERFORM                                                              
042100     VARYING INDX FROM +1 BY +1                                           
042200       UNTIL INDX > REQU-KVRADER OR                                       
042300             DATA-ENTERED-YES                                             
042400       IF REQU-VALFLAGGA-LINE (INDX) = ALL '+'                            
042500         CONTINUE                                                         
042600       ELSE                                                               
042700         SET DATA-ENTERED-YES    TO TRUE                                  
042800       END-IF                                                             
042900     END-PERFORM                                                          
043000     .                                                                    
043100     EJECT                                                                
043200 F-READ-SHOW-INFO SECTION.                                                
043300                                                                          
043400     IF WS-IDPRODNR > ZERO                                                
043500           MOVE WS-IDPRODNR TO W-420-IDPRODNR-MIN                         
043600                               W-420-IDPRODNR-MAX                         
043700           MOVE 1           TO W-420-IDPURAD-MIN                          
043800           MOVE 99999       TO W-420-IDPURAD-MAX                          
043900           PERFORM IMS-GU-KUNDORDER-SEK-INV-INT                           
044000           IF SEGMENT-FOUND                                               
044100              MOVE KORD-IDDISTR   TO IDDISTR-WS                           
044200                                     W-4402-IDDISTR-MIN                   
044300                                     W-4402-IDDISTR-MAX                   
044400              MOVE KORD-IDKUNDNR  TO IDKUNDNR-WS                          
044500                                     W-4402-IDKUNDNR-MIN                  
044600                                     W-4402-IDKUNDNR-MAX                  
044700              MOVE KORD-IDORDNR5  TO IDORDNR-WS                           
044800           END-IF                                                         
044900     ELSE                                                                 
045000        PERFORM FA-READ-BASICDATA                                         
045100     END-IF                                                               
045200                                                                          
045300     MOVE WS-IDKOLLI     TO W-IDKOLLI                                     
045400     MOVE WS-IDPRODNR    TO W-IDPRODNR                                    
045500                            RESP-IDPRODNR-UT                              
045600     MOVE IDDISTR-WS     TO RESP-IDDISTR-UT                               
045700     MOVE IDKUNDNR-WS    TO RESP-IDKUNDNR-UT                              
045800     MOVE IDORDNR-WS     TO RESP-IDORDNR-UT                               
045900     INSPECT RESP-IDPRODNR-UT  REPLACING LEADING ZERO BY SPACE            
046000     INSPECT RESP-IDDISTR-UT   REPLACING LEADING ZERO BY SPACE            
046100     INSPECT RESP-IDKUNDNR-UT  REPLACING LEADING ZERO BY SPACE            
046200     INSPECT RESP-IDORDNR-UT   REPLACING LEADING ZERO BY SPACE            
046300                                                                          
046400     IF INDATA-OK                                                         
046500                                                                          
046600     MOVE ZERO           TO RESP-KVRADER                                  
046700     PERFORM IMS-GU-WDE601                                                
046800                                                                          
046900     IF SEGMENT-MISSING                                                   
047000        MOVE INF-URVAL-SAKNAS   TO RESP-IDMSG-INFO                        
047100        MOVE 'KEY'              TO RESP-IDELMT-ERROR                      
047200     ELSE                                                                 
047300       IF WDE6-VORD-IDDC = REQU-IDDC-KEY                                  
047400        MOVE +1 TO INDX                                                   
047500        PERFORM FB-READ-LINEDATA                                          
047600                                                                          
047700        IF SEGMENT-FOUND                                                  
047800          MOVE WDE6-VORD-IDPRODNR TO RESP-IDPRODNR-START                  
047900          MOVE WDE6-KOLLI-IDKOLLI TO RESP-IDKOLLI-START                   
048000        ELSE                                                              
048100          MOVE WS-IDPRODNR        TO RESP-IDPRODNR-START                  
048200          MOVE WS-IDKOLLI         TO RESP-IDKOLLI-START                   
048300        END-IF                                                            
048400                                                                          
048500        PERFORM UNTIL INDX > MAX-KVRADER                                  
048600          IF SEGMENT-FOUND                                                
048700             IF WDE6-KOLLI-KDKOLSTA = 1                                   
048800               MOVE WDE6-KOLLI-IDKOLLI  TO                                
048900                                        RESP-IDKOLLI-LINE (INDX)          
049000                                        W-IDKOLLI                         
049100               MOVE WDE6-KOLLI-IDTRPTNR TO                                
049200                                        RESP-IDTRPTNR-OLD (INDX)          
049300               MOVE WDE6-VORD-KDFRAKT   TO                                
049400                                        RESP-KDFRAKT-OLD  (INDX)          
049500               ADD 1 TO INDX                                              
049600                        RESP-KVRADER                                      
049700             END-IF                                                       
049800             PERFORM IMS-GNP-WDE611-ST                                    
049900          ELSE                                                            
050000             MOVE MFS-ERASE-FIELD TO RESP-IDKOLLI-LINE (INDX)             
050100             ADD 1 TO INDX                                                
050200          END-IF                                                          
050300        END-PERFORM                                                       
050400                                                                          
050500         IF SEGMENT-FOUND                                                 
050600           MOVE WDE6-KOLLI-IDKOLLI TO RESP-IDKOLLI-NEXT                   
050700           IF RESP-IDMSG-INFO = SPACE                                     
050800           IF NOT REQU-UPDATE                                             
050900             MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                 
051000             MOVE '*'                  TO RESP-IDELMT-ERROR               
051100           END-IF                                                         
051200           END-IF                                                         
051300         ELSE                                                             
051400           IF RESP-IDMSG-INFO = SPACE                                     
051500             MOVE RESP-IDKOLLI-START   TO RESP-IDKOLLI-NEXT               
051600             MOVE INF-LAST-PAGE        TO RESP-IDMSG-INFO                 
051700             MOVE '*'                  TO RESP-IDELMT-ERROR               
051800           END-IF                                                         
051900         END-IF                                                           
052000       ELSE                                                               
052100         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
052200         PERFORM MFS-ERASE-FIELD-IN                                       
052300       END-IF                                                             
052400                                                                          
052500     END-IF                                                               
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 FA-READ-BASICDATA SECTION.                                               
053000                                                                          
053100     MOVE W-IDDISTR  TO W-IDDISTR-MIN                                     
053200     MOVE W-IDKUNDNR TO W-IDKUNDNR-MIN                                    
053300     MOVE W-IDKUNDRF TO W-IDKUNDRF-MIN                                    
053400     MOVE W-IDDISTR  TO W-IDDISTR-MAX                                     
053500     MOVE W-IDKUNDNR TO W-IDKUNDNR-MAX                                    
053600     MOVE W-IDKUNDRF TO W-IDKUNDRF-MAX                                    
053700     PERFORM IMS-GN-WDE4A1                                                
053800                                                                          
053900     IF SEGMENT-MISSING                                                   
054000        MOVE INF-URVAL-SAKNAS   TO RESP-IDMSG-INFO                        
054100        MOVE 'KEY'              TO RESP-IDELMT-ERROR                      
054200     ELSE                                                                 
054300                                                                          
054400       MOVE WDE4A-SEQA-IDPRODNR TO W-IDPRODNR                             
054500                                  WS-IDPRODNR                             
054600       PERFORM IMS-GU-WDE601                                              
054700         IF SEGMENT-MISSING                                               
054800            MOVE INF-URVAL-SAKNAS TO RESP-IDMSG-INFO                      
054900            MOVE 'KEY'            TO RESP-IDELMT-ERROR                    
055000         ELSE                                                             
055100                                                                          
055200               MOVE WDE4A-SEQA-IDPRODNR TO W-IDPRODNR                     
055300                                          WS-IDPRODNR                     
055400                                          W-IDPRODNR-MIN                  
055500           PERFORM UNTIL WDE6-VORD-IDDC = REQU-IDDC-KEY AND               
055600                         WDE6-VORD-FLDIRLEV = NOO      OR                 
055700                         SW-DO-NOT-CONTINUE = 'Y'                         
055800             PERFORM IMS-GN-WDE4A1                                        
055900             IF STATUS-WS = SPACE                                         
056000               MOVE WDE4A-SEQA-IDPRODNR TO W-IDPRODNR                     
056100                                          WS-IDPRODNR                     
056200               PERFORM IMS-GU-WDE601                                      
056300             ELSE                                                         
056400               MOVE INF-URVAL-SAKNAS TO RESP-IDMSG-INFO                   
056500               MOVE 'KEY'            TO RESP-IDELMT-ERROR                 
056600               MOVE 'Y' TO SW-DO-NOT-CONTINUE                             
056700             END-IF                                                       
056800           END-PERFORM                                                    
056900         END-IF                                                           
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300                                                                          
057400 FB-READ-LINEDATA SECTION.                                                
057500                                                                          
057600     IF REQU-UPDATE                                                       
057700       IF REQU-IDMSGVER = '101'                                           
057800          PERFORM IMS-GNP-WDE611-NEXT                                     
057900       ELSE                                                               
058000           PERFORM IMS-GNP-WDE611                                         
058100       END-IF                                                             
058200     ELSE                                                                 
058300       IF REQU-NEXT                                                       
058400         PERFORM IMS-GNP-WDE611-NEXT                                      
058500       ELSE                                                               
058600         IF REQU-FIRST                                                    
058700           PERFORM IMS-GNP-WDE611                                         
058800         ELSE                                                             
058900           IF REQU-IDMSGVER = '101'                                       
059000              PERFORM IMS-GNP-WDE611-NEXT                                 
059100           ELSE                                                           
059200              PERFORM IMS-GNP-WDE611                                      
059300           END-IF                                                         
059400         END-IF                                                           
059500       END-IF                                                             
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 G-CHECK-INPUT SECTION.                                                   
060000                                                                          
060100     MOVE YES  TO INDATA-SW                                               
060200     PERFORM GB-CHECK-INPUT-FIELDS                                        
060300     IF DATA-ENTERED-NO                                                   
060400*      MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
060500       MOVE ERR-MUST-BE-ENTERED  TO RESP-IDMSG-ERROR                      
060600*      MOVE 'KEY'               TO RESP-IDELMT-ERROR                      
060700       PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                  
060800       PERFORM MFS-READ-IN-AGAIN                                          
060900       MOVE NOO TO INDATA-SW                                              
061000     ELSE                                                                 
061100       IF REQU-FLTRPTCHG = ALL '+'                                        
061200         MOVE MFS-NUM-FIELD-OK TO RESP-FLTRPTCHG-ATTRIB                   
061300       ELSE                                                               
061400          IF REQU-FLTRPTCHG = 'J'                                         
061500          OR REQU-FLTRPTCHG = 'Y'                                         
061600            MOVE 'Y' TO SW-CHANGE-ALL-CASES                               
061700            MOVE MFS-ALPHA-FIELD-OK TO RESP-FLTRPTCHG-ATTRIB              
061800          ELSE                                                            
061900             IF INDATA-OK                                                 
062000               MOVE MFS-ALPHA-FIELD-WRONG TO RESP-FLTRPTCHG-ATTRIB        
062100               MOVE NOO TO INDATA-SW                                      
062200             END-IF                                                       
062300          END-IF                                                          
062400       END-IF                                                             
062500                                                                          
062600       IF REQU-IDTRPTNR = ALL '+'                                         
062700         MOVE MFS-NUM-FIELD-OK TO RESP-IDTRPTNR-ATTRIB                    
062800       ELSE                                                               
062900         IF REQU-IDTRPTNR NOT NUMERIC                                     
063000           IF INDATA-OK                                                   
063100             MOVE MFS-NUM-FIELD-WRONG TO RESP-IDTRPTNR-ATTRIB             
063200             MOVE ERR-NUMERIC-FLDS    TO RESP-IDMSG-ERROR                 
063300*            MOVE 'IDTRPTNR'    TO RESP-IDELMT-ERROR                      
063400             MOVE NOO TO INDATA-SW                                        
063500           END-IF                                                         
063600         END-IF                                                           
063700       END-IF                                                             
063800                                                                          
063900       IF REQU-KDFRAKT = ALL '+'                                          
064000         MOVE MFS-NUM-FIELD-OK TO RESP-KDFRAKT-ATTRIB                     
064100       ELSE                                                               
064200         IF REQU-KDFRAKT NOT NUMERIC                                      
064300           IF INDATA-OK                                                   
064400             MOVE MFS-NUM-FIELD-WRONG TO RESP-KDFRAKT-ATTRIB              
064500             MOVE ERR-NUMERIC-FLDS    TO RESP-IDMSG-ERROR                 
064600*            MOVE 'KDFRAKT'     TO RESP-IDELMT-ERROR                      
064700             MOVE NOO TO INDATA-SW                                        
064800           END-IF                                                         
064900         END-IF                                                           
065000       END-IF                                                             
065100                                                                          
065200       IF (REQU-FLTRPTCHG NOT = ALL '+'                                   
065300       AND REQU-IDTRPTNR     = ALL '+'                                    
065400       AND REQU-KDFRAKT      = ALL '+')                                   
065500       OR (REQU-FLTRPTCHG    = ALL '+'                                    
065600       AND REQU-IDTRPTNR NOT = ALL '+'                                    
065700       AND REQU-KDFRAKT  NOT = ALL '+')                                   
065800         IF INDATA-OK                                                     
065900           MOVE MFS-ALPHA-FIELD-WRONG TO RESP-FLTRPTCHG-ATTRIB            
066000           MOVE MFS-NUM-FIELD-WRONG   TO RESP-IDTRPTNR-ATTRIB             
066100           MOVE MFS-NUM-FIELD-WRONG   TO RESP-KDFRAKT-ATTRIB              
066200*          MOVE 'KDFRAKT'       TO RESP-IDELMT-ERROR                      
066300           MOVE NOO TO INDATA-SW                                          
066400         END-IF                                                           
066500       ELSE                                                               
066600           MOVE MFS-ALPHA-FIELD-OK    TO RESP-FLTRPTCHG-ATTRIB            
066700           MOVE MFS-NUM-FIELD-OK      TO RESP-IDTRPTNR-ATTRIB             
066800           MOVE MFS-NUM-FIELD-OK      TO RESP-KDFRAKT-ATTRIB              
066900           IF SW-CHANGE-ALL-CASES = YES                                   
067000             IF REQU-KDFRAKT = ALL '+'                                    
067100               MOVE MFS-NUM-FIELD-WRONG TO RESP-KDFRAKT-ATTRIB            
067200               MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR                  
067300*              MOVE 'KDFRAKT'   TO RESP-IDELMT-ERROR                      
067400               MOVE NOO TO INDATA-SW                                      
067500*              CONTINUE                                                   
067600             ELSE                                                         
067700               IF REQU-KDFRAKT NOT NUMERIC                                
067800                 IF INDATA-OK                                             
067900                   MOVE MFS-NUM-FIELD-WRONG TO RESP-KDFRAKT-ATTRIB        
068000                   MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR              
068100*                  MOVE 'KDFRAKT' TO RESP-IDELMT-ERROR                    
068200                   MOVE NOO TO INDATA-SW                                  
068300                 END-IF                                                   
068400               ELSE                                                       
068500                 MOVE REQU-KDFRAKT    TO W-4402-KDFRAKT-MIN               
068600                                         W-4402-KDFRAKT-MAX               
068700               END-IF                                                     
068800             END-IF                                                       
068900                                                                          
069000             IF REQU-IDTRPTNR = ALL '+'                                   
069100               MOVE MFS-NUM-FIELD-WRONG TO RESP-IDTRPTNR-ATTRIB           
069200               MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR                  
069300*              MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                       
069400               MOVE NOO TO INDATA-SW                                      
069500             ELSE                                                         
069600               IF REQU-IDTRPTNR NOT NUMERIC                               
069700                 MOVE MFS-NUM-FIELD-WRONG TO RESP-IDTRPTNR-ATTRIB         
069800                 MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR                
069900*                MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                     
070000                 MOVE NOO TO INDATA-SW                                    
070100               ELSE                                                       
070200                 MOVE REQU-IDTRPTNR   TO W-4402-IDTRPTNR                  
070300               END-IF                                                     
070400             END-IF                                                       
070500             IF REQU-IDDC-KEY = '41' OR '43' OR '61' OR '67'              
070600               CONTINUE                                                   
070700             ELSE                                                         
070800               PERFORM GC-CHECK-IDTRPTNR                                  
070900             END-IF                                                       
071000           END-IF                                                         
071100       END-IF                                                             
071200                                                                          
071300       MOVE +1 TO INDX                                                    
071400       PERFORM UNTIL INDX > REQU-KVRADER OR INDATA-WRONG                  
071500         IF REQU-VALFLAGGA-LINE (INDX) = ALL '+'                          
071600           IF REQU-IDTRPTNR-LINE (INDX) NOT = ALL '+' AND                 
071700              REQU-KDFRAKT-LINE (INDX) NOT = ALL '+'                      
071800             IF INDATA-OK                                                 
071900               MOVE MFS-ALPHA-FIELD-WRONG TO                              
072000                   RESP-VALFLAGGA-LINE-ATTR (INDX)                        
072100               MOVE NOO TO INDATA-SW                                      
072200             END-IF                                                       
072300           ELSE                                                           
072400             MOVE MFS-ALPHA-FIELD-OK TO                                   
072500                              RESP-VALFLAGGA-LINE-ATTR (INDX)             
072600           END-IF                                                         
072700         ELSE                                                             
072800           IF (REQU-VALFLAGGA-LINE (INDX) = 'X' OR '+')                   
072900             IF SW-CHANGE-ALL-CASES = YES                                 
073000             AND REQU-VALFLAGGA-LINE (INDX) = 'X'                         
073100               IF INDATA-OK                                               
073200                 MOVE MFS-ALPHA-FIELD-WRONG TO                            
073300                 RESP-VALFLAGGA-LINE-ATTR(INDX)                           
073400                 MOVE NOO TO INDATA-SW                                    
073500               END-IF                                                     
073600             ELSE                                                         
073700             MOVE MFS-ALPHA-FIELD-OK TO                                   
073800                              RESP-VALFLAGGA-LINE-ATTR (INDX)             
073900             END-IF                                                       
074000           ELSE                                                           
074100             IF INDATA-OK                                                 
074200               MOVE MFS-ALPHA-FIELD-WRONG TO                              
074300                    RESP-VALFLAGGA-LINE-ATTR(INDX)                        
074400               MOVE NOO TO INDATA-SW                                      
074500             END-IF                                                       
074600           END-IF                                                         
074700         END-IF                                                           
074800                                                                          
074900         IF REQU-IDTRPTNR-LINE (INDX) = ALL '+'                           
075000           IF REQU-VALFLAGGA-LINE (INDX) = 'X'                            
075100             IF INDATA-OK                                                 
075200               MOVE MFS-NUM-FIELD-WRONG TO                                
075300                    RESP-IDTRPTNR-LINE-ATTR(INDX)                         
075400*              MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                       
075500               MOVE NOO TO INDATA-SW                                      
075600             END-IF                                                       
075700           ELSE                                                           
075800             MOVE MFS-NUM-FIELD-OK TO                                     
075900                                RESP-IDTRPTNR-LINE-ATTR(INDX)             
076000           END-IF                                                         
076100         ELSE                                                             
076200           IF REQU-IDTRPTNR-LINE (INDX) NUMERIC                           
076300               MOVE MFS-NUM-FIELD-OK TO                                   
076400                    RESP-IDTRPTNR-LINE-ATTR(INDX)                         
076500*              MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                       
076600*              MOVE REQU-IDTRPTNR-LINE (INDX) TO                          
076700*                   W-4402-IDTRPTNR                                       
076800           ELSE                                                           
076900             IF INDATA-OK                                                 
077000               MOVE MFS-NUM-FIELD-WRONG TO                                
077100                    RESP-IDTRPTNR-LINE-ATTR(INDX)                         
077200               MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR                  
077300*              MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                       
077400               MOVE NOO TO INDATA-SW                                      
077500             END-IF                                                       
077600           END-IF                                                         
077700         END-IF                                                           
077800         IF REQU-KDFRAKT-LINE (INDX) = ALL '+'                            
077900           IF REQU-VALFLAGGA-LINE (INDX) = 'X'                            
078000             IF INDATA-OK                                                 
078100               MOVE MFS-NUM-FIELD-WRONG TO                                
078200                    RESP-KDFRAKT-LINE-ATTR(INDX)                          
078300               MOVE NOO TO INDATA-SW                                      
078400             END-IF                                                       
078500           ELSE                                                           
078600             MOVE MFS-NUM-FIELD-OK TO                                     
078700                                RESP-KDFRAKT-LINE-ATTR(INDX)              
078800           END-IF                                                         
078900         ELSE                                                             
079000           IF REQU-KDFRAKT-LINE (INDX) NUMERIC                            
079100               MOVE MFS-NUM-FIELD-OK TO                                   
079200                    RESP-KDFRAKT-LINE-ATTR(INDX)                          
079300*              MOVE REQU-KDFRAKT-LINE (INDX) TO                           
079400*                   W-4402-KDFRAKT-MIN                                    
079500*                   W-4402-KDFRAKT-MAX                                    
079600*              PERFORM GC-CHECK-IDTRPTNR                                  
079700           ELSE                                                           
079800             IF INDATA-OK                                                 
079900               MOVE MFS-NUM-FIELD-WRONG TO                                
080000                    RESP-KDFRAKT-LINE-ATTR(INDX)                          
080100               MOVE ERR-NUMERIC-FLDS TO RESP-IDMSG-ERROR                  
080200*              MOVE 'KDFRAKT'  TO RESP-IDELMT-ERROR                       
080300               MOVE NOO TO INDATA-SW                                      
080400             END-IF                                                       
080500           END-IF                                                         
080600         END-IF                                                           
080700         IF INDATA-OK                                                     
080800           IF REQU-VALFLAGGA-LINE (INDX) = 'X'                            
080900             MOVE REQU-IDTRPTNR-LINE (INDX) TO                            
081000                  W-4402-IDTRPTNR                                         
081100             MOVE REQU-KDFRAKT-LINE (INDX)  TO                            
081200                  W-4402-KDFRAKT-MIN                                      
081300                  W-4402-KDFRAKT-MAX                                      
081400             IF REQU-IDDC-KEY = '41' OR '43' OR '61' OR '67'              
081500               CONTINUE                                                   
081600             ELSE                                                         
081700               PERFORM GC-CHECK-IDTRPTNR                                  
081800             END-IF                                                       
081900           END-IF                                                         
082000         END-IF                                                           
082100         ADD +1 TO INDX                                                   
082200       END-PERFORM                                                        
082300                                                                          
082400       IF INDATA-WRONG                                                    
082500         IF RESP-IDMSG-ERROR = '024'                                      
082600           CONTINUE                                                       
082700*LK        MOVE 'IDTRP' TO RESP-IDELMT-ERROR                              
082800         ELSE                                                             
082900           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
083000         END-IF                                                           
083100         PERFORM MFS-FAELT-RAETT-THE-REST                                 
083200         PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                
083300       ELSE                                                               
083400*READ DATABASE-SEGMENT FOR INPUT-CONTROL                                  
083500         IF SEGMENT-FOUND                                                 
083600            NEXT SENTENCE                                                 
083700         ELSE                                                             
083800           PERFORM MFS-ERASE-FIELD-IN                                     
083900         END-IF                                                           
084000       END-IF                                                             
084100     END-IF                                                               
084200     .                                                                    
084300     EJECT                                                                
084400 GB-CHECK-INPUT-FIELDS SECTION.                                           
084500                                                                          
084600     IF REQU-FLTRPTCHG      = ALL '+' AND                                 
084700        REQU-IDTRPTNR       = ALL '+' AND                                 
084800        REQU-KDFRAKT        = ALL '+'                                     
084900       SET DATA-ENTERED-NO       TO TRUE                                  
085000     ELSE                                                                 
085100       IF REQU-FLTRPTCHG    = ALL '+' OR                                  
085200          REQU-IDTRPTNR     = ALL '+' OR                                  
085300          REQU-KDFRAKT      = ALL '+'                                     
085400         SET DATA-ENTERED-NO     TO TRUE                                  
085500       ELSE                                                               
085600         SET DATA-ENTERED-YES    TO TRUE                                  
085700       END-IF                                                             
085800     END-IF                                                               
085900                                                                          
086000     PERFORM                                                              
086100     VARYING INDX FROM +1 BY +1                                           
086200       UNTIL INDX > REQU-KVRADER OR                                       
086300             DATA-ENTERED-YES                                             
086400       IF REQU-VALFLAGGA-LINE (INDX) = ALL '+'                            
086500         CONTINUE                                                         
086600       ELSE                                                               
086700         SET DATA-ENTERED-YES    TO TRUE                                  
086800       END-IF                                                             
086900     END-PERFORM                                                          
087000     .                                                                    
087100     EJECT                                                                
087200 GC-CHECK-IDTRPTNR     SECTION.                                           
087300                                                                          
087400                                                                          
087500     IF SW-CHANGE-ALL-CASES = YES                                         
087600       PERFORM IMS-LAS-GU-4401                                            
087700       PERFORM IMS-GNP-4402-KVAL                                          
087800       IF SEGMENT-FOUND                                                   
087900         CONTINUE                                                         
088000       ELSE                                                               
088100         MOVE ZERO                TO W-4402-IDKUNDNR-MIN                  
088200                                     W-4402-IDKUNDNR-MAX                  
088300         PERFORM IMS-GNP-4402-KVAL-FIRST                                  
088400         IF SEGMENT-FOUND                                                 
088500           CONTINUE                                                       
088600         ELSE                                                             
088700           MOVE MFS-NUM-FIELD-WRONG TO RESP-IDTRPTNR-ATTRIB               
088800           MOVE MFS-NUM-FIELD-WRONG TO RESP-KDFRAKT-ATTRIB                
088900*          MOVE ERR-NUMERIC-FLDS      TO RESP-IDMSG-ERROR                 
089000           MOVE ERR-TRANSPORT-MISSING TO RESP-IDMSG-ERROR                 
089100*          MOVE 'IDTRPTNR'     TO RESP-IDELMT-ERROR                       
089200           MOVE NOO TO INDATA-SW                                          
089300         END-IF                                                           
089400       END-IF                                                             
089500     ELSE                                                                 
089600                                                                          
089700*      MOVE +1 TO INDX                                                    
089800*      PERFORM                                                            
089900*        UNTIL INDX > REQU-KVRADER OR                                     
090000*              INDATA-WRONG                                               
090100*        IF REQU-VALFLAGGA-LINE (INDX) = ALL '+'                          
090200*          CONTINUE                                                       
090300*        ELSE                                                             
090400*          MOVE REQU-IDTRPTNR-LINE (INDX) TO W-4402-IDTRPTNR              
090500*          MOVE REQU-KDFRAKT-LINE  (INDX) TO W-4402-KDFRAKT-MIN           
090600*                                            W-4402-KDFRAKT-MAX           
090700           PERFORM IMS-LAS-GU-4401                                        
090800           PERFORM IMS-GNP-4402-KVAL                                      
090900           IF SEGMENT-FOUND                                               
091000             CONTINUE                                                     
091100           ELSE                                                           
091200             MOVE ZERO                TO W-4402-IDKUNDNR-MIN              
091300                                         W-4402-IDKUNDNR-MAX              
091400             PERFORM IMS-GNP-4402-KVAL-FIRST                              
091500             IF SEGMENT-FOUND                                             
091600               CONTINUE                                                   
091700             ELSE                                                         
091800               MOVE MFS-NUM-FIELD-WRONG TO                                
091900                    RESP-IDTRPTNR-LINE-ATTR (INDX)                        
092000               MOVE MFS-NUM-FIELD-WRONG TO                                
092100                    RESP-KDFRAKT-LINE-ATTR (INDX)                         
092200*              MOVE 'KDFRAKT'  TO RESP-IDELMT-ERROR                       
092300               MOVE ERR-TRANSPORT-MISSING TO RESP-IDMSG-ERROR             
092400*              MOVE 'IDTRPTNR' TO RESP-IDELMT-ERROR                       
092500               MOVE NOO TO INDATA-SW                                      
092600             END-IF                                                       
092700           END-IF                                                         
092800*        END-IF                                                           
092900*        ADD +1 TO INDX                                                   
093000*      END-PERFORM                                                        
093100     END-IF                                                               
093200     .                                                                    
093300     EJECT                                                                
093400 H-UPDATE SECTION.                                                        
093500                                                                          
093600     MOVE 'N'             TO UPDATE-SW                                    
093700     IF REQU-IDPRODNR-START NOT = ALL '+' AND                             
093800        REQU-IDPRODNR-START IS NUMERIC                                    
093900       MOVE REQU-IDPRODNR-START  TO WS-IDPRODNR                           
094000     END-IF                                                               
094100                                                                          
094200     IF REQU-IDKOLLI-START NOT = ALL '+' AND                              
094300        REQU-IDKOLLI-START IS NUMERIC                                     
094400       MOVE REQU-IDKOLLI-START   TO WS-IDKOLLI                            
094500     END-IF                                                               
094600                                                                          
094700     MOVE WS-IDPRODNR     TO W-IDPRODNR                                   
094800     MOVE +1 TO INDX                                                      
094900     PERFORM IMS-GU-WDE601                                                
095000                                                                          
095100     IF SEGMENT-FOUND                                                     
095200       IF WDE6-VORD-IDDC = REQU-IDDC-KEY                                  
095300         MOVE YES      TO UPDATE-SW                                       
095400       END-IF                                                             
095500       IF UPDATE-E611                                                     
095600         IF SW-CHANGE-ALL-CASES = YES                                     
095700*    UPDATE ALL CASES                                                     
095800                                                                          
095900            PERFORM IMS-GHNP-WDE611-ALL-CASES                             
096000            PERFORM UNTIL STATUS-WS NOT = SPACE                           
096100              IF WDE6-KOLLI-KDKOLSTA = 1                                  
096200                MOVE REQU-IDTRPTNR TO WDE6-KOLLI-IDTRPTNR                 
096300                PERFORM IMS-REPL-WDE611                                   
096400              END-IF                                                      
096500              PERFORM IMS-GHNP-WDE611-ALL-CASES                           
096600            END-PERFORM                                                   
096700         ELSE                                                             
096800                                                                          
096900*    UPDATE SELECTED CASES                                                
097000                                                                          
097100           PERFORM UNTIL INDX > REQU-KVRADER                              
097200             IF REQU-VALFLAGGA-LINE (INDX) NOT = ALL '+'                  
097300               INSPECT REQU-IDKOLLI-LINE(INDX) REPLACING                  
097400                       LEADING SPACE BY ZERO                              
097500               MOVE REQU-IDKOLLI-LINE(INDX) TO W-IDKOLLI                  
097600               PERFORM IMS-GHU-WDE611                                     
097700               IF SEGMENT-FOUND                                           
097800                 IF WDE6-KOLLI-KDKOLSTA = 1                               
097900                   MOVE REQU-IDTRPTNR-LINE (INDX) TO                      
098000                        WDE6-KOLLI-IDTRPTNR                               
098100                   PERFORM IMS-REPL-WDE611                                
098200                 END-IF                                                   
098300               END-IF                                                     
098400             END-IF                                                       
098500             ADD +1 TO INDX                                               
098600           END-PERFORM                                                    
098700         END-IF                                                           
098800                                                                          
098900         IF SW-CHANGE-ALL-CASES = YES                                     
099000           MOVE SPACE TO STATUS-WS                                        
099100         END-IF                                                           
099200         MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                          
099300         PERFORM MFS-FORM-ATTR                                            
099400         PERFORM MFS-ERASE-FIELD-IN                                       
099500         MOVE SPACE TO STATUS-WS                                          
099600       END-IF                                                             
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
100100                                                                          
100200*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
100300     MOVE +1 TO INDX                                                      
100400     PERFORM UNTIL INDX > MAX-KVRADER                                     
100500       MOVE ALL-SPACE       TO RESP-VALFLAGGA-LINE (INDX)                 
100600                               RESP-IDKOLLI-LINE   (INDX)                 
100700                               RESP-IDTRPTNR-OLD   (INDX)                 
100800                               RESP-IDTRPTNR-LINE  (INDX)                 
100900                               RESP-KDFRAKT-OLD    (INDX)                 
101000                               RESP-KDFRAKT-LINE   (INDX)                 
101100       ADD +1 TO INDX                                                     
101200     END-PERFORM                                                          
101300     .                                                                    
101400     SKIP3                                                                
101500 MFS-ERASE-FIELD-IN SECTION.                                              
101600                                                                          
101700*    --- ALLA INDATA-FÄLT                                                 
101800     MOVE ALL-SPACE       TO RESP-FLTRPTCHG                               
101900                             RESP-IDTRPTNR                                
102000                             RESP-KDFRAKT                                 
102100                                                                          
102200     MOVE +1 TO INDX                                                      
102300     PERFORM UNTIL INDX > MAX-KVRADER                                     
102400       PERFORM MFS-ERASE-LINE-FIELD-IN                                    
102500       ADD +1 TO INDX                                                     
102600     END-PERFORM                                                          
102700     .                                                                    
102800     EJECT                                                                
102900 MFS-ERASE-LINE-FIELD-IN SECTION.                                         
103000                                                                          
103100*    --- INPUT-FIELD ON SCROLL KEYS                                       
103200     MOVE ALL-SPACE       TO RESP-VALFLAGGA-LINE (INDX)                   
103300                             RESP-IDKOLLI-LINE   (INDX)                   
103400                             RESP-IDTRPTNR-OLD   (INDX)                   
103500                             RESP-IDTRPTNR-LINE  (INDX)                   
103600                             RESP-KDFRAKT-OLD    (INDX)                   
103700                             RESP-KDFRAKT-LINE   (INDX)                   
103800     .                                                                    
103900     SKIP3                                                                
104000 MFS-DO-NOT-TOUCH-FIELD-IN  SECTION.                                      
104100                                                                          
104200*    --- ALLA INDATA-FÄLT                                                 
104300     MOVE ALL-PLUS                TO RESP-FLTRPTCHG                       
104400                                     RESP-IDTRPTNR                        
104500                                     RESP-KDFRAKT                         
104600                                                                          
104700     MOVE +1 TO INDX                                                      
104800     PERFORM UNTIL INDX > MAX-KVRADER                                     
104900       MOVE ALL-PLUS        TO RESP-VALFLAGGA-LINE  (INDX)                
105000                               RESP-IDKOLLI-LINE    (INDX)                
105100                               RESP-IDTRPTNR-OLD    (INDX)                
105200                               RESP-IDTRPTNR-LINE   (INDX)                
105300                               RESP-KDFRAKT-OLD     (INDX)                
105400                               RESP-KDFRAKT-LINE    (INDX)                
105500       ADD +1 TO INDX                                                     
105600     END-PERFORM                                                          
105700     .                                                                    
105800     EJECT                                                                
105900*                                                                         
106000 MFS-FORM-ATTR SECTION.                                                   
106100                                                                          
106200*    --- ALL INDATA-FIELDS                                                
106300     MOVE MFS-FORMAT-DEFAULT-ATTR TO RESP-FLTRPTCHG-ATTRIB                
106400                                     RESP-IDTRPTNR-ATTRIB                 
106500                                     RESP-KDFRAKT-ATTRIB                  
106600     MOVE +1 TO INDX                                                      
106700     PERFORM UNTIL INDX > MAX-KVRADER                                     
106800       MOVE MFS-FORMAT-DEFAULT-ATTR TO                                    
106900                                  RESP-VALFLAGGA-LINE-ATTR (INDX)         
107000                                  RESP-IDKOLLI-LINE-ATTR   (INDX)         
107100                                  RESP-IDTRPTNR-OLD-ATTR   (INDX)         
107200                                  RESP-IDTRPTNR-LINE-ATTR  (INDX)         
107300                                  RESP-KDFRAKT-OLD-ATTR    (INDX)         
107400                                  RESP-KDFRAKT-LINE-ATTR   (INDX)         
107500       ADD +1 TO INDX                                                     
107600     END-PERFORM                                                          
107700     .                                                                    
107800     SKIP2                                                                
107900 MFS-FAELT-RAETT-THE-REST  SECTION.                                       
108000                                                                          
108100     PERFORM UNTIL INDX > MAX-KVRADER                                     
108200      MOVE MFS-ALFA-FAELT-RAETT TO RESP-VALFLAGGA-LINE-ATTR (INDX)        
108300                                   RESP-IDTRPTNR-LINE-ATTR  (INDX)        
108400                                   RESP-KDFRAKT-LINE-ATTR   (INDX)        
108500       ADD +1 TO INDX                                                     
108600     END-PERFORM                                                          
108700     .                                                                    
108800     SKIP2                                                                
108900 MFS-READ-IN-AGAIN SECTION.                                               
109000                                                                          
109100     IF REQU-FLTRPTCHG NOT = ALL '+'                                      
109200       MOVE MFS-ADD-READ-FIELD TO RESP-FLTRPTCHG-ATTRIB                   
109300     END-IF                                                               
109400     IF REQU-IDTRPTNR NOT = ALL '+'                                       
109500       MOVE MFS-ADD-READ-FIELD TO RESP-IDTRPTNR-ATTRIB                    
109600     END-IF                                                               
109700     IF REQU-KDFRAKT  NOT = ALL '+'                                       
109800       MOVE MFS-ADD-READ-FIELD TO RESP-KDFRAKT-ATTRIB                     
109900     END-IF                                                               
110000                                                                          
110100     MOVE +1 TO INDX                                                      
110200     PERFORM UNTIL INDX > MAX-KVRADER                                     
110300       PERFORM MFS-ADD-READ-FIELD-LINE                                    
110400       ADD +1 TO INDX                                                     
110500     END-PERFORM                                                          
110600     .                                                                    
110700     SKIP2                                                                
110800 MFS-ADD-READ-FIELD-LINE       SECTION.                                   
110900                                                                          
111000     IF REQU-IDKOLLI-LINE (INDX) NOT = ALL '+'                            
111100       MOVE MFS-ADD-READ-FIELD TO RESP-IDKOLLI-LINE-ATTR   (INDX)         
111200     END-IF                                                               
111300     IF REQU-VALFLAGGA-LINE (INDX) NOT = ALL '+'                          
111400       MOVE MFS-ADD-READ-FIELD TO RESP-VALFLAGGA-LINE-ATTR (INDX)         
111500     END-IF                                                               
111600     IF REQU-IDTRPTNR-LINE (INDX) NOT = ALL '+'                           
111700       MOVE MFS-ADD-READ-FIELD TO RESP-IDTRPTNR-LINE-ATTR  (INDX)         
111800     END-IF                                                               
111900*    IF REQU-IDTRPTNR-OLD-IN (INDX) NOT = ALL '+'                         
112000       MOVE MFS-ADD-READ-FIELD TO RESP-IDTRPTNR-OLD-ATTR   (INDX)         
112100*    END-IF                                                               
112200     IF REQU-KDFRAKT-LINE (INDX) NOT = ALL '+'                            
112300       MOVE MFS-ADD-READ-FIELD TO RESP-KDFRAKT-LINE-ATTR  (INDX)          
112400     END-IF                                                               
112500*    IF REQU-KDFRAKT-OLD-IN (INDX) NOT = ALL '+'                          
112600       MOVE MFS-ADD-READ-FIELD TO RESP-KDFRAKT-OLD-ATTR   (INDX)          
112700*    END-IF                                                               
112800     .                                                                    
112900     SKIP3                                                                
113000 IMS-GN-WDE4A1 SECTION.                                                   
113100                                                                          
113200     STRING 'WDE4A1  (WDE4A1KY>=' W-WDE4A1KY-MIN-X                        
113300                    '&WDE4A1KY<=' W-WDE4A1KY-MAX-X ')'                    
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
113600     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE4A01 SSA1                  
113700     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
113800     PERFORM IMS-STATUSCHECK                                              
113900     .                                                                    
114000     EJECT                                                                
114100 IMS-GU-WDE601 SECTION.                                                   
114200                                                                          
114300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GE' TO GOOD-STATUSCODES                                      
114600     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
114700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSCHECK                                              
114900     .                                                                    
115000     EJECT                                                                
115100 IMS-GNP-WDE611 SECTION.                                                  
115200                                                                          
115300     MOVE '  GE' TO GOOD-STATUSCODES                                      
115400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611                        
115500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
115600     PERFORM IMS-STATUSCHECK                                              
115700     .                                                                    
115800     SKIP3                                                                
115900 IMS-GNP-WDE611-NEXT SECTION.                                             
116000                                                                          
116100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
116200          DELIMITED BY SIZE INTO SSA1                                     
116300     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
116400          DELIMITED BY SIZE INTO SSA2                                     
116500     MOVE '  GE' TO GOOD-STATUSCODES                                      
116600     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
116700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
116800     PERFORM IMS-STATUSCHECK                                              
116900     .                                                                    
117000     SKIP3                                                                
117100 IMS-GNP-WDE611-ST   SECTION.                                             
117200                                                                          
117300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
117400          DELIMITED BY SIZE INTO SSA1                                     
117500     STRING 'WDE611  (IDKOLLI  >' W-IDKOLLI-X ')'                         
117600          DELIMITED BY SIZE INTO SSA2                                     
117700     MOVE '  GE' TO GOOD-STATUSCODES                                      
117800     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
117900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSCHECK                                              
118100     .                                                                    
118200     SKIP3                                                                
118300 IMS-GHU-WDE611 SECTION.                                                  
118400                                                                          
118500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
118600          DELIMITED BY SIZE INTO SSA1                                     
118700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
118800          DELIMITED BY SIZE INTO SSA2                                     
118900     MOVE '  GE' TO GOOD-STATUSCODES                                      
119000     CALL CBLTDLI USING GHU  WDE6-PCB DLI-IO-WDE611 SSA1 SSA2             
119100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUSCHECK                                              
119300     .                                                                    
119400     SKIP3                                                                
119500 IMS-GHNP-WDE611-ALL-CASES SECTION.                                       
119600                                                                          
119700     MOVE 'WDE611   '         TO SSA1                                     
119800     MOVE '  GE' TO GOOD-STATUSCODES                                      
119900     CALL CBLTDLI USING GHNP WDE6-PCB DLI-IO-WDE611 SSA1                  
120000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSCHECK                                              
120200     .                                                                    
120300     SKIP3                                                                
120400 IMS-REPL-WDE611 SECTION.                                                 
120500                                                                          
120600     MOVE '  ' TO GOOD-STATUSCODES                                        
120700     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-WDE611                       
120800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
120900     PERFORM IMS-STATUSCHECK                                              
121000     .                                                                    
121100     EJECT                                                                
121200 IMS-GU-KUNDORDER-SEK-INV-INT SECTION.                                    
121300                                                                          
121400     STRING 'WDE411  (WDE4BSEQ>=' W-WDE420-KEYSEQ-MIN-X                   
121500                    '&WDE4BSEQ<=' W-WDE420-KEYSEQ-MAX-X ')'               
121600            DELIMITED BY SIZE INTO SSA1                                   
121700     MOVE 'WDE401   ' TO SSA2                                             
121800     MOVE '  GE' TO GOOD-STATUSCODES                                      
121900     CALL CBLTDLI USING GU WDE43-PCB DLI-IO-AREA SSA1 SSA2                
122000     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
122100     PERFORM IMS-STATUSCHECK                                              
122200     .                                                                    
122300     EJECT                                                                
122400 IMS-LAS-GU-4401 SECTION.                                                 
122500                                                                          
122600     STRING 'WLXXDM01(WDGXKEY  =' W-4401-WDGXKEY-X ')'                    
122700            DELIMITED BY SIZE INTO SSA1                                   
122800     MOVE '  ' TO GOOD-STATUSCODES                                        
122900     CALL CBLTDLI USING GU XXDM-PCB DLI-IO-XXDM SSA1                      
123000     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
123100     PERFORM IMS-STATUSCHECK                                              
123200                                                                          
123300     SKIP2                                                                
123400     .                                                                    
123500 IMS-LAS-GNP-4402 SECTION.                                                
123600                                                                          
123700     STRING 'WLXXDM11(KY4402  >=' W-4402-KY4402-MIN-X                     
123800                    '&KY4402  <=' W-4402-KY4402-MAX-X ')'                 
123900            DELIMITED BY SIZE INTO SSA1                                   
124000     MOVE '  GE' TO GOOD-STATUSCODES                                      
124100     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-XXDM SSA1                     
124200     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
124300     PERFORM IMS-STATUSCHECK                                              
124400                                                                          
124500     .                                                                    
124600 IMS-GNP-4402-KVAL SECTION.                                               
124700     SKIP2                                                                
124800     STRING 'WLXXDM11(KY4402  >=' W-4402-KY4402-MIN-X                     
124900                    '&KY4402  <=' W-4402-KY4402-MAX-X                     
125000                    '&IDTRPTNR =' W-4402-IDTRPTNR-X ')'                   
125100            DELIMITED BY SIZE INTO SSA1                                   
125200     MOVE '  GE' TO GOOD-STATUSCODES                                      
125300     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-XXDM SSA1                     
125400     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
125500     PERFORM IMS-STATUSCHECK                                              
125600     SKIP3                                                                
125700     .                                                                    
125800     EJECT                                                                
125900 IMS-GNP-4402-KVAL-FIRST SECTION.                                         
126000     SKIP2                                                                
126100     STRING 'WLXXDM11*F(KY4402  >=' W-4402-KY4402-MIN-X                   
126200                    '&KY4402  <=' W-4402-KY4402-MAX-X                     
126300                    '&IDTRPTNR =' W-4402-IDTRPTNR-X ')'                   
126400            DELIMITED BY SIZE INTO SSA1                                   
126500     MOVE '  GE' TO GOOD-STATUSCODES                                      
126600     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-XXDM SSA1                     
126700     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSCHECK                                              
126900     SKIP3                                                                
127000     .                                                                    
127100     EJECT                                                                
127200 IMS-STATUSCHECK SECTION.                                                 
127300                                                                          
127400     SET STATUS-IX TO 1                                                   
127500     SEARCH GOOD-STATUS                                                   
127600       AT END                                                             
127700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
127800         DELIMITED BY SIZE INTO ERROR-TEXT                                
127900         CALL FELLOG                                                      
128000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
128100         CONTINUE                                                         
128200     END-SEARCH                                                           
128300     .                                                                    
