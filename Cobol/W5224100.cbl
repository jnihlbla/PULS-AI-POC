000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5224100.                                                
000300 AUTHOR.         LENA SKOGLUND GUIDE KONSULT AB                           
000400 DATE-WRITTEN.   03/05/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        THE PROGRAM                                                      
001100*        - READS FILE W52203 AND CREATES VAT DATA                         
001200*        - SENDS VAT DATA TO CARPARTS.DRP.DISTDOC                         
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100*          --- SELECTED VAT-DATA FROM IVW-TABLE                           
002200     SELECT W52203                     ASSIGN TO W52241D1.                
002300     EJECT                                                                
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800 FD  W52203                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100 01  IN-POST.                                                             
003200*    03  -COPY W522VAT  -L.                                               
003300                                                                          
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W5224100'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000     EJECT                                                                
004100                                                                          
004200 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
004300 77  W52203-EOF-SW               PIC X       VALUE 'N'.                   
004400     88  END-OF-W52203                       VALUE 'J'.                   
004500     EJECT                                                                
004600 01  WS-IDLANDX3                 PIC X(3)        VALUE SPACE.             
004700 01  WS-KDVALISO                 PIC X(3)        VALUE SPACE.             
004800 01  WS-S11-2                    PIC S9(11)V9(2) VALUE ZERO.              
004900 01  WS-SUNTO                    PIC S9(11)V9(2) VALUE ZERO.              
005000 01  WS-SUNTO-SEK                PIC S9(11)V9(2) VALUE ZERO.              
005100 01  WS-SUNTO-SEK-TOT            PIC S9(11)V9(2) VALUE ZERO.              
005200 01  WS-KVANTAL                  PIC S9(4)       VALUE ZERO.              
005300 01  WS-KVPOST                   PIC S9(6)       VALUE ZERO.              
005400 01  WS-DISP-KVPOST              PIC Z(3)9.                               
005500 01  WS-DISP-KVANTAL             PIC Z(3)9.                               
005700                                                                          
005800 77  WS-KDFINDOC                 PIC X(3).                                
005900     88 WS-GODK-KDFINDOC         VALUE 'INT'                              
006000                                       'INV'                              
006100                                       'CR '.                             
006200                                                                          
006300*    --------ERROR MESSAGE---                                             
006400 77  ERRORTEXT                   PIC X(80)   VALUE SPACE.                 
006500 77  KDRC-DISPLAY                PIC Z(5).                                
006600                                                                          
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007300     EJECT                                                                
007400*    --- PARAMETERS TO ABEND                                              
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800     EJECT                                                                
007900*    --- INFIL                                                            
008000 01  IN-AREA-START               PIC X(24)   VALUE                        
008100                                             'IN-AREA-START'.             
008200 01  IN-AREA.                                                             
008300*    03  -COPY W522VAT     -PRE IN-                                       
008400     EJECT                                                                
008500                                                                          
008600 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
008700 01  -COPY WZ01SEND                                                       
008800     EJECT                                                                
008900                                                                          
009000 01  HDR-AREA.                                                            
009100*    03  -COPY WZ01REQU                                                   
009200*    03  -COPY WZ04HDR                                                    
009300     EJECT                                                                
009400                                                                          
009500 01  UT-AREA-START           PIC X(24) VALUE                              
009600                                             'UT-AREA-START'.             
009700 01  UT-HDR1.                                                             
009800     03 FILLER               PIC X(10) VALUE SPACE.                       
009900     03 FILLER               PIC X(18) VALUE 'ACCOUNTING PERIOD '.        
010000     03 UT-TIAA              PIC 9(2).                                    
010100     03 UT-TIRP              PIC 9(2).                                    
010200                                                                          
010300 01  UT-HDR2.                                                             
010400     03 FILLER               PIC X(3)  VALUE 'COU'.                       
010500     03 FILLER               PIC X(2)  VALUE SPACE.                       
010600     03 FILLER               PIC X(15) VALUE '    INV AMOUNT '.           
010800     03 FILLER               PIC X(3)  VALUE 'CCY'.                       
010900     03 FILLER               PIC X(3)  VALUE SPACE.                       
011000     03 FILLER               PIC X(15) VALUE '    INV AMOUNT '.           
011200     03 FILLER               PIC X(3)  VALUE 'SEK'.                       
011300                                                                          
011400 01  UT-LINE.                                                             
011500     03 UT-IDLANDX3-BET      PIC X(3).                                    
011600     03 FILLER               PIC X(2)  VALUE SPACE.                       
011700     03 UT-SUNTO             PIC Z(10)9.99-.                              
011900     03 UT-KDVALISO          PIC X(3).                                    
012000     03 FILLER               PIC X(3)  VALUE ' = '.                       
012100     03 UT-SUNTO-SEK         PIC Z(10)9.99-.                              
012300     03 FILLER               PIC X(3)  VALUE 'SEK'.                       
012400                                                                          
012500 01  UT-LINE-SEK.                                                         
012600     03 FILLER               PIC X(6)  VALUE 'TOTAL'.                     
012700     03 FILLER               PIC X(20) VALUE SPACE.                       
012800     03 UT-SUNTO-SEK-TOT     PIC Z(10)9.99-.                              
013000     03 FILLER               PIC X(3)  VALUE 'SEK'.                       
013100                                                                          
013200 01  UT-NADA                 PIC X(40) VALUE 'NO INVOICE FOUND'.          
013300                                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009   -PRE MSG-                                              
013800                                                                          
013900     EJECT                                                                
014000                                                                          
014100 PROCEDURE DIVISION  USING MSG-PCB.                                       
014200     ENTRY 'DLITCBL' USING MSG-PCB.                                       
014300                                                                          
014400 MAIN SECTION.                                                            
014500     PERFORM A-INIT                                                       
014600                                                                          
014700     PERFORM S10-READ-W52203                                              
014800     PERFORM UNTIL END-OF-W52203                                          
014900       IF  IN-IDLANDX3-SEND =  'BE '                                      
015100         IF NOT IN-IDLANDX3-BET = WS-IDLANDX3                             
015200           PERFORM B-BRYT                                                 
015300         END-IF                                                           
015400         PERFORM C-SUMMERA                                                
015500       END-IF                                                             
015600       PERFORM S10-READ-W52203                                            
015700     END-PERFORM                                                          
015800     PERFORM B-BRYT                                                       
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100     MOVE ZERO                    TO RETURN-CODE                          
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     DISPLAY                         ' '                                  
016900     MOVE '***'                   TO WS-IDLANDX3                          
017000     MOVE ZERO                    TO WS-SUNTO-SEK-TOT                     
017100     MOVE ZERO                    TO WS-KVANTAL                           
017200     MOVE ZERO                    TO WS-KVPOST                            
017300     OPEN INPUT  W52203                                                   
017400                                                                          
017500     PERFORM S21-SEND-OPEN                                                
017600     PERFORM S22-PUT-HEADER                                               
017700     .                                                                    
017800     EJECT                                                                
017900 B-BRYT    SECTION.                                                       
018000     IF NOT WS-IDLANDX3 = '***'                                           
018100       IF WS-KVANTAL = +1                                                 
018200         DISPLAY                     UT-HDR1                              
018300         PERFORM S23-PUT-HDR1                                             
018400         DISPLAY                     UT-HDR2                              
018500         PERFORM S24-PUT-HDR2                                             
018600       END-IF                                                             
018700       PERFORM BA-CREATE-LINE                                             
018800       DISPLAY                       UT-LINE                              
018900       PERFORM S25-PUT-LINE                                               
019000     END-IF                                                               
019100                                                                          
019200     IF NOT END-OF-W52203                                                 
019300       MOVE IN-IDLANDX3-BET       TO WS-IDLANDX3                          
019400       MOVE IN-KDVALISO           TO WS-KDVALISO                          
019500       ADD +1                     TO WS-KVANTAL                           
019600       MOVE ZERO                  TO WS-SUNTO                             
019700       MOVE ZERO                  TO WS-SUNTO-SEK                         
019800     END-IF                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 BA-CREATE-LINE   SECTION.                                                
020200     MOVE WS-IDLANDX3             TO UT-IDLANDX3-BET                      
020300     MOVE WS-KDVALISO             TO UT-KDVALISO                          
020400     MOVE WS-SUNTO                TO UT-SUNTO                             
020500     MOVE WS-SUNTO-SEK            TO UT-SUNTO-SEK                         
020600     .                                                                    
020700     EJECT                                                                
020800 C-SUMMERA SECTION.                                                       
020900     MOVE IN-KDFINDOC             TO WS-KDFINDOC                          
021000     IF NOT WS-GODK-KDFINDOC                                              
021100       DISPLAY '********************************'                         
021200       DISPLAY 'FEL!! ABEND: OKÄND KDFINDOC ' IN-KDFINDOC                 
021300       DISPLAY '********************************'                         
021400       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
021500     END-IF                                                               
021600                                                                          
021700     ADD +1                       TO WS-KVPOST                            
021800     IF IN-KDFINDOC = 'CR  '                                              
021900       COMPUTE WS-S11-2            =  IN-SUNTO-TOT * -1                   
022000     ELSE                                                                 
022100       COMPUTE WS-S11-2            =  IN-SUNTO-TOT                        
022200     END-IF                                                               
022300     ADD WS-S11-2                 TO WS-SUNTO                             
022400                                                                          
022500     COMPUTE WS-S11-2 ROUNDED   = WS-S11-2 * IN-PRKURS                    
022600     ADD WS-S11-2                 TO WS-SUNTO-SEK                         
022700     ADD WS-S11-2                 TO WS-SUNTO-SEK-TOT                     
022800     MOVE IN-TIAA                 TO UT-TIAA                              
022900     MOVE IN-TIRP                 TO UT-TIRP                              
023000     .                                                                    
023100     EJECT                                                                
023200 Z-FINIT SECTION.                                                         
023300     CLOSE W52203                                                         
023400                                                                          
023500     IF  WS-KVPOST = 0                                                    
023600       DISPLAY                       UT-NADA                              
023700       PERFORM S26-PUT-LINE-NADA                                          
023800     ELSE                                                                 
023900       MOVE WS-SUNTO-SEK-TOT      TO UT-SUNTO-SEK-TOT                     
024000       DISPLAY                       UT-LINE-SEK                          
024100       PERFORM S27-PUT-LINE-SEK                                           
024200                                                                          
024300       MOVE WS-KVPOST             TO WS-DISP-KVPOST                       
024400       MOVE WS-KVANTAL            TO WS-DISP-KVANTAL                      
024500       DISPLAY                       ' '                                  
024600       DISPLAY 'ANTAL FAKTUROR:'     WS-DISP-KVPOST                       
024700       DISPLAY 'ANTAL LÄNDER  :'     WS-DISP-KVANTAL                      
024800     END-IF                                                               
024900                                                                          
025000     PERFORM S29-SEND-CLOSE                                               
025100     DISPLAY                         ' '                                  
025200     .                                                                    
025300     EJECT                                                                
025400 S10-READ-W52203  SECTION.                                                
025500     READ W52203                INTO IN-AREA                              
025600     AT END                                                               
025700        MOVE YES                  TO W52203-EOF-SW                        
025800     END-READ                                                             
025900     .                                                                    
026000     EJECT                                                                
026100 S21-SEND-OPEN SECTION.                                                   
026200     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
026300     MOVE 'OPEN'                  TO SEND-KDFUNC                          
026400     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
026500                                     SEND-OPEN-AREA                       
026600     IF SEND-KDRC > ZERO                                                  
026700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
026800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
026900       DELIMITED BY SIZE        INTO ERRORTEXT                            
027000       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 S22-PUT-HEADER SECTION.                                                  
027500     MOVE 1                           TO REQU-IDMSGVER                    
027600     MOVE 'R'                         TO REQU-KDPGMACT                    
027700     MOVE IDPGM                       TO REQU-IDUSER                      
027800     MOVE 'W522'                      TO HDR-IDOUTTYPE                    
027900     MOVE 'W52241'                    TO HDR-IDOUTREC                     
028000     MOVE FUNCTION CURRENT-DATE(3:10) TO HDR-IDLIST                       
028100     MOVE 'PUT'                       TO SEND-KDFUNC                      
028200     MOVE LENGTH OF HDR-AREA          TO SEND-KVDLEN                      
028300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
028400                                     SEND-KVDLEN                          
028500                                     HDR-AREA                             
028600     IF SEND-KDRC > ZERO                                                  
028700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
028800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
028900       DELIMITED BY SIZE        INTO ERRORTEXT                            
029000       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 S23-PUT-HDR1 SECTION.                                                    
029500     MOVE 'PUT'                   TO SEND-KDFUNC                          
029600     MOVE LENGTH OF UT-HDR1       TO SEND-KVDLEN                          
029700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
029800                                     SEND-KVDLEN                          
029900                                     UT-HDR1                              
030000     IF SEND-KDRC > ZERO                                                  
030100       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
030200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
030300       DELIMITED BY SIZE        INTO ERRORTEXT                            
030400       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 S24-PUT-HDR2 SECTION.                                                    
030900     MOVE 'PUT'                   TO SEND-KDFUNC                          
031000     MOVE LENGTH OF UT-HDR2       TO SEND-KVDLEN                          
031100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
031200                                     SEND-KVDLEN                          
031300                                     UT-HDR2                              
031400     IF SEND-KDRC > ZERO                                                  
031500       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
031600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
031700       DELIMITED BY SIZE        INTO ERRORTEXT                            
031800       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 S25-PUT-LINE SECTION.                                                    
032300     MOVE 'PUT'                   TO SEND-KDFUNC                          
032400     MOVE LENGTH OF UT-LINE       TO SEND-KVDLEN                          
032500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
032600                                     SEND-KVDLEN                          
032700                                     UT-LINE                              
032800     IF SEND-KDRC > ZERO                                                  
032900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
033000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033100       DELIMITED BY SIZE        INTO ERRORTEXT                            
033200       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
033300     END-IF                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 S26-PUT-LINE-NADA  SECTION.                                              
033700     MOVE 'PUT'                   TO SEND-KDFUNC                          
033800     MOVE LENGTH OF UT-NADA       TO SEND-KVDLEN                          
033900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
034000                                     SEND-KVDLEN                          
034100                                     UT-NADA                              
034200     IF SEND-KDRC > ZERO                                                  
034300       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
034400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
034500       DELIMITED BY SIZE        INTO ERRORTEXT                            
034600       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 S27-PUT-LINE-SEK  SECTION.                                               
035100     MOVE 'PUT'                   TO SEND-KDFUNC                          
035200     MOVE LENGTH OF UT-LINE-SEK   TO SEND-KVDLEN                          
035300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
035400                                     SEND-KVDLEN                          
035500                                     UT-LINE-SEK                          
035600     IF SEND-KDRC > ZERO                                                  
035700       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
035800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035900       DELIMITED BY SIZE        INTO ERRORTEXT                            
036000       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 S29-SEND-CLOSE SECTION.                                                  
036500     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
036600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
036700     .                                                                    
