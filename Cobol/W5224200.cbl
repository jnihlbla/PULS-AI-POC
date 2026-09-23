000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5224200.                                                
000300 AUTHOR.         LENA SKOGLUND GUIDE KONSULT AB                           
000400 DATE-WRITTEN.   13/05/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*                                                                         
001000*        THE PROGRAM                                                      
001100*        - READS PRM-DATA FROM SYSIN                                      
001200*        - READS FILE W52203 AND CREATES VAT DATA                         
001300*        - SENDS VAT DATA TO CARPARTS.DRP.DISTDOC                         
001400*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400*          --- SELECTED VAT-DATA FROM IVW-TABLE                           
002500     SELECT W52203                     ASSIGN TO W52242D1.                
002600     SKIP2                                                                
002700*          --- FILE OF DOMESTIC VAT                                       
002800     SELECT W52242-001                 ASSIGN TO W52242D2.                
002900     EJECT                                                                
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400 FD  W52203                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700 01  IN-POST.                                                             
003800*    03  -COPY W522VAT  -L.                                               
003900                                                                          
004000*---------------                                                          
004100 FD  W52242-001                                                           
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400 01  W52242-001-LINE         PIC X(74).                                   
004500*---------------                                                          
004600     EJECT                                                                
004700                                                                          
004800 WORKING-STORAGE SECTION.                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W5224200'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200     EJECT                                                                
005300                                                                          
005400 77  W52203-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W52203                       VALUE 'J'.                   
005600                                                                          
005700 77  WS-IDLANDX3                 PIC X(3).                                
005800     88 WS-GODK-IDLANDX3         VALUE 'NL '                              
005900                                       'ES '                              
005910                                       'PT '                              
006000                                       'NO '                              
006100                                       'GB '                              
006200                                       'IT '                              
006300                                       'AT '.                             
006400                                                                          
006500 01  WS-KVPOST                   PIC S9(6)      VALUE ZERO.               
006600 01  WS-DISP-KVPOST              PIC Z(3)9.                               
006700                                                                          
006800*    --- SPAR-AREA FÖR BRYTNING                                           
006900 01  WS-SPAR.                                                             
007000     03  SPAR-IDLANDX3           PIC X(3)  VALUE ZERO.                    
007100                                                                          
007200*    --------ERROR MESSAGE---                                             
007300 77  ERRORTEXT                   PIC X(80)  VALUE SPACE.                  
007400 77  KDRC-DISPLAY                PIC Z(5).                                
007500                                                                          
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700*                                                                         
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008200     EJECT                                                                
008300*    --- PARAMETERS TO ABEND                                              
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700     EJECT                                                                
008800*    --- INFIL                                                            
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                             'IN-AREA-START'.             
009100 01  IN-AREA.                                                             
009200*    03  -COPY W522VAT     -PRE IN-                                       
009300     EJECT                                                                
009400                                                                          
009500 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
009600 01  -COPY WZ01SEND                                                       
009700     EJECT                                                                
009800                                                                          
009900 01  HDR-AREA.                                                            
010000*    03  -COPY WZ01REQU                                                   
010100*    03  -COPY WZ04HDR                                                    
010200     EJECT                                                                
010300                                                                          
010400 01  UT-AREA-START           PIC X(24) VALUE 'UT-AREA-START'.             
010500 01  UT-HDR1.                                                             
010600     03 FILLER               PIC X(10) VALUE SPACE.                       
010700     03 FILLER               PIC X(18) VALUE 'ACCOUNTING PERIOD '.        
010800     03 UT-TIAA              PIC 9(2).                                    
010900     03 UT-TIRP              PIC 9(2).                                    
011000     03 FILLER               PIC X(3) VALUE SPACE.                        
011100     03 UT-IDLANDX3          PIC X(3).                                    
011200                                                                          
011300 01  UT-HDR2.                                                             
011400     03 FILLER               PIC X(18)    VALUE ' VAT NR'.                
011500     03 FILLER               PIC X(4)     VALUE 'DIST'.                   
011600     03 FILLER               PIC X(9)     VALUE '   INV NO'.              
011700     03 FILLER               PIC X(1)     VALUE SPACE.                    
011800     03 FILLER               PIC X(6)     VALUE 'INV DA'.                 
011900     03 FILLER               PIC X(13)    VALUE '   INV AMOUNT'.          
012000     03 FILLER               PIC X(1)     VALUE SPACE.                    
012100     03 FILLER               PIC X(3)     VALUE 'CCY'.                    
012200     03 FILLER               PIC X(12)    VALUE '  VAT AMOUNT'.           
012300                                                                          
012400 01  UT-LINE.                                                             
012500     03 FILLER               PIC X(1)       VALUE SPACE.                  
012600     03 UT-IDVAT-BET         PIC X(17).                                   
012700     03 UT-IDDISTR           PIC Z(3)9.                                   
012800     03 UT-IDFINDOC          PIC Z(8)9.                                   
012900     03 FILLER               PIC X(1)       VALUE SPACE.                  
013000     03 UT-DAFINDOC          PIC 9(6).                                    
013100     03 UT-SUNTO-TOT         PIC Z(9)9.9(2).                              
013200     03 FILLER               PIC X(1)       VALUE SPACE.                  
013300     03 UT-KDVALISO          PIC X(3).                                    
013400     03 UT-SUVAT-BILLIT-TOT  PIC Z(8)9.9(2).                              
013500                                                                          
013600 01  UT-NADA                 PIC X(40) VALUE ' NO INVOICE FOUND'.         
013700     EJECT                                                                
013800                                                                          
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500*01  -COPY W0009   -PRE MSG-                                              
014600                                                                          
014700     EJECT                                                                
014800                                                                          
014900 PROCEDURE DIVISION  USING MSG-PCB.                                       
015000     ENTRY 'DLITCBL' USING MSG-PCB.                                       
015100                                                                          
015200 MAIN SECTION.                                                            
015300     PERFORM A-INIT                                                       
015400                                                                          
015500     PERFORM S10-READ-W52203                                              
015600     PERFORM UNTIL END-OF-W52203                                          
015800       MOVE IN-IDLANDX3-SEND TO WS-IDLANDX3                               
015900       IF (IN-IDLANDX3-SEND = IN-IDLANDX3-BET                             
016000       AND WS-GODK-IDLANDX3)                                              
016200         PERFORM B-CREATE-LINE                                            
016300         PERFORM S25-WRITE-LINE                                           
016400       END-IF                                                             
016500       PERFORM S10-READ-W52203                                            
016600     END-PERFORM                                                          
016700                                                                          
016800     PERFORM Z-FINIT                                                      
016900     MOVE ZERO                    TO RETURN-CODE                          
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300                                                                          
017400 A-INIT SECTION.                                                          
017500     DISPLAY                         ' '                                  
017600     MOVE ZERO                    TO WS-KVPOST                            
017700     MOVE '***'                   TO SPAR-IDLANDX3                        
017800     OPEN INPUT  W52203                                                   
017900     OPEN OUTPUT W52242-001                                               
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 B-CREATE-LINE   SECTION.                                                 
018400     ADD  +1                      TO WS-KVPOST                            
018500     IF NOT IN-IDLANDX3-BET  = SPAR-IDLANDX3                              
018600       MOVE IN-IDLANDX3-BET       TO SPAR-IDLANDX3                        
018700       MOVE IN-IDLANDX3-BET       TO UT-IDLANDX3                          
018800       MOVE IN-TIAA               TO UT-TIAA                              
018900       MOVE IN-TIRP               TO UT-TIRP                              
019000       WRITE W52242-001-LINE    FROM UT-HDR1 AFTER 1                      
019100       WRITE W52242-001-LINE    FROM UT-HDR2 AFTER 1                      
019200     END-IF                                                               
019300                                                                          
019400     MOVE IN-IDVAT-BET            TO UT-IDVAT-BET                         
019500     MOVE IN-IDDISTR              TO UT-IDDISTR                           
019600     MOVE IN-IDFINDOC             TO UT-IDFINDOC                          
019700     MOVE IN-DAFINDOC             TO UT-DAFINDOC                          
019800     MOVE IN-SUNTO-TOT            TO UT-SUNTO-TOT                         
019900     MOVE IN-KDVALISO             TO UT-KDVALISO                          
020000     MOVE IN-SUVAT-BILLIT-TOT     TO UT-SUVAT-BILLIT-TOT                  
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 Z-FINIT SECTION.                                                         
020500     IF  WS-KVPOST = 0                                                    
020600       DISPLAY                       UT-NADA                              
020700       WRITE W52242-001-LINE FROM UT-NADA AFTER 1                         
020800     END-IF                                                               
020900                                                                          
021000     MOVE WS-KVPOST               TO WS-DISP-KVPOST                       
021100     DISPLAY                         ' '                                  
021200     DISPLAY 'ANTAL FAKTUROR:'       WS-DISP-KVPOST                       
021300     DISPLAY                         ' '                                  
021400                                                                          
021500     CLOSE W52203                                                         
021600           W52242-001                                                     
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 S10-READ-W52203  SECTION.                                                
022100     READ W52203                INTO IN-AREA                              
022200     AT END                                                               
022300        MOVE YES                  TO W52203-EOF-SW                        
022400     END-READ                                                             
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 S25-WRITE-LINE        SECTION.                                           
022900     WRITE W52242-001-LINE FROM UT-LINE AFTER 1                           
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 S21-SEND-OPEN SECTION.                                                   
023400     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
023500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
023600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
023700                                     SEND-OPEN-AREA                       
023800     IF SEND-KDRC > ZERO                                                  
023900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
024000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
024100       DELIMITED BY SIZE        INTO ERRORTEXT                            
024200       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
024300     END-IF                                                               
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700 S22-PUT-HEADER SECTION.                                                  
024800     MOVE 1                           TO REQU-IDMSGVER                    
024900     MOVE 'R'                         TO REQU-KDPGMACT                    
025000     MOVE IDPGM                       TO REQU-IDUSER                      
025100     MOVE 'W522'                      TO HDR-IDOUTTYPE                    
025200     MOVE 'W52242'                    TO HDR-IDOUTREC                     
025210     MOVE FUNCTION CURRENT-DATE(3:10) TO HDR-IDLIST                       
025300     MOVE 'PUT'                       TO SEND-KDFUNC                      
025400     MOVE LENGTH OF HDR-AREA          TO SEND-KVDLEN                      
025500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
025600                                     SEND-KVDLEN                          
025700                                     HDR-AREA                             
025800     IF SEND-KDRC > ZERO                                                  
025900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
026000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
026100       DELIMITED BY SIZE        INTO ERRORTEXT                            
026200       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 S23-PUT-HDR1  SECTION.                                                   
026800     MOVE 'PUT'                   TO SEND-KDFUNC                          
026900     MOVE LENGTH OF UT-HDR1       TO SEND-KVDLEN                          
027000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
027100                                     SEND-KVDLEN                          
027200                                     UT-HDR1                              
027300     IF SEND-KDRC > ZERO                                                  
027400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
027500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
027600       DELIMITED BY SIZE        INTO ERRORTEXT                            
027700       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200 S24-PUT-HDR2  SECTION.                                                   
028300     MOVE 'PUT'                   TO SEND-KDFUNC                          
028400     MOVE LENGTH OF UT-HDR2       TO SEND-KVDLEN                          
028500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
028600                                     SEND-KVDLEN                          
028700                                     UT-HDR2                              
028800     IF SEND-KDRC > ZERO                                                  
028900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
029000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
029100       DELIMITED BY SIZE        INTO ERRORTEXT                            
029200       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 S25-PUT-LINE SECTION.                                                    
029800     MOVE 'PUT'                   TO SEND-KDFUNC                          
029900     MOVE LENGTH OF UT-LINE       TO SEND-KVDLEN                          
030000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
030100                                     SEND-KVDLEN                          
030200                                     UT-LINE                              
030300     IF SEND-KDRC > ZERO                                                  
030400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
030500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
030600       DELIMITED BY SIZE        INTO ERRORTEXT                            
030700       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 S26-PUT-LINE-NADA  SECTION.                                              
031300     MOVE 'PUT'                   TO SEND-KDFUNC                          
031400     MOVE LENGTH OF UT-NADA       TO SEND-KVDLEN                          
031500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
031600                                     SEND-KVDLEN                          
031700                                     UT-NADA                              
031800     IF SEND-KDRC > ZERO                                                  
031900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
032000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032100       DELIMITED BY SIZE        INTO ERRORTEXT                            
032200       CALL ABEND              USING RKOD-ABEND-WITH-DUMP                 
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600                                                                          
032700 S29-SEND-CLOSE SECTION.                                                  
032800     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
032900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
033000     .                                                                    
