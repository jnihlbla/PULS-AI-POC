000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6111100.                                                
000400*AUTHOR.         BERT ANDERSSON.                                          
000500*DATE-WRITTEN.   92/04/03.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER IGENOM EN FIL W6111010, SELEKTERAR              
001100*        UT ALLA MOTTAGNA EJ INLAGDA PARTIER.                             
001200*        DESSA PARTIER SÄNDS TILL SUBPROGRAM W611PRIO SOM                 
001300*        RÄKNAR UT PRIORITERING OCH UPPDATERAR W6D1.                      
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR W6CKPB (W6G2)                              
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL W61110                                               
002700     SELECT W61110                     ASSIGN TO W61111D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W61110                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600     SKIP2                                                                
003700*01   -COPY W6111001   -PRE IN-   -L.                                     
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W6111100'.            
004200 01  CHKP-VAR.                                                            
004300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004700 03  MAX-ANT-UPPDAT              PIC S9(3)   VALUE +100 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  WS-OLD-IDLOPNRM             PIC S9(9)   VALUE ZERO COMP-3.           
005100 77  ANT-POSTER-W61110           PIC S9(7)   VALUE ZERO COMP-3.           
005200 77  ANT-UPPDAT                  PIC S9(7)   VALUE ZERO COMP-3.           
005300**** ANT-UPPDATERINGAR BERÄKNAS I SUBPGM. W611PRIO.                       
005400*                                                                         
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800*                                                                         
005900 77  W61110-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W61110                       VALUE 'J'.                   
006100     SKIP2                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  W611PRIO                PIC X(8)    VALUE 'W611PRIO'.            
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     SKIP2                                                                
007300 01  IN-AREA                     PIC X(20)   VALUE '*IN-AREA*'.           
007400*01  POST  -COPY W6111001        -PRE IN-   -RED  IN-AREA                 
007500     SKIP2                                                                
007600*01   -COPY W611PRIO                                                      
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008600     03  W-IDARTNR-X.                                                     
008700         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008800     03  W-IDRADNR-X.                                                     
008900         05  W-IDRADNR           PIC S9(3)   VALUE ZERO COMP-3.           
009000     03  W-WDGX-6015-KEY-X.                                               
009100         05  W-IDHTYP-6015       PIC X(4)    VALUE '6015'.                
009200         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
009300     03  W-KDSEGKEY-X.                                                    
009400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA                 PIC X(32)  VALUE SPACE.                  
011800     SKIP3                                                                
011900     03  W6CKPB11 REDEFINES IO-AREA.                                      
012000*        05  -COPY W6GX6016                                               
012100     SKIP3                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012500     SKIP2                                                                
012600*01  -COPY W0008  -PRE CKPB-                                              
012700     05  FILLER                  PIC X.                                   
012800     SKIP2                                                                
012900*01  -COPY W0008  -PRE PRIO-INLA-                                         
013000     05  FILLER                  PIC X.                                   
013100     SKIP2                                                                
013200*01  -COPY W0008  -PRE PRIO-ARTC-                                         
013300     05  FILLER                  PIC X.                                   
013400     SKIP2                                                                
013500*01  -COPY W0008  -PRE PRIO-ARTM-                                         
013600     05  FILLER                  PIC X.                                   
013610     SKIP2                                                                
013620*01  -COPY W0008  -PRE PRIO-ORDQ-                                         
013630     05  FILLER                  PIC X.                                   
013700     SKIP2                                                                
013710*01  -COPY W0008  -PRE PRIO-ARTS-                                         
013720     05  FILLER                  PIC X.                                   
013730     SKIP2                                                                
013740*01  -COPY W0008  -PRE PRIO-KVAI-                                         
013750     05  FILLER                  PIC X.                                   
013760     SKIP2                                                                
013791*01  -COPY W0008  -PRE PRIO-INLI1-                                        
013792     05  FILLER                  PIC X.                                   
013793     SKIP2                                                                
013794*01  -COPY W0008  -PRE PRIO-KVAE-                                         
013795     05  FILLER                  PIC X.                                   
013796     SKIP2                                                                
013800     EJECT                                                                
013900 PROCEDURE DIVISION  USING MSG-PCB CKPB-PCB PRIO-INLA-PCB                 
014000                           PRIO-ARTC-PCB PRIO-ARTM-PCB                    
014010                           PRIO-ORDQ-PCB PRIO-ARTS-PCB                    
014020                           PRIO-KVAI-PCB                                  
014030                           PRIO-INLI1-PCB PRIO-KVAE-PCB.                  
014100     ENTRY 'DLITCBL' USING MSG-PCB CKPB-PCB PRIO-INLA-PCB                 
014200                           PRIO-ARTC-PCB PRIO-ARTM-PCB                    
014210                           PRIO-ORDQ-PCB PRIO-ARTS-PCB                    
014220                           PRIO-KVAI-PCB                                  
014230                           PRIO-INLI1-PCB PRIO-KVAE-PCB.                  
014300                                                                          
014400     PERFORM A-INIT                                                       
014500     PERFORM S01-LAES-W61110                                              
014600                                                                          
014700     PERFORM UNTIL END-OF-W61110                                          
014800                                                                          
014900       PERFORM B-KONTROLL-EV-CALL-W611PRIO                                
015000       IF ANT-UPPDAT > MAX-ANT-UPPDAT                                     
015100         PERFORM X-TAG-CHECKPOINT                                         
015200       END-IF                                                             
015300       PERFORM S01-LAES-W61110                                            
015400                                                                          
015500     END-PERFORM                                                          
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN INPUT W61110                                                    
016600                                                                          
016700     MOVE IDPGM                     TO POSTSUM-PROGNAMN                   
016800     MOVE '1'                       TO W-KDSEGKEY                         
016900     MOVE ZERO                      TO ANT-UPPDAT                         
017000                                       WS-OLD-IDLOPNRM                    
017100                                                                          
017200     PERFORM IMS-RESTART                                                  
017300     PERFORM IMS-LAS-ATERSTART                                            
017400                                                                          
017500     IF SEGMENT-FINNS                                                     
017700       IF 6016-KVPOST > ZERO                                              
017800         PERFORM AA-ATERSTART-EFTER-ABEND                                 
017900       END-IF                                                             
018000     END-IF                                                               
018100     .                                                                    
018200     SKIP2                                                                
018300 AA-ATERSTART-EFTER-ABEND SECTION.                                        
018400     SKIP2                                                                
018500     PERFORM UNTIL ANT-POSTER-W61110 = 6016-KVPOST OR                     
018600                                       END-OF-W61110                      
018800       PERFORM S01-LAES-W61110                                            
018900     END-PERFORM                                                          
019000     .                                                                    
019100     SKIP2                                                                
019200 B-KONTROLL-EV-CALL-W611PRIO SECTION.                                     
019400                                                                          
019900     IF IN-IDLOPNRM NOT = WS-OLD-IDLOPNRM                                 
020000       IF IN-TIINLMOT > 0                                                 
020100         IF IN-FLKLAR = 'N'                                               
020200           MOVE IN-IDDC             TO PRIO-IDDC                          
020400           MOVE IN-IDLEVNR          TO PRIO-IDLEVNR                       
020500           MOVE IN-IDFS             TO PRIO-IDFS                          
020600           MOVE IN-TIAVIDAT         TO PRIO-TIAVIDAT                      
020700           MOVE IN-IDRADNR-INL      TO PRIO-IDRADNR-INL                   
020800           MOVE ANT-UPPDAT          TO PRIO-KVUPPDAT                      
020810           MOVE 'W6111100'          TO PRIO-IDPGM                         
020900           CALL W611PRIO USING  PRIO-W611PRIO PRIO-INLA-PCB               
021000                                              PRIO-ARTC-PCB               
021100                                              PRIO-ARTM-PCB               
021110                                              PRIO-ORDQ-PCB               
021120                                              PRIO-ARTS-PCB               
021130                                              PRIO-KVAI-PCB               
021150                                              PRIO-INLI1-PCB              
021160                                              PRIO-KVAE-PCB               
021200           MOVE PRIO-KVUPPDAT       TO ANT-UPPDAT                         
021300         END-IF                                                           
021400       END-IF                                                             
021500       MOVE IN-IDLOPNRM             TO WS-OLD-IDLOPNRM                    
021600     END-IF                                                               
021800     .                                                                    
021900     SKIP2                                                                
022000 Z-FINIT SECTION.                                                         
022100                                                                          
022200     PERFORM IMS-LAS-ATERSTART                                            
022300     MOVE ZERO                 TO 6016-KVPOST                             
022400     ACCEPT 6016-TIUPPDAT FROM DATE                                       
022500     ACCEPT 6016-TIUPPTID FROM TIME                                       
022600     IF SEGMENT-SAKNAS                                                    
022700       MOVE '1'                TO 6016-KDSEGKEY                           
022800       PERFORM IMS-ISRT-ATERSTART                                         
022900     ELSE                                                                 
023000       PERFORM IMS-REPL-ATERSTART                                         
023100     END-IF                                                               
023200                                                                          
023300     CLOSE W61110                                                         
023400                                                                          
023500     MOVE 'S' TO POSTSUM-OPKOD                                            
023600                                                                          
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023900     .                                                                    
024000     SKIP2                                                                
024100 S01-LAES-W61110  SECTION.                                                
024300                                                                          
024400     READ W61110 INTO IN-POST                                             
024500     AT END                                                               
024600        SET END-OF-W61110           TO TRUE                               
024700                                                                          
024800     NOT AT END                                                           
024900        MOVE 'W61110'               TO POSTSUM-FDNAMN                     
025000        MOVE 'W61111D1'             TO POSTSUM-DDNAMN2                    
025100        MOVE 'PRIO'                 TO POSTSUM-TRANSTYP                   
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025300        ADD +1                      TO ANT-POSTER-W61110                  
025400     END-READ                                                             
025600     .                                                                    
025700     SKIP2                                                                
025800 X-TAG-CHECKPOINT   SECTION.                                              
026000                                                                          
026100     PERFORM IMS-LAS-ATERSTART                                            
026200     IF SEGMENT-SAKNAS                                                    
026300       MOVE LOW-VALUE               TO 6016-W6GX6016                      
026400       MOVE '1'                     TO 6016-KDSEGKEY                      
026500       MOVE +0                      TO 6016-KVPOST                        
026600     END-IF                                                               
026700     MOVE ANT-POSTER-W61110         TO 6016-KVPOST                        
026800     ACCEPT 6016-TIUPPDAT           FROM DATE                             
026900     ACCEPT 6016-TIUPPTID           FROM TIME                             
027000     IF SEGMENT-SAKNAS                                                    
027100       MOVE '1'                     TO 6016-KDSEGKEY                      
027200       PERFORM IMS-ISRT-ATERSTART                                         
027300     ELSE                                                                 
027400       PERFORM IMS-REPL-ATERSTART                                         
027500     END-IF                                                               
027600     PERFORM IMS-CHECKPOINT                                               
027700     MOVE ZERO                      TO ANT-UPPDAT                         
027800     .                                                                    
027900     EJECT                                                                
028000* --- IMS SEKTIONER ---                                                   
028100     SKIP3                                                                
028200 IMS-RESTART SECTION.                                                     
028300                                                                          
028400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028500     MOVE '  ' TO GODK-STATUSKODER                                        
028600     CALL CBLTDLI USING XRST MSG-PCB                                      
028700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028800                        CHKP-AREA-LENGTH CHKP-AREA                        
028900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029000     PERFORM IMS-STATUSKONTROLL                                           
029100     .                                                                    
029200     SKIP2                                                                
029300 IMS-CHECKPOINT SECTION.                                                  
029400                                                                          
029500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
029600     MOVE '  XD' TO GODK-STATUSKODER                                      
029700     CALL CBLTDLI USING CHKP MSG-PCB                                      
029800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
029900                        CHKP-AREA-LENGTH CHKP-AREA                        
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200                                                                          
030300     IF IMS-EJ-OK                                                         
030400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
030500       DISPLAY FELTEXT                                                    
030600       CALL FELLOG                                                        
030700     END-IF                                                               
030800     .                                                                    
030900     SKIP2                                                                
031000 IMS-REPL-ATERSTART SECTION.                                              
031100                                                                          
031200     MOVE SPACE TO GODK-STATUSKODER                                       
031300     CALL CBLTDLI USING REPL CKPB-PCB DLI-IO-AREA                         
031400     MOVE CKPB-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031700     SKIP2                                                                
031800 IMS-LAS-ATERSTART SECTION.                                               
031900                                                                          
032000     STRING 'W6CKPB01(W6GXKEY  =' W-WDGX-6015-KEY-X ')'                   
032100            DELIMITED BY SIZE INTO SSA1                                   
032200     STRING 'W6CKPB11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
032300            DELIMITED BY SIZE INTO SSA2                                   
032400     MOVE '  GE' TO GODK-STATUSKODER                                      
032500     CALL CBLTDLI USING GHU CKPB-PCB DLI-IO-AREA SSA1 SSA2                
032600     MOVE CKPB-STATUS-CODE TO STATUS-WS                                   
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     SKIP2                                                                
033000 IMS-ISRT-ATERSTART SECTION.                                              
033100                                                                          
033200     STRING 'W6CKPB01(W6GXKEY  =' W-WDGX-6015-KEY-X ')'                   
033300          DELIMITED BY SIZE INTO SSA1                                     
033400     MOVE 'W6CKPB11' TO SSA2                                              
033500     MOVE '  ' TO GODK-STATUSKODER                                        
033600     CALL CBLTDLI USING ISRT CKPB-PCB DLI-IO-AREA SSA1 SSA2               
033700     MOVE CKPB-STATUS-CODE TO STATUS-WS                                   
033800     PERFORM IMS-STATUSKONTROLL                                           
033900     .                                                                    
034000     SKIP2                                                                
034100 IMS-STATUSKONTROLL SECTION.                                              
034200                                                                          
034300     SET STATUS-IX TO 1                                                   
034400     SEARCH GODK-STATUS                                                   
034500       AT END                                                             
034600         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
034700         DISPLAY FELTEXT                                                  
034800         CALL FELLOG                                                      
034900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035000         CONTINUE                                                         
035100     END-SEARCH                                                           
035200     .                                                                    
