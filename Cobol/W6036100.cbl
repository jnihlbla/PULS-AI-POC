000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6036100.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   12/01/26.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        BACKGROUND MPP FOR PRINTING CHINESE LABELS                       
000900*        ON WEB (CHINESE DC'S)                                            
001000*                                                                         
001100*        PROGRAMMET          READS      WDK6                              
001200*        PROGRAMMET          READS      WDK7                              
001300*        PROGRAMMET          READS      WDB6                              
001400*    SUB PROGRAMMET W612LABL READS      WDD3                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W60361T                                             
001800*        REQUEST:     W60361I1                                            
001900*                                                                         
002000     SKIP3                                                                
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6036100'.            
002900 77  JA                          PIC X       VALUE 'Y'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
003200 77  MAX-INDX                    PIC S9(4)   VALUE +500 COMP SYNC.        
003300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
003400     88  INDATA-OK                           VALUE 'Y'.                   
003500     88  INDATA-FEL                          VALUE 'N'.                   
003600*      --- VALID IDDC CODES                                               
003700*01    -COPY WWDC99                                                       
003800                                                                          
003900*    --- PARAMETRAR TILL SUBPROGRAM W612LABL                              
004000*01    -COPY W612LABL PRE LABL-                                           
004100                                                                          
004200*01    -COPY WPLKSUMM                                                     
004300                                                                          
004400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
004500 01  GENERELLA-SUBPROGRAM.                                                
004600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
004800     03  W612LABL                PIC X(8)    VALUE 'W612LABL'.            
004900     03  W612MRP                 PIC X(8)    VALUE 'W612MRP '.            
005000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005200     EJECT                                                                
005300 01  MESSAGE-CODES.                                                       
005400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005500     03  ERR-NOT-FOUND           PIC X(3)    VALUE '025'.                 
005600     03  ERR-IS-INVALID          PIC X(3)    VALUE '023'.                 
005700     03  ERR-NO-DATA             PIC X(3)    VALUE '014'.                 
005800     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '312'.                 
005900     EJECT                                                                
006000 01  ERROR-MESSAGES.                                                      
006100     03  PART-NO-MSG.                                                     
006200         05 FILLER               PIC X(8)    VALUE 'Part No.'.            
006300         05 PART-NO              PIC Z(7)9.                               
006400         05 PART-NO-NON-NUMERIC REDEFINES PART-NO PIC X(8).               
006500     EJECT                                                                
006600 01  KDRC-DISPLAY                PIC Z(5).                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000     EJECT                                                                
007100*                                                                         
007200*    --- PARAMETERS TO ABEND                                              
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007600*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007800     SKIP3                                                                
007900*01  -COPY WZ01SUB                                                        
008000     SKIP3                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008200*01  -COPY WZ01SEND                                                       
008300     SKIP3                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008500     SKIP3                                                                
008600 01  REQU-AREA.                                                           
008700*    03  -COPY WZ01REQU                                                   
008800*    03  -COPY W60361I1                                                   
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009100     SKIP3                                                                
009200 01  RESP-AREA.                                                           
009300*    03  -COPY WZ01RESP                                                   
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-IDARTNR-X.                                                     
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100     03  W-IDDC-X.                                                        
010200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010300     03  W-IDLAND-X.                                                      
010400         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
010500     SKIP2                                                                
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FOUND                       VALUE '  '.                  
010900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(128).                              
011500 01  SSA2                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNKTIONSKODER                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012200     SKIP3                                                                
012300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
012400 01  DLI-IO-WDK611.                                                       
012500*    03  -COPY WDK611                                                     
012600 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK701'.        
012700 01  DLI-IO-WDK701.                                                       
012800*    03  -COPY WDK701                                                     
012900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK712'.        
013000 01  DLI-IO-WDK712.                                                       
013100*    03  -COPY WDK712                                                     
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
013400 01  DLI-IO-WDB601.                                                       
013500*    03  -COPY WDB601                                                     
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800*01  -COPY W0009  -PRE MSG-                                               
013900     EJECT                                                                
014000 01  DISTRWEB-PCB                PIC X.                                   
014100 01  DISTRWE2-PCB                PIC X.                                   
014200     EJECT                                                                
014300*01  -COPY W0008  -PRE WDK6-                                              
014400     05  FILLER                  PIC X.                                   
014500*01  -COPY W0008  -PRE WDK7-                                              
014600     05  FILLER                  PIC X.                                   
014700*01  -COPY W0008  -PRE WDB6-                                              
014800     05  FILLER                  PIC X.                                   
014900*   PCB'ER FÖR SUBPGM W612LABL                                            
015000 01  WDD3-PCB                    PIC X.                                   
015100 01  WDT4-PCB                    PIC X.                                   
015200 01  LABL-WDB6-PCB               PIC X.                                   
015300*   PCB'ER FÖR SUBPGM W612MRP                                             
015400 01  MRP-WDD3-PCB                PIC X.                                   
015500 01  MRP-WDK6-PCB                PIC X.                                   
015600 01  MRP-WDG2-PCB                PIC X.                                   
015700 01  MRP-WDC3-PCB                PIC X.                                   
015800 01  MRP-WDJ1-PCB                PIC X.                                   
015900 01  MRP-WDJ4-PCB                PIC X.                                   
016000 01  MRP-WDD3A-PCB               PIC X.                                   
016100     EJECT                                                                
016200 PROCEDURE DIVISION  USING MSG-PCB DISTRWEB-PCB DISTRWE2-PCB              
016300                           WDK6-PCB WDK7-PCB WDB6-PCB WDD3-PCB            
016400                           WDT4-PCB LABL-WDB6-PCB                         
016500                           MRP-WDD3-PCB MRP-WDK6-PCB                      
016600                           MRP-WDG2-PCB MRP-WDC3-PCB                      
016700                           MRP-WDJ1-PCB MRP-WDJ4-PCB                      
016800                           MRP-WDD3A-PCB.                                 
016900 MAIN SECTION.                                                            
017000                                                                          
017100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017200     IF SUB-KDRC = 0                                                      
017300       PERFORM A-INIT                                                     
017400       PERFORM B-VALIDATE-INPUT-DATA                                      
017500       IF INDATA-OK                                                       
017600         PERFORM C-PROCESS-REQU-DATA                                      
017700       ELSE                                                               
017800         PERFORM S02-RETURN-RESPONSE                                      
017900       END-IF                                                             
018000     END-IF                                                               
018100     MOVE ZERO                        TO RETURN-CODE                      
018200     GOBACK                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 A-INIT SECTION.                                                          
018600     MOVE JA                          TO INDATA-SW                        
018700     INITIALIZE LABL-W612LABL                                             
018800     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
018900                                         RESP-IDMSG-INFO                  
019000                                         RESP-IDELMT-ERROR                
019100     MOVE '001'                       TO RESP-IDMSGVER                    
019200     MOVE REQU-IDDC                   TO WS-IDDC                          
019300     .                                                                    
019400     EJECT                                                                
019500 B-VALIDATE-INPUT-DATA SECTION.                                           
019600*    VALIDATE IDDC                                                        
019700     IF LDC-CN OR NDC-CN OR NDC-IN                                        
019800       MOVE WS-IDDC                   TO W-IDDC                           
019900     ELSE                                                                 
020000       MOVE NEJ                       TO INDATA-SW                        
020100       MOVE 'IDDC'                    TO RESP-IDELMT-ERROR                
020200       MOVE ERR-WRONG-KEY             TO RESP-IDMSG-ERROR                 
020300     END-IF                                                               
020400*                                                                         
020500     PERFORM                                                              
020600     VARYING INDX FROM +1 BY +1                                           
020700       UNTIL (REQU-IDARTNR (INDX) = 0) OR                                 
020800             (INDX > MAX-INDX)         OR                                 
020900             (INDATA-FEL)                                                 
021000*      VALIDATE IDARTNR                                                   
021100       IF INDATA-OK                                                       
021200         IF REQU-IDARTNR (INDX) IS NUMERIC                                
021300           MOVE REQU-IDARTNR (INDX)   TO W-IDARTNR                        
021400           PERFORM IMS-GU-WDK701                                          
021500           IF SEGMENT-MISSING                                             
021600             MOVE NEJ                 TO INDATA-SW                        
021700             MOVE REQU-IDARTNR (INDX) TO PART-NO                          
021800             MOVE PART-NO-MSG         TO RESP-IDELMT-ERROR                
021900             MOVE ERR-NOT-FOUND       TO RESP-IDMSG-ERROR                 
022000           END-IF                                                         
022100         ELSE                                                             
022200           MOVE NEJ                   TO INDATA-SW                        
022300           MOVE REQU-IDARTNR (INDX)   TO PART-NO-NON-NUMERIC              
022400           MOVE PART-NO-MSG           TO RESP-IDELMT-ERROR                
022500           MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                 
022600         END-IF                                                           
022700       END-IF                                                             
022800*                                                                         
022900*      VALIDATE KVANTAL                                                   
023000       IF INDATA-OK                                                       
023100         IF REQU-KVANTAL (INDX) IS NUMERIC AND                            
023200            REQU-KVANTAL (INDX) > 0                                       
023300           CONTINUE                                                       
023400         ELSE                                                             
023500           MOVE NEJ                   TO INDATA-SW                        
023600           MOVE 'KVANTAL'             TO RESP-IDELMT-ERROR                
023700           MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                 
023800         END-IF                                                           
023900       END-IF                                                             
024000*                                                                         
024100*    VALIDATE IDDC                                                        
024200     IF NDC-IN                                                            
024300*      VALIDATE KVQPACK                                                   
024400       IF INDATA-OK                                                       
024500         IF REQU-KVQPACK (INDX) IS NUMERIC AND                            
024600            REQU-KVQPACK (INDX) >= 0                                      
024700           CONTINUE                                                       
024800         ELSE                                                             
024900           MOVE NEJ                   TO INDATA-SW                        
025000           MOVE 'KVQPACK'             TO RESP-IDELMT-ERROR                
025100           MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                 
025200         END-IF                                                           
025300       END-IF                                                             
025400     END-IF                                                               
025500*                                                                         
025600     END-PERFORM                                                          
025700*    IF INDEX IS '1', THEN NO DATA IS ENTERED                             
025800     IF INDX = +1                                                         
025900       MOVE NEJ                       TO INDATA-SW                        
026000       MOVE ERR-NO-DATA               TO RESP-IDMSG-ERROR                 
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 C-PROCESS-REQU-DATA SECTION.                                             
026500     PERFORM                                                              
026600     VARYING INDX FROM +1 BY +1                                           
026700       UNTIL (REQU-IDARTNR (INDX) = 0) OR                                 
026800             (INDX > MAX-INDX)         OR                                 
026900             (INDATA-FEL)                                                 
027000*      GET COUNTRY OF ORIGIN FROM WDK712 OR WDK611                        
027100*      BUT FIRST GET IDLAND FROM WDB601                                   
027200       PERFORM IMS-GU-WDB601                                              
027300       MOVE REQU-IDARTNR (INDX)       TO W-IDARTNR                        
027400       MOVE DCS-IDLANDX2              TO W-IDLAND                         
027500       PERFORM IMS-GU-WDK712                                              
027600       IF SEGMENT-FOUND AND                                               
027700          LART-KDARTURS NOT = SPACE                                       
027800         MOVE LART-KDARTURS           TO LABL-KDARTURS (INDX)             
027900       ELSE                                                               
028000         PERFORM IMS-GU-WDK611                                            
028100         IF SEGMENT-FOUND AND                                             
028200            CLAG-KDARTURS NOT = SPACE                                     
028300           MOVE CLAG-KDARTURS         TO LABL-KDARTURS (INDX)             
028400         ELSE                                                             
028500           MOVE NEJ                   TO INDATA-SW                        
028600           MOVE REQU-IDARTNR (INDX)   TO PART-NO                          
028700           MOVE PART-NO-MSG           TO RESP-IDELMT-ERROR                
028800           MOVE ERR-IS-INVALID        TO RESP-IDMSG-ERROR                 
028900           MOVE ERR-ORIGIN-MISSING    TO RESP-IDMSG-INFO                  
029000         END-IF                                                           
029100       END-IF                                                             
029200       MOVE W-IDARTNR                 TO LABL-IDARTNR  (INDX)             
029300       MOVE W-IDDC                    TO LABL-IDDC                        
029400       MOVE REQU-KVANTAL (INDX)       TO LABL-KVANTAL (INDX)              
029500       IF NDC-IN                                                          
029600         MOVE REQU-KVQPACK (INDX)     TO LABL-KVQPACK (INDX)              
029700       ELSE                                                               
029800         MOVE +0                      TO LABL-KVQPACK (INDX)              
029900       END-IF                                                             
030000     END-PERFORM                                                          
030100                                                                          
030200     COMPUTE LABL-KVRADER = INDX - 1                                      
030300                                                                          
030400     IF INDATA-OK                                                         
030500       IF NDC-IN                                                          
030600*MRP INDIA LABEL                                                          
030700         INITIALIZE PLK-SUMM-WPLKSUMM                                     
030800                                                                          
030900         CALL W612MRP              USING LABL-W612LABL                    
031000* PLKSUMM is ONLY a placeholder. Valid only when MRP labels               
031100* are created during picking process.                                     
031200                                           PLK-SUMM-WPLKSUMM              
031300                                           REQU-WZ01REQU                  
031400                                           DISTRWEB-PCB                   
031500                                           DISTRWE2-PCB                   
031600                                           MRP-WDD3-PCB                   
031700                                           MRP-WDK6-PCB                   
031800                                           MRP-WDG2-PCB                   
031900                                           MRP-WDC3-PCB                   
032000                                           MRP-WDJ1-PCB                   
032100                                           MRP-WDJ4-PCB                   
032200                                           MRP-WDD3A-PCB                  
032300       ELSE                                                               
032400*        CALL W612LABL TO CREATE CHINESE LABLES                           
032500         CALL W612LABL             USING LABL-W612LABL                    
032600                                           REQU-WZ01REQU                  
032700                                           DISTRWEB-PCB                   
032800                                           WDD3-PCB                       
032900                                           WDT4-PCB                       
033000                                           LABL-WDB6-PCB                  
033100       END-IF                                                             
033200     ELSE                                                                 
033300       PERFORM S02-RETURN-RESPONSE                                        
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700*                                                                         
033800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
033900     MOVE 'GETARG'                    TO SUB-KDFUNC                       
034000     MOVE 'CARPARTS.NDC.CREATELABELS' TO SUB-ADDISPABS                    
034100     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
034200                                                                          
034300     CALL WZ01SUB                  USING SUB-CONTROL-AREA                 
034400                                         SUB-KVDLEN                       
034500                                         REQU-AREA                        
034600                                                                          
034700     IF SUB-KDRC > 0                                                      
034800       MOVE SUB-KDRC                  TO KDRC-DISPLAY                     
034900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
035000         DELIMITED BY SIZE INTO FELTEXT                                   
035100       CALL ABEND                  USING RKOD-ABEND-WITH-DUMP             
035200     END-IF                                                               
035300     .                                                                    
035400     SKIP3                                                                
035500 S02-RETURN-RESPONSE SECTION.                                             
035600     MOVE 'RETURN'                    TO SUB-KDFUNC                       
035700     MOVE LENGTH OF RESP-AREA         TO SUB-KVDLEN                       
035800                                                                          
035900     CALL WZ01SUB                  USING SUB-CONTROL-AREA                 
036000                                         SUB-KVDLEN                       
036100                                         RESP-AREA                        
036200                                                                          
036300     IF SUB-KDRC > 0                                                      
036400       MOVE SUB-KDRC                  TO KDRC-DISPLAY                     
036500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
036600         DELIMITED BY SIZE INTO FELTEXT                                   
036700       CALL ABEND                  USING RKOD-ABEND-WITH-DUMP             
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 IMS-GU-WDK611 SECTION.                                                   
037200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
037300       DELIMITED BY SIZE            INTO SSA1                             
037400     MOVE 'WDK611  '                  TO SSA2                             
037500     MOVE '  '                        TO GODK-STATUSKODER                 
037600     CALL CBLTDLI                  USING GU                               
037700                                         WDK6-PCB                         
037800                                         DLI-IO-WDK611                    
037900                                         SSA1                             
038000                                         SSA2                             
038100     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     SKIP3                                                                
038500 IMS-GU-WDK701 SECTION.                                                   
038600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
038700       DELIMITED BY SIZE INTO SSA1                                        
038800     MOVE '  GE'                      TO GODK-STATUSKODER                 
038900     CALL CBLTDLI                  USING GU                               
039000                                         WDK7-PCB                         
039100                                         DLI-IO-WDK701                    
039200                                         SSA1                             
039300     MOVE WDK7-STATUS-CODE            TO STATUS-WS                        
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     EJECT                                                                
039700 IMS-GU-WDK712 SECTION.                                                   
039800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
039900       DELIMITED BY SIZE INTO SSA1                                        
040000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
040100       DELIMITED BY SIZE INTO SSA2                                        
040200     MOVE '  GE' TO GODK-STATUSKODER                                      
040300     CALL CBLTDLI                  USING GU                               
040400                                         WDK7-PCB                         
040500                                         DLI-IO-WDK712                    
040600                                         SSA1                             
040700                                         SSA2                             
040800     MOVE WDK7-STATUS-CODE            TO STATUS-WS                        
040900     PERFORM IMS-STATUSKONTROLL                                           
041000     .                                                                    
041100     EJECT                                                                
041200 IMS-GU-WDB601 SECTION.                                                   
041300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
041400       DELIMITED BY SIZE INTO SSA1                                        
041500     MOVE '    ' TO GODK-STATUSKODER                                      
041600     CALL CBLTDLI                  USING GU                               
041700                                         WDB6-PCB                         
041800                                         DLI-IO-WDB601                    
041900                                         SSA1                             
042000     MOVE WDB6-STATUS-CODE            TO STATUS-WS                        
042100     PERFORM IMS-STATUSKONTROLL                                           
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-STATUSKONTROLL SECTION.                                              
042500                                                                          
042600     SET STATUS-IX                    TO 1                                
042700     SEARCH GODK-STATUS                                                   
042800       AT END                                                             
042900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
043000         DELIMITED BY SIZE INTO FELTEXT                                   
043100         CALL FELLOG                                                      
043200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043300         CONTINUE                                                         
043400     END-SEARCH                                                           
043500     .                                                                    
