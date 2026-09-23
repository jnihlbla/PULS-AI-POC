000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017000.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE.                                  
000400 DATE-WRITTEN.   OCTOBER 2004                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.INVQUERYADJ                                 
000800*    WEB-LDC: WL017000 PROGRAM IS A REPLICA OF W5010500 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        INVENTORY QUERY, ADJUSTMENTS                                     
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: WL0170T                                             
001700*        REQUEST:     WL0170I1                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        RESPONSE:    WL0170O1                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'WL017000'.            
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003800 77  KDRC-DISPLAY                PIC Z(5).                                
003900 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
004000 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004100 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  SDC-NDC-FOUND               PIC X       VALUE 'N'.                   
005000 77  NUM-SDC-NDC                 PIC S9(9)   VALUE +0   COMP-3.           
005100 77  SDC-NDC-FINNS               PIC X       VALUE 'N'.                   
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 01  -COPY WWDCKONS                                                       
005710                                                                          
005720 77  INDX                        PIC S9(2)   VALUE +0   COMP SYNC.        
005800 77  ROW                         PIC S9(1)   VALUE +0   COMP SYNC.        
005900                                                                          
006000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006100     88  KEYS-OK                             VALUE 'J'.                   
006200     88  KEYS-WRONG                          VALUE 'N'.                   
006300                                                                          
006400 77  WS-IDELMT-ERROR             PIC X(16) VALUE SPACE.                   
006500 77  WS-IDMSG-ERROR              PIC X(03) VALUE SPACE.                   
006600 77  WS-IDMSG-INFO               PIC X(03) VALUE SPACE.                   
006700                                                                          
006800 01  W-KVJUSTYP-TOT.                                                      
006900     03 W-KVJUSTYP               PIC X.                                   
007000     03 W-FLAUTLSJ               PIC X.                                   
007100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007800     SKIP3                                                                
007900*    --- PARAMETERS TO ABEND                                              
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008400                                                                          
008500 01  FILLER              PIC X(16)  VALUE 'WDATAREA'.                     
008600*01      -COPY WDATAREA                                                   
008700                                                                          
008800 01  MESSAGE-CODES.                                                       
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009000                                                                          
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009800     SKIP3                                                                
009900*01  -COPY WZ01SUB                                                        
010000                                                                          
010100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010200     SKIP3                                                                
010300 01  REQU-AREA.                                                           
010400*    03  -COPY WZ01REQU                                                   
010500*    03  -COPY WL0170I1                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010800     SKIP3                                                                
010900 01  RESP-AREA.                                                           
011000*    03  -COPY WZ01RESP                                                   
011100*    03  -COPY WL0170O1                                                   
011200                                                                          
011300                                                                          
011400 01      NYCKLAR-TILL-DLI.                                                
011500                                                                          
011600   03    W-IDARTNR-X.                                                     
011700     05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
011800                                                                          
011900   03    W-IDDC-X.                                                        
012000     05  W-IDDC          PIC X(2)    VALUE SPACE.                         
012400                                                                          
012500   03    W-DAREGDAT-X.                                                    
012600     05  W-DAREGDAT      PIC 9(8)    VALUE 99999999.                      
012700******************************************************************        
012800*****                                                                     
012900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013000*****                                                                     
013100 01  IMS-WS.                                                              
013200   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
013300     SKIP3                                                                
013400*****                    **** STATUS-KOD FRÅN IMS                         
013500   03    STATUS-WS       PIC XX.                                          
013600         88  SEGMENT-FOUND       VALUE '  '.                              
013700         88  SEGMENT-MISSING     VALUE 'GE'.                              
013800         88  SEGMENT-EXISTS      VALUE 'II'.                              
013900                                                                          
014000   03    GOOD-STATUSCODES.                                                
014100     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200                                                                          
014300 01      SSA1            PIC X(128) VALUE SPACE.                          
014400                                                                          
014500*                            IMS FUNKTIONSKODER                           
014600*01      -COPY W0003                                                      
014700                                                                          
014800*                            DLI INPUT-OUTPUT AREA                        
014810*-------- WDK6-ARTIKELREG                                                 
014820                                                                          
014830 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
014840 01  DLI-IO-WDK601.                                                       
014850*  03  -COPY WDK601.                                                      
014860*-------- WDH7                                                            
014880 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDH701'.           
014890 01  DLI-IO-WDH701.                                                       
014891*  03  -COPY WDH701.                                                      
014892 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDH711'.           
014893 01  DLI-IO-WDH711.                                                       
014894*  03  -COPY WDH711.                                                      
015800                                                                          
015900     EJECT                                                                
016000 LINKAGE SECTION.                                                         
016100 01  MSG-PCB                     PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008     -PRE WDK6-                                           
016400         05  FILLER           PIC X.                                      
016500     EJECT                                                                
016600*01  -COPY W0008     -PRE WDH7-                                           
016700         05  FILLER           PIC X.                                      
016800     EJECT                                                                
016900                                                                          
017000 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDH7-PCB.                     
017100                                                                          
017200 MAIN SECTION.                                                            
017300     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDH7-PCB.                     
017400                                                                          
017500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017600     IF SUB-KDRC = 0                                                      
017700        IF REQU-KDPGMACT = 'S'                                            
017800           PERFORM A-INIT                                                 
017900           PERFORM B-CHECK-KEYS                                           
018000           IF KEYS-OK                                                     
018100              PERFORM F-READ-SHOW-INFO                                    
018200           END-IF                                                         
018300        END-IF                                                            
018400                                                                          
018500       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
018600       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
018700       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
018800       IF WS-IDMSG-ERROR NOT = SPACE                                      
018900           MOVE ALL '+' TO RESP-WL0170O1(1:10)                            
019000           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
019100           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
019200           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
019300           MOVE 001              TO RESP-IDMSGVER                         
019400       END-IF                                                             
019500       PERFORM S02-RETURN-RESPONSE                                        
019600     END-IF                                                               
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100                                                                          
020200 A-INIT SECTION.                                                          
020300                                                                          
020400     MOVE ALL '+'   TO RESP-AREA                                          
020500     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
020600                       RESP-IDMSG-INFO                                    
020700                       RESP-IDELMT-ERROR                                  
020800                       RESP-WL0170O1                                      
020900     MOVE 001       TO RESP-IDMSGVER                                      
021000     .                                                                    
021100                                                                          
021200 B-CHECK-KEYS SECTION.                                                    
021300                                                                          
021400     MOVE YES TO KEYS-SW                                                  
021500                                                                          
021600     IF REQU-IDARTNR-KEY NOT NUMERIC                                      
021700       MOVE '024'              TO RESP-IDMSG-ERROR                        
021800*      MUST BE NUMERIC ***                                                
021900       MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                       
022000       MOVE NOO                TO KEYS-SW                                 
022100     END-IF                                                               
022200                                                                          
022300     MOVE REQU-IDDC-KEY     TO RESP-IDDC-KEY                              
022400                                                                          
022500     .                                                                    
022600                                                                          
022700 F-READ-SHOW-INFO SECTION.                                                
022800                                                                          
022900     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
023000***                           RESP-IDARTNR-KEY                            
023100                                                                          
023200     PERFORM IMS-01-GET-CDC-ARTIKEL                                       
023300                                                                          
023400     IF SEGMENT-FOUND                                                     
023500        IF ART-KDERS-UTG > +0                                             
023600           MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                   
023700           MOVE '223'              TO RESP-IDMSG-ERROR                    
023800        END-IF                                                            
023900                                                                          
024000        PERFORM IMS-02-GU-ROT-INVHIST                                     
024100        IF SEGMENT-FOUND                                                  
024200           MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                      
024400           PERFORM FA-SDC-NDC-LDC                                         
024500        ELSE                                                              
024600           MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                   
024700           MOVE '025'              TO RESP-IDMSG-ERROR                    
024800        END-IF                                                            
024900     ELSE                                                                 
025000        MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                      
025100        MOVE '025'              TO RESP-IDMSG-ERROR                       
025200     END-IF                                                               
025300     .                                                                    
029400                                                                          
029500 FA-SDC-NDC-LDC   SECTION.                                                
029600     MOVE 'FB-SDC-NDC      '  TO CURR-SECTION                             
029700                                                                          
029800     MOVE REQU-IDDC-KEY TO W-IDDC                                         
029900     PERFORM IMS-04-GET-INVENTORY-DCFIRST                                 
030000     IF SEGMENT-FOUND                                                     
030100        MOVE NOO      TO SDC-NDC-FOUND                                    
030200        MOVE +1       TO INDX                                             
030300        MOVE 99999999 TO W-DAREGDAT                                       
030400                                                                          
030500        PERFORM UNTIL INDX > 48                                           
030600           IF SEGMENT-FOUND                                               
030700              MOVE YES TO SDC-NDC-FOUND                                   
030800              PERFORM S101-CONV-DATE                                      
030900              IF DAT-KDSVAR-OK                                            
031000                MOVE DAT-TIAAVVD TO RESP-TIJUSTDA (INDX)                  
031100              ELSE                                                        
031200                MOVE ZERO TO RESP-TIJUSTDA (INDX)                         
031300              END-IF                                                      
031400                                                                          
031500              MOVE INVH-KDJUSTYP TO W-KVJUSTYP                            
031600                                                                          
031700              IF INVH-FLAUTLSJ = 'J'                                      
031800                MOVE 'A'       TO W-FLAUTLSJ                              
031900              ELSE                                                        
032000                MOVE SPACE     TO W-FLAUTLSJ                              
032100              END-IF                                                      
032200                                                                          
032300              MOVE W-KVJUSTYP-TOT     TO RESP-KDJUSTYP (INDX)             
032400              MOVE INVH-KVJUSTKV      TO RESP-KVJUSTKV (INDX)             
032410              MOVE INVH-IDUSER-CLO    TO RESP-IDUSER-CLO (INDX)           
032600                                                                          
032700              IF INVH-KDJUSTYP NOT = 6                                    
032800                COMPUTE NUM-SDC-NDC =                                     
032900                                 NUM-SDC-NDC + INVH-KVJUSTKV              
033000              END-IF                                                      
033100                                                                          
033200              PERFORM IMS-05-GET-INVENTORY-SDC-NDC                        
033300           END-IF                                                         
033400           ADD +1 TO INDX                                                 
033500        END-PERFORM                                                       
033600                                                                          
033700        IF SDC-NDC-FOUND = YES                                            
033800           MOVE NUM-SDC-NDC TO RESP-SUINVJUST                             
033900        END-IF                                                            
034000                                                                          
034100     ELSE                                                                 
034300        MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                      
034400        MOVE '025'              TO RESP-IDMSG-ERROR                       
034500*       PART MISSING ***                                                  
034700     END-IF                                                               
034800     .                                                                    
034900                                                                          
035000*    --- DISPATCHER SECTIONS                                              
035100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
035200     MOVE 'S01-FETCH-REQUEST' TO CURR-SECTION                             
035300                                                                          
035400     MOVE 'GETARG'                   TO SUB-KDFUNC                        
035500     MOVE 'CARPARTS.LDC.INVQUERYADJ' TO SUB-ADDISPABS                     
035600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
035700                                                                          
035800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
035900                                                                          
036000     IF SUB-KDRC > 0                                                      
036100       MOVE SUB-KDRC       TO KDRC-DISPLAY                                
036200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
036300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036500     END-IF                                                               
036600     .                                                                    
036700                                                                          
036800 S02-RETURN-RESPONSE SECTION.                                             
036900     MOVE 'S02-RETURN-RESP  ' TO CURR-SECTION                             
037000                                                                          
037100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
037200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
037300                                                                          
037400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
037500                                                                          
037600     IF SUB-KDRC > 0                                                      
037700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
037800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
037900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
038000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038100     END-IF                                                               
038200     .                                                                    
038300                                                                          
038400 S101-CONV-DATE  SECTION.                                                 
038500     MOVE 'S101-CONV-DATE   ' TO CURR-SECTION                             
038600                                                                          
038700     MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                         
038800     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
038900                                                                          
039000     CALL WDATKONV USING  DAT-KDDATFORM                                   
039100                          DAT-I-TIDATUM                                   
039200                          DAT-O-TIDATUM                                   
039300                          DAT-KDSVAR                                      
039400     .                                                                    
039500 IMS-01-GET-CDC-ARTIKEL SECTION.                                          
039600     MOVE 'IMS-01     ' TO CURR-IMS-SECTION                               
039700                                                                          
039800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
039900            DELIMITED BY SIZE INTO SSA1                                   
040000     MOVE '  GE' TO GOOD-STATUSCODES                                      
040100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
040200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUS-CHECK                                             
040400     .                                                                    
040500                                                                          
040600 IMS-02-GU-ROT-INVHIST SECTION.                                           
040700     MOVE 'IMS-02     ' TO CURR-IMS-SECTION                               
040800                                                                          
040900     STRING 'WDH701  (IDARTNR  =' W-IDARTNR-X ')'                         
041000            DELIMITED BY SIZE INTO SSA1                                   
041100     MOVE '  GE' TO GOOD-STATUSCODES                                      
041200     CALL CBLTDLI USING GU WDH7-PCB DLI-IO-WDH701 SSA1                    
041300     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUS-CHECK                                             
041500     .                                                                    
042800                                                                          
042900 IMS-04-GET-INVENTORY-DCFIRST SECTION.                                    
043000     MOVE 'IMS-04     ' TO CURR-IMS-SECTION                               
043100                                                                          
043200     STRING 'WDH711  *F(DAREGDAT<=' W-DAREGDAT-X                          
043300                    '&IDDC     =' W-IDDC-X ')'                            
043400            DELIMITED BY SIZE INTO SSA1                                   
043500     MOVE '  GE' TO GOOD-STATUSCODES                                      
043600     CALL CBLTDLI USING GNP WDH7-PCB DLI-IO-WDH711 SSA1                   
043700     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
043800     PERFORM IMS-STATUS-CHECK                                             
043900     .                                                                    
044000                                                                          
044100 IMS-05-GET-INVENTORY-SDC-NDC SECTION.                                    
044200     MOVE 'IMS-05     ' TO CURR-IMS-SECTION                               
044300                                                                          
044400     STRING 'WDH711  (DAREGDAT<=' W-DAREGDAT-X                            
044500                    '&IDDC     =' W-IDDC-X ')'                            
044600            DELIMITED BY SIZE INTO SSA1                                   
044700     MOVE '  GE' TO GOOD-STATUSCODES                                      
044800     CALL CBLTDLI USING GNP WDH7-PCB DLI-IO-WDH711 SSA1                   
044900     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUS-CHECK                                             
045100     .                                                                    
045200 IMS-STATUS-CHECK   SECTION.                                              
045300                                                                          
045400     SET STATUS-IX TO 1                                                   
045500     SEARCH GOOD-STATUS                                                   
045600       AT END                                                             
045700         CALL FELLOG                                                      
045800     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
045900       CONTINUE                                                           
046000     END-SEARCH                                                           
046100     .                                                                    
