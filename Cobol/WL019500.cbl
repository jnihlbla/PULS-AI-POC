000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019500.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE.                                  
000400 DATE-WRITTEN.   NOVEMBER  2005                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.LDC.XREFRENAULT                                    
000800*    WEB-LDC: WL019500 PROGRAM IS A REPLICA OF W1010700 PROGRAM           
000900*             AND W1010800                                                
001000*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        CROSS INDEX                                                      
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WL0195T                                             
001800*        REQUEST:     WL0195I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    WL0195O1                                            
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000                                                                          
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'WL019500'.            
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700 77  CURR-DISPLAY                PIC X(16) VALUE 'MAIN'.                  
003800 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
003900 77  CURR-DISP-SECTION           PIC X(16) VALUE SPACE.                   
004000 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004100                                                                          
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400 77  ART-IX                      PIC 9(3)    VALUE ZERO.                  
004500 77  INDX                        PIC 9(3)    VALUE ZERO.                  
004600 77  INDX-MAX                    PIC 9(3)    VALUE 500.                   
004700 77  WS-KVRADER                  PIC 9(3)    VALUE ZERO.                  
004800 77  WS-IDLEVNR-3636             PIC X(5)    VALUE '3636 '.               
004900 77  WS-IDLEVNR-DLJMA            PIC X(5)    VALUE 'DLJMA'.               
005000 77  BELEVART-WS                    PIC X(30).                            
005100                                                                          
005200 01  VOLVO-IDARTNR-TAB.                                                   
005300     03  VOLVO-IDARTNR PIC 9(9)  OCCURS 500.                              
005400                                                                          
005500 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005600     88  KEYS-OK                             VALUE 'J'.                   
005700     88  KEYS-WRONG                          VALUE 'N'.                   
005800                                                                          
005900 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
006000     88  IDARTNR-OK                          VALUE 'J'.                   
006100                                                                          
006200 77  WS-IDELMT-ERROR             PIC X(16) VALUE SPACE.                   
006300 77  WS-IDMSG-ERROR              PIC X(03) VALUE SPACE.                   
006400 77  WS-IDMSG-INFO               PIC X(03) VALUE SPACE.                   
006500                                                                          
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007300     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
007400                                                                          
007500*    --- PARAMETERS TO ABEND                                              
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000                                                                          
008400 01  FILLER              PIC X(16)  VALUE 'WWDC99'.                       
008500*01      -COPY WWDC99                                                     
008600                                                                          
008700 01  FILLER              PIC X(16)  VALUE 'WDATAREA'.                     
008800*01      -COPY WDATAREA                                                   
008900                                                                          
009000 01  MESSAGE-CODES.                                                       
009100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009200                                                                          
009300*                                                                         
009400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009500                                                                          
009600*01  -COPY WZ01SUB                                                        
009700                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009900                                                                          
010000 01  REQU-AREA.                                                           
010100*    03  -COPY WZ01REQU                                                   
010200*    03  -COPY WL0195I1                                                   
010300                                                                          
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010600                                                                          
010700 01  RESP-AREA.                                                           
010800*    03  -COPY WZ01RESP                                                   
010900*    03  -COPY WL0195O1                                                   
011000                                                                          
011100                                                                          
011200 01      NYCKLAR-TILL-DLI.                                                
011300                                                                          
011400   03    W-IDARTNR-X.                                                     
011500     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
011600                                                                          
011700   03    W-IDSKYLT               PIC X(3)    VALUE 'GB'.                  
011800                                                                          
011900   03  W-WDF5BSEQ.                                                        
012000     05  W-SEQB-IDLEVART         PIC X(30)   VALUE LOW-VALUE.             
012100                                                                          
012200******************************************************************        
012300*****                                                                     
012400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012500*****                                                                     
012600 01  IMS-WS.                                                              
012700   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
012800                                                                          
012900*****                    **** STATUS-KOD FRÅN IMS                         
013000   03    STATUS-WS       PIC XX.                                          
013100         88  SEGMENT-FOUND       VALUE '  '.                              
013200         88  SEGMENT-MISSING     VALUE 'GE'.                              
013300         88  END-OF-DB           VALUE 'GB'.                              
013400                                                                          
013500   03    GOOD-STATUSCODES.                                                
013600     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700                                                                          
013800 01      SSA1            PIC X(128) VALUE SPACE.                          
013900 01      SSA2            PIC X(128) VALUE SPACE.                          
014000                                                                          
014100*                            IMS FUNKTIONSKODER                           
014200*01      -COPY W0003                                                      
014300                                                                          
014400*                            DLI INPUT-OUTPUT AREA                        
014500 01      DLI-IO-AREA     PIC X(200)  VALUE SPACE.                         
014600                                                                          
014700 01  FILLER              PIC X(16)   VALUE 'WDF501-AREA'.                 
014800 01  DLI-IO-WDF501.                                                       
014900*    03  -COPY WDF501                                                     
015000                                                                          
015100 01  FILLER              PIC X(16)   VALUE 'WDF502-AREA'.                 
015200 01  DLI-IO-WDF502.                                                       
015300*    03  -COPY WDF502                                                     
015400                                                                          
015500 01  FILLER              PIC X(16)   VALUE 'WDD311-AREA'.                 
015600 01  DLI-IO-WDD311.                                                       
015700*    03  -COPY WDD311   -PRE 311-                                         
015800                                                                          
015900 01  FILLER              PIC X(16)   VALUE 'WDK601-AREA'.                 
016000 01  DLI-IO-WDK601.                                                       
016100*    03  -COPY WDK601   -PRE 601-                                         
016200                                                                          
016300 LINKAGE SECTION.                                                         
016400 01  MSG-PCB                     PIC X.                                   
016500                                                                          
016600*01  -COPY W0008     -PRE WDF5-                                           
016700         05  FILLER           PIC X.                                      
016800                                                                          
016900*01  -COPY W0008     -PRE WDF5B-                                          
017000         05  FILLER           PIC X.                                      
017100                                                                          
017200*01  -COPY W0008     -PRE WDD3-                                           
017300         05  FILLER           PIC X.                                      
017400                                                                          
017500*01  -COPY W0008     -PRE WDK6-                                           
017600         05  FILLER           PIC X.                                      
017700                                                                          
017800                                                                          
017900 PROCEDURE DIVISION  USING MSG-PCB  WDF5-PCB WDF5B-PCB                    
018000                           WDD3-PCB WDK6-PCB.                             
018100                                                                          
018200 MAIN SECTION.                                                            
018300     ENTRY 'DLITCBL' USING MSG-PCB  WDF5-PCB WDF5B-PCB                    
018400                           WDD3-PCB WDK6-PCB.                             
018500                                                                          
018600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018700     IF SUB-KDRC = 0                                                      
018800        IF REQU-KDPGMACT = 'S'                                            
018900           PERFORM A-INIT                                                 
019000           PERFORM B-CHECK-KEYS                                           
019100           IF KEYS-OK                                                     
019200              PERFORM F-READ-SHOW-INFO                                    
019300           END-IF                                                         
019400        END-IF                                                            
019500                                                                          
019600       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
019700       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
019800       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
019900       IF WS-IDMSG-ERROR NOT = SPACE                                      
020000           MOVE ALL '+' TO RESP-WL0195O1(1:10)                            
020100           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
020200           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
020300           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
020400           MOVE 001              TO RESP-IDMSGVER                         
020500       END-IF                                                             
020600       PERFORM S02-RETURN-RESPONSE                                        
020700     END-IF                                                               
020800                                                                          
020900     MOVE ZERO TO RETURN-CODE                                             
021000     GOBACK                                                               
021100     .                                                                    
021200                                                                          
021300 A-INIT SECTION.                                                          
021400     MOVE 'A-INIT           ' TO CURR-SECTION                             
021500                                                                          
021600     MOVE ALL '+'   TO RESP-AREA                                          
021700     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
021800                       RESP-IDMSG-INFO                                    
021900                       RESP-IDELMT-ERROR                                  
022000                       RESP-WL0195O1                                      
022100     MOVE 001       TO RESP-IDMSGVER                                      
022200                                                                          
022300     MOVE +1        TO ART-IX                                             
022400     PERFORM UNTIL ART-IX > INDX-MAX                                      
022500        MOVE ZERO   TO VOLVO-IDARTNR(ART-IX)                              
022600        ADD +1      TO ART-IX                                             
022700     END-PERFORM                                                          
022800     .                                                                    
022900                                                                          
023000 B-CHECK-KEYS SECTION.                                                    
023100     MOVE 'B-CHECK-KEYS     ' TO CURR-SECTION                             
023200                                                                          
023300     MOVE YES TO KEYS-SW                                                  
023400                                                                          
023500     IF REQU-IDARTNR-KEY = ALL '+' AND                                    
023600        REQU-BELEVART-KEY = ALL '+'                                       
023700        MOVE '026'              TO RESP-IDMSG-ERROR                       
023800*       INVALID         ***                                               
023900        MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                      
024000        MOVE NOO                TO KEYS-SW                                
024100     END-IF                                                               
024200                                                                          
024300     IF REQU-IDARTNR-KEY NOT = ALL '+' AND                                
024400        REQU-BELEVART-KEY NOT = ALL '+'                                   
024500        MOVE '032'              TO RESP-IDMSG-ERROR                       
024600*       INVALID COMBINATION  ***                                          
024700        MOVE 'BELEV'            TO RESP-IDELMT-ERROR                      
024800        MOVE NOO                TO KEYS-SW                                
024900        MOVE REQU-IDARTNR-KEY   TO RESP-IDARTNR-KEY                       
025000        MOVE REQU-BELEVART-KEY  TO RESP-BELEVART-KEY                      
025100     END-IF                                                               
025200                                                                          
025300     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
025400        IF REQU-IDARTNR-KEY NOT NUMERIC                                   
025500           MOVE '024'              TO RESP-IDMSG-ERROR                    
025600*          MUST BE NUMERIC ***                                            
025700           MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                   
025800           MOVE NOO                TO KEYS-SW                             
025900        END-IF                                                            
026000     END-IF                                                               
026100                                                                          
026200     MOVE REQU-IDDC-KEY            TO RESP-IDDC-KEY                       
026300                                      WS-IDDC                             
026400     IF KEYS-OK                                                           
026500        IF REQU-IDARTNR-KEY NOT = ALL '+'                                 
026600           MOVE REQU-IDARTNR-KEY   TO RESP-IDARTNR-KEY                    
026700        END-IF                                                            
026800        IF REQU-BELEVART-KEY NOT = ALL '+'                                
026900           MOVE REQU-BELEVART-KEY  TO RESP-BELEVART-KEY                   
027000        END-IF                                                            
027100     END-IF                                                               
027200                                                                          
027300     .                                                                    
027400                                                                          
027500 F-READ-SHOW-INFO SECTION.                                                
027600     MOVE 'F-READ-SHOW-INFO ' TO CURR-SECTION                             
027700                                                                          
027800     MOVE ZERO TO WS-KVRADER                                              
027900     IF REQU-BELEVART-KEY = ALL '+'                                       
028000        PERFORM FA-SHOW-VOLVO                                             
028100     ELSE                                                                 
028200        PERFORM FB-SHOW-OTHER                                             
028300     END-IF                                                               
028400     MOVE WS-KVRADER TO RESP-KVRADER                                      
028500     IF WS-KVRADER = ZERO                                                 
028600        MOVE '025'              TO RESP-IDMSG-ERROR                       
028700*       NOT FOUND       ***                                               
028800        IF REQU-IDARTNR-KEY NOT = ALL '+'                                 
028900           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
029000        ELSE                                                              
029100           MOVE 'BELEV'         TO RESP-IDELMT-ERROR                      
029200        END-IF                                                            
029300     END-IF                                                               
029400                                                                          
029500     .                                                                    
029600                                                                          
029700 FA-SHOW-VOLVO    SECTION.                                                
029800     MOVE 'FA-SHOW-VOLVO    ' TO CURR-SECTION                             
029900                                                                          
030000     MOVE +1               TO INDX                                        
030100     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
030200                                                                          
030300     PERFORM IMS-01-GU-WDF501                                             
030400     IF SEGMENT-FOUND                                                     
030500       MOVE XART-IDARTNR TO W-IDARTNR                                     
030600                                                                          
030700       PERFORM IMS-04-GU-WDK601                                           
030800       IF SEGMENT-FOUND                                                   
031200         PERFORM IMS-03-GU-WDD311                                         
031300                                                                          
031400         PERFORM IMS-02-GNP-WDF502                                        
031500         PERFORM UNTIL SEGMENT-MISSING OR                                 
031600                       INDX > INDX-MAX                                    
031700                                                                          
031800           MOVE XART-IDARTNR TO RESP-IDARTNR (INDX)                       
031900           MOVE 311-TEXT-BEART TO RESP-BEART (INDX)                       
032000           MOVE XLEV-IDLEVNR TO RESP-IDLEVNR (INDX)                       
032100           MOVE XLEV-IDBENR TO RESP-IDBENR (INDX)                         
032200           MOVE XLEV-BELEVART TO RESP-BELEVART (INDX)                     
032300                                                                          
032400           ADD +1 TO WS-KVRADER                                           
032500           ADD +1 TO INDX                                                 
032600                                                                          
032700           PERFORM IMS-02-GNP-WDF502                                      
032800         END-PERFORM                                                      
033000       END-IF                                                             
033100     END-IF                                                               
033200     .                                                                    
033300                                                                          
033400 FB-SHOW-OTHER    SECTION.                                                
033500     MOVE 'FB-SHOW-OTHER    ' TO CURR-SECTION                             
033600                                                                          
033700     MOVE +1               TO INDX                                        
033800                                                                          
033900     MOVE REQU-BELEVART-KEY TO BELEVART-WS                                
034000     CALL W009REDU USING BELEVART-WS W-SEQB-IDLEVART                      
034100                                                                          
034200     PERFORM IMS-05-GU-WDF501-BSEQ                                        
034300                                                                          
034400     PERFORM UNTIL SEGMENT-MISSING OR                                     
034500                   END-OF-DB       OR                                     
034600                   INDX > INDX-MAX                                        
034700       MOVE XART-IDARTNR TO W-IDARTNR                                     
034800                                                                          
034900       PERFORM IMS-04-GU-WDK601                                           
035100       IF SEGMENT-FOUND                                                   
035500         MOVE XART-IDARTNR TO W-IDARTNR                                   
035600         PERFORM IMS-03-GU-WDD311                                         
035700                                                                          
035800         PERFORM IMS-07-GNP-WDF502-B                                      
035900         PERFORM UNTIL SEGMENT-MISSING OR                                 
036000                       INDX > INDX-MAX                                    
036100                                                                          
036200           MOVE XART-IDARTNR TO RESP-IDARTNR (INDX)                       
036300           MOVE 311-TEXT-BEART TO RESP-BEART (INDX)                       
036400           MOVE XLEV-IDLEVNR TO RESP-IDLEVNR (INDX)                       
036500           MOVE XLEV-IDBENR TO RESP-IDBENR (INDX)                         
036600           MOVE XLEV-BELEVART TO RESP-BELEVART (INDX)                     
036700                                                                          
036800           ADD +1 TO WS-KVRADER                                           
036900           ADD +1 TO INDX                                                 
037000                                                                          
037100           PERFORM IMS-07-GNP-WDF502-B                                    
037200         END-PERFORM                                                      
037400       END-IF                                                             
037500       PERFORM IMS-06-GN-WDF501-BSEQ                                      
037600     END-PERFORM                                                          
037700     .                                                                    
037800                                                                          
037900                                                                          
038000*    --- DISPATCHER SECTIONS                                              
038100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
038200     MOVE 'S01-FETCH-REQUEST' TO CURR-DISP-SECTION                        
038300                                                                          
038400     MOVE 'GETARG'                   TO SUB-KDFUNC                        
038500     MOVE 'CARPARTS.LDC.XREFRENAULT' TO SUB-ADDISPABS                     
038600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
038700                                                                          
038800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
038900                                                                          
039000     IF SUB-KDRC > 0                                                      
039100       MOVE SUB-KDRC       TO KDRC-DISPLAY                                
039200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
039300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039500     END-IF                                                               
039600     .                                                                    
039700                                                                          
039800 S02-RETURN-RESPONSE SECTION.                                             
039900     MOVE 'S02-RETURN-RESP  ' TO CURR-DISP-SECTION                        
040000                                                                          
040100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
040200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
040300                                                                          
040400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
040500                                                                          
040600     IF SUB-KDRC > 0                                                      
040700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
040900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041100     END-IF                                                               
041200     .                                                                    
041300                                                                          
041400 IMS-01-GU-WDF501       SECTION.                                          
041500     MOVE 'IMS-01' TO CURR-IMS-SECTION                                    
041600                                                                          
041700     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
041800            DELIMITED BY SIZE INTO SSA1                                   
041900     MOVE '  GE'                TO GOOD-STATUSCODES                       
042000     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
042100     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
042200     PERFORM IMS-STATUS-CHECK                                             
042300     .                                                                    
042400                                                                          
042500 IMS-02-GNP-WDF502     SECTION.                                           
042600     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
042700                                                                          
042800     MOVE 'WDF502   '  TO SSA1                                            
042900     MOVE '  GE'              TO GOOD-STATUSCODES                         
043000     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
043100     MOVE WDF5-STATUS-CODE   TO STATUS-WS                                 
043200     PERFORM IMS-STATUS-CHECK                                             
043300     .                                                                    
043400                                                                          
043500 IMS-03-GU-WDD311       SECTION.                                          
043600     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
043700                                                                          
043800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
043900            DELIMITED BY SIZE INTO SSA1                                   
044000     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT ')'                           
044100            DELIMITED BY SIZE INTO SSA2                                   
044200     MOVE '  GE'                TO GOOD-STATUSCODES                       
044300     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311  SSA1 SSA2              
044400     MOVE WDD3-STATUS-CODE      TO STATUS-WS                              
044500     PERFORM IMS-STATUS-CHECK                                             
044600     IF SEGMENT-MISSING                                                   
044700        MOVE SPACE TO 311-TEXT-BEART                                      
044800     END-IF                                                               
044900     .                                                                    
045000                                                                          
045100 IMS-04-GU-WDK601 SECTION.                                                
045200     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
045300                                                                          
045400     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
045500          DELIMITED BY SIZE INTO SSA1                                     
045600     MOVE '  GE'   TO GOOD-STATUSCODES                                    
045700     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK601   SSA1                 
045800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
045900     PERFORM IMS-STATUS-CHECK                                             
046000     .                                                                    
046100                                                                          
046200 IMS-05-GU-WDF501-BSEQ  SECTION.                                          
046300     MOVE 'IMS-08' TO CURR-IMS-SECTION                                    
046400                                                                          
046500     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
046600          DELIMITED BY SIZE INTO SSA1                                     
046700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
046800     CALL CBLTDLI USING GU WDF5B-PCB DLI-IO-WDF501 SSA1                   
046900     MOVE WDF5B-STATUS-CODE TO STATUS-WS                                  
047000     PERFORM IMS-STATUS-CHECK                                             
047100     .                                                                    
047200                                                                          
047300 IMS-06-GN-WDF501-BSEQ  SECTION.                                          
047400     MOVE 'IMS-09' TO CURR-IMS-SECTION                                    
047500                                                                          
047600     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
047700          DELIMITED BY SIZE INTO SSA1                                     
047800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
047900     CALL CBLTDLI USING GN WDF5B-PCB DLI-IO-WDF501 SSA1                   
048000     MOVE WDF5B-STATUS-CODE TO STATUS-WS                                  
048100     PERFORM IMS-STATUS-CHECK                                             
048200     .                                                                    
048300                                                                          
048400 IMS-07-GNP-WDF502-B   SECTION.                                           
048500     MOVE 'IMS-10' TO CURR-IMS-SECTION                                    
048600                                                                          
048700     MOVE 'WDF502   '  TO SSA1                                            
048800     MOVE '  GE'              TO GOOD-STATUSCODES                         
048900     CALL CBLTDLI USING GNP WDF5B-PCB DLI-IO-WDF502 SSA1                  
049000     MOVE WDF5B-STATUS-CODE   TO STATUS-WS                                
049100     PERFORM IMS-STATUS-CHECK                                             
049200     .                                                                    
049300                                                                          
049400 IMS-STATUS-CHECK   SECTION.                                              
049500                                                                          
049600     SET STATUS-IX TO 1                                                   
049700     SEARCH GOOD-STATUS                                                   
049800       AT END                                                             
049900         CALL FELLOG                                                      
050000     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
050100       CONTINUE                                                           
050200     END-SEARCH                                                           
050300     .                                                                    
