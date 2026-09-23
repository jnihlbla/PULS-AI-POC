000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000301 PROGRAM-ID.     W4033B00.                                                
000400 AUTHOR.         UMESH JAIN.                                              
000500 DATE-WRITTEN.   09/02/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       PICK BY VOICE                                            
000903*                CARPARTS.3IV2.REQUPRINT                                  
001003*                CARPARTS.3IV2.RESPPRINT                                  
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        PROGRAM FOR PICK-BY-VOICE WHICH TRIGGERS PRINTING OF             
001400*        CASE LABELS AND DELIVERY NOTES AS A PART OF THE                  
001500*        CARRIER_FINALIZATION PROCESS BY SENDING A REQUEST_PRINT          
001600*        MESSAGE TO PULS.                                                 
001700*                                                                         
001800*        THE PROGRAM READS     WDE6                                       
001900*        THE PROGRAM READS     WDE4                                       
002000*                                                                         
002100*    INDATA.                                                              
002201*        TRANSACTION: W4033BU                                             
002300*        REQUEST:     W403REQU + W403PRIS                                 
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    W403RESP + W403PRES                                 
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
004001 77  IDPGM                       PIC X(08)   VALUE 'W4033B00'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  REQUEST-SW                  PIC X       VALUE 'J'.                   
004700     88  REQUEST-OK                          VALUE 'J'.                   
004800     88  REQUEST-WRONG                       VALUE 'N'.                   
004900                                                                          
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  VBAR                        PIC X       VALUE X'BB'.                 
005300                                                                          
005400 77  DATA-SW                     PIC X       VALUE 'J'.                   
005500     88  DATA-OK                             VALUE 'J'.                   
005600     88  DATA-WRONG                          VALUE 'N'.                   
005700     EJECT                                                                
005800 01  WS-IDPRTLST.                                                         
005900     03 WS-SYSTDEL               PIC X(1).                                
006000     03 WS-LISTTYP               PIC X(2).                                
006100     03 WS-DC                    PIC X(2).                                
006200     03 WS-KDPRT                 PIC 9(2).                                
006300                                                                          
006400 77  WS-IDDISTR-IN               PIC 9(4).                                
006500 77  WS-IDKUNDNR-IN              PIC 9(6).                                
006600                                                                          
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007600                                                                          
007700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007800*01 -COPY WMEDAREA                                                        
007900     SKIP3                                                                
008000*    -- WORK FIELD FOR EDITING WMEDKONV MESSAGE                           
008100 01  TEMP-MESSAGE                PIC X(50).                               
008200     EJECT                                                                
008300 01  MESSAGE-CODES.                                                       
008400     03  ERR-PRINT-NOT-FOUND     PIC X(3)    VALUE '772'.                 
008500     03  ERR-INVALID-DC          PIC X(3)    VALUE '440'.                 
008600     03  ERR-WRONG-CASE          PIC X(3)    VALUE '795'.                 
008700     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '701'.                 
008800     03  ERR-INVALID-VALUE       PIC X(3)    VALUE '492'.                 
008900     03  ERR-VALUE-TOO-LONG      PIC X(3)    VALUE '493'.                 
009000     03  ERR-WRONG-RCD-TYPE      PIC X(3)    VALUE '495'.                 
009100     03  ERR-WRONG-MSG-TYPE      PIC X(3)    VALUE '497'.                 
009200     03  ERR-INV-MSG-STRUCTURE   PIC X(3)    VALUE '499'.                 
009300     EJECT                                                                
009400*    --- PARAMETERS FOR PRINT SUBROUTINE W006PRT                          
009500*01  -COPY W006PRT                                                        
009600     EJECT                                                                
009700                                                                          
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010400     EJECT                                                                
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'RECV AREA'.           
010700     SKIP3                                                                
010800 01  RECV-AREA                   PIC X(1000).                             
010900     SKIP3                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'REQUEST HDR'.         
011100     SKIP3                                                                
011200*01  -COPY W403REQU                                                       
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'W403PRIS'.            
011500*01  -COPY W403PRIS                                                       
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
011800     SKIP3                                                                
011900 01  SEND-AREA                   PIC X(1000).                             
012000     SKIP3                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'RESPONSE HDR'.        
012200     SKIP3                                                                
012300*01  -COPY W403RESP                                                       
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'W403PRES'.            
012600*01  -COPY W403PRES                                                       
012700     EJECT                                                                
012800*    --- WORK-AREAS FOR MESSAGE PROCESSING                                
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'MSG-WS'.              
013100     SKIP3                                                                
013200 01  FIELD-LENGTHS.                                                       
013300*    --- W403REQU FIELDS                                                  
013400     03 LIDMSG3IV                PIC S9(4)   BINARY.                      
013500     03 LIDMVER3IV               PIC S9(4)   BINARY.                      
013600     03 LIDMTYP3IV               PIC S9(4)   BINARY.                      
013700     03 LIDDC                    PIC S9(4)   BINARY.                      
013800     03 LIDANSTNR                PIC S9(4)   BINARY.                      
013900     03 LIDSNO3IV                PIC S9(4)   BINARY.                      
014000                                                                          
014100*    --- W403RESP FIELDS                                                  
014200     03 LTISTAMP3IV              PIC S9(4)   BINARY.                      
014300     03 LKDRESP3IV               PIC S9(4)   BINARY.                      
014400     03 LBERESP3IV               PIC S9(4)   BINARY.                      
014500     03 LKDPCD3IV                PIC S9(4)   BINARY.                      
014600                                                                          
014700*    --- W403PRIS FIELDS                                                  
014800     03 LIDRTYP3IV               PIC S9(4)   BINARY.                      
014900     03 LIDPRODNR                PIC S9(4)   BINARY.                      
015000     03 LIDPLKLST                PIC S9(4)   BINARY.                      
015100     03 LIDLOPNR-ORD             PIC S9(4)   BINARY.                      
015200     03 LIDKOLLI                 PIC S9(4)   BINARY.                      
015300     03 LKDPRTVAL                PIC S9(4)   BINARY.                      
015400                                                                          
015500 01  FILLER                  PIC X(16) VALUE 'MID W4I33301AREA'.          
015600 01  4333-MSG-IO-AREA.                                                    
015700     03  4333-KVLL             PIC S9(4)   COMP SYNC.                     
015800     03  4333-Z1               PIC X.                                     
015900     03  4333-Z2               PIC X.                                     
016000     03  4333-TRANSKOD         PIC X(8)    VALUE 'W4T333  '.              
016100     03  4333-IDTRANS          PIC X(4)    VALUE '433Z'.                  
016200     03  4333-KDMFSFOR         PIC X       VALUE '1'.                     
016300*    03  MID -COPY W4I33301  -PRE 4333-.                                  
016400     SKIP2                                                                
016500                                                                          
016600 01  FILLER                  PIC X(16) VALUE 'MID W4I34101AREA'.          
016700 01  4341-MSG-IO-AREA.                                                    
016800     03  4341-KVLL             PIC S9(4)   COMP SYNC.                     
016900     03  4341-Z1               PIC X.                                     
017000     03  4341-Z2               PIC X.                                     
017100     03  4341-TRANSKOD         PIC X(8)    VALUE 'W4T341  '.              
017200     03  4341-IDTRANS          PIC X(4)    VALUE '433Z'.                  
017300     03  4341-KDMFSFOR         PIC X       VALUE '1'.                     
017400*    03  MID -COPY W4I34101  -PRE 4341-.                                  
017500     SKIP2                                                                
017600*                                                                         
017700     EJECT                                                                
017800*    --- PARAMETERS TO WZ01RECV                                           
017900 01  FILLER                      PIC X(16)   VALUE 'WZ01RECV'.            
018000     SKIP3                                                                
018100*01  -COPY WZ01RECV                                                       
018200     EJECT                                                                
018300*    --- PARAMETERS TO WZ01SEND                                           
018400 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
018500     SKIP3                                                                
018600*01  -COPY WZ01SEND                                                       
018700     EJECT                                                                
018800*                                                                         
018900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200     SKIP3                                                                
019300 01  KEYS-FOR-DLI.                                                        
019400     03  W-IDPRODNR-X.                                                    
019500         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
019600                                                                          
019700     03  W-IDKOLLI-X.                                                     
019800         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
019900                                                                          
020000 01  W-CHECK-DIGTS-SEED.                                                  
020100     03  W-IDPRODNR-SEED         PIC 9(7)   VALUE ZERO.                   
020200     03  W-IDKOLLI-SEED          PIC 9(2)   VALUE ZERO.                   
020300                                                                          
020400 77  WS-SEED                     PIC 9(9)   VALUE ZERO.                   
020500                                                                          
020600 01  WS-VARIABLES.                                                        
020700     03 W-RANDOM-NUM                             PIC V999.                
020800     03 W-RANDOM-NUM-RED REDEFINES W-RANDOM-NUM  PIC 9(3).                
020900                                                                          
021000     SKIP2                                                                
021100*    --- STATUS-KOD FRÅN IMS                                              
021200 01  STATUS-WS                   PIC XX.                                  
021300     88  SEGMENT-FOUND                       VALUE '  '.                  
021400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021500     SKIP2                                                                
021600 01  GOOD-STATUSCODES.                                                    
021700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021800     SKIP3                                                                
021900 01  SSA1                        PIC X(64).                               
022000 01  SSA2                        PIC X(64).                               
022100     EJECT                                                                
022200*    --- IMS FUNCTION CODES                                               
022300*01  -COPY W0003                                                          
022400     EJECT                                                                
022500                                                                          
022600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
022700 01  DLI-IO-WDE611.                                                       
022800*    03  -COPY WDE611                                                     
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
023000 01  DLI-IO-WDE401.                                                       
023100*    03  -COPY WDE401                                                     
023200     EJECT                                                                
023300 LINKAGE SECTION.                                                         
023400*01  -COPY W0009     -PRE MSG-                                            
023500     SKIP2                                                                
023600*01  -COPY W0009     -PRE 4333-                                           
023700     SKIP2                                                                
023800*01  -COPY W0009     -PRE 4341-                                           
023900     SKIP2                                                                
024000*01  -COPY W0008     -PRE WDE6-                                           
024100     05  FILLER                  PIC X.                                   
024200*01  -COPY W0008     -PRE WDE4-                                           
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500 PROCEDURE DIVISION  USING MSG-PCB  4333-PCB 4341-PCB                     
024600                           WDE6-PCB WDE4-PCB.                             
024700 MAIN SECTION.                                                            
024800     ENTRY 'DLITCBL' USING MSG-PCB  4333-PCB 4341-PCB                     
024900                           WDE6-PCB WDE4-PCB.                             
025000                                                                          
025100     PERFORM S03-RECEIVE-OPEN                                             
025200     IF RECV-KDRC = 0                                                     
025300       PERFORM A-INIT                                                     
025400**** PROCESS REQUEST HEADER AND SUMMARY                                   
025500       PERFORM B-PROCESS-REQUEST                                          
025501                                                                          
025510       PERFORM S03-RECEIVE-CLOSE                                          
025700       PERFORM S04-SEND-OPEN                                              
025800       IF SEND-KDRC = 0                                                   
025900*        --PROCESS RESPONSE HEADER AND SUMMARY RECORDS                    
026000         PERFORM C-PROCESS-RESPONSE                                       
026100         PERFORM S04-SEND-CLOSE                                           
026200       END-IF                                                             
026300                                                                          
026500     END-IF                                                               
026600                                                                          
026700                                                                          
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027500     MOVE 'S'   TO MED-IDSKYLT                                            
027600     MOVE ZERO  TO RESP-KDRESP3IV                                         
027700     MOVE SPACE TO RESP-BERESP3IV                                         
027800     .                                                                    
027900     EJECT                                                                
028000     EJECT                                                                
028100 B-PROCESS-REQUEST SECTION.                                               
028200                                                                          
028300*    -- SAVE VCOM SENDER TAG INFO FOR MQ/WMDB/VCOM GATEWAY                
028400     MOVE RECV-ADDISPXTRA TO SEND-ADDISPXTRA                              
028500                                                                          
028600     MOVE YES TO REQUEST-SW                                               
028700                                                                          
028800     PERFORM S03-RECEIVE-MESSAGE                                          
028900     IF RECV-KDRC > 0                                                     
029000*      -- HEADER MISSING                                                  
029100       MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                         
029200       CALL WMEDKONV USING MED-WMEDAREA                                   
029300       SET REQUEST-WRONG TO TRUE                                          
029400     ELSE                                                                 
029500       PERFORM BA-PROCESS-REQUEST-HEADER                                  
029600     END-IF                                                               
029700                                                                          
029800     IF REQUEST-OK                                                        
029900       PERFORM S03-RECEIVE-MESSAGE                                        
030000       IF RECV-KDRC > 0                                                   
030100*      -- PRINT REQUEST MISSING                                           
030200         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
030300         CALL WMEDKONV USING MED-WMEDAREA                                 
030400         SET REQUEST-WRONG TO TRUE                                        
030500       ELSE                                                               
030600         PERFORM BB-PROCESS-PRINT-REQUEST-DATA                            
030700         IF REQUEST-OK                                                    
030800*          -- VALIDATE INPUT DATA AND INITIATE 4333 AND 4341              
030900           PERFORM BC-VALIDATE-INPUT-DATA                                 
031000           IF DATA-OK                                                     
031100             PERFORM BD-MAKE-CALLS-TO-4333-4341                           
031200           END-IF                                                         
031300         END-IF                                                           
031400       END-IF                                                             
031500     END-IF                                                               
031600                                                                          
031700     IF REQUEST-WRONG OR DATA-WRONG                                       
031800       MOVE MED-IDMFSFEL TO RESP-KDRESP3IV                                
031900       MOVE MED-TEMFSFEL TO RESP-BERESP3IV                                
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 BA-PROCESS-REQUEST-HEADER SECTION.                                       
032400                                                                          
032500     UNSTRING RECV-AREA (1:RECV-KVDLEN)                                   
032600       DELIMITED BY VBAR INTO                                             
032700         REQU-IDMSG3IV      COUNT IN LIDMSG3IV                            
032800         REQU-IDMVER3IV     COUNT IN LIDMVER3IV                           
032900         REQU-IDMTYP3IV     COUNT IN LIDMTYP3IV                           
033000         REQU-TISTAMP3IV    COUNT IN LTISTAMP3IV                          
033100         REQU-IDDC          COUNT IN LIDDC                                
033200         REQU-IDANSTNR      COUNT IN LIDANSTNR                            
033300         REQU-IDSNO3IV      COUNT IN LIDSNO3IV                            
033400       OVERFLOW                                                           
033500*      -- TOO MANY FIELDS                                                 
033600         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
033700         CALL WMEDKONV USING MED-WMEDAREA                                 
033800         SET REQUEST-WRONG TO TRUE                                        
033900     END-UNSTRING                                                         
034000                                                                          
034100     IF LIDMSG3IV > LENGTH OF REQU-IDMSG3IV                               
034200*      -- MSG ID TOO LONG                                                 
034300       MOVE ERR-VALUE-TOO-LONG TO MED-IDMFSFEL                            
034400       CALL WMEDKONV USING MED-WMEDAREA                                   
034500       STRING 'MEDDELANDE-ID ' MED-TEMFSFEL                               
034600         DELIMITED BY SIZE INTO TEMP-MESSAGE                              
034700       MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                  
034800       SET REQUEST-WRONG TO TRUE                                          
034900     END-IF                                                               
035000*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
035100     MOVE REQU-IDMSG3IV TO RESP-IDMSG3IV                                  
035200                                                                          
035300*--  REQU-IDMVER3IV IGNORED                                               
035400                                                                          
035500     IF REQU-IDMTYP3IV NOT = 'RequestPrint'                               
035600       MOVE ERR-WRONG-MSG-TYPE TO MED-IDMFSFEL                            
035700       CALL WMEDKONV USING MED-WMEDAREA                                   
035800       SET REQUEST-WRONG TO TRUE                                          
035900     END-IF                                                               
036000                                                                          
036100*--  REQU-TISTAMP3IV IGNORED                                              
036200                                                                          
036300     IF REQUEST-OK                                                        
036400       IF LIDDC > LENGTH OF REQU-IDDC                                     
036500       OR REQU-IDDC NOT = '11'                                            
036600*        --- INVALID DC                                                   
036700         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
036800         CALL WMEDKONV USING MED-WMEDAREA                                 
036900         STRING 'LAGERIDENTITET ' MED-TEMFSFEL                            
037000           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
037100         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
037200         SET REQUEST-WRONG TO TRUE                                        
037300       END-IF                                                             
037400     END-IF                                                               
037500*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
037600     MOVE REQU-IDDC     TO RESP-IDDC                                      
037700                                                                          
037800     IF REQUEST-OK                                                        
037900       IF LIDANSTNR > LENGTH OF REQU-IDANSTNR                             
038000       OR REQU-IDANSTNR NOT NUMERIC                                       
038100*        --- INVALID OPERATOR ID                                          
038200         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
038300         CALL WMEDKONV USING MED-WMEDAREA                                 
038400         STRING 'PACKAREIDENTITET ' MED-TEMFSFEL                          
038500           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
038600         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
038700         SET REQUEST-WRONG TO TRUE                                        
038800       END-IF                                                             
038900     END-IF                                                               
039000*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
039100     MOVE REQU-IDANSTNR TO RESP-IDANSTNR                                  
039200                                                                          
039300     IF REQUEST-OK                                                        
039400       IF LIDSNO3IV > LENGTH OF REQU-IDSNO3IV                             
039500*        --- SERIAL NBR TOO LONG                                          
039600         MOVE ERR-VALUE-TOO-LONG TO MED-IDMFSFEL                          
039700         CALL WMEDKONV USING MED-WMEDAREA                                 
039800         STRING 'SERIENUMMER ' MED-TEMFSFEL                               
039900           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
040000         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
040100         SET REQUEST-WRONG TO TRUE                                        
040200       END-IF                                                             
040300     END-IF                                                               
040400*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE HEADER                 
040500     MOVE REQU-IDSNO3IV TO RESP-IDSNO3IV                                  
040600                                                                          
040700     .                                                                    
040800 BB-PROCESS-PRINT-REQUEST-DATA SECTION.                                   
040900                                                                          
041000     UNSTRING RECV-AREA (1:RECV-KVDLEN)                                   
041100       DELIMITED BY VBAR INTO                                             
041200         PRIS-IDRTYP3IV     COUNT IN LIDRTYP3IV                           
041300         PRIS-IDPRODNR      COUNT IN LIDPRODNR                            
041400         PRIS-IDPLKLST      COUNT IN LIDPLKLST                            
041500         PRIS-IDLOPNR-ORD   COUNT IN LIDLOPNR-ORD                         
041600         PRIS-IDKOLLI       COUNT IN LIDKOLLI                             
041700         PRIS-KDPRTVAL      COUNT IN LKDPRTVAL                            
041800       OVERFLOW                                                           
041900*      -- TOO MANY FIELDS                                                 
042000         MOVE ERR-INV-MSG-STRUCTURE TO MED-IDMFSFEL                       
042100         CALL WMEDKONV USING MED-WMEDAREA                                 
042200         SET REQUEST-WRONG TO TRUE                                        
042300     END-UNSTRING                                                         
042400                                                                          
042500     IF PRIS-IDRTYP3IV NOT = 'PrintSummary'                               
042600*      -- INCORRECT RECORD TYPE                                           
042700       MOVE ERR-WRONG-RCD-TYPE TO MED-IDMFSFEL                            
042800       CALL WMEDKONV USING MED-WMEDAREA                                   
042900       SET REQUEST-WRONG TO TRUE                                          
043000     END-IF                                                               
043100                                                                          
043200     IF REQUEST-OK                                                        
043300       IF LIDPRODNR > LENGTH OF PRIS-IDPRODNR                             
043400       OR PRIS-IDPRODNR NOT NUMERIC                                       
043500*        --- INVALID PRODUCTION NUMBER                                    
043600         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
043700         CALL WMEDKONV USING MED-WMEDAREA                                 
043800         STRING 'PRODUKTIONSNUMMER ' MED-TEMFSFEL                         
043900           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
044000         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
044100         SET REQUEST-WRONG TO TRUE                                        
044200       END-IF                                                             
044300     END-IF                                                               
044400*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE SUMMARY                
044500     MOVE PRIS-IDPRODNR TO PRES-IDPRODNR                                  
044600                                                                          
044700     IF REQUEST-OK                                                        
044800       IF LIDPLKLST > LENGTH OF PRIS-IDPLKLST                             
044900       OR PRIS-IDPLKLST NOT NUMERIC                                       
045000*        --- INVALID PICKING LIST NUMBER                                  
045100         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
045200         CALL WMEDKONV USING MED-WMEDAREA                                 
045300         STRING 'PLOCKLISTENUMMER ' MED-TEMFSFEL                          
045400           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
045500         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
045600         SET REQUEST-WRONG TO TRUE                                        
045700       END-IF                                                             
045802     END-IF                                                               
045902*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE SUMMARY                
046002     MOVE PRIS-IDPLKLST TO PRES-IDPLKLST                                  
046102                                                                          
046202     IF REQUEST-OK                                                        
046302       IF LIDLOPNR-ORD  > LENGTH OF PRIS-IDLOPNR-ORD                      
046402       OR PRIS-IDLOPNR-ORD NOT NUMERIC                                    
046502*        --- INVALID CARRIER INDEX                                        
046602         MOVE ERR-INVALID-VALUE TO MED-IDMFSFEL                           
046702         CALL WMEDKONV USING MED-WMEDAREA                                 
046802         STRING 'LÖPNUMMER FÖR ORDERDEL '  MED-TEMFSFEL                   
046902           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
047002         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
047102         SET REQUEST-WRONG TO TRUE                                        
047202       END-IF                                                             
047302     END-IF                                                               
047402*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE SUMMARY                
047502     MOVE PRIS-IDLOPNR-ORD TO PRES-IDLOPNR-ORD                            
047602                                                                          
047702     IF REQUEST-OK                                                        
047802       IF LIDKOLLI  > LENGTH OF PRIS-IDKOLLI                              
047902       OR PRIS-IDKOLLI NOT NUMERIC                                        
048002*        --- INVALID CASE NUMBER                                          
048102         MOVE ERR-INVALID-VALUE  TO MED-IDMFSFEL                          
048202         CALL WMEDKONV USING MED-WMEDAREA                                 
048302         STRING 'KOLLINUMMER ' MED-TEMFSFEL                               
048402           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
048502         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
048602         SET REQUEST-WRONG TO TRUE                                        
048702       END-IF                                                             
048802     END-IF                                                               
048902*    -- THE VALUE MUST BE RETURNED IN THE RESPONSE SUMMARY                
049002     MOVE PRIS-IDKOLLI  TO PRES-IDKOLLI                                   
049102                                                                          
049202     IF REQUEST-OK                                                        
049302       IF LKDPRTVAL > LENGTH OF PRIS-KDPRTVAL                             
049402       OR PRIS-KDPRTVAL NOT NUMERIC                                       
049502*        --- INVALID PRINTER ID                                           
049602         MOVE ERR-INVALID-VALUE  TO MED-IDMFSFEL                          
049702         CALL WMEDKONV USING MED-WMEDAREA                                 
049802         STRING 'PRINTERNUMMER ' MED-TEMFSFEL                             
049902           DELIMITED BY SIZE INTO TEMP-MESSAGE                            
050002         MOVE TEMP-MESSAGE TO MED-TEMFSFEL                                
050102         SET REQUEST-WRONG TO TRUE                                        
050202       END-IF                                                             
050302     END-IF                                                               
050402     .                                                                    
050502     EJECT                                                                
050602 BC-VALIDATE-INPUT-DATA SECTION.                                          
050702                                                                          
050802     MOVE YES                   TO DATA-SW                                
050902                                                                          
051002***CHECK IF WAREHOUSE ID IS '11' I.E. CDC                                 
051102     IF REQU-IDDC = '11'                                                  
051202       CONTINUE                                                           
051302     ELSE                                                                 
051402       MOVE NOO                 TO DATA-SW                                
051502       MOVE ERR-INVALID-DC      TO MED-IDMFSINF                           
051602       CALL WMEDKONV USING MED-WMEDAREA                                   
051702     END-IF                                                               
051802                                                                          
051902***CHECK IF THE PRINTER CODE ENTERED IS VALID                             
052002     IF DATA-OK                                                           
052102       PERFORM BCA-CHECK-PRINTER-CODE                                     
052202     END-IF                                                               
052302     .                                                                    
052402     EJECT                                                                
052502 BCA-CHECK-PRINTER-CODE SECTION.                                          
052602                                                                          
052702**** CHECK PRINTER FOR CASE LABEL PRINTING - 4KF+IDDC+KDPRTVAL            
052802     MOVE '4'                TO WS-SYSTDEL                                
052902     MOVE 'KF'               TO WS-LISTTYP                                
053002     MOVE REQU-IDDC          TO WS-DC                                     
053102     MOVE PRIS-KDPRTVAL      TO WS-KDPRT                                  
053202                                                                          
053302     MOVE 001                TO PRT-KDCALL                                
053402     MOVE WS-IDPRTLST        TO PRT-IDPRTLST                              
053502                                                                          
053602     CALL W006PRT USING PRT-W006PRT                                       
053702                                                                          
053802     IF PRT-KDSVAR = 'R'                                                  
053902       CONTINUE                                                           
054002     ELSE                                                                 
054102       MOVE NOO                 TO DATA-SW                                
054202       MOVE ERR-PRINT-NOT-FOUND TO MED-IDMFSFEL                           
054302       CALL WMEDKONV USING MED-WMEDAREA                                   
054402     END-IF                                                               
054502                                                                          
054602**** CHECK PRINTER FOR DELIVERY NOTE PRINTING - 4FS+IDDC+KDPRTVAL         
054702     MOVE '4'                TO WS-SYSTDEL                                
054802     MOVE 'FS'               TO WS-LISTTYP                                
054902     MOVE REQU-IDDC          TO WS-DC                                     
055002     MOVE PRIS-KDPRTVAL      TO WS-KDPRT                                  
055102                                                                          
055202     MOVE 001                TO PRT-KDCALL                                
055302     MOVE WS-IDPRTLST        TO PRT-IDPRTLST                              
055402                                                                          
055502     CALL W006PRT USING PRT-W006PRT                                       
055602                                                                          
055702     IF PRT-KDSVAR = 'R'                                                  
055802       CONTINUE                                                           
055902     ELSE                                                                 
056002       MOVE NOO                 TO DATA-SW                                
056102       MOVE ERR-PRINT-NOT-FOUND TO MED-IDMFSFEL                           
056202       CALL WMEDKONV USING MED-WMEDAREA                                   
056302     END-IF                                                               
056402     .                                                                    
056502     EJECT                                                                
056602 BD-MAKE-CALLS-TO-4333-4341  SECTION.                                     
056702                                                                          
056802     PERFORM BDA-READ-BASICDATA                                           
056902                                                                          
057002     PERFORM BDC-ISRT-MID-DATA-4333                                       
057102                                                                          
057202     PERFORM BDD-ISRT-MID-DATA-4341                                       
057302     .                                                                    
057402 BDA-READ-BASICDATA SECTION.                                              
057502                                                                          
057602     MOVE PRIS-IDPRODNR TO W-IDPRODNR                                     
057702     MOVE PRIS-IDKOLLI  TO W-IDKOLLI                                      
057802                                                                          
057902*** IMS CALL TO WDE611 VIA WDE601                                         
058002     PERFORM IMS-GU-WDE611                                                
058102     IF SEGMENT-FOUND                                                     
058202       IF KOLLI-KDKOLSTA > 0                                              
058302         CONTINUE                                                         
058402       ELSE                                                               
058502         MOVE NOO                 TO DATA-SW                              
058602         MOVE ERR-WRONG-CASE      TO MED-IDMFSFEL                         
058702         CALL WMEDKONV USING MED-WMEDAREA                                 
058802       END-IF                                                             
058902     ELSE                                                                 
059002       MOVE NOO                   TO DATA-SW                              
059102       MOVE ERR-WRONG-CASE        TO MED-IDMFSFEL                         
059202       CALL WMEDKONV USING MED-WMEDAREA                                   
059302     END-IF                                                               
059402                                                                          
059502*** IMS CALL TO WDE401 VIA SECONDARY INDEX WDE4ESEQ                       
059602     PERFORM IMS-GU-WDE401                                                
059702     IF SEGMENT-FOUND                                                     
059802       MOVE KORD-IDDISTR          TO WS-IDDISTR-IN                        
059902       MOVE KORD-IDKUNDNR         TO WS-IDKUNDNR-IN                       
060002       MOVE KORD-IDORDNR5         TO 4341-MID-IDORDNR-IN                  
060102     ELSE                                                                 
060202       MOVE NOO                   TO DATA-SW                              
060302       MOVE ERR-ORDER-MISSING     TO MED-IDMFSFEL                         
060402       CALL WMEDKONV USING MED-WMEDAREA                                   
060502     END-IF                                                               
060602     .                                                                    
060702     EJECT                                                                
060802                                                                          
060902                                                                          
061002 BDC-ISRT-MID-DATA-4333 SECTION.                                          
061102                                                                          
061202     COMPUTE 4333-KVLL = LENGTH OF 4333-MID-W4I33301 + 17                 
061302                                                                          
061402     MOVE ZEROS                   TO 4333-MID-IDDISTR-UT                  
061502                                     4333-MID-IDKUNDNR-UT                 
061602                                     4333-MID-IDORDNR-UT                  
061702                                     4333-MID-IDKOLLI-UT                  
061802                                     4333-MID-IDPRODNR-UT                 
061902     MOVE ALL '+'                 TO 4333-MID-IDDISTR-IN                  
062002                                     4333-MID-IDKUNDNR-IN                 
062102                                     4333-MID-IDORDNR-IN                  
062202     MOVE ZERO                    TO 4333-MID-IDKOLLI-TOM                 
062302     MOVE SPACE                   TO 4333-MID-KDPRTVAL-UT                 
062402                                     4333-MID-IDDC-UT                     
062502                                                                          
062602     MOVE PRIS-IDKOLLI            TO 4333-MID-IDKOLLI-IN                  
062702     MOVE REQU-IDDC               TO 4333-MID-IDDC-IN                     
062802     MOVE PRIS-IDPRODNR           TO 4333-MID-IDPRODNR-IN                 
062902     MOVE PRIS-KDPRTVAL           TO 4333-MID-KDPRTVAL-IN                 
063002                                                                          
063102     PERFORM IMS-ISRT-4333-MSG                                            
063202     .                                                                    
063302     EJECT                                                                
063402 BDD-ISRT-MID-DATA-4341 SECTION.                                          
063502                                                                          
063602     COMPUTE 4341-KVLL = LENGTH OF 4341-MID-W4I34101 + 17                 
063702                                                                          
063802     MOVE ZEROS                   TO 4341-MID-IDDISTR-UT                  
063902                                     4341-MID-IDKUNDNR-UT                 
064002                                     4341-MID-IDORDNR-UT                  
064102                                     4341-MID-IDPLKLST-UT                 
064202                                     4341-MID-IDKOLLI-UT                  
064302                                     4341-MID-IDKOLLI-TOM-UT              
064402     MOVE ALL '+'                 TO 4341-MID-IDDC-UT                     
064502                                     4341-MID-IDKOLLI-TOM-IN              
064602                                                                          
064702     MOVE SPACE                   TO 4341-MID-IDPLKLST-IN                 
064802                                     4341-MID-KDPRTVAL-UT                 
064902     MOVE PRIS-IDKOLLI            TO 4341-MID-IDKOLLI-IN                  
065002     MOVE PRIS-KDPRTVAL           TO 4341-MID-KDPRTVAL-IN                 
065102     MOVE REQU-IDDC               TO 4341-MID-IDDC-IN                     
065202     MOVE WS-IDDISTR-IN           TO 4341-MID-IDDISTR-IN                  
065302     MOVE WS-IDKUNDNR-IN          TO 4341-MID-IDKUNDNR-IN                 
065402     MOVE 'N'                     TO 4341-MID-FL-SVENSK-FSEDEL            
065502                                                                          
065602     PERFORM IMS-ISRT-4341-MSG                                            
065702     .                                                                    
065802     EJECT                                                                
065902                                                                          
066002 C-PROCESS-RESPONSE            SECTION.                                   
066102                                                                          
066202     MOVE FUNCTION CURRENT-DATE(1:16) TO RESP-TISTAMP3IV                  
066302     MOVE '0'                         TO RESP-TISTAMP3IV(17:1)            
066402                                                                          
066502     PERFORM CA-PROCESS-RESPONSE-HEADER                                   
066602     PERFORM S04-SEND-MESSAGE                                             
066702*    -- ONLY THE HEADER SHOULD BE SENT IF THERE WAS AN ERROR              
066802     IF REQUEST-OK AND DATA-OK                                            
066902       PERFORM CB-PROCESS-PRINT-RESULT-DATA                               
067002       PERFORM S04-SEND-MESSAGE                                           
067102     END-IF                                                               
067202     .                                                                    
067302     EJECT                                                                
067402 CA-PROCESS-RESPONSE-HEADER SECTION.                                      
067502                                                                          
067602     MOVE SPACE TO SEND-AREA                                              
067702     MOVE 1     TO SEND-KVDLEN                                            
067802                                                                          
067902*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
068002     MOVE ZERO TO LIDMSG3IV                                               
068102     INSPECT FUNCTION REVERSE(RESP-IDMSG3IV)                              
068202       TALLYING LIDMSG3IV FOR LEADING SPACE                               
068302     COMPUTE LIDMSG3IV = LENGTH OF RESP-IDMSG3IV - LIDMSG3IV              
068402     STRING RESP-IDMSG3IV (1:LIDMSG3IV)  VBAR                             
068502       DELIMITED BY SIZE                                                  
068602       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
068702                                                                          
068802     STRING 'ResponsePrint' VBAR                                          
068902       DELIMITED BY SIZE                                                  
069002       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
069102                                                                          
069202     STRING RESP-TISTAMP3IV VBAR                                          
069302       DELIMITED BY SIZE                                                  
069402       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
069502                                                                          
069602     STRING RESP-IDDC VBAR                                                
069702       DELIMITED BY SIZE                                                  
069802       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
069902                                                                          
070002*    -- NUMERIC - REMOVE LEADING SPACE                                    
070102     MOVE 1 TO LIDANSTNR                                                  
070202     INSPECT RESP-IDANSTNR                                                
070302       TALLYING LIDANSTNR FOR LEADING SPACE                               
070402     STRING RESP-IDANSTNR (LIDANSTNR:) VBAR                               
070502       DELIMITED BY SIZE                                                  
070602       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
070702                                                                          
070802*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
070902     MOVE ZERO TO LIDSNO3IV                                               
071002     INSPECT FUNCTION REVERSE(RESP-IDSNO3IV)                              
071102       TALLYING LIDSNO3IV FOR LEADING SPACE                               
071202     COMPUTE LIDSNO3IV = LENGTH OF RESP-IDSNO3IV - LIDSNO3IV              
071302     STRING RESP-IDSNO3IV (1:LIDSNO3IV)  VBAR                             
071402       DELIMITED BY SIZE                                                  
071502       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
071602                                                                          
071702*    -- NUMERIC - REMOVE LEADING SPACE                                    
071802     MOVE 1 TO LKDRESP3IV                                                 
071902     INSPECT RESP-KDRESP3IV                                               
072002       TALLYING LKDRESP3IV FOR LEADING SPACE                              
072102     STRING RESP-KDRESP3IV (LKDRESP3IV:) VBAR                             
072202       DELIMITED BY SIZE                                                  
072302       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
072402                                                                          
072502*    -- ALPHANUMERIC - REMOVE TRAILING SPACE                              
072602*    -- LAST FIELD! - NO TRAILING VERTICAL BAR                            
072702     MOVE ZERO TO LBERESP3IV                                              
072802     INSPECT FUNCTION REVERSE(RESP-BERESP3IV)                             
072902       TALLYING LBERESP3IV FOR LEADING SPACE                              
073002     COMPUTE LBERESP3IV = LENGTH OF RESP-BERESP3IV - LBERESP3IV           
073003     IF LBERESP3IV > 0                                                    
073102       STRING RESP-BERESP3IV (1:LBERESP3IV)                               
073202         DELIMITED BY SIZE                                                
073302         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
073303     END-IF                                                               
073304                                                                          
073305*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
073306*    -- ADJUST TO CORRECT LENGTH                                          
073307     SUBTRACT 1 FROM SEND-KVDLEN                                          
073402     .                                                                    
073502     EJECT                                                                
073602 CB-PROCESS-PRINT-RESULT-DATA SECTION.                                    
073702                                                                          
073802     MOVE SPACE TO SEND-AREA                                              
073902     MOVE 1     TO SEND-KVDLEN                                            
074002                                                                          
074102     STRING 'PrintResult' VBAR                                            
074202       DELIMITED BY SIZE                                                  
074302       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
074402                                                                          
074502*    -- NUMERIC - REMOVE LEADING SPACE                                    
074602     MOVE 1 TO LIDPRODNR                                                  
074702     INSPECT PRES-IDPRODNR                                                
074802       TALLYING LIDPRODNR FOR LEADING SPACE                               
074902     STRING PRES-IDPRODNR (LIDPRODNR:) VBAR                               
075002       DELIMITED BY SIZE                                                  
075102       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
075202                                                                          
075302     STRING PRES-IDPLKLST VBAR                                            
075402       DELIMITED BY SIZE                                                  
075502       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
075602                                                                          
075702*    -- NUMERIC - REMOVE LEADING SPACE                                    
075802     MOVE 1 TO LIDLOPNR-ORD                                               
075902     INSPECT PRES-IDLOPNR-ORD                                             
076002       TALLYING LIDLOPNR-ORD FOR LEADING SPACE                            
076102     STRING PRES-IDLOPNR-ORD (LIDLOPNR-ORD:) VBAR                         
076202       DELIMITED BY SIZE                                                  
076302       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
076402                                                                          
076502*    -- NUMERIC - REMOVE LEADING SPACE                                    
076602     MOVE 1 TO LIDKOLLI                                                   
076702     INSPECT PRES-IDKOLLI                                                 
076802       TALLYING LIDKOLLI  FOR LEADING SPACE                               
076902     STRING PRES-IDKOLLI (LIDKOLLI:) VBAR                                 
077002       DELIMITED BY SIZE                                                  
077102       INTO SEND-AREA WITH POINTER SEND-KVDLEN                            
077202                                                                          
077302*    --COMPUTE CHECK DIGITS BY GENERATING RANDOM NUMBER                   
077402     IF REQUEST-OK AND DATA-OK                                            
077502       MOVE PRIS-IDPRODNR           TO W-IDPRODNR-SEED                    
077602       MOVE PRIS-IDKOLLI            TO W-IDKOLLI-SEED                     
077702       MOVE W-CHECK-DIGTS-SEED      TO WS-SEED                            
077802       COMPUTE W-RANDOM-NUM = FUNCTION RANDOM(WS-SEED)                    
077902       MOVE W-RANDOM-NUM-RED        TO PRES-KDPCD3IV                      
078002                                                                          
078102       STRING PRES-KDPCD3IV                                               
078202         DELIMITED BY SIZE                                                
078302         INTO SEND-AREA WITH POINTER SEND-KVDLEN                          
078402     END-IF                                                               
078403                                                                          
078404*    -- SEND-KVDLEN "POINTS" TO AFTER LAST CHARACTER IN MSG               
078405*    -- ADJUST TO CORRECT LENGTH                                          
078406     SUBTRACT 1 FROM SEND-KVDLEN                                          
078902     .                                                                    
079002     EJECT                                                                
079102*    --- DISPATCHER SECTIONS                                              
079202 S03-RECEIVE-OPEN SECTION.                                                
079302                                                                          
079402     MOVE 'OPEN'                   TO RECV-KDFUNC                         
079503     MOVE 'CARPARTS.3IV2.REQUPRINT' TO RECV-ADDISPABS                     
079602     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
079702                                                                          
079802     IF RECV-KDRC > 0 AND NOT = 20                                        
079902       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
080002       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
080102       DELIMITED BY SIZE INTO ERROR-TEXT                                  
080202       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
080302     END-IF                                                               
080402     .                                                                    
080502     SKIP3                                                                
080602 S03-RECEIVE-MESSAGE SECTION.                                             
080702                                                                          
080802     MOVE 'GET'                      TO RECV-KDFUNC                       
080902     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
081002     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
081003                                                                          
081005     IF RECV-KVDLEN = 0                                                   
081006       MOVE 1 TO RECV-KVDLEN                                              
081007     END-IF                                                               
081102                                                                          
081202     IF RECV-KDRC > 1                                                     
081302       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
081402       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
081502       DELIMITED BY SIZE INTO ERROR-TEXT                                  
081602       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
081702     END-IF                                                               
081802     .                                                                    
081902     SKIP3                                                                
082002 S03-RECEIVE-CLOSE SECTION.                                               
082102                                                                          
082202     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
082302     CALL WZ01RECV USING RECV-CONTROL-AREA                                
082402                                                                          
082502     IF RECV-KDRC > 0                                                     
082602       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
082702       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
082802       DELIMITED BY SIZE INTO ERROR-TEXT                                  
082902       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
083002     END-IF                                                               
083102     .                                                                    
083202     EJECT                                                                
083302 S04-SEND-OPEN SECTION.                                                   
083402                                                                          
083502     MOVE 'OPEN'                   TO SEND-KDFUNC                         
083602     MOVE 'CARPARTS.3IV2.RESPPRINT' TO SEND-ADDISPABS                     
083702     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
083802                                                                          
083902     IF SEND-KDRC > 0                                                     
084002       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
084102       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
084202       DELIMITED BY SIZE INTO ERROR-TEXT                                  
084302       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
084402     END-IF                                                               
084502     .                                                                    
084602     SKIP3                                                                
084702 S04-SEND-MESSAGE SECTION.                                                
084802                                                                          
084902     MOVE 'PUT'                      TO SEND-KDFUNC                       
085002**** MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
085102     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
085202                                                                          
085204     IF SEND-KVDLEN = 0                                                   
085205       MOVE 1 TO SEND-KVDLEN                                              
085206     END-IF                                                               
085207                                                                          
085302     IF SEND-KDRC > 0                                                     
085402       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
085502       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
085602       DELIMITED BY SIZE INTO ERROR-TEXT                                  
085702       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
085802     END-IF                                                               
085902     .                                                                    
086002     SKIP3                                                                
086102 S04-SEND-CLOSE SECTION.                                                  
086202                                                                          
086302     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
086402     CALL WZ01SEND USING SEND-CONTROL-AREA                                
086502                                                                          
086602     IF SEND-KDRC > 0                                                     
086702       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
086802       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
086902       DELIMITED BY SIZE INTO ERROR-TEXT                                  
087002       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
087102     END-IF                                                               
087202     .                                                                    
087302     EJECT                                                                
087402                                                                          
087502 IMS-GU-WDE611 SECTION.                                                   
087602                                                                          
087702     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
087802          DELIMITED BY SIZE INTO SSA1                                     
087902     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X  ')'                        
088002          DELIMITED BY SIZE INTO SSA2                                     
088102     MOVE '  GE' TO GOOD-STATUSCODES                                      
088202     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
088302     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
088402     PERFORM IMS-STATUSCHECK                                              
088502     .                                                                    
088602     EJECT                                                                
088702 IMS-GU-WDE401 SECTION.                                                   
088802                                                                          
088902     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
089002          DELIMITED BY SIZE INTO SSA1                                     
089102     MOVE '  GE' TO GOOD-STATUSCODES                                      
089202     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
089302     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
089402     PERFORM IMS-STATUSCHECK                                              
089502     .                                                                    
089602     EJECT                                                                
089702 IMS-ISRT-4333-MSG         SECTION.                                       
089802                                                                          
089902     MOVE LOW-VALUE TO 4333-Z1 4333-Z2                                    
090002     MOVE SPACE TO GOOD-STATUSCODES                                       
090102     CALL CBLTDLI USING ISRT 4333-PCB 4333-MSG-IO-AREA                    
090202     MOVE 4333-STATUS-CODE TO STATUS-WS                                   
090302     PERFORM IMS-STATUSCHECK                                              
090402     .                                                                    
090502     EJECT                                                                
090602                                                                          
090702 IMS-ISRT-4341-MSG         SECTION.                                       
090802                                                                          
090902     MOVE LOW-VALUE TO 4341-Z1 4341-Z2                                    
091002     MOVE SPACE TO GOOD-STATUSCODES                                       
091102     CALL CBLTDLI USING ISRT 4341-PCB 4341-MSG-IO-AREA                    
091202     MOVE 4341-STATUS-CODE TO STATUS-WS                                   
091302     PERFORM IMS-STATUSCHECK                                              
091402     .                                                                    
091502     EJECT                                                                
091602                                                                          
091702 IMS-STATUSCHECK         SECTION.                                         
091802                                                                          
091902     SET STATUS-IX TO 1                                                   
092002     SEARCH GOOD-STATUS                                                   
092102       AT END                                                             
092202         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
092302         DELIMITED BY SIZE INTO ERROR-TEXT                                
092402         CALL FELLOG                                                      
092502       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
092602         CONTINUE                                                         
092702     END-SEARCH                                                           
092802     .                                                                    
