000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018400.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   JUNI 2005.                                               
000500                                                                          
000600     REMARKS.                                                             
000700* WL018400 PROGRAM IS A REPLICA OF W4066600 PROGRAM                       
000800* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
000900*                                                                         
001000*    NAMN:       CARPARTS.LDC.ORDERSTARTCARRIERS                          
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        LÄSER ALLA PÅBÖRJADE LASTBÄRARE PÅ HÄNDELSREG.                   
001400*                                                                         
001500*        PROGRAMMET LÄSER      WL4495 (WDR4)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: WL0184                                              
001900*        REQU:        WL0184I1                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         WL0184O1                                            
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'WL018400'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
003500 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
003600 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
003700 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004500                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700 77  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
004800 77  WS-IDTRPTNR                 PIC X(3)    VALUE SPACE.                 
004900 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
005000 77  WS-FLFARLIG                 PIC X(1)    VALUE SPACE.                 
005100                                                                          
005200 77  W-IDTRPTNR                  PIC S9(3)   COMP-3 VALUE ZERO.           
005300 77  W-IDTRPTNR-NEXT             PIC S9(3)   COMP-3 VALUE ZERO.           
005400 77  W-IDTRPTNR-ENTER            PIC S9(3)   COMP-3 VALUE ZERO.           
005500 77  W-IDDC                      PIC  X(2)          VALUE SPACE.          
005600 77  W-IDLBBET                   PIC  X(12)         VALUE SPACE.          
005700 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
005800                                                                          
005900 77  SW-WL449501-LAEST           PIC X       VALUE 'N'.                   
006000     88  WL449501-LAEST                      VALUE 'J'.                   
006100                                                                          
006200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006300     88  NYCKLAR-OK                          VALUE 'J'.                   
006400     88  NYCKLAR-FEL                         VALUE 'N'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '4666'.                
006800     88  GODK-MID                            VALUE '4663' '4664'          
006900                                                   '4666'.                
007000     88  HELP-MID                            VALUE '0551'.                
007100                                                                          
007200 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
007300                                                                          
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008200     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
008300     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
008400*                                                                         
008500*    --- PARAMETERS TO ABEND                                              
008600*                                                                         
008700 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100                                                                          
009200 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
009300 01  MESSAGE-CODES.                                                       
009400     03  ERR-UNAUTHORIZED         PIC X(3)    VALUE '00A'.                
009500     03  ERR-MISSING              PIC X(3)    VALUE '041'.                
009600     03  ERR-WRONG-KEY            PIC X(3)    VALUE '022'.                
009700     03  ERR-CORR-FIELDS          PIC X(3)    VALUE '023'.                
009800     03  ERR-ARTIKEL-SAKNAS       PIC X(3)    VALUE '017'.                
009900     03  ERR-WRONG-AREA-FOR-ORDER PIC X(3)    VALUE '317'.                
010000     03  ERR-NOTHING             PIC X(3)    VALUE '005'.                 
010100     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010400                                                                          
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
010700*01  -COPY WZ01AUTH                                                       
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
011000*01  -COPY WMSGCONV                                                       
011100                                                                          
011200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011300                                                                          
011400*01  -COPY WZ01SUB                                                        
011500                                                                          
011600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011700                                                                          
011800 01  REQU-AREA.                                                           
011900*    03  -COPY WZ01REQ2                                                   
012000*    03  -COPY WL0184I1                                                   
012100                                                                          
012200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012300                                                                          
012400 01  RESP-AREA.                                                           
012500*    03  -COPY WZ01RES2                                                   
012600*    03  -COPY WL0184O1                                                   
012700                                                                          
012800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100 01  NYCKLAR-TILL-DLI.                                                    
013200     03  W-IDHTYP-MIN-X.                                                  
013300         05  W-IDHTYP-MIN        PIC X(4)    VALUE '4495'.                
013400     03  W-IDHTYP-MAX-X.                                                  
013500         05  W-IDHTYP-MAX        PIC X(4)    VALUE '4495'.                
013600                                                                          
013700*    --- STATUS-KOD FRÅN IMS                                              
013800 01  STATUS-WS                   PIC XX.                                  
013900     88  SEGMENT-FINNS                       VALUE '  '.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014200                                                                          
014300 01  GODK-STATUSKODER.                                                    
014400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014500                                                                          
014600 01  SSA1                        PIC X(96).                               
014700                                                                          
014800*    --- IMS FUNKTIONSKODER                                               
014900*01  -COPY W0003                                                          
015000                                                                          
015100*    ---  DLI INPUT-OUTPUT AREA                                           
015200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015300                                                                          
015400 01  DLI-IO-AREA.                                                         
015500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
015600                                                                          
015700     03  WL449501 REDEFINES IO-AREA.                                      
015800*        05  -COPY WDGX4495                                               
015900                                                                          
016000 LINKAGE SECTION.                                                         
016100                                                                          
016200*01  -COPY W0009   -PRE MSG-                                              
016300                                                                          
016400 01  ATAB-PCB                 PIC X.                                      
016500                                                                          
016600*01  -COPY W0008  -PRE 4495-                                              
016700     05  FILLER                  PIC X.                                   
016800                                                                          
016900 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB 4495-PCB.                     
017000                                                                          
017100 MAIN SECTION.                                                            
017200                                                                          
017300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017400     IF SUB-KDRC = 0                                                      
017500                                                                          
017600       PERFORM A-INIT                                                     
017700       PERFORM B-KOLLA-NYCKLAR                                            
017800       IF NYCKLAR-OK                                                      
017900         PERFORM F-LAES-VISA-INFO                                         
018000       END-IF                                                             
018100                                                                          
018200       IF SUB-KDTRANS (1:6) = 'WLA184'                                    
018300         PERFORM S11-MSG-CONV                                             
018400       END-IF                                                             
018500                                                                          
018600       PERFORM S02-RETURN-RESPONSE                                        
018700     END-IF                                                               
018800                                                                          
018900     MOVE ZERO                   TO RETURN-CODE                           
019000     GOBACK                                                               
019100     .                                                                    
019200                                                                          
019300 A-INIT SECTION.                                                          
019400     MOVE 'STA A-INIT        '   TO PGM-POS                               
019500                                                                          
019600     MOVE LOW-VALUES             TO RESP-WL0184O1                         
019700     MOVE 001                    TO RESP-IDRESVER                         
019800     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
019900                                    RESP-IDMSG-INFO                       
020000                                    RESP-IDELMT-ERROR                     
020100                                                                          
020200     MOVE ZERO                   TO RESP-KVRADER-MAX                      
020300     MOVE NEJ                    TO SW-WL449501-LAEST                     
020400                                                                          
020500     IF SUB-KDTRANS(1:6) = 'WLA184'                                       
020600       MOVE 001                  TO AUTH-KDCALL                           
020700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
020800                                    REQU-WZ01REQ2                         
020900       IF AUTH-KDRC > 0                                                   
021000         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
021100         MOVE NEJ                TO NYCKLAR-SW                            
021200       END-IF                                                             
021300       MOVE FUNCTION UPPER-CASE (REQU-IDUSER)                             
021400                                 TO REQU-IDUSER                           
021500       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                           
021600                                 TO REQU-IDDC-KEY                         
021700     END-IF                                                               
021800     .                                                                    
021900                                                                          
022000 B-KOLLA-NYCKLAR SECTION.                                                 
022100     MOVE 'STA B-KOLLA-NYCKLAR ' TO PGM-POS                               
022200                                                                          
022300     MOVE JA                     TO NYCKLAR-SW                            
022400                                                                          
022500*    -- KONTROLL AV IDTRPTNR                                              
022600                                                                          
022700     IF REQU-IDTRPTNR-KEY = LOW-VALUES                                    
022800       MOVE ZERO                 TO WS-IDTRPTNR                           
022900     ELSE                                                                 
023000       MOVE REQU-IDTRPTNR-KEY    TO WS-IDTRPTNR                           
023100     END-IF                                                               
023200                                                                          
023300                                                                          
023400     IF WS-IDTRPTNR NUMERIC AND WS-IDTRPTNR > ZERO                        
023500       MOVE WS-IDTRPTNR          TO W-IDTRPTNR                            
023600     ELSE                                                                 
023700       MOVE ZERO                 TO W-IDTRPTNR                            
023800     END-IF                                                               
023900                                                                          
024000     MOVE WS-IDTRPTNR            TO RESP-IDTRPTNR-KEY                     
024100     MOVE REQU-IDDC-KEY          TO RESP-IDDC-KEY                         
024200                                                                          
024300     IF NYCKLAR-FEL                                                       
024400       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
024500       MOVE 'IDTRPTNR'           TO RESP-IDELMT-ERROR                     
024600     END-IF                                                               
024700     MOVE 'END B-KOLLA-NYCKLAR ' TO PGM-POS                               
024800     .                                                                    
024900                                                                          
025000 F-LAES-VISA-INFO SECTION.                                                
025100     MOVE 'STA F-LAES-VISA-INFO' TO PGM-POS                               
025200                                                                          
025300     PERFORM IMS-GU-WL449501                                              
025400     IF W-IDTRPTNR > ZERO                                                 
025500       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
025600                    (4495-IDDC     = REQU-IDDC-KEY AND                    
025700                     4495-IDTRPTNR = W-IDTRPTNR)                          
025800         PERFORM IMS-GN-WL449501                                          
025900       END-PERFORM                                                        
026000     END-IF                                                               
026100     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
026200       MOVE ERR-LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                      
026300       MOVE 'IDTRPTNR'           TO RESP-IDELMT-ERROR                     
026400     ELSE                                                                 
026500       MOVE +1                   TO INDX                                  
026600                                                                          
026700       PERFORM UNTIL INDX > MAX-INDX                                      
026800         IF SEGMENT-FINNS                                                 
026900           IF (W-IDTRPTNR         =  ZERO AND                             
027000               REQU-IDDC-KEY       =  4495-IDDC)    OR                    
027100              (REQU-IDDC-KEY       =  4495-IDDC     AND                   
027200               W-IDTRPTNR          =  4495-IDTRPTNR)                      
027300             COMPUTE RESP-KVRADER-MAX = INDX                              
027400             MOVE 4495-IDTRPTNR  TO RESP-IDTRPTNR (INDX)                  
027500             MOVE 4495-IDLBBET   TO RESP-IDLBBET (INDX)                   
027600             ADD 1               TO INDX                                  
027700           END-IF                                                         
027800           PERFORM IMS-GN-WL449501                                        
027900         ELSE                                                             
028000           ADD 1                 TO INDX                                  
028100         END-IF                                                           
028200       END-PERFORM                                                        
028300                                                                          
028400     END-IF                                                               
028500     .                                                                    
028600                                                                          
028700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
028800     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
028900                                                                          
029000     MOVE 'GETARG'               TO SUB-KDFUNC                            
029100     MOVE 'CARPARTS.LDC.ORDERSTARTCARRIERS' TO SUB-ADDISPABS              
029200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
029300                                                                          
029400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
029500                                                                          
029600     IF SUB-KDRC > 0                                                      
029700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
029800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
029900       DELIMITED BY SIZE INTO FELTEXT                                     
030000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030100     END-IF                                                               
030200     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
030300     .                                                                    
030400                                                                          
030500 S02-RETURN-RESPONSE SECTION.                                             
030600     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
030700                                                                          
030800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
030900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
031000                                                                          
031100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
031200                                                                          
031300     IF SUB-KDRC > 0                                                      
031400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
031500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
031600       DELIMITED BY SIZE INTO FELTEXT                                     
031700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031800     END-IF                                                               
031900     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
032000     .                                                                    
032100                                                                          
032200 S11-MSG-CONV SECTION.                                                    
032300     MOVE LOW-VALUES             TO RESP-MESSAGES (1)                     
032400                                    RESP-MESSAGES (2)                     
032500     MOVE 1                      TO MSG-IX                                
032600*    REQUEST OK                                                           
032700     MOVE 200                    TO RESP-KDSTATUS-API                     
032800     IF RESP-IDMSG-INFO > SPACE                                           
032900       MOVE SPACES               TO MSG-CONV-AREA                         
033000       MOVE RESP-IDMSG-INFO      TO MSG-CONV-IDMSG-IN                     
033100       CALL WMSGCONV          USING MSG-CONV-AREA                         
033200       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
033300       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
033400       ADD 1                     TO MSG-IX                                
033500     END-IF                                                               
033600     IF RESP-IDMSG-ERROR > SPACE                                          
033700*      BAD REQUEST                                                        
033800       MOVE 400                  TO RESP-KDSTATUS-API                     
033900       MOVE SPACES               TO MSG-CONV-AREA                         
034000       MOVE RESP-IDMSG-ERROR     TO MSG-CONV-IDMSG-IN                     
034100       MOVE RESP-IDELMT-ERROR    TO MSG-CONV-IDELMT                       
034200       CALL WMSGCONV          USING MSG-CONV-AREA                         
034300       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
034400       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
034500     END-IF                                                               
034600     .                                                                    
034700* --- IMS SEKTIONER ---                                                   
034800                                                                          
034900 IMS-GU-WL449501  SECTION.                                                
035000     STRING 'WL449501(IDHTYP  >=' W-IDHTYP-MIN-X                          
035100                    '&IDHTYP  <=' W-IDHTYP-MAX-X ')'                      
035200                      DELIMITED BY SIZE INTO SSA1                         
035300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
035400     CALL CBLTDLI USING GU 4495-PCB IO-AREA SSA1                          
035500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800                                                                          
035900 IMS-GN-WL449501  SECTION.                                                
036000     STRING 'WL449501(IDHTYP  >=' W-IDHTYP-MIN-X                          
036100                    '&IDHTYP  <=' W-IDHTYP-MAX-X ')'                      
036200                      DELIMITED BY SIZE INTO SSA1                         
036300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
036400     CALL CBLTDLI USING GN 4495-PCB IO-AREA SSA1                          
036500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
036600     PERFORM IMS-STATUSKONTROLL                                           
036700     .                                                                    
036800                                                                          
036900 IMS-STATUSKONTROLL SECTION.                                              
037000                                                                          
037100     SET STATUS-IX TO 1                                                   
037200     SEARCH GODK-STATUS                                                   
037300       AT END                                                             
037400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037500         DELIMITED BY SIZE INTO FELTEXT                                   
037600         CALL FELLOG                                                      
037700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037800         CONTINUE                                                         
037900     END-SEARCH                                                           
038000     .                                                                    
