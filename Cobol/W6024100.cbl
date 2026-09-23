000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6024100.                                                
000400 AUTHOR.         SRINADH NADIMPALLI.                                      
000500 DATE-WRITTEN.   24/08/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME: DRIVER PROGRAM FOR SHOW AND RECEIVE CROSS DOCK CASES           
000900*                                                                         
001000*    FUNCTION:                                                            
001100*                                                                         
001200*        THE PROGRAM READS     WDE6                                       
001300*        THE PROGRAM READS     WDE4                                       
001400*        THE PROGRAM READS     WDE1                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6T241,W6T241U                                      
001800*        REQUEST:     W60241I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60241O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6024100'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000 77  WS-RESP-AREA                PIC S9(5)   VALUE ZERO COMP-3.           
004100                                                                          
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004500 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004600                                                                          
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W602CROS                PIC X(8)    VALUE 'W602CROS'.            
005800     SKIP3                                                                
005900*    --- PARAMETERS TO ABEND                                              
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
006700     03  ERR-UNAUTHORIZED        PIC X(3)   VALUE '00A'.                  
006800     03  UPDATE-DONE             PIC X(3)   VALUE '001'.                  
006900     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
007000     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
007100     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
007200     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
007300     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
007400     03  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
007500     03  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
007600     03  MUST-ENTER-EMP-ID       PIC X(3)   VALUE '026'.                  
007700     03  TRACKING-ID-MISSING     PIC X(3)   VALUE '422'.                  
007800     03  CASE-NOT-FOUND          PIC X(3)   VALUE '287'.                  
007900     EJECT                                                                
008000*01  -COPY W602CROS                                                       
008100     EJECT                                                                
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008400     SKIP3                                                                
008500*01  -COPY WZ01SUB                                                        
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008800     SKIP3                                                                
008900 01  REQU-AREA.                                                           
009000*    03  -COPY WZ01REQ2                                                   
009100*    03  -COPY W60241I1                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009400     SKIP3                                                                
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RES2                                                   
009700*    03  -COPY W60241O1                                                   
009800     EJECT                                                                
009900*    --- IMS FUNCTION CODES                                               
010000*01  -COPY W0003                                                          
010100     EJECT                                                                
010200                                                                          
010300 LINKAGE SECTION.                                                         
010400*01  -COPY W0009   -PRE MSG-                                              
010500 01  CROS-WDE6-PCB               PIC X.                                   
010600 01  CROS-WDE4-PCB               PIC X.                                   
010700 01  CROS-WDE41-PCB              PIC X.                                   
010800 01  CROS-WDE1-PCB               PIC X.                                   
010900 01  CROS-WDE6F-PCB              PIC X.                                   
010910 01  CROS-WDB6-PCB               PIC X.                                   
011000 01  PLATS-DM-PCB                PIC X.                                   
011100 01  PLATS-DN-PCB                PIC X.                                   
011200 01  PLATS-DP-PCB                PIC X.                                   
011300 01  PLATS-DO-PCB                PIC X.                                   
011400 01  PLATS-WDE6C-PCB             PIC X.                                   
011500 01  PLATS-GMTC-PCB              PIC X.                                   
011600 01  PLATS-WDB6-PCB              PIC X.                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING MSG-PCB CROS-WDE6-PCB CROS-WDE4-PCB            
011900                           CROS-WDE41-PCB                                 
012000                           CROS-WDE1-PCB CROS-WDE6F-PCB                   
012010                           CROS-WDB6-PCB                                  
012100                           PLATS-DM-PCB  PLATS-DN-PCB                     
012200                           PLATS-DP-PCB  PLATS-DO-PCB                     
012300                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
012400                           PLATS-WDB6-PCB.                                
012500 MAIN SECTION.                                                            
012600     ENTRY 'DLITCBL' USING MSG-PCB CROS-WDE6-PCB CROS-WDE4-PCB            
012700                           CROS-WDE41-PCB                                 
012800                           CROS-WDE1-PCB CROS-WDE6F-PCB                   
012810                           CROS-WDB6-PCB                                  
012900                           PLATS-DM-PCB  PLATS-DN-PCB                     
013000                           PLATS-DP-PCB  PLATS-DO-PCB                     
013100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
013200                           PLATS-WDB6-PCB.                                
013300                                                                          
013400*---- MOVE SUBPROGRAM CALL TO IT'S RIGHT PLACE ----                       
013500                                                                          
013600                                                                          
013700*------------------------                                                 
013800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013900     IF SUB-KDRC = 0                                                      
014000       PERFORM A-INIT                                                     
014100       PERFORM B-CHECK-KEYS                                               
014200       IF KEYS-OK                                                         
014300         PERFORM C-MOVE-REQ-DATA                                          
014310                                                                          
014400         CALL W602CROS USING 602CROS-W602CROS                             
014500                             CROS-WDE6-PCB CROS-WDE4-PCB                  
014600                             CROS-WDE41-PCB                               
014700                             CROS-WDE1-PCB CROS-WDE6F-PCB                 
014710                             CROS-WDB6-PCB                                
014800                             PLATS-DM-PCB  PLATS-DN-PCB                   
014900                             PLATS-DP-PCB  PLATS-DO-PCB                   
015000                             PLATS-WDE6C-PCB PLATS-GMTC-PCB               
015100                             PLATS-WDB6-PCB                               
015200         MOVE 602CROS-KVRADER TO MAX-INDX                                 
015300         MOVE 602CROS-IDMSG-INFO TO RESP-IDMSG-INFO                       
015310         IF 602CROS-KVRADER = ZERO                                        
015320          MOVE LINES-NOT-FOUND TO RESP-IDMSG-INFO                         
015330         END-IF                                                           
015400         PERFORM D-FILL-RESPAREA                                          
015500       END-IF                                                             
015600       PERFORM S02-RETURN-RESPONSE                                        
015700     END-IF                                                               
015800                                                                          
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600     MOVE ALL '+' TO RESP-AREA                                            
016700     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
016800                     RESP-IDMSG-INFO                                      
016900                     RESP-IDELMT-ERROR                                    
017000     MOVE ZERO    TO RESP-KVRADER                                         
017100     MOVE '001'   TO RESP-IDRESVER                                        
017200                                                                          
017300     .                                                                    
017400     EJECT                                                                
017500 B-CHECK-KEYS SECTION.                                                    
017600                                                                          
017700     MOVE YES TO KEYS-SW                                                  
017800     IF REQU-NOTREC-KEY = YES AND                                         
017900        REQU-TIRFSDAT-KEY NOT = ALL '+'                                   
018000        MOVE NOO TO KEYS-SW                                               
018100     END-IF                                                               
018200                                                                          
018300     IF KEYS-WRONG                                                        
018400       MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                        
018500     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 C-MOVE-REQ-DATA SECTION.                                                 
018810                                                                          
018900     MOVE 002        TO 602CROS-KDCALL                                    
019000     MOVE REQU-IDDC-KEY           TO 602CROS-IDDC-KEY                     
019100     MOVE REQU-KDPGMACT           TO 602CROS-KDPGMACT                     
019200     IF REQU-KDPGMACT = 'S'                                               
019300        IF REQU-TIRFSDAT-KEY NOT = ALL '+' AND                            
019310           REQU-TIRFSDAT-KEY NOT = LOW-VALUE                              
019400         MOVE REQU-TIRFSDAT-KEY       TO 602CROS-TIRFSDAT-KEY             
019500        ELSE                                                              
019600         MOVE ZERO TO 602CROS-TIRFSDAT-KEY                                
019700        END-IF                                                            
019800        IF REQU-IDDISTR-KEY NOT = ALL '+' AND                             
019810           REQU-IDDISTR-KEY NOT = LOW-VALUE                               
019900         MOVE REQU-IDDISTR-KEY       TO 602CROS-IDDISTR-KEY               
020000        ELSE                                                              
020100         MOVE ZERO TO 602CROS-IDDISTR-KEY                                 
020200        END-IF                                                            
020300        IF REQU-IDKUNDNR-KEY NOT = ALL '+' AND                            
020310           REQU-IDKUNDNR-KEY NOT = LOW-VALUE                              
020400         MOVE REQU-IDKUNDNR-KEY       TO 602CROS-IDKUNDNR-KEY             
020500        ELSE                                                              
020600         MOVE ZERO TO 602CROS-IDKUNDNR-KEY                                
020700        END-IF                                                            
020800        IF REQU-IDTRPTNR-CROSS-KEY NOT = ALL '+' AND                      
020810           REQU-IDTRPTNR-CROSS-KEY NOT = LOW-VALUE                        
020900        MOVE REQU-IDTRPTNR-CROSS-KEY TO 602CROS-IDTRPTNR-CROSS-KEY        
021000        ELSE                                                              
021100         MOVE ZERO TO 602CROS-IDTRPTNR-CROSS-KEY                          
021200        END-IF                                                            
021300        IF REQU-IDLEVNR-KEY NOT = ALL '+' AND                             
021310           REQU-IDLEVNR-KEY NOT = LOW-VALUE                               
021320         MOVE FUNCTION UPPER-CASE (REQU-IDLEVNR-KEY) TO                   
021330                                   REQU-IDLEVNR-KEY                       
021400         MOVE REQU-IDLEVNR-KEY        TO 602CROS-IDLEVNR-KEY              
021500        ELSE                                                              
021600         MOVE SPACES TO 602CROS-IDLEVNR-KEY                               
021700        END-IF                                                            
021800        IF REQU-IDSUPREF-KEY NOT = ALL '+' AND                            
021810           REQU-IDSUPREF-KEY NOT = LOW-VALUE                              
021820         MOVE FUNCTION UPPER-CASE (REQU-IDSUPREF-KEY) TO                  
021830                                   REQU-IDSUPREF-KEY                      
021900         MOVE REQU-IDSUPREF-KEY       TO 602CROS-IDSUPREF-KEY             
022000        ELSE                                                              
022100         MOVE SPACES TO 602CROS-IDSUPREF-KEY                              
022200        END-IF                                                            
022300        IF REQU-NOTREC-KEY NOT = ALL '+' AND                              
022310           REQU-NOTREC-KEY NOT = LOW-VALUE                                
022400         MOVE REQU-NOTREC-KEY         TO 602CROS-NOTREC-KEY               
022500        ELSE                                                              
022600         MOVE SPACES TO 602CROS-NOTREC-KEY                                
022700        END-IF                                                            
022800     ELSE                                                                 
022900        MOVE REQU-KVRADER TO MAX-INDX                                     
023000                             602CROS-KVRADER                              
023100        MOVE REQU-TOT-UNREC-VKORDBTO TO 602CROS-UNREC-VKORDBTO            
023200        MOVE REQU-TOT-UNREC-VLORDBTO TO 602CROS-UNREC-VLORDBTO            
023300        MOVE REQU-TOT-REC-VKORDBTO   TO 602CROS-REC-VKORDBTO              
023400        MOVE REQU-TOT-REC-VLORDBTO   TO 602CROS-REC-VLORDBTO              
023500        MOVE REQU-TOT-REC-KVKOLLI    TO 602CROS-REC-KVKOLLI               
023600        MOVE REQU-TOT-UNREC-KVKOLLI  TO 602CROS-UNREC-KVKOLLI             
023700        MOVE +1 TO INDX                                                   
023800        PERFORM UNTIL INDX > MAX-INDX                                     
023900         MOVE REQU-KDCMDVAL (INDX) TO 602CROS-KDCMDVAL (INDX)             
024000         MOVE REQU-IDDC-SEND(INDX) TO 602CROS-IDDC-SEND(INDX)             
024100         MOVE REQU-TIRECXDAT(INDX) TO 602CROS-TIRECXDAT(INDX)             
024200         MOVE REQU-TIRECXTID(INDX) TO 602CROS-TIRECXTID(INDX)             
024300         MOVE REQU-IDDISTR (INDX)  TO 602CROS-IDDISTR (INDX)              
024400         MOVE REQU-IDKUNDNR (INDX) TO 602CROS-IDKUNDNR (INDX)             
024500         MOVE REQU-IDORDNR5 (INDX) TO 602CROS-IDORDNR5 (INDX)             
024600         MOVE REQU-IDKOLLI (INDX)  TO 602CROS-IDKOLLI (INDX)              
024700         MOVE REQU-TIRFSDAT(INDX)  TO 602CROS-TIRFSDAT(INDX)              
024800         MOVE REQU-TISKEPPN(INDX)  TO 602CROS-TISKEPPN(INDX)              
024900         MOVE REQU-KDKOLLI (INDX)  TO 602CROS-KDKOLLI (INDX)              
024910         MOVE REQU-IDLBBET (INDX)  TO 602CROS-IDLBBET (INDX)              
025000         MOVE REQU-VKORDBTO(INDX)  TO 602CROS-VKORDBTO(INDX)              
025100         MOVE REQU-VLORDBTO(INDX)  TO 602CROS-VLORDBTO(INDX)              
025200         MOVE REQU-IDPSN   (INDX)  TO 602CROS-IDPSN (INDX)                
025300         MOVE REQU-IDPRODNR(INDX)  TO 602CROS-IDPRODNR(INDX)              
025400         MOVE REQU-IDTRPTNR-CROSS (INDX) TO                               
025500              602CROS-IDTRPTNR-CROSS (INDX)                               
025600         MOVE REQU-IDLEVNR (INDX)  TO 602CROS-IDLEVNR (INDX)              
025700         MOVE REQU-IDSUPREF(INDX)  TO 602CROS-IDSUPREF(INDX)              
025800         MOVE REQU-IDMSG-ERROR-LINE(INDX) TO                              
025900              602CROS-IDMSG-ERROR-LINE (INDX)                             
026000         ADD +1 TO INDX                                                   
026100        END-PERFORM                                                       
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 D-FILL-RESPAREA SECTION.                                                 
026600     MOVE +1 TO INDX                                                      
026700     MOVE 602CROS-UNREC-VKORDBTO  TO RESP-UNREC-VKORDBTO                  
026800     MOVE 602CROS-UNREC-VLORDBTO  TO RESP-UNREC-VLORDBTO                  
026900     MOVE 602CROS-REC-VKORDBTO    TO RESP-REC-VKORDBTO                    
027000     MOVE 602CROS-REC-VLORDBTO    TO RESP-REC-VLORDBTO                    
027100     MOVE 602CROS-REC-KVKOLLI     TO RESP-REC-KVKOLLI                     
027200     MOVE 602CROS-UNREC-KVKOLLI   TO RESP-UNREC-KVKOLLI                   
027300     PERFORM UNTIL INDX > MAX-INDX                                        
027400     MOVE 602CROS-KDCMDVAL (INDX) TO RESP-KDCMDVAL (INDX)                 
027500     MOVE 602CROS-IDDC-SEND(INDX) TO RESP-IDDC-SEND(INDX)                 
027600     IF 602CROS-TIRECXDAT(INDX) NOT NUMERIC                               
027700      MOVE ZERO TO RESP-TIRECXDAT(INDX)                                   
027701     ELSE                                                                 
027702      MOVE 602CROS-TIRECXDAT(INDX) TO RESP-TIRECXDAT(INDX)                
027703     END-IF                                                               
027704     IF 602CROS-TIRECXTID(INDX) NOT NUMERIC                               
027705      MOVE ZERO TO RESP-TIRECXTID(INDX)                                   
027706     ELSE                                                                 
027707      MOVE 602CROS-TIRECXTID(INDX) TO RESP-TIRECXTID(INDX)                
027708     END-IF                                                               
027800     MOVE 602CROS-IDDISTR (INDX)  TO RESP-IDDISTR (INDX)                  
027900     MOVE 602CROS-IDKUNDNR (INDX) TO RESP-IDKUNDNR (INDX)                 
028000     MOVE 602CROS-IDORDNR5 (INDX) TO RESP-IDORDNR5 (INDX)                 
028100     MOVE 602CROS-IDKOLLI (INDX)  TO RESP-IDKOLLI (INDX)                  
028200     MOVE 602CROS-TIRFSDAT(INDX)  TO RESP-TIRFSDAT(INDX)                  
028300     MOVE 602CROS-TISKEPPN(INDX)  TO RESP-TISKEPPN(INDX)                  
028400     MOVE 602CROS-KDKOLLI (INDX)  TO RESP-KDKOLLI (INDX)                  
028410     MOVE 602CROS-IDLBBET (INDX)  TO RESP-IDLBBET (INDX)                  
028500     MOVE 602CROS-VKORDBTO(INDX)  TO RESP-VKORDBTO(INDX)                  
028600     MOVE 602CROS-VLORDBTO(INDX)  TO RESP-VLORDBTO(INDX)                  
028700     MOVE 602CROS-IDPSN (INDX)    TO RESP-IDPSN   (INDX)                  
028800     MOVE 602CROS-IDPRODNR(INDX)  TO RESP-IDPRODNR(INDX)                  
028900     MOVE 602CROS-IDLEVNR (INDX)  TO RESP-IDLEVNR (INDX)                  
029000     MOVE 602CROS-IDSUPREF(INDX)  TO RESP-IDSUPREF(INDX)                  
029100     MOVE 602CROS-IDTRPTNR-CROSS (INDX) TO                                
029200                              RESP-IDTRPTNR-CROSS (INDX)                  
029300     MOVE 602CROS-IDMSG-ERROR-LINE (INDX) TO                              
029400                            RESP-IDMSG-ERROR-LINE (INDX)                  
029500     ADD +1 TO INDX                                                       
029600     END-PERFORM                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 Z-FINIT SECTION.                                                         
030000                                                                          
030100     .                                                                    
030200     EJECT                                                                
030300*    --- DISPATCHER SECTIONS                                              
030400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
030500                                                                          
030600     MOVE 'GETARG'               TO SUB-KDFUNC                            
030700     MOVE 'CARPARTS.PULS.CROSSDOCKCASES' TO SUB-ADDISPABS                 
030800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
030900                                                                          
031000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
031100                                                                          
031200     IF SUB-KDRC > 0                                                      
031300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
031400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
031500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031700     END-IF                                                               
031800     .                                                                    
031900     SKIP3                                                                
032000 S02-RETURN-RESPONSE SECTION.                                             
032100                                                                          
032200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
032300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
032400     MOVE 602CROS-KVRADER TO RESP-KVRADER                                 
032500     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
032600       - LENGTH OF RESP-TABELLRAD * (500 - RESP-KVRADER)                  
032700                                                                          
032800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
032900     MOVE WS-RESP-AREA               TO SUB-KVDLEN                        
033000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
033100                                                                          
033200     IF SUB-KDRC > 0                                                      
033300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
033400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
033500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
