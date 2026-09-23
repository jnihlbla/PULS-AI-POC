000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ020100.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   02/02/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        MAINTENANCE OF WEB USER PROFILE                                  
001000*        IT IS ALSO USED TO BACKUP WEB SESSION DATA (KDPGMACT V)          
001100*                                                                         
001200*        THE PROGRAM UPDATES TABLE TZ2PROF                                
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: WZ0201T                                             
001600*        REQUEST:     WZ0201I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ0201O1                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800 77  IDPGM                       PIC X(08)   VALUE 'WZ020100'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300                                                                          
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
003800     88  KEYS-OK                             VALUE 'J'.                   
003900     88  KEYS-WRONG                          VALUE 'N'.                   
004000                                                                          
004100 77  MSG-IX                      PIC S9(9) VALUE +0 COMP SYNC.            
004200*    --- WORK FIELDS USED WHEN STRINGING PROFILE SEGMENTS                 
004300*    --- INTO THE RESPONSE, OR EXTRACTING SEGMENTS FROM                   
004400*    --- THE REQUEST.                                                     
004500 01  PIX                         PIC S9(9)   BINARY.                      
004600                                                                          
004700 01  REMAINING-DATA              PIC S9(9)   BINARY.                      
004800                                                                          
004900 01  LENGTH-OF-PIECE             PIC S9(9)   BINARY.                      
005000                                                                          
005100 01  AUTH-IDUSER-START           PIC S9(9)   BINARY.                      
005200 01  AUTH-IDUSER-SLUT            PIC S9(9)   BINARY.                      
005300 01  AUTH-LENG                   PIC S9(9)   BINARY.                      
005400 01  PROF-IDDC-START             PIC S9(9)   BINARY.                      
005500 01  PROF-IDDC-END               PIC S9(9)   BINARY.                      
005600 01  PROF-IDDC-LEN               PIC S9(9)   BINARY.                      
005700                                                                          
005800 77  WS-OPEN-TAG                 PIC X(30).                               
005900                                                                          
006000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
006600     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
006700                                                                          
006800*    --- PARAMETERS TO ABEND                                              
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007400                                                                          
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007700     03  ERR-MISSING-IN-TAB      PIC X(3)    VALUE '025'.                 
007800     03  ERR-SYSTEM              PIC X(3)    VALUE '099'.                 
007900     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
008000                                                                          
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008300*01  -COPY WZ01SUB                                                        
008400                                                                          
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
008600*01  -COPY WMSGCONV                                                       
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
008900*01  -COPY WZ01AUTH                                                       
009000                                                                          
009100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009200                                                                          
009300 01  REQU-AREA.                                                           
009400*    03  -COPY WZ01REQU                                                   
009500*    03  -COPY WZ0201I1                                                   
009600 01  REQU-DATA-2.                                                         
009700*    03  -COPY WZ01REQ2 -PRE R2-                                          
009800                                                                          
009900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000                                                                          
010100 01  RESP-AREA.                                                           
010200*    03  -COPY WZ01RESP                                                   
010300*    03  -COPY WZ0201O1                                                   
010400 01  RESP-VER-2.                                                          
010500*    03  -COPY WZ01RES2 -PRE R2-                                          
010600*    03  -COPY WZ0201O2 -PRE R2-                                          
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011000                                                                          
011100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011200 01  DB2-WS.                                                              
011300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011400         88  CURSOR-OK                       VALUE 000.                   
011500         88  LINES-FOUND                     VALUE 000.                   
011600         88  LINES-MISSING                   VALUE 100.                   
011700         88  RESOURCE-WRONG                  VALUE 904.                   
011800     03  GOOD-SQLCODECODES.                                               
011900         05  GOOD-SQLCODE OCCURS 5                                        
012000             INDEXED BY SQLCODE-IX PIC 9(3).                              
012100                                                                          
012200 01  FILLER                      PIC X(16)   VALUE 'TZ2PROF-AREA'.        
012300*                                                                         
012400*01  -COPY TZ2PROF -PRE TZ2PROF-                                          
012500                                                                          
012600     EXEC SQL INCLUDE TZ2PROF END-EXEC.                                   
012700                                                                          
012800 PROCEDURE DIVISION.                                                      
012900                                                                          
013000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013100     IF SUB-KDRC = 0                                                      
013200       PERFORM A-INIT                                                     
013300       IF KEYS-OK                                                         
013400         PERFORM B-CHECK-KEYS                                             
013500         IF KEYS-OK                                                       
013600           IF REQU-KDPGMACT = SPACE OR                                    
013700              REQU-QUERY                                                  
013800*            -- QUERY PROFILE DATA                                        
013900             PERFORM F-FETCH-INFO                                         
014000           ELSE                                                           
014100             IF REQU-KDPGMACT = 'U'                                       
014200*              -- UPDATE PROFILE OR BACKUP SESSION DATA                   
014300               PERFORM G-UPDATE-INSERT-DELETE-INFO                        
014400             END-IF                                                       
014500           END-IF                                                         
014600         END-IF                                                           
014700       END-IF                                                             
014800       IF REQU-KDPGMACT NOT = 'V'                                         
014900*        -- NO RESPONSE WHEN BACKING UP SESSION DATA                      
015000         PERFORM S02-RETURN-RESPONSE                                      
015100       END-IF                                                             
015200     END-IF                                                               
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700                                                                          
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     INITIALIZE RESP-AREA                                                 
016100     MOVE '000'   TO RESP-IDMSG-INFO                                      
016200     MOVE '000'   TO RESP-IDMSG-ERROR                                     
016300     MOVE ALL '+' TO RESP-IDELMT-ERROR                                    
016400     MOVE 001     TO RESP-IDMSGVER                                        
016500     MOVE ZERO    TO RESP-TEPROFDATA-L                                    
016600     MOVE SPACE   TO RESP-TEPROFDATA-TOT                                  
016700                                                                          
016800     INITIALIZE GOOD-SQLCODECODES                                         
016900                                                                          
017000     IF SUB-KDTRANS(1:6) = 'WZA201'                                       
017100       MOVE REQU-AREA            TO REQU-DATA-2                           
017200       MOVE 002                  TO R2-RESP-IDRESVER                      
017300       MOVE 001                  TO AUTH-KDCALL                           
017400       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
017500                                    R2-REQU-WZ01REQ2                      
017600       IF AUTH-KDRC > 0                                                   
017700         MOVE ERR-UNAUTHORIZED   TO R2-RESP-IDMSG-ERROR                   
017800         MOVE NOO                TO KEYS-SW                               
017900       ELSE                                                               
018000         MOVE FUNCTION UPPER-CASE (R2-REQU-IDUSER)                        
018100                                 TO REQU-IDUSER                           
018200       END-IF                                                             
018300       MOVE R2-REQU-IDREQVER     TO REQU-IDMSGVER                         
018400       MOVE R2-REQU-KDPGMACT     TO REQU-KDPGMACT                         
018500       MOVE SPACES               TO REQU-IDPROFGRP                        
018600     END-IF                                                               
018700     .                                                                    
018800                                                                          
018900 B-CHECK-KEYS SECTION.                                                    
019000                                                                          
019100     MOVE YES TO KEYS-SW                                                  
019200                                                                          
019300     IF REQU-IDMSGVER NOT = 001 AND 002                                   
019400       MOVE 'IDMSGVER' TO RESP-IDELMT-ERROR                               
019500                          R2-RESP-IDELMT-ERROR                            
019600       SET KEYS-WRONG TO TRUE                                             
019700     END-IF                                                               
019800     IF REQU-IDMSGVER = 001 AND                                           
019900        (REQU-KDPGMACT = SPACE OR 'U' OR 'V')                             
020000       CONTINUE                                                           
020100     ELSE                                                                 
020200       IF REQU-IDMSGVER = 002 AND REQU-QUERY                              
020300         CONTINUE                                                         
020400       ELSE                                                               
020500         MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                     
020600                                    R2-RESP-IDELMT-ERROR                  
020700         SET KEYS-WRONG          TO TRUE                                  
020800       END-IF                                                             
020900     END-IF                                                               
021000     IF REQU-IDUSER = SPACE OR ALL '+'                                    
021100       MOVE 'IDUSER' TO RESP-IDELMT-ERROR                                 
021200                        R2-RESP-IDELMT-ERROR                              
021300       SET KEYS-WRONG TO TRUE                                             
021400     END-IF                                                               
021500                                                                          
021600     IF KEYS-WRONG                                                        
021700       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
021800                             R2-RESP-IDMSG-ERROR                          
021900     END-IF                                                               
022000     .                                                                    
022100                                                                          
022200 F-FETCH-INFO SECTION.                                                    
022300                                                                          
022400     MOVE REQU-IDUSER    TO TZ2PROF-IDUSER                                
022500     MOVE REQU-IDPROFGRP TO TZ2PROF-IDPROFGRP                             
022600     PERFORM DB2-DCL-OPN-TZ2PROF-CRS                                      
022700                                                                          
022800     MOVE 1 TO PIX                                                        
022900     SET LINES-FOUND TO TRUE                                              
023000     PERFORM DB2-FETCH-TZ2PROF-CRS                                        
023100     PERFORM UNTIL LINES-MISSING                                          
023200                                                                          
023300       STRING TZ2PROF-TEPROFDATA-D(1:TZ2PROF-TEPROFDATA-L)                
023400              DELIMITED BY SIZE                                           
023500         INTO RESP-TEPROFDATA-TOT WITH POINTER PIX                        
023600         ON OVERFLOW                                                      
023700           MOVE 'PROFILE DATA TOO LONG' TO ERROR-TEXT                     
023800           CALL ABEND USING RKOD-ABEND-NO-DUMP                            
023900       END-STRING                                                         
024000                                                                          
024100       PERFORM DB2-FETCH-TZ2PROF-CRS                                      
024200     END-PERFORM                                                          
024300                                                                          
024400     COMPUTE RESP-TEPROFDATA-L = PIX - 1                                  
024500     IF PIX = 1                                                           
024600       MOVE 'IDUSER'           TO RESP-IDELMT-ERROR                       
024700                                  R2-RESP-IDELMT-ERROR                    
024800       MOVE ERR-MISSING-IN-TAB TO RESP-IDMSG-ERROR                        
024900                                  R2-RESP-IDMSG-ERROR                     
025000       MOVE SPACE TO RESP-TEPROFDATA-TOT                                  
025100     ELSE                                                                 
025200*      -- CHECK THAT AUTH.IDUSER FIELD IN THE PROFILE CONTAINS            
025300*      -- THE CORRECT USER ID. IF NOT - CHANGE IT TO THE CORRECT          
025400*      -- VALUE TO PREVENT THE OTHER USER FROM BEING HACKED OUT.          
025500       MOVE 1 TO AUTH-IDUSER-START                                        
025600       INSPECT RESP-TEPROFDATA-TOT (1:RESP-TEPROFDATA-L)                  
025700       TALLYING AUTH-IDUSER-START FOR CHARACTERS                          
025800       BEFORE INITIAL '<AUTH.IDUSER>'                                     
025900       ADD 13 TO AUTH-IDUSER-START                                        
026000       IF AUTH-IDUSER-START < RESP-TEPROFDATA-L                           
026100         MOVE 1 TO AUTH-IDUSER-SLUT                                       
026200         INSPECT RESP-TEPROFDATA-TOT (1:RESP-TEPROFDATA-L)                
026300         TALLYING AUTH-IDUSER-SLUT  FOR CHARACTERS                        
026400         BEFORE INITIAL '</AUTH.IDUSER>'                                  
026500         IF  AUTH-IDUSER-SLUT > AUTH-IDUSER-START                         
026600         AND AUTH-IDUSER-SLUT < RESP-TEPROFDATA-L                         
026700           SUBTRACT AUTH-IDUSER-START FROM AUTH-IDUSER-SLUT               
026800           GIVING AUTH-LENG                                               
026900           IF RESP-TEPROFDATA-TOT(AUTH-IDUSER-START:AUTH-LENG)            
027000           NOT = REQU-IDUSER                                              
027100              MOVE REQU-IDUSER TO                                         
027200                 RESP-TEPROFDATA-TOT(AUTH-IDUSER-START:AUTH-LENG)         
027300           END-IF                                                         
027400         END-IF                                                           
027500       END-IF                                                             
027600       IF SUB-KDTRANS(1:6) = 'WZA201'                                     
027700         PERFORM FA-EXTRACT-INFO                                          
027800       END-IF                                                             
027900     END-IF                                                               
028000     .                                                                    
028100                                                                          
028200 FA-EXTRACT-INFO SECTION.                                                 
028300                                                                          
028400     XML PARSE RESP-TEPROFDATA-TOT (1:RESP-TEPROFDATA-L)                  
028500       PROCESSING PROCEDURE FAA-HANDLE-PARSE                              
028600       ON EXCEPTION                                                       
028700         PERFORM FAB-HANDLE-EXCEPTION                                     
028800       NOT ON EXCEPTION                                                   
028900         PERFORM FAC-CHECK-IDDC                                           
029000     END-XML                                                              
029100     .                                                                    
029200                                                                          
029300 FAA-HANDLE-PARSE SECTION.                                                
029400                                                                          
029500     EVALUATE XML-EVENT                                                   
029600       WHEN 'START-OF-ELEMENT'                                            
029700         MOVE XML-TEXT           TO WS-OPEN-TAG                           
029800       WHEN 'CONTENT-CHARACTERS'                                          
029900         EVALUATE WS-OPEN-TAG                                             
030000           WHEN 'PROF.IDDC'                                               
030100             MOVE XML-TEXT       TO R2-RESP-IDDC                          
030200           WHEN 'PROF.IDSPRAK'                                            
030300             MOVE XML-TEXT       TO R2-RESP-IDSPRAK                       
030400           WHEN 'PROF.KDMATT'                                             
030500             MOVE XML-TEXT       TO R2-RESP-KDMATT                        
030600           WHEN 'PROF.IDRT'                                               
030700             MOVE XML-TEXT       TO R2-RESP-IDRT                          
030800           WHEN 'PROF.IDLEGSEL'                                           
030900             MOVE XML-TEXT       TO R2-RESP-IDLEGSEL                      
031000           WHEN 'PROF.IDFTG'                                              
031100             MOVE XML-TEXT       TO R2-RESP-IDFTG                         
031200         END-EVALUATE                                                     
031300       WHEN 'END-OF-ELEMENT'                                              
031400         MOVE SPACES             TO WS-OPEN-TAG                           
031500     END-EVALUATE                                                         
031600     .                                                                    
031700                                                                          
031800 FAB-HANDLE-EXCEPTION SECTION.                                            
031900                                                                          
032000     MOVE 'PROFILE'              TO RESP-IDELMT-ERROR                     
032100                                    R2-RESP-IDELMT-ERROR                  
032200     MOVE ERR-SYSTEM             TO RESP-IDMSG-ERROR                      
032300                                    R2-RESP-IDMSG-ERROR                   
032400     .                                                                    
032500                                                                          
032600 FAC-CHECK-IDDC SECTION.                                                  
032700                                                                          
032800     IF R2-RESP-IDDC = SPACES OR LOW-VALUES                               
032900       MOVE 'IDUSER'             TO RESP-IDELMT-ERROR                     
033000                                    R2-RESP-IDELMT-ERROR                  
033100       MOVE ERR-MISSING-IN-TAB   TO RESP-IDMSG-ERROR                      
033200                                    R2-RESP-IDMSG-ERROR                   
033300     END-IF                                                               
033400     .                                                                    
033500                                                                          
033600 G-UPDATE-INSERT-DELETE-INFO SECTION.                                     
033700                                                                          
033800*      -- CHECK THAT AUTH.IDUSER FIELD IN THE PROFILE CONTAINS            
033900*      -- THE CORRECT USER ID. IF NOT - CHANGE IT TO THE CORRECT          
034000*      -- VALUE TO PREVENT THE OTHER USER FROM BEING HACKED OUT.          
034100       MOVE 1 TO AUTH-IDUSER-START                                        
034200       INSPECT REQU-TEPROFDATA-TOT (1:REQU-TEPROFDATA-L)                  
034300       TALLYING AUTH-IDUSER-START FOR CHARACTERS                          
034400       BEFORE INITIAL '<AUTH.IDUSER>'                                     
034500       ADD 13 TO AUTH-IDUSER-START                                        
034600       IF AUTH-IDUSER-START < REQU-TEPROFDATA-L                           
034700         MOVE 1 TO AUTH-IDUSER-SLUT                                       
034800         INSPECT REQU-TEPROFDATA-TOT (1:REQU-TEPROFDATA-L)                
034900         TALLYING AUTH-IDUSER-SLUT  FOR CHARACTERS                        
035000         BEFORE INITIAL '</AUTH.IDUSER>'                                  
035100         IF  AUTH-IDUSER-SLUT > AUTH-IDUSER-START                         
035200         AND AUTH-IDUSER-SLUT < REQU-TEPROFDATA-L                         
035300           SUBTRACT AUTH-IDUSER-START FROM AUTH-IDUSER-SLUT               
035400           GIVING AUTH-LENG                                               
035500           IF REQU-TEPROFDATA-TOT(AUTH-IDUSER-START:AUTH-LENG)            
035600           NOT = REQU-IDUSER                                              
035700              MOVE REQU-IDUSER TO                                         
035800                 REQU-TEPROFDATA-TOT(AUTH-IDUSER-START:AUTH-LENG)         
035900           END-IF                                                         
036000         END-IF                                                           
036100       END-IF                                                             
036200                                                                          
036300     MOVE REQU-IDUSER    TO TZ2PROF-IDUSER                                
036400     MOVE REQU-IDPROFGRP TO TZ2PROF-IDPROFGRP                             
036500     PERFORM DB2-DCL-OPN-TZ2PROF-CRS                                      
036600                                                                          
036700     MOVE 1    TO PIX                                                     
036800     MOVE ZERO TO TZ2PROF-IDLOPNR                                         
036900                                                                          
037000*--  REPLACE EXISTING ROWS IN TABLE                                       
037100     SET LINES-FOUND TO TRUE                                              
037200     PERFORM DB2-FETCH-TZ2PROF-CRS                                        
037300     PERFORM UNTIL LINES-MISSING OR PIX > REQU-TEPROFDATA-L               
037400       PERFORM S10-NEXT-PIECE-OF-DATA                                     
037500       PERFORM DB2-UPDATE-TZ2PROF-TAB                                     
037600       PERFORM DB2-FETCH-TZ2PROF-CRS                                      
037700     END-PERFORM                                                          
037800*--  LINES ARE MISSING, OR PIX > REQUEST DATA LENGTH, SO ....             
037900                                                                          
038000*--  .... EITHER DELETE UNUSED ROWS FROM TABLE, ....                      
038100     PERFORM UNTIL LINES-MISSING                                          
038200       PERFORM DB2-DELETE-TZ2PROF-TAB                                     
038300       PERFORM DB2-FETCH-TZ2PROF-CRS                                      
038400     END-PERFORM                                                          
038500                                                                          
038600*--  .... OR ADD NEW ROWS TO TABLE                                        
038700     PERFORM UNTIL PIX > REQU-TEPROFDATA-L                                
038800       ADD 1 TO TZ2PROF-IDLOPNR                                           
038900       PERFORM S10-NEXT-PIECE-OF-DATA                                     
039000       PERFORM DB2-INSERT-TZ2PROF-TAB                                     
039100     END-PERFORM                                                          
039200     .                                                                    
039300                                                                          
039400*    --- DISPATCHER SECTIONS                                              
039500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
039600                                                                          
039700     MOVE 'GETARG'                   TO SUB-KDFUNC                        
039800     MOVE 'CARPARTS.COMMON.USERPROF' TO SUB-ADDISPABS                     
039900     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
040000                                                                          
040100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
040200                                                                          
040300     IF SUB-KDRC > 0                                                      
040400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
040600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
040700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040800     END-IF                                                               
040900     .                                                                    
041000                                                                          
041100 S02-RETURN-RESPONSE SECTION.                                             
041200                                                                          
041300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
041400                                                                          
041500     IF SUB-KDTRANS(1:6) = 'WZA201'                                       
041600       PERFORM S11-MSG-CONV                                               
041700       MOVE RESP-VER-2           TO RESP-AREA                             
041800       COMPUTE SUB-KVDLEN = LENGTH OF RESP-VER-2                          
041900     ELSE                                                                 
042000*      -- COMPUTE ACTUAL LENGTH OF RESPONSE WHICH DEPENDS                 
042100*      -- ON THE LENGTH OF THE VARCHAR FIELD TEPROFDATA                   
042200       COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                         
042300                            LENGTH OF RESP-TEPROFDATA-TOT +               
042400                            RESP-TEPROFDATA-L                             
042500     END-IF                                                               
042600                                                                          
042700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
042800                                                                          
042900     IF SUB-KDRC > 0                                                      
043000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
043100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
043200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
043300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043400     END-IF                                                               
043500     .                                                                    
043600                                                                          
043700 S10-NEXT-PIECE-OF-DATA SECTION.                                          
043800                                                                          
043900     COMPUTE REMAINING-DATA = REQU-TEPROFDATA-L - PIX + 1                 
044000     COMPUTE                                                              
044100         LENGTH-OF-PIECE = FUNCTION MIN( REMAINING-DATA, 10000)           
044200                                                                          
044300     MOVE LENGTH-OF-PIECE                TO TZ2PROF-TEPROFDATA-L          
044400     MOVE REQU-TEPROFDATA-TOT(PIX:LENGTH-OF-PIECE)                        
044500                                         TO TZ2PROF-TEPROFDATA-D          
044600                                                                          
044700     ADD LENGTH-OF-PIECE TO PIX                                           
044800     .                                                                    
044900 S11-MSG-CONV SECTION.                                                    
045000     MOVE SPACES                 TO R2-RESP-MESSAGES (1)                  
045100                                    R2-RESP-MESSAGES (2)                  
045200     MOVE 1                      TO MSG-IX                                
045300*    REQUEST OK                                                           
045400     MOVE 200                    TO R2-RESP-KDSTATUS-API                  
045500     IF R2-RESP-IDMSG-INFO > SPACE                                        
045600       MOVE SPACES               TO MSG-CONV-AREA                         
045700       MOVE R2-RESP-IDMSG-INFO   TO MSG-CONV-IDMSG-IN                     
045800       CALL WMSGCONV          USING MSG-CONV-AREA                         
045900       MOVE MSG-CONV-IDMSG-OUT   TO R2-RESP-IDMSG   (MSG-IX)              
046000       MOVE MSG-CONV-MESSAGE     TO R2-RESP-MESSAGE (MSG-IX)              
046100       ADD 1                     TO MSG-IX                                
046200     END-IF                                                               
046300     IF R2-RESP-IDMSG-ERROR > SPACE                                       
046400*      BAD REQUEST                                                        
046500       MOVE 400                  TO R2-RESP-KDSTATUS-API                  
046600       MOVE SPACES               TO MSG-CONV-AREA                         
046700       MOVE R2-RESP-IDMSG-ERROR  TO MSG-CONV-IDMSG-IN                     
046800       MOVE R2-RESP-IDELMT-ERROR TO MSG-CONV-IDELMT                       
046900       CALL WMSGCONV          USING MSG-CONV-AREA                         
047000       MOVE MSG-CONV-IDMSG-OUT   TO R2-RESP-IDMSG   (MSG-IX)              
047100       MOVE MSG-CONV-MESSAGE     TO R2-RESP-MESSAGE (MSG-IX)              
047200     END-IF                                                               
047300     .                                                                    
047400                                                                          
047500 DB2-DCL-OPN-TZ2PROF-CRS  SECTION.                                        
047600                                                                          
047700     MOVE 000100  TO GOOD-SQLCODECODES                                    
047800     EXEC SQL                                                             
047900         DECLARE TZ2PROF-CRS CURSOR FOR                                   
048000                                                                          
048100         SELECT  IDLOPNR, TEPROFDATA                                      
048200         FROM    TZ2PROF                                                  
048300         WHERE   IDUSER = :REQU-IDUSER                                    
048400         AND     IDPROFGRP = :REQU-IDPROFGRP                              
048500         FOR UPDATE OF TEPROFDATA                                         
048600     END-EXEC                                                             
048700                                                                          
048800     MOVE 000100  TO GOOD-SQLCODECODES                                    
048900     EXEC SQL OPEN TZ2PROF-CRS END-EXEC                                   
049000                                                                          
049100     .                                                                    
049200                                                                          
049300 DB2-FETCH-TZ2PROF-CRS  SECTION.                                          
049400                                                                          
049500     MOVE 000100  TO GOOD-SQLCODECODES                                    
049600     EXEC SQL                                                             
049700       FETCH TZ2PROF-CRS INTO                                             
049800          :TZ2PROF-IDLOPNR                                                
049900        , :TZ2PROF-TEPROFDATA                                             
050000     END-EXEC                                                             
050100                                                                          
050200     MOVE SQLCODE TO SQLCODE-WS                                           
050300     PERFORM DB2-STATUS-CHECK                                             
050400     .                                                                    
050500                                                                          
050600 DB2-CLOSE-TZ2PROF-CRS  SECTION.                                          
050700                                                                          
050800     EXEC SQL                                                             
050900         CLOSE TZ2PROF-CRS                                                
051000     END-EXEC                                                             
051100     .                                                                    
051200                                                                          
051300 DB2-UPDATE-TZ2PROF-TAB  SECTION.                                         
051400                                                                          
051500     MOVE 000     TO GOOD-SQLCODECODES                                    
051600                                                                          
051700     EXEC SQL                                                             
051800         UPDATE TZ2PROF                                                   
051900         SET TEPROFDATA  = :TZ2PROF-TEPROFDATA                            
052000         WHERE CURRENT OF TZ2PROF-CRS                                     
052100     END-EXEC                                                             
052200                                                                          
052300     MOVE SQLCODE TO SQLCODE-WS                                           
052400     PERFORM DB2-STATUS-CHECK                                             
052500     .                                                                    
052600                                                                          
052700 DB2-INSERT-TZ2PROF-TAB  SECTION.                                         
052800                                                                          
052900     MOVE 000   TO GOOD-SQLCODECODES                                      
053000                                                                          
053100     EXEC SQL                                                             
053200         INSERT INTO TZ2PROF (                                            
053300          IDUSER,                                                         
053400          IDPROFGRP,                                                      
053500          IDLOPNR,                                                        
053600          TEPROFDATA                                                      
053700         )                                                                
053800         VALUES (                                                         
053900          :TZ2PROF-IDUSER,                                                
054000          :TZ2PROF-IDPROFGRP,                                             
054100          :TZ2PROF-IDLOPNR,                                               
054200          :TZ2PROF-TEPROFDATA                                             
054300         )                                                                
054400     END-EXEC                                                             
054500                                                                          
054600     MOVE SQLCODE TO SQLCODE-WS                                           
054700     PERFORM DB2-STATUS-CHECK                                             
054800     .                                                                    
054900                                                                          
055000 DB2-DELETE-TZ2PROF-TAB  SECTION.                                         
055100                                                                          
055200     MOVE 000   TO GOOD-SQLCODECODES                                      
055300                                                                          
055400     EXEC SQL                                                             
055500         DELETE FROM TZ2PROF                                              
055600         WHERE CURRENT OF TZ2PROF-CRS                                     
055700     END-EXEC                                                             
055800                                                                          
055900     MOVE SQLCODE TO SQLCODE-WS                                           
056000     PERFORM DB2-STATUS-CHECK                                             
056100     .                                                                    
056200                                                                          
056300 DB2-STATUS-CHECK  SECTION.                                               
056400                                                                          
056500     SET SQLCODE-IX TO 1                                                  
056600     SEARCH GOOD-SQLCODE                                                  
056700       AT END                                                             
056800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
056900          DELIMITED BY SIZE INTO ERROR-TEXT                               
057000          CALL ABEND USING RKOD-ABEND-DB2                                 
057100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
057200     END-SEARCH                                                           
057300     .                                                                    
