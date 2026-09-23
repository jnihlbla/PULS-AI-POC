000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W0052000.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   2016-11-03                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.LOGDATA.SAVE                                            
001000*    FUNCTION:                                                            
001100*      - RECEIVES LINES FROM WEB                                          
001200*      - READS ALL LINES VIA WZ01SUB                                      
001700*                                                                         
001900*        THE PROGRAM INSERTS         ROWS IN TABLE TP0WLOG                
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W0T520U                                             
002400*        REQUEST:     HEADER ONLY                                         
002500*                                                                         
003000*        SÄNDNING VIA WZ01  TILL MAIL                                     
003100*        MOD:         WZ01MAIL (VIA WZ01)                                 
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W0052000'.            
003900                                                                          
004000*    --- WORK FIELD FOR ERROR MESSAGES CALLING ABEND                      
004100 77  ERRORTEXT                   PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +0001.            
005110                                                                          
005200     EJECT                                                                
005300                                                                          
005400*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005900     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
006000                                                                          
006100*    --- PARAMETERS TO ABEND                                              
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     EJECT                                                                
006800                                                                          
006900*    --- AREAS FOR COMMUNICATION                                          
007000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007100                                                                          
007200 01  -COPY WZ01SUB                                                        
007300     EJECT                                                                
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007600 01  REQU-AREA.                                                           
007700*    03  -COPY WZ01REQU -PRE IN-                                          
007800*    03  -COPY W00520I1 -PRE MID-W00520I1-                                
007900     EJECT                                                                
008000                                                                          
008010 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008020     SKIP3                                                                
008030 01  RESP-AREA.                                                           
008040     03  -COPY WZ01RESP                                                   
008050                                                                          
008100 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
008200 01  -COPY WZ01SEND                                                       
008300     EJECT                                                                
008400                                                                          
008910 01  HDR-AREA.                                                            
008920*    03  -COPY WZ01REQU -PRE WARN-                                        
008930*    03  -COPY WZ04HDR                                                    
008940     EJECT                                                                
008950                                                                          
008960 01  DAP-LINE-AREA.                                                       
008970     03  DAP-LINE-TEXT           PIC X(15)   VALUE SPACE.                 
008971     03  DAP-LINE-VALUE          PIC X(50)   VALUE SPACE.                 
008980     EJECT                                                                
009002                                                                          
009010 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009400 01  DB2-WS.                                                              
009500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009600         88  INSERT-OK                       VALUE 000.                   
009700         88  CURSOR-OK                       VALUE 000.                   
009800         88  LINES-FOUND                     VALUE 000.                   
009900         88  LINES-MISSING                   VALUE 100.                   
009910         88  LINES-DUPLICATE                 VALUE 803.                   
010000         88  RESOURCE-WRONG                  VALUE 904.                   
010100                                                                          
010200     03  GOOD-SQLCODECODES.                                               
010300         05  GOOD-SQLCODE OCCURS 5                                        
010400             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010600                                                                          
010700 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010800 01  WS-AREA.                                                             
010900     03 WS-IDDC                PIC X(2)  VALUE SPACE.                     
011000     03 WS-TIREGDAT            PIC S9(7) VALUE ZERO COMP-3.               
011100     03 WS-TIREGTID            PIC S9(7) VALUE ZERO COMP-3.               
011300     03 WS-IDUSER              PIC X(8)  VALUE SPACE.                     
011400     03 WS-KVMILSEC            PIC S9(7) VALUE ZERO COMP-3.               
011500     03 WS-BEWEBSCR            PIC X(50) VALUE SPACE.                     
011600     03 WS-BEWEBURL            PIC X(50) VALUE SPACE.                     
011700     03 WS-IDLOPNR             PIC S9(3) VALUE ZERO COMP-3.               
018900                                                                          
019000 01  FILLER                    PIC X(16)    VALUE 'TP0WLOG-AREA'.         
019100*01  -COPY TP0WLOG -PRE TP0WLOG-                                          
019200     EJECT                                                                
019300                                                                          
020200     EXEC SQL INCLUDE TP0WLOG END-EXEC.                                   
020300     EJECT                                                                
021000                                                                          
021100 LINKAGE SECTION.                                                         
021200                                                                          
021300 PROCEDURE DIVISION.                                                      
021400 MAIN SECTION.                                                            
021500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021600                                                                          
021900     IF SUB-KDRC = ZERO                                                   
022000       PERFORM A-INIT                                                     
022600       PERFORM B-LINES                                                    
022700       PERFORM C-INSERT-TP0WLOG                                           
022800       PERFORM D-WARNING-LONG-TIME                                        
022900       PERFORM S02-RETURN-RESPONSE                                        
024600     END-IF                                                               
024700                                                                          
024900     MOVE ZERO TO RETURN-CODE                                             
025000     GOBACK                                                               
025100     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400 A-INIT SECTION.                                                          
025500     CALL W009WAIT USING WS-TIME-WAIT                                     
025900                                                                          
026000     MOVE SPACE TO RESP-IDMSG-ERROR                                       
026100     MOVE SPACE TO RESP-IDMSG-INFO                                        
026200     MOVE SPACE TO RESP-IDELMT-ERROR                                      
026300                                                                          
026500     INITIALIZE GOOD-SQLCODECODES                                         
026600     .                                                                    
026700     EJECT                                                                
026800                                                                          
026900 B-LINES SECTION.                                                         
027000     MOVE MID-W00520I1-IDDC            TO WS-IDDC                         
027100     MOVE MID-W00520I1-TIREGDAT        TO WS-TIREGDAT                     
027200     MOVE MID-W00520I1-TIREGTID        TO WS-TIREGTID                     
027300     MOVE MID-W00520I1-IDUSER          TO WS-IDUSER                       
027400     MOVE MID-W00520I1-KVMILSEC        TO WS-KVMILSEC                     
027500     MOVE MID-W00520I1-BEWEBSCR        TO WS-BEWEBSCR                     
027600     MOVE MID-W00520I1-BEWEBURL        TO WS-BEWEBURL                     
027700     MOVE 1                            TO WS-IDLOPNR                      
033300     .                                                                    
033400     EJECT                                                                
033500                                                                          
033600 C-INSERT-TP0WLOG SECTION.                                                
033800     PERFORM DB2-INSERT-TP0WLOG                                           
033810     IF LINES-DUPLICATE                                                   
033811       PERFORM UNTIL INSERT-OK OR (WS-IDLOPNR = 100)                      
033820         ADD  1                        TO WS-IDLOPNR                      
033830         PERFORM DB2-INSERT-TP0WLOG                                       
033831       END-PERFORM                                                        
033840     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200 D-WARNING-LONG-TIME SECTION.                                             
034210     IF WS-KVMILSEC > 5000                                                
034211       PERFORM S90-SEND-OPEN                                              
034212       MOVE 001                      TO WARN-REQU-IDMSGVER                
034213       MOVE 'R'                      TO WARN-REQU-KDPGMACT                
034214       MOVE IDPGM                    TO WARN-REQU-IDUSER                  
034215                                                                          
034216       MOVE 'WEBLONGTIME'            TO HDR-IDOUTTYPE                     
034217       MOVE SPACE                    TO HDR-IDOUTREC                      
034218                                                                          
034219       MOVE MID-W00520I1-IDDC         TO HDR-IDOUTREC                     
034220       MOVE MID-W00520I1-TIREGTID     TO HDR-IDLIST                       
034221       PERFORM S90-PUT-HEADER                                             
034222                                                                          
034223*****UPDATES   LINE WARNING                                               
034224       MOVE 'DC:           '      TO DAP-LINE-TEXT                        
034225       MOVE MID-W00520I1-IDDC     TO DAP-LINE-VALUE                       
034226       PERFORM S90-PUT-LINE                                               
034228       MOVE 'SCREEN:       '      TO DAP-LINE-TEXT                        
034229       MOVE MID-W00520I1-BEWEBSCR TO DAP-LINE-VALUE                       
034230       PERFORM S90-PUT-LINE                                               
034231       MOVE 'USER:         '      TO DAP-LINE-TEXT                        
034240       MOVE MID-W00520I1-IDUSER   TO DAP-LINE-VALUE                       
034241       PERFORM S90-PUT-LINE                                               
034242       MOVE 'RESPONSE TIME: '     TO DAP-LINE-TEXT                        
034243       MOVE MID-W00520I1-KVMILSEC TO DAP-LINE-VALUE                       
034244       PERFORM S90-PUT-LINE                                               
034245       MOVE 'DATE:          '     TO DAP-LINE-TEXT                        
034246       MOVE MID-W00520I1-TIREGDAT TO DAP-LINE-VALUE                       
034247       PERFORM S90-PUT-LINE                                               
034248       MOVE 'TIME:          '     TO DAP-LINE-TEXT                        
034249       MOVE MID-W00520I1-TIREGTID TO DAP-LINE-VALUE                       
034250       PERFORM S90-PUT-LINE                                               
034251       MOVE 'URL:           '     TO DAP-LINE-TEXT                        
034252       MOVE MID-W00520I1-BEWEBURL TO DAP-LINE-VALUE                       
034253       PERFORM S90-PUT-LINE                                               
034260                                                                          
034273*******SENDING   ERRORTEXT TO DAP                                         
034280       PERFORM S90-SEND-CLOSE                                             
034290     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500                                                                          
035700*    --- DISPATCHER-SECTIONS                                              
035800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
035900     MOVE 'GETARG'                   TO SUB-KDFUNC                        
036000     MOVE 'CARPARTS.LOGDATA.SAVE'    TO SUB-ADDISPABS                     
036010     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
036100     CALL WZ01SUB  USING SUB-CONTROL-AREA                                 
036200                         SUB-KVDLEN                                       
036210                         REQU-AREA                                        
036300     IF SUB-KDRC > 0                                                      
036400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
036500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
036600       DELIMITED BY SIZE INTO ERRORTEXT                                   
036700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036800     END-IF                                                               
036900     .                                                                    
036901                                                                          
036910 S02-RETURN-RESPONSE SECTION.                                             
036920     MOVE 'RETURN'                   TO SUB-KDFUNC                        
036930     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
036940                                                                          
036950     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
036960                                                                          
036970     IF SUB-KDRC > 0                                                      
036980       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
036990       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
036991       DELIMITED BY SIZE INTO ERRORTEXT                                   
036992       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036993     END-IF                                                               
036994     .                                                                    
036995     EJECT                                                                
039700                                                                          
039800 S90-SEND-OPEN SECTION.                                                   
039900     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
040000     MOVE 'OPEN'                          TO SEND-KDFUNC                  
040100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
040200                         SEND-OPEN-AREA                                   
040300     IF SEND-KDRC > ZERO                                                  
040400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
040500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
040600       DELIMITED BY SIZE INTO ERRORTEXT                                   
040700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200 S90-PUT-HEADER SECTION.                                                  
041300     MOVE 'PUT'                           TO SEND-KDFUNC                  
041400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
041500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041600                         SEND-KVDLEN                                      
041700                         HDR-AREA                                         
041800     IF SEND-KDRC > ZERO                                                  
041900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
042000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
042100       DELIMITED BY SIZE INTO ERRORTEXT                                   
042200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600                                                                          
042700 S90-PUT-LINE SECTION.                                                    
042800     MOVE 'PUT'                           TO SEND-KDFUNC                  
042900     MOVE LENGTH OF DAP-LINE-AREA         TO SEND-KVDLEN                  
043000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043100                         SEND-KVDLEN                                      
043200                         DAP-LINE-AREA                                    
043300     IF SEND-KDRC > ZERO                                                  
043400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
043500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
043600       DELIMITED BY SIZE INTO ERRORTEXT                                   
043700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043710     END-IF                                                               
043720     .                                                                    
043730     EJECT                                                                
043740                                                                          
043831 S90-SEND-CLOSE SECTION.                                                  
043832     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
043833     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043834     .                                                                    
043835     EJECT                                                                
043836                                                                          
043840 DB2-INSERT-TP0WLOG SECTION.                                              
043900     SKIP2                                                                
044000     MOVE 000803 TO GOOD-SQLCODECODES                                     
044100     EXEC SQL                                                             
044200         INSERT INTO TP0WLOG                                              
044300         (IDDC                                                            
044400         ,TIREGDAT                                                        
044500         ,TIREGTID                                                        
044600         ,IDUSER                                                          
044700         ,KVMILSEC                                                        
044800         ,BEWEBSCR                                                        
044900         ,BEWEBURL                                                        
045000         ,IDLOPNR)                                                        
051000         VALUES(:WS-IDDC                                                  
051100               ,:WS-TIREGDAT                                              
051200               ,:WS-TIREGTID                                              
051300               ,:WS-IDUSER                                                
051400               ,:WS-KVMILSEC                                              
051500               ,:WS-BEWEBSCR                                              
051600               ,:WS-BEWEBURL                                              
051700               ,:WS-IDLOPNR)                                              
057700     END-EXEC                                                             
057800                                                                          
057900     MOVE SQLCODE TO SQLCODE-WS                                           
058000     PERFORM DB2-STATUS-CONTROL                                           
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
063800 DB2-STATUS-CONTROL SECTION.                                              
063900     SET SQLCODE-IX TO 1                                                  
064000     SEARCH GOOD-SQLCODE                                                  
064100       AT END                                                             
064200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
064300          DELIMITED BY SIZE INTO ERRORTEXT                                
064400          CALL ABEND USING RKOD-ABEND-DB2                                 
064500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
064600     END-SEARCH                                                           
064700     .                                                                    
