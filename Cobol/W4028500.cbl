000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4028500.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/08/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.STARTORDERUPLOAD                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        UPLOAD OF EXCEL (.CSV) ORDER FILE FROM SPX AND OTHERS.           
001100*        THIS PROGRAM DOES NOT PROCESS THE FILE ITSELF,                   
001200*        IT JUST ORDERS ROUTINE W412S2 TO DO THE PROCESSING.              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W40285U                                             
001600*        REQUEST:     W40285I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    (JUST WZ01RESP)                                     
002000                                                                          
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4028500'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900 77  KDRC-DISPLAY                PIC Z(5).                                
003000                                                                          
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  W-ORDERFILE-DATE            PIC X(8).                                
003500 77  W-ORDERFILE-TIME            PIC X(6).                                
003600 77  W-ORDERFILE                 PIC X(40).                               
003700                                                                          
003800     EJECT                                                                
003900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004000 01  GENERAL-SUBPROGRAMS.                                                 
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
004400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
004500     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
004600     SKIP3                                                                
004700*    --- PARAMETERS TO ABEND                                              
004800                                                                          
004900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005200     SKIP3                                                                
005300 01  MESSAGE-CODES.                                                       
005400     03  ERR-IS-INVALID          PIC X(3)    VALUE '023'.                 
005500     03  INFO-PROCESSING-STARTED PIC X(3)    VALUE '101'.                 
005600     EJECT                                                                
005700*                                                                         
005800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005900     SKIP3                                                                
006000*01  -COPY WZ01SUB                                                        
006100     EJECT                                                                
006200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006300     SKIP3                                                                
006400 01  REQU-AREA.                                                           
006500*    03  -COPY WZ01REQU                                                   
006600*    03  -COPY W40285I1                                                   
006700     EJECT                                                                
006800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006900     SKIP3                                                                
007000 01  RESP-AREA.                                                           
007100*    03  -COPY WZ01RESP                                                   
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
007400     SKIP3                                                                
007500*01  -COPY WZ01SEND                                                       
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
007800     SKIP3                                                                
007900 01  SEND-AREA.                                                           
008000*    03  -COPY WZ01SOP -PRE SOP-                                          
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'EMAD-AREA'.           
008300     SKIP3                                                                
008400*01  -COPY W009EMAD                                                       
008500                                                                          
008600     EJECT                                                                
008700 PROCEDURE DIVISION.                                                      
008800 MAIN SECTION.                                                            
008900     ENTRY 'DLITCBL'.                                                     
009000                                                                          
009100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009200     IF SUB-KDRC = 0                                                      
009300       PERFORM A-INIT                                                     
009400       PERFORM B-CHECK-INPUT                                              
009500                                                                          
009600       IF RESP-IDMSG-ERROR = SPACE                                        
009700*        -- TRIGGER ACTIVATION OF RTN W412S2 TO SOP                       
009800         PERFORM C-SETUP-SOP-DATA                                         
009900         PERFORM S04-SEND-OPEN                                            
010000         PERFORM S04-SEND-MESSAGE                                         
010100         PERFORM S04-SEND-CLOSE                                           
010200         MOVE INFO-PROCESSING-STARTED TO RESP-IDMSG-INFO                  
010300       END-IF                                                             
010400                                                                          
010500       PERFORM S02-RETURN-RESPONSE                                        
010600     END-IF                                                               
010700                                                                          
010800     MOVE ZERO TO RETURN-CODE                                             
010900     GOBACK                                                               
011000     .                                                                    
011100     EJECT                                                                
011200 A-INIT SECTION.                                                          
011300                                                                          
011400     MOVE SPACE TO RESP-WZ01RESP                                          
011500     MOVE '001' TO RESP-IDMSGVER                                          
011600     .                                                                    
011700     EJECT                                                                
011800 B-CHECK-INPUT SECTION.                                                   
011900                                                                          
012000     MOVE REQU-IDMAIL TO EMAD-IDMAIL                                      
012100     CALL W009EMAD USING EMAD-W009EMAD                                    
012200     IF EMAD-KDSVAR = 'F'                                                 
012300       MOVE ERR-IS-INVALID      TO RESP-IDMSG-ERROR                       
012400       MOVE 'IDMAIL' TO RESP-IDELMT-ERROR                                 
012500     END-IF                                                               
012600                                                                          
012700     IF REQU-IDPATH-ORDERFILE = SPACE OR '+'                              
012800       MOVE ERR-IS-INVALID      TO RESP-IDMSG-ERROR                       
012900       MOVE 'IDPATH' TO RESP-IDELMT-ERROR                                 
013000     END-IF                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 C-SETUP-SOP-DATA SECTION.                                                
013400                                                                          
013500     MOVE ZERO TO TALLY                                                   
013600     INSPECT REQU-IDPATH-ORDERFILE                                        
013700       TALLYING TALLY FOR CHARACTERS                                      
013800       BEFORE INITIAL 'w40285-'                                           
013900     MOVE REQU-IDPATH-ORDERFILE(TALLY + 8:8)  TO W-ORDERFILE-DATE         
014000     MOVE REQU-IDPATH-ORDERFILE(TALLY + 17:6) TO W-ORDERFILE-TIME         
014100     MOVE REQU-IDPATH-ORDERFILE(TALLY + 1:)   TO W-ORDERFILE              
014200                                                                          
014300     MOVE 'W412S2' TO SOP-REQU-IDPROCESS                                  
014400     MOVE 'O'      TO SOP-REQU-KDSOPFUNK                                  
014500     MOVE ZERO  TO SOP-REQU-TIORDDAT                                      
014510     MOVE SPACE TO SOP-REQU-TESYMBV                                       
014600     STRING ' MAILID('        DELIMITED BY SIZE                           
014700        REQU-IDMAIL           DELIMITED BY SPACE                          
014800        ') OPATH('            DELIMITED BY SIZE                           
014900        REQU-IDPATH-ORDERFILE DELIMITED BY SPACE                          
015000        ') OFILE('            DELIMITED BY SIZE                           
015100        W-ORDERFILE           DELIMITED BY SPACE                          
015200        ') ODATE('            DELIMITED BY SIZE                           
015300        W-ORDERFILE-DATE      DELIMITED BY SIZE                           
015400        ') OTIME('            DELIMITED BY SIZE                           
015500        W-ORDERFILE-TIME      DELIMITED BY SIZE                           
015510        ') OIDUSER('          DELIMITED BY SIZE                           
015520        REQU-IDUSER           DELIMITED BY SIZE                           
015600        ') '                  DELIMITED BY SIZE                           
015700        INTO SOP-REQU-TESYMBV                                             
015800     .                                                                    
015900     EJECT                                                                
016000*    --- DISPATCHER SECTIONS                                              
016100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
016200                                                                          
016300     MOVE 'GETARG'                         TO SUB-KDFUNC                  
016400     MOVE 'CARPARTS.PULS.STARTORDERUPLOAD' TO SUB-ADDISPABS               
016500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
016600                                                                          
016700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
016800                                                                          
016900     IF SUB-KDRC > 0                                                      
017000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
017200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017400     END-IF                                                               
017500     .                                                                    
017600     SKIP3                                                                
017700 S02-RETURN-RESPONSE SECTION.                                             
017800                                                                          
017900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
018000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
018100                                                                          
018200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
018300                                                                          
018400     IF SUB-KDRC > 0                                                      
018500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
018600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
018700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
018800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018900     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 S04-SEND-OPEN SECTION.                                                   
019300                                                                          
019400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
019500     MOVE 'CARPARTS.PULS.SOP'        TO SEND-ADDISPABS                    
019600     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
019700                                                                          
019800     IF SEND-KDRC > 0                                                     
019900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
020000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
020100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020300     END-IF                                                               
020400     .                                                                    
020500     SKIP3                                                                
020600 S04-SEND-MESSAGE SECTION.                                                
020700                                                                          
020800     MOVE 'PUT'                      TO SEND-KDFUNC                       
020900     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
021100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
021200                                                                          
021300     IF SEND-KDRC > 0                                                     
021400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
021500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
021600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021800     END-IF                                                               
021900     .                                                                    
022000     SKIP3                                                                
022100 S04-SEND-CLOSE SECTION.                                                  
022200                                                                          
022300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
022400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
022500                                                                          
022600     IF SEND-KDRC > 0                                                     
022700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
022800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
022900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
023000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
023100     END-IF                                                               
023200     .                                                                    
