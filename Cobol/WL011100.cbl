000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL011100.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/10/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.PRLABEL'                                   
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM                               
001100*        HANTERAR UTSKRIFT AV FLAGGOR.                                    
001200*                                                                         
001300*        WL011100 PROGRAM IS A REPLICA OF W6019400 PROGRAM                
001400*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001500*                                                                         
001600*    INDATA.                                                              
001700*        REQUEST:     WL0111I1                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        RESPONSE:    - (ONLY WZ01RESP)                                   
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
003400 77  IDPGM                       PIC X(08)   VALUE 'WL011100'.            
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003800 77  KDRC-DISPLAY                PIC Z(5).                                
003900                                                                          
004000     EJECT                                                                
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004800                                                                          
004900 01  FILLER                      PIC X(8)    VALUE 'IX-*****'.            
005000 01  IX-INDEXVARIABLER.                                                   
005100*     -- POST INOM MID                                                    
005200   03  IX-POST                   PIC S9(9)   VALUE ZERO COMP SYNC.        
005300*     -- MAX ANTAL POSTER I MID                                           
005400   03    MAX-IX-POST             PIC S9(9)   VALUE +15  COMP SYNC.        
005500                                                                          
005600     EJECT                                                                
005700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005800 01  GENERAL-SUBPROGRAMS.                                                 
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006300     SKIP3                                                                
006400*    --- PARAMETERS TO ABEND                                              
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006900     SKIP3                                                                
007000*                                                                         
007100 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
007200*   -COPY WZ01SEND                                                        
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)  VALUE 'WZ01SUB '.             
007500*   -COPY WZ01SUB                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
007800                                                                          
007900 01  REQU-AREA.                                                           
008000*    03 -COPY WZ01REQU                                                    
008100*    03 -COPY WL0111I1                                                    
008200                                                                          
008300 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
008400                                                                          
008500 01  RESP-AREA.                                                           
008600*    03 -COPY WZ01RESP                                                    
008700                                                                          
008800 01  FILLER                      PIC X(16)  VALUE 'HDR-AREA'.             
008900                                                                          
009000 01  HDR-AREA.                                                            
009100*    03 -COPY WZ01REQU -PRE HDR-                                          
009200*    03 -COPY WZ04HDR                                                     
009300                                                                          
009400 01  FILLER                 PIC X(16)  VALUE 'REP-AREA-LINE'.             
009500 01  REP-AREA-LINE.                                                       
009600*    03 -COPY WL01111                                                     
009700                                                                          
009800     EJECT                                                                
009900 01   SYS-ERROR             PIC X(3)   VALUE '099'.                       
010000     EJECT                                                                
010100 LINKAGE SECTION.                                                         
010200 01  MSG-PCB                     PIC X.                                   
010300                                                                          
010400 01  DISTRDOC-PCB                PIC X.                                   
010500     EJECT                                                                
010600 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
010700 MAIN SECTION.                                                            
010800     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
010900                                                                          
011000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     IF REQU-KDPGMACT = 'E'                                               
011400     AND REQU-KVRADER  NUMERIC                                            
011500     AND REQU-KVRADER > 0                                                 
011600                                                                          
011700       IF WZ04-SEND-IDCOM = ZERO                                          
011800         PERFORM S02-SEND-OPEN                                            
011900         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
012000       END-IF                                                             
012100       PERFORM S02-PUT-HEADER                                             
012200                                                                          
012300       MOVE +1                   TO IX-POST                               
012400       PERFORM UNTIL (IX-POST > MAX-IX-POST                               
012500                  OR  IX-POST > REQU-KVRADER)                             
012600                                                                          
012700         PERFORM B-RED-SKR-SIDA                                           
012800         PERFORM S02-PUT-REPORT-LINE                                      
012900         ADD +1                  TO IX-POST                               
013000       END-PERFORM                                                        
013100                                                                          
013200       PERFORM S05-SEND-CLOSE                                             
013300                                                                          
013400     ELSE                                                                 
013500       IF REQU-KVRADER = 0                                                
013600         MOVE 'KVRADER' TO RESP-IDELMT-ERROR                              
013700         MOVE '126'     TO RESP-IDMSG-ERROR                               
013800       ELSE                                                               
013900         IF REQU-KVRADER NOT NUMERIC                                      
014000           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
014100           MOVE '024'     TO RESP-IDMSG-ERROR                             
014200         ELSE                                                             
014300*          -- KDPGMACT NOT = E                                            
014400           MOVE 'KDPGMACT' TO RESP-IDELMT-ERROR                           
014500           MOVE SYS-ERROR  TO RESP-IDMSG-ERROR                            
014600         END-IF                                                           
014700       END-IF                                                             
014800     END-IF                                                               
014900                                                                          
015000     IF RESP-IDMSG-ERROR NOT = SPACE                                      
015100       PERFORM S02-RETURN-RESPONSE                                        
015200     END-IF                                                               
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900                                                                          
016000     MOVE ALL '+'                    TO RESP-AREA                         
016100     MOVE SPACE                      TO RESP-IDMSG-ERROR                  
016200                                        RESP-IDMSG-INFO                   
016300                                        RESP-IDELMT-ERROR                 
016400     MOVE 001                        TO RESP-IDMSGVER                     
016500                                                                          
016600     MOVE 001                        TO HDR-REQU-IDMSGVER                 
016700     MOVE 'CASE-LABEL-INB'           TO HDR-IDOUTTYPE                     
016800     MOVE SPACE                      TO HDR-IDOUTREC                      
016900     MOVE REQU-IDDC-KEY              TO HDR-IDOUTREC(1:2)                 
017000     MOVE REQU-IDUSER                TO HDR-IDOUTREC(3:8)                 
017100     ACCEPT HDR-IDLIST(1:6)      FROM   DATE                              
017200     ACCEPT HDR-IDLIST(7:4)      FROM   TIME                              
017300     .                                                                    
017400                                                                          
017500     EJECT                                                                
017600 B-RED-SKR-SIDA SECTION.                                                  
017700                                                                          
017800     MOVE '1'                          TO REP-IDAFPRCD                    
017900     MOVE REQU-IDARTNR  (IX-POST)      TO REP-IDARTNR                     
018000     MOVE REQU-KVINLART-LINE (IX-POST) TO REP-KVINLART                    
018100     MOVE REQU-KDSORT   (IX-POST)      TO REP-KDSORT                      
018200     MOVE REQU-TIINLMOT-LINE (IX-POST) TO REP-TIINLMOT                    
018300     MOVE REQU-VKKOLLIN (IX-POST)      TO REP-VKKOLLIN                    
018400     MOVE REQU-IDLOPNRM (IX-POST)      TO REP-IDLOPNRM                    
018500     MOVE REQU-ADLAGOMR (IX-POST)      TO REP-ADLAGOMR                    
018600     MOVE REQU-ADGANG   (IX-POST)      TO REP-ADGANG                      
018700     MOVE REQU-ADPLATS  (IX-POST)      TO REP-ADPLATS                     
018710     MOVE REQU-IDLEVNR-KOLLI (IX-POST) TO REP-IDLEVNR-KOLLI               
018720     MOVE REQU-IDOKOLLI (IX-POST)      TO REP-IDOKOLLI                    
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100*    --- DISPATCHER SECTIONS                                              
019200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019300                                                                          
019400     MOVE 'GETARG'               TO SUB-KDFUNC                            
019500     MOVE 'CARPARTS.LDC.PRLABEL' TO SUB-ADDISPABS                         
019600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
019700                                                                          
019800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019900                                                                          
020000     IF SUB-KDRC > 0                                                      
020100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020500     END-IF                                                               
020600     .                                                                    
020700     SKIP3                                                                
020800 S02-RETURN-RESPONSE SECTION.                                             
020900                                                                          
021000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021200                                                                          
021300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021400                                                                          
021500     IF SUB-KDRC > 0                                                      
021600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022000     END-IF                                                               
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400* DISPATCHER-SEKTIONER                                                    
022500 S02-SEND-OPEN SECTION.                                                   
022600                                                                          
022700     MOVE 'CARPARTS.DAP.DISTRDOCWEB'      TO SEND-ADDISPABS               
022800     MOVE 'OPEN'                          TO SEND-KDFUNC                  
022900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
023000                         SEND-OPEN-AREA                                   
023100     IF SEND-KDRC > ZERO                                                  
023200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
023300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
023400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
023500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
023600     END-IF                                                               
023700     .                                                                    
023800     SKIP3                                                                
023900 S02-PUT-HEADER SECTION.                                                  
024000                                                                          
024100     MOVE 'PUT'                           TO SEND-KDFUNC                  
024200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
024300     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
024400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
024500                         SEND-KVDLEN                                      
024600                         HDR-AREA                                         
024700     IF SEND-KDRC > ZERO                                                  
024800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
024900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
025000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 S02-PUT-REPORT-LINE    SECTION.                                          
025600                                                                          
025700     MOVE 'PUT'                           TO SEND-KDFUNC                  
025800     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
025900     MOVE LENGTH OF REP-AREA-LINE         TO SEND-KVDLEN                  
026000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
026100                         SEND-KVDLEN                                      
026200                         REP-AREA-LINE                                    
026300     IF SEND-KDRC > ZERO                                                  
026400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
026500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
026600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026800     END-IF                                                               
026900     .                                                                    
027000     SKIP3                                                                
027100 S05-SEND-CLOSE SECTION.                                                  
027200                                                                          
027300     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
027400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
027500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
027600     .                                                                    
