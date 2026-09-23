000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W11800.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.BACKOUTINBOUND                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W118T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011810 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6011800 (TRANSACTION W6T114)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W118T                                             
002100*        REQUEST:     W60118I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60118O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W11800'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +50  COMP.             
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100     EJECT                                                                
005200 01  RESPONSE-CODES.                                                      
005300     03  WERR-INVALID-UPDATE     PIC X(3)    VALUE '007'.                 
005400                                                                          
005500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005900     03  W6011810                PIC X(8)    VALUE 'W6011810'.            
006000     SKIP3                                                                
006100*    --- PARAMETERS TO ABEND                                              
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600     EJECT                                                                
006700*                                                                         
006800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006900     SKIP3                                                                
007000*01  -COPY WZ01SUB                                                        
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007300     SKIP3                                                                
007400 01  REQU-AREA.                                                           
007500*    03  -COPY WZ01REQU                                                   
007600*    03  -COPY W60118I1                                                   
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007900     SKIP3                                                                
008000 01  RESP-AREA.                                                           
008100*    03  -COPY WZ01RESP                                                   
008200*    03  -COPY W60118O1                                                   
008300     EJECT                                                                
008400 LINKAGE SECTION.                                                         
008500                                                                          
008600*01  -COPY W0009   -PRE MSG-                                              
008700     EJECT                                                                
008800*01  -COPY W0009   -PRE 6202-                                             
008900     EJECT                                                                
009000*01  -COPY W0008  -PRE INLA-                                              
009100     05  FILLER                  PIC X.                                   
009200     EJECT                                                                
009300*01  -COPY W0008  -PRE SEQB-                                              
009400     05  FILLER                  PIC X.                                   
009500     EJECT                                                                
009600*01  -COPY W0008  -PRE PLAA-                                              
009700     05  FILLER                  PIC X.                                   
009800     EJECT                                                                
009900*01  -COPY W0008  -PRE ARTC-                                              
010000     05  FILLER                  PIC X.                                   
010100     EJECT                                                                
010200*01  -COPY W0008  -PRE INLE-                                              
010300     05  FILLER                  PIC X.                                   
010400     EJECT                                                                
010500*01  -COPY W0008  -PRE KVAE-                                              
010600     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800*01  -COPY W0008  -PRE WDK7-                                              
010900     05  FILLER                  PIC X.                                   
011000*01  -COPY W0008  -PRE INLC-                                              
011100     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011300*01  -COPY W0008  -PRE LOGA-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600*01  -COPY W0008  -PRE UPFA-                                              
011700     05  FILLER                  PIC X.                                   
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE WDD3-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012310 01  9305-AVG-PCB                PIC X.                                   
012320     EJECT                                                                
012330 01  AVG-WDB6-PCB                PIC X.                                   
012340     EJECT                                                                
012400*01  -COPY W0008  -PRE WDB6-                                              
012500     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012700*01  -COPY W0008 -PRE LEV-                                                
012800     05  FILLER                  PIC X(5).                                
012900     EJECT                                                                
013300*01  -COPY W0008  -PRE 9305-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB 6202-PCB          INLA-PCB             
013700                           SEQB-PCB PLAA-PCB                              
013800                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
013900                           INLC-PCB LOGA-PCB UPFA-PCB                     
014000                           WDD3-PCB 9305-AVG-PCB                          
014010                           AVG-WDB6-PCB WDB6-PCB                          
014100                           LEV-PCB 9305-PCB.                              
014200 MAIN SECTION.                                                            
014300     ENTRY 'DLITCBL' USING MSG-PCB 6202-PCB          INLA-PCB             
014400                           SEQB-PCB PLAA-PCB                              
014500                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
014600                           INLC-PCB LOGA-PCB UPFA-PCB                     
014700                           WDD3-PCB 9305-AVG-PCB                          
014710                           AVG-WDB6-PCB WDB6-PCB                          
014800                           LEV-PCB 9305-PCB.                              
014900                                                                          
015000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015100     IF SUB-KDRC = 0                                                      
015200       PERFORM A-INIT                                                     
015300                                                                          
015400       CALL W6011810 USING REQU-AREA   RESP-AREA  MAX-KVRADER             
015500                           MSG-PCB 6202-PCB          INLA-PCB             
015600                           SEQB-PCB PLAA-PCB                              
015700                           ARTC-PCB INLE-PCB KVAE-PCB WDK7-PCB            
015800                           INLC-PCB LOGA-PCB UPFA-PCB                     
015900                           WDD3-PCB 9305-AVG-PCB                          
015910                           AVG-WDB6-PCB WDB6-PCB                          
016000                           LEV-PCB 9305-PCB                               
016100                                                                          
016200** TO HANDLE ERROR MESSAGE ON W6011CA, WEB SCREEN                         
016300       IF RESP-IDMSG-ERROR = WERR-INVALID-UPDATE                          
016400         MOVE SPACES TO RESP-IDMSG-ERROR                                  
016500       END-IF                                                             
016600                                                                          
016700       PERFORM S02-RETURN-RESPONSE                                        
016800     END-IF                                                               
016900                                                                          
017000     PERFORM Z-FINIT                                                      
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600     CONTINUE                                                             
017700     .                                                                    
017800     EJECT                                                                
017900 Z-FINIT SECTION.                                                         
018000     CONTINUE                                                             
018100     .                                                                    
018200     EJECT                                                                
018300*    --- DISPATCHER SECTIONS                                              
018400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
018500                                                                          
018600     MOVE 'GETARG'               TO SUB-KDFUNC                            
018700     MOVE 'CARPARTS.NDC.BACKOUTINBOUND'     TO SUB-ADDISPABS              
018800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
018900                                                                          
019000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019100                                                                          
019200     IF SUB-KDRC > 0                                                      
019300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
019400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
019500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019700     END-IF                                                               
019800     .                                                                    
019900     SKIP3                                                                
020000 S02-RETURN-RESPONSE SECTION.                                             
020100                                                                          
020200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
020300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
020400                                                                          
020500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
020600                                                                          
020700     IF SUB-KDRC > 0                                                      
020800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
