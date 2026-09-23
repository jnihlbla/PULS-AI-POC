000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W14800.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.BINLOCALRECIEPT.Q.TRANSP.REMARKS            
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W148T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6014810 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6014800 (TRANSACTION W6T148)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W148T                                             
002100*        REQUEST:     W60148I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60148O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W14800'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +50 COMP.              
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005600     03  W6014810                PIC X(8)    VALUE 'W6014810'.            
005700     SKIP3                                                                
005800*    --- PARAMETERS TO ABEND                                              
005900                                                                          
006000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006300     EJECT                                                                
006400*                                                                         
006500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006600     SKIP3                                                                
006700*01  -COPY WZ01SUB                                                        
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007000     SKIP3                                                                
007100 01  REQU-AREA.                                                           
007200*    03  -COPY WZ01REQU                                                   
007300*    03  -COPY W60148I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60148O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300*01  -COPY W0009   -PRE MSG-                                              
008400     EJECT                                                                
008500                                                                          
008600*01  -COPY W0009   -PRE ALT1-                                             
008700     EJECT                                                                
008800                                                                          
008900*01  -COPY W0009   -PRE DISP-                                             
009000     EJECT                                                                
009100                                                                          
009200*01  -COPY W0008  -PRE USEA-                                              
009300     05  FILLER                  PIC X.                                   
009400                                                                          
009500*01  -COPY W0008  -PRE INLBSEQ-                                           
009600     05  FILLER                  PIC X.                                   
009700     EJECT                                                                
009800                                                                          
009900*01  -COPY W0008  -PRE INLCSEQ-                                           
010000     05  FILLER                  PIC X.                                   
010100     EJECT                                                                
010200                                                                          
010300*01  -COPY W0008  -PRE KOMA-                                              
010400     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010510*01  -COPY W0008  -PRE WDD3-                                              
010520     05  FILLER                  PIC X.                                   
010530     EJECT                                                                
       01  WDB6-PCB                    PIC X.                                   
010600                                                                          
010700 PROCEDURE DIVISION  USING MSG-PCB                                        
010800                           ALT1-PCB                                       
010900                           DISP-PCB                                       
011000                           USEA-PCB                                       
011100                           INLBSEQ-PCB                                    
011200                           INLCSEQ-PCB                                    
011300                           KOMA-PCB                                       
011310                           WDD3-PCB                                       
                                 WDB6-PCB.                                      
011400                                                                          
011500     ENTRY 'DLITCBL' USING MSG-PCB                                        
011600                           ALT1-PCB                                       
011700                           DISP-PCB                                       
011800                           USEA-PCB                                       
011900                           INLBSEQ-PCB                                    
012000                           INLCSEQ-PCB                                    
012100                           KOMA-PCB                                       
012110                           WDD3-PCB                                       
                                 WDB6-PCB.                                      
012200                                                                          
012300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012400     IF SUB-KDRC = 0                                                      
012500       PERFORM A-INIT                                                     
012600                                                                          
012700       CALL W6014810 USING REQU-AREA   RESP-AREA  MAX-KVRADER             
012800                           MSG-PCB                                        
012900                           ALT1-PCB                                       
013000                           DISP-PCB                                       
013100                           USEA-PCB                                       
013200                           INLBSEQ-PCB                                    
013300                           INLCSEQ-PCB                                    
013400                           KOMA-PCB                                       
013410                           WDD3-PCB                                       
                                 WDB6-PCB                                       
013500                                                                          
013600       PERFORM S02-RETURN-RESPONSE                                        
013700     END-IF                                                               
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     CONTINUE                                                             
014600     .                                                                    
014700     EJECT                                                                
014800 Z-FINIT SECTION.                                                         
014900     CONTINUE                                                             
015000     .                                                                    
015100     EJECT                                                                
015200*    --- DISPATCHER SECTIONS                                              
015300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
015400                                                                          
015500     MOVE 'GETARG'               TO SUB-KDFUNC                            
015600     MOVE 'CARPARTS.NDC.BINLOCALRECIEPT.Q.TRANSP.REMARKS'                 
015700                                 TO SUB-ADDISPABS                         
015800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
015900                                                                          
016000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
016100                                                                          
016200     IF SUB-KDRC > 0                                                      
016300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
016500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016700     END-IF                                                               
016800     .                                                                    
016900     SKIP3                                                                
017000 S02-RETURN-RESPONSE SECTION.                                             
017100                                                                          
017200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
017300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
017400*    COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
017500*         (50 - RESP-KVRADER) * LENGTH OF RESP-RAD                        
017600                                                                          
017700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
017800                                                                          
017900     IF SUB-KDRC > 0                                                      
018000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
018100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
018200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
018300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018400     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
