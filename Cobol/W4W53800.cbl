000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W53800.                                                
000300 AUTHOR.         LENA BROMANDER                                           
000400 DATE-WRITTEN.   2020-04-16.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.CUSTOMSINFO                                 
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W538T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W4053810 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W4053800 (TRANSACTION W4T538)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4W538T/U                                           
002100*        REQUEST:     W40538I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W40538O1                                            
002500*                                                                         
002600*                                                                         
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
004000 77  IDPGM                       PIC X(08)   VALUE 'W4W53800'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004700                                                                          
004800     EJECT                                                                
004900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005300     03  W4053810                PIC X(8)    VALUE 'W4053810'.            
005400     SKIP3                                                                
005500*    --- PARAMETERS TO ABEND                                              
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     EJECT                                                                
006100*                                                                         
006200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006300     SKIP3                                                                
006400*01  -COPY WZ01SUB                                                        
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006700     SKIP3                                                                
006800 01  REQU-AREA.                                                           
006900*    03  -COPY WZ01REQU                                                   
007000*    03  -COPY W40538I1                                                   
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007300     SKIP3                                                                
007400 01  RESP-AREA.                                                           
007500*    03  -COPY WZ01RESP                                                   
007600*    03  -COPY W40538O1                                                   
007700     EJECT                                                                
007800 LINKAGE SECTION.                                                         
007900                                                                          
008000*01  -COPY W0009   -PRE MSG-                                              
008100*01  -COPY W0009   -PRE ALT-                                              
008200 01  WDM7-PCB                    PIC X.                                   
008300 01  WDM7A-PCB                   PIC X.                                   
008400 01  WDM8-PCB                    PIC X.                                   
008500 01  4587-PCB                    PIC X.                                   
008600 01  WDE6-PCB                    PIC X.                                   
008700                                                                          
008800                                                                          
008900     EJECT                                                                
009000 PROCEDURE DIVISION  USING MSG-PCB                                        
009100                          ALT-PCB   WDM7-PCB                              
009200                          WDM7A-PCB WDM8-PCB                              
009300                          4587-PCB  WDE6-PCB.                             
009400                                                                          
009500 MAIN SECTION.                                                            
009600     ENTRY 'DLITCBL' USING MSG-PCB                                        
009700                          ALT-PCB   WDM7-PCB                              
009800                          WDM7A-PCB WDM8-PCB                              
009900                          4587-PCB  WDE6-PCB.                             
010000                                                                          
010100                                                                          
010200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010300     IF SUB-KDRC = 0                                                      
010400       PERFORM A-INIT                                                     
010500                                                                          
010600       CALL W4053810 USING REQU-AREA RESP-AREA MAX-KVRADER                
010700                          ALT-PCB   WDM7-PCB                              
010800                          WDM7A-PCB WDM8-PCB                              
010900                          4587-PCB  WDE6-PCB                              
011000                                                                          
011100       PERFORM S02-RETURN-RESPONSE                                        
011200     END-IF                                                               
011300                                                                          
011400     PERFORM Z-FINIT                                                      
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012000     IF REQU-QUERY                                                        
012010       MOVE 'T'  TO REQU-KDPGMACT                                         
012020     END-IF                                                               
012200     .                                                                    
012300     EJECT                                                                
012400 Z-FINIT SECTION.                                                         
012500     CONTINUE                                                             
012600     .                                                                    
012700     EJECT                                                                
012800*    --- DISPATCHER SECTIONS                                              
012900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013000                                                                          
013100     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013200     MOVE 'CARPARTS.NDC.CUSTOMSINFO'                                      
013300                                   TO SUB-ADDISPABS                       
013400     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013500                                                                          
013600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013700                                                                          
013800     IF SUB-KDRC > 0                                                      
013900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014300     END-IF                                                               
014400     .                                                                    
014500     SKIP3                                                                
014600 S02-RETURN-RESPONSE SECTION.                                             
014700                                                                          
014800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014900*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015000     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
015100           (500 - RESP-KVRADER) * LENGTH OF RESP-RADER                    
015200                                                                          
015300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015400                                                                          
015500     IF SUB-KDRC > 0                                                      
015600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016000     END-IF                                                               
016100     .                                                                    
016200     EJECT                                                                
