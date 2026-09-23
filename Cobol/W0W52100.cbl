000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0W52100.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   17/01/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       WEB RESPONSE TIME LOG                                    
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W0W521T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W0052110 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W0052100 (TRANSACTION W0T521)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W0W521T                                             
002100*        REQUEST:     W00521I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W00521O1                                            
002500*                                                                         
002600*    CHANGE LOG:                                                          
002700*      17/01/03 - SARASWATHY S    - INITIAL VERSION.                      
002800*                                   WEB RESPONSE TIME LOG.                
002900*                                   ETRACKER 10291333.                    
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W0W52100'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
005100                                                                          
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W0052110                PIC X(8)    VALUE 'W0052110'.            
005800     SKIP3                                                                
005900*    --- PARAMETERS TO ABEND                                              
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006400     EJECT                                                                
006500*                                                                         
006600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006700     SKIP3                                                                
006800*01  -COPY WZ01SUB                                                        
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007100     SKIP3                                                                
007200 01  REQU-AREA.                                                           
007300*    03  -COPY WZ01REQU                                                   
007400*    03  -COPY W00521I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W00521O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300                                                                          
008400*01  -COPY W0009   -PRE MSG-                                              
008500     05  FILLER                  PIC X.                                   
009300     EJECT                                                                
009500 PROCEDURE DIVISION  USING MSG-PCB.                                       
009600*                                                                         
009800     ENTRY 'DLITCBL' USING MSG-PCB.                                       
009900                                                                          
010000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010100     IF SUB-KDRC = 0                                                      
010200       PERFORM A-INIT                                                     
010300                                                                          
010400       CALL W0052110 USING REQU-AREA RESP-AREA MAX-KVRADER                
010500                                                                          
010700       PERFORM S02-RETURN-RESPONSE                                        
010800     END-IF                                                               
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600     CONTINUE                                                             
011700     .                                                                    
011800     EJECT                                                                
011900 Z-FINIT SECTION.                                                         
012000     CONTINUE                                                             
012100     .                                                                    
012200     EJECT                                                                
012300*    --- DISPATCHER SECTIONS                                              
012400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012500                                                                          
012600     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012700     MOVE 'CARPARTS.LOGDATA.GETLOGDATA' TO SUB-ADDISPABS                  
012800     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
012900                                                                          
013000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013100                                                                          
013200     IF SUB-KDRC > 0                                                      
013300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013700     END-IF                                                               
013800     .                                                                    
013900     SKIP3                                                                
014000 S02-RETURN-RESPONSE SECTION.                                             
014100                                                                          
014200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014300*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014400     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
014500           (500 - RESP-KVRADER) * LENGTH OF RESP-WEBRESPTIME              
014600                                                                          
014700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014800                                                                          
014900     IF SUB-KDRC > 0                                                      
015000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015400     END-IF                                                               
015500     .                                                                    
015600     EJECT                                                                
