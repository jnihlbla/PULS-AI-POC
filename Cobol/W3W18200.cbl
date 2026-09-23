000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3W18200.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   12/12/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.CORECREATESHIPMENT                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W3W182T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3018210 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W3018200 (TRANSACTION W3T182)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W3W182T OR W3T182U                                  
002100*        REQUEST:     W30182I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W30182O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W3W18200'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004500                                                                          
004600     EJECT                                                                
004700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005100     03  W3018210                PIC X(8)    VALUE 'W3018210'.            
005200     SKIP3                                                                
005300*    --- PARAMETERS TO ABEND                                              
005400                                                                          
005500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005800     EJECT                                                                
005900*                                                                         
006000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006100     SKIP3                                                                
006200*01  -COPY WZ01SUB                                                        
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006500     SKIP3                                                                
006600 01  REQU-AREA.                                                           
006700*    03  -COPY WZ01REQU                                                   
006800*    03  -COPY W30182I1                                                   
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007100     SKIP3                                                                
007200 01  RESP-AREA.                                                           
007300*    03  -COPY WZ01RESP                                                   
007400*    03  -COPY W30182O1                                                   
007500     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700*01  -COPY W0009  -PRE MSG-                                               
007800*01  -COPY W0009  -PRE ALT-                                               
007900                                                                          
008000*01  -COPY W0008  -PRE USEA-                                              
008100     05  FILLER                  PIC X.                                   
008200                                                                          
008300*01  -COPY W0008  -PRE ARTC-                                              
008400     05  FILLER                  PIC X.                                   
008500                                                                          
008600*01  -COPY W0008  -PRE ARTS-                                              
008700     05  FILLER                  PIC X.                                   
008800                                                                          
008900*01  -COPY W0008  -PRE BENA-                                              
009000     05  FILLER                  PIC X.                                   
009100                                                                          
009200*01  -COPY W0008  -PRE 3171-                                              
009300     05  FILLER                  PIC X.                                   
009400                                                                          
009500*01  -COPY W0008  -PRE XXLD-                                              
009600     05  FILLER                  PIC X.                                   
010000                                                                          
010100*01  -COPY W0008  -PRE LOGA-                                              
010200     05  FILLER                  PIC X.                                   
010300*01  -COPY W0008  -PRE 3169-                                              
010400     05  FILLER                  PIC X.                                   
010500*01  -COPY W0008  -PRE WDB6-                                              
010600     05  FILLER                  PIC X.                                   
010900                                                                          
011000     EJECT                                                                
011100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
011200     ARTS-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
011300     3169-PCB WDB6-PCB.                                                   
011400                                                                          
011500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
011600     ARTS-PCB BENA-PCB 3171-PCB XXLD-PCB LOGA-PCB                         
011700     3169-PCB WDB6-PCB.                                                   
011800                                                                          
011900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012000     IF SUB-KDRC = 0                                                      
012100       PERFORM A-INIT                                                     
012200                                                                          
012300       CALL W3018210 USING REQU-AREA RESP-AREA MAX-KVRADER                
012400                                   ALT-PCB USEA-PCB ARTC-PCB              
012500                           ARTS-PCB BENA-PCB 3171-PCB                     
012600                           XXLD-PCB LOGA-PCB                              
012700                           3169-PCB WDB6-PCB                              
012800       PERFORM S02-RETURN-RESPONSE                                        
012900     END-IF                                                               
013000                                                                          
013100     PERFORM Z-FINIT                                                      
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     CONTINUE                                                             
013800     .                                                                    
013900     EJECT                                                                
014000 Z-FINIT SECTION.                                                         
014100     CONTINUE                                                             
014200     .                                                                    
014300     EJECT                                                                
014400*    --- DISPATCHER SECTIONS                                              
014500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
014600                                                                          
014700     MOVE 'GETARG'                 TO SUB-KDFUNC                          
014800     MOVE 'CARPARTS.NDC.CORECREATESHIPMENT' TO SUB-ADDISPABS              
014900     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
015000                                                                          
015100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
015200                                                                          
015300     IF SUB-KDRC > 0                                                      
015400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
015600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015800     END-IF                                                               
015900     .                                                                    
016000     SKIP3                                                                
016100 S02-RETURN-RESPONSE SECTION.                                             
016200                                                                          
016300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
016400*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
016500     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
016600           (500 - RESP-KVRADER) * LENGTH OF RESP-FAKTURA-RAD              
016700                                                                          
016800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
016900                                                                          
017000     IF SUB-KDRC > 0                                                      
017100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017500     END-IF                                                               
017600     .                                                                    
017700     EJECT                                                                
