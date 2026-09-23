000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W20700.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   JUNE 2012.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.INSPECTIONREPORTDESC                        
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W207T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6020710 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6020700                                          
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W207T/U                                           
002100*        REQUEST:     W60207I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60207O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W20700'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  MAX-KVRADER                 PIC S9(4)   VALUE +15 COMP.              
004500                                                                          
004600     EJECT                                                                
004700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005100     03  W6020710                PIC X(8)    VALUE 'W6020710'.            
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
006800*    03  -COPY W60207I1                                                   
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007100     SKIP3                                                                
007200 01  RESP-AREA.                                                           
007300*    03  -COPY WZ01RESP                                                   
007400*    03  -COPY W60207O1                                                   
007500     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800*01  -COPY W0009   -PRE MSG-                                              
007900 01  KVAE-PCB                    PIC X.                                   
008000     EJECT                                                                
008100 PROCEDURE DIVISION  USING MSG-PCB KVAE-PCB.                              
008200                                                                          
008300     ENTRY 'DLITCBL' USING MSG-PCB KVAE-PCB.                              
008400                                                                          
008500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
008600     IF SUB-KDRC = 0                                                      
008700       PERFORM A-INIT                                                     
008800                                                                          
008900       CALL W6020710 USING REQU-AREA RESP-AREA MAX-KVRADER                
009000                           KVAE-PCB                                       
009100       PERFORM S02-RETURN-RESPONSE                                        
009200     END-IF                                                               
009300                                                                          
009400     PERFORM Z-FINIT                                                      
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000     CONTINUE                                                             
010100     .                                                                    
010200     EJECT                                                                
010300 Z-FINIT SECTION.                                                         
010400     CONTINUE                                                             
010500     .                                                                    
010600     EJECT                                                                
010700*    --- DISPATCHER SECTIONS                                              
010800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
010900                                                                          
011000     MOVE 'GETARG'                 TO SUB-KDFUNC                          
011100     MOVE 'CARPARTS.NDC.INSPECTIONREPORTDESC'                             
011200                                   TO SUB-ADDISPABS                       
011300     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
011400                                                                          
011500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
011600                                                                          
011700     IF SUB-KDRC > 0                                                      
011800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
011900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
012200     END-IF                                                               
012300     .                                                                    
012400     SKIP3                                                                
012500 S02-RETURN-RESPONSE SECTION.                                             
012600                                                                          
012700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
012800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
012900                                                                          
013000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
013100                                                                          
013200     IF SUB-KDRC > 0                                                      
013300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
013500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013700     END-IF                                                               
013800     .                                                                    
013900     EJECT                                                                
