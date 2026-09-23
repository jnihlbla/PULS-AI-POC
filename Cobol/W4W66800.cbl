000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W66800.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   14/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.TRANSPORTINFO                               
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W668T                     
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W4066810 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W4W668T                                             
001800*        REQUEST:     W40668I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W40668O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W4W66800'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
003910 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005310     03  W4066810                PIC X(8)    VALUE 'W4066810'.            
005400     SKIP3                                                                
005500*    --- PARAMETERS TO ABEND                                              
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     SKIP3                                                                
006400*                                                                         
006500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006600     SKIP3                                                                
006700*01  -COPY WZ01SUB                                                        
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007000     SKIP3                                                                
007100 01  REQU-AREA.                                                           
007200*    03  -COPY WZ01REQU                                                   
007300*    03  -COPY W40668I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W40668O1                                                   
008000     EJECT                                                                
008100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
008200*                                                                         
010400 LINKAGE SECTION.                                                         
010500*01  -COPY W0009   -PRE MSG-                                              
010600     EJECT                                                                
010700*01  -COPY W0009   -PRE ALT-USA-                                          
010710     EJECT                                                                
010711*01  -COPY W0009   -PRE ALT-JAP-                                          
010712     EJECT                                                                
010713*01  -COPY W0009   -PRE ALT-AUS-                                          
010714     EJECT                                                                
010718*01  -COPY W0008   -PRE 4463-                                             
010719     05  FILLER                  PIC X.                                   
010720     EJECT                                                                
010722*01  -COPY W0008  -PRE WDB6-                                              
010723     05  FILLER                  PIC X.                                   
010724     EJECT                                                                
010730 PROCEDURE DIVISION  USING MSG-PCB ALT-USA-PCB ALT-JAP-PCB                
010800                           ALT-AUS-PCB 4463-PCB WDB6-PCB.                 
010900 MAIN SECTION.                                                            
011000     ENTRY 'DLITCBL' USING MSG-PCB ALT-USA-PCB ALT-JAP-PCB                
011100                           ALT-AUS-PCB 4463-PCB WDB6-PCB.                 
011200                                                                          
011300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011400     IF SUB-KDRC = 0                                                      
011500       PERFORM A-INIT                                                     
011600       CALL W4066810 USING REQU-AREA RESP-AREA MAX-KVRADER                
011700                           ALT-USA-PCB ALT-JAP-PCB                        
011800                           ALT-AUS-PCB 4463-PCB WDB6-PCB                  
011900       PERFORM S02-RETURN-RESPONSE                                        
012000     END-IF                                                               
012100                                                                          
012200     PERFORM Z-FINIT                                                      
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700 A-INIT SECTION.                                                          
012800     CONTINUE                                                             
012900     .                                                                    
013000     EJECT                                                                
013100 Z-FINIT SECTION.                                                         
013200     CONTINUE                                                             
013300     .                                                                    
013400     EJECT                                                                
013500*    --- DISPATCHER SECTIONS                                              
013600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013700                                                                          
013800     MOVE 'GETARG'               TO SUB-KDFUNC                            
013900     MOVE 'CARPARTS.NDC.TRANSPORTINFO'  TO SUB-ADDISPABS                  
014000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
014100                                                                          
014200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
014300                                                                          
014400     IF SUB-KDRC > 0                                                      
014500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014900     END-IF                                                               
015000     .                                                                    
015100     SKIP3                                                                
015200 S02-RETURN-RESPONSE SECTION.                                             
015300                                                                          
015400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
015500*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015600                                                                          
015610     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
015620           (500 - RESP-KVRADER) * LENGTH OF RESP-RADER                    
015630                                                                          
015700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015800                                                                          
015900     IF SUB-KDRC > 0                                                      
016000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
016200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016400     END-IF                                                               
016500     .                                                                    
016600     EJECT                                                                
