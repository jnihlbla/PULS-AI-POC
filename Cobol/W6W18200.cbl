000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W18200.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/07/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.REGPREPACKMATERIAL                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W161T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6016110 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W182T                                             
001800*        REQUEST:     W60182I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60182O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W18200'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  MAX-KVRADER                 PIC S9(4)   VALUE +10 COMP.              
004400                                                                          
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004900     03  W6018210                PIC X(8)    VALUE 'W6018210'.            
005000     SKIP3                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP3                                                                
005700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005800     SKIP3                                                                
005900*01  -COPY WZ01SUB                                                        
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006200     SKIP3                                                                
006300 01  REQU-AREA.                                                           
006400*    03  -COPY WZ01REQU                                                   
006500*    03  -COPY W60182I1                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006800     SKIP3                                                                
006900 01  RESP-AREA.                                                           
007000*    03  -COPY WZ01RESP                                                   
007100*    03  -COPY W60182O1                                                   
007200     EJECT                                                                
007300 LINKAGE SECTION.                                                         
007310*01  -COPY W0009  -PRE MSG-                                               
007320     EJECT                                                                
007400*01  -COPY W0008  -PRE WDK6-                                              
007500     05  FILLER                  PIC X.                                   
007600     EJECT                                                                
007700*01  -COPY W0008  -PRE KWDK6-                                             
007800     05  FILLER                  PIC X.                                   
007900     EJECT                                                                
008000*01  -COPY W0008  -PRE WDT3-                                              
008100     05  FILLER                  PIC X.                                   
009400     EJECT                                                                
009410*01  -COPY W0008  -PRE WDK7-                                              
009420     05  FILLER                  PIC X.                                   
009430     EJECT                                                                
009500                                                                          
009600 PROCEDURE DIVISION  USING MSG-PCB  WDK6-PCB KWDK6-PCB WDT3-PCB           
009700                           WDK7-PCB.                                      
009800                                                                          
009900 MAIN SECTION.                                                            
010000     ENTRY 'DLITCBL' USING MSG-PCB  WDK6-PCB KWDK6-PCB WDT3-PCB           
010200                           WDK7-PCB.                                      
010300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010400     IF SUB-KDRC = 0                                                      
010500       PERFORM A-INIT                                                     
010600       CALL W6018210 USING REQU-AREA RESP-AREA MAX-KVRADER                
010700                           WDK6-PCB KWDK6-PCB WDT3-PCB WDK7-PCB           
010900       PERFORM S02-RETURN-RESPONSE                                        
011000     END-IF                                                               
011100                                                                          
011200     PERFORM Z-FINIT                                                      
011300     MOVE ZERO TO RETURN-CODE                                             
011400     GOBACK                                                               
011500     .                                                                    
011600     EJECT                                                                
011700 A-INIT SECTION.                                                          
011800     CONTINUE                                                             
011900     .                                                                    
012000     EJECT                                                                
012100 Z-FINIT SECTION.                                                         
012200     CONTINUE                                                             
012300     .                                                                    
012400     EJECT                                                                
012500*    --- DISPATCHER SECTIONS                                              
012600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012700                                                                          
012800     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012900     MOVE 'CARPARTS.NDC.REGPREPACKMATERIAL'                               
012910                                   TO SUB-ADDISPABS                       
013000     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013100                                                                          
013200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013300                                                                          
013400     IF SUB-KDRC > 0                                                      
013500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013900     END-IF                                                               
014000     .                                                                    
014100     SKIP3                                                                
014200 S02-RETURN-RESPONSE SECTION.                                             
014300                                                                          
014400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014800                                                                          
014900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015000                                                                          
015100     IF SUB-KDRC > 0                                                      
015200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015600     END-IF                                                               
015700     .                                                                    
015800     EJECT                                                                
