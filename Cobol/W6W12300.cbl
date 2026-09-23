000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W12300.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   JUL 2012.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.CASESURVEY.QUERY                            
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W123T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6012310 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W123T                                             
001800*        REQUEST:     W60123I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60123O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W12300'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004400                                                                          
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004900     03  W6012310                PIC X(8)    VALUE 'W6012310'.            
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
006500*    03  -COPY W60123I1                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006800     SKIP3                                                                
006900 01  RESP-AREA.                                                           
007000*    03  -COPY WZ01RESP                                                   
007100*    03  -COPY W60123O1                                                   
007200     EJECT                                                                
007300 LINKAGE SECTION.                                                         
007400*01  -COPY W0009  -PRE MSG-                                               
007500     EJECT                                                                
007600 01  ALT1-PCB                    PIC X.                                   
007700 01  ALT2-PCB                    PIC X.                                   
007800 01  ALT3-PCB                    PIC X.                                   
007900 01  USEA-PCB                    PIC X.                                   
008000 01  INLA-PCB                    PIC X.                                   
008100 01  INLB1-PCB                   PIC X.                                   
008200 01  INLB-PCB                    PIC X.                                   
008300 01  INLH1-PCB                   PIC X.                                   
008400 01  LOPA-PCB                    PIC X.                                   
008500 01  PLAA-PCB                    PIC X.                                   
008600 01  INLB-PMRK-PCB               PIC X.                                   
008700 01  INLC-PMRK-PCB               PIC X.                                   
008800 01  WDB6-PCB                    PIC X.                                   
008900 01  BENA-PCB                    PIC X.                                   
009000     EJECT                                                                
009100                                                                          
009200 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
009300                           USEA-PCB INLA-PCB INLB1-PCB INLB-PCB           
009400                           INLH1-PCB LOPA-PCB PLAA-PCB                    
009500                           INLB-PMRK-PCB INLC-PMRK-PCB                    
009600                           WDB6-PCB BENA-PCB.                             
009700                                                                          
009800 MAIN SECTION.                                                            
009900     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
010000                           USEA-PCB INLA-PCB INLB1-PCB INLB-PCB           
010100                           INLH1-PCB LOPA-PCB PLAA-PCB                    
010200                           INLB-PMRK-PCB INLC-PMRK-PCB                    
010300                           WDB6-PCB BENA-PCB.                             
010400                                                                          
010500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010600     IF SUB-KDRC = 0                                                      
010700       PERFORM A-INIT                                                     
010800       CALL W6012310 USING REQU-AREA RESP-AREA MAX-KVRADER                
010900                           ALT1-PCB ALT2-PCB ALT3-PCB USEA-PCB            
011000                           INLA-PCB INLB1-PCB INLB-PCB INLH1-PCB          
011100                           LOPA-PCB PLAA-PCB INLB-PMRK-PCB                
011200                           INLC-PMRK-PCB WDB6-PCB BENA-PCB                
011300       PERFORM S02-RETURN-RESPONSE                                        
011400     END-IF                                                               
011500                                                                          
011600     PERFORM Z-FINIT                                                      
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012200     CONTINUE                                                             
012300     .                                                                    
012400     EJECT                                                                
012500 Z-FINIT SECTION.                                                         
012600     CONTINUE                                                             
012700     .                                                                    
012800     EJECT                                                                
012900*    --- DISPATCHER SECTIONS                                              
013000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013100                                                                          
013200     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013300     MOVE 'CARPARTS.NDC.CASESURVEY.QUERY'                                 
013400                                   TO SUB-ADDISPABS                       
013500     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013600                                                                          
013700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013800                                                                          
013900     IF SUB-KDRC > 0                                                      
014000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014400     END-IF                                                               
014500     .                                                                    
014600     SKIP3                                                                
014700 S02-RETURN-RESPONSE SECTION.                                             
014800                                                                          
014900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
015000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015100     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
015200           (500 - RESP-KVRADER) * LENGTH OF RESP-RAD                      
015300                                                                          
015400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015500                                                                          
015600     IF SUB-KDRC > 0                                                      
015700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016100     END-IF                                                               
016200     .                                                                    
016300     EJECT                                                                
