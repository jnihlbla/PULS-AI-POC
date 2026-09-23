000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W13500.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   JUN 2012.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.DELETECASE                                  
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W135T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6013510 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W135T                                             
001800*        REQUEST:     W60135I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60135O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W13500'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  MAX-KVRADER                 PIC S9(4)   VALUE +14 COMP.              
004400                                                                          
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004900     03  W6013510                PIC X(8)    VALUE 'W6013510'.            
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
006500*    03  -COPY W60135I1                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006800     SKIP3                                                                
006900 01  RESP-AREA.                                                           
007000*    03  -COPY WZ01RESP                                                   
007100*    03  -COPY W60135O1                                                   
007200     EJECT                                                                
007300 LINKAGE SECTION.                                                         
007400*01  -COPY W0009  -PRE MSG-                                               
007500     EJECT                                                                
007600 01  6191-PCB                    PIC X.                                   
007700 01  INLA-PCB                    PIC X.                                   
007800 01  INLA-CSEQ-PCB               PIC X.                                   
007900 01  INLD-PCB                    PIC X.                                   
008000 01  WDB6-PCB                    PIC X.                                   
008100 01  PMRK-INLB-PCB               PIC X.                                   
008200 01  PMRK-INLC-PCB               PIC X.                                   
008300 01  PMRK-PLAA-PCB               PIC X.                                   
008900     EJECT                                                                
009500                                                                          
009600 PROCEDURE DIVISION  USING MSG-PCB 6191-PCB INLA-PCB                      
009700                           INLA-CSEQ-PCB INLD-PCB WDB6-PCB                
009800                           PMRK-INLB-PCB PMRK-INLC-PCB                    
009900                           PMRK-PLAA-PCB.                                 
010000                                                                          
010100 MAIN SECTION.                                                            
010200     ENTRY 'DLITCBL' USING MSG-PCB 6191-PCB INLA-PCB                      
010300                           INLA-CSEQ-PCB INLD-PCB WDB6-PCB                
010400                           PMRK-INLB-PCB PMRK-INLC-PCB                    
010500                           PMRK-PLAA-PCB.                                 
010600                                                                          
010700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010800     IF SUB-KDRC = 0                                                      
010900       PERFORM A-INIT                                                     
011000       CALL W6013510 USING REQU-AREA RESP-AREA MAX-KVRADER                
011100                           6191-PCB INLA-PCB                              
011200                           INLA-CSEQ-PCB INLD-PCB WDB6-PCB                
011300                           PMRK-INLB-PCB PMRK-INLC-PCB                    
011400                           PMRK-PLAA-PCB                                  
011700       PERFORM S02-RETURN-RESPONSE                                        
011800     END-IF                                                               
011900                                                                          
012000     PERFORM Z-FINIT                                                      
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-INIT SECTION.                                                          
012600     CONTINUE                                                             
012700     .                                                                    
012800     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013000     CONTINUE                                                             
013100     .                                                                    
013200     EJECT                                                                
013300*    --- DISPATCHER SECTIONS                                              
013400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013500                                                                          
013600     MOVE 'GETARG'                  TO SUB-KDFUNC                         
013700     MOVE 'CARPARTS.NDC.DELETECASE' TO SUB-ADDISPABS                      
013800     MOVE LENGTH OF REQU-AREA       TO SUB-KVDLEN                         
013900                                                                          
014000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
014100                                                                          
014200     IF SUB-KDRC > 0                                                      
014300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014700     END-IF                                                               
014800     .                                                                    
014900     SKIP3                                                                
015000 S02-RETURN-RESPONSE SECTION.                                             
015100                                                                          
015200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
015210     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015300*    COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
015400*       (MAX-KVRADER - RESP-KVRADER) * LENGTH OF RESP-RAD                 
015500                                                                          
015600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015700                                                                          
015800     IF SUB-KDRC > 0                                                      
015900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
016100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
