000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W13200.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   12/02/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000701*    NAME:       CARPARTS.NDC.PRIORITYQUEUE                               
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W132T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6013210 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6013200 (TRANSACTION W6T132)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W132T/U                                           
002100*        REQUEST:     W60132I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60132O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W13200'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +1000 COMP.            
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005600     03  W6013210                PIC X(8)    VALUE 'W6013210'.            
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
007300*    03  -COPY W60132I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60132O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300*01  -COPY W0009   -PRE MSG-                                              
008500 01  ALT-PCB                  PIC X.                                      
008600 01  6197-PCB                 PIC X.                                      
009000 01  INLA1-PCB                PIC X.                                      
009100 01  INLA2-PCB                PIC X.                                      
009200 01  INLA3-PCB                PIC X.                                      
009300 01  INLC-PCB                 PIC X.                                      
009400 01  INLF-PCB                 PIC X.                                      
009500 01  INLH-PCB                 PIC X.                                      
009600 01  PLAA-PCB                 PIC X.                                      
009700 01  WDK6-PCB                 PIC X.                                      
009800 01  WDK7-PCB                 PIC X.                                      
009900 01  WDB6-PCB                 PIC X.                                      
009910 01  WDD3-PCB                 PIC X.                                      
010000**  PCB'ER FÖR SUBPGM WITHIN W6013210                                     
010100 01  PMRK-INLB-PCB            PIC X.                                      
010200 01  PMRK-INLC-PCB            PIC X.                                      
010300 01  PMRK-PLAA-PCB            PIC X.                                      
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 6197-PCB                       
012500                           INLA1-PCB INLA2-PCB INLA3-PCB                  
012600                           INLC-PCB INLF-PCB INLH-PCB                     
012700                           PLAA-PCB WDK6-PCB WDK7-PCB WDB6-PCB            
012800                           WDD3-PCB                                       
013000                           PMRK-INLB-PCB PMRK-INLC-PCB                    
013100                           PMRK-PLAA-PCB.                                 
013500                                                                          
013600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 6197-PCB                       
013700                           INLA1-PCB INLA2-PCB INLA3-PCB                  
013800                           INLC-PCB INLF-PCB INLH-PCB                     
013900                           PLAA-PCB WDK6-PCB WDK7-PCB WDB6-PCB            
013910                           WDD3-PCB                                       
014000                           PMRK-INLB-PCB PMRK-INLC-PCB                    
014100                           PMRK-PLAA-PCB.                                 
015100                                                                          
015200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015300     IF SUB-KDRC = 0                                                      
015400       PERFORM A-INIT                                                     
015500                                                                          
015600       CALL W6013210 USING REQU-AREA RESP-AREA MAX-KVRADER                
015700                           ALT-PCB 6197-PCB                               
015800                           INLA1-PCB INLA2-PCB INLA3-PCB                  
015900                           INLC-PCB  INLF-PCB  INLH-PCB PLAA-PCB          
016000                           WDK6-PCB  WDK7-PCB  WDB6-PCB WDD3-PCB          
016100                           PMRK-INLB-PCB PMRK-INLC-PCB                    
016200                           PMRK-PLAA-PCB                                  
016800       PERFORM S02-RETURN-RESPONSE                                        
016900     END-IF                                                               
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700     CONTINUE                                                             
017800     .                                                                    
017900     EJECT                                                                
018000 Z-FINIT SECTION.                                                         
018100     CONTINUE                                                             
018200     .                                                                    
018300     EJECT                                                                
018400*    --- DISPATCHER SECTIONS                                              
018500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
018600                                                                          
018700     MOVE 'GETARG'               TO SUB-KDFUNC                            
018801     MOVE 'CARPARTS.NDC.PRIORITYQUEUE' TO SUB-ADDISPABS                   
018900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
019000                                                                          
019100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019200                                                                          
019300     IF SUB-KDRC > 0                                                      
019400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
019500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
019600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019800     END-IF                                                               
019900     .                                                                    
020000     SKIP3                                                                
020100 S02-RETURN-RESPONSE SECTION.                                             
020200                                                                          
020300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
020400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
020500                                                                          
020600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
020700                                                                          
020800     IF SUB-KDRC > 0                                                      
020900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021300     END-IF                                                               
022000     .                                                                    
030000     EJECT                                                                
