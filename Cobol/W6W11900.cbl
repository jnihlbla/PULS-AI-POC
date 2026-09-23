000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W11900.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   2012/02/08.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.BACKOUTINBOUND                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W119U                     
001100*        (CURRENTLY NO QUERY TRANSACTION - W6W119T - EXISTS, SINCE        
001200*        QUERIES ARE NOT TRIGGERED FROM THE WEB ENVIRONMENT)              
001300*                                                                         
001400*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001500*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011910 WHICH              
001600*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001700*                                                                         
001800*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001900*        MINAL EXISTS - W6011900 (TRANSACTION W6T119 AND W6T110U)         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W6W119T                                             
002300*        REQUEST:     W60119I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    W60119O1                                            
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
004000 77  IDPGM                       PIC X(08)   VALUE 'W6W11900'.            
004100                                                                          
004200*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W6011910                PIC X(8)    VALUE 'W6011910'.            
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
007400*    03  -COPY W60119I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W60119O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300*01  -COPY W0009  -PRE MSG-                                               
008400     05  FILLER             PIC X.                                        
008500 01  INLA-PCB               PIC X.                                        
008600 01  INLC-PCB               PIC X.                                        
008700 01  ARTC-PCB               PIC X.                                        
008800 01  INLB-PCB               PIC X.                                        
008900 01  INLE-PCB               PIC X.                                        
009000 01  ZZAC-PCB               PIC X.                                        
009100 01  LASA-PCB               PIC X.                                        
009200 01  KVAE-PCB               PIC X.                                        
009300 01  ARTS-PCB               PIC X.                                        
009400 01  INLC-INL-PCB           PIC X.                                        
009500 01  LOGA-PCB               PIC X.                                        
009600 01  FILB-PCB               PIC X.                                        
009700 01  SAPA-PCB               PIC X.                                        
009800 01  WDG2-PCB               PIC X.                                        
009810 01  9305-AVG-PCB           PIC X.                                        
009820 01  AVG-WDB6-PCB           PIC X.                                        
009900 01  WDB6-LEV-PCB           PIC X.                                        
010000 01  WDB6-PCB               PIC X.                                        
010010 01  WDK7-PCB               PIC X.                                        
010030 01  WDF1-PCB               PIC X.                                        
010040 01  9305-PCB               PIC X.                                        
010100     EJECT                                                                
010200 PROCEDURE DIVISION  USING                                                
010300                     MSG-PCB           INLC-PCB                           
010400                     INLA-PCB ARTC-PCB INLB-PCB                           
010500                     INLE-PCB ZZAC-PCB                                    
010600                     LASA-PCB KVAE-PCB ARTS-PCB                           
010700                     INLC-INL-PCB LOGA-PCB                                
010800                     FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB              
010900                     AVG-WDB6-PCB WDB6-LEV-PCB WDB6-PCB WDK7-PCB          
010910                     WDF1-PCB 9305-PCB.                                   
011000     ENTRY 'DLITCBL' USING                                                
011100                     MSG-PCB           INLC-PCB                           
011200                     INLA-PCB ARTC-PCB INLB-PCB                           
011300                     INLE-PCB ZZAC-PCB                                    
011400                     LASA-PCB KVAE-PCB ARTS-PCB                           
011500                     INLC-INL-PCB LOGA-PCB                                
011600                     FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB              
011700                     AVG-WDB6-PCB WDB6-LEV-PCB WDB6-PCB WDK7-PCB          
011710                     WDF1-PCB 9305-PCB.                                   
011800                                                                          
011900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012000     IF SUB-KDRC = 0                                                      
012100       PERFORM A-INIT                                                     
012200                                                                          
012300       CALL W6011910 USING                                                
012400            REQU-AREA RESP-AREA                                           
012500            MSG-PCB           INLC-PCB                                    
012600            INLA-PCB ARTC-PCB INLB-PCB                                    
012700            INLE-PCB ZZAC-PCB                                             
012800            LASA-PCB KVAE-PCB ARTS-PCB                                    
012900            INLC-INL-PCB LOGA-PCB                                         
013000            FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB AVG-WDB6-PCB          
013100            WDB6-LEV-PCB WDB6-PCB WDK7-PCB                                
013110            WDF1-PCB 9305-PCB                                             
013200                                                                          
013300       PERFORM S02-RETURN-RESPONSE                                        
013400     END-IF                                                               
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     CONTINUE                                                             
014300     .                                                                    
014400     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014600     CONTINUE                                                             
014700     .                                                                    
014800     EJECT                                                                
014900*    --- DISPATCHER SECTIONS                                              
015000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
015100                                                                          
015200     MOVE 'GETARG'               TO SUB-KDFUNC                            
015300     MOVE 'CARPARTS.NDC.BACKOUTINBOUND'     TO SUB-ADDISPABS              
015400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
015500                                                                          
015600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
015700                                                                          
015800     IF SUB-KDRC > 0                                                      
015900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
016100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016300     END-IF                                                               
016400     .                                                                    
016500     SKIP3                                                                
016600 S02-RETURN-RESPONSE SECTION.                                             
016700                                                                          
016800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
016900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
017000                                                                          
017100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
017200                                                                          
017300     IF SUB-KDRC > 0                                                      
017400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017800     END-IF                                                               
017900     .                                                                    
018000     EJECT                                                                
