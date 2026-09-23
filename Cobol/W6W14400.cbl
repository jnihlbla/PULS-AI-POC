000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W14400.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.BINLOCALRECIEPT                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W144T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6014410 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6014400 (TRANSACTION W6T144)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W144T                                             
002100*        REQUEST:     W60144I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60144O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W14400'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  MAX-KVRADER                 PIC S9(4)   VALUE +50  COMP.             
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W6014410                PIC X(8)    VALUE 'W6014410'.            
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
007400*    03  -COPY W60144I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W60144O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300                                                                          
008400*01  -COPY W0009   -PRE MSG-                                              
008500 01  ALT1-PCB                  PIC X.                                     
008600 01  DISP-PCB                 PIC X.                                      
008700 01  INLA1-PCB                PIC X.                                      
008800 01  INLA2-PCB                PIC X.                                      
008900 01  INLC-PCB                 PIC X.                                      
009000 01  PLAA-PCB                 PIC X.                                      
009100 01  ARTD-PCB                 PIC X.                                      
009200 01  ARTS-PCB                 PIC X.                                      
009300 01  UPFA-PCB                 PIC X.                                      
009400 01  ARTC-PCB                 PIC X.                                      
009500 01  WDB6-PCB                 PIC X.                                      
009600                                                                          
009700**  PCB'ER FÖR SUBPGM WITHIN W6014410                                     
009800 01  PMRK-INLB-PCB               PIC X.                                   
009900 01  PMRK-INLC-PCB               PIC X.                                   
010000 01  PMRK-PLAA-PCB               PIC X.                                   
010100 01  STYR-HANA-PCB               PIC X.                                   
010200 01  STYR-PLAA-PCB               PIC X.                                   
010300 01  KOM-KOMA-PCB                PIC X.                                   
010400                                                                          
010500     EJECT                                                                
010600 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB                      
010700                           INLA1-PCB                                      
010800                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
010900                           ARTS-PCB                                       
011000                           UPFA-PCB ARTC-PCB WDB6-PCB                     
011100                           PMRK-INLB-PCB PMRK-INLC-PCB                    
011200                           PMRK-PLAA-PCB                                  
011300                           STYR-HANA-PCB                                  
011400                           STYR-PLAA-PCB                                  
011500                           KOM-KOMA-PCB.                                  
011600                                                                          
011700     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB                      
011800                           INLA1-PCB                                      
011900                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
012000                           ARTS-PCB                                       
012100                           UPFA-PCB ARTC-PCB WDB6-PCB                     
012200                           PMRK-INLB-PCB PMRK-INLC-PCB                    
012300                           PMRK-PLAA-PCB                                  
012400                           STYR-HANA-PCB                                  
012500                           STYR-PLAA-PCB                                  
012600                           KOM-KOMA-PCB.                                  
012700                                                                          
012800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012900     IF SUB-KDRC = 0                                                      
013000       PERFORM A-INIT                                                     
013100                                                                          
013200       CALL W6014410 USING REQU-AREA   RESP-AREA MAX-KVRADER              
013300                           MSG-PCB ALT1-PCB DISP-PCB                      
013400                           INLA1-PCB                                      
013500                           INLA2-PCB INLC-PCB PLAA-PCB ARTD-PCB           
013600                           ARTS-PCB                                       
013700                           UPFA-PCB ARTC-PCB WDB6-PCB                     
013800                           PMRK-INLB-PCB PMRK-INLC-PCB                    
013900                           PMRK-PLAA-PCB                                  
014000                           STYR-HANA-PCB                                  
014100                           STYR-PLAA-PCB                                  
014200                           KOM-KOMA-PCB                                   
014300                                                                          
014400       PERFORM S02-RETURN-RESPONSE                                        
014500     END-IF                                                               
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015300     CONTINUE                                                             
015400     .                                                                    
015500     EJECT                                                                
015600 Z-FINIT SECTION.                                                         
015700     CONTINUE                                                             
015800     .                                                                    
015900     EJECT                                                                
016000*    --- DISPATCHER SECTIONS                                              
016100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
016200                                                                          
016300     MOVE 'GETARG'               TO SUB-KDFUNC                            
016400     MOVE 'CARPARTS.NDC.BINLOCALRECIEPT' TO SUB-ADDISPABS                 
016500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
016600                                                                          
016700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
016800                                                                          
016900     IF SUB-KDRC > 0                                                      
017000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
017200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017400     END-IF                                                               
017500     .                                                                    
017600     SKIP3                                                                
017700 S02-RETURN-RESPONSE SECTION.                                             
017800                                                                          
017900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
018000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
018100                                                                          
018200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
018300                                                                          
018400     IF SUB-KDRC > 0                                                      
018500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
018600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
018700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
018800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018900     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
