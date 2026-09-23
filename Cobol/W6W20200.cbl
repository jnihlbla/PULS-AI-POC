000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W20200.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   12/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.INSPECTIONREPORT                            
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W202T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6020210 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6020200 (TRANSACTION W6T202)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W202T                                             
002100*        REQUEST:     W60202I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60202O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W20200'.            
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
005100     03  W6020210                PIC X(8)    VALUE 'W6020210'.            
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
006800*    03  -COPY W60202I1                                                   
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007100     SKIP3                                                                
007200 01  RESP-AREA.                                                           
007300*    03  -COPY WZ01RESP                                                   
007400*    03  -COPY W60202O1                                                   
007500     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800*01  -COPY W0009   -PRE MSG-                                              
007900 01  KVAE-PCB                    PIC X.                                   
008000 01  KVAI-PCB                    PIC X.                                   
008100 01  INLC-PCB                    PIC X.                                   
008200 01  BENA-PCB                    PIC X.                                   
008300 01  LEVA-PCB                    PIC X.                                   
008400 01  ARTC-PCB                    PIC X.                                   
008500 01  XXLA-PCB                    PIC X.                                   
008600 01  WDP3-PCB                    PIC X.                                   
008700 01  LOPB-PCB                    PIC X.                                   
008800 01  INLE-PCB                    PIC X.                                   
008900 01  W6F1-PCB                    PIC X.                                   
009000 01  W6INLA-PCB                  PIC X.                                   
009100 01  UPFA-PCB                    PIC X.                                   
009200 01  WDB6-PCB                    PIC X.                                   
009210 01  WDK7-PCB                    PIC X.                                   
009220 01  WDL6-PCB                    PIC X.                                   
009300     EJECT                                                                
009400 PROCEDURE DIVISION  USING MSG-PCB  KVAE-PCB KVAI-PCB                     
009500                           INLC-PCB BENA-PCB LEVA-PCB                     
009600                           ARTC-PCB XXLA-PCB WDP3-PCB                     
009700                           LOPB-PCB INLE-PCB W6F1-PCB                     
009800                           W6INLA-PCB UPFA-PCB WDB6-PCB WDK7-PCB          
009900                           WDL6-PCB.                                      
010000     ENTRY 'DLITCBL' USING MSG-PCB  KVAE-PCB KVAI-PCB                     
010100                           INLC-PCB BENA-PCB LEVA-PCB                     
010200                           ARTC-PCB XXLA-PCB WDP3-PCB                     
010300                           LOPB-PCB INLE-PCB W6F1-PCB                     
010400                           W6INLA-PCB UPFA-PCB WDB6-PCB WDK7-PCB          
010410                           WDL6-PCB.                                      
010500                                                                          
010600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010700     IF SUB-KDRC = 0                                                      
010800       PERFORM A-INIT                                                     
010900                                                                          
011000       CALL W6020210 USING REQU-AREA RESP-AREA                            
011100                           KVAE-PCB KVAI-PCB INLC-PCB BENA-PCB            
011200                           LEVA-PCB ARTC-PCB XXLA-PCB WDP3-PCB            
011300                           LOPB-PCB INLE-PCB W6F1-PCB W6INLA-PCB          
011400                           UPFA-PCB WDB6-PCB WDK7-PCB WDL6-PCB            
011410       IF REQU-IDMSGVER NOT = 101                                         
011500         PERFORM S02-RETURN-RESPONSE                                      
011600       END-IF                                                             
011700     END-IF                                                               
011800                                                                          
011900     PERFORM Z-FINIT                                                      
012000     MOVE ZERO TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400 A-INIT SECTION.                                                          
012500     CONTINUE                                                             
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012900     CONTINUE                                                             
013000     .                                                                    
013100     EJECT                                                                
013200*    --- DISPATCHER SECTIONS                                              
013300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013400                                                                          
013500     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013600     MOVE 'CARPARTS.NDC.INSPECTIONREPORT'                                 
013700                                   TO SUB-ADDISPABS                       
013800     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
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
015300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015600                                                                          
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
