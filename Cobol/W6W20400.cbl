000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W20400.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   12/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.IRPACKING                                   
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W204T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6020410 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6020400 (TRANSACTION W6T204)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W204T/U                                           
002100*        REQUEST:     W60204I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60204O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W20400'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004700                                                                          
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005600     03  W6020410                PIC X(8)    VALUE 'W6020410'.            
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
007300*    03  -COPY W60204I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60204O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300*01  -COPY W0009   -PRE MSG-                                              
008400 01  KVAE-PCB                    PIC X.                                   
008500 01  BENA-PCB                    PIC X.                                   
008600 01  LEVA-PCB                    PIC X.                                   
008700 01  EMBB-PCB                    PIC X.                                   
008800 01  WDP3-PCB                    PIC X.                                   
008900 01  ARTC-PCB                    PIC X.                                   
008900 01  WDB6-PCB                    PIC X.                                   
009000 01  W6H7-PCB                    PIC X.                                   
009100 01  9305-PCB                    PIC X.                                   
009200 01  WDK6-PCB                    PIC X.                                   
009300 01  LOPB-PCB                    PIC X.                                   
009400 01  FILC-PCB                    PIC X.                                   
009410 01  WDK7-PCB                    PIC X.                                   
009420 01  KRUP-WDB6-PCB               PIC X.                                   
009500     EJECT                                                                
009600 PROCEDURE DIVISION  USING MSG-PCB KVAE-PCB BENA-PCB                      
009700                           LEVA-PCB EMBB-PCB WDP3-PCB ARTC-PCB            
                                 WDB6-PCB                                       
009800                           W6H7-PCB 9305-PCB WDK6-PCB LOPB-PCB            
009900                           FILC-PCB WDK7-PCB KRUP-WDB6-PCB.               
010000                                                                          
010100     ENTRY 'DLITCBL' USING MSG-PCB KVAE-PCB BENA-PCB                      
010200                           LEVA-PCB EMBB-PCB WDP3-PCB ARTC-PCB            
                                 WDB6-PCB                                       
010300                           W6H7-PCB 9305-PCB WDK6-PCB LOPB-PCB            
010400                           FILC-PCB WDK7-PCB KRUP-WDB6-PCB.               
010500                                                                          
010600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010700     IF SUB-KDRC = 0                                                      
010800       PERFORM A-INIT                                                     
010900                                                                          
011000       CALL W6020410 USING REQU-AREA RESP-AREA MAX-KVRADER                
011100                           MSG-PCB  KVAE-PCB BENA-PCB                     
011200                           LEVA-PCB EMBB-PCB WDP3-PCB                     
011300                           ARTC-PCB WDB6-PCB                              
011300                           W6H7-PCB 9305-PCB                              
011400                           WDK6-PCB LOPB-PCB FILC-PCB WDK7-PCB            
011410                           KRUP-WDB6-PCB                                  
011500       PERFORM S02-RETURN-RESPONSE                                        
011600     END-IF                                                               
011700                                                                          
011800     PERFORM Z-FINIT                                                      
011900     MOVE ZERO TO RETURN-CODE                                             
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300 A-INIT SECTION.                                                          
012400     CONTINUE                                                             
012500     .                                                                    
012600     EJECT                                                                
012700 Z-FINIT SECTION.                                                         
012800     CONTINUE                                                             
012900     .                                                                    
013000     EJECT                                                                
013100*    --- DISPATCHER SECTIONS                                              
013200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013300                                                                          
013400     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013500     MOVE 'CARPARTS.NDC.IRPACKING' TO SUB-ADDISPABS                       
013600     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013700                                                                          
013800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013900                                                                          
014000     IF SUB-KDRC > 0                                                      
014100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014500     END-IF                                                               
014600     .                                                                    
014700     SKIP3                                                                
014800 S02-RETURN-RESPONSE SECTION.                                             
014900                                                                          
015000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
015100*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015200     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
015300           (500 - RESP-KVRADER) * LENGTH OF RESP-LINE                     
015400                                                                          
015500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015600                                                                          
015700     IF SUB-KDRC > 0                                                      
015800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
016000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016200     END-IF                                                               
016300     .                                                                    
016400     EJECT                                                                
