000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W12600.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/03/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.ADDRESSINFORMATION.QUERY                    
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W126T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6012610 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6012600 (TRANSACTION W6T126)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W126T                                             
002100*        REQUEST:     W60126I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60126O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W12600'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +100 COMP.             
004700 77  W-KVRADER                   PIC S9(4)   COMP.                        
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W6012610                PIC X(8)    VALUE 'W6012610'.            
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
007400*    03  -COPY W60126I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W60126O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300 01  MSG-PCB                     PIC X.                                   
008400                                                                          
008500 01  UPFB-PCB                    PIC X.                                   
008600     EJECT                                                                
008700 PROCEDURE DIVISION  USING MSG-PCB UPFB-PCB.                              
008800 MAIN SECTION.                                                            
008900     ENTRY 'DLITCBL' USING MSG-PCB UPFB-PCB.                              
009000                                                                          
009100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009200     IF SUB-KDRC = 0                                                      
009300       PERFORM A-INIT                                                     
009400                                                                          
009500       CALL W6012610 USING                                                
009600            MSG-PCB UPFB-PCB  MAX-KVRADER                                 
009700            REQU-AREA  RESP-AREA                                          
009800                                                                          
009900       PERFORM S02-RETURN-RESPONSE                                        
010000     END-IF                                                               
010100                                                                          
010200     PERFORM Z-FINIT                                                      
010300     MOVE ZERO TO RETURN-CODE                                             
010400     GOBACK                                                               
010500     .                                                                    
010600     EJECT                                                                
010700 A-INIT SECTION.                                                          
010800     CONTINUE                                                             
010900     .                                                                    
011000     EJECT                                                                
011100 Z-FINIT SECTION.                                                         
011200     CONTINUE                                                             
011300     .                                                                    
011400     EJECT                                                                
011500*    --- DISPATCHER SECTIONS                                              
011600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
011700                                                                          
011800     MOVE 'GETARG'               TO SUB-KDFUNC                            
011900     MOVE 'CARPARTS.NDC.ADDRESSINFORMATION.QUERY'                         
011910                                 TO SUB-ADDISPABS                         
012000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
012100                                                                          
012200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
012300                                                                          
012400     IF SUB-KDRC > 0                                                      
012500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
012600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
012900     END-IF                                                               
013000     .                                                                    
013100     SKIP3                                                                
013200 S02-RETURN-RESPONSE SECTION.                                             
013300                                                                          
013400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
013500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
013600     MOVE RESP-KVRADER               TO W-KVRADER                         
013700     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
013800         LENGTH OF RESP-RAD-GRUPP * (MAX-KVRADER - W-KVRADER)             
013900                                                                          
014000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014100                                                                          
014200     IF SUB-KDRC > 0                                                      
014300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
014500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014700     END-IF                                                               
014800     .                                                                    
014900     EJECT                                                                
