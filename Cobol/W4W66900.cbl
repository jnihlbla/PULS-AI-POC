000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W66900.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   14/05/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.BILLOFLADING                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W669T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W4066910 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W4066900 (TRANSACTION W4T669)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4W669T/U                                           
002100*        REQUEST:     W40669I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W40669O1                                            
002500*                                                                         
002600*    CHANGE LOG:                                                          
002700*      14/05/08 - REDDY RAHUL     - INITIAL VERSION.                      
002800*                                   TORONTO CHANGES.                      
002900*                                   ETRACKER 10228352.                    
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W4W66900'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
005100                                                                          
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W4066910                PIC X(8)    VALUE 'W4066910'.            
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
007400*    03  -COPY W40669I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W40669O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300                                                                          
008400*01  -COPY W0009   -PRE MSG-                                              
008500 01  ORQI-PCB                    PIC X.                                   
008600 01  4463-PCB                    PIC X.                                   
008700 01  1165-PCB                    PIC X.                                   
008900 01  WDB6-PCB                    PIC X.                                   
009000                                                                          
009100     EJECT                                                                
009200 PROCEDURE DIVISION  USING MSG-PCB  ORQI-PCB 4463-PCB                     
009300                           1165-PCB WDB6-PCB.                             
009400                                                                          
009500     ENTRY 'DLITCBL' USING MSG-PCB  ORQI-PCB 4463-PCB                     
009600                           1165-PCB WDB6-PCB.                             
009700                                                                          
009800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009900     IF SUB-KDRC = 0                                                      
010000       PERFORM A-INIT                                                     
010100                                                                          
010200       CALL W4066910 USING REQU-AREA RESP-AREA MAX-KVRADER                
010300                           ORQI-PCB  4463-PCB  1165-PCB                   
010400                           WDB6-PCB                                       
010500       PERFORM S02-RETURN-RESPONSE                                        
010600     END-IF                                                               
010700                                                                          
010800     PERFORM Z-FINIT                                                      
010900     MOVE ZERO TO RETURN-CODE                                             
011000     GOBACK                                                               
011100     .                                                                    
011200     EJECT                                                                
011300 A-INIT SECTION.                                                          
011400     CONTINUE                                                             
011500     .                                                                    
011600     EJECT                                                                
011700 Z-FINIT SECTION.                                                         
011800     CONTINUE                                                             
011900     .                                                                    
012000     EJECT                                                                
012100*    --- DISPATCHER SECTIONS                                              
012200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012300                                                                          
012400     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012500     MOVE 'CARPARTS.NDC.BILLOFLADING'                                     
012600                                   TO SUB-ADDISPABS                       
012700     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
012800                                                                          
012900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013000                                                                          
013100     IF SUB-KDRC > 0                                                      
013200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013600     END-IF                                                               
013700     .                                                                    
013800     SKIP3                                                                
013900 S02-RETURN-RESPONSE SECTION.                                             
014000                                                                          
014100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014200*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014300     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
014400           (500 - RESP-KVRADER) * LENGTH OF RESP-TABELLRAD                
014500                                                                          
014600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014700                                                                          
014800     IF SUB-KDRC > 0                                                      
014900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015300     END-IF                                                               
015400     .                                                                    
015500     EJECT                                                                
