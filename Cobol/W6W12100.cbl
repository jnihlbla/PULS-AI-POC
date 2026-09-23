000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W12100.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   12/08/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.UNLOADINGINFORMATION                        
000800*                                                                         
000900*    FUNCTION:                                                            
001001*        THIS IS A DRIVER PGM FOR TRANSACTION W6W121T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6012110 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6012100 (TRANSACTION W6T121)                     
001800*                                                                         
001900*    INDATA.                                                              
002001*        TRANSACTION: W6W121T                                             
002100*        REQUEST:     W60121I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60121O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W12100'.            
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
005600     03  W6012110                PIC X(8)    VALUE 'W6012110'.            
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
007300*    03  -COPY W60121I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60121O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300*01  -COPY W0009   -PRE MSG-                                              
008401 01  ALT-PCB                     PIC X.                                   
008501 01  ALT1-PCB                    PIC X.                                   
008601 01  DISP-PCB                    PIC X.                                   
008701 01  USEA-PCB                    PIC X.                                   
008801 01  LISB-PCB                    PIC X.                                   
008901 01  INLA-PCB                    PIC X.                                   
009001 01  INLA-ALT-PCB                PIC X.                                   
009101 01  LASA-A-PCB                  PIC X.                                   
009201 01  LASA-B-PCB                  PIC X.                                   
009301 01  PLAA-PCB                    PIC X.                                   
009401 01  HANA-PCB                    PIC X.                                   
009501 01  STYR-PLAA-PCB               PIC X.                                   
009601 01  KOM-KOMA-PCB                PIC X.                                   
009701 01  LASA-W6012111-PCB           PIC X.                                   
009801 01  INLA-W6012111-PCB           PIC X.                                   
009901 01  LASA-W6012112-PCB           PIC X.                                   
010001 01  INLA-W6012112-PCB           PIC X.                                   
010101 01  WDK6-W6012112-PCB           PIC X.                                   
010102 01  WDK7-PCB                    PIC X.                                   
010200     EJECT                                                                
010301 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  ALT1-PCB                     
010401                           DISP-PCB USEA-PCB LISB-PCB                     
010501                           INLA-PCB INLA-ALT-PCB LASA-A-PCB               
010601                           LASA-B-PCB PLAA-PCB HANA-PCB                   
010701                           STYR-PLAA-PCB KOM-KOMA-PCB                     
010801                           LASA-W6012111-PCB INLA-W6012111-PCB            
010901                           LASA-W6012112-PCB INLA-W6012112-PCB            
011001                           WDK6-W6012112-PCB WDK7-PCB.                    
011100                                                                          
011201     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  ALT1-PCB                     
011301                           DISP-PCB USEA-PCB LISB-PCB                     
011401                           INLA-PCB INLA-ALT-PCB LASA-A-PCB               
011501                           LASA-B-PCB PLAA-PCB HANA-PCB                   
011601                           STYR-PLAA-PCB KOM-KOMA-PCB                     
011701                           LASA-W6012111-PCB INLA-W6012111-PCB            
011801                           LASA-W6012112-PCB INLA-W6012112-PCB            
011901                           WDK6-W6012112-PCB WDK7-PCB.                    
012000                                                                          
012100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012200     IF SUB-KDRC = 0                                                      
012300       PERFORM A-INIT                                                     
012400                                                                          
012500       CALL W6012110 USING REQU-AREA RESP-AREA MAX-KVRADER                
012601                           MSG-PCB   ALT-PCB   ALT1-PCB                   
012701                           DISP-PCB  USEA-PCB  LISB-PCB                   
012801                           INLA-PCB  INLA-ALT-PCB LASA-A-PCB              
012901                           LASA-B-PCB PLAA-PCB HANA-PCB                   
013001                           STYR-PLAA-PCB       KOM-KOMA-PCB               
013101                           LASA-W6012111-PCB   INLA-W6012111-PCB          
013201                           LASA-W6012112-PCB   INLA-W6012112-PCB          
013301                           WDK6-W6012112-PCB WDK7-PCB                     
013400       PERFORM S02-RETURN-RESPONSE                                        
013500     END-IF                                                               
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014210     MOVE '001'                  TO REQU-IDMSGVER                         
014300     CONTINUE                                                             
014400     .                                                                    
014500     EJECT                                                                
014600 Z-FINIT SECTION.                                                         
014700     CONTINUE                                                             
014800     .                                                                    
014900     EJECT                                                                
015000*    --- DISPATCHER SECTIONS                                              
015100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
015200                                                                          
015300     MOVE 'GETARG'                 TO SUB-KDFUNC                          
015400     MOVE 'CARPARTS.NDC.UNLOADINGINFORMATION'                             
015410                                   TO SUB-ADDISPABS                       
015500     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
015600                                                                          
015700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
015800                                                                          
015900     IF SUB-KDRC > 0                                                      
016000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
016200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016400     END-IF                                                               
016500     .                                                                    
016600     SKIP3                                                                
016700 S02-RETURN-RESPONSE SECTION.                                             
016800                                                                          
016900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
017000*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
017100     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
017201           (500 - RESP-KVRADER) * LENGTH OF RESP-LINE                     
017300                                                                          
017400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
017500                                                                          
017600     IF SUB-KDRC > 0                                                      
017700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
018000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018100     END-IF                                                               
018200     .                                                                    
019000     EJECT                                                                
