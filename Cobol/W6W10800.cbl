000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6W10800.                                                
000400*AUTHOR.         RAHUL REDDY.                                             
000500*DATE-WRITTEN.   12/05/22.                                                
000600                                                                          
000700*    NAME:       CARPARTS.NDC.INQUIRYTREATMENT.QUERY                      
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W108T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6010810 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6010800 (TRANSACTION W6T108)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W108T                                             
002100*        REQUEST:     W60108I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60108O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(08)   VALUE 'W6W10800'.            
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003500 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
003900 01  GENERAL-SUBPROGRAM.                                                  
004000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
004200     03  W6010810                PIC X(8)    VALUE 'W6010810'.            
004300     EJECT                                                                
004400*                                                                         
004500*    --- PARAMETERS TO ABEND                                              
004600                                                                          
004700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005000     EJECT                                                                
005100*                                                                         
005200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005300     SKIP3                                                                
005400*01  -COPY WZ01SUB                                                        
005500     EJECT                                                                
005600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
005700     SKIP3                                                                
005800 01  REQU-AREA.                                                           
005900*    03  -COPY WZ01REQU                                                   
006000*    03  -COPY W60108I1                                                   
006100     EJECT                                                                
006200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006300     SKIP3                                                                
006400 01  RESP-AREA.                                                           
006500*    03  -COPY WZ01RESP                                                   
006600*    03  -COPY W60108O1                                                   
006700     EJECT                                                                
006800 LINKAGE SECTION.                                                         
006900                                                                          
007000*01  -COPY W0009   -PRE MSG-                                              
007100     EJECT                                                                
007200 01  HANA-PCB                    PIC X.                                   
007300 01  PLAA-PCB                    PIC X.                                   
007400 01  WDP3-PCB                    PIC X.                                   
007500 01  ARTC-PCB                    PIC X.                                   
007600 01  WDK7-PCB                    PIC X.                                   
007700 01  WDP3A-PCB                   PIC X.                                   
007800 01  WDP3B-PCB                   PIC X.                                   
007900 01  WDP3C-PCB                   PIC X.                                   
008000     EJECT                                                                
008100 PROCEDURE DIVISION  USING MSG-PCB                                        
008200                           HANA-PCB                                       
008300                           PLAA-PCB                                       
008400                           WDP3-PCB                                       
008500                           ARTC-PCB                                       
008600                           WDK7-PCB                                       
008700                           WDP3A-PCB                                      
008800                           WDP3B-PCB                                      
008900                           WDP3C-PCB.                                     
009000                                                                          
009100     ENTRY 'DLITCBL' USING MSG-PCB                                        
009200                           HANA-PCB                                       
009300                           PLAA-PCB                                       
009400                           WDP3-PCB                                       
009500                           ARTC-PCB                                       
009600                           WDK7-PCB                                       
009700                           WDP3A-PCB                                      
009800                           WDP3B-PCB                                      
009900                           WDP3C-PCB.                                     
010000                                                                          
010100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010200     IF SUB-KDRC = 0                                                      
010300       PERFORM A-INIT                                                     
010400                                                                          
010500       CALL W6010810 USING REQU-AREA RESP-AREA                            
010600                           HANA-PCB  PLAA-PCB  WDP3-PCB  ARTC-PCB         
010700                           WDK7-PCB  WDP3A-PCB WDP3B-PCB WDP3C-PCB        
010800       PERFORM S02-RETURN-RESPONSE                                        
010900     END-IF                                                               
011000                                                                          
011100     PERFORM Z-FINIT                                                      
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600                                                                          
011700 A-INIT SECTION.                                                          
011800     CONTINUE                                                             
011900     .                                                                    
012000     EJECT                                                                
012100 Z-FINIT SECTION.                                                         
012200     CONTINUE                                                             
012300     .                                                                    
012400     EJECT                                                                
012500                                                                          
012600*    --- DISPATCHER SECTIONS                                              
012700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012800                                                                          
012900     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013000     MOVE 'CARPARTS.NDC.INQUIRYTREATMENT.QUERY'                           
013010                                   TO SUB-ADDISPABS                       
013100     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013200                                                                          
013300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013400                                                                          
013500     IF SUB-KDRC > 0                                                      
013600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014000     END-IF                                                               
014100     .                                                                    
014200     SKIP3                                                                
014300 S02-RETURN-RESPONSE SECTION.                                             
014400                                                                          
014500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014900                                                                          
015000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015100                                                                          
015200     IF SUB-KDRC > 0                                                      
015300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015700     END-IF                                                               
015800     .                                                                    
015900     EJECT                                                                
