000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W11600.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.REGADVICENOTEWACCOUNT                       
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W116T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011610 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6011600 (TRANSACTION W6T116)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W116T                                             
002100*        REQUEST:     W60116I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60116O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W11600'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100     EJECT                                                                
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005600     03  W6011610                PIC X(8)    VALUE 'W6011610'.            
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
007300*    03  -COPY W60116I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60116O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300 01 -COPY W0009     -PRE MSG-                                             
008400                                                                          
008500*   PCB'ER FÖR SUB PGM W611610                                            
008600                                                                          
008700 01  WDB6-LEV-PCB                PIC X.                                   
008800                                                                          
008900 01  WDB6-PCB                    PIC X.                                   
009000                                                                          
009100*   PCB'ER FÖR SUB PGM W611REG                                            
009200                                                                          
009300 01  REG-INLA1-PCB               PIC X.                                   
009400                                                                          
009500 01  REG-INLA2-PCB               PIC X.                                   
009600                                                                          
009700 01  REG-INLA3-PCB               PIC X.                                   
009800                                                                          
009900 01  REG-LEVA-PCB                PIC X.                                   
010000                                                                          
010100 01  REG-ARTC-PCB                PIC X.                                   
010200                                                                          
010300 01  REG-BENA-PCB                PIC X.                                   
010400                                                                          
010500 01  REG-WDD9-PCB                PIC X.                                   
010600                                                                          
010700 01  REG-WDK7-PCB                PIC X.                                   
010800                                                                          
010810 01  REG-WDB6-PCB                PIC X.                                   
010820                                                                          
010900*   PCB'ER FÖR SUB PGM W411SAP                                            
011000                                                                          
011100 01  SAP-SAPC-PCB              PIC X.                                     
011200                                                                          
011210*01    -COPY W0008     -PRE 9305-                                         
011220     05  FILLER                  PIC X(30).                               
011230                                                                          
011300     EJECT                                                                
011400 PROCEDURE DIVISION  USING MSG-PCB                                        
011500                           WDB6-LEV-PCB   WDB6-PCB                        
011600                           REG-INLA1-PCB  REG-INLA2-PCB                   
011700                           REG-INLA3-PCB  REG-LEVA-PCB                    
011800                                          REG-ARTC-PCB                    
011900                           REG-BENA-PCB   REG-WDD9-PCB                    
012000                           REG-WDK7-PCB   REG-WDB6-PCB                    
012100                           SAP-SAPC-PCB                                   
012110                           9305-PCB.                                      
012200 MAIN SECTION.                                                            
012300     ENTRY 'DLITCBL' USING MSG-PCB                                        
012400                           WDB6-LEV-PCB   WDB6-PCB                        
012500                           REG-INLA1-PCB  REG-INLA2-PCB                   
012600                           REG-INLA3-PCB  REG-LEVA-PCB                    
012700                                          REG-ARTC-PCB                    
012800                           REG-BENA-PCB   REG-WDD9-PCB                    
012900                           REG-WDK7-PCB   REG-WDB6-PCB                    
013000                           SAP-SAPC-PCB                                   
013010                           9305-PCB.                                      
013100                                                                          
013200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013300     IF SUB-KDRC = 0                                                      
013400       PERFORM A-INIT                                                     
013500                                                                          
013600       CALL W6011610 USING REQU-AREA   RESP-AREA  MAX-KVRADER             
013700                           MSG-PCB                                        
013800                           WDB6-LEV-PCB   WDB6-PCB                        
013900                           REG-INLA1-PCB  REG-INLA2-PCB                   
014000                           REG-INLA3-PCB  REG-LEVA-PCB                    
014100                                          REG-ARTC-PCB                    
014200                           REG-BENA-PCB   REG-WDD9-PCB                    
014300                           REG-WDK7-PCB   REG-WDB6-PCB                    
014400                           SAP-SAPC-PCB                                   
014410                           9305-PCB                                       
014500                                                                          
014600       PERFORM S02-RETURN-RESPONSE                                        
014700     END-IF                                                               
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     CONTINUE                                                             
015600     .                                                                    
015700     EJECT                                                                
015800 Z-FINIT SECTION.                                                         
015900     CONTINUE                                                             
016000     .                                                                    
016100     EJECT                                                                
016200*    --- DISPATCHER SECTIONS                                              
016300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
016400                                                                          
016500     MOVE 'GETARG'               TO SUB-KDFUNC                            
016600     MOVE 'CARPARTS.NDC.REGADVICENOTEWACCOUNT'    TO SUB-ADDISPABS        
016700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
016800                                                                          
016900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
017000                                                                          
017100     IF SUB-KDRC > 0                                                      
017200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
017400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017600     END-IF                                                               
017700     .                                                                    
017800     SKIP3                                                                
017900 S02-RETURN-RESPONSE SECTION.                                             
018000                                                                          
018100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
018200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
018300                                                                          
018400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
018500                                                                          
018600     IF SUB-KDRC > 0                                                      
018700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
018800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
018900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019100     END-IF                                                               
019200     .                                                                    
019300     EJECT                                                                
