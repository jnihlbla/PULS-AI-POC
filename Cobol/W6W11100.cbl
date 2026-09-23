000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W11100.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.REGISTERADVICENOTE                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W111T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011110 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6011100 (TRANSACTION W6T111)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W111T                                             
002100*        REQUEST:     W60111I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60111O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W11100'.            
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
005600     03  W6011110                PIC X(8)    VALUE 'W6011110'.            
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
007300*    03  -COPY W60111I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60111O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300 01 -COPY W0009     -PRE MSG-                                             
008400                                                                          
008500 01  USEA-PCB                    PIC X.                                   
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
010500 01  REG-INLB-PCB                PIC X.                                   
010600                                                                          
010700 01  REG-WDK7-PCB                PIC X.                                   
010710                                                                          
010720 01  REG-WDB6-PCB                PIC X.                                   
010730*01    -COPY W0008     -PRE 9305-                                         
010740     05  FILLER                  PIC X(30).                               
010800     EJECT                                                                
010900 PROCEDURE DIVISION  USING MSG-PCB                                        
011000                           WDB6-LEV-PCB  WDB6-PCB                         
011100                           REG-INLA1-PCB REG-INLA2-PCB                    
011200                           REG-INLA3-PCB REG-LEVA-PCB                     
011300                           REG-ARTC-PCB  REG-BENA-PCB                     
011400                           REG-INLB-PCB  REG-WDK7-PCB                     
011410                           REG-WDB6-PCB  9305-PCB.                        
011500                                                                          
011600 MAIN SECTION.                                                            
011700     ENTRY 'DLITCBL' USING MSG-PCB                                        
011800                           WDB6-LEV-PCB  WDB6-PCB                         
011900                           REG-INLA1-PCB REG-INLA2-PCB                    
012000                           REG-INLA3-PCB REG-LEVA-PCB                     
012100                           REG-ARTC-PCB  REG-BENA-PCB                     
012200                           REG-INLB-PCB  REG-WDK7-PCB                     
012210                           REG-WDB6-PCB  9305-PCB.                        
012300                                                                          
012400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012500     IF SUB-KDRC = 0                                                      
012600       PERFORM A-INIT                                                     
012700                                                                          
012800       CALL W6011110 USING REQU-AREA   RESP-AREA  MAX-KVRADER             
012900                         MSG-PCB       WDB6-LEV-PCB  WDB6-PCB             
013000                         REG-INLA1-PCB REG-INLA2-PCB REG-INLA3-PCB        
013100                         REG-LEVA-PCB  REG-ARTC-PCB  REG-BENA-PCB         
013200                         REG-INLB-PCB  REG-WDK7-PCB  REG-WDB6-PCB         
013210                         9305-PCB                                         
013300                                                                          
013400       PERFORM S02-RETURN-RESPONSE                                        
013500     END-IF                                                               
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
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
015300     MOVE 'GETARG'               TO SUB-KDFUNC                            
015400     MOVE 'CARPARTS.NDC.REGISTERADVICENOTE'   TO SUB-ADDISPABS            
015500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
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
017000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
017100                                                                          
017200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
017300                                                                          
017400     IF SUB-KDRC > 0                                                      
017500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
