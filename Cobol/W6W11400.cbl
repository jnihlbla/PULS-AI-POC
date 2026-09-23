000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W11400.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.CORRECTADVICENOTE                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W114T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011410 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W6011400 (TRANSACTION W6T114)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6W114T                                             
002100*        REQUEST:     W60114I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W60114O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W6W11400'.            
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
005600     03  W6011410                PIC X(8)    VALUE 'W6011410'.            
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
007300*    03  -COPY W60114I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600     SKIP3                                                                
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007900*    03  -COPY W60114O1                                                   
008000     EJECT                                                                
008100 LINKAGE SECTION.                                                         
008200                                                                          
008300 01  -COPY W0009   -PRE MSG-                                              
008400                                                                          
008500 01  W6INLB-PCB                  PIC X.                                   
008600                                                                          
008700 01  INLA1-PCB                   PIC X.                                   
008800                                                                          
008900 01  INLA2-PCB                   PIC X.                                   
009000                                                                          
009100 01  LEVA-PCB                    PIC X.                                   
009200                                                                          
009300 01  ARTC-PCB                    PIC X.                                   
009400                                                                          
009500 01  WDD9-PCB                    PIC X.                                   
009600                                                                          
009700 01  ARTS-PCB                    PIC X.                                   
009800                                                                          
009900 01  WDB6-PCB                    PIC X.                                   
010000                                                                          
010100     EJECT                                                                
010200*   PCB'ER FÖR SUB PGM W611REG                                            
010300                                                                          
010400 01  REG-INLA1-PCB               PIC X.                                   
010500                                                                          
010600 01  REG-INLA2-PCB               PIC X.                                   
010700                                                                          
010800 01  REG-INLA3-PCB               PIC X.                                   
010900                                                                          
011000 01  REG-LEVA-PCB                PIC X.                                   
011100                                                                          
011200 01  REG-ARTC-PCB                PIC X.                                   
011300                                                                          
011400 01  REG-BENA-PCB                PIC X.                                   
011500                                                                          
011600 01  REG-WDD9-PCB                PIC X.                                   
011700                                                                          
011800 01  REG-WDK7-PCB                PIC X.                                   
011900                                                                          
011910 01  REG-WDB6-PCB                PIC X.                                   
011920                                                                          
012000*   PCB'ER FÖR SUB PGM W411SAP                                            
012100                                                                          
012200 01  SAP-SAPC-PCB                PIC X.                                   
012300                                                                          
012600 01  LEVP-INLB-PCB               PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING MSG-PCB       W6INLB-PCB                       
012900                           INLA1-PCB INLA2-PCB LEVA-PCB                   
013000                           ARTC-PCB WDD9-PCB ARTS-PCB                     
013100                           WDB6-PCB                                       
013200                           REG-INLA1-PCB REG-INLA2-PCB                    
013300                           REG-INLA3-PCB REG-LEVA-PCB                     
013400                                         REG-ARTC-PCB                     
013500                           REG-BENA-PCB  REG-WDD9-PCB                     
013600                           REG-WDK7-PCB  REG-WDB6-PCB                     
013700                           SAP-SAPC-PCB                                   
013800                           LEVP-INLB-PCB.                                 
013900 MAIN SECTION.                                                            
014000     ENTRY 'DLITCBL' USING MSG-PCB       W6INLB-PCB INLA1-PCB             
014100                           INLA2-PCB                                      
014200                           LEVA-PCB ARTC-PCB WDD9-PCB ARTS-PCB            
014300                           WDB6-PCB                                       
014400                           REG-INLA1-PCB REG-INLA2-PCB                    
014500                           REG-INLA3-PCB REG-LEVA-PCB                     
014600                                         REG-ARTC-PCB                     
014700                           REG-BENA-PCB  REG-WDD9-PCB                     
014800                           REG-WDK7-PCB  REG-WDB6-PCB                     
014900                           SAP-SAPC-PCB                                   
015000                           LEVP-INLB-PCB.                                 
015100                                                                          
015200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015300     IF SUB-KDRC = 0                                                      
015400       PERFORM A-INIT                                                     
015500                                                                          
015600       CALL W6011410 USING REQU-AREA   RESP-AREA  MAX-KVRADER             
015700                           MSG-PCB     W6INLB-PCB INLA1-PCB               
015800                           INLA2-PCB                                      
015900                           LEVA-PCB ARTC-PCB WDD9-PCB ARTS-PCB            
016000                           WDB6-PCB                                       
016100                           REG-INLA1-PCB REG-INLA2-PCB                    
016200                           REG-INLA3-PCB REG-LEVA-PCB                     
016300                                         REG-ARTC-PCB                     
016400                           REG-BENA-PCB  REG-WDD9-PCB                     
016500                           REG-WDK7-PCB  REG-WDB6-PCB                     
016600                           SAP-SAPC-PCB                                   
016700                           LEVP-INLB-PCB                                  
016800                                                                          
016900       PERFORM S02-RETURN-RESPONSE                                        
017000     END-IF                                                               
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800     CONTINUE                                                             
017900     .                                                                    
018000     EJECT                                                                
018100 Z-FINIT SECTION.                                                         
018200     CONTINUE                                                             
018300     .                                                                    
018400     EJECT                                                                
018500*    --- DISPATCHER SECTIONS                                              
018600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
018700                                                                          
018800     MOVE 'GETARG'               TO SUB-KDFUNC                            
018900     MOVE 'CARPARTS.NDC.CORRECTADVICENOTE'   TO SUB-ADDISPABS             
019000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
019100                                                                          
019200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
019300                                                                          
019400     IF SUB-KDRC > 0                                                      
019500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
019600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
019700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019900     END-IF                                                               
020000     .                                                                    
020100     SKIP3                                                                
020200 S02-RETURN-RESPONSE SECTION.                                             
020300                                                                          
020400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
020500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
020600                                                                          
020700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
020800                                                                          
020900     IF SUB-KDRC > 0                                                      
021000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
