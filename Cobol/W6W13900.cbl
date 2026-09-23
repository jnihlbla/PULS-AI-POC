000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W13900.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/06/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.RECEIVINGCONTROL.QUERY                      
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W139T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6013910 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W139T                                             
001800*        REQUEST:     W60139I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60139O1                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W13900'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400     EJECT                                                                
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  W6013910                PIC X(8)    VALUE 'W6013910'.            
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005000     SKIP3                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP3                                                                
005700*                                                                         
005800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005900     SKIP3                                                                
006000*01  -COPY WZ01SUB                                                        
006100     EJECT                                                                
006200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006300     SKIP3                                                                
006400 01  REQU-AREA.                                                           
006500*    03  -COPY WZ01REQU                                                   
006600*    03  -COPY W60139I1                                                   
006700     EJECT                                                                
006800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006900     SKIP3                                                                
007000 01  RESP-AREA.                                                           
007100*    03  -COPY WZ01RESP                                                   
007200*    03  -COPY W60139O1                                                   
007300     EJECT                                                                
007400 LINKAGE SECTION.                                                         
007500*01  -COPY W0009  -PRE MSG-                                               
007600                                                                          
007700*01  -COPY W0009  -PRE ALT-                                               
007800     EJECT                                                                
007810*01  -COPY W0009  -PRE ALT-IMS-                                           
007820     EJECT                                                                
007900*01  -COPY W0008  -PRE UPFA-                                              
008000     05  FILLER                  PIC X.                                   
008100     EJECT                                                                
008200*01  -COPY W0008  -PRE INLA-                                              
008300     05  FILLER                  PIC X.                                   
008400     EJECT                                                                
008500*01  -COPY W0008  -PRE KVAH-                                              
008600     05  FILLER                  PIC X.                                   
008700     EJECT                                                                
008800*01  -COPY W0008  -PRE WDF5-                                              
008900     05  FILLER                  PIC X.                                   
009000     EJECT                                                                
009100*01  -COPY W0008  -PRE LEVA-                                              
009200     05  FILLER                  PIC X.                                   
009300     EJECT                                                                
009400*01  -COPY W0008  -PRE KVAE1-                                             
009500     05  FILLER                  PIC X.                                   
009600     EJECT                                                                
009700*01  -COPY W0008  -PRE ARTC-                                              
009800     05  FILLER                  PIC X.                                   
009900     EJECT                                                                
010000*01  -COPY W0008  -PRE BENA-                                              
010100     05  FILLER                  PIC X.                                   
010200     EJECT                                                                
010300*01  -COPY W0008  -PRE INLC-                                              
010400     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010600*01  -COPY W0008  -PRE KVAE2-                                             
010700     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010900*01  -COPY W0008  -PRE WDB6-                                              
011000     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALT-IMS-PCB UPFA-PCB           
011300      INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB KVAE1-PCB ARTC-PCB              
011400      BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB.                               
011500                                                                          
011600 MAIN SECTION.                                                            
011700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT-IMS-PCB UPFA-PCB           
011800      INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB KVAE1-PCB ARTC-PCB              
011900      BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB.                               
012000                                                                          
012100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012200     IF SUB-KDRC = 0                                                      
012300       PERFORM A-INIT                                                     
012400       CALL W6013910 USING REQU-AREA RESP-AREA                            
012500                           ALT-PCB ALT-IMS-PCB UPFA-PCB                   
012600                           INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB            
012700                           KVAE1-PCB ARTC-PCB                             
012800                           BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB           
012900       PERFORM S02-RETURN-RESPONSE                                        
013000     END-IF                                                               
013100                                                                          
013200     PERFORM Z-FINIT                                                      
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013800     CONTINUE                                                             
013900     .                                                                    
014000     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014200     CONTINUE                                                             
014300     .                                                                    
014400     EJECT                                                                
014500*    --- DISPATCHER SECTIONS                                              
014600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
014700                                                                          
014800     MOVE 'GETARG'               TO SUB-KDFUNC                            
014900     MOVE 'CARPARTS.NDC.RECEIVINGCONTROL.QUERY'                           
014910                                 TO SUB-ADDISPABS                         
015000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
015100                                                                          
015200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
015300                                                                          
015400     IF SUB-KDRC > 0                                                      
015500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
015700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015900     END-IF                                                               
016000     .                                                                    
016100     SKIP3                                                                
016200 S02-RETURN-RESPONSE SECTION.                                             
016300                                                                          
016400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
016500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
016600                                                                          
016700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
016800                                                                          
016900     IF SUB-KDRC > 0                                                      
017000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
017300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017400     END-IF                                                               
017500     .                                                                    
017600     EJECT                                                                
