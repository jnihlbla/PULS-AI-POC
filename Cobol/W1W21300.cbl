000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1W21300.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   15/09/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.PARTSINCLUDEDINKIT                         
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W1W213T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W1021310 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W1021300 (TRANSACTION W1T213)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W1W213T                                             
002100*        REQUEST:     W10213I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W1W213O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W1W21300'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004401 77  INDX                        PIC S9(9)   VALUE +0 COMP SYNC.          
004500                                                                          
004600     EJECT                                                                
004700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005100     03  W1021310                PIC X(8)    VALUE 'W1021310'.            
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
006800*    03  -COPY W10213I1                                                   
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007100     SKIP3                                                                
007200 01  RESP-AREA.                                                           
007300*    03  -COPY WZ01RESP                                                   
007400*    03  -COPY W10213O1                                                   
007500     EJECT                                                                
007510 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007520     SKIP3                                                                
007530 01  DISP-RESP-AREA.                                                      
007540*    03  -COPY WZ01RESP -PRE DISP-                                        
007550*    03  -COPY W1W213O1 -PRE DISP-                                        
007560     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800*01  -COPY W0009   -PRE MSG-                                              
007900     EJECT                                                                
008000 01  SATB-PCB                    PIC X.                                   
008100 01  BENA-PCB                    PIC X.                                   
008200 01  BENA-A-PCB                  PIC X.                                   
008300 01  ARTC-PCB                    PIC X.                                   
008400 01  WDK7-PCB                    PIC X.                                   
008500     EJECT                                                                
008600 PROCEDURE DIVISION  USING MSG-PCB    SATB-PCB BENA-PCB                   
008700                           BENA-A-PCB ARTC-PCB WDK7-PCB.                  
008800                                                                          
008900     ENTRY 'DLITCBL' USING MSG-PCB    SATB-PCB BENA-PCB                   
009000                           BENA-A-PCB ARTC-PCB WDK7-PCB.                  
009100                                                                          
009200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009300     IF SUB-KDRC = 0                                                      
009400       PERFORM A-INIT                                                     
009500                                                                          
009600       CALL W1021310 USING REQU-AREA RESP-AREA MAX-KVRADER                
009700                           SATB-PCB  BENA-PCB  BENA-A-PCB                 
009800                           ARTC-PCB WDK7-PCB                              
009810       PERFORM B-PREP-DISPATCHER-RESP-AREA                                
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
011010 B-PREP-DISPATCHER-RESP-AREA SECTION.                                     
011020                                                                          
011021     MOVE RESP-WZ01RESP       TO DISP-RESP-WZ01RESP                       
011022                                                                          
011023     MOVE RESP-IDRADNR-START  TO DISP-RESP-IDRADNR-START                  
011024     MOVE RESP-KVRADER        TO DISP-RESP-KVRADER                        
011025     MOVE RESP-BEART          TO DISP-RESP-BEART                          
011026     MOVE RESP-KDPRODSL       TO DISP-RESP-KDPRODSL                       
011027     MOVE RESP-IDFKNGRP       TO DISP-RESP-IDFKNGRP                       
011028     MOVE RESP-IDSTRTYP       TO DISP-RESP-IDSTRTYP                       
011029     MOVE RESP-KDPSLLOC       TO DISP-RESP-KDPSLLOC                       
011030                                                                          
011031     PERFORM                                                              
011032     VARYING INDX FROM +1 BY +1                                           
011033       UNTIL INDX > DISP-RESP-KVRADER                                     
011034         MOVE RESP-SELECT-LINE-ATTR (INDX)                                
011035                              TO DISP-RESP-SELECT-LINE-ATTR (INDX)        
011042         MOVE RESP-SELECT-LINE (INDX)                                     
011043                              TO DISP-RESP-SELECT-LINE (INDX)             
011054         MOVE RESP-IDRADNR-LINE (INDX)                                    
011055                              TO DISP-RESP-IDRADNR-LINE (INDX)            
011056         MOVE RESP-ADLAGOMR-LINE (INDX)                                   
011057                              TO DISP-RESP-ADLAGOMR-LINE (INDX)           
011058         MOVE RESP-ADGANG-LINE (INDX)                                     
011059                              TO DISP-RESP-ADGANG-LINE (INDX)             
011060         MOVE RESP-ADPLATS-LINE (INDX)                                    
011061                              TO DISP-RESP-ADPLATS-LINE (INDX)            
011062                                                                          
011063         MOVE RESP-REANTPSA-LINE (INDX)                                   
011064                              TO DISP-RESP-REANTPSA-LINE (INDX)           
011065         MOVE RESP-IDARTNR-LINE (INDX)                                    
011066                              TO DISP-RESP-IDARTNR-LINE (INDX)            
011072         MOVE RESP-BEART-LINE (INDX)                                      
011073                              TO DISP-RESP-BEART-LINE (INDX)              
011074         MOVE RESP-IDSTRTYP-LINE (INDX)                                   
011075                              TO DISP-RESP-IDSTRTYP-LINE (INDX)           
011076         MOVE RESP-KDISATS-LINE (INDX)                                    
011077                              TO DISP-RESP-KDISATS-LINE (INDX)            
011078         IF RESP-TIAAVV-LINE (INDX) = ZERO                                
011079           INSPECT DISP-RESP-TIAAVV-LINE (INDX)                           
011080             REPLACING CHARACTERS BY SPACE                                
011081         ELSE                                                             
011082           MOVE RESP-TIAAVV-LINE (INDX)                                   
011083                              TO DISP-RESP-TIAAVV-LINE (INDX)             
011084         END-IF                                                           
011085         MOVE RESP-KDFARLIG-LINE    (INDX)                                
011086                              TO DISP-RESP-KDFARLIG-LINE (INDX)           
011087     END-PERFORM                                                          
011088     .                                                                    
011090     EJECT                                                                
011100 Z-FINIT SECTION.                                                         
011200     CONTINUE                                                             
011300     .                                                                    
011400     EJECT                                                                
011500*    --- DISPATCHER SECTIONS                                              
011600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
011700                                                                          
011800     MOVE 'GETARG'                 TO SUB-KDFUNC                          
011900     MOVE 'CARPARTS.PULS.PARTSINCLUDEDINKIT'                              
012000                                   TO SUB-ADDISPABS                       
012100     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
012200                                                                          
012300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
012400                                                                          
012500     IF SUB-KDRC > 0                                                      
012600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
012700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013000     END-IF                                                               
013100     .                                                                    
013200     SKIP3                                                                
013300 S02-RETURN-RESPONSE SECTION.                                             
013400                                                                          
013500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
013600     COMPUTE SUB-KVDLEN = LENGTH OF DISP-RESP-AREA -                      
013700           (500 - RESP-KVRADER) * LENGTH OF DISP-RESP-OUTPUT              
013800                                                                          
013900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN DISP-RESP-AREA        
014000                                                                          
014100     IF SUB-KDRC > 0                                                      
014200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
014400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014600     END-IF                                                               
014700     .                                                                    
014800     EJECT                                                                
