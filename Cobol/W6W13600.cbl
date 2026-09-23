001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6W13600.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   12/07/17.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       TEST                                                     
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        THIS IS A DRIVER PGM FOR TRANSACTION W6W136T/U                   
002200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
002300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6013610 WHICH              
002400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
002500*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSACTION: W6W136T                                             
003000*        REQUEST:     W60136I1                                            
003100*                                                                         
003200*    OUTDATA.                                                             
003300*        RESPONSE:    W60136O1                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500     SKIP3                                                                
004600 FILE SECTION.                                                            
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W6W13600'.            
005100                                                                          
005200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005400 77  KDRC-DISPLAY                PIC Z(5).                                
005410 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
005500                                                                          
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800                                                                          
006800     EJECT                                                                
006900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007000 01  GENERAL-SUBPROGRAMS.                                                 
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007400     03  W6013610                PIC X(8)    VALUE 'W6013610'.            
007500     SKIP3                                                                
007600*    --- PARAMETERS TO ABEND                                              
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008200     SKIP3                                                                
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009300     SKIP3                                                                
009400*01  -COPY WZ01SUB                                                        
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009700     SKIP3                                                                
009800 01  REQU-AREA.                                                           
009900*    03  -COPY WZ01REQU                                                   
010000*    03  -COPY W60136I1                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010300     SKIP3                                                                
010400 01  RESP-AREA.                                                           
010500*    03  -COPY WZ01RESP                                                   
010600*    03  -COPY W60136O1                                                   
010700     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013710     EJECT                                                                
013720*01  -COPY W0009   -PRE 6191-                                             
013730     EJECT                                                                
013740*01  -COPY W0009   -PRE 6194-                                             
013750     EJECT                                                                
013760*01  -COPY W0009   -PRE 6195-                                             
013770     EJECT                                                                
013780*01  -COPY W0009   -PRE 6197-                                             
013790     EJECT                                                                
013791*01  -COPY W0009   -PRE 6202-                                             
013792     EJECT                                                                
013793*01  -COPY W0009   -PRE DISP-                                             
013794     EJECT                                                                
013795*01  -COPY W0008  -PRE USEA-                                              
013796     05  FILLER                  PIC X.                                   
013797     EJECT                                                                
013798*01  -COPY W0008  -PRE INLA-                                              
013799     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013801*01  -COPY W0008  -PRE INLABSEQ-                                          
013802     05  FILLER                  PIC X.                                   
013803     EJECT                                                                
013804*01  -COPY W0008  -PRE INLC-                                              
013805     05  FILLER                  PIC X.                                   
013806     EJECT                                                                
013807*01  -COPY W0008  -PRE PLAA-                                              
013808     05  FILLER                  PIC X.                                   
013809     EJECT                                                                
013810*01  -COPY W0008  -PRE LOPA-                                              
013811     05  FILLER                  PIC X.                                   
013812     EJECT                                                                
013813*01  -COPY W0008  -PRE KVAE-                                              
013814     05  FILLER                  PIC X.                                   
013815     EJECT                                                                
013816*01  -COPY W0008  -PRE KVABSEQ-                                           
013817     05  FILLER                  PIC X.                                   
013818     EJECT                                                                
013819*01  -COPY W0008  -PRE WDK6-                                              
013820     05  FILLER                  PIC X.                                   
013821     EJECT                                                                
013822*01  -COPY W0008  -PRE WDB6-                                              
013823     05  FILLER                  PIC X.                                   
013824     EJECT                                                                
013825 01  PMRK-INLA1-PCB              PIC X.                                   
013826                                                                          
013827 01  PMRK-INLA2-PCB              PIC X.                                   
013828                                                                          
013829 01  PMRK-PLAA-PCB               PIC X.                                   
013830                                                                          
013831 01  KOM-KOMA-PCB                PIC X.                                   
013832                                                                          
013833 01  STYR-HANA-PCB               PIC X.                                   
013834                                                                          
013835 01  STYR-PLAA-PCB               PIC X.                                   
013836                                                                          
013837*01  -COPY W0008  -PRE UPFA-                                              
013838     05  FILLER                  PIC X.                                   
014043 PROCEDURE DIVISION  USING MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
014044                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
014045                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
014046                           PLAA-PCB LOPA-PCB KVAE-PCB                     
014047                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
014048                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
014049                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
014050                           STYR-HANA-PCB                                  
014060                           STYR-PLAA-PCB UPFA-PCB.                        
014066 MAIN SECTION.                                                            
014070     ENTRY 'DLITCBL' USING MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
014080                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
014081                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
014082                           PLAA-PCB LOPA-PCB KVAE-PCB                     
014083                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
014084                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
014085                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
014086                           STYR-HANA-PCB                                  
014087                           STYR-PLAA-PCB UPFA-PCB.                        
014120                                                                          
014200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014300     IF SUB-KDRC = 0                                                      
014400       PERFORM A-INIT                                                     
014410       CALL W6013610 USING REQU-AREA RESP-AREA MAX-KVRADER                
014420                           MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
014430                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
014440                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
014450                           PLAA-PCB LOPA-PCB KVAE-PCB                     
014460                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
014470                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
014480                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
014490                           STYR-HANA-PCB                                  
014491                           STYR-PLAA-PCB UPFA-PCB                         
015000       PERFORM S02-RETURN-RESPONSE                                        
015100     END-IF                                                               
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     CONTINUE                                                             
017100     .                                                                    
017200     EJECT                                                                
020800 Z-FINIT SECTION.                                                         
020900                                                                          
021400     .                                                                    
021500     EJECT                                                                
021600*    --- DISPATCHER SECTIONS                                              
021700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
021800                                                                          
021900     MOVE 'GETARG'               TO SUB-KDFUNC                            
022000     MOVE 'CARPARTS.NDC.CREATECASES'                                      
022010                                 TO SUB-ADDISPABS                         
022100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
022200                                                                          
022300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
022400                                                                          
022500     IF SUB-KDRC > 0                                                      
022600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
022700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
022800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
023000     END-IF                                                               
023100     .                                                                    
023200     SKIP3                                                                
023300 S02-RETURN-RESPONSE SECTION.                                             
023400                                                                          
023500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
023600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
023700                                                                          
023800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
023900                                                                          
024000     IF SUB-KDRC > 0                                                      
024100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
024200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
024300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
024400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
