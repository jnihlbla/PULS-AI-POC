001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6W13300.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   12/07/17.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       TEST                                                     
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        THIS IS A DRIVER PGM FOR TRANSACTION W6W133T/U                   
002200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
002300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6013310 WHICH              
002400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
002500*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSACTION: W6W133T                                             
003000*        REQUEST:     W60133I1                                            
003100*                                                                         
003200*    OUTDATA.                                                             
003300*        RESPONSE:    W60133O1                                            
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
005000 77  IDPGM                       PIC X(08)   VALUE 'W6W13300'.            
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
007400     03  W6013310                PIC X(8)    VALUE 'W6013310'.            
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
010000*    03  -COPY W60133I1                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010300     SKIP3                                                                
010400 01  RESP-AREA.                                                           
010500*    03  -COPY WZ01RESP                                                   
010600*    03  -COPY W60133O1                                                   
010700     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013800*01  -COPY W0009   -PRE MSG-                                              
013810     EJECT                                                                
013820*01  -COPY W0009   -PRE ALT1-                                             
013830     EJECT                                                                
013840*01  -COPY W0009   -PRE ALT2-                                             
013850     EJECT                                                                
013860*01  -COPY W0009   -PRE ALT3-                                             
013870     EJECT                                                                
013880*01  -COPY W0009   -PRE ALT4-                                             
013890     EJECT                                                                
013891*01  -COPY W0009   -PRE 6197-                                             
013892     EJECT                                                                
013893*01  -COPY W0009   -PRE DISP-                                             
013894     EJECT                                                                
013895*01  -COPY W0008  -PRE USEA-                                              
013896     05  FILLER                  PIC X.                                   
013897     EJECT                                                                
013898*01  -COPY W0008  -PRE PLAA-                                              
013899     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
013910*01  -COPY W0008  -PRE LOPA-                                              
013920     05  FILLER                  PIC X.                                   
013930     EJECT                                                                
013940*01  -COPY W0008  -PRE INLA-                                              
013950     05  FILLER                  PIC X.                                   
013960     EJECT                                                                
013970*01  -COPY W0008  -PRE INLD-                                              
013980     05  FILLER                  PIC X.                                   
013990     EJECT                                                                
013991*01  -COPY W0008  -PRE INLC-                                              
013992     05  FILLER                  PIC X.                                   
013993     EJECT                                                                
013994*01  -COPY W0008  -PRE ALT-INLC-                                          
013995     05  FILLER                  PIC X.                                   
013996     EJECT                                                                
013997*01  -COPY W0008  -PRE INLB1-                                             
013998     05  FILLER                  PIC X.                                   
013999     EJECT                                                                
014000*01  -COPY W0008  -PRE INLG-                                              
014001     05  FILLER                  PIC X.                                   
014002     EJECT                                                                
014003*01  -COPY W0008  -PRE KVAE-                                              
014004     05  FILLER                  PIC X.                                   
014005     EJECT                                                                
014006*01  -COPY W0008  -PRE KVABSEQ-                                           
014007     05  FILLER                  PIC X.                                   
014008     EJECT                                                                
014009*01  -COPY W0008  -PRE WDK6-                                              
014010     05  FILLER                  PIC X.                                   
014011     EJECT                                                                
014012*01  -COPY W0008  -PRE WDB6-                                              
014013     05  FILLER                  PIC X.                                   
014014*01  -COPY W0008  -PRE WDD3-                                              
014015     05  FILLER                  PIC X.                                   
014016     EJECT                                                                
014017**  PCB'ER FÖR SUBPGM                                                     
014018 01  ADR-INLA-PCB                PIC X.                                   
014019                                                                          
014020 01  ADR-INLC-PCB                PIC X.                                   
014021                                                                          
014022 01  ADR-PLAA-PCB                PIC X.                                   
014023                                                                          
014024 01  ADR-WDK6-PCB                PIC X.                                   
014025                                                                          
014026 01  ADR-STYR-HANA-PCB           PIC X.                                   
014027                                                                          
014028 01  ADR-STYR-PLAA-PCB           PIC X.                                   
014029                                                                          
014030 01  KOM-KOMA-PCB                PIC X.                                   
014031                                                                          
014032 01  PMRK-INLB-PCB               PIC X.                                   
014033                                                                          
014034 01  PMRK-INLC-PCB               PIC X.                                   
014035                                                                          
014036 01  PMRK-PLAA-PCB               PIC X.                                   
014037                                                                          
014038 01  STYR-HANA-PCB               PIC X.                                   
014039                                                                          
014040 01  STYR-PLAA-PCB               PIC X.                                   
014041     EJECT                                                                
014042*01  -COPY W0008  -PRE UPFA-                                              
014043     05  FILLER                  PIC X.                                   
014044     EJECT                                                                
014045 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
014046                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
014047                           PLAA-PCB LOPA-PCB INLA-PCB                     
014048                           INLD-PCB                                       
014049                           INLC-PCB                                       
014050                           ALT-INLC-PCB                                   
014051                           INLB1-PCB                                      
014052                           INLG-PCB KVAE-PCB                              
014053                           KVABSEQ-PCB                                    
014054                           WDK6-PCB WDB6-PCB WDD3-PCB                     
014055                           ADR-INLA-PCB                                   
014056                           ADR-INLC-PCB                                   
014057                           ADR-PLAA-PCB                                   
014058                           ADR-WDK6-PCB                                   
014059                           ADR-STYR-HANA-PCB                              
014060                           ADR-STYR-PLAA-PCB                              
014061                           KOM-KOMA-PCB                                   
014062                           PMRK-INLB-PCB PMRK-INLC-PCB                    
014063                           PMRK-PLAA-PCB                                  
014064                           STYR-HANA-PCB                                  
014065                           STYR-PLAA-PCB                                  
014066                           UPFA-PCB.                                      
014067 MAIN SECTION.                                                            
014070     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
014090                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
014091                           PLAA-PCB LOPA-PCB INLA-PCB                     
014092                           INLD-PCB                                       
014093                           INLC-PCB                                       
014094                           ALT-INLC-PCB                                   
014095                           INLB1-PCB                                      
014096                           INLG-PCB KVAE-PCB                              
014097                           KVABSEQ-PCB                                    
014098                           WDK6-PCB WDB6-PCB WDD3-PCB                     
014099                           ADR-INLA-PCB                                   
014100                           ADR-INLC-PCB                                   
014101                           ADR-PLAA-PCB                                   
014102                           ADR-WDK6-PCB                                   
014103                           ADR-STYR-HANA-PCB                              
014104                           ADR-STYR-PLAA-PCB                              
014105                           KOM-KOMA-PCB                                   
014106                           PMRK-INLB-PCB PMRK-INLC-PCB                    
014107                           PMRK-PLAA-PCB                                  
014108                           STYR-HANA-PCB                                  
014109                           STYR-PLAA-PCB                                  
014110                           UPFA-PCB.                                      
014120                                                                          
014200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014300     IF SUB-KDRC = 0                                                      
014400       PERFORM A-INIT                                                     
014500       CALL W6013310 USING REQU-AREA RESP-AREA MAX-KVRADER                
014600                           MSG-PCB ALT1-PCB ALT2-PCB ALT3-PCB             
014700                           ALT4-PCB 6197-PCB DISP-PCB USEA-PCB            
014800                           PLAA-PCB LOPA-PCB INLA-PCB                     
014900                           INLD-PCB                                       
014910                           INLC-PCB                                       
014920                           ALT-INLC-PCB                                   
014930                           INLB1-PCB                                      
014940                           INLG-PCB KVAE-PCB                              
014950                           KVABSEQ-PCB                                    
014960                           WDK6-PCB WDB6-PCB WDD3-PCB                     
014970                           ADR-INLA-PCB                                   
014980                           ADR-INLC-PCB                                   
014990                           ADR-PLAA-PCB                                   
014991                           ADR-WDK6-PCB                                   
014992                           ADR-STYR-HANA-PCB                              
014993                           ADR-STYR-PLAA-PCB                              
014994                           KOM-KOMA-PCB                                   
014995                           PMRK-INLB-PCB PMRK-INLC-PCB                    
014996                           PMRK-PLAA-PCB                                  
014997                           STYR-HANA-PCB                                  
014998                           STYR-PLAA-PCB                                  
014999                           UPFA-PCB                                       
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
022000     MOVE 'CARPARTS.NDC.CASESURVEYBATCH'                                  
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
