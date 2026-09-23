000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W10900.                                                
000300 AUTHOR.         RAHUL JAIN.                                              
000400 DATE-WRITTEN.   JUL 2012.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.RECEIVINGREPORT.QUERY                       
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W109T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6010910 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W109T                                             
001800*        REQUEST:     W60109I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60109O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W10900'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004500 01  GENERAL-SUBPROGRAMS.                                                 
004600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004800     03  W6010910                PIC X(8)    VALUE 'W6010910'.            
004900     SKIP3                                                                
005000*    --- PARAMETERS TO ABEND                                              
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500     SKIP3                                                                
005600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005700     SKIP3                                                                
005800*01  -COPY WZ01SUB                                                        
005900     EJECT                                                                
006000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006100     SKIP3                                                                
006200 01  REQU-AREA.                                                           
006300*    03  -COPY WZ01REQU                                                   
006400*    03  -COPY W60109I1                                                   
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006700     SKIP3                                                                
006800 01  RESP-AREA.                                                           
006900*    03  -COPY WZ01RESP                                                   
007000*    03  -COPY W60109O1                                                   
007100     EJECT                                                                
007200 LINKAGE SECTION.                                                         
007300*01  -COPY W0009  -PRE MSG-                                               
007400     EJECT                                                                
007500 01  INLA-PCB                    PIC X.                                   
007600 01  INLC-PCB                    PIC X.                                   
007700 01  INLD-PCB                    PIC X.                                   
007800 01  PLAA-PCB                    PIC X.                                   
007900 01  ARTC-PCB                    PIC X.                                   
008000 01  ARTD-PCB                    PIC X.                                   
008100 01  WDF5-PCB                    PIC X.                                   
008200 01  WDK7-PCB                    PIC X.                                   
008300 01  WDB6-PCB                    PIC X.                                   
008400 01  BENA-PCB                    PIC X.                                   
008500 01  ADR-INLA-PCB                PIC X.                                   
008600 01  ADR-INLC-PCB                PIC X.                                   
008700 01  ADR-PLAA-PCB                PIC X.                                   
008800 01  ADR-WDK6-PCB                PIC X.                                   
008900 01  STYR-HANA-PCB               PIC X.                                   
009000 01  STYR-PLAA-PCB               PIC X.                                   
009100     EJECT                                                                
009200                                                                          
009300 PROCEDURE DIVISION  USING MSG-PCB INLA-PCB INLC-PCB INLD-PCB             
009400                 PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB WDK7-PCB             
009500                 WDB6-PCB BENA-PCB                                        
009600          ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB             
009700                 STYR-HANA-PCB STYR-PLAA-PCB.                             
009800                                                                          
009900 MAIN SECTION.                                                            
010000     ENTRY 'DLITCBL' USING MSG-PCB INLA-PCB INLC-PCB INLD-PCB             
010100                 PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB WDK7-PCB             
010200                 WDB6-PCB BENA-PCB                                        
010300          ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB             
010400                 STYR-HANA-PCB STYR-PLAA-PCB.                             
010500                                                                          
010600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010700     IF SUB-KDRC = 0                                                      
010800       PERFORM A-INIT                                                     
010900       CALL W6010910 USING REQU-AREA RESP-AREA                            
011000                           INLA-PCB INLC-PCB INLD-PCB                     
011100                           PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB            
011200                           WDK7-PCB WDB6-PCB BENA-PCB                     
011300                           ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB         
011400                           ADR-WDK6-PCB STYR-HANA-PCB                     
011500                           STYR-PLAA-PCB                                  
011600                                                                          
011700       PERFORM S02-RETURN-RESPONSE                                        
011800     END-IF                                                               
011900                                                                          
012000     PERFORM Z-FINIT                                                      
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-INIT SECTION.                                                          
012600     CONTINUE                                                             
012700     .                                                                    
012800     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013000     CONTINUE                                                             
013100     .                                                                    
013200     EJECT                                                                
013300*    --- DISPATCHER SECTIONS                                              
013400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
013500                                                                          
013600     MOVE 'GETARG'                 TO SUB-KDFUNC                          
013700     MOVE 'CARPARTS.NDC.RECEIVINGREPORT.QUERY'                            
013800                                   TO SUB-ADDISPABS                       
013900     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
014000                                                                          
014100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
014200                                                                          
014300     IF SUB-KDRC > 0                                                      
014400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
014600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014800     END-IF                                                               
014900     .                                                                    
015000     SKIP3                                                                
015100 S02-RETURN-RESPONSE SECTION.                                             
015200                                                                          
015300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
015400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
015500                                                                          
015600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015700                                                                          
015800     IF SUB-KDRC > 0                                                      
015900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
016100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
