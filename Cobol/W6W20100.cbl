000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W20100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/04/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       TEST                                                     
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W201T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6020110 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W201T                                             
001800*        REQUEST:     W60201I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60201O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W20100'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004400                                                                          
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004900     03  W6020110                PIC X(8)    VALUE 'W6020110'.            
005000     SKIP3                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP3                                                                
005700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
005800     SKIP3                                                                
005900*01  -COPY WZ01SUB                                                        
006000     EJECT                                                                
006100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006200     SKIP3                                                                
006300 01  REQU-AREA.                                                           
006400*    03  -COPY WZ01REQU                                                   
006500*    03  -COPY W60201I1                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006800     SKIP3                                                                
006900 01  RESP-AREA.                                                           
007000*    03  -COPY WZ01RESP                                                   
007100*    03  -COPY W60201O1                                                   
007200     EJECT                                                                
007300 LINKAGE SECTION.                                                         
007400*01  -COPY W0009  -PRE MSG-                                               
007500     EJECT                                                                
007810*01  -COPY W0008  -PRE KVAE-                                              
007820     05  FILLER                  PIC X.                                   
007830     EJECT                                                                
007900*01  -COPY W0008  -PRE KVAF-                                              
008000     05  FILLER                  PIC X.                                   
008100     EJECT                                                                
008200*01  -COPY W0008  -PRE KVAG-                                              
008300     05  FILLER                  PIC X.                                   
008400     EJECT                                                                
008500*01  -COPY W0008  -PRE BENA-                                              
008600     05  FILLER                  PIC X.                                   
008700     EJECT                                                                
008800*01  -COPY W0008  -PRE WDP3-                                              
008900     05  WDP3-KEY-FB-AREA-KDARBTYP      PIC X(8).                         
009000     05  WDP3-KEY-FB-AREA-IDPERSON      PIC S9(3) COMP-3.                 
009100     EJECT                                                                
009200*01  -COPY W0008  -PRE WDK6-                                              
009300     05  FILLER                  PIC X.                                   
009400     EJECT                                                                
009410*01  -COPY W0008  -PRE WDK7-                                              
009420     05  FILLER                  PIC X.                                   
009430     EJECT                                                                
009410*01  -COPY W0008  -PRE WDB6-                                              
009420     05  FILLER                  PIC X.                                   
009430     EJECT                                                                
009500                                                                          
009600 PROCEDURE DIVISION  USING MSG-PCB KVAE-PCB KVAF-PCB                      
009700                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
009710                           WDK7-PCB WDB6-PCB.                             
009800                                                                          
009900 MAIN SECTION.                                                            
010000     ENTRY 'DLITCBL' USING MSG-PCB KVAE-PCB KVAF-PCB                      
010100                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
010110                           WDK7-PCB WDB6-PCB.                             
010200                                                                          
010300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010400     IF SUB-KDRC = 0                                                      
010500       PERFORM A-INIT                                                     
010600       CALL W6020110 USING REQU-AREA RESP-AREA MAX-KVRADER                
010700                           KVAE-PCB KVAF-PCB                              
010800                           KVAG-PCB BENA-PCB WDP3-PCB WDK6-PCB            
010810                           WDK7-PCB WDB6-PCB                              
010900       PERFORM S02-RETURN-RESPONSE                                        
011000     END-IF                                                               
011100                                                                          
011200     PERFORM Z-FINIT                                                      
011300     MOVE ZERO TO RETURN-CODE                                             
011400     GOBACK                                                               
011500     .                                                                    
011600     EJECT                                                                
011700 A-INIT SECTION.                                                          
011800     CONTINUE                                                             
011900     .                                                                    
012000     EJECT                                                                
012100 Z-FINIT SECTION.                                                         
012200     CONTINUE                                                             
012300     .                                                                    
012400     EJECT                                                                
012500*    --- DISPATCHER SECTIONS                                              
012600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012700                                                                          
012800     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012900     MOVE 'CARPARTS.NDC.IRINQUIRY' TO SUB-ADDISPABS                       
013000     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
013100                                                                          
013200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013300                                                                          
013400     IF SUB-KDRC > 0                                                      
013500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013900     END-IF                                                               
014000     .                                                                    
014100     SKIP3                                                                
014200 S02-RETURN-RESPONSE SECTION.                                             
014300                                                                          
014400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014500*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014600     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
014700           (500 - RESP-KVRADER) * LENGTH OF RESP-LINE                     
014800                                                                          
014900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
015000                                                                          
015100     IF SUB-KDRC > 0                                                      
015200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015600     END-IF                                                               
015700     .                                                                    
015800     EJECT                                                                
