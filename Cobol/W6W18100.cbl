001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6W18100.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   12/05/07.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       CARPARTS.NDC.PREPACKAGING                                
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        THIS IS A DRIVER PGM FOR TRANSACTION W6W181T/U                   
002200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED TO BEING              
002300*        CALLED VIA IMS-CONNECT AND CALLS SUBPROGRAM W6018110             
002400*        WHICH CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.          
002500*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSACTION: W6W181T                                             
003000*        REQUEST:     W60181I1                                            
003100*                                                                         
003200*    OUTDATA.                                                             
003300*        RESPONSE:    W60181O1                                            
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
005000 77  IDPGM                       PIC X(08)   VALUE 'W6W18100'.            
005100                                                                          
005200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005400 77  KDRC-DISPLAY                PIC Z(5).                                
005500                                                                          
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005710 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
005800                                                                          
006900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007000 01  GENERAL-SUBPROGRAMS.                                                 
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007400     03  W6018110                PIC X(8)    VALUE 'W6018110'.            
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
010000*    03  -COPY W60181I1                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010300     SKIP3                                                                
010400 01  RESP-AREA.                                                           
010500*    03  -COPY WZ01RESP                                                   
010600*    03  -COPY W60181O1                                                   
010700     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013800*01  -COPY W0009  -PRE MSG-                                               
013810     EJECT                                                                
013820*01  -COPY W0009  -PRE DISP-                                              
013830     EJECT                                                                
013870*01  -COPY W0008  -PRE ARTC-                                              
013880     05  FILLER                  PIC X.                                   
013890                                                                          
013891*01  -COPY W0008  -PRE BENA-                                              
013892     05  FILLER                  PIC X.                                   
013893     EJECT                                                                
013894*01  -COPY W0008  -PRE XXBI-                                              
013895     05 FILLER                   PIC X.                                   
013896     EJECT                                                                
013897*01  -COPY W0008  -PRE WDT3-                                              
013898     05 FILLER                   PIC X.                                   
013899                                                                          
013900*01  -COPY W0008  -PRE WDK7-                                              
013901     05 FILLER                   PIC X.                                   
       01 WDB6-PCB                     PIC X.                                   
013902                                                                          
013903*    PCB'ER FÖR SUBPGM                                                    
013904                                                                          
013910 01 KOM-KOMA-PCB                 PIC X.                                   
014000     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB ARTC-PCB                      
014002                           BENA-PCB XXBI-PCB WDT3-PCB WDK7-PCB            
                                 WDB6-PCB                                       
014003                           KOM-KOMA-PCB.                                  
014004 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB ARTC-PCB                      
014020                           BENA-PCB XXBI-PCB WDT3-PCB WDK7-PCB            
                                 WDB6-PCB                                       
014030                           KOM-KOMA-PCB.                                  
014100                                                                          
016010     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016020     IF SUB-KDRC = 0                                                      
016030       PERFORM A-INIT                                                     
016040       CALL W6018110 USING REQU-AREA RESP-AREA MAX-KVRADER                
016050                           MSG-PCB DISP-PCB  ARTC-PCB  BENA-PCB           
016060                           XXBI-PCB WDT3-PCB WDK7-PCB WDB6-PCB            
016061                           KOM-KOMA-PCB                                   
016070       PERFORM S02-RETURN-RESPONSE                                        
016080     END-IF                                                               
016090                                                                          
016091     PERFORM Z-FINIT                                                      
016092     MOVE ZERO TO RETURN-CODE                                             
016093     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016500     CONTINUE                                                             
017100     .                                                                    
017200     EJECT                                                                
020800 Z-FINIT SECTION.                                                         
020900     CONTINUE                                                             
021400     .                                                                    
021500     EJECT                                                                
021600*    --- DISPATCHER SECTIONS                                              
021610 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
021620                                                                          
021630     MOVE 'GETARG'                 TO SUB-KDFUNC                          
021640     MOVE 'CARPARTS.NDC.PREPACKAGING' TO SUB-ADDISPABS                    
021650     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
021660                                                                          
021670     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
021680                                                                          
021690     IF SUB-KDRC > 0                                                      
021691       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021692       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
021693       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021694       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021695     END-IF                                                               
021696     .                                                                    
021697     SKIP3                                                                
021698 S02-RETURN-RESPONSE SECTION.                                             
021699                                                                          
021700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021702     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
021703           (500 - RESP-KVRADER) * LENGTH OF RESP-LINE                     
021704                                                                          
021705     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021706                                                                          
021707     IF SUB-KDRC > 0                                                      
021708       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021709       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
021710       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021711       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021712     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
