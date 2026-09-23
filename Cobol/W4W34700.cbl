000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W34700.                                                
000300 AUTHOR.         LOVISH KUMAR.                                            
000400 DATE-WRITTEN.   20/04/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.CHANGETRANSPORTID                           
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W347T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W4034710 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W4034700 (TRANSACTION W4T347)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4W347T/U                                           
002100*        REQUEST:     W40347I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W40347O1                                            
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
003800 77  IDPGM                       PIC X(08)   VALUE 'W4W34700'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004600                                                                          
004700     EJECT                                                                
004800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004900 01  GENERAL-SUBPROGRAMS.                                                 
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005200     03  W4034710                PIC X(8)    VALUE 'W4034710'.            
005300     SKIP3                                                                
005400*    --- PARAMETERS TO ABEND                                              
005500                                                                          
005600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005900     EJECT                                                                
006000*                                                                         
006100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006200     SKIP3                                                                
006300*01  -COPY WZ01SUB                                                        
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006600     SKIP3                                                                
006700 01  REQU-AREA.                                                           
006800*    03  -COPY WZ01REQU                                                   
006900*    03  -COPY W40347I1                                                   
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007200     SKIP3                                                                
007300 01  RESP-AREA.                                                           
007400*    03  -COPY WZ01RESP                                                   
007500*    03  -COPY W40347O1                                                   
007600     EJECT                                                                
007700 LINKAGE SECTION.                                                         
007800                                                                          
007900*01  -COPY W0009     -PRE MSG-                                            
008000     EJECT                                                                
008100*01  -COPY W0008     -PRE USEA-                                           
008200     05  FILLER                  PIC X.                                   
008300     EJECT                                                                
008400 01  WDE4A-PCB       PIC X.                                               
008500 01  WDE43-PCB       PIC X.                                               
008600 01  WDE6-PCB        PIC X.                                               
008700 01  XXDM-PCB        PIC X.                                               
008800                                                                          
008900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDE4A-PCB WDE43-PCB            
009000                                           WDE6-PCB XXDM-PCB.             
009100 MAIN SECTION.                                                            
009200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE4A-PCB WDE43-PCB           
009300                                            WDE6-PCB XXDM-PCB.            
009400                                                                          
009500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009600     IF SUB-KDRC = 0                                                      
009700       PERFORM A-INIT                                                     
009800                                                                          
009900       CALL W4034710 USING REQU-AREA RESP-AREA MAX-KVRADER                
010000                           WDE4A-PCB WDE43-PCB WDE6-PCB XXDM-PCB          
010100       PERFORM S02-RETURN-RESPONSE                                        
010200     END-IF                                                               
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500     MOVE ZERO TO RETURN-CODE                                             
010600     GOBACK                                                               
010700     .                                                                    
010800     EJECT                                                                
010900 A-INIT SECTION.                                                          
011000     CONTINUE                                                             
011100     .                                                                    
011200     EJECT                                                                
011300 Z-FINIT SECTION.                                                         
011400     CONTINUE                                                             
011500     .                                                                    
011600     EJECT                                                                
011700*    --- DISPATCHER SECTIONS                                              
011800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
011900                                                                          
012000     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012100     MOVE 'CARPARTS.LDC.CHANGETRANSPORTID' TO SUB-ADDISPABS               
012200     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
012300                                                                          
012400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
012500                                                                          
012600     IF SUB-KDRC > 0                                                      
012700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
012800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013100     END-IF                                                               
013200     .                                                                    
013300     SKIP3                                                                
013400 S02-RETURN-RESPONSE SECTION.                                             
013500                                                                          
013600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
013700*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
013800     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
013900           (500 - RESP-KVRADER) * LENGTH OF RESP-RAD                      
014000                                                                          
014100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014200                                                                          
014300     IF SUB-KDRC > 0                                                      
014400       DISPLAY 'OUTERROR'                                                 
014500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
014700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014900     END-IF                                                               
015000     .                                                                    
015100     EJECT                                                                
