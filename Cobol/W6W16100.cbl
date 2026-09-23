000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6W16100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   12/07/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.HANDLINGCODEREG                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W6W161T/U                   
001100*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001200*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6016110 WHICH              
001300*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6W161T                                             
001800*        REQUEST:     W60161I1                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        RESPONSE:    W60161O1                                            
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
003500 77  IDPGM                       PIC X(08)   VALUE 'W6W16100'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300 77  MAX-KVRADER                 PIC S9(4)   VALUE +1 COMP.               
004400                                                                          
004500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
004900     03  W6016110                PIC X(8)    VALUE 'W6016110'.            
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
006500*    03  -COPY W60161I1                                                   
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
006800     SKIP3                                                                
006900 01  RESP-AREA.                                                           
007000*    03  -COPY WZ01RESP                                                   
007100*    03  -COPY W60161O1                                                   
007200     EJECT                                                                
007300 LINKAGE SECTION.                                                         
007400*01      -COPY W0009     -PRE MSG-                                        
007500     EJECT                                                                
007600*01      -COPY W0009     -PRE DISP-                                       
007700     EJECT                                                                
007800*01      -COPY W0008     -PRE ARTC-                                       
007900      05 FILLER              PIC X.                                       
008000     EJECT                                                                
008100*    PCB'ER FÖR SUBPGM                                                    
008300 01 KOM-KOMA-PCB         PIC X.                                           
008400     EJECT                                                                
008510*01      -COPY W0008     -PRE WDK7-                                       
008520      05 FILLER              PIC X.                                       
008530     EJECT                                                                
008531*01      -COPY W0008     -PRE WDT5-                                       
008532      05 FILLER              PIC X.                                       
008533     EJECT                                                                
008534 01  W005K7-WDB6-PCB     PIC X.                                           
008535 01  W005K7-WDK6-PCB     PIC X.                                           
008536 01  W005K7-WDK7-PCB     PIC X.                                           
008540     EJECT                                                                
008600 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB         
008700                           WDK7-PCB WDT5-PCB W005K7-WDB6-PCB              
008710                           W005K7-WDK6-PCB W005K7-WDK7-PCB.               
008800 MAIN SECTION.                                                            
008900     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB         
008901                           WDK7-PCB WDT5-PCB W005K7-WDB6-PCB              
008902                           W005K7-WDK6-PCB W005K7-WDK7-PCB.               
009000                                                                          
009100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009200     IF SUB-KDRC = 0                                                      
009300       PERFORM A-INIT                                                     
009400       CALL W6016110 USING REQU-AREA RESP-AREA MAX-KVRADER                
009500                           MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB         
009510                           WDK7-PCB WDT5-PCB W005K7-WDB6-PCB              
009520                           W005K7-WDK6-PCB W005K7-WDK7-PCB                
009600       PERFORM S02-RETURN-RESPONSE                                        
009700     END-IF                                                               
009800                                                                          
009900     MOVE ZERO TO RETURN-CODE                                             
010000     GOBACK                                                               
010100     .                                                                    
010200     EJECT                                                                
010300 A-INIT SECTION.                                                          
010400     MOVE 1 TO REQU-KVRADER                                               
010500     .                                                                    
010600     EJECT                                                                
010700*    --- DISPATCHER SECTIONS                                              
010800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
010900                                                                          
011000     MOVE 'GETARG'                 TO SUB-KDFUNC                          
011100     MOVE 'CARPARTS.NDC.HANDLINGCODEREG'                                  
011200                                   TO SUB-ADDISPABS                       
011300     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
011400                                                                          
011500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
011600                                                                          
011700     IF SUB-KDRC > 0                                                      
011800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
011900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
012200     END-IF                                                               
012300     .                                                                    
012400     SKIP3                                                                
012500 S02-RETURN-RESPONSE SECTION.                                             
012600                                                                          
012700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
012800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
012900                                                                          
013000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
013100                                                                          
013200     IF SUB-KDRC > 0                                                      
013300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
013500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013700     END-IF                                                               
013800     .                                                                    
013900     EJECT                                                                
