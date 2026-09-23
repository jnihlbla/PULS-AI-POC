000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3W18300.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   14/04/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.CORECREATESHIPMENT2                         
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W3W183T/U                   
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3018310 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W3018300 (TRANSACTION W3T183)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W3W183T/U                                           
002100*        REQUEST:     W30183I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W30183O1                                            
002500*                                                                         
002600*    CHANGE LOG:                                                          
002700*      14/04/22 - REDDY RAHUL     - INITIAL VERSION.                      
002800*                                   TORONTO CHANGES.                      
002900*                                   ETRACKER 10228590.                    
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400 77  IDPGM                       PIC X(08)   VALUE 'W3W18300'.            
004500                                                                          
004600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900                                                                          
005000 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
005100                                                                          
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  W3018310                PIC X(8)    VALUE 'W3018310'.            
005800     SKIP3                                                                
005900*    --- PARAMETERS TO ABEND                                              
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006400     EJECT                                                                
006500*                                                                         
006600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006700     SKIP3                                                                
006800*01  -COPY WZ01SUB                                                        
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007100     SKIP3                                                                
007200 01  REQU-AREA.                                                           
007300*    03  -COPY WZ01REQU                                                   
007400*    03  -COPY W30183I1                                                   
007500     EJECT                                                                
007600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007700     SKIP3                                                                
007800 01  RESP-AREA.                                                           
007900*    03  -COPY WZ01RESP                                                   
008000*    03  -COPY W30183O1                                                   
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300                                                                          
008400*01  -COPY W0009   -PRE MSG-                                              
008410*01  -COPY W0009   -PRE ALT-                                              
008500     EJECT                                                                
008600 01  USEA-PCB                PIC X.                                       
008700 01  BENA-PCB                PIC X.                                       
008800 01  XXLD-PCB                PIC X.                                       
008900 01  ARTS-PCB                PIC X.                                       
009000 01  3165-PCB                PIC X.                                       
009100 01  LOGA-PCB                PIC X.                                       
009200 01  ARTC-PCB                PIC X.                                       
009200 01  WDB6-PCB                PIC X.                                       
009300     EJECT                                                                
009500 PROCEDURE DIVISION  USING MSG-PCB   ALT-PCB   BENA-PCB  XXLD-PCB         
009510                           ARTS-PCB  3165-PCB  LOGA-PCB  ARTC-PCB         
009600                           WDB6-PCB.                                      
009800     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB   BENA-PCB  XXLD-PCB         
009810                           ARTS-PCB  3165-PCB  LOGA-PCB  ARTC-PCB         
009900                           WDB6-PCB.                                      
010000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010100     IF SUB-KDRC = 0                                                      
010200       PERFORM A-INIT                                                     
010300                                                                          
010400       CALL W3018310 USING REQU-AREA RESP-AREA MAX-KVRADER                
010600                           ALT-PCB   BENA-PCB  XXLD-PCB                   
010610                           ARTS-PCB  3165-PCB  LOGA-PCB  ARTC-PCB         
                                 WDB6-PCB                                       
010700       PERFORM S02-RETURN-RESPONSE                                        
010800     END-IF                                                               
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600     CONTINUE                                                             
011700     .                                                                    
011800     EJECT                                                                
011900 Z-FINIT SECTION.                                                         
012000     CONTINUE                                                             
012100     .                                                                    
012200     EJECT                                                                
012300*    --- DISPATCHER SECTIONS                                              
012400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
012500                                                                          
012600     MOVE 'GETARG'                 TO SUB-KDFUNC                          
012700     MOVE 'CARPARTS.NDC.CORECREATESHIPMENT2' TO SUB-ADDISPABS             
012800     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
012900                                                                          
013000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
013100                                                                          
013200     IF SUB-KDRC > 0                                                      
013300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
013400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
013500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
013600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
013700     END-IF                                                               
013800     .                                                                    
013900     SKIP3                                                                
014000 S02-RETURN-RESPONSE SECTION.                                             
014100                                                                          
014200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
014300*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
014400     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
014500           (500 - RESP-KVRADER) * LENGTH OF RESP-PROFORMA-RAD             
014600                                                                          
014700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014800                                                                          
014900     IF SUB-KDRC > 0                                                      
015000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
015100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
015200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015400     END-IF                                                               
015500     .                                                                    
015600     EJECT                                                                
