000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W41100.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   2012-10-22.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.GOODSRECEIVINGINFO                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W411T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3017210 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W3017200 (TRANSACTION W3T172)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4W411T                                             
002100*        REQUEST:     W40411I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W40411O1                                            
002500                                                                          
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4W41100'.            
003900                                                                          
004000*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
005100                                                                          
005200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005300 01  GENERAL-SUBPROGRAMS.                                                 
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005604     03  W4041110                PIC X(8)    VALUE 'W4041110'.            
005700                                                                          
005800*    --- PARAMETERS TO ABEND                                              
005900                                                                          
006000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006300     EJECT                                                                
006400*                                                                         
006500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006600                                                                          
006700*01  -COPY WZ01SUB                                                        
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007000                                                                          
007100 01  REQU-AREA.                                                           
007200*    03  -COPY WZ01REQU                                                   
007301*    03  -COPY W40411I1                                                   
007400     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007600                                                                          
007700 01  RESP-AREA.                                                           
007800*    03  -COPY WZ01RESP                                                   
007901*    03  -COPY W40411O1                                                   
008000                                                                          
008100     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008303                                                                          
009203 01  MSG-PCB                     PIC X.                                   
009300 01  WDB2-PCB                    PIC X.                                   
010803                                                                          
010903     EJECT                                                                
011903 PROCEDURE DIVISION  USING MSG-PCB WDB2-PCB.                              
012402                                                                          
012502     ENTRY 'DLITCBL' USING MSG-PCB WDB2-PCB.                              
012900                                                                          
013000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013100     IF SUB-KDRC = 0                                                      
013202       CALL W4041110 USING REQU-AREA RESP-AREA                            
013402                  WDB2-PCB                                                
013700                                                                          
013800       PERFORM S02-RETURN-RESPONSE                                        
013900     END-IF                                                               
014000                                                                          
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014900     EJECT                                                                
015000*    --- DISPATCHER SECTIONS                                              
015100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
015200                                                                          
015300     MOVE 'GETARG'               TO SUB-KDFUNC                            
015402     MOVE 'CARPARTS.NDC.GOODSRECEIVINGINFO'                               
015410                                 TO SUB-ADDISPABS                         
015500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
015600                                                                          
015700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
015800                                                                          
015900     IF SUB-KDRC > 0                                                      
016000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
016100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
016200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
016300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
016400     END-IF                                                               
016500     .                                                                    
016600     SKIP3                                                                
016700 S02-RETURN-RESPONSE SECTION.                                             
016800                                                                          
016900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
017000*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
017200     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA                             
017400                                                                          
017500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
017600                                                                          
017700     IF SUB-KDRC > 0                                                      
017800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
017900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
018000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
018100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018200     END-IF                                                               
018300     .                                                                    
018400     EJECT                                                                
