000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3W17200.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   2012-10-22.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CARPARTS.NDC.CORETREATMENT                               
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THIS IS A DRIVER PGM FOR TRANSACTION W3W172T                     
001200*                                                                         
001300*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001400*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3017210 WHICH              
001500*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001600*                                                                         
001700*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001800*        MINAL EXISTS - W3017200 (TRANSACTION W3T172)                     
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W3W172T                                             
002200*        REQUEST:     W30172I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    W30172O1                                            
002600*                                                                         
002700*    CHANGE LOG:                                                          
002800*                                                                         
002900*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003000*      ----------------------------------------------------------         
003100*      15/02/25 - REDDY RAHUL     - PRINT CORE LABELS IN SITTARD          
003200*                                   E'TRACKER 10206694                    
003300*                                                                         
003400                                                                          
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W3W17200'.            
004000                                                                          
004100*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  MAX-KVRADER                 PIC S9(4)   VALUE +300 COMP.             
004600 77  W-KVRADER                   PIC S9(4)   COMP.                        
004700                                                                          
004800                                                                          
004900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005300     03  W3017210                PIC X(8)    VALUE 'W3017210'.            
005400                                                                          
005500*    --- PARAMETERS TO ABEND                                              
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     EJECT                                                                
006100*                                                                         
006200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006300                                                                          
006400*01  -COPY WZ01SUB                                                        
006500     EJECT                                                                
006600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
006700                                                                          
006800 01  REQU-AREA.                                                           
006900*    03  -COPY WZ01REQU                                                   
007000*    03  -COPY W30172I1                                                   
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007300                                                                          
007400 01  RESP-AREA.                                                           
007500*    03  -COPY WZ01RESP                                                   
007600*    03  -COPY W30172O1                                                   
007700                                                                          
007800     EJECT                                                                
007900 LINKAGE SECTION.                                                         
008000                                                                          
008100*01  -COPY W0009   -PRE MSG-                                              
008200 01  DISTRDOC-PCB                PIC X.                                   
008300 01  USEA-PCB                    PIC X.                                   
008400 01  BYTF-PCB                    PIC X.                                   
008500 01  WDR2-PCB                    PIC X.                                   
008600 01  BENA-PCB                    PIC X.                                   
008700 01  WDK6-PCB                    PIC X.                                   
008800 01  BYTF2-PCB                   PIC X.                                   
008900 01  WDK7-PCB                    PIC X.                                   
009000 01  WDB6-PCB                    PIC X.                                   
009100                                                                          
009200     EJECT                                                                
009300 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB USEA-PCB                  
009400                  BYTF-PCB  WDR2-PCB BENA-PCB                             
009500                  WDK6-PCB BYTF2-PCB WDK7-PCB WDB6-PCB.                   
009600                                                                          
009700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB USEA-PCB                  
009800                  BYTF-PCB  WDR2-PCB BENA-PCB                             
009900                  WDK6-PCB BYTF2-PCB WDK7-PCB WDB6-PCB.                   
010000                                                                          
010100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
010200     IF SUB-KDRC = 0                                                      
010300       CALL W3017210 USING REQU-AREA RESP-AREA MAX-KVRADER                
010400                MSG-PCB  DISTRDOC-PCB USEA-PCB  BYTF-PCB WDR2-PCB         
010500                BENA-PCB WDK6-PCB     BYTF2-PCB WDK7-PCB WDB6-PCB         
010600                                                                          
010700       PERFORM S02-RETURN-RESPONSE                                        
010800     END-IF                                                               
010900                                                                          
011000     MOVE ZERO TO RETURN-CODE                                             
011100     GOBACK                                                               
011200     .                                                                    
011300     EJECT                                                                
011400*    --- DISPATCHER SECTIONS                                              
011500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
011600                                                                          
011700     MOVE 'GETARG'               TO SUB-KDFUNC                            
011800     MOVE 'CARPARTS.NDC.CORETREATMENT'  TO SUB-ADDISPABS                  
011900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
012000                                                                          
012100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
012200                                                                          
012300     IF SUB-KDRC > 0                                                      
012400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
012500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
012800     END-IF                                                               
012900     .                                                                    
013000     SKIP3                                                                
013100 S02-RETURN-RESPONSE SECTION.                                             
013200                                                                          
013300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
013400*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
013500     MOVE RESP-KVRADER TO W-KVRADER                                       
013600     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
013700     (MAX-KVRADER - W-KVRADER) * LENGTH OF RESP-RADINFO                   
013800                                                                          
013900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
014000                                                                          
014100     IF SUB-KDRC > 0                                                      
014200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
014400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014600     END-IF                                                               
014700     .                                                                    
014800     EJECT                                                                
