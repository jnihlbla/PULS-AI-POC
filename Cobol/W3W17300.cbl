000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3W17300.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   2012-10-22.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CARPARTS.NDC.COREINSPECTION                              
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THIS IS A DRIVER PGM FOR TRANSACTION W3W172T                     
001200*                                                                         
001300*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001400*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3017310 WHICH              
001500*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001600*                                                                         
001700*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001800*        MINAL EXISTS - W3017300 (TRANSACTION W3T173)                     
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W3W173T                                             
002200*        REQUEST:     W30173I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    W30173O1                                            
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
003900 77  IDPGM                       PIC X(08)   VALUE 'W3W17300'.            
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
005300     03  W3017310                PIC X(8)    VALUE 'W3017310'.            
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
007000*    03  -COPY W30173I1                                                   
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007300                                                                          
007400 01  RESP-AREA.                                                           
007500*    03  -COPY WZ01RESP                                                   
007600*    03  -COPY W30173O1                                                   
007700                                                                          
007800     EJECT                                                                
007900 LINKAGE SECTION.                                                         
008000                                                                          
008100*01  -COPY W0009   -PRE MSG-                                              
008200 01  DISTRDOC-PCB                PIC X.                                   
008400 01  BENA-PCB                    PIC X.                                   
008500 01  ARTC-PCB                    PIC X.                                   
008600 01  WDK7-PCB                    PIC X.                                   
008600 01  WDB6-PCB                    PIC X.                                   
008700                                                                          
008800     EJECT                                                                
008900 PROCEDURE DIVISION  USING MSG-PCB  DISTRDOC-PCB                          
009000                           BENA-PCB ARTC-PCB WDK7-PCB                     
009100                           WDB6-PCB.                                      
009200                                                                          
009300     ENTRY 'DLITCBL' USING MSG-PCB  DISTRDOC-PCB                          
009400                           BENA-PCB ARTC-PCB WDK7-PCB                     
009500                           WDB6-PCB.                                      
009600                                                                          
009700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
009800     IF SUB-KDRC = 0                                                      
009900                                                                          
010000       CALL W3017310 USING                                                
010100            REQU-AREA RESP-AREA MAX-KVRADER DISTRDOC-PCB                  
010200            BENA-PCB ARTC-PCB WDK7-PCB WDB6-PCB                           
010300                                                                          
010400       PERFORM S02-RETURN-RESPONSE                                        
010500     END-IF                                                               
010600                                                                          
010700     MOVE ZERO TO RETURN-CODE                                             
010800     GOBACK                                                               
010900     .                                                                    
011000     EJECT                                                                
011100*    --- DISPATCHER SECTIONS                                              
011200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
011300                                                                          
011400     MOVE 'GETARG'               TO SUB-KDFUNC                            
011500     MOVE 'CARPARTS.NDC.COREINSPECTION'  TO SUB-ADDISPABS                 
011600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
011700                                                                          
011800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
011900                                                                          
012000     IF SUB-KDRC > 0                                                      
012100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
012200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
012300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
012400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
012500     END-IF                                                               
012600     .                                                                    
012700     SKIP3                                                                
012800 S02-RETURN-RESPONSE SECTION.                                             
012900                                                                          
013000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
013100*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
013200     MOVE RESP-KVRADER TO W-KVRADER                                       
013300     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
013400     (MAX-KVRADER - W-KVRADER) * LENGTH OF RESP-IDARTNR                   
013500                                                                          
013600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
013700                                                                          
013800     IF SUB-KDRC > 0                                                      
013900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
014000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
014100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
014200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014300     END-IF                                                               
014400     .                                                                    
014500     EJECT                                                                
