000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ141100.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   11/03/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*      GENERAL PROGRAM TO SEND A REPORT FILE                              
001000*      TO DISTRIBUTION & PRINT                                            
001100*                                                                         
001200*      THE FILE CONTAINS SEVERAL REPORTS SEPARATED                        
001300*      BY LEADING HEADER RECORDS THAT IDENTIFY                            
001400*      WHICH D&P ADDRESS SHOULD BE USED AS A DESTINATION                  
001500*      FOR THE FOLLOWING REPORT.                                          
001600*      THERE SHOULD BE 1-3 HEADER RECORDS BEFORE EACH REPORT              
001700*      STARTING WITH THE STRING "¤DAP" FOLLOWED BY:                       
001800*        RCD 1: REPORT TYPE (OUTPUT TYPE) - MUST BE SPECIFIED             
001900*        RCD 2: SUB TYPE (OUTPUT RECEIVER) - OPTIONAL, DFLT=SPACE         
002000*        RCD 3: LIST ID - OPTIONAL, DEFAULT IS YYMMDDHHMMSS               
002100*                                                                         
002200*      SYSIN CODE SAYS HOW TO HANDLE CONTROL CHAR FOR VBA FILES           
002300*                                                                         
002400*        0 OR NONE - DEFAULT. FIRST POS IS PART OF DATA                   
002500*        1         - SKIP FIRST POS IN THE DATA.                          
002600*                    TO BE USED ONLY FOR VBA FILES.                       
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- INPUT DATA                                                 
003700     SELECT INDATA                     ASSIGN TO WZ1411D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  INDATA                                                               
004400     RECORDING       V                                                    
004500     RECORD IS VARYING FROM 1 TO 3000 CHARACTERS                          
004600            DEPENDING ON IN-KVDLEN                                        
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  FILLER                     PIC X(3000).                              
005000                                                                          
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'WZ141100'.            
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005700                                                                          
005800*    -- PREFIX THAT IDENTIFIES CONTROL RECORDS                            
005900*    -- IN THE INPUT FILE                                                 
006000 77  DAP-PREFIX                  PIC X(4)    VALUE '¤DAP'.                
006100     SKIP2                                                                
006200 01  ERRTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500 77  KDRC-DISPLAY                PIC Z(3)9.                               
006600                                                                          
006700 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-INDATA                       VALUE 'Y'.                   
006900                                                                          
007000 77  DAP-ADDRESS                 PIC X(50)   VALUE                        
007100                                 'CARPARTS.DAP.DISTRDOC'.                 
007200                                                                          
007300     EJECT                                                                
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007900     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
008000     SKIP3                                                                
008100*    --- PARAMETERS TO ABEND                                              
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008600     EJECT                                                                
008700*                                                                         
008800*    --- PARAMETERS TO POSTSUM                                            
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100*                                                                         
009200*    --- PARAMETERS TO W009WAIT                                           
009300 77  5-SECONDS                   PIC S9(9)   COMP VALUE +500.             
009400     EJECT                                                                
009500*                                                                         
009600 01  WS-KVDLEN                   PIC 9(9)   BINARY.                       
009700                                                                          
009800 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
009900                                                                          
010000 01  IN-KVDLEN                   PIC 9(9)   BINARY.                       
010100                                                                          
010200 01  IN-AREA                     PIC X(3000).                             
010300*    -- THE ¤DAP PREFIX MAY BE IN POS 1 OR 2                              
010400 01  FILLER REDEFINES IN-AREA.                                            
010500     03 FILLER                   PIC X(1).                                
010600     03 IN-PREFIX                PIC X(4).                                
010700     03 IN-HDR-DATA              PIC X(30).                               
010800 01  FILLER REDEFINES IN-AREA.                                            
010900     03 IN-PREFIX2               PIC X(4).                                
011000     03 IN-HDR-DATA2             PIC X(30).                               
011100                                                                          
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
011400     SKIP3                                                                
011500 01  HDR-KVDLEN                  PIC 9(9)   BINARY.                       
011600                                                                          
011700 01  HDR-AREA.                                                            
011800     03 -COPY WZ01REQU -PRE HDR-                                          
011900     03 -COPY WZ04HDR                                                     
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
012200     SKIP3                                                                
012300 01  -COPY WZ01SEND                                                       
012400                                                                          
012500     EJECT                                                                
012600 01  SEND-AREA                   PIC X(3000).                             
012700                                                                          
012800     EJECT                                                                
012900                                                                          
013000 01  SYSIN-AREA.                                                          
013100     03  SYSIN-CODE              PIC X.                                   
013200         88  SW-DEFAULT                      VALUE '0'.                   
013300         88  SW-SKIP-ATTR                    VALUE '1'.                   
013400*SYSIN-CODE                                                               
013500*TELLS IF THE FIRST CHAR IS TO BE IGNORED.                                
013600*SHOULD BE USED ONLY FOR VBA FILES.                                       
013700*    DEFAULT VALUE 0 -> CONTROL CHAR (FIRST CHAR) IS NOT SKIPPED          
013800*            VALUE 1 -> CONTROL CHAR (FIRST CHAR) SKIPPED                 
013900     EJECT                                                                
014000 PROCEDURE DIVISION.                                                      
014100 MAIN SECTION.                                                            
014200                                                                          
014300     PERFORM A-INIT                                                       
014400     PERFORM S01-READ-INDATA                                              
014500                                                                          
014600*    - THE ¤DAP PREFIX IS ALLOWED TO BE IN EITHER POS 1 OR 2              
014700*    - TO BETTER SUPPORT BOTH RECFM VB OR VBA.                            
014800*    - (ORIGINALLY IT WAS 2 FOR BOTH RECORD TYPES)                        
014900     PERFORM UNTIL END-OF-INDATA                                          
015000     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
015100                                                                          
015200       PERFORM S04-SEND-OPEN                                              
015300                                                                          
015400       PERFORM B-READ-AND-CHECK-HDR-RCDS                                  
015500       PERFORM S04-SEND-HEADER-RECORD                                     
015600                                                                          
015700       PERFORM UNTIL END-OF-INDATA                                        
015800       OR DAP-PREFIX = IN-PREFIX OR IN-PREFIX2                            
015900         PERFORM S04-SEND-MESSAGE-FROM-INAREA                             
016000         PERFORM S01-READ-INDATA                                          
016100       END-PERFORM                                                        
016200                                                                          
016300       PERFORM S04-SEND-CLOSE                                             
016400                                                                          
016500       IF DAP-PREFIX = IN-PREFIX OR IN-PREFIX2                            
016600         CALL W009WAIT USING 5-SECONDS                                    
016700       END-IF                                                             
016800     END-PERFORM                                                          
016900                                                                          
017000     IF NOT END-OF-INDATA                                                 
017100       STRING 'WZ1411  OUTPUT TYPE MISSING'                               
017200         DELIMITED BY SIZE INTO ERRTEXT                                   
017300       DISPLAY  ERRTEXT                                                   
017400       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
017500     END-IF                                                               
017600                                                                          
017700     PERFORM Z-FINIT                                                      
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     OPEN INPUT INDATA                                                    
018600                                                                          
018700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018800                                                                          
018900     ACCEPT SYSIN-AREA FROM SYSIN                                         
019000                                                                          
019100     MOVE '001'        TO HDR-REQU-IDMSGVER                               
019200     MOVE SPACE        TO HDR-REQU-KDPGMACT                               
019300     MOVE 'WZ1411'     TO HDR-REQU-IDUSER                                 
019400     MOVE DAP-ADDRESS  TO SEND-ADDISPABS                                  
019500     .                                                                    
019600     EJECT                                                                
019700 B-READ-AND-CHECK-HDR-RCDS  SECTION.                                      
019800                                                                          
019900*    -- FIRST HDR RECORD ALREADY READ                                     
020000*    -- IT CONTAINS THE OUTPUT TYPE                                       
020100     IF IN-PREFIX = DAP-PREFIX                                            
020200       MOVE IN-HDR-DATA   TO HDR-IDOUTTYPE                                
020300     ELSE                                                                 
020400       MOVE IN-HDR-DATA2  TO HDR-IDOUTTYPE                                
020500     END-IF                                                               
020600                                                                          
020700     PERFORM S01-READ-INDATA                                              
020800     IF END-OF-INDATA                                                     
020900     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
021000       MOVE SPACE       TO HDR-IDOUTREC                                   
021100     ELSE                                                                 
021200       IF IN-PREFIX = DAP-PREFIX                                          
021300         MOVE IN-HDR-DATA  TO HDR-IDOUTREC                                
021400       ELSE                                                               
021500         MOVE IN-HDR-DATA2 TO HDR-IDOUTREC                                
021600       END-IF                                                             
021700       PERFORM S01-READ-INDATA                                            
021800     END-IF                                                               
021900                                                                          
022000     IF END-OF-INDATA                                                     
022100     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
022200       MOVE FUNCTION CURRENT-DATE(3:12) TO HDR-IDLIST                     
022300     ELSE                                                                 
022400       IF IN-PREFIX = DAP-PREFIX                                          
022500         MOVE IN-HDR-DATA  TO HDR-IDLIST                                  
022600       ELSE                                                               
022700         MOVE IN-HDR-DATA2 TO HDR-IDLIST                                  
022800       END-IF                                                             
022900       PERFORM S01-READ-INDATA                                            
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 Z-FINIT SECTION.                                                         
023400                                                                          
023500     CLOSE INDATA                                                         
023600                                                                          
023700     SKIP2                                                                
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     .                                                                    
024100     EJECT                                                                
024200 S01-READ-INDATA  SECTION.                                                
024300     SKIP2                                                                
024400     READ INDATA INTO IN-AREA                                             
024500     AT END                                                               
024600        SET END-OF-INDATA TO TRUE                                         
024700                                                                          
024800     NOT AT END                                                           
024900        MOVE 'INDATA'   TO POSTSUM-FDNAMN                                 
025000        MOVE 'WZ1411D1' TO POSTSUM-DDNAMN2                                
025100        MOVE SPACE      TO POSTSUM-TRANSTYP                               
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025300     END-READ                                                             
025400     .                                                                    
025500     EJECT                                                                
025600 S04-SEND-OPEN SECTION.                                                   
025700                                                                          
025800     MOVE 'OPEN'                     TO SEND-KDFUNC                       
025900     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
026000                                                                          
026100     IF SEND-KDRC > 0                                                     
026200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
026300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
026400       DELIMITED BY SIZE INTO ERRTEXT                                     
026500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026600     END-IF                                                               
026700     .                                                                    
026800     SKIP3                                                                
026900 S04-SEND-MESSAGE-FROM-INAREA SECTION.                                    
027000                                                                          
027100     MOVE 'PUT'                      TO SEND-KDFUNC                       
027200                                                                          
027300     IF SW-SKIP-ATTR                                                      
027400       COMPUTE WS-KVDLEN = IN-KVDLEN - 1                                  
027500       CALL WZ01SEND              USING SEND-CONTROL-AREA                 
027600                                        WS-KVDLEN                         
027700                                        IN-AREA (2:)                      
027800     ELSE                                                                 
027900       CALL WZ01SEND              USING SEND-CONTROL-AREA                 
028000                                        IN-KVDLEN                         
028100                                        IN-AREA                           
028200     END-IF                                                               
028300                                                                          
028400     IF SEND-KDRC > 0                                                     
028500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
028600       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
028700       DELIMITED BY SIZE INTO ERRTEXT                                     
028800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
028900     END-IF                                                               
029000     .                                                                    
029100     SKIP3                                                                
029200 S04-SEND-HEADER-RECORD       SECTION.                                    
029300                                                                          
029400     MOVE 'PUT'                      TO SEND-KDFUNC                       
029500     MOVE LENGTH OF HDR-AREA         TO HDR-KVDLEN                        
029600     CALL WZ01SEND USING SEND-CONTROL-AREA HDR-KVDLEN HDR-AREA            
029700                                                                          
029800     IF SEND-KDRC > 0                                                     
029900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
030000       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
030100       DELIMITED BY SIZE INTO ERRTEXT                                     
030200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030300     END-IF                                                               
030400     .                                                                    
030500     SKIP3                                                                
030600 S04-SEND-CLOSE SECTION.                                                  
030700                                                                          
030800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
030900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031000                                                                          
031100     IF SEND-KDRC > 0                                                     
031200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
031300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
031400       DELIMITED BY SIZE INTO ERRTEXT                                     
031500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031600     END-IF                                                               
031700     .                                                                    
