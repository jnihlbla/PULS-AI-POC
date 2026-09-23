000100*COMPOPT DB2BIND=YES                                                      
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ141300.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   11/03/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNCTION:                                                            
001000*      GENERAL PROGRAM TO SEND A REPORT FILE                              
001100*      TO DISTRIBUTION & PRINT (PROCEDURE WZ14DAP4)                       
001200*      THIS VERSION DOES NOT USE IMS QUEUES BUT CALLS                     
001300*      D&P DIRECTLY VIA SUBROUTINE CALLS                                  
001400*                                                                         
001500*      THE FILE CONTAINS SEVERAL REPORTS SEPARATED                        
001600*      BY LEADING HEADER RECORDS THAT IDENTIFY                            
001700*      WHICH D&P ADDRESS SHOULD BE USED AS A DESTINATION                  
001800*      FOR THE FOLLOWING REPORT.                                          
001900*      THERE SHOULD BE 1-3 HEADER RECORDS BEFORE EACH REPORT              
002000*      STARTING WITH THE STRING "¤DAP" FOLLOWED BY:                       
002100*        RCD 1: REPORT TYPE (OUTPUT TYPE) - MUST BE SPECIFIED             
002200*        RCD 2: SUB TYPE (OUTPUT RECEIVER) - OPTIONAL, DFLT=SPACE         
002300*        RCD 3: LIST ID - OPTIONAL, DEFAULT IS YYMMDDHHMMSS               
002400*                                                                         
002500*      SYSIN CODE SAYS HOW TO HANDLE CONTROL CHAR FOR VBA FILES           
002600*                                                                         
002700*        0 OR NONE - DEFAULT. FIRST POS IS PART OF DATA                   
002800*        1         - SKIP FIRST POS IN THE DATA.                          
002900*                    TO BE USED ONLY FOR VBA FILES.                       
003000*                                                                         
003100*      INPUT FILE IS READ AS GSAM TO ENABLE CHECKPOINT RESTART.           
003200*      IN CASE OF ABEND, USE "CKPTID=LAST" IN JCL TO RESTART FROM         
003300*      LAST CHECKPOINT.                                                   
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'WZ141300'.            
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800                                                                          
004900 01  CHKP-VAR.                                                            
005000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005400     03 GSAM-PCB-LENGTH          PIC S9(9)   VALUE +48 COMP SYNC.         
005500                                                                          
005600 77  TYPE-OF-RUN                 PIC X       VALUE 'N'.                   
005700     88  NORMAL-RUN                          VALUE 'N'.                   
005800     88  RESTART-RUN                         VALUE 'R'.                   
005900                                                                          
006000*    -- PREFIX THAT IDENTIFIES CONTROL RECORDS                            
006100*    -- IN THE INPUT FILE                                                 
006200 77  DAP-PREFIX                  PIC X(4)    VALUE '¤DAP'.                
006300     SKIP2                                                                
006400 01  ERRTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700 77  KDRC-DISPLAY                PIC Z(3)9.                               
006800                                                                          
006900 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
007000     88  END-OF-INDATA                       VALUE 'Y'.                   
007100                                                                          
007200     EJECT                                                                
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007900     03  WZ04DAP                 PIC X(8)    VALUE 'WZ04DAP '.            
008000     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
008100     SKIP3                                                                
008200*    --- PARAMETERS TO ABEND                                              
008300                                                                          
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700     EJECT                                                                
008800*                                                                         
008900*    --- PARAMETERS TO POSTSUM                                            
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200*                                                                         
009300*    --- PARAMETERS TO W009WAIT                                           
009400 77  5-SECONDS                   PIC S9(9)   COMP VALUE +500.             
009410 77  10-MILSEC                   PIC S9(9)   COMP VALUE +1.               
009500     EJECT                                                                
009600*                                                                         
009700 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
009800                                                                          
009900 01  IN-REC.                                                              
010000     03  IN-KVDLEN               PIC S9(3) COMP.                          
010100     03  IN-DATA                 PIC X(3000).                             
010200                                                                          
010300 01  IN-AREA                     PIC X(3000).                             
010400*    -- THE ¤DAP PREFIX MAY BE IN POS 1 OR 2                              
010500 01  FILLER REDEFINES IN-AREA.                                            
010600     03 FILLER                   PIC X(1).                                
010700     03 IN-PREFIX                PIC X(4).                                
010800     03 IN-HDR-DATA              PIC X(30).                               
010900 01  FILLER REDEFINES IN-AREA.                                            
011000     03 IN-PREFIX2               PIC X(4).                                
011100     03 IN-HDR-DATA2             PIC X(30).                               
011200                                                                          
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'DAP-AREA '.           
011500     SKIP3                                                                
011600 01  -COPY WZ04DAP                                                        
011700                                                                          
011800 01  SYSIN-AREA.                                                          
011900     03  SYSIN-CODE              PIC X.                                   
012000         88  SW-DEFAULT                      VALUE '0'.                   
012100         88  SW-SKIP-ATTR                    VALUE '1'.                   
012200*SYSIN-CODE                                                               
012300*TELLS IF THE FIRST CHAR IS TO BE IGNORED.                                
012400*SHOULD BE USED ONLY FOR VBA FILES.                                       
012500*    DEFAULT VALUE 0 -> CONTROL CHAR (FIRST CHAR) IS NOT SKIPPED          
012600*            VALUE 1 -> CONTROL CHAR (FIRST CHAR) SKIPPED                 
012700                                                                          
012800*    --- STATUS-KOD FRÅN IMS                                              
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FOUND                       VALUE '  '.                  
013100     88  SEGMENT-MISSING                     VALUE 'GE' 'GB'.             
013200     88  IMS-NOT-OK                          VALUE 'XD'.                  
013300                                                                          
013400 01  GOOD-STATUSCODES.                                                    
013500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013600                                                                          
013700 01  SSA1                        PIC X(200).                              
013800*    --- IMS FUNCTION CODES                                               
013900*01  -COPY W0003                                                          
014000                                                                          
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300*01  -COPY W0009   -PRE MSG-                                              
014400 01  OUT0-PCB              PIC X.                                         
014500 01  OUT01-PCB             PIC X.                                         
014600 01  OUT02-PCB             PIC X.                                         
014700 01  OUT03-PCB             PIC X.                                         
014800 01  OUT04-PCB             PIC X.                                         
014900 01  OUT05-PCB             PIC X.                                         
015000 01  OUT06-PCB             PIC X.                                         
015100 01  OUT07-PCB             PIC X.                                         
015200 01  OUT08-PCB             PIC X.                                         
015300 01  OUT09-PCB             PIC X.                                         
015400 01  OUT10-PCB             PIC X.                                         
015500 01  OUT11-PCB             PIC X.                                         
015600 01  OUT12-PCB             PIC X.                                         
015700 01  OUT13-PCB             PIC X.                                         
015800 01  OUT14-PCB             PIC X.                                         
015900 01  OUT15-PCB             PIC X.                                         
016000 01  DISTRDOC-PCB          PIC X.                                         
016100*01  -COPY W0008  -PRE IN-GSAM-                                           
016200     05  KEYFB-RSA               PIC X(12).                               
016300* KEYFB-RSA WILL HAVE THE POSITION OF LAST READ RECORD FROM INPUT         
016400* GSAM FILE. THIS IS REQUIRED TO REPOSITION THE POINTER IN INPUT          
016500* FILE DURING RESTART AFTER AN ABEND RUN.                                 
016600                                                                          
016700 PROCEDURE DIVISION                                                       
016800           USING MSG-PCB                                                  
016900                 OUT0-PCB  OUT01-PCB OUT02-PCB OUT03-PCB OUT04-PCB        
017000                 OUT05-PCB OUT06-PCB OUT07-PCB OUT08-PCB OUT09-PCB        
017100                 OUT10-PCB OUT11-PCB OUT12-PCB OUT13-PCB OUT14-PCB        
017200                 OUT15-PCB DISTRDOC-PCB IN-GSAM-PCB.                      
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL'                                                      
017500           USING MSG-PCB                                                  
017600                 OUT0-PCB  OUT01-PCB OUT02-PCB OUT03-PCB OUT04-PCB        
017700                 OUT05-PCB OUT06-PCB OUT07-PCB OUT08-PCB OUT09-PCB        
017800                 OUT10-PCB OUT11-PCB OUT12-PCB OUT13-PCB OUT14-PCB        
017900                 OUT15-PCB DISTRDOC-PCB IN-GSAM-PCB.                      
018000                                                                          
018100     PERFORM A-INIT                                                       
018200                                                                          
018300     IF NORMAL-RUN                                                        
018400       PERFORM IMS-GN-INDATA                                              
018500     ELSE                                                                 
018600       PERFORM IMS-GU-INDATA                                              
018700     END-IF                                                               
018800                                                                          
018900*    - THE ¤DAP PREFIX IS ALLOWED TO BE IN EITHER POS 1 OR 2              
019000*    - TO BETTER SUPPORT BOTH RECFM VB OR VBA.                            
019100*    - (ORIGINALLY IT WAS 2 FOR BOTH RECORD TYPES)                        
019200     PERFORM UNTIL END-OF-INDATA                                          
019300     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
019400                                                                          
019500       PERFORM B-READ-AND-CHECK-HDR-RCDS                                  
019600       PERFORM S04-DAP-OPEN                                               
019700                                                                          
019800       PERFORM UNTIL END-OF-INDATA                                        
019900       OR DAP-PREFIX = IN-PREFIX OR IN-PREFIX2                            
020000         PERFORM S04-DAP-PUT-FROM-INAREA                                  
020100         PERFORM IMS-GN-INDATA                                            
020200       END-PERFORM                                                        
020300                                                                          
020400       PERFORM S04-DAP-CLOSE                                              
020500       PERFORM IMS-CHECKPOINT                                             
020600                                                                          
020700       IF DAP-PREFIX = IN-PREFIX OR IN-PREFIX2                            
020800         CALL W009WAIT USING 10-MILSEC                                    
020900       END-IF                                                             
021000     END-PERFORM                                                          
021100                                                                          
021200     IF NOT END-OF-INDATA                                                 
021300       STRING 'WZ1413  OUTPUT TYPE MISSING'                               
021400         DELIMITED BY SIZE INTO ERRTEXT                                   
021500       DISPLAY  ERRTEXT                                                   
021600       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
021700     END-IF                                                               
021800                                                                          
021900     PERFORM Z-FINIT                                                      
022000                                                                          
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022800                                                                          
022900     ACCEPT SYSIN-AREA FROM SYSIN                                         
023000                                                                          
023100     PERFORM IMS-RESTART                                                  
023200     .                                                                    
023300                                                                          
023400 B-READ-AND-CHECK-HDR-RCDS  SECTION.                                      
023500                                                                          
023600*    -- FIRST HDR RECORD ALREADY READ                                     
023700*    -- IT CONTAINS THE OUTPUT TYPE                                       
023800     IF IN-PREFIX = DAP-PREFIX                                            
023900       MOVE IN-HDR-DATA   TO DAP-IDOUTTYPE                                
024000     ELSE                                                                 
024100       MOVE IN-HDR-DATA2  TO DAP-IDOUTTYPE                                
024200     END-IF                                                               
024300                                                                          
024400     PERFORM IMS-GN-INDATA                                                
024500     IF END-OF-INDATA                                                     
024600     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
024700       MOVE SPACE       TO DAP-IDOUTREC                                   
024800     ELSE                                                                 
024900       IF IN-PREFIX = DAP-PREFIX                                          
025000         MOVE IN-HDR-DATA  TO DAP-IDOUTREC                                
025100       ELSE                                                               
025200         MOVE IN-HDR-DATA2 TO DAP-IDOUTREC                                
025300       END-IF                                                             
025400       PERFORM IMS-GN-INDATA                                              
025500     END-IF                                                               
025600                                                                          
025700     IF END-OF-INDATA                                                     
025800     OR NOT (DAP-PREFIX = IN-PREFIX OR IN-PREFIX2)                        
025900       MOVE FUNCTION CURRENT-DATE(3:12) TO DAP-IDLIST                     
026000     ELSE                                                                 
026100       IF IN-PREFIX = DAP-PREFIX                                          
026200         MOVE IN-HDR-DATA  TO DAP-IDLIST                                  
026300       ELSE                                                               
026400         MOVE IN-HDR-DATA2 TO DAP-IDLIST                                  
026500       END-IF                                                             
026600       PERFORM IMS-GN-INDATA                                              
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 Z-FINIT SECTION.                                                         
027100                                                                          
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500                                                                          
027600 S01-POSTSUM  SECTION.                                                    
027700                                                                          
027800     MOVE 'INDATA'      TO POSTSUM-FDNAMN                                 
027900     MOVE 'WDS3G'       TO POSTSUM-DDNAMN2                                
028000     MOVE SPACE         TO POSTSUM-TRANSTYP                               
028100     CALL POSTSUM    USING POSTSUM-PARM                                   
028200     .                                                                    
028300                                                                          
028400 S04-DAP-OPEN SECTION.                                                    
028500                                                                          
028600     MOVE 'OPEN'                     TO DAP-KDFUNC                        
028700     CALL WZ04DAP USING DAP-WZ04DAP                                       
028800                                                                          
028900     IF DAP-KDRC > 0                                                      
029000       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
029100       STRING 'WZ04DAP "OPEN" ERROR RC=' KDRC-DISPLAY                     
029200              '. ' DAP-BEFEL                                              
029300       DELIMITED BY SIZE INTO ERRTEXT                                     
029400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029500     END-IF                                                               
029600     .                                                                    
029700     SKIP3                                                                
029800 S04-DAP-PUT-FROM-INAREA SECTION.                                         
029900                                                                          
030000     MOVE 'PUT'                      TO DAP-KDFUNC                        
030100     IF SW-SKIP-ATTR                                                      
030200       COMPUTE DAP-KVDLEN = IN-KVDLEN - 1                                 
030300       MOVE IN-AREA(2:DAP-KVDLEN)    TO DAP-TEOUTDATA                     
030400     ELSE                                                                 
030500       COMPUTE DAP-KVDLEN = IN-KVDLEN                                     
030600       MOVE IN-AREA(1:DAP-KVDLEN)    TO DAP-TEOUTDATA                     
030700     END-IF                                                               
030800                                                                          
030900     CALL WZ04DAP USING DAP-WZ04DAP                                       
031000                                                                          
031100     IF DAP-KDRC > 0                                                      
031200       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
031300       STRING 'WZ04DAP "PUT" ERROR RC=' KDRC-DISPLAY                      
031400              '. ' DAP-BEFEL                                              
031500       DELIMITED BY SIZE INTO ERRTEXT                                     
031600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031700     END-IF                                                               
031800     .                                                                    
031900     SKIP3                                                                
032000 S04-DAP-CLOSE SECTION.                                                   
032100                                                                          
032200     MOVE 'CLOSE'                    TO DAP-KDFUNC                        
032300     CALL WZ04DAP USING DAP-WZ04DAP                                       
032400                                                                          
032500     IF DAP-KDRC > 0                                                      
032600       MOVE DAP-KDRC TO KDRC-DISPLAY                                      
032700       STRING 'WZ04DAP "CLOSE" ERROR RC=' KDRC-DISPLAY                    
032800              '. ' DAP-BEFEL                                              
032900       DELIMITED BY SIZE INTO ERRTEXT                                     
033000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033100     END-IF                                                               
033200     .                                                                    
033300 IMS-GU-INDATA SECTION.                                                   
033400                                                                          
033500* EXECUTES DURING A RESTART TO READ THE FIRST RECORD AFTER THE            
033600* LAST CHECKPOINT                                                         
033700                                                                          
033800     MOVE 'IMS-GU-INDATA'        TO SSA1                                  
033900     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
034000     CALL CBLTDLI             USING GU                                    
034100                                    IN-GSAM-PCB                           
034200                                    IN-REC                                
034300                                    KEYFB-RSA                             
034400     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
034500     IF SEGMENT-FOUND                                                     
034600       COMPUTE IN-KVDLEN = IN-KVDLEN - 2                                  
034700       MOVE IN-DATA (1:IN-KVDLEN)                                         
034800                                 TO IN-AREA                               
034900     ELSE                                                                 
035000       SET END-OF-INDATA         TO TRUE                                  
035100       MOVE SPACES               TO IN-AREA                               
035200     END-IF                                                               
035300     PERFORM S01-POSTSUM                                                  
035400     PERFORM IMS-STATUSCHECK                                              
035500     .                                                                    
035600 IMS-GN-INDATA SECTION.                                                   
035700                                                                          
035800     MOVE 'IMS-GN-INDATA'        TO SSA1                                  
035900     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
036000     CALL CBLTDLI             USING GN                                    
036100                                    IN-GSAM-PCB                           
036200                                    IN-REC                                
036300     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
036400     IF SEGMENT-FOUND                                                     
036500       COMPUTE IN-KVDLEN = IN-KVDLEN - 2                                  
036600       MOVE IN-DATA (1:IN-KVDLEN)                                         
036700                                 TO IN-AREA                               
036800     ELSE                                                                 
036900       SET END-OF-INDATA         TO TRUE                                  
037000       MOVE SPACES               TO IN-AREA                               
037100     END-IF                                                               
037200     PERFORM S01-POSTSUM                                                  
037300     PERFORM IMS-STATUSCHECK                                              
037400      .                                                                   
037500 IMS-RESTART SECTION.                                                     
037600                                                                          
037700     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
037800     MOVE '  '                   TO GOOD-STATUSCODES                      
037900     CALL CBLTDLI             USING XRST                                  
038000                                    MSG-PCB                               
038100                                    CHKP-MSG-IO-AREA-LENGTH               
038200                                    CHKP-MSG-IO-AREA                      
038300                                    CHKP-AREA-LENGTH                      
038400                                    CHKP-AREA                             
038500                                    GSAM-PCB-LENGTH                       
038600                                    IN-GSAM-PCB                           
038700                                                                          
038800     IF CHKP-MSG-IO-AREA = SPACES                                         
038900       SET NORMAL-RUN            TO TRUE                                  
039000     ELSE                                                                 
039100       SET RESTART-RUN           TO TRUE                                  
039200     END-IF                                                               
039300                                                                          
039400     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
039500     PERFORM IMS-STATUSCHECK                                              
039600     .                                                                    
039700                                                                          
039800 IMS-CHECKPOINT SECTION.                                                  
039900                                                                          
040000     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
040100     MOVE '  XD'                 TO GOOD-STATUSCODES                      
040200     CALL CBLTDLI             USING CHKP                                  
040300                                    MSG-PCB                               
040400                                    CHKP-MSG-IO-AREA-LENGTH               
040500                                    CHKP-MSG-IO-AREA                      
040600                                    CHKP-AREA-LENGTH                      
040700                                    CHKP-AREA                             
040800                                    GSAM-PCB-LENGTH                       
040900                                    IN-GSAM-PCB                           
041000     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
041100     PERFORM IMS-STATUSCHECK                                              
041200                                                                          
041300     IF IMS-NOT-OK                                                        
041400       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
041500       TO ERRTEXT-STR                                                     
041600       DISPLAY ERRTEXT                                                    
041700       CALL FELLOG                                                        
041800     END-IF                                                               
041900     .                                                                    
042000                                                                          
042100 IMS-STATUSCHECK SECTION.                                                 
042200                                                                          
042300     SET STATUS-IX               TO 1                                     
042400     SEARCH GOOD-STATUS                                                   
042500       AT END                                                             
042600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
042700           DELIMITED BY SIZE INTO ERRTEXT-STR                             
042800         DISPLAY ERRTEXT                                                  
042900         CALL FELLOG                                                      
043000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
043100         CONTINUE                                                         
043200     END-SEARCH                                                           
043300     .                                                                    
