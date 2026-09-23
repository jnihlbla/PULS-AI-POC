001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5017API.                                                
001600 AUTHOR.         ARINDAM METIA.                                           
001700 DATE-WRITTEN.   24/09/10.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        LOGIC                                                            
002200*        LOGIC                                                            
002300*                                                                         
002500*                                                                         
002600*    INDATA.                                                              
002900*                                                                         
003000*    OUTDATA.                                                             
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W5017API'.            
003910 77  WS-CURRENT-SECTION          PIC X(16)   VALUE SPACE.                 
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005000                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600                                                                          
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700                                                                          
011200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011900*    --- STATUS CODES FROM IMS                                            
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
013600 01  ALL-SSA.                                                             
013700     03 SSA1                     PIC X(128).                              
013800     03 SSA2                     PIC X(64).                               
013000     EJECT                                                                
013001                                                                          
013010 01  NYCKLAR-TILL-DLI.                                                    
013020     03  W-IDARTNR-X.                                                     
013030         05  W-IDARTNR           PIC S9(9)   VALUE +0  COMP-3.            
013040                                                                          
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
014060 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014070 01  DLI-IO-WDK601.                                                       
014080*    03 -COPY WDK601                                                      
014081 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014082 01  DLI-IO-WDK611.                                                       
014083*    03 -COPY WDK611                                                      
014090                                                                          
014100 LINKAGE SECTION.                                                         
014300*    -COPY W5017API                                                       
014600     EJECT                                                                
014200*01  -COPY W0008     -PRE WDK6-                                           
014210     05  FILLER                  PIC X.                                   
014220     EJECT                                                                
014230                                                                          
014701 PROCEDURE DIVISION  USING API-W5017API WDK6-PCB.                         
014702 MAIN SECTION.                                                            
014800                                                                          
014900     PERFORM A-INIT                                                       
015000                                                                          
015100     PERFORM B-VALIDATE-INPUT                                             
015200                                                                          
015300     IF KEYS-OK                                                           
015700        PERFORM C-GET-STANDARD-PRICE                                      
015900     END-IF                                                               
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     MOVE 'A-INIT'                    TO WS-CURRENT-SECTION               
017500                                                                          
017600     INITIALIZE API-OUTPUT-DATA                                           
018100                                                                          
018200     MOVE SPACE                       TO API-KDSVAR                       
018300     MOVE SPACES                      TO API-FEL-TEXT                     
018500     .                                                                    
020200     EJECT                                                                
020300 B-VALIDATE-INPUT SECTION.                                                
020400                                                                          
020500     MOVE 'B-VALIDATE-INPUT'          TO WS-CURRENT-SECTION               
020600                                                                          
020700     IF API-IDARTNR-IN NUMERIC AND API-IDARTNR-IN > 0                     
020800       MOVE API-IDARTNR-IN            TO W-IDARTNR                        
021300     ELSE                                                                 
021400       MOVE NOO                       TO KEYS-SW                          
021500       SET API-KDSVAR-FEL             TO TRUE                             
021600       MOVE '022'                     TO API-IDMSG-ERROR                  
021700       MOVE 'IDARTNR'                 TO API-IDELMT-ERROR                 
021800       MOVE 'INVALID PART NUM '       TO API-FEL-TEXT                     
021900     END-IF                                                               
022000     .                                                                    
023300     EJECT                                                                
023400 C-GET-STANDARD-PRICE SECTION.                                            
023500                                                                          
023510     MOVE 'C-GET-STANDARD-PRICE'      TO WS-CURRENT-SECTION               
023511     PERFORM IMS-GU-WDK601                                                
023512     IF SEGMENT-FOUND                                                     
023514        PERFORM IMS-GNP-WDK611                                            
023515        IF SEGMENT-FOUND                                                  
023516           MOVE CLAG-PRARTSTD         TO API-PRARTSTD-OUT                 
023517           MOVE 'SEK'                 TO API-KDVALISO-OUT                 
023518        ELSE                                                              
023519*          NOT FOUND ***                                                  
021500           SET API-KDSVAR-FEL         TO TRUE                             
023519           MOVE '025'                 TO API-IDMSG-ERROR                  
023520           MOVE 'STANDARD PRICE'      TO API-IDELMT-ERROR                 
023521        END-IF                                                            
023522     ELSE                                                                 
023523*       NOT FOUND ***                                                     
021500        SET API-KDSVAR-FEL            TO TRUE                             
023524        MOVE '025'                    TO API-IDMSG-ERROR                  
023525        MOVE 'IDARTNR'                TO API-IDELMT-ERROR                 
023526     END-IF                                                               
023527     .                                                                    
023600     EJECT                                                                
023610 Z-FINIT SECTION.                                                         
023620     .                                                                    
023630     EJECT                                                                
023700 IMS-GU-WDK601 SECTION.                                                   
023710                                                                          
023800     MOVE 'IMS-GU-WDK601 '  TO WS-CURRENT-SECTION                         
023900     MOVE SPACES            TO ALL-SSA                                    
024000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
024100          DELIMITED BY SIZE INTO SSA1                                     
024200     MOVE '  GE' TO GOOD-STATUSCODES                                      
024300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
024400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024500     PERFORM IMS-STATUSCHECK                                              
024600     .                                                                    
024700     EJECT                                                                
024900  IMS-GNP-WDK611 SECTION.                                                 
024910                                                                          
025000      MOVE 'IMS-GNP-WDK611 '  TO WS-CURRENT-SECTION                       
025100                                                                          
025200      MOVE 'WDK611 ' TO SSA2                                              
025300      MOVE '  GE' TO GOOD-STATUSCODES                                     
025400      CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA2                  
025500      MOVE WDK6-STATUS-CODE TO STATUS-WS                                  
025600      PERFORM IMS-STATUSCHECK                                             
025700      .                                                                   
025800      EJECT                                                               
032800 IMS-STATUSCHECK SECTION.                                                 
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GOOD-STATUS                                                   
033200       AT END                                                             
033300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033400         DELIMITED BY SIZE INTO ERROR-TEXT                                
033500         CALL FELLOG                                                      
033600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033700         CONTINUE                                                         
033800     END-SEARCH                                                           
033900     .                                                                    
