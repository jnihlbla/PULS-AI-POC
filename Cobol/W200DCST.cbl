000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W200DCST.                                                
000400 AUTHOR.         PRIYASOPHIA GALBAO.                                      
000500 DATE-WRITTEN.   02/08/2023                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    REMARKS:                                                             
000900*        THE PROGRAM IS A SUB-MODULE OF:                                  
001000*                    W9051400                                             
001100*                                                                         
001200*                                                                         
001300*    FUNCTION:                                                            
001400*        DC STOCK ENQUIRY                                                 
001500*        MANDATORY FIELD: IDARTNR, IDDC                                   
001600*        RETURN BACK THE STOCK AVAILABILITY INFO PER DC                   
001700*                                                                         
001800*        FETCHES THE FOLLOWING INFO FOR A GIVEN DC AND PART NO.:          
001900*        1. STOCK BALANCE (KVLS)                                          
002000*        2. AVAILABLE BALANCE (KVDISP)                                    
002100*        3. BO BALANCE DAY (KVROS-DAG)                                    
002200*        4. BO BALANCE BULK (KVROS-BULK)                                  
002300*        5. ORDER QUEUE BALANCE DAY (KVOKS-DAG)                           
002400*        6. ORDER QUEUE BALANCE BULK (KVOKS-BULK)                         
002500*        7. PREL ORDER Q BALANCE FOR PREPLANNED ORDERS(KVOKS-PREL)        
002600*        8. ORDERED NOT INVOICED QTY (KVEFRS)                             
002700*        9. ADVICE BALANCE (KVAKS)                                        
002800*       10. ISO-CODE FOR NAME OF COUNTRY (IDLANDX2)                       
002900*       11. CAMPAIGN INFORMATION PART (Y/N)                               
003000*                                                                         
003100*        THE PROGRAM READS     WDB6                                       
003200*                              WDK6                                       
003300*                              WDK7                                       
003400*                              WDK9                                       
003500*                              WDQ4C                                      
003600*                              WDM2A1                                     
003700*                                                                         
003800*    ABENDCODES:                                                          
003900*        U0016 -  . . . .                                                 
004000*        U1000 -  . . . .                                                 
004100*                                                                         
004200                                                                          
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP2                                                                
004600 INPUT-OUTPUT SECTION.                                                    
004700                                                                          
004800 FILE-CONTROL.                                                            
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100     SKIP2                                                                
005200 FILE SECTION.                                                            
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W200DCST'.            
005700 77  WS-CURRENT-SECTION          PIC X(16)   VALUE SPACE.                 
005800 77  WS-CURRENT-IMS-SECTION      PIC X(16)   VALUE SPACE.                 
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006100 77  FEL                         PIC X       VALUE 'F'.                   
006200     EJECT                                                                
006300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006400     88  KEYS-OK                             VALUE 'J'.                   
006500     88  KEYS-ERR                            VALUE 'N'.                   
006600*                                                                         
006700     EJECT                                                                
006800 01  MISCELLANEOUS.                                                       
006900     03  WS-KVDISP-SLAG          PIC S9(7)   VALUE ZERO COMP-3.           
007000     03  WS-KVOKS-PREL           PIC S9(7)   VALUE ZERO COMP-3.           
007100     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
007200     03  WS-KVDISP-CLAG          PIC S9(7)   VALUE ZERO COMP-3.           
007300                                                                          
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     SKIP2                                                                
008000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008500     SKIP2                                                                
008600 01  ERROR-TEXT.                                                          
008700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008800     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- AREAS FOR IMS-SECTIONS                                           
009100*                                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400     SKIP3                                                                
009500 01  KEYS-FOR-DLI.                                                        
009600     03  W-IDARTNR-X.                                                     
009700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009800                                                                          
009900     03  W-KDSEGKEY-X.                                                    
010000         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
010100                                                                          
010200     03  W-WDQ4CKY-FOM.                                                   
010300         05  W-Q4-IDARTNR-F      PIC  S9(9)    COMP-3.                    
010400         05  W-Q4-IDDC-F         PIC  X(2).                               
010500         05  FILLER              PIC  X(13)    VALUE LOW-VALUE.           
010600                                                                          
010700     03  W-WDQ4CKY-TOM.                                                   
010800         05  W-Q4-IDARTNR-T      PIC  S9(9)    COMP-3.                    
010900         05  W-Q4-IDDC-T         PIC  X(2).                               
011000         05  FILLER              PIC  X(13)    VALUE HIGH-VALUE.          
011100                                                                          
011200     03  W-WDM2A1KY-MIN-X.                                                
011300         05  W-IDARTNR-MIN      PIC S9(9)           COMP-3.               
011400         05  W-IDKAMPRF-MIN     PIC S9(7)           COMP-3.               
011500         05  W-IDDC-MIN         PIC X(02).                                
011600                                                                          
011700     03  W-WDM2A1KY-MAX-X.                                                
011800         05  W-IDARTNR-MAX      PIC S9(9)           COMP-3.               
011900         05  W-IDKAMPRF-MAX     PIC S9(7)           COMP-3.               
012000         05  W-IDDC-MAX         PIC X(02).                                
012100                                                                          
012200     03  W-IDDC-X.                                                        
012300         05  W-IDDC             PIC X(02).                                
012400                                                                          
012500     SKIP2                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FOUND                       VALUE '  '.                  
012900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013100     88  SEGMENT-FINAL                       VALUE 'GB'.                  
013200     SKIP2                                                                
013300 01  GOOD-STATUSCODES.                                                    
013400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  ALL-SSA.                                                             
013700     03 SSA1                     PIC X(128).                              
013800     03 SSA2                     PIC X(64).                               
013900     EJECT                                                                
014000*    --- IMS FUNCTION CODES                                               
014100*01  -COPY W0003                                                          
014200     EJECT                                                                
014300*                                                                         
014400*    --- VALID IDDC CODES                                                 
014500*                                                                         
014600*01    -COPY WWDC99                                                       
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
015000 01  DLI-IO-WDB601.                                                       
015100*    03  -COPY WDB601                                                     
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
015300 01  DLI-IO-WDK611.                                                       
015400*    03  -COPY WDK611                                                     
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
015600 01  DLI-IO-WDK711.                                                       
015700*    03  -COPY WDK711                                                     
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
015900 01  DLI-IO-WDK901.                                                       
016000*    03  -COPY WDK901                                                     
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4C1'.                      
016200 01  DLI-IO-WDQ4C1.                                                       
016300*    03  -COPY WDQ4C1                                                     
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM2A1'.                      
016500 01  DLI-IO-WDM2A1.                                                       
016600*    03  -COPY WDM2A1                                                     
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*    -COPY W200DCST                                                       
017000*                                                                         
017100*01  -COPY W0008  -PRE WDB6-                                              
017200     05  FILLER                  PIC X.                                   
017300*01  -COPY W0008  -PRE WDK6-                                              
017400     05  FILLER                  PIC X.                                   
017500*01  -COPY W0008  -PRE WDK7-                                              
017600     05  FILLER                  PIC X.                                   
017700*01  -COPY W0008  -PRE WDK9-                                              
017800     05  FILLER                  PIC X.                                   
017900*01  -COPY W0008  -PRE WDQ4C-                                             
018000     05  FILLER                  PIC X.                                   
018100*01  -COPY W0008  -PRE WDM2A-                                             
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400 PROCEDURE DIVISION  USING DCST-W200DCST WDB6-PCB                         
018500                           WDK6-PCB  WDK7-PCB                             
018600                           WDK9-PCB                                       
018700                           WDQ4C-PCB WDM2A-PCB.                           
018800 MAIN SECTION.                                                            
018900                                                                          
019000                                                                          
019100     PERFORM A-INIT                                                       
019200                                                                          
019300     PERFORM B-VALIDATE-INPUT                                             
019400                                                                          
019500     IF KEYS-OK                                                           
019600       IF CDC-SE                                                          
019700         PERFORM C-GET-CDC-STOCK-INFO                                     
019800       ELSE                                                               
019900         PERFORM D-GET-DC-STOCK-INFO                                      
020000       END-IF                                                             
020100     END-IF                                                               
020200                                                                          
020300     PERFORM Z-FINIT                                                      
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     MOVE 'A-INIT'                    TO WS-CURRENT-SECTION               
021200                                                                          
021300     INITIALIZE DCST-OUTPUT-DATA                                          
021400     MOVE ZERO                        TO WS-KVDISP-SLAG                   
021500                                         WS-KVOKS-PREL                    
021600     MOVE LOW-VALUE                   TO W-WDM2A1KY-MIN-X                 
021700     MOVE HIGH-VALUE                  TO W-WDM2A1KY-MAX-X                 
021800                                                                          
021900     MOVE SPACE                       TO DCST-KDSVAR                      
022000     MOVE SPACES                      TO DCST-FEL-TEXT                    
022100                                                                          
022200     .                                                                    
022300     EJECT                                                                
022400 B-VALIDATE-INPUT SECTION.                                                
022500                                                                          
022600     MOVE 'B-VALIDATE-INPUT'          TO WS-CURRENT-SECTION               
022700                                                                          
022800     IF DCST-IDARTNR-IN NUMERIC AND DCST-IDARTNR-IN > 0                   
022900       MOVE DCST-IDARTNR-IN           TO W-IDARTNR                        
023000                                         W-Q4-IDARTNR-F                   
023100                                         W-Q4-IDARTNR-T                   
023200                                         W-IDARTNR-MIN                    
023300                                         W-IDARTNR-MAX                    
023400     ELSE                                                                 
023500       MOVE NOO                       TO KEYS-SW                          
023600       SET DCST-KDSVAR-FEL            TO TRUE                             
023700       MOVE '022'                     TO DCST-IDMSG-ERROR                 
023800       MOVE 'IDARTNR'                 TO DCST-IDELMT-ERROR                
023900       MOVE 'INVALID PART NUM ' TO DCST-FEL-TEXT                          
024000     END-IF                                                               
024100*                                                                         
024200     IF DCST-IDDC-IN NOT= SPACES                                          
024300       MOVE DCST-IDDC-IN              TO W-IDDC                           
024400                                         WS-IDDC                          
024500                                         W-Q4-IDDC-F                      
024600                                         W-Q4-IDDC-T                      
024700       PERFORM IMS-GU-WDB601                                              
024800       IF SEGMENT-MISSING                                                 
024900         MOVE NOO                     TO KEYS-SW                          
025000         SET DCST-KDSVAR-FEL          TO TRUE                             
025100         MOVE '025'                   TO DCST-IDMSG-ERROR                 
025200         MOVE 'IDDC'                  TO DCST-IDELMT-ERROR                
025300         MOVE 'DC NOT FOUND'          TO DCST-FEL-TEXT                    
025400                                                                          
025500       END-IF                                                             
025600     ELSE                                                                 
025700       MOVE NOO                       TO KEYS-SW                          
025800       SET DCST-KDSVAR-FEL            TO TRUE                             
025900       MOVE '026'                     TO DCST-IDMSG-ERROR                 
026000       MOVE 'IDDC'                    TO DCST-IDELMT-ERROR                
026100       MOVE 'DC MUST BE PROVIDED'     TO DCST-FEL-TEXT                    
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 C-GET-CDC-STOCK-INFO SECTION.                                            
026600                                                                          
026700     MOVE 'C-GET-CDC-STOCK-INFO'      TO WS-CURRENT-SECTION               
026800                                                                          
026900     PERFORM CA-GET-ORDER-INFO                                            
027000                                                                          
027100     PERFORM IMS-GU-WDK611                                                
027200     IF SEGMENT-FOUND                                                     
027300       COMPUTE WS-KVDISP-CLAG =  CLAG-KVLS  -                             
027400                                 WS-KVOKS-TOT -                           
027500                                 CLAG-KVRESS                              
027600                                                                          
027700          MOVE WS-IDDC           TO DCST-IDDC                             
027800          MOVE CLAG-KVLS         TO DCST-KVLS                             
027900          MOVE WS-KVDISP-SLAG    TO DCST-KVDISP                           
028000*AS WE DO NOT HAVE CLAG-KVROS FOR DAY AND BULK SEPARATELY, MOVING         
028100*CLAG-KVROS TO THE -DAY FIELD AND -BULK FIELD WILL BE 0 FOR CDC           
028200          MOVE CLAG-KVROS        TO DCST-KVROS-DAG                        
028300          MOVE 0                 TO DCST-KVROS-BULK                       
028400          PERFORM CA-GET-ORDER-INFO                                       
028500          MOVE 0                 TO DCST-KVOKS-PREL                       
028600          MOVE CLAG-KVEFRS       TO DCST-KVEFRS                           
028700          MOVE CLAG-KVAKS-CDC    TO DCST-KVAKS                            
028800          MOVE CLAG-KVRESS       TO DCST-KVRESS                           
028900          MOVE CLAG-KVVORKO      TO DCST-KVVORKO                          
029000          MOVE DCS-IDLANDX2      TO DCST-IDLANDX2                         
029100          PERFORM S01-GET-FLKAMPART                                       
029200     ELSE                                                                 
029300       SET DCST-KDSVAR-FEL            TO TRUE                             
029400       MOVE '025'                     TO DCST-IDMSG-ERROR                 
029500       MOVE 'IDARTNR'                 TO DCST-IDELMT-ERROR                
029600       MOVE 'PART NO. NOT FOUND'      TO DCST-FEL-TEXT                    
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000 CA-GET-ORDER-INFO SECTION.                                               
030100                                                                          
030200     MOVE 'CA-GET-ORDER-INFO'           TO WS-CURRENT-SECTION             
030300                                                                          
030400     PERFORM IMS-GU-WDK901                                                
030500     IF SEGMENT-FOUND                                                     
030600        COMPUTE WS-KVOKS-TOT            =  ART-KVOKS-BULK +               
030700                                           ART-KVOKS-DAG  +               
030800                                           ART-KVOKS-VOR                  
030900        MOVE ART-KVOKS-DAG              TO DCST-KVOKS-DAG                 
031000        MOVE ART-KVOKS-DAG              TO DCST-KVOKS-DAG                 
031100        MOVE ART-KVOKS-BULK             TO DCST-KVOKS-BULK                
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 D-GET-DC-STOCK-INFO SECTION.                                             
031600                                                                          
031700     MOVE 'D-GET-DC-STOCK-INFO'       TO WS-CURRENT-SECTION               
031800                                                                          
031900     PERFORM IMS-GU-WDK711                                                
032000     IF SEGMENT-FOUND                                                     
032100       COMPUTE WS-KVDISP-SLAG =  SLAG-KVLS  -                             
032200                                 SLAG-KVOKS-DAG -                         
032300                                 SLAG-KVOKS-BULK                          
032400                                                                          
032500       MOVE SLAG-IDDC                 TO DCST-IDDC                        
032600       MOVE SLAG-KVLS                 TO DCST-KVLS                        
032700       MOVE WS-KVDISP-SLAG            TO DCST-KVDISP                      
032800       MOVE SLAG-KVROS-DAG            TO DCST-KVROS-DAG                   
032900       MOVE SLAG-KVROS-BULK           TO DCST-KVROS-BULK                  
033000       MOVE SLAG-KVOKS-DAG            TO DCST-KVOKS-DAG                   
033100       MOVE SLAG-KVOKS-BULK           TO DCST-KVOKS-BULK                  
033200       PERFORM DA-GET-KVOKS-PREL                                          
033300       MOVE WS-KVOKS-PREL             TO DCST-KVOKS-PREL                  
033400       MOVE SLAG-KVEFRS               TO DCST-KVEFRS                      
033500       MOVE SLAG-KVAKS-SDC            TO DCST-KVAKS                       
033600       MOVE SLAG-KVRESS               TO DCST-KVRESS                      
033700       MOVE ZERO                      TO DCST-KVVORKO                     
033800       MOVE DCS-IDLANDX2              TO DCST-IDLANDX2                    
033900       PERFORM S01-GET-FLKAMPART                                          
034000     ELSE                                                                 
034100       SET DCST-KDSVAR-FEL            TO TRUE                             
034200       MOVE '025'                     TO DCST-IDMSG-ERROR                 
034300       MOVE 'IDARTNR'                 TO DCST-IDELMT-ERROR                
034400       MOVE 'PART NO. NOT FOUND'      TO DCST-FEL-TEXT                    
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 DA-GET-KVOKS-PREL SECTION.                                               
034900                                                                          
035000     MOVE 'DA-GET-KVOKS-PREL'         TO WS-CURRENT-SECTION               
035100                                                                          
035200     MOVE ZERO              TO WS-KVOKS-PREL                              
035300     PERFORM IMS-GN-WDQ4C1                                                
035400     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINAL                       
035500        ADD SEQC-KVOKS-PREL TO WS-KVOKS-PREL                              
035600        PERFORM IMS-GN-WDQ4C1                                             
035700     END-PERFORM                                                          
035800     .                                                                    
035900     EJECT                                                                
036000 S01-GET-FLKAMPART  SECTION.                                              
036100                                                                          
036200     MOVE 'S01-GET-FLKAMPART'         TO WS-CURRENT-SECTION               
036300                                                                          
036400     PERFORM IMS-GU-WDM2A1-DC                                             
036500                                                                          
036600     IF SEGMENT-FOUND                                                     
036700       MOVE 'Y'                       TO DCST-FLKAMPART                   
036800     ELSE                                                                 
036900       MOVE 'N'                       TO DCST-FLKAMPART                   
037000     END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 Z-FINIT SECTION.                                                         
037400     .                                                                    
037500     EJECT                                                                
037600* --- IMS SECTIONS  ---                                                   
037700                                                                          
037800     EJECT                                                                
037900 IMS-GU-WDB601    SECTION.                                                
038000                                                                          
038100     MOVE 'IMS-GU-WDB601   '        TO WS-CURRENT-IMS-SECTION             
038200                                                                          
038300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
038400          DELIMITED BY SIZE INTO SSA1                                     
038500     MOVE '  GE'            TO GOOD-STATUSCODES                           
038600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
038700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
038800     PERFORM IMS-STATUSCHECK                                              
038900     .                                                                    
039000     EJECT                                                                
039100 IMS-GU-WDK611 SECTION.                                                   
039200                                                                          
039300     MOVE 'IMS-GU-WDK611   '        TO WS-CURRENT-IMS-SECTION             
039400                                                                          
039500     MOVE SPACES            TO ALL-SSA                                    
039600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
039700          DELIMITED BY SIZE INTO SSA1                                     
039800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
039900          DELIMITED BY SIZE INTO SSA2                                     
040000     MOVE '  GE'            TO GOOD-STATUSCODES                           
040100     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
040200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUSCHECK                                              
040400     .                                                                    
040500     EJECT                                                                
040600 IMS-GU-WDK711 SECTION.                                                   
040700                                                                          
040800     MOVE 'IMS-GU-WDK711   '        TO WS-CURRENT-IMS-SECTION             
040900                                                                          
041000     MOVE SPACES            TO ALL-SSA                                    
041100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
041200          DELIMITED BY SIZE INTO SSA1                                     
041300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
041400          DELIMITED BY SIZE INTO SSA2                                     
041500     MOVE '  GE'            TO GOOD-STATUSCODES                           
041600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
041700     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
041800     PERFORM IMS-STATUSCHECK                                              
041900     .                                                                    
042000     EJECT                                                                
042100 IMS-GU-WDK901    SECTION.                                                
042200                                                                          
042300     MOVE 'IMS-GU-WDK901   '        TO WS-CURRENT-IMS-SECTION             
042400                                                                          
042500     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
042600          DELIMITED BY SIZE INTO SSA1                                     
042700     MOVE '  GE'            TO GOOD-STATUSCODES                           
042800     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
042900     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
043000     PERFORM IMS-STATUSCHECK                                              
043100     .                                                                    
043200     EJECT                                                                
043300 IMS-GN-WDQ4C1 SECTION.                                                   
043400                                                                          
043500     MOVE 'IMS-GN-WDQ4C1   '        TO WS-CURRENT-IMS-SECTION             
043600                                                                          
043700     MOVE SPACES            TO ALL-SSA                                    
043800     STRING 'WDQ4C1  (WDQ4C1KY>=' W-WDQ4CKY-FOM                           
043900                    '&WDQ4C1KY<=' W-WDQ4CKY-TOM ')'                       
044000            DELIMITED BY SIZE INTO SSA1                                   
044100     MOVE '  GEGB'            TO GOOD-STATUSCODES                         
044200     CALL CBLTDLI USING GN WDQ4C-PCB DLI-IO-WDQ4C1 SSA1                   
044300     MOVE WDQ4C-STATUS-CODE   TO STATUS-WS                                
044400     PERFORM IMS-STATUSCHECK                                              
044500     .                                                                    
044600     EJECT                                                                
044700 IMS-GU-WDM2A1-DC SECTION.                                                
044800                                                                          
044900     MOVE 'IMS-GU-WDM2A1-DC'        TO WS-CURRENT-IMS-SECTION             
045000                                                                          
045100     STRING 'WDM2A1  (WDM2A1KY>=' W-WDM2A1KY-MIN-X                        
045200                    '&WDM2A1KY<=' W-WDM2A1KY-MAX-X                        
045300                    '&IDDC     =' W-IDDC-X ')'                            
045400            DELIMITED BY SIZE INTO SSA1                                   
045500     MOVE '  GE'              TO GOOD-STATUSCODES                         
045600     CALL CBLTDLI USING GU WDM2A-PCB DLI-IO-WDM2A1 SSA1                   
045700     MOVE WDM2A-STATUS-CODE   TO STATUS-WS                                
045800     PERFORM IMS-STATUSCHECK                                              
045900     .                                                                    
046000     EJECT                                                                
046100 IMS-STATUSCHECK SECTION.                                                 
046200                                                                          
046300     SET STATUS-IX TO 1                                                   
046400     SEARCH GOOD-STATUS                                                   
046500       AT END                                                             
046600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046700           DELIMITED BY SIZE INTO ERROR-TEXT                              
046800         DISPLAY ERROR-TEXT                                               
046900         CALL FELLOG                                                      
047000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
047100         CONTINUE                                                         
047200     END-SEARCH                                                           
047300     .                                                                    
