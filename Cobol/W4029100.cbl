001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4029100.                                                
001400 AUTHOR.         NILSSON LINDA.                                           
001500 DATE-WRITTEN.   02/09/05.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       CARPARTS.PULS.PROFORMA                                   
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        BACKGROUND MPP RECIEVE PRICE WITH WZ01 FROM                      
002110*        PRICEAGENT AND UPDATES WDE8/WDE9 PROFORMA ORDERS                 
002200*                                                                         
002301*        THE PROGRAM READS     WDC7                                       
002303*        THE PROGRAM UPDATES   WDE8                                       
002310*        THE PROGRAM UPDATES   WDE9                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W40291X                                             
002700*        REQUEST:     W40291I1                                            
002800*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                        PIC X(08)  VALUE 'W4029100'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005000 77  ERROR-TEXT                   PIC X(80)  VALUE SPACE.                 
005100 77  KDRC-DISPLAY                 PIC Z(5).                               
005110 77  FELTEXT                      PIC X(80)  VALUE SPACE.                 
005200                                                                          
005300 77  YES                          PIC X      VALUE 'J'.                   
005400 77  NOO                          PIC X      VALUE 'N'.                   
006100                                                                          
006200 77  KEYS-SW                      PIC X      VALUE 'J'.                   
006300     88  KEYS-OK                             VALUE 'J'.                   
006400     88  KEYS-WRONG                          VALUE 'N'.                   
006500     EJECT                                                                
007310 01  WS-SPAR-AREOR.                                                       
007320     03  WS-SUORDV-LOC-SPAR       PIC S9(9)V9(2) COMP-3                   
007321                                                       VALUE ZERO.        
007330     03  WS-SUORDV-LOCPREL-SPAR   PIC S9(9)V9(2) COMP-3                   
007331                                                       VALUE ZERO.        
007332     03  WS-PRARTNTO-LOC-SPAR     PIC S9(7)V9(2) COMP-3                   
007333                                                       VALUE ZERO.        
007334     03  WS-PRARTNTO-LOCPREL-SPAR PIC S9(7)V9(2) COMP-3                   
007335                                                       VALUE ZERO.        
007336     03  WS-PRARTNTO-LOC-RADSUM   PIC S9(7)V9(2) COMP-3                   
007337                                                       VALUE ZERO.        
007338     03  WS-PRARTNTO-LOCPREL-RADSUM PIC S9(7)V9(2) COMP-3                 
007339                                                       VALUE ZERO.        
007340     SKIP3                                                                
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77  RKOD-ABEND                   PIC S9(4)  COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP           PIC S9(4)  COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP         PIC S9(4)  COMP VALUE +1000.            
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008300     03  ERR-WRONG-KEY            PIC X(3)   VALUE '022'.                 
008800     EJECT                                                                
008801*    --- IMS STATUS CODES                                                 
008802 01  STATUS-WS                    PIC XX.                                 
008803     88  SEGMENT-FINNS                       VALUE '  '.                  
008804     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008805     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008806     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008807     88  IMS-NOT-OK                          VALUE 'XD'.                  
008808     SKIP2                                                                
008809 01  GOOD-STATUSCODES.                                                    
008810     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008811     SKIP3                                                                
008812*                                                                         
008813 01  SSA1                        PIC X(100).                              
008816*                                                                         
008817*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008818 01  GENERAL-SUBPROGRAMS.                                                 
008819     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
008820     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
008821     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
008822     03  WZ01RECV                PIC X(8)   VALUE 'WZ01RECV'.             
008823     SKIP3                                                                
008824 01  FILLER                      PIC X(16)  VALUE 'RECV-CONTROL'.         
008825     SKIP3                                                                
008826 01  -COPY WZ01RECV                                                       
008827     EJECT                                                                
008828 01  FILLER                      PIC X(16)  VALUE 'RECV-AREA'.            
008829     SKIP3                                                                
008830 01  RECV-AREA.                                                           
008831*    03  -COPY WZ01RESP                                                   
008832*    03  -COPY W40291I1                                                   
008833     EJECT                                                                
008834*    --- PARAMETERS TO SUBPROGRAM WMEDKONV                                
008835*01 -COPY WMEDAREA                                                        
008836     SKIP3                                                                
008840*    --- PARAMETERS TO SUBPROGRAM W005INIT                                
008850*                                                                         
008860 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT'.             
008870     SKIP3                                                                
008880*01 -COPY WMSGINIT                                                        
008890     EJECT                                                                
008892*    --- IMS FUNCTIONCODES                                                
008893                                                                          
008894*01  -COPY W0003                                                          
008895     EJECT                                                                
008896*                                                                         
008897*    --- WORK-AREAS TO IMS-SECTIONS                                       
008899 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
008900     SKIP3                                                                
008910*                                                                         
010400 01  FILLER                      PIC X(16)  VALUE 'SEND-AREA'.            
010500     SKIP3                                                                
011000*    --- KEYES TO DLI                                                     
011010 01  NYCKLAR-TILL-DLI.                                                    
011020     03  W-WDC701KY-X.                                                    
011030         05  W-WDC7-IDDISTR      PIC 9(4)   VALUE ZERO.                   
011031         05  W-WDC7-IDKUNDNR     PIC 9(7)   VALUE ZERO.                   
011032         05  W-WDC7-IDBUNDLE     PIC X(15)  VALUE SPACE.                  
011033*                                                                         
011034     03  W-IDPRQUES-X.                                                    
011035         05  W-IDPRQUES          PIC 9(7)   VALUE ZERO.                   
011039*                                                                         
011060     03  W-WDE801KY-X.                                                    
011070         05  W-WDE8-IDGMTREF.                                             
011071             07  W-WDE8-IDDISTR  PIC S9(5)  VALUE ZERO COMP-3.            
011072             07  W-WDE8-IDKUNDNR PIC S9(7)  VALUE ZERO COMP-3.            
011073             07  W-WDE8-IDKUNDRF PIC X(10)  VALUE SPACE.                  
011076*                                                                         
011092     03  W-WDE901KY-MIN-X.                                                
011093         05  W-WDE9-IDORDER-MIN  PIC S9(7)  VALUE ZERO COMP-3.            
011094         05  W-WDE9-IDARTNR-MIN  PIC S9(9)  VALUE ZERO COMP-3.            
011095         05  FILLER              PIC X(2)   VALUE LOW-VALUE.              
011096                                                                          
011097     03  W-WDE901KY-MAX-X.                                                
011098         05  W-WDE9-IDORDER-MAX  PIC S9(7)  VALUE ZERO COMP-3.            
011099         05  W-WDE9-IDARTNR-MAX  PIC S9(9)  VALUE ZERO COMP-3.            
011100         05  FILLER              PIC X(2)   VALUE HIGH-VALUE.             
011110                                                                          
011120     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011301 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDC701'.        
011302 01  DLI-IO-WDC701.                                                       
011303*    03  -COPY WDC701                                                     
011304     EJECT                                                                
011305 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDC711'.        
011306 01  DLI-IO-WDC711.                                                       
011307*    03  -COPY WDC711                                                     
011308 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDE801'.        
011309 01  DLI-IO-WDE801.                                                       
011310*    03  -COPY WDE801                                                     
011311 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDE901'.        
011312 01  DLI-IO-WDE901.                                                       
011320*    03  -COPY WDE901                                                     
011600     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800*01  -COPY W0009  -PRE MSG-                                               
011900     05  FILLER                  PIC X.                                   
011901                                                                          
011902*01  -COPY W0008  -PRE WDC7-                                              
011903     05  FILLER                  PIC X.                                   
011904                                                                          
011905*01  -COPY W0008  -PRE WDE8-                                              
011906     05  FILLER                  PIC X.                                   
011907                                                                          
011908*01  -COPY W0008  -PRE WDE9-                                              
011910     05  FILLER                  PIC X.                                   
012000     EJECT                                                                
012101 PROCEDURE DIVISION  USING MSG-PCB WDC7-PCB WDE8-PCB                      
012102     WDE9-PCB.                                                            
012103 MAIN SECTION.                                                            
012104     ENTRY 'DLITCBL' USING MSG-PCB WDC7-PCB WDE8-PCB                      
012110     WDE9-PCB.                                                            
012200                                                                          
012400     PERFORM S03-RECEIVE-OPEN                                             
012500     PERFORM S03-RECEIVE-MESSAGE                                          
012600     IF RECV-KDRC = 0                                                     
012700       PERFORM A-INIT                                                     
012710       PERFORM IMS-GHU-WDC701                                             
012711       MOVE PRQ-IDDISTR           TO W-WDE8-IDDISTR                       
012712       MOVE PRQ-IDKUNDNR          TO W-WDE8-IDKUNDNR                      
012713       MOVE PRQ-IDORDNR7          TO W-WDE8-IDKUNDRF                      
012714       PERFORM IMS-GHU-WDE801                                             
012715       MOVE PHUV-IDORDER          TO W-WDE9-IDORDER-MIN                   
012716                                     W-WDE9-IDORDER-MAX                   
012717       PERFORM IMS-GHNP-WDC711                                            
012720       PERFORM UNTIL SEGMENT-SAKNAS                                       
012722         IF LPRQ-FLALL NOT = YES                                          
012730           IF LPRQ-KDPRSTA = 'A' OR 'M'                                   
012736             MOVE LPRQ-IDARTNR    TO W-WDE9-IDARTNR-MIN                   
012737                                     W-WDE9-IDARTNR-MAX                   
012738             MOVE LPRQ-IDPRQUES   TO W-IDPRQUES                           
012742             PERFORM IMS-GHU-WDE901                                       
012744             PERFORM D-CALCULATE-SUORDV                                   
012754             PERFORM B-UPDATE-DEAL-PR-LINE                                
012756             PERFORM IMS-REPL-WDE901                                      
012762*    --- TESTFIX                                                          
012763             MOVE YES TO LPRQ-FLALL                                       
012765             PERFORM IMS-REPL-WDC711                                      
012767*    --- TESTFIX SLUT                                                     
012770*            PERFORM IMS-DLET-WDC711                                      
012792           END-IF                                                         
012796         END-IF                                                           
012798         PERFORM IMS-GHNP-WDC711                                          
012800       END-PERFORM                                                        
013100       PERFORM C-UPDATE-DEAL-PR-SUM                                       
013200       PERFORM IMS-REPL-WDE801                                            
013500     END-IF                                                               
014100     PERFORM S03-RECEIVE-CLOSE                                            
014400     MOVE ZERO TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900                                                                          
014910*    --- NYCKLAR WDC7                                                     
015000     MOVE MID-IDDISTR      TO W-WDC7-IDDISTR                              
015100     MOVE MID-IDKUNDNR     TO W-WDC7-IDKUNDNR                             
015200     MOVE MID-IDBUNDLE     TO W-WDC7-IDBUNDLE                             
015600     .                                                                    
015700     EJECT                                                                
015710 B-UPDATE-DEAL-PR-LINE SECTION.                                           
015720                                                                          
015730     IF PRAD-PRARTNTO-LOC > 0                                             
015731       CONTINUE                                                           
015732     ELSE                                                                 
015733       MOVE LPRQ-PRARTNTO-LOC     TO PRAD-PRARTNTO-LOC                    
015734     END-IF                                                               
015751     MOVE ZERO                    TO PRAD-PRARTNTO-LOCPREL                
015752     MOVE LPRQ-PRARTBTO-LOC       TO PRAD-PRARTBTO-LOC                    
015753     MOVE LPRQ-KDVALISO           TO PRAD-KDVALISO                        
015754     MOVE LPRQ-KDVAT              TO PRAD-KDVAT                           
015755     MOVE LPRQ-REARTRAB           TO PRAD-RERAB                           
015756     MOVE LPRQ-KDRAB              TO PRAD-KDRAB                           
015757     MOVE LPRQ-BEART-VIPS         TO PRAD-BEART-VIPS                      
015760     .                                                                    
015770     EJECT                                                                
015780 C-UPDATE-DEAL-PR-SUM SECTION.                                            
015790                                                                          
015791     COMPUTE PHUV-SUORDV-LOC = PHUV-SUORDV-LOC +                          
015792                               WS-SUORDV-LOC-SPAR                         
015793                                                                          
015794     COMPUTE PHUV-SUORDV-LOCPREL = PHUV-SUORDV-LOCPREL -                  
015795                                   WS-SUORDV-LOCPREL-SPAR                 
015796                                                                          
015799     MOVE PRAD-KDVALISO           TO PHUV-KDVALISO                        
015800     .                                                                    
015801     EJECT                                                                
015810 D-CALCULATE-SUORDV SECTION.                                              
015900                                                                          
015901     MOVE ZERO                    TO WS-PRARTNTO-LOC-SPAR                 
015902                                     WS-PRARTNTO-LOCPREL-SPAR             
015903                                     WS-PRARTNTO-LOC-RADSUM               
015904                                     WS-PRARTNTO-LOCPREL-RADSUM           
015920     MOVE LPRQ-PRARTNTO-LOC       TO WS-PRARTNTO-LOC-SPAR                 
015922     MOVE PRAD-PRARTNTO-LOCPREL   TO WS-PRARTNTO-LOCPREL-SPAR             
015930                                                                          
016000     COMPUTE WS-PRARTNTO-LOC-RADSUM = PRAD-KVBEART-Q *                    
016010                                      WS-PRARTNTO-LOC-SPAR                
016011                                                                          
016012     COMPUTE WS-PRARTNTO-LOCPREL-RADSUM = PRAD-KVBEART-Q *                
016013                                          WS-PRARTNTO-LOCPREL-SPAR        
016014                                                                          
016020     COMPUTE WS-SUORDV-LOC-SPAR = WS-SUORDV-LOC-SPAR +                    
016030                                  WS-PRARTNTO-LOC-RADSUM                  
016100                                                                          
016200     COMPUTE WS-SUORDV-LOCPREL-SPAR = WS-SUORDV-LOCPREL-SPAR +            
016210                                      WS-PRARTNTO-LOCPREL-RADSUM          
016300     .                                                                    
016400     EJECT                                                                
020100*    --- DISPATCHER SECTIONS                                              
020200 S03-RECEIVE-OPEN SECTION.                                                
020300                                                                          
020400     MOVE 'OPEN'                   TO RECV-KDFUNC                         
020500     MOVE 'CARPARTS.PULS.PROFORMA' TO RECV-ADDISPABS                      
020600     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
020700                                                                          
020800     IF RECV-KDRC > 0                                                     
020900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
021000       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
021100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021300     END-IF                                                               
021400     .                                                                    
021500     SKIP3                                                                
021600 S03-RECEIVE-MESSAGE SECTION.                                             
021700                                                                          
021800     MOVE 'GET'                      TO RECV-KDFUNC                       
021900     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
022000     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
022100                                                                          
022200     IF RECV-KDRC > 1                                                     
022300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
022400       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
022500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022700     END-IF                                                               
022800     .                                                                    
022900     SKIP3                                                                
023000 S03-RECEIVE-CLOSE SECTION.                                               
023100                                                                          
023200     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
023300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
023400                                                                          
023500     IF RECV-KDRC > 0                                                     
023600       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
023700       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
023800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
023900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024210*    --- IMS SECTIONS                                                     
028702 IMS-GHU-WDC701 SECTION.                                                  
028703                                                                          
028704     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
028705          DELIMITED BY SIZE INTO SSA1                                     
028706     MOVE '  ' TO GOOD-STATUSCODES                                        
028707     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC701 SSA1                   
028708     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
028709     PERFORM IMS-STATUSCHECK                                              
028710     .                                                                    
028711     SKIP3                                                                
028720 IMS-GHNP-WDC711 SECTION.                                                 
028721                                                                          
028722     MOVE 'WDC711  ' TO SSA1                                              
028724     MOVE '  GE' TO GOOD-STATUSCODES                                      
028725     CALL CBLTDLI USING GHNP WDC7-PCB DLI-IO-WDC711 SSA1                  
028726     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
028727     PERFORM IMS-STATUSCHECK                                              
028728     .                                                                    
028729     SKIP3                                                                
028730 IMS-REPL-WDC711 SECTION.                                                 
028731                                                                          
028732     MOVE '  ' TO GOOD-STATUSCODES                                        
028733     CALL CBLTDLI USING REPL WDC7-PCB DLI-IO-WDC711                       
028734     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
028735     PERFORM IMS-STATUSCHECK                                              
028736     .                                                                    
028737     EJECT                                                                
028738 IMS-DLET-WDC711 SECTION.                                                 
028739                                                                          
028740     MOVE '  ' TO GOOD-STATUSCODES                                        
028741     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC711                       
028742     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
028743     PERFORM IMS-STATUSCHECK                                              
028744     .                                                                    
028745     EJECT                                                                
028746 IMS-GHU-WDE801 SECTION.                                                  
028747                                                                          
028748     STRING 'WDE801  (WDE801KY =' W-WDE801KY-X ')'                        
028751          DELIMITED BY SIZE INTO SSA1                                     
028752     MOVE '  ' TO GOOD-STATUSCODES                                        
028753     CALL CBLTDLI USING GHU WDE8-PCB DLI-IO-WDE801 SSA1                   
028754     MOVE WDE8-STATUS-CODE TO STATUS-WS                                   
028755     PERFORM IMS-STATUSCHECK                                              
028756     .                                                                    
028757     SKIP3                                                                
028758 IMS-REPL-WDE801 SECTION.                                                 
028759                                                                          
028760     MOVE '  ' TO GOOD-STATUSCODES                                        
028761     CALL CBLTDLI USING REPL WDE8-PCB DLI-IO-WDE801                       
028762     MOVE WDE8-STATUS-CODE TO STATUS-WS                                   
028763     PERFORM IMS-STATUSCHECK                                              
028764     .                                                                    
028765     EJECT                                                                
028766 IMS-GHU-WDE901 SECTION.                                                  
028767                                                                          
028768     STRING 'WDE901  (WDE901KY>=' W-WDE901KY-MIN-X                        
028769                    '&WDE901KY<=' W-WDE901KY-MAX-X                        
028770                    '&IDPRQUES =' W-IDPRQUES-X ')'                        
028771          DELIMITED BY SIZE INTO SSA1                                     
028772     MOVE '  ' TO GOOD-STATUSCODES                                        
028773     CALL CBLTDLI USING GHU WDE9-PCB DLI-IO-WDE901 SSA1                   
028774     MOVE WDE9-STATUS-CODE TO STATUS-WS                                   
028775     PERFORM IMS-STATUSCHECK                                              
028776     .                                                                    
028777     SKIP3                                                                
028815 IMS-REPL-WDE901 SECTION.                                                 
028816                                                                          
028817     MOVE '  ' TO GOOD-STATUSCODES                                        
028818     CALL CBLTDLI USING REPL WDE9-PCB DLI-IO-WDE901                       
028819     MOVE WDE9-STATUS-CODE TO STATUS-WS                                   
028820     PERFORM IMS-STATUSCHECK                                              
028821     .                                                                    
028830     EJECT                                                                
028900 IMS-STATUSCHECK SECTION.                                                 
029000                                                                          
029100     SET STATUS-IX TO 1                                                   
029200     SEARCH GOOD-STATUS                                                   
029300       AT END                                                             
029400         STRING ' WRONG STATUSCODE FROM IMS: ' STATUS-WS                  
029500         DELIMITED BY SIZE INTO FELTEXT                                   
029600         CALL FELLOG                                                      
029700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029800         CONTINUE                                                         
029900     END-SEARCH                                                           
030000     .                                                                    
