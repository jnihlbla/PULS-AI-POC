000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4071400.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   95/08/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        QUESTION ON ADDITIONAL COSTS.                                    
001000*        KEYS THAT HAV TO BE FILLED IN: DISTR/CUST/REPORT.NO              
001100*        READS DATABASE WLKREE AND SHOWS INFO.                            
001200*                                                                         
001300*        THE PROGRAM READS     WLKREE (WDA2)                              
001400*                              WLARTC (WDK6)                              
001500*                              WLARTS (WDK7)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W4T714                                              
001900*        MID:         W4I71401                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W4O71401                                            
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900*    -- CHECKED BY WY2000                                                 
003000     SKIP3                                                                
003100 77  IDPGM                       PIC X(08)   VALUE 'W4071400'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
003500                                                                          
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FOR SCROLL LINES                                           
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +1200 COMP SYNC.        
004200 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004300 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
004400 77  IDARTNR-WS                  PIC 9(8)   VALUE ZERO.                   
004500 77  IDRADNR-WS                  PIC 9(4)   VALUE ZERO.                   
004600 77  WS-RELANDCO                 PIC S9(3)V9(2) VALUE ZERO.               
004700 77  WS-PRCOST                   PIC S9(7)V9(2) VALUE ZERO.               
004800                                                                          
005300 77  WS-KDVALISO-COST            PIC X(3)   VALUE SPACE.                  
005400                                                                          
005500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005600                                                                          
005700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005800     88  KEYS-OK                             VALUE 'Y'.                   
005900     88  KEYS-WRONG                          VALUE 'N'.                   
006000                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  OWN-MID                             VALUE '4714'.                
006300     88  GOOD-MID                            VALUE '4711' '4712'          
006400                                                   '4713' '4714'          
006500                                                   '4715' '4716'.         
006600     88  HELP-MID                            VALUE '0551'.                
006700     88  4737-MID                            VALUE '4737'.                
006800     EJECT                                                                
006900*      --- VALID IDDC CODES                                               
007000*                                                                         
007100*01    -COPY WWDC99                                                       
007200       EJECT                                                              
007600                                                                          
007700 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
007800*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'WSIDFTG'.             
008100*01  -COPY WWIDFTG                                                        
008200     EJECT                                                                
008300*                                                                         
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERAL-SUBPROGRAM.                                                  
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
009100     EJECT                                                                
009200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009300*01 -COPY WMEDAREA                                                        
009400                                                                          
009500 01  MESSAGE-CODES.                                                       
009600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009900     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
010000     03  ERR-NO-UPDATE           PIC X(3)    VALUE '777'.                 
010100     03  ERR-LINE-MISSING        PIC X(3)    VALUE '005'.                 
010200     EJECT                                                                
010300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010600                                                                          
010700*01 -COPY WMSGINIT                                                        
010800                                                                          
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200                                                                          
011300*01  MID -COPY W4I71401                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600                                                                          
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W4O71401                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300                                                                          
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012700*                                                                         
012800     EJECT                                                                
012900*    ---  LÄNKAREA TILL W418OKOD                                          
013000                                                                          
013100*    03 -COPY W418OKOD           -PRE OKOD-.                              
013200     EJECT                                                                
013300                                                                          
013400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013500                                                                          
013600 01  KEYS-TO-DLI.                                                         
013700     03  W-IDLEVANM-X.                                                    
013800         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013900         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014000         05  W-IDRAPPNR          PIC 9(7).                                
014100     03  W-WDA2KEY-X.                                                     
014200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014300         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
014400                                                                          
014500     03  W-IDARTNR-X.                                                     
014600         05  WW-IDARTNR          PIC S9(9)   COMP-3 VALUE ZERO.           
014700                                                                          
014800     03  W-IDDC-X.                                                        
014900         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
015000                                                                          
015100     03  W-KDSEGKEY-X.                                                    
015200         05  W-KDSEGKEY          PIC  X(1)   VALUE '1'.                   
015300                                                                          
015400*    -NYCKLAR TIL WDB201                                                  
015500     03  W-IDGMT-X.                                                       
015600       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
015700       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
015800     03  W-IDGMT-MIN-X.                                                   
015900       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
016000       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
016100     03  W-IDGMT-MAX-X.                                                   
016200       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
016300       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
016400                                                                          
016500* TILL WDB101                                                             
016600     03  W-WDB101KY-X.                                                    
016700       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
016800       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
016900*                                                                         
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FOUND                       VALUE '  '.                  
017300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017500                                                                          
017600 01  GOOD-STATUSCODES.                                                    
017700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800                                                                          
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100     EJECT                                                                
018200*    --- IMS FUNCTION CODES                                               
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018700                                                                          
018800 01  DLI-IO-AREA.                                                         
018900     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
019000                                                                          
019100     03  WLKREE01 REDEFINES IO-AREA.                                      
019200*        05  -COPY WDA201                                                 
019300                                                                          
019400     03  WLKREE11 REDEFINES IO-AREA.                                      
019500*        05  -COPY WDA211                                                 
019600     SKIP2                                                                
019700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
019800 01  DLI-IO-WDK611.                                                       
019900*    03  -COPY WDK611                                                     
020000     SKIP2                                                                
020100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK711'.        
020200 01  DLI-IO-WDK711.                                                       
020300*    03  -COPY WDK711                                                     
020400     SKIP2                                                                
020500     EJECT                                                                
020600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB201'.           
020700 01  DLI-IO-WDB201.                                                       
020800*    03  -COPY WDB201.                                                    
020900     EJECT                                                                
021000 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB101'.           
021100 01  DLI-IO-WDB101.                                                       
021200*    03  -COPY WDB101.                                                    
021300     EJECT                                                                
021400 LINKAGE SECTION.                                                         
021500                                                                          
021600*01  -COPY W0009   -PRE MSG-                                              
021700*01  -COPY W0008   -PRE USEA-                                             
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008  -PRE KREE-                                              
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008  -PRE ARTC-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008  -PRE ARTS-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE WDB1-                                          
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008      -PRE WDB2-                                          
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500 PROCEDURE DIVISION  USING MSG-PCB                                        
023600                           USEA-PCB                                       
023700                           KREE-PCB                                       
023800                           ARTC-PCB                                       
023900                           ARTS-PCB                                       
024000                           WDB1-PCB                                       
024100                           WDB2-PCB.                                      
024200     ENTRY 'DLITCBL' USING MSG-PCB                                        
024300                           USEA-PCB                                       
024400                           KREE-PCB                                       
024500                           ARTC-PCB                                       
024600                           ARTS-PCB                                       
024700                           WDB1-PCB                                       
024800                           WDB2-PCB.                                      
024900                                                                          
025000     PERFORM IMS-GET-MSG                                                  
025100     IF SEGMENT-FOUND                                                     
025200       PERFORM A-INIT                                                     
025300       PERFORM B-CHECK-KEYS                                               
025400       IF KEYS-OK                                                         
025500         IF MFS-FIRST                                                     
025600           PERFORM C-FIRST-PAGE                                           
025700         ELSE                                                             
025800           IF MFS-NEXT                                                    
025900             PERFORM D-NEXT-PAGE                                          
026000           ELSE                                                           
026100             PERFORM E-SAME-PAGE                                          
026200           END-IF                                                         
026300         END-IF                                                           
026400         PERFORM F-READ-SHOW-INFO                                         
026500       END-IF                                                             
026600       MOVE MAX-MOD-LENGTH TO MSG-KVLL                                    
026700       PERFORM IMS-INSERT-MSG                                             
026800     END-IF                                                               
026900                                                                          
027000     MOVE ZERO TO RETURN-CODE                                             
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500                                                                          
027600     IF MSG-DOUBLE-TRANSACTIONS                                           
027700       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I71401                 
027800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028000     ELSE                                                                 
028100       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I71401                  
028200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
028300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028400     END-IF                                                               
028500                                                                          
028600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028900                                                                          
029000     MOVE LOW-VALUE TO MSG-AREA                                           
029100     MOVE 'W4O71401' TO MFS-IDMOD                                         
029200     MOVE '4714' TO MOD-IDTRANS                                           
029300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
029400                                                                          
029500     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71401 + 4                        
029600                                                                          
029700     IF OWN-MID OR HELP-MID                                               
029800       CONTINUE                                                           
029900     ELSE                                                                 
030000       MOVE SPACE TO MFS-KDTRTYP                                          
030100       MOVE '7' TO MFS-IDPFK                                              
030200     END-IF                                                               
030300                                                                          
030400     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
030500                                                                          
030600     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
030700     .                                                                    
030800     EJECT                                                                
030900 B-CHECK-KEYS SECTION.                                                    
031000                                                                          
031100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031200     MOVE '001'             TO MSGI-KDCALL                                
031300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031400     MOVE '4714'            TO MSGI-IDTRANS                               
031500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031600     IF GOOD-MID                                                          
031700       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
031800       MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                           
031900       MOVE MID-IDRAPPNR-IN    TO MSGI-IDRAPPNR                           
032000       IF MID-IDARTNR-IN  = ALL '+'                                       
032100          MOVE '+++++++++'     TO MSGI-IDARTNR                            
032200       ELSE                                                               
032300          MOVE MID-IDARTNR-IN  TO WS-IDARTNR                              
032400          MOVE WS-IDARTNR      TO MSGI-IDARTNR                            
032500       END-IF                                                             
032600       MOVE MID-IDRADNR-IN     TO MSGI-IDRADNR                            
032700     END-IF                                                               
032800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032900                                                                          
033000     IF MSGI-IDLAND-SPR = 'GB'                                            
033100       MOVE 'GB'                TO MED-IDSKYLT                            
033200     ELSE                                                                 
033300       MOVE 'S '                TO MED-IDSKYLT                            
033400     END-IF                                                               
033500                                                                          
033600     MOVE YES TO KEYS-SW                                                  
033700                                                                          
033800     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
033900                             MOD-IDKUNDNR-IN                              
034000                             MOD-IDRAPPNR-IN                              
034100                             MOD-IDARTNR-IN                               
034200                             MOD-IDRADNR-IN                               
034300                                                                          
034400                                                                          
034500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
034600       MOVE '7'         TO MFS-IDPFK                                      
034700       MOVE SPACE       TO MFS-KDTRTYP                                    
034800     END-IF                                                               
034900                                                                          
035000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
035100       MOVE '7'         TO MFS-IDPFK                                      
035200       MOVE SPACE       TO MFS-KDTRTYP                                    
035300     END-IF                                                               
035400                                                                          
035500     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
035600       MOVE '7'         TO MFS-IDPFK                                      
035700       MOVE SPACE       TO MFS-KDTRTYP                                    
035800     END-IF                                                               
035900                                                                          
035910     IF MSGI-IDDISTR NOT NUMERIC OR MSGI-IDDISTR <= 0                     
035920       MOVE NOO                  TO KEYS-SW                               
035930     ELSE                                                                 
036000       MOVE MSGI-IDDISTR  TO W-IDDISTR                                    
036100                             W-IDDISTR-WDB2                               
036200                             TEST-IDDISTR                                 
036210     END-IF                                                               
036220                                                                          
036230     IF MSGI-IDKUNDNR NOT NUMERIC OR MSGI-IDKUNDNR < 0                    
036240       MOVE NOO                  TO KEYS-SW                               
036250     ELSE                                                                 
036300       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
036400                             W-IDKUNDNR-WDB2                              
036410     END-IF                                                               
036420                                                                          
036500     MOVE MSGI-IDRAPPNR TO W-IDRAPPNR                                     
036600     MOVE MSGI-IDRADNR                    TO IDRADNR-WS                   
036700     MOVE MSGI-IDARTNR (2:8)              TO IDARTNR-WS                   
036800                                                                          
036900     IF MID-IDARTNR-IN NOT  = ALL '+'                                     
037000       MOVE '7'                  TO MFS-IDPFK                             
037100       MOVE SPACE                TO MFS-KDTRTYP                           
037200       IF 4737-MID                                                        
037300          IF IDARTNR-WS  NOT NUMERIC                                      
037400             MOVE ZERO           TO IDARTNR-WS                            
037500          END-IF                                                          
037600       END-IF                                                             
037700     ELSE                                                                 
037800       IF GOOD-MID                                                        
037900          MOVE MID-IDARTNR-UT             TO IDARTNR-WS                   
038000       END-IF                                                             
038100       IF 4737-MID                                                        
038200          IF IDARTNR-WS  NOT NUMERIC                                      
038300             MOVE ZERO           TO IDARTNR-WS                            
038400          END-IF                                                          
038500       END-IF                                                             
038600     END-IF                                                               
038700                                                                          
038800     IF MID-IDRADNR-IN NOT = ALL '+'                                      
038900       MOVE '7'                  TO MFS-IDPFK                             
039000       MOVE SPACE                TO MFS-KDTRTYP                           
039100       IF 4737-MID                                                        
039200          MOVE ZERO              TO IDRADNR-WS                            
039300       END-IF                                                             
039400     ELSE                                                                 
039500       IF GOOD-MID                                                        
039600          MOVE MID-IDRADNR-UT             TO IDRADNR-WS                   
039700       END-IF                                                             
039800     END-IF                                                               
039900                                                                          
040000     INSPECT IDARTNR-WS  REPLACING LEADING SPACE BY ZERO                  
040100     INSPECT IDRADNR-WS  REPLACING LEADING SPACE BY ZERO                  
040200                                                                          
040300     IF IDARTNR-WS NOT NUMERIC OR IDRADNR-WS NOT NUMERIC                  
040400       MOVE NOO                  TO KEYS-SW                               
040500     ELSE                                                                 
040600       MOVE IDARTNR-WS           TO W-IDARTNR                             
040700       MOVE IDRADNR-WS           TO W-IDRADNR                             
040800     END-IF                                                               
040900                                                                          
041400     IF KEYS-OK                                                           
041500       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
041600       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
041700       MOVE MSGI-IDRAPPNR        TO MOD-IDRAPPNR-UT                       
041800       MOVE IDARTNR-WS           TO MOD-IDARTNR-UT                        
041900       MOVE IDRADNR-WS           TO MOD-IDRADNR-UT                        
042000     ELSE                                                                 
042100       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                             
042200                               MOD-IDKUNDNR-UT                            
042300                               MOD-IDRAPPNR-UT                            
042400                               MOD-IDARTNR-UT                             
042500                               MOD-IDRADNR-UT                             
042600     END-IF                                                               
042700                                                                          
042800     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
042900     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
043000     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
043100     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
043200     INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE              
043300     IF MOD-IDKUNDNR-UT          = SPACE                                  
043400        MOVE '     0'           TO MOD-IDKUNDNR-UT                        
043500     END-IF                                                               
043600                                                                          
043700     IF KEYS-WRONG                                                        
043800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
043900       CALL WMEDKONV USING MED-WMEDAREA                                   
044000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044100       PERFORM MFS-ERASE-FIELD-IN                                         
044200       PERFORM MFS-ERASE-FIELD-OUT                                        
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 C-FIRST-PAGE SECTION.                                                    
044700                                                                          
044800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044900     CALL WMEDKONV USING MED-WMEDAREA                                     
045000     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
045100                                                                          
045200     PERFORM MFS-ERASE-FIELD-IN                                           
045300     .                                                                    
045400     EJECT                                                                
045500 D-NEXT-PAGE SECTION.                                                     
045600                                                                          
045700     IF OWN-MID                                                           
045800       MOVE MID-IDARTNR-NEXT      TO W-IDARTNR                            
045900       MOVE MID-IDRADNR-NEXT      TO W-IDRADNR                            
046000     ELSE                                                                 
046100       MOVE IDARTNR-WS            TO W-IDARTNR                            
046200       MOVE IDRADNR-WS            TO W-IDRADNR                            
046300       PERFORM MFS-ERASE-FIELD-IN                                         
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 E-SAME-PAGE SECTION.                                                     
046800                                                                          
046900     IF GOOD-MID OR HELP-MID                                              
047000       MOVE MID-IDARTNR-ENTER     TO W-IDARTNR                            
047100       MOVE MID-IDRADNR-ENTER     TO W-IDRADNR                            
047200     ELSE                                                                 
047300       MOVE ZERO                  TO W-IDARTNR                            
047400                                     W-IDRADNR                            
047500       PERFORM MFS-ERASE-FIELD-IN                                         
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900 F-READ-SHOW-INFO SECTION.                                                
048000                                                                          
048100     PERFORM IMS-GET-KREE01                                               
048200                                                                          
048300     IF SEGMENT-MISSING                                                   
048400        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
048500        CALL WMEDKONV USING MED-WMEDAREA                                  
048600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
048700        PERFORM MFS-ERASE-FIELD-OUT                                       
048800     ELSE                                                                 
048900       MOVE ANM-DALEVANM (3:6)   TO MOD-TILEVANM                          
049000       MOVE ANM-RELANDCO         TO MOD-RELANDCO                          
049100                                    WS-RELANDCO                           
049200       MOVE ANM-PRFRAKT          TO MOD-PRFRAKT                           
049300       MOVE ANM-PRFOERS          TO MOD-PRFOERS                           
049400       MOVE ANM-PRLEGKST         TO MOD-PRLEGKST                          
049500                                                                          
049600       IF DIST79-DEALER-PRICE OR                                          
049620          DIST79-ECOM-PRICE                                               
049700         MOVE ANM-KDVALISO       TO MOD-KDVALISO                          
049800         IF ANM-KDVALISO = SPACE                                          
049900           PERFORM S10-HAMTA-KDVALISO                                     
050000         END-IF                                                           
050100       ELSE                                                               
050200*CHINA-PRICE1                                                             
050210*INDIA-PRICE1                                                             
050300         MOVE ANM-IDFTG TO WS-IDFTG                                       
050310         EVALUATE TRUE                                                    
050400         WHEN IDFTG-CN                                                    
050500             MOVE 'CNY'         TO MOD-KDVALISO                           
050610         WHEN IDFTG-IN                                                    
050620             MOVE 'INR'         TO MOD-KDVALISO                           
050640         WHEN IDFTG-KR                                                    
050650             MOVE 'KRW'         TO MOD-KDVALISO                           
050670         WHEN IDFTG-TR                                                    
050680             MOVE 'TRY'         TO MOD-KDVALISO                           
050690         WHEN IDFTG-MX                                                    
050691             MOVE 'MXN'         TO MOD-KDVALISO                           
050692         WHEN IDFTG-BR                                                    
050693             MOVE 'BRL'         TO MOD-KDVALISO                           
050694         WHEN IDFTG-MY                                                    
050695             MOVE 'MYR'         TO MOD-KDVALISO                           
050696         WHEN IDFTG-TH                                                    
050697             MOVE 'THB'         TO MOD-KDVALISO                           
050698         WHEN IDFTG-TW                                                    
050699             MOVE 'TWD'         TO MOD-KDVALISO                           
050700         WHEN IDFTG-ZA                                                    
050701             MOVE 'ZAR'         TO MOD-KDVALISO                           
050710         WHEN OTHER                                                       
050800             MOVE 'SEK'         TO MOD-KDVALISO                           
050810         END-EVALUATE                                                     
050900       END-IF                                                             
051000                                                                          
051100       MOVE +1 TO INDX                                                    
051200                                                                          
051300       PERFORM IMS-GNP-KREE11                                             
051400       IF SEGMENT-FOUND                                                   
051500         MOVE LEV-IDARTNR        TO MOD-IDARTNR-ENTER                     
051600         MOVE LEV-IDRADNR        TO MOD-IDRADNR-ENTER                     
051700       ELSE                                                               
051800         MOVE ZERO               TO MOD-IDARTNR-ENTER                     
051900                                    MOD-IDRADNR-ENTER                     
052000                                    MOD-IDARTNR-NEXT                      
052100                                    MOD-IDRADNR-NEXT                      
052200       END-IF                                                             
052300                                                                          
052400       PERFORM UNTIL INDX > MAX-INDX                                      
052500         IF SEGMENT-FOUND                                                 
052600           MOVE LEV-IDARTNR      TO MOD-IDARTNR    (INDX)                 
052700           MOVE LEV-IDRADNR      TO MOD-IDRADNR    (INDX)                 
052800           MOVE LEV-KDANMORS     TO MOD-KDANMORS   (INDX)                 
052900                                                                          
053000           IF DIST79-DEALER-PRICE OR                                      
053020              DIST79-ECOM-PRICE                                           
053100             COMPUTE MOD-PRARTNTO (INDX) =                                
053200                     LEV-PRARTBTO-LOC * LEV-KVLEVANM-BEKR                 
053300           ELSE                                                           
053400*                                                                         
053401*CHINA-PRICE2                                                             
053410*INDIA-PRICE2                                                             
053420*KOREA-PRICE2                                                             
053430*TURKEY-PRICE2                                                            
053440*MEXICO-PRICE2                                                            
053450*BRASIL-PRICE2                                                            
053500             MOVE LEV-IDFTG      TO WS-IDFTG                              
053600             IF IDFTG-CN OR                                               
053610                IDFTG-IN OR                                               
053620                IDFTG-KR OR                                               
053630                IDFTG-TR OR                                               
053631                IDFTG-MX OR                                               
053632                IDFTG-BR OR                                               
053640                IDFTG-MY OR                                               
053650                IDFTG-TH OR                                               
053660                IDFTG-TW OR                                               
053670                IDFTG-ZA                                                  
053700               COMPUTE MOD-PRARTNTO (INDX) =                              
053800                     LEV-PRARTBTO-LOCINV * LEV-KVLEVANM-BEKR              
053940             ELSE                                                         
054000               COMPUTE MOD-PRARTNTO (INDX) =                              
054100                     LEV-PRARTBTO * LEV-KVLEVANM-BEKR                     
054200             END-IF                                                       
054300           END-IF                                                         
054400                                                                          
054500           PERFORM FA-COMPUTE-ADD-COSTS                                   
054600           MOVE WS-PRCOST        TO MOD-PRARTBTO   (INDX)                 
054700           PERFORM FB-FIND-STORAGE                                        
054800           PERFORM IMS-GNP-KREE11                                         
054900         ELSE                                                             
055000           MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR    (INDX)                 
055100                                    MOD-KDANMORS   (INDX)                 
055200                                    MOD-PRARTNTO   (INDX)                 
055300                                    MOD-PRARTBTO   (INDX)                 
055400         END-IF                                                           
055500         ADD 1 TO INDX                                                    
055600       END-PERFORM                                                        
055700                                                                          
055800       IF SEGMENT-FOUND                                                   
055900         MOVE LEV-IDARTNR        TO MOD-IDARTNR-NEXT                      
056000         MOVE LEV-IDRADNR        TO MOD-IDRADNR-NEXT                      
056100         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
056200         CALL WMEDKONV USING MED-WMEDAREA                                 
056300         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
056400       ELSE                                                               
056500         MOVE ZERO             TO MOD-IDARTNR-NEXT                        
056600                                  MOD-IDRADNR-NEXT                        
056700       END-IF                                                             
056800     END-IF                                                               
056900     .                                                                    
057000     EJECT                                                                
057100 FA-COMPUTE-ADD-COSTS SECTION.                                            
057200                                                                          
057300     MOVE LEV-KDANMORS  TO OKOD-KDANMORS                                  
057400*--ANROPA KONTROLL AV ORSAKSKODER                                         
057500     CALL W418OKOD USING OKOD-W418OKOD                                    
057600     IF OKOD-FL-KOD-SOM-BAER-TK = 'J'                                     
057700       IF DIST79-DEALER-PRICE OR                                          
057720          DIST79-ECOM-PRICE                                               
057800         COMPUTE WS-PRCOST ROUNDED =                                      
057900         ((LEV-PRARTBTO-LOC * LEV-KVLEVANM-BEKR) *                        
058000                                 WS-RELANDCO) / 100                       
058100       ELSE                                                               
058200*CHINA-PRICE3                                                             
058210*INDIA-PRICE3                                                             
058220*KOREA-PRICE3                                                             
058230*TURKEY-PRICE3                                                            
058240*MEXICO-PRICE3                                                            
058250*BRASIL-PRICE3                                                            
058350         MOVE LEV-IDFTG        TO WS-IDFTG                                
058400         IF IDFTG-CN OR                                                   
058410            IDFTG-IN OR                                                   
058420            IDFTG-KR OR                                                   
058430            IDFTG-TR OR                                                   
058431            IDFTG-MX OR                                                   
058432            IDFTG-BR OR                                                   
058440            IDFTG-MY OR                                                   
058450            IDFTG-TH OR                                                   
058460            IDFTG-TW OR                                                   
058470            IDFTG-ZA                                                      
058500           COMPUTE WS-PRCOST ROUNDED =                                    
058600           ((LEV-PRARTBTO-LOCINV * LEV-KVLEVANM-BEKR) *                   
058700           WS-RELANDCO) / 100                                             
058800         ELSE                                                             
058900           COMPUTE WS-PRCOST ROUNDED =                                    
059000           ((LEV-PRARTBTO * LEV-KVLEVANM-BEKR) *                          
059100           WS-RELANDCO) / 100                                             
059200         END-IF                                                           
059300       END-IF                                                             
059400     ELSE                                                                 
059500        MOVE ZERO             TO WS-PRCOST                                
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 FB-FIND-STORAGE SECTION.                                                 
060000                                                                          
060100     MOVE LEV-IDARTNR          TO WW-IDARTNR                              
060200                                                                          
060300     MOVE LEV-IDDC               TO WS-IDDC                               
060400     IF CDC-SE OR GOOD-DDC                                                
060500       PERFORM IMS-GU-WLARTC11                                            
060600       IF SEGMENT-FOUND                                                   
060700          MOVE CLAG-ADLAGOMR     TO MOD-ADLAGOMR (INDX)                   
060800          MOVE CLAG-ADGANG       TO MOD-ADGANG   (INDX)                   
060900          MOVE CLAG-ADPLATS      TO MOD-ADPLATS  (INDX)                   
061000       ELSE                                                               
061100          MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR (INDX)                   
061200                                    MOD-ADGANG   (INDX)                   
061300                                    MOD-ADPLATS  (INDX)                   
061400       END-IF                                                             
061500     ELSE                                                                 
061600       MOVE LEV-IDDC TO W-IDDC                                            
061700                                                                          
061800       PERFORM IMS-GU-WLARTS11                                            
061900       IF SEGMENT-FOUND                                                   
062000          MOVE SLAG-ADLAGOMR     TO MOD-ADLAGOMR (INDX)                   
062100          MOVE SLAG-ADGANG       TO MOD-ADGANG   (INDX)                   
062200          MOVE SLAG-ADPLATS      TO MOD-ADPLATS  (INDX)                   
062300       ELSE                                                               
062400          MOVE MFS-RENSA-FAELT   TO MOD-ADLAGOMR (INDX)                   
062500                                    MOD-ADGANG   (INDX)                   
062600                                    MOD-ADPLATS  (INDX)                   
062700       END-IF                                                             
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 S10-HAMTA-KDVALISO   SECTION.                                            
063200                                                                          
063300     PERFORM IMS-GU-GMTA-WDB201                                           
063400     IF SEGMENT-FOUND                                                     
063500       CONTINUE                                                           
063600     ELSE                                                                 
063700       PERFORM IMS-GET-WDB201                                             
063800     END-IF                                                               
063900     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
064000     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
064100     PERFORM IMS-GU-WDB1-WDB101                                           
064200     IF SEGMENT-FOUND                                                     
064300       IF DIST79-DEALER-PRICE                                             
064400         MOVE BET-KDVALISO   TO MOD-KDVALISO                              
064500       ELSE                                                               
064600         IF DIST79-ECOM-PRICE                                             
064800           MOVE SPACE        TO MOD-KDVALISO                              
064910         ELSE                                                             
064920           MOVE 'SEK'        TO MOD-KDVALISO                              
064921         END-IF                                                           
064930       END-IF                                                             
065000     ELSE                                                                 
065100       MOVE SPACE            TO MOD-KDVALISO                              
065200     END-IF                                                               
065300     .                                                                    
065400     EJECT                                                                
066700 MFS-ERASE-FIELD-OUT SECTION.                                             
066800                                                                          
066900*    --- ALLA UTDATA-FÄLT                                                 
067000*    --- INCL. SCROLL KEYS                                                
067100     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
067200                             MOD-IDKUNDNR-UT                              
067300                             MOD-IDRAPPNR-UT                              
067400                             MOD-IDARTNR-UT                               
067500                             MOD-IDRADNR-UT                               
067600                             MOD-IDARTNR-ENTER                            
067700                             MOD-IDARTNR-NEXT                             
067800                             MOD-IDRADNR-ENTER                            
067900                             MOD-IDRADNR-NEXT                             
068000     .                                                                    
068100 MFS-ERASE-FIELD-IN SECTION.                                              
068200                                                                          
068300*    --- ALLA INDATA-FÄLT                                                 
068400     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
068500                             MOD-IDKUNDNR-IN                              
068600                             MOD-IDRAPPNR-IN                              
068700                             MOD-IDARTNR-IN                               
068800                             MOD-IDRADNR-IN                               
068900     .                                                                    
069000     EJECT                                                                
069100* --- IMS SECTIONS ---                                                    
069200                                                                          
069300 IMS-GET-MSG SECTION.                                                     
069400                                                                          
069500     MOVE '  QC' TO GOOD-STATUSCODES                                      
069600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069800     PERFORM IMS-STATUSCHECK                                              
069900     .                                                                    
070000                                                                          
070100 IMS-INSERT-MSG SECTION.                                                  
070200                                                                          
070300     IF MSGI-IDLAND-SPR = 'GB'                                            
070400       MOVE 'N' TO MFS-KDHUVOMR                                           
070500     END-IF                                                               
070600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070700     MOVE SPACE TO GOOD-STATUSCODES                                       
070800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
070900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071000     PERFORM IMS-STATUSCHECK                                              
071100     .                                                                    
071200     EJECT                                                                
071300 IMS-GET-KREE01 SECTION.                                                  
071400                                                                          
071500     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
071600          DELIMITED BY SIZE INTO SSA1                                     
071700     MOVE '  GE' TO GOOD-STATUSCODES                                      
071800     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
071900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSCHECK                                              
072100     .                                                                    
072200     EJECT                                                                
072300 IMS-GNP-KREE11 SECTION.                                                  
072400                                                                          
072500     STRING 'WLKREE11(WDA211KY>=' W-WDA2KEY-X ')'                         
072600          DELIMITED BY SIZE INTO SSA1                                     
072700     MOVE '  GE' TO GOOD-STATUSCODES                                      
072800     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
072900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUSCHECK                                              
073100     .                                                                    
073200     EJECT                                                                
073300 IMS-GU-WLARTC11     SECTION.                                             
073400                                                                          
073500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
073600          DELIMITED BY SIZE INTO SSA1                                     
073700     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
073800          DELIMITED BY SIZE INTO SSA2                                     
073900     MOVE '  GE' TO GOOD-STATUSCODES                                      
074000     CALL CBLTDLI USING GU ARTC-PCB CLAG-WDK611 SSA1 SSA2                 
074100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
074200     PERFORM IMS-STATUSCHECK                                              
074300     .                                                                    
074400     EJECT                                                                
074500 IMS-GU-WLARTS11     SECTION.                                             
074600                                                                          
074700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
074800          DELIMITED BY SIZE INTO SSA1                                     
074900     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
075000          DELIMITED BY SIZE INTO SSA2                                     
075100     MOVE '  GE' TO GOOD-STATUSCODES                                      
075200     CALL CBLTDLI USING GU ARTS-PCB SLAG-WDK711 SSA1 SSA2                 
075300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
075400     PERFORM IMS-STATUSCHECK                                              
075500     .                                                                    
075600     EJECT                                                                
075700 IMS-GU-GMTA-WDB201              SECTION.                                 
075800                                                                          
075900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
076000          DELIMITED BY SIZE INTO SSA1                                     
076100     MOVE '  GE'              TO GOOD-STATUSCODES                         
076200                                                                          
076300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
076400     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
076500     PERFORM IMS-STATUSCHECK                                              
076600     .                                                                    
076700     EJECT                                                                
076800 IMS-GET-WDB201 SECTION.                                                  
076900                                                                          
077000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
077100                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     MOVE '    '              TO GOOD-STATUSCODES                         
077400                                                                          
077500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
077600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
077700     PERFORM IMS-STATUSCHECK                                              
077800     .                                                                    
077900      EJECT                                                               
078000 IMS-GU-WDB1-WDB101              SECTION.                                 
078100                                                                          
078200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
078300          DELIMITED BY SIZE INTO SSA1                                     
078400     MOVE '  GE'              TO GOOD-STATUSCODES                         
078500                                                                          
078600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
078700     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
078800     PERFORM IMS-STATUSCHECK                                              
078900     .                                                                    
079000     EJECT                                                                
079100 IMS-STATUSCHECK SECTION.                                                 
079200                                                                          
079300     SET STATUS-IX TO 1                                                   
079400     SEARCH GOOD-STATUS                                                   
079500       AT END                                                             
079600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
079700         DELIMITED BY SIZE INTO ERROR-TEXT                                
079800         CALL FELLOG                                                      
079900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
080000         CONTINUE                                                         
080100     END-SEARCH                                                           
080200     .                                                                    
