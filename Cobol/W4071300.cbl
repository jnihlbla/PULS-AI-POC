000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4071300.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   95/08/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    OBS! UPPDATERINGSMÖJLIGHETEN ÄR SLÄCKT TILLS VIDARE.FÄLTEN           
000900*         ÄR STÄNGDA/SLÄCKA I FORMATET.UPPDAT. STJÄRNMÄRKT I PGM.         
001000*         KAN EV. TAS BORT HELT LÄNGRE FRAM.OSÄKERT I NULÄGET.            
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        QUESTION ON DESCREPANCY SCREEN-2.                                
001400*        KEYS THAT HAV TO BE FILLED IN: DISTR/CUST/REPORT.NO              
001500*        READS DATABASE WLKREE AND SHOWS INFO.                            
001600*        UPPDATES IDFTG AND IDANALYSNR ON LINE 19                         
001700*                                                                         
001800*        THE PROGRAM READS     WLKREE (WDA2)                              
001900*        THE PROGRAM UPDATES   WLKREE (WDA2)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W4T713                                              
002300*                     W4T713U                                             
002400*        MID:         W4I71301                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        MOD:         W4O71301                                            
002800                                                                          
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -- CHECKED BY WY2000                                                 
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W4071300'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
004000                                                                          
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FOR SCROLL LINES                                           
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004700 77  MAX-MOD-LENGTH              PIC S9(5)  VALUE +1200 COMP SYNC.        
004800 77  DUMMY-IDARTNR               PIC S9(9)  COMP-3 VALUE +100.            
004900 77  IDARTNR-WS                  PIC X(8)   VALUE ZERO.                   
005000 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
005100 77  IDRADNR-WS                  PIC 9(4)   VALUE ZERO.                   
005200 77  DUMMY-NR-SW                 PIC X      VALUE 'N'.                    
005300*    --- WORKAREA FOR IDANALYSNR                                          
005400 01  FILLER                      PIC X(8)   VALUE 'ANALYSNR'.             
005500 01  IDANALYSNR-SPAR             PIC 9(8).                                
005900                                                                          
006000*    --- WORKAREA FOR KDKREBEH                                            
006100 01  FILLER                      PIC X(8)   VALUE 'KDKREBEH'.             
006200 01  SPAR-KDKREBEH               PIC X(2).                                
006300 01  TEST-KDKREBEH.                                                       
006400     03 KDKREBEH-1               PIC X.                                   
006500     03 KDKREBEH-2               PIC X.                                   
006600     03 KDKREBEH-3               PIC X.                                   
006700*    --- WORKAREA FOR IDFTG                                               
006800 01  FILLER                      PIC X(8)   VALUE 'IDFTG   '.             
006900 01  TEST-IDFTG                  PIC X(2).                                
007000     88 IDFTG-GODKAEND           VALUE '05'                               
007100                                       '06'                               
007200                                       '07'                               
007300                                       '09'                               
007400                                       '53'                               
007500                                       '54'                               
007600                                       '57'                               
007700                                       '75'                               
007800                                       '76'                               
007900                                       '90'.                              
008000 01  IDFTG-SPAR                  PIC 9(02).                               
008100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
008200                                                                          
008300 77  INDATA-SW                   PIC X      VALUE 'N'.                    
008400     88  INDATA-OK                           VALUE 'Y'.                   
008500                                                                          
008600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008700     88  KEYS-OK                             VALUE 'Y'.                   
008800     88  KEYS-WRONG                          VALUE 'N'.                   
008900                                                                          
009000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009100     88  OWN-MID                             VALUE '4713'.                
009200     88  GOOD-MID                            VALUE '4711' '4712'          
009300                                                   '4713' '4714'          
009400                                                   '4715' '4716'.         
009500     88  HELP-MID                            VALUE '0551'.                
009600     88  4737-MID                            VALUE '4737'.                
009700     EJECT                                                                
009800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009900 01  GENERAL-SUBPROGRAM.                                                  
010000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     EJECT                                                                
010500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010600*01 -COPY WMEDAREA                                                        
010700                                                                          
010800 01  MESSAGE-CODES.                                                       
010900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011000     03  INF-UPDATE-OK           PIC X(3)    VALUE '101'.                 
011100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011300     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
011400     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
011500     03  ERR-LINE-MISSING        PIC X(3)    VALUE '005'.                 
011600     EJECT                                                                
011700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000                                                                          
012100*01 -COPY WMSGINIT                                                        
012200                                                                          
012300 01 W-MINKEY.                                                             
012400    03 W-MINKEY-IDARTNR          PIC S9(9)   COMP-3.                      
012500    03 W-MINKEY-IDRADNR          PIC S9(9)   COMP-3.                      
012600                                                                          
012700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013000                                                                          
013100*01  MID -COPY W4I71301                                                   
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013400                                                                          
013500*01  -COPY WMSGAREA                                                       
013600     EJECT                                                                
013700     03  MOD REDEFINES MSG-AREA.                                          
013800*      05  -COPY W4O71301                                                 
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014100                                                                          
014200*01  -COPY WMFSAREA                                                       
014300     EJECT                                                                
014400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014500*                                                                         
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014800                                                                          
014900 01  KEYS-TO-DLI.                                                         
015000     03  W-IDLEVANM-X.                                                    
015100         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015200         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015300         05  W-IDRAPPNR          PIC 9(7).                                
015400     03  W-WDA2KEY-X.                                                     
015500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015600         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
015700     03  W-IDSKYLT-X.                                                     
015800         05  W-IDSKYLT           PIC X(3).                                
015900     03  W-IDARTNR-X.                                                     
016000         05  W-IDARTNR-BENA      PIC S9(9)   VALUE ZERO COMP-3.           
016100                                                                          
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FOUND                       VALUE '  '.                  
016500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016700                                                                          
016800 01  GOOD-STATUSCODES.                                                    
016900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000                                                                          
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNCTION CODES                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA201'.         
017900                                                                          
018000 01  DLI-IO-AREA-WDA201.                                                  
018100   03  WLKREE01.                                                          
018200*    05  -COPY WDA201                                                     
018300                                                                          
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA211'.         
018600                                                                          
018700 01  DLI-IO-AREA-WDA211.                                                  
018800   03  WLKREE11.                                                          
018900*        05  -COPY WDA211                                                 
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD311'.         
019200                                                                          
019300 01  DLI-IO-AREA-WDD311.                                                  
019400   03  WLBENA11.                                                          
019500*    05  -COPY WDD311                                                     
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800                                                                          
019900*01  -COPY W0009   -PRE MSG-                                              
020000*01  -COPY W0008   -PRE USEA-                                             
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300*01  -COPY W0008  -PRE KREE-                                              
020400     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020600*01  -COPY W0008  -PRE BENA-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900 PROCEDURE DIVISION  USING MSG-PCB                                        
021000                           USEA-PCB                                       
021100                           KREE-PCB                                       
021200                           BENA-PCB.                                      
021300 MAIN SECTION.                                                            
021400     ENTRY 'DLITCBL' USING MSG-PCB                                        
021500                           USEA-PCB                                       
021600                           KREE-PCB                                       
021700                           BENA-PCB.                                      
021800                                                                          
021900     PERFORM IMS-GET-MSG                                                  
022000     IF SEGMENT-FOUND                                                     
022100       PERFORM A-INIT                                                     
022200       PERFORM B-CHECK-KEYS                                               
022300       IF KEYS-OK                                                         
022700         IF MFS-FIRST                                                     
022800           PERFORM C-FIRST-PAGE                                           
022900         ELSE                                                             
023000           IF MFS-NEXT                                                    
023100             PERFORM D-NEXT-PAGE                                          
023200           ELSE                                                           
023300             PERFORM E-SAME-PAGE                                          
023400           END-IF                                                         
023500         END-IF                                                           
023700         PERFORM F-READ-SHOW-INFO                                         
023800       END-IF                                                             
023900       MOVE MAX-MOD-LENGTH TO MSG-KVLL                                    
024000       PERFORM IMS-INSERT-MSG                                             
024100     END-IF                                                               
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     IF MSG-DOUBLE-TRANSACTIONS                                           
025000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I71301                 
025100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
025200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
025300     ELSE                                                                 
025400       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W4I71301                 
025500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
025600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
025700     END-IF                                                               
025800                                                                          
025900     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
026000     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
026100     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
026200                                                                          
026300     MOVE LOW-VALUE                       TO MSG-AREA                     
026400     MOVE 'W4O71301'                      TO MFS-IDMOD                    
026500     MOVE '4713'                          TO MOD-IDTRANS                  
026600     MOVE MFS-ERASE-FIELD                 TO MOD-TEMFSFEL                 
026700                                             MOD-TEMFSINF                 
026800                                                                          
026900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71301 + 4                        
027000                                                                          
027100     IF OWN-MID OR HELP-MID                                               
027200       CONTINUE                                                           
027300     ELSE                                                                 
027400       MOVE SPACE                         TO MFS-KDTRTYP                  
027500       MOVE '7'                           TO MFS-IDPFK                    
027600     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 B-CHECK-KEYS SECTION.                                                    
028000                                                                          
028100     MOVE ALL '+'                         TO MSGI-WMSGINIT                
028200     MOVE '001'                           TO MSGI-KDCALL                  
028300     MOVE MSG-SIGNON-USERID               TO MSGI-IDUSER                  
028400     MOVE '4713'                          TO MSGI-IDTRANS                 
028500     MOVE MSG-LTERM-NAME                  TO MSGI-IDLTERM-USER            
028600     IF GOOD-MID                                                          
028700       MOVE MID-IDDISTR-IN                TO MSGI-IDDISTR                 
028800       MOVE MID-IDKUNDNR-IN               TO MSGI-IDKUNDNR                
028900       MOVE MID-IDRAPPNR-IN               TO MSGI-IDRAPPNR                
029000       IF MID-IDARTNR-IN  = ALL '+'                                       
029100          MOVE '+++++++++'                TO MSGI-IDARTNR                 
029200       ELSE                                                               
029300          MOVE MID-IDARTNR-IN             TO WS-IDARTNR                   
029400          MOVE WS-IDARTNR                 TO MSGI-IDARTNR                 
029500       END-IF                                                             
029600       MOVE MID-IDRADNR-IN                TO MSGI-IDRADNR                 
029700     END-IF                                                               
029800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029900                                                                          
030000     IF MSGI-IDLAND-SPR = 'GB'                                            
030100       MOVE 'GB'                          TO MED-IDSKYLT                  
030200     ELSE                                                                 
030300       MOVE 'S '                          TO MED-IDSKYLT                  
030400     END-IF                                                               
030500                                                                          
030600     MOVE YES                             TO KEYS-SW                      
030700                                                                          
030800     MOVE MFS-ERASE-FIELD                 TO MOD-IDDISTR-IN               
030900                                             MOD-IDKUNDNR-IN              
031000                                             MOD-IDRAPPNR-IN              
031100                                             MOD-IDARTNR-IN               
031200                                             MOD-IDRADNR-IN               
031300                                                                          
031400     IF MID-IDDISTR-IN NOT = ALL '+'                                      
031500       MOVE '7'                           TO MFS-IDPFK                    
031600       MOVE SPACE                         TO MFS-KDTRTYP                  
031700     END-IF                                                               
031800                                                                          
031900     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
032000       MOVE '7'                           TO MFS-IDPFK                    
032100       MOVE SPACE                         TO MFS-KDTRTYP                  
032200     END-IF                                                               
032300                                                                          
032400     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
032500       MOVE '7'                           TO MFS-IDPFK                    
032600       MOVE SPACE                         TO MFS-KDTRTYP                  
032700     END-IF                                                               
032800                                                                          
032900     MOVE MSGI-IDDISTR                    TO W-IDDISTR                    
033000     MOVE MSGI-IDKUNDNR                   TO W-IDKUNDNR                   
033100     MOVE MSGI-IDRAPPNR                   TO W-IDRAPPNR                   
033200     MOVE MSGI-IDRADNR                    TO IDRADNR-WS                   
033300     MOVE MSGI-IDARTNR (2:8)              TO IDARTNR-WS                   
033400                                                                          
033500     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
033600       MOVE '7'                           TO MFS-IDPFK                    
033700       MOVE SPACE                         TO MFS-KDTRTYP                  
033800       IF 4737-MID                                                        
033900          IF IDARTNR-WS  NOT NUMERIC                                      
034000             MOVE ZERO           TO IDARTNR-WS                            
034100          END-IF                                                          
034200       END-IF                                                             
034300     ELSE                                                                 
034400       IF GOOD-MID                                                        
034500          MOVE MID-IDARTNR-UT             TO IDARTNR-WS                   
034600       END-IF                                                             
034700       IF 4737-MID                                                        
034800          IF IDARTNR-WS  NOT NUMERIC                                      
034900             MOVE ZERO           TO IDARTNR-WS                            
035000          END-IF                                                          
035100       END-IF                                                             
035200     END-IF                                                               
035300                                                                          
035400     IF MID-IDRADNR-IN NOT = ALL '+'                                      
035500       MOVE '7'                           TO MFS-IDPFK                    
035600       MOVE SPACE                         TO MFS-KDTRTYP                  
035700       IF 4737-MID                                                        
035800          MOVE ZERO              TO IDRADNR-WS                            
035900       END-IF                                                             
036000     ELSE                                                                 
036100       IF GOOD-MID                                                        
036200          MOVE MID-IDRADNR-UT             TO IDRADNR-WS                   
036300       END-IF                                                             
036400     END-IF                                                               
036500                                                                          
036600     INSPECT IDARTNR-WS  REPLACING LEADING SPACE BY ZERO                  
036700     INSPECT IDRADNR-WS  REPLACING LEADING SPACE BY ZERO                  
036800                                                                          
036900     IF IDARTNR-WS NOT NUMERIC                                            
037000       MOVE NOO                           TO KEYS-SW                      
037100     ELSE                                                                 
037200       MOVE IDARTNR-WS                    TO W-IDARTNR                    
037300       MOVE IDRADNR-WS                    TO W-IDRADNR                    
037400     END-IF                                                               
037500                                                                          
037600     IF IDRADNR-WS NOT NUMERIC                                            
037700       MOVE NOO                           TO KEYS-SW                      
037800     END-IF                                                               
037900                                                                          
038000     IF KEYS-OK                                                           
038100       MOVE MSGI-IDDISTR                  TO MOD-IDDISTR-UT               
038200       MOVE MSGI-IDKUNDNR                 TO MOD-IDKUNDNR-UT              
038300       MOVE MSGI-IDRAPPNR                 TO MOD-IDRAPPNR-UT              
038400       MOVE IDARTNR-WS                    TO MOD-IDARTNR-UT               
038500       MOVE IDRADNR-WS                    TO MOD-IDRADNR-UT               
038600     ELSE                                                                 
038700       MOVE MFS-ERASE-FIELD               TO MOD-IDDISTR-UT               
038800                                             MOD-IDKUNDNR-UT              
038900                                             MOD-IDRAPPNR-UT              
039000                                             MOD-IDARTNR-UT               
039100                                             MOD-IDRADNR-UT               
039200     END-IF                                                               
039300     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
039400     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
039500     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
039600     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
039700     INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE              
039800     IF MOD-IDKUNDNR-UT          = SPACE                                  
039900        MOVE '     0'                     TO MOD-IDKUNDNR-UT              
040000     END-IF                                                               
040100                                                                          
040200     IF KEYS-WRONG                                                        
040300       MOVE ERR-WRONG-KEY                 TO MED-IDMFSFEL                 
040400       CALL WMEDKONV USING MED-WMEDAREA                                   
040500       MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                 
040600       PERFORM MFS-ERASE-FIELD-IN                                         
040700       PERFORM MFS-ERASE-FIELD-OUT                                        
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 C-FIRST-PAGE SECTION.                                                    
041200                                                                          
041300     MOVE INF-FIRST-PAGE                  TO MED-IDMFSINF                 
041400     CALL WMEDKONV USING MED-WMEDAREA                                     
041500     MOVE MED-MFSINF                      TO MOD-TEMFSFEL                 
041600                                                                          
041700     PERFORM MFS-ERASE-FIELD-IN                                           
041800     MOVE MFS-ERASE-FIELD                  TO MOD-RAD19-IDARTNR           
041900                                              MOD-RAD19-IDRADNR           
042000                                              MOD-RAD19-IDFTG             
042100                                              MOD-RAD19-IDANALYSNR        
042200     .                                                                    
042300     EJECT                                                                
042400 D-NEXT-PAGE SECTION.                                                     
042500                                                                          
042600     IF OWN-MID                                                           
042700       MOVE MID-IDARTNR-NEXT               TO W-IDARTNR                   
042800       MOVE MID-IDRADNR-NEXT               TO W-IDRADNR                   
042900     ELSE                                                                 
043000       MOVE IDARTNR-WS                     TO W-IDARTNR                   
043100       MOVE IDRADNR-WS                     TO W-IDRADNR                   
043200       PERFORM MFS-ERASE-FIELD-IN                                         
043300     END-IF                                                               
043400     MOVE MFS-ERASE-FIELD                  TO MOD-RAD19-IDARTNR           
043500                                              MOD-RAD19-IDRADNR           
043600                                              MOD-RAD19-IDFTG             
043700                                              MOD-RAD19-IDANALYSNR        
043800     .                                                                    
043900     EJECT                                                                
044000 E-SAME-PAGE SECTION.                                                     
044100                                                                          
044200     IF GOOD-MID OR HELP-MID                                              
044300       MOVE MID-IDARTNR-ENTER              TO W-IDARTNR                   
044400       MOVE MID-IDRADNR-ENTER              TO W-IDRADNR                   
044500     ELSE                                                                 
044600       MOVE ZERO                           TO W-IDARTNR                   
044700                                              W-IDRADNR                   
044800       PERFORM MFS-ERASE-FIELD-IN                                         
044900     END-IF                                                               
045000     MOVE MFS-ERASE-FIELD                  TO MOD-RAD19-IDARTNR           
045100                                              MOD-RAD19-IDRADNR           
045200                                              MOD-RAD19-IDFTG             
045300                                              MOD-RAD19-IDANALYSNR        
045400     .                                                                    
045500     EJECT                                                                
045600 F-READ-SHOW-INFO SECTION.                                                
045700                                                                          
045800     PERFORM IMS-GET-KREE01                                               
045900                                                                          
046000     IF SEGMENT-MISSING                                                   
046100        MOVE ERR-WRONG-KEY                 TO MED-IDMFSFEL                
046200        CALL WMEDKONV USING MED-WMEDAREA                                  
046300        MOVE MED-MFSFEL                    TO MOD-TEMFSFEL                
046400        PERFORM MFS-ERASE-FIELD-OUT                                       
046500     ELSE                                                                 
046600       MOVE ANM-DALEVANM                   TO MOD-TILEVANM                
046700       MOVE +1                             TO INDX                        
046800       PERFORM IMS-GNP-KREE11                                             
046900       IF SEGMENT-FOUND                                                   
047000         MOVE LEV-IDARTNR                  TO MOD-IDARTNR-ENTER           
047100         MOVE LEV-IDRADNR                  TO MOD-IDRADNR-ENTER           
047200       ELSE                                                               
047300         MOVE ZERO                         TO MOD-IDARTNR-ENTER           
047400                                              MOD-IDRADNR-ENTER           
047500                                              MOD-IDARTNR-NEXT            
047600                                              MOD-IDRADNR-NEXT            
047700       END-IF                                                             
047800                                                                          
047900       PERFORM UNTIL INDX > MAX-INDX                                      
048000         IF SEGMENT-FOUND                                                 
048100           MOVE LEV-IDARTNR                TO MOD-IDARTNR   (INDX)        
048200                                              W-IDARTNR-BENA              
048300           MOVE LEV-IDRADNR                TO MOD-IDRADNR   (INDX)        
048400           MOVE LEV-IDKOLLI                TO MOD-IDKOLLI   (INDX)        
048500           MOVE LEV-IDORDNR7               TO MOD-IDORDNR   (INDX)        
048600           PERFORM FA-GET-DESCRIPTION                                     
048700           MOVE LEV-KVLEVANM-BEKR          TO MOD-KVLEVANM  (INDX)        
048800           MOVE LEV-KDANMORS               TO MOD-KDANMORS  (INDX)        
048900           MOVE LEV-IDFTG                  TO MOD-IDFTG     (INDX)        
049000*SAP  ÄNDRAT FÖR SAP/R3                                                   
049100           MOVE LEV-IDANALYS               TO MOD-IDANALYS  (INDX)        
049210           IF MFS-SPLIT                                                   
049300             MOVE LEV-IDKST                TO MOD-IDKST     (INDX)        
049310           ELSE                                                           
049320             MOVE LEV-IDKONTO              TO MOD-IDKONTO   (INDX)        
049330           END-IF                                                         
049400           PERFORM IMS-GNP-KREE11                                         
049500         ELSE                                                             
049600           MOVE MFS-ERASE-FIELD            TO MOD-IDARTNR   (INDX)        
049700                                              MOD-IDRADNR   (INDX)        
049800                                              MOD-IDKOLLI   (INDX)        
049900                                              MOD-IDORDNR   (INDX)        
050000                                              MOD-BEART     (INDX)        
050100                                              MOD-KVLEVANM  (INDX)        
050200                                              MOD-KDANMORS  (INDX)        
050300                                              MOD-IDFTG     (INDX)        
050400                                              MOD-IDANALYS  (INDX)        
050500                                              MOD-IDKONTO   (INDX)        
050600*                                             MOD-IDKST     (INDX)        
050700         END-IF                                                           
050800         ADD 1                             TO INDX                        
050900       END-PERFORM                                                        
051000                                                                          
051100       IF SEGMENT-FOUND                                                   
051200         MOVE LEV-IDARTNR                  TO MOD-IDARTNR-NEXT            
051300         MOVE LEV-IDRADNR                  TO MOD-IDRADNR-NEXT            
051400         MOVE INF-MORE-INFO-EXISTS         TO MED-IDMFSINF                
051500         CALL WMEDKONV USING MED-WMEDAREA                                 
051600         MOVE MED-TEMFSINF                 TO MOD-TEMFSINF                
051700       ELSE                                                               
051800         MOVE ZERO                         TO MOD-IDARTNR-NEXT            
051900                                              MOD-IDRADNR-NEXT            
052000       END-IF                                                             
052100                                                                          
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 FA-GET-DESCRIPTION SECTION.                                              
052600                                                                          
052700     PERFORM IMS-GET-BENA01-SEQ                                           
052800     MOVE MED-IDSKYLT                      TO W-IDSKYLT                   
052900     PERFORM IMS-GNP-BENA11-SEQ                                           
053000     IF SEGMENT-FOUND                                                     
053100        MOVE TEXT-BEART                    TO MOD-BEART (INDX)            
053200     ELSE                                                                 
053300        MOVE MFS-ERASE-FIELD               TO MOD-BEART (INDX)            
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
078500 MFS-ERASE-FIELD-OUT SECTION.                                             
078600                                                                          
078700*    --- ALLA UTDATA-FÄLT                                                 
078800*    --- INCL. SCROLL KEYS                                                
078900     MOVE MFS-ERASE-FIELD                  TO MOD-IDDISTR-UT              
079000                                              MOD-IDKUNDNR-UT             
079100                                              MOD-IDRAPPNR-UT             
079200                                              MOD-IDARTNR-UT              
079300                                              MOD-IDRADNR-UT              
079400                                              MOD-IDARTNR-ENTER           
079500                                              MOD-IDARTNR-NEXT            
079600                                              MOD-IDRADNR-ENTER           
079700                                              MOD-IDRADNR-NEXT            
079800     .                                                                    
079900 MFS-ERASE-FIELD-IN SECTION.                                              
080000                                                                          
080100*    --- ALLA INDATA-FÄLT                                                 
080200     MOVE MFS-ERASE-FIELD                  TO MOD-IDDISTR-IN              
080300                                              MOD-IDKUNDNR-IN             
080400                                              MOD-IDRAPPNR-IN             
080500                                              MOD-IDARTNR-IN              
080600                                              MOD-IDRADNR-IN              
080700     .                                                                    
080800     EJECT                                                                
080900* --- IMS SECTIONS ---                                                    
081000                                                                          
081100 IMS-GET-MSG SECTION.                                                     
081200                                                                          
081300     MOVE '  QC' TO GOOD-STATUSCODES                                      
081400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
081500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081600     PERFORM IMS-STATUSCHECK                                              
081700     .                                                                    
081800 IMS-INSERT-MSG SECTION.                                                  
081900                                                                          
082000     IF MSGI-IDLAND-SPR = 'GB'                                            
082100       MOVE 'N' TO MFS-KDHUVOMR                                           
082110       IF MFS-SPLIT                                                       
082120         MOVE 'Cost centre'    TO MOD-IDKONTO-TXT                         
082130       ELSE                                                               
082140         MOVE '   Acc.No.'     TO MOD-IDKONTO-TXT                         
082150       END-IF                                                             
082160     ELSE                                                                 
082170       IF MFS-SPLIT                                                       
082180         MOVE 'Kostn.st.'      TO MOD-IDKONTO-TXT                         
082190       ELSE                                                               
082191         MOVE '   Kontonr'     TO MOD-IDKONTO-TXT                         
082192       END-IF                                                             
082200     END-IF                                                               
082300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
082400     MOVE SPACE TO GOOD-STATUSCODES                                       
082500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
082600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082700     PERFORM IMS-STATUSCHECK                                              
082800     .                                                                    
082900     EJECT                                                                
083000 IMS-GET-KREE01 SECTION.                                                  
083100                                                                          
083200     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
083300          DELIMITED BY SIZE INTO SSA1                                     
083400     MOVE '  GE' TO GOOD-STATUSCODES                                      
083500     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
083600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSCHECK                                              
083800     .                                                                    
083900     EJECT                                                                
084000 IMS-GNP-KREE11      SECTION.                                             
084100                                                                          
084200     STRING 'WLKREE11(WDA211KY>=' W-WDA2KEY-X ')'                         
084300          DELIMITED BY SIZE INTO SSA1                                     
084400     MOVE '  GE' TO GOOD-STATUSCODES                                      
084500     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
084600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSCHECK                                              
084800     .                                                                    
084900*IMS-GHNP-KREE11 SECTION.                                                 
085000*                                                                         
085100*    STRING 'WLKREE11(WDA211KY =' W-WDA2KEY-X ')'                         
085200*         DELIMITED BY SIZE INTO SSA1                                     
085300*    MOVE '  GE' TO GOOD-STATUSCODES                                      
085400*    CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA-WDA211 SSA1             
085500*    MOVE KREE-STATUS-CODE TO STATUS-WS                                   
085600*    PERFORM IMS-STATUSCHECK                                              
085700*    .                                                                    
085800*    EJECT                                                                
085900*IMS-REPL-KREE   SECTION.                                                 
086000*                                                                         
086100*    MOVE '  ' TO GOOD-STATUSCODES                                        
086200*    CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA-WDA211                  
086300*    MOVE KREE-STATUS-CODE TO STATUS-WS                                   
086400*    PERFORM IMS-STATUSCHECK                                              
086500*    .                                                                    
086600*    EJECT                                                                
086700 IMS-GET-BENA01-SEQ SECTION.                                              
086800                                                                          
086900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
087000             DELIMITED BY SIZE INTO SSA1                                  
087100     MOVE '    ' TO GOOD-STATUSCODES                                      
087200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-WDD311 SSA1               
087300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSCHECK                                              
087500     .                                                                    
087600 IMS-GNP-BENA11-SEQ SECTION.                                              
087700                                                                          
087800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
087900             DELIMITED BY SIZE INTO SSA1                                  
088000     MOVE '  GE' TO GOOD-STATUSCODES                                      
088100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-WDD311 SSA1              
088200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
088300     PERFORM IMS-STATUSCHECK                                              
088400     .                                                                    
088500     EJECT                                                                
089600 IMS-STATUSCHECK SECTION.                                                 
089700                                                                          
089800     SET STATUS-IX TO 1                                                   
089900     SEARCH GOOD-STATUS                                                   
090000       AT END                                                             
090100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
090200         DELIMITED BY SIZE INTO ERROR-TEXT                                
090300         CALL FELLOG                                                      
090400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
090500         CONTINUE                                                         
090600     END-SEARCH                                                           
090700     .                                                                    
