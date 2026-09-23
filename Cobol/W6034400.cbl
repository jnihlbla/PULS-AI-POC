000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6034400.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ADD LOCATIONS                                                    
000900*                                                                         
001000*     CHANGED 97-07-04 JOHAN LINDKVIST                                    
001100*     CHANGED 97-02-04 JOHAN LINDKVIST (IO-COUNT EXCEEDED)                
001200*                                                                         
001300*        THE PROGRAM READS     WL6313 (WDGX)                              
001400*        THE PROGRAM READS     WL6315 (WDGX)                              
001500*        THE PROGRAM ADD       WLLOCA (WDJ8)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W6T344                                              
001900*        MID:         W6I34401                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W6O34401                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6034400'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
004000 77  MAX-IO-COUNT               PIC S9(4)  VALUE +600.                    
004100 77  LNG-P-TO-P-PREFIX          PIC S9(4)  VALUE +17   COMP SYNC.         
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300                                                                          
004400 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
004500 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
004600 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
004700 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
004800 77  WS-ADSEC11-FOM              PIC 9(3)   VALUE ZERO.                   
004900 77  WS-ADSEC11-TOM              PIC 9(3)   VALUE ZERO.                   
005000 77  WS-ADSEC11                  PIC 9(4)   VALUE ZERO.                   
005100 77  WS-ADLEVEL11-FOM            PIC 9(1)   VALUE ZERO.                   
005200 77  WS-ADLEVEL11-TOM            PIC 9(1)   VALUE ZERO.                   
005300 77  WS-ADLEVEL11                PIC 9(2)   VALUE ZERO.                   
005400 77  WS-ADSEQ-FOM                PIC 9(1)   VALUE ZERO.                   
005500 77  WS-ADSEQ-TOM                PIC 9(1)   VALUE ZERO.                   
005600 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
005700 77  WS-KDAOE                    PIC X(1)   VALUE SPACE.                  
005800                                                                          
005900 77  WS-SECTION-STEP             PIC 9      VALUE 1.                      
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006200     88  INDATA-OK                           VALUE 'Y'.                   
006300     88  INDATA-WRONG                        VALUE 'N'.                   
006400                                                                          
006500 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006600     88  KEYS-OK                             VALUE 'Y'.                   
006700     88  KEYS-WRONG                          VALUE 'N'.                   
006800                                                                          
006900 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007000     88  RESTART                             VALUE 'Y'.                   
007100                                                                          
007200 77  PRIME                       PIC X        VALUE 'P'.                  
007300 77  BUFFER                      PIC X        VALUE 'R'.                  
007400 77  MIXED                       PIC X        VALUE 'M'.                  
007500 77  ALLA                        PIC X        VALUE 'A'.                  
007600 77  ODD                         PIC X        VALUE 'O'.                  
007700 77  EVEN                        PIC X        VALUE 'E'.                  
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  OWN-MID                             VALUE '6344'.                
008100     88  GOOD-MID                            VALUE '6344'.                
008200     88  HELP-MID                            VALUE '0551'.                
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDC99                                                       
008600       EJECT                                                              
008700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008800 01  GENERAL-SUBPROGRAMS.                                                 
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     EJECT                                                                
009400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009500*01 -COPY WMEDAREA                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010300     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
010400     EJECT                                                                
010500   03    MED-1.                                                           
010600     05    FILLER                  PIC X(23)   VALUE                      
010700         'UPDATED UNTIL LOCATION '.                                       
010800     05    MED-IDDC                PIC X(02).                             
010900     05    FILLER                  PIC X(02)   VALUE SPACE.               
011000     05    MED-ADLAGOMR            PIC 9(02).                             
011100     05    FILLER                  PIC X(01)   VALUE SPACE.               
011200     05    MED-ADGANG              PIC 9(02).                             
011300     05    FILLER                  PIC X(01)   VALUE SPACE.               
011400     05    MED-ADSEC11             PIC 9(03).                             
011500     05    MED-ADLEVEL11           PIC 9(01).                             
011600     05    MED-ADSEQ               PIC 9(01).                             
011700     05    FILLER                  PIC X(01)   VALUE '.'.                 
011800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012100     SKIP3                                                                
012200*01 -COPY WMSGINIT                                                        
012300     EJECT                                                                
012400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W6I34401                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W6O34401                                                 
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100 01      P-TO-P-SW.                                                       
014200                                                                          
014300   03     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.          
014400   03     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.              
014500   03     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.              
014600   03     P-TO-P-KDTRANS          PIC X(8).                               
014700   03     P-TO-P-IDTRANS          PIC X(4).                               
014800   03     P-TO-P-KDMFSFOR         PIC X(1).                               
014900   03     -COPY W6I34401 -PRE 6314-UT-                                    
015000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015100*                                                                         
015200     EJECT                                                                
015300 01  WORK-AREA.                                                           
015400     03 WORK-ADLAGOMR            PIC 9(2).                                
015500     03 WORK-ADGANG              PIC 9(2).                                
015600     03 WORK-ADPLATS.                                                     
015700        05 WORK-ADSEC11          PIC 9(3).                                
015800        05 WORK-ADLEVEL11        PIC 9(1).                                
015900        05 WORK-ADSEQ            PIC 9(1).                                
016000                                                                          
016100 01  SAVE-AREA.                                                           
016200     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
016300     03 SAVE-ADLAGOMR            PIC 9(2).                                
016400     03 SAVE-ADGANG              PIC 9(2).                                
016500     03 SAVE-ADPLATS.                                                     
016600        05 SAVE-ADSEC11          PIC 9(3).                                
016700        05 SAVE-ADLEVEL11        PIC 9(1).                                
016800        05 SAVE-ADSEQ            PIC 9(1).                                
016900                                                                          
017000                                                                          
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  KEYS-TO-DLI.                                                         
017400     03  W-WDGXKEY-6313-X.                                                
017500          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
017600          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
017700          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
017800                                                                          
017900     03  W-WDGXKEY-6314-X.                                                
018000         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
018100                                                                          
018200     03  W-WDGXKEY-6315-X.                                                
018300          05 W-6315-IDHTYP       PIC X(4)    VALUE '6315'.                
018400          05 W-6315-IDDC         PIC X(2)    VALUE SPACE.                 
018500          05 W-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
018600                                                                          
018700     03  W-WDGXKEY-6316-X.                                                
018800         05  W-KDSTOR            PIC X(3)    VALUE SPACE.                 
018900                                                                          
019000     03  W-WDJ8KEY-X.                                                     
019100         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
019200         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
019300         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
019400         05  W-LOC-ADPLATS.                                               
019500             07 W-LOC-ADSEC11    PIC 9(3).                                
019600             07 W-LOC-ADLEVEL11  PIC 9(1).                                
019700             07 W-LOC-ADSEQ      PIC 9(1).                                
019800     SKIP2                                                                
019900*    --- STATUS-KOD FRÅN IMS                                              
020000 01  STATUS-WS                   PIC XX.                                  
020100     88  SEGMENT-FOUND                       VALUE '  '.                  
020200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
020300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020400     SKIP2                                                                
020500 01  GOOD-STATUSCODES.                                                    
020600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020700     SKIP3                                                                
020800 01  SSA1                        PIC X(64).                               
020900 01  SSA2                        PIC X(64).                               
021000     EJECT                                                                
021100*    --- IMS FUNCTION CODES                                               
021200*01  -COPY W0003                                                          
021300     EJECT                                                                
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021500 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
021600 01  WL631301-AREA.                                                       
021700*    03  -COPY WDGX6313                                                   
021800     EJECT                                                                
021900 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
022000 01  WL631311-AREA.                                                       
022100*    03  -COPY WDGX6314                                                   
022200                                                                          
022300 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
022400 01  WL631501-AREA.                                                       
022500*    03  -COPY WDGX6315                                                   
022600     EJECT                                                                
022700 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
022800 01  WL631511-AREA.                                                       
022900*    03  -COPY WDGX6316                                                   
023000                                                                          
023100 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
023200 01  DLI-IO-WLLOCA01.                                                     
023300*    03  -COPY WDJ801                                                     
023400     EJECT                                                                
023500 LINKAGE SECTION.                                                         
023600*01  -COPY W0009  -PRE MSG-                                               
023700     EJECT                                                                
023800*01  -COPY W0009  -PRE ALT6314-                                           
023900     EJECT                                                                
024000*01  -COPY W0008  -PRE USEA-                                              
024100     05  FILLER                  PIC X.                                   
024200     EJECT                                                                
024300*01  -COPY W0008  -PRE 6313-                                              
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008  -PRE 6315-                                              
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008  -PRE LOCA-                                              
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200 PROCEDURE DIVISION  USING MSG-PCB ALT6314-PCB                            
025300           USEA-PCB                                                       
025400           6313-PCB 6315-PCB LOCA-PCB.                                    
025500 MAIN SECTION.                                                            
025600     ENTRY 'DLITCBL' USING MSG-PCB ALT6314-PCB                            
025700            USEA-PCB                                                      
025800            6313-PCB 6315-PCB LOCA-PCB.                                   
025900                                                                          
026000     PERFORM IMS-GET-MSG                                                  
026100     IF SEGMENT-FOUND                                                     
026200       PERFORM A-INIT                                                     
026300       IF GOOD-MID OR HELP-MID                                            
026400          PERFORM B-CHECK-KEYS                                            
026500          IF KEYS-OK                                                      
026600             IF MFS-UPDATE                                                
026700                PERFORM G-CHECK-INPUT                                     
026800                IF INDATA-OK                                              
026900                   PERFORM H-UPDATE                                       
027000                END-IF                                                    
027100              ELSE                                                        
027200                IF MFS-FIRST                                              
027300                   PERFORM C-FIRST-PAGE                                   
027400                 ELSE                                                     
027500                   PERFORM E-SAME-PAGE                                    
027600                END-IF                                                    
027700             END-IF                                                       
027800          END-IF                                                          
027900       END-IF                                                             
028000       IF RESTART                                                         
028100          COMPUTE P-TO-P-KVLL =  LNG-P-TO-P-PREFIX +                      
028200                                 LENGTH OF MID-W6I34401                   
028300          MOVE 'W6T344U '     TO P-TO-P-KDTRANS                           
028400          MOVE '6344'         TO P-TO-P-IDTRANS                           
028500          MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                          
028600          MOVE MID-W6I34401   TO 6314-UT-MID-W6I34401                     
028700          PERFORM IMS-ISRT-ALT-MSG-6314                                   
028800        ELSE                                                              
028900          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34401 + 4                   
029000          PERFORM IMS-INSERT-MSG                                          
029100       END-IF                                                             
029200     END-IF                                                               
029300                                                                          
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 A-INIT SECTION.                                                          
029900                                                                          
030000     IF MSG-DOUBLE-TRANSACTIONS                                           
030100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34401                 
030200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030400     ELSE                                                                 
030500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I34401                  
030600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030800     END-IF                                                               
030900                                                                          
031000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031300                                                                          
031400     MOVE LOW-VALUE TO MSG-AREA                                           
031500     MOVE 'W6O344N1' TO MFS-IDMOD                                         
031600     MOVE '6344' TO MOD-IDTRANS                                           
031700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
031800                                                                          
031900     MOVE NOO                  TO RESTART-SW                              
032000                                                                          
032100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032200     MOVE '001'             TO MSGI-KDCALL                                
032300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032500     MOVE '6344'            TO MSGI-IDTRANS                               
032600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032700                                                                          
032800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
032900     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
033000     MOVE MSGI-IDDC         TO WS-IDDC                                    
033100                                                                          
033200     IF GOOD-MID OR HELP-MID                                              
033300       CONTINUE                                                           
033400     ELSE                                                                 
033500       MOVE SPACE TO MFS-KDTRTYP                                          
033600       MOVE '7' TO MFS-IDPFK                                              
033700       PERFORM MFS-INIT-KEY-FIELD-IN                                      
033800       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
033900       PERFORM MFS-ERASE-FIELD-IN                                         
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 B-CHECK-KEYS SECTION.                                                    
034400                                                                          
034500                                                                          
034600     MOVE YES               TO KEYS-SW                                    
034700                                                                          
034800     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
034900                               MOD-ADLAGOMR-IN                            
035000                               MOD-ADGANG-FOM-IN                          
035100                               MOD-ADGANG-TOM-IN                          
035200                               MOD-ADSEC11-FOM-IN                         
035300                               MOD-ADSEC11-TOM-IN                         
035400                               MOD-ADLEVEL11-FOM-IN                       
035500                               MOD-ADLEVEL11-TOM-IN                       
035600                               MOD-ADSEQ-FOM-IN                           
035700                               MOD-ADSEQ-TOM-IN                           
035800                               MOD-KDAOE-IN                               
035900                                                                          
036000*                                                                         
036100*    -- CONTROL  ON WAREHOUSE                                             
036200*                                                                         
036300     IF CDC                                                               
036400       CONTINUE                                                           
036500     ELSE                                                                 
036600       MOVE NOO                 TO KEYS-SW                                
036700     END-IF                                                               
036800*                                                                         
036900*    -- CONTROL  AREA                                                     
037000*                                                                         
037100     IF MID-ADLAGOMR-IN = ALL '+'                                         
037200       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
037300       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
037400     ELSE                                                                 
037500       IF MID-ADLAGOMR-IN NUMERIC                                         
037600          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
037700          MOVE '7' TO MFS-IDPFK                                           
037800          MOVE SPACE TO MFS-KDTRTYP                                       
037900       ELSE                                                               
038000          MOVE NOO             TO KEYS-SW                                 
038100       END-IF                                                             
038200     END-IF                                                               
038300*                                                                         
038400*    -- CONTROL  AISLE FROM                                               
038500*                                                                         
038600     IF MID-ADGANG-FOM-IN = ALL '+'                                       
038700       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
038800       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
038900     ELSE                                                                 
039000       IF MID-ADGANG-FOM-IN NUMERIC                                       
039100          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
039200          MOVE '7' TO MFS-IDPFK                                           
039300          MOVE SPACE TO MFS-KDTRTYP                                       
039400       ELSE                                                               
039500          MOVE NOO               TO KEYS-SW                               
039600       END-IF                                                             
039700     END-IF                                                               
039800*                                                                         
039900*    -- CONTROL  AISLE THRU                                               
040000*                                                                         
040100     IF MID-ADGANG-TOM-IN = ALL '+'                                       
040200       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
040300       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
040400     ELSE                                                                 
040500       IF MID-ADGANG-TOM-IN NUMERIC                                       
040600          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
040700          MOVE '7' TO MFS-IDPFK                                           
040800          MOVE SPACE TO MFS-KDTRTYP                                       
040900       ELSE                                                               
041000          MOVE NOO               TO KEYS-SW                               
041100       END-IF                                                             
041200     END-IF                                                               
041300*                                                                         
041400*    -- CONTROL  SECTION FROM                                             
041500*                                                                         
041600     IF MID-ADSEC11-FOM-IN = ALL '+'                                      
041700       INSPECT MID-ADSEC11-FOM-UT REPLACING LEADING SPACE BY ZERO         
041800       MOVE MID-ADSEC11-FOM-UT  TO WS-ADSEC11-FOM                         
041900     ELSE                                                                 
042000       IF MID-ADSEC11-FOM-IN NUMERIC                                      
042100          MOVE MID-ADSEC11-FOM-IN TO WS-ADSEC11-FOM                       
042200          MOVE '7' TO MFS-IDPFK                                           
042300          MOVE SPACE TO MFS-KDTRTYP                                       
042400       ELSE                                                               
042500          MOVE NOO              TO KEYS-SW                                
042600       END-IF                                                             
042700     END-IF                                                               
042800                                                                          
042900*                                                                         
043000*    -- CONTROL  SECTION THRU                                             
043100*                                                                         
043200     IF MID-ADSEC11-TOM-IN = ALL '+'                                      
043300       INSPECT MID-ADSEC11-TOM-UT REPLACING LEADING SPACE BY ZERO         
043400       MOVE MID-ADSEC11-TOM-UT  TO WS-ADSEC11-TOM                         
043500     ELSE                                                                 
043600       IF MID-ADSEC11-TOM-IN NUMERIC                                      
043700          MOVE MID-ADSEC11-TOM-IN TO WS-ADSEC11-TOM                       
043800          MOVE '7' TO MFS-IDPFK                                           
043900          MOVE SPACE TO MFS-KDTRTYP                                       
044000       ELSE                                                               
044100          MOVE NOO              TO KEYS-SW                                
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500*                                                                         
044600*    -- CONTROL  LEVEL FROM                                               
044700*                                                                         
044800     IF MID-ADLEVEL11-FOM-IN = ALL '+'                                    
044900      INSPECT MID-ADLEVEL11-FOM-UT REPLACING LEADING SPACE BY ZERO        
045000       MOVE MID-ADLEVEL11-FOM-UT  TO WS-ADLEVEL11-FOM                     
045100     ELSE                                                                 
045200       IF MID-ADLEVEL11-FOM-IN NUMERIC                                    
045300          MOVE MID-ADLEVEL11-FOM-IN TO WS-ADLEVEL11-FOM                   
045400          MOVE '7' TO MFS-IDPFK                                           
045500          MOVE SPACE TO MFS-KDTRTYP                                       
045600       ELSE                                                               
045700          MOVE NOO                TO KEYS-SW                              
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100*                                                                         
046200*    -- CONTROL  LEVEL THRU                                               
046300*                                                                         
046400     IF MID-ADLEVEL11-TOM-IN = ALL '+'                                    
046500      INSPECT MID-ADLEVEL11-TOM-UT REPLACING LEADING SPACE BY ZERO        
046600       MOVE MID-ADLEVEL11-TOM-UT  TO WS-ADLEVEL11-TOM                     
046700     ELSE                                                                 
046800       IF MID-ADLEVEL11-TOM-IN NUMERIC                                    
046900          MOVE MID-ADLEVEL11-TOM-IN TO WS-ADLEVEL11-TOM                   
047000          MOVE '7' TO MFS-IDPFK                                           
047100          MOVE SPACE TO MFS-KDTRTYP                                       
047200       ELSE                                                               
047300          MOVE NOO                TO KEYS-SW                              
047400       END-IF                                                             
047500     END-IF                                                               
047600                                                                          
047700*                                                                         
047800*    -- CONTROL  PLACEMENT FROM    (SEQUENCE)                             
047900*                                                                         
048000     IF MID-ADSEQ-FOM-IN = ALL '+'                                        
048100       INSPECT MID-ADSEQ-FOM-UT REPLACING LEADING SPACE BY ZERO           
048200       MOVE MID-ADSEQ-FOM-UT      TO WS-ADSEQ-FOM                         
048300     ELSE                                                                 
048400       IF MID-ADSEQ-FOM-IN NUMERIC                                        
048500          MOVE MID-ADSEQ-FOM-IN TO WS-ADSEQ-FOM                           
048600          MOVE '7' TO MFS-IDPFK                                           
048700          MOVE SPACE TO MFS-KDTRTYP                                       
048800       ELSE                                                               
048900          MOVE NOO                TO KEYS-SW                              
049000       END-IF                                                             
049100     END-IF                                                               
049200                                                                          
049300*                                                                         
049400*    -- CONTROL  PLACEMENT THRU    (SEQUENCE)                             
049500*                                                                         
049600     IF MID-ADSEQ-TOM-IN = ALL '+'                                        
049700       INSPECT MID-ADSEQ-TOM-UT REPLACING LEADING SPACE BY ZERO           
049800       MOVE MID-ADSEQ-TOM-UT      TO WS-ADSEQ-TOM                         
049900     ELSE                                                                 
050000       IF MID-ADSEQ-TOM-IN NUMERIC                                        
050100          MOVE MID-ADSEQ-TOM-IN   TO WS-ADSEQ-TOM                         
050200          MOVE '7' TO MFS-IDPFK                                           
050300          MOVE SPACE TO MFS-KDTRTYP                                       
050400       ELSE                                                               
050500          MOVE NOO                TO KEYS-SW                              
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900*                                                                         
051000*    -- CONTROL  ALL/ODD/EVEN                                             
051100*                                                                         
051200     IF MID-KDAOE-IN = ALL '+'                                            
051300       INSPECT MID-KDAOE-UT REPLACING LEADING SPACE BY 'A'                
051400       MOVE MID-KDAOE-UT          TO WS-KDAOE                             
051500     ELSE                                                                 
051600          INSPECT MID-KDAOE-IN REPLACING LEADING SPACE BY 'A'             
051700       IF MID-KDAOE-IN = ALLA OR ODD OR EVEN                              
051800          MOVE MID-KDAOE-IN       TO WS-KDAOE                             
051900          MOVE '7' TO MFS-IDPFK                                           
052000          MOVE SPACE TO MFS-KDTRTYP                                       
052100       ELSE                                                               
052200          MOVE NOO                TO KEYS-SW                              
052300       END-IF                                                             
052400     END-IF                                                               
052500                                                                          
052600*                                                                         
052700*    -- FILL MOD KEY-OUTPUT FIELDS                                        
052800*                                                                         
052900     MOVE WS-IDDC        TO  MOD-IDDC-UT                                  
053000     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
053100     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
053200     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
053300     MOVE WS-ADSEC11-FOM TO  MOD-ADSEC11-FOM-UT                           
053400     MOVE WS-ADSEC11-TOM TO  MOD-ADSEC11-TOM-UT                           
053500     MOVE WS-ADLEVEL11-FOM TO MOD-ADLEVEL11-FOM-UT                        
053600     MOVE WS-ADLEVEL11-TOM TO MOD-ADLEVEL11-TOM-UT                        
053700     MOVE WS-ADSEQ-FOM   TO  MOD-ADSEQ-FOM-UT                             
053800     MOVE WS-ADSEQ-TOM   TO  MOD-ADSEQ-TOM-UT                             
053900     MOVE WS-KDAOE       TO  MOD-KDAOE-UT                                 
054000                                                                          
054100* KONTROLL FÖR ATT UNDVIKA 'NOLLOR' (0) I INDATA                          
054200*    IF WS-ADGANG-FOM     = ZERO  OR                                      
054300*       WS-ADSEC11-FOM    = ZERO  OR                                      
054400*       WS-ADLEVEL11-FOM  = ZERO  OR                                      
054500*       WS-ADSEQ-FOM      = ZERO  OR                                      
054600*       WS-ADGANG-TOM     = ZERO  OR                                      
054700*       WS-ADSEC11-TOM    = ZERO  OR                                      
054800*       WS-ADLEVEL11-TOM  = ZERO  OR                                      
054900*       WS-ADSEQ-TOM      = ZERO                                          
055000*                                                                         
055100*          MOVE NOO               TO KEYS-SW                              
055200*    END-IF                                                               
055300                                                                          
055400     IF KEYS-WRONG                                                        
055500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
055600       CALL WMEDKONV USING MED-WMEDAREA                                   
055700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055800      ELSE                                                                
055900* KONTROLL AV INTERVALL FÖR IN-DATA                                       
056000       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
056100          WS-ADSEC11-FOM  >  WS-ADSEC11-TOM OR                            
056200          WS-ADLEVEL11-FOM > WS-ADLEVEL11-TOM OR                          
056300          WS-ADSEQ-FOM    >  WS-ADSEQ-TOM                                 
056400                                                                          
056500          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
056600          CALL WMEDKONV USING MED-WMEDAREA                                
056700          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
056800          MOVE NOO                 TO KEYS-SW                             
056900       END-IF                                                             
057000* KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER               
057100       EVALUATE WS-KDAOE                                                  
057200       WHEN ALLA                                                          
057300         MOVE 1 TO WS-SECTION-STEP                                        
057400       WHEN EVEN                                                          
057500* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
057600         IF FUNCTION MOD (WS-ADSEC11-FOM 2) = 0 AND                       
057700            FUNCTION MOD (WS-ADSEC11-TOM 2) = 0                           
057800            MOVE 2 TO WS-SECTION-STEP                                     
057900         ELSE                                                             
058000            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
058100            CALL WMEDKONV USING MED-WMEDAREA                              
058200            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
058300            MOVE NOO               TO KEYS-SW                             
058400         END-IF                                                           
058500       WHEN ODD                                                           
058600* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
058700         IF FUNCTION MOD (WS-ADSEC11-FOM 2) = 1 AND                       
058800            FUNCTION MOD (WS-ADSEC11-TOM 2) = 1                           
058900            MOVE 2 TO WS-SECTION-STEP                                     
059000         ELSE                                                             
059100            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
059200            CALL WMEDKONV USING MED-WMEDAREA                              
059300            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
059400            MOVE NOO               TO KEYS-SW                             
059500         END-IF                                                           
059600       END-EVALUATE                                                       
059700     END-IF                                                               
059800                                                                          
059900     IF KEYS-WRONG                                                        
060000       PERFORM MFS-ERASE-FIELD-IN                                         
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 C-FIRST-PAGE SECTION.                                                    
060500                                                                          
060600     PERFORM MFS-ERASE-FIELD-IN                                           
060700*  SÄTTER DEFAULT VÄRDE PÅ "NUMBER OF PARTS"                              
060800          MOVE 1 TO MID-KVMPART                                           
060900          MOVE 1 TO MOD-KVMPART                                           
061000                                                                          
061100     MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOC-ATTR                         
061200*                                                                         
061300*    -- INIT SAVE KEYS                                                    
061400*                                                                         
061500     MOVE '6344'                TO SAVE-IDTRANS                           
061600     MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                          
061700     MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                            
061800     MOVE WS-ADSEC11-FOM        TO SAVE-ADSEC11                           
061900     MOVE WS-ADLEVEL11-FOM      TO SAVE-ADLEVEL11                         
062000     MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                             
062100                                                                          
062200     MOVE '002'                 TO MSGI-KDCALL                            
062300     MOVE SAVE-AREA             TO MSGI-SPAR-AREA                         
062400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062500                                                                          
062600     MOVE INF-PRESS-PF11        TO MED-IDMFSINF                           
062700     CALL WMEDKONV USING MED-WMEDAREA                                     
062800     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
062900     .                                                                    
063000     EJECT                                                                
063100 E-SAME-PAGE SECTION.                                                     
063200                                                                          
063300     IF OWN-MID OR HELP-MID                                               
063400       IF MID-KDLOC   = ALL '+' AND                                       
063500          MID-KDFREQ  = ALL '+' AND                                       
063600          MID-KDSTOR  = ALL '+' AND                                       
063700          MID-TELOC   = ALL '+' AND                                       
063800          MID-KVMPART = ALL '+'                                           
063900          PERFORM MFS-ERASE-FIELD-IN                                      
064000*  SÄTTER DEFAULT VÄRDE PÅ "NUMBER OF PARTS"                              
064100          MOVE 1 TO MID-KVMPART                                           
064200          MOVE 1 TO MOD-KVMPART                                           
064300                                                                          
064400          MOVE MFS-ADD-SET-CURSOR TO MOD-KDLOC-ATTR                       
064500        ELSE                                                              
064600         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
064700         CALL WMEDKONV USING MED-WMEDAREA                                 
064800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
064900         PERFORM EA-MID-INDATA-TO-MOD                                     
065000       END-IF                                                             
065100     ELSE                                                                 
065200       PERFORM MFS-ERASE-FIELD-IN                                         
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 EA-MID-INDATA-TO-MOD SECTION.                                            
065700                                                                          
065800     IF MID-KDLOC NOT = ALL '+'                                           
065900        MOVE MID-KDLOC             TO MOD-KDLOC                           
066000        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOC-ATTR                      
066100      ELSE                                                                
066200        MOVE MFS-ERASE-FIELD       TO MOD-KDLOC                           
066300     END-IF                                                               
066400                                                                          
066500     IF MID-KDFREQ NOT = ALL '+'                                          
066600        MOVE MID-KDFREQ             TO MOD-KDFREQ                         
066700        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
066800      ELSE                                                                
066900        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ                         
067000     END-IF                                                               
067100                                                                          
067200     IF MID-KDSTOR NOT = ALL '+'                                          
067300        MOVE MID-KDSTOR             TO MOD-KDSTOR                         
067400        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
067500      ELSE                                                                
067600        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
067700     END-IF                                                               
067800                                                                          
067900     IF MID-TELOC NOT = ALL '+'                                           
068000        MOVE MID-TELOC             TO MOD-TELOC                           
068100        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOC-ATTR                      
068200      ELSE                                                                
068300        MOVE MFS-ERASE-FIELD       TO MOD-TELOC                           
068400     END-IF                                                               
068500                                                                          
068600     IF MID-KVMPART NOT = ALL '+'                                         
068700        MOVE MID-KVMPART           TO MOD-KVMPART                         
068800        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPART-ATTR                    
068900      ELSE                                                                
069000        MOVE MFS-ERASE-FIELD       TO MOD-KVMPART                         
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400 G-CHECK-INPUT SECTION.                                                   
069500                                                                          
069600     MOVE YES  TO INDATA-SW                                               
069700                                                                          
069800     IF MID-KDLOC      = ALL '+' AND                                      
069900        MID-KDFREQ     = ALL '+' AND                                      
070000        MID-KDSTOR     = ALL '+' AND                                      
070100        MID-TELOC      = ALL '+' AND                                      
070200        MID-KVMPART    = ALL '+'                                          
070300                                                                          
070400        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
070500        CALL WMEDKONV USING MED-WMEDAREA                                  
070600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070700                                                                          
070800        PERFORM MFS-DONT-TOUCH-FIELD-OUT-IN                               
070900        MOVE NOO TO INDATA-SW                                             
071000      ELSE                                                                
071100        PERFORM GA-CHECK-INPUT-NEW                                        
071200        IF INDATA-WRONG                                                   
071300           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
071400           CALL WMEDKONV USING MED-WMEDAREA                               
071500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
071600           PERFORM MFS-DONT-TOUCH-FIELD-OUT-IN                            
071700         END-IF                                                           
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 GA-CHECK-INPUT-NEW SECTION.                                              
072200                                                                          
072300*                                                                         
072400*    -- CONTROL  ON MULTIPLE PART                                         
072500*                                                                         
072600      IF MID-KVMPART NOT = ALL '+'                                        
072700        IF MID-KVMPART  NOT NUMERIC                                       
072800          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                    
072900          MOVE NOO                 TO INDATA-SW                           
073000         ELSE                                                             
073100          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPART-ATTR                       
073200        END-IF                                                            
073300       ELSE                                                               
073400        MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                      
073500        MOVE NOO                 TO INDATA-SW                             
073600      END-IF                                                              
073700                                                                          
073800*                                                                         
073900*    -- CONTROL  ON STORAGE CODE                                          
074000*                                                                         
074100     IF MID-KDSTOR   = ALL '+'                                            
074200        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDSTOR-ATTR                       
074300        MOVE NOO                 TO INDATA-SW                             
074400      ELSE                                                                
074500        MOVE WS-IDDC             TO W-6315-IDDC                           
074600        MOVE MID-KDSTOR          TO W-KDSTOR                              
074700        PERFORM  IMS-GU-6316                                              
074800        IF SEGMENT-MISSING                                                
074900           MOVE NOO                     TO INDATA-SW                      
075000           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                
075100         ELSE                                                             
075200           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                
075300        END-IF                                                            
075400     END-IF                                                               
075500*                                                                         
075600*    -- CONTROL  ON FREQUENCY CODE                                        
075700*                                                                         
075800                                                                          
075900     IF MID-KDFREQ   = ALL '+'                                            
076000        MOVE MFS-NUM-FIELD-WRONG TO MOD-KDFREQ-ATTR                       
076100        MOVE NOO                 TO INDATA-SW                             
076200      ELSE                                                                
076300        IF MID-KDFREQ   NUMERIC                                           
076400           MOVE WS-IDDC             TO W-6313-IDDC                        
076500           MOVE MID-KDFREQ          TO W-KDFREQ                           
076600           PERFORM  IMS-GU-6314                                           
076700           IF SEGMENT-MISSING                                             
076800              MOVE NOO                     TO INDATA-SW                   
076900              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR             
077000            ELSE                                                          
077100              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR             
077200           END-IF                                                         
077300         ELSE                                                             
077400           MOVE NOO                     TO INDATA-SW                      
077500           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                
077600        END-IF                                                            
077700     END-IF                                                               
077800                                                                          
077900*                                                                         
078000*    -- CONTROL  ON LOCATION TYPE                                         
078100*                                                                         
078200     IF MID-KDLOC    = ALL '+'                                            
078300        MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                      
078400        MOVE NOO                   TO INDATA-SW                           
078500      ELSE                                                                
078600        IF MID-KDLOC  =  PRIME OR BUFFER OR MIXED                         
078700           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOC-ATTR                   
078800         ELSE                                                             
078900           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                   
079000           MOVE NOO                   TO INDATA-SW                        
079100        END-IF                                                            
079200     END-IF                                                               
079300*                                                                         
079400*    -- CONTROL  ON MULTIPLE PART                                         
079500*                                                                         
079600     MOVE MFS-ALPHA-FIELD-OK          TO MOD-TELOC-ATTR                   
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 H-UPDATE SECTION.                                                        
080100                                                                          
080200     PERFORM HA-INIT                                                      
080300     PERFORM HB-UPDATE                                                    
080400     PERFORM HC-CLOSE                                                     
080500                                                                          
080600     .                                                                    
080700     EJECT                                                                
080800 HA-INIT SECTION.                                                         
080900                                                                          
081000     MOVE WS-IDDC            TO W-LOC-IDDC                                
081100                                LOC-IDDC                                  
081200     MOVE SAVE-ADLAGOMR        TO W-LOC-ADLAGOMR                          
081300                                LOC-ADLAGOMR                              
081400     MOVE SAVE-ADGANG          TO WS-ADGANG                               
081500     MOVE SAVE-ADSEC11         TO WS-ADSEC11                              
081600     MOVE SAVE-ADLEVEL11       TO WS-ADLEVEL11                            
081700     MOVE SAVE-ADSEQ           TO WS-ADSEQ                                
081800                                                                          
081900     MOVE MID-KDLOC          TO LOC-KDLOC   MOD-KDLOC                     
082000     MOVE MID-KDFREQ         TO LOC-KDFREQ  MOD-KDFREQ                    
082100     MOVE MID-KDSTOR         TO LOC-KDSTOR  MOD-KDSTOR                    
082200     MOVE MID-KVMPART        TO LOC-KVMPART MOD-KVMPART                   
082300     IF MID-TELOC  NOT = ALL '+'                                          
082400        MOVE MID-TELOC       TO LOC-TELOC   MOD-TELOC                     
082500     ELSE                                                                 
082600        MOVE SPACES          TO LOC-TELOC   MOD-TELOC                     
082700     END-IF                                                               
082800     MOVE ZEROES             TO IO-COUNT                                  
082900     .                                                                    
083000     EJECT                                                                
083100 HB-UPDATE SECTION.                                                       
083200                                                                          
083300     PERFORM UNTIL WS-ADGANG    > WS-ADGANG-TOM OR                        
083400                   IO-COUNT     > MAX-IO-COUNT  OR                        
083500                   SEGMENT-FOUND-EXISTS                                   
083600         MOVE    WS-ADGANG       TO LOC-ADGANG                            
083700                                    W-LOC-ADGANG                          
083800*                                                                         
083900         PERFORM UNTIL WS-ADSEC11  > WS-ADSEC11-TOM OR                    
084000                       IO-COUNT    > MAX-IO-COUNT OR                      
084100                       SEGMENT-FOUND-EXISTS                               
084200             MOVE    WS-ADSEC11     TO W-LOC-ADSEC11                      
084300*                                                                         
084400             PERFORM UNTIL WS-ADLEVEL11 > WS-ADLEVEL11-TOM OR             
084500                           IO-COUNT      > MAX-IO-COUNT OR                
084600                           SEGMENT-FOUND-EXISTS                           
084700                 MOVE    WS-ADLEVEL11   TO W-LOC-ADLEVEL11                
084800*                                                                         
084900                 PERFORM UNTIL WS-ADSEQ    > WS-ADSEQ-TOM OR              
085000                               IO-COUNT    > MAX-IO-COUNT OR              
085100                               SEGMENT-FOUND-EXISTS                       
085200*                                                                         
085300                     MOVE  WS-ADSEQ         TO   W-LOC-ADSEQ              
085400                     MOVE  W-LOC-ADPLATS    TO   LOC-ADPLATS              
085500                     MOVE  ZERO             TO   LOC-KVPLATS              
085600                     PERFORM IMS-ISRT-LOCA-LOC                            
085700                     IF SEGMENT-FOUND                                     
085800                        ADD +1     TO IO-COUNT                            
085900                        ADD +1     TO WS-ADSEQ                            
086000                     END-IF                                               
086100                 END-PERFORM                                              
086200                 IF IO-COUNT  <= MAX-IO-COUNT AND                         
086300                    SEGMENT-FOUND                                         
086400                    MOVE WS-ADSEQ-FOM TO WS-ADSEQ                         
086500                    ADD +1            TO WS-ADLEVEL11                     
086600                 END-IF                                                   
086700*                                                                         
086800             END-PERFORM                                                  
086900             IF IO-COUNT  <= MAX-IO-COUNT AND                             
087000                SEGMENT-FOUND                                             
087100                MOVE WS-ADLEVEL11-FOM TO WS-ADLEVEL11                     
087200* WS-SECTION-STEP INNEHÅLLER 1 OM ALLA SEKTIONER SKALL SKAPAS.            
087300* OM ENDAST DEN UDDA ELLER JÄMNA SIDAN AV GÅNGEN SKALL SKAPAS             
087400* INNEHÅLLER WS-SECTION-STEP 2.                                           
087500                ADD WS-SECTION-STEP   TO WS-ADSEC11                       
087600             END-IF                                                       
087700*                                                                         
087800         END-PERFORM                                                      
087900         IF IO-COUNT  <= MAX-IO-COUNT AND                                 
088000            SEGMENT-FOUND                                                 
088100            MOVE WS-ADSEC11-FOM       TO WS-ADSEC11                       
088200            ADD +1                    TO WS-ADGANG                        
088300         END-IF                                                           
088400     END-PERFORM                                                          
088500                                                                          
088600                                                                          
088700     .                                                                    
088800     EJECT                                                                
088900 HC-CLOSE SECTION.                                                        
089000                                                                          
089100     IF SEGMENT-FOUND-EXISTS                                              
089200           MOVE W-LOC-IDDC            TO MED-IDDC                         
089300           MOVE W-LOC-ADLAGOMR        TO MED-ADLAGOMR                     
089400           MOVE W-LOC-ADGANG          TO MED-ADGANG                       
089500           MOVE W-LOC-ADSEC11         TO MED-ADSEC11                      
089600           MOVE W-LOC-ADLEVEL11       TO MED-ADLEVEL11                    
089700           MOVE W-LOC-ADSEQ           TO MED-ADSEQ                        
089800           MOVE MED-1                 TO MOD-TEMFSFEL                     
089900      ELSE                                                                
090000        IF WS-ADGANG > WS-ADGANG-TOM                                      
090100                                                                          
090200           MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                     
090300           CALL WMEDKONV USING MED-WMEDAREA                               
090400           MOVE MED-MFSINF            TO MOD-TEMFSINF                     
090500*                                                                         
090600*    -- INIT SAVE KEYS                                                    
090700*                                                                         
090800           MOVE '6344'                TO SAVE-IDTRANS                     
090900           MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                    
091000           MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                      
091100           MOVE WS-ADSEC11-FOM        TO SAVE-ADSEC11                     
091200           MOVE WS-ADLEVEL11-FOM      TO SAVE-ADLEVEL11                   
091300           MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                       
091400                                                                          
091500           MOVE '002'                 TO MSGI-KDCALL                      
091600           MOVE SAVE-AREA             TO MSGI-SPAR-AREA                   
091700                                                                          
091800           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
091900         ELSE                                                             
092000           MOVE YES                   TO RESTART-SW                       
092100           MOVE MID-W6I34401          TO 6314-UT-MID-W6I34401             
092200           MOVE W-LOC-ADLAGOMR        TO SAVE-ADLAGOMR                    
092300           MOVE WS-ADGANG             TO SAVE-ADGANG                      
092400           MOVE WS-ADSEC11            TO SAVE-ADSEC11                     
092500           MOVE WS-ADLEVEL11          TO SAVE-ADLEVEL11                   
092600           MOVE WS-ADSEQ              TO SAVE-ADSEQ                       
092700           MOVE '6344'                TO SAVE-IDTRANS                     
092800                                                                          
092900           MOVE '002'      TO MSGI-KDCALL                                 
093000           MOVE SAVE-AREA  TO MSGI-SPAR-AREA                              
093100           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
093200        END-IF                                                            
093300      END-IF                                                              
093400     .                                                                    
093500     EJECT                                                                
093600 MFS-INIT-KEY-FIELD-IN SECTION.                                           
093700                                                                          
093800*    --- ALL INPUT KEY FIELDS                                             
093900                                                                          
094000     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
094100                               MOD-ADLAGOMR-IN                            
094200                               MOD-ADGANG-FOM-IN                          
094300                               MOD-ADGANG-TOM-IN                          
094400                               MOD-ADSEC11-FOM-IN                         
094500                               MOD-ADSEC11-TOM-IN                         
094600                               MOD-ADLEVEL11-FOM-IN                       
094700                               MOD-ADLEVEL11-TOM-IN                       
094800                               MOD-ADSEQ-FOM-IN                           
094900                               MOD-ADSEQ-TOM-IN                           
095000                               MOD-KDAOE-IN                               
095100     .                                                                    
095200     EJECT                                                                
095300 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
095400                                                                          
095500*    --- ALL OUTPUT KEY FIELDS                                            
095600                                                                          
095700     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
095800     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
095900                               MOD-ADGANG-FOM-UT                          
096000                               MOD-ADGANG-TOM-UT                          
096100                               MOD-ADSEC11-FOM-UT                         
096200                               MOD-ADSEC11-TOM-UT                         
096300                               MOD-ADLEVEL11-FOM-UT                       
096400                               MOD-ADLEVEL11-TOM-UT                       
096500                               MOD-ADSEQ-FOM-UT                           
096600                               MOD-ADSEQ-TOM-UT                           
096700                               MOD-KDAOE-UT                               
096800     .                                                                    
096900     EJECT                                                                
097000 MFS-ERASE-FIELD-IN SECTION.                                              
097100                                                                          
097200*    --- ALL INPUT DATA FIELDS                                            
097300     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
097400                                MOD-KDFREQ                                
097500                                MOD-KDSTOR                                
097600                                MOD-TELOC                                 
097700                                MOD-KVMPART                               
097800     .                                                                    
097900     EJECT                                                                
098000 MFS-DONT-TOUCH-FIELD-OUT-IN  SECTION.                                    
098100                                                                          
098200*    --- ALL OUTPUT DATA FIELDS                                           
098300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
098400                                    MOD-KDFREQ                            
098500                                    MOD-KDSTOR                            
098600                                    MOD-TELOC                             
098700                                    MOD-KVMPART                           
098800     .                                                                    
098900     EJECT                                                                
099000* --- IMS SECTIONS ---                                                    
099100     SKIP3                                                                
099200 IMS-GET-MSG SECTION.                                                     
099300                                                                          
099400     MOVE '  QC' TO GOOD-STATUSCODES                                      
099500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099700     PERFORM IMS-STATUSCHECK                                              
099800     .                                                                    
099900     SKIP3                                                                
100000 IMS-INSERT-MSG SECTION.                                                  
100100                                                                          
100200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100300     MOVE SPACE TO GOOD-STATUSCODES                                       
100400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100600     PERFORM IMS-STATUSCHECK                                              
100700     .                                                                    
100800     EJECT                                                                
100900 IMS-ISRT-ALT-MSG-6314  SECTION.                                          
101000     MOVE SPACE  TO GOOD-STATUSCODES                                      
101100     CALL  CBLTDLI  USING ISRT ALT6314-PCB P-TO-P-SW                      
101200     MOVE ALT6314-STATUS-CODE TO STATUS-WS                                
101300     PERFORM IMS-STATUSCHECK                                              
101400     .                                                                    
101500     SKIP3                                                                
101600 IMS-GU-6314 SECTION.                                                     
101700                                                                          
101800     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
101900          DELIMITED BY SIZE INTO SSA1                                     
102000     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
102100          DELIMITED BY SIZE INTO SSA2                                     
102200     MOVE '  GE' TO GOOD-STATUSCODES                                      
102300     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
102400     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSCHECK                                              
102600     .                                                                    
102700     SKIP3                                                                
102800 IMS-GU-6316 SECTION.                                                     
102900                                                                          
103000     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
103100          DELIMITED BY SIZE INTO SSA1                                     
103200     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
103300          DELIMITED BY SIZE INTO SSA2                                     
103400     MOVE '  GE' TO GOOD-STATUSCODES                                      
103500     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
103600     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
103700     PERFORM IMS-STATUSCHECK                                              
103800     .                                                                    
103900     SKIP3                                                                
104000 IMS-ISRT-LOCA-LOC SECTION.                                               
104100                                                                          
104200     MOVE 'WLLOCA01 ' TO SSA1                                             
104300     MOVE '  II' TO GOOD-STATUSCODES                                      
104400     CALL CBLTDLI USING ISRT LOCA-PCB LOC-WDJ801 SSA1                     
104500     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
104600     PERFORM IMS-STATUSCHECK                                              
104700     .                                                                    
104800     SKIP3                                                                
104900 IMS-STATUSCHECK SECTION.                                                 
105000                                                                          
105100     SET STATUS-IX TO 1                                                   
105200     SEARCH GOOD-STATUS                                                   
105300       AT END                                                             
105400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
105500         DELIMITED BY SIZE INTO ERROR-TEXT                                
105600         CALL FELLOG                                                      
105700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
105800         CONTINUE                                                         
105900     END-SEARCH                                                           
106000     .                                                                    
