000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031500.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*      CHANGE LOCATIONS                                                   
000900*                                                                         
001000* CHANGED :    97 - 07 - 09    JOHAN LINDKVIST                            
001100* CHANGED :    98 - 01 - 19    JOHAN LINDKVIST (MASS-DELETE)              
001200* CHANGED :    98 - 02 - 04    JOHAN LINDKVIST (IO-COUNT EXCEEDED)        
001300* CHANGED :                                                               
001400*                                                                         
001500*        THE PROGRAM READS     WL6313 (WDGX)                              
001600*        THE PROGRAM READS     WL6315 (WDGX)                              
001700*        THE PROGRAM ADD/DLET  WLLOCA (WDJ8)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W6T315                                              
002100*        MID:         W6I31501                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W6O31501                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6031500'.            
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
004200 77  MAX-IO-COUNT               PIC S9(4)  VALUE +200  COMP SYNC.         
004300 77  LNG-P-TO-P-PREFIX          PIC S9(4)  VALUE +17   COMP SYNC.         
004400*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004500                                                                          
004600 77  WS-ADLAGOMR                 PIC 9(2)   VALUE ZERO.                   
004700 77  WS-ADGANG-FOM               PIC 9(2)   VALUE ZERO.                   
004800 77  WS-ADGANG-TOM               PIC 9(2)   VALUE ZERO.                   
004900 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
005000 77  WS-ADSEC-FOM                PIC 9(2)   VALUE ZERO.                   
005100 77  WS-ADSEC-TOM                PIC 9(2)   VALUE ZERO.                   
005200 77  WS-ADSEC                    PIC 9(3)   VALUE ZERO.                   
005300 77  WS-ADLEVEL-FOM              PIC 9(2)   VALUE ZERO.                   
005400 77  WS-ADLEVEL-TOM              PIC 9(2)   VALUE ZERO.                   
005500 77  WS-ADLEVEL                  PIC 9(3)   VALUE ZERO.                   
005600 77  WS-ADSEQ-FOM                PIC 9(1)   VALUE ZERO.                   
005700 77  WS-ADSEQ-TOM                PIC 9(1)   VALUE ZERO.                   
005800 77  WS-ADSEQ                    PIC 9(2)   VALUE ZERO.                   
005900 77  WS-KDAOE                    PIC X(1)   VALUE SPACE.                  
006000 77  WS-KDDEL                    PIC X(1)   VALUE SPACE.                  
006100                                                                          
006200 77  WS-SECTION-STEP             PIC 9      VALUE 1.                      
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006500     88  INDATA-OK                           VALUE 'Y'.                   
006600     88  INDATA-WRONG                        VALUE 'N'.                   
006700                                                                          
006800 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006900     88  KEYS-OK                             VALUE 'Y'.                   
007000     88  KEYS-WRONG                          VALUE 'N'.                   
007100                                                                          
007200 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007300     88  RESTART                             VALUE 'Y'.                   
007400                                                                          
007500 77  PRIME                       PIC X        VALUE 'P'.                  
007600 77  BUFFER                      PIC X        VALUE 'R'.                  
007700 77  MIXED                       PIC X        VALUE 'M'.                  
007800 77  ALLA                        PIC X        VALUE 'A'.                  
007900 77  ODD                         PIC X        VALUE 'O'.                  
008000 77  EVEN                        PIC X        VALUE 'E'.                  
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  OWN-MID                             VALUE '6315'.                
008400     88  GOOD-MID                            VALUE '6315'.                
008500     88  HELP-MID                            VALUE '0551'.                
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
010700         'CHANGED UNTIL LOCATION '.                                       
010800     05    MED-IDDC                PIC X(02).                             
010900     05    FILLER                  PIC X(02)   VALUE SPACE.               
011000     05    MED-ADLAGOMR            PIC 9(02).                             
011100     05    FILLER                  PIC X(01)   VALUE SPACE.               
011200     05    MED-ADGANG              PIC 9(02).                             
011300     05    FILLER                  PIC X(01)   VALUE SPACE.               
011400     05    MED-ADSEC               PIC 9(02).                             
011500     05    MED-ADLEVEL             PIC 9(02).                             
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
012800*01  MID -COPY W6I31501                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W6O31501                                                 
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
014900   03     -COPY W6I31501 -PRE 6315-UT-                                    
015000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015100*                                                                         
015200     EJECT                                                                
015300 01  WORK-AREA.                                                           
015400     03 WORK-ADLAGOMR            PIC 9(2).                                
015500     03 WORK-ADGANG              PIC 9(2).                                
015600     03 WORK-ADPLATS.                                                     
015700        05 WORK-ADSEC            PIC 9(2).                                
015800        05 WORK-ADLEVEL          PIC 9(2).                                
015900        05 WORK-ADSEQ            PIC 9(1).                                
016000                                                                          
016100 01  SAVE-AREA.                                                           
016200     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
016300     03 SAVE-ADLAGOMR            PIC 9(2).                                
016400     03 SAVE-ADGANG              PIC 9(2).                                
016500     03 SAVE-ADPLATS.                                                     
016600        05 SAVE-ADSEC            PIC 9(2).                                
016700        05 SAVE-ADLEVEL          PIC 9(2).                                
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
019500             07 W-LOC-ADSEC      PIC 9(2).                                
019600             07 W-LOC-ADLEVEL    PIC 9(2).                                
019700             07 W-LOC-ADSEQ      PIC 9(1).                                
019800                                                                          
019900     03  W-IDDC-B6-X.                                                     
020000         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
020100     SKIP2                                                                
020200*    --- STATUS-KOD FRÅN IMS                                              
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FOUND                       VALUE '  '.                  
020500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
020600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020700     SKIP2                                                                
020800 01  GOOD-STATUSCODES.                                                    
020900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021000     SKIP3                                                                
021100 01  SSA1                        PIC X(64).                               
021200 01  SSA2                        PIC X(64).                               
021300     EJECT                                                                
021400*    --- IMS FUNCTION CODES                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
021900 01  WL631301-AREA.                                                       
022000*    03  -COPY WDGX6313                                                   
022100     EJECT                                                                
022200 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
022300 01  WL631311-AREA.                                                       
022400*    03  -COPY WDGX6314                                                   
022500                                                                          
022600 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
022700 01  WL631501-AREA.                                                       
022800*    03  -COPY WDGX6315                                                   
022900     EJECT                                                                
023000 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
023100 01  WL631511-AREA.                                                       
023200*    03  -COPY WDGX6316                                                   
023300                                                                          
023400 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
023500 01  DLI-IO-WLLOCA01.                                                     
023600*    03  -COPY WDJ801                                                     
023700     EJECT                                                                
023800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023900 01   DLI-IO-AREA-B601.                                                   
024000*     03  -COPY WDB601                                                    
024100     EJECT                                                                
024200 LINKAGE SECTION.                                                         
024300*01  -COPY W0009  -PRE MSG-                                               
024400     EJECT                                                                
024500*01  -COPY W0009  -PRE ALT6315-                                           
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE USEA-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE 6313-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE 6315-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE LOCA-                                              
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008      -PRE WDB6-                                          
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200 PROCEDURE DIVISION  USING MSG-PCB ALT6315-PCB                            
026300           USEA-PCB                                                       
026400           6313-PCB 6315-PCB LOCA-PCB                                     
026500           WDB6-PCB.                                                      
026600 MAIN SECTION.                                                            
026700     ENTRY 'DLITCBL' USING MSG-PCB ALT6315-PCB                            
026800            USEA-PCB                                                      
026900            6313-PCB 6315-PCB LOCA-PCB                                    
027000            WDB6-PCB.                                                     
027100                                                                          
027200     PERFORM IMS-GET-MSG                                                  
027300     IF SEGMENT-FOUND                                                     
027400       PERFORM A-INIT                                                     
027500       IF GOOD-MID OR HELP-MID                                            
027600          PERFORM B-CHECK-KEYS                                            
027700          IF KEYS-OK                                                      
027800             IF MFS-UPDATE                                                
027900                PERFORM G-CHECK-INPUT                                     
028000                IF INDATA-OK                                              
028100                   PERFORM H-UPDATE                                       
028200                END-IF                                                    
028300              ELSE                                                        
028400                IF MFS-FIRST                                              
028500                   PERFORM C-FIRST-PAGE                                   
028600                 ELSE                                                     
028700                   PERFORM E-SAME-PAGE                                    
028800                END-IF                                                    
028900              END-IF                                                      
029000          END-IF                                                          
029100       END-IF                                                             
029200       IF RESTART                                                         
029300          COMPUTE P-TO-P-KVLL =  LNG-P-TO-P-PREFIX +                      
029400                                 LENGTH OF MID-W6I31501                   
029500          MOVE 'W6T315U '     TO P-TO-P-KDTRANS                           
029600          MOVE '6315'         TO P-TO-P-IDTRANS                           
029700          MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                          
029800          MOVE MID-W6I31501   TO 6315-UT-MID-W6I31501                     
029900          PERFORM IMS-ISRT-ALT-MSG-6315                                   
030000        ELSE                                                              
030100          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31501 + 4                   
030200          PERFORM IMS-INSERT-MSG                                          
030300       END-IF                                                             
030400     END-IF                                                               
030500                                                                          
030600     MOVE ZERO TO RETURN-CODE                                             
030700     GOBACK                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 A-INIT SECTION.                                                          
031100                                                                          
031200     IF MSG-DOUBLE-TRANSACTIONS                                           
031300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31501                 
031400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031600     ELSE                                                                 
031700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I31501                  
031800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032000     END-IF                                                               
032100                                                                          
032200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032500                                                                          
032600     MOVE LOW-VALUE TO MSG-AREA                                           
032700     MOVE 'W6O315N1' TO MFS-IDMOD                                         
032800     MOVE '6315' TO MOD-IDTRANS                                           
032900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
033000                                                                          
033100     MOVE NEJ                  TO RESTART-SW                              
033200                                                                          
033300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033400     MOVE '001'             TO MSGI-KDCALL                                
033500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033700     MOVE '6315'            TO MSGI-IDTRANS                               
033800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033900                                                                          
034000     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
034100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
034200                                                                          
034300     IF GOOD-MID OR HELP-MID                                              
034400       CONTINUE                                                           
034500     ELSE                                                                 
034600       MOVE SPACE TO MFS-KDTRTYP                                          
034700       MOVE '7' TO MFS-IDPFK                                              
034800       PERFORM MFS-INIT-KEY-FIELD-IN                                      
034900       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
035000       PERFORM MFS-ERASE-FIELD-IN                                         
035100     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 B-CHECK-KEYS SECTION.                                                    
035500                                                                          
035600                                                                          
035700     MOVE JA                TO KEYS-SW                                    
035800                                                                          
035900     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
036000                               MOD-ADLAGOMR-IN                            
036100                               MOD-ADGANG-FOM-IN                          
036200                               MOD-ADGANG-TOM-IN                          
036300                               MOD-ADSEC-FOM-IN                           
036400                               MOD-ADSEC-TOM-IN                           
036500                               MOD-ADLEVEL-FOM-IN                         
036600                               MOD-ADLEVEL-TOM-IN                         
036700                               MOD-ADSEQ-FOM-IN                           
036800                               MOD-ADSEQ-TOM-IN                           
036900                               MOD-KDAOE-IN                               
037000                                                                          
037100*                                                                         
037200*    -- CONTROL  ON WAREHOUSE                                             
037300*                                                                         
037400     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
037500     PERFORM IMS-GU-WDB601                                                
037600     IF SEGMENT-FOUND                                                     
037700     AND (DCS-SDC                                                         
037800     OR   DCS-NDC-NA                                                      
037900     OR   DCS-NDC-PF                                                      
038000     OR   DCS-NDC-OTHERS)                                                 
038100       CONTINUE                                                           
038200     ELSE                                                                 
038300       MOVE NEJ                 TO KEYS-SW                                
038400     END-IF                                                               
038500*                                                                         
038600*    -- CONTROL  AREA                                                     
038700*                                                                         
038800     IF MID-ADLAGOMR-IN = ALL '+'                                         
038900       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
039000       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
039100     ELSE                                                                 
039200       IF MID-ADLAGOMR-IN NUMERIC                                         
039300          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
039400          MOVE '7' TO MFS-IDPFK                                           
039500          MOVE SPACE TO MFS-KDTRTYP                                       
039600       ELSE                                                               
039700          MOVE NEJ             TO KEYS-SW                                 
039800       END-IF                                                             
039900     END-IF                                                               
040000*                                                                         
040100*    -- CONTROL  AISLE FROM                                               
040200*                                                                         
040300     IF MID-ADGANG-FOM-IN = ALL '+'                                       
040400       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
040500       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
040600     ELSE                                                                 
040700       IF MID-ADGANG-FOM-IN NUMERIC                                       
040800          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
040900          MOVE '7' TO MFS-IDPFK                                           
041000          MOVE SPACE TO MFS-KDTRTYP                                       
041100       ELSE                                                               
041200          MOVE NEJ               TO KEYS-SW                               
041300       END-IF                                                             
041400     END-IF                                                               
041500*                                                                         
041600*    -- CONTROL  AISLE THRU                                               
041700*                                                                         
041800     IF MID-ADGANG-TOM-IN = ALL '+'                                       
041900       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
042000       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
042100     ELSE                                                                 
042200       IF MID-ADGANG-TOM-IN NUMERIC                                       
042300          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
042400          MOVE '7' TO MFS-IDPFK                                           
042500          MOVE SPACE TO MFS-KDTRTYP                                       
042600       ELSE                                                               
042700          MOVE NEJ               TO KEYS-SW                               
042800       END-IF                                                             
042900     END-IF                                                               
043000*                                                                         
043100*    -- CONTROL  SECTION FROM                                             
043200*                                                                         
043300     IF MID-ADSEC-FOM-IN = ALL '+'                                        
043400       INSPECT MID-ADSEC-FOM-UT REPLACING LEADING SPACE BY ZERO           
043500       MOVE MID-ADSEC-FOM-UT    TO WS-ADSEC-FOM                           
043600     ELSE                                                                 
043700       IF MID-ADSEC-FOM-IN NUMERIC                                        
043800          MOVE MID-ADSEC-FOM-IN TO WS-ADSEC-FOM                           
043900          MOVE '7' TO MFS-IDPFK                                           
044000          MOVE SPACE TO MFS-KDTRTYP                                       
044100       ELSE                                                               
044200          MOVE NEJ              TO KEYS-SW                                
044300       END-IF                                                             
044400     END-IF                                                               
044500                                                                          
044600*                                                                         
044700*    -- CONTROL  SECTION THRU                                             
044800*                                                                         
044900     IF MID-ADSEC-TOM-IN = ALL '+'                                        
045000       INSPECT MID-ADSEC-TOM-UT REPLACING LEADING SPACE BY ZERO           
045100       MOVE MID-ADSEC-TOM-UT    TO WS-ADSEC-TOM                           
045200     ELSE                                                                 
045300       IF MID-ADSEC-TOM-IN NUMERIC                                        
045400          MOVE MID-ADSEC-TOM-IN TO WS-ADSEC-TOM                           
045500          MOVE '7' TO MFS-IDPFK                                           
045600          MOVE SPACE TO MFS-KDTRTYP                                       
045700       ELSE                                                               
045800          MOVE NEJ              TO KEYS-SW                                
045900       END-IF                                                             
046000     END-IF                                                               
046100                                                                          
046200*                                                                         
046300*    -- CONTROL  LEVEL FROM                                               
046400*                                                                         
046500     IF MID-ADLEVEL-FOM-IN = ALL '+'                                      
046600       INSPECT MID-ADLEVEL-FOM-UT REPLACING LEADING SPACE BY ZERO         
046700       MOVE MID-ADLEVEL-FOM-UT    TO WS-ADLEVEL-FOM                       
046800     ELSE                                                                 
046900       IF MID-ADLEVEL-FOM-IN NUMERIC                                      
047000          MOVE MID-ADLEVEL-FOM-IN TO WS-ADLEVEL-FOM                       
047100          MOVE '7' TO MFS-IDPFK                                           
047200          MOVE SPACE TO MFS-KDTRTYP                                       
047300       ELSE                                                               
047400          MOVE NEJ                TO KEYS-SW                              
047500       END-IF                                                             
047600     END-IF                                                               
047700                                                                          
047800*                                                                         
047900*    -- CONTROL  LEVEL THRU                                               
048000*                                                                         
048100     IF MID-ADLEVEL-TOM-IN = ALL '+'                                      
048200       INSPECT MID-ADLEVEL-TOM-UT REPLACING LEADING SPACE BY ZERO         
048300       MOVE MID-ADLEVEL-TOM-UT    TO WS-ADLEVEL-TOM                       
048400     ELSE                                                                 
048500       IF MID-ADLEVEL-TOM-IN NUMERIC                                      
048600          MOVE MID-ADLEVEL-TOM-IN TO WS-ADLEVEL-TOM                       
048700          MOVE '7' TO MFS-IDPFK                                           
048800          MOVE SPACE TO MFS-KDTRTYP                                       
048900       ELSE                                                               
049000          MOVE NEJ                TO KEYS-SW                              
049100       END-IF                                                             
049200     END-IF                                                               
049300                                                                          
049400*                                                                         
049500*    -- CONTROL  PLACEMENT FROM  (SEQUENCE)                               
049600*                                                                         
049700     IF MID-ADSEQ-FOM-IN = ALL '+'                                        
049800       INSPECT MID-ADSEQ-FOM-UT REPLACING LEADING SPACE BY ZERO           
049900       MOVE MID-ADSEQ-FOM-UT      TO WS-ADSEQ-FOM                         
050000     ELSE                                                                 
050100       IF MID-ADSEQ-FOM-IN NUMERIC                                        
050200          MOVE MID-ADSEQ-FOM-IN TO WS-ADSEQ-FOM                           
050300          MOVE '7' TO MFS-IDPFK                                           
050400          MOVE SPACE TO MFS-KDTRTYP                                       
050500       ELSE                                                               
050600          MOVE NEJ                TO KEYS-SW                              
050700       END-IF                                                             
050800     END-IF                                                               
050900                                                                          
051000*                                                                         
051100*    -- CONTROL  PLACEMENT THRU  (SEQUENCE)                               
051200*                                                                         
051300     IF MID-ADSEQ-TOM-IN = ALL '+'                                        
051400       INSPECT MID-ADSEQ-TOM-UT REPLACING LEADING SPACE BY ZERO           
051500       MOVE MID-ADSEQ-TOM-UT      TO WS-ADSEQ-TOM                         
051600     ELSE                                                                 
051700       IF MID-ADSEQ-TOM-IN NUMERIC                                        
051800          MOVE MID-ADSEQ-TOM-IN   TO WS-ADSEQ-TOM                         
051900          MOVE '7' TO MFS-IDPFK                                           
052000          MOVE SPACE TO MFS-KDTRTYP                                       
052100       ELSE                                                               
052200          MOVE NEJ                TO KEYS-SW                              
052300       END-IF                                                             
052400     END-IF                                                               
052500                                                                          
052600*                                                                         
052700*    -- CONTROL  ALL/ODD/EVEN                                             
052800*                                                                         
052900     IF MID-KDAOE-IN = ALL '+'                                            
053000       MOVE MID-KDAOE-UT          TO WS-KDAOE                             
053100     ELSE                                                                 
053200       IF MID-KDAOE-IN = ALLA OR ODD OR EVEN                              
053300          MOVE MID-KDAOE-IN       TO WS-KDAOE                             
053400          MOVE '7' TO MFS-IDPFK                                           
053500          MOVE SPACE TO MFS-KDTRTYP                                       
053600       ELSE                                                               
053700          MOVE NEJ                TO KEYS-SW                              
053800       END-IF                                                             
053900     END-IF                                                               
054000                                                                          
054100*                                                                         
054200*    -- FILL MOD KEY-OUTPUT FIELDS                                        
054300*                                                                         
054400     MOVE W-IDDC-B6      TO  MOD-IDDC-UT                                  
054500     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
054600     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
054700     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
054800     MOVE WS-ADSEC-FOM   TO  MOD-ADSEC-FOM-UT                             
054900     MOVE WS-ADSEC-TOM   TO  MOD-ADSEC-TOM-UT                             
055000     MOVE WS-ADLEVEL-FOM TO  MOD-ADLEVEL-FOM-UT                           
055100     MOVE WS-ADLEVEL-TOM TO  MOD-ADLEVEL-TOM-UT                           
055200     MOVE WS-ADSEQ-FOM   TO  MOD-ADSEQ-FOM-UT                             
055300     MOVE WS-ADSEQ-TOM   TO  MOD-ADSEQ-TOM-UT                             
055400     MOVE WS-KDAOE       TO  MOD-KDAOE-UT                                 
055500                                                                          
055600* KONTROLL FÖR ATT UNDVIKA 'NOLLOR' (0) I INDATA                          
055700*    IF WS-ADGANG-FOM     = ZERO  OR                                      
055800*       WS-ADSEC-FOM      = ZERO  OR                                      
055900*       WS-ADLEVEL-FOM    = ZERO  OR                                      
056000*       WS-ADSEQ-FOM      = ZERO  OR                                      
056100*       WS-ADGANG-TOM     = ZERO  OR                                      
056200*       WS-ADSEC-TOM      = ZERO  OR                                      
056300*       WS-ADLEVEL-TOM    = ZERO  OR                                      
056400*       WS-ADSEQ-TOM      = ZERO                                          
056500*                                                                         
056600*          MOVE NEJ               TO KEYS-SW                              
056700*    END-IF                                                               
056800                                                                          
056900     IF KEYS-WRONG                                                        
057000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
057100       CALL WMEDKONV USING MED-WMEDAREA                                   
057200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057300      ELSE                                                                
057400* KONTROLL AV INTERVALL FÖR IN-DATA                                       
057500       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
057600          WS-ADSEC-FOM    >  WS-ADSEC-TOM   OR                            
057700          WS-ADLEVEL-FOM  >  WS-ADLEVEL-TOM OR                            
057800          WS-ADSEQ-FOM    >  WS-ADSEQ-TOM                                 
057900                                                                          
058000          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
058100          CALL WMEDKONV USING MED-WMEDAREA                                
058200          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
058300          MOVE NEJ                 TO KEYS-SW                             
058400       END-IF                                                             
058500* KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER               
058600       EVALUATE WS-KDAOE                                                  
058700       WHEN ALLA                                                          
058800         MOVE 1 TO WS-SECTION-STEP                                        
058900       WHEN EVEN                                                          
059000* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
059100         IF FUNCTION MOD (WS-ADSEC-FOM 2) = 0   AND                       
059200            FUNCTION MOD (WS-ADSEC-TOM 2) = 0                             
059300            MOVE 2 TO WS-SECTION-STEP                                     
059400         ELSE                                                             
059500            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
059600            CALL WMEDKONV USING MED-WMEDAREA                              
059700            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
059800            MOVE NEJ               TO KEYS-SW                             
059900         END-IF                                                           
060000       WHEN ODD                                                           
060100* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
060200         IF FUNCTION MOD (WS-ADSEC-FOM 2) = 1   AND                       
060300            FUNCTION MOD (WS-ADSEC-TOM 2) = 1                             
060400            MOVE 2 TO WS-SECTION-STEP                                     
060500         ELSE                                                             
060600            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
060700            CALL WMEDKONV USING MED-WMEDAREA                              
060800            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
060900            MOVE NEJ               TO KEYS-SW                             
061000         END-IF                                                           
061100       END-EVALUATE                                                       
061200     END-IF                                                               
061300                                                                          
061400     IF KEYS-WRONG                                                        
061500       PERFORM MFS-ERASE-FIELD-IN                                         
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900 C-FIRST-PAGE SECTION.                                                    
062000                                                                          
062100     PERFORM MFS-ERASE-FIELD-IN                                           
062200                                                                          
062300     MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOC-ATTR                         
062400*                                                                         
062500*    -- INIT SAVE KEYS                                                    
062600*                                                                         
062700     MOVE '6315'                TO SAVE-IDTRANS                           
062800     MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                          
062900     MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                            
063000     MOVE WS-ADSEC-FOM          TO SAVE-ADSEC                             
063100     MOVE WS-ADLEVEL-FOM        TO SAVE-ADLEVEL                           
063200     MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                             
063300                                                                          
063400     MOVE '002'                 TO MSGI-KDCALL                            
063500     MOVE SAVE-AREA             TO MSGI-SPAR-AREA                         
063600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063700                                                                          
063800     MOVE INF-PRESS-PF11        TO MED-IDMFSINF                           
063900     CALL WMEDKONV USING MED-WMEDAREA                                     
064000     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
064100     .                                                                    
064200     EJECT                                                                
064300 E-SAME-PAGE SECTION.                                                     
064400                                                                          
064500     IF OWN-MID OR HELP-MID                                               
064600       IF MID-KDLOC   = ALL '+' AND                                       
064700          MID-KDDEL   = ALL '+' AND                                       
064800          MID-KDFREQ  = ALL '+' AND                                       
064900          MID-KDSTOR  = ALL '+' AND                                       
065000          MID-TELOC   = ALL '+' AND                                       
065100          MID-KVMPART = ALL '+'                                           
065200          PERFORM MFS-ERASE-FIELD-IN                                      
065300        ELSE                                                              
065400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
065500         CALL WMEDKONV USING MED-WMEDAREA                                 
065600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
065700         PERFORM EA-MID-INDATA-TO-MOD                                     
065800       END-IF                                                             
065900     ELSE                                                                 
066000       PERFORM MFS-ERASE-FIELD-IN                                         
066100     END-IF                                                               
066200     .                                                                    
066300     EJECT                                                                
066400 EA-MID-INDATA-TO-MOD SECTION.                                            
066500                                                                          
066600     IF MID-KDDEL NOT = ALL '+'                                           
066700        MOVE MID-KDDEL             TO MOD-KDDEL                           
066800        MOVE MFS-ADD-READ-FIELD    TO MOD-KDDEL-ATTR                      
066900      ELSE                                                                
067000        MOVE MFS-ERASE-FIELD       TO MOD-KDDEL                           
067100     END-IF                                                               
067200                                                                          
067300     IF MID-KDLOC NOT = ALL '+'                                           
067400        MOVE MID-KDLOC             TO MOD-KDLOC                           
067500        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOC-ATTR                      
067600      ELSE                                                                
067700        MOVE MFS-ERASE-FIELD       TO MOD-KDLOC                           
067800     END-IF                                                               
067900                                                                          
068000     IF MID-KDFREQ NOT = ALL '+'                                          
068100        MOVE MID-KDFREQ             TO MOD-KDFREQ                         
068200        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
068300      ELSE                                                                
068400        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ                         
068500     END-IF                                                               
068600                                                                          
068700     IF MID-KDSTOR NOT = ALL '+'                                          
068800        MOVE MID-KDSTOR             TO MOD-KDSTOR                         
068900        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
069000      ELSE                                                                
069100        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
069200     END-IF                                                               
069300                                                                          
069400     IF MID-TELOC NOT = ALL '+'                                           
069500        MOVE MID-TELOC             TO MOD-TELOC                           
069600        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOC-ATTR                      
069700      ELSE                                                                
069800        MOVE MFS-ERASE-FIELD       TO MOD-TELOC                           
069900     END-IF                                                               
070000                                                                          
070100     IF MID-KVMPART NOT = ALL '+'                                         
070200        MOVE MID-KVMPART           TO MOD-KVMPART                         
070300        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPART-ATTR                    
070400      ELSE                                                                
070500        MOVE MFS-ERASE-FIELD       TO MOD-KVMPART                         
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 G-CHECK-INPUT SECTION.                                                   
071000                                                                          
071100     MOVE JA   TO INDATA-SW                                               
071200                                                                          
071300                                                                          
071400     IF MID-KDLOC      = ALL '+' AND                                      
071500        MID-KDDEL      = ALL '+' AND                                      
071600        MID-KDFREQ     = ALL '+' AND                                      
071700        MID-KDSTOR     = ALL '+' AND                                      
071800        MID-TELOC      = ALL '+' AND                                      
071900        MID-KVMPART    = ALL '+'                                          
072000                                                                          
072100        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
072200        CALL WMEDKONV USING MED-WMEDAREA                                  
072300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
072400                                                                          
072500        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
072600        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
072700        MOVE NEJ TO INDATA-SW                                             
072800      ELSE                                                                
072900        PERFORM GA-CHECK-INPUT-CHG                                        
073000        IF INDATA-WRONG                                                   
073100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
073200           CALL WMEDKONV USING MED-WMEDAREA                               
073300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
073400           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
073500           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
073600         END-IF                                                           
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000 GA-CHECK-INPUT-CHG SECTION.                                              
074100*                                                                         
074200*   -- CONTROL  DELETE CONFIRM                                            
074300*                                                                         
074400     IF MID-KDDEL    = ALL '+'                                            
074500         MOVE MFS-ALPHA-FIELD-OK TO MOD-KDDEL-ATTR                        
074600     ELSE                                                                 
074700       IF MID-KDDEL    = 'C' OR 'D' OR SPACE                              
074800         MOVE MFS-ALPHA-FIELD-OK TO MOD-KDDEL-ATTR                        
074900       ELSE                                                               
075000         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDDEL-ATTR                     
075100         MOVE NEJ TO INDATA-SW                                            
075200       END-IF                                                             
075300     END-IF                                                               
075400                                                                          
075500*                                                                         
075600*    -- CONTROL  ON MULTIPLE PART                                         
075700*                                                                         
075800      IF MID-KVMPART NOT = ALL '+'                                        
075900        IF MID-KVMPART NOT NUMERIC                                        
076000          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                    
076100          MOVE NEJ                 TO INDATA-SW                           
076200         ELSE                                                             
076300          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPART-ATTR                       
076400        END-IF                                                            
076500      END-IF                                                              
076600*                                                                         
076700*    -- CONTROL  ON SIZE REMARK                                           
076800*                                                                         
076900                                                                          
077000     IF MID-TELOC NOT = ALL '+'                                           
077100        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOC-ATTR                         
077200     END-IF                                                               
077300                                                                          
077400*                                                                         
077500*    -- CONTROL  ON STORAGE CODE                                          
077600*                                                                         
077700     IF MID-KDSTOR   NOT = ALL '+'                                        
077800        MOVE W-IDDC-B6           TO W-6315-IDDC                           
077900        MOVE MID-KDSTOR          TO W-KDSTOR                              
078000        PERFORM  IMS-GU-6316                                              
078100        IF SEGMENT-MISSING                                                
078200           MOVE NEJ                     TO INDATA-SW                      
078300           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                
078400         ELSE                                                             
078500           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                
078600        END-IF                                                            
078700     END-IF                                                               
078800                                                                          
078900*                                                                         
079000*    -- CONTROL  ON FREQUENCY CODE                                        
079100*                                                                         
079200     IF MID-KDFREQ   NOT = ALL '+'                                        
079300        IF MID-KDFREQ   NUMERIC                                           
079400           MOVE W-IDDC-B6           TO W-6313-IDDC                        
079500           MOVE MID-KDFREQ          TO W-KDFREQ                           
079600           PERFORM  IMS-GU-6314                                           
079700           IF SEGMENT-MISSING                                             
079800              MOVE NEJ                     TO INDATA-SW                   
079900              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR             
080000            ELSE                                                          
080100              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR             
080200           END-IF                                                         
080300         ELSE                                                             
080400           MOVE NEJ                     TO INDATA-SW                      
080500           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                
080600        END-IF                                                            
080700     END-IF                                                               
080800                                                                          
080900*                                                                         
081000*    -- CONTROL  ON LOCATION TYPE                                         
081100*                                                                         
081200     IF MID-KDLOC    NOT = ALL '+'                                        
081300        IF MID-KDLOC  =  PRIME OR BUFFER OR MIXED                         
081400           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOC-ATTR                   
081500         ELSE                                                             
081600           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                   
081700           MOVE NEJ                   TO INDATA-SW                        
081800        END-IF                                                            
081900     END-IF                                                               
082000                                                                          
082100     .                                                                    
082200     EJECT                                                                
082300 H-UPDATE SECTION.                                                        
082400                                                                          
082500     PERFORM HA-INIT                                                      
082600     PERFORM HB-UPDATE                                                    
082700     PERFORM HC-CLOSE                                                     
082800                                                                          
082900     .                                                                    
083000     EJECT                                                                
083100 HA-INIT SECTION.                                                         
083200                                                                          
083300     MOVE W-IDDC-B6          TO W-LOC-IDDC                                
083400                                LOC-IDDC                                  
083500     MOVE SAVE-ADLAGOMR        TO W-LOC-ADLAGOMR                          
083600                                LOC-ADLAGOMR                              
083700     MOVE SAVE-ADGANG          TO WS-ADGANG                               
083800     MOVE SAVE-ADSEC           TO WS-ADSEC                                
083900     MOVE SAVE-ADLEVEL         TO WS-ADLEVEL                              
084000     MOVE SAVE-ADSEQ           TO WS-ADSEQ                                
084100                                                                          
084200     IF MID-KDDEL  NOT = ALL '+'                                          
084300        MOVE MID-KDDEL   TO MOD-KDDEL                                     
084400     END-IF                                                               
084500     IF MID-KDLOC  NOT = ALL '+'                                          
084600        MOVE MID-KDLOC   TO MOD-KDLOC                                     
084700     END-IF                                                               
084800     IF MID-KDFREQ  NOT = ALL '+'                                         
084900        MOVE MID-KDFREQ  TO MOD-KDFREQ                                    
085000     END-IF                                                               
085100     IF MID-KDSTOR NOT = ALL '+'                                          
085200        MOVE MID-KDSTOR  TO MOD-KDSTOR                                    
085300     END-IF                                                               
085400     IF MID-KVMPART NOT = ALL '+'                                         
085500        MOVE MID-KVMPART TO MOD-KVMPART                                   
085600     END-IF                                                               
085700     IF MID-TELOC  NOT = ALL '+'                                          
085800        MOVE MID-TELOC   TO MOD-TELOC                                     
085900     END-IF                                                               
086000                                                                          
086100     MOVE ZEROES             TO IO-COUNT                                  
086200                                                                          
086300     IF MID-KDDEL = 'D'                                                   
086400       MOVE JA   TO  WS-KDDEL                                             
086500     ELSE                                                                 
086600       MOVE NEJ  TO  WS-KDDEL                                             
086700     END-IF                                                               
086800                                                                          
086900     .                                                                    
087000     EJECT                                                                
087100 HB-UPDATE SECTION.                                                       
087200                                                                          
087300     PERFORM UNTIL WS-ADGANG    > WS-ADGANG-TOM OR                        
087400                   IO-COUNT     > MAX-IO-COUNT                            
087500         MOVE    WS-ADGANG       TO LOC-ADGANG                            
087600                                    W-LOC-ADGANG                          
087700                                                                          
087800         PERFORM UNTIL WS-ADSEC    > WS-ADSEC-TOM OR                      
087900                       IO-COUNT    > MAX-IO-COUNT                         
088000             MOVE    WS-ADSEC       TO W-LOC-ADSEC                        
088100                                                                          
088200             PERFORM UNTIL WS-ADLEVEL > WS-ADLEVEL-TOM  OR                
088300                           IO-COUNT      > MAX-IO-COUNT                   
088400                 MOVE    WS-ADLEVEL     TO W-LOC-ADLEVEL                  
088500                                                                          
088600                 PERFORM UNTIL WS-ADSEQ    > WS-ADSEQ-TOM OR              
088700                               IO-COUNT    > MAX-IO-COUNT                 
088800                                                                          
088900                     MOVE  WS-ADSEQ         TO   W-LOC-ADSEQ              
089000                     MOVE  W-LOC-ADPLATS    TO   LOC-ADPLATS              
089100                                                                          
089200                     PERFORM  IMS-GHU-LOCA                                
089300                     ADD +1                 TO IO-COUNT                   
089400                                                                          
089500                     IF SEGMENT-FOUND                                     
089600                       IF WS-KDDEL = NEJ                                  
089700                          IF MID-KDLOC NOT = ALL '+'                      
089800                             MOVE MID-KDLOC TO LOC-KDLOC                  
089900                          END-IF                                          
090000                          IF MID-KDFREQ NOT = ALL '+'                     
090100                             MOVE MID-KDFREQ TO LOC-KDFREQ                
090200                          END-IF                                          
090300                          IF MID-KDSTOR NOT = ALL '+'                     
090400                             MOVE MID-KDSTOR TO LOC-KDSTOR                
090500                          END-IF                                          
090600                          IF MID-KVMPART NOT = ALL '+'                    
090700                             MOVE MID-KVMPART TO LOC-KVMPART              
090800                          END-IF                                          
090900                          IF MID-TELOC NOT = ALL '+'                      
091000                             MOVE MID-TELOC TO LOC-TELOC                  
091100                          END-IF                                          
091200                                                                          
091300                          PERFORM IMS-REPL-LOCA-LOC                       
091400                        ELSE                                              
091500                          PERFORM IMS-DLET-LOCA-LOC                       
091600                        END-IF                                            
091700                        ADD +1     TO IO-COUNT                            
091800                     END-IF                                               
091900                     ADD +1     TO WS-ADSEQ                               
092000                 END-PERFORM                                              
092100                 IF IO-COUNT  <= MAX-IO-COUNT                             
092200                    MOVE WS-ADSEQ-FOM TO WS-ADSEQ                         
092300                    ADD +1            TO WS-ADLEVEL                       
092400                 END-IF                                                   
092500                                                                          
092600             END-PERFORM                                                  
092700             IF IO-COUNT  <= MAX-IO-COUNT                                 
092800                MOVE WS-ADLEVEL-FOM   TO WS-ADLEVEL                       
092900                ADD WS-SECTION-STEP   TO WS-ADSEC                         
093000             END-IF                                                       
093100                                                                          
093200         END-PERFORM                                                      
093300         IF IO-COUNT  <= MAX-IO-COUNT                                     
093400            MOVE WS-ADSEC-FOM         TO WS-ADSEC                         
093500            ADD +1                    TO WS-ADGANG                        
093600         END-IF                                                           
093700     END-PERFORM                                                          
093800                                                                          
093900                                                                          
094000     .                                                                    
094100     EJECT                                                                
094200 HC-CLOSE SECTION.                                                        
094300                                                                          
094400      IF WS-ADGANG > WS-ADGANG-TOM                                        
094500                                                                          
094600         MOVE W-LOC-IDDC            TO MED-IDDC                           
094700         MOVE W-LOC-ADLAGOMR        TO MED-ADLAGOMR                       
094800         MOVE W-LOC-ADGANG          TO MED-ADGANG                         
094900         MOVE W-LOC-ADSEC           TO MED-ADSEC                          
095000         MOVE W-LOC-ADLEVEL         TO MED-ADLEVEL                        
095100         MOVE W-LOC-ADSEQ           TO MED-ADSEQ                          
095200         MOVE MED-1                 TO MOD-TEMFSFEL                       
095300                                                                          
095400         MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                       
095500         CALL WMEDKONV USING MED-WMEDAREA                                 
095600         MOVE MED-MFSINF            TO MOD-TEMFSINF                       
095700*                                                                         
095800*    -- INIT SAVE KEYS                                                    
095900*                                                                         
096000         MOVE '6315'                TO SAVE-IDTRANS                       
096100         MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                      
096200         MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                        
096300         MOVE WS-ADSEC-FOM          TO SAVE-ADSEC                         
096400         MOVE WS-ADLEVEL-FOM        TO SAVE-ADLEVEL                       
096500         MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                         
096600                                                                          
096700         MOVE '002'                 TO MSGI-KDCALL                        
096800         MOVE SAVE-AREA             TO MSGI-SPAR-AREA                     
096900                                                                          
097000         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
097100       ELSE                                                               
097200         MOVE JA                    TO RESTART-SW                         
097300         MOVE MID-W6I31501          TO 6315-UT-MID-W6I31501               
097400         MOVE W-LOC-ADLAGOMR        TO SAVE-ADLAGOMR                      
097500         MOVE WS-ADGANG             TO SAVE-ADGANG                        
097600         MOVE WS-ADSEC              TO SAVE-ADSEC                         
097700         MOVE WS-ADLEVEL            TO SAVE-ADLEVEL                       
097800         MOVE WS-ADSEQ              TO SAVE-ADSEQ                         
097900         MOVE '6315'                TO SAVE-IDTRANS                       
098000                                                                          
098100         MOVE '002'      TO MSGI-KDCALL                                   
098200         MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                
098300         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
098400      END-IF                                                              
098500     .                                                                    
098600     EJECT                                                                
098700 MFS-INIT-KEY-FIELD-IN SECTION.                                           
098800                                                                          
098900*    --- ALL INPUT KEY FIELDS                                             
099000                                                                          
099100     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
099200                               MOD-ADLAGOMR-IN                            
099300                               MOD-ADGANG-FOM-IN                          
099400                               MOD-ADGANG-TOM-IN                          
099500                               MOD-ADSEC-FOM-IN                           
099600                               MOD-ADSEC-TOM-IN                           
099700                               MOD-ADLEVEL-FOM-IN                         
099800                               MOD-ADLEVEL-TOM-IN                         
099900                               MOD-ADSEQ-FOM-IN                           
100000                               MOD-ADSEQ-TOM-IN                           
100100                               MOD-KDAOE-IN                               
100200     .                                                                    
100300     EJECT                                                                
100400 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
100500                                                                          
100600*    --- ALL OUTPUT KEY FIELDS                                            
100700                                                                          
100800     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
100900     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
101000                               MOD-ADGANG-FOM-UT                          
101100                               MOD-ADGANG-TOM-UT                          
101200                               MOD-ADSEC-FOM-UT                           
101300                               MOD-ADSEC-TOM-UT                           
101400                               MOD-ADLEVEL-FOM-UT                         
101500                               MOD-ADLEVEL-TOM-UT                         
101600                               MOD-ADSEQ-FOM-UT                           
101700                               MOD-ADSEQ-TOM-UT                           
101800                               MOD-KDAOE-UT                               
101900     .                                                                    
102000     EJECT                                                                
102100 MFS-ERASE-FIELD-IN SECTION.                                              
102200                                                                          
102300*    --- ALL INPUT DATA FIELDS                                            
102400     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
102500                                MOD-KDDEL                                 
102600                                MOD-KDFREQ                                
102700                                MOD-KDSTOR                                
102800                                MOD-TELOC                                 
102900                                MOD-KVMPART                               
103000     .                                                                    
103100     EJECT                                                                
103200 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
103300                                                                          
103400*    --- ALL OUTPUT DATA FIELDS                                           
103500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
103600                                    MOD-KDDEL                             
103700                                    MOD-KDFREQ                            
103800                                    MOD-KDSTOR                            
103900                                    MOD-TELOC                             
104000                                    MOD-KVMPART                           
104100                                    MOD-KDDEL                             
104200     .                                                                    
104300     SKIP3                                                                
104400 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
104500                                                                          
104600*    --- ALL INPUT DATA FIELDS                                            
104700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
104800                                    MOD-KDDEL                             
104900                                    MOD-KDFREQ                            
105000                                    MOD-KDSTOR                            
105100                                    MOD-TELOC                             
105200                                    MOD-KVMPART                           
105300                                    MOD-KDDEL                             
105400     .                                                                    
105500     EJECT                                                                
105600* --- IMS SECTIONS ---                                                    
105700     SKIP3                                                                
105800 IMS-GET-MSG SECTION.                                                     
105900                                                                          
106000     MOVE '  QC' TO GOOD-STATUSCODES                                      
106100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
106200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106300     PERFORM IMS-STATUSCHECK                                              
106400     .                                                                    
106500     SKIP3                                                                
106600 IMS-INSERT-MSG SECTION.                                                  
106700                                                                          
106800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
106900     MOVE SPACE TO GOOD-STATUSCODES                                       
107000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
107100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107200     PERFORM IMS-STATUSCHECK                                              
107300     .                                                                    
107400     EJECT                                                                
107500 IMS-ISRT-ALT-MSG-6315  SECTION.                                          
107600     MOVE SPACE  TO GOOD-STATUSCODES                                      
107700     CALL  CBLTDLI  USING ISRT ALT6315-PCB P-TO-P-SW                      
107800     MOVE ALT6315-STATUS-CODE TO STATUS-WS                                
107900     PERFORM IMS-STATUSCHECK                                              
108000     .                                                                    
108100     SKIP3                                                                
108200 IMS-GU-6314 SECTION.                                                     
108300                                                                          
108400     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
108500          DELIMITED BY SIZE INTO SSA1                                     
108600     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
108700          DELIMITED BY SIZE INTO SSA2                                     
108800     MOVE '  GE' TO GOOD-STATUSCODES                                      
108900     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
109000     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
109100     PERFORM IMS-STATUSCHECK                                              
109200     .                                                                    
109300     SKIP3                                                                
109400 IMS-GU-6316 SECTION.                                                     
109500                                                                          
109600     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
109700          DELIMITED BY SIZE INTO SSA1                                     
109800     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
109900          DELIMITED BY SIZE INTO SSA2                                     
110000     MOVE '  GE' TO GOOD-STATUSCODES                                      
110100     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
110200     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
110300     PERFORM IMS-STATUSCHECK                                              
110400     .                                                                    
110500     SKIP3                                                                
110600 IMS-GHU-LOCA SECTION.                                                    
110700                                                                          
110800     STRING 'WLLOCA01(WDJ801KY =' W-WDJ8KEY-X ')'                         
110900          DELIMITED BY SIZE INTO SSA1                                     
111000     MOVE '  GE' TO GOOD-STATUSCODES                                      
111100     CALL CBLTDLI USING GHU LOCA-PCB LOC-WDJ801 SSA1                      
111200     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
111300     PERFORM IMS-STATUSCHECK                                              
111400     .                                                                    
111500     SKIP3                                                                
111600 IMS-REPL-LOCA-LOC SECTION.                                               
111700                                                                          
111800     MOVE '  ' TO GOOD-STATUSCODES                                        
111900     CALL CBLTDLI USING REPL LOCA-PCB LOC-WDJ801                          
112000     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
112100     PERFORM IMS-STATUSCHECK                                              
112200     .                                                                    
112300     SKIP3                                                                
112400 IMS-DLET-LOCA-LOC SECTION.                                               
112500                                                                          
112600     MOVE '  ' TO GOOD-STATUSCODES                                        
112700     CALL CBLTDLI USING DLET LOCA-PCB LOC-WDJ801                          
112800     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSCHECK                                              
113000     .                                                                    
113100     EJECT                                                                
113200                                                                          
113300 IMS-GU-WDB601    SECTION.                                                
113400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     MOVE '  GE' TO GOOD-STATUSCODES                                      
113700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
113800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
113900     PERFORM IMS-STATUSCHECK                                              
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-STATUSCHECK SECTION.                                                 
114300                                                                          
114400     SET STATUS-IX TO 1                                                   
114500     SEARCH GOOD-STATUS                                                   
114600       AT END                                                             
114700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
114800         DELIMITED BY SIZE INTO ERROR-TEXT                                
114900         CALL FELLOG                                                      
115000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
115100         CONTINUE                                                         
115200     END-SEARCH                                                           
115300     .                                                                    
