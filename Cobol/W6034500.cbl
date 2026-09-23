000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6034500.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*      CHANGE LOCATIONS                                                   
000900*                                                                         
001000* CHANGED :    97 - 07 - 09    JOHAN LINDKVIST                            
001100* CHANGED :    98 - 01 - 19    JOHAN LINDKVIST (MASS-DELETE)              
001110* CHANGED :    98 - 02 - 04    JOHAN LINDKVIST (IO-COUNT EXCEEDED)        
001120* CHANGED :                                                               
001200*                                                                         
001300*        THE PROGRAM READS     WL6313 (WDGX)                              
001400*        THE PROGRAM READS     WL6315 (WDGX)                              
001500*        THE PROGRAM ADD/DLET  WLLOCA (WDJ8)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W6T345                                              
001900*        MID:         W6I34501                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W6O34501                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6034500'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'Y'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  IO-COUNT                   PIC S9(4)  VALUE +0    COMP SYNC.         
004000 77  MAX-IO-COUNT               PIC S9(4)  VALUE +200  COMP SYNC.         
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
005800 77  WS-KDDEL                    PIC X(1)   VALUE SPACE.                  
005900                                                                          
006000 77  WS-SECTION-STEP             PIC 9      VALUE 1.                      
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006300     88  INDATA-OK                           VALUE 'Y'.                   
006400     88  INDATA-WRONG                        VALUE 'N'.                   
006500                                                                          
006600 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006700     88  KEYS-OK                             VALUE 'Y'.                   
006800     88  KEYS-WRONG                          VALUE 'N'.                   
006900                                                                          
007000 77  RESTART-SW                  PIC X       VALUE 'N'.                   
007100     88  RESTART                             VALUE 'Y'.                   
007200                                                                          
007300 77  PRIME                       PIC X        VALUE 'P'.                  
007400 77  BUFFER                      PIC X        VALUE 'R'.                  
007500 77  MIXED                       PIC X        VALUE 'M'.                  
007600 77  ALLA                        PIC X        VALUE 'A'.                  
007700 77  ODD                         PIC X        VALUE 'O'.                  
007800 77  EVEN                        PIC X        VALUE 'E'.                  
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  OWN-MID                             VALUE '6345'.                
008200     88  GOOD-MID                            VALUE '6345'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500*      --- VALID IDDC CODES                                               
008600*                                                                         
008700*01    -COPY WWDC99                                                       
008800       EJECT                                                              
010400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010500 01  GENERAL-SUBPROGRAMS.                                                 
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     EJECT                                                                
011100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011200*01 -COPY WMEDAREA                                                        
011300     SKIP3                                                                
011400 01  MESSAGE-CODES.                                                       
011500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012000     03  WRONG-INTERVAL-INFO     PIC X(3)    VALUE '738'.                 
012100     EJECT                                                                
012200   03    MED-1.                                                           
012300     05    FILLER                  PIC X(23)   VALUE                      
012400         'CHANGED UNTIL LOCATION '.                                       
012500     05    MED-IDDC                PIC X(02).                             
012600     05    FILLER                  PIC X(02)   VALUE SPACE.               
012700     05    MED-ADLAGOMR            PIC 9(02).                             
012800     05    FILLER                  PIC X(01)   VALUE SPACE.               
012900     05    MED-ADGANG              PIC 9(02).                             
013000     05    FILLER                  PIC X(01)   VALUE SPACE.               
013100     05    MED-ADSEC11             PIC 9(03).                             
013200     05    MED-ADLEVEL11           PIC 9(01).                             
013300     05    MED-ADSEQ               PIC 9(01).                             
013400     05    FILLER                  PIC X(01)   VALUE '.'.                 
013500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013800     SKIP3                                                                
013900*01 -COPY WMSGINIT                                                        
014000     EJECT                                                                
014100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014400     SKIP3                                                                
014500*01  MID -COPY W6I34501                                                   
014600     EJECT                                                                
014700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014800     SKIP3                                                                
014900*01  -COPY WMSGAREA                                                       
015000     EJECT                                                                
015100     03  MOD REDEFINES MSG-AREA.                                          
015200*      05  -COPY W6O34501                                                 
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015500     SKIP3                                                                
015600*01  -COPY WMFSAREA                                                       
015700     EJECT                                                                
015800 01      P-TO-P-SW.                                                       
015900                                                                          
016000   03     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.          
016100   03     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.              
016200   03     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.              
016300   03     P-TO-P-KDTRANS          PIC X(8).                               
016400   03     P-TO-P-IDTRANS          PIC X(4).                               
016500   03     P-TO-P-KDMFSFOR         PIC X(1).                               
016600   03     -COPY W6I34501 -PRE 6345-UT-                                    
016700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
016800*                                                                         
016900     EJECT                                                                
017000 01  WORK-AREA.                                                           
017100     03 WORK-ADLAGOMR            PIC 9(2).                                
017200     03 WORK-ADGANG              PIC 9(2).                                
017300     03 WORK-ADPLATS.                                                     
017400        05 WORK-ADSEC11          PIC 9(3).                                
017500        05 WORK-ADLEVEL11        PIC 9(1).                                
017600        05 WORK-ADSEQ            PIC 9(1).                                
017700                                                                          
017800 01  SAVE-AREA.                                                           
017900     03 SAVE-IDTRANS             PIC X(4)    VALUE  SPACE.                
018000     03 SAVE-ADLAGOMR            PIC 9(2).                                
018100     03 SAVE-ADGANG              PIC 9(2).                                
018200     03 SAVE-ADPLATS.                                                     
018300        05 SAVE-ADSEC11          PIC 9(3).                                
018400        05 SAVE-ADLEVEL11        PIC 9(1).                                
018500        05 SAVE-ADSEQ            PIC 9(1).                                
018600                                                                          
018700                                                                          
018800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018900     SKIP3                                                                
019000 01  KEYS-TO-DLI.                                                         
019100     03  W-WDGXKEY-6313-X.                                                
019200          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
019300          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
019400          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
019500                                                                          
019600     03  W-WDGXKEY-6314-X.                                                
019700         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
019800                                                                          
019900     03  W-WDGXKEY-6315-X.                                                
020000          05 W-6315-IDHTYP       PIC X(4)    VALUE '6315'.                
020100          05 W-6315-IDDC         PIC X(2)    VALUE SPACE.                 
020200          05 W-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
020300                                                                          
020400     03  W-WDGXKEY-6316-X.                                                
020500         05  W-KDSTOR            PIC X(3)    VALUE SPACE.                 
020600                                                                          
020700     03  W-WDJ8KEY-X.                                                     
020800         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
020900         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
021000         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
021100         05  W-LOC-ADPLATS.                                               
021200             07 W-LOC-ADSEC11    PIC 9(3).                                
021300             07 W-LOC-ADLEVEL11  PIC 9(1).                                
021400             07 W-LOC-ADSEQ      PIC 9(1).                                
021500     SKIP2                                                                
021600*    --- STATUS-KOD FRÅN IMS                                              
021700 01  STATUS-WS                   PIC XX.                                  
021800     88  SEGMENT-FOUND                       VALUE '  '.                  
021900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
022000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022100     SKIP2                                                                
022200 01  GOOD-STATUSCODES.                                                    
022300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022400     SKIP3                                                                
022500 01  SSA1                        PIC X(64).                               
022600 01  SSA2                        PIC X(64).                               
022700     EJECT                                                                
022800*    --- IMS FUNCTION CODES                                               
022900*01  -COPY W0003                                                          
023000     EJECT                                                                
023100*    ---  DLI INPUT-OUTPUT AREA                                           
023200 01  FILLER         PIC X(20) VALUE 'WL631301-AREA'.                      
023300 01  WL631301-AREA.                                                       
023400*    03  -COPY WDGX6313                                                   
023500     EJECT                                                                
023600 01  FILLER         PIC X(20) VALUE 'WL631311-AREA'.                      
023700 01  WL631311-AREA.                                                       
023800*    03  -COPY WDGX6314                                                   
023900                                                                          
024000 01  FILLER         PIC X(20) VALUE 'WL631501-AREA'.                      
024100 01  WL631501-AREA.                                                       
024200*    03  -COPY WDGX6315                                                   
024300     EJECT                                                                
024400 01  FILLER         PIC X(20) VALUE 'WL631511-AREA'.                      
024500 01  WL631511-AREA.                                                       
024600*    03  -COPY WDGX6316                                                   
024700                                                                          
024800 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
024900 01  DLI-IO-WLLOCA01.                                                     
025000*    03  -COPY WDJ801                                                     
025100     EJECT                                                                
025200 LINKAGE SECTION.                                                         
025300*01  -COPY W0009  -PRE MSG-                                               
025400     EJECT                                                                
025500*01  -COPY W0009  -PRE ALT6345-                                           
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE USEA-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008  -PRE 6313-                                              
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE 6315-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE LOCA-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900 PROCEDURE DIVISION  USING MSG-PCB ALT6345-PCB                            
027000           USEA-PCB                                                       
027100           6313-PCB 6315-PCB LOCA-PCB.                                    
027200 MAIN SECTION.                                                            
027300     ENTRY 'DLITCBL' USING MSG-PCB ALT6345-PCB                            
027400            USEA-PCB                                                      
027500            6313-PCB 6315-PCB LOCA-PCB.                                   
027600                                                                          
027700     PERFORM IMS-GET-MSG                                                  
027800     IF SEGMENT-FOUND                                                     
027900       PERFORM A-INIT                                                     
028000       IF GOOD-MID OR HELP-MID                                            
028100          PERFORM B-CHECK-KEYS                                            
028200          IF KEYS-OK                                                      
028300             IF MFS-UPDATE                                                
028700                PERFORM G-CHECK-INPUT                                     
028900                IF INDATA-OK                                              
029000                   PERFORM H-UPDATE                                       
029100                END-IF                                                    
029200              ELSE                                                        
029300                IF MFS-FIRST                                              
029400                   PERFORM C-FIRST-PAGE                                   
029500                 ELSE                                                     
029600                   PERFORM E-SAME-PAGE                                    
029700                END-IF                                                    
029800              END-IF                                                      
029900          END-IF                                                          
030000       END-IF                                                             
030100       IF RESTART                                                         
030200          COMPUTE P-TO-P-KVLL =  LNG-P-TO-P-PREFIX +                      
030300                                 LENGTH OF MID-W6I34501                   
030400          MOVE 'W6T345U '     TO P-TO-P-KDTRANS                           
030500          MOVE '6345'         TO P-TO-P-IDTRANS                           
030600          MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                          
030700          MOVE MID-W6I34501   TO 6345-UT-MID-W6I34501                     
030800          PERFORM IMS-ISRT-ALT-MSG-6345                                   
030900        ELSE                                                              
031000          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34501 + 4                   
031100          PERFORM IMS-INSERT-MSG                                          
031200       END-IF                                                             
031300     END-IF                                                               
031400                                                                          
031500     MOVE ZERO TO RETURN-CODE                                             
031600     GOBACK                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 A-INIT SECTION.                                                          
032000                                                                          
032100     IF MSG-DOUBLE-TRANSACTIONS                                           
032200       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34501                 
032300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032500     ELSE                                                                 
032600       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I34501                  
032700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032900     END-IF                                                               
033000                                                                          
033100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033400                                                                          
033500     MOVE LOW-VALUE TO MSG-AREA                                           
033600     MOVE 'W6O345N1' TO MFS-IDMOD                                         
033700     MOVE '6345' TO MOD-IDTRANS                                           
033800     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
033900                                                                          
034000     MOVE NEJ                  TO RESTART-SW                              
034100                                                                          
034200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034300     MOVE '001'             TO MSGI-KDCALL                                
034400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034600     MOVE '6345'            TO MSGI-IDTRANS                               
034700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034800                                                                          
034900     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
035000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
035100     MOVE MSGI-IDDC         TO WS-IDDC                                    
035200                                                                          
035300     IF GOOD-MID OR HELP-MID                                              
035400       CONTINUE                                                           
035500     ELSE                                                                 
035600       MOVE SPACE TO MFS-KDTRTYP                                          
035700       MOVE '7' TO MFS-IDPFK                                              
035800       PERFORM MFS-INIT-KEY-FIELD-IN                                      
035900       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
036000       PERFORM MFS-ERASE-FIELD-IN                                         
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 B-CHECK-KEYS SECTION.                                                    
036500                                                                          
036600                                                                          
036700     MOVE JA                TO KEYS-SW                                    
036800                                                                          
036900     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
037000                               MOD-ADLAGOMR-IN                            
037100                               MOD-ADGANG-FOM-IN                          
037200                               MOD-ADGANG-TOM-IN                          
037300                               MOD-ADSEC11-FOM-IN                         
037400                               MOD-ADSEC11-TOM-IN                         
037500                               MOD-ADLEVEL11-FOM-IN                       
037600                               MOD-ADLEVEL11-TOM-IN                       
037700                               MOD-ADSEQ-FOM-IN                           
037800                               MOD-ADSEQ-TOM-IN                           
037900                               MOD-KDAOE-IN                               
038100                                                                          
038200*                                                                         
038300*    -- CONTROL  ON WAREHOUSE                                             
038400*                                                                         
038500     IF CDC                                                               
038600       CONTINUE                                                           
038700     ELSE                                                                 
038800       MOVE NEJ                 TO KEYS-SW                                
038900     END-IF                                                               
039000*                                                                         
039100*    -- CONTROL  AREA                                                     
039200*                                                                         
039300     IF MID-ADLAGOMR-IN = ALL '+'                                         
039400       INSPECT MID-ADLAGOMR-UT REPLACING LEADING SPACE BY ZERO            
039500       MOVE MID-ADLAGOMR-UT    TO WS-ADLAGOMR                             
039600     ELSE                                                                 
039700       IF MID-ADLAGOMR-IN NUMERIC                                         
039800          MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                             
039810          MOVE '7' TO MFS-IDPFK                                           
039820          MOVE SPACE TO MFS-KDTRTYP                                       
039900       ELSE                                                               
040000          MOVE NEJ             TO KEYS-SW                                 
040100       END-IF                                                             
040200     END-IF                                                               
040300*                                                                         
040400*    -- CONTROL  AISLE FROM                                               
040500*                                                                         
040600     IF MID-ADGANG-FOM-IN = ALL '+'                                       
040700       INSPECT MID-ADGANG-FOM-UT REPLACING LEADING SPACE BY ZERO          
040800       MOVE MID-ADGANG-FOM-UT    TO WS-ADGANG-FOM                         
040900     ELSE                                                                 
041000       IF MID-ADGANG-FOM-IN NUMERIC                                       
041100          MOVE MID-ADGANG-FOM-IN TO WS-ADGANG-FOM                         
041110          MOVE '7' TO MFS-IDPFK                                           
041120          MOVE SPACE TO MFS-KDTRTYP                                       
041200       ELSE                                                               
041300          MOVE NEJ               TO KEYS-SW                               
041400       END-IF                                                             
041500     END-IF                                                               
041600*                                                                         
041700*    -- CONTROL  AISLE THRU                                               
041800*                                                                         
041900     IF MID-ADGANG-TOM-IN = ALL '+'                                       
042000       INSPECT MID-ADGANG-TOM-UT REPLACING LEADING SPACE BY ZERO          
042100       MOVE MID-ADGANG-TOM-UT    TO WS-ADGANG-TOM                         
042200     ELSE                                                                 
042300       IF MID-ADGANG-TOM-IN NUMERIC                                       
042400          MOVE MID-ADGANG-TOM-IN TO WS-ADGANG-TOM                         
042410          MOVE '7' TO MFS-IDPFK                                           
042420          MOVE SPACE TO MFS-KDTRTYP                                       
042500       ELSE                                                               
042600          MOVE NEJ               TO KEYS-SW                               
042700       END-IF                                                             
042800     END-IF                                                               
042900*                                                                         
043000*    -- CONTROL  SECTION FROM                                             
043100*                                                                         
043200     IF MID-ADSEC11-FOM-IN = ALL '+'                                      
043300       INSPECT MID-ADSEC11-FOM-UT REPLACING LEADING SPACE BY ZERO         
043400       MOVE MID-ADSEC11-FOM-UT  TO WS-ADSEC11-FOM                         
043500     ELSE                                                                 
043600       IF MID-ADSEC11-FOM-IN NUMERIC                                      
043700          MOVE MID-ADSEC11-FOM-IN TO WS-ADSEC11-FOM                       
043710          MOVE '7' TO MFS-IDPFK                                           
043720          MOVE SPACE TO MFS-KDTRTYP                                       
043800       ELSE                                                               
043900          MOVE NEJ              TO KEYS-SW                                
044000       END-IF                                                             
044100     END-IF                                                               
044200                                                                          
044300*                                                                         
044400*    -- CONTROL  SECTION THRU                                             
044500*                                                                         
044600     IF MID-ADSEC11-TOM-IN = ALL '+'                                      
044700       INSPECT MID-ADSEC11-TOM-UT REPLACING LEADING SPACE BY ZERO         
044800       MOVE MID-ADSEC11-TOM-UT  TO WS-ADSEC11-TOM                         
044900     ELSE                                                                 
045000       IF MID-ADSEC11-TOM-IN NUMERIC                                      
045100          MOVE MID-ADSEC11-TOM-IN TO WS-ADSEC11-TOM                       
045110          MOVE '7' TO MFS-IDPFK                                           
045120          MOVE SPACE TO MFS-KDTRTYP                                       
045200       ELSE                                                               
045300          MOVE NEJ              TO KEYS-SW                                
045400       END-IF                                                             
045500     END-IF                                                               
045600                                                                          
045700*                                                                         
045800*    -- CONTROL  LEVEL FROM                                               
045900*                                                                         
046000     IF MID-ADLEVEL11-FOM-IN = ALL '+'                                    
046100      INSPECT MID-ADLEVEL11-FOM-UT REPLACING LEADING SPACE BY ZERO        
046200       MOVE MID-ADLEVEL11-FOM-UT  TO WS-ADLEVEL11-FOM                     
046300     ELSE                                                                 
046400       IF MID-ADLEVEL11-FOM-IN NUMERIC                                    
046500          MOVE MID-ADLEVEL11-FOM-IN TO WS-ADLEVEL11-FOM                   
046510          MOVE '7' TO MFS-IDPFK                                           
046520          MOVE SPACE TO MFS-KDTRTYP                                       
046600       ELSE                                                               
046700          MOVE NEJ                TO KEYS-SW                              
046800       END-IF                                                             
046900     END-IF                                                               
047000                                                                          
047100*                                                                         
047200*    -- CONTROL  LEVEL THRU                                               
047300*                                                                         
047400     IF MID-ADLEVEL11-TOM-IN = ALL '+'                                    
047500      INSPECT MID-ADLEVEL11-TOM-UT REPLACING LEADING SPACE BY ZERO        
047600       MOVE MID-ADLEVEL11-TOM-UT  TO WS-ADLEVEL11-TOM                     
047700     ELSE                                                                 
047800       IF MID-ADLEVEL11-TOM-IN NUMERIC                                    
047900          MOVE MID-ADLEVEL11-TOM-IN TO WS-ADLEVEL11-TOM                   
047910          MOVE '7' TO MFS-IDPFK                                           
047920          MOVE SPACE TO MFS-KDTRTYP                                       
048000       ELSE                                                               
048100          MOVE NEJ                TO KEYS-SW                              
048200       END-IF                                                             
048300     END-IF                                                               
048400                                                                          
048500*                                                                         
048600*    -- CONTROL  PLACEMENT FROM  (SEQUENCE)                               
048700*                                                                         
048800     IF MID-ADSEQ-FOM-IN = ALL '+'                                        
048900       INSPECT MID-ADSEQ-FOM-UT REPLACING LEADING SPACE BY ZERO           
049000       MOVE MID-ADSEQ-FOM-UT      TO WS-ADSEQ-FOM                         
049100     ELSE                                                                 
049200       IF MID-ADSEQ-FOM-IN NUMERIC                                        
049300          MOVE MID-ADSEQ-FOM-IN TO WS-ADSEQ-FOM                           
049310          MOVE '7' TO MFS-IDPFK                                           
049320          MOVE SPACE TO MFS-KDTRTYP                                       
049400       ELSE                                                               
049500          MOVE NEJ                TO KEYS-SW                              
049600       END-IF                                                             
049700     END-IF                                                               
049800                                                                          
049900*                                                                         
050000*    -- CONTROL  PLACEMENT THRU  (SEQUENCE)                               
050100*                                                                         
050200     IF MID-ADSEQ-TOM-IN = ALL '+'                                        
050300       INSPECT MID-ADSEQ-TOM-UT REPLACING LEADING SPACE BY ZERO           
050400       MOVE MID-ADSEQ-TOM-UT      TO WS-ADSEQ-TOM                         
050500     ELSE                                                                 
050600       IF MID-ADSEQ-TOM-IN NUMERIC                                        
050700          MOVE MID-ADSEQ-TOM-IN   TO WS-ADSEQ-TOM                         
050710          MOVE '7' TO MFS-IDPFK                                           
050720          MOVE SPACE TO MFS-KDTRTYP                                       
050800       ELSE                                                               
050900          MOVE NEJ                TO KEYS-SW                              
051000       END-IF                                                             
051100     END-IF                                                               
051200                                                                          
051300*                                                                         
051400*    -- CONTROL  ALL/ODD/EVEN                                             
051500*                                                                         
051600     IF MID-KDAOE-IN = ALL '+'                                            
051700       MOVE MID-KDAOE-UT          TO WS-KDAOE                             
051800     ELSE                                                                 
051900       IF MID-KDAOE-IN = ALLA OR ODD OR EVEN                              
052000          MOVE MID-KDAOE-IN       TO WS-KDAOE                             
052010          MOVE '7' TO MFS-IDPFK                                           
052020          MOVE SPACE TO MFS-KDTRTYP                                       
052100       ELSE                                                               
052200          MOVE NEJ                TO KEYS-SW                              
052300       END-IF                                                             
052400     END-IF                                                               
052500                                                                          
054300*                                                                         
054400*    -- FILL MOD KEY-OUTPUT FIELDS                                        
054500*                                                                         
054600     MOVE WS-IDDC        TO  MOD-IDDC-UT                                  
054700     MOVE WS-ADLAGOMR    TO  MOD-ADLAGOMR-UT                              
054800     MOVE WS-ADGANG-FOM  TO  MOD-ADGANG-FOM-UT                            
054900     MOVE WS-ADGANG-TOM  TO  MOD-ADGANG-TOM-UT                            
055000     MOVE WS-ADSEC11-FOM TO  MOD-ADSEC11-FOM-UT                           
055100     MOVE WS-ADSEC11-TOM TO  MOD-ADSEC11-TOM-UT                           
055200     MOVE WS-ADLEVEL11-FOM TO MOD-ADLEVEL11-FOM-UT                        
055300     MOVE WS-ADLEVEL11-TOM TO MOD-ADLEVEL11-TOM-UT                        
055400     MOVE WS-ADSEQ-FOM   TO  MOD-ADSEQ-FOM-UT                             
055500     MOVE WS-ADSEQ-TOM   TO  MOD-ADSEQ-TOM-UT                             
055510     MOVE WS-KDAOE       TO  MOD-KDAOE-UT                                 
055600                                                                          
055700* KONTROLL FÖR ATT UNDVIKA 'NOLLOR' (0) I INDATA                          
055800*    IF WS-ADGANG-FOM     = ZERO  OR                                      
055900*       WS-ADSEC11-FOM    = ZERO  OR                                      
056000*       WS-ADLEVEL11-FOM  = ZERO  OR                                      
056100*       WS-ADSEQ-FOM      = ZERO  OR                                      
056200*       WS-ADGANG-TOM     = ZERO  OR                                      
056300*       WS-ADSEC11-TOM    = ZERO  OR                                      
056400*       WS-ADLEVEL11-TOM  = ZERO  OR                                      
056500*       WS-ADSEQ-TOM      = ZERO                                          
056600*                                                                         
056700*          MOVE NEJ               TO KEYS-SW                              
056800*    END-IF                                                               
056900                                                                          
057000     IF KEYS-WRONG                                                        
057100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
057200       CALL WMEDKONV USING MED-WMEDAREA                                   
057300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
057400      ELSE                                                                
057500* KONTROLL AV INTERVALL FÖR IN-DATA                                       
057600       IF WS-ADGANG-FOM   >  WS-ADGANG-TOM  OR                            
057700          WS-ADSEC11-FOM  >  WS-ADSEC11-TOM OR                            
057800          WS-ADLEVEL11-FOM > WS-ADLEVEL11-TOM OR                          
057900          WS-ADSEQ-FOM    >  WS-ADSEQ-TOM                                 
058000                                                                          
058100          MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                        
058200          CALL WMEDKONV USING MED-WMEDAREA                                
058300          MOVE MED-MFSFEL          TO MOD-TEMFSFEL                        
058400          MOVE NEJ                 TO KEYS-SW                             
058500       END-IF                                                             
058600* KONTROLL FÖR ATT KOLLA A/O/E GENTEMOT ANGIVNA INTERVALLER               
058700       EVALUATE WS-KDAOE                                                  
058800       WHEN ALLA                                                          
058900         MOVE 1 TO WS-SECTION-STEP                                        
059000       WHEN EVEN                                                          
059100* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
059200         IF FUNCTION MOD (WS-ADSEC11-FOM 2) = 0 AND                       
059300            FUNCTION MOD (WS-ADSEC11-TOM 2) = 0                           
059400            MOVE 2 TO WS-SECTION-STEP                                     
059500         ELSE                                                             
059600            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
059700            CALL WMEDKONV USING MED-WMEDAREA                              
059800            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
059900            MOVE NEJ               TO KEYS-SW                             
060000         END-IF                                                           
060100       WHEN ODD                                                           
060200* FUNCTION MOD GER RESTEN AV DIVISION, MAO. 1 OM UDDA, 0 OM JÄMNT         
060300         IF FUNCTION MOD (WS-ADSEC11-FOM 2) = 1 AND                       
060400            FUNCTION MOD (WS-ADSEC11-TOM 2) = 1                           
060500            MOVE 2 TO WS-SECTION-STEP                                     
060600         ELSE                                                             
060700            MOVE WRONG-INTERVAL-INFO TO MED-IDMFSFEL                      
060800            CALL WMEDKONV USING MED-WMEDAREA                              
060900            MOVE MED-MFSFEL        TO MOD-TEMFSFEL                        
061000            MOVE NEJ               TO KEYS-SW                             
061100         END-IF                                                           
061200       END-EVALUATE                                                       
061300     END-IF                                                               
061400                                                                          
061500     IF KEYS-WRONG                                                        
061600       PERFORM MFS-ERASE-FIELD-IN                                         
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 C-FIRST-PAGE SECTION.                                                    
062100                                                                          
062200     PERFORM MFS-ERASE-FIELD-IN                                           
062300                                                                          
062400     MOVE MFS-ADD-SET-CURSOR    TO MOD-KDLOC-ATTR                         
062500*                                                                         
062600*    -- INIT SAVE KEYS                                                    
062700*                                                                         
062800     MOVE '6345'                TO SAVE-IDTRANS                           
062900     MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                          
063000     MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                            
063100     MOVE WS-ADSEC11-FOM        TO SAVE-ADSEC11                           
063200     MOVE WS-ADLEVEL11-FOM      TO SAVE-ADLEVEL11                         
063300     MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                             
063400                                                                          
063500     MOVE '002'                 TO MSGI-KDCALL                            
063600     MOVE SAVE-AREA             TO MSGI-SPAR-AREA                         
063700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
063800                                                                          
063900     MOVE INF-PRESS-PF11        TO MED-IDMFSINF                           
064000     CALL WMEDKONV USING MED-WMEDAREA                                     
064100     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
064200     .                                                                    
064300     EJECT                                                                
064400 E-SAME-PAGE SECTION.                                                     
064500                                                                          
064600     IF OWN-MID OR HELP-MID                                               
064700       IF MID-KDLOC   = ALL '+' AND                                       
064800          MID-KDDEL   = ALL '+' AND                                       
064810          MID-KDFREQ  = ALL '+' AND                                       
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
067210     IF MID-KDLOC NOT = ALL '+'                                           
067220        MOVE MID-KDLOC             TO MOD-KDLOC                           
067230        MOVE MFS-ADD-READ-FIELD    TO MOD-KDLOC-ATTR                      
067240      ELSE                                                                
067250        MOVE MFS-ERASE-FIELD       TO MOD-KDLOC                           
067260     END-IF                                                               
067270                                                                          
067300     IF MID-KDFREQ NOT = ALL '+'                                          
067400        MOVE MID-KDFREQ             TO MOD-KDFREQ                         
067500        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
067600      ELSE                                                                
067700        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ                         
067800     END-IF                                                               
067900                                                                          
068000     IF MID-KDSTOR NOT = ALL '+'                                          
068100        MOVE MID-KDSTOR             TO MOD-KDSTOR                         
068200        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
068300      ELSE                                                                
068400        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR                         
068500     END-IF                                                               
068600                                                                          
068700     IF MID-TELOC NOT = ALL '+'                                           
068800        MOVE MID-TELOC             TO MOD-TELOC                           
068900        MOVE MFS-ADD-READ-FIELD    TO MOD-TELOC-ATTR                      
069000      ELSE                                                                
069100        MOVE MFS-ERASE-FIELD       TO MOD-TELOC                           
069200     END-IF                                                               
069300                                                                          
069400     IF MID-KVMPART NOT = ALL '+'                                         
069500        MOVE MID-KVMPART           TO MOD-KVMPART                         
069600        MOVE MFS-ADD-READ-FIELD    TO MOD-KVMPART-ATTR                    
069700      ELSE                                                                
069800        MOVE MFS-ERASE-FIELD       TO MOD-KVMPART                         
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 G-CHECK-INPUT SECTION.                                                   
070300                                                                          
070400     MOVE JA   TO INDATA-SW                                               
070410                                                                          
070510                                                                          
070600     IF MID-KDLOC      = ALL '+' AND                                      
070700        MID-KDDEL      = ALL '+' AND                                      
070710        MID-KDFREQ     = ALL '+' AND                                      
070800        MID-KDSTOR     = ALL '+' AND                                      
070900        MID-TELOC      = ALL '+' AND                                      
071000        MID-KVMPART    = ALL '+'                                          
071100                                                                          
071200        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
071300        CALL WMEDKONV USING MED-WMEDAREA                                  
071400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
071500                                                                          
071600        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
071700        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
071800        MOVE NEJ TO INDATA-SW                                             
071900      ELSE                                                                
072000        PERFORM GA-CHECK-INPUT-CHG                                        
072100        IF INDATA-WRONG                                                   
072200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
072300           CALL WMEDKONV USING MED-WMEDAREA                               
072400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
072500           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
072600           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
072700         END-IF                                                           
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100 GA-CHECK-INPUT-CHG SECTION.                                              
073200*                                                                         
073210*   -- CONTROL  DELETE CONFIRM                                            
073211*                                                                         
073220     IF MID-KDDEL    = ALL '+'                                            
073230         MOVE MFS-ALPHA-FIELD-OK TO MOD-KDDEL-ATTR                        
073240     ELSE                                                                 
073250       IF MID-KDDEL    = 'C' OR 'D' OR SPACE                              
073260         MOVE MFS-ALPHA-FIELD-OK TO MOD-KDDEL-ATTR                        
073270       ELSE                                                               
073280         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDDEL-ATTR                     
073290         MOVE NEJ TO INDATA-SW                                            
073292       END-IF                                                             
073293     END-IF                                                               
073294                                                                          
073300*                                                                         
073400*    -- CONTROL  ON MULTIPLE PART                                         
073500*                                                                         
073600      IF MID-KVMPART NOT = ALL '+'                                        
073700        IF MID-KVMPART NOT NUMERIC                                        
073800          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVMPART-ATTR                    
073900          MOVE NEJ                 TO INDATA-SW                           
074000         ELSE                                                             
074100          MOVE MFS-NUM-FIELD-OK TO MOD-KVMPART-ATTR                       
074200        END-IF                                                            
074300      END-IF                                                              
074400*                                                                         
074500*    -- CONTROL  ON SIZE REMARK                                           
074600*                                                                         
074700                                                                          
074800     IF MID-TELOC NOT = ALL '+'                                           
074900        MOVE MFS-ALPHA-FIELD-OK TO MOD-TELOC-ATTR                         
075000     END-IF                                                               
075100                                                                          
075200*                                                                         
075300*    -- CONTROL  ON STORAGE CODE                                          
075400*                                                                         
075500     IF MID-KDSTOR   NOT = ALL '+'                                        
075600        MOVE WS-IDDC             TO W-6315-IDDC                           
075700        MOVE MID-KDSTOR          TO W-KDSTOR                              
075800        PERFORM  IMS-GU-6316                                              
075900        IF SEGMENT-MISSING                                                
076000           MOVE NEJ                     TO INDATA-SW                      
076100           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                
076200         ELSE                                                             
076300           MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                
076400        END-IF                                                            
076500     END-IF                                                               
076600                                                                          
076700*                                                                         
076800*    -- CONTROL  ON FREQUENCY CODE                                        
076900*                                                                         
077000     IF MID-KDFREQ   NOT = ALL '+'                                        
077100        IF MID-KDFREQ   NUMERIC                                           
077200           MOVE WS-IDDC             TO W-6313-IDDC                        
077300           MOVE MID-KDFREQ          TO W-KDFREQ                           
077400           PERFORM  IMS-GU-6314                                           
077500           IF SEGMENT-MISSING                                             
077600              MOVE NEJ                     TO INDATA-SW                   
077700              MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR             
077800            ELSE                                                          
077900              MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR             
078000           END-IF                                                         
078100         ELSE                                                             
078200           MOVE NEJ                     TO INDATA-SW                      
078300           MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                
078400        END-IF                                                            
078500     END-IF                                                               
078600                                                                          
078700*                                                                         
078800*    -- CONTROL  ON LOCATION TYPE                                         
078900*                                                                         
079000     IF MID-KDLOC    NOT = ALL '+'                                        
079100        IF MID-KDLOC  =  PRIME OR BUFFER OR MIXED                         
079200           MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDLOC-ATTR                   
079300         ELSE                                                             
079400           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDLOC-ATTR                   
079500           MOVE NEJ                   TO INDATA-SW                        
079600        END-IF                                                            
079700     END-IF                                                               
079800                                                                          
079900     .                                                                    
080000     EJECT                                                                
080100 H-UPDATE SECTION.                                                        
080200                                                                          
080300     PERFORM HA-INIT                                                      
080400     PERFORM HB-UPDATE                                                    
080500     PERFORM HC-CLOSE                                                     
080600                                                                          
080700     .                                                                    
080800     EJECT                                                                
080900 HA-INIT SECTION.                                                         
081000                                                                          
081100     MOVE WS-IDDC            TO W-LOC-IDDC                                
081200                                LOC-IDDC                                  
081300     MOVE SAVE-ADLAGOMR        TO W-LOC-ADLAGOMR                          
081400                                LOC-ADLAGOMR                              
081500     MOVE SAVE-ADGANG          TO WS-ADGANG                               
081600     MOVE SAVE-ADSEC11         TO WS-ADSEC11                              
081700     MOVE SAVE-ADLEVEL11       TO WS-ADLEVEL11                            
081800     MOVE SAVE-ADSEQ           TO WS-ADSEQ                                
081901                                                                          
081910     IF MID-KDDEL  NOT = ALL '+'                                          
081920        MOVE MID-KDDEL   TO MOD-KDDEL                                     
081930     END-IF                                                               
082000     IF MID-KDLOC  NOT = ALL '+'                                          
082100        MOVE MID-KDLOC   TO MOD-KDLOC                                     
082200     END-IF                                                               
082300     IF MID-KDFREQ  NOT = ALL '+'                                         
082400        MOVE MID-KDFREQ  TO MOD-KDFREQ                                    
082500     END-IF                                                               
082600     IF MID-KDSTOR NOT = ALL '+'                                          
082700        MOVE MID-KDSTOR  TO MOD-KDSTOR                                    
082800     END-IF                                                               
082900     IF MID-KVMPART NOT = ALL '+'                                         
083000        MOVE MID-KVMPART TO MOD-KVMPART                                   
083100     END-IF                                                               
083200     IF MID-TELOC  NOT = ALL '+'                                          
083300        MOVE MID-TELOC   TO MOD-TELOC                                     
083400     END-IF                                                               
083500                                                                          
083600     MOVE ZEROES             TO IO-COUNT                                  
083610                                                                          
083620     IF MID-KDDEL = 'D'                                                   
083630       MOVE JA   TO  WS-KDDEL                                             
083640     ELSE                                                                 
083641       MOVE NEJ  TO  WS-KDDEL                                             
083642     END-IF                                                               
083650                                                                          
083700     .                                                                    
083800     EJECT                                                                
083900 HB-UPDATE SECTION.                                                       
084000                                                                          
084100     PERFORM UNTIL WS-ADGANG    > WS-ADGANG-TOM OR                        
084200                   IO-COUNT     > MAX-IO-COUNT                            
084300         MOVE    WS-ADGANG       TO LOC-ADGANG                            
084400                                    W-LOC-ADGANG                          
084500                                                                          
084600         PERFORM UNTIL WS-ADSEC11  > WS-ADSEC11-TOM OR                    
084700                       IO-COUNT    > MAX-IO-COUNT                         
084800             MOVE    WS-ADSEC11     TO W-LOC-ADSEC11                      
084900                                                                          
085000             PERFORM UNTIL WS-ADLEVEL11 > WS-ADLEVEL11-TOM OR             
085100                           IO-COUNT      > MAX-IO-COUNT                   
085200                 MOVE    WS-ADLEVEL11   TO W-LOC-ADLEVEL11                
085300                                                                          
085400                 PERFORM UNTIL WS-ADSEQ    > WS-ADSEQ-TOM OR              
085500                               IO-COUNT    > MAX-IO-COUNT                 
085600                                                                          
085700                     MOVE  WS-ADSEQ         TO   W-LOC-ADSEQ              
085800                     MOVE  W-LOC-ADPLATS    TO   LOC-ADPLATS              
085900                                                                          
086000                     PERFORM  IMS-GHU-LOCA                                
086100                     ADD +1                 TO IO-COUNT                   
086200                                                                          
086300                     IF SEGMENT-FOUND                                     
086400                       IF WS-KDDEL = NEJ                                  
086500                          IF MID-KDLOC NOT = ALL '+'                      
086600                             MOVE MID-KDLOC TO LOC-KDLOC                  
086700                          END-IF                                          
086800                          IF MID-KDFREQ NOT = ALL '+'                     
086900                             MOVE MID-KDFREQ TO LOC-KDFREQ                
087000                          END-IF                                          
087100                          IF MID-KDSTOR NOT = ALL '+'                     
087200                             MOVE MID-KDSTOR TO LOC-KDSTOR                
087300                          END-IF                                          
087400                          IF MID-KVMPART NOT = ALL '+'                    
087500                             MOVE MID-KVMPART TO LOC-KVMPART              
087600                          END-IF                                          
087700                          IF MID-TELOC NOT = ALL '+'                      
087800                             MOVE MID-TELOC TO LOC-TELOC                  
087900                          END-IF                                          
088000                                                                          
088100                          PERFORM IMS-REPL-LOCA-LOC                       
088200                        ELSE                                              
088300                          PERFORM IMS-DLET-LOCA-LOC                       
088400                        END-IF                                            
088500                        ADD +1     TO IO-COUNT                            
088600                     END-IF                                               
088700                     ADD +1     TO WS-ADSEQ                               
088800                 END-PERFORM                                              
088900                 IF IO-COUNT  <= MAX-IO-COUNT                             
089000                    MOVE WS-ADSEQ-FOM TO WS-ADSEQ                         
089100                    ADD +1            TO WS-ADLEVEL11                     
089200                 END-IF                                                   
089300                                                                          
089400             END-PERFORM                                                  
089500             IF IO-COUNT  <= MAX-IO-COUNT                                 
089600                MOVE WS-ADLEVEL11-FOM TO WS-ADLEVEL11                     
089700                ADD WS-SECTION-STEP   TO WS-ADSEC11                       
089800             END-IF                                                       
089900                                                                          
090000         END-PERFORM                                                      
090100         IF IO-COUNT  <= MAX-IO-COUNT                                     
090200            MOVE WS-ADSEC11-FOM       TO WS-ADSEC11                       
090300            ADD +1                    TO WS-ADGANG                        
090400         END-IF                                                           
090500     END-PERFORM                                                          
090600                                                                          
090700                                                                          
090800     .                                                                    
090900     EJECT                                                                
091000 HC-CLOSE SECTION.                                                        
091100                                                                          
091200      IF WS-ADGANG > WS-ADGANG-TOM                                        
091300                                                                          
091400         MOVE W-LOC-IDDC            TO MED-IDDC                           
091500         MOVE W-LOC-ADLAGOMR        TO MED-ADLAGOMR                       
091600         MOVE W-LOC-ADGANG          TO MED-ADGANG                         
091700         MOVE W-LOC-ADSEC11         TO MED-ADSEC11                        
091800         MOVE W-LOC-ADLEVEL11       TO MED-ADLEVEL11                      
091900         MOVE W-LOC-ADSEQ           TO MED-ADSEQ                          
092000         MOVE MED-1                 TO MOD-TEMFSFEL                       
092100                                                                          
092200         MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                       
092300         CALL WMEDKONV USING MED-WMEDAREA                                 
092400         MOVE MED-MFSINF            TO MOD-TEMFSINF                       
092500*                                                                         
092600*    -- INIT SAVE KEYS                                                    
092700*                                                                         
092800         MOVE '6345'                TO SAVE-IDTRANS                       
092900         MOVE WS-ADLAGOMR           TO SAVE-ADLAGOMR                      
093000         MOVE WS-ADGANG-FOM         TO SAVE-ADGANG                        
093100         MOVE WS-ADSEC11-FOM        TO SAVE-ADSEC11                       
093200         MOVE WS-ADLEVEL11-FOM      TO SAVE-ADLEVEL11                     
093300         MOVE WS-ADSEQ-FOM          TO SAVE-ADSEQ                         
093400                                                                          
093500         MOVE '002'                 TO MSGI-KDCALL                        
093600         MOVE SAVE-AREA             TO MSGI-SPAR-AREA                     
093700                                                                          
093800         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
093900       ELSE                                                               
094000         MOVE JA                    TO RESTART-SW                         
094100         MOVE MID-W6I34501          TO 6345-UT-MID-W6I34501               
094200         MOVE W-LOC-ADLAGOMR        TO SAVE-ADLAGOMR                      
094300         MOVE WS-ADGANG             TO SAVE-ADGANG                        
094400         MOVE WS-ADSEC11            TO SAVE-ADSEC11                       
094500         MOVE WS-ADLEVEL11          TO SAVE-ADLEVEL11                     
094600         MOVE WS-ADSEQ              TO SAVE-ADSEQ                         
094700         MOVE '6345'                TO SAVE-IDTRANS                       
094800                                                                          
094900         MOVE '002'      TO MSGI-KDCALL                                   
095000         MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                
095100         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
095200      END-IF                                                              
095300     .                                                                    
095400     EJECT                                                                
095500 MFS-INIT-KEY-FIELD-IN SECTION.                                           
095600                                                                          
095700*    --- ALL INPUT KEY FIELDS                                             
095800                                                                          
095900     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
096000                               MOD-ADLAGOMR-IN                            
096100                               MOD-ADGANG-FOM-IN                          
096200                               MOD-ADGANG-TOM-IN                          
096300                               MOD-ADSEC11-FOM-IN                         
096400                               MOD-ADSEC11-TOM-IN                         
096500                               MOD-ADLEVEL11-FOM-IN                       
096600                               MOD-ADLEVEL11-TOM-IN                       
096700                               MOD-ADSEQ-FOM-IN                           
096800                               MOD-ADSEQ-TOM-IN                           
096900                               MOD-KDAOE-IN                               
097100     .                                                                    
097200     EJECT                                                                
097300 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
097400                                                                          
097500*    --- ALL OUTPUT KEY FIELDS                                            
097600                                                                          
097700     MOVE MSGI-IDDC         TO MOD-IDDC-UT                                
097800     MOVE MFS-ERASE-FIELD   TO MOD-ADLAGOMR-UT                            
097900                               MOD-ADGANG-FOM-UT                          
098000                               MOD-ADGANG-TOM-UT                          
098100                               MOD-ADSEC11-FOM-UT                         
098200                               MOD-ADSEC11-TOM-UT                         
098300                               MOD-ADLEVEL11-FOM-UT                       
098400                               MOD-ADLEVEL11-TOM-UT                       
098500                               MOD-ADSEQ-FOM-UT                           
098600                               MOD-ADSEQ-TOM-UT                           
098700                               MOD-KDAOE-UT                               
098900     .                                                                    
099000     EJECT                                                                
099100 MFS-ERASE-FIELD-IN SECTION.                                              
099200                                                                          
099300*    --- ALL INPUT DATA FIELDS                                            
099400     MOVE MFS-ERASE-FIELD    TO MOD-KDLOC                                 
099500                                MOD-KDDEL                                 
099510                                MOD-KDFREQ                                
099600                                MOD-KDSTOR                                
099700                                MOD-TELOC                                 
099800                                MOD-KVMPART                               
099900     .                                                                    
100000     EJECT                                                                
100100 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
100200                                                                          
100300*    --- ALL OUTPUT DATA FIELDS                                           
100400     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
100410                                    MOD-KDDEL                             
100500                                    MOD-KDFREQ                            
100600                                    MOD-KDSTOR                            
100700                                    MOD-TELOC                             
100800                                    MOD-KVMPART                           
100810                                    MOD-KDDEL                             
100900     .                                                                    
101000     SKIP3                                                                
101100 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
101200                                                                          
101300*    --- ALL INPUT DATA FIELDS                                            
101400     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDLOC                             
101410                                    MOD-KDDEL                             
101500                                    MOD-KDFREQ                            
101600                                    MOD-KDSTOR                            
101700                                    MOD-TELOC                             
101800                                    MOD-KVMPART                           
101900                                    MOD-KDDEL                             
101910     .                                                                    
102000     EJECT                                                                
102100* --- IMS SECTIONS ---                                                    
102200     SKIP3                                                                
102300 IMS-GET-MSG SECTION.                                                     
102400                                                                          
102500     MOVE '  QC' TO GOOD-STATUSCODES                                      
102600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102800     PERFORM IMS-STATUSCHECK                                              
102900     .                                                                    
103000     SKIP3                                                                
103100 IMS-INSERT-MSG SECTION.                                                  
103200                                                                          
103300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103400     MOVE SPACE TO GOOD-STATUSCODES                                       
103500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103700     PERFORM IMS-STATUSCHECK                                              
103800     .                                                                    
103900     EJECT                                                                
104000 IMS-ISRT-ALT-MSG-6345  SECTION.                                          
104100     MOVE SPACE  TO GOOD-STATUSCODES                                      
104200     CALL  CBLTDLI  USING ISRT ALT6345-PCB P-TO-P-SW                      
104300     MOVE ALT6345-STATUS-CODE TO STATUS-WS                                
104400     PERFORM IMS-STATUSCHECK                                              
104500     .                                                                    
104600     SKIP3                                                                
104700 IMS-GU-6314 SECTION.                                                     
104800                                                                          
104900     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
105000          DELIMITED BY SIZE INTO SSA1                                     
105100     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
105200          DELIMITED BY SIZE INTO SSA2                                     
105300     MOVE '  GE' TO GOOD-STATUSCODES                                      
105400     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
105500     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
105600     PERFORM IMS-STATUSCHECK                                              
105700     .                                                                    
105800     SKIP3                                                                
105900 IMS-GU-6316 SECTION.                                                     
106000                                                                          
106100     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
106200          DELIMITED BY SIZE INTO SSA1                                     
106300     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
106400          DELIMITED BY SIZE INTO SSA2                                     
106500     MOVE '  GE' TO GOOD-STATUSCODES                                      
106600     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
106700     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSCHECK                                              
106900     .                                                                    
107000     SKIP3                                                                
107100 IMS-GHU-LOCA SECTION.                                                    
107200                                                                          
107300     STRING 'WLLOCA01(WDJ801KY =' W-WDJ8KEY-X ')'                         
107400          DELIMITED BY SIZE INTO SSA1                                     
107500     MOVE '  GE' TO GOOD-STATUSCODES                                      
107600     CALL CBLTDLI USING GHU LOCA-PCB LOC-WDJ801 SSA1                      
107700     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSCHECK                                              
107900     .                                                                    
108000     SKIP3                                                                
108100 IMS-REPL-LOCA-LOC SECTION.                                               
108200                                                                          
108300     MOVE '  ' TO GOOD-STATUSCODES                                        
108400     CALL CBLTDLI USING REPL LOCA-PCB LOC-WDJ801                          
108500     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSCHECK                                              
108700     .                                                                    
108800     SKIP3                                                                
108900 IMS-DLET-LOCA-LOC SECTION.                                               
109000                                                                          
109100     MOVE '  ' TO GOOD-STATUSCODES                                        
109200     CALL CBLTDLI USING DLET LOCA-PCB LOC-WDJ801                          
109300     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
109400     PERFORM IMS-STATUSCHECK                                              
109500     .                                                                    
109600     SKIP3                                                                
109700 IMS-STATUSCHECK SECTION.                                                 
109800                                                                          
109900     SET STATUS-IX TO 1                                                   
110000     SEARCH GOOD-STATUS                                                   
110100       AT END                                                             
110200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
110300         DELIMITED BY SIZE INTO ERROR-TEXT                                
110400         CALL FELLOG                                                      
110500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
110600         CONTINUE                                                         
110700     END-SEARCH                                                           
110800     .                                                                    
