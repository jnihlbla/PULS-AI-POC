001100**********************************************************                
001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5013500.                                                
001400 AUTHOR.         BARSHARANI BISHOYE.                                      
001500 DATE-WRITTEN.   21/11/08.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    FUNCTION:                                                            
001900*        SCREEN FOR SUPPLIER VAT REGD NO                                  
001910*        THE PROGRAM READS     WDF1                                       
002000*        THE PROGRAM UPDATES   WDF1                                       
002200*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: W5T135                                              
002600*        MID:         W5I13501                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        MOD:         W5O13501                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W5013500'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004411 77  WS-IDLEVNR                  PIC X(5)   VALUE SPACE.                  
004420 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004600                                                                          
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005001                                                                          
005010 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005020     88  INDATA-OK                           VALUE 'J'.                   
005030     88  INDATA-WRONG                        VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  OWN-MID                             VALUE '5135'.                
005400     88  GOOD-MID                            VALUE '5131' '5132'          
005500                                                   '5133' '5134'          
005600                                                   '5135' '5136'          
005700                                                   '5137' '5138'          
005800                                                   '5139'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000     EJECT                                                                
006010*01  -COPY WWDCKONS                                                       
006020*                                                                         
006100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     EJECT                                                                
006800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006900*01 -COPY WMEDAREA                                                        
007000     SKIP3                                                                
007100 01  MESSAGE-CODES.                                                       
007200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007410     03  ERR-NOT-AUTH            PIC X(3)    VALUE '405'.                 
007500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007700     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
007710     03  ERR-SUPP-MISSING        PIC X(3)    VALUE '273'.                 
007800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007910     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008001     03  ERR-SUPP-NOT-UPD-DC     PIC X(3)    VALUE '437'.                 
008010     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
008100     EJECT                                                                
008200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008900*                                                                         
009000 01  SAVE-AREA.                                                           
009100     03  SAVE-IDTRANS           PIC X(4)    VALUE '5135'.                 
009200     EJECT                                                                
009300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W5I13501                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000     SKIP3                                                                
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W5O13501                                                 
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700     SKIP3                                                                
010800*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
010910 01  FILLER             PIC X(16) VALUE 'WWDC99       '.                  
010920*01   -COPY WWDC99.                                                       
010930     EJECT                                                                
010940 01  FILLER             PIC X(16) VALUE 'DC-WWDC99    '.                  
010950*01   -COPY WWDC99   -PRE DC-                                             
010960     EJECT                                                                
010970 01  FILLER             PIC X(16) VALUE 'WWIDFTG      '.                  
010980*01 -COPY WWIDFTG                                                         
010991     EJECT                                                                
011000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  KEYS-FOR-DLI.                                                        
011500     03  W-IDLEVNR-X.                                                     
011600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011601     03  W-IDDC-X.                                                        
011602         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011610     03  W-IDLEVSUF-X.                                                    
011620         05  W-IDLEVSUF          PIC S9(2)   VALUE ZERO COMP-3.           
011700     SKIP2                                                                
011800*    --- STATUS CODES FROM IMS                                            
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400                                                                          
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
013600 01  DLI-IO-WDF101.                                                       
013700*    03  -COPY WDF101                                                     
013800     EJECT                                                                
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
014000 01  DLI-IO-WDF106.                                                       
014100*    03  -COPY WDF106                                                     
014200     EJECT                                                                
014210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
014220 01  DLI-IO-WDF116.                                                       
014230*    03  -COPY WDF116 -PRE WDF1-                                          
014240     EJECT                                                                
014300                                                                          
014400 LINKAGE SECTION.                                                         
014500*01  -COPY W0009   -PRE MSG-                                              
014600     05  FILLER                  PIC X.                                   
014610*01  -COPY W0009   -PRE WDP7-                                             
014620     05  FILLER                  PIC X.                                   
014700*01  -COPY W0008  -PRE WDF1-                                              
014800     05  FILLER                  PIC X.                                   
014930     EJECT                                                                
015000                                                                          
015100 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDF1-PCB .                    
015200 MAIN SECTION.                                                            
015300     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDF1-PCB .                    
015400                                                                          
015500     PERFORM IMS-GET-MSG                                                  
015600     IF SEGMENT-FOUND                                                     
015700       PERFORM A-INIT                                                     
015800       PERFORM B-CHECK-KEYS                                               
015900       IF KEYS-OK                                                         
015910         IF MFS-UPDATE                                                    
015920           PERFORM G-CHECK-INPUT                                          
015930           IF INDATA-OK                                                   
015940             PERFORM H-UPPDATE                                            
015950           END-IF                                                         
015960         ELSE                                                             
015970           IF MFS-FIRST                                                   
015980             PERFORM C-FIRST-PAGE                                         
015990           ELSE                                                           
015991               PERFORM E-SAME-PAGE                                        
015992           END-IF                                                         
015993         END-IF                                                           
015994         PERFORM F-READ-SHOW-INFO                                         
015995       END-IF                                                             
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13501 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF MSG-DOUBLE-TRANSACTIONS                                           
017300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I13501                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I13501                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W5O135N1' TO MFS-IDMOD                                         
018800     MOVE '5135' TO MOD-IDTRANS                                           
018900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019100     IF OWN-MID OR HELP-MID                                               
019200       CONTINUE                                                           
019300     ELSE                                                                 
019400       MOVE SPACE TO MFS-KDTRTYP                                          
019500       MOVE '7' TO MFS-IDPFK                                              
019600     END-IF                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 B-CHECK-KEYS SECTION.                                                    
021695                                                                          
021696     MOVE ALL '+'                TO MSGI-WMSGINIT                         
021697     MOVE '001'                  TO MSGI-KDCALL                           
021698     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
021699     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
021700     MOVE '5135'                 TO MSGI-IDTRANS                          
021701     IF GOOD-MID                                                          
021702       MOVE MID-IDLEVNR-IN       TO MSGI-IDLEVNR                          
021703       MOVE MID-IDDC-IN          TO MSGI-IDDC-KEY                         
021706     ELSE                                                                 
021707       MOVE ALL '+'              TO MID-IDDC-IN                           
021708                                    MID-IDLEVNR-IN                        
021709     END-IF                                                               
021710     CALL W005INIT            USING MSGI-WMSGINIT                         
021711                                    WDP7-PCB                              
021712                                                                          
021715     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
021716     MOVE YES                    TO KEYS-SW                               
021717                                                                          
021718     MOVE MFS-ERASE-FIELD        TO MOD-IDLEVNR-IN                        
021719                                                                          
021720     IF MID-IDLEVNR-IN = ALL '+'                                          
021721       MOVE MSGI-IDLEVNR         TO WS-IDLEVNR                            
021724     ELSE                                                                 
021725       MOVE MID-IDLEVNR-IN       TO WS-IDLEVNR                            
021726       MOVE '7'                  TO MFS-IDPFK                             
021727       MOVE SPACE                TO MFS-KDTRTYP                           
021728     END-IF                                                               
021729                                                                          
021730     IF WS-IDLEVNR NOT = SPACE                                            
021731       MOVE WS-IDLEVNR           TO W-IDLEVNR                             
021732     ELSE                                                                 
021733       MOVE NOO                  TO KEYS-SW                               
021734       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
021735     END-IF                                                               
021736                                                                          
021737     MOVE MFS-ERASE-FIELD        TO MOD-IDDC-IN                           
021738                                                                          
021739     IF MID-IDDC-IN = ALL '+'                                             
021740       MOVE MSGI-IDDC-KEY        TO WS-IDDC                               
021741       IF CDC-SE OR NDC-CN OR NDC-US                                      
021742          CONTINUE                                                        
021743       ELSE                                                               
021744          MOVE WC-CDC-SE         TO WS-IDDC                               
021745       END-IF                                                             
021746     ELSE                                                                 
021747       MOVE MID-IDDC-IN          TO WS-IDDC                               
021748       MOVE '7'                  TO MFS-IDPFK                             
021749       MOVE SPACE                TO MFS-KDTRTYP                           
021750     END-IF                                                               
021751                                                                          
021752     IF WS-IDDC = SPACE                                                   
021753       MOVE MSGI-IDDC            TO WS-IDDC                               
021754     END-IF                                                               
021755                                                                          
021756     IF CDC-SE OR NDC-CN OR NDC-US                                        
021757       MOVE WS-IDDC              TO W-IDDC                                
021758     ELSE                                                                 
021759       MOVE NOO                  TO KEYS-SW                               
021760       MOVE ERR-WRONG-DC         TO MED-IDMFSFEL                          
021761     END-IF                                                               
021762                                                                          
021763     IF GOOD-MID   OR KEYS-OK                                             
021764       MOVE WS-IDLEVNR           TO MOD-IDLEVNR-UT                        
021765       INSPECT MOD-IDLEVNR-UT    REPLACING LEADING ZERO BY SPACE          
021767       MOVE WS-IDDC              TO MOD-IDDC-UT                           
021768     ELSE                                                                 
021769       IF KEYS-WRONG                                                      
021770         MOVE WS-IDLEVNR         TO MOD-IDLEVNR-UT                        
021771         INSPECT MOD-IDLEVNR-UT  REPLACING LEADING ZERO BY SPACE          
021772         MOVE WS-IDDC            TO MOD-IDDC-UT                           
021773       ELSE                                                               
021774         MOVE MFS-ERASE-FIELD    TO MOD-IDLEVNR-UT                        
021775                                    MOD-IDDC-UT                           
021776       END-IF                                                             
021777     END-IF                                                               
021778                                                                          
021779     IF KEYS-WRONG                                                        
021800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021900       CALL WMEDKONV USING MED-WMEDAREA                                   
022000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022100       PERFORM MFS-ERASE-FIELD-IN                                         
022200       PERFORM MFS-ERASE-FIELD-OUT                                        
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 C-FIRST-PAGE SECTION.                                                    
022700                                                                          
022800     PERFORM MFS-ERASE-FIELD-IN                                           
022900     .                                                                    
023000     EJECT                                                                
023100                                                                          
023200 E-SAME-PAGE SECTION.                                                     
023300                                                                          
023400     IF OWN-MID OR HELP-MID                                               
023500       IF  MID-IDVAT-IN = ALL '+'                                         
023600         PERFORM MFS-ERASE-FIELD-IN                                       
023700       ELSE                                                               
023800         MOVE INF-PRESS-PF11            TO MED-IDMFSINF                   
023900         CALL WMEDKONV USING MED-WMEDAREA                                 
024000         MOVE MED-MFSINF                TO MOD-TEMFSFEL                   
024100         PERFORM EA-MID-INDATA-FOR-MOD                                    
024200       END-IF                                                             
024300     ELSE                                                                 
024400      PERFORM MFS-ERASE-FIELD-IN                                          
024500     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 EA-MID-INDATA-FOR-MOD SECTION.                                           
024900     IF MID-IDVAT-IN        =  ALL '+'                                    
025000       MOVE MFS-ERASE-FIELD              TO MOD-IDVAT-IN                  
025100     ELSE                                                                 
025200       MOVE MFS-DO-NOT-TOUCH-FIELD       TO MOD-IDVAT-IN                  
025300     END-IF                                                               
025400                                                                          
025500     .                                                                    
025600     EJECT                                                                
025700 F-READ-SHOW-INFO SECTION.                                                
025800                                                                          
025810      PERFORM IMS-GU-WDF101                                               
025820      IF SEGMENT-MISSING                                                  
025830        MOVE ERR-SUPP-MISSING   TO MED-IDMFSFEL                           
025840        CALL WMEDKONV        USING MED-WMEDAREA                           
025850        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
025860        PERFORM MFS-ERASE-FIELD-OUT                                       
025870      ELSE                                                                
026000        IF SEGMENT-FOUND                                                  
026100          PERFORM IMS-GNP-WDF106                                          
026200          IF SEGMENT-FOUND                                                
026300            MOVE ADR-IDVAT  TO MOD-IDVAT                                  
026400          ELSE                                                            
026500            CALL WMEDKONV USING MED-WMEDAREA                              
026600            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
026700            PERFORM MFS-ERASE-FIELD-OUT                                   
026800          END-IF                                                          
026900        ELSE                                                              
027000           CALL WMEDKONV USING MED-WMEDAREA                               
027100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
027200           PERFORM MFS-ERASE-FIELD-OUT                                    
027400        END-IF                                                            
027410      END-IF                                                              
027420      IF CDC-SE                                                           
027430        CONTINUE                                                          
027440      ELSE                                                                
027450        PERFORM IMS-GU-WDF116                                             
027451        IF SEGMENT-FOUND                                                  
027452           CONTINUE                                                       
027453        ELSE                                                              
027460          MOVE ERR-SUPP-NOT-UPD-DC                                        
027470                                   TO MED-IDMFSFEL                        
027480            CALL WMEDKONV      USING MED-WMEDAREA                         
027490            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
027491            PERFORM MFS-ERASE-FIELD-IN                                    
027492            PERFORM MFS-ERASE-FIELD-OUT                                   
027493        END-IF                                                            
027494      END-IF                                                              
027500     .                                                                    
027600     EJECT                                                                
027700                                                                          
027800 G-CHECK-INPUT SECTION.                                                   
027907                                                                          
027912     MOVE YES                    TO INDATA-SW                             
027913                                                                          
027923     IF MID-IDVAT-IN = ALL '+'                                            
027924       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
027925       CALL WMEDKONV          USING MED-WMEDAREA                          
027926       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
027927       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
027928       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
027929       MOVE NOO                  TO INDATA-SW                             
027930     ELSE                                                                 
027931       PERFORM GA-CHECK-INPUT-FIELDS                                      
027932       IF INDATA-WRONG                                                    
027933         MOVE ERR-CORR-HILITE-FLDS         TO MED-IDMFSFEL                
027934         CALL WMEDKONV USING MED-WMEDAREA                                 
027935         MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                
027936         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
027937         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
027938       ELSE                                                               
027939         PERFORM GB-CHECK-DATABASES                                       
027940         IF INDATA-WRONG                                                  
027941           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
027942           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
027943         END-IF                                                           
027944       END-IF                                                             
027945     END-IF                                                               
027946     .                                                                    
027947     EJECT                                                                
027948 GA-CHECK-INPUT-FIELDS SECTION.                                           
027949*VAT REGD NO                                                              
027950                                                                          
027951     IF MID-IDVAT-IN             NOT =  ALL '+'                           
027952       IF MID-IDVAT-IN               =  SPACE                             
027953         MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-IDVAT-IN-ATTR              
027954         MOVE NOO                       TO INDATA-SW                      
027955       ELSE                                                               
027956         MOVE MFS-ALPHA-FIELD-OK        TO MOD-IDVAT-IN-ATTR              
027957       END-IF                                                             
027958     ELSE                                                                 
027959       MOVE MFS-ALPHA-FIELD-OK          TO MOD-IDVAT-IN-ATTR              
027960     END-IF                                                               
027961     .                                                                    
027962     EJECT                                                                
027963 GB-CHECK-DATABASES  SECTION.                                             
027964                                                                          
027965     IF INDATA-OK                                                         
027967       PERFORM IMS-GU-WDF101                                              
027971       IF SEGMENT-MISSING                                                 
027972         MOVE ERR-KEYS-MISSING                                            
027973                                 TO MED-IDMFSFEL                          
027974         MOVE NOO                TO INDATA-SW                             
027975         CALL WMEDKONV        USING MED-WMEDAREA                          
027976         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
027977         PERFORM MFS-ERASE-FIELD-OUT                                      
027978       END-IF                                                             
027980     ELSE                                                                 
029200       IF MID-IDVAT-IN NOT = ALL '+'                                      
029210         CONTINUE                                                         
029900       ELSE                                                               
030000         MOVE MFS-DO-NOT-TOUCH-FIELD        TO MOD-IDVAT-IN-ATTR          
030100       END-IF                                                             
031300     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032340 H-UPPDATE SECTION.                                                       
032400                                                                          
032500     IF MID-IDLEVNR-IN           NOT =  ALL '+'                           
032501     OR MID-IDDC-IN              NOT =  ALL '+'                           
032510     OR MID-IDVAT-IN             NOT =  ALL '+'                           
032600       PERFORM HA-UPDATE-CHK                                              
033300     END-IF                                                               
033400                                                                          
033500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
033510     MOVE 'GB '           TO MED-IDSKYLT                                  
033600     CALL WMEDKONV USING MED-WMEDAREA                                     
033700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
033800     PERFORM MFS-FORM-ATTR                                                
033900     PERFORM MFS-ERASE-FIELD-IN                                           
034000     .                                                                    
034100     EJECT                                                                
034200 HA-UPDATE-CHK SECTION.                                                   
034300     IF MID-IDVAT-IN NOT = ALL '+'                                        
034310       PERFORM IMS-GHU-WDF106                                             
034320       IF SEGMENT-FOUND                                                   
034330         IF MID-IDVAT-IN         NOT =  ALL '+'                           
034340           MOVE MID-IDVAT-IN           TO ADR-IDVAT                       
034360         END-IF                                                           
034391         PERFORM IMS-REPL-WDF106                                          
034400       END-IF                                                             
034500     END-IF                                                               
034700                                                                          
034800     .                                                                    
034810     EJECT                                                                
034900                                                                          
035000 MFS-ERASE-FIELD-OUT SECTION.                                             
035100                                                                          
035200*    --- ALLA UTDATA-FÄLT                                                 
035300     MOVE MFS-ERASE-FIELD TO MOD-IDLEVNR-IN                               
035400                             MOD-IDDC-IN                                  
035500                             MOD-IDVAT                                    
035600     .                                                                    
035700     SKIP3                                                                
035800 MFS-ERASE-FIELD-IN SECTION.                                              
035900                                                                          
036000*    --- ALLA INDATA-FÄLT                                                 
036100     MOVE MFS-ERASE-FIELD TO MOD-IDLEVNR-IN                               
036200                             MOD-IDDC-IN                                  
036300                             MOD-IDVAT-IN                                 
036400     .                                                                    
036500     EJECT                                                                
036600 MFS-DONT-TOUCH-FIELD-OUT SECTION.                                        
036700                                                                          
036800*    --- ALLA UTDATA-FÄLT                                                 
036900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDVAT                             
037000     .                                                                    
037100     SKIP3                                                                
037200 MFS-DONT-TOUCH-FIELD-IN SECTION.                                         
037300                                                                          
037400*    --- ALLA INDATA-FÄLT                                                 
037500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDLEVNR-IN                        
037600                                    MOD-IDDC-IN                           
037700                                    MOD-IDVAT-IN                          
037800     .                                                                    
037900     EJECT                                                                
038000 MFS-FORM-ATTR SECTION.                                                   
038100                                                                          
038200*    --- ALL INDATA-FIELDS                                                
038300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDVAT-IN-ATTR                    
038400     .                                                                    
038500     SKIP2                                                                
038600 MFS-READ-IN-AGAIN SECTION.                                               
038700                                                                          
038800*    --- ALL INDATA-FIELDS                                                
038900     MOVE MFS-ADD-READ-FIELD TO MOD-IDVAT-IN-ATTR                         
039000     .                                                                    
039100     EJECT                                                                
039200* --- IMS SECTIONS ---                                                    
039300     SKIP3                                                                
039400 IMS-GET-MSG SECTION.                                                     
039500                                                                          
039600     MOVE '  QC' TO GOOD-STATUSCODES                                      
039700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039900     PERFORM IMS-STATUSCHECK                                              
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-INSERT-MSG SECTION.                                                  
040300                                                                          
040700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040800     MOVE SPACE TO GOOD-STATUSCODES                                       
040900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041100     PERFORM IMS-STATUSCHECK                                              
041200     .                                                                    
041300     EJECT                                                                
041400 IMS-GU-WDF101 SECTION.                                                   
041500     SKIP2                                                                
041600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
041700          DELIMITED BY SIZE INTO SSA1                                     
041800     MOVE '  GE' TO GOOD-STATUSCODES                                      
041900     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
042000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
042100     PERFORM IMS-STATUSCHECK                                              
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-GNP-WDF106 SECTION.                                                  
042500     SKIP2                                                                
042600     MOVE 'WDF106   ' TO SSA1                                             
042700     MOVE '  GE' TO GOOD-STATUSCODES                                      
042800     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
042900     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
043000     PERFORM IMS-STATUSCHECK                                              
043100     .                                                                    
043200     EJECT                                                                
045150 IMS-GHU-WDF106  SECTION.                                                 
045151                                                                          
045152     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
045153          DELIMITED BY SIZE INTO SSA1                                     
045154     MOVE 'WDF106             '  TO SSA2                                  
045156     MOVE '  GE'           TO GOOD-STATUSCODES                            
045157     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF106 SSA1 SSA2              
045158     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
045159     PERFORM IMS-STATUSCHECK                                              
045160     .                                                                    
045161     EJECT                                                                
045162 IMS-REPL-WDF106 SECTION.                                                 
045163                                                                          
045164     MOVE '  ' TO GOOD-STATUSCODES                                        
045165     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF106                       
045166     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
045167     PERFORM IMS-STATUSCHECK                                              
045168     .                                                                    
045169     EJECT                                                                
045170 IMS-GU-WDF116 SECTION.                                                   
045171                                                                          
045172     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
045180             DELIMITED BY SIZE INTO SSA1                                  
045190     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
045191             DELIMITED BY SIZE INTO SSA2                                  
045192     MOVE '  GE'                 TO GOOD-STATUSCODES                      
045193     CALL CBLTDLI             USING GU                                    
045194                                    WDF1-PCB                              
045195                                    DLI-IO-WDF116                         
045196                                    SSA1 SSA2                             
045197     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
045198     PERFORM IMS-STATUSCHECK                                              
045199     .                                                                    
045200                                                                          
045220 IMS-STATUSCHECK SECTION.                                                 
045300                                                                          
045400     SET STATUS-IX TO 1                                                   
045500     SEARCH GOOD-STATUS                                                   
045600       AT END                                                             
045700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
045800         DELIMITED BY SIZE INTO ERROR-TEXT                                
045900         CALL FELLOG                                                      
046000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
046100         CONTINUE                                                         
046200     END-SEARCH                                                           
046300     .                                                                    
