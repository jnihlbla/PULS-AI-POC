000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5012500.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   18/05/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SCREEN 5125 DISPLAYS ORDER PRICE                                 
000900*                                                                         
001000*        PROGRAM READS WDK7                                               
001100*                      WDK6                                               
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W5T125                                              
001500*        MID:         W5I12501                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W5O12501                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W5012500'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X      VALUE 'J'.                    
003200 77  NOO                         PIC X      VALUE 'N'.                    
003300                                                                          
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
003600 77  WDK7-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
003700 77  WDK7-MAX-INDX               PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
003810 77  MAX-INDX2                   PIC S9(4)  VALUE +18   COMP SYNC.        
003900 77  MAX-INDX-INT                PIC S9(4)  VALUE +28   COMP SYNC.        
004000 77  WS-DAPRLIST                 PIC 9(8).                                
004010 77  WS-TIREGDAT                 PIC 9(7).                                
004100 77  W-GE-WDK711                 PIC X(1)   VALUE 'N'.                    
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300 77  PRICE-FOUND-SW              PIC X      VALUE 'N'.                    
004400     88  PRICE-FOUND                        VALUE 'J'.                    
004500     88  PRICE-MISSING                      VALUE 'N'.                    
004600                                                                          
004700 77  PART-MISSING-SW             PIC X      VALUE 'J'.                    
004800     88  PART-MISSING                       VALUE 'J'.                    
004900     88  PART-FOUND                         VALUE 'N'.                    
005000                                                                          
005100 77  KEYS-SW                     PIC X      VALUE 'J'.                    
005200     88  KEYS-OK                            VALUE 'J'.                    
005300     88  KEYS-WRONG                         VALUE 'N'.                    
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)   VALUE SPACE.                  
005600     88  OWN-MID                            VALUE '5125'.                 
005700     88  GOOD-MID                           VALUE '5121' '5122'           
005800                                                  '5123' '5124'           
005900                                                  '5125' '5126'           
006000                                                  '5127' '5128'           
006100                                                  '5129'.                 
006200     88  HELP-MID                           VALUE '0551'.                 
006300 01  WS-INDX-I        PIC 9(4) COMP.                                      
006400 01  WS-INDX-J        PIC 9(4) COMP.                                      
006500 01  WS-INDX-K        PIC 9(4) COMP.                                      
006600 01  TABLE-LENGTH     PIC 9(4) COMP VALUE 27.                             
006700 01  W-OUTPUT-TABLE.                                                      
006800     03 W-TABELLRAD        OCCURS 28 TIMES.                               
006900        05 WW-KDPRURSP      PIC X.                                        
007000        05 WW-IDDC          PIC X(2).                                     
007100        05 WW-DAPRLIST      PIC 9(8).                                     
007200        05 WW-IDLEVNR       PIC X(5).                                     
007300        05 WW-PRARTBEL-PR   PIC Z(7)9.9(5).                               
007400        05 WW-KDVALISO      PIC X(3).                                     
007500        05 WW-KDSTATUS      PIC X(5).                                     
007510        05 WW-DAREGDAT      PIC 9(8).                                     
007520        05 WW-IDUSER        PIC X(8).                                     
007600 01  TABLE-SORT             PIC X(55).                                    
007700                                                                          
007800 01  W-DAPRLIST                  PIC 9(8)    VALUE ZERO.                  
007900 01  FILLER REDEFINES W-DAPRLIST.                                         
008000     03  W-DAPRLIST-SS           PIC 9(2).                                
008100     03  W-DAPRLIST-AAMMDD       PIC 9(6).                                
008200 01  VARIABLER.                                                           
008500     03  W-DAPRLIST-9KOMPL       PIC 9(8)   VALUE ZERO.                   
008600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008700 01  GENERAL-SUBPROGRAMS.                                                 
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     EJECT                                                                
009300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009400*01 -COPY WMEDAREA                                                        
009500     SKIP3                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  INF-PRICE-MISSING       PIC X(3)    VALUE '301'.                 
009800     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
009900     03  ERR-WRONG-INPUT         PIC X(3)    VALUE '020'.                 
010200     EJECT                                                                
010300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010600     SKIP3                                                                
010700*01 -COPY WMSGINIT                                                        
010800     EJECT                                                                
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W5I12501                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W5O12501                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*01  -COPY WWDCKONS                                                       
012700     EJECT                                                                
012800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  KEYS-FOR-DLI.                                                        
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.         
013500     03  W-IDDC-X.                                                        
013600         05  W-IDDC                PIC X(2)    VALUE SPACES.              
013700     03  W-WDK621KY-X.                                                    
013800         05  W-DAPRLIST-9K-21      PIC 9(8)    VALUE ZERO.                
013900         05  W-IDLEVNR-21          PIC X(5)    VALUE LOW-VALUE.           
014000     SKIP2                                                                
014100*    --- STATUS CODES FROM IMS                                            
014200 01  STATUS-WS                   PIC XX.                                  
014300     88  SEGMENT-FOUND                       VALUE '  '.                  
014400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014600     SKIP2                                                                
014700 01  GOOD-STATUSCODES.                                                    
014800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014900     SKIP3                                                                
015000 01  SSA1                        PIC X(64).                               
015100 01  SSA2                        PIC X(64).                               
015200 01  SSA3                        PIC X(64).                               
015300     EJECT                                                                
015400*    --- IMS FUNCTION CODES                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
015900 01  DLI-IO-WDK601.                                                       
016000*    03  -COPY WDK601                                                     
016100     EJECT                                                                
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016300 01  DLI-IO-WDK611.                                                       
016400*    03  -COPY WDK611                                                     
016500     EJECT                                                                
016600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK621'.                      
016700 01  DLI-IO-WDK621.                                                       
016800*    03  -COPY WDK621                                                     
016900     EJECT                                                                
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
017100 01  DLI-IO-WDK701.                                                       
017200*    03  -COPY WDK701                                                     
017300     EJECT                                                                
017400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
017500 01  DLI-IO-WDK711.                                                       
017600*    03  -COPY WDK711                                                     
017700     EJECT                                                                
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
017900 01  DLI-IO-WDK724.                                                       
018000*    03  -COPY WDK724                                                     
018100                                                                          
018200 LINKAGE SECTION.                                                         
018300*01  -COPY W0009   -PRE MSG-                                              
018400*01  -COPY W0008   -PRE WDP7-                                             
018500     05  FILLER                  PIC X.                                   
018600                                                                          
018700*01  -COPY W0008  -PRE WDK7-                                              
018710     05  KFB-IDARTNR             PIC S9(9) COMP-3.                        
018720     05  KFB-IDDC                PIC X(2).                                
018900                                                                          
019000*01  -COPY W0008  -PRE WDK6-                                              
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK7-PCB WDK6-PCB.            
019400 MAIN SECTION.                                                            
019500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK7-PCB WDK6-PCB.            
019600                                                                          
019700     PERFORM IMS-GET-MSG                                                  
019800     IF SEGMENT-FOUND                                                     
019900       PERFORM A-INIT                                                     
020000       PERFORM B-CHECK-KEYS                                               
020100       IF KEYS-OK                                                         
020200         PERFORM F-READ-SHOW-INFO                                         
020300       END-IF                                                             
020400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
020500*                                                                         
020600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O12501 + 4                      
020700       PERFORM IMS-INSERT-MSG                                             
020800     END-IF                                                               
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500     IF MSG-DOUBLE-TRANSACTIONS                                           
021600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I12501                 
021700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
021800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
021900     ELSE                                                                 
022000       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W5I12501                 
022100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
022200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
022300     END-IF                                                               
022400                                                                          
022500     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
022700     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
022800                                                                          
022900     MOVE LOW-VALUE                       TO MSG-AREA                     
023000     MOVE 'W5O125N1'                      TO MFS-IDMOD                    
023100     MOVE '5125'                          TO MOD-IDTRANS                  
023200     MOVE MFS-ERASE-FIELD                 TO MOD-IDARTNR-IN               
023300                                             MOD-TEMFSFEL                 
023301                                             MOD-TEMFSINF                 
023310     MOVE 'GB'                            TO MED-IDSKYLT                  
024100     .                                                                    
024200     EJECT                                                                
024300 B-CHECK-KEYS SECTION.                                                    
024400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024500     MOVE '001'             TO MSGI-KDCALL                                
024600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024800     MOVE '5125'            TO MSGI-IDTRANS                               
024900     IF GOOD-MID                                                          
025000       MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                            
025100     ELSE                                                                 
025200       MOVE MSGI-IDARTNR       TO MID-IDARTNR-IN                          
025300     END-IF                                                               
025400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025500                                                                          
025600*    - LANGUAGE TO BE USED BY MEDKONV                                     
025700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
025800                                                                          
025900     MOVE YES TO KEYS-SW                                                  
026000                                                                          
026100     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
026200                                                                          
026700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
026800     IF MSGI-IDARTNR NUMERIC                                              
026900       MOVE MSGI-IDARTNR  TO W-IDARTNR                                    
027000     ELSE                                                                 
027100       MOVE NOO           TO KEYS-SW                                      
027200     END-IF                                                               
027300                                                                          
027400     IF KEYS-OK                                                           
027500       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
027600     ELSE                                                                 
027700       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                             
027800     END-IF                                                               
027900                                                                          
028000     IF KEYS-WRONG                                                        
028100       MOVE ERR-WRONG-INPUT TO MED-IDMFSFEL                               
028200       CALL WMEDKONV USING MED-WMEDAREA                                   
028300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028400       PERFORM MFS-ERASE-FIELD-IN                                         
028500       PERFORM MFS-ERASE-FIELD-UT                                         
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 F-READ-SHOW-INFO SECTION.                                                
029000     PERFORM FA-READ-SHOW-WDK6                                            
029100     PERFORM FB-READ-SHOW-WDK7                                            
029200     PERFORM FC-MOVE-ARRAY-DATA                                           
029300                                                                          
029400     .                                                                    
029500     EJECT                                                                
029600 FA-READ-SHOW-WDK6 SECTION.                                               
029700     PERFORM IMS-GU-WDK611                                                
029800                                                                          
029900     IF SEGMENT-MISSING                                                   
030000       MOVE YES TO PART-MISSING-SW                                        
030100       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
030200       CALL WMEDKONV USING MED-WMEDAREA                                   
030300       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
030400       PERFORM MFS-ERASE-FIELD-UT                                         
030500     ELSE                                                                 
030600       MOVE NOO TO PART-MISSING-SW                                        
030700       MOVE W-DAPRLIST-9KOMPL     TO W-DAPRLIST-9K-21                     
030800                                                                          
030900       MOVE +1 TO INDX                                                    
031000       PERFORM IMS-GNP-WDK621                                             
031100          PERFORM UNTIL INDX > MAX-INDX2 OR SEGMENT-MISSING               
031200           IF SEGMENT-FOUND                                               
031300            IF PRL-FLHUVLEV = 'J'                                         
031400               MOVE WC-CDC-SE     TO WW-IDDC(INDX)                        
031500            ELSE                                                          
031510               EVALUATE PRL-KDVALISO                                      
031511                 WHEN 'SEK'                                               
031512                   MOVE WC-CDC-SE     TO WW-IDDC(INDX)                    
031513                 WHEN 'CNY'                                               
031514                   MOVE WC-NDC-CN-71  TO WW-IDDC(INDX)                    
031515                 WHEN 'AUD'                                               
031516                   MOVE WC-NDC-AU     TO WW-IDDC(INDX)                    
031517                 WHEN 'JPY'                                               
031518                   MOVE WC-NDC-JP-61  TO WW-IDDC(INDX)                    
031519                 WHEN 'CAD'                                               
031520                   MOVE WC-NDC-CA     TO WW-IDDC(INDX)                    
031521                 WHEN 'USD'                                               
031522                   MOVE WC-NDC-US-RU  TO WW-IDDC(INDX)                    
031523                 WHEN 'INR'                                               
031524                   MOVE WC-NDC-IN     TO WW-IDDC(INDX)                    
031525                 WHEN 'KRW'                                               
031526                   MOVE WC-NDC-KR     TO WW-IDDC(INDX)                    
031527**** SAME CURRENCY AS US, CAN'T HAVE THAT WITH                            
031528**** THE SOLUTION WE HAVE RIGHT NOW                                       
031529*                WHEN 'USD'                                               
031530*                  MOVE WC-NDC-AE     TO WW-IDDC(INDX)                    
031531                 WHEN 'TRY'                                               
031532                   MOVE WC-NDC-TR     TO WW-IDDC(INDX)                    
031533                 WHEN 'MYR'                                               
031534                   MOVE WC-NDC-MY     TO WW-IDDC(INDX)                    
031535                 WHEN 'THB'                                               
031536                   MOVE WC-NDC-TH     TO WW-IDDC(INDX)                    
031537                 WHEN 'TWD'                                               
031538                   MOVE WC-NDC-TW     TO WW-IDDC(INDX)                    
031539                 WHEN 'MXN'                                               
031540                   MOVE WC-NDC-MX     TO WW-IDDC(INDX)                    
031541                 WHEN 'BRL'                                               
031542                   MOVE WC-NDC-BR     TO WW-IDDC(INDX)                    
031543                 WHEN 'ZAR'                                               
031544                   MOVE WC-NDC-ZA     TO WW-IDDC(INDX)                    
031545                 WHEN OTHER                                               
031546                   MOVE WC-CDC-SE     TO WW-IDDC(INDX)                    
031550               END-EVALUATE                                               
031560               IF PRL-KDPRURSP > SPACES                                   
031570                  CONTINUE                                                
031580               ELSE                                                       
031590                  MOVE SPACES TO WW-IDDC(INDX)                            
031600               END-IF                                                     
032500            END-IF                                                        
032600            IF PRL-SUINLEV-PR > 0                                         
032700               MOVE 'DEL'       TO WW-KDSTATUS(INDX)                      
032800            ELSE                                                          
032900               MOVE 'EST'      TO WW-KDSTATUS(INDX)                       
033000            END-IF                                                        
033100                                                                          
033200            MOVE PRL-KDPRURSP    TO WW-KDPRURSP (INDX)                    
033300            COMPUTE WS-DAPRLIST = 99999999 -                              
033400                                  PRL-DAPRLIST-9KOMPL                     
033500            MOVE WS-DAPRLIST     TO WW-DAPRLIST (INDX)                    
033600            MOVE PRL-IDLEVNR     TO WW-IDLEVNR (INDX)                     
033700            MOVE PRL-PRARTBEL-PR TO WW-PRARTBEL-PR (INDX)                 
033800            MOVE PRL-KDVALISO    TO WW-KDVALISO (INDX)                    
033801            MOVE PRL-TIREGDAT    TO WS-TIREGDAT                           
033803            IF WS-TIREGDAT NOT = ZERO                                     
033810              MOVE '20'          TO WW-DAREGDAT(INDX) (1:2)               
033811              MOVE WS-TIREGDAT(2:6)                                       
033812                          TO WW-DAREGDAT(INDX) (3:6)                      
033813            ELSE                                                          
033814              MOVE MFS-ERASE-FIELD TO WW-DAREGDAT(INDX)                   
033815            END-IF                                                        
033820            MOVE PRL-IDUSER      TO WW-IDUSER   (INDX)                    
033900                                                                          
034000            ADD 1                TO INDX                                  
034100            PERFORM IMS-GNP-WDK621                                        
034200           ELSE                                                           
034300            MOVE MFS-ERASE-FIELD TO WW-KDPRURSP (INDX)                    
034400                                    WW-DAPRLIST (INDX)                    
034500                                    WW-IDLEVNR (INDX)                     
034600                                    WW-PRARTBEL-PR (INDX)                 
034700                                    WW-KDVALISO (INDX)                    
034800                                    WW-KDSTATUS (INDX)                    
034810                                    WW-DAREGDAT (INDX)                    
034820                                    WW-IDUSER   (INDX)                    
034900            ADD 1                TO INDX                                  
035000           END-IF                                                         
035100          END-PERFORM                                                     
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500*                                                                         
035600 FB-READ-SHOW-WDK7 SECTION.                                               
035700                                                                          
035800     PERFORM IMS-GU-WDK701                                                
035900     IF SEGMENT-FOUND                                                     
036000      MOVE NOO TO PART-MISSING-SW                                         
036100      MOVE 'N'  TO W-GE-WDK711                                            
036200      MOVE INDX TO WDK7-IX                                                
036300      COMPUTE WDK7-MAX-INDX = MAX-INDX-INT - (INDX - 1)                   
036400      PERFORM UNTIL W-GE-WDK711 = 'Y' OR WDK7-IX > WDK7-MAX-INDX          
036500       PERFORM IMS-GNP-WDK711                                             
036600       IF SEGMENT-FOUND                                                   
036700         MOVE SLAG-IDDC TO W-IDDC                                         
036800         PERFORM FBA-GET-WDK724-PRICE                                     
036900       ELSE                                                               
037000         MOVE 'Y' TO W-GE-WDK711                                          
037100       END-IF                                                             
037200      END-PERFORM                                                         
037300     ELSE                                                                 
037400       IF PART-MISSING                                                    
037500          MOVE ERR-PART-MISSING TO MED-IDMFSFEL                           
037600          CALL WMEDKONV USING MED-WMEDAREA                                
037700          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
037800          PERFORM MFS-ERASE-FIELD-UT                                      
037900       END-IF                                                             
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 FBA-GET-WDK724-PRICE SECTION.                                            
038400                                                                          
038610     PERFORM IMS-GNP-WDK724                                               
038700     PERFORM UNTIL SEGMENT-MISSING                                        
038800      IF SEGMENT-FOUND                                                    
038900         PERFORM FBAA-MOVE-PRICE-VALUES                                   
039000      END-IF                                                              
039110      PERFORM IMS-GNP-WDK724                                              
039200     END-PERFORM                                                          
039300                                                                          
039400     .                                                                    
039500     EJECT                                                                
039600*                                                                         
039700 FBAA-MOVE-PRICE-VALUES SECTION.                                          
039800     MOVE ZEROS TO W-DAPRLIST                                             
039900     IF WDK7-IX NOT > MAX-INDX-INT                                        
040000      MOVE KFB-IDDC          TO WW-IDDC(WDK7-IX)                          
040100      MOVE SPRL-IDLEVNR-PR   TO WW-IDLEVNR(WDK7-IX)                       
040200      COMPUTE W-DAPRLIST = 99999999 - SPRL-DAPRLIST-9KOMPL                
040300      MOVE W-DAPRLIST        TO WW-DAPRLIST(WDK7-IX)                      
040400      MOVE SPRL-KDPRURSP     TO WW-KDPRURSP(WDK7-IX)                      
040500      MOVE SPRL-PRARTBEL-PR  TO WW-PRARTBEL-PR(WDK7-IX)                   
040600      MOVE SPRL-KDVALISO     TO WW-KDVALISO(WDK7-IX)                      
040700      IF SPRL-SUINLEV-PR > ZERO                                           
040800        MOVE 'DEL'           TO WW-KDSTATUS(WDK7-IX)                      
040900      ELSE                                                                
041000        MOVE 'EST'           TO WW-KDSTATUS(WDK7-IX)                      
041100      END-IF                                                              
041101      MOVE SPRL-TIREGDAT    TO WS-TIREGDAT                                
041103      IF WS-TIREGDAT NOT = ZERO                                           
041110        MOVE '20'            TO WW-DAREGDAT(WDK7-IX) (1:2)                
041111        MOVE WS-TIREGDAT(2:6)                                             
041112                             TO WW-DAREGDAT(WDK7-IX) (3:6)                
041113      ELSE                                                                
041114        MOVE MFS-ERASE-FIELD TO WW-DAREGDAT(WDK7-IX)                      
041115      END-IF                                                              
041120      MOVE SPRL-IDUSER       TO WW-IDUSER  (WDK7-IX)                      
041200      ADD 1                  TO WDK7-IX                                   
041300     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 FC-MOVE-ARRAY-DATA SECTION.                                              
041700     MOVE 1 TO WS-INDX-K                                                  
041800     PERFORM FCA-SORT-ARRAY-DATA UNTIL WS-INDX-K = 0                      
041900     PERFORM FCB-MOVE-MOD-DATA                                            
042000     .                                                                    
042100     EJECT                                                                
042200 FCA-SORT-ARRAY-DATA SECTION.                                             
042300     MOVE 0 TO WS-INDX-K                                                  
042400     PERFORM VARYING WS-INDX-I FROM 1 BY 1                                
042500                               UNTIL WS-INDX-I > TABLE-LENGTH             
042600        ADD 1 WS-INDX-I GIVING WS-INDX-J                                  
042700        IF WW-DAPRLIST(WS-INDX-I) < WW-DAPRLIST(WS-INDX-J) AND            
042710           WS-INDX-J <= 28                                                
042800           MOVE W-TABELLRAD(WS-INDX-I) TO TABLE-SORT                      
042900           MOVE W-TABELLRAD(WS-INDX-J) TO W-TABELLRAD(WS-INDX-I)          
043000           MOVE TABLE-SORT     TO W-TABELLRAD(WS-INDX-J)                  
043100           MOVE 1 TO WS-INDX-K                                            
043200        END-IF                                                            
043300     END-PERFORM                                                          
043400     .                                                                    
043500     EJECT                                                                
043600 FCB-MOVE-MOD-DATA SECTION.                                               
043700     MOVE +1 TO IX                                                        
043800     PERFORM UNTIL IX > MAX-INDX                                          
043900       IF WW-DAPRLIST(IX) > ZEROES                                        
044000         MOVE YES                 TO  PRICE-FOUND-SW                      
044100         MOVE WW-IDDC(IX)         TO  MOD-IDDC(IX)                        
044200         MOVE WW-IDLEVNR(IX)      TO  MOD-IDLEVNR(IX)                     
044300         MOVE WW-DAPRLIST(IX)     TO  MOD-DAPRLIST(IX)                    
044400         MOVE WW-KDPRURSP(IX)     TO  MOD-KDPRURSP(IX)                    
044500         MOVE WW-PRARTBEL-PR(IX)  TO  MOD-PRARTBEL-PR(IX)                 
044600         MOVE WW-KDVALISO(IX)     TO  MOD-KDVALISO(IX)                    
044700         MOVE WW-KDSTATUS(IX)     TO  MOD-KDSTATUS(IX)                    
044800         MOVE WW-KDSTATUS(IX)     TO  MOD-KDSTATUS(IX)                    
044810         MOVE WW-DAREGDAT(IX)     TO  MOD-DAREGDAT(IX)                    
044820         MOVE WW-IDUSER(IX)       TO  MOD-IDUSER(IX)                      
044900       ELSE                                                               
045000         MOVE MFS-ERASE-FIELD TO MOD-KDPRURSP (IX)                        
045100                                 MOD-IDDC     (IX)                        
045200                                 MOD-DAPRLIST (IX)                        
045300                                 MOD-IDLEVNR (IX)                         
045400                                 MOD-PRARTBEL-PR (IX)                     
045500                                 MOD-KDVALISO (IX)                        
045600                                 MOD-KDSTATUS (IX)                        
045610                                 MOD-DAREGDAT(IX)                         
045620                                 MOD-IDUSER(IX)                           
045700       END-IF                                                             
045800       ADD 1 TO IX                                                        
045900     END-PERFORM                                                          
046000     IF PRICE-MISSING                                                     
046100        MOVE INF-PRICE-MISSING     TO MED-IDMFSFEL                        
046200        CALL WMEDKONV USING MED-WMEDAREA                                  
046300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
046400        PERFORM MFS-ERASE-FIELD-UT                                        
046500     END-IF                                                               
046600     IF PART-MISSING                                                      
046700        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
046800        CALL WMEDKONV USING MED-WMEDAREA                                  
046900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047000        PERFORM MFS-ERASE-FIELD-UT                                        
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400*                                                                         
047500 MFS-ERASE-FIELD-UT SECTION.                                              
047600                                                                          
047700*    --- ALLA UTDATA-FÄLT                                                 
047800     MOVE 1 TO INDX                                                       
047900     PERFORM UNTIL INDX > MAX-INDX                                        
048000      MOVE MFS-ERASE-FIELD TO MOD-KDPRURSP (INDX)                         
048100                              MOD-DAPRLIST (INDX)                         
048200                              MOD-IDLEVNR (INDX)                          
048300                              MOD-PRARTBEL-PR (INDX)                      
048400                              MOD-KDVALISO (INDX)                         
048500                              MOD-KDSTATUS (INDX)                         
048510                              MOD-DAREGDAT(INDX)                          
048520                              MOD-IDUSER(INDX)                            
048600      ADD 1 TO INDX                                                       
048700     END-PERFORM                                                          
048800     .                                                                    
048900     SKIP3                                                                
049000*                                                                         
049100 MFS-ERASE-FIELD-IN SECTION.                                              
049200                                                                          
049300*    --- ALLA INDATA-FÄLT                                                 
049400     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
049500     .                                                                    
049600     EJECT                                                                
049700* --- IMS SECTIONS ---                                                    
049800     SKIP3                                                                
049900 IMS-GET-MSG SECTION.                                                     
050000                                                                          
050100     MOVE '  QC' TO GOOD-STATUSCODES                                      
050200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050400     PERFORM IMS-STATUSCHECK                                              
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-INSERT-MSG SECTION.                                                  
050800                                                                          
050900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
051000     MOVE SPACE TO GOOD-STATUSCODES                                       
051100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
051200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
051300     PERFORM IMS-STATUSCHECK                                              
051400     .                                                                    
051500     EJECT                                                                
051600 IMS-GU-WDK611 SECTION.                                                   
051700                                                                          
051800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
051900          DELIMITED BY SIZE INTO SSA1                                     
052000     MOVE 'WDK611  '  TO SSA2                                             
052100     MOVE '  GE'   TO GOOD-STATUSCODES                                    
052200     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
052300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSCHECK                                              
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GNP-WDK621 SECTION.                                                  
052800                                                                          
052900     STRING 'WDK621  (WDK621KY=>' W-WDK621KY-X ')'                        
053000          DELIMITED BY SIZE INTO SSA1                                     
053100     MOVE '  GE' TO GOOD-STATUSCODES                                      
053200     CALL CBLTDLI USING                                                   
053300                  GNP WDK6-PCB DLI-IO-WDK621 SSA1                         
053400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSCHECK                                              
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-GU-WDK701 SECTION.                                                   
053900                                                                          
054000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
054100          DELIMITED BY SIZE INTO SSA1                                     
054200     MOVE '  GE'              TO GOOD-STATUSCODES                         
054300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
054400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
054500     PERFORM IMS-STATUSCHECK                                              
054600     .                                                                    
054700     EJECT                                                                
054800 IMS-GNP-WDK711 SECTION.                                                  
055100     MOVE 'WDK711   '       TO SSA1                                       
055200     MOVE '  GE' TO GOOD-STATUSCODES                                      
055300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
055400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
055500     PERFORM IMS-STATUSCHECK                                              
055600     .                                                                    
055700     SKIP3                                                                
057010 IMS-GNP-WDK724 SECTION.                                                  
057020     MOVE 'WDK724   ' TO SSA1                                             
057600     MOVE '  GE' TO GOOD-STATUSCODES                                      
057700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
057800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
057900     PERFORM IMS-STATUSCHECK                                              
058000     .                                                                    
058100     EJECT                                                                
058200                                                                          
058300 IMS-STATUSCHECK SECTION.                                                 
058400                                                                          
058500     SET STATUS-IX TO 1                                                   
058600     SEARCH GOOD-STATUS                                                   
058700       AT END                                                             
058800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
058900         DELIMITED BY SIZE INTO ERROR-TEXT                                
059000         CALL FELLOG                                                      
059100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
059200         CONTINUE                                                         
059300     END-SEARCH                                                           
059400     .                                                                    
