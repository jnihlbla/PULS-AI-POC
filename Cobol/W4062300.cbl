001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4062300.                                                
001600 AUTHOR.         KARANDE DIGAMBAR.                                        
001700 DATE-WRITTEN.   02/06/19.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        SHOWS IDSHIPMENT PER DISTRICT,IDKUNDNR AND DC. ROW CAN BE        
002200*        SELECTED TO JUMP TO SCREEN 4624 BOOKING INFORMATION BILL-        
002300*        IT TO SHOW MORE ABOUT THIS IDSHIPMENT                            
002400*                                                                         
002500*                                                                         
002501*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
002502*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0181               
002503*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
002504*                                                                         
002505*        THE PROGRAM READS     WDE1B                                      
002510*        THE PROGRAM READS     WDE1                                       
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSACTION: W4T623                                              
002900*        MID:         W4I62301                                            
003000*                                                                         
003100*    OUTDATA.                                                             
003200*        MOD:         W4O62301                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4062300'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700                                                                          
004801*    --- INDEX FOR SCROLL LINES                                           
004802 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004810 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004820 77  SPRAK-IX                    PIC S9(9)  VALUE +1    COMP SYNC.        
004830 77  KDCMD-GIVEN-SW              PIC X      VALUE 'N'.                    
004840     88  KDCMD-GIVEN                        VALUE 'J'.                    
004850     88  KDCMD-NOT-GIVEN                    VALUE 'N'.                    
004860                                                                          
004900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005210     88  INDATA-OK                           VALUE 'J'.                   
005220     88  INDATA-WRONG                        VALUE 'N'.                   
005300                                                                          
005400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005500     88  KEYS-OK                             VALUE 'J'.                   
005600     88  KEYS-WRONG                          VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  OWN-MID                             VALUE '4623'.                
006000     88  GOOD-MID                            VALUE '4621' '4622'          
006100                                                   '4623' '4624'          
006200                                                   '4625' '4626'          
006300                                                   '4627' '4628'          
006400                                                   '4629'.                
006500     88  HELP-MID                            VALUE '0551'.                
006510                                                                          
006520 77  W-KEY-DIST-KUND             PIC X(1)    VALUE 'N'.                   
006530     88  KEY-DIST-KUND                       VALUE 'J'.                   
006540     88  KEY-DIST                            VALUE 'N'.                   
006600     EJECT                                                                
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007600*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
007810     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007900     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
008001     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008010     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008011     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008020     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
008030     03  ERR-SELECT-LINE         PIC X(3)    VALUE '309'.                 
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008300     EJECT                                                                
008301 01  FILLER-1.                                                            
008302     03  FILLER                  PIC X(40)                                
008303         VALUE '351 ONLY BILL-IT MARKET ALLOWED        '.                 
008306 01  FILLER REDEFINES FILLER-1.                                           
008307     03  ERR-NOT-BILL-IT         OCCURS 1  PIC X(40).                     
008308     EJECT                                                                
008309*    ----DISTR-DEALER-PRICE----                                           
008310 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
008340     EJECT                                                                
008400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700     SKIP3                                                                
008800*01 -COPY WMSGINIT                                                        
008900     EJECT                                                                
009000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009100*                                                                         
009200 01  SAVE-AREA.                                                           
009300     03  SAVE-IDTRANS            PIC X(4)    VALUE '4623'.                
009400     03  SAVE-IDDC               PIC X(2).                                
009403     03  SAVE-IDDISTR            PIC S9(5).                               
009404     03  SAVE-IDKUNDNR-ENTER     PIC S9(7)        COMP-3.                 
009405     03  SAVE-IDKUNDNR-NEXT      PIC S9(7)        COMP-3.                 
009406     03  SAVE-TISKEPP9-ENTER     PIC S9(7)        COMP-3.                 
009407     03  SAVE-TISKEPP9-NEXT      PIC S9(7)        COMP-3.                 
009409     03  SAVE-IDSHIPM-ENTER      PIC  9(7).                               
009410     03  SAVE-IDSHIPM-NEXT       PIC  9(7).                               
009411     03  SAVE-INPUT  OCCURS 14.                                           
009420         05  SAVE-RECORD-PRESENT     PIC  X(1).                           
009440         05  SAVE-IDSHIPM            PIC  9(7).                           
009450         05  SAVE-IDKUNDNR           PIC S9(7).                           
009490                                                                          
009500     EJECT                                                                
009510 01  W-INIT-SAVE.                                                         
009511     03  W-INIT-INPUT  OCCURS 14.                                         
009520         05  W-INIT-RECORD-PRESENT     PIC X(1).                          
009540         05  W-INIT-IDSHIPM            PIC 9(7).                          
009550         05  W-INIT-IDKUNDNR           PIC S9(7).                         
009600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009900     SKIP3                                                                
010000*01  MID -COPY W4I62301                                                   
010100     EJECT                                                                
010150 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
010160 01  P-TO-P-SW1.                                                          
010170     03  PTOP1-LL                PIC S9(4)   VALUE 28 COMP SYNC.          
010180     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
010190     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
010191     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T624 '.             
010192     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
010193     03  FILLER                  PIC  X(4)   VALUE '4624'.                
010194     03  PTOP1-KDMFSFOR          PIC  X(1).                               
010195*    03  -COPY W4I62401   -PRE PTOP1-                                     
010197 77  W-IDDISTR-4                 PIC 9(04)  VALUE ZERO.                   
010198 77  W-IDKUNDNR-6                PIC 9(06)  VALUE ZERO.                   
010199     EJECT                                                                
010200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010300     SKIP3                                                                
010400*01  -COPY WMSGAREA                                                       
010500     EJECT                                                                
010600     03  MOD REDEFINES MSG-AREA.                                          
010700*      05  -COPY W4O62301                                                 
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011000     SKIP3                                                                
011100*01  -COPY WMFSAREA                                                       
011200     EJECT                                                                
011300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  KEYS-TO-DLI.                                                         
011801*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011802     03  W-WDE1B1KY-MIN.                                                  
011804         05  W-WDE1B-IDDC-MIN        PIC X(2).                            
011805         05  W-WDE1B-IDDISTR-MIN     PIC S9(5)        COMP-3.             
011806         05  W-WDE1B-TISKEPP9-MIN    PIC S9(7)        COMP-3.             
011807         05  W-WDE1B-IDSHIPM-MIN     PIC  9(7).                           
011808         05  W-WDE1B-IDKUNDNR-MIN    PIC S9(7)        COMP-3.             
011810                                                                          
011811     03  W-WDE1B1KY-MAX.                                                  
011812         05  W-WDE1B-IDDC-MAX        PIC  X(2).                           
011813         05  W-WDE1B-IDDISTR-MAX     PIC S9(5)        COMP-3.             
011814         05  W-WDE1B-TISKEPP9-MAX    PIC S9(7)        COMP-3.             
011815         05  W-WDE1B-IDSHIPM-MAX     PIC  9(7).                           
011816         05  W-WDE1B-IDKUNDNR-MAX    PIC S9(7)        COMP-3.             
011818                                                                          
011819     03  W-WDE1B-IDKUNDNR-KVAL-X.                                         
011820         05  W-WDE1B-IDKUNDNR-KVAL   PIC S9(7)        COMP-3.             
011830                                                                          
011832     03  W-WDE1-IDSHIPM-X.                                                
011833         05  W-WDE1-IDSHIPM          PIC 9(7)     VALUE ZERO.             
011834                                                                          
011835     03  W-WDE1B-IDSHIPM-PREV        PIC  9(7).                           
011836                                                                          
011837     03  W-IDDC-B6-X.                                                     
011838         05 W-IDDC-B6                PIC X(2).                            
011839                                                                          
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                       PIC XX.                              
012200     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GOOD-STATUSCODES.                                                    
012700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(256).                              
013000 01  SSA2                        PIC X(128).                              
013100     EJECT                                                                
013200*    --- IMS FUNCTION CODES                                               
013300*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700                                                                          
013801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE1B1'.                      
013802 01  DLI-IO-WDE1B1.                                                       
013803*    03  -COPY WDE1B1                                                     
013804     EJECT                                                                
013805 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
013806 01  DLI-IO-WDE101.                                                       
013807*    03  -COPY WDE101                                                     
013808                                                                          
013809 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013810 01   DLI-IO-AREA-B601.                                                   
013820*     03  -COPY WDB601                                                    
013830                                                                          
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300*01  -COPY W0009   -PRE MSG-                                              
014310*01  -COPY W0009   -PRE ALT1-                                             
014400*01  -COPY W0008   -PRE USEA-                                             
014500     05  FILLER                  PIC X.                                   
014601                                                                          
014602*01  -COPY W0008  -PRE WDE1B-                                             
014603     05  FILLER                  PIC X.                                   
014604                                                                          
014605*01  -COPY W0008  -PRE WDE1-                                              
014606     05  FILLER                  PIC X.                                   
014607                                                                          
014608*01  -COPY W0008  -PRE WDB6-                                              
014609     05  FILLER                  PIC X.                                   
014610                                                                          
014700     EJECT                                                                
014801 PROCEDURE DIVISION  USING MSG-PCB   ALT1-PCB  USEA-PCB                   
014802                           WDE1B-PCB WDE1-PCB  WDB6-PCB.                  
014803                                                                          
014804 MAIN SECTION.                                                            
014810     ENTRY 'DLITCBL' USING MSG-PCB   ALT1-PCB  USEA-PCB                   
014820                           WDE1B-PCB WDE1-PCB  WDB6-PCB.                  
014900                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FOUND                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-CHECK-KEYS                                               
015500       IF KEYS-OK                                                         
015701           IF MFS-FIRST                                                   
015702             PERFORM C-FIRST-PAGE                                         
015703           ELSE                                                           
015704             IF MFS-NEXT                                                  
015705               PERFORM D-NEXT-PAGE                                        
015706             ELSE                                                         
015707               IF MFS-SPLIT                                               
015708                  PERFORM I-PF9-SPLIT                                     
015710               ELSE                                                       
015711                  PERFORM E-SAME-PAGE                                     
015712               END-IF                                                     
015713             END-IF                                                       
015720           END-IF                                                         
015730           IF INDATA-OK                                                   
015740              IF NOT MFS-SPLIT                                            
016000                 PERFORM F-READ-SHOW-INFO                                 
016001              END-IF                                                      
016010           END-IF                                                         
016100       END-IF                                                             
016200*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016300*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016310       IF MFS-SPLIT  AND INDATA-OK                                        
016312          COMPUTE PTOP1-LL = LENGTH OF PTOP1-MID-W4I62401 + 17            
016313          PERFORM IMS-ISRT-MSG-ALT1                                       
016330       ELSE                                                               
016400          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O62301 + 4                   
016500          PERFORM IMS-INSERT-MSG                                          
016510       END-IF                                                             
016600     END-IF                                                               
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
017500     IF MSG-DOUBLE-TRANSACTIONS                                           
017600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I62301                 
017700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017810                              PTOP1-KDMFSFOR                              
017900     ELSE                                                                 
018000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I62301                  
018100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018210                              PTOP1-KDMFSFOR                              
018300     END-IF                                                               
018400                                                                          
018500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018800                                                                          
018900     MOVE LOW-VALUE TO MSG-AREA                                           
019000     MOVE 'W4O623N1' TO MFS-IDMOD                                         
019100     MOVE '4623' TO MOD-IDTRANS                                           
019200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019300                                                                          
019400     IF OWN-MID OR HELP-MID                                               
019500       CONTINUE                                                           
019600     ELSE                                                                 
019700       MOVE SPACE TO MFS-KDTRTYP                                          
019800       MOVE '7' TO MFS-IDPFK                                              
019900     END-IF                                                               
019901                                                                          
019910     MOVE  1  TO INDX                                                     
020000     PERFORM UNTIL INDX > MAX-INDX                                        
020010        MOVE NOO    TO  W-INIT-RECORD-PRESENT (INDX)                      
020030        MOVE ZERO   TO  W-INIT-IDSHIPM (INDX)                             
020031                        W-INIT-IDKUNDNR (INDX)                            
020040        ADD  1      TO  INDX                                              
020100     END-PERFORM                                                          
020110     MOVE ZERO      TO  W-WDE1B-TISKEPP9-MIN                              
020120                        W-WDE1B-IDSHIPM-MIN                               
020130                        W-WDE1B-IDKUNDNR-MIN                              
020131                        W-WDE1B-IDKUNDNR-KVAL                             
020140     MOVE ALL '9'   TO  W-WDE1B-TISKEPP9-MAX                              
020150                        W-WDE1B-IDSHIPM-MAX                               
020160                        W-WDE1B-IDKUNDNR-MAX                              
020200     .                                                                    
020300     EJECT                                                                
020400 B-CHECK-KEYS SECTION.                                                    
020401                                                                          
020600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020700     MOVE '001'             TO MSGI-KDCALL                                
020800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021000     MOVE '4623'            TO MSGI-IDTRANS                               
021100     IF OWN-MID                                                           
021201         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
021210         MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                         
021220         MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                         
021300     END-IF                                                               
021400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021500     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
021600                                                                          
021700*    - LANGUAGE TO BE USED BY MEDKONV                                     
021800     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021900                                                                          
022000     MOVE YES TO KEYS-SW                                                  
022201                                                                          
022202*    -- CHECK OF INPUT KEY                                                
022203     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
022204                             MOD-IDKUNDNR-IN                              
022205                             MOD-IDDC-IN                                  
022206                                                                          
022207     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
022208       MOVE '7'         TO MFS-IDPFK                                      
022209       MOVE SPACE       TO MFS-KDTRTYP                                    
022210       IF MID-IDDISTR-IN NOT NUMERIC                                      
022211         MOVE NOO       TO KEYS-SW                                        
022212       END-IF                                                             
022213     END-IF                                                               
022214                                                                          
022215     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
022216       MOVE '7'         TO MFS-IDPFK                                      
022217       MOVE SPACE       TO MFS-KDTRTYP                                    
022218       IF MID-IDKUNDNR-IN NOT NUMERIC                                     
022219         MOVE NOO       TO KEYS-SW                                        
022220       END-IF                                                             
022221     END-IF                                                               
022222                                                                          
022223     IF MID-IDDC-IN     NOT = ALL '+'                                     
022224       MOVE '7'         TO MFS-IDPFK                                      
022225       MOVE SPACE       TO MFS-KDTRTYP                                    
022226     END-IF                                                               
022227                                                                          
022228     IF MSGI-IDDISTR  NUMERIC                                             
022229        IF MSGI-IDDISTR > ZERO                                            
022230           MOVE MSGI-IDDISTR  TO W-WDE1B-IDDISTR-MIN                      
022231                                 W-WDE1B-IDDISTR-MAX                      
022232        ELSE                                                              
022233           MOVE NOO           TO KEYS-SW                                  
022234        END-IF                                                            
022235     ELSE                                                                 
022236        MOVE NOO              TO KEYS-SW                                  
022237     END-IF                                                               
022239                                                                          
022240     MOVE MSGI-IDDISTR TO TEST-IDDISTR                                    
022245                                                                          
022246     IF MSGI-IDKUNDNR NUMERIC                                             
022247        MOVE  YES             TO W-KEY-DIST-KUND                          
022248        MOVE MSGI-IDKUNDNR    TO W-WDE1B-IDKUNDNR-MIN                     
022249                                 W-WDE1B-IDKUNDNR-MAX                     
022250                                 W-WDE1B-IDKUNDNR-KVAL                    
022253     END-IF                                                               
022254                                                                          
022255*    MOVE MSGI-IDDC-KEY       TO WS-IDDC                                  
022256*    IF GOOD-DC                                                           
022257*       MOVE MSGI-IDDC-KEY    TO W-WDE1B-IDDC-MIN                         
022258*                                W-WDE1B-IDDC-MAX                         
022259*    ELSE                                                                 
022263*       MOVE MSGI-IDDC        TO WS-IDDC                                  
022264*       IF GOOD-DC                                                        
022265*          MOVE MSGI-IDDC     TO W-WDE1B-IDDC-MIN                         
022266*                                  W-WDE1B-IDDC-MAX                       
022267*                                  MSGI-IDDC-KEY                          
022268*       ELSE                                                              
022273*          MOVE NOO           TO KEYS-SW                                  
022275*       END-IF                                                            
022277*    END-IF                                                               
022278* LL                                                                      
022279     IF  MSG-SIGNON-USERID(1:5) = 'PC361'                                 
022281        MOVE MSGI-IDDC-KEY       TO W-IDDC-B6                             
022282        PERFORM IMS-GU-WDB601                                             
022283        IF DCS-KDDC = SPACE                                               
022284           MOVE NOO           TO KEYS-SW                                  
022285        ELSE                                                              
022286           MOVE MSGI-IDDC-KEY    TO W-WDE1B-IDDC-MIN                      
022287                                    W-WDE1B-IDDC-MAX                      
022289        END-IF                                                            
022291     ELSE                                                                 
022293       MOVE MSGI-IDDC           TO W-IDDC-B6                              
022294       PERFORM IMS-GU-WDB601                                              
022295       IF DCS-CDC                                                         
022296          MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                 
022297          PERFORM IMS-GU-WDB601                                           
022298          IF DCS-CDC OR (DCS-DDC AND DCS-IDLANDX2 = 'SE')                 
022299             MOVE MSGI-IDDC-KEY    TO W-WDE1B-IDDC-MIN                    
022300                                      W-WDE1B-IDDC-MAX                    
022301          ELSE                                                            
022302             MOVE MSGI-IDDC        TO W-WDE1B-IDDC-MIN                    
022303                                      W-WDE1B-IDDC-MAX                    
022304                                      MSGI-IDDC-KEY                       
022305          END-IF                                                          
022306       ELSE                                                               
022307          MOVE MSGI-IDDC           TO W-WDE1B-IDDC-MIN                    
022308                                      W-WDE1B-IDDC-MAX                    
022309                                      MSGI-IDDC-KEY                       
022314       END-IF                                                             
022315     END-IF                                                               
022316                                                                          
022317     IF GOOD-MID OR KEYS-OK                                               
022318       MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                           
022320       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
022321       MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                          
022322       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
022323       MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                              
022324     ELSE                                                                 
022325       PERFORM MFS-ERASE-FIELD-OUT                                        
022330     END-IF                                                               
022400                                                                          
022500     IF KEYS-WRONG                                                        
022600        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
022700        CALL WMEDKONV       USING MED-WMEDAREA                            
022800        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
022900        PERFORM MFS-ERASE-FIELD-IN                                        
023000        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
023100     END-IF                                                               
023200     .                                                                    
023301     EJECT                                                                
023302 C-FIRST-PAGE SECTION.                                                    
023303                                                                          
023304     MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                             
023305     CALL WMEDKONV         USING MED-WMEDAREA                             
023306     MOVE MED-MFSINF          TO MOD-TEMFSFEL                             
023307                                                                          
023308     PERFORM MFS-ERASE-FIELD-IN                                           
023309     .                                                                    
023310     EJECT                                                                
023311 D-NEXT-PAGE SECTION.                                                     
023312                                                                          
023316     IF SAVE-IDTRANS = '4623'                                             
023317        IF DCS-IDDC NOT = SAVE-IDDC                                       
023318           MOVE SAVE-IDDC             TO  W-IDDC-B6                       
023319           PERFORM IMS-GU-WDB601                                          
023320        END-IF                                                            
023321        IF DCS-KDDC = SPACE OR DCS-DDC                                    
023322           CONTINUE                                                       
023323        ELSE                                                              
023324           MOVE SAVE-IDDC             TO  W-WDE1B-IDDC-MIN                
023325                                          W-WDE1B-IDDC-MAX                
023326        END-IF                                                            
023327                                                                          
023328        IF SAVE-IDDISTR   NUMERIC AND SAVE-IDDISTR   > ZERO               
023329           MOVE SAVE-IDDISTR          TO  W-WDE1B-IDDISTR-MIN             
023330                                          W-WDE1B-IDDISTR-MAX             
023331        END-IF                                                            
023332                                                                          
023333        IF SAVE-IDKUNDNR-NEXT   NUMERIC                                   
023334           MOVE SAVE-IDKUNDNR-NEXT  TO  W-WDE1B-IDKUNDNR-MIN              
023335                                        W-WDE1B-IDKUNDNR-MAX              
023336                                        W-WDE1B-IDKUNDNR-KVAL             
023337        ELSE                                                              
023338          IF SAVE-IDKUNDNR-ENTER NUMERIC                                  
023339             MOVE SAVE-IDKUNDNR-ENTER  TO W-WDE1B-IDKUNDNR-MIN            
023340                                          W-WDE1B-IDKUNDNR-MAX            
023341                                          W-WDE1B-IDKUNDNR-KVAL           
023342          END-IF                                                          
023343        END-IF                                                            
023344                                                                          
023345        IF SAVE-TISKEPP9-NEXT  NUMERIC AND                                
023346           SAVE-TISKEPP9-NEXT  > ZERO                                     
023347           MOVE SAVE-TISKEPP9-NEXT  TO W-WDE1B-TISKEPP9-MIN               
023348        ELSE                                                              
023349           MOVE SAVE-TISKEPP9-ENTER TO W-WDE1B-TISKEPP9-MIN               
023350        END-IF                                                            
023351                                                                          
023352        IF SAVE-IDSHIPM-NEXT   NUMERIC AND                                
023353           SAVE-IDSHIPM-NEXT    > ZERO                                    
023354           MOVE SAVE-IDSHIPM-NEXT   TO W-WDE1B-IDSHIPM-MIN                
023355        ELSE                                                              
023356           MOVE SAVE-IDSHIPM-ENTER  TO W-WDE1B-IDSHIPM-MIN                
023357        END-IF                                                            
023358                                                                          
023359     ELSE                                                                 
023360        PERFORM MFS-ERASE-FIELD-IN                                        
023361     END-IF                                                               
023362     .                                                                    
023363     EJECT                                                                
023364                                                                          
023365 E-SAME-PAGE SECTION.                                                     
023369     IF SAVE-IDTRANS = '4623' OR '0551'                                   
023370       MOVE SAVE-IDDC            TO W-WDE1B-IDDC-MIN                      
023371                                    W-WDE1B-IDDC-MAX                      
023372                                                                          
023373       MOVE SAVE-IDDISTR         TO W-WDE1B-IDDISTR-MIN                   
023374                                    W-WDE1B-IDDISTR-MAX                   
023376       MOVE SAVE-IDKUNDNR-ENTER  TO W-WDE1B-IDKUNDNR-MIN                  
023377                                    W-WDE1B-IDKUNDNR-MAX                  
023378                                    W-WDE1B-IDKUNDNR-KVAL                 
023380                                                                          
023381       MOVE SAVE-IDSHIPM-ENTER   TO W-WDE1B-IDSHIPM-MIN                   
023382       MOVE SAVE-TISKEPP9-ENTER  TO W-WDE1B-TISKEPP9-MIN                  
023383                                                                          
023384       IF MID-W4I62301  = ALL '+'                                         
023385          PERFORM MFS-ERASE-FIELD-IN                                      
023386       ELSE                                                               
023387          MOVE 1  TO INDX                                                 
023388          PERFORM UNTIL INDX > MAX-INDX                                   
023389            IF MID-KDCMD (INDX ) NOT = ALL '+'                            
023390               IF MID-KDCMD (INDX)  = 'S'  OR  'X'                        
023391                  MOVE INF-PRESS-PF9  TO MED-IDMFSINF                     
023392                  CALL WMEDKONV    USING MED-WMEDAREA                     
023393                  MOVE MED-MFSINF     TO MOD-TEMFSFEL                     
023394                  MOVE NOO            TO INDATA-SW                        
023395                  MOVE MFS-ADD-READ-FIELD                                 
023396                                      TO MOD-KDCMD-ATTR (INDX)            
023397               ELSE                                                       
023398                  MOVE MFS-ALPHA-FIELD-WRONG                              
023399                                      TO MOD-KDCMD-ATTR (INDX)            
023400                  MOVE NOO            TO INDATA-SW                        
023401                  MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL               
023402                  CALL WMEDKONV          USING MED-WMEDAREA               
023403                  MOVE MED-MFSFEL           TO MOD-TEMFSFEL               
023404               END-IF                                                     
023405            END-IF                                                        
023406            ADD  1  TO INDX                                               
023407          END-PERFORM                                                     
023408       END-IF                                                             
023414     ELSE                                                                 
023415       PERFORM MFS-ERASE-FIELD-IN                                         
023416     END-IF                                                               
023417     IF INDATA-WRONG                                                      
023418       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
023419       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
023420     END-IF                                                               
023421     .                                                                    
023422     EJECT                                                                
023423                                                                          
023600 F-READ-SHOW-INFO SECTION.                                                
023740     MOVE 1  TO INDX                                                      
023741     PERFORM UNTIL INDX > MAX-INDX                                        
023742        MOVE W-INIT-INPUT (INDX)  TO SAVE-INPUT (INDX)                    
023743        ADD 1  TO INDX                                                    
023744     END-PERFORM                                                          
023745                                                                          
023750     MOVE +1 TO INDX                                                      
023801     IF KEY-DIST-KUND                                                     
023810        PERFORM IMS-GU-WDE1B-KUND                                         
023820     ELSE                                                                 
023822        PERFORM IMS-GU-WDE1B                                              
023830     END-IF                                                               
023840     IF SEGMENT-FOUND                                                     
023850        MOVE SEQB-IDSHIPM  TO W-WDE1-IDSHIPM                              
023860        PERFORM IMS-GU-WDE1                                               
023870     END-IF                                                               
023900                                                                          
024000     IF SEGMENT-MISSING                                                   
024110        MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                             
024200        CALL WMEDKONV USING MED-WMEDAREA                                  
024300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024400        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
024410        MOVE 1  TO INDX                                                   
024420        PERFORM UNTIL INDX > MAX-INDX                                     
024430           MOVE W-INIT-INPUT (INDX)  TO SAVE-INPUT (INDX)                 
024440           ADD 1  TO INDX                                                 
024450        END-PERFORM                                                       
024460        MOVE W-WDE1B-IDDC-MIN      TO SAVE-IDDC                           
024470        MOVE W-WDE1B-IDDISTR-MIN   TO SAVE-IDDISTR                        
024480        MOVE W-WDE1B-IDKUNDNR-MIN  TO SAVE-IDKUNDNR-ENTER                 
024481                                      SAVE-IDKUNDNR-NEXT                  
024490        MOVE W-WDE1B-TISKEPP9-MIN  TO SAVE-TISKEPP9-ENTER                 
024491                                      SAVE-TISKEPP9-NEXT                  
024492        MOVE W-WDE1B-IDSHIPM-MIN   TO SAVE-IDSHIPM-ENTER                  
024493                                      SAVE-IDSHIPM-NEXT                   
024500     ELSE                                                                 
024611       IF SEGMENT-FOUND                                                   
024612         MOVE SEQB-IDDC             TO SAVE-IDDC                          
024613         MOVE SEQB-IDDISTR          TO SAVE-IDDISTR                       
024615         MOVE SEQB-TISKEPPN-9KOMPL  TO SAVE-TISKEPP9-ENTER                
024618         MOVE SEQB-IDKUNDNR         TO SAVE-IDKUNDNR-ENTER                
024619         MOVE SHIP-IDSHIPM          TO SAVE-IDSHIPM-ENTER                 
024627       ELSE                                                               
024628         MOVE W-WDE1B-IDDC-MIN      TO SAVE-IDDC                          
024629         MOVE W-WDE1B-IDDISTR-MIN   TO SAVE-IDDISTR                       
024631         MOVE W-WDE1B-IDKUNDNR-MIN  TO SAVE-IDKUNDNR-ENTER                
024632                                       SAVE-IDKUNDNR-NEXT                 
024633         MOVE W-WDE1B-TISKEPP9-MIN  TO SAVE-TISKEPP9-ENTER                
024634                                       SAVE-TISKEPP9-NEXT                 
024635         MOVE W-WDE1B-IDSHIPM-MIN   TO SAVE-IDSHIPM-ENTER                 
024636                                       SAVE-IDSHIPM-NEXT                  
024637       END-IF                                                             
024638                                                                          
024639       PERFORM UNTIL INDX > MAX-INDX                                      
024641         IF SEGMENT-FOUND                                                 
024642           IF SHIP-IDTRPTNR <= 998                                        
024643             MOVE YES            TO SAVE-RECORD-PRESENT (INDX)            
024644             MOVE SHIP-IDSHIPM   TO MOD-IDSHIPM (INDX)                    
024645                                    SAVE-IDSHIPM (INDX)                   
024646                                    W-WDE1B-IDSHIPM-PREV                  
024647             MOVE SEQB-IDKUNDNR  TO SAVE-IDKUNDNR (INDX)                  
024648             MOVE SHIP-IDTRPTNR  TO MOD-IDTRPTNR (INDX)                   
024649             MOVE SHIP-IDLBBET   TO MOD-IDLBBET (INDX)                    
024650             MOVE SHIP-TISKPTID  TO MOD-TISKPTID (INDX)                   
024651             MOVE SHIP-TISKEPPN  TO MOD-TISKEPPN (INDX)                   
024652             MOVE MFS-ERASE-FIELD                                         
024653                                 TO MOD-KDCMD (INDX)                      
024654             ADD 1 TO INDX                                                
024655           ELSE                                                           
024656             IF  MSG-SIGNON-USERID(1:5) = 'PC361'                         
024658                MOVE YES            TO SAVE-RECORD-PRESENT (INDX)         
024659                MOVE SHIP-IDSHIPM   TO MOD-IDSHIPM (INDX)                 
024660                                       SAVE-IDSHIPM (INDX)                
024661                                       W-WDE1B-IDSHIPM-PREV               
024662                MOVE SEQB-IDKUNDNR  TO SAVE-IDKUNDNR (INDX)               
024663                MOVE SHIP-IDTRPTNR  TO MOD-IDTRPTNR (INDX)                
024664                MOVE SHIP-IDLBBET   TO MOD-IDLBBET (INDX)                 
024665                MOVE SHIP-TISKPTID  TO MOD-TISKPTID (INDX)                
024666                MOVE SHIP-TISKEPPN  TO MOD-TISKEPPN (INDX)                
024667                MOVE MFS-ERASE-FIELD                                      
024668                                    TO MOD-KDCMD (INDX)                   
024669                ADD 1 TO INDX                                             
024670             END-IF                                                       
024671           END-IF                                                         
024672           IF KEY-DIST-KUND                                               
024673              PERFORM IMS-GN-WDE1B-KUND                                   
024674           ELSE                                                           
024675              PERFORM IMS-GN-WDE1B                                        
024676           END-IF                                                         
024677           IF SEGMENT-FOUND                                               
024678              MOVE SEQB-IDSHIPM  TO W-WDE1-IDSHIPM                        
024679              PERFORM IMS-GU-WDE1                                         
024680           END-IF                                                         
024681         ELSE                                                             
024682           MOVE MFS-ERASE-FIELD TO MOD-IDSHIPM (INDX)                     
024683                                   MOD-IDTRPTNR (INDX)                    
024684                                   MOD-IDLBBET (INDX)                     
024685                                   MOD-TISKPTID (INDX)                    
024686                                   MOD-TISKEPPN (INDX)                    
024687           MOVE MFS-CLOSE-FIELD TO MOD-KDCMD-ATTR (INDX)                  
024688           ADD 1 TO INDX                                                  
024689         END-IF                                                           
024690*        MOVE MFS-ERASE-FIELD TO MOD-KDCMD (INDX)                         
024691       END-PERFORM                                                        
024692                                                                          
024693       IF SEGMENT-FOUND                                                   
024694         MOVE SEQB-TISKEPPN-9KOMPL  TO SAVE-TISKEPP9-NEXT                 
024695         MOVE SEQB-IDKUNDNR         TO SAVE-IDKUNDNR-NEXT                 
024696         MOVE SHIP-IDSHIPM          TO SAVE-IDSHIPM-NEXT                  
024697         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
024698         CALL WMEDKONV           USING MED-WMEDAREA                       
024699         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
024700       ELSE                                                               
024701         MOVE SAVE-TISKEPP9-ENTER   TO SAVE-TISKEPP9-NEXT                 
024702         MOVE SAVE-IDKUNDNR-ENTER   TO SAVE-IDKUNDNR-NEXT                 
024703         MOVE SAVE-IDSHIPM-ENTER    TO SAVE-IDSHIPM-NEXT                  
024704         MOVE INF-LAST-PAGE         TO MED-IDMFSINF                       
024705         CALL WMEDKONV           USING MED-WMEDAREA                       
024706         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
024707       END-IF                                                             
024708                                                                          
024709       MOVE '002'       TO MSGI-KDCALL                                    
024710       MOVE '4623'      TO SAVE-IDTRANS                                   
024711       MOVE SAVE-AREA   TO MSGI-SPAR-AREA                                 
024712       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
024720     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
024901                                                                          
024910 I-PF9-SPLIT      SECTION.                                                
024920                                                                          
024924     MOVE  1       TO INDX                                                
024925     PERFORM UNTIL INDX > MAX-INDX                                        
024926       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
024927         IF KDCMD-GIVEN                                                   
024928           MOVE NOO                  TO INDATA-SW                         
024929           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024930           CALL WMEDKONV          USING MED-WMEDAREA                      
024931           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
024932           MOVE MFS-ALPHA-FIELD-WRONG                                     
024933                                     TO MOD-KDCMD-ATTR (INDX)             
024934         ELSE                                                             
024935           MOVE YES TO KDCMD-GIVEN-SW                                     
024936           IF MID-KDCMD (INDX) = 'S' OR 'X'                               
024937             IF SAVE-RECORD-PRESENT (INDX)  =  YES                        
024938               MOVE ALL '+'              TO PTOP1-MID-W4I62401            
024939               MOVE SAVE-IDDC            TO PTOP1-MID-IDDC-IN             
024940               MOVE SAVE-IDDISTR         TO W-IDDISTR-4                   
024941               MOVE W-IDDISTR-4          TO PTOP1-MID-IDDISTR-IN          
024942               MOVE SAVE-IDKUNDNR (INDX) TO W-IDKUNDNR-6                  
024943               MOVE W-IDKUNDNR-6         TO PTOP1-MID-IDKUNDNR-IN         
024945               MOVE SAVE-IDSHIPM (INDX)  TO PTOP1-MID-IDSHIPM-IN          
024946               MOVE ZERO                 TO PTOP1-MID-IDTRPTNR-IN         
024959             ELSE                                                         
024960               MOVE NOO                  TO INDATA-SW                     
024961               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
024962               CALL WMEDKONV          USING MED-WMEDAREA                  
024963               MOVE MED-MFSFEL           TO MOD-TEMFSFEL                  
024964               MOVE MFS-ALPHA-FIELD-WRONG                                 
024965                                         TO MOD-KDCMD-ATTR (INDX)         
024966             END-IF                                                       
024967           ELSE                                                           
024968             MOVE NOO                  TO INDATA-SW                       
024969             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
024970             CALL WMEDKONV          USING MED-WMEDAREA                    
024971             MOVE MED-MFSFEL           TO MOD-TEMFSFEL                    
024972             MOVE MFS-ALPHA-FIELD-WRONG                                   
024973                                       TO MOD-KDCMD-ATTR (INDX)           
024974           END-IF                                                         
024975         END-IF                                                           
024976       END-IF                                                             
024977       ADD 1     TO INDX                                                  
024978     END-PERFORM                                                          
024979                                                                          
024980     IF KDCMD-NOT-GIVEN                                                   
024981       MOVE NOO                    TO INDATA-SW                           
024982       MOVE ERR-SELECT-LINE        TO MED-IDMFSFEL                        
024983       CALL WMEDKONV            USING MED-WMEDAREA                        
024984       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
024985     END-IF                                                               
024986                                                                          
024987     IF INDATA-WRONG                                                      
024988       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
024989       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
024990     END-IF                                                               
024991     .                                                                    
024992     EJECT                                                                
025000                                                                          
026000 MFS-ERASE-FIELD-OUT SECTION.                                             
026100                                                                          
026200*    --- ALLA UTDATA-FÄLT                                                 
026310*    --- INCL. SCROLL KEYS                                                
026400     MOVE MFS-ERASE-FIELD  TO MOD-IDDISTR-UT                              
026500                              MOD-IDKUNDNR-UT                             
026510                              MOD-IDDC-UT                                 
026600     .                                                                    
026701     SKIP3                                                                
026702 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
026703                                                                          
026704*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
026705     MOVE 1               TO INDX                                         
026706     PERFORM UNTIL INDX > MAX-INDX                                        
026708        MOVE MFS-ERASE-FIELD TO MOD-KDCMD (INDX)                          
026709                                MOD-IDSHIPM (INDX)                        
026710                                MOD-IDTRPTNR (INDX)                       
026711                                MOD-IDLBBET (INDX)                        
026712                                MOD-TISKPTID (INDX)                       
026713                                MOD-TISKEPPN (INDX)                       
026714        ADD  1               TO INDX                                      
026716     END-PERFORM                                                          
026720     .                                                                    
026800     SKIP3                                                                
026900 MFS-ERASE-FIELD-IN SECTION.                                              
027000                                                                          
027100*    --- ALLA INDATA-FÄLT                                                 
027200     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
027300                             MOD-IDKUNDNR-IN                              
027310                             MOD-IDDC-IN                                  
027400     .                                                                    
027500     EJECT                                                                
027600 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
027700                                                                          
027800*    --- ALLA UTDATA-FÄLT                                                 
027910*    --- INCL SCROLL KEYS AND LINEDATA                                    
028000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-UT                        
028100                                    MOD-IDKUNDNR-UT                       
028200                                    MOD-IDDC-UT                           
028201     MOVE +1 TO INDX                                                      
028202     PERFORM UNTIL INDX > MAX-INDX                                        
028203       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
028204       ADD +1 TO INDX                                                     
028205     END-PERFORM                                                          
028206     .                                                                    
028207     EJECT                                                                
028208     SKIP2                                                                
028209 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
028210                                                                          
028211*    --- OUTDATA FIELD ON SCROLL KEYS                                     
028212     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD (INDX)                      
028213                                    MOD-IDSHIPM (INDX)                    
028214                                    MOD-IDTRPTNR (INDX)                   
028215                                    MOD-IDLBBET (INDX)                    
028216                                    MOD-TISKPTID (INDX)                   
028217                                    MOD-TISKEPPN (INDX)                   
028300     .                                                                    
028400     SKIP3                                                                
028500 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
028600                                                                          
028700*    --- ALLA INDATA-FÄLT                                                 
028800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-IN                        
028900                                    MOD-IDKUNDNR-IN                       
028910                                    MOD-IDDC-IN                           
029000     .                                                                    
029100     EJECT                                                                
030600* --- IMS SECTIONS ---                                                    
030700     SKIP3                                                                
030800 IMS-GET-MSG SECTION.                                                     
030900                                                                          
031000     MOVE '  QC' TO GOOD-STATUSCODES                                      
031100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSCHECK                                              
031400     .                                                                    
031500     SKIP3                                                                
031600 IMS-INSERT-MSG SECTION.                                                  
031700                                                                          
032100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032200     MOVE SPACE TO GOOD-STATUSCODES                                       
032300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032500     PERFORM IMS-STATUSCHECK                                              
032600     .                                                                    
032701     EJECT                                                                
032702 IMS-ISRT-MSG-ALT1 SECTION.                                               
032703                                                                          
032704     MOVE SPACE TO GOOD-STATUSCODES                                       
032705     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW1                          
032706     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
032707     PERFORM IMS-STATUSCHECK                                              
032708     .                                                                    
032709     SKIP3                                                                
032710 IMS-GU-WDE1B SECTION.                                                    
032711                                                                          
032712     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032713                    '&WDE1B1KY<=' W-WDE1B1KY-MAX ')'                      
032714          DELIMITED BY SIZE INTO SSA1                                     
032715     MOVE '  GE' TO GOOD-STATUSCODES                                      
032716     CALL CBLTDLI USING GU WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032717     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032718     PERFORM IMS-STATUSCHECK                                              
032730     .                                                                    
032731     EJECT                                                                
032732 IMS-GN-WDE1B SECTION.                                                    
032733                                                                          
032734     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032735                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032736                    '&IDSHIPM NE' W-WDE1B-IDSHIPM-PREV ')'                
032737          DELIMITED BY SIZE INTO SSA1                                     
032738     MOVE '  GE' TO GOOD-STATUSCODES                                      
032739     CALL CBLTDLI USING GN WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032740     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032741     PERFORM IMS-STATUSCHECK                                              
032742     .                                                                    
032743     EJECT                                                                
032744 IMS-GU-WDE1B-KUND SECTION.                                               
032745                                                                          
032750     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032751                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032752                    '&IDKUNDNR =' W-WDE1B-IDKUNDNR-KVAL-X ')'             
032753          DELIMITED BY SIZE INTO SSA1                                     
032754     MOVE '  GE' TO GOOD-STATUSCODES                                      
032755     CALL CBLTDLI USING GU WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032756     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032773     PERFORM IMS-STATUSCHECK                                              
032774     .                                                                    
032775     EJECT                                                                
032776 IMS-GN-WDE1B-KUND SECTION.                                               
032777                                                                          
032778     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032779                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032780                    '&IDKUNDNR =' W-WDE1B-IDKUNDNR-KVAL-X ')'             
032782          DELIMITED BY SIZE INTO SSA1                                     
032783     MOVE '  GE' TO GOOD-STATUSCODES                                      
032784     CALL CBLTDLI USING GN WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032785     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032786     PERFORM IMS-STATUSCHECK                                              
032787     .                                                                    
032788     EJECT                                                                
032789 IMS-GU-WDE1      SECTION.                                                
032790                                                                          
032791     STRING 'WDE101  (IDSHIPM  =' W-WDE1-IDSHIPM-X ')'                    
032792          DELIMITED BY SIZE INTO SSA1                                     
032793     MOVE '  GE' TO GOOD-STATUSCODES                                      
032794     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
032795     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
032796     PERFORM IMS-STATUSCHECK                                              
032803     .                                                                    
032804     EJECT                                                                
032805 IMS-GU-WDB601    SECTION.                                                
032806     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032807          DELIMITED BY SIZE INTO SSA1                                     
032808     MOVE '  GE' TO GOOD-STATUSCODES                                      
032809     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
032810     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032820     PERFORM IMS-STATUSCHECK                                              
032830     IF SEGMENT-MISSING                                                   
032840         MOVE SPACE TO DCS-KDDC                                           
032850     END-IF                                                               
032860     .                                                                    
032900 IMS-STATUSCHECK SECTION.                                                 
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GOOD-STATUS                                                   
033300       AT END                                                             
033400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033500         DELIMITED BY SIZE INTO ERROR-TEXT                                
033600         CALL FELLOG                                                      
033700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    
