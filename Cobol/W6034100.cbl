001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6034100.                                                
001500 AUTHOR.         MARTIEN HOMPES.                                          
001600 DATE-WRITTEN.   97/02/06.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        MAINTANCE FREQUENCY TABEL                                        
002100*                                                                         
002210*        THE PROGRAM UPDATES   WL6313 (WDGX)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSACTION: W6T341                                              
002600*        MID:         W6I34101                                            
002700*                                                                         
002800*    OUTDATA.                                                             
002900*        MOD:         W6O34101                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6034100'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  NEW                         PIC X        VALUE 'N'.                  
004401 77  DEL                         PIC X        VALUE 'D'.                  
004402 77  CHG                         PIC X        VALUE 'C'.                  
004403                                                                          
004404*    --- INDEX FOR SCROLL LINES                                           
004405 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004802     88  INDATA-OK                           VALUE 'Y'.                   
004810     88  INDATA-WRONG                        VALUE 'N'.                   
004900                                                                          
005000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005100     88  KEYS-OK                             VALUE 'Y'.                   
005200     88  KEYS-WRONG                          VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  OWN-MID                             VALUE '6341'.                
005600     88  GOOD-MID                            VALUE '6341'.                
006100     88  HELP-MID                            VALUE '0551'.                
006101                                                                          
006120 77  WS-KDFREQ-IN                PIC 9(2)   VALUE ZERO.                   
006130 77  WS-KDFREQ                   PIC 9(2)   VALUE ZERO.                   
006140 77  WS-TEFREQ                   PIC X(10)  VALUE SPACE.                  
006150 77  WS-KVPB-FOM                 PIC 9(5)V9(1).                           
006160 77  WS-KVPB-TOM                 PIC 9(5)V9(2).                           
006170 77  WS-RELOCFAC                 PIC 9(1)V9(2).                           
006171                                                                          
006172*      --- VALID IDDC CODES                                               
006173*                                                                         
006174*01    -COPY WWDC99                                                       
006175       EJECT                                                              
006300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007000     EJECT                                                                
007100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007504     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007505     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007620     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900     EJECT                                                                
008000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008410*    --- PARAMETERS FOR SUB PROGRAM WDECAREA                              
008420*                                                                         
008500 01  FILLER                      PIC X(16)  VALUE 'WDECAREA '.            
008503*01    -COPY WDECAREA                                                     
008504     SKIP3                                                                
008505     EJECT                                                                
008507*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008508*                                                                         
008509 01  SAVE-AREA.                                                           
008510     03  SAVE-IDTRANS        PIC X(4)    VALUE  SPACE.                    
008511     03  SAVE-KDFREQ-ENTER   PIC X(2).                                    
008520     03  SAVE-KDFREQ-NEXT    PIC X(2).                                    
008600     EJECT                                                                
008700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I34101                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O34101                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  KEYS-TO-DLI.                                                         
011001*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011002     03  W-KDFREQ-MIN-X.                                                  
011003          05  W-KDFREQ-MIN       PIC X(2).                                
011004                                                                          
011005     03  W-KDFREQ-MAX-X.                                                  
011006          05  W-KDFREQ-MAX       PIC X(2)    VALUE HIGH-VALUE.            
011007                                                                          
011008     03  W-WDGXKEY-6313-X.                                                
011009          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
011010          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
011011          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
011012                                                                          
011013     03  W-WDGXKEY-6314-X.                                                
011020         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FOUND                       VALUE '  '.                  
011500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011610     88  END-OF-DATABASE                     VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GOOD-STATUSCODES.                                                    
011900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNCTION CODES                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013002 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
013005 01  WL631301-AREA.                                                       
013006*    03  -COPY WDGX6313                                                   
013007     EJECT                                                                
013009 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
013011 01  WL631311-AREA.                                                       
013020*    03  -COPY WDGX6314                                                   
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE 6313-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6313-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6313-PCB.                     
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FOUND                                                     
014500       PERFORM A-INIT                                                     
014600          PERFORM B-CHECK-KEYS                                            
014700          IF KEYS-OK                                                      
014801             IF MFS-UPDATE                                                
014802                PERFORM G-CHECK-INPUT                                     
014803                IF INDATA-OK                                              
014804                   PERFORM H-UPDATE                                       
014805                   PERFORM F-READ-SHOW-INFO                               
014806                 END-IF                                                   
014810              ELSE                                                        
014901                IF MFS-FIRST                                              
014902                   PERFORM C-FIRST-PAGE                                   
014903                ELSE                                                      
014904                   IF MFS-NEXT                                            
014905                      PERFORM D-NEXT-PAGE                                 
014906                   ELSE                                                   
014907                      PERFORM E-SAME-PAGE                                 
014908                   END-IF                                                 
014910               END-IF                                                     
014920               PERFORM F-READ-SHOW-INFO                                   
015110             END-IF                                                       
015300          END-IF                                                          
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O34101 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DOUBLE-TRANSACTIONS                                           
016800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I34101                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W6I34101                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
017800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
017900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
018000                                                                          
018100     MOVE LOW-VALUE        TO MSG-AREA                                    
018200     MOVE 'W6O341N1'       TO MFS-IDMOD                                   
018300     MOVE '6341'           TO MOD-IDTRANS                                 
018400     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
018500                                                                          
018600     IF GOOD-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                         TO MFS-KDTRTYP                  
019000       MOVE '7'                           TO MFS-IDPFK                    
019010       PERFORM MFS-INIT-KEY-FIELD-IN                                      
019012       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
019013       PERFORM MFS-ERASE-FIELD-IN                                         
019020       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
019100     END-IF                                                               
019110                                                                          
019200     IF MID-KDFREQ-IN NOT = ALL '+'                                       
019300       MOVE '7'         TO MFS-IDPFK                                      
019310       MOVE SPACE       TO MFS-KDTRTYP                                    
019320     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-CHECK-KEYS SECTION.                                                    
019700                                                                          
021001                                                                          
021002     MOVE YES               TO KEYS-SW                                    
021003                                                                          
021004     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021005     MOVE '001'             TO MSGI-KDCALL                                
021006     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021007     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021008     MOVE '6341'            TO MSGI-IDTRANS                               
021009                                                                          
021010     IF OWN-MID                                                           
021011        MOVE MID-KDFREQ-IN TO MSGI-KDFREQ                                 
021012     END-IF                                                               
021013                                                                          
021014     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021015*                                                                         
021017     MOVE MFS-RENSA-FAELT   TO MOD-KDFREQ-IN                              
021018                               MOD-IDDC-IN                                
021019                                                                          
021020     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021021     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
021022     MOVE MSGI-IDDC         TO WS-IDDC                                    
021023                                                                          
021024*    -- CONTROL  ON FREQUENCY                                             
021025     IF MSGI-KDFREQ NUMERIC                                               
021026        IF MSGI-KDFREQ < 10                                               
021027           MOVE '10' TO MSGI-KDFREQ                                       
021028           MOVE '10' TO W-KDFREQ-MIN                                      
021029           MOVE '99' TO W-KDFREQ-MAX                                      
021030        END-IF                                                            
021032     ELSE                                                                 
021033        IF MSGI-KDFREQ = '0 '  OR MSGI-KDFREQ = SPACE                     
021034           MOVE '10' TO MSGI-KDFREQ                                       
021035           MOVE '10' TO W-KDFREQ-MIN                                      
021036           MOVE '99' TO W-KDFREQ-MAX                                      
021038        ELSE                                                              
021040           MOVE NOO                 TO KEYS-SW                            
021041        END-IF                                                            
021042     END-IF                                                               
021043                                                                          
021044*    -- CONTROL  ON IDDC                                                  
021045     IF CDC                                                               
021046       CONTINUE                                                           
021047     ELSE                                                                 
021048       MOVE NOO                 TO KEYS-SW                                
021049     END-IF                                                               
021050                                                                          
021051*    --  END OF CONTROLS                                                  
021052                                                                          
021053                                                                          
021054     IF GOOD-MID OR KEYS-OK                                               
021055        MOVE MSGI-KDFREQ    TO WS-KDFREQ                                  
021060                                                                          
021062        MOVE WS-KDFREQ      TO W-KDFREQ                                   
021063                                                                          
021064        MOVE WS-KDFREQ      TO MOD-KDFREQ-UT                              
021065        MOVE WS-IDDC        TO MOD-IDDC-UT                                
021067     ELSE                                                                 
021068        MOVE MFS-RENSA-FAELT TO MOD-KDFREQ-UT                             
021070     END-IF                                                               
021080                                                                          
021300     IF KEYS-WRONG                                                        
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-ERASE-FIELD-IN                                         
021710       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FIRST-PAGE SECTION.                                                    
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
022107                                                                          
022108     PERFORM MFS-ERASE-FIELD-IN                                           
022110     MOVE WS-KDFREQ      TO W-KDFREQ                                      
022111     .                                                                    
022112     EJECT                                                                
022113 D-NEXT-PAGE SECTION.                                                     
022114                                                                          
022115     IF SAVE-IDTRANS = '6341'                                             
022116       MOVE SAVE-KDFREQ-NEXT TO W-KDFREQ                                  
022118     ELSE                                                                 
022119       MOVE ZEROES           TO W-KDFREQ                                  
022121     END-IF                                                               
022122     .                                                                    
022123     EJECT                                                                
022124 E-SAME-PAGE SECTION.                                                     
022125                                                                          
022126     IF OWN-MID OR HELP-MID                                               
022127       MOVE SAVE-KDFREQ-ENTER TO W-KDFREQ                                 
022129       IF MID-KDFREQ-MAIN   = ALL '+' AND                                 
022130          MID-TEFREQ-MAIN   = ALL '+' AND                                 
022131          MID-KVPB-FOM-MAIN = ALL '+' AND                                 
022132          MID-KVPB-TOM-MAIN = ALL '+' AND                                 
022133          MID-RELOCFAC-MAIN = ALL '+' AND                                 
022134          MID-KDANDR-MAIN   = ALL '+'                                     
022135          PERFORM MFS-ERASE-FIELD-IN                                      
022136       ELSE                                                               
022137         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022138         CALL WMEDKONV USING MED-WMEDAREA                                 
022139         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022140         PERFORM EA-MID-INDATA-TO-MOD                                     
022141       END-IF                                                             
022142     ELSE                                                                 
022143       PERFORM MFS-ERASE-FIELD-IN                                         
022144     END-IF                                                               
022145     .                                                                    
022146     EJECT                                                                
022147 EA-MID-INDATA-TO-MOD SECTION.                                            
022148                                                                          
022154     IF MID-RELOCFAC-MAIN  = ALL '+'                                      
022155        MOVE MFS-ERASE-FIELD        TO MOD-RELOCFAC-ATTR                  
022158      ELSE                                                                
022159        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELOCFAC-MAIN                  
022160        MOVE MFS-ADD-READ-FIELD     TO MOD-RELOCFAC-ATTR                  
022161     END-IF                                                               
022162                                                                          
022163     IF MID-KVPB-FOM-MAIN  = ALL '+'                                      
022164        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-FOM-ATTR                  
022167      ELSE                                                                
022168        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVPB-FOM-MAIN                  
022169        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-FOM-ATTR                  
022170     END-IF                                                               
022171                                                                          
022172     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
022173        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-TOM-ATTR                  
022176      ELSE                                                                
022177        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVPB-TOM-MAIN                  
022178        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-TOM-ATTR                  
022179     END-IF                                                               
022180                                                                          
022181     IF MID-TEFREQ-MAIN  = ALL '+'                                        
022182        MOVE MFS-ERASE-FIELD        TO MOD-TEFREQ-ATTR                    
022184      ELSE                                                                
022185        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEFREQ-MAIN                    
022186        MOVE MFS-ADD-READ-FIELD     TO MOD-TEFREQ-ATTR                    
022188     END-IF                                                               
022189                                                                          
022190     IF MID-KDANDR-MAIN  = ALL '+'                                        
022191        MOVE MFS-ERASE-FIELD        TO MOD-KDANDR-ATTR                    
022193      ELSE                                                                
022194        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDANDR-MAIN                    
022195        MOVE MFS-ADD-READ-FIELD     TO MOD-KDANDR-ATTR                    
022197     END-IF                                                               
022198                                                                          
022199     IF MID-KDFREQ-MAIN  = ALL '+'                                        
022200        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ-ATTR                    
022202      ELSE                                                                
022203        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-MAIN                    
022204        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
022206     END-IF                                                               
022210     .                                                                    
022300     EJECT                                                                
022400 F-READ-SHOW-INFO SECTION.                                                
022500                                                                          
022610     MOVE WS-IDDC      TO  W-6313-IDDC                                    
022611                                                                          
022620     PERFORM  IMS-GU-6313-6314                                            
022700                                                                          
022800     IF SEGMENT-MISSING                                                   
022920        MOVE ITEMS-MISSING TO MED-IDMFSFEL                                
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
023210        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
023220        MOVE WS-KDFREQ     TO SAVE-KDFREQ-ENTER                           
023230        MOVE WS-KDFREQ     TO SAVE-KDFREQ-NEXT                            
023300     ELSE                                                                 
023400        MOVE 6314-KDFREQ   TO SAVE-KDFREQ-ENTER                           
023405        MOVE +1            TO INDX                                        
023414        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
023415                      INDX > MAX-INDX                                     
023418         MOVE 6314-KDFREQ         TO MOD-KDFREQ-LINE   (INDX)             
023420         MOVE 6314-TEFREQ         TO MOD-TEFREQ-LINE   (INDX)             
023421         MOVE 6314-KVPB-FOM       TO MOD-KVPB-FOM-LINE (INDX)             
023422         MOVE 6314-KVPB-TOM       TO MOD-KVPB-TOM-LINE (INDX)             
023423         MOVE 6314-RELOCFAC       TO MOD-RELOCFAC-LINE (INDX)             
023424         PERFORM IMS-GN-6313-6314                                         
023432         ADD 1 TO INDX                                                    
023433        END-PERFORM                                                       
023440        IF SEGMENT-FOUND                                                  
023441           MOVE 6314-KDFREQ  TO SAVE-KDFREQ-NEXT                          
023442           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
023443           CALL WMEDKONV USING MED-WMEDAREA                               
023444           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
023445         ELSE                                                             
023446           MOVE 6314-KDFREQ         TO SAVE-KDFREQ-NEXT                   
023447           IF MFS-NEXT                                                    
023448              MOVE INF-LAST-PAGE-SHOWN TO MED-IDMFSFEL                    
023449              CALL WMEDKONV         USING MED-WMEDAREA                    
023450              MOVE MED-MFSFEL       TO    MOD-TEMFSFEL                    
023451           END-IF                                                         
023452           PERFORM UNTIL INDX > MAX-INDX                                  
023453             MOVE MFS-ERASE-FIELD   TO   MOD-KDFREQ-LINE   (INDX)         
023454                                         MOD-TEFREQ-LINE   (INDX)         
023455                                         MOD-KVPB-FOM-LINE (INDX)         
023456                                         MOD-KVPB-TOM-LINE (INDX)         
023457                                         MOD-RELOCFAC-LINE (INDX)         
023458             ADD 1 TO INDX                                                
023459           END-PERFORM                                                    
023460         END-IF                                                           
023461                                                                          
023462       MOVE '002'      TO MSGI-KDCALL                                     
023463       MOVE '6341'     TO SAVE-IDTRANS                                    
023464       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
023470       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024602 G-CHECK-INPUT SECTION.                                                   
024603                                                                          
024604     MOVE YES  TO INDATA-SW                                               
024605                                                                          
024606     IF MID-KDFREQ-MAIN   = ALL '+' AND                                   
024607        MID-TEFREQ-MAIN   = ALL '+' AND                                   
024608        MID-KVPB-FOM-MAIN = ALL '+' AND                                   
024609        MID-KVPB-TOM-MAIN = ALL '+' AND                                   
024610        MID-RELOCFAC-MAIN = ALL '+' AND                                   
024620        MID-KDANDR-MAIN   = ALL '+'                                       
024630          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
024640          CALL WMEDKONV USING MED-WMEDAREA                                
024650          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
024662          MOVE NOO TO INDATA-SW                                           
024663     END-IF                                                               
024664     IF INDATA-OK                                                         
024665        IF MID-KDFREQ-MAIN = ALL '+'                                      
024666           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDFREQ-ATTR                 
024667           MOVE NOO                    TO INDATA-SW                       
024668         ELSE                                                             
024669           IF MID-KDFREQ-MAIN NUMERIC                                     
024670              IF MID-KDFREQ-MAIN < 10                                     
024671                 MOVE MFS-NUM-FIELD-WRONG TO MOD-KDANDR-ATTR              
024672                 MOVE NOO              TO INDATA-SW                       
024674              ELSE                                                        
024676                 MOVE MFS-ALPHA-FIELD-OK TO MOD-KDFREQ-ATTR               
024677              END-IF                                                      
024678            ELSE                                                          
024679              MOVE MFS-NUM-FIELD-WRONG TO MOD-KDANDR-ATTR                 
024680              MOVE NOO                 TO INDATA-SW                       
024681           END-IF                                                         
024682        END-IF                                                            
024683        IF MID-KDANDR-MAIN = ALL '+'                                      
024684           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDANDR-ATTR                 
024685           MOVE NOO                    TO INDATA-SW                       
024686         ELSE                                                             
024687           IF MID-KDANDR-MAIN = NEW OR DEL OR CHG                         
024688              MOVE MFS-ALPHA-FIELD-OK  TO MOD-KDANDR-ATTR                 
024694           ELSE                                                           
024695              MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDANDR-ATTR           
024696              MOVE NOO                       TO INDATA-SW                 
024697           END-IF                                                         
024698        END-IF                                                            
024699        IF INDATA-WRONG                                                   
024700           PERFORM GD-MID-INDATA-TO-MOD                                   
024701           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024702           CALL WMEDKONV USING MED-WMEDAREA                               
024703           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
024704        END-IF                                                            
024705     END-IF                                                               
024706                                                                          
024707     IF INDATA-OK                                                         
024708        IF MID-KDANDR-MAIN = NEW                                          
024709           PERFORM GA-CHECK-INPUT-NEW                                     
024710        END-IF                                                            
024711        IF MID-KDANDR-MAIN = CHG                                          
024712           PERFORM GB-CHECK-INPUT-CHANGE                                  
024713        END-IF                                                            
024714        IF MID-KDANDR-MAIN = DEL                                          
024715           PERFORM GC-CHECK-INPUT-DELETE                                  
024716        END-IF                                                            
024717        IF INDATA-WRONG                                                   
024719           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024720           CALL WMEDKONV USING MED-WMEDAREA                               
024721           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
024722        END-IF                                                            
024723     END-IF                                                               
024724                                                                          
024725     IF INDATA-WRONG                                                      
024726        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
024760     END-IF                                                               
024769                                                                          
024770     .                                                                    
024771     EJECT                                                                
024772 GA-CHECK-INPUT-NEW SECTION.                                              
024773                                                                          
024774*                                                                         
024775*   --- CHECK RELOCATION FACTOR                                           
024776*                                                                         
024777     IF MID-RELOCFAC-MAIN = ALL '+'                                       
024778        MOVE NOO                    TO INDATA-SW                          
024779        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-RELOCFAC-ATTR                  
024780       ELSE                                                               
024781        MOVE MID-RELOCFAC-MAIN      TO DEC-IDFRIDATA                      
024782        MOVE 1                      TO DEC-KVHELTAL                       
024783        MOVE 2                      TO DEC-KVDECIMAL                      
024784        CALL WDECEDIT USING DEC-WDECAREA                                  
024785        END-CALL                                                          
024786        IF DEC-KDSVAR-OK                                                  
024787          MOVE MFS-ALPHA-FIELD-OK   TO MOD-RELOCFAC-ATTR                  
024788          MOVE DEC-IDEDITDATA       TO WS-RELOCFAC                        
024789         ELSE                                                             
024790          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-RELOCFAC-ATTR                 
024791          MOVE NOO                   TO INDATA-SW                         
024792        END-IF                                                            
024793     END-IF                                                               
024794*                                                                         
024795*   --- CHECK FORECAST FACTOR THRU                                        
024796*                                                                         
024797     IF MID-KVPB-FOM-MAIN = ALL '+'                                       
024798        MOVE NOO                    TO INDATA-SW                          
024799        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KVPB-FOM-ATTR                  
024800       ELSE                                                               
024801        MOVE MID-KVPB-FOM-MAIN      TO DEC-IDFRIDATA                      
024802        MOVE 6                      TO DEC-KVHELTAL                       
024803        MOVE 1                      TO DEC-KVDECIMAL                      
024804        CALL WDECEDIT USING DEC-WDECAREA                                  
024805        END-CALL                                                          
024806        IF DEC-KDSVAR-OK                                                  
024807          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-FOM-ATTR                  
024808          MOVE DEC-IDEDITDATA       TO WS-KVPB-FOM                        
024809         ELSE                                                             
024810          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-FOM-ATTR                 
024811          MOVE NOO                   TO INDATA-SW                         
024812        END-IF                                                            
024813     END-IF                                                               
024814*                                                                         
024815*   --- CHECK FORECAST FACTOR FROM                                        
024816*                                                                         
024817     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
024818        MOVE NOO                    TO INDATA-SW                          
024819        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KVPB-TOM-ATTR                  
024820       ELSE                                                               
024821        MOVE MID-KVPB-TOM-MAIN      TO DEC-IDFRIDATA                      
024822        MOVE 6                      TO DEC-KVHELTAL                       
024823        MOVE 1                      TO DEC-KVDECIMAL                      
024824        CALL WDECEDIT USING DEC-WDECAREA                                  
024825        END-CALL                                                          
024826        IF DEC-KDSVAR-OK                                                  
024827          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-TOM-ATTR                  
024828          MOVE DEC-IDEDITDATA       TO WS-KVPB-TOM                        
024829         ELSE                                                             
024830          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-TOM-ATTR                 
024831          MOVE NOO                   TO INDATA-SW                         
024832        END-IF                                                            
024833     END-IF                                                               
024834*                                                                         
024835*   --- CHECK FREQUENCY DESCRIPTION                                       
024836*                                                                         
024837     IF MID-TEFREQ-MAIN = ALL '+'                                         
024838        MOVE NOO                     TO INDATA-SW                         
024839        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-TEFREQ-ATTR                   
024840      ELSE                                                                
024841        MOVE MFS-ALPHA-FIELD-OK      TO MOD-TEFREQ-ATTR                   
024842        MOVE MID-TEFREQ-MAIN         TO WS-TEFREQ                         
024843     END-IF                                                               
024844                                                                          
024845*                                                                         
024846*   --- CHECK FREQUENCY ALREADY EXIST                                     
024847*                                                                         
024848     MOVE WS-IDDC                    TO W-6313-IDDC                       
024849     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
024850                                                                          
024851     PERFORM  IMS-GU-6313-6314                                            
024860                                                                          
024870     IF SEGMENT-FOUND                                                     
024871        MOVE NOO                     TO INDATA-SW                         
024872        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
024873       ELSE                                                               
024874        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
024875        MOVE MID-KDFREQ-MAIN         TO WS-KDFREQ                         
024876     END-IF                                                               
024877                                                                          
024883     .                                                                    
024884     EJECT                                                                
024885 GB-CHECK-INPUT-CHANGE SECTION.                                           
024886*                                                                         
024887*   --- CHECK RELOCATION FACTOR                                           
024888*                                                                         
024889     IF MID-RELOCFAC-MAIN NOT = ALL '+'                                   
024890        MOVE MID-RELOCFAC-MAIN      TO DEC-IDFRIDATA                      
024891        MOVE 1                      TO DEC-KVHELTAL                       
024892        MOVE 2                      TO DEC-KVDECIMAL                      
024893        CALL WDECEDIT USING DEC-WDECAREA                                  
024894        END-CALL                                                          
024895        IF DEC-KDSVAR-OK                                                  
024896          MOVE MFS-ALPHA-FIELD-OK   TO MOD-RELOCFAC-ATTR                  
024897          MOVE DEC-IDEDITDATA       TO WS-RELOCFAC                        
024898*         MOVE DEC-IDEDITDATA       TO FREQ-RELOCFAC                      
024899         ELSE                                                             
024900          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-RELOCFAC-ATTR                 
024901          MOVE NOO                   TO INDATA-SW                         
024902        END-IF                                                            
024903     END-IF                                                               
024904*                                                                         
024905*   --- CHECK FORECAST FACTOR THRU                                        
024906*                                                                         
024907     IF MID-KVPB-FOM-MAIN NOT = ALL '+'                                   
024908        MOVE MID-KVPB-FOM-MAIN      TO DEC-IDFRIDATA                      
024909        MOVE 6                      TO DEC-KVHELTAL                       
024910        MOVE 1                      TO DEC-KVDECIMAL                      
024911        CALL WDECEDIT USING DEC-WDECAREA                                  
024912        END-CALL                                                          
024913        IF DEC-KDSVAR-OK                                                  
024914          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-FOM-ATTR                  
024915          MOVE DEC-IDEDITDATA       TO WS-KVPB-FOM                        
024916*         MOVE DEC-IDEDITDATA       TO FREQ-KVPB-FOM                      
024917         ELSE                                                             
024918          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-FOM-ATTR                 
024919          MOVE NOO                   TO INDATA-SW                         
024920        END-IF                                                            
024921     END-IF                                                               
024922*                                                                         
024923*   --- CHECK FORECAST FACTOR THRU                                        
024924*                                                                         
024925     IF MID-KVPB-TOM-MAIN NOT = ALL '+'                                   
024926        MOVE MID-KVPB-TOM-MAIN      TO DEC-IDFRIDATA                      
024927        MOVE 6                      TO DEC-KVHELTAL                       
024928        MOVE 1                      TO DEC-KVDECIMAL                      
024929        CALL WDECEDIT USING DEC-WDECAREA                                  
024930        END-CALL                                                          
024931        IF DEC-KDSVAR-OK                                                  
024932          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-TOM-ATTR                  
024933          MOVE DEC-IDEDITDATA       TO WS-KVPB-TOM                        
024934*         MOVE DEC-IDEDITDATA       TO FREQ-KVPB-TOM                      
024935         ELSE                                                             
024936          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-TOM-ATTR                 
024937          MOVE NOO                   TO INDATA-SW                         
024938        END-IF                                                            
024939     END-IF                                                               
024940*                                                                         
024941*   --- CHECK FREQUENCY DESCRIPTION                                       
024942*                                                                         
024943     IF MID-TEFREQ-MAIN NOT = ALL '+'                                     
024944        MOVE MFS-ALPHA-FIELD-OK      TO MOD-TEFREQ-ATTR                   
024945        MOVE MID-TEFREQ-MAIN         TO WS-TEFREQ                         
024946     END-IF                                                               
024947                                                                          
024948*                                                                         
024949*   --- CHECK IF FREQUENCY KODE EXIST                                     
024950*                                                                         
024951     MOVE WS-IDDC                    TO W-6313-IDDC                       
024952     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
024953                                                                          
024954     PERFORM  IMS-GHU-6314                                                
024955                                                                          
024956     IF SEGMENT-MISSING                                                   
024957        MOVE NOO                     TO INDATA-SW                         
024958        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
024959       ELSE                                                               
024960        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
024961        MOVE MID-KDFREQ-MAIN         TO WS-KDFREQ                         
024962     END-IF                                                               
024963     .                                                                    
024964     EJECT                                                                
024965                                                                          
024966 GC-CHECK-INPUT-DELETE SECTION.                                           
024967                                                                          
024968     MOVE WS-IDDC                    TO W-6313-IDDC                       
024969     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
024970                                                                          
024971     PERFORM  IMS-GHU-6314                                                
024972*                                                                         
024973*   --- CHECK IF FREQUENCY KODE EXIST                                     
024974*                                                                         
024975     IF SEGMENT-MISSING                                                   
024976        MOVE NOO                     TO INDATA-SW                         
024977        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
024978       ELSE                                                               
024979        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
024980     END-IF                                                               
024981     .                                                                    
024982     EJECT                                                                
024983 GD-MID-INDATA-TO-MOD SECTION.                                            
024984                                                                          
024985     IF MID-RELOCFAC-MAIN  = ALL '+'                                      
024986        MOVE MFS-ERASE-FIELD        TO MOD-RELOCFAC-ATTR                  
024987      ELSE                                                                
024988        MOVE MFS-ADD-READ-FIELD     TO MOD-RELOCFAC-ATTR                  
024989     END-IF                                                               
024990                                                                          
024991     IF MID-KVPB-FOM-MAIN  = ALL '+'                                      
024992        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-FOM-ATTR                  
024993      ELSE                                                                
024994        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-FOM-ATTR                  
024995     END-IF                                                               
024996                                                                          
024997     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
024998        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-TOM-ATTR                  
024999      ELSE                                                                
025000        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-TOM-ATTR                  
025001     END-IF                                                               
025002                                                                          
025003     IF MID-TEFREQ-MAIN  = ALL '+'                                        
025004        MOVE MFS-ERASE-FIELD        TO MOD-TEFREQ-ATTR                    
025005      ELSE                                                                
025006        MOVE MFS-ADD-READ-FIELD     TO MOD-TEFREQ-ATTR                    
025007     END-IF                                                               
025008                                                                          
025022     .                                                                    
025023     EJECT                                                                
025024 H-UPDATE SECTION.                                                        
025025                                                                          
025026     IF MID-KDANDR-MAIN = NEW                                             
025027        PERFORM HA-NEW                                                    
025028     END-IF                                                               
025029     IF MID-KDANDR-MAIN = CHG                                             
025030        PERFORM HB-CHANGE                                                 
025031     END-IF                                                               
025032     IF MID-KDANDR-MAIN = DEL                                             
025033        PERFORM HC-DELETE                                                 
025034     END-IF                                                               
025035                                                                          
025036     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025037     CALL WMEDKONV USING MED-WMEDAREA                                     
025038     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025039     PERFORM MFS-FORM-ATTR                                                
025040     PERFORM MFS-ERASE-FIELD-IN                                           
025041     .                                                                    
025042     EJECT                                                                
025043 HA-NEW SECTION.                                                          
025044       MOVE WS-IDDC           TO W-6313-IDDC                              
025046       MOVE MID-KDFREQ-MAIN   TO W-KDFREQ                                 
025047                                 6314-KDFREQ                              
025048                                 W-KDFREQ                                 
025049                                 MOD-KDFREQ-UT                            
025050       MOVE WS-TEFREQ         TO 6314-TEFREQ                              
025051       MOVE WS-KVPB-FOM       TO 6314-KVPB-FOM                            
025052       MOVE WS-KVPB-TOM       TO 6314-KVPB-TOM                            
025053       MOVE WS-RELOCFAC       TO 6314-RELOCFAC                            
025054                                                                          
025056       PERFORM IMS-ISRT-6313-6314                                         
025057                                                                          
025058     .                                                                    
025059     EJECT                                                                
025060 HB-CHANGE SECTION.                                                       
025061     IF MID-RELOCFAC-MAIN NOT = ALL '+'                                   
025062        MOVE WS-RELOCFAC          TO 6314-RELOCFAC                        
025063     END-IF                                                               
025064     IF MID-KVPB-FOM-MAIN NOT = ALL '+'                                   
025065        MOVE WS-KVPB-FOM          TO 6314-KVPB-FOM                        
025066     END-IF                                                               
025067     IF MID-KVPB-TOM-MAIN NOT = ALL '+'                                   
025068        MOVE WS-KVPB-TOM          TO 6314-KVPB-TOM                        
025069     END-IF                                                               
025070     IF MID-TEFREQ-MAIN   NOT = ALL '+'                                   
025071        MOVE WS-TEFREQ            TO 6314-TEFREQ                          
025072     END-IF                                                               
025073                                                                          
025074     PERFORM IMS-REPL-6313-6314                                           
025075                                                                          
025076     MOVE WS-KDFREQ            TO W-KDFREQ                                
025077                                  MOD-KDFREQ-UT                           
025078     .                                                                    
025079     EJECT                                                                
025080 HC-DELETE SECTION.                                                       
025081                                                                          
025082     PERFORM IMS-DLET-6313-6314                                           
025083     .                                                                    
025090     EJECT                                                                
025100 MFS-INIT-KEY-FIELD-IN SECTION.                                           
025200                                                                          
025300*    --- ALL INPUT KEY FIELDS                                             
025400                                                                          
025500     MOVE MFS-ERASE-FIELD          TO MOD-IDDC-IN                         
025501                                      MOD-KDFREQ-IN                       
025506     .                                                                    
025507     EJECT                                                                
025508 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
025509                                                                          
025510*    --- ALL OUTPUT KEY FIELDS                                            
025511                                                                          
025512     MOVE MSGI-IDDC                TO MOD-IDDC-UT                         
025513     MOVE MFS-ERASE-FIELD          TO MOD-KDFREQ-UT                       
025516     .                                                                    
025517     EJECT                                                                
025518 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
025519                                                                          
025520*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
025521                                                                          
025522     MOVE +1 TO INDX                                                      
025523     PERFORM UNTIL INDX > MAX-INDX                                        
025524      MOVE MFS-ERASE-FIELD     TO MOD-KDFREQ-LINE   (INDX)                
025525                                  MOD-TEFREQ-LINE   (INDX)                
025526                                  MOD-KVPB-FOM-LINE (INDX)                
025527                                  MOD-KVPB-TOM-LINE (INDX)                
025528                                  MOD-RELOCFAC-LINE (INDX)                
025529      ADD +1 TO INDX                                                      
025530     END-PERFORM                                                          
025540     .                                                                    
025600     SKIP3                                                                
025700 MFS-ERASE-FIELD-IN SECTION.                                              
025800                                                                          
025900*    --- ALL INPUT DATA FIELDS                                            
025901                                                                          
025910     MOVE MFS-ERASE-FIELD          TO MOD-KDFREQ-MAIN                     
025920                                      MOD-TEFREQ-MAIN                     
025930                                      MOD-KVPB-FOM-MAIN                   
025940                                      MOD-KVPB-TOM-MAIN                   
025950                                      MOD-RELOCFAC-MAIN                   
025960                                      MOD-KDANDR-MAIN                     
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
026500                                                                          
026600*    --- ALL OUTDATA FIELDS                                               
026710*    --- INCL SCROLL KEYS AND LINEDATA                                    
026720     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-MAIN                       
026730                                    MOD-TEFREQ-MAIN                       
026740                                    MOD-KVPB-FOM-MAIN                     
026750                                    MOD-KVPB-TOM-MAIN                     
026760                                    MOD-RELOCFAC-MAIN                     
026770                                    MOD-KDANDR-MAIN                       
027001     MOVE +1 TO INDX                                                      
027002     PERFORM UNTIL INDX > MAX-INDX                                        
027003       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
027004       ADD +1 TO INDX                                                     
027005     END-PERFORM                                                          
027006     .                                                                    
027007     EJECT                                                                
027009 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
027010                                                                          
027011*    --- OUTDATA FIELD ON SCROLL KEYS                                     
027012     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-LINE   (INDX)              
027013                                    MOD-TEFREQ-LINE   (INDX)              
027014                                    MOD-KVPB-FOM-LINE (INDX)              
027015                                    MOD-KVPB-TOM-LINE (INDX)              
027016                                    MOD-RELOCFAC-LINE (INDX)              
027800     .                                                                    
027900     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALL INDATA-FIELDS                                                
028300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDFREQ-ATTR                      
028400                                     MOD-TEFREQ-ATTR                      
028410                                     MOD-KVPB-FOM-ATTR                    
028420                                     MOD-KVPB-TOM-ATTR                    
028430                                     MOD-RELOCFAC-ATTR                    
028440                                     MOD-KDANDR-ATTR                      
028500     .                                                                    
028600     SKIP2                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GOOD-STATUSCODES                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSCHECK                                              
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GOOD-STATUSCODES                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSCHECK                                              
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GU-6313-6314 SECTION.                                                
031503                                                                          
031504     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
031505          DELIMITED BY SIZE INTO SSA1                                     
031506     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X                        
031507                    '&KDFREQ  =<' W-KDFREQ-MAX-X ')'                      
031508          DELIMITED BY SIZE INTO SSA2                                     
031509     MOVE '  GE' TO GOOD-STATUSCODES                                      
031510     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
031520     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031530     PERFORM IMS-STATUSCHECK                                              
031531     .                                                                    
031532     SKIP3                                                                
031533 IMS-GN-6313-6314 SECTION.                                                
031534                                                                          
031535     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
031536          DELIMITED BY SIZE INTO SSA1                                     
031537     STRING 'WL631311(KDFREQ  >=' W-WDGXKEY-6314-X                        
031538                    '&KDFREQ  =<' W-KDFREQ-MAX-X ')'                      
031539          DELIMITED BY SIZE INTO SSA2                                     
031540     MOVE '  GE' TO GOOD-STATUSCODES                                      
031541     CALL CBLTDLI USING GN 6313-PCB 6314-WDGX6314 SSA1 SSA2               
031542     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031543     PERFORM IMS-STATUSCHECK                                              
031544     .                                                                    
031545     SKIP3                                                                
031561 IMS-GHU-6314 SECTION.                                                    
031562                                                                          
031563     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
031564          DELIMITED BY SIZE INTO SSA1                                     
031565     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
031566          DELIMITED BY SIZE INTO SSA2                                     
031567     MOVE '  GE' TO GOOD-STATUSCODES                                      
031568     CALL CBLTDLI USING GHU 6313-PCB 6314-WDGX6314 SSA1 SSA2              
031569     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031570     PERFORM IMS-STATUSCHECK                                              
031571     .                                                                    
031572     SKIP3                                                                
031634 IMS-ISRT-6313-6314 SECTION.                                              
031635                                                                          
031636                                                                          
031637     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
031638          DELIMITED BY SIZE INTO SSA1                                     
031639     MOVE 'WL631311 '  TO SSA2                                            
031640     MOVE '  II' TO GOOD-STATUSCODES                                      
031641     CALL CBLTDLI USING ISRT 6313-PCB 6314-WDGX6314 SSA1 SSA2             
031642     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031643     PERFORM IMS-STATUSCHECK                                              
031644     .                                                                    
031645     SKIP3                                                                
031646 IMS-REPL-6313-6314 SECTION.                                              
031647                                                                          
031648     MOVE '  ' TO GOOD-STATUSCODES                                        
031649     CALL CBLTDLI USING REPL 6313-PCB 6314-WDGX6314                       
031650     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031651     PERFORM IMS-STATUSCHECK                                              
031652     .                                                                    
031653     SKIP3                                                                
031654 IMS-DLET-6313-6314 SECTION.                                              
031655                                                                          
031656     MOVE '  ' TO GOOD-STATUSCODES                                        
031657     CALL CBLTDLI USING DLET 6313-PCB 6314-WDGX6314                       
031658     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
031659     PERFORM IMS-STATUSCHECK                                              
031660     .                                                                    
031670     EJECT                                                                
031700 IMS-STATUSCHECK SECTION.                                                 
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GOOD-STATUS                                                   
032100       AT END                                                             
032200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
032300         DELIMITED BY SIZE INTO ERROR-TEXT                                
032400         CALL FELLOG                                                      
032500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
