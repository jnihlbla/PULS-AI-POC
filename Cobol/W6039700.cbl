000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6039700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/06/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN KOPIA AV 6307.                                  
000900*        GODSMOTTAGNINGSHISTORIK SDC/NDC                                  
001000*        - KOMPLETTERANDE INFORMATION TILL 6307.                          
001100*                                                                         
001200*        PROGRAMMET LÄSER  WLINLC (WDL6)                                  
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T397X                                             
001600*        MID:         W6I39701                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O39701                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08) VALUE 'W6039700'.              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000 77  JA                          PIC X     VALUE 'J'.                     
003100 77  NEJ                         PIC X     VALUE 'N'.                     
003200 77  INDX                        PIC S9(3) VALUE +0    COMP-3.            
003300 77  MAX-INDX                    PIC S9(3) VALUE +13   COMP-3.            
003400 77  WS-TIREGDAT                 PIC 9(6)  VALUE ZERO.                    
003500 77  WS-IDINLEV-REGDAT           PIC 9(6)  VALUE ZERO.                    
003600 77  WS-DAINLEV                  PIC 9(16) VALUE ZERO.                    
003700 77  WS-FLDC                     PIC X     VALUE SPACE.                   
003800                                                                          
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200                                                                          
004300 77  BYT-SW                      PIC X       VALUE 'J'.                   
004400     88  BYT-BILD                            VALUE 'J'.                   
004500     88  BYT-EJ-BILD                         VALUE 'N'.                   
004600                                                                          
004700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004800     88  ALLT-OK                             VALUE 'J'.                   
004900                                                                          
005000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005100     88  EGEN-MID                            VALUE '6397'.                
005200     88  GODK-MID                            VALUE '6307' '6397'.         
005300     88  HELP-MID                            VALUE '0551'.                
005400     88  HOPP-MID                            VALUE '6307'.                
005500     EJECT                                                                
005600*      --- VALID IDDC CODES                                               
005700*                                                                         
005800*01    -COPY WWDC99                                                       
005900*01    -COPY WWDC99 -PRE INL-                                             
006000       EJECT                                                              
006100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006200 01  GENERELLA-SUBPROGRAM.                                                
006300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006900*01 -COPY WMEDAREA                                                        
007000     SKIP3                                                                
007100 01  MESSAGE-CODES.                                                       
007200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007300     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
007400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008000     SKIP3                                                                
008100*01 -COPY WMSGINIT                                                        
008200     EJECT                                                                
008300 01  SPAR-AREA.                                                           
008400     03  SPAR-IDTRANS              PIC X(4)    VALUE '6397'.              
008500     03  SPAR-DAINLEV-ENTER        PIC 9(16)   VALUE ZERO.                
008600     03  SPAR-DAINLEV-NEXT         PIC 9(16)   VALUE ZERO.                
008700     03  SPAR-HOPP-DAINLEV-ENTER   PIC 9(16)   VALUE ZERO.                
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W6I39701                                                   
009400     EJECT                                                                
009500 01  W-PROG-TO-PROG-SW-6307.                                              
009600     03  M-SW-LL-6307            PIC S9(4)   VALUE +80 COMP SYNC.         
009700     03  M-SW-Z1-Z2-6307         PIC X(2)    VALUE LOW-VALUE.             
009800     03  M-SW-KDTRANS-6307       PIC X(8)    VALUE 'W6T307  '.            
009900     03  M-SW-IDTRANS-6307       PIC X(4)    VALUE '6397'.                
010000     03  M-SW-KDMFSTYP-6307      PIC X(1)    VALUE '2'.                   
010100                                                                          
010200*    03  MID -COPY W6I30701 -PRE 6307-                                    
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'MSG-AREA'.            
010500     SKIP3                                                                
010600*01  -COPY WMSGAREA                                                       
010700     EJECT                                                                
010800     03  MOD REDEFINES MSG-AREA.                                          
010900*      05  -COPY W6O39701                                                 
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011200     SKIP3                                                                
011300*01  -COPY WMFSAREA                                                       
011400     EJECT                                                                
011500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011600     SKIP3                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011900 01  NYCKLAR-TILL-DLI.                                                    
012000     03  W-DAINLEV-MIN-X.                                                 
012100         05  W-DAINLEV-MIN       PIC 9(16)  VALUE ZERO.                   
012200     03  W-IDARTNR-X.                                                     
012300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012400     SKIP2                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
014100 01  DLI-IO-WDL601.                                                       
014200*    03  -COPY WDL601                                                     
014300     EJECT                                                                
014400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
014500 01  DLI-IO-WDL611.                                                       
014600*    03  -COPY WDL611                                                     
014700     EJECT                                                                
014800 LINKAGE SECTION.                                                         
014900*01  -COPY W0009   -PRE MSG-                                              
015000     EJECT                                                                
015100*01  -COPY W0009   -PRE ALT-                                              
015200     EJECT                                                                
015300*01  -COPY W0008   -PRE USEA-                                             
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008   -PRE WDL6-                                             
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDL6-PCB.             
016000 MAIN SECTION.                                                            
016100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDL6-PCB.             
016200                                                                          
016300     PERFORM IMS-GET-MSG                                                  
016400     IF SEGMENT-FINNS                                                     
016500        PERFORM A-INIT                                                    
016600        PERFORM B-KOLLA-NYCKLAR                                           
016700        IF NYCKLAR-OK                                                     
016800          IF MFS-ENTER AND EGEN-MID                                       
016900             PERFORM H-BYT-BILD                                           
017000          ELSE                                                            
017100             IF MFS-FIRST                                                 
017200                PERFORM C-FOERSTA-SIDA                                    
017300             ELSE                                                         
017400                IF MFS-NEXT                                               
017500                   PERFORM D-NAESTA-SIDA                                  
017600                ELSE                                                      
017700                   PERFORM E-SAMMA-SIDA                                   
017800                END-IF                                                    
017900             END-IF                                                       
018000             PERFORM F-LAES-VISA-INFO                                     
018100          END-IF                                                          
018200        END-IF                                                            
018300        IF BYT-EJ-BILD                                                    
018400           COMPUTE MSG-KVLL = LENGTH OF MOD-W6O39701 + 4                  
018500           PERFORM IMS-INSERT-MSG                                         
018600        END-IF                                                            
018700     END-IF                                                               
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019400                                                                          
019500     IF MSG-DUBBLA-TRANSKODER                                             
019600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I39701                 
019700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019900     ELSE                                                                 
020000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I39701                  
020100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020300     END-IF                                                               
020400                                                                          
020500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020800                                                                          
020900     MOVE LOW-VALUE  TO MSG-AREA                                          
021000     MOVE 'W6O397N1' TO MFS-IDMOD                                         
021100     MOVE '6397'     TO MOD-IDTRANS                                       
021200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021300                                                                          
021400     IF EGEN-MID                                                          
021500        CONTINUE                                                          
021600     ELSE                                                                 
021700        MOVE SPACE TO MFS-KDTRTYP                                         
021800        MOVE '7'   TO MFS-IDPFK                                           
021900     END-IF                                                               
022000                                                                          
022100     MOVE JA TO ALLT-SW                                                   
022200     MOVE NEJ TO BYT-SW                                                   
022300     .                                                                    
022400     EJECT                                                                
022500 B-KOLLA-NYCKLAR SECTION.                                                 
022600                                                                          
022700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022800     MOVE '001'             TO MSGI-KDCALL                                
022900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023100     MOVE '6397'            TO MSGI-IDTRANS                               
023200     IF GODK-MID                                                          
023300       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
023400     END-IF                                                               
023500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023600     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
023700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023800                                                                          
023900     MOVE JA TO NYCKLAR-SW                                                
024000                                                                          
024100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
024200     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
024300       MOVE '7'         TO MFS-IDPFK                                      
024400       MOVE SPACE       TO MFS-KDTRTYP                                    
024500     END-IF                                                               
024600     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
024700     IF MSGI-IDARTNR NUMERIC                                              
024800       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
024900     ELSE                                                                 
025000       MOVE NEJ TO NYCKLAR-SW                                             
025100     END-IF                                                               
025200                                                                          
025300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
025400     IF GODK-MID                                                          
025500        IF MID-IDDC-IN NOT = ALL '+'                                      
025600           MOVE MID-IDDC-IN TO WS-IDDC                                    
025700           MOVE '7'         TO MFS-IDPFK                                  
025800           MOVE SPACE       TO MFS-KDTRTYP                                
025900        ELSE                                                              
026000           MOVE MID-IDDC-UT TO WS-IDDC                                    
026100        END-IF                                                            
026200     ELSE                                                                 
026300        MOVE MSGI-IDDC-KEY  TO WS-IDDC                                    
026400     END-IF                                                               
026500                                                                          
026600     IF GOOD-DC                                                           
026700     OR WS-IDDC NUMERIC                                                   
026800        CONTINUE                                                          
026900     ELSE                                                                 
027000        MOVE NEJ TO NYCKLAR-SW                                            
027100     END-IF                                                               
027200                                                                          
027300     IF GODK-MID OR NYCKLAR-OK                                            
027400        MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                               
027500        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
027600        MOVE WS-IDDC TO MOD-IDDC-UT                                       
027700        INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE               
027800     ELSE                                                                 
027900        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                            
028000                                MOD-IDDC-UT                               
028100     END-IF                                                               
028200                                                                          
028300     IF NYCKLAR-FEL                                                       
028400        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
028500        CALL WMEDKONV USING MED-WMEDAREA                                  
028600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
028700        PERFORM MFS-RENSA-FAELT-UT                                        
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 C-FOERSTA-SIDA SECTION.                                                  
029200                                                                          
029300     IF HOPP-MID                                                          
029400        IF SPAR-HOPP-DAINLEV-ENTER NUMERIC                                
029500           MOVE SPAR-HOPP-DAINLEV-ENTER TO W-DAINLEV-MIN                  
029600        END-IF                                                            
029700     END-IF                                                               
029800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
029900     CALL WMEDKONV USING MED-WMEDAREA                                     
030000     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
030100     .                                                                    
030200     EJECT                                                                
030300 D-NAESTA-SIDA SECTION.                                                   
030400                                                                          
030500     IF SPAR-IDTRANS = '6397'                                             
030600        MOVE SPAR-DAINLEV-NEXT TO W-DAINLEV-MIN                           
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 E-SAMMA-SIDA SECTION.                                                    
031100                                                                          
031200     IF SPAR-IDTRANS = '6397'                                             
031300        MOVE SPAR-DAINLEV-ENTER TO W-DAINLEV-MIN                          
031400        MOVE INF-FIRST-PAGE TO MED-IDMFSINF                               
031500        CALL WMEDKONV USING MED-WMEDAREA                                  
031600        MOVE MED-MFSINF     TO MOD-TEMFSFEL                               
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 F-LAES-VISA-INFO SECTION.                                                
032100                                                                          
032200     PERFORM IMS-GET-WDL601                                               
032300                                                                          
032400     IF SEGMENT-SAKNAS                                                    
032500        MOVE PARTNO-MISSING TO MED-IDMFSFEL                               
032600        CALL WMEDKONV USING MED-WMEDAREA                                  
032700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
032800        PERFORM MFS-RENSA-FAELT-UT                                        
032900     ELSE                                                                 
033000        PERFORM IMS-GET-WDL611                                            
033100        IF SEGMENT-FINNS                                                  
033200           MOVE INL-DAINLEV TO SPAR-DAINLEV-ENTER                         
033300           MOVE INL-IDDC    TO INL-WS-IDDC                                
033400           IF (WS-IDDC = '20' AND INL-SDC)                                
033500           OR (WS-IDDC = '40' AND INL-NDC-NA)                             
033600           OR (WS-IDDC = '60' AND INL-NDC-PACIFIC)                        
033700           OR WS-IDDC      = INL-IDDC                                     
033800              MOVE 'J' TO WS-FLDC                                         
033900           ELSE                                                           
034000              MOVE 'N' TO WS-FLDC                                         
034100           END-IF                                                         
034200           IF WS-FLDC = 'J'                                               
034300              MOVE +1 TO INDX                                             
034400           ELSE                                                           
034500              MOVE +0 TO INDX                                             
034600           END-IF                                                         
034700        ELSE                                                              
034800           MOVE W-DAINLEV-MIN TO SPAR-DAINLEV-ENTER                       
034900        END-IF                                                            
035000                                                                          
035100        PERFORM UNTIL INDX > MAX-INDX                                     
035200           IF SEGMENT-FINNS                                               
035300              IF WS-FLDC = 'J'                                            
035400                 MOVE INL-IDPTYP      TO MOD-IDPTYP(INDX)                 
035500                 MOVE INL-IDDC        TO MOD-IDDC(INDX)                   
035600                 MOVE INL-IDLEVNR     TO MOD-IDLEVNR(INDX)                
035700                 MOVE INL-KDRT        TO MOD-KDRT(INDX)                   
035800                 MOVE INL-TIINLINL    TO MOD-TIINLINL(INDX)               
035900                 MOVE INL-KVAVIS      TO MOD-KVAVIS(INDX)                 
036000                 MOVE INL-KVANTMOT    TO MOD-KVANTMOT(INDX)               
036100                 MOVE INL-KVART-SKROT TO MOD-KVART-SKROT(INDX)            
036200                 MOVE INL-IDUSER-003  TO MOD-IDUSER-003(INDX)             
036300              END-IF                                                      
036400           ELSE                                                           
036500              IF INDX = 0                                                 
036510                MOVE 1 TO INDX                                            
036520              END-IF                                                      
036530              PERFORM MFS-RENSA-RAD-FAELT-UT                              
036700           END-IF                                                         
036800                                                                          
036900           PERFORM IMS-GET-WDL611                                         
037000                                                                          
037100           IF SEGMENT-FINNS                                               
037200              MOVE INL-IDDC TO INL-WS-IDDC                                
037300              IF (WS-IDDC = '20' AND INL-SDC)                             
037400              OR (WS-IDDC = '40' AND INL-NDC-NA)                          
037500              OR (WS-IDDC = '60' AND INL-NDC-PACIFIC)                     
037600              OR WS-IDDC  = INL-IDDC                                      
037700                 MOVE 'J' TO WS-FLDC                                      
037800              ELSE                                                        
037900                 MOVE 'N' TO WS-FLDC                                      
038000              END-IF                                                      
038100           END-IF                                                         
038200           IF WS-FLDC = 'J' OR SEGMENT-SAKNAS                             
038300              ADD 1 TO INDX                                               
038400           END-IF                                                         
038500        END-PERFORM                                                       
038600                                                                          
038700        IF SEGMENT-FINNS                                                  
038800           MOVE INL-DAINLEV TO SPAR-DAINLEV-NEXT                          
038900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
039000           CALL WMEDKONV USING MED-WMEDAREA                               
039100           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
039200        ELSE                                                              
039300           MOVE SPAR-DAINLEV-ENTER TO SPAR-DAINLEV-NEXT                   
039400        END-IF                                                            
039500                                                                          
039600        MOVE '002'     TO MSGI-KDCALL                                     
039700        MOVE '6397'    TO SPAR-IDTRANS                                    
039800        MOVE SPAR-AREA TO MSGI-SPAR-AREA                                  
039900        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 H-BYT-BILD SECTION.                                                      
040400                                                                          
040500     MOVE JA TO BYT-SW                                                    
040600                                                                          
040700     MOVE LOW-VALUE    TO 6307-MID-W6I30701                               
040800     MOVE '+'          TO 6307-MID-FLMORE                                 
040900     MOVE MSGI-IDARTNR TO 6307-MID-IDARTNR-IN                             
041000     MOVE WS-IDDC      TO 6307-MID-IDDC-IN                                
041100     PERFORM IMS-INSERT-ALT-MSG-6307                                      
041200     .                                                                    
041300     EJECT                                                                
041400 MFS-RENSA-FAELT-UT SECTION.                                              
041500                                                                          
041600     MOVE 0 TO SPAR-DAINLEV-ENTER                                         
041700               SPAR-DAINLEV-NEXT                                          
041800               SPAR-HOPP-DAINLEV-ENTER                                    
041900               W-DAINLEV-MIN                                              
042000                                                                          
042100     MOVE +1 TO INDX                                                      
042200     PERFORM UNTIL INDX > MAX-INDX                                        
042300        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
042400        ADD +1 TO INDX                                                    
042500     END-PERFORM                                                          
042600     .                                                                    
042700     SKIP3                                                                
042800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
042900                                                                          
043000     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP      (INDX)                       
043100                             MOD-IDDC        (INDX)                       
043200                             MOD-IDLEVNR     (INDX)                       
043300                             MOD-KDRT        (INDX)                       
043400                             MOD-TIINLINL    (INDX)                       
043500                             MOD-KVAVIS      (INDX)                       
043600                             MOD-KVANTMOT    (INDX)                       
043700                             MOD-KVART-SKROT (INDX)                       
043800                             MOD-IDUSER-003  (INDX)                       
043900     .                                                                    
044000     EJECT                                                                
044100* --- IMS SEKTIONER ---                                                   
044200     SKIP3                                                                
044300 IMS-GET-MSG SECTION.                                                     
044400     MOVE '  QC' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
044600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     SKIP3                                                                
045000 IMS-INSERT-MSG SECTION.                                                  
045100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
045200     MOVE SPACE TO GODK-STATUSKODER                                       
045300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
045400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700     SKIP3                                                                
045800 IMS-INSERT-ALT-MSG-6307 SECTION.                                         
045900     MOVE SPACE TO GODK-STATUSKODER                                       
046000     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-6307               
046100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046500 IMS-GET-WDL601 SECTION.                                                  
046600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
046700          DELIMITED BY SIZE INTO SSA1                                     
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
047000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     SKIP3                                                                
047400 IMS-GET-WDL611 SECTION.                                                  
047500     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-MIN-X ')'                     
047600          DELIMITED BY SIZE INTO SSA1                                     
047700     MOVE '  GE' TO GODK-STATUSKODER                                      
047800     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
047900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     EJECT                                                                
048300 IMS-STATUSKONTROLL SECTION.                                              
048400     SET STATUS-IX TO 1                                                   
048500     SEARCH GODK-STATUS                                                   
048600       AT END                                                             
048700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
048800         DELIMITED BY SIZE INTO FELTEXT                                   
048900         CALL FELLOG                                                      
049000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
049100         CONTINUE                                                         
049200     END-SEARCH                                                           
049300     .                                                                    
