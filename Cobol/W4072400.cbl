000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4072400.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   94/08/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MPP SOM VISAR INFO OM EN ARTIKELS FÖREKOMST PÅ                   
001000*        LEVERANSANMÄRKNINGSREGISTRET WDA2.                               
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
001300*        PROGRAMMET LÄSER      WLKREI (WDA2)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T724                                              
001700*        MID:         W4I72401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O72401                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700*    -COPY WY2000W1                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)  VALUE 'W4072400'.             
003000 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
003100 77  JA                          PIC X      VALUE 'J'.                    
003200 77  YES                         PIC X      VALUE 'Y'.                    
003300 77  NEJ                         PIC X      VALUE 'N'.                    
003400 77  FEL                         PIC X      VALUE 'F'.                    
003500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
003700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
003900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004200 77  W-IDDISTR-SEC               PIC 9(4).                                
004300 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
004400 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
004500 77  WS-KDKREBEH                 PIC X(3)    VALUE SPACE.                 
004600 77  WS-IDRADNR                  PIC 9(4)    VALUE ZERO.                  
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  P-TO-P-SW                   PIC X       VALUE 'N'.                   
005700     88  HOPP-OK                             VALUE 'J'.                   
005800     88  HOPP-FEL                            VALUE 'F'.                   
005900     88  EJ-BILD-BYTE                        VALUE 'N'.                   
006000                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  EGEN-MID                            VALUE '4724'.                
006300     88  GODK-MID                            VALUE '4721' '4722'          
006400                                                   '4723' '4724'          
006500                                                   '4725' '4726'          
006600                                                   '4727' '4728'          
006700                                                   '4729'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
007800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
007900     EJECT                                                                
008000*    ---  LÄNKAREA TILL W418OKOD                                          
008100 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
008200                                                                          
008300*01 -COPY W418OKOD           -PRE OKOD-.                                  
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
008600 01  FILLER                      PIC X(16)   VALUE 'W006PRT '.            
008700                                                                          
008800*01  -COPY W006PRT                                                        
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
009100                                                                          
009200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009300*01 -COPY WMEDAREA                                                        
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'W005INIT'.            
009600                                                                          
009700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010100*                                                                         
010200 01  SPAR-AREA.                                                           
010300     03  SPAR-IDTRANS             PIC X(4)    VALUE '4724'.               
010400     03  SPAR-INFO-RADER OCCURS 13.                                       
010500       05  SPAR-IDRADNR           PIC S9(5) VALUE ZERO  COMP-3.           
010600     EJECT                                                                
010700                                                                          
010800*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
010900*   -COPY WSECAREA                                                        
011000     EJECT                                                                
011100 01  MESSAGE-CODES.                                                       
011200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011300     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
011400     03  ERR-END-VALUE           PIC X(3)    VALUE '240'.                 
011500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300                                                                          
012400*01  MID -COPY W4I72401                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700                                                                          
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     03  MOD REDEFINES MSG-AREA.                                          
013100*      05  -COPY W4O72401                                                 
013200     EJECT                                                                
013300     03  4712-MID REDEFINES MSG-AREA.                                     
013400       05  FILLER                PIC X(13).                               
013500*      05  -COPY W4I71201  -PRE 4712-                                     
013600     EJECT                                                                
013700     03  4715-MID REDEFINES MSG-AREA.                                     
013800       05  FILLER                PIC X(13).                               
013900*      05  -COPY W4I71501  -PRE 4715-                                     
014000     EJECT                                                                
014100     03  4723-MID REDEFINES MSG-AREA.                                     
014200       05  FILLER                PIC X(13).                               
014300*      05  -COPY W4I72301  -PRE 4723-                                     
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014600                                                                          
014700*01  -COPY WMFSAREA                                                       
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000                                                                          
015100 01  NYCKLAR-TILL-DLI.                                                    
015200     03  W-IDLEVANM-X.                                                    
015300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015500         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
015600                                                                          
015700     03  W-IDDC-X.                                                        
015800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015900                                                                          
016000     03  W-WDA211KY-X.                                                    
016100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016200         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
016300                                                                          
016400     03  W-WDA2D1KY-MIN-X.                                                
016500         05  W-IDFTG-MIN         PIC  9(2)   VALUE ZERO.                  
016600         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
016700         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
016800         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
016900         05  W-IDRAPPNR-MIN      PIC  9(7)   VALUE ZERO.                  
017000         05  W-IDRADNR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
017100                                                                          
017200     03  W-WDA2D1KY-MAX-X.                                                
017300         05  W-IDFTG-MAX         PIC  9(2)   VALUE ZERO.                  
017400         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
017500         05  W-IDDISTR-MAX       PIC S9(5)   VALUE 99999 COMP-3.          
017600         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999 COMP-3.        
017700         05  FILLER              PIC X(10)   VALUE HIGH-VALUE.            
017800                                                                          
017900     03  W-KDANMORS-X.                                                    
018000         05  W-KDANMORS          PIC X(2).                                
018100     EJECT                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  BASEN-SLUT                          VALUE 'GB'.                  
018800                                                                          
018900 01  GODK-STATUSKODER.                                                    
019000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100                                                                          
019200 01  SSA1                        PIC X(128).                              
019300 01  SSA2                        PIC X(64).                               
019400     EJECT                                                                
019500*    --- IMS FUNKTIONSKODER                                               
019600*01  -COPY W0003                                                          
019700     EJECT                                                                
019800*    ---  DLI INPUT-OUTPUT AREA                                           
019900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020000                                                                          
020100 01  FILLER                      PIC X(16)   VALUE 'WDA201-AREA'.         
020200 01  DLI-IO-AREA-WDA201.                                                  
020300*    03  -COPY WDA201                                                     
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)   VALUE 'WDA211-AREA'.         
020600 01  DLI-IO-AREA-WDA211.                                                  
020700*    03  -COPY WDA211                                                     
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16)   VALUE 'WDA2D1-AREA'.         
021000 01  DLI-IO-AREA-WDA2D1.                                                  
021100*    03  -COPY WDA2D1                                                     
021200     EJECT                                                                
021300 LINKAGE SECTION.                                                         
021400                                                                          
021500*01  -COPY W0009   -PRE MSG-                                              
021600     EJECT                                                                
021700*01  -COPY W0009   -PRE 4712-                                             
021800*01  -COPY W0009   -PRE 4715-                                             
021900*01  -COPY W0009   -PRE 4723-                                             
022000     EJECT                                                                
022100*01  -COPY W0008  -PRE USEA-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE KREE-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE KREI-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000 PROCEDURE DIVISION  USING MSG-PCB                                        
023100                           4712-PCB                                       
023200                           4715-PCB                                       
023300                           4723-PCB                                       
023400                           USEA-PCB                                       
023500                           KREE-PCB                                       
023600                           KREI-PCB.                                      
023700     ENTRY 'DLITCBL' USING MSG-PCB                                        
023800                           4712-PCB                                       
023900                           4715-PCB                                       
024000                           4723-PCB                                       
024100                           USEA-PCB                                       
024200                           KREE-PCB                                       
024300                           KREI-PCB.                                      
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600     IF SEGMENT-FINNS                                                     
024700       PERFORM A-INIT                                                     
024800       PERFORM B-KOLLA-NYCKLAR                                            
024900       IF NYCKLAR-OK                                                      
025000         IF MFS-FIRST                                                     
025100           PERFORM C-FOERSTA-SIDA                                         
025200         ELSE                                                             
025300           IF MFS-NEXT                                                    
025400             PERFORM D-NAESTA-SIDA                                        
025500           ELSE                                                           
025600             PERFORM E-SAMMA-SIDA                                         
025700           END-IF                                                         
025800         END-IF                                                           
025900         IF EJ-BILD-BYTE                                                  
026000           PERFORM F-LAES-VISA-INFO                                       
026100         END-IF                                                           
026200       END-IF                                                             
026300       PERFORM Z-FINIT                                                    
026400     END-IF                                                               
026500                                                                          
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT                         SECTION.                                  
027100                                                                          
027200     IF MSG-DUBBLA-TRANSKODER                                             
027300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72401                 
027400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027600     ELSE                                                                 
027700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I72401                  
027800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028000     END-IF                                                               
028100                                                                          
028200     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
028300     MOVE MSG-IDPFK             TO MFS-IDPFK                              
028400     MOVE MFS-IDTRANS           TO W-IDTRANS                              
028500                                                                          
028600     MOVE LOW-VALUE             TO MSG-AREA                               
028700     MOVE 'W4O72401'            TO MFS-IDMOD                              
028800     MOVE '4724'                TO MOD-IDTRANS                            
028900     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
029000     MOVE SPACE                 TO MED-IDMFSFEL MED-IDMFSINF              
029100                                                                          
029200     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O72401 + 4                  
029300                                                                          
029400     IF EGEN-MID OR HELP-MID                                              
029500       CONTINUE                                                           
029600     ELSE                                                                 
029700       MOVE SPACE TO MFS-KDTRTYP                                          
029800       MOVE '7' TO MFS-IDPFK                                              
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 B-KOLLA-NYCKLAR                SECTION.                                  
030300                                                                          
030400     MOVE ALL '+'               TO MSGI-WMSGINIT                          
030500     MOVE '001'                 TO MSGI-KDCALL                            
030600     IF EGEN-MID                                                          
030700        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
030800     END-IF                                                               
030900     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
031000     MOVE '4724'                TO MSGI-IDTRANS                           
031100     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
031200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031300     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
031400                                                                          
031500     IF MSGI-IDLAND-SPR = 'GB'                                            
031600       MOVE 'GB '               TO MED-IDSKYLT                            
031700     ELSE                                                                 
031800       MOVE 'S  '               TO MED-IDSKYLT                            
031900     END-IF                                                               
032000                                                                          
032100     MOVE JA                    TO NYCKLAR-SW                             
032200                                                                          
032300*    -- KONTROLL AV IDARTNR                                               
032400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
032500                                                                          
032600     IF MID-IDARTNR-IN  NOT = ALL '+'                                     
032700       MOVE '7'                 TO MFS-IDPFK                              
032800       MOVE SPACE               TO MFS-KDTRTYP                            
032900     END-IF                                                               
033000                                                                          
033100     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
033200       MOVE MSGI-IDARTNR        TO W-IDARTNR-MIN                          
033300                                   W-IDARTNR-MAX                          
033400                                   MOD-IDARTNR-UT                         
033500       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
033600     ELSE                                                                 
033700       MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN                         
033800       MOVE NEJ                 TO NYCKLAR-SW                             
033900     END-IF                                                               
034000                                                                          
034100*    -- KONTROLL AV IDDISTR                                               
034200     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
034300                                                                          
034400     IF EGEN-MID                                                          
034500       IF MID-IDDISTR-IN = ALL '+'                                        
034600         MOVE MID-IDDISTR-UT    TO WS-IDDISTR                             
034700         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
034800       ELSE                                                               
034900         MOVE MID-IDDISTR-IN    TO WS-IDDISTR                             
035000         MOVE '7'       TO MFS-IDPFK                                      
035100         MOVE SPACE             TO MFS-KDTRTYP                            
035200       END-IF                                                             
035300     ELSE                                                                 
035400       MOVE ZERO                TO WS-IDDISTR                             
035500     END-IF                                                               
035600                                                                          
035700     IF WS-IDDISTR NUMERIC                                                
035800       MOVE WS-IDDISTR          TO MOD-IDDISTR-UT                         
035900       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
036000       IF WS-IDDISTR > ZERO                                               
036100         MOVE WS-IDDISTR        TO W-IDDISTR-MIN                          
036200                                   W-IDDISTR-MAX                          
036300       ELSE                                                               
036400         IF MID-IDDISTR-IN NOT = ALL '+'                                  
036500           MOVE NEJ             TO NYCKLAR-SW                             
036600         END-IF                                                           
036700       END-IF                                                             
036800     ELSE                                                                 
036900       MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                         
037000       MOVE NEJ                 TO NYCKLAR-SW                             
037100     END-IF                                                               
037200                                                                          
037300*    -- KONTROLL AV IDKUNDNR                                              
037400     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
037500                                                                          
037600     IF EGEN-MID                                                          
037700       IF MID-IDKUNDNR-IN = ALL '+'                                       
037800         MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                            
037900         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
038000       ELSE                                                               
038100         MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                            
038200         MOVE '7'               TO MFS-IDPFK                              
038300         MOVE SPACE             TO MFS-KDTRTYP                            
038400       END-IF                                                             
038500     ELSE                                                                 
038600       MOVE ZERO                TO WS-IDKUNDNR                            
038700     END-IF                                                               
038800                                                                          
038900     IF WS-IDKUNDNR NUMERIC                                               
039000       MOVE WS-IDKUNDNR         TO MOD-IDKUNDNR-UT                        
039100       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
039200       IF WS-IDKUNDNR > ZERO                                              
039300         IF W-IDDISTR-MIN > ZERO                                          
039400           MOVE WS-IDKUNDNR     TO W-IDKUNDNR-MIN                         
039500                                   W-IDKUNDNR-MAX                         
039600         ELSE                                                             
039700           MOVE NEJ             TO NYCKLAR-SW                             
039800         END-IF                                                           
039900       END-IF                                                             
040000     ELSE                                                                 
040100       MOVE MFS-RENSA-FAELT     TO MOD-IDKUNDNR-UT                        
040200       MOVE NEJ                 TO NYCKLAR-SW                             
040300     END-IF                                                               
040400                                                                          
040500*    -- KONTROLL AV KDANMORS                                              
040600     MOVE MFS-RENSA-FAELT       TO MOD-KDANMORS-IN                        
040700                                                                          
040800     IF EGEN-MID                                                          
040900       IF MID-KDANMORS-IN = ALL '+'                                       
041000         MOVE MID-KDANMORS-UT   TO WS-KDANMORS                            
041100       ELSE                                                               
041200         MOVE MID-KDANMORS-IN   TO WS-KDANMORS                            
041300         MOVE '7'               TO MFS-IDPFK                              
041400         MOVE SPACE             TO MFS-KDTRTYP                            
041500       END-IF                                                             
041600     ELSE                                                                 
041700       MOVE SPACE               TO WS-KDANMORS                            
041800     END-IF                                                               
041900                                                                          
042000     IF WS-KDANMORS NUMERIC                                               
042100       MOVE WS-KDANMORS         TO W-KDANMORS                             
042200       MOVE WS-KDANMORS         TO MOD-KDANMORS-UT                        
042300     ELSE                                                                 
042400       MOVE MFS-RENSA-FAELT     TO MOD-KDANMORS-UT                        
042500     END-IF                                                               
042600                                                                          
042700*    -- KONTROLL AV IDDC                                                  
042800     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
042900                                                                          
043000     IF EGEN-MID                                                          
043100       IF MID-IDDC-IN = ALL '+'                                           
043200         MOVE MID-IDDC-UT       TO W-IDDC                                 
043300         INSPECT W-IDDC REPLACING LEADING SPACE BY ZERO                   
043400       ELSE                                                               
043500         MOVE MID-IDDC-IN       TO W-IDDC                                 
043600         MOVE '7'               TO MFS-IDPFK                              
043700         MOVE SPACE             TO MFS-KDTRTYP                            
043800       END-IF                                                             
043900     ELSE                                                                 
044000       MOVE ZERO                TO W-IDDC                                 
044100     END-IF                                                               
044200                                                                          
044300     MOVE W-IDDC                TO MOD-IDDC-UT                            
044400     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
044500                                                                          
044600     IF GODK-MID OR NYCKLAR-OK                                            
044700       CONTINUE                                                           
044800     ELSE                                                                 
044900       MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-UT                         
045000                                   MOD-IDDISTR-UT                         
045100                                   MOD-IDKUNDNR-UT                        
045200                                   MOD-KDANMORS-UT                        
045300                                   MOD-IDDC-UT                            
045400     END-IF                                                               
045500                                                                          
045600     IF NYCKLAR-FEL                                                       
045700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045800       PERFORM MFS-RENSA-FAELT-IN                                         
045900       PERFORM MFS-RENSA-FAELT-UT                                         
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 C-FOERSTA-SIDA                 SECTION.                                  
046400                                                                          
046500     MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                           
046600                                                                          
046700     PERFORM MFS-RENSA-FAELT-IN                                           
046800     .                                                                    
046900     EJECT                                                                
047000 D-NAESTA-SIDA                  SECTION.                                  
047100                                                                          
047200     IF MID-IDDISTR-NEXT > ZERO                                           
047300       MOVE MID-IDDISTR-NEXT    TO W-IDDISTR-MIN                          
047400       MOVE MID-IDKUNDNR-NEXT   TO W-IDKUNDNR-MIN                         
047500       MOVE MID-IDRAPPNR-NEXT   TO W-IDRAPPNR-MIN                         
047600       MOVE MID-IDRADNR-NEXT    TO W-IDRADNR-MIN                          
047700     END-IF                                                               
047800     PERFORM MFS-RENSA-FAELT-IN                                           
047900     .                                                                    
048000     EJECT                                                                
048100 E-SAMMA-SIDA                   SECTION.                                  
048200                                                                          
048300     IF MID-INPUT = ALL '+'                                               
048400       MOVE NEJ                    TO P-TO-P-SW                           
048500       IF EGEN-MID OR HELP-MID                                            
048600         IF MID-IDDISTR-ENTER > ZERO                                      
048700           MOVE MID-IDDISTR-ENTER  TO W-IDDISTR-MIN                       
048800           MOVE MID-IDKUNDNR-ENTER TO W-IDKUNDNR-MIN                      
048900           MOVE MID-IDRAPPNR-ENTER TO W-IDRAPPNR-MIN                      
049000           MOVE MID-IDRADNR-ENTER  TO W-IDRADNR-MIN                       
049100         END-IF                                                           
049200         PERFORM MFS-RENSA-FAELT-IN                                       
049300       ELSE                                                               
049400         PERFORM MFS-RENSA-FAELT-IN                                       
049500       END-IF                                                             
049600     ELSE                                                                 
049700       PERFORM EA-HOPP-TILL-VALD-BILD                                     
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100 EA-HOPP-TILL-VALD-BILD         SECTION.                                  
050200                                                                          
050300     MOVE +1 TO INDX                                                      
050400     PERFORM UNTIL INDX > MAX-INDX                                        
050500       IF MID-KDCMDVAL       (INDX) NOT = ALL '+'                         
050600         IF MID-KDCMDVAL     (INDX) = 'LA ' OR 'DR '                      
050700           MOVE +70                 TO MSG-KVLL                           
050800           MOVE 'W4T712  '          TO MSG-KDTRANS-1                      
050900           MOVE '472D'              TO MSG-IDTRANS-1                      
051000           MOVE MFS-KDMFSFOR        TO MSG-KDMFSFOR-1                     
051100           MOVE MID-IDDISTR  (INDX) TO 4712-MID-IDDISTR-IN                
051200           INSPECT 4712-MID-IDDISTR-IN                                    
051300                   REPLACING LEADING SPACE BY ZERO                        
051400           MOVE MID-IDKUNDNR (INDX) TO 4712-MID-IDKUNDNR-IN               
051500           INSPECT 4712-MID-IDKUNDNR-IN                                   
051600                   REPLACING LEADING SPACE BY ZERO                        
051700           MOVE MID-IDRAPPNR (INDX) TO 4712-MID-IDRAPPNR-IN               
051800           INSPECT 4712-MID-IDRAPPNR-IN                                   
051900                   REPLACING LEADING SPACE BY ZERO                        
052000           MOVE ZERO                TO 4712-MID-IDARTNR-IN                
052100                                       4712-MID-IDRADNR-IN                
052200           MOVE MFS-RENSA-FAELT     TO 4712-MID-IDDISTR-UT                
052300                                       4712-MID-IDKUNDNR-UT               
052400                                       4712-MID-IDRAPPNR-UT               
052500                                       4712-MID-IDARTNR-UT                
052600                                       4712-MID-IDRADNR-UT                
052700                                                                          
052800           PERFORM IMS-INSERT-MSG-4712                                    
052900           MOVE JA                    TO P-TO-P-SW                        
053000           MOVE MAX-INDX TO INDX                                          
053100         ELSE                                                             
053200           IF MID-KDCMDVAL     (INDX) = 'RT ' OR 'RP '                    
053300             MOVE +70                 TO MSG-KVLL                         
053400             MOVE 'W4T715  '          TO MSG-KDTRANS-1                    
053500             MOVE '472D'              TO MSG-IDTRANS-1                    
053600             MOVE MFS-KDMFSFOR        TO MSG-KDMFSFOR-1                   
053700             MOVE MID-IDDISTR  (INDX) TO 4715-MID-IDDISTR-IN              
053800             INSPECT 4715-MID-IDDISTR-IN                                  
053900                     REPLACING LEADING SPACE BY ZERO                      
054000             MOVE MID-IDKUNDNR (INDX) TO 4715-MID-IDKUNDNR-IN             
054100             INSPECT 4715-MID-IDKUNDNR-IN                                 
054200                     REPLACING LEADING SPACE BY ZERO                      
054300             MOVE MID-IDRAPPNR (INDX) TO 4715-MID-IDRAPPNR-IN             
054400             INSPECT 4715-MID-IDRAPPNR-IN                                 
054500                     REPLACING LEADING SPACE BY ZERO                      
054600             MOVE ZERO                TO 4715-MID-IDARTNR-IN              
054700                                         4715-MID-IDRADNR-IN              
054800                                                                          
054900             MOVE MFS-RENSA-FAELT     TO 4715-MID-IDDISTR-UT              
055000                                         4715-MID-IDKUNDNR-UT             
055100                                         4715-MID-IDRAPPNR-UT             
055200                                         4715-MID-IDARTNR-UT              
055300                                         4715-MID-IDRADNR-UT              
055400                                                                          
055500             PERFORM IMS-INSERT-MSG-4715                                  
055600             MOVE JA                   TO P-TO-P-SW                       
055700             MOVE MAX-INDX TO INDX                                        
055800           ELSE                                                           
055900             IF MID-KDCMDVAL   (INDX) = 'TXT'                             
056000               MOVE +75               TO MSG-KVLL                         
056100               MOVE 'W4T723  '        TO MSG-KDTRANS-1                    
056200               MOVE '472D'            TO MSG-IDTRANS-1                    
056300               MOVE MFS-KDMFSFOR      TO MSG-KDMFSFOR-1                   
056400               MOVE MID-IDDISTR (INDX) TO 4723-MID-IDDISTR-IN             
056500               INSPECT 4723-MID-IDDISTR-IN                                
056600                       REPLACING LEADING SPACE BY ZERO                    
056700               MOVE MID-IDKUNDNR (INDX) TO 4723-MID-IDKUNDNR-IN           
056800               INSPECT 4723-MID-IDKUNDNR-IN                               
056900                       REPLACING LEADING SPACE BY ZERO                    
057000               MOVE MID-IDRAPPNR (INDX) TO 4723-MID-IDRAPPNR-IN           
057100               INSPECT 4723-MID-IDRAPPNR-IN                               
057200                       REPLACING LEADING SPACE BY ZERO                    
057300                                                                          
057400               MOVE SPAR-IDRADNR (INDX)  TO WS-IDRADNR                    
057500               MOVE WS-IDRADNR        TO 4723-MID-IDRADNR-IN              
057600                                                                          
057700               MOVE MSGI-IDARTNR      TO 4723-MID-IDARTNR-IN              
057800               MOVE MFS-RENSA-FAELT   TO 4723-MID-IDDISTR-UT              
057900                                         4723-MID-IDKUNDNR-UT             
058000                                         4723-MID-IDRAPPNR-UT             
058100                                         4723-MID-IDARTNR-UT              
058200                                         4723-MID-IDRADNR-UT              
058300                                                                          
058400               PERFORM IMS-INSERT-MSG-4723                                
058500               MOVE JA                 TO P-TO-P-SW                       
058600               MOVE MAX-INDX TO INDX                                      
058700             ELSE                                                         
058800               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
058900               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(INDX)         
059000               MOVE FEL                TO P-TO-P-SW                       
059100               MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL (INDX)              
059200             END-IF                                                       
059300           END-IF                                                         
059400         END-IF                                                           
059500       ELSE                                                               
059600         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL (INDX)                     
059700       END-IF                                                             
059800       ADD 1 TO INDX                                                      
059900     END-PERFORM                                                          
060000                                                                          
060100     IF HOPP-FEL                                                          
060200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 F-LAES-VISA-INFO               SECTION.                                  
060700                                                                          
060800     MOVE +1 TO INDX                                                      
060900     PERFORM FA-LAES-RADDATA                                              
061000                                                                          
061100     IF SEGMENT-SAKNAS                                                    
061200       MOVE ERR-KEY-MISSING     TO MED-IDMFSFEL                           
061300       PERFORM MFS-RENSA-FAELT-UT                                         
061400     ELSE                                                                 
061500       MOVE ANM-IDDISTR         TO MOD-IDDISTR-ENTER                      
061600       MOVE ANM-IDKUNDNR        TO MOD-IDKUNDNR-ENTER                     
061700       MOVE ANM-IDRAPPNR        TO MOD-IDRAPPNR-ENTER                     
061800       MOVE LEV-IDRADNR         TO MOD-IDRADNR-ENTER                      
061900                                                                          
062000       MOVE +1 TO INDX                                                    
062100       PERFORM UNTIL INDX > MAX-INDX                                      
062200         IF SEGMENT-FINNS                                                 
062300           PERFORM FB-RED-MOD-RAD                                         
062310           IF INDX <= MAX-INDX                                            
062400             PERFORM FA-LAES-RADDATA                                      
062410           END-IF                                                         
062500         ELSE                                                             
062600           PERFORM MFS-RENSA-RAD                                          
062700           ADD 1 TO INDX                                                  
062800         END-IF                                                           
062900       END-PERFORM                                                        
063000                                                                          
063100       IF SEGMENT-FINNS                                                   
063200         MOVE ANM-IDDISTR       TO MOD-IDDISTR-NEXT                       
063300         MOVE ANM-IDKUNDNR      TO MOD-IDKUNDNR-NEXT                      
063400         MOVE ANM-IDRAPPNR      TO MOD-IDRAPPNR-NEXT                      
063500         MOVE LEV-IDRADNR       TO MOD-IDRADNR-NEXT                       
063600                                                                          
063700         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
063800       ELSE                                                               
063900         MOVE ZERO              TO MOD-IDDISTR-NEXT                       
064000                                   MOD-IDKUNDNR-NEXT                      
064100                                   MOD-IDRAPPNR-NEXT                      
064200                                   MOD-IDRADNR-NEXT                       
064300       END-IF                                                             
064400                                                                          
064500       MOVE '002'      TO MSGI-KDCALL                                     
064600       MOVE '4724'     TO SPAR-IDTRANS                                    
064700       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
064800       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200 FA-LAES-RADDATA                SECTION.                                  
065300                                                                          
065400     MOVE MSGI-IDFTG            TO W-IDFTG-MIN                            
065500                                   W-IDFTG-MAX                            
065600     IF W-KDANMORS NUMERIC                                                
065700       PERFORM IMS-GET-KREI-KOD                                           
065800       PERFORM FC-KOLLA-BEHORIGHET                                        
065900     ELSE                                                                 
066000       PERFORM IMS-GET-KREI                                               
066100       PERFORM FD-KOLLA-BEHORIGHET                                        
066200     END-IF                                                               
066300                                                                          
066400     IF SEGMENT-FINNS                                                     
066500       MOVE SEQD-IDLEVANM       TO W-IDLEVANM-X                           
066600       PERFORM IMS-GU-KREE01                                              
066700                                                                          
066800       MOVE SEQD-IDARTNR        TO W-IDARTNR                              
066900       MOVE SEQD-IDRADNR        TO W-IDRADNR                              
067000                                   SPAR-IDRADNR (INDX)                    
067100       PERFORM IMS-GNP-KREE11                                             
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500 FB-RED-MOD-RAD                 SECTION.                                  
067600                                                                          
067700     IF W-IDDC NOT = ZERO                                                 
067800       IF LEV-IDDC = W-IDDC                                               
067900         PERFORM FBA-FLYTTA-RADDATA                                       
068000         ADD 1 TO INDX                                                    
068100       END-IF                                                             
068200     ELSE                                                                 
068300       PERFORM FBA-FLYTTA-RADDATA                                         
068400       ADD 1 TO INDX                                                      
068500     END-IF                                                               
068600     .                                                                    
068700     EJECT                                                                
068800 FBA-FLYTTA-RADDATA             SECTION.                                  
068900                                                                          
069000       MOVE MFS-RENSA-FAELT     TO MOD-KDCMDVAL      (INDX)               
069100       MOVE ANM-IDDISTR         TO MOD-IDDISTR       (INDX)               
069200       MOVE ANM-IDKUNDNR        TO MOD-IDKUNDNR      (INDX)               
069300       MOVE ANM-IDRAPPNR        TO MOD-IDRAPPNR      (INDX)               
069400       MOVE LEV-IDDC            TO MOD-IDDC          (INDX)               
069500       MOVE ANM-DALEVANM(3:6)   TO MOD-TILEVANM      (INDX)               
069600       MOVE LEV-KDANMORS        TO MOD-KDANMORS      (INDX)               
069700                                  OKOD-KDANMORS                           
069800       IF SWEDISH-TEXT                                                    
069900         IF LEV-KDKREBEH = 'Y  '                                          
070000           MOVE 'J  '           TO MOD-KDKREBEH      (INDX)               
070100         ELSE                                                             
070200           IF LEV-KDKREBEH (1:1) = 'C'                                    
070300             MOVE LEV-KDKREBEH  TO WS-KDKREBEH                            
070400             MOVE 'Ä'           TO WS-KDKREBEH (1:1)                      
070500             MOVE WS-KDKREBEH   TO MOD-KDKREBEH      (INDX)               
070600           ELSE                                                           
070700             MOVE LEV-KDKREBEH  TO MOD-KDKREBEH      (INDX)               
070800           END-IF                                                         
070900         END-IF                                                           
071000       ELSE                                                               
071100         MOVE LEV-KDKREBEH      TO MOD-KDKREBEH      (INDX)               
071200       END-IF                                                             
071300       MOVE LEV-KVLEVANM-BEKR   TO MOD-KVLEVANM      (INDX)               
071400                                                                          
071500       CALL W418OKOD USING OKOD-W418OKOD                                  
071600       IF (OKOD-FL-RETILL = 'J') OR                                       
071700          (OKOD-FL-INTERNUPPACKNING = 'J')                                
071800         IF ANM-DARETILL > 0                                              
071900           MOVE ANM-DARETILL(3:6) TO TMP1-YYMMDD                          
072000           MOVE ANM-DARETANK(3:6) TO TMP2-YYMMDD                          
072100           MOVE LEV-TIINLINL      TO TMP3-YYMMDD                          
072200           PERFORM WY2000Q1                                               
072300                                                                          
072400           IF TMP1-YYMMDD >= TMP2-YYMMDD AND                              
072500              TMP1-YYMMDD >= TMP3-YYMMDD                                  
072600             IF SWEDISH-TEXT                                              
072700               MOVE 'RT '       TO MOD-DATUM-TEXT    (INDX)               
072800             ELSE                                                         
072900               MOVE 'RP '       TO MOD-DATUM-TEXT    (INDX)               
073000             END-IF                                                       
073100             MOVE ANM-DARETILL(3:6)  TO MOD-TIRETILL      (INDX)          
073200           END-IF                                                         
073300                                                                          
073400           IF TMP2-YYMMDD >= TMP1-YYMMDD AND                              
073500              TMP2-YYMMDD >= TMP3-YYMMDD                                  
073600             IF SWEDISH-TEXT                                              
073700               MOVE 'MOT'       TO MOD-DATUM-TEXT    (INDX)               
073800             ELSE                                                         
073900               MOVE 'REC'       TO MOD-DATUM-TEXT    (INDX)               
074000             END-IF                                                       
074100             MOVE ANM-DARETANK(3:6)  TO MOD-TIRETILL      (INDX)          
074200           END-IF                                                         
074300                                                                          
074400           IF TMP3-YYMMDD >= TMP1-YYMMDD AND                              
074500              TMP3-YYMMDD >= TMP2-YYMMDD                                  
074600             IF SWEDISH-TEXT                                              
074700               MOVE 'INL'       TO MOD-DATUM-TEXT    (INDX)               
074800             ELSE                                                         
074900               MOVE 'BIN'       TO MOD-DATUM-TEXT    (INDX)               
075000             END-IF                                                       
075100             MOVE LEV-TIINLINL  TO MOD-TIRETILL      (INDX)               
075200           END-IF                                                         
075300         END-IF                                                           
075400       END-IF                                                             
075500                                                                          
075600       MOVE LEV-KVRETINL        TO MOD-KVRETINL      (INDX)               
075700       MOVE LEV-KVRETINL-SKR    TO MOD-KVRETINL-SKR  (INDX)               
075800       IF LEV-FLTEXT             = JA                                     
075900         IF MSGI-IDLAND-SPR = 'GB'                                        
076000           MOVE YES             TO MOD-FLTEXT        (INDX)               
076100         ELSE                                                             
076200           MOVE LEV-FLTEXT      TO MOD-FLTEXT        (INDX)               
076300         END-IF                                                           
076400       ELSE                                                               
076500         MOVE LEV-FLTEXT        TO MOD-FLTEXT        (INDX)               
076600       END-IF                                                             
076700     .                                                                    
076800     EJECT                                                                
076900 FC-KOLLA-BEHORIGHET SECTION.                                             
077000                                                                          
077100     MOVE 'F'                     TO SEC-KDSVAR                           
077200     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
077300                   SEGMENT-SAKNAS          OR                             
077400                   BASEN-SLUT                                             
077500*       MOVE 'RI00149 '           TO SEC-IDUSER                           
077600        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
077700        MOVE '4724'               TO SEC-IDTRANS                          
077800        MOVE SEQD-IDDISTR         TO W-IDDISTR-SEC                        
077900        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
078000                                                                          
078100        CALL WSECURIT USING SEC-IDUSER                                    
078200                            SEC-IDTRANS                                   
078300                            SEC-IDKEY                                     
078400                            SEC-KDSVAR                                    
078500                                                                          
078600        IF SEC-KDSVAR = 'F'                                               
078700           PERFORM IMS-GET-KREI-KOD                                       
078800        END-IF                                                            
078900     END-PERFORM                                                          
079000     .                                                                    
079100     EJECT                                                                
079200 FD-KOLLA-BEHORIGHET SECTION.                                             
079300                                                                          
079400     MOVE 'F'                     TO SEC-KDSVAR                           
079500     PERFORM UNTIL SEC-KDSVAR NOT = 'F'    OR                             
079600                   SEGMENT-SAKNAS          OR                             
079700                   BASEN-SLUT                                             
079800*       MOVE 'RI00149 '           TO SEC-IDUSER                           
079900        MOVE MSGI-IDUSER          TO SEC-IDUSER                           
080000        MOVE '4724'               TO SEC-IDTRANS                          
080100        MOVE SEQD-IDDISTR         TO W-IDDISTR-SEC                        
080200        MOVE W-IDDISTR-SEC        TO SEC-IDKEY                            
080300                                                                          
080400        CALL WSECURIT USING SEC-IDUSER                                    
080500                            SEC-IDTRANS                                   
080600                            SEC-IDKEY                                     
080700                            SEC-KDSVAR                                    
080800                                                                          
080900        IF SEC-KDSVAR = 'F'                                               
081000           PERFORM IMS-GET-KREI                                           
081100        END-IF                                                            
081200     END-PERFORM                                                          
081300     .                                                                    
081400     EJECT                                                                
081500 Z-FINIT                        SECTION.                                  
081600                                                                          
081700     IF EJ-BILD-BYTE OR HOPP-FEL                                          
081800                                                                          
081900       IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE            
082000         CALL WMEDKONV USING MED-WMEDAREA                                 
082100         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
082200         MOVE MED-MFSINF        TO MOD-TEMFSINF                           
082300       END-IF                                                             
082400                                                                          
082500       MOVE MAX-MOD-LAENGD      TO MSG-KVLL                               
082600       PERFORM IMS-INSERT-MSG                                             
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 MFS-RENSA-FAELT-UT             SECTION.                                  
083100                                                                          
083200*    --- ALLA UTDATA-FÄLT                                                 
083300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
083400     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-ENTER                      
083500                                   MOD-IDDISTR-NEXT                       
083600                                   MOD-IDKUNDNR-ENTER                     
083700                                   MOD-IDKUNDNR-NEXT                      
083800                                   MOD-IDRAPPNR-ENTER                     
083900                                   MOD-IDRAPPNR-NEXT                      
084000                                   MOD-IDRADNR-ENTER                      
084100                                   MOD-IDRADNR-NEXT                       
084200     MOVE +1 TO INDX                                                      
084300     PERFORM UNTIL INDX > MAX-INDX                                        
084400       PERFORM MFS-RENSA-RAD                                              
084500       ADD +1 TO INDX                                                     
084600     END-PERFORM                                                          
084700     .                                                                    
084800     EJECT                                                                
084900 MFS-RENSA-RAD                  SECTION.                                  
085000                                                                          
085100     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR       (INDX)               
085200                                   MOD-IDKUNDNR      (INDX)               
085300                                   MOD-IDRAPPNR      (INDX)               
085400                                   MOD-IDDC          (INDX)               
085500                                   MOD-TILEVANM      (INDX)               
085600                                   MOD-KDANMORS      (INDX)               
085700                                   MOD-KDKREBEH      (INDX)               
085800                                   MOD-KVLEVANM      (INDX)               
085900                                   MOD-DATUM-TEXT    (INDX)               
086000                                   MOD-TIRETILL      (INDX)               
086100                                   MOD-KVRETINL      (INDX)               
086200                                   MOD-KVRETINL-SKR  (INDX)               
086300                                   MOD-FLTEXT        (INDX)               
086400     MOVE MFS-STAENG-FAELT      TO MOD-KDCMDVAL-ATTR (INDX)               
086500     .                                                                    
086600                                                                          
086700 MFS-RENSA-FAELT-IN             SECTION.                                  
086800                                                                          
086900*    --- ALLA INDATA-FÄLT                                                 
087000     MOVE +1 TO INDX                                                      
087100     PERFORM UNTIL INDX > MAX-INDX                                        
087200       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL (INDX)                        
087300       ADD +1 TO INDX                                                     
087400     END-PERFORM                                                          
087500     .                                                                    
087600     EJECT                                                                
087700 MFS-ROER-EJ-FAELT-UT           SECTION.                                  
087800                                                                          
087900*    --- ALLA UTDATA-FÄLT                                                 
088000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
088100     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDISTR-ENTER                      
088200                                   MOD-IDDISTR-NEXT                       
088300                                   MOD-IDKUNDNR-ENTER                     
088400                                   MOD-IDKUNDNR-NEXT                      
088500                                   MOD-IDRAPPNR-ENTER                     
088600                                   MOD-IDRAPPNR-NEXT                      
088700                                   MOD-IDRADNR-ENTER                      
088800                                   MOD-IDRADNR-NEXT                       
088900     MOVE +1 TO INDX                                                      
089000     PERFORM UNTIL INDX > MAX-INDX                                        
089100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
089200       ADD +1 TO INDX                                                     
089300     END-PERFORM                                                          
089400     .                                                                    
089500     EJECT                                                                
089600 MFS-ROER-EJ-RAD-FAELT-UT       SECTION.                                  
089700                                                                          
089800     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDISTR       (INDX)               
089900                                   MOD-IDKUNDNR      (INDX)               
090000                                   MOD-IDRAPPNR      (INDX)               
090100                                   MOD-IDDC          (INDX)               
090200                                   MOD-TILEVANM      (INDX)               
090300                                   MOD-KDANMORS      (INDX)               
090400                                   MOD-KDKREBEH      (INDX)               
090500                                   MOD-KVLEVANM      (INDX)               
090600                                   MOD-DATUM-TEXT    (INDX)               
090700                                   MOD-TIRETILL      (INDX)               
090800                                   MOD-KVRETINL      (INDX)               
090900                                   MOD-KVRETINL-SKR  (INDX)               
091000                                   MOD-FLTEXT        (INDX)               
091100     .                                                                    
091200     EJECT                                                                
091300* --- IMS SEKTIONER ---                                                   
091400                                                                          
091500 IMS-GET-MSG                    SECTION.                                  
091600                                                                          
091700     MOVE '  QC' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200                                                                          
092300 IMS-INSERT-MSG                 SECTION.                                  
092400                                                                          
092500     IF MSGI-IDLAND-SPR = 'GB'                                            
092600       MOVE 'N' TO MFS-KDHUVOMR                                           
092700     END-IF                                                               
092800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092900     MOVE SPACE TO GODK-STATUSKODER                                       
093000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-INSERT-MSG-4712            SECTION.                                  
093600                                                                          
093700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093800     MOVE SPACE TO GODK-STATUSKODER                                       
093900     CALL CBLTDLI USING ISRT 4712-PCB MSG-IO-AREA                         
094000     MOVE 4712-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     .                                                                    
094300                                                                          
094400 IMS-INSERT-MSG-4715            SECTION.                                  
094500                                                                          
094600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094700     MOVE SPACE TO GODK-STATUSKODER                                       
094800     CALL CBLTDLI USING ISRT 4715-PCB MSG-IO-AREA                         
094900     MOVE 4715-STATUS-CODE TO STATUS-WS                                   
095000     PERFORM IMS-STATUSKONTROLL                                           
095100     .                                                                    
095200                                                                          
095300 IMS-INSERT-MSG-4723            SECTION.                                  
095400                                                                          
095500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095600     MOVE SPACE TO GODK-STATUSKODER                                       
095700     CALL CBLTDLI USING ISRT 4723-PCB MSG-IO-AREA                         
095800     MOVE 4723-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-GU-KREE01                  SECTION.                                  
096300                                                                          
096400     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
096500          DELIMITED BY SIZE INTO SSA1                                     
096600     MOVE '    ' TO GODK-STATUSKODER                                      
096700     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
096800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100                                                                          
097200 IMS-GNP-KREE11                 SECTION.                                  
097300                                                                          
097400     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
097500          DELIMITED BY SIZE INTO SSA1                                     
097600     MOVE '    ' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
097800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     EJECT                                                                
098200 IMS-GET-KREI-KOD               SECTION.                                  
098300                                                                          
098400     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2D1KY-MIN-X                        
098500                    '&WDA2D1KY<=' W-WDA2D1KY-MAX-X                        
098600                    '&KDANMORS =' W-KDANMORS-X ')'                        
098700          DELIMITED BY SIZE INTO SSA1                                     
098800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
098900     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA-WDA2D1 SSA1               
099000     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300                                                                          
099400 IMS-GET-KREI                   SECTION.                                  
099500                                                                          
099600     STRING 'WLKREI01(WDA2D1KY>=' W-WDA2D1KY-MIN-X                        
099700                    '&WDA2D1KY<=' W-WDA2D1KY-MAX-X ')'                    
099800          DELIMITED BY SIZE INTO SSA1                                     
099900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
100000     CALL CBLTDLI USING GN KREI-PCB DLI-IO-AREA-WDA2D1 SSA1               
100100     MOVE KREI-STATUS-CODE TO STATUS-WS                                   
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400                                                                          
100500 IMS-STATUSKONTROLL             SECTION.                                  
100600                                                                          
100700     SET STATUS-IX TO 1                                                   
100800     SEARCH GODK-STATUS                                                   
100900       AT END                                                             
101000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
101100         DELIMITED BY SIZE INTO FELTEXT                                   
101200         CALL FELLOG                                                      
101300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
101400         CONTINUE                                                         
101500     END-SEARCH                                                           
101600     .                                                                    
101700     EJECT                                                                
101800*    -COPY WY2000Q1                                                       
