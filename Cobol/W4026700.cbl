000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026700.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   91/05/06.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MPP PROGRAM SOM:                                                 
001100*        - GÖR ÄNDRINGAR AV KOMPLETTERINGSINFO I ETT BEFINTLIGT           
001200*          PROFORMAHUVUD                                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER OCH UPPDATERAR :                                
001500*                           - WLPROC (WDE8) PROFORMA HUVUD                
001600*                                                                         
001700*        PROGRAMMET LÄSER : - WLPROD (WDE9) PROFORMA RADER                
001800*                                                                         
001900*        PROGRAMMET LÄSER : - WLXXIE (WDG2) VALUTAREGISTER                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W4T267  W4T267U                                     
002300*        MID:         W4I26701                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W4O26701                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W4026700'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 77  UPPDAT-TID                  PIC 9(8)    VALUE ZERO.                  
004600 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  WS-INDEX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +515 COMP SYNC.        
004900                                                                          
005000 77  WS-KONSTANT                 PIC S9V9(3) VALUE +1.002  COMP-3.        
005100 77  WS-SUMMA                    PIC S9(9)V9(4) VALUE ZERO COMP-3.        
005200 77  WS-PREMBHNT                 PIC S9(7)V9(4) VALUE ZERO COMP-3.        
005300 77  WS-REAVDRAG                 PIC S9(2)V9(4) VALUE ZERO COMP-3.        
005400 77  WS-PRAVDRAG                 PIC S9(7)V9(5) VALUE ZERO COMP-3.        
005500 77  WS-REFOERS                  PIC S9(2)V9(5) VALUE ZERO COMP-3.        
005600 77  WS-PRFOERS                  PIC S9(7)V9(5) VALUE ZERO COMP-3.        
005700 77  WS-SUFKTBEL                 PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005800 77  WS-PRKURS                   PIC S9(6)V9(5) VALUE ZERO.               
005810 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005820 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005900                                                                          
006000*    -ARBETSETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
006200 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
006300 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
006400 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006500 01  WS-IDKUNDRF-RED.                                                     
006600     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
006700     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007400     88  INDATA-OK                           VALUE 'J'.                   
007500     88  INDATA-FEL                          VALUE 'N'.                   
007600                                                                          
007700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007800     88  ALLT-OK                             VALUE 'J'.                   
007900     88  ALLT-FEL                            VALUE 'N'.                   
008000                                                                          
008100 77  OTILL-UPPDAT-SW             PIC X       VALUE 'J'.                   
008200     88  OTILL-UPPDATERING                   VALUE 'N'.                   
008300                                                                          
008400 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
008500     88  UPPDATERING-OK                      VALUE 'J'.                   
008600     88  UPPDATERING-EJ                      VALUE 'N'.                   
008700                                                                          
008800 77  FLAGGA-FOERS                PIC X       VALUE 'N'.                   
008900     88  FOERS-OK                            VALUE 'J'.                   
009000                                                                          
009100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009200     88  EGEN-MID                            VALUE '4267'.                
009300     88  GODK-MID                            VALUE '4262' '4263'          
009400                                                   '4264' '4265'          
009500                                                   '4266' '4267'          
009600                                                   '4268' '4269'.         
009700     EJECT                                                                
009800*    --- SPAR AREA                                                        
009900                                                                          
010000 01  SPAR-AREA.                                                           
010100     03  SPAR-SUMMA              PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010200     03  SPAR-SUORDV-LOC         PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010300     03  SPAR-SUORDV-LOCPREL     PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010400     03  SPAR-SUORDV             PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010500     03  SPAR-SUFOBV             PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010600     03  SPAR-SUFKTBEL           PIC S9(9)V9(2) VALUE ZERO COMP-3.        
010700     03  SPAR-PRMOMS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
010800     03  SPAR-PREMBHNT           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
010900     03  SPAR-REEMBHNT           PIC S9(2)V9(1) VALUE ZERO COMP-3.        
011000     03  SPAR-PRAVDRAG           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011100     03  SPAR-REAVDRAG           PIC S9(2)V9    VALUE ZERO COMP-3.        
011200     03  SPAR-PRFRAKT            PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011300     03  SPAR-PRFOERS            PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011400     03  SPAR-REFOERS            PIC S9(2)V9(3) VALUE ZERO COMP-3.        
011500     03  SPAR-REOVKOFF           PIC S9(2)V9    VALUE ZERO COMP-3.        
011600     03  SPAR-PRLEGKST           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011700     03  SPAR-KDVALUTA           PIC S9(3)      VALUE ZERO COMP-3.        
011800     03  SPAR-PRKURS             PIC S9(6)V9(5) VALUE ZERO COMP-3.        
011900     03  SPAR-SUFKTBEL-UTL       PIC S9(9)V9(4) VALUE ZERO COMP-3.        
012000                                                                          
012100     03  SPAR-KDVALUTA-MID       PIC S9(3)      VALUE ZERO COMP-3.        
012200     EJECT                                                                
012300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012400 01  GENERELLA-SUBPROGRAM.                                                
012500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013000     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
013010     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013300*   -COPY WMEDAREA                                                        
013400     EJECT                                                                
013500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013600*   -COPY WMSGINIT                                                        
013700     EJECT                                                                
013800*    -PARAMETRAR TILL SUBPGM  W411EXCH                                    
013900*   -COPY W411EXCH                                                        
013910*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
013920*   -COPY W510CURR                                                        
013930     EJECT                                                                
014000     SKIP3                                                                
014100 01  FELM-CODES.                                                          
014200     03  FILLER                  PIC X(16)   VALUE 'FELM AREA'.           
014300     03  FELM-KOR-UPPLYSTA-FAELT PIC X(3)    VALUE '001'.                 
014400     03  FELM-OTILL-UPPDATERING  PIC X(3)    VALUE '007'.                 
014500     03  FELM-FINNS-EJ           PIC X(3)    VALUE '010'.                 
014600     03  FELM-RADER-SAKNAS       PIC X(3)    VALUE '029'.                 
014700     03  FELM-ORDERN-ANNULLERAD  PIC X(3)    VALUE '052'.                 
014800     03  FELM-FEL-NYCKEL         PIC X(3)    VALUE '401'.                 
014900     03  FELM-OBEHOERIG          PIC X(3)    VALUE '405'.                 
015000     SKIP3                                                                
015100 01  MESSAGE-CODES.                                                       
015200     03  FILLER                  PIC X(16)   VALUE 'INFO AREA'.           
015300     03  INFO-TRYCK-PF11         PIC X(3)    VALUE '003'.                 
015400     03  INFO-UPPDAT-GJORD       PIC X(3)    VALUE '101'.                 
015500     EJECT                                                                
015600*01  -COPY WDECAREA                                                       
015700     EJECT                                                                
015800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016100     SKIP3                                                                
016200*01  MID -COPY W4I26701                                                   
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016500     SKIP3                                                                
016600*01  -COPY WMSGAREA                                                       
016700     EJECT                                                                
016800     03  MOD REDEFINES MSG-AREA.                                          
016900*      05  -COPY W4O26701                                                 
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017200     SKIP3                                                                
017300*01  -COPY WMFSAREA                                                       
017400     EJECT                                                                
017500 01  TEST-IDDISTR                PIC S9(5)              COMP-3.           
017600 01  FILLER REDEFINES TEST-IDDISTR.                                       
017700*    03   -COPY WWDIST03.                                                 
017800 01  FILLER REDEFINES TEST-IDDISTR.                                       
017900*    ----DIST79-DEALER-PRICE-----                                         
018000*    03   -COPY WWDIST79                                                  
018100     EJECT                                                                
018200*01  -COPY W475CONS         -PRE CONS-                                    
018300     EJECT                                                                
018400*01  -COPY W930VAL                                                        
018500     EJECT                                                                
018600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018800     SKIP3                                                                
018900*    --- STATUS-KOD FRÅN IMS                                              
019000 01  STATUS-WS                   PIC XX.                                  
019100     88  SEGMENT-FINNS                       VALUE '  '.                  
019200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP3                                                                
019800 01  NYCKLAR-TILL-DLI.                                                    
019900*--------------------WDE8                                                 
020000     03  W-WDE801KY-X.                                                    
020100         05  W-PHUV-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
020200         05  W-PHUV-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
020300         05  W-PHUV-IDKUNDRF.                                             
020400            07 W-PHUV-IDORDNR7   PIC 9(7)     VALUE ZERO.                 
020500            07 FILLER            PIC X(3)     VALUE SPACE.                
020600                                                                          
020700*--------------------WDE9                                                 
020800     03  W-WDE901KY-MIN-X.                                                
020900         05  W-PRAD-IDORDER-MIN  PIC S9(7)    VALUE ZERO COMP-3.          
021000         05  W-PRAD-IDARTNR-MIN  PIC S9(9)    VALUE ZERO COMP-3.          
021100         05  W-PRAD-IDLOPNR-MIN  PIC S9(3)    VALUE ZERO COMP-3.          
021200                                                                          
021300     03  W-WDE901KY-MAX-X.                                                
021400         05  W-PRAD-IDORDER-MAX  PIC S9(7)    VALUE ZERO COMP-3.          
021500         05  W-PRAD-IDARTNR-MAX  PIC S9(9)    VALUE +999999999            
021600                                                         COMP-3.          
021700         05  W-PRAD-IDLOPNR-MAX  PIC S9(3)    VALUE +999 COMP-3.          
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(94).                               
023000 01  SSA2                        PIC X(94).                               
023100     EJECT                                                                
023200*    --- IMS FUNKTIONSKODER                                               
023300*01  -COPY W0003                                                          
023400     EJECT                                                                
023500*    ---  DLI INPUT-OUTPUT AREA                                           
023600                                                                          
023700*---------------IO-AREA FÖR BARA LÄSNING                                  
023800 01  DLI-IO-AREA.                                                         
023900     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
024000     SKIP3                                                                
024100 01  FILLER                      PIC X(16)   VALUE                        
024200                                              'IO-WDE801'.                
024300 01  DLI-IO-AREA-WDE801.                                                  
024400     03  WLPROC01.                                                        
024500*        05  -COPY WDE801                                                 
024600     EJECT                                                                
025600     EJECT                                                                
025700 LINKAGE SECTION.                                                         
025800                                                                          
025900*01  -COPY W0009      -PRE MSG-                                           
026000     EJECT                                                                
026100*01  -COPY W0008      -PRE USEA-                                          
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400*01  -COPY W0008      -PRE PROC-                                          
026500     05  FILLER                  PIC X.                                   
026600     EJECT                                                                
026700*01  -COPY W0008      -PRE PROD-                                          
026800     05  FILLER                  PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008      -PRE WDG2-                                          
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
027400                                     PROC-PCB   PROD-PCB                  
027500                                     WDG2-PCB.                            
027600     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
027700                                     PROC-PCB   PROD-PCB                  
027800                                     WDG2-PCB.                            
027900     PERFORM IMS-GET-MSG                                                  
028000     IF SEGMENT-FINNS                                                     
028100       PERFORM A-INIT                                                     
028200       PERFORM B-KOLLA-NYCKLAR                                            
028300                                                                          
028400       IF NYCKLAR-OK                                                      
028500         PERFORM IMS-GHU-PROC-WDE801-PHUV                                 
028600                                                                          
028700         IF SEGMENT-FINNS                                                 
028800           PERFORM C-KOLLA-OM-RAETT-PHUV                                  
028900                                                                          
029000           IF ALLT-OK                                                     
029100             IF MFS-UPDATE                                                
029200               PERFORM D-UPPDATERA                                        
029300             ELSE                                                         
029400               PERFORM E-BEHANDLA-ENTER                                   
029500             END-IF                                                       
029600                                                                          
029700             PERFORM F-LAES-VISA-BILD                                     
029800           END-IF                                                         
029900         ELSE                                                             
030000           MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                             
030100           CALL WMEDKONV USING MED-WMEDAREA                               
030200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
030300         END-IF                                                           
030400       END-IF                                                             
030500                                                                          
030600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
030700       PERFORM IMS-INSERT-MSG                                             
030800     END-IF                                                               
030900                                                                          
031000     MOVE ZERO TO RETURN-CODE                                             
031100     GOBACK                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 A-INIT SECTION.                                                          
031500                                                                          
031600     IF MSG-DUBBLA-TRANSKODER                                             
031700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26701                 
031800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
031900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032000     ELSE                                                                 
032100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26701                  
032200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032400     END-IF                                                               
032500                                                                          
032600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
032700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
032800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032900                                                                          
033000     MOVE LOW-VALUE TO MSG-AREA                                           
033100     MOVE 'W4O26701' TO MFS-IDMOD                                         
033200     MOVE '4267' TO MOD-IDTRANS                                           
033300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033400                                                                          
033500     ACCEPT DAGENS-DATUM FROM DATE                                        
033600     ACCEPT UPPDAT-TID   FROM TIME                                        
033700                                                                          
033800     IF NOT EGEN-MID                                                      
033900       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
034000                               MID-IDKUNDNR-IN                            
034100                               MID-IDORDNR7-IN                            
034200                               MID-IDARTNR-IN                             
034300                               MID-IDDISTR-UT                             
034400                               MID-IDKUNDNR-UT                            
034500                               MID-IDORDNR7-UT                            
034600                               MID-IDARTNR-UT                             
034700       MOVE SPACE TO MFS-KDTRTYP                                          
034800       MOVE ' ' TO MFS-IDPFK                                              
034900       PERFORM MFS-RENSA-FAELT-IN                                         
035000     END-IF                                                               
035100                                                                          
035200     IF ENGLISH-TEXT                                                      
035300       MOVE +2 TO SPRAK-IX                                                
035400       MOVE 'GB ' TO MED-IDSKYLT                                          
035500     ELSE                                                                 
035600       MOVE +1 TO SPRAK-IX                                                
035700       MOVE 'S  ' TO MED-IDSKYLT                                          
035800     END-IF                                                               
035900     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
035910     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
036000     .                                                                    
036100     EJECT                                                                
036200 B-KOLLA-NYCKLAR SECTION.                                                 
036300                                                                          
036400     MOVE JA TO NYCKLAR-SW                                                
036500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036600     MOVE '001'             TO MSGI-KDCALL                                
036700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036800     MOVE '4267'            TO MSGI-IDTRANS                               
036900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037000     IF MFS-IDTRANS = '4267'                                              
037100        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
037200        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
037300                                                                          
037400        MOVE MID-IDORDNR7-IN TO WS-IDKUNDRF-1-7                           
037500        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
037600          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
037700        ELSE                                                              
037800          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
037900        END-IF                                                            
038000        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
038100        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
038200     END-IF                                                               
038300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038400                                                                          
038500     PERFORM BA-KOLLA-DISTRIKT                                            
038600     PERFORM BB-KOLLA-KUNDNR                                              
038700     PERFORM BC-KOLLA-ORDER                                               
038800                                                                          
038900                                                                          
039000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
039100                                                                          
039200                                                                          
039300     IF NYCKLAR-FEL                                                       
039400       MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                               
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039700       PERFORM MFS-RENSA-FAELT-IN                                         
039800       PERFORM MFS-RENSA-FAELT-UT                                         
039900                                                                          
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 BA-KOLLA-DISTRIKT SECTION.                                               
040400                                                                          
040500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
040600                                                                          
040700     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
040800     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
040900                                                                          
041000     IF MID-IDDISTR-IN = ALL '+'                                          
041100       CONTINUE                                                           
041200     ELSE                                                                 
041300       MOVE ' '         TO MFS-IDPFK                                      
041400       MOVE SPACE       TO MFS-KDTRTYP                                    
041500     END-IF                                                               
041600                                                                          
041700     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
041800     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
041900                                                                          
042000     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
042100       MOVE WS-IDDISTR TO W-PHUV-IDDISTR                                  
042200     ELSE                                                                 
042300       MOVE NEJ TO NYCKLAR-SW                                             
042400     END-IF                                                               
042500                                                                          
042600     MOVE WS-IDDISTR   TO TEST-IDDISTR                                    
042700     IF DIST79-DEALER-PRICE                                               
042800       IF ENGLISH-TEXT                                                    
042900        MOVE 'DEALERPRICE'   TO MOD-TEDDI                                 
043000       ELSE                                                               
043100        MOVE '    ÅF PRIS'   TO MOD-TEDDI                                 
043200       END-IF                                                             
043300     ELSE                                                                 
044100       MOVE SPACE            TO MOD-TEDDI                                 
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 BB-KOLLA-KUNDNR SECTION.                                                 
044700                                                                          
044800     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
044900                                                                          
045000     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
045100     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
045200                                                                          
045300     IF MID-IDKUNDNR-IN = ALL '+'                                         
045400       CONTINUE                                                           
045500     ELSE                                                                 
045600       MOVE ' '         TO MFS-IDPFK                                      
045700       MOVE SPACE       TO MFS-KDTRTYP                                    
045800     END-IF                                                               
045900                                                                          
046000     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
046100     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
046200     IF MOD-IDKUNDNR-UT = SPACE                                           
046300       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
046400     END-IF                                                               
046500                                                                          
046600     IF WS-IDKUNDNR NUMERIC                                               
046700       MOVE WS-IDKUNDNR TO W-PHUV-IDKUNDNR                                
046800     ELSE                                                                 
046900       MOVE NEJ TO NYCKLAR-SW                                             
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 BC-KOLLA-ORDER SECTION.                                                  
047400                                                                          
047500     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-IN                              
047600                                                                          
047700     MOVE MSGI-IDKUNDRF    TO WS-IDORDNR7                                 
047800     INSPECT WS-IDORDNR7 REPLACING LEADING SPACE BY ZERO                  
047900                                                                          
048000     IF MID-IDORDNR7-IN = ALL '+'                                         
048100       CONTINUE                                                           
048200     ELSE                                                                 
048300       MOVE ' '         TO MFS-IDPFK                                      
048400       MOVE SPACE       TO MFS-KDTRTYP                                    
048500     END-IF                                                               
048600                                                                          
048700     MOVE WS-IDORDNR7 TO MOD-IDORDNR7-UT                                  
048800     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
048900                                                                          
049000     IF WS-IDORDNR7 NUMERIC                                               
049100       MOVE WS-IDORDNR7 TO W-PHUV-IDORDNR7                                
049200     ELSE                                                                 
049300       MOVE NEJ TO NYCKLAR-SW                                             
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 C-KOLLA-OM-RAETT-PHUV SECTION.                                           
049800                                                                          
049900     MOVE JA TO ALLT-SW                                                   
050000                                                                          
050100     IF PHUV-FLBORT    = 'N'                                              
050200                                                                          
050300       IF MFS-UPDATE                                                      
050400         MOVE PHUV-TIFORDAT   TO TMP1-YYMMDD                              
050500         MOVE DAGENS-DATUM    TO TMP2-YYMMDD                              
050600         PERFORM WY2000P1                                                 
050700         IF TMP1-YYMMDD > TMP2-YYMMDD AND                                 
050800            PHUV-TIORDDAT = ZERO                                          
050900                                                                          
051000           PERFORM S01-KOLLA-BEHOERIGHET                                  
051100           PERFORM S02-KOLLA-OM-PRAD-FINNS                                
051200                                                                          
051300         ELSE                                                             
051400           PERFORM CA-VISA-OTILL-UPPDATERING                              
051500         END-IF                                                           
051600                                                                          
051700       ELSE                                                               
051800         PERFORM S01-KOLLA-BEHOERIGHET                                    
051900         PERFORM S02-KOLLA-OM-PRAD-FINNS                                  
052000       END-IF                                                             
052100                                                                          
052200     ELSE                                                                 
052300       MOVE FELM-ORDERN-ANNULLERAD TO MED-IDMFSFEL                        
052400       CALL WMEDKONV USING MED-WMEDAREA                                   
052500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
052600       MOVE NEJ TO ALLT-SW                                                
052700     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 CA-VISA-OTILL-UPPDATERING SECTION.                                       
053100                                                                          
053200     MOVE NEJ TO OTILL-UPPDAT-SW                                          
053300     MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                          
053400     CALL WMEDKONV USING MED-WMEDAREA                                     
053500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
053600     .                                                                    
053700     EJECT                                                                
053800 D-UPPDATERA SECTION.                                                     
053900                                                                          
054000     IF NOT OTILL-UPPDATERING                                             
054100       IF MID-INPUT NOT = ALL '+'                                         
054200                                                                          
054300         PERFORM DA-SPARA-FAELT-FOR-BERAEKNING                            
054400         PERFORM DB-KOLLA-INPUT                                           
054500                                                                          
054600         IF INDATA-FEL                                                    
054700           MOVE FELM-KOR-UPPLYSTA-FAELT TO MED-IDMFSFEL                   
054800           CALL WMEDKONV USING MED-WMEDAREA                               
054900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
055000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
055100           MOVE NEJ TO UPPDAT-SW                                          
055200                                                                          
055300         ELSE                                                             
055400           PERFORM DC-AENDRA-I-DB                                         
055500           MOVE JA TO UPPDAT-SW                                           
055600         END-IF                                                           
055700                                                                          
055800       ELSE                                                               
055900         PERFORM DD-BERAEKNA-NYA-VARDEN-PA-WDE8                           
056000                                                                          
056100       END-IF                                                             
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 DA-SPARA-FAELT-FOR-BERAEKNING SECTION.                                   
056600                                                                          
056700     IF DIST79-DEALER-PRICE                                               
056800       MOVE PHUV-SUORDV-LOC     TO SPAR-SUORDV-LOC                        
056900       MOVE PHUV-SUORDV-LOCPREL TO SPAR-SUORDV-LOCPREL                    
057000       MOVE PHUV-PRKURS         TO SPAR-PRKURS                            
057100       MOVE PHUV-KDVALUTA       TO SPAR-KDVALUTA                          
057200     ELSE                                                                 
057300       MOVE PHUV-SUORDV    TO SPAR-SUORDV                                 
057400       MOVE PHUV-PREMBHNT  TO SPAR-PREMBHNT                               
057500       MOVE PHUV-REEMBHNT  TO SPAR-REEMBHNT                               
057600       MOVE PHUV-PRAVDRAG  TO SPAR-PRAVDRAG                               
057700       MOVE PHUV-REAVDRAG  TO SPAR-REAVDRAG                               
057800       MOVE PHUV-PRFRAKT   TO SPAR-PRFRAKT                                
057900       MOVE PHUV-PRFOERS   TO SPAR-PRFOERS                                
058000       MOVE PHUV-REFOERS   TO SPAR-REFOERS                                
058100       MOVE PHUV-REOVKOFF  TO SPAR-REOVKOFF                               
058200       MOVE PHUV-PRLEGKST  TO SPAR-PRLEGKST                               
058300       MOVE PHUV-KDVALUTA  TO SPAR-KDVALUTA                               
058400       MOVE PHUV-PRKURS    TO SPAR-PRKURS                                 
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800 DB-KOLLA-INPUT SECTION.                                                  
058900                                                                          
059000     MOVE JA TO INDATA-SW                                                 
059100                                                                          
059200     IF DIST79-DEALER-PRICE                                               
059300       CONTINUE                                                           
059400     ELSE                                                                 
059500       PERFORM DBA-KOLLA-PRAVDRAG-REAVDRAG                                
059600                                                                          
059700       PERFORM DBB-KOLLA-PRFRAKT                                          
059800                                                                          
059900       PERFORM DBC-KOLLA-PRFOERS-REFOERS                                  
060000                                                                          
060100       PERFORM DBD-KOLLA-REOVKOFF                                         
060200                                                                          
060300       PERFORM DBE-KOLLA-PRLEGKST                                         
060400                                                                          
060500       PERFORM DBF-KOLLA-KDVALUTA                                         
060600                                                                          
060700       PERFORM DBG-KOLLA-PRKURS                                           
060800     END-IF                                                               
060900                                                                          
061000     IF PHUV-BELOSORT = SPACE AND                                         
061100        MID-BELOSORT  = ALL '+'                                           
061200       MOVE NEJ                    TO INDATA-SW                           
061300       MOVE MFS-ALFA-FAELT-FEL     TO MOD-BELOSORT-ATTR                   
061400     ELSE                                                                 
061500       IF MID-BELOSORT NOT = ALL '+'                                      
061600         MOVE MID-BELOSORT         TO MOD-BELOSORT                        
061700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELOSORT-ATTR                   
061800       END-IF                                                             
061900     END-IF                                                               
062000                                                                          
062100     IF MID-TEBANKTO NOT = ALL '+'                                        
062200       IF MID-TEBANK-RAD1  = ALL '+' AND                                  
062300          MID-TEBANK-RAD2  = ALL '+' AND                                  
062400          PHUV-TEBANK(1)   = SPACE   AND                                  
062500          PHUV-TEBANK(2)   = SPACE                                        
062600         MOVE NEJ                  TO INDATA-SW                           
062700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANKTO-ATTR                   
062800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-TEBANK-RAD1-ATTR                
062900                                      MOD-TEBANK-RAD2-ATTR                
063000       ELSE                                                               
063100         MOVE MID-TEBANKTO         TO MOD-TEBANKTO                        
063200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANKTO-ATTR                   
063300       END-IF                                                             
063400     END-IF                                                               
063500                                                                          
063600     IF MID-TEBANK-RAD1 NOT = ALL '+'                                     
063700       IF MID-TEBANKTO  = ALL '+' AND                                     
063800          PHUV-TEBANKTO = SPACE                                           
063900         MOVE NEJ                  TO INDATA-SW                           
064000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANK-RAD1-ATTR                
064100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-TEBANKTO-ATTR                   
064200       ELSE                                                               
064300         MOVE MID-TEBANK-RAD1      TO MOD-TEBANK-RAD1                     
064400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANK-RAD1-ATTR                
064500       END-IF                                                             
064600     END-IF                                                               
064700                                                                          
064800     IF MID-TEBANK-RAD2 NOT = ALL '+'                                     
064900       IF MID-TEBANKTO  = ALL '+' AND                                     
065000          PHUV-TEBANKTO = SPACE                                           
065100         MOVE NEJ                  TO INDATA-SW                           
065200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANK-RAD2-ATTR                
065300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-TEBANKTO-ATTR                   
065400       ELSE                                                               
065500         MOVE MID-TEBANK-RAD2      TO MOD-TEBANK-RAD2                     
065600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEBANK-RAD2-ATTR                
065700       END-IF                                                             
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100 DBA-KOLLA-PRAVDRAG-REAVDRAG SECTION.                                     
066200                                                                          
066300     IF MID-PRAVDRAG NOT = ALL '+'                                        
066400       IF MID-REAVDRAG = ALL '+'                                          
066500         MOVE MID-PRAVDRAG TO DEC-IDFRIDATA                               
066600         MOVE +7           TO DEC-KVHELTAL                                
066700         MOVE +2           TO DEC-KVDECIMAL                               
066800         CALL WDECEDIT USING DEC-WDECAREA                                 
066900         IF DEC-KDSVAR-OK                                                 
067000           MOVE DEC-IDEDITDATA       TO SPAR-PRAVDRAG                     
067100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRAVDRAG-ATTR                 
067200           PERFORM DBAA-BERAEKNA-NY-REAVDRAG                              
067300         ELSE                                                             
067400           MOVE NEJ               TO INDATA-SW                            
067500           MOVE MFS-NUM-FAELT-FEL TO MOD-PRAVDRAG-ATTR                    
067600         END-IF                                                           
067700       ELSE                                                               
067800         MOVE NEJ                 TO INDATA-SW                            
067900         MOVE MFS-NUM-FAELT-FEL   TO MOD-PRAVDRAG-ATTR                    
068000         MOVE MFS-NUM-FAELT-FEL   TO MOD-REAVDRAG-ATTR                    
068100       END-IF                                                             
068200                                                                          
068300     ELSE                                                                 
068400       IF MID-REAVDRAG NOT = ALL '+'                                      
068500         MOVE MID-REAVDRAG TO DEC-IDFRIDATA                               
068600         MOVE +2           TO DEC-KVHELTAL                                
068700         MOVE +1           TO DEC-KVDECIMAL                               
068800         CALL WDECEDIT USING DEC-WDECAREA                                 
068900         IF DEC-KDSVAR-OK                                                 
069000           MOVE DEC-IDEDITDATA       TO SPAR-REAVDRAG                     
069100           MOVE MFS-NUM-FAELT-RAETT  TO MOD-REAVDRAG-ATTR                 
069200           PERFORM DBAB-BERAEKNA-NY-PRAVDRAG                              
069300         ELSE                                                             
069400           MOVE NEJ               TO INDATA-SW                            
069500           MOVE MFS-NUM-FAELT-FEL TO MOD-REAVDRAG-ATTR                    
069600         END-IF                                                           
069700       END-IF                                                             
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 DBAA-BERAEKNA-NY-REAVDRAG SECTION.                                       
070200                                                                          
070300     IF SPAR-SUORDV   NOT = ZERO                                          
070400        COMPUTE WS-REAVDRAG = (SPAR-PRAVDRAG /                            
070500                               SPAR-SUORDV)  *                            
070600                               100                                        
070700     ELSE                                                                 
070800        COMPUTE WS-REAVDRAG = SPAR-REAVDRAG                               
070900     END-IF                                                               
071000                                                                          
071100     COMPUTE SPAR-REAVDRAG ROUNDED = WS-REAVDRAG                          
071200     .                                                                    
071300     EJECT                                                                
071400 DBAB-BERAEKNA-NY-PRAVDRAG SECTION.                                       
071500                                                                          
071600     COMPUTE   WS-PRAVDRAG = (SPAR-SUORDV    *                            
071700                              SPAR-REAVDRAG) /                            
071800                              100                                         
071900                                                                          
072000     COMPUTE SPAR-PRAVDRAG ROUNDED = WS-PRAVDRAG                          
072100     .                                                                    
072200     EJECT                                                                
072300 DBB-KOLLA-PRFRAKT SECTION.                                               
072400                                                                          
072500     IF MID-PRFRAKT NOT = ALL '+'                                         
072600       MOVE MID-PRFRAKT TO DEC-IDFRIDATA                                  
072700       MOVE +7           TO DEC-KVHELTAL                                  
072800       MOVE +2           TO DEC-KVDECIMAL                                 
072900       CALL WDECEDIT USING DEC-WDECAREA                                   
073000       IF DEC-KDSVAR-OK                                                   
073100         MOVE DEC-IDEDITDATA       TO SPAR-PRFRAKT                        
073200         MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRFRAKT-ATTR                    
073300                                                                          
073400         IF MID-PRFOERS = ALL '+' AND                                     
073500            MID-REFOERS = ALL '+' AND                                     
073600            PHUV-REFOERS > ZERO                                           
073700           PERFORM S03-BERAEKNA-NY-PRFOERS                                
073800         END-IF                                                           
073900                                                                          
074000       ELSE                                                               
074100         MOVE NEJ                 TO INDATA-SW                            
074200         MOVE MFS-NUM-FAELT-FEL   TO MOD-PRFRAKT-ATTR                     
074300       END-IF                                                             
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 DBC-KOLLA-PRFOERS-REFOERS SECTION.                                       
074800                                                                          
074900     IF MID-REFOERS NOT = ALL '+'                                         
075000       IF MID-PRFOERS = ALL '+'                                           
075100         IF MID-REOVKOFF NOT = ALL '+' OR                                 
075200            PHUV-REOVKOFF > ZERO                                          
075300           PERFORM DBCA-KOLLA-OM-RAETT-REFOERS                            
075400           PERFORM DBCB-KOLLA-OM-RAETT-REOVKOFF                           
075500                                                                          
075600           IF FOERS-OK                                                    
075700             PERFORM S03-BERAEKNA-NY-PRFOERS                              
075800           END-IF                                                         
075900         ELSE                                                             
076000           MOVE NEJ                 TO INDATA-SW                          
076100           MOVE MFS-NUM-FAELT-FEL   TO MOD-REOVKOFF-ATTR                  
076200           MOVE MFS-NUM-FAELT-RAETT TO MOD-REFOERS-ATTR                   
076300         END-IF                                                           
076400                                                                          
076500       ELSE                                                               
076600         MOVE NEJ               TO INDATA-SW                              
076700         MOVE MFS-NUM-FAELT-FEL TO MOD-PRFOERS-ATTR                       
076800         MOVE MFS-NUM-FAELT-FEL TO MOD-REFOERS-ATTR                       
076900       END-IF                                                             
077000                                                                          
077100     ELSE                                                                 
077200       IF MID-PRFOERS NOT = ALL '+'                                       
077300         MOVE MID-PRFOERS TO DEC-IDFRIDATA                                
077400         MOVE +7          TO DEC-KVHELTAL                                 
077500         MOVE +2          TO DEC-KVDECIMAL                                
077600         CALL WDECEDIT USING DEC-WDECAREA                                 
077700         IF DEC-KDSVAR-OK                                                 
077800           MOVE DEC-IDEDITDATA       TO SPAR-PRFOERS                      
077900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRFOERS-ATTR                  
078000         ELSE                                                             
078100           MOVE NEJ               TO INDATA-SW                            
078200           MOVE MFS-NUM-FAELT-FEL TO MOD-PRFOERS-ATTR                     
078300         END-IF                                                           
078400       END-IF                                                             
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 DBCA-KOLLA-OM-RAETT-REFOERS SECTION.                                     
078900                                                                          
079000     MOVE NEJ TO FLAGGA-FOERS                                             
079100     MOVE MID-REFOERS TO DEC-IDFRIDATA                                    
079200     MOVE +2          TO DEC-KVHELTAL                                     
079300     MOVE +3          TO DEC-KVDECIMAL                                    
079400     CALL WDECEDIT USING DEC-WDECAREA                                     
079500     IF DEC-KDSVAR-OK                                                     
079600       MOVE DEC-IDEDITDATA           TO SPAR-REFOERS                      
079700       MOVE MFS-NUM-FAELT-RAETT      TO MOD-REFOERS-ATTR                  
079800       MOVE JA                       TO FLAGGA-FOERS                      
079900     ELSE                                                                 
080000       MOVE NEJ                   TO INDATA-SW                            
080100       MOVE MFS-NUM-FAELT-FEL TO MOD-REFOERS-ATTR                         
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 DBCB-KOLLA-OM-RAETT-REOVKOFF SECTION.                                    
080600                                                                          
080700     IF MID-REOVKOFF = ALL '+'                                            
080800       MOVE JA TO FLAGGA-FOERS                                            
080900                                                                          
081000     ELSE                                                                 
081100       MOVE NEJ TO FLAGGA-FOERS                                           
081200       MOVE MID-REOVKOFF        TO DEC-IDFRIDATA                          
081300       MOVE +2                  TO DEC-KVHELTAL                           
081400       MOVE +1                  TO DEC-KVDECIMAL                          
081500       CALL WDECEDIT USING DEC-WDECAREA                                   
081600       IF DEC-KDSVAR-OK                                                   
081700         MOVE DEC-IDEDITDATA    TO SPAR-REOVKOFF                          
081800         MOVE MFS-NUM-FAELT-RAETT TO MOD-REOVKOFF-ATTR                    
081900         MOVE JA                TO FLAGGA-FOERS                           
082000       ELSE                                                               
082100         MOVE NEJ               TO INDATA-SW                              
082200         MOVE MFS-NUM-FAELT-FEL TO MOD-REOVKOFF-ATTR                      
082300       END-IF                                                             
082400     END-IF                                                               
082500     .                                                                    
082600     EJECT                                                                
082700 DBD-KOLLA-REOVKOFF SECTION.                                              
082800                                                                          
082900     IF MID-REOVKOFF NOT = ALL '+'                                        
083000       MOVE MID-REOVKOFF TO DEC-IDFRIDATA                                 
083100       MOVE +2           TO DEC-KVHELTAL                                  
083200       MOVE +1           TO DEC-KVDECIMAL                                 
083300       CALL WDECEDIT USING DEC-WDECAREA                                   
083400       IF DEC-KDSVAR-OK                                                   
083500         MOVE DEC-IDEDITDATA      TO SPAR-REOVKOFF                        
083600         MOVE MFS-NUM-FAELT-RAETT TO MOD-REOVKOFF-ATTR                    
083700                                                                          
083800         IF MID-REFOERS = ALL '+' AND                                     
083900            PHUV-REFOERS > ZERO                                           
084000           PERFORM S03-BERAEKNA-NY-PRFOERS                                
084100         END-IF                                                           
084200                                                                          
084300       ELSE                                                               
084400         MOVE NEJ               TO INDATA-SW                              
084500         MOVE MFS-NUM-FAELT-FEL TO MOD-REOVKOFF-ATTR                      
084600       END-IF                                                             
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 DBE-KOLLA-PRLEGKST SECTION.                                              
085100                                                                          
085200     IF MID-PRLEGKST NOT = ALL '+'                                        
085300       MOVE MID-PRLEGKST TO DEC-IDFRIDATA                                 
085400       MOVE +7           TO DEC-KVHELTAL                                  
085500       MOVE +2           TO DEC-KVDECIMAL                                 
085600       CALL WDECEDIT USING DEC-WDECAREA                                   
085700       IF DEC-KDSVAR-OK                                                   
085800         MOVE DEC-IDEDITDATA       TO SPAR-PRLEGKST                       
085900         MOVE MFS-NUM-FAELT-RAETT  TO MOD-PRLEGKST-ATTR                   
086000       ELSE                                                               
086100         MOVE NEJ                 TO INDATA-SW                            
086200         MOVE MFS-NUM-FAELT-FEL   TO MOD-PRLEGKST-ATTR                    
086300       END-IF                                                             
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 DBF-KOLLA-KDVALUTA SECTION.                                              
086800                                                                          
086900     IF MID-KDVALUTA NOT = ALL '+'                                        
087000                                                                          
087100       IF MID-KDVALUTA NUMERIC                                            
087200         MOVE MID-KDVALUTA        TO SPAR-KDVALUTA-MID                    
087300                                                                          
087400         IF SPAR-KDVALUTA-MID < +1               OR                       
087500            SPAR-KDVALUTA-MID > TAB-IX-MAX                                
087600           MOVE NEJ                 TO INDATA-SW                          
087700           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDVALUTA-ATTR                  
087800         ELSE                                                             
087900           MOVE SPAR-KDVALUTA-MID   TO SPAR-KDVALUTA                      
088000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDVALUTA-ATTR                  
088100         END-IF                                                           
088200                                                                          
088300       ELSE                                                               
088400         MOVE NEJ                 TO INDATA-SW                            
088500         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDVALUTA-ATTR                    
088600       END-IF                                                             
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 DBG-KOLLA-PRKURS SECTION.                                                
089100                                                                          
089200     IF MID-KDVALUTA NOT = ALL '+' AND                                    
089300        MID-PRKURS       = ALL '+'                                        
089500                                                                          
089700       MOVE TAB-KDVALISO(SPAR-KDVALUTA)                                   
089800                                  TO CURR-KDVALISO-ROW                    
090100       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
090110       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
090120       MOVE 'M'                   TO CURR-KDVALTYP                        
090200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
090300       IF CURR-KDSVAR = ' '                                               
090400          MOVE CURR-PRKURS-NEW     TO WS-PRKURS                           
090500          MOVE WS-PRKURS           TO SPAR-PRKURS                         
090600          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDVALUTA-ATTR                   
090700       ELSE                                                               
090800          MOVE MFS-NUM-FAELT-FEL TO MOD-KDVALUTA-ATTR                     
090900          MOVE NEJ               TO INDATA-SW                             
091000       END-IF                                                             
091100                                                                          
091200     ELSE                                                                 
091300       IF MID-PRKURS NOT = ALL '+'                                        
091400         MOVE MID-PRKURS   TO DEC-IDFRIDATA                               
091500         MOVE +6           TO DEC-KVHELTAL                                
091600         MOVE +4           TO DEC-KVDECIMAL                               
091700         CALL WDECEDIT USING DEC-WDECAREA                                 
091800                                                                          
091900         IF DEC-KDSVAR-OK                                                 
092000           MOVE DEC-IDEDITDATA        TO SPAR-PRKURS                      
092100                                                                          
092200           IF SPAR-PRKURS > ZERO                                          
092300             MOVE MFS-NUM-FAELT-RAETT TO MOD-PRKURS-ATTR                  
092400           ELSE                                                           
092500             MOVE NEJ                 TO INDATA-SW                        
092600             MOVE MFS-NUM-FAELT-FEL   TO MOD-PRKURS-ATTR                  
092700           END-IF                                                         
092800                                                                          
092900         ELSE                                                             
093000           MOVE NEJ                   TO INDATA-SW                        
093100           MOVE MFS-NUM-FAELT-FEL     TO MOD-PRKURS-ATTR                  
093200         END-IF                                                           
093300       END-IF                                                             
093400     END-IF                                                               
093500     .                                                                    
093600     EJECT                                                                
093700 DC-AENDRA-I-DB SECTION.                                                  
093800                                                                          
093900     IF MID-PRAVDRAG NOT = ALL '+'                                        
094000       MOVE SPAR-PRAVDRAG         TO PHUV-PRAVDRAG                        
094100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRAVDRAG-ATTR                    
094200       MOVE SPAR-REAVDRAG         TO PHUV-REAVDRAG                        
094300     END-IF                                                               
094400                                                                          
094500     IF MID-REAVDRAG NOT = ALL '+'                                        
094600       MOVE SPAR-REAVDRAG         TO PHUV-REAVDRAG                        
094700       MOVE SPAR-PRAVDRAG         TO PHUV-PRAVDRAG                        
094800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRAVDRAG-ATTR                    
094900     END-IF                                                               
095000                                                                          
095100     IF MID-PRAVDRAG NOT = ALL '+' OR                                     
095200        MID-REAVDRAG NOT = ALL '+'                                        
095300       COMPUTE WS-SUMMA         = PHUV-SUORDV   -                         
095400                                  PHUV-PRAVDRAG                           
095500       COMPUTE WS-PREMBHNT      = WS-SUMMA      *                         
095600                                  SPAR-REEMBHNT /                         
095700                                  100                                     
095800       COMPUTE PHUV-PREMBHNT ROUNDED = WS-PREMBHNT                        
095900     END-IF                                                               
096000                                                                          
096100     IF MID-PRFRAKT NOT = ALL '+'                                         
096200       MOVE SPAR-PRFRAKT          TO PHUV-PRFRAKT                         
096300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFRAKT-ATTR                     
096400                                                                          
096500       IF MID-PRFOERS = ALL '+' AND                                       
096600          MID-REFOERS = ALL '+' AND                                       
096700          PHUV-REFOERS > ZERO                                             
096800         MOVE SPAR-PRFOERS          TO PHUV-PRFOERS                       
096900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFOERS-ATTR                   
097000       END-IF                                                             
097100     END-IF                                                               
097200                                                                          
097300     IF MID-PRFOERS NOT = ALL '+'                                         
097400       MOVE SPAR-PRFOERS          TO PHUV-PRFOERS                         
097500       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFOERS-ATTR                     
097600       MOVE ZERO                  TO PHUV-REFOERS                         
097700     END-IF                                                               
097800                                                                          
097900     IF MID-REFOERS NOT = ALL '+'                                         
098000       MOVE SPAR-REFOERS          TO PHUV-REFOERS                         
098100       MOVE SPAR-PRFOERS          TO PHUV-PRFOERS                         
098200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFOERS-ATTR                     
098300     END-IF                                                               
098400                                                                          
098500     IF MID-REOVKOFF NOT = ALL '+'                                        
098600       MOVE SPAR-REOVKOFF         TO PHUV-REOVKOFF                        
098700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REOVKOFF-ATTR                    
098800                                                                          
098900       IF PHUV-REFOERS > ZERO                                             
099000         MOVE SPAR-PRFOERS          TO PHUV-PRFOERS                       
099100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRFOERS-ATTR                   
099200       END-IF                                                             
099300     END-IF                                                               
099400                                                                          
099500     IF MID-PRLEGKST NOT = ALL '+'                                        
099600       MOVE SPAR-PRLEGKST         TO PHUV-PRLEGKST                        
099700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRLEGKST-ATTR                    
099800     END-IF                                                               
099900                                                                          
100000     IF MID-KDVALUTA NOT = ALL '+'                                        
100100       MOVE SPAR-KDVALUTA         TO PHUV-KDVALUTA                        
100200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDVALUTA-ATTR                    
100300     END-IF                                                               
100400                                                                          
100500     IF MID-PRKURS NOT = ALL '+' OR                                       
100600        WS-PRKURS > +0                                                    
100700       MOVE SPAR-PRKURS           TO PHUV-PRKURS                          
100800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRKURS-ATTR                      
100900     END-IF                                                               
101000                                                                          
101100     IF MID-BELOSORT NOT = ALL '+'                                        
101200       MOVE MID-BELOSORT          TO PHUV-BELOSORT                        
101300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BELOSORT-ATTR                    
101400     END-IF                                                               
101500                                                                          
101600     IF MID-TEBANKTO NOT = ALL '+'                                        
101700       MOVE MID-TEBANKTO          TO PHUV-TEBANKTO                        
101800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBANKTO-ATTR                    
101900     END-IF                                                               
102000                                                                          
102100     IF MID-TEBANK-RAD1 NOT = ALL '+'                                     
102200       MOVE MID-TEBANK-RAD1       TO PHUV-TEBANK(1)                       
102300       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBANK-RAD1-ATTR                 
102400     END-IF                                                               
102500                                                                          
102600     IF MID-TEBANK-RAD2 NOT = ALL '+'                                     
102700       MOVE MID-TEBANK-RAD2       TO PHUV-TEBANK(2)                       
102800       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBANK-RAD2-ATTR                 
102900     END-IF                                                               
103000                                                                          
103100     MOVE DAGENS-DATUM TO PHUV-TIUPPDAT                                   
103200     MOVE UPPDAT-TID   TO PHUV-TIUPPTID                                   
103300                                                                          
103400     PERFORM IMS-REPL-PROC-WDE801                                         
103500     .                                                                    
103600     EJECT                                                                
103700 DD-BERAEKNA-NYA-VARDEN-PA-WDE8 SECTION.                                  
103800                                                                          
103900     COMPUTE   WS-PRAVDRAG = (PHUV-SUORDV    *                            
104000                              PHUV-REAVDRAG) /                            
104100                             100                                          
104200                                                                          
104300                                                                          
104400     COMPUTE PHUV-PRAVDRAG ROUNDED = WS-PRAVDRAG                          
104500                                                                          
104600     COMPUTE WS-SUMMA      = (PHUV-SUORDV    +                            
104700                              PHUV-PRFRAKT   +                            
104800                              PHUV-PREMBHNT) *                            
104900                              WS-KONSTANT                                 
105000                                                                          
105100     COMPUTE WS-PRFOERS    = (WS-SUMMA       +                            
105200                             (PHUV-REOVKOFF  /                            
105300                              100            *                            
105400                              WS-SUMMA))     *                            
105500                              PHUV-REFOERS   /                            
105600                              100                                         
105700                                                                          
105800     COMPUTE PHUV-PRFOERS ROUNDED = WS-PRFOERS                            
105900                                                                          
106000     MOVE DAGENS-DATUM TO PHUV-TIUPPDAT                                   
106100     MOVE UPPDAT-TID   TO PHUV-TIUPPTID                                   
106200                                                                          
106300     PERFORM IMS-REPL-PROC-WDE801                                         
106400     .                                                                    
106500     EJECT                                                                
106600 E-BEHANDLA-ENTER SECTION.                                                
106700                                                                          
106800     IF MID-IDDISTR-IN  = ALL '+' AND                                     
106900        MID-IDKUNDNR-IN = ALL '+' AND                                     
107000        MID-IDORDNR7-IN = ALL '+' AND                                     
107100        MID-IDARTNR-IN  = ALL '+'                                         
107200                                                                          
107300       IF EGEN-MID AND MID-INPUT NOT = ALL '+'                            
107400                                                                          
107500         MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                             
107600         CALL WMEDKONV USING MED-WMEDAREA                                 
107700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
107800         MOVE NEJ TO INDATA-SW                                            
107900                                                                          
108000         PERFORM EA-LAES-IN-IGEN-INDATA                                   
108100                                                                          
108200       ELSE                                                               
108300         PERFORM MFS-RENSA-FAELT-IN                                       
108400       END-IF                                                             
108500                                                                          
108600     ELSE                                                                 
108700       PERFORM MFS-RENSA-FAELT-IN                                         
108800     END-IF                                                               
108900     .                                                                    
109000     EJECT                                                                
109100 EA-LAES-IN-IGEN-INDATA SECTION.                                          
109200                                                                          
109300     IF MID-PRAVDRAG NOT = ALL '+'                                        
109400       MOVE MFS-ROER-EJ-FAELT     TO MOD-PRAVDRAG                         
109500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRAVDRAG-ATTR                    
109600     END-IF                                                               
109700                                                                          
109800     IF MID-REAVDRAG NOT = ALL '+'                                        
109900       MOVE MFS-ROER-EJ-FAELT     TO MOD-REAVDRAG                         
110000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REAVDRAG-ATTR                    
110100     END-IF                                                               
110200                                                                          
110300     IF MID-PRFRAKT NOT = ALL '+'                                         
110400       MOVE MFS-ROER-EJ-FAELT     TO MOD-PRFRAKT                          
110500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRFRAKT-ATTR                     
110600     END-IF                                                               
110700                                                                          
110800     IF MID-PRFOERS NOT = ALL '+'                                         
110900       MOVE MFS-ROER-EJ-FAELT     TO MOD-PRFOERS                          
111000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRFOERS-ATTR                     
111100     END-IF                                                               
111200                                                                          
111300     IF MID-REFOERS NOT = ALL '+'                                         
111400       MOVE MFS-ROER-EJ-FAELT     TO MOD-REFOERS                          
111500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REFOERS-ATTR                     
111600     END-IF                                                               
111700                                                                          
111800     IF MID-REOVKOFF NOT = ALL '+'                                        
111900       MOVE MFS-ROER-EJ-FAELT     TO MOD-REOVKOFF                         
112000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REOVKOFF-ATTR                    
112100     END-IF                                                               
112200                                                                          
112300     IF MID-PRLEGKST NOT = ALL '+'                                        
112400       MOVE MFS-ROER-EJ-FAELT     TO MOD-PRLEGKST                         
112500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRLEGKST-ATTR                    
112600     END-IF                                                               
112700                                                                          
112800     IF MID-KDVALUTA NOT = ALL '+'                                        
112900       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDVALUTA                         
113000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDVALUTA-ATTR                    
113100     END-IF                                                               
113200                                                                          
113300     IF MID-PRKURS NOT = ALL '+' OR                                       
113400        WS-PRKURS > ZERO                                                  
113500       MOVE MFS-ROER-EJ-FAELT     TO MOD-PRKURS                           
113600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRKURS-ATTR                      
113700     END-IF                                                               
113800                                                                          
113900     IF MID-BELOSORT NOT = ALL '+'                                        
114000       MOVE MFS-ROER-EJ-FAELT     TO MOD-BELOSORT                         
114100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELOSORT-ATTR                    
114200     END-IF                                                               
114300                                                                          
114400     IF MID-TEBANKTO NOT = ALL '+'                                        
114500       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBANKTO                         
114600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBANKTO-ATTR                    
114700     END-IF                                                               
114800                                                                          
114900     IF MID-TEBANK-RAD1 NOT = ALL '+'                                     
115000       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBANK-RAD1                      
115100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBANK-RAD2                      
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 F-LAES-VISA-BILD SECTION.                                                
115600                                                                          
115700     PERFORM IMS-GHU-PROC-WDE801-PHUV                                     
115800                                                                          
115900     IF DIST79-DEALER-PRICE                                               
116000       COMPUTE SPAR-SUMMA = PHUV-SUORDV-LOC +                             
116100                            PHUV-SUORDV-LOCPREL                           
116200       PERFORM S04-OMRAKNING-LOC-TO-SEK                                   
116300       MOVE EXCH-SUORDV-UT       TO MOD-SUORDV                            
116400       MOVE EXCH-SUORDV-UT       TO MOD-SUFOBV                            
116500       MOVE EXCH-SUORDV-UT       TO MOD-SUFKTBEL                          
116600       IF PHUV-SUORDV-LOCPREL > 0                                         
116700         MOVE '*'                 TO MOD-TEASTRIX-SUORDV                  
116800         MOVE '*'                 TO MOD-TEASTRIX-SUFOBV                  
116900         MOVE '*'                 TO MOD-TEASTRIX-SUFKTB                  
117000       ELSE                                                               
117100         MOVE ' '             TO MOD-TEASTRIX-SUORDV                      
117200         MOVE ' '             TO MOD-TEASTRIX-SUFOBV                      
117300         MOVE ' '             TO MOD-TEASTRIX-SUFKTB                      
117400       END-IF                                                             
117500     ELSE                                                                 
117600       MOVE PHUV-SUORDV   TO MOD-SUORDV                                   
117700                                                                          
117800       COMPUTE SPAR-SUFOBV = PHUV-SUORDV   -                              
117900                             PHUV-PRAVDRAG +                              
118000                             PHUV-PREMBHNT                                
118100       MOVE SPAR-SUFOBV   TO MOD-SUFOBV                                   
118200                                                                          
118300       PERFORM FA-BERAEKNA-VISA-SUFKTBEL                                  
118400       MOVE PHUV-PREMBHNT TO MOD-PREMBHNT                                 
118500     END-IF                                                               
118600                                                                          
118700     PERFORM FB-BERAEKNA-VISA-PRMOMS                                      
118800     PERFORM FC-BERAEKNA-VISA-SUFKTBEL-UTL                                
118900     PERFORM FD-FLYTTA-TILL-UPPDAT-FAELT                                  
119000     PERFORM FE-KOLLA-OM-UPPDAT-GJORD                                     
119100                                                                          
119200     IF DIST79-DEALER-PRICE                                               
119300        MOVE MFS-STAENG-FAELT  TO MOD-PRAVDRAG-ATTR                       
119400        MOVE MFS-STAENG-FAELT  TO MOD-REAVDRAG-ATTR                       
119500        MOVE MFS-STAENG-FAELT  TO MOD-PRFRAKT-ATTR                        
119600        MOVE MFS-STAENG-FAELT  TO MOD-PRFOERS-ATTR                        
119700        MOVE MFS-STAENG-FAELT  TO MOD-REFOERS-ATTR                        
119800        MOVE MFS-STAENG-FAELT  TO MOD-REOVKOFF-ATTR                       
119900        MOVE MFS-STAENG-FAELT  TO MOD-PRLEGKST-ATTR                       
120000        MOVE MFS-STAENG-FAELT  TO MOD-KDVALUTA-ATTR                       
120100        MOVE MFS-STAENG-FAELT  TO MOD-PRKURS-ATTR                         
120200     END-IF                                                               
120300     .                                                                    
120400     EJECT                                                                
120500 FA-BERAEKNA-VISA-SUFKTBEL SECTION.                                       
120600                                                                          
120700     MOVE PHUV-IDDISTR TO TEST-IDDISTR                                    
120800                                                                          
120900     IF DIST03-SVERIGE AND PHUV-KDMOMSIN = +1                             
121000       COMPUTE  WS-SUFKTBEL =  PHUV-SUORDV     +                          
121100                               PHUV-PRFRAKT    +                          
121200                               PHUV-PREMBHNT   +                          
121300                               PHUV-PRFOERS    +                          
121400                               PHUV-PRLEGKST   -                          
121500                               PHUV-PRAVDRAG                              
121600                                                                          
121700       COMPUTE SPAR-SUFKTBEL = WS-SUFKTBEL     *                          
121800                              (1               +                          
121900                              (CONS-REMOMS     /                          
122000                               100))                                      
122100     ELSE                                                                 
122200       COMPUTE  WS-SUFKTBEL =  PHUV-SUORDV     +                          
122300                               PHUV-PRFRAKT    +                          
122400                               PHUV-PREMBHNT   +                          
122500                               PHUV-PRFOERS    +                          
122600                               PHUV-PRLEGKST   -                          
122700                               PHUV-PRAVDRAG                              
122800                                                                          
122900       MOVE WS-SUFKTBEL TO SPAR-SUFKTBEL                                  
123000     END-IF                                                               
123100                                                                          
123200     MOVE SPAR-SUFKTBEL TO MOD-SUFKTBEL                                   
123300     .                                                                    
123400     EJECT                                                                
123500 FB-BERAEKNA-VISA-PRMOMS SECTION.                                         
123600                                                                          
123700     MOVE PHUV-IDDISTR TO TEST-IDDISTR                                    
123800                                                                          
123900     IF DIST03-SVERIGE AND PHUV-KDMOMSIN = +1                             
124000       COMPUTE SPAR-PRMOMS   =  SPAR-SUFKTBEL -                           
124100                                WS-SUFKTBEL                               
124200                                                                          
124300       MOVE SPAR-PRMOMS TO MOD-PRMOMS                                     
124400     ELSE                                                                 
124500                                                                          
124600       MOVE ZERO TO MOD-PRMOMS                                            
124700     END-IF                                                               
124800     .                                                                    
124900     EJECT                                                                
125000 FC-BERAEKNA-VISA-SUFKTBEL-UTL SECTION.                                   
125100                                                                          
125200     IF DIST79-DEALER-PRICE                                               
125300       MOVE SPAR-SUMMA       TO MOD-SUFKTBEL-UTL                          
125400     ELSE                                                                 
125500       COMPUTE SPAR-SUFKTBEL-UTL ROUNDED = SPAR-SUFKTBEL /                
125600                                           PHUV-PRKURS                    
125700       MOVE SPAR-SUFKTBEL-UTL TO MOD-SUFKTBEL-UTL                         
125800     END-IF                                                               
125900                                                                          
126000     MOVE TAB-KDVALISO(PHUV-KDVALUTA)                                     
126100                               TO MOD-BEVALUTA                            
126200     .                                                                    
126300     EJECT                                                                
126400 FD-FLYTTA-TILL-UPPDAT-FAELT SECTION.                                     
126500                                                                          
126600     IF INDATA-FEL                                                        
126700                                                                          
126800       IF MID-PRAVDRAG = ALL '+'                                          
126900         MOVE PHUV-PRAVDRAG TO MOD-PRAVDRAG                               
127000       END-IF                                                             
127100                                                                          
127200       IF MID-PRFRAKT = ALL '+'                                           
127300         MOVE PHUV-PRFRAKT TO MOD-PRFRAKT                                 
127400       END-IF                                                             
127500                                                                          
127600       IF MID-PRFOERS = ALL '+'                                           
127700         MOVE PHUV-PRFOERS TO MOD-PRFOERS                                 
127800       END-IF                                                             
127900                                                                          
128000       IF MID-REOVKOFF = ALL '+'                                          
128100         MOVE PHUV-REOVKOFF TO MOD-REOVKOFF                               
128200       END-IF                                                             
128300                                                                          
128400       IF MID-PRLEGKST = ALL '+'                                          
128500         MOVE PHUV-PRLEGKST TO MOD-PRLEGKST                               
128600       END-IF                                                             
128700                                                                          
128800       IF MID-KDVALUTA = ALL '+'                                          
128900         MOVE PHUV-KDVALUTA TO MOD-KDVALUTA                               
129000       END-IF                                                             
129100                                                                          
129200       IF MID-PRKURS = ALL '+' OR                                         
129300          WS-PRKURS > +0                                                  
129400         MOVE PHUV-PRKURS TO MOD-PRKURS                                   
129500       END-IF                                                             
129600                                                                          
129700       IF MID-BELOSORT = ALL '+'                                          
129800         MOVE PHUV-BELOSORT TO MOD-BELOSORT                               
129900       END-IF                                                             
130000                                                                          
130100       IF MID-TEBANKTO = ALL '+'                                          
130200         MOVE PHUV-TEBANKTO TO MOD-TEBANKTO                               
130300       END-IF                                                             
130400                                                                          
130500       IF MID-TEBANK-RAD1 = ALL '+'                                       
130600         MOVE PHUV-TEBANK(1) TO MOD-TEBANK-RAD1                           
130700       END-IF                                                             
130800                                                                          
130900       IF MID-TEBANK-RAD2 = ALL '+'                                       
131000         MOVE PHUV-TEBANK(2) TO MOD-TEBANK-RAD2                           
131100       END-IF                                                             
131200                                                                          
131300     ELSE                                                                 
131400       MOVE PHUV-PRAVDRAG  TO MOD-PRAVDRAG                                
131500       MOVE PHUV-PRFRAKT   TO MOD-PRFRAKT                                 
131600       MOVE PHUV-PRFOERS   TO MOD-PRFOERS                                 
131700       MOVE PHUV-REOVKOFF  TO MOD-REOVKOFF                                
131800       MOVE PHUV-PRLEGKST  TO MOD-PRLEGKST                                
131900       MOVE PHUV-KDVALUTA  TO MOD-KDVALUTA                                
132000       MOVE PHUV-PRKURS    TO MOD-PRKURS                                  
132100       MOVE PHUV-BELOSORT  TO MOD-BELOSORT                                
132200       MOVE PHUV-TEBANKTO  TO MOD-TEBANKTO                                
132300       MOVE PHUV-TEBANK(1) TO MOD-TEBANK-RAD1                             
132400       MOVE PHUV-TEBANK(2) TO MOD-TEBANK-RAD2                             
132500     END-IF                                                               
132600     .                                                                    
132700     EJECT                                                                
132800 FE-KOLLA-OM-UPPDAT-GJORD SECTION.                                        
132900                                                                          
133000     IF UPPDATERING-OK                                                    
133100                                                                          
133200       MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                             
133300       CALL WMEDKONV USING MED-WMEDAREA                                   
133400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
133500                                                                          
133600       MOVE MFS-RENSA-FAELT TO MOD-REAVDRAG                               
133700                               MOD-REFOERS                                
133800     END-IF                                                               
133900     .                                                                    
134000     EJECT                                                                
134100 S01-KOLLA-BEHOERIGHET SECTION.                                           
134200                                                                          
134300     IF PHUV-KDPROTYP = 'L'                                               
134400                                                                          
134500       IF PHUV-IDUSER NOT = MSG-SIGNON-USERID                             
134600         MOVE NEJ TO ALLT-SW                                              
134700         MOVE FELM-OBEHOERIG TO MED-IDMFSFEL                              
134800         CALL WMEDKONV USING MED-WMEDAREA                                 
134900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
135000       END-IF                                                             
135100                                                                          
135200     END-IF                                                               
135300     .                                                                    
135400     EJECT                                                                
135500 S02-KOLLA-OM-PRAD-FINNS SECTION.                                         
135600                                                                          
135700     IF ALLT-OK                                                           
135800                                                                          
135900       MOVE PHUV-IDORDER TO W-PRAD-IDORDER-MIN                            
136000                            W-PRAD-IDORDER-MAX                            
136100       PERFORM IMS-GN-PROD-WDE901-PRAD                                    
136200                                                                          
136300       IF SEGMENT-FINNS                                                   
136400         CONTINUE                                                         
136500                                                                          
136600       ELSE                                                               
136700         MOVE NEJ TO ALLT-SW                                              
136800         MOVE FELM-RADER-SAKNAS TO MED-IDMFSFEL                           
136900         CALL WMEDKONV USING MED-WMEDAREA                                 
137000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
137100       END-IF                                                             
137200                                                                          
137300     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137600 S03-BERAEKNA-NY-PRFOERS SECTION.                                         
137700                                                                          
137800     COMPUTE WS-SUMMA   = (SPAR-SUORDV    +                               
137900                           SPAR-PREMBHNT  +                               
138000                           SPAR-PRFRAKT)  *                               
138100                           WS-KONSTANT                                    
138200                                                                          
138300     COMPUTE WS-PRFOERS = (WS-SUMMA       +                               
138400                          (SPAR-REOVKOFF  /                               
138500                           100            *                               
138600                           WS-SUMMA))     *                               
138700                           SPAR-REFOERS   /                               
138800                           100                                            
138900                                                                          
139000     COMPUTE SPAR-PRFOERS ROUNDED = WS-PRFOERS                            
139100     .                                                                    
139200     EJECT                                                                
139300 S04-OMRAKNING-LOC-TO-SEK SECTION.                                        
139400                                                                          
139500     MOVE PHUV-PRKURS        TO EXCH-PRKURS                               
139600*      +1 KDCALL = LOKAL VALUTA TILL SEK                                  
139700     MOVE +1                 TO EXCH-KDCALL                               
139800     MOVE SPAR-SUMMA         TO EXCH-SUORDV-IN                            
139900     MOVE +0                 TO EXCH-PRARTNTO-IN                          
140000     CALL W411EXCH USING    EXCH-W411EXCH                                 
140100     .                                                                    
140200     EJECT                                                                
140300 MFS-RENSA-FAELT-UT SECTION.                                              
140400                                                                          
140500*    --- ALLA UTDATA-FÄLT                                                 
140600     MOVE MFS-RENSA-FAELT TO MOD-SUORDV                                   
140700                             MOD-SUFOBV                                   
140800                             MOD-SUFKTBEL                                 
140900                             MOD-PRMOMS                                   
141000                             MOD-PREMBHNT                                 
141100                             MOD-PRAVDRAG                                 
141200                             MOD-PRFRAKT                                  
141300                             MOD-PRFOERS                                  
141400                             MOD-REFOERS                                  
141500                             MOD-REOVKOFF                                 
141600                             MOD-PRLEGKST                                 
141700                             MOD-KDVALUTA                                 
141800                             MOD-PRKURS                                   
141900                             MOD-SUFKTBEL-UTL                             
142000                             MOD-BEVALUTA                                 
142100                             MOD-BELOSORT                                 
142200                             MOD-TEBANKTO                                 
142300                             MOD-TEBANK-RAD1                              
142400                             MOD-TEBANK-RAD2                              
142500     .                                                                    
142600     EJECT                                                                
142700 MFS-RENSA-FAELT-IN SECTION.                                              
142800                                                                          
142900*    --- ALLA INDATA-FÄLT                                                 
143000     MOVE MFS-RENSA-FAELT TO MOD-PRAVDRAG                                 
143100                             MOD-REAVDRAG                                 
143200                             MOD-PRFRAKT                                  
143300                             MOD-PRFOERS                                  
143400                             MOD-REFOERS                                  
143500                             MOD-REOVKOFF                                 
143600                             MOD-PRLEGKST                                 
143700                             MOD-KDVALUTA                                 
143800                             MOD-PRKURS                                   
143900                             MOD-BELOSORT                                 
144000                             MOD-TEBANKTO                                 
144100                             MOD-TEBANK-RAD1                              
144200                             MOD-TEBANK-RAD2                              
144300     .                                                                    
144400     EJECT                                                                
144500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
144600                                                                          
144700*    --- ALLA INDATA-FÄLT                                                 
144800     MOVE MFS-ROER-EJ-FAELT TO MOD-PRAVDRAG                               
144900                               MOD-REAVDRAG                               
145000                               MOD-PRFRAKT                                
145100                               MOD-PRFOERS                                
145200                               MOD-REFOERS                                
145300                               MOD-REOVKOFF                               
145400                               MOD-PRLEGKST                               
145500                               MOD-KDVALUTA                               
145600                               MOD-PRKURS                                 
145700                               MOD-BELOSORT                               
145800                               MOD-TEBANKTO                               
145900                               MOD-TEBANK-RAD1                            
146000                               MOD-TEBANK-RAD2                            
146100     .                                                                    
146200     EJECT                                                                
146300* --- IMS SEKTIONER ---                                                   
146400     SKIP3                                                                
146500 IMS-GET-MSG SECTION.                                                     
146600                                                                          
146700     MOVE '  QC' TO GODK-STATUSKODER                                      
146800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
146900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200     SKIP3                                                                
147300 IMS-INSERT-MSG SECTION.                                                  
147400                                                                          
147500     IF ENGLISH-TEXT                                                      
147600       MOVE 'N' TO MFS-KDHUVOMR                                           
147700     END-IF                                                               
147800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
147900     MOVE SPACE TO GODK-STATUSKODER                                       
148000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
148100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
148200     PERFORM IMS-STATUSKONTROLL                                           
148300     .                                                                    
148400     EJECT                                                                
148500 IMS-GHU-PROC-WDE801-PHUV SECTION.                                        
148600                                                                          
148700     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
148800          DELIMITED BY SIZE INTO SSA1                                     
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-WDE801 SSA1              
149100     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUSKONTROLL                                           
149300     .                                                                    
149400     SKIP3                                                                
149500 IMS-GN-PROD-WDE901-PRAD SECTION.                                         
149600                                                                          
149700     STRING 'WLPROD01(WDE901KY=>' W-WDE901KY-MIN-X                        
149800                    '&WDE901KY=<' W-WDE901KY-MAX-X ')'                    
149900          DELIMITED BY SIZE INTO SSA1                                     
150000     MOVE '  GE' TO GODK-STATUSKODER                                      
150100     CALL CBLTDLI USING GU PROD-PCB DLI-IO-AREA SSA1                      
150200     MOVE PROD-STATUS-CODE TO STATUS-WS                                   
150300     PERFORM IMS-STATUSKONTROLL                                           
150400     .                                                                    
150500     EJECT                                                                
150600 IMS-REPL-PROC-WDE801 SECTION.                                            
150700                                                                          
150800     MOVE '    ' TO GODK-STATUSKODER                                      
150900     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-WDE801                  
151000     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
151100     PERFORM IMS-STATUSKONTROLL                                           
151200     .                                                                    
151300     EJECT                                                                
152600 IMS-STATUSKONTROLL SECTION.                                              
152700                                                                          
152800     SET STATUS-IX TO 1                                                   
152900     SEARCH GODK-STATUS                                                   
153000       AT END                                                             
153100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
153200         DELIMITED BY SIZE INTO FELTEXT                                   
153300         CALL FELLOG                                                      
153400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
153500     END-SEARCH                                                           
153600     .                                                                    
153700     EJECT                                                                
153800*    -COPY WY2000P1                                                       
