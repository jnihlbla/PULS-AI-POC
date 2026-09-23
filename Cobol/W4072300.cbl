000010*                                                                         
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0162      *        
000040******************************************************************        
000050*                                                                         
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4072300.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   95/07/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISA/UPPDATERA TEXT INFO PÅ LEVERANSANMÄRKNINGSRAD               
001000*        KAN ÄVEN STARTAS UPP VIA DISPATCHEN                              
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
001300                                                                          
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T723                                              
001600*        MID:         W4I72301                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O72301                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002501*    -- CHECKED BY WY2000                                                 
002510     SKIP3                                                                
002600 77  IDPGM                       PIC X(08)   VALUE 'W4072300'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  YES                         PIC X       VALUE 'Y'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003410 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003420                                                                          
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004500     88  EGEN-MID                            VALUE '4723'.                
004600     88  GODK-MID                            VALUE '4723'                 
004610                                                   '472D'.                
004700     88  HELP-MID                            VALUE '0551'.                
004701                                                                          
004710 77  OK-BEHANDLAD                PIC X(3)    VALUE '101'.                 
004720 77  STARTAD-AV-DISPATCHEN-SW    PIC X       VALUE 'N'.                   
004730     88  STARTAD-AV-DISPATCHEN               VALUE 'J'.                   
004740                                                                          
004800     EJECT                                                                
004900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005000 01  GENERELLA-SUBPROGRAM.                                                
005100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     EJECT                                                                
005600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005700*01 -COPY WMEDAREA                                                        
005800     SKIP3                                                                
005900 01  MESSAGE-CODES.                                                       
006000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006010     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006200     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
006300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006800*                                                                         
006900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007000     SKIP3                                                                
007100*01 -COPY WMSGINIT                                                        
007200                                                                          
007300*                                                                         
007400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007700     SKIP3                                                                
007800*01  MID -COPY W4I72301                                                   
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008100     SKIP3                                                                
008200*01  -COPY WMSGAREA                                                       
008300     EJECT                                                                
008400     03  MOD REDEFINES MSG-AREA.                                          
008500*      05  -COPY W4O72301    -PRE MOD-                                    
008600     EJECT                                                                
008610 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
008620     SKIP3                                                                
008630 01  KOM-IO-AREA.                                                         
008640*03  -COPY WMSGKOM                                                        
008650     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008800     SKIP3                                                                
008900*01  -COPY WMFSAREA                                                       
009000     EJECT                                                                
009100                                                                          
009200     SKIP2                                                                
009300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009400*                                                                         
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800                                                                          
009900 01  NYCKLAR-TILL-DLI.                                                    
010000                                                                          
010100     03  W-IDLEVANM-X.                                                    
010200         05  W-IDDISTR           PIC S9(5)   COMP-3 VALUE ZERO.           
010300         05  W-IDKUNDNR          PIC S9(7)   COMP-3 VALUE ZERO.           
010400         05  W-IDRAPPNR          PIC  X(7)          VALUE ZERO.           
010500                                                                          
010600     03  W-WDA211KY-X.                                                    
010700         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
010800         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
010900                                                                          
010910     03  W-KDSEGKEY-X.                                                    
010920         05  W-KDSEGKEY          PIC  X(1)          VALUE '1'.            
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  STATUS-OK                           VALUE '  '.                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(192).                              
012200 01  SSA2                        PIC X(64).                               
012300 01  SSA3                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013000     SKIP3                                                                
013100 01  DLI-IO-AREA.                                                         
013200     03  IO-AREA                 PIC X(1100) VALUE SPACE.                 
013300     03  WLKREE11 REDEFINES IO-AREA.                                      
013400*        05  -COPY WDA211                                                 
013500     EJECT                                                                
013510     03  WLKREE21 REDEFINES IO-AREA.                                      
013520*        05  -COPY WDA221                                                 
013530     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800*01  -COPY W0009   -PRE MSG-                                              
013900*01  -COPY W0009   -PRE DISP-                                             
014000     EJECT                                                                
014100*01  -COPY W0008   -PRE USEA-                                             
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400*01  -COPY W0008   -PRE KREE-                                             
014410     05  FILLER                  PIC X.                                   
014420     EJECT                                                                
014500 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB USEA-PCB                     
014600                           KREE-PCB.                                      
014700     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB USEA-PCB                     
014800                           KREE-PCB.                                      
014900                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015110       PERFORM IMS-GN-MSG                                                 
015120       IF SEGMENT-FINNS                                                   
015130          MOVE JA TO STARTAD-AV-DISPATCHEN-SW                             
015140       END-IF                                                             
015200       PERFORM A-INIT                                                     
015300       PERFORM B-KOLLA-NYCKLAR                                            
015400       IF NYCKLAR-OK                                                      
015500         IF MFS-UPDATE                                                    
015600             PERFORM H-UPPDATERA                                          
015700         ELSE                                                             
015800            IF MFS-FIRST                                                  
015900               PERFORM C-FOERSTA-SIDA                                     
016000            ELSE                                                          
016100               PERFORM E-SAMMA-SIDA                                       
016200            END-IF                                                        
016300         END-IF                                                           
016310         IF MFS-FIRST OR MFS-UPDATE                                       
016400            PERFORM F-LAES-VISA-INFO                                      
016410         END-IF                                                           
016500       END-IF                                                             
016510       IF STARTAD-AV-DISPATCHEN                                           
016511          PERFORM I-SVARA-DISPATCHEN                                      
016512       ELSE                                                               
016600          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72301 + 4                   
016700          PERFORM IMS-INSERT-MSG                                          
016710       END-IF                                                             
016800     END-IF                                                               
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017600     IF MSG-DUBBLA-TRANSKODER                                             
017700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72301                 
017800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
018000     ELSE                                                                 
018100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I72301                 
018200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
018300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
018400     END-IF                                                               
018500                                                                          
018600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018900                                                                          
019000     MOVE LOW-VALUE   TO MSG-AREA                                         
019100     MOVE 'W4O72301'  TO MFS-IDMOD                                        
019200     MOVE '4723'      TO MOD-IDTRANS                                      
019300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019400                                                                          
019500     IF EGEN-MID OR HELP-MID                                              
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
019900       MOVE '7' TO MFS-IDPFK                                              
020000     END-IF                                                               
020170                                                                          
020200     .                                                                    
020300     EJECT                                                                
020400 B-KOLLA-NYCKLAR SECTION.                                                 
020500                                                                          
020510     IF STARTAD-AV-DISPATCHEN                                             
020511        MOVE MID-IDDISTR-IN    TO W-IDDISTR                               
020520        MOVE MID-IDKUNDNR-IN   TO W-IDKUNDNR                              
020521        MOVE MID-IDRAPPNR-IN   TO W-IDRAPPNR                              
020522        MOVE MID-IDARTNR-IN    TO W-IDARTNR                               
020523        MOVE MID-IDRADNR-IN    TO W-IDRADNR                               
020530     ELSE                                                                 
020600        MOVE ALL '+'           TO MSGI-WMSGINIT                           
020700        MOVE '001'             TO MSGI-KDCALL                             
020800        IF GODK-MID                                                       
020900           MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                           
021000           MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                          
021100           MOVE MID-IDRAPPNR-IN TO MSGI-IDRAPPNR                          
021200           MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                           
021300           MOVE MID-IDRADNR-IN  TO MSGI-IDRADNR                           
021400        END-IF                                                            
021500        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
021510        MOVE '4723'            TO MSGI-IDTRANS                            
021520        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
021600        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
021700                                                                          
021710                                                                          
021720        IF MSGI-IDLAND-SPR = 'GB'                                         
021730          MOVE 'GB'             TO MED-IDSKYLT                            
021740        ELSE                                                              
021750          MOVE 'S '             TO MED-IDSKYLT                            
021760        END-IF                                                            
021770                                                                          
021800        MOVE JA TO NYCKLAR-SW                                             
021900                                                                          
022000        PERFORM BA-KOLLA-IDDISTR                                          
022100        PERFORM BB-KOLLA-IDKUNDNR                                         
022200        PERFORM BC-KOLLA-IDRAPPNR                                         
022300        PERFORM BD-KOLLA-IDARTNR                                          
022400        PERFORM BE-KOLLA-IDRADNR                                          
022500                                                                          
022600*       IF GODK-MID OR NYCKLAR-OK                                         
024300*       END-IF                                                            
024400                                                                          
024500        IF NYCKLAR-FEL                                                    
024600          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
024700          CALL WMEDKONV USING MED-WMEDAREA                                
024800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
025000          PERFORM MFS-RENSA-FAELT-UT                                      
025100        END-IF                                                            
025110     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500                                                                          
025600 BA-KOLLA-IDDISTR  SECTION.                                               
025700                                                                          
025800     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
025900                                                                          
026000     IF MID-IDDISTR-IN          NOT = ALL '+'                             
026100       MOVE '7'                 TO MFS-IDPFK                              
026200       MOVE SPACE               TO MFS-KDTRTYP                            
026300     END-IF                                                               
026400                                                                          
026500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
026600       MOVE MSGI-IDDISTR        TO W-IDDISTR                              
026610                                   MOD-IDDISTR-UT                         
026620       INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE            
026700     ELSE                                                                 
026800       MOVE NEJ                 TO NYCKLAR-SW                             
026900     END-IF                                                               
027000                                                                          
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 BB-KOLLA-IDKUNDNR   SECTION.                                             
027500                                                                          
027600     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
027700                                                                          
027800     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
027900       MOVE '7'                 TO MFS-IDPFK                              
028000       MOVE SPACE               TO MFS-KDTRTYP                            
028100     END-IF                                                               
028200                                                                          
028300     IF MSGI-IDKUNDNR           NUMERIC                                   
028400       MOVE MSGI-IDKUNDNR       TO W-IDKUNDNR                             
028410                                   MOD-IDKUNDNR-UT                        
028420       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
028500     ELSE                                                                 
028600       MOVE NEJ                 TO NYCKLAR-SW                             
028700     END-IF                                                               
028800                                                                          
028900     .                                                                    
029000     EJECT                                                                
029100 BC-KOLLA-IDRAPPNR   SECTION.                                             
029200                                                                          
029300     MOVE MFS-RENSA-FAELT       TO MOD-IDRAPPNR-IN                        
029400                                                                          
029500     IF MID-IDRAPPNR-IN         NOT = ALL '+'                             
029600       MOVE '7'                 TO MFS-IDPFK                              
029700       MOVE SPACE               TO MFS-KDTRTYP                            
029800     END-IF                                                               
029900                                                                          
030000     IF MSGI-IDRAPPNR           NUMERIC AND MSGI-IDRAPPNR > ZERO          
030100       MOVE MSGI-IDRAPPNR       TO W-IDRAPPNR                             
030200     ELSE                                                                 
030300*FIX                                                                      
030400       INSPECT MID-IDRAPPNR-UT REPLACING LEADING SPACE BY ZERO            
030500       MOVE MID-IDRAPPNR-UT     TO MSGI-IDRAPPNR                          
030600                                   W-IDRAPPNR                             
030700*      MOVE NEJ                 TO NYCKLAR-SW                             
030800     END-IF                                                               
030810     MOVE MSGI-IDRAPPNR         TO MOD-IDRAPPNR-UT                        
030820     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
030900                                                                          
031000     .                                                                    
031100     EJECT                                                                
031200 BD-KOLLA-IDARTNR   SECTION.                                              
031300                                                                          
031400     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
031500                                                                          
031600     IF MID-IDARTNR-IN          NOT = ALL '+'                             
031700       MOVE '7'                 TO MFS-IDPFK                              
031800       MOVE SPACE               TO MFS-KDTRTYP                            
031900     END-IF                                                               
032000                                                                          
032100     IF MSGI-IDARTNR            NUMERIC AND MSGI-IDARTNR > ZERO           
032200       MOVE MSGI-IDARTNR        TO W-IDARTNR                              
032210                                   MOD-IDARTNR-UT                         
032220       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE            
032300     ELSE                                                                 
032400       MOVE NEJ                 TO NYCKLAR-SW                             
032410       MOVE MSGI-IDARTNR        TO W-IDARTNR                              
032420                                   MOD-IDARTNR-UT                         
032430       INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE            
032500     END-IF                                                               
032600                                                                          
032700     .                                                                    
032800     EJECT                                                                
032900 BE-KOLLA-IDRADNR   SECTION.                                              
033000                                                                          
033100     MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR-IN                         
033200                                                                          
033300     IF MID-IDRADNR-IN          NOT = ALL '+'                             
033400       MOVE '7'                 TO MFS-IDPFK                              
033500       MOVE SPACE               TO MFS-KDTRTYP                            
033600     END-IF                                                               
033700                                                                          
033800*    IF MSGI-IDRADNR            NUMERIC AND MSGI-IDRADNR > ZERO           
033810     IF MSGI-IDRADNR            NUMERIC                                   
033900       MOVE MSGI-IDRADNR        TO W-IDRADNR                              
033910                                   MOD-IDRADNR-UT                         
033920       INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE            
034000     ELSE                                                                 
034100       MOVE NEJ                 TO NYCKLAR-SW                             
034200     END-IF                                                               
034300                                                                          
034400     .                                                                    
034500     EJECT                                                                
034600 C-FOERSTA-SIDA SECTION.                                                  
034700                                                                          
034800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
034900     CALL WMEDKONV USING MED-WMEDAREA                                     
035000     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
035100                                                                          
035400     .                                                                    
035500     EJECT                                                                
035600 E-SAMMA-SIDA SECTION.                                                    
035700                                                                          
035800     IF EGEN-MID OR HELP-MID                                              
035900        CONTINUE                                                          
035910        PERFORM MFS-ROER-EJ-FAELT-UT                                      
036000     ELSE                                                                 
036100        PERFORM MFS-RENSA-FAELT-UT                                        
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 F-LAES-VISA-INFO SECTION.                                                
036600                                                                          
036700     PERFORM IMS-GU-WLKREE21                                              
036800                                                                          
036900     IF SEGMENT-SAKNAS                                                    
037000        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
037100        CALL WMEDKONV USING MED-WMEDAREA                                  
037200        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
037300        PERFORM FA-TOEM-BILD                                              
037400     ELSE                                                                 
037500        PERFORM FB-REDIGERA-BILD                                          
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000 FA-TOEM-BILD          SECTION.                                           
038100                                                                          
038200     MOVE +1                          TO INDX                             
038300     PERFORM UNTIL INDX               >  3                                
038400        MOVE SPACE                    TO MOD-TEANMNOT-REG(INDX)           
038500                                         MOD-TEANMNOT-DLR(INDX)           
038510                                         MOD-TEANMNOT-ADM(INDX)           
038600                                         MOD-TEANMNOT-REM(INDX)           
038700                                         MOD-TEANMNOT-RET(INDX)           
038800                                                                          
038900        ADD +1                        TO INDX                             
039000     END-PERFORM                                                          
039100                                                                          
039200     .                                                                    
039300     EJECT                                                                
039400                                                                          
039500 FB-REDIGERA-BILD          SECTION.                                       
039600                                                                          
039700     MOVE +1                          TO INDX                             
039800     PERFORM UNTIL INDX               >  3                                
039900        MOVE TXT-TEANMNOT-REG (INDX)  TO MOD-TEANMNOT-REG(INDX)           
039910        MOVE TXT-TEANMNOT-DLR (INDX)  TO MOD-TEANMNOT-DLR(INDX)           
040000        MOVE TXT-TEANMNOT-ADM (INDX)  TO MOD-TEANMNOT-ADM(INDX)           
040100        MOVE TXT-TEANMNOT-REM (INDX)  TO MOD-TEANMNOT-REM(INDX)           
040200        MOVE TXT-TEANMNOT-RET (INDX)  TO MOD-TEANMNOT-RET(INDX)           
040300                                                                          
040400        ADD +1                        TO INDX                             
040500     END-PERFORM                                                          
040600                                                                          
040700     .                                                                    
040800     EJECT                                                                
041000                                                                          
041100 H-UPPDATERA  SECTION.                                                    
041200                                                                          
041201     IF MID-INPUT                     = SPACE                             
041202        CONTINUE                                                          
041203     ELSE                                                                 
041210       PERFORM IMS-GHU-WLKREE11                                           
041211       IF SEGMENT-FINNS                                                   
041220         IF LEV-FLTEXT                = NEJ                               
041221             MOVE JA                  TO LEV-FLTEXT                       
041222             PERFORM IMS-REPL-WLKREE11                                    
041223         END-IF                                                           
041230                                                                          
041240         PERFORM IMS-GHNP-WLKREE21                                        
041250                                                                          
041300         MOVE +1                      TO INDX                             
041400         PERFORM UNTIL INDX           >  3                                
041500            MOVE MID-TEANMNOT-REG (INDX) TO TXT-TEANMNOT-REG(INDX)        
041510            MOVE MID-TEANMNOT-DLR (INDX) TO TXT-TEANMNOT-DLR(INDX)        
041600            MOVE MID-TEANMNOT-ADM (INDX) TO TXT-TEANMNOT-ADM(INDX)        
041700            MOVE MID-TEANMNOT-REM (INDX) TO TXT-TEANMNOT-REM(INDX)        
041800            MOVE MID-TEANMNOT-RET (INDX) TO TXT-TEANMNOT-RET(INDX)        
041900                                                                          
042000            ADD +1                    TO INDX                             
042100         END-PERFORM                                                      
042110                                                                          
042120         IF SEGMENT-FINNS                                                 
042130             PERFORM IMS-REPL-WLKREE21                                    
042140         ELSE                                                             
042150             MOVE '1'           TO TXT-KDSEGKEY                           
042180             PERFORM IMS-ISRT-WLKREE21                                    
042190         END-IF                                                           
042191                                                                          
042192         MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                           
042193         CALL WMEDKONV USING MED-WMEDAREA                                 
042194         MOVE MED-MFSINF        TO MOD-TEMFSINF                           
042195       ELSE                                                               
042196         MOVE ERR-INFO-MISSING  TO MED-IDMFSFEL                           
042197         CALL WMEDKONV USING MED-WMEDAREA                                 
042198         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
042199         PERFORM FA-TOEM-BILD                                             
042200       END-IF                                                             
042300     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000 I-SVARA-DISPATCHEN SECTION.                                              
043100                                                                          
044100     MOVE OK-BEHANDLAD                TO MSG-KOM-IDMFSMED                 
044300                                                                          
044400     PERFORM IMS-INSERT-DISP-MSG                                          
044900     .                                                                    
045000     EJECT                                                                
088200 MFS-RENSA-FAELT-UT SECTION.                                              
088300                                                                          
088410     MOVE +1                          TO INDX                             
088420     PERFORM UNTIL INDX               >  3                                
088430        MOVE MFS-RENSA-FAELT          TO MOD-TEANMNOT-REG(INDX)           
088440                                         MOD-TEANMNOT-DLR(INDX)           
088441                                         MOD-TEANMNOT-ADM(INDX)           
088450                                         MOD-TEANMNOT-REM(INDX)           
088460                                         MOD-TEANMNOT-RET(INDX)           
088470                                                                          
088480        ADD +1                        TO INDX                             
088490     END-PERFORM                                                          
088491     .                                                                    
088492                                                                          
089200                                                                          
089300 MFS-ROER-EJ-FAELT-UT SECTION.                                            
089400                                                                          
089500     MOVE +1                          TO INDX                             
089600     PERFORM UNTIL INDX               >  3                                
089700        MOVE MFS-ROER-EJ-FAELT        TO MOD-TEANMNOT-REG(INDX)           
089800                                         MOD-TEANMNOT-DLR(INDX)           
089810                                         MOD-TEANMNOT-ADM(INDX)           
089900                                         MOD-TEANMNOT-REM(INDX)           
090000                                         MOD-TEANMNOT-RET(INDX)           
090100                                                                          
090200        ADD +1                        TO INDX                             
090300     END-PERFORM                                                          
090400     .                                                                    
090500                                                                          
090600                                                                          
099600                                                                          
099700* --- IMS SEKTIONER ---                                                   
099800     SKIP3                                                                
099900 IMS-GET-MSG SECTION.                                                     
100000                                                                          
100100     MOVE '  QC' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     SKIP3                                                                
100610 IMS-GN-MSG SECTION.                                                      
100620                                                                          
100630     MOVE '  QD'   TO GODK-STATUSKODER                                    
100640     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
100650     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100660     PERFORM IMS-STATUSKONTROLL                                           
100670     .                                                                    
100680     EJECT                                                                
100690 IMS-INSERT-DISP-MSG SECTION.                                             
100691                                                                          
100692     MOVE '  '  TO GODK-STATUSKODER                                       
100693     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
100694     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
100695     PERFORM IMS-STATUSKONTROLL                                           
100696     .                                                                    
100697     SKIP3                                                                
100700 IMS-INSERT-MSG SECTION.                                                  
100800                                                                          
100900     IF MSGI-IDLAND-SPR = 'GB'                                            
101000       MOVE 'N' TO MFS-KDHUVOMR                                           
101100     END-IF                                                               
101200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101300     MOVE SPACE TO GODK-STATUSKODER                                       
101400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101600     PERFORM IMS-STATUSKONTROLL                                           
101700     .                                                                    
101800     EJECT                                                                
101900                                                                          
103800 IMS-GHU-WLKREE11       SECTION.                                          
103900                                                                          
104000     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
104100          DELIMITED BY SIZE INTO SSA1                                     
104110     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
104120          DELIMITED BY SIZE INTO SSA2                                     
104200     MOVE '  GE'           TO GODK-STATUSKODER                            
104300     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA SSA1 SSA2                
104400     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700                                                                          
104701 IMS-REPL-WLKREE11      SECTION.                                          
104702                                                                          
104703     MOVE '    '           TO GODK-STATUSKODER                            
104704     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
104705     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
104706     PERFORM IMS-STATUSKONTROLL                                           
104707     .                                                                    
104708     EJECT                                                                
104709                                                                          
104710 IMS-GU-WLKREE21       SECTION.                                           
104711                                                                          
104712     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
104713          DELIMITED BY SIZE INTO SSA1                                     
104714     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
104715          DELIMITED BY SIZE INTO SSA2                                     
104716     STRING 'WLKREE21(KDSEGKEY =' W-KDSEGKEY-X ')'                        
104718          DELIMITED BY SIZE INTO SSA3                                     
104719     MOVE '  GE'           TO GODK-STATUSKODER                            
104720     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1 SSA2 SSA3            
104721     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
104722     PERFORM IMS-STATUSKONTROLL                                           
104723     .                                                                    
104724                                                                          
104725 IMS-GHNP-WLKREE21       SECTION.                                         
104726                                                                          
104727     STRING 'WLKREE21(KDSEGKEY =' W-KDSEGKEY-X ')'                        
104728          DELIMITED BY SIZE INTO SSA1                                     
104729     MOVE '  GE'           TO GODK-STATUSKODER                            
104730     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA SSA1                    
104731     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
104732     PERFORM IMS-STATUSKONTROLL                                           
104733     .                                                                    
104734                                                                          
104735 IMS-ISRT-WLKREE21       SECTION.                                         
104736                                                                          
104737     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
104740          DELIMITED BY SIZE INTO SSA1                                     
104750     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
104760          DELIMITED BY SIZE INTO SSA2                                     
104770     MOVE 'WLKREE21'           TO SSA3                                    
104790     MOVE '    '           TO GODK-STATUSKODER                            
104791     CALL CBLTDLI USING ISRT KREE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
104792     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
104793     PERFORM IMS-STATUSKONTROLL                                           
104794     .                                                                    
104795                                                                          
104800 IMS-REPL-WLKREE21      SECTION.                                          
104900                                                                          
105000     MOVE '    '           TO GODK-STATUSKODER                            
105100     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA                         
105200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSKONTROLL                                           
105400     .                                                                    
105500     EJECT                                                                
105600                                                                          
105700                                                                          
115500 IMS-STATUSKONTROLL SECTION.                                              
115600                                                                          
115700     SET STATUS-IX TO 1                                                   
115800     SEARCH GODK-STATUS                                                   
115900       AT END                                                             
116000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
116100         DELIMITED BY SIZE INTO FELTEXT                                   
116200         CALL FELLOG                                                      
116300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
116400         CONTINUE                                                         
116500     END-SEARCH                                                           
116600     .                                                                    
