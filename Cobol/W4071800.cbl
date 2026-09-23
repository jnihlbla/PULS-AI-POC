001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4071800.                                                
001600 AUTHOR.         OLSSON SUSANNE.                                          
001700 DATE-WRITTEN.   05/09/20.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
001910*                                                                         
002000*    FUNKTION:                                                            
002100*        VISA/UPPDATERA TEXTINFORMATION TILL DE MAIL SOM SKAPAS           
002200*        NÄR EN KREDITRAD GÅR VIDARE TILL NÄSTA NIVÅ FÖR ATTEST           
002300*        ELLER NEKAS OCH MAIL SKICKAS TILL ISSUER/REKL.BEHANDLARE.        
002400*                                                                         
002510*        PROGRAMMET LÄSER      WDR5                                       
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T718 W4T718U                                      
002900*        MID:         W4I71801                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O71801                                            
003210*                                                                         
003220*    E-TRACKER: 2072166  DATE 20050920                                    
003230*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4071800'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004620 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
004630 77  MAX-INDX                    PIC S9(4)   VALUE +5  COMP SYNC.         
004700                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '4718'.                
006100     88  GODK-MID                            VALUE '4718'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007600*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008210     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
008220     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008230     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008240     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008250     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700     SKIP3                                                                
008800*01 -COPY WMSGINIT                                                        
008900     EJECT                                                                
009000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009100*                                                                         
009200 01  SPAR-AREA.                                                           
009300     03  SPAR-IDTRANS           PIC X(4)    VALUE '4718'.                 
009500     EJECT                                                                
009600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009900     SKIP3                                                                
010000*01  MID -COPY W4I71801                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010300     SKIP3                                                                
010400*01  -COPY WMSGAREA                                                       
010500     EJECT                                                                
010600     03  MOD REDEFINES MSG-AREA.                                          
010700*      05  -COPY W4O71801                                                 
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011000     SKIP3                                                                
011100*01  -COPY WMFSAREA                                                       
011200     EJECT                                                                
011300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011820                                                                          
011830     03  W-WDGXKEY-4103-X.                                                
011840         05  W-IDHTYP            PIC X(4)    VALUE '4103'.                
011850         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011860         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011870         05  W-IDRAPPNR          PIC  9(7)   VALUE ZERO.                  
011880         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
011890                                                                          
011891     03  W-KEY4104-X.                                                     
011892         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011893         05  W-KDKRENOT          PIC X(2)    VALUE SPACE.                 
011894                                                                          
011895     03  W-KDSEGKEY-X.                                                    
011896         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011897                                                                          
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013010 01  SSA3                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700                                                                          
013801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
013802 01  DLI-IO-WDGX4103.                                                     
013803*    03  -COPY WDGX4103                                                   
013804     EJECT                                                                
013805 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
013806 01  DLI-IO-WDGX4104.                                                     
013807*    03  -COPY WDGX4104                                                   
013808     EJECT                                                                
013809 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4105'.                    
013810 01  DLI-IO-WDGX4105.                                                     
013811*    03  -COPY WDGX4105                                                   
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300*01  -COPY W0009   -PRE MSG-                                              
014400*01  -COPY W0008   -PRE WDP7-                                             
014500     05  FILLER                  PIC X.                                   
014601                                                                          
014602*01  -COPY W0008  -PRE 4103-                                              
014610     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014801 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 4103-PCB.                     
014802 MAIN SECTION.                                                            
014810     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 4103-PCB.                     
014900                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FINNS                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-KOLLA-NYCKLAR                                            
015500       IF NYCKLAR-OK                                                      
015600         IF MFS-UPDATE                                                    
015700            PERFORM H-UPPDATERA                                           
015800         ELSE                                                             
015900            IF MFS-FIRST                                                  
015910               PERFORM C-FOERSTA-SIDA                                     
015920            ELSE                                                          
015930               PERFORM E-SAMMA-SIDA                                       
015940            END-IF                                                        
015950         END-IF                                                           
015960         IF MFS-FIRST OR MFS-UPDATE                                       
015970            PERFORM F-LAES-VISA-INFO                                      
015980         END-IF                                                           
016100       END-IF                                                             
016400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71801 + 4                      
016500       PERFORM IMS-INSERT-MSG                                             
016600     END-IF                                                               
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
017500     IF MSG-DUBBLA-TRANSKODER                                             
017600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I71801                 
017700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017900     ELSE                                                                 
018000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I71801                  
018100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018300     END-IF                                                               
018400                                                                          
018500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018800                                                                          
018900     MOVE LOW-VALUE TO MSG-AREA                                           
019000     MOVE 'W4O718N1' TO MFS-IDMOD                                         
019100     MOVE '4718' TO MOD-IDTRANS                                           
019200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019300                                                                          
019400     IF EGEN-MID OR HELP-MID                                              
019500       CONTINUE                                                           
019600     ELSE                                                                 
019700       MOVE SPACE TO MFS-KDTRTYP                                          
019800       MOVE '7' TO MFS-IDPFK                                              
019900     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 B-KOLLA-NYCKLAR SECTION.                                                 
020500                                                                          
020600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020700     MOVE '001'             TO MSGI-KDCALL                                
020800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021000     MOVE '4718'            TO MSGI-IDTRANS                               
021100     IF GODK-MID                                                          
021201         MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                             
021203         MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                            
021204         MOVE MID-IDRAPPNR-IN TO MSGI-IDRAPPNR                            
021210         MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                            
021220         MOVE MID-KDKRENOT-IN TO MSGI-KDKRENOT                            
021300     END-IF                                                               
021400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021600                                                                          
021700*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
021800     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021900                                                                          
022000     MOVE JA TO NYCKLAR-SW                                                
022100                                                                          
022201                                                                          
022202*    -- KONTROLL AV IDDISTR                                               
022203     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
022204                                                                          
022205     IF MID-IDDISTR-IN NOT = ALL '+'                                      
022206       MOVE '7'         TO MFS-IDPFK                                      
022207       MOVE SPACE       TO MFS-KDTRTYP                                    
022208     END-IF                                                               
022209     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
022210     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
022211       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
022212     ELSE                                                                 
022213       MOVE NEJ TO NYCKLAR-SW                                             
022214     END-IF                                                               
022215                                                                          
022217*    -- KONTROLL AV IDKUNDNR                                              
022218     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
022219                                                                          
022220     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
022221       MOVE '7'         TO MFS-IDPFK                                      
022222       MOVE SPACE       TO MFS-KDTRTYP                                    
022223     END-IF                                                               
022224     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
022225     IF MSGI-IDKUNDNR NUMERIC                                             
022226       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
022227     ELSE                                                                 
022228       MOVE NEJ TO NYCKLAR-SW                                             
022229     END-IF                                                               
022230                                                                          
022231*    -- KONTROLL AV IDRAPPNR                                              
022232     MOVE MFS-RENSA-FAELT TO MOD-IDRAPPNR-IN                              
022233                                                                          
022234     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
022235       MOVE '7'         TO MFS-IDPFK                                      
022236       MOVE SPACE       TO MFS-KDTRTYP                                    
022237     END-IF                                                               
022238     INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
022239     IF MSGI-IDRAPPNR NUMERIC AND MSGI-IDRAPPNR > ZERO                    
022240       MOVE MSGI-IDRAPPNR TO W-IDRAPPNR                                   
022241     ELSE                                                                 
022242       MOVE NEJ TO NYCKLAR-SW                                             
022243     END-IF                                                               
022244                                                                          
022245*    -- KONTROLL AV IDDC                                                  
022246     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
022247                                                                          
022248     IF MID-IDDC-IN NOT = ALL '+'                                         
022249       MOVE '7'         TO MFS-IDPFK                                      
022250       MOVE SPACE       TO MFS-KDTRTYP                                    
022251     END-IF                                                               
022260     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
022301                                                                          
022302*    -- KONTROLL AV KDKRENOT                                              
022303     MOVE MFS-RENSA-FAELT TO MOD-KDKRENOT-IN                              
022304                                                                          
022305     IF MID-KDKRENOT-IN NOT = ALL '+'                                     
022306       MOVE '7'         TO MFS-IDPFK                                      
022307       MOVE SPACE       TO MFS-KDTRTYP                                    
022315     END-IF                                                               
022317     MOVE MSGI-KDKRENOT TO W-KDKRENOT                                     
022318                                                                          
022319     IF GODK-MID OR NYCKLAR-OK                                            
022323       MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                            
022324       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
022325       MOVE MSGI-IDKUNDNR    TO MOD-IDKUNDNR-UT                           
022326       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
022327       MOVE MSGI-IDRAPPNR    TO MOD-IDRAPPNR-UT                           
022328       INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE            
022329       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
022330       MOVE MSGI-KDKRENOT    TO MOD-KDKRENOT-UT                           
022331     ELSE                                                                 
022332       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
022333                               MOD-IDKUNDNR-UT                            
022334                               MOD-IDRAPPNR-UT                            
022335                               MOD-IDDC-UT                                
022336                               MOD-KDKRENOT-UT                            
022340     END-IF                                                               
022400                                                                          
022500     IF NYCKLAR-FEL                                                       
022600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022700       CALL WMEDKONV USING MED-WMEDAREA                                   
022800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023000       PERFORM MFS-RENSA-FAELT-UT                                         
023100     END-IF                                                               
023200     .                                                                    
023400     EJECT                                                                
023500 C-FOERSTA-SIDA SECTION.                                                  
023510                                                                          
023520     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023530     CALL WMEDKONV USING MED-WMEDAREA                                     
023540     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
023550                                                                          
023560     .                                                                    
023570     EJECT                                                                
023580 E-SAMMA-SIDA SECTION.                                                    
023590                                                                          
023591     IF EGEN-MID OR HELP-MID                                              
023592       IF MID-INPUT NOT = ALL '+'                                         
023593         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023594         CALL WMEDKONV USING MED-WMEDAREA                                 
023595         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
023596         PERFORM MFS-ROER-EJ-FAELT-UT                                     
023597         PERFORM MFS-LAES-IN-IGEN                                         
023598       ELSE                                                               
023599         PERFORM MFS-ROER-EJ-FAELT-UT                                     
023600       END-IF                                                             
023601     ELSE                                                                 
023602       PERFORM MFS-RENSA-FAELT-UT                                         
023603     END-IF                                                               
023604     .                                                                    
023605     EJECT                                                                
023610 F-LAES-VISA-INFO SECTION.                                                
023700                                                                          
023800     PERFORM IMS-GU-WDGX4104-KVAL                                         
023900                                                                          
024000     IF SEGMENT-SAKNAS                                                    
024110        MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                           
024200        CALL WMEDKONV USING MED-WMEDAREA                                  
024300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024400        PERFORM MFS-RENSA-FAELT-UT                                        
024500     ELSE                                                                 
024611                                                                          
024612       PERFORM IMS-GNP-WDGX4105                                           
024614                                                                          
024615       IF SEGMENT-FINNS                                                   
024620         MOVE +1                          TO INDX                         
024630         PERFORM UNTIL INDX               > MAX-INDX                      
024640            MOVE 4105-TEMEMO (INDX)  TO MOD-TEMEMO (INDX)                 
024690                                                                          
024691            ADD +1                        TO INDX                         
024692         END-PERFORM                                                      
024693       ELSE                                                               
024694        PERFORM MFS-RENSA-FAELT-UT                                        
024700       END-IF                                                             
024710     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
025000 H-UPPDATERA  SECTION.                                                    
025100                                                                          
025210     IF MID-INPUT                     = ALL '+'                           
025310       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025320       CALL WMEDKONV USING MED-WMEDAREA                                   
025330       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025400     ELSE                                                                 
025500       PERFORM IMS-GU-WDGX4104-KVAL                                       
025510       IF SEGMENT-FINNS                                                   
025600         PERFORM IMS-GHNP-WDGX4105                                        
025940                                                                          
025950         MOVE +1                      TO INDX                             
025960         PERFORM UNTIL INDX           > MAX-INDX                          
025961           IF SEGMENT-FINNS                                               
025962             IF MID-TEMEMO (INDX) = ALL '+'                               
025963               CONTINUE                                                   
025965             ELSE                                                         
025966               MOVE MID-TEMEMO (INDX) TO 4105-TEMEMO (INDX)               
025967             END-IF                                                       
025968           ELSE                                                           
025970             IF MID-TEMEMO (INDX) = ALL '+'                               
025980               MOVE SPACE             TO 4105-TEMEMO (INDX)               
025981             ELSE                                                         
025990               MOVE MID-TEMEMO (INDX) TO 4105-TEMEMO (INDX)               
025991             END-IF                                                       
025992           END-IF                                                         
025994            ADD +1                    TO INDX                             
025995         END-PERFORM                                                      
025996                                                                          
025997         IF SEGMENT-FINNS                                                 
025998            PERFORM IMS-REPL-WDGX4105                                     
025999         ELSE                                                             
026000            MOVE '1'           TO 4105-KDSEGKEY                           
026001            PERFORM IMS-ISRT-WDGX4105                                     
026002         END-IF                                                           
026003                                                                          
026004         MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                           
026005         CALL WMEDKONV USING MED-WMEDAREA                                 
026006         MOVE MED-MFSINF        TO MOD-TEMFSINF                           
026007       ELSE                                                               
026008         MOVE ERR-INFO-MISSING  TO MED-IDMFSFEL                           
026009         CALL WMEDKONV USING MED-WMEDAREA                                 
026010         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
026011         PERFORM MFS-RENSA-FAELT-UT                                       
026013       END-IF                                                             
026014     END-IF                                                               
026015     .                                                                    
026016     EJECT                                                                
026020 MFS-RENSA-FAELT-UT SECTION.                                              
026100                                                                          
026200*    --- ALLA UTDATA-FÄLT                                                 
026510                                                                          
026520     MOVE +1                          TO INDX                             
026530     PERFORM UNTIL INDX               > MAX-INDX                          
026540        MOVE MFS-RENSA-FAELT          TO MOD-TEMEMO (INDX)                
026590                                                                          
026591        ADD +1                        TO INDX                             
026592     END-PERFORM                                                          
026600     .                                                                    
026800     SKIP3                                                                
027600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027700                                                                          
027800*    --- ALLA UTDATA-FÄLT                                                 
027900                                                                          
028200     MOVE +1                          TO INDX                             
028210     PERFORM UNTIL INDX               > MAX-INDX                          
028220        MOVE MFS-ROER-EJ-FAELT        TO MOD-TEMEMO (INDX)                
028270                                                                          
028280        ADD +1                        TO INDX                             
028290     END-PERFORM                                                          
028300     .                                                                    
028400 MFS-LAES-IN-IGEN  SECTION.                                               
028500                                                                          
028600*    --- ALLA UTDATA-FÄLT                                                 
028700                                                                          
028800     MOVE +1                          TO INDX                             
028900     PERFORM UNTIL INDX               > MAX-INDX                          
029000        MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-TEMEMO-ATTR (INDX)           
029100                                                                          
029200        ADD +1                        TO INDX                             
029300     END-PERFORM                                                          
029400     .                                                                    
030600* --- IMS SEKTIONER ---                                                   
030700     SKIP3                                                                
030800 IMS-GET-MSG SECTION.                                                     
030900                                                                          
031000     MOVE '  QC' TO GODK-STATUSKODER                                      
031100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031500     SKIP3                                                                
031600 IMS-INSERT-MSG SECTION.                                                  
031700                                                                          
032100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032200     MOVE SPACE TO GODK-STATUSKODER                                       
032300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032500     PERFORM IMS-STATUSKONTROLL                                           
032600     .                                                                    
032701     EJECT                                                                
032810 IMS-GU-WDGX4104-KVAL SECTION.                                            
032820                                                                          
032830     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
032840          DELIMITED BY SIZE INTO SSA1                                     
032850     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
032860          DELIMITED BY SIZE INTO SSA2                                     
032870     MOVE '  GE' TO GODK-STATUSKODER                                      
032880     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2             
032890     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032891     PERFORM IMS-STATUSKONTROLL                                           
032892     .                                                                    
032893     EJECT                                                                
032894 IMS-GNP-WDGX4105 SECTION.                                                
032895                                                                          
032896     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
032897          DELIMITED BY SIZE INTO SSA1                                     
032898     MOVE 'WDGX4105 ' TO SSA2                                             
032899     MOVE '  GE' TO GODK-STATUSKODER                                      
032900     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4105 SSA1 SSA2            
032901     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032902     PERFORM IMS-STATUSKONTROLL                                           
032903     .                                                                    
032904     EJECT                                                                
032905 IMS-GHNP-WDGX4105 SECTION.                                               
032906                                                                          
032907     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
032908          DELIMITED BY SIZE INTO SSA1                                     
032909     MOVE 'WDGX4105 ' TO SSA2                                             
032910     MOVE '  GE' TO GODK-STATUSKODER                                      
032911     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4105 SSA1 SSA2           
032912     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032913     PERFORM IMS-STATUSKONTROLL                                           
032914     .                                                                    
032915     EJECT                                                                
032916 IMS-REPL-WDGX4105 SECTION.                                               
032917                                                                          
032918     MOVE '  ' TO GODK-STATUSKODER                                        
032919     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4105                     
032920     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032921     PERFORM IMS-STATUSKONTROLL                                           
032922     .                                                                    
032923     EJECT                                                                
032924 IMS-ISRT-WDGX4105 SECTION.                                               
032925                                                                          
032926     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
032927          DELIMITED BY SIZE INTO SSA1                                     
032928     STRING 'WDGX4104(KEY4104  =' W-KEY4104-X ')'                         
032929          DELIMITED BY SIZE INTO SSA2                                     
032930     MOVE 'WDGX4105 ' TO SSA3                                             
032931     MOVE '  II' TO GODK-STATUSKODER                                      
032932     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4105 SSA1 SSA2           
032933                                                      SSA3                
032934     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032935     PERFORM IMS-STATUSKONTROLL                                           
032936     .                                                                    
032937     EJECT                                                                
032940 IMS-STATUSKONTROLL SECTION.                                              
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GODK-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500         DELIMITED BY SIZE INTO FELTEXT                                   
033600         CALL FELLOG                                                      
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    
