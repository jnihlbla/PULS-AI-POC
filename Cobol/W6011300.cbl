000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011300.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/02/24.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR R31 TRANSAKTIONER                                         
001100*        FLERA LEVERANTÖRER, FLERA AVIER OCH FLERA ARTIKLAR               
001200*                                                                         
001300*    SUB PROGRAMMET W611REG UPPDATERAR W6INLA (W6D1)                      
001400*                           LÄSER      WLLEVA (WDF1)                      
001500*                           LÄSER      WLARTC (WDK6)                      
001600*                           LÄSER      WLBENA (WDD3)                      
001700*                           LÄSER      WLINLB (WDD9)                      
001800*                           LÄSER      WDB6                               
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T113                                              
002200*        MID:         W6I11301                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O11301                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6011300'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +770  COMP SYNC.        
004500 77  RAD-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77  MAX-RAD                     PIC S9(4)  VALUE +12   COMP SYNC.        
004700 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005200 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
005300 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
005400 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
005500 77  WS-IDKONTO                  PIC X(10)   VALUE SPACE.                 
005600 77  WS-IDKST                    PIC X(5)    VALUE SPACE.                 
005700 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
005800 77  WS-FLGODK                   PIC X(1)    VALUE SPACE.                 
005900 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006500 77  INDATA-IFYLLT-SW            PIC X       VALUE 'N'.                   
006600     88  INDATA-IFYLLT                       VALUE 'J'.                   
006700     88  INDATA-EJ-IFYLLT                    VALUE 'N'.                   
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007400     88  ALLT-OK                             VALUE 'J'.                   
007500                                                                          
007600 77  W611REG-SW                  PIC X       VALUE 'N'.                   
007700     88  W611REG-OK                          VALUE 'J'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '6113'.                
008100     88  GODK-MID                            VALUE '6111' '6112'          
008200                                                   '6113' '6114'          
008300                                                   '6115' '6116'          
008400                                                   '6118' '6119'.         
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  W611REG                 PIC X(8)    VALUE 'W611REG'.             
009300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     EJECT                                                                
009500*01 -COPY WMSGINIT                                                        
009600     SKIP3                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000 01  MESSAGE-CODES.                                                       
010100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010300     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
010400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010600     03  ERR-IN-LINE-ONE         PIC X(3)    VALUE '184'.                 
010700     03  INF-ADVICE-N-EXIST      PIC X(3)    VALUE '201'.                 
010800     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011000     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.                 
011100                                                                          
011200     EJECT                                                                
011300*01  -COPY W611REG0                                                       
011400*01  FILLER -COPY W611REG3    -RED LAENK-W611REG0.                        
011500     EJECT                                                                
011600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011900     SKIP3                                                                
012000*01  MID -COPY W6I11301                                                   
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600     03  MOD REDEFINES MSG-AREA.                                          
012700*      05  -COPY W6O11301                                                 
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600 01  KEYS-TO-DLI.                                                         
013700     03  W-IDDC-B6-X.                                                     
013800         05 W-IDDC-B6                  PIC X(2).                          
013900     03  W-IDDC-B6-LEV-X.                                                 
014000         05 W-IDDC-B6-LEV              PIC X(5).                          
014100                                                                          
014200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014300 01   DLI-IO-AREA-B601.                                                   
014400*     03  -COPY WDB601                                                    
014500                                                                          
014600 01  FILLER               PIC X(16)   VALUE 'WDB601 LEV'.                 
014700 01   DLI-IO-AREA-B601-LEV.                                               
014800*     03  -COPY WDB601   -PRE LEV-                                        
014900                                                                          
015000 01  SSA1                        PIC X(256).                              
015100                                                                          
015200 01  STATUS-WS                   PIC XX.                                  
015300     88 SEGMENT-FINNS                        VALUE '  '.                  
015400     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
015500     88 BASEN-SLUT                           VALUE 'GB'.                  
015600                                                                          
015700 01  GODK-STATUSKODER.                                                    
015800     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
015900                                                                          
016000     EJECT                                                                
016100*    --- IMS FUNKTIONSKODER                                               
016200*01  -COPY W0003                                                          
016300     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600 01 -COPY W0009     -PRE MSG-                                             
016700                                                                          
016800 01  USEA-PCB                    PIC X.                                   
016900                                                                          
017000*01  -COPY W0008   -PRE WDB6-LEV-                                         
017100     05  FILLER                  PIC X.                                   
017200                                                                          
017300*01  -COPY W0008   -PRE WDB6-                                             
017400     05  FILLER                  PIC X.                                   
017500                                                                          
017600*   PCB'ER FÖR SUB PGM W611REG                                            
017700                                                                          
017800 01  REG-INLA1-PCB               PIC X.                                   
017900                                                                          
018000 01  REG-INLA2-PCB               PIC X.                                   
018100                                                                          
018200 01  REG-INLA3-PCB               PIC X.                                   
018300                                                                          
018400 01  REG-LEVA-PCB                PIC X.                                   
018500                                                                          
018600 01  REG-ARTC-PCB                PIC X.                                   
018700                                                                          
018800 01  REG-BENA-PCB                PIC X.                                   
018900                                                                          
019000 01  REG-WDD9-PCB                PIC X.                                   
019100                                                                          
019110 01  REG-WDK7-PCB                PIC X.                                   
019120                                                                          
019130 01  REG-WDB6-PCB                PIC X.                                   
019200     EJECT                                                                
019300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
019400                           WDB6-LEV-PCB   WDB6-PCB                        
019500                           REG-INLA1-PCB  REG-INLA2-PCB                   
019600                           REG-INLA3-PCB  REG-LEVA-PCB                    
019700                                          REG-ARTC-PCB                    
019800                           REG-BENA-PCB   REG-WDD9-PCB                    
019810                           REG-WDK7-PCB   REG-WDB6-PCB.                   
019900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
020000                           WDB6-LEV-PCB   WDB6-PCB                        
020100                           REG-INLA1-PCB  REG-INLA2-PCB                   
020200                           REG-INLA3-PCB  REG-LEVA-PCB                    
020300                                          REG-ARTC-PCB                    
020400                           REG-BENA-PCB   REG-WDD9-PCB                    
020410                           REG-WDK7-PCB   REG-WDB6-PCB.                   
020500     PERFORM IMS-GET-MSG                                                  
020600     IF SEGMENT-FINNS                                                     
020700       PERFORM A-INIT                                                     
020800       PERFORM B-FLYTTA-NYCKLAR                                           
020900       IF NYCKLAR-OK                                                      
021000         IF MFS-ENTER AND EGEN-MID                                        
021100             PERFORM G-KOLLA-INPUT-UPPDATERA                              
021200           ELSE                                                           
021300             IF HELP-MID                                                  
021400                 PERFORM F-LAES-VISA-INFO                                 
021500              ELSE                                                        
021600                 PERFORM MFS-RENSA-FAELT-IN                               
021700             END-IF                                                       
021800         END-IF                                                           
021900       END-IF                                                             
022000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
022100       PERFORM IMS-INSERT-MSG                                             
022200     END-IF                                                               
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     IF MSG-DUBBLA-TRANSKODER                                             
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11301                 
023200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023400     ELSE                                                                 
023500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11301                  
023600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023800     END-IF                                                               
023900                                                                          
024000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
024200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024300                                                                          
024400     MOVE LOW-VALUE TO MSG-AREA                                           
024500     MOVE 'W6O113N1' TO MFS-IDMOD                                         
024600     MOVE '6113' TO MOD-IDTRANS                                           
024700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024800                                                                          
024900     IF EGEN-MID OR HELP-MID                                              
025000       CONTINUE                                                           
025100      ELSE                                                                
025200       MOVE SPACE TO MFS-KDTRTYP                                          
025300       MOVE '7' TO MFS-IDPFK                                              
025400     END-IF                                                               
025500                                                                          
025600     MOVE NEJ                  TO INDATA-IFYLLT-SW                        
025700     MOVE JA                   TO W611REG-SW                              
025800     MOVE SPACE                TO REG3-IDTRANS                            
025900                                  REG3-IDLBBET                            
026000                                                                          
026100     MOVE +1                   TO RAD-IX                                  
026200     PERFORM UNTIL RAD-IX      >  MAX-RAD                                 
026300         MOVE JA               TO REG3-IDLEVNR-OK (RAD-IX)                
026400                                  REG3-KDRT-OK    (RAD-IX)                
026500                                  REG3-TIAVIDAT-OK(RAD-IX)                
026600                                  REG3-IDARTNR-OK (RAD-IX)                
026700                                  REG3-IDFS-OK    (RAD-IX)                
026800         MOVE ZERO             TO REG3-KDRT       (RAD-IX)                
026900                                  REG3-TIAVIDAT   (RAD-IX)                
027000                                  REG3-IDARTNR    (RAD-IX)                
027100                                  REG3-KVAVIS     (RAD-IX)                
027200         MOVE SPACE            TO REG3-IDFS       (RAD-IX)                
027300                                  REG3-IDMFSFEL   (RAD-IX)                
027400                                  REG3-IDLEVNR    (RAD-IX)                
027500         ADD +1                TO RAD-IX                                  
027600     END-PERFORM                                                          
027700     PERFORM AA-INIT-NYCKLAR                                              
027800                                                                          
027900     IF MSGI-IDLAND-SPR = 'GB'                                            
028000       MOVE +2 TO SPRAK-IX                                                
028100       MOVE 'GB ' TO MED-IDSKYLT                                          
028200     ELSE                                                                 
028300       MOVE +1 TO SPRAK-IX                                                
028400       MOVE 'S  ' TO MED-IDSKYLT                                          
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800*----------------------------------------------------------------*        
028900 AA-INIT-NYCKLAR SECTION.                                                 
029000                                                                          
029100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
029200     MOVE '001'                  TO MSGI-KDCALL                           
029300     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
029400     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
029500     MOVE '6113'                 TO MSGI-IDTRANS                          
029600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029700     .                                                                    
029800     EJECT                                                                
029900 B-FLYTTA-NYCKLAR SECTION.                                                
030000                                                                          
030100     MOVE JA                   TO NYCKLAR-SW                              
030200                                                                          
030300     PERFORM BA-FLYTTA-IDLEVNR                                            
030400     PERFORM BB-FLYTTA-IDFS                                               
030500     PERFORM BC-FLYTTA-TIAVIDAT                                           
030600     PERFORM BD-FLYTTA-IDLBBET                                            
030700     PERFORM BE-FLYTTA-FLGODK                                             
030800     PERFORM BF-FLYTTA-ADINLOMR-PRT                                       
030900     PERFORM BG-FLYTTA-IDDC                                               
031000     PERFORM BI-KOLLA-SDC-NDC-LEV                                         
031100                                                                          
031200     PERFORM BH-FLYTTA-OEVRIGA-NYCKLAR                                    
031300                                                                          
031400     IF NYCKLAR-OK                                                        
031500         MOVE WS-IDLEVNR       TO MOD-IDLEVNR-UT                          
031600         MOVE WS-IDFS          TO MOD-IDFS-UT                             
031700         MOVE WS-TIAVIDAT      TO MOD-TIAVIDAT-UT                         
031800         INSPECT MOD-TIAVIDAT-UT REPLACING LEADING ZERO BY SPACE          
031900         MOVE WS-ADINLOMR-PRT  TO MOD-ADINLOMR-PRT-UT                     
032000         MOVE WS-IDLBBET       TO MOD-IDLBBET-UT                          
032100         MOVE WS-FLGODK        TO MOD-FLGODK-UT                           
032200*        MOVE DCS-IDDC         TO MOD-IDDC-UT                             
032300         MOVE W-IDDC-B6        TO MOD-IDDC-UT                             
032400      ELSE                                                                
032500         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-UT                          
032600                                  MOD-IDFS-UT                             
032700                                  MOD-TIAVIDAT-UT                         
032800                                  MOD-ADINLOMR-PRT-UT                     
032900                                  MOD-IDLBBET-UT                          
033000                                  MOD-FLGODK-UT                           
033100                                  MOD-IDDC-UT                             
033200*        + ÖVRIGA SPAR-NYCKLAR                                            
033300                                  MOD-KDRT-UT                             
033400                                  MOD-FLKLIVIS-UT                         
033500                                  MOD-IDLOPNRM-UT                         
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 BA-FLYTTA-IDLEVNR SECTION.                                               
034000                                                                          
034100     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
034200                                                                          
034300     IF MID-IDLEVNR-IN         = ALL '+'                                  
034400         MOVE MID-IDLEVNR-UT   TO WS-IDLEVNR                              
034500     ELSE                                                                 
034600         MOVE MID-IDLEVNR-IN   TO WS-IDLEVNR                              
034700         MOVE SPACE            TO MFS-KDTRTYP                             
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 BB-FLYTTA-IDFS    SECTION.                                               
035200                                                                          
035300     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
035400                                                                          
035500     IF MID-IDFS-IN            = ALL '+'                                  
035600         MOVE MID-IDFS-UT      TO WS-IDFS                                 
035700     ELSE                                                                 
035800         MOVE MID-IDFS-IN      TO WS-IDFS                                 
035900         MOVE SPACE            TO MFS-KDTRTYP                             
036000     END-IF                                                               
036100                                                                          
036200     .                                                                    
036300     EJECT                                                                
036400 BC-FLYTTA-TIAVIDAT SECTION.                                              
036500                                                                          
036600     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
036700                                                                          
036800     IF MID-TIAVIDAT-IN        = ALL '+'                                  
036900         MOVE MID-TIAVIDAT-UT  TO WS-TIAVIDAT                             
037000         INSPECT WS-TIAVIDAT REPLACING LEADING SPACE BY ZERO              
037100     ELSE                                                                 
037200         MOVE MID-TIAVIDAT-IN  TO WS-TIAVIDAT                             
037300         MOVE SPACE            TO MFS-KDTRTYP                             
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 BD-FLYTTA-IDLBBET SECTION.                                               
037800                                                                          
037900     MOVE MFS-RENSA-FAELT        TO MOD-IDLBBET-IN                        
038000                                                                          
038100     IF EGEN-MID OR HELP-MID                                              
038200         IF MID-IDLBBET-IN       = ALL '+'                                
038300             MOVE MID-IDLBBET-UT TO WS-IDLBBET                            
038400         ELSE                                                             
038500             MOVE MID-IDLBBET-IN TO WS-IDLBBET                            
038600             MOVE SPACE          TO MFS-KDTRTYP                           
038700         END-IF                                                           
038800      ELSE                                                                
038900         MOVE SPACE              TO WS-IDLBBET                            
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300 BE-FLYTTA-FLGODK  SECTION.                                               
039400                                                                          
039500     MOVE MFS-RENSA-FAELT      TO MOD-FLGODK-IN                           
039600                                                                          
039700     IF EGEN-MID                                                          
039800         IF MID-FLGODK-IN      = ALL '+'                                  
039900             MOVE SPACE        TO WS-FLGODK                               
040000         ELSE                                                             
040100             MOVE MID-FLGODK-IN TO WS-FLGODK                              
040200             MOVE SPACE        TO MFS-KDTRTYP                             
040300         END-IF                                                           
040400      ELSE                                                                
040500         MOVE SPACE            TO WS-FLGODK                               
040600     END-IF                                                               
040700                                                                          
040800     IF WS-FLGODK = SPACE  OR  JA  OR  YES  OR  NEJ                       
040900         MOVE WS-FLGODK        TO REG3-FLGODK                             
041000         IF REG3-FLGODK = YES                                             
041100            MOVE    JA         TO REG3-FLGODK                             
041200         END-IF                                                           
041300      ELSE                                                                
041400         MOVE NEJ              TO NYCKLAR-SW                              
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 BF-FLYTTA-ADINLOMR-PRT SECTION.                                          
041900                                                                          
042000     MOVE MFS-RENSA-FAELT          TO MOD-ADINLOMR-PRT-IN                 
042100                                                                          
042200     IF MID-ADINLOMR-PRT-IN        = ALL '+'                              
042300         MOVE MID-ADINLOMR-PRT-UT  TO WS-ADINLOMR-PRT                     
042400      ELSE                                                                
042500         MOVE MID-ADINLOMR-PRT-IN  TO WS-ADINLOMR-PRT                     
042600         MOVE SPACE                TO MFS-KDTRTYP                         
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 BG-FLYTTA-IDDC   SECTION.                                                
043100                                                                          
043200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
043300                                                                          
043400     IF MID-IDDC-IN = ALL '+'                                             
043500       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
043600     ELSE                                                                 
043700       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
043800       MOVE    SPACE    TO MFS-KDTRTYP                                    
043900     END-IF                                                               
044000     PERFORM IMS-GU-WDB601                                                
044100                                                                          
044200     IF DCS-CDC OR DCS-CDC-TR                                             
044300       MOVE  DCS-IDDC  TO REG3-IDDC                                       
044400     ELSE                                                                 
044500       MOVE    NEJ     TO NYCKLAR-SW                                      
044600       MOVE   SPACE    TO DCS-IDDC                                        
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 BH-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
045100     SKIP2                                                                
045200     MOVE MFS-RENSA-FAELT      TO MOD-KDRT-IN                             
045300                                  MOD-FLKLIVIS-IN                         
045400                                  MOD-IDLOPNRM-IN                         
045500                                                                          
045600     IF MID-KDRT-IN       = ALL '+'                                       
045700         MOVE MID-KDRT-UT      TO MOD-KDRT-UT                             
045800     ELSE                                                                 
045900         MOVE MID-KDRT-IN      TO MOD-KDRT-UT                             
046000     END-IF                                                               
046100                                                                          
046200     IF MID-FLKLIVIS-IN   = ALL '+'                                       
046300         MOVE MID-FLKLIVIS-UT  TO MOD-FLKLIVIS-UT                         
046400     ELSE                                                                 
046500         MOVE MID-FLKLIVIS-IN  TO MOD-FLKLIVIS-UT                         
046600     END-IF                                                               
046700                                                                          
046800     IF MID-IDLOPNRM-IN   = ALL '+'                                       
046900         MOVE MID-IDLOPNRM-UT  TO MOD-IDLOPNRM-UT                         
047000     ELSE                                                                 
047100         MOVE MID-IDLOPNRM-IN  TO MOD-IDLOPNRM-UT                         
047200     END-IF                                                               
047300                                                                          
047400     .                                                                    
047500     EJECT                                                                
047600 BI-KOLLA-SDC-NDC-LEV  SECTION.                                           
047700                                                                          
047800     IF WS-IDLEVNR NOT = SPACE                                            
047900        MOVE WS-IDLEVNR TO W-IDDC-B6-LEV                                  
048000        PERFORM IMS-GU-WDB601-LEV                                         
048100        IF SEGMENT-FINNS                                                  
048200           MOVE NEJ TO NYCKLAR-SW                                         
048300        END-IF                                                            
048400     END-IF                                                               
048500     .                                                                    
048600     EJECT                                                                
048700 F-LAES-VISA-INFO        SECTION.                                         
048800                                                                          
048900     IF MID-FLGODK-IN               = ALL '+'                             
049000         MOVE MFS-RENSA-FAELT       TO MOD-FLGODK-IN-ATTR                 
049100      ELSE                                                                
049200         MOVE MID-FLGODK-IN         TO MOD-FLGODK-IN                      
049300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLGODK-IN-ATTR                 
049400     END-IF                                                               
049500     PERFORM FA-FLYTTA-RADER                                              
049600     .                                                                    
049700     EJECT                                                                
049800 FA-FLYTTA-RADER            SECTION.                                      
049900                                                                          
050000     MOVE +1                        TO RAD-IX                             
050100     PERFORM UNTIL RAD-IX           >  MAX-RAD                            
050200         IF MID-IDLEVNR-RAD(RAD-IX) =  ALL '+'                            
050300             MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-RAD (RAD-IX)           
050400          ELSE                                                            
050500             MOVE MID-IDLEVNR-RAD(RAD-IX) TO                              
050600                                       MOD-IDLEVNR-RAD (RAD-IX)           
050700             MOVE MFS-ADD-LAES-IN-FAELT TO                                
050800                                 MOD-IDLEVNR-RAD-ATTR  (RAD-IX)           
050900         END-IF                                                           
051000                                                                          
051100         IF MID-KDRT-RAD(RAD-IX)    =  ALL '+'                            
051200             MOVE MFS-RENSA-FAELT   TO MOD-KDRT-RAD    (RAD-IX)           
051300          ELSE                                                            
051400             MOVE MID-KDRT-RAD(RAD-IX) TO                                 
051500                                       MOD-KDRT-RAD    (RAD-IX)           
051600             MOVE MFS-ADD-LAES-IN-FAELT TO                                
051700                                 MOD-KDRT-RAD-ATTR     (RAD-IX)           
051800         END-IF                                                           
051900                                                                          
052000         IF MID-IDFS-RAD(RAD-IX)    =  ALL '+'                            
052100             MOVE MFS-RENSA-FAELT   TO MOD-IDFS-RAD    (RAD-IX)           
052200          ELSE                                                            
052300             MOVE MID-IDFS-RAD(RAD-IX) TO                                 
052400                                       MOD-IDFS-RAD    (RAD-IX)           
052500             MOVE MFS-ADD-LAES-IN-FAELT TO                                
052600                                 MOD-IDFS-RAD-ATTR     (RAD-IX)           
052700         END-IF                                                           
052800                                                                          
052900         IF MID-TIAVIDAT-RAD(RAD-IX) = ALL '+'                            
053000             MOVE MFS-RENSA-FAELT   TO MOD-TIAVIDAT-RAD(RAD-IX)           
053100          ELSE                                                            
053200             MOVE MID-TIAVIDAT-RAD(RAD-IX) TO                             
053300                                       MOD-TIAVIDAT-RAD(RAD-IX)           
053400             MOVE MFS-ADD-LAES-IN-FAELT TO                                
053500                                 MOD-TIAVIDAT-RAD-ATTR (RAD-IX)           
053600         END-IF                                                           
053700                                                                          
053800         IF MID-IDARTNR-RAD(RAD-IX) =  ALL '+'                            
053900             MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-RAD (RAD-IX)           
054000          ELSE                                                            
054100             MOVE MID-IDARTNR-RAD(RAD-IX) TO                              
054200                                       MOD-IDARTNR-RAD (RAD-IX)           
054300             MOVE MFS-ADD-LAES-IN-FAELT TO                                
054400                                 MOD-IDARTNR-RAD-ATTR (RAD-IX)            
054500         END-IF                                                           
054600                                                                          
054700         IF MID-KVAVIS-RAD(RAD-IX)  =  ALL '+'                            
054800             MOVE MFS-RENSA-FAELT   TO MOD-KVAVIS-RAD (RAD-IX)            
054900          ELSE                                                            
055000             MOVE MID-KVAVIS-RAD(RAD-IX) TO                               
055100                                       MOD-KVAVIS-RAD (RAD-IX)            
055200             MOVE MFS-ADD-LAES-IN-FAELT TO                                
055300                                 MOD-KVAVIS-RAD-ATTR  (RAD-IX)            
055400         END-IF                                                           
055500                                                                          
055600         ADD +1                     TO RAD-IX                             
055700     END-PERFORM                                                          
055800     .                                                                    
055900     EJECT                                                                
056000 G-KOLLA-INPUT-UPPDATERA SECTION.                                         
056100                                                                          
056200     MOVE SPACE                TO MED-IDMFSFEL                            
056300     MOVE JA                   TO INDATA-SW                               
056400     PERFORM GA-KOLLA-RADERNA                                             
056500     IF INDATA-FEL                                                        
056600         IF INDATA-EJ-IFYLLT                                              
056700             MOVE INF-UPDATE-NOT-DONE  TO MED-IDMFSFEL                    
056800         ELSE                                                             
056900             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
057000         END-IF                                                           
057100         CALL WMEDKONV USING MED-WMEDAREA                                 
057200         MOVE MED-TEMFSFEL               TO MOD-TEMFSFEL                  
057300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
057400     ELSE                                                                 
057500         PERFORM GB-ANROPA-W611REG                                        
057600         IF WS-FLGODK          = JA OR NEJ OR YES                         
057700             CONTINUE                                                     
057800          ELSE                                                            
057900             PERFORM GC-KOLLA-FELFLAGGOR                                  
058000         END-IF                                                           
058100                                                                          
058200         IF W611REG-OK                                                    
058300             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
058400             CALL WMEDKONV USING MED-WMEDAREA                             
058500             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
058600             PERFORM MFS-FORM-ATTR                                        
058700             PERFORM MFS-RENSA-FAELT-IN                                   
058800          ELSE                                                            
058900             IF MED-IDMFSINF =  INF-ADVICE-N-EXIST                        
059000                 CALL WMEDKONV USING MED-WMEDAREA                         
059100                 MOVE MED-TEMFSINF     TO MOD-TEMFSINF                    
059200               ELSE                                                       
059300                 IF MED-IDMFSFEL       = SPACE                            
059400                     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL            
059500                 ELSE                                                     
059600                     IF MED-IDMFSFEL = INF-PART-SUPERSEDED  OR            
059700                                       INF-AVROP-SAKNAS                   
059800                        MOVE MFS-OEPPNA-ALFA-FAELT TO                     
059900                                      MOD-FLGODK-IN-ATTR                  
060000                        PERFORM MFS-STAENG-RAD-FAELT                      
060100                     END-IF                                               
060200                 END-IF                                                   
060300             END-IF                                                       
060400             CALL WMEDKONV USING MED-WMEDAREA                             
060500             MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                        
060600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
060700         END-IF                                                           
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 GA-KOLLA-RADERNA    SECTION.                                             
061200                                                                          
061300     MOVE +1 TO RAD-IX                                                    
061400     PERFORM UNTIL RAD-IX            > MAX-RAD                            
061500         IF MID-IDLEVNR-RAD (RAD-IX) = ALL '+' AND                        
061600            MID-KDRT-RAD    (RAD-IX) = ALL '+' AND                        
061700            MID-IDFS-RAD    (RAD-IX) = ALL '+' AND                        
061800            MID-TIAVIDAT-RAD(RAD-IX) = ALL '+' AND                        
061900            MID-IDARTNR-RAD (RAD-IX) = ALL '+' AND                        
062000            MID-KVAVIS-RAD  (RAD-IX) = ALL '+'                            
062100             CONTINUE                                                     
062200          ELSE                                                            
062300             MOVE JA           TO INDATA-IFYLLT-SW                        
062400             PERFORM GAA-KOLLA-IDLEVNR-RAD                                
062500             PERFORM GAB-KOLLA-KDRT-RAD                                   
062600             PERFORM GAC-KOLLA-IDFS-RAD                                   
062700             PERFORM GAD-KOLLA-TIAVIDAT-RAD                               
062800             PERFORM GAE-KOLLA-IDARTNR-RAD                                
062900             PERFORM GAF-KOLLA-KVAVIS-RAD                                 
063000         END-IF                                                           
063100         ADD +1                TO RAD-IX                                  
063200     END-PERFORM                                                          
063300                                                                          
063400     IF INDATA-EJ-IFYLLT                                                  
063500         MOVE NEJ              TO INDATA-SW                               
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 GAA-KOLLA-IDLEVNR-RAD   SECTION.                                         
064000                                                                          
064100     IF MID-IDLEVNR-RAD (RAD-IX)  NOT = SPACE                             
064200         MOVE MFS-ALFA-FAELT-RAETT TO                                     
064300                               MOD-IDLEVNR-RAD-ATTR (RAD-IX)              
064400      ELSE                                                                
064500         MOVE MFS-ALFA-FAELT-FEL TO                                       
064600                               MOD-IDLEVNR-RAD-ATTR (RAD-IX)              
064700         MOVE NEJ               TO INDATA-SW                              
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 GAB-KOLLA-KDRT-RAD        SECTION.                                       
065200                                                                          
065300     IF (MID-KDRT-RAD (RAD-IX)     NUMERIC     AND                        
065400         MID-KDRT-RAD (RAD-IX)     NOT = '99') OR                         
065500         MID-KDRT-RAD (RAD-IX)     = ALL '+'                              
065600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDRT-RAD-ATTR (RAD-IX)           
065700      ELSE                                                                
065800         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDRT-RAD-ATTR (RAD-IX)           
065900         MOVE NEJ                 TO INDATA-SW                            
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 GAC-KOLLA-IDFS-RAD        SECTION.                                       
066400                                                                          
066500     IF MID-IDFS-RAD(RAD-IX)       NOT = SPACE AND                        
066600                                   NOT = ALL '+'                          
066700       MOVE 1   TO INDX                                                   
066800       MOVE NEJ TO FL-IDFS-OK                                             
066900       MOVE MID-IDFS-RAD(RAD-IX) TO WS-IDFS                               
067000       PERFORM UNTIL INDX > 8 OR FL-IDFS-OK = JA                          
067100         IF WS-IDFS(INDX:1) NUMERIC                                       
067200           IF WS-IDFS(INDX:1) > ZERO                                      
067300             MOVE JA TO FL-IDFS-OK                                        
067400           END-IF                                                         
067500         END-IF                                                           
067600         ADD 1 TO INDX                                                    
067700       END-PERFORM                                                        
067800                                                                          
067900       IF FL-IDFS-OK = JA                                                 
068000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFS-RAD-ATTR (RAD-IX)          
068100       ELSE                                                               
068200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDFS-RAD-ATTR (RAD-IX)          
068300         MOVE NEJ                  TO INDATA-SW                           
068400       END-IF                                                             
068500      ELSE                                                                
068600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDFS-RAD-ATTR (RAD-IX)          
068700         MOVE NEJ                  TO INDATA-SW                           
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 GAD-KOLLA-TIAVIDAT-RAD    SECTION.                                       
069200                                                                          
069300     IF MID-TIAVIDAT-RAD(RAD-IX)  NUMERIC                                 
069400         MOVE MFS-NUM-FAELT-RAETT TO                                      
069500                                  MOD-TIAVIDAT-RAD-ATTR (RAD-IX)          
069600      ELSE                                                                
069700         MOVE MFS-NUM-FAELT-FEL   TO                                      
069800                                  MOD-TIAVIDAT-RAD-ATTR (RAD-IX)          
069900         MOVE NEJ                 TO INDATA-SW                            
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300 GAE-KOLLA-IDARTNR-RAD     SECTION.                                       
070400                                                                          
070500     IF MID-IDARTNR-RAD(RAD-IX)   NUMERIC                                 
070600         MOVE MFS-NUM-FAELT-RAETT TO                                      
070700                                  MOD-IDARTNR-RAD-ATTR (RAD-IX)           
070800      ELSE                                                                
070900         MOVE MFS-NUM-FAELT-FEL   TO                                      
071000                                  MOD-IDARTNR-RAD-ATTR (RAD-IX)           
071100         MOVE NEJ                 TO INDATA-SW                            
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 GAF-KOLLA-KVAVIS-RAD      SECTION.                                       
071600                                                                          
071700     IF MID-KVAVIS-RAD(RAD-IX)    NUMERIC                                 
071800         MOVE MFS-NUM-FAELT-RAETT TO                                      
071900                                  MOD-KVAVIS-RAD-ATTR (RAD-IX)            
072000      ELSE                                                                
072100         MOVE MFS-NUM-FAELT-FEL   TO                                      
072200                                  MOD-KVAVIS-RAD-ATTR (RAD-IX)            
072300         MOVE NEJ                 TO INDATA-SW                            
072400     END-IF                                                               
072500     .                                                                    
072600     EJECT                                                                
072700 GB-ANROPA-W611REG  SECTION.                                              
072800                                                                          
072900     PERFORM GBA-SKAPA-LAENKAREA                                          
073000                                                                          
073100     CALL W611REG USING LAENK-W611REG0                                    
073200                        REG-INLA1-PCB  REG-INLA2-PCB                      
073300                        REG-INLA3-PCB  REG-LEVA-PCB                       
073400                                       REG-ARTC-PCB                       
073500                        REG-BENA-PCB   REG-WDD9-PCB                       
073510                        REG-WDK7-PCB   REG-WDB6-PCB                       
073600     .                                                                    
073700     EJECT                                                                
073800 GBA-SKAPA-LAENKAREA  SECTION.                                            
073900                                                                          
074000     MOVE '6113'               TO REG3-IDTRANS                            
074100     MOVE WS-IDLBBET           TO REG3-IDLBBET                            
074200                                                                          
074300     MOVE +1                   TO RAD-IX                                  
074400     PERFORM UNTIL RAD-IX               >  MAX-RAD                        
074500         IF MID-IDLEVNR-RAD(RAD-IX)     =  ALL '+'                        
074600             CONTINUE                                                     
074700          ELSE                                                            
074800             MOVE MID-IDLEVNR-RAD(RAD-IX)  TO                             
074900                                           REG3-IDLEVNR (RAD-IX)          
075000             IF MID-KDRT-RAD (RAD-IX)      NUMERIC                        
075100                 MOVE MID-KDRT-RAD(RAD-IX) TO REG3-KDRT (RAD-IX)          
075200             END-IF                                                       
075300             MOVE MID-IDFS-RAD(RAD-IX)     TO REG3-IDFS (RAD-IX)          
075400             MOVE MID-TIAVIDAT-RAD(RAD-IX) TO                             
075500                                           REG3-TIAVIDAT(RAD-IX)          
075600             MOVE MID-IDARTNR-RAD(RAD-IX)  TO                             
075700                                           REG3-IDARTNR (RAD-IX)          
075800             MOVE MID-KVAVIS-RAD(RAD-IX)   TO                             
075900                                           REG3-KVAVIS  (RAD-IX)          
076000         END-IF                                                           
076100         ADD +1                         TO RAD-IX                         
076200     END-PERFORM                                                          
076300     .                                                                    
076400     EJECT                                                                
076500 GC-KOLLA-FELFLAGGOR  SECTION.                                            
076600                                                                          
076700     MOVE +1                              TO RAD-IX                       
076800     PERFORM UNTIL RAD-IX                 >  MAX-RAD                      
076900         IF REG3-IDLEVNR-OK (RAD-IX)      = JA                            
077000             MOVE MFS-ALFA-FAELT-RAETT    TO                              
077100                         MOD-IDLEVNR-RAD-ATTR(RAD-IX)                     
077200          ELSE                                                            
077300             MOVE MFS-ALFA-FAELT-FEL      TO                              
077400                         MOD-IDLEVNR-RAD-ATTR(RAD-IX)                     
077500             MOVE NEJ                     TO W611REG-SW                   
077600         END-IF                                                           
077700                                                                          
077800         IF REG3-KDRT-OK (RAD-IX)         = JA                            
077900             MOVE MFS-NUM-FAELT-RAETT     TO                              
078000                         MOD-KDRT-RAD-ATTR(RAD-IX)                        
078100          ELSE                                                            
078200             MOVE MFS-NUM-FAELT-FEL       TO                              
078300                         MOD-KDRT-RAD-ATTR(RAD-IX)                        
078400             MOVE NEJ                     TO W611REG-SW                   
078500                                             REG3-FLGODK-IDARTNR          
078600                                             REG3-FLGODK-IDFS             
078700         END-IF                                                           
078800                                                                          
078900         IF REG3-IDFS-OK (RAD-IX)         = JA                            
079000             MOVE MFS-ALFA-FAELT-RAETT    TO                              
079100                         MOD-IDFS-RAD-ATTR(RAD-IX)                        
079200          ELSE                                                            
079300             MOVE MFS-ALFA-FAELT-FEL       TO                             
079400                         MOD-IDFS-RAD-ATTR(RAD-IX)                        
079500             MOVE NEJ                     TO W611REG-SW                   
079600         END-IF                                                           
079700                                                                          
079800         IF REG3-TIAVIDAT-OK (RAD-IX)     = JA                            
079900             MOVE MFS-NUM-FAELT-RAETT     TO                              
080000                         MOD-TIAVIDAT-RAD-ATTR (RAD-IX)                   
080100          ELSE                                                            
080200             MOVE MFS-NUM-FAELT-FEL       TO                              
080300                         MOD-TIAVIDAT-RAD-ATTR (RAD-IX)                   
080400             MOVE NEJ                     TO W611REG-SW                   
080500                                             REG3-FLGODK-IDARTNR          
080600                                             REG3-FLGODK-IDFS             
080700         END-IF                                                           
080800                                                                          
080900         IF REG3-IDARTNR-OK (RAD-IX)      = JA                            
081000             MOVE MFS-NUM-FAELT-RAETT     TO                              
081100                         MOD-IDARTNR-RAD-ATTR (RAD-IX)                    
081200          ELSE                                                            
081300             MOVE MFS-NUM-FAELT-FEL       TO                              
081400                         MOD-IDARTNR-RAD-ATTR (RAD-IX)                    
081500             MOVE NEJ                     TO W611REG-SW                   
081600         END-IF                                                           
081700                                                                          
081800         IF MED-IDMFSFEL = SPACE OR INF-ADVICE-N-EXIST                    
081900           IF REG3-IDMFSFEL (RAD-IX) NOT = SPACE                          
082000             MOVE REG3-IDMFSFEL (RAD-IX)   TO MED-IDMFSFEL                
082100           END-IF                                                         
082200         END-IF                                                           
082300                                                                          
082400         ADD +1                           TO RAD-IX                       
082500     END-PERFORM                                                          
082600                                                                          
082700     IF MED-IDMFSFEL = SPACE OR INF-ADVICE-N-EXIST                        
082800       IF REG3-FLGODK-IDARTNR             =  JA OR                        
082900          REG3-FLGODK-IDFS                =  JA                           
083000           MOVE MFS-OEPPNA-ALFA-FAELT     TO MOD-FLGODK-IN-ATTR           
083100           MOVE   INF-ADVICE-N-EXIST      TO MED-IDMFSINF                 
083200           PERFORM MFS-STAENG-RAD-FAELT                                   
083300           IF REG3-FLGODK-IDFS            = JA                            
083400               MOVE MFS-RENSA-FAELT       TO MOD-IDLBBET-UT               
083500           END-IF                                                         
083600       END-IF                                                             
083700     ELSE                                                                 
083800       MOVE +1       TO RAD-IX                                            
083900       PERFORM UNTIL RAD-IX > MAX-RAD                                     
084000         IF REG3-IDMFSFEL(RAD-IX) = INF-ADVICE-N-EXIST                    
084100             MOVE MFS-NUM-FAELT-RAETT     TO                              
084200                         MOD-IDARTNR-RAD-ATTR (RAD-IX)                    
084300                         MOD-IDFS-RAD-ATTR (RAD-IX)                       
084400         END-IF                                                           
084500         ADD +1      TO RAD-IX                                            
084600       END-PERFORM                                                        
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 MFS-RENSA-FAELT-IN SECTION.                                              
085100                                                                          
085200     MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-IN                          
085300                                  MOD-FLGODK-IN                           
085400                                                                          
085500     PERFORM MFS-RENSA-RADER-FAELT-IN                                     
085600                                                                          
085700     .                                                                    
085800     SKIP3                                                                
085900 MFS-RENSA-RADER-FAELT-IN SECTION.                                        
086000                                                                          
086100     MOVE +1                   TO RAD-IX                                  
086200     PERFORM UNTIL RAD-IX      >  MAX-RAD                                 
086300         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-RAD (RAD-IX)                
086400                                  MOD-KDRT-RAD    (RAD-IX)                
086500                                  MOD-IDFS-RAD    (RAD-IX)                
086600                                  MOD-TIAVIDAT-RAD(RAD-IX)                
086700                                  MOD-IDARTNR-RAD (RAD-IX)                
086800                                  MOD-KVAVIS-RAD  (RAD-IX)                
086900         ADD +1                TO RAD-IX                                  
087000     END-PERFORM                                                          
087100     .                                                                    
087200     EJECT                                                                
087300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
087400                                                                          
087500     MOVE +1                     TO RAD-IX                                
087600     PERFORM UNTIL RAD-IX        >  MAX-RAD                               
087700         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-RAD (RAD-IX)              
087800                                    MOD-KDRT-RAD    (RAD-IX)              
087900                                    MOD-IDFS-RAD    (RAD-IX)              
088000                                    MOD-TIAVIDAT-RAD(RAD-IX)              
088100                                    MOD-IDARTNR-RAD (RAD-IX)              
088200                                    MOD-KVAVIS-RAD  (RAD-IX)              
088300         ADD +1                  TO RAD-IX                                
088400     END-PERFORM                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 MFS-FORM-ATTR SECTION.                                                   
088800                                                                          
088900*    --- ALLA INDATA-FÄLT                                                 
089000     MOVE MFS-FORMATETS-ATTR   TO MOD-IDLBBET-IN                          
089100                                  MOD-FLGODK-IN                           
089200                                                                          
089300     PERFORM MFS-FORM-ATTR-RADER                                          
089400     .                                                                    
089500     SKIP2                                                                
089600 MFS-FORM-ATTR-RADER   SECTION.                                           
089700                                                                          
089800     MOVE +1                     TO RAD-IX                                
089900     PERFORM UNTIL RAD-IX        >  MAX-RAD                               
090000         MOVE MFS-FORMATETS-ATTR TO MOD-IDLEVNR-RAD-ATTR (RAD-IX)         
090100                                    MOD-KDRT-RAD-ATTR    (RAD-IX)         
090200                                    MOD-IDFS-RAD-ATTR    (RAD-IX)         
090300                                    MOD-IDARTNR-RAD-ATTR (RAD-IX)         
090400                                    MOD-KVAVIS-RAD-ATTR  (RAD-IX)         
090500         ADD +1                  TO RAD-IX                                
090600     END-PERFORM                                                          
090700     .                                                                    
090800     EJECT                                                                
090900 MFS-STAENG-RAD-FAELT  SECTION.                                           
091000                                                                          
091100     MOVE +1                   TO RAD-IX                                  
091200     PERFORM UNTIL RAD-IX      >  MAX-RAD                                 
091300         IF REG3-IDARTNR-OK (RAD-IX) = JA                                 
091400             MOVE MFS-STAENG-FAELT    TO                                  
091500                                  MOD-IDARTNR-RAD-ATTR (RAD-IX)           
091600          ELSE                                                            
091700             MOVE MFS-STAENG-FAELT-HI TO                                  
091800                                  MOD-IDARTNR-RAD-ATTR (RAD-IX)           
091900         END-IF                                                           
092000         IF REG3-IDFS-OK (RAD-IX)      = JA                               
092100             MOVE MFS-STAENG-FAELT     TO                                 
092200                                  MOD-IDFS-RAD-ATTR    (RAD-IX)           
092300          ELSE                                                            
092400             MOVE MFS-STAENG-FAELT-HI TO                                  
092500                                  MOD-IDFS-RAD-ATTR    (RAD-IX)           
092600         END-IF                                                           
092700         MOVE MFS-STAENG-FAELT TO MOD-IDLEVNR-RAD-ATTR (RAD-IX)           
092800                                  MOD-KDRT-RAD-ATTR    (RAD-IX)           
092900                                  MOD-KVAVIS-RAD-ATTR  (RAD-IX)           
093000                                  MOD-TIAVIDAT-RAD-ATTR(RAD-IX)           
093100         ADD +1                TO RAD-IX                                  
093200     END-PERFORM                                                          
093300     .                                                                    
093400     EJECT                                                                
093500* --- IMS SEKTIONER ---                                                   
093600 IMS-GET-MSG SECTION.                                                     
093700                                                                          
093800     MOVE '  QC' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     .                                                                    
094300     SKIP3                                                                
094400 IMS-INSERT-MSG SECTION.                                                  
094500                                                                          
094600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
094700       MOVE '0' TO MFS-KDHUVOMR                                           
094800     END-IF                                                               
094900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095000     MOVE SPACE TO GODK-STATUSKODER                                       
095100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     SKIP2                                                                
095600 IMS-GU-WDB601-LEV SECTION.                                               
095700     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
095800          DELIMITED BY SIZE INTO SSA1                                     
095900     MOVE '  GE' TO GODK-STATUSKODER                                      
096000     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
096100     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400     EJECT                                                                
096500 IMS-GU-WDB601    SECTION.                                                
096600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
096700          DELIMITED BY SIZE INTO SSA1                                     
096800     MOVE '  GE' TO GODK-STATUSKODER                                      
096900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
097000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     IF SEGMENT-SAKNAS                                                    
097300         MOVE SPACE TO DCS-KDDC                                           
097400     END-IF                                                               
097500     .                                                                    
097600 IMS-STATUSKONTROLL SECTION.                                              
097700                                                                          
097800     SET STATUS-IX TO 1                                                   
097900     SEARCH GODK-STATUS                                                   
098000       AT END                                                             
098100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
098200         DELIMITED BY SIZE INTO FELTEXT                                   
098300         CALL FELLOG                                                      
098400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
098500         CONTINUE                                                         
098600     END-SEARCH                                                           
098700     .                                                                    
