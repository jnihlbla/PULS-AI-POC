000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011200.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/02/24.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR R31 TRANSAKTIONER                                         
001100*        EN LEVERANTÖR FLERA AVIER OCH FLERA ARTIKLAR                     
001200*                                                                         
001300*    SUB PROGRAMMET W611REG UPPDATERAR W6INLA (W6D1)                      
001400*                           LÄSER      WLLEVA (WDF1)                      
001500*                           LÄSER      WLARTC (WDK6)                      
001600*                           LÄSER      WLBENA (WDD3)                      
001700*                           LÄSER      WLINLB (WDD9)                      
001800*                           LÄSER      WDB6                               
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T112                                              
002200*        MID:         W6I11201                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O11201                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6011200'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +877  COMP SYNC.        
004500 77  RAD-IX1                     PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77  RAD-IX2                     PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAX-RAD                     PIC S9(4)  VALUE +24   COMP SYNC.        
004800 77  MAX-REG2                    PIC S9(4)  VALUE +24   COMP SYNC.        
004900 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
005000                                                                          
005100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200                                                                          
005300 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005400 77  WS-KDRT                     PIC X(2)    VALUE SPACE.                 
005500 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
005600 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
005700 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
005800 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
005900 77  WS-FLGODK                   PIC X(1)    VALUE SPACE.                 
006000 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  INDATA-IFYLLT-SW            PIC X       VALUE 'N'.                   
006700     88  INDATA-IFYLLT                       VALUE 'J'.                   
006800     88  INDATA-EJ-IFYLLT                    VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007300                                                                          
007400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007500     88  ALLT-OK                             VALUE 'J'.                   
007600                                                                          
007700 77  W611REG-SW                  PIC X       VALUE 'J'.                   
007800     88  W611REG-OK                          VALUE 'J'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '6112'.                
008200     88  GODK-MID                            VALUE '6111' '6112'          
008300                                                   '6113' '6114'          
008400                                                   '6115' '6116'          
008500                                                   '6118' '6119'.         
008600     88  HELP-MID                            VALUE '0551'.                
008700     EJECT                                                                
008800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008900 01  GENERELLA-SUBPROGRAM.                                                
009000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  W611REG                 PIC X(8)    VALUE 'W611REG'.             
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009500     EJECT                                                                
009600*01 -COPY WMSGINIT                                                        
009700     SKIP3                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010400     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010700     03  ERR-IN-LINE-ONE         PIC X(3)    VALUE '184'.                 
010800     03  INF-ADVICE-N-EXIST      PIC X(3)    VALUE '201'.                 
010900     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.                 
011200                                                                          
011300                                                                          
011400     EJECT                                                                
011500*01  -COPY W611REG0                                                       
011600*01  FILLER -COPY W611REG2    -RED LAENK-W611REG0                         
011700     EJECT                                                                
011800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012100     SKIP3                                                                
012200*01  MID -COPY W6I11201                                                   
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012500     SKIP3                                                                
012600*01  -COPY WMSGAREA                                                       
012700     EJECT                                                                
012800     03  MOD REDEFINES MSG-AREA.                                          
012900*      05  -COPY W6O11201                                                 
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013200     SKIP3                                                                
013300*01  -COPY WMFSAREA                                                       
013400     EJECT                                                                
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  KEYS-TO-DLI.                                                         
013800     03  W-IDDC-B6-X.                                                     
013900         05 W-IDDC-B6                  PIC X(2).                          
014000     03  W-IDDC-B6-LEV-X.                                                 
014100         05 W-IDDC-B6-LEV              PIC X(5).                          
014200                                                                          
014300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014400 01   DLI-IO-AREA-B601.                                                   
014500*     03  -COPY WDB601                                                    
014600                                                                          
014700 01  FILLER               PIC X(16)   VALUE 'WDB601 LEV'.                 
014800 01   DLI-IO-AREA-B601-LEV.                                               
014900*     03  -COPY WDB601   -PRE LEV-                                        
015000                                                                          
015100 01  SSA1                        PIC X(256).                              
015200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88 SEGMENT-FINNS                        VALUE '  '.                  
015600     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
015700     88 BASEN-SLUT                           VALUE 'GB'.                  
015800                                                                          
015900 01  GODK-STATUSKODER.                                                    
016000     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
016100                                                                          
016200     EJECT                                                                
016300*    --- IMS FUNKTIONSKODER                                               
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800 01 -COPY W0009     -PRE MSG-                                             
016900                                                                          
017000 01  USEA-PCB                    PIC X.                                   
017100                                                                          
017200*01  -COPY W0008   -PRE WDB6-LEV-                                         
017300     05  FILLER                  PIC X.                                   
017400                                                                          
017500*01  -COPY W0008   -PRE WDB6-                                             
017600     05  FILLER                  PIC X.                                   
017700                                                                          
017800*   PCB'ER FÖR SUB PGM W611REG                                            
017900                                                                          
018000 01  REG-INLA1-PCB               PIC X.                                   
018100                                                                          
018200 01  REG-INLA2-PCB               PIC X.                                   
018300                                                                          
018400 01  REG-INLA3-PCB               PIC X.                                   
018500                                                                          
018600 01  REG-LEVA-PCB                PIC X.                                   
018700                                                                          
018800 01  REG-ARTC-PCB                PIC X.                                   
018900                                                                          
019000 01  REG-BENA-PCB                PIC X.                                   
019100                                                                          
019200 01  REG-WDD9-PCB                PIC X.                                   
019300                                                                          
019310 01  REG-WDK7-PCB                PIC X.                                   
019320                                                                          
019330 01  REG-WDB6-PCB                PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
019600                           WDB6-LEV-PCB   WDB6-PCB                        
019700                           REG-INLA1-PCB  REG-INLA2-PCB                   
019800                           REG-INLA3-PCB  REG-LEVA-PCB                    
019900                                          REG-ARTC-PCB                    
020000                           REG-BENA-PCB   REG-WDD9-PCB                    
020010                           REG-WDK7-PCB   REG-WDB6-PCB.                   
020100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
020200                           WDB6-LEV-PCB   WDB6-PCB                        
020300                           REG-INLA1-PCB  REG-INLA2-PCB                   
020400                           REG-INLA3-PCB  REG-LEVA-PCB                    
020500                                          REG-ARTC-PCB                    
020600                           REG-BENA-PCB   REG-WDD9-PCB                    
020610                           REG-WDK7-PCB   REG-WDB6-PCB.                   
020700                                                                          
020800     PERFORM IMS-GET-MSG                                                  
020900     IF SEGMENT-FINNS                                                     
021000       PERFORM A-INIT                                                     
021100       PERFORM B-KOLLA-NYCKLAR                                            
021200       IF NYCKLAR-OK                                                      
021300         IF MFS-ENTER AND EGEN-MID                                        
021400           PERFORM G-KOLLA-INPUT-UPPDATERA                                
021500          ELSE                                                            
021600           IF HELP-MID                                                    
021700               PERFORM F-LAES-VISA-INFO                                   
021800            ELSE                                                          
021900               PERFORM MFS-RENSA-FAELT-IN                                 
022000           END-IF                                                         
022100         END-IF                                                           
022200       END-IF                                                             
022300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
022400       PERFORM IMS-INSERT-MSG                                             
022500     END-IF                                                               
022600                                                                          
022700     MOVE ZERO TO RETURN-CODE                                             
022800     GOBACK                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 A-INIT SECTION.                                                          
023200                                                                          
023300     IF MSG-DUBBLA-TRANSKODER                                             
023400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I11201                 
023500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023700     ELSE                                                                 
023800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11201                  
023900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024100     END-IF                                                               
024200                                                                          
024300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
024500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024600                                                                          
024700     MOVE LOW-VALUE TO MSG-AREA                                           
024800     MOVE 'W6O112N1' TO MFS-IDMOD                                         
024900     MOVE '6112' TO MOD-IDTRANS                                           
025000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025100                                                                          
025200     IF EGEN-MID OR HELP-MID                                              
025300       CONTINUE                                                           
025400     ELSE                                                                 
025500       MOVE SPACE TO MFS-KDTRTYP                                          
025600       MOVE '7' TO MFS-IDPFK                                              
025700     END-IF                                                               
025800                                                                          
025900     MOVE SPACE                TO REG2-IDTRANS                            
026000                                  REG2-IDDC                               
026100                                  REG2-IDLBBET                            
026200                                  REG2-IDLEVNR                            
026300     MOVE NEJ                  TO REG2-FLGODK-IDFS                        
026400                                  REG2-FLGODK-IDARTNR                     
026500     MOVE JA                   TO REG2-IDLEVNR-OK                         
026600                                  REG2-KDRT-OK                            
026700                                  REG2-TIAVIDAT-OK                        
026800     MOVE ZERO                 TO REG2-KDRT                               
026900                                  REG2-TIAVIDAT                           
027000                                                                          
027100     MOVE +1                   TO RAD-IX1                                 
027200     PERFORM UNTIL RAD-IX1     >  MAX-REG2                                
027300         MOVE JA               TO REG2-IDARTNR-OK (RAD-IX1)               
027400                                  REG2-IDFS-OK    (RAD-IX1)               
027500         MOVE ZERO             TO REG2-IDARTNR    (RAD-IX1)               
027600                                  REG2-KVAVIS     (RAD-IX1)               
027700         MOVE SPACE            TO REG2-IDFS       (RAD-IX1)               
027800                                  REG2-IDMFSFEL   (RAD-IX1)               
027900         ADD +1                TO RAD-IX1                                 
028000     END-PERFORM                                                          
028100                                                                          
028200     MOVE JA                   TO W611REG-SW                              
028300     MOVE NEJ                  TO INDATA-IFYLLT-SW                        
028400     PERFORM AA-INIT-NYCKLAR                                              
028500                                                                          
028600     IF MSGI-IDLAND-SPR = 'GB'                                            
028700       MOVE +2 TO SPRAK-IX                                                
028800       MOVE 'GB ' TO MED-IDSKYLT                                          
028900     ELSE                                                                 
029000       MOVE +1 TO SPRAK-IX                                                
029100       MOVE 'S  ' TO MED-IDSKYLT                                          
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500*----------------------------------------------------------------*        
029600 AA-INIT-NYCKLAR SECTION.                                                 
029700                                                                          
029800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
029900     MOVE '001'                  TO MSGI-KDCALL                           
030000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030100     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030200     MOVE '6112'                 TO MSGI-IDTRANS                          
030300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030400     .                                                                    
030500     EJECT                                                                
030600 B-KOLLA-NYCKLAR SECTION.                                                 
030700                                                                          
030800     MOVE JA TO NYCKLAR-SW                                                
030900                                                                          
031000     PERFORM BA-KOLLA-IDLEVNR                                             
031100     PERFORM BB-KOLLA-KDRT                                                
031200     PERFORM BC-KOLLA-IDFS                                                
031300     PERFORM BD-KOLLA-TIAVIDAT                                            
031400     PERFORM BE-KOLLA-IDLBBET                                             
031500     PERFORM BF-KOLLA-FLGODK                                              
031600     PERFORM BG-KOLLA-IDDC                                                
031700     PERFORM BI-KOLLA-SDC-NDC-LEV                                         
031800     MOVE MFS-RENSA-FAELT          TO MOD-ADINLOMR-PRT-IN                 
031900     IF MID-ADINLOMR-PRT-IN        = ALL '+'                              
032000         MOVE MID-ADINLOMR-PRT-UT  TO WS-ADINLOMR-PRT                     
032100     ELSE                                                                 
032200         MOVE MID-ADINLOMR-PRT-IN  TO WS-ADINLOMR-PRT                     
032300         MOVE SPACE            TO MFS-KDTRTYP                             
032400     END-IF                                                               
032500     PERFORM BH-FLYTTA-OEVRIGA-NYCKLAR                                    
032600     IF GODK-MID OR NYCKLAR-OK                                            
032700         MOVE WS-IDLEVNR       TO MOD-IDLEVNR-UT                          
032800         MOVE WS-KDRT          TO MOD-KDRT-UT                             
032900         INSPECT MOD-KDRT-UT REPLACING LEADING ZERO BY SPACE              
033000         IF MOD-KDRT-UT        =  SPACE                                   
033100             MOVE ' 0'         TO MOD-KDRT-UT                             
033200         END-IF                                                           
033300         MOVE WS-IDFS          TO MOD-IDFS-UT                             
033400         MOVE WS-TIAVIDAT      TO MOD-TIAVIDAT-UT                         
033500         INSPECT MOD-TIAVIDAT-UT REPLACING LEADING ZERO BY SPACE          
033600         MOVE WS-ADINLOMR-PRT  TO MOD-ADINLOMR-PRT-UT                     
033700         MOVE WS-IDLBBET       TO MOD-IDLBBET-UT                          
033800         MOVE WS-FLGODK        TO MOD-FLGODK-UT                           
033900         MOVE DCS-IDDC         TO MOD-IDDC-UT                             
034000      ELSE                                                                
034100         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-UT                          
034200                                  MOD-KDRT-UT                             
034300                                  MOD-IDFS-UT                             
034400                                  MOD-TIAVIDAT-UT                         
034500                                  MOD-ADINLOMR-PRT-UT                     
034600                                  MOD-IDLBBET-UT                          
034700                                  MOD-FLGODK-UT                           
034800                                  MOD-IDDC-UT                             
034900*        + ÖVRIGA SPAR-NYCKLAR                                            
035000                                  MOD-FLKLIVIS-UT                         
035100                                  MOD-IDLOPNRM-UT                         
035200     END-IF                                                               
035300                                                                          
035400     IF NYCKLAR-FEL                                                       
035500       MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                            
035600       CALL WMEDKONV USING     MED-WMEDAREA                               
035700       MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                            
035800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
035900       PERFORM MFS-LAES-IN-IGEN                                           
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 BA-KOLLA-IDLEVNR SECTION.                                                
036400                                                                          
036500     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                          
036600                                                                          
036700     IF MID-IDLEVNR-IN         = ALL '+'                                  
036800         MOVE MID-IDLEVNR-UT   TO WS-IDLEVNR                              
036900     ELSE                                                                 
037000         MOVE MID-IDLEVNR-IN   TO WS-IDLEVNR                              
037100         MOVE SPACE            TO MFS-KDTRTYP                             
037200     END-IF                                                               
037300                                                                          
037400     IF WS-IDLEVNR NOT = SPACE                                            
037500         CONTINUE                                                         
037600     ELSE                                                                 
037700         MOVE NEJ              TO NYCKLAR-SW                              
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 BB-KOLLA-KDRT    SECTION.                                                
038200                                                                          
038300     MOVE MFS-RENSA-FAELT        TO MOD-KDRT-IN                           
038400                                                                          
038500     IF GODK-MID OR HELP-MID                                              
038600         IF MID-KDRT-IN          = ALL '+'                                
038700             MOVE MID-KDRT-UT    TO WS-KDRT                               
038800             INSPECT WS-KDRT REPLACING LEADING SPACE BY ZERO              
038900         ELSE                                                             
039000             MOVE MID-KDRT-IN    TO WS-KDRT                               
039100             MOVE SPACE          TO MFS-KDTRTYP                           
039200         END-IF                                                           
039300     ELSE                                                                 
039400         MOVE ZERO             TO WS-KDRT                                 
039500     END-IF                                                               
039600                                                                          
039700     IF WS-KDRT  NUMERIC AND WS-KDRT NOT = 99                             
039800         CONTINUE                                                         
039900     ELSE                                                                 
040000         MOVE NEJ              TO NYCKLAR-SW                              
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 BC-KOLLA-IDFS    SECTION.                                                
040500                                                                          
040600     MOVE MFS-RENSA-FAELT      TO MOD-IDFS-IN                             
040700                                                                          
040800     IF MID-IDFS-IN            = ALL '+'                                  
040900         MOVE MID-IDFS-UT      TO WS-IDFS                                 
041000     ELSE                                                                 
041100         MOVE MID-IDFS-IN      TO WS-IDFS                                 
041200         MOVE SPACE            TO MFS-KDTRTYP                             
041300     END-IF                                                               
041400                                                                          
041500     .                                                                    
041600     EJECT                                                                
041700 BD-KOLLA-TIAVIDAT SECTION.                                               
041800                                                                          
041900     MOVE MFS-RENSA-FAELT      TO MOD-TIAVIDAT-IN                         
042000                                                                          
042100     IF MID-TIAVIDAT-IN        = ALL '+'                                  
042200         MOVE MID-TIAVIDAT-UT  TO WS-TIAVIDAT                             
042300         INSPECT WS-TIAVIDAT REPLACING LEADING SPACE BY ZERO              
042400     ELSE                                                                 
042500         MOVE MID-TIAVIDAT-IN  TO WS-TIAVIDAT                             
042600         MOVE SPACE            TO MFS-KDTRTYP                             
042700     END-IF                                                               
042800                                                                          
042900     IF WS-TIAVIDAT NUMERIC AND WS-TIAVIDAT > ZERO                        
043000         CONTINUE                                                         
043100     ELSE                                                                 
043200       MOVE NEJ                TO NYCKLAR-SW                              
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600 BE-KOLLA-IDLBBET  SECTION.                                               
043700                                                                          
043800     MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-IN                          
043900                                                                          
044000     IF EGEN-MID OR HELP-MID                                              
044100         IF MID-IDLBBET-IN     = ALL '+'                                  
044200             MOVE MID-IDLBBET-UT TO WS-IDLBBET                            
044300         ELSE                                                             
044400             MOVE MID-IDLBBET-IN TO WS-IDLBBET                            
044500             MOVE SPACE        TO MFS-KDTRTYP                             
044600         END-IF                                                           
044700      ELSE                                                                
044800         MOVE SPACE            TO WS-IDLBBET                              
044900     END-IF                                                               
045000                                                                          
045100     .                                                                    
045200     EJECT                                                                
045300 BF-KOLLA-FLGODK  SECTION.                                                
045400                                                                          
045500     MOVE MFS-RENSA-FAELT      TO MOD-FLGODK-IN                           
045600                                                                          
045700     IF EGEN-MID                                                          
045800         IF MID-FLGODK-IN      = ALL '+'                                  
045900             MOVE SPACE        TO WS-FLGODK                               
046000         ELSE                                                             
046100             MOVE MID-FLGODK-IN TO WS-FLGODK                              
046200             MOVE SPACE        TO MFS-KDTRTYP                             
046300         END-IF                                                           
046400      ELSE                                                                
046500         MOVE SPACE            TO WS-FLGODK                               
046600     END-IF                                                               
046700                                                                          
046800     IF WS-FLGODK              =  SPACE OR JA OR NEJ OR YES               
046900         MOVE WS-FLGODK        TO REG2-FLGODK                             
047000         IF REG2-FLGODK = YES                                             
047100            MOVE    JA         TO REG2-FLGODK                             
047200         END-IF                                                           
047300      ELSE                                                                
047400         MOVE NEJ              TO NYCKLAR-SW                              
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 BG-KOLLA-IDDC    SECTION.                                                
047900                                                                          
048000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
048100                                                                          
048200     IF EGEN-MID OR HELP-MID                                              
048300       IF MID-IDDC-IN = ALL '+'                                           
048400         MOVE MSGI-IDDC   TO W-IDDC-B6                                    
048500       ELSE                                                               
048600         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
048700         MOVE    SPACE    TO MFS-KDTRTYP                                  
048800       END-IF                                                             
048900     ELSE                                                                 
049000       IF GODK-MID                                                        
049100         MOVE MSGI-IDDC   TO W-IDDC-B6                                    
049200       ELSE                                                               
049300         MOVE SPACE       TO W-IDDC-B6                                    
049400       END-IF                                                             
049500     END-IF                                                               
049600     PERFORM IMS-GU-WDB601                                                
049700                                                                          
049800     IF DCS-CDC OR DCS-CDC-TR                                             
049900       MOVE  DCS-IDDC  TO REG2-IDDC                                       
050000     ELSE                                                                 
050100       MOVE    NEJ     TO NYCKLAR-SW                                      
050200       MOVE   SPACE    TO DCS-IDDC                                        
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 BH-FLYTTA-OEVRIGA-NYCKLAR  SECTION.                                      
050700     SKIP2                                                                
050800     MOVE MFS-RENSA-FAELT      TO MOD-FLKLIVIS-IN                         
050900                                  MOD-IDLOPNRM-IN                         
051000                                                                          
051100     IF MID-FLKLIVIS-IN   = ALL '+'                                       
051200         MOVE MID-FLKLIVIS-UT  TO MOD-FLKLIVIS-UT                         
051300     ELSE                                                                 
051400         MOVE MID-FLKLIVIS-IN  TO MOD-FLKLIVIS-UT                         
051500     END-IF                                                               
051600                                                                          
051700     IF MID-IDLOPNRM-IN   = ALL '+'                                       
051800         MOVE MID-IDLOPNRM-UT  TO MOD-IDLOPNRM-UT                         
051900     ELSE                                                                 
052000         MOVE MID-IDLOPNRM-IN  TO MOD-IDLOPNRM-UT                         
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 BI-KOLLA-SDC-NDC-LEV  SECTION.                                           
052500                                                                          
052600     IF WS-IDLEVNR NOT = SPACE                                            
052700       MOVE WS-IDLEVNR TO W-IDDC-B6-LEV                                   
052800       PERFORM IMS-GU-WDB601-LEV                                          
052900       IF SEGMENT-FINNS                                                   
053000          MOVE NEJ TO NYCKLAR-SW                                          
053100       END-IF                                                             
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 G-KOLLA-INPUT-UPPDATERA SECTION.                                         
053600                                                                          
053700     MOVE JA                   TO INDATA-SW                               
053800     MOVE SPACE                TO MED-IDMFSFEL                            
053900     PERFORM GA-KOLLA-KOLUMNERNA                                          
054000     IF INDATA-FEL                                                        
054100         IF INDATA-EJ-IFYLLT                                              
054200             MOVE INF-UPDATE-NOT-DONE  TO MED-IDMFSFEL                    
054300          ELSE                                                            
054400             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
054500         END-IF                                                           
054600         CALL WMEDKONV USING MED-WMEDAREA                                 
054700         MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                      
054800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
054900     ELSE                                                                 
055000         PERFORM GB-ANROPA-W611REG                                        
055100         IF WS-FLGODK    = JA OR YES OR NEJ                               
055200             CONTINUE                                                     
055300          ELSE                                                            
055400             PERFORM GC-KOLLA-FELFLAGGOR                                  
055500         END-IF                                                           
055600                                                                          
055700         IF W611REG-OK                                                    
055800             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
055900             CALL WMEDKONV USING MED-WMEDAREA                             
056000             MOVE MED-TEMFSINF      TO MOD-TEMFSINF                       
056100             PERFORM MFS-FORM-ATTR                                        
056200             PERFORM MFS-RENSA-FAELT-IN                                   
056300          ELSE                                                            
056400             IF MED-IDMFSINF =  INF-ADVICE-N-EXIST                        
056500                 CALL WMEDKONV USING MED-WMEDAREA                         
056600                 MOVE MED-TEMFSINF     TO MOD-TEMFSINF                    
056700               ELSE                                                       
056800                 IF MED-IDMFSFEL       = SPACE                            
056900                     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL            
057000                 ELSE                                                     
057100                   IF MED-IDMFSFEL = INF-PART-SUPERSEDED  OR              
057200                                     INF-AVROP-SAKNAS                     
057300                      MOVE MFS-OEPPNA-ALFA-FAELT TO                       
057400                                         MOD-FLGODK-IN-ATTR               
057500                      PERFORM MFS-STAENG-KOL-FAELT                        
057600                   END-IF                                                 
057700                 END-IF                                                   
057800             END-IF                                                       
057900             CALL WMEDKONV USING MED-WMEDAREA                             
058000             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
058100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
058200         END-IF                                                           
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 GA-KOLLA-KOLUMNERNA SECTION.                                             
058700                                                                          
058800     PERFORM GAA-KOLLA-KOLUMN                                             
058900     IF INDATA-EJ-IFYLLT                                                  
059000         MOVE NEJ              TO INDATA-SW                               
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 GAA-KOLLA-KOLUMN    SECTION.                                             
059500                                                                          
059600     MOVE +1 TO RAD-IX1                                                   
059700     PERFORM UNTIL RAD-IX1     > MAX-RAD                                  
059800         EVALUATE TRUE                                                    
059900           WHEN MID-IDARTNR(RAD-IX1) = ALL '+' AND                        
060000                MID-KVAVIS (RAD-IX1) = ALL '+' AND                        
060100               (MID-IDFS  (RAD-IX1)  = ALL '+' OR SPACE)                  
060200                 CONTINUE                                                 
060300                                                                          
060400           WHEN MID-IDARTNR(RAD-IX1) NUMERIC AND                          
060500                MID-KVAVIS (RAD-IX1) NUMERIC AND                          
060600               (MID-IDFS   (RAD-IX1) NOT = SPACE AND                      
060700               (MID-IDFS   (RAD-IX1) NOT = ALL '+'))                      
060800                MOVE MFS-NUM-FAELT-RAETT TO                               
060900                            MOD-IDARTNR-ATTR (RAD-IX1)                    
061000                            MOD-KVAVIS-ATTR  (RAD-IX1)                    
061100                MOVE JA               TO INDATA-IFYLLT-SW                 
061200                MOVE 1   TO INDX                                          
061300                MOVE NEJ TO FL-IDFS-OK                                    
061400                MOVE MID-IDFS(RAD-IX1) TO WS-IDFS                         
061500                PERFORM UNTIL INDX > 8 OR FL-IDFS-OK = JA                 
061600                  IF WS-IDFS(INDX:1) NUMERIC                              
061700                     IF WS-IDFS(INDX:1) > ZERO                            
061800                        MOVE JA TO FL-IDFS-OK                             
061900                     END-IF                                               
062000                  END-IF                                                  
062100                  ADD 1 TO INDX                                           
062200                END-PERFORM                                               
062300                                                                          
062400                IF FL-IDFS-OK = JA                                        
062500                   MOVE MFS-ALFA-FAELT-RAETT TO                           
062600                              MOD-IDFS-ATTR (RAD-IX1)                     
062700                ELSE                                                      
062800                   MOVE MFS-ALFA-FAELT-FEL TO                             
062900                            MOD-IDFS-ATTR (RAD-IX1)                       
063000                   MOVE NEJ      TO INDATA-SW                             
063100                END-IF                                                    
063200                                                                          
063300           WHEN OTHER                                                     
063400                 MOVE MFS-NUM-FAELT-FEL TO                                
063500                            MOD-IDARTNR-ATTR (RAD-IX1)                    
063600                            MOD-KVAVIS-ATTR  (RAD-IX1)                    
063700                 MOVE MFS-ALFA-FAELT-FEL TO                               
063800                            MOD-IDFS-ATTR (RAD-IX1)                       
063900                 MOVE NEJ      TO INDATA-SW                               
064000                                                                          
064100         END-EVALUATE                                                     
064200         ADD +1                TO RAD-IX1                                 
064300     END-PERFORM                                                          
064400     .                                                                    
064500     EJECT                                                                
064600 GB-ANROPA-W611REG  SECTION.                                              
064700                                                                          
064800     PERFORM GBA-SKAPA-LAENKAREA                                          
064900                                                                          
065000     CALL W611REG USING LAENK-W611REG0                                    
065100                        REG-INLA1-PCB   REG-INLA2-PCB                     
065200                        REG-INLA3-PCB   REG-LEVA-PCB                      
065300                                        REG-ARTC-PCB                      
065400                        REG-BENA-PCB    REG-WDD9-PCB                      
065500                        REG-WDK7-PCB    REG-WDB6-PCB                      
065600     .                                                                    
065700     EJECT                                                                
065800 GBA-SKAPA-LAENKAREA  SECTION.                                            
065900                                                                          
066000     MOVE '6112'               TO REG2-IDTRANS                            
066100     MOVE WS-IDLEVNR           TO REG2-IDLEVNR                            
066200     MOVE WS-KDRT              TO REG2-KDRT                               
066300     MOVE WS-TIAVIDAT          TO REG2-TIAVIDAT                           
066400     MOVE WS-IDLBBET           TO REG2-IDLBBET                            
066500                                                                          
066600     MOVE +1                   TO RAD-IX2                                 
066700                                  RAD-IX1                                 
066800     PERFORM UNTIL RAD-IX1      >  MAX-RAD                                
066900         IF MID-IDARTNR(RAD-IX1)       = ALL '+'                          
067000             MOVE ZERO                  TO REG2-IDARTNR (RAD-IX2)         
067100                                           REG2-KVAVIS  (RAD-IX2)         
067200             MOVE SPACE                 TO REG2-IDFS    (RAD-IX2)         
067300          ELSE                                                            
067400             MOVE MID-IDARTNR(RAD-IX1) TO REG2-IDARTNR (RAD-IX2)          
067500             MOVE MID-KVAVIS(RAD-IX1)  TO REG2-KVAVIS  (RAD-IX2)          
067600             MOVE MID-IDFS  (RAD-IX1)  TO REG2-IDFS    (RAD-IX2)          
067700         END-IF                                                           
067800         ADD +1                         TO RAD-IX1                        
067900                                           RAD-IX2                        
068000     END-PERFORM                                                          
068100     .                                                                    
068200     EJECT                                                                
068300 GC-KOLLA-FELFLAGGOR  SECTION.                                            
068400                                                                          
068500     MOVE +1                   TO RAD-IX2                                 
068600     PERFORM GCA-KOLLA-FELFLAGGOR-KOLUMN                                  
068700                                                                          
068800     IF REG2-IDLEVNR-OK                 =  JA  AND                        
068900        REG2-KDRT-OK                    =  JA  AND                        
069000        REG2-TIAVIDAT-OK                =  JA                             
069100         IF REG2-FLGODK-IDARTNR         =  JA OR                          
069200            REG2-FLGODK-IDFS            =  JA                             
069300           IF MED-IDMFSFEL =  INF-ADVICE-N-EXIST                          
069400             MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLGODK-IN-ATTR             
069500             PERFORM MFS-STAENG-KOL-FAELT                                 
069600           END-IF                                                         
069700         END-IF                                                           
069800      ELSE                                                                
069900         MOVE ERR-IN-LINE-ONE       TO MED-IDMFSFEL                       
070000         MOVE NEJ                   TO W611REG-SW                         
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 GCA-KOLLA-FELFLAGGOR-KOLUMN  SECTION.                                    
070500                                                                          
070600     MOVE +1                   TO RAD-IX1                                 
070700     PERFORM UNTIL RAD-IX1     >  MAX-RAD                                 
070800         IF REG2-IDARTNR-OK (RAD-IX2) = JA                                
070900             MOVE MFS-NUM-FAELT-RAETT     TO                              
071000                         MOD-IDARTNR-ATTR (RAD-IX1)                       
071100          ELSE                                                            
071200             IF MED-IDMFSFEL = SPACE OR INF-ADVICE-N-EXIST                
071300               MOVE REG2-IDMFSFEL (RAD-IX2) TO MED-IDMFSFEL               
071400             END-IF                                                       
071500             MOVE MFS-NUM-FAELT-FEL       TO                              
071600                         MOD-IDARTNR-ATTR (RAD-IX1)                       
071700             MOVE NEJ                     TO W611REG-SW                   
071800         END-IF                                                           
071900                                                                          
072000         IF REG2-IDFS-OK   (RAD-IX2)      = JA                            
072100             MOVE MFS-ALFA-FAELT-RAETT TO                                 
072200                         MOD-IDFS-ATTR (RAD-IX1)                          
072300          ELSE                                                            
072400             MOVE MFS-ALFA-FAELT-FEL      TO                              
072500                         MOD-IDFS-ATTR (RAD-IX1)                          
072600             MOVE NEJ                     TO W611REG-SW                   
072700         END-IF                                                           
072800         ADD +1                           TO RAD-IX1                      
072900                                             RAD-IX2                      
073000     END-PERFORM                                                          
073100                                                                          
073200     IF MED-IDMFSFEL = SPACE OR INF-ADVICE-N-EXIST                        
073300       CONTINUE                                                           
073400     ELSE                                                                 
073500       MOVE +1                            TO RAD-IX1                      
073600                                             RAD-IX2                      
073700       PERFORM UNTIL RAD-IX1 > MAX-RAD                                    
073800         IF REG2-IDMFSFEL (RAD-IX2) = INF-ADVICE-N-EXIST                  
073900            MOVE MFS-ALFA-FAELT-RAETT  TO                                 
074000                     MOD-IDARTNR-ATTR (RAD-IX1)                           
074100         END-IF                                                           
074200         ADD +1                           TO RAD-IX1                      
074300                                             RAD-IX2                      
074400       END-PERFORM                                                        
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 F-LAES-VISA-INFO        SECTION.                                         
074900                                                                          
075000     IF MID-FLGODK-IN               = ALL '+'                             
075100         MOVE MFS-RENSA-FAELT       TO MOD-FLGODK-IN-ATTR                 
075200      ELSE                                                                
075300         MOVE MID-FLGODK-IN         TO MOD-FLGODK-IN                      
075400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLGODK-IN-ATTR                 
075500     END-IF                                                               
075600                                                                          
075700     PERFORM FA-FLYTTA-KOLUMN-FAELT                                       
075800     .                                                                    
075900     EJECT                                                                
076000 FA-FLYTTA-KOLUMN-FAELT    SECTION.                                       
076100                                                                          
076200     MOVE +1                        TO RAD-IX1                            
076300     PERFORM UNTIL RAD-IX1          >  MAX-RAD                            
076400         IF MID-IDARTNR(RAD-IX1)   =  ALL '+'                             
076500             MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR    (RAD-IX1)           
076600          ELSE                                                            
076700             MOVE MID-IDARTNR(RAD-IX1) TO                                 
076800                                       MOD-IDARTNR    (RAD-IX1)           
076900             MOVE MFS-ADD-LAES-IN-FAELT TO                                
077000                                 MOD-IDARTNR-ATTR (RAD-IX1)               
077100         END-IF                                                           
077200                                                                          
077300         IF MID-KVAVIS(RAD-IX1)    =  ALL '+'                             
077400             MOVE MFS-RENSA-FAELT   TO MOD-KVAVIS    (RAD-IX1)            
077500          ELSE                                                            
077600             MOVE MID-KVAVIS(RAD-IX1) TO                                  
077700                                       MOD-KVAVIS    (RAD-IX1)            
077800             MOVE MFS-ADD-LAES-IN-FAELT TO                                
077900                                 MOD-KVAVIS-ATTR  (RAD-IX1)               
078000         END-IF                                                           
078100                                                                          
078200         IF MID-IDFS(RAD-IX1)      =  ALL '+'                             
078300             MOVE MFS-RENSA-FAELT   TO MOD-IDFS      (RAD-IX1)            
078400          ELSE                                                            
078500             MOVE MID-IDFS(RAD-IX1)  TO                                   
078600                                       MOD-IDFS      (RAD-IX1)            
078700             MOVE MFS-ADD-LAES-IN-FAELT TO                                
078800                                 MOD-IDFS-ATTR    (RAD-IX1)               
078900         END-IF                                                           
079000         ADD +1                     TO RAD-IX1                            
079100     END-PERFORM                                                          
079200     .                                                                    
079300     EJECT                                                                
079400 MFS-RENSA-FAELT-IN SECTION.                                              
079500                                                                          
079600*    --- ALLA UTDATA-FÄLT                                                 
079700     MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-IN                          
079800                                  MOD-FLGODK-IN                           
079900     MOVE +1                   TO RAD-IX1                                 
080000     PERFORM UNTIL RAD-IX1     >  MAX-RAD                                 
080100         MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR    (RAD-IX1)                
080200                                  MOD-KVAVIS     (RAD-IX1)                
080300                                  MOD-IDFS       (RAD-IX1)                
080400         ADD +1                TO RAD-IX1                                 
080500     END-PERFORM                                                          
080600     .                                                                    
080700     SKIP2                                                                
080800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
080900                                                                          
081000     MOVE +1                     TO RAD-IX1                               
081100     PERFORM UNTIL RAD-IX1       >  MAX-RAD                               
081200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR    (RAD-IX1)              
081300                                    MOD-KVAVIS     (RAD-IX1)              
081400                                    MOD-IDFS       (RAD-IX1)              
081500         ADD +1                  TO RAD-IX1                               
081600     END-PERFORM                                                          
081700     .                                                                    
081800     EJECT                                                                
081900 MFS-FORM-ATTR SECTION.                                                   
082000                                                                          
082100*    --- ALLA INDATA-FÄLT                                                 
082200     MOVE MFS-FORMATETS-ATTR   TO MOD-IDLBBET-IN                          
082300                                  MOD-FLGODK-IN                           
082400                                                                          
082500     MOVE +1                     TO RAD-IX1                               
082600     PERFORM UNTIL RAD-IX1       >  MAX-RAD                               
082700         MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR (RAD-IX1)            
082800                                    MOD-KVAVIS-ATTR  (RAD-IX1)            
082900                                    MOD-IDFS-ATTR    (RAD-IX1)            
083000         ADD +1                  TO RAD-IX1                               
083100     END-PERFORM                                                          
083200     .                                                                    
083300     SKIP2                                                                
083400 MFS-LAES-IN-IGEN SECTION.                                                
083500                                                                          
083600     MOVE +1                     TO RAD-IX1                               
083700     PERFORM UNTIL RAD-IX1       >  MAX-RAD                               
083800         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
083900                                    MOD-IDARTNR-ATTR (RAD-IX1)            
084000                                    MOD-KVAVIS-ATTR  (RAD-IX1)            
084100                                    MOD-IDFS-ATTR    (RAD-IX1)            
084200         ADD +1                  TO RAD-IX1                               
084300     END-PERFORM                                                          
084400     .                                                                    
084500     SKIP2                                                                
084600 MFS-STAENG-KOL-FAELT  SECTION.                                           
084700                                                                          
084800     MOVE +1                   TO RAD-IX2                                 
084900                                  RAD-IX1                                 
085000     PERFORM UNTIL RAD-IX1     >  MAX-RAD                                 
085100         IF REG2-IDARTNR-OK (RAD-IX2) = JA                                
085200             MOVE MFS-STAENG-FAELT    TO                                  
085300                                  MOD-IDARTNR-ATTR (RAD-IX1)              
085400          ELSE                                                            
085500             MOVE MFS-STAENG-FAELT-HI TO                                  
085600                                  MOD-IDARTNR-ATTR (RAD-IX1)              
085700         END-IF                                                           
085800         IF REG2-IDFS-OK (RAD-IX2)     = JA                               
085900             MOVE MFS-STAENG-FAELT     TO                                 
086000                                  MOD-IDFS-ATTR    (RAD-IX1)              
086100          ELSE                                                            
086200             MOVE MFS-STAENG-FAELT-HI TO                                  
086300                                  MOD-IDFS-ATTR    (RAD-IX1)              
086400         END-IF                                                           
086500         MOVE MFS-STAENG-FAELT TO MOD-KVAVIS-ATTR  (RAD-IX1)              
086600         ADD +1                TO RAD-IX1                                 
086700                                  RAD-IX2                                 
086800     END-PERFORM                                                          
086900     .                                                                    
087000     SKIP2                                                                
087100* --- IMS SEKTIONER ---                                                   
087200     SKIP3                                                                
087300 IMS-GET-MSG SECTION.                                                     
087400                                                                          
087500     MOVE '  QC' TO GODK-STATUSKODER                                      
087600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
087700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000     SKIP3                                                                
088100 IMS-INSERT-MSG SECTION.                                                  
088200                                                                          
088300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
088400       MOVE '0' TO MFS-KDHUVOMR                                           
088500     END-IF                                                               
088600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088700     MOVE SPACE TO GODK-STATUSKODER                                       
088800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     .                                                                    
089200 IMS-GU-WDB601-LEV SECTION.                                               
089300     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
089400          DELIMITED BY SIZE INTO SSA1                                     
089500     MOVE '  GE' TO GODK-STATUSKODER                                      
089600     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
089700     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
089800     PERFORM IMS-STATUSKONTROLL                                           
089900     .                                                                    
090000     EJECT                                                                
090100 IMS-GU-WDB601    SECTION.                                                
090200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
090600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     IF SEGMENT-SAKNAS                                                    
090900         MOVE SPACE TO DCS-KDDC                                           
091000     END-IF                                                               
091100     .                                                                    
091200     EJECT                                                                
091300 IMS-STATUSKONTROLL SECTION.                                              
091400                                                                          
091500     SET STATUS-IX TO 1                                                   
091600     SEARCH GODK-STATUS                                                   
091700       AT END                                                             
091800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
091900         DELIMITED BY SIZE INTO FELTEXT                                   
092000         CALL FELLOG                                                      
092100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
092200         CONTINUE                                                         
092300     END-SEARCH                                                           
092400     .                                                                    
