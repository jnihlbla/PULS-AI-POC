000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4026800.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   91/05/23.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MPP PROGRAM SOM:                                                 
001100*        - GÖR ÄNDRINGAR AV KOMPLETTERINGSINFO I ETT BEFINTLIGT           
001200*          PROFORMAHUVUD                                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER OCH UPPDATERAR :                                
001500*                           - WLPROC (WDE8) PROFORMA HUVUD                
001600*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T268  W4T268U                                     
002100*        MID:         W4I26801                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O26801                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001*    -COPY WY2000W1                                                       
003010     SKIP3                                                                
003100 77  IDPGM                       PIC X(08)   VALUE 'W4026800'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 77  UPPDAT-TID                  PIC 9(8)    VALUE ZERO.                  
004200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004300 77  WS-INDEX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1340 COMP SYNC.        
004500                                                                          
004600*    -ARBETSETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004800 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
004900 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
005000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005010 01  WS-IDKUNDRF-RED.                                                     
005020     03 WS-IDKUNDRF-1-7          PIC X(7)    VALUE SPACE.                 
005030     03 WS-IDKUNDRF-8-10         PIC X(3)    VALUE SPACE.                 
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200     88  ALLT-FEL                            VALUE 'N'.                   
006300                                                                          
006400 77  OTILL-UPPDAT-SW             PIC X       VALUE 'J'.                   
006500     88  OTILL-UPPDATERING                   VALUE 'N'.                   
006600                                                                          
006700 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
006800     88  UPPDATERING-OK                      VALUE 'J'.                   
006900     88  UPPDATERING-EJ                      VALUE 'N'.                   
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '4268'.                
007300     88  GODK-MID                            VALUE '4262' '4263'          
007400                                                   '4264' '4265'          
007500                                                   '4266' '4267'          
007600                                                   '4268' '4269'.         
007700     EJECT                                                                
009800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009900 01  GENERELLA-SUBPROGRAM.                                                
010000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010310     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010600*   -COPY WMEDAREA                                                        
010800     EJECT                                                                
010810*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010820*   -COPY WMSGINIT                                                        
010830     EJECT                                                                
010900 01  FELM-CODES.                                                          
011000     03  FILLER                  PIC X(16)   VALUE 'FELM AREA'.           
011200     03  FELM-OTILL-UPPDATERING  PIC X(3)    VALUE '007'.                 
011300     03  FELM-FINNS-EJ           PIC X(3)    VALUE '010'.                 
011400     03  FELM-PF11-O-EJ-INDATA   PIC X(3)    VALUE '011'.                 
011510     03  FELM-ORDERN-ANNULLERAD  PIC X(3)    VALUE '052'.                 
011600     03  FELM-FEL-NYCKEL         PIC X(3)    VALUE '401'.                 
011700     03  FELM-OBEHOERIG          PIC X(3)    VALUE '405'.                 
011900     SKIP3                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  FILLER                  PIC X(16)   VALUE 'INFO AREA'.           
012200     03  INFO-TRYCK-PF11         PIC X(3)    VALUE '003'.                 
012300     03  INFO-UPPDAT-GJORD       PIC X(3)    VALUE '101'.                 
012400     EJECT                                                                
012500*01  -COPY WDECAREA                                                       
012700     EJECT                                                                
012800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013100     SKIP3                                                                
013200*01  MID -COPY W4I26801                                                   
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013600     SKIP3                                                                
013700*01  -COPY WMSGAREA                                                       
013900     EJECT                                                                
014000     03  MOD REDEFINES MSG-AREA.                                          
014100*      05  -COPY W4O26801                                                 
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014500     SKIP3                                                                
014600*01  -COPY WMFSAREA                                                       
014800     EJECT                                                                
016000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016200     SKIP3                                                                
016300*    --- STATUS-KOD FRÅN IMS                                              
016400 01  STATUS-WS                   PIC XX.                                  
016500     88  SEGMENT-FINNS                       VALUE '  '.                  
016600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016800     SKIP2                                                                
016900 01  GODK-STATUSKODER.                                                    
017000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017100     SKIP3                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300*--------------------WDE8                                                 
017400     03  W-WDE801KY-X.                                                    
017500         05  W-PHUV-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
017600         05  W-PHUV-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
017700         05  W-PHUV-IDKUNDRF.                                             
017800            07 W-PHUV-IDORDNR7   PIC 9(7)     VALUE ZERO.                 
017900            07 FILLER            PIC X(3)     VALUE SPACE.                
019400     SKIP3                                                                
019500 01  SSA1                        PIC X(94).                               
019700     EJECT                                                                
019800*    --- IMS FUNKTIONSKODER                                               
019900*01  -COPY W0003                                                          
020100     EJECT                                                                
020200*    ---  DLI INPUT-OUTPUT AREA                                           
020300                                                                          
020800 01  FILLER                      PIC X(16)   VALUE                        
020900                                              'IO-WDE801'.                
021000 01  DLI-IO-AREA-WDE801.                                                  
021100     03  WLPROC01.                                                        
021200*        05  -COPY WDE801                                                 
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700*01  -COPY W0009      -PRE MSG-                                           
021900     EJECT                                                                
022000*01  -COPY W0008      -PRE USEA-                                          
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008      -PRE PROC-                                          
022500     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB   PROC-PCB.                 
022900     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB   PROC-PCB.                 
023000                                                                          
023100     PERFORM IMS-GET-MSG                                                  
023200     IF SEGMENT-FINNS                                                     
023300       PERFORM A-INIT                                                     
023400       PERFORM B-KOLLA-NYCKLAR                                            
023500                                                                          
023600       IF NYCKLAR-OK                                                      
023700         PERFORM IMS-GHU-PROC-WDE801-PHUV                                 
023800                                                                          
023900         IF SEGMENT-FINNS                                                 
024000           PERFORM C-KOLLA-OM-RAETT-PHUV                                  
024100                                                                          
024200           IF ALLT-OK                                                     
024300             IF MFS-UPDATE                                                
024400               PERFORM D-UPPDATERA                                        
024500             ELSE                                                         
024600               PERFORM E-BEHANDLA-ENTER                                   
024700             END-IF                                                       
024800                                                                          
024900             PERFORM F-LAES-VISA-BILD                                     
025000           END-IF                                                         
025100         ELSE                                                             
025200           MOVE FELM-FINNS-EJ TO MED-IDMFSFEL                             
025300           CALL WMEDKONV USING MED-WMEDAREA                               
025400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
025500         END-IF                                                           
025600       END-IF                                                             
025700                                                                          
025800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025900       PERFORM IMS-INSERT-MSG                                             
026000     END-IF                                                               
026100                                                                          
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT SECTION.                                                          
026700                                                                          
026800     IF MSG-DUBBLA-TRANSKODER                                             
026900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I26801                 
027000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027200     ELSE                                                                 
027300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I26801                  
027400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027600     END-IF                                                               
027700                                                                          
027800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
027900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
028000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028100                                                                          
028200     MOVE LOW-VALUE TO MSG-AREA                                           
028300     MOVE 'W4O26801' TO MFS-IDMOD                                         
028400     MOVE '4268' TO MOD-IDTRANS                                           
028500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028600                                                                          
028700     ACCEPT DAGENS-DATUM FROM DATE                                        
028800     ACCEPT UPPDAT-TID   FROM TIME                                        
028900                                                                          
029000     IF NOT EGEN-MID                                                      
029100       MOVE SPACE TO MFS-KDTRTYP                                          
029200       MOVE ' ' TO MFS-IDPFK                                              
029300       PERFORM MFS-RENSA-ALLA-FAELT                                       
029310       MOVE MFS-RENSA-FAELT TO MID-IDDISTR-IN                             
029320                               MID-IDKUNDNR-IN                            
029330                               MID-IDORDNR7-IN                            
029340                               MID-IDARTNR-IN                             
029350                               MID-IDDISTR-UT                             
029360                               MID-IDKUNDNR-UT                            
029370                               MID-IDORDNR7-UT                            
029380                               MID-IDARTNR-UT                             
029400     END-IF                                                               
029500                                                                          
029600     IF ENGLISH-TEXT                                                      
029700       MOVE +2 TO SPRAK-IX                                                
029800       MOVE 'GB ' TO MED-IDSKYLT                                          
029900     ELSE                                                                 
030000       MOVE +1 TO SPRAK-IX                                                
030100       MOVE 'S  ' TO MED-IDSKYLT                                          
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 B-KOLLA-NYCKLAR SECTION.                                                 
030600                                                                          
030700     MOVE JA TO NYCKLAR-SW                                                
030710                                                                          
030720     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030730     MOVE '001'             TO MSGI-KDCALL                                
030740     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030741     MOVE '4268'            TO MSGI-IDTRANS                               
030742     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030750     IF MFS-IDTRANS = '4268'                                              
030760        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
030770        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
030772                                                                          
030773        MOVE MID-IDORDNR7-IN TO WS-IDKUNDRF-1-7                           
030774        IF WS-IDKUNDRF-1-7 = ALL '+'                                      
030775          MOVE '+++'         TO WS-IDKUNDRF-8-10                          
030776        ELSE                                                              
030777          MOVE SPACE         TO WS-IDKUNDRF-8-10                          
030778        END-IF                                                            
030779        MOVE WS-IDKUNDRF-RED TO MSGI-IDKUNDRF                             
030780        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
030781     END-IF                                                               
030790     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030800                                                                          
030900     PERFORM BA-KOLLA-DISTRIKT                                            
031000     PERFORM BB-KOLLA-KUNDNR                                              
031100     PERFORM BC-KOLLA-ORDER                                               
031200                                                                          
031300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
031400                                                                          
031500                                                                          
031600     IF NYCKLAR-FEL                                                       
031700       MOVE FELM-FEL-NYCKEL TO MED-IDMFSFEL                               
031800       CALL WMEDKONV USING MED-WMEDAREA                                   
031900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032100       PERFORM MFS-RENSA-ALLA-FAELT                                       
032200                                                                          
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 BA-KOLLA-DISTRIKT SECTION.                                               
033300                                                                          
033400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
033410                                                                          
033420     MOVE MSGI-IDDISTR   TO WS-IDDISTR                                    
033430     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
033440                                                                          
033450     IF MID-IDDISTR-IN = ALL '+'                                          
033460       CONTINUE                                                           
033470     ELSE                                                                 
033480       MOVE ' '         TO MFS-IDPFK                                      
033490       MOVE SPACE       TO MFS-KDTRTYP                                    
033491     END-IF                                                               
033492                                                                          
034500     MOVE WS-IDDISTR TO MOD-IDDISTR-UT                                    
034600     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
034700                                                                          
034800     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
034900       MOVE WS-IDDISTR TO W-PHUV-IDDISTR                                  
035000     ELSE                                                                 
035100       MOVE NEJ TO NYCKLAR-SW                                             
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 BB-KOLLA-KUNDNR SECTION.                                                 
035600                                                                          
035700     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
035710                                                                          
035720     MOVE MSGI-IDKUNDNR    TO WS-IDKUNDNR                                 
035730     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
035740                                                                          
035750     IF MID-IDKUNDNR-IN = ALL '+'                                         
035760       CONTINUE                                                           
035770     ELSE                                                                 
035780       MOVE ' '         TO MFS-IDPFK                                      
035790       MOVE SPACE       TO MFS-KDTRTYP                                    
035791     END-IF                                                               
035792                                                                          
035793     MOVE WS-IDKUNDNR TO MOD-IDKUNDNR-UT                                  
035794     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
035795     IF MOD-IDKUNDNR-UT = SPACE                                           
035796       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
035797     END-IF                                                               
038100                                                                          
038200     IF WS-IDKUNDNR NUMERIC                                               
038300       MOVE WS-IDKUNDNR TO W-PHUV-IDKUNDNR                                
038400     ELSE                                                                 
038500       MOVE NEJ TO NYCKLAR-SW                                             
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 BC-KOLLA-ORDER SECTION.                                                  
039000                                                                          
039100     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-IN                              
039210                                                                          
039220     MOVE MSGI-IDKUNDRF    TO WS-IDORDNR7                                 
039230     INSPECT WS-IDORDNR7 REPLACING LEADING SPACE BY ZERO                  
039240                                                                          
039300     IF MID-IDORDNR7-IN = ALL '+'                                         
039400       CONTINUE                                                           
039600     ELSE                                                                 
039800       MOVE ' '         TO MFS-IDPFK                                      
039900       MOVE SPACE       TO MFS-KDTRTYP                                    
040000     END-IF                                                               
040100                                                                          
040200     MOVE WS-IDORDNR7 TO MOD-IDORDNR7-UT                                  
040300     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
040400                                                                          
040500     IF WS-IDORDNR7 NUMERIC                                               
040600       MOVE WS-IDORDNR7 TO W-PHUV-IDORDNR7                                
040700     ELSE                                                                 
040800       MOVE NEJ TO NYCKLAR-SW                                             
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 C-KOLLA-OM-RAETT-PHUV SECTION.                                           
041300                                                                          
041400     MOVE JA TO ALLT-SW                                                   
041500                                                                          
041600     IF PHUV-FLBORT    = 'N'                                              
041700                                                                          
041800       IF MFS-UPDATE                                                      
041801         MOVE PHUV-TIFORDAT   TO TMP1-YYMMDD                              
041802         MOVE DAGENS-DATUM    TO TMP2-YYMMDD                              
041810         PERFORM WY2000P1                                                 
041900         IF TMP1-YYMMDD > TMP2-YYMMDD AND                                 
042000            PHUV-TIORDDAT = ZERO                                          
042100                                                                          
042200           PERFORM S01-KOLLA-BEHOERIGHET                                  
042400                                                                          
042500         ELSE                                                             
042600           PERFORM CA-VISA-OTILL-UPPDATERING                              
042700         END-IF                                                           
042800                                                                          
042900       ELSE                                                               
043000         PERFORM S01-KOLLA-BEHOERIGHET                                    
043200       END-IF                                                             
043300                                                                          
043400     ELSE                                                                 
043500       MOVE FELM-ORDERN-ANNULLERAD TO MED-IDMFSFEL                        
043510       CALL WMEDKONV USING MED-WMEDAREA                                   
043520       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043530       MOVE NEJ TO  ALLT-SW                                               
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043810 CA-VISA-OTILL-UPPDATERING SECTION.                                       
043820                                                                          
043830     MOVE NEJ TO OTILL-UPPDAT-SW                                          
043840     MOVE FELM-OTILL-UPPDATERING TO MED-IDMFSFEL                          
043850     CALL WMEDKONV USING MED-WMEDAREA                                     
043860     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
043870     .                                                                    
043880     EJECT                                                                
043900 D-UPPDATERA SECTION.                                                     
044000                                                                          
044100     IF NOT OTILL-UPPDATERING                                             
044200       IF MID-INPUT NOT = ALL '+'                                         
044300                                                                          
045500           PERFORM DA-AENDRA-I-DB                                         
045600           MOVE JA TO UPPDAT-SW                                           
045800                                                                          
045900       ELSE                                                               
046000         MOVE FELM-PF11-O-EJ-INDATA TO MED-IDMFSFEL                       
046010         CALL WMEDKONV USING MED-WMEDAREA                                 
046020         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
046100                                                                          
046200       END-IF                                                             
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
080600 DA-AENDRA-I-DB SECTION.                                                  
080700                                                                          
080800     IF MID-TEBETVIL(1) NOT = ALL '+'                                     
080900       MOVE MID-TEBETVIL(1)       TO PHUV-TEBETVIL(1)                     
081000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBETVIL-ATTR(1)                 
081200     END-IF                                                               
081300                                                                          
081310     IF MID-TEBETVIL(2) NOT = ALL '+'                                     
081320       MOVE MID-TEBETVIL(2)       TO PHUV-TEBETVIL(2)                     
081330       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBETVIL-ATTR(2)                 
081340     END-IF                                                               
081350                                                                          
081360     IF MID-TEBETVIL(3) NOT = ALL '+'                                     
081370       MOVE MID-TEBETVIL(3)       TO PHUV-TEBETVIL(3)                     
081380       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBETVIL-ATTR(3)                 
081390     END-IF                                                               
081391                                                                          
081392     IF MID-TEBETVIL(4) NOT = ALL '+'                                     
081393       MOVE MID-TEBETVIL(4)       TO PHUV-TEBETVIL(4)                     
081394       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEBETVIL-ATTR(4)                 
081395     END-IF                                                               
081396                                                                          
082000     IF MID-TEGILTIG(1) NOT = ALL '+'                                     
082100       MOVE MID-TEGILTIG(1)       TO PHUV-TEGILTIG(1)                     
082200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEGILTIG-ATTR(1)                 
082400     END-IF                                                               
082500                                                                          
082510     IF MID-TEGILTIG(2) NOT = ALL '+'                                     
082520       MOVE MID-TEGILTIG(2)       TO PHUV-TEGILTIG(2)                     
082530       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEGILTIG-ATTR(2)                 
082540     END-IF                                                               
082550                                                                          
082560     IF MID-TEGILTIG(3) NOT = ALL '+'                                     
082570       MOVE MID-TEGILTIG(3)       TO PHUV-TEGILTIG(3)                     
082580       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEGILTIG-ATTR(3)                 
082590     END-IF                                                               
082591                                                                          
082592     IF MID-TEGILTIG(4) NOT = ALL '+'                                     
082593       MOVE MID-TEGILTIG(4)       TO PHUV-TEGILTIG(4)                     
082594       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEGILTIG-ATTR(4)                 
082595     END-IF                                                               
082596                                                                          
082600     IF MID-TELEVVIL(1) NOT = ALL '+'                                     
082700       MOVE MID-TELEVVIL(1)       TO PHUV-TELEVVIL(1)                     
082900       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELEVVIL-ATTR(1)                 
083000     END-IF                                                               
083100                                                                          
083110     IF MID-TELEVVIL(2) NOT = ALL '+'                                     
083120       MOVE MID-TELEVVIL(2)       TO PHUV-TELEVVIL(2)                     
083130       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELEVVIL-ATTR(2)                 
083140     END-IF                                                               
083150                                                                          
083160     IF MID-TELEVVIL(3) NOT = ALL '+'                                     
083170       MOVE MID-TELEVVIL(3)       TO PHUV-TELEVVIL(3)                     
083180       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TELEVVIL-ATTR(3)                 
083190     END-IF                                                               
083191                                                                          
083192     IF MID-TEPACK NOT = ALL '+'                                          
083193       MOVE MID-TEPACK            TO PHUV-TEPACK                          
083194       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEPACK-ATTR                      
083195     END-IF                                                               
083196                                                                          
083197     IF MID-TEFRITT(1) NOT = ALL '+'                                      
083198       MOVE MID-TEFRITT(1)        TO PHUV-TEFRITT(1)                      
083199       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEFRITT-ATTR(1)                  
083200     END-IF                                                               
083300                                                                          
083400     IF MID-TEFRITT(2) NOT = ALL '+'                                      
083500       MOVE MID-TEFRITT(2)        TO PHUV-TEFRITT(2)                      
083600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEFRITT-ATTR(2)                  
083610     END-IF                                                               
083620                                                                          
083630     IF MID-TEFRITT(3) NOT = ALL '+'                                      
083640       MOVE MID-TEFRITT(3)        TO PHUV-TEFRITT(3)                      
083650       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEFRITT-ATTR(3)                  
083660     END-IF                                                               
083670                                                                          
083680     IF MID-TEFRITT(4) NOT = ALL '+'                                      
083690       MOVE MID-TEFRITT(4)        TO PHUV-TEFRITT(4)                      
083691       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEFRITT-ATTR(4)                  
083692     END-IF                                                               
088800                                                                          
088900     MOVE DAGENS-DATUM TO PHUV-TIUPPDAT                                   
089000     MOVE UPPDAT-TID   TO PHUV-TIUPPTID                                   
089100                                                                          
089200     PERFORM IMS-REPL-PROC-WDE801                                         
089300     .                                                                    
089400     EJECT                                                                
091800 E-BEHANDLA-ENTER SECTION.                                                
091900                                                                          
092000     IF MID-IDDISTR-IN  = ALL '+' AND                                     
092100        MID-IDKUNDNR-IN = ALL '+' AND                                     
092200        MID-IDORDNR7-IN = ALL '+' AND                                     
092300        MID-IDARTNR-IN  = ALL '+'                                         
092400                                                                          
092500       IF EGEN-MID AND MID-INPUT NOT = ALL '+'                            
092600                                                                          
092700         MOVE INFO-TRYCK-PF11 TO MED-IDMFSFEL                             
092800         CALL WMEDKONV USING MED-WMEDAREA                                 
092900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
092910         MOVE NEJ TO INDATA-SW                                            
093000                                                                          
093100         PERFORM EA-LAES-IN-IGEN-INDATA                                   
093200                                                                          
093300       ELSE                                                               
093400         PERFORM MFS-RENSA-ALLA-FAELT                                     
093500       END-IF                                                             
093600                                                                          
093700     ELSE                                                                 
093800       PERFORM MFS-RENSA-ALLA-FAELT                                       
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200 EA-LAES-IN-IGEN-INDATA SECTION.                                          
094300                                                                          
094400     IF MID-TEBETVIL(1) NOT = ALL '+'                                     
094500       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBETVIL(1)                      
094600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBETVIL-ATTR(1)                 
094700     END-IF                                                               
094800                                                                          
094810     IF MID-TEBETVIL(2) NOT = ALL '+'                                     
094820       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBETVIL(2)                      
094830       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBETVIL-ATTR(2)                 
094840     END-IF                                                               
094850                                                                          
094860     IF MID-TEBETVIL(3) NOT = ALL '+'                                     
094870       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBETVIL(3)                      
094880       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBETVIL-ATTR(3)                 
094890     END-IF                                                               
094891                                                                          
094892     IF MID-TEBETVIL(4) NOT = ALL '+'                                     
094893       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEBETVIL(4)                      
094894       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEBETVIL-ATTR(4)                 
094895     END-IF                                                               
094896                                                                          
094900     IF MID-TEGILTIG(1) NOT = ALL '+'                                     
095000       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEGILTIG(1)                      
095100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEGILTIG-ATTR(1)                 
095200     END-IF                                                               
095300                                                                          
095310     IF MID-TEGILTIG(2) NOT = ALL '+'                                     
095320       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEGILTIG(2)                      
095330       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEGILTIG-ATTR(2)                 
095340     END-IF                                                               
095350                                                                          
095360     IF MID-TEGILTIG(3) NOT = ALL '+'                                     
095370       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEGILTIG(3)                      
095380       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEGILTIG-ATTR(3)                 
095390     END-IF                                                               
095391                                                                          
095392     IF MID-TEGILTIG(4) NOT = ALL '+'                                     
095393       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEGILTIG(4)                      
095394       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEGILTIG-ATTR(4)                 
095395     END-IF                                                               
095396                                                                          
095400     IF MID-TELEVVIL(1) NOT = ALL '+'                                     
095500       MOVE MFS-ROER-EJ-FAELT     TO MOD-TELEVVIL(1)                      
095600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVVIL-ATTR(1)                 
095700     END-IF                                                               
095800                                                                          
095900     IF MID-TELEVVIL(2) NOT = ALL '+'                                     
096000       MOVE MFS-ROER-EJ-FAELT     TO MOD-TELEVVIL(2)                      
096100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVVIL-ATTR(2)                 
096200     END-IF                                                               
096300                                                                          
096310     IF MID-TELEVVIL(3) NOT = ALL '+'                                     
096320       MOVE MFS-ROER-EJ-FAELT     TO MOD-TELEVVIL(3)                      
096330       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TELEVVIL-ATTR(3)                 
096340     END-IF                                                               
096350                                                                          
096400     IF MID-TEPACK NOT = ALL '+'                                          
096500       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEPACK                           
096600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEPACK-ATTR                      
096700     END-IF                                                               
096800                                                                          
096900     IF MID-TEFRITT(1) NOT = ALL '+'                                      
097000       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEFRITT(1)                       
097100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRITT-ATTR(1)                  
097200     END-IF                                                               
097300                                                                          
097400     IF MID-TEFRITT(2) NOT = ALL '+'                                      
097500       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEFRITT(2)                       
097600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRITT-ATTR(2)                  
097700     END-IF                                                               
097800                                                                          
097810     IF MID-TEFRITT(3) NOT = ALL '+'                                      
097820       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEFRITT(3)                       
097830       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRITT-ATTR(3)                  
097840     END-IF                                                               
097850                                                                          
097860     IF MID-TEFRITT(4) NOT = ALL '+'                                      
097870       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEFRITT(4)                       
097880       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRITT-ATTR(4)                  
097890     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 F-LAES-VISA-BILD SECTION.                                                
101600                                                                          
101700     PERFORM IMS-GHU-PROC-WDE801-PHUV                                     
101800                                                                          
101900     IF INDATA-FEL                                                        
102000       PERFORM FA-FLYTTA-RAETT-FAELT-TILL-MOD                             
102100                                                                          
102200     ELSE                                                                 
102300       PERFORM FB-FLYTTA-ALLA-FAELT-TILL-MOD                              
102400     END-IF                                                               
102500                                                                          
103300     PERFORM FC-KOLLA-OM-UPPDAT-GJORD                                     
103400     .                                                                    
103500     EJECT                                                                
103600 FA-FLYTTA-RAETT-FAELT-TILL-MOD SECTION.                                  
108400                                                                          
108700     IF MID-TEBETVIL(1) = ALL '+'                                         
108800       MOVE PHUV-TEBETVIL(1) TO MOD-TEBETVIL(1)                           
108900     END-IF                                                               
109000                                                                          
109010     IF MID-TEBETVIL(2) = ALL '+'                                         
109020       MOVE PHUV-TEBETVIL(2) TO MOD-TEBETVIL(2)                           
109030     END-IF                                                               
109040                                                                          
109050     IF MID-TEBETVIL(3) = ALL '+'                                         
109060       MOVE PHUV-TEBETVIL(3) TO MOD-TEBETVIL(3)                           
109070     END-IF                                                               
109080                                                                          
109090     IF MID-TEBETVIL(4) = ALL '+'                                         
109091       MOVE PHUV-TEBETVIL(4) TO MOD-TEBETVIL(4)                           
109092     END-IF                                                               
109093                                                                          
109100     IF MID-TEGILTIG(1) = ALL '+'                                         
109200       MOVE PHUV-TEGILTIG(1) TO MOD-TEGILTIG(1)                           
109300     END-IF                                                               
109400                                                                          
109410     IF MID-TEGILTIG(2) = ALL '+'                                         
109420       MOVE PHUV-TEGILTIG(2) TO MOD-TEGILTIG(2)                           
109430     END-IF                                                               
109440                                                                          
109450     IF MID-TEGILTIG(3) = ALL '+'                                         
109460       MOVE PHUV-TEGILTIG(3) TO MOD-TEGILTIG(3)                           
109470     END-IF                                                               
109480                                                                          
109490     IF MID-TEGILTIG(4) = ALL '+'                                         
109491       MOVE PHUV-TEGILTIG(4) TO MOD-TEGILTIG(4)                           
109492     END-IF                                                               
109493                                                                          
109500     IF MID-TELEVVIL(1) = ALL '+'                                         
109600       MOVE PHUV-TELEVVIL(1) TO MOD-TELEVVIL(1)                           
109700     END-IF                                                               
109800                                                                          
109810     IF MID-TELEVVIL(2) = ALL '+'                                         
109820       MOVE PHUV-TELEVVIL(2) TO MOD-TELEVVIL(2)                           
109830     END-IF                                                               
109840                                                                          
109850     IF MID-TELEVVIL(3) = ALL '+'                                         
109860       MOVE PHUV-TELEVVIL(3) TO MOD-TELEVVIL(3)                           
109870     END-IF                                                               
109880                                                                          
109900     IF MID-TEPACK = ALL '+'                                              
110000       MOVE PHUV-TEPACK TO MOD-TEPACK                                     
110100     END-IF                                                               
110200                                                                          
110300     IF MID-TEFRITT(1) = ALL '+'                                          
110400       MOVE PHUV-TEFRITT(1) TO MOD-TEFRITT(1)                             
110500     END-IF                                                               
110600                                                                          
110610     IF MID-TEFRITT(2) = ALL '+'                                          
110620       MOVE PHUV-TEFRITT(2) TO MOD-TEFRITT(2)                             
110630     END-IF                                                               
110640                                                                          
110650     IF MID-TEFRITT(3) = ALL '+'                                          
110660       MOVE PHUV-TEFRITT(3) TO MOD-TEFRITT(3)                             
110670     END-IF                                                               
110680                                                                          
110690     IF MID-TEFRITT(4) = ALL '+'                                          
110691       MOVE PHUV-TEFRITT(4) TO MOD-TEFRITT(4)                             
110692     END-IF                                                               
113400     .                                                                    
113401     EJECT                                                                
113410 FB-FLYTTA-ALLA-FAELT-TILL-MOD SECTION.                                   
113500                                                                          
113600     MOVE PHUV-TEBETVIL(1) TO MOD-TEBETVIL(1)                             
113700     MOVE PHUV-TEBETVIL(2) TO MOD-TEBETVIL(2)                             
113800     MOVE PHUV-TEBETVIL(3) TO MOD-TEBETVIL(3)                             
113900     MOVE PHUV-TEBETVIL(4) TO MOD-TEBETVIL(4)                             
114000     MOVE PHUV-TEGILTIG(1) TO MOD-TEGILTIG(1)                             
114100     MOVE PHUV-TEGILTIG(2) TO MOD-TEGILTIG(2)                             
114200     MOVE PHUV-TEGILTIG(3) TO MOD-TEGILTIG(3)                             
114300     MOVE PHUV-TEGILTIG(4) TO MOD-TEGILTIG(4)                             
114310     MOVE PHUV-TELEVVIL(1) TO MOD-TELEVVIL(1)                             
114320     MOVE PHUV-TELEVVIL(2) TO MOD-TELEVVIL(2)                             
114330     MOVE PHUV-TELEVVIL(3) TO MOD-TELEVVIL(3)                             
114400     MOVE PHUV-TEPACK      TO MOD-TEPACK                                  
114500     MOVE PHUV-TEFRITT(1)  TO MOD-TEFRITT(1)                              
114600     MOVE PHUV-TEFRITT(2)  TO MOD-TEFRITT(2)                              
114700     MOVE PHUV-TEFRITT(3)  TO MOD-TEFRITT(3)                              
114800     MOVE PHUV-TEFRITT(4)  TO MOD-TEFRITT(4)                              
114900     .                                                                    
115000     EJECT                                                                
115100 FC-KOLLA-OM-UPPDAT-GJORD SECTION.                                        
115200                                                                          
115300     IF UPPDATERING-OK                                                    
115400                                                                          
115500       MOVE INFO-UPPDAT-GJORD TO MED-IDMFSINF                             
115600       CALL WMEDKONV USING MED-WMEDAREA                                   
115700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
115800                                                                          
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200 S01-KOLLA-BEHOERIGHET SECTION.                                           
116300                                                                          
116400     IF PHUV-KDPROTYP = 'L'                                               
116500                                                                          
116600       IF PHUV-IDUSER NOT = MSG-SIGNON-USERID                             
116700         MOVE NEJ TO ALLT-SW                                              
116800         MOVE FELM-OBEHOERIG TO MED-IDMFSFEL                              
116900         CALL WMEDKONV USING MED-WMEDAREA                                 
117000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
117100       END-IF                                                             
117200                                                                          
117300     END-IF                                                               
117400     .                                                                    
117500     EJECT                                                                
120500 MFS-RENSA-ALLA-FAELT SECTION.                                            
120600                                                                          
120700*    --- ALLA IN/UTDATA-FÄLT                                              
120800     MOVE MFS-RENSA-FAELT TO MOD-TEBETVIL(1)                              
120900                             MOD-TEBETVIL(2)                              
121000                             MOD-TEBETVIL(3)                              
121100                             MOD-TEBETVIL(4)                              
121200                             MOD-TEGILTIG(1)                              
121300                             MOD-TEGILTIG(2)                              
121400                             MOD-TEGILTIG(3)                              
121500                             MOD-TEGILTIG(4)                              
121600                             MOD-TELEVVIL(1)                              
121700                             MOD-TELEVVIL(2)                              
121800                             MOD-TELEVVIL(3)                              
121900                             MOD-TEPACK                                   
122000                             MOD-TEFRITT(1)                               
122100                             MOD-TEFRITT(2)                               
122200                             MOD-TEFRITT(3)                               
122300                             MOD-TEFRITT(4)                               
122700     .                                                                    
122800     EJECT                                                                
126900* --- IMS SEKTIONER ---                                                   
127000     SKIP3                                                                
127100 IMS-GET-MSG SECTION.                                                     
127200                                                                          
127300     MOVE '  QC' TO GODK-STATUSKODER                                      
127400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
127500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     SKIP3                                                                
127900 IMS-INSERT-MSG SECTION.                                                  
128000                                                                          
128100     IF ENGLISH-TEXT                                                      
128200       MOVE 'N' TO MFS-KDHUVOMR                                           
128300     END-IF                                                               
128400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
128500     MOVE SPACE TO GODK-STATUSKODER                                       
128600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
128700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
128800     PERFORM IMS-STATUSKONTROLL                                           
128900     .                                                                    
129000     EJECT                                                                
129100 IMS-GHU-PROC-WDE801-PHUV SECTION.                                        
129200                                                                          
129300     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
129400          DELIMITED BY SIZE INTO SSA1                                     
129500     MOVE '  GE' TO GODK-STATUSKODER                                      
129600     CALL CBLTDLI USING GHU PROC-PCB DLI-IO-AREA-WDE801 SSA1              
129700     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
129800     PERFORM IMS-STATUSKONTROLL                                           
129900     .                                                                    
130000     SKIP3                                                                
131200 IMS-REPL-PROC-WDE801 SECTION.                                            
131300                                                                          
131400     MOVE '    ' TO GODK-STATUSKODER                                      
131500     CALL CBLTDLI USING REPL PROC-PCB DLI-IO-AREA-WDE801                  
131600     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     EJECT                                                                
132000 IMS-STATUSKONTROLL SECTION.                                              
132100                                                                          
132200     SET STATUS-IX TO 1                                                   
132300     SEARCH GODK-STATUS                                                   
132400       AT END                                                             
132500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
132600         DELIMITED BY SIZE INTO FELTEXT                                   
132700         CALL FELLOG                                                      
132800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
132900     END-SEARCH                                                           
133000     .                                                                    
133010     EJECT                                                                
133100*    -COPY WY2000P1                                                       
