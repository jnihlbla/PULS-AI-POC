000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6021300.                                                
000400*AUTHOR.         ELAINE CURTSSON.                                         
000500*DATE-WRITTEN.   AUGUSTI 92.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SPECIALKONTROLL  (KONTROLLKOD)                                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
001300*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001600*        PROGRAMMET LÄSER      WLPROA (W6G1)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T213                                              
002000*        MID:         W6I21301                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O21301                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W6021300'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  MAX-INDX                    PIC S9(4)  VALUE +5    COMP SYNC.        
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004700 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004800 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
004900 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005000 77  WS-KDKVAINF                 PIC X(1)    VALUE SPACE.                 
005100 77  WS-KDKVATYP                 PIC X(1)    VALUE SPACE.                 
005200 77  WS-IDPROVPL-PRI             PIC X(1)    VALUE SPACE.                 
005300 77  WS-IDPROVPL-SEK             PIC X(1)    VALUE SPACE.                 
005400 77  W-SPAR-FLUPG                PIC X(1)    VALUE SPACE.                 
005500 77  W-SPAR-FLKVASAK             PIC X(1)    VALUE SPACE.                 
005600                                                                          
005700 77  DATUM                       PIC 9(6)    VALUE ZERO.                  
005800                                                                          
005900 77  UPPD-ART-SW                 PIC X       VALUE 'J'.                   
006000     88  UPPD-ART                            VALUE 'N'.                   
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  KKOD-SW                     PIC X       VALUE 'J'.                   
006700     88  KKOD-OK                             VALUE 'J'.                   
006800     88  KKOD-FEL                            VALUE 'N'.                   
006900                                                                          
007000 77  IDKVAFEL-SW                 PIC X       VALUE 'J'.                   
007100     88  IDKVAFEL-OK                         VALUE 'J'.                   
007200     88  IDKVAFEL-FEL                        VALUE 'N'.                   
007300                                                                          
007400 77  IDPROVPL-SW                 PIC X       VALUE 'J'.                   
007500     88  IDPROVPL-OK                         VALUE 'J'.                   
007600     88  IDPROVPL-FEL                        VALUE 'N'.                   
007700                                                                          
007800 77  ARTNR-UPPDAT-SW             PIC X       VALUE 'J'.                   
007900     88  ARTNR-UPPDAT                        VALUE 'J'.                   
008000     88  ARTNR-EJ-UPPDAT                     VALUE 'N'.                   
008100                                                                          
008200 77  W6KVAH22-SW                 PIC X       VALUE 'J'.                   
008300     88  W6KVAH22-FINNS                      VALUE 'J'.                   
008400     88  W6KVAH22-SAKNAS                     VALUE 'N'.                   
008500                                                                          
008600 77  W6KVAH01-SW                 PIC X       VALUE 'J'.                   
008700     88  W6KVAH01-FINNS                      VALUE 'J'.                   
008800     88  W6KVAH01-SAKNAS                     VALUE 'N'.                   
008900                                                                          
009000 77  W6KVAH11-SW                 PIC X       VALUE 'J'.                   
009100     88  W6KVAH11-FINNS                      VALUE 'J'.                   
009200     88  W6KVAH11-SAKNAS                     VALUE 'N'.                   
009300                                                                          
009400 77  W6KVAH12-SW                 PIC X       VALUE 'J'.                   
009500     88  W6KVAH12-FINNS                      VALUE 'J'.                   
009600     88  W6KVAH12-SAKNAS                     VALUE 'N'.                   
009700                                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000     88  NYCKLAR-FEL                         VALUE 'N'.                   
010100                                                                          
010200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010300     88  EGEN-MID                            VALUE '6213'.                
010400     88  GODK-MID                            VALUE '6211' '6212'          
010500                                                   '6213' '6214'          
010600                                                   '6215' '6216'          
010700                                                   '6217' '6218'          
010800                                                   '6219'.                
010900     88  HELP-MID                            VALUE '0551'.                
011000     EJECT                                                                
011100 01  -COPY WWPRODSL                                                       
011200     EJECT                                                                
011300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011400 01  GENERELLA-SUBPROGRAM.                                                
011500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012200*01 -COPY WMSGINIT                                                        
012300     EJECT                                                                
012400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012500*01 -COPY WMEDAREA                                                        
012600     SKIP3                                                                
012700 01  MESSAGE-CODES.                                                       
012800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013500     03  ERR-WRONG-PARTNR        PIC X(3)    VALUE '769'.                 
013600     03  ERR-LEV-SAKNAS          PIC X(3)    VALUE '092'.                 
013700     03  INF-UPPDAT-OTILLATET    PIC X(3)    VALUE '777'.                 
013800     03  INF-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
013900     03  INF-ARTIKEL-HIST-SAKNAS PIC X(3)    VALUE '214'.                 
014000     03  INF-PROVPLAN-SAKNAS     PIC X(3)    VALUE '221'.                 
014100     03  INSPECT-CODE-MISSING    PIC X(3)    VALUE '234'.                 
014200     EJECT                                                                
014300*01  -COPY WDECAREA                                                       
014400     EJECT                                                                
014500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014600*                                                                         
014700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014800     SKIP3                                                                
014900*01  MID -COPY W6I21301                                                   
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015200     SKIP3                                                                
015300*01  -COPY WMSGAREA                                                       
015400     EJECT                                                                
015500     03  MOD REDEFINES MSG-AREA.                                          
015600*      05  -COPY W6O21301                                                 
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015900     SKIP3                                                                
016000*01  -COPY WMFSAREA                                                       
016100     EJECT                                                                
016200 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
016300*01  FILLER -COPY WWBYT03 -RED TEST-IDARTNR                               
016400     EJECT                                                                
016500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016900     SKIP3                                                                
017000 01  NYCKLAR-TILL-DLI.                                                    
017100     03  W-IDARTNR-X.                                                     
017200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017300     03  W-KDCLAGER-X.                                                    
017400         05  W-KDCLAGER          PIC S9(1)   VALUE +1   COMP-3.           
017500     03  W-IDLEVNR-X.                                                     
017600         05  W-IDLEVNR           PIC  X(5)  VALUE SPACE.                  
017700     03  W-IDKVAINF-X.                                                    
017800         05  W-IDKVAINF          PIC 9(2)   VALUE ZERO.                   
017900     03  W-IDSKYLT-X.                                                     
018000         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
018100     03  W-W6GX-6101-KEY-X.                                               
018200         05 W-IDHTYP-6101        PIC X(04)  VALUE '6101'.                 
018300         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
018400     03  W-W6GX-6102-KEY-X.                                               
018500         05 W-IDPROVPL           PIC 9(1).                                
018600         05 W-KDPROVPL           PIC X(1).                                
018700     SKIP2                                                                
018800*    --- STATUS-KOD FRÅN IMS                                              
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FINNS                       VALUE '  '.                  
019100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019300     SKIP2                                                                
019400 01  GODK-STATUSKODER.                                                    
019500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019600     SKIP3                                                                
019700 01  SSA1                        PIC X(64).                               
019800 01  SSA2                        PIC X(64).                               
019900 01  SSA3                        PIC X(64).                               
020000     EJECT                                                                
020100*    --- IMS FUNKTIONSKODER                                               
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400*    ---  DLI INPUT-OUTPUT AREA                                           
020500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020600     SKIP3                                                                
020700 01  DLI-IO-AREA1.                                                        
020800     03  IO-AREA1                PIC X(1154)  VALUE SPACE.                
020900     SKIP3                                                                
021000     03  W6KVAH01 REDEFINES IO-AREA1.                                     
021100*        05  -COPY W6D201                                                 
021200     SKIP3                                                                
021300     03  W6KVAH11 REDEFINES IO-AREA1.                                     
021400*        05  -COPY W6D211                                                 
021500     SKIP3                                                                
021600     03  W6KVAH12 REDEFINES IO-AREA1.                                     
021700*        05  -COPY W6D212                                                 
021800     SKIP3                                                                
021900     03  W6KVAH22 REDEFINES IO-AREA1.                                     
022000*        05  -COPY W6D222                                                 
022100     SKIP3                                                                
022200 01  DLI-IO-AREA2.                                                        
022300     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
022400     03  WLLEVA01 REDEFINES IO-AREA2.                                     
022500*        05  -COPY WDF101  -PRE LEVA-                                     
022600     SKIP3                                                                
022700     03  WLLEVA14 REDEFINES IO-AREA2.                                     
022800*        05  -COPY WDF106  -PRE LEVA-                                     
022900     SKIP3                                                                
023000 01  DLI-IO-AREA3.                                                        
023100     03  IO-AREA3                PIC X(110) VALUE SPACE.                  
023200     03  WLARTC01 REDEFINES IO-AREA3.                                     
023300*        05  -COPY WDK601  -PRE ARTC-                                     
023400     SKIP3                                                                
023500 01  DLI-IO-AREA4.                                                        
023600     03  WLBENA11.                                                        
023700*        05  -COPY WDD311                                                 
023800     SKIP3                                                                
023900 01  DLI-IO-AREA5.                                                        
024000     03  IO-AREA5                PIC X(150)  VALUE SPACE.                 
024100     03  W6PROA01 REDEFINES IO-AREA5.                                     
024200*        05  -COPY W6GX01                                                 
024300     SKIP3                                                                
024400     03  W6PROA11 REDEFINES IO-AREA5.                                     
024500*        05  -COPY W6GX6102                                               
024600     EJECT                                                                
024700 01  DLI-IO-AREA6.                                                        
024800     03  IO-AREA6                PIC X(300)  VALUE SPACE.                 
024900     03  W6LEVA01 REDEFINES IO-AREA6.                                     
025000*        05  -COPY W6F101   -PRE W6LEVA-                                  
025100     EJECT                                                                
025200 LINKAGE SECTION.                                                         
025300                                                                          
025400*01  -COPY W0009   -PRE MSG-                                              
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE USEA-                                              
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008  -PRE KVAH-                                              
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE LEVA-                                              
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500*01  -COPY W0008  -PRE ARTC-                                              
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
026800*01  -COPY W0008  -PRE BENA-                                              
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01  -COPY W0008  -PRE PROA-                                              
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01  -COPY W0008  -PRE W6F1-                                              
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
027800                                   KVAH-PCB LEVA-PCB                      
027900     ARTC-PCB BENA-PCB PROA-PCB W6F1-PCB.                                 
028000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
028100                                   KVAH-PCB LEVA-PCB                      
028200     ARTC-PCB BENA-PCB PROA-PCB W6F1-PCB.                                 
028300                                                                          
028400     PERFORM IMS-GET-MSG                                                  
028500     IF SEGMENT-FINNS                                                     
028600       PERFORM A-INIT                                                     
028700       PERFORM B-KOLLA-NYCKLAR                                            
028800       IF NYCKLAR-OK                                                      
028900         IF MFS-UPDATE                                                    
029000           PERFORM G-KOLLA-INPUT                                          
029100           IF INDATA-OK                                                   
029200              PERFORM H-UPPDATERA                                         
029300              PERFORM F-LAES-VISA-INFO                                    
029400           END-IF                                                         
029500         ELSE                                                             
029600           IF MFS-FIRST                                                   
029700             PERFORM C-FOERSTA-SIDA                                       
029800           ELSE                                                           
029900             IF MFS-NEXT                                                  
030000               PERFORM D-NAESTA-SIDA                                      
030100             ELSE                                                         
030200               PERFORM E-SAMMA-SIDA                                       
030300             END-IF                                                       
030400           END-IF                                                         
030500           IF INDATA-OK                                                   
030600              PERFORM F-LAES-VISA-INFO                                    
030700           END-IF                                                         
030800         END-IF                                                           
030900       ELSE                                                               
031000           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                           
031100           CALL WMEDKONV USING MED-WMEDAREA                               
031200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
031300                                                                          
031400       END-IF                                                             
031500       MOVE LENGTH OF MOD-W6O21301 TO MSG-KVLL                            
031600       ADD                +4       TO MSG-KVLL                            
031700       PERFORM IMS-INSERT-MSG                                             
031800     END-IF                                                               
031900                                                                          
032000     MOVE ZERO TO RETURN-CODE                                             
032100     GOBACK                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 A-INIT SECTION.                                                          
032500                                                                          
032600     IF MSG-DUBBLA-TRANSKODER                                             
032700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21301                 
032800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033000     ELSE                                                                 
033100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21301                  
033200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
033300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033400     END-IF                                                               
033500                                                                          
033600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033900                                                                          
034000     MOVE LOW-VALUE TO MSG-AREA                                           
034100     MOVE 'W6O213N1' TO MFS-IDMOD                                         
034200     MOVE '6213' TO MOD-IDTRANS                                           
034300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034400                                                                          
034500     IF EGEN-MID OR HELP-MID                                              
034600       CONTINUE                                                           
034700     ELSE                                                                 
034800       MOVE SPACE TO MFS-KDTRTYP                                          
034900       MOVE '7' TO MFS-IDPFK                                              
035000     END-IF                                                               
035100                                                                          
035200     ACCEPT DATUM FROM DATE                                               
035300     PERFORM MFS-FORM-ATTR                                                
035400                                                                          
035500     .                                                                    
035600     EJECT                                                                
035700 B-KOLLA-NYCKLAR SECTION.                                                 
035800                                                                          
035900     IF GODK-MID OR HELP-MID                                              
036000        CONTINUE                                                          
036100     ELSE                                                                 
036200        MOVE ALL '+'        TO MID-IDLEVNR-IN                             
036300                               MID-IDKVAINFI                              
036400                               MID-TIREGDAT-IN                            
036500                               MID-KDKVAINF-IN                            
036600        MOVE SPACE          TO MID-IDLEVNR-UT                             
036700                               MID-IDKVAINFU                              
036800                               MID-TIREGDAT-UT                            
036900                               MID-KDKVAINF-UT                            
037000     END-IF                                                               
037100                                                                          
037200     MOVE JA TO NYCKLAR-SW                                                
037300                                                                          
037400*    -- KONTROLL AV IDARTNR                                               
037500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037600                                                                          
037700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
037800     MOVE '001'             TO MSGI-KDCALL                                
037900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
038000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
038100     MOVE '6213'                 TO MSGI-IDTRANS                          
038200                                                                          
038300     IF MFS-IDTRANS = '6213'                                              
038400         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
038500     ELSE                                                                 
038600       MOVE SPACE          TO MID-IDLEVNR-IN                              
038700                              MOD-IDLEVNR-UT                              
038800**                                                                        
038900       IF MID-IDARTNR-IN NUMERIC                                          
039000       AND MID-IDARTNR-IN > ZERO                                          
039100         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
039200       END-IF                                                             
039300     END-IF                                                               
039400                                                                          
039500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039600                                                                          
039700     IF MSGI-IDLAND-SPR = 'GB'                                            
039800       MOVE +2 TO SPRAK-IX                                                
039900       MOVE 'GB ' TO MED-IDSKYLT                                          
040000                     W-IDSKYLT                                            
040100     ELSE                                                                 
040200       MOVE +1 TO SPRAK-IX                                                
040300       MOVE 'S  ' TO MED-IDSKYLT                                          
040400                     W-IDSKYLT                                            
040500     END-IF                                                               
040600                                                                          
040700     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
040800     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
040900                                                                          
041000     IF MID-IDARTNR-IN = ALL '+'                                          
041100       CONTINUE                                                           
041200     ELSE                                                                 
041300       MOVE '7'         TO MFS-IDPFK                                      
041400       MOVE SPACE       TO MFS-KDTRTYP                                    
041500     END-IF                                                               
041600     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
041700       MOVE WS-IDARTNR TO W-IDARTNR                                       
041800     ELSE                                                                 
041900       MOVE NEJ TO NYCKLAR-SW                                             
042000     END-IF                                                               
042100                                                                          
042200*    -- KONTROLL AV IDLEVNR                                               
042300     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
042400                                                                          
042500     IF MID-IDLEVNR-IN = ALL '+'                                          
042600       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
042700     ELSE                                                                 
042800       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
042900       MOVE '7'         TO MFS-IDPFK                                      
043000       MOVE SPACE       TO MFS-KDTRTYP                                    
043100     END-IF                                                               
043200                                                                          
043300     MOVE WS-IDLEVNR TO W-IDLEVNR                                         
043400     IF W-IDLEVNR NOT = SPACE                                             
043500       CONTINUE                                                           
043600     ELSE                                                                 
043700       MOVE NEJ TO NYCKLAR-SW                                             
043800     END-IF                                                               
043900                                                                          
044000*    -- KONTROLL AV IDKVAINF                                              
044100     MOVE MFS-RENSA-FAELT   TO MOD-IDKVAINFI                              
044200                                                                          
044300     IF MID-IDKVAINFI   = ALL '+'                                         
044400       MOVE MID-IDKVAINFU   TO WS-IDKVAINF                                
044500       INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                
044600     ELSE                                                                 
044700       MOVE MID-IDKVAINFI   TO WS-IDKVAINF                                
044800     END-IF                                                               
044900                                                                          
045000     IF WS-IDKVAINF NUMERIC                                               
045100       MOVE WS-IDKVAINF TO W-IDKVAINF                                     
045200     ELSE                                                                 
045300       MOVE NEJ TO NYCKLAR-SW                                             
045400     END-IF                                                               
045500                                                                          
045600*    -- KONTROLL AV TIREGDAT                                              
045700     MOVE MFS-RENSA-FAELT   TO MOD-TIREGDAT-IN                            
045800                                                                          
045900     IF MID-TIREGDAT-IN = ALL '+'                                         
046000       MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                                
046100       INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                
046200     ELSE                                                                 
046300       MOVE MID-TIREGDAT-IN TO WS-TIREGDAT                                
046400     END-IF                                                               
046500                                                                          
046600*    -- KONTROLL AV KDKVAINF                                              
046700     MOVE MFS-RENSA-FAELT   TO MOD-KDKVAINF-IN                            
046800                                                                          
046900     IF MID-KDKVAINF-IN = ALL '+'                                         
047000       MOVE MID-KDKVAINF-UT TO WS-KDKVAINF                                
047100     ELSE                                                                 
047200       MOVE MID-KDKVAINF-IN TO WS-KDKVAINF                                
047300     END-IF                                                               
047400                                                                          
047500     IF GODK-MID OR NYCKLAR-OK                                            
047600       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
047700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
047800       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
047900       MOVE WS-IDKVAINF TO MOD-IDKVAINFU                                  
048000       INSPECT MOD-IDKVAINFU   REPLACING LEADING ZERO BY SPACE            
048100       MOVE WS-TIREGDAT TO MOD-TIREGDAT-UT                                
048200       INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE            
048300       MOVE WS-KDKVAINF TO MOD-KDKVAINF-UT                                
048400     ELSE                                                                 
048500       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
048600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048700*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
048800       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                             
048900                               MOD-IDKVAINFU                              
049000                               MOD-TIREGDAT-UT                            
049100                               MOD-KDKVAINF-UT                            
049200     END-IF                                                               
049300                                                                          
049400     .                                                                    
049500     EJECT                                                                
049600 C-FOERSTA-SIDA SECTION.                                                  
049700                                                                          
049800     MOVE INF-FIRST-PAGE    TO MED-IDMFSINF                               
049900     CALL WMEDKONV       USING MED-WMEDAREA                               
050000     MOVE MED-MFSINF        TO MOD-TEMFSINF                               
050100                                                                          
050200     MOVE ZERO              TO MOD-IDKVAINF-ENTER                         
050300                               MOD-IDKVAINF-NEXT                          
050400                                                                          
050500     PERFORM MFS-RENSA-FAELT-IN                                           
050600     .                                                                    
050700     EJECT                                                                
050800 D-NAESTA-SIDA SECTION.                                                   
050900                                                                          
051000     MOVE MID-IDKVAINF-NEXT TO W-IDKVAINF                                 
051100                                                                          
051200     PERFORM MFS-RENSA-FAELT-IN                                           
051300     .                                                                    
051400     EJECT                                                                
051500 E-SAMMA-SIDA SECTION.                                                    
051600                                                                          
051700     IF EGEN-MID OR HELP-MID                                              
051800       IF MID-IDKVAINFI NOT = ALL '+'                                     
051900          MOVE MID-IDKVAINFI   TO MID-IDKVAINF-ENTER                      
052000       END-IF                                                             
052100       MOVE MID-IDKVAINF-ENTER TO W-IDKVAINF                              
052200       IF MID-INPUT = ALL '+'                                             
052300         PERFORM MFS-RENSA-FAELT-IN                                       
052400       ELSE                                                               
052500         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
052600         CALL WMEDKONV    USING MED-WMEDAREA                              
052700         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
052800         PERFORM EA-MID-INDATA-TILL-MOD                                   
052900       END-IF                                                             
053000     ELSE                                                                 
053100       PERFORM MFS-RENSA-FAELT-IN                                         
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 EA-MID-INDATA-TILL-MOD SECTION.                                          
053600                                                                          
053700     IF MID-FLKVASAK NOT = ALL '+'                                        
053800        MOVE MID-FLKVASAK          TO MOD-FLKVASAK                        
053900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKVASAK-ATTR                   
054000     ELSE                                                                 
054100        MOVE MFS-RENSA-FAELT       TO MOD-FLKVASAK                        
054200     END-IF                                                               
054300                                                                          
054400     IF MID-FLUPG NOT = ALL '+'                                           
054500        MOVE MID-FLUPG             TO MOD-FLUPG                           
054600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLUPG-ATTR                      
054700     ELSE                                                                 
054800        MOVE MFS-RENSA-FAELT       TO MOD-FLUPG                           
054900     END-IF                                                               
055000                                                                          
055100     IF MID-KDCMD-LEV NOT = ALL '+'                                       
055200        MOVE MID-KDCMD-LEV         TO MOD-KDCMD-LEV                       
055300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-LEV-ATTR                  
055400     ELSE                                                                 
055500        MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-LEV                       
055600     END-IF                                                               
055700                                                                          
055800     IF MID-KDCMD-SPEC NOT = ALL '+'                                      
055900        MOVE MID-KDCMD-SPEC        TO MOD-KDCMD-SPEC                      
056000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-SPEC-ATTR                 
056100     ELSE                                                                 
056200        MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-SPEC                      
056300     END-IF                                                               
056400                                                                          
056500     IF MID-IDKVAINF-IN NOT = ALL '+'                                     
056600        MOVE MID-IDKVAINF-IN       TO MOD-IDKVAINF-IN                     
056700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKVAINF-IN-ATTR                
056800     ELSE                                                                 
056900        MOVE MFS-RENSA-FAELT       TO MOD-IDKVAINF-IN                     
057000     END-IF                                                               
057100                                                                          
057200     IF MID-IDPROVPL-PRI-IN NOT = ALL '+'                                 
057300        MOVE MID-IDPROVPL-PRI-IN   TO MOD-IDPROVPL-PRI-IN                 
057400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROVPL-PRI-IN-ATTR            
057500     ELSE                                                                 
057600        MOVE MFS-RENSA-FAELT       TO MOD-IDPROVPL-PRI-IN                 
057700     END-IF                                                               
057800                                                                          
057900     IF MID-KVSKPLOT-PRI-IN NOT = ALL '+'                                 
058000        MOVE MID-KVSKPLOT-PRI-IN   TO MOD-KVSKPLOT-PRI-IN                 
058100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVSKPLOT-PRI-IN-ATTR            
058200     ELSE                                                                 
058300        MOVE MFS-RENSA-FAELT       TO MOD-KVSKPLOT-PRI-IN                 
058400     END-IF                                                               
058500                                                                          
058600     IF MID-BEANST-IN NOT = ALL '+'                                       
058700        MOVE MID-BEANST-IN         TO MOD-BEANST-IN                       
058800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEANST-IN-ATTR                  
058900     ELSE                                                                 
059000        MOVE MFS-RENSA-FAELT       TO MOD-BEANST-IN                       
059100     END-IF                                                               
059200                                                                          
059300     IF MID-IDTFN-IN NOT = ALL '+'                                        
059400        MOVE MID-IDTFN-IN          TO MOD-IDTFN-IN                        
059500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTFN-IN-ATTR                   
059600     ELSE                                                                 
059700        MOVE MFS-RENSA-FAELT       TO MOD-IDTFN-IN                        
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 F-LAES-VISA-INFO SECTION.                                                
060200                                                                          
060300     PERFORM FA-LAES-GRUNDDATA                                            
060400                                                                          
060500     IF SEGMENT-SAKNAS                                                    
060600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
060700        CALL WMEDKONV USING MED-WMEDAREA                                  
060800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
060900        PERFORM MFS-RENSA-FAELT-UT                                        
061000     ELSE                                                                 
061100       IF W6KVAH01-FINNS AND W6KVAH12-FINNS                               
061200         MOVE +1 TO INDX                                                  
061300         PERFORM IMS-GU-W6KVAH01                                          
061400         PERFORM IMS-GNP-W6KVAH22-NEXT                                    
061500         IF SEGMENT-FINNS                                                 
061600           MOVE SPEC-IDKVAINF TO MOD-IDKVAINF-ENTER                       
061700         ELSE                                                             
061800           MOVE ZERO          TO MOD-IDKVAINF-ENTER                       
061900         END-IF                                                           
062000                                                                          
062100         PERFORM UNTIL INDX > MAX-INDX                                    
062200           IF SEGMENT-FINNS                                               
062300             MOVE SPEC-IDKVAINF     TO MOD-IDKVAINF     (INDX)            
062400             MOVE SPEC-BEANST       TO MOD-BEANST       (INDX)            
062500             MOVE SPEC-IDPROVPL-PRI TO MOD-IDPROVPL-PRI (INDX)            
062600             MOVE SPEC-IDTFN        TO MOD-IDTFN        (INDX)            
062700             MOVE SPEC-KVSKPLOT-PRI TO MOD-KVSKPLOT-PRI (INDX)            
062800             PERFORM IMS-GNP-W6KVAH22-NEXT                                
062900           ELSE                                                           
063000             MOVE MFS-RENSA-FAELT TO MOD-IDKVAINF      (INDX)             
063100                                     MOD-BEANST        (INDX)             
063200                                     MOD-IDPROVPL-PRI  (INDX)             
063300                                     MOD-IDTFN         (INDX)             
063400                                     MOD-KVSKPLOT-PRI  (INDX)             
063500           END-IF                                                         
063600           ADD 1 TO INDX                                                  
063700         END-PERFORM                                                      
063800                                                                          
063900         IF SEGMENT-FINNS                                                 
064000           MOVE SPEC-IDKVAINF TO MOD-IDKVAINF-NEXT                        
064100           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
064200           CALL WMEDKONV USING MED-WMEDAREA                               
064300           MOVE MED-MFSINF   TO MOD-TEMFSINF                              
064400         ELSE                                                             
064500           MOVE MOD-IDKVAINF-ENTER   TO MOD-IDKVAINF-NEXT                 
064600         END-IF                                                           
064700       ELSE                                                               
064800         IF W6KVAH01-SAKNAS                                               
064900           MOVE INF-ARTIKEL-SAKNAS   TO MED-IDMFSINF                      
065000           CALL WMEDKONV USING MED-WMEDAREA                               
065100           MOVE MED-MFSINF   TO MOD-TEMFSINF                              
065200         END-IF                                                           
065300       END-IF                                                             
065400     END-IF                                                               
065500                                                                          
065600     .                                                                    
065700     EJECT                                                                
065800 FA-LAES-GRUNDDATA SECTION.                                               
065900                                                                          
066000     PERFORM IMS-GU-W6KVAH01                                              
066100     IF SEGMENT-FINNS                                                     
066200        MOVE JA TO W6KVAH01-SW                                            
066300        IF W-IDLEVNR = SPACE                                              
066400           PERFORM IMS-GNP-OKVAL-W6KVAH12                                 
066500           IF SEGMENT-FINNS                                               
066600              MOVE LEV-IDLEVNR TO MOD-IDLEVNR-UT                          
066700                                  W-IDLEVNR                               
066800              PERFORM FAA-FLYTTA-LEV-TILL-MOD                             
066900              MOVE JA TO W6KVAH12-SW                                      
067000           ELSE                                                           
067100              MOVE 1 TO INDX                                              
067200              PERFORM UNTIL INDX > MAX-INDX                               
067300                  PERFORM MFS-RENSA-RAD-FAELT-UT                          
067400                  ADD 1 TO INDX                                           
067500              END-PERFORM                                                 
067600              MOVE NEJ TO W6KVAH12-SW                                     
067700              PERFORM IMS-GU-WLARTC01                                     
067800              IF SEGMENT-FINNS                                            
067900                 MOVE ARTC-ART-IDLEVNR TO MOD-IDLEVNR-UT                  
068000                                      W-IDLEVNR                           
068100              END-IF                                                      
068200           END-IF                                                         
068300        ELSE                                                              
068400           PERFORM IMS-GNP-W6KVAH12                                       
068500           IF SEGMENT-FINNS                                               
068600              PERFORM FAA-FLYTTA-LEV-TILL-MOD                             
068700              MOVE JA TO W6KVAH12-SW                                      
068800           ELSE                                                           
068900              MOVE 1 TO INDX                                              
069000              PERFORM UNTIL INDX > MAX-INDX                               
069100                  PERFORM MFS-RENSA-RAD-FAELT-UT                          
069200                  ADD 1 TO INDX                                           
069300              END-PERFORM                                                 
069400              MOVE NEJ TO W6KVAH12-SW                                     
069500              PERFORM IMS-GU-WLLEVA01                                     
069600              IF SEGMENT-SAKNAS                                           
069700                 MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                       
069800                 CALL WMEDKONV USING MED-WMEDAREA                         
069900                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
070000                 PERFORM MFS-RENSA-FAELT-IN                               
070100                 PERFORM MFS-RENSA-FAELT-UT                               
070200              END-IF                                                      
070300           END-IF                                                         
070400        END-IF                                                            
070500     ELSE                                                                 
070600        MOVE NEJ TO W6KVAH01-SW                                           
070700        PERFORM IMS-GU-WLARTC01                                           
070800        IF SEGMENT-FINNS                                                  
070900           MOVE ARTC-ART-IDLEVNR TO MOD-IDLEVNR-UT                        
071000                                W-IDLEVNR                                 
071100        END-IF                                                            
071200     END-IF                                                               
071300     PERFORM FAB-HAEMTA-BELEV-OCH-BEART                                   
071400                                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 FAA-FLYTTA-LEV-TILL-MOD   SECTION.                                       
071800                                                                          
071900     MOVE LEV-FLKVASAK     TO MOD-FLKVASAK                                
072000     MOVE LEV-TIKVASAK     TO MOD-TIKVASAK                                
072100     MOVE LEV-FLUPG        TO MOD-FLUPG                                   
072200     MOVE LEV-TIUPG        TO MOD-TIUPG                                   
072300                                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 FAB-HAEMTA-BELEV-OCH-BEART SECTION.                                      
072700                                                                          
072800     PERFORM IMS-GU-WLLEVA01                                              
072900     IF SEGMENT-FINNS                                                     
073000        PERFORM IMS-GNP-WLLEVA14                                          
073100        IF SEGMENT-FINNS                                                  
073200           MOVE LEVA-ADR-BELEV TO MOD-BELEV                               
073300        END-IF                                                            
073400     END-IF                                                               
073500     PERFORM IMS-GU-WLBENA11                                              
073600     IF SEGMENT-FINNS                                                     
073700        MOVE TEXT-BEART   TO MOD-BEART                                    
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 G-KOLLA-INPUT SECTION.                                                   
074200                                                                          
074300     IF MID-INPUT = ALL '+'                                               
074400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
074500       CALL WMEDKONV USING MED-WMEDAREA                                   
074600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
074700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
074800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
074900       MOVE NEJ TO INDATA-SW                                              
075000     ELSE                                                                 
075100                                                                          
075200       MOVE W-IDARTNR   TO TEST-IDARTNR                                   
075300       IF BYT03-OBJEKT                                                    
075400          MOVE NEJ TO INDATA-SW                                           
075500       ELSE                                                               
075600          PERFORM IMS-GU-WLARTC01                                         
075700          IF SEGMENT-FINNS                                                
075800             MOVE ARTC-ART-KDPRODSL    TO TEST-KDPRODSL                   
075900             IF KDPRODSL-BIMA                                             
076000                MOVE JA  TO UPPD-ART-SW                                   
076100                MOVE NEJ TO INDATA-SW                                     
076200             END-IF                                                       
076300          END-IF                                                          
076400       END-IF                                                             
076500       PERFORM IMS-GU-W6KVAH01                                            
076600       IF SEGMENT-FINNS                                                   
076700          MOVE ART-KDKVATYP      TO WS-KDKVATYP                           
076800          MOVE ART-IDPROVPL-PRI  TO WS-IDPROVPL-PRI                       
076900          MOVE ART-IDPROVPL-SEK  TO WS-IDPROVPL-SEK                       
077000          PERFORM IMS-GNP-W6KVAH12                                        
077100          IF SEGMENT-FINNS                                                
077200             MOVE LEV-FLUPG            TO W-SPAR-FLUPG                    
077300             MOVE LEV-FLKVASAK         TO W-SPAR-FLKVASAK                 
077400          ELSE                                                            
077500             PERFORM IMS-GU-WLLEVA01                                      
077600             IF SEGMENT-SAKNAS                                            
077700                MOVE MFS-ALFA-FAELT-FEL TO MID-IDLEVNR-IN                 
077800                MOVE NEJ TO INDATA-SW                                     
077900             ELSE                                                         
078000                 IF WS-IDPROVPL-PRI > ZERO                                
078100                    MOVE WS-IDPROVPL-PRI  TO W-IDPROVPL                   
078200                    MOVE 'N'              TO W-KDPROVPL                   
078300                    PERFORM IMS-GU-W6PROA11                               
078400                    IF SEGMENT-SAKNAS                                     
078500                       MOVE NEJ TO IDPROVPL-SW                            
078600                       MOVE NEJ TO INDATA-SW                              
078700                    END-IF                                                
078800                 END-IF                                                   
078900                                                                          
079000                 IF WS-IDPROVPL-SEK > ZERO                                
079100                    MOVE WS-IDPROVPL-SEK  TO W-IDPROVPL                   
079200                    MOVE 'N'              TO W-KDPROVPL                   
079300                    PERFORM IMS-GU-W6PROA11                               
079400                    IF SEGMENT-SAKNAS                                     
079500                       MOVE NEJ TO IDPROVPL-SW                            
079600                       MOVE NEJ TO INDATA-SW                              
079700                    END-IF                                                
079800                 END-IF                                                   
079900             END-IF                                                       
080000          END-IF                                                          
080100         IF MID-FLKVASAK NOT = ALL '+'                                    
080200            MOVE MID-FLKVASAK           TO W-SPAR-FLKVASAK                
080300            IF MID-FLKVASAK = JA OR NEJ                                   
080400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKVASAK-ATTR             
080500            ELSE                                                          
080600             IF MID-FLKVASAK = NEJ                                        
080700               IF (WS-KDKVATYP > 0) AND                                   
080800                  (WS-IDPROVPL-PRI > 0 OR                                 
080900                   WS-IDPROVPL-SEK > 0)                                   
081000                 MOVE MFS-ALFA-FAELT-RAETT TO                             
081100                                     MOD-FLKVASAK-ATTR                    
081200               ELSE                                                       
081300                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVASAK-ATTR             
081400                 MOVE NEJ TO INDATA-SW                                    
081500                             KKOD-SW                                      
081600               END-IF                                                     
081700             ELSE                                                         
081800               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVASAK-ATTR               
081900               MOVE NEJ TO INDATA-SW                                      
082000             END-IF                                                       
082100            END-IF                                                        
082200         END-IF                                                           
082300                                                                          
082400         IF MID-FLUPG NOT = ALL '+'                                       
082500            MOVE MID-FLUPG             TO W-SPAR-FLUPG                    
082600            IF MID-FLUPG = JA                                             
082700               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLUPG-ATTR                
082800            ELSE                                                          
082900             IF MID-FLUPG = NEJ                                           
083000               IF (WS-KDKVATYP > 0) AND                                   
083100                  (WS-IDPROVPL-PRI > 0 OR                                 
083200                   WS-IDPROVPL-SEK > 0)                                   
083300                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLUPG-ATTR              
083400               ELSE                                                       
083500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLUPG-ATTR                
083600                 MOVE NEJ TO INDATA-SW                                    
083700                             KKOD-SW                                      
083800               END-IF                                                     
083900             ELSE                                                         
084000               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLUPG-ATTR                  
084100               MOVE NEJ TO INDATA-SW                                      
084200             END-IF                                                       
084300            END-IF                                                        
084400         END-IF                                                           
084500                                                                          
084600         IF  W-SPAR-FLUPG    = NEJ                                        
084700         AND W-SPAR-FLKVASAK = JA                                         
084800             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLUPG-ATTR                    
084900             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKVASAK-ATTR                 
085000             MOVE NEJ TO INDATA-SW                                        
085100         END-IF                                                           
085200                                                                          
085300         IF MID-KDCMD-LEV NOT = ALL '+'                                   
085400            IF MID-KDCMD-LEV = 'B' OR 'D' OR SPACE                        
085500               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-LEV-ATTR            
085600            ELSE                                                          
085700               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-LEV-ATTR            
085800               MOVE NEJ TO INDATA-SW                                      
085900            END-IF                                                        
086000         END-IF                                                           
086100                                                                          
086200         IF MID-KDCMD-LEV = 'B' OR 'D'                                    
086300            PERFORM IMS-GU-W6KVAH01                                       
086400            IF SEGMENT-FINNS                                              
086500               PERFORM IMS-GNP-W6KVAH12                                   
086600               IF SEGMENT-FINNS                                           
086700                  PERFORM IMS-GNP-OKVAL-W6KVAH22                          
086800                  IF SEGMENT-FINNS                                        
086900                     MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-LEV-ATTR        
087000                     MOVE NEJ TO INDATA-SW                                
087100                     MOVE NEJ TO IDKVAFEL-SW                              
087200                  END-IF                                                  
087300               ELSE                                                       
087400                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-LEV-ATTR           
087500                  MOVE NEJ TO INDATA-SW                                   
087600               END-IF                                                     
087700            ELSE                                                          
087800               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-LEV-ATTR              
087900               MOVE NEJ TO INDATA-SW                                      
088000            END-IF                                                        
088100         END-IF                                                           
088200                                                                          
088300         IF MID-IDKVAINF-IN NOT = ALL '+'                                 
088400           IF MID-IDKVAINF-IN NOT NUMERIC                                 
088500             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKVAINF-IN-ATTR             
088600             MOVE NEJ TO INDATA-SW                                        
088700           ELSE                                                           
088800             PERFORM IMS-GU-W6KVAH01                                      
088900             MOVE MID-IDKVAINF-IN TO W-IDKVAINF                           
089000             IF SEGMENT-FINNS                                             
089100               PERFORM IMS-GNP-W6KVAH11                                   
089200               IF SEGMENT-FINNS                                           
089300                  MOVE JA TO W6KVAH11-SW                                  
089400                  MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKVAINF-IN-ATTR        
089500               ELSE                                                       
089600                  MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKVAINF-IN-ATTR        
089700                  MOVE NEJ TO INDATA-SW                                   
089800               END-IF                                                     
089900             END-IF                                                       
090000           END-IF                                                         
090100         END-IF                                                           
090200                                                                          
090300         IF MID-IDPROVPL-PRI-IN NOT = ALL '+'                             
090400            IF MID-IDPROVPL-PRI-IN NUMERIC                                
090500              MOVE MID-IDPROVPL-PRI-IN TO W-IDPROVPL                      
090600              MOVE 'R'              TO W-KDPROVPL                         
090700              PERFORM IMS-GU-W6PROA11                                     
090800              IF SEGMENT-SAKNAS                                           
090900                 MOVE MFS-ALFA-FAELT-FEL                                  
091000                                    TO MOD-IDPROVPL-PRI-IN-ATTR           
091100                 MOVE NEJ TO INDATA-SW                                    
091200              ELSE                                                        
091300                 MOVE MFS-ALFA-FAELT-RAETT                                
091400                                    TO MOD-IDPROVPL-PRI-IN-ATTR           
091500              END-IF                                                      
091600            ELSE                                                          
091700              MOVE MFS-ALFA-FAELT-FEL                                     
091800                                    TO MOD-IDPROVPL-PRI-IN-ATTR           
091900              MOVE NEJ TO INDATA-SW                                       
092000            END-IF                                                        
092100         END-IF                                                           
092200                                                                          
092300         IF MID-KVSKPLOT-PRI-IN NOT = ALL '+'                             
092400           IF MID-KVSKPLOT-PRI-IN NOT NUMERIC                             
092500             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVSKPLOT-PRI-IN-ATTR         
092600             MOVE NEJ TO INDATA-SW                                        
092700           ELSE                                                           
092800             IF MID-KVSKPLOT-PRI-IN = 0                                   
092900              MOVE MFS-NUM-FAELT-FEL   TO MOD-KVSKPLOT-PRI-IN-ATTR        
093000              MOVE NEJ TO INDATA-SW                                       
093100             ELSE                                                         
093200              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKPLOT-PRI-IN-ATTR        
093300             END-IF                                                       
093400           END-IF                                                         
093500         END-IF                                                           
093600                                                                          
093700         IF MID-KDCMD-SPEC  = SPACE OR                                    
093800            MID-KDCMD-SPEC  = ALL '+'                                     
093900            IF MID-IDKVAINF-IN NOT = ALL '+'                              
094000               PERFORM IMS-GU-W6KVAH01                                    
094100               MOVE NEJ TO W6KVAH22-SW                                    
094200               IF SEGMENT-FINNS                                           
094300                  PERFORM IMS-GNP-W6KVAH12                                
094400                  IF SEGMENT-FINNS                                        
094500                     MOVE MID-IDKVAINF-IN TO W-IDKVAINF                   
094600                     PERFORM IMS-GNP-W6KVAH22                             
094700                     IF SEGMENT-FINNS                                     
094800                        MOVE JA TO W6KVAH22-SW                            
094900                     END-IF                                               
095000                  END-IF                                                  
095100               END-IF                                                     
095200               IF W6KVAH22-FINNS                                          
095300                  IF MID-BEANST-IN           = ALL '+'                    
095400                     AND MID-IDTFN-IN        = ALL '+'                    
095500                     AND MID-IDPROVPL-PRI-IN = ALL '+'                    
095600                     AND MID-KVSKPLOT-PRI-IN = ALL '+'                    
095700                     MOVE MFS-ALFA-FAELT-FEL TO                           
095800                          MOD-IDPROVPL-PRI-IN-ATTR                        
095900                     MOVE MFS-NUM-FAELT-FEL TO                            
096000                          MOD-KVSKPLOT-PRI-IN-ATTR                        
096100                     MOVE NEJ TO INDATA-SW                                
096200                  END-IF                                                  
096300               END-IF                                                     
096400               IF MID-BEANST-IN = ALL '+' AND                             
096500                  ((W6KVAH22-FINNS AND SPEC-BEANST = SPACE) OR            
096600                  (W6KVAH22-SAKNAS AND W6KVAH11-FINNS))                   
096700                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEANST-IN-ATTR         
096800                  MOVE NEJ TO INDATA-SW                                   
096900               ELSE                                                       
097000                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEANST-IN-ATTR         
097100               END-IF                                                     
097200               IF MID-IDTFN-IN = ALL '+' AND                              
097300                  ((W6KVAH22-FINNS AND SPEC-IDTFN = SPACE) OR             
097400                  (W6KVAH22-SAKNAS AND W6KVAH11-FINNS))                   
097500                  MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDTFN-IN-ATTR          
097600                  MOVE NEJ TO INDATA-SW                                   
097700               ELSE                                                       
097800                  MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTFN-IN-ATTR          
097900               END-IF                                                     
098000            END-IF                                                        
098100         END-IF                                                           
098200                                                                          
098300         IF MID-IDKVAINF-IN        NOT = ALL '+'                          
098400            OR MID-IDPROVPL-PRI-IN NOT = ALL '+'                          
098500            OR MID-KVSKPLOT-PRI-IN NOT = ALL '+'                          
098600            OR MID-BEANST-IN       NOT = ALL '+'                          
098700            OR MID-IDTFN-IN        NOT = ALL '+'                          
098800            PERFORM IMS-GU-W6KVAH01                                       
098900            IF SEGMENT-SAKNAS                                             
099000               MOVE NEJ TO INDATA-SW                                      
099100               IF MID-IDKVAINF-IN NOT = ALL '+'                           
099200                  MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAINF-IN-ATTR          
099300               END-IF                                                     
099400               IF MID-IDPROVPL-PRI-IN NOT = ALL '+'                       
099500                 MOVE MFS-ALFA-FAELT-FEL TO                               
099600                                         MOD-IDPROVPL-PRI-IN-ATTR         
099700               END-IF                                                     
099800               IF MID-KVSKPLOT-PRI-IN NOT = ALL '+'                       
099900                  MOVE MFS-NUM-FAELT-FEL TO                               
100000                                         MOD-KVSKPLOT-PRI-IN-ATTR         
100100               END-IF                                                     
100200               IF MID-BEANST-IN NOT = ALL '+'                             
100300                  MOVE MFS-ALFA-FAELT-FEL TO MOD-BEANST-IN-ATTR           
100400               END-IF                                                     
100500               IF MID-IDTFN-IN NOT = ALL '+'                              
100600                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTFN-IN-ATTR            
100700               END-IF                                                     
100800            ELSE                                                          
100900               IF ART-KDKVATYP = 0                                        
101000                 MOVE NEJ TO INDATA-SW                                    
101100                             KKOD-SW                                      
101200                 IF MID-IDKVAINF-IN NOT = ALL '+'                         
101300                   MOVE MFS-NUM-FAELT-FEL TO                              
101400                                       MOD-IDKVAINF-IN-ATTR               
101500                 END-IF                                                   
101600                 IF MID-IDPROVPL-PRI-IN NOT = ALL '+'                     
101700                   MOVE MFS-NUM-FAELT-FEL TO                              
101800                                     MOD-IDPROVPL-PRI-IN-ATTR             
101900                 END-IF                                                   
102000                 IF MID-KVSKPLOT-PRI-IN NOT = ALL '+'                     
102100                   MOVE MFS-NUM-FAELT-FEL TO                              
102200                                     MOD-KVSKPLOT-PRI-IN-ATTR             
102300                 END-IF                                                   
102400                 IF MID-BEANST-IN NOT = ALL '+'                           
102500                   MOVE MFS-NUM-FAELT-FEL TO                              
102600                                     MOD-BEANST-IN-ATTR                   
102700                 END-IF                                                   
102800                 IF MID-IDTFN-IN NOT = ALL '+'                            
102900                   MOVE MFS-NUM-FAELT-FEL TO                              
103000                                     MOD-IDTFN-IN-ATTR                    
103100                 END-IF                                                   
103200               END-IF                                                     
103300            END-IF                                                        
103400         END-IF                                                           
103500                                                                          
103600         IF MID-KDCMD-SPEC NOT = ALL '+'                                  
103700            IF MID-KDCMD-SPEC = 'B' OR 'D' OR SPACE                       
103800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-SPEC-ATTR           
103900            ELSE                                                          
104000               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-SPEC-ATTR           
104100               MOVE NEJ TO INDATA-SW                                      
104200            END-IF                                                        
104300         END-IF                                                           
104400                                                                          
104500         IF (MID-KDCMD-SPEC = 'B' OR 'D')                                 
104600             AND MID-IDKVAINF-IN NOT = ALL '+'                            
104700            PERFORM IMS-GU-W6KVAH01                                       
104800            MOVE MID-IDKVAINF-IN TO W-IDKVAINF                            
104900            IF SEGMENT-FINNS                                              
105000              PERFORM IMS-GNP-W6KVAH22                                    
105100              IF SEGMENT-SAKNAS                                           
105200                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDKVAINF-IN-ATTR        
105300                 MOVE NEJ TO INDATA-SW                                    
105400              ELSE                                                        
105500                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKVAINF-IN-ATTR        
105600              END-IF                                                      
105700            END-IF                                                        
105800         END-IF                                                           
105900                                                                          
106000         IF MID-KDCMD-SPEC = 'B' OR 'D'                                   
106100            IF MID-IDKVAINF-IN = ALL '+'                                  
106200               MOVE MFS-NUM-FAELT-FEL TO MOD-IDKVAINF-IN-ATTR             
106300               MOVE NEJ TO INDATA-SW                                      
106400            END-IF                                                        
106500         END-IF                                                           
106600       ELSE                                                               
106700         MOVE NEJ TO INDATA-SW                                            
106800                     KKOD-SW                                              
106900       END-IF                                                             
107000                                                                          
107100       IF INDATA-FEL                                                      
107200        IF KKOD-FEL                                                       
107300          MOVE INSPECT-CODE-MISSING      TO MED-IDMFSFEL                  
107400        ELSE                                                              
107500         IF IDKVAFEL-FEL                                                  
107600            MOVE INF-UPPDAT-OTILLATET    TO MED-IDMFSFEL                  
107700         ELSE                                                             
107800            MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                  
107900         END-IF                                                           
108000         IF IDPROVPL-FEL                                                  
108100            MOVE INF-PROVPLAN-SAKNAS     TO MED-IDMFSFEL                  
108200         END-IF                                                           
108300         IF W6KVAH11-SAKNAS                                               
108400            MOVE INF-ARTIKEL-HIST-SAKNAS TO MED-IDMFSFEL                  
108500         END-IF                                                           
108600        END-IF                                                            
108700        CALL WMEDKONV USING MED-WMEDAREA                                  
108800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
108900*           MOVE W-IDLEVNR TO MOD-TEMFSFEL                                
109000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
109100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
109200       END-IF                                                             
109300     END-IF                                                               
109400     .                                                                    
109500     EJECT                                                                
109600 H-UPPDATERA SECTION.                                                     
109700                                                                          
109800     PERFORM IMS-GU-W6KVAH01                                              
109900     IF SEGMENT-FINNS                                                     
110000       MOVE ART-IDPROVPL-PRI TO WS-IDPROVPL-PRI                           
110100       MOVE ART-IDPROVPL-SEK TO WS-IDPROVPL-SEK                           
110200       PERFORM IMS-GHNP-W6KVAH12                                          
110300       IF SEGMENT-FINNS                                                   
110400          PERFORM HA-AENDRA-LEVNR                                         
110500       ELSE                                                               
110600          PERFORM HB-NYUPPLAEGG-LEVNR                                     
110700       END-IF                                                             
110800     ELSE                                                                 
110900       CALL FELLOG                                                        
111000     END-IF                                                               
111100     IF MID-IDKVAINF-IN NOT = ALL '+'                                     
111200        PERFORM HD-UPPDATERA-RAD                                          
111300     END-IF                                                               
111400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
111500     CALL WMEDKONV USING MED-WMEDAREA                                     
111600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
111700     PERFORM MFS-FORM-ATTR                                                
111800     PERFORM MFS-RENSA-FAELT-IN                                           
111900     .                                                                    
112000     EJECT                                                                
112100 HA-AENDRA-LEVNR    SECTION.                                              
112200                                                                          
112300     IF MID-KDCMD-LEV = 'D' OR 'B'                                        
112400        PERFORM IMS-DLET-W6KVAH12                                         
112500     ELSE                                                                 
112600        IF MID-FLKVASAK NOT = ALL '+'                                     
112700           MOVE MID-FLKVASAK TO LEV-FLKVASAK                              
112800           MOVE DATUM        TO LEV-TIKVASAK                              
112900           MOVE '1'          TO LEV-KDKVASAK                              
113000        END-IF                                                            
113100                                                                          
113200        IF MID-FLUPG NOT = ALL '+'                                        
113300          MOVE MID-FLUPG     TO LEV-FLUPG                                 
113400          MOVE '1'           TO LEV-KDKVAUP                               
113500          MOVE DATUM         TO LEV-TIUPG                                 
113600        END-IF                                                            
113700                                                                          
113800        PERFORM IMS-REPL-W6KVAH12                                         
113900     END-IF                                                               
114000     .                                                                    
114100     EJECT                                                                
114200 HB-NYUPPLAEGG-LEVNR SECTION.                                             
114300                                                                          
114400     MOVE SPACE           TO LEV-W6D212                                   
114500     MOVE ZERO            TO LEV-KVSKPLOT-PRI                             
114600                             LEV-KVSKPLOT-SEK                             
114700                             LEV-TIKVASAK                                 
114800                             LEV-TIUPG                                    
114900                             LEV-TIUPPDAT                                 
115000     MOVE NEJ             TO LEV-FLKVARED                                 
115100                             LEV-FLSKPSAK                                 
115200                                                                          
115300     MOVE W-IDLEVNR       TO LEV-IDLEVNR                                  
115400                                                                          
115500     IF MID-FLKVASAK NOT = ALL '+'                                        
115600        MOVE MID-FLKVASAK TO LEV-FLKVASAK                                 
115700        MOVE DATUM        TO LEV-TIKVASAK                                 
115800        MOVE '1'          TO LEV-KDKVASAK                                 
115900     ELSE                                                                 
116000        PERFORM IMS-GU-W6LEVA01                                           
116100        IF SEGMENT-FINNS AND W6LEVA-LEV-FLSKPLOT = 'N'                    
116200          MOVE JA         TO LEV-FLKVASAK                                 
116300        ELSE                                                              
116400          MOVE NEJ        TO LEV-FLKVASAK                                 
116500        END-IF                                                            
116600        MOVE DATUM        TO LEV-TIKVASAK                                 
116700        MOVE '1'          TO LEV-KDKVASAK                                 
116800     END-IF                                                               
116900                                                                          
117000     IF MID-FLUPG NOT = ALL '+'                                           
117100       MOVE MID-FLUPG     TO LEV-FLUPG                                    
117200       MOVE '1'           TO LEV-KDKVAUP                                  
117300       MOVE DATUM         TO LEV-TIUPG                                    
117400     ELSE                                                                 
117500       MOVE JA            TO LEV-FLUPG                                    
117600       MOVE '1'           TO LEV-KDKVAUP                                  
117700       MOVE DATUM         TO LEV-TIUPG                                    
117800     END-IF                                                               
117900                                                                          
118000     IF WS-IDPROVPL-PRI NOT = ZERO                                        
118100        MOVE WS-IDPROVPL-PRI  TO W-IDPROVPL                               
118200        MOVE 'N'              TO W-KDPROVPL                               
118300        PERFORM IMS-GU-W6PROA11                                           
118400        COMPUTE LEV-KVSKPLOT-PRI = 6102-KVSKPLOT + 1                      
118500     ELSE                                                                 
118600        MOVE ZERO             TO LEV-KVSKPLOT-PRI                         
118700     END-IF                                                               
118800                                                                          
118900     IF WS-IDPROVPL-PRI NOT = ZERO                                        
119000        MOVE WS-IDPROVPL-SEK  TO W-IDPROVPL                               
119100        MOVE 'N'              TO W-KDPROVPL                               
119200        PERFORM IMS-GU-W6PROA11                                           
119300        COMPUTE LEV-KVSKPLOT-SEK = 6102-KVSKPLOT + 1                      
119400     ELSE                                                                 
119500        MOVE ZERO             TO LEV-KVSKPLOT-SEK                         
119600     END-IF                                                               
119700                                                                          
119800     MOVE DATUM           TO LEV-TIUPPDAT                                 
119900                                                                          
120000     PERFORM IMS-ISRT-W6KVAH12                                            
120100                                                                          
120200     .                                                                    
120300     EJECT                                                                
120400 HD-UPPDATERA-RAD   SECTION.                                              
120500                                                                          
120600     MOVE MID-IDKVAINF-IN TO W-IDKVAINF                                   
120700     PERFORM IMS-GU-W6KVAH01                                              
120800     PERFORM IMS-GNP-W6KVAH12                                             
120900     PERFORM IMS-GHNP-W6KVAH22                                            
121000     IF SEGMENT-SAKNAS                                                    
121100        MOVE SPACE TO SPEC-W6D222                                         
121200     END-IF                                                               
121300     IF MID-BEANST-IN NOT = ALL '+'                                       
121400        MOVE MID-BEANST-IN       TO SPEC-BEANST                           
121500     END-IF                                                               
121600     IF MID-IDPROVPL-PRI-IN NOT = ALL '+'                                 
121700        MOVE MID-IDPROVPL-PRI-IN TO SPEC-IDPROVPL-PRI                     
121800     ELSE                                                                 
121900        IF SEGMENT-SAKNAS                                                 
122000           MOVE '3'              TO SPEC-IDPROVPL-PRI                     
122100        END-IF                                                            
122200     END-IF                                                               
122300     IF MID-IDTFN-IN NOT = ALL '+'                                        
122400        MOVE MID-IDTFN-IN        TO SPEC-IDTFN                            
122500     END-IF                                                               
122600     IF MID-KVSKPLOT-PRI-IN NOT = ALL '+'                                 
122700        MOVE MID-KVSKPLOT-PRI-IN TO SPEC-KVSKPLOT-PRI                     
122800     ELSE                                                                 
122900        IF SEGMENT-SAKNAS                                                 
123000           MOVE '2'              TO SPEC-KVSKPLOT-PRI                     
123100        END-IF                                                            
123200     END-IF                                                               
123300     IF SEGMENT-FINNS                                                     
123400        IF MID-KDCMD-SPEC = 'D' OR 'B'                                    
123500           PERFORM IMS-DLET-W6KVAH22                                      
123600        ELSE                                                              
123700           PERFORM IMS-REPL-W6KVAH22                                      
123800        END-IF                                                            
123900     ELSE                                                                 
124000        MOVE MID-IDKVAINF-IN TO SPEC-IDKVAINF                             
124100        PERFORM IMS-ISRT-W6KVAH22                                         
124200     END-IF                                                               
124300     .                                                                    
124400     EJECT                                                                
124500 MFS-RENSA-FAELT-UT SECTION.                                              
124600                                                                          
124700     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
124800                             MOD-BELEV                                    
124900                             MOD-FLKVASAK                                 
125000                             MOD-TIKVASAK                                 
125100                             MOD-FLUPG                                    
125200                             MOD-TIUPG                                    
125300                             MOD-IDKVAINF-ENTER                           
125400                             MOD-IDKVAINF-NEXT                            
125500                                                                          
125600     MOVE 1 TO INDX                                                       
125700     PERFORM UNTIL INDX > MAX-INDX                                        
125800         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
125900         ADD 1 TO INDX                                                    
126000     END-PERFORM                                                          
126100     .                                                                    
126200     SKIP2                                                                
126300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
126400                                                                          
126500     MOVE MFS-RENSA-FAELT TO MOD-IDKVAINF     (INDX)                      
126600                             MOD-IDPROVPL-PRI (INDX)                      
126700                             MOD-KVSKPLOT-PRI (INDX)                      
126800                             MOD-BEANST       (INDX)                      
126900                             MOD-IDTFN        (INDX)                      
127000     .                                                                    
127100     SKIP2                                                                
127200 MFS-RENSA-FAELT-IN SECTION.                                              
127300                                                                          
127400     MOVE MFS-RENSA-FAELT TO MOD-IDKVAINF-IN                              
127500                             MOD-IDPROVPL-PRI-IN                          
127600                             MOD-KVSKPLOT-PRI-IN                          
127700                             MOD-BEANST-IN                                
127800                             MOD-IDTFN-IN                                 
127900                             MOD-KDCMD-LEV                                
128000                             MOD-KDCMD-SPEC                               
128100     .                                                                    
128200     EJECT                                                                
128300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
128400                                                                          
128500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
128600                               MOD-IDLEVNR-UT                             
128700                               MOD-IDKVAINFU                              
128800                               MOD-TIREGDAT-UT                            
128900                               MOD-KDKVAINF-UT                            
129000*    MOVE MFS-ROER-EJ-FAELT TO                                            
129100                               MOD-BEART                                  
129200                               MOD-BELEV                                  
129300                               MOD-FLKVASAK                               
129400                               MOD-TIKVASAK                               
129500                               MOD-FLUPG                                  
129600                               MOD-TIUPG                                  
129700                               MOD-IDKVAINF-ENTER                         
129800                               MOD-IDKVAINF-NEXT                          
129900     MOVE +1 TO INDX                                                      
130000     PERFORM UNTIL INDX > MAX-INDX                                        
130100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
130200       ADD +1 TO INDX                                                     
130300     END-PERFORM                                                          
130400     .                                                                    
130500     SKIP2                                                                
130600 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
130700                                                                          
130800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAINF     (INDX)                    
130900                               MOD-IDPROVPL-PRI (INDX)                    
131000                               MOD-KVSKPLOT-PRI (INDX)                    
131100                               MOD-BEANST       (INDX)                    
131200                               MOD-IDTFN        (INDX)                    
131300     .                                                                    
131400     SKIP2                                                                
131500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
131600                                                                          
131700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKVAINF-IN                            
131800                               MOD-IDPROVPL-PRI-IN                        
131900                               MOD-KVSKPLOT-PRI-IN                        
132000                               MOD-KVSKPLOT-PRI-IN                        
132100                               MOD-BEANST-IN                              
132200                               MOD-IDTFN-IN                               
132300                               MOD-KDCMD-LEV                              
132400                               MOD-KDCMD-SPEC                             
132500**  FIX IGEN                                                              
132600                               MOD-IDARTNR-IN                             
132700                               MOD-IDLEVNR-IN                             
132800**  SLUT FIX                                                              
132900     .                                                                    
133000     EJECT                                                                
133100 MFS-FORM-ATTR SECTION.                                                   
133200                                                                          
133300     MOVE MFS-FORMATETS-ATTR TO MOD-FLKVASAK-ATTR                         
133400                                MOD-FLUPG-ATTR                            
133500                                MOD-KDCMD-LEV-ATTR                        
133600                                MOD-KDCMD-SPEC-ATTR                       
133700                                MOD-IDKVAINF-IN-ATTR                      
133800                                MOD-IDPROVPL-PRI-IN-ATTR                  
133900                                MOD-KVSKPLOT-PRI-IN-ATTR                  
134000                                MOD-BEANST-IN-ATTR                        
134100                                MOD-IDTFN-IN-ATTR                         
134200     .                                                                    
134300     EJECT                                                                
134400* --- IMS SEKTIONER ---                                                   
134500     SKIP3                                                                
134600 IMS-GET-MSG SECTION.                                                     
134700                                                                          
134800     MOVE '  QC' TO GODK-STATUSKODER                                      
134900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
135000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135100     PERFORM IMS-STATUSKONTROLL                                           
135200     .                                                                    
135300     SKIP3                                                                
135400 IMS-INSERT-MSG SECTION.                                                  
135500                                                                          
135600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
135700       MOVE '0' TO MFS-KDHUVOMR                                           
135800     END-IF                                                               
135900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
136000     MOVE SPACE TO GODK-STATUSKODER                                       
136100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
136200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     EJECT                                                                
136600 IMS-GU-W6KVAH01 SECTION.                                                 
136700     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
136800          DELIMITED BY SIZE INTO SSA1                                     
136900     MOVE '  GE' TO GODK-STATUSKODER                                      
137000     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA1 SSA1                     
137100     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
137200     PERFORM IMS-STATUSKONTROLL                                           
137300     .                                                                    
137400     EJECT                                                                
137500 IMS-GU-W6LEVA01 SECTION.                                                 
137600     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
137700          DELIMITED BY SIZE INTO SSA1                                     
137800     MOVE '  GE' TO GODK-STATUSKODER                                      
137900     CALL CBLTDLI USING GU W6F1-PCB DLI-IO-AREA1 SSA1                     
138000     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
138100     PERFORM IMS-STATUSKONTROLL                                           
138200     .                                                                    
138300     EJECT                                                                
138400 IMS-GNP-W6KVAH11 SECTION.                                                
138500     STRING 'W6KVAH11(IDKVAINF =' W-IDKVAINF-X ')'                        
138600          DELIMITED BY SIZE INTO SSA1                                     
138700     MOVE '  GE' TO GODK-STATUSKODER                                      
138800     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1                    
138900     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200     EJECT                                                                
139300 IMS-GHNP-W6KVAH12 SECTION.                                               
139400     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
139500          DELIMITED BY SIZE INTO SSA1                                     
139600     MOVE '  GE' TO GODK-STATUSKODER                                      
139700     CALL CBLTDLI USING GHNP KVAH-PCB DLI-IO-AREA1 SSA1                   
139800     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
139900     PERFORM IMS-STATUSKONTROLL                                           
140000     .                                                                    
140100     EJECT                                                                
140200 IMS-GNP-W6KVAH12 SECTION.                                                
140300     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
140400          DELIMITED BY SIZE INTO SSA1                                     
140500     MOVE '  GE' TO GODK-STATUSKODER                                      
140600     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1                    
140700     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000     EJECT                                                                
141100 IMS-GNP-W6KVAH22 SECTION.                                                
141200     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
141300          DELIMITED BY SIZE INTO SSA1                                     
141400     STRING 'W6KVAH22(IDKVAINF =' W-IDKVAINF-X ')'                        
141500          DELIMITED BY SIZE INTO SSA2                                     
141600     MOVE '  GE' TO GODK-STATUSKODER                                      
141700     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1 SSA2               
141800     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
141900     PERFORM IMS-STATUSKONTROLL                                           
142000     .                                                                    
142100     EJECT                                                                
142200 IMS-GNP-OKVAL-W6KVAH12 SECTION.                                          
142300     MOVE 'W6KVAH12' TO SSA1                                              
142400     MOVE '  GE' TO GODK-STATUSKODER                                      
142500     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1                    
142600     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900     EJECT                                                                
143000 IMS-ISRT-W6KVAH12 SECTION.                                               
143100                                                                          
143200     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
143300          DELIMITED BY SIZE INTO SSA1                                     
143400     MOVE 'W6KVAH12 ' TO SSA2                                             
143500     MOVE '  II' TO GODK-STATUSKODER                                      
143600     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA1 SSA1 SSA2              
143700     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     .                                                                    
144000     EJECT                                                                
144100 IMS-REPL-W6KVAH12 SECTION.                                               
144200                                                                          
144300     MOVE '  ' TO GODK-STATUSKODER                                        
144400     CALL CBLTDLI USING REPL KVAH-PCB DLI-IO-AREA1                        
144500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     .                                                                    
144800     EJECT                                                                
144900 IMS-DLET-W6KVAH12 SECTION.                                               
145000                                                                          
145100     MOVE '  ' TO GODK-STATUSKODER                                        
145200     CALL CBLTDLI USING DLET KVAH-PCB DLI-IO-AREA1                        
145300     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
145400     PERFORM IMS-STATUSKONTROLL                                           
145500     .                                                                    
145600     EJECT                                                                
145700 IMS-GNP-OKVAL-W6KVAH22 SECTION.                                          
145800     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
145900          DELIMITED BY SIZE INTO SSA1                                     
146000     MOVE 'W6KVAH22' TO SSA2                                              
146100     MOVE '  GE' TO GODK-STATUSKODER                                      
146200     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1 SSA2               
146300     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
146400     PERFORM IMS-STATUSKONTROLL                                           
146500     .                                                                    
146600     EJECT                                                                
146700 IMS-GHNP-W6KVAH22 SECTION.                                               
146800     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
146900          DELIMITED BY SIZE INTO SSA1                                     
147000     STRING 'W6KVAH22(IDKVAINF =' W-IDKVAINF-X ')'                        
147100          DELIMITED BY SIZE INTO SSA2                                     
147200     MOVE '  GE' TO GODK-STATUSKODER                                      
147300     CALL CBLTDLI USING GHNP KVAH-PCB DLI-IO-AREA1 SSA1 SSA2              
147400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
147500     PERFORM IMS-STATUSKONTROLL                                           
147600     .                                                                    
147700     EJECT                                                                
147800 IMS-GNP-W6KVAH22-NEXT SECTION.                                           
147900     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X ')'                         
148000          DELIMITED BY SIZE INTO SSA1                                     
148100     STRING 'W6KVAH22(IDKVAINF=>' W-IDKVAINF-X ')'                        
148200          DELIMITED BY SIZE INTO SSA2                                     
148300     MOVE '  GE' TO GODK-STATUSKODER                                      
148400     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA1 SSA1 SSA2               
148500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
148600     PERFORM IMS-STATUSKONTROLL                                           
148700     .                                                                    
148800     EJECT                                                                
148900 IMS-ISRT-W6KVAH22 SECTION.                                               
149000                                                                          
149100     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
149200          DELIMITED BY SIZE INTO SSA1                                     
149300     STRING 'W6KVAH12(IDLEVNR  =' W-IDLEVNR-X  ')'                        
149400          DELIMITED BY SIZE INTO SSA2                                     
149500     MOVE 'W6KVAH22 ' TO SSA3                                             
149600     MOVE '  II' TO GODK-STATUSKODER                                      
149700     CALL CBLTDLI USING ISRT KVAH-PCB DLI-IO-AREA1 SSA1 SSA2 SSA3         
149800     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     EJECT                                                                
150200 IMS-REPL-W6KVAH22 SECTION.                                               
150300                                                                          
150400     MOVE '  ' TO GODK-STATUSKODER                                        
150500     CALL CBLTDLI USING REPL KVAH-PCB DLI-IO-AREA1                        
150600     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000 IMS-DLET-W6KVAH22 SECTION.                                               
151100                                                                          
151200     MOVE '  ' TO GODK-STATUSKODER                                        
151300     CALL CBLTDLI USING DLET KVAH-PCB DLI-IO-AREA1                        
151400     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700     EJECT                                                                
151800 IMS-GU-WLLEVA01 SECTION.                                                 
151900     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
152000          DELIMITED BY SIZE INTO SSA1                                     
152100     MOVE '  GE' TO GODK-STATUSKODER                                      
152200     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA2 SSA1                     
152300     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
152400     PERFORM IMS-STATUSKONTROLL                                           
152500     .                                                                    
152600     EJECT                                                                
152700 IMS-GNP-WLLEVA14 SECTION.                                                
152800     MOVE 'WLLEVA14' TO SSA1                                              
152900     MOVE '  GE' TO GODK-STATUSKODER                                      
153000     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA2 SSA1                    
153100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
153200     PERFORM IMS-STATUSKONTROLL                                           
153300     .                                                                    
153400     EJECT                                                                
153500 IMS-GU-WLARTC01 SECTION.                                                 
153600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
153700          DELIMITED BY SIZE INTO SSA1                                     
153800     MOVE '  GE' TO GODK-STATUSKODER                                      
153900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
154000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300     EJECT                                                                
154400 IMS-GU-WLBENA11 SECTION.                                                 
154500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
154600          DELIMITED BY SIZE INTO SSA1                                     
154700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
154800          DELIMITED BY SIZE INTO SSA2                                     
154900     MOVE '  GE' TO GODK-STATUSKODER                                      
155000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA4 SSA1 SSA2                
155100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
155200     PERFORM IMS-STATUSKONTROLL                                           
155300     .                                                                    
155400     EJECT                                                                
155500 IMS-GU-W6PROA11 SECTION.                                                 
155600                                                                          
155700     STRING 'W6PROA01(W6GXKEY  =' W-W6GX-6101-KEY-X ')'                   
155800            DELIMITED BY SIZE INTO SSA1                                   
155900     STRING 'W6PROA11(W6GXKEY  =' W-W6GX-6102-KEY-X ')'                   
156000            DELIMITED BY SIZE INTO SSA2                                   
156100     MOVE '  GE'                  TO GODK-STATUSKODER                     
156200     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA5 SSA1 SSA2                
156300     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600 IMS-STATUSKONTROLL SECTION.                                              
156700                                                                          
156800     SET STATUS-IX TO 1                                                   
156900     SEARCH GODK-STATUS                                                   
157000       AT END                                                             
157100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
157200         DELIMITED BY SIZE INTO FELTEXT                                   
157300         CALL FELLOG                                                      
157400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
157500         CONTINUE                                                         
157600     END-SEARCH                                                           
157700     .                                                                    
