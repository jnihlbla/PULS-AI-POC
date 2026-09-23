000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1053200.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   94/02/18.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000800*    FUNKTION:                                                            
000900*        SÖKBEGREPP VADIS / KATALOG                                       
001000*        UPPDATERAR WDN1 (WLKATM) MED DATA FÖR VADIS-APPLIKATIONEN        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLKATM (WDN1)                              
001300*                   LÄSER   WDGX1212  (WDR2)                              
001400*                   LÄSER   WDGX1214  (WDR2)                              
001410*                   LÄSER   WDGX1216  (WDR2)                              
001500*    INDATA.                                                              
001600*        TRANSAKTION: W1T532                                              
001700*        MID:         W1I53201                                            
001800*    UTDATA.                                                              
001900*        MOD:         W1O53201                                            
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002401*    -- CHECKED BY WY2000                                                 
002410     SKIP3                                                                
002500 77  IDPGM                       PIC X(08)   VALUE 'W1053200'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000                                                                          
003100*    --- ÖVRIGA ARBETSFÄLT                                                
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500 01  DAGENS-DATUM-LONG-X.                                                 
003501     03  DAGENS-SEKEL            PIC 9(2).                                
003510     03  DAGENS-DATUM.                                                    
003600         05 DAGENS-AAR           PIC 9(2).                                
003700         05 DAGENS-MAANAD        PIC 9(2).                                
003800         05 DAGENS-DAG           PIC 9(2).                                
003801******   DAGENS ÅR I FORMATET ÅÅÅÅ                                        
003802 01  DAGENS-AAR-LONG REDEFINES DAGENS-DATUM-LONG-X                        
003803                                 PIC 9(4).                                
003812******   NÄSTA ÅR I FORMATET ÅÅÅÅ                                         
003813 01  NAESTA-AAR                  PIC 9(4).                                
003814******   ÅRET DÄRPÅ I SAMMA FORMAT                                        
003815 01  MAX-AAR                     PIC 9(4).                                
003816                                                                          
003820 01  WS-KDCATPUB                 PIC X(6)    VALUE SPACE.                 
004500                                                                          
004600 01  FILLER                      PIC X(16)  VALUE 'INDEX-NYCKLAR'.        
004700                                                                          
004800 77  IX                          PIC S9(9)  VALUE +0   COMP SYNC.         
004810 77  IX2                         PIC S9(9)  VALUE +0   COMP SYNC.         
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
005000 77  RAD                         PIC S9(9)  VALUE +0   COMP SYNC.         
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 01  FILLER                      PIC X(16)  VALUE 'SKARM-ARBFALT'.        
005500 77  WS-IDCATNR                  PIC X(5)    VALUE SPACE.                 
005600                                                                          
005700 01  FILLER                      PIC X(16)  VALUE 'SWITCHAR'.             
005800 77  INPUT-FINNS-SW              PIC X       VALUE 'N'.                   
005900     88  INPUT-FINNS                         VALUE 'J'.                   
006000     88  INPUT-SAKNAS                        VALUE 'N'.                   
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006600 77  INTERVALL-SW                PIC X       VALUE 'J'.                   
006700     88  INTERVALL-OK                        VALUE 'J'.                   
006800     88  INTERVALL-FEL                       VALUE 'N'.                   
006900                                                                          
007000 77  KOMBINATION-SW              PIC X       VALUE 'J'.                   
007100     88  KOMBINATION-OK                      VALUE 'J'.                   
007200     88  KOMBINATION-FEL                     VALUE 'N'.                   
007300                                                                          
007400 77  ALLT-SW                     PIC X       VALUE 'N'.                   
007500*            INDIKERAR KLART ATT VISA BAS-VÄRDEN                          
007600     88  ALLT-OK                             VALUE 'J'.                   
007700                                                                          
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100                                                                          
008200 77  RAD-RENSAD                  PIC X       VALUE 'N'.                   
008300                                                                          
008400                                                                          
008500 01  FILLER               PIC X(16)  VALUE 'ARB-O-SPAR-FAELT'.            
008600*    --- ARBETSFÄLT FÖR INDATA OCH  SPAR-FÄLT                             
008700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008800     88  EGEN-MID                            VALUE '1532'.                
008900     88  GODK-MID                            VALUE '1514' '1515'          
009000                                                   '1517' '1518'          
009100                                                   '1519' '1531'          
009200                                                   '1532'.                
009300     88  HELP-MID                            VALUE '0551'.                
009400                                                                          
009500 01  WS-PARTNER-GRUPP.                                                    
009501     03  WS-PARTNER-GRP          OCCURS 6                                 
009503                                 PIC X(6)    VALUE SPACE.                 
009510                                                                          
009520 77  WS-IDPARTGRP                PIC X(6)    VALUE SPACE.                 
009600     88  IDPARTGRP-OK                        VALUE '      '               
009800                                                   'AME   '               
009900                                                   'EUR   '               
010000                                                   'NOR   '               
010100                                                   'INT   '.              
010200 01  WS-MFSKOD.                                                           
010300     03  FILLER                  PIC XX.                                  
010400                                                                          
010410 01  WS-TESTFAELT.                                                        
010450     03 WS-IDCATNR-IN        PIC X(5).                                    
010460     03 FILLER               PIC X         VALUE '/'.                     
010470     03 WS-IDCATNR-UT        PIC X(5).                                    
010480     03 FILLER               PIC X         VALUE '/'.                     
010490     03 WS-PARTN-GRP         OCCURS 6 TIMES.                              
010491        05 WS-PARTGRP        PIC X(6).                                    
010492        05 FILLER            PIC X         VALUE '/'.                     
010493     03 WS-FLKATVAD          PIC X.                                       
010494     03 FILLER               PIC X         VALUE '/'.                     
010495*    03 WS-RAD               OCCURS 8 TIMES.                              
010496     03 WS-RAD.                                                           
010497        05 WS-IDMODELL       PIC X(3).                                    
010498        05 FILLER            PIC X         VALUE '/'.                     
010499        05 WS-TIMODAAR-STA   PIC X(4).                                    
010500        05 FILLER            PIC X         VALUE '/'.                     
010501        05 WS-TIMODAAR-STO   PIC X(4).                                    
010502        05 FILLER            PIC X         VALUE '/'.                     
010503        05 WS-IDVARIANT      PIC X(15).                                   
010504        05 FILLER            PIC X         VALUE '/'.                     
010505        05 WS-IDRADNR        PIC 9(3).                                    
010506 01  WS-TESTFAELT2.                                                       
010512        05 WS-ATTR1          PIC X(2).                                    
010513        05 WS-PARTGRP1       PIC X(6).                                    
010514        05 FILLER            PIC X         VALUE '/'.                     
010515        05 WS-ATTR2          PIC X(2).                                    
010516        05 WS-PARTGRP2       PIC X(6).                                    
010517        05 FILLER            PIC X         VALUE '/'.                     
010518        05 WS-ATTR3          PIC X(2).                                    
010519        05 WS-PARTGRP3       PIC X(6).                                    
010520        05 FILLER            PIC X         VALUE '/'.                     
010521        05 WS-ATTR4          PIC X(2).                                    
010522        05 WS-PARTGRP4       PIC X(6).                                    
010523        05 FILLER            PIC X         VALUE '/'.                     
010524        05 WS-ATTR5          PIC X(2).                                    
010525        05 WS-PARTGRP5       PIC X(6).                                    
010526        05 FILLER            PIC X         VALUE '/'.                     
010527        05 WS-ATTR6          PIC X(2).                                    
010528        05 WS-PARTGRP6       PIC X(6).                                    
010530 77  SPAR-IDMODELL               PIC X(3)  VALUE SPACE.                   
010600 77  SPAR-IDVARIANT              PIC X(15) VALUE SPACE.                   
010700 77  SPAR-TIMODAAR-STA           PIC 9(4) VALUE ZERO.                     
010800 77  SPAR-TIMODAAR-STO           PIC 9(4) VALUE ZERO.                     
010900 77  SPAR-IDRADNR                PIC 9(3) VALUE ZERO.                     
011000     EJECT                                                                
011100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011200 01  GENERELLA-SUBPROGRAM.                                                
011300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011510     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011800*01 -COPY WMEDAREA                                                        
011910     EJECT                                                                
011920*    --- PARAMETRAR TILL SUBPROGRAM WDATAREA                              
011930*01  -COPY WDATAREA                                                       
011940     EJECT                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012300     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
012400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012500     03  ERR-FEL-RADNR           PIC X(3)    VALUE '014'.                 
012600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012700     03  ERR-INVALID-COMBINATION PIC X(3)    VALUE '238'.                 
012800     03  ERR-FOM-STOERRE-TOM     PIC X(3)    VALUE '240'.                 
012900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013000     SKIP3                                                                
013010 01  SPECIAL-MEDDELANDE.                                                  
013011     03 SPEC-INF-1.                                                       
013020       05 FILLER                 PIC X(50)                                
013030       VALUE '--- SKAPA EN VADIS-GEN-TABELL PÅ 1533. (KOPIERA)'.          
013040       05 FILLER                 PIC X(50)                                
013050       VALUE '--- CREATE A VADIS-GEN-TABLE ON 1533. (COPY)'.              
013060     03 FILLER  REDEFINES SPEC-INF-1.                                     
013070       05 SPECIAL-INF-1          PIC X(50)  OCCURS 2.                     
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)  VALUE 'TEST-AREA'.            
013300*01       -COPY WDN101    -PRE TEST- .                                    
013400     EJECT                                                                
013500*                                                                         
013600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013700                                                                          
013800 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
013900*01  MID    -COPY W1I53201                                                
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014200     SKIP3                                                                
014300*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014600*    03  -COPY W1O53201 -RED MSG-AREA.                                    
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014900     SKIP3                                                                
015000*01  -COPY WMFSAREA                                                       
015100     EJECT                                                                
015200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300*                                                                         
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015600     SKIP3                                                                
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-IDCATNR-X.                                                     
015900         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
015910     03  W-TIAAAA-X.                                                      
015920         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
016000                                                                          
016100 01  W-1211-KEY-X.                                                        
016200     03  IDHTYP-1211               PIC X(4)  VALUE '1211'.                
016300     03  FILLER                    PIC X(26) VALUE LOW-VALUE.             
016400                                                                          
016500 01  W-1212-KEY-X.                                                        
016600     03  W-1212-IDMODELL           PIC X(3) VALUE SPACE.                  
016700                                                                          
016800 01  W-1213-KEY-X.                                                        
016900     03  IDHTYP-1213               PIC X(4)  VALUE '1213'.                
017000     03  FILLER                    PIC X(26) VALUE LOW-VALUE.             
017100                                                                          
017200 01  W-1214-KEY-X.                                                        
017300     03  W-1214-IDVARIANT          PIC X(15) VALUE SPACE.                 
017400                                                                          
017500 01  W-1215-KEY-X.                                                        
017600     03  IDHTYP-1215               PIC X(4)  VALUE '1215'.                
017700     03  W-1215-IDMODELL           PIC X(3)  VALUE SPACE.                 
017800     03  FILLER                    PIC X(23) VALUE LOW-VALUE.             
017900                                                                          
018000 01  W-1216-KEY-X.                                                        
018100     03  W-1216-IDVARIANT          PIC X(15) VALUE SPACE.                 
018200                                                                          
018300*    --- STATUS-KOD FRÅN IMS                                              
018400 01  STATUS-WS                   PIC XX.                                  
018500     88  SEGMENT-FINNS                       VALUE '  '.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     SKIP2                                                                
018800 01  GODK-STATUSKODER.                                                    
018900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019000     SKIP3                                                                
019100 01  SSA1                        PIC X(64).                               
019200 01  SSA2                        PIC X(64).                               
019300     EJECT                                                                
019400*    --- IMS FUNKTIONSKODER                                               
019500*01  -COPY W0003                                                          
019600     EJECT                                                                
019700*    ---  DLI INPUT-OUTPUT AREA                                           
019800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019900     SKIP3                                                                
020000 01  DLI-IO-AREA.                                                         
020100                                                                          
020200     03  FILLER                  PIC X(16)   VALUE 'WDN101-AREA'.         
020300     03  IO-AREA-1               PIC X(496)  VALUE SPACE.                 
020400     03  WLKATM01 REDEFINES IO-AREA-1.                                    
020500*        05  -COPY WDN101    -PRE KATM-                                   
020501     EJECT                                                                
020510     03  WLKATM11 REDEFINES IO-AREA-1 .                                   
020520*        05  -COPY WDN111  -PRE KATM-                                     
020600     EJECT                                                                
020700                                                                          
020800     03  FILLER                  PIC X(16)  VALUE 'WDGX1212-AREA'.        
020900     03  IO-AREA-2               PIC X(32)  VALUE SPACE.                  
021000     03  WDGX1212 REDEFINES IO-AREA-2.                                    
021100*        05  -COPY WDGX1212                                               
021200     EJECT                                                                
021300                                                                          
021400     03  FILLER                  PIC X(16)  VALUE 'WDGX1214-AREA'.        
021500     03  IO-AREA-3               PIC X(32)  VALUE SPACE.                  
021600     03  WDGX1214 REDEFINES IO-AREA-3.                                    
021700*        05  -COPY WDGX1214                                               
021800     EJECT                                                                
021900                                                                          
022000     03  FILLER                  PIC X(16)  VALUE 'WDGX1216-AREA'.        
022100     03  IO-AREA-4               PIC X(32)  VALUE SPACE.                  
022200     03  WDGX01 REDEFINES IO-AREA-4.                                      
022300*        05  -COPY WDGX1215                                               
022400     03  WDGX1216 REDEFINES IO-AREA-4.                                    
022500*        05  -COPY WDGX1216                                               
022600     EJECT                                                                
022700 LINKAGE SECTION.                                                         
022800                                                                          
022900*01  -COPY W0009   -PRE MSG-                                              
023000     EJECT                                                                
023100*01  -COPY W0008  -PRE KATM-                                              
023200     05  FILLER                  PIC X.                                   
023300*01  -COPY W0008  -PRE 1212-                                              
023400     05  FILLER                  PIC X.                                   
023500*01  -COPY W0008  -PRE 1214-                                              
023600     05  FILLER                  PIC X.                                   
023700*01  -COPY W0008  -PRE 1216-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB KATM-PCB 1212-PCB                      
024100                                   1214-PCB 1216-PCB.                     
024200     ENTRY 'DLITCBL' USING MSG-PCB KATM-PCB 1212-PCB                      
024300                                   1214-PCB 1216-PCB.                     
024400     PERFORM IMS-GET-MSG                                                  
024500     IF SEGMENT-FINNS                                                     
024600       PERFORM A-INIT                                                     
024700       PERFORM B-KOLLA-NYCKLAR                                            
024800       IF  NYCKLAR-OK                                                     
024900         IF MFS-UPDATE                                                    
025000           PERFORM C-KONTROLLERA-INPUT                                    
025100           IF INDATA-OK                                                   
025200             PERFORM D-UPPDATERA                                          
025210             PERFORM F-LAES-BAS-VISA-INFO                                 
025300           END-IF                                                         
025400         ELSE                                                             
025500           PERFORM E-ENTER-TRYCKNING                                      
025510           PERFORM F-LAES-BAS-VISA-INFO                                   
025600         END-IF                                                           
025800       END-IF                                                             
025926       MOVE LENGTH OF MOD-W1O53201  TO MSG-KVLL                           
025930       ADD +4                       TO MSG-KVLL                           
026000       PERFORM IMS-INSERT-MSG                                             
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     IF MSG-DUBBLA-TRANSKODER                                             
027000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I53201                 
027100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027300     ELSE                                                                 
027400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I53201                  
027500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027700     END-IF                                                               
027800                                                                          
027900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028200                                                                          
028300     MOVE LOW-VALUE TO MSG-AREA                                           
028400     MOVE 'W1O53201' TO MFS-IDMOD                                         
028500     MOVE '1532' TO MOD-IDTRANS                                           
028600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028700                                                                          
028800     IF EGEN-MID OR HELP-MID                                              
028900       CONTINUE                                                           
029000     ELSE                                                                 
029100       MOVE SPACE TO MFS-KDTRTYP                                          
029200     END-IF                                                               
029300                                                                          
029400     IF MFS-IDPFK = '7' OR '8'                                            
029500       MOVE SPACE TO MFS-IDPFK                                            
029600     END-IF                                                               
029700                                                                          
029800     IF ENGLISH-TEXT                                                      
029900       MOVE +2 TO SPRAK-IX                                                
030000       MOVE 'GB ' TO MED-IDSKYLT                                          
030100     ELSE                                                                 
030200       MOVE +1 TO SPRAK-IX                                                
030300       MOVE 'S  ' TO MED-IDSKYLT                                          
030400     END-IF                                                               
030410                                                                          
030500     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-LONG-X              
030700     ADD  DAGENS-AAR-LONG 1  GIVING NAESTA-AAR                            
030800     ADD  NAESTA-AAR 1       GIVING MAX-AAR                               
031700     .                                                                    
031800     EJECT                                                                
031900 B-KOLLA-NYCKLAR SECTION.                                                 
032000                                                                          
032100     MOVE JA                             TO NYCKLAR-SW                    
032200*    -- KONTROLL AV IDCATNR                                               
032300     MOVE MFS-RENSA-FAELT                TO MOD-IDCATNR-IN                
032400                                                                          
032500     IF MID-IDCATNR-IN = ALL '+'                                          
032600       MOVE MID-IDCATNR-UT               TO WS-IDCATNR                    
032700       INSPECT WS-IDCATNR REPLACING LEADING SPACE BY ZERO                 
032800     ELSE                                                                 
032900       MOVE MID-IDCATNR-IN               TO WS-IDCATNR                    
033000       MOVE SPACE                       TO MFS-IDPFK   MFS-KDTRTYP        
033100     END-IF                                                               
033200                                                                          
033300     IF WS-IDCATNR NUMERIC AND WS-IDCATNR > ZERO                          
033400       MOVE WS-IDCATNR                  TO W-IDCATNR                      
033500     ELSE                                                                 
033600       MOVE NEJ                         TO NYCKLAR-SW                     
033700     END-IF                                                               
033800                                                                          
033900     IF NYCKLAR-OK  OR   GODK-MID                                         
034000       MOVE WS-IDCATNR                  TO MOD-IDCATNR-UT                 
034100       INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE             
034200     ELSE                                                                 
034300       MOVE MFS-RENSA-FAELT             TO MOD-IDCATNR-UT                 
034400     END-IF                                                               
034500                                                                          
034600     IF NYCKLAR-FEL                                                       
034700       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
034800       CALL WMEDKONV USING     MED-WMEDAREA                               
034900       MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                               
035000       PERFORM MFS-RENSA-MOD-FAELT-IN                                     
035100       PERFORM MFS-RENSA-MOD-FAELT-UT                                     
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 C-KONTROLLERA-INPUT  SECTION.                                            
035600     SKIP2                                                                
035700     MOVE JA  TO INDATA-SW                                                
035800     MOVE NEJ TO INPUT-FINNS-SW                                           
036210                                                                          
036220     MOVE +1 TO IX                                                        
036230     PERFORM UNTIL IX > +6  OR INPUT-FINNS                                
036231       IF MID-IDPARTGRP(IX) NOT = ALL '+'                                 
036235         MOVE JA TO INPUT-FINNS-SW                                        
036240       END-IF                                                             
036292                                                                          
036293       ADD +1 TO IX                                                       
036294     END-PERFORM                                                          
036295                                                                          
036296     IF MID-FLKATVAD NOT = ALL '+'                                        
036297       MOVE JA TO INPUT-FINNS-SW                                          
036298     END-IF                                                               
036300                                                                          
036400     MOVE +1 TO RAD                                                       
036500     PERFORM UNTIL RAD > +8  OR INPUT-FINNS                               
036600       SET INPUT-SAKNAS TO TRUE                                           
036700       IF MID-IDMODELL    (RAD)  NOT = ALL '+'                            
036800       OR MID-TIMODAAR-STA (RAD) NOT = ALL '+'                            
036900       OR MID-TIMODAAR-STO (RAD) NOT = ALL '+'                            
037000       OR MID-IDVARIANT (RAD)    NOT = ALL '+'                            
037100         SET INPUT-FINNS TO TRUE                                          
037200       END-IF                                                             
037300                                                                          
037400       ADD +1 TO RAD                                                      
037500     END-PERFORM                                                          
037600*****                                                                     
037700     IF INPUT-SAKNAS                                                      
037800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
037900       CALL WMEDKONV USING MED-WMEDAREA                                   
038000       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
038100                                                                          
038200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
038300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
038400       MOVE NEJ TO INDATA-SW                                              
038500     ELSE                                                                 
038600*      PREPARERA ATT LÄSA IN ALLT DATA IGEN                               
038700       PERFORM MFS-LAESIN-MODIF-DATA-IGEN                                 
038800* ---------------                                                         
039810       MOVE +1 TO IX                                                      
039820       PERFORM UNTIL IX > +6                                              
039830                                                                          
039840         IF MID-IDPARTGRP(IX) NOT = ALL '+'                               
039850           MOVE MID-IDPARTGRP(IX) TO WS-IDPARTGRP                         
039851                                     WS-PARTNER-GRP(IX)                   
039894         ELSE                                                             
039895           MOVE MOD-IDPARTGRP(IX) TO WS-IDPARTGRP                         
039896                                     WS-PARTNER-GRP(IX)                   
039897         END-IF                                                           
039898                                                                          
039899         IF IDPARTGRP-OK                                                  
039900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPARTGRP-ATTR(IX)            
039901           MOVE WS-IDPARTGRP         TO MOD-IDPARTGRP(IX)                 
039902         ELSE                                                             
039903           MOVE NEJ TO INDATA-SW                                          
039904           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPARTGRP-ATTR(IX)            
039905           MOVE WS-IDPARTGRP         TO MOD-IDPARTGRP(IX)                 
039906         END-IF                                                           
039907                                                                          
039908         ADD +1 TO IX                                                     
039909       END-PERFORM                                                        
039910* ---------------                                                         
039911* KONTROLLERA KOMBINATIONERNA                                             
039913       IF WS-PARTNER-GRP (1) = SPACE                                      
039914         MOVE +2 TO IX                                                    
039915         PERFORM UNTIL IX > +6                                            
039916                                                                          
039917           IF WS-PARTNER-GRP (IX) NOT = SPACE                             
039918               MOVE NEJ TO INDATA-SW                                      
039919               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPARTGRP-ATTR(IX)          
039921           END-IF                                                         
039922                                                                          
039923           ADD +1 TO IX                                                   
039924         END-PERFORM                                                      
039925       ELSE                                                               
039926         MOVE +1 TO IX                                                    
039927         MOVE +2 TO IX2                                                   
039928         PERFORM UNTIL IX > +6                                            
039930           PERFORM UNTIL IX2 > +6                                         
039931                                                                          
039933             IF WS-PARTNER-GRP (IX) = SPACE                               
039934             OR WS-PARTNER-GRP (IX2) = SPACE                              
039935               CONTINUE                                                   
039936             ELSE                                                         
039937               IF WS-PARTNER-GRP (IX) = WS-PARTNER-GRP (IX2)              
039938                  MOVE NEJ TO INDATA-SW                                   
039939                  MOVE MFS-ALFA-FAELT-FEL                                 
039940                               TO MOD-IDPARTGRP-ATTR(IX)                  
039941                                  MOD-IDPARTGRP-ATTR(IX2)                 
039942               END-IF                                                     
039943             END-IF                                                       
039944                                                                          
039945             ADD +1          TO IX2                                       
039946           END-PERFORM                                                    
039947           ADD +1            TO IX                                        
039948           MOVE IX           TO IX2                                       
039949           ADD +1            TO IX2                                       
039950         END-PERFORM                                                      
039952       END-IF                                                             
039953* ---------------                                                         
039954       IF MID-FLKATVAD NOT = ALL '+'                                      
039955         IF MID-FLKATVAD = 'J'                                            
039956         OR MID-FLKATVAD = 'N'                                            
039957           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKATVAD-ATTR                 
039958           MOVE MID-FLKATVAD         TO MOD-FLKATVAD                      
039959         ELSE                                                             
039960           MOVE NEJ TO INDATA-SW                                          
039961           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKATVAD-ATTR                 
039962           MOVE MID-FLKATVAD         TO MOD-FLKATVAD                      
039963         END-IF                                                           
039964       END-IF                                                             
039970* ---------------                                                         
040000       MOVE +1 TO RAD                                                     
040100       PERFORM UNTIL RAD > +8                                             
040200                                                                          
040300         IF MID-IDMODELL     (RAD) NOT = ALL '+'                          
040400         OR MID-TIMODAAR-STA (RAD) NOT = ALL '+'                          
040500         OR MID-TIMODAAR-STO (RAD) NOT = ALL '+'                          
040600         OR MID-IDVARIANT    (RAD) NOT = ALL '+'                          
040700           PERFORM CA-KOLLA-RADINPUT-FORMELLT                             
040800         END-IF                                                           
040900                                                                          
041000         ADD +1 TO RAD                                                    
041100       END-PERFORM                                                        
041200                                                                          
041300       IF INDATA-FEL                                                      
041400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
041500         CALL WMEDKONV USING            MED-WMEDAREA                      
041600         MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                      
042000       ELSE                                                               
042100*                   INDATA ÄR RÄTT HITTILLS,                              
042200*                   FLYTTA WDN101 TILL TEST-AREAN                         
042300         PERFORM IMS-GET-KATM01                                           
042400         MOVE KATM-KAT-WDN101 TO TEST-KAT-WDN101                          
042500                                                                          
042600*                   FLYTTA FORMELLT GODKÄNT INDATA TILL TEST-AREAN        
042610         MOVE +1 TO IX                                                    
042620         PERFORM UNTIL IX > +6                                            
042621           MOVE WS-PARTNER-GRP(IX) TO TEST-KAT-IDPARTGRP(IX)              
042699           ADD +1 TO IX                                                   
042700         END-PERFORM                                                      
042701                                                                          
042710         IF MID-FLKATVAD     NOT = ALL '+'                                
042720            MOVE MID-FLKATVAD TO TEST-KAT-FLKATVAD                        
042730         END-IF                                                           
042800                                                                          
043000         MOVE 1 TO RAD                                                    
043100         PERFORM UNTIL RAD > +8                                           
043200         OR INDATA-FEL                                                    
043300           IF MID-IDMODELL   (RAD) NOT = ALL '+'                          
043400           OR MID-TIMODAAR-STA (RAD) NOT = ALL '+'                        
043500           OR MID-TIMODAAR-STO (RAD) NOT = ALL '+'                        
043600           OR MID-IDVARIANT  (RAD) NOT = ALL '+'                          
043700*                           INPUT-FINNS PÅ RADEN                          
043800*                           JÄMFÖR MOT TEST-AREAN, OCH FLYTTA DIT         
043900             PERFORM CB-KOLLA-RADDATA-KOPPLAT                             
044000             IF KOMBINATION-OK                                            
044100             AND INTERVALL-OK                                             
044200*                           KOLLA ATT VÄRDENA ÄR RÄTT I TEST-AREAN        
044300*                           MOT 1212   1214   1216                        
044400               PERFORM CC-KOLLA-RAD-MOT-12XX-BAS                          
044500             END-IF                                                       
044600           END-IF                                                         
044700           ADD +1 TO RAD                                                  
044800         END-PERFORM                                                      
044900                                                                          
045000         IF INDATA-FEL                                                    
045100           CALL WMEDKONV USING  MED-WMEDAREA                              
045200           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
045500         END-IF                                                           
045600       END-IF                                                             
045700     END-IF                                                               
045800     .                                                                    
045900     EJECT                                                                
046000 CA-KOLLA-RADINPUT-FORMELLT  SECTION.                                     
046100     SKIP2                                                                
046200     IF MID-IDMODELL(RAD) (1:1) = '/'                                     
046300*    ----------------- RENSNINGS-TECKEN FÖR HELA RADEN I "MODELL"         
046400       MOVE SPACE TO MID-IDMODELL (RAD)                                   
046500     END-IF                                                               
046600                                                                          
046700     IF MID-IDMODELL(RAD) (1:1) = '='                                     
046800*    ----------------- KOPIERAR FÖREGÅENDE RADS INMATADE MODELL           
046900       IF SPAR-IDMODELL NUMERIC                                           
047000         MOVE SPAR-IDMODELL TO MID-IDMODELL (RAD)                         
047100       END-IF                                                             
047200     END-IF                                                               
047300*    -----------------                                                    
047400     IF MID-IDMODELL (RAD) NOT = ALL '+'                                  
047500       IF MID-IDMODELL (RAD) = SPACE                                      
047600       OR MID-IDMODELL (RAD) NUMERIC                                      
047700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMODELL-ATTR (RAD)             
047800       ELSE                                                               
047900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDMODELL-ATTR (RAD)             
048000         MOVE NEJ TO INDATA-SW                                            
048100       END-IF                                                             
048200       MOVE MID-IDMODELL (RAD) TO SPAR-IDMODELL                           
048300     END-IF                                                               
048400*    -----------------                                                    
048500     IF MID-TIMODAAR-STA (RAD) (1:1) = '='                                
048600       MOVE SPAR-TIMODAAR-STA TO MID-TIMODAAR-STA (RAD)                   
048700     END-IF                                                               
048800     IF MID-TIMODAAR-STA (RAD) NOT = ALL '+'                              
048900       IF MID-TIMODAAR-STA (RAD) = SPACE                                  
049000                                OR '000 ' OR '00  ' OR '0   '             
049100         MOVE '0000' TO MID-TIMODAAR-STA (RAD)                            
049200       END-IF                                                             
049300       IF MID-TIMODAAR-STA (RAD) NUMERIC                                  
049400       AND (MID-TIMODAAR-STA(RAD) = ZERO   OR                             
049500           (MID-TIMODAAR-STA(RAD) >= 1974 AND <= MAX-AAR))                
049600         MOVE MFS-NUM-FAELT-RAETT   TO MOD-TIMODAAR-STA-ATTR (RAD)        
049700         MOVE MID-TIMODAAR-STA(RAD) TO SPAR-TIMODAAR-STA                  
049800       ELSE                                                               
049900         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STA-ATTR (RAD)          
050000         MOVE NEJ TO INDATA-SW                                            
050100       END-IF                                                             
050200     END-IF                                                               
050300*    -----------------                                                    
050400     IF MID-TIMODAAR-STO (RAD) (1:1) = '='                                
050500       MOVE SPAR-TIMODAAR-STO TO MID-TIMODAAR-STO (RAD)                   
050600     END-IF                                                               
050700     IF MID-TIMODAAR-STO (RAD) NOT = ALL '+'                              
050800       IF MID-TIMODAAR-STO (RAD) = SPACE                                  
050900                                OR '000 ' OR '00  ' OR '0   '             
051000         MOVE '0000' TO MID-TIMODAAR-STO (RAD)                            
051100       END-IF                                                             
051200       IF MID-TIMODAAR-STO (RAD) NUMERIC                                  
051300       AND (MID-TIMODAAR-STO(RAD) = ZERO   OR                             
051400           (MID-TIMODAAR-STO(RAD) >= 1974 AND <= MAX-AAR))                
051500         MOVE MFS-NUM-FAELT-RAETT   TO MOD-TIMODAAR-STO-ATTR (RAD)        
051600         MOVE MID-TIMODAAR-STO(RAD) TO SPAR-TIMODAAR-STO                  
051700       ELSE                                                               
051800         MOVE MFS-NUM-FAELT-FEL   TO MOD-TIMODAAR-STO-ATTR (RAD)          
051900         MOVE NEJ TO INDATA-SW                                            
052000       END-IF                                                             
052100     END-IF                                                               
052200*    -----------------                                                    
052300     IF MID-IDVARIANT (RAD) (1:1) = '='                                   
052400       MOVE SPAR-IDVARIANT TO MID-IDVARIANT (RAD)                         
052500     END-IF                                                               
052600     IF MID-IDVARIANT (RAD) NOT = ALL '+'                                 
052700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARIANT-ATTR (RAD)              
052800       MOVE MID-IDVARIANT (RAD) TO SPAR-IDVARIANT                         
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 CB-KOLLA-RADDATA-KOPPLAT    SECTION.                                     
053300     SKIP2                                                                
053400     MOVE JA  TO KOMBINATION-SW,   INTERVALL-SW                           
053500     MOVE NEJ TO RAD-RENSAD                                               
053600*      --- MIXA IHOP NYINMATAT DATA MED BEFINTLIGT OCH KOLLA              
053700*      --- KATM01 ÄR INKOPIERAD TILL TEST-AREA I C- SECTION               
053800                                                                          
053900*FLYTTA MODELL                                                            
054000     IF MID-IDMODELL(RAD) NOT = ALL '+'                                   
054100       IF  MID-IDMODELL(RAD) = SPACE                                      
054200       AND MID-IDRADNR (RAD) > ZERO                                       
054300*        EN RENSAD RAD                                                    
054400         MOVE SPACE  TO TEST-KAT-IDMODELL(RAD)                            
054500         MOVE ZERO   TO TEST-KAT-TIMODAAR-STA(RAD)                        
054600         MOVE ZERO   TO TEST-KAT-TIMODAAR-STO(RAD)                        
054700         MOVE SPACE  TO TEST-KAT-IDVARIANT(RAD)                           
054800         MOVE JA TO RAD-RENSAD                                            
054900       ELSE                                                               
055000         MOVE MID-IDMODELL(RAD) TO TEST-KAT-IDMODELL (RAD)                
055100       END-IF                                                             
055200     END-IF                                                               
055300                                                                          
055400     IF RAD-RENSAD = NEJ                                                  
055500*FLYTTA STARTÅR                                                           
055610       IF MID-TIMODAAR-STA(RAD) NOT = ALL '+'                             
055700         INSPECT MID-TIMODAAR-STA(RAD) REPLACING ALL SPACE BY ZERO        
055800                                       ALL LOW-VALUE BY ZERO              
055900         MOVE MID-TIMODAAR-STA(RAD) TO TEST-KAT-TIMODAAR-STA(RAD)         
056000       ELSE                                                               
056100         CONTINUE                                                         
056200*        DETTA TAS OM HAND I CC- SECTION                                  
056300       END-IF                                                             
056400                                                                          
056500*FLYTTA   STOPPÅR                                                         
056600       IF MID-TIMODAAR-STO(RAD) NOT = ALL '+'                             
056700         INSPECT MID-TIMODAAR-STO(RAD) REPLACING ALL SPACE BY ZERO        
056800                                       ALL LOW-VALUE BY ZERO              
056900         MOVE MID-TIMODAAR-STO(RAD) TO TEST-KAT-TIMODAAR-STO(RAD)         
057000       ELSE                                                               
057100         CONTINUE                                                         
057200*        DETTA TAS OM HAND I CC- SECTION                                  
057300       END-IF                                                             
057400                                                                          
057500*FLYTTA   VARIANT                                                         
057610       IF MID-IDVARIANT(RAD) NOT = ALL '+'                                
057700         MOVE MID-IDVARIANT(RAD) TO TEST-KAT-IDVARIANT(RAD)               
057800       END-IF                                                             
057900     END-IF                                                               
058000                                                                          
058100     IF TEST-KAT-TIMODAAR-STA(RAD) > ZERO                                 
058200     AND TEST-KAT-TIMODAAR-STO(RAD) > ZERO                                
058300       IF TEST-KAT-TIMODAAR-STA(RAD) > TEST-KAT-TIMODAAR-STO(RAD)         
058400         MOVE NEJ TO INTERVALL-SW                                         
058500         MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (RAD)            
058600                                   MOD-TIMODAAR-STO-ATTR (RAD)            
058610         MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STA(RAD)                  
058620                                   MOD-TIMODAAR-STO(RAD)                  
058700       END-IF                                                             
058800     END-IF                                                               
058900                                                                          
059000     IF KOMBINATION-FEL                                                   
059200       MOVE ERR-INVALID-COMBINATION TO MED-IDMFSFEL                       
059300       MOVE NEJ TO INDATA-SW                                              
059400     ELSE                                                                 
059500       IF INTERVALL-FEL                                                   
059700         MOVE ERR-FOM-STOERRE-TOM     TO MED-IDMFSFEL                     
059800         MOVE NEJ TO INDATA-SW                                            
059900       END-IF                                                             
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300 CC-KOLLA-RAD-MOT-12XX-BAS        SECTION.                                
060400     SKIP2                                                                
060500     IF RAD-RENSAD = NEJ                                                  
060600       MOVE TEST-KAT-IDMODELL(RAD) TO W-1212-IDMODELL                     
060700                                      W-1215-IDMODELL                     
060800* KOLLA MODELLENS START OCH STOPPÅR                                       
060900       PERFORM IMS-GET-1212-MODELL                                        
061000       IF SEGMENT-FINNS                                                   
061100         IF 1212-TIMODAAR-STA = ZEROS                                     
061200           MOVE 1975 TO 1212-TIMODAAR-STA                                 
061300*          TIDIGASTE STARTÅR FÖR ATT FÅ RIKTIGT INTERVALL                 
061400         END-IF                                                           
061500         IF 1212-TIMODAAR-STO = ZEROS                                     
061600           MOVE MAX-AAR TO 1212-TIMODAAR-STO                              
061700*          STÖRSTA TILLÅTNA SLUTÅR FÖR ATT FÅ RIKTIGT INTERVALL           
061800         END-IF                                                           
061900                                                                          
062000* KOLLA OM MAN BARA GIVIT MODELL UTAN ÅRTAL.                              
062100         IF  MID-IDMODELL (RAD) NOT = ALL '+'                             
062200           IF MID-TIMODAAR-STA (RAD) = '0000' OR '++++'                   
062300             IF TEST-KAT-TIMODAAR-STA(RAD) = ZERO                         
062400*              LÄGG IN DEFAULT-ÅRTAL FRÅN 1212                            
062500               MOVE 1212-TIMODAAR-STA                                     
062600                                  TO TEST-KAT-TIMODAAR-STA(RAD)           
062700             END-IF                                                       
062800           END-IF                                                         
062900           IF MID-TIMODAAR-STO (RAD) = '0000' OR '++++'                   
063000             IF TEST-KAT-TIMODAAR-STO(RAD) = ZERO                         
063100*              LÄGG IN DEFAULT-ÅRTAL FRÅN 1212                            
063200               MOVE 1212-TIMODAAR-STO                                     
063300                                  TO TEST-KAT-TIMODAAR-STO(RAD)           
063400             END-IF                                                       
063500           END-IF                                                         
063600         END-IF                                                           
063700                                                                          
063800* KOLLA STARTÅRET INOM TILLÅTET INTERVALL                                 
063900         IF TEST-KAT-TIMODAAR-STA(RAD) NOT = ZERO                         
064000         AND (TEST-KAT-TIMODAAR-STA(RAD) < 1212-TIMODAAR-STA              
064100                                     OR  > 1212-TIMODAAR-STO)             
064200           MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STA-ATTR (RAD)          
064210           MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STA(RAD)                
064300           MOVE NEJ TO INDATA-SW                                          
064400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064500         END-IF                                                           
064600* KOLLA STOPPÅRET INOM TILLÅTET INTERVALL                                 
064700         IF TEST-KAT-TIMODAAR-STO(RAD) NOT = ZERO                         
064800         AND (TEST-KAT-TIMODAAR-STO(RAD) < 1212-TIMODAAR-STA              
064900                                     OR  > 1212-TIMODAAR-STO)             
065000           MOVE MFS-NUM-FAELT-FEL TO MOD-TIMODAAR-STO-ATTR (RAD)          
065010           MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STO(RAD)                
065100           MOVE NEJ TO INDATA-SW                                          
065200           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
065300         END-IF                                                           
065400                                                                          
065500* KOLLA VARIANTEN OCH DESS TILLÅTNA START OCH STOPPÅR                     
065600         IF INDATA-OK                                                     
065700         IF TEST-KAT-IDVARIANT(RAD) NOT = SPACE                           
065800           MOVE TEST-KAT-IDVARIANT(RAD) TO W-1214-IDVARIANT               
065900                                           W-1216-IDVARIANT               
066000           PERFORM IMS-GET-1214-VARIANT                                   
066100           IF SEGMENT-FINNS                                               
066200             IF 1214-TIVARAAR-STA = ZERO                                  
066210               MOVE 1212-TIMODAAR-STA TO 1214-TIVARAAR-STA                
066400*              TIDIGASTE STARTÅR FÖR ATT FÅ RIKTIGT INTERVALL             
066500             END-IF                                                       
066600             IF 1214-TIVARAAR-STO = ZERO                                  
066610               MOVE 1212-TIMODAAR-STO TO 1214-TIVARAAR-STO                
066800*              STÖRSTA TILLÅTNA SLUTÅR FÖR ATT FÅ RIKTIGT INTERV          
066900             END-IF                                                       
067000* KOLLA VARIANT-STARTÅR, INOM TILLÅTET INTERVALL                          
067100             IF TEST-KAT-TIMODAAR-STA(RAD) NOT = ZERO                     
067200             AND (TEST-KAT-TIMODAAR-STA(RAD)                              
067300                                         < 1214-TIVARAAR-STA              
067400                                      OR > 1214-TIVARAAR-STO)             
067500               MOVE MFS-NUM-FAELT-FEL                                     
067600                                    TO MOD-TIMODAAR-STA-ATTR(RAD)         
067610               MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STA(RAD)            
067700               MOVE NEJ TO INDATA-SW                                      
067800               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
067900             END-IF                                                       
068000* KOLLA VARIANT STOPPÅR, INOM TILLÅTET INTERVALL                          
068100             IF TEST-KAT-TIMODAAR-STO(RAD) NOT = ZERO                     
068200             AND (TEST-KAT-TIMODAAR-STO(RAD)                              
068300                                         < 1214-TIVARAAR-STA              
068400                                      OR > 1214-TIVARAAR-STO)             
068500               MOVE MFS-NUM-FAELT-FEL                                     
068600                                    TO MOD-TIMODAAR-STO-ATTR (RAD)        
068610               MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STO(RAD)            
068700               MOVE NEJ TO INDATA-SW                                      
068800               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
068900             END-IF                                                       
069000                                                                          
069100             IF TEST-KAT-IDMODELL(RAD) NOT = SPACE                        
069200* KOLLA ÅRSINTERVALLET FÖR VARIANTEN PÅ DENNA MODELL                      
069300               PERFORM IMS-GET-1216-MODELLVARIANT                         
069400                                                                          
069500               IF SEGMENT-FINNS                                           
069600*                TIDIGASTE STARTÅR FÖR ATT FÅ RIKTIGT INTERVALL           
069700                 IF 1216-TIMOVAAR-STA = ZERO                              
069810                   MOVE 1212-TIMODAAR-STA TO 1216-TIMOVAAR-STA            
069900                 END-IF                                                   
070000*                STÖRSTA TILLÅTNA SLUTÅR FÖR ATT FÅ RIKTIGT INTERV        
070100                 IF 1216-TIMOVAAR-STO = ZERO                              
070110                   MOVE 1212-TIMODAAR-STO TO 1216-TIMOVAAR-STO            
070300                 END-IF                                                   
070400* KOLLA MODELL/VARIANT STARTÅR, INOM TILLÅTET INTERVALL                   
070500                 IF TEST-KAT-TIMODAAR-STA(RAD) NOT = ZERO                 
070600                 AND (TEST-KAT-TIMODAAR-STA(RAD)                          
070700                                         < 1216-TIMOVAAR-STA              
070800                                      OR > 1216-TIMOVAAR-STO)             
070900                   MOVE MFS-NUM-FAELT-FEL TO                              
071000                                       MOD-TIMODAAR-STA-ATTR (RAD)        
071100                   MOVE MFS-ALFA-FAELT-FEL                                
071200                                       TO MOD-IDMODELL-ATTR (RAD)         
071300                                          MOD-IDVARIANT-ATTR (RAD)        
071301                   MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STA(RAD)        
071302                                             MOD-IDMODELL(RAD)            
071303                                             MOD-IDVARIANT(RAD)           
071400                   MOVE NEJ TO INDATA-SW                                  
071500                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
071600                 END-IF                                                   
071700* KOLLA MODELL/VARIANT STOPPÅR, INOM TILLÅTET INTERVALL                   
071800                 IF TEST-KAT-TIMODAAR-STO(RAD) NOT = ZERO                 
071900                 AND (TEST-KAT-TIMODAAR-STO(RAD)                          
072000                                         < 1216-TIMOVAAR-STA              
072100                                      OR > 1216-TIMOVAAR-STO)             
072200                   MOVE MFS-NUM-FAELT-FEL TO                              
072300                                       MOD-TIMODAAR-STO-ATTR (RAD)        
072400                   MOVE MFS-ALFA-FAELT-FEL                                
072500                                       TO MOD-IDMODELL-ATTR (RAD)         
072600                                          MOD-IDVARIANT-ATTR (RAD)        
072610                   MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STO(RAD)        
072620                                             MOD-IDMODELL(RAD)            
072630                                             MOD-IDVARIANT(RAD)           
072700                   MOVE NEJ TO INDATA-SW                                  
072800                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
072900                 END-IF                                                   
073000               ELSE                                                       
073100* KOMBINATIONEN MODELL OCH VARIANT FINNS EJ                               
073200                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (RAD)        
073300                                          MOD-IDVARIANT-ATTR (RAD)        
073320                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDMODELL(RAD)              
073330                                           MOD-IDVARIANT(RAD)             
073400                 MOVE NEJ TO INDATA-SW                                    
073500                 MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                  
073600               END-IF                                                     
073700             END-IF                                                       
073800           ELSE                                                           
073900             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDVARIANT-ATTR (RAD)          
073910             MOVE MFS-ROER-EJ-FAELT  TO MOD-IDVARIANT(RAD)                
074000             MOVE NEJ TO INDATA-SW                                        
074100             MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                      
074200           END-IF                                                         
074300         END-IF                                                           
074400         END-IF                                                           
074500       ELSE                                                               
074600* MODELLEN FINNS EJ PÅ WDGX1212-BASEN                                     
074700         MOVE NEJ TO INDATA-SW                                            
074800         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMODELL-ATTR (RAD)               
074810         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDMODELL (RAD)                    
074900         MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                          
075000       END-IF                                                             
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 D-UPPDATERA SECTION.                                                     
075500     SKIP2                                                                
075600     MOVE JA TO ALLT-SW                                                   
075710     MOVE +1 TO IX                                                        
075720     PERFORM UNTIL IX > +6                                                
075730       MOVE TEST-KAT-IDPARTGRP(IX)                                        
075740                             TO KATM-KAT-IDPARTGRP(IX)                    
075797       ADD +1 TO IX                                                       
075798     END-PERFORM                                                          
075799                                                                          
075800     MOVE TEST-KAT-FLKATVAD  TO KATM-KAT-FLKATVAD                         
075802                                                                          
075810     MOVE +1 TO RAD                                                       
075900     MOVE +1 TO IX                                                        
076000     PERFORM UNTIL RAD  > +8                                              
076100*      DET FINNS ÅTTA RADER                                               
076200*      OM TEST-KAT-IDMODELL ÄR BLANK SKALL DENNA RAD EJ UPPD.             
076300       IF TEST-KAT-IDMODELL (RAD) = SPACE                                 
076400         CONTINUE                                                         
076500       ELSE                                                               
076600         MOVE TEST-KAT-IDMODELL(RAD) TO KATM-KAT-IDMODELL(IX)             
076700         MOVE TEST-KAT-TIMODAAR-STA(RAD)                                  
076800                                      TO KATM-KAT-TIMODAAR-STA(IX)        
076900         MOVE TEST-KAT-TIMODAAR-STO(RAD)                                  
077000                                      TO KATM-KAT-TIMODAAR-STO(IX)        
077100         MOVE TEST-KAT-IDVARIANT(RAD) TO KATM-KAT-IDVARIANT(IX)           
077200         ADD +1 TO IX                                                     
077300       END-IF                                                             
077400       ADD +1 TO RAD                                                      
077500     END-PERFORM                                                          
077600                                                                          
077700     PERFORM UNTIL IX > +8                                                
077800       MOVE SPACE TO KATM-KAT-IDMODELL(IX)                                
077900       MOVE ZERO  TO KATM-KAT-TIMODAAR-STA(IX)                            
078000       MOVE ZERO  TO KATM-KAT-TIMODAAR-STO(IX)                            
078100       MOVE SPACE TO KATM-KAT-IDVARIANT(IX)                               
078200       ADD +1 TO IX                                                       
078300     END-PERFORM                                                          
078400                                                                          
078500     PERFORM IMS-REPL-KATM                                                
078600                                                                          
078700     MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                             
078800     CALL WMEDKONV USING         MED-WMEDAREA                             
078900     MOVE MED-TEMFSINF        TO MOD-TEMFSINF                             
079000                                                                          
079100     PERFORM MFS-FORM-ATTR                                                
079200     PERFORM MFS-RENSA-MOD-FAELT-IN                                       
079300     .                                                                    
079400     EJECT                                                                
079500 E-ENTER-TRYCKNING SECTION.                                               
079600     SKIP2                                                                
079700     IF EGEN-MID OR HELP-MID                                              
079800* * * * * * KOLLA OM MAN GIVIT NY NYCKEL VID ENTER                        
079801       IF MID-IDCATNR-IN = ALL '+'                                        
079810* * * * * * KOLLA OM MAN GIVIT INPUT VID ENTER                            
079900         SET INPUT-SAKNAS TO TRUE                                         
080000                                                                          
080400                                                                          
080500         MOVE +1 TO IX                                                    
080600         PERFORM UNTIL IX > +6  OR INPUT-FINNS                            
080700           IF MID-IDPARTGRP(IX) NOT = ALL '+'                             
080800             SET INPUT-FINNS TO TRUE                                      
080900           END-IF                                                         
081400           ADD +1 TO IX                                                   
081500         END-PERFORM                                                      
081510                                                                          
081520         MOVE +1 TO RAD                                                   
081530         PERFORM UNTIL RAD > +8  OR INPUT-FINNS                           
081540*                      KOLLA-RADINMATNING                                 
081550           IF MID-IDMODELL (RAD) NOT = ALL '+'                            
081560           OR MID-TIMODAAR-STA (RAD) NOT = ALL '+'                        
081570           OR MID-TIMODAAR-STO (RAD) NOT = ALL '+'                        
081580           OR MID-IDVARIANT (RAD) NOT = ALL '+'                           
081590             SET INPUT-FINNS TO TRUE                                      
081591           END-IF                                                         
081592           ADD +1 TO RAD                                                  
081593         END-PERFORM                                                      
081600                                                                          
081700         IF INPUT-FINNS                                                   
081800           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
081900           CALL WMEDKONV USING MED-WMEDAREA                               
082000           MOVE MED-TEMFSINF TO MOD-TEMFSFEL                              
082100           MOVE NEJ TO INDATA-SW                                          
082200                                                                          
082300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
082400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
082500           PERFORM MFS-LAESIN-MODIF-DATA-IGEN                             
082600         END-IF                                                           
082610       END-IF                                                             
082700     END-IF                                                               
082800     .                                                                    
082900     EJECT                                                                
083000 F-LAES-BAS-VISA-INFO SECTION.                                            
083100     SKIP2                                                                
083300     PERFORM IMS-GET-KATM01                                               
083400     IF SEGMENT-FINNS                                                     
083410       IF INDATA-OK                                                       
083500         MOVE KATM-KAT-BECAT-RAD1 TO MOD-BECAT-RAD1                       
083600         MOVE KATM-KAT-TIREGDAT   TO MOD-TIREGDAT-KAT                     
083700         MOVE KATM-KAT-BEMASTER   TO MOD-BEMASTER                         
083800         MOVE KATM-KAT-BEEMBLEM   TO MOD-BEEMBLEM                         
083900         MOVE KATM-KAT-FLKATVAD   TO MOD-FLKATVAD                         
084000                                                                          
084100         MOVE +1 TO IX                                                    
084200         PERFORM UNTIL IX > +6                                            
084201           MOVE KATM-KAT-IDPARTGRP(IX)                                    
084202                                  TO MOD-IDPARTGRP(IX)                    
084203           ADD +1 TO IX                                                   
084204         END-PERFORM                                                      
084210                                                                          
084220         MOVE +1 TO RAD                                                   
084230         PERFORM UNTIL RAD > +8                                           
084300                                                                          
084400           IF KATM-KAT-IDMODELL(RAD) > SPACE                              
084500           AND KATM-KAT-IDMODELL(RAD) NUMERIC                             
084600             MOVE KATM-KAT-IDMODELL(RAD) TO MOD-IDMODELL(RAD)             
084700             MOVE RAD                  TO MOD-IDRADNR(RAD)                
084800                                                                          
084900             IF KATM-KAT-TIMODAAR-STA(RAD) > ZERO                         
085000               MOVE KATM-KAT-TIMODAAR-STA(RAD)                            
085100                                         TO MOD-TIMODAAR-STA(RAD)         
085200             ELSE                                                         
085300               MOVE MFS-RENSA-FAELT  TO MOD-TIMODAAR-STA(RAD)             
085400             END-IF                                                       
085500                                                                          
085600             IF KATM-KAT-TIMODAAR-STO(RAD) > ZERO                         
085700               MOVE KATM-KAT-TIMODAAR-STO(RAD)                            
085800                                         TO MOD-TIMODAAR-STO(RAD)         
085900             ELSE                                                         
086000               MOVE MFS-RENSA-FAELT  TO MOD-TIMODAAR-STO(RAD)             
086100             END-IF                                                       
086200                                                                          
086300             IF KATM-KAT-IDVARIANT(RAD) > SPACE                           
086400               MOVE KATM-KAT-IDVARIANT(RAD) TO MOD-IDVARIANT(RAD)         
086500             ELSE                                                         
086600               MOVE MFS-RENSA-FAELT  TO MOD-IDVARIANT(RAD)                
086700             END-IF                                                       
086800           ELSE                                                           
086900             MOVE MFS-RENSA-FAELT    TO MOD-IDMODELL(RAD)                 
087000             MOVE MFS-RENSA-FAELT    TO MOD-TIMODAAR-STA(RAD)             
087100             MOVE MFS-RENSA-FAELT    TO MOD-TIMODAAR-STO(RAD)             
087200             MOVE MFS-RENSA-FAELT    TO MOD-IDVARIANT(RAD)                
087300             MOVE ZEROS              TO MOD-IDRADNR(RAD)                  
087400           END-IF                                                         
087500                                                                          
087600           ADD +1 TO RAD                                                  
087700         END-PERFORM                                                      
087701                                                                          
087703*******      HÄMTA PUBKOD FÖR NÄSTA GENERERING TILL VADIS                 
087704                                                                          
087705         MOVE 'AAMMDD'       TO DAT-KDDATFORM                             
087706         MOVE DAGENS-DATUM TO DAT-I-TIDATUM                               
087707                                                                          
087708         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
087709                         DAT-O-TIDATUM DAT-KDSVAR                         
087710                                                                          
087711         IF DAT-KDSVAR-OK                                                 
087713             MOVE DAT-TISEKEL  TO WS-KDCATPUB (1:2)                       
087714             MOVE DAT-TIAAVVD(1:4)                                        
087715                               TO WS-KDCATPUB (3:4)                       
087716         ELSE                                                             
087717             STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS            
087718             DELIMITED BY SIZE INTO FELTEXT                               
087719             CALL FELLOG                                                  
087720         END-IF                                                           
087721*  LÄS VADIS-GENERERINGSTABELL OCH LÄGG UT NÄRMASTE PUBKOD                
087722         MOVE DAGENS-AAR-LONG  TO W-TIAAAA                                
087723         PERFORM IMS-GET-KATM11                                           
087724         IF SEGMENT-FINNS                                                 
087730           MOVE 1              TO IX                                      
087740           PERFORM UNTIL IX > 12                                          
087750           OR        (KATM-TAB-KDCATPUB-FOM (IX) >= WS-KDCATPUB           
087751           AND        KATM-TAB-FLVADGEN (IX) = 'J')                       
087760              ADD 1            TO IX                                      
087770           END-PERFORM                                                    
087780           IF IX > 12                                                     
087790              ADD 1            TO W-TIAAAA                                
087791              PERFORM IMS-GET-KATM11                                      
087792              MOVE 1              TO IX                                   
087793              PERFORM UNTIL IX > 12                                       
087794              OR    (KATM-TAB-KDCATPUB-FOM (IX) >= WS-KDCATPUB            
087795              AND    KATM-TAB-FLVADGEN (IX) = 'J')                        
087797                 ADD 1            TO IX                                   
087798              END-PERFORM                                                 
087799              IF IX > 12                                                  
087800                 MOVE SPACE    TO MOD-KDCATPUB-R                          
087803              ELSE                                                        
087804                 MOVE KATM-TAB-KDCATPUB-FOM (IX) (4:3)                    
087806                                 TO MOD-KDCATPUB-R                        
087807              END-IF                                                      
087808           ELSE                                                           
087809              MOVE KATM-TAB-KDCATPUB-FOM (IX) (4:3)                       
087810                                 TO MOD-KDCATPUB-R                        
087813           END-IF                                                         
087814         ELSE                                                             
087815*          TABELL FANNS INTE UPPDATERAD                                   
087816                                                                          
087817           MOVE SPACE          TO MOD-KDCATPUB-R                          
087818           MOVE SPECIAL-INF-1(SPRAK-IX)  TO MOD-TEMFSINF                  
087819         END-IF                                                           
087820       ELSE                                                               
087821         MOVE +1 TO IX                                                    
087822         PERFORM UNTIL IX > +6                                            
087823           IF MID-IDPARTGRP(IX) = ALL '+'                                 
087824             MOVE MFS-ROER-EJ-FAELT TO MOD-IDPARTGRP(IX)                  
087830           END-IF                                                         
087834           ADD +1 TO IX                                                   
087835         END-PERFORM                                                      
087836                                                                          
087900         MOVE +1 TO RAD                                                   
088000         PERFORM UNTIL RAD > +8                                           
088010           IF MID-IDMODELL(RAD) = ALL '+'                                 
088011             MOVE MFS-ROER-EJ-FAELT TO MOD-IDMODELL(RAD)                  
088012           END-IF                                                         
088020           IF MID-TIMODAAR-STA(RAD) = ALL '+'                             
088021             MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STA(RAD)              
088022           END-IF                                                         
088030           IF MID-TIMODAAR-STO(RAD) = ALL '+'                             
088031             MOVE MFS-ROER-EJ-FAELT TO MOD-TIMODAAR-STO(RAD)              
088032           END-IF                                                         
088040           IF MID-IDVARIANT(RAD) = ALL '+'                                
088041             MOVE MFS-ROER-EJ-FAELT TO MOD-IDVARIANT(RAD)                 
088050           END-IF                                                         
088100                                                                          
088200           ADD +1 TO RAD                                                  
088210         END-PERFORM                                                      
088300       END-IF                                                             
088400     ELSE                                                                 
088410       MOVE ERR-NOT-REGISTERED       TO MED-IDMFSINF                      
088420       CALL WMEDKONV USING MED-WMEDAREA                                   
088430       MOVE MED-TEMFSINF             TO MOD-TEMFSFEL                      
088440       MOVE NEJ                      TO INDATA-SW                         
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 MFS-RENSA-MOD-FAELT-IN SECTION.                                          
089400     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
089401                             MOD-IDPARTGRP(1)                             
089402                             MOD-IDPARTGRP(2)                             
089403                             MOD-IDPARTGRP(3)                             
089404                             MOD-IDPARTGRP(4)                             
089405                             MOD-IDPARTGRP(5)                             
089406                             MOD-IDPARTGRP(6)                             
089410                             MOD-FLKATVAD                                 
089500                             MOD-IDRADNR(1)  MOD-IDRADNR(6)               
089600                             MOD-IDRADNR(2)  MOD-IDRADNR(7)               
089700                             MOD-IDRADNR(3)  MOD-IDRADNR(8)               
089800                             MOD-IDRADNR(4)                               
089900                             MOD-IDRADNR(5)                               
090000     .                                                                    
090100     SKIP3                                                                
090200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
090300     SKIP2                                                                
090400     MOVE MFS-ROER-EJ-FAELT TO WS-MFSKOD                                  
090500     PERFORM MFS-KOD-TILL-IN-FAELT                                        
090600     .                                                                    
090700     EJECT                                                                
090800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
090900     SKIP2                                                                
091000*    --- ALLA UTDATA-FÄLT                                                 
091100     MOVE MFS-ROER-EJ-FAELT TO WS-MFSKOD                                  
091200     PERFORM MFS-KOD-TILL-UT-FAELT                                        
091300                                                                          
091400     .                                                                    
091500     SKIP2                                                                
091600 MFS-RENSA-MOD-FAELT-UT SECTION.                                          
091700                                                                          
091800*    --- ALLA UTDATA-FÄLT                                                 
091900     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-UT                               
092000                             MOD-FLKATVAD                                 
092010                             MOD-KDCATPUB-R                               
092100                             MOD-BECAT-RAD1                               
092200                             MOD-TIREGDAT-KAT                             
092300                             MOD-BEMASTER                                 
092400                             MOD-BEEMBLEM                                 
092500     MOVE +1 TO IX                                                        
092600     PERFORM UNTIL IX > +6                                                
092700       MOVE MFS-RENSA-FAELT TO MOD-IDPARTGRP(IX)                          
093200       ADD +1 TO IX                                                       
093300     END-PERFORM                                                          
093301                                                                          
093310     MOVE +1 TO RAD                                                       
093320     PERFORM UNTIL RAD > +8                                               
093330       MOVE MFS-RENSA-FAELT TO MOD-IDMODELL(RAD)                          
093340                               MOD-TIMODAAR-STA(RAD)                      
093350                               MOD-TIMODAAR-STO(RAD)                      
093360                               MOD-IDVARIANT(RAD)                         
093370                               MOD-IDRADNR(RAD)                           
093380       ADD +1 TO RAD                                                      
093390     END-PERFORM                                                          
093400     .                                                                    
093500     EJECT                                                                
093600 MFS-FORM-ATTR SECTION.                                                   
093700                                                                          
093800*    --- ALLA INDATA-FÄLT                                                 
094000     MOVE +1 TO IX                                                        
094100     PERFORM UNTIL IX > +6                                                
094200       MOVE MFS-FORMATETS-ATTR   TO MOD-IDPARTGRP-ATTR(IX)                
094600       ADD +1 TO IX                                                       
094700     END-PERFORM                                                          
094701                                                                          
094702     MOVE MFS-FORMATETS-ATTR   TO MOD-FLKATVAD-ATTR                       
094703                                                                          
094710     MOVE +1 TO RAD                                                       
094720     PERFORM UNTIL RAD > +8                                               
094730       MOVE MFS-FORMATETS-ATTR TO MOD-IDMODELL-ATTR(RAD)                  
094740                                  MOD-TIMODAAR-STA-ATTR(RAD)              
094750                                  MOD-TIMODAAR-STO-ATTR(RAD)              
094760                                  MOD-IDVARIANT-ATTR(RAD)                 
094770       ADD +1 TO RAD                                                      
094780     END-PERFORM                                                          
094800     .                                                                    
094900     SKIP2                                                                
095000 MFS-LAESIN-MODIF-DATA-IGEN   SECTION.                                    
095100                                                                          
095200     MOVE MFS-ROER-EJ-FAELT  TO MOD-BECAT-RAD1                            
095300                                MOD-TIREGDAT-KAT                          
095400                                MOD-BEMASTER                              
095500                                MOD-BEEMBLEM                              
095510                                                                          
095520     MOVE +1 TO IX                                                        
095530     PERFORM UNTIL IX > +6                                                
095600       IF MID-IDPARTGRP(IX) NOT = ALL '+'                                 
095700         MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDPARTGRP-ATTR(IX)           
095800       ELSE                                                               
095900         MOVE MFS-ROER-EJ-FAELT       TO MOD-IDPARTGRP(IX)                
096000       END-IF                                                             
096001       ADD +1 TO IX                                                       
096010     END-PERFORM                                                          
096011                                                                          
096030     IF MID-FLKATVAD NOT = ALL '+'                                        
096040       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLKATVAD-ATTR                  
096050     ELSE                                                                 
096060       MOVE MFS-ROER-EJ-FAELT       TO MOD-FLKATVAD                       
096070     END-IF                                                               
096071                                                                          
096080     MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCATPUB-R                            
096100                                                                          
096200     MOVE +1 TO IX                                                        
096300     PERFORM UNTIL IX > +8                                                
096400       IF  MID-IDMODELL (IX) NOT = ALL '+'                                
096500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMODELL-ATTR (IX)             
096600       ELSE                                                               
096700         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDMODELL      (IX)             
096800       END-IF                                                             
096900       IF  MID-TIMODAAR-STA (IX) NOT = ALL '+'                            
097000         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIMODAAR-STA-ATTR (IX)         
097100       ELSE                                                               
097200         MOVE MFS-ROER-EJ-FAELT     TO MOD-TIMODAAR-STA      (IX)         
097300       END-IF                                                             
097400       IF  MID-TIMODAAR-STO (IX) NOT = ALL '+'                            
097500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIMODAAR-STO-ATTR (IX)         
097600       ELSE                                                               
097700         MOVE MFS-ROER-EJ-FAELT     TO MOD-TIMODAAR-STO      (IX)         
097800       END-IF                                                             
097900       IF  MID-IDVARIANT   (IX) NOT = ALL '+'                             
098000         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVARIANT-ATTR (IX)            
098100       ELSE                                                               
098200         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDVARIANT      (IX)            
098300       END-IF                                                             
098400       ADD +1 TO IX                                                       
098500     END-PERFORM                                                          
098600     .                                                                    
098700     EJECT                                                                
098800 MFS-KOD-TILL-IN-FAELT SECTION.                                           
098900     SKIP2                                                                
099000*    --- ALLA RENA INDATA-FÄLT                                            
099100     CONTINUE                                                             
099200     .                                                                    
099300     EJECT                                                                
099400 MFS-KOD-TILL-UT-FAELT SECTION.                                           
099500     SKIP2                                                                
099700     MOVE WS-MFSKOD  TO MOD-BECAT-RAD1                                    
099800                        MOD-TIREGDAT-KAT                                  
099900                        MOD-BEMASTER                                      
100000                        MOD-BEEMBLEM                                      
100010                        MOD-FLKATVAD                                      
100020                        MOD-KDCATPUB-R                                    
100100     MOVE 1 TO IX                                                         
100200     PERFORM UNTIL IX > +6                                                
100210       MOVE WS-MFSKOD  TO MOD-IDPARTGRP(IX)                               
100400       ADD 1 TO IX                                                        
100500     END-PERFORM                                                          
100501                                                                          
100510     MOVE 1 TO RAD                                                        
100520     PERFORM UNTIL RAD > +8                                               
100530       PERFORM MFS-KOD-TILL-RAD-DATA                                      
100540       ADD 1 TO RAD                                                       
100550     END-PERFORM                                                          
100600     .                                                                    
100700     EJECT                                                                
100800 MFS-KOD-TILL-RAD-DATA  SECTION.                                          
100900     SKIP2                                                                
101000     MOVE WS-MFSKOD  TO MOD-IDMODELL    (RAD)                             
101100                        MOD-TIMODAAR-STA(RAD)                             
101200                        MOD-TIMODAAR-STO(RAD)                             
101300                        MOD-IDVARIANT   (RAD)                             
101400                        MOD-IDRADNR     (RAD)                             
101500     .                                                                    
101700     EJECT                                                                
103300* --- IMS SEKTIONER ---                                                   
103400 IMS-GET-MSG SECTION.                                                     
103500                                                                          
103600     MOVE '  QC' TO GODK-STATUSKODER                                      
103700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
103800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103900     PERFORM IMS-STATUSKONTROLL                                           
104000     .                                                                    
104100     SKIP3                                                                
104200 IMS-INSERT-MSG SECTION.                                                  
104300                                                                          
104400     IF ENGLISH-TEXT                                                      
104500       MOVE 'N' TO MFS-KDHUVOMR                                           
104600     END-IF                                                               
104700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
104800     MOVE SPACE TO GODK-STATUSKODER                                       
104900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
105000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300     EJECT                                                                
105400 IMS-GET-KATM01 SECTION.                                                  
105500     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
105600          DELIMITED BY SIZE INTO SSA1                                     
105700     MOVE '  GE' TO GODK-STATUSKODER                                      
105800     CALL CBLTDLI USING GHU KATM-PCB     IO-AREA-1 SSA1                   
105900     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
106000     PERFORM IMS-STATUSKONTROLL                                           
106100     .                                                                    
106200     SKIP3                                                                
106210 IMS-GET-KATM11 SECTION.                                                  
106220     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
106230          DELIMITED BY SIZE INTO SSA1                                     
106240     MOVE '  GE' TO GODK-STATUSKODER                                      
106250     CALL CBLTDLI USING GU KATM-PCB     IO-AREA-1 SSA1                    
106260     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
106270     PERFORM IMS-STATUSKONTROLL                                           
106280     .                                                                    
106290     SKIP3                                                                
106300 IMS-REPL-KATM SECTION.                                                   
106400                                                                          
106500     MOVE '  ' TO GODK-STATUSKODER                                        
106600     CALL CBLTDLI USING REPL KATM-PCB     IO-AREA-1                       
106700     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     .                                                                    
107000     EJECT                                                                
107100 IMS-GET-1212-MODELL SECTION.                                             
107200                                                                          
107300     STRING 'WDR201  (WDGXKEY  =' W-1211-KEY-X ')'                        
107400          DELIMITED BY SIZE INTO SSA1                                     
107500     STRING 'WDGX1212(IDMODELL =' W-1212-KEY-X ')'                        
107600          DELIMITED BY SIZE INTO SSA2                                     
107700     MOVE '  GE' TO GODK-STATUSKODER                                      
107800     CALL CBLTDLI USING GU 1212-PCB     IO-AREA-2 SSA1 SSA2               
107900     MOVE 1212-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GET-1214-VARIANT SECTION.                                            
108400                                                                          
108500     STRING 'WDR201  (WDGXKEY  =' W-1213-KEY-X ')'                        
108600          DELIMITED BY SIZE INTO SSA1                                     
108700     STRING 'WDGX1214(IDVARIAN =' W-1214-KEY-X ')'                        
108800          DELIMITED BY SIZE INTO SSA2                                     
108900     MOVE '  GE' TO GODK-STATUSKODER                                      
109000     CALL CBLTDLI USING GU 1214-PCB     IO-AREA-3 SSA1 SSA2               
109100     MOVE 1214-STATUS-CODE TO STATUS-WS                                   
109200     PERFORM IMS-STATUSKONTROLL                                           
109300     .                                                                    
109400     EJECT                                                                
109500 IMS-GET-1216-MODELLVARIANT SECTION.                                      
109600                                                                          
109700     STRING 'WDR201  (WDGXKEY  =' W-1215-KEY-X ')'                        
109800          DELIMITED BY SIZE INTO SSA1                                     
109900     STRING 'WDGX1216(IDVARIAN =' W-1216-KEY-X ')'                        
110000          DELIMITED BY SIZE INTO SSA2                                     
110100     MOVE '  GE' TO GODK-STATUSKODER                                      
110200     CALL CBLTDLI USING GU 1216-PCB     IO-AREA-4 SSA1 SSA2               
110300     MOVE 1216-STATUS-CODE TO STATUS-WS                                   
110400     PERFORM IMS-STATUSKONTROLL                                           
110500     .                                                                    
110600     EJECT                                                                
110700 IMS-STATUSKONTROLL SECTION.                                              
110800                                                                          
110900     SET STATUS-IX TO 1                                                   
111000     SEARCH GODK-STATUS                                                   
111100       AT END                                                             
111200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
111300         DELIMITED BY SIZE INTO FELTEXT                                   
111400         CALL FELLOG                                                      
111500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
111600         CONTINUE                                                         
111700     END-SEARCH                                                           
111800     .                                                                    
