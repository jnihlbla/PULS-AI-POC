000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4263400.                                                
000400*AUTHOR.         INGER NILSSON.                                           
000500*DATE-WRITTEN.   92/01/06.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONTROLLRAPPORT FAKTURERING                                      
001100*        FIL TILL EKONOMI, SAPR3 VCCS                                     
001200*        FIL TILL EKONOMI, SAPR3 VCCN                                     
001210*        FIL TILL EKONOMI, SAPR3 VCUS                                     
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  OM RETURKOD FRÅN SORT                                   
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- KR-NR                                                      
002900     SELECT W4263401                   ASSIGN TO W42634D1.                
003000     SKIP2                                                                
003100*          --- FIL TILL PEDAL, VCCN                                       
003200     SELECT W4263402                   ASSIGN TO W42634D2.                
003300     SKIP2                                                                
003400*          --- FIL TILL PEDAL, VCCS                                       
003500     SELECT W4263403                   ASSIGN TO W42634D3.                
003600     SKIP2                                                                
003700*          --- KONCERNLEVERANTÖRER                                        
003800     SELECT W4263404                   ASSIGN TO W42634D4.                
003900     SKIP2                                                                
003910*          --- FIL TILL PEDAL, VCUS                                       
003920     SELECT W4263405                   ASSIGN TO W42634D5.                
003930     SKIP2                                                                
004000*          --- SORTFIL                                                    
004100     SELECT SORTFIL                    ASSIGN TO W42634DS.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W4263401                                                             
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100*01  -COPY W426PEK       -L.                                              
005200     EJECT                                                                
005300 FD  W4263402                                                             
005400     RECORDING F                                                          
005500     BLOCK CONTAINS 0.                                                    
005600                                                                          
005700*01  UT-CN-POST   -COPY W57060   -L.                                      
005800     SKIP3                                                                
005900 FD  W4263403                                                             
006000     RECORDING F                                                          
006100     BLOCK CONTAINS 0.                                                    
006200                                                                          
006300*01  UT-POST   -COPY W51060   -L.                                         
006400     EJECT                                                                
006500 FD  W4263404                                                             
006600     RECORDING F                                                          
006700     BLOCK CONTAINS 0.                                                    
006800                                                                          
006900*01  UT2-POST   -COPY W42636   -L.                                        
007000     EJECT                                                                
007010 FD  W4263405                                                             
007020     RECORDING F                                                          
007030     BLOCK CONTAINS 0.                                                    
007040                                                                          
007050*01  UT-US-POST   -COPY W57060   -PRE US- -L.                             
007060     SKIP3                                                                
007100 SD  SORTFIL                                                              
007200     LABEL RECORD STANDARD                                                
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500     SKIP2                                                                
007600 01  SORTPOST  -COPY W4263401  -PRE S-                                    
007700     EJECT                                                                
007800 WORKING-STORAGE SECTION.                                                 
007900     SKIP2                                                                
008000                                                                          
008100*    -- CHECKED BY WY2000                                                 
008200 77  IDPGM                      PIC X(8)       VALUE 'W4263400'.          
008300 77  IX1                        PIC S9(9)      VALUE +0 COMP SYNC.        
008400 77  JA                         PIC X          VALUE 'J'.                 
008500 77  NEJ                        PIC X          VALUE 'N'.                 
008600 77  WS-SUOMK                   PIC S9(7)V9(2) VALUE ZERO.                
008700 77  FL-SKRIVIT                 PIC X          VALUE 'N'.                 
008710                                                                          
008900                                                                          
009000 77  W42634-EOF-SW               PIC X       VALUE 'N'.                   
009100     88  END-OF-W4263401                     VALUE 'J'.                   
009200 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
009300     88  END-OF-SORTFIL                      VALUE 'J'.                   
009400     EJECT                                                                
009500                                                                          
009600*01  -COPY WWDC99                                                         
009700                                                                          
009800 01  W-DIFF-PRARTSTD             PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
009900 01  W-IN-PRARTSTD               PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
010000 01  W-KDSORT                    PIC X(2)              VALUE ZERO.        
010100 01  W-KDPRODSL                  PIC S9(3) COMP-3      VALUE ZERO.        
010110 01  WS-IDARTNR                  PIC S9(9) COMP-3      VALUE ZERO.        
010200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010300 01  FILLER REDEFINES DAGENS-DATUM.                                       
010400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010700     EJECT                                                                
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011400     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
011500     SKIP2                                                                
011600*    --- PARAMETRAR TILL ABEND                                            
011700                                                                          
011800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012000     SKIP2                                                                
012100 01  FELTEXT.                                                             
012200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
013000*01 -COPY W009CIA                                                         
013100                                                                          
013200     EJECT                                                                
013300 01  IN-AREA-START               PIC X(24)   VALUE                        
013400                                 'IN-AREA-START  '.                       
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W426PEK   -PRE IN-                                        
013800                                                                          
013900     EJECT                                                                
014000 01  UT-AREA-START               PIC X(24)   VALUE                        
014100                                 'UT-AREA-START  '.                       
014200     SKIP2                                                                
014300 01  UT-AREA.                                                             
014400*    03  -COPY W51060.                                                    
014500     EJECT                                                                
014600 01  UT-CN-AREA-START            PIC X(24)   VALUE                        
014700                                 'UT-CN-AREA-START  '.                    
014800     SKIP2                                                                
014900 01  UT-CN-AREA.                                                          
015000*    03  -COPY W57060 -PRE CN-                                            
015100     EJECT                                                                
015200 01  UT2-AREA.                                                            
015300*    03  -COPY W42636  -PRE KONC-                                         
015400     EJECT                                                                
015410 01  UT-US-AREA-START            PIC X(24)   VALUE                        
015420                                 'UT-US-AREA-START  '.                    
015430     SKIP2                                                                
015440 01  UT-US-AREA.                                                          
015450*    03  -COPY W57060 -PRE US-                                            
015460     EJECT                                                                
015500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015600*                                                                         
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-IDKR-X.                                                        
016200         05  W-IDKR              PIC 9(05)   VALUE ZERO.                  
016300     03  W-IDARTNR-X.                                                     
016400         05 W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.             
016500     03  W-KDSEGKEY-X.                                                    
016600         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
016700     SKIP2                                                                
016800*    --- STATUS-KOD FRÅN IMS                                              
016900 01  STATUS-WS                   PIC XX.                                  
017000     88  SEGMENT-FINNS                       VALUE '  '.                  
017100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017300     SKIP2                                                                
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(64).                               
017800 01  SSA2                        PIC X(64).                               
017900     EJECT                                                                
018000*    --- IMS FUNKTIONSKODER                                               
018100*01  -COPY W0003                                                          
018200     EJECT                                                                
018300*    ---  DLI INPUT-OUTPUT AREA                                           
018400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
018500     SKIP3                                                                
018600 01  DLI-IO-AREA1.                                                        
018700     03  IO-AREA1                PIC X(550)  VALUE SPACE.                 
018800     SKIP3                                                                
018900     03  W6H701 REDEFINES IO-AREA1.                                       
019000*        05  -COPY W6H701                                                 
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
019300     SKIP3                                                                
019400 01  DLI-IO-AREA2.                                                        
019500     03  IO-AREA2                PIC X(394)  VALUE SPACE.                 
019600     SKIP3                                                                
019700     03  W6H712 REDEFINES IO-AREA2.                                       
019800*        05  -COPY W6H712                                                 
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK601'.        
020100 01  DLI-IO-WDK601.                                                       
020200*    03 -COPY WDK601                                                      
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
020500 01  DLI-IO-WDK611.                                                       
020600*    03 -COPY WDK611                                                      
020700     EJECT                                                                
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE W6H7-                                              
021200     05  FILLER                  PIC X.                                   
021300*01  -COPY W0008  -PRE WDK6-                                              
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION  USING W6H7-PCB WDK6-PCB.                             
021700     ENTRY 'DLITCBL' USING W6H7-PCB WDK6-PCB.                             
021800                                                                          
021900     SKIP2                                                                
022000     PERFORM A-INIT                                                       
022100                                                                          
022200     SORT SORTFIL ASCENDING KEY S-IDFTG S-IDVERNR                         
022300                  INPUT  PROCEDURE B-IN-BEHANDLING                        
022400                  OUTPUT PROCEDURE C-UT-BEHANDLING                        
022500     IF SORT-RETURN > 0                                                   
022600        DISPLAY '***  W4263400 - FEL VID SORTERING ***'                   
022700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
022800     ELSE                                                                 
022900        PERFORM Z-FINIT                                                   
023000        MOVE ZERO TO RETURN-CODE                                          
023100        GOBACK                                                            
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     OPEN INPUT  W4263401                                                 
023800                                                                          
023900     OPEN OUTPUT W4263402                                                 
024000                 W4263403                                                 
024100                 W4263404                                                 
024110                 W4263405                                                 
024200     SKIP2                                                                
024300     ACCEPT DAGENS-DATUM  FROM DATE                                       
024400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024500     .                                                                    
024600     EJECT                                                                
024700 B-IN-BEHANDLING SECTION.                                                 
024800                                                                          
024900     PERFORM S01-LAES-W4263401                                            
025000     PERFORM UNTIL END-OF-W4263401                                        
025100                                                                          
025200       MOVE IN-EK-IDKR TO W-IDKR                                          
025300       PERFORM IMS-GET-W6H701                                             
025400       MOVE KR-IDKR    TO S-IDKR                                          
025500                                                                          
025600       PERFORM IMS-GET-W6H712                                             
025700       MOVE EK-IDVERNR TO S-IDVERNR                                       
025800                                                                          
025900       RELEASE S-SORTPOST                                                 
026000                                                                          
026100       PERFORM S01-LAES-W4263401                                          
026200                                                                          
026300     END-PERFORM                                                          
026400     .                                                                    
026500     EJECT                                                                
026600 C-UT-BEHANDLING SECTION.                                                 
026700                                                                          
026800     PERFORM S02-LAES-SORTFIL                                             
026900                                                                          
027000     PERFORM UNTIL END-OF-SORTFIL                                         
027100        MOVE S-IDKR TO W-IDKR                                             
027200        PERFORM IMS-GET-W6H701                                            
027300                                                                          
027400        PERFORM IMS-GET-W6H712                                            
027500                                                                          
027600        IF KR-IDLEVNR = '1225' OR '1229' OR '1555' OR '1614' OR           
027700                        '1621' OR '1625' OR '1003' OR '1013' OR           
027800                        '12054' OR '13450' OR '13456' OR                  
027900                        'BP2TG' OR 'BP2TE' OR 'BSNRA' OR 'AVKVA'          
028000                    OR  'C7CUL' OR 'BSB5A' OR 'BP2TH' OR 'BP2TC'          
028100                    OR  'BP2TF' OR 'CBGKA'                                
028200                                                                          
028300          PERFORM CB-SKRIV-EJ-BOKADE-SAPR3                                
028400        ELSE                                                              
028500          MOVE KR-IDDC TO WS-IDDC                                         
028600          IF NDC-CN                                                       
028700            PERFORM CC-SKRIV-DET-PEDAL-CN                                 
028800          ELSE                                                            
028810            IF NDC-US                                                     
028900              PERFORM CD-SKRIV-DET-PEDAL-US                               
028901            ELSE                                                          
028910              PERFORM CA-SKRIV-DET-PEDAL                                  
028920            END-IF                                                        
029000          END-IF                                                          
029100        END-IF                                                            
029200                                                                          
029300        PERFORM S02-LAES-SORTFIL                                          
029400     END-PERFORM                                                          
029500     .                                                                    
029600     EJECT                                                                
029700 CA-SKRIV-DET-PEDAL SECTION.                                              
029800                                                                          
029900     MOVE SPACE                      TO EKHT-W51060                       
030000     MOVE NEJ                        TO FL-SKRIVIT                        
030100                                                                          
030200     MOVE IDPGM                      TO EKHT-IDPGM                        
030300     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
030400                                        EKHT-DAVERDAT                     
030500     ACCEPT EKHT-TIKLOCK FROM TIME                                        
030600     MOVE +1                         TO EKHT-IDSEKVNR                     
030700     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
030800                                                                          
030900     MOVE '102'                      TO EKHT-KDEKHHT                      
031000     MOVE '107'                      TO EKHT-KDEKSHT                      
031100     MOVE 'DET  '                    TO EKHT-KDEKNIVA                     
031200     MOVE +0                         TO EKHT-IDDISTR                      
031300                                        EKHT-IDKUNDNR                     
031400     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
031500     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
031600     MOVE +0                         TO EKHT-KDPRODSL                     
031700                                        EKHT-KDPSLLOC                     
031800     MOVE KR-IDARTNR                 TO EKHT-IDARTNR                      
031900     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
032000     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
032100     IF EK-KVKRRET = +0                                                   
032200         MOVE +0 TO EKHT-PRARTSTD                                         
032300     ELSE                                                                 
032400         COMPUTE EKHT-PRARTSTD = EK-SUARTSTD /                            
032500                                 EK-KVKRRET                               
032600         END-COMPUTE                                                      
032700     END-IF                                                               
032800     IF EKHT-PRARTSTD < ZERO                                              
032900       COMPUTE EKHT-PRARTSTD = EKHT-PRARTSTD * -1                         
033000       END-COMPUTE                                                        
033100     END-IF                                                               
033200     MOVE ZERO                       TO EKHT-PRARTNTO                     
033300                                        EKHT-PRARTSJK                     
033400                                        EKHT-PRHEMTAG                     
033500                                        EKHT-PRLANDCO                     
033600                                        EKHT-PRINK                        
033700                                        EKHT-PRDIRLON                     
033800                                        EKHT-PRDMTRL                      
033900                                        EKHT-PROVRPAL                     
034000                                        EKHT-SUBEL                        
034100                                        EKHT-SUVAT                        
034200                                        EKHT-IDKONTO                      
034400     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
034500     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
034600     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
034700     COMPUTE EKHT-KVANTAL = EK-KVKRRET * -1                               
034800     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
034900     MOVE ZERO                       TO EKHT-KDFRAKT                      
035000                                        EKHT-DAAVIDAT                     
035100                                        EKHT-KDAVVTYP                     
035200                                        EKHT-KDRT                         
035300                                        EKHT-IDORDNR5                     
035400     MOVE SPACE                      TO EKHT-KDSORT                       
035401                                        EKHT-IDKST                        
035410     MOVE SPACE                      TO EKHT-FLDCET                       
035420     MOVE SPACE                      TO EKHT-IDKUNDRF                     
035430     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
035500     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
035600                                                                          
035700     IF EKHT-KVANTAL   NOT = +0                                           
035800       PERFORM S05-SKRIV-PEDAL                                            
035900       MOVE JA TO FL-SKRIVIT                                              
036000     END-IF                                                               
036100     IF EK-PRMOMS    NOT = ZERO                                           
036200       PERFORM CAA-SKRIV-MOMS-PEDAL                                       
036300       MOVE JA TO FL-SKRIVIT                                              
036400     END-IF                                                               
036500     IF EK-SUMAT     NOT = ZERO                                           
036600       PERFORM CAC-SKRIV-MATR-PEDAL                                       
036700       MOVE JA TO FL-SKRIVIT                                              
036800     END-IF                                                               
036900     IF EK-SUBESDIFF NOT = ZERO                                           
037000       PERFORM CAD-SKRIV-DIFF-PEDAL                                       
037100       MOVE JA TO FL-SKRIVIT                                              
037200     END-IF                                                               
037300     IF EK-SUHEMTAG  NOT = ZERO                                           
037400       PERFORM CAE-SKRIV-HEMT-PEDAL                                       
037500       MOVE JA TO FL-SKRIVIT                                              
037600     END-IF                                                               
037700     IF EK-SUKPALAG  NOT = ZERO                                           
037800       PERFORM CAF-SKRIV-KALK-PEDAL                                       
037900       MOVE JA TO FL-SKRIVIT                                              
038000     END-IF                                                               
038100                                                                          
038200     COMPUTE WS-SUOMK = EK-SUOMK-INT                                      
038300                                                                          
038400     IF WS-SUOMK  NOT = ZERO                                              
038500       PERFORM CAG-SKRIV-ARB-PEDAL                                        
038600       MOVE JA TO FL-SKRIVIT                                              
038700     END-IF                                                               
038800                                                                          
038900     COMPUTE WS-SUOMK = EK-SUOMK-EXT                                      
039000                                                                          
039100     IF WS-SUOMK  NOT = ZERO                                              
039200       PERFORM CAJ-SKRIV-TRP-PEDAL                                        
039300       MOVE JA TO FL-SKRIVIT                                              
039400     END-IF                                                               
039500                                                                          
039600     COMPUTE WS-SUOMK = EK-SUOMK-INT + EK-SUOMK-EXT                       
039700                                                                          
039800     IF FL-SKRIVIT = JA                                                   
039900       PERFORM CAH-SKRIV-SUM-PEDAL                                        
040000     END-IF                                                               
040100                                                                          
040200     MOVE KR-IDARTNR                TO W-IDARTNR                          
040300     PERFORM IMS-GET-WDK601                                               
040400     IF SEGMENT-FINNS                                                     
040500       MOVE ART-KDSORT              TO W-KDSORT                           
040600       MOVE ART-KDPRODSL            TO W-KDPRODSL                         
040700       PERFORM IMS-GET-WDK611                                             
040800       IF SEGMENT-FINNS                                                   
040900         IF EK-KVKRRET NOT = ZERO                                         
041000           COMPUTE W-IN-PRARTSTD = EK-SUARTSTD / EK-KVKRRET               
041100           IF W-IN-PRARTSTD < 0                                           
041200             COMPUTE W-IN-PRARTSTD = W-IN-PRARTSTD * -1                   
041300           END-IF                                                         
041400           IF CLAG-PRARTSTD NOT = W-IN-PRARTSTD                           
041500             PERFORM CAI-SKRIV-401-401                                    
041600           END-IF                                                         
041700         END-IF                                                           
041800       END-IF                                                             
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 CAA-SKRIV-MOMS-PEDAL SECTION.                                            
042300                                                                          
042400     MOVE SPACE                      TO EKHT-W51060                       
042500                                                                          
042600     MOVE IDPGM                      TO EKHT-IDPGM                        
042700     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
042800                                        EKHT-DAVERDAT                     
042900     ACCEPT EKHT-TIKLOCK FROM TIME                                        
043000     MOVE +1                         TO EKHT-IDSEKVNR                     
043100     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
043200                                                                          
043300     MOVE '102'                      TO EKHT-KDEKHHT                      
043400     MOVE '107'                      TO EKHT-KDEKSHT                      
043500     MOVE 'MOMS '                    TO EKHT-KDEKNIVA                     
043600     MOVE +0                         TO EKHT-IDDISTR                      
043700                                        EKHT-IDKUNDNR                     
043800     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
043900     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
044000     MOVE +0                         TO EKHT-KDPRODSL                     
044100                                        EKHT-KDPSLLOC                     
044200     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
044300     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
044400     MOVE ZERO                       TO EKHT-IDARTNR                      
044500                                        EKHT-PRARTNTO                     
044600                                        EKHT-PRARTSTD                     
044700                                        EKHT-PRARTSJK                     
044800                                        EKHT-PRHEMTAG                     
044900                                        EKHT-PRLANDCO                     
045000                                        EKHT-PRINK                        
045100                                        EKHT-PRDIRLON                     
045200                                        EKHT-PRDMTRL                      
045300                                        EKHT-PROVRPAL                     
045400                                        EKHT-KVANTAL                      
045500                                        EKHT-SUVAT                        
045600                                        EKHT-IDKONTO                      
045800     IF EK-PRMOMS < ZERO                                                  
045900       COMPUTE EKHT-SUBEL = EK-PRMOMS * -1                                
046000     ELSE                                                                 
046100       MOVE EK-PRMOMS                TO EKHT-SUBEL                        
046200     END-IF                                                               
046300     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
046400     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
046500     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
046600     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
046700     MOVE ZERO                       TO EKHT-KDFRAKT                      
046800                                        EKHT-DAAVIDAT                     
046900                                        EKHT-KDAVVTYP                     
047000                                        EKHT-KDRT                         
047100     MOVE SPACE                      TO EKHT-KDSORT                       
047110     MOVE SPACE                      TO EKHT-FLDCET                       
047111                                        EKHT-IDKST                        
047120     MOVE SPACE                      TO EKHT-IDKUNDRF                     
047130     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
047200     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
047300                                                                          
047400     PERFORM S05-SKRIV-PEDAL                                              
047500     .                                                                    
047600     EJECT                                                                
047700 CAC-SKRIV-MATR-PEDAL SECTION.                                            
047800                                                                          
047900     MOVE SPACE                      TO EKHT-W51060                       
048000                                                                          
048100     MOVE IDPGM                      TO EKHT-IDPGM                        
048200     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
048300                                        EKHT-DAVERDAT                     
048400     ACCEPT EKHT-TIKLOCK FROM TIME                                        
048500     MOVE +1                         TO EKHT-IDSEKVNR                     
048600     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
048700     MOVE '102'                      TO EKHT-KDEKHHT                      
048800     MOVE '107'                      TO EKHT-KDEKSHT                      
048900     MOVE 'MATR '                    TO EKHT-KDEKNIVA                     
049000     MOVE +0                         TO EKHT-IDDISTR                      
049100                                        EKHT-IDKUNDNR                     
049200     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
049300     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
049400     MOVE +0                         TO EKHT-KDPRODSL                     
049500                                        EKHT-KDPSLLOC                     
049600     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
049700     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
049800     MOVE ZERO                       TO EKHT-IDARTNR                      
049900                                        EKHT-PRARTNTO                     
050000                                        EKHT-PRARTSTD                     
050100                                        EKHT-PRARTSJK                     
050200                                        EKHT-PRHEMTAG                     
050300                                        EKHT-PRLANDCO                     
050400                                        EKHT-PRINK                        
050500                                        EKHT-PRDIRLON                     
050600                                        EKHT-PRDMTRL                      
050700                                        EKHT-PROVRPAL                     
050800                                        EKHT-KVANTAL                      
050900                                        EKHT-SUVAT                        
051000                                        EKHT-IDKONTO                      
051200     MOVE EK-SUMAT                   TO EKHT-SUBEL                        
051300     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
051400     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
051500     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
051600     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
051700     MOVE ZERO                       TO EKHT-KDFRAKT                      
051800                                        EKHT-DAAVIDAT                     
051900                                        EKHT-KDAVVTYP                     
052000                                        EKHT-KDRT                         
052100     MOVE SPACE                      TO EKHT-KDSORT                       
052101                                        EKHT-IDKST                        
052110     MOVE SPACE                      TO EKHT-FLDCET                       
052120     MOVE SPACE                      TO EKHT-IDKUNDRF                     
052130     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
052200     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
052300                                                                          
052400     PERFORM S05-SKRIV-PEDAL                                              
052500     .                                                                    
052600     EJECT                                                                
052700 CAD-SKRIV-DIFF-PEDAL SECTION.                                            
052800                                                                          
052900     MOVE SPACE                      TO EKHT-W51060                       
053000                                                                          
053100     MOVE IDPGM                      TO EKHT-IDPGM                        
053200     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
053300                                        EKHT-DAVERDAT                     
053400     ACCEPT EKHT-TIKLOCK FROM TIME                                        
053500     MOVE +1                         TO EKHT-IDSEKVNR                     
053600     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
053700                                                                          
053800     MOVE '102'                      TO EKHT-KDEKHHT                      
053900     MOVE '107'                      TO EKHT-KDEKSHT                      
054000     MOVE 'DIFF '                    TO EKHT-KDEKNIVA                     
054100     MOVE +0                         TO EKHT-IDDISTR                      
054200                                        EKHT-IDKUNDNR                     
054300     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
054400     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
054500     MOVE +0                         TO EKHT-KDPRODSL                     
054600                                        EKHT-KDPSLLOC                     
054700     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
054800     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
054900     MOVE ZERO                       TO EKHT-IDARTNR                      
055000                                        EKHT-PRARTNTO                     
055100                                        EKHT-PRARTSTD                     
055200                                        EKHT-PRARTSJK                     
055300                                        EKHT-PRHEMTAG                     
055400                                        EKHT-PRLANDCO                     
055500                                        EKHT-PRINK                        
055600                                        EKHT-PRDIRLON                     
055700                                        EKHT-PRDMTRL                      
055800                                        EKHT-PROVRPAL                     
055900                                        EKHT-KVANTAL                      
056000                                        EKHT-SUVAT                        
056100                                        EKHT-IDKONTO                      
056300     MOVE EK-SUBESDIFF               TO EKHT-SUBEL                        
056400     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
056500     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
056600     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
056700     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
056800     MOVE ZERO                       TO EKHT-KDFRAKT                      
056900                                        EKHT-DAAVIDAT                     
057000                                        EKHT-KDAVVTYP                     
057100                                        EKHT-KDRT                         
057200     MOVE SPACE                      TO EKHT-KDSORT                       
057201                                        EKHT-IDKST                        
057210     MOVE SPACE                      TO EKHT-FLDCET                       
057220     MOVE SPACE                      TO EKHT-IDKUNDRF                     
057230     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
057300     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
057400                                                                          
057500     PERFORM S05-SKRIV-PEDAL                                              
057600     .                                                                    
057700     EJECT                                                                
057800 CAE-SKRIV-HEMT-PEDAL SECTION.                                            
057900                                                                          
058000     MOVE SPACE                      TO EKHT-W51060                       
058100                                                                          
058200     MOVE IDPGM                      TO EKHT-IDPGM                        
058300     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
058400                                        EKHT-DAVERDAT                     
058500     ACCEPT EKHT-TIKLOCK FROM TIME                                        
058600     MOVE +1                         TO EKHT-IDSEKVNR                     
058700     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
058800                                                                          
058900     MOVE '102'                      TO EKHT-KDEKHHT                      
059000     MOVE '107'                      TO EKHT-KDEKSHT                      
059100     MOVE 'HEMT '                    TO EKHT-KDEKNIVA                     
059200     MOVE +0                         TO EKHT-IDDISTR                      
059300                                        EKHT-IDKUNDNR                     
059400     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
059500     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
059600     MOVE +0                         TO EKHT-KDPRODSL                     
059700                                        EKHT-KDPSLLOC                     
059800     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
059900     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
060000     MOVE ZERO                       TO EKHT-IDARTNR                      
060100                                        EKHT-PRARTNTO                     
060200                                        EKHT-PRARTSTD                     
060300                                        EKHT-PRARTSJK                     
060400                                        EKHT-PRHEMTAG                     
060500                                        EKHT-PRLANDCO                     
060600                                        EKHT-PRINK                        
060700                                        EKHT-PRDIRLON                     
060800                                        EKHT-PRDMTRL                      
060900                                        EKHT-PROVRPAL                     
061000                                        EKHT-KVANTAL                      
061100                                        EKHT-SUVAT                        
061200                                        EKHT-IDKONTO                      
061400     IF EK-SUHEMTAG  < ZERO                                               
061500       COMPUTE EKHT-SUBEL = EK-SUHEMTAG    * -1                           
061600     ELSE                                                                 
061700       MOVE EK-SUHEMTAG              TO EKHT-SUBEL                        
061800     END-IF                                                               
061900     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
062000     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
062100     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
062200     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
062300     MOVE ZERO                       TO EKHT-KDFRAKT                      
062400                                        EKHT-DAAVIDAT                     
062500                                        EKHT-KDAVVTYP                     
062600                                        EKHT-KDRT                         
062700     MOVE SPACE                      TO EKHT-KDSORT                       
062701                                        EKHT-IDKST                        
062710     MOVE SPACE                      TO EKHT-FLDCET                       
062720     MOVE SPACE                      TO EKHT-IDKUNDRF                     
062730     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
062800     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
062900                                                                          
063000     PERFORM S05-SKRIV-PEDAL                                              
063100     .                                                                    
063200     EJECT                                                                
063300 CAF-SKRIV-KALK-PEDAL SECTION.                                            
063400                                                                          
063500     MOVE SPACE                      TO EKHT-W51060                       
063600                                                                          
063700     MOVE IDPGM                      TO EKHT-IDPGM                        
063800     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
063900                                        EKHT-DAVERDAT                     
064000     ACCEPT EKHT-TIKLOCK FROM TIME                                        
064100     MOVE +1                         TO EKHT-IDSEKVNR                     
064200     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
064300                                                                          
064400     MOVE '102'                      TO EKHT-KDEKHHT                      
064500     MOVE '107'                      TO EKHT-KDEKSHT                      
064600     MOVE 'KALK '                    TO EKHT-KDEKNIVA                     
064700     MOVE +0                         TO EKHT-IDDISTR                      
064800                                        EKHT-IDKUNDNR                     
064900     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
065000     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
065100     MOVE +0                         TO EKHT-KDPRODSL                     
065200                                        EKHT-KDPSLLOC                     
065300     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
065400     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
065500     MOVE ZERO                       TO EKHT-IDARTNR                      
065600                                        EKHT-PRARTNTO                     
065700                                        EKHT-PRARTSTD                     
065800                                        EKHT-PRARTSJK                     
065900                                        EKHT-PRHEMTAG                     
066000                                        EKHT-PRLANDCO                     
066100                                        EKHT-PRINK                        
066200                                        EKHT-PRDIRLON                     
066300                                        EKHT-PRDMTRL                      
066400                                        EKHT-PROVRPAL                     
066500                                        EKHT-KVANTAL                      
066600                                        EKHT-SUVAT                        
066700                                        EKHT-IDKONTO                      
066900     IF EK-SUKPALAG  < ZERO                                               
067000       COMPUTE EKHT-SUBEL = EK-SUKPALAG     * -1                          
067100     ELSE                                                                 
067200       MOVE EK-SUKPALAG              TO EKHT-SUBEL                        
067300     END-IF                                                               
067400     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
067500     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
067600     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
067700     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
067800     MOVE ZERO                       TO EKHT-KDFRAKT                      
067900                                        EKHT-DAAVIDAT                     
068000                                        EKHT-KDAVVTYP                     
068100                                        EKHT-KDRT                         
068200     MOVE SPACE                      TO EKHT-KDSORT                       
068201                                        EKHT-IDKST                        
068210     MOVE SPACE                      TO EKHT-FLDCET                       
068220     MOVE SPACE                      TO EKHT-IDKUNDRF                     
068230     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
068300     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
068400                                                                          
068500     PERFORM S05-SKRIV-PEDAL                                              
068600     .                                                                    
068700     EJECT                                                                
068800 CAG-SKRIV-ARB-PEDAL SECTION.                                             
068900                                                                          
069000     MOVE SPACE                      TO EKHT-W51060                       
069100                                                                          
069200     MOVE IDPGM                      TO EKHT-IDPGM                        
069300     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
069400                                        EKHT-DAVERDAT                     
069500     ACCEPT EKHT-TIKLOCK FROM TIME                                        
069600     MOVE +1                         TO EKHT-IDSEKVNR                     
069700     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
069800                                                                          
069900     MOVE '102'                      TO EKHT-KDEKHHT                      
070000     MOVE '107'                      TO EKHT-KDEKSHT                      
070100     MOVE 'ARB  '                    TO EKHT-KDEKNIVA                     
070200     MOVE +0                         TO EKHT-IDDISTR                      
070300                                        EKHT-IDKUNDNR                     
070400     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
070500     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
070600     MOVE +0                         TO EKHT-KDPRODSL                     
070700                                        EKHT-KDPSLLOC                     
070800     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
070900     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
071000     MOVE ZERO                       TO EKHT-IDARTNR                      
071100                                        EKHT-PRARTNTO                     
071200                                        EKHT-PRARTSTD                     
071300                                        EKHT-PRARTSJK                     
071400                                        EKHT-PRHEMTAG                     
071500                                        EKHT-PRLANDCO                     
071600                                        EKHT-PRINK                        
071700                                        EKHT-PRDIRLON                     
071800                                        EKHT-PRDMTRL                      
071900                                        EKHT-PROVRPAL                     
072000                                        EKHT-KVANTAL                      
072100                                        EKHT-SUVAT                        
072200                                        EKHT-IDKONTO                      
072400     MOVE WS-SUOMK                   TO EKHT-SUBEL                        
072500     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
072600     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
072700     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
072800     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
072900     MOVE ZERO                       TO EKHT-KDFRAKT                      
073000                                        EKHT-DAAVIDAT                     
073100                                        EKHT-KDAVVTYP                     
073200                                        EKHT-KDRT                         
073300     MOVE SPACE                      TO EKHT-KDSORT                       
073301                                        EKHT-IDKST                        
073310     MOVE SPACE                      TO EKHT-FLDCET                       
073320     MOVE SPACE                      TO EKHT-IDKUNDRF                     
073330     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
073400     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
073500                                                                          
073600     PERFORM S05-SKRIV-PEDAL                                              
073700     .                                                                    
073800     EJECT                                                                
073900 CAH-SKRIV-SUM-PEDAL SECTION.                                             
074000                                                                          
074100     MOVE SPACE                      TO EKHT-W51060                       
074200                                                                          
074300     MOVE IDPGM                      TO EKHT-IDPGM                        
074400     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
074500                                        EKHT-DAVERDAT                     
074600     ACCEPT EKHT-TIKLOCK FROM TIME                                        
074700     MOVE +1                         TO EKHT-IDSEKVNR                     
074800     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
074900                                                                          
075000     MOVE '102'                      TO EKHT-KDEKHHT                      
075100     MOVE '107'                      TO EKHT-KDEKSHT                      
075200     MOVE 'SUM  '                    TO EKHT-KDEKNIVA                     
075300     MOVE +0                         TO EKHT-IDDISTR                      
075400                                        EKHT-IDKUNDNR                     
075500     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
075600     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
075700     MOVE +0                         TO EKHT-KDPRODSL                     
075800                                        EKHT-KDPSLLOC                     
075900     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
076000     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
076100     MOVE ZERO                       TO EKHT-IDARTNR                      
076200                                        EKHT-PRARTNTO                     
076300                                        EKHT-PRARTSTD                     
076400                                        EKHT-PRARTSJK                     
076500                                        EKHT-PRHEMTAG                     
076600                                        EKHT-PRLANDCO                     
076700                                        EKHT-PRINK                        
076800                                        EKHT-PRDIRLON                     
076900                                        EKHT-PRDMTRL                      
077000                                        EKHT-PROVRPAL                     
077100                                        EKHT-KVANTAL                      
077200                                        EKHT-IDKONTO                      
077400     IF EK-PRMOMS < ZERO                                                  
077500       COMPUTE EKHT-SUVAT = EK-PRMOMS * -1                                
077600     ELSE                                                                 
077700       MOVE EK-PRMOMS    TO EKHT-SUVAT                                    
077800     END-IF                                                               
077900                                                                          
078000     COMPUTE EKHT-SUBEL ROUNDED =                                         
078100                        EK-SUARTSTD  + EK-SUKPALAG + EK-SUHEMTAG +        
078200                        EK-SUBESDIFF + EK-SUMAT    + WS-SUOMK   +         
078300                        EK-PRMOMS                                         
078400     END-COMPUTE                                                          
078500     IF EKHT-SUBEL < ZERO                                                 
078600       COMPUTE EKHT-SUBEL = EKHT-SUBEL * -1                               
078700     END-IF                                                               
078800     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
078900     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
079000     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
079100     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
079200     MOVE ZERO                       TO EKHT-KDFRAKT                      
079300                                        EKHT-DAAVIDAT                     
079400                                        EKHT-KDAVVTYP                     
079500                                        EKHT-KDRT                         
079600     MOVE SPACE                      TO EKHT-KDSORT                       
079601                                        EKHT-IDKST                        
079610     MOVE SPACE                      TO EKHT-FLDCET                       
079620     MOVE SPACE                      TO EKHT-IDKUNDRF                     
079630     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
079700     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
079800                                                                          
079900     PERFORM S05-SKRIV-PEDAL                                              
080000     .                                                                    
080100     EJECT                                                                
080200 CAI-SKRIV-401-401 SECTION.                                               
080300     MOVE SPACE            TO EKHT-W51060                                 
080400     COMPUTE W-IN-PRARTSTD   = EK-SUARTSTD / EK-KVKRRET                   
080500     IF W-IN-PRARTSTD < 0                                                 
080600       COMPUTE W-IN-PRARTSTD = W-IN-PRARTSTD * -1                         
080700     END-IF                                                               
080800     COMPUTE W-DIFF-PRARTSTD = CLAG-PRARTSTD - W-IN-PRARTSTD              
080900     MOVE W-DIFF-PRARTSTD  TO EKHT-PRARTSTD                               
081000     MOVE EK-KVKRRET       TO EKHT-KVANTAL                                
081100                                                                          
081200     MOVE IDPGM            TO EKHT-IDPGM                                  
081300     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
081400                              EKHT-DAVERDAT                               
081500     ACCEPT    EKHT-TIKLOCK   FROM TIME                                   
081600     MOVE 1                TO EKHT-IDSEKVNR                               
081700     MOVE '401'            TO EKHT-KDEKHHT                                
081800     MOVE '401'            TO EKHT-KDEKSHT                                
081900     MOVE 'W510EKHA'       TO EKHT-IDCPYTXT                               
082000     MOVE 'DET'            TO EKHT-KDEKNIVA                               
082100     MOVE KR-IDARTNR       TO EKHT-IDARTNR                                
082200     MOVE KR-IDDC          TO EKHT-IDDC-SEND                              
082300                                                                          
082400     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
082500     MOVE KR-IDARTNR       TO CIA-IDARTBET-IN                             
082600     CALL W009CIA USING       CIA-W009CIA                                 
082700     MOVE CIA-IDARTBET-UT  TO EKHT-IDVERGL                                
082800                                                                          
082900     MOVE 'SEK'            TO EKHT-KDVALISO                               
083000     MOVE 1.00             TO EKHT-PRKURS                                 
083100     MOVE W-KDPRODSL       TO EKHT-KDPRODSL                               
083200     MOVE W-KDSORT         TO EKHT-KDSORT                                 
083300                                                                          
083400     MOVE SPACE            TO EKHT-IDDC-REC                               
083500     MOVE ZERO             TO EKHT-KDPSLLOC                               
083600     MOVE +0               TO EKHT-IDDISTR                                
083700     MOVE +0               TO EKHT-IDKUNDNR                               
083800     MOVE SPACE            TO EKHT-IDUSER                                 
083900     MOVE SPACE            TO EKHT-FLLSBOK                                
084000     MOVE ZERO             TO EKHT-PRARTNTO                               
084100     MOVE ZERO             TO EKHT-PRARTSJK                               
084200     MOVE ZERO             TO EKHT-PRHEMTAG                               
084300     MOVE ZERO             TO EKHT-PRLANDCO                               
084400     MOVE ZERO             TO EKHT-PRINK                                  
084500     MOVE ZERO             TO EKHT-PRDIRLON                               
084600     MOVE ZERO             TO EKHT-PRDMTRL                                
084700     MOVE ZERO             TO EKHT-PROVRPAL                               
084800     MOVE ZERO             TO EKHT-SUBEL                                  
084900     MOVE SPACE            TO EKHT-IDTRANS                                
085000                              EKHT-IDLEVNR                                
085100     MOVE ZERO             TO EKHT-BEVAT                                  
085200                              EKHT-IDANALYS                               
085300                              EKHT-IDKONTO                                
085500                              EKHT-KDANMORS                               
085600                              EKHT-KDFRAKT                                
085700                              EKHT-SUVAT                                  
085800                              EKHT-DAAVIDAT                               
085900                              EKHT-IDAVINR                                
086000                              EKHT-KDAVVTYP                               
086100                              EKHT-KDRT                                   
086200                              EKHT-KVANTMOT                               
086300                              EKHT-KVAVIS                                 
086400                              EKHT-IDORDNR5                               
086410     MOVE SPACE            TO EKHT-FLDCET                                 
086411                              EKHT-IDKST                                  
086420     MOVE SPACE            TO EKHT-IDKUNDRF                               
086430     MOVE SPACE            TO EKHT-IDFAKT-EXP                             
086500     MOVE 'SEPV'           TO EKHT-KDTRADP                                
086600     PERFORM S05-SKRIV-PEDAL                                              
086700     EJECT                                                                
086800     .                                                                    
086900     EJECT                                                                
087000 CAJ-SKRIV-TRP-PEDAL SECTION.                                             
087100                                                                          
087200     MOVE SPACE                      TO EKHT-W51060                       
087300                                                                          
087400     MOVE IDPGM                      TO EKHT-IDPGM                        
087500     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
087600                                        EKHT-DAVERDAT                     
087700     ACCEPT EKHT-TIKLOCK FROM TIME                                        
087800     MOVE +1                         TO EKHT-IDSEKVNR                     
087900     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
088000                                                                          
088100     MOVE '102'                      TO EKHT-KDEKHHT                      
088200     MOVE '107'                      TO EKHT-KDEKSHT                      
088300     MOVE 'TRP  '                    TO EKHT-KDEKNIVA                     
088400     MOVE +0                         TO EKHT-IDDISTR                      
088500                                        EKHT-IDKUNDNR                     
088600     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
088700     MOVE EK-IDVERNR                 TO EKHT-IDVERGL                      
088800     MOVE +0                         TO EKHT-KDPRODSL                     
088900                                        EKHT-KDPSLLOC                     
089000     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
089100     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
089200     MOVE ZERO                       TO EKHT-IDARTNR                      
089300                                        EKHT-PRARTNTO                     
089400                                        EKHT-PRARTSTD                     
089500                                        EKHT-PRARTSJK                     
089600                                        EKHT-PRHEMTAG                     
089700                                        EKHT-PRLANDCO                     
089800                                        EKHT-PRINK                        
089900                                        EKHT-PRDIRLON                     
090000                                        EKHT-PRDMTRL                      
090100                                        EKHT-PROVRPAL                     
090200                                        EKHT-KVANTAL                      
090300                                        EKHT-SUVAT                        
090400                                        EKHT-IDKONTO                      
090600     MOVE WS-SUOMK                   TO EKHT-SUBEL                        
090700     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
090800     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
090900     MOVE KR-KVANTMOT                TO EKHT-KVANTMOT                     
091000     MOVE KR-KVAVIS                  TO EKHT-KVAVIS                       
091100     MOVE ZERO                       TO EKHT-KDFRAKT                      
091200                                        EKHT-DAAVIDAT                     
091300                                        EKHT-KDAVVTYP                     
091400                                        EKHT-KDRT                         
091500     MOVE SPACE                      TO EKHT-KDSORT                       
091501                                        EKHT-IDKST                        
091510     MOVE SPACE                      TO EKHT-FLDCET                       
091520     MOVE SPACE                      TO EKHT-IDKUNDRF                     
091530     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
091600     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
091700                                                                          
091800     PERFORM S05-SKRIV-PEDAL                                              
091900     .                                                                    
092000     EJECT                                                                
092100 CB-SKRIV-EJ-BOKADE-SAPR3 SECTION.                                        
092200     MOVE EK-IDVERNR     TO KONC-IDVERNR                                  
092300     MOVE EK-TIFAKT      TO KONC-TIFAKT                                   
092400     MOVE KR-IDKR        TO KONC-IDKR                                     
092500     COMPUTE KONC-BELOPP = (EK-KVKRRET * EK-PRARTBEL-PR) +                
092600     (((EK-SUOMK-INT + EK-SUOMK-EXT + EK-SUMAT + EK-PRMOMS) * -1)         
092700         / EK-PRKURS)                                                     
092800     WRITE UT2-POST FROM UT2-AREA                                         
092900                                                                          
093000     MOVE 'KONC'         TO POSTSUM-TRANSTYP                              
093100     MOVE 'W4263404'     TO POSTSUM-FDNAMN                                
093200     MOVE 'W42634D4'     TO POSTSUM-DDNAMN2                               
093300     CALL POSTSUM USING POSTSUM-PARM                                      
093400     .                                                                    
093500     EJECT                                                                
093600                                                                          
093700 CC-SKRIV-DET-PEDAL-CN SECTION.                                           
093800     MOVE SPACE                      TO CN-EKHT-W57060                    
093900     MOVE NEJ                        TO FL-SKRIVIT                        
094000                                                                          
094100     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
094200     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
094300                                        CN-EKHT-DAVERDAT                  
094400     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
094500     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
094600     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
094700                                                                          
094800     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
094900     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
095000     MOVE 'DET  '                    TO CN-EKHT-KDEKNIVA                  
095100     MOVE +0                         TO CN-EKHT-IDDISTR                   
095200                                        CN-EKHT-IDKUNDNR                  
095300     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
095400     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
095500     MOVE +0                         TO CN-EKHT-KDPRODSL                  
095600                                        CN-EKHT-KDPSLLOC                  
095700     MOVE KR-IDARTNR                 TO CN-EKHT-IDARTNR                   
095800     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
095900     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
096000     IF EK-KVKRRET = +0                                                   
096100       MOVE +0 TO CN-EKHT-PRARTSTD                                        
096200     ELSE                                                                 
096300       MOVE EK-PRARTBEL-PR           TO CN-EKHT-PRARTSTD                  
096400     END-IF                                                               
096500     MOVE ZERO                       TO CN-EKHT-PRARTNTO                  
096600                                        CN-EKHT-PRARTSJK                  
096700                                        CN-EKHT-PRHEMTAG                  
096800                                        CN-EKHT-PRLANDCO                  
096900                                        CN-EKHT-PRINK                     
097000                                        CN-EKHT-PRDIRLON                  
097100                                        CN-EKHT-PRDMTRL                   
097200                                        CN-EKHT-PROVRPAL                  
097300                                        CN-EKHT-SUBEL                     
097400                                        CN-EKHT-SUVAT                     
097500                                        CN-EKHT-IDKONTO                   
097700     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
097800     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
097900     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
098000     COMPUTE CN-EKHT-KVANTAL = EK-KVKRRET * -1                            
098100     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
098200     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
098300                                        CN-EKHT-DAAVIDAT                  
098400                                        CN-EKHT-KDAVVTYP                  
098500                                        CN-EKHT-KDRT                      
098600                                        CN-EKHT-IDORDNR5                  
098700     MOVE SPACE                      TO CN-EKHT-KDSORT                    
098701                                        CN-EKHT-IDKST                     
098710     MOVE SPACE                      TO CN-EKHT-FLDCET                    
098720     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
098730     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
098800     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
098900                                                                          
098910     MOVE KR-IDARTNR                TO W-IDARTNR                          
098920     PERFORM IMS-GET-WDK601                                               
098930     IF SEGMENT-FINNS                                                     
098950       MOVE ART-KDPRODSL            TO CN-EKHT-KDPRODSL                   
098960     END-IF                                                               
098970                                                                          
099000     IF CN-EKHT-KVANTAL   NOT = +0                                        
099100       PERFORM S06-SKRIV-PEDAL                                            
099200       MOVE JA TO FL-SKRIVIT                                              
099300     END-IF                                                               
099800     IF EK-SUMAT     NOT = ZERO                                           
099900       PERFORM CCC-SKRIV-MATR-PEDAL                                       
100000       MOVE JA TO FL-SKRIVIT                                              
100100     END-IF                                                               
100200     IF EK-SUBESDIFF NOT = ZERO                                           
100300       PERFORM CCD-SKRIV-DIFF-PEDAL                                       
100400       MOVE JA TO FL-SKRIVIT                                              
100500     END-IF                                                               
100600     IF EK-SUHEMTAG  NOT = ZERO                                           
100700       PERFORM CCE-SKRIV-HEMT-PEDAL                                       
100800       MOVE JA TO FL-SKRIVIT                                              
100900     END-IF                                                               
101000     IF EK-SUKPALAG  NOT = ZERO                                           
101100       PERFORM CCF-SKRIV-KALK-PEDAL                                       
101200       MOVE JA TO FL-SKRIVIT                                              
101300     END-IF                                                               
101400                                                                          
101500     COMPUTE WS-SUOMK = EK-SUOMK-INT                                      
101600                                                                          
101700     IF WS-SUOMK  NOT = ZERO                                              
101800       PERFORM CCG-SKRIV-ARB-PEDAL                                        
101900       MOVE JA TO FL-SKRIVIT                                              
102000     END-IF                                                               
102100                                                                          
102200     COMPUTE WS-SUOMK = EK-SUOMK-EXT                                      
102300                                                                          
102400     IF WS-SUOMK  NOT = ZERO                                              
102500       PERFORM CCJ-SKRIV-TRP-PEDAL                                        
102600       MOVE JA TO FL-SKRIVIT                                              
102700     END-IF                                                               
102800                                                                          
102900     COMPUTE WS-SUOMK = EK-SUOMK-INT + EK-SUOMK-EXT                       
103000                                                                          
103100     IF FL-SKRIVIT = JA                                                   
103200       PERFORM CCH-SKRIV-SUM-PEDAL                                        
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
109200 CCC-SKRIV-MATR-PEDAL SECTION.                                            
109300     MOVE SPACE                      TO CN-EKHT-W57060                    
109400                                                                          
109500     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
109600     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
109700                                        CN-EKHT-DAVERDAT                  
109800     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
109900     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
110000     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
110100     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
110200     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
110300     MOVE 'MATR '                    TO CN-EKHT-KDEKNIVA                  
110400     MOVE +0                         TO CN-EKHT-IDDISTR                   
110500                                        CN-EKHT-IDKUNDNR                  
110600     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
110700     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
110800     MOVE +0                         TO CN-EKHT-KDPRODSL                  
110900                                        CN-EKHT-KDPSLLOC                  
110910     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
111000     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
111100     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
111200     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
111300                                        CN-EKHT-PRARTNTO                  
111400                                        CN-EKHT-PRARTSTD                  
111500                                        CN-EKHT-PRARTSJK                  
111600                                        CN-EKHT-PRHEMTAG                  
111700                                        CN-EKHT-PRLANDCO                  
111800                                        CN-EKHT-PRINK                     
111900                                        CN-EKHT-PRDIRLON                  
112000                                        CN-EKHT-PRDMTRL                   
112100                                        CN-EKHT-PROVRPAL                  
112200                                        CN-EKHT-KVANTAL                   
112300                                        CN-EKHT-SUVAT                     
112400                                        CN-EKHT-IDKONTO                   
112600     MOVE EK-SUMAT                   TO CN-EKHT-SUBEL                     
112700     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
112800     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
112900     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
113000     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
113100     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
113200                                        CN-EKHT-DAAVIDAT                  
113300                                        CN-EKHT-KDAVVTYP                  
113400                                        CN-EKHT-KDRT                      
113500     MOVE SPACE                      TO CN-EKHT-KDSORT                    
113501                                        CN-EKHT-IDKST                     
113510     MOVE SPACE                      TO CN-EKHT-FLDCET                    
113520     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
113530     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
113600     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
113700                                                                          
113800     PERFORM S06-SKRIV-PEDAL                                              
113900     .                                                                    
114000     EJECT                                                                
114100                                                                          
114200 CCD-SKRIV-DIFF-PEDAL SECTION.                                            
114300     MOVE SPACE                      TO CN-EKHT-W57060                    
114400                                                                          
114500     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
114600     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
114700                                        CN-EKHT-DAVERDAT                  
114800     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
114900     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
115000     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
115100                                                                          
115200     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
115300     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
115400     MOVE 'DIFF '                    TO CN-EKHT-KDEKNIVA                  
115500     MOVE +0                         TO CN-EKHT-IDDISTR                   
115600                                        CN-EKHT-IDKUNDNR                  
115700     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
115800     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
115900     MOVE +0                         TO CN-EKHT-KDPRODSL                  
116000                                        CN-EKHT-KDPSLLOC                  
116010     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
116100     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
116200     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
116300     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
116400                                        CN-EKHT-PRARTNTO                  
116500                                        CN-EKHT-PRARTSTD                  
116600                                        CN-EKHT-PRARTSJK                  
116700                                        CN-EKHT-PRHEMTAG                  
116800                                        CN-EKHT-PRLANDCO                  
116900                                        CN-EKHT-PRINK                     
117000                                        CN-EKHT-PRDIRLON                  
117100                                        CN-EKHT-PRDMTRL                   
117200                                        CN-EKHT-PROVRPAL                  
117300                                        CN-EKHT-KVANTAL                   
117400                                        CN-EKHT-SUVAT                     
117500                                        CN-EKHT-IDKONTO                   
117700     MOVE EK-SUBESDIFF               TO CN-EKHT-SUBEL                     
117800     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
117900     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
118000     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
118100     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
118200     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
118300                                        CN-EKHT-DAAVIDAT                  
118400                                        CN-EKHT-KDAVVTYP                  
118500                                        CN-EKHT-KDRT                      
118600     MOVE SPACE                      TO CN-EKHT-KDSORT                    
118601                                        CN-EKHT-IDKST                     
118610     MOVE SPACE                      TO CN-EKHT-FLDCET                    
118620     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
118630     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
118700     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
118800                                                                          
118900     PERFORM S06-SKRIV-PEDAL                                              
119000     .                                                                    
119100     EJECT                                                                
119200                                                                          
119300 CCE-SKRIV-HEMT-PEDAL SECTION.                                            
119400     MOVE SPACE                      TO CN-EKHT-W57060                    
119500                                                                          
119600     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
119700     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
119800                                        CN-EKHT-DAVERDAT                  
119900     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
120000     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
120100     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
120200                                                                          
120300     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
120400     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
120500     MOVE 'HEMT '                    TO CN-EKHT-KDEKNIVA                  
120600     MOVE +0                         TO CN-EKHT-IDDISTR                   
120700                                        CN-EKHT-IDKUNDNR                  
120800     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
120900     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
121000     MOVE +0                         TO CN-EKHT-KDPRODSL                  
121100                                        CN-EKHT-KDPSLLOC                  
121110     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
121200     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
121300     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
121400     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
121500                                        CN-EKHT-PRARTNTO                  
121600                                        CN-EKHT-PRARTSTD                  
121700                                        CN-EKHT-PRARTSJK                  
121800                                        CN-EKHT-PRHEMTAG                  
121900                                        CN-EKHT-PRLANDCO                  
122000                                        CN-EKHT-PRINK                     
122100                                        CN-EKHT-PRDIRLON                  
122200                                        CN-EKHT-PRDMTRL                   
122300                                        CN-EKHT-PROVRPAL                  
122400                                        CN-EKHT-KVANTAL                   
122500                                        CN-EKHT-SUVAT                     
122600                                        CN-EKHT-IDKONTO                   
122800     IF EK-SUHEMTAG  < ZERO                                               
122900       COMPUTE CN-EKHT-SUBEL = EK-SUHEMTAG    * -1                        
123000     ELSE                                                                 
123100       MOVE EK-SUHEMTAG              TO CN-EKHT-SUBEL                     
123200     END-IF                                                               
123300     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
123400     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
123500     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
123600     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
123700     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
123800                                        CN-EKHT-DAAVIDAT                  
123900                                        CN-EKHT-KDAVVTYP                  
124000                                        CN-EKHT-KDRT                      
124100     MOVE SPACE                      TO CN-EKHT-KDSORT                    
124110     MOVE SPACE                      TO CN-EKHT-FLDCET                    
124111                                        CN-EKHT-IDKST                     
124120     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
124130     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
124200     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
124300                                                                          
124400     PERFORM S06-SKRIV-PEDAL                                              
124500     .                                                                    
124600     EJECT                                                                
124700                                                                          
124800 CCF-SKRIV-KALK-PEDAL SECTION.                                            
124900     MOVE SPACE                      TO CN-EKHT-W57060                    
125000                                                                          
125100     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
125200     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
125300                                        CN-EKHT-DAVERDAT                  
125400     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
125500     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
125600     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
125700                                                                          
125800     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
125900     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
126000     MOVE 'KALK '                    TO CN-EKHT-KDEKNIVA                  
126100     MOVE +0                         TO CN-EKHT-IDDISTR                   
126200                                        CN-EKHT-IDKUNDNR                  
126300     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
126400     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
126500     MOVE +0                         TO CN-EKHT-KDPRODSL                  
126600                                        CN-EKHT-KDPSLLOC                  
126610     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
126700     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
126800     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
126900     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
127000                                        CN-EKHT-PRARTNTO                  
127100                                        CN-EKHT-PRARTSTD                  
127200                                        CN-EKHT-PRARTSJK                  
127300                                        CN-EKHT-PRHEMTAG                  
127400                                        CN-EKHT-PRLANDCO                  
127500                                        CN-EKHT-PRINK                     
127600                                        CN-EKHT-PRDIRLON                  
127700                                        CN-EKHT-PRDMTRL                   
127800                                        CN-EKHT-PROVRPAL                  
127900                                        CN-EKHT-KVANTAL                   
128000                                        CN-EKHT-SUVAT                     
128100                                        CN-EKHT-IDKONTO                   
128300     IF EK-SUKPALAG  < ZERO                                               
128400       COMPUTE CN-EKHT-SUBEL = EK-SUKPALAG     * -1                       
128500     ELSE                                                                 
128600       MOVE EK-SUKPALAG              TO CN-EKHT-SUBEL                     
128700     END-IF                                                               
128800     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
128900     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
129000     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
129100     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
129200     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
129300                                        CN-EKHT-DAAVIDAT                  
129400                                        CN-EKHT-KDAVVTYP                  
129500                                        CN-EKHT-KDRT                      
129600     MOVE SPACE                      TO CN-EKHT-KDSORT                    
129601                                        CN-EKHT-IDKST                     
129610     MOVE SPACE                      TO CN-EKHT-FLDCET                    
129620     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
129630     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
129700     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
129800                                                                          
129900     PERFORM S06-SKRIV-PEDAL                                              
130000     .                                                                    
130100     EJECT                                                                
130200                                                                          
130300 CCG-SKRIV-ARB-PEDAL SECTION.                                             
130400     MOVE SPACE                      TO CN-EKHT-W57060                    
130500                                                                          
130600     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
130700     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
130800                                        CN-EKHT-DAVERDAT                  
130900     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
131000     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
131100     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
131200                                                                          
131300     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
131400     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
131500     MOVE 'ARB  '                    TO CN-EKHT-KDEKNIVA                  
131600     MOVE +0                         TO CN-EKHT-IDDISTR                   
131700                                        CN-EKHT-IDKUNDNR                  
131800     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
131900     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
132000     MOVE +0                         TO CN-EKHT-KDPRODSL                  
132100                                        CN-EKHT-KDPSLLOC                  
132110     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
132200     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
132300     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
132400     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
132500                                        CN-EKHT-PRARTNTO                  
132600                                        CN-EKHT-PRARTSTD                  
132700                                        CN-EKHT-PRARTSJK                  
132800                                        CN-EKHT-PRHEMTAG                  
132900                                        CN-EKHT-PRLANDCO                  
133000                                        CN-EKHT-PRINK                     
133100                                        CN-EKHT-PRDIRLON                  
133200                                        CN-EKHT-PRDMTRL                   
133300                                        CN-EKHT-PROVRPAL                  
133400                                        CN-EKHT-KVANTAL                   
133500                                        CN-EKHT-SUVAT                     
133600                                        CN-EKHT-IDKONTO                   
133800     MOVE WS-SUOMK                   TO CN-EKHT-SUBEL                     
133900     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
134000     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
134100     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
134200     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
134300     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
134400                                        CN-EKHT-DAAVIDAT                  
134500                                        CN-EKHT-KDAVVTYP                  
134600                                        CN-EKHT-KDRT                      
134700     MOVE SPACE                      TO CN-EKHT-KDSORT                    
134701                                        CN-EKHT-IDKST                     
134710     MOVE SPACE                      TO CN-EKHT-FLDCET                    
134720     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
134730     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
134800     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
134900                                                                          
135000     PERFORM S06-SKRIV-PEDAL                                              
135100     .                                                                    
135200     EJECT                                                                
135300                                                                          
135400 CCH-SKRIV-SUM-PEDAL SECTION.                                             
135500     MOVE SPACE                      TO CN-EKHT-W57060                    
135600                                                                          
135700     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
135800     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
135900                                        CN-EKHT-DAVERDAT                  
136000     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
136100     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
136200     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
136300                                                                          
136400     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
136500     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
136600     MOVE 'SUM  '                    TO CN-EKHT-KDEKNIVA                  
136700     MOVE +0                         TO CN-EKHT-IDDISTR                   
136800                                        CN-EKHT-IDKUNDNR                  
136900     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
137000     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
137100     MOVE +0                         TO CN-EKHT-KDPRODSL                  
137200                                        CN-EKHT-KDPSLLOC                  
137210     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
137300     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
137400     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
137500     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
137600                                        CN-EKHT-PRARTNTO                  
137700                                        CN-EKHT-PRARTSTD                  
137800                                        CN-EKHT-PRARTSJK                  
137900                                        CN-EKHT-PRHEMTAG                  
138000                                        CN-EKHT-PRLANDCO                  
138100                                        CN-EKHT-PRINK                     
138200                                        CN-EKHT-PRDIRLON                  
138300                                        CN-EKHT-PRDMTRL                   
138400                                        CN-EKHT-PROVRPAL                  
138500                                        CN-EKHT-KVANTAL                   
138600                                        CN-EKHT-IDKONTO                   
138800     IF EK-PRMOMS < ZERO                                                  
138900       COMPUTE CN-EKHT-SUVAT = EK-PRMOMS * -1                             
139000     ELSE                                                                 
139100       MOVE EK-PRMOMS    TO CN-EKHT-SUVAT                                 
139200     END-IF                                                               
139300                                                                          
139400*    COMPUTE CN-EKHT-SUBEL ROUNDED =                                      
139500*                       EK-SUARTSTD  + EK-SUKPALAG + EK-SUHEMTAG +        
139600*                       EK-SUBESDIFF + EK-SUMAT    + WS-SUOMK    +        
139700*                       EK-PRMOMS                                         
139800     COMPUTE CN-EKHT-SUBEL ROUNDED =                                      
139810                        EK-SUARTSTD  +                                    
139820                        EK-SUBESDIFF + EK-SUMAT    + WS-SUOMK             
139900     END-COMPUTE                                                          
140000     IF CN-EKHT-SUBEL < ZERO                                              
140100       COMPUTE CN-EKHT-SUBEL = CN-EKHT-SUBEL * -1                         
140200     END-IF                                                               
140300     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
140400     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
140500     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
140600     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
140700     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
140800                                        CN-EKHT-DAAVIDAT                  
140900                                        CN-EKHT-KDAVVTYP                  
141000                                        CN-EKHT-KDRT                      
141100     MOVE SPACE                      TO CN-EKHT-KDSORT                    
141110     MOVE SPACE                      TO CN-EKHT-FLDCET                    
141111                                        CN-EKHT-IDKST                     
141120     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
141130     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
141200     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
141300                                                                          
141400     PERFORM S06-SKRIV-PEDAL                                              
141500     .                                                                    
141600     EJECT                                                                
141700                                                                          
141800 CCJ-SKRIV-TRP-PEDAL SECTION.                                             
141900     MOVE SPACE                      TO CN-EKHT-W57060                    
142000                                                                          
142100     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
142200     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
142300                                        CN-EKHT-DAVERDAT                  
142400     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
142500     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
142600     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
142700                                                                          
142800     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
142900     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
143000     MOVE 'TRP  '                    TO CN-EKHT-KDEKNIVA                  
143100     MOVE +0                         TO CN-EKHT-IDDISTR                   
143200                                        CN-EKHT-IDKUNDNR                  
143300     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
143400     MOVE EK-IDVERNR                 TO CN-EKHT-IDVERGL                   
143500     MOVE +0                         TO CN-EKHT-KDPRODSL                  
143600                                        CN-EKHT-KDPSLLOC                  
143610     MOVE ART-KDPRODSL               TO CN-EKHT-KDPRODSL                  
143700     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
143800     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
143900     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
144000                                        CN-EKHT-PRARTNTO                  
144100                                        CN-EKHT-PRARTSTD                  
144200                                        CN-EKHT-PRARTSJK                  
144300                                        CN-EKHT-PRHEMTAG                  
144400                                        CN-EKHT-PRLANDCO                  
144500                                        CN-EKHT-PRINK                     
144600                                        CN-EKHT-PRDIRLON                  
144700                                        CN-EKHT-PRDMTRL                   
144800                                        CN-EKHT-PROVRPAL                  
144900                                        CN-EKHT-KVANTAL                   
145000                                        CN-EKHT-SUVAT                     
145100                                        CN-EKHT-IDKONTO                   
145300     MOVE WS-SUOMK                   TO CN-EKHT-SUBEL                     
145400     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
145500     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
145600     MOVE KR-KVANTMOT                TO CN-EKHT-KVANTMOT                  
145700     MOVE KR-KVAVIS                  TO CN-EKHT-KVAVIS                    
145800     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
145900                                        CN-EKHT-DAAVIDAT                  
146000                                        CN-EKHT-KDAVVTYP                  
146100                                        CN-EKHT-KDRT                      
146200     MOVE SPACE                      TO CN-EKHT-KDSORT                    
146201                                        CN-EKHT-IDKST                     
146210     MOVE SPACE                      TO CN-EKHT-FLDCET                    
146220     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
146230     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
146300     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
146400                                                                          
146500     PERFORM S06-SKRIV-PEDAL                                              
146600     .                                                                    
146700     EJECT                                                                
146710 CD-SKRIV-DET-PEDAL-US SECTION.                                           
146720     MOVE SPACE                      TO US-EKHT-W57060                    
146730     MOVE NEJ                        TO FL-SKRIVIT                        
146740                                                                          
146750     MOVE IDPGM                      TO US-EKHT-IDPGM                     
146760     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
146770                                        US-EKHT-DAVERDAT                  
146780     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
146790     MOVE +1                         TO US-EKHT-IDSEKVNR                  
146791     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
146792                                                                          
146793     MOVE '102'                      TO US-EKHT-KDEKHHT                   
146794     MOVE '107'                      TO US-EKHT-KDEKSHT                   
146795     MOVE 'DET  '                    TO US-EKHT-KDEKNIVA                  
146796     MOVE +0                         TO US-EKHT-IDDISTR                   
146797                                        US-EKHT-IDKUNDNR                  
146798     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
146799     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
146800     MOVE +0                         TO US-EKHT-KDPRODSL                  
146801                                        US-EKHT-KDPSLLOC                  
146802     MOVE KR-IDARTNR                 TO US-EKHT-IDARTNR                   
146803                                        WS-IDARTNR                        
146804     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
146805     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
146806     IF EK-KVKRRET = +0                                                   
146807       MOVE +0 TO US-EKHT-PRARTSTD                                        
146808     ELSE                                                                 
146809       MOVE EK-PRARTBEL-PR           TO US-EKHT-PRARTSTD                  
146810     END-IF                                                               
146811     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
146812                                        US-EKHT-PRARTSJK                  
146813                                        US-EKHT-PRHEMTAG                  
146814                                        US-EKHT-PRLANDCO                  
146815                                        US-EKHT-PRINK                     
146816                                        US-EKHT-PRDIRLON                  
146817                                        US-EKHT-PRDMTRL                   
146818                                        US-EKHT-PROVRPAL                  
146819                                        US-EKHT-SUBEL                     
146820                                        US-EKHT-SUVAT                     
146821                                        US-EKHT-IDKONTO                   
146822     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
146823     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
146824     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
146825     COMPUTE US-EKHT-KVANTAL = EK-KVKRRET * -1                            
146826     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
146827     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
146828                                        US-EKHT-DAAVIDAT                  
146829                                        US-EKHT-KDAVVTYP                  
146830                                        US-EKHT-KDRT                      
146831                                        US-EKHT-IDORDNR5                  
146832     MOVE SPACE                      TO US-EKHT-KDSORT                    
146833                                        US-EKHT-IDKST                     
146834     MOVE SPACE                      TO US-EKHT-FLDCET                    
146835     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
146836     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
146837     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
146838                                                                          
146839     IF US-EKHT-KVANTAL   NOT = +0                                        
146840       PERFORM S07-SKRIV-PEDAL-US                                         
146841       MOVE JA TO FL-SKRIVIT                                              
146842     END-IF                                                               
146845     IF EK-SUMAT     NOT = ZERO                                           
146846       PERFORM CDC-SKRIV-MATR-PEDAL                                       
146847       MOVE JA TO FL-SKRIVIT                                              
146848     END-IF                                                               
146849     IF EK-SUBESDIFF NOT = ZERO                                           
146850       PERFORM CDD-SKRIV-DIFF-PEDAL                                       
146851       MOVE JA TO FL-SKRIVIT                                              
146852     END-IF                                                               
146853     IF EK-SUHEMTAG  NOT = ZERO                                           
146854       PERFORM CDE-SKRIV-HEMT-PEDAL                                       
146855       MOVE JA TO FL-SKRIVIT                                              
146856     END-IF                                                               
146857     IF EK-SUKPALAG  NOT = ZERO                                           
146858       PERFORM CDF-SKRIV-KALK-PEDAL                                       
146859       MOVE JA TO FL-SKRIVIT                                              
146860     END-IF                                                               
146861                                                                          
146862     COMPUTE WS-SUOMK = EK-SUOMK-INT                                      
146863                                                                          
146864     IF WS-SUOMK  NOT = ZERO                                              
146865       PERFORM CDG-SKRIV-ARB-PEDAL                                        
146866       MOVE JA TO FL-SKRIVIT                                              
146867     END-IF                                                               
146868                                                                          
146869     COMPUTE WS-SUOMK = EK-SUOMK-EXT                                      
146870                                                                          
146871     IF WS-SUOMK  NOT = ZERO                                              
146872       PERFORM CDJ-SKRIV-TRP-PEDAL                                        
146873       MOVE JA TO FL-SKRIVIT                                              
146874     END-IF                                                               
146875                                                                          
146876     COMPUTE WS-SUOMK = EK-SUOMK-INT + EK-SUOMK-EXT                       
146877                                                                          
146878     IF FL-SKRIVIT = JA                                                   
146879       PERFORM CDH-SKRIV-SUM-PEDAL                                        
146880     END-IF                                                               
146881     .                                                                    
146882     EJECT                                                                
146883                                                                          
146941 CDC-SKRIV-MATR-PEDAL SECTION.                                            
146942     MOVE SPACE                      TO US-EKHT-W57060                    
146943                                                                          
146944     MOVE IDPGM                      TO US-EKHT-IDPGM                     
146945     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
146946                                        US-EKHT-DAVERDAT                  
146947     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
146948     MOVE +1                         TO US-EKHT-IDSEKVNR                  
146949     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
146950     MOVE '102'                      TO US-EKHT-KDEKHHT                   
146951     MOVE '107'                      TO US-EKHT-KDEKSHT                   
146952     MOVE 'MATR '                    TO US-EKHT-KDEKNIVA                  
146953     MOVE +0                         TO US-EKHT-IDDISTR                   
146954                                        US-EKHT-IDKUNDNR                  
146955     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
146956     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
146957     MOVE +0                         TO US-EKHT-KDPRODSL                  
146958                                        US-EKHT-KDPSLLOC                  
146959     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
146960     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
146961     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
146962     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
146964                                        US-EKHT-PRARTSTD                  
146965                                        US-EKHT-PRARTSJK                  
146966                                        US-EKHT-PRHEMTAG                  
146967                                        US-EKHT-PRLANDCO                  
146968                                        US-EKHT-PRINK                     
146969                                        US-EKHT-PRDIRLON                  
146970                                        US-EKHT-PRDMTRL                   
146971                                        US-EKHT-PROVRPAL                  
146972                                        US-EKHT-KVANTAL                   
146973                                        US-EKHT-SUVAT                     
146974                                        US-EKHT-IDKONTO                   
146975     MOVE EK-SUMAT                   TO US-EKHT-SUBEL                     
146976     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
146977     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
146978     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
146979     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
146980     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
146981                                        US-EKHT-DAAVIDAT                  
146982                                        US-EKHT-KDAVVTYP                  
146983                                        US-EKHT-KDRT                      
146984     MOVE SPACE                      TO US-EKHT-KDSORT                    
146985                                        US-EKHT-IDKST                     
146986     MOVE SPACE                      TO US-EKHT-FLDCET                    
146987     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
146988     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
146989     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
146990                                                                          
146991     PERFORM S07-SKRIV-PEDAL-US                                           
146992     .                                                                    
146993     EJECT                                                                
146994                                                                          
146995 CDD-SKRIV-DIFF-PEDAL SECTION.                                            
146996     MOVE SPACE                      TO US-EKHT-W57060                    
146997                                                                          
146998     MOVE IDPGM                      TO US-EKHT-IDPGM                     
146999     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147000                                        US-EKHT-DAVERDAT                  
147001     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147002     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147003     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147004                                                                          
147005     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147006     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147007     MOVE 'DIFF '                    TO US-EKHT-KDEKNIVA                  
147008     MOVE +0                         TO US-EKHT-IDDISTR                   
147009                                        US-EKHT-IDKUNDNR                  
147010     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147011     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147012     MOVE +0                         TO US-EKHT-KDPRODSL                  
147013                                        US-EKHT-KDPSLLOC                  
147014     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147015     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147016     MOVE ZERO                       TO US-EKHT-IDARTNR                   
147017                                        US-EKHT-PRARTNTO                  
147018                                        US-EKHT-PRARTSTD                  
147019                                        US-EKHT-PRARTSJK                  
147020                                        US-EKHT-PRHEMTAG                  
147021                                        US-EKHT-PRLANDCO                  
147022                                        US-EKHT-PRINK                     
147023                                        US-EKHT-PRDIRLON                  
147024                                        US-EKHT-PRDMTRL                   
147025                                        US-EKHT-PROVRPAL                  
147026                                        US-EKHT-KVANTAL                   
147027                                        US-EKHT-SUVAT                     
147028                                        US-EKHT-IDKONTO                   
147029     MOVE EK-SUBESDIFF               TO US-EKHT-SUBEL                     
147030     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147031     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147032     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147033     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147034     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147035                                        US-EKHT-DAAVIDAT                  
147036                                        US-EKHT-KDAVVTYP                  
147037                                        US-EKHT-KDRT                      
147038     MOVE SPACE                      TO US-EKHT-KDSORT                    
147039     MOVE SPACE                      TO US-EKHT-FLDCET                    
147040                                        US-EKHT-IDKST                     
147041     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147042     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147043     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147044                                                                          
147045     PERFORM S07-SKRIV-PEDAL-US                                           
147046     .                                                                    
147047     EJECT                                                                
147048                                                                          
147049 CDE-SKRIV-HEMT-PEDAL SECTION.                                            
147050     MOVE SPACE                      TO US-EKHT-W57060                    
147051                                                                          
147052     MOVE IDPGM                      TO US-EKHT-IDPGM                     
147053     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147054                                        US-EKHT-DAVERDAT                  
147055     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147056     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147057     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147058                                                                          
147059     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147060     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147061     MOVE 'HEMT '                    TO US-EKHT-KDEKNIVA                  
147062     MOVE +0                         TO US-EKHT-IDDISTR                   
147063                                        US-EKHT-IDKUNDNR                  
147064     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147065     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147066     MOVE +0                         TO US-EKHT-KDPRODSL                  
147067                                        US-EKHT-KDPSLLOC                  
147068     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147069     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147070     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
147071     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
147072                                        US-EKHT-PRARTSTD                  
147073                                        US-EKHT-PRARTSJK                  
147074                                        US-EKHT-PRHEMTAG                  
147075                                        US-EKHT-PRLANDCO                  
147076                                        US-EKHT-PRINK                     
147077                                        US-EKHT-PRDIRLON                  
147078                                        US-EKHT-PRDMTRL                   
147079                                        US-EKHT-PROVRPAL                  
147080                                        US-EKHT-KVANTAL                   
147081                                        US-EKHT-SUVAT                     
147082                                        US-EKHT-IDKONTO                   
147083     IF EK-SUHEMTAG  < ZERO                                               
147084       COMPUTE US-EKHT-SUBEL = EK-SUHEMTAG    * -1                        
147085     ELSE                                                                 
147086       MOVE EK-SUHEMTAG              TO US-EKHT-SUBEL                     
147087     END-IF                                                               
147088     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147089     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147090     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147091     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147092     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147093                                        US-EKHT-DAAVIDAT                  
147094                                        US-EKHT-KDAVVTYP                  
147095                                        US-EKHT-KDRT                      
147096     MOVE SPACE                      TO US-EKHT-KDSORT                    
147097     MOVE SPACE                      TO US-EKHT-FLDCET                    
147098                                        US-EKHT-IDKST                     
147099     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147100     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147101     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147102                                                                          
147103     PERFORM S07-SKRIV-PEDAL-US                                           
147104     .                                                                    
147105     EJECT                                                                
147106                                                                          
147107 CDF-SKRIV-KALK-PEDAL SECTION.                                            
147108     MOVE SPACE                      TO US-EKHT-W57060                    
147109                                                                          
147110     MOVE IDPGM                      TO US-EKHT-IDPGM                     
147111     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147112                                        US-EKHT-DAVERDAT                  
147113     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147114     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147115     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147116                                                                          
147117     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147118     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147119     MOVE 'KALK '                    TO US-EKHT-KDEKNIVA                  
147120     MOVE +0                         TO US-EKHT-IDDISTR                   
147121                                        US-EKHT-IDKUNDNR                  
147122     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147123     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147124     MOVE +0                         TO US-EKHT-KDPRODSL                  
147125                                        US-EKHT-KDPSLLOC                  
147126     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147127     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147128     MOVE ZERO                       TO US-EKHT-IDARTNR                   
147129                                        US-EKHT-PRARTNTO                  
147130                                        US-EKHT-PRARTSTD                  
147131                                        US-EKHT-PRARTSJK                  
147132                                        US-EKHT-PRHEMTAG                  
147133                                        US-EKHT-PRLANDCO                  
147134                                        US-EKHT-PRINK                     
147135                                        US-EKHT-PRDIRLON                  
147136                                        US-EKHT-PRDMTRL                   
147137                                        US-EKHT-PROVRPAL                  
147138                                        US-EKHT-KVANTAL                   
147139                                        US-EKHT-SUVAT                     
147140                                        US-EKHT-IDKONTO                   
147141     IF EK-SUKPALAG  < ZERO                                               
147142       COMPUTE US-EKHT-SUBEL = EK-SUKPALAG     * -1                       
147143     ELSE                                                                 
147144       MOVE EK-SUKPALAG              TO US-EKHT-SUBEL                     
147145     END-IF                                                               
147146     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147147     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147148     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147149     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147150     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147151                                        US-EKHT-DAAVIDAT                  
147152                                        US-EKHT-KDAVVTYP                  
147153                                        US-EKHT-KDRT                      
147154     MOVE SPACE                      TO US-EKHT-KDSORT                    
147155                                        US-EKHT-IDKST                     
147156     MOVE SPACE                      TO US-EKHT-FLDCET                    
147157     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147158     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147159     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147160                                                                          
147161     PERFORM S07-SKRIV-PEDAL-US                                           
147162     .                                                                    
147163     EJECT                                                                
147164                                                                          
147165 CDG-SKRIV-ARB-PEDAL SECTION.                                             
147166     MOVE SPACE                      TO US-EKHT-W57060                    
147167                                                                          
147168     MOVE IDPGM                      TO US-EKHT-IDPGM                     
147169     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147170                                        US-EKHT-DAVERDAT                  
147171     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147172     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147173     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147174                                                                          
147175     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147176     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147177     MOVE 'ARB  '                    TO US-EKHT-KDEKNIVA                  
147178     MOVE +0                         TO US-EKHT-IDDISTR                   
147179                                        US-EKHT-IDKUNDNR                  
147180     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147181     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147182     MOVE +0                         TO US-EKHT-KDPRODSL                  
147183                                        US-EKHT-KDPSLLOC                  
147184     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147185     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147186     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
147187     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
147188                                        US-EKHT-PRARTSTD                  
147189                                        US-EKHT-PRARTSJK                  
147190                                        US-EKHT-PRHEMTAG                  
147191                                        US-EKHT-PRLANDCO                  
147192                                        US-EKHT-PRINK                     
147193                                        US-EKHT-PRDIRLON                  
147194                                        US-EKHT-PRDMTRL                   
147195                                        US-EKHT-PROVRPAL                  
147196                                        US-EKHT-KVANTAL                   
147197                                        US-EKHT-SUVAT                     
147198                                        US-EKHT-IDKONTO                   
147199     MOVE WS-SUOMK                   TO US-EKHT-SUBEL                     
147200     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147201     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147202     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147203     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147204     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147205                                        US-EKHT-DAAVIDAT                  
147206                                        US-EKHT-KDAVVTYP                  
147207                                        US-EKHT-KDRT                      
147208     MOVE SPACE                      TO US-EKHT-KDSORT                    
147209                                        US-EKHT-IDKST                     
147210     MOVE SPACE                      TO US-EKHT-FLDCET                    
147211     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147212     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147213     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147214                                                                          
147215     PERFORM S07-SKRIV-PEDAL-US                                           
147216     .                                                                    
147217     EJECT                                                                
147218                                                                          
147219 CDH-SKRIV-SUM-PEDAL SECTION.                                             
147220     MOVE SPACE                      TO US-EKHT-W57060                    
147221                                                                          
147222     MOVE IDPGM                      TO US-EKHT-IDPGM                     
147223     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147224                                        US-EKHT-DAVERDAT                  
147225     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147226     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147227     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147228                                                                          
147229     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147230     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147231     MOVE 'SUM  '                    TO US-EKHT-KDEKNIVA                  
147232     MOVE +0                         TO US-EKHT-IDDISTR                   
147233                                        US-EKHT-IDKUNDNR                  
147234     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147235     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147236     MOVE +0                         TO US-EKHT-KDPRODSL                  
147237                                        US-EKHT-KDPSLLOC                  
147238     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147239     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147240     MOVE ZERO                       TO US-EKHT-IDARTNR                   
147241                                        US-EKHT-PRARTNTO                  
147242                                        US-EKHT-PRARTSTD                  
147243                                        US-EKHT-PRARTSJK                  
147244                                        US-EKHT-PRHEMTAG                  
147245                                        US-EKHT-PRLANDCO                  
147246                                        US-EKHT-PRINK                     
147247                                        US-EKHT-PRDIRLON                  
147248                                        US-EKHT-PRDMTRL                   
147249                                        US-EKHT-PROVRPAL                  
147250                                        US-EKHT-KVANTAL                   
147251                                        US-EKHT-IDKONTO                   
147252     IF EK-PRMOMS < ZERO                                                  
147253       COMPUTE US-EKHT-SUVAT = EK-PRMOMS * -1                             
147254     ELSE                                                                 
147255       MOVE EK-PRMOMS    TO US-EKHT-SUVAT                                 
147256     END-IF                                                               
147257                                                                          
147258     COMPUTE US-EKHT-SUBEL ROUNDED =                                      
147259                        EK-SUARTSTD  +                                    
147260                        EK-SUBESDIFF + EK-SUMAT    + WS-SUOMK    +        
147261                        EK-PRMOMS                                         
147262     END-COMPUTE                                                          
147263     IF US-EKHT-SUBEL < ZERO                                              
147264       COMPUTE US-EKHT-SUBEL = US-EKHT-SUBEL * -1                         
147265     END-IF                                                               
147266     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147267     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147268     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147269     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147270     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147271                                        US-EKHT-DAAVIDAT                  
147272                                        US-EKHT-KDAVVTYP                  
147273                                        US-EKHT-KDRT                      
147274     MOVE SPACE                      TO US-EKHT-KDSORT                    
147275                                        US-EKHT-IDKST                     
147276     MOVE SPACE                      TO US-EKHT-FLDCET                    
147277     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147278     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147279     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147280                                                                          
147281     PERFORM S07-SKRIV-PEDAL-US                                           
147282     .                                                                    
147283     EJECT                                                                
147284                                                                          
147285 CDJ-SKRIV-TRP-PEDAL SECTION.                                             
147286     MOVE SPACE                      TO US-EKHT-W57060                    
147287                                                                          
147288     MOVE IDPGM                      TO US-EKHT-IDPGM                     
147289     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
147290                                        US-EKHT-DAVERDAT                  
147291     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
147292     MOVE +1                         TO US-EKHT-IDSEKVNR                  
147293     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
147294                                                                          
147295     MOVE '102'                      TO US-EKHT-KDEKHHT                   
147296     MOVE '107'                      TO US-EKHT-KDEKSHT                   
147297     MOVE 'TRP  '                    TO US-EKHT-KDEKNIVA                  
147298     MOVE +0                         TO US-EKHT-IDDISTR                   
147299                                        US-EKHT-IDKUNDNR                  
147300     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
147301     MOVE EK-IDVERNR                 TO US-EKHT-IDVERGL                   
147302     MOVE +0                         TO US-EKHT-KDPRODSL                  
147303                                        US-EKHT-KDPSLLOC                  
147304     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
147305     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
147306     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
147307     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
147308                                        US-EKHT-PRARTSTD                  
147309                                        US-EKHT-PRARTSJK                  
147310                                        US-EKHT-PRHEMTAG                  
147311                                        US-EKHT-PRLANDCO                  
147312                                        US-EKHT-PRINK                     
147313                                        US-EKHT-PRDIRLON                  
147314                                        US-EKHT-PRDMTRL                   
147315                                        US-EKHT-PROVRPAL                  
147316                                        US-EKHT-KVANTAL                   
147317                                        US-EKHT-SUVAT                     
147318                                        US-EKHT-IDKONTO                   
147319     MOVE WS-SUOMK                   TO US-EKHT-SUBEL                     
147320     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
147321     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
147322     MOVE KR-KVANTMOT                TO US-EKHT-KVANTMOT                  
147323     MOVE KR-KVAVIS                  TO US-EKHT-KVAVIS                    
147324     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
147325                                        US-EKHT-DAAVIDAT                  
147326                                        US-EKHT-KDAVVTYP                  
147327                                        US-EKHT-KDRT                      
147328     MOVE SPACE                      TO US-EKHT-KDSORT                    
147329     MOVE SPACE                      TO US-EKHT-FLDCET                    
147330                                        US-EKHT-IDKST                     
147331     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
147332     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
147333     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
147334                                                                          
147335     PERFORM S07-SKRIV-PEDAL-US                                           
147336     .                                                                    
147337     EJECT                                                                
147338 Z-FINIT SECTION.                                                         
147339     CLOSE W4263401                                                       
147340           W4263402                                                       
147341           W4263403                                                       
147342           W4263404                                                       
147343           W4263405                                                       
147350     SKIP2                                                                
147400     MOVE 'S' TO POSTSUM-OPKOD                                            
147500     CALL POSTSUM USING POSTSUM-PARM                                      
147600     .                                                                    
147700     EJECT                                                                
147800 S01-LAES-W4263401  SECTION.                                              
147900     SKIP2                                                                
148000     READ W4263401 INTO IN-AREA                                           
148100     AT END                                                               
148200        SET END-OF-W4263401 TO TRUE                                       
148300                                                                          
148400     NOT AT END                                                           
148500        MOVE 'W4263401' TO POSTSUM-FDNAMN                                 
148600        MOVE 'W42634D1' TO POSTSUM-DDNAMN2                                
148700        MOVE IN-EK-IDPTYP TO POSTSUM-TRANSTYP                             
148800        CALL POSTSUM USING POSTSUM-PARM                                   
148900     END-READ                                                             
149000     .                                                                    
149100     EJECT                                                                
149200 S02-LAES-SORTFIL   SECTION.                                              
149300     SKIP2                                                                
149400       RETURN SORTFIL                                                     
149500       AT END                                                             
149600          SET END-OF-SORTFIL TO TRUE                                      
149700       END-RETURN                                                         
149800     .                                                                    
149900     EJECT                                                                
150000 S05-SKRIV-PEDAL SECTION.                                                 
150100     SKIP2                                                                
150200     WRITE UT-POST FROM UT-AREA                                           
150300                                                                          
150400     MOVE EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                              
150500     MOVE 'W4263403'     TO POSTSUM-FDNAMN                                
150600     MOVE 'W42634D3'     TO POSTSUM-DDNAMN2                               
150700     CALL POSTSUM USING POSTSUM-PARM                                      
150800     .                                                                    
150900     EJECT                                                                
151000 S06-SKRIV-PEDAL SECTION.                                                 
151100     SKIP2                                                                
151200     WRITE UT-CN-POST FROM UT-CN-AREA                                     
151300                                                                          
151400     MOVE CN-EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                           
151500     MOVE 'W4263402'        TO POSTSUM-FDNAMN                             
151600     MOVE 'W42634D2'        TO POSTSUM-DDNAMN2                            
151700     CALL POSTSUM USING POSTSUM-PARM                                      
151800     .                                                                    
151900     EJECT                                                                
151910 S07-SKRIV-PEDAL-US SECTION.                                              
151920     SKIP2                                                                
151930     WRITE US-UT-US-POST FROM UT-US-AREA                                  
151940                                                                          
151950     MOVE US-EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                           
151960     MOVE 'W4263405'        TO POSTSUM-FDNAMN                             
151970     MOVE 'W42634D5'        TO POSTSUM-DDNAMN2                            
151980     CALL POSTSUM USING POSTSUM-PARM                                      
151990     .                                                                    
151991     EJECT                                                                
152000 S99-ABEND SECTION.                                                       
152100     SKIP2                                                                
152200     MOVE 'S' TO POSTSUM-OPKOD                                            
152300     CALL POSTSUM USING POSTSUM-PARM                                      
152400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
152500     .                                                                    
152600     EJECT                                                                
152700* --- IMS SEKTIONER ---                                                   
152800                                                                          
152900 IMS-GET-W6H701 SECTION.                                                  
153000     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
153100          DELIMITED BY SIZE INTO SSA1                                     
153200     MOVE '  ' TO GODK-STATUSKODER                                        
153300     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA1 SSA1                     
153400     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
153500     PERFORM IMS-STATUSKONTROLL                                           
153600     .                                                                    
153700     SKIP2                                                                
153800 IMS-GET-W6H712 SECTION.                                                  
153900     MOVE 'W6H712   '         TO SSA1                                     
154000     MOVE '  ' TO GODK-STATUSKODER                                        
154100     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-AREA2 SSA1                    
154200     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
154300     PERFORM IMS-STATUSKONTROLL                                           
154400     .                                                                    
154500     EJECT                                                                
154600 IMS-GET-WDK601 SECTION.                                                  
154700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
154800          DELIMITED BY SIZE INTO SSA1                                     
154900     MOVE '  GE'           TO GODK-STATUSKODER                            
155000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
155100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155200     PERFORM IMS-STATUSKONTROLL                                           
155300     .                                                                    
155400     EJECT                                                                
155500 IMS-GET-WDK611 SECTION.                                                  
155600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
155700          DELIMITED BY SIZE INTO SSA1                                     
155800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
155900          DELIMITED BY SIZE INTO SSA2                                     
156000     MOVE '  GE'           TO GODK-STATUSKODER                            
156100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
156200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
156300     PERFORM IMS-STATUSKONTROLL                                           
156400     .                                                                    
156500     EJECT                                                                
156600 IMS-STATUSKONTROLL SECTION.                                              
156700     SKIP2                                                                
156800     SET STATUS-IX TO 1                                                   
156900     SEARCH GODK-STATUS                                                   
157000       AT END                                                             
157100         CALL FELLOG                                                      
157200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
157300     END-SEARCH                                                           
157400     .                                                                    
