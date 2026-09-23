000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4263500.                                                
000400*AUTHOR.         KENT JEBSEN.                                             
000500*DATE-WRITTEN.   00/12/11.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONTROLLRAPPORT-KREDITERING                                      
001100*        FIL TILL EKONOMI, SAPR3 VCCS                                     
001200*        FIL TILL EKONOMI, SAPR3 VCCN                                     
001210*        FIL TILL EKONOMI, SAPR3 VCUS                                     
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
001500*                              WDK6                                       
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- KR-NR                                                      
002500     SELECT W4263501                   ASSIGN TO W42635D1.                
002600     SKIP2                                                                
002700*          --- FIL TILL PEDAL, VCCS                                       
002800     SELECT W4263502                   ASSIGN TO W42635D2.                
002900     SKIP2                                                                
003000*         -- FIL FÖR UTSKRIFT AV LISTA KR EJ BEHANDLADE AV EKONOMI        
003100     SELECT W4263503                   ASSIGN TO W42635D3.                
003200     SKIP2                                                                
003300*          --- FIL TILL PEDAL, VCCN                                       
003400     SELECT W4263504                   ASSIGN TO W42635D4.                
003500     SKIP2                                                                
003510*          --- FIL TILL PEDAL, VCUS                                       
003520     SELECT W4263505                   ASSIGN TO W42635D5.                
003530     SKIP2                                                                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W4263501                                                             
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W426KEK       -L.                                              
004600     SKIP3                                                                
004700 FD  W4263502                                                             
004800     RECORDING F                                                          
004900     BLOCK CONTAINS 0.                                                    
005000*01  UT-POST   -COPY W51060   -L.                                         
005100     SKIP3                                                                
005200 FD  W4263503                                                             
005300     RECORDING F                                                          
005400     BLOCK CONTAINS 0.                                                    
005500*01  UT2-POST   -COPY W42633   -L.                                        
005600     SKIP3                                                                
005700 FD  W4263504                                                             
005800     RECORDING F                                                          
005900     BLOCK CONTAINS 0.                                                    
006000*01  UT-CN-POST   -COPY W57060   -L.                                      
006100     SKIP3                                                                
006110 FD  W4263505                                                             
006120     RECORDING F                                                          
006130     BLOCK CONTAINS 0.                                                    
006140*01  UT-US-POST   -COPY W57060 PRE US- -L.                                
006150     SKIP3                                                                
006200                                                                          
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)      VALUE 'W4263500'.          
006800 77  IX1                         PIC S9(9)     VALUE +0 COMP SYNC.        
006900 77  JA                          PIC X         VALUE 'J'.                 
007000 77  NEJ                         PIC X         VALUE 'N'.                 
007100 77  WS-SUOMK                    PIC 9(7)V9(2) VALUE ZERO.                
007200                                                                          
007300 01  W-DIFF-PRARTSTD             PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
007400 01  W-IN-PRARTSTD               PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
007500 01  W-KDSORT                    PIC X(2)              VALUE ZERO.        
007600 01  W-KDPRODSL                  PIC S9(3) COMP-3      VALUE ZERO.        
007610 01  WS-IDARTNR                  PIC S9(9) COMP-3      VALUE ZERO.        
007700                                                                          
007800                                                                          
007900*01  -COPY WWDC99                                                         
008000                                                                          
008100                                                                          
008200 77  W42635-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W4263501                     VALUE 'J'.                   
008400     EJECT                                                                
008500 01  DYNAMISKA-SUBPROGRAM.                                                
008600*                                                                         
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
009700*01 -COPY W009CIA                                                         
009800                                                                          
009900     EJECT                                                                
010000 01  IN-AREA-START               PIC X(24)   VALUE                        
010100                                 'IN-AREA-START  '.                       
010200     SKIP2                                                                
010300                                                                          
010400*01  AREA -COPY W426KEK   -PRE IN-                                        
010500                                                                          
010600     EJECT                                                                
010700 01  UT-AREA-START               PIC X(24)   VALUE                        
010800                                 'UT-AREA-START  '.                       
010900     SKIP2                                                                
011000 01  UT-AREA.                                                             
011100*    03  -COPY W51060.                                                    
011200     EJECT                                                                
011300 01  UT-CN-AREA-START            PIC X(24)   VALUE                        
011400                                 'UT-CN-AREA-START  '.                    
011500     SKIP2                                                                
011600 01  UT-CN-AREA.                                                          
011700*    03  -COPY W57060  -PRE CN-                                           
011800     EJECT                                                                
011810 01  UT-US-AREA-START            PIC X(24)   VALUE                        
011820                                 'UT-US-AREA-START  '.                    
011830     SKIP2                                                                
011840 01  UT-US-AREA.                                                          
011850*    03  -COPY W57060  -PRE US-                                           
011860     EJECT                                                                
011900 01  UT2-AREA.                                                            
012000*    03  -COPY W42633  -PRE KONC-                                         
012100     EJECT                                                                
012200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012300*                                                                         
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-IDKR-X.                                                        
012900         05  W-IDKR              PIC 9(05)   VALUE ZERO.                  
013000     SKIP2                                                                
013100     03  W-IDARTNR-X.                                                     
013200         05 W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.             
013300     03  W-KDSEGKEY-X.                                                    
013400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
013500     SKIP2                                                                
013600     03  W-IDSEGMNR-X.                                                    
013700         05  W-IDSEGMNR          PIC S9 COMP-3 VALUE ZERO.                
013800     SKIP2                                                                
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014400     SKIP2                                                                
014500 01  GODK-STATUSKODER.                                                    
014600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000 01  SSA3                        PIC X(64).                               
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500*    ---  DLI INPUT-OUTPUT AREA                                           
015600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
015700     SKIP3                                                                
015800 01  DLI-IO-AREA1.                                                        
015900     03  IO-AREA1                PIC X(550)  VALUE SPACE.                 
016000     SKIP3                                                                
016100     03  W6H701 REDEFINES IO-AREA1.                                       
016200*        05  -COPY W6H701                                                 
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
016500     SKIP3                                                                
016600 01  DLI-IO-AREA2.                                                        
016700     03  IO-AREA2                PIC X(394)  VALUE SPACE.                 
016800     SKIP3                                                                
016900     03  W6H712 REDEFINES IO-AREA2.                                       
017000*        05  -COPY W6H712                                                 
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
017300     SKIP3                                                                
017400 01  DLI-IO-AREA3.                                                        
017500     03  IO-AREA3                PIC X(394)  VALUE SPACE.                 
017600     SKIP3                                                                
017700     03  W6H721 REDEFINES IO-AREA3.                                       
017800*        05  -COPY W6H721                                                 
017900     EJECT                                                                
018000 01  FILLER                         PIC X(16) VALUE 'WDK601-AREA'.        
018100 01  DLI-IO-WDK601.                                                       
018200*    03  -COPY WDK601                                                     
018300     EJECT                                                                
018400 01  FILLER                         PIC X(16) VALUE 'WDK611-AREA'.        
018500 01  DLI-IO-WDK611.                                                       
018600*    03  -COPY WDK611                                                     
018700     EJECT                                                                
018800 LINKAGE SECTION.                                                         
018900                                                                          
019000*01  -COPY W0008  -PRE W6H7-                                              
019100     05  FILLER                  PIC X.                                   
019200*01  -COPY W0008  -PRE WDK6-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING W6H7-PCB WDK6-PCB.                             
019600     ENTRY 'DLITCBL' USING W6H7-PCB WDK6-PCB.                             
019700                                                                          
019800     SKIP2                                                                
019900     PERFORM A-INIT                                                       
020000                                                                          
020100     PERFORM B-BEHANDLA-DATA                                              
020200                                                                          
020300     PERFORM Z-FINIT                                                      
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT SECTION.                                                          
020900                                                                          
021000     OPEN INPUT  W4263501                                                 
021100                                                                          
021200     OPEN OUTPUT W4263502                                                 
021300                 W4263503                                                 
021400                 W4263504                                                 
021410                 W4263505                                                 
021500                                                                          
021600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021700     .                                                                    
021800     EJECT                                                                
021900 B-BEHANDLA-DATA SECTION.                                                 
022000                                                                          
022100     PERFORM S01-LAES-W4263501                                            
022200     PERFORM UNTIL END-OF-W4263501                                        
022300                                                                          
022400       MOVE IN-KEK-IDKR     TO W-IDKR                                     
022500       MOVE IN-KEK-IDSEGMNR TO W-IDSEGMNR                                 
022600       PERFORM IMS-GU-W6H701                                              
022700                                                                          
022800       PERFORM IMS-GET-W6H712                                             
022900       PERFORM IMS-GU-W6H721                                              
023000       IF KR-IDLEVNR = '1225' OR '1229' OR '1555' OR '1614' OR            
023100                       '1621' OR '1625' OR '1003' OR '1013' OR            
023200                       '12054' OR '13450' OR '13456' OR                   
023300                       'BP2TG' OR 'BP2TE' OR 'BSNRA' OR 'AVKVA'           
023400                    OR 'C7CUL' OR 'BSB5A' OR 'BP2TH' OR 'BP2TC'           
023500                    OR 'BP2TF' OR 'CBGKA'                                 
023600                                                                          
023700         PERFORM CB-SKRIV-EJ-BOKADE-SAPR3                                 
023800       ELSE                                                               
023900         MOVE KR-IDDC TO WS-IDDC                                          
024000         IF NDC-CN                                                        
024100           PERFORM CC-SKRIV-DET-PEDAL-CN                                  
024200         ELSE                                                             
024210           IF NDC-US                                                      
024300             PERFORM CD-SKRIV-DET-PEDAL-US                                
024310           ELSE                                                           
024311             PERFORM CA-SKRIV-DET-PEDAL                                   
024320           END-IF                                                         
024400         END-IF                                                           
024500       END-IF                                                             
024600                                                                          
024700       PERFORM S01-LAES-W4263501                                          
024800                                                                          
024900     END-PERFORM                                                          
025000     .                                                                    
025100     EJECT                                                                
025200 CA-SKRIV-DET-PEDAL SECTION.                                              
025300                                                                          
025400     MOVE SPACE                      TO EKHT-W51060                       
025500                                                                          
025600     MOVE IDPGM                      TO EKHT-IDPGM                        
025700     MOVE FUNCTION CURRENT-DATE(1:8) TO EKHT-DAREGDAT                     
025800                                        EKHT-DAVERDAT                     
025900     ACCEPT EKHT-TIKLOCK FROM TIME                                        
026000     MOVE +1                         TO EKHT-IDSEKVNR                     
026100     MOVE 'W510EKHA'                 TO EKHT-IDCPYTXT                     
026200                                                                          
026300     MOVE '102'                      TO EKHT-KDEKHHT                      
026400     MOVE '107'                      TO EKHT-KDEKSHT                      
026500     MOVE 'DET  '                    TO EKHT-KDEKNIVA                     
026600     MOVE +0                         TO EKHT-IDDISTR                      
026700                                        EKHT-IDKUNDNR                     
026800     MOVE KR-IDDC                    TO EKHT-IDDC-SEND                    
026900     MOVE KRED-IDVERNR               TO EKHT-IDVERGL                      
027000     MOVE +0                         TO EKHT-KDPRODSL                     
027100                                        EKHT-KDPSLLOC                     
027200     MOVE KR-IDARTNR                 TO EKHT-IDARTNR                      
027300     MOVE EK-KDVALISO                TO EKHT-KDVALISO                     
027400     MOVE EK-PRKURS                  TO EKHT-PRKURS                       
027500     IF EK-KVKRRET = +0                                                   
027600         MOVE +0 TO EKHT-PRARTSTD                                         
027700     ELSE                                                                 
027800         COMPUTE EKHT-PRARTSTD = EK-SUARTSTD /                            
027900                                 EK-KVKRRET                               
028000         END-COMPUTE                                                      
028100     END-IF                                                               
028200     IF EKHT-PRARTSTD < ZERO                                              
028300       COMPUTE EKHT-PRARTSTD = EKHT-PRARTSTD * -1                         
028400       END-COMPUTE                                                        
028500     END-IF                                                               
028600     MOVE ZERO                       TO EKHT-PRARTNTO                     
028700                                        EKHT-PRARTSJK                     
028800                                        EKHT-PRHEMTAG                     
028900                                        EKHT-PRLANDCO                     
029000                                        EKHT-PRINK                        
029100                                        EKHT-PRDIRLON                     
029200                                        EKHT-PRDMTRL                      
029300                                        EKHT-PROVRPAL                     
029400                                        EKHT-SUBEL                        
029500                                        EKHT-SUVAT                        
029600                                        EKHT-IDKONTO                      
029800     MOVE KR-IDAVINR                 TO EKHT-IDAVINR                      
029900     MOVE KR-IDLEVNR                 TO EKHT-IDLEVNR                      
030000     MOVE ZERO                       TO EKHT-KDFRAKT                      
030100                                        EKHT-DAAVIDAT                     
030200                                        EKHT-KDAVVTYP                     
030300                                        EKHT-KDRT                         
030400                                        EKHT-IDORDNR5                     
030500     MOVE SPACE                      TO EKHT-KDSORT                       
030501                                        EKHT-IDKST                        
030510     MOVE SPACE                      TO EKHT-FLDCET                       
030520     MOVE SPACE                      TO EKHT-IDKUNDRF                     
030530     MOVE SPACE                      TO EKHT-IDFAKT-EXP                   
030600     MOVE 'SEPV'                     TO EKHT-KDTRADP                      
030700                                                                          
030800     IF KRED-IDPTYP = 'AUT'                                               
030900       COMPUTE EKHT-KVANTMOT = KR-KVANTMOT * -1                           
031000       MOVE EK-KVKRRET      TO EKHT-KVANTAL                               
031100       COMPUTE EKHT-KVAVIS   = KR-KVAVIS * -1                             
031200       IF EKHT-KVANTAL NOT = +0                                           
031300         PERFORM S05-SKRIV-PEDAL                                          
031400       END-IF                                                             
031500       MOVE ZERO            TO EKHT-KVANTAL                               
031600       IF EK-SUBESDIFF NOT = ZERO                                         
031700         PERFORM CAD-SKRIV-DIFF-PEDAL                                     
031800       END-IF                                                             
031900       IF EK-SUHEMTAG NOT = ZERO                                          
032000         PERFORM CAE-SKRIV-HEMT-PEDAL                                     
032100       END-IF                                                             
032200       IF EK-SUKPALAG NOT = ZERO                                          
032300         PERFORM CAF-SKRIV-KALK-PEDAL                                     
032400       END-IF                                                             
032500     ELSE                                                                 
032600       MOVE ZERO TO EKHT-KVANTMOT                                         
032700                    EKHT-KVAVIS                                           
032800     END-IF                                                               
032900                                                                          
033000     MOVE ZERO TO EKHT-KVANTAL                                            
033100                                                                          
033200     IF KRED-PRMOMS NOT = ZERO                                            
033300       PERFORM CAA-SKRIV-MOMS-PEDAL                                       
033400     END-IF                                                               
033500     IF KRED-SUMAT-5DEC NOT = ZERO                                        
033600       PERFORM CAC-SKRIV-MATR-PEDAL                                       
033700     END-IF                                                               
033800                                                                          
033900     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
034000                                                                          
034100     IF WS-SUOMK NOT = ZERO                                               
034200       PERFORM CAG-SKRIV-ARB-PEDAL                                        
034300     END-IF                                                               
034400                                                                          
034500     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-EXT-5DEC                       
034600                                                                          
034700     IF WS-SUOMK NOT = ZERO                                               
034800       PERFORM CAJ-SKRIV-TRP-PEDAL                                        
034900     END-IF                                                               
035000                                                                          
035100     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
035200                                            + KRED-SUOMK-EXT-5DEC         
035300                                                                          
035400     PERFORM CAH-SKRIV-SUM-PEDAL                                          
035500                                                                          
035600     MOVE KR-IDARTNR           TO W-IDARTNR                               
035700     PERFORM IMS-GET-WDK601                                               
035800     IF SEGMENT-FINNS                                                     
035900       MOVE ART-KDSORT              TO W-KDSORT                           
036000       MOVE ART-KDPRODSL            TO W-KDPRODSL                         
036100       PERFORM IMS-GET-WDK611                                             
036200       IF EK-KVKRRET NOT = ZERO                                           
036300         IF SEGMENT-FINNS                                                 
036400           COMPUTE W-IN-PRARTSTD = EK-SUARTSTD / EK-KVKRRET               
036500           IF W-IN-PRARTSTD < 0                                           
036600             COMPUTE W-IN-PRARTSTD = W-IN-PRARTSTD * -1                   
036700           END-IF                                                         
036800           IF CLAG-PRARTSTD NOT = W-IN-PRARTSTD                           
036900             PERFORM CAI-SKRIV-401-401                                    
037000           END-IF                                                         
037100         END-IF                                                           
037200       END-IF                                                             
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 CAA-SKRIV-MOMS-PEDAL SECTION.                                            
037700                                                                          
037800     ACCEPT EKHT-TIKLOCK FROM TIME                                        
037900                                                                          
038000     MOVE 'MOMS '                    TO EKHT-KDEKNIVA                     
038100     MOVE ZERO                       TO EKHT-IDARTNR                      
038200                                        EKHT-PRARTSTD                     
038300     MOVE KRED-PRMOMS                TO EKHT-SUBEL                        
038400                                                                          
038500     PERFORM S05-SKRIV-PEDAL                                              
038600     .                                                                    
038700     EJECT                                                                
038800 CAC-SKRIV-MATR-PEDAL SECTION.                                            
038900                                                                          
039000     ACCEPT EKHT-TIKLOCK FROM TIME                                        
039100     MOVE 'MATR '                    TO EKHT-KDEKNIVA                     
039200     MOVE ZERO                       TO EKHT-IDARTNR                      
039300                                        EKHT-PRARTSTD                     
039400     COMPUTE EKHT-SUBEL ROUNDED = KRED-SUMAT-5DEC                         
039500     PERFORM S05-SKRIV-PEDAL                                              
039600     .                                                                    
039700     EJECT                                                                
039800 CAD-SKRIV-DIFF-PEDAL SECTION.                                            
039900                                                                          
040000     ACCEPT EKHT-TIKLOCK FROM TIME                                        
040100                                                                          
040200     MOVE 'DIFF '                    TO EKHT-KDEKNIVA                     
040300     MOVE ZERO                       TO EKHT-IDARTNR                      
040400                                        EKHT-PRARTSTD                     
040500     COMPUTE EKHT-SUBEL = EK-SUBESDIFF * -1                               
040600                                                                          
040700     PERFORM S05-SKRIV-PEDAL                                              
040800     .                                                                    
040900     EJECT                                                                
041000 CAE-SKRIV-HEMT-PEDAL SECTION.                                            
041100                                                                          
041200     ACCEPT EKHT-TIKLOCK FROM TIME                                        
041300                                                                          
041400     MOVE 'HEMT '                    TO EKHT-KDEKNIVA                     
041500     MOVE ZERO                       TO EKHT-IDARTNR                      
041600                                        EKHT-PRARTSTD                     
041700     IF EK-SUHEMTAG  > ZERO                                               
041800       COMPUTE EKHT-SUBEL = EK-SUHEMTAG * -1                              
041900     ELSE                                                                 
042000       MOVE EK-SUHEMTAG TO EKHT-SUBEL                                     
042100     END-IF                                                               
042200                                                                          
042300     PERFORM S05-SKRIV-PEDAL                                              
042400     .                                                                    
042500     EJECT                                                                
042600 CAF-SKRIV-KALK-PEDAL SECTION.                                            
042700                                                                          
042800     ACCEPT EKHT-TIKLOCK FROM TIME                                        
042900                                                                          
043000     MOVE 'KALK '                    TO EKHT-KDEKNIVA                     
043100     MOVE ZERO                       TO EKHT-IDARTNR                      
043200                                        EKHT-PRARTSTD                     
043300     IF EK-SUKPALAG  > ZERO                                               
043400       COMPUTE EKHT-SUBEL = EK-SUKPALAG * -1                              
043500     ELSE                                                                 
043600       MOVE EK-SUKPALAG TO EKHT-SUBEL                                     
043700     END-IF                                                               
043800                                                                          
043900     PERFORM S05-SKRIV-PEDAL                                              
044000     .                                                                    
044100     EJECT                                                                
044200 CAG-SKRIV-ARB-PEDAL SECTION.                                             
044300                                                                          
044400     ACCEPT EKHT-TIKLOCK FROM TIME                                        
044500                                                                          
044600     MOVE 'ARB  '                    TO EKHT-KDEKNIVA                     
044700     MOVE ZERO                       TO EKHT-IDARTNR                      
044800                                        EKHT-PRARTSTD                     
044900     MOVE WS-SUOMK                   TO EKHT-SUBEL                        
045000                                                                          
045100     PERFORM S05-SKRIV-PEDAL                                              
045200     .                                                                    
045300     EJECT                                                                
045400 CAH-SKRIV-SUM-PEDAL SECTION.                                             
045500                                                                          
045600     ACCEPT EKHT-TIKLOCK FROM TIME                                        
045700                                                                          
045800     MOVE 'SUM  '                    TO EKHT-KDEKNIVA                     
045900     MOVE ZERO                       TO EKHT-IDARTNR                      
046000                                        EKHT-PRARTSTD                     
046100     MOVE KRED-PRMOMS                TO EKHT-SUVAT                        
046200                                                                          
046300     IF KRED-IDPTYP = 'AUT'                                               
046400       COMPUTE EKHT-SUBEL ROUNDED =                                       
046500                        EK-SUARTSTD  + EK-SUKPALAG + EK-SUHEMTAG +        
046600                        EK-SUBESDIFF - KRED-PRMOMS -                      
046700                        KRED-SUMAT-5DEC - WS-SUOMK                        
046800       END-COMPUTE                                                        
046900     ELSE                                                                 
047000       COMPUTE EKHT-SUBEL ROUNDED =                                       
047100              (KRED-SUMAT-5DEC + WS-SUOMK + KRED-PRMOMS) * -1             
047200       END-COMPUTE                                                        
047300     END-IF                                                               
047400                                                                          
047500     PERFORM S05-SKRIV-PEDAL                                              
047600     .                                                                    
047700     EJECT                                                                
047800 CAI-SKRIV-401-401 SECTION.                                               
047900     MOVE SPACE            TO EKHT-W51060                                 
048000                                                                          
048100     COMPUTE W-IN-PRARTSTD = EK-SUARTSTD / EK-KVKRRET                     
048200     IF W-IN-PRARTSTD < 0                                                 
048300       COMPUTE W-IN-PRARTSTD = W-IN-PRARTSTD * -1                         
048400     END-IF                                                               
048500     COMPUTE W-DIFF-PRARTSTD = CLAG-PRARTSTD - W-IN-PRARTSTD              
048600     MOVE  W-DIFF-PRARTSTD TO EKHT-PRARTSTD                               
048700                                                                          
048800     MOVE IDPGM            TO EKHT-IDPGM                                  
048900     MOVE FUNCTION CURRENT-DATE (1:8) TO EKHT-DAREGDAT                    
049000                              EKHT-DAVERDAT                               
049100     ACCEPT    EKHT-TIKLOCK   FROM TIME                                   
049200     MOVE 1                TO EKHT-IDSEKVNR                               
049300     MOVE '401'            TO EKHT-KDEKHHT                                
049400     MOVE '401'            TO EKHT-KDEKSHT                                
049500     MOVE 'W510EKHA'       TO EKHT-IDCPYTXT                               
049600     MOVE 'DET'            TO EKHT-KDEKNIVA                               
049700     MOVE KR-IDARTNR       TO EKHT-IDARTNR                                
049800     MOVE KR-IDDC          TO EKHT-IDDC-SEND                              
049900     MOVE EK-KVKRRET       TO EKHT-KVANTAL                                
050000                                                                          
050100     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
050200     MOVE KR-IDARTNR       TO CIA-IDARTBET-IN                             
050300     CALL W009CIA USING       CIA-W009CIA                                 
050400     MOVE CIA-IDARTBET-UT  TO EKHT-IDVERGL                                
050500                                                                          
050600     MOVE 'SEK'            TO EKHT-KDVALISO                               
050700     MOVE 1.00             TO EKHT-PRKURS                                 
050800     MOVE W-KDPRODSL       TO EKHT-KDPRODSL                               
050900     MOVE W-DIFF-PRARTSTD  TO EKHT-PRARTSTD                               
051000     MOVE W-KDSORT         TO EKHT-KDSORT                                 
051100                                                                          
051200     MOVE SPACE            TO EKHT-IDDC-REC                               
051300     MOVE ZERO             TO EKHT-KDPSLLOC                               
051400     MOVE +0               TO EKHT-IDDISTR                                
051500     MOVE +0               TO EKHT-IDKUNDNR                               
051600     MOVE SPACE            TO EKHT-IDUSER                                 
051700     MOVE SPACE            TO EKHT-FLLSBOK                                
051800     MOVE ZERO             TO EKHT-PRARTNTO                               
051900     MOVE ZERO             TO EKHT-PRARTSJK                               
052000     MOVE ZERO             TO EKHT-PRHEMTAG                               
052100     MOVE ZERO             TO EKHT-PRLANDCO                               
052200     MOVE ZERO             TO EKHT-PRINK                                  
052300     MOVE ZERO             TO EKHT-PRDIRLON                               
052400     MOVE ZERO             TO EKHT-PRDMTRL                                
052500     MOVE ZERO             TO EKHT-PROVRPAL                               
052600     MOVE ZERO             TO EKHT-SUBEL                                  
052700     MOVE SPACE            TO EKHT-IDTRANS                                
052800                              EKHT-IDLEVNR                                
052900     MOVE ZERO             TO EKHT-BEVAT                                  
053000                              EKHT-IDANALYS                               
053100                              EKHT-IDKONTO                                
053300                              EKHT-KDANMORS                               
053400                              EKHT-KDFRAKT                                
053500                              EKHT-SUVAT                                  
053600                              EKHT-DAAVIDAT                               
053700                              EKHT-IDAVINR                                
053800                              EKHT-KDAVVTYP                               
053900                              EKHT-KDRT                                   
054000                              EKHT-KVANTMOT                               
054100                              EKHT-KVAVIS                                 
054200                              EKHT-IDORDNR5                               
054300     MOVE 'SEPV'           TO EKHT-KDTRADP                                
054310     MOVE SPACE            TO EKHT-FLDCET                                 
054311                              EKHT-IDKST                                  
054320     MOVE SPACE            TO EKHT-IDKUNDRF                               
054330     MOVE SPACE            TO EKHT-IDFAKT-EXP                             
054400     PERFORM S05-SKRIV-PEDAL                                              
054500     EJECT                                                                
054600     .                                                                    
054700 CAJ-SKRIV-TRP-PEDAL SECTION.                                             
054800                                                                          
054900     ACCEPT EKHT-TIKLOCK FROM TIME                                        
055000                                                                          
055100     MOVE 'TRP  '                    TO EKHT-KDEKNIVA                     
055200     MOVE ZERO                       TO EKHT-IDARTNR                      
055300                                        EKHT-PRARTSTD                     
055400     MOVE WS-SUOMK                   TO EKHT-SUBEL                        
055500                                                                          
055600     PERFORM S05-SKRIV-PEDAL                                              
055700     .                                                                    
055800     EJECT                                                                
055900 CB-SKRIV-EJ-BOKADE-SAPR3 SECTION.                                        
056000     MOVE KRED-IDVERNR   TO KONC-IDVERNR                                  
056100     MOVE KRED-TIKRED    TO KONC-TIFAKT                                   
056200     MOVE KR-IDKR        TO KONC-IDKR                                     
056300     WRITE UT2-POST FROM UT2-AREA                                         
056400                                                                          
056500     MOVE 'KONC'         TO POSTSUM-TRANSTYP                              
056600     MOVE 'W4263503'     TO POSTSUM-FDNAMN                                
056700     MOVE 'W42635D3'     TO POSTSUM-DDNAMN2                               
056800     CALL POSTSUM USING POSTSUM-PARM                                      
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200 CC-SKRIV-DET-PEDAL-CN SECTION.                                           
057300     MOVE SPACE                      TO CN-EKHT-W57060                    
057400                                                                          
057500     MOVE IDPGM                      TO CN-EKHT-IDPGM                     
057600     MOVE FUNCTION CURRENT-DATE(1:8) TO CN-EKHT-TIREGDAT                  
057700                                        CN-EKHT-DAVERDAT                  
057800     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
057900     MOVE +1                         TO CN-EKHT-IDSEKVNR                  
058000     MOVE 'W570EKHA'                 TO CN-EKHT-IDCPYTXT                  
058100                                                                          
058200     MOVE '102'                      TO CN-EKHT-KDEKHHT                   
058300     MOVE '107'                      TO CN-EKHT-KDEKSHT                   
058400     MOVE 'DET  '                    TO CN-EKHT-KDEKNIVA                  
058500     MOVE +0                         TO CN-EKHT-IDDISTR                   
058600                                        CN-EKHT-IDKUNDNR                  
058700     MOVE KR-IDDC                    TO CN-EKHT-IDDC-SEND                 
058800     MOVE KRED-IDVERNR               TO CN-EKHT-IDVERGL                   
058900     MOVE +0                         TO CN-EKHT-KDPRODSL                  
059000                                        CN-EKHT-KDPSLLOC                  
059100     MOVE KR-IDARTNR                 TO CN-EKHT-IDARTNR                   
059200     MOVE EK-KDVALISO                TO CN-EKHT-KDVALISO                  
059300     MOVE EK-PRKURS                  TO CN-EKHT-PRKURS                    
059400     IF EK-KVKRRET = +0                                                   
059500         MOVE +0                     TO CN-EKHT-PRARTSTD                  
059600     ELSE                                                                 
059700         MOVE EK-PRARTBEL-PR         TO CN-EKHT-PRARTSTD                  
059800     END-IF                                                               
059900     MOVE ZERO                       TO CN-EKHT-PRARTNTO                  
060000                                        CN-EKHT-PRARTSJK                  
060100                                        CN-EKHT-PRHEMTAG                  
060200                                        CN-EKHT-PRLANDCO                  
060300                                        CN-EKHT-PRINK                     
060400                                        CN-EKHT-PRDIRLON                  
060500                                        CN-EKHT-PRDMTRL                   
060600                                        CN-EKHT-PROVRPAL                  
060700                                        CN-EKHT-SUBEL                     
060800                                        CN-EKHT-SUVAT                     
060900                                        CN-EKHT-IDKONTO                   
061100     MOVE KR-IDAVINR                 TO CN-EKHT-IDAVINR                   
061200     MOVE KR-IDLEVNR                 TO CN-EKHT-IDLEVNR                   
061300     MOVE ZERO                       TO CN-EKHT-KDFRAKT                   
061400                                        CN-EKHT-DAAVIDAT                  
061500                                        CN-EKHT-KDAVVTYP                  
061600                                        CN-EKHT-KDRT                      
061700                                        CN-EKHT-IDORDNR5                  
061800     MOVE SPACE                      TO CN-EKHT-KDSORT                    
061801                                        CN-EKHT-IDKST                     
061810     MOVE SPACE                      TO CN-EKHT-FLDCET                    
061820     MOVE SPACE                      TO CN-EKHT-IDKUNDRF                  
061830     MOVE SPACE                      TO CN-EKHT-IDFAKT-EXP                
061900     MOVE 'CN05'                     TO CN-EKHT-KDTRADP                   
061901                                                                          
061902     MOVE KR-IDARTNR           TO W-IDARTNR                               
061910     PERFORM IMS-GET-WDK601                                               
061920     IF SEGMENT-FINNS                                                     
061940       MOVE ART-KDPRODSL       TO CN-EKHT-KDPRODSL                        
061950     END-IF                                                               
062000                                                                          
062100     IF KRED-IDPTYP = 'AUT'                                               
062200       COMPUTE CN-EKHT-KVANTMOT = KR-KVANTMOT * -1                        
062300       MOVE EK-KVKRRET      TO CN-EKHT-KVANTAL                            
062400       COMPUTE CN-EKHT-KVAVIS   = KR-KVAVIS * -1                          
062500       IF CN-EKHT-KVANTAL NOT = +0                                        
062600         PERFORM S06-SKRIV-PEDAL                                          
062700       END-IF                                                             
062800       MOVE ZERO            TO CN-EKHT-KVANTAL                            
062900       IF EK-SUBESDIFF NOT = ZERO                                         
063000         PERFORM CCD-SKRIV-DIFF-PEDAL                                     
063100       END-IF                                                             
063200       IF EK-SUHEMTAG NOT = ZERO                                          
063300         PERFORM CCE-SKRIV-HEMT-PEDAL                                     
063400       END-IF                                                             
063500       IF EK-SUKPALAG NOT = ZERO                                          
063600         PERFORM CCF-SKRIV-KALK-PEDAL                                     
063700       END-IF                                                             
063800     ELSE                                                                 
063900       MOVE ZERO TO CN-EKHT-KVANTMOT                                      
064000                    CN-EKHT-KVAVIS                                        
064100     END-IF                                                               
064200                                                                          
064300     MOVE ZERO TO CN-EKHT-KVANTAL                                         
064400                                                                          
064500*    IF KRED-PRMOMS NOT = ZERO                                            
064600*      PERFORM CCA-SKRIV-MOMS-PEDAL                                       
064700*    END-IF                                                               
064800     IF KRED-SUMAT-5DEC NOT = ZERO                                        
064900       PERFORM CCC-SKRIV-MATR-PEDAL                                       
065000     END-IF                                                               
065100                                                                          
065200     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
065300                                                                          
065400     IF WS-SUOMK NOT = ZERO                                               
065500       PERFORM CCG-SKRIV-ARB-PEDAL                                        
065600     END-IF                                                               
065700                                                                          
065800     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-EXT-5DEC                       
065900                                                                          
066000     IF WS-SUOMK NOT = ZERO                                               
066100       PERFORM CCJ-SKRIV-TRP-PEDAL                                        
066200     END-IF                                                               
066300                                                                          
066400     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
066500                                            + KRED-SUOMK-EXT-5DEC         
066600                                                                          
066700     PERFORM CCH-SKRIV-SUM-PEDAL                                          
066800     .                                                                    
066900     EJECT                                                                
067000                                                                          
067100 CCA-SKRIV-MOMS-PEDAL SECTION.                                            
067200     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
067300                                                                          
067400     MOVE 'MOMS '                    TO CN-EKHT-KDEKNIVA                  
067500     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
067600                                        CN-EKHT-PRARTSTD                  
067700     MOVE KRED-PRMOMS                TO CN-EKHT-SUBEL                     
067800                                                                          
067900     PERFORM S06-SKRIV-PEDAL                                              
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300 CCC-SKRIV-MATR-PEDAL SECTION.                                            
068400     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
068500     MOVE 'MATR '                    TO CN-EKHT-KDEKNIVA                  
068600     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
068700                                        CN-EKHT-PRARTSTD                  
068800     COMPUTE CN-EKHT-SUBEL ROUNDED = KRED-SUMAT-5DEC                      
068900     PERFORM S06-SKRIV-PEDAL                                              
069000     .                                                                    
069100     EJECT                                                                
069200 CCD-SKRIV-DIFF-PEDAL SECTION.                                            
069300                                                                          
069400     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
069500                                                                          
069600     MOVE 'DIFF '                    TO CN-EKHT-KDEKNIVA                  
069700     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
069800                                        CN-EKHT-PRARTSTD                  
069900     COMPUTE CN-EKHT-SUBEL = EK-SUBESDIFF * -1                            
070000                                                                          
070100     PERFORM S06-SKRIV-PEDAL                                              
070200     .                                                                    
070300     EJECT                                                                
070400 CCE-SKRIV-HEMT-PEDAL SECTION.                                            
070500                                                                          
070600     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
070700                                                                          
070800     MOVE 'HEMT '                    TO CN-EKHT-KDEKNIVA                  
070900     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
071000                                        CN-EKHT-PRARTSTD                  
071100     IF EK-SUHEMTAG  > ZERO                                               
071200       COMPUTE CN-EKHT-SUBEL = EK-SUHEMTAG * -1                           
071300     ELSE                                                                 
071400       MOVE EK-SUHEMTAG TO CN-EKHT-SUBEL                                  
071500     END-IF                                                               
071600                                                                          
071700     PERFORM S06-SKRIV-PEDAL                                              
071800     .                                                                    
071900     EJECT                                                                
072000 CCF-SKRIV-KALK-PEDAL SECTION.                                            
072100                                                                          
072200     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
072300                                                                          
072400     MOVE 'KALK '                    TO CN-EKHT-KDEKNIVA                  
072500     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
072600                                        CN-EKHT-PRARTSTD                  
072700     IF EK-SUKPALAG  > ZERO                                               
072800       COMPUTE CN-EKHT-SUBEL = EK-SUKPALAG * -1                           
072900     ELSE                                                                 
073000       MOVE EK-SUKPALAG TO CN-EKHT-SUBEL                                  
073100     END-IF                                                               
073200                                                                          
073300     PERFORM S06-SKRIV-PEDAL                                              
073400     .                                                                    
073500     EJECT                                                                
073600 CCG-SKRIV-ARB-PEDAL SECTION.                                             
073700                                                                          
073800     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
073900                                                                          
074000     MOVE 'ARB  '                    TO CN-EKHT-KDEKNIVA                  
074100     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
074200                                        CN-EKHT-PRARTSTD                  
074300     MOVE WS-SUOMK                   TO CN-EKHT-SUBEL                     
074400                                                                          
074500     PERFORM S06-SKRIV-PEDAL                                              
074600     .                                                                    
074700     EJECT                                                                
074800 CCH-SKRIV-SUM-PEDAL SECTION.                                             
074900                                                                          
075000     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
075100                                                                          
075200     MOVE 'SUM  '                    TO CN-EKHT-KDEKNIVA                  
075300     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
075400                                        CN-EKHT-PRARTSTD                  
075500*    MOVE KRED-PRMOMS                TO CN-EKHT-SUVAT                     
075600                                                                          
075700     IF KRED-IDPTYP = 'AUT'                                               
076200*      COMPUTE CN-EKHT-SUBEL ROUNDED =                                    
076210*                       EK-SUARTSTD  +                                    
076220*                       EK-SUBESDIFF - KRED-PRMOMS -                      
076230*                       KRED-SUMAT-5DEC - WS-SUOMK                        
076300*      END-COMPUTE                                                        
076310       COMPUTE CN-EKHT-SUBEL ROUNDED =                                    
076320                        EK-SUARTSTD  +                                    
076330                        EK-SUBESDIFF -                                    
076340                        KRED-SUMAT-5DEC - WS-SUOMK                        
076350       END-COMPUTE                                                        
076400     ELSE                                                                 
076500*      COMPUTE CN-EKHT-SUBEL ROUNDED =                                    
076600*             (KRED-SUMAT-5DEC + WS-SUOMK + KRED-PRMOMS) * -1             
076700*      END-COMPUTE                                                        
076710       COMPUTE CN-EKHT-SUBEL ROUNDED =                                    
076720              (KRED-SUMAT-5DEC + WS-SUOMK) * -1                           
076730       END-COMPUTE                                                        
076800     END-IF                                                               
076900                                                                          
077000     PERFORM S06-SKRIV-PEDAL                                              
077100     .                                                                    
077200     EJECT                                                                
077300 CCJ-SKRIV-TRP-PEDAL SECTION.                                             
077400                                                                          
077500     ACCEPT CN-EKHT-TIKLOCK FROM TIME                                     
077600                                                                          
077700     MOVE 'TRP  '                    TO CN-EKHT-KDEKNIVA                  
077800     MOVE ZERO                       TO CN-EKHT-IDARTNR                   
077900                                        CN-EKHT-PRARTSTD                  
078000     MOVE WS-SUOMK                   TO CN-EKHT-SUBEL                     
078100                                                                          
078200     PERFORM S06-SKRIV-PEDAL                                              
078300     .                                                                    
078400     EJECT                                                                
078401                                                                          
078410 CD-SKRIV-DET-PEDAL-US SECTION.                                           
078420     MOVE SPACE                      TO US-EKHT-W57060                    
078430                                                                          
078440     MOVE IDPGM                      TO US-EKHT-IDPGM                     
078450     MOVE FUNCTION CURRENT-DATE(1:8) TO US-EKHT-TIREGDAT                  
078460                                        US-EKHT-DAVERDAT                  
078470     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078480     MOVE +1                         TO US-EKHT-IDSEKVNR                  
078490     MOVE 'W561EKHA'                 TO US-EKHT-IDCPYTXT                  
078491                                                                          
078492     MOVE '102'                      TO US-EKHT-KDEKHHT                   
078493     MOVE '107'                      TO US-EKHT-KDEKSHT                   
078494     MOVE 'DET  '                    TO US-EKHT-KDEKNIVA                  
078495     MOVE +0                         TO US-EKHT-IDDISTR                   
078496                                        US-EKHT-IDKUNDNR                  
078497     MOVE KR-IDDC                    TO US-EKHT-IDDC-SEND                 
078498     MOVE KRED-IDVERNR               TO US-EKHT-IDVERGL                   
078499     MOVE +0                         TO US-EKHT-KDPRODSL                  
078500                                        US-EKHT-KDPSLLOC                  
078501     MOVE KR-IDARTNR                 TO US-EKHT-IDARTNR                   
078502                                        WS-IDARTNR                        
078503     MOVE EK-KDVALISO                TO US-EKHT-KDVALISO                  
078504     MOVE EK-PRKURS                  TO US-EKHT-PRKURS                    
078505     IF EK-KVKRRET = +0                                                   
078506         MOVE +0                     TO US-EKHT-PRARTSTD                  
078507     ELSE                                                                 
078508         MOVE EK-PRARTBEL-PR         TO US-EKHT-PRARTSTD                  
078509     END-IF                                                               
078510     MOVE ZERO                       TO US-EKHT-PRARTNTO                  
078511                                        US-EKHT-PRARTSJK                  
078512                                        US-EKHT-PRHEMTAG                  
078513                                        US-EKHT-PRLANDCO                  
078514                                        US-EKHT-PRINK                     
078515                                        US-EKHT-PRDIRLON                  
078516                                        US-EKHT-PRDMTRL                   
078517                                        US-EKHT-PROVRPAL                  
078518                                        US-EKHT-SUBEL                     
078519                                        US-EKHT-SUVAT                     
078520                                        US-EKHT-IDKONTO                   
078521     MOVE KR-IDAVINR                 TO US-EKHT-IDAVINR                   
078522     MOVE KR-IDLEVNR                 TO US-EKHT-IDLEVNR                   
078523     MOVE ZERO                       TO US-EKHT-KDFRAKT                   
078524                                        US-EKHT-DAAVIDAT                  
078525                                        US-EKHT-KDAVVTYP                  
078526                                        US-EKHT-KDRT                      
078527                                        US-EKHT-IDORDNR5                  
078528     MOVE SPACE                      TO US-EKHT-KDSORT                    
078529                                        US-EKHT-IDKST                     
078530     MOVE SPACE                      TO US-EKHT-FLDCET                    
078531     MOVE SPACE                      TO US-EKHT-IDKUNDRF                  
078532     MOVE SPACE                      TO US-EKHT-IDFAKT-EXP                
078533     MOVE 'US01'                     TO US-EKHT-KDTRADP                   
078534                                                                          
078535     IF KRED-IDPTYP = 'AUT'                                               
078536       COMPUTE US-EKHT-KVANTMOT = KR-KVANTMOT * -1                        
078537       MOVE EK-KVKRRET      TO US-EKHT-KVANTAL                            
078538       COMPUTE US-EKHT-KVAVIS   = KR-KVAVIS * -1                          
078539       IF US-EKHT-KVANTAL NOT = +0                                        
078540         PERFORM S07-SKRIV-PEDAL-US                                       
078541       END-IF                                                             
078542       MOVE ZERO            TO US-EKHT-KVANTAL                            
078543       IF EK-SUBESDIFF NOT = ZERO                                         
078544         PERFORM CDD-SKRIV-DIFF-PEDAL                                     
078545       END-IF                                                             
078546       IF EK-SUHEMTAG NOT = ZERO                                          
078547         PERFORM CDE-SKRIV-HEMT-PEDAL                                     
078548       END-IF                                                             
078549       IF EK-SUKPALAG NOT = ZERO                                          
078550         PERFORM CDF-SKRIV-KALK-PEDAL                                     
078551       END-IF                                                             
078552     ELSE                                                                 
078553       MOVE ZERO TO US-EKHT-KVANTMOT                                      
078554                    US-EKHT-KVAVIS                                        
078555     END-IF                                                               
078556                                                                          
078557     MOVE ZERO TO US-EKHT-KVANTAL                                         
078558                                                                          
078559     IF KRED-PRMOMS NOT = ZERO                                            
078560       PERFORM CDA-SKRIV-MOMS-PEDAL                                       
078561     END-IF                                                               
078562     IF KRED-SUMAT-5DEC NOT = ZERO                                        
078563       PERFORM CDC-SKRIV-MATR-PEDAL                                       
078564     END-IF                                                               
078565                                                                          
078566     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
078567                                                                          
078568     IF WS-SUOMK NOT = ZERO                                               
078569       PERFORM CDG-SKRIV-ARB-PEDAL                                        
078570     END-IF                                                               
078571                                                                          
078572     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-EXT-5DEC                       
078573                                                                          
078574     IF WS-SUOMK NOT = ZERO                                               
078575       PERFORM CDJ-SKRIV-TRP-PEDAL                                        
078576     END-IF                                                               
078577                                                                          
078578     COMPUTE WS-SUOMK ROUNDED = KRED-SUOMK-INT-5DEC                       
078579                                            + KRED-SUOMK-EXT-5DEC         
078580                                                                          
078581     PERFORM CDH-SKRIV-SUM-PEDAL                                          
078582     .                                                                    
078583     EJECT                                                                
078584                                                                          
078585 CDA-SKRIV-MOMS-PEDAL SECTION.                                            
078586     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078587                                                                          
078588     MOVE 'MOMS '                    TO US-EKHT-KDEKNIVA                  
078589     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078590                                        US-EKHT-PRARTSTD                  
078591     MOVE KRED-PRMOMS                TO US-EKHT-SUBEL                     
078592                                                                          
078593     PERFORM S07-SKRIV-PEDAL-US                                           
078594     .                                                                    
078595     EJECT                                                                
078596                                                                          
078597 CDC-SKRIV-MATR-PEDAL SECTION.                                            
078598     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078599     MOVE 'MATR '                    TO US-EKHT-KDEKNIVA                  
078600     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078601                                        US-EKHT-PRARTSTD                  
078602     COMPUTE US-EKHT-SUBEL ROUNDED = KRED-SUMAT-5DEC                      
078603     PERFORM S07-SKRIV-PEDAL-US                                           
078604     .                                                                    
078605     EJECT                                                                
078606 CDD-SKRIV-DIFF-PEDAL SECTION.                                            
078607                                                                          
078608     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078609                                                                          
078610     MOVE 'DIFF '                    TO US-EKHT-KDEKNIVA                  
078611     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078612                                        US-EKHT-PRARTSTD                  
078613     COMPUTE US-EKHT-SUBEL = EK-SUBESDIFF * -1                            
078614                                                                          
078615     PERFORM S07-SKRIV-PEDAL-US                                           
078616     .                                                                    
078617     EJECT                                                                
078618 CDE-SKRIV-HEMT-PEDAL SECTION.                                            
078619                                                                          
078620     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078621                                                                          
078622     MOVE 'HEMT '                    TO US-EKHT-KDEKNIVA                  
078623     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078624                                        US-EKHT-PRARTSTD                  
078625     IF EK-SUHEMTAG  > ZERO                                               
078626       COMPUTE US-EKHT-SUBEL = EK-SUHEMTAG * -1                           
078627     ELSE                                                                 
078628       MOVE EK-SUHEMTAG TO US-EKHT-SUBEL                                  
078629     END-IF                                                               
078630                                                                          
078631     PERFORM S07-SKRIV-PEDAL-US                                           
078632     .                                                                    
078633     EJECT                                                                
078634 CDF-SKRIV-KALK-PEDAL SECTION.                                            
078635                                                                          
078636     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078637                                                                          
078638     MOVE 'KALK '                    TO US-EKHT-KDEKNIVA                  
078639     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078640                                        US-EKHT-PRARTSTD                  
078641     IF EK-SUKPALAG  > ZERO                                               
078642       COMPUTE US-EKHT-SUBEL = EK-SUKPALAG * -1                           
078643     ELSE                                                                 
078644       MOVE EK-SUKPALAG TO US-EKHT-SUBEL                                  
078645     END-IF                                                               
078646                                                                          
078647     PERFORM S07-SKRIV-PEDAL-US                                           
078648     .                                                                    
078649     EJECT                                                                
078650 CDG-SKRIV-ARB-PEDAL SECTION.                                             
078651                                                                          
078652     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078653                                                                          
078654     MOVE 'ARB  '                    TO US-EKHT-KDEKNIVA                  
078655     MOVE WS-IDARTNR                 TO US-EKHT-IDARTNR                   
078656                                        US-EKHT-PRARTSTD                  
078657     MOVE WS-SUOMK                   TO US-EKHT-SUBEL                     
078658                                                                          
078659     PERFORM S07-SKRIV-PEDAL-US                                           
078660     .                                                                    
078661     EJECT                                                                
078662 CDH-SKRIV-SUM-PEDAL SECTION.                                             
078663                                                                          
078664     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078665                                                                          
078666     MOVE 'SUM  '                    TO US-EKHT-KDEKNIVA                  
078667     MOVE ZERO                       TO US-EKHT-IDARTNR                   
078668                                        US-EKHT-PRARTSTD                  
078669     MOVE KRED-PRMOMS                TO US-EKHT-SUVAT                     
078670                                                                          
078671     IF KRED-IDPTYP = 'AUT'                                               
078674       COMPUTE US-EKHT-SUBEL ROUNDED =                                    
078675                        EK-SUARTSTD  +                                    
078676                        EK-SUBESDIFF - KRED-PRMOMS -                      
078677                        KRED-SUMAT-5DEC - WS-SUOMK                        
078678       END-COMPUTE                                                        
078679     ELSE                                                                 
078680       COMPUTE US-EKHT-SUBEL ROUNDED =                                    
078681              (KRED-SUMAT-5DEC + WS-SUOMK + KRED-PRMOMS) * -1             
078682       END-COMPUTE                                                        
078683     END-IF                                                               
078684                                                                          
078685     PERFORM S07-SKRIV-PEDAL-US                                           
078686     .                                                                    
078687     EJECT                                                                
078688 CDJ-SKRIV-TRP-PEDAL SECTION.                                             
078689                                                                          
078690     ACCEPT US-EKHT-TIKLOCK FROM TIME                                     
078691                                                                          
078692     MOVE 'TRP  '                    TO US-EKHT-KDEKNIVA                  
078693     MOVE ZERO                       TO US-EKHT-IDARTNR                   
078694                                        US-EKHT-PRARTSTD                  
078695     MOVE WS-SUOMK                   TO US-EKHT-SUBEL                     
078696                                                                          
078697     PERFORM S07-SKRIV-PEDAL-US                                           
078698     .                                                                    
078699     EJECT                                                                
078700 Z-FINIT SECTION.                                                         
078701     CLOSE W4263501                                                       
078710           W4263502                                                       
078800           W4263503                                                       
078900           W4263504                                                       
078910           W4263505                                                       
079000     SKIP2                                                                
079100     MOVE 'S' TO POSTSUM-OPKOD                                            
079200     CALL POSTSUM USING POSTSUM-PARM                                      
079300     .                                                                    
079400     EJECT                                                                
079500 S01-LAES-W4263501  SECTION.                                              
079600     SKIP2                                                                
079700     READ W4263501 INTO IN-AREA                                           
079800     AT END                                                               
079900        SET END-OF-W4263501 TO TRUE                                       
080000                                                                          
080100     NOT AT END                                                           
080200        MOVE 'W4263501' TO POSTSUM-FDNAMN                                 
080300        MOVE 'W42635D1' TO POSTSUM-DDNAMN2                                
080400        MOVE IN-KEK-IDPTYP TO POSTSUM-TRANSTYP                            
080500        CALL POSTSUM USING POSTSUM-PARM                                   
080600     END-READ                                                             
080700     .                                                                    
080800     EJECT                                                                
080900 S05-SKRIV-PEDAL SECTION.                                                 
081000     SKIP2                                                                
081100     WRITE UT-POST FROM UT-AREA                                           
081200                                                                          
081300     MOVE EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                              
081400     MOVE 'W4263502'     TO POSTSUM-FDNAMN                                
081500     MOVE 'W42635D2'     TO POSTSUM-DDNAMN2                               
081600     CALL POSTSUM USING POSTSUM-PARM                                      
081700     .                                                                    
081800     EJECT                                                                
081900 S06-SKRIV-PEDAL SECTION.                                                 
082000     SKIP2                                                                
082100     WRITE UT-CN-POST FROM UT-CN-AREA                                     
082200                                                                          
082300     MOVE CN-EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                           
082400     MOVE 'W4263504'        TO POSTSUM-FDNAMN                             
082500     MOVE 'W42635D4'        TO POSTSUM-DDNAMN2                            
082600     CALL POSTSUM USING POSTSUM-PARM                                      
082700     .                                                                    
082800     EJECT                                                                
082810 S07-SKRIV-PEDAL-US SECTION.                                              
082820     SKIP2                                                                
082830     WRITE UT-US-POST FROM UT-US-AREA                                     
082840                                                                          
082850     MOVE US-EKHT-CT-IDPTYP TO POSTSUM-TRANSTYP                           
082860     MOVE 'W4263505'        TO POSTSUM-FDNAMN                             
082870     MOVE 'W42635D5'        TO POSTSUM-DDNAMN2                            
082880     CALL POSTSUM USING POSTSUM-PARM                                      
082890     .                                                                    
082891     EJECT                                                                
082900* --- IMS SEKTIONER ---                                                   
083000                                                                          
083100 IMS-GU-W6H701 SECTION.                                                   
083200     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
083300          DELIMITED BY SIZE INTO SSA1                                     
083400     MOVE '  ' TO GODK-STATUSKODER                                        
083500     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA1 SSA1                     
083600     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     SKIP2                                                                
084000 IMS-GET-W6H712 SECTION.                                                  
084100     MOVE 'W6H712   '         TO SSA1                                     
084200     MOVE '  ' TO GODK-STATUSKODER                                        
084300     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-AREA2 SSA1                    
084400     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700     EJECT                                                                
084800 IMS-GU-W6H721 SECTION.                                                   
084900     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
085000          DELIMITED BY SIZE INTO SSA1                                     
085100     MOVE 'W6H712   '          TO SSA2                                    
085200     STRING 'W6H721  (IDSEGMNR =' W-IDSEGMNR-X ')'                        
085300          DELIMITED BY SIZE INTO SSA3                                     
085400     MOVE '  ' TO GODK-STATUSKODER                                        
085500     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA3 SSA1 SSA2 SSA3           
085600     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     .                                                                    
085900     EJECT                                                                
086000 IMS-GET-WDK601 SECTION.                                                  
086100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
086200          DELIMITED BY SIZE INTO SSA1                                     
086300     MOVE '  GE'           TO GODK-STATUSKODER                            
086400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
086500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
086600     PERFORM IMS-STATUSKONTROLL                                           
086700     .                                                                    
086800     EJECT                                                                
086900 IMS-GET-WDK611 SECTION.                                                  
087000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
087100          DELIMITED BY SIZE INTO SSA1                                     
087200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
087300          DELIMITED BY SIZE INTO SSA2                                     
087400     MOVE '  GE'           TO GODK-STATUSKODER                            
087500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
087600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     .                                                                    
087900     EJECT                                                                
088000 IMS-STATUSKONTROLL SECTION.                                              
088100     SKIP2                                                                
088200     SET STATUS-IX TO 1                                                   
088300     SEARCH GODK-STATUS                                                   
088400       AT END                                                             
088500         CALL FELLOG                                                      
088600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
088700     END-SEARCH                                                           
088800     .                                                                    
