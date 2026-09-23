000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411KAMP.                                                
000500 AUTHOR.         ANNELIE ENGLUND                                          
000600 DATE-WRITTEN.   MAJ 1990                                                 
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*                                                                         
001200*        PROGRAMMET TAR EMOT KAMPANJORDERRADER OCH TPO4-RADER             
001300*        VID ORDER-ENTRY.                                                 
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
001700*        PROGRAMMET LÄSER OCH                                             
001800*                   UPPDATERAR WDM2           KAMPANJREGISTER             
002100*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)  TRANSAKTIONBAS              
002200*                                                                         
002300*                                                                         
002400*    LÄNKAREA: W411KAMP                                                   
002500                                                                          
002600* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -COPY WY2000W1                                                       
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(08)   VALUE 'W411KAMP'.            
003410 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
003420 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
003500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  EXEKV-TID                   PIC X(6).                                
004100                                                                          
004200 77  W-TIREGDAT                  PIC 9(6).                                
004300                                                                          
004400 77  W-ANTAL                     PIC 9(7).                                
004500*      --- VALID IDDC CODES                                               
004600*                                                                         
004700*01    -COPY WWDCKONS                                                     
004800       EJECT                                                              
004900                                                                          
005000 01  GENERELLA-SUBPROGRAM.                                                
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005500                                                                          
005600     EJECT                                                                
005700*    --- PARAMETRAR TILL ABEND                                            
005800                                                                          
005900 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
006000                                                                          
006100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
006200                                                                          
006300*01  -COPY WDATAREA                                                       
006400                                                                          
006500     EJECT                                                                
006600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006700*                                                                         
006800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006900     SKIP3                                                                
007000 01  NYCKLAR-TILL-DLI.                                                    
007100                                                                          
007200     03  W-WDA501KY-MIN.                                                  
007300         05  W-IDDISTR-MIN       PIC S9(5) COMP-3   VALUE ZERO.           
007400         05  W-IDKUNDNR-MIN      PIC S9(7) COMP-3   VALUE ZERO.           
007500         05  W-IDKUNDRF-MIN      PIC X(10)          VALUE SPACE.          
007600         05  W-IDARTNR-MIN       PIC S9(9) COMP-3   VALUE ZERO.           
007700         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3   VALUE ZERO.           
007800                                                                          
007900     03  W-WDA501KY-MAX.                                                  
008000         05  W-IDDISTR-MAX       PIC S9(5) COMP-3   VALUE ZERO.           
008100         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3   VALUE ZERO.           
008200         05  W-IDKUNDRF-MAX      PIC X(10)          VALUE ZERO.           
008300         05  W-IDARTNR-MAX       PIC S9(9) COMP-3   VALUE ZERO.           
008400         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3   VALUE ZERO.           
008500                                                                          
008600     03  W-IDARTNR-X.                                                     
008700         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
008800                                                                          
008900     03  W-KDSTARAD-X.                                                    
009000         05  W-KDSTARAD          PIC X        VALUE SPACE.                
009100                                                                          
009200     03  W-TITPO-X.                                                       
009300         05  W-TITPO             PIC S9(7)    VALUE ZERO COMP-3.          
009400                                                                          
009500     03  W-KDTPOTYP-X.                                                    
009600         05  W-KDTPOTYP          PIC S9       VALUE ZERO COMP-3.          
009700                                                                          
009710     03  W-WDM201-X.                                                      
009720         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
009730         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
009740                                                                          
009750     03  W-WDM211-IDARTNR-X.                                              
009760         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
009770                                                                          
009771     03  W-WDM221-X.                                                      
009772         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
009773         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
009774         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
009775         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
009776                                                                          
013200     EJECT                                                                
013300*   --- AREOR TILL TRANSAR                                                
013400     SKIP2                                                                
013500*01  -COPY WDGZRYA                                                        
013600*                                                                         
013700     EJECT                                                                
013800*01  -COPY W092P001  -PRE SORT-                                           
013900*                                                                         
014000     EJECT                                                                
014100*    --- STATUS-KOD FRÅN IMS                                              
014200 01  STATUS-WS                   PIC XX.                                  
014300     88  SEGMENT-FINNS                       VALUE '  '.                  
014400     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
014500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(128).                              
015200 01  SSA2                        PIC X(128).                              
015210 01  SSA3                        PIC X(128).                              
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
016110 01  FILLER                    PIC X(16)   VALUE 'RAD-WDA501'.            
016120 01  DLI-IO-WDA501.                                                       
016200*    03  WLORDP01  -COPY WDA501                                           
016300     EJECT                                                                
016400 01  FILLER                    PIC X(16)   VALUE 'ZZAC-WDGZ01'.           
016500 01  DLI-IO-WDGZ01.                                                       
017000*    03  WLZZAC01  -COPY WDGZ01     -PRE ZZAC-                            
017100     EJECT                                                                
017200 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
017300 01  DLI-IO-WDM201.                                                       
017310*    03 -COPY WDM201                                                      
017320     EJECT                                                                
017330 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
017340 01  DLI-IO-WDM211.                                                       
017350*    03 -COPY WDM211                                                      
017360     EJECT                                                                
017370 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
017380 01  DLI-IO-WDM221.                                                       
017390*    03 -COPY WDM221                                                      
017391     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500                                                                          
017600*                                                                         
017700*   -COPY W411KAMP -PRE W411-                                             
017800*                                                                         
017900     EJECT                                                                
018000*01  -COPY W0008      -PRE ORDP-                                          
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018900*01  -COPY W0008      -PRE ZZAC-                                          
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008      -PRE WDM2-                                          
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING W411-KAMP-W411KAMP ORDP-PCB                    
019600                           ZZAC-PCB WDM2-PCB.                             
019700                                                                          
019800     MOVE ZERO   TO W411-KAMP-KDORDBEK                                    
019900     MOVE NEJ    TO W411-KAMP-FLKLAR                                      
020100     IF W411-KAMP-IDSYSTEM NOT = 'OREL'                                   
020200        IF W411-KAMP-FLORDSPE NOT = JA                                    
020210       AND W411-KAMP-FLOVRLEV NOT = JA                                    
020300           IF W411-KAMP-IDKAMPRF > 0 AND W411-KAMP-FLFORBI = NEJ          
020400              PERFORM A-INIT                                              
020500              PERFORM B-ANTALSTABELL-KONTROLL                             
020600              IF W411-KAMP-KDORDBEK = ZERO                                
020610             AND W411-KAMP-KDTPOTYP = 4                                   
020700                 PERFORM C-KONTROLLERA-DUBBLETT-RAD                       
020800              END-IF                                                      
020900              IF W411-KAMP-KDORDBEK = ZERO                                
020910             AND W411-KAMP-KDTPOTYP = 4                                   
021000                 PERFORM D-KONTROLLERA-TITPO                              
021100              END-IF                                                      
021200              IF W411-KAMP-KDORDBEK = ZERO                                
021300                 PERFORM E-RAD-BEHANDLING                                 
021400              END-IF                                                      
021500              IF W411-KAMP-KDORDBEK = ZERO                                
021600                 PERFORM F-UPPDATERA                                      
021700              END-IF                                                      
021800           END-IF                                                         
021900        END-IF                                                            
022000     END-IF                                                               
022100                                                                          
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     ACCEPT EXEKV-TID FROM TIME                                           
022800                                                                          
022900     MOVE 'IDAG' TO DAT-KDDATFORM                                         
023000     MOVE ZERO   TO DAT-I-TIDATUM                                         
023100     CALL WDATKONV USING DAT-KDDATFORM,                                   
023200                         DAT-I-TIDATUM,                                   
023300                         DAT-O-TIDATUM,                                   
023400                         DAT-KDSVAR                                       
023500     IF DAT-KDSVAR-OK                                                     
023600       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
023700     ELSE                                                                 
023800       MOVE 'FEL FRÅN SUBPROGRAM W411KAMP I SECTION A' TO FELTEXT         
023900       CALL ABEND USING RKOD-ABEND                                        
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 B-ANTALSTABELL-KONTROLL SECTION.                                         
024400                                                                          
024500     MOVE W411-KAMP-IDKAMPRF TO W-KAMP-IDKAMPRF                           
024510     MOVE W411-KAMP-IDDC     TO W-KAMP-IDDC                               
024600     MOVE W411-KAMP-IDARTNR  TO W-KART-IDARTNR                            
024700                                                                          
024800     PERFORM IMS-GU-WDM211                                                
024900     IF SEGMENT-SAKNAS                                                    
025000       MOVE 75 TO W411-KAMP-KDORDBEK                                      
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 C-KONTROLLERA-DUBBLETT-RAD SECTION.                                      
025500                                                                          
025600     MOVE W411-KAMP-IDDISTR  TO W-IDDISTR-MIN                             
025700                                W-IDDISTR-MAX                             
025800     MOVE W411-KAMP-IDKUNDNR TO W-IDKUNDNR-MIN                            
025900                                W-IDKUNDNR-MAX                            
026000     MOVE SPACE              TO W-IDKUNDRF-MIN                            
026100                                W-IDKUNDRF-MAX                            
026200     MOVE W411-KAMP-IDKUNDRF(3:5) TO W-IDKUNDRF-MIN                       
026300                                W-IDKUNDRF-MAX                            
026400     MOVE W411-KAMP-IDARTNR  TO W-IDARTNR-MIN                             
026500                                W-IDARTNR-MAX                             
026600     MOVE ZERO               TO W-IDLOPNR-MIN                             
026700     MOVE 999                TO W-IDLOPNR-MAX                             
026800     MOVE 4                  TO W-KDTPOTYP                                
026900     MOVE 1                  TO W-KDSTARAD                                
027000     MOVE W411-KAMP-TITPO    TO W-TITPO                                   
027100     PERFORM IMS-GU-ORDP-WDA501                                           
027200     IF SEGMENT-FINNS                                                     
027300       MOVE 72 TO W411-KAMP-KDORDBEK                                      
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 D-KONTROLLERA-TITPO SECTION.                                             
027800                                                                          
027900     MOVE W411-KAMP-IDKAMPRF TO W-KAMP-IDKAMPRF                           
027910     MOVE W411-KAMP-IDDC     TO W-KAMP-IDDC                               
028000     PERFORM IMS-GU-WDM201                                                
028100                                                                          
028110     IF SEGMENT-FINNS                                                     
028200       MOVE W411-KAMP-TITPO TO TMP1-YYMMDD                                
028300       MOVE KAMP-TISTADAT   TO TMP2-YYMMDD                                
028400       PERFORM WY2000P1                                                   
028500       IF TMP1-YYMMDD < TMP2-YYMMDD OR                                    
028600         (KAMP-TISTODAT > +0 AND W411-KAMP-TITPO > KAMP-TISTODAT)         
028700         MOVE 75           TO W411-KAMP-KDORDBEK                          
028800       END-IF                                                             
028810     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 E-RAD-BEHANDLING SECTION.                                                
029200                                                                          
029300     COMPUTE W-ANTAL = W411-KAMP-KVBEART-Q + KART-KVBEART-KUND            
029400                                                                          
029500     IF W411-KAMP-KDTPOTYP NOT = 4                                        
029600       SUBTRACT KART-KVBEART-TPO4 FROM W-ANTAL                            
029700     END-IF                                                               
029800                                                                          
029900     IF W-ANTAL > KART-KVBEART-KAMP                                       
030000       MOVE 76 TO W411-KAMP-KDORDBEK                                      
030100     ELSE                                                                 
030200       PERFORM EA-KOLLA-MARKNAD                                           
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 EA-KOLLA-MARKNAD SECTION.                                                
030700                                                                          
030800     MOVE W411-KAMP-IDKAMPRF TO W-KAMP-IDKAMPRF                           
030810     MOVE W411-KAMP-IDDC     TO W-KAMP-IDDC                               
031000     MOVE W411-KAMP-IDARTNR  TO W-KART-IDARTNR                            
031100     MOVE W411-KAMP-IDDISTR  TO W-KMRK-IDDISTR-FOM                        
031200     MOVE W411-KAMP-IDDISTR  TO W-KMRK-IDDISTR-TOM                        
031300     MOVE W411-KAMP-IDKUNDNR TO W-KMRK-IDKUNDNR-FOM                       
031400     MOVE W411-KAMP-IDKUNDNR TO W-KMRK-IDKUNDNR-TOM                       
031500                                                                          
031600     PERFORM S20-FINN-INTERVALL                                           
031700                                                                          
031710     IF W411-KAMP-KDORDBEK = ZERO                                         
031800       PERFORM IMS-GHU-WDM221                                             
031900                                                                          
032000       IF SEGMENT-FINNS                                                   
032100         COMPUTE W-ANTAL =                                                
032200         W411-KAMP-KVBEART-Q + KMRK-KVBEART-KUND                          
032300         IF W-ANTAL > KMRK-KVBEART-KAMP                                   
032400           MOVE 76 TO W411-KAMP-KDORDBEK                                  
032500         ELSE                                                             
032600           ADD W411-KAMP-KVBEART-Q TO KMRK-KVBEART-KUND                   
032700           PERFORM IMS-REPL-WDM221                                        
032800         END-IF                                                           
032900       ELSE                                                               
033000         MOVE 75 TO W411-KAMP-KDORDBEK                                    
033100       END-IF                                                             
033110     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 F-UPPDATERA SECTION.                                                     
033500                                                                          
033501     MOVE W411-KAMP-IDKAMPRF TO W-KAMP-IDKAMPRF                           
033502     MOVE W411-KAMP-IDDC     TO W-KAMP-IDDC                               
033503     MOVE W411-KAMP-IDARTNR  TO W-KART-IDARTNR                            
033510     PERFORM IMS-GHU-WDM211                                               
033520     IF SEGMENT-FINNS                                                     
033600       IF W411-KAMP-KDTPOTYP = 4                                          
033700          ADD W411-KAMP-KVBEART-Q TO KART-KVBEART-KUND                    
033800          ADD W411-KAMP-KVBEART-Q TO KART-KVBEART-TPO4                    
033900          PERFORM IMS-REPL-WDM211                                         
034000                                                                          
034100          PERFORM FA-LAGG-UPP-RAD                                         
034200          IF W411-KAMP-IDSYSTEM NOT = 'VR'                                
034300            PERFORM FB-SKAPA-TRANS-TILL-VR                                
034400          END-IF                                                          
034500       ELSE                                                               
034600          ADD W411-KAMP-KVBEART-Q TO KART-KVBEART-KUND                    
034700          PERFORM IMS-REPL-WDM211                                         
034800       END-IF                                                             
034810     END-IF                                                               
034900      .                                                                   
035000      EJECT                                                               
035100 FA-LAGG-UPP-RAD SECTION.                                                 
035200                                                                          
035300     PERFORM FAB-SKAPA-RADKO                                              
035400     PERFORM IMS-ISRT-ORDP-WDA501                                         
035500     IF SEGMENT-FINNS-REDAN                                               
035600       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
035700         ADD 1 TO RAD-IDLOPNR                                             
035800         PERFORM IMS-ISRT-ORDP-WDA501                                     
035900       END-PERFORM                                                        
036000     END-IF                                                               
036100     MOVE JA TO W411-KAMP-FLKLAR                                          
036200     .                                                                    
036300     EJECT                                                                
036400 FAB-SKAPA-RADKO SECTION.                                                 
036500                                                                          
036600     MOVE W411-KAMP-IDDISTR   TO RAD-IDDISTR                              
036700     MOVE W411-KAMP-IDKUNDNR  TO RAD-IDKUNDNR                             
036800     MOVE SPACE               TO RAD-IDKUNDRF                             
036900     MOVE W411-KAMP-IDKUNDRF (3:5) TO RAD-IDORDNR5                        
037000     MOVE W411-KAMP-IDARTNR   TO RAD-IDARTNR                              
037100     MOVE 1                   TO RAD-IDLOPNR                              
037200     MOVE W411-KAMP-BERADREF  TO RAD-BERADREF                             
037300     MOVE NEJ                 TO RAD-FLERS                                
037400     MOVE W411-KAMP-IDANSK    TO RAD-IDANSK                               
037500     MOVE W411-KAMP-IDKONTO   TO RAD-IDKONTO                              
037600     MOVE W411-KAMP-IDANALYS  TO RAD-IDANALYS                             
037700     MOVE W411-KAMP-IDKST     TO RAD-IDKST                                
037800     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
037900     MOVE WC-CDC-SE           TO RAD-IDDC                                 
038000                                 RAD-IDDC-RO                              
038100     MOVE W411-KAMP-KDDSP     TO RAD-KDDSP                                
038200     MOVE W411-KAMP-KDFAKTYP  TO RAD-KDFAKTYP                             
038300     MOVE W411-KAMP-KDFRAKT   TO RAD-KDFRAKT                              
038400     MOVE 1                   TO RAD-KDKVBRYT                             
038500     MOVE W411-KAMP-KDORDING  TO RAD-KDORDING                             
038600     MOVE W411-KAMP-KDORDKL   TO RAD-KDORDKL                              
038700     MOVE W411-KAMP-KDPRODSL  TO RAD-KDPRODSL                             
038800     MOVE 20                  TO RAD-KDRAPRIO                             
038900     MOVE ZERO                TO RAD-KDROO                                
039000     MOVE 1                   TO RAD-KDSTARAD                             
039100     MOVE W411-KAMP-KDTPOTYP  TO RAD-KDTPOTYP                             
039200     MOVE W411-KAMP-KDVRINFO  TO RAD-KDVRINFO                             
039300     MOVE W411-KAMP-KVBEART-Q TO RAD-KVART                                
039400                                 RAD-KVBEART-Q                            
039500     MOVE ZERO                TO RAD-KVRO                                 
039600     MOVE W411-KAMP-PRARTNTO  TO RAD-PRARTNTO                             
039700     MOVE W411-KAMP-DEAL-PR-LINE TO RAD-DEAL-PR-LINE                      
039800     MOVE W411-KAMP-REKSIFFR  TO RAD-REKSIFFR                             
039900     MOVE ZERO                TO RAD-TIAVBOKN                             
040000     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
040100     MOVE ZERO                TO RAD-TIRES                                
040200     MOVE ZERO                TO RAD-DARODAT                              
040300     MOVE W411-KAMP-TITPO     TO RAD-TITPO                                
040400     MOVE W411-KAMP-KDPRTYP   TO RAD-KDPRTYP                              
040500     MOVE W411-KAMP-BEVOLREF  TO RAD-BEVOLREF                             
040600     MOVE W411-KAMP-FLINVEST  TO RAD-FLINVEST                             
040700     MOVE W411-KAMP-FLPRTILL  TO RAD-FLPRTILL                             
040800     MOVE JA                  TO RAD-FLTPOBEK                             
040900     MOVE W411-KAMP-BEKUNDRF  TO RAD-BEKUNDRF                             
041000     MOVE W411-KAMP-IDKAMPRF  TO RAD-IDKAMPRF                             
041100     MOVE W411-KAMP-IDLEVNR   TO RAD-IDLEVNR                              
041200     MOVE W411-KAMP-IDSYSTEM  TO RAD-IDSYSTEM                             
041300     MOVE EXEKV-TID           TO RAD-TIREGTID                             
041400     MOVE ZERO                TO RAD-DASENDAT                             
041500                                 RAD-TISENBEK-KL                          
041600     MOVE SPACE               TO RAD-KDORDTYP-LDC                         
041700     MOVE ZERO                TO RAD-TIREPDAT                             
041800     MOVE SPACE               TO RAD-IDKUNDRF-WIP                         
042000     MOVE SPACE               TO RAD-CLEARGROUP                           
042001     MOVE SPACE               TO RAD-KDROPACK                             
042002     MOVE SPACE               TO RAD-IDARBREF                             
042010     MOVE +0                  TO RAD-PRAVCOST                             
042100     .                                                                    
042200                                                                          
042300 FB-SKAPA-TRANS-TILL-VR SECTION.                                          
042400                                                                          
042500     MOVE +1         TO ZZAC-IDLOGLOP                                     
042600     MOVE 'RYA'      TO RYA-IDPTYP                                        
042700                        ZZAC-IDPTYP                                       
042800     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
042900     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
043000     MOVE W411-KAMP-IDDISTR TO RYA-IDDISTR                                
043100     MOVE W411-KAMP-IDKUNDNR TO RYA-IDKUNDNR                              
043200     MOVE W411-KAMP-IDKUNDRF TO RYA-IDKUNDRF                              
043300     MOVE W411-KAMP-IDARTNR TO RYA-IDARTNR                                
043400     MOVE W411-KAMP-REKSIFFR TO RYA-REKSIFFR                              
043500     MOVE W411-KAMP-KVBEART-Q TO RYA-KVBEART                              
043600     MOVE W411-KAMP-TITPO TO RYA-TITPO                                    
043700     MOVE W411-KAMP-KDTPOTYP TO RYA-KDTPOTYP                              
043800     MOVE 0              TO RYA-KDVRTPO                                   
043900     MOVE W411-KAMP-KDVRINFO TO RYA-KDVRINFO                              
044000                                                                          
044100     MOVE RYA-WDGZRYA   TO ZZAC-LOGGPOST                                  
044200     MOVE SPACE         TO ZZAC-SORTPOST                                  
044300     PERFORM IMS-ISRT-ZZAC-WDG6                                           
044400     IF SEGMENT-FINNS-REDAN                                               
044500       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
044600         ADD +1 TO ZZAC-IDLOGLOP                                          
044700         PERFORM IMS-ISRT-ZZAC-WDG6                                       
044800       END-PERFORM                                                        
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200                                                                          
045300 S20-FINN-INTERVALL SECTION.                                              
045400                                                                          
045410     MOVE W411-KAMP-IDKAMPRF TO W-KAMP-IDKAMPRF                           
045420     MOVE W411-KAMP-IDDC     TO W-KAMP-IDDC                               
045430     MOVE W411-KAMP-IDARTNR  TO W-KART-IDARTNR                            
045500     PERFORM IMS-GU-WDM211                                                
045501     IF SEGMENT-FINNS                                                     
045510       PERFORM IMS-GNP-WDM221                                             
045600       IF SEGMENT-FINNS                                                   
045700         PERFORM UNTIL SEGMENT-SAKNAS                                     
045800           IF  W411-KAMP-IDDISTR > KMRK-IDDISTR-TOM                       
045900           OR  W411-KAMP-IDDISTR < KMRK-IDDISTR-FOM                       
046000             CONTINUE                                                     
046100           ELSE                                                           
046200             IF  W411-KAMP-IDDISTR = KMRK-IDDISTR-TOM                     
046300             AND W411-KAMP-IDKUNDNR > KMRK-IDKUNDNR-TOM                   
046400               CONTINUE                                                   
046500             ELSE                                                         
046600               IF  W411-KAMP-IDDISTR = KMRK-IDDISTR-FOM                   
046700               AND W411-KAMP-IDKUNDNR < KMRK-IDKUNDNR-FOM                 
046800                 CONTINUE                                                 
046900               ELSE                                                       
047000                 MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM             
047100                 MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM             
047200                 MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM            
047300                 MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM            
047400               END-IF                                                     
047500             END-IF                                                       
047600           END-IF                                                         
047700           PERFORM IMS-GNP-WDM221                                         
047800         END-PERFORM                                                      
047810       ELSE                                                               
047820         MOVE 76 TO W411-KAMP-KDORDBEK                                    
047910       END-IF                                                             
047920     ELSE                                                                 
047930       MOVE 76 TO W411-KAMP-KDORDBEK                                      
047940     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200                                                                          
048300* --- IMS SEKTIONER ---                                                   
048400     SKIP3                                                                
048401 IMS-GU-WDM201 SECTION.                                                   
048402     MOVE 'IMS-GU-WDM201      ' TO DBS-SECTION                            
048403                                                                          
048404     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
048405          DELIMITED BY SIZE INTO SSA1                                     
048408     MOVE '  GE'              TO GODK-STATUSKODER                         
048409     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
048410     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
048411     PERFORM IMS-STATUSKONTROLL                                           
048412     .                                                                    
048413                                                                          
048426 IMS-GU-WDM211 SECTION.                                                   
048427     MOVE 'IMS-GU-WDM211      ' TO DBS-SECTION                            
048428                                                                          
048429     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
048430          DELIMITED BY SIZE INTO SSA1                                     
048431     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
048432          DELIMITED BY SIZE INTO SSA2                                     
048433     MOVE '  GE'              TO GODK-STATUSKODER                         
048434     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
048435     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
048436     PERFORM IMS-STATUSKONTROLL                                           
048437     .                                                                    
048438                                                                          
048439 IMS-GNP-WDM221 SECTION.                                                  
048440     MOVE 'IMS-GNP-WDM221             ' TO DBS-SECTION                    
048441                                                                          
048442     MOVE 'WDM221 '           TO SSA1                                     
048443     MOVE '    GE'            TO GODK-STATUSKODER                         
048444     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
048445     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
048446     PERFORM IMS-STATUSKONTROLL                                           
048447     .                                                                    
048448                                                                          
048449 IMS-GHU-WDM211 SECTION.                                                  
048450     MOVE 'IMS-GHU-WDM211      ' TO DBS-SECTION                           
048451                                                                          
048452     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
048453          DELIMITED BY SIZE INTO SSA1                                     
048454     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
048455          DELIMITED BY SIZE INTO SSA2                                     
048456     MOVE '  GE'              TO GODK-STATUSKODER                         
048457     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
048458     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
048459     PERFORM IMS-STATUSKONTROLL                                           
048460     .                                                                    
048461                                                                          
048462 IMS-REPL-WDM211 SECTION.                                                 
048463     MOVE 'IMS-REPL-WDM211     ' TO DBS-SECTION                           
048464                                                                          
048465     MOVE '  '             TO GODK-STATUSKODER                            
048466     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
048467     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
048468     PERFORM IMS-STATUSKONTROLL                                           
048469     .                                                                    
048470 IMS-GHU-WDM221 SECTION.                                                  
048471     MOVE 'IMS-GHU-WDM221      ' TO DBS-SECTION                           
048472                                                                          
048473     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
048474          DELIMITED BY SIZE INTO SSA1                                     
048475     STRING 'WDM211  (IDARTNR  =' W-WDM211-IDARTNR-X ')'                  
048476          DELIMITED BY SIZE INTO SSA2                                     
048477     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
048478          DELIMITED BY SIZE INTO SSA3                                     
048479     MOVE '  GE' TO GODK-STATUSKODER                                      
048480     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
048481     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
048482     PERFORM IMS-STATUSKONTROLL                                           
048483     .                                                                    
048484                                                                          
048485 IMS-REPL-WDM221 SECTION.                                                 
048486     MOVE 'IMS-REPL-WDM221     ' TO DBS-SECTION                           
048487                                                                          
048488     MOVE '  '             TO GODK-STATUSKODER                            
048489     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
048490     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
048491     PERFORM IMS-STATUSKONTROLL                                           
048500     .                                                                    
054000 IMS-GU-ORDP-WDA501 SECTION.                                              
054010     MOVE 'IMS-GU-ORDP-WDA501  ' TO DBS-SECTION                           
054100                                                                          
054200     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN                          
054300                    '&WDA501KY<=' W-WDA501KY-MAX                          
054400                    '&KDSTARAD =' W-KDSTARAD-X                            
054500                    '&KDTPOTYP =' W-KDTPOTYP-X                            
054600                    '&TITPO    =' W-TITPO-X ')'                           
054700          DELIMITED BY SIZE INTO SSA1                                     
054800     MOVE '  GE' TO GODK-STATUSKODER                                      
054900     CALL CBLTDLI USING GU ORDP-PCB RAD-WDA501 SSA1                       
055000     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
055100     PERFORM IMS-STATUSKONTROLL                                           
055200     .                                                                    
055300     SKIP2                                                                
055400 IMS-ISRT-ORDP-WDA501 SECTION.                                            
055410     MOVE 'IMS-ISRT-ORDP-WDA501' TO DBS-SECTION                           
055500                                                                          
055600     MOVE 'WLORDP01 '  TO SSA1                                            
055700     MOVE '  II' TO GODK-STATUSKODER                                      
055800     CALL CBLTDLI USING ISRT ORDP-PCB RAD-WDA501 SSA1                     
055900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     .                                                                    
057800     EJECT                                                                
057900 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
057910     MOVE 'IMS-ISRT-ZZAC-WDG6  ' TO DBS-SECTION                           
058000                                                                          
058100     MOVE 'WLZZAC01 ' TO SSA1                                             
058200     MOVE '  II' TO GODK-STATUSKODER                                      
058300     CALL CBLTDLI USING ISRT ZZAC-PCB ZZAC-WDGZ01 SSA1                    
058400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
058500     PERFORM IMS-STATUSKONTROLL                                           
058600     .                                                                    
058700     EJECT                                                                
058800 IMS-STATUSKONTROLL SECTION.                                              
058900                                                                          
059000     SET STATUS-IX TO 1                                                   
059100     SEARCH GODK-STATUS                                                   
059200       AT END                                                             
059300       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
059400       DELIMITED BY SIZE INTO  FELTEXT                                    
059500       CALL FELLOG                                                        
059600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
059700     END-SEARCH                                                           
059800     .                                                                    
059900     EJECT                                                                
060000*    -COPY WY2000P1                                                       
