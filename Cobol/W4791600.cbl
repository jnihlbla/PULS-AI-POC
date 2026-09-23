000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4791600.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   91/11/11.                                                
000510 DATE-COMPILED.                                                           
000600*                                                                         
000900*    FUNKTION:                                                            
001000*    LÄSER INFILER FRÅN DE PGM SOM INNEHÅLLER                             
001100*    REGLER FÖR RENSNING.                                                 
001200*    SKRIVER POSTER FÖR:                                                  
001210*      A. UPPDATERING AV WDL1                     (W47918)                
001310*      C. TRANSAKTION TILL NOAC MED RENSADE ORDER (W47920)                
001320*    INFILERNA GÅR DESSUTOM VIDARE TILL RENSNINGSPGMET                    
002000*                                                                         
002100                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT          SECTION.                                           
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- ORDER SOM SKALL RENSAS PÅ Q2,Q3                            
003000     SELECT W47910                     ASSIGN TO W47916D1.                
003100                                                                          
003200*          --- ORDER SOM SKALL RENSAS PÅ Q1,Q2                            
003300     SELECT W47917                     ASSIGN TO W47916D2.                
003400                                                                          
003500*          --- ORDER I STATUS E SOM RENSAS PÅ Q2                          
003600     SELECT W47914                     ASSIGN TO W47916D3.                
003700                                                                          
003800*          --- ORDERBEKR.POSTER TILL WDL1                                 
003900     SELECT W47918                     ASSIGN TO W47916D4.                
003940                                                                          
003950*          --- RENSADE ORDER TILL NOACK                                   
003960     SELECT W47920                     ASSIGN TO W47916D6.                
003961                                                                          
003962*          --- RENSNINGSPOSTER TILL W47913                                
003963     SELECT W47921                     ASSIGN TO W47916D7.                
003970     EJECT                                                                
003980 DATA DIVISION.                                                           
003990                                                                          
004000 FILE                  SECTION.                                           
004100                                                                          
004200 FD  W47910                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  -COPY W479010       -L.                                              
004700     SKIP3                                                                
004800 FD  W47917                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100     SKIP2                                                                
005200*01  -COPY W479017       -L.                                              
005300     SKIP3                                                                
005400 FD  W47914                                                               
005500     RECORDING       F                                                    
005510     BLOCK CONTAINS  0.                                                   
005520     SKIP2                                                                
005530*01  -COPY W479014       -L.                                              
005540     SKIP3                                                                
005550 FD  W47918                                                               
005560     RECORDING       F                                                    
005570     BLOCK CONTAINS  0.                                                   
005580     SKIP2                                                                
005590*01  POST  -COPY W4797401 -PRE UT-   -L.                                  
005591     SKIP3                                                                
005598 FD  W47920                                                               
005599     RECORDING       V                                                    
005600     BLOCK CONTAINS  0.                                                   
005601     SKIP2                                                                
005602*01  POST  -COPY W461S051 -PRE UT3-  -L.                                  
005603     SKIP3                                                                
005604 FD  W47921                                                               
005605     RECORDING       F                                                    
005606     BLOCK CONTAINS  0.                                                   
005607     SKIP2                                                                
005608*01  POST  -COPY W479021 -PRE UT4-   -L.                                  
005609     EJECT                                                                
005610 WORKING-STORAGE       SECTION.                                           
005613*    -- CHECKED BY WY2000                                                 
005614 77  IDPGM                       PIC X(8)    VALUE 'W4791300'.            
005615 77  JA                          PIC X       VALUE 'J'.                   
005616 77  NEJ                         PIC X       VALUE 'N'.                   
005617 77  WS-IDLOPNR                  PIC S9(3)   COMP-3 VALUE ZERO.           
005620 77  IDDC-IX                     PIC  9(2)   VALUE ZERO.                  
005621 77  IDDC-MAX                    PIC  9(2)   VALUE 13.                    
005630                                                                          
005640 77  W47910-EOF-SW               PIC X       VALUE 'N'.                   
005650     88  END-OF-W47910                       VALUE 'J'.                   
005660 77  W47914-EOF-SW               PIC X       VALUE 'N'.                   
005670     88  END-OF-W47914                       VALUE 'J'.                   
005680 77  W47917-EOF-SW               PIC X       VALUE 'N'.                   
005690     88  END-OF-W47917                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  FILLER                      PIC X(16) VALUE 'IDDC-TAB.'.             
005900                                                                          
006000 01  IDDC-TABELL.                                                         
006100     03 IDDC                            OCCURS 13.                        
006200       05 WS-IDDC                PIC  X(2).                               
006300     EJECT                                                                
006400 01  TEST-IDDISTR                PIC S9(5)   COMP-3 VALUE ZERO.           
006700*01  FILLER  -COPY WWDIS130      -RED TEST-IDDISTR.                       
006800     EJECT                                                                
006810 01  ARBETS-FAELT.                                                        
006820     03 IDKUNDRF-RO-X            PIC X(10)   VALUE ZERO.                  
006830     03 IDKUNDRF-RESTORDER REDEFINES IDKUNDRF-RO-X.                       
006840       05  KUNDRF-RO-X           PIC X(5).                                
006850       05  KUNDRF-RO-9           PIC 9(5).                                
006860     SKIP3                                                                
006861 01  SPAR-IDORDNR7               PIC 9(7)    VALUE ZERO.                  
006862     EJECT                                                                
006870 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006880 01  FILLER REDEFINES DAGENS-DATUM.                                       
006890     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007510     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007710     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008300     SKIP2                                                                
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(20)   VALUE SPACE.                 
008700     EJECT                                                                
008701*    --- PARAMETRAR TILL SUBPROGRAM DATKORT                               
008702*                                                                         
008703 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47978'.              
008704     SKIP2                                                                
008705 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008706     SKIP2                                                                
008707*01  -COPY WDATKORT                                                       
008710     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009101 01  FILLER                      PIC X(8)    VALUE 'W460DIS1'.            
009110*    --- PARAMETRAR TILL W460DIS1                                         
009120*                                                                         
009130*01  -COPY W460DIS1                                                       
009140     EJECT                                                                
009200 01  IN1-AREA-START              PIC X(24)   VALUE                        
009300                                 'IN1-AREA-START  '.                      
009400     SKIP2                                                                
009500                                                                          
009600*01  AREA -COPY W479010      -PRE IN1-                                    
009700     EJECT                                                                
009800 01  IN2-AREA-START              PIC X(24)   VALUE                        
009900                                 'IN2-AREA-START  '.                      
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY W479017      -PRE IN2-                                    
010300     EJECT                                                                
010400 01  IN3-AREA-START              PIC X(24)   VALUE                        
010500                                 'IN3-AREA-START  '.                      
010600     SKIP2                                                                
010610                                                                          
010620*01  AREA -COPY W479014      -PRE IN3-                                    
010630     EJECT                                                                
010640 01  UT-AREA-START               PIC X(24)   VALUE                        
010650                                 'UT-AREA-START   '.                      
010660     SKIP2                                                                
010670                                                                          
010680*01  AREA -COPY W4797401     -PRE  UT-                                    
010690     EJECT                                                                
010697 01  UT-AREA3-START              PIC X(24)   VALUE                        
010698                                 'UT-AREA3-START  '.                      
010700                                                                          
010701*01  AREA -COPY W461S051     -PRE  UT3-                                   
010702     EJECT                                                                
010703 01  UT-AREA4-START              PIC X(24)   VALUE                        
010704                                 'UT-AREA4-START  '.                      
010706                                                                          
010707*01  AREA -COPY W479021      -PRE  UT4-                                   
010708     EJECT                                                                
010709*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010710*                                                                         
010712 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010713                                                                          
010714 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-WDQ101KY-MIN.                                                  
012200         05  Q1-IDORDER-MIN      PIC S9(7)    COMP-3.                     
012300         05  Q1-IDARTNR-MIN      PIC S9(9)    COMP-3.                     
012400         05  Q1-IDLOPNR-MIN      PIC S9(3)    COMP-3.                     
012500         05  Q1-IDSEKVNR-MIN     PIC S9(3)    COMP-3.                     
012600         05  Q1-IDDC-MIN         PIC X(2).                                
012700         05  Q1-KDORDBEK-MIN     PIC 9(2).                                
012800     03  W-WDQ101KY-MAX.                                                  
012900         05  Q1-IDORDER-MAX      PIC S9(7)    COMP-3.                     
013000         05  Q1-IDARTNR-MAX      PIC S9(9)    COMP-3.                     
013100         05  Q1-IDLOPNR-MAX      PIC S9(3)    COMP-3.                     
013200         05  Q1-IDSEKVNR-MAX     PIC S9(3)    COMP-3.                     
013300         05  Q1-IDDC-MAX         PIC X(2).                                
013400         05  Q1-KDORDBEK-MAX     PIC 9(2).                                
013500     03  W-IDORDER-X.                                                     
013600         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FINNS                       VALUE '  '.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     88  BASEN-SLUT                          VALUE 'GB'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(80).                               
015500     EJECT                                                                
015600*    --- IMS FUNKTIONSKODER                                               
015700*01  -COPY W0003                                                          
015800     EJECT                                                                
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016100     SKIP3                                                                
016200 01  DLI-IO-AREA.                                                         
016300     03  IO-AREA                 PIC X(4000) VALUE SPACE.                 
016400     SKIP3                                                                
016500     03  WLORDP01 REDEFINES IO-AREA.                                      
016600*        05  -COPY WDA501                                                 
016700     SKIP3                                                                
016800     03  WLOGAP01 REDEFINES IO-AREA.                                      
016900*        05  -COPY WDE401                                                 
017000     SKIP3                                                                
018200     SKIP3                                                                
018700     03  WLORQM01 REDEFINES IO-AREA.                                      
018800*        05  -COPY WDQ101                                                 
018900     SKIP3                                                                
019000     03  WLORQI01 REDEFINES IO-AREA.                                      
019100*        05  -COPY WDQ201                                                 
019200     SKIP3                                                                
019300     03  WLORQI12 REDEFINES IO-AREA.                                      
019400*        05  -COPY WDQ212                                                 
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-2'.        
020300     SKIP3                                                                
020310 01  DLI-IO-AREA-2.                                                       
020320     03  IO-AREA-2               PIC X(3000) VALUE SPACE.                 
020330     EJECT                                                                
020340     03  WLORDP01 REDEFINES IO-AREA-2.                                    
020350*        05  -COPY WDA501                                                 
020351     EJECT                                                                
020360 LINKAGE               SECTION.                                           
021200*01  -COPY W0009  -PRE MSG-                                               
021400     EJECT                                                                
021410*01  -COPY W0008  -PRE ORQI-                                              
021420     05  FILLER                  PIC X.                                   
021430     EJECT                                                                
021500*01  -COPY W0008  -PRE ORQM-                                              
021600     05  FILLER                  PIC X.                                   
021700     EJECT                                                                
021800 PROCEDURE DIVISION  USING  MSG-PCB                                       
022200                           ORQI-PCB                                       
022210                           ORQM-PCB.                                      
022220 MAIN SECTION.                                                            
022300     ENTRY 'DLITCBL' USING  MSG-PCB                                       
022400                           ORQI-PCB                                       
022430                           ORQM-PCB.                                      
022440                                                                          
022450     PERFORM A-INIT                                                       
022460                                                                          
022470     PERFORM S01-LAES-W47910                                              
022480     PERFORM UNTIL END-OF-W47910                                          
022490       PERFORM B-SKAPA-NOAC-FIL                                           
022500       PERFORM S01-LAES-W47910                                            
022600     END-PERFORM                                                          
022700                                                                          
022800     PERFORM S03-LAES-W47914                                              
022900     PERFORM UNTIL END-OF-W47914                                          
023000       PERFORM D-SKAPA-RENSNINGSPOST                                      
023100       PERFORM S03-LAES-W47914                                            
023200     END-PERFORM                                                          
023300                                                                          
023400     PERFORM S02-LAES-W47917                                              
023500     PERFORM UNTIL END-OF-W47917                                          
023600       PERFORM C-SKAPA-FICHE                                              
023700       PERFORM S02-LAES-W47917                                            
023800     END-PERFORM                                                          
023900                                                                          
024000     PERFORM Z-FINIT                                                      
024100                                                                          
024200     MOVE ZERO                    TO RETURN-CODE                          
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 A-INIT                SECTION.                                           
024700                                                                          
024800     OPEN INPUT  W47910                                                   
024900                 W47914                                                   
025000                 W47917                                                   
025100     OPEN OUTPUT W47918                                                   
025210                 W47920                                                   
025211                 W47921                                                   
025220     SKIP2                                                                
025221     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025222     MOVE D-AAR                   TO DAGENS-DATUM-AAR                     
025223     MOVE D-MAANAD                TO DAGENS-DATUM-MAANAD                  
025224     MOVE D-DAG                   TO DAGENS-DATUM-DAG                     
025240     MOVE IDPGM                   TO POSTSUM-PROGNAMN                     
025250     MOVE +1                      TO WS-IDLOPNR                           
025260     .                                                                    
025270     EJECT                                                                
025829 B-SKAPA-NOAC-FIL     SECTION.                                            
025830                                                                          
025834     IF IN1-IDORDER                 = W-IDORDER                           
025835        CONTINUE                                                          
025836     ELSE                                                                 
025837        PERFORM S07-NOLLA-IDDC-TAB                                        
025838        MOVE +1                      TO IDDC-IX                           
025839        MOVE IN1-IDORDER             TO W-IDORDER                         
025840        PERFORM IMS-GET-ORQI01                                            
025841        IF SEGMENT-FINNS                                                  
025842           MOVE OHUV-IDDISTR            TO UT3-RENS-SOR0-IDDISTR          
025843                                           UT3-RENS-IDDISTR               
025844                                           TEST-IDDISTR                   
025845                                           DIS1-IDDISTR                   
025846           MOVE OHUV-IDKUNDNR           TO UT3-RENS-SOR0-IDKUNDNR         
025847                                           UT3-RENS-IDKUNDNR              
025850           MOVE OHUV-TIREGDAT           TO UT3-RENS-TIORDREG              
025851           MOVE OHUV-BEKUNDRF           TO UT3-RENS-BEVOLREF              
025852           MOVE OHUV-IDORDNR7           TO SPAR-IDORDNR7                  
025853           PERFORM IMS-GHNP-ORQI12                                        
025854           PERFORM UNTIL SEGMENT-SAKNAS                                   
025855              IF IDDC-IX < IDDC-MAX                                       
025856                 MOVE ARB-IDDC          TO WS-IDDC (IDDC-IX)              
025857              ELSE                                                        
025858             MOVE 'FLER ÄN 12 C-LAGER I IDDC-TAB' TO FELTEXT-STR          
025859                 DISPLAY FELTEXT                                          
025860                 CALL ABEND USING RKOD-ABEND-UTAN-DUMP                    
025861              END-IF                                                      
025862              PERFORM IMS-GHNP-ORQI12                                     
025863              ADD +1                     TO IDDC-IX                       
025864           END-PERFORM                                                    
025869           CALL W460DIS1 USING DIS1-W460DIS1                              
025870                                                                          
025873           IF (DIS1-KDSVAR = JA OR DIS130-NOAC)                           
025874              MOVE ZERO                  TO UT3-RENS-SOR0-IDRONR          
025875                                            UT3-RENS-SOR0-TIRODAT         
025876              MOVE '051'                 TO UT3-RENS-SOR0-IDPTYP          
025877                                            UT3-RENS-IDPTYP               
025878              MOVE SPAR-IDORDNR7         TO UT3-RENS-IDORDNR              
025879              MOVE +1                    TO IDDC-IX                       
025880              PERFORM UNTIL (IDDC-IX > IDDC-MAX) OR                       
025881                            (WS-IDDC (IDDC-IX) = +0)                      
025882                IF WS-IDDC(IDDC-IX) = IN1-IDDC                            
025883                  MOVE WS-IDDC (IDDC-IX) TO UT3-RENS-IDDC                 
025884                  MOVE WS-IDLOPNR        TO UT3-RENS-SOR0-IDLOPNR         
025885                  PERFORM S06-SKRIV-W47920                                
025886                  ADD +1                 TO WS-IDLOPNR                    
025888                END-IF                                                    
025889                                                                          
025890                ADD +1                   TO IDDC-IX                       
025891              END-PERFORM                                                 
025892           END-IF                                                         
025893        END-IF                                                            
025894     END-IF                                                               
025895     PERFORM BA-SKAPA-RENSNINGSPOST                                       
025896     .                                                                    
025897     EJECT                                                                
025898 BA-SKAPA-RENSNINGSPOST SECTION.                                          
025899                                                                          
025900     MOVE IN1-IDPTYP             TO UT4-IDPTYP                            
025901     MOVE IN1-IDORDER            TO UT4-IDORDER                           
025902     MOVE IN1-IDDC               TO UT4-IDDC                              
025903     MOVE IN1-IDPRODNR           TO UT4-IDPRODNR                          
025904     MOVE IN1-IDPLKLST           TO UT4-IDPLKLST                          
025905     MOVE IN1-IDDISTR            TO UT4-IDDISTR                           
025906     MOVE IN1-IDKUNDNR           TO UT4-IDKUNDNR                          
025907     MOVE IN1-IDKUNDRF           TO UT4-IDKUNDRF                          
025908     MOVE IN1-IDLEVNR            TO UT4-IDLEVNR                           
025909     PERFORM S08-SKRIV-W47921                                             
025910     .                                                                    
025911     EJECT                                                                
025912 C-SKAPA-FICHE         SECTION.                                           
025920                                                                          
025921     PERFORM CA-SKAPA-POSTER                                              
025922     PERFORM CB-SKAPA-RENSNINGSPOST                                       
025923     .                                                                    
025924     EJECT                                                                
025925 CA-SKAPA-POSTER       SECTION.                                           
025926                                                                          
025927     MOVE LOW-VALUE               TO W-WDQ101KY-MIN                       
025928     MOVE HIGH-VALUE              TO W-WDQ101KY-MAX                       
025929     MOVE IN2-IDORDER             TO Q1-IDORDER-MIN                       
025930                                     Q1-IDORDER-MAX                       
025931     PERFORM IMS-GU-ORQM01                                                
025932     IF SEGMENT-FINNS                                                     
025933        PERFORM S07-NOLLA-IDDC-TAB                                        
025934        MOVE OBKR-IDDISTR         TO UT-IDDISTR                           
025935        MOVE OBKR-IDKUNDNR        TO UT-IDKUNDNR                          
025936        MOVE OBKR-IDKUNDRF        TO UT-IDKUNDRF                          
025937        MOVE DAGENS-DATUM         TO UT-TIHIST-OBKR                       
025938        MOVE 'WDL112'             TO UT-IDSEGM                            
025939        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
025940          PERFORM CAB-SKAPA-W47918                                        
025942          PERFORM IMS-GN-ORQM01                                           
025943        END-PERFORM                                                       
025944     END-IF                                                               
025945     .                                                                    
025946     EJECT                                                                
025947 CAB-SKAPA-W47918      SECTION.                                           
025948                                                                          
025949     MOVE +1                   TO IDDC-IX                                 
025950     PERFORM UNTIL (IDDC-IX > IDDC-MAX)                        OR         
025951                   (WS-IDDC (IDDC-IX) = OBKR-IDDC) OR                     
025952                   (WS-IDDC (IDDC-IX) = +0)                               
025953        ADD +1                 TO IDDC-IX                                 
025954     END-PERFORM                                                          
025955                                                                          
025956     IF IDDC-IX              > IDDC-MAX                                   
025957        MOVE 'FLER ÄN 12 C-LAGER I IDDC-TAB' TO FELTEXT-STR               
025958        DISPLAY FELTEXT                                                   
025959        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
025960     ELSE                                                                 
025961        IF WS-IDDC (IDDC-IX)   = +0                                       
025962           MOVE OBKR-IDDC         TO WS-IDDC (IDDC-IX)                    
025963                                     UT-IDDC                              
025964           PERFORM S04-SKRIV-W47918                                       
025965        END-IF                                                            
025966     END-IF                                                               
025967     .                                                                    
025968     EJECT                                                                
026000 CB-SKAPA-RENSNINGSPOST SECTION.                                          
026001                                                                          
026002     MOVE IN2-IDPTYP             TO UT4-IDPTYP                            
026003     MOVE IN2-IDORDER            TO UT4-IDORDER                           
026005     MOVE IN2-IDDISTR            TO UT4-IDDISTR                           
026006     MOVE IN2-IDKUNDNR           TO UT4-IDKUNDNR                          
026007     MOVE '0000000000'           TO UT4-IDKUNDRF                          
026008     MOVE ZERO                   TO UT4-IDDC                              
026009                                    UT4-IDPRODNR                          
026010                                    UT4-IDPLKLST                          
026011     MOVE SPACE                  TO UT4-IDLEVNR                           
026012     PERFORM S08-SKRIV-W47921                                             
026013     .                                                                    
026014     EJECT                                                                
026015 D-SKAPA-RENSNINGSPOST  SECTION.                                          
026016                                                                          
026017     MOVE IN3-IDPTYP             TO UT4-IDPTYP                            
026018     MOVE IN3-IDORDER            TO UT4-IDORDER                           
026019     MOVE IN3-IDDISTR            TO UT4-IDDISTR                           
026020     MOVE IN3-IDKUNDNR           TO UT4-IDKUNDNR                          
026021     MOVE '0000000000'           TO UT4-IDKUNDRF                          
026022     MOVE ZERO                   TO UT4-IDDC                              
026023                                    UT4-IDPRODNR                          
026024                                    UT4-IDPLKLST                          
026025     MOVE SPACE                  TO UT4-IDLEVNR                           
026026     PERFORM S08-SKRIV-W47921                                             
026027     .                                                                    
026028     EJECT                                                                
026029 Z-FINIT               SECTION.                                           
026030                                                                          
026031     CLOSE W47910                                                         
026032           W47914                                                         
026033           W47917                                                         
026034           W47918                                                         
026036           W47920                                                         
026037           W47921                                                         
026038     SKIP2                                                                
026040     MOVE 'S'                     TO POSTSUM-OPKOD                        
026100     CALL POSTSUM USING POSTSUM-PARM                                      
026200     .                                                                    
026300     EJECT                                                                
026400 S01-LAES-W47910       SECTION.                                           
026500                                                                          
026600     READ W47910                INTO IN1-AREA                             
026700     AT END                                                               
026800        MOVE HIGH-VALUE           TO IN1-AREA                             
026900        SET END-OF-W47910         TO TRUE                                 
027000                                                                          
027100     NOT AT END                                                           
027200        MOVE 'W47910'             TO POSTSUM-FDNAMN                       
027300        MOVE 'W47913D1'           TO POSTSUM-DDNAMN2                      
027400        MOVE IN1-IDPTYP           TO POSTSUM-TRANSTYP                     
027500        CALL POSTSUM USING POSTSUM-PARM                                   
027600     END-READ                                                             
027700     .                                                                    
027800     EJECT                                                                
027900 S02-LAES-W47917       SECTION.                                           
028000                                                                          
028100     READ W47917                INTO IN2-AREA                             
028200     AT END                                                               
028300        MOVE HIGH-VALUE           TO IN2-AREA                             
028400        SET END-OF-W47917         TO TRUE                                 
028500                                                                          
028600     NOT AT END                                                           
028700        MOVE 'W47917'             TO POSTSUM-FDNAMN                       
028800        MOVE 'W47913D2'           TO POSTSUM-DDNAMN2                      
028900        MOVE IN2-IDPTYP           TO POSTSUM-TRANSTYP                     
029000        CALL POSTSUM USING POSTSUM-PARM                                   
029100     END-READ                                                             
029200     .                                                                    
029300     EJECT                                                                
029400 S03-LAES-W47914       SECTION.                                           
029500                                                                          
029510     READ W47914                INTO IN3-AREA                             
029520     AT END                                                               
029530        MOVE HIGH-VALUE           TO IN3-AREA                             
029540        SET END-OF-W47914         TO TRUE                                 
029550                                                                          
029560     NOT AT END                                                           
029570        MOVE 'W47914'             TO POSTSUM-FDNAMN                       
029580        MOVE 'W47913D3'           TO POSTSUM-DDNAMN2                      
029590        MOVE IN3-IDPTYP           TO POSTSUM-TRANSTYP                     
029591        CALL POSTSUM USING POSTSUM-PARM                                   
029592     END-READ                                                             
029593     .                                                                    
029594     EJECT                                                                
029595 S04-SKRIV-W47918      SECTION.                                           
029596                                                                          
029597     WRITE UT-POST           FROM UT-AREA                                 
029598     MOVE 'W47918'             TO POSTSUM-FDNAMN                          
029599     MOVE 'W47913D4'           TO POSTSUM-DDNAMN2                         
029600     MOVE 'WDL1'               TO POSTSUM-TRANSTYP                        
029601     CALL POSTSUM USING POSTSUM-PARM                                      
029602     .                                                                    
029603     EJECT                                                                
029616 S06-SKRIV-W47920      SECTION.                                           
029617                                                                          
029618     WRITE UT3-POST          FROM UT3-AREA                                
029619     MOVE 'W47920'             TO POSTSUM-FDNAMN                          
029620     MOVE 'W47913D6'           TO POSTSUM-DDNAMN2                         
029630     MOVE 'NOAC'               TO POSTSUM-TRANSTYP                        
029640     CALL POSTSUM USING POSTSUM-PARM                                      
029650     .                                                                    
029660     EJECT                                                                
029670 S07-NOLLA-IDDC-TAB    SECTION.                                           
029680                                                                          
029690     MOVE +1                   TO IDDC-IX                                 
029700     PERFORM UNTIL IDDC-IX > IDDC-MAX                                     
029800        MOVE +0                TO WS-IDDC (IDDC-IX)                       
029810        ADD +1                 TO IDDC-IX                                 
029811     END-PERFORM                                                          
029812     .                                                                    
029813     EJECT                                                                
029814* --- IMS SEKTIONER ---                                                   
029815 S08-SKRIV-W47921      SECTION.                                           
029816                                                                          
029817     WRITE UT4-POST          FROM UT4-AREA                                
029818     MOVE 'W47921'             TO POSTSUM-FDNAMN                          
029819     MOVE 'W47913D7'           TO POSTSUM-DDNAMN2                         
029820     MOVE 'RENS'               TO POSTSUM-TRANSTYP                        
029821     CALL POSTSUM USING POSTSUM-PARM                                      
029822     .                                                                    
029823     EJECT                                                                
029824* --- IMS SEKTIONER ---                                                   
029825                                                                          
036200     EJECT                                                                
036300 IMS-GU-ORQM01         SECTION.                                           
036400                                                                          
036500     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN                          
036600                    '&WDQ101KY<=' W-WDQ101KY-MAX ')'                      
036700          DELIMITED BY SIZE     INTO SSA1                                 
036800     MOVE '  GE'                  TO GODK-STATUSKODER                     
036900     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
037000     MOVE ORQM-STATUS-CODE        TO STATUS-WS                            
037100     PERFORM IMS-STATUSKONTROLL                                           
037200     .                                                                    
037210     SKIP2                                                                
037300 IMS-GN-ORQM01         SECTION.                                           
037400                                                                          
037500     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN                          
037600                    '&WDQ101KY<=' W-WDQ101KY-MAX ')'                      
037700          DELIMITED BY SIZE     INTO SSA1                                 
037800     MOVE '  GEGB'                TO GODK-STATUSKODER                     
037900     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-AREA SSA1                      
038000     MOVE ORQM-STATUS-CODE        TO STATUS-WS                            
038010     PERFORM IMS-STATUSKONTROLL                                           
038020     .                                                                    
038100     EJECT                                                                
038200 IMS-GET-ORQI01        SECTION.                                           
038300                                                                          
038400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
038500          DELIMITED BY SIZE     INTO SSA1                                 
038600     MOVE '  GE'                  TO GODK-STATUSKODER                     
038700     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA SSA1                      
038800     MOVE ORQI-STATUS-CODE        TO STATUS-WS                            
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100                                                                          
039200 IMS-GHNP-ORQI12       SECTION.                                           
039300                                                                          
039400     MOVE 'WLORQI12 '             TO SSA1                                 
039500     MOVE '  GE'                  TO GODK-STATUSKODER                     
039600     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA SSA1                     
039700     MOVE ORQI-STATUS-CODE        TO STATUS-WS                            
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040800     EJECT                                                                
042700 IMS-STATUSKONTROLL    SECTION.                                           
042800                                                                          
042900     SET STATUS-IX                TO 1                                    
043000     SEARCH GODK-STATUS                                                   
043100       AT END                                                             
043200         MOVE ' OGILTIG STATUSKOD: ' TO FELTEXT-STR                       
043300         DISPLAY FELTEXT STATUS-WS                                        
043400         CALL FELLOG                                                      
043500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
043600     END-SEARCH                                                           
043700     .                                                                    
