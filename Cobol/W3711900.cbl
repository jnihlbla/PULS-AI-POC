000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3711900.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   DEC-1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM LÄSER AKTUELLA RETURER PÅ WDM6 OCH BYGGER/SKRIVER;           
001100*        . TRANSIT-POSTER             TILL RAPPORTER/HISTORIK             
001200*        . POÄNG-POSTER               TILL RAPPORTER/HISTORIK             
001300*        . UPPDATERINGS-POSTER        TILL WDM6                           
001400*                                                                         
002200*       -PGM LÄSER WDGX3158 (WDR1)                                        
002700*                                                                         
003700*    ABENDKODER:                                                          
003800*        U0016 -  . . . .                                                 
003900*        U1000 -  . . . .                                                 
004000*                                                                         
004100                                                                          
004300 ENVIRONMENT DIVISION.                                                    
004400                                                                          
004500 INPUT-OUTPUT SECTION.                                                    
004600                                                                          
004700 FILE-CONTROL.                                                            
004900*          --- AKTUELLA RETURER                                           
005000     SELECT W37113                     ASSIGN TO W37119D1.                
005100                                                                          
005500*          --- TRANSIT-POSTER                                             
005600     SELECT W37106                     ASSIGN TO W37119D2.                
005700                                                                          
005800*          --- POÄNG-POSTER                                               
005900     SELECT W37107                     ASSIGN TO W37119D3.                
005910                                                                          
006000*          --- UPPDATERINGS-POSTER                                        
006100     SELECT W37108                     ASSIGN TO W37119D4.                
006600     EJECT                                                                
006610                                                                          
006700 DATA DIVISION.                                                           
006800                                                                          
006900 FILE SECTION.                                                            
007100 FD  W37113                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500*01  RET-REC     -COPY W37109      -L.                                    
007800     EJECT                                                                
007900                                                                          
008810 FD  W37106                                                               
008820     RECORDING       F                                                    
008830     BLOCK CONTAINS  0.                                                   
008840                                                                          
008850*01  TRANSIT-REC -COPY W37109      -L.                                    
008860     EJECT                                                                
008870                                                                          
008880 FD  W37107                                                               
008890     RECORDING       F                                                    
008891     BLOCK CONTAINS  0.                                                   
008892                                                                          
008893*01  POINT-REC   -COPY W37109      -L.                                    
011400     EJECT                                                                
011410                                                                          
011430 FD  W37108                                                               
011440     RECORDING       F                                                    
011450     BLOCK CONTAINS  0.                                                   
011460                                                                          
011470*01  UPD-REC     -COPY W37109      -L.                                    
011480     EJECT                                                                
011490                                                                          
011500 WORKING-STORAGE SECTION.                                                 
011700*    -- CHECKED BY WY2000                                                 
011800 77  IDPGM                        PIC X(8)    VALUE 'W3711900'.           
011900 77  JA                           PIC X       VALUE 'J'.                  
012000 77  NEJ                          PIC X       VALUE 'N'.                  
012100 77  INDX                         PIC S9(2)   VALUE +0 COMP SYNC.         
012300 77  W37113-EOF-SW                PIC X       VALUE 'N'.                  
012400     88  END-OF-W37113                        VALUE 'J'.                  
012401 77  SW-FIRST-TIME                PIC X       VALUE 'J'.                  
012402     88  FIRST-TIME                           VALUE 'J'.                  
012403 77  SW-RETURN-IN-TRANSIT         PIC X       VALUE 'N'.                  
012404     88  RETURN-IN-TRANSIT                    VALUE 'J'.                  
012405 77  SW-UPPDATERING               PIC X       VALUE 'N'.                  
012406     88  UPPDATERING                          VALUE 'J'.                  
012410 77  WS-SPAR-IDBYTRAP-UPD         PIC S9(7)   VALUE +0 COMP-3.            
012500 77  WS-SPAR-IDDISTR-UPD          PIC S9(5)   VALUE +0 COMP-3.            
012600 77  WS-SPAR-KDEXCHA-UPD          PIC S9(3)   VALUE +0 COMP-3.            
012610                                                                          
012620 77  WS-SPAR-IDDISTR-TRANSIT      PIC S9(5)   VALUE +0 COMP-3.            
012630 77  WS-SPAR-KDEXCHA-TRANSIT      PIC S9(3)   VALUE +0 COMP-3.            
012640                                                                          
012700 77  WS-SUPOINT                   PIC S9(7)   VALUE +0 COMP-3.            
013110                                                                          
013200 01  FELTEXT                      PIC X(80).                              
013201     EJECT                                                                
013210                                                                          
019000 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR         PIC 9(2).                               
019300     03  DAGENS-DATUM-MAANAD      PIC 9(2).                               
019400     03  DAGENS-DATUM-DAG         PIC 9(2).                               
019500                                                                          
019600 01  WS-DAREGDAT.                                                         
019700     03  WS-DAREGDAT-SEKEL       PIC 9(2).                                
019800     03  WS-DAREGDAT-AAMMDD      PIC 9(6).                                
019900                                                                          
020150 01  WS-TIAAVV-NUM               PIC 9(4).                                
020160 01  WS-JFR-EXTENDED             PIC S9(5)   COMP-3.                      
020161 01  WS-JFR-EXTENDED-Y2K         PIC S9(7)   COMP-3.                      
020170 01  WS-JFR-DAGENS               PIC S9(5)   COMP-3.                      
020180 01  WS-JFR-DAGENS-Y2K           PIC S9(7)   COMP-3.                      
020200     EJECT                                                                
020210                                                                          
020300 01  DYNAMISKA-SUBPROGRAM.                                                
020400*                                                                         
020500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
021300     EJECT                                                                
021310                                                                          
021400*    --- PARAMETRAR TILL ABEND                                            
021500                                                                          
021600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021900     EJECT                                                                
021910                                                                          
021920 01  W009VADD-DATUM              PIC S9(5)   VALUE ZERO COMP-3.           
021930 01  W009VADD-ANTAL              PIC S9(3)   VALUE ZERO COMP-3.           
021940     EJECT                                                                
021950                                                                          
022110*    --- PARAMETRAR TILL DATKORT                                          
022120*                                                                         
022200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37119'.              
022300                                                                          
022400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022500                                                                          
022600*01  -COPY WDATKORT                                                       
022700     EJECT                                                                
022710                                                                          
022800*    --- PARAMETRAR TILL POSTSUM                                          
022900*                                                                         
023000*01  -COPY W0005   -PRE  POSTSUM-                                         
023100     EJECT                                                                
023300                                                                          
023400*-----------------------------------------PARAMETRAR TILL                 
023500*                                         SUBPROGRAM WDATKONV             
023600 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
023700*01  -COPY WDATAREA.                                                      
023800     EJECT                                                                
023900                                                                          
024500 01  IN-AREA-START               PIC X(24)   VALUE                        
024600                                 'IN-AREA-START  '.                       
024700*01  AREA -COPY W37109     -PRE RET-                                      
025100     EJECT                                                                
025110                                                                          
025200 01  UPD-AREA-START              PIC X(24)   VALUE                        
025300                                 'UPPDAT-AREA-START  '.                   
025500                                                                          
025510*01  AREA -COPY W37109     -PRE UPD-                                      
026200     EJECT                                                                
026201                                                                          
026202 01  TRANSIT-AREA-START          PIC X(24)   VALUE                        
026203                                 'TRANSIT-AREA-START  '.                  
026204                                                                          
026205*01  AREA -COPY W37109     -PRE TRANSIT-                                  
026206     EJECT                                                                
026207                                                                          
026208 01  POINT-AREA-START            PIC X(24)   VALUE                        
026209                                 'POINT-AREA-START  '.                    
026210                                                                          
026211*01  AREA -COPY W37109     -PRE POINT-                                    
026212     EJECT                                                                
026220                                                                          
026300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026400*                                                                         
026710 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026720                                                                          
026800 01  NYCKLAR-TILL-DLI.                                                    
026810     03  W-3157-IDHTYP-X.                                                 
026820         05  W-3157-IDHTYP       PIC  X(4)  VALUE '3157'.                 
026830         05  W-3157-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
026840                                                                          
026850     03  W-3158-WDGXKEY-X.                                                
026860         05  W-3158-IDDISTR      PIC  S9(5) VALUE ZERO COMP-3.            
026861         05  W-3158-KDEXCHA      PIC  S9(3) VALUE ZERO COMP-3.            
026870     EJECT                                                                
030500                                                                          
030600*    --- STATUS-KOD FRÅN IMS                                              
030700 01  STATUS-WS                   PIC XX.                                  
030800     88  SEGMENT-FINNS                       VALUE '  '.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000                                                                          
031100 01  GODK-STATUSKODER.                                                    
031200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031300                                                                          
031400 01  SSA1                        PIC X(64).                               
031500 01  SSA2                        PIC X(64).                               
031800     EJECT                                                                
031810                                                                          
031900*    --- IMS FUNKTIONSKODER                                               
032000*01  -COPY W0003                                                          
032100     EJECT                                                                
032110                                                                          
032200*    ---  DLI INPUT-OUTPUT AREA                                           
035010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
035020 01  DLI-IO-WDGX3158.                                                     
035030*    03  -COPY WDGX3158                                                   
035040     EJECT                                                                
035050                                                                          
035500 LINKAGE SECTION.                                                         
035700*01  -COPY W0008  -PRE 3158-                                              
035800     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037110                                                                          
037200 PROCEDURE DIVISION  USING 3158-PCB.                                      
037400 MAIN SECTION.                                                            
037500     ENTRY 'DLITCBL' USING 3158-PCB.                                      
037700                                                                          
037800     PERFORM A-INIT                                                       
037900                                                                          
038200     PERFORM S01-LAES-W37113                                              
038201                                                                          
038210     IF NOT END-OF-W37113                                                 
038211       MOVE RET-IDBYTRAP     TO WS-SPAR-IDBYTRAP-UPD                      
038212       MOVE RET-IDDISTR      TO WS-SPAR-IDDISTR-UPD                       
038213                                WS-SPAR-IDDISTR-TRANSIT                   
038220       MOVE RET-KDEXCHA      TO WS-SPAR-KDEXCHA-UPD                       
038221                                WS-SPAR-KDEXCHA-TRANSIT                   
038224     END-IF                                                               
038230                                                                          
038300     PERFORM UNTIL END-OF-W37113                                          
038600       PERFORM B-BEARBETA                                                 
038700       PERFORM S01-LAES-W37113                                            
038800     END-PERFORM                                                          
038900                                                                          
039110     IF WS-SUPOINT > ZERO                                                 
039120       PERFORM S10-RETURNS-IN-TRANSIT                                     
039130     END-IF                                                               
039140                                                                          
039160     PERFORM S12-UPDATE                                                   
039180                                                                          
039190     PERFORM Z-FINIT                                                      
039191                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039700                                                                          
039710 A-INIT SECTION.                                                          
039800     OPEN INPUT  W37113                                                   
039900                                                                          
040000     OPEN OUTPUT W37106                                                   
040100                 W37107                                                   
040200                 W37108                                                   
040500                                                                          
040600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
040700     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
040800     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
040900     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
041000     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
042100     .                                                                    
042200     EJECT                                                                
042400                                                                          
042410 B-BEARBETA SECTION.                                                      
042503     PERFORM S10-RETURNS-IN-TRANSIT                                       
042510     PERFORM S11-POINT-RETURNS                                            
042600     PERFORM S12-UPDATE                                                   
445000     .                                                                    
445100     EJECT                                                                
445110                                                                          
445377 Z-FINIT SECTION.                                                         
445380     CLOSE W37113                                                         
445400           W37106                                                         
445500           W37107                                                         
445600           W37108                                                         
445900                                                                          
446000     MOVE 'S' TO POSTSUM-OPKOD                                            
446100     CALL POSTSUM USING POSTSUM-PARM                                      
446200     .                                                                    
446300     EJECT                                                                
446310                                                                          
446400 S01-LAES-W37113  SECTION.                                                
446600     READ W37113 INTO RET-AREA                                            
446700     AT END                                                               
446800        MOVE HIGH-VALUE         TO RET-AREA                               
446810        MOVE 99999              TO RET-IDDISTR                            
446820        MOVE 999                TO RET-KDEXCHA                            
446830        MOVE 9999999            TO RET-IDBYTRAP                           
446900        SET END-OF-W37113       TO TRUE                                   
447000                                                                          
447100     NOT AT END                                                           
447200        MOVE 'W37113'           TO POSTSUM-FDNAMN                         
447300        MOVE 'W37119D1'         TO POSTSUM-DDNAMN2                        
447400        MOVE SPACE              TO POSTSUM-TRANSTYP                       
447500        CALL POSTSUM USING POSTSUM-PARM                                   
447600     END-READ                                                             
447700     .                                                                    
447800                                                                          
450010 S02-SKRIV-W37106 SECTION.                                                
450030     MOVE SPACE                 TO   TRANSIT-REC                          
450040     WRITE TRANSIT-REC          FROM TRANSIT-AREA                         
450050                                                                          
450060     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
450070     MOVE 'W37106'              TO   POSTSUM-FDNAMN                       
450080     MOVE 'W37119D2'            TO   POSTSUM-DDNAMN2                      
450090     CALL POSTSUM USING POSTSUM-PARM                                      
450091     .                                                                    
450093                                                                          
450094 S03-SKRIV-W37107 SECTION.                                                
450096     MOVE SPACE                 TO   POINT-REC                            
450097     WRITE POINT-REC            FROM POINT-AREA                           
450098                                                                          
450099     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
450100     MOVE 'W37107'              TO   POSTSUM-FDNAMN                       
450101     MOVE 'W37119D3'            TO   POSTSUM-DDNAMN2                      
450102     CALL POSTSUM USING POSTSUM-PARM                                      
465800     .                                                                    
465801                                                                          
465810 S04-SKRIV-W37108 SECTION.                                                
465820     MOVE SPACE                 TO   UPD-REC                              
465830     WRITE UPD-REC              FROM UPD-AREA                             
465840                                                                          
465850     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
465860     MOVE 'W37108'              TO   POSTSUM-FDNAMN                       
465870     MOVE 'W37119D4'            TO   POSTSUM-DDNAMN2                      
465880     CALL POSTSUM USING POSTSUM-PARM                                      
465890     .                                                                    
465900     EJECT                                                                
466000                                                                          
466010 S10-RETURNS-IN-TRANSIT SECTION.                                          
466020     IF WS-SPAR-IDDISTR-TRANSIT NOT = RET-IDDISTR OR                      
466030        WS-SPAR-KDEXCHA-TRANSIT NOT = RET-KDEXCHA                         
466040       IF WS-SUPOINT > ZERO                                               
466041         MOVE 'SUT'                TO TRANSIT-IDPTYP                      
466050         MOVE WS-SPAR-IDDISTR-TRANSIT                                     
466051                                   TO TRANSIT-IDDISTR                     
466052         MOVE WS-SPAR-KDEXCHA-TRANSIT                                     
466053                                   TO TRANSIT-KDEXCHA                     
466054         MOVE NEJ                  TO TRANSIT-FLBYTKND                    
466055                                      TRANSIT-FLINKLBS                    
466070         MOVE SPACE                TO TRANSIT-TENOTE                      
466080                                      TRANSIT-KDBYTREF                    
466090         MOVE ZERO                 TO TRANSIT-IDORDER                     
466091                                      TRANSIT-IDBYTRAP                    
466092                                      TRANSIT-IDKUNDNR                    
466093                                      TRANSIT-IDARTNR                     
466094                                      TRANSIT-IDFKNGRP                    
466095                                      TRANSIT-IDBYTRAD                    
466096                                      TRANSIT-DADATUM                     
466097                                      TRANSIT-KDBYTSTA-RAPP               
466098                                      TRANSIT-KDBYTSTA-OBJ                
466099                                      TRANSIT-KVPOINT                     
466100                                      TRANSIT-KVANTAL                     
466101                                      TRANSIT-KVRETUR-GODK                
466102                                      TRANSIT-KVVECKOR                    
466103         MOVE WS-SUPOINT           TO TRANSIT-SUPOINT                     
466104                                                                          
466105         MOVE WS-SPAR-IDDISTR-TRANSIT                                     
466106                                   TO W-3158-IDDISTR                      
466107         MOVE WS-SPAR-KDEXCHA-TRANSIT                                     
466108                                   TO W-3158-KDEXCHA                      
466109         PERFORM IMS-GU-WDGX3158                                          
466110                                                                          
466111         IF SEGMENT-FINNS                                                 
466112           IF 3158-IDDISTR-BET NOT = ZERO                                 
466113             MOVE 3158-IDDISTR-BET TO TRANSIT-IDDISTR-BET                 
466114             MOVE JA               TO TRANSIT-FLBYTKND                    
466115                                      TRANSIT-FLINKLBS                    
466116           ELSE                                                           
466117             MOVE 3158-IDDISTR     TO TRANSIT-IDDISTR-BET                 
466118           END-IF                                                         
466119         ELSE                                                             
466120           MOVE ZERO               TO TRANSIT-IDDISTR-BET                 
466121         END-IF                                                           
466122                                                                          
466123         PERFORM S02-SKRIV-W37106                                         
466124                                                                          
466125         MOVE ZERO                 TO WS-SUPOINT                          
466126       END-IF                                                             
466127                                                                          
466128       MOVE RET-IDDISTR            TO WS-SPAR-IDDISTR-TRANSIT             
466129       MOVE RET-KDEXCHA            TO WS-SPAR-KDEXCHA-TRANSIT             
466130     END-IF                                                               
466131                                                                          
466132     IF NOT END-OF-W37113                                                 
466133       MOVE RET-DAAAVV             TO TRANSIT-DAAAVV                      
466134                                                                          
466135       PERFORM S20-KONTROLL-TRANSIT                                       
466136                                                                          
466137       IF RETURN-IN-TRANSIT                                               
466138         COMPUTE WS-SUPOINT = WS-SUPOINT + RET-SUPOINT                    
466139         END-COMPUTE                                                      
466142       END-IF                                                             
466143     END-IF                                                               
466144     .                                                                    
466145     EJECT                                                                
466146                                                                          
466147 S11-POINT-RETURNS SECTION.                                               
466148     PERFORM S20-KONTROLL-TRANSIT                                         
466149                                                                          
466150     IF RET-KDBYTSTA-RAPP >= '4'                                          
466152       MOVE RET-IDDISTR           TO W-3158-IDDISTR                       
466153       MOVE RET-KDEXCHA           TO W-3158-KDEXCHA                       
466154       PERFORM IMS-GU-WDGX3158                                            
466155                                                                          
466156       IF SEGMENT-FINNS                                                   
466157         MOVE RET-AREA            TO POINT-AREA                           
466158         MOVE 3158-IDDISTR-BET    TO POINT-IDDISTR-BET                    
466160         MOVE JA                  TO POINT-FLBYTKND                       
466161                                     POINT-FLINKLBS                       
466162       ELSE                                                               
466163         MOVE RET-AREA            TO POINT-AREA                           
466164         MOVE ZERO                TO POINT-IDDISTR-BET                    
466166         MOVE NEJ                 TO POINT-FLBYTKND                       
466167                                     POINT-FLINKLBS                       
466168       END-IF                                                             
466169                                                                          
466170       PERFORM S03-SKRIV-W37107                                           
466171     END-IF                                                               
466172     .                                                                    
466173     EJECT                                                                
466174                                                                          
466175 S12-UPDATE SECTION.                                                      
466176     IF FIRST-TIME                                                        
466177       MOVE NEJ               TO SW-FIRST-TIME                            
466178     ELSE                                                                 
466179       IF UPPDATERING                                                     
466180         IF WS-SPAR-IDDISTR-UPD NOT = RET-IDDISTR OR                      
466181          WS-SPAR-KDEXCHA-UPD   NOT = RET-KDEXCHA OR                      
466182          WS-SPAR-IDBYTRAP-UPD  NOT = RET-IDBYTRAP                        
466183                                                                          
466186           PERFORM S04-SKRIV-W37108                                       
466187                                                                          
466188           MOVE NEJ           TO SW-UPPDATERING                           
466189         END-IF                                                           
466190       END-IF                                                             
466191     END-IF                                                               
466192                                                                          
466193     IF NOT END-OF-W37113                                                 
466194       IF RET-KDBYTSTA-RAPP >= '4'                                        
466195         MOVE JA                TO SW-UPPDATERING                         
466200       END-IF                                                             
466201       MOVE RET-AREA            TO UPD-AREA                               
466202       MOVE RET-IDBYTRAP        TO WS-SPAR-IDBYTRAP-UPD                   
466203       MOVE RET-IDDISTR         TO WS-SPAR-IDDISTR-UPD                    
466204       MOVE RET-KDEXCHA         TO WS-SPAR-KDEXCHA-UPD                    
466205     END-IF                                                               
466206     .                                                                    
466207     EJECT                                                                
466208                                                                          
466209 S20-KONTROLL-TRANSIT SECTION.                                            
466210     MOVE NEJ                      TO SW-RETURN-IN-TRANSIT                
466300                                                                          
466400     IF RET-KDBYTSTA-RAPP = '3'                                           
466500       MOVE JA                     TO SW-RETURN-IN-TRANSIT                
466600     ELSE                                                                 
466610       IF RET-KDBYTSTA-RAPP = '2'                                         
466620                                                                          
466630         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
466640         IF RET-DADATUM NOT = 99999999                                    
466641           MOVE RET-DADATUM        TO WS-DAREGDAT                         
466650           MOVE WS-DAREGDAT-AAMMDD TO DAT-I-TIDATUM                       
466651         ELSE                                                             
466652           MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                       
466653         END-IF                                                           
466660                                                                          
466670         CALL WDATKONV USING DAT-KDDATFORM                                
466680                             DAT-I-TIDATUM                                
466690                             DAT-O-TIDATUM                                
466691                             DAT-KDSVAR                                   
466692                                                                          
466693         IF DAT-KDSVAR = ' '                                              
466694           MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV-NUM                       
466695         ELSE                                                             
466696           DISPLAY '**** TIAAVV' DAT-I-TIDATUM                            
466697           MOVE +1000 TO RKOD-ABEND-MED-DUMP                              
466698           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
466699         END-IF                                                           
466700                                                                          
466710         MOVE WS-TIAAVV-NUM        TO W009VADD-DATUM                      
466711                                                                          
466712         MOVE RET-IDDISTR          TO W-3158-IDDISTR                      
466713         MOVE RET-KDEXCHA          TO W-3158-KDEXCHA                      
466714         PERFORM IMS-GU-WDGX3158                                          
466715                                                                          
466716         IF SEGMENT-FINNS                                                 
466720           MOVE 3158-KVVECKOR-BYRE                                        
466721                                   TO W009VADD-ANTAL                      
466722         ELSE                                                             
466723           MOVE 10                 TO W009VADD-ANTAL                      
466724         END-IF                                                           
466725                                                                          
466730         CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                
466740         MOVE W009VADD-DATUM       TO WS-JFR-EXTENDED                     
466741                                                                          
466742         IF WS-JFR-EXTENDED < 9012                                        
466743           COMPUTE WS-JFR-EXTENDED-Y2K = 200000 +                         
466744                                         WS-JFR-EXTENDED                  
466745           END-COMPUTE                                                    
466746         ELSE                                                             
466747           COMPUTE WS-JFR-EXTENDED-Y2K = 190000 +                         
466748                                         WS-JFR-EXTENDED                  
466749           END-COMPUTE                                                    
466750         END-IF                                                           
466751                                                                          
466760         MOVE 'AAMMDD'             TO DAT-KDDATFORM                       
466770         MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                       
466780                                                                          
466790         CALL WDATKONV USING DAT-KDDATFORM                                
466791                             DAT-I-TIDATUM                                
466792                             DAT-O-TIDATUM                                
466793                             DAT-KDSVAR                                   
466794                                                                          
466795         IF DAT-KDSVAR = ' '                                              
466796           MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV-NUM                       
466797           MOVE WS-TIAAVV-NUM      TO WS-JFR-DAGENS                       
466799         ELSE                                                             
466800           DISPLAY '**** TIAAVV' DAT-I-TIDATUM                            
466801           MOVE +1000 TO RKOD-ABEND-MED-DUMP                              
466802           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
466810         END-IF                                                           
466820                                                                          
466825         IF WS-JFR-DAGENS < 9012                                          
466826           COMPUTE WS-JFR-DAGENS-Y2K = 200000 +                           
466827                                       WS-JFR-DAGENS                      
466828           END-COMPUTE                                                    
466829         ELSE                                                             
466830           COMPUTE WS-JFR-DAGENS-Y2K = 190000 +                           
466831                                       WS-JFR-DAGENS                      
466832           END-COMPUTE                                                    
466833         END-IF                                                           
466834                                                                          
466836         IF WS-JFR-DAGENS-Y2K <= WS-JFR-EXTENDED-Y2K                      
466840           MOVE JA                 TO SW-RETURN-IN-TRANSIT                
466850         END-IF                                                           
466860       END-IF                                                             
466870     END-IF                                                               
466880     .                                                                    
466890     EJECT                                                                
466891                                                                          
466900* --- IMS SEKTIONER ---                                                   
469520                                                                          
469521 IMS-GU-WDGX3158 SECTION.                                                 
469530     STRING 'WDR101  (WDGXKEY  =' W-3157-IDHTYP-X ')'                     
469540          DELIMITED BY SIZE INTO SSA1                                     
469550     STRING 'WDGX3158(KEY3158  =' W-3158-WDGXKEY-X ')'                    
469560          DELIMITED BY SIZE INTO SSA2                                     
469570     MOVE '  GE'           TO GODK-STATUSKODER                            
469580     CALL CBLTDLI USING GU   3158-PCB DLI-IO-WDGX3158 SSA1 SSA2           
469590     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
469591     PERFORM IMS-STATUSKONTROLL                                           
469592     .                                                                    
473900     EJECT                                                                
474100                                                                          
474110 IMS-STATUSKONTROLL SECTION.                                              
474200     SET STATUS-IX TO 1                                                   
474300     SEARCH GODK-STATUS                                                   
474400       AT END                                                             
474500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
474600           DELIMITED BY SIZE INTO FELTEXT                                 
474700         DISPLAY FELTEXT                                                  
474800         CALL FELLOG                                                      
474900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
475000         CONTINUE                                                         
475100     END-SEARCH                                                           
475200     .                                                                    
