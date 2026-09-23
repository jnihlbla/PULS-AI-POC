000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3712100.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   DEC-1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM LÄSER FAKTUROR/KREDITERINGAR/JUSTERINGAR PÅ WDA7             
000900*        OCH BYGGER/SKRIVER;                                              
001100*        . PENDING-POSTER             TILL RAPPORTER/HISTORIK             
001200*        . POÄNG-POSTER               TILL RAPPORTER/HISTORIK OCH         
001210*                                          UPPDATERING-WDGX3158           
001300*        . UPPDATERINGS-POSTER        TILL WDA7                           
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
004900*          --- AKTUELLA FAKT/KRED/JUST                                    
005000     SELECT W37115                     ASSIGN TO W37121D1.                
005100                                                                          
005500*          --- PENDING-POSTER                                             
005600     SELECT W37134                     ASSIGN TO W37121D2.                
005700                                                                          
005800*          --- POÄNG-POSTER                                               
005900     SELECT W37135                     ASSIGN TO W37121D3.                
005910                                                                          
006000*          --- UPPDATERINGS-POSTER                                        
006100     SELECT W37136                     ASSIGN TO W37121D4.                
006600     EJECT                                                                
006610                                                                          
006700 DATA DIVISION.                                                           
006800                                                                          
006900 FILE SECTION.                                                            
007100 FD  W37115                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500*01  FKJ-REC     -COPY W37109      -L.                                    
007800     EJECT                                                                
007900                                                                          
008810 FD  W37134                                                               
008820     RECORDING       F                                                    
008830     BLOCK CONTAINS  0.                                                   
008840                                                                          
008850*01  PENDING-REC -COPY W37109      -L.                                    
008860     EJECT                                                                
008870                                                                          
008880 FD  W37135                                                               
008890     RECORDING       F                                                    
008891     BLOCK CONTAINS  0.                                                   
008892                                                                          
008893*01  POINT-REC   -COPY W37109      -L.                                    
011400     EJECT                                                                
011410                                                                          
011430 FD  W37136                                                               
011440     RECORDING       F                                                    
011450     BLOCK CONTAINS  0.                                                   
011460                                                                          
011470*01  UPD-REC     -COPY W37109      -L.                                    
011480     EJECT                                                                
011490                                                                          
011500 WORKING-STORAGE SECTION.                                                 
011700*    -- CHECKED BY WY2000                                                 
011800 77  IDPGM                        PIC X(8)    VALUE 'W3712100'.           
011900 77  JA                           PIC X       VALUE 'J'.                  
012000 77  NEJ                          PIC X       VALUE 'N'.                  
012100 77  INDX                         PIC S9(2)   VALUE +0  COMP SYNC.        
012200 77  MAX-INDX                     PIC S9(2)   VALUE +16 COMP SYNC.        
012300 77  W37115-EOF-SW                PIC X       VALUE 'N'.                  
012400     88  END-OF-W37115                        VALUE 'J'.                  
012401 77  SW-TIME-LIMIT                PIC X       VALUE 'N'.                  
012402     88  TIME-LIMIT-EXCEEDED                  VALUE 'J'.                  
012403 77  SW-EXCLUSION                 PIC X       VALUE 'N'.                  
012404     88  EXCLUSION                            VALUE 'J'.                  
012500 77  WS-SPAR-IDDISTR-UPD          PIC S9(5)   VALUE +0 COMP-3.            
012600 77  WS-SPAR-KDEXCHA-UPD          PIC S9(3)   VALUE +0 COMP-3.            
012610                                                                          
012620 77  WS-SPAR-IDDISTR-PENDING      PIC S9(5)   VALUE +0 COMP-3.            
012630 77  WS-SPAR-KDEXCHA-PENDING      PIC S9(3)   VALUE +0 COMP-3.            
012640                                                                          
012700 77  WS-SUPOINT                   PIC S9(7)   VALUE +0 COMP-3.            
013110                                                                          
013200 01  FELTEXT                      PIC X(80).                              
013300     EJECT                                                                
019000 01  DAGENS-DATUM                 PIC 9(6)    VALUE ZERO.                 
019010                                                                          
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
022200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37121'.              
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
024700*01  AREA -COPY W37109     -PRE FKJ-                                      
025100     EJECT                                                                
025110                                                                          
025200 01  UPD-AREA-START              PIC X(24)   VALUE                        
025300                                 'UPPDAT-AREA-START  '.                   
025500                                                                          
025510*01  AREA -COPY W37109     -PRE UPD-                                      
026200     EJECT                                                                
026201                                                                          
026202 01  PENDING-AREA-START          PIC X(24)   VALUE                        
026203                                 'PENDING-AREA-START  '.                  
026204                                                                          
026205*01  AREA -COPY W37109     -PRE PENDING-                                  
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
038200     PERFORM S01-LAES-W37115                                              
038201                                                                          
038300     PERFORM UNTIL END-OF-W37115                                          
038500       PERFORM B-BEARBETA                                                 
038700       PERFORM S01-LAES-W37115                                            
038800     END-PERFORM                                                          
038900                                                                          
038910     IF WS-SUPOINT NOT = ZERO                                             
038920       PERFORM S12-PENDING                                                
039000     END-IF                                                               
039001                                                                          
039010     PERFORM Z-FINIT                                                      
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500     EJECT                                                                
039700                                                                          
039710 A-INIT SECTION.                                                          
039800     OPEN INPUT  W37115                                                   
039900                                                                          
040000     OPEN OUTPUT W37134                                                   
040100                 W37135                                                   
040200                 W37136                                                   
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
042420     MOVE NEJ                  TO SW-TIME-LIMIT                           
042430                                  SW-EXCLUSION                            
042440                                                                          
042502     MOVE FKJ-IDDISTR          TO W-3158-IDDISTR                          
042503     MOVE FKJ-KDEXCHA          TO W-3158-KDEXCHA                          
042504     PERFORM IMS-GU-WDGX3158                                              
042505                                                                          
042508     IF SEGMENT-FINNS                                                     
042509*    KONTROLLERA OM DET ÄR AKTUELLT ATT RAPPORTERA                        
042510       PERFORM BA-CONTROL-TIME-LIMIT                                      
042511*    KONTROLLERA OM FUNKTIONSGRUPPEN SKALL INGÅ                           
042512       PERFORM BB-CONTROL-EXCLUSION                                       
042513     END-IF                                                               
042514                                                                          
042516     IF SEGMENT-SAKNAS                                                    
042517*    STYRPARAMETRAR SAKNAS FÖR AKTUELL KUND                               
042518       PERFORM S10-REPORT                                                 
042519       PERFORM S11-UPDATE                                                 
042520     ELSE                                                                 
042521       IF EXCLUSION                                                       
042522*    TRANSAKTIONEN SKALL EJ PÅVERKA POÄNGSALDOT                           
042523         PERFORM S10-REPORT                                               
042524         PERFORM S11-UPDATE                                               
042525       ELSE                                                               
042526         IF TIME-LIMIT-EXCEEDED                                           
042527*    TRANSAKTIONEN HAR NÅTT FÄRDIGTIDPUNKT                                
042528           PERFORM S10-REPORT                                             
042529           PERFORM S11-UPDATE                                             
042530         ELSE                                                             
042531*    TRANSAKTIONEN HAR INTE NÅTT FÄRDIGTIDPUNKT                           
042540           PERFORM S12-PENDING                                            
042600         END-IF                                                           
042700       END-IF                                                             
042701     END-IF                                                               
445000     .                                                                    
445100     EJECT                                                                
445110                                                                          
445120 BA-CONTROL-TIME-LIMIT SECTION.                                           
445133     IF FKJ-IDPTYP = 'KRE' OR 'ADJ'                                       
445134       MOVE JA                 TO SW-TIME-LIMIT                           
445135     ELSE                                                                 
445137       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
445138       MOVE FKJ-DADATUM(3:6)   TO DAT-I-TIDATUM                           
445139                                                                          
445140       CALL WDATKONV USING DAT-KDDATFORM                                  
445141                           DAT-I-TIDATUM                                  
445142                           DAT-O-TIDATUM                                  
445150                           DAT-KDSVAR                                     
445160                                                                          
445170       IF DAT-KDSVAR = ' '                                                
445180         MOVE DAT-TIAAVV-GRP   TO WS-TIAAVV-NUM                           
445181       ELSE                                                               
445182         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
445183         MOVE +1000 TO RKOD-ABEND-MED-DUMP                                
445184         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
445185       END-IF                                                             
445186                                                                          
445187       MOVE WS-TIAAVV-NUM      TO W009VADD-DATUM                          
445189       COMPUTE W009VADD-ANTAL = 3158-KVVECKOR-BYFA +                      
445190                                FKJ-KVVECKOR                              
445191       END-COMPUTE                                                        
445195       CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                  
445196       MOVE W009VADD-DATUM     TO WS-JFR-EXTENDED                         
445197                                                                          
445198       IF WS-JFR-EXTENDED < 9012                                          
445199         COMPUTE WS-JFR-EXTENDED-Y2K = 200000 +                           
445200                                       WS-JFR-EXTENDED                    
445201         END-COMPUTE                                                      
445202       ELSE                                                               
445203         COMPUTE WS-JFR-EXTENDED-Y2K = 190000 +                           
445204                                       WS-JFR-EXTENDED                    
445205         END-COMPUTE                                                      
445206       END-IF                                                             
445207                                                                          
445208       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
445209       MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                           
445210                                                                          
445211       CALL WDATKONV USING DAT-KDDATFORM                                  
445212                           DAT-I-TIDATUM                                  
445213                           DAT-O-TIDATUM                                  
445214                           DAT-KDSVAR                                     
445215                                                                          
445216       IF DAT-KDSVAR = ' '                                                
445217         MOVE DAT-TIAAVV-GRP   TO WS-TIAAVV-NUM                           
445218         MOVE WS-TIAAVV-NUM    TO WS-JFR-DAGENS                           
445219       ELSE                                                               
445220         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
445221         MOVE +1000 TO RKOD-ABEND-MED-DUMP                                
445222         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
445223       END-IF                                                             
445224                                                                          
445228       IF WS-JFR-DAGENS < 9012                                            
445229         COMPUTE WS-JFR-DAGENS-Y2K = 200000 +                             
445230                                     WS-JFR-DAGENS                        
445231         END-COMPUTE                                                      
445232       ELSE                                                               
445233         COMPUTE WS-JFR-DAGENS-Y2K = 190000 +                             
445234                                     WS-JFR-DAGENS                        
445235         END-COMPUTE                                                      
445236       END-IF                                                             
445237                                                                          
445238       IF WS-JFR-DAGENS-Y2K >= WS-JFR-EXTENDED-Y2K                        
445239         MOVE JA               TO SW-TIME-LIMIT                           
445240       END-IF                                                             
445241     END-IF                                                               
445242     .                                                                    
445243     EJECT                                                                
445244                                                                          
445245 BB-CONTROL-EXCLUSION SECTION.                                            
445246     IF FKJ-IDPTYP = 'FAK' OR 'KRE'                                       
445250       MOVE +1                 TO INDX                                    
445260       PERFORM UNTIL INDX > MAX-INDX                                      
445261         IF FKJ-IDFKNGRP >= 3158-IDFKNGRP-FOM (INDX) AND                  
445262            FKJ-IDFKNGRP <= 3158-IDFKNGRP-TOM (INDX)                      
445264           MOVE JA             TO SW-EXCLUSION                            
445265           MOVE +16            TO INDX                                    
445266         END-IF                                                           
445267         ADD +1                TO INDX                                    
445268       END-PERFORM                                                        
445269     END-IF                                                               
445306     .                                                                    
445307     EJECT                                                                
445308                                                                          
445384 Z-FINIT SECTION.                                                         
445390     CLOSE W37115                                                         
445400           W37134                                                         
445500           W37135                                                         
445600           W37136                                                         
445900                                                                          
446000     MOVE 'S' TO POSTSUM-OPKOD                                            
446100     CALL POSTSUM USING POSTSUM-PARM                                      
446200     .                                                                    
446300     EJECT                                                                
446310                                                                          
446400 S01-LAES-W37115  SECTION.                                                
446600     READ W37115 INTO FKJ-AREA                                            
446700     AT END                                                               
446800        MOVE HIGH-VALUE         TO FKJ-AREA                               
446810        MOVE 99999              TO FKJ-IDDISTR                            
446820        MOVE 999                TO FKJ-KDEXCHA                            
446830        MOVE 9999999            TO FKJ-IDBYTRAP                           
446900        SET END-OF-W37115       TO TRUE                                   
447000                                                                          
447100     NOT AT END                                                           
447200        MOVE 'W37115'           TO POSTSUM-FDNAMN                         
447300        MOVE 'W37121D1'         TO POSTSUM-DDNAMN2                        
447400        MOVE SPACE              TO POSTSUM-TRANSTYP                       
447500        CALL POSTSUM USING POSTSUM-PARM                                   
447600     END-READ                                                             
447700     .                                                                    
447800                                                                          
450010 S02-SKRIV-W37134 SECTION.                                                
450030     MOVE SPACE                 TO   PENDING-REC                          
450040     WRITE PENDING-REC          FROM PENDING-AREA                         
450050                                                                          
450060     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
450070     MOVE 'W37134'              TO   POSTSUM-FDNAMN                       
450080     MOVE 'W37121D2'            TO   POSTSUM-DDNAMN2                      
450090     CALL POSTSUM USING POSTSUM-PARM                                      
450091     .                                                                    
450093                                                                          
450094 S03-SKRIV-W37135 SECTION.                                                
450096     MOVE SPACE                 TO   POINT-REC                            
450097     WRITE POINT-REC            FROM POINT-AREA                           
450098                                                                          
450099     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
450100     MOVE 'W37135'              TO   POSTSUM-FDNAMN                       
450101     MOVE 'W37121D3'            TO   POSTSUM-DDNAMN2                      
450102     CALL POSTSUM USING POSTSUM-PARM                                      
465800     .                                                                    
465801                                                                          
465810 S04-SKRIV-W37136 SECTION.                                                
465820     MOVE SPACE                 TO   UPD-REC                              
465830     WRITE UPD-REC              FROM UPD-AREA                             
465840                                                                          
465850     MOVE SPACE                 TO   POSTSUM-TRANSTYP                     
465860     MOVE 'W37136'              TO   POSTSUM-FDNAMN                       
465870     MOVE 'W37121D4'            TO   POSTSUM-DDNAMN2                      
465880     CALL POSTSUM USING POSTSUM-PARM                                      
465890     .                                                                    
465900     EJECT                                                                
466000                                                                          
466010 S10-REPORT SECTION.                                                      
466117     MOVE FKJ-AREA                TO POINT-AREA                           
466118                                                                          
466119     IF SEGMENT-FINNS                                                     
466120*    STYRPARAMETRAR FINNS FÖR AKTUELL KUND                                
466121       IF 3158-IDDISTR-BET NOT = ZERO                                     
466122         MOVE 3158-IDDISTR-BET    TO POINT-IDDISTR-BET                    
466123       ELSE                                                               
466124         MOVE 3158-IDDISTR        TO POINT-IDDISTR-BET                    
466125       END-IF                                                             
466126       MOVE JA                    TO POINT-FLBYTKND                       
466127       IF EXCLUSION                                                       
466128         MOVE NEJ                 TO POINT-FLINKLBS                       
466129       ELSE                                                               
466130         MOVE JA                  TO POINT-FLINKLBS                       
466131       END-IF                                                             
466132     ELSE                                                                 
466133*    STYRPARAMETRAR SAKNAS FÖR AKTUELL KUND                               
466134       MOVE ZERO                  TO POINT-IDDISTR-BET                    
466135       MOVE NEJ                   TO POINT-FLBYTKND                       
466136                                     POINT-FLINKLBS                       
466137     END-IF                                                               
466138                                                                          
466139     PERFORM S03-SKRIV-W37135                                             
466140     .                                                                    
466141     EJECT                                                                
466142                                                                          
466150 S11-UPDATE SECTION.                                                      
466152     MOVE FKJ-AREA            TO UPD-AREA                                 
466153                                                                          
466154     PERFORM S04-SKRIV-W37136                                             
466158     .                                                                    
466159     EJECT                                                                
466160                                                                          
466161 S12-PENDING SECTION.                                                     
466162     IF WS-SPAR-IDDISTR-PENDING NOT = FKJ-IDDISTR OR                      
466163        WS-SPAR-KDEXCHA-PENDING NOT = FKJ-KDEXCHA                         
466164       IF WS-SUPOINT NOT = ZERO                                           
466165         MOVE 'SUP'               TO PENDING-IDPTYP                       
466166         MOVE WS-SPAR-IDDISTR-PENDING                                     
466167                                  TO PENDING-IDDISTR                      
466168         MOVE WS-SPAR-KDEXCHA-PENDING                                     
466169                                  TO PENDING-KDEXCHA                      
466170         MOVE JA                  TO PENDING-FLBYTKND                     
466171         MOVE JA                  TO PENDING-FLINKLBS                     
466172         MOVE SPACE               TO PENDING-TENOTE                       
466173                                     PENDING-KDBYTREF                     
466174         MOVE ZERO                TO PENDING-IDORDER                      
466175                                     PENDING-IDBYTRAP                     
466180                                     PENDING-IDKUNDNR                     
466190                                     PENDING-IDARTNR                      
466200                                     PENDING-IDFKNGRP                     
466300                                     PENDING-IDBYTRAD                     
466400                                     PENDING-DADATUM                      
466500                                     PENDING-KDBYTSTA-RAPP                
466510                                     PENDING-KDBYTSTA-OBJ                 
466600                                     PENDING-KVPOINT                      
466700                                     PENDING-KVANTAL                      
466800                                     PENDING-KVRETUR-GODK                 
466810                                     PENDING-KVVECKOR                     
466820         MOVE WS-SUPOINT          TO PENDING-SUPOINT                      
466821         MOVE WS-SPAR-IDDISTR-PENDING                                     
466822                                  TO W-3158-IDDISTR                       
466823         MOVE WS-SPAR-KDEXCHA-PENDING                                     
466824                                  TO W-3158-KDEXCHA                       
466826                                                                          
466827         PERFORM IMS-GU-WDGX3158                                          
466830         MOVE 3158-IDDISTR-BET    TO PENDING-IDDISTR-BET                  
466840                                                                          
466850         PERFORM S02-SKRIV-W37134                                         
466851                                                                          
466852         MOVE ZERO                TO WS-SUPOINT                           
466860       END-IF                                                             
466870                                                                          
466890       MOVE FKJ-IDDISTR           TO WS-SPAR-IDDISTR-PENDING              
466892       MOVE FKJ-KDEXCHA           TO WS-SPAR-KDEXCHA-PENDING              
466896     END-IF                                                               
466897                                                                          
466910     IF NOT END-OF-W37115                                                 
466911       MOVE FKJ-DAAAVV            TO PENDING-DAAAVV                       
466913       IF FKJ-IDPTYP = 'FAK' OR 'ADJ'                                     
466914         COMPUTE WS-SUPOINT = WS-SUPOINT + FKJ-SUPOINT                    
466920         END-COMPUTE                                                      
466921       ELSE                                                               
466922         IF FKJ-IDPTYP = 'KRE'                                            
466923           COMPUTE WS-SUPOINT = WS-SUPOINT - FKJ-SUPOINT                  
466924           END-COMPUTE                                                    
466925         END-IF                                                           
466926       END-IF                                                             
466927     END-IF                                                               
466930     .                                                                    
466940     EJECT                                                                
466950                                                                          
467000* --- IMS SEKTIONER ---                                                   
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
