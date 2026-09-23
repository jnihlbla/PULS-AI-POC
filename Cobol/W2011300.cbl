000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011300.                                                
000300 AUTHOR.         KENT HELLQVIST.UPPDATERINGSPROGRAM (MPP).                
000400 DATE-WRITTEN.   DECEMBER 1985.                                           
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*       UPPDATERAR DFU-ANSLUTNA LEVERANTÖRER PÅ WDGX-BASEN.               
000800*       KDVECKOSL KAN UPPDATERAS.                                         
000900*       SAMT FL-PERIODSLUT OCH BEGÄRD (PERIOD-)SÄNDNINGSDATUM.            
001000*       IDHTYP=2215.                                                      
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T113                                              
001400*                     W2T113U                                             
001500*        MID:         W2I11301                                            
001600*    UTDATA.                                                              
001700*        MOD:         W2O11301                                            
001800*    SUBPROGRAM.                                                          
001900*        FELLOG                                                           
002000*        CBLTDLI                                                          
002100*        WDATKONV                                                         
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -COPY WY2000W1                                                       
002900     SKIP3                                                                
003000 77   PROGRAM-NAMN           VALUE 'W2011300'                             
003100                                 PIC  X(08).                              
003200 77  JA                          PIC  X(01)   VALUE 'J'.                  
003300 77  NEJ                         PIC  X(01)   VALUE 'N'.                  
003500 77  WS-IDLEVNR-SPAR             PIC  X(05).                              
003640 77  MAX-IND                     PIC S9(09)  VALUE +10  COMP SYNC.        
003700 77  WS-IDTRANS                  PIC  X(04).                              
003800     88  EGEN-BILD                          VALUE '2113'.                 
003900                                                                          
004000*------------------------------- SWITCHAR                                 
004100 77  SW-INPUT-RAETT              PIC X(01)  VALUE 'J'.                    
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400     03  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
004500     03  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
004600     EJECT                                                                
004700 01  NYCKLAR-TILL-DLI.                                                    
004800     03  W-WDGXKEY-ROT.                                                   
004900          05  FILLER           PIC X(04)  VALUE '2215'.                   
005000          05  FILLER           PIC X(26)  VALUE LOW-VALUE.                
005100     03  W-WDGXKEY-IDLEVNR.                                               
005200          05  W-IDLEVNR        PIC X(5).                                  
005300                                                                          
005400                                                                          
005500 01  MEDDELANDE.                                                          
005600     03  FEL-1                   PIC X(40) VALUE                          
005700            'SIDA 1 VISAS, FANNS EJ FLER LEVERANTÖRER'.                   
005800     03  FEL-2                   PIC X(32) VALUE                          
005900            'UPPLYSTA FÄLT FEL               '.                           
006000     03  FEL-3                   PIC X(40) VALUE                          
006100            'GÅR EJ MED J I BÅDE VECKO- & PERIODBATCH'.                   
006200     03  MED-1                   PIC X(32) VALUE                          
006300            'MER INFO PÅ NÄSTA SIDA          '.                           
006400     03  MED-2                   PIC X(32) VALUE                          
006500            'TRYCK PF11 FÖR UPPDATERING      '.                           
006600     03  MED-3                   PIC X(32) VALUE                          
006700            'UPPDATERING UTFÖRD              '.                           
006800                                                                          
006900 01  FLAGGA-FEL3                 PIC X     VALUE SPACE.                   
007000                                                                          
007100 01  DAGENS-DATUM                PIC 9(06).                               
007200                                                                          
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
007500     EJECT                                                                
007600*01  -COPY WDATAREA.                                                      
007700                                                                          
007800     EJECT                                                                
007900*                        ****    MFS OCH SKÄRMHANTERING                   
008000 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
008100     SKIP2                                                                
008200*01  MID -COPY W2I11301                                                   
008300     EJECT                                                                
008400 01  FILLER              PIC X(16)   VALUE 'WMSGAREA'.                    
008500     SKIP2                                                                
008600*01  -COPY WMSGAREA                                                       
008700     EJECT                                                                
008800*    03  MOD -COPY W2O11301  -RED MSG-AREA.                               
008900     EJECT                                                                
009000*01  -COPY WMFSAREA.                                                      
009100     EJECT                                                                
009200******************************************************************        
009300*****                                                                     
009400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*****                                                                     
009600 01  IMS-WS.                                                              
009700     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
009800     SKIP3                                                                
009900*****                    **** STATUS-KOD FRÅN IMS                         
010000     03  STATUS-WS               PIC X(2).                                
010100         88  SEGMENT-FINNS                   VALUE '  '.                  
010200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
010300     SKIP3                                                                
010400     03  GODK-STATUSKODER.                                                
010500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800     EJECT                                                                
010900*                            IMS FUNKTIONSKODER                           
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*                            DLI INPUT-OUTPUT AREA                        
011300 01  DLI-IO-AREA.                                                         
011400     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
011500     SKIP3                                                                
011600*    03  WLXXBK -COPY WDGX01      -PRE WLXXBK- -RED IO-AREA.              
011700     EJECT                                                                
011800*    03  WLXXBK -COPY WDGX2216    -PRE WLXXBK- -RED IO-AREA.              
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100     SKIP2                                                                
012200*01  -COPY W0009     -PRE MSG-                                            
012300     EJECT                                                                
012400*01  -COPY W0008     -PRE WLXXBK-                                         
012500         05  FILLER              PIC X.                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION USING MSG-PCB WLXXBK-PCB.                             
012800     SKIP1                                                                
012900     ENTRY 'DLITCBL' USING MSG-PCB WLXXBK-PCB.                            
013000     SKIP2                                                                
013100     PERFORM IMS-GET-MSG                                                  
013200     IF SEGMENT-FINNS                                                     
013300        PERFORM A-INIT-SPARA-INPUT                                        
013400        PERFORM IMS-GET-WLXXBK01                                          
013500        IF MFS-UPDATE  AND EGEN-BILD                                      
013600           PERFORM B-KOLLA-INPUT                                          
013700           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT                   
013800           IF SW-INPUT-RAETT = JA                                         
013900              PERFORM C-KOLLA-MOT-BASEN                                   
014000              IF SW-INPUT-RAETT = JA                                      
014100                 PERFORM D-UPPDATERA                                      
014200              ELSE                                                        
014300                 IF FLAGGA-FEL3 = JA                                      
014400                    MOVE FEL-3 TO MOD-TEMFSFEL                            
014500                 ELSE                                                     
014600                    MOVE FEL-2 TO MOD-TEMFSFEL                            
014700                 END-IF                                                   
014800              END-IF                                                      
014900           ELSE                                                           
015000              MOVE FEL-2 TO MOD-TEMFSFEL                                  
015100           END-IF                                                         
015200        ELSE                                                              
015300           PERFORM S01-FIXA-STARTVARDE                                    
015400           PERFORM E-VISA-BILD                                            
015500        END-IF                                                            
015600                                                                          
015710        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11301 + 4                     
015800        PERFORM IMS-INSERT-MSG                                            
015900     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT-SPARA-INPUT SECTION.                                              
016600     SKIP2                                                                
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11301               
016900         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS  WS-IDTRANS                    
017000         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
017100         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
017200         MOVE MSG-IDPFK            TO MFS-IDPFK                           
017300     ELSE                                                                 
017400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11301                
017500         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS  WS-IDTRANS                    
017600         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
017700         MOVE ' ' TO MFS-KDTRTYP                                          
017800                     MFS-IDPFK                                            
017900     END-IF                                                               
018000                                                                          
018100     ACCEPT DAGENS-DATUM FROM DATE                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MOD-W2O11301                                       
018400     MOVE 'W2O113N1' TO MFS-IDMOD                                         
018500     MOVE '2113' TO MOD-IDTRANS                                           
018600                                                                          
018700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
018800                             MOD-TEMFSINF                                 
018900                             MOD-IDLEVNR-START-IN                         
019000     IF MID-IDLEVNR-START-IN NOT = SPACE                                  
019100        MOVE MID-IDLEVNR-START-IN TO MOD-IDLEVNR-START-UT                 
019200     ELSE                                                                 
019300        MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-START-UT                 
019400     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 B-KOLLA-INPUT SECTION.                                                   
019800                                                                          
019900     MOVE JA TO SW-INPUT-RAETT                                            
020000                                                                          
020100        SET MID-INFO-IND TO 1                                             
020200****                                                                      
020300* SÄTT UPP INDEX MED 1 SÅ LÄNGE:                                          
020400*  -INDEX < MAX   OCH                                                     
020500*  -INFO-IDLEVNR (INDEX) ÄR NUMERISKT  OCH                                
020600*  -INMATAT IDLEVNR INTE ÄR LIKA MED INFO-IDLEVNR (INDEX)                 
020700****                                                                      
020900        MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO WS-IDLEVNR-SPAR           
021000        PERFORM UNTIL MID-INFO-IND = MAX-IND OR                           
021100                MID-IDLEVNR = MID-INFO-IDLEVNR (MID-INFO-IND)             
021200                                                                          
021300           SET MID-INFO-IND UP BY 1                                       
021400           MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO WS-IDLEVNR-SPAR        
021411                                                                          
021500        END-PERFORM                                                       
021510                                                                          
021620        IF MID-IDLEVNR = WS-IDLEVNR-SPAR                                  
021700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDLEVNR-IN-ATTR              
021800        ELSE                                                              
021900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVNR-IN-ATTR              
022000           MOVE NEJ                   TO SW-INPUT-RAETT                   
022100        END-IF                                                            
022200        MOVE MFS-ROER-EJ-FAELT        TO MOD-IDLEVNR-IN                   
022300                                                                          
022310     IF MID-KDVECKOSL NOT = ALL '+'                                       
022320        IF MID-KDVECKOSL = 'D' OR 'N'                                     
022330           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDVECKOSL-IN-ATTR             
022331           IF MID-KDVECKOSL = 'N'                                         
022332              MOVE ZERO                 TO MID-TISEND-PER                 
022333                                           MOD-TISEND-PER-IN              
022334           END-IF                                                         
022340        ELSE                                                              
022350           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDVECKOSL-IN-ATTR              
022360           MOVE NEJ                     TO SW-INPUT-RAETT                 
022370        END-IF                                                            
022380        MOVE MFS-ROER-EJ-FAELT          TO MOD-KDVECKOSL-IN               
022390     ELSE                                                                 
022391        MOVE MFS-RENSA-FAELT            TO MOD-KDVECKOSL-IN               
022392     END-IF                                                               
022393                                                                          
022400     IF MID-TISEND-PER NOT = ALL '+'                                      
022500        IF MID-TISEND-PER NUMERIC                                         
022600           MOVE MID-TISEND-PER   TO TMP1-YYMMDD                           
022700           MOVE DAGENS-DATUM     TO TMP2-YYMMDD                           
022800           PERFORM WY2000P1                                               
022900           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
023000              MOVE 'AAMMDD'            TO DAT-KDDATFORM                   
023100              MOVE MID-TISEND-PER      TO DAT-I-TIDATUM                   
023200              CALL WDATKONV USING DAT-KDDATFORM                           
023300                                  DAT-I-TIDATUM                           
023400                                  DAT-O-TIDATUM                           
023500                                  DAT-KDSVAR                              
023600              IF DAT-KDSVAR-OK                                            
023700                 MOVE MFS-NUM-FAELT-RAETT TO                              
023800                                       MOD-TISEND-PER-IN-ATTR             
023900              ELSE                                                        
024000                 MOVE MFS-NUM-FAELT-FEL   TO                              
024100                                       MOD-TISEND-PER-IN-ATTR             
024200                 MOVE NEJ                 TO SW-INPUT-RAETT               
024300              END-IF                                                      
024400           ELSE                                                           
024500              IF MID-TISEND-PER = ZERO                                    
024600                 MOVE MFS-NUM-FAELT-RAETT TO                              
024700                                          MOD-TISEND-PER-IN-ATTR          
024800              ELSE                                                        
024900                 MOVE MFS-NUM-FAELT-FEL   TO                              
025000                                          MOD-TISEND-PER-IN-ATTR          
025100                 MOVE NEJ                 TO SW-INPUT-RAETT               
025200              END-IF                                                      
025300           END-IF                                                         
025400        ELSE                                                              
025500           MOVE MFS-NUM-FAELT-FEL   TO                                    
025600                                    MOD-TISEND-PER-IN-ATTR                
025700           MOVE NEJ                     TO SW-INPUT-RAETT                 
025800        END-IF                                                            
025900        MOVE MFS-ROER-EJ-FAELT          TO MOD-TISEND-PER-IN              
026000     ELSE                                                                 
026100        MOVE MFS-RENSA-FAELT            TO MOD-TISEND-PER-IN              
026200     END-IF                                                               
031000     IF MID-KDVECKOSL  = ALL '+'  AND                                     
031300        MID-TISEND-PER = ALL '+'                                          
031400        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDVECKOSL-IN-ATTR          
031700        MOVE MFS-NUM-FAELT-FEL          TO MOD-TISEND-PER-IN-ATTR         
031800        MOVE NEJ                        TO SW-INPUT-RAETT                 
031900     END-IF                                                               
032000                                                                          
032100     IF SW-INPUT-RAETT = NEJ                                              
032200        PERFORM S02-ROER-EJ-FAELT                                         
032300     END-IF                                                               
032400                                                                          
032500     .                                                                    
032600     EJECT                                                                
032700 C-KOLLA-MOT-BASEN SECTION.                                               
032800     SKIP2                                                                
032900     MOVE MID-IDLEVNR                  TO W-IDLEVNR                       
033000     PERFORM IMS-GET-WLXXBK11-UNIK                                        
033100                                                                          
033200                                                                          
033300     IF MID-TISEND-PER NOT = ALL '+'                                      
033400        IF MID-TISEND-PER NUMERIC                                         
033500           MOVE MID-TISEND-PER           TO TMP1-YYMMDD                   
033600           MOVE WLXXBK-2216-TISEND-SEN   TO TMP2-YYMMDD                   
033700           PERFORM WY2000P1                                               
033800           IF TMP1-YYMMDD >= TMP2-YYMMDD OR                               
033900              MID-TISEND-PER = ZERO                                       
034000              MOVE MFS-NUM-FAELT-RAETT TO MOD-TISEND-PER-IN-ATTR          
034100           ELSE                                                           
034200              MOVE MFS-NUM-FAELT-FEL   TO MOD-TISEND-PER-IN-ATTR          
034300              MOVE NEJ                 TO SW-INPUT-RAETT                  
034400           END-IF                                                         
034500        END-IF                                                            
034600     END-IF                                                               
034881                                                                          
035000     IF SW-INPUT-RAETT = NEJ                                              
035100        PERFORM S02-ROER-EJ-FAELT                                         
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
039000 D-UPPDATERA SECTION.                                                     
039100     SKIP2                                                                
039200     PERFORM S02-ROER-EJ-FAELT                                            
039300                                                                          
039400     SET MOD-INFO-IND TO MID-INFO-IND                                     
039500                                                                          
039600     IF MID-KDVECKOSL NOT = ALL '+'                                       
039700        MOVE MID-KDVECKOSL        TO WLXXBK-2216-KDVECKOSL                
039800                                     MOD-INFO-KDVECKOSL                   
039900                                        (MOD-INFO-IND)                    
040000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-INFO-KDVECKOSL-ATTR             
040100                                        (MOD-INFO-IND)                    
040200        IF WLXXBK-2216-KDVECKOSL = 'D'                                    
040201           MOVE 'J'               TO WLXXBK-2216-FLLEVVB                  
040202        ELSE                                                              
040203           MOVE 'N'               TO WLXXBK-2216-FLLEVVB                  
040204        END-IF                                                            
040205        MOVE 'N'               TO WLXXBK-2216-FLLEVPLP                    
040210     END-IF                                                               
040300                                                                          
041900                                                                          
042000     IF MID-TISEND-PER NOT = ALL '+'                                      
042100        MOVE MID-TISEND-PER       TO WLXXBK-2216-TISEND-PER               
042200                                     MOD-INFO-TISEND-PER                  
042300                                        (MOD-INFO-IND)                    
042400        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-INFO-TISEND-PER-ATTR            
042500                                        (MOD-INFO-IND)                    
042600     END-IF                                                               
042700                                                                          
042800     PERFORM IMS-REPLACE                                                  
042900                                                                          
043000                                                                          
043100     MOVE MFS-RENSA-FAELT            TO MOD-IDLEVNR-IN                    
043200                                        MOD-KDVECKOSL-IN                  
043500                                        MOD-TISEND-PER-IN                 
043600                                                                          
043700     MOVE MFS-FORMATETS-ATTR         TO MOD-IDLEVNR-IN-ATTR               
043800                                        MOD-KDVECKOSL-IN-ATTR             
044100                                        MOD-TISEND-PER-IN-ATTR            
044200     MOVE MED-3                      TO MOD-TEMFSINF                      
044300     .                                                                    
044400     EJECT                                                                
044500 E-VISA-BILD SECTION.                                                     
044600     SKIP2                                                                
044700     IF MFS-IDPFK = '8'                                                   
044800        PERFORM IMS-GNP-WLXXBK11                                          
044900     ELSE                                                                 
045000        PERFORM IMS-GET-WLXXBK11                                          
045100     END-IF                                                               
045200                                                                          
045300     IF SEGMENT-SAKNAS                                                    
045400        MOVE SPACE                TO W-IDLEVNR                            
045500        PERFORM IMS-GET-WLXXBK11-FIRST                                    
045600     END-IF                                                               
045700                                                                          
045800     SET MOD-INFO-IND             TO 1                                    
045801     PERFORM EA-FLYTTA-TILL-MOD                                           
045940                                                                          
046000     PERFORM UNTIL MOD-INFO-IND = MAX-IND                                 
046010        SET MOD-INFO-IND UP BY 1                                          
046020        PERFORM EA-FLYTTA-TILL-MOD                                        
051700     END-PERFORM                                                          
051800                                                                          
051900     IF SEGMENT-FINNS                                                     
052000        MOVE MED-1                  TO MOD-TEMFSINF                       
052100     END-IF                                                               
052200                                                                          
052300     MOVE MFS-ROER-EJ-FAELT         TO MOD-IDLEVNR-IN                     
052400                                       MOD-KDVECKOSL-IN                   
052700                                       MOD-TISEND-PER-IN                  
052800                                                                          
052900     IF MID-IDLEVNR    = ALL '+'  AND                                     
053000        MID-KDVECKOSL  = ALL '+'  AND                                     
053300        MID-TISEND-PER = ALL '+'                                          
053400        MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-IN                     
053500     ELSE                                                                 
053600        MOVE MED-2                  TO MOD-TEMFSINF                       
053700        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDLEVNR-IN-ATTR                
053800                                       MOD-KDVECKOSL-IN-ATTR              
054100                                       MOD-TISEND-PER-IN-ATTR             
054200     END-IF                                                               
054300                                                                          
054400     .                                                                    
054500     EJECT                                                                
054501 EA-FLYTTA-TILL-MOD SECTION.                                              
054510                                                                          
054511     IF SEGMENT-FINNS                                                     
054523        MOVE WLXXBK-2216-IDLEVNR   TO MOD-INFO-IDLEVNR                    
054524                                  (MOD-INFO-IND)                          
054525        MOVE WLXXBK-2216-IDOVERFNR TO MOD-INFO-IDOVERFNR                  
054526                                  (MOD-INFO-IND)                          
054527        MOVE WLXXBK-2216-KDVECKOSL  TO MOD-INFO-KDVECKOSL                 
054528                                  (MOD-INFO-IND)                          
054540        MOVE WLXXBK-2216-TISEND-SEN TO MOD-INFO-TISEND-SEN                
054541                                  (MOD-INFO-IND)                          
054546        MOVE WLXXBK-2216-TISEND-PER TO MOD-INFO-TISEND-PER                
054547                                  (MOD-INFO-IND)                          
054548        PERFORM IMS-GET-WLXXBK11                                          
054549     ELSE                                                                 
054550        MOVE MFS-RENSA-FAELT TO MOD-INFO-IDLEVNR                          
054551                                  (MOD-INFO-IND)                          
054552                                MOD-INFO-IDOVERFNR                        
054553                                  (MOD-INFO-IND)                          
054554                                MOD-INFO-KDVECKOSL                        
054555                                  (MOD-INFO-IND)                          
054556                                MOD-INFO-TISEND-SEN                       
054557                                  (MOD-INFO-IND)                          
054562                                MOD-INFO-TISEND-PER                       
054563                                  (MOD-INFO-IND)                          
054564     END-IF                                                               
054565     .                                                                    
054566     EJECT                                                                
054600 S01-FIXA-STARTVARDE SECTION.                                             
054700     SKIP3                                                                
054800     IF NOT EGEN-BILD  OR  MFS-IDPFK = '7'                                
054900        MOVE SPACE           TO W-IDLEVNR                                 
055000        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT                      
055100     ELSE                                                                 
055200        IF MFS-IDPFK = '8'                                                
055300           MOVE MID-INFO-IDLEVNR (10) TO WS-IDLEVNR-SPAR                  
055400           MOVE WS-IDLEVNR-SPAR       TO W-IDLEVNR                        
055500           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT                   
055600        ELSE                                                              
055700           MOVE MID-INFO-IDLEVNR (1)  TO WS-IDLEVNR-SPAR                  
055800           IF MID-IDLEVNR-START-IN NOT = ALL '+'                          
055900              MOVE MID-IDLEVNR-START-IN TO WS-IDLEVNR-SPAR                
056000           END-IF                                                         
056100           MOVE WS-IDLEVNR-SPAR       TO W-IDLEVNR                        
056200        END-IF                                                            
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 S02-ROER-EJ-FAELT SECTION.                                               
056700     SKIP3                                                                
056800     SET MOD-INFO-IND                TO 1                                 
056900                                                                          
056910     MOVE MFS-ROER-EJ-FAELT  TO MOD-INFO-IDLEVNR                          
056920                                (MOD-INFO-IND)                            
056930                                MOD-INFO-IDOVERFNR                        
056940                                (MOD-INFO-IND)                            
056950                                MOD-INFO-KDVECKOSL                        
056960                                (MOD-INFO-IND)                            
056970                                MOD-INFO-TISEND-SEN                       
056980                                (MOD-INFO-IND)                            
056994                                MOD-INFO-TISEND-PER                       
056995                                (MOD-INFO-IND)                            
056996                                                                          
057000     PERFORM UNTIL MOD-INFO-IND = MAX-IND                                 
057010        SET MOD-INFO-IND UP BY 1                                          
057100        MOVE MFS-ROER-EJ-FAELT  TO MOD-INFO-IDLEVNR                       
057200                                   (MOD-INFO-IND)                         
057300                                   MOD-INFO-IDOVERFNR                     
057400                                   (MOD-INFO-IND)                         
057500                                   MOD-INFO-KDVECKOSL                     
057600                                   (MOD-INFO-IND)                         
057700                                   MOD-INFO-TISEND-SEN                    
057800                                   (MOD-INFO-IND)                         
058300                                   MOD-INFO-TISEND-PER                    
058400                                   (MOD-INFO-IND)                         
058600     END-PERFORM                                                          
058700                                                                          
058800     EJECT                                                                
058900* IMS SEKTIONER                                                           
059000     SKIP3                                                                
059100     .                                                                    
059200 IMS-GET-MSG SECTION.                                                     
059300     SKIP2                                                                
059400     MOVE '  QC' TO GODK-STATUSKODER                                      
059500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059700     PERFORM IMS-STATUS-KONTROLL                                          
059800     SKIP3                                                                
059900     .                                                                    
060000 IMS-INSERT-MSG SECTION.                                                  
060100     SKIP2                                                                
060300     MOVE 'N' TO MFS-KDHUVOMR                                             
060500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060600     MOVE SPACE TO GODK-STATUSKODER                                       
060700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060900     PERFORM IMS-STATUS-KONTROLL                                          
061000     .                                                                    
061100     EJECT                                                                
061200 IMS-GET-WLXXBK01 SECTION.                                                
061300     SKIP2                                                                
061400     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
061500             DELIMITED BY SIZE INTO SSA1                                  
061600     MOVE '  ' TO GODK-STATUSKODER                                        
061700     CALL CBLTDLI USING GHU WLXXBK-PCB DLI-IO-AREA SSA1                   
061800     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
061900     PERFORM IMS-STATUS-KONTROLL                                          
062000     SKIP3                                                                
062100     .                                                                    
062200 IMS-GET-WLXXBK11 SECTION.                                                
062300     SKIP2                                                                
062400     STRING 'WLXXBK11(IDLEVNR =>' W-WDGXKEY-IDLEVNR ')'                   
062500             DELIMITED BY SIZE INTO SSA1                                  
062600     MOVE '  GE' TO GODK-STATUSKODER                                      
062700     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
062800     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
062900     PERFORM IMS-STATUS-KONTROLL                                          
063000     SKIP3                                                                
063100     .                                                                    
063200     EJECT                                                                
063300 IMS-GET-WLXXBK11-UNIK SECTION.                                           
063400     SKIP2                                                                
063500     STRING 'WLXXBK11(IDLEVNR  =' W-WDGXKEY-IDLEVNR ')'                   
063600             DELIMITED BY SIZE INTO SSA1                                  
063700     MOVE '  ' TO GODK-STATUSKODER                                        
063800     CALL CBLTDLI USING GHNP WLXXBK-PCB DLI-IO-AREA SSA1                  
063900     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
064000     PERFORM IMS-STATUS-KONTROLL                                          
064100     SKIP3                                                                
064200     .                                                                    
064300 IMS-GET-WLXXBK11-FIRST SECTION.                                          
064400     SKIP2                                                                
064500     STRING 'WLXXBK11*F(IDLEVNR =>' W-WDGXKEY-IDLEVNR ')'                 
064600             DELIMITED BY SIZE INTO SSA1                                  
064700     MOVE '  GE' TO GODK-STATUSKODER                                      
064800     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
064900     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
065000     PERFORM IMS-STATUS-KONTROLL                                          
065100     SKIP3                                                                
065200     .                                                                    
065300     SKIP3                                                                
065400 IMS-GNP-WLXXBK11 SECTION.                                                
065500     SKIP2                                                                
065600     STRING 'WLXXBK11(IDLEVNR  >' W-WDGXKEY-IDLEVNR ')'                   
065700             DELIMITED BY SIZE INTO SSA1                                  
065800     MOVE '  GE' TO GODK-STATUSKODER                                      
065900     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
066000     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
066100     PERFORM IMS-STATUS-KONTROLL                                          
066200     SKIP3                                                                
066300     .                                                                    
066400     EJECT                                                                
066500 IMS-REPLACE SECTION.                                                     
066600     SKIP2                                                                
067900     MOVE '  '   TO GODK-STATUSKODER                                      
068000     CALL CBLTDLI USING REPL WLXXBK-PCB DLI-IO-AREA                       
068100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
068200     PERFORM IMS-STATUS-KONTROLL                                          
068300     SKIP3                                                                
068400     SKIP3                                                                
068500     .                                                                    
068600 IMS-STATUS-KONTROLL SECTION.                                             
068700     SET STATUS-IX TO 1                                                   
068800     SEARCH GODK-STATUS AT END CALL FELLOG                                
068900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
069000     END-SEARCH                                                           
069100     CONTINUE                                                             
069200     .                                                                    
069300     EJECT                                                                
069400*    -COPY WY2000P1                                                       
