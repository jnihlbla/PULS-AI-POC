000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011400.                                                
000300 AUTHOR.         KENT HELLQVIST.UPPDATERINGSPROGRAM (MPP).                
000400 DATE-WRITTEN.   FEBRUARI 1986.                                           
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*       UTFÖR BORTTAG ELLER NYUPPLÄGG AV LEVERANTÖRER PÅ                  
000800*       WDGX-BASEN. IDHTYP = 2215.                                        
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W2T114                                              
001200*                     W2T114U                                             
001300*        MID:         W2I11401                                            
001400*    UTDATA.                                                              
001500*        MOD:         W2O11401                                            
001600*    SUBPROGRAM.                                                          
001700*        FELLOG                                                           
001800*        CBLTDLI                                                          
001900*    SKIP3                                                                
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP3                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77   PROGRAM-NAMN           VALUE 'W2011400'                             
002900                                 PIC  X(08).                              
003000 77  CURRENT-SECTION         PIC X(30) VALUE SPACE.                       
003100 77  DBS-SECTION             PIC X(30) VALUE SPACE.                       
003200 77  JA                          PIC  X(01)   VALUE 'J'.                  
003300 77  NEJ                         PIC  X(01)   VALUE 'N'.                  
003400 77  MAX-MOD-LAENGD              PIC S9(04)  VALUE +375 COMP SYNC.        
003500 77  WS-IDLEVNR-SPAR             PIC  X(05).                              
003600 77  MAX-IND                     PIC S9(09)  VALUE +10  COMP SYNC.        
003700 77  WS-IDTRANS                  PIC  X(04).                              
003800     88  EGEN-BILD                          VALUE '2114'.                 
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
005210     03  W-KDSEGKEY-X.                                                    
005220         05  W-KDSEGKEY        PIC X(1)    VALUE '1'.                     
005221     03  W-IDLEVNR-X.                                                     
005222          05  W-IDLEVNR-WDD9   PIC X(5).                                  
005230     03  W-KDAVROP-X.                                                     
005240         05  W-KDAVROP         PIC S9(1)   VALUE +2   COMP-3.             
005300                                                                          
005400                                                                          
005500 01  MEDDELANDE.                                                          
005600     03  FEL-1                   PIC X(40) VALUE                          
005700            'SIDA 1 VISAS, FANNS EJ FLER LEVERANTÖRER'.                   
005800     03  FEL-2                   PIC X(32) VALUE                          
005900            'UPPLYSTA FÄLT FEL               '.                           
006000     03  FEL-3                   PIC X(32) VALUE                          
006100            'LEVERANTÖR FINNS REDAN          '.                           
006200     03  FEL-4                   PIC X(32) VALUE                          
006300            'LEVERANTÖR SAKNAS               '.                           
006310     03  FEL-5                   PIC X(32) VALUE                          
006320            'BORTTAG EJ TILLÅTET             '.                           
006400     03  MED-1                   PIC X(32) VALUE                          
006500            'MER INFO PÅ NÄSTA SIDA          '.                           
006600     03  MED-2                   PIC X(32) VALUE                          
006700            'TRYCK PF11 FÖR UPPDATERING      '.                           
006800     03  MED-3                   PIC X(32) VALUE                          
006900            'UPPDATERING UTFÖRD              '.                           
007000                                                                          
007100                                                                          
007200     EJECT                                                                
007300*                        ****    MFS OCH SKÄRMHANTERING                   
007400 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
007500     SKIP2                                                                
007600*01  MID -COPY W2I11401                                                   
007700     EJECT                                                                
007800 01  FILLER              PIC X(16)   VALUE 'WMSGAREA'.                    
007900     SKIP2                                                                
008000*01  -COPY WMSGAREA                                                       
008100     EJECT                                                                
008200*    03  MOD -COPY W2O11401  -RED MSG-AREA.                               
008300     EJECT                                                                
008400*01  -COPY WMFSAREA.                                                      
008500     EJECT                                                                
008600******************************************************************        
008700*****                                                                     
008800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*****                                                                     
009000 01  IMS-WS.                                                              
009100     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
009200     SKIP3                                                                
009300*****                    **** STATUS-KOD FRÅN IMS                         
009400     03  STATUS-WS               PIC X(2).                                
009500         88  SEGMENT-FINNS                   VALUE '  '.                  
009600         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
009610         88  SEGMENT-SLUT                    VALUE 'GB'.                  
009700         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
009800     SKIP3                                                                
009900     03  GODK-STATUSKODER.                                                
010000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*                            IMS FUNKTIONSKODER                           
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010800*                            DLI INPUT-OUTPUT AREA                        
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA                 PIC X(60)   VALUE SPACE.                 
011100     SKIP3                                                                
011200*    03  XXBK -COPY WDGX01        -PRE XXBK- -RED IO-AREA.                
011300     EJECT                                                                
011400*    03  XXBK -COPY WDGX2216      -PRE XXBK- -RED IO-AREA.                
011500     EJECT                                                                
011510 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDD902'.         
011520 01  DLI-IO-WDD902.                                                       
011530*    03 -COPY WDD902                                                      
011540     EJECT                                                                
011550 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDD905'.         
011560 01  DLI-IO-WDD905.                                                       
011570*    03 -COPY WDD905                                                      
011580     EJECT                                                                
011600 LINKAGE SECTION.                                                         
011700     SKIP2                                                                
011800*01  -COPY W0009     -PRE MSG-                                            
011900     EJECT                                                                
012000*01  -COPY W0008     -PRE WLXXBK-                                         
012100         05  FILLER              PIC X.                                   
012200     EJECT                                                                
012210*01  -COPY W0008     -PRE WDD9-                                           
012220         05  FILLER              PIC X.                                   
012230     EJECT                                                                
012300 PROCEDURE DIVISION USING MSG-PCB WLXXBK-PCB WDD9-PCB.                    
012400     SKIP1                                                                
012500     ENTRY 'DLITCBL' USING MSG-PCB WLXXBK-PCB WDD9-PCB.                   
012600     SKIP2                                                                
012700     PERFORM IMS-GET-MSG                                                  
012800     IF SEGMENT-FINNS                                                     
012900        PERFORM A-INIT-SPARA-INPUT                                        
013000        PERFORM IMS-GET-WLXXBK01                                          
013100        IF MFS-UPDATE  AND EGEN-BILD                                      
013200           PERFORM B-KOLLA-INPUT                                          
013300           IF SW-INPUT-RAETT = JA                                         
013400              PERFORM C-KOLLA-INPUT-2                                     
013500              IF SW-INPUT-RAETT = JA                                      
013600                 MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT             
013700                 PERFORM D-UPPDATERA                                      
013800              ELSE                                                        
013810                 IF MOD-TEMFSFEL = SPACE                                  
013900                    MOVE FEL-2 TO MOD-TEMFSFEL                            
013910                 END-IF                                                   
014000              END-IF                                                      
014100           ELSE                                                           
014200              MOVE FEL-2 TO MOD-TEMFSFEL                                  
014300           END-IF                                                         
014400        ELSE                                                              
014500           IF NOT EGEN-BILD  OR  MFS-IDPFK = '7'                          
014600              MOVE SPACE                    TO W-IDLEVNR                  
014700              MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT                
014800           ELSE                                                           
014900              IF MFS-IDPFK = '8'                                          
015000                 MOVE MID-INFO-IDLEVNR (10) TO WS-IDLEVNR-SPAR            
015100                 MOVE WS-IDLEVNR-SPAR       TO W-IDLEVNR                  
015200                 MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT             
015300              ELSE                                                        
015400                 MOVE MID-INFO-IDLEVNR (1)  TO WS-IDLEVNR-SPAR            
015500                 IF MID-IDLEVNR-START-IN NOT = ALL '+'                    
015600                    MOVE MID-IDLEVNR-START-IN TO WS-IDLEVNR-SPAR          
015700                 END-IF                                                   
015800                 MOVE WS-IDLEVNR-SPAR       TO W-IDLEVNR                  
015900              END-IF                                                      
016000           END-IF                                                         
016100           PERFORM E-VISA-BILD                                            
016200        END-IF                                                            
016300                                                                          
016400        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
016500        PERFORM IMS-INSERT-MSG                                            
016600     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT-SPARA-INPUT SECTION.                                              
017300     MOVE 'A-INIT-SPARA-INPUT      ' TO CURRENT-SECTION                   
017400                                                                          
017500     IF MSG-DUBBLA-TRANSKODER                                             
017600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11401               
017700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS  WS-IDTRANS                    
017800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
017900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
018000         MOVE MSG-IDPFK            TO MFS-IDPFK                           
018100     ELSE                                                                 
018200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11401                
018300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS  WS-IDTRANS                    
018400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
018500         MOVE ' ' TO MFS-KDTRTYP                                          
018600                     MFS-IDPFK                                            
018700     END-IF                                                               
018800                                                                          
018900     MOVE LOW-VALUE TO MSG-AREA                                           
019000     MOVE 'W2O11401' TO MFS-IDMOD                                         
019100     MOVE '2114' TO MOD-IDTRANS                                           
019200                                                                          
019300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
019400                             MOD-TEMFSINF                                 
019500                             MOD-IDLEVNR-START-IN                         
019510     MOVE SPACE           TO MOD-TEMFSFEL                                 
019600     IF MID-IDLEVNR-START-IN NOT = SPACE                                  
019700        MOVE MID-IDLEVNR-START-IN                                         
019800                             TO MOD-IDLEVNR-START-UT                      
019900     ELSE                                                                 
020000        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-START-UT                      
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 B-KOLLA-INPUT SECTION.                                                   
020410     MOVE 'B-KOLLA-INPUT           ' TO CURRENT-SECTION                   
020500                                                                          
020600     MOVE JA TO SW-INPUT-RAETT                                            
020700                                                                          
020800        IF MID-KDCMD-DELETE                                               
020900           SET MID-INFO-IND  TO  1                                        
021000           MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO                        
021100                                 WS-IDLEVNR-SPAR                          
021200           PERFORM UNTIL MID-INFO-IND = MAX-IND   OR                      
021300                   MID-IDLEVNR = MID-INFO-IDLEVNR                         
021400                                     (MID-INFO-IND)                       
021500              SET MID-INFO-IND UP BY 1                                    
021600              MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO                     
021700                                    WS-IDLEVNR-SPAR                       
021800           END-PERFORM                                                    
021900           IF MID-INFO-IND < MAX-IND                                      
022000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR            
022100           ELSE                                                           
022200              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-IN-ATTR            
022300              MOVE NEJ                  TO SW-INPUT-RAETT                 
022400           END-IF                                                         
022500        ELSE                                                              
022600           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDLEVNR-IN-ATTR            
022700        END-IF                                                            
022800        MOVE MFS-ROER-EJ-FAELT            TO MOD-IDLEVNR-IN               
022900                                                                          
023000     IF MID-KDEDI = ALL '+'                                               
023100        IF MID-KDCMD-INSERT                                               
023200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEDI-IN-ATTR                 
023300        END-IF                                                            
023400     ELSE                                                                 
023500        IF MID-KDEDI = 'V' OR 'O' OR 'T' OR 'F' OR 'E'                    
023600           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDEDI-IN-ATTR              
023700        ELSE                                                              
023800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDEDI-IN-ATTR                 
023900           MOVE NEJ                  TO SW-INPUT-RAETT                    
024000        END-IF                                                            
024100        MOVE MFS-ROER-EJ-FAELT TO MOD-KDEDI-IN                            
024200     END-IF                                                               
024300                                                                          
024400     IF MID-FLAVIS = ALL '+'                                              
024500        IF MID-KDCMD-INSERT                                               
024600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVIS-IN-ATTR                
024700        END-IF                                                            
024800     ELSE                                                                 
024900        IF MID-FLAVIS = 'J' OR 'N'                                        
025000           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLAVIS-IN-ATTR             
025100        ELSE                                                              
025200           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAVIS-IN-ATTR                
025300           MOVE NEJ                  TO SW-INPUT-RAETT                    
025400        END-IF                                                            
025500        MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVIS-IN                           
025600     END-IF                                                               
025700                                                                          
025800     IF MID-FLODETTE = ALL '+'                                            
025900        IF MID-KDCMD-INSERT                                               
026000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLODETTE-IN-ATTR              
026100        END-IF                                                            
026200     ELSE                                                                 
026300        IF MID-FLODETTE = 'J' OR 'N'                                      
026400           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLODETTE-IN-ATTR           
026500        ELSE                                                              
026600           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLODETTE-IN-ATTR              
026700           MOVE NEJ                  TO SW-INPUT-RAETT                    
026800        END-IF                                                            
026900        MOVE MFS-ROER-EJ-FAELT TO MOD-FLODETTE-IN                         
027000     END-IF                                                               
027100                                                                          
027200     IF MID-IDLEVKND = ALL '+'                                            
027300        IF MID-KDCMD-INSERT                                               
027400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-IN-ATTR              
027500        END-IF                                                            
027600     ELSE                                                                 
027700        IF MID-IDLEVKND NOT = SPACE                                       
027800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-IN-ATTR              
027900        ELSE                                                              
028000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVKND-IN-ATTR              
028100           MOVE NEJ                  TO SW-INPUT-RAETT                    
028200        END-IF                                                            
028300        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVKND-IN                   
028400     END-IF                                                               
028500                                                                          
028600     IF MID-KDCMD-INSERT OR MID-KDCMD-DELETE OR MID-KDCMD-REPLACE         
028700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                    
028800        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD-IN                         
028900     ELSE                                                                 
029000        IF MID-KDCMD = ALL '+'                                            
029100           MOVE MFS-RENSA-FAELT   TO MOD-KDCMD-IN                         
029200        ELSE                                                              
029300           MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                         
029400        END-IF                                                            
029500        MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR                    
029600        MOVE NEJ                  TO SW-INPUT-RAETT                       
029700     END-IF                                                               
029800                                                                          
029900     IF SW-INPUT-RAETT = NEJ                                              
030000        PERFORM S02-ROER-EJ-FAELT                                         
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500                                                                          
030600 C-KOLLA-INPUT-2 SECTION.                                                 
030610     MOVE 'C-KOLLA-INPUT-2         ' TO CURRENT-SECTION                   
030700                                                                          
030800     IF MID-KDCMD-INSERT                                                  
030900        IF MID-IDLEVNR       NOT = ALL '+' AND                            
031000           MID-FLODETTE      NOT = ALL '+' AND                            
031100           MID-IDLEVKND      NOT = ALL '+' AND                            
031200           MID-KDEDI         NOT = ALL '+' AND                            
031300           MID-FLAVIS        NOT = ALL '+'                                
031400                                                                          
031500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR               
031600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLODETTE-IN-ATTR              
031700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-IN-ATTR              
031800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEDI-IN-ATTR                 
031900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVIS-IN-ATTR                
032000                                                                          
032100           MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                    
032200           MOVE MFS-RENSA-FAELT      TO MOD-FLODETTE-IN                   
032300           MOVE MFS-RENSA-FAELT      TO MOD-IDLEVKND-IN                   
032400           MOVE MFS-RENSA-FAELT      TO MOD-KDEDI-IN                      
032500           MOVE MFS-RENSA-FAELT      TO MOD-FLAVIS-IN                     
032600        ELSE                                                              
032700           IF MID-IDLEVNR       NOT = ALL '+' AND                         
032800              MID-FLODETTE      NOT = ALL '+' AND                         
032900              MID-IDLEVKND          = ALL '+' AND                         
033000              MID-KDEDI             = ALL '+' AND                         
033100              MID-FLAVIS            = ALL '+'                             
033200                                                                          
033300                                                                          
033400              MOVE SPACE             TO MID-IDLEVKND                      
033500                                        MID-KDEDI                         
033600                                        MID-FLAVIS                        
033700                                                                          
033800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR            
033900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLODETTE-IN-ATTR           
034000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-IN-ATTR           
034100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEDI-IN-ATTR              
034200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVIS-IN-ATTR             
034300                                                                          
034400              MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-IN                 
034500              MOVE MFS-RENSA-FAELT      TO MOD-FLODETTE-IN                
034600              MOVE MFS-RENSA-FAELT      TO MOD-IDLEVKND-IN                
034700              MOVE MFS-RENSA-FAELT      TO MOD-KDEDI-IN                   
034800              MOVE MFS-RENSA-FAELT      TO MOD-FLAVIS-IN                  
034900           ELSE                                                           
035000              MOVE NEJ       TO SW-INPUT-RAETT                            
035100              IF MID-IDLEVNR           = ALL '+'                          
035200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR           
035300              END-IF                                                      
035400              IF MID-FLODETTE          = ALL '+'                          
035500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLODETTE-IN-ATTR          
035600              END-IF                                                      
035700              IF MID-IDLEVKND          = ALL '+'                          
035800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVKND-IN-ATTR          
035900              END-IF                                                      
036000              IF MID-KDEDI             = ALL '+'                          
036100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDEDI-IN-ATTR             
036200              END-IF                                                      
036300              IF MID-FLAVIS            = ALL '+'                          
036400                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAVIS-IN-ATTR            
036500              END-IF                                                      
036600              MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVNR-IN                
036700              MOVE MFS-ROER-EJ-FAELT     TO MOD-FLODETTE-IN               
036800              MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVKND-IN               
036900              MOVE MFS-ROER-EJ-FAELT     TO MOD-KDEDI-IN                  
037000              MOVE MFS-ROER-EJ-FAELT     TO MOD-FLAVIS-IN                 
037100           END-IF                                                         
037200        END-IF                                                            
037210        IF SW-INPUT-RAETT = NEJ                                           
037220           PERFORM S02-ROER-EJ-FAELT                                      
037230        END-IF                                                            
037300     ELSE                                                                 
037302        IF MID-KDCMD-DELETE                                               
037304           MOVE MID-IDLEVNR              TO W-IDLEVNR-WDD9                
037307           PERFORM IMS-GU-WDD902-ASEQ                                     
037308           PERFORM UNTIL SEGMENT-SAKNAS                                   
037309                      OR SEGMENT-SLUT                                     
037310                      OR SW-INPUT-RAETT = NEJ                             
037313              PERFORM IMS-GNP-WDD905                                      
037314              IF SEGMENT-FINNS                                            
037316                 MOVE FEL-5              TO MOD-TEMFSFEL                  
037317                 MOVE NEJ                TO SW-INPUT-RAETT                
037320              END-IF                                                      
037321              PERFORM IMS-GN-WDD902-ASEQ                                  
037322           END-PERFORM                                                    
037326        END-IF                                                            
037330     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000                                                                          
038100 D-UPPDATERA SECTION.                                                     
038110     MOVE ' D-UPPDATERA            ' TO CURRENT-SECTION                   
038200                                                                          
038300     IF MID-KDCMD-INSERT                                                  
038400        MOVE MID-IDLEVNR     TO XXBK-2216-IDLEVNR                         
038500        MOVE ZERO            TO XXBK-2216-IDOVERFNR                       
038600                                XXBK-2216-IDOVERFNR-VV                    
038700                                XXBK-2216-TISEND-BEG                      
038800                                XXBK-2216-TISEND-SEN                      
038900                                XXBK-2216-TISEND-PER                      
039000        MOVE 'D'             TO XXBK-2216-KDVECKOSL                       
039100        MOVE 'J'             TO XXBK-2216-FLLEVVB                         
039200        MOVE 'N'             TO XXBK-2216-FLLEVPLP                        
039300        MOVE MID-KDEDI       TO XXBK-2216-KDEDI                           
039400        MOVE MID-FLAVIS      TO XXBK-2216-FLAVIS                          
039500        MOVE MID-IDLEVKND    TO XXBK-2216-IDLEVKND                        
039600        MOVE MID-FLODETTE    TO XXBK-2216-FLODETTE                        
039700                                                                          
039800        PERFORM IMS-ISRT-WLXXBK11                                         
039900        IF SEGMENT-FINNS-REDAN                                            
040000           MOVE FEL-3        TO MOD-TEMFSFEL                              
040100           MOVE NEJ          TO SW-INPUT-RAETT                            
040200        ELSE                                                              
040300           MOVE MID-IDLEVNR  TO W-IDLEVNR                                 
040400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-INFO-IDLEVNR-ATTR            
040500           PERFORM S01-FYLL-BILD-LAES-XXBK11                              
040600        END-IF                                                            
040700     ELSE                                                                 
040800        MOVE MID-IDLEVNR             TO W-IDLEVNR                         
040900        PERFORM IMS-GET-WLXXBK11-KVAL-UNIK                                
041000        IF SEGMENT-FINNS                                                  
041100           IF MID-KDCMD-REPLACE                                           
041200              IF MID-IDLEVKND NOT  = ALL '+'                              
041300                 MOVE MID-IDLEVKND   TO XXBK-2216-IDLEVKND                
041400              END-IF                                                      
041500              IF MID-KDEDI     NOT = ALL '+'                              
041600                 MOVE MID-KDEDI      TO XXBK-2216-KDEDI                   
041700              END-IF                                                      
041800              IF MID-FLAVIS NOT = ALL '+'                                 
041900                 MOVE MID-FLAVIS     TO XXBK-2216-FLAVIS                  
042000              END-IF                                                      
042100              IF MID-FLODETTE NOT = ALL '+'                               
042200                 MOVE MID-FLODETTE   TO XXBK-2216-FLODETTE                
042300              END-IF                                                      
042400              PERFORM IMS-REPL-WLXXBK11                                   
042500           ELSE                                                           
042600              PERFORM IMS-DLET-WLXXBK11                                   
042700              MOVE MID-INFO-IDLEVNR (1) TO WS-IDLEVNR-SPAR                
042800              MOVE WS-IDLEVNR-SPAR      TO W-IDLEVNR                      
042900           END-IF                                                         
043000           PERFORM IMS-GET-WLXXBK11-KVAL-FIRST                            
043100           PERFORM S01-FYLL-BILD-LAES-XXBK11                              
043200        ELSE                                                              
043300           MOVE FEL-4                TO MOD-TEMFSFEL                      
043400           MOVE NEJ                  TO SW-INPUT-RAETT                    
043500        END-IF                                                            
043600     END-IF                                                               
043700                                                                          
043800     IF SW-INPUT-RAETT = JA                                               
043900        MOVE MFS-RENSA-FAELT            TO MOD-IDLEVNR-IN                 
044000                                           MOD-KDEDI-IN                   
044100                                           MOD-FLAVIS-IN                  
044200                                           MOD-IDLEVKND-IN                
044300                                           MOD-FLODETTE-IN                
044400                                           MOD-KDCMD-IN                   
044500                                                                          
044600*       MOVE MFS-FORMATETS-ATTR         TO MOD-IDLEVNR-IN-ATTR            
044700*                                          MOD-KDEDI-IN-ATTR              
044800*                                          MOD-FLAVIS-IN-ATTR             
044900*                                          MOD-IDLEVKND-IN-ATTR           
045000*                                          MOD-FLODETTE-IN-ATTR           
045100*                                          MOD-KDCMD-IN-ATTR              
045200        MOVE MED-3                      TO MOD-TEMFSINF                   
045300     ELSE                                                                 
045400        PERFORM S02-ROER-EJ-FAELT                                         
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 E-VISA-BILD SECTION.                                                     
045810     MOVE ' E-VISA-BILD            ' TO CURRENT-SECTION                   
045900                                                                          
046000     IF MFS-IDPFK = '8'                                                   
046100        PERFORM IMS-GET-WLXXBK11                                          
046200     ELSE                                                                 
046300        PERFORM IMS-GET-WLXXBK11-KVAL                                     
046400     END-IF                                                               
046500                                                                          
046600     PERFORM S01-FYLL-BILD-LAES-XXBK11                                    
046700                                                                          
046800     MOVE MFS-ROER-EJ-FAELT         TO MOD-IDLEVNR-IN                     
046900                                       MOD-KDEDI-IN                       
047000                                       MOD-FLAVIS-IN                      
047100                                       MOD-IDLEVKND-IN                    
047200                                       MOD-FLODETTE-IN                    
047300                                       MOD-KDCMD-IN                       
047400                                                                          
047500     IF MID-IDLEVNR    = ALL '+'  AND                                     
047600        MID-KDEDI      = ALL '+'  AND                                     
047700        MID-FLAVIS     = ALL '+'  AND                                     
047800        MID-IDLEVKND   = ALL '+'  AND                                     
047900        MID-FLODETTE   = ALL '+'  AND                                     
048000        MID-KDCMD      = ALL '+'                                          
048100*       MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-IN-ATTR                
048200        MOVE MFS-FORMATETS-ATTR     TO MOD-IDLEVNR-IN-ATTR                
048300                                       MOD-KDEDI-IN-ATTR                  
048400                                       MOD-FLAVIS-IN-ATTR                 
048500                                       MOD-IDLEVKND-IN-ATTR               
048600                                       MOD-FLODETTE-IN-ATTR               
048700                                       MOD-KDCMD-IN-ATTR                  
048800     ELSE                                                                 
048900        MOVE MED-2                  TO MOD-TEMFSINF                       
049000        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDLEVNR-IN-ATTR                
049100                                       MOD-KDEDI-IN-ATTR                  
049200                                       MOD-FLAVIS-IN-ATTR                 
049300                                       MOD-IDLEVKND-IN-ATTR               
049400                                       MOD-FLODETTE-IN-ATTR               
049500                                       MOD-KDCMD-IN-ATTR                  
049600     END-IF                                                               
049700                                                                          
049800     .                                                                    
049900     EJECT                                                                
050000 S01-FYLL-BILD-LAES-XXBK11 SECTION.                                       
050010     MOVE 'S01-FYLL-BILD-LAES-XXBK11' TO CURRENT-SECTION                  
050100                                                                          
050200     IF SEGMENT-SAKNAS                                                    
050300        MOVE SPACE                TO W-IDLEVNR                            
050400        PERFORM IMS-GET-WLXXBK11-KVAL-FIRST                               
050500     END-IF                                                               
050600                                                                          
050700     SET MOD-INFO-IND TO 1                                                
050800                                                                          
050900     PERFORM UNTIL MOD-INFO-IND = MAX-IND                                 
051000        IF SEGMENT-FINNS                                                  
051100           MOVE XXBK-2216-IDLEVNR  TO MOD-INFO-IDLEVNR                    
051200                                      (MOD-INFO-IND)                      
051300           MOVE XXBK-2216-KDEDI    TO MOD-INFO-KDEDI                      
051400                                      (MOD-INFO-IND)                      
051500           MOVE XXBK-2216-FLAVIS TO MOD-INFO-FLAVIS                       
051600                                      (MOD-INFO-IND)                      
051700           MOVE XXBK-2216-IDLEVKND TO MOD-INFO-IDLEVKND                   
051800                                      (MOD-INFO-IND)                      
051900           MOVE XXBK-2216-FLODETTE TO MOD-INFO-FLODETTE                   
052000                                      (MOD-INFO-IND)                      
052100           PERFORM IMS-GET-WLXXBK11-KVAL                                  
052200        ELSE                                                              
052300           MOVE MFS-RENSA-FAELT TO MOD-INFO-IDLEVNR                       
052400                                     (MOD-INFO-IND)                       
052500                                   MOD-INFO-KDEDI                         
052600                                     (MOD-INFO-IND)                       
052700                                   MOD-INFO-FLAVIS                        
052800                                     (MOD-INFO-IND)                       
052900                                   MOD-INFO-IDLEVKND                      
053000                                     (MOD-INFO-IND)                       
053100                                   MOD-INFO-FLODETTE                      
053200                                     (MOD-INFO-IND)                       
053300        END-IF                                                            
053400                                                                          
053500        SET MOD-INFO-IND UP BY 1                                          
053600     END-PERFORM                                                          
053700                                                                          
053800     IF SEGMENT-FINNS                                                     
053900        MOVE MED-1              TO MOD-TEMFSINF                           
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 S02-ROER-EJ-FAELT SECTION.                                               
054310     MOVE 'S02-ROER-EJ-FAELT        ' TO CURRENT-SECTION                  
054400     SKIP3                                                                
054500     SET MOD-INFO-IND                TO 1                                 
054600                                                                          
054700     PERFORM UNTIL MOD-INFO-IND = MAX-IND                                 
054800        MOVE MFS-ROER-EJ-FAELT  TO MOD-INFO-IDLEVNR                       
054900                                   (MOD-INFO-IND)                         
055000                                   MOD-INFO-KDEDI                         
055100                                   (MOD-INFO-IND)                         
055200                                   MOD-INFO-FLAVIS                        
055300                                   (MOD-INFO-IND)                         
055400                                   MOD-INFO-IDLEVKND                      
055500                                   (MOD-INFO-IND)                         
055600                                   MOD-INFO-FLODETTE                      
055700                                   (MOD-INFO-IND)                         
055800        SET MOD-INFO-IND UP BY 1                                          
055900     END-PERFORM                                                          
056000                                                                          
056100     IF MID-IDLEVNR = ALL '+'                                             
056200        MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-IN                          
056300     ELSE                                                                 
056400        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-IN                          
056500     END-IF                                                               
056600     IF MID-KDEDI   = ALL '+'                                             
056700        MOVE MFS-RENSA-FAELT   TO MOD-KDEDI-IN                            
056800     ELSE                                                                 
056900        MOVE MFS-ROER-EJ-FAELT TO MOD-KDEDI-IN                            
057000     END-IF                                                               
057100     IF MID-FLAVIS      = ALL '+'                                         
057200        MOVE MFS-RENSA-FAELT   TO MOD-FLAVIS-IN                           
057300     ELSE                                                                 
057400        MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVIS-IN                           
057500     END-IF                                                               
057600     IF MID-IDLEVKND = ALL '+'                                            
057700        MOVE MFS-RENSA-FAELT   TO MOD-IDLEVKND-IN                         
057800     ELSE                                                                 
057900        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKND-IN                         
058000     END-IF                                                               
058100     IF MID-FLODETTE    = ALL '+'                                         
058200        MOVE MFS-RENSA-FAELT   TO MOD-FLODETTE-IN                         
058300     ELSE                                                                 
058400        MOVE MFS-ROER-EJ-FAELT TO MOD-FLODETTE-IN                         
058500     END-IF                                                               
058600     IF MID-KDCMD   = ALL '+'                                             
058700        MOVE MFS-RENSA-FAELT   TO MOD-KDCMD-IN                            
058800     ELSE                                                                 
058900        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                            
059000     END-IF                                                               
059100     EJECT                                                                
059200* IMS SEKTIONER                                                           
059300     SKIP3                                                                
059400     .                                                                    
059500 IMS-GET-MSG SECTION.                                                     
059600     SKIP2                                                                
059700     MOVE '  QC' TO GODK-STATUSKODER                                      
059800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
059900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060000     PERFORM IMS-STATUS-KONTROLL                                          
060100     SKIP3                                                                
060200     .                                                                    
060300 IMS-INSERT-MSG SECTION.                                                  
060400     SKIP2                                                                
060500     IF ENGLISH-TEXT                                                      
060600        MOVE 'N' TO MFS-KDHUVOMR                                          
060700     END-IF                                                               
060800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
060900     MOVE SPACE TO GODK-STATUSKODER                                       
061000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
061100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061200     PERFORM IMS-STATUS-KONTROLL                                          
061300     .                                                                    
061400     EJECT                                                                
061500 IMS-GET-WLXXBK01 SECTION.                                                
061510     MOVE 'IMS-GET-WLXXBK01            ' TO DBS-SECTION                   
061600                                                                          
061700     STRING 'WLXXBK01(WDGXKEY  =' W-WDGXKEY-ROT ')'                       
061800             DELIMITED BY SIZE INTO SSA1                                  
061900     MOVE '  ' TO GODK-STATUSKODER                                        
062000     CALL CBLTDLI USING GU WLXXBK-PCB DLI-IO-AREA SSA1                    
062100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
062200     PERFORM IMS-STATUS-KONTROLL                                          
062300     SKIP3                                                                
062400     .                                                                    
062500 IMS-GET-WLXXBK11-KVAL SECTION.                                           
062510     MOVE 'IMS-GET-WLXXBK11-KVAL       ' TO DBS-SECTION                   
062600                                                                          
062700     STRING 'WLXXBK11(IDLEVNR =>' W-WDGXKEY-IDLEVNR ')'                   
062800            DELIMITED BY SIZE INTO SSA1                                   
062900     MOVE '  GE' TO GODK-STATUSKODER                                      
063000     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
063100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
063200     PERFORM IMS-STATUS-KONTROLL                                          
063300     SKIP3                                                                
063400     .                                                                    
063500 IMS-GET-WLXXBK11-KVAL-UNIK SECTION.                                      
063510     MOVE 'IMS-GET-WLXXBK11-KVAL-UNIK  ' TO DBS-SECTION                   
063600                                                                          
063700     STRING 'WLXXBK11(IDLEVNR  =' W-WDGXKEY-IDLEVNR ')'                   
063800            DELIMITED BY SIZE INTO SSA1                                   
063900     MOVE '  GE' TO GODK-STATUSKODER                                      
064000     CALL CBLTDLI USING GHNP WLXXBK-PCB DLI-IO-AREA SSA1                  
064100     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
064200     PERFORM IMS-STATUS-KONTROLL                                          
064300     SKIP3                                                                
064400     .                                                                    
064500     EJECT                                                                
064600 IMS-GET-WLXXBK11-KVAL-FIRST SECTION.                                     
064610     MOVE 'IMS-GET-WLXXBK11-KVAL-FIRST ' TO DBS-SECTION                   
064700                                                                          
064800     STRING 'WLXXBK11*F(IDLEVNR =>' W-WDGXKEY-IDLEVNR ')'                 
064900             DELIMITED BY SIZE INTO SSA1                                  
065000     MOVE '  GE' TO GODK-STATUSKODER                                      
065100     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
065200     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
065300     PERFORM IMS-STATUS-KONTROLL                                          
065400     SKIP3                                                                
065500     .                                                                    
065600     SKIP3                                                                
065700 IMS-GET-WLXXBK11 SECTION.                                                
065710     MOVE 'IMS-GET-WLXXBK11            ' TO DBS-SECTION                   
065800                                                                          
065900     STRING 'WLXXBK11(IDLEVNR  >' W-WDGXKEY-IDLEVNR ')'                   
066000            DELIMITED BY SIZE INTO SSA1                                   
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GNP WLXXBK-PCB DLI-IO-AREA SSA1                   
066300     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
066400     PERFORM IMS-STATUS-KONTROLL                                          
066500     .                                                                    
066600     EJECT                                                                
066700 IMS-ISRT-WLXXBK11 SECTION.                                               
066710     MOVE 'IMS-ISRT-WLXXBK11           ' TO DBS-SECTION                   
066800                                                                          
066900     STRING 'WLXXBK01*P(WDGXKEY  =' W-WDGXKEY-ROT ')'                     
067000             DELIMITED BY SIZE INTO SSA1                                  
067100     MOVE 'WLXXBK11 '  TO SSA2                                            
067200     MOVE '  II' TO GODK-STATUSKODER                                      
067300     CALL CBLTDLI USING ISRT WLXXBK-PCB DLI-IO-AREA SSA1 SSA2             
067400     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
067500     PERFORM IMS-STATUS-KONTROLL                                          
067600     SKIP3                                                                
067700     .                                                                    
067800 IMS-REPL-WLXXBK11 SECTION.                                               
067810     MOVE 'IMS-REPL-WLXXBK11           ' TO DBS-SECTION                   
067900                                                                          
068000     MOVE '  '   TO GODK-STATUSKODER                                      
068100     CALL CBLTDLI USING REPL WLXXBK-PCB DLI-IO-AREA                       
068200     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
068300     PERFORM IMS-STATUS-KONTROLL                                          
068400     SKIP3                                                                
068500     .                                                                    
068600 IMS-DLET-WLXXBK11 SECTION.                                               
068610     MOVE 'IMS-DLET-WLXXBK11           ' TO DBS-SECTION                   
068700                                                                          
068800     MOVE '  '   TO GODK-STATUSKODER                                      
068900     CALL CBLTDLI USING DLET WLXXBK-PCB DLI-IO-AREA                       
069000     MOVE WLXXBK-STATUS-CODE TO STATUS-WS                                 
069100     PERFORM IMS-STATUS-KONTROLL                                          
069200     EJECT                                                                
069300     SKIP3                                                                
069400     .                                                                    
069410 IMS-GU-WDD902-ASEQ            SECTION.                                   
069411     MOVE 'IMS-GU-WDD902-ASEQ          ' TO DBS-SECTION                   
069420                                                                          
069430     STRING 'WDD902  (WDD9ASEQ =' W-IDLEVNR-X ')'                         
069440            DELIMITED BY SIZE INTO SSA1                                   
069450     MOVE '  GE'                TO GODK-STATUSKODER                       
069460     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1                    
069470     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
069471     PERFORM IMS-STATUS-KONTROLL                                          
069490     .                                                                    
069491                                                                          
069492 IMS-GN-WDD902-ASEQ           SECTION.                                    
069493     MOVE 'IMS-GN-WDD902-ASEQ         ' TO DBS-SECTION                    
069494                                                                          
069495     STRING 'WDD902  (WDD9ASEQ =' W-IDLEVNR-X ')'                         
069496            DELIMITED BY SIZE INTO SSA1                                   
069497     MOVE '  GE'                TO GODK-STATUSKODER                       
069498     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-WDD902 SSA1                    
069499     MOVE WDD9-STATUS-CODE      TO STATUS-WS                              
069500     PERFORM IMS-STATUS-KONTROLL                                          
069501     .                                                                    
069502                                                                          
069503 IMS-GNP-WDD905 SECTION.                                                  
069504                                                                          
069505     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
069506                            DELIMITED BY SIZE INTO SSA1                   
069507     MOVE '  GE'           TO GODK-STATUSKODER                            
069508     CALL CBLTDLI       USING GNP WDD9-PCB DLI-IO-WDD905 SSA1             
069509     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
069510     PERFORM IMS-STATUS-KONTROLL                                          
069511     .                                                                    
069512     EJECT                                                                
069570 IMS-STATUS-KONTROLL SECTION.                                             
069600     SET STATUS-IX TO 1                                                   
069700     SEARCH GODK-STATUS AT END CALL FELLOG                                
069800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
069900     END-SEARCH                                                           
070000     CONTINUE                                                             
070100     .                                                                    
