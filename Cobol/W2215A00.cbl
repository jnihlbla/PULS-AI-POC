000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2215A00.                                                
000301 AUTHOR.         OLSSON SUSANNE.                                          
000401 DATE-WRITTEN.   19/07/22.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801*    FUNKTION:                                                            
000901*        UPPDATERING OMSPEC LEVERANSPLAN                                  
001001*        FIL W2215A FRÅN W2215000                                         
001101*                                                                         
001201*        PROGRAMMET UPPDATERAR WDK6                                       
001301*        PROGRAMMET UPPDATERAR WDD9                                       
001401*        PROGRAMMET UPPDATERAR WDD6                                       
001501*        PROGRAMMET UPPDATERAR WDR4  ÅTERSTARTSREGISTER                   
001601*                                    HTYP=4579, SEGMENT=WDGX4580          
001701*                                                                         
001800*        PROGRAMMET FLYTTAR AVROP VIA SUB PGM W221BLOC                    
001901*                   UPPDATERAR WDD9                                       
002001*                   WDR5 (WDGX2224) H-TYP 2223                            
002101*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- UPPDATERINGSPOSTER FRÅN PGM W2215000                       
003100     SELECT W2215A                     ASSIGN TO W2215AD1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W2215A                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W2215A      -L.                                                
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W2215A00'.            
004600 01  CHKP-VAR.                                                            
004700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005100     03 CHKP-ANT                 PIC S9(5)   VALUE +0   COMP-3.           
005200     03 CHKP-MAX                 PIC S9(3)   VALUE +900 COMP-3.           
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005601 77  BLOC-TAB-IX                 PIC S9(4)   VALUE +0   COMP SYNC.        
005701 77  BLOC-MAX-IX                 PIC S9(4)   VALUE +60  COMP SYNC.        
005800                                                                          
005900 01  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
006000 01  CURRENT-IMS-SECTION         PIC X(32)   VALUE SPACE.                 
006101 01  W-SPAR-IDARTNR              PIC S9(9)   VALUE +0   COMP-3.           
006200                                                                          
006300 01  WC-IDPTYP.                                                           
006400     03  BORTTAG-OMSPEC          PIC X(3)    VALUE '001'.                 
006500     03  BORTTAG-FORSLAG         PIC X(3)    VALUE '002'.                 
006600     03  UPDATE-WDK611           PIC X(3)    VALUE '003'.                 
006700     03  NYUPPL-OMSPEC           PIC X(3)    VALUE '004'.                 
006800     03  UPPDAT-OMSPEC           PIC X(3)    VALUE '005'.                 
006900     03  NYUPPL-LEV              PIC X(3)    VALUE '006'.                 
007000     03  NYUPPL-DAG-AVROP        PIC X(3)    VALUE '007'.                 
007100     03  NYUPPL-AVROP            PIC X(3)    VALUE '008'.                 
007201     03  FLYTTA-BLOC-AVROP       PIC X(3)    VALUE '009'.                 
007300                                                                          
007400     SKIP2                                                                
007500*    --- PARAMETRAR TILL ABEND                                            
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400                                                                          
008500 77  W-KVPOST-IN                 PIC S9(7)   VALUE +0  COMP-3.            
008600                                                                          
008700 77  W2215A-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-W2215A                       VALUE 'J'.                   
008900     EJECT                                                                
009000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM.                                       
009200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009501                                                                          
009600     EJECT                                                                
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800*                                                                         
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010401     03  W221BLOC                PIC X(8)    VALUE 'W221BLOC'.            
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL DATKORT                                          
010700*                                                                         
010800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2215A'.              
010900     SKIP2                                                                
011000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011100     SKIP2                                                                
011200*01  -COPY WDATKORT                                                       
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL POSTSUM                                          
011500*                                                                         
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011801*    --- LÄNKAREA TILL SUBPROGRAM W221BLOC                                
011901*                                                                         
012001*01  -COPY W221BLOC                                                       
012101     EJECT                                                                
012200 01  IN-AREA-START               PIC X(24)   VALUE                        
012300                                             'IN-AREA-START'.             
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W2215A     -PRE IN-                                       
012700*                                                                         
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013000     SKIP3                                                                
013100 01  NYCKLAR-TILL-DLI.                                                    
013200     03  W-IDARTNR-X.                                                     
013300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013400                                                                          
013500     03  W-WDD901KY-X.                                                    
013600         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
013700         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
013800                                                                          
013900     03  W-IDLEVNR-X.                                                     
014000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
014100                                                                          
014200     03  W-KDAVROP-X.                                                     
014300         05  W-KDAVROP           PIC S9      VALUE ZERO COMP-3.           
014400                                                                          
014500     03  W-WDD905KY-X.                                                    
014600         05  W-DAAVROP-AVS       PIC 9(6)    VALUE ZERO.                  
014700         05  W-TILEVDAG          PIC S9      VALUE ZERO COMP-3.           
014800                                                                          
014900     03  W-WDD601KY-MIN-X.                                                
015000         05 W-IDDC-MIN           PIC X(2)  VALUE SPACE.                   
015100         05 W-IDLEVNR-MIN        PIC X(5)  VALUE SPACE.                   
015200         05 W-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.             
015301         05 FILLER               PIC X(2)  VALUE LOW-VALUE.               
015401                                                                          
015501                                                                          
015600     03  W-WDD601KY-MAX-X.                                                
015700         05 W-IDDC-MAX           PIC X(2)  VALUE SPACE.                   
015800         05 W-IDLEVNR-MAX        PIC X(5)  VALUE SPACE.                   
015900         05 W-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.             
016001         05 FILLER               PIC X(2)  VALUE HIGH-VALUE.              
016101                                                                          
016200                                                                          
016300     03  W-WDGXKEY-4579-X.                                                
016400         05  W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
016500         05  W-IDPGM             PIC X(8)    VALUE 'W2215A00'.            
016600         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
016700                                                                          
016800     SKIP2                                                                
016900*    --- STATUS-KOD FRÅN IMS                                              
017000 01  STATUS-WS                   PIC XX.                                  
017100     88  SEGMENT-FINNS                       VALUE '  '.                  
017200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017500     88  IMS-EJ-OK                           VALUE 'XD'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018001 01  ALL-SSA.                                                             
018100     03  SSA1                        PIC X(64).                           
018200     03  SSA2                        PIC X(64).                           
018301     03  SSA3                        PIC X(64).                           
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900                                                                          
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019100 01  DLI-IO-WDK601.                                                       
019200*    03  -COPY WDK601                                                     
019300     EJECT                                                                
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
019500 01  DLI-IO-WDK611.                                                       
019600*    03  -COPY WDK611                                                     
019700     EJECT                                                                
019800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
019900 01  DLI-IO-WDD901.                                                       
020000*    03  -COPY WDD901   -PRE D901-                                        
020100     EJECT                                                                
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
020300 01  DLI-IO-WDD902.                                                       
020400*    03  -COPY WDD902   -PRE D902-                                        
020500     EJECT                                                                
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
020700 01  DLI-IO-WDD904.                                                       
020800*    03  -COPY WDD904   -PRE D904-                                        
020900     EJECT                                                                
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
021100 01  DLI-IO-WDD905.                                                       
021200*    03  -COPY WDD905   -PRE D905-                                        
021300     SKIP2                                                                
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
021500 01  DLI-IO-WDD601.                                                       
021600*    03  -COPY WDD601                                                     
021700     EJECT                                                                
021800*-ÅTERSTARTSREGISTER WDR4                                                 
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
022000 01  DLI-IO-WDGX4580.                                                     
022100*    03  -COPY WDGX4580                                                   
022200                                                                          
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009   -PRE MSG-                                              
022700                                                                          
022800*01  -COPY W0008  -PRE WDK6-                                              
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008  -PRE WDD9-                                              
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008  -PRE WDD6-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700*01  -COPY W0008  -PRE 4579-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024001 01  BLOC-WDD9-PCB               PIC X.                                   
024100 01  BLOC-WDF3-PCB               PIC X.                                   
024201 01  BLOC-WDR2-PCB               PIC X.                                   
024301 01  BLOC-WDR5-PCB               PIC X.                                   
024401                                                                          
024500     EJECT                                                                
024600 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDD9-PCB                      
024700                           WDD6-PCB 4579-PCB                              
024801                           BLOC-WDD9-PCB                                  
024901                           BLOC-WDF3-PCB                                  
025001                           BLOC-WDR2-PCB                                  
025101                           BLOC-WDR5-PCB.                                 
025201                                                                          
025300 MAIN SECTION.                                                            
025400     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDD9-PCB                      
025500                           WDD6-PCB 4579-PCB                              
025601                           BLOC-WDD9-PCB                                  
025701                           BLOC-WDF3-PCB                                  
025801                           BLOC-WDR2-PCB                                  
025901                           BLOC-WDR5-PCB.                                 
026000                                                                          
026100     SKIP2                                                                
026200     PERFORM A-INIT                                                       
026300                                                                          
026400     PERFORM IMS-GHU-RESTART                                              
026500                                                                          
026600     IF 4580-KVPOST > +0                                                  
026700       PERFORM S11-LAS-FRAM-TILL-CHKPOINT                                 
026800     ELSE                                                                 
026900       PERFORM S01-LAES-W2215A                                            
027000     END-IF                                                               
027101                                                                          
027200     PERFORM UNTIL END-OF-W2215A                                          
027300       IF CHKP-ANT > CHKP-MAX                                             
027400         PERFORM X-TAG-CHECKPOINT                                         
027500       END-IF                                                             
027600                                                                          
027701       IF IN-UPLP-IDARTNR NOT = W-SPAR-IDARTNR                            
027801                                                                          
027901         IF BLOC-TAB-IX > 0                                               
028102           CALL W221BLOC USING BLOC-W221BLOC                              
028202                                    BLOC-WDD9-PCB                         
028302                                    BLOC-WDF3-PCB                         
028402                                    BLOC-WDR2-PCB                         
028502                                    BLOC-WDR5-PCB                         
028704         END-IF                                                           
028705                                                                          
028802         MOVE IN-UPLP-IDARTNR  TO W-SPAR-IDARTNR                          
028902         INITIALIZE  BLOC-W221BLOC                                        
029002         MOVE IDPGM TO BLOC-IDPGM                                         
029102         MOVE +0    TO BLOC-TAB-IX                                        
029202                                                                          
029302       END-IF                                                             
029402                                                                          
029502       EVALUATE IN-UPLP-IDPTYP                                            
029602          WHEN BORTTAG-OMSPEC                                             
029702               PERFORM B-BORTTAG-OMSPEC                                   
029802                                                                          
029902          WHEN BORTTAG-FORSLAG                                            
030002               PERFORM C-BORTTAG-FORSLAG                                  
030102                                                                          
030202          WHEN UPDATE-WDK611                                              
030302               PERFORM D-UPDATE-WDK611                                    
030402                                                                          
030502          WHEN NYUPPL-OMSPEC                                              
030602               PERFORM E-NYUPPL-OMSPEC                                    
030702                                                                          
030802          WHEN UPPDAT-OMSPEC                                              
030902               PERFORM F-UPPDAT-OMSPEC                                    
031002                                                                          
031102          WHEN NYUPPL-LEV                                                 
031202               PERFORM G-NYUPPL-LEV                                       
031302                                                                          
031402          WHEN NYUPPL-DAG-AVROP                                           
031502               PERFORM H-NYUPPL-AVROP                                     
031602                                                                          
031702          WHEN NYUPPL-AVROP                                               
031802               PERFORM H-NYUPPL-AVROP                                     
031902                                                                          
032002          WHEN FLYTTA-BLOC-AVROP                                          
032102               IF BLOC-TAB-IX = 0                                         
032202                 PERFORM J-BEHANDLA-BLOC-AVROP                            
032302               END-IF                                                     
032402                                                                          
032502               PERFORM I-FLYTTA-BLOC-AVROP                                
032602                                                                          
032702       END-EVALUATE                                                       
032802                                                                          
032902       PERFORM S01-LAES-W2215A                                            
033002     END-PERFORM                                                          
033102                                                                          
033202     IF END-OF-W2215A                                                     
033303        IF BLOC-TAB-IX > 0                                                
033403          CALL W221BLOC USING BLOC-W221BLOC                               
033503                                   BLOC-WDD9-PCB                          
033603                                   BLOC-WDF3-PCB                          
033703                                   BLOC-WDR2-PCB                          
033803                                   BLOC-WDR5-PCB                          
033903        END-IF                                                            
034003     END-IF                                                               
034103                                                                          
034203                                                                          
034303     PERFORM Z-FINIT                                                      
034403                                                                          
034503     MOVE ZERO TO RETURN-CODE                                             
034603     GOBACK                                                               
034703     .                                                                    
034803     EJECT                                                                
034903 A-INIT SECTION.                                                          
035003     SKIP2                                                                
035103                                                                          
035203     OPEN INPUT W2215A                                                    
035303                                                                          
035403     PERFORM IMS-RESTART                                                  
035503                                                                          
035603     MOVE +0          TO CHKP-ANT                                         
035703                         W-KVPOST-IN                                      
035803                         W-SPAR-IDARTNR                                   
035903                         BLOC-TAB-IX                                      
036003                                                                          
036103     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036203     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
036303     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
036403     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
036503     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
036603                                                                          
036703                                                                          
036803     .                                                                    
036903     EJECT                                                                
037003 B-BORTTAG-OMSPEC SECTION.                                                
037103     MOVE 'B-BORTTAG-OMSPEC' TO CURRENT-SECTION                           
037203                                                                          
037303     MOVE IN-UPLP-IDARTNR     TO W-IDARTNR-D9                             
037403     MOVE IN-UPLP-IDDC        TO W-IDDC-D9                                
037503     MOVE IN-UPLP-IDLEVNR     TO W-IDLEVNR                                
037603                                                                          
037703     PERFORM IMS-GHU-WDD904                                               
037803     IF SEGMENT-FINNS                                                     
037903       PERFORM IMS-DLET-WDD904                                            
038003     END-IF                                                               
038103     .                                                                    
038203     EJECT                                                                
038303 C-BORTTAG-FORSLAG SECTION.                                               
038403     MOVE 'C-BORTTAG-FORSLAG ' TO CURRENT-SECTION                         
038503                                                                          
038603     MOVE IN-UPLP-IDARTNR    TO W-IDARTNR-D9                              
038703     MOVE IN-UPLP-IDDC       TO W-IDDC-D9                                 
038803                                                                          
038903     PERFORM IMS-GU-WDD901                                                
039003     IF SEGMENT-FINNS                                                     
039103        MOVE IN-UPLP-KDAVROP TO W-KDAVROP                                 
039203        PERFORM IMS-GHNP-WDD905                                           
039303        PERFORM UNTIL SEGMENT-SAKNAS                                      
039403           PERFORM IMS-DLET-WDD905                                        
039503           PERFORM IMS-GHNP-WDD905                                        
039603        END-PERFORM                                                       
039703     END-IF                                                               
039803                                                                          
039903*    -- TAG ÄVEN BORT ARTIKELN FRÅN FÖRSLAGS-KÖN                          
040003     MOVE IN-UPLP-IDDC       TO W-IDDC-MIN                                
040103                                W-IDDC-MAX                                
040203     MOVE IN-UPLP-IDLEVNR    TO W-IDLEVNR-MIN                             
040303                                W-IDLEVNR-MAX                             
040403     MOVE IN-UPLP-IDARTNR    TO W-IDARTNR-MIN                             
040503                                W-IDARTNR-MAX                             
040603                                                                          
040703     PERFORM IMS-GHU-WDD601                                               
040803     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
040903        PERFORM IMS-DLET-WDD601                                           
041003        PERFORM IMS-GHN-WDD601                                            
041103     END-PERFORM                                                          
041203     .                                                                    
041303     EJECT                                                                
041403 D-UPDATE-WDK611   SECTION.                                               
041503     MOVE 'D-UPDATE-WDK611 ' TO CURRENT-SECTION                           
041603                                                                          
041703     MOVE IN-UPLP-IDARTNR    TO W-IDARTNR                                 
041803                                                                          
041903     PERFORM IMS-GHU-WDK611                                               
042003     IF SEGMENT-FINNS                                                     
042103        MOVE IN-UPLP-KDLEVPLF    TO CLAG-KDLEVPLF                         
042203        MOVE IN-UPLP-KDLPSP      TO CLAG-KDLPSP                           
042303        MOVE IN-UPLP-TILPSP      TO CLAG-TILPSP                           
042403        MOVE IN-UPLP-TIOMSPEC    TO CLAG-TIOMSPEC                         
042503        MOVE IN-UPLP-FLSKROT-WLC TO CLAG-FLSKROT-WLC                      
042603                                                                          
042703        PERFORM IMS-REPL-WDK611                                           
042803     END-IF                                                               
042903     .                                                                    
043003     EJECT                                                                
043103 E-NYUPPL-OMSPEC   SECTION.                                               
043203     MOVE 'E-NYUPPL-OMSPEC ' TO CURRENT-SECTION                           
043303                                                                          
043403     MOVE IN-UPLP-IDARTNR      TO W-IDARTNR-D9                            
043503     MOVE IN-UPLP-IDDC         TO W-IDDC-D9                               
043603                                                                          
043703     PERFORM IMS-GU-WDD901                                                
043803     IF SEGMENT-SAKNAS                                                    
043903        MOVE IN-UPLP-IDARTNR   TO D901-IDARTNR                            
044003        MOVE IN-UPLP-IDDC      TO D901-IDDC                               
044103        PERFORM IMS-ISRT-WDD901                                           
044203                                                                          
044303        MOVE SPACE             TO D902-WDD902                             
044403        MOVE IN-UPLP-IDLEVNR   TO D902-IDLEVNR                            
044503        MOVE ZERO              TO D902-KVBR                               
044603        MOVE ZERO              TO D902-TILEVPL                            
044703        PERFORM IMS-ISRT-WDD902                                           
044803     ELSE                                                                 
044903                                                                          
045003       MOVE IN-UPLP-IDLEVNR      TO W-IDLEVNR                             
045103                                                                          
045203       PERFORM IMS-GNP-WDD902                                             
045303       IF SEGMENT-SAKNAS                                                  
045403          MOVE SPACE             TO D902-WDD902                           
045503          MOVE IN-UPLP-IDLEVNR   TO D902-IDLEVNR                          
045603          MOVE ZERO              TO D902-KVBR                             
045703          MOVE ZERO              TO D902-TILEVPL                          
045803          PERFORM IMS-ISRT-WDD902                                         
045903       END-IF                                                             
046003     END-IF                                                               
046103                                                                          
046203     MOVE IN-UPLP-DASPECST        TO D904-DASPECST                        
046303     MOVE IN-UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)                 
046403     MOVE IN-UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)                 
046503     MOVE IN-UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)                 
046603     MOVE IN-UPLP-KVBEST-PL       TO D904-KVBEST-PL                       
046703     MOVE IN-UPLP-KDPLKOEP        TO D904-KDPLKOEP                        
046803                                                                          
046903     PERFORM IMS-ISRT-WDD904                                              
047003                                                                          
047103     .                                                                    
047203     EJECT                                                                
047303 F-UPPDAT-OMSPEC   SECTION.                                               
047403     MOVE 'F-UPPDAT-OMSPEC ' TO CURRENT-SECTION                           
047503                                                                          
047603     MOVE IN-UPLP-IDARTNR    TO W-IDARTNR-D9                              
047703     MOVE IN-UPLP-IDDC       TO W-IDDC-D9                                 
047803     MOVE IN-UPLP-IDLEVNR    TO W-IDLEVNR                                 
047903                                                                          
048003     PERFORM IMS-GHU-WDD904                                               
048103                                                                          
048203     IF SEGMENT-FINNS                                                     
048303        MOVE IN-UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)              
048403        MOVE IN-UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)              
048503        MOVE IN-UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)              
048603        MOVE IN-UPLP-KVBEST-PL    TO D904-KVBEST-PL                       
048703        MOVE IN-UPLP-KDPLKOEP     TO D904-KDPLKOEP                        
048803                                                                          
048903        PERFORM IMS-REPL-WDD904                                           
049003     ELSE                                                                 
049103                                                                          
049203*--- EFTERSOM W2215000 SKAPAR TRANS SOM RENSAR FÖRSLAGET                  
049303*--- OCH SEDAN SKAPAR NYA POSTER KAN BEFINTLIGT SEGMENT                   
049403*--- HA TAGITS BORT I DET HÄR PROGRAMMET FÅR VI GÖRA ETT NYUPPLÄGG        
049503                                                                          
049603        MOVE IN-UPLP-DASPECST     TO D904-DASPECST                        
049703        MOVE IN-UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)              
049803        MOVE IN-UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)              
049903        MOVE IN-UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)              
050003        MOVE IN-UPLP-KVBEST-PL    TO D904-KVBEST-PL                       
050103        MOVE IN-UPLP-KDPLKOEP     TO D904-KDPLKOEP                        
050203                                                                          
050303        PERFORM IMS-ISRT-WDD904                                           
050403     END-IF                                                               
050503                                                                          
050603     .                                                                    
050703     EJECT                                                                
050803 G-NYUPPL-LEV   SECTION.                                                  
050903     MOVE 'G-NYUPPL-LEV    ' TO CURRENT-SECTION                           
051003                                                                          
051103     MOVE IN-UPLP-IDARTNR TO W-IDARTNR-D9                                 
051203     MOVE IN-UPLP-IDDC    TO W-IDDC-D9                                    
051303                                                                          
051403     PERFORM IMS-GU-WDD901                                                
051503     IF SEGMENT-SAKNAS                                                    
051603        MOVE IN-UPLP-IDARTNR    TO D901-IDARTNR                           
051703        MOVE IN-UPLP-IDDC       TO D901-IDDC                              
051803        PERFORM IMS-ISRT-WDD901                                           
051903                                                                          
052003        MOVE SPACE              TO D902-WDD902                            
052103        MOVE IN-UPLP-IDLEVNR    TO D902-IDLEVNR                           
052203        MOVE ZERO               TO D902-KVBR                              
052303        MOVE ZERO               TO D902-TILEVPL                           
052403        PERFORM IMS-ISRT-WDD902                                           
052503     ELSE                                                                 
052603                                                                          
052703       MOVE IN-UPLP-IDLEVNR       TO W-IDLEVNR                            
052803       PERFORM IMS-GNP-WDD902                                             
052903       IF SEGMENT-SAKNAS                                                  
053003          MOVE SPACE              TO D902-WDD902                          
053103          MOVE IN-UPLP-IDLEVNR    TO D902-IDLEVNR                         
053203          MOVE ZERO               TO D902-KVBR                            
053303          MOVE ZERO               TO D902-TILEVPL                         
053403          PERFORM IMS-ISRT-WDD902                                         
053503       END-IF                                                             
053603     END-IF                                                               
053703     .                                                                    
053803     EJECT                                                                
053903 H-NYUPPL-AVROP SECTION.                                                  
054003     MOVE 'H-NYUPPL-AVROP     ' TO CURRENT-SECTION                        
054103                                                                          
054203     MOVE IN-UPLP-IDARTNR     TO W-IDARTNR-D9                             
054303     MOVE IN-UPLP-IDDC        TO W-IDDC-D9                                
054403     MOVE IN-UPLP-IDLEVNR     TO W-IDLEVNR                                
054503     MOVE IN-UPLP-KDAVROP     TO W-KDAVROP                                
054603     MOVE IN-UPLP-DAAVROP-AVS TO W-DAAVROP-AVS                            
054703     MOVE IN-UPLP-TILEVDAG    TO W-TILEVDAG                               
054803                                                                          
054903     PERFORM IMS-GHU-WDD905                                               
055003     IF SEGMENT-FINNS                                                     
055103        ADD IN-UPLP-KVAVROP   TO D905-KVAVROP                             
055203        PERFORM IMS-REPL-WDD905                                           
055303     ELSE                                                                 
055403                                                                          
055503*--- EFTERSOM W2215700 SKAPAR TRANS SOM RENSAR FÖRSLAGET                  
055603*--- OCH SEDAN SKAPAR NYA POSTER KAN BEFINTLIGT SEGMENT                   
055703*--- HA TAGITS BORT I DET HÄR PROGRAMMET FÅR VI GÖRA ETT NYUPPLÄGG        
055803                                                                          
055903        MOVE IN-UPLP-IDARTNR       TO W-IDARTNR-D9                        
056003        MOVE IN-UPLP-IDDC          TO W-IDDC-D9                           
056103        MOVE IN-UPLP-IDLEVNR       TO W-IDLEVNR                           
056203        MOVE IN-UPLP-KDAVROP       TO D905-KDAVROP                        
056303        MOVE IN-UPLP-DAAVROP-AVS   TO D905-DAAVROP-AVS                    
056403        MOVE IN-UPLP-TILEVDAG      TO D905-TILEVDAG                       
056503        MOVE IN-UPLP-TIAVRDAT-INL  TO D905-TIAVRDAT-INL                   
056603        MOVE IN-UPLP-TIAVRDAT-DISP TO D905-TIAVRDAT-DISP                  
056703        MOVE IN-UPLP-KVAVROP       TO D905-KVAVROP                        
056803                                                                          
056903        PERFORM IMS-ISRT-WDD905                                           
057003     END-IF                                                               
057103     .                                                                    
057203     EJECT                                                                
062503 J-BEHANDLA-BLOC-AVROP SECTION.                                           
062603     MOVE 'J-BEHANDLA-BLOC-AVROP ' TO CURRENT-SECTION                     
062703                                                                          
062803     MOVE IN-UPLP-IDARTNR         TO BLOC-IDARTNR                         
062903     MOVE IN-UPLP-IDDC            TO BLOC-IDDC                            
063003     MOVE IN-UPLP-IDLEVNR         TO BLOC-IDLEVNR                         
063103     MOVE IN-UPLP-IDLEVNR-SHIP    TO BLOC-IDLEVNR-SHIP                    
063203     MOVE IN-UPLP-IDLANDX2-SHIP   TO BLOC-IDLANDX2-SHIP                   
063303     MOVE IN-UPLP-IDANSK          TO BLOC-IDANSK                          
063403     MOVE IN-UPLP-KDAVROP         TO BLOC-KDAVROP                         
063503     MOVE IN-UPLP-KVQ             TO BLOC-KVQ                             
063603     MOVE IN-UPLP-KVPALL          TO BLOC-KVPALL                          
063703     MOVE IN-UPLP-KVULOAD         TO BLOC-KVULOAD                         
063803     MOVE IN-UPLP-TIAAMMDD-SPECST TO BLOC-TIAAMMDD-SPECST                 
063903     MOVE IN-UPLP-TIAAMMDD-FT     TO BLOC-TIAAMMDD-FT                     
064003     MOVE IN-UPLP-KVDAGAR-TT      TO BLOC-KVDAGAR-TT                      
064103     MOVE IN-UPLP-KVDAGAR-INLEV   TO BLOC-KVDAGAR-INLEV                   
064203                                                                          
064303     MOVE IN-UPLP-FLAGGA-DAGL-AVROP  TO BLOC-FLAGGA-DAGL-AVROP            
064403     MOVE IN-UPLP-TILEVDAG-DAGL(1)   TO BLOC-TILEVDAG-DAGL(1)             
064503     MOVE IN-UPLP-TILEVDAG-DAGL(2)   TO BLOC-TILEVDAG-DAGL(2)             
064603     MOVE IN-UPLP-TILEVDAG-DAGL(3)   TO BLOC-TILEVDAG-DAGL(3)             
064703     MOVE IN-UPLP-TILEVDAG-DAGL(4)   TO BLOC-TILEVDAG-DAGL(4)             
064803     MOVE IN-UPLP-TILEVDAG-DAGL(5)   TO BLOC-TILEVDAG-DAGL(5)             
064903                                                                          
066302     .                                                                    
066402     EJECT                                                                
066502 I-FLYTTA-BLOC-AVROP SECTION.                                             
066602     MOVE 'I-FLYTTA-BLOC-AVROP ' TO CURRENT-SECTION                       
066702                                                                          
066802     IF BLOC-TAB-IX = BLOC-MAX-IX                                         
066902       DISPLAY 'BLOC-TAB FULL IDARTNR ' IN-UPLP-IDARTNR                   
067002       DISPLAY 'IDLEVNR '  IN-UPLP-IDLEVNR                                
067102       DISPLAY 'DAAVROP-AVS '  IN-UPLP-DAAVROP-AVS                        
067202     ELSE                                                                 
067402       ADD +1  TO BLOC-TAB-IX                                             
067502       IF BLOC-TAB-IX < BLOC-MAX-IX                                       
067602         MOVE IN-UPLP-DAAVROP-AVS TO                                      
067702                                  BLOC-DAAVROP-AVS(BLOC-TAB-IX)           
067802         MOVE IN-UPLP-TILEVDAG    TO                                      
067902                                  BLOC-TILEVDAG(BLOC-TAB-IX)              
068002         MOVE IN-UPLP-KVAVROP     TO                                      
068102                                  BLOC-KVAVROP(BLOC-TAB-IX)               
068202         MOVE IN-UPLP-DAAVROP-FOM TO                                      
068302                                  BLOC-DAAVROP-FOM(BLOC-TAB-IX)           
068402         MOVE IN-UPLP-DAAVROP-TOM TO                                      
068502                                  BLOC-DAAVROP-TOM(BLOC-TAB-IX)           
068602         MOVE IN-UPLP-DAAVROP-TFOM TO                                     
068702                                  BLOC-DAAVROP-TFOM(BLOC-TAB-IX)          
068802       END-IF                                                             
068902     END-IF                                                               
069002     .                                                                    
069102     EJECT                                                                
069202 Z-FINIT SECTION.                                                         
069302     MOVE 'Z-FINIT '  TO CURRENT-SECTION                                  
069402                                                                          
069502     CLOSE W2215A                                                         
069602     SKIP2                                                                
069702     MOVE 'S' TO POSTSUM-OPKOD                                            
069802     CALL POSTSUM USING POSTSUM-PARM                                      
069902                                                                          
070002     PERFORM IMS-GHU-RESTART                                              
070102                                                                          
070202     MOVE +0                           TO 4580-KVPOST                     
070302     ACCEPT 4580-TIUPPDAT FROM DATE                                       
070402     ACCEPT 4580-TIUPPTID FROM TIME                                       
070502                                                                          
070602     PERFORM IMS-REPL-RESTART                                             
070702                                                                          
070802     .                                                                    
070902     EJECT                                                                
071002 S01-LAES-W2215A  SECTION.                                                
071102     SKIP2                                                                
071202     READ W2215A INTO IN-AREA                                             
071302     AT END                                                               
071402        MOVE HIGH-VALUE TO IN-AREA                                        
071502        SET END-OF-W2215A TO TRUE                                         
071602                                                                          
071702     NOT AT END                                                           
071802        MOVE 'W2215A'       TO POSTSUM-FDNAMN                             
071902        MOVE 'W2215AD1'     TO POSTSUM-DDNAMN2                            
072002        MOVE IN-UPLP-IDPTYP TO POSTSUM-TRANSTYP                           
072102        CALL POSTSUM USING POSTSUM-PARM                                   
072202                                                                          
072302        ADD 1 TO W-KVPOST-IN                                              
072402     END-READ                                                             
072502     .                                                                    
072602     EJECT                                                                
072702 S11-LAS-FRAM-TILL-CHKPOINT SECTION.                                      
072802     MOVE 'S11-LAS-FRAM-TILL-CHKPOINT '  TO CURRENT-SECTION               
072902                                                                          
073002     PERFORM S01-LAES-W2215A                                              
073102                                                                          
073202     PERFORM UNTIL END-OF-W2215A OR                                       
073302                    W-KVPOST-IN = 4580-KVPOST                             
073402        PERFORM S01-LAES-W2215A                                           
073502     END-PERFORM                                                          
073602                                                                          
073702     IF END-OF-W2215A                                                     
073802        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
073902                                       TO FELTEXT-STR                     
074002        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
074102     END-IF                                                               
074202     .                                                                    
074302     EJECT                                                                
074402 X-TAG-CHECKPOINT   SECTION.                                              
074502                                                                          
074602* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
074702* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
074802                                                                          
074902* -- UPPDATERA ÅTERSTARTREGISTRET                                         
075002     PERFORM IMS-GHU-RESTART                                              
075102     MOVE W-KVPOST-IN    TO 4580-KVPOST                                   
075202     ACCEPT 4580-TIUPPDAT FROM DATE                                       
075302     ACCEPT 4580-TIUPPTID FROM TIME                                       
075402                                                                          
075502     PERFORM IMS-REPL-RESTART                                             
075602                                                                          
075702     PERFORM IMS-CHECKPOINT                                               
075802     MOVE ZERO TO CHKP-ANT                                                
075902* --- LÄS OM DATABAS OM DET BEHÖVS                                        
076002     .                                                                    
076102     EJECT                                                                
076202* --- IMS SEKTIONER ---                                                   
076302                                                                          
076402     EJECT                                                                
076502 IMS-GHU-WDK611 SECTION.                                                  
076602     MOVE 'IMS-GHU-WDK611 '  TO CURRENT-IMS-SECTION                       
076702                                                                          
076802     MOVE SPACE               TO ALL-SSA                                  
076902     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
077002          DELIMITED BY SIZE INTO SSA1                                     
077102     MOVE 'WDK611   ' TO SSA2                                             
077202     MOVE '  ' TO GODK-STATUSKODER                                        
077302     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
077402     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
077502     PERFORM IMS-STATUSKONTROLL                                           
077602     .                                                                    
077702     SKIP3                                                                
077802 IMS-REPL-WDK611 SECTION.                                                 
077902     MOVE 'IMS-REPL-WDK611 '  TO CURRENT-IMS-SECTION                      
078002                                                                          
078102     MOVE '  ' TO GODK-STATUSKODER                                        
078202     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
078302     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
078402     PERFORM IMS-STATUSKONTROLL                                           
078502     ADD +3 TO CHKP-ANT                                                   
078602     .                                                                    
078702     EJECT                                                                
078802 IMS-GU-WDD901 SECTION.                                                   
078902     MOVE 'IMS-GU-WDD901 '  TO CURRENT-IMS-SECTION                        
079002                                                                          
079102     MOVE SPACE               TO ALL-SSA                                  
079202     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
079302          DELIMITED BY SIZE INTO SSA1                                     
079402     MOVE '  GE' TO GODK-STATUSKODER                                      
079502     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
079602     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
079702     PERFORM IMS-STATUSKONTROLL                                           
079802     .                                                                    
079902     EJECT                                                                
080002 IMS-ISRT-WDD901 SECTION.                                                 
080102     MOVE 'IMS-ISRT-WDD901 '  TO CURRENT-IMS-SECTION                      
080202                                                                          
080302     MOVE SPACE               TO ALL-SSA                                  
080402     MOVE 'WDD901 '           TO SSA1                                     
080502     MOVE '    ' TO GODK-STATUSKODER                                      
080602     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD901 SSA1                  
080702     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
080802     PERFORM IMS-STATUSKONTROLL                                           
080902     ADD +2 TO CHKP-ANT                                                   
081002     .                                                                    
081102     EJECT                                                                
081202 IMS-GNP-WDD902 SECTION.                                                  
081302     MOVE 'IMS-GNP-WDD902 '  TO CURRENT-IMS-SECTION                       
081402                                                                          
081502     MOVE SPACE               TO ALL-SSA                                  
081602     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
081702          DELIMITED BY SIZE INTO SSA1                                     
081802     MOVE '  GE' TO GODK-STATUSKODER                                      
081902     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
082002     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
082102     PERFORM IMS-STATUSKONTROLL                                           
082202     .                                                                    
082302     EJECT                                                                
082402 IMS-ISRT-WDD902 SECTION.                                                 
082502     MOVE 'IMS-ISRT-WDD902 '  TO CURRENT-IMS-SECTION                      
082602                                                                          
082702     MOVE SPACE               TO ALL-SSA                                  
082802     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
082902          DELIMITED BY SIZE INTO SSA1                                     
083002     MOVE 'WDD902 '           TO SSA2                                     
083102     MOVE '    ' TO GODK-STATUSKODER                                      
083202     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
083302     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
083402     PERFORM IMS-STATUSKONTROLL                                           
083502     ADD +2 TO CHKP-ANT                                                   
083602     .                                                                    
083702     EJECT                                                                
083802 IMS-GHU-WDD904 SECTION.                                                  
083902     MOVE 'IMS-GHU-WDD904 '  TO CURRENT-IMS-SECTION                       
084002                                                                          
084102     MOVE SPACE               TO ALL-SSA                                  
084202     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
084302          DELIMITED BY SIZE INTO SSA1                                     
084402     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
084502          DELIMITED BY SIZE INTO SSA2                                     
084602     MOVE 'WDD904 ' TO SSA3                                               
084702     MOVE '  GE' TO GODK-STATUSKODER                                      
084802     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3         
084902     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
085002     PERFORM IMS-STATUSKONTROLL                                           
085102     .                                                                    
085202     SKIP3                                                                
085302 IMS-ISRT-WDD904 SECTION.                                                 
085402     MOVE 'IMS-ISRT-WDD904 '  TO CURRENT-IMS-SECTION                      
085502                                                                          
085602     MOVE SPACE               TO ALL-SSA                                  
085702     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
085802          DELIMITED BY SIZE INTO SSA1                                     
085902     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
086002          DELIMITED BY SIZE INTO SSA2                                     
086102     MOVE 'WDD904 ' TO SSA3                                               
086202     MOVE '  II' TO GODK-STATUSKODER                                      
086302     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
086402     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
086502     PERFORM IMS-STATUSKONTROLL                                           
086602     ADD +1 TO CHKP-ANT                                                   
086702     .                                                                    
086802     SKIP3                                                                
086902 IMS-REPL-WDD904 SECTION.                                                 
087002     MOVE 'IMS-REPL-WDD904 '  TO CURRENT-IMS-SECTION                      
087102                                                                          
087202     MOVE '  ' TO GODK-STATUSKODER                                        
087302     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD904                       
087402     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
087502     PERFORM IMS-STATUSKONTROLL                                           
087602     ADD +1 TO CHKP-ANT                                                   
087702     .                                                                    
087802     SKIP3                                                                
087902 IMS-DLET-WDD904 SECTION.                                                 
088002     MOVE 'IMS-DLET-WDD904 '  TO CURRENT-IMS-SECTION                      
088102                                                                          
088202     MOVE '  ' TO GODK-STATUSKODER                                        
088302     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
088402     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
088502     PERFORM IMS-STATUSKONTROLL                                           
088602     ADD +1 TO CHKP-ANT                                                   
088702     .                                                                    
088802     EJECT                                                                
088902 IMS-GHNP-WDD905 SECTION.                                                 
089002     MOVE 'IMS-GHNP-WDD905 '  TO CURRENT-IMS-SECTION                      
089102                                                                          
089202     MOVE SPACE               TO ALL-SSA                                  
089302     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
089402          DELIMITED BY SIZE INTO SSA1                                     
089502     MOVE '  GE' TO GODK-STATUSKODER                                      
089602     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
089702     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
089802     PERFORM IMS-STATUSKONTROLL                                           
089902     .                                                                    
090002     SKIP3                                                                
090102 IMS-GHU-WDD905 SECTION.                                                  
090202     MOVE 'IMS-GHU-WDD905 '  TO CURRENT-IMS-SECTION                       
090302                                                                          
090402     MOVE SPACE               TO ALL-SSA                                  
090502     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
090602          DELIMITED BY SIZE INTO SSA1                                     
090702     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
090802          DELIMITED BY SIZE INTO SSA2                                     
090902     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
091002                    '&KDAVROP  =' W-KDAVROP-X ')'                         
091102          DELIMITED BY SIZE INTO SSA3                                     
091202     MOVE '  GE' TO GODK-STATUSKODER                                      
091302     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3         
091402     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
091502     PERFORM IMS-STATUSKONTROLL                                           
091602     .                                                                    
091702     SKIP3                                                                
091802 IMS-ISRT-WDD905 SECTION.                                                 
091902     MOVE 'IMS-ISRT-WDD905 '  TO CURRENT-IMS-SECTION                      
092002                                                                          
092102     MOVE SPACE               TO ALL-SSA                                  
092202     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
092302          DELIMITED BY SIZE INTO SSA1                                     
092402     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
092502            DELIMITED BY SIZE INTO SSA2                                   
092602     MOVE 'WDD905 ' TO SSA3                                               
092702     MOVE '    ' TO GODK-STATUSKODER                                      
092802     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
092902     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
093002     PERFORM IMS-STATUSKONTROLL                                           
093102     ADD +1 TO CHKP-ANT                                                   
093202     .                                                                    
093302     SKIP3                                                                
093402 IMS-REPL-WDD905 SECTION.                                                 
093502     MOVE 'IMS-REPL-WDD905 '  TO CURRENT-IMS-SECTION                      
093602                                                                          
093702     MOVE '  ' TO GODK-STATUSKODER                                        
093802     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
093902     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
094002     PERFORM IMS-STATUSKONTROLL                                           
094102     ADD +1 TO CHKP-ANT                                                   
094202     .                                                                    
094302     SKIP3                                                                
094402 IMS-DLET-WDD905 SECTION.                                                 
094502     MOVE 'IMS-DLET-WDD905 '  TO CURRENT-IMS-SECTION                      
094602                                                                          
094702     MOVE '  ' TO GODK-STATUSKODER                                        
094802     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
094902     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
095002     PERFORM IMS-STATUSKONTROLL                                           
095102     ADD +1 TO CHKP-ANT                                                   
095202     .                                                                    
095302     EJECT                                                                
095402 IMS-GHU-WDD601 SECTION.                                                  
095502     MOVE 'IMS-GHU-WDD601 '  TO CURRENT-IMS-SECTION                       
095602                                                                          
095702     MOVE SPACE               TO ALL-SSA                                  
095802     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
095902                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
096002          DELIMITED BY SIZE INTO SSA1                                     
096102     MOVE '  GE' TO GODK-STATUSKODER                                      
096202     CALL CBLTDLI USING GHU WDD6-PCB DLI-IO-WDD601 SSA1                   
096302     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
096402     PERFORM IMS-STATUSKONTROLL                                           
096502     .                                                                    
096602     SKIP3                                                                
096702 IMS-GHN-WDD601 SECTION.                                                  
096802     MOVE 'IMS-GHN-WDD601 '  TO CURRENT-IMS-SECTION                       
096902                                                                          
097002     MOVE SPACE               TO ALL-SSA                                  
097102     STRING 'WDD601  (WDD601KY>=' W-WDD601KY-MIN-X                        
097202                    '&WDD601KY<=' W-WDD601KY-MAX-X ')'                    
097302          DELIMITED BY SIZE INTO SSA1                                     
097402     MOVE '  GEGB' TO GODK-STATUSKODER                                    
097502     CALL CBLTDLI USING GHN WDD6-PCB DLI-IO-WDD601 SSA1                   
097602     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
097702     PERFORM IMS-STATUSKONTROLL                                           
097802     .                                                                    
097902     SKIP3                                                                
098002 IMS-DLET-WDD601 SECTION.                                                 
098102     MOVE 'IMS-DLET-WDD601 '  TO CURRENT-IMS-SECTION                      
098202                                                                          
098302     MOVE '  ' TO GODK-STATUSKODER                                        
098402     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
098502     MOVE WDD6-STATUS-CODE TO STATUS-WS                                   
098602     PERFORM IMS-STATUSKONTROLL                                           
098702     ADD +1 TO CHKP-ANT                                                   
098802     .                                                                    
098902     EJECT                                                                
099002 IMS-GHU-RESTART  SECTION.                                                
099102     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
099202                                                                          
099302     MOVE SPACE          TO ALL-SSA                                       
099402     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
099502          DELIMITED BY SIZE INTO SSA1                                     
099602     MOVE 'WDR470   '    TO SSA2                                          
099702     MOVE '    '         TO GODK-STATUSKODER                              
099802     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
099902     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
100002     PERFORM IMS-STATUSKONTROLL                                           
100102     .                                                                    
100202     EJECT                                                                
100302 IMS-REPL-RESTART SECTION.                                                
100402     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
100502                                                                          
100602     MOVE '  '             TO GODK-STATUSKODER                            
100702     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
100802     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
100902     PERFORM IMS-STATUSKONTROLL                                           
101002     .                                                                    
101102     EJECT                                                                
101202 IMS-RESTART SECTION.                                                     
101302     SKIP2                                                                
101402     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
101502     MOVE '  ' TO GODK-STATUSKODER                                        
101602     CALL CBLTDLI USING XRST MSG-PCB                                      
101702                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
101802                        CHKP-AREA-LENGTH CHKP-AREA                        
101902     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102002     PERFORM IMS-STATUSKONTROLL                                           
102102     .                                                                    
102202     SKIP3                                                                
102302 IMS-CHECKPOINT SECTION.                                                  
102402     SKIP2                                                                
102502     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
102602     MOVE '  XD' TO GODK-STATUSKODER                                      
102702     CALL CBLTDLI USING CHKP MSG-PCB                                      
102802                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
102902                        CHKP-AREA-LENGTH CHKP-AREA                        
103002     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103102     PERFORM IMS-STATUSKONTROLL                                           
103202                                                                          
103302     IF IMS-EJ-OK                                                         
103402       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
103502       DISPLAY FELTEXT                                                    
103602       CALL FELLOG                                                        
103702     END-IF                                                               
103802     .                                                                    
103902     EJECT                                                                
104002 IMS-STATUSKONTROLL SECTION.                                              
104102     SKIP2                                                                
104202     SET STATUS-IX TO 1                                                   
104302     SEARCH GODK-STATUS                                                   
104402       AT END                                                             
104502         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
104602           DELIMITED BY SIZE INTO FELTEXT                                 
104702         DISPLAY FELTEXT                                                  
104802         CALL FELLOG                                                      
104902       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
105002         CONTINUE                                                         
106002     END-SEARCH                                                           
110001     .                                                                    
