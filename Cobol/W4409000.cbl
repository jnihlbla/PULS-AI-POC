000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4409000.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   91/04/15.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        FIXPGM.                                                          
001100*        LÄSER INFIL SOM TALAR OM VILKEN ARTIKEL SOM SKALL UPPDATE        
001200*        ARTIKELREGISTREN (K6, K7 & K9)                                   
001300*                                                                         
001400*        PROGRAMMET UPPATERAR WLARTC (WDK6)                               
001410*        PROGRAMMET UPPATERAR WLARTS (WDK7)                               
001500*        PROGRAMMET UPPATERAR WLARTM (WDK9)                               
001600*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL MED ARTIKLAR                                         
002600     SELECT INFIL                      ASSIGN TO W44090D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  INFIL                                                                
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500 01  INPOST  -COPY W440056  -L                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP2                                                                
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W4409000'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  KVAKS                       PIC X(13) VALUE 'KVAKS        '.         
004300 77  KVAKS-PAV                   PIC X(13) VALUE 'KVAKS-E      '.         
004400 77  KVAKS-T                     PIC X(13) VALUE 'KVAKS-F      '.         
004500 77  KVEFRS                      PIC X(13) VALUE 'KVEFRS       '.         
004600 77  KVLS                        PIC X(13) VALUE 'KVLS         '.         
004800 77  KVOKS-BULK                  PIC X(13) VALUE 'KVOKS-BULK   '.         
004900 77  KVOKS-DAG                   PIC X(13) VALUE 'KVOKS-DAG    '.         
005000 77  KVOKS-VOR                   PIC X(13) VALUE 'KVOKS-VOR    '.         
005010 77  KVOKS                       PIC X(13) VALUE 'KVOKS        '.         
005100 77  KVPREAVB-BULK               PIC X(13) VALUE 'KVPREAVB-BULK'.         
005200 77  KVPREAVB-DAG                PIC X(13) VALUE 'KVPREAVB-DAG '.         
005300 77  KVPREAVB-VOR                PIC X(13) VALUE 'KVPREAVB-VOR '.         
005400 77  KVPRERO-BULK                PIC X(13) VALUE 'KVPRERO-BULK '.         
005500 77  KVPRERO-DAG                 PIC X(13) VALUE 'KVPRERO-DAG  '.         
005510 77  KVOFFERT                    PIC X(13) VALUE 'KVOFFERT     '.         
005600 77  KVRESS                      PIC X(13) VALUE 'KVRESS       '.         
005700 77  KVRETUR                     PIC X(13) VALUE 'KVRETUR      '.         
005800 77  KVROS                       PIC X(13) VALUE 'KVROS        '.         
005810 77  KVROS-DAG                   PIC X(13) VALUE 'KVROS-DAG    '.         
005820 77  KVROS-BULK                  PIC X(13) VALUE 'KVROS-BULK   '.         
005900 77  KVSLAGER                    PIC X(13) VALUE 'KVSLAGER     '.         
006000 77  KVSPANT                     PIC X(13) VALUE 'KVSPANT      '.         
006200 77  KVUTRS                      PIC X(13) VALUE 'KVUTRS       '.         
006300 77  SUTPO-EJPB                  PIC X(13) VALUE 'SUTPO-EJPB   '.         
006400 77  SUTPO-PB                    PIC X(13) VALUE 'SUTPO-PB     '.         
006500 77  SUTPO-TOT                   PIC X(13) VALUE 'SUTPO-TOT    '.         
006700                                                                          
006800 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
006900     88  END-OF-INFIL                        VALUE 'J'.                   
007000     EJECT                                                                
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     SKIP2                                                                
008300 01  FELTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005      -PRE  POSTSUM-                                      
009000     EJECT                                                                
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                 'IN-AREA-START  '.                       
009300     SKIP2                                                                
009400                                                                          
009500 01  IN-AREA.                                                             
009600     03  -COPY W440056                                                    
010300     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000     03  W-IDARTNR-X.                                                     
011100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011200     03  W-IDDC-X.                                                        
011300         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
011400     03  W-DABEHOV-X.                                                     
011600         05 W-DABEHOV            PIC  9(6)   VALUE ZERO.                  
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300 01  FILLER                     PIC X(16)   VALUE 'K611-IO-AREA'.         
013500 01  K611-IO-AREA.                                                        
013900*    03  -COPY WDK611                                                     
014000     EJECT                                                                
014010 01  FILLER                     PIC X(16)   VALUE 'K901-IO-AREA'.         
014020 01  K901-IO-AREA.                                                        
014030*    03  -COPY WDK901                                                     
014040     EJECT                                                                
014050 01  FILLER                     PIC X(16)   VALUE 'K911-IO-AREA'.         
014060 01  K911-IO-AREA.                                                        
014070*    03  -COPY WDK911                                                     
014080     EJECT                                                                
014090 01  FILLER                     PIC X(16)   VALUE 'K711-IO-AREA'.         
014100 01  K711-IO-AREA.                                                        
014200*    03  -COPY WDK711                                                     
014300     EJECT                                                                
014700 LINKAGE SECTION.                                                         
015000*01  -COPY W0008      -PRE ARTC-                                          
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300*01  -COPY W0008      -PRE ARTM-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015510*01  -COPY W0008      -PRE ARTS-                                          
015520     05  FILLER                  PIC X.                                   
015530     EJECT                                                                
015600 PROCEDURE DIVISION  USING ARTC-PCB ARTM-PCB ARTS-PCB.                    
015700     ENTRY 'DLITCBL' USING ARTC-PCB ARTM-PCB ARTS-PCB.                    
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-INFIL                                               
016200     PERFORM UNTIL END-OF-INFIL                                           
016300       MOVE IDDC     TO W-IDDC                                            
016400       MOVE IDARTNR  TO W-IDARTNR                                         
016500       EVALUATE IDDB                                                      
016510         WHEN '611'                                                       
016600           PERFORM IMS-GHU-ARTC11                                         
016700           IF SEGMENT-FINNS                                               
017000              PERFORM B-UPPDATERA-WDK611                                  
017100              PERFORM IMS-REPL-ARTC                                       
017500           ELSE                                                           
017600              DISPLAY 'ARTIKEL ' IDARTNR ' SAKNAS PÅ WDK6'                
017700           END-IF                                                         
017710         WHEN '901'                                                       
018000           PERFORM IMS-GET-ARTM01                                         
018100           IF SEGMENT-FINNS                                               
018300              MOVE IDARTNR  TO W-IDARTNR                                  
018400              PERFORM C-UPPADTERA-WDK901                                  
018500              PERFORM IMS-REPL-ARTM01                                     
018600           ELSE                                                           
018700              DISPLAY 'ARTIKEL ' IDARTNR ' SAKNAS PÅ WDK9'                
018800           END-IF                                                         
018900         WHEN '911'                                                       
019100           PERFORM IMS-GET-ARTM01                                         
019200           IF SEGMENT-FINNS                                               
019400              MOVE TIBEHOV  TO W-DABEHOV                                  
019410              IF TIBEHOV NOT = ZERO                                       
019420                IF TIBEHOV < 5000                                         
019430                  MOVE 20   TO W-DABEHOV (1:2)                            
019440                ELSE                                                      
019450                  IF TIBEHOV < 9999                                       
019460                    MOVE 19 TO W-DABEHOV (1:2)                            
019470                  ELSE                                                    
019480                    MOVE 999999 TO W-DABEHOV                              
019490                  END-IF                                                  
019491                END-IF                                                    
019492              END-IF                                                      
019500              PERFORM IMS-GET-ARTM11                                      
019600              IF SEGMENT-FINNS                                            
019700                 PERFORM D-UPPDATERA-WDK911                               
019800                 IF ANT-SUTPO-PB = 0 AND ANT-SUTPO-EJPB = 0               
019900                     PERFORM IMS-DELETE                                   
020000                 ELSE                                                     
020100                     PERFORM IMS-REPL-ARTM11                              
020200                 END-IF                                                   
020300              ELSE                                                        
020400                 IF KDMETOD = 'INS'                                       
020500                    PERFORM E-INSERTA-WDK911                              
020600                 ELSE                                                     
020700                   DISPLAY 'BEHOV ' TIBEHOV                               
020800                   DISPLAY ' SAKNAS FÖR ' IDARTNR  ' PÅ WDK9'             
020900                 END-IF                                                   
021000              END-IF                                                      
021100           ELSE                                                           
021200              DISPLAY 'ARTIKEL ' IDARTNR ' SAKNAS PÅ WDK9'                
021300           END-IF                                                         
021410         WHEN '711'                                                       
021411           PERFORM IMS-GHU-ARTS11                                         
021412           IF SEGMENT-FINNS                                               
021413              PERFORM F-UPPDATERA-WDK711                                  
021414              PERFORM IMS-REPL-ARTS                                       
021415           ELSE                                                           
021416              DISPLAY 'ARTIKEL ' IDARTNR ' SAKNAS PÅ WDK7'                
021417           END-IF                                                         
021420         WHEN OTHER                                                       
021500           DISPLAY 'EJ GODK.SEGMENTTYP: ' IDDB                            
021800       END-EVALUATE                                                       
021900       PERFORM S01-LAES-INFIL                                             
022000     END-PERFORM                                                          
022100                                                                          
022200     PERFORM Z-FINIT                                                      
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     OPEN INPUT  INFIL                                                    
023100     SKIP2                                                                
023300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023400     .                                                                    
023500     EJECT                                                                
023600 B-UPPDATERA-WDK611 SECTION.                                              
023700                                                                          
023800     IF IDSALDO = KVAKS                                                   
023900        PERFORM BA-KVAKS-CDC                                              
024000     END-IF                                                               
024100     IF IDSALDO = KVAKS-PAV                                               
024200        PERFORM BB-KVAKS-PAV                                              
024300     END-IF                                                               
024400     IF IDSALDO = KVAKS-T                                                 
024500        PERFORM BC-KVAKS-T                                                
024600     END-IF                                                               
024700     IF IDSALDO = KVEFRS                                                  
024800        PERFORM BD-KVEFRS                                                 
024900     END-IF                                                               
025000     IF IDSALDO = KVLS                                                    
025100        PERFORM BE-KVLS                                                   
025200     END-IF                                                               
025600     IF IDSALDO = KVRESS                                                  
025700        PERFORM BG-KVRESS                                                 
025800     END-IF                                                               
025900     IF IDSALDO = KVRETUR                                                 
026000        PERFORM BH-KVRETUR                                                
026100     END-IF                                                               
026200     IF IDSALDO = KVROS                                                   
026300        PERFORM BI-KVROS                                                  
026400     END-IF                                                               
026500     IF IDSALDO = KVSLAGER                                                
026600        PERFORM BJ-KVSLAGER                                               
026700     END-IF                                                               
026800     IF IDSALDO = KVSPANT                                                 
026900        PERFORM BK-KVSPANT                                                
027000     END-IF                                                               
027400     IF IDSALDO = KVUTRS                                                  
027500        PERFORM BM-KVUTRS                                                 
027600     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 BA-KVAKS-CDC SECTION.                                                    
028000                                                                          
028100     IF KDMETOD = 'ADD'                                                   
028200        COMPUTE CLAG-KVAKS-CDC = CLAG-KVAKS-CDC    + KVANTAL              
028300     ELSE                                                                 
028400       IF KDMETOD = 'SUB'                                                 
028500          COMPUTE CLAG-KVAKS-CDC = CLAG-KVAKS-CDC    - KVANTAL            
028600       ELSE                                                               
028700          IF KDMETOD = 'REP'                                              
028800             MOVE KVANTAL TO CLAG-KVAKS-CDC                               
028900          END-IF                                                          
029000       END-IF                                                             
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 BB-KVAKS-PAV SECTION.                                                    
029500                                                                          
029600     IF KDMETOD = 'ADD'                                                   
029700        COMPUTE CLAG-KVAKS-PAV = CLAG-KVAKS-PAV    + KVANTAL              
029800     ELSE                                                                 
029900       IF KDMETOD = 'SUB'                                                 
030000          COMPUTE CLAG-KVAKS-PAV = CLAG-KVAKS-PAV    - KVANTAL            
030100       ELSE                                                               
030200          IF KDMETOD = 'REP'                                              
030300             MOVE KVANTAL TO CLAG-KVAKS-PAV                               
030400          END-IF                                                          
030500       END-IF                                                             
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 BC-KVAKS-T   SECTION.                                                    
031000                                                                          
031100     IF KDMETOD = 'ADD'                                                   
031200        COMPUTE CLAG-KVAKS-T   = CLAG-KVAKS-T      + KVANTAL              
031300       ELSE                                                               
031400         IF KDMETOD = 'SUB'                                               
031500            COMPUTE CLAG-KVAKS-T   = CLAG-KVAKS-T      - KVANTAL          
031600          ELSE                                                            
031700            IF KDMETOD = 'REP'                                            
031800               MOVE KVANTAL TO CLAG-KVAKS-T                               
031900            END-IF                                                        
032000          END-IF                                                          
032100       END-IF                                                             
032200     .                                                                    
032300     EJECT                                                                
032400 BD-KVEFRS    SECTION.                                                    
032500                                                                          
032600     IF KDMETOD = 'ADD'                                                   
032700        COMPUTE CLAG-KVEFRS    = CLAG-KVEFRS       + KVANTAL              
032800       ELSE                                                               
032900         IF KDMETOD = 'SUB'                                               
033000            COMPUTE CLAG-KVEFRS    = CLAG-KVEFRS       - KVANTAL          
033100          ELSE                                                            
033200            IF KDMETOD = 'REP'                                            
033300               MOVE KVANTAL TO CLAG-KVEFRS                                
033400            END-IF                                                        
033500          END-IF                                                          
033600       END-IF                                                             
033700     .                                                                    
033800     EJECT                                                                
033900 BE-KVLS      SECTION.                                                    
034000                                                                          
034100     IF KDMETOD = 'ADD'                                                   
034200        COMPUTE CLAG-KVLS      = CLAG-KVLS         + KVANTAL              
034300       ELSE                                                               
034400         IF KDMETOD = 'SUB'                                               
034500            COMPUTE CLAG-KVLS      = CLAG-KVLS         - KVANTAL          
034600          ELSE                                                            
034700            IF KDMETOD = 'REP'                                            
034800               MOVE KVANTAL TO CLAG-KVLS                                  
034900            END-IF                                                        
035000          END-IF                                                          
035100       END-IF                                                             
035200     .                                                                    
035300     EJECT                                                                
036900 BG-KVRESS    SECTION.                                                    
037000                                                                          
037100     IF KDMETOD = 'ADD'                                                   
037200        COMPUTE CLAG-KVRESS    = CLAG-KVRESS       + KVANTAL              
037300       ELSE                                                               
037400         IF KDMETOD = 'SUB'                                               
037500            COMPUTE CLAG-KVRESS    = CLAG-KVRESS       - KVANTAL          
037600          ELSE                                                            
037700            IF KDMETOD = 'REP'                                            
037800               MOVE KVANTAL TO CLAG-KVRESS                                
037900            END-IF                                                        
038000          END-IF                                                          
038100       END-IF                                                             
038200     .                                                                    
038300     EJECT                                                                
038400 BH-KVRETUR   SECTION.                                                    
038500                                                                          
038600     IF KDMETOD = 'ADD'                                                   
038700        COMPUTE CLAG-KVRETUR   = CLAG-KVRETUR      + KVANTAL              
038800       ELSE                                                               
038900         IF KDMETOD = 'SUB'                                               
039000            COMPUTE CLAG-KVRETUR   = CLAG-KVRETUR      - KVANTAL          
039100          ELSE                                                            
039200            IF KDMETOD = 'REP'                                            
039300               MOVE KVANTAL TO CLAG-KVRETUR                               
039400            END-IF                                                        
039500          END-IF                                                          
039600       END-IF                                                             
039700     .                                                                    
039800     EJECT                                                                
039900 BI-KVROS     SECTION.                                                    
040000                                                                          
040100     IF KDMETOD = 'ADD'                                                   
040200        COMPUTE CLAG-KVROS     = CLAG-KVROS        + KVANTAL              
040300       ELSE                                                               
040400         IF KDMETOD = 'SUB'                                               
040500            COMPUTE CLAG-KVROS     = CLAG-KVROS        - KVANTAL          
040600          ELSE                                                            
040700            IF KDMETOD = 'REP'                                            
040800               MOVE KVANTAL TO CLAG-KVROS                                 
040900            END-IF                                                        
041000          END-IF                                                          
041100       END-IF                                                             
041200     .                                                                    
041300     EJECT                                                                
041400 BJ-KVSLAGER  SECTION.                                                    
041500                                                                          
041600     IF KDMETOD = 'ADD'                                                   
041700        COMPUTE CLAG-KVSLAGER  = CLAG-KVSLAGER     + KVANTAL              
041800       ELSE                                                               
041900         IF KDMETOD = 'SUB'                                               
042000            COMPUTE CLAG-KVSLAGER  = CLAG-KVSLAGER     - KVANTAL          
042100          ELSE                                                            
042200            IF KDMETOD = 'REP'                                            
042300               MOVE KVANTAL TO CLAG-KVSLAGER                              
042400            END-IF                                                        
042500          END-IF                                                          
042600       END-IF                                                             
042700     .                                                                    
042800     EJECT                                                                
042900 BK-KVSPANT   SECTION.                                                    
043000                                                                          
043100     IF KDMETOD = 'ADD'                                                   
043200        COMPUTE CLAG-KVSPANT   = CLAG-KVSPANT      + KVANTAL              
043300       ELSE                                                               
043400         IF KDMETOD = 'SUB'                                               
043500            COMPUTE CLAG-KVSPANT   = CLAG-KVSPANT      - KVANTAL          
043600          ELSE                                                            
043700            IF KDMETOD = 'REP'                                            
043800               MOVE KVANTAL TO CLAG-KVSPANT                               
043900            END-IF                                                        
044000          END-IF                                                          
044100       END-IF                                                             
044200     .                                                                    
044300     EJECT                                                                
045900 BM-KVUTRS    SECTION.                                                    
046000                                                                          
046100     IF KDMETOD = 'ADD'                                                   
046200        COMPUTE CLAG-KVUTRS    = CLAG-KVUTRS       + KVANTAL              
046300       ELSE                                                               
046400         IF KDMETOD = 'SUB'                                               
046500            COMPUTE CLAG-KVUTRS    = CLAG-KVUTRS       - KVANTAL          
046600          ELSE                                                            
046700            IF KDMETOD = 'REP'                                            
046800               MOVE KVANTAL TO CLAG-KVUTRS                                
046900            END-IF                                                        
047000          END-IF                                                          
047100       END-IF                                                             
047200     .                                                                    
047300     EJECT                                                                
047400 C-UPPADTERA-WDK901 SECTION.                                              
047500                                                                          
047600     IF IDSALDO = KVOKS-BULK                                              
047700        PERFORM CA-KVOKS-BULK                                             
047800     END-IF                                                               
047900     IF IDSALDO = KVOKS-DAG                                               
048000        PERFORM CB-KVOKS-DAG                                              
048100     END-IF                                                               
048200     IF IDSALDO = KVOKS-VOR                                               
048300        PERFORM CC-KVOKS-VOR                                              
048400     END-IF                                                               
048500     IF IDSALDO = KVPREAVB-BULK                                           
048600        PERFORM CD-KVPREAVB-BULK                                          
048700     END-IF                                                               
048800     IF IDSALDO = KVPREAVB-DAG                                            
048900        PERFORM CE-KVPREAVB-DAG                                           
049000     END-IF                                                               
049100     IF IDSALDO = KVPREAVB-VOR                                            
049200        PERFORM CF-KVPREAVB-VOR                                           
049300     END-IF                                                               
049400     IF IDSALDO = KVPRERO-BULK                                            
049500        PERFORM CG-KVPRERO-BULK                                           
049600     END-IF                                                               
049700     IF IDSALDO = KVPRERO-DAG                                             
049800        PERFORM CH-KVPRERO-DAG                                            
049900     END-IF                                                               
050000     IF IDSALDO = SUTPO-TOT                                               
050100        PERFORM CI-SUTPO-TOT                                              
050200     END-IF                                                               
050300     IF IDSALDO = KVOFFERT                                                
050400        PERFORM CJ-KVOFFERT                                               
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 CA-KVOKS-BULK SECTION.                                                   
050900                                                                          
051000     IF KDMETOD = 'ADD'                                                   
051100        COMPUTE ART-KVOKS-BULK =                                          
051200                ART-KVOKS-BULK +                                          
051300                KVANTAL                                                   
051400       ELSE                                                               
051500         IF KDMETOD = 'SUB'                                               
051600            COMPUTE ART-KVOKS-BULK =                                      
051700                    ART-KVOKS-BULK -                                      
051800                    KVANTAL                                               
051900          ELSE                                                            
052000            IF KDMETOD = 'REP'                                            
052100               MOVE KVANTAL TO ART-KVOKS-BULK                             
052200            END-IF                                                        
052300          END-IF                                                          
052400       END-IF                                                             
052500     .                                                                    
052600     EJECT                                                                
052700 CB-KVOKS-DAG SECTION.                                                    
052800                                                                          
052900     IF KDMETOD = 'ADD'                                                   
053000        COMPUTE ART-KVOKS-DAG =                                           
053100                ART-KVOKS-DAG +                                           
053200                KVANTAL                                                   
053300       ELSE                                                               
053400         IF KDMETOD = 'SUB'                                               
053500            COMPUTE ART-KVOKS-DAG =                                       
053600                    ART-KVOKS-DAG -                                       
053700                    KVANTAL                                               
053800          ELSE                                                            
053900            IF KDMETOD = 'REP'                                            
054000               MOVE KVANTAL TO ART-KVOKS-DAG                              
054100            END-IF                                                        
054200          END-IF                                                          
054300       END-IF                                                             
054400     .                                                                    
054500     EJECT                                                                
054600 CC-KVOKS-VOR SECTION.                                                    
054700                                                                          
054800     IF KDMETOD = 'ADD'                                                   
054900        COMPUTE ART-KVOKS-VOR =                                           
055000                ART-KVOKS-VOR +                                           
055100                KVANTAL                                                   
055200       ELSE                                                               
055300         IF KDMETOD = 'SUB'                                               
055400            COMPUTE ART-KVOKS-VOR =                                       
055500                    ART-KVOKS-VOR -                                       
055600                    KVANTAL                                               
055700          ELSE                                                            
055800            IF KDMETOD = 'REP'                                            
055900               MOVE KVANTAL TO ART-KVOKS-VOR                              
056000            END-IF                                                        
056100          END-IF                                                          
056200       END-IF                                                             
056300     .                                                                    
056400     EJECT                                                                
056500 CD-KVPREAVB-BULK SECTION.                                                
056600                                                                          
056700     IF KDMETOD = 'ADD'                                                   
056800        COMPUTE ART-KVPREAVB-BULK =                                       
056900                ART-KVPREAVB-BULK +                                       
057000                KVANTAL                                                   
057100       ELSE                                                               
057200         IF KDMETOD = 'SUB'                                               
057300            COMPUTE ART-KVPREAVB-BULK =                                   
057400                    ART-KVPREAVB-BULK -                                   
057500                    KVANTAL                                               
057600          ELSE                                                            
057700            IF KDMETOD = 'REP'                                            
057800               MOVE KVANTAL TO ART-KVPREAVB-BULK                          
057900            END-IF                                                        
058000          END-IF                                                          
058100       END-IF                                                             
058200     .                                                                    
058300     EJECT                                                                
058400 CE-KVPREAVB-DAG  SECTION.                                                
058500                                                                          
058600     IF KDMETOD = 'ADD'                                                   
058700        COMPUTE ART-KVPREAVB-DAG =                                        
058800                ART-KVPREAVB-DAG +                                        
058900                KVANTAL                                                   
059000       ELSE                                                               
059100         IF KDMETOD = 'SUB'                                               
059200            COMPUTE ART-KVPREAVB-DAG =                                    
059300                    ART-KVPREAVB-DAG -                                    
059400                    KVANTAL                                               
059500          ELSE                                                            
059600            IF KDMETOD = 'REP'                                            
059700               MOVE KVANTAL TO ART-KVPREAVB-DAG                           
059800            END-IF                                                        
059900          END-IF                                                          
060000       END-IF                                                             
060100     .                                                                    
060200     EJECT                                                                
060300 CF-KVPREAVB-VOR  SECTION.                                                
060400                                                                          
060500     IF KDMETOD = 'ADD'                                                   
060600        COMPUTE ART-KVPREAVB-VOR =                                        
060700                ART-KVPREAVB-VOR +                                        
060800                KVANTAL                                                   
060900       ELSE                                                               
061000         IF KDMETOD = 'SUB'                                               
061100            COMPUTE ART-KVPREAVB-VOR =                                    
061200                    ART-KVPREAVB-VOR -                                    
061300                    KVANTAL                                               
061400          ELSE                                                            
061500            IF KDMETOD = 'REP'                                            
061600               MOVE KVANTAL TO ART-KVPREAVB-VOR                           
061700            END-IF                                                        
061800          END-IF                                                          
061900       END-IF                                                             
062000     .                                                                    
062100     EJECT                                                                
062200 CG-KVPRERO-BULK SECTION.                                                 
062300                                                                          
062400     IF KDMETOD = 'ADD'                                                   
062500        COMPUTE ART-KVPRERO-BULK =                                        
062600                ART-KVPRERO-BULK +                                        
062700                KVANTAL                                                   
062800       ELSE                                                               
062900         IF KDMETOD = 'SUB'                                               
063000            COMPUTE ART-KVPRERO-BULK =                                    
063100                    ART-KVPRERO-BULK -                                    
063200                    KVANTAL                                               
063300          ELSE                                                            
063400            IF KDMETOD = 'REP'                                            
063500               MOVE KVANTAL TO ART-KVPRERO-BULK                           
063600            END-IF                                                        
063700          END-IF                                                          
063800       END-IF                                                             
063900     .                                                                    
064000     EJECT                                                                
064100 CH-KVPRERO-DAG  SECTION.                                                 
064200                                                                          
064300     IF KDMETOD = 'ADD'                                                   
064400        COMPUTE ART-KVPRERO-DAG =                                         
064500                ART-KVPRERO-DAG +                                         
064600                KVANTAL                                                   
064700       ELSE                                                               
064800         IF KDMETOD = 'SUB'                                               
064900            COMPUTE ART-KVPRERO-DAG =                                     
065000                    ART-KVPRERO-DAG -                                     
065100                    KVANTAL                                               
065200          ELSE                                                            
065300            IF KDMETOD = 'REP'                                            
065400               MOVE KVANTAL TO ART-KVPRERO-DAG                            
065500            END-IF                                                        
065600          END-IF                                                          
065700       END-IF                                                             
065800     .                                                                    
065900     EJECT                                                                
066000 CI-SUTPO-TOT    SECTION.                                                 
066100                                                                          
066200     IF KDMETOD = 'ADD'                                                   
066300        COMPUTE ART-SUTPO-TOT =                                           
066400                ART-SUTPO-TOT +                                           
066500                KVANTAL                                                   
066600       ELSE                                                               
066700         IF KDMETOD = 'SUB'                                               
066800            COMPUTE ART-SUTPO-TOT =                                       
066900                    ART-SUTPO-TOT -                                       
067000                    KVANTAL                                               
067100          ELSE                                                            
067200            IF KDMETOD = 'REP'                                            
067300               MOVE KVANTAL TO ART-SUTPO-TOT                              
067400            END-IF                                                        
067500          END-IF                                                          
067600       END-IF                                                             
067700     .                                                                    
067800     EJECT                                                                
067900 CJ-KVOFFERT     SECTION.                                                 
068000                                                                          
068100     IF KDMETOD = 'ADD'                                                   
068200        COMPUTE ART-KVOFFERT =                                            
068300                ART-KVOFFERT +                                            
068400                KVANTAL                                                   
068500     ELSE                                                                 
068600       IF KDMETOD = 'SUB'                                                 
068700          COMPUTE ART-KVOFFERT =                                          
068800                  ART-KVOFFERT -                                          
068900                  KVANTAL                                                 
069400        END-IF                                                            
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 D-UPPDATERA-WDK911 SECTION.                                              
069900                                                                          
070000     IF IDSALDO = SUTPO-PB                                                
070100        PERFORM DA-SUTPO-PB                                               
070200     END-IF                                                               
070300     IF IDSALDO = SUTPO-EJPB                                              
070400        PERFORM DB-SUTPO-EJPB                                             
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 DA-SUTPO-PB     SECTION.                                                 
070900                                                                          
071000     IF KDMETOD = 'ADD'                                                   
071100        COMPUTE ANT-SUTPO-PB   = ANT-SUTPO-PB      + KVANTAL              
071200       ELSE                                                               
071300         IF KDMETOD = 'SUB'                                               
071400            COMPUTE ANT-SUTPO-PB   = ANT-SUTPO-PB      - KVANTAL          
071500          ELSE                                                            
071600            IF KDMETOD = 'REP'                                            
071700               MOVE KVANTAL TO ANT-SUTPO-PB                               
071800            END-IF                                                        
071900          END-IF                                                          
072000       END-IF                                                             
072100     .                                                                    
072200     EJECT                                                                
072300 DB-SUTPO-EJPB   SECTION.                                                 
072400                                                                          
072500     IF KDMETOD = 'ADD'                                                   
072600        COMPUTE ANT-SUTPO-EJPB = ANT-SUTPO-EJPB    + KVANTAL              
072700       ELSE                                                               
072800         IF KDMETOD = 'SUB'                                               
072900            COMPUTE ANT-SUTPO-EJPB = ANT-SUTPO-EJPB    - KVANTAL          
073000          ELSE                                                            
073100            IF KDMETOD = 'REP'                                            
073200               MOVE KVANTAL TO ANT-SUTPO-EJPB                             
073300            END-IF                                                        
073400          END-IF                                                          
073500       END-IF                                                             
073600     .                                                                    
073700     EJECT                                                                
073800 E-INSERTA-WDK911   SECTION.                                              
073900                                                                          
074000     IF TIBEHOV > ZERO                                                    
074200       MOVE TIBEHOV        TO ANT-DABEHOV                                 
074210       IF TIBEHOV < 5000                                                  
074220         MOVE 20           TO ANT-DABEHOV (1:2)                           
074230       ELSE                                                               
074240         IF TIBEHOV < 9999                                                
074250           MOVE 19         TO ANT-DABEHOV (1:2)                           
074260         ELSE                                                             
074270           MOVE 999999     TO ANT-DABEHOV                                 
074280         END-IF                                                           
074290       END-IF                                                             
074300       IF IDSALDO = SUTPO-PB                                              
074400          MOVE KVANTAL     TO ANT-SUTPO-PB                                
074500          MOVE ZERO        TO ANT-SUTPO-EJPB                              
074600          PERFORM IMS-ISRT-ARTM                                           
074700       END-IF                                                             
074800       IF IDSALDO = SUTPO-EJPB                                            
074900          MOVE KVANTAL     TO ANT-SUTPO-EJPB                              
075000          MOVE ZERO        TO ANT-SUTPO-PB                                
075100          PERFORM IMS-ISRT-ARTM                                           
075200       END-IF                                                             
075300     ELSE                                                                 
075400       DISPLAY 'TIBEHOV EJ > NOLL'                                        
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075710 F-UPPDATERA-WDK711 SECTION.                                              
075720                                                                          
075730     IF IDSALDO = KVOKS-DAG                                               
075740        PERFORM FA-KVOKS-DAG                                              
075751     END-IF                                                               
075760     IF IDSALDO = KVOKS-BULK                                              
075770        PERFORM FB-KVOKS-BULK                                             
075772     END-IF                                                               
075773     IF IDSALDO = KVROS-DAG                                               
075774        PERFORM FC-KVROS-DAG                                              
075775     END-IF                                                               
075776     IF IDSALDO = KVROS-BULK                                              
075777        PERFORM FD-KVROS-BULK                                             
075780     END-IF                                                               
075790     IF IDSALDO = KVRESS                                                  
075800        PERFORM FE-KVRESS                                                 
075810     END-IF                                                               
075817     .                                                                    
075818     EJECT                                                                
075819 FA-KVOKS-DAG SECTION.                                                    
075820                                                                          
075821     IF KDMETOD = 'ADD'                                                   
075822        COMPUTE SLAG-KVOKS-DAG   = SLAG-KVOKS-DAG  + KVANTAL              
075823     ELSE                                                                 
075824       IF KDMETOD = 'SUB'                                                 
075825          COMPUTE SLAG-KVOKS-DAG = SLAG-KVOKS-DAG  - KVANTAL              
075826       ELSE                                                               
075827          IF KDMETOD = 'REP'                                              
075828             MOVE KVANTAL TO SLAG-KVOKS-DAG                               
075829          END-IF                                                          
075830       END-IF                                                             
075831     END-IF                                                               
075832     .                                                                    
075833     EJECT                                                                
075834 FB-KVOKS-BULK SECTION.                                                   
075835                                                                          
075836     IF KDMETOD = 'ADD'                                                   
075837        COMPUTE SLAG-KVOKS-BULK   = SLAG-KVOKS-BULK + KVANTAL             
075838     ELSE                                                                 
075839       IF KDMETOD = 'SUB'                                                 
075840          COMPUTE SLAG-KVOKS-BULK = SLAG-KVOKS-BULK - KVANTAL             
075841       ELSE                                                               
075842          IF KDMETOD = 'REP'                                              
075843             MOVE KVANTAL TO SLAG-KVOKS-BULK                              
075844          END-IF                                                          
075845       END-IF                                                             
075846     END-IF                                                               
075847     .                                                                    
075848     EJECT                                                                
075849 FC-KVROS-DAG  SECTION.                                                   
075850                                                                          
075851     IF KDMETOD = 'ADD'                                                   
075852        COMPUTE SLAG-KVROS-DAG    = SLAG-KVROS-DAG  + KVANTAL             
075853     ELSE                                                                 
075854       IF KDMETOD = 'SUB'                                                 
075855          COMPUTE SLAG-KVROS-DAG  = SLAG-KVROS-DAG  - KVANTAL             
075856       ELSE                                                               
075857          IF KDMETOD = 'REP'                                              
075858             MOVE KVANTAL TO SLAG-KVROS-DAG                               
075859          END-IF                                                          
075860       END-IF                                                             
075861     END-IF                                                               
075862     .                                                                    
075863     EJECT                                                                
075864 FD-KVROS-BULK SECTION.                                                   
075865                                                                          
075866     IF KDMETOD = 'ADD'                                                   
075867        COMPUTE SLAG-KVROS-BULK   = SLAG-KVROS-BULK + KVANTAL             
075868     ELSE                                                                 
075869       IF KDMETOD = 'SUB'                                                 
075870          COMPUTE SLAG-KVROS-BULK = SLAG-KVROS-BULK - KVANTAL             
075871       ELSE                                                               
075872          IF KDMETOD = 'REP'                                              
075873             MOVE KVANTAL TO SLAG-KVROS-BULK                              
075874          END-IF                                                          
075875       END-IF                                                             
075876     END-IF                                                               
075877     .                                                                    
075878     EJECT                                                                
075879 FE-KVRESS SECTION.                                                       
075880                                                                          
075881     IF KDMETOD = 'ADD'                                                   
075882        COMPUTE SLAG-KVRESS = SLAG-KVRESS + KVANTAL                       
075883     ELSE                                                                 
075884       IF KDMETOD = 'SUB'                                                 
075885          COMPUTE SLAG-KVRESS = SLAG-KVRESS - KVANTAL                     
075886       ELSE                                                               
075887          IF KDMETOD = 'REP'                                              
075888             MOVE KVANTAL TO SLAG-KVRESS                                  
075889          END-IF                                                          
075890       END-IF                                                             
075891     END-IF                                                               
075892     .                                                                    
075893     EJECT                                                                
075894 Z-FINIT SECTION.                                                         
075900     CLOSE INFIL                                                          
076000     SKIP2                                                                
076100     MOVE 'S' TO POSTSUM-OPKOD                                            
076200     CALL POSTSUM USING POSTSUM-PARM                                      
076300     .                                                                    
076400     EJECT                                                                
076500 S01-LAES-INFIL   SECTION.                                                
076600     SKIP2                                                                
076700     READ INFIL INTO IN-AREA                                              
076800     AT END                                                               
076900        MOVE HIGH-VALUE TO IN-AREA                                        
077000        SET END-OF-INFIL TO TRUE                                          
077100                                                                          
077200     END-READ                                                             
077300     .                                                                    
077400     EJECT                                                                
077500* --- IMS SEKTIONER ---                                                   
077600     SKIP3                                                                
077800 IMS-GHU-ARTC11   SECTION.                                                
077900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
078000          DELIMITED BY SIZE INTO SSA1                                     
078010     MOVE   'WLARTC11' TO SSA2                                            
078100     MOVE '  GE' TO GODK-STATUSKODER                                      
078200     CALL CBLTDLI USING GHU ARTC-PCB K611-IO-AREA SSA1 SSA2               
078300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
078400     PERFORM IMS-STATUSKONTROLL                                           
078500     .                                                                    
079600     SKIP3                                                                
079700 IMS-REPL-ARTC SECTION.                                                   
079800                                                                          
079900     MOVE '  ' TO GODK-STATUSKODER                                        
080000     CALL CBLTDLI USING REPL ARTC-PCB K611-IO-AREA                        
080100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     .                                                                    
080400     EJECT                                                                
080500 IMS-GET-ARTM01   SECTION.                                                
080600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
080700          DELIMITED BY SIZE INTO SSA1                                     
080800     MOVE '  GE' TO GODK-STATUSKODER                                      
080900     CALL CBLTDLI USING GHU ARTM-PCB K901-IO-AREA SSA1                    
081000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
081100     PERFORM IMS-STATUSKONTROLL                                           
081200     .                                                                    
081310     SKIP3                                                                
081400 IMS-GET-ARTM11  SECTION.                                                 
081500     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
081600          DELIMITED BY SIZE INTO SSA1                                     
081700     MOVE '  GE' TO GODK-STATUSKODER                                      
081800     CALL CBLTDLI USING GHNP ARTM-PCB K911-IO-AREA SSA1                   
081900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
082000     PERFORM IMS-STATUSKONTROLL                                           
082100     .                                                                    
082200     EJECT                                                                
082300 IMS-REPL-ARTM01 SECTION.                                                 
082400                                                                          
082500     MOVE '  ' TO GODK-STATUSKODER                                        
082600     CALL CBLTDLI USING REPL ARTM-PCB K901-IO-AREA                        
082700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
082910     SKIP3                                                                
082920 IMS-REPL-ARTM11 SECTION.                                                 
082930                                                                          
082940     MOVE '  ' TO GODK-STATUSKODER                                        
082950     CALL CBLTDLI USING REPL ARTM-PCB K911-IO-AREA                        
082960     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
082970     PERFORM IMS-STATUSKONTROLL                                           
082980     .                                                                    
082990     EJECT                                                                
083100 IMS-DELETE SECTION.                                                      
083200                                                                          
083300     MOVE '  ' TO GODK-STATUSKODER                                        
083400     CALL CBLTDLI USING DLET ARTM-PCB K911-IO-AREA                        
083500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
083600     PERFORM IMS-STATUSKONTROLL                                           
083700     .                                                                    
083800     SKIP3                                                                
083900 IMS-ISRT-ARTM SECTION.                                                   
084000                                                                          
084100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
084200          DELIMITED BY SIZE INTO SSA1                                     
084300     MOVE 'WLARTM11'          TO SSA2                                     
084400     MOVE '  ' TO GODK-STATUSKODER                                        
084500     CALL CBLTDLI USING ISRT ARTM-PCB K911-IO-AREA SSA1 SSA2              
084600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
084700     PERFORM IMS-STATUSKONTROLL                                           
084800     .                                                                    
084900     EJECT                                                                
084920 IMS-GHU-ARTS11   SECTION.                                                
084930     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
084940          DELIMITED BY SIZE INTO SSA1                                     
084941     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
084942          DELIMITED BY SIZE INTO SSA2                                     
084960     MOVE '  GE' TO GODK-STATUSKODER                                      
084970     CALL CBLTDLI USING GHU ARTS-PCB K711-IO-AREA SSA1 SSA2               
084980     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
084990     PERFORM IMS-STATUSKONTROLL                                           
084991     .                                                                    
084992     SKIP3                                                                
084993 IMS-REPL-ARTS SECTION.                                                   
084994                                                                          
084995     MOVE '  ' TO GODK-STATUSKODER                                        
084996     CALL CBLTDLI USING REPL ARTS-PCB K711-IO-AREA                        
084997     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
084998     PERFORM IMS-STATUSKONTROLL                                           
084999     .                                                                    
085000     SKIP3                                                                
085030 IMS-STATUSKONTROLL SECTION.                                              
085100     SKIP2                                                                
085200     SET STATUS-IX TO 1                                                   
085300     SEARCH GODK-STATUS                                                   
085400       AT END                                                             
085500         MOVE 'EJ GODK. STATUSKOD FRN IMS' TO FELTEXT-STR                 
085600         DISPLAY FELTEXT                                                  
085700         CALL FELLOG                                                      
085800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
085900     END-SEARCH                                                           
086000     .                                                                    
