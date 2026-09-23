000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2120400.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   96/10/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        BEARBETNING AV UPPDATERINGSTRANSAR FRÅN W21202                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- TRANSAKTIONSFIL                                            
002400     SELECT W21203                     ASSIGN TO W21204D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W21203                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W2120301      -L.                                              
003500                                                                          
003600*01  -COPY W2120302      -L.                                              
003700                                                                          
003800*01  -COPY W2120303      -L.                                              
003900                                                                          
004000*01  -COPY W2120304      -L.                                              
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2120400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000*01  -COPY WWDCKONS                                                       
005100                                                                          
005200 01  BEHANDLINGSKODER-R22.                                                
005300     03  NYTT-BEST           PIC S9(1)   VALUE +1    COMP-3.              
005400     03  BEKR-BEST           PIC S9(1)   VALUE +2    COMP-3.              
005500     03  JUSTE-UPP           PIC S9(1)   VALUE +3    COMP-3.              
005600     03  JUSTE-NED           PIC S9(1)   VALUE +4    COMP-3.              
005700     03  NYTT-ANNU           PIC S9(1)   VALUE +5    COMP-3.              
005800     03  BEKR-ANNU           PIC S9(1)   VALUE +6    COMP-3.              
005900                                                                          
006000                                                                          
006100 01  CHKP-VAR.                                                            
006200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200                                                                          
007300 77  W21203-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W21203                       VALUE 'J'.                   
007500                                                                          
007600 77  SEGM-SKA-FINNAS-SW          PIC X       VALUE 'J'.                   
007700     88  SEGM-SKA-FINNAS                     VALUE 'J'.                   
007800     88  SEGM-KAN-SAKNAS                     VALUE 'N'.                   
007900                                                                          
008000 77  RAETT-SEGM-SW               PIC X       VALUE 'N'.                   
008100     88  RAETT-SEGM                          VALUE 'J'.                   
008200     88  FEL-SEGM                            VALUE 'N'.                   
008300                                                                          
008400 77  RAETT-AVT-SW                PIC X       VALUE 'N'.                   
008500     88  RAETT-AVT                           VALUE 'J'.                   
008600     88  FEL-AVT                             VALUE 'N'.                   
008700                                                                          
008800 77  AVTAL-FINNS-SW              PIC X(1)    VALUE 'N'.                   
008900     88  AVTAL-FINNS                         VALUE 'J'.                   
009000     88  AVTAL-SAKNAS                        VALUE 'N'.                   
009100     EJECT                                                                
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700     EJECT                                                                
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL ABEND                                            
011000*                                                                         
011100 01  RKOD                        PIC S9(4)   VALUE ZERO BINARY.           
011200     EJECT                                                                
011300 01  IN-AREA-START               PIC X(24)   VALUE                        
011400                                             'IN-AREA-START'.             
011500     SKIP2                                                                
011600 01  IN-AREA.                                                             
011700     03  IN-IDPTYP           PIC X(3).                                    
011800     03  IN-IDARTNR          PIC S9(9)  COMP-3.                           
011900     03  FILLER              PIC X(50).                                   
012000                                                                          
012100*01  FILLER -COPY W2120301  -PRE IN-  -RED  IN-AREA                       
012200                                                                          
012300*01  FILLER -COPY W2120302  -PRE IN-  -RED  IN-AREA                       
012400                                                                          
012500*01  FILLER -COPY W2120303  -PRE IN-  -RED  IN-AREA                       
012600                                                                          
012700*01  FILLER -COPY W2120304  -PRE IN-  -RED  IN-AREA                       
012800                                                                          
012900     EJECT                                                                
013000*    -- ARBETSFÄLT                                                        
013100 01  TEST-IDAVTAL            PIC 9(13).                                   
013200 01  FILLER REDEFINES TEST-IDAVTAL.                                       
013300     03  FILLER              PIC 9.                                       
013400     03  TEST-PREFIX         PIC 9(3).                                    
013500     03  TEST-ORDERNR        PIC 9(6).                                    
013600     03  FILLER REDEFINES TEST-ORDERNR.                                   
013700         05  ORDERNR-POS1    PIC 9.                                       
013800         05  FILLER          PIC 9(5).                                    
013900     03  TEST-SUFFIX         PIC 9(3).                                    
014000                                                                          
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP3                                                                
014400 01  NYCKLAR-TILL-DLI.                                                    
014500     03  W-IDARTNR-X.                                                     
014600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014700     03  W-WDD901KY-X.                                                    
014800         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
014900         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
015000     03  W-IDLEVNR-X.                                                     
015100         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015200     03  W-IDBEST-X.                                                      
015300         05  W-IDBEST            PIC S9(13)  VALUE ZERO COMP-3.           
015400     03  W-IDAVTAL-X.                                                     
015500         05  W-IDAVTAL           PIC S9(13)  VALUE ZERO COMP-3.           
015600     03  W-KDERS-0-X.                                                     
015700         05    FILLER            PIC S9(3)   VALUE ZERO COMP-3.           
015800     SKIP2                                                                
015900*    --- STATUS-KOD FRÅN IMS                                              
016000 01  STATUS-WS                   PIC XX.                                  
016100     88  SEGMENT-FINNS                       VALUE '  '.                  
016200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016500     88  IMS-EJ-OK                           VALUE 'XD'.                  
016600     SKIP2                                                                
016700 01  GODK-STATUSKODER.                                                    
016800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01  SSA1                        PIC X(64).                               
017100 01  SSA2                        PIC X(64).                               
017200 01  SSA3                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800                                                                          
017900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
018000 01  DLI-IO-WLARTC01.                                                     
018100*    03  -COPY WDK601  -PRE ARTC-                                         
018200     EJECT                                                                
018300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
018400 01  DLI-IO-WLARTC11.                                                     
018500*    03  -COPY WDK611  -PRE ARTC-                                         
018600     EJECT                                                                
018700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC22'.                    
018800 01  DLI-IO-WLARTC22.                                                     
018900*    03  -COPY WDK622  -PRE ARTC-                                         
019000     EJECT                                                                
019100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC23'.                    
019200 01  DLI-IO-WLARTC23.                                                     
019300*    03  -COPY WDK623  -PRE ARTC-                                         
019400     EJECT                                                                
019500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLB01'.                    
019600 01  DLI-IO-WLINLB01.                                                     
019700*    03  -COPY WDD901  -PRE INLB-ART-                                     
019800     EJECT                                                                
019900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLB11'.                    
020000 01  DLI-IO-WLINLB11.                                                     
020100*    03  -COPY WDD902  -PRE INLB-LEVPL-                                   
020200                                                                          
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500                                                                          
020600*01  -COPY W0009  -PRE MSG-                                               
020700     EJECT                                                                
020800*01  -COPY W0008  -PRE ARTC-                                              
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE INLB-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB INLB-PCB.                     
021500 MAIN SECTION.                                                            
021600     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB INLB-PCB.                     
021700                                                                          
021800     PERFORM A-INIT                                                       
021900     PERFORM S01-LAES-W21203                                              
022000     PERFORM UNTIL END-OF-W21203                                          
022100       IF IN-IDARTNR NOT = ARTC-ART-IDARTNR                               
022200         IF CHKP-ANT > CHKP-MAX                                           
022300           PERFORM X-TAG-CHECKPOINT                                       
022400         END-IF                                                           
022500         MOVE IN-IDARTNR TO W-IDARTNR                                     
022600         PERFORM IMS-GET-ARTC-ART                                         
022700       END-IF                                                             
022800       EVALUATE IN-IDPTYP                                                 
022900         WHEN 'NBE'  PERFORM B-NYUPPL-BEST-INFO                           
023000         WHEN 'UBE'  PERFORM C-UPPDAT-BEST-INFO                           
023100         WHEN 'BBE'  PERFORM D-BORTTAG-BEST                               
023200         WHEN 'NAV'  PERFORM E-NYUPPL-AVT-INFO                            
023300         WHEN 'BAV'  PERFORM G-BORTTAG-AVTAL                              
023400         WHEN 'NLE'  PERFORM H-NYUPPL-LEVPLAN-INFO                        
023500         WHEN 'ULE'  PERFORM I-UPPDAT-LEVPLAN-INFO                        
023600         WHEN 'UMF'  PERFORM J-UPPDAT-MTRLF-INFO                          
023700                                                                          
023800         WHEN OTHER                                                       
023900                     MOVE 16 TO RKOD                                      
024000                     CALL ABEND USING RKOD                                
024100       END-EVALUATE                                                       
024200                                                                          
024300       PERFORM S01-LAES-W21203                                            
024400     END-PERFORM                                                          
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300     SKIP2                                                                
025400     PERFORM IMS-RESTART                                                  
025500                                                                          
025600     OPEN INPUT W21203                                                    
025700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025800                                                                          
025900     MOVE ZERO TO ARTC-ART-IDARTNR                                        
026000     .                                                                    
026100     EJECT                                                                
026200 B-NYUPPL-BEST-INFO SECTION.                                              
026300     SKIP2                                                                
026400     MOVE IN-BEST-IDBEST       TO W-IDBEST                                
026500                                                                          
026600     PERFORM BA-HITTA-BEST-NYUPPL                                         
026700     IF SEGMENT-SAKNAS                                                    
026800       MOVE IN-BEST-IDBEST       TO ARTC-BEST-IDBEST                      
026900       MOVE IN-BEST-IDLEVNR-BEST TO ARTC-BEST-IDLEVNR-BEST                
027000       MOVE IN-BEST-KDBEH-BEST   TO ARTC-BEST-KDBEH-BEST                  
027100       MOVE IN-BEST-KVBEST       TO ARTC-BEST-KVBEST                      
027200       MOVE IN-BEST-KVBEST-BEKR  TO ARTC-BEST-KVBEST-BEKR                 
027300       MOVE IN-BEST-TIBEST       TO ARTC-BEST-TIBEST                      
027400                                                                          
027500       PERFORM IMS-ISRT-ARTC-BEST                                         
027600     END-IF                                                               
027700                                                                          
027800*   --- ANNULLATION TAR BORT EV AVTALSSEGMENT                             
027900*   --- FÖR NEDCAR-AVTAL KOLLAS OM DE HAR BYTT AVTALSNUMMER EFTER         
028000*   --- UPPLÄGG. I SÅ FALL FIXAR VI ORDERNUMRET OCH KOLLAR IGEN           
028100                                                                          
028200     IF IN-BEST-KDBEH-BEST = 5 OR 6                                       
028300       MOVE IN-BEST-IDBEST TO W-IDAVTAL                                   
028400       PERFORM IMS-GET-F-ARTC-AVT                                         
028500       MOVE NEJ TO RAETT-AVT-SW                                           
028600       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
028700          IF IN-AVT-IDLEVNR-AVT = ARTC-AVT-IDLEVNR-AVT                    
028800             PERFORM IMS-DLET-ARTC-AVT                                    
028900             MOVE JA TO RAETT-AVT-SW                                      
029000          END-IF                                                          
029100          PERFORM IMS-GET-ARTC-AVT                                        
029200       END-PERFORM                                                        
029300       IF FEL-AVT                                                         
029400         MOVE W-IDAVTAL TO TEST-IDAVTAL                                   
029500         IF TEST-PREFIX = 640                                             
029600          IF ORDERNR-POS1 = 1                                             
029700           MOVE 0 TO ORDERNR-POS1                                         
029800           MOVE TEST-IDAVTAL TO W-IDAVTAL                                 
029900           PERFORM IMS-GET-F-ARTC-AVT                                     
030000           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
030100              IF IN-AVT-IDLEVNR-AVT = ARTC-AVT-IDLEVNR-AVT                
030200                 PERFORM IMS-DLET-ARTC-AVT                                
030300                 MOVE JA TO RAETT-AVT-SW                                  
030400              END-IF                                                      
030500              PERFORM IMS-GET-ARTC-AVT                                    
030600           END-PERFORM                                                    
030700           IF FEL-AVT                                                     
030800             IF TEST-SUFFIX = 100                                         
030900               MOVE 115 TO TEST-SUFFIX                                    
031000               MOVE TEST-IDAVTAL TO W-IDAVTAL                             
031100               PERFORM IMS-GET-F-ARTC-AVT                                 
031200               PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT               
031300                  IF IN-AVT-IDLEVNR-AVT = ARTC-AVT-IDLEVNR-AVT            
031400                     PERFORM IMS-DLET-ARTC-AVT                            
031500                     MOVE JA TO RAETT-AVT-SW                              
031600                  END-IF                                                  
031700                  PERFORM IMS-GET-ARTC-AVT                                
031800               END-PERFORM                                                
031900             END-IF                                                       
032000           END-IF                                                         
032100          ELSE                                                            
032200             IF TEST-SUFFIX = 100                                         
032300               MOVE 115 TO TEST-SUFFIX                                    
032400               MOVE TEST-IDAVTAL TO W-IDAVTAL                             
032500               PERFORM IMS-GET-F-ARTC-AVT                                 
032600               PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT               
032700                  IF IN-AVT-IDLEVNR-AVT = ARTC-AVT-IDLEVNR-AVT            
032800                     PERFORM IMS-DLET-ARTC-AVT                            
032900                     MOVE JA TO RAETT-AVT-SW                              
033000                  END-IF                                                  
033100                  PERFORM IMS-GET-ARTC-AVT                                
033200               END-PERFORM                                                
033300             END-IF                                                       
033400           END-IF                                                         
033500         END-IF                                                           
033600       END-IF                                                             
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 BA-HITTA-BEST-NYUPPL         SECTION.                                    
034100     SKIP2                                                                
034200     SET FEL-SEGM TO TRUE                                                 
034300     PERFORM IMS-GET-ARTC-BEST-FIRST                                      
034400     PERFORM UNTIL SEGMENT-SAKNAS OR RAETT-SEGM                           
034500       IF  IN-BEST-IDBEST       = ARTC-BEST-IDBEST                        
034600       AND IN-BEST-IDLEVNR-BEST = ARTC-BEST-IDLEVNR-BEST                  
034700       AND IN-BEST-TIBEST       = ARTC-BEST-TIBEST                        
034800       AND IN-BEST-KDBEH-BEST   = ARTC-BEST-KDBEH-BEST                    
034900       AND IN-BEST-KVBEST       = ARTC-BEST-KVBEST                        
035000       AND IN-BEST-KVBEST-BEKR  = ARTC-BEST-KVBEST-BEKR                   
035100         SET RAETT-SEGM TO TRUE                                           
035200       ELSE                                                               
035300         PERFORM IMS-GET-ARTC-BEST-NEXT                                   
035400       END-IF                                                             
035500     END-PERFORM                                                          
035600     .                                                                    
035700     EJECT                                                                
035800 C-UPPDAT-BEST-INFO SECTION.                                              
035900     SKIP2                                                                
036000     MOVE IN-BEST-IDBEST       TO W-IDBEST                                
036100                                                                          
036200     PERFORM CA-HITTA-BEST-UPPDAT                                         
036300     IF RAETT-SEGM                                                        
036400       MOVE IN-BEST-KVBEST-BEKR TO ARTC-BEST-KVBEST-BEKR                  
036500       MOVE IN-BEST-KDBEH-BEST  TO ARTC-BEST-KDBEH-BEST                   
036600                                                                          
036700       PERFORM IMS-REPL-ARTC-BEST                                         
036800     END-IF                                                               
036900     .                                                                    
037000     SKIP3                                                                
037100 CA-HITTA-BEST-UPPDAT   SECTION.                                          
037200     SKIP2                                                                
037300     SET FEL-SEGM TO TRUE                                                 
037400     PERFORM IMS-GET-ARTC-BEST-FIRST                                      
037500     PERFORM UNTIL SEGMENT-SAKNAS OR RAETT-SEGM                           
037600       IF ARTC-BEST-IDLEVNR-BEST = IN-BEST-IDLEVNR-BEST                   
037700         IF     (IN-BEST-KDBEH-BEST = BEKR-BEST                           
037800           AND ARTC-BEST-KDBEH-BEST = NYTT-BEST)                          
037900         OR     (IN-BEST-KDBEH-BEST = BEKR-ANNU                           
038000           AND ARTC-BEST-KDBEH-BEST = NYTT-ANNU)                          
038100                                                                          
038200           SET RAETT-SEGM TO TRUE                                         
038300         END-IF                                                           
038400       END-IF                                                             
038500       IF FEL-SEGM                                                        
038600         PERFORM IMS-GET-ARTC-BEST-NEXT                                   
038700       END-IF                                                             
038800     END-PERFORM                                                          
038900     .                                                                    
039000     EJECT                                                                
039100 D-BORTTAG-BEST SECTION.                                                  
039200     SKIP2                                                                
039300     MOVE IN-BEST-IDBEST   TO W-IDBEST                                    
039400                                                                          
039500     PERFORM DA-HITTA-BEST-BORTTAG                                        
039600     IF RAETT-SEGM                                                        
039700        PERFORM IMS-DLET-ARTC-BEST                                        
039800        PERFORM DB-BORTTAG-AVT-VID-BEST-ANULL                             
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 DA-HITTA-BEST-BORTTAG  SECTION.                                          
040300     SKIP2                                                                
040400     SET FEL-SEGM TO TRUE                                                 
040500     PERFORM IMS-GET-ARTC-BEST-FIRST                                      
040600     PERFORM UNTIL SEGMENT-SAKNAS OR RAETT-SEGM                           
040700       IF ARTC-BEST-TIBEST = IN-BEST-TIBEST                               
040800         SET RAETT-SEGM TO TRUE                                           
040900       ELSE                                                               
041000         PERFORM IMS-GET-ARTC-BEST-NEXT                                   
041100       END-IF                                                             
041200     END-PERFORM                                                          
041300     .                                                                    
041400     EJECT                                                                
041500 DB-BORTTAG-AVT-VID-BEST-ANULL SECTION.                                   
041600     SKIP2                                                                
041700     MOVE IN-BEST-IDBEST   TO W-IDAVTAL                                   
041800                                                                          
041900     PERFORM IMS-GET-ARTC-AVT                                             
042000     IF SEGMENT-FINNS                                                     
042100        PERFORM IMS-DLET-ARTC-AVT                                         
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 E-NYUPPL-AVT-INFO SECTION.                                               
042600     SKIP2                                                                
042700     MOVE IN-AVT-IDAVTAL     TO W-IDAVTAL                                 
042800                                ARTC-AVT-IDAVTAL                          
042900     MOVE IN-AVT-IDLEVNR-AVT TO ARTC-AVT-IDLEVNR-AVT                      
043000     MOVE IN-AVT-IDLEVNR-SHIP TO ARTC-AVT-IDLEVNR-SHIP                    
043100     MOVE IN-AVT-KDBEH-AVT   TO ARTC-AVT-KDBEH-AVT                        
043200     MOVE IN-AVT-KVAVTANT    TO ARTC-AVT-KVAVTANT                         
043300     MOVE IN-AVT-TIAVTAL     TO ARTC-AVT-TIAVTAL                          
043400                                                                          
043500     PERFORM IMS-ISRT-ARTC-AVT                                            
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 G-BORTTAG-AVTAL SECTION.                                                 
044000     SKIP2                                                                
044100     MOVE IN-AVT-IDAVTAL  TO W-IDAVTAL                                    
044200     PERFORM IMS-GET-F-ARTC-AVT                                           
044300                                                                          
044400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
044500        IF IN-AVT-IDLEVNR-AVT = ARTC-AVT-IDLEVNR-AVT                      
044600           PERFORM IMS-DLET-ARTC-AVT                                      
044700        END-IF                                                            
044800        PERFORM IMS-GET-ARTC-AVT                                          
044900     END-PERFORM                                                          
045000     .                                                                    
045100     EJECT                                                                
045200 H-NYUPPL-LEVPLAN-INFO SECTION.                                           
045300     SKIP2                                                                
045400     MOVE IN-LEVPL-IDARTNR TO W-IDARTNR-D9                                
045500                              INLB-ART-IDARTNR                            
045600     MOVE WC-CDC-SE        TO W-IDDC-D9                                   
045700                              INLB-ART-IDDC                               
045800     PERFORM IMS-ISRT-INLB-ART                                            
045900                                                                          
046000                                                                          
046100     MOVE IN-LEVPL-IDLEVNR TO W-IDLEVNR                                   
046200                              INLB-LEVPL-IDLEVNR                          
046300     MOVE IN-LEVPL-KVBR    TO INLB-LEVPL-KVBR                             
046400     MOVE IN-LEVPL-TILEVPL TO INLB-LEVPL-TILEVPL                          
046500     PERFORM IMS-ISRT-INLB-LEVPL                                          
046600     .                                                                    
046700     EJECT                                                                
046800 I-UPPDAT-LEVPLAN-INFO SECTION.                                           
046900     SKIP2                                                                
047000     MOVE W-IDARTNR        TO W-IDARTNR-D9                                
047100     MOVE WC-CDC-SE        TO W-IDDC-D9                                   
047200     MOVE IN-LEVPL-IDLEVNR TO W-IDLEVNR                                   
047300     PERFORM IMS-GET-INLB-LEVPL                                           
047400                                                                          
047500     MOVE IN-LEVPL-KVBR TO INLB-LEVPL-KVBR                                
047600     PERFORM IMS-REPL-INLB-LEVPL                                          
047700     .                                                                    
047800     EJECT                                                                
047900 J-UPPDAT-MTRLF-INFO SECTION.                                             
048000     SKIP2                                                                
048100     PERFORM IMS-GET-ARTC-CLAG                                            
048200                                                                          
048300     MOVE IN-MTRLF-KDAVT TO ARTC-CLAG-KDAVT                               
048400     MOVE IN-MTRLF-KDKSP TO ARTC-CLAG-KDKSP                               
048500*****MOVE IN-MTRLF-IDINK TO ARTC-CLAG-IDINK                               
048600*****  VI TILLÅTER INTE LÄNGRE ATT IDINK UPPDATERAS HÄR                   
048700                                                                          
048800     PERFORM IMS-REPL-ARTC-CLAG                                           
048900     .                                                                    
049000     EJECT                                                                
049100 Z-FINIT SECTION.                                                         
049200     SKIP2                                                                
049300     CLOSE W21203                                                         
049400                                                                          
049500     MOVE 'S' TO POSTSUM-OPKOD                                            
049600     CALL POSTSUM USING POSTSUM-PARM                                      
049700     .                                                                    
049800     EJECT                                                                
049900 S01-LAES-W21203  SECTION.                                                
050000     SKIP2                                                                
050100     READ W21203 INTO IN-AREA                                             
050200     AT END                                                               
050300        SET END-OF-W21203 TO TRUE                                         
050400                                                                          
050500     NOT AT END                                                           
050600        MOVE 'W21203' TO POSTSUM-FDNAMN                                   
050700        MOVE 'W21204D1' TO POSTSUM-DDNAMN2                                
050800        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
050900        CALL POSTSUM USING POSTSUM-PARM                                   
051000                                                                          
051100     END-READ                                                             
051200     .                                                                    
051300     EJECT                                                                
051400     EJECT                                                                
051500 X-TAG-CHECKPOINT   SECTION.                                              
051600                                                                          
051700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
051800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
051900                                                                          
052000     PERFORM IMS-CHECKPOINT                                               
052100     MOVE ZERO TO CHKP-ANT                                                
052200                                                                          
052300* --- TRIGGA OMLÄSNING AV ARTIKEL-ROT                                     
052400     MOVE ZERO TO ARTC-ART-IDARTNR                                        
052500     .                                                                    
052600     EJECT                                                                
052700* --- IMS SEKTIONER ---                                                   
052800                                                                          
052900                                                                          
053000 IMS-GET-ARTC-ART SECTION.                                                
053100                                                                          
053200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X                             
053300                    '&KDERS    =' W-KDERS-0-X ')'                         
053400             DELIMITED BY SIZE INTO SSA1                                  
053500     MOVE '  '   TO GODK-STATUSKODER                                      
053600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
053700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053800     PERFORM IMS-STATUSKONTROLL                                           
053900     .                                                                    
054000     EJECT                                                                
054100 IMS-GET-ARTC-CLAG  SECTION.                                              
054200     SKIP2                                                                
054300     MOVE   'WLARTC11*F(KDSEGKEY =1)' TO SSA1                             
054400     MOVE '  ' TO GODK-STATUSKODER                                        
054500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC11 SSA1                
054600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
054700     PERFORM IMS-STATUSKONTROLL                                           
054800     .                                                                    
054900     SKIP3                                                                
055000 IMS-REPL-ARTC-CLAG SECTION.                                              
055100                                                                          
055200     MOVE '  ' TO GODK-STATUSKODER                                        
055300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
055400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
055500     PERFORM IMS-STATUSKONTROLL                                           
055600     ADD 1 TO CHKP-ANT                                                    
055700     .                                                                    
055800     EJECT                                                                
055900 IMS-GET-ARTC-BEST-FIRST SECTION.                                         
056000     SKIP2                                                                
056100     MOVE   'WLARTC11(KDSEGKEY =1)'         TO SSA1                       
056200     STRING 'WLARTC22*F(IDBEST   =' W-IDBEST-X ')'                        
056300             DELIMITED BY SIZE INTO SSA2                                  
056400     MOVE '  GE' TO GODK-STATUSKODER                                      
056500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC22 SSA1 SSA2           
056600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056700     PERFORM IMS-STATUSKONTROLL                                           
056800     SKIP3                                                                
056900     .                                                                    
057000 IMS-GET-ARTC-BEST-NEXT SECTION.                                          
057100     SKIP2                                                                
057200     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA1                               
057300     STRING 'WLARTC22(IDBEST   =' W-IDBEST-X ')'                          
057400             DELIMITED BY SIZE INTO SSA2                                  
057500     MOVE '  GE' TO GODK-STATUSKODER                                      
057600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC22 SSA1 SSA2           
057700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUSKONTROLL                                           
057900     .                                                                    
058000     SKIP3                                                                
058100 IMS-REPL-ARTC-BEST SECTION.                                              
058200                                                                          
058300     MOVE '  ' TO GODK-STATUSKODER                                        
058400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC22                     
058500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     ADD 1 TO CHKP-ANT                                                    
058800     .                                                                    
058900     SKIP3                                                                
059000 IMS-DLET-ARTC-BEST SECTION.                                              
059100                                                                          
059200     MOVE '  ' TO GODK-STATUSKODER                                        
059300     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-WLARTC22                     
059400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     ADD 1 TO CHKP-ANT                                                    
059700     .                                                                    
059800     SKIP3                                                                
059900 IMS-ISRT-ARTC-BEST      SECTION.                                         
060000     SKIP2                                                                
060100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
060200             DELIMITED BY SIZE    INTO SSA1                               
060300     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA2                               
060400     MOVE   'WLARTC22*F'            TO SSA3                               
060500     MOVE '  ' TO GODK-STATUSKODER                                        
060600     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-WLARTC22                     
060700                        SSA1 SSA2 SSA3                                    
060800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     ADD 1 TO CHKP-ANT                                                    
061100     .                                                                    
061200     EJECT                                                                
061300 IMS-GET-F-ARTC-AVT    SECTION.                                           
061400     SKIP2                                                                
061500     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA1                               
061600     STRING 'WLARTC23*F(IDAVTAL  =' W-IDAVTAL-X ')'                       
061700             DELIMITED BY SIZE    INTO SSA2                               
061800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
061900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC23 SSA1 SSA2           
062000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300     SKIP3                                                                
062400 IMS-GET-ARTC-AVT    SECTION.                                             
062500     SKIP2                                                                
062600     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA1                               
062700     STRING 'WLARTC23(IDAVTAL  =' W-IDAVTAL-X ')'                         
062800             DELIMITED BY SIZE    INTO SSA2                               
062900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
063000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WLARTC23 SSA1 SSA2           
063100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-ISRT-ARTC-AVT   SECTION.                                             
063600     SKIP2                                                                
063700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
063800             DELIMITED BY SIZE    INTO SSA1                               
063900     MOVE   'WLARTC11(KDSEGKEY =1)' TO SSA2                               
064000     MOVE   'WLARTC23*F'            TO SSA3                               
064100     MOVE '  ' TO GODK-STATUSKODER                                        
064200     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-WLARTC23                     
064300                        SSA1 SSA2 SSA3                                    
064400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     ADD 1 TO CHKP-ANT                                                    
064700     .                                                                    
064800     SKIP3                                                                
064900 IMS-DLET-ARTC-AVT SECTION.                                               
065000                                                                          
065100     MOVE '  ' TO GODK-STATUSKODER                                        
065200     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-WLARTC23                     
065300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     ADD 1 TO CHKP-ANT                                                    
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-GET-INLB-LEVPL   SECTION.                                            
065900     SKIP2                                                                
066000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
066100             DELIMITED BY SIZE INTO SSA1                                  
066200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
066300             DELIMITED BY SIZE INTO SSA2                                  
066400     MOVE '  '   TO GODK-STATUSKODER                                      
066500     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-WLINLB11 SSA1 SSA2            
066600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     SKIP3                                                                
067000 IMS-ISRT-INLB-ART     SECTION.                                           
067100     SKIP2                                                                
067200     MOVE 'WLINLB01 ' TO SSA1                                             
067300     MOVE '  II' TO GODK-STATUSKODER                                      
067400     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-WLINLB01 SSA1                
067500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     ADD 1 TO CHKP-ANT                                                    
067800     .                                                                    
067900     SKIP3                                                                
068000 IMS-ISRT-INLB-LEVPL   SECTION.                                           
068100     SKIP2                                                                
068200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
068300             DELIMITED BY SIZE INTO SSA1                                  
068400     MOVE 'WLINLB11 '            TO SSA2                                  
068500     MOVE '  II' TO GODK-STATUSKODER                                      
068600     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-WLINLB11 SSA1 SSA2           
068700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     ADD 1 TO CHKP-ANT                                                    
069000     .                                                                    
069100     SKIP3                                                                
069200 IMS-REPL-INLB-LEVPL SECTION.                                             
069300                                                                          
069400     MOVE '  ' TO GODK-STATUSKODER                                        
069500     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-WLINLB11                     
069600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     ADD 1 TO CHKP-ANT                                                    
069900     .                                                                    
070000     EJECT                                                                
070100 IMS-RESTART SECTION.                                                     
070200     SKIP2                                                                
070300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070400     MOVE '  ' TO GODK-STATUSKODER                                        
070500     CALL CBLTDLI USING XRST MSG-PCB                                      
070600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070700                        CHKP-AREA-LENGTH CHKP-AREA                        
070800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070900     PERFORM IMS-STATUSKONTROLL                                           
071000     .                                                                    
071100     EJECT                                                                
071200 IMS-CHECKPOINT SECTION.                                                  
071300     SKIP2                                                                
071400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
071500     MOVE '  XD' TO GODK-STATUSKODER                                      
071600     CALL CBLTDLI USING CHKP MSG-PCB                                      
071700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071800                        CHKP-AREA-LENGTH CHKP-AREA                        
071900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072000     PERFORM IMS-STATUSKONTROLL                                           
072100                                                                          
072200     IF IMS-EJ-OK                                                         
072300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
072400       DISPLAY FELTEXT                                                    
072500       CALL FELLOG                                                        
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 IMS-STATUSKONTROLL SECTION.                                              
073000     SKIP2                                                                
073100     SET STATUS-IX TO 1                                                   
073200     SEARCH GODK-STATUS                                                   
073300       AT END                                                             
073400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073500           DELIMITED BY SIZE INTO FELTEXT                                 
073600         DISPLAY FELTEXT                                                  
073700         CALL FELLOG                                                      
073800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073900         CONTINUE                                                         
074000     END-SEARCH                                                           
074100     .                                                                    
